// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.FieldAliasEnumBridge;

interface

uses

  mormot.core.collections,

  DPT.Rsm.Model;

type

  /// <summary>
  ///   §6.15 closure bridge: maps each per-binary RTL type alias used
  ///   in <c>$21</c>/<c>$22</c> param records and <c>$2C</c> field
  ///   records to the corresponding <see cref="TRsmEnumDef"/> using
  ///   the conventional <c>F&lt;TypeName&gt;</c> field-naming convention.
  /// </summary>
  /// <remarks>
  ///   The linker emits cross-unit-RTL type references via a
  ///   per-binary 2-byte alias id (e.g. <c>$0671</c> for
  ///   <c>TThreadPriority</c> in DebugTarget.rsm). This alias is the
  ///   SAME 2-byte value the linker uses in both the <c>$21</c> param
  ///   record for a register-passed parameter of that type and the
  ///   <c>$2C</c> field record for a class field of that type. There
  ///   is no direct registry entry mapping the alias to the type's
  ///   <c>$2A</c> primary id, so live evaluate auto-detection can't
  ///   reach the enum's constant names through the standard channels.
  ///
  ///   <para>This bridge exploits the F-prefix field-naming convention
  ///   used by virtually every Delphi class: a field <c>FFoo</c> of
  ///   type <c>TFoo</c> exposes the type's name through its own
  ///   identifier. The post-process walks every class member with a
  ///   non-zero <c>PrimitiveTypeId</c>, computes a candidate type name
  ///   by stripping the leading F and prepending T, and registers the
  ///   alias -> EnumDef pair when that candidate name matches a
  ///   parsed <see cref="TRsmEnumDef"/>. After this pass,
  ///   <see cref="TRsmReader.IsEnumTypeId"/> recognises the alias as
  ///   enum-typed and
  ///   <see cref="TRsmReader.TryResolveByScopeLocalTypeId"/> resolves
  ///   ordinals to element names.</para>
  ///
  ///   <para>Non-F-prefixed field names are ignored. Conflicts (two
  ///   different fields claiming the same alias) keep the first
  ///   registration to make the bridge deterministic; subsequent
  ///   members get ignored.</para>
  /// </remarks>
  TRsmFieldAliasEnumBridge = class
  private
    FClasses                  : IList<TRsmClassInfo>;
    FProcs                    : IList<TRsmProc>;
    FEnumDefs                 : IList<TRsmEnumDef>;
    FScopeLocalTypeIdToEnumDef: IKeyValue<UInt32, Integer>;
    FTypeIdByName             : IKeyValue<String, UInt32>;
    /// <summary>
    ///   Returns the candidate Delphi type name derived from a field
    ///   or parameter identifier via the conventional prefix rules:
    ///   F-prefix (class field, <c>FFoo -&gt; TFoo</c>) and A-prefix
    ///   (procedure argument, <c>AFoo -&gt; TFoo</c>). Empty string
    ///   when no convention applies.
    /// </summary>
    function CandidateTypeName(const AIdentifier: String): String;
    /// <summary>
    ///   §6.24 last-resort heuristic. Binds record/class field members
    ///   that the structural decode left with NO type id
    ///   (<c>PrimitiveTypeId = TypeIdx = PointerTargetTypeIdx = 0</c>)
    ///   to an enum by the field-name convention <c>&lt;X&gt; -&gt;
    ///   T&lt;X&gt;</c> or the TFW-style <c>T&lt;X&gt;Typ</c>. Fires only
    ///   under the hard leakage guards documented in the implementation
    ///   (genuine non-synthesised <c>$03</c> enum, unique candidate,
    ///   non-F-prefix, all ids zero) so it never clobbers a structural
    ///   binding or invents a phantom enum. The canonical case is TFW's
    ///   <c>TAd.Land: TLandTyp</c>, which no <c>$2C</c> / <c>$67</c>
    ///   record ties to its type (see RSM format reference §6.24 / §4.15).
    /// </summary>
    procedure BindZeroIdFieldsByEnumNameConvention;
  public
    constructor Create(
      AClasses                  : IList<TRsmClassInfo>;
      AProcs                    : IList<TRsmProc>;
      AEnumDefs                 : IList<TRsmEnumDef>;
      AScopeLocalTypeIdToEnumDef: IKeyValue<UInt32, Integer>;
      ATypeIdByName             : IKeyValue<String, UInt32>);
    procedure Run;
  end;

implementation

uses

  System.StrUtils,
  System.SysUtils;

{ TRsmFieldAliasEnumBridge }

constructor TRsmFieldAliasEnumBridge.Create(
  AClasses                  : IList<TRsmClassInfo>;
  AProcs                    : IList<TRsmProc>;
  AEnumDefs                 : IList<TRsmEnumDef>;
  AScopeLocalTypeIdToEnumDef: IKeyValue<UInt32, Integer>;
  ATypeIdByName             : IKeyValue<String, UInt32>);
begin
  inherited Create;
  FClasses                   := AClasses;
  FProcs                     := AProcs;
  FEnumDefs                  := AEnumDefs;
  FScopeLocalTypeIdToEnumDef := AScopeLocalTypeIdToEnumDef;
  FTypeIdByName              := ATypeIdByName;
end;

function TRsmFieldAliasEnumBridge.CandidateTypeName(
  const AIdentifier: String): String;
var
  Prefix : Char;
begin
  // Empty / single-char names cannot have a 1-char prefix payload.
  if Length(AIdentifier) < 2 then Exit('');
  // Two Delphi conventions match this bridge:
  //   F-prefix on class fields:   FFoo  -> TFoo
  //   A-prefix on proc arguments: AFoo  -> TFoo
  // Case-sensitive on the leading letter so we don't accidentally
  // rewrite identifiers like 'fred' or 'abort'.
  Prefix := AIdentifier[1];
  if (Prefix <> 'F') and (Prefix <> 'A') then Exit('');
  Result := 'T' + Copy(AIdentifier, 2, MaxInt);
end;

procedure TRsmFieldAliasEnumBridge.Run;
var
  EnumByNameLower: IKeyValue<String, Integer>;
  ClsIdx         : Integer;
  Cls            : TRsmClassInfo;
  MemIdx         : Integer;
  Mem            : TRsmClassMember;
  ProcIdx        : Integer;
  Proc           : TRsmProc;
  LocIdx         : Integer;
  Loc            : TRsmLocal;
  AliasId        : UInt32;
  Candidate      : String;
  EnumIdx        : Integer;
begin
  if FEnumDefs.Count = 0 then Exit;
  // Build a lowercase index of all enum-def names. Sparse / explicit-
  // value enums also live in this list, so the bridge benefits both
  // contiguous and sparse cross-unit RTL enums.
  EnumByNameLower := Collections.NewPlainKeyValue<String, Integer>;
  for EnumIdx := 0 to FEnumDefs.Count - 1 do
    EnumByNameLower[LowerCase(FEnumDefs[EnumIdx].TypeName)] := EnumIdx;
  // Pass 1 -- walk every class member's PrimitiveTypeId looking for
  // F-prefixed field names that match an enum type.
  for ClsIdx := 0 to FClasses.Count - 1 do
  begin
    Cls := FClasses[ClsIdx];
    for MemIdx := 0 to Cls.Members.Count - 1 do
    begin
      Mem := Cls.Members[MemIdx];
      AliasId := Mem.PrimitiveTypeId;
      if AliasId = 0 then Continue;
      if FScopeLocalTypeIdToEnumDef.ContainsKey(AliasId) then Continue;
      Candidate := CandidateTypeName(Mem.Name);
      if Candidate = '' then Continue;
      if EnumByNameLower.TryGetValue(LowerCase(Candidate), EnumIdx) then
        FScopeLocalTypeIdToEnumDef[AliasId] := EnumIdx;
    end;
  end;
  // Pass 2 -- walk every procedure's register/stack-passed parameter
  // looking for A-prefixed argument names that match an enum type.
  // The A-prefix is the standard Delphi convention for proc args
  // and gives us a bridge for enums that never appear as class
  // fields (so Pass 1 missed them). TFW's
  // KonsCommonLoad(AKonsCommonTyp: TKonsCommonTyp; ...) is the
  // canonical case.
  if FProcs <> nil then
  begin
    for ProcIdx := 0 to FProcs.Count - 1 do
    begin
      Proc := FProcs[ProcIdx];
      if Proc.Locals = nil then Continue;
      for LocIdx := 0 to Proc.Locals.Count - 1 do
      begin
        Loc := Proc.Locals[LocIdx];
        AliasId := Loc.TypeIdx;
        if AliasId = 0 then Continue;
        if FScopeLocalTypeIdToEnumDef.ContainsKey(AliasId) then Continue;
        Candidate := CandidateTypeName(Loc.Name);
        if Candidate = '' then Continue;
        if EnumByNameLower.TryGetValue(LowerCase(Candidate), EnumIdx) then
          FScopeLocalTypeIdToEnumDef[AliasId] := EnumIdx;
      end;
    end;
  end;
  // Pass 3 -- §6.24: last-resort name-convention binding for fields the
  // structural decode left with no type id at all.
  BindZeroIdFieldsByEnumNameConvention;
end;

procedure TRsmFieldAliasEnumBridge.BindZeroIdFieldsByEnumNameConvention;
// §6.24 closure. Some record/class fields are never tied to their type
// by ANY structural channel: TFW's strict-private pointer-to-record
// fields (the canonical TAd.Land: TLandTyp) have no $2C field record and
// no $67 use-site that identifies the owning field (five refuted rounds,
// see RSM format reference §6.20 R6-R9 / §6.23). Such a member arrives
// here with PrimitiveTypeId = TypeIdx = PointerTargetTypeIdx = 0, so the
// live evaluator can only surface the raw ordinal (the §6.20 Pfad-2
// ceiling). This pass recovers the enum NAME via the only remaining
// signal -- the Delphi identifier convention that a field <X> of an enum
// type is usually named after that type (T<X>, or T<X>Typ in TFW's
// suffix convention) -- gated hard so it never invents a phantom binding:
//
//   * the member name does NOT start with the F<Upper> field convention
//     (those are §4.15 Pass 1 / the §6.19 pointer-alias bridge territory);
//   * exactly ONE of {T<X>, T<X>Typ} resolves to a genuine, NON-synthesised
//     $03 ENUM_DEF (a synthesised def may be a §6.25 phantom -- the spec
//     wants a real $03 enum);
//   * that enum type name is UNIQUE among the $03 defs (two sibling units
//     declaring the same enum name is ambiguous -- we don't guess a unit);
//   * the enum has a non-zero $2A registry id (the value we publish as
//     PrimitiveTypeId so the evaluator's auto-detect Path 1 picks it up);
//   * all three of the member's type-id slots are zero -- never clobber a
//     structural binding.
//
// On a hit it sets Member.PrimitiveTypeId to the enum's 2-byte $2A id and
// registers (id -> EnumDef) in the same FScopeLocalTypeIdToEnumDef map the
// $1E / F-prefix bridges use, so TRsmReader.IsEnumTypeId and
// TryGetEnumConstantName resolve it through the established path.
var
  NonSynthByName: IKeyValue<String, Integer>;
  NonSynthCount : IKeyValue<String, Integer>;
  Def           : TRsmEnumDef;
  EnumIdx       : Integer;
  ClsIdx, MemIdx: Integer;
  Cls           : TRsmClassInfo;
  Mem           : TRsmClassMember;
  LName         : String;
  MatchedDef    : Integer;
  MatchedName   : String;
  MatchCount    : Integer;
  EnumRawId     : UInt32;
  EnumId        : UInt16;
  CountSeen     : Integer;

  // True when AName resolves to a single, non-synthesised $03 enum def.
  // On success ADef receives the def index. Ambiguous names (more than
  // one def of that name) are rejected.
  function ResolveUniqueEnum(const AName: String; out ADef: Integer): Boolean;
  var
    Cnt: Integer;
  begin
    Result := False;
    if not NonSynthByName.TryGetValue(LowerCase(AName), ADef) then Exit;
    if NonSynthCount.TryGetValue(LowerCase(AName), Cnt) and (Cnt = 1) then
      Result := True;
  end;

begin
  if (FEnumDefs.Count = 0) or (FTypeIdByName = nil) then Exit;
  // Index the authoritative ($03-sourced) enum defs by name, plus a
  // per-name count so we can reject ambiguous cross-unit duplicates.
  NonSynthByName := Collections.NewPlainKeyValue<String, Integer>;
  NonSynthCount  := Collections.NewPlainKeyValue<String, Integer>;
  for EnumIdx := 0 to FEnumDefs.Count - 1 do
  begin
    Def := FEnumDefs[EnumIdx];
    if Def.Synthesized then Continue;
    LName := LowerCase(Def.TypeName);
    NonSynthByName[LName] := EnumIdx;
    if NonSynthCount.TryGetValue(LName, CountSeen) then
      NonSynthCount[LName] := CountSeen + 1
    else
      NonSynthCount[LName] := 1;
  end;

  for ClsIdx := 0 to FClasses.Count - 1 do
  begin
    Cls := FClasses[ClsIdx];
    for MemIdx := 0 to Cls.Members.Count - 1 do
    begin
      Mem := Cls.Members[MemIdx];
      // Never clobber a structural binding.
      if (Mem.PrimitiveTypeId <> 0) or (Mem.TypeIdx <> 0) or
         (Mem.PointerTargetTypeIdx <> 0) then
        Continue;
      // Skip the F<Upper> field convention -- §4.15 Pass 1 (typed
      // fields) and the §6.19 pointer-alias bridge own those names.
      if (Length(Mem.Name) >= 2) and (Mem.Name[1] = 'F') and
         CharInSet(Mem.Name[2], ['A'..'Z']) then
        Continue;
      if Length(Mem.Name) < 1 then Continue;
      // Exactly one of {T<X>, T<X>Typ} must resolve to a unique $03 enum.
      MatchCount  := 0;
      MatchedDef  := -1;
      MatchedName := '';
      var Cand: Integer;
      if ResolveUniqueEnum('T' + Mem.Name, Cand) then
      begin
        Inc(MatchCount); MatchedDef := Cand; MatchedName := 'T' + Mem.Name;
      end;
      if ResolveUniqueEnum('T' + Mem.Name + 'Typ', Cand) then
      begin
        Inc(MatchCount); MatchedDef := Cand; MatchedName := 'T' + Mem.Name + 'Typ';
      end;
      if MatchCount <> 1 then Continue;
      // The matched enum must carry a non-zero $2A registry id -- that
      // 2-byte id is what we publish as PrimitiveTypeId.
      if not FTypeIdByName.TryGetValue(LowerCase(MatchedName), EnumRawId) then
        Continue;
      EnumId := UInt16(EnumRawId);
      if EnumId = 0 then Continue;
      // Register the resolution under the published id (mirrors the
      // $1E / F-prefix bridges) and stamp the member.
      if not FScopeLocalTypeIdToEnumDef.ContainsKey(EnumId) then
        FScopeLocalTypeIdToEnumDef[EnumId] := MatchedDef;
      Mem.PrimitiveTypeId := EnumId;
      FClasses[ClsIdx].Members[MemIdx] := Mem;
    end;
  end;
end;

end.
