// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.FieldAliasEnumBridge;

interface

uses
  System.StrUtils,
  System.SysUtils,

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
    /// <summary>
    ///   Returns the candidate Delphi type name derived from a field
    ///   or parameter identifier via the conventional prefix rules:
    ///   F-prefix (class field, <c>FFoo -&gt; TFoo</c>) and A-prefix
    ///   (procedure argument, <c>AFoo -&gt; TFoo</c>). Empty string
    ///   when no convention applies.
    /// </summary>
    function CandidateTypeName(const AIdentifier: String): String;
  public
    constructor Create(
      AClasses                  : IList<TRsmClassInfo>;
      AProcs                    : IList<TRsmProc>;
      AEnumDefs                 : IList<TRsmEnumDef>;
      AScopeLocalTypeIdToEnumDef: IKeyValue<UInt32, Integer>);
    procedure Run;
  end;

implementation

{ TRsmFieldAliasEnumBridge }

constructor TRsmFieldAliasEnumBridge.Create(
  AClasses                  : IList<TRsmClassInfo>;
  AProcs                    : IList<TRsmProc>;
  AEnumDefs                 : IList<TRsmEnumDef>;
  AScopeLocalTypeIdToEnumDef: IKeyValue<UInt32, Integer>);
begin
  inherited Create;
  FClasses                   := AClasses;
  FProcs                     := AProcs;
  FEnumDefs                  := AEnumDefs;
  FScopeLocalTypeIdToEnumDef := AScopeLocalTypeIdToEnumDef;
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
end;

end.
