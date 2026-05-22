// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.Reader;

// Public facade over the RSM (CSH7) symbol container. Coordinates a
// <see cref="DPT.Rsm.Scanner.TRsmScanner"/> instance and runs the
// post-process passes (Format-A field linking, class-parent
// derivation, cross-unit parent resolution) on the scanner's
// outputs. All Find/Is/TryGet lookups consumers depend on live here.
//
// The single source of debug information used by the debugger.

interface

uses

  System.Classes,
  System.StrUtils,
  System.SysUtils,

  mormot.core.base,
  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.Scanner,
  DPT.Rsm.FormatALinker;

type

  /// <summary>
  ///   Reader for the RSM (CSH7) symbol container produced by the
  ///   Delphi linker option -VR. Composes a TRsmScanner for the byte
  ///   stream walk and runs the post-process passes that need access
  ///   to the freshly-scanned data on top.
  /// </summary>
  TRsmReader = class
  private
    FScanner             : TRsmScanner;
    /// Maps an RSM 2-byte type id (as encoded in $66 $00 $00
    /// <lo><hi> payloads on locals / globals and on $2A registry
    /// entries) to the index of the matching struct in Classes.
    /// Populated by LinkMemberTypeIdsFromFormatA's ScanTypeRegistry
    /// pass. Needed because Classes[i].TypeIdx holds the file
    /// offset of the class name (used as a unique internal token),
    /// NOT the RSM 2-byte id; without this map a lookup from a
    /// global's encoded type id would never find the right struct.
    FRsmTypeIdToClassIdx : IKeyValue<UInt32, Integer>;
    /// Lowercased type-name -> primary 2-byte type id, populated by
    /// ScanTypeRegistry from $2A entries. Used by the name-based
    /// enum resolver: a field <c>F&lt;X&gt;</c> whose owning class
    /// doesn't carry a usable Format-A type id can fall back to
    /// looking up the enum named <c>T&lt;X&gt;</c> here, then
    /// formatting the value through the standard enum path.
    FTypeIdByName        : IKeyValue<String, UInt32>;
    /// Maps a scope-local enum type id (hi-byte $1E, allocated
    /// per-(unit, type) pair by the Delphi compiler when same-
    /// compilation cross-unit enums are referenced) directly to an
    /// index into <see cref="EnumDefs"/>. Two variables of the same
    /// EnumAlpha.TStatus share the SAME scope-local type id, so a
    /// single anchor variable whose name carries a unit-suffix hint
    /// is enough to bridge ALL variables of that scope-local id to
    /// the correct EnumDef -- including variables whose own name
    /// gives no unit hint. Populated by BuildScopeLocalTypeIdBridge
    /// during RunPostProcess.
    FScopeLocalTypeIdToEnumDef: IKeyValue<UInt32, Integer>;
    /// Second-pass linker that walks the byte stream for Format-A
    /// records ($2A type-registry + $2C field) and patches each
    /// class member's TypeIdx / PrimitiveTypeId. Counterpart to the
    /// scanner's Format-B (offset-only) field walk; both encodings
    /// sit in the same byte stream and together give full member-
    /// name + member-type information.
    FFormatALinker       : TRsmFormatALinker;
    procedure DeriveClassParents;
    procedure ResolveParentNamesFromTypeIds;
    function  GetOnPhase: TProc<String>;
    procedure SetOnPhase(const AValue: TProc<String>);
    function  GetProcs: IList<TRsmProc>;
    function  GetClasses: IList<TRsmClassInfo>;
    function  GetEnumDefs: IList<TRsmEnumDef>;
    procedure BuildScopeLocalTypeIdBridge;
    procedure RunPostProcess;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const AExePath: String);
    procedure LoadFromBytes(const ABytes: TBytes);
    procedure LoadFromBuffer(ABuf: PByte; ASize: NativeInt);
    function  FindProcContaining(ASegmentOffset: NativeUInt): Integer;
    function  FindProcByName(const AName: String): Integer;
    function  FindClassByName(const AName: String): Integer;
    function  FindGlobalTypeIdx(const AName: String): UInt32;
    function  FindClassIdxByRsmTypeId(ARsmId: UInt32): Integer;
    function  IsEnumTypeId(ATypeId: UInt32): Boolean;
    function  FindTypeIdByName(const AName: String): UInt32;
    function  TryGetEnumConstantName(ATypeId: UInt32; AOrdinal: Integer;
      out AName: String; const AExpectedPrefix: String = ''): Boolean;
    function  FindRecordsByMemberName(const AFieldName: String): TArray<Integer>;
    function  FindBestRecordForGlobalAndField(const AGlobalName,
      AFieldName: String): Integer;
    function  FindStructByTypeIdx(ATypeIdx: UInt32): Integer;
    function  FindClassMember(const AClassName, AFieldName: String;
      out AMember: TRsmClassMember): Boolean;
    function  FindStructMemberByTypeIdx(ATypeIdx: UInt32; const AFieldName: String;
      out AMember: TRsmClassMember): Boolean;
    function  IsRecordTypeIdx(ATypeIdx: UInt32): Boolean;
    procedure RecomputeProcSizes;

    function  TryResolveScopeLocalEnum(const AVariableName: String;
      AOrdinal: Integer; const AExpectedType: String;
      out AName: String): Boolean;
    function  TryResolveByScopeLocalTypeId(ATypeId: UInt32;
      AOrdinal: Integer; out AName: String): Boolean;
    property  Procs: IList<TRsmProc> read GetProcs;
    property  Classes: IList<TRsmClassInfo> read GetClasses;
    /// <summary>
    ///   The <c>$03</c> ENUM_DEF records the scanner extracted, in
    ///   first-seen order. Each entry binds a (unit, type) pair to
    ///   its ordered element list -- the authoritative source for
    ///   disambiguating same-name enums declared in sibling units.
    ///   Two units that both export <c>TStatus</c> produce TWO
    ///   entries here, not one.
    /// </summary>
    property  EnumDefs: IList<TRsmEnumDef> read GetEnumDefs;
    /// <summary>
    ///   Optional progress callback fired by the scanner at each
    ///   major parsing phase. Lets callers surface what the parser
    ///   is doing on very large RSM files, where a single phase can
    ///   run for many seconds and otherwise looks indistinguishable
    ///   from a hang.
    /// </summary>
    property OnPhase: TProc<String> read GetOnPhase write SetOnPhase;
  end;

implementation

{ TRsmReader }

constructor TRsmReader.Create;
begin
  inherited Create;
  FScanner                   := TRsmScanner.Create;
  FRsmTypeIdToClassIdx       := Collections.NewPlainKeyValue<UInt32, Integer>;
  FTypeIdByName              := Collections.NewPlainKeyValue<String, UInt32>;
  FScopeLocalTypeIdToEnumDef := Collections.NewPlainKeyValue<UInt32, Integer>;
  FFormatALinker             := TRsmFormatALinker.Create(
    FScanner.Classes, FScanner.ClassByName,
    FRsmTypeIdToClassIdx, FTypeIdByName);
end;

destructor TRsmReader.Destroy;
begin
  FFormatALinker.Free;
  FScanner.Free;
  inherited;
end;

function TRsmReader.GetOnPhase: TProc<String>;
begin
  Result := FScanner.OnPhase;
end;

procedure TRsmReader.SetOnPhase(const AValue: TProc<String>);
begin
  FScanner.OnPhase := AValue;
end;

function TRsmReader.GetProcs: IList<TRsmProc>;
begin
  Result := FScanner.Procs;
end;

function TRsmReader.GetClasses: IList<TRsmClassInfo>;
begin
  Result := FScanner.Classes;
end;

function TRsmReader.GetEnumDefs: IList<TRsmEnumDef>;
begin
  Result := FScanner.EnumDefs;
end;

/// <summary>
///   Resolves an enum-typed variable's ordinal value to the
///   identifier name of the matching element, using the
///   <see cref="EnumDefs"/> registry plus a name-hint heuristic
///   for picking the right (unit, type) pair when the variable's
///   stored type id is scope-local and does not match any of the
///   registered enum primaries.
/// </summary>
/// <param name="AVariableName">
///   Name of the variable being evaluated. When the variable name
///   ends in a substring that matches one of the registered units'
///   trailing segment (case-insensitive, e.g. "Alpha" matches
///   <c>DebugTarget.EnumAlpha</c>), only that unit's enum of the
///   given type name is considered -- this disambiguates sibling-
///   unit enums that share the same type name. When no unit hint
///   matches, all (type-name = <c>AExpectedType</c>) entries
///   compete and the LAST-declared one wins (Delphi's uses-order
///   "last wins" rule for unqualified type references).
/// </param>
/// <param name="AExpectedType">
///   Optional type name hint (e.g. "TStatus"). When empty, every
///   parsed enum def is searched -- expensive on large binaries
///   and prone to false matches, so callers should supply a hint
///   whenever they can derive one from the variable's RSM
///   metadata.
/// </param>
function TRsmReader.TryResolveScopeLocalEnum(const AVariableName: String;
  AOrdinal: Integer; const AExpectedType: String;
  out AName: String): Boolean;
// Disambiguates same-name enums declared in sibling units. Walks
// FScanner.EnumDefs (the $03 ENUM_DEF records) and picks the entry
// whose (UnitName, TypeName) matches:
//   1. TypeName equals AExpectedType (case-insensitive). When the
//      caller omits the hint, every def matches and the unit-suffix
//      filter alone has to disambiguate.
//   2. AVariableName contains a substring of the unit's TRAILING
//      segment (the part after the last '.'), case-insensitive.
//      For unit "DebugTarget.EnumAlpha" the trailing segment is
//      "EnumAlpha"; we also try the version with a common prefix
//      stripped ("Alpha") since the user-facing convention rarely
//      duplicates the "Enum" / "Module" / "Unit" boilerplate in
//      variable names.
//
// When multiple defs match, the LAST-encountered one wins (Delphi's
// uses-order "last wins" rule for unqualified type references). When
// no def matches by unit hint but at least one matches by type name
// alone, that fallback path also picks the last-encountered def.
var
  I       : Integer;
  Defs    : IList<TRsmEnumDef>;
  Def     : TRsmEnumDef;
  Best    : Integer;
  BestSh  : Integer;
  LowerVar: String;

  function UnitTrailingShort(const AUnitName: String): String;
  // Returns "Alpha" for "DebugTarget.EnumAlpha", "Status" for
  // "Tpm.Cap.UnitStatus", etc. Strips through the last '.' and a
  // single optional common prefix ("Enum" / "Module" / "Unit") to
  // produce the substring most likely to appear in a variable name
  // that references this unit's types.
  const
    StripPrefixes: array[0..3] of String = ('Enum', 'Module', 'Unit', 'Mod');
  var
    DotPos : Integer;
    Tail   : String;
    PI     : Integer;
  begin
    DotPos := AUnitName.LastIndexOf('.');
    if DotPos >= 0 then
      Tail := AUnitName.Substring(DotPos + 1)
    else
      Tail := AUnitName;
    for PI := 0 to High(StripPrefixes) do
      if StartsText(StripPrefixes[PI], Tail) and
         (Length(Tail) > Length(StripPrefixes[PI])) then
      begin
        Tail := Copy(Tail, Length(StripPrefixes[PI]) + 1, MaxInt);
        Break;
      end;
    Result := Tail;
  end;

begin
  AName  := '';
  Result := False;
  Defs := FScanner.EnumDefs;
  if Defs.Count = 0 then Exit;
  LowerVar := LowerCase(AVariableName);

  // First pass: prefer entries whose unit trailing-short appears in
  // the variable name (the strong disambiguator).
  Best   := -1;
  BestSh := 0;
  for I := 0 to Defs.Count - 1 do
  begin
    Def := Defs[I];
    if (AExpectedType <> '') and not SameText(Def.TypeName, AExpectedType) then
      Continue;
    var CandName: String;
    if not Def.TryFindByOrdinal(AOrdinal, CandName) then
      Continue;
    var ShortName: String := UnitTrailingShort(Def.UnitName);
    if (ShortName <> '') and (Length(ShortName) >= 2) and
       LowerVar.EndsWith(LowerCase(ShortName)) then
    begin
      // EndsWith (not Contains) keeps the heuristic tight. The
      // common Delphi convention is "G<TypeNameLessT><UnitTail>" for
      // globals or "F<TypeNameLessT><UnitTail>" for fields -- the
      // unit-trailing-short lands at the END of the identifier, not
      // somewhere in the middle.
      //
      // Prefer longer matches if multiple unit shorts share a tail
      // (e.g. "AlphaTwo" beats "Alpha" if both happen to match);
      // on ties the last-declared wins (Delphi's "uses last wins").
      if Length(ShortName) >= BestSh then
      begin
        Best   := I;
        BestSh := Length(ShortName);
        AName  := CandName;
      end;
    end;
  end;
  if Best >= 0 then
  begin
    Result := True;
    Exit;
  end;

  // Fallback: no unit-suffix hint matched. Pick the LAST def whose
  // type-name and ordinal range are compatible -- this matches
  // Delphi's uses-order "last wins" for unqualified type references.
  Best := -1;
  for I := 0 to Defs.Count - 1 do
  begin
    Def := Defs[I];
    if (AExpectedType <> '') and not SameText(Def.TypeName, AExpectedType) then
      Continue;
    var CandName: String;
    if not Def.TryFindByOrdinal(AOrdinal, CandName) then
      Continue;
    Best  := I;
    AName := CandName;
  end;
  Result := Best >= 0;
end;

procedure TRsmReader.LoadFromFile(const AExePath: String);
begin
  FScanner.LoadFromFile(AExePath);
  RunPostProcess;
end;

procedure TRsmReader.LoadFromBytes(const ABytes: TBytes);
begin
  FScanner.LoadFromBytes(ABytes);
  RunPostProcess;
end;

procedure TRsmReader.LoadFromBuffer(ABuf: PByte; ASize: NativeInt);
begin
  FScanner.LoadFromBuffer(ABuf, ASize);
  RunPostProcess;
end;

procedure TRsmReader.RunPostProcess;
var
  ReportPhase: TProc<String>;

  procedure Report(const APhase: String);
  begin
    if Assigned(ReportPhase) then
      ReportPhase(APhase);
  end;

begin
  FRsmTypeIdToClassIdx.Clear;
  FTypeIdByName.Clear;
  FScopeLocalTypeIdToEnumDef.Clear;
  if (FScanner.Buf = nil) or (FScanner.Sz < 8) then Exit;
  ReportPhase := FScanner.OnPhase;
  FFormatALinker.Run(FScanner.Buf, FScanner.Sz);
  Report('LinkMemberTypeIdsFromFormatA');
  DeriveClassParents;
  Report('DeriveClassParents');
  ResolveParentNamesFromTypeIds;
  Report('ResolveParentNamesFromTypeIds');
  BuildScopeLocalTypeIdBridge;
  Report('BuildScopeLocalTypeIdBridge');
  Report('done');
end;

/// <summary>
///   Post-scan pass that walks every registered global,
///   identifies those whose stored type id carries the scope-
///   local enum marker ($1E hi-byte), and -- via the same
///   unit-suffix matching used by
///   <see cref="TryResolveScopeLocalEnum"/> -- binds each
///   distinct scope-local type id to the EnumDef of the unit
///   the anchor variable's name implies. Because the compiler
///   reuses the same scope-local id for all variables of the
///   same (unit, type) pair, a SINGLE conventionally-named
///   anchor variable is enough to bridge every other variable
///   sharing that type id -- even those whose own name carries
///   no unit hint at all.
/// </summary>
procedure TRsmReader.BuildScopeLocalTypeIdBridge;
// Strong-form bridge from scope-local type id ($1E** hi-byte) to
// the matching TRsmEnumDef. Each (unit, type) pair has its own
// scope-local id assigned by the compiler at consumer-unit compile
// time, and ALL variables of that (unit, type) pair share the
// same id. So a single anchor variable per id is enough -- one
// whose name happens to end with the unit's trailing-short -- to
// bind every other variable of that id to the correct EnumDef,
// even variables whose own name carries no unit hint at all.
//
// Walks every (lowercased) name in GlobalByName; for ids carrying
// the $1E marker that aren't yet bridged, re-uses the unit-suffix
// matcher from TryResolveScopeLocalEnum's first pass to find a
// matching def. The first bridge built for an id wins -- typically
// the first conventionally-named global declared with that type.
var
  Pair    : TPair<String, UInt32>;
  Defs    : IList<TRsmEnumDef>;

  function UnitTrailingShort(const AUnitName: String): String;
  const
    StripPrefixes: array[0..3] of String = ('Enum', 'Module', 'Unit', 'Mod');
  var
    DotPos: Integer;
    Tail  : String;
    PI    : Integer;
  begin
    DotPos := AUnitName.LastIndexOf('.');
    if DotPos >= 0 then
      Tail := AUnitName.Substring(DotPos + 1)
    else
      Tail := AUnitName;
    for PI := 0 to High(StripPrefixes) do
      if StartsText(StripPrefixes[PI], Tail) and
         (Length(Tail) > Length(StripPrefixes[PI])) then
      begin
        Tail := Copy(Tail, Length(StripPrefixes[PI]) + 1, MaxInt);
        Break;
      end;
    Result := Tail;
  end;

begin
  Defs := FScanner.EnumDefs;
  if Defs.Count = 0 then Exit;
  for Pair in FScanner.GlobalByName do
  begin
    var TypeId: UInt32 := Pair.Value;
    if ((TypeId shr 8) and $FF) <> $1E then Continue;
    if FScopeLocalTypeIdToEnumDef.ContainsKey(TypeId) then Continue;
    var VarLower: String := Pair.Key; // already lowercased on insert
    var BestIdx : Integer := -1;
    var BestLen : Integer := 0;
    for var I: Integer := 0 to Defs.Count - 1 do
    begin
      var Def: TRsmEnumDef := Defs[I];
      var ShortName: String := UnitTrailingShort(Def.UnitName);
      if (Length(ShortName) >= 2) and
         VarLower.EndsWith(LowerCase(ShortName)) then
      begin
        if Length(ShortName) > BestLen then
        begin
          BestIdx := I;
          BestLen := Length(ShortName);
        end;
      end;
    end;
    if BestIdx >= 0 then
      FScopeLocalTypeIdToEnumDef[TypeId] := BestIdx;
  end;
end;

/// <summary>
///   Direct bridge from a variable's stored scope-local enum
///   type id to an element name -- the strong-form resolver
///   that doesn't depend on the variable's own name carrying a
///   unit suffix. Built by
///   <see cref="BuildScopeLocalTypeIdBridge"/> via at least one
///   anchor variable per scope-local id. Returns False when no
///   bridge entry exists for <paramref name="ATypeId"/> or the
///   ordinal sits in an enum gap; callers should fall back to
///   <see cref="TryResolveScopeLocalEnum"/> in that case.
/// </summary>
function TRsmReader.TryResolveByScopeLocalTypeId(ATypeId: UInt32;
  AOrdinal: Integer; out AName: String): Boolean;
var
  DefIdx: Integer;
begin
  AName  := '';
  Result := False;
  if not FScopeLocalTypeIdToEnumDef.TryGetValue(ATypeId, DefIdx) then Exit;
  if (DefIdx < 0) or (DefIdx >= FScanner.EnumDefs.Count) then Exit;
  Result := FScanner.EnumDefs[DefIdx].TryFindByOrdinal(AOrdinal, AName);
end;

/// <summary>
///   Resolve <c>ParentName</c> for every class that has a
///   non-zero <c>ParentRawId</c> and is still missing a parent
///   after <c>DeriveClassParents</c>. The raw id was captured
///   from the two bytes immediately preceding the class-name
///   length byte; it is meaningful only after the type registry
///   (<c>FRsmTypeIdToClassIdx</c>) has been populated by
///   <c>LinkMemberTypeIdsFromFormatA</c>, which is why this
///   pass runs after both DiscoverAndParseAllStructs and
///   LinkMemberTypeIdsFromFormatA.
/// </summary>
procedure TRsmReader.ResolveParentNamesFromTypeIds;
// Cross-unit inheritance the offset-matching heuristic cannot
// bridge: e.g. a user class declared in DebugTarget inheriting
// from System.Classes.TComponent. The RSM emits a 16-bit type-id
// in the two bytes immediately preceding the class-name length
// byte for such cross-unit parents; same-unit inheritance leaves
// those bytes zero and is handled by DeriveClassParents instead.
//
// We capture that raw id during class discovery (into
// ParentRawId), then resolve it here once the type registry
// (FRsmTypeIdToClassIdx) has been built by
// LinkMemberTypeIdsFromFormatA. Classes whose ParentName was
// already filled in by DeriveClassParents are left alone -- the
// offset heuristic is reliable for in-file chains and we treat
// the type-id pass as a fallback that only fills gaps.
var
  I, ClsIdx: Integer;
  Info     : TRsmClassInfo;
  FClasses : IList<TRsmClassInfo>;
begin
  FClasses := FScanner.Classes;
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
    if Info.Kind <> skClass then Continue;
    if Info.ParentName <> '' then Continue;
    if Info.ParentRawId = 0 then Continue;
    if not FRsmTypeIdToClassIdx.TryGetValue(Info.ParentRawId, ClsIdx) then
      Continue;
    if (ClsIdx < 0) or (ClsIdx >= FClasses.Count) or (ClsIdx = I) then
      Continue;
    Info.ParentName := FClasses[ClsIdx].Name;
    FClasses[I] := Info;
  end;
end;

procedure TRsmReader.DeriveClassParents;
// The RSM symbol stream does not expose an explicit class -> parent
// reference in any form we have identified. The compiler instead bakes
// inheritance into the instance LAYOUT: a class C inheriting from P
// starts its own fields at offset (P's instance size), with the VMT
// pointer taking the very first slot. We exploit this layout to
// reconstruct the hierarchy: for each class C, find the class P whose
// own-fields end exactly at C's first own field offset, and call that
// P the parent of C.
//
// The heuristic does the right thing for class layouts where each
// candidate parent has a distinct instance size (the common case for
// hand-written Delphi code, where sibling classes rarely happen to
// reach the same byte-exact instance boundary). Where two candidates
// collide -- e.g. two unrelated classes that both end at offset $18
// for a child to "choose between" -- we conservatively leave
// ParentName empty, since picking wrong would hide inherited fields
// behind a foreign type. Records (skRecord) are skipped: Delphi
// records cannot inherit, so the offset-matching test wouldn't carry
// meaning for them.
const
  WIN64_PTR_SIZE = 8;
  WIN32_PTR_SIZE = 4;
var
  I, J       : Integer;
  Info       : TRsmClassInfo;
  PtrSize    : UInt32;
  FirstOffs  : array of UInt32;
  LastEnds   : array of UInt32;
  Kinds      : array of TRsmStructKind;
  Cand, Match: Integer;
  FClasses   : IList<TRsmClassInfo>;

  function InstanceSize(AInfo: TRsmClassInfo; AFirstOff, ALastOff: UInt32): UInt32;
  var
    K       : Integer;
    PrevOff : UInt32;
    LastGap : UInt32;
  begin
    // Walk members in offset order to estimate the last field's
    // byte width from the gap to the next, falling back to one
    // pointer for the trailing field whose size we cannot infer
    // from a successor.
    if AInfo.Members.Count = 0 then Exit(AFirstOff);
    PrevOff := AFirstOff;
    LastGap := PtrSize;
    for K := 0 to AInfo.Members.Count - 1 do
    begin
      if AInfo.Members[K].Offset > PrevOff then
        LastGap := AInfo.Members[K].Offset - PrevOff;
      PrevOff := AInfo.Members[K].Offset;
    end;
    Result := ALastOff + LastGap;
  end;

begin
  FClasses := FScanner.Classes;
  if FClasses.Count = 0 then Exit;

  // Detect pointer size from the smallest first-field offset across
  // all class-kind entries. A 16-byte aligned proc-only binary still
  // gives us a sane fallback to 8 (Win64) because that's the modern
  // Delphi default.
  PtrSize := WIN64_PTR_SIZE;
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
    if (Info.Kind <> skClass) or (Info.Members.Count = 0) then Continue;
    if Info.Members[0].Offset = WIN32_PTR_SIZE then
    begin
      PtrSize := WIN32_PTR_SIZE;
      Break;
    end;
  end;

  // Cache the small per-class fields the inner parent-matching loop
  // needs into plain arrays. Indexing into an array of byte-wide
  // enums is a single load and replaces a full TRsmClassInfo copy
  // (Name + ParentName strings + Members IList, three refcount
  // atomic ops per access).
  SetLength(FirstOffs, FClasses.Count);
  SetLength(LastEnds, FClasses.Count);
  SetLength(Kinds, FClasses.Count);
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
    Kinds[I] := Info.Kind;
    FirstOffs[I] := 0;
    LastEnds[I]  := 0;
    if Info.Kind <> skClass then Continue;
    if Info.Members.Count = 0 then Continue;
    FirstOffs[I] := Info.Members[0].Offset;
    for J := 1 to Info.Members.Count - 1 do
      if Info.Members[J].Offset < FirstOffs[I] then
        FirstOffs[I] := Info.Members[J].Offset;
    LastEnds[I] := 0;
    for J := 0 to Info.Members.Count - 1 do
      if Info.Members[J].Offset > LastEnds[I] then
        LastEnds[I] := Info.Members[J].Offset;
    LastEnds[I] := InstanceSize(Info, FirstOffs[I], LastEnds[I]);
  end;

  for I := 0 to FClasses.Count - 1 do
  begin
    if Kinds[I] <> skClass then Continue;
    if FirstOffs[I] <= PtrSize then Continue;
    Match := -1;
    // Walk candidates from I-1 downto 0 so the first hit is the
    // latest-declared class whose instance ends exactly where I's
    // own fields start.
    for Cand := I - 1 downto 0 do
    begin
      if Kinds[Cand] <> skClass then Continue;
      if LastEnds[Cand] = FirstOffs[I] then
      begin
        Match := Cand;
        Break;
      end;
    end;
    // Tolerance fallback: when no candidate's LastEnds matches
    // FirstOffs[I] exactly, an RTL ancestor like TComponent is the
    // typical reason -- accept the closest preceding match within
    // 16 bytes (tiebreaker: latest-declared).
    if Match < 0 then
    begin
      var BestGap: UInt32 := 17;
      for Cand := I - 1 downto 0 do
      begin
        if Kinds[Cand] <> skClass then Continue;
        if LastEnds[Cand] = 0 then Continue;
        if LastEnds[Cand] > FirstOffs[I] then Continue;
        var Gap: UInt32 := FirstOffs[I] - LastEnds[Cand];
        if Gap < BestGap then
        begin
          BestGap := Gap;
          Match := Cand;
        end;
      end;
      if BestGap > 16 then Match := -1;
    end;
    if Match >= 0 then
    begin
      Info := FClasses[I];
      Info.ParentName := FClasses[Match].Name;
      FClasses[I] := Info;
    end;
  end;
end;

/// <summary>
///   Recompute the Size field of every proc as the gap to the
///   next proc by SegmentOffset. Callers may patch each proc's
///   SegmentOffset from a side channel (e.g. the .map file)
///   and then invoke this method to refresh sizes.
/// </summary>
procedure TRsmReader.RecomputeProcSizes;
begin
  FScanner.RecomputeProcSizes;
end;

function TRsmReader.FindProcContaining(ASegmentOffset: NativeUInt): Integer;
var
  I    : Integer;
  Procs: IList<TRsmProc>;
begin
  Procs := FScanner.Procs;
  for I := 0 to Procs.Count - 1 do
    if (Procs[I].SegmentOffset <= ASegmentOffset) and
       (ASegmentOffset < Procs[I].SegmentOffset + Procs[I].Size) then
      Exit(I);
  Result := -1;
end;

function TRsmReader.FindProcByName(const AName: String): Integer;
begin
  // Hot path: case-insensitive direct hit via the name index, which
  // is populated whenever a proc is added. The "@"-prefixed alias
  // is a Delphi linker convention -- some procs are stored as
  // "@MyName" but should be findable by the bare "MyName" too --
  // so the index carries both keys at insert time.
  if FScanner.ProcByName.TryGetValue(LowerCase(AName), Result) then
    Exit;
  Result := -1;
end;

function TRsmReader.FindClassByName(const AName: String): Integer;
begin
  if FScanner.ClassByName.TryGetValue(LowerCase(AName), Result) then
    Exit;
  Result := -1;
end;

/// <summary>
///   Returns the 2-byte RSM type id of the module-level global
///   <c>AName</c>, or 0 when the name is not a known global
///   (either it's a local / parameter / proc, or the reader
///   didn't pick it up). Combine with FindStructByTypeIdx to
///   resolve to a record / class entry in Classes.
/// </summary>
function TRsmReader.FindGlobalTypeIdx(const AName: String): UInt32;
begin
  if not FScanner.GlobalByName.TryGetValue(LowerCase(AName), Result) then
    Result := 0;
end;

/// <summary>
///   Resolves an RSM 2-byte type id (as encoded in $66 $00 $00
///   payload on a local / global record, or in a $2A registry
///   entry) directly to the Classes index of the matching
///   struct. Returns -1 when the id is unknown.
/// </summary>
function TRsmReader.FindClassIdxByRsmTypeId(ARsmId: UInt32): Integer;
begin
  if not FRsmTypeIdToClassIdx.TryGetValue(ARsmId, Result) then
    Result := -1;
end;

/// <summary>
///   Returns True when <paramref name="ATypeId"/> was seen in any
///   $25 enum-constant record -- i.e. the type id designates an
///   enumerated type. Used by auto-detection to pick the enum
///   formatter for enum-typed locals / globals / dotted-terminal
///   fields whose <c>Member.PrimitiveTypeId</c> carries the same
///   2-byte id.
/// </summary>
function TRsmReader.IsEnumTypeId(ATypeId: UInt32): Boolean;
begin
  if ATypeId = 0 then Exit(False);
  Result := FScanner.EnumTypeIds.ContainsKey(ATypeId);
end;

/// <summary>
///   Resolves a type name (case-insensitive) to the 2-byte
///   primary type id captured from its <c>$2A</c> registry entry.
///   Returns 0 when the name isn't registered. Combine with
///   <see cref="IsEnumTypeId"/> to confirm the resolved id
///   belongs to an enum.
/// </summary>
function TRsmReader.FindTypeIdByName(const AName: String): UInt32;
begin
  if not FTypeIdByName.TryGetValue(LowerCase(AName), Result) then
    Result := 0;
end;

/// <summary>
///   Resolves an enum (<c>ATypeId</c>, <c>AOrdinal</c>) pair to
///   the enum-constant's identifier name. Returns False when the
///   pair is not registered (either the type isn't an enum or
///   the ordinal is outside the declared element range).
/// </summary>
/// <param name="AExpectedPrefix">
///   When the primary id is aliased to multiple secondaries
///   (the same primary appears in several $2A entries), pick
///   the candidate whose constant name starts with this
///   prefix. Empty string disables prefix filtering and
///   re-applies the safer "ambiguous -> fail" behavior.
/// </param>
function TRsmReader.TryGetEnumConstantName(ATypeId: UInt32;
  AOrdinal: Integer; out AName: String; const AExpectedPrefix: String): Boolean;
var
  AliasList: IList<UInt32>;
  OrdStr   : String;
  I        : Integer;
  Candidate: String;
begin
  AName  := '';
  Result := False;
  if ATypeId = 0 then Exit;
  OrdStr := ':' + IntToStr(AOrdinal);
  // First try the input id directly -- the $25 record may have
  // registered constants under it.
  if FScanner.EnumConstNames.TryGetValue(IntToStr(ATypeId) + OrdStr, AName) then
    Exit(True);
  if not FScanner.EnumAliasesByPrimary.TryGetValue(ATypeId, AliasList) then
    Exit;
  // The compiler emits multiple secondary ids per $2A entry; only one
  // (or some) actually carries the $25-registered constants for this
  // primary, others may belong to UNRELATED enums whose ids happened
  // to appear in this entry's body. When the caller supplies an
  // expected prefix (derived from the enum's type name via Delphi's
  // PascalCase-acronym convention, e.g. TWindowState -> "ws"),
  // return the first candidate whose name starts with that prefix.
  // Without a prefix hint, fall back to the safer "all agree or
  // fail" rule that prevents misleading wrong-name results.
  if AExpectedPrefix <> '' then
  begin
    for I := 0 to AliasList.Count - 1 do
      if FScanner.EnumConstNames.TryGetValue(
        IntToStr(AliasList[I]) + OrdStr, Candidate) then
        if StartsText(AExpectedPrefix, Candidate) then
        begin
          AName  := Candidate;
          Result := True;
          Exit;
        end;
    Exit;
  end;
  for I := 0 to AliasList.Count - 1 do
    if FScanner.EnumConstNames.TryGetValue(
      IntToStr(AliasList[I]) + OrdStr, Candidate) then
    begin
      if AName = '' then
        AName := Candidate
      else if AName <> Candidate then
      begin
        AName  := '';
        Exit(False);
      end;
    end;
  Result := AName <> '';
end;

/// <summary>
///   Returns the indices of every <c>skRecord</c> entry in
///   <c>Classes</c> that has a member named
///   <paramref name="AFieldName"/> (case-insensitive). Used by
///   the dotted-walk in <c>TDebugger.EvaluateVariable</c> as a
///   reliable fallback when a global's encoded type id does
///   not match the registry's id for the same type.
/// </summary>
function TRsmReader.FindRecordsByMemberName(const AFieldName: String): TArray<Integer>;
var
  Hits    : IList<Integer>;
  I, M    : Integer;
  Info    : TRsmClassInfo;
  FClasses: IList<TRsmClassInfo>;
begin
  // Linear over FClasses (~few thousand entries on TFW-class
  // binaries) but only ever called from the dotted-walk's
  // fallback path, so the cost is bounded by user actions.
  Hits := Collections.NewPlainList<Integer>;
  FClasses := FScanner.Classes;
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
    if Info.Kind <> skRecord then Continue;
    for M := 0 to Info.Members.Count - 1 do
      if SameText(Info.Members[M].Name, AFieldName) then
      begin
        Hits.Add(I);
        Break;
      end;
  end;
  Result := Hits.AsArray;
end;

/// <summary>
///   Resolves the record type behind a global variable by combining
///   two structural signals -- neither of which depends on the
///   global's encoded 2-byte type id (which is unreliable on
///   real-world binaries):
///   <list type="number">
///     <item>Name hint: a record literally named <c>T</c> +
///       <paramref name="AGlobalName"/> that carries
///       <paramref name="AFieldName"/> wins outright.</item>
///     <item>Proximity: among all records carrying
///       <paramref name="AFieldName"/>, the one whose definition
///       in the RSM byte stream lies closest to the global's $20
///       record wins.</item>
///   </list>
///   Returns -1 when no record carrying the field exists, or
///   when the global has not been seen by the reader.
/// </summary>
function TRsmReader.FindBestRecordForGlobalAndField(
  const AGlobalName, AFieldName: String): Integer;
var
  GlobalOff   : NativeInt;
  HintIdx     : Integer;
  BestIdx     : Integer;
  BestDist    : NativeInt;
  I, M        : Integer;
  Info        : TRsmClassInfo;
  HasField    : Boolean;
  Dist        : NativeInt;
  HintHasField: Boolean;
  FClasses    : IList<TRsmClassInfo>;
begin
  Result := -1;
  FClasses := FScanner.Classes;

  // Name hint: a record literally named "T<global>" is a strong
  // Delphi-convention signal that the global's type IS that record.
  // Verify the candidate carries AFieldName before accepting --
  // otherwise the convention misleads us into a wrong record.
  HintIdx := FindClassByName('T' + AGlobalName);
  if (HintIdx >= 0) and (FClasses[HintIdx].Kind = skRecord) then
  begin
    HintHasField := False;
    Info := FClasses[HintIdx];
    for M := 0 to Info.Members.Count - 1 do
      if SameText(Info.Members[M].Name, AFieldName) then
      begin
        HintHasField := True;
        Break;
      end;
    if HintHasField then
    begin
      Result := HintIdx;
      Exit;
    end;
  end;

  // Proximity: among all records carrying AFieldName, the one whose
  // file-offset definition lies closest to the global's $20 record
  // wins. Records and the globals that hold them are emitted in the
  // same per-unit section of the RSM byte stream, so the closest
  // record-with-this-field IS the global's record type.
  if not FScanner.GlobalFileOffset.TryGetValue(LowerCase(AGlobalName), GlobalOff) then
    Exit;

  BestIdx  := -1;
  BestDist := High(NativeInt);
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
    if Info.Kind <> skRecord then Continue;
    if Info.TypeIdx = 0 then Continue;
    HasField := False;
    for M := 0 to Info.Members.Count - 1 do
      if SameText(Info.Members[M].Name, AFieldName) then
      begin
        HasField := True;
        Break;
      end;
    if not HasField then Continue;
    Dist := NativeInt(Info.TypeIdx) - GlobalOff;
    if Dist < 0 then Dist := -Dist;
    if Dist < BestDist then
    begin
      BestDist := Dist;
      BestIdx  := I;
    end;
  end;
  Result := BestIdx;
end;

function TRsmReader.FindStructByTypeIdx(ATypeIdx: UInt32): Integer;
var
  I       : Integer;
  FClasses: IList<TRsmClassInfo>;
begin
  FClasses := FScanner.Classes;
  for I := 0 to FClasses.Count - 1 do
    if FClasses[I].TypeIdx = ATypeIdx then
      Exit(I);
  Result := -1;
end;

function TRsmReader.IsRecordTypeIdx(ATypeIdx: UInt32): Boolean;
var
  Idx: Integer;
begin
  Idx := FindStructByTypeIdx(ATypeIdx);
  Result := (Idx >= 0) and (FScanner.Classes[Idx].Kind = skRecord);
end;

function TRsmReader.FindClassMember(const AClassName, AFieldName: String;
  out AMember: TRsmClassMember): Boolean;
// Looks up a class field by name, walking the inheritance chain via
// ParentName so callers can resolve inherited fields the same way they
// resolve own ones. The walk terminates either at a class with no
// ParentName or after a defensive depth cap that prevents an unsound
// hierarchy (cycles, deep chains we never instantiated in test data)
// from spinning the lookup forever.
const
  MaxChainDepth = 32;
var
  ClsIdx, I, Steps: Integer;
  Info            : TRsmClassInfo;
  CurrentClass    : String;
  FClasses        : IList<TRsmClassInfo>;
begin
  Result := False;
  AMember := Default(TRsmClassMember);
  FClasses := FScanner.Classes;
  CurrentClass := AClassName;
  Steps := 0;
  while (CurrentClass <> '') and (Steps < MaxChainDepth) do
  begin
    ClsIdx := FindClassByName(CurrentClass);
    if ClsIdx < 0 then Exit;
    Info := FClasses[ClsIdx];
    for I := 0 to Info.Members.Count - 1 do
      if SameText(Info.Members[I].Name, AFieldName) then
      begin
        AMember := Info.Members[I];
        Exit(True);
      end;
    CurrentClass := Info.ParentName;
    Inc(Steps);
  end;
end;

function TRsmReader.FindStructMemberByTypeIdx(ATypeIdx: UInt32;
  const AFieldName: String; out AMember: TRsmClassMember): Boolean;
var
  Idx, I  : Integer;
  Info    : TRsmClassInfo;
  FClasses: IList<TRsmClassInfo>;
begin
  Result := False;
  AMember := Default(TRsmClassMember);
  Idx := FindStructByTypeIdx(ATypeIdx);
  if Idx < 0 then
    Exit;
  FClasses := FScanner.Classes;
  Info := FClasses[Idx];
  for I := 0 to Info.Members.Count - 1 do
    if SameText(Info.Members[I].Name, AFieldName) then
    begin
      AMember := Info.Members[I];
      Exit(True);
    end;
end;

end.
