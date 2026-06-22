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

  System.SysUtils,

  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.Scanner,
  DPT.Rsm.FormatALinker,
  DPT.Rsm.ClassParentDeriver,
  DPT.Rsm.CrossUnitParentResolver,
  DPT.Rsm.ScopeLocalEnumBridge,
  DPT.Rsm.FieldAliasEnumBridge,
  DPT.Rsm.PropertyLinker;

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
    FFormatALinker            : TRsmFormatALinker;
    /// Derives class -> parent links from instance-layout heuristic.
    FClassParentDeriver       : TRsmClassParentDeriver;
    /// Cross-unit inheritance fallback (resolves ParentRawId via the
    /// FormatA type-id map).
    FCrossUnitParentResolver  : TRsmCrossUnitParentResolver;
    /// Strong-form bridge from scope-local enum type ids to EnumDef.
    FScopeLocalEnumBridge     : TRsmScopeLocalEnumBridge;
    /// §6.15 closure: F-prefix field-name bridge that maps per-binary
    /// cross-unit RTL type aliases (used in $21/$22 param records and
    /// $2C field records, with arbitrary Hi bytes like $06 / $08 /
    /// $0A / ...) to the corresponding EnumDef via class members whose
    /// names follow the conventional <c>F&lt;TypeName&gt;</c>
    /// naming pattern. Populates the same
    /// <see cref="FScopeLocalTypeIdToEnumDef"/> map the $1E-marker
    /// bridge uses, so IsEnumTypeId and TryGetEnumConstantName get
    /// the new entries transparently.
    FFieldAliasEnumBridge     : TRsmFieldAliasEnumBridge;
    /// §4.16 property-record linker: walks <c>$31</c> records, parses
    /// each property declaration, attributes it to the owning class
    /// via byte-offset proximity, and bridges field-backed properties
    /// to their underlying <c>$2C</c> field via the body+7..+8 alias
    /// id match. Populates <see cref="TRsmClassInfo.Properties"/>
    /// per class. Run as a post-process pass after the class list is
    /// populated; runs independently of the Format-A field linker.
    FPropertyLinker           : TRsmPropertyLinker;
    function  GetOnPhase: TProc<String>;
    procedure SetOnPhase(const AValue: TProc<String>);
    function  GetProcs: IList<TRsmProc>;
    function  GetClasses: IList<TRsmClassInfo>;
    function  GetEnumDefs: IList<TRsmEnumDef>;
    function  GetUnitUseSegments: IList<TRsmUnitUseSegment>;
    function  GetSourceFiles: IList<TRsmSourceFile>;
    procedure RunPostProcess;
    procedure FilterPhantomEnumDefs;
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
    function  TryGetGlobalVa(const AName: String; out AVa: UInt32): Boolean;
    function  FindClassIdxByRsmTypeId(ARsmId: UInt32): Integer;
    function  IsEnumTypeId(ATypeId: UInt32): Boolean;
    function  FindTypeIdByName(const AName: String): UInt32;
    function  TryGetEnumConstantName(ATypeId: UInt32; AOrdinal: Integer;
      out AName: String; const AExpectedPrefix: String = ''): Boolean;
    function  FindRecordsByMemberName(const AFieldName: String): TArray<Integer>;
    function  FindRecordBySizeAndMemberName(ARecordSize: UInt32;
      const AFieldName: String): Integer;
    function  FindBestRecordForGlobalAndField(const AGlobalName,
      AFieldName: String): Integer;
    function  FindStructByTypeIdx(ATypeIdx: UInt32): Integer;
    function  FindClassMember(const AClassName, AFieldName: String;
      out AMember: TRsmClassMember): Boolean;
    /// <summary>
    ///   Looks up a property by name on a class. The walk follows
    ///   the inheritance chain via <c>ParentName</c>, so callers can
    ///   resolve properties declared on an ancestor exactly like
    ///   they resolve inherited fields. Returns False when no
    ///   property of that name exists anywhere in the chain.
    ///   See <see cref="TRsmClassProperty"/> for the field-backed vs
    ///   getter-backed distinction.
    /// </summary>
    function  FindClassProperty(const AClassName, APropertyName: String;
      out AProperty: TRsmClassProperty): Boolean; overload;
    /// As above, but also reports the class the property was actually
    /// declared on (AOwnerClass) -- the start class or an ancestor up
    /// the ParentName chain. The caller needs it to qualify a
    /// getter-backed property's accessor (e.g. TControl.GetText, not
    /// TFormAd.GetText) when resolving the getter's proc address.
    function  FindClassProperty(const AClassName, APropertyName: String;
      out AProperty: TRsmClassProperty; out AOwnerClass: String): Boolean; overload;
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
    ///   Underlying scanner. Exposed (read-only) so callers can read
    ///   raw bytes via <c>ByteAt</c>/<c>DwordAt</c>, inspect the
    ///   <c>Is64Bit</c> arch flag, or run targeted diagnostics that
    ///   need byte-level access without re-mapping the .rsm file.
    /// </summary>
    property  Scanner: TRsmScanner read FScanner;
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
    ///   §6.21 cross-unit symbol-import segments produced by the
    ///   scanner. One entry per <c>$64 NL UnitName $00 $00 $00</c>
    ///   structural anchor on the wire; the <c>Refs</c> list inside
    ///   each segment carries the <c>$66</c> / <c>$67</c> / <c>$70</c>
    ///   entries that follow it. Read-only forwarding of
    ///   <c>Scanner.UnitUseSegments</c>.
    /// </summary>
    property  UnitUseSegments: IList<TRsmUnitUseSegment> read GetUnitUseSegments;
    /// <summary>
    ///   §4.17 source-file (`$70`) records, one per distinct importing
    ///   unit. A <see cref="TRsmUnitUseSegment.SourceFileIdx"/> indexes
    ///   into this list -- the importing unit's name is stored ONCE
    ///   here, not copied per segment. Read-only forwarding of
    ///   <c>Scanner.SourceFiles</c>.
    /// </summary>
    property  SourceFiles: IList<TRsmSourceFile> read GetSourceFiles;
    /// <summary>
    ///   Returns the deduplicated list of unit name(s) that DECLARE
    ///   the given type, derived from the cross-unit symbol-import
    ///   table: walks every <c>$64</c> segment and returns the
    ///   distinct <c>UnitName</c> values whose <c>$66</c> entries
    ///   include <c>ATypeName</c>. The wire shape is
    ///   <c>$64 &lt;DeclaringUnit&gt; ... $66 &lt;TypeName&gt;</c> --
    ///   so a hit means the imported-from unit is the result, e.g.
    ///   <c>UnitsDeclaringType('TLandTyp')</c> returns
    ///   <c>['Base.Types']</c>. The INVERSE question -- "which units
    ///   IMPORT this type?" -- is answered exactly by
    ///   <see cref="UnitsImporting"/> via the <c>$70</c> source-file
    ///   attribution (§4.17), no longer only heuristically. Names are
    ///   deduplicated case-insensitively while preserving first-seen
    ///   order.
    /// </summary>
    function  UnitsDeclaringType(const ATypeName: String): TArray<String>;
    /// <summary>
    ///   The exact inverse of <see cref="UnitsDeclaringType"/>: returns
    ///   the deduplicated unit name(s) that IMPORT <paramref
    ///   name="ATypeName"/>. Walks every <c>$64</c> segment whose
    ///   <c>$66</c> entries include the type and returns the
    ///   <c>UnitName</c> of the <c>$70</c> source-file record that
    ///   introduced each such segment's block
    ///   (<c>SourceFiles[Seg.SourceFileIdx].UnitName</c>, §4.17). E.g.
    ///   <c>UnitsImporting('TLandTyp')</c> returns every TFW unit whose
    ///   <c>uses</c> pulls in <c>TLandTyp</c>. Segments with no
    ///   introducer (<c>SourceFileIdx &lt; 0</c>) are skipped. Names are
    ///   deduplicated case-insensitively while preserving first-seen
    ///   order.
    /// </summary>
    function  UnitsImporting(const ATypeName: String): TArray<String>;
    /// <summary>
    ///   §4.18: the declaring unit of the proc at index
    ///   <paramref name="AProcIdx"/> in <see cref="Procs"/>, resolved
    ///   through the proc's <c>SourceFileIdx</c> foreign key into
    ///   <see cref="SourceFiles"/>. Each unit's proc block in the RSM
    ///   symbol stream is immediately preceded by exactly one <c>$70</c>
    ///   source-file introducer naming that unit's own source file
    ///   (a <c>.pas</c>/<c>.inc</c> for a unit, the full-path
    ///   <c>.dpr</c>/<c>.dpk</c> for the program/package main file);
    ///   the scanner stamps the live introducer cursor onto every proc
    ///   it creates. Returns the empty string for a proc whose
    ///   <c>SourceFileIdx &lt; 0</c> — i.e. one that precedes the first
    ///   introducer, which in practice is a linker-synthesised import
    ///   thunk (<c>MoveFile</c>, <c>CloseHandle</c>, …) that genuinely
    ///   has no Delphi declaring unit in the <c>.rsm</c>. Out-of-range
    ///   indices also return the empty string. This SUPERSEDES the
    ///   former §6.28 "no proc → declaring-unit edge" negative result,
    ///   which was wrong: it had rejected the program's own
    ///   <c>.dpr</c> introducer and so froze the cursor on the last
    ///   imported unit.
    /// </summary>
    function  DeclaringUnitOfProc(AProcIdx: Integer): String;
    /// <summary>
    ///   Convenience overload of <see cref="DeclaringUnitOfProc"/> that
    ///   resolves a proc by (case-insensitive) name first. Returns the
    ///   empty string when the name is unknown or the proc has no
    ///   declaring unit (see the index overload).
    /// </summary>
    function  DeclaringUnitOfProcNamed(const AProcName: String): String;
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

uses

  System.StrUtils;

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
    FRsmTypeIdToClassIdx, FTypeIdByName,
    FScanner.EnumDecoder);
  FClassParentDeriver        := TRsmClassParentDeriver.Create(FScanner.Classes);
  FCrossUnitParentResolver   := TRsmCrossUnitParentResolver.Create(
    FScanner.Classes, FRsmTypeIdToClassIdx);
  FScopeLocalEnumBridge      := TRsmScopeLocalEnumBridge.Create(
    FScanner.GlobalByName, FScanner.EnumDefs, FScopeLocalTypeIdToEnumDef);
  FFieldAliasEnumBridge      := TRsmFieldAliasEnumBridge.Create(
    FScanner.Classes, FScanner.Procs, FScanner.EnumDefs,
    FScopeLocalTypeIdToEnumDef, FTypeIdByName);
  FPropertyLinker            := TRsmPropertyLinker.Create(
    FScanner.Classes, FScanner.ClassByName, FRsmTypeIdToClassIdx);
end;

destructor TRsmReader.Destroy;
begin
  FPropertyLinker.Free;
  FFieldAliasEnumBridge.Free;
  FScopeLocalEnumBridge.Free;
  FCrossUnitParentResolver.Free;
  FClassParentDeriver.Free;
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

function TRsmReader.GetUnitUseSegments: IList<TRsmUnitUseSegment>;
begin
  Result := FScanner.UnitUseSegments;
end;

function TRsmReader.GetSourceFiles: IList<TRsmSourceFile>;
begin
  Result := FScanner.SourceFiles;
end;

/// <summary>
///   Walks every cross-unit symbol-import segment (<c>$64</c>) the
///   scanner collected and returns the declaring-unit names whose
///   <c>$66</c> type-references include <c>ATypeName</c>. The
///   segment's <c>UnitName</c> field IS the declaring/source unit
///   for its <c>$66</c>/<c>$67</c>/<c>$70</c> entries (verified
///   empirically: <c>$64 'Base.Types'</c> segments list types
///   declared in <c>Base.Types.pas</c> such as <c>THttpImplementation</c>
///   and <c>S_3</c>). Comparison is case-insensitive (matching the
///   rest of the reader); the returned array is deduplicated
///   case-insensitively while preserving the first-seen order, so
///   callers can trust the result as a set without re-sorting.
/// </summary>
function TRsmReader.UnitsDeclaringType(const ATypeName: String): TArray<String>;
var
  Segments: IList<TRsmUnitUseSegment>;
  Seen    : IKeyValue<String, Boolean>;
  Hits    : IList<String>;
  I, J    : Integer;
  Seg     : TRsmUnitUseSegment;
  Ref     : TRsmUnitUseRef;
  LowerName: String;
  LowerUnit: String;
  Match   : Boolean;
begin
  Result := nil;
  Segments := FScanner.UnitUseSegments;
  if (Segments = nil) or (Segments.Count = 0) or (ATypeName = '') then Exit;
  LowerName := LowerCase(ATypeName);
  Seen := Collections.NewPlainKeyValue<String, Boolean>;
  Hits := Collections.NewPlainList<String>;
  for I := 0 to Segments.Count - 1 do
  begin
    Seg := Segments[I];
    if Seg.Refs = nil then Continue;
    Match := False;
    for J := 0 to Seg.Refs.Count - 1 do
    begin
      Ref := Seg.Refs[J];
      if (Ref.Kind = uukType) and (LowerCase(Ref.Name) = LowerName) then
      begin
        Match := True;
        Break;
      end;
    end;
    if not Match then Continue;
    LowerUnit := LowerCase(Seg.UnitName);
    if Seen.ContainsKey(LowerUnit) then Continue;
    Seen[LowerUnit] := True;
    Hits.Add(Seg.UnitName);
  end;
  SetLength(Result, Hits.Count);
  for I := 0 to Hits.Count - 1 do
    Result[I] := Hits[I];
end;

function TRsmReader.UnitsImporting(const ATypeName: String): TArray<String>;
// Inverse of UnitsDeclaringType: same segment/$66 match, but returns
// the IMPORTING unit (the $70 source-file record that introduced the
// segment's block, SourceFiles[Seg.SourceFileIdx].UnitName) instead of
// the declaring unit (Seg.UnitName). See §4.17.
var
  Segments  : IList<TRsmUnitUseSegment>;
  SrcFiles  : IList<TRsmSourceFile>;
  Seen      : IKeyValue<String, Boolean>;
  Hits      : IList<String>;
  I, J      : Integer;
  Seg       : TRsmUnitUseSegment;
  Ref       : TRsmUnitUseRef;
  LowerName : String;
  Importer  : String;
  LowerImp  : String;
  Match     : Boolean;
begin
  Result := nil;
  Segments := FScanner.UnitUseSegments;
  SrcFiles := FScanner.SourceFiles;
  if (Segments = nil) or (Segments.Count = 0) or (ATypeName = '') then Exit;
  LowerName := LowerCase(ATypeName);
  Seen := Collections.NewPlainKeyValue<String, Boolean>;
  Hits := Collections.NewPlainList<String>;
  for I := 0 to Segments.Count - 1 do
  begin
    Seg := Segments[I];
    if Seg.Refs = nil then Continue;
    // Only segments attributed to a $70 introducer can name an importer.
    if (Seg.SourceFileIdx < 0) or (SrcFiles = nil) or
       (Seg.SourceFileIdx >= SrcFiles.Count) then Continue;
    Match := False;
    for J := 0 to Seg.Refs.Count - 1 do
    begin
      Ref := Seg.Refs[J];
      if (Ref.Kind = uukType) and (LowerCase(Ref.Name) = LowerName) then
      begin
        Match := True;
        Break;
      end;
    end;
    if not Match then Continue;
    Importer := SrcFiles[Seg.SourceFileIdx].UnitName;
    LowerImp := LowerCase(Importer);
    if Seen.ContainsKey(LowerImp) then Continue;
    Seen[LowerImp] := True;
    Hits.Add(Importer);
  end;
  SetLength(Result, Hits.Count);
  for I := 0 to Hits.Count - 1 do
    Result[I] := Hits[I];
end;

function TRsmReader.DeclaringUnitOfProc(AProcIdx: Integer): String;
// §4.18: resolve a proc to its declaring unit through the
// SourceFileIdx foreign key the scanner stamped from the live $70
// source-file introducer cursor. Empty string for thunks (SourceFileIdx
// < 0) and out-of-range indices.
var
  Procs   : IList<TRsmProc>;
  SrcFiles: IList<TRsmSourceFile>;
  Idx     : Integer;
begin
  Result := '';
  Procs := FScanner.Procs;
  if (Procs = nil) or (AProcIdx < 0) or (AProcIdx >= Procs.Count) then Exit;
  Idx := Procs[AProcIdx].SourceFileIdx;
  SrcFiles := FScanner.SourceFiles;
  if (SrcFiles = nil) or (Idx < 0) or (Idx >= SrcFiles.Count) then Exit;
  Result := SrcFiles[Idx].UnitName;
end;

function TRsmReader.DeclaringUnitOfProcNamed(const AProcName: String): String;
var
  Idx: Integer;
begin
  Result := '';
  if AProcName = '' then Exit;
  if not FScanner.ProcByName.TryGetValue(LowerCase(AProcName), Idx) then Exit;
  Result := DeclaringUnitOfProc(Idx);
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
  // §6.25 R1: drop phantom synthesised EnumDefs. MUST run FIRST -- the
  // scope-local and field-alias enum bridges below store INDICES into
  // EnumDefs, so deleting entries after they run would invalidate those
  // indices (observed: tpHigher / cross-unit TStatus resolved to the
  // wrong element). The scanner already populated EnumDefs (both $03 and
  // synthesised) and FClasses inside FScanner.LoadFromFile, and no
  // post-process pass adds EnumDefs, so filtering here is complete.
  FilterPhantomEnumDefs;
  Report('FilterPhantomEnumDefs');
  FFormatALinker.Run(FScanner.Buf, FScanner.Sz);
  Report('LinkMemberTypeIdsFromFormatA');
  FClassParentDeriver.Run;
  Report('DeriveClassParents');
  FCrossUnitParentResolver.Run;
  Report('ResolveParentNamesFromTypeIds');
  FScopeLocalEnumBridge.Run;
  Report('BuildScopeLocalTypeIdBridge');
  // §6.15: must run AFTER FFormatALinker so class members carry
  // their PrimitiveTypeId. Also after FScopeLocalEnumBridge so the
  // $1E-marker bridge gets first claim on any aliases it can cover.
  FFieldAliasEnumBridge.Run;
  Report('BuildFieldAliasEnumBridge');
  // §4.16: $31 property records. Independent of the bridge passes,
  // depends only on the FClasses list having been populated by the
  // scanner. Bridges field-backed properties to their underlying
  // $2C field via the body+7..+8 alias-id match.
  FPropertyLinker.Run(FScanner.Buf, FScanner.Sz);
  Report('LinkPropertiesFromFormatA');
  Report('done');
end;

procedure TRsmReader.FilterPhantomEnumDefs;
// §6.25 R1 closure. The same-compilation $2A flush synthesises an
// EnumDef from whatever $25 enum constants are pending when a $2A
// surfaces -- even when that $2A is a non-enum type (a class / record /
// interface). The constants it grabs actually belong to ANOTHER enum;
// the result is a phantom (RsmDesk's <c>TPublishableVariantType</c>, a
// class in System.TypInfo, carrying TTypeKind's element names). The flush
// ALSO produces a redundant synthesised duplicate of every contiguous
// enum that already has a $03.
//
// Drops a SYNTHESISED def (never a $03-sourced one) on any of (a)/(c)/(d)/(b):
//   (a) its type name is a real VMT class (skClass) -- a class is not an
//       enum. This catches class phantoms whose borrowed constants are
//       NOT in any $03 (named consts / sparse-enum elements), which (b)
//       can't see. skClass ONLY, never skRecord: a genuine enum can ALSO
//       be registered skRecord by the record-discovery heuristic, so a
//       skRecord drop would delete real enums. Concrete TFW
//       counterexample: TZUGFeRDXMLObjectTyp is a real 131-element enum
//       (xmlNone, xmlC...) that is also skRecord in FClasses -- dropping
//       skRecord-named synth defs kills it. (The earlier rationale here
//       cited TThreadPriority/TUnicodeBreak; that example is refuted --
//       neither is skRecord on current TFW. The risk is real, the
//       example was wrong. See §6.25 re-investigation.)
//   (b) all its elements map to ONE $03-sourced enum. The $03 ENUM_DEF is
//       the authoritative enum oracle (RE probe, §6.25: every contiguous
//       enum has exactly one $03, no class/record has any). A different
//       $03 type name => a phantom that borrowed those constants
//       (TPublishableVariantType <- TTypeKind); the SAME name => a
//       redundant synthesised duplicate of a contiguous enum. Both are
//       superseded by the $03 def and removed.
//   (c) its name follows a definitively-non-enum convention (interface /
//       pointer alias / C / Windows struct) and shadows no $03 -- the
//       inline IsNonEnumTypeName drop (§6.25 closure (c)).
//   (d) it shares its (TypeName, UnitName) with a $03-sourced def -- the
//       same Delphi type (names are unit-unique), so the $03 is
//       authoritative and the synth is a duplicate/const-polluted view.
//       Unit-scoped, so a cross-unit homonym (different unit) survives.
//       Closes the TTokenKind-style doubling that (b) misses when shared
//       element names across sibling enums make the element->type map
//       ambiguous (§6.25; TFW: 24 such same-unit synth duplicates dropped,
//       the lone cross-unit TDataType kept).
// Sparse enums survive: the linker emits no $03 for them, so their
// elements map to no $03 and their name is not a class. They are exactly
// why the synthesis must stay. The residual still leaking after this:
// record/interface/struct phantoms (not in FClasses) that borrowed
// non-$03 constants -- fenced only by the dup-ordinal guard in
// TRsmEnumDecoder.RecordTypeRegistry when the borrow spans families.
//
// NB: the bulk win here is upstream -- the $03 reading-gap fix in
// HandleEnumDefRecord (variable header padding) that made DPT.rsm's 730
// $03 enums parse at all; before it, this filter had nothing to dedup
// against (every DPT enum was synthesised).

  // §6.25 closure (c). True when ANAME follows a definitively-non-enum
  // naming convention, so a SYNTHESISED EnumDef carrying that name is a
  // phantom (the $25 enum-constant run got flushed under a non-enum $2A
  // -- an interface, a pointer alias, or a C/Windows struct). Genuine
  // Delphi enums are T<Upper> WITHOUT exception on the corpus, so that
  // shape is protected FIRST (TZO, TLandTyp, TZUGFeRDXMLObjectTyp). The
  // matched conventions are the high-confidence ones only; E*/C* class
  // families are intentionally left (codebase-specific, higher FP risk)
  // and remain fenced by the dup-ordinal guard.
  function IsNonEnumTypeName(const AName: String): Boolean;
  var
    K        : Integer;
    AllCaps  : Boolean;
    HasLetter: Boolean;
  begin
    Result := False;
    if Length(AName) < 2 then Exit;
    // Protect every genuine enum: T<Upper>. Must come first so all-caps
    // T-names (TZO) never fall through to the ALL-CAPS rule below.
    if (AName[1] = 'T') and CharInSet(AName[2], ['A'..'Z']) then Exit;
    // Interface alias I<Upper> (IRichChunk, ISpellChecker).
    if (AName[1] = 'I') and CharInSet(AName[2], ['A'..'Z']) then Exit(True);
    // Pointer alias P<Upper> (PCLSID, PBlobRef). PtrInt (P + lowercase)
    // is intentionally NOT matched -- stay conservative.
    if (AName[1] = 'P') and CharInSet(AName[2], ['A'..'Z']) then Exit(True);
    // C-translated struct with a leading underscore (_QOS_SD_MODE).
    if AName[1] = '_' then Exit(True);
    // Windows struct tag<Upper> (tagXFORM, tagMSG).
    if (Length(AName) >= 4) and (AName[1] = 't') and (AName[2] = 'a') and
       (AName[3] = 'g') and CharInSet(AName[4], ['A'..'Z']) then Exit(True);
    // ALL-CAPS API typedef: every char A-Z/0-9/_, >=1 letter, len >= 4
    // (QOS_OBJECT_HDR, NET_STRING, GETTEXTEX, D3DVALUE). T<Upper> is
    // already excluded above.
    if Length(AName) >= 4 then
    begin
      AllCaps := True;
      HasLetter := False;
      for K := 1 to Length(AName) do
        if CharInSet(AName[K], ['A'..'Z']) then
          HasLetter := True
        else if not CharInSet(AName[K], ['0'..'9', '_']) then
        begin
          AllCaps := False;
          Break;
        end;
      if AllCaps and HasLetter then Exit(True);
    end;
  end;

var
  Defs       : IList<TRsmEnumDef>;
  ElemToType : IKeyValue<String, String>;  // lc element name -> owning $03 type name
  Names03    : IKeyValue<String, Boolean>; // lc names of $03-sourced enums
  Names03Unit: IKeyValue<String, Boolean>; // "lc name|lc unit" of $03-sourced enums
  I, J, Ci   : Integer;
  Lc, Common : String;
  LcName     : String;  // LowerCase(TypeName) of the current def, cached
  AllMapped  : Boolean;
  OwnerName  : String;
begin
  Defs := FScanner.EnumDefs;
  // Map every $03-sourced element name to its owning type (first-wins,
  // stable). Synthesised defs are excluded -- only $03 is authoritative.
  // Also collect the $03-sourced TYPE names so the (c) name-convention
  // drop never removes a synth def that shadows a real $03 enum, and the
  // (name|unit) keys so the (d) same-type duplicate drop is unit-scoped.
  ElemToType  := Collections.NewPlainKeyValue<String, String>;
  Names03     := Collections.NewPlainKeyValue<String, Boolean>;
  Names03Unit := Collections.NewPlainKeyValue<String, Boolean>;
  for I := 0 to Defs.Count - 1 do
    if (not Defs[I].Synthesized) and (Defs[I].Elements <> nil) then
    begin
      LcName := LowerCase(Defs[I].TypeName);
      Names03[LcName] := True;
      Names03Unit[LcName + '|' + LowerCase(Defs[I].UnitName)] := True;
      for J := 0 to Defs[I].Elements.Count - 1 do
      begin
        Lc := LowerCase(Defs[I].Elements[J].Name);
        if not ElemToType.ContainsKey(Lc) then
          ElemToType[Lc] := Defs[I].TypeName;
      end;
    end;
  // Drop a synthesised def whose elements ALL map to one and the same
  // $03 type. Two sub-cases, both redundant against the authoritative
  // $03 def and therefore removed:
  //   * SAME type name  -> a synthesised duplicate of a contiguous enum
  //     that also has a $03 (the $03 is authoritative; keep only it).
  //   * DIFFERENT name  -> a phantom that borrowed that enum's constants
  //     under a non-enum $2A (TPublishableVariantType carrying TTypeKind).
  // Genuine sparse enums survive: their elements appear in NO $03 (the
  // linker emits none for sparse), so they never map to a Common type.
  for I := Defs.Count - 1 downto 0 do
  begin
    if (not Defs[I].Synthesized) or (Defs[I].Elements = nil) or
       (Defs[I].Elements.Count = 0) then Continue;
    // (a) A synthesised def whose name is a real VMT CLASS is a phantom:
    // a class is never an enum. Catches class phantoms that borrowed
    // NON-$03 constants (named consts / sparse-enum elements), which the
    // $03-coverage rule below cannot see (no $03 to map them to) -- e.g.
    // TIdSchedulerOfThreadDefault. skClass only (never skRecord: a real
    // enum can ALSO be discovered as skRecord, so a skRecord drop would
    // delete it -- e.g. TFW's TZUGFeRDXMLObjectTyp, a 131-element enum
    // also registered skRecord; §6.25). The big non-FClasses phantom
    // residual (P*/I*/ALL_CAPS Windows-struct names) is handled by (c)
    // below via a naming-convention drop.
    Ci := FindClassByName(Defs[I].TypeName);
    if (Ci >= 0) and (FScanner.Classes[Ci].Kind = skClass) then
    begin
      Defs.Delete(I);
      Continue;
    end;
    // (c) §6.25 closure: a synth def whose NAME follows a
    // definitively-non-enum convention (interface/pointer alias, C /
    // Windows struct) is a phantom. These borrowed $25 records under a
    // non-enum $2A; most are NOT in FClasses (so (a) can't see them) and
    // carry non-$03 constants (so (b) can't either). Guards: T<Upper>
    // names are never matched (IsNonEnumTypeName protects them), and a
    // name shadowing a real $03 enum is never dropped.
    LcName := LowerCase(Defs[I].TypeName);  // reused by (c) and (d)
    if IsNonEnumTypeName(Defs[I].TypeName) and
       not Names03.ContainsKey(LcName) then
    begin
      Defs.Delete(I);
      Continue;
    end;
    // (d) §6.25 same-type duplicate drop (the TTokenKind-doubling closure).
    // A synthesised def that shares its (TypeName, UnitName) with a
    // $03-sourced def IS that same Delphi type -- a type name is unique
    // within a unit -- so the $03 is its authoritative form and the synth
    // is a redundant or const-polluted view. (b) misses these: element
    // names shared across sibling enums make the first-wins element->type
    // map ambiguous, so a synth whose elements all live in its own $03 can
    // still map some element to a DIFFERENT $03 enum and fail the
    // "all map to one type" test (observed on TFW: TLngTyp 28/28 in-$03 yet
    // not dropped). Unit-scoped, so a genuine cross-unit homonym survives
    // (TFW's TDataType: a synth in VCLTee.TeeSpline keeps its own identity
    // because the same-name $03 lives in bmpfilt, a DIFFERENT unit -- and
    // DebugTarget's EnumAlpha/Beta/Gamma TStatus trio likewise).
    if Names03Unit.ContainsKey(LcName + '|' + LowerCase(Defs[I].UnitName)) then
    begin
      Defs.Delete(I);
      Continue;
    end;
    // (b) $03-coverage dedup.
    AllMapped := True;
    Common := '';
    for J := 0 to Defs[I].Elements.Count - 1 do
    begin
      if not ElemToType.TryGetValue(LowerCase(Defs[I].Elements[J].Name),
        OwnerName) then
      begin
        AllMapped := False;
        Break;
      end;
      if Common = '' then
        Common := OwnerName
      else if not SameText(Common, OwnerName) then
      begin
        AllMapped := False;  // elements span multiple $03 enums -- not a clean cover
        Break;
      end;
    end;
    if AllMapped and (Common <> '') then
      Defs.Delete(I);
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
///   Returns True with the decoded 4-byte VA slot recovered from
///   the module-global's $27 / $20 record. Platform semantics:
///   on Win32 this is the absolute VA (image base $00400000 already
///   included); on Win64 this is the RVA relative to image base
///   $140000000. Caller knows the platform via the loaded EXE and
///   applies the offset accordingly. Returns False when the global
///   was not registered or its VA slot did not carry the expected
///   $07 low-nibble tag.
/// </summary>
function TRsmReader.TryGetGlobalVa(const AName: String;
  out AVa: UInt32): Boolean;
begin
  Result := FScanner.GlobalVa.TryGetValue(LowerCase(AName), AVa);
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
  // Direct hit via the scanner's $25 enum-constant channel.
  if FScanner.EnumTypeIds.ContainsKey(ATypeId) then Exit(True);
  // §6.15: scope-local + field-alias bridges share the same map.
  // An entry there means we've bridged the alias to an EnumDef even
  // when no $25 record registered the id directly.
  Result := FScopeLocalTypeIdToEnumDef.ContainsKey(ATypeId);
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
  // §6.15: try the scope-local / field-alias bridge before the
  // $2A-alias chain. When this map carries the id, we have a
  // direct EnumDef pointer and can read the element name without
  // walking the alias list.
  if TryResolveByScopeLocalTypeId(ATypeId, AOrdinal, AName) then
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

function TRsmReader.FindRecordBySizeAndMemberName(ARecordSize: UInt32;
  const AFieldName: String): Integer;
// §6.36 nested-record bridge. A record-typed member of another record
// (e.g. TXAdresse.Anschrift: TXAnschrift) carries TypeIdx = 0 -- the
// §4.14 "record-field id = 0" gap -- so the dotted walk can't resolve
// the nested record by id. Two structural signals recover it: (1) the
// member's Size (gap to the next field, or RecordSize - offset for the
// terminal field) equals the nested record's TOTAL byte size, and
// (2) the next dotted segment names a field of that nested record.
// Among skRecords whose summed layout extent == ARecordSize AND which
// declare AFieldName, return the index iff exactly one qualifies --
// the unique-match guard from the §6.18 / §6.19 pointer-to-record
// fallbacks, so a size+name collision declines rather than mis-navigates.
var
  FClasses          : IList<TRsmClassInfo>;
  I, M              : Integer;
  Info              : TRsmClassInfo;
  Total, Ext        : UInt32;
  HasField          : Boolean;
  FoundIdx, MatchCnt: Integer;
begin
  Result := -1;
  if (ARecordSize = 0) or (AFieldName = '') then Exit;
  FClasses := FScanner.Classes;
  FoundIdx := -1;
  MatchCnt := 0;
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
    if Info.Kind <> skRecord then Continue;
    Total    := 0;
    HasField := False;
    for M := 0 to Info.Members.Count - 1 do
    begin
      Ext := Info.Members[M].Offset + Info.Members[M].Size;
      if Ext > Total then Total := Ext;
      if SameText(Info.Members[M].Name, AFieldName) then HasField := True;
    end;
    if HasField and (Total = ARecordSize) then
    begin
      if MatchCnt = 0 then FoundIdx := I;
      Inc(MatchCnt);
    end;
  end;
  if MatchCnt = 1 then Result := FoundIdx;
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

function TRsmReader.FindClassProperty(const AClassName, APropertyName: String;
  out AProperty: TRsmClassProperty): Boolean;
var
  OwnerClass: String;
begin
  Result := FindClassProperty(AClassName, APropertyName, AProperty, OwnerClass);
end;

function TRsmReader.FindClassProperty(const AClassName, APropertyName: String;
  out AProperty: TRsmClassProperty; out AOwnerClass: String): Boolean;
// Walks the class chain (own then ancestors) looking for a property
// of the requested name. Properties live in TRsmClassInfo.Properties,
// populated by TRsmPropertyLinker during RunPostProcess. AOwnerClass
// reports the class the match was declared on (needed to qualify a
// getter-backed property's accessor proc).
const
  MaxChainDepth = 32;
var
  ClsIdx, I, Steps: Integer;
  Info            : TRsmClassInfo;
  CurrentClass    : String;
  FClasses        : IList<TRsmClassInfo>;
begin
  Result := False;
  AProperty := Default(TRsmClassProperty);
  AOwnerClass := '';
  FClasses := FScanner.Classes;
  CurrentClass := AClassName;
  Steps := 0;
  while (CurrentClass <> '') and (Steps < MaxChainDepth) do
  begin
    ClsIdx := FindClassByName(CurrentClass);
    if ClsIdx < 0 then Exit;
    Info := FClasses[ClsIdx];
    if Info.Properties <> nil then
      for I := 0 to Info.Properties.Count - 1 do
        if SameText(Info.Properties[I].Name, APropertyName) then
        begin
          AProperty := Info.Properties[I];
          AOwnerClass := Info.Name;
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
