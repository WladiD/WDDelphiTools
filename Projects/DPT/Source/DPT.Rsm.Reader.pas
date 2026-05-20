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
  DPT.Rsm.Scanner;

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
    procedure LinkMemberTypeIdsFromFormatA;
    procedure DeriveClassParents;
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
    procedure ResolveParentNamesFromTypeIds;
    function  GetOnPhase: TProc<String>;
    procedure SetOnPhase(const AValue: TProc<String>);
    function  GetProcs: IList<TRsmProc>;
    function  GetClasses: IList<TRsmClassInfo>;
    procedure RunPostProcess;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Optional progress callback fired by the scanner at each
    ///   major parsing phase. Lets callers surface what the parser
    ///   is doing on very large RSM files, where a single phase can
    ///   run for many seconds and otherwise looks indistinguishable
    ///   from a hang.
    /// </summary>
    property OnPhase: TProc<String> read GetOnPhase write SetOnPhase;
    procedure LoadFromFile(const AExePath: String);
    procedure LoadFromBytes(const ABytes: TBytes);
    procedure LoadFromBuffer(ABuf: PByte; ASize: NativeInt);
    function  FindProcContaining(ASegmentOffset: NativeUInt): Integer;
    function  FindProcByName(const AName: String): Integer;
    function  FindClassByName(const AName: String): Integer;
    /// <summary>
    ///   Returns the 2-byte RSM type id of the module-level global
    ///   <c>AName</c>, or 0 when the name is not a known global
    ///   (either it's a local / parameter / proc, or the reader
    ///   didn't pick it up). Combine with FindStructByTypeIdx to
    ///   resolve to a record / class entry in Classes.
    /// </summary>
    function  FindGlobalTypeIdx(const AName: String): UInt32;
    /// <summary>
    ///   Resolves an RSM 2-byte type id (as encoded in $66 $00 $00
    ///   payload on a local / global record, or in a $2A registry
    ///   entry) directly to the Classes index of the matching
    ///   struct. Returns -1 when the id is unknown.
    /// </summary>
    function  FindClassIdxByRsmTypeId(ARsmId: UInt32): Integer;
    /// <summary>
    ///   Returns True when <paramref name="ATypeId"/> was seen in any
    ///   $25 enum-constant record -- i.e. the type id designates an
    ///   enumerated type. Used by auto-detection to pick the enum
    ///   formatter for enum-typed locals / globals / dotted-terminal
    ///   fields whose <c>Member.PrimitiveTypeId</c> carries the same
    ///   2-byte id.
    /// </summary>
    function  IsEnumTypeId(ATypeId: UInt32): Boolean;
    /// <summary>
    ///   Resolves a type name (case-insensitive) to the 2-byte
    ///   primary type id captured from its <c>$2A</c> registry entry.
    ///   Returns 0 when the name isn't registered. Combine with
    ///   <see cref="IsEnumTypeId"/> to confirm the resolved id
    ///   belongs to an enum.
    /// </summary>
    function  FindTypeIdByName(const AName: String): UInt32;
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
    function  TryGetEnumConstantName(ATypeId: UInt32; AOrdinal: Integer;
      out AName: String; const AExpectedPrefix: String = ''): Boolean;
    /// <summary>
    ///   Returns the indices of every <c>skRecord</c> entry in
    ///   <c>Classes</c> that has a member named
    ///   <paramref name="AFieldName"/> (case-insensitive). Used by
    ///   the dotted-walk in <c>TDebugger.EvaluateVariable</c> as a
    ///   reliable fallback when a global's encoded type id does
    ///   not match the registry's id for the same type.
    /// </summary>
    function  FindRecordsByMemberName(const AFieldName: String): TArray<Integer>;
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
    function  FindBestRecordForGlobalAndField(const AGlobalName,
      AFieldName: String): Integer;
    function  FindStructByTypeIdx(ATypeIdx: UInt32): Integer;
    function  FindClassMember(const AClassName, AFieldName: String;
      out AMember: TRsmClassMember): Boolean;
    function  FindStructMemberByTypeIdx(ATypeIdx: UInt32; const AFieldName: String;
      out AMember: TRsmClassMember): Boolean;
    function  IsRecordTypeIdx(ATypeIdx: UInt32): Boolean;
    /// <summary>
    ///   Recompute the Size field of every proc as the gap to the
    ///   next proc by SegmentOffset. Callers may patch each proc's
    ///   SegmentOffset from a side channel (e.g. the .map file)
    ///   and then invoke this method to refresh sizes.
    /// </summary>
    procedure RecomputeProcSizes;
    property  Procs: IList<TRsmProc> read GetProcs;
    property  Classes: IList<TRsmClassInfo> read GetClasses;
  end;

implementation

{ TRsmReader }

constructor TRsmReader.Create;
begin
  inherited Create;
  FScanner             := TRsmScanner.Create;
  FRsmTypeIdToClassIdx := Collections.NewPlainKeyValue<UInt32, Integer>;
  FTypeIdByName        := Collections.NewPlainKeyValue<String, UInt32>;
end;

destructor TRsmReader.Destroy;
begin
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
  if (FScanner.Buf = nil) or (FScanner.Sz < 8) then Exit;
  ReportPhase := FScanner.OnPhase;
  LinkMemberTypeIdsFromFormatA;
  Report('LinkMemberTypeIdsFromFormatA');
  DeriveClassParents;
  Report('DeriveClassParents');
  ResolveParentNamesFromTypeIds;
  Report('ResolveParentNamesFromTypeIds');
  Report('done');
end;

procedure TRsmReader.LinkMemberTypeIdsFromFormatA;
// RSM emits class fields in TWO parallel encodings within the
// symbol cluster:
//
//   Format B (offset-only, what ScanFieldsBackwardFromClassName
//   reads): <DWORD-offset> <namelen> <name>. No field-type info.
//
//   Format A (rich, also present): contains a real 2-byte type-id
//   for every field, plus a "parent class" 2-byte type-id at the
//   end of each record. A separate "type registry" maps each
//   class/record name to its 2-byte type-id.
//
//     Registry entry:  2A <NL> <name> 20 00 00 <type-id 2 bytes>
//     Field record:    (2C | FF 2C) <NL> <name> 00 02 00
//                      <field-type-id 2 bytes> ...
//                      07 00 00 08 <parent-type-id 2 bytes>
//
// After ScanFieldsBackwardFromClassName / ScanFieldsForwardFromRecordName
// have collected the classes/records and their members (with
// names + offsets but TypeIdx = 0), we walk the byte stream again
// and use Format A to populate each Member.TypeIdx.
type
  TRawId = record Lo, Hi: Byte; end;
var
  Buf      : PByte;
  Sz       : NativeInt;
  FClasses : IList<TRsmClassInfo>;
  // Format-A-confirmed (classIdx, field-name) pairs. The backward
  // Format-B scan over-collects field candidates from neighbouring
  // class declarations because its window is fixed, so we keep an
  // authoritative ownership map here and use it to prune Members
  // before downstream code (parent derivation, evaluator) sees them.
  // Stored as a key/value set keyed on "<classIdx>:<lower-field-name>"
  // so lookups are O(1) instead of O(N): on large binaries the
  // earlier linear scan turned PruneSpuriousMembers into the single
  // biggest hot spot, blowing past 30 seconds for 800MB+ files.
  Confirmed: IKeyValue<String, Boolean>;

  function ByteAt(AOffset: NativeInt): Byte;
  begin
    Result := (Buf + AOffset)^;
  end;

  function RawIdKey(const ARaw: TRawId): UInt32; inline;
  begin
    Result := UInt32(ARaw.Lo) or (UInt32(ARaw.Hi) shl 8);
  end;

  function ConfirmedKey(AClsIdx: Integer; const AFieldName: String): String;
  begin
    Result := IntToStr(AClsIdx) + ':' + LowerCase(AFieldName);
  end;

  procedure ConfirmedAdd(AClsIdx: Integer; const AFieldName: String);
  begin
    Confirmed[ConfirmedKey(AClsIdx, AFieldName)] := True;
  end;

  function IsConfirmed(AClsIdx: Integer; const AFieldName: String): Boolean;
  begin
    Result := Confirmed.ContainsKey(ConfirmedKey(AClsIdx, AFieldName));
  end;

  procedure PruneSpuriousMembers;
  var
    I, M    : Integer;
    Info    : TRsmClassInfo;
    Pruned  : IList<TRsmClassMember>;
    AnyHit  : Boolean;
  begin
    for I := 0 to FClasses.Count - 1 do
    begin
      Info := FClasses[I];
      if Info.Kind <> skClass then Continue;
      // Records emit their fields forward from the record name with
      // explicit offsets, so the Format-B scan can't over-collect
      // for them; skip records to avoid touching their layout.
      AnyHit := False;
      for M := 0 to Info.Members.Count - 1 do
        if IsConfirmed(I, Info.Members[M].Name) then
        begin
          AnyHit := True;
          Break;
        end;
      // If no Format A field record confirmed ANY member, the class
      // has no Format A coverage at all (e.g. system classes without
      // user fields). Leave its Members untouched in that case so
      // we don't accidentally erase legitimate data we just haven't
      // verified yet.
      if not AnyHit then Continue;
      Pruned := Collections.NewPlainList<TRsmClassMember>;
      for M := 0 to Info.Members.Count - 1 do
        if IsConfirmed(I, Info.Members[M].Name) then
          Pruned.Add(Info.Members[M]);
      Info.Members := Pruned;
      FClasses[I]  := Info;
    end;
  end;

  function FindClassIdxForRawId(const ARaw: TRawId): Integer;
  begin
    // Direct O(1) lookup via the registry-built hashmap. Returns
    // -1 if the raw id is unknown (built-in types or types we
    // didn't parse). Replaces the prior O(N^2) scan.
    if not FRsmTypeIdToClassIdx.TryGetValue(RawIdKey(ARaw), Result) then
      Result := -1;
  end;

  procedure ScanTypeRegistry;
  var
    P, NL  : Integer;
    Skip   : PtrInt;
    Name   : String;
    Id     : TRawId;
    ClsIdx : Integer;
  begin
    // Walk the byte stream looking for $2A name-id records and
    // populate FRsmTypeIdToClassIdx by joining each registered name
    // against the already-built FClassByName index. The byte-walk
    // is driven by mORMot's SSE2-accelerated ByteScanIndex so we
    // jump directly to the next $2A tag instead of incrementing
    // 858M times in Pascal (a half-second tag-hunt becomes
    // microseconds). Records whose first name character isn't 'T'
    // are rejected via a single byte peek before we ever build a
    // String, since the registry only carries Delphi class /
    // record names which all start with 'T'.
    P := 0;
    while P + 12 < Sz do
    begin
      Skip := ByteScanIndex(PByteArray(Buf + P), Sz - P - 12, TRsmTag.TYPE_REGISTRY_TAG);
      if Skip < 0 then Break;
      Inc(P, Skip);
      // The byte directly after the name (a "kind" flag) varies
      // across registry entries, so we don't constrain it. The
      // two bytes after that ARE reliably $00 $00 across every
      // entry observed in DebugTarget.rsm and TFW.rsm, which
      // keeps the false-positive rate near zero on incidental $2A
      // bytes in the file.
      NL := ByteAt(P + 1);
      if (NL >= 2) and (NL <= 40) and
         (P + 2 + NL + 5 < Sz) and
         (ByteAt(P + 2) = Byte(Ord('T'))) and
         (ByteAt(P + 2 + NL + 1) = $00) and
         (ByteAt(P + 2 + NL + 2) = $00) and
         FScanner.ReadIdentifier(P + 1, Name) then
      begin
        Id.Lo := ByteAt(P + 2 + NL + 3);
        Id.Hi := ByteAt(P + 2 + NL + 4);
        ClsIdx := FindClassByName(Name);
        if ClsIdx >= 0 then
          FRsmTypeIdToClassIdx[RawIdKey(Id)] := ClsIdx;
        // Also record the (name, typeId) pair regardless of whether
        // the name matches a class in FClasses. The name-based enum
        // resolver in AutoDetectFormatterName uses this to translate
        // a field name like FThreadPriority into a known enum's id
        // when Format-A linking did not capture the field's type --
        // the common cross-unit case in real-world binaries.
        FTypeIdByName[LowerCase(Name)] := RawIdKey(Id);
        P := P + 2 + NL + 5;
      end
      else
        Inc(P);
    end;
  end;

  procedure LinkFieldsFromFormatA;
  var
    P, NL    : Integer;
    Skip     : PtrInt;
    Name     : String;
    FieldId  : TRawId;
    ParentId : TRawId;
    ParentIdx: Integer;
    FieldIdx : Integer;
    I, M     : Integer;
    Member   : TRsmClassMember;
    EndOff   : Integer;
  begin
    // Same SSE2-accelerated scanning as ScanTypeRegistry, but the
    // tag we hunt for is $2C and the record may be prefixed with
    // a $FF byte (continuation marker for non-first fields). We
    // jump directly to the next $2C using ByteScanIndex, then
    // resolve the actual TagOff from the byte one position before
    // (if it's $FF). The first-character peek for 'F' rejects
    // 99.6% of incidental $2C hits before allocating the name.
    P := 0;
    while P + 24 < Sz do
    begin
      Skip := ByteScanIndex(PByteArray(Buf + P), Sz - P - 24, $2C);
      if Skip < 0 then Break;
      Inc(P, Skip);
      var TagOff: Integer := P;
      // Tag form $FF $2C uses the position one before, but the
      // field name still starts at TagOff+2 (i.e. one past the
      // length byte). Either way the first-character byte that
      // determines whether this is a field name lives at TagOff+2.
      // Structural anchor instead of "Name[1]='F'". The Delphi
      // F-prefix convention isn't universal -- real-world records
      // have fields named WhdrHeader, DbKindName, etc. that the
      // old check rejected, leaving them with TypeIdx=0 so the
      // dotted-walk couldn't transition from a parent record
      // into them. Validate the "$00 $02 $00" payload prefix
      // BEFORE allocating the name; this is a more selective
      // guard than any first-character rule could be.
      NL := ByteAt(TagOff + 1);
      if (NL < 2) or (NL > 40) or (TagOff + 2 + NL + 7 >= Sz) then
      begin
        Inc(P);
        Continue;
      end;
      var After: Integer := TagOff + 2 + NL;
      if (ByteAt(After) <> $00) or (ByteAt(After + 1) <> $02) or
         (ByteAt(After + 2) <> $00) then
      begin
        Inc(P);
        Continue;
      end;
      if not FScanner.ReadIdentifier(TagOff + 1, Name) then
      begin
        Inc(P);
        Continue;
      end;
      // Final size guard for the rest of the field record (a
      // bounded forward window plus the 2-byte parent id).
      if After + 5 + 6 + 2 > Sz then
      begin
        Inc(P);
        Continue;
      end;
      FieldId.Lo := ByteAt(After + 3);
      FieldId.Hi := ByteAt(After + 4);
      // Record varies in length: scan a bounded window for the
      // terminator "07 00 00 08 <parent-id-lo> <parent-id-hi>".
      EndOff := -1;
      for I := After + 5 to After + 30 do
      begin
        if I + 5 > Sz then Break;
        if (ByteAt(I) = $07) and (ByteAt(I + 1) = $00) and
           (ByteAt(I + 2) = $00) and (ByteAt(I + 3) = $08) then
        begin
          EndOff := I;
          Break;
        end;
      end;
      if EndOff < 0 then
      begin
        Inc(P);
        Continue;
      end;
      ParentId.Lo := ByteAt(EndOff + 4);
      ParentId.Hi := ByteAt(EndOff + 5);
      ParentIdx := FindClassIdxForRawId(ParentId);
      if ParentIdx < 0 then
      begin
        Inc(P);
        Continue;
      end;
      // Find the member by name in the parent class and resolve
      // its TypeIdx via the field-type-id. Also record that this
      // (class, field-name) pair was authoritatively confirmed by
      // Format A so PruneSpuriousMembers can drop members the
      // backward field scan over-eagerly attributed to this class.
      //
      // Three $2C body shapes the compiler emits for non-class
      // fields, all routed into Member.PrimitiveTypeId so the
      // evaluate-tool's auto-detection path can pick the matching
      // formatter without an explicit <c>type</c> argument:
      //   - body=14: numeric primitive (Integer, Word, Byte,
      //     Int64, UnicodeString, Single, Double, Extended).
      //   - body=15: numeric primitive with extra leading byte
      //     (Boolean, Currency, ...).
      //   - body=9: managed reference primitive (AnsiString,
      //     WideString, ShortString).
      for M := 0 to FClasses[ParentIdx].Members.Count - 1 do
      begin
        Member := FClasses[ParentIdx].Members[M];
        if SameText(Member.Name, Name) and (Member.TypeIdx = 0) and
           (Member.PrimitiveTypeId = 0) then
        begin
          FieldIdx := FindClassIdxForRawId(FieldId);
          if FieldIdx >= 0 then
            Member.TypeIdx := FClasses[FieldIdx].TypeIdx
          else
          begin
            var BodyLen: Integer := EndOff - After;
            if (BodyLen = 14) or (BodyLen = 15) then
              Member.PrimitiveTypeId :=
                UInt16(ByteAt(EndOff - 5)) or
                (UInt16(ByteAt(EndOff - 4)) shl 8)
            else if (BodyLen = 9) and
                    (ByteAt(After + 5) = $9C) and
                    (ByteAt(After + 6) = $01) then
              Member.PrimitiveTypeId :=
                UInt16(ByteAt(After + 3)) or
                (UInt16(ByteAt(After + 4)) shl 8)
            // Enum-typed field. Body shape adds a $00 separator
            // between the type id and the "$9C $01" reference
            // marker (so the marker sits at +6..+7 instead of
            // +5..+6).
            else if (BodyLen >= 10) and
                    (ByteAt(After + 6) = $9C) and
                    (ByteAt(After + 7) = $01) then
              Member.PrimitiveTypeId :=
                UInt16(ByteAt(After + 3)) or
                (UInt16(ByteAt(After + 4)) shl 8);
          end;
          FClasses[ParentIdx].Members[M] := Member;
          ConfirmedAdd(ParentIdx, Name);
          Break;
        end;
      end;
      P := EndOff + 6;
    end;
  end;

begin
  Buf      := FScanner.Buf;
  Sz       := FScanner.Sz;
  FClasses := FScanner.Classes;
  FRsmTypeIdToClassIdx.Clear;
  Confirmed := Collections.NewPlainKeyValue<String, Boolean>;
  ScanTypeRegistry;
  LinkFieldsFromFormatA;
  PruneSpuriousMembers;
end;

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

function TRsmReader.FindGlobalTypeIdx(const AName: String): UInt32;
begin
  if not FScanner.GlobalByName.TryGetValue(LowerCase(AName), Result) then
    Result := 0;
end;

function TRsmReader.FindClassIdxByRsmTypeId(ARsmId: UInt32): Integer;
begin
  if not FRsmTypeIdToClassIdx.TryGetValue(ARsmId, Result) then
    Result := -1;
end;

function TRsmReader.IsEnumTypeId(ATypeId: UInt32): Boolean;
begin
  if ATypeId = 0 then Exit(False);
  Result := FScanner.EnumTypeIds.ContainsKey(ATypeId);
end;

function TRsmReader.FindTypeIdByName(const AName: String): UInt32;
begin
  if not FTypeIdByName.TryGetValue(LowerCase(AName), Result) then
    Result := 0;
end;

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
