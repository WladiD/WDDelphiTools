// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.FormatALinker;

// Format-A linker: walks the RSM byte buffer for $2A type-registry
// entries and $2C field records (both Format A encodings), populates
// the type-id-to-class index, and patches each class member's TypeIdx
// / PrimitiveTypeId. Counterpart to the scanner's Format-B field walk
// (offset-only, in TRsmStructDiscoverer); both encodings sit in the
// same byte stream and together give the reader full member-name +
// member-type information.
//
// Outputs (mutated in place):
//   - ARsmTypeIdToClassIdx : 2-byte RSM type id -> Classes index.
//   - ATypeIdByName        : lowercased type name -> 2-byte RSM id.
//   - AClasses             : each member's TypeIdx / PrimitiveTypeId
//                            patched in-place; spurious members from
//                            the Format-B over-collection are pruned.

interface

uses
  System.SysUtils,

  mormot.core.base,
  mormot.core.collections,

  DPT.Rsm.Model;

type

  /// <summary>
  ///   Second-pass linker that walks the RSM byte stream for Format-A
  ///   records ($2A type-registry + $2C field) and joins the parsed
  ///   data against the already-discovered <c>Classes</c> / member
  ///   lists. Stateless across runs apart from the (buf, sz) and the
  ///   per-run "confirmed members" scratch set cached at the start
  ///   of each <see cref="Run"/>.
  /// </summary>
  TRsmFormatALinker = class
  private
    type
      TRawId = record Lo, Hi: Byte; end;
    var
      FBuf                : PByte;
      FSz                 : NativeInt;
      FClasses            : IList<TRsmClassInfo>;
      FClassByName        : IKeyValue<String, Integer>;
      FRsmTypeIdToClassIdx: IKeyValue<UInt32, Integer>;
      FTypeIdByName       : IKeyValue<String, UInt32>;
      /// Format-A-confirmed (classIdx, field-name) pairs. The backward
      /// Format-B scan over-collects field candidates from neighbouring
      /// class declarations because its window is fixed, so we keep an
      /// authoritative ownership map here and use it to prune Members
      /// before downstream code (parent derivation, evaluator) sees them.
      /// Stored as a key/value set keyed on "<classIdx>:<lower-field-name>"
      /// so lookups are O(1) instead of O(N): on large binaries the
      /// earlier linear scan turned PruneSpuriousMembers into the single
      /// biggest hot spot, blowing past 30 seconds for 800MB+ files.
      FConfirmed          : IKeyValue<String, Boolean>;
      /// Sorted byte offsets of every skRecord's $0E name marker
      /// + parallel array of Classes indices. Used by the
      /// build-map walker to translate a $0E sentinel position
      /// back to a class index in O(log N).
      FRecordOffsets      : TArray<UInt32>;
      FOffsetToClsIdx     : TArray<Integer>;
      /// Per-block ownership bridge: each $2C block's starting
      /// byte offset paired with the FClasses index of the
      /// record that owns the block's fields. Built by
      /// <see cref="BuildBlockOwnerIndex"/> by pairing each unit's
      /// contiguous $2C field-blocks with its $0E record
      /// declarations in source-declaration order. A linear /
      /// binary lookup over <c>FBlockStarts</c> resolves any
      /// $2C record's owner without needing the unit-local
      /// parent id at all, which is what the TFW
      /// UserKonsOutlook fields need (their unit-local ids
      /// collide across other units, so a global id->record
      /// map would last-wins to the wrong class).
      FBlockStarts        : TArray<UInt32>;
      FBlockOwners        : TArray<Integer>;
    function  ByteAt(AOffset: NativeInt): Byte; inline;
    function  ReadIdentifier(AOffset: NativeInt; out AName: String): Boolean; inline;
    function  RawIdKey(const ARaw: TRawId): UInt32; inline;
    function  ConfirmedKey(AClsIdx: Integer; const AFieldName: String): String;
    procedure ConfirmedAdd(AClsIdx: Integer; const AFieldName: String);
    function  IsConfirmed(AClsIdx: Integer; const AFieldName: String): Boolean;
    function  FindClassIdxByName(const AName: String): Integer;
    function  FindClassIdxForRawId(const ARaw: TRawId): Integer;
    procedure BuildRecordOffsetIndex;
    function  FindRecordAtOffset(AOffset: NativeInt): Integer;
    procedure BuildBlockOwnerIndex;
    function  FindBlockOwnerAt(AOffset: UInt32): Integer;
    procedure ScanTypeRegistry;
    procedure LinkFieldsFromFormatA;
    procedure PruneSpuriousMembers;
  public
    constructor Create(
      AClasses             : IList<TRsmClassInfo>;
      AClassByName         : IKeyValue<String, Integer>;
      ARsmTypeIdToClassIdx : IKeyValue<UInt32, Integer>;
      ATypeIdByName        : IKeyValue<String, UInt32>);

    /// <summary>
    ///   Run the three Format-A passes (type-registry scan, field
    ///   linking, spurious-member prune) over the supplied buffer.
    ///   Caller is responsible for clearing the output containers
    ///   beforehand if needed (typically already done by the reader's
    ///   <c>RunPostProcess</c> at the start of every load).
    /// </summary>
    procedure Run(ABuf: PByte; ASz: NativeInt);
  end;

implementation

uses
  DPT.Rsm.BufferIO;

{ TRsmFormatALinker }

constructor TRsmFormatALinker.Create(
  AClasses             : IList<TRsmClassInfo>;
  AClassByName         : IKeyValue<String, Integer>;
  ARsmTypeIdToClassIdx : IKeyValue<UInt32, Integer>;
  ATypeIdByName        : IKeyValue<String, UInt32>);
begin
  inherited Create;
  FClasses             := AClasses;
  FClassByName         := AClassByName;
  FRsmTypeIdToClassIdx := ARsmTypeIdToClassIdx;
  FTypeIdByName        := ATypeIdByName;
end;

function TRsmFormatALinker.ByteAt(AOffset: NativeInt): Byte;
begin
  Result := RsmByteAt(FBuf, AOffset);
end;

function TRsmFormatALinker.ReadIdentifier(AOffset: NativeInt;
  out AName: String): Boolean;
begin
  Result := RsmReadIdentifier(FBuf, FSz, AOffset, AName);
end;

function TRsmFormatALinker.RawIdKey(const ARaw: TRawId): UInt32;
begin
  Result := UInt32(ARaw.Lo) or (UInt32(ARaw.Hi) shl 8);
end;

function TRsmFormatALinker.ConfirmedKey(AClsIdx: Integer;
  const AFieldName: String): String;
begin
  Result := IntToStr(AClsIdx) + ':' + LowerCase(AFieldName);
end;

procedure TRsmFormatALinker.ConfirmedAdd(AClsIdx: Integer;
  const AFieldName: String);
begin
  FConfirmed[ConfirmedKey(AClsIdx, AFieldName)] := True;
end;

function TRsmFormatALinker.IsConfirmed(AClsIdx: Integer;
  const AFieldName: String): Boolean;
begin
  Result := FConfirmed.ContainsKey(ConfirmedKey(AClsIdx, AFieldName));
end;

function TRsmFormatALinker.FindClassIdxByName(const AName: String): Integer;
begin
  if not FClassByName.TryGetValue(LowerCase(AName), Result) then
    Result := -1;
end;

function TRsmFormatALinker.FindClassIdxForRawId(const ARaw: TRawId): Integer;
begin
  // Direct O(1) lookup via the registry-built hashmap. Returns
  // -1 if the raw id is unknown (built-in types or types we
  // didn't parse). Replaces the prior O(N^2) scan.
  if not FRsmTypeIdToClassIdx.TryGetValue(RawIdKey(ARaw), Result) then
    Result := -1;
end;

procedure TRsmFormatALinker.BuildRecordOffsetIndex;
var
  I, N, J: Integer;
begin
  N := 0;
  SetLength(FRecordOffsets,  FClasses.Count);
  SetLength(FOffsetToClsIdx, FClasses.Count);
  for I := 0 to FClasses.Count - 1 do
    if FClasses[I].Kind = skRecord then
    begin
      FRecordOffsets[N]  := FClasses[I].TypeIdx;
      FOffsetToClsIdx[N] := I;
      Inc(N);
    end;
  SetLength(FRecordOffsets,  N);
  SetLength(FOffsetToClsIdx, N);
  // Insertion sort -- the structural discoverer adds records in
  // byte-stream order so the input is virtually always already
  // sorted; the loop costs O(N) in the common path.
  for I := 1 to N - 1 do
  begin
    if FRecordOffsets[I] >= FRecordOffsets[I - 1] then Continue;
    var Off: UInt32  := FRecordOffsets[I];
    var Cls: Integer := FOffsetToClsIdx[I];
    J := I - 1;
    while (J >= 0) and (FRecordOffsets[J] > Off) do
    begin
      FRecordOffsets[J + 1]  := FRecordOffsets[J];
      FOffsetToClsIdx[J + 1] := FOffsetToClsIdx[J];
      Dec(J);
    end;
    FRecordOffsets[J + 1]  := Off;
    FOffsetToClsIdx[J + 1] := Cls;
  end;
end;

function TRsmFormatALinker.FindRecordAtOffset(AOffset: NativeInt): Integer;
// Returns the FClasses index whose record name marker sits at the
// supplied byte offset, or -1 if no record starts there.
var
  Lo, Hi, Mid: Integer;
begin
  Result := -1;
  Lo := 0;
  Hi := Length(FRecordOffsets) - 1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) shr 1;
    if FRecordOffsets[Mid] = UInt32(AOffset) then
      Exit(FOffsetToClsIdx[Mid]);
    if FRecordOffsets[Mid] < UInt32(AOffset) then
      Lo := Mid + 1
    else
      Hi := Mid - 1;
  end;
end;

procedure TRsmFormatALinker.BuildBlockOwnerIndex;
// Single pass over the RSM that pairs each $2C block's start
// offset with the FClasses index of the owning record. The
// Delphi compiler emits a unit's contents in this order:
//
//   1. All $2C field-record blocks for the unit's records,
//      grouped by parent local id, in source declaration order.
//   2. The unit's type table (enum defs, pointer types, etc.).
//   3. All $0E record name markers for the unit's records, in
//      the same source declaration order.
//
// So within a single unit, the i-th $2C block in the byte
// stream maps to the i-th $0E record we see after the block
// stream ends. A new $2C block (no $FF prefix) after $0E
// records have started means we crossed into the next unit;
// flush the pending pairings and start fresh.
//
// We record (BlockStartOffset, OwningClassIdx) for each
// successfully paired block, then sort by offset so the linker
// can binary-search the block any $2C record belongs to
// without needing the unit-local parent id at all -- which
// matters because those local ids collide across units (TFW's
// "$44" is TUserKonsOutlook in one unit and a totally different
// class elsewhere; a global LocalId->ClassIdx map last-wins to
// the wrong record).
const
  STATE_NEUTRAL = 0;
  STATE_FIELDS  = 1;
  STATE_RECORDS = 2;
var
  P             : NativeInt;
  BlockStarts   : IList<UInt32>;
  RecordClsIdxs : IList<Integer>;
  AllStarts     : IList<UInt32>;
  AllOwners     : IList<Integer>;
  LastLocalId   : Integer;
  State         : Integer;
  I, J          : Integer;

  procedure FlushPairings;
  var
    Lim, K: Integer;
  begin
    Lim := BlockStarts.Count;
    if RecordClsIdxs.Count < Lim then Lim := RecordClsIdxs.Count;
    for K := 0 to Lim - 1 do
    begin
      AllStarts.Add(BlockStarts[K]);
      AllOwners.Add(RecordClsIdxs[K]);
    end;
    BlockStarts.Clear;
    RecordClsIdxs.Clear;
    LastLocalId := -1;
  end;

begin
  FBlockStarts := nil;
  FBlockOwners := nil;
  if (FBuf = nil) or (FSz < 16) then Exit;
  BlockStarts   := Collections.NewPlainList<UInt32>;
  RecordClsIdxs := Collections.NewPlainList<Integer>;
  AllStarts     := Collections.NewPlainList<UInt32>;
  AllOwners     := Collections.NewPlainList<Integer>;
  LastLocalId := -1;
  State := STATE_NEUTRAL;
  P := 1;
  while P + 32 < FSz do
  begin
    var B: Byte := ByteAt(P);
    if (B = $2C) and (ByteAt(P - 1) <> $FF) then
    begin
      // Candidate new $2C block start. Validate with the same
      // payload-prefix + terminator filter the main linker uses
      // so binary noise that incidentally contains a $2C byte
      // doesn't poison the pairing.
      var NL: Integer := ByteAt(P + 1);
      if (NL < 2) or (NL > 40) or (P + 2 + NL + 12 >= FSz) then
      begin
        Inc(P);
        Continue;
      end;
      var After: Integer := P + 2 + NL;
      if (ByteAt(After) <> $00) or (ByteAt(After + 1) <> $02) or
         (ByteAt(After + 2) <> $00) then
      begin
        Inc(P);
        Continue;
      end;
      var EndOff: Integer := -1;
      for I := After + 5 to After + 30 do
      begin
        if I + 5 >= FSz then Break;
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
      var Lo: Byte := ByteAt(EndOff + 4);
      var Hi: Byte := ByteAt(EndOff + 5);
      // Only the narrow encoding (Hi==$FF as next-record
      // continuation marker, parent id is just the byte Lo)
      // needs the block-owner fallback. The wide encoding's
      // parent id is in the $2A registry and the linker's
      // primary path handles it; tracking it here would
      // pollute the block-by-block pairing with class-typed
      // blocks (DebugTarget) whose ids overlap with our
      // narrow id space.
      if Hi <> $FF then
      begin
        P := EndOff + 6;
        Continue;
      end;
      var LocalId: Integer := Lo;
      if LocalId <> LastLocalId then
      begin
        // New block. If we'd been collecting $0E records, the
        // new block crosses into the next unit; pair what we
        // have and reset.
        if State = STATE_RECORDS then FlushPairings;
        BlockStarts.Add(UInt32(P));
        LastLocalId := LocalId;
        State := STATE_FIELDS;
      end;
      P := EndOff + 6;
      Continue;
    end;
    if B = $0E then
    begin
      // TRsmClassInfo.TypeIdx stores the NAME-LENGTH byte
      // position (one past the $0E sentinel itself, where the
      // discoverer parses the identifier from). Match that by
      // probing P+1 instead of P.
      var ClsIdx: Integer := FindRecordAtOffset(P + 1);
      // Only pair records that ACTUALLY have fields in the
      // structural view. Records without fields (or with only
      // managed-type filler) produce no $2C block, so counting
      // them here would skew the pairing -- every empty record
      // we accumulate steals a slot from a real one.
      if (ClsIdx >= 0) and (FClasses[ClsIdx].Members.Count > 0) then
      begin
        RecordClsIdxs.Add(ClsIdx);
        State := STATE_RECORDS;
        var NL: Integer := ByteAt(P + 1);
        if (NL > 0) and (P + 2 + NL < FSz) then
          P := P + 2 + NL
        else
          Inc(P);
        Continue;
      end;
    end;
    Inc(P);
  end;
  FlushPairings;
  // Materialise the parallel sorted arrays. AllStarts is built
  // in byte-stream order so it's already sorted in the common
  // path; insertion sort handles any out-of-order entries (which
  // shouldn't happen but the cost is O(N) when sorted).
  SetLength(FBlockStarts, AllStarts.Count);
  SetLength(FBlockOwners, AllStarts.Count);
  for I := 0 to AllStarts.Count - 1 do
  begin
    FBlockStarts[I] := AllStarts[I];
    FBlockOwners[I] := AllOwners[I];
  end;
  for I := 1 to Length(FBlockStarts) - 1 do
  begin
    if FBlockStarts[I] >= FBlockStarts[I - 1] then Continue;
    var Off: UInt32  := FBlockStarts[I];
    var Cls: Integer := FBlockOwners[I];
    J := I - 1;
    while (J >= 0) and (FBlockStarts[J] > Off) do
    begin
      FBlockStarts[J + 1] := FBlockStarts[J];
      FBlockOwners[J + 1] := FBlockOwners[J];
      Dec(J);
    end;
    FBlockStarts[J + 1] := Off;
    FBlockOwners[J + 1] := Cls;
  end;
end;

function TRsmFormatALinker.FindBlockOwnerAt(AOffset: UInt32): Integer;
// Binary search for the largest BlockStart <= AOffset. Returns
// the parallel BlockOwner. -1 if no block contains this offset
// (offset is before the first block, or no blocks were paired).
var
  Lo, Hi, Mid, Best: Integer;
begin
  Result := -1;
  Lo := 0;
  Hi := Length(FBlockStarts) - 1;
  Best := -1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) shr 1;
    if FBlockStarts[Mid] <= AOffset then
    begin
      Best := Mid;
      Lo := Mid + 1;
    end
    else
      Hi := Mid - 1;
  end;
  if Best >= 0 then
    Result := FBlockOwners[Best];
end;


procedure TRsmFormatALinker.ScanTypeRegistry;
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
  while P + 12 < FSz do
  begin
    Skip := ByteScanIndex(PByteArray(FBuf + P), FSz - P - 12, TRsmTag.TYPE_REGISTRY_TAG);
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
       (P + 2 + NL + 5 < FSz) and
       (ByteAt(P + 2) = Byte(Ord('T'))) and
       (ByteAt(P + 2 + NL + 1) = $00) and
       (ByteAt(P + 2 + NL + 2) = $00) and
       ReadIdentifier(P + 1, Name) then
    begin
      Id.Lo := ByteAt(P + 2 + NL + 3);
      Id.Hi := ByteAt(P + 2 + NL + 4);
      ClsIdx := FindClassIdxByName(Name);
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

procedure TRsmFormatALinker.LinkFieldsFromFormatA;
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
  while P + 24 < FSz do
  begin
    Skip := ByteScanIndex(PByteArray(FBuf + P), FSz - P - 24, $2C);
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
    if (NL < 2) or (NL > 40) or (TagOff + 2 + NL + 7 >= FSz) then
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
    if not ReadIdentifier(TagOff + 1, Name) then
    begin
      Inc(P);
      Continue;
    end;
    // Final size guard for the rest of the field record (a
    // bounded forward window plus the 2-byte parent id).
    if After + 5 + 6 + 2 > FSz then
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
      if I + 5 > FSz then Break;
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
    // Resolve the owning class/record. Two paths, tried in
    // order:
    //   1. $2A registry id (Wide encoding only). This is the
    //      common DebugTarget case and the cross-unit-resolvable
    //      case where the parent id ended up in the global
    //      registry. The 1-byte form (Hi=$FF) skips this because
    //      $FF would be misread as a high byte, mapping to a
    //      bogus class on a registry hit.
    //   2. Byte-stream block-owner index. Built by pairing each
    //      unit's contiguous $2C field-blocks with its $0E
    //      record name markers in source-declaration order;
    //      handles the TFW UserKonsOutlook case where the unit-
    //      local parent id ($44, $48, ...) collides with other
    //      units' classes in the registry and the registry
    //      lookup would return the wrong record.
    var ParentLoB: Byte := ByteAt(EndOff + 4);
    var ParentHiB: Byte := ByteAt(EndOff + 5);
    ParentIdx := -1;
    if ParentHiB <> $FF then
    begin
      ParentId.Lo := ParentLoB;
      ParentId.Hi := ParentHiB;
      ParentIdx := FindClassIdxForRawId(ParentId);
    end;
    if ParentIdx < 0 then
    begin
      ParentIdx := FindBlockOwnerAt(UInt32(TagOff));
      if ParentIdx < 0 then
      begin
        Inc(P);
        Continue;
      end;
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
              (UInt16(ByteAt(After + 4)) shl 8)
          // Same shape as the BodyLen>=10 branch above, but with
          // a TWO-byte separator between the field's type id and
          // the "$9C $01" marker (marker at +7..+8 instead of
          // +6..+7). The compiler emits this longer separator
          // when the field's byte offset within the record
          // doesn't fit in the shorter form (observed for fields
          // at record offsets >= 256: CalendarID at 259,
          // SyncDirection at 771, etc). Without this branch the
          // linker leaves Member.PrimitiveTypeId at 0 for every
          // enum/string field past a 256-byte boundary, which
          // sends the evaluator's auto-detect chain into the
          // name-based fallback -- the source of the TFW
          // UserKonsOutlook.SyncDirection misroute.
          else if (BodyLen >= 11) and
                  (ByteAt(After + 7) = $9C) and
                  (ByteAt(After + 8) = $01) then
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

procedure TRsmFormatALinker.PruneSpuriousMembers;
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

procedure TRsmFormatALinker.Run(ABuf: PByte; ASz: NativeInt);
begin
  FBuf := ABuf;
  FSz  := ASz;
  if (FBuf = nil) or (FSz < 8) then Exit;
  FConfirmed := Collections.NewPlainKeyValue<String, Boolean>;
  ScanTypeRegistry;
  BuildRecordOffsetIndex;
  BuildBlockOwnerIndex;
  LinkFieldsFromFormatA;
  PruneSpuriousMembers;
end;

end.
