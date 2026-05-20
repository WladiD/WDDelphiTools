// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.Scanner;

// Byte-stream scanner for the RSM (CSH7) symbol container. This unit
// owns:
//   * The TMemoryMap lifecycle (so the buffer remains live as long
//     as the scanner exists -- post-process in DPT.Rsm.Reader walks
//     the same bytes after the scan loop returns).
//   * The byte-stream helpers (ByteAt, DwordAt, ReadIdentifier,
//     IsPrintableAscii, IsValidFieldTypeinfoPrefix). These are
//     exposed publicly so the reader's post-process passes can
//     reuse them without copy-pasting.
//   * The four scan-phase methods that walk the buffer top-to-bottom:
//       1. ScanSymbolStream      -- procs, locals, params, globals,
//                                   enum-constant records, type-
//                                   registry entries.
//       2. RecomputeProcSizes    -- derive each proc's Size from the
//                                   gap to the next proc.
//       3. DiscoverAndParseAllStructs -- class + record definitions
//                                   with their member fields.
// The scanner populates a collection of mORMot IList<T> /
// IKeyValue<K,V> containers, exposed as read-write properties so the
// reader can hand them through to lookup callers and run further
// post-process that may set additional fields (e.g. ParentName).

interface

uses
  System.Classes,
  System.SysUtils,

  mormot.core.base,
  mormot.core.collections,
  mormot.core.os,

  DPT.Rsm.Model;

type

  /// <summary>
  ///   Owns the raw RSM byte buffer and the collections that the
  ///   scan phases populate. After <see cref="LoadFromFile"/> or
  ///   <see cref="LoadFromBuffer"/> returns, the buffer is still
  ///   alive (the memory map is held by the scanner) and the
  ///   collections are filled with the per-tag scan output; the
  ///   reader runs its own post-process on this data.
  /// </summary>
  TRsmScanner = class
  private
    FBuf         : PByte;
    FSz          : NativeInt;
    FMapping     : TMemoryMap;
    FOwnsMapping : Boolean;
    FProcs       : IList<TRsmProc>;
    FClasses     : IList<TRsmClassInfo>;
    /// Case-insensitive name -> FProcs/FClasses index. Built during
    /// the scan so the inner loops collapse from O(N) per lookup to
    /// O(1) (and the outer scan from O(N^2) to O(N)).
    FProcByName  : IKeyValue<String, Integer>;
    FClassByName : IKeyValue<String, Integer>;
    /// Module-level variables: name -> RSM 2-byte type id.
    FGlobalByName: IKeyValue<String, UInt32>;
    /// Module-level variables: name -> file offset of the $20/$27
    /// record. The reader's proximity-based record-type resolver
    /// joins records against this to find the right type when the
    /// encoded type id is unreliable.
    FGlobalFileOffset: IKeyValue<String, NativeInt>;
    /// Maps "<enumTypeId>:<ordinal>" -> enum-constant identifier name.
    /// Populated by ScanSymbolStream from $25 records.
    FEnumConstNames : IKeyValue<String, String>;
    /// All RSM type ids that appear as enums (in $25 or via $2A
    /// alias linking). The "is this an enum?" oracle.
    FEnumTypeIds    : IKeyValue<UInt32, Boolean>;
    /// Subset of FEnumTypeIds populated ONLY from the cross-unit $25
    /// form. Used as the filter set when scanning $2A bodies for
    /// secondaries; avoids false-matches with program-local enum ids
    /// whose $2E hi byte coincidentally appears in many class bodies.
    FCrossUnitEnumIds: IKeyValue<UInt32, Boolean>;
    /// Maps an enum alias id (the type id used in $2A primary slot
    /// or in a class-field record) to the LIST of canonical ids
    /// under which (typeId, ordinal) -> name pairs might be stored
    /// in FEnumConstNames.
    FEnumAliasesByPrimary: IKeyValue<UInt32, IList<UInt32>>;
    FOnPhase     : TProc<String>;
    procedure ReportPhase(const APhase: String); inline;
    procedure ScanSymbolStream;
    procedure DiscoverAndParseAllStructs;
    procedure ScanFieldsBackwardFromClassName(AClassNameOff: NativeInt;
      AKind: TRsmStructKind; const AClassName: String;
      AMinStartOff: NativeInt = 0);
    procedure ScanFieldsForwardFromRecordName(ARecordNameOff: NativeInt;
      const ARecordName: String);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Optional progress callback fired by LoadFromBuffer at each
    ///   major parsing phase. Lets callers surface what the parser
    ///   is doing on very large RSM files, where a single phase can
    ///   run for many seconds and otherwise looks indistinguishable
    ///   from a hang.
    /// </summary>
    property OnPhase: TProc<String> read FOnPhase write FOnPhase;

    procedure LoadFromFile(const AExePath: String);
    procedure LoadFromBytes(const ABytes: TBytes);
    procedure LoadFromBuffer(ABuf: PByte; ASize: NativeInt);

    /// <summary>
    ///   Recompute the Size field of every proc as the gap to the
    ///   next proc by SegmentOffset. Run automatically from
    ///   LoadFromBuffer; also exposed publicly because callers may
    ///   patch each proc's SegmentOffset from a side channel
    ///   (e.g. the .map file) and then invoke this method to
    ///   refresh sizes.
    /// </summary>
    procedure RecomputeProcSizes;

    /// <summary>
    ///   Length-prefixed identifier reader. Returns False when the
    ///   length is implausible for a Delphi identifier or any byte
    ///   in the run is not an identifier character.
    /// </summary>
    function ReadIdentifier(AOffset: NativeInt; out AName: String): Boolean;
    /// <summary>
    ///   Single byte at the given offset within the loaded buffer.
    ///   No bounds check (callers verify offset).
    /// </summary>
    function ByteAt(AOffset: NativeInt): Byte; inline;
    /// <summary>
    ///   Little-endian DWORD at the given offset within the loaded
    ///   buffer. No bounds check.
    /// </summary>
    function DwordAt(AOffset: NativeInt): UInt32; inline;
    /// <summary>
    ///   True for bytes that may appear in an RSM identifier (digits,
    ///   ASCII letters, plus the qualified-name / generic / linker-
    ///   alias punctuation: '.', '_', '$', '@', '&lt;', '&gt;', ',').
    /// </summary>
    function IsPrintableAscii(AB: Byte): Boolean; inline;
    /// <summary>
    ///   Validates the 6 bytes that follow a record-field's name.
    ///   Anchors the field-walker on structural shape rather than the
    ///   field name's first character, so records whose fields don't
    ///   use the F-prefix convention parse correctly.
    /// </summary>
    function IsValidFieldTypeinfoPrefix(AOffset: NativeInt): Boolean;

    /// Loaded buffer base pointer. Lives until the next LoadFrom*
    /// call or the scanner's destruction.
    property Buf: PByte read FBuf;
    /// Loaded buffer size in bytes.
    property Sz : NativeInt read FSz;

    property Procs              : IList<TRsmProc> read FProcs;
    property Classes            : IList<TRsmClassInfo> read FClasses;
    property ProcByName         : IKeyValue<String, Integer> read FProcByName;
    property ClassByName        : IKeyValue<String, Integer> read FClassByName;
    property GlobalByName       : IKeyValue<String, UInt32> read FGlobalByName;
    property GlobalFileOffset   : IKeyValue<String, NativeInt> read FGlobalFileOffset;
    property EnumConstNames     : IKeyValue<String, String> read FEnumConstNames;
    property EnumTypeIds        : IKeyValue<UInt32, Boolean> read FEnumTypeIds;
    property CrossUnitEnumIds   : IKeyValue<UInt32, Boolean> read FCrossUnitEnumIds;
    property EnumAliasesByPrimary: IKeyValue<UInt32, IList<UInt32>> read FEnumAliasesByPrimary;
  end;

implementation

function CompareProcBySegmentOffset(const A, B): Integer;
var
  Sa, Sb: NativeUInt;
begin
  Sa := TRsmProc(A).SegmentOffset;
  Sb := TRsmProc(B).SegmentOffset;
  Result := Ord(Sa > Sb) - Ord(Sa < Sb);
end;

{ TRsmScanner }

constructor TRsmScanner.Create;
begin
  inherited Create;
  FProcs       := Collections.NewPlainList<TRsmProc>;
  FClasses     := Collections.NewPlainList<TRsmClassInfo>;
  FProcByName  := Collections.NewPlainKeyValue<String, Integer>;
  FClassByName := Collections.NewPlainKeyValue<String, Integer>;
  FGlobalByName := Collections.NewPlainKeyValue<String, UInt32>;
  FGlobalFileOffset := Collections.NewPlainKeyValue<String, NativeInt>;
  FEnumConstNames := Collections.NewPlainKeyValue<String, String>;
  FEnumTypeIds    := Collections.NewPlainKeyValue<UInt32, Boolean>;
  FEnumAliasesByPrimary := Collections.NewPlainKeyValue<UInt32, IList<UInt32>>;
  FCrossUnitEnumIds := Collections.NewPlainKeyValue<UInt32, Boolean>;
end;

destructor TRsmScanner.Destroy;
begin
  if FOwnsMapping then
    FMapping.UnMap;
  inherited;
end;

procedure TRsmScanner.ReportPhase(const APhase: String);
begin
  if Assigned(FOnPhase) then
    FOnPhase(APhase);
end;

function TRsmScanner.ByteAt(AOffset: NativeInt): Byte;
begin
  Result := (FBuf + AOffset)^;
end;

function TRsmScanner.DwordAt(AOffset: NativeInt): UInt32;
begin
  Result := PUInt32(FBuf + AOffset)^;
end;

function TRsmScanner.IsPrintableAscii(AB: Byte): Boolean;
begin
  // Identifier bytes accepted inside RSM length-prefixed names.
  // The CSH7 emitter writes names verbatim from Delphi source, but
  // a "name" can be qualified or compiler-decorated:
  //   - 'TFormMain.Create'                  -- class-method PROC records
  //   - 'TFormMain.Create$ActRec'           -- closure / nested-func record
  //   - 'TList<TFoo>.Add'                   -- generic instantiation
  //   - '@MyName'                           -- linker-emitted alias
  //   - 'TMap<TKey,TValue>.Get'             -- multi-arg generics (comma)
  // Keeping the check tight (no whitespace, no control bytes) limits
  // false-positive PROC records when a random byte run happens to
  // pass the length-prefix shape, while still letting real qualified
  // names through.
  Result := ((AB >= Ord('A')) and (AB <= Ord('Z'))) or
            ((AB >= Ord('a')) and (AB <= Ord('z'))) or
            ((AB >= Ord('0')) and (AB <= Ord('9'))) or
            (AB = Ord('_')) or
            (AB = Ord('.')) or
            (AB = Ord('$')) or
            (AB = Ord('<')) or
            (AB = Ord('>')) or
            (AB = Ord(',')) or
            (AB = Ord('@'));
end;

function TRsmScanner.ReadIdentifier(AOffset: NativeInt; out AName: String): Boolean;
var
  L, I: Integer;
  Buf2: TBytes;
begin
  Result := False;
  AName := '';
  if (AOffset < 0) or (AOffset >= FSz) then Exit;
  L := ByteAt(AOffset);
  if (L < 1) or (L > 64) then Exit;
  if AOffset + 1 + L > FSz then Exit;
  for I := 0 to L - 1 do
    if not IsPrintableAscii(ByteAt(AOffset + 1 + I)) then Exit;
  SetLength(Buf2, L);
  Move((FBuf + AOffset + 1)^, Buf2[0], L);
  AName := TEncoding.ANSI.GetString(Buf2);
  Result := True;
end;

function TRsmScanner.IsValidFieldTypeinfoPrefix(AOffset: NativeInt): Boolean;
// Validates the 6 bytes that follow a record-field's name. The
// RSM emits every field with the exact prefix
//   $02 $00 <last-flag> $00 $00 $00       (last-flag: $00 or $02)
// where last-flag = $02 marks the record's terminal field. This
// structural anchor is far more selective than any first-character
// heuristic on the field name (e.g. requiring 'F' as the Delphi
// house-style prefix would drop every TFW record whose source uses
// a different convention -- TAppCaps.DbKindName, TMdt.Id, ...).
begin
  Result := False;
  if AOffset + 5 >= FSz then Exit;
  if ByteAt(AOffset)     <> $02 then Exit;
  if ByteAt(AOffset + 1) <> $00 then Exit;
  if (ByteAt(AOffset + 2) <> $00) and (ByteAt(AOffset + 2) <> $02) then Exit;
  if ByteAt(AOffset + 3) <> $00 then Exit;
  if ByteAt(AOffset + 4) <> $00 then Exit;
  if ByteAt(AOffset + 5) <> $00 then Exit;
  Result := True;
end;

procedure TRsmScanner.LoadFromFile(const AExePath: String);
var
  RsmPath: String;
begin
  if FOwnsMapping then
  begin
    FMapping.UnMap;
    FOwnsMapping := False;
  end;
  RsmPath := ChangeFileExt(AExePath, '.rsm');
  if not FileExists(RsmPath) then
    Exit;
  // Map(filename, aForceMap=true) keeps the mapping alive for the
  // entire scanner lifetime. The post-process pass in the reader
  // walks the same bytes after this method returns, so the mapping
  // MUST outlive LoadFromBuffer. aForceMap forces mmap even for
  // small files (the heuristic in mORMot would otherwise prefer a
  // heap copy for ones < some threshold, but we don't care --
  // even a small RSM benefits from skipping the copy).
  if not FMapping.Map(RsmPath, {aForceMap=}True) then
    Exit;
  FOwnsMapping := True;
  ReportPhase('read file');
  LoadFromBuffer(PByte(FMapping.Buffer), FMapping.Size);
end;

procedure TRsmScanner.LoadFromBytes(const ABytes: TBytes);
begin
  if Length(ABytes) = 0 then
    LoadFromBuffer(nil, 0)
  else
    LoadFromBuffer(PByte(@ABytes[0]), Length(ABytes));
end;

procedure TRsmScanner.LoadFromBuffer(ABuf: PByte; ASize: NativeInt);
begin
  FProcs.Clear;
  FClasses.Clear;
  FProcByName.Clear;
  FClassByName.Clear;
  FGlobalByName.Clear;
  FGlobalFileOffset.Clear;
  FEnumConstNames.Clear;
  FEnumTypeIds.Clear;
  FCrossUnitEnumIds.Clear;
  FEnumAliasesByPrimary.Clear;
  FBuf := ABuf;
  FSz  := ASize;
  if (ABuf = nil) or (ASize < 8) then
    Exit;
  if PUInt32(ABuf)^ <> TRsmTag.SigCSH7 then
    Exit;

  // Each ReportPhase reports the wall-clock time elapsed since the
  // PREVIOUS ReportPhase call -- so the label must name the phase
  // that JUST finished, not the next one.
  ScanSymbolStream;
  ReportPhase('ScanSymbolStream');
  RecomputeProcSizes;
  ReportPhase('RecomputeProcSizes');
  DiscoverAndParseAllStructs;
  ReportPhase('DiscoverAndParseAllStructs');
end;

procedure TRsmScanner.ScanFieldsBackwardFromClassName(AClassNameOff: NativeInt;
  AKind: TRsmStructKind; const AClassName: String;
  AMinStartOff: NativeInt = 0);
// Scan a fixed window of bytes BEFORE the class-trailer position
// looking for <DWORD-offset-LE> <namelen> <name> patterns. The
// class trailer (the bytes between the last field's name and the
// class name itself) varies in length but is always small, so a
// wider window catches whatever sits between the fields and the
// class name. Each <offset, namelen, name> triple is recorded as
// a candidate; duplicates and overlaps are filtered out, and the
// surviving members are sorted by offset to give source-declaration
// order back.
//
// AMinStartOff caps the backward scan: bytes at or below this
// offset are NOT considered. This prevents a class's scan from
// reaching across the previous class's class-def region (which on
// closely-packed classes like TDerived -> TDeepDerived would
// otherwise leak the previous class's own fields into the next
// class's member list, corrupting the FirstOffs used by
// DeriveClassParents and producing an entirely wrong inheritance
// chain). Callers pass the previous class's TypeIdx; pass 0 (the
// default) for the very first class in the file.
const
  MaxFields    = 128;
  // Generous backward window so the scan reaches whatever
  // distance separates the class anchor from its earliest field
  // record. The AMinStartOff cap at the previous class's
  // TypeIdx keeps the window from leaking across class
  // boundaries no matter how large this constant is, so the
  // effective per-class scan is naturally bounded by class
  // packing. RTL classes such as TStrings emit ~6 KB of method
  // records between their last field and the class trailer
  // (TStringList similar), and TComponent has ~1 KB; 64 KB
  // comfortably covers all observed cases.
  ScanWindow   = 65536;
type
  TCandidate = record
    Pos    : NativeInt; // start of the offset DWORD
    Off    : UInt32;
    Name   : String;
  end;
var
  Cands    : array[0..MaxFields - 1] of TCandidate;
  Count    : Integer;
  StartOff : NativeInt;
  P        : NativeInt;
  NameLen  : Byte;
  Name     : String;
  Off      : UInt32;
  Info     : TRsmClassInfo;
  List     : IList<TRsmClassMember>;
  I, J     : Integer;
  Tmp      : TCandidate;
  Member   : TRsmClassMember;
  Skip     : Boolean;
begin
  Count := 0;
  StartOff := AClassNameOff - ScanWindow;
  if StartOff < 0 then StartOff := 0;
  if StartOff < AMinStartOff then StartOff := AMinStartOff;

  // Walk forward across the candidate window, recording every
  // position that successfully parses as <DWORD offset> <namelen>
  // <printable-name>. The DWORD-offset gate (must be small and the
  // high two bytes must be zero) keeps random byte sequences from
  // matching.
  P := StartOff;
  while (P + 5 < AClassNameOff) and (Count < MaxFields) do
  begin
    Off := DwordAt(P);
    if Off > $FFFF then
    begin
      Inc(P);
      Continue;
    end;
    NameLen := ByteAt(P + 4);
    if (NameLen < 1) or (NameLen > 40) then
    begin
      Inc(P);
      Continue;
    end;
    if not ReadIdentifier(P + 4, Name) then
    begin
      Inc(P);
      Continue;
    end;
    // Heuristic guard: a real Delphi field name starts with 'F'
    // by convention. This rules out non-field length-prefixed
    // strings (e.g. unit names, class-trailer text fragments)
    // that happen to sit within the scan window.
    if (Length(Name) < 1) or (Name[1] <> 'F') then
    begin
      Inc(P);
      Continue;
    end;
    // Reject when this match overlaps an earlier-recorded one
    // (e.g. when the same span produces both <off, name> and
    // <off+1, name>-shifted). Earlier wins.
    Skip := False;
    for I := 0 to Count - 1 do
      if Cands[I].Pos + 5 + Length(Cands[I].Name) > P then
      begin
        Skip := True;
        Break;
      end;
    if Skip then
    begin
      Inc(P);
      Continue;
    end;
    Cands[Count].Pos  := P;
    Cands[Count].Off  := Off;
    Cands[Count].Name := Name;
    Inc(Count);
    Inc(P);
  end;

  // Sort by offset (ascending) so members come out in declaration
  // order, regardless of the byte-stream order.
  for I := 0 to Count - 2 do
    for J := I + 1 to Count - 1 do
      if Cands[J].Off < Cands[I].Off then
      begin
        Tmp := Cands[I];
        Cands[I] := Cands[J];
        Cands[J] := Tmp;
      end;

  List := Collections.NewPlainList<TRsmClassMember>;
  for I := 0 to Count - 1 do
  begin
    Member := Default(TRsmClassMember);
    Member.Name    := Cands[I].Name;
    Member.Offset  := Cands[I].Off;
    Member.TypeIdx := 0;
    // Member size = gap to the next member's offset. The members
    // are already sorted by offset above, so the gap is the natural
    // proxy for sub-DWORD field widths (Byte, Word, ...). Last
    // member's size stays 0 (unknown -- caller falls back to the
    // user-requested type's width).
    if (I + 1 < Count) and (Cands[I + 1].Off > Cands[I].Off) then
      Member.Size := Cands[I + 1].Off - Cands[I].Off
    else
      Member.Size := 0;
    List.Add(Member);
  end;

  Info := Default(TRsmClassInfo);
  Info.Name    := AClassName;
  Info.TypeIdx := UInt32(AClassNameOff);
  Info.Kind    := AKind;
  Info.Members := List;
  FClassByName[LowerCase(AClassName)] := FClasses.Count;
  FClasses.Add(Info);
end;

procedure TRsmScanner.ScanFieldsForwardFromRecordName(ARecordNameOff: NativeInt;
  const ARecordName: String);
// Records emit their fields after the name in declaration order.
// Each field record consists of:
//
//   <$02> <namelen> <name>                  (tag + length-prefixed name)
//   <$02 $00 <last-flag> $00 $00 $00>       (6-byte type-info; last-flag
//                                            is $00 for non-terminal
//                                            fields, $02 for the last)
//
// Non-terminal fields are followed by a next-field-offset DWORD
// that gives the byte offset of the following field within the
// record. The offset's exact position varies by platform:
//
//   * Win32: <DWORD next-offset> immediately after the typeinfo
//     (4 bytes after typeinfo end).
//   * Win64: <4 zero bytes> <DWORD next-offset> <4 zero bytes>
//     (12 bytes after typeinfo end). The leading zero pad means
//     a quick check on byte+4 lets us pick the right layout: $02
//     there is the next field's tag (Win32 layout); zero is
//     padding (Win64 layout, real offset four bytes further on).
//
// Field 0 starts at offset 0; subsequent fields start at the
// next-offset emitted by the prior field. The last field has the
// $02 last-flag and is followed by either nothing (managed
// record) or a small trailer (unmanaged); the walker stops as
// soon as the last-flag fires.
const
  MaxFields = 32;
var
  Cands       : array[0..MaxFields - 1] of TRsmClassMember;
  Offsets     : array[0..MaxFields] of UInt32;
  Count       : Integer;
  NL          : Integer;
  PStart      : NativeInt;
  PScan       : NativeInt;
  P           : NativeInt;
  NameLen     : Byte;
  Name        : String;
  LastFlag    : Byte;
  NextOff     : UInt32;
  IsLast      : Boolean;
  I           : Integer;
  Info        : TRsmClassInfo;
  List        : IList<TRsmClassMember>;
  Member      : TRsmClassMember;
  FoundFirst  : Boolean;
begin
  NL := Length(ARecordName);
  if ARecordNameOff + 1 + NL + 4 > FSz then Exit;
  PStart := ARecordNameOff + 1 + NL + 4;

  // The header between the size DWORD and the first field tag has
  // a variable layout (count, flags, padding) we don't fully
  // understand, so locate the first field tag heuristically: scan
  // forward up to a bounded window for the first $02 byte that
  // looks like a complete field record. "Looks like" is now a
  // STRUCTURAL anchor rather than a name-character heuristic:
  // the 6 bytes after the field name must form the documented
  // field-typeinfo prefix
  //     $02 $00 <last-flag> $00 $00 $00          (last-flag in {$00, $02})
  // which is far more selective than any first-character rule
  // could be, and DOES NOT depend on Delphi house style (so
  // records whose fields don't start with 'F' -- TFW's
  // TAppCaps.DbKindName, TMdt.Id, ... -- get parsed correctly).
  // 4 KB scan window. TAppCaps in TFW puts its first $02-field
  // record ~500 bytes past the name (a variant-record case list
  // / nested sub-record headers fill that gap), so the older
  // 64-byte window dropped large interface-scope records
  // entirely. 4 KB is well above anything we've seen in practice
  // and the strict typeinfo-prefix anchor (above) keeps the
  // false-positive rate at zero across the gap.
  FoundFirst := False;
  P := PStart;
  PScan := PStart + 4096;
  if PScan > FSz - 8 then PScan := FSz - 8;
  while P < PScan do
  begin
    if (ByteAt(P) = $02) then
    begin
      NameLen := ByteAt(P + 1);
      if (NameLen >= 1) and (NameLen <= 40) and
         ReadIdentifier(P + 1, Name) and
         IsValidFieldTypeinfoPrefix(P + 2 + Length(Name)) then
      begin
        FoundFirst := True;
        Break;
      end;
    end;
    Inc(P);
  end;
  if not FoundFirst then Exit;

  // Walk the field list. Each iteration advances P past the
  // current field's complete encoding (tag + name + 6-byte
  // typeinfo + optional 4-byte next-offset). The same structural
  // typeinfo-prefix anchor used in the first-field search above
  // gates every subsequent field too, so we never have to
  // depend on the field name's first character.
  Count := 0;
  Offsets[0] := 0;
  while (P + 8 < FSz) and (Count < MaxFields) do
  begin
    if ByteAt(P) <> $02 then Break;
    NameLen := ByteAt(P + 1);
    if (NameLen < 1) or (NameLen > 40) then Break;
    if not ReadIdentifier(P + 1, Name) then Break;
    if not IsValidFieldTypeinfoPrefix(P + 2 + Length(Name)) then Break;

    // 6-byte typeinfo follows the name; byte 2 is the last-flag.
    if P + 2 + Length(Name) + 6 > FSz then Break;
    LastFlag := ByteAt(P + 2 + Length(Name) + 2);
    IsLast := (LastFlag = $02);

    Cands[Count].Name    := Name;
    Cands[Count].Offset  := Offsets[Count];
    Cands[Count].TypeIdx := 0;
    Inc(Count);

    if IsLast then Break;

    // Pick the next-offset DWORD location based on the layout
    // hint: Win32 has it directly after the typeinfo, Win64 puts
    // 4 zero pad bytes first. byte_at(typeinfo_end + 4) = $02
    // means the next field tag follows immediately, so we're
    // looking at the Win32 layout; anything else means Win64.
    var TypeinfoEnd: NativeInt := P + 2 + Length(Name) + 6;
    if TypeinfoEnd + 8 > FSz then Break;
    var Tag4: Byte := ByteAt(TypeinfoEnd + 4);
    if Tag4 = $02 then
    begin
      NextOff := DwordAt(TypeinfoEnd);
      Offsets[Count] := NextOff;
      P := TypeinfoEnd + 4;
    end
    else
    begin
      if TypeinfoEnd + 12 > FSz then Break;
      NextOff := DwordAt(TypeinfoEnd + 4);
      Offsets[Count] := NextOff;
      P := TypeinfoEnd + 12;
    end;
  end;

  if Count = 0 then Exit;

  List := Collections.NewPlainList<TRsmClassMember>;
  for I := 0 to Count - 1 do
  begin
    Member := Default(TRsmClassMember);
    Member.Name    := Cands[I].Name;
    Member.Offset  := Cands[I].Offset;
    Member.TypeIdx := 0;
    // Member size = gap to the next member's offset. Records can
    // contain variant cases (case Byte of ...) whose siblings share
    // an overlay offset, so the next member's offset isn't strictly
    // greater than the current one's. Treat that as "unknown" (0):
    // a wrong derived size on a variant overlay would be worse than
    // letting EvaluateVariable fall back to the user-requested type
    // width.
    if (I + 1 < Count) and (Cands[I + 1].Offset > Cands[I].Offset) then
      Member.Size := Cands[I + 1].Offset - Cands[I].Offset
    else
      Member.Size := 0;
    List.Add(Member);
  end;

  Info := Default(TRsmClassInfo);
  Info.Name    := ARecordName;
  Info.TypeIdx := UInt32(ARecordNameOff);
  Info.Kind    := skRecord;
  Info.Members := List;
  FClassByName[LowerCase(ARecordName)] := FClasses.Count;
  FClasses.Add(Info);
end;

procedure TRsmScanner.DiscoverAndParseAllStructs;
// Walk the entire RSM byte-by-byte and parse every class or record
// whose presence is signalled by one of two distinctive markers:
//
//   * Class trailer pattern (Win32):
//       <NL> <name> 04 00 00 00 07 <NL> <name> 58 00 00 00
//   * Class trailer pattern (Win64):
//       <NL> <name> 08 00 00 00 00 00 00 00 07 <NL> <name>
//                                            C8 00 00 00 00 00 00 00
//     The constant DWORDs are upgraded to QWORDs on x64. The
//     duplicated length-prefixed name with a $07 tag between is
//     the stable part across both platforms.
//
//   * Record sentinel: $0E <NL> <name>
//     The $0E byte precedes every record name in the type stream;
//     classes don't have a sentinel.
//
// Each detected struct is parsed by the existing field walker
// (backward window for classes, forward walk with explicit
// next-offset DWORDs for records). Duplicate names that happen
// to match more than once (e.g. the same class name copied into
// multiple sub-streams) are filtered by checking the existing
// class index before re-adding.
var
  P    : NativeInt;
  L    : Byte;
  Name : String;

  // Compare two length-prefixed names without materializing
  // either into a String. ASecondStart points at the second
  // name's length byte; AFirstNameStart points at the first
  // name's character bytes (i.e. the byte AFTER its length
  // prefix). Used in the hot path so we never allocate the
  // first-name String before the class/record trailer matches.
  function SecondNameMatchesBytes(ASecondStart, AFirstNameStart: NativeInt;
    ANL: Byte): Boolean;
  var
    K: Integer;
  begin
    Result := False;
    if ByteAt(ASecondStart) <> ANL then Exit;
    for K := 0 to Integer(ANL) - 1 do
      if ByteAt(ASecondStart + 1 + K) <> ByteAt(AFirstNameStart + K) then Exit;
    Result := True;
  end;

  // Cheap "could this be a T-prefixed identifier" check without
  // allocation. Validates length range, that the first character
  // is 'T' (which every class / record name we care about starts
  // with), and that the remaining bytes are valid identifier
  // characters. This shaves 99%+ of byte positions in the file
  // off the hot path before we ever build a String.
  function IsTPrefixedIdent(AOff: NativeInt; AL: Byte): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if (AL < 2) or (AOff + 1 + AL > FSz) then Exit;
    if ByteAt(AOff + 1) <> Byte(Ord('T')) then Exit;
    for I := 1 to AL - 1 do
      if not IsPrintableAscii(ByteAt(AOff + 1 + I)) then Exit;
    Result := True;
  end;

  // Locate the class trailer marker `04 00 00 00 07 <L> <name>`
  // (Win32) or `08 00 00 00 00 00 00 00 07 <L> <name>` (Win64)
  // anywhere in the next AWindow bytes after the first occurrence
  // of the class name. The tight checks above (trailer immediately
  // at NameEnd+4 / NameEnd+8) cover classes with no methods; this
  // forward-scan fallback covers classes that declare methods,
  // where the RSM emits each method record between the first
  // class-name and the trailer pattern, pushing the trailer past
  // the tight-check offsets. The exact 4/8-byte zero prefix, the
  // $07 type tag, and an exactly-matching duplicate name keep the
  // false-positive rate negligible.
  function FindClassTrailerWithin(ANameEnd: NativeInt; AWindow: NativeInt;
    ANameStart: NativeInt; ANL: Byte): Boolean;
  var
    P, Stop: NativeInt;
  begin
    Result := False;
    Stop := ANameEnd + AWindow;
    if Stop > FSz - 5 - Integer(ANL) then
      Stop := FSz - 5 - Integer(ANL);
    P := ANameEnd;
    while P < Stop do
    begin
      // Win32 trailer: 04 00 00 00 07 <L> <name>
      if (ByteAt(P) = $04) and (ByteAt(P + 1) = $00) and
         (ByteAt(P + 2) = $00) and (ByteAt(P + 3) = $00) and
         (ByteAt(P + 4) = $07) and
         SecondNameMatchesBytes(P + 5, ANameStart, ANL) then
        Exit(True);
      // Win64 trailer: 08 00 00 00 00 00 00 00 07 <L> <name>
      if (P + 9 + Integer(ANL) < FSz) and
         (ByteAt(P) = $08) and (ByteAt(P + 1) = $00) and
         (ByteAt(P + 2) = $00) and (ByteAt(P + 3) = $00) and
         (ByteAt(P + 4) = $00) and (ByteAt(P + 5) = $00) and
         (ByteAt(P + 6) = $00) and (ByteAt(P + 7) = $00) and
         (ByteAt(P + 8) = $07) and
         SecondNameMatchesBytes(P + 9, ANameStart, ANL) then
        Exit(True);
      Inc(P);
    end;
  end;

  function FindClassByName(const AName: String): Integer;
  begin
    if not FClassByName.TryGetValue(LowerCase(AName), Result) then
      Result := -1;
  end;

begin
  // Earlier the hot loop allocated a TBytes + String for every
  // byte position where a length-prefixed run of printable chars
  // existed -- tens of millions of allocations on TFW after the
  // identifier charset was widened to accept '.', '$' and friends.
  // We now do a byte-only quick-reject for "starts with 'T'" plus
  // the class/record trailer pattern up front, and only allocate
  // the actual name String once we know the entry is interesting
  // enough to be passed into ScanFields*. This cuts the phase from
  // hundreds of seconds to a few seconds on TFW-sized binaries.
  P := 1;
  while P + 24 < FSz do
  begin
    L := ByteAt(P);
    if (L >= 2) and (L <= 40) and IsTPrefixedIdent(P, L) then
    begin
      var NameStart: NativeInt := P + 1;
      var NameEnd  : NativeInt := P + 1 + L;
      var IsClass  : Boolean := False;
      // Win32: $07 at NameEnd + 4, duplicate name at +5.
      if (NameEnd + 5 + L < FSz) and (ByteAt(NameEnd + 4) = $07) and
         SecondNameMatchesBytes(NameEnd + 5, NameStart, L) then
        IsClass := True
      // Win64: $07 at NameEnd + 8, duplicate name at +9.
      else if (NameEnd + 9 + L < FSz) and (ByteAt(NameEnd + 8) = $07) and
              SecondNameMatchesBytes(NameEnd + 9, NameStart, L) then
        IsClass := True
      // Classes that declare methods: the RSM emits a method record
      // (procedure name + signature, etc.) for each method between
      // the first class-name and the trailer marker, so the trailer
      // no longer sits at NameEnd+4 / NameEnd+8.
      //
      // Filter trigger before scanning: a class-def with methods
      // always starts with a small DWORD at NameEnd whose three
      // high bytes are zero (the method-records header size). The
      // *type-record* region's same name has non-zero garbage at
      // NameEnd+1..4, so this filter cleanly separates the two
      // and prevents the field-scanner from binding to the wrong
      // anchor position. Without this filter the forward-scan
      // matches a later region's trailer for the same class name
      // and FindClassByName then locks out the correct match.
      else if (NameEnd + 4 < FSz) and
              (ByteAt(NameEnd + 1) = 0) and
              (ByteAt(NameEnd + 2) = 0) and
              (ByteAt(NameEnd + 3) = 0) and
              (ByteAt(NameEnd + 4) = 0) and
              // 8 KB window covers method-heavy RTL classes such as
              // TStrings (~5.6 KB between first-name and trailer)
              // and TForm-tier classes with dozens of published
              // properties and event hooks. The trailer pattern
              // (4-byte zero prefix, $07 type tag, exact-length
              // duplicate name) is selective enough that wider
              // windows don't increase the false-positive rate.
              FindClassTrailerWithin(NameEnd, 8192, NameStart, L) then
        IsClass := True;

      if IsClass then
      begin
        if ReadIdentifier(P, Name) and (FindClassByName(Name) < 0) then
        begin
          // Cap the backward field-scan at the previous class's
          // anchor position so a tightly-packed sibling (e.g.
          // TDerived immediately followed by TDeepDerived) cannot
          // leak its own fields into this class's member list.
          // FClasses is filled in source-declaration order, so the
          // last-added entry is the closest preceding class in the
          // byte stream.
          var PrevAnchor: NativeInt := 0;
          if FClasses.Count > 0 then
            PrevAnchor := NativeInt(FClasses[FClasses.Count - 1].TypeIdx);
          ScanFieldsBackwardFromClassName(P, skClass, Name, PrevAnchor);
          // Capture the two bytes immediately before the length
          // byte as a candidate parent-type-id. Cross-unit
          // inheritance (e.g. user class -> System.Classes
          // TComponent) puts a non-zero RSM 16-bit type-id here;
          // same-unit inheritance leaves it zero. We don't
          // resolve to a name yet -- the type registry isn't built
          // until LinkMemberTypeIdsFromFormatA runs later --
          // we just record the raw id for the post-pass.
          if (P >= 2) and (FClasses.Count > 0) then
          begin
            var ClsIdx: Integer := FClasses.Count - 1;
            var CInfo : TRsmClassInfo := FClasses[ClsIdx];
            CInfo.ParentRawId := UInt32(ByteAt(P - 2)) or
                                 (UInt32(ByteAt(P - 1)) shl 8);
            FClasses[ClsIdx] := CInfo;
          end;
        end;
      end
      else if (P > 0) and (ByteAt(P - 1) = TRsmTag.RECORD_SENTINEL) then
      begin
        // Record probe: $0E sentinel right before the name.
        if ReadIdentifier(P, Name) and (FindClassByName(Name) < 0) then
          ScanFieldsForwardFromRecordName(P, Name);
      end;
    end;
    Inc(P);
  end;
end;

procedure TRsmScanner.ScanSymbolStream;
// Walk the RSM symbol stream looking for procedure ($28 tag) and
// local ($20 tag) records. Each proc owns the locals between its
// own record and the next proc (or the $63 scope-end byte). Both
// Win32 and Win64 proc-address forms are decoded inline below.
//
// Tag-byte definitions live in <see cref="TRsmTag"/> (in
// DPT.Rsm.Model); locally aliased here for readability inside the
// scan loop.
//
// === Win32 proc-address encoding ===
// After the name and the fixed "20 00 00" prefix, a 4-byte little-
// endian DWORD packs the proc's runtime VA into the high 28 bits
// and a 4-bit type tag (always $7 for code) into the low 4 bits.
// Recovering the segment-relative RVA is therefore
//     RVA = (DWORD >> 4) - ($400000 + $1000)
// where $400000 is the Win32 image base and $1000 is the .text
// section's typical RVA.
//
// === Win64 proc-address encoding ===
// The address block is variable-length: 4 bytes when the proc
// is small (size <= $80), 5 bytes for larger procs, followed by
// the fixed terminator "04 10 21 2e 00". Only bytes 0..2 are
// needed to recover the RVA; bytes 3/4 encode proc size + local-
// layout info and aren't needed for address resolution.
//
// Decoded layout (LSB-first; bits are of VA = RVA + $1000, i.e.
// RVA measured from the image base instead of from the .text
// section start):
//    byte 0 bits 0-6 : constant $03 (encoding-kind tag for code)
//    byte 0 bit  7   : VA bit 4
//    byte 1          : VA bits 5-12  (full 8 bits)
//    byte 2          : VA bits 13-20 (full 8 bits)
// Procs are 16-byte aligned, so VA bits 0-3 are implicitly zero.
// This covers code sections up to 2MB; for larger binaries the
// higher VA bits would have to come from bytes 3/4 (not yet RE'd).
const
  PROC_TAG          = TRsmTag.PROC_TAG;
  LOCAL_TAG         = TRsmTag.LOCAL_TAG;
  PARAM_TAG         = TRsmTag.PARAM_TAG;
  REGVAR_TAG        = TRsmTag.REGVAR_TAG;
  GLOBAL_PRIM_TAG   = TRsmTag.GLOBAL_PRIM_TAG;
  ENUM_CONST_TAG    = TRsmTag.ENUM_CONST_TAG;
  TYPE_REGISTRY_TAG = TRsmTag.TYPE_REGISTRY_TAG;
  SCOPE_END         = TRsmTag.SCOPE_END;
  AddrScanWindow    = 32;
var
  P        : NativeInt;
  Tag      : Byte;
  NameLen  : Byte;
  Name     : String;
  Proc     : TRsmProc;
  Loc      : TRsmLocal;
  InProc   : Boolean;
  LocalIdx : Integer;
  RegParam : Integer;
  /// Guards SCOPE_END from closing the proc scope while we are
  /// still inside the proc's address-payload bytes. The payload
  /// for the $A0 sub-form runs ~18 bytes of essentially arbitrary
  /// data (timestamp/type-ref/encoded VA/trailer), and on large
  /// binaries it routinely contains a $63 byte. Without this
  /// guard, that incidental $63 fires SCOPE_END BEFORE the first
  /// real param/local record has been read -- so on TFW every
  /// class-method's Self / params silently vanished. The flag
  /// flips to True only once we've actually parsed a local-shaped
  /// record ($20 / $21 / $22) since the last PROC_TAG, which
  /// puts SCOPE_END detection back on solid ground.
  SeenLocalSinceProc: Boolean;

  function FindProcByName(const AName: String): Integer;
  begin
    if not FProcByName.TryGetValue(LowerCase(AName), Result) then
      Result := -1;
  end;

  function DecodeAddrPayload(AStartOff: NativeInt): NativeUInt;
  // The proc record's post-name payload starts with a sub-tag byte
  // that picks between two layouts:
  //
  //   $20 -- simple inline form. Address bytes follow the constant
  //          "20 00 00" 3-byte prefix at offset 3, in the Win32
  //          DWORD form ($xy yz zz 04 with low nibble $7) or the
  //          Win64 3-byte form ($xy yy yy, anchored by the
  //          "04 10 21 2e 00" marker just after it).
  //
  //   $A0 -- extended form used by large binaries. A type-ref /
  //          timestamp lives between the prefix and the Win32
  //          address DWORD, putting the address bytes at offset 7.
  //
  //   anything else -- forward declaration or pure cross-reference
  //                    record with no usable address. Real address
  //                    arrives in a later occurrence of the same
  //                    proc, which the caller patches in.

    function TryWin32(AOff: NativeInt): NativeUInt;
    // Win32 PROC-address payload: 4 bytes encoding the full
    // 28-bit virtual address as ((VA shl 4) or $07). The low
    // nibble of byte 0 is the format marker ($07); the remaining
    // 28 bits are the VA. There is NO constant tag byte at byte
    // 3 -- earlier code required byte 3 = $04 because the test
    // corpus only contained binaries with VAs in [$401000,
    // $4FFFFF] (the high byte of (VA shl 4) is $04 in that
    // window). TFW puts most of its code well beyond that range
    // (e.g. 0x598BBD4), so byte 3 simply carries the VA's high
    // bits and varies build to build. The decoded-range guard
    // (0 < Dec < 256 MB, matching the 28-bit encoding capacity)
    // remains the integrity check.
    var DW: UInt32; Dec: Int64;
    begin
      Result := 0;
      if AOff + 3 >= FSz then Exit;
      if (ByteAt(AOff) and $0F) <> $07 then Exit;
      DW := DwordAt(AOff);
      Dec := (Int64(DW) shr 4) - $401000;
      if (Dec > 0) and (Dec < $10000000) then
        Result := NativeUInt(Dec);
    end;

    function TryWin64(AOff: NativeInt): NativeUInt;
    var MOff: NativeInt; B0, B1, B2: Byte; Va: UInt32;
    begin
      Result := 0;
      if AOff + 4 >= FSz then Exit;
      if (ByteAt(AOff) and $7F) <> $03 then Exit;
      // Marker layout "04 10 ?? 2E 00": bytes 0/1/3/4 are
      // constant across the corpus; the byte at position 2 is a
      // per-binary counter that the linker varies build-to-build
      // (it isn't tied to the proc, but to the overall RSM
      // module layout), so we accept any value there.
      for MOff := AOff + 4 to AOff + 5 do
        if (MOff + 4 < FSz) and
           (ByteAt(MOff)     = $04) and (ByteAt(MOff + 1) = $10) and
           (ByteAt(MOff + 3) = $2E) and (ByteAt(MOff + 4) = $00) then
        begin
          B0 := ByteAt(AOff);
          B1 := ByteAt(AOff + 1);
          B2 := ByteAt(AOff + 2);
          Va := ((UInt32(B0) shr 7) and 1) shl 4 or
                (UInt32(B1) shl 5) or
                (UInt32(B2) shl 13);
          Va := Va and $1FFFFF;
          if Va >= $1000 then
            Exit(NativeUInt(Va - $1000));
        end;
    end;

  var
    Tag: Byte;
  begin
    // Sub-tag dispatch:
    //   $20 -- simple inline form, address at +3.
    //   $A0 -- extended form with type-ref/timestamp metadata,
    //          address DWORD at +7.
    //   $41 -- another extended variant seen in large binaries
    //          (e.g. method records), address DWORD at +4 after
    //          a "41 02 10 00" header.
    //   $80, $00, ... -- forward-declaration / cross-reference
    //          records with no embedded address. The caller's
    //          dedup-and-patch step picks up the address when a
    //          later definition record arrives.
    Result := 0;
    if AStartOff + 3 >= FSz then Exit;
    Tag := ByteAt(AStartOff);
    case Tag of
      $20:
        begin
          Result := TryWin32(AStartOff + 3);
          if Result = 0 then
            Result := TryWin64(AStartOff + 3);
        end;
      $A0:
        Result := TryWin32(AStartOff + 7);
      $41:
        Result := TryWin32(AStartOff + 4);
    end;
  end;

begin
  InProc := False;
  SeenLocalSinceProc := False;
  LocalIdx := 0;
  RegParam := 0;
  P := 0;
  // Tracks the most recently scanned $25 cross-unit enum-constant
  // record. The Delphi RSM emits all of an enum's $25 records
  // back-to-back, then the $2A registry entry that names it. By
  // remembering the last $25 secondary id and updating it on every
  // new $25, we can bridge the $2A primary to the secondary the
  // $25 records actually carry their constants under -- without
  // depending on a single fixed body-offset slot, which differs
  // across compile contexts (TWindowState has multiple $2A entries
  // with different secondaries, only one of which holds the real
  // constants).
  var LastEnumSecondary: UInt32 := 0;
  var LastEnumSecondaryPos: NativeInt := -1;
  while P + 2 < FSz do
  begin
    Tag := ByteAt(P);
    // Proc record: $28 NameLen Name <variable-length payload>.
    if Tag = PROC_TAG then
    begin
      NameLen := ByteAt(P + 1);
      if (NameLen >= 1) and (NameLen <= 64) and
         ReadIdentifier(P + 1, Name) then
      begin
        // Real proc declarations are preceded by a distinctive
        // run-up (e.g. $FF $28 for the extended form, or just $28
        // for the simple inline form). Detect the address payload
        // by scanning the post-name window for a recognizable
        // encoding pattern rather than assuming a fixed offset
        // because large binaries emit a richer prefix (timestamp /
        // type-ref / etc.) between name and address.
        var Decoded: NativeUInt := 0;
        if P + 2 + Length(Name) + 4 < FSz then
          Decoded := DecodeAddrPayload(P + 2 + Length(Name));
        // First time we see this name: add a fresh entry.
        // Already there: a later occurrence often carries the real
        // address while the first was a forward declaration, so
        // patch the existing entry when the new decode yields a
        // non-zero result.
        var ExistingIdx: Integer := FindProcByName(Name);
        if ExistingIdx < 0 then
        begin
          Proc := Default(TRsmProc);
          Proc.Name := Name;
          Proc.SegmentOffset := Decoded;
          if Decoded > 0 then
            Proc.Size := $1000
          else
            Proc.Size := 0;
          Proc.Locals := Collections.NewPlainList<TRsmLocal>;
          FProcByName[LowerCase(Name)] := FProcs.Count;
          // Linker-emitted "@Name" aliases should be findable by
          // both the prefixed and the bare form, so register the
          // alias too when the stored name starts with "@".
          if (Length(Name) > 0) and (Name[1] = '@') then
            FProcByName[LowerCase(Copy(Name, 2, MaxInt))] := FProcs.Count;
          FProcs.Add(Proc);
          InProc := True;
          SeenLocalSinceProc := False;
          LocalIdx := 0;
          RegParam := 0;
        end
        else if (FProcs[ExistingIdx].SegmentOffset = 0) and (Decoded > 0) then
        begin
          Proc := FProcs[ExistingIdx];
          Proc.SegmentOffset := Decoded;
          Proc.Size := $1000;
          FProcs[ExistingIdx] := Proc;
        end;
        P := P + 2 + Length(Name);
        Continue;
      end;
    end
    // Parameter record: $22 NameLen Name $62 $00 $00 <type-id 2 bytes>.
    // Unlike LOCAL_TAG, this record does NOT carry a frame offset:
    // open-array parameters (and other parameters in the calling
    // convention's register slots) live in CPU registers, not on
    // the stack. We capture the parameter so callers see it in the
    // Locals list, tagged as register-passed with its zero-based
    // register-param index; the higher layer resolves the index to
    // the concrete register (RCX/RDX/... on Win64, EAX/EDX/ECX on
    // Win32 default register convention).
    else if (Tag = PARAM_TAG) and InProc and (FProcs.Count > 0) then
    begin
      NameLen := ByteAt(P + 1);
      if (NameLen >= 1) and (NameLen <= 64) and
         ReadIdentifier(P + 1, Name) then
      begin
        Loc := Default(TRsmLocal);
        Loc.Name        := Name;
        Loc.BpOffset    := 0;
        Loc.TypeIdx     := 0;
        Loc.Kind        := lkRegister;
        Loc.RegParamIdx := Byte(RegParam);
        Inc(RegParam);
        var PayloadStart: NativeInt := P + 2 + Length(Name);
        if (PayloadStart + 4 < FSz) and
           (ByteAt(PayloadStart)     = $62) and
           (ByteAt(PayloadStart + 1) = $00) and
           (ByteAt(PayloadStart + 2) = $00) then
        begin
          // Type-id is the LE 16-bit value "<lo> <hi>" when hi is
          // a structured-type marker ($2E for most types, $2F for
          // some Win64 sets); otherwise the type-id is the single
          // byte at +3 (built-in primitive types).
          var Hi: Byte := ByteAt(PayloadStart + 4);
          if (Hi = $2E) or (Hi = $2F) then
            Loc.TypeIdx := UInt32(ByteAt(PayloadStart + 3)) or
                            (UInt32(Hi) shl 8)
          else
            Loc.TypeIdx := ByteAt(PayloadStart + 3);
        end;
        FProcs[FProcs.Count - 1].Locals.Add(Loc);
        Inc(LocalIdx);
        SeenLocalSinceProc := True;
        // Advance past the parameter's payload (3-byte prefix
        // "$62 $00 $00" + 2-byte type-id = 5 bytes), then look for
        // the hidden high-index sub-record "$20 $21 ..." that
        // follows an open-array parameter. Open-array params occupy
        // TWO register slots (pointer + high-index), so when the
        // compiler emitted that hidden record we consume an extra
        // RegParam slot to keep subsequent params' indices correct.
        P := P + 2 + Length(Name);
        if (P + 4 < FSz) and (ByteAt(P) = $62) and
           (ByteAt(P + 1) = $00) and (ByteAt(P + 2) = $00) then
          P := P + 5;
        if (P + 1 < FSz) and (ByteAt(P) = $20) and (ByteAt(P + 1) = $21) then
          Inc(RegParam);
        Continue;
      end;
    end
    // Register-passed variable: $21 NameLen Name $66 $00 $00 <typeidLo> <typeidHi>.
    else if (Tag = REGVAR_TAG) and InProc and (FProcs.Count > 0) then
    begin
      NameLen := ByteAt(P + 1);
      if (NameLen >= 1) and (NameLen <= 64) and
         ReadIdentifier(P + 1, Name) then
      begin
        Loc := Default(TRsmLocal);
        Loc.Name        := Name;
        Loc.BpOffset    := 0;
        Loc.TypeIdx     := 0;
        Loc.Kind        := lkRegister;
        Loc.RegParamIdx := Byte(RegParam);
        Inc(RegParam);
        var PayloadStart: NativeInt := P + 2 + Length(Name);
        if (PayloadStart + 4 < FSz) and
           (ByteAt(PayloadStart)     = $66) and
           (ByteAt(PayloadStart + 1) = $00) and
           (ByteAt(PayloadStart + 2) = $00) then
        begin
          var Hi: Byte := ByteAt(PayloadStart + 4);
          if (Hi = $2E) or (Hi = $2F) then
            Loc.TypeIdx := UInt32(ByteAt(PayloadStart + 3)) or
                            (UInt32(Hi) shl 8)
          else
            Loc.TypeIdx := ByteAt(PayloadStart + 3);
        end;
        FProcs[FProcs.Count - 1].Locals.Add(Loc);
        Inc(LocalIdx);
        SeenLocalSinceProc := True;
        P := P + 2 + Length(Name);
        if (P + 4 < FSz) and (ByteAt(P) = $66) and
           (ByteAt(P + 1) = $00) and (ByteAt(P + 2) = $00) then
          P := P + 5;
        Continue;
      end;
    end
    else if (Tag = LOCAL_TAG) and InProc and (FProcs.Count > 0) then
    begin
      NameLen := ByteAt(P + 1);
      if (NameLen >= 1) and (NameLen <= 64) and
         ReadIdentifier(P + 1, Name) then
      begin
        Loc := Default(TRsmLocal);
        Loc.Name     := Name;
        Loc.BpOffset := -10000 - (LocalIdx * 4);
        Loc.TypeIdx  := 0;
        Loc.Kind     := lkBpRel;
        var PayloadStart: NativeInt := P + 2 + Length(Name);
        if PayloadStart + 5 < FSz then
        begin
          var Byte4: Byte := ByteAt(PayloadStart + 4);
          if (Byte4 = $2E) or (Byte4 = $2F) then
          begin
            var Byte5: Byte := ByteAt(PayloadStart + 5);
            if (Byte5 and 1) = 1 then
            begin
              if PayloadStart + 6 < FSz then
              begin
                var Hi : Byte := ByteAt(PayloadStart + 6);
                var W  : Word := Word(Byte5) or (Word(Hi) shl 8);
                var SW : SmallInt := SmallInt(W);
                Loc.BpOffset := (Int32(SW) - 1) div 4;
              end;
            end
            else
            begin
              var Ofs8: ShortInt := ShortInt(Byte5);
              Loc.BpOffset := Int32(Ofs8) div 2;
            end;
            Loc.TypeIdx := UInt32(ByteAt(PayloadStart + 3)) or
                           (UInt32(Byte4) shl 8);
          end
          else
          begin
            var Next5: Byte := ByteAt(PayloadStart + 5);
            if (Next5 = LOCAL_TAG) or (Next5 = PROC_TAG) or (Next5 = SCOPE_END) then
            begin
              var Ofs8: ShortInt := ShortInt(Byte4);
              Loc.BpOffset := Int32(Ofs8) div 2;
              Loc.TypeIdx  := ByteAt(PayloadStart + 3);
            end
            else if PayloadStart + 6 < FSz then
            begin
              var Next6: Byte := ByteAt(PayloadStart + 6);
              if (Next6 = LOCAL_TAG) or (Next6 = PROC_TAG) or (Next6 = SCOPE_END) then
              begin
                var Hi : Byte := ByteAt(PayloadStart + 5);
                var W  : Word := Word(Byte4) or (Word(Hi) shl 8);
                var SW : SmallInt := SmallInt(W);
                Loc.BpOffset := (Int32(SW) - 1) div 4;
                Loc.TypeIdx  := ByteAt(PayloadStart + 3);
              end;
            end;
          end;
        end;
        FProcs[FProcs.Count - 1].Locals.Add(Loc);
        Inc(LocalIdx);
        SeenLocalSinceProc := True;
        // Also publish the (name, RSM-type-id) pair into the
        // global type-index. The InProc gate above can't reliably
        // tell a stack local apart from a module-level variable
        // emitted in the same byte run (procs and globals share
        // the $20 tag and SCOPE_END doesn't always fire between
        // them), so we record the type info for both.
        if P + 2 + Integer(NameLen) + 4 < FSz then
        begin
          var GLo: Byte := ByteAt(P + 2 + NameLen + 3);
          var GHi: Byte := ByteAt(P + 2 + NameLen + 4);
          FGlobalByName[LowerCase(Name)] :=
            UInt32(GLo) or (UInt32(GHi) shl 8);
          FGlobalFileOffset[LowerCase(Name)] := P;
        end;
        P := P + 2 + Length(Name);
        Continue;
      end;
    end
    // Module-level global variable (outside any proc scope).
    else if (Tag = LOCAL_TAG) and (not InProc) then
    begin
      NameLen := ByteAt(P + 1);
      if (NameLen >= 1) and (NameLen <= 40) and
         (P + 2 + Integer(NameLen) + 5 < FSz) and
         (ByteAt(P + 2 + NameLen + 1) = $00) and
         (ByteAt(P + 2 + NameLen + 2) = $00) and
         ReadIdentifier(P + 1, Name) then
      begin
        var Lo: Byte := ByteAt(P + 2 + NameLen + 3);
        var Hi: Byte := ByteAt(P + 2 + NameLen + 4);
        FGlobalByName[LowerCase(Name)] := UInt32(Lo) or (UInt32(Hi) shl 8);
        FGlobalFileOffset[LowerCase(Name)] := P;
        P := P + 2 + Length(Name) + 5;
        Continue;
      end;
    end
    // Top-level primitive global: $27 NL Name $66 $00 $00 <id> <4-byte VA>.
    else if (Tag = GLOBAL_PRIM_TAG) and (not InProc) then
    begin
      NameLen := ByteAt(P + 1);
      if (NameLen >= 1) and (NameLen <= 40) and
         (P + 2 + Integer(NameLen) + 4 < FSz) and
         (ByteAt(P + 2 + NameLen)     = $66) and
         (ByteAt(P + 2 + NameLen + 1) = $00) and
         (ByteAt(P + 2 + NameLen + 2) = $00) and
         ReadIdentifier(P + 1, Name) then
      begin
        // Two-byte vs single-byte type id. Enum / set / structured-
        // typed globals carry a 2-byte id with $2E or $2F in the hi
        // slot (e.g. $2E75 for TLightStatus); plain primitives have
        // a single byte ($02 = Integer, $04 = string, ...).
        var PrimId: UInt32;
        var HiByte: Byte := 0;
        if P + 2 + Integer(NameLen) + 4 < FSz then
          HiByte := ByteAt(P + 2 + NameLen + 4);
        if (HiByte = $2E) or (HiByte = $2F) then
          PrimId := UInt32(ByteAt(P + 2 + NameLen + 3)) or
                    (UInt32(HiByte) shl 8)
        else
          PrimId := ByteAt(P + 2 + NameLen + 3);
        FGlobalByName[LowerCase(Name)] := PrimId;
        FGlobalFileOffset[LowerCase(Name)] := P;
        P := P + 2 + Length(Name) + 4;
        Continue;
      end;
    end
    // Enum-constant record (works inside or outside a proc scope).
    else if Tag = ENUM_CONST_TAG then
    begin
      NameLen := ByteAt(P + 1);
      // Program-local form (8-byte body).
      if (NameLen >= 1) and (NameLen <= 64) and
         (P + 2 + Integer(NameLen) + 8 < FSz) and
         (ByteAt(P + 2 + NameLen)     = $0A) and
         (ByteAt(P + 2 + NameLen + 1) = $00) and
         (ByteAt(P + 2 + NameLen + 2) = $00) and
         (ByteAt(P + 2 + NameLen + 4) = $2E) and
         (ByteAt(P + 2 + NameLen + 5) = $00) and
         (ByteAt(P + 2 + NameLen + 6) = $00) and
         ReadIdentifier(P + 1, Name) then
      begin
        var EnumTypeId: UInt32 :=
          UInt32(ByteAt(P + 2 + NameLen + 3)) or (UInt32($2E) shl 8);
        // The ordinal is stored doubled (the LSB is reserved as a
        // form discriminator the same way BPRel offsets reserve it).
        var Ordinal: Integer := Integer(ByteAt(P + 2 + NameLen + 7)) shr 1;
        FEnumTypeIds[EnumTypeId] := True;
        FEnumConstNames[IntToStr(EnumTypeId) + ':' + IntToStr(Ordinal)] := Name;
        P := P + 2 + Length(Name) + 8;
        Continue;
      end;
      // Cross-unit form (12-byte body).
      if (NameLen >= 1) and (NameLen <= 64) and
         (P + 2 + Integer(NameLen) + 12 < FSz) and
         (ByteAt(P + 2 + NameLen)     = $8A) and
         (ByteAt(P + 2 + NameLen + 1) = $00) and
         (ByteAt(P + 2 + NameLen + 2) = $00) and
         (ByteAt(P + 2 + NameLen + 9) = $00) and
         (ByteAt(P + 2 + NameLen + 10) = $00) and
         ReadIdentifier(P + 1, Name) then
      begin
        var EnumTypeId: UInt32 :=
          UInt32(ByteAt(P + 2 + NameLen + 7)) or
          (UInt32(ByteAt(P + 2 + NameLen + 8)) shl 8);
        var Ordinal: Integer :=
          Integer(ByteAt(P + 2 + NameLen + 11)) shr 1;
        if EnumTypeId <> 0 then
        begin
          FEnumTypeIds[EnumTypeId] := True;
          FCrossUnitEnumIds[EnumTypeId] := True;
          FEnumConstNames[IntToStr(EnumTypeId) + ':' + IntToStr(Ordinal)] := Name;
          LastEnumSecondary := EnumTypeId;
          LastEnumSecondaryPos := P;
        end;
        P := P + 2 + Length(Name) + 12;
        Continue;
      end;
    end
    // Type-registry entry ($2A NL Name <flag> $00 $00 <primary-id>).
    // Bridge the primary to any cross-unit enum secondary that
    // appears as a 2-byte LE slot in this $2A's body.
    else if Tag = TYPE_REGISTRY_TAG then
    begin
      NameLen := ByteAt(P + 1);
      if (NameLen >= 2) and (NameLen <= 40) and
         (P + 2 + Integer(NameLen) + 5 < FSz) and
         (ByteAt(P + 2 + NameLen + 1) = $00) and
         (ByteAt(P + 2 + NameLen + 2) = $00) then
      begin
        var Primary: UInt32 :=
          UInt32(ByteAt(P + 2 + NameLen + 3)) or
          (UInt32(ByteAt(P + 2 + NameLen + 4)) shl 8);
        // Only the +7,+8 slot has been observed to consistently
        // hold the secondary id in genuine enum $2A entries.
        if (Primary <> 0) and (P + 2 + NameLen + 8 < FSz) then
        begin
          var SecCandidate: UInt32 :=
            UInt32(ByteAt(P + 2 + NameLen + 7)) or
            (UInt32(ByteAt(P + 2 + NameLen + 8)) shl 8);
          if (SecCandidate <> 0) and (SecCandidate <> Primary) and
             FCrossUnitEnumIds.ContainsKey(SecCandidate) then
          begin
            FEnumTypeIds[Primary] := True;
            var AliasList: IList<UInt32>;
            if not FEnumAliasesByPrimary.TryGetValue(Primary, AliasList) then
            begin
              AliasList := Collections.NewPlainList<UInt32>;
              FEnumAliasesByPrimary[Primary] := AliasList;
            end;
            if AliasList.IndexOf(SecCandidate) < 0 then
              AliasList.Add(SecCandidate);
          end;
        end;
      end;
    end
    else if (Tag = SCOPE_END) and SeenLocalSinceProc then
    begin
      InProc := False;
      SeenLocalSinceProc := False;
      LocalIdx := 0;
    end;
    Inc(P);
  end;
end;

procedure TRsmScanner.RecomputeProcSizes;
// Each proc record stores only its starting RVA; the size is not
// emitted, so estimate it from the gap to the next proc whose
// SegmentOffset is greater. Build a sorted index over the procs and
// assign Size := nextStart - thisStart (capped to a sane upper
// bound; the last proc gets the $1000 placeholder). Procs whose
// SegmentOffset is 0 (address decoding failed; the symbol-stream
// encoding for the proc's address wasn't recognized) get Size = 0
// and are treated as name-only entries.
const
  MaxProcSize  = $4000;
  LastFallback = $1000;
var
  Idx        : TIntegerDynArray;
  I, J, RunEnd: Integer;
  NextStart  : NativeUInt;
  RunOffset  : NativeUInt;
  Size       : NativeUInt;
  Gap        : Int64;
  P          : TRsmProc;
begin
  if FProcs.Count < 2 then Exit;
  FProcs.Sort(Idx, @CompareProcBySegmentOffset);

  // Assign sizes by walking runs of equal offset. Each run shares
  // one Size value (gap to the first subsequent offset > theirs).
  I := 0;
  while I <= High(Idx) do
  begin
    RunOffset := FProcs[Idx[I]].SegmentOffset;
    RunEnd := I;
    while (RunEnd + 1 <= High(Idx)) and
          (FProcs[Idx[RunEnd + 1]].SegmentOffset = RunOffset) do
      Inc(RunEnd);

    if RunOffset = 0 then
      Size := 0
    else if RunEnd = High(Idx) then
      Size := LastFallback
    else
    begin
      NextStart := FProcs[Idx[RunEnd + 1]].SegmentOffset;
      Gap := Int64(NextStart) - Int64(RunOffset);
      if Gap > MaxProcSize then Gap := MaxProcSize;
      Size := NativeUInt(Gap);
    end;

    for J := I to RunEnd do
    begin
      P := FProcs[Idx[J]];
      P.Size := Size;
      FProcs[Idx[J]] := P;
    end;
    I := RunEnd + 1;
  end;
end;

end.
