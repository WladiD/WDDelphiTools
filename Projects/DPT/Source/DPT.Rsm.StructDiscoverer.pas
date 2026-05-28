// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.StructDiscoverer;

// Discovers class + record declarations in the RSM (CSH7) byte stream
// and parses their member fields. Pulled out of DPT.Rsm.Scanner so the
// "second pass" -- scanning the file byte-by-byte looking for the
// duplicate-name class trailer (Win32 / Win64 layouts) and the $0E
// record sentinel, then walking each struct's fields -- has a single
// home, separate from the symbol-stream walker.
//
// The scanner owns the FClasses / FClassByName containers and passes
// them in at construction; this class writes into them. Buffer access
// goes through the shared <c>DPT.Rsm.BufferIO</c> helpers so the
// discoverer never depends on the scanner type.

interface

uses

  mormot.core.collections,

  DPT.Rsm.Model;

type

  /// <summary>
  ///   Class / record discovery pass for an RSM symbol container.
  ///   Stateless across runs apart from the (buf, sz) it caches at
  ///   the start of each <see cref="Run"/>.
  /// </summary>
  TRsmStructDiscoverer = class
  private
    FBuf        : PByte;
    FSz        : NativeInt;
    FClasses    : IList<TRsmClassInfo>;
    FClassByName: IKeyValue<String, Integer>;
    function  ByteAt(AOffset: NativeInt): Byte; inline;
    function  DwordAt(AOffset: NativeInt): UInt32; inline;
    function  IsPrintableAscii(AB: Byte): Boolean; inline;
    function  ReadIdentifier(AOffset: NativeInt; out AName: String): Boolean; inline;
    function  IsValidFieldTypeinfoPrefix(AOffset: NativeInt): Boolean; inline;
    function  FindClassIdxByName(const AName: String): Integer;
    procedure ScanFieldsBackwardFromClassName(AClassNameOff: NativeInt;
      AKind: TRsmStructKind; const AClassName: String;
      AMinStartOff: NativeInt = 0);
    procedure ScanFieldsForwardFromRecordName(ARecordNameOff: NativeInt;
      const ARecordName: String);
  public
    constructor Create(AClasses: IList<TRsmClassInfo>;
      AClassByName: IKeyValue<String, Integer>);

    /// <summary>
    ///   Walk the entire RSM byte-by-byte and parse every class or
    ///   record signalled by the class-trailer / record-sentinel
    ///   patterns. Writes results into <c>AClasses</c> /
    ///   <c>AClassByName</c> in source-declaration order.
    /// </summary>
    procedure Run(ABuf: PByte; ASz: NativeInt);
  end;

implementation

uses

  System.SysUtils,

  mormot.core.base,

  DPT.Rsm.BufferIO;

{ TRsmStructDiscoverer }

constructor TRsmStructDiscoverer.Create(AClasses: IList<TRsmClassInfo>;
  AClassByName: IKeyValue<String, Integer>);
begin
  inherited Create;
  FClasses     := AClasses;
  FClassByName := AClassByName;
end;

function TRsmStructDiscoverer.ByteAt(AOffset: NativeInt): Byte;
begin
  Result := RsmByteAt(FBuf, AOffset);
end;

function TRsmStructDiscoverer.DwordAt(AOffset: NativeInt): UInt32;
begin
  Result := RsmDwordAt(FBuf, AOffset);
end;

function TRsmStructDiscoverer.IsPrintableAscii(AB: Byte): Boolean;
begin
  Result := RsmIsPrintableAscii(AB);
end;

function TRsmStructDiscoverer.ReadIdentifier(AOffset: NativeInt;
  out AName: String): Boolean;
begin
  Result := RsmReadIdentifier(FBuf, FSz, AOffset, AName);
end;

function TRsmStructDiscoverer.IsValidFieldTypeinfoPrefix(
  AOffset: NativeInt): Boolean;
begin
  Result := RsmIsValidFieldTypeinfoPrefix(FBuf, FSz, AOffset);
end;

function TRsmStructDiscoverer.FindClassIdxByName(const AName: String): Integer;
begin
  if not FClassByName.TryGetValue(LowerCase(AName), Result) then
    Result := -1;
end;

procedure TRsmStructDiscoverer.ScanFieldsBackwardFromClassName(
  AClassNameOff: NativeInt; AKind: TRsmStructKind; const AClassName: String;
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
  // Upper bound on captured fields per class. Was 128, which silently
  // truncated very large legacy VCL forms: TFW's TFormAd has ~496 real
  // fields (controls + strict-private F-fields), and at 128 every field
  // past scan position #128 — including `FAd` (offset 0x0C5C, the field
  // the live `evaluate Self.FAd.Land` repro needs) — was dropped while
  // the early published-control run filled the slots (§6.16). 2048 gives
  // ~4x headroom over the largest observed class and still bounds the
  // O(Count^2) overlap-reject and offset-sort loops below to a few
  // million ops on the worst class (negligible against the 45 s
  // DiscoverAndParseAllStructs budget). Cross-class leakage is prevented
  // by AMinStartOff (the previous class's TypeIdx), NOT by this cap, so
  // raising it does not pull in a neighbouring class's fields.
  MaxFields    = 2048;
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
    // Off = 0 is reserved for the VMT pointer (offset 0..3 / 0..7
    // depending on platform); real class fields always sit past
    // it. Skip Off = 0 to avoid the false-positive that fires on
    // an incidental "<4 zero bytes> 04 Self <typeinfo-anchor>"
    // sequence emitted near every method-bearing class (the
    // $21 REGVAR record for the method's implicit Self parameter
    // sits in the backward window after the class trailer).
    if (Off > $FFFF) or (Off = 0) then
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
    // Structural anchor: every real class-field record is followed
    // immediately by a 4-byte typeinfo prefix `$02 $00 <section-flag>
    // $00`. The third byte is a section/visibility marker. Full
    // taxonomy (§6.14 closure, pinned by
    // TestVisibilityMarkerTaxonomy32):
    //   $00 = terminal record in the field section (last field, or
    //         last field of a class with no further sections)
    //   $01 = strict private / protected next-field
    //   $02 = strict protected / public next-field (in non-$M+
    //         classes; default-visibility fields in TInner /
    //         TDerived land here)
    //   $03 = public next-field in a $M+ class (e.g.
    //         TNoFPrefixHost.PublicInt sits inside a class that
    //         got $M+ flipped on via its `published` section)
    // The walker accepts marker in [$00..$0F]; the predicate is
    // intentionally looser than the observed set because the linker
    // may introduce additional markers for visibilities we haven't
    // probed yet (interface fields, class-helper-added members),
    // and the surrounding bytes 0/1/3 of the anchor still pin the
    // shape tightly.
    //
    // This anchor is far more selective than the previous Name[1]='F'
    // heuristic and admits classes whose fields don't follow the
    // conventional F-prefix (e.g. DebugTarget's TNoFPrefixHost, or
    // real-world TFW-style CamelCase fields). The 6-byte anchor the
    // forward record-field walker uses (RsmIsValidFieldTypeinfoPrefix)
    // would be tighter but breaks on the terminal field of a class
    // that declares methods (e.g. TDerived.FDerivedLabel), where
    // byte +4 of the typeinfo is non-zero.
    if (P + 4 + 1 + NameLen + 3 >= FSz) or
       (ByteAt(P + 4 + 1 + NameLen)     <> $02) or
       (ByteAt(P + 4 + 1 + NameLen + 1) <> $00) or
       (ByteAt(P + 4 + 1 + NameLen + 2) > $0F) or
       (ByteAt(P + 4 + 1 + NameLen + 3) <> $00) then
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

procedure TRsmStructDiscoverer.ScanFieldsForwardFromRecordName(
  ARecordNameOff: NativeInt; const ARecordName: String);
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
  RecordSize  : UInt32;
begin
  NL := Length(ARecordName);
  if ARecordNameOff + 1 + NL + 4 > FSz then Exit;
  // The 4-byte LE DWORD between the record name and the field-record
  // stream is the record's total byte size (§6.13 closure -- used to
  // derive the terminal field's Size by subtracting its offset).
  RecordSize := DwordAt(ARecordNameOff + 1 + NL);
  PStart := ARecordNameOff + 1 + NL + 4;

  // The header between the size DWORD and the first field tag uses
  // the SIMPLE shape mapped in DPT.Rsm.Format.md §4.13: byte 0 =
  // managed-field count (N), byte 5+N*K = declared field count,
  // total length = 17 + N*8 on Win32 / 25 + N*16 on Win64.
  // The "elaborate ~500-byte header" hypothesis that previously
  // motivated the 4 KB scan window is **refuted** -- the §6.4
  // diagnostic (Test.DPT.Rsm.Tfw.TestTfwSimpleRecordHeaderCovers
  // TfwRecords) verified that even TFW's TAppCaps fits the simple
  // shape exactly (gap = 33 bytes = 17 + 2*8 for managed=2, with
  // member[0]="AddDict" landing precisely at PStart+33).
  //
  // We still scan rather than jumping to the predicted offset,
  // because the strict typeinfo-prefix anchor gives us "right
  // first field or no match" detection essentially for free: a
  // STRUCTURAL match on the 6 bytes after the field name
  //     $02 $00 <last-flag> $00 $00 $00          (last-flag in {$00, $02})
  // is far more selective than any first-character rule could
  // be, and DOES NOT depend on Delphi house style (so records
  // whose fields don't start with 'F' -- TFW's TAppCaps.DbKindName,
  // TMdt.Id, ... -- get parsed correctly).
  // The 4 KB window is kept as a safety margin against any
  // unforeseen header variants; on the entire DebugTarget + TFW
  // corpus it resolves on the very first iteration.
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
    //
    // For the LAST member (no successor), §6.13 closure: use the
    // record's total byte size (from the 4-byte DWORD between the
    // record name and the field-record stream) to compute
    // `Size = RecordSize - Member.Offset`. This recovers the
    // terminal field's width for the common non-variant case
    // without forcing the evaluator's type-fallback path. Variant
    // overlays still fall through to Size = 0 (they have a
    // sibling at the same offset, so the (I + 1 < Count) branch
    // doesn't fire and we'd hit the I + 1 == Count branch -- but
    // the sibling's Cands[I+1].Offset equals Cands[I].Offset so
    // the first arm doesn't take it. Only TRUE terminal fields
    // benefit; variants stay safe.
    if (I + 1 < Count) and (Cands[I + 1].Offset > Cands[I].Offset) then
      Member.Size := Cands[I + 1].Offset - Cands[I].Offset
    else if (I + 1 = Count) and (RecordSize > Cands[I].Offset) then
      Member.Size := RecordSize - Cands[I].Offset
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

procedure TRsmStructDiscoverer.Run(ABuf: PByte; ASz: NativeInt);
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
      // Defer to a CLOSER class-def of the same name (§6.16). A class
      // name appears many times in the stream (type registry, the
      // trailer's own second copy, references, AND -- the case that
      // bites -- an earlier spurious occurrence that also passes the
      // 4-zero method-header gate). The trailer belongs to the class-def
      // occurrence nearest before it. So if, while scanning forward for
      // the trailer, we hit another occurrence of THIS name that is
      // itself class-def-shaped, abort: that closer occurrence owns the
      // trailer, and the Run loop will reach and parse it. Without this
      // guard, widening the window lets a far/earlier same-name
      // occurrence reach and steal the real class-def's trailer, so its
      // members get attributed to the wrong neighbour (a TComponent ->
      // TComponentInterfaceDelegate-style mis-anchor on DebugTarget).
      // The trailer's OWN second copy is never seen here: the trailer
      // checks above Exit(True) at the marker, 5/9 bytes before it.
      if SecondNameMatchesBytes(P, ANameStart, ANL) then
      begin
        var QEnd: NativeInt := P + 1 + Integer(ANL);
        if (QEnd + 8 < FSz) and
           ( // 4-zero method-block header (method-class def)
             ((ByteAt(QEnd + 1) = 0) and (ByteAt(QEnd + 2) = 0) and
              (ByteAt(QEnd + 3) = 0) and (ByteAt(QEnd + 4) = 0)) or
             // Win32 tight trailer marker right after the name
             ((ByteAt(QEnd) = $04) and (ByteAt(QEnd + 1) = 0) and
              (ByteAt(QEnd + 2) = 0) and (ByteAt(QEnd + 3) = 0) and
              (ByteAt(QEnd + 4) = $07)) or
             // Win64 tight trailer marker right after the name
             ((ByteAt(QEnd) = $08) and (ByteAt(QEnd + 1) = 0) and
              (ByteAt(QEnd + 2) = 0) and (ByteAt(QEnd + 3) = 0) and
              (ByteAt(QEnd + 4) = 0) and (ByteAt(QEnd + 5) = 0) and
              (ByteAt(QEnd + 6) = 0) and (ByteAt(QEnd + 7) = 0) and
              (ByteAt(QEnd + 8) = $07)) ) then
          Exit(False);
      end;
      Inc(P);
    end;
  end;

begin
  FBuf := ABuf;
  FSz  := ASz;
  if (ABuf = nil) or (ASz < 8) then Exit;
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
      // always starts with a small DWORD at NameEnd whose **three
      // high bytes are zero** (the method-records header size /
      // count -- the low byte at +1 carries the actual value and
      // varies by build). The *type-record* region's same name
      // has non-zero garbage at NameEnd+1..+4, so checking just
      // the high three bytes (+2..+4) cleanly separates the two.
      // Earlier the filter also required +1 = 0; that incidentally
      // held on the C:\MSE\TFW corpus we calibrated against but
      // breaks on builds whose TFormAd-class method blocks emit a
      // non-zero count (the live C:\MSE-26.04-Mongo build emits
      // 0x1D at +1, breaking class discovery for every method-
      // bearing class). The comment had said "three high bytes
      // zero" all along -- the code was tighter than its intent.
      else if (NameEnd + 4 < FSz) and
              (ByteAt(NameEnd + 2) = 0) and
              (ByteAt(NameEnd + 3) = 0) and
              (ByteAt(NameEnd + 4) = 0) and
              // 16 KB window. Method-heavy classes put their trailer one
              // method record per declared method past the class-def
              // name: TStrings ~5.6 KB, RTL classes like TStream/TWriter
              // and large VCL forms (TFW's TFormAd ~12.6 KB) need more
              // than the original 8 KB. Correctness at this width does
              // NOT rely on the window being tight -- FindClassTrailerWithin
              // defers to a closer same-name class-def (see §6.16), so a
              // far/spurious same-name occurrence can no longer reach and
              // steal the real class-def's trailer. 16 KB is the
              // perf/coverage balance: it discovers the large classes
              // while keeping DiscoverAndParseAllStructs well inside its
              // budget (~16 s vs 45 s; 64 KB blew it at ~54 s).
              FindClassTrailerWithin(NameEnd, 16384, NameStart, L) then
        IsClass := True;

      if IsClass then
      begin
        if ReadIdentifier(P, Name) and (FindClassIdxByName(Name) < 0) then
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
        if ReadIdentifier(P, Name) and (FindClassIdxByName(Name) < 0) then
          ScanFieldsForwardFromRecordName(P, Name);
      end;
    end;
    Inc(P);
  end;
end;

end.
