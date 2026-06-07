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

  System.SysUtils,

  mormot.core.collections,
  mormot.core.os,

  DPT.Rsm.Model,
  DPT.Rsm.BufferIO,
  DPT.Rsm.EnumDecoder,
  DPT.Rsm.StructDiscoverer;

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
    /// Module-level variables: name -> 32-bit decoded VA recovered
    /// from the 4-byte slot that follows the type id in $27 and $20
    /// (module-global form) records. The slot's wire encoding is
    /// (Value shl 4) or $07 -- same scheme as the $28 PROC Win32
    /// address. The Value's platform-specific meaning:
    /// * **Win32**: absolute VA (image base $00400000 already
    ///   included; RVA-from-image-base = Value - $00400000).
    /// * **Win64**: RVA relative to the image base ($140000000 by
    ///   convention); absolute VA = $140000000 + Value.
    /// Only populated when the slot's low nibble is the expected
    /// $07 tag; absent for records where the VA cannot be
    /// recovered.
    FGlobalVa        : IKeyValue<String, UInt32>;
    /// §6.21 cross-unit symbol-import segments, in scan order.
    /// Populated by HandleUnitUseIntroRecord on each $64-prefixed
    /// segment that matches the structural anchor (`$64 NL UnitName
    /// $00 $00 $00`). Each segment's Refs are the consecutive
    /// $66/$67/$70 entries that follow up to the next $63 SCOPE_END
    /// (or the first byte that's not in {$66,$67,$70,$63}).
    FUnitUseSegments : IList<TRsmUnitUseSegment>;
    /// §4.17 source-file records (`$70 <SourceFile>` introducers), one
    /// per distinct importing unit. Each `$64` segment's
    /// `SourceFileIdx` indexes into this list, so the importing unit's
    /// name is stored ONCE here rather than copied per segment.
    /// Populated by `HandleSourceFileIntroRecord`.
    FSourceFiles     : IList<TRsmSourceFile>;
    /// Dedup index (lower(UnitName) -> FSourceFiles index) so a `$70`
    /// introducer that recurs for the same unit reuses its entry
    /// instead of appending a duplicate.
    FSourceFileByName: IKeyValue<String, Integer>;
    /// Scan-loop state: the FSourceFiles index of the most recent
    /// `$70` introducer, stamped onto the `$64` segments that follow.
    /// Reset to -1 at the start of each scan.
    FCurrentSourceFileIdx: Integer;
    /// True when the loaded fixture targets x64 (PE Machine field
    /// `$8664`). Detected in `LoadFromFile` by reading the .exe's PE
    /// header; defaults to False (Win32) when only `LoadFromBytes`
    /// or `LoadFromBuffer` is used and no .exe is available to
    /// inspect. Consumed by `DecodeProcAddrPayload` to choose the
    /// correct subtraction constant for the `$A0` sub-form (Win32
    /// subtracts `$401000` = image base + .text RVA; Win64
    /// subtracts only `$1000` = .text RVA, since the Win64 image
    /// base `$140000000` sits in VA bits 32-39 which the 4-byte
    /// encoded slot doesn't carry). See §6.2 in DPT.Rsm.Format.md.
    FIs64Bit         : Boolean;
    /// Owns the enum-related lookup tables and the cross-unit
    /// pending-buffer state machine. Pulled out of TRsmScanner so
    /// the $25/$03/$2A handling lives in one place; the scanner
    /// still does the byte-level format parsing and forwards
    /// parsed data via FEnumDecoder.Record* method calls.
    FEnumDecoder     : TRsmEnumDecoder;
    /// Owns the class / record discovery pass (Win32 / Win64
    /// trailer detection, $0E record sentinel, backward / forward
    /// field walkers). The scanner constructs it with the shared
    /// FClasses / FClassByName containers and runs it from
    /// LoadFromBuffer after ScanSymbolStream + RecomputeProcSizes.
    FStructDiscoverer: TRsmStructDiscoverer;
    FOnPhase     : TProc<String>;
    /// Scan-loop state, valid only for the duration of
    /// <see cref="ScanSymbolStream"/>. Held as fields so the per-tag
    /// handler methods below can share them without bloated var-
    /// parameter lists.
    FScanInProc            : Boolean;
    FScanSeenLocalSinceProc: Boolean;
    FScanLocalIdx          : Integer;
    FScanRegParam          : Integer;
    function  GetEnumConstNames: IKeyValue<String, String>;
    function  GetEnumTypeIds: IKeyValue<UInt32, Boolean>;
    function  GetCrossUnitEnumIds: IKeyValue<UInt32, Boolean>;
    function  GetEnumAliasesByPrimary: IKeyValue<UInt32, IList<UInt32>>;
    function  GetEnumDefs: IList<TRsmEnumDef>;
    function  GetEnumDecoder: TRsmEnumDecoder;
    procedure ReportPhase(const APhase: String); inline;
    function  DecodeProcAddrPayload(AStartOff: NativeInt): NativeUInt;
    function  HandleProcRecord(var P: NativeInt): Boolean;
    function  HandleParamRecord(var P: NativeInt): Boolean;
    function  HandleRegVarRecord(var P: NativeInt): Boolean;
    function  HandleLocalRecord(var P: NativeInt): Boolean;
    function  DecodeTypeIdByStructuralLookahead(APayloadStart: NativeInt): UInt32;
    function  HandleModuleGlobalLocalTagRecord(var P: NativeInt): Boolean;
    function  HandleGlobalPrimRecord(var P: NativeInt): Boolean;
    function  HandleEnumConstantRecord(var P: NativeInt): Boolean;
    procedure HandleEnumDefRecord(P: NativeInt);
    procedure HandleTypeRegistryRecord(P: NativeInt);
    function  HandleUnitUseIntroRecord(var P: NativeInt): Boolean;
    function  HandleSourceFileIntroRecord(var P: NativeInt): Boolean;
    procedure ScanSymbolStream;
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
    ///   True when the 4 bytes at <c>AOffset</c> equal <c>AB0..AB3</c> in
    ///   stream order — one 32-bit load, but the call spells out the
    ///   expected anchor bytes (self-documenting). See RsmDwordAtEquals.
    /// </summary>
    function DwordAtEquals(AOffset: NativeInt;
      AB0, AB1, AB2, AB3: Byte): Boolean; inline;
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
    /// Architecture flag (see <see cref="FIs64Bit"/>). Public so the
    /// reader and tests can confirm the right decoder path was
    /// selected.
    property Is64Bit: Boolean read FIs64Bit;

    property Procs              : IList<TRsmProc> read FProcs;
    property Classes            : IList<TRsmClassInfo> read FClasses;
    property ProcByName         : IKeyValue<String, Integer> read FProcByName;
    property ClassByName        : IKeyValue<String, Integer> read FClassByName;
    property GlobalByName       : IKeyValue<String, UInt32> read FGlobalByName;
    property GlobalFileOffset   : IKeyValue<String, NativeInt> read FGlobalFileOffset;
    property GlobalVa           : IKeyValue<String, UInt32> read FGlobalVa;
    /// §6.21 cross-unit symbol-import segments. See
    /// <c>FUnitUseSegments</c> for the per-segment shape.
    property UnitUseSegments    : IList<TRsmUnitUseSegment> read FUnitUseSegments;
    /// §4.17 source-file (`$70`) records, one per distinct importing
    /// unit. A `$64` segment's `SourceFileIdx` indexes into this list.
    property SourceFiles        : IList<TRsmSourceFile> read FSourceFiles;
    property EnumConstNames     : IKeyValue<String, String> read GetEnumConstNames;
    property EnumTypeIds        : IKeyValue<UInt32, Boolean> read GetEnumTypeIds;
    property CrossUnitEnumIds   : IKeyValue<UInt32, Boolean> read GetCrossUnitEnumIds;
    property EnumAliasesByPrimary: IKeyValue<UInt32, IList<UInt32>> read GetEnumAliasesByPrimary;
    property EnumDefs            : IList<TRsmEnumDef> read GetEnumDefs;
    /// Direct access to the enum-decoder instance. Exposed so the
    /// Format-A linker can call <c>FindNearestPrimaryByLowByte</c>
    /// during §6.9 enum-typed $2C field linking; the existing
    /// per-collection getters return read-only views, but the
    /// nearest-offset lookup is a method, not a collection.
    property EnumDecoder         : TRsmEnumDecoder read GetEnumDecoder;
  end;

implementation

uses

  System.Classes,

  mormot.core.base;

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
  FProcs            := Collections.NewPlainList<TRsmProc>;
  FClasses          := Collections.NewPlainList<TRsmClassInfo>;
  FProcByName       := Collections.NewPlainKeyValue<String, Integer>;
  FClassByName      := Collections.NewPlainKeyValue<String, Integer>;
  FGlobalByName     := Collections.NewPlainKeyValue<String, UInt32>;
  FGlobalFileOffset := Collections.NewPlainKeyValue<String, NativeInt>;
  FGlobalVa         := Collections.NewPlainKeyValue<String, UInt32>;
  FUnitUseSegments  := Collections.NewPlainList<TRsmUnitUseSegment>;
  FSourceFiles      := Collections.NewPlainList<TRsmSourceFile>;
  FSourceFileByName := Collections.NewPlainKeyValue<String, Integer>;
  FEnumDecoder      := TRsmEnumDecoder.Create;
  FStructDiscoverer := TRsmStructDiscoverer.Create(FClasses, FClassByName);
end;

destructor TRsmScanner.Destroy;
begin
  if FOwnsMapping then
    FMapping.UnMap;
  FStructDiscoverer.Free;
  FEnumDecoder.Free;
  inherited;
end;

function TRsmScanner.GetEnumConstNames: IKeyValue<String, String>;
begin
  Result := FEnumDecoder.EnumConstNames;
end;

function TRsmScanner.GetEnumTypeIds: IKeyValue<UInt32, Boolean>;
begin
  Result := FEnumDecoder.EnumTypeIds;
end;

function TRsmScanner.GetCrossUnitEnumIds: IKeyValue<UInt32, Boolean>;
begin
  Result := FEnumDecoder.CrossUnitEnumIds;
end;

function TRsmScanner.GetEnumAliasesByPrimary: IKeyValue<UInt32, IList<UInt32>>;
begin
  Result := FEnumDecoder.EnumAliasesByPrimary;
end;

function TRsmScanner.GetEnumDefs: IList<TRsmEnumDef>;
begin
  Result := FEnumDecoder.EnumDefs;
end;

function TRsmScanner.GetEnumDecoder: TRsmEnumDecoder;
begin
  Result := FEnumDecoder;
end;

procedure TRsmScanner.ReportPhase(const APhase: String);
begin
  if Assigned(FOnPhase) then
    FOnPhase(APhase);
end;

function TRsmScanner.ByteAt(AOffset: NativeInt): Byte;
begin
  Result := RsmByteAt(FBuf, AOffset);
end;

function TRsmScanner.DwordAt(AOffset: NativeInt): UInt32;
begin
  Result := RsmDwordAt(FBuf, AOffset);
end;

function TRsmScanner.DwordAtEquals(AOffset: NativeInt;
  AB0, AB1, AB2, AB3: Byte): Boolean;
begin
  Result := RsmDwordAtEquals(FBuf, AOffset, AB0, AB1, AB2, AB3);
end;

function TRsmScanner.IsPrintableAscii(AB: Byte): Boolean;
begin
  Result := RsmIsPrintableAscii(AB);
end;

function TRsmScanner.ReadIdentifier(AOffset: NativeInt; out AName: String): Boolean;
begin
  Result := RsmReadIdentifier(FBuf, FSz, AOffset, AName);
end;

function TRsmScanner.IsValidFieldTypeinfoPrefix(AOffset: NativeInt): Boolean;
begin
  Result := RsmIsValidFieldTypeinfoPrefix(FBuf, FSz, AOffset);
end;

function DetectIs64BitExe(const AExePath: String): Boolean;
// Reads the PE header's COFF Machine field to distinguish Win32
// ($014C = IMAGE_FILE_MACHINE_I386) from Win64 ($8664 =
// IMAGE_FILE_MACHINE_AMD64). Returns False on any IO error or when
// the file isn't a recognisable PE -- callers treat that as "assume
// Win32", which matches the historical decoder behaviour.
var
  Stream  : TFileStream;
  PEOffset: UInt32;
  Machine : UInt16;
begin
  Result := False;
  if not FileExists(AExePath) then Exit;
  try
    Stream := TFileStream.Create(AExePath,
      fmOpenRead or fmShareDenyNone);
    try
      if Stream.Size < $40 then Exit;
      Stream.Position := $3C;
      if Stream.Read(PEOffset, 4) <> 4 then Exit;
      if Int64(PEOffset) + 6 >= Stream.Size then Exit;
      // PE signature is 4 bytes "PE\0\0", Machine sits at PE+4.
      Stream.Position := PEOffset + 4;
      if Stream.Read(Machine, 2) <> 2 then Exit;
      Result := Machine = $8664;
    finally
      Stream.Free;
    end;
  except
    // IO failures fall through to Result := False -- the historical
    // decoder treats every unknown binary as Win32.
  end;
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
  // Detect arch from the .exe BEFORE the .rsm load so the proc-
  // address decoder picks the right $A0-form subtraction constant
  // (Win32: $401000, Win64: $1000 -- the difference is the Win32
  // image base $400000 that the Win64 encoding pushes into VA bits
  // 32-39 which the 4-byte slot doesn't carry).
  FIs64Bit := DetectIs64BitExe(AExePath);
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
  FGlobalVa.Clear;
  FUnitUseSegments.Clear;
  FSourceFiles.Clear;
  FSourceFileByName.Clear;
  FEnumDecoder.Reset;
  FBuf := ABuf;
  FSz  := ASize;
  // FIs64Bit is set by LoadFromFile from the .exe's PE header. The
  // LoadFromBytes / LoadFromBuffer entry points have no .exe to
  // inspect, so we don't reset it here; callers that need a clean
  // arch slate must call LoadFromFile or set the flag explicitly.
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
  FStructDiscoverer.Run(FBuf, FSz);
  ReportPhase('DiscoverAndParseAllStructs');
end;

function TRsmScanner.DecodeProcAddrPayload(AStartOff: NativeInt): NativeUInt;
// Decode the variable-length address payload that follows a
// $28 PROC record's name. Returns the segment-relative RVA
// (offset within the .text section) or 0 when the record is a
// forward-declaration / cross-reference with no usable address.
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
// Two sub-forms are observed:
//
// $20 sub-tag (simple inline form, used by small Win64 binaries like
// DebugTarget):
//   address block is variable-length: 4 bytes when the proc is small
//   (size <= $80), 5 bytes for larger procs, followed by the fixed
//   terminator "04 10 ?? 2E 00". Only bytes 0..2 are needed to
//   recover the RVA; bytes 3/4 encode proc size + local-layout info.
//   Decoded layout (LSB-first):
//     byte 0 bits 0-6 : constant $03 (encoding-kind tag for code)
//     byte 0 bit  7   : VA bit 4
//     byte 1          : VA bits 5-12  (full 8 bits)
//     byte 2          : VA bits 13-20 (full 8 bits)
//   Procs are 16-byte aligned, so VA bits 0-3 are implicitly zero.
//   This covers code sections up to 2 MB.
//
// $A0 sub-tag (extended form with type-ref / timestamp metadata,
// used by larger Win64 binaries like TFW.Win64; closed §6.2):
//   bytes 0..2 are the sub-tag preamble `$A0 $00 $00`. The address
//   sits at bytes 7..10 of the payload as a 4-byte LE DWORD encoding
//   `(VA shl 4) or $07`, exactly the same wire format Win32 uses --
//   but the Win64 VA's high 32 bits ($140000000-shaped image base
//   bits) don't fit in the 4-byte slot, so the encoder stores only
//   the lower 32 bits. SegmentOffset is then recovered as
//   `(DW shr 4) - $1000` (subtracting the .text section's RVA from
//   the image base only, NOT the Win32 image-base + .text constant
//   `$401000` that the Win32 decoder uses). The arch-specific
//   subtraction is what the new `TryWin64A0` handles.
//   Verified on TFW.Win64: TFormMain.Create at byte 3..10 `A0 00 00
//   F4 A2 28 8C 07 05 E1 7B` -- DW(7..10) = $7BE10507, shr 4 =
//   $07BE1050, -$1000 = $07BE0050 which matches the .map entry
//   `0001:07BE0050 Tfw.Main.Form.TFormMain.Create` byte-exactly.

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

  function TryWin64A0(AOff: NativeInt): NativeUInt;
  // Win64 $A0-form address decoder. Two sub-variants distinguished
  // by byte 0's low nibble:
  //
  // Variant A (high-RVA, low nibble = $07): 4-byte LE DWORD
  // `(VA shl 4) or $07`. Subtracts $1000 (the .text-section RVA)
  // instead of $401000 -- the Win32 image base $400000 sits in VA
  // bits 20-31 there, whereas Win64's image base $140000000 sits in
  // bits 32-39 which the 4-byte slot doesn't carry. See §6.2.
  // Used for procs with .text-RVA in (~0, 256 MB]. Capacity caps at
  // 28 bits.
  //
  // Variant B (low-RVA, (byte 0 and $7F) == $03): same 21-bit
  // packed form the $20 sub-tag's TryWin64 uses, applied to bytes
  // 0..2 of the $A0 form's address slot:
  //   byte 0 bit 7 = VA bit 4
  //   byte 1      = VA bits 5..12
  //   byte 2      = VA bits 13..20
  // Verified on TFW.Win64 procs whose .map RVA fits in 21 bits
  // (TStringList.Add @$145460, TObject.Create @$10EE0,
  // System.SysUtils.IntToStr @$536C0). See §6.7.
  var DW: UInt32; Dec: Int64; B0, B1, B2: Byte; Va: UInt32;
  begin
    Result := 0;
    if AOff + 3 >= FSz then Exit;
    B0 := ByteAt(AOff);
    if (B0 and $0F) = $07 then
    begin
      // Variant A: 28-bit VA in 4-byte slot.
      DW := DwordAt(AOff);
      Dec := (Int64(DW) shr 4) - $1000;
      if (Dec > 0) and (Dec < $10000000) then
        Result := NativeUInt(Dec);
      Exit;
    end;
    if (B0 and $7F) = $03 then
    begin
      // Variant B: 21-bit VA across bytes 0..2.
      B1 := ByteAt(AOff + 1);
      B2 := ByteAt(AOff + 2);
      Va := ((UInt32(B0) shr 7) and 1) shl 4 or
            (UInt32(B1) shl 5) or
            (UInt32(B2) shl 13);
      Va := Va and $1FFFFF;
      if Va >= $1000 then
        Result := NativeUInt(Va - $1000);
    end;
  end;

  function TryWin64(AOff: NativeInt): NativeUInt;
  var MOff: NativeInt; B0, B1, B2: Byte; Va: UInt32;
  begin
    Result := 0;
    if AOff + 4 >= FSz then Exit;
    if (ByteAt(AOff) and $7F) <> $03 then Exit;
    // Marker layout "04 ?? ?? 2E ??": this 4-byte form is what the
    // Win64 proc-address payload's trailer looks like, and only bytes
    // 0 (`$04`) and 3 (`$2E`) are reliably stable in practice. See
    // Format.md §4.1 for the full per-proc marker shape (variable
    // length, ends with `<platform-anchor> $2E <owner-ref>`).
    //
    // §6.22 finding: the "$2E" here ISN'T a constant -- it's the HI
    // byte of a 2-byte LE owner-id pair pointing at the owning
    // class's $2A registry entry. Every observed class on
    // DebugTarget + TFW lands in the $2E00..$2FFF range so the HI
    // byte is always $2E or $2F and the sanity check works
    // coincidence-based. A linker build that scattered class type
    // ids across a wider range would break this check; the
    // appropriate fix when that surfaces is to also accept $2F here
    // and/or move the anchor onto the platform-anchor byte at +2
    // ($11 on Win32, $3D on Win64).
    //
    // §6.17 closure: the earlier `byte 1 = $10 AND byte 4 = $00`
    // restriction matched only no-Self procs like LocalsProcedure;
    // every instance method (TDerived.TouchSelf, TStaleSelfHost.Probe)
    // fell through, landed with SegmentOffset = 0, got Size = 0 in
    // RecomputeProcSizes, and FindProcContaining for any PC inside
    // them returned the preceding proc whose Size extended across
    // the gap.
    for MOff := AOff + 4 to AOff + 5 do
      if (MOff + 4 < FSz) and
         (ByteAt(MOff)     = $04) and
         (ByteAt(MOff + 3) = $2E) then
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
      begin
        // On Win64 the $A0 form sits at bytes 7..10 with the same
        // (VA shl 4) | $07 wire layout as Win32, but the implicit
        // image base differs ($140000000 vs $400000) so the
        // subtraction constant changes. See TryWin64A0 / §6.2.
        if FIs64Bit then
          Result := TryWin64A0(AStartOff + 7)
        else
          Result := TryWin32(AStartOff + 7);
      end;
    $41:
      Result := TryWin32(AStartOff + 4);
  end;
end;

function TRsmScanner.HandleProcRecord(var P: NativeInt): Boolean;
// Proc record: $28 NameLen Name <variable-length payload>.
// Returns True after consuming the record (P advanced past the
// name); False on rejection so the outer dispatcher single-byte-
// advances.
var
  NameLen    : Byte;
  Name       : String;
  Proc       : TRsmProc;
  Decoded    : NativeUInt;
  ExistingIdx: Integer;
begin
  Result := False;
  NameLen := ByteAt(P + 1);
  if (NameLen < 1) or (NameLen > 64) then Exit;
  if not ReadIdentifier(P + 1, Name) then Exit;
  // Real proc declarations are preceded by a distinctive
  // run-up (e.g. $FF $28 for the extended form, or just $28
  // for the simple inline form). Detect the address payload
  // by scanning the post-name window for a recognizable
  // encoding pattern rather than assuming a fixed offset
  // because large binaries emit a richer prefix (timestamp /
  // type-ref / etc.) between name and address.
  Decoded := 0;
  if P + 2 + Length(Name) + 4 < FSz then
    Decoded := DecodeProcAddrPayload(P + 2 + Length(Name));
  // First time we see this name: add a fresh entry.
  // Already there: a later occurrence often carries the real
  // address while the first was a forward declaration, so
  // patch the existing entry when the new decode yields a
  // non-zero result.
  if not FProcByName.TryGetValue(LowerCase(Name), ExistingIdx) then
    ExistingIdx := -1;
  if ExistingIdx < 0 then
  begin
    Proc := Default(TRsmProc);
    Proc.Name := Name;
    Proc.SegmentOffset := Decoded;
    if Decoded > 0 then
      Proc.Size := $1000
    else
      Proc.Size := 0;
    // §4.18: bind the proc to its declaring source file via the live
    // $70 source-file introducer cursor. Each unit's proc block is
    // immediately preceded by exactly one $70 record naming the unit's
    // own source file; the cursor freezes on it until the next unit's
    // introducer. -1 here means the proc precedes any $70 (the early-
    // stream import thunks).
    Proc.StreamOffset := NativeUInt(P);
    Proc.SourceFileIdx := FCurrentSourceFileIdx;
    Proc.Locals := Collections.NewPlainList<TRsmLocal>;
    FProcByName[LowerCase(Name)] := FProcs.Count;
    // Linker-emitted "@Name" aliases should be findable by
    // both the prefixed and the bare form, so register the
    // alias too when the stored name starts with "@".
    if (Length(Name) > 0) and (Name[1] = '@') then
      FProcByName[LowerCase(Copy(Name, 2, MaxInt))] := FProcs.Count;
    FProcs.Add(Proc);
    FScanInProc := True;
    FScanSeenLocalSinceProc := False;
    FScanLocalIdx := 0;
    FScanRegParam := 0;
  end
  else if (FProcs[ExistingIdx].SegmentOffset = 0) and (Decoded > 0) then
  begin
    Proc := FProcs[ExistingIdx];
    Proc.SegmentOffset := Decoded;
    Proc.Size := $1000;
    // The definition record carries the authoritative declaring-unit
    // cursor; prefer it over a forward declaration's (which may have
    // preceded the unit's $70 introducer and so be -1).
    Proc.StreamOffset := NativeUInt(P);
    if FCurrentSourceFileIdx >= 0 then
      Proc.SourceFileIdx := FCurrentSourceFileIdx;
    FProcs[ExistingIdx] := Proc;
  end;
  P := P + 2 + Length(Name);
  Result := True;
end;

function TRsmScanner.DecodeTypeIdByStructuralLookahead(
  APayloadStart: NativeInt): UInt32;
// §6.15 closure: when the typeId Hi byte (at APayloadStart+4) is
// neither $2E nor $2F (the well-known structured-type markers), the
// record may still be a 2-byte typeId — cross-unit RTL types use a
// linker-minted per-binary alias whose Hi byte varies widely. The
// distinguishing feature is the position of the NEXT record's tag
// byte:
//
//   1-byte typeId form:  <lo> <pad>  <next-tag>           (next tag at +5)
//   2-byte typeId form:  <lo> <hi>   <pad>  <next-tag>    (next tag at +6)
//
// Continuation tag set: the param/regvar/local/proc/scope-end tags
// (plus $25 ENUM_CONST when the proc body has cross-unit constants
// inline, $03 ENUM_DEF, $28 PROC for the next proc record).
// Falls back to 1-byte read when neither +5 nor +6 carries a
// recognizable continuation byte (e.g. the open-array hidden
// sub-record `$20 $21` case, handled by the caller's existing
// post-advance check).
const
  // Continuation tags that can follow a $21/$22 record's payload
  // within the proc-body byte stream. Wider than HandleLocalRecord's
  // set (which only needs $20/$28/$63) because param records can be
  // followed by another param ($21/$22) BEFORE the proc body starts.
  ContTags = [$20, $21, $22, $25, $28, $63, $03];
begin
  if APayloadStart + 6 >= FSz then
  begin
    // Not enough bytes to apply the lookahead; default to 1-byte.
    Result := ByteAt(APayloadStart + 3);
    Exit;
  end;
  var Byte5: Byte := ByteAt(APayloadStart + 5);
  if Byte5 in ContTags then
  begin
    // 1-byte typeId at +3, byte +4 is padding, next record at +5.
    Result := ByteAt(APayloadStart + 3);
    Exit;
  end;
  var Byte6: Byte := ByteAt(APayloadStart + 6);
  if Byte6 in ContTags then
  begin
    // 2-byte typeId at +3..+4, byte +5 is padding, next record at +6.
    Result := UInt32(ByteAt(APayloadStart + 3)) or
              (UInt32(ByteAt(APayloadStart + 4)) shl 8);
    Exit;
  end;
  // Neither position carries a known tag — fall back to the
  // historical 1-byte read so callers that rely on the existing
  // post-advance heuristics (open-array $20 $21 detection) keep
  // working.
  Result := ByteAt(APayloadStart + 3);
end;

function TRsmScanner.HandleParamRecord(var P: NativeInt): Boolean;
// Parameter record: $22 NameLen Name $62 $00 $00 <type-id 2 bytes>.
// Unlike LOCAL_TAG, this record does NOT carry a frame offset:
// open-array parameters (and other parameters in the calling
// convention's register slots) live in CPU registers, not on
// the stack. We capture the parameter so callers see it in the
// Locals list, tagged as register-passed with its zero-based
// register-param index; the higher layer resolves the index to
// the concrete register (RCX/RDX/... on Win64, EAX/EDX/ECX on
// Win32 default register convention).
var
  NameLen: Byte;
  Name   : String;
  Loc    : TRsmLocal;
begin
  Result := False;
  NameLen := ByteAt(P + 1);
  if (NameLen < 1) or (NameLen > 64) then Exit;
  if not ReadIdentifier(P + 1, Name) then Exit;
  Loc := Default(TRsmLocal);
  Loc.Name        := Name;
  Loc.BpOffset    := 0;
  Loc.TypeIdx     := 0;
  Loc.Kind        := lkRegister;
  Loc.RegParamIdx := Byte(FScanRegParam);
  Inc(FScanRegParam);
  var PayloadStart: NativeInt := P + 2 + Length(Name);
  if (PayloadStart + 4 < FSz) and
     (ByteAt(PayloadStart)     = $62) and
     (ByteAt(PayloadStart + 1) = $00) and
     (ByteAt(PayloadStart + 2) = $00) then
  begin
    // Type-id is the LE 16-bit value "<lo> <hi>" when hi is
    // a structured-type marker ($2E for most types, $2F for
    // some Win64 sets); otherwise structural disambiguation by
    // continuation-tag position decides 1-byte vs 2-byte read.
    // 2-byte typeIds with non-$2E/$2F Hi bytes occur for cross-
    // unit RTL types where the linker mints a per-binary alias
    // id whose Hi byte varies (§6.15 — observed: Self at Hi=$04
    // through $13 for various RTL classes, AStatusPriority
    // TThreadPriority at Hi=$06). Without this disambiguator the
    // 1-byte fallback truncates the alias into random foreign
    // type ids.
    var Hi: Byte := ByteAt(PayloadStart + 4);
    if (Hi = $2E) or (Hi = $2F) then
      Loc.TypeIdx := UInt32(ByteAt(PayloadStart + 3)) or
                      (UInt32(Hi) shl 8)
    else
      Loc.TypeIdx := DecodeTypeIdByStructuralLookahead(PayloadStart);
  end;
  FProcs[FProcs.Count - 1].Locals.Add(Loc);
  Inc(FScanLocalIdx);
  FScanSeenLocalSinceProc := True;
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
    Inc(FScanRegParam);
  Result := True;
end;

function TRsmScanner.HandleRegVarRecord(var P: NativeInt): Boolean;
// Register-passed variable: $21 NameLen Name $66 $00 $00 <typeidLo> <typeidHi>.
var
  NameLen: Byte;
  Name   : String;
  Loc    : TRsmLocal;
begin
  Result := False;
  NameLen := ByteAt(P + 1);
  if (NameLen < 1) or (NameLen > 64) then Exit;
  if not ReadIdentifier(P + 1, Name) then Exit;
  Loc := Default(TRsmLocal);
  Loc.Name        := Name;
  Loc.BpOffset    := 0;
  Loc.TypeIdx     := 0;
  Loc.Kind        := lkRegister;
  Loc.RegParamIdx := Byte(FScanRegParam);
  Inc(FScanRegParam);
  var PayloadStart: NativeInt := P + 2 + Length(Name);
  if (PayloadStart + 4 < FSz) and
     (ByteAt(PayloadStart)     = $66) and
     (ByteAt(PayloadStart + 1) = $00) and
     (ByteAt(PayloadStart + 2) = $00) then
  begin
    // Same structural-lookahead disambiguation as $22 PARAM
    // (see comment in HandleParamRecord). §6.15 closure for
    // the most common case: cross-unit RTL types whose alias
    // Hi byte is not $2E/$2F.
    var Hi: Byte := ByteAt(PayloadStart + 4);
    if (Hi = $2E) or (Hi = $2F) then
      Loc.TypeIdx := UInt32(ByteAt(PayloadStart + 3)) or
                      (UInt32(Hi) shl 8)
    else
      Loc.TypeIdx := DecodeTypeIdByStructuralLookahead(PayloadStart);
  end;
  FProcs[FProcs.Count - 1].Locals.Add(Loc);
  Inc(FScanLocalIdx);
  FScanSeenLocalSinceProc := True;
  P := P + 2 + Length(Name);
  if (P + 4 < FSz) and (ByteAt(P) = $66) and
     (ByteAt(P + 1) = $00) and (ByteAt(P + 2) = $00) then
    P := P + 5;
  Result := True;
end;

function TRsmScanner.HandleLocalRecord(var P: NativeInt): Boolean;
// Stack local: $20 NameLen Name <typeinfo + BP-offset payload>.
// Only valid inside a proc scope (gated by the dispatcher).
var
  NameLen: Byte;
  Name   : String;
  Loc    : TRsmLocal;
begin
  Result := False;
  NameLen := ByteAt(P + 1);
  if (NameLen < 1) or (NameLen > 64) then Exit;
  if not ReadIdentifier(P + 1, Name) then Exit;
  Loc := Default(TRsmLocal);
  Loc.Name     := Name;
  Loc.BpOffset := -10000 - (FScanLocalIdx * 4);
  Loc.TypeIdx  := 0;
  Loc.Kind     := lkBpRel;
  var PayloadStart: NativeInt := P + 2 + Length(Name);
  // Inline-declared var form (Delphi "var X := expr;" in a proc body):
  // the body opens with the 4-byte anchor $66 $00 $01 $04 instead of
  // the classic stack-local $66 $00 $00. After the anchor comes a 1- or
  // 2-byte per-proc type ref, then the BPRel offset (single byte
  // ShortInt div 2, or wide LSB-continuation (W-1) div 4), then a
  // terminator: the next record tag OR the $9C $17 trailing-init
  // signature. The type-width is not flagged, so we probe TypeW in
  // {1,2}: accept the first whose offset is a plausible non-positive
  // BPRel value followed by a valid terminator. This rejects the
  // type-hi byte (positive / non-terminated) being read as the offset.
  // See DPT.Rsm.Format.md §4.4 (inline-var form). The classic forms
  // (LocalA/B/C, EdgeCase locals) keep $66 $00 $00 and fall through to
  // the Shape-A/B decoder below.
  if (PayloadStart + 6 < FSz) and
     // Anchor $66 $00 $01 $04 in one 32-bit load (vs four ByteAt calls);
     // the byte values are spelled out so the call doubles as the doc.
     DwordAtEquals(PayloadStart, $66, $00, $01, $04) then
  begin
    for var TypeW := 1 to 2 do
    begin
      var OfsPos: NativeInt := PayloadStart + 4 + TypeW;
      if OfsPos + 1 >= FSz then Continue;
      var OB: Byte := ByteAt(OfsPos);
      var Ofs: Int32;
      var TermPos: NativeInt;
      if (OB and 1) = 0 then
      begin
        Ofs := Int32(ShortInt(OB)) div 2;
        TermPos := OfsPos + 1;
      end
      else
      begin
        var W: Word := Word(OB) or (Word(ByteAt(OfsPos + 1)) shl 8);
        Ofs := (Int32(SmallInt(W)) - 1) div 4;
        TermPos := OfsPos + 2;
      end;
      // A real local sits below EBP -> non-positive offset.
      if Ofs > 0 then Continue;
      if TermPos >= FSz then Continue;
      var TB: Byte := ByteAt(TermPos);
      if not ((TB = TRsmTag.LOCAL_TAG) or (TB = TRsmTag.PROC_TAG) or
              (TB = TRsmTag.SCOPE_END) or (TB = TRsmTag.PARAM_TAG) or
              (TB = TRsmTag.REGVAR_TAG) or (TB = TRsmTag.ENUM_CONST_TAG) or
              (TB = TRsmTag.ENUM_DEF_TAG) or (TB = TRsmTag.TYPE_REGISTRY_TAG) or
              ((TB = $9C) and (TermPos + 1 < FSz) and
               (ByteAt(TermPos + 1) = $17))) then
        Continue;
      Loc.BpOffset := Ofs;
      if TypeW = 1 then
        Loc.TypeIdx := ByteAt(PayloadStart + 4)
      else
        Loc.TypeIdx := UInt32(ByteAt(PayloadStart + 4)) or
                       (UInt32(ByteAt(PayloadStart + 5)) shl 8);
      Break;
    end;
    // If neither combo validated, Loc.BpOffset keeps the synthesized
    // fallback so the canary tests still surface the miss.
  end
  else if PayloadStart + 5 < FSz then
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
      if (Next5 = TRsmTag.LOCAL_TAG) or (Next5 = TRsmTag.PROC_TAG) or
         (Next5 = TRsmTag.SCOPE_END) then
      begin
        var Ofs8: ShortInt := ShortInt(Byte4);
        Loc.BpOffset := Int32(Ofs8) div 2;
        Loc.TypeIdx  := ByteAt(PayloadStart + 3);
      end
      else if PayloadStart + 6 < FSz then
      begin
        var Next6: Byte := ByteAt(PayloadStart + 6);
        if (Next6 = TRsmTag.LOCAL_TAG) or (Next6 = TRsmTag.PROC_TAG) or
           (Next6 = TRsmTag.SCOPE_END) then
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
  Inc(FScanLocalIdx);
  FScanSeenLocalSinceProc := True;
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
    // When the body opens with the distinctive $66 $00 $00 anchor
    // (a real module-global record the dispatcher mis-routed to us
    // because FScanInProc was still True), the 4-byte VA slot at
    // +5..+8 carries (Value shl 4) | $07 -- see §4.4/§4.5 of
    // DPT.Rsm.Format.md. Decode it only when the anchor + low-
    // nibble tag both line up; stack-locals never carry this pair.
    if (P + 2 + Integer(NameLen) + 8 < FSz) and
       (ByteAt(P + 2 + NameLen)     = $66) and
       (ByteAt(P + 2 + NameLen + 1) = $00) and
       (ByteAt(P + 2 + NameLen + 2) = $00) then
    begin
      var VaByte0: Byte := ByteAt(P + 2 + NameLen + 5);
      if (VaByte0 and $0F) = $07 then
      begin
        var VaDword: UInt32 := DwordAt(P + 2 + NameLen + 5);
        FGlobalVa[LowerCase(Name)] := VaDword shr 4;
      end;
    end;
  end;
  P := P + 2 + Length(Name);
  Result := True;
end;

function TRsmScanner.HandleModuleGlobalLocalTagRecord(var P: NativeInt): Boolean;
// Module-level global variable emitted with the LOCAL_TAG ($20)
// outside any proc scope. The compiler reuses $20 for both stack
// locals and module-level variables; the dispatcher's FScanInProc
// gate picks the right handler.
var
  NameLen: Byte;
  Name   : String;
begin
  Result := False;
  NameLen := ByteAt(P + 1);
  if (NameLen < 1) or (NameLen > 40) then Exit;
  if P + 2 + Integer(NameLen) + 5 >= FSz then Exit;
  if (ByteAt(P + 2 + NameLen + 1) <> $00) or
     (ByteAt(P + 2 + NameLen + 2) <> $00) then Exit;
  if not ReadIdentifier(P + 1, Name) then Exit;
  var Lo: Byte := ByteAt(P + 2 + NameLen + 3);
  var Hi: Byte := ByteAt(P + 2 + NameLen + 4);
  FGlobalByName[LowerCase(Name)] := UInt32(Lo) or (UInt32(Hi) shl 8);
  FGlobalFileOffset[LowerCase(Name)] := P;
  // Module-global $20 form carries the same 4-byte (Value shl 4) | $07
  // VA slot the $27 GLOBAL_PRIM record uses; the slot sits right
  // after the 2-byte type id (NameEnd+5..+8). Decode only when the
  // low nibble matches the expected $07 tag.
  if P + 2 + Integer(NameLen) + 8 < FSz then
  begin
    var VaByte0: Byte := ByteAt(P + 2 + NameLen + 5);
    if (VaByte0 and $0F) = $07 then
    begin
      var VaDword: UInt32 := DwordAt(P + 2 + NameLen + 5);
      FGlobalVa[LowerCase(Name)] := VaDword shr 4;
    end;
  end;
  P := P + 2 + Length(Name) + 5;
  Result := True;
end;

function TRsmScanner.HandleGlobalPrimRecord(var P: NativeInt): Boolean;
// Top-level primitive global: $27 NL Name $66 $00 $00 <id> <4-byte VA>.
//
// We deliberately do NOT gate this branch on `not InProc`. The $27
// tag is reserved for top-level primitive globals -- it never
// appears as an in-proc local record, so the InProc state is not a
// valid filter here. Gating on InProc breaks early-region globals
// (GGlobalInt / GGlobalString in DebugTarget) when an earlier
// proc record opens InProc but emits no LOCAL/PARAM/REGVAR record
// to flip SeenLocalSinceProc -- in that case SCOPE_END can never
// close the scope, and every subsequent $27 record is silently
// skipped. The $27 + $66 + $00 $00 + ReadIdentifier validation
// chain is strong enough to anchor the decode without the InProc
// guard.
var
  NameLen: Byte;
  Name   : String;
begin
  Result := False;
  NameLen := ByteAt(P + 1);
  if (NameLen < 1) or (NameLen > 40) then Exit;
  if P + 2 + Integer(NameLen) + 4 >= FSz then Exit;
  if (ByteAt(P + 2 + NameLen)     <> $66) or
     (ByteAt(P + 2 + NameLen + 1) <> $00) or
     (ByteAt(P + 2 + NameLen + 2) <> $00) then Exit;
  if not ReadIdentifier(P + 1, Name) then Exit;
  // Two-byte vs single-byte type id. Structured-typed globals
  // carry a 2-byte id where the hi byte is a "kind" marker:
  //   $2E -- program-local enum primary (e.g. $2E75 = TLightStatus)
  //   $2F -- some Win64 set types
  //   $1E -- scope-local enum alias for same-compilation cross-
  //          unit enums (e.g. TStatus from sibling units). The
  //          lo byte is sequentially allocated per scope, and
  //          the bridge from this alias to the enum's primary
  //          lives in the $03 ENUM_DEF + $63 $65 UNIT_BRIDGE
  //          record pair, not in this $27 record.
  // Plain primitives have a single byte ($02 = Integer,
  // $04 = string, ...) with whatever byte follows as the start
  // of the 4-byte VA.
  var PrimId: UInt32;
  var HiByte: Byte := ByteAt(P + 2 + NameLen + 4);
  if (HiByte = $2E) or (HiByte = $2F) or (HiByte = $1E) then
    PrimId := UInt32(ByteAt(P + 2 + NameLen + 3)) or
              (UInt32(HiByte) shl 8)
  else
    PrimId := ByteAt(P + 2 + NameLen + 3);
  FGlobalByName[LowerCase(Name)] := PrimId;
  FGlobalFileOffset[LowerCase(Name)] := P;
  // 4-byte VA slot: starts at the byte after the type id (offset +4
  // for 1-byte ids, +5 for 2-byte structured ids). Encoded as
  // (Value shl 4) or $07; decode only when the low nibble matches.
  var VaOffset: Integer;
  if (HiByte = $2E) or (HiByte = $2F) or (HiByte = $1E) then
    VaOffset := 5
  else
    VaOffset := 4;
  if P + 2 + Integer(NameLen) + VaOffset + 3 < FSz then
  begin
    var VaByte0: Byte := ByteAt(P + 2 + NameLen + VaOffset);
    if (VaByte0 and $0F) = $07 then
    begin
      var VaDword: UInt32 := DwordAt(P + 2 + NameLen + VaOffset);
      FGlobalVa[LowerCase(Name)] := VaDword shr 4;
    end;
  end;
  P := P + 2 + Length(Name) + 4;
  Result := True;
end;

function TRsmScanner.HandleEnumConstantRecord(var P: NativeInt): Boolean;
// Enum-constant record ($25). Works inside or outside a proc scope.
// Two top-level forms (program-local and cross-unit); the cross-
// unit form further splits into three body variants chosen by the
// payload's typeId-hi byte and the LSB of the ordinal byte.
var
  NameLen: Byte;
  Name   : String;
begin
  Result := False;
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
    FEnumDecoder.RecordProgramLocalConstant(EnumTypeId, Ordinal, Name);
    P := P + 2 + Length(Name) + 8;
    Exit(True);
  end;
  // Cross-unit form -- three body variants depending on whether
  // the enum's primary id is "real cross-unit" (a separately-
  // compiled RTL/VCL type like TThreadPriority with primary
  // 0x0441) or "same-compilation cross-unit" (a user type
  // declared in a sibling source file, e.g. TStatus from
  // DebugTarget.EnumAlpha) -- and, within the latter family,
  // whether the element's explicit ordinal value fits in a
  // single byte or needs Delphi's LSB-discriminated 2-byte
  // encoding (Delphi enums may carry arbitrary explicit ordinal
  // assignments, e.g. <c>(a = 1, b = 100)</c>).
  //
  // RTL 12-byte body (e.g. TThreadPriority):
  //   $25 NL Name $8A $00 $00 <4-RVA> <typeId-lo> <typeId-hi>
  //                 $00 $00 <2*ord>
  //   typeId-hi != $00 (real cross-unit id, e.g. $04 for TThreadPriority).
  //
  // Same-compilation 11-byte body (single-byte ord, ord <= 127):
  //   $25 NL Name $8A $00 $00 <4-RVA> <typeId-lo> $00 $00 <2*ord>
  //   typeId-hi = $00 (small shared secondary, e.g. 0x0002).
  //
  // Same-compilation 12-byte body (2-byte ord, sparse / ord >= 128):
  //   $25 NL Name $8A $00 $00 <4-RVA> <typeId-lo> $00 $00 <ord-byte-lo> <ord-byte-hi>
  //   typeId-hi = $00 AND ord-byte-lo's LSB = 1. The ordinal is
  //   decoded as <c>(W - 1) / 4</c> where W is the LE word at
  //   +10..+11 -- the same LSB-as-continuation encoding the
  //   BPRel-offset decoder uses for stack offsets > +/-64.
  //
  // Discriminator order: typeId-hi first picks RTL vs same-comp;
  // then byte[+10]'s LSB picks single- vs 2-byte ord within
  // same-comp.
  if (NameLen >= 1) and (NameLen <= 64) and
     (P + 2 + Integer(NameLen) + 11 < FSz) and
     (ByteAt(P + 2 + NameLen)     = $8A) and
     (ByteAt(P + 2 + NameLen + 1) = $00) and
     (ByteAt(P + 2 + NameLen + 2) = $00) and
     (ByteAt(P + 2 + NameLen + 9) = $00) and
     ReadIdentifier(P + 1, Name) then
  begin
    var EnumTypeId: UInt32 :=
      UInt32(ByteAt(P + 2 + NameLen + 7)) or
      (UInt32(ByteAt(P + 2 + NameLen + 8)) shl 8);
    var HiByte: Byte := ByteAt(P + 2 + NameLen + 8);
    var Ordinal: Integer;
    var BodyLen: Integer;
    if HiByte <> 0 then
    begin
      // RTL 12-byte form: requires +10 = $00 (double-pad) and
      // single-byte 2*ord at +11.
      if (P + 2 + Integer(NameLen) + 12 >= FSz) or
         (ByteAt(P + 2 + NameLen + 10) <> $00) then
        Exit(False);
      Ordinal := Integer(ByteAt(P + 2 + NameLen + 11)) shr 1;
      BodyLen := 12;
    end
    else
    begin
      var OrdByte0: Byte := ByteAt(P + 2 + NameLen + 10);
      if (OrdByte0 and 1) = 0 then
      begin
        // Same-comp 11-byte form: single-byte 2*ord at +10.
        Ordinal := Integer(OrdByte0) shr 1;
        BodyLen := 11;
      end
      else
      begin
        // Same-comp 12-byte sparse form: 2-byte LSB-extended ord
        // at +10..+11. Decoded as (W - 1) >> 2 per Delphi's
        // LSB-as-continuation scheme.
        if P + 2 + Integer(NameLen) + 12 >= FSz then
          Exit(False);
        var W: Word := Word(OrdByte0) or
                       (Word(ByteAt(P + 2 + NameLen + 11)) shl 8);
        Ordinal := (Integer(W) - 1) shr 2;
        BodyLen := 12;
      end;
    end;
    if EnumTypeId <> 0 then
    begin
      // For the RTL 12-byte form, the typeId IS a unique primary
      // -- register it directly. For the same-compilation forms,
      // the typeId is the shared secondary (typically 0x0002
      // across sibling-unit enums); BUFFER the constant rather
      // than writing it under the secondary, so the upcoming
      // $2A registry entry can flush it under its TRUE primary
      // and avoid the last-wins collision on the secondary.
      if HiByte <> 0 then
        FEnumDecoder.RecordCrossUnitRtlConstant(EnumTypeId, Ordinal, Name)
      else
        FEnumDecoder.RecordCrossUnitSameCompConstant(EnumTypeId, Ordinal, Name);
    end;
    P := P + 2 + Length(Name) + BodyLen;
    Exit(True);
  end;
end;

procedure TRsmScanner.HandleEnumDefRecord(P: NativeInt);
// Enum type-definition record ($03): the AUTHORITATIVE source for
// a (unit, type) pair's element list in declaration order.
//   $03 NL TypeName $01 $00 $00 $00 $00 <max-ord> $00 $00 $00 $00 $00 $00 $00
//     (<elem-len> <elem-name>) * (max-ord + 1)
//     <unit-len> <unit-name>
// Two units declaring the same type name produce TWO independent
// $03 records; the (unit, type) pair disambiguates same-name
// cross-unit enums that the $2A registry collapses last-wins.
//
// Doesn't try to advance the dispatcher's P past the unit name --
// the record's trailer varies. Single-byte advance is fine; the
// outer loop's $03 dispatch will skip the inner bytes harmlessly.
var
  NameLen: Byte;
  Name   : String;
begin
  NameLen := ByteAt(P + 1);
  if (NameLen < 2) or (NameLen > 40) then Exit;
  if P + 2 + Integer(NameLen) + 13 >= FSz then Exit;
  if (ByteAt(P + 2 + NameLen)      <> $01) or
     (ByteAt(P + 2 + NameLen + 1)  <> $00) or
     (ByteAt(P + 2 + NameLen + 2)  <> $00) or
     (ByteAt(P + 2 + NameLen + 3)  <> $00) or
     (ByteAt(P + 2 + NameLen + 4)  <> $00) or
     (ByteAt(P + 2 + NameLen + 6)  <> $00) or
     (ByteAt(P + 2 + NameLen + 7)  <> $00) or
     (ByteAt(P + 2 + NameLen + 8)  <> $00) or
     (ByteAt(P + 2 + NameLen + 9)  <> $00) or
     (ByteAt(P + 2 + NameLen + 10) <> $00) or
     (ByteAt(P + 2 + NameLen + 11) <> $00) or
     (ByteAt(P + 2 + NameLen + 12) <> $00) then Exit;
  if not ReadIdentifier(P + 1, Name) then Exit;
  // Number of elements = max-ord + 1 (max-ord at payload byte
  // +5 relative to the post-name position).
  var MaxOrd: Integer := ByteAt(P + 2 + NameLen + 5);
  var ElemCount: Integer := MaxOrd + 1;
  // Sanity cap. Real enums rarely exceed ~256 elements; cap
  // generously to bail on a coincidental $03 byte hit.
  if (ElemCount < 1) or (ElemCount > 512) then Exit;
  // The element list follows a VARIABLE-LENGTH zero padding after the
  // `$01 $00 $00 $00 $00 <MaxOrd>` 6-byte prefix. Different Delphi
  // toolchains emit different pad widths: DebugTarget's build emits 7
  // pad bytes (first element at +13), but DPT.exe's build emits 11
  // (first element at +17). Hard-coding +13 made HandleEnumDefRecord
  // read a $00 ElemLen and bail on DPT.rsm, so NONE of its $03 records
  // parsed -- every enum fell through to the lossy $25/$2A synthesis
  // (§6.25). Skip the zero run to the first real ElemLen instead of
  // assuming a fixed header size. The first element's length byte is
  // always in [1, 64] and never $00, so a zero is unambiguously padding.
  var CursorPos: NativeInt := P + 2 + NameLen + 6;
  var PadStop: NativeInt := CursorPos + 32;
  if PadStop > FSz then PadStop := FSz;
  while (CursorPos < PadStop) and (ByteAt(CursorPos) = $00) do
    Inc(CursorPos);
  var Elements: IList<TRsmEnumElement> :=
    Collections.NewPlainList<TRsmEnumElement>;
  for var EI: Integer := 0 to ElemCount - 1 do
  begin
    if CursorPos >= FSz then Exit;
    var ElemLen: Byte := ByteAt(CursorPos);
    if (ElemLen < 1) or (ElemLen > 64) or
       (CursorPos + 1 + ElemLen > FSz) then Exit;
    var ElemName: String;
    if not ReadIdentifier(CursorPos, ElemName) then Exit;
    // Ordinal defaults to list index. The $03 record format is
    // only emitted by the linker for contiguous 0..N-1 enums --
    // sparse / explicit-value enums (<c>type T = (a = 1, b = 5)</c>)
    // skip the $03 channel entirely (see DebugTarget.dpr's
    // TSparseEnum + Test.DPT.Rsm.Scanner.
    // TestSparseEnumResolvesViaEnumConstNames32, plus §4.7/§6.1
    // of DPT.Rsm.Format.md). For sparse enums the per-element
    // ordinals come through the $25 channel and EnumDefs simply
    // doesn't list them.
    var Elem: TRsmEnumElement;
    Elem.Name    := ElemName;
    Elem.Ordinal := EI;
    Elements.Add(Elem);
    CursorPos := CursorPos + 1 + ElemLen;
  end;
  // Unit name comes right after the element list. Optional in
  // theory but in practice always present for user enums.
  var UnitName: String := '';
  if CursorPos < FSz then
  begin
    var UnitLen: Byte := ByteAt(CursorPos);
    if (UnitLen >= 1) and (UnitLen <= 64) and
       (CursorPos + 1 + UnitLen <= FSz) then
      ReadIdentifier(CursorPos, UnitName);
  end;
  var Def: TRsmEnumDef;
  Def.TypeName := Name;
  Def.UnitName := UnitName;
  Def.Elements := Elements;
  Def.Synthesized := False;  // authoritative $03 ENUM_DEF source
  FEnumDecoder.RecordEnumDef(Def);
end;

procedure TRsmScanner.HandleTypeRegistryRecord(P: NativeInt);
// Type-registry entry ($2A NL Name <body-flag> $00 $00 <primary-id>
// [<payload>]). The body-flag byte at +0 selects the body shape (NOT
// the type kind -- see Test.DPT.Rsm.Scanner.
// Test2ATypeRegistryFlagIsBodyShapeNotKind32 and §4.8 of
// DPT.Rsm.Format.md). Parses the header + secondary candidate,
// recovers the owning unit name via a forward-scan when
// same-compilation $25 constants are pending, then hands the parsed
// data to FEnumDecoder which performs the primary/secondary bridge,
// pending-buffer flush, and EnumDef synthesis.
//
// Doesn't advance the dispatcher's P -- the outer loop's
// single-byte advance is sufficient; subsequent bytes are
// re-dispatched harmlessly.
var
  NameLen     : Byte;
  Name        : String;
  Primary     : UInt32;
  SecCandidate: UInt32;
  UnitNameSparse: String;
begin
  NameLen := ByteAt(P + 1);
  if (NameLen < 2) or (NameLen > 40) then Exit;
  if P + 2 + Integer(NameLen) + 5 >= FSz then Exit;
  if (ByteAt(P + 2 + NameLen + 1) <> $00) or
     (ByteAt(P + 2 + NameLen + 2) <> $00) then Exit;
  if not ReadIdentifier(P + 1, Name) then Exit;
  Primary :=
    UInt32(ByteAt(P + 2 + NameLen + 3)) or
    (UInt32(ByteAt(P + 2 + NameLen + 4)) shl 8);
  // Only the +7,+8 slot has been observed to consistently
  // hold the secondary id in genuine enum $2A entries.
  SecCandidate := 0;
  if P + 2 + NameLen + 8 < FSz then
    SecCandidate :=
      UInt32(ByteAt(P + 2 + NameLen + 7)) or
      (UInt32(ByteAt(P + 2 + NameLen + 8)) shl 8);
  // Locate the OWNING UNIT via forward-scan only when same-comp
  // $25 constants are actually pending; otherwise the unit name
  // has no consumer and the scan would be wasted work.
  //
  // The unit-init proc carries the owning unit name (a dotted
  // NAMESPACE, e.g. "DPT.Dcu.Diff", "System.Variants"), but it sits at
  // the END of the unit's symbol block -- after the unit's method PROC
  // records. So the FIRST dotted proc after the $2A is usually a class
  // METHOD ("TDcuDiff.ListEntries"), NOT the unit. Grabbing that method
  // as the unit name was the RsmDesk "UnitName: TDcuDiff.ListEntries"
  // defect (§6.25).
  //
  // The unit-init proc is the first dotted proc that is a clean dotted
  // NAMESPACE -- distinguished from the noise between it and the $2A by
  // two rules (verified against DPT.rsm):
  //   * first segment is NOT a Delphi type identifier (T-class,
  //     E-exception or I-interface followed by an uppercase letter),
  //     which rejects method names like "TDcuDiff.ListEntries";
  //   * the name carries no generic angle brackets, which rejects
  //     specialization procs like "Collections.NewList<System.string>".
  // The current unit's methods/specializations all precede its init
  // proc, so the FIRST clean namespace encountered IS the owning unit
  // (no overshoot into the next unit). The window is generous because a
  // large class pushes the init proc far out (TMcpServer ~21 KB). If no
  // clean namespace turns up we fall back to the first dotted proc so
  // the enum is NEVER dropped -- an earlier attempt that dropped the
  // def when no clean unit name was found collapsed DPT.rsm 95 -> 22.
  UnitNameSparse := '';
  if (Primary <> 0) and FEnumDecoder.HasPendingConstants then
  begin
    var Start: NativeInt := P + 2 + NameLen + 5;

    // SYNTHESIS GATE (tight, 1 KB). Synthesise an EnumDef only when a
    // dotted proc sits within 1 KB of the $2A. This is the original
    // gate and it must stay tight: widening it lets many non-enum $2A
    // entries (whose nearest proc is far) flush their pending const
    // pile into a bogus EnumDef -- a 1 KB->64 KB widening alone took
    // DebugTarget 132 -> 166 synthesised defs. The gate is decoupled
    // from the name search below precisely so name quality can improve
    // without changing WHICH entries synthesise.
    var GateName: String := '';
    var Qg: NativeInt := Start;
    var GateStop: NativeInt := Start + 1024;
    if GateStop > FSz - 2 then GateStop := FSz - 2;
    while Qg < GateStop do
    begin
      if ByteAt(Qg) = TRsmTag.PROC_TAG then
      begin
        var GNL: Byte := ByteAt(Qg + 1);
        if (GNL >= 2) and (GNL <= 64) and (Qg + 2 + GNL <= FSz) then
        begin
          var GName: String;
          if ReadIdentifier(Qg + 1, GName) and GName.Contains('.') then
          begin
            GateName := GName;
            Break;
          end;
        end;
      end;
      Inc(Qg);
    end;

    if GateName <> '' then
    begin
      // NAME SEARCH (wide, 64 KB). The unit-init proc carries the owning
      // unit name (a dotted NAMESPACE, e.g. "DPT.Dcu.Diff",
      // "System.Variants") but sits at the END of the unit's symbol
      // block -- after all its method PROC records. The nearby GateName
      // is therefore usually a class METHOD ("TDcuDiff.ListEntries"),
      // which surfaced in RsmDesk as a wrong UnitName (§6.25). Search
      // wider for the first dotted proc that is a clean dotted NAMESPACE:
      //   * first segment is NOT a Delphi type identifier (T-class,
      //     E-exception, I-interface + uppercase) -- rejects methods;
      //   * no generic angle brackets -- rejects specialization procs
      //     like "Collections.NewList<System.string>".
      // The unit's own methods/specializations all precede its init
      // proc, so the FIRST clean namespace IS the owning unit (no
      // overshoot). A big class pushes the init proc far out (TMcpServer
      // ~21 KB), hence the wide window. Fall back to GateName (the
      // nearby method) so the enum is never dropped.
      UnitNameSparse := GateName;
      var Qn: NativeInt := Start;
      var NameStop: NativeInt := Start + 1048576;
      if NameStop > FSz - 2 then NameStop := FSz - 2;
      while Qn < NameStop do
      begin
        if ByteAt(Qn) = TRsmTag.PROC_TAG then
        begin
          var NNL: Byte := ByteAt(Qn + 1);
          if (NNL >= 2) and (NNL <= 64) and (Qn + 2 + NNL <= FSz) then
          begin
            var NName: String;
            if ReadIdentifier(Qn + 1, NName) and NName.Contains('.') then
            begin
              var DotI: Integer := Pos('.', NName);
              var Seg : String  := Copy(NName, 1, DotI - 1);
              var IsTypeIdent: Boolean := (Length(Seg) >= 2) and
                CharInSet(Seg[1], ['T', 'E', 'I']) and
                CharInSet(Seg[2], ['A'..'Z']);
              if (not IsTypeIdent) and (not NName.Contains('<')) then
              begin
                UnitNameSparse := NName;
                Break;
              end;
            end;
          end;
        end;
        Inc(Qn);
      end;
    end;
  end;
  // Pass P (the $2A tag's file offset) so the EnumDecoder can
  // record it alongside the primary id for the §6.9 nearest-offset
  // bridge. The cast to UInt32 truncates on the (currently
  // hypothetical) >4 GB-buffer case; RSM sidecars even on TFW-class
  // binaries land at ~1 GB, so the upper bits are unused.
  FEnumDecoder.RecordTypeRegistry(Primary, SecCandidate, Name,
    UnitNameSparse, UInt32(P));
end;

function TRsmScanner.HandleUnitUseIntroRecord(var P: NativeInt): Boolean;
// §6.21 cross-unit symbol-import segment introducer. Layout:
//
//   $64 <NL: u8> <UnitName> $00 $00 $00
//   (
//     $66 <NL: u8> <TypeName>   <4-byte LE RVA>
//     $67 <NL: u8> <SymbolName> <4-byte LE RVA>
//     $70 <NL: u8> <FileName>   <4-byte LE RVA>
//   )*
//   $63                                          <- SCOPE_END (optional)
//
// Structural anchor: the three trailing zero bytes immediately after
// the unit name. Without them an incidental $64 byte inside another
// record's payload would fire here, so we reject the open and fall
// through to the dispatcher's single-byte advance.
//
// Each $66/$67/$70 entry's payload is a 4-byte little-endian RVA
// into the canonical declaration's slot (matches the $2A entry body
// bytes +3..+6 for $66, and the per-element +3 stride observed for
// $67 enum-element siblings).
//
// Bail conditions inside the segment walk:
//   * Hit $63 SCOPE_END -- normal close; consume the byte.
//   * Hit a tag outside {$66, $67, $70, $63} -- segment ended without
//     an explicit close (the next thing on the wire is some other
//     record); leave P pointing at that byte, the outer dispatcher
//     reroutes.
//   * Inner entry's name length is out of [1..64] or name fails the
//     printable-ASCII validator -- treat as the segment ending here
//     (the same gracefulness the rest of the scanner has).
//   * Payload all-zero ($00 $00 $00 $00) -- reject, to dodge false
//     positives where a long zero run mimics the shape.
//
// Perf note: $64 is a VERY common byte value in real .rsm files
// (TFW.rsm has tens of thousands of incidental $64 bytes inside
// proc-address payloads, type-id bytes, ordinal slots, etc.). The
// fast-reject path therefore uses direct PByte arithmetic instead
// of going through ByteAt's call indirection, and bails as soon as
// the length byte or the trailing-zero anchor disagrees. Only the
// genuine anchors (a few thousand in TFW) go through the heavier
// ReadIdentifier validator and the entry walk.
const
  REF_TYPE   = TRsmTag.UNIT_USE_TYPE;
  REF_SYMBOL = TRsmTag.UNIT_USE_SYMBOL;
  REF_FILE   = TRsmTag.UNIT_USE_FILE;
var
  NameLen: Byte;
  UnitName: String;
  Q       : NativeInt;
  Seg     : TRsmUnitUseSegment;
  Tag     : Byte;
  EntryLen: Byte;
  EntryName: String;
  EntryPayloadOff: NativeInt;
  Rva     : UInt32;
  Ref     : TRsmUnitUseRef;
  AnchorPtr: PByte;
begin
  Result := False;
  if P + 5 >= FSz then Exit;
  // Direct buffer access for the fast-reject path: 4 byte reads + a
  // length-byte range check, all without method-call indirection.
  NameLen := (FBuf + P + 1)^;
  if (NameLen < 1) or (NameLen > 64) then Exit;
  if P + 2 + Integer(NameLen) + 3 > FSz then Exit;
  // The trailing three zero bytes are the structural anchor.
  AnchorPtr := FBuf + P + 2 + NameLen;
  if (AnchorPtr[0] <> $00) or (AnchorPtr[1] <> $00) or
     (AnchorPtr[2] <> $00) then Exit;
  // Past the fast-reject gate: validate the name with the standard
  // printable-ASCII walker.
  if not ReadIdentifier(P + 1, UnitName) then Exit;

  Seg.UnitName      := UnitName;
  Seg.StartOffset   := NativeUInt(P);
  // §4.17: attribute this segment to the source-file (`$70`) record
  // that most recently introduced the surrounding `$64` run. -1 when
  // no `$70` preceded it (no orphans observed in practice -- see §4.17).
  Seg.SourceFileIdx := FCurrentSourceFileIdx;
  Seg.Refs          := Collections.NewPlainList<TRsmUnitUseRef>;

  Q := P + 2 + Integer(NameLen) + 3;
  while Q + 1 < FSz do
  begin
    Tag := ByteAt(Q);
    if Tag = TRsmTag.SCOPE_END then
    begin
      // Consume the SCOPE_END and stop.
      Inc(Q);
      Break;
    end;
    if (Tag <> REF_TYPE) and (Tag <> REF_SYMBOL) and (Tag <> REF_FILE) then
      Break;
    EntryLen := ByteAt(Q + 1);
    if (EntryLen < 1) or (EntryLen > 64) then Break;
    EntryPayloadOff := Q + 2 + Integer(EntryLen);
    if EntryPayloadOff + 4 > FSz then Break;
    if not ReadIdentifier(Q + 1, EntryName) then Break;
    // Reject all-zero payload as a structural anchor for "this byte
    // run isn't an entry, the segment ended one tag ago".
    if DwordAtEquals(EntryPayloadOff, 0, 0, 0, 0) then
      Break;
    Rva := UInt32(ByteAt(EntryPayloadOff))            or
           (UInt32(ByteAt(EntryPayloadOff + 1)) shl 8)  or
           (UInt32(ByteAt(EntryPayloadOff + 2)) shl 16) or
           (UInt32(ByteAt(EntryPayloadOff + 3)) shl 24);
    Ref.Name := EntryName;
    Ref.Rva  := Rva;
    case Tag of
      REF_TYPE  : Ref.Kind := uukType;
      REF_SYMBOL: Ref.Kind := uukSymbol;
    else
      Ref.Kind := uukFile;
    end;
    Seg.Refs.Add(Ref);
    Q := EntryPayloadOff + 4;
  end;

  FUnitUseSegments.Add(Seg);
  P := Q;
  Result := True;
end;

function SourceFileToUnitName(const AFile: String): String;
// §4.17 / §4.18: derive the Delphi unit name from a $70 source-file
// name by stripping the directory prefix and the source extension.
// The program/package main file arrives as a FULL path with a .dpr /
// .dpk extension, so the directory strip is what turns
// C:\...\DebugTarget.dpr into DebugTarget. Dotted unit names
// (System.SysConst.pas -> System.SysConst) survive because only the
// exact trailing extension is removed, never the last dot.
var
  D: Integer;
begin
  Result := AFile;
  D := LastDelimiter('\/:', Result);
  if D > 0 then
    Result := Copy(Result, D + 1, MaxInt);
  if Length(Result) >= 4 then
  begin
    var Ext: String := LowerCase(Copy(Result, Length(Result) - 3, 4));
    if (Ext = '.pas') or (Ext = '.inc') or
       (Ext = '.dpr') or (Ext = '.dpk') then
      SetLength(Result, Length(Result) - 4);
  end;
end;

function TRsmScanner.HandleSourceFileIntroRecord(var P: NativeInt): Boolean;
// §4.17 / §4.18: a standalone $70 <SourceFile> <RVA:4> $00 record that
// INTRODUCES the run of records belonging to the unit whose source
// file this is. Shape + tight anchor:
//
//   $70 <NL> <SourceFile .pas/.inc/.dpr/.dpk> <RVA:4 LE> $00 (\$64|\$65)...
//                                                            ^ next opener
//
// Two introducer flavours share this shape:
//   * an IMPORTED unit's uses-block, followed by $00 $64 (the $64
//     import-segment opener) — the §4.17 case;
//   * the PROGRAM / package main file, carried as a FULL path with a
//     .dpr / .dpk extension and followed by $00 $65 (the used-unit
//     list opener) — the §4.18 case that anchors the program module's
//     procs to the program unit. (The former §6.28 rejected this and
//     so froze the cursor on the last imported .pas, mis-attributing
//     every program proc.)
//
// The trailing $00 then a $64/$65 byte is what distinguishes an
// introducer from (a) an incidental $70 byte and (b) the lone
// System.pas record (System imports nothing, so neither follows --
// left to the single-byte fallback; it needs no attribution). On a
// hit we record/dedup the source file, set FCurrentSourceFileIdx, and
// advance
// P to the $64 so the dispatcher hands it to HandleUnitUseIntroRecord,
// which stamps the segment's SourceFileIdx.
var
  NameLen   : Byte;
  FileName  : String;
  PayloadOff: NativeInt;
  Rva       : UInt32;
  UnitName  : String;
  LowerKey  : String;
  Idx       : Integer;
  SF        : TRsmSourceFile;
  ExtLower  : String;

  // A source-file name may be a bare basename (an imported unit's
  // .pas/.inc, all identifier chars) OR a FULL path (the program /
  // package main file's .dpr/.dpk), which additionally contains
  // path separators, a drive colon, and spaces (e.g.
  // C:\Program Files (x86)\...). The global identifier validator
  // rejects those, so read the name here with a path-aware charset.
  // Kept LOCAL to this handler so the identifier alphabet used by
  // every other record stays unchanged.
  function ReadSourceFileName(ALen: Byte; out AName: String): Boolean;
  var
    I: Integer;
    B: Byte;
  begin
    Result := False;
    AName := '';
    for I := 0 to ALen - 1 do
    begin
      B := ByteAt(P + 2 + I);
      if RsmIsPrintableAscii(B) or (B = Ord('\')) or (B = Ord('/')) or
         (B = Ord(':')) or (B = Ord(' ')) or (B = Ord('(')) or
         (B = Ord(')')) or (B = Ord('-')) then
        AName := AName + Chr(B)
      else
        Exit;
    end;
    Result := AName <> '';
  end;

begin
  Result := False;
  NameLen := ByteAt(P + 1);
  if (NameLen < 1) or (NameLen > 64) then Exit;
  PayloadOff := P + 2 + Integer(NameLen);     // 4-byte RVA starts here
  // Need RVA(4) + trailer(1) + the introducing $64/$65 (1) in range.
  if PayloadOff + 5 >= FSz then Exit;
  if ByteAt(PayloadOff + 4) <> $00 then Exit;
  if not ReadSourceFileName(NameLen, FileName) then Exit;
  // Tight extension gate: only a real Delphi source file introduces a
  // uses-block. .pas/.inc cover units/includes; .dpr/.dpk cover the
  // program / package main file (carried as a full path).
  if Length(FileName) < 5 then Exit;
  ExtLower := LowerCase(Copy(FileName, Length(FileName) - 3, 4));
  // The introducer-follower byte (at PayloadOff+5) distinguishes the
  // two flavours and is gated PER EXTENSION so the widening is
  // surgical:
  //   * an IMPORTED unit's .pas/.inc uses-block is followed by a $64
  //     import-segment opener (UNIT_USE_INTRO) -- the §4.17 case. The
  //     lone System.pas (System imports nothing) is followed by $65,
  //     NOT $64, so it stays UN-recorded exactly as §4.17 documents.
  //   * the PROGRAM / package main file (.dpr/.dpk, full path) is
  //     followed by a $65 used-unit list -- the §4.18 case that anchors
  //     the program module's procs to the program unit. (The former
  //     §6.28 rejected this and so froze the cursor on the last
  //     imported .pas.)
  if (ExtLower = '.pas') or (ExtLower = '.inc') then
  begin
    if ByteAt(PayloadOff + 5) <> TRsmTag.UNIT_USE_INTRO then Exit;
  end
  else if (ExtLower = '.dpr') or (ExtLower = '.dpk') then
  begin
    if ByteAt(PayloadOff + 5) <> TRsmTag.USED_UNIT_LIST then Exit;
  end
  else
    Exit;

  Rva := UInt32(ByteAt(PayloadOff))             or
         (UInt32(ByteAt(PayloadOff + 1)) shl 8)  or
         (UInt32(ByteAt(PayloadOff + 2)) shl 16) or
         (UInt32(ByteAt(PayloadOff + 3)) shl 24);
  UnitName := SourceFileToUnitName(FileName);
  LowerKey := LowerCase(UnitName);
  if not FSourceFileByName.TryGetValue(LowerKey, Idx) then
  begin
    SF.SourceFile  := FileName;
    SF.UnitName    := UnitName;
    SF.StartOffset := NativeUInt(P);
    SF.Rva         := Rva;
    FSourceFiles.Add(SF);
    Idx := FSourceFiles.Count - 1;
    FSourceFileByName[LowerKey] := Idx;
  end;
  FCurrentSourceFileIdx := Idx;
  // Advance to the introducing $64; the dispatcher handles it next
  // (Continue, no Inc) and stamps the segment's SourceFileIdx.
  P := PayloadOff + 5;
  Result := True;
end;

procedure TRsmScanner.ScanSymbolStream;
// Walk the RSM symbol stream and dispatch each record to its
// per-tag handler. Handlers update P themselves when they
// successfully consume a record and return True; on rejection
// they leave P untouched and return False, so the outer loop
// falls through to the single-byte advance below. Tag-byte
// definitions live in <see cref="TRsmTag"/> (in DPT.Rsm.Model);
// locally aliased here so the case-labels stay readable.
//
// FScanSeenLocalSinceProc guards SCOPE_END from closing the
// proc scope while we're still inside the proc's address-
// payload bytes. The $A0 sub-form's payload runs ~18 bytes of
// essentially arbitrary data (timestamp/type-ref/encoded VA/
// trailer) and on large binaries routinely contains a $63 byte.
// Without this guard, an incidental $63 would fire SCOPE_END
// BEFORE the first real param/local record had been read -- on
// TFW every class-method's Self / params silently vanished. The
// flag flips True only once we've actually parsed a local-
// shaped record ($20 / $21 / $22) since the last PROC_TAG.
const
  PROC_TAG          = TRsmTag.PROC_TAG;
  LOCAL_TAG         = TRsmTag.LOCAL_TAG;
  PARAM_TAG         = TRsmTag.PARAM_TAG;
  REGVAR_TAG        = TRsmTag.REGVAR_TAG;
  GLOBAL_PRIM_TAG   = TRsmTag.GLOBAL_PRIM_TAG;
  ENUM_CONST_TAG    = TRsmTag.ENUM_CONST_TAG;
  TYPE_REGISTRY_TAG = TRsmTag.TYPE_REGISTRY_TAG;
  ENUM_DEF_TAG      = TRsmTag.ENUM_DEF_TAG;
  UNIT_USE_INTRO    = TRsmTag.UNIT_USE_INTRO;
  UNIT_USE_FILE     = TRsmTag.UNIT_USE_FILE;
  SCOPE_END         = TRsmTag.SCOPE_END;
var
  P  : NativeInt;
  Tag: Byte;
begin
  FScanInProc := False;
  FScanSeenLocalSinceProc := False;
  FScanLocalIdx := 0;
  FScanRegParam := 0;
  FCurrentSourceFileIdx := -1;
  P := 0;
  while P + 2 < FSz do
  begin
    Tag := ByteAt(P);
    case Tag of
      PROC_TAG:
        if HandleProcRecord(P) then Continue;
      PARAM_TAG:
        if FScanInProc and (FProcs.Count > 0) and
           HandleParamRecord(P) then Continue;
      REGVAR_TAG:
        if FScanInProc and (FProcs.Count > 0) and
           HandleRegVarRecord(P) then Continue;
      LOCAL_TAG:
        if FScanInProc and (FProcs.Count > 0) then
        begin
          if HandleLocalRecord(P) then Continue;
        end
        else if not FScanInProc then
        begin
          if HandleModuleGlobalLocalTagRecord(P) then Continue;
        end;
      GLOBAL_PRIM_TAG:
        if HandleGlobalPrimRecord(P) then Continue;
      ENUM_CONST_TAG:
        if HandleEnumConstantRecord(P) then Continue;
      ENUM_DEF_TAG:
        HandleEnumDefRecord(P);
      TYPE_REGISTRY_TAG:
        HandleTypeRegistryRecord(P);
      UNIT_USE_INTRO:
        if HandleUnitUseIntroRecord(P) then Continue;
      UNIT_USE_FILE:
        if HandleSourceFileIntroRecord(P) then Continue;
      SCOPE_END:
        if FScanSeenLocalSinceProc then
        begin
          FScanInProc := False;
          FScanSeenLocalSinceProc := False;
          FScanLocalIdx := 0;
        end;
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
