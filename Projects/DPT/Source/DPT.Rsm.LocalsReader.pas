// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.LocalsReader;

// Reader for Borland's RSM (remote symbol map) files produced by the
// Delphi linker option -VR ("Include remote debug symbols"). The
// on-disk container uses the magic 'CSH7' and stores procedures,
// locals (with BPREL-style frame offsets), classes, records, and
// types in a tagged variable-length symbol stream.
//
// This is the project's only debug-info reader; TD32 (FB09) used to
// be parsed by a parallel DPT.Td32.LocalsReader unit but has been
// retired. Builds for the test fixture must therefore include -VR
// so a .rsm sidecar exists next to the EXE.

interface

uses

  System.Classes,
  System.SysUtils,

  mormot.core.collections;

type

  /// <summary>
  ///   A single local variable extracted from an RSM symbol stream:
  ///   its source name, its signed offset relative to the procedure's
  ///   base pointer (EBP/RBP), and the RSM type-id of its declared
  ///   type.
  /// </summary>
  /// <summary>
  ///   How a local/parameter is reached from the executing thread:
  ///   either as a BP-relative stack slot (lkBpRel) where the address
  ///   is FramePtr + BpOffset, or as a register parameter (lkRegister)
  ///   where the value lives in a CPU register at the time of the
  ///   call. Register parameters appear for the first few arguments of
  ///   a Delphi proc under the default "register" / Win64 ABI -- they
  ///   are NOT spilled to stack slots, so callers must read them out of
  ///   the thread's register file instead of from FramePtr + offset.
  /// </summary>
  TRsmLocalKind = (lkBpRel, lkRegister);

  TRsmLocal = record
    Name    : String;
    BpOffset: Int32;
    TypeIdx : UInt32;
    Kind    : TRsmLocalKind;
    /// <summary>
    ///   Zero-based index of the parameter within the calling-
    ///   convention's register order: 0 = 1st reg param, 1 = 2nd, etc.
    ///   Only meaningful when Kind = lkRegister. The concrete register
    ///   is platform-specific (x86 Delphi default: EAX, EDX, ECX;
    ///   Win64 ABI: RCX, RDX, R8, R9), so the higher layer resolves
    ///   index -> register name when reading the value.
    /// </summary>
    RegParamIdx: Byte;
  end;

  /// <summary>
  ///   A procedure scope extracted from an RSM symbol stream, with
  ///   all of its locals attached. <c>SegmentOffset</c> is the RVA
  ///   of the procedure's first instruction within the executable's
  ///   code segment, in the same convention as the TD32 reader uses.
  /// </summary>
  TRsmProc = record
    Name         : String;
    SegmentOffset: NativeUInt;
    Size         : NativeUInt;
    Locals       : IList<TRsmLocal>;
  end;

  /// <summary>
  ///   A field of a Delphi class or record extracted from an RSM type
  ///   record: the field's name, its byte offset within the parent
  ///   instance, and the RSM type-id of the field's declared type.
  ///   For class fields the offset is relative to the instance pointer
  ///   (so the first field sits at offset SizeOf(Pointer), past the
  ///   VMT slot); for record fields the offset is relative to the
  ///   start of the record.
  /// </summary>
  TRsmClassMember = record
    Name   : String;
    Offset : UInt32;
    TypeIdx: UInt32;
  end;

  /// <summary>
  ///   Whether a structured type from the RSM type stream is a Delphi
  ///   class (with a VMT pointer at instance offset 0) or a Delphi
  ///   record (no VMT, fields start at offset 0). Decides whether
  ///   navigation through this type dereferences the holding slot
  ///   (class) or treats it as inline data (record).
  /// </summary>
  TRsmStructKind = (skClass, skRecord);

  /// <summary>
  ///   A class or record declared in the debugged binary, parsed from
  ///   the RSM type stream: its short name, its RSM type-id (used by
  ///   record-typed field navigation, where the holding field's static
  ///   TypeIdx is the only key to identify the inline record's layout),
  ///   the kind, and the ordered list of its member fields.
  /// </summary>
  TRsmClassInfo = record
    Name      : String;
    TypeIdx   : UInt32;
    Kind      : TRsmStructKind;
    Members   : IList<TRsmClassMember>;
    /// <summary>
    ///   Name of the immediate parent class for class-kind entries.
    ///   Empty when the class is at the top of the user-visible
    ///   hierarchy (the implicit TObject parent or, for records,
    ///   any inheritance at all). Populated by a post-parse pass
    ///   that matches each class's first-own-field offset against
    ///   other classes' last-own-field end, since the RSM symbol
    ///   stream itself does not emit an explicit parent reference
    ///   in any form the reader has identified.
    /// </summary>
    ParentName: String;
  end;

  /// <summary>
  ///   Reader for the RSM (CSH7) symbol container produced by the
  ///   Delphi linker option -VR. The single source of debug
  ///   information used by the debugger.
  /// </summary>
  TRsmLocalsReader = class
  private
    FProcs       : IList<TRsmProc>;
    FClasses     : IList<TRsmClassInfo>;
    /// Case-insensitive name -> FProcs/FClasses index. These exist
    /// purely to speed up FindProcByName / FindClassByName from
    /// O(N) per lookup to O(1), which collapses the inner loops of
    /// ScanSymbolStream and DiscoverAndParseAllStructs from O(N^2)
    /// to O(N) on large binaries.
    FProcByName  : IKeyValue<String, Integer>;
    FClassByName : IKeyValue<String, Integer>;
    FOnPhase     : TProc<String>;
    procedure DeriveClassParents;
    procedure ReportPhase(const APhase: String); inline;
  public
    constructor Create;
    /// <summary>
    ///   Optional progress callback fired by LoadFromBytes at each
    ///   major parsing phase. Lets callers surface what the parser
    ///   is doing on very large RSM files, where a single phase can
    ///   run for many seconds and otherwise looks indistinguishable
    ///   from a hang.
    /// </summary>
    property OnPhase: TProc<String> read FOnPhase write FOnPhase;
    /// <summary>
    ///   Loads RSM debug info from the sidecar file produced next to
    ///   a Delphi executable built with -VR. The sidecar's path is
    ///   <c>ChangeFileExt(AExePath, '.rsm')</c>; this method takes
    ///   the EXE path so callers can stay symmetric with the TD32
    ///   reader's <c>LoadFromFile(ExePath)</c>.
    /// </summary>
    procedure LoadFromFile(const AExePath: String);
    procedure LoadFromBytes(const ABytes: TBytes);
    function  FindProcContaining(ASegmentOffset: NativeUInt): Integer;
    function  FindProcByName(const AName: String): Integer;
    function  FindClassByName(const AName: String): Integer;
    function  FindStructByTypeIdx(ATypeIdx: UInt32): Integer;
    function  FindClassMember(const AClassName, AFieldName: String;
      out AMember: TRsmClassMember): Boolean;
    function  FindStructMemberByTypeIdx(ATypeIdx: UInt32; const AFieldName: String;
      out AMember: TRsmClassMember): Boolean;
    function  IsRecordTypeIdx(ATypeIdx: UInt32): Boolean;
    /// <summary>
    ///   Recompute the Size field of every proc as the gap to the
    ///   next proc by SegmentOffset. The address-decoder couldn't
    ///   reverse Win64 proc addresses, so callers may patch each
    ///   proc's SegmentOffset from a side channel (e.g. the .map
    ///   file) and then invoke this method to refresh sizes.
    /// </summary>
    procedure RecomputeProcSizes;
    property  Procs: IList<TRsmProc> read FProcs;
    property  Classes: IList<TRsmClassInfo> read FClasses;
  end;

implementation

constructor TRsmLocalsReader.Create;
begin
  inherited Create;
  FProcs       := Collections.NewPlainList<TRsmProc>;
  FClasses     := Collections.NewPlainList<TRsmClassInfo>;
  FProcByName  := Collections.NewPlainKeyValue<String, Integer>;
  FClassByName := Collections.NewPlainKeyValue<String, Integer>;
end;

procedure TRsmLocalsReader.ReportPhase(const APhase: String);
begin
  if Assigned(FOnPhase) then
    FOnPhase(APhase);
end;

procedure TRsmLocalsReader.LoadFromFile(const AExePath: String);
var
  RsmPath: String;
  Stream : TFileStream;
  Bytes  : TBytes;
begin
  RsmPath := ChangeFileExt(AExePath, '.rsm');
  if not FileExists(RsmPath) then
    Exit;
  ReportPhase('read file');
  Stream := TFileStream.Create(RsmPath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Bytes, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Bytes[0], Stream.Size);
  finally
    Stream.Free;
  end;
  LoadFromBytes(Bytes);
end;

procedure TRsmLocalsReader.LoadFromBytes(const ABytes: TBytes);
const
  SigCSH7: UInt32 = $37485343; // 'CSH7' on disk in little-endian byte order
var
  Buf: PByte;
  Sz : NativeInt;

  function ByteAt(AOffset: NativeInt): Byte;
  begin
    Result := (Buf + AOffset)^;
  end;

  function DwordAt(AOffset: NativeInt): UInt32;
  begin
    Result := PUInt32(Buf + AOffset)^;
  end;

  function IsPrintableAscii(AB: Byte): Boolean;
  begin
    // Identifier bytes accepted inside RSM length-prefixed names.
    // The CSH7 emitter writes names verbatim from Delphi source, but
    // a "name" can be qualified or compiler-decorated:
    //   - 'TFormMain.Create'                  -- class-method PROC records
    //                                            (TFW emits this form for
    //                                            every method)
    //   - 'TFormMain.Create$ActRec'           -- closure / nested-func record
    //   - 'TList<TFoo>.Add'                   -- generic instantiation
    //   - '@MyName'                           -- linker-emitted alias
    //   - 'TMap<TKey,TValue>.Get'             -- multi-arg generics (comma)
    // Keeping the check tight (no whitespace, no control bytes) limits
    // false-positive PROC records when a random byte run happens to
    // pass the length-prefix shape, while still letting real qualified
    // names through. The earlier alnum-plus-underscore set rejected
    // every dot- or generic-prefixed PROC name on TFW, dropping 36% of
    // procs from the reader and breaking FindProcContaining.
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

  function ReadIdentifier(AOffset: NativeInt; out AName: String): Boolean;
  // A length-prefixed identifier: 1 byte name-length followed by N bytes
  // of identifier text. Returns False when the length is implausible
  // for a Delphi identifier or any byte in the run is not an identifier
  // character (so non-name bytes can't be misread as a name).
  var
    L, I: Integer;
    Buf2: TBytes;
  begin
    Result := False;
    AName := '';
    if (AOffset < 0) or (AOffset >= Sz) then Exit;
    L := ByteAt(AOffset);
    if (L < 1) or (L > 64) then Exit;
    if AOffset + 1 + L > Sz then Exit;
    for I := 0 to L - 1 do
      if not IsPrintableAscii(ByteAt(AOffset + 1 + I)) then Exit;
    SetLength(Buf2, L);
    Move((Buf + AOffset + 1)^, Buf2[0], L);
    AName := TEncoding.ANSI.GetString(Buf2);
    Result := True;
  end;

  procedure ScanFieldsBackwardFromClassName(AClassNameOff: NativeInt;
    AKind: TRsmStructKind; const AClassName: String);
  // Scan a fixed window of bytes BEFORE the class-trailer position
  // looking for <DWORD-offset-LE> <namelen> <name> patterns. The
  // class trailer (the bytes between the last field's name and the
  // class name itself) varies in length but is always small, so a
  // wider window catches whatever sits between the fields and the
  // class name. Each <offset, namelen, name> triple is recorded as
  // a candidate; duplicates and overlaps are filtered out, and the
  // surviving members are sorted by offset to give source-declaration
  // order back.
  const
    MaxFields    = 32;
    ScanWindow   = 256;
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
      List.Add(Member);
    end;

    Info.Name    := AClassName;
    Info.TypeIdx := UInt32(AClassNameOff);
    Info.Kind    := AKind;
    Info.Members := List;
    FClassByName[LowerCase(AClassName)] := FClasses.Count;
    FClasses.Add(Info);
  end;

  procedure ScanFieldsForwardFromRecordName(ARecordNameOff: NativeInt;
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
    if ARecordNameOff + 1 + NL + 4 > Sz then Exit;
    PStart := ARecordNameOff + 1 + NL + 4;

    // The header between the size DWORD and the first field tag has
    // a variable layout (count, flags, padding) we don't fully
    // understand, so locate the first field tag heuristically: scan
    // forward up to a bounded window for the first $02 byte that is
    // followed by a plausible namelen and identifier-only name.
    FoundFirst := False;
    P := PStart;
    PScan := PStart + 64;
    if PScan > Sz - 8 then PScan := Sz - 8;
    while P < PScan do
    begin
      if (ByteAt(P) = $02) then
      begin
        NameLen := ByteAt(P + 1);
        if (NameLen >= 1) and (NameLen <= 40) and
           ReadIdentifier(P + 1, Name) and
           (Length(Name) >= 1) and (Name[1] = 'F') then
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
    // typeinfo + optional 4-byte next-offset).
    Count := 0;
    Offsets[0] := 0;
    while (P + 8 < Sz) and (Count < MaxFields) do
    begin
      if ByteAt(P) <> $02 then Break;
      NameLen := ByteAt(P + 1);
      if (NameLen < 1) or (NameLen > 40) then Break;
      if not ReadIdentifier(P + 1, Name) then Break;
      if (Length(Name) < 1) or (Name[1] <> 'F') then Break;

      // 6-byte typeinfo follows the name; byte 2 is the last-flag.
      if P + 2 + Length(Name) + 6 > Sz then Break;
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
      if TypeinfoEnd + 8 > Sz then Break;
      var Tag4: Byte := ByteAt(TypeinfoEnd + 4);
      if Tag4 = $02 then
      begin
        NextOff := DwordAt(TypeinfoEnd);
        Offsets[Count] := NextOff;
        P := TypeinfoEnd + 4;
      end
      else
      begin
        if TypeinfoEnd + 12 > Sz then Break;
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
      List.Add(Member);
    end;

    Info.Name    := ARecordName;
    Info.TypeIdx := UInt32(ARecordNameOff);
    Info.Kind    := skRecord;
    Info.Members := List;
    FClassByName[LowerCase(ARecordName)] := FClasses.Count;
    FClasses.Add(Info);
  end;

  procedure DiscoverAndParseAllStructs;
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
  // multiple sub-streams) are filtered by checking FindClassByName
  // before re-adding.
  var
    P    : NativeInt;
    L    : Byte;
    Name : String;

    function SecondNameMatches(ASecondStart: NativeInt; ANL: Byte;
      const AName: String): Boolean;
    var
      K: Integer;
    begin
      Result := False;
      if ByteAt(ASecondStart) <> ANL then Exit;
      for K := 0 to Integer(ANL) - 1 do
        if ByteAt(ASecondStart + 1 + K) <> Byte(Ord(AName[K + 1])) then Exit;
      Result := True;
    end;

  begin
    P := 1;
    while P + 24 < Sz do
    begin
      L := ByteAt(P);
      if (L >= 1) and (L <= 40) and
         ReadIdentifier(P, Name) and
         (Length(Name) >= 2) and (Name[1] = 'T') then
      begin
        // Probe both class-trailer layouts.
        var NameEnd: NativeInt := P + 1 + L;
        var IsClass: Boolean := False;
        // Win32: $07 at NameEnd + 4, duplicate name at +5.
        if (NameEnd + 5 + L < Sz) and (ByteAt(NameEnd + 4) = $07) and
           SecondNameMatches(NameEnd + 5, L, Name) then
          IsClass := True
        // Win64: $07 at NameEnd + 8, duplicate name at +9.
        else if (NameEnd + 9 + L < Sz) and (ByteAt(NameEnd + 8) = $07) and
                SecondNameMatches(NameEnd + 9, L, Name) then
          IsClass := True;

        if IsClass then
        begin
          if FindClassByName(Name) < 0 then
            ScanFieldsBackwardFromClassName(P, skClass, Name);
        end
        else if (P > 0) and (ByteAt(P - 1) = $0E) then
        begin
          // Record probe: $0E sentinel right before the name.
          if FindClassByName(Name) < 0 then
            ScanFieldsForwardFromRecordName(P, Name);
        end;
      end;
      Inc(P);
    end;
  end;

  procedure ScanSymbolStream;
  // Walk the RSM symbol stream looking for procedure ($28 tag) and
  // local ($20 tag) records. Each proc owns the locals between its
  // own record and the next proc (or the $63 scope-end byte). Both
  // Win32 and Win64 proc-address forms are decoded inline below.
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
    PROC_TAG  = $28;
    LOCAL_TAG = $20;
    PARAM_TAG = $22;
    // Register-passed variable record: $21 NameLen Name $66 $00 $00 <type-id-lo> <type-id-hi>.
    // Used for the parameters that live in CPU registers under
    // Delphi's default calling convention -- on x86 that's the first
    // three slots (EAX/EDX/ECX), on x64 the first four (RCX/RDX/R8/R9).
    // Constructors emit an extra slot for the hidden allocation flag.
    // Without this tag, register-resident params (Self, AOwner, ...)
    // never make it into Proc.Locals and get_locals / evaluate fail
    // for every method whose params don't spill to the stack.
    REGVAR_TAG = $21;
    SCOPE_END = $63;
    AddrScanWindow = 32;
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
        if AOff + 3 >= Sz then Exit;
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
        if AOff + 4 >= Sz then Exit;
        if (ByteAt(AOff) and $7F) <> $03 then Exit;
        // Marker layout "04 10 ?? 2E 00": bytes 0/1/3/4 are
        // constant across the corpus; the byte at position 2 is a
        // per-binary counter that the linker varies build-to-build
        // (it isn't tied to the proc, but to the overall RSM
        // module layout), so we accept any value there.
        for MOff := AOff + 4 to AOff + 5 do
          if (MOff + 4 < Sz) and
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
      if AStartOff + 3 >= Sz then Exit;
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
    P := 0;
    while P + 2 < Sz do
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
          if P + 2 + Length(Name) + 4 < Sz then
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
      // Local record: $20 NameLen Name <5-or-6-byte type+frame ref>.
      // The two bytes after the constant "66 00 00" type-ref prefix
      // pack the type-id (first byte) and the frame offset (second
      // byte, encoded as 2 * BpOffset signed int8 -- so $f8 means
      // BpOffset = -4). For larger offsets (e.g. ShortString locals
      // whose offsets exceed +/-64) the encoding switches to a
      // multi-byte form that hasn't been fully decoded yet; in that
      // case we fall back to a synthesized ordinal offset so the
      // distinct-offsets invariant still holds.
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
          if (PayloadStart + 4 < Sz) and
             (ByteAt(PayloadStart)     = $62) and
             (ByteAt(PayloadStart + 1) = $00) and
             (ByteAt(PayloadStart + 2) = $00) then
          begin
            // Type-id is the LE 16-bit value "<lo> $2E" (or just
            // "<lo>" if byte 4 is not $2E for built-in types).
            var Hi: Byte := ByteAt(PayloadStart + 4);
            if Hi = $2E then
              Loc.TypeIdx := UInt32(ByteAt(PayloadStart + 3)) or
                              (UInt32($2E) shl 8)
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
          if (P + 4 < Sz) and (ByteAt(P) = $62) and
             (ByteAt(P + 1) = $00) and (ByteAt(P + 2) = $00) then
            P := P + 5;
          if (P + 1 < Sz) and (ByteAt(P) = $20) and (ByteAt(P + 1) = $21) then
            Inc(RegParam);
          Continue;
        end;
      end
      // Register-passed variable: $21 NameLen Name $66 $00 $00 <typeidLo> <typeidHi>.
      // These hold the Self pointer and the parameters that fit into
      // the calling convention's register slots. Without this branch
      // a method like "constructor Create(AOwner: TComponent)" has
      // NO locals after the scan -- Self / AOwner ride EAX / ECX,
      // never spill, and the previous code only looked at BPREL
      // ($20) and open-array ($22) records. Sequential RegParamIdx
      // matches what RegisterParamBytes already expects (0=EAX/RCX,
      // 1=EDX/RDX, 2=ECX/R8, 3=R9 on x64).
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
          if (PayloadStart + 4 < Sz) and
             (ByteAt(PayloadStart)     = $66) and
             (ByteAt(PayloadStart + 1) = $00) and
             (ByteAt(PayloadStart + 2) = $00) then
          begin
            // Same two-byte type-id form as PARAM_TAG; surface as the
            // LE 16-bit value so the higher-level walker can match it
            // against the class/record registry.
            var Hi: Byte := ByteAt(PayloadStart + 4);
            if Hi = $2E then
              Loc.TypeIdx := UInt32(ByteAt(PayloadStart + 3)) or
                              (UInt32($2E) shl 8)
            else
              Loc.TypeIdx := ByteAt(PayloadStart + 3);
          end;
          FProcs[FProcs.Count - 1].Locals.Add(Loc);
          Inc(LocalIdx);
          SeenLocalSinceProc := True;
          // Advance past the name + 5-byte payload "$66 $00 $00 <lo> <hi>".
          P := P + 2 + Length(Name);
          if (P + 4 < Sz) and (ByteAt(P) = $66) and
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
          // Synthesized fallback is far outside the real-frame range
          // (real BpOffsets sit in the [-4, -frameSize] window), so a
          // synthesized value cannot collide with a decoded one.
          Loc.BpOffset := -10000 - (LocalIdx * 4);
          Loc.TypeIdx  := 0;
          Loc.Kind     := lkBpRel;
          // The frame offset is encoded as a tagged variable-length
          // integer placed after the type-ref prefix "$66 $00 $00".
          // The byte at payload offset 4 picks one of two type-id
          // widths, and then the offset can take either a 1-byte
          // signed or a 2-byte little-endian form:
          //
          //   byte4 = $2E -- two-byte type-id "<lo> $2E" for class /
          //                  record / set / dyn-array locals. The
          //                  offset starts at byte 5 and continues
          //                  for either 1 byte (LSB of byte5 = 0)
          //                  or 2 bytes LE (LSB of byte5 = 1). The
          //                  bytes that follow may include trailing
          //                  type-name metadata which the outer
          //                  loop skips over by advancing
          //                  byte-by-byte until the next record
          //                  tag, so we deliberately do NOT require
          //                  byte 6 to be a record tag here.
          //
          //   byte4 = anything else -- one-byte type-id form. The
          //                  offset starts at byte 4. If the next
          //                  record tag sits at byte 5 it's the
          //                  1-byte signed form (BpOffset = byte4
          //                  signed div 2); if it sits at byte 6
          //                  then bytes 4-5 hold the 2-byte LE form
          //                  ((SmallInt-1) div 4).
          //
          // Both 1-byte signed forms use the SignedInt8 / 2 scaling
          // because BpOffset is always a multiple of 2 in practice
          // (the encoder doubles it to free the LSB as a form
          // discriminator).
          var PayloadStart: NativeInt := P + 2 + Length(Name);
          if PayloadStart + 5 < Sz then
          begin
            var Byte4: Byte := ByteAt(PayloadStart + 4);
            if Byte4 = $2E then
            begin
              // Two-byte type-id form. The offset bytes start at +5.
              var Byte5: Byte := ByteAt(PayloadStart + 5);
              if (Byte5 and 1) = 1 then
              begin
                if PayloadStart + 6 < Sz then
                begin
                  // 2-byte LE offset.
                  var Hi : Byte := ByteAt(PayloadStart + 6);
                  var W  : Word := Word(Byte5) or (Word(Hi) shl 8);
                  var SW : SmallInt := SmallInt(W);
                  Loc.BpOffset := (Int32(SW) - 1) div 4;
                end;
              end
              else
              begin
                // 1-byte signed offset.
                var Ofs8: ShortInt := ShortInt(Byte5);
                Loc.BpOffset := Int32(Ofs8) div 2;
              end;
              // Surface the type-id as the LE 16-bit value to allow
              // the higher-level walker to match it against the
              // class/record registry's 2-byte ids.
              Loc.TypeIdx := UInt32(ByteAt(PayloadStart + 3)) or
                             (UInt32($2E) shl 8);
            end
            else
            begin
              // One-byte type-id form. Disambiguate offset width by
              // looking at the next record tag's position.
              var Next5: Byte := ByteAt(PayloadStart + 5);
              if (Next5 = LOCAL_TAG) or (Next5 = PROC_TAG) or (Next5 = SCOPE_END) then
              begin
                // 5-byte form, 1-byte signed offset at byte 4.
                var Ofs8: ShortInt := ShortInt(Byte4);
                Loc.BpOffset := Int32(Ofs8) div 2;
                Loc.TypeIdx  := ByteAt(PayloadStart + 3);
              end
              else if PayloadStart + 6 < Sz then
              begin
                var Next6: Byte := ByteAt(PayloadStart + 6);
                if (Next6 = LOCAL_TAG) or (Next6 = PROC_TAG) or (Next6 = SCOPE_END) then
                begin
                  // 6-byte form, 2-byte LE offset at bytes 4-5.
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
          P := P + 2 + Length(Name);
          Continue;
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

  procedure LinkMemberTypeIdsFromFormatA;
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
    NameToRawId : array of record Name: String; Id: TRawId; end;
    NameRawIdCnt: Integer;
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

    function RawIdEqual(const A, B: TRawId): Boolean;
    begin
      Result := (A.Lo = B.Lo) and (A.Hi = B.Hi);
    end;

    function FindClassIdxForRawId(const ARaw: TRawId): Integer;
    var
      I, K: Integer;
    begin
      // Find the name registered with this raw-id and look it up in
      // our parsed Classes list. Returns -1 if either the raw-id or
      // the name isn't known to us (built-in types fall here).
      Result := -1;
      for I := 0 to NameRawIdCnt - 1 do
        if RawIdEqual(NameToRawId[I].Id, ARaw) then
        begin
          for K := 0 to FClasses.Count - 1 do
            if SameText(FClasses[K].Name, NameToRawId[I].Name) then
              Exit(K);
          Exit;
        end;
    end;

    procedure ScanTypeRegistry;
    var
      P, NL, K  : Integer;
      Name      : String;
      Valid     : Boolean;
      Id        : TRawId;
    begin
      P := 0;
      while P + 12 < Sz do
      begin
        if ByteAt(P) = $2A then
        begin
          NL := ByteAt(P + 1);
          if (NL >= 2) and (NL <= 40) and
             ReadIdentifier(P + 1, Name) and
             (Length(Name) >= 2) and (Name[1] = 'T') and
             (P + 2 + NL + 5 < Sz) and
             (ByteAt(P + 2 + NL) = $20) and
             (ByteAt(P + 2 + NL + 1) = $00) and
             (ByteAt(P + 2 + NL + 2) = $00) then
          begin
            Id.Lo := ByteAt(P + 2 + NL + 3);
            Id.Hi := ByteAt(P + 2 + NL + 4);
            Valid := True;
            // Deduplicate: registry can list the same name twice
            // (one record before, one after). Last entry wins.
            for K := 0 to NameRawIdCnt - 1 do
              if SameText(NameToRawId[K].Name, Name) then
              begin
                NameToRawId[K].Id := Id;
                Valid := False;
                Break;
              end;
            if Valid then
            begin
              if NameRawIdCnt >= Length(NameToRawId) then
                SetLength(NameToRawId, (NameRawIdCnt + 1) * 2);
              NameToRawId[NameRawIdCnt].Name := Name;
              NameToRawId[NameRawIdCnt].Id   := Id;
              Inc(NameRawIdCnt);
            end;
            P := P + 2 + NL + 5;
            Continue;
          end;
        end;
        Inc(P);
      end;
    end;

    procedure LinkFieldsFromFormatA;
    var
      P, NL    : Integer;
      Name     : String;
      FieldId  : TRawId;
      ParentId : TRawId;
      ParentIdx: Integer;
      FieldIdx : Integer;
      I, M     : Integer;
      Member   : TRsmClassMember;
      EndOff   : Integer;
    begin
      P := 0;
      while P + 24 < Sz do
      begin
        // A field record starts with either $2C (first field of a
        // class in Format A) or $FF $2C (subsequent fields). Peek
        // both forms.
        var TagOff: Integer := -1;
        if ByteAt(P) = $2C then
          TagOff := P
        else if (P + 1 < Sz) and (ByteAt(P) = $FF) and (ByteAt(P + 1) = $2C) then
          TagOff := P + 1;
        if TagOff < 0 then
        begin
          Inc(P);
          Continue;
        end;
        NL := ByteAt(TagOff + 1);
        if (NL < 1) or (NL > 40) or (not ReadIdentifier(TagOff + 1, Name)) or
           (Length(Name) = 0) or (Name[1] <> 'F') then
        begin
          Inc(P);
          Continue;
        end;
        // After the name expect the constant "00 02 00" + 2-byte
        // field-type-id.
        var After: Integer := TagOff + 2 + Length(Name);
        if After + 5 + 6 + 2 > Sz then
        begin
          Inc(P);
          Continue;
        end;
        if (ByteAt(After) <> $00) or (ByteAt(After + 1) <> $02) or
           (ByteAt(After + 2) <> $00) then
        begin
          Inc(P);
          Continue;
        end;
        FieldId.Lo := ByteAt(After + 3);
        FieldId.Hi := ByteAt(After + 4);
        // Record varies in length: scan a bounded window for the
        // terminator "07 00 00 08 <parent-id-lo> <parent-id-hi>".
        // The window is small (under 32 bytes) for both known forms.
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
        // Look up parent class in our list via the registry.
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
        for M := 0 to FClasses[ParentIdx].Members.Count - 1 do
        begin
          Member := FClasses[ParentIdx].Members[M];
          if SameText(Member.Name, Name) and (Member.TypeIdx = 0) then
          begin
            FieldIdx := FindClassIdxForRawId(FieldId);
            if FieldIdx >= 0 then
            begin
              Member.TypeIdx := FClasses[FieldIdx].TypeIdx;
              FClasses[ParentIdx].Members[M] := Member;
            end;
            ConfirmedAdd(ParentIdx, Name);
            Break;
          end;
        end;
        P := EndOff + 6;
      end;
    end;

  begin
    SetLength(NameToRawId, 64);
    NameRawIdCnt := 0;
    Confirmed := Collections.NewPlainKeyValue<String, Boolean>;
    ScanTypeRegistry;
    LinkFieldsFromFormatA;
    PruneSpuriousMembers;
  end;

begin
  FProcs.Clear;
  FClasses.Clear;
  FProcByName.Clear;
  FClassByName.Clear;
  if Length(ABytes) < 8 then
    Exit;
  if PUInt32(@ABytes[0])^ <> SigCSH7 then
    Exit;

  Buf := PByte(@ABytes[0]);
  Sz  := Length(ABytes);
  ReportPhase('ScanSymbolStream');
  ScanSymbolStream;
  ReportPhase('RecomputeProcSizes');
  RecomputeProcSizes;
  ReportPhase('DiscoverAndParseAllStructs');
  DiscoverAndParseAllStructs;
  ReportPhase('LinkMemberTypeIdsFromFormatA');
  LinkMemberTypeIdsFromFormatA;
  ReportPhase('DeriveClassParents');
  DeriveClassParents;
  ReportPhase('done');
end;

procedure TRsmLocalsReader.DeriveClassParents;
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
  I, J     : Integer;
  Info     : TRsmClassInfo;
  PtrSize  : UInt32;
  FirstOffs: array of UInt32;
  LastEnds : array of UInt32;
  Cand, Match: Integer;
  CandCount  : Integer;

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

  SetLength(FirstOffs, FClasses.Count);
  SetLength(LastEnds, FClasses.Count);
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
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
    Info := FClasses[I];
    if Info.Kind <> skClass then Continue;
    if FirstOffs[I] <= PtrSize then Continue;
    Match := -1;
    CandCount := 0;
    // Walk candidates from highest to lowest FClasses index so the
    // first hit is the latest-declared class. Delphi RSM emits the
    // type stream in source-declaration order with system classes
    // up front, so the most-recently-declared candidate is the one
    // most likely to be the actual parent (and not a same-sized
    // system class that happens to share an instance boundary).
    for Cand := FClasses.Count - 1 downto 0 do
    begin
      if Cand = I then Continue;
      if FClasses[Cand].Kind <> skClass then Continue;
      // A parent has to come BEFORE the child in the stream; skip
      // anything declared later than us, which is a sibling/child
      // at best and never the parent.
      if Cand > I then Continue;
      if LastEnds[Cand] = FirstOffs[I] then
      begin
        Inc(CandCount);
        if Match < 0 then Match := Cand;
      end;
    end;
    if Match >= 0 then
    begin
      Info.ParentName := FClasses[Match].Name;
      FClasses[I] := Info;
    end;
  end;

end;

procedure TRsmLocalsReader.RecomputeProcSizes;
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
  Idx       : array of Integer;
  I, J      : Integer;
  Tmp       : Integer;
  NextStart : NativeUInt;
  P         : TRsmProc;
  Gap       : Int64;
begin
  if FProcs.Count < 2 then Exit;
  SetLength(Idx, FProcs.Count);
  for I := 0 to FProcs.Count - 1 do Idx[I] := I;

  for I := 1 to High(Idx) do
  begin
    Tmp := Idx[I];
    J := I - 1;
    while (J >= 0) and (FProcs[Idx[J]].SegmentOffset > FProcs[Tmp].SegmentOffset) do
    begin
      Idx[J + 1] := Idx[J];
      Dec(J);
    end;
    Idx[J + 1] := Tmp;
  end;

  for I := 0 to High(Idx) - 1 do
  begin
    P := FProcs[Idx[I]];
    if P.SegmentOffset = 0 then
    begin
      P.Size := 0;
      FProcs[Idx[I]] := P;
      Continue;
    end;
    NextStart := 0;
    for J := I + 1 to High(Idx) do
      if FProcs[Idx[J]].SegmentOffset > P.SegmentOffset then
      begin
        NextStart := FProcs[Idx[J]].SegmentOffset;
        Break;
      end;
    if NextStart = 0 then
      P.Size := LastFallback
    else
    begin
      Gap := Int64(NextStart) - Int64(P.SegmentOffset);
      if Gap > MaxProcSize then Gap := MaxProcSize;
      P.Size := NativeUInt(Gap);
    end;
    FProcs[Idx[I]] := P;
  end;
  if Length(Idx) > 0 then
  begin
    P := FProcs[Idx[High(Idx)]];
    if P.SegmentOffset <> 0 then
    begin
      P.Size := LastFallback;
      FProcs[Idx[High(Idx)]] := P;
    end;
  end;
end;

function TRsmLocalsReader.FindProcContaining(ASegmentOffset: NativeUInt): Integer;
var
  I: Integer;
begin
  for I := 0 to FProcs.Count - 1 do
    if (FProcs[I].SegmentOffset <= ASegmentOffset) and
       (ASegmentOffset < FProcs[I].SegmentOffset + FProcs[I].Size) then
      Exit(I);
  Result := -1;
end;

function TRsmLocalsReader.FindProcByName(const AName: String): Integer;
begin
  // Hot path: case-insensitive direct hit via the name index, which
  // is populated whenever a proc is added to FProcs. The
  // "@"-prefixed alias is a Delphi linker convention -- some procs
  // are stored as "@MyName" but should be findable by the bare
  // "MyName" too -- so the index carries both keys at insert time.
  if FProcByName.TryGetValue(LowerCase(AName), Result) then
    Exit;
  Result := -1;
end;

function TRsmLocalsReader.FindClassByName(const AName: String): Integer;
begin
  if FClassByName.TryGetValue(LowerCase(AName), Result) then
    Exit;
  Result := -1;
end;

function TRsmLocalsReader.FindStructByTypeIdx(ATypeIdx: UInt32): Integer;
var
  I: Integer;
begin
  for I := 0 to FClasses.Count - 1 do
    if FClasses[I].TypeIdx = ATypeIdx then
      Exit(I);
  Result := -1;
end;

function TRsmLocalsReader.IsRecordTypeIdx(ATypeIdx: UInt32): Boolean;
var
  Idx: Integer;
begin
  Idx := FindStructByTypeIdx(ATypeIdx);
  Result := (Idx >= 0) and (FClasses[Idx].Kind = skRecord);
end;

function TRsmLocalsReader.FindClassMember(const AClassName, AFieldName: String;
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
begin
  Result := False;
  AMember := Default(TRsmClassMember);
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

function TRsmLocalsReader.FindStructMemberByTypeIdx(ATypeIdx: UInt32;
  const AFieldName: String; out AMember: TRsmClassMember): Boolean;
var
  Idx, I: Integer;
  Info  : TRsmClassInfo;
begin
  Result := False;
  AMember := Default(TRsmClassMember);
  Idx := FindStructByTypeIdx(ATypeIdx);
  if Idx < 0 then
    Exit;
  Info := FClasses[Idx];
  for I := 0 to Info.Members.Count - 1 do
    if SameText(Info.Members[I].Name, AFieldName) then
    begin
      AMember := Info.Members[I];
      Exit(True);
    end;
end;

end.
