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
  TRsmLocal = record
    Name    : String;
    BpOffset: Int32;
    TypeIdx : UInt32;
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
    Name   : String;
    TypeIdx: UInt32;
    Kind   : TRsmStructKind;
    Members: IList<TRsmClassMember>;
  end;

  /// <summary>
  ///   Reader for the RSM (CSH7) symbol container produced by the
  ///   Delphi linker option -VR. The single source of debug
  ///   information used by the debugger.
  /// </summary>
  TRsmLocalsReader = class
  private
    FProcs  : IList<TRsmProc>;
    FClasses: IList<TRsmClassInfo>;
  public
    constructor Create;
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
  FProcs := Collections.NewPlainList<TRsmProc>;
  FClasses := Collections.NewPlainList<TRsmClassInfo>;
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
    // Identifier bytes: ASCII letters, digits, underscore. Field/class
    // names are emitted verbatim from Delphi source identifiers, so
    // restricting to identifier characters cuts down on false-positive
    // matches in unrelated payload bytes.
    Result := ((AB >= Ord('A')) and (AB <= Ord('Z'))) or
              ((AB >= Ord('a')) and (AB <= Ord('z'))) or
              ((AB >= Ord('0')) and (AB <= Ord('9'))) or
              (AB = Ord('_'));
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
  // own record and the next proc (or the $63 scope-end byte). The
  // Win32 proc-address form is decoded inline below; the Win64 form
  // is a variable-length encoding that hasn't been reversed, so
  // Win64 procs land with SegmentOffset = 0 and a side channel
  // (typically the .map file) must patch them before
  // FindProcContaining can resolve PC -> proc on Win64.
  const
    PROC_TAG  = $28;
    LOCAL_TAG = $20;
    SCOPE_END = $63;
  var
    P        : NativeInt;
    Tag      : Byte;
    NameLen  : Byte;
    Name     : String;
    Proc     : TRsmProc;
    Loc      : TRsmLocal;
    InProc   : Boolean;
    LocalIdx : Integer;
  begin
    InProc := False;
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
          // Skip the proc-name reference forms (e.g. $07 $28 ... in
          // some streams). Real proc declarations are preceded by a
          // distinctive run-up; the simplest filter is to require
          // the name to look like a Delphi identifier and to NOT be
          // already registered (to skip duplicates).
          if FindProcByName(Name) < 0 then
          begin
            Proc := Default(TRsmProc);
            Proc.Name := Name;
            // Win32 proc address encoding: the 4 bytes immediately
            // after the proc name and a fixed "20 00 00" prefix
            // pack the proc's runtime VA into the high 28 bits and
            // a 4-bit type/flag tag into the low 4 bits. Recovering
            // the segment-relative RVA is therefore
            //     RVA = (DWORD >> 4) - $401000
            // where $401000 is the Win32 image-base ($400000) plus
            // the .text section's typical RVA ($1000). Win64 uses a
            // different (variable-length) encoding that hasn't been
            // decoded yet; the same formula yields out-of-range
            // values there, so we accept the result only if it fits
            // a plausible code-segment RVA window.
            Proc.SegmentOffset := 0;
            Proc.Size := 0;
            if P + 2 + Length(Name) + 7 < Sz then
            begin
              // Proc payload starts 2 (tag+namelen) + Length(Name)
              // bytes from P. After the 3-byte "20 00 00" prefix,
              // the 4 address bytes start at offset 3.
              var AddrDword: UInt32 := DwordAt(P + 2 + Length(Name) + 3);
              var Decoded: Int64 := (Int64(AddrDword) shr 4) - $401000;
              if (Decoded > 0) and (Decoded < $1000000) then
              begin
                Proc.SegmentOffset := NativeUInt(Decoded);
                // Size unknown from the proc record; use a
                // generous placeholder so FindProcContaining can
                // hit, then patch up below once all procs are
                // collected (size = next-proc-start - this-start).
                Proc.Size := $1000;
              end;
            end;
            Proc.Locals := Collections.NewPlainList<TRsmLocal>;
            FProcs.Add(Proc);
            InProc := True;
            LocalIdx := 0;
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
          // The frame offset is encoded as a tagged variable-length
          // integer placed after the type-ref prefix "$66 $00 $00".
          // Three forms have been observed empirically against the
          // TD32 ground truth from the same EXE:
          //
          //   5-byte payload "$66 $00 $00 <type> <off8>":
          //     1-byte signed offset, decoded as SignedInt8 / 2.
          //     Used for built-in scalar types whose offsets fit in
          //     +/-64 bytes from the frame pointer.
          //
          //   6-byte payload "$66 $00 $00 <type> <enc-lo> <enc-hi>"
          //   where (enc-lo and 1) = 1:
          //     2-byte little-endian signed offset, encoded as
          //     (BpOffset * 4) + 1; decoded as (SmallInt - 1) div 4.
          //     Used for built-in types with offsets exceeding the
          //     1-byte range (e.g. ShortString locals).
          //
          //   6-byte payload "$66 $00 $00 <type> <classId> <off8>"
          //   where (classId and 1) = 0:
          //     class-typed local (e.g. TStringList). One additional
          //     byte of class-id sits between the type and the
          //     offset; the offset itself is again 1-byte signed / 2.
          //
          // Length discrimination: the byte at payload offset 5 is
          // the next record's tag byte ($20/$28/$63) for the 5-byte
          // form; otherwise the 6-byte form applies and the offset
          // form is selected by the LSB of the 4th payload byte.
          var PayloadStart: NativeInt := P + 2 + Length(Name);
          if PayloadStart + 5 < Sz then
          begin
            var Next5: Byte := ByteAt(PayloadStart + 5);
            if (Next5 = LOCAL_TAG) or (Next5 = PROC_TAG) or (Next5 = SCOPE_END) then
            begin
              // 5-byte form, 1-byte offset.
              var Ofs8: ShortInt := ShortInt(ByteAt(PayloadStart + 4));
              Loc.BpOffset := Int32(Ofs8) div 2;
              Loc.TypeIdx  := ByteAt(PayloadStart + 3);
            end
            else if PayloadStart + 6 < Sz then
            begin
              var Next6: Byte := ByteAt(PayloadStart + 6);
              if (Next6 = LOCAL_TAG) or (Next6 = PROC_TAG) or (Next6 = SCOPE_END) then
              begin
                var Ofs0: Byte := ByteAt(PayloadStart + 4);
                if (Ofs0 and 1) = 1 then
                begin
                  // 6-byte built-in form: 2-byte LE offset.
                  var Hi : Byte := ByteAt(PayloadStart + 5);
                  var W  : Word := Word(Ofs0) or (Word(Hi) shl 8);
                  var SW : SmallInt := SmallInt(W);
                  Loc.BpOffset := (Int32(SW) - 1) div 4;
                  Loc.TypeIdx  := ByteAt(PayloadStart + 3);
                end
                else
                begin
                  // 6-byte class form: <classId> <off8>.
                  var Ofs8: ShortInt := ShortInt(ByteAt(PayloadStart + 5));
                  Loc.BpOffset := Int32(Ofs8) div 2;
                  Loc.TypeIdx  := ByteAt(PayloadStart + 3);
                end;
              end;
            end;
          end;
          FProcs[FProcs.Count - 1].Locals.Add(Loc);
          Inc(LocalIdx);
          P := P + 2 + Length(Name);
          Continue;
        end;
      end
      else if Tag = SCOPE_END then
      begin
        InProc := False;
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
        // its TypeIdx via the field-type-id.
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
            Break;
          end;
        end;
        P := EndOff + 6;
      end;
    end;

  begin
    SetLength(NameToRawId, 64);
    NameRawIdCnt := 0;
    ScanTypeRegistry;
    LinkFieldsFromFormatA;
  end;

begin
  FProcs.Clear;
  FClasses.Clear;
  if Length(ABytes) < 8 then
    Exit;
  if PUInt32(@ABytes[0])^ <> SigCSH7 then
    Exit;

  Buf := PByte(@ABytes[0]);
  Sz  := Length(ABytes);
  ScanSymbolStream;
  RecomputeProcSizes;
  DiscoverAndParseAllStructs;
  LinkMemberTypeIdsFromFormatA;
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
var
  I       : Integer;
  Stored  : String;
  StripAt : String;
begin
  for I := 0 to FProcs.Count - 1 do
  begin
    Stored := FProcs[I].Name;
    if SameText(Stored, AName) then
      Exit(I);
    if (Length(Stored) > 0) and (Stored[1] = '@') then
    begin
      StripAt := Copy(Stored, 2, MaxInt);
      if SameText(StripAt, AName) then
        Exit(I);
    end;
  end;
  Result := -1;
end;

function TRsmLocalsReader.FindClassByName(const AName: String): Integer;
var
  I: Integer;
begin
  for I := 0 to FClasses.Count - 1 do
    if SameText(FClasses[I].Name, AName) then
      Exit(I);
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
var
  ClsIdx, I: Integer;
  Info     : TRsmClassInfo;
begin
  Result := False;
  AMember := Default(TRsmClassMember);
  ClsIdx := FindClassByName(AClassName);
  if ClsIdx < 0 then
    Exit;
  Info := FClasses[ClsIdx];
  for I := 0 to Info.Members.Count - 1 do
    if SameText(Info.Members[I].Name, AFieldName) then
    begin
      AMember := Info.Members[I];
      Exit(True);
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
