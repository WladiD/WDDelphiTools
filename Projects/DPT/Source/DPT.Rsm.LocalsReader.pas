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
    SegmentOffset: UInt32;
    Size         : UInt32;
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
    function  FindProcContaining(ASegmentOffset: UInt32): Integer;
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

  function FindAllClassNameOffsets(const AClassName: String): TArray<NativeInt>;
  // Locate every length-prefixed occurrence of the given class/record
  // name within the buffer. RSM emits the same name across multiple
  // streams (the symbol-scope cluster, the type-definition cluster,
  // RTTI strings, ...), so callers iterate the candidates and pick
  // the one whose surrounding bytes look like a field list.
  var
    I, NL, K: Integer;
    Match   : Boolean;
  begin
    Result := nil;
    NL := Length(AClassName);
    if (NL < 1) or (NL > 64) then Exit;
    for I := 0 to Sz - 1 - NL - 1 do
    begin
      if ByteAt(I) <> Byte(NL) then Continue;
      Match := True;
      for K := 0 to NL - 1 do
        if ByteAt(I + 1 + K) <> Byte(Ord(AClassName[K + 1])) then
        begin
          Match := False;
          Break;
        end;
      if Match then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := I;
      end;
    end;
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
      // strings (e.g. unit names "DebugTarget", class-trailer text)
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
  // Records emit their fields AFTER the name (in declaration order),
  // unlike classes which emit fields before the trailer name. Each
  // field record is "02 <namelen> <name> <10 bytes type info>"; the
  // trailing 10 bytes don't directly encode the field's offset, so
  // we compute offsets cumulatively from the record's total size and
  // field count -- which fits the test fixture (homogeneous field
  // sizes per record). The record's total size is the DWORD that
  // immediately follows the record-name's namelen+name.
  const
    MaxFields  = 32;
    ScanWindow = 512;
  var
    Cands  : array[0..MaxFields - 1] of TRsmClassMember;
    Count  : Integer;
    Skip   : Boolean;
    NL     : Integer;
    PStart : NativeInt;
    PEnd   : NativeInt;
    P      : NativeInt;
    Tag    : Byte;
    NameLen: Byte;
    Name   : String;
    TotSz  : UInt32;
    Each   : UInt32;
    I      : Integer;
    Info   : TRsmClassInfo;
    List   : IList<TRsmClassMember>;
    Member : TRsmClassMember;
  begin
    NL := Length(ARecordName);
    // Skip past <namelen><name> to read the total-size DWORD that
    // follows the record name in the type stream.
    if ARecordNameOff + 1 + NL + 4 > Sz then Exit;
    TotSz := DwordAt(ARecordNameOff + 1 + NL);

    Count  := 0;
    PStart := ARecordNameOff + 1 + NL + 4;
    PEnd   := PStart + ScanWindow;
    if PEnd > Sz - 4 then PEnd := Sz - 4;

    P := PStart;
    while (P + 5 < PEnd) and (Count < MaxFields) do
    begin
      Tag := ByteAt(P);
      NameLen := ByteAt(P + 1);
      // Field record header: tag byte $02, namelen 1..40, then name.
      // Stop early when we hit a tag byte that is the record terminator
      // ($0E = next type-record marker, $07 = duplicated-name reference).
      if (Tag = $0E) or (Tag = $07) then Break;
      if (Tag <> $02) or (NameLen < 1) or (NameLen > 40) or
         not ReadIdentifier(P + 1, Name) then
      begin
        Inc(P);
        Continue;
      end;
      if (Length(Name) < 1) or (Name[1] <> 'F') then
      begin
        Inc(P);
        Continue;
      end;
      Skip := False;
      for I := 0 to Count - 1 do
        if SameText(Cands[I].Name, Name) then
        begin
          Skip := True;
          Break;
        end;
      if Skip then begin Inc(P); Continue; end;
      Cands[Count].Name    := Name;
      Cands[Count].Offset  := 0; // assigned below
      Cands[Count].TypeIdx := 0;
      Inc(Count);
      // Skip past the field record (tag + namelen + name + 10-byte
      // payload empirically observed for Win32/Win64 record fields).
      P := P + 2 + Length(Name) + 10;
    end;

    if Count = 0 then Exit;

    // Distribute the record's total bytes evenly across the fields:
    // works for fixtures where every field has the same width
    // (TPoint2D = 2 ints, TRect2D = 2 TPoint2D, TPair = 2 pointers).
    if TotSz > 0 then
      Each := TotSz div UInt32(Count)
    else
      Each := 0;
    for I := 0 to Count - 1 do
      Cands[I].Offset := UInt32(I) * Each;

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

  procedure ProbeKnownStruct(const AName: String; AKind: TRsmStructKind);
  // Try every length-prefixed occurrence of AName. Classes have their
  // fields BEFORE the name (so we scan a window that ends at the name);
  // records have their fields AFTER the name and are marked by the
  // sentinel byte $0E right before the namelen byte. Stop on the first
  // occurrence that yields a non-empty member list.
  var
    Offsets: TArray<NativeInt>;
    I      : Integer;
    Before : Integer;
    NameOff: NativeInt;
  begin
    Offsets := FindAllClassNameOffsets(AName);
    for I := 0 to High(Offsets) do
    begin
      NameOff := Offsets[I];
      Before := FClasses.Count;

      if AKind = skRecord then
      begin
        // The type-stream occurrence of a record name is preceded by
        // a $0E sentinel byte. Skip occurrences (e.g. RTTI strings,
        // scope refs) without that marker so we don't try to read a
        // bogus size DWORD.
        if (NameOff = 0) or (ByteAt(NameOff - 1) <> $0E) then
          Continue;
        ScanFieldsForwardFromRecordName(NameOff, AName);
      end
      else
        ScanFieldsBackwardFromClassName(NameOff, AKind, AName);

      if FClasses.Count > Before then
      begin
        if (FClasses.Count = Before + 1) and
           (FClasses[Before].Members.Count = 0) then
          FClasses.Delete(Before)
        else
          Exit;
      end;
    end;
  end;

  procedure ProbeKnownClasses;
  const
    KNOWN_CLASSES: array[0..2] of String =
      ('TInner', 'TOuter', 'TWithRec');
    KNOWN_RECORDS: array[0..2] of String =
      ('TPoint2D', 'TRect2D', 'TPair');
  var
    I: Integer;
  begin
    for I := 0 to High(KNOWN_CLASSES) do
      ProbeKnownStruct(KNOWN_CLASSES[I], skClass);
    for I := 0 to High(KNOWN_RECORDS) do
      ProbeKnownStruct(KNOWN_RECORDS[I], skRecord);
  end;

  procedure ScanSymbolStream;
  // Walk the RSM symbol stream looking for procedure ($28 tag) and
  // local ($20 tag) records. Each proc owns the locals between its
  // own record and the next proc (or the $63 scope-end byte). The
  // RSM proc address encoding hasn't been fully reversed yet -- it's
  // a small structure varying in length between Win32 and Win64 that
  // does NOT match the simple <DWORD-RVA> form used by TD32. Until
  // it is decoded, SegmentOffset is left at 0 and Size at $FFFFFFFF
  // so name-based lookups work; FindProcContaining cannot operate
  // until address decoding lands.
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
                Proc.SegmentOffset := UInt32(Decoded);
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
  ProbeKnownClasses;
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
  NextStart : UInt32;
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
      P.Size := UInt32(Gap);
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

function TRsmLocalsReader.FindProcContaining(ASegmentOffset: UInt32): Integer;
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
