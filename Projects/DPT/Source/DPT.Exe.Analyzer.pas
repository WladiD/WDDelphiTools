// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Exe.Analyzer;

interface

uses

  System.Classes,
  System.SysUtils,

  mormot.core.collections,

  DPT.Dcu.Index,
  DPT.Dcu.Types,
  DPT.Exe.Types;

type

  /// <summary>
  ///   Analyzes a Delphi-built EXE: reads its PE header, finds a sibling
  ///   .map file when present, parses it for the linked-unit list, and
  ///   optionally cross-references unit names against a previously built
  ///   DcuIndex to attach DCU paths.
  /// </summary>
  TExeAnalyzer = class
  private
    class procedure ReadPEHeader(const AContent: TBytes;
      var AResult: TExeAnalysisResult);
    class function  SubsystemNameOf(AValue: Word): string;
    class procedure DetectAndPopulateUnits(const AContent: TBytes;
      var AResult: TExeAnalysisResult);
    class procedure PopulateUnitsFromMap(const AMapFilePath: string;
      var AResult: TExeAnalysisResult);
    class function  PopulateUnitsFromTD32(const AContent: TBytes;
      var AResult: TExeAnalysisResult): Boolean;
    class function  PopulateUnitsFromRttiScan(const AContent: TBytes;
      var AResult: TExeAnalysisResult): Boolean;
    class function  LooksLikeUnitName(const AName: string): Boolean;
  public
    class function Analyze(const AExePath: string): TExeAnalysisResult; overload;
    class function Analyze(const AExePath: string;
      const AIndex: TDcuIndex): TExeAnalysisResult; overload;

    /// <summary>
    ///   Walks the units of an existing analysis result and stamps each
    ///   <c>ResolvedDcu</c> with the path from the given DcuIndex when
    ///   the unit name has a match. Multi-platform indexes are honoured:
    ///   the resolver prefers an entry whose platform matches the EXE's
    ///   platform, falling back to any match if there is none.
    /// </summary>
    class procedure ResolveAgainstIndex(var AResult: TExeAnalysisResult;
      const AIndex: TDcuIndex);
  end;

implementation

uses

  System.DateUtils,
  System.IOUtils,

  JclTD32,

  DPT.MapFileParser;

const
  // PE / COFF constants
  IMAGE_DOS_SIGNATURE  = $5A4D;        // 'MZ'
  IMAGE_NT_SIGNATURE   = $00004550;    // 'PE\0\0'
  IMAGE_FILE_MACHINE_I386  = $014C;
  IMAGE_FILE_MACHINE_AMD64 = $8664;
  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $010B;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC = $020B;

function ReadUInt16LE(const ABytes: TBytes; AOfs: Integer): Word;
begin
  if (AOfs < 0) or (AOfs + 2 > Length(ABytes)) then
    Exit(0);
  Result := ABytes[AOfs] or (Word(ABytes[AOfs + 1]) shl 8);
end;

function ReadUInt32LE(const ABytes: TBytes; AOfs: Integer): UInt32;
begin
  if (AOfs < 0) or (AOfs + 4 > Length(ABytes)) then
    Exit(0);
  Result :=
    UInt32(ABytes[AOfs])           or
    (UInt32(ABytes[AOfs + 1]) shl  8) or
    (UInt32(ABytes[AOfs + 2]) shl 16) or
    (UInt32(ABytes[AOfs + 3]) shl 24);
end;

function ReadAnsiString(const ABytes: TBytes; AOfs, ALen: Integer): string;
var
  I    : Integer;
  Last : Integer;
  Buf  : TBytes;
begin
  Result := '';
  if (AOfs < 0) or (AOfs + ALen > Length(ABytes)) then
    Exit;
  Last := ALen;
  for I := 0 to ALen - 1 do
    if ABytes[AOfs + I] = 0 then
    begin
      Last := I;
      Break;
    end;
  if Last = 0 then
    Exit;
  SetLength(Buf, Last);
  Move(ABytes[AOfs], Buf[0], Last);
  Result := TEncoding.ASCII.GetString(Buf);
end;

{ TExeAnalyzer }

class function TExeAnalyzer.SubsystemNameOf(AValue: Word): string;
begin
  case AValue of
    1 : Result := 'Native';
    2 : Result := 'GUI';
    3 : Result := 'Console';
    5 : Result := 'OS/2 CUI';
    7 : Result := 'POSIX';
    9 : Result := 'WindowsCE GUI';
    10: Result := 'EFI Application';
    11: Result := 'EFI Boot Service Driver';
    12: Result := 'EFI Runtime Driver';
    13: Result := 'EFI ROM';
    14: Result := 'XBOX';
  else
    Result := 'Unknown(' + IntToStr(AValue) + ')';
  end;
end;

class procedure TExeAnalyzer.ReadPEHeader(const AContent: TBytes;
  var AResult: TExeAnalysisResult);
var
  Machine        : Word;
  NumSections    : Word;
  OptHdrMagic    : Word;
  OptHdrSize     : Word;
  PEOfs          : Integer;
  SecOfs         : Integer;
  SectionName    : string;
  SectionRawSz   : UInt32;
  SectionVirtSz  : UInt32;
  Subsystem      : Word;
  SubsystemOfs   : Integer;
  Timestamp      : UInt32;
begin
  AResult.IsPE := False;
  AResult.Sections := Collections.NewPlainList<TExePeSection>;

  if Length(AContent) < 64 then
  begin
    AResult.Diagnostics.Add('File too small for a PE image');
    Exit;
  end;
  if ReadUInt16LE(AContent, 0) <> IMAGE_DOS_SIGNATURE then
  begin
    AResult.Diagnostics.Add('DOS signature MZ not found');
    Exit;
  end;

  PEOfs := ReadUInt32LE(AContent, $3C);
  if (PEOfs <= 0) or (PEOfs + 24 > Length(AContent)) then
  begin
    AResult.Diagnostics.Add('e_lfanew points outside the file');
    Exit;
  end;
  if ReadUInt32LE(AContent, PEOfs) <> IMAGE_NT_SIGNATURE then
  begin
    AResult.Diagnostics.Add('PE signature not found at e_lfanew');
    Exit;
  end;

  AResult.IsPE := True;

  // COFF file header at PEOfs+4
  Machine     := ReadUInt16LE(AContent, PEOfs +  4);
  NumSections := ReadUInt16LE(AContent, PEOfs +  6);
  Timestamp   := ReadUInt32LE(AContent, PEOfs +  8);
  OptHdrSize  := ReadUInt16LE(AContent, PEOfs + 20);
  // Characteristics: PEOfs + 22 (not used)

  AResult.Compiled := UnixToDateTime(Timestamp, False);

  case Machine of
    IMAGE_FILE_MACHINE_I386 : AResult.Platform := dpWin32;
    IMAGE_FILE_MACHINE_AMD64: AResult.Platform := dpWin64;
  else
    AResult.Platform := dpUnknown;
    AResult.Diagnostics.Add(System.SysUtils.Format(
      'Unknown PE machine code $%.4x', [Machine]));
  end;

  // Optional header starts at PEOfs + 24
  OptHdrMagic := ReadUInt16LE(AContent, PEOfs + 24);
  // Subsystem field offset relative to optional header start:
  //   PE32  -> 68  (image-base 32-bit)
  //   PE32+ -> 68  (image-base 64-bit but Subsystem still at 68 in PE32+)
  // Actually in PE32+ Subsystem is at offset 68 too (image base widens earlier).
  case OptHdrMagic of
    IMAGE_NT_OPTIONAL_HDR32_MAGIC: SubsystemOfs := PEOfs + 24 + 68;
    IMAGE_NT_OPTIONAL_HDR64_MAGIC: SubsystemOfs := PEOfs + 24 + 68;
  else
    SubsystemOfs := -1;
  end;
  if SubsystemOfs > 0 then
  begin
    Subsystem := ReadUInt16LE(AContent, SubsystemOfs);
    AResult.SubsystemName := SubsystemNameOf(Subsystem);
  end
  else
    AResult.SubsystemName := 'Unknown';

  // Section table follows the optional header.
  SecOfs := PEOfs + 24 + OptHdrSize;
  for var I := 0 to Integer(NumSections) - 1 do
  begin
    if SecOfs + 40 > Length(AContent) then
      Break;
    SectionName := ReadAnsiString(AContent, SecOfs, 8);
    SectionVirtSz := ReadUInt32LE(AContent, SecOfs + 8);
    SectionRawSz := ReadUInt32LE(AContent, SecOfs + 16);
    AResult.Sections.Add(
      TExePeSection.Create(SectionName, SectionVirtSz, SectionRawSz));
    Inc(SecOfs, 40);
  end;
end;

class procedure TExeAnalyzer.PopulateUnitsFromMap(const AMapFilePath: string;
  var AResult: TExeAnalysisResult);
var
  AddedNames: IKeyValue<string, Boolean>;
  Idx       : Integer;
  LineEntry : TMapLineNumber;
  Parser    : TMapFileParser;
  Segment   : TMapSegment;
  SegIdx    : Integer;
  SegSize   : UInt64;
begin
  Parser := TMapFileParser.Create(AMapFilePath);
  try
    if Parser.LineNumbersCnt = 0 then
    begin
      AResult.Diagnostics.Add('Map file produced no line-number entries: '
        + AMapFilePath);
      Exit;
    end;

    AddedNames := Collections.NewPlainKeyValue<string, Boolean>;

    // Prefer the segment table because it gives us per-unit code size.
    // Some old map formats omit segments; in that case we fall back to
    // iterating the line numbers and synthesise zero-sized entries.
    if Parser.SegmentsCnt > 0 then
    begin
      // A single unit typically owns several segments (code, data,
      // BSS, TLS, ...). Aggregate them per unit: lowest StartVA across
      // all segments tells us where the unit "lives", summed sizes
      // tell us how many bytes total it contributes to the image.
      var AggIndex := Collections.NewPlainKeyValue<string, Integer>;
      for SegIdx := 0 to Parser.SegmentsCnt - 1 do
      begin
        Segment := Parser.SegmentByIndex[SegIdx];
        if Segment.UnitName = '' then
          Continue;
        if Segment.EndVA > Segment.StartVA then
          SegSize := Segment.EndVA - Segment.StartVA
        else
          SegSize := 0;
        var LowName := LowerCase(Segment.UnitName);
        var ExistingIdx: Integer;
        if AggIndex.TryGetValue(LowName, ExistingIdx) then
        begin
          var Existing := AResult.Units[ExistingIdx];
          if Segment.StartVA < Existing.StartVA then
            Existing.StartVA := Segment.StartVA;
          Inc(Existing.CodeSize, SegSize);
          AResult.Units[ExistingIdx] := Existing;
        end
        else
        begin
          AResult.Units.Add(
            TExeUnitEntry.Create(Segment.UnitName, Segment.StartVA, SegSize));
          AggIndex.Add(LowName, AResult.Units.Count - 1);
          AddedNames.Add(LowName, True);
        end;
      end;
    end
    else
    begin
      for Idx := 0 to Parser.LineNumbersCnt - 1 do
      begin
        LineEntry := Parser.LineNumberByIndex[Idx];
        if LineEntry.UnitName = '' then
          Continue;
        if AddedNames.ContainsKey(LowerCase(LineEntry.UnitName)) then
          Continue;
        AResult.Units.Add(TExeUnitEntry.Create(LineEntry.UnitName, 0, 0));
        AddedNames.Add(LowerCase(LineEntry.UnitName), True);
      end;
    end;
  finally
    Parser.Free;
  end;
end;

class function TExeAnalyzer.LooksLikeUnitName(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Length(AName) < 3) or (Length(AName) > 200) then
    Exit;
  // First char must be a letter or underscore.
  case AName[1] of
    'A'..'Z', 'a'..'z', '_': ;
  else
    Exit;
  end;
  // Every byte: letter, digit, dot, underscore. No double dots, no
  // trailing dot.
  for I := 1 to Length(AName) do
    case AName[I] of
      'A'..'Z', 'a'..'z', '0'..'9', '_', '.': ;
    else
      Exit;
    end;
  if AName[Length(AName)] = '.' then Exit;
  for I := 1 to Length(AName) - 1 do
    if (AName[I] = '.') and (AName[I + 1] = '.') then Exit;
  Result := True;
end;

class function TExeAnalyzer.PopulateUnitsFromTD32(const AContent: TBytes;
  var AResult: TExeAnalysisResult): Boolean;
var
  AddedNames: IKeyValue<string, Boolean>;
  I         : Integer;
  ModuleName: string;
  Parser    : TJclTD32InfoParser;
  Sig       : UInt32;
  Stream    : TBytesStream;
  TD32Bytes : TBytes;
  TD32Start : Integer;

  function FindTd32Start: Integer;
  // Scan forward for the FB09 / FB0A signature on a 4-byte boundary.
  // The header sits at the very start of the TD32 stream; the same
  // signature appears again in the trailer at EOF. We accept the
  // earliest hit because that is the header. The scan starts after
  // the PE optional header (the magic bytes never appear that early
  // in legitimate Delphi binaries) to skip the obvious noise of the
  // DOS stub.
  var
    Idx   : Integer;
    Probe : UInt32;
    Sig09 : UInt32;
    Sig0A : UInt32;
  begin
    Result := -1;
    Sig09 := $39304246; // 'FB09' little-endian DWORD value
    Sig0A := $41304246; // 'FB0A'
    Idx := 256;         // skip DOS stub + early PE header noise
    while Idx + 8 <= Length(AContent) do
    begin
      Probe :=
        UInt32(AContent[Idx])           or
        (UInt32(AContent[Idx + 1]) shl  8) or
        (UInt32(AContent[Idx + 2]) shl 16) or
        (UInt32(AContent[Idx + 3]) shl 24);
      if (Probe = Sig09) or (Probe = Sig0A) then
        Exit(Idx);
      Inc(Idx, 4);
    end;
  end;

begin
  Result := False;
  TD32Start := FindTd32Start;
  if TD32Start < 0 then
  begin
    AResult.Diagnostics.Add('No FB09/FB0A signature found - no TD32 debug info');
    Exit;
  end;

  // Validate: signature must be followed by a plausible Offset that
  // points within the remaining bytes. The 8-byte trailer at EOF has
  // the same signature, so accepting only the FIRST hit guarantees we
  // get the header.
  Sig :=
    UInt32(AContent[TD32Start])           or
    (UInt32(AContent[TD32Start + 1]) shl  8) or
    (UInt32(AContent[TD32Start + 2]) shl 16) or
    (UInt32(AContent[TD32Start + 3]) shl 24);
  if not ((Sig = $39304246) or (Sig = $41304246)) then
    Exit;

  SetLength(TD32Bytes, Length(AContent) - TD32Start);
  if Length(TD32Bytes) > 0 then
    Move(AContent[TD32Start], TD32Bytes[0], Length(TD32Bytes));

  Stream := TBytesStream.Create(TD32Bytes);
  Parser := nil;
  try
    if not TJclTD32InfoParser.IsTD32DebugInfoValid(Stream.Memory, Stream.Size) then
    begin
      AResult.Diagnostics.Add('TD32 signature located but JCL parser ' +
        'rejects the data layout');
      Exit;
    end;
    Parser := TJclTD32InfoParser.Create(Stream);
    if not Parser.ValidData then
    begin
      AResult.Diagnostics.Add('TD32 parser ran but reports invalid data');
      Exit;
    end;

    AddedNames := Collections.NewPlainKeyValue<string, Boolean>;
    for I := 0 to Parser.ModuleCount - 1 do
    begin
      ModuleName := Parser.Names[Parser.Modules[I].NameIndex];
      if (ModuleName = '') or
         AddedNames.ContainsKey(LowerCase(ModuleName)) then
        Continue;
      AResult.Units.Add(TExeUnitEntry.Create(ModuleName, 0, 0));
      AddedNames.Add(LowerCase(ModuleName), True);
    end;
    Result := AResult.Units.Count > 0;
  finally
    Parser.Free;
    Stream.Free;
  end;
end;

class function TExeAnalyzer.PopulateUnitsFromRttiScan(const AContent: TBytes;
  var AResult: TExeAnalysisResult): Boolean;
// Last-resort scan: walk the file looking for length-prefixed ASCII
// short-strings whose payload looks like a Pascal qualified identifier.
// Two boundary checks suppress the dominant false-positive class
// (length bytes coincidentally embedded inside other strings - in
// particular generic-instantiation names where '<' = $3C looks like
// a 60-byte length):
//   * the byte BEFORE the candidate length must not be an identifier
//     character; real length bytes are preceded by a structure tag,
//     padding or a non-textual byte
//   * the byte AFTER the captured string must not continue the
//     identifier; otherwise we are truncating a longer string
// Dotted names only - undotted ones (SysUtils, JclMath) are
// indistinguishable from class/type names without proper RTTI parsing.

  function IsIdentChar(AByte: Byte): Boolean; inline;
  begin
    Result :=
      ((AByte >= Ord('A')) and (AByte <= Ord('Z'))) or
      ((AByte >= Ord('a')) and (AByte <= Ord('z'))) or
      ((AByte >= Ord('0')) and (AByte <= Ord('9'))) or
      (AByte = Ord('_')) or (AByte = Ord('.'));
  end;

var
  AddedNames: IKeyValue<string, Boolean>;
  Buf       : TBytes;
  Candidate : string;
  Len       : Integer;
  Offset    : Integer;
begin
  AddedNames := Collections.NewPlainKeyValue<string, Boolean>;
  Offset := 0;
  while Offset < Length(AContent) - 1 do
  begin
    Len := AContent[Offset];
    if (Len < 4) or (Len > 100)
      or (Offset + 1 + Len > Length(AContent))
    then
    begin
      Inc(Offset);
      Continue;
    end;

    // Boundary check 1: byte before the length must not look like an
    // identifier byte - we would otherwise sit inside another string.
    if (Offset > 0) and IsIdentChar(AContent[Offset - 1]) then
    begin
      Inc(Offset);
      Continue;
    end;

    // Boundary check 2: byte after the captured string must not
    // continue the identifier - otherwise we are reading a truncated
    // prefix of a longer string.
    if (Offset + 1 + Len < Length(AContent))
      and IsIdentChar(AContent[Offset + 1 + Len])
    then
    begin
      Inc(Offset);
      Continue;
    end;

    SetLength(Buf, Len);
    Move(AContent[Offset + 1], Buf[0], Len);
    try
      Candidate := TEncoding.ASCII.GetString(Buf);
    except
      Inc(Offset);
      Continue;
    end;
    if not LooksLikeUnitName(Candidate) then
    begin
      Inc(Offset);
      Continue;
    end;
    if Pos('.', Candidate) = 0 then
    begin
      Inc(Offset);
      Continue;
    end;
    // Class-prefix filter: real Delphi unit names virtually never start
    // with the Pascal class-name pattern T<UpperCase>. Names like
    // "TMask.TMaskSet" or "TDirectory.TEnumerator" are nested class /
    // type references, not units. Discarding them improves the
    // signal-to-noise ratio of the heuristic dramatically. Edge case:
    // the Taifun-style unit name "TaifunFormat" survives because its
    // second char is lowercase.
    if (Length(Candidate) >= 2) and (Candidate[1] = 'T')
       and CharInSet(Candidate[2], ['A'..'Z']) then
    begin
      Inc(Offset);
      Continue;
    end;
    if not AddedNames.ContainsKey(LowerCase(Candidate)) then
    begin
      AddedNames.Add(LowerCase(Candidate), True);
      AResult.Units.Add(TExeUnitEntry.Create(Candidate, 0, 0));
    end;
    Inc(Offset, 1 + Len);
  end;
  Result := AResult.Units.Count > 0;
end;

class procedure TExeAnalyzer.DetectAndPopulateUnits(const AContent: TBytes;
  var AResult: TExeAnalysisResult);
var
  MapPath: string;
begin
  AResult.Detection := edmNone;
  AResult.DetectionDetail := '';

  // Strategy 1 (most reliable): a sibling .map file alongside the EXE.
  MapPath := ChangeFileExt(AResult.FilePath, '.map');
  if FileExists(MapPath) then
  begin
    AResult.MapFilePath := MapPath;
    PopulateUnitsFromMap(MapPath, AResult);
    AResult.Detection := edmMap;
    AResult.DetectionDetail := '.map file (' + ExtractFileName(MapPath)
      + ', ' + IntToStr(AResult.Units.Count) + ' units)';
    Exit;
  end;

  // Strategy 2: TD32 (FB09) debug info embedded in the EXE.
  if PopulateUnitsFromTD32(AContent, AResult) then
  begin
    AResult.Detection := edmTd32;
    AResult.DetectionDetail := 'embedded TD32 debug info ('
      + IntToStr(AResult.Units.Count) + ' units)';
    Exit;
  end;

  // Strategy 3 (last resort): heuristic identifier scan.
  if PopulateUnitsFromRttiScan(AContent, AResult) then
  begin
    AResult.Detection := edmRtti;
    AResult.DetectionDetail := 'heuristic identifier scan ('
      + IntToStr(AResult.Units.Count) + ' candidates - dotted names only)';
    Exit;
  end;

  AResult.DetectionDetail := 'No .map, no TD32 debug info, no plausible '
    + 'identifier candidates - unit list cannot be reconstructed';
  AResult.Diagnostics.Add(AResult.DetectionDetail);
end;

class procedure TExeAnalyzer.ResolveAgainstIndex(
  var AResult: TExeAnalysisResult; const AIndex: TDcuIndex);
var
  Entry      : TDcuIndexEntry;
  ExePlatform: TDcuPlatform;
  I          : Integer;
  Match      : TDcuIndexEntry;
  MatchFound : Boolean;
  MatchExact : Boolean;
  Unit_      : TExeUnitEntry;
begin
  if AIndex.Units = nil then
    Exit;
  ExePlatform := AResult.Platform;

  for I := 0 to AResult.Units.Count - 1 do
  begin
    Unit_ := AResult.Units[I];
    MatchFound := False;
    MatchExact := False;
    for Entry in AIndex.Units do
      if SameText(Entry.UnitName, Unit_.Name) then
      begin
        if Entry.Platform = ExePlatform then
        begin
          Match := Entry;
          MatchFound := True;
          MatchExact := True;
          Break;
        end
        else if not MatchFound then
        begin
          Match := Entry;
          MatchFound := True;
        end;
      end;
    if MatchFound then
    begin
      Unit_.ResolvedDcu := Match.FilePath;
      AResult.Units[I] := Unit_;
      if not MatchExact then
        AResult.Diagnostics.Add(System.SysUtils.Format(
          'Unit %s: index has %s but EXE is %s - resolved by name only',
          [Unit_.Name, DcuPlatformName[Match.Platform],
           DcuPlatformName[ExePlatform]]));
    end;
  end;
end;

class function TExeAnalyzer.Analyze(const AExePath: string): TExeAnalysisResult;
var
  Bytes : TBytes;
  Stream: TFileStream;
begin
  Result := Default(TExeAnalysisResult);
  Result.FilePath := AExePath;
  Result.Sections := Collections.NewPlainList<TExePeSection>;
  Result.Units := Collections.NewPlainList<TExeUnitEntry>;
  Result.Diagnostics := Collections.NewList<string>;

  if not FileExists(AExePath) then
  begin
    Result.Diagnostics.Add('File not found: ' + AExePath);
    Exit;
  end;

  Stream := TFileStream.Create(AExePath, fmOpenRead or fmShareDenyWrite);
  try
    Result.FileSize := Stream.Size;
    // The header parser needs only the first ~4 KB; the TD32 / RTTI
    // fallbacks need the whole file. We pull everything because the
    // file size is bounded (a multi-hundred-MB EXE is unrealistic for
    // Delphi) and the alternative - two passes with a stream rewind -
    // adds complexity without saving meaningful memory.
    SetLength(Bytes, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Bytes[0], Stream.Size);
  finally
    Stream.Free;
  end;

  ReadPEHeader(Bytes, Result);
  if not Result.IsPE then
    Exit;

  DetectAndPopulateUnits(Bytes, Result);
end;

class function TExeAnalyzer.Analyze(const AExePath: string;
  const AIndex: TDcuIndex): TExeAnalysisResult;
begin
  Result := Analyze(AExePath);
  if Result.IsPE then
    ResolveAgainstIndex(Result, AIndex);
end;

end.
