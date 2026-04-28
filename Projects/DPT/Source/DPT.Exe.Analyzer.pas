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
    class procedure DetectAndPopulateUnits(var AResult: TExeAnalysisResult);
    class procedure PopulateUnitsFromMap(const AMapFilePath: string;
      var AResult: TExeAnalysisResult);
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

class procedure TExeAnalyzer.DetectAndPopulateUnits(
  var AResult: TExeAnalysisResult);
var
  MapPath: string;
begin
  AResult.Detection := edmNone;
  AResult.DetectionDetail := '';

  // Probe for a sibling .map file - default Delphi convention is the
  // same base name with a .map extension.
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

  AResult.DetectionDetail := 'No .map file found alongside the EXE; '
    + 'TD32 and RTTI fallbacks are not yet implemented';
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
    // 4 KB are enough for the DOS stub + PE header + section table of
    // every realistic Delphi binary, so we do not pull the whole exe
    // into memory.
    SetLength(Bytes, 4096);
    var ReadBytes := Stream.Read(Bytes, Length(Bytes));
    if ReadBytes < Length(Bytes) then
      SetLength(Bytes, ReadBytes);
  finally
    Stream.Free;
  end;

  ReadPEHeader(Bytes, Result);
  if not Result.IsPE then
    Exit;

  DetectAndPopulateUnits(Result);
end;

class function TExeAnalyzer.Analyze(const AExePath: string;
  const AIndex: TDcuIndex): TExeAnalysisResult;
begin
  Result := Analyze(AExePath);
  if Result.IsPE then
    ResolveAgainstIndex(Result, AIndex);
end;

end.
