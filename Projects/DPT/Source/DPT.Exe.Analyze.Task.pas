// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Exe.Analyze.Task;

interface

uses

  System.Classes,

  mormot.core.collections,

  DPT.Exe.Types,
  DPT.Task,
  DPT.Types;

type

  TExeAnalyzeFormat = (eafText, eafJson);
  TExeUnitSortBy    = (esbVA, esbName, esbSize);

  TDptExeAnalyzeTask = class(TDptTaskBase)
  private
    FExeFile  : string;
    FFormat   : TExeAnalyzeFormat;
    FUnitsOnly: Boolean;
    FIndexFile: string;
    FSortBy   : TExeUnitSortBy;
    procedure RenderJson(ALines: TStrings);
    procedure RenderText(ALines: TStrings);
    procedure RenderUnitsOnly(ALines: TStrings);
  public
    /// <summary>
    ///   Sorts the unit list according to the given criterion. Exposed
    ///   as a class method so unit tests can exercise the comparators
    ///   without going through CLI parsing or the global Execute path.
    /// </summary>
    class procedure SortUnits(AUnits: mormot.core.collections.IList<TExeUnitEntry>;
      ABy: TExeUnitSortBy);
    constructor Create; override;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

uses

  System.DateUtils,
  System.StrUtils,
  System.SysUtils,

  DPT.Dcu.Index,
  DPT.Dcu.Index.Json,
  DPT.Dcu.Types,
  DPT.Exe.Analyzer;

var
  GResult: TExeAnalysisResult;

function JsonEscape(const S: string): string;
var
  Sb: TStringBuilder;
  Ch: Char;
begin
  Sb := TStringBuilder.Create(Length(S) + 2);
  try
    for Ch in S do
      case Ch of
        '"' : Sb.Append('\"');
        '\' : Sb.Append('\\');
        #8  : Sb.Append('\b');
        #9  : Sb.Append('\t');
        #10 : Sb.Append('\n');
        #12 : Sb.Append('\f');
        #13 : Sb.Append('\r');
      else
        if Ord(Ch) < 32 then
          Sb.Append(System.SysUtils.Format('\u%.4x', [Ord(Ch)]))
        else
          Sb.Append(Ch);
      end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

function JsonStr(const S: string): string;
begin
  Result := '"' + JsonEscape(S) + '"';
end;

function HumanSize(ABytes: UInt64): string;
const
  KB = 1024;
  MB = 1024 * 1024;
  GB = UInt64(1024) * 1024 * 1024;
begin
  if ABytes >= GB then
    Result := FormatFloat('#,##0.0 "GB"', ABytes / GB)
  else if ABytes >= MB then
    Result := FormatFloat('#,##0.0 "MB"', ABytes / MB)
  else if ABytes >= KB then
    Result := FormatFloat('#,##0.0 "KB"', ABytes / KB)
  else
    Result := FormatFloat('#,##0 "B"', ABytes);
end;

{ TDptExeAnalyzeTask }

constructor TDptExeAnalyzeTask.Create;
begin
  inherited;
  FFormat := eafText;
  FUnitsOnly := False;
  FIndexFile := '';
  FSortBy := esbVA;
end;

function CompareByVA(const A, B): Integer;
begin
  if TExeUnitEntry(A).StartVA < TExeUnitEntry(B).StartVA then Result := -1
  else if TExeUnitEntry(A).StartVA > TExeUnitEntry(B).StartVA then Result := 1
  else Result := 0;
end;

function CompareByName(const A, B): Integer;
begin
  Result := CompareText(TExeUnitEntry(A).Name, TExeUnitEntry(B).Name);
end;

function CompareBySize(const A, B): Integer;
begin
  // Descending: largest first - that's the actionable view ("who's
  // eating space?"). Equal sizes fall back to name for a stable
  // visual order.
  if TExeUnitEntry(A).CodeSize > TExeUnitEntry(B).CodeSize then Result := -1
  else if TExeUnitEntry(A).CodeSize < TExeUnitEntry(B).CodeSize then Result := 1
  else Result := CompareText(TExeUnitEntry(A).Name, TExeUnitEntry(B).Name);
end;

class procedure TDptExeAnalyzeTask.SortUnits(AUnits: IList<TExeUnitEntry>;
  ABy: TExeUnitSortBy);
begin
  if (AUnits = nil) or (AUnits.Count <= 1) then
    Exit;
  case ABy of
    esbVA  : AUnits.Sort(CompareByVA);
    esbName: AUnits.Sort(CompareByName);
    esbSize: AUnits.Sort(CompareBySize);
  end;
end;

procedure TDptExeAnalyzeTask.Parse(CmdLine: TCmdLineConsumer);
var
  Param     : string;
  ValueLower: string;
begin
  FExeFile := '';

  while CmdLine.HasParameter do
  begin
    Param := CmdLine.CheckParameter('Option/ExeFile');
    if Param.StartsWith('--Format=', True) then
    begin
      ValueLower := LowerCase(Param.Substring(9).DeQuotedString('"'));
      if ValueLower = 'text' then
        FFormat := eafText
      else if ValueLower = 'json' then
        FFormat := eafJson
      else
        CmdLine.InvalidParameter('Unknown --Format value: ' + ValueLower);
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--UnitsOnly') then
    begin
      FUnitsOnly := True;
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--Index=', True) then
    begin
      FIndexFile := ExpandFileName(Param.Substring(8).DeQuotedString('"'));
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--SortBy=', True) then
    begin
      ValueLower := LowerCase(Param.Substring(9).DeQuotedString('"'));
      if ValueLower = 'va' then
        FSortBy := esbVA
      else if ValueLower = 'name' then
        FSortBy := esbName
      else if ValueLower = 'size' then
        FSortBy := esbSize
      else
        CmdLine.InvalidParameter('Unknown --SortBy value: ' + ValueLower
          + ' (expected VA, Name, or Size)');
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--') then
      CmdLine.InvalidParameter('Unknown option: ' + Param)
    else
    begin
      if FExeFile = '' then
      begin
        FExeFile := ExpandFileName(Param);
        CmdLine.ConsumeParameter;
      end
      else
        CmdLine.InvalidParameter('Unexpected positional argument: ' + Param);
    end;
  end;

  if FExeFile = '' then
    raise EInvalidParameter.Create('ExeAnalyze requires <ExeFile> argument');
end;

procedure TDptExeAnalyzeTask.RenderUnitsOnly(ALines: TStrings);
var
  Entry: TExeUnitEntry;
begin
  for Entry in GResult.Units do
    ALines.Add(Entry.Name);
end;

procedure TDptExeAnalyzeTask.RenderText(ALines: TStrings);
var
  Entry      : TExeUnitEntry;
  Resolved   : Integer;
  Section    : TExePeSection;
  Unresolved : Integer;
begin
  ALines.Add('File:        ' + GResult.FilePath);
  ALines.Add('Size:        ' + FormatFloat('#,##0', GResult.FileSize) + ' bytes');
  if GResult.Compiled > 0 then
    ALines.Add('Compiled:    ' + FormatDateTime('yyyy-mm-dd hh:nn:ss',
      GResult.Compiled));

  ALines.Add('');
  ALines.Add('[Detection]');
  ALines.Add('  Method:    ' + TExeDetectionMethodName[GResult.Detection]);
  ALines.Add('  Detail:    ' + GResult.DetectionDetail);
  if GResult.MapFilePath <> '' then
    ALines.Add('  Map file:  ' + GResult.MapFilePath);

  if GResult.IsPE then
  begin
    ALines.Add('');
    ALines.Add('[Header]');
    ALines.Add('  Format:    ' + IfThen(GResult.Platform = dpWin64, 'PE32+', 'PE32'));
    ALines.Add('  Platform:  ' + DcuPlatformName[GResult.Platform]);
    ALines.Add('  Subsystem: ' + GResult.SubsystemName);
    if GResult.Sections.Count > 0 then
    begin
      ALines.Add('  Sections:');
      ALines.Add(System.SysUtils.Format('    %-10s %15s  %-12s %15s',
        ['Name', 'Virtual', '', 'Raw']));
      for Section in GResult.Sections do
        ALines.Add(System.SysUtils.Format('    %-10s %15s  %-12s %15s',
          [Section.Name,
           FormatFloat('#,##0', Section.VirtualSize),
           '(' + HumanSize(Section.VirtualSize) + ')',
           FormatFloat('#,##0', Section.RawSize)]));
    end;
  end;

  ALines.Add('');
  ALines.Add('[Linked units] (' + IntToStr(GResult.Units.Count) + ')');
  if GResult.Units.Count = 0 then
    ALines.Add('  (none detected)')
  else
  begin
    // Hex-width follows the EXE's bitness so Win32 stays compact while
    // Win64 still has the full 16-digit address.
    var HexWidth: Integer;
    if GResult.Platform = dpWin64 then
      HexWidth := 16
    else
      HexWidth := 8;
    var HexFmt := '$%.' + IntToStr(HexWidth) + 'x';

    ALines.Add(System.SysUtils.Format('  %-*s  %12s  %s',
      [HexWidth + 1, 'VA', 'CodeSize', 'Unit']));

    for Entry in GResult.Units do
    begin
      var VAStr := System.SysUtils.Format(HexFmt, [Entry.StartVA]);
      var SizeStr: string;
      if Entry.CodeSize > 0 then
        SizeStr := FormatFloat('#,##0', Entry.CodeSize)
      else
        SizeStr := '-';
      var Line := System.SysUtils.Format('  %-*s  %12s  %s',
        [HexWidth + 1, VAStr, SizeStr, Entry.Name]);
      if FIndexFile <> '' then
      begin
        if Entry.ResolvedDcu <> '' then
          Line := Line + '  -> ' + Entry.ResolvedDcu
        else
          Line := Line + '  -> (unresolved)';
      end;
      ALines.Add(Line);
    end;
  end;

  if FIndexFile <> '' then
  begin
    Resolved := 0;
    Unresolved := 0;
    for Entry in GResult.Units do
      if Entry.ResolvedDcu <> '' then Inc(Resolved) else Inc(Unresolved);
    ALines.Add('');
    ALines.Add('[Resolution against index]');
    ALines.Add('  Index file: ' + FIndexFile);
    ALines.Add(System.SysUtils.Format('  Resolved:   %d / %d',
      [Resolved, GResult.Units.Count]));
    ALines.Add(System.SysUtils.Format('  Unresolved: %d', [Unresolved]));
  end;

  if GResult.Diagnostics.Count > 0 then
  begin
    ALines.Add('');
    ALines.Add('[Diagnostics]');
    for var Diag in GResult.Diagnostics do
      ALines.Add('  ' + Diag);
  end;
end;

procedure TDptExeAnalyzeTask.RenderJson(ALines: TStrings);
var
  Entry  : TExeUnitEntry;
  First  : Boolean;
  Sb     : TStringBuilder;
  Section: TExePeSection;

  procedure Add(const S: string);
  begin
    Sb.AppendLine(S);
  end;

begin
  Sb := TStringBuilder.Create;
  try
    Add('{');
    Add('  "file": ' + JsonStr(GResult.FilePath) + ',');
    Add('  "size": ' + IntToStr(GResult.FileSize) + ',');
    if GResult.Compiled > 0 then
      Add('  "compiled": ' + JsonStr(DateToISO8601(GResult.Compiled, True)) + ',')
    else
      Add('  "compiled": null,');

    Add('  "detection": {');
    Add('    "method": ' + JsonStr(TExeDetectionMethodName[GResult.Detection]) + ',');
    Add('    "detail": ' + JsonStr(GResult.DetectionDetail) + ',');
    Add('    "mapFile": ' + IfThen(GResult.MapFilePath = '', 'null',
      JsonStr(GResult.MapFilePath)));
    Add('  },');

    Add('  "header": {');
    Add('    "isPE": ' + IfThen(GResult.IsPE, 'true', 'false') + ',');
    Add('    "platform": ' + JsonStr(DcuPlatformName[GResult.Platform]) + ',');
    Add('    "subsystem": ' + JsonStr(GResult.SubsystemName) + ',');
    Sb.Append('    "sections": [');
    First := True;
    for Section in GResult.Sections do
    begin
      if not First then Sb.Append(', ');
      Sb.Append(System.SysUtils.Format(
        '{"name": %s, "virtualSize": %d, "rawSize": %d}',
        [JsonStr(Section.Name), Section.VirtualSize, Section.RawSize]));
      First := False;
    end;
    Add(']');
    Add('  },');

    Sb.Append('  "units": [');
    First := True;
    for Entry in GResult.Units do
    begin
      if not First then Sb.Append(', ');
      if FIndexFile <> '' then
        Sb.Append(System.SysUtils.Format(
          '{"name": %s, "startVA": %d, "codeSize": %d, "resolvedDcu": %s}',
          [JsonStr(Entry.Name), Entry.StartVA, Entry.CodeSize,
           IfThen(Entry.ResolvedDcu = '', 'null', JsonStr(Entry.ResolvedDcu))]))
      else
        Sb.Append(System.SysUtils.Format(
          '{"name": %s, "startVA": %d, "codeSize": %d}',
          [JsonStr(Entry.Name), Entry.StartVA, Entry.CodeSize]));
      First := False;
    end;
    Add('],');

    Sb.Append('  "diagnostics": [');
    First := True;
    for var Diag in GResult.Diagnostics do
    begin
      if not First then Sb.Append(', ');
      Sb.Append(JsonStr(Diag));
      First := False;
    end;
    Add(']');

    Add('}');
    ALines.Text := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

procedure TDptExeAnalyzeTask.Execute;
var
  Index: TDcuIndex;
  Lines: TStringList;
begin
  if FIndexFile <> '' then
  begin
    if not FileExists(FIndexFile) then
      raise EDptRuntimeError.Create('Index file not found: ' + FIndexFile);
    Index := TDcuIndexJsonReader.LoadFromFile(FIndexFile);
    GResult := TExeAnalyzer.Analyze(FExeFile, Index);
  end
  else
    GResult := TExeAnalyzer.Analyze(FExeFile);

  SortUnits(GResult.Units, FSortBy);

  Lines := TStringList.Create;
  try
    if FUnitsOnly then
      RenderUnitsOnly(Lines)
    else
      case FFormat of
        eafText: RenderText(Lines);
        eafJson: RenderJson(Lines);
      end;
    for var L in Lines do
      Writeln(L);
  finally
    Lines.Free;
  end;
end;

end.
