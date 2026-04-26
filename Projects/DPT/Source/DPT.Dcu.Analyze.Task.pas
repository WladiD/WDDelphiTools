// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Analyze.Task;

interface

uses

  System.Classes,

  DPT.Dcu.Types,
  DPT.Task,
  DPT.Types;

type

  TDcuAnalyzeFormat = (dafText, dafJson);

  TDcuAnalyzeSection = (dasHeader, dasUses, dasSymbols, dasSections);
  TDcuAnalyzeSections = set of TDcuAnalyzeSection;

  TDptDcuAnalyzeTask = class(TDptTaskBase)
  private
    FDcuFile : string;
    FFormat  : TDcuAnalyzeFormat;
    FSections: TDcuAnalyzeSections;
    FVerbose : Boolean;
    procedure RenderJson(const AResult: TDcuAnalysisResult; ALines: TStrings);
    procedure RenderText(const AResult: TDcuAnalysisResult; ALines: TStrings);
  public
    constructor Create; override;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
    property DcuFile: string read FDcuFile write FDcuFile;
    property Format: TDcuAnalyzeFormat read FFormat write FFormat;
    property Sections: TDcuAnalyzeSections read FSections write FSections;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

uses

  System.StrUtils,
  System.SysUtils,

  DPT.Dcu.Analyzer;

const
  AllSections: TDcuAnalyzeSections = [dasHeader, dasUses, dasSymbols, dasSections];

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

{ TDptDcuAnalyzeTask }

constructor TDptDcuAnalyzeTask.Create;
begin
  inherited;
  FFormat := dafText;
  FSections := AllSections;
  FVerbose := False;
end;

procedure TDptDcuAnalyzeTask.Parse(CmdLine: TCmdLineConsumer);
var
  ExplicitSections: Boolean;
  Param           : string;
  ValueLower      : string;
begin
  FDcuFile := '';
  FFormat := dafText;
  FSections := [];
  ExplicitSections := False;

  while CmdLine.HasParameter do
  begin
    Param := CmdLine.CheckParameter('Option/DcuFile');

    if SameText(Param, '--Header') then
    begin
      FSections := FSections + [dasHeader];
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Uses') then
    begin
      FSections := FSections + [dasUses];
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Symbols') then
    begin
      FSections := FSections + [dasSymbols];
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Sections') then
    begin
      FSections := FSections + [dasSections];
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--All') then
    begin
      FSections := AllSections;
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--Format=', True) then
    begin
      ValueLower := LowerCase(Param.Substring(9).DeQuotedString('"'));
      if ValueLower = 'text' then
        FFormat := dafText
      else if ValueLower = 'json' then
        FFormat := dafJson
      else
        CmdLine.InvalidParameter('Unknown --Format value: ' + ValueLower);
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Verbose') then
    begin
      FVerbose := True;
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--') then
    begin
      CmdLine.InvalidParameter('Unknown option: ' + Param);
    end
    else
    begin
      if FDcuFile = '' then
      begin
        FDcuFile := ExpandFileName(Param);
        CmdLine.ConsumeParameter;
      end
      else
        CmdLine.InvalidParameter('Unexpected positional argument: ' + Param);
    end;
  end;

  if not ExplicitSections then
    FSections := AllSections;

  if FDcuFile = '' then
    raise EInvalidParameter.Create('DcuAnalyze requires <DcuFile> argument');
end;

procedure TDptDcuAnalyzeTask.RenderText(const AResult: TDcuAnalysisResult;
  ALines: TStrings);
var
  IncludeSrc: TDcuSourceRef;
  UsesEntry : TDcuUsesEntry;
begin
  ALines.Add('File:      ' + AResult.FilePath);
  ALines.Add('Size:      ' + IntToStr(AResult.FileSize) + ' bytes');
  if AResult.IsDcu then
    ALines.Add('IsDcu:     yes')
  else
    ALines.Add('IsDcu:     no');

  if dasHeader in FSections then
  begin
    ALines.Add('');
    ALines.Add('[Header]');
    ALines.Add('  Magic:    ' + AResult.Header.MagicHex);
    ALines.Add('  Compiler: ' + DcuKnownCompilerName[AResult.Header.DetectedCompiler]);
    if AResult.Header.UnitName <> '' then
      ALines.Add('  Unit:     ' + AResult.Header.UnitName);
    if AResult.Header.PrimarySource.FileName <> '' then
      ALines.Add('  Source:   ' + AResult.Header.PrimarySource.FileName);
    if AResult.Header.IncludeSources.Count > 0 then
    begin
      ALines.Add('  Includes (' + IntToStr(AResult.Header.IncludeSources.Count) + '):');
      for IncludeSrc in AResult.Header.IncludeSources do
        ALines.Add('    - ' + IncludeSrc.FileName);
    end;
    if FVerbose then
      ALines.Add('  Preview:  ' + AResult.FirstBytesPreview);
  end;

  if dasUses in FSections then
  begin
    ALines.Add('');
    ALines.Add('[Implicit System Reference]');
    if not AResult.UsesParsed then
      ALines.Add('  (not detected)')
    else
    begin
      for UsesEntry in AResult.InterfaceUses do
        ALines.Add(System.SysUtils.Format('  %-32s trailer=$%.8x',
          [UsesEntry.UnitName, UsesEntry.CRC]));
    end;
  end;

  if dasSymbols in FSections then
  begin
    ALines.Add('');
    ALines.Add('[Symbols]');
    ALines.Add('  (not implemented in iteration 1)');
  end;

  if dasSections in FSections then
  begin
    ALines.Add('');
    ALines.Add('[Sections]');
    ALines.Add('  (not implemented in iteration 1)');
  end;

  if (AResult.Diagnostics.Count > 0) and FVerbose then
  begin
    ALines.Add('');
    ALines.Add('[Diagnostics]');
    for var Diag in AResult.Diagnostics do
      ALines.Add('  ' + Diag);
  end;
end;

procedure TDptDcuAnalyzeTask.RenderJson(const AResult: TDcuAnalysisResult;
  ALines: TStrings);
var
  First     : Boolean;
  IncludeSrc: TDcuSourceRef;
  Sb        : TStringBuilder;
  UsesEntry : TDcuUsesEntry;

  procedure Add(const S: string);
  begin
    Sb.AppendLine(S);
  end;

begin
  Sb := TStringBuilder.Create;
  try
    Add('{');
    Add('  "file": ' + JsonStr(AResult.FilePath) + ',');
    Add('  "size": ' + IntToStr(AResult.FileSize) + ',');
    Add('  "isDcu": ' + IfThen(AResult.IsDcu, 'true', 'false') + ',');

    if dasHeader in FSections then
    begin
      Add('  "header": {');
      Add('    "magic": ' + JsonStr(AResult.Header.MagicHex) + ',');
      Add('    "compiler": ' + JsonStr(DcuKnownCompilerName[AResult.Header.DetectedCompiler]) + ',');
      Add('    "unit": ' + JsonStr(AResult.Header.UnitName) + ',');
      Add('    "source": ' + JsonStr(AResult.Header.PrimarySource.FileName) + ',');
      Sb.Append('    "includes": [');
      First := True;
      for IncludeSrc in AResult.Header.IncludeSources do
      begin
        if not First then Sb.Append(', ');
        Sb.Append(JsonStr(IncludeSrc.FileName));
        First := False;
      end;
      Add(']');
      Add('  },');
    end;

    if dasUses in FSections then
    begin
      Add('  "interfaceUses": {');
      Add('    "parsed": ' + IfThen(AResult.UsesParsed, 'true', 'false') + ',');
      Add('    "count": ' + IntToStr(AResult.InterfaceUses.Count) + ',');
      Add('    "entries": [');
      First := True;
      for UsesEntry in AResult.InterfaceUses do
      begin
        if not First then Sb.AppendLine(',');
        Sb.Append(System.SysUtils.Format('      { "unit": %s, "crc": "0x%.8x" }',
          [JsonStr(UsesEntry.UnitName), UsesEntry.CRC]));
        First := False;
      end;
      Sb.AppendLine;
      Add('    ]');
      Add('  },');
    end;

    if dasSymbols in FSections then
      Add('  "symbols": null,');

    if dasSections in FSections then
      Add('  "sections": null,');

    Sb.Append('  "diagnostics": [');
    First := True;
    for var Diag in AResult.Diagnostics do
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

procedure TDptDcuAnalyzeTask.Execute;
var
  Lines : TStringList;
  Result: TDcuAnalysisResult;
begin
  if not FileExists(FDcuFile) then
    raise EDptRuntimeError.Create('DCU file not found: ' + FDcuFile);

  Result := TDcuAnalyzer.Analyze(FDcuFile);
  Lines := TStringList.Create;
  try
    case FFormat of
      dafText: RenderText(Result, Lines);
      dafJson: RenderJson(Result, Lines);
    end;
    for var L in Lines do
      Writeln(L);
  finally
    Lines.Free;
  end;
end;

end.
