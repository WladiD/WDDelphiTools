// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Diff.Task;

interface

uses

  System.Classes,

  DPT.Dcu.Diff,
  DPT.Task,
  DPT.Types;

type

  TDcuDiffFormat = (ddfText, ddfJson);

  /// <summary>
  ///   CLI front-end for <see cref="DPT.Dcu.Diff.TDcuDiff"/>. Compares two
  ///   DCU files and reports the differences with a non-zero exit code
  ///   when any tracked field has changed - the standard CI integration
  ///   shape.
  ///   Exit codes: 0 = identical, 1 = different, 2 = error (missing file
  ///   or unreadable DCU).
  /// </summary>
  TDptDcuDiffTask = class(TDptTaskBase)
  private
    FFileA  : string;
    FFileB  : string;
    FFormat : TDcuDiffFormat;
    FQuiet  : Boolean;
    procedure RenderJson(const AReport: TDcuDiffReport; ALines: TStrings);
    procedure RenderText(const AReport: TDcuDiffReport; ALines: TStrings);
  public
    constructor Create; override;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
    property FileA: string read FFileA write FFileA;
    property FileB: string read FFileB write FFileB;
    property Format: TDcuDiffFormat read FFormat write FFormat;
    property Quiet: Boolean read FQuiet write FQuiet;
  end;

implementation

uses

  System.StrUtils,
  System.SysUtils,

  mormot.core.collections,

  DPT.Dcu.Analyzer,
  DPT.Dcu.Types;

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

{ TDptDcuDiffTask }

constructor TDptDcuDiffTask.Create;
begin
  inherited;
  FFormat := ddfText;
  FQuiet := False;
end;

procedure TDptDcuDiffTask.Parse(CmdLine: TCmdLineConsumer);
var
  Param     : string;
  ValueLower: string;
begin
  FFileA := '';
  FFileB := '';

  while CmdLine.HasParameter do
  begin
    Param := CmdLine.CheckParameter('Option/DcuFile');
    if Param.StartsWith('--Format=', True) then
    begin
      ValueLower := LowerCase(Param.Substring(9).DeQuotedString('"'));
      if ValueLower = 'text' then
        FFormat := ddfText
      else if ValueLower = 'json' then
        FFormat := ddfJson
      else
        CmdLine.InvalidParameter('Unknown --Format value: ' + ValueLower);
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Quiet') then
    begin
      FQuiet := True;
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--') then
    begin
      CmdLine.InvalidParameter('Unknown option: ' + Param);
    end
    else
    begin
      if FFileA = '' then
      begin
        FFileA := ExpandFileName(Param);
        CmdLine.ConsumeParameter;
      end
      else if FFileB = '' then
      begin
        FFileB := ExpandFileName(Param);
        CmdLine.ConsumeParameter;
      end
      else
        CmdLine.InvalidParameter('Unexpected positional argument: ' + Param);
    end;
  end;

  if FFileA = '' then
    raise EInvalidParameter.Create('DcuDiff requires <DcuFileA> argument');
  if FFileB = '' then
    raise EInvalidParameter.Create('DcuDiff requires <DcuFileB> argument');
end;

procedure TDptDcuDiffTask.RenderText(const AReport: TDcuDiffReport;
  ALines: TStrings);
begin
  ALines.Add('A:      ' + AReport.PathA);
  ALines.Add('B:      ' + AReport.PathB);
  ALines.Add('Status: ' + DcuDiffStatusName[AReport.Status]);

  if not SameText(AReport.UnitNameA, AReport.UnitNameB) then
    ALines.Add(System.SysUtils.Format('Unit:   %s -> %s',
      [AReport.UnitNameA, AReport.UnitNameB]));

  if AReport.HeaderChanges.Count > 0 then
  begin
    ALines.Add('');
    ALines.Add('[Header changes]');
    for var Line in AReport.HeaderChanges do
      ALines.Add('  ' + Line);
  end;

  if (AReport.IncludesAdded.Count > 0) or (AReport.IncludesRemoved.Count > 0) then
  begin
    ALines.Add('');
    ALines.Add('[Sources/Includes]');
    for var Item in AReport.IncludesRemoved do
      ALines.Add('  - ' + Item);
    for var Item in AReport.IncludesAdded do
      ALines.Add('  + ' + Item);
  end;

  if (AReport.InterfaceUsesAdded.Count > 0) or
     (AReport.InterfaceUsesRemoved.Count > 0) or
     AReport.InterfaceUsesOrderChange
  then
  begin
    ALines.Add('');
    ALines.Add('[Interface uses]');
    for var Item in AReport.InterfaceUsesRemoved do
      ALines.Add('  - ' + Item);
    for var Item in AReport.InterfaceUsesAdded do
      ALines.Add('  + ' + Item);
    if AReport.InterfaceUsesOrderChange then
      ALines.Add('  ! Order changed (same set, different sequence)');
  end;

  if (AReport.ImplUsesAdded.Count > 0) or
     (AReport.ImplUsesRemoved.Count > 0) or
     AReport.ImplUsesOrderChange
  then
  begin
    ALines.Add('');
    ALines.Add('[Implementation uses]');
    for var Item in AReport.ImplUsesRemoved do
      ALines.Add('  - ' + Item);
    for var Item in AReport.ImplUsesAdded do
      ALines.Add('  + ' + Item);
    if AReport.ImplUsesOrderChange then
      ALines.Add('  ! Order changed (same set, different sequence)');
  end;
end;

procedure TDptDcuDiffTask.RenderJson(const AReport: TDcuDiffReport;
  ALines: TStrings);
var
  Sb   : TStringBuilder;
  First: Boolean;

  procedure AddLine(const S: string);
  begin
    Sb.AppendLine(S);
  end;

  procedure AppendStringArray(AList: IList<string>);
  var
    Item: string;
  begin
    Sb.Append('[');
    First := True;
    for Item in AList do
    begin
      if not First then Sb.Append(', ');
      Sb.Append(JsonStr(Item));
      First := False;
    end;
    Sb.Append(']');
  end;

begin
  Sb := TStringBuilder.Create;
  try
    AddLine('{');
    AddLine('  "fileA": ' + JsonStr(AReport.PathA) + ',');
    AddLine('  "fileB": ' + JsonStr(AReport.PathB) + ',');
    AddLine('  "status": ' + JsonStr(DcuDiffStatusName[AReport.Status]) + ',');
    AddLine('  "unitNameA": ' + JsonStr(AReport.UnitNameA) + ',');
    AddLine('  "unitNameB": ' + JsonStr(AReport.UnitNameB) + ',');

    Sb.Append('  "headerChanges": ');
    AppendStringArray(AReport.HeaderChanges);
    AddLine(',');

    Sb.Append('  "includesAdded": ');
    AppendStringArray(AReport.IncludesAdded);
    AddLine(',');
    Sb.Append('  "includesRemoved": ');
    AppendStringArray(AReport.IncludesRemoved);
    AddLine(',');

    Sb.Append('  "interfaceUsesAdded": ');
    AppendStringArray(AReport.InterfaceUsesAdded);
    AddLine(',');
    Sb.Append('  "interfaceUsesRemoved": ');
    AppendStringArray(AReport.InterfaceUsesRemoved);
    AddLine(',');
    AddLine('  "interfaceUsesOrderChange": '
      + IfThen(AReport.InterfaceUsesOrderChange, 'true', 'false') + ',');

    Sb.Append('  "implementationUsesAdded": ');
    AppendStringArray(AReport.ImplUsesAdded);
    AddLine(',');
    Sb.Append('  "implementationUsesRemoved": ');
    AppendStringArray(AReport.ImplUsesRemoved);
    AddLine(',');
    AddLine('  "implementationUsesOrderChange": '
      + IfThen(AReport.ImplUsesOrderChange, 'true', 'false'));

    AddLine('}');
    ALines.Text := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

procedure TDptDcuDiffTask.Execute;
var
  Lines  : TStringList;
  ResA   : TDcuAnalysisResult;
  ResB   : TDcuAnalysisResult;
  Report : TDcuDiffReport;
begin
  if not FileExists(FFileA) then
  begin
    Writeln('ERROR: DCU file not found: ' + FFileA);
    System.ExitCode := 2;
    Exit;
  end;
  if not FileExists(FFileB) then
  begin
    Writeln('ERROR: DCU file not found: ' + FFileB);
    System.ExitCode := 2;
    Exit;
  end;

  ResA := TDcuAnalyzer.Analyze(FFileA);
  ResB := TDcuAnalyzer.Analyze(FFileB);
  Report := TDcuDiff.Compare(ResA, ResB);

  if not FQuiet then
  begin
    Lines := TStringList.Create;
    try
      case FFormat of
        ddfText: RenderText(Report, Lines);
        ddfJson: RenderJson(Report, Lines);
      end;
      for var L in Lines do
        Writeln(L);
    finally
      Lines.Free;
    end;
  end;

  if Report.Status = ddIdentical then
    System.ExitCode := 0
  else
    System.ExitCode := 1;
end;

end.
