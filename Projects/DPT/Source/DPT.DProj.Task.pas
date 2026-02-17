// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.DProj.Task;

interface

uses

  System.SysUtils,

  JclIDEUtils,

  DPT.DProjAnalyzer,
  DPT.Task,
  DPT.Types,
  DPT.Utils;

type

  TDptDProjPrintConfigsTask = class(TDptTaskBase)
  public
    ProjectFile: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

  TDptDProjPrintCurConfigTask = class(TDptTaskBase)
  public
    ProjectFile: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

  TDptDProjPrintSearchPathsTask = class(TDptTaskBase)
  public
    Config     : String;
    Platform   : String;
    ProjectFile: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

{ TDptDProjPrintConfigsTask }

procedure TDptDProjPrintConfigsTask.Parse(CmdLine: TCmdLineConsumer);
begin
  ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
  CheckAndExecutePreProcessor(ProjectFile);
  CmdLine.ConsumeParameter;
end;

procedure TDptDProjPrintConfigsTask.Execute;
var
  Analyzer: TDProjAnalyzer;
  Configs : TArray<String>;
  C       : String;
begin
  Analyzer := TDProjAnalyzer.Create(ProjectFile);
  try
    Configs := Analyzer.GetConfigs;
    for C in Configs do
      Writeln(C);
  finally
    Analyzer.Free;
  end;
end;

{ TDptDProjPrintCurConfigTask }

procedure TDptDProjPrintCurConfigTask.Parse(CmdLine: TCmdLineConsumer);
begin
  ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
  CheckAndExecutePreProcessor(ProjectFile);
  CmdLine.ConsumeParameter;
end;

procedure TDptDProjPrintCurConfigTask.Execute;
var
  Analyzer: TDProjAnalyzer;
begin
  Analyzer := TDProjAnalyzer.Create(ProjectFile);
  try
    Writeln(Analyzer.GetDefaultConfig);
  finally
    Analyzer.Free;
  end;
end;

{ TDptDProjPrintSearchPathsTask }

procedure TDptDProjPrintSearchPathsTask.Parse(CmdLine: TCmdLineConsumer);
begin
  ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
  CheckAndExecutePreProcessor(ProjectFile);
  CmdLine.ConsumeParameter;

  if CmdLine.HasParameter then
  begin
    Config := CmdLine.CheckParameter('Config');
    CmdLine.ConsumeParameter;
  end;

  if CmdLine.HasParameter then
  begin
    Platform := CmdLine.CheckParameter('Platform');
    CmdLine.ConsumeParameter;
  end;
end;

procedure TDptDProjPrintSearchPathsTask.Execute;
var
  Analyzer: TDProjAnalyzer;
  BDSPath : String;
  Full    : String;
  IdePath : String;
  ProjPath: String;
begin
  Analyzer := TDProjAnalyzer.Create(ProjectFile);
  try
    if Config = '' then
      Config := Analyzer.GetDefaultConfig;
    if Platform = '' then
      Platform := 'Win32';

    ProjPath := Analyzer.GetProjectSearchPath(Config, Platform);
    BDSPath := ExcludeTrailingPathDelimiter(Installation.RootDir);
    ProjPath := StringReplace(ProjPath, '$(BDS)', BDSPath, [rfReplaceAll, rfIgnoreCase]);
    
    if SameText(Platform, 'Win64') then
      IdePath := Installation.LibrarySearchPath[bpWin64]
    else
      IdePath := Installation.LibrarySearchPath[bpWin32];

    if (ProjPath <> '') and (IdePath <> '') then
      Full := IdePath + ';' + ProjPath
    else
      Full := IdePath + ProjPath;

    for var PathEntry in Full.Split([';'], TStringSplitOptions.ExcludeEmpty) do
      Writeln(PathEntry);
  finally
    Analyzer.Free;
  end;
end;

end.
