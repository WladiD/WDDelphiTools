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
  DPT.Types;

type

  TDptDProjPrintConfigsTask = class(TDptTaskBase)
  public
    ProjectFile: String;
    procedure Execute; override;
  end;

  TDptDProjPrintCurConfigTask = class(TDptTaskBase)
  public
    ProjectFile: String;
    procedure Execute; override;
  end;

  TDptDProjPrintSearchPathsTask = class(TDptTaskBase)
  public
    Config     : String;
    Platform   : String;
    ProjectFile: String;
    procedure Execute; override;
  end;

implementation

{ TDptDProjPrintConfigsTask }

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
