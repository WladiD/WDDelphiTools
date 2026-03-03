// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.DProj.Task;

interface

uses

  DPT.DProjAnalyzer,
  DPT.Task,
  DPT.Types;

type

  TDptDProjTaskBase = class(TDptTaskBase)
  protected
    FAnalyzer: TDProjAnalyzer;
  public
    ProjectFile: String;
    destructor Destroy; override;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
  end;

  TDptDProjConfigPlatformTaskBase = class(TDptDProjTaskBase)
  public
    Config     : String;
    Platform   : String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
  end;

  TDptDProjPrintConfigsTask = class(TDptDProjTaskBase)
  public
    procedure Execute; override;
  end;

  TDptDProjPrintCurConfigTask = class(TDptDProjTaskBase)
  public
    procedure Execute; override;
  end;

  TDptDProjPrintOutputFileTask = class(TDptDProjConfigPlatformTaskBase)
  public
    procedure Execute; override;
  end;

  TDptDProjPrintSearchPathsTask = class(TDptDProjConfigPlatformTaskBase)
  public
    procedure Execute; override;
  end;

implementation

uses

  System.SysUtils,

  JclIDEUtils,

  DPT.Utils;

{ TDptDProjTaskBase }

destructor TDptDProjTaskBase.Destroy;
begin
  FAnalyzer.Free;
  inherited;
end;

procedure TDptDProjTaskBase.Parse(CmdLine: TCmdLineConsumer);
begin
  ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
  CheckAndExecutePreProcessor(ProjectFile);
  FAnalyzer := TDProjAnalyzer.Create(ProjectFile);
  CmdLine.ConsumeParameter;
end;

{ TDptDProjConfigPlatformTaskBase }

procedure TDptDProjConfigPlatformTaskBase.Parse(CmdLine: TCmdLineConsumer);
begin
  inherited Parse(CmdLine);

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

{ TDptDProjPrintConfigsTask }

procedure TDptDProjPrintConfigsTask.Execute;
begin
  for var Config: String in FAnalyzer.GetConfigs do
    Writeln(Config);
end;

{ TDptDProjPrintCurConfigTask }

procedure TDptDProjPrintCurConfigTask.Execute;
begin
  Writeln(FAnalyzer.GetDefaultConfig);
end;

{ TDptDProjPrintOutputFileTask }

procedure TDptDProjPrintOutputFileTask.Execute;
begin
  if Config = '' then
    Config := FAnalyzer.GetDefaultConfig;
  if Platform = '' then
    Platform := 'Win32';

  Writeln(FAnalyzer.GetProjectOutputFile(Config, Platform));
end;

{ TDptDProjPrintSearchPathsTask }

procedure TDptDProjPrintSearchPathsTask.Execute;
var
  BDSPath : String;
  Full    : String;
  IdePath : String;
  ProjPath: String;
begin
  if Config = '' then
    Config := FAnalyzer.GetDefaultConfig;
  if Platform = '' then
    Platform := 'Win32';

  ProjPath := FAnalyzer.GetProjectSearchPath(Config, Platform);
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

  for var PathEntry: String in Full.Split([';'], TStringSplitOptions.ExcludeEmpty) do
    Writeln(PathEntry);
end;

end.
