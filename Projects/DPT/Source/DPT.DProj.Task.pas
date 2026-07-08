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
var
  Arg: String;
begin
  inherited Parse(CmdLine);

  // Recognise Platform and Config by content, in any order - exactly like the
  // Build action: a Win32/Win64 token is the platform, anything else is the
  // configuration. Both stay empty when omitted so Execute applies the
  // per-task defaults.
  while CmdLine.HasParameter do
  begin
    Arg := CmdLine.CheckParameter('Platform/Config');
    if (Platform = '') and (SameText(Arg, 'Win32') or SameText(Arg, 'Win64')) then
      Platform := Arg
    else if Config = '' then
      Config := Arg
    else
      Break;
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
begin
  if Config = '' then
    Config := FAnalyzer.GetDefaultConfig;
  if Platform = '' then
    Platform := 'Win32';

  for var PathEntry: String in EnvOptions.EffectiveUnitSearchPath(FAnalyzer, Config, Platform) do
    Writeln(PathEntry);
end;

end.
