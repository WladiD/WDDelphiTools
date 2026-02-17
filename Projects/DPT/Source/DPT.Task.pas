// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Task;

interface

uses

  System.Classes,
  System.SysUtils,

  JclIDEUtils,

  DPT.Detection,
  DPT.Logger,
  DPT.Types;

type

  TDptTaskBase = class
  private
    FInstallation  : TJclBorRADToolInstallation;
    FInstallations : TJclBorRADToolInstallations;
    FLogger        : ILogger;
    FWorkflowEngine: TObject;
  protected
    function  Installation: TJclBorRADToolInstallation;
    procedure Writeln(const Text: String = ''); virtual;
  public
    DelphiVersion: TDelphiVersion;
    IsX64: Boolean;
    property Logger: ILogger read FLogger write FLogger;
    property WorkflowEngine: TObject read FWorkflowEngine write FWorkflowEngine;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Parse(CmdLine: TCmdLineConsumer); virtual;
    procedure Execute; virtual; abstract;
  end;

  TDptTaskClass = class of TDptTaskBase;

implementation

{ TDptTaskBase }

constructor TDptTaskBase.Create;
begin
  FLogger := TConsoleLogger.Create;
end;

destructor TDptTaskBase.Destroy;
begin
  FInstallations.Free;
  inherited Destroy;
end;

procedure TDptTaskBase.Parse(CmdLine: TCmdLineConsumer);
begin
  // Default implementation does nothing
end;

function TDptTaskBase.Installation: TJclBorRADToolInstallation;
var
  DelphiVersionAsInt: Integer;
begin
  if Assigned(FInstallation) then
  begin
    Result := FInstallation;
    Exit;
  end;

  if not Assigned(FInstallations) then
    FInstallations := TJclBorRADToolInstallations.Create;

  DelphiVersionAsInt := DelphiVersionIntegerArray[DelphiVersion];

  if not FInstallations.DelphiVersionInstalled[DelphiVersionAsInt] then
    raise Exception.CreateFmt('Delphi Version %s (%d) not installed',
      [DelphiVersionStringArray[DelphiVersion], DelphiVersionAsInt]);

  FInstallation := FInstallations.DelphiInstallationFromVersion[DelphiVersionAsInt];
  FInstallation.OutputCallback := Writeln;
  Result := FInstallation;
end;

procedure TDptTaskBase.Writeln(const Text: String);
begin
  if Assigned(FLogger) then
    FLogger.Log(Text);
end;

end.
