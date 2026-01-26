// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Fixtures;

interface

uses

  System.SyncObjs,
  System.SysUtils,

  Slim.Fixture,

  DPT.OpenUnitTask,
  DPT.Tasks,
  DPT.Types;

type

  [SlimFixture('TDptOpenUnitFixture')]
  TDptOpenUnitFixture = class(TSlimDecisionTableFixture)
  private
    FDelphiVersion: String;
    FLine         : String;
    FMember       : String;
    FUnitPath     : String;
  public
    procedure Reset; override;
    function  OpenUnitTask: Boolean;
    property DelphiVersion: String read FDelphiVersion write FDelphiVersion;
    property Line: String read FLine write FLine;
    property Member: String read FMember write FMember;
    property UnitPath: String read FUnitPath write FUnitPath;
  end;

  [SlimFixture('TDptPrintPathFixture')]
  TDptPrintPathFixture = class(TSlimDecisionTableFixture)
  private
    FDelphiVersion: String;
    FPathType     : String;
  public
    procedure Reset; override;
    function  Path: String;
    property DelphiVersion: String read FDelphiVersion write FDelphiVersion;
    property PathType: String read FPathType write FPathType;
  end;

  [SlimFixture('TDptIdeLifecycleFixture')]
  TDptIdeLifecycleFixture = class(TSlimFixture)
  private
    FDelphiVersion: String;
  public
    procedure SetDelphiVersion(const Value: String);
    function StartIde: Boolean;
    function StopIde: Boolean;
  end;

  [SlimFixture('TDptControl')]
  TDptControl = class(TSlimFixture)
  public
    class var StopServerEvent: TEvent;
    class constructor Create;
    class destructor Destroy;
  public
    function  Echo(const Value: String): String;
    procedure StopServer;
  end;

implementation

{ TDptOpenUnitFixture }

procedure TDptOpenUnitFixture.Reset;
begin
  inherited;
  FDelphiVersion := 'RECENT';
  FUnitPath := '';
  FLine := '';
  FMember := '';
end;

function TDptOpenUnitFixture.OpenUnitTask: Boolean;
var
  LTask   : TDptOpenUnitTask;
  LVersion: TDelphiVersion;
begin
  Result := True;
  LTask := TDptOpenUnitTask.Create;
  try
    if SameText(FDelphiVersion, 'RECENT') then
      LVersion := FindMostRecentDelphiVersion
    else if not IsValidDelphiVersion(FDelphiVersion, LVersion) then
      raise Exception.Create('Invalid Delphi version: ' + FDelphiVersion);

    LTask.DelphiVersion := LVersion;
    LTask.FullPathToUnit := FUnitPath;
    LTask.GoToLine := StrToIntDef(FLine, 0);
    LTask.MemberImplementation := FMember;
    LTask.Execute;
  finally
    LTask.Free;
  end;
end;

{ TDptPrintPathFixture }

procedure TDptPrintPathFixture.Reset;
begin
  inherited;
  FDelphiVersion := 'RECENT';
  FPathType := '';
end;

function TDptPrintPathFixture.Path: String;
var
  LTask   : TDptPrintPathTask;
  LVersion: TDelphiVersion;
begin
  LTask := TDptPrintPathTask.Create;
  try
    if SameText(FDelphiVersion, 'RECENT') then
      LVersion := FindMostRecentDelphiVersion
    else if not IsValidDelphiVersion(FDelphiVersion, LVersion) then
      raise Exception.Create('Invalid Delphi version: ' + FDelphiVersion);

    LTask.DelphiVersion := LVersion;
    LTask.PathToPrint := FPathType;
    Result := LTask.GetPathResult;
  finally
    LTask.Free;
  end;
end;

{ TDptIdeLifecycleFixture }

procedure TDptIdeLifecycleFixture.SetDelphiVersion(const Value: String);
begin
  FDelphiVersion := Value;
end;

function TDptIdeLifecycleFixture.StartIde: Boolean;
var
  LTask: TDptStartTask;
  LVersion: TDelphiVersion;
begin
  Result := True;
  LTask := TDptStartTask.Create;
  try
    try
      if SameText(FDelphiVersion, 'RECENT') then
        LVersion := FindMostRecentDelphiVersion
      else if not IsValidDelphiVersion(FDelphiVersion, LVersion) then
        raise Exception.Create('Invalid Delphi version: ' + FDelphiVersion);

      LTask.DelphiVersion := LVersion;
      LTask.Execute;
    except
      Result := False;
    end;
  finally
    LTask.Free;
  end;
end;

function TDptIdeLifecycleFixture.StopIde: Boolean;
var
  LTask: TDptStopTask;
  LVersion: TDelphiVersion;
begin
  Result := True;
  LTask := TDptStopTask.Create;
  try
    try
      if SameText(FDelphiVersion, 'RECENT') then
        LVersion := FindMostRecentDelphiVersion
      else if not IsValidDelphiVersion(FDelphiVersion, LVersion) then
        raise Exception.Create('Invalid Delphi version: ' + FDelphiVersion);

      LTask.DelphiVersion := LVersion;
      LTask.Execute;
    except
      Result := False;
    end;
  finally
    LTask.Free;
  end;
end;

{ TDptControl }

class constructor TDptControl.Create;
begin
  StopServerEvent := TEvent.Create(nil, True, False, '');
end;

class destructor TDptControl.Destroy;
begin
  FreeAndNil(StopServerEvent);
end;

function TDptControl.Echo(const Value: String): String;
begin
  Result := Value;
end;

procedure TDptControl.StopServer;
begin
  if Assigned(StopServerEvent) then
    StopServerEvent.SetEvent;
end;

initialization

RegisterSlimFixture(TDptControl);
RegisterSlimFixture(TDptOpenUnitFixture);
RegisterSlimFixture(TDptPrintPathFixture);
RegisterSlimFixture(TDptIdeLifecycleFixture);

end.
