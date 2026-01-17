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
  LTask   : TDPOpenUnitTask;
  LVersion: TDelphiVersion;
begin
  Result := True;
  LTask := TDPOpenUnitTask.Create;
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

end.
