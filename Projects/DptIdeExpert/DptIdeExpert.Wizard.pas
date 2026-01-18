// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DptIdeExpert.Wizard;

interface

uses

  System.SysUtils,
  System.Classes,
  ToolsAPI,

  Slim.Server,

  DptIdeExpert.Fixtures;

type

  TDptIdeWizard = class(TInterfacedObject, IOTAWizard)
  private
    FSlimServer: TSlimServer;
    function GetPort: Integer;
    procedure LogToIde(const AMsg: string);
  public
    constructor Create;
    destructor Destroy; override;
    // IOTAWizard methods
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTANotifier methods
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPackageWizard(TDptIdeWizard.Create);
end;

{ TDptIdeWizard }

constructor TDptIdeWizard.Create;
var
  LPort: Integer;
begin
  inherited Create;
  LPort := GetPort;

  try
    FSlimServer := TSlimServer.Create(nil);
    FSlimServer.DefaultPort := LPort;
    FSlimServer.Active := True;

    // Register Fixtures
    DptIdeExpert.Fixtures.RegisterIdeFixtures;

    LogToIde(Format('DPT Slim Server active on port %d', [LPort]));
  except
    on E: Exception do
    begin
      LogToIde(Format('DPT Slim Server failed to start: %s', [E.Message]));
      if Assigned(FSlimServer) then
        FreeAndNil(FSlimServer);
    end;
  end;
end;

destructor TDptIdeWizard.Destroy;
begin
  if Assigned(FSlimServer) then
  begin
    FSlimServer.Active := False;
    FSlimServer.Free;
    LogToIde('DPT Slim Server stopped');
  end;
  inherited;
end;

procedure TDptIdeWizard.LogToIde(const AMsg: string);
var
  LGroup      : IOTAMessageGroup;
  LMsgServices: IOTAMessageServices;
begin
  if Supports(BorlandIDEServices, IOTAMessageServices, LMsgServices) then
  begin
    LGroup := LMsgServices.GetGroup('DPT');
    if not Assigned(LGroup) then
      LGroup := LMsgServices.AddMessageGroup('DPT');
    LMsgServices.AddTitleMessage(AMsg, LGroup);
  end;
end;

procedure TDptIdeWizard.Execute;
begin
  // No action needed for menu clicks currently
end;

function TDptIdeWizard.GetIDString: string;
begin
  Result := 'DptIdeExpert.SlimServer';
end;

function TDptIdeWizard.GetName: string;
begin
  Result := 'DPT Slim Server Expert';
end;

function TDptIdeWizard.GetPort: Integer;
begin
  // Calculate port based on CompilerVersion.
  // D12 (Athens) = 36.0.   36 - 24 = 12.  9000 + 12 = 9012.
  // D11 (Alexandria) = 35. 35 - 24 = 11.  9000 + 11 = 9011.
  Result := 9000 + Round(CompilerVersion - 24);
end;

function TDptIdeWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TDptIdeWizard.AfterSave;
begin
end;

procedure TDptIdeWizard.BeforeSave;
begin
end;

procedure TDptIdeWizard.Destroyed;
begin
end;

procedure TDptIdeWizard.Modified;
begin
end;

end.
