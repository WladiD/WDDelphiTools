// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DptIdeExpert.Fixtures;

interface

uses

  System.SysUtils,
  System.Classes,
  ToolsAPI,

  Slim.Fixture;

type

  [SlimFixture('TDptIdeOpenUnitFixture')]
  TDptIdeOpenUnitFixture = class(TSlimDecisionTableFixture)
  private
    FLine    : String;
    FUnitPath: String;
  public
    procedure Reset; override;
    
    [SlimMemberSyncMode(smSynchronized)]
    function OpenUnit: Boolean;
    
    property Line: String read FLine write FLine;
    property UnitPath: String read FUnitPath write FUnitPath;
  end;

procedure RegisterIdeFixtures;

implementation

procedure RegisterIdeFixtures;
begin
  RegisterSlimFixture(TDptIdeOpenUnitFixture);
end;

{ TDptIdeOpenUnitFixture }

procedure TDptIdeOpenUnitFixture.Reset;
begin
  inherited;
  FUnitPath := '';
  FLine := '';
end;

function TDptIdeOpenUnitFixture.OpenUnit: Boolean;
var
  ActionServices: IOTAActionServices;
  EP            : TOTAEditPos;
  I             : Integer;
  LTargetLine   : Integer;
  Module        : IOTAModule;
  ModuleServices: IOTAModuleServices;
  SourceEditor  : IOTASourceEditor;
begin
  Result := False;
  
  if not Supports(BorlandIDEServices, IOTAActionServices, ActionServices) then
    Exit;

  // 1. Open the file
  if not ActionServices.OpenFile(FUnitPath) then
    Exit;

  // 2. Jump to line (if specified)
  LTargetLine := StrToIntDef(FLine, 0);
  if LTargetLine > 0 then
  begin
    if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    begin
      Module := nil;
      // Find the module we just opened
      for I := 0 to ModuleServices.ModuleCount - 1 do
      begin
        if SameText(ModuleServices.Modules[I].FileName, FUnitPath) then
        begin
          Module := ModuleServices.Modules[I];
          Break;
        end;
      end;
      
      if Assigned(Module) then
      begin
        for I := 0 to Module.GetModuleFileCount - 1 do
        begin
           if Supports(Module.GetModuleFileEditor(I), IOTASourceEditor, SourceEditor) then
           begin
             SourceEditor.Show;
             if SourceEditor.GetEditViewCount > 0 then
             begin
               // Set Cursor to Line, Column 1
               EP.Line := LTargetLine;
               EP.Col := 1;
               SourceEditor.GetEditView(0).CursorPos := EP;
               SourceEditor.GetEditView(0).Center(LTargetLine, 1);
               SourceEditor.GetEditView(0).Paint;
             end;
             Break;
           end;
        end;
      end;
    end;
  end;
  
  Result := True;
end;

end.
