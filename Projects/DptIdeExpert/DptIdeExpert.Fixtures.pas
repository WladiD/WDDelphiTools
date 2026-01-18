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
    FUnitPath: String;
    FLine: String;
  public
    procedure Reset; override;
    
    [SlimMemberSyncMode(smSynchronized)]
    function OpenUnit: Boolean;
    
    property UnitPath: String read FUnitPath write FUnitPath;
    property Line: String read FLine write FLine;
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
  ModuleServices: IOTAModuleServices;
  SourceEditor: IOTASourceEditor;
  Module: IOTAModule;
  LTargetLine: Integer;
  I: Integer;
  EP: TOTAEditPos;
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
      Module := NIL;
      // Find the module we just opened
      for I := 0 to ModuleServices.ModuleCount - 1 do
      begin
        if SameText(ModuleServices.Modules[I].FileName, FUnitPath) then
        begin
          Module := ModuleServices.Modules[I];
          Break;
        end;
      end;
      
      if (Module <> NIL) then
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
