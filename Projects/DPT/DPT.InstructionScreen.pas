// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.InstructionScreen;

interface

uses

  System.SysUtils,

  DPT.Types,
  DPT.Tasks;

type

  TDptInstructionScreen = class
  private
    type
      TActionInfo = record
        Name: String;
        Params: String;
        Description: Array of String;
        constructor Create(const AName, AParams: String; const ADesc: Array of String);
      end;
    class var FActions: TArray<TActionInfo>;
    class function  GetActions: TArray<TActionInfo>;
    class procedure PrintAction(const AInfo: TActionInfo; ADetailed: Boolean);
    class procedure PrintDelphiVersions;
    class procedure PrintHeader;
  public
    class procedure ShowCompact;
    class procedure ShowHelp(const AActionName: String);
  end;

implementation

{ TDptInstructionScreen.TActionInfo }

constructor TDptInstructionScreen.TActionInfo.Create(const AName, AParams: String; const ADesc: Array of String);
var
  i: Integer;
begin
  Name := AName;
  Params := AParams;
  SetLength(Description, Length(ADesc));
  for i := 0 to High(ADesc) do
    Description[i] := ADesc[i];
end;

{ TDptInstructionScreen }

class function TDptInstructionScreen.GetActions: TArray<TActionInfo>;
begin
  if Length(FActions) = 0 then
  begin
    FActions := [
      TActionInfo.Create('Build', '<ProjectFile> [Platform] [Config] [ExtraArgs]', [
        'Builds the specified project using MSBuild.',
        'Defaults: Platform=Win32, Config=Debug',
        'Example: DPT RECENT Build MyProject.dproj Win64 Release "/t:Clean;Build"'
      ]),
      TActionInfo.Create('HandleProtocol', '<dpt://URL>', [
        'Handles URL protocol requests (e.g. dpt://openunit/?file=...&line=...)'
      ]),
      TActionInfo.Create('IsPackageRegistered', '<PackageFileName>', [
        'Checks if a package is registered in the IDE (ExitCode 1 if not).'
      ]),
      TActionInfo.Create('OpenUnit', '<FullPathToUnit> [GoToLine <Line>] [GoToMemberImplementation <Name>]', [
        'Opens the specified unit in the IDE. Starts IDE if not running.'
      ]),
      TActionInfo.Create('PrintPath', '<PathLiteral>', [
        'Prints IDE paths. Available literals:',
        ValidPathBds + ', ' + ValidPathBdsBin + ',',
        ValidPathBplOutputWin32 + ', ' + ValidPathBplOutputWin64 + ',',
        ValidPathDcpOutputWin32 + ', ' + ValidPathDcpOutputWin64
      ]),
      TActionInfo.Create('RegisterPackage', '<PathToBPL>', [
        'Registers a design-time package in the IDE.'
      ]),
      TActionInfo.Create('RemovePackage', '<PackageFileName>', [
        'Removes a design-time package registration by its name.'
      ]),
      TActionInfo.Create('RemovePackagesBySourceDir', '<SourceDir>', [
        'Removes all design-time packages located in SourceDir.'
      ]),
      TActionInfo.Create('Start', '', [
        'Starts the IDE and waits until it is responsive.'
      ]),
      TActionInfo.Create('Stop', '', [
        'Terminates the IDE process. WARNING: Unsaved changes will be lost!'
      ])
    ];
  end;
  Result := FActions;
end;

class procedure TDptInstructionScreen.PrintHeader;
begin
  Writeln('Delphi Processing Tools (DPT)');
  Writeln('Version 0.06 - 2026 - Waldemar Derr');
  Writeln('https://github.com/WladiD/WDDelphiTools/tree/master/Projects/DPT');
  Writeln;
end;

class procedure TDptInstructionScreen.PrintDelphiVersions;
begin
  Writeln('DelphiVersion:');
  Writeln('  RECENT              Automatically selects the newest installed version');
  for var Loop: Integer := 1 to Integer(High(TDelphiVersion)) do
    Writeln('  ' + Format('%-19s', [DelphiVersionStringArray[TDelphiVersion(Loop)]]));
  Writeln;
end;

class procedure TDptInstructionScreen.PrintAction(const AInfo: TActionInfo; ADetailed: Boolean);
begin
  if AInfo.Params <> '' then
    Writeln('  ' + AInfo.Name + ' ' + AInfo.Params)
  else
    Writeln('  ' + AInfo.Name);

  if ADetailed then
  begin
    for var Line in AInfo.Description do
      Writeln('    ' + Line);
    Writeln;
  end;
end;

class procedure TDptInstructionScreen.ShowCompact;
begin
  PrintHeader;
  Writeln('Usage:');
  Writeln('  ' + ExtractFileName(ParamStr(0)) + ' DelphiVersion Action [Parameters]');
  Writeln('  ' + ExtractFileName(ParamStr(0)) + ' Help [Action]');
  Writeln;

  Writeln('Actions:');
  for var Action in GetActions do
    PrintAction(Action, False);

  Writeln;
  Writeln('For more details use: ' + ExtractFileName(ParamStr(0)) + ' Help Action');
end;

class procedure TDptInstructionScreen.ShowHelp(const AActionName: String);
var
  Found: Boolean;
begin
  PrintHeader;

  if AActionName = '' then
  begin
    // Show detailed help for all actions
    Writeln('Usage: ' + ExtractFileName(ParamStr(0)) + ' Help <Action>');
    Writeln;
    PrintDelphiVersions;
    Writeln('Available Actions:');
    for var Action in GetActions do
      PrintAction(Action, True);
  end
  else
  begin
    Found := False;
    for var Action in GetActions do
    begin
      if SameText(Action.Name, AActionName) then
      begin
        Writeln('Action Help: ' + Action.Name);
        Writeln;
        PrintAction(Action, True);
        Found := True;
        Break;
      end;
    end;

    if not Found then
      Writeln('Unknown action: ' + AActionName);

    PrintDelphiVersions;
  end;
end;

end.
