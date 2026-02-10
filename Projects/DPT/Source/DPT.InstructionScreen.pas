// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.InstructionScreen;

interface

uses

  System.SysUtils,

  DPT.PrintPath.Task,
  DPT.Types;

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
        'Automatically sets up the environment variables (rsvars.bat) and passes the current Delphi version.',
        'If <ProjectFile> is not a .dproj file, it is processed by the internal TmplCodeGen preprocessor first.',
        '  - Supports embedded configs: (* Name-conf.json ... *)',
        '  - Supports generation instructions: // TmplCodeGen Prefix',
        '  - Supports include partials: // TmplCodeGen include_partials [Target]',
        'Defaults: Platform=Win32, Config=Debug',
        'Example: DPT LATEST Build MyProject.dproj Win64 Release "/t:Clean;Build"'
      ]),
      TActionInfo.Create('BuildAndRun', '<ProjectFile> [Platform] [Config] [--OnlyIfChanged] [-- <Args>]', [
        'Builds and executes the project.',
        'Supports standard Build parameters and TmplCodeGen preprocessing (see Build action).',
        '--OnlyIfChanged: Skips build if executable is newer than source files.',
        '-- <Args>: Passes all subsequent arguments to the executable.',
        'Example: DPT LATEST BuildAndRun MyProject.dproj Win64 Release --OnlyIfChanged -- -run -debug'
      ]),
      TActionInfo.Create('DProjPrintConfigs', '<ProjectFile>', [
        'Lists all build configurations defined in the specified .dproj file.',
        'Example: DPT D12 DProjPrintConfigs MyProject.dproj'
      ]),
      TActionInfo.Create('DProjPrintCurConfig', '<ProjectFile>', [
        'Displays the default/active build configuration of the specified .dproj file.',
        'Example: DPT D12 DProjPrintCurConfig MyProject.dproj'
      ]),
      TActionInfo.Create('DProjPrintSearchPaths', '<ProjectFile> [Config] [Platform]', [
        'Displays the effective unit search path for the project.',
        'Combines the project''s specific search path (resolving variables) with the IDE''s global library path.',
        'Defaults: Config=<ActiveConfig>, Platform=Win32.',
        'Example: DPT D12 DProjPrintSearchPaths MyProject.dproj Release Win64'
      ]),
      TActionInfo.Create('ExportBuildEnvironment', '<TargetPath>', [
        'Exports a minimal Delphi build environment to the specified directory.',
        'The environment can be used on a clean Windows machine for CI/CD builds.',
        'Includes required BDS files, registry settings (HKCU/HKLM), DPT.exe and initialization scripts.',
        'Generates a smart Init...bat script that handles Admin rights and Unattended mode.',
        'WARNING: The target machine still requires a valid license/activation.',
        'Example: DPT D12 ExportBuildEnvironment C:\Temp\Delphi12Build'
      ]),
      TActionInfo.Create('HandleProtocol', '<dpt://URL>', [
        'Internal handler for "dpt://" URI schemes.',
        'Used to trigger actions like opening units from external applications (e.g., browsers or log viewers).',
        'Example: dpt://openunit/?file=C:\MyUnit.pas&line=50'
      ]),
      TActionInfo.Create('ImportBuildEnvironment', '', [
        'Restores a build environment from the directory where this DPT.exe is located.',
        'Copies BDS files to Program Files, restores AppData, and imports Registry settings.',
        'The target paths are determined automatically based on the DelphiVersion parameter.',
        'Intended to be called by the generated Init...bat script.',
        'Example: DPT D12 ImportBuildEnvironment'
      ]),
      TActionInfo.Create('IsPackageRegistered', '<PackageFileName>', [
        'Checks if a specific BPL package is currently registered in the IDE.',
        'Returns ExitCode 0 if registered, 1 if not.'
      ]),
      TActionInfo.Create('Lint', '[--verbose] [--fitnesse-dir=<Path>] <StyleFile> <TargetFiles...>', [
        'Analyzes one or more Delphi units for style violations based on the specified StyleFile.',
        'Uses an internal Slim/FitNesse engine to verify the code structure.',
        'All target files are processed in a single FitNesse session (Suite execution).',
        'Options:',
        '  --verbose: Displays full FitNesse and Slim server logs.',
        '  --fitnesse-dir=<Path>: Explicitly sets the FitNesse installation directory.',
        'Configuration Priority:',
        '  1. --fitnesse-dir parameter',
        '  2. "Dir" entry in [FitNesse] section of DptConfig.ini (searched in PATH)',
        'Example: DPT LATEST Lint --verbose Lint\TaifunUnitStyle.pas Unit1.pas Unit2.pas'
      ]),
      TActionInfo.Create('LintSetup', '<Split|Join> <StyleFile>', [
        'Splits a 3-column style file into 3 separate files for easier editing,',
        'or joins them back into a single aligned style file.',
        'Example: DPT LATEST LintSetup Split Lint\TaifunUnitStyle.pas'
      ]),
      TActionInfo.Create('OpenUnit', '<FullPathToUnit> [GoToLine <Line>] [GoToMemberImplementation <Name>]', [
        'Opens a source file in the Delphi IDE via the Slim Server plugin.',
        'Supports navigating to a specific line number or finding a member implementation (Class.Method).',
        'Automatically starts the IDE if it is not running and waits for the plugin to become available.'
      ]),
      TActionInfo.Create('PrintPath', '<PathLiteral>', [
        'Outputs various IDE configuration paths to the console.',
        'Useful for build scripts to locate BDS, Bin, or default BPL/DCP output directories.',
        'Available literals:',
        ValidPathBds + ', ' + ValidPathBdsBin + ',',
        ValidPathBplOutputWin32 + ', ' + ValidPathBplOutputWin64 + ',',
        ValidPathDcpOutputWin32 + ', ' + ValidPathDcpOutputWin64
      ]),
      TActionInfo.Create('RegisterPackage', '<PathToBPL>', [
        'Registers a specific BPL file as a design-time package in the currently selected Delphi version.'
      ]),
      TActionInfo.Create('RemovePackage', '<PackageFileName>', [
        'Unregisters a design-time package by its file name (without path or extension).'
      ]),
      TActionInfo.Create('RemovePackagesBySourceDir', '<SourceDir>', [
        'Scans the registry for design-time packages located inside the specified directory tree and unregisters them.'
      ]),
      TActionInfo.Create('Start', '', [
        'Ensures the Delphi IDE is running.',
        'If not, it launches the process and waits for it to become responsive.',
        'Brings the IDE window to the front.'
      ]),
      TActionInfo.Create('Stop', '', [
        'Forcefully terminates the running Delphi IDE process associated with the selected version.',
        'WARNING: Unsaved data will be lost.'
      ])
    ];
  end;
  Result := FActions;
end;

class procedure TDptInstructionScreen.PrintHeader;
begin
  Writeln('Delphi Processing Tools (DPT)');
  Writeln('Version 0.2 - 2026 - Waldemar Derr');
  Writeln('https://github.com/WladiD/WDDelphiTools/tree/master/Projects/DPT');
  Writeln;
end;

class procedure TDptInstructionScreen.PrintDelphiVersions;
begin
  Writeln('DelphiVersion:');
  Writeln('  LATEST              Automatically selects the newest installed version (Alias: RECENT)');
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
