// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.InstructionScreen;

interface

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

uses

  System.SysUtils,

  DPT.Detection,
  DPT.PrintPath.Task,
  DPT.Types;

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
      TActionInfo.Create('Build', '<ProjectFile> [Platform] [Config] [--OnlyIfChanged] [ExtraArgs]', [
        'Builds the specified project using MSBuild.',
        'Automatically sets up the environment variables (rsvars.bat) and passes the current Delphi version.',
        'If <ProjectFile> is not a .dproj file, it is processed by the internal TmplCodeGen preprocessor first.',
        'Options:',
        '  --OnlyIfChanged: Skips build if executable is newer than source files.',
        'Defaults: Platform=Win32, Config=Debug',
        'Example: DPT LATEST Build MyProject.dproj Win64 Release --OnlyIfChanged "/t:Clean;Build"'
      ]),
      TActionInfo.Create('Compile', '<ProjectFile> [Platform] [Config] [--OnlyIfChanged] [ExtraArgs]', [
        'Performs an incremental build (MSBuild /t:Make) of the specified project.',
        'Only recompiles units that have changed since the last build.',
        'Same parameters and features as Build action (rsvars.bat, TmplCodeGen preprocessing).',
        'Example: DPT LATEST Compile MyProject.dproj Win64 Release'
      ]),
      TActionInfo.Create('BuildAndRun', '<ProjectFile> [Platform] [Config] [--OnlyIfChanged] [--NoWait] [ExtraArgs] [-- <Args>]', [
        'Builds and executes the project.',
        'Supports standard Build parameters and TmplCodeGen preprocessing (see Build action).',
        'Options:',
        '  --OnlyIfChanged: Skips build if executable is newer than source files.',
        '  --NoWait: Launches the executable asynchronously without waiting for it to terminate.',
        '  ExtraArgs: Any unrecognized arguments are forwarded to MSBuild (e.g. /p:DCC_DcuOutput=...).',
        '  -- <Args>: Passes all subsequent arguments to the executable.',
        'Example: DPT LATEST BuildAndRun MyProject.dproj Win64 Release --OnlyIfChanged --NoWait -- -run -debug'
      ]),
      TActionInfo.Create('CompileAndRun', '<ProjectFile> [Platform] [Config] [--OnlyIfChanged] [--NoWait] [ExtraArgs] [-- <Args>]', [
        'Performs an incremental build (MSBuild /t:Make) and executes the project.',
        'Same parameters and features as BuildAndRun, but only recompiles changed units.',
        'Example: DPT LATEST CompileAndRun MyProject.dproj Win64 Release --OnlyIfChanged -- -run'
      ]),
      TActionInfo.Create('DcuAnalyze', '<DcuFile> [--Header] [--Uses] [--Symbols] [--Sections] [--All] [--Resolve] [--SearchPath=<dirs>] [--Format=Text|Json] [--Verbose]', [
        'Analyzes a Delphi compiled unit (.dcu) and prints structural information.',
        'Decodes the magic bytes, the embedded source file references (primary',
        'unit source + includes), the unit name and the full uses table split',
        'into interface- and implementation-scope entries.',
        'Symbols and Sections inventories are reserved for a later iteration.',
        'Section filters (default: --All):',
        '  --Header   Magic, compiler guess, unit name, source file references.',
        '  --Uses     Interface uses + Implementation uses (separate lists).',
        '             Extracted by scanning for the $63 $64 / $63 $65 markers',
        '             that the modern DCU layout uses to tag each scope.',
        '  --Symbols  Imported type and method cross-references ($66 / $67 tags).',
        '             Tells you which type and method symbols this DCU consumes',
        '             from other units. The unit''s own declared symbols are not',
        '             yet extracted in this iteration.',
        '  --Sections Reserved for a later iteration.',
        'Uses resolution:',
        '  --Resolve              For every uses entry, locate the imported',
        '                         unit''s .dcu on disk and report the path.',
        '                         The DCU''s own directory is always probed first,',
        '                         then any --SearchPath dirs, then the Delphi RTL',
        '                         library path inferred from the detected',
        '                         (compiler, platform) tuple.',
        '  --SearchPath=<dirs>    Semicolon-separated list of additional search',
        '                         directories. Implies --Resolve.',
        'Output:',
        '  --Format=Text (default) or --Format=Json for machine-readable output.',
        '  --Verbose adds a hex preview of the first bytes and any analyzer diagnostics.',
        'The DelphiVersion argument is irrelevant for this action: the compiler that',
        'produced the DCU is detected from the file itself.',
        'Example: DPT DcuAnalyze C:\Path\To\MyUnit.dcu --Format=Json'
      ]),
      TActionInfo.Create('DcuDiff', '<DcuFileA> <DcuFileB> [--Format=Text|Json] [--Quiet]', [
        'Compares two Delphi compiled units (.dcu) and reports the differences',
        'in the dimensions DcuAnalyze can extract: header (magic/compiler/platform),',
        'source-file references, interface uses and implementation uses.',
        'Uses lists are compared as ordered sequences: the report distinguishes',
        '"set membership changed" (units added/removed) from "order changed"',
        '(same set, different sequence). Order matters because Delphi uses the',
        'uses-clause order for unit initialization and name resolution.',
        'Options:',
        '  --Format=Text (default) or --Format=Json for machine-readable output.',
        '  --Quiet suppresses output and reports only via the exit code (CI mode).',
        'Exit codes:',
        '  0  the two DCUs are identical in every tracked dimension',
        '  1  the two DCUs differ',
        '  2  one of the input files could not be read',
        'Example: DPT DcuDiff old\MyUnit.dcu new\MyUnit.dcu --Format=Json'
      ]),
      TActionInfo.Create('DcuIndex', 'Build|Query|Stats <args> [options]', [
        'Builds and queries an aggregated index of DCU files. Three sub-modes:',
        'Build:',
        '  DcuIndex Build <Dir1> [Dir2 ...] [--Recursive|--NoRecursive]',
        '                                   [--Parallel|--NoParallel]',
        '                                   [--Output=<file>] [--Pattern=<glob>]',
        '  Walks the given directories, runs DcuAnalyze on every matching file',
        '  and writes the aggregated index as JSON. With no --Output the JSON',
        '  is streamed to stdout so it can be piped into other tools.',
        '  Defaults: --Recursive, --Parallel, --Pattern=*.dcu.',
        'Query:',
        '  DcuIndex Query <indexFile> <one of the options below>',
        '  Loads a previously built index and answers exactly one question.',
        '  Output is one match per line; exit code 0 on hit, 1 on no-result.',
        '    --Unit=<UnitName>',
        '        Locate the DCU file for the given unit name. Returns one',
        '        line per (compiler, platform) variant present in the index',
        '        with the format: "<UnitName> [Compiler/Platform] <path>".',
        '    --ImportsOf=<UnitName>',
        '        Forward dependency: returns the list of units that',
        '        <UnitName> declares in its uses clauses (interface and',
        '        implementation merged). Answers "what does this unit use?".',
        '    --ImportedBy=<UnitName>',
        '        Reverse dependency: returns every unit that has <UnitName>',
        '        in its own uses clauses. Inverse of --ImportsOf. Answers',
        '        "who depends on this unit?" - useful for impact analysis.',
        '    --References=<SymbolName>',
        '        Returns every unit whose imported type or method',
        '        cross-references contain <SymbolName>. Inspects the data',
        '        that DcuAnalyze --Symbols extracts ($66 / $67 tags).',
        '        Answers "who consumes TStringList / SameText / ...?".',
        '    --DefinedIn=<SymbolName>',
        '        Returns every unit that **declares** <SymbolName> as one',
        '        of its own exported types or routines ($2A / $28 tags).',
        '        Inverse of --References. Answers',
        '        "where is TStringList declared?".',
        'Stats:',
        '  DcuIndex Stats <indexFile>',
        '  Prints summary: DCU count, distribution per compiler/platform,',
        '  total size, distinct imported-unit count.',
        'Example:',
        '  DPT DcuIndex Build C:\Path\To\Lib --Output=lib.json',
        '  DPT DcuIndex Query lib.json --ImportedBy=mormot.core.collections'
      ]),
      TActionInfo.Create('DProjPrintConfigs', '<ProjectFile>', [
        'Lists all build configurations defined in the specified .dproj file.',
        'Example: DPT D13 DProjPrintConfigs MyProject.dproj'
      ]),
      TActionInfo.Create('DProjPrintCurConfig', '<ProjectFile>', [
        'Displays the default/active build configuration of the specified .dproj file.',
        'Example: DPT D13 DProjPrintCurConfig MyProject.dproj'
      ]),
      TActionInfo.Create('DProjPrintOutputFile', '<ProjectFile> [Config] [Platform]', [
        'Displays the output executable file path for the project based on current configuration and platform.',
        'Example: DPT D13 DProjPrintOutputFile MyProject.dproj Release Win64'
      ]),
      TActionInfo.Create('DProjPrintSearchPaths', '<ProjectFile> [Config] [Platform]', [
        'Displays the effective unit search path for the project.',
        'Combines the project''s specific search path (resolving variables) with the IDE''s global library path.',
        'Defaults: Config=<ActiveConfig>, Platform=Win32.',
        'Example: DPT D13 DProjPrintSearchPaths MyProject.dproj Release Win64'
      ]),
      TActionInfo.Create('ExportBuildEnvironment', '<TargetPath>', [
        'Exports a minimal Delphi build environment to the specified directory.',
        'The environment can be used on a clean Windows machine for CI/CD builds.',
        'Includes required BDS files, registry settings (HKCU/HKLM), DPT.exe and initialization scripts.',
        'Generates a smart Init...bat script that handles Admin rights and Unattended mode.',
        'WARNING: The target machine still requires a valid license/activation.',
        'Example: DPT D13 ExportBuildEnvironment C:\Temp\Delphi12Build'
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
        'Example: DPT D13 ImportBuildEnvironment'
      ]),
      TActionInfo.Create('IsPackageRegistered', '<PackageFileName>', [
        'Checks if a specific BPL package is currently registered in the IDE.',
        'Returns ExitCode 0 if registered, 1 if not.'
      ]),
      TActionInfo.Create('Lint', '[--verbose] [--fitnesse-dir=<Path>] <StyleFile> <TargetFiles...>', [
        'Analyzes one or more Delphi units for style violations based on the specified StyleFile.',
        'Requires a corresponding "<StyleFile>.Linting.wiki" containing the FitNesse test definitions.',
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
        'Splits a 2-column style file (Code and Description) into 2 separate files for easier editing:',
        '  - <StyleFile>.Template.pas',
        '  - <StyleFile>.Descriptions.txt',
        'Or joins them back into a single aligned style file.',
        'Example: DPT LATEST LintSetup Split Lint\TaifunUnitStyle.pas'
      ]),
      TActionInfo.Create('Format', '<ScriptFile> <TargetFiles...>', [
        'Formats one or more target files based on the rules defined in a DWScript script.',
        'The script can inspect the ParseTree CST and manipulate tokens or trivia.',
        'TargetFiles supports glob wildcards (* and ?).',
        'Example: DPT LATEST Format Format\TaifunFormat.pas Projects\DPT\Source\*.pas'
      ]),
      TActionInfo.Create('McpDebugger', '', [
        'Starts a standalone Model Context Protocol (MCP) server for debugging Delphi applications.',
        'The server runs continuously in the background and provides the following tools for AI agents:',
        '  - start_debug_session:     Starts a new debug session for the specified executable',
        '  - stop_debug_session:      Detaches from the debugged process and ends the session',
        '  - terminate_debug_session: Kills the debugged process and ends the session',
        '  - set_breakpoint:          Sets a hardware breakpoint in a Delphi unit',
        '  - list_breakpoints:        Lists all currently set hardware breakpoints',
        '  - remove_breakpoint:       Removes an existing hardware breakpoint',
        '  - ignore_exception:        Instructs the debugger to silently ignore an exception class',
        '  - unignore_exception:      Instructs the debugger to stop ignoring an exception class',
        '  - list_ignored_exceptions: Lists all currently ignored exception classes',
        '  - list_threads:            Lists all currently active thread IDs in the debugged process',
        '  - switch_thread:           Switches the debugger focus to a specific thread ID',
        '  - continue:                Continues execution of the debugged process',
        '  - step_into:               Steps into the next source line, entering function calls',
        '  - step_over:               Steps over the current source line, skipping function calls',
        '  - wait_until_paused:       Waits for the debug session to pause and returns state',
        '  - get_state:               Returns the current debugger state instantly',
        '  - get_stack_trace:         Returns the current call stack of the debugged process',
        '  - get_registers:           Returns the current CPU registers',
        '  - get_stack_slots:         Returns a list of stack slots with interpretation',
        '  - get_stack_memory:        Reads the memory of the current stack frame',
        '  - read_memory:             Reads a range of memory from the debugged process',
        '  - read_global_variable:    Reads the value of a global variable by name',
        '  - get_proc_asm:            Returns the assembly bytes of the current procedure',
        'Example: DPT LATEST McpDebugger'
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
  Writeln('Delphi Processing Tools (DPT) [' + {$IFDEF CPUX64}'Win64'{$ELSE}'Win32'{$ENDIF} + ']');
  Writeln('Version 1.0 - 2026 - Waldemar Derr');
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
  Writeln('  ' + ExtractFileName(ParamStr(0)) + ' [DelphiVersion] Action [Parameters]');
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
