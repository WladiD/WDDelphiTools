// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Build.Task;

interface

uses

  Winapi.Windows,

  System.Classes,
  System.IOUtils,
  System.RegularExpressions,
  System.SysUtils,
  System.Types,

  JclIDEUtils,

  DPT.Task,
  DPT.Types,
  DPT.Utils;

type

  TDptBuildTask = class(TDptTaskBase)
  protected
    function RunShellCommand(const CommandLine: String): Integer;
  public
    Config        : String;
    ExtraArgs     : String;
    ProjectFile   : String;
    TargetPlatform: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

  TDptBuildAndRunTask = class(TDptBuildTask)
  protected
    function FindExeFile: String;
    function IsBuildNeeded(const AExePath: String): Boolean;
  public
    OnlyIfChanged: Boolean;
    RunArgs: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

uses
  DPT.Workflow;

{ TDptBuildTask }

procedure TDptBuildTask.Parse(CmdLine: TCmdLineConsumer);
begin
  // ProjectFile (Required)
  ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
  CheckAndExecutePreProcessor(ProjectFile);
  CmdLine.ConsumeParameter;

  // Platform (Optional)
  if CmdLine.HasParameter then
  begin
    TargetPlatform := CmdLine.CheckParameter('Platform');
    CmdLine.ConsumeParameter;
  end
  else
    TargetPlatform := 'Win32';

  // Config (Optional)
  if CmdLine.HasParameter then
  begin
    Config := CmdLine.CheckParameter('Config');
    CmdLine.ConsumeParameter;
  end
  else
    Config := 'Debug';

  // ExtraArgs (Optional - consume all remaining)
  ExtraArgs := '';
  while CmdLine.HasParameter do
  begin
     ExtraArgs := ExtraArgs + ' ' + CmdLine.CheckParameter('ExtraArg');
     CmdLine.ConsumeParameter;
  end;
  ExtraArgs := Trim(ExtraArgs);
end;

function TDptBuildTask.RunShellCommand(const CommandLine: String): Integer;
var
  Cmd: String;
  PI : TProcessInformation;
  SI : TStartupInfo;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  // Ensure the new process inherits standard handles if we want to pipe output,
  // but for console sharing, simple inheritance usually works if bInheritHandles is True.
  // However, simply running in the same console usually just requires not creating a new console.
  Cmd := CommandLine;
  UniqueString(Cmd);

  if not CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil, nil, SI, PI) then
    RaiseLastOSError;

  try
    WaitForSingleObject(PI.hProcess, INFINITE);
    GetExitCodeProcess(PI.hProcess, DWORD(Result));
  finally
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
  end;
end;

procedure TDptBuildTask.Execute;
var
  CmdLine       : String;
  ExitCode      : Integer;
  ProductVersion: String;
  RsvarsPath    : String;
begin
  RsvarsPath := IncludeTrailingPathDelimiter(Installation.BinFolderName) + 'rsvars.bat';
  if not FileExists(RsvarsPath) then
    raise Exception.Create('rsvars.bat not found at ' + RsvarsPath);

  // Extract ProductVersion from RootDir (e.g. "23.0" from ".../Studio/23.0")
  ProductVersion := ExtractFileName(ExcludeTrailingPathDelimiter(Installation.RootDir));

  Writeln('Setting up Delphi environment from: ' + RsvarsPath);
  Writeln('PRODUCTVERSION: ' + ProductVersion);
  Writeln('Building ' + ProjectFile + '...');

  // Build command line
  // cmd.exe /c ""rsvars.bat" && msbuild "%PROJECT_FILE%" /t:Build /p:Configuration=%BUILD_CONFIG%;Config=%BUILD_CONFIG%;Platform=%BUILD_PLATFORM%;PRODUCTVERSION=%PRODUCTVERSION% %EXTRA_MSBUILD_PARAMS%"
  CmdLine := Format('/c ""%s" && msbuild "%s" /t:Build /p:Configuration=%s;Config=%s;Platform=%s;PRODUCTVERSION=%s %s"',
    [RsvarsPath, ProjectFile, Config, Config, TargetPlatform, ProductVersion, ExtraArgs]);

  ExitCode := RunShellCommand('cmd.exe ' + CmdLine);

  if ExitCode <> 0 then
  begin
    Writeln('ERROR: Build failed with exit code ' + IntToStr(ExitCode));
    System.ExitCode := ExitCode;
  end
  else
    Writeln('Build successful.');
end;

{ TDptBuildAndRunTask }

procedure TDptBuildAndRunTask.Parse(CmdLine: TCmdLineConsumer);
var
  Arg         : String;
  ArgsConsumed: Boolean;
begin
  // ProjectFile (Required)
  ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
  CheckAndExecutePreProcessor(ProjectFile);
  CmdLine.ConsumeParameter;

  // Defaults
  TargetPlatform := 'Win32';
  Config := 'Debug';
  OnlyIfChanged := False;
  ExtraArgs := '';
  RunArgs := '';

  ArgsConsumed := False; // Flag to track if we hit "--"

  while CmdLine.HasParameter do
  begin
    Arg := CmdLine.CheckParameter('Args');

    if ArgsConsumed then
    begin
      // Append to RunArgs
      RunArgs := RunArgs + ' ' + Arg;
      CmdLine.ConsumeParameter;
      Continue;
    end;

    if Arg = '--' then
    begin
      ArgsConsumed := True;
      CmdLine.ConsumeParameter;
      Continue;
    end;

    if SameText(Arg, '--OnlyIfChanged') then
    begin
      OnlyIfChanged := True;
      CmdLine.ConsumeParameter;
      Continue;
    end;

    if (TargetPlatform = 'Win32') and ((SameText(Arg, 'Win32')) or (SameText(Arg, 'Win64'))) then
    begin
      TargetPlatform := Arg;
      CmdLine.ConsumeParameter;
    end
    else if (Config = 'Debug') and ((SameText(Arg, 'Debug')) or (SameText(Arg, 'Release')) or (SameText(Arg, 'FitNesse'))) then
    begin
      Config := Arg;
      CmdLine.ConsumeParameter;
    end
    else
    begin
      // Assume ExtraArg for MSBuild
      ExtraArgs := ExtraArgs + ' ' + Arg;
      CmdLine.ConsumeParameter;
    end;
  end;

  ExtraArgs := Trim(ExtraArgs);
  RunArgs := Trim(RunArgs);
end;

function TDptBuildAndRunTask.FindExeFile: String;
var
  BaseName      : String;
  ExeOutput     : String;
  PossiblePath  : String;
  ProjectContent: String;
  GroupMatches  : TMatchCollection;
  GroupMatch    : TMatch;
  Condition     : String;
  ValMatch      : TMatch;
  Body          : String;
  CondMatch     : TMatch;
  SkipGroup     : Boolean;
  RootPath      : String;
begin
  BaseName := ChangeFileExt(ExtractFileName(ProjectFile), '.exe');
  ExeOutput := '';

  // Try to find custom output path in .dproj by parsing PropertyGroups
  if FileExists(ProjectFile) then
  begin
    ProjectContent := TFile.ReadAllText(ProjectFile);

    // Find all PropertyGroups. Use [\s\S] to match across lines.
    GroupMatches := TRegEx.Matches(ProjectContent, '<PropertyGroup(.*?)>([\s\S]*?)</PropertyGroup>', [roIgnoreCase]);

    for GroupMatch in GroupMatches do
    begin
      // Extract Condition from attributes (Group 1)
      Condition := GroupMatch.Groups[1].Value; // e.g. ' Condition="..."'
      Body := GroupMatch.Groups[2].Value;

      // Optimization: If no DCC_ExeOutput inside, skip this group
      if not Body.Contains('DCC_ExeOutput') then
        Continue;

      // Check Condition compatibility
      SkipGroup := False;

      // Check Config mismatch: '$(Config)'=='Something'
      // We look for patterns like: '$(Config)'=='Debug'
      // If 'Something' is not our Config, we skip.
      for CondMatch in TRegEx.Matches(Condition, '''\$\(Config\)''==''([^'']*)''', [roIgnoreCase]) do
      begin
        if not SameText(CondMatch.Groups[1].Value, Config) then
        begin
          SkipGroup := True;
          Break;
        end;
      end;
      if SkipGroup then Continue;

      // Check Platform mismatch
      for CondMatch in TRegEx.Matches(Condition, '''\$\(Platform\)''==''([^'']*)''', [roIgnoreCase]) do
      begin
        if not SameText(CondMatch.Groups[1].Value, TargetPlatform) then
        begin
          SkipGroup := True;
          Break;
        end;
      end;
      if SkipGroup then Continue;

      // If we are here, the group is applicable (or generic).
      // Look for DCC_ExeOutput
      ValMatch := TRegEx.Match(Body, '<DCC_ExeOutput>(.*?)</DCC_ExeOutput>', [roIgnoreCase]);
      if ValMatch.Success then
      begin
        ExeOutput := ValMatch.Groups[1].Value;
      end;
    end;
  end;

  if ExeOutput <> '' then
  begin
    // Replace variables
    ExeOutput := ExeOutput.Replace('$(Platform)', TargetPlatform, [rfReplaceAll, rfIgnoreCase]);
    ExeOutput := ExeOutput.Replace('$(Config)', Config, [rfReplaceAll, rfIgnoreCase]);

    // Construct path
    if TPath.IsPathRooted(ExeOutput) then
      PossiblePath := TPath.Combine(ExeOutput, BaseName)
    else
      PossiblePath := ExpandFileName(
        IncludeTrailingPathDelimiter(ExtractFilePath(ProjectFile)) +
        IncludeTrailingPathDelimiter(ExeOutput) +
        BaseName);

    Exit(PossiblePath); // Return calculated path, even if not exists!
  end;

  // If no explicit output found, fall back to Project Root (default for empty DCC_ExeOutput)
  RootPath := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFilePath(ProjectFile)) + BaseName);
  Result := RootPath;
end;

function TDptBuildAndRunTask.IsBuildNeeded(const AExePath: String): Boolean;
var
  ExeTime   : TDateTime;
  Files     : TStringDynArray;
  ProjectDir: String;
  SourceFile: String;
begin
  if not FileExists(AExePath) then
    Exit(True);

  // Allow a small tolerance (e.g., 2 seconds) for file system time differences
  ExeTime := TFile.GetLastWriteTime(AExePath);
  ProjectDir := ExtractFilePath(ExpandFileName(ProjectFile));

  // Extensions to check
  // We check .pas, .dpr, .dproj, .dfm, .fmx, .inc, .res
  Files := TDirectory.GetFiles(ProjectDir, '*.*', TSearchOption.soAllDirectories);

  for SourceFile in Files do
  begin
    var Ext: String := LowerCase(ExtractFileExt(SourceFile));
    if (Ext = '.pas') or (Ext = '.dpr') or (Ext = '.dproj') or
       (Ext = '.dfm') or (Ext = '.fmx') or (Ext = '.inc') or (Ext = '.res') then
    begin
      if TFile.GetLastWriteTime(SourceFile) > ExeTime then
      begin
        Writeln(Format('Source file "%s" is newer than executable.', [ExtractFileName(SourceFile)]));
        Exit(True);
      end;
    end;
  end;

  Result := False;
end;

procedure TDptBuildAndRunTask.Execute;
var
  ExePath: String;
  ExitCode: Integer;
begin
  ExePath := FindExeFile;

  if OnlyIfChanged then
  begin
    if not IsBuildNeeded(ExePath) then
    begin
      Writeln('Executable is up to date. Skipping build.');
    end
    else
    begin
      inherited Execute;
      if System.ExitCode <> 0 then
        Exit; // Build failed
    end;
  end
  else
  begin
    inherited Execute;
    if System.ExitCode <> 0 then
      Exit; // Build failed
  end;

  // Re-evaluate ExePath as it might have been created/moved
  if not FileExists(ExePath) then
     ExePath := FindExeFile;

  if not FileExists(ExePath) then
  begin
    Writeln('ERROR: Executable not found at ' + ExePath);
    System.ExitCode := 1;
    Exit;
  end;

  Writeln('Running ' + ExePath + ' ' + RunArgs + '...');
  Writeln('--------------------------------------------------');

  ExitCode := RunShellCommand('"' + ExePath + '" ' + RunArgs);

  if ExitCode <> 0 then
    Writeln('Application exited with code ' + IntToStr(ExitCode));

  System.ExitCode := ExitCode;

  if Assigned(WorkflowEngine) then
    TDptWorkflowEngine(WorkflowEngine).RegisterRunResult(ExtractFileName(ProjectFile), ExitCode);
end;

end.
