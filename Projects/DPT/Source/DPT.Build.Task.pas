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

  System.Collections.Factory,
  System.Collections.Interfaces,

  DPT.Task,
  DPT.Types,
  DPT.Utils;

type

  TDptBuildTask = class(TDptTaskBase)
  protected
    function  RunShellCommand(const CommandLine: String): Integer;
    function  FindExeFile: String;
    procedure CheckExeNotLocked(const ExePath: String);
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

function TDptBuildTask.FindExeFile: String;
var
  BaseName      : String;
  ExeOutput     : String;
  PossiblePath  : String;
  ProjectContent: String;
  GroupMatches  : TMatchCollection;
  GroupMatch    : TMatch;
  RawCondition  : String;
  Body          : String;
  ValMatch      : TMatch;
  Definitions   : IDictionary_String_String;
  RootPath      : String;

  function EvaluateCondition(const ACondition: String): Boolean;
  var
    Resolved: String;
    Parts: TArray<String>;
    Part: String;
    EqPos, NeqPos: Integer;
    Left, Right: String;
    ResultBool: Boolean;
    VarMatches: TMatchCollection;
    VarMatch: TMatch;
    VarName: String;
    VarValue: String;
  begin
    // Empty condition is always true
    if Trim(ACondition) = '' then
      Exit(True);

    Resolved := ACondition;

    // 1. Resolve variables $(Var) by finding them and looking up in Definitions
    VarMatches := TRegEx.Matches(Resolved, '\$\((\w+)\)');
    for VarMatch in VarMatches do
    begin
      VarName := VarMatch.Groups[1].Value; // e.g. 'Config'
      if Definitions.TryGetValue(LowerCase(VarName), VarValue) then
        Resolved := StringReplace(Resolved, VarMatch.Value, VarValue, [rfReplaceAll, rfIgnoreCase])
      else
        Resolved := StringReplace(Resolved, VarMatch.Value, '', [rfReplaceAll, rfIgnoreCase]);
    end;

    // 2. Split by ' or ' (simple OR support)
    Parts := Resolved.Split([' or ', ' OR '], TStringSplitOptions.ExcludeEmpty);
    if Length(Parts) = 0 then
       Parts := [Resolved];

    Result := False;
    for Part in Parts do
    begin
      // Evaluate single expression
      // Handle 'A'=='B'
      EqPos := Pos('==', Part);
      NeqPos := Pos('!=', Part);
      
      ResultBool := False;
      if EqPos > 0 then
      begin
        Left := Trim(Copy(Part, 1, EqPos - 1));
        Right := Trim(Copy(Part, EqPos + 2, Length(Part)));
        // Remove quotes
        Left := StringReplace(Left, '''', '', [rfReplaceAll]);
        Right := StringReplace(Right, '''', '', [rfReplaceAll]);
        if SameText(Left, Right) then ResultBool := True;
      end
      else if NeqPos > 0 then
      begin
        Left := Trim(Copy(Part, 1, NeqPos - 1));
        Right := Trim(Copy(Part, NeqPos + 2, Length(Part)));
        // Remove quotes
        Left := StringReplace(Left, '''', '', [rfReplaceAll]);
        Right := StringReplace(Right, '''', '', [rfReplaceAll]);
        if not SameText(Left, Right) then ResultBool := True;
      end
      else
      begin
        // Fallback: If part is just "true" or "false" (after var replacement)
        if SameText(Trim(Part), 'true') then ResultBool := True;
      end;

      if ResultBool then
      begin
        Result := True;
        Break; // Short-circuit OR
      end;
    end;
  end;

  procedure ParseProperties(const ABody: String);
  var
    PropMatches: TMatchCollection;
    PropMatch: TMatch;
    Key, Value: String;
  begin
    // Regex to find <Key>Value</Key>
    PropMatches := TRegEx.Matches(ABody, '<(\w+)>([^<]+)</\1>');
    for PropMatch in PropMatches do
    begin
      Key := PropMatch.Groups[1].Value;
      Value := PropMatch.Groups[2].Value;
      Definitions[LowerCase(Key)] := Value;
    end;
  end;

begin
  BaseName := ChangeFileExt(ExtractFileName(ProjectFile), '.exe');
  ExeOutput := '';
  Definitions := TCollections.CreateDictionary_String_String;

  Definitions['config'] := Config;
  Definitions['platform'] := TargetPlatform;
  Definitions['base'] := ''; // Initialize Base as empty/undefined initially

  if FileExists(ProjectFile) then
  begin
    ProjectContent := TFile.ReadAllText(ProjectFile);

    // Find all PropertyGroups sequentially
    GroupMatches := TRegEx.Matches(ProjectContent, '<PropertyGroup(.*?)>([\s\S]*?)</PropertyGroup>', [roIgnoreCase]);

    for GroupMatch in GroupMatches do
    begin
      // Extract Condition from attributes
      RawCondition := '';
      ValMatch := TRegEx.Match(GroupMatch.Groups[1].Value, 'Condition="([^"]+)"', [roIgnoreCase]);
      if ValMatch.Success then
        RawCondition := ValMatch.Groups[1].Value;

      Body := GroupMatch.Groups[2].Value;

      if EvaluateCondition(RawCondition) then
      begin
        // Parse properties to update Definitions (like Cfg_1, Base, etc.)
        ParseProperties(Body);

        // Check for DCC_ExeOutput in this active block
        ValMatch := TRegEx.Match(Body, '<DCC_ExeOutput>(.*?)</DCC_ExeOutput>', [roIgnoreCase]);
        if ValMatch.Success then
        begin
          ExeOutput := ValMatch.Groups[1].Value;
        end;
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

    Exit(PossiblePath);
  end;

  RootPath := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFilePath(ProjectFile)) + BaseName);
  Result := RootPath;
end;

procedure TDptBuildTask.CheckExeNotLocked(const ExePath: String);
var
  Stream: TFileStream;
begin
  if not FileExists(ExePath) then
    Exit;

  try
    // Try to open with exclusive write access to check if it's locked
    Stream := TFileStream.Create(ExePath, fmOpenReadWrite or fmShareExclusive);
    try
      // If successful, the file is not locked
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('ERROR: Executable is currently in use or locked: ' + ExePath);
      Writeln('Please close the application and try again.');
      System.ExitCode := 1;
      Abort;
    end;
  end;
end;

procedure TDptBuildTask.Execute;
var
  CmdLine       : String;
  ExitCode      : Integer;
  ProductVersion: String;
  RsvarsPath    : String;
  ExePath       : String;
begin
  // Pre-check if output executable is locked
  ExePath := FindExeFile;
  CheckExeNotLocked(ExePath);

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
