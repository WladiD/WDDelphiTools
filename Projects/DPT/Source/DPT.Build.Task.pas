// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Build.Task;

interface

uses

  DPT.Logger,
  DPT.Task,
  DPT.Types;

type

  /// <summary>
  ///   Outcome of a project build. <c>ExitCode = 0</c> means success;
  ///   <c>Skipped</c> is True when an <c>--OnlyIfChanged</c> build found the
  ///   output already up to date and compiled nothing.
  /// </summary>
  TDptBuildResult = record
    Skipped  : Boolean;
    ExitCode : Integer;
    OutputExe: String;
    NewerFile: String;
    function Success: Boolean;
  end;

  /// <summary>
  ///   Abstraction over "build a .dproj and tell me the output executable".
  ///   The production implementation drives the same code path as the
  ///   <c>Build</c> CLI action; tests inject a stub so they neither require
  ///   the Delphi toolchain nor invoke msbuild. Obtain one via
  ///   <see cref="CreateProjectBuilder"/>.
  /// </summary>
  IDptProjectBuilder = interface
    ['{6C3A1F2E-8B4D-4F0A-9E51-3D7C2A6B1E90}']
    /// <summary>
    ///   Build <paramref name="AProjectFile"/> for the given config/platform,
    ///   forwarding all progress to <paramref name="ALogger"/> (never to the
    ///   process stdout). <paramref name="AExtraArgs"/> is passed verbatim to
    ///   msbuild (this is where forced debug switches travel).
    /// </summary>
    function BuildProject(const AProjectFile, AConfig, APlatform, AExtraArgs: String;
      AOnlyIfChanged: Boolean; const ALogger: ILogger): TDptBuildResult;
  end;

  TDptBuildTask = class(TDptTaskBase)
  protected
    function  GetMSBuildTarget: String; virtual;
    function  GetActionDisplayName: String; virtual;
    function  IsExeLocked(const ExePath: String): Boolean;
    function  RunShellCommand(const CommandLine: String): Integer;
    function  IsBuildNeeded(const AExePath: String; out ANewerFile: String): Boolean;
  public
    Config        : String;
    ExtraArgs     : String;
    ProjectFile   : String;
    TargetPlatform: String;
    OnlyIfChanged : Boolean;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    /// <summary>
    ///   Core build routine shared by the CLI action and the MCP debugger:
    ///   resolves the output exe, honours <c>OnlyIfChanged</c>, runs msbuild
    ///   and returns the outcome. Logs through the inherited (injectable)
    ///   logger; never touches <c>System.ExitCode</c>.
    /// </summary>
    function  RunBuild: TDptBuildResult;
    procedure Execute; override;
  end;

  TDptCompileTask = class(TDptBuildTask)
  protected
    function GetMSBuildTarget: String; override;
    function GetActionDisplayName: String; override;
  end;

  TDptBuildAndRunTask = class(TDptBuildTask)
  public
    NoWait: Boolean;
    RunArgs: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

  TDptCompileAndRunTask = class(TDptBuildAndRunTask)
  protected
    function GetMSBuildTarget: String; override;
    function GetActionDisplayName: String; override;
  end;

/// <summary>
///   Create the production project builder, bound to a specific Delphi
///   installation. Drives <see cref="TDptBuildTask.RunBuild"/> internally so
///   the MCP debugger compiles through exactly the same path as the CLI
///   <c>Build</c> action.
/// </summary>
function CreateProjectBuilder(ADelphiVersion: TDelphiVersion; AIsX64: Boolean): IDptProjectBuilder;

implementation

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

  DPT.Utils,

  DPT.DProjAnalyzer,
  DPT.Workflow;

{ TDptBuildTask }

function TDptBuildTask.GetMSBuildTarget: String;
begin
  Result := 'Build';
end;

function TDptBuildTask.GetActionDisplayName: String;
begin
  Result := 'Build';
end;

procedure TDptBuildTask.Parse(CmdLine: TCmdLineConsumer);
var
  Arg: String;
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

  while CmdLine.HasParameter do
  begin
    Arg := CmdLine.CheckParameter('Args');

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
end;

function TDptBuildTask.RunShellCommand(const CommandLine: String): Integer;
var
  Buffer    : array[0..4095] of Byte;
  ByteBuf   : TBytes; // accumulated, not-yet-emitted child output bytes
  BytesRead : DWORD;
  Cmd       : String;
  PI        : TProcessInformation;
  ReadPipe  : THandle;
  SA        : TSecurityAttributes;
  SI        : TStartupInfo;
  WritePipe : THandle;

  // Emit every complete (newline-terminated) line accumulated in ByteBuf,
  // decoding each line's bytes as UTF-8 (msbuild/dcc emit UTF-8). Splitting on
  // the newline bytes 10/13 is safe: they never occur inside a UTF-8
  // multi-byte sequence, so a character is never severed across the split.
  procedure FlushLines(AFinal: Boolean);
  var
    I, Start: Integer;
  begin
    Start := 0;
    I := 0;
    while I < Length(ByteBuf) do
    begin
      if (ByteBuf[I] = 10) or (ByteBuf[I] = 13) then
      begin
        Writeln(TEncoding.UTF8.GetString(ByteBuf, Start, I - Start));
        // Swallow a paired CRLF/LFCR so we don't emit a spurious blank line.
        if (I + 1 < Length(ByteBuf)) and
           (((ByteBuf[I] = 13) and (ByteBuf[I + 1] = 10)) or
            ((ByteBuf[I] = 10) and (ByteBuf[I + 1] = 13))) then
          Inc(I);
        Start := I + 1;
      end;
      Inc(I);
    end;
    if Start > 0 then
      ByteBuf := Copy(ByteBuf, Start, Length(ByteBuf) - Start);
    if AFinal and (Length(ByteBuf) > 0) then
    begin
      Writeln(TEncoding.UTF8.GetString(ByteBuf));
      SetLength(ByteBuf, 0);
    end;
  end;

begin
  // Redirect the child's stdout/stderr through a pipe we drain ourselves.
  // The previous implementation let the child inherit our console handles,
  // which is fatal when this runs inside the MCP server: msbuild chatter
  // would land on the JSON-RPC stdout stream. Routing it through the logger
  // keeps the CLI behaviour (the console logger echoes it) while staying
  // invisible to the protocol when a capturing logger is injected.
  FillChar(SA, SizeOf(SA), 0);
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;

  if not CreatePipe(ReadPipe, WritePipe, @SA, 0) then
    RaiseLastOSError;
  try
    // Keep our read end out of the child.
    SetHandleInformation(ReadPipe, HANDLE_FLAG_INHERIT, 0);

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESTDHANDLES;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    SI.hStdOutput := WritePipe;
    SI.hStdError := WritePipe;

    Cmd := CommandLine;
    UniqueString(Cmd);

    if not CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil, nil, SI, PI) then
      RaiseLastOSError;

    // Close our copy of the write end so ReadFile reports EOF once the child
    // (and its descendants) have exited and released theirs.
    CloseHandle(WritePipe);
    WritePipe := 0;

    try
      SetLength(ByteBuf, 0);
      while ReadFile(ReadPipe, Buffer, SizeOf(Buffer), BytesRead, nil) and (BytesRead > 0) do
      begin
        var Old: Integer := Length(ByteBuf);
        SetLength(ByteBuf, Old + Integer(BytesRead));
        Move(Buffer[0], ByteBuf[Old], BytesRead);
        FlushLines(False);
      end;
      FlushLines(True);

      WaitForSingleObject(PI.hProcess, INFINITE);
      GetExitCodeProcess(PI.hProcess, DWORD(Result));
    finally
      CloseHandle(PI.hProcess);
      CloseHandle(PI.hThread);
    end;
  finally
    if WritePipe <> 0 then
      CloseHandle(WritePipe);
    CloseHandle(ReadPipe);
  end;
end;

function TDptBuildTask.IsExeLocked(const ExePath: String): Boolean;
var
  Stream: TFileStream;
begin
  if not FileExists(ExePath) then
    Exit(False);

  try
    // Opening with exclusive write access succeeds only when nobody holds it.
    Stream := TFileStream.Create(ExePath, fmOpenReadWrite or fmShareExclusive);
    Stream.Free;
    Result := False;
  except
    Result := True;
  end;
end;

function TDptBuildResult.Success: Boolean;
begin
  Result := ExitCode = 0;
end;

function TDptBuildTask.RunBuild: TDptBuildResult;
var
  CmdLine       : String;
  ProductVersion: String;
  RsvarsPath    : String;
  Analyzer      : TDProjAnalyzer;
begin
  Result := Default(TDptBuildResult);

  // Resolve the output executable (same logic the DProjPrintOutputFile action uses).
  Analyzer := TDProjAnalyzer.Create(ProjectFile);
  try
    Result.OutputExe := Analyzer.GetProjectOutputFile(Config, TargetPlatform);
  finally
    Analyzer.Free;
  end;

  if OnlyIfChanged then
  begin
    if not IsBuildNeeded(Result.OutputExe, Result.NewerFile) then
    begin
      Writeln('Executable is up to date. Skipping build.');
      Result.Skipped := True;
      Exit;
    end;

    if Result.NewerFile <> '' then
      Writeln(Format('Build needed because "%s" is newer than executable.', [ExtractFileName(Result.NewerFile)]));
  end;

  if IsExeLocked(Result.OutputExe) then
  begin
    Writeln('ERROR: Executable is currently in use or locked: ' + Result.OutputExe);
    Writeln('Please close the application and try again.');
    Result.ExitCode := 1;
    Exit;
  end;

  RsvarsPath := IncludeTrailingPathDelimiter(Installation.BinFolderName) + 'rsvars.bat';
  if not FileExists(RsvarsPath) then
    raise Exception.Create('rsvars.bat not found at ' + RsvarsPath);

  // Extract ProductVersion from RootDir (e.g. "23.0" from ".../Studio/23.0")
  ProductVersion := ExtractFileName(ExcludeTrailingPathDelimiter(Installation.RootDir));

  Writeln('Setting up Delphi environment from: ' + RsvarsPath);
  Writeln('PRODUCTVERSION: ' + ProductVersion);
  Writeln(GetActionDisplayName + ': ' + ProjectFile + '...');

  CmdLine := Format('/c ""%s" && msbuild "%s" /t:%s /p:Configuration=%s;Config=%s;Platform=%s;PRODUCTVERSION=%s %s"',
    [RsvarsPath, ProjectFile, GetMSBuildTarget, Config, Config, TargetPlatform, ProductVersion, ExtraArgs]);

  Result.ExitCode := RunShellCommand('cmd.exe ' + CmdLine);

  if not Result.Success then
    Writeln('ERROR: ' + GetActionDisplayName + ' failed with exit code ' + IntToStr(Result.ExitCode))
  else
    Writeln(GetActionDisplayName + ' successful.');
end;

procedure TDptBuildTask.Execute;
var
  R: TDptBuildResult;
begin
  R := RunBuild;
  if not R.Success then
    System.ExitCode := R.ExitCode;
end;

{ TDptCompileTask }

function TDptCompileTask.GetMSBuildTarget: String;
begin
  Result := 'Make';
end;

function TDptCompileTask.GetActionDisplayName: String;
begin
  Result := 'Compile';
end;

{ TDptCompileAndRunTask }

function TDptCompileAndRunTask.GetMSBuildTarget: String;
begin
  Result := 'Make';
end;

function TDptCompileAndRunTask.GetActionDisplayName: String;
begin
  Result := 'Compile';
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

    if SameText(Arg, '--NoWait') then
    begin
      NoWait := True;
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

function TDptBuildTask.IsBuildNeeded(const AExePath: String; out ANewerFile: String): Boolean;
var
  Analyzer    : TDProjAnalyzer;
  BDSPath     : String;
  ExeTime     : TDateTime;
  FullPaths   : String;
  IdePath     : String;
  Inst        : TJclBorRADToolInstallation;
  PathEntry   : String;
  ProjectDir  : String;
  ProjectFiles: TArray<String>;
  ProjPath    : String;
  ResolvedPath: String;
  SourceFile  : String;
begin
  ANewerFile := '';
  if not FileExists(AExePath) then
    Exit(True);

  // Allow a small tolerance (e.g., 2 seconds) for file system time differences
  ExeTime := TFile.GetLastWriteTime(AExePath);
  ProjectDir := ExtractFilePath(ExpandFileName(ProjectFile));

  // 1. Check the project file itself (.dproj)
  if TFile.GetLastWriteTime(ProjectFile) > ExeTime then
  begin
    ANewerFile := ProjectFile;
    Exit(True);
  end;

  Analyzer := TDProjAnalyzer.Create(ProjectFile);
  try
    // 2. Check all files explicitly included in the project (.pas, etc.) via DCCReference
    ProjectFiles := Analyzer.GetProjectFiles;
    for SourceFile in ProjectFiles do
    begin
      if FileExists(SourceFile) and (TFile.GetLastWriteTime(SourceFile) > ExeTime) then
      begin
        ANewerFile := SourceFile;
        Exit(True);
      end;
    end;

    ProjPath := Analyzer.GetProjectSearchPath(Config, TargetPlatform);
  finally
    Analyzer.Free;
  end;

  // 3. Check the project directory (top-level only, like the compiler does)
  for SourceFile in TDirectory.GetFilesEnumerator(ProjectDir, '*', TSearchOption.soTopDirectoryOnly,
    function(const APath: string; const ASearchRec: TSearchRec): Boolean
    begin
      if ASearchRec.TimeStamp <= ExeTime then
        Exit(False);

      var Ext: String := LowerCase(ExtractFileExt(ASearchRec.Name));
      Result := (Ext = '.pas') or (Ext = '.dpr') or (Ext = '.inc') or
                (Ext = '.res') or (Ext = '.dfm') or (Ext = '.fmx');
    end) do
  begin
    ANewerFile := SourceFile;
    Exit(True);
  end;

  Inst := nil;
  try
    Inst := Installation;
  except
    // Installation method may raise an exception if Delphi is not properly configured/detected,
    // which can happen during unit tests. We ignore it here and proceed without IDE paths.
  end;

  if Assigned(Inst) then
  begin
    BDSPath := ExcludeTrailingPathDelimiter(Inst.RootDir);
    ProjPath := StringReplace(ProjPath, '$(BDS)', BDSPath, [rfReplaceAll, rfIgnoreCase]);

    if SameText(TargetPlatform, 'Win64') then
      IdePath := Inst.LibrarySearchPath[bpWin64]
    else
      IdePath := Inst.LibrarySearchPath[bpWin32];
  end
  else
  begin
    BDSPath := '';
    IdePath := '';
  end;

  if (ProjPath <> '') and (IdePath <> '') then
    FullPaths := IdePath + ';' + ProjPath
  else
    FullPaths := IdePath + ProjPath;

  for PathEntry in FullPaths.Split([';'], TStringSplitOptions.ExcludeEmpty) do
  begin
    ResolvedPath := TPath.Combine(ProjectDir, PathEntry);
    ResolvedPath := TPath.GetFullPath(ResolvedPath);

    if not TDirectory.Exists(ResolvedPath) then
      Continue;

    // Search paths only require top-directory scanning as compiler resolves units directly there
    for SourceFile in TDirectory.GetFilesEnumerator(ResolvedPath, '*', TSearchOption.soTopDirectoryOnly,
      function(const APath: string; const ASearchRec: TSearchRec): Boolean
      begin
        if ASearchRec.TimeStamp <= ExeTime then
          Exit(False);

        var Ext: String := LowerCase(ExtractFileExt(ASearchRec.Name));
        Result := (Ext = '.pas') or (Ext = '.inc');
      end) do
    begin
      ANewerFile := SourceFile;
      Exit(True);
    end;
  end;

  Result := False;
end;

procedure TDptBuildAndRunTask.Execute;
var
  ExePath: String;
  ExitCode: Integer;
  Analyzer: TDProjAnalyzer;
begin
  inherited Execute;
  if System.ExitCode <> 0 then
    Exit; // Build failed

  // Re-evaluate ExePath as it might have been created/moved
  Analyzer := TDProjAnalyzer.Create(ProjectFile);
  try
    ExePath := Analyzer.GetProjectOutputFile(Config, TargetPlatform);
  finally
    Analyzer.Free;
  end;

  if not FileExists(ExePath) then
  begin
    Writeln('ERROR: Executable not found at ' + ExePath);
    System.ExitCode := 1;
    Exit;
  end;

  if NoWait then
  begin
    Writeln('Starting ' + ExePath + ' ' + RunArgs + ' (detached)...');
    Writeln('--------------------------------------------------');
    var SI: TStartupInfo;
    var PI: TProcessInformation;
    var Cmd: String;
    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    Cmd := '"' + ExePath + '" ' + RunArgs;
    UniqueString(Cmd);
    if not CreateProcess(nil, PChar(Cmd), nil, nil, False, 0, nil, PChar(ExtractFilePath(ExePath)), SI, PI) then
      RaiseLastOSError;
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
    System.ExitCode := 0;
  end
  else
  begin
    Writeln('Running ' + ExePath + ' ' + RunArgs + '...');
    Writeln('--------------------------------------------------');

    ExitCode := RunShellCommand('"' + ExePath + '" ' + RunArgs);

    if ExitCode <> 0 then
      Writeln('Application exited with code ' + IntToStr(ExitCode));

    System.ExitCode := ExitCode;
  end;
end;

{ TDptProjectBuilder }

type
  TDptProjectBuilder = class(TInterfacedObject, IDptProjectBuilder)
  private
    FDelphiVersion: TDelphiVersion;
    FIsX64        : Boolean;
  public
    constructor Create(ADelphiVersion: TDelphiVersion; AIsX64: Boolean);
    function BuildProject(const AProjectFile, AConfig, APlatform, AExtraArgs: String;
      AOnlyIfChanged: Boolean; const ALogger: ILogger): TDptBuildResult;
  end;

constructor TDptProjectBuilder.Create(ADelphiVersion: TDelphiVersion; AIsX64: Boolean);
begin
  inherited Create;
  FDelphiVersion := ADelphiVersion;
  FIsX64 := AIsX64;
end;

function TDptProjectBuilder.BuildProject(const AProjectFile, AConfig, APlatform,
  AExtraArgs: String; AOnlyIfChanged: Boolean; const ALogger: ILogger): TDptBuildResult;
var
  Task: TDptBuildTask;
begin
  Task := TDptBuildTask.Create;
  try
    Task.DelphiVersion := FDelphiVersion;
    Task.IsX64 := FIsX64;
    if ALogger <> nil then
      Task.Logger := ALogger;
    Task.ProjectFile := AProjectFile;
    Task.Config := AConfig;
    Task.TargetPlatform := APlatform;
    Task.ExtraArgs := AExtraArgs;
    Task.OnlyIfChanged := AOnlyIfChanged;
    Result := Task.RunBuild;
  finally
    Task.Free;
  end;
end;

function CreateProjectBuilder(ADelphiVersion: TDelphiVersion; AIsX64: Boolean): IDptProjectBuilder;
begin
  Result := TDptProjectBuilder.Create(ADelphiVersion, AIsX64);
end;

end.
