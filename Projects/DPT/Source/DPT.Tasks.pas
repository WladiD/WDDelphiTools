// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Tasks;

interface

uses

  Winapi.Windows,

  System.Classes,
  System.IOUtils,
  System.RegularExpressions,
  System.SysUtils,
  System.Types,

  JclIDEUtils,

  DPT.DProjAnalyzer,
  DPT.IdeManager,
  DPT.Types;

type

  TDptRemovePackageTaskBase = class(TDptTaskBase)
  protected
    function IsPackageMatching(const PackageFileName: String): Boolean; virtual; abstract;
  public
    procedure Execute; override;
  end;

  TDptRemovePackagesBySourceDirTask = class(TDptRemovePackageTaskBase)
  protected
    function IsPackageMatching(const PackageFileName: String): Boolean; override;
  public
    SourceDir: String;
    procedure Execute; override;
  end;

  TDptRemovePackageTask = class(TDptRemovePackageTaskBase)
  protected
    function IsPackageMatching(const PackageFileName: String): Boolean; override;
  public
    PackageFileName: String;
    procedure Execute; override;
  end;

  TDptRegisterPackageTask = class(TDptTaskBase)
  public
    PathToBPL: String;
    procedure Execute; override;
  end;

  TDptIsPackageRegisteredTask = class(TDptTaskBase)
  public
    PackageFileName: String;
    procedure Execute; override;
  end;

  TDptPrintPathTask = class(TDptTaskBase)
  public
    PathToPrint: String;
    function GetPathResult: String;
    procedure Execute; override;
  end;

  TDptDProjPrintConfigsTask = class(TDptTaskBase)
  public
    ProjectFile: String;
    procedure Execute; override;
  end;

  TDptDProjPrintCurConfigTask = class(TDptTaskBase)
  public
    ProjectFile: String;
    procedure Execute; override;
  end;

  TDptDProjPrintSearchPathsTask = class(TDptTaskBase)
  public
    ProjectFile: String;
    Config     : String;
    Platform   : String;
    procedure Execute; override;
  end;

  TDptStartTask = class(TDptTaskBase)
  public
    procedure Execute; override;
  end;

  TDptStopTask = class(TDptTaskBase)
  public
    procedure Execute; override;
  end;

  TDptBuildTask = class(TDptTaskBase)
  protected
    function RunShellCommand(const CommandLine: String): Integer;
  public
    Config        : String;
    ExtraArgs     : String;
    ProjectFile   : String;
    TargetPlatform: String;
    procedure Execute; override;
  end;

  TDptBuildAndRunTask = class(TDptBuildTask)
  protected
    function FindExeFile: String;
    function IsBuildNeeded(const AExePath: String): Boolean;
  public
    OnlyIfChanged: Boolean;
    RunArgs: String;
    procedure Execute; override;
  end;

const
  ValidPathBds            = 'BDSPath';
  ValidPathBdsBin         = 'BDSBINPath';
  ValidPathBplOutputWin32 = 'BPLOutputPath-Win32';
  ValidPathBplOutputWin64 = 'BPLOutputPath-Win64';
  ValidPathDcpOutputWin32 = 'DCPOutputPath-Win32';
  ValidPathDcpOutputWin64 = 'DCPOutputPath-Win64';
  ValidPathToPrint        =
    ValidPathBds            + '|' +
    ValidPathBdsBin         + '|' +
    ValidPathBplOutputWin32 + '|' +
    ValidPathBplOutputWin64 + '|' +
    ValidPathDcpOutputWin32 + '|' +
    ValidPathDcpOutputWin64;

implementation

{ TDptRemovePackageTaskBase }

procedure TDptRemovePackageTaskBase.Execute;
var
  DeletePackageList: TStrings;
  Loop             : Integer;
  PackageFileName  : String;
begin
  DeletePackageList := TStringList.Create;
  try
    for Loop := 0 to Installation.IdePackages.Count[IsX64] - 1 do
    begin
      PackageFileName := Installation.IdePackages.PackageFileNames[Loop, IsX64];
      if IsPackageMatching(PackageFileName) then
        DeletePackageList.Add(PackageFileName);
    end;

    for Loop := 0 to DeletePackageList.Count - 1 do
    begin
      PackageFileName := DeletePackageList[Loop];
      Write(PackageFileName);
      if Installation.IdePackages.RemovePackage(PackageFileName, IsX64) then
        Writeln(' > deleted')
      else
        Writeln(' > deletion failed');
    end;
  finally
    DeletePackageList.Free;
  end;
end;

{ TDptRemovePackagesBySourceDirTask }

function TDptRemovePackagesBySourceDirTask.IsPackageMatching(const PackageFileName: String): Boolean;
begin
  Result := Pos(SourceDir, LowerCase(PackageFileName)) = 1;
end;

procedure TDptRemovePackagesBySourceDirTask.Execute;
begin
  SourceDir := LowerCase(SourceDir);
  inherited Execute;
end;

{ TDptRemovePackageTask }

function TDptRemovePackageTask.IsPackageMatching(const PackageFileName: String): Boolean;
var
  ComparePFN: String;
  ExtLength : Integer;
begin
  ComparePFN := LowerCase(ExtractFileName(PackageFileName));
  ExtLength := Length(ExtractFileExt(ComparePFN));
  ComparePFN := Copy(ComparePFN, 1, Length(ComparePFN) - ExtLength);
  Result := ComparePFN = Self.PackageFileName;
end;

procedure TDptRemovePackageTask.Execute;
begin
  PackageFileName := LowerCase(PackageFileName);
  inherited Execute;
end;

{ TDptRegisterPackageTask }

procedure TDptRegisterPackageTask.Execute;
begin
  Installation.RegisterPackage(PathToBPL, '');
end;

{ TDptIsPackageRegisteredTask }

procedure TDptIsPackageRegisteredTask.Execute;
var
  Found: Boolean;
  Loop : Integer;
  Pkg  : String;
  TargetName: String;
begin
  Found := False;
  TargetName := ChangeFileExt(ExtractFileName(PackageFileName), '');

  for Loop := 0 to Installation.IdePackages.Count[IsX64] - 1 do
  begin
    Pkg := Installation.IdePackages.PackageFileNames[Loop, IsX64];
    if SameText(ChangeFileExt(ExtractFileName(Pkg), ''), TargetName) then
    begin
      Found := True;
      Break;
    end;
  end;

  if Found then
    Writeln('Package is registered.')
  else
  begin
    Writeln('Package is NOT registered.');
    System.ExitCode := 1;
  end;
end;

{ TDptPrintPathTask }

function TDptPrintPathTask.GetPathResult: String;
var
  LPP: String;
begin
  LPP := UpperCase(PathToPrint);
  if LPP = UpperCase(ValidPathBds) then
    Result := Installation.RootDir
  else if LPP = UpperCase(ValidPathBdsBin) then
    Result := Installation.BinFolderName
  else if LPP = UpperCase(ValidPathBplOutputWin32) then
    Result := Installation.BPLOutputPath[bpWin32]
  else if LPP = UpperCase(ValidPathBplOutputWin64) then
    Result := Installation.BPLOutputPath[bpWin64]
  else if LPP = UpperCase(ValidPathDcpOutputWin32) then
    Result := Installation.DCPOutputPath[bpWin32]
  else if LPP = UpperCase(ValidPathDcpOutputWin64) then
    Result := Installation.DCPOutputPath[bpWin64]
  else
    Result := '';

  if Result <> '' then
    Result := ExcludeTrailingPathDelimiter(Result);
end;

procedure TDptPrintPathTask.Execute;
begin
  Writeln(GetPathResult);
end;

{ TDptDProjPrintConfigsTask }

procedure TDptDProjPrintConfigsTask.Execute;
var
  Analyzer: TDProjAnalyzer;
  Configs : TArray<String>;
  C       : String;
begin
  Analyzer := TDProjAnalyzer.Create(ProjectFile);
  try
    Configs := Analyzer.GetConfigs;
    for C in Configs do
      Writeln(C);
  finally
    Analyzer.Free;
  end;
end;

{ TDptDProjPrintCurConfigTask }

procedure TDptDProjPrintCurConfigTask.Execute;
var
  Analyzer: TDProjAnalyzer;
begin
  Analyzer := TDProjAnalyzer.Create(ProjectFile);
  try
    Writeln(Analyzer.GetDefaultConfig);
  finally
    Analyzer.Free;
  end;
end;

{ TDptDProjPrintSearchPathsTask }

procedure TDptDProjPrintSearchPathsTask.Execute;
var
  Analyzer: TDProjAnalyzer;
  BDSPath : String;
  Full    : String;
  IdePath : String;
  ProjPath: String;
begin
  Analyzer := TDProjAnalyzer.Create(ProjectFile);
  try
    if Config = '' then
      Config := Analyzer.GetDefaultConfig;
    if Platform = '' then
      Platform := 'Win32';

    ProjPath := Analyzer.GetProjectSearchPath(Config, Platform);
    BDSPath := ExcludeTrailingPathDelimiter(Installation.RootDir);
    ProjPath := StringReplace(ProjPath, '$(BDS)', BDSPath, [rfReplaceAll, rfIgnoreCase]);
    
    if SameText(Platform, 'Win64') then
      IdePath := Installation.LibrarySearchPath[bpWin64]
    else
      IdePath := Installation.LibrarySearchPath[bpWin32];

    if (ProjPath <> '') and (IdePath <> '') then
      Full := IdePath + ';' + ProjPath
    else
      Full := IdePath + ProjPath;

    for var PathEntry in Full.Split([';'], TStringSplitOptions.ExcludeEmpty) do
      Writeln(PathEntry);
  finally
    Analyzer.Free;
  end;
end;

{ TDptStartTask }

procedure TDptStartTask.Execute;
var
  BinPath    : String;
  FoundWnd   : HWND;
  FoundPID   : DWORD;
  ProcessInfo: TProcessInformation;
begin
  BinPath := Installation.BinFolderName;

  if TDptIdeManager.IsBdsRunning(BinPath, FoundWnd, FoundPID) then
  begin
    Writeln('IDE is already running.');
    TDptIdeManager.BringToFront(FoundWnd);
  end
  else
  begin
    TDptIdeManager.StartIDE(BinPath, ProcessInfo);
    CloseHandle(ProcessInfo.hProcess);
    TDptIdeManager.WaitForIDE(BinPath, FoundWnd, FoundPID);
    TDptIdeManager.BringToFront(FoundWnd);
    Writeln('IDE started and ready.');
  end;
end;

{ TDptStopTask }

procedure TDptStopTask.Execute;
var
  BinPath: String;
begin
  BinPath := Installation.BinFolderName;
  if TDptIdeManager.TerminateIDE(BinPath) then
    Writeln('IDE stopped.')
  else
    Writeln('IDE was not running or could not be stopped.');
end;

{ TDptBuildTask }

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
end;

end.
