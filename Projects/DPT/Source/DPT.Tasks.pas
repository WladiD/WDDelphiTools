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

  TDptExportBuildEnvironmentTask = class(TDptTaskBase)
  private
    procedure CopyDirectory(const SourceDir, DestDir: String);
    procedure ExportRegistry;
    procedure CreateInitScripts;
  public
    TargetPath: String;
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

{ TDptExportBuildEnvironmentTask }

procedure TDptExportBuildEnvironmentTask.CopyDirectory(const SourceDir, DestDir: String);
var
  SR: TSearchRec;
begin
  if not DirectoryExists(DestDir) then
    TDirectory.CreateDirectory(DestDir);

  if FindFirst(IncludeTrailingPathDelimiter(SourceDir) + '*', faAnyFile, SR) = 0 then
  begin
    try
      repeat
        if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          if (SR.Attr and faDirectory) <> 0 then
            CopyDirectory(IncludeTrailingPathDelimiter(SourceDir) + SR.Name, IncludeTrailingPathDelimiter(DestDir) + SR.Name)
          else
            TFile.Copy(IncludeTrailingPathDelimiter(SourceDir) + SR.Name, IncludeTrailingPathDelimiter(DestDir) + SR.Name, True);
        end;
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  end;
end;

procedure TDptExportBuildEnvironmentTask.ExportRegistry;
var
  RegPath: String;
  RegFileHKCU: String;
  RegFileHKLM: String;
  ProductVersion: String;

  procedure RunReg(const ARegCmd: String);
  var
    LSI: TStartupInfo;
    LPI: TProcessInformation;
    LCmd: String;
  begin
    LCmd := ARegCmd;
    UniqueString(LCmd);
    FillChar(LSI, SizeOf(LSI), 0);
    LSI.cb := SizeOf(LSI);
    if CreateProcess(nil, PChar(LCmd), nil, nil, False, 0, nil, nil, LSI, LPI) then
    begin
      WaitForSingleObject(LPI.hProcess, INFINITE);
      CloseHandle(LPI.hProcess);
      CloseHandle(LPI.hThread);
    end;
  end;

begin
  // Extract ProductVersion from RootDir (e.g. "23.0" from ".../Studio/23.0")
  ProductVersion := ExtractFileName(ExcludeTrailingPathDelimiter(Installation.RootDir));
  RegPath := Format('Software\Embarcadero\BDS\%s', [ProductVersion]);
  
  if not DirectoryExists(IncludeTrailingPathDelimiter(TargetPath) + 'Registry') then
    TDirectory.CreateDirectory(IncludeTrailingPathDelimiter(TargetPath) + 'Registry');

  RegFileHKCU := IncludeTrailingPathDelimiter(TargetPath) + 'Registry\BDS_Registry_HKCU.reg';
  RegFileHKLM := IncludeTrailingPathDelimiter(TargetPath) + 'Registry\BDS_Registry_HKLM.reg';

  RunReg(Format('reg export "HKEY_CURRENT_USER\%s" "%s" /y', [RegPath, RegFileHKCU]));
  RunReg(Format('reg export "HKEY_LOCAL_MACHINE\%s" "%s" /y', [RegPath, RegFileHKLM]));
end;

procedure TDptExportBuildEnvironmentTask.CreateInitScripts;
var
  VerStr : String;
  DprPath: String;
  BatPath: String;
  DprContent: TStringList;
  BatContent: TStringList;
  TargetBdsRoot: String;
begin
  VerStr := DelphiVersionStringArray[DelphiVersion];
  DprPath := IncludeTrailingPathDelimiter(TargetPath) + 'Init' + VerStr + 'BuildEnvironment.dpr';
  BatPath := IncludeTrailingPathDelimiter(TargetPath) + 'Init' + VerStr + 'BuildEnvironment.bat';
  TargetBdsRoot := ExcludeTrailingPathDelimiter(Installation.RootDir);
  var ProductVersion: String := ExtractFileName(TargetBdsRoot);

  DprContent := TStringList.Create;
  try
    DprContent.Add('(* Init' + VerStr + 'BuildEnvironment-conf.json');
    DprContent.Add('{');
    DprContent.Add('  "Template": "ConsoleApp.TMPL.dproj"');
    DprContent.Add('}');
    DprContent.Add('*)');
    DprContent.Add('// TmplCodeGen Init' + VerStr + 'BuildEnvironment');
    DprContent.Add('');
    DprContent.Add('program Init' + VerStr + 'BuildEnvironment;');
    DprContent.Add('{$APPTYPE CONSOLE}');
    DprContent.Add('begin');
    DprContent.Add('  Writeln(''Delphi Build Environment Initialized Successfully.'');');
    DprContent.Add('end.');
    DprContent.SaveToFile(DprPath, TEncoding.UTF8);
  finally
    DprContent.Free;
  end;

  BatContent := TStringList.Create;
  try
    BatContent.Add('@echo off');
    BatContent.Add('setlocal');
    BatContent.Add('cd /d "%~dp0"');
    BatContent.Add('');
    BatContent.Add('REM Check for Admin rights');
    BatContent.Add('net session >nul 2>&1');
    BatContent.Add('if %errorLevel% neq 0 (');
    BatContent.Add('    echo ERROR: This script requires Administrator privileges.');
    BatContent.Add('    echo Please right-click and select "Run as administrator".');
    BatContent.Add('    pause');
    BatContent.Add('    exit /b 1');
    BatContent.Add(')');
    BatContent.Add('');
    BatContent.Add('set "REG_EXE=reg"');
    BatContent.Add('if exist "%SystemRoot%\SysWOW64\reg.exe" set "REG_EXE=%SystemRoot%\SysWOW64\reg.exe"');
    BatContent.Add('');
    BatContent.Add('set "TARGET_BDS=' + TargetBdsRoot + '"');
    BatContent.Add('set "SOURCE_BDS=%~dp0BDS"');
    BatContent.Add('');
    BatContent.Add('echo Restoring BDS files to %TARGET_BDS%...');
    BatContent.Add('if not exist "%TARGET_BDS%" mkdir "%TARGET_BDS%"');
    BatContent.Add('xcopy "%SOURCE_BDS%\*" "%TARGET_BDS%\" /E /I /Y /Q');
    BatContent.Add('');
    BatContent.Add('echo Restoring EnvOptions.proj...');
    BatContent.Add('set "TARGET_ENV_PROJ=%APPDATA%\Embarcadero\BDS\' + ProductVersion + '"');
    BatContent.Add('if not exist "%TARGET_ENV_PROJ%" mkdir "%TARGET_ENV_PROJ%"');
    BatContent.Add('if exist "%~dp0AppData\EnvOptions.proj" copy /Y "%~dp0AppData\EnvOptions.proj" "%TARGET_ENV_PROJ%\EnvOptions.proj"');
    BatContent.Add('echo Restoring environment.proj...');
    BatContent.Add('if exist "%~dp0AppData\environment.proj" copy /Y "%~dp0AppData\environment.proj" "%TARGET_ENV_PROJ%\environment.proj"');
    BatContent.Add('');
    BatContent.Add('echo Importing Registry settings...');
    BatContent.Add('"%REG_EXE%" import "Registry\BDS_Registry_HKCU.reg"');
    BatContent.Add('if exist "Registry\BDS_Registry_HKLM.reg" (') ;
    BatContent.Add('  echo Importing HKLM Registry settings ^(Requires Admin rights^)...');
    BatContent.Add('  "%REG_EXE%" import "Registry\BDS_Registry_HKLM.reg"');
    BatContent.Add(')');
    BatContent.Add('');
    BatContent.Add('echo Testing Build Environment...');
    BatContent.Add('DPT.exe ' + VerStr + ' BuildAndRun "' + ExtractFileName(DprPath) + '" --OnlyIfChanged');
    BatContent.Add('');
    BatContent.Add('if %ERRORLEVEL% equ 0 (');
    BatContent.Add('  echo SUCCESS > init_success.marker');
    BatContent.Add('  echo Initialization complete.');
    BatContent.Add(') else (');
    BatContent.Add('  echo ERROR: Initialization failed during build test.');
    BatContent.Add('  exit /b 1');
    BatContent.Add(')');
    BatContent.SaveToFile(BatPath, TEncoding.Default);
  finally
    BatContent.Free;
  end;
end;

procedure TDptExportBuildEnvironmentTask.Execute;
var
  BdsRoot: String;
  TargetBds: String;
  Whitelist: TArray<String>;
  Folder: String;
begin
  if DirectoryExists(TargetPath) and (Length(TDirectory.GetFileSystemEntries(TargetPath)) > 0) then
    raise Exception.Create('Target directory must be empty or not exist.');

  TDirectory.CreateDirectory(TargetPath);
  TDirectory.CreateDirectory(IncludeTrailingPathDelimiter(TargetPath) + 'BDS');
  TDirectory.CreateDirectory(IncludeTrailingPathDelimiter(TargetPath) + 'Templates');

  BdsRoot := IncludeTrailingPathDelimiter(Installation.RootDir);
  TargetBds := IncludeTrailingPathDelimiter(TargetPath) + 'BDS';

  Writeln('Exporting BDS files from: ' + BdsRoot);
  Whitelist := ['bin', 'lib', 'Imports', 'include', 'redist'];
  for Folder in Whitelist do
  begin
    if DirectoryExists(BdsRoot + Folder) then
    begin
      Writeln('  Copying ' + Folder + '...');
      CopyDirectory(BdsRoot + Folder, TargetBds + '\' + Folder);
    end;
  end;

  Writeln('Exporting Registry...');
  ExportRegistry;

  Writeln('Copying DPT.exe...');
  TFile.Copy(ParamStr(0), IncludeTrailingPathDelimiter(TargetPath) + 'DPT.exe', True);

  Writeln('Copying Template...');
  // Try to find the template relative to the DPT.exe or project root
  var ProjectRoot: String := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');
  var TemplateSrc: String := ProjectRoot + 'Examples\MinConsoleApp\Templates\ConsoleApp.TMPL.dproj';
  
  if not FileExists(TemplateSrc) then
     TemplateSrc := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\Examples\MinConsoleApp\Templates\ConsoleApp.TMPL.dproj');

  if FileExists(TemplateSrc) then
    TFile.Copy(TemplateSrc, IncludeTrailingPathDelimiter(TargetPath) + 'Templates\ConsoleApp.TMPL.dproj', True)
  else
    Writeln('Warning: Template ConsoleApp.TMPL.dproj not found at ' + TemplateSrc);

  Writeln('Creating Init scripts...');
  CreateInitScripts;

  var AppDataDir: String := GetEnvironmentVariable('APPDATA');
  var ProductVersion: String := ExtractFileName(ExcludeTrailingPathDelimiter(Installation.RootDir));

  if not DirectoryExists(IncludeTrailingPathDelimiter(TargetPath) + 'AppData') then
    TDirectory.CreateDirectory(IncludeTrailingPathDelimiter(TargetPath) + 'AppData');

  Writeln('Exporting EnvOptions.proj...');
  var EnvOptionsSrc: String := AppDataDir + '\Embarcadero\BDS\' + ProductVersion + '\EnvOptions.proj';
  if FileExists(EnvOptionsSrc) then
    TFile.Copy(EnvOptionsSrc, IncludeTrailingPathDelimiter(TargetPath) + 'AppData\EnvOptions.proj', True)
  else
    Writeln('Warning: EnvOptions.proj not found at ' + EnvOptionsSrc);

  Writeln('Exporting environment.proj...');
  var EnvironmentSrc: String := AppDataDir + '\Embarcadero\BDS\' + ProductVersion + '\environment.proj';

  if FileExists(EnvironmentSrc) then
    TFile.Copy(EnvironmentSrc, IncludeTrailingPathDelimiter(TargetPath) + 'AppData\environment.proj', True)
  else
    Writeln('Warning: environment.proj not found at ' + EnvOptionsSrc);

  Writeln('Export completed successfully to: ' + TargetPath);
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
