// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.BuildEnvironment.Task;

interface

uses

  Winapi.Windows,

  System.Classes,
  System.IOUtils,
  System.SysUtils,

  JclIDEUtils,

  DPT.Task,
  DPT.Types;

type

  TDptExportBuildEnvironmentTask = class(TDptTaskBase)
  private
    procedure CopyDirectory(const SourceDir, DestDir: String);
    procedure CreateInitScripts;
    procedure ExportRegistry;
  public
    TargetPath: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

  TDptImportBuildEnvironmentTask = class(TDptTaskBase)
  private
    procedure CopyDirectory(const SourceDir, DestDir: String);
    procedure ImportRegistry;
  public
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

{ TDptExportBuildEnvironmentTask }

procedure TDptExportBuildEnvironmentTask.Parse(CmdLine: TCmdLineConsumer);
begin
  TargetPath := ExpandFileName(CmdLine.CheckParameter('TargetPath'));
  CmdLine.ConsumeParameter;
end;

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
  ProductVersion: String;
  RegFileHKCU   : String;
  RegFileHKLM   : String;
  RegPath       : String;

  procedure RunReg(const ARegCmd: String);
  var
    LCmd: String;
    LPI : TProcessInformation;
    LSI : TStartupInfo;
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
  BatContent   : TStringList;
  BatPath      : String;
  DprContent   : TStringList;
  DprPath      : String;
  TargetBdsRoot: String;
  VerStr       : String;
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
    BatContent.Add('if exist "BuildEnvironment.success" (');
    BatContent.Add('    echo Build Environment already initialized ^(BuildEnvironment.success found^).');
    BatContent.Add('    exit /b 0');
    BatContent.Add(')');
    BatContent.Add('');
    BatContent.Add('REM Check for Unattended mode (e.g. Docker or CI)');
    BatContent.Add('set "UNATTENDED=0"');
    BatContent.Add('if /I "%1"=="--Unattended" set "UNATTENDED=1"');
    BatContent.Add('if /I "%1"=="-u" set "UNATTENDED=1"');
    BatContent.Add('');
    BatContent.Add('if "%UNATTENDED%"=="1" goto :RunInit');
    BatContent.Add('');
    BatContent.Add('REM Check for Admin rights and self-elevate if needed');
    BatContent.Add('net session >nul 2>&1');
    BatContent.Add('if %errorLevel% neq 0 (');
    BatContent.Add('    echo Requesting administrative privileges...');
    BatContent.Add('    powershell -Command "Start-Process -FilePath ''%0'' -Verb RunAs"');
    BatContent.Add('    exit /b');
    BatContent.Add(')');
    BatContent.Add('');
    BatContent.Add(':RunInit');
    BatContent.Add('echo Initializing Build Environment...');
    BatContent.Add('DPT.exe ' + VerStr + ' ImportBuildEnvironment');
    BatContent.Add('');
    BatContent.Add('if %ERRORLEVEL% equ 0 (');
    BatContent.Add('  echo Testing Build Environment...');
    BatContent.Add('  DPT.exe ' + VerStr + ' BuildAndRun "' + ExtractFileName(DprPath) + '" --OnlyIfChanged');
    BatContent.Add(')');
    BatContent.Add('');
    BatContent.Add('if %ERRORLEVEL% equ 0 (');
    BatContent.Add('  echo SUCCESS > BuildEnvironment.success');
    BatContent.Add('  echo Initialization complete.');
    BatContent.Add(') else (');
    BatContent.Add('  echo ERROR: Initialization failed.');
    BatContent.Add('  if "%UNATTENDED%"=="0" pause');
    BatContent.Add('  exit /b 1');
    BatContent.Add(')');
    BatContent.SaveToFile(BatPath, TEncoding.Default);
  finally
    BatContent.Free;
  end;
end;

procedure TDptExportBuildEnvironmentTask.Execute;
var
  BdsRoot  : String;
  Folder   : String;
  TargetBds: String;
  Whitelist: TArray<String>;
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

{ TDptImportBuildEnvironmentTask }

procedure TDptImportBuildEnvironmentTask.Parse(CmdLine: TCmdLineConsumer);
begin
  // No extra parameters
end;

procedure TDptImportBuildEnvironmentTask.CopyDirectory(const SourceDir, DestDir: String);
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

procedure TDptImportBuildEnvironmentTask.ImportRegistry;
var
  RegFileHKCU: String;
  RegFileHKLM: String;
  SourceRoot : String;

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
  SourceRoot := ExtractFilePath(ParamStr(0));
  RegFileHKCU := IncludeTrailingPathDelimiter(SourceRoot) + 'Registry\BDS_Registry_HKCU.reg';
  RegFileHKLM := IncludeTrailingPathDelimiter(SourceRoot) + 'Registry\BDS_Registry_HKLM.reg';

  if FileExists(RegFileHKCU) then
  begin
    Writeln('Importing HKCU Registry settings...');
    RunReg(Format('reg import "%s"', [RegFileHKCU]));
  end
  else
    Writeln('Warning: HKCU Registry file not found at ' + RegFileHKCU);

  if FileExists(RegFileHKLM) then
  begin
    Writeln('Importing HKLM Registry settings (Requires Admin rights)...');
    RunReg(Format('reg import "%s"', [RegFileHKLM]));
  end;
end;

procedure TDptImportBuildEnvironmentTask.Execute;
var
  SourceRoot    : String;
  SourceBDS     : String;
  TargetBDS     : String;
  SourceAppData : String;
  TargetAppData : String;
  ProductVersion: String;
  AppDataDir    : String;
  ProgramFiles  : String;
begin
  SourceRoot := ExtractFilePath(ParamStr(0)); // Assume DPT.exe is in the root of the export

  // 1. Check if we are in a valid export environment
  SourceBDS := IncludeTrailingPathDelimiter(SourceRoot) + 'BDS';
  if not DirectoryExists(SourceBDS) then
    raise Exception.Create('Invalid Build Environment: BDS folder not found at ' + SourceBDS);

  // Determine ProductVersion based on DelphiVersion (avoiding Installation property access)
  case DelphiVersion of
    dvD12:   ProductVersion := '23.0';
    dvD11:   ProductVersion := '22.0';
    dvD10_3: ProductVersion := '20.0';
    dvD10_1: ProductVersion := '18.0';
    dvD2007: ProductVersion := '5.0';
  else
    raise Exception.Create('Unsupported Delphi version for Import');
  end;

  // 2. Define Targets
  ProgramFiles := GetEnvironmentVariable('ProgramFiles(x86)');
  if ProgramFiles = '' then
    ProgramFiles := GetEnvironmentVariable('ProgramFiles');

  TargetBDS := IncludeTrailingPathDelimiter(ProgramFiles) + 'Embarcadero\Studio\' + ProductVersion;

  AppDataDir := GetEnvironmentVariable('APPDATA');
  TargetAppData := AppDataDir + '\Embarcadero\BDS\' + ProductVersion;

  Writeln('Importing Build Environment for Delphi ' + DelphiVersionStringArray[DelphiVersion] + ' (' + ProductVersion + ')...');
  Writeln('  Source: ' + SourceRoot);
  Writeln('  Target BDS: ' + TargetBDS);
  Writeln('  Target AppData: ' + TargetAppData);

  // 3. Copy BDS Files
  Writeln('Restoring BDS files...');
  CopyDirectory(SourceBDS, TargetBDS);

  // 4. Copy AppData Files
  Writeln('Restoring AppData files...');
  SourceAppData := IncludeTrailingPathDelimiter(SourceRoot) + 'AppData';
  if not DirectoryExists(TargetAppData) then
    TDirectory.CreateDirectory(TargetAppData);

  if FileExists(SourceAppData + '\EnvOptions.proj') then
  begin
    Writeln('  Restoring EnvOptions.proj...');
    TFile.Copy(SourceAppData + '\EnvOptions.proj', TargetAppData + '\EnvOptions.proj', True);
  end;

  if FileExists(SourceAppData + '\environment.proj') then
  begin
    Writeln('  Restoring environment.proj...');
    TFile.Copy(SourceAppData + '\environment.proj', TargetAppData + '\environment.proj', True);
  end;

  // 5. Import Registry
  ImportRegistry;

  Writeln('Build Environment Import Complete.');
end;

end.
