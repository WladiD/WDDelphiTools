// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Tasks;

interface

uses

  System.Classes,
  System.SysUtils,
  Winapi.Windows,

  JclIDEUtils,

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

  TDptStartTask = class(TDptTaskBase)
  public
    procedure Execute; override;
  end;

  TDptStopTask = class(TDptTaskBase)
  public
    procedure Execute; override;
  end;

  TDptBuildTask = class(TDptTaskBase)
  private
    function RunShellCommand(const CommandLine: String): Integer;
  public
    Config        : String;
    ExtraArgs     : String;
    ProjectFile   : String;
    TargetPlatform: String;
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
  // cmd.exe /c ""rsvars.bat" && msbuild "Project" /t:Build /p:Configuration=Config;Platform=Platform;PRODUCTVERSION=ProductVersion ExtraArgs"
  CmdLine := Format('/c ""%s" && msbuild "%s" /t:Build /p:Configuration=%s;Platform=%s;PRODUCTVERSION=%s %s"',
    [RsvarsPath, ProjectFile, Config, TargetPlatform, ProductVersion, ExtraArgs]);

  ExitCode := RunShellCommand('cmd.exe ' + CmdLine);

  if ExitCode <> 0 then
  begin
    Writeln('ERROR: Build failed with exit code ' + IntToStr(ExitCode));
    System.ExitCode := ExitCode;
  end
  else
    Writeln('Build successful.');
end;

end.
