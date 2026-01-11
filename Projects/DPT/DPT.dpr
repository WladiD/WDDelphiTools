program DPT;

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Win.Registry,

  JclIDEUtils,
  DPT.Types,
  DPT.OpenUnitTask;

type
  TDPRemovePackageTaskBase = class(TDPTaskBase)
  protected
    function IsPackageMatching(const PackageFileName: string): Boolean; virtual; abstract;
  public
    procedure Execute; override;
  end;

  TDPRemovePackagesBySourceDirTask = class(TDPRemovePackageTaskBase)
  protected
    function IsPackageMatching(const PackageFileName: string): Boolean; override;
  public
    SourceDir: string;

    procedure Execute; override;
  end;

  TDPRemovePackageTask = class(TDPRemovePackageTaskBase)
  protected
    function IsPackageMatching(const PackageFileName: string): Boolean; override;
  public
    PackageFileName: string;

    procedure Execute; override;
  end;

  TDPRegisterPackageTask = class(TDPTaskBase)
  public
    PathToBPL: string;

    procedure Execute; override;
  end;

  TDPPrintPathTask = class(TDPTaskBase)
  public
    PathToPrint: string;

    procedure Execute; override;
  end;

const
  ValidPathToPrint: string = 'BDSPath|BDSBINPath|BPLOutputPath-Win32|BPLOutputPath-Win64|DCPOutputPath-Win32|DCPOutputPath-Win64';

function IsValidDelphiVersion(VersionString: string; out DelphiVersion: TDelphiVersion): Boolean;
var
  cc: Integer;
begin
  Result := TRUE;
  for cc := 1 to Integer(High(TDelphiVersion)) do
  begin
    DelphiVersion := TDelphiVersion(cc);
    if VersionString = DelphiVersionStringArray[DelphiVersion] then
      Exit;
  end;
  DelphiVersion := dvUnknown;
  Result := FALSE;
end;

function FindMostRecentDelphiVersion: TDelphiVersion;
var
  Installations: TJclBorRADToolInstallations;
  cc: Integer;
begin
  Result := dvUnknown;
  Installations := TJclBorRADToolInstallations.Create;
  try
    for cc := Integer(High(TDelphiVersion)) downto 1 do
    begin
      if Installations.DelphiVersionInstalled[DelphiVersionIntegerArray[TDelphiVersion(cc)]] then
      begin
        Result := TDelphiVersion(cc);
        Break;
      end;
    end;
  finally
    Installations.Free;
  end;
end;

{ TDPRemovePackageTaskBase }

procedure TDPRemovePackageTaskBase.Execute;
var
  cc: Integer;
  PackageFileName: string;
  DeletePackageList: TStrings;
begin
  DeletePackageList := TStringList.Create;
  try
    for cc := 0 to Installation.IdePackages.Count[IsX64] - 1 do
    begin
      PackageFileName := Installation.IdePackages.PackageFileNames[cc, IsX64];
      if IsPackageMatching(PackageFileName) then
        DeletePackageList.Add(PackageFileName);
    end;

    for cc := 0 to DeletePackageList.Count - 1 do
    begin
      PackageFileName := DeletePackageList[cc];
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

{ TDPRemovePackagesBySourceDirTask }

function TDPRemovePackagesBySourceDirTask.IsPackageMatching(const PackageFileName: string): Boolean;
begin
  Result := Pos(SourceDir, LowerCase(PackageFileName)) = 1;
end;

procedure TDPRemovePackagesBySourceDirTask.Execute;
begin
  SourceDir := LowerCase(SourceDir);
  inherited Execute;
end;

{ TDPRemovePackageTask }

function TDPRemovePackageTask.IsPackageMatching(const PackageFileName: string): Boolean;
var
  ComparePFN: string;
  ExtLength: Integer;
begin
  ComparePFN := LowerCase(ExtractFileName(PackageFileName));
  ExtLength := Length(ExtractFileExt(ComparePFN));
  ComparePFN := Copy(ComparePFN, 1, Length(ComparePFN) - ExtLength);
  Result := ComparePFN = Self.PackageFileName;
end;

procedure TDPRemovePackageTask.Execute;
begin
  PackageFileName := LowerCase(PackageFileName);
  inherited Execute;
end;

{ TDPInstallPackageTask }

procedure TDPRegisterPackageTask.Execute;
begin
  Installation.RegisterPackage(PathToBPL, '');
end;

{ TDPPrintPathTask }

procedure TDPPrintPathTask.Execute;
var
  LPP, OutputPath: string;
begin
  LPP := UpperCase(PathToPrint);
  if LPP = UpperCase('BDSPath') then
    OutputPath := Installation.RootDir
  else if LPP = UpperCase('BDSBINPath') then
    OutputPath := Installation.BinFolderName
  else if LPP = UpperCase('BPLOutputPath-Win32') then
    OutputPath := Installation.BPLOutputPath[bpWin32]
  else if LPP = UpperCase('BPLOutputPath-Win64') then
    OutputPath := Installation.BPLOutputPath[bpWin64]
  else if LPP = UpperCase('DCPOutputPath-Win32') then
    OutputPath := Installation.DCPOutputPath[bpWin32]
  else if LPP = UpperCase('DCPOutputPath-Win64') then
    OutputPath := Installation.DCPOutputPath[bpWin64]
  else
    OutputPath := '';

  Writeln(ExcludeTrailingPathDelimiter(OutputPath));
end;

procedure ProcessCMDLine;
var
  CMDLine: TCMDLineConsumer;
  ParamValue: string;
  DelphiVersion: TDelphiVersion;
  DPTask: TDPTaskBase;

  procedure InitDPTask(DPTaskClass: TDPTaskClass);
  begin
    DPTask := DPTaskClass.Create;
    DPTask.DelphiVersion := DelphiVersion;
  end;

  procedure SerializeRemovePackagesBySourceDirTask;
  var
    LocalDPTask: TDPRemovePackagesBySourceDirTask absolute DPTask;
    SourceDir: string;
  begin
    InitDPTask(TDPRemovePackagesBySourceDirTask);

    SourceDir := CMDLine.CheckParameter('SourceDir');
    LocalDPTask.SourceDir := SourceDir;

    Writeln('Unregister design time packages contained in "' + SourceDir + '"...');
  end;

  procedure SerializeRemovePackageTask;
  var
    LocalDPTask: TDPRemovePackageTask absolute DPTask;
    PackageFileName: string;
  begin
    InitDPTask(TDPRemovePackageTask);

    PackageFileName := CMDLine.CheckParameter('PackageFileName');
    LocalDPTask.PackageFileName := PackageFileName;

    Writeln('Unregister design time package "' + PackageFileName + '"...');
  end;

  procedure SerializeRegisterPackageTask;
  var
    LocalDPTask: TDPRegisterPackageTask absolute DPTask;
    PathToBPL: string;
  begin
    InitDPTask(TDPRegisterPackageTask);

    PathToBPL := CMDLine.CheckParameter('PathToBPL');
    LocalDPTask.PathToBPL := PathToBPL;

    Writeln(Format('Register design time package "%s"...', [PathToBPL]));
  end;

  procedure SerializePrintPathTask;
  var
    LocalDPTask: TDPPrintPathTask absolute DPTask;
    PathToPrint: string;
  begin
    InitDPTask(TDPPrintPathTask);

    PathToPrint := CMDLine.CheckParameter('PathToPrint');
    if Pos('|' + UpperCase(PathToPrint) + '|',  '|' + UpperCase(ValidPathToPrint) + '|') >= 1 then
      CMDLine.ConsumeParameter
    else
      CMDLine.InvalidParameter('Unknown path literal');

    LocalDPTask.PathToPrint := PathToPrint;
  end;

  procedure SerializeOpenUnitTask;
  var
    LocalDPTask: TDPOpenUnitTask absolute DPTask;
    FullPathToUnit: string;
    NextParam: string;
  begin
    InitDPTask(TDPOpenUnitTask);

    FullPathToUnit := CMDLine.CheckParameter('FullPathToUnit');
    CMDLine.ConsumeParameter; // Consume file path
    LocalDPTask.FullPathToUnit := FullPathToUnit;

    if CMDLine.HasParameter then
    begin
       NextParam := CMDLine.CheckParameter('Optional: GoToLine');
       if SameText(NextParam, 'GoToLine') then
       begin
         CMDLine.ConsumeParameter; // Consume 'GoToLine' keyword
         LocalDPTask.GoToLine := StrToIntDef(CMDLine.CheckParameter('LineNumber'), 0);
         CMDLine.ConsumeParameter; // Consume line number
       end;
    end;

    Writeln('Opening unit "' + FullPathToUnit + '"...');
  end;

begin
  DPTask := nil;
  CMDLine := TCMDLineConsumer.Create;
  try
    ParamValue := CMDLine.CheckParameter('DelphiVersion');
    if SameText(ParamValue, 'RECENT') then
    begin
      DelphiVersion := FindMostRecentDelphiVersion;
      if DelphiVersion = dvUnknown then
        raise Exception.Create('No supported Delphi version found on this machine');
      CMDLine.ConsumeParameter;
    end
    else if IsValidDelphiVersion(UpperCase(ParamValue), DelphiVersion) then
      CMDLine.ConsumeParameter
    else
      CMDLine.InvalidParameter('Not accepted version');

    ParamValue := UpperCase(CMDLine.CheckParameter('Mode'));
    if ParamValue = UpperCase('RemovePackagesBySourceDir') then
    begin
      CMDLine.ConsumeParameter;
      SerializeRemovePackagesBySourceDirTask;
    end
    else if ParamValue = UpperCase('RemovePackage') then
    begin
      CMDLine.ConsumeParameter;
      SerializeRemovePackageTask;
    end
    else if ParamValue = UpperCase('RegisterPackage') then
    begin
      CMDLine.ConsumeParameter;
      SerializeRegisterPackageTask;
    end
    else if ParamValue = UpperCase('PrintPath') then
    begin
      CMDLine.ConsumeParameter;
      SerializePrintPathTask;
    end
    else if ParamValue = UpperCase('OpenUnit') then
    begin
      CMDLine.ConsumeParameter;
      SerializeOpenUnitTask;
    end
    else
      CMDLine.InvalidParameter('Not accepted mode');

    if Assigned(DPTask) then
      DPTask.Execute
    else
      raise Exception.Create('DPTask not initialized');
  finally
    DPTask.Free;
    CMDLine.Free;
  end;
end;

procedure InstructionScreen;
var
  cc: Integer;
begin
  Writeln('Delphi (Package) Tools');
  Writeln('0.05 - 2026 - Waldemar Derr');
  Writeln('https://github.com/WladiD/WDDelphiTools/tree/master/Projects/DPT');
  Writeln;
  Writeln('Usage:');
  Writeln(ExtractFileName(ParamStr(0)) + ' DelphiVersion Mode [OtherModeSpecificParameters]');
  Writeln;
  Writeln('  DelphiVersion');
  Writeln('    RECENT (automatically selects the newest installed version)');

  for cc := 1 to Integer(High(TDelphiVersion)) do
    Writeln('    ' + DelphiVersionStringArray[TDelphiVersion(cc)]);

  Writeln;
  Writeln('  Mode');
  Writeln('    RemovePackagesBySourceDir SourceDir');
  Writeln('      Removes the registration of design time packages for the defined');
  Writeln('      Delphi-IDE which are located in SourceDir');
  Writeln;
  Writeln('    RemovePackage PackageFileName');
  Writeln('      Removes the design time registration of the specified package by their');
  Writeln('      name (without file extension) only.');
  Writeln;
  Writeln('    RegisterPackage PathToBPL');
  Writeln('      Register a package specified in PathToBPL as design time package');
  Writeln;
  Writeln('    PrintPath (' + ValidPathToPrint + ')');
  Writeln('      Prints the path');
  Writeln;
  Writeln('    OpenUnit FullPathToUnit [GoToLine LineNumber]');
  Writeln('      Opens the specified unit in the IDE. Starts IDE if not running.');
end;

begin
  try
    if ParamCount > 1 then
      ProcessCMDLine
    else
      InstructionScreen;
  except
    on E:Exception do
    begin
      Writeln(E.Classname, ': ', E.Message);
      ExitCode := Integer(E.ClassType);
    end;
  end;
end.
