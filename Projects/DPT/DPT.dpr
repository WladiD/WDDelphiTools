program DPT;

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Win.Registry,

  JclIDEUtils;

type
  EInvalidParameter = class(Exception);

  TCMDLineConsumer = class
  private
    FCurrentParameter: Integer;
    FCurrentMeaningParam: string;

  public
    constructor Create;

    function HasParameter: Boolean;
    function CheckParameter(MeaningParam: string): string;
    procedure ConsumeParameter;
    procedure InvalidParameter(ErrorMessage: string);
  end;

  TDelphiVersion = (dvUnknown, dvD2007, dvD10_1, dvD10_3);

  TDPTaskBase = class;
  TDPTaskClass = class of TDPTaskBase;

  TDPTaskBase = class
  private
    FInstallation: TJclBorRADToolInstallation;
    FInstallations: TJclBorRADToolInstallations;

  protected
    procedure Output(const Text: string);

    function Installation: TJclBorRADToolInstallation;

  public
    DelphiVersion: TDelphiVersion;
    IsX64: Boolean;

    destructor Destroy; override;

    procedure Execute; virtual; abstract;
  end;

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
  DelphiVersionStringArray: array [TDelphiVersion] of string = ('', 'D2007', 'D10.1', 'D10.3');
  DelphiVersionIntegerArray: array [TDelphiVersion] of Integer = (0, 11, 24, 26);
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

{ TCMDLineConsumer }

constructor TCMDLineConsumer.Create;
begin
  FCurrentParameter := 1;
end;

function TCMDLineConsumer.HasParameter: Boolean;
begin
  Result := FCurrentParameter <= ParamCount;
end;

function TCMDLineConsumer.CheckParameter(MeaningParam: string): string;
begin
  FCurrentMeaningParam := MeaningParam;

  if not HasParameter then
    InvalidParameter('Parameter not available');

  Result := ParamStr(FCurrentParameter);
end;

procedure TCMDLineConsumer.ConsumeParameter;
begin
  Inc(FCurrentParameter);
end;

procedure TCMDLineConsumer.InvalidParameter(ErrorMessage: string);
begin
  if HasParameter then
    raise EInvalidParameter.CreateFmt('%s = "%s": %s', [FCurrentMeaningParam,
      ParamStr(FCurrentParameter), ErrorMessage])
  else
    raise EInvalidParameter.CreateFmt('%s: %s', [FCurrentMeaningParam, ErrorMessage]);
end;

{ TDPTaskBase }

destructor TDPTaskBase.Destroy;
begin
  FInstallations.Free;

  inherited Destroy;
end;

function TDPTaskBase.Installation: TJclBorRADToolInstallation;
var
  DelphiVersionAsInt: Integer;
begin
  if Assigned(FInstallation) then
  begin
    Result := FInstallation;
    Exit;
  end;

  if not Assigned(FInstallations) then
    FInstallations := TJclBorRADToolInstallations.Create;

  DelphiVersionAsInt := DelphiVersionIntegerArray[DelphiVersion];

  if not FInstallations.DelphiVersionInstalled[DelphiVersionAsInt] then
    raise Exception.CreateFmt('Delphi Version %s (%d) not installed',
      [DelphiVersionStringArray[DelphiVersion], DelphiVersionAsInt]);

  FInstallation := FInstallations.DelphiInstallationFromVersion[DelphiVersionAsInt];
  FInstallation.OutputCallback := Output;
  Result := FInstallation;
end;

procedure TDPTaskBase.Output(const Text: string);
begin
  Writeln(Text);
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

begin
  DPTask := nil;
  CMDLine := TCMDLineConsumer.Create;
  try
    ParamValue := CMDLine.CheckParameter('DelphiVersion');
    if IsValidDelphiVersion(UpperCase(ParamValue), DelphiVersion) then
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
  Writeln('0.03a - 2020 - Waldemar Derr');
  Writeln('https://github.com/WladiD/WDDelphiTools/tree/master/Projects/DPTCmd');
  Writeln;
  Writeln('Usage:');
  Writeln(ExtractFileName(ParamStr(0)) + ' DelphiVersion Mode [OtherModeSpecificParameters]');
  Writeln;
  Writeln('  DelphiVersion');

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
