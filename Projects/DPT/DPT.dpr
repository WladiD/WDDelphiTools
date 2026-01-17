// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

program DPT;

{$APPTYPE CONSOLE}

uses

  Winapi.Windows,

  System.Classes,
  System.SysUtils,
  System.Win.Registry,
  System.NetEncoding,

  {$IFDEF FITNESSE}
  Slim.Server,
  Slim.Fixture,
  Slim.CmdUtils,
  DPT.Fixtures,
  {$ENDIF}

  JclIDEUtils,

  DPT.OpenUnitTask,
  DPT.Types;

type

  TDPRemovePackageTaskBase = class(TDPTaskBase)
  protected
    function IsPackageMatching(const PackageFileName: String): Boolean; virtual; abstract;
  public
    procedure Execute; override;
  end;

  TDPRemovePackagesBySourceDirTask = class(TDPRemovePackageTaskBase)
  protected
    function IsPackageMatching(const PackageFileName: String): Boolean; override;
  public
    SourceDir: String;
    procedure Execute; override;
  end;

  TDPRemovePackageTask = class(TDPRemovePackageTaskBase)
  protected
    function IsPackageMatching(const PackageFileName: String): Boolean; override;
  public
    PackageFileName: String;
    procedure Execute; override;
  end;

  TDPRegisterPackageTask = class(TDPTaskBase)
  public
    PathToBPL: String;
    procedure Execute; override;
  end;

  TDPPrintPathTask = class(TDPTaskBase)
  public
    PathToPrint: String;
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

function IsValidDelphiVersion(VersionString: String; out DelphiVersion: TDelphiVersion): Boolean;
begin
  Result := True;
  for var Loop: Integer := 1 to Integer(High(TDelphiVersion)) do
  begin
    DelphiVersion := TDelphiVersion(Loop);
    if VersionString = DelphiVersionStringArray[DelphiVersion] then
      Exit;
  end;
  DelphiVersion := dvUnknown;
  Result := False;
end;

function FindMostRecentDelphiVersion: TDelphiVersion;
var
  Installations: TJclBorRADToolInstallations;
begin
  Result := dvUnknown;
  Installations := TJclBorRADToolInstallations.Create;
  try
    for var Loop: Integer := Integer(High(TDelphiVersion)) downto 1 do
    begin
      if Installations.DelphiVersionInstalled[DelphiVersionIntegerArray[TDelphiVersion(Loop)]] then
      begin
        Result := TDelphiVersion(Loop);
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

{ TDPRemovePackagesBySourceDirTask }

function TDPRemovePackagesBySourceDirTask.IsPackageMatching(const PackageFileName: String): Boolean;
begin
  Result := Pos(SourceDir, LowerCase(PackageFileName)) = 1;
end;

procedure TDPRemovePackagesBySourceDirTask.Execute;
begin
  SourceDir := LowerCase(SourceDir);
  inherited Execute;
end;

{ TDPRemovePackageTask }

function TDPRemovePackageTask.IsPackageMatching(const PackageFileName: String): Boolean;
var
  ComparePFN: String;
  ExtLength : Integer;
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
  LPP, OutputPath: String;
begin
  LPP := UpperCase(PathToPrint);
  if LPP = UpperCase(ValidPathBds) then
    OutputPath := Installation.RootDir
  else if LPP = UpperCase(ValidPathBdsBin) then
    OutputPath := Installation.BinFolderName
  else if LPP = UpperCase(ValidPathBplOutputWin32) then
    OutputPath := Installation.BPLOutputPath[bpWin32]
  else if LPP = UpperCase(ValidPathBplOutputWin64) then
    OutputPath := Installation.BPLOutputPath[bpWin64]
  else if LPP = UpperCase(ValidPathDcpOutputWin32) then
    OutputPath := Installation.DCPOutputPath[bpWin32]
  else if LPP = UpperCase(ValidPathDcpOutputWin64) then
    OutputPath := Installation.DCPOutputPath[bpWin64]
  else
    OutputPath := '';

  Writeln(ExcludeTrailingPathDelimiter(OutputPath));
end;

procedure ProcessCMDLine;
var
  CMDLine      : TCMDLineConsumer;
  DelphiVersion: TDelphiVersion;
  DPTask       : TDPTaskBase;
  ParamValue   : String;

  procedure InitDPTask(DPTaskClass: TDPTaskClass);
  begin
    DPTask := DPTaskClass.Create;
    DPTask.DelphiVersion := DelphiVersion;
  end;

  procedure SerializeRemovePackagesBySourceDirTask;
  var
    LocalDPTask: TDPRemovePackagesBySourceDirTask absolute DPTask;
    SourceDir  : String;
  begin
    InitDPTask(TDPRemovePackagesBySourceDirTask);

    SourceDir := CMDLine.CheckParameter('SourceDir');
    LocalDPTask.SourceDir := SourceDir;

    Writeln('Unregister design time packages contained in "' + SourceDir + '"...');
  end;

  procedure SerializeRemovePackageTask;
  var
    LocalDPTask    : TDPRemovePackageTask absolute DPTask;
    PackageFileName: String;
  begin
    InitDPTask(TDPRemovePackageTask);

    PackageFileName := CMDLine.CheckParameter('PackageFileName');
    LocalDPTask.PackageFileName := PackageFileName;

    Writeln('Unregister design time package "' + PackageFileName + '"...');
  end;

  procedure SerializeRegisterPackageTask;
  var
    LocalDPTask: TDPRegisterPackageTask absolute DPTask;
    PathToBPL  : String;
  begin
    InitDPTask(TDPRegisterPackageTask);

    PathToBPL := CMDLine.CheckParameter('PathToBPL');
    LocalDPTask.PathToBPL := PathToBPL;

    Writeln(Format('Register design time package "%s"...', [PathToBPL]));
  end;

  procedure SerializePrintPathTask;
  var
    LocalDPTask: TDPPrintPathTask absolute DPTask;
    PathToPrint: String;
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
    FullPathToUnit: String;
    LocalDPTask   : TDPOpenUnitTask absolute DPTask;
    NextParam     : String;
  begin
    InitDPTask(TDPOpenUnitTask);

    FullPathToUnit := CMDLine.CheckParameter('FullPathToUnit');
    CMDLine.ConsumeParameter; // Consume file path
    LocalDPTask.FullPathToUnit := FullPathToUnit;

    while CMDLine.HasParameter do
    begin
       NextParam := CMDLine.CheckParameter('Optional: GoToLine / GoToMemberImplementation');
       if SameText(NextParam, 'GoToLine') then
       begin
         CMDLine.ConsumeParameter; // Consume 'GoToLine' keyword
         LocalDPTask.GoToLine := StrToIntDef(CMDLine.CheckParameter('LineNumber'), 0);
         CMDLine.ConsumeParameter; // Consume line number
       end
       else if SameText(NextParam, 'GoToMemberImplementation') then
       begin
         CMDLine.ConsumeParameter; // Consume keyword
         LocalDPTask.MemberImplementation := CMDLine.CheckParameter('MemberName');
         CMDLine.ConsumeParameter; // Consume value
       end
       else
         Break;
    end;

    Writeln('Opening unit "' + FullPathToUnit + '"...');
  end;

  procedure SerializeHandleProtocolTask;
  var
    URL, Command, ParamsStr: String;
    Params: TStringList;
    LocalDPTask: TDPOpenUnitTask absolute DPTask;
    QPos: Integer;
  begin
    URL := CMDLine.CheckParameter('URL');
    CMDLine.ConsumeParameter;

    // Remove dpt://
    if Pos('dpt://', LowerCase(URL)) <> 1 then
      raise Exception.Create('Invalid protocol');

    Delete(URL, 1, 6); // remove dpt://

    // Split Command and Params
    QPos := Pos('?', URL);
    if QPos > 0 then
    begin
      Command := Copy(URL, 1, QPos - 1);
      ParamsStr := Copy(URL, QPos + 1, Length(URL));
    end
    else
    begin
      Command := URL;
      ParamsStr := '';
    end;

    // Remove trailing slash from command if present
    if (Length(Command) > 0) and (Command[Length(Command)] = '/') then
      Delete(Command, Length(Command), 1);

    if SameText(Command, 'openunit') then
    begin
      InitDPTask(TDPOpenUnitTask);

      Params := TStringList.Create;
      try
        Params.Delimiter := '&';
        Params.StrictDelimiter := True;
        Params.DelimitedText := ParamsStr;

        LocalDPTask.FullPathToUnit := TNetEncoding.URL.Decode(Params.Values['file']);
        LocalDPTask.GoToLine := StrToIntDef(Params.Values['line'], 0);
        LocalDPTask.MemberImplementation := TNetEncoding.URL.Decode(Params.Values['member']);

        Writeln('Opening unit "' + LocalDPTask.FullPathToUnit + '"...');
      finally
        Params.Free;
      end;
    end
    else
      raise Exception.Create('Unknown command: ' + Command);
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

    ParamValue := CMDLine.CheckParameter('Mode');
    if SameText(ParamValue, 'RemovePackagesBySourceDir') then
    begin
      CMDLine.ConsumeParameter;
      SerializeRemovePackagesBySourceDirTask;
    end
    else if SameText(ParamValue, 'RemovePackage') then
    begin
      CMDLine.ConsumeParameter;
      SerializeRemovePackageTask;
    end
    else if SameText(ParamValue, 'RegisterPackage') then
    begin
      CMDLine.ConsumeParameter;
      SerializeRegisterPackageTask;
    end
    else if SameText(ParamValue, 'PrintPath') then
    begin
      CMDLine.ConsumeParameter;
      SerializePrintPathTask;
    end
    else if SameText(ParamValue, 'OpenUnit') then
    begin
      CMDLine.ConsumeParameter;
      SerializeOpenUnitTask;
    end
    else if SameText(ParamValue, 'HandleProtocol') then
    begin
      CMDLine.ConsumeParameter;
      SerializeHandleProtocolTask;
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

  for var Loop: Integer := 1 to Integer(High(TDelphiVersion)) do
    Writeln('    ' + DelphiVersionStringArray[TDelphiVersion(Loop)]);

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
  Writeln('    OpenUnit FullPathToUnit [GoToLine LineNumber] [GoToMemberImplementation Class.Member]');
  Writeln('      Opens the specified unit in the IDE. Starts IDE if not running.');
  Writeln;
  Writeln('    HandleProtocol dpt://Command/?Params');
  Writeln('      Handles URL protocol requests (e.g. dpt://openunit/?file=...&line=...&member=...)');
end;

{$IFDEF FITNESSE}
type TSlimFixtureResolverHelper = class(TSlimFixtureResolver);
{$ENDIF}

begin
  try
    {$IFDEF FITNESSE}
    var SlimServer: TSlimServer := TSlimServer.Create(nil);
    try
      var LPort: Integer;
      if not HasSlimPortParam(LPort) then
        LPort := 9000;

      SlimServer.DefaultPort := LPort;
      SlimServer.Active := True;

      Writeln('Slim Server started on port ', LPort);
      
      Writeln('Registered Fixtures:');
      for var Loop: Integer := 0 to TSlimFixtureResolverHelper.FFixtures.Count - 1 do
        Writeln('  ', TClass(TSlimFixtureResolverHelper.FFixtures[Loop]).ClassName);

      Writeln('Server running... (Ctrl+C or call StopServer to stop)');
      StopServerEvent.WaitFor(INFINITE);
    finally
      SlimServer.Free;
    end;
    {$ELSE}
    if ParamCount > 1 then
      ProcessCMDLine
    else
      InstructionScreen;
    {$ENDIF}
  except
    on E:Exception do
    begin
      Writeln(E.Classname, ': ', E.Message);
      ExitCode := Integer(E.ClassType);
    end;
  end;
end.