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
  System.Contnrs,

  {$IFDEF FITNESSE}
  Slim.Server,
  Slim.Fixture,
  Slim.CmdUtils,
  DPT.Fixtures,
  {$ENDIF}

  JclIDEUtils,

  DPT.InstructionScreen,
  DPT.OpenUnitTask,
  DPT.Tasks,
  DPT.Types;

procedure ProcessCmdLine;
var
  CmdLine      : TCmdLineConsumer;
  DelphiVersion: TDelphiVersion;
  DptTask      : TDptTaskBase;
  ParamValue   : String;

  procedure InitDptTask(DPTaskClass: TDptTaskClass);
  begin
    DptTask := DPTaskClass.Create;
    DptTask.DelphiVersion := DelphiVersion;
  end;

  procedure SerializeRemovePackagesBySourceDirTask;
  var
    LocalDPTask: TDptRemovePackagesBySourceDirTask absolute DptTask;
    SourceDir  : String;
  begin
    InitDptTask(TDptRemovePackagesBySourceDirTask);

    SourceDir := CmdLine.CheckParameter('SourceDir');
    CmdLine.ConsumeParameter;
    LocalDPTask.SourceDir := SourceDir;

    Writeln('Unregister design time packages contained in "' + SourceDir + '"...');
  end;

  procedure SerializeRemovePackageTask;
  var
    LocalDPTask    : TDptRemovePackageTask absolute DptTask;
    PackageFileName: String;
  begin
    InitDptTask(TDptRemovePackageTask);

    PackageFileName := CmdLine.CheckParameter('PackageFileName');
    CmdLine.ConsumeParameter;
    LocalDPTask.PackageFileName := PackageFileName;

    Writeln('Unregister design time package "' + PackageFileName + '"...');
  end;

  procedure SerializeRegisterPackageTask;
  var
    LocalDPTask: TDptRegisterPackageTask absolute DptTask;
    PathToBPL  : String;
  begin
    InitDptTask(TDptRegisterPackageTask);

    PathToBPL := CmdLine.CheckParameter('PathToBPL');
    CmdLine.ConsumeParameter;
    LocalDPTask.PathToBPL := PathToBPL;

    Writeln(Format('Register design time package "%s"...', [PathToBPL]));
  end;

  procedure SerializeIsPackageRegisteredTask;
  var
    LocalDPTask    : TDptIsPackageRegisteredTask absolute DptTask;
    PackageFileName: String;
  begin
    InitDptTask(TDptIsPackageRegisteredTask);

    PackageFileName := CmdLine.CheckParameter('PackageFileName');
    CmdLine.ConsumeParameter;
    LocalDPTask.PackageFileName := PackageFileName;

    Writeln(Format('Checking if package "%s" is registered...', [PackageFileName]));
  end;

  procedure SerializePrintPathTask;
  var
    LocalDPTask: TDptPrintPathTask absolute DptTask;
    PathToPrint: String;
  begin
    InitDptTask(TDptPrintPathTask);

    PathToPrint := CmdLine.CheckParameter('PathToPrint');
    if Pos('|' + UpperCase(PathToPrint) + '|',  '|' + UpperCase(ValidPathToPrint) + '|') >= 1 then
      CmdLine.ConsumeParameter
    else
      CmdLine.InvalidParameter('Unknown path literal');

    LocalDPTask.PathToPrint := PathToPrint;
  end;

  procedure SerializeOpenUnitTask;
  var
    FullPathToUnit: String;
    LocalDPTask   : TDptOpenUnitTask absolute DptTask;
    NextParam     : String;
  begin
    InitDptTask(TDptOpenUnitTask);

    FullPathToUnit := CmdLine.CheckParameter('FullPathToUnit');
    CmdLine.ConsumeParameter; // Consume file path
    LocalDPTask.FullPathToUnit := FullPathToUnit;

    while CmdLine.HasParameter do
    begin
       NextParam := CmdLine.CheckParameter('Optional: GoToLine / GoToMemberImplementation');
       if SameText(NextParam, 'GoToLine') then
       begin
         CmdLine.ConsumeParameter; // Consume 'GoToLine' keyword
         LocalDPTask.GoToLine := StrToIntDef(CmdLine.CheckParameter('LineNumber'), 0);
         CmdLine.ConsumeParameter; // Consume line number
       end
       else if SameText(NextParam, 'GoToMemberImplementation') then
       begin
         CmdLine.ConsumeParameter; // Consume keyword
         LocalDPTask.MemberImplementation := CmdLine.CheckParameter('MemberName');
         CmdLine.ConsumeParameter; // Consume value
       end
       else
         Break;
    end;

    Writeln('Opening unit "' + FullPathToUnit + '"...');
  end;

  procedure SerializeBuildTask;
  var
    LocalDPTask: TDptBuildTask absolute DptTask;
  begin
    InitDptTask(TDptBuildTask);

    // ProjectFile (Required)
    LocalDPTask.ProjectFile := CmdLine.CheckParameter('ProjectFile');
    CmdLine.ConsumeParameter;

    // Platform (Optional)
    if CmdLine.HasParameter then
    begin
      LocalDPTask.TargetPlatform := CmdLine.CheckParameter('Platform');
      CmdLine.ConsumeParameter;
    end
    else
      LocalDPTask.TargetPlatform := 'Win32';

    // Config (Optional)
    if CmdLine.HasParameter then
    begin
      LocalDPTask.Config := CmdLine.CheckParameter('Config');
      CmdLine.ConsumeParameter;
    end
    else
      LocalDPTask.Config := 'Debug';

    // ExtraArgs (Optional - consume all remaining)
    LocalDPTask.ExtraArgs := '';
    while CmdLine.HasParameter do
    begin
       LocalDPTask.ExtraArgs := LocalDPTask.ExtraArgs + ' ' + CmdLine.CheckParameter('ExtraArg');
       CmdLine.ConsumeParameter;
    end;
    LocalDPTask.ExtraArgs := Trim(LocalDPTask.ExtraArgs);
  end;

  procedure SerializeHandleProtocolTask;
  var
    URL, Command, ParamsStr: String;
    Params: TStringList;
    LocalDPTask: TDptOpenUnitTask absolute DptTask;
    QPos: Integer;
  begin
    URL := CmdLine.CheckParameter('URL');
    CmdLine.ConsumeParameter;

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
      InitDptTask(TDptOpenUnitTask);

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
  DptTask := nil;
  CmdLine := TCmdLineConsumer.Create;
  try
    ParamValue := CmdLine.CheckParameter('DelphiVersion');
    if SameText(ParamValue, 'RECENT') then
    begin
      DelphiVersion := FindMostRecentDelphiVersion;
      if DelphiVersion = dvUnknown then
        raise Exception.Create('No supported Delphi version found on this machine');
      CmdLine.ConsumeParameter;
    end
    else if IsValidDelphiVersion(UpperCase(ParamValue), DelphiVersion) then
      CmdLine.ConsumeParameter
    else
      CmdLine.InvalidParameter('Not accepted version');

    ParamValue := CmdLine.CheckParameter('Action');
    if SameText(ParamValue, 'RemovePackagesBySourceDir') then
    begin
      CmdLine.ConsumeParameter;
      SerializeRemovePackagesBySourceDirTask;
    end
    else if SameText(ParamValue, 'Build') then
    begin
      CmdLine.ConsumeParameter;
      SerializeBuildTask;
    end
    else if SameText(ParamValue, 'RemovePackage') then
    begin
      CmdLine.ConsumeParameter;
      SerializeRemovePackageTask;
    end
    else if SameText(ParamValue, 'RegisterPackage') then
    begin
      CmdLine.ConsumeParameter;
      SerializeRegisterPackageTask;
    end
    else if SameText(ParamValue, 'IsPackageRegistered') then
    begin
      CmdLine.ConsumeParameter;
      SerializeIsPackageRegisteredTask;
    end
    else if SameText(ParamValue, 'PrintPath') then
    begin
      CmdLine.ConsumeParameter;
      SerializePrintPathTask;
    end
    else if SameText(ParamValue, 'OpenUnit') then
    begin
      CmdLine.ConsumeParameter;
      SerializeOpenUnitTask;
    end
    else if SameText(ParamValue, 'HandleProtocol') then
    begin
      CmdLine.ConsumeParameter;
      SerializeHandleProtocolTask;
    end
    else if SameText(ParamValue, 'Start') then
    begin
      CmdLine.ConsumeParameter;
      InitDptTask(TDptStartTask);
    end
    else if SameText(ParamValue, 'Stop') then
    begin
      CmdLine.ConsumeParameter;
      InitDptTask(TDptStopTask);
    end
    else
      CmdLine.InvalidParameter('Not accepted action');

    if Assigned(DptTask) then
      DptTask.Execute
    else
      raise Exception.Create('DptTask not initialized');
  finally
    DptTask.Free;
    CmdLine.Free;
  end;
end;

{$IFDEF FITNESSE}
type
  TSlimFixtureResolverHelper = class(TSlimFixtureResolver);
{$ENDIF}

begin
  try
    {$IFDEF FITNESSE}
    var LPort: Integer;
    var LIsSlimStart: Boolean;
    LIsSlimStart := HasSlimPortParam(LPort);

    // If no explicit port param and no other CLI params (<= 1, likely just exe name or empty), default to Slim Server on 9000
    if (not LIsSlimStart) and (ParamCount <= 1) then
    begin
      LIsSlimStart := True;
      LPort := 9000;
    end;

    if LIsSlimStart then
    begin
      var SlimServer: TSlimServer := TSlimServer.Create(nil);
      try
        SlimServer.DefaultPort := LPort;
        SlimServer.Active := True;

        Writeln('Slim Server started on port ', LPort);

        Writeln('Registered Fixtures:');
        for var Loop: Integer := 0 to TSlimFixtureResolverHelper.FFixtures.Count - 1 do
          Writeln('  ', TClass(TSlimFixtureResolverHelper.FFixtures[Loop]).ClassName);

        Writeln('Server running... (Ctrl+C or call StopServer to stop)');
        TDptControl.StopServerEvent.WaitFor(INFINITE);
      finally
        SlimServer.Free;
      end;
      Exit;
    end;
    {$ENDIF}

    // Check for Help command
    if (ParamCount >= 1) and (SameText(ParamStr(1), 'Help') or SameText(ParamStr(1), '-h') or SameText(ParamStr(1), '/?')) then
    begin
      TDptInstructionScreen.ShowHelp(ParamStr(2));
      Exit;
    end;

    // Always process CLI commands if arguments are present (Version + Action)
    if ParamCount > 1 then
    begin
      ProcessCmdLine;
      Exit;
    end;

    {$IFNDEF FITNESSE}
    TDptInstructionScreen.ShowCompact;
    {$ENDIF}
  except
    on E:Exception do
    begin
      Writeln(E.Classname, ': ', E.Message);
      ExitCode := Integer(E.ClassType);
    end;
  end;
end.
