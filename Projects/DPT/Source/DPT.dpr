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

  TmplCodeGen.Common,
  TmplCodeGen.Generator,
  TmplCodeGen.Includer,
  TmplCodeGen.Logger,
  TmplCodeGen.PreProcess,
  TmplCodeGen.Utils,

  DPT.InstructionScreen,
  DPT.OpenUnitTask,
  DPT.Preprocessor,
  DPT.Tasks,
  DPT.Types;

procedure ProcessCmdLine;
var
  CmdLine      : TCmdLineConsumer;
  DelphiVersion: TDelphiVersion;
  DptTask      : TDptTaskBase;
  ParamValue   : String;

  procedure CheckAndExecutePreProcessor(var AProjectFile: String);
  var
    PreProcessor: TDptPreprocessor;
  begin
    if not SameText(ExtractFileExt(AProjectFile), '.dproj') then
    begin
      PreProcessor := TDptPreprocessor.Create;
      try
        AProjectFile := PreProcessor.Execute(AProjectFile);
      finally
        PreProcessor.Free;
      end;
    end;
  end;

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
    LocalDPTask.ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
    CheckAndExecutePreProcessor(LocalDPTask.ProjectFile);
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

  procedure SerializeBuildAndRunTask;
  var
    Arg         : String;
    ArgsConsumed: Boolean;
    LocalDPTask : TDptBuildAndRunTask absolute DptTask;
  begin
    InitDptTask(TDptBuildAndRunTask);

    // ProjectFile (Required)
    LocalDPTask.ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
    CheckAndExecutePreProcessor(LocalDPTask.ProjectFile);
    CmdLine.ConsumeParameter;

    // Defaults
    LocalDPTask.TargetPlatform := 'Win32';
    LocalDPTask.Config := 'Debug';
    LocalDPTask.OnlyIfChanged := False;
    LocalDPTask.ExtraArgs := '';
    LocalDPTask.RunArgs := '';

    ArgsConsumed := False; // Flag to track if we hit "--"

    while CmdLine.HasParameter do
    begin
      Arg := CmdLine.CheckParameter('Args');

      if ArgsConsumed then
      begin
        // Append to RunArgs
        LocalDPTask.RunArgs := LocalDPTask.RunArgs + ' ' + Arg;
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
        LocalDPTask.OnlyIfChanged := True;
        CmdLine.ConsumeParameter;
        Continue;
      end;

      // Heuristic for Platform/Config/ExtraArgs
      // Standard Build Task logic: Platform -> Config -> ExtraArgs
      // But here order is flexible because of flags.
      // Let's stick to strict order for Platform/Config if they look like it?
      // Or rely on user passing them before flags.

      // Simplification: Assume standard order for Build params, but allow flags interspersed?
      // Better: Just check values against known platforms/configs or assume position.
      // Let's assume standard positional logic as much as possible, BUT allow OnlyIfChanged anywhere before --

      if (LocalDPTask.TargetPlatform = 'Win32') and ((SameText(Arg, 'Win32')) or (SameText(Arg, 'Win64'))) then
      begin
        LocalDPTask.TargetPlatform := Arg;
        CmdLine.ConsumeParameter;
      end
      else if (LocalDPTask.Config = 'Debug') and ((SameText(Arg, 'Debug')) or (SameText(Arg, 'Release')) or (SameText(Arg, 'FitNesse'))) then
      begin
        LocalDPTask.Config := Arg;
        CmdLine.ConsumeParameter;
      end
      else
      begin
        // Assume ExtraArg for MSBuild
        LocalDPTask.ExtraArgs := LocalDPTask.ExtraArgs + ' ' + Arg;
        CmdLine.ConsumeParameter;
      end;
    end;

    LocalDPTask.ExtraArgs := Trim(LocalDPTask.ExtraArgs);
    LocalDPTask.RunArgs := Trim(LocalDPTask.RunArgs);
  end;

  procedure SerializeDProjPrintConfigsTask;
  var
    LocalDPTask: TDptDProjPrintConfigsTask absolute DptTask;
  begin
    InitDptTask(TDptDProjPrintConfigsTask);
    LocalDPTask.ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
    CheckAndExecutePreProcessor(LocalDPTask.ProjectFile);
    CmdLine.ConsumeParameter;
  end;

  procedure SerializeDProjPrintCurConfigTask;
  var
    LocalDPTask: TDptDProjPrintCurConfigTask absolute DptTask;
  begin
    InitDptTask(TDptDProjPrintCurConfigTask);
    LocalDPTask.ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
    CheckAndExecutePreProcessor(LocalDPTask.ProjectFile);
    CmdLine.ConsumeParameter;
  end;

  procedure SerializeDProjPrintSearchPathsTask;
  var
    LocalDPTask: TDptDProjPrintSearchPathsTask absolute DptTask;
  begin
    InitDptTask(TDptDProjPrintSearchPathsTask);
    LocalDPTask.ProjectFile := ExpandFileName(CmdLine.CheckParameter('ProjectFile'));
    CheckAndExecutePreProcessor(LocalDPTask.ProjectFile);
    CmdLine.ConsumeParameter;

    if CmdLine.HasParameter then
    begin
      LocalDPTask.Config := CmdLine.CheckParameter('Config');
      CmdLine.ConsumeParameter;
    end;

    if CmdLine.HasParameter then
    begin
      LocalDPTask.Platform := CmdLine.CheckParameter('Platform');
      CmdLine.ConsumeParameter;
    end;
  end;

  procedure SerializeExportBuildEnvironmentTask;
  var
    LocalDPTask: TDptExportBuildEnvironmentTask absolute DptTask;
  begin
    InitDptTask(TDptExportBuildEnvironmentTask);
    LocalDPTask.TargetPath := ExpandFileName(CmdLine.CheckParameter('TargetPath'));
    CmdLine.ConsumeParameter;
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
    else if SameText(ParamValue, 'BuildAndRun') then
    begin
      CmdLine.ConsumeParameter;
      SerializeBuildAndRunTask;
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
    else if SameText(ParamValue, 'DProjPrintConfigs') then
    begin
      CmdLine.ConsumeParameter;
      SerializeDProjPrintConfigsTask;
    end
    else if SameText(ParamValue, 'DProjPrintCurConfig') then
    begin
      CmdLine.ConsumeParameter;
      SerializeDProjPrintCurConfigTask;
    end
    else if SameText(ParamValue, 'DProjPrintSearchPaths') then
    begin
      CmdLine.ConsumeParameter;
      SerializeDProjPrintSearchPathsTask;
    end
    else if SameText(ParamValue, 'ExportBuildEnvironment') then
    begin
      CmdLine.ConsumeParameter;
      SerializeExportBuildEnvironmentTask;
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
