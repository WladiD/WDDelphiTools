// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Application;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Contnrs,
  DPT.Types,
  DPT.Workflow;

type
  TDptTaskDispatcher = class
  private
    FTasks: TDictionary<string, TDptTaskClass>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterTask(const ActionName: string; TaskClass: TDptTaskClass);
    function ExecuteDispatch(CmdLine: TCmdLineConsumer; WorkflowEngine: TDptWorkflowEngine): Integer;
  end;

  TDptApplication = class
  public
    class procedure Run;
  end;

implementation

uses
  Winapi.Windows,
  System.IOUtils,
  System.NetEncoding,
  System.Hash,
  DPT.Detection,
  DPT.InstructionScreen,
  DPT.Build.Task,
  DPT.BuildEnvironment.Task,
  DPT.DProj.Task,
  DPT.IdeControl.Task,
  DPT.Lint.Task,
  DPT.Lint.Setup.Task,
  DPT.Lint.Fixtures,
  DPT.OpenUnitTask,
  DPT.PrintPath.Task,
  DPT.RemovePackage.Task,
  DPT.RegisterPackage.Task,
  DPT.DProjAnalyzer,
  Slim.Server,
  Slim.Fixture,
  Slim.CmdUtils,
  DPT.Fixtures;

type
  TSlimFixtureResolverHelper = class(TSlimFixtureResolver);

{ TDptTaskDispatcher }

constructor TDptTaskDispatcher.Create;
begin
  FTasks := TDictionary<string, TDptTaskClass>.Create(
    TEqualityComparer<string>.Construct(
      function(const L, R: string): Boolean
      begin
        Result := SameText(L, R);
      end,
      function(const V: string): Integer
      begin
        Result := THashBobJenkins.GetHashValue(LowerCase(V));
      end
    )
  );
  
  RegisterTask('RemovePackagesBySourceDir', TDptRemovePackagesBySourceDirTask);
  RegisterTask('Build', TDptBuildTask);
  RegisterTask('BuildAndRun', TDptBuildAndRunTask);
  RegisterTask('RemovePackage', TDptRemovePackageTask);
  RegisterTask('RegisterPackage', TDptRegisterPackageTask);
  RegisterTask('IsPackageRegistered', TDptIsPackageRegisteredTask);
  RegisterTask('Lint', TDptLintTask);
  RegisterTask('LintSetup', TDptLintSetupTask);
  RegisterTask('PrintPath', TDptPrintPathTask);
  RegisterTask('OpenUnit', TDptOpenUnitTask);
  RegisterTask('DProjPrintConfigs', TDptDProjPrintConfigsTask);
  RegisterTask('DProjPrintCurConfig', TDptDProjPrintCurConfigTask);
  RegisterTask('DProjPrintSearchPaths', TDptDProjPrintSearchPathsTask);
  RegisterTask('ExportBuildEnvironment', TDptExportBuildEnvironmentTask);
  RegisterTask('ImportBuildEnvironment', TDptImportBuildEnvironmentTask);
  RegisterTask('Start', TDptStartTask);
  RegisterTask('Stop', TDptStopTask);
end;

destructor TDptTaskDispatcher.Destroy;
begin
  FTasks.Free;
  inherited;
end;

procedure TDptTaskDispatcher.RegisterTask(const ActionName: string; TaskClass: TDptTaskClass);
begin
  FTasks.Add(ActionName, TaskClass);
end;

function TDptTaskDispatcher.ExecuteDispatch(CmdLine: TCmdLineConsumer; WorkflowEngine: TDptWorkflowEngine): Integer;
var
  DelphiVersion: TDelphiVersion;
  ParamValue: string;
  TaskClass: TDptTaskClass;
  Task: TDptTaskBase;
  Action: string;
begin
  Result := 0;
  
  // 1. Delphi Version
  ParamValue := CmdLine.CheckParameter('DelphiVersion');
  if IsLatestVersionAlias(ParamValue) then
  begin
    DelphiVersion := FindMostRecentDelphiVersion;
    if DelphiVersion = dvUnknown then
      raise Exception.Create('No supported Delphi version found on this machine');
    CmdLine.ConsumeParameter;
  end
  else if IsValidDelphiVersion(ParamValue, DelphiVersion) then
  begin
    CmdLine.ConsumeParameter;
  end
  else
    DelphiVersion := dvUnknown;

  // 2. Action
  Action := CmdLine.CheckParameter('Action');
  
  // Special Handling for HandleProtocol (Legacy/URL)
  if SameText(Action, 'HandleProtocol') then
  begin
    CmdLine.ConsumeParameter;
    var URL := CmdLine.CheckParameter('URL');
    CmdLine.ConsumeParameter;

    if Pos('dpt://', LowerCase(URL)) <> 1 then
      raise Exception.Create('Invalid protocol');

    Delete(URL, 1, 6);
    var QPos := Pos('?', URL);
    var Command, ParamsStr: string;
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

    if (Length(Command) > 0) and (Command[Length(Command)] = '/') then
      Delete(Command, Length(Command), 1);

    if SameText(Command, 'openunit') then
    begin
      Task := TDptOpenUnitTask.Create;
      try
        Task.DelphiVersion := DelphiVersion;
        Task.WorkflowEngine := WorkflowEngine;
        
        var Params := TStringList.Create;
        try
          Params.Delimiter := '&';
          Params.StrictDelimiter := True;
          Params.DelimitedText := ParamsStr;

          var OpenUnitTask := TDptOpenUnitTask(Task);
          OpenUnitTask.FullPathToUnit := TNetEncoding.URL.Decode(Params.Values['file']);
          OpenUnitTask.GoToLine := StrToIntDef(Params.Values['line'], 0);
          OpenUnitTask.MemberImplementation := TNetEncoding.URL.Decode(Params.Values['member']);

          Writeln('Opening unit "' + OpenUnitTask.FullPathToUnit + '"...');
          Task.Execute;
        finally
          Params.Free;
        end;
      finally
        Task.Free;
      end;
      Exit;
    end
    else
      raise Exception.Create('Unknown protocol command: ' + Command);
  end;

  if FTasks.TryGetValue(Action, TaskClass) then
  begin
    CmdLine.ConsumeParameter;
    Task := TaskClass.Create;
    try
      Task.DelphiVersion := DelphiVersion;
      Task.WorkflowEngine := WorkflowEngine;
      Task.Parse(CmdLine);
      Task.Execute;
    finally
      Task.Free;
    end;
  end
  else if SameText(Action, 'AiSession') then
  begin
    // Handled in Run
    Exit;
  end
  else
    CmdLine.InvalidParameter('Not accepted action: ' + Action);
end;

{ TDptApplication }

class procedure TDptApplication.Run;
begin
  SetConsoleOutputCP(CP_UTF8);
  SetConsoleCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  SetTextCodePage(Input, CP_UTF8);

  try
    var LHostPID: DWORD;
    case DetectAIMode(LHostPID) of
      amCursor: Writeln(Format('AI-Mode from Cursor detected (Host-PID: %d)', [LHostPID]));
      amGemini: Writeln(Format('AI-Mode from Gemini CLI detected (Host-PID: %d)', [LHostPID]));
    end;

    var LPort: Integer;
    var LIsSlimStart: Boolean;
    LIsSlimStart := Slim.CmdUtils.HasSlimPortParam(LPort);

    {$IFDEF FITNESSE}
    if (not LIsSlimStart) and (ParamCount <= 1) then
    begin
      LIsSlimStart := True;
      LPort := 9000;
    end;
    {$ENDIF}

    if LIsSlimStart then
    begin
      // Force initialization of fixture units by referencing symbols
      TDptControl.ClassName;
      TDptLintFixture.ClassName;
      TDptLintUnitContextFixture.ClassName;
      TDptLintUsesFixture.ClassName;

      var SlimServer: TSlimServer := TSlimServer.Create(nil);
      try
        SlimServer.DefaultPort := LPort;
        SlimServer.Active := True;

        Writeln('Slim Server started on port ', LPort);
        Writeln('Registered Fixtures:');
        for var Loop: Integer := 0 to TSlimFixtureResolverHelper.FFixtures.Count - 1 do
          Writeln('  ', TClass(TSlimFixtureResolverHelper.FFixtures[Loop]).ClassName);

        Writeln('Server running... (Ctrl+C or call StopServer to stop)');
        DPT.Fixtures.TDptControl.StopServerEvent.WaitFor(INFINITE);
      finally
        SlimServer.Free;
      end;
      Exit;
    end;

    // Check for Help command
    if (ParamCount >= 1) and (SameText(ParamStr(1), 'Help') or SameText(ParamStr(1), '-h') or SameText(ParamStr(1), '/?')) then
    begin
      TDptInstructionScreen.ShowHelp(ParamStr(2));
      System.ExitCode := 0;
      Exit;
    end;

    // Command processing
    var WorkflowEngine: TDptWorkflowEngine := nil;
    try
      if ParamCount >= 1 then
      begin
        var LCmdLine := TCmdLineConsumer.Create;
        try
          var LAction, LAiSessionAction: string;
          
          // Try to identify action (skip version if present)
          var LArg := LCmdLine.CheckParameter('Version/Action');
          var LDummyVersion: TDelphiVersion;
          if IsLatestVersionAlias(LArg) or IsValidDelphiVersion(LArg, LDummyVersion) then
          begin
            LCmdLine.ConsumeParameter;
            if LCmdLine.HasParameter then
              LArg := LCmdLine.CheckParameter('Action')
            else
              LArg := '';
          end;
          LAction := LArg;
          
          LAiSessionAction := '';
          if SameText(LAction, 'AiSession') and LCmdLine.HasParameter then
          begin
            LCmdLine.ConsumeParameter;
            if LCmdLine.HasParameter then
              LAiSessionAction := LCmdLine.CheckParameter('AiAction');
          end;

          WorkflowEngine := TDptWorkflowEngine.Create(LAction, LAiSessionAction);
          
          // Reset LCmdLine for Dispatcher
          LCmdLine.Free;
          LCmdLine := TCmdLineConsumer.Create;

          // Configure context for engine
          if (SameText(LAction, 'Build') or SameText(LAction, 'BuildAndRun')) and (ParamCount >= 2) then
          begin
            var LProjFileArgIdx := 2;
            if IsLatestVersionAlias(ParamStr(1)) or IsValidDelphiVersion(ParamStr(1), LDummyVersion) then LProjFileArgIdx := 3;
            
            if ParamCount >= LProjFileArgIdx then
            begin
              var LProjFile := ExpandFileName(ParamStr(LProjFileArgIdx));
              if TFile.Exists(LProjFile) and SameText(ExtractFileExt(LProjFile), '.dproj') then
              begin
                var LAnalyzer := TDProjAnalyzer.Create(LProjFile);
                try
                  var LFiles := LAnalyzer.GetProjectFiles;
                  WorkflowEngine.SetCurrentProjectFile(LProjFile);
                  WorkflowEngine.SetProjectFiles(LFiles);
                finally
                  LAnalyzer.Free;
                end;
              end;
            end;
          end
          else if SameText(LAction, 'Lint') then
          begin
            var LLintTargetIdx := 3;
            if not IsLatestVersionAlias(ParamStr(1)) then LLintTargetIdx := 2;
            if ParamCount >= LLintTargetIdx + 1 then
              WorkflowEngine.SetLintTarget(ParamStr(LLintTargetIdx + 1));
          end;

          var Instructions: string;
          if Assigned(WorkflowEngine) and (WorkflowEngine.CheckConditions(Instructions, gtBefore) = waExit) then
          begin
            Writeln('-------------------------------------------------------------------------------');
            Writeln('DPT WORKFLOW VIOLATION:');
            Writeln(Instructions);
            Writeln('-------------------------------------------------------------------------------');
            
            var LFinalExitCode := WorkflowEngine.ExitCode;
            if LFinalExitCode = 0 then LFinalExitCode := 1;

            var AfterInstructions: string;
            WorkflowEngine.CheckConditions(AfterInstructions, gtAfter);
            if AfterInstructions <> '' then
            begin
              Writeln(AfterInstructions);
              Writeln('-------------------------------------------------------------------------------');
            end;

            System.ExitCode := LFinalExitCode;
            Exit;
          end;

          if SameText(LAction, 'AiSession') then
          begin
            if SameText(LAiSessionAction, 'Start') then WorkflowEngine.StartSession
            else if SameText(LAiSessionAction, 'Stop') then WorkflowEngine.StopSession
            else if SameText(LAiSessionAction, 'Reset') then WorkflowEngine.ResetSession
            else if SameText(LAiSessionAction, 'Status') then WorkflowEngine.ShowStatus
            else if SameText(LAiSessionAction, 'RegisterFiles') then
            begin
              var LFiles: TArray<string>;
              SetLength(LFiles, ParamCount - 3);
              for var I := 4 to ParamCount do
                LFiles[I-4] := ParamStr(I);
              WorkflowEngine.AddFilesToSession(LFiles);
            end
            else
              Writeln('Unknown AiSession action: ', LAiSessionAction);
            Exit;
          end;

          // Dispatch CLI command
          var Dispatcher := TDptTaskDispatcher.Create;
          try
            Dispatcher.ExecuteDispatch(LCmdLine, WorkflowEngine);
            
            if Assigned(WorkflowEngine) then
            begin
              WorkflowEngine.ExitCode := System.ExitCode;
              var AfterInstructions: string;
              WorkflowEngine.CheckConditions(AfterInstructions, gtAfter);
              if AfterInstructions <> '' then
              begin
                 Writeln('-------------------------------------------------------------------------------');
                 Writeln(AfterInstructions);
                 Writeln('-------------------------------------------------------------------------------');
              end;
              System.ExitCode := WorkflowEngine.ExitCode;
            end;
          finally
            Dispatcher.Free;
          end;
          Exit;
        finally
          LCmdLine.Free;
        end;
      end;

      {$IFNDEF FITNESSE}
      TDptInstructionScreen.ShowCompact;
      {$ENDIF}
    finally
      WorkflowEngine.Free;
    end;
  except
    on E:Exception do
    begin
      if not (E is EInvalidParameter) then
        Writeln(E.Classname, ': ', E.Message);
      TDptInstructionScreen.ShowCompact;
      System.ExitCode := 1;
    end;
  end;
end;

end.
