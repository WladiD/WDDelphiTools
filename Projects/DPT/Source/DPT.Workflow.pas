// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Workflow;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  Winapi.Windows,
  System.Variants,
  System.StrUtils,
  System.Math,
  
  ExprParser,
  ExprParserTools,
  DPT.Detection,
  DPT.Workflow.Session;

type
  TDptWorkflowAction = (waNone, waExit);

  TDptWorkflowBlock = class
  public
    Condition: string;
    Instructions: string;
    Action: TDptWorkflowAction;
  end;

  TDptWorkflowEngine = class
  private
    FWorkflowFile: string;
    FSessionFile: string;
    FBlocks: TObjectList<TDptWorkflowBlock>;
    FSession: TDptSessionData;
    FHostPID: DWORD;
    FAIMode: TAIMode;
    
    // Context information for conditions
    FCurrentAction: string;
    FCurrentAiSessionAction: string;
    FCurrentLintTargetFile: string;
    FCurrentProjectFiles: TArray<string>;

    procedure LoadWorkflow;
    function FindWorkflowFile: string;
    
    // Callback methods
    function OnGetVariableCallback(Sender: TObject; const VarName: string; var Value: Variant): Boolean;
    function OnExecuteFunctionCallback(Sender: TObject; const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;

  public
    constructor Create(const AAction, AAiSessionAction: string);
    destructor Destroy; override;
    
    procedure SetLintTarget(const AFile: string);
    procedure SetProjectFiles(const AFiles: TArray<string>);

    function CheckConditions(out AInstructions: string): TDptWorkflowAction;
    
    // Session management
    procedure StartSession;
    procedure StopSession;
    procedure ResetSession;
    procedure AddFilesToSession(const AFiles: TArray<string>);
    procedure ShowStatus;
    procedure ReportLintResult(const AFileName: string; ASuccess: Boolean);
    
    property Session: TDptSessionData read FSession;
    property WorkflowFile: string read FWorkflowFile;
    property HostPID: DWORD read FHostPID;
  end;

implementation

const
  WorkflowFileName = '.DptAiWorkflow';

{ TDptWorkflowEngine }

constructor TDptWorkflowEngine.Create(const AAction, AAiSessionAction: string);
begin
  FBlocks := TObjectList<TDptWorkflowBlock>.Create(True);
  FCurrentAction := AAction;
  FCurrentAiSessionAction := AAiSessionAction;
  
  FAIMode := DetectAIMode(FHostPID);
  if FAIMode <> amNone then
  begin
    FWorkflowFile := FindWorkflowFile;
    if FWorkflowFile <> '' then
    begin
      FSessionFile := FWorkflowFile + '.Session' + IntToStr(FHostPID) + '.json';
      FSession := TDptSessionData.Create;
      FSession.HostPID := FHostPID;
      if TFile.Exists(FSessionFile) then
        FSession.LoadFromFile(FSessionFile);
      LoadWorkflow;
    end;
  end;
end;

destructor TDptWorkflowEngine.Destroy;
begin
  FSession.Free;
  FBlocks.Free;
  inherited;
end;

function TDptWorkflowEngine.FindWorkflowFile: string;
var
  CurrentDir: string;
  ParentDir: string;
begin
  Result := '';
  CurrentDir := GetCurrentDir;
  while True do
  begin
    if TFile.Exists(TPath.Combine(CurrentDir, WorkflowFileName)) then
    begin
      Result := TPath.Combine(CurrentDir, WorkflowFileName);
      Break;
    end;
    ParentDir := TPath.GetDirectoryName(CurrentDir);
    if (ParentDir = '') or (ParentDir = CurrentDir) then Break;
    CurrentDir := ParentDir;
  end;
end;

procedure TDptWorkflowEngine.LoadWorkflow;
var
  Lines: TStringList;
  I: Integer;
  Block: TDptWorkflowBlock;
  InBlock: Boolean;
  Line: string;
  ActionStr: string;
begin
  if not TFile.Exists(FWorkflowFile) then Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FWorkflowFile);
    Block := nil;
    InBlock := False;
    
    for I := 0 to Lines.Count - 1 do
    begin
      Line := System.SysUtils.Trim(Lines[I]);
      if (Line = '') or Line.StartsWith('#') then Continue;
      
      if Pos('DptCondition:', Line) = 1 then
      begin
        Block := TDptWorkflowBlock.Create;
        Block.Condition := System.SysUtils.Trim(Copy(Line, 14, MaxInt));
        FBlocks.Add(Block);
      end
      else if Pos('{', Line) = 1 then
      begin
        InBlock := True;
      end
      else if Pos('}', Line) = 1 then
      begin
        InBlock := False;
        Block := nil;
      end
      else if Assigned(Block) then
      begin
        if InBlock then
        begin
          if Pos('DptAction:', Line) = 1 then
          begin
            ActionStr := System.SysUtils.Trim(Copy(Line, 11, MaxInt));
            if Pos('ExitDptProcess', ActionStr) = 1 then
              Block.Action := waExit;
          end
          else
            Block.Instructions := Block.Instructions + Lines[I] + sLineBreak;
        end
        else
        begin
          // Still reading condition
          Block.Condition := Block.Condition + ' ' + Line;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TDptWorkflowEngine.OnGetVariableCallback(Sender: TObject; const VarName: string; var Value: Variant): Boolean;
begin
  Result := True;
  if SameText(VarName, 'AiSessionStarted') then
  begin
    if Assigned(FSession) and TFile.Exists(FSessionFile) then
      Value := 1
    else
      Value := 0;
  end
  else
    Result := False;
end;

function TDptWorkflowEngine.OnExecuteFunctionCallback(Sender: TObject; const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;
var
  I: Integer;
  FilesVar: Variant;
  FileName: string;
  Found: Boolean;
  Entry: TDptSessionFileEntry;
  AllValid: Boolean;
begin
  Result := True;
  ResVal := 0;
  
  if SameText(FuncName, 'AiSessionStarted') then
  begin
    if Assigned(FSession) and TFile.Exists(FSessionFile) then
      ResVal := 1
    else
      ResVal := 0;
  end
  else if SameText(FuncName, 'IsCurrentAction') then
  begin
    if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
      ResVal := IfThen(SameText(VarToStr(Args[0]), FCurrentAction), 1, 0);
  end
  else if SameText(FuncName, 'IsCurrentAiSessionAction') then
  begin
    if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
      ResVal := IfThen(SameText(VarToStr(Args[0]), FCurrentAiSessionAction), 1, 0);
  end
  else if SameText(FuncName, 'GetCurrentLintTargetFile') then
  begin
    ResVal := FCurrentLintTargetFile;
  end
  else if SameText(FuncName, 'IsFileRegisteredInAiSession') then
  begin
    if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
    begin
      FileName := ExpandFileName(VarToStr(Args[0]));
      Found := False;
      for Entry in FSession.Files do
      begin
        if SameText(Entry.Path, FileName) then
        begin
          Found := True;
          Break;
        end;
      end;
      ResVal := IfThen(Found, 1, 0);
    end;
  end
  else if SameText(FuncName, 'GetCurrentProjectFiles') then
  begin
    if Length(FCurrentProjectFiles) = 0 then
      ResVal := VarArrayCreate([0, -1], varVariant)
    else
    begin
      ResVal := VarArrayCreate([0, Length(FCurrentProjectFiles) - 1], varVariant);
      for I := 0 to Length(FCurrentProjectFiles) - 1 do
        ResVal[I] := FCurrentProjectFiles[I];
    end;
  end
  else if SameText(FuncName, 'HasValidLintResult') then
  begin
    ResVal := 1; // Default to valid if no session or no files
    if Assigned(FSession) and VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) then
    begin
      FilesVar := Args[0];
      AllValid := True;
      if VarIsArray(FilesVar) then
      begin
        for I := 0 to VarArrayHighBound(FilesVar, 1) do
        begin
          FileName := VarToStr(FilesVar[I]);
          if not TFile.Exists(FileName) then Continue;
          FileName := ExpandFileName(FileName);
          
          // Only require lint results for files modified or created since session start
          if TFile.GetLastWriteTime(FileName) <= FSession.StartTime then
            Continue;

          Found := False;
          for Entry in FSession.Files do
          begin
            if SameText(Entry.Path, FileName) then
            begin
              // Check if lint was successful and file hasn't changed since lint
              if Entry.LintSuccess and (Entry.LastLintTime >= TFile.GetLastWriteTime(FileName)) then
                Found := True;
              Break;
            end;
          end;

          if not Found then
          begin
            AllValid := False;
            Break;
          end;
        end;
      end;
      ResVal := IfThen(AllValid, 1, 0);
    end;
  end
  else
    Result := False;
end;

function TDptWorkflowEngine.CheckConditions(out AInstructions: string): TDptWorkflowAction;
var
  Parser: TExprParser;
  Block: TDptWorkflowBlock;
  TrimmedInstructions: string;
begin
  Result := waNone;
  AInstructions := '';
  if FBlocks.Count = 0 then Exit;

  Parser := TExprParser.Create;
  try
    Parser.OnGetVariable := OnGetVariableCallback;
    Parser.OnExecuteFunction := OnExecuteFunctionCallback;
    
    for Block in FBlocks do
    begin
      if Parser.Eval(Block.Condition) then
      begin
        if Parser.Value <> 0 then
        begin
          TrimmedInstructions := System.SysUtils.Trim(Block.Instructions);
          AInstructions := AInstructions + TrimmedInstructions + sLineBreak + sLineBreak;
          if Block.Action > Result then
            Result := Block.Action;
        end;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TDptWorkflowEngine.StartSession;
begin
  if FSessionFile = '' then
    raise Exception.Create('No workflow file found. Cannot start session.');
    
  if not Assigned(FSession) then
    FSession := TDptSessionData.Create;
    
  FSession.HostPID := FHostPID;
  FSession.StartTime := Now;
  FSession.SaveToFile(FSessionFile);
  Writeln('AI session started for PID ', FHostPID);
  Writeln('Session file: ', FSessionFile);
end;

procedure TDptWorkflowEngine.StopSession;
begin
  if TFile.Exists(FSessionFile) then
  begin
    TFile.Delete(FSessionFile);
    Writeln('AI session stopped for PID ', FHostPID);
  end
  else
    Writeln('No active AI session found for PID ', FHostPID);
end;

procedure TDptWorkflowEngine.ResetSession;
begin
  if Assigned(FSession) then
  begin
    FSession.Files.Clear;
    FSession.StartTime := Now;
    FSession.SaveToFile(FSessionFile);
    Writeln('AI session reset.');
  end;
end;

procedure TDptWorkflowEngine.AddFilesToSession(const AFiles: TArray<string>);
var
  FileName: string;
  Entry: TDptSessionFileEntry;
begin
  for FileName in AFiles do
  begin
    Entry.Path := ExpandFileName(FileName);
    Entry.Hash := ''; 
    Entry.LastLintTime := 0;
    Entry.LintSuccess := False;
    FSession.Files.Add(Entry);
    Writeln('Registered file in AI session: ', FileName);
  end;
  FSession.SaveToFile(FSessionFile);
end;

procedure TDptWorkflowEngine.ShowStatus;
var
  I: Integer;
begin
  if not TFile.Exists(FSessionFile) then
  begin
    Writeln('No active AI session.');
    Exit;
  end;
  
  Writeln('AI Session Status:');
  Writeln('  Host PID:   ', FSession.HostPID);
  Writeln('  Started:    ', DateTimeToStr(FSession.StartTime));
  Writeln('  Files:      ', FSession.Files.Count);
  for I := 0 to FSession.Files.Count - 1 do
    Writeln('    - ', FSession.Files[I].Path, ' [Lint: ', IfThen(FSession.Files[I].LintSuccess, 'OK', 'FAILED'), ']');
end;

procedure TDptWorkflowEngine.ReportLintResult(const AFileName: string; ASuccess: Boolean);
var
  I: Integer;
  Entry: TDptSessionFileEntry;
  FullName: string;
begin
  if not Assigned(FSession) or (FSessionFile = '') then Exit;
  
  FullName := ExpandFileName(AFileName);
  for I := 0 to FSession.Files.Count - 1 do
  begin
    Entry := FSession.Files[I];
    if SameText(Entry.Path, FullName) then
    begin
      Entry.LintSuccess := ASuccess;
      Entry.LastLintTime := Now;
      FSession.Files[I] := Entry;
      FSession.SaveToFile(FSessionFile);
      Break;
    end;
  end;
end;

procedure TDptWorkflowEngine.SetLintTarget(const AFile: string);
begin
  FCurrentLintTargetFile := AFile;
end;

procedure TDptWorkflowEngine.SetProjectFiles(const AFiles: TArray<string>);
begin
  FCurrentProjectFiles := AFiles;
end;

end.
