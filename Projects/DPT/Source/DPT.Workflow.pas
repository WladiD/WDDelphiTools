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

  TDptGuardType = (gtBefore, gtAfter);

  TDptWorkflowBlock = class
  public
    GuardType: TDptGuardType;
    Condition: string;
    Instructions: string;
    NestedBlocks: TObjectList<TDptWorkflowBlock>;
    constructor Create;
    destructor Destroy; override;
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
    FExitRequested: Boolean;
    FExitCode: Integer;

    procedure LoadWorkflow;
    function FindWorkflowFile: string;
    
    procedure EvalBlocks(AParser: TExprParser; ABlocks: TObjectList<TDptWorkflowBlock>; AGuardType: TDptGuardType; var AInstructions: string);
    function ProcessInstructions(AParser: TExprParser; const AStr: string): string;

    // Callback methods
    function OnGetVariableCallback(Sender: TObject; const VarName: string; var Value: Variant): Boolean;
    function OnExecuteFunctionCallback(Sender: TObject; const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;

    // Script functions logic
    function ExprParserAiSessionStarted: Variant;
    function ExprParserIsCurrentAction(const Args: Variant): Variant;
    function ExprParserIsCurrentAiSessionAction(const Args: Variant): Variant;
    function ExprParserGetCurrentLintTargetFile: Variant;
    function ExprParserIsFileRegisteredInAiSession(const Args: Variant): Variant;
    function ExprParserGetCurrentProjectFiles: Variant;
    function ExprParserHasValidLintResult(const Args: Variant): Variant;
    function ExprParserRequestDptExit: Variant;
    function ExprParserRequestDptExitWithCode(const Args: Variant): Variant;
    function ExprParserGetExitCode: Variant;

  public
    constructor Create(const AAction, AAiSessionAction: string);
    destructor Destroy; override;
    
    procedure SetLintTarget(const AFile: string);
    procedure SetProjectFiles(const AFiles: TArray<string>);
    procedure SetExitCode(ACode: Integer);

    function CheckConditions(out AInstructions: string; AGuardType: TDptGuardType = gtBefore): TDptWorkflowAction;
    
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
    property ExitCode: Integer read FExitCode write SetExitCode;
  end;

implementation

const
  WorkflowFileName = '.DptAiWorkflow';

{ TDptWorkflowBlock }

constructor TDptWorkflowBlock.Create;
begin
  NestedBlocks := TObjectList<TDptWorkflowBlock>.Create(True);
end;

destructor TDptWorkflowBlock.Destroy;
begin
  NestedBlocks.Free;
  inherited;
end;

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
  CurrentLine: Integer;

  procedure ParseBlocks(ABlocks: TObjectList<TDptWorkflowBlock>; AParentBlock: TDptWorkflowBlock);
  var
    Line: string;
    CurrentBlock: TDptWorkflowBlock;
    Remaining: string;
  begin
    CurrentBlock := nil;
    while CurrentLine < Lines.Count do
    begin
      Line := Trim(Lines[CurrentLine]);
      Inc(CurrentLine);
      
      if (Line = '') or Line.StartsWith('#') then Continue;

      if Line.StartsWith('BeforeDptGuard:') then
      begin
        CurrentBlock := TDptWorkflowBlock.Create;
        CurrentBlock.GuardType := gtBefore;
        CurrentBlock.Condition := Trim(Copy(Line, 16, MaxInt));
        
        if CurrentBlock.Condition.EndsWith('{') then
        begin
          CurrentBlock.Condition := Trim(Copy(CurrentBlock.Condition, 1, Length(CurrentBlock.Condition) - 1));
          ABlocks.Add(CurrentBlock);
          ParseBlocks(CurrentBlock.NestedBlocks, CurrentBlock);
          CurrentBlock := nil;
        end
        else
          ABlocks.Add(CurrentBlock);
      end
      else if Line.StartsWith('AfterDptGuard:') then
      begin
        CurrentBlock := TDptWorkflowBlock.Create;
        CurrentBlock.GuardType := gtAfter;
        CurrentBlock.Condition := Trim(Copy(Line, 15, MaxInt));
        
        if CurrentBlock.Condition.EndsWith('{') then
        begin
          CurrentBlock.Condition := Trim(Copy(CurrentBlock.Condition, 1, Length(CurrentBlock.Condition) - 1));
          ABlocks.Add(CurrentBlock);
          ParseBlocks(CurrentBlock.NestedBlocks, CurrentBlock);
          CurrentBlock := nil;
        end
        else
          ABlocks.Add(CurrentBlock);
      end
      else if Line.StartsWith('{') then
      begin
        if Assigned(CurrentBlock) then
        begin
          Remaining := Trim(Copy(Line, 2, MaxInt));
          if Remaining.EndsWith('}') then
          begin
            CurrentBlock.Instructions := Trim(Copy(Remaining, 1, Length(Remaining) - 1));
            CurrentBlock := nil;
          end
          else
          begin
            if Remaining <> '' then
              CurrentBlock.Instructions := Remaining + sLineBreak;
            ParseBlocks(CurrentBlock.NestedBlocks, CurrentBlock);
            CurrentBlock := nil;
          end;
        end;
      end
      else if Line.StartsWith('}') then
      begin
        Exit;
      end
      else
      begin
        if Assigned(CurrentBlock) then
          CurrentBlock.Condition := CurrentBlock.Condition + ' ' + Line
        else if Assigned(AParentBlock) then
          AParentBlock.Instructions := AParentBlock.Instructions + Lines[CurrentLine-1] + sLineBreak;
      end;
    end;
  end;

begin
  if not TFile.Exists(FWorkflowFile) then Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FWorkflowFile);
    CurrentLine := 0;
    ParseBlocks(FBlocks, nil);
  finally
    Lines.Free;
  end;
end;

function TDptWorkflowEngine.OnGetVariableCallback(Sender: TObject; const VarName: string; var Value: Variant): Boolean;
begin
  Result := True;
  if SameText(VarName, 'AiSessionStarted') then
  begin
    Value := Assigned(FSession) and TFile.Exists(FSessionFile);
  end
  else
    Result := False;
end;

function TDptWorkflowEngine.OnExecuteFunctionCallback(Sender: TObject; const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;
begin
  if SameText(FuncName, 'AiSessionStarted') then
    ResVal := ExprParserAiSessionStarted
  else if SameText(FuncName, 'IsCurrentAction') then
    ResVal := ExprParserIsCurrentAction(Args)
  else if SameText(FuncName, 'IsCurrentAiSessionAction') then
    ResVal := ExprParserIsCurrentAiSessionAction(Args)
  else if SameText(FuncName, 'GetCurrentLintTargetFile') then
    ResVal := ExprParserGetCurrentLintTargetFile
  else if SameText(FuncName, 'IsFileRegisteredInAiSession') then
    ResVal := ExprParserIsFileRegisteredInAiSession(Args)
  else if SameText(FuncName, 'GetCurrentProjectFiles') then
    ResVal := ExprParserGetCurrentProjectFiles
  else if SameText(FuncName, 'HasValidLintResult') then
    ResVal := ExprParserHasValidLintResult(Args)
  else if SameText(FuncName, 'RequestDptExit') then
    ResVal := ExprParserRequestDptExit
  else if SameText(FuncName, 'RequestDptExitWithCode') then
    ResVal := ExprParserRequestDptExitWithCode(Args)
  else if SameText(FuncName, 'GetExitCode') then
    ResVal := ExprParserGetExitCode
  else
    Exit(False);

  Result := True;
end;

function TDptWorkflowEngine.ExprParserAiSessionStarted: Variant;
begin
  Result := Assigned(FSession) and TFile.Exists(FSessionFile);
end;

function TDptWorkflowEngine.ExprParserRequestDptExit: Variant;
begin
  FExitRequested := True;
  Result := True;
end;

function TDptWorkflowEngine.ExprParserRequestDptExitWithCode(const Args: Variant): Variant;
begin
  FExitRequested := True;
  if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
    FExitCode := Args[0];
  Result := True;
end;

function TDptWorkflowEngine.ExprParserGetExitCode: Variant;
begin
  Result := FExitCode;
end;

function TDptWorkflowEngine.ExprParserIsCurrentAction(const Args: Variant): Variant;
begin
  Result := False;
  if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
    Result := SameText(VarToStr(Args[0]), FCurrentAction);
end;

function TDptWorkflowEngine.ExprParserIsCurrentAiSessionAction(const Args: Variant): Variant;
begin
  Result := False;
  if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
    Result := SameText(VarToStr(Args[0]), FCurrentAiSessionAction);
end;

function TDptWorkflowEngine.ExprParserGetCurrentLintTargetFile: Variant;
begin
  Result := FCurrentLintTargetFile;
end;

function TDptWorkflowEngine.ExprParserIsFileRegisteredInAiSession(const Args: Variant): Variant;
var
  FileName: string;
  Entry: TDptSessionFileEntry;
begin
  Result := False;
  if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
  begin
    FileName := ExpandFileName(VarToStr(Args[0]));
    for Entry in FSession.Files do
    begin
      if SameText(Entry.Path, FileName) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TDptWorkflowEngine.ExprParserGetCurrentProjectFiles: Variant;
var
  I: Integer;
begin
  if Length(FCurrentProjectFiles) = 0 then
    Result := VarArrayCreate([0, -1], varVariant)
  else
  begin
    Result := VarArrayCreate([0, Length(FCurrentProjectFiles) - 1], varVariant);
    for I := 0 to Length(FCurrentProjectFiles) - 1 do
      Result[I] := FCurrentProjectFiles[I];
  end;
end;

function TDptWorkflowEngine.ExprParserHasValidLintResult(const Args: Variant): Variant;
var
  FilesVar: Variant;
  I: Integer;
  FileName: string;
  Found: Boolean;
  Entry: TDptSessionFileEntry;
  AllValid: Boolean;
begin
  Result := True; // Default to valid if no session or no files
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
        if TFile.GetLastWriteTime(FileName) < FSession.StartTime then
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
    Result := AllValid;
  end;
end;

function TDptWorkflowEngine.ProcessInstructions(AParser: TExprParser; const AStr: string): string;
var
  I, Start: Integer;
  Expr: string;
begin
  Result := '';
  I := 1;
  while I <= Length(AStr) do
  begin
    if AStr[I] = '`' then
    begin
      // Escaping: Double backtick becomes a single literal backtick
      if (I < Length(AStr)) and (AStr[I + 1] = '`') then
      begin
        Result := Result + '`';
        Inc(I, 2);
        Continue;
      end;

      // Find expression
      Start := I + 1;
      I := Start;
      while (I <= Length(AStr)) and (AStr[I] <> '`') do
        Inc(I);

      if I <= Length(AStr) then
      begin
        Expr := Copy(AStr, Start, I - Start);
        if AParser.Eval(Expr) then
          Result := Result + VarToStr(AParser.Value)
        else
          Result := Result + '`' + Expr + '`'; // Fallback: Keep as text if not a valid expression
        Inc(I);
      end
      else
      begin
        // Unclosed backtick: treat as literal text
        Result := Result + '`';
        I := Start;
      end;
    end
    else
    begin
      Result := Result + AStr[I];
      Inc(I);
    end;
  end;
end;

procedure TDptWorkflowEngine.EvalBlocks(AParser: TExprParser; ABlocks: TObjectList<TDptWorkflowBlock>; AGuardType: TDptGuardType; var AInstructions: string);
var
  Block: TDptWorkflowBlock;
  TrimmedInstructions: string;
begin
  for Block in ABlocks do
  begin
    if FExitRequested then Exit;

    if (Block.GuardType = AGuardType) and AParser.Eval(Block.Condition) then
    begin
      if AParser.Value <> 0 then
      begin
        TrimmedInstructions := Trim(Block.Instructions);
        if TrimmedInstructions <> '' then
          AInstructions := AInstructions + ProcessInstructions(AParser, TrimmedInstructions) + sLineBreak + sLineBreak;
        
        if Block.NestedBlocks.Count > 0 then
          EvalBlocks(AParser, Block.NestedBlocks, AGuardType, AInstructions);
      end
    end;
    
    if FExitRequested then Exit;
  end;
end;

function TDptWorkflowEngine.CheckConditions(out AInstructions: string; AGuardType: TDptGuardType = gtBefore): TDptWorkflowAction;
var
  Parser: TExprParser;
begin
  Result := waNone;
  AInstructions := '';
  FExitRequested := False;
  if FBlocks.Count = 0 then Exit;

  Parser := TExprParser.Create;
  try
    Parser.OnGetVariable := OnGetVariableCallback;
    Parser.OnExecuteFunction := OnExecuteFunctionCallback;
    
    EvalBlocks(Parser, FBlocks, AGuardType, AInstructions);
    
    if FExitRequested then
      Result := waExit;
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

procedure TDptWorkflowEngine.SetExitCode(ACode: Integer);
begin
  FExitCode := ACode;
end;

end.
