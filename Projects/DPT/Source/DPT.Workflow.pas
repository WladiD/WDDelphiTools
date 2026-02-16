// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Workflow;

interface

uses

  mormot.core.collections,
  Winapi.Windows,

  System.Classes,
  System.IOUtils,
  System.Math,
  System.StrUtils,
  System.SysUtils,
  System.Variants,

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
    NestedBlocks: IList<TDptWorkflowBlock>;
    constructor Create;
  end;

  TDptWorkflowEngine = class
  private
    FWorkflowFile: string;
    FSessionFile: string;
    FBlocks: IList<TDptWorkflowBlock>;
    FSession: TDptSessionData;
    FHostPID: DWORD;
    FAIMode: TAIMode;
    
    // Context information for conditions
    FCurrentAction: string;
    FCurrentAiSessionAction: string;
    FCurrentLintTargetFile: string;
    FCurrentProjectFile: string;
    FCurrentProjectFiles: TArray<string>;
    FExitRequested: Boolean;
    FExitCode: Integer;

    procedure LoadWorkflow;
    function FindWorkflowFile: string;
    
    procedure ParseBlocks(ALines: TStrings; var ACurrentLine: Integer; const ABlocks: IList<TDptWorkflowBlock>; AParentBlock: TDptWorkflowBlock);
    procedure EvalBlocks(AParser: TExprParser; const ABlocks: IList<TDptWorkflowBlock>; AGuardType: TDptGuardType; var AInstructions: string);
    function ProcessInstructions(AParser: TExprParser; const AStr: string): string;

    // Callback methods
    function OnGetVariableCallback(Sender: TObject; const VarName: string; var Value: Variant): Boolean;
    function OnExecuteFunctionCallback(Sender: TObject; const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;

    // Script functions logic
    function ExprParserAiSessionStarted: Variant;
    function ExprParserIsCurrentAction(const Args: Variant): Variant;
    function ExprParserIsCurrentAiSessionAction(const Args: Variant): Variant;
    function ExprParserIsCurrentBuildProjectFile(const Args: Variant): Variant;
    function ExprParserGetCurrentLintTargetFile: Variant;
    function ExprParserIsFileRegisteredInAiSession(const Args: Variant): Variant;
    function ExprParserGetCurrentProjectFiles: Variant;
    function ExprParserHasValidLintResult(const Args: Variant): Variant;
    function ExprParserHasValidRunResult(const Args: Variant): Variant;
    function ExprParserGetInvalidLintFiles(const Args: Variant): Variant;
    function ExprParserRequestDptExit: Variant;
    function ExprParserRequestDptExitWithCode(const Args: Variant): Variant;
    function ExprParserGetExitCode: Variant;

  public
    constructor Create(const AAction, AAiSessionAction: string);
    destructor Destroy; override;
    
    procedure SetLintTarget(const AFile: string);
    procedure SetCurrentProjectFile(const AFile: string);
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
    procedure RegisterRunResult(const ATarget: string; AExitCode: Integer);
    
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
  NestedBlocks := Collections.NewList<TDptWorkflowBlock>;
end;

{ TDptWorkflowEngine }

constructor TDptWorkflowEngine.Create(const AAction, AAiSessionAction: string);
begin
  FBlocks := Collections.NewList<TDptWorkflowBlock>;
  FCurrentAction := AAction;
  FCurrentAiSessionAction := AAiSessionAction;
  
  FAIMode := DetectAIMode(FHostPID);
  if FAIMode <> amNone then
  begin
    FWorkflowFile := FindWorkflowFile;
    if FWorkflowFile <> '' then
    begin
      Writeln('AI Workflow file FOUND: ', FWorkflowFile);
      FSessionFile := FWorkflowFile + '.Session' + IntToStr(FHostPID) + '.json';
      FSession := TDptSessionData.Create;
      FSession.HostPID := FHostPID;
      if TFile.Exists(FSessionFile) then
        FSession.LoadFromFile(FSessionFile);
      LoadWorkflow;
    end
    else
      Writeln('AI Workflow file NOT found. Searched up from: ', GetCurrentDir);
  end;
end;

destructor TDptWorkflowEngine.Destroy;
begin
  FSession.Free;
  inherited;
end;

function TDptWorkflowEngine.FindWorkflowFile: string;
var
  CurrentDir: string;
  ParentDir: string;
  Candidate: string;
begin
  Result := '';
  CurrentDir := GetCurrentDir;
  
  // 1. Search up from current directory
  while CurrentDir <> '' do
  begin
    Candidate := TPath.Combine(CurrentDir, WorkflowFileName);
    if TFile.Exists(Candidate) then
    begin
      Result := Candidate;
      Break;
    end;
    
    ParentDir := TPath.GetDirectoryName(CurrentDir);
    if (ParentDir = '') or (ParentDir = CurrentDir) then 
      Break;
    CurrentDir := ParentDir;
  end;

  // 2. Fallback: Check directory of DPT.exe itself
  if Result = '' then
  begin
    Candidate := TPath.Combine(ExtractFilePath(ParamStr(0)), WorkflowFileName);
    if TFile.Exists(Candidate) then
      Result := Candidate;
  end;
end;

procedure TDptWorkflowEngine.ParseBlocks(ALines: TStrings; var ACurrentLine: Integer; const ABlocks: IList<TDptWorkflowBlock>; AParentBlock: TDptWorkflowBlock);
var
  Line: string;
  TrimmedLine: string;
  CurrentBlock: TDptWorkflowBlock;
  Remaining: string;
begin
  CurrentBlock := nil;
  while ACurrentLine < ALines.Count do
  begin
    Line := ALines[ACurrentLine];
    TrimmedLine := Trim(Line);
    Inc(ACurrentLine);
    
    if (TrimmedLine = '') then
    begin
      if Assigned(AParentBlock) then
        AParentBlock.Instructions := AParentBlock.Instructions + sLineBreak;
      Continue;
    end;

    if TrimmedLine.StartsWith('#') then Continue;

    if TrimmedLine.StartsWith('BeforeDptGuard:') then
    begin
      CurrentBlock := TDptWorkflowBlock.Create;
      CurrentBlock.GuardType := gtBefore;
      CurrentBlock.Condition := Trim(Copy(TrimmedLine, 16, MaxInt));
      
      if CurrentBlock.Condition.EndsWith('{') then
      begin
        CurrentBlock.Condition := Trim(Copy(CurrentBlock.Condition, 1, Length(CurrentBlock.Condition) - 1));
        ABlocks.Add(CurrentBlock);
        ParseBlocks(ALines, ACurrentLine, CurrentBlock.NestedBlocks, CurrentBlock);
        CurrentBlock := nil;
      end
      else
        ABlocks.Add(CurrentBlock);
    end
    else if TrimmedLine.StartsWith('AfterDptGuard:') then
    begin
      CurrentBlock := TDptWorkflowBlock.Create;
      CurrentBlock.GuardType := gtAfter;
      CurrentBlock.Condition := Trim(Copy(TrimmedLine, 15, MaxInt));
      
      if CurrentBlock.Condition.EndsWith('{') then
      begin
        CurrentBlock.Condition := Trim(Copy(CurrentBlock.Condition, 1, Length(CurrentBlock.Condition) - 1));
        ABlocks.Add(CurrentBlock);
        ParseBlocks(ALines, ACurrentLine, CurrentBlock.NestedBlocks, CurrentBlock);
        CurrentBlock := nil;
      end
      else
        ABlocks.Add(CurrentBlock);
    end
    else if TrimmedLine.StartsWith('{') then
    begin
      if Assigned(CurrentBlock) then
      begin
        Remaining := Trim(Copy(TrimmedLine, 2, MaxInt));
        if Remaining.EndsWith('}') then
        begin
          CurrentBlock.Instructions := Trim(Copy(Remaining, 1, Length(Remaining) - 1));
          CurrentBlock := nil;
        end
        else
        begin
          if Remaining <> '' then
            CurrentBlock.Instructions := Remaining + sLineBreak;
          ParseBlocks(ALines, ACurrentLine, CurrentBlock.NestedBlocks, CurrentBlock);
          CurrentBlock := nil;
        end;
      end;
    end
    else if TrimmedLine.StartsWith('}') then
    begin
      Exit;
    end
    else
    begin
      if Assigned(CurrentBlock) then
        CurrentBlock.Condition := CurrentBlock.Condition + ' ' + TrimmedLine
      else if Assigned(AParentBlock) then
        AParentBlock.Instructions := AParentBlock.Instructions + Line + sLineBreak;
    end;
  end;
end;

procedure TDptWorkflowEngine.LoadWorkflow;
var
  Lines: TStringList;
  CurrentLine: Integer;
begin
  if not TFile.Exists(FWorkflowFile) then Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FWorkflowFile, TEncoding.UTF8);
    CurrentLine := 0;
    ParseBlocks(Lines, CurrentLine, FBlocks, nil);
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
  else if SameText(FuncName, 'IsCurrentBuildProjectFile') then
    ResVal := ExprParserIsCurrentBuildProjectFile(Args)
  else if SameText(FuncName, 'GetCurrentLintTargetFile') then
    ResVal := ExprParserGetCurrentLintTargetFile
  else if SameText(FuncName, 'IsFileRegisteredInAiSession') then
    ResVal := ExprParserIsFileRegisteredInAiSession(Args)
  else if SameText(FuncName, 'GetCurrentProjectFiles') then
    ResVal := ExprParserGetCurrentProjectFiles
  else if SameText(FuncName, 'HasValidLintResult') then
    ResVal := ExprParserHasValidLintResult(Args)
  else if SameText(FuncName, 'HasValidRunResult') then
    ResVal := ExprParserHasValidRunResult(Args)
  else if SameText(FuncName, 'GetInvalidLintFiles') then
    ResVal := ExprParserGetInvalidLintFiles(Args)
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

function TDptWorkflowEngine.ExprParserIsCurrentBuildProjectFile(const Args: Variant): Variant;
begin
  Result := False;
  if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
    Result := SameText(ExtractFileName(VarToStr(Args[0])), ExtractFileName(FCurrentProjectFile));
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
        var FileTime := TFile.GetLastWriteTime(FileName);
        
        // Use a small tolerance (1 second) to account for file system precision and timing
        if FileTime < (FSession.StartTime - EncodeTime(0, 0, 1, 0)) then
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

function TDptWorkflowEngine.ExprParserHasValidRunResult(const Args: Variant): Variant;
var
  TargetName: string;
  FilesVar: Variant;
  I: Integer;
  FileName: string;
  RunEntry: TDptSessionRunResult;
  Found: Boolean;
begin
  Result := True; // Default to valid if no session or bad args
  if Assigned(FSession) and VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 1) then
  begin
    TargetName := VarToStr(Args[0]); // Project file or Target name
    FilesVar := Args[1]; // Dependent files

    // 1. Find Run Result
    Found := False;
    for RunEntry in FSession.RunResults do
    begin
      if SameText(RunEntry.Target, TargetName) then
      begin
        Found := True;
        break;
      end;
    end;

    if not Found or
       (RunEntry.ExitCode <> 0) then
      Exit(False);

    // 3. Check Timestamps
    if VarIsArray(FilesVar) then
    begin
      for I := 0 to VarArrayHighBound(FilesVar, 1) do
      begin
        FileName := VarToStr(FilesVar[I]);
        if not TFile.Exists(FileName) then Continue;
        FileName := ExpandFileName(FileName);
        
        // Use a small tolerance
        var Limit := RunEntry.RunTime + EncodeTime(0, 0, 1, 0);
        var FileTime := TFile.GetLastWriteTime(FileName);
        
        if FileTime > Limit then
          Exit(False);
      end;
    end;
  end;
end;

procedure TDptWorkflowEngine.RegisterRunResult(const ATarget: string; AExitCode: Integer);
var
  I: Integer;
  Entry: TDptSessionRunResult;
  Found: Boolean;
begin
  if not Assigned(FSession) then
    Exit;

  Found := False;
  for I := 0 to FSession.RunResults.Count - 1 do
  begin
    if SameText(FSession.RunResults[I].Target, ATarget) then
    begin
      Entry := FSession.RunResults[I];
      Entry.ExitCode := AExitCode;
      Entry.RunTime := Now;
      FSession.RunResults[I] := Entry;
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    Entry.Target := ATarget;
    Entry.ExitCode := AExitCode;
    Entry.RunTime := Now;
    FSession.RunResults.Add(Entry);
  end;

  FSession.SaveToFile(FSessionFile);
end;

function TDptWorkflowEngine.ExprParserGetInvalidLintFiles(const Args: Variant): Variant;
var
  FileName: string;
  FilesVar: Variant;
  InvalidFiles: TStringList;
  Found: Boolean;
  Entry: TDptSessionFileEntry;
  I: Integer;
begin
  Result := '';
  if Assigned(FSession) and VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) then
  begin
    FilesVar := Args[0];
    InvalidFiles := TStringList.Create;
    try
      if VarIsArray(FilesVar) then
      begin
        for I := 0 to VarArrayHighBound(FilesVar, 1) do
        begin
          FileName := VarToStr(FilesVar[I]);
          if not TFile.Exists(FileName) then Continue;
          FileName := ExpandFileName(FileName);
          
          // Only check files modified or created since session start
          if TFile.GetLastWriteTime(FileName) < FSession.StartTime then
            Continue;

          Found := False;
          for Entry in FSession.Files do
          begin
            if SameText(Entry.Path, FileName) then
            begin
              if Entry.LintSuccess and (Entry.LastLintTime >= TFile.GetLastWriteTime(FileName)) then
                Found := True;
              Break;
            end;
          end;

          if not Found then
            InvalidFiles.Add(ExtractFileName(FileName));
        end;
      end;
      
      Result := '';
      for I := 0 to InvalidFiles.Count - 1 do
      begin
        if I > 0 then Result := Result + sLineBreak;
        Result := Result + InvalidFiles[I];
      end;
    finally
      InvalidFiles.Free;
    end;
  end;
end;

function TDptWorkflowEngine.ProcessInstructions(AParser: TExprParser; const AStr: string): string;
var
  I, Start: Integer;
  Expr: string;
  InstrLines: TStringList;
  Line: string;
begin
  Result := '';
  InstrLines := TStringList.Create;
  try
    InstrLines.Text := AStr;
    for Line in InstrLines do
    begin
      // Skip lines that are likely commands (no backticks, but contain function calls)
      if (Line <> '') and (Pos('`', Line) = 0) and (Pos('(', Line) > 0) and (Pos(')', Line) > 0) then
        Continue;

      var ProcessedLine := '';
      I := 1;
      while I <= Length(Line) do
      begin
        if Line[I] = '`' then
        begin
          // Escaping: Double backtick becomes a single literal backtick
          if (I < Length(Line)) and (Line[I + 1] = '`') then
          begin
            ProcessedLine := ProcessedLine + '`';
            Inc(I, 2);
            Continue;
          end;

          // Find expression
          Start := I + 1;
          I := Start;
          while (I <= Length(Line)) and (Line[I] <> '`') do
            Inc(I);

          if I <= Length(Line) then
          begin
            Expr := Copy(Line, Start, I - Start);
            if AParser.Eval(Expr) then
            begin
              var Prefix := ProcessedLine;
              var EvalResult := VarToStr(AParser.Value);
              
              var LResultLines := TStringList.Create;
              try
                LResultLines.Text := EvalResult;
                // Remove trailing empty line if present
                if (LResultLines.Count > 0) and (LResultLines[LResultLines.Count-1] = '') then
                  LResultLines.Delete(LResultLines.Count-1);
                  
                for var LIdx := 0 to LResultLines.Count - 1 do
                begin
                  if LIdx > 0 then
                    ProcessedLine := ProcessedLine + sLineBreak + Prefix;
                  ProcessedLine := ProcessedLine + LResultLines[LIdx];
                end;
              finally
                LResultLines.Free;
              end;
            end
            else
              ProcessedLine := ProcessedLine + '`' + Expr + '`'; // Fallback: Keep as text if not a valid expression
            Inc(I);
          end
          else
          begin
            // Unclosed backtick: treat as literal text
            ProcessedLine := ProcessedLine + '`';
            I := Start;
          end;
        end
        else
        begin
          ProcessedLine := ProcessedLine + Line[I];
          Inc(I);
        end;
      end;
      
      Result := Result + ProcessedLine + sLineBreak;
    end;
  finally
    InstrLines.Free;
  end;
end;

procedure TDptWorkflowEngine.EvalBlocks(AParser: TExprParser; const ABlocks: IList<TDptWorkflowBlock>; AGuardType: TDptGuardType; var AInstructions: string);
var
  Block: TDptWorkflowBlock;
  InstrLines: TStringList;
  Line: string;
begin
  for Block in ABlocks do
  begin
    if FExitRequested then Exit;

    if (Block.GuardType = AGuardType) and AParser.Eval(Block.Condition) then
    begin
      if AParser.Value <> 0 then
      begin
        var RawInstructions := Block.Instructions;
        // Remove only purely empty lines at the very beginning or end
        while (RawInstructions <> '') and ((RawInstructions[1] = #13) or (RawInstructions[1] = #10)) do
          Delete(RawInstructions, 1, 1);
        while (RawInstructions <> '') and ((RawInstructions[Length(RawInstructions)] = #13) or (RawInstructions[Length(RawInstructions)] = #10)) do
          Delete(RawInstructions, Length(RawInstructions), 1);

        if RawInstructions <> '' then
        begin
          // Execute commands (lines without backticks)
          InstrLines := TStringList.Create;
          try
            InstrLines.Text := RawInstructions;
            for Line in InstrLines do
            begin
              var TrimmedLine := Trim(Line);
              if (TrimmedLine <> '') and (Pos('`', TrimmedLine) = 0) then
                AParser.Eval(TrimmedLine);
            end;
          finally
            InstrLines.Free;
          end;

          var Processed := ProcessInstructions(AParser, RawInstructions);
          if Processed <> '' then
            AInstructions := AInstructions + Processed + sLineBreak;
        end;
        
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

  if TFile.Exists(FSessionFile) then
  begin
    Writeln('AI session is already active for PID ', FHostPID);
    Writeln('To restart the session and reset the baseline time, use:');
    Writeln('  DPT.exe LATEST AiSession Reset');
    Exit;
  end;
    
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
    // We explicitly keep the StartTime to prevent cheating the baseline
    FSession.SaveToFile(FSessionFile);
    Writeln('AI session reset. Success list cleared, but baseline time remains: ', DateTimeToStr(FSession.StartTime));
  end;
end;

procedure TDptWorkflowEngine.AddFilesToSession(const AFiles: TArray<string>);
var
  FileName: string;
  Entry: TDptSessionFileEntry;
  FullFileName: string;
  ExistingEntry: TDptSessionFileEntry;
  AlreadyRegistered: Boolean;
begin
  for FileName in AFiles do
  begin
    FullFileName := ExpandFileName(FileName);
    if not TFile.Exists(FullFileName) then
    begin
      Writeln('ERROR: File not found: ', FileName);
      raise Exception.Create('Cannot register non-existent file: ' + FileName);
    end;

    AlreadyRegistered := False;
    for ExistingEntry in FSession.Files do
    begin
      if SameText(ExistingEntry.Path, FullFileName) then
      begin
        AlreadyRegistered := True;
        Break;
      end;
    end;

    if AlreadyRegistered then
    begin
      Writeln('File already registered in AI session: ', FileName);
      Continue;
    end;

    Entry.Path := FullFileName;
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

procedure TDptWorkflowEngine.SetCurrentProjectFile(const AFile: string);
begin
  FCurrentProjectFile := AFile;
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
