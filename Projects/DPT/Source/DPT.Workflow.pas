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
  System.SysUtils,
  System.Variants,

  ExprParser,

  DPT.Types,
  DPT.Git;

type

  TDptWorkflowAction = (waNone, waExit);

  TDptGuardType = (gtBefore, gtAfter);

  TDptWorkflowBlock = class
  public
    Condition   : String;
    GuardType   : TDptGuardType;
    Instructions: String;
    NestedBlocks: IList<TDptWorkflowBlock>;
    constructor Create;
  end;

  TDptWorkflowEngine = class
  private
    FAIMode                : TAIMode;
    FBlocks                : IList<TDptWorkflowBlock>;
    FCurrentAction         : String;
    FCurrentProjectFile    : String;
    FCurrentProjectFiles   : TArray<String>;
    FExitCode              : Integer;
    FExitRequested         : Boolean;
    FHostPID               : DWORD;
    FIgnorePatterns        : TStringList;
    FLintTarget            : String;
    FLastFixedFiles        : TStringList;
    FLastFormattedFiles    : TStringList;
    FWorkflowFile          : String;
    procedure EvalBlocks(AParser: TExprParser; const ABlocks: IList<TDptWorkflowBlock>; AGuardType: TDptGuardType; var AInstructions: string);
    function  ExprParserFixLineEndingsWindowsInGitModifiedFiles: Variant;
    function  ExprParserFixUtf8BomInGitModifiedFiles: Variant;
    function  ExprParserFormatGitModifiedFiles(const Args: Variant): Variant;
    function  ExprParserGetCurrentProjectFiles: Variant;
    function  ExprParserGetExitCode: Variant;
    function  ExprParserGetLastFixedFiles: Variant;
    function  ExprParserGetLastFormattedFiles: Variant;
    function  ExprParserIgnoreFormatPattern(const Args: Variant): Variant;
    function  ExprParserIsCurrentAction(const Args: Variant): Variant;
    function  ExprParserIsCurrentBuildProjectFile(const Args: Variant): Variant;
    function  ExprParserRequestDptExit: Variant;
    function  ExprParserRequestDptExitWithCode(const Args: Variant): Variant;
    function  FindWorkflowFile: String;
    procedure LoadWorkflow;
    function  OnExecuteFunctionCallback(Sender: TObject; const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;
    function  OnGetVariableCallback(Sender: TObject; const VarName: string; var Value: Variant): Boolean;
    procedure ParseBlocks(ALines: TStrings; var ACurrentLine: Integer; const ABlocks: IList<TDptWorkflowBlock>; AParentBlock: TDptWorkflowBlock);
    function  ProcessInstructions(AParser: TExprParser; const AStr: string): string;
  public
    constructor Create(const AAction: string);
    destructor  Destroy; override;
    function  CheckConditions(out AInstructions: string; AGuardType: TDptGuardType = gtBefore): TDptWorkflowAction;
    procedure SetCurrentProjectFile(const AFile: string);
    procedure SetExitCode(ACode: Integer);
    procedure SetLintTarget(const ATarget: string);
    procedure SetProjectFiles(const AFiles: TArray<string>);
    property WorkflowFile: string read FWorkflowFile;
    property HostPID: DWORD read FHostPID;
    property ExitCode: Integer read FExitCode write SetExitCode;
  end;

implementation

uses

  System.IOUtils,
  System.Math,
  System.StrUtils,
  System.Masks,

  ExprParserTools,

  ParseTree.Nodes,
  ParseTree.Parser,
  ParseTree.Writer,
  DPT.Formatter.DWS,

  DPT.Detection;

const
  WorkflowFileName = '.DptAiWorkflow';

{ TDptWorkflowBlock }

constructor TDptWorkflowBlock.Create;
begin
  NestedBlocks := Collections.NewList<TDptWorkflowBlock>;
end;

{ TDptWorkflowEngine }

constructor TDptWorkflowEngine.Create(const AAction: string);
begin
  FBlocks := Collections.NewList<TDptWorkflowBlock>;
  FIgnorePatterns := TStringList.Create;
  FLastFixedFiles := TStringList.Create;
  FLastFormattedFiles := TStringList.Create;
  FCurrentAction := AAction;
  FHostPID := GetCurrentProcessId;
  
  FAIMode := DetectAIMode(FHostPID);
  if FAIMode <> amNone then
  begin
    FWorkflowFile := FindWorkflowFile;
    var IsMcpDebugger := False;
    for var i := 1 to ParamCount do
      if SameText(ParamStr(i), 'McpDebugger') then IsMcpDebugger := True;

    if FWorkflowFile <> '' then
    begin
      if not IsMcpDebugger then Writeln('AI Workflow file FOUND: ', FWorkflowFile);
      LoadWorkflow;
    end
    else
      if not IsMcpDebugger then Writeln('AI Workflow file NOT found. Searched up from: ', GetCurrentDir);
  end;
end;

destructor TDptWorkflowEngine.Destroy;
begin
  FIgnorePatterns.Free;
  FLastFixedFiles.Free;
  FLastFormattedFiles.Free;
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
  Result := False;
end;

function TDptWorkflowEngine.OnExecuteFunctionCallback(Sender: TObject; const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;
begin
  if SameText(FuncName, 'IsCurrentAction') then
    ResVal := ExprParserIsCurrentAction(Args)
  else if SameText(FuncName, 'IsCurrentBuildProjectFile') then
    ResVal := ExprParserIsCurrentBuildProjectFile(Args)
  else if SameText(FuncName, 'GetCurrentProjectFiles') then
    ResVal := ExprParserGetCurrentProjectFiles
  else if SameText(FuncName, 'RequestDptExit') then
    ResVal := ExprParserRequestDptExit
  else if SameText(FuncName, 'RequestDptExitWithCode') then
    ResVal := ExprParserRequestDptExitWithCode(Args)
  else if SameText(FuncName, 'GetExitCode') then
    ResVal := ExprParserGetExitCode
  else if SameText(FuncName, 'FixLineEndingsWindowsInGitModifiedFiles') then
    ResVal := ExprParserFixLineEndingsWindowsInGitModifiedFiles
  else if SameText(FuncName, 'FixUtf8BomInGitModifiedFiles') then
    ResVal := ExprParserFixUtf8BomInGitModifiedFiles
  else if SameText(FuncName, 'GetLastFixedFiles') then
    ResVal := ExprParserGetLastFixedFiles
  else if SameText(FuncName, 'IgnoreFormatPattern') then
    ResVal := ExprParserIgnoreFormatPattern(Args)
  else if SameText(FuncName, 'FormatGitModifiedFiles') then
    ResVal := ExprParserFormatGitModifiedFiles(Args)
  else if SameText(FuncName, 'GetLastFormattedFiles') then
    ResVal := ExprParserGetLastFormattedFiles
  else
    Exit(False);

  Result := True;
end;

function TDptWorkflowEngine.ExprParserIgnoreFormatPattern(const Args: Variant): Variant;
var
  I: Integer;
begin
  Result := True;
  if VarIsArray(Args) then
  begin
    for I := 0 to VarArrayHighBound(Args, 1) do
    begin
      if not VarIsClear(Args[I]) then
        FIgnorePatterns.Add(VarToStr(Args[I]));
    end;
  end
  else if not VarIsClear(Args) then
    FIgnorePatterns.Add(VarToStr(Args));
end;

function TDptWorkflowEngine.ExprParserFormatGitModifiedFiles(const Args: Variant): Variant;
var
  ModifiedFiles: TArray<string>;
  FileName: string;
  IgnorePattern: string;
  SkipFile: Boolean;
  Content: string;
  NewContent: string;
  ScriptFile: string;
  Formatter: TDptDwsFormatter;
  PTParser: TParseTreeParser;
  PTWriter: TSyntaxTreeWriter;
  PTUnit: TCompilationUnitSyntax;
begin
  FLastFormattedFiles.Clear;
  Result := False;

  if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
    ScriptFile := VarToStr(Args[0])
  else
    Exit; // Script file is required

  if not TFile.Exists(ScriptFile) then
  begin
    // Try to find it relative to the workflow file or app dir
    var Candidate := TPath.Combine(ExtractFilePath(FWorkflowFile), ScriptFile);
    if TFile.Exists(Candidate) then
      ScriptFile := Candidate
    else
    begin
      Candidate := TPath.Combine(TPath.Combine(ExtractFilePath(ParamStr(0)), 'Format'), ScriptFile);
      if TFile.Exists(Candidate) then
        ScriptFile := Candidate;
    end;
  end;

  if not TFile.Exists(ScriptFile) then
    Exit;

  Formatter := TDptDwsFormatter.Create;
  try
    Formatter.LoadScript(ScriptFile);

    ModifiedFiles := TDptGit.GetModifiedFiles(GetCurrentDir);
    for FileName in ModifiedFiles do
    begin
      if not TFile.Exists(FileName) then Continue;
      
      // Nur Delphi Dateien
      var Ext := LowerCase(ExtractFileExt(FileName));
      if (Ext <> '.pas') and (Ext <> '.dpr') and (Ext <> '.dpk') then
        Continue;

      SkipFile := False;
      for IgnorePattern in FIgnorePatterns do
      begin
        if MatchesMask(FileName, IgnorePattern) or MatchesMask(ExtractFileName(FileName), IgnorePattern) then
        begin
          SkipFile := True;
          Break;
        end;
      end;

      if SkipFile then Continue;

      try
        Content := TFile.ReadAllText(FileName, TEncoding.UTF8);
        
        PTParser := TParseTreeParser.Create;
        try
          PTUnit := PTParser.Parse(Content);
          try
            Formatter.FormatUnit(PTUnit);
            PTWriter := TSyntaxTreeWriter.Create;
            try
              NewContent := PTWriter.GenerateSource(PTUnit);
            finally
              PTWriter.Free;
            end;
          finally
            PTUnit.Free;
          end;
        finally
          PTParser.Free;
        end;
        
        if Content <> NewContent then
        begin
          TFile.WriteAllText(FileName, NewContent, TEncoding.UTF8);
          FLastFormattedFiles.Add(ExtractFileName(FileName));
          Result := True;
        end;
      except
        // Fehler beim Formatieren ignorieren
      end;
    end;
  finally
    Formatter.Free;
  end;
end;

function TDptWorkflowEngine.ExprParserGetLastFormattedFiles: Variant;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FLastFormattedFiles.Count - 1 do
  begin
    if I > 0 then Result := Result + sLineBreak;
    Result := Result + FLastFormattedFiles[I];
  end;
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

function TDptWorkflowEngine.ExprParserIsCurrentBuildProjectFile(const Args: Variant): Variant;
begin
  Result := False;
  if VarIsArray(Args) and (VarArrayHighBound(Args, 1) >= 0) and (not VarIsClear(Args[0])) then
    Result := SameText(ExtractFileName(VarToStr(Args[0])), ExtractFileName(FCurrentProjectFile));
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

function TDptWorkflowEngine.ExprParserFixLineEndingsWindowsInGitModifiedFiles: Variant;
var
  ModifiedFiles: TArray<string>;
  Content: string;
  NewContent: string;
  FileName: string;
begin
  FLastFixedFiles.Clear;
  Result := False;
  ModifiedFiles := TDptGit.GetModifiedFiles(GetCurrentDir);
  for FileName in ModifiedFiles do
  begin
    if TFile.Exists(FileName) then
    begin
      try
        Content := TFile.ReadAllText(FileName);
        // Standardize to CRLF
        NewContent := AdjustLineBreaks(Content, tlbsCRLF);
        if Content <> NewContent then
        begin
          TFile.WriteAllText(FileName, NewContent, TEncoding.UTF8);
          FLastFixedFiles.Add(ExtractFileName(FileName));
          Result := True;
        end;
      except
        // Ignore errors during fix
      end;
    end;
  end;
end;

function TDptWorkflowEngine.ExprParserFixUtf8BomInGitModifiedFiles: Variant;
var
  ModifiedFiles: TArray<string>;
  FileName: string;
  Bytes: TBytes;
  BOM: TBytes;
begin
  FLastFixedFiles.Clear;
  Result := False;
  BOM := TEncoding.UTF8.GetPreamble;
  ModifiedFiles := TDptGit.GetModifiedFiles(GetCurrentDir);
  for FileName in ModifiedFiles do
  begin
    if TFile.Exists(FileName) then
    begin
      try
        Bytes := TFile.ReadAllBytes(FileName);
        if (Length(Bytes) < Length(BOM)) or
           (Bytes[0] <> BOM[0]) or
           (Bytes[1] <> BOM[1]) or
           (Bytes[2] <> BOM[2]) then
        begin
          // Write back with UTF8 encoding which adds BOM
          TFile.WriteAllText(FileName, TFile.ReadAllText(FileName), TEncoding.UTF8);
          FLastFixedFiles.Add(ExtractFileName(FileName));
          Result := True;
        end;
      except
        // Ignore
      end;
    end;
  end;
end;

function TDptWorkflowEngine.ExprParserGetLastFixedFiles: Variant;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FLastFixedFiles.Count - 1 do
  begin
    if I > 0 then Result := Result + sLineBreak;
    Result := Result + FLastFixedFiles[I];
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
      begin
        // Exception: If it looks like a list item, treat as text
        if not Trim(Line).StartsWith('-') then
          Continue;
      end;

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

procedure TDptWorkflowEngine.SetLintTarget(const ATarget: string);
begin
  FLintTarget := ATarget;
end;

end.
