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
  
  mormot.core.base,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.datetime,
  mormot.core.unicode,
  mormot.core.json,
  mormot.core.text,
  
  ExprParser,
  ExprParserTools,
  DPT.Detection;

type
  TDptWorkflowAction = (waNone, waExit);

  TDptWorkflowBlock = class
  public
    Condition: string;
    Instructions: string;
    Action: TDptWorkflowAction;
  end;

  TDptSessionFileEntry = record
    Path: string;
    Hash: string;
    LastLintTime: TDateTime;
    LintSuccess: Boolean;
  end;

  TDptSessionData = class
  public
    HostPID: DWORD;
    StartTime: TDateTime;
    Files: TList<TDptSessionFileEntry>;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
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
    
    property Session: TDptSessionData read FSession;
    property WorkflowFile: string read FWorkflowFile;
  end;

implementation

const
  WorkflowFileName = '.DptAiWorkflow';

{ TDptSessionData }

constructor TDptSessionData.Create;
begin
  Files := TList<TDptSessionFileEntry>.Create;
end;

destructor TDptSessionData.Destroy;
begin
  Files.Free;
  inherited;
end;

procedure TDptSessionData.LoadFromFile(const AFileName: string);
var
  JSON: RawUTF8;
  Doc: Variant; 
  I: Integer;
  Entry: TDptSessionFileEntry;
  FilesVar: Variant;
begin
  Files.Clear;
  if not TFile.Exists(AFileName) then Exit;
  
  JSON := StringToUTF8(TFile.ReadAllText(AFileName));
  Doc := _Json(JSON);
  
  if VarIsEmptyOrNull(Doc) then Exit;
  
  HostPID := Doc.HostPID;
  // Explicit cast to RawUTF8 to avoid implicit conversion warning from Variant
  StartTime := Iso8601ToDateTime(RawUTF8(Doc.StartTime));
  
  FilesVar := Doc.Files;
  if VarIsArray(FilesVar) then
  begin
    for I := 0 to VarArrayHighBound(FilesVar, 1) do
    begin
      var Item: Variant := FilesVar[I];
      Entry.Path := Item.Path;
      Entry.Hash := Item.Hash;
      // Explicit cast to RawUTF8
      Entry.LastLintTime := Iso8601ToDateTime(RawUTF8(Item.LastLintTime));
      Entry.LintSuccess := Item.LintSuccess;
      Files.Add(Entry);
    end;
  end;
end;

procedure TDptSessionData.SaveToFile(const AFileName: string);
var
  Doc: Variant;
  FilesArr: Variant;
  I: Integer;
begin
  Doc := _Obj(['HostPID', Integer(HostPID), 'StartTime', DateTimeToIso8601(StartTime, True)]);
  
  TDocVariant.NewArray(FilesArr);
  for I := 0 to Files.Count - 1 do
  begin
    var FileDoc: Variant := _Obj([
      'Path', StringToUTF8(Files[I].Path),
      'Hash', StringToUTF8(Files[I].Hash),
      'LastLintTime', DateTimeToIso8601(Files[I].LastLintTime, True),
      'LintSuccess', Files[I].LintSuccess
    ]);
    TDocVariantData(FilesArr).AddItem(FileDoc);
  end;
  
  Doc.Files := FilesArr;
  
  TFile.WriteAllText(AFileName, UTF8ToString(VariantSaveJSON(Doc, twJsonEscape)));
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
    var ParentDir := TPath.GetDirectoryName(CurrentDir);
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
      if Line = '' then Continue;
      
      // Use Pos/Copy instead of string helpers to avoid E2018 if helpers are shadowed/unavailable
      if Pos('DptCondition:', Line) = 1 then // StartsWith(..., True) logic simplified
      begin
        Block := TDptWorkflowBlock.Create;
        Block.Condition := System.SysUtils.Trim(Copy(Line, 14, MaxInt));
        FBlocks.Add(Block);
      end
      else if Pos('{', Line) = 1 then
        InBlock := True
      else if Pos('}', Line) = 1 then
      begin
        InBlock := False;
        Block := nil;
      end
      else if InBlock and Assigned(Block) then
      begin
        if Pos('DptAction:', Line) = 1 then
        begin
          ActionStr := System.SysUtils.Trim(Copy(Line, 11, MaxInt));
          if Pos('ExitDptProcess', ActionStr) = 1 then
            Block.Action := waExit;
        end
        else
          Block.Instructions := Block.Instructions + Lines[I] + sLineBreak;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TDptWorkflowEngine.OnGetVariableCallback(Sender: TObject; const VarName: string; var Value: Variant): Boolean;
begin
  if System.SysUtils.SameText(VarName, 'AiSessionStarted') then
  begin
    Value := Assigned(FSession) and TFile.Exists(FSessionFile);
    Result := True;
  end
  else
    Result := False;
end;

function TDptWorkflowEngine.OnExecuteFunctionCallback(Sender: TObject; const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;
begin
  Result := True;
  if System.SysUtils.SameText(FuncName, 'IsCurrentAction') then
    ResVal := System.SysUtils.SameText(FCurrentAction, VarToStr(Args[0]))
  else if System.SysUtils.SameText(FuncName, 'IsCurrentAiSessionAction') then
    ResVal := System.SysUtils.SameText(FCurrentAiSessionAction, VarToStr(Args[0]))
  else if System.SysUtils.SameText(FuncName, 'HasValidLintResult') then
  begin
    // TODO: Logic for checking lint results
    ResVal := False; 
  end
  else if System.SysUtils.SameText(FuncName, 'IsFileRegisteredInAiSession') then
  begin
    // TODO: Check session data
    ResVal := False;
  end
  else if System.SysUtils.SameText(FuncName, 'GetCurrentLintTargetFile') then
    ResVal := FCurrentLintTargetFile
  else if System.SysUtils.SameText(FuncName, 'GetCurrentProjectFiles') then
    ResVal := '' // Placeholder
  else
    Result := False;
end;

function TDptWorkflowEngine.CheckConditions(out AInstructions: string): TDptWorkflowAction;
var
  Parser: TExprParser;
  Block: TDptWorkflowBlock;
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
        if (TVarData(Parser.Value).VType = varBoolean) and (Parser.Value = True) then
        begin
          // Explicitly cast Block.Instructions to string to match sLineBreak
          AInstructions := AInstructions + string(Trim(Block.Instructions)) + sLineBreak + sLineBreak;
          if Block.Action > Result then
            Result := Block.Action;
        end;
      end;
    end;
  finally
    Parser.Free;
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
