// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.McpLinter.Server;

interface

uses

  System.Classes,
  System.JSON,
  System.SyncObjs,

  mormot.core.collections;

type

  TLineAdjustment = record
    OriginalLine: Integer;
    Delta       : Integer;
  end;

  TLineOffsetTracker = class
  private
    FAdjustments: IList<TLineAdjustment>;
  public
    constructor Create;
    procedure Reset;
    function  TranslateLine(AOriginalLine: Integer): Integer;
    procedure RecordReplacement(AOrigStartLine, AOrigEndLine, ANewLineCount: Integer);
  end;

  TMcpLinterServer = class
  private
    FExitRequest    : Boolean;
    FInputReader    : TTextReader;
    FOffsetTrackers : IKeyValue<String, TLineOffsetTracker>;
    FOutputLock     : TCriticalSection;
    FOutputWriter   : TTextWriter;
    function  GetOrCreateTracker(const AFile: String): TLineOffsetTracker;
    function  GetFileLineCount(const AFile: String): Integer;
    function  MakeErrorResult(const AText: String): TJSONObject;
    function  MakeTextResult(const AText: String): TJSONObject;
    function  NormalizeLineEndings(const AText: String): String;
    procedure ProcessMessage(const AMessage: String);
    procedure SendError(const AID: TJSONValue; ACode: Integer; const AMessage: String);
    procedure SendResponse(const AID: TJSONValue; AResult: TJSONObject);
    procedure WriteOutput(const AJSON: String);
  private
    function HandleDeleteLines(AParams: TJSONObject): TJSONObject;
    function HandleGetLinterResults(AParams: TJSONObject): TJSONObject;
    function HandleListTools(AParams: TJSONObject): TJSONObject;
    function HandleReadCodeLines(AParams: TJSONObject): TJSONObject;
    function HandleReplaceCodeLines(AParams: TJSONObject): TJSONObject;
  public
    constructor Create(AInput: TTextReader = nil; AOutput: TTextWriter = nil);
    destructor  Destroy; override;
    procedure Run;
    procedure RunOnce;
  end;

implementation

uses

  System.IniFiles,
  System.IOUtils,
  System.SysUtils,

  DPT.Lint.Context,
  DPT.Lint.Task,
  DPT.Task,
  DPT.Types;

type

  TDptSilentLintTask = class(TDptLintTask)
  protected
    procedure Writeln(const Text: String = ''); override;
  end;

{ TDptSilentLintTask }

procedure TDptSilentLintTask.Writeln(const Text: String);
begin
  // Suppress all output to avoid corrupting the JSON-RPC stream
end;

{ TLineOffsetTracker }

constructor TLineOffsetTracker.Create;
begin
  inherited Create;
  FAdjustments := Collections.NewPlainList<TLineAdjustment>;
end;

procedure TLineOffsetTracker.Reset;
begin
  FAdjustments.Clear;
end;

function TLineOffsetTracker.TranslateLine(AOriginalLine: Integer): Integer;
var
  I: Integer;
begin
  Result := AOriginalLine;
  for I := 0 to FAdjustments.Count - 1 do
  begin
    if FAdjustments[I].OriginalLine <= AOriginalLine then
      Result := Result + FAdjustments[I].Delta;
  end;
end;

procedure TLineOffsetTracker.RecordReplacement(AOrigStartLine, AOrigEndLine, ANewLineCount: Integer);
var
  Adj: TLineAdjustment;
begin
  Adj.OriginalLine := AOrigStartLine;
  Adj.Delta := ANewLineCount - (AOrigEndLine - AOrigStartLine + 1);
  if Adj.Delta <> 0 then
    FAdjustments.Add(Adj);
end;

{ TMcpLinterServer }

constructor TMcpLinterServer.Create(AInput: TTextReader; AOutput: TTextWriter);
begin
  inherited Create;
  FExitRequest := False;
  FInputReader := AInput;
  FOutputWriter := AOutput;
  FOutputLock := TCriticalSection.Create;
  FOffsetTrackers := Collections.NewKeyValue<String, TLineOffsetTracker>;
end;

destructor TMcpLinterServer.Destroy;
begin
  FOutputLock.Free;
  inherited Destroy;
end;

function TMcpLinterServer.GetOrCreateTracker(const AFile: String): TLineOffsetTracker;
var
  Key: String;
begin
  Key := AnsiLowerCase(AFile);
  if not FOffsetTrackers.TryGetValue(Key, Result) then
  begin
    Result := TLineOffsetTracker.Create;
    FOffsetTrackers.Add(Key, Result);
  end;
end;

function TMcpLinterServer.GetFileLineCount(const AFile: String): Integer;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFile, TEncoding.UTF8);
    Result := Lines.Count;
  finally
    Lines.Free;
  end;
end;

function TMcpLinterServer.NormalizeLineEndings(const AText: String): String;
begin
  Result := AText.Replace(#13#10, #10).Replace(#13, #10).Replace(#10, #13#10);
end;

procedure TMcpLinterServer.WriteOutput(const AJSON: String);
begin
  FOutputLock.Enter;
  try
    if Assigned(FOutputWriter) then
    begin
      FOutputWriter.WriteLine(AJSON);
      FOutputWriter.Flush;
    end
    else
    begin
      System.Write(AJSON + #13#10);
      System.Flush(System.Output);
    end;
  finally
    FOutputLock.Leave;
  end;
end;

procedure TMcpLinterServer.SendResponse(const AID: TJSONValue; AResult: TJSONObject);
var
  Resp: TJSONObject;
begin
  Resp := TJSONObject.Create;
  try
    Resp.AddPair('jsonrpc', '2.0');
    if AID <> nil then
      Resp.AddPair('id', AID.Clone as TJSONValue);
    Resp.AddPair('result', AResult);
    WriteOutput(Resp.ToJSON);
  finally
    Resp.Free;
  end;
end;

procedure TMcpLinterServer.SendError(const AID: TJSONValue; ACode: Integer; const AMessage: String);
var
  Err : TJSONObject;
  Resp: TJSONObject;
begin
  Resp := TJSONObject.Create;
  try
    Resp.AddPair('jsonrpc', '2.0');
    if AID <> nil then
      Resp.AddPair('id', AID.Clone as TJSONValue);
    Err := TJSONObject.Create;
    Err.AddPair('code', TJSONNumber.Create(ACode));
    Err.AddPair('message', AMessage);
    Resp.AddPair('error', Err);
    WriteOutput(Resp.ToJSON);
  finally
    Resp.Free;
  end;
end;

function TMcpLinterServer.MakeTextResult(const AText: String): TJSONObject;
var
  ContentArr: TJSONArray;
begin
  Result := TJSONObject.Create;
  ContentArr := TJSONArray.Create;
  ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', AText));
  Result.AddPair('content', ContentArr);
end;

function TMcpLinterServer.MakeErrorResult(const AText: String): TJSONObject;
var
  ContentArr: TJSONArray;
begin
  Result := TJSONObject.Create;
  ContentArr := TJSONArray.Create;
  ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', AText));
  Result.AddPair('content', ContentArr);
  Result.AddPair('isError', TJSONBool.Create(True));
end;

procedure TMcpLinterServer.ProcessMessage(const AMessage: String);
var
  ID       : TJSONValue;
  JSON     : TJSONObject;
  Method   : String;
  MethodVal: TJSONValue;
  Params   : TJSONObject;
  ResultObj: TJSONObject;
begin
  try
    JSON := TJSONObject.ParseJSONValue(AMessage) as TJSONObject;
    if JSON = nil then
      Exit;
    try
      ID := JSON.GetValue('id');
      MethodVal := JSON.GetValue('method');
      if MethodVal = nil then
        Exit;
      Method := MethodVal.Value;
      Params := JSON.GetValue('params') as TJSONObject;

      if Method = 'initialize' then
      begin
        ResultObj := TJSONObject.Create;
        ResultObj.AddPair('protocolVersion', '2024-11-05');
        var CapObj := TJSONObject.Create;
        CapObj.AddPair('tools', TJSONObject.Create);
        ResultObj.AddPair('capabilities', CapObj);
        ResultObj.AddPair('serverInfo', TJSONObject.Create.AddPair('name', 'DPT-Linter').AddPair('version', '1.0.0'));
        SendResponse(ID, ResultObj);
      end
      else if Method = 'notifications/initialized' then
      begin
        // Handled
      end
      else if Method = 'tools/list' then
        SendResponse(ID, HandleListTools(Params))
      else if Method = 'tools/call' then
      begin
        if Params = nil then
        begin
          SendError(ID, -32602, 'Missing params');
          Exit;
        end;
        var NameVal := Params.GetValue('name');
        if NameVal = nil then
        begin
          SendError(ID, -32602, 'Missing tool name');
          Exit;
        end;
        var ToolName := NameVal.Value;
        var ToolParams := Params.GetValue('arguments') as TJSONObject;

        if ToolName = 'get_linter_results' then
          SendResponse(ID, HandleGetLinterResults(ToolParams))
        else if ToolName = 'read_code_lines' then
          SendResponse(ID, HandleReadCodeLines(ToolParams))
        else if ToolName = 'replace_code_lines' then
          SendResponse(ID, HandleReplaceCodeLines(ToolParams))
        else if ToolName = 'delete_lines' then
          SendResponse(ID, HandleDeleteLines(ToolParams))
        else
          SendError(ID, -32601, 'Tool not found');
      end
      else
        SendError(ID, -32601, 'Method not found');
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      SendError(nil, -32603, E.Message);
  end;
end;

function TMcpLinterServer.HandleListTools(AParams: TJSONObject): TJSONObject;
begin
  var ToolsArr := TJSONArray.Create;

  // get_linter_results
  var ToolLint := TJSONObject.Create;
  ToolLint.AddPair('name', 'get_linter_results');
  ToolLint.AddPair('description',
    'Runs the linting pipeline for a Delphi source file against a style definition and returns all style violations. ' +
    'Each violation includes the line number and a descriptive message. ' +
    'This resets the line offset tracker for the file, so subsequent read_code_lines and replace_code_lines calls use the line numbers from this result. ' +
    'After fixing violations, call this again to verify all issues are resolved.');
  var SchemaLint := TJSONObject.Create;
  SchemaLint.AddPair('type', 'object');
  var PropLint := TJSONObject.Create;
  PropLint.AddPair('file', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Absolute path to the Delphi source file (.pas) to lint'));
  PropLint.AddPair('style_file', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Absolute path to the style definition file (e.g. TaifunUnitStyle.pas)'));
  SchemaLint.AddPair('properties', PropLint);
  var ReqLint := TJSONArray.Create;
  ReqLint.Add('file');
  ReqLint.Add('style_file');
  SchemaLint.AddPair('required', ReqLint);
  ToolLint.AddPair('inputSchema', SchemaLint);
  ToolsArr.Add(ToolLint);

  // read_code_lines
  var ToolRead := TJSONObject.Create;
  ToolRead.AddPair('name', 'read_code_lines');
  ToolRead.AddPair('description',
    'Reads a range of lines from a file. Each line is returned with its line number in the format "NNN| content". ' +
    'Line numbers are automatically adjusted if previous replace_code_lines calls shifted lines since the last get_linter_results. ' +
    'The response includes total_lines (total line count of the file).');
  var SchemaRead := TJSONObject.Create;
  SchemaRead.AddPair('type', 'object');
  var PropRead := TJSONObject.Create;
  PropRead.AddPair('file', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Absolute path to the file to read'));
  PropRead.AddPair('start_line', TJSONObject.Create.AddPair('type', 'integer').AddPair('description', 'First line to read (1-based)'));
  PropRead.AddPair('end_line', TJSONObject.Create.AddPair('type', 'integer').AddPair('description', 'Last line to read (1-based, inclusive)'));
  SchemaRead.AddPair('properties', PropRead);
  var ReqRead := TJSONArray.Create;
  ReqRead.Add('file');
  ReqRead.Add('start_line');
  ReqRead.Add('end_line');
  SchemaRead.AddPair('required', ReqRead);
  ToolRead.AddPair('inputSchema', SchemaRead);
  ToolsArr.Add(ToolRead);

  // replace_code_lines
  var ToolReplace := TJSONObject.Create;
  ToolReplace.AddPair('name', 'replace_code_lines');
  ToolReplace.AddPair('description',
    'Replaces a range of lines in a file with new content. Line numbers refer to the original positions from the last get_linter_results call; ' +
    'the server automatically adjusts for any shifts caused by previous replacements. ' +
    'Line endings in new_content are automatically normalized to Windows format (CRLF). The file encoding (UTF-8 with BOM) is preserved. ' +
    'IMPORTANT: new_content must contain ONLY the replacement for the specified line range. ' +
    'Do NOT include lines that are outside the range (before start_line or after end_line) -- they will be duplicated otherwise. ' +
    'The server validates this and returns a warning if the last lines of new_content duplicate the lines immediately following the replaced range. ' +
    'To INSERT lines: replace_code_lines(file, N, N, "original_line\nnew_line") replaces line N with itself plus new lines. ' +
    'To DELETE lines: use the delete_lines tool instead.');
  var SchemaReplace := TJSONObject.Create;
  SchemaReplace.AddPair('type', 'object');
  var PropReplace := TJSONObject.Create;
  PropReplace.AddPair('file', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Absolute path to the file to modify'));
  PropReplace.AddPair('start_line', TJSONObject.Create.AddPair('type', 'integer').AddPair('description', 'First line to replace (1-based, from get_linter_results)'));
  PropReplace.AddPair('end_line', TJSONObject.Create.AddPair('type', 'integer').AddPair('description', 'Last line to replace (1-based, inclusive)'));
  PropReplace.AddPair('new_content', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Replacement text (can be multiline with \n). Line endings are normalized to CRLF automatically.'));
  SchemaReplace.AddPair('properties', PropReplace);
  var ReqReplace := TJSONArray.Create;
  ReqReplace.Add('file');
  ReqReplace.Add('start_line');
  ReqReplace.Add('end_line');
  ReqReplace.Add('new_content');
  SchemaReplace.AddPair('required', ReqReplace);
  ToolReplace.AddPair('inputSchema', SchemaReplace);
  ToolsArr.Add(ToolReplace);

  // delete_lines
  var ToolDelete := TJSONObject.Create;
  ToolDelete.AddPair('name', 'delete_lines');
  ToolDelete.AddPair('description',
    'Deletes one or more lines from a file. Use this to remove blank lines, unused declarations, or other unwanted lines. ' +
    'Line numbers refer to the original positions from the last get_linter_results call; ' +
    'the server automatically adjusts for any shifts caused by previous operations. ' +
    'To delete a single line: delete_lines(file, N, N). To delete a range: delete_lines(file, N, M).');
  var SchemaDelete := TJSONObject.Create;
  SchemaDelete.AddPair('type', 'object');
  var PropDelete := TJSONObject.Create;
  PropDelete.AddPair('file', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Absolute path to the file to modify'));
  PropDelete.AddPair('start_line', TJSONObject.Create.AddPair('type', 'integer').AddPair('description', 'First line to delete (1-based, from get_linter_results)'));
  PropDelete.AddPair('end_line', TJSONObject.Create.AddPair('type', 'integer').AddPair('description', 'Last line to delete (1-based, inclusive)'));
  SchemaDelete.AddPair('properties', PropDelete);
  var ReqDelete := TJSONArray.Create;
  ReqDelete.Add('file');
  ReqDelete.Add('start_line');
  ReqDelete.Add('end_line');
  SchemaDelete.AddPair('required', ReqDelete);
  ToolDelete.AddPair('inputSchema', SchemaDelete);
  ToolsArr.Add(ToolDelete);

  Result := TJSONObject.Create;
  Result.AddPair('tools', ToolsArr);
end;

function TMcpLinterServer.HandleDeleteLines(AParams: TJSONObject): TJSONObject;
var
  ActualEnd  : Integer;
  ActualStart: Integer;
  DeleteCount: Integer;
  EndLine    : Integer;
  FilePath   : String;
  Lines      : TStringList;
  StartLine  : Integer;
  Tracker    : TLineOffsetTracker;
begin
  FilePath := AParams.GetValue('file').Value;
  StartLine := (AParams.GetValue('start_line') as TJSONNumber).AsInt;
  EndLine := (AParams.GetValue('end_line') as TJSONNumber).AsInt;

  if not FileExists(FilePath) then
    Exit(MakeErrorResult('File not found: ' + FilePath));

  Tracker := GetOrCreateTracker(FilePath);
  ActualStart := Tracker.TranslateLine(StartLine);
  ActualEnd := Tracker.TranslateLine(EndLine);

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FilePath, TEncoding.UTF8);

    if (ActualStart < 1) or (ActualEnd > Lines.Count) or (ActualStart > ActualEnd) then
      Exit(MakeErrorResult(Format('Line range %d-%d (actual %d-%d) is out of bounds (file has %d lines)',
        [StartLine, EndLine, ActualStart, ActualEnd, Lines.Count])));

    DeleteCount := ActualEnd - ActualStart + 1;

    for var I: Integer := ActualEnd downto ActualStart do
      Lines.Delete(I - 1);

    var Encoding: TEncoding;
    var Preamble: TBytes;
    var RawBytes: TBytes := TFile.ReadAllBytes(FilePath);
    Preamble := TEncoding.UTF8.GetPreamble;
    if (Length(RawBytes) >= Length(Preamble)) and
       (Length(Preamble) > 0) and
       CompareMem(@RawBytes[0], @Preamble[0], Length(Preamble)) then
      Encoding := TEncoding.UTF8
    else
      Encoding := TUTF8Encoding.Create(False);

    try
      Lines.WriteBOM := Encoding = TEncoding.UTF8;
      Lines.SaveToFile(FilePath, Encoding);
    finally
      if Encoding <> TEncoding.UTF8 then
        Encoding.Free;
    end;

    Tracker.RecordReplacement(StartLine, EndLine, 0);

    Result := MakeTextResult(Format('Deleted %d line(s) %d-%d. File now has %d lines.',
      [DeleteCount, StartLine, EndLine, Lines.Count]));
  finally
    Lines.Free;
  end;
end;

function TMcpLinterServer.HandleGetLinterResults(AParams: TJSONObject): TJSONObject;
var
  FilePath    : String;
  FitNesseDir : String;
  Ini         : TIniFile;
  IniPath     : String;
  LintTask    : TDptSilentLintTask;
  StyleFile   : String;
  TotalLines  : Integer;
  Tracker     : TLineOffsetTracker;
  V           : TStyleViolation;
  ViolationArr: TJSONArray;
  ViolationObj: TJSONObject;
begin
  FilePath := AParams.GetValue('file').Value;
  StyleFile := AParams.GetValue('style_file').Value;

  if not FileExists(FilePath) then
    Exit(MakeErrorResult('File not found: ' + FilePath));
  if not FileExists(StyleFile) then
    Exit(MakeErrorResult('Style file not found: ' + StyleFile));

  // Read FitNesse dir from config
  FitNesseDir := '';
  IniPath := TPath.Combine(ExtractFilePath(ParamStr(0)), DptConfigFileName);
  if not TFile.Exists(IniPath) then
    IniPath := FileSearch(DptConfigFileName, GetEnvironmentVariable('PATH'));
  if IniPath <> '' then
  begin
    Ini := TIniFile.Create(IniPath);
    try
      FitNesseDir := Ini.ReadString('FitNesse', 'Dir', '');
    finally
      Ini.Free;
    end;
  end;
  if FitNesseDir = '' then
    Exit(MakeErrorResult('FitNesse directory not configured. Create a ' + DptConfigFileName +
      ' with [FitNesse] Dir=C:\Path\To\FitNesse'));

  Tracker := GetOrCreateTracker(FilePath);
  Tracker.Reset;

  LintTask := TDptSilentLintTask.Create;
  try
    LintTask.StyleFile := StyleFile;
    LintTask.TargetFiles.Add(FilePath);
    LintTask.FitNesseDir := FitNesseDir;
    LintTask.FitNesseRoot := TPath.Combine(FitNesseDir, 'FitNesseRoot');

    try
      LintTask.Execute;
    except
      on E: Exception do
        Exit(MakeErrorResult('Linting failed: ' + E.Message));
    end;
  finally
    LintTask.Free;
  end;

  TotalLines := GetFileLineCount(FilePath);

  ViolationArr := TJSONArray.Create;
  try
    for var I: Integer := 0 to TDptLintContext.Violations.Count - 1 do
    begin
      V := TDptLintContext.Violations[I];
      if SameText(V.FileSpec, FilePath) or SameText(V.FileSpec, ExtractFileName(FilePath)) then
      begin
        ViolationObj := TJSONObject.Create;
        ViolationObj.AddPair('line', TJSONNumber.Create(V.Line));
        ViolationObj.AddPair('message', V.Message);
        ViolationArr.Add(ViolationObj);
      end;
    end;

    var ResultObj := TJSONObject.Create;
    ResultObj.AddPair('violations', ViolationArr);
    ResultObj.AddPair('total_lines', TJSONNumber.Create(TotalLines));
    ResultObj.AddPair('violation_count', TJSONNumber.Create(ViolationArr.Count));
    Result := MakeTextResult(ResultObj.ToJSON);
    ResultObj.Free;
  except
    ViolationArr.Free;
    raise;
  end;
end;

function TMcpLinterServer.HandleReadCodeLines(AParams: TJSONObject): TJSONObject;
var
  ActualEnd  : Integer;
  ActualStart: Integer;
  EndLine    : Integer;
  FilePath   : String;
  Lines      : TStringList;
  Output     : TStringBuilder;
  StartLine  : Integer;
  TotalLines : Integer;
  Tracker    : TLineOffsetTracker;
begin
  FilePath := AParams.GetValue('file').Value;
  StartLine := (AParams.GetValue('start_line') as TJSONNumber).AsInt;
  EndLine := (AParams.GetValue('end_line') as TJSONNumber).AsInt;

  if not FileExists(FilePath) then
    Exit(MakeErrorResult('File not found: ' + FilePath));

  Tracker := GetOrCreateTracker(FilePath);
  ActualStart := Tracker.TranslateLine(StartLine);
  ActualEnd := Tracker.TranslateLine(EndLine);

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FilePath, TEncoding.UTF8);
    TotalLines := Lines.Count;

    if ActualStart < 1 then ActualStart := 1;
    if ActualEnd > TotalLines then ActualEnd := TotalLines;

    Output := TStringBuilder.Create;
    try
      for var I: Integer := ActualStart to ActualEnd do
        Output.AppendLine(Format('%4d| %s', [I, Lines[I - 1]]));

      var ResultObj := TJSONObject.Create;
      try
        ResultObj.AddPair('lines', Output.ToString.TrimRight);
        ResultObj.AddPair('total_lines', TJSONNumber.Create(TotalLines));
        Result := MakeTextResult(ResultObj.ToJSON);
      finally
        ResultObj.Free;
      end;
    finally
      Output.Free;
    end;
  finally
    Lines.Free;
  end;
end;

function TMcpLinterServer.HandleReplaceCodeLines(AParams: TJSONObject): TJSONObject;
var
  ActualEnd   : Integer;
  ActualStart : Integer;
  EndLine     : Integer;
  FilePath    : String;
  Lines       : TStringList;
  NewContent  : String;
  NewLines    : TArray<String>;
  NewLineCount: Integer;
  StartLine   : Integer;
  Tracker     : TLineOffsetTracker;
  Warning     : String;
begin
  FilePath := AParams.GetValue('file').Value;
  StartLine := (AParams.GetValue('start_line') as TJSONNumber).AsInt;
  EndLine := (AParams.GetValue('end_line') as TJSONNumber).AsInt;
  NewContent := AParams.GetValue('new_content').Value;

  if not FileExists(FilePath) then
    Exit(MakeErrorResult('File not found: ' + FilePath));

  Tracker := GetOrCreateTracker(FilePath);
  ActualStart := Tracker.TranslateLine(StartLine);
  ActualEnd := Tracker.TranslateLine(EndLine);

  Warning := '';
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FilePath, TEncoding.UTF8);

    if (ActualStart < 1) or (ActualEnd > Lines.Count) or (ActualStart > ActualEnd) then
      Exit(MakeErrorResult(Format('Line range %d-%d (actual %d-%d) is out of bounds (file has %d lines)',
        [StartLine, EndLine, ActualStart, ActualEnd, Lines.Count])));

    NewContent := NormalizeLineEndings(NewContent);

    if NewContent = '' then
    begin
      NewLines := [];
      NewLineCount := 0;
    end
    else
    begin
      NewLines := NewContent.Split([#13#10]);
      NewLineCount := Length(NewLines);
    end;

    // Detect trailing overlap: check if the last K lines of new_content duplicate
    // the first K lines immediately after the replaced range in the file.
    // The agent sometimes accidentally includes context lines beyond end_line.
    if (NewLineCount > 0) and (ActualEnd < Lines.Count) then
    begin
      var OverlapCount: Integer := 0;
      var MaxCheck: Integer := NewLineCount;
      var AvailableAfter: Integer := Lines.Count - ActualEnd;
      if MaxCheck > AvailableAfter then
        MaxCheck := AvailableAfter;
      if MaxCheck > 10 then
        MaxCheck := 10;

      for var K: Integer := 1 to MaxCheck do
      begin
        var AllMatch: Boolean := True;
        for var J: Integer := 0 to K - 1 do
        begin
          if Trim(NewLines[NewLineCount - K + J]) <> Trim(Lines[ActualEnd + J]) then
          begin
            AllMatch := False;
            Break;
          end;
        end;
        if AllMatch then
          OverlapCount := K;
      end;

      if OverlapCount > 0 then
        Warning := Format(' WARNING: The last %d line(s) of new_content appear to duplicate ' +
          'the lines immediately after the replaced range (lines %d-%d). ' +
          'This likely means new_content contains lines that should not be part of the replacement. ' +
          'Please verify the result with read_code_lines and use replace_code_lines or delete_lines to fix if needed.',
          [OverlapCount, ActualEnd + 1, ActualEnd + OverlapCount]);
    end;

    for var I: Integer := ActualEnd downto ActualStart do
      Lines.Delete(I - 1);

    for var I: Integer := 0 to NewLineCount - 1 do
      Lines.Insert(ActualStart - 1 + I, NewLines[I]);

    var Encoding: TEncoding;
    var Preamble: TBytes;
    var RawBytes: TBytes := TFile.ReadAllBytes(FilePath);
    Preamble := TEncoding.UTF8.GetPreamble;
    if (Length(RawBytes) >= Length(Preamble)) and
       (Length(Preamble) > 0) and
       CompareMem(@RawBytes[0], @Preamble[0], Length(Preamble)) then
      Encoding := TEncoding.UTF8
    else
      Encoding := TUTF8Encoding.Create(False);

    try
      Lines.WriteBOM := Encoding = TEncoding.UTF8;
      Lines.SaveToFile(FilePath, Encoding);
    finally
      if Encoding <> TEncoding.UTF8 then
        Encoding.Free;
    end;

    Tracker.RecordReplacement(StartLine, EndLine, NewLineCount);

    var Delta: Integer := NewLineCount - (EndLine - StartLine + 1);
    Result := MakeTextResult(Format('Replaced lines %d-%d with %d line(s). Delta: %d. File now has %d lines.%s',
      [StartLine, EndLine, NewLineCount, Delta, Lines.Count, Warning]));
  finally
    Lines.Free;
  end;
end;

procedure TMcpLinterServer.RunOnce;
var
  Line: String;
begin
  if Assigned(FInputReader) then
    Line := FInputReader.ReadLine
  else
    System.Readln(System.Input, Line);

  if Line <> '' then
    ProcessMessage(Line);
end;

procedure TMcpLinterServer.Run;
begin
  while not FExitRequest do
  begin
    if Assigned(FInputReader) then
    begin
      if FInputReader.Peek = -1 then
        Break;
    end
    else if System.EOF(System.Input) then
      Break;

    RunOnce;
  end;
end;

end.
