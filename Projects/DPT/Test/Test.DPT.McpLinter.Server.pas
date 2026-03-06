unit Test.DPT.McpLinter.Server;

interface

uses

  DUnitX.TestFramework,

  System.Classes,
  System.JSON,
  System.SyncObjs,

  DPT.McpLinter.Server;

type

  [TestFixture]
  TLineOffsetTrackerTests = class
  public
    [Test]
    procedure TestTranslateLineNoAdjustments;
    [Test]
    procedure TestTranslateLineAfterInsert;
    [Test]
    procedure TestTranslateLineAfterDelete;
    [Test]
    procedure TestTranslateLineMultipleReplacements;
    [Test]
    procedure TestResetClearsAdjustments;
  end;

  [TestFixture]
  TMcpLinterServerTests = class
  public
    [Test]
    procedure TestMcpLinterInitialize;
    [Test]
    procedure TestMcpLinterToolsList;
    [Test]
    procedure TestMcpLinterUnknownTool;
    [Test]
    procedure TestReadCodeLinesBasic;
    [Test]
    procedure TestReadCodeLinesFileNotFound;
    [Test]
    procedure TestReadCodeLinesOutOfRange;
    [Test]
    procedure TestReadCodeLinesWithOffset;
    [Test]
    procedure TestReplaceCodeLinesBasic;
    [Test]
    procedure TestReplaceCodeLinesInsertMore;
    [Test]
    procedure TestReplaceCodeLinesDeleteLines;
    [Test]
    procedure TestReplaceCodeLinesNormalizesLineEndings;
    [Test]
    procedure TestReplaceCodeLinesMixedLineEndings;
    [Test]
    procedure TestReplaceCodeLinesFileNotFound;
    [Test]
    procedure TestReplaceCodeLinesSequentialWithOffset;
    [Test]
    procedure TestReplaceCodeLinesOverlapWarning;
    [Test]
    procedure TestReplaceCodeLinesMultiLineOverlapWarning;
    [Test]
    procedure TestReplaceCodeLinesNoOverlapWarning;
    [Test]
    procedure TestDeleteLinesBasic;
    [Test]
    procedure TestDeleteLinesMultiple;
    [Test]
    procedure TestDeleteLinesWithOffset;
    [Test]
    procedure TestDeleteLinesFileNotFound;
    [Test]
    procedure TestGetLinterResultsFileNotFound;
    [Test]
    procedure TestMcpLinterFullWorkflow;
  end;

implementation

uses

  Winapi.Windows,

  System.IOUtils,
  System.SysUtils;

type

  TStringTextReader = class(TTextReader)
  public
    FLines: TStringList;
    FIndex: Integer;
    constructor Create(const AText: string);
    destructor Destroy; override;
    function ReadLine: string; override;
    function Peek: Integer; override;
    function GetEndOfStream: Boolean; override;
    procedure Close; override;
    function Read: Integer; override;
    function ReadBlock(var Buffer: TArray<Char>; Index, Count: Integer): Integer; override;
    function ReadToEnd: string; override;
    procedure Rewind; override;
  end;

  TStringTextWriter = class(TTextWriter)
  private
    FOutput: TStringList;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(const S: string); override;
    procedure WriteLine(const S: string); override;
    procedure Close; override;
    procedure Flush; override;
    function GetCount: Integer;
    function GetLine(AIndex: Integer): string;
    property Output: TStringList read FOutput;
  end;

{ TStringTextReader }

constructor TStringTextReader.Create(const AText: string);
begin
  inherited Create;
  FLines := TStringList.Create;
  FLines.Text := AText;
  FIndex := 0;
end;

destructor TStringTextReader.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TStringTextReader.Close;
begin
end;

function TStringTextReader.GetEndOfStream: Boolean;
begin
  Result := FIndex >= FLines.Count;
end;

function TStringTextReader.Peek: Integer;
begin
  if FIndex < FLines.Count then Result := 1 else Result := -1;
end;

function TStringTextReader.Read: Integer;
begin
  Result := -1;
end;

function TStringTextReader.ReadBlock(var Buffer: TArray<Char>; Index, Count: Integer): Integer;
begin
  Result := 0;
end;

function TStringTextReader.ReadLine: string;
begin
  if FIndex < FLines.Count then
  begin
    Result := FLines[FIndex];
    Inc(FIndex);
  end
  else
    Result := '';
end;

function TStringTextReader.ReadToEnd: string;
begin
  Result := '';
end;

procedure TStringTextReader.Rewind;
begin
  FIndex := 0;
end;

{ TStringTextWriter }

constructor TStringTextWriter.Create;
begin
  inherited Create;
  FOutput := TStringList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TStringTextWriter.Destroy;
begin
  FLock.Free;
  FOutput.Free;
  inherited;
end;

procedure TStringTextWriter.Close;
begin
end;

procedure TStringTextWriter.Flush;
begin
end;

procedure TStringTextWriter.Write(const S: string);
begin
end;

procedure TStringTextWriter.WriteLine(const S: string);
begin
  FLock.Enter;
  try
    FOutput.Add(S);
  finally
    FLock.Leave;
  end;
end;

function TStringTextWriter.GetCount: Integer;
begin
  FLock.Enter;
  try
    Result := FOutput.Count;
  finally
    FLock.Leave;
  end;
end;

function TStringTextWriter.GetLine(AIndex: Integer): string;
begin
  FLock.Enter;
  try
    Result := FOutput[AIndex];
  finally
    FLock.Leave;
  end;
end;

{ Helper: Create a temp file with known content }

function CreateTempFileWithContent(const AContent: String): String;
var
  Encoding: TEncoding;
begin
  Result := TPath.Combine(TPath.GetTempPath, 'dpt_test_' + IntToStr(GetTickCount) + '.pas');
  Encoding := TEncoding.UTF8;
  TFile.WriteAllText(Result, AContent, Encoding);
end;

function CreateTempFileWithLines(const ALines: array of String): String;
var
  SL: TStringList;
begin
  Result := TPath.Combine(TPath.GetTempPath, 'dpt_test_' + IntToStr(GetTickCount) + '.pas');
  SL := TStringList.Create;
  try
    for var S in ALines do
      SL.Add(S);
    SL.WriteBOM := True;
    SL.SaveToFile(Result, TEncoding.UTF8);
  finally
    SL.Free;
  end;
end;

function EscapePath(const APath: String): String;
begin
  Result := StringReplace(APath, '\', '\\', [rfReplaceAll]);
end;

{ TLineOffsetTrackerTests }

procedure TLineOffsetTrackerTests.TestTranslateLineNoAdjustments;
var
  Tracker: TLineOffsetTracker;
begin
  Tracker := TLineOffsetTracker.Create;
  try
    Assert.AreEqual(50, Tracker.TranslateLine(50));
    Assert.AreEqual(1, Tracker.TranslateLine(1));
    Assert.AreEqual(1000, Tracker.TranslateLine(1000));
  finally
    Tracker.Free;
  end;
end;

procedure TLineOffsetTrackerTests.TestTranslateLineAfterInsert;
var
  Tracker: TLineOffsetTracker;
begin
  Tracker := TLineOffsetTracker.Create;
  try
    // Replace 3 lines (10-12) with 5 lines -> Delta = +2
    Tracker.RecordReplacement(10, 12, 5);

    // Lines before the change: unaffected
    Assert.AreEqual(5, Tracker.TranslateLine(5));
    Assert.AreEqual(9, Tracker.TranslateLine(9));

    // Line at the change point: shifted
    Assert.AreEqual(12, Tracker.TranslateLine(10));

    // Lines after the change: shifted by +2
    Assert.AreEqual(22, Tracker.TranslateLine(20));
    Assert.AreEqual(102, Tracker.TranslateLine(100));
  finally
    Tracker.Free;
  end;
end;

procedure TLineOffsetTrackerTests.TestTranslateLineAfterDelete;
var
  Tracker: TLineOffsetTracker;
begin
  Tracker := TLineOffsetTracker.Create;
  try
    // Replace 5 lines (20-24) with 2 lines -> Delta = -3
    Tracker.RecordReplacement(20, 24, 2);

    Assert.AreEqual(15, Tracker.TranslateLine(15));
    Assert.AreEqual(19, Tracker.TranslateLine(19));

    // Line at the change: shifted
    Assert.AreEqual(17, Tracker.TranslateLine(20));

    // Lines after: shifted by -3
    Assert.AreEqual(47, Tracker.TranslateLine(50));
  finally
    Tracker.Free;
  end;
end;

procedure TLineOffsetTrackerTests.TestTranslateLineMultipleReplacements;
var
  Tracker: TLineOffsetTracker;
begin
  Tracker := TLineOffsetTracker.Create;
  try
    // First: replace lines 10-12 (3 lines) with 5 lines -> Delta = +2
    Tracker.RecordReplacement(10, 12, 5);
    // Second: replace lines 50-51 (2 lines) with 1 line -> Delta = -1
    Tracker.RecordReplacement(50, 51, 1);

    // Line 5: before both changes -> no shift
    Assert.AreEqual(5, Tracker.TranslateLine(5));

    // Line 30: after first change (+2), before second -> +2
    Assert.AreEqual(32, Tracker.TranslateLine(30));

    // Line 100: after both changes (+2 and -1) -> +1
    Assert.AreEqual(101, Tracker.TranslateLine(100));
  finally
    Tracker.Free;
  end;
end;

procedure TLineOffsetTrackerTests.TestResetClearsAdjustments;
var
  Tracker: TLineOffsetTracker;
begin
  Tracker := TLineOffsetTracker.Create;
  try
    Tracker.RecordReplacement(10, 12, 5);
    Assert.AreEqual(102, Tracker.TranslateLine(100));

    Tracker.Reset;
    Assert.AreEqual(100, Tracker.TranslateLine(100));
  finally
    Tracker.Free;
  end;
end;

{ TMcpLinterServerTests }

procedure TMcpLinterServerTests.TestMcpLinterInitialize;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  LJSON       : TJSONObject;
begin
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpLinterServer.Create(InputReader, OutputWriter);
  try
    Server.RunOnce;
    Assert.AreEqual(1, OutputWriter.GetCount, 'Expected one response');

    LJSON := TJSONObject.ParseJSONValue(OutputWriter.GetLine(0)) as TJSONObject;
    try
      var ResultObj := LJSON.GetValue('result') as TJSONObject;
      Assert.IsNotNull(ResultObj, 'Missing result');
      Assert.AreEqual('2024-11-05', ResultObj.GetValue('protocolVersion').Value);
      var ServerInfo := ResultObj.GetValue('serverInfo') as TJSONObject;
      Assert.AreEqual('DPT-Linter', ServerInfo.GetValue('name').Value);
    finally
      LJSON.Free;
    end;
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpLinterServerTests.TestMcpLinterToolsList;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  LJSON       : TJSONObject;
begin
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpLinterServer.Create(InputReader, OutputWriter);
  try
    Server.RunOnce; // initialize
    Server.RunOnce; // tools/list

    Assert.AreEqual(2, OutputWriter.GetCount);

    LJSON := TJSONObject.ParseJSONValue(OutputWriter.GetLine(1)) as TJSONObject;
    try
      var ResultObj := LJSON.GetValue('result') as TJSONObject;
      var ToolsArr := ResultObj.GetValue('tools') as TJSONArray;
      Assert.AreEqual(4, ToolsArr.Count, 'Expected 4 tools');

      var ToolNames: String := '';
      for var I := 0 to ToolsArr.Count - 1 do
        ToolNames := ToolNames + (ToolsArr[I] as TJSONObject).GetValue('name').Value + ',';

      Assert.IsTrue(ToolNames.Contains('get_linter_results'), 'Missing get_linter_results');
      Assert.IsTrue(ToolNames.Contains('read_code_lines'), 'Missing read_code_lines');
      Assert.IsTrue(ToolNames.Contains('replace_code_lines'), 'Missing replace_code_lines');
      Assert.IsTrue(ToolNames.Contains('delete_lines'), 'Missing delete_lines');
    finally
      LJSON.Free;
    end;
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpLinterServerTests.TestMcpLinterUnknownTool;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
begin
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "nonexistent_tool", "arguments": {}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpLinterServer.Create(InputReader, OutputWriter);
  try
    Server.RunOnce;
    Server.RunOnce;

    Assert.AreEqual(2, OutputWriter.GetCount);
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('Tool not found'),
      'Expected tool not found error: ' + OutputWriter.GetLine(1));
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpLinterServerTests.TestReadCodeLinesBasic;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
begin
  TempFile := CreateTempFileWithLines(['Line1', 'Line2', 'Line3', 'Line4', 'Line5']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "read_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 4}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // read_code_lines

      Assert.AreEqual(2, OutputWriter.GetCount);
      var Response := OutputWriter.GetLine(1);
      Assert.IsTrue(Response.Contains('Line2'), 'Should contain Line2: ' + Response);
      Assert.IsTrue(Response.Contains('Line3'), 'Should contain Line3: ' + Response);
      Assert.IsTrue(Response.Contains('Line4'), 'Should contain Line4: ' + Response);
      Assert.IsFalse(Response.Contains('Line1'), 'Should not contain Line1');
      Assert.IsFalse(Response.Contains('Line5'), 'Should not contain Line5');
      Assert.IsTrue(Response.Contains('total_lines'), 'Should contain total_lines');
      Assert.IsTrue(Response.Contains('5'), 'total_lines should be 5');
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReadCodeLinesFileNotFound;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
begin
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "read_code_lines", "arguments": {"file": "C:\\\\nonexistent\\\\file.pas", "start_line": 1, "end_line": 5}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpLinterServer.Create(InputReader, OutputWriter);
  try
    Server.RunOnce;
    Server.RunOnce;

    Assert.AreEqual(2, OutputWriter.GetCount);
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('File not found'),
      'Expected file not found error: ' + OutputWriter.GetLine(1));
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpLinterServerTests.TestReadCodeLinesOutOfRange;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
begin
  TempFile := CreateTempFileWithLines(['Line1', 'Line2', 'Line3']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "read_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 100}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      Assert.AreEqual(2, OutputWriter.GetCount);
      var Response := OutputWriter.GetLine(1);
      Assert.IsTrue(Response.Contains('Line2'), 'Should contain Line2');
      Assert.IsTrue(Response.Contains('Line3'), 'Should contain Line3');
      Assert.IsFalse(Response.Contains('isError'), 'Should not be an error');
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesBasic;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  ResultLines : TStringList;
begin
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC', 'DDD', 'EEE']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 3, "end_line": 4, "new_content": "XXX\nYYY"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      Assert.AreEqual(2, OutputWriter.GetCount);
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('Replaced'), 'Expected replacement confirmation');

      ResultLines := TStringList.Create;
      try
        ResultLines.LoadFromFile(TempFile, TEncoding.UTF8);
        Assert.AreEqual(5, ResultLines.Count);
        Assert.AreEqual('AAA', ResultLines[0]);
        Assert.AreEqual('BBB', ResultLines[1]);
        Assert.AreEqual('XXX', ResultLines[2]);
        Assert.AreEqual('YYY', ResultLines[3]);
        Assert.AreEqual('EEE', ResultLines[4]);
      finally
        ResultLines.Free;
      end;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesInsertMore;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  ResultLines : TStringList;
begin
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC', 'DDD']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 3, "new_content": "X1\nX2\nX3\nX4\nX5"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      Assert.AreEqual(2, OutputWriter.GetCount);
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('Delta: 3'), 'Expected Delta 3: ' + OutputWriter.GetLine(1));

      ResultLines := TStringList.Create;
      try
        ResultLines.LoadFromFile(TempFile, TEncoding.UTF8);
        Assert.AreEqual(7, ResultLines.Count);
        Assert.AreEqual('AAA', ResultLines[0]);
        Assert.AreEqual('X1', ResultLines[1]);
        Assert.AreEqual('X5', ResultLines[5]);
        Assert.AreEqual('DDD', ResultLines[6]);
      finally
        ResultLines.Free;
      end;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesDeleteLines;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  ResultLines : TStringList;
begin
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 5, "new_content": "ONLY"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      Assert.AreEqual(2, OutputWriter.GetCount);
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('Delta: -3'), 'Expected Delta -3: ' + OutputWriter.GetLine(1));

      ResultLines := TStringList.Create;
      try
        ResultLines.LoadFromFile(TempFile, TEncoding.UTF8);
        Assert.AreEqual(3, ResultLines.Count);
        Assert.AreEqual('AAA', ResultLines[0]);
        Assert.AreEqual('ONLY', ResultLines[1]);
        Assert.AreEqual('FFF', ResultLines[2]);
      finally
        ResultLines.Free;
      end;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesNormalizesLineEndings;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  RawContent  : TBytes;
  RawStr      : String;
begin
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC']);
  try
    // Send new_content with bare \n (Unix-style)
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 2, "new_content": "X1\nX2"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      // Read raw bytes and check for CRLF
      RawContent := TFile.ReadAllBytes(TempFile);
      RawStr := TEncoding.UTF8.GetString(RawContent);
      Assert.IsTrue(RawStr.Contains(#13#10), 'File should contain CRLF line endings');
      // Ensure no bare LF (without preceding CR)
      var NoCRLF := RawStr.Replace(#13#10, '');
      Assert.IsFalse(NoCRLF.Contains(#10), 'File should not contain bare LF');
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesMixedLineEndings;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  RawContent  : TBytes;
  RawStr      : String;
begin
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC']);
  try
    // Send new_content with mixed line endings: \r\n and \n
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 2, "new_content": "X1\r\nX2\nX3"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      RawContent := TFile.ReadAllBytes(TempFile);
      RawStr := TEncoding.UTF8.GetString(RawContent);
      var NoCRLF := RawStr.Replace(#13#10, '');
      Assert.IsFalse(NoCRLF.Contains(#10), 'File should not contain bare LF after mixed input');
      Assert.IsFalse(NoCRLF.Contains(#13), 'File should not contain bare CR');
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesFileNotFound;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
begin
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "C:\\\\nonexistent\\\\file.pas", "start_line": 1, "end_line": 1, "new_content": "test"}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpLinterServer.Create(InputReader, OutputWriter);
  try
    Server.RunOnce;
    Server.RunOnce;

    Assert.AreEqual(2, OutputWriter.GetCount);
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('File not found'),
      'Expected file not found: ' + OutputWriter.GetLine(1));
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesSequentialWithOffset;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  ResultLines : TStringList;
begin
  TempFile := CreateTempFileWithLines([
    'Line01', 'Line02', 'Line03', 'Line04', 'Line05',
    'Line06', 'Line07', 'Line08', 'Line09', 'Line10']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      // First: replace lines 3-4 (2 lines) with 4 lines -> Delta = +2
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 3, "end_line": 4, "new_content": "New3a\nNew3b\nNew3c\nNew3d"}}}' + sLineBreak +
      // Second: replace lines 7-8 using ORIGINAL line numbers (server adjusts to actual 9-10 due to +2 offset)
      '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 7, "end_line": 8, "new_content": "New7"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // first replace
      Server.RunOnce; // second replace

      Assert.AreEqual(3, OutputWriter.GetCount);
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('Delta: 2'), 'First replace Delta should be 2');
      Assert.IsTrue(OutputWriter.GetLine(2).Contains('Delta: -1'), 'Second replace Delta should be -1');

      ResultLines := TStringList.Create;
      try
        ResultLines.LoadFromFile(TempFile, TEncoding.UTF8);
        Assert.AreEqual(11, ResultLines.Count, 'File should have 11 lines (10 + 2 - 1)');
        Assert.AreEqual('Line01', ResultLines[0]);
        Assert.AreEqual('Line02', ResultLines[1]);
        Assert.AreEqual('New3a', ResultLines[2]);
        Assert.AreEqual('New3d', ResultLines[5]);
        Assert.AreEqual('Line05', ResultLines[6]);
        Assert.AreEqual('Line06', ResultLines[7]);
        Assert.AreEqual('New7', ResultLines[8]);
        Assert.AreEqual('Line09', ResultLines[9]);
        Assert.AreEqual('Line10', ResultLines[10]);
      finally
        ResultLines.Free;
      end;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReadCodeLinesWithOffset;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
begin
  TempFile := CreateTempFileWithLines([
    'Line01', 'Line02', 'Line03', 'Line04', 'Line05',
    'Line06', 'Line07', 'Line08', 'Line09', 'Line10']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      // Replace lines 3-4 with 6 lines -> Delta = +4
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 3, "end_line": 4, "new_content": "N3a\nN3b\nN3c\nN3d\nN3e\nN3f"}}}' + sLineBreak +
      // Read original lines 7-8 (should now be actual lines 11-12 due to +4 offset)
      '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "read_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 7, "end_line": 8}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // replace
      Server.RunOnce; // read

      Assert.AreEqual(3, OutputWriter.GetCount);
      var ReadResponse := OutputWriter.GetLine(2);
      Assert.IsTrue(ReadResponse.Contains('Line07'), 'Should read Line07 (offset applied): ' + ReadResponse);
      Assert.IsTrue(ReadResponse.Contains('Line08'), 'Should read Line08 (offset applied): ' + ReadResponse);
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesOverlapWarning;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
begin
  // File: AAA, BBB, CCC, DDD, EEE
  // Agent replaces lines 2-3 with "XXX\nYYY\nDDD" -- accidentally including line 4 "DDD"
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC', 'DDD', 'EEE']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 3, "new_content": "XXX\nYYY\nDDD"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      Assert.AreEqual(2, OutputWriter.GetCount);
      var Response := OutputWriter.GetLine(1);
      Assert.IsTrue(Response.Contains('WARNING'), 'Expected overlap WARNING: ' + Response);
      Assert.IsTrue(Response.Contains('last 1 line'), 'Expected 1 line overlap: ' + Response);
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesMultiLineOverlapWarning;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
begin
  // File: AAA, BBB, CCC, DDD, EEE, FFF
  // Agent replaces lines 2-3 with "XXX\nDDD\nEEE" -- accidentally including lines 4-5
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 3, "new_content": "XXX\nDDD\nEEE"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      Assert.AreEqual(2, OutputWriter.GetCount);
      var Response := OutputWriter.GetLine(1);
      Assert.IsTrue(Response.Contains('WARNING'), 'Expected overlap WARNING: ' + Response);
      Assert.IsTrue(Response.Contains('last 2 line'), 'Expected 2 line overlap: ' + Response);
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestReplaceCodeLinesNoOverlapWarning;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
begin
  // File: AAA, BBB, CCC, DDD, EEE
  // Agent replaces lines 2-3 with "XXX\nYYY" -- no overlap with line 4 "DDD"
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC', 'DDD', 'EEE']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 3, "new_content": "XXX\nYYY"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      Assert.AreEqual(2, OutputWriter.GetCount);
      var Response := OutputWriter.GetLine(1);
      Assert.IsFalse(Response.Contains('WARNING'), 'Should NOT contain WARNING: ' + Response);
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestDeleteLinesBasic;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  ResultLines : TStringList;
begin
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC', 'DDD', 'EEE']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "delete_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 3, "end_line": 3}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      Assert.AreEqual(2, OutputWriter.GetCount);
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('Deleted 1 line'), 'Expected single line deletion: ' + OutputWriter.GetLine(1));

      ResultLines := TStringList.Create;
      try
        ResultLines.LoadFromFile(TempFile, TEncoding.UTF8);
        Assert.AreEqual(4, ResultLines.Count);
        Assert.AreEqual('AAA', ResultLines[0]);
        Assert.AreEqual('BBB', ResultLines[1]);
        Assert.AreEqual('DDD', ResultLines[2]);
        Assert.AreEqual('EEE', ResultLines[3]);
      finally
        ResultLines.Free;
      end;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestDeleteLinesMultiple;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  ResultLines : TStringList;
begin
  TempFile := CreateTempFileWithLines(['AAA', 'BBB', 'CCC', 'DDD', 'EEE']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "delete_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 2, "end_line": 4}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;

      Assert.AreEqual(2, OutputWriter.GetCount);
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('Deleted 3 line'), 'Expected 3 lines deleted: ' + OutputWriter.GetLine(1));

      ResultLines := TStringList.Create;
      try
        ResultLines.LoadFromFile(TempFile, TEncoding.UTF8);
        Assert.AreEqual(2, ResultLines.Count);
        Assert.AreEqual('AAA', ResultLines[0]);
        Assert.AreEqual('EEE', ResultLines[1]);
      finally
        ResultLines.Free;
      end;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestDeleteLinesWithOffset;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  ResultLines : TStringList;
begin
  TempFile := CreateTempFileWithLines([
    'Line01', 'Line02', 'Line03', 'Line04', 'Line05',
    'Line06', 'Line07', 'Line08', 'Line09', 'Line10']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      // Replace lines 3-4 (2 lines) with 4 lines -> Delta = +2
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 3, "end_line": 4, "new_content": "New3a\nNew3b\nNew3c\nNew3d"}}}' + sLineBreak +
      // Delete original line 7 (should be actual line 9 due to +2 offset)
      '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "delete_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 7, "end_line": 7}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // replace
      Server.RunOnce; // delete

      Assert.AreEqual(3, OutputWriter.GetCount);
      Assert.IsTrue(OutputWriter.GetLine(2).Contains('Deleted 1 line'), 'Expected single line deletion');

      ResultLines := TStringList.Create;
      try
        ResultLines.LoadFromFile(TempFile, TEncoding.UTF8);
        Assert.AreEqual(11, ResultLines.Count, 'File should have 11 lines (10 + 2 - 1)');
        Assert.AreEqual('Line01', ResultLines[0]);
        Assert.AreEqual('Line02', ResultLines[1]);
        Assert.AreEqual('New3a', ResultLines[2]);
        Assert.AreEqual('New3d', ResultLines[5]);
        Assert.AreEqual('Line05', ResultLines[6]);
        Assert.AreEqual('Line06', ResultLines[7]);
        Assert.AreEqual('Line08', ResultLines[8]);
        Assert.AreEqual('Line09', ResultLines[9]);
        Assert.AreEqual('Line10', ResultLines[10]);
      finally
        ResultLines.Free;
      end;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TMcpLinterServerTests.TestDeleteLinesFileNotFound;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
begin
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "delete_lines", "arguments": {"file": "C:\\\\nonexistent\\\\file.pas", "start_line": 1, "end_line": 1}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpLinterServer.Create(InputReader, OutputWriter);
  try
    Server.RunOnce;
    Server.RunOnce;

    Assert.AreEqual(2, OutputWriter.GetCount);
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('File not found'),
      'Expected file not found: ' + OutputWriter.GetLine(1));
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpLinterServerTests.TestGetLinterResultsFileNotFound;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
begin
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "get_linter_results", "arguments": {"file": "C:\\\\nonexistent\\\\file.pas", "style_file": "C:\\\\nonexistent\\\\style.pas"}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpLinterServer.Create(InputReader, OutputWriter);
  try
    Server.RunOnce;
    Server.RunOnce;

    Assert.AreEqual(2, OutputWriter.GetCount);
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('File not found'),
      'Expected file not found: ' + OutputWriter.GetLine(1));
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpLinterServerTests.TestMcpLinterFullWorkflow;
var
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  Server      : TMcpLinterServer;
  TempFile    : String;
  ResultLines : TStringList;
begin
  TempFile := CreateTempFileWithLines([
    'Alpha', 'Bravo', 'Charlie', 'Delta', 'Echo',
    'Foxtrot', 'Golf', 'Hotel', 'India', 'Juliet']);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      // Read lines 3-5
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "read_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 3, "end_line": 5}}}' + sLineBreak +
      // Replace line 4 with 2 lines (Delta = +1)
      '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 4, "end_line": 4, "new_content": "Delta-Fixed\nDelta-Extra"}}}' + sLineBreak +
      // Read lines 8-9 (original), should read actual 9-10 due to +1 offset
      '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "read_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 8, "end_line": 9}}}' + sLineBreak +
      // Replace lines 8-9 (original) with 1 line (Delta = -1, cumulative = 0)
      '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "replace_code_lines", "arguments": {"file": "' + EscapePath(TempFile) + '", "start_line": 8, "end_line": 9, "new_content": "HotelIndia-Merged"}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpLinterServer.Create(InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // read 3-5
      Server.RunOnce; // replace 4
      Server.RunOnce; // read 8-9 (with offset)
      Server.RunOnce; // replace 8-9 (with offset)

      Assert.AreEqual(5, OutputWriter.GetCount);

      // Verify first read
      var ReadResp1 := OutputWriter.GetLine(1);
      Assert.IsTrue(ReadResp1.Contains('Charlie'), 'First read should contain Charlie');
      Assert.IsTrue(ReadResp1.Contains('Delta'), 'First read should contain Delta');
      Assert.IsTrue(ReadResp1.Contains('Echo'), 'First read should contain Echo');

      // Verify second read (with offset applied)
      var ReadResp2 := OutputWriter.GetLine(3);
      Assert.IsTrue(ReadResp2.Contains('Hotel'), 'Second read should contain Hotel: ' + ReadResp2);
      Assert.IsTrue(ReadResp2.Contains('India'), 'Second read should contain India: ' + ReadResp2);

      // Verify final file content
      ResultLines := TStringList.Create;
      try
        ResultLines.LoadFromFile(TempFile, TEncoding.UTF8);
        Assert.AreEqual(10, ResultLines.Count, 'File should be back to 10 lines (+1 -1)');
        Assert.AreEqual('Alpha', ResultLines[0]);
        Assert.AreEqual('Delta-Fixed', ResultLines[3]);
        Assert.AreEqual('Delta-Extra', ResultLines[4]);
        Assert.AreEqual('HotelIndia-Merged', ResultLines[8]);
        Assert.AreEqual('Juliet', ResultLines[9]);
      finally
        ResultLines.Free;
      end;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TLineOffsetTrackerTests);
  TDUnitX.RegisterTestFixture(TMcpLinterServerTests);

end.
