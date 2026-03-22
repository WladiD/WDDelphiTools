unit Test.DPT.MCP.Server;

interface

uses
  Winapi.Windows,
  DUnitX.TestFramework,
  DPT.Debugger,
  DPT.MCP.Server,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.SyncObjs,
  System.IOUtils;

type
  [TestFixture]
  TMcpServerTests = class
  public
    [Test]
    procedure TestMcpProtocolFlow;
    [Test]
    procedure TestMcpExceptionFlow;
    [Test]
    procedure TestMcpSteppingFlow;
    [Test]
    procedure TestMcpStackFrameInfo;
    [Test]
    procedure TestMcpBreakpointManagement;
    [Test]
    procedure TestMcpPendingBreakpoints;
    [Test]
    procedure TestMcpBreakpointLimit;
    [Test]
    procedure TestMcpGetState;
    [Test]
    procedure TestMcpBreakpointUnresolvable;
    [Test]
    procedure TestMcpStopDebugSession;
    [Test]
    procedure TestMcpTerminateDebugSession;
    [Test]
    procedure TestMcpStartSessionWithoutMapFile;
    [Test]
    procedure TestMcpWaitUntilPaused;
    [Test]
    procedure TestMcpIgnoredExceptions;
    [Test]
    procedure TestMcpMultiThreadedBreakpoints;
  end;

implementation

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

procedure WaitForOutput(AWriter: TStringTextWriter; AMinCount: Integer; ATimeoutMs: Integer = 5000);
var
  Elapsed: Integer;
begin
  Elapsed := 0;
  while (AWriter.GetCount < AMinCount) and (Elapsed < ATimeoutMs) do
  begin
    Sleep(50);
    Inc(Elapsed, 50);
  end;
  Assert.IsTrue(AWriter.GetCount >= AMinCount,
    Format('Timeout waiting for output. Expected %d lines, got %d', [AMinCount, AWriter.GetCount]));
end;

{ TMcpServerTests }

procedure TMcpServerTests.TestMcpProtocolFlow;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // initialize
      Assert.AreEqual(1, OutputWriter.GetCount, 'Initialize failed');

      Server.RunOnce; // set_breakpoint
      Assert.AreEqual(2, OutputWriter.GetCount, 'SetBreakpoint failed');

      Server.RunOnce; // ignore_exception
      Assert.AreEqual(3, OutputWriter.GetCount, 'IgnoreException failed');

      Server.RunOnce; // continue (async)
      Assert.AreEqual(4, OutputWriter.GetCount, 'Continue failed');
      Assert.IsTrue(OutputWriter.GetLine(3).Contains('Execution resumed'), 'Continue should return immediately: ' + OutputWriter.GetLine(3));

      // Wait for breakpoint notification from debugger thread
      WaitForOutput(OutputWriter, 6);
      Assert.IsTrue(OutputWriter.GetLine(4).Contains('notifications/stopped'), 'Expected stopped notification: ' + OutputWriter.GetLine(4));
      Assert.IsTrue(OutputWriter.GetLine(4).Contains('breakpoint'), 'Expected breakpoint reason');

      // Now state is paused, we can inspect
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_stack_trace", "arguments": {}}}');
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "get_stack_memory", "arguments": {}}}');
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 7, "method": "tools/call", "params": {"name": "read_global_variable", "arguments": {"name": "DebugTarget.GGlobalInt", "size": 4}}}');
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 8, "method": "tools/call", "params": {"name": "get_stack_slots", "arguments": {}}}');

      Server.RunOnce; // get_stack_trace
      Assert.AreEqual(7, OutputWriter.GetCount, 'StackTrace failed');
      Assert.IsTrue(OutputWriter.GetLine(6).Contains('DeepProcedure'), 'DeepProcedure missing');

      Server.RunOnce; // get_stack_memory
      Assert.AreEqual(8, OutputWriter.GetCount, 'StackMemory failed');
      Assert.IsTrue(OutputWriter.GetLine(7).Contains('78 56 34 12'), 'LocalInt missing in stack dump');

      Server.RunOnce; // read_global_variable
      Assert.AreEqual(9, OutputWriter.GetCount, 'ReadGlobalVariable failed');
      Assert.IsTrue(OutputWriter.GetLine(8).Contains('44 33 22 11'), 'GGlobalInt value $11223344 missing');

      Server.RunOnce; // get_stack_slots
      Assert.AreEqual(10, OutputWriter.GetCount, 'GetStackSlots failed');
      Assert.IsTrue(OutputWriter.GetLine(9).Contains('12345678'), 'LocalInt missing in stack slots');

    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpExceptionFlow;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  LJSON: TJSONObject;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\ExceptionTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('ExceptionTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // continue (async)

      // Wait for exception notification from debugger thread
      WaitForOutput(OutputWriter, 3);

      var ExceptionNotifFound := False;
      for var I := 0 to OutputWriter.GetCount - 1 do
      begin
        if OutputWriter.GetLine(I).Contains('notifications/debugger_exception') then
        begin
          ExceptionNotifFound := True;
          LJSON := TJSONObject.ParseJSONValue(OutputWriter.GetLine(I)) as TJSONObject;
          try
            var Params := LJSON.GetValue('params') as TJSONObject;
            Assert.AreEqual('c0000005', Params.GetValue('code').Value);
            Assert.AreEqual('ExceptionTarget.CrashProcedure', Params.GetValue('procedure').Value);
          finally
            LJSON.Free;
          end;
        end;
      end;
      Assert.IsTrue(ExceptionNotifFound, 'Exception notification not received');

      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');
      Server.RunOnce;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpSteppingFlow;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
  StepNotif: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 22}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp
      Server.RunOnce; // continue (async)

      // Wait for breakpoint notification at line 22
      WaitForOutput(OutputWriter, 5);
      Assert.IsTrue(OutputWriter.GetLine(3).Contains('notifications/stopped'), 'Expected stopped notification after continue');

      // step_into (async)
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "step_into", "arguments": {}}}');
      Server.RunOnce;
      Assert.IsTrue(OutputWriter.GetLine(5).Contains('Stepping into'), 'Step into should return immediately');

      // Wait for step notification
      WaitForOutput(OutputWriter, 8);

      // step_into again (async)
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "step_into", "arguments": {}}}');
      Server.RunOnce;

      // Wait for step notification
      WaitForOutput(OutputWriter, 11);
      StepNotif := OutputWriter.GetLine(9);
      Assert.IsTrue(StepNotif.Contains('notifications/stopped'), 'Expected stopped notification after step: ' + StepNotif);
      Assert.IsTrue(StepNotif.Contains('DebugTarget'), 'Step should be in DebugTarget: ' + StepNotif);

    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpStackFrameInfo;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  LJSON: TJSONObject;
  ResultObj, Meta: TJSONObject;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue (async)

      // Wait for breakpoint notification
      WaitForOutput(OutputWriter, 6);

      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_stack_slots", "arguments": {}}}');
      Server.RunOnce; // get_stack_slots

      Assert.AreEqual(7, OutputWriter.GetCount);
      LJSON := TJSONObject.ParseJSONValue(OutputWriter.GetLine(6)) as TJSONObject;
      try
        ResultObj := LJSON.GetValue('result') as TJSONObject;
        Meta := ResultObj.GetValue('frame_metadata') as TJSONObject;
        Assert.IsNotNull(Meta, 'Frame metadata missing');
        Assert.AreEqual('DebugTarget.DeepProcedure', Meta.GetValue('procedure').Value);

        var LocalSize := (Meta.GetValue('local_variable_size') as TJSONNumber).AsInt;
        Assert.IsTrue(LocalSize >= 4, 'Detected local size too small: ' + IntToStr(LocalSize));
      finally
        LJSON.Free;
      end;
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpBreakpointManagement;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "list_breakpoints", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "remove_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "list_breakpoints", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp

      Server.RunOnce; // list_breakpoints (should have 1)
      Assert.IsTrue(OutputWriter.GetLine(2).Contains('DebugTarget.dpr') and OutputWriter.GetLine(2).Contains('13'), 'Breakpoint missing in list');

      Server.RunOnce; // remove_breakpoint
      Assert.IsTrue(OutputWriter.GetLine(3).Contains('removed'), 'Remove failed');

      Server.RunOnce; // list_breakpoints (should be empty)
      Assert.IsTrue(OutputWriter.GetLine(4).Contains('[]'), 'Breakpoint list should be empty: ' + OutputWriter.GetLine(4));

    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpPendingBreakpoints;
var
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "list_breakpoints", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    Format('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "start_debug_session", "arguments": {"executable_path": "%s"}}}', [StringReplace(ExePath, '\', '\\', [rfReplaceAll])]) + sLineBreak +
    '{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpServer.Create(nil, InputReader, OutputWriter);
  try
    Server.RunOnce; // init
    Assert.AreEqual(1, OutputWriter.GetCount);

    // Set breakpoint before session (pending)
    Server.RunOnce;
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('pending'), 'Should indicate pending: ' + OutputWriter.GetLine(1));

    // List should show pending BP
    Server.RunOnce;
    Assert.IsTrue(OutputWriter.GetLine(2).Contains('DebugTarget.dpr') and OutputWriter.GetLine(2).Contains('pending'), 'Should list pending BP');

    // Ignore exception
    Server.RunOnce;

    // Start debug session - transfers pending BPs
    Server.RunOnce;
    Assert.IsTrue(OutputWriter.GetLine(4).Contains('Debug session started'), 'Session should start: ' + OutputWriter.GetLine(4));

    // Continue (async) - should hit the pending breakpoint
    Server.RunOnce;
    Assert.IsTrue(OutputWriter.GetLine(5).Contains('Execution resumed'), 'Continue should return immediately');

    // Wait for breakpoint notification
    WaitForOutput(OutputWriter, 8);
    Assert.IsTrue(OutputWriter.GetLine(6).Contains('notifications/stopped'), 'Expected breakpoint notification');
    Assert.IsTrue(OutputWriter.GetLine(6).Contains('breakpoint'), 'Expected breakpoint reason');

  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpServerTests.TestMcpBreakpointLimit;
var
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
begin
  // Test without a session (pending BPs)
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "A.pas", "line": 1}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "B.pas", "line": 2}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "C.pas", "line": 3}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "D.pas", "line": 4}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "E.pas", "line": 5}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpServer.Create(nil, InputReader, OutputWriter);
  try
    Server.RunOnce; // init

    // Set 4 breakpoints (should succeed)
    Server.RunOnce;
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('pending'), 'BP 1 should succeed');
    Server.RunOnce;
    Assert.IsTrue(OutputWriter.GetLine(2).Contains('pending'), 'BP 2 should succeed');
    Server.RunOnce;
    Assert.IsTrue(OutputWriter.GetLine(3).Contains('pending'), 'BP 3 should succeed');
    Server.RunOnce;
    Assert.IsTrue(OutputWriter.GetLine(4).Contains('pending'), 'BP 4 should succeed');

    // 5th breakpoint should fail
    Server.RunOnce;
    Assert.IsTrue(OutputWriter.GetLine(5).Contains('Maximum of 4'), '5th BP should fail with limit error: ' + OutputWriter.GetLine(5));

  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpServerTests.TestMcpGetState;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  // Test get_state in no_session state
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "get_state", "arguments": {}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpServer.Create(nil, InputReader, OutputWriter);
  try
    Server.RunOnce; // init
    Server.RunOnce; // get_state
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('no_session'), 'Should be no_session: ' + OutputWriter.GetLine(1));
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;

  // Test get_state in paused state
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue (async)

      // Wait for breakpoint notification
      WaitForOutput(OutputWriter, 6);

      // get_state should show paused with location
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_state", "arguments": {}}}');
      Server.RunOnce;
      Assert.IsTrue(OutputWriter.GetLine(6).Contains('paused'), 'Should be paused: ' + OutputWriter.GetLine(6));
      Assert.IsTrue(OutputWriter.GetLine(6).Contains('DebugTarget'), 'Should contain unit name');

    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpBreakpointUnresolvable;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  // Test 1: set_breakpoint on invalid unit during active session -> immediate error
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "NonExistent.pas", "line": 1}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_breakpoint on invalid unit
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('Could not resolve address'),
        'Should report unresolvable address: ' + OutputWriter.GetLine(1));
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;

  // Test 2: set_breakpoint without extension should auto-append .pas
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget", "line": 13}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_breakpoint without .pas extension
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('Breakpoint set'),
        'BP without extension should succeed: ' + OutputWriter.GetLine(1));
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;

  // Test 3: pending BP on invalid unit -> warning on session start
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "NonExistent.pas", "line": 1}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "start_debug_session", "arguments": {"executable_path": "' + StringReplace(ExePath, '\', '\\', [rfReplaceAll]) + '"}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpServer.Create(nil, InputReader, OutputWriter);
  try
    Server.RunOnce; // init
    Server.RunOnce; // set_breakpoint (pending)
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('pending'), 'Should be pending: ' + OutputWriter.GetLine(1));

    Server.RunOnce; // start_debug_session
    Assert.IsTrue(OutputWriter.GetLine(2).Contains('Debug session started'), 'Session should start: ' + OutputWriter.GetLine(2));
    Assert.IsTrue(OutputWriter.GetLine(2).Contains('WARNING'),
      'Should warn about unresolvable BP: ' + OutputWriter.GetLine(2));
    Assert.IsTrue(OutputWriter.GetLine(2).Contains('NonExistent.pas'),
      'Warning should mention the unit name: ' + OutputWriter.GetLine(2));
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpServerTests.TestMcpStopDebugSession;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_breakpoint
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue (async)
      WaitForOutput(OutputWriter, 6); // wait for breakpoint notification

      // Now paused - stop the session (detach)
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "stop_debug_session", "arguments": {}}}');
      Server.RunOnce;
      Assert.IsTrue(OutputWriter.GetLine(6).Contains('continues running'),
        'Should confirm process continues: ' + OutputWriter.GetLine(6));
      Assert.AreEqual(Ord(dsNoSession), Ord(Server.State), 'State should be no_session');

      // Verify get_state returns no_session
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "get_state", "arguments": {}}}');
      Server.RunOnce;
      Assert.IsTrue(OutputWriter.GetLine(7).Contains('no_session'),
        'get_state should return no_session: ' + OutputWriter.GetLine(7));
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpTerminateDebugSession;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_breakpoint
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue (async)
      WaitForOutput(OutputWriter, 6); // wait for breakpoint notification

      // Now paused - terminate the session
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "terminate_debug_session", "arguments": {}}}');
      Server.RunOnce;
      Assert.IsTrue(OutputWriter.GetLine(6).Contains('killed'),
        'Should confirm process was killed: ' + OutputWriter.GetLine(6));
      Assert.AreEqual(Ord(dsNoSession), Ord(Server.State), 'State should be no_session');

      // Verify inspection tools are rejected
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "get_stack_trace", "arguments": {}}}');
      Server.RunOnce;
      Assert.IsTrue(OutputWriter.GetLine(7).Contains('Invalid state'),
        'Inspection should fail in no_session: ' + OutputWriter.GetLine(7));
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpStartSessionWithoutMapFile;
var
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  
  // Create a temporary copy of the executable WITHOUT the map file, using a unique name
  var TempPath := ChangeFileExt(ExePath, Format('.NoMap.%d.exe', [GetTickCount]));
  TFile.Copy(ExePath, TempPath, True);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "start_debug_session", "arguments": {"executable_path": "' + StringReplace(TempPath, '\', '\\', [rfReplaceAll]) + '"}}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "terminate_debug_session", "arguments": {}}}');

    OutputWriter := TStringTextWriter.Create;
    // Do NOT pass a debugger instance here; handle_start_debug_session creates its own.
    Server := TMcpServer.Create(nil, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      
      Server.RunOnce; // start_debug_session
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('WARNING: No .map file found'),
        'Warning about missing map file should be present: ' + OutputWriter.GetLine(1));
      Assert.IsTrue(OutputWriter.GetLine(1).Contains('/p:DCC_MapFile=3'),
        'Instruction on how to create the map file should be present: ' + OutputWriter.GetLine(1));
        
      // Ensure the session was actually started despite the missing map
      Assert.AreEqual(Ord(dsPaused), Ord(Server.State), 'State should be paused after starting');
      
      Server.RunOnce; // terminate_debug_session
      Assert.IsTrue(OutputWriter.GetLine(2).Contains('killed'),
        'Process should be terminated properly: ' + OutputWriter.GetLine(2));
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    // Cleanup temporary file
    if FileExists(TempPath) then
    begin
      // Give the OS a moment to release the file lock from the terminated process
      Sleep(100);
      try
        TFile.Delete(TempPath);
      except
        // ignore errors
      end;
    end;
  end;
end;

procedure TMcpServerTests.TestMcpWaitUntilPaused;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 22}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp
      Server.RunOnce; // continue (async)

      // Wait for breakpoint notification at line 22
      WaitForOutput(OutputWriter, 5);
      Assert.IsTrue(OutputWriter.GetLine(3).Contains('notifications/stopped'), 'Expected stopped notification after continue');

      // step_into (async)
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "step_into", "arguments": {}}}');
      Server.RunOnce;
      Assert.IsTrue(OutputWriter.GetLine(5).Contains('Stepping into'), 'Step into should return immediately');

      // Call wait_until_paused. It should block until the step finishes.
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "wait_until_paused", "arguments": {"timeout_ms": 5000}}}');
      Server.RunOnce;
      
      // We expect the debugger notification(s) and the wait_until_paused response.
      WaitForOutput(OutputWriter, 8);
      
      // Verify wait_until_paused returns paused state.
      // Since notifications and sampling requests can vary in count, scan the output 
      // from the end to find the actual response to the wait_until_paused (id=5) call.
      // We must avoid matching sampling requests which also might contain an "id" property.
      
      Writeln('=== DEBUG OUTPUT WRITER LINES ===');
      for var I := 0 to OutputWriter.GetCount - 1 do
        Writeln(Format('[%d]: %s', [I, OutputWriter.GetLine(I)]));
      Writeln('=================================');
      
      var WaitResponse := '';
      var LJSON: TJSONObject;
      for var I := OutputWriter.GetCount - 1 downto 0 do
      begin
        var Line := OutputWriter.GetLine(I);
        // Only consider lines that look like a valid response to id=5
        if (Line.Contains('"id":"5"') or Line.Contains('"id": 5') or Line.Contains('"id":5')) then
        begin
          LJSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
          if Assigned(LJSON) then
          begin
            try
              // Responses to tools/call contain a 'result' object for successful calls
              if Assigned(LJSON.GetValue('result')) and not Assigned(LJSON.GetValue('method')) then
              begin
                WaitResponse := Line;
                Break;
              end;
            finally
              LJSON.Free;
            end;
          end;
        end;
      end;
      
      Assert.IsNotEmpty(WaitResponse, 'Could not find wait_until_paused response in output');
      Assert.IsTrue(WaitResponse.Contains('\"paused\"') or WaitResponse.Contains('"paused"'), 'wait_until_paused should return paused state: ' + WaitResponse);
      Assert.IsTrue(WaitResponse.Contains('DebugTarget'), 'wait_until_paused should contain unit name');

    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpIgnoredExceptions;
var
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
begin
  // We don't even need a running debugger process, we can just test the MCP list management
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    // 1. Check default list (should contain EAbort)
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "list_ignored_exceptions", "arguments": {}}}' + sLineBreak +
    // 2. Add an exception
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "EZeroDivide"}}}' + sLineBreak +
    // 3. Add another
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "EAccessViolation"}}}' + sLineBreak +
    // 4. Check list again (should have EAbort, EZeroDivide, EAccessViolation)
    '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "list_ignored_exceptions", "arguments": {}}}' + sLineBreak +
    // 5. Remove an exception
    '{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "unignore_exception", "arguments": {"class_name": "EZeroDivide"}}}' + sLineBreak +
    // 6. Check list again (EZeroDivide should be gone)
    '{"jsonrpc": "2.0", "id": 7, "method": "tools/call", "params": {"name": "list_ignored_exceptions", "arguments": {}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpServer.Create(nil, InputReader, OutputWriter);
  try
    Server.RunOnce; // init
    Assert.AreEqual(1, OutputWriter.GetCount);

    Server.RunOnce; // list defaults
    Assert.AreEqual(2, OutputWriter.GetCount);
    Assert.IsTrue(OutputWriter.GetLine(1).Contains('EAbort'), 'Default EAbort is missing');

    Server.RunOnce; // add EZeroDivide
    Assert.AreEqual(3, OutputWriter.GetCount);

    Server.RunOnce; // add EAccessViolation
    Assert.AreEqual(4, OutputWriter.GetCount);

    Server.RunOnce; // list after adds
    Assert.AreEqual(5, OutputWriter.GetCount);
    Assert.IsTrue(OutputWriter.GetLine(4).Contains('EAbort'), 'EAbort missing after adds');
    Assert.IsTrue(OutputWriter.GetLine(4).Contains('EZeroDivide'), 'EZeroDivide missing');
    Assert.IsTrue(OutputWriter.GetLine(4).Contains('EAccessViolation'), 'EAccessViolation missing');

    Server.RunOnce; // remove EZeroDivide
    Assert.AreEqual(6, OutputWriter.GetCount);

    Server.RunOnce; // list after removal
    Assert.AreEqual(7, OutputWriter.GetCount);
    Assert.IsTrue(OutputWriter.GetLine(6).Contains('EAbort'), 'EAbort missing after removal');
    Assert.IsFalse(OutputWriter.GetLine(6).Contains('EZeroDivide'), 'EZeroDivide should be removed');
    Assert.IsTrue(OutputWriter.GetLine(6).Contains('EAccessViolation'), 'EAccessViolation missing after removal');

  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

procedure TMcpServerTests.TestMcpMultiThreadedBreakpoints;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
  LJSON: TJSONObject;
  ResultObj: TJSONObject;
  ThreadId1, ThreadId2: Int64;
  I: Integer;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\MultiThreadTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('MultiThreadTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  // We set breakpoints on the lines we defined in MultiThreadTarget.dpr
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "MultiThreadTarget.dpr", "line": 29}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "MultiThreadTarget.dpr", "line": 40}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_breakpoint 1
      Server.RunOnce; // set_breakpoint 2
      Server.RunOnce; // continue (async)

      // Wait for the first breakpoint notification
      WaitForOutput(OutputWriter, 6);
      
      // Get state to read the first thread_id
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_state", "arguments": {}}}');
      Server.RunOnce;
      
      ThreadId1 := 0;
      for I := OutputWriter.GetCount - 1 downto 0 do
      begin
        if OutputWriter.GetLine(I).Contains('"id": 5') or OutputWriter.GetLine(I).Contains('"id":"5"') or OutputWriter.GetLine(I).Contains('"id":5') then
        begin
          LJSON := TJSONObject.ParseJSONValue(OutputWriter.GetLine(I)) as TJSONObject;
          try
            ResultObj := LJSON.GetValue('result') as TJSONObject;
            var ContentArr := ResultObj.GetValue('content') as TJSONArray;
            var TextVal := (ContentArr.Items[0] as TJSONObject).GetValue('text').Value;
            var StateJSON := TJSONObject.ParseJSONValue(TextVal) as TJSONObject;
            try
              ThreadId1 := (StateJSON.GetValue('thread_id') as TJSONNumber).AsInt64;
            finally
              StateJSON.Free;
            end;
          finally
            LJSON.Free;
          end;
          Break;
        end;
      end;
      
      Assert.IsTrue(ThreadId1 > 0, 'Thread ID 1 should be greater than 0');

      // Continue to hit the second breakpoint
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');
      Server.RunOnce;

      // Wait for the second breakpoint notification
      WaitForOutput(OutputWriter, 10);
      
      // Get state to read the second thread_id
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 7, "method": "tools/call", "params": {"name": "get_state", "arguments": {}}}');
      Server.RunOnce;

      ThreadId2 := 0;
      for I := OutputWriter.GetCount - 1 downto 0 do
      begin
        if OutputWriter.GetLine(I).Contains('"id": 7') or OutputWriter.GetLine(I).Contains('"id":"7"') or OutputWriter.GetLine(I).Contains('"id":7') then
        begin
          LJSON := TJSONObject.ParseJSONValue(OutputWriter.GetLine(I)) as TJSONObject;
          try
            ResultObj := LJSON.GetValue('result') as TJSONObject;
            var ContentArr := ResultObj.GetValue('content') as TJSONArray;
            var TextVal := (ContentArr.Items[0] as TJSONObject).GetValue('text').Value;
            var StateJSON := TJSONObject.ParseJSONValue(TextVal) as TJSONObject;
            try
              ThreadId2 := (StateJSON.GetValue('thread_id') as TJSONNumber).AsInt64;
            finally
              StateJSON.Free;
            end;
          finally
            LJSON.Free;
          end;
          Break;
        end;
      end;

      Assert.IsTrue(ThreadId2 > 0, 'Thread ID 2 should be greater than 0');
      Assert.AreNotEqual(ThreadId1, ThreadId2, 'Breakpoints should be hit in different threads');

    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMcpServerTests);

end.
