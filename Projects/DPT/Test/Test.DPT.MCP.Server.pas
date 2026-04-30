unit Test.DPT.MCP.Server;

interface

uses

  Winapi.Windows,

  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.JSON,
  System.SyncObjs,
  System.SysUtils,

  DUnitX.TestFramework,

  DPT.Debugger,
  DPT.MCP.Server;

type

  [TestFixture]
  TMcpServerTests = class
  private
    function ResolveTargetPath(const AExeName: string; AUse64Bit: Boolean): string;
    procedure DoTestMcpProtocolFlow(AUse64Bit: Boolean);
    procedure DoTestMcpExceptionFlow(AUse64Bit: Boolean);
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
    procedure TestMcpGetLocalsHappyPath;
    [Test]
    procedure TestMcpGetLocalsNoLocalsInProcedure;
    [Test]
    procedure TestMcpGetLocalsListedInTools;
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
    procedure TestMcpStartSessionWithoutMapAndPendingBreakpoint;
    [Test]
    procedure TestMcpSetBreakpointWithoutMapHintsAtMissingMap;
    [Test]
    procedure TestMcpWaitUntilPaused;
    [Test]
    procedure TestMcpIgnoredExceptions;
    [Test]
    procedure TestMcpMultiThreadedBreakpoints;
    [Test]
    procedure TestMcpListAndSwitchThreads;
    [Test]
    procedure TestMcpMapFileUnlockAfterExit;
    {$IFDEF CPUX64}
    [Test]
    procedure TestMcpProtocolFlow64;
    [Test]
    procedure TestMcpExceptionFlow64;
    {$ENDIF}
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

function TMcpServerTests.ResolveTargetPath(const AExeName: string; AUse64Bit: Boolean): string;
begin
  if AUse64Bit then
  begin
    Result := ExpandFileName('Projects\DPT\Test\Win64\' + AExeName);
    if not FileExists(Result) then
      Result := ExpandFileName('Win64\' + AExeName);
  end
  else
  begin
    Result := ExpandFileName('Projects\DPT\Test\Win32\' + AExeName);
    if not FileExists(Result) then
      Result := ExpandFileName('Win32\' + AExeName);
  end;
end;

procedure TMcpServerTests.DoTestMcpProtocolFlow(AUse64Bit: Boolean);
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
  StackMemoryLine: string;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', AUse64Bit);
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
      StackMemoryLine := OutputWriter.GetLine(7);
      if not AUse64Bit then
        Assert.IsTrue(StackMemoryLine.Contains('78 56 34 12'), 'LocalInt missing in stack dump')
      else
        // On x64, LocalInt may be in a register rather than on the stack
        Assert.IsTrue(StackMemoryLine.Contains('SP'), 'Stack memory should contain SP/RSP indicator');

      Server.RunOnce; // read_global_variable
      Assert.AreEqual(9, OutputWriter.GetCount, 'ReadGlobalVariable failed');
      Assert.IsTrue(OutputWriter.GetLine(8).Contains('44 33 22 11'), 'GGlobalInt value $11223344 missing');

      Server.RunOnce; // get_stack_slots
      Assert.AreEqual(10, OutputWriter.GetCount, 'GetStackSlots failed');
      if not AUse64Bit then
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

procedure TMcpServerTests.DoTestMcpExceptionFlow(AUse64Bit: Boolean);
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  LJSON: TJSONObject;
  ExePath, MapFile: string;
begin
  ExePath := ResolveTargetPath('ExceptionTarget.exe', AUse64Bit);
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

procedure TMcpServerTests.TestMcpProtocolFlow;
begin
  DoTestMcpProtocolFlow(False);
end;

procedure TMcpServerTests.TestMcpExceptionFlow;
begin
  DoTestMcpExceptionFlow(False);
end;

{$IFDEF CPUX64}
procedure TMcpServerTests.TestMcpProtocolFlow64;
begin
  DoTestMcpProtocolFlow(True);
end;

procedure TMcpServerTests.TestMcpExceptionFlow64;
begin
  DoTestMcpExceptionFlow(True);
end;
{$ENDIF}

procedure TMcpServerTests.TestMcpSteppingFlow;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
  StepNotif: string;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
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
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
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

procedure TMcpServerTests.TestMcpGetLocalsHappyPath;
var
  Debugger    : TDebugger;
  Server      : TMcpServer;
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath     : String;
  MapFile     : String;
  LJSON       : TJSONObject;
  ResultObj   : TJSONObject;
  ContentArr  : TJSONArray;
  Inner       : TJSONObject;
  Locals      : TJSONArray;
  Names       : TStringList;
  HexValues   : TStringList;
  I           : Integer;
  Line        : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  MapFile := ChangeFileExt(ExePath, '.map');

  // Break on the Writeln in LocalsProcedure once all three locals
  // (LocalA / LocalB / LocalC) have been assigned.
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 38}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    Debugger.LoadDebugInfoFromExe(ExePath);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp line 38
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue (async)

      WaitForOutput(OutputWriter, 6); // stopped notification + sampling request

      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_locals", "arguments": {}}}');
      Server.RunOnce;

      Assert.AreEqual(7, OutputWriter.GetCount, 'get_locals response missing');
      Line := OutputWriter.GetLine(6);
      LJSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
      try
        ResultObj := LJSON.GetValue('result') as TJSONObject;
        Assert.IsNotNull(ResultObj, 'No result object in get_locals response');
        ContentArr := ResultObj.GetValue('content') as TJSONArray;
        Assert.IsNotNull(ContentArr, 'No content array in get_locals response');
        Assert.IsTrue(ContentArr.Count > 0, 'Empty content array');

        // Inner payload is a JSON string inside content[0].text.
        Inner := TJSONObject.ParseJSONValue(
          (ContentArr.Items[0] as TJSONObject).GetValue('text').Value) as TJSONObject;
        try
          Assert.IsTrue(Inner.GetValue('procedure').Value.Contains('LocalsProcedure'),
            'procedure field should reference LocalsProcedure, got: ' +
            Inner.GetValue('procedure').Value);
          Locals := Inner.GetValue('locals') as TJSONArray;
          Assert.IsNotNull(Locals, 'locals array missing');
          Assert.IsTrue(Locals.Count >= 3,
            Format('Expected at least 3 locals, got %d', [Locals.Count]));

          Names := TStringList.Create;
          HexValues := TStringList.Create;
          try
            for I := 0 to Locals.Count - 1 do
            begin
              Names.Add((Locals.Items[I] as TJSONObject).GetValue('name').Value);
              HexValues.Values[(Locals.Items[I] as TJSONObject).GetValue('name').Value] :=
                (Locals.Items[I] as TJSONObject).GetValue('hex').Value;
            end;
            Assert.IsTrue(Names.IndexOf('LocalA') >= 0, 'LocalA missing');
            Assert.IsTrue(Names.IndexOf('LocalB') >= 0, 'LocalB missing');
            Assert.IsTrue(Names.IndexOf('LocalC') >= 0, 'LocalC missing');
            // LocalA = $12345678 -> first 4 LE bytes "78 56 34 12"
            Assert.IsTrue(HexValues.Values['LocalA'].StartsWith('78 56 34 12'),
              'LocalA hex must start with "78 56 34 12", got: ' + HexValues.Values['LocalA']);
            // LocalB = $1122334455667788 -> 8 LE bytes "88 77 66 55 44 33 22 11"
            Assert.AreEqual('88 77 66 55 44 33 22 11',
              HexValues.Values['LocalB'].ToUpper,
              'LocalB hex mismatch');
            // LocalC = $DEADBEEF -> first 4 LE bytes "EF BE AD DE"
            Assert.IsTrue(HexValues.Values['LocalC'].ToUpper.StartsWith('EF BE AD DE'),
              'LocalC hex must start with "EF BE AD DE", got: ' + HexValues.Values['LocalC']);
          finally
            HexValues.Free;
            Names.Free;
          end;
        finally
          Inner.Free;
        end;
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

procedure TMcpServerTests.TestMcpGetLocalsNoLocalsInProcedure;
var
  Debugger    : TDebugger;
  Server      : TMcpServer;
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath     : String;
  MapFile     : String;
  LJSON       : TJSONObject;
  ResultObj   : TJSONObject;
  ContentArr  : TJSONArray;
  Text        : String;
  Line        : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  MapFile := ChangeFileExt(ExePath, '.map');

  // TargetProcedure (line 17) is covered by TD32 but declares no
  // local variables. The handler must report this distinctly so an
  // AI agent can tell it apart from "no debug info".
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 17}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    Debugger.LoadDebugInfoFromExe(ExePath);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp line 17
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue

      WaitForOutput(OutputWriter, 6);

      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_locals", "arguments": {}}}');
      Server.RunOnce;

      Line := OutputWriter.GetLine(6);
      LJSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
      try
        ResultObj := LJSON.GetValue('result') as TJSONObject;
        ContentArr := ResultObj.GetValue('content') as TJSONArray;
        Text := (ContentArr.Items[0] as TJSONObject).GetValue('text').Value;
        Assert.IsTrue(Text.Contains('no recorded local variables'),
          'Response should explain that the procedure has no locals; got: ' + Text);
        Assert.IsTrue(Text.Contains('TargetProcedure'),
          'Response should name the procedure; got: ' + Text);
        // Must NOT be marked isError (this is informational, not a failure).
        var IsErr := ResultObj.GetValue('isError');
        Assert.IsTrue((IsErr = nil) or not (IsErr is TJSONBool) or
          not (IsErr as TJSONBool).AsBoolean,
          'Empty-locals response should be a regular text result, not isError');
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

procedure TMcpServerTests.TestMcpGetLocalsListedInTools;
var
  Debugger    : TDebugger;
  Server      : TMcpServer;
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  LJSON       : TJSONObject;
  ResultObj   : TJSONObject;
  Tools       : TJSONArray;
  Found       : Boolean;
  I           : Integer;
begin
  // The tool must show up in tools/list with a non-empty description so
  // an AI agent discovers it via the standard MCP introspection flow.
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/list"}');

  Debugger := nil;
  OutputWriter := TStringTextWriter.Create;
  Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
  try
    Server.RunOnce; // init
    Server.RunOnce; // tools/list

    Assert.IsTrue(OutputWriter.GetCount >= 2, 'tools/list response missing');
    LJSON := TJSONObject.ParseJSONValue(OutputWriter.GetLine(1)) as TJSONObject;
    try
      ResultObj := LJSON.GetValue('result') as TJSONObject;
      Tools := ResultObj.GetValue('tools') as TJSONArray;
      Assert.IsNotNull(Tools);
      Found := False;
      for I := 0 to Tools.Count - 1 do
      begin
        var T := Tools.Items[I] as TJSONObject;
        if T.GetValue('name').Value = 'get_locals' then
        begin
          Found := True;
          Assert.IsTrue(T.GetValue('description').Value.Length > 50,
            'get_locals must have a non-trivial description');
          Break;
        end;
      end;
      Assert.IsTrue(Found, 'get_locals tool not registered in tools/list');
    finally
      LJSON.Free;
    end;
  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
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
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
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
  ExePath := ResolveTargetPath('DebugTarget.exe', False);

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
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
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
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
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
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
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
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
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
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  
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

procedure TMcpServerTests.TestMcpStartSessionWithoutMapAndPendingBreakpoint;
var
  Server      : TMcpServer;
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath     : String;
  TempPath    : String;
  StartLine   : String;
begin
  // When the user has queued breakpoints but the executable ships without
  // a .map file, the start-session response must explain BOTH facts: the
  // missing map (root cause) AND the breakpoints that won't trigger as a
  // consequence. Previously the unresolved-breakpoints warning hid the
  // missing-map message, leaving the AI agent guessing at the root cause.
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  TempPath := ChangeFileExt(ExePath, Format('.NoMapBP.%d.exe', [GetTickCount]));
  TFile.Copy(ExePath, TempPath, True);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "start_debug_session", "arguments": {"executable_path": "' +
        StringReplace(TempPath, '\', '\\', [rfReplaceAll]) + '"}}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "terminate_debug_session", "arguments": {}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(nil, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_breakpoint (queued as pending, no resolution yet)
      Server.RunOnce; // start_debug_session

      StartLine := OutputWriter.GetLine(2);
      Assert.IsTrue(StartLine.Contains('No .map file found'),
        'Missing-map warning must appear even when breakpoints are pending: ' + StartLine);
      Assert.IsTrue(StartLine.Contains('DebugTarget.dpr:13'),
        'Pending unresolvable breakpoint must be listed: ' + StartLine);
      Assert.IsTrue(StartLine.Contains('will not trigger'),
        'Consequence must be stated explicitly: ' + StartLine);

      Server.RunOnce; // terminate
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempPath) then
    begin
      Sleep(100);
      try TFile.Delete(TempPath); except end;
    end;
  end;
end;

procedure TMcpServerTests.TestMcpSetBreakpointWithoutMapHintsAtMissingMap;
var
  Server      : TMcpServer;
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath     : String;
  TempPath    : String;
  ErrorLine   : String;
begin
  // After the session is started against a no-.map binary, calling
  // set_breakpoint must point the AI agent at the missing .map as the
  // most likely cause - rather than vaguely suggesting to "verify unit
  // names and line numbers" - because that is the actionable fix.
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  TempPath := ChangeFileExt(ExePath, Format('.NoMapSB.%d.exe', [GetTickCount]));
  TFile.Copy(ExePath, TempPath, True);
  try
    InputReader := TStringTextReader.Create(
      '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "start_debug_session", "arguments": {"executable_path": "' +
        StringReplace(TempPath, '\', '\\', [rfReplaceAll]) + '"}}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
      '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "terminate_debug_session", "arguments": {}}}');

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(nil, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // start_debug_session (no map)
      Server.RunOnce; // set_breakpoint (must error with map-missing hint)

      ErrorLine := OutputWriter.GetLine(2);
      Assert.IsTrue(ErrorLine.Contains('Could not resolve address'),
        'Generic resolution-failure message expected: ' + ErrorLine);
      Assert.IsTrue(ErrorLine.Contains('No .map file'),
        'Hint must explicitly call out the missing .map file: ' + ErrorLine);
      Assert.IsTrue(ErrorLine.Contains('DCC_MapFile=3'),
        'Hint must mention the actionable rebuild parameter: ' + ErrorLine);

      Server.RunOnce; // terminate
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    if FileExists(TempPath) then
    begin
      Sleep(100);
      try TFile.Delete(TempPath); except end;
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
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
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

      var WaitResponse := '';      var LJSON: TJSONObject;
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
  ExePath := ResolveTargetPath('MultiThreadTarget.exe', False);
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
      WaitForOutput(OutputWriter, 7);
      
      ThreadId1 := 0;
      for I := OutputWriter.GetCount - 1 downto 0 do
      begin
        var Line := OutputWriter.GetLine(I);
        if (Line.Contains('"id": 5') or Line.Contains('"id":"5"') or Line.Contains('"id":5')) and Line.Contains('"result"') then
        begin
          LJSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
          try
            ResultObj := LJSON.GetValue('result') as TJSONObject;
            if Assigned(ResultObj) then
            begin
              var ContentArr := ResultObj.GetValue('content') as TJSONArray;
              if Assigned(ContentArr) and (ContentArr.Count > 0) then
              begin
                var TextVal := (ContentArr.Items[0] as TJSONObject).GetValue('text').Value;
                var StateJSON := TJSONObject.ParseJSONValue(TextVal) as TJSONObject;
                try
                  ThreadId1 := (StateJSON.GetValue('thread_id') as TJSONNumber).AsInt64;
                finally
                  StateJSON.Free;
                end;
              end;
            end;
          finally
            LJSON.Free;
          end;
          if ThreadId1 > 0 then Break;
        end;
      end;
      
      Assert.IsTrue(ThreadId1 > 0, 'Thread ID 1 should be greater than 0. Last checked line: ' + OutputWriter.GetLine(OutputWriter.GetCount - 1));

      // Continue to hit the second breakpoint
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');
      Server.RunOnce;

      // Wait for the second breakpoint notification
      WaitForOutput(OutputWriter, 10);
      
      // Get state to read the second thread_id
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 7, "method": "tools/call", "params": {"name": "get_state", "arguments": {}}}');
      Server.RunOnce;
      WaitForOutput(OutputWriter, 11);

      ThreadId2 := 0;
      for I := OutputWriter.GetCount - 1 downto 0 do
      begin
        var Line := OutputWriter.GetLine(I);
        if (Line.Contains('"id": 7') or Line.Contains('"id":"7"') or Line.Contains('"id":7')) and Line.Contains('"result"') then
        begin
          LJSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
          try
            ResultObj := LJSON.GetValue('result') as TJSONObject;
            if Assigned(ResultObj) then
            begin
              var ContentArr := ResultObj.GetValue('content') as TJSONArray;
              if Assigned(ContentArr) and (ContentArr.Count > 0) then
              begin
                var TextVal := (ContentArr.Items[0] as TJSONObject).GetValue('text').Value;
                var StateJSON := TJSONObject.ParseJSONValue(TextVal) as TJSONObject;
                try
                  ThreadId2 := (StateJSON.GetValue('thread_id') as TJSONNumber).AsInt64;
                finally
                  StateJSON.Free;
                end;
              end;
            end;
          finally
            LJSON.Free;
          end;
          if ThreadId2 > 0 then Break;
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

procedure TMcpServerTests.TestMcpListAndSwitchThreads;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
  LJSON, ResultObj, SwitchResult: TJSONObject;
  ThreadIds: TArray<Int64>;
  I: Integer;
  OtherThreadId: Int64;
  Arr: TJSONArray;
begin
  ExePath := ResolveTargetPath('MultiThreadTarget.exe', False);
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "MultiThreadTarget.dpr", "line": 29}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_breakpoint
      Server.RunOnce; // continue (async)

      // Wait for breakpoint notification
      WaitForOutput(OutputWriter, 5);

      // Call list_threads
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "list_threads", "arguments": {}}}');
      Server.RunOnce;

      SetLength(ThreadIds, 0);
      for I := OutputWriter.GetCount - 1 downto 0 do
      begin
        if OutputWriter.GetLine(I).Contains('"id": 4') or OutputWriter.GetLine(I).Contains('"id":"4"') or OutputWriter.GetLine(I).Contains('"id":4') then
        begin
          LJSON := TJSONObject.ParseJSONValue(OutputWriter.GetLine(I)) as TJSONObject;
          try
            ResultObj := LJSON.GetValue('result') as TJSONObject;
            var ContentArr := ResultObj.GetValue('content') as TJSONArray;
            var TextVal := (ContentArr.Items[0] as TJSONObject).GetValue('text').Value;
            Arr := TJSONObject.ParseJSONValue(TextVal) as TJSONArray;
            try
              SetLength(ThreadIds, Arr.Count);
              for var J := 0 to Arr.Count - 1 do
                ThreadIds[J] := (Arr.Items[J] as TJSONNumber).AsInt64;
            finally
              Arr.Free;
            end;
          finally
            LJSON.Free;
          end;
          Break;
        end;
      end;

      Assert.IsTrue(Length(ThreadIds) > 1, 'Should list multiple active threads');

      // Pick a thread ID that is not the current one (Main vs Worker)
      OtherThreadId := 0;
      for I := 0 to High(ThreadIds) do
      begin
        if ThreadIds[I] <> Debugger.LastThreadId then
        begin
          OtherThreadId := ThreadIds[I];
          Break;
        end;
      end;
      Assert.IsTrue(OtherThreadId > 0, 'Could not find another thread to switch to');

      // Call switch_thread
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "switch_thread", "arguments": {"thread_id": ' + IntToStr(OtherThreadId) + '}}}');
      Server.RunOnce;

      // Verify switch was successful
      SwitchResult := nil;
      for I := OutputWriter.GetCount - 1 downto 0 do
      begin
        if OutputWriter.GetLine(I).Contains('"id": 5') or OutputWriter.GetLine(I).Contains('"id":"5"') or OutputWriter.GetLine(I).Contains('"id":5') then
        begin
          SwitchResult := TJSONObject.ParseJSONValue(OutputWriter.GetLine(I)) as TJSONObject;
          Break;
        end;
      end;
      Assert.IsNotNull(SwitchResult, 'Expected switch_thread response');
      try
        Assert.IsFalse(SwitchResult.ToJSON.Contains('isError'), 'Switching thread should not return an error');
      finally
        SwitchResult.Free;
      end;

      // Get state to verify thread id changed
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "get_state", "arguments": {}}}');
      Server.RunOnce;
      
      var NewThreadId: Int64 := 0;
      for I := OutputWriter.GetCount - 1 downto 0 do
      begin
        if OutputWriter.GetLine(I).Contains('"id": 6') or OutputWriter.GetLine(I).Contains('"id":"6"') or OutputWriter.GetLine(I).Contains('"id":6') then
        begin
          LJSON := TJSONObject.ParseJSONValue(OutputWriter.GetLine(I)) as TJSONObject;
          try
            ResultObj := LJSON.GetValue('result') as TJSONObject;
            var ContentArr := ResultObj.GetValue('content') as TJSONArray;
            var TextVal := (ContentArr.Items[0] as TJSONObject).GetValue('text').Value;
            var StateJSON := TJSONObject.ParseJSONValue(TextVal) as TJSONObject;
            try
              NewThreadId := (StateJSON.GetValue('thread_id') as TJSONNumber).AsInt64;
            finally
              StateJSON.Free;
            end;
          finally
            LJSON.Free;
          end;
          Break;
        end;
      end;
      
      Assert.AreEqual(OtherThreadId, NewThreadId, 'Thread ID in state should match the switched thread ID');

    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TMcpServerTests.TestMcpMapFileUnlockAfterExit;
var
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath, MapFile: string;
  FileHandle: THandle;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  MapFile := ChangeFileExt(ExePath, '.map');

  // We test starting a debug session, continuing until exit, and then attempting to access the map file
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    Format('{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "start_debug_session", "arguments": {"executable_path": "%s"}}}', [StringReplace(ExePath, '\', '\\', [rfReplaceAll])]) + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "wait_until_paused", "arguments": {"timeout_ms": 5000}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "stop_debug_session", "arguments": {}}}');

  OutputWriter := TStringTextWriter.Create;
  Server := TMcpServer.Create(nil, InputReader, OutputWriter);
  try
    Server.RunOnce; // init
    
    Server.RunOnce; // ignore_exception

    Server.RunOnce; // start_debug_session (this will lock the map file)
    Assert.IsTrue(OutputWriter.GetLine(2).Contains('Debug session started'), 'Session should start');
    Assert.AreEqual(Ord(dsPaused), Ord(Server.State), 'State should be paused after starting');

    Server.RunOnce; // continue (let the program run and exit)
    
    Server.RunOnce; // wait_until_paused (will wait until the debugger catches the exit event)
    Assert.AreEqual(Ord(dsExited), Ord(Server.State), 'State should be exited');
    
    // Now call stop_debug_session (this should free the debugger and thus the map file)
    Server.RunOnce; // stop_debug_session
    Assert.AreEqual(Ord(dsNoSession), Ord(Server.State), 'State should be no_session after stopping');

    // Attempt to open the map file for writing (GENERIC_WRITE, share mode 0 to enforce exclusivity)
    // If it's locked by the TDebugger, this will fail.
    FileHandle := CreateFile(PChar(MapFile), GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    Assert.AreNotEqual(THandle(INVALID_HANDLE_VALUE), FileHandle, 'Map file is locked after debug session exit and stop_debug_session!');
    
    if FileHandle <> INVALID_HANDLE_VALUE then
      CloseHandle(FileHandle);

  finally
    Server.Free;
    InputReader.Free;
    OutputWriter.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMcpServerTests);

end.
