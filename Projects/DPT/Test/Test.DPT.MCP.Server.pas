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
  DPT.MCP.Server,
  Test.DPT.McpFixture;

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
    procedure TestMcpGetOutputReturnsCapturedLines;
    [Test]
    procedure TestMcpGetOutputSourceFilter;
    [Test]
    procedure TestMcpGetOutputSourceFilterAndTail;
    [Test]
    procedure TestMcpRecentOutputIsDeltaSinceLastResume;
    [Test]
    procedure TestMcpEvaluateExtendedTypes;
    [Test]
    procedure TestMcpEvaluateLocalShortString;
    [Test]
    procedure TestMcpEvaluateRejectsInvalidNameAndType;
    [Test]
    procedure TestMcpEvaluateOneLevelFieldNavigation;
    [Test]
    procedure TestMcpEvaluateTwoLevelFieldNavigation;
    [Test]
    procedure TestMcpEvaluateFieldNavigationOnNilMidChain;
    [Test]
    procedure TestMcpEvaluateFieldNavigationUnknownField;
    [Test]
    procedure TestMcpEvaluateRecordFieldNavigation;
    [Test]
    procedure TestMcpEvaluateNestedRecordFieldNavigation;
    [Test]
    procedure TestMcpEvaluateRecordToClassTransition;
    [Test]
    procedure TestMcpEvaluateRecordUnknownFieldFails;
    [Test]
    procedure TestMcpEvaluateGlobalRecordFieldNavigation;
    [Test]
    procedure TestMcpEvaluateHeaderPrefixedRecord;
    [Test]
    procedure TestMcpEvaluateVariantRecord;
    [Test]
    procedure TestMcpEvaluateNarrowIntFieldWidth;
    [Test]
    procedure TestMcpEvaluateClassFieldThroughRegisterParam;
    [Test]
    procedure TestMcpEvaluateSelfFieldInsideInstanceMethod;
    [Test]
    procedure TestMcpEvaluateMultiLevelInheritedField;
    [Test]
    procedure TestMcpEvaluateRtlInheritedField;
    [Test]
    procedure TestMcpEvaluateInheritedFieldViaVmtWalk;
    [Test]
    procedure TestMcpEvaluateFloatTypes;
    [Test]
    procedure TestMcpEvaluateAutoTypeDetection;
    [Test]
    procedure TestMcpEvaluateAutoTypeDetectionMoreTypes;
    [Test]
    procedure TestMcpEvaluateAutoTypeDetectionForLocal;
    [Test]
    procedure TestMcpEvaluateAutoTypeDetectionForGlobal;
    [Test]
    procedure TestMcpEvaluateAutoTypeDetectionRecordTerminal;
    [Test]
    procedure TestMcpEvaluateAutoTypeDetectionSelfDottedClassField;
    [Test]
    procedure TestMcpEvaluateAutoTypeDetectionEnum;
    [Test]
    procedure TestMcpEvaluateAutoTypeDetectionEnumOnLocalRecord;
    [Test]
    procedure TestMcpEvaluateAutoTypeDetectionEnumInVariantCase;
    [Test]
    procedure TestMcpEvaluateEnumNameClashPicksWrongType;
    [Test]
    procedure TestMcpEvaluateEnumWithoutNameMatchFails;
    [Test]
    procedure TestMcpEvaluateCrossUnitEnumWithSameTypeName;
    [Test]
    procedure TestMcpEvaluateCrossUnitEnumWithoutUnitSuffixHint;
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
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
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 7, "method": "tools/call", "params": {"name": "evaluate", "arguments": {"name": "DebugTarget.GGlobalInt", "type": "int"}}}');
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 8, "method": "tools/call", "params": {"name": "evaluate", "arguments": {"name": "DebugTarget.GGlobalString", "type": "string"}}}');
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 9, "method": "tools/call", "params": {"name": "evaluate", "arguments": {"name": "DebugTarget.GGlobalObject", "type": "object"}}}');
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 10, "method": "tools/call", "params": {"name": "evaluate", "arguments": {"name": "LocalInt", "type": "int"}}}');
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 11, "method": "tools/call", "params": {"name": "get_stack_slots", "arguments": {}}}');

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

      Server.RunOnce; // evaluate GGlobalInt
      Assert.AreEqual(9, OutputWriter.GetCount, 'Evaluate GGlobalInt failed');
      Assert.IsTrue(OutputWriter.GetLine(8).Contains('287454020'), 'GGlobalInt value 287454020 missing');

      Server.RunOnce; // evaluate GGlobalString
      Assert.AreEqual(10, OutputWriter.GetCount, 'Evaluate GGlobalString failed');
      Assert.IsTrue(OutputWriter.GetLine(9).Contains('Hello Global'), 'GGlobalString value "Hello Global" missing: ' + OutputWriter.GetLine(9));

      Server.RunOnce; // evaluate GGlobalObject
      Assert.AreEqual(11, OutputWriter.GetCount, 'Evaluate GGlobalObject failed');
      Assert.IsTrue(OutputWriter.GetLine(10).Contains('TStringList'), 'GGlobalObject class name TStringList missing: ' + OutputWriter.GetLine(10));

      Server.RunOnce; // evaluate LocalInt
      Assert.AreEqual(12, OutputWriter.GetCount, 'Evaluate LocalInt failed');
      Assert.IsTrue(OutputWriter.GetLine(11).Contains('305419896'), 'LocalInt value 305419896 ($12345678) missing: ' + OutputWriter.GetLine(11));

      Server.RunOnce; // get_stack_slots
      Assert.AreEqual(13, OutputWriter.GetCount, 'GetStackSlots failed');
      if not AUse64Bit then
        Assert.IsTrue(OutputWriter.GetLine(12).Contains('12345678'), 'LocalInt missing in stack slots');

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
    Debugger.LoadDebugInfoFromExe(ExePath);
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 19}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    Debugger.LoadDebugInfoFromExe(ExePath);
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
      WaitForOutput(OutputWriter, 10);
      
      StepNotif := '';
      for var I := OutputWriter.GetCount - 1 downto 0 do
      begin
        if OutputWriter.GetLine(I).Contains('notifications/stopped') then
        begin
          StepNotif := OutputWriter.GetLine(I);
          Break;
        end;
      end;
      Assert.IsTrue(StepNotif <> '', 'Expected stopped notification after second step');
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 45}}}' + sLineBreak +
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 19}}}' + sLineBreak +
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "list_breakpoints", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "remove_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "list_breakpoints", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    Debugger.LoadDebugInfoFromExe(ExePath);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp

      Server.RunOnce; // list_breakpoints (should have 1)
      Assert.IsTrue(OutputWriter.GetLine(2).Contains('DebugTarget.dpr') and OutputWriter.GetLine(2).Contains('15'), 'Breakpoint missing in list');

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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
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
    Debugger.LoadDebugInfoFromExe(ExePath);
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget", "line": 15}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    Debugger.LoadDebugInfoFromExe(ExePath);
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
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

procedure TMcpServerTests.TestMcpGetOutputReturnsCapturedLines;
var
  Debugger    : TDebugger;
  Server      : TMcpServer;
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath     : String;
  MapFile     : String;
  Line        : String;
  LJSON       : TJSONObject;
  ResultObj   : TJSONObject;
  ContentArr  : TJSONArray;
  Inner       : TJSONObject;
  LinesArr    : TJSONArray;
  HasLocals   : Boolean;
  HasOds      : Boolean;
  I           : Integer;
  Src         : String;
  Txt         : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  MapFile := ChangeFileExt(ExePath, '.map');

  // Break at the LocalsProcedure Writeln (line 38). Both the prior
  // stderr-tag-line / ods-tag-line and the Target/Deep stdout lines
  // have been emitted by then, so the buffer is well-populated.
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 45}}}' + sLineBreak +
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
      Server.RunOnce; // set_bp 38
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue

      WaitForOutput(OutputWriter, 6);

      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_output", "arguments": {}}}');
      Server.RunOnce;

      Line := OutputWriter.GetLine(6);
      LJSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
      try
        ResultObj := LJSON.GetValue('result') as TJSONObject;
        ContentArr := ResultObj.GetValue('content') as TJSONArray;
        Inner := TJSONObject.ParseJSONValue(
          (ContentArr.Items[0] as TJSONObject).GetValue('text').Value) as TJSONObject;
        try
          LinesArr := Inner.GetValue('lines') as TJSONArray;
          Assert.IsNotNull(LinesArr);
          Assert.IsTrue(LinesArr.Count > 0,
            'get_output must return at least one line at this point');

          HasLocals := False;
          HasOds := False;
          for I := 0 to LinesArr.Count - 1 do
          begin
            Src := (LinesArr.Items[I] as TJSONObject).GetValue('source').Value;
            Txt := (LinesArr.Items[I] as TJSONObject).GetValue('text').Value;
            if (Src = 'stdout') and (Txt.Contains('Target') or Txt.Contains('Deep')) then HasLocals := True;
            if (Src = 'ods') and (Txt.Contains('ods-tag-line')) then HasOds := True;
          end;
          Assert.IsTrue(HasLocals, 'Expected a stdout line from Target/Deep');
          Assert.IsTrue(HasOds, 'Expected the ods-tag-line ODS entry');
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

procedure TMcpServerTests.TestMcpGetOutputSourceFilter;
var
  Debugger    : TDebugger;
  Server      : TMcpServer;
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath     : String;
  MapFile     : String;
  Line        : String;
  LJSON       : TJSONObject;
  ResultObj   : TJSONObject;
  ContentArr  : TJSONArray;
  Inner       : TJSONObject;
  LinesArr    : TJSONArray;
  I           : Integer;
  Src         : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 45}}}' + sLineBreak +
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
      Server.RunOnce; // set_bp 38
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue

      WaitForOutput(OutputWriter, 6);

      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_output", "arguments": {"source_filter": ["ods"]}}}');
      Server.RunOnce;

      Line := OutputWriter.GetLine(6);
      LJSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
      try
        ResultObj := LJSON.GetValue('result') as TJSONObject;
        ContentArr := ResultObj.GetValue('content') as TJSONArray;
        Inner := TJSONObject.ParseJSONValue(
          (ContentArr.Items[0] as TJSONObject).GetValue('text').Value) as TJSONObject;
        try
          LinesArr := Inner.GetValue('lines') as TJSONArray;
          Assert.IsNotNull(LinesArr);
          Assert.IsTrue(LinesArr.Count > 0,
            'get_output must return at least one line at this point');

          for I := 0 to LinesArr.Count - 1 do
          begin
            Src := (LinesArr.Items[I] as TJSONObject).GetValue('source').Value;
            Assert.AreEqual('ods', Src, 'Expected only ods lines due to source_filter');
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

procedure TMcpServerTests.TestMcpGetOutputSourceFilterAndTail;
var
  Debugger    : TDebugger;
  Server      : TMcpServer;
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath     : String;
  MapFile     : String;
  Line        : String;
  LJSON       : TJSONObject;
  ResultObj   : TJSONObject;
  ContentArr  : TJSONArray;
  Inner       : TJSONObject;
  LinesArr    : TJSONArray;
  I           : Integer;
  Src         : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 45}}}' + sLineBreak +
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
      Server.RunOnce; // set_bp 38
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue

      WaitForOutput(OutputWriter, 6);

      // Filter by stdout and take only the last 1 item.
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_output", "arguments": {"source_filter": ["stdout"], "tail": 1}}}');
      Server.RunOnce;

      Line := OutputWriter.GetLine(6);
      LJSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
      try
        ResultObj := LJSON.GetValue('result') as TJSONObject;
        ContentArr := ResultObj.GetValue('content') as TJSONArray;
        Inner := TJSONObject.ParseJSONValue(
          (ContentArr.Items[0] as TJSONObject).GetValue('text').Value) as TJSONObject;
        try
          LinesArr := Inner.GetValue('lines') as TJSONArray;
          Assert.IsNotNull(LinesArr);
          Assert.AreEqual(1, LinesArr.Count, 'get_output must return exactly 1 line due to tail=1');

          Src := (LinesArr.Items[0] as TJSONObject).GetValue('source').Value;
          Assert.AreEqual('stdout', Src, 'Expected only stdout line due to source_filter');
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

/// <summary>
///   Two consecutive breakpoints with output produced between them.
///   The first pause's <c>recent_output</c> must contain the lines
///   emitted before reaching the first BP. After continue, the second
///   pause's <c>recent_output</c> must contain ONLY the lines produced
///   between the two pauses, NOT a re-delivery of the earlier lines.
/// </summary>
procedure TMcpServerTests.TestMcpRecentOutputIsDeltaSinceLastResume;
var
  Debugger     : TDebugger;
  Server       : TMcpServer;
  InputReader  : TStringTextReader;
  OutputWriter : TStringTextWriter;
  ExePath      : String;
  MapFile      : String;

  function ParseStateRecentOutput(const ALine: String): TJSONArray;
  var
    LJSON, ResultObj, Inner: TJSONObject;
    ContentArr: TJSONArray;
  begin
    LJSON := TJSONObject.ParseJSONValue(ALine) as TJSONObject;
    try
      ResultObj := LJSON.GetValue('result') as TJSONObject;
      ContentArr := ResultObj.GetValue('content') as TJSONArray;
      Inner := TJSONObject.ParseJSONValue(
        (ContentArr.Items[0] as TJSONObject).GetValue('text').Value) as TJSONObject;
      try
        // Clone the array out so the caller can use it after we free Inner.
        var Found := Inner.GetValue('recent_output');
        if Found = nil then
          Result := nil
        else
          Result := TJSONObject.ParseJSONValue(Found.ToJSON) as TJSONArray;
      finally
        Inner.Free;
      end;
    finally
      LJSON.Free;
    end;
  end;

  function HasTextInArray(AArr: TJSONArray; const ASubstr: String): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if AArr = nil then Exit;
    for I := 0 to AArr.Count - 1 do
      if (AArr.Items[I] as TJSONObject).GetValue('text').Value.Contains(ASubstr) then
        Exit(True);
  end;

var
  Recent1: TJSONArray;
  Recent2: TJSONArray;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  MapFile := ChangeFileExt(ExePath, '.map');

  // Two breakpoints: line 17 (Writeln('Target') in TargetProcedure)
  // and line 38 (Writeln('Locals ...') in LocalsProcedure). Between
  // them the program executes the inner Writeln('Target') and
  // Writeln('Deep') calls.
  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 19}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 45}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    Debugger.LoadDebugInfoFromExe(ExePath);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp 17
      Server.RunOnce; // set_bp 38
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue (resume from initial pause)

      // Wait for first BP (line 17). 7 entries: 5 responses + stopped notification + sampling.
      WaitForOutput(OutputWriter, 7);

      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "get_state", "arguments": {}}}');
      Server.RunOnce;

      Recent1 := ParseStateRecentOutput(OutputWriter.GetLine(7));
      try
        Assert.IsNotNull(Recent1, 'First pause must include recent_output');
        Assert.IsTrue(HasTextInArray(Recent1, 'stderr-tag-line'),
          'First pause must contain the stderr-tag-line emitted before BP1');
        Assert.IsTrue(HasTextInArray(Recent1, 'ods-tag-line'),
          'First pause must contain the ods-tag-line emitted before BP1');
      finally
        if Recent1 <> nil then Recent1.Free;
      end;

      // Continue to second BP. State after: stopped at line 38.
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 7, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');
      Server.RunOnce;
      WaitForOutput(OutputWriter, 11); // +continue-resp +stopped +sampling +get_state-resp later

      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 8, "method": "tools/call", "params": {"name": "get_state", "arguments": {}}}');
      Server.RunOnce;

      // Find the latest get_state response line (last line of output).
      Recent2 := ParseStateRecentOutput(OutputWriter.GetLine(OutputWriter.GetCount - 1));
      try
        Assert.IsNotNull(Recent2,
          'Second pause must include recent_output (Target/Deep stdout lines)');
        Assert.IsTrue(HasTextInArray(Recent2, 'Target'),
          'Second pause should include the Writeln("Target") line');
        Assert.IsTrue(HasTextInArray(Recent2, 'Deep'),
          'Second pause should include the Writeln("Deep") line');
        // Critical delta property: the earlier lines must NOT reappear.
        Assert.IsFalse(HasTextInArray(Recent2, 'stderr-tag-line'),
          'stderr-tag-line was already delivered at the first pause and must not re-appear');
        Assert.IsFalse(HasTextInArray(Recent2, 'ods-tag-line'),
          'ods-tag-line was already delivered at the first pause and must not re-appear');
      finally
        if Recent2 <> nil then Recent2.Free;
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

/// <summary>
///   Covers the type matrix beyond the int/string/object trio that the
///   original evaluate tool already exercised: int64, ansistring,
///   widestring, shortstring (globals only), plus the empty-string and
///   nil-object edge cases. All seven evaluations happen in one paused
///   session at line 15 (DeepProcedure Writeln) so that the per-test
///   CreateProcess overhead is paid once.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateExtendedTypes;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;

  procedure ExpectContains(const ALine, ASubstr, ACtx: String);
  begin
    Assert.IsTrue(ALine.Contains(ASubstr),
      Format('%s: expected "%s", got: %s', [ACtx, ASubstr, ALine]));
  end;

begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // $1122334455667788 = 1234605616436508552 in decimal.
    Line := Fixture.Eval('DebugTarget.GGlobalInt64', 'int64');
    ExpectContains(Line, '1234605616436508552', 'int64');

    Line := Fixture.Eval('DebugTarget.GGlobalAnsi', 'ansistring');
    ExpectContains(Line, 'Hello Ansi', 'ansistring');

    Line := Fixture.Eval('DebugTarget.GGlobalWide', 'widestring');
    ExpectContains(Line, 'Hello Wide', 'widestring');

    Line := Fixture.Eval('DebugTarget.GGlobalShort', 'shortstring');
    ExpectContains(Line, 'Hello Short', 'shortstring');

    // Empty UnicodeString: the value part of the response is empty,
    // but the variable-name and type prefix must still be present.
    Line := Fixture.Eval('DebugTarget.GGlobalEmptyString', 'string');
    ExpectContains(Line, 'GGlobalEmptyString', 'empty string variable name');
    ExpectContains(Line, '(string)', 'empty string type tag');

    Line := Fixture.Eval('DebugTarget.GGlobalNilObject', 'object');
    ExpectContains(Line, 'nil', 'nil object');
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Local-variable ShortString case: <c>GetLocals</c> only delivers
///   8 raw bytes per slot, which would truncate any ShortString
///   longer than 7 chars. <c>EvaluateVariable</c> resolves the
///   runtime address via <c>GetLocalAddress</c> (with the Win64
///   frame-base correction) and reads the full 256-byte inline
///   buffer. Breakpoint at line 45 (Writeln in LocalsProcedure)
///   guarantees <c>LocalShort</c> has been assigned before we
///   inspect it.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateLocalShortString;
var
  Fixture : TMcpEvalFixture;
  ExePath : String;
  RespLine: String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 45);
  try
    RespLine := Fixture.Eval('LocalShort', 'shortstring');
    Assert.IsTrue(RespLine.Contains('Hello Local Short'),
      'LocalShort must evaluate to its full inline value "Hello Local Short" (>7 chars, exercises the 256-byte read path), got: ' + RespLine);
    Assert.IsTrue(RespLine.Contains('LocalShort'),
      'Response should include the variable name: ' + RespLine);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   The MCP handler must surface a clear failure when (a) the
///   variable name resolves to neither a local nor a global, and
///   (b) the type argument is not in the supported list. Both are
///   the realistic "AI agent typed something wrong" cases that
///   should yield an error the agent can act on, not a silent
///   default.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateRejectsInvalidNameAndType;
var
  Debugger    : TDebugger;
  Server      : TMcpServer;
  InputReader : TStringTextReader;
  OutputWriter: TStringTextWriter;
  ExePath     : String;
  MapFile     : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  MapFile := ChangeFileExt(ExePath, '.map');

  InputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
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
      Server.RunOnce; // set_bp
      Server.RunOnce; // ignore_exc
      Server.RunOnce; // continue
      WaitForOutput(OutputWriter, 6);

      // Unknown variable name (neither local nor global).
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "evaluate", "arguments": {"name": "ThisVariableDoesNotExist__xyz", "type": "int"}}}');
      Server.RunOnce;
      Assert.IsTrue(OutputWriter.GetLine(6).Contains('Failed to evaluate variable'),
        'Unknown name must produce a Failed-to-evaluate error: ' + OutputWriter.GetLine(6));
      Assert.IsTrue(OutputWriter.GetLine(6).Contains('isError'),
        'Unknown-name response must be flagged as an error: ' + OutputWriter.GetLine(6));

      // Known variable, but unsupported type literal. The handler must
      // also fail rather than fall through to a default decoding.
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "evaluate", "arguments": {"name": "DebugTarget.GGlobalInt", "type": "no_such_type_xyz"}}}');
      Server.RunOnce;
      Assert.IsTrue(OutputWriter.GetLine(7).Contains('Failed to evaluate variable'),
        'Unsupported type must produce a Failed-to-evaluate error: ' + OutputWriter.GetLine(7));
      Assert.IsTrue(OutputWriter.GetLine(7).Contains('isError'),
        'Unsupported-type response must be flagged as an error: ' + OutputWriter.GetLine(7));
    finally
      Server.Free;
      InputReader.Free;
      OutputWriter.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

/// <summary>
///   Single-level field navigation:
///   <c>evaluate("GGlobalOuter.FOuterInt", "int")</c> must dereference
///   the <c>GGlobalOuter</c> pointer, look up <c>FOuterInt</c>'s
///   offset from TD32 class info, and return the integer at that
///   runtime address. Same flow for <c>FOuterStr</c> (string field
///   on first level).
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateOneLevelFieldNavigation;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // FOuterInt = $11111111 = 286331153
    Line := Fixture.Eval('GGlobalOuter.FOuterInt', 'int');
    Assert.IsTrue(Line.Contains('286331153'),
      'GGlobalOuter.FOuterInt must equal 286331153 ($11111111), got: ' + Line);

    // FOuterStr = 'Hello Outer'
    Line := Fixture.Eval('GGlobalOuter.FOuterStr', 'string');
    Assert.IsTrue(Line.Contains('Hello Outer'),
      'GGlobalOuter.FOuterStr must equal "Hello Outer", got: ' + Line);

    // FOuterInner navigated as object: must return its class name
    Line := Fixture.Eval('GGlobalOuter.FOuterInner', 'object');
    Assert.IsTrue(Line.Contains('TInner'),
      'GGlobalOuter.FOuterInner must report TInner runtime class, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Two-level navigation:
///   <c>evaluate("GGlobalOuter.FOuterInner.FInnerInt", "int")</c>
///   must follow the field chain twice. The walker has to:
///   <list type="number">
///     <item>Resolve <c>GGlobalOuter</c> to its global address.</item>
///     <item>Read pointer there → instance of <c>TOuter</c>.</item>
///     <item>Determine runtime class via VMT, look up
///       <c>FOuterInner</c> offset.</item>
///     <item>Read pointer at that field → instance of <c>TInner</c>.</item>
///     <item>Determine <c>TInner</c>'s runtime class, look up
///       <c>FInnerInt</c> offset.</item>
///     <item>Read 4-byte int at that final address.</item>
///   </list>
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateTwoLevelFieldNavigation;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // FInnerInt = $22222222 = 572662306
    Line := Fixture.Eval('GGlobalOuter.FOuterInner.FInnerInt', 'int');
    Assert.IsTrue(Line.Contains('572662306'),
      'GGlobalOuter.FOuterInner.FInnerInt must equal 572662306 ($22222222), got: ' + Line);

    // FInnerStr = 'Hello Inner'
    Line := Fixture.Eval('GGlobalOuter.FOuterInner.FInnerStr', 'string');
    Assert.IsTrue(Line.Contains('Hello Inner'),
      'GGlobalOuter.FOuterInner.FInnerStr must equal "Hello Inner", got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   When a field in the chain is nil, the walker must surface
///   <c>"nil"</c> as the value (for the final segment) rather than
///   dereferencing a null pointer or returning random garbage. Uses
///   <c>GGlobalNilObject</c> as the trivial nil case (a global
///   <c>TObject</c> set to nil).
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateFieldNavigationOnNilMidChain;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // GGlobalNilObject is nil. Asking for it as object directly (no
    // navigation needed) must say "nil".
    Line := Fixture.Eval('DebugTarget.GGlobalNilObject', 'object');
    Assert.IsTrue(Line.Contains('nil'),
      'GGlobalNilObject must evaluate to nil, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   When a navigated field name doesn't exist on the resolved class,
///   the handler must surface <c>"Failed to evaluate variable"</c>
///   so the AI agent can detect typos rather than reading random
///   nearby memory.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateFieldNavigationUnknownField;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // FNoSuch is not declared on TOuter; the walker should fail.
    Line := Fixture.Eval('GGlobalOuter.FNoSuchField', 'int');
    Assert.IsTrue(Line.Contains('Failed to evaluate variable'),
      'Unknown field must produce a Failed-to-evaluate error: ' + Line);
    Assert.IsTrue(Line.Contains('isError'),
      'Unknown-field response must be flagged as an error: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Record-typed fields embedded in a class are walked WITHOUT
///   dereferencing the record's slot (the record sits inline within
///   the holding class). This test covers the basic
///   class → record hop:
///   <c>GGlobalWithRec.FOrigin.FX</c> / <c>.FY</c> where
///   <c>FOrigin</c> is a <c>TPoint2D</c> record.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateRecordFieldNavigation;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // FOrigin.FX = $11111111 = 286331153
    Line := Fixture.Eval('GGlobalWithRec.FOrigin.FX', 'int');
    Assert.IsTrue(Line.Contains('286331153'),
      'GGlobalWithRec.FOrigin.FX must equal 286331153 ($11111111), got: ' + Line);

    // FOrigin.FY = $22222222 = 572662306. The +4 offset within the
    // record proves the navigator advanced inline rather than
    // dereferencing the record slot as a pointer.
    Line := Fixture.Eval('GGlobalWithRec.FOrigin.FY', 'int');
    Assert.IsTrue(Line.Contains('572662306'),
      'GGlobalWithRec.FOrigin.FY must equal 572662306 ($22222222), got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Records nested within records: <c>TRect2D</c> contains two
///   <c>TPoint2D</c> fields, each holding two integers. Walking
///   <c>GGlobalWithRec.FBounds.FTopLeft.FX</c> must take three hops
///   total: one class → record (<c>FBounds</c>), one
///   record → record (<c>FTopLeft</c> inside <c>FBounds</c>), and
///   one record → primitive (<c>FX</c> inside <c>FTopLeft</c>). Two
///   of the four sub-fields are checked to verify offsets at both
///   top-left and bottom-right of the rect.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateNestedRecordFieldNavigation;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // FBounds.FTopLeft.FX = $33333333 = 858993459
    Line := Fixture.Eval('GGlobalWithRec.FBounds.FTopLeft.FX', 'int');
    Assert.IsTrue(Line.Contains('858993459'),
      'GGlobalWithRec.FBounds.FTopLeft.FX must equal 858993459 ($33333333), got: ' + Line);

    // FBounds.FTopLeft.FY = $44444444 = 1145324612
    Line := Fixture.Eval('GGlobalWithRec.FBounds.FTopLeft.FY', 'int');
    Assert.IsTrue(Line.Contains('1145324612'),
      'GGlobalWithRec.FBounds.FTopLeft.FY must equal 1145324612 ($44444444), got: ' + Line);

    // FBounds.FBottomRight.FX = $55555555 = 1431655765. Validates the
    // record-to-record offset: FBottomRight starts 8 bytes into FBounds.
    Line := Fixture.Eval('GGlobalWithRec.FBounds.FBottomRight.FX', 'int');
    Assert.IsTrue(Line.Contains('1431655765'),
      'GGlobalWithRec.FBounds.FBottomRight.FX must equal 1431655765 ($55555555), got: ' + Line);

    // FBounds.FBottomRight.FY = $66666666 = 1717986918
    Line := Fixture.Eval('GGlobalWithRec.FBounds.FBottomRight.FY', 'int');
    Assert.IsTrue(Line.Contains('1717986918'),
      'GGlobalWithRec.FBounds.FBottomRight.FY must equal 1717986918 ($66666666), got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   A record may itself hold a class-typed field: <c>TPair</c>
///   contains <c>FObj</c> of type <c>TInner</c>. Navigation through
///   <c>GGlobalWithRec.FPair.FObj.FInnerInt</c> must transition from
///   the record hop (no deref) BACK to the class hop (deref + VMT
///   walk) at <c>FObj</c>. Also verifies a string field of the
///   record (<c>FLabel</c>) for an inline-string read.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateRecordToClassTransition;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // FPair.FObj.FInnerInt = $77777777 = 2004318071. This proves
    // record -> class transition works: the navigator must deref FObj
    // (a class slot inside a record) before looking up FInnerInt.
    Line := Fixture.Eval('GGlobalWithRec.FPair.FObj.FInnerInt', 'int');
    Assert.IsTrue(Line.Contains('2004318071'),
      'GGlobalWithRec.FPair.FObj.FInnerInt must equal 2004318071 ($77777777), got: ' + Line);

    // FPair.FObj.FInnerStr = 'Inner via Pair'
    Line := Fixture.Eval('GGlobalWithRec.FPair.FObj.FInnerStr', 'string');
    Assert.IsTrue(Line.Contains('Inner via Pair'),
      'GGlobalWithRec.FPair.FObj.FInnerStr must equal "Inner via Pair", got: ' + Line);

    // FPair.FLabel = 'PairLabel'. Pure record path (no class transition);
    // verifies that string fields work under the record-hop branch too.
    Line := Fixture.Eval('GGlobalWithRec.FPair.FLabel', 'string');
    Assert.IsTrue(Line.Contains('PairLabel'),
      'GGlobalWithRec.FPair.FLabel must equal "PairLabel", got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Asking for a non-existent field on a record (as opposed to a
///   class) must produce the same Failed-to-evaluate error path.
///   Without this guard, a bad field name on a record could silently
///   advance by 0 and read the start of the record as the requested
///   type.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateRecordUnknownFieldFails;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // FNoSuch is not declared on TPoint2D; the record-hop must fail.
    Line := Fixture.Eval('GGlobalWithRec.FOrigin.FNoSuchField', 'int');
    Assert.IsTrue(Line.Contains('Failed to evaluate variable'),
      'Unknown record field must produce a Failed-to-evaluate error: ' + Line);
    Assert.IsTrue(Line.Contains('isError'),
      'Unknown-record-field response must be flagged as an error: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   The first segment of a dotted path can be a global variable
///   whose declared type IS a record (no enclosing class). The walk
///   must treat that first hop as inline -- NOT dereference it like
///   an object pointer -- and look up the field directly via the
///   record's type registry.
/// </summary>
/// <remarks>
///   DebugTarget declares two such globals: <c>GGlobalMixed</c> of
///   type <c>TMixedRec</c> and <c>GGlobalP3D</c> of type
///   <c>TPoint3D</c>. Both are initialised with sentinel values the
///   test cross-references via the typed evaluate.
///
///   Pre-fix this test fails because the evaluate dotted-walk
///   unconditionally enters its class-hop branch on the first
///   segment, reading the record's first 4-8 bytes as if they were a
///   VMT pointer to an instance.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateGlobalRecordFieldNavigation;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // GGlobalMixed.FMixedInt = Integer($A1A1A1A1) sign-extends to
    // -1583242847.
    Line := Fixture.Eval('GGlobalMixed.FMixedInt', 'int');
    Assert.IsTrue(Line.Contains('-1583242847'),
      'GGlobalMixed.FMixedInt must equal -1583242847 ($A1A1A1A1), got: ' + Line);

    // GGlobalMixed.FMixedStr is a string at offset 16 (after Int +
    // 4 padding + Int64). Reading as a Delphi string evaluates the
    // pointer at that slot and dereferences it -- a real string
    // pointer here proves the field offset arithmetic is correct.
    Line := Fixture.Eval('GGlobalMixed.FMixedStr', 'string');
    Assert.IsTrue(Line.Contains('Mixed-A3'),
      'GGlobalMixed.FMixedStr must equal "Mixed-A3", got: ' + Line);

    // GGlobalP3D.FY at offset 4 (TPoint3D = record FX, FY, FZ: Integer).
    // Value $B2B2B2B2 = -1296911694 signed.
    Line := Fixture.Eval('GGlobalP3D.FY', 'int');
    Assert.IsTrue(Line.Contains('-1296911694'),
      'GGlobalP3D.FY must equal -1296911694 ($B2B2B2B2), got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Mirrors the "TRecHeader prefix" pattern observed in real-world
///   Delphi binaries (TFW). <c>TWithHeader</c>'s layout is:
///   <list type="bullet">
///     <item><c>WhdrHeader: TWhdrHeader</c> -- 8 bytes (Magic + Ver
///       at offset 0)</item>
///     <item><c>WhdrId: Integer</c> -- offset 8</item>
///     <item><c>WhdrShortStr: ShortString</c> -- offset 12,
///       length-prefixed inline</item>
///     <item><c>WhdrLongStr: string</c> -- immediately after the
///       ShortString</item>
///   </list>
///   A correct evaluator MUST compute the field offset of
///   <c>WhdrId</c> as 8, NOT 0 -- offset 0 would re-read
///   <c>WhdrMagic</c> (<c>$E1E1E1E1</c>) and silently "succeed" with
///   the wrong value.
/// </summary>
/// <remarks>
///   The bug class this test was written to catch on TFW
///   (<c>AppCaps.DbKindName</c> returned ok=False even though the
///   record was resolved correctly) is reproduced here without
///   needing the TFW binary, so the fix can be validated against a
///   checked-in fixture.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateHeaderPrefixedRecord;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // WhdrId at offset 8 = Integer($E3E3E3E3) = -471604253.
    // A scanner that puts WhdrId at offset 0 would return
    // $E1E1E1E1 = -505290017 (WhdrMagic). The two sentinels are
    // intentionally distinct so a wrong-offset bug surfaces.
    Line := Fixture.Eval('GGlobalWithHeader.WhdrId', 'int');
    Assert.IsTrue(Line.Contains('-471604253'),
      'GGlobalWithHeader.WhdrId must equal -471604253 ($E3E3E3E3), got: ' + Line);

    // Nested record-in-record: WhdrHeader.WhdrVer at offset 4
    // (within WhdrHeader at offset 0 of TWithHeader). $E2E2E2E2
    // as signed Int32 = -488447262. Wrong offsets here would
    // return WhdrMagic ($E1E1E1E1) or random garbage further in.
    // This case used to fail BEFORE the Format-A field-record
    // structural-anchor fix: WhdrHeader's TypeIdx wasn't being
    // linked back to TWhdrHeader (the F-prefix gate dropped
    // non-F-prefixed field-record entries), so the dotted-walk
    // couldn't transition from TWithHeader into TWhdrHeader.
    Line := Fixture.Eval('GGlobalWithHeader.WhdrHeader.WhdrVer', 'int');
    Assert.IsTrue(Line.Contains('-488447262'),
      'GGlobalWithHeader.WhdrHeader.WhdrVer must equal -488447262 ($E2E2E2E2), got: ' + Line);

    // ShortString past the header AND past WhdrId. Verifies the
    // offset arithmetic stays correct across non-uniform field
    // widths and that the inline-shortstring read at FieldAddr
    // sees the right buffer.
    Line := Fixture.Eval('GGlobalWithHeader.WhdrShortStr', 'shortstring');
    Assert.IsTrue(Line.Contains('whdr-short'),
      'GGlobalWithHeader.WhdrShortStr must equal "whdr-short", got: ' + Line);

    // Long string (Delphi default UnicodeString): the slot at
    // its computed offset must hold a real string pointer; a
    // wrong offset would read a garbage 4 bytes and dereferencing
    // them as a string heap-walk would fail.
    Line := Fixture.Eval('GGlobalWithHeader.WhdrLongStr', 'string');
    Assert.IsTrue(Line.Contains('Whdr-Long-E5'),
      'GGlobalWithHeader.WhdrLongStr must equal "Whdr-Long-E5", got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Mirrors the "case Byte of" variant pattern from TFW's
///   <c>TKonsApl</c>. <c>TVarRec</c> layout is:
///   <list type="bullet">
///     <item><c>VrCommon: Integer</c> -- offset 0, size 4</item>
///     <item><c>case Byte of</c>
///       <list type="bullet">
///         <item>0: <c>(VrAsInt: Integer)</c> -- overlay at offset 4,
///           size 4</item>
///         <item>1: <c>(VrAsByteA..D: Byte)</c> -- overlay at offset
///           4, four bytes</item>
///       </list>
///     </item>
///   </list>
///   The RSM emits each variant case as a <c>$02</c>-prefixed field
///   record but with overlapping offsets (next-offset DWORD does NOT
///   advance between siblings of different variant branches). A
///   correct scanner must:
///   <list type="bullet">
///     <item>surface every named variant field with its own offset
///       (so dotted paths into either branch resolve correctly),
///       AND</item>
///     <item>leave <c>VrCommon</c> at offset 0 with <c>VrAsInt</c>
///       at offset 4 (NOT at some accumulated offset 8/12/16 if the
///       scanner naively treated all field records as sequential).</item>
///   </list>
/// </summary>
/// <remarks>
///   If the scanner mishandles the variant overlay, <c>VrAsInt</c>'s
///   read would land in the wrong memory and the assertion would
///   fail with random bytes instead of the <c>$F2F2F2F2</c>
///   sentinel.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateVariantRecord;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // VrCommon at offset 0 = Integer($F0F0F0F0) = -252645136.
    // This part is identical to a non-variant record and is the
    // baseline -- if it fails, the bug isn't variant-specific.
    Line := Fixture.Eval('GGlobalVarRec.VrCommon', 'int');
    Assert.IsTrue(Line.Contains('-252645136'),
      'GGlobalVarRec.VrCommon must equal -252645136 ($F0F0F0F0), got: ' + Line);

    // VrAsInt at offset 4 = Integer($F2F2F2F2) = -218959118.
    // This is the variant-overlay assertion: if the scanner did
    // NOT mark VrAsInt as overlaying VrCommon's tail, the field
    // table would put it past the variant union (offset 8 or
    // further) and the read would return zeros / garbage.
    Line := Fixture.Eval('GGlobalVarRec.VrAsInt', 'int');
    Assert.IsTrue(Line.Contains('-218959118'),
      'GGlobalVarRec.VrAsInt must equal -218959118 ($F2F2F2F2), got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Sub-DWORD fields (Word, Byte) read via <c>type:"int"</c> must
///   be width-clamped to the field's declared size. Without the
///   clamp, the read pulls 4 bytes and visibly concatenates the
///   next field's bytes into the high part of the result -- the
///   bug that turned <c>MdtGlobal.Id</c> (Word = 1) into 65537 on
///   TFW because the adjacent <c>IdUmsatz</c> (= 1) leaked into
///   bits 16-31.
/// </summary>
/// <remarks>
///   <c>TNarrowInts</c> mirrors the TMdt prefix layout without
///   needing the TFW binary: two adjacent Words then two adjacent
///   Bytes, each with a distinct sentinel so a wrong-width read is
///   visibly different from a correct one. The field-size info
///   comes from the offset gap between consecutive members
///   (populated by <c>ScanFieldsForwardFromRecordName</c>) and is
///   propagated from the dotted-walk into
///   <c>EvaluateVariable</c>'s int formatter as
///   <c>FieldKnownSize</c>.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateNarrowIntFieldWidth;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // NiWord (Word at offset 0) = 1. Naive 4-byte read would
    // return 1 or ($1234 shl 16) = $12340001 = 305397761.
    // Width-clamped read returns 1.
    Line := Fixture.Eval('GGlobalNarrow.NiWord', 'int');
    Assert.IsTrue(Line.Contains(': 1"'),
      'GGlobalNarrow.NiWord must equal 1 (width-clamped read), got: ' + Line);
    Assert.IsFalse(Line.Contains('305397761'),
      'NiWord must NOT carry NiWord2''s bytes into the high part: ' + Line);

    // NiWord2 (Word at offset 2) = $1234 = 4660.
    Line := Fixture.Eval('GGlobalNarrow.NiWord2', 'int');
    Assert.IsTrue(Line.Contains('4660'),
      'GGlobalNarrow.NiWord2 must equal 4660 ($1234), got: ' + Line);

    // NiByte (Byte at offset 4) = $A5 = 165. Naive 4-byte read
    // would mix in NiByte2 ($5A) and NiInteger's low bytes.
    Line := Fixture.Eval('GGlobalNarrow.NiByte', 'int');
    Assert.IsTrue(Line.Contains('165'),
      'GGlobalNarrow.NiByte must equal 165 ($A5), got: ' + Line);

    // NiByte2 (Byte at offset 5) = $5A = 90.
    Line := Fixture.Eval('GGlobalNarrow.NiByte2', 'int');
    Assert.IsTrue(Line.Contains(': 90"'),
      'GGlobalNarrow.NiByte2 must equal 90 ($5A), got: ' + Line);

    // NiInteger (Integer at offset 6) = $DEADBEEF = -559038737.
    // Last member in the record -- its Size derivation from the
    // offset chain is undefined (no next member), so the clamp
    // must NOT shorten the read here. Acts as a regression guard
    // against accidentally clamping a last-member full-width
    // field down to 0 or a stale size.
    Line := Fixture.Eval('GGlobalNarrow.NiInteger', 'int');
    Assert.IsTrue(Line.Contains('-559038737'),
      'GGlobalNarrow.NiInteger must equal -559038737 ($DEADBEEF) -- ' +
      'last-member field must not be clamped, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Dotted-walk navigation through a register-passed class pointer.
///   Inside <c>TouchRegClassParam</c>, both parameters
///   (<c>AInner</c>, <c>AOther</c>) are class references that live in
///   CPU registers with NO stack slot (Delphi register call: EAX/EDX
///   on x86, RCX/RDX on x64). Today the dotted-walk routes through
///   <c>GetLocalAddress</c>, which ignores <c>lkRegister</c> and
///   returns a junk base-pointer offset -- the subsequent
///   <c>ReadTargetPointer</c> then dereferences a stack address and
///   reads garbage as an instance pointer. The fix must source the
///   instance pointer directly from the register file.
/// </summary>
/// <remarks>
///   <para>Self (an instance method's implicit first arg) and a
///   class-typed parameter share the same register-call mechanics, so
///   the underlying bug AND its fix are identical. We exercise it via
///   a plain procedure with two class parameters instead of a class
///   method, to keep the RSM reader's class-discovery heuristic on
///   the path the existing tests cover.</para>
///   <para>The whole-name <c>evaluate AInner</c> case (no dot) is
///   already covered by other tests; this fixture targets the dotted
///   case <c>AInner.FInnerInt</c>, which is the one that fails today.</para>
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateClassFieldThroughRegisterParam;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 231);
  try
    // AInner = GGlobalOuter.FOuterInner -- TInner with
    // FInnerInt = $22222222 = 572662306 and FInnerStr = 'Hello Inner'.
    // First register slot (EAX/RCX). Without the fix, the dotted-walk
    // dereferences a stack address and reads junk.
    Line := Fixture.Eval('AInner.FInnerInt', 'int');
    Assert.IsTrue(Line.Contains('572662306'),
      'AInner.FInnerInt must equal 572662306 ($22222222) -- register-passed ' +
      'class pointer must source instance from register, not BP slot, got: ' + Line);

    Line := Fixture.Eval('AInner.FInnerStr', 'string');
    Assert.IsTrue(Line.Contains('Hello Inner'),
      'AInner.FInnerStr must equal "Hello Inner", got: ' + Line);

    // AOther = GGlobalDerived -- TDerived inherits from TInner with
    // FInnerInt = $C1C1C1C1 = -1044266559 and FInnerStr = 'Inherited Inner'.
    // Second register slot (EDX/RDX). Proves the fix isn't first-slot
    // specific. Inherited-field lookup also rides this code path.
    Line := Fixture.Eval('AOther.FInnerInt', 'int');
    Assert.IsTrue(Line.Contains('-1044266559'),
      'AOther.FInnerInt must equal -1044266559 ($C1C1C1C1, inherited from TInner), got: ' + Line);

    Line := Fixture.Eval('AOther.FInnerStr', 'string');
    Assert.IsTrue(Line.Contains('Inherited Inner'),
      'AOther.FInnerStr must equal "Inherited Inner" (inherited), got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Self-field navigation inside an instance method. Driving the
///   debugger to the first statement of <c>TDerived.TouchSelf</c>
///   gives us a receiver pointer that lives in the first register
///   slot (EAX on x86, RCX on x64) with NO stack slot, exactly like
///   <c>TFormMain.Create</c> in the user's real-world TFW scenario.
///   Covers both:
///   <list type="bullet">
///     <item>Own fields of the receiver (<c>Self.FDerivedExtra</c>,
///       <c>Self.FDerivedLabel</c>) -- requires TDerived to be in
///       the RSM class index.</item>
///     <item>Inherited fields (<c>Self.FInnerInt</c>,
///       <c>Self.FInnerStr</c>) -- requires the inheritance walk
///       in <c>FindClassMember</c> to traverse TDerived ->
///       TInner.</item>
///   </list>
///   This test exposes a class-discovery gap distinct from the
///   register-Self pointer-source fix: today, declaring a method on
///   a class shifts its RSM type-record shape enough to make the
///   reader's class-signature heuristic miss it, so even with the
///   pointer available the field lookup fails.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateSelfFieldInsideInstanceMethod;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 241);
  try
    // Whole-name Self resolves via the existing register-local read
    // path (Locals[].RawBytes already holds the EAX value). Acts as a
    // sanity check that the BP fired inside TouchSelf with Self live
    // in the first register slot.
    Line := Fixture.Eval('Self', 'object');
    Assert.IsTrue(Line.Contains('TDerived'),
      'Self must report TDerived runtime class, got: ' + Line);

    // Self.FDerivedExtra = $C2C2C2C2 = -1027423550 (own field).
    Line := Fixture.Eval('Self.FDerivedExtra', 'int');
    Assert.IsTrue(Line.Contains('-1027423550'),
      'Self.FDerivedExtra must equal -1027423550 ($C2C2C2C2), got: ' + Line);

    // Self.FDerivedLabel = 'Derived-C3' (own string field).
    Line := Fixture.Eval('Self.FDerivedLabel', 'string');
    Assert.IsTrue(Line.Contains('Derived-C3'),
      'Self.FDerivedLabel must equal "Derived-C3", got: ' + Line);

    // Self.FInnerInt = $C1C1C1C1 = -1044266559 (inherited from
    // TInner; proves the inheritance walk works through the
    // register-Self pointer-source path).
    Line := Fixture.Eval('Self.FInnerInt', 'int');
    Assert.IsTrue(Line.Contains('-1044266559'),
      'Self.FInnerInt must equal -1044266559 ($C1C1C1C1, inherited), got: ' + Line);

    // Self.FInnerStr = 'Inherited Inner' (inherited string field).
    Line := Fixture.Eval('Self.FInnerStr', 'string');
    Assert.IsTrue(Line.Contains('Inherited Inner'),
      'Self.FInnerStr must equal "Inherited Inner" (inherited), got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Multi-level inheritance navigation. <c>GGlobalDeep</c> is a
///   <c>TDeepDerived</c> instance whose class hierarchy is:
///   <c>TDeepDerived -&gt; TDerived -&gt; TInner -&gt; TObject</c>.
///   Resolving <c>GGlobalDeep.FInnerInt</c> requires
///   <c>FindClassMember</c> to walk TWO ancestor links to reach the
///   field's owning class (TInner). A reliable ancestor walk needs
///   each link in the chain to be encoded via the RSM-supplied
///   parent reference, not the offset-collision heuristic that
///   silently leaves <c>ParentName</c> empty whenever two unrelated
///   classes happen to end at the same instance offset.
/// </summary>
/// <remarks>
///   Covers each level of the chain so the failure mode tells us
///   precisely where the recursion stops:
///   <list type="bullet">
///     <item><c>FDeepFlag</c> -- own field of TDeepDerived, no
///       walk needed.</item>
///     <item><c>FDerivedExtra</c> / <c>FDerivedLabel</c> -- one
///       hop up to TDerived.</item>
///     <item><c>FInnerInt</c> / <c>FInnerStr</c> -- two hops up to
///       TInner.</item>
///   </list>
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateMultiLevelInheritedField;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // FDeepFlag = $D3D3D3D3 = -741092397 (own field, no inheritance walk).
    Line := Fixture.Eval('GGlobalDeep.FDeepFlag', 'int');
    Assert.IsTrue(Line.Contains('-741092397'),
      'GGlobalDeep.FDeepFlag must equal -741092397 ($D3D3D3D3, own field), got: ' + Line);

    // FDerivedExtra = $D2D2D2D2 = -757935406 (one inheritance hop:
    // TDeepDerived -> TDerived).
    Line := Fixture.Eval('GGlobalDeep.FDerivedExtra', 'int');
    Assert.IsTrue(Line.Contains('-757935406'),
      'GGlobalDeep.FDerivedExtra must equal -757935406 ($D2D2D2D2, ' +
      'inherited from TDerived), got: ' + Line);

    // FDerivedLabel = 'Deep Derived' (one inheritance hop).
    Line := Fixture.Eval('GGlobalDeep.FDerivedLabel', 'string');
    Assert.IsTrue(Line.Contains('Deep Derived'),
      'GGlobalDeep.FDerivedLabel must equal "Deep Derived" (inherited), got: ' + Line);

    // FInnerInt = $D1D1D1D1 = -774778415 (two inheritance hops:
    // TDeepDerived -> TDerived -> TInner).
    Line := Fixture.Eval('GGlobalDeep.FInnerInt', 'int');
    Assert.IsTrue(Line.Contains('-774778415'),
      'GGlobalDeep.FInnerInt must equal -774778415 ($D1D1D1D1, two hops ' +
      'up to TInner), got: ' + Line);

    // FInnerStr = 'Deep Inner' (two inheritance hops).
    Line := Fixture.Eval('GGlobalDeep.FInnerStr', 'string');
    Assert.IsTrue(Line.Contains('Deep Inner'),
      'GGlobalDeep.FInnerStr must equal "Deep Inner" (two hops up), got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Inherited-field navigation through an RTL ancestor.
///   <c>TMyComp</c> inherits <c>System.Classes.TComponent</c>, so
///   <c>AComp.FCustomFlag</c> is the leaf class's own field while
///   <c>AComp.FName</c> and <c>AComp.FTag</c> live on TComponent
///   itself. The same shape applies to the user's real-world TFW
///   scenario where <c>TFormMain.FTag</c> / <c>TFormMain.FName</c>
///   resolve through several VCL ancestors down to TComponent.
/// </summary>
/// <remarks>
///   FName is the field backing the <c>Name</c> property; FTag
///   backs <c>Tag</c>. Both are private on TComponent but the RSM
///   emits them. The walk must:
///   <list type="number">
///     <item>Discover TComponent in <c>FClasses</c>.</item>
///     <item>Hook TMyComp's <c>ParentName</c> chain up to
///       TComponent (possibly through TPersistent in between).</item>
///     <item>Recurse through <c>FindClassMember</c> until a field
///       with the given name is found.</item>
///   </list>
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateRtlInheritedField;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 209);
  try
    // Sanity: AComp is the live TMyComp instance.
    Line := Fixture.Eval('AComp', 'object');
    Assert.IsTrue(Line.Contains('TMyComp'),
      'AComp must report TMyComp runtime class, got: ' + Line);

    // AComp.FCustomFlag = $7E7E7E7E = 2122219134 (own field of TMyComp).
    Line := Fixture.Eval('AComp.FCustomFlag', 'int');
    Assert.IsTrue(Line.Contains('2122219134'),
      'AComp.FCustomFlag must equal 2122219134 ($7E7E7E7E, own field), got: ' + Line);

    // AComp.FName = 'CompName' (inherited from TComponent via TPersistent).
    Line := Fixture.Eval('AComp.FName', 'string');
    Assert.IsTrue(Line.Contains('CompName'),
      'AComp.FName must equal "CompName" (inherited from TComponent), got: ' + Line);

    // AComp.FTag = $7D7D7D7D = 2105376125 (inherited from TComponent).
    Line := Fixture.Eval('AComp.FTag', 'int');
    Assert.IsTrue(Line.Contains('2105376125'),
      'AComp.FTag must equal 2105376125 ($7D7D7D7D, inherited from TComponent), got: ' + Line);

    // Also reachable through the global handle (GetAddressFromSymbol
    // path), which exercises the same ancestor walk via a different
    // first-segment resolution.
    Line := Fixture.Eval('GGlobalComp.FName', 'string');
    Assert.IsTrue(Line.Contains('CompName'),
      'GGlobalComp.FName must equal "CompName" via global handle, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Inherited-field navigation when the receiver class declares
///   NO own fields. <c>TEmptyChild = class(TComponent)</c> with an
///   empty body means <c>FirstOffs[TEmptyChild] = 0</c>, so the
///   offset-matching parent heuristic in <c>DeriveClassParents</c>
///   (including the tolerant variant) skips the class outright and
///   leaves <c>ParentName</c> empty. The only way to surface
///   <c>FName</c> / <c>FTag</c> from TComponent is to walk the live
///   VMT chain at runtime: read the receiver's VMT, follow the
///   <c>vmtParent</c> slot up to TComponent, and look the field up
///   there.
/// </summary>
/// <remarks>
///   Mirrors a TFW pattern where intermediate VCL ancestors
///   (CMainForm, CBaseForm, TCustomForm, ...) may declare no own
///   instance fields the RSM scanner can latch onto, so the
///   chain must be reconstructed from the runtime VMT.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateInheritedFieldViaVmtWalk;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 217);
  try
    // Sanity: AEmpty resolves as a TEmptyChild instance.
    Line := Fixture.Eval('AEmpty', 'object');
    Assert.IsTrue(Line.Contains('TEmptyChild'),
      'AEmpty must report TEmptyChild runtime class, got: ' + Line);

    // AEmpty.FName = 'EmptyName' (inherited from TComponent; the
    // receiver class declares no own fields, so this resolution
    // MUST go through the live-VMT ancestor walk).
    Line := Fixture.Eval('AEmpty.FName', 'string');
    Assert.IsTrue(Line.Contains('EmptyName'),
      'AEmpty.FName must equal "EmptyName" via VMT-walk to TComponent, got: ' + Line);

    // AEmpty.FTag = $7C7C7C7C = 2088533116 (inherited from TComponent).
    Line := Fixture.Eval('AEmpty.FTag', 'int');
    Assert.IsTrue(Line.Contains('2088533116'),
      'AEmpty.FTag must equal 2088533116 ($7C7C7C7C, inherited from TComponent), got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   IEEE 754 float field navigation. Sentinels are powers-of-2
///   sums (1.5 = 1 + 0.5, 2.25 = 2 + 0.25, 3.125 = 3 + 0.125)
///   so they're exactly representable in <c>Single</c>,
///   <c>Double</c> and <c>Extended</c> -- the round-trip from
///   source-level literal through IEEE 754 binary back through the
///   formatter must yield the literal string. The evaluator's
///   float formatters MUST use invariant FormatSettings (period
///   as decimal separator); otherwise the test fails on German
///   locale runtime which would emit "1,5" instead of "1.5".
/// </summary>
/// <remarks>
///   <c>Extended</c> is 80-bit (10 bytes) on Win32 and aliased to
///   <c>Double</c> (8 bytes) on Win64. The formatter must consult
///   the target architecture to decide whether to re-read 10 bytes
///   from the field address (Win32) or just consume the 8-byte
///   slot that GetLocals / the dotted walk already provides
///   (Win64).
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateFloatTypes;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    Line := Fixture.Eval('GGlobalFloats.FSingle', 'single');
    Assert.IsTrue(Line.Contains('1.5'),
      'GGlobalFloats.FSingle must equal 1.5, got: ' + Line);

    Line := Fixture.Eval('GGlobalFloats.FDouble', 'double');
    Assert.IsTrue(Line.Contains('2.25'),
      'GGlobalFloats.FDouble must equal 2.25, got: ' + Line);

    Line := Fixture.Eval('GGlobalFloats.FExtended', 'extended');
    Assert.IsTrue(Line.Contains('3.125'),
      'GGlobalFloats.FExtended must equal 3.125, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Auto-type-detection: when the <c>type</c> argument is omitted
///   from the <c>evaluate</c> request, the server picks the formatter
///   from the field's RSM-derived type id. Covers the major primitive
///   classes (Integer / Word / Double / string / class-pointer) plus
///   the existing structured-type fast-path (record / class), all
///   reachable through the same dispatch table the explicit-type
///   path uses.
/// </summary>
/// <remarks>
///   Each assertion compares the auto-detected output against the
///   explicit-type baseline so a regression in auto-detection cannot
///   silently fall through to a wrong-but-non-empty formatter.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateAutoTypeDetection;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // Integer (4-byte signed) — record-field path.
    Line := Fixture.EvalAuto('GGlobalMixed.FMixedInt');
    Assert.IsTrue(Line.Contains('-1583242847'),
      'auto-detect must format GGlobalMixed.FMixedInt as int ' +
      '($A1A1A1A1 = -1583242847), got: ' + Line);

    // Word (2-byte, requires Member.Size width clamp).
    Line := Fixture.EvalAuto('GGlobalNarrow.NiWord');
    Assert.IsTrue(Line.Contains(': 1"'),
      'auto-detect must format GGlobalNarrow.NiWord as int=1, got: ' + Line);

    // string (UnicodeString) — pointer with -4 length prefix.
    Line := Fixture.EvalAuto('GGlobalMixed.FMixedStr');
    Assert.IsTrue(Line.Contains('Mixed-A3'),
      'auto-detect must format GGlobalMixed.FMixedStr as string ' +
      '"Mixed-A3", got: ' + Line);

    // Double — IEEE 754 64-bit.
    Line := Fixture.EvalAuto('GGlobalFloats.FDouble');
    Assert.IsTrue(Line.Contains('2.25'),
      'auto-detect must format GGlobalFloats.FDouble as double=2.25, got: ' + Line);

    // class instance pointer — VMT walk yields "ClassName @ HexAddr".
    Line := Fixture.EvalAuto('GGlobalOuter');
    Assert.IsTrue(Line.Contains('TOuter @ '),
      'auto-detect must format GGlobalOuter as object "TOuter @ <hex>", got: ' + Line);

    // Dotted class-typed terminal field — terminal Member.TypeIdx must
    // resolve through the RSM-id class registry. Without that path,
    // real-world dotted walks like "Self.FNotification" never get a
    // formatter assigned and surface as "Failed to evaluate".
    Line := Fixture.EvalAuto('GGlobalOuter.FOuterInner');
    Assert.IsTrue(Line.Contains('TInner @ '),
      'auto-detect must format GGlobalOuter.FOuterInner as object "TInner @ <hex>", got: ' + Line);

    // Whole-name global typed as an RTL-defined class (TStringList lives
    // in System.Classes, not in the program's own RSM type-registry).
    // The previous Path 4 (FindGlobalTypeIdx -> registry) misses these
    // because RTL types aren't registered there. The class-pointer
    // probe fallback recovers them by reading the VMT classname --
    // same mechanism that fixes "Application" / "Self.<RtlField>".
    Line := Fixture.EvalAuto('GGlobalObject');
    Assert.IsTrue(Line.Contains('TStringList @ '),
      'auto-detect must format GGlobalObject as object "TStringList @ <hex>", got: ' + Line);

    // Nil class pointer: auto-detection cannot disambiguate "nil
    // object" from "Integer 0" without type info, so the probe
    // declines. The failure message must surface the AHint suggesting
    // explicit types instead of bailing with an opaque "Failed".
    Line := Fixture.EvalAuto('GGlobalNilObject');
    Assert.IsTrue(Line.Contains('Failed to evaluate variable') and
                  Line.Contains('type=object') and
                  Line.Contains('nil pointer'),
      'auto-detect failure on a nil class global must include the ' +
      'type-suggestion hint, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Auto-type-detection coverage for the primitive types the
///   smaller test (<c>TestMcpEvaluateAutoTypeDetection</c>) didn't
///   exercise: AnsiString, WideString, ShortString, Boolean, and
///   Currency. Each ID needs an entry in
///   <c>FPrimitiveTypeFormatters</c>; missing IDs surface here as a
///   "Failed to evaluate variable" error.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateAutoTypeDetectionMoreTypes;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    Line := Fixture.EvalAuto('GGlobalPrim.FAnsi');
    Assert.IsTrue(Line.Contains('Prim-Ansi'),
      'auto-detect must format FAnsi as ansistring "Prim-Ansi", got: ' + Line);

    Line := Fixture.EvalAuto('GGlobalPrim.FWide');
    Assert.IsTrue(Line.Contains('Prim-Wide'),
      'auto-detect must format FWide as widestring "Prim-Wide", got: ' + Line);

    Line := Fixture.EvalAuto('GGlobalPrim.FShort');
    Assert.IsTrue(Line.Contains('Prim-Short'),
      'auto-detect must format FShort as shortstring "Prim-Short", got: ' + Line);

    Line := Fixture.EvalAuto('GGlobalPrim.FBool');
    Assert.IsTrue(Line.Contains('True'),
      'auto-detect must format FBool as bool "True", got: ' + Line);

    Line := Fixture.EvalAuto('GGlobalPrim.FCurr');
    Assert.IsTrue(Line.Contains('1234.5678'),
      'auto-detect must format FCurr as currency "1234.5678", got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Auto-type-detection for matched local variables at a paused
///   breakpoint. Requires <c>TLocalVar</c> to carry the RSM-derived
///   <c>TypeIdx</c> so <c>AutoDetectFormatterName</c> can route via
///   <c>FPrimitiveTypeFormatters</c> for primitive-typed locals.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateAutoTypeDetectionForLocal;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  // Line 45 is the breakpoint inside LocalsProcedure where LocalA,
  // LocalB, LocalString and LocalShort hold their assigned values.
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 45);
  try
    Line := Fixture.EvalAuto('LocalA');
    Assert.IsTrue(Line.Contains('305419896'),
      'auto-detect must format LocalA as int ($12345678 = 305419896), got: ' + Line);

    Line := Fixture.EvalAuto('LocalString');
    Assert.IsTrue(Line.Contains('Hello Local'),
      'auto-detect must format LocalString as string, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Auto-type-detection for whole-name primitive globals (not
///   reached via a dotted path). Requires the
///   <c>FindGlobalTypeIdx</c> raw 2-byte id to also map through
///   <c>FPrimitiveTypeFormatters</c> -- not just the class-id case
///   covered by the smaller test.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateAutoTypeDetectionForGlobal;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    // GGlobalInt is set to $11223344 in main BEFORE TargetProcedure
    // runs and reaches the BP, so the post-init value is observable.
    Line := Fixture.EvalAuto('GGlobalInt');
    Assert.IsTrue(Line.Contains('287454020'),
      'auto-detect must format GGlobalInt as int ($11223344 = 287454020), got: ' + Line);

    Line := Fixture.EvalAuto('GGlobalString');
    Assert.IsTrue(Line.Contains('Hello Global'),
      'auto-detect must format GGlobalString as string, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Asking for an inline-record terminal without an explicit type
///   has no sensible single-value answer. The server must surface a
///   helpful message that points the caller at sub-navigation
///   (.FieldName), not silently print zeroes or fall through to a
///   bogus formatter.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateAutoTypeDetectionRecordTerminal;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 15);
  try
    Line := Fixture.EvalAuto('GGlobalFloats');
    Assert.IsTrue(Line.Contains('record') and Line.Contains('TFloats'),
      'auto-detect on a record terminal must report the record type ' +
      'and suggest sub-navigation, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Auto-type-detection on a class-typed terminal field reached via a
///   register-passed Self. Mirrors the shape that real-world VCL code
///   uses constantly: <c>FormShow</c> referencing <c>Self.FNotification</c>
///   etc. The whole-name Self case is already covered elsewhere; this
///   exercises the DOTTED variant where the LAST hop's
///   <c>Member.TypeIdx</c> drives the formatter pick.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateAutoTypeDetectionSelfDottedClassField;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  // Line 250 is inside TClassFieldHost.TouchSelf with Self live in the
  // first register slot. GGlobalClassHost.FHostNested points at a TInner
  // (program-defined class). FHostRtlList points at a TStringList
  // (RTL-defined class) -- different RSM namespace, exercises the
  // cross-unit class-id resolution that VCL forms hit constantly.
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 250);
  try
    Line := Fixture.EvalAuto('Self.FHostNested');
    Assert.IsTrue(Line.Contains('TInner @ '),
      'auto-detect on Self.<ProgramClassField> must format as object ' +
      '"TInner @ <hex>", got: ' + Line);

    // Sanity: explicit type=object must succeed so the failure surface
    // below is isolated to auto-detect, not field resolution.
    Line := Fixture.Eval('Self.FHostRtlList', 'object');
    Assert.IsTrue(Line.Contains('TStringList @ '),
      'explicit type=object on Self.<RtlClassField> must work, got: ' + Line);

    Line := Fixture.EvalAuto('Self.FHostRtlList');
    Assert.IsTrue(Line.Contains('TStringList @ '),
      'auto-detect on Self.<RtlClassField> must format as object ' +
      '"TStringList @ <hex>", got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Auto-type-detection on enum-typed variables and fields. The
///   expected output shape is "Identifier (Ordinal)" -- the human-
///   readable enum name with the numeric ordinal in parens, so the
///   caller doesn't lose access to the underlying byte value.
/// </summary>
/// <remarks>
///   Three slots, all initialised in <c>EnumProbeProcedure</c> at
///   line 271:
///   <list type="bullet">
///     <item><c>LocalLight</c> = <c>lsYellow</c> (ord 1) -- local on
///       BP stack.</item>
///     <item><c>GGlobalLight</c> = <c>lsGreen</c> (ord 2) -- module
///       global with an initialiser.</item>
///     <item><c>GGlobalEnumRec.FLight</c> = <c>lsRed</c> (ord 0) --
///       enum field reached via record-dotted navigation.</item>
///   </list>
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateAutoTypeDetectionEnum;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 281);
  try
    Line := Fixture.EvalAuto('LocalLight');
    Assert.IsTrue(Line.Contains('lsYellow') and Line.Contains('(1)'),
      'auto-detect on enum local must format as "lsYellow (1)", got: ' + Line);

    Line := Fixture.EvalAuto('GGlobalLight');
    Assert.IsTrue(Line.Contains('lsGreen') and Line.Contains('(2)'),
      'auto-detect on enum global must format as "lsGreen (2)", got: ' + Line);

    Line := Fixture.EvalAuto('GGlobalEnumRec.FLight');
    Assert.IsTrue(Line.Contains('lsRed') and Line.Contains('(0)'),
      'auto-detect on dotted enum field must format as "lsRed (0)", got: ' + Line);

    // Cross-unit enum field via Self/dotted navigation. The field
    // name follows Delphi convention F<TypeWithoutT>, so the
    // name-based enum resolver infers the type as TThreadPriority
    // (declared in System.Classes) and auto-detects the value.
    Line := Fixture.EvalAuto('GGlobalThPriHost.FThreadPriority');
    Assert.IsTrue(Line.Contains('tpHigher') and Line.Contains('(4)'),
      'auto-detect on cross-unit enum field via name convention must ' +
      'format as "tpHigher (4)", got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Positive regression coverage for enum-typed terminal fields
///   reached via a LOCAL record (the global-record case is covered
///   by <see cref="TestMcpEvaluateAutoTypeDetectionEnum"/>). Both a
///   plain <c>TEnumHostRec</c> and a <c>packed record</c> variant
///   are probed with <c>FLight = lsRed</c> (ordinal 0 -- the first
///   enum element) so a future regression in
///   <c>AutoDetectFormatterName</c>'s enum routing for ordinal 0
///   trips here. The companion test
///   <see cref="TestMcpEvaluateAutoTypeDetectionEnumInVariantCase"/>
///   covers the variant-case nested-record path.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateAutoTypeDetectionEnumOnLocalRecord;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 281);
  try
    Line := Fixture.EvalAuto('LocalEnumRec.FLight');
    Assert.IsTrue(Line.Contains('lsRed') and Line.Contains('(0)'),
      'auto-detect on enum field of LOCAL record (ordinal 0) must format ' +
      'as "lsRed (0)", got: ' + Line);

    Line := Fixture.EvalAuto('LocalEnumRecPacked.FLight');
    Assert.IsTrue(Line.Contains('lsRed') and Line.Contains('(0)'),
      'auto-detect on enum field of LOCAL packed record (ordinal 0) must ' +
      'format as "lsRed (0)", got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Regression coverage for the dotted-walk's record-local
///   priming: a LOCAL of a record type with a nested record in a
///   variant case must walk through the variant-case overlay's
///   declared offset. Mirrors the TFW shape where
///   <c>UserKonsOutlook.SyncStatus</c> is an enum field on a
///   packed record sitting in a variant case
///   <c>5: (Outlook: TUserKonsOutlook)</c> of <c>TUserKons</c>.
/// </summary>
/// <remarks>
///   The reproducer in <c>DebugTarget</c> uses
///   <c>TEnumVariantHost</c>:
///   <code>
///     TEnumVariantHost = packed record
///       FTag: Integer;
///       case Integer of
///         0: (FFlag1: Boolean);
///         5: (FInner: TEnumHostRec);
///     end;
///   </code>
///   The probe sets <c>LocalVariantHost.FInner.FLight := lsRed</c>
///   (ordinal 0). Before the dotted-walk priming was extended to
///   handle local record types via <c>FindStructByTypeIdx</c>, the
///   first hop tried class semantics on <c>LocalVariantHost</c>
///   and returned <c>"&lt;unknown&gt; (&lt;garbage&gt;)"</c>.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateAutoTypeDetectionEnumInVariantCase;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 281);
  try
    Line := Fixture.EvalAuto('LocalVariantHost.FInner.FLight');
    Assert.IsTrue(Line.Contains('lsRed') and Line.Contains('(0)'),
      'auto-detect on enum field of variant-case nested record ' +
      '(ordinal 0) must format as "lsRed (0)", got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Regression coverage for the TFW
///   <c>UserKonsOutlook.SyncDirection</c> misroute. The field
///   <c>SyncDirection : TFieldStatusKind</c> sits at record offset
///   771 -- well past the 256-byte boundary where the compiler
///   switches the <c>$2C</c> field record's body shape to a
///   two-byte separator between the field's type id and the
///   <c>$9C $01</c> reference marker. Until the Format-A linker
///   learned the long-separator shape, fields past that boundary
///   arrived with <c>Member.PrimitiveTypeId = 0</c>; auto-detect
///   then fell through to the name-based fallback and resolved
///   <c>T+"SyncDirection" = "TSyncDirection"</c> (a red-herring
///   enum anchored by <c>GSyncDirectionAnchor</c>), returning the
///   wrong enum's <c>sdBeta (1)</c> instead of <c>fskDraft (1)</c>.
/// </summary>
/// <remarks>
///   With the long-separator branch in place, the field's
///   <c>PrimitiveTypeId</c> carries the real
///   <c>TFieldStatusKind</c> id and Path 1 of
///   <c>AutoDetectFormatterName</c> resolves directly to the
///   correct constant.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateEnumNameClashPicksWrongType;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 430);
  try
    Line := Fixture.EvalAuto('GFieldHost.SyncDirection');
    Assert.IsTrue(Line.Contains('fskDraft') and Line.Contains('(1)'),
      'auto-detect on enum-typed record field whose name clashes ' +
      'with a registered foreign enum (TSyncDirection) must format ' +
      'as "fskDraft (1)" -- the field''s ACTUAL TFieldStatusKind ' +
      'constant, not TSyncDirection''s "sdBeta" -- got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Regression coverage for the TFW
///   <c>UserKonsOutlook.SyncStatus</c> failure. Same long-separator
///   shape as the <c>SyncDirection</c> sibling, but the field
///   name <c>SyncStatus</c> has no <c>T+Name</c> match in the
///   registry (<c>TSyncStatus</c> is intentionally absent). Before
///   the linker fix, this would fall through every auto-detect
///   path and end at the "no RSM type metadata" hint; with the
///   long-separator branch in place, the field's
///   <c>TFieldUnregKind</c> id reaches
///   <c>AutoDetectFormatterName</c> Path 1 directly and the
///   formatter returns <c>fukActive (1)</c>.
/// </summary>
procedure TMcpServerTests.TestMcpEvaluateEnumWithoutNameMatchFails;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 430);
  try
    Line := Fixture.EvalAuto('GFieldHost.SyncStatus');
    Assert.IsTrue(Line.Contains('fukActive') and Line.Contains('(1)'),
      'auto-detect on enum-typed record field whose name has no ' +
      'T+Name match (TSyncStatus is NOT registered) must still ' +
      'format as "fukActive (1)" via the field''s declared ' +
      'TFieldUnregKind type, got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Three units (DebugTarget.EnumAlpha / .EnumBeta / .EnumGamma)
///   declare the SAME type name <c>TStatus</c> with different
///   element prefixes (sa / sb / sc). DebugTarget.dpr pulls all
///   three into the same compile unit; the auto-detect path must
///   pick the right unit's enum for each variable based on the
///   variable's STORED type id, not on the name-keyed registry
///   which collapses last-wins for shared type names.
/// </summary>
/// <remarks>
///   Four variables initialised in <c>CrossUnitEnumProbe</c> at
///   line 372:
///   <list type="bullet">
///     <item><c>GStatusAlpha</c> = <c>saRunning</c> (ord 1) -- declared
///       with the fully-qualified type <c>DebugTarget.EnumAlpha.TStatus</c>.</item>
///     <item><c>GStatusBeta</c> = <c>sbStopped</c> (ord 2) -- fully
///       qualified to <c>DebugTarget.EnumBeta.TStatus</c>.</item>
///     <item><c>GStatusGamma</c> = <c>scInit</c> (ord 0) -- fully
///       qualified to <c>DebugTarget.EnumGamma.TStatus</c>.</item>
///     <item><c>GStatusUnq</c> = <c>scWorking</c> (ord 1) -- declared
///       with the UNQUALIFIED <c>TStatus</c>; Delphi's uses-order
///       picks Gamma (the last unit).</item>
///   </list>
///
///   Resolved end-to-end via the <c>$03</c> ENUM_DEF record parser
///   (added to <c>DPT.Rsm.Scanner</c>): each (unit, type) pair gets
///   its OWN entry in <c>TRsmReader.EnumDefs</c>, so three sibling
///   units declaring <c>TStatus</c> produce three distinct entries
///   instead of collapsing last-wins. The auto-detect path in
///   <c>TDebugger</c> calls <c>TryResolveScopeLocalEnum</c> for
///   globals whose registry type id carries the scope-local marker
///   ($1E hi byte); the resolver picks the right unit by matching
///   the variable name's TRAILING segment against each registered
///   unit's trailing-short ("Alpha" matches <c>DebugTarget.EnumAlpha</c>,
///   etc.). When no unit-suffix match fires (the
///   <c>GStatusUnq</c> case), the resolver falls back to the
///   LAST-declared TStatus def -- which matches Delphi's
///   uses-order "last wins" rule.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateCrossUnitEnumWithSameTypeName;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 372);
  try
    // GStatusAlpha: TStatus from EnumAlpha (contiguous 0..2). saRunning = ord 1.
    Line := Fixture.EvalAuto('GStatusAlpha');
    Assert.IsTrue(Line.Contains('saRunning') and Line.Contains('(1)'),
      'GStatusAlpha must format as "saRunning (1)" (DebugTarget.EnumAlpha.TStatus, contiguous), got: ' + Line);

    // GStatusBeta: TStatus from EnumBeta declared sparse
    // (sbIdle = 1, sbActive = 5, sbStopped = 10). sbStopped's explicit ordinal is 10.
    Line := Fixture.EvalAuto('GStatusBeta');
    Assert.IsTrue(Line.Contains('sbStopped') and Line.Contains('(10)'),
      'GStatusBeta must format as "sbStopped (10)" (DebugTarget.EnumBeta.TStatus, sparse), got: ' + Line);

    // GStatusGamma: TStatus from EnumGamma sparse
    // (scInit = 7, scWorking = 13, scComplete = 100). scInit's explicit ordinal is 7.
    Line := Fixture.EvalAuto('GStatusGamma');
    Assert.IsTrue(Line.Contains('scInit') and Line.Contains('(7)'),
      'GStatusGamma must format as "scInit (7)" (DebugTarget.EnumGamma.TStatus, sparse), got: ' + Line);

    // GStatusUnq: unqualified TStatus picks Gamma via uses-order
    // last-wins. scWorking's explicit ordinal in Gamma is 13.
    Line := Fixture.EvalAuto('GStatusUnq');
    Assert.IsTrue(Line.Contains('scWorking') and Line.Contains('(13)'),
      'GStatusUnq must format as "scWorking (13)" (uses-order last wins -> Gamma.TStatus, sparse), got: ' + Line);
  finally
    Fixture.Free;
  end;
end;

/// <summary>
///   Red-test for the "Variable-typeId -> Primary echter Bridge" gap.
///   <c>GStatusToggle</c> is declared with the fully-qualified type
///   <c>DebugTarget.EnumAlpha.TStatus</c> and initialised to
///   <c>saRunning</c> (ord 1) -- so a correct resolution must return
///   <c>saRunning (1)</c>. The variable's name "GStatusToggle"
///   derives via <c>DeriveTypeHintFromVariableName</c> to the hint
///   <c>"TStatus"</c>, but its trailing word "Toggle" matches none
///   of the registered unit shorts (Alpha / Beta / Gamma), so the
///   name-suffix heuristic in <c>TryResolveScopeLocalEnum</c>
///   fails and the fallback last-wins picks the LAST <c>TStatus</c>
///   def with an element at ord 1.
/// </summary>
/// <remarks>
///   Resolved via the per-typeId bridge built in
///   <c>BuildScopeLocalTypeIdBridge</c>: <c>GStatusAlpha</c> is
///   the conventionally-named anchor variable for scope-local
///   type id <c>0x1E99</c> (it ends with "Alpha"), so the bridge
///   binds that id to Alpha's EnumDef. <c>GStatusToggle</c>
///   carries the SAME stored type id (the compiler emits one
///   scope-local id per (unit, type) pair, not per variable),
///   so it inherits Alpha's binding via <c>TypeId -> EnumDef</c>
///   lookup, regardless of its own name lacking a unit suffix.
///   The name-suffix heuristic in <see cref="TryResolveScope-
///   LocalEnum"/> remains as a fallback (Path B) for the rare
///   case where NO variable of a given scope-local id has a
///   conventionally-named name.
/// </remarks>
procedure TMcpServerTests.TestMcpEvaluateCrossUnitEnumWithoutUnitSuffixHint;
var
  Fixture: TMcpEvalFixture;
  ExePath: String;
  Line   : String;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  Fixture := TMcpEvalFixture.CreateAtBreakpoint(
    ExePath, ChangeFileExt(ExePath, '.map'), 'DebugTarget.dpr', 372);
  try
    Line := Fixture.EvalAuto('GStatusToggle');
    Assert.IsTrue(Line.Contains('saRunning') and Line.Contains('(1)'),
      'GStatusToggle must format as "saRunning (1)" -- the variable is ' +
      'declared with DebugTarget.EnumAlpha.TStatus, so its actual unit ' +
      'is Alpha regardless of the variable name lacking an "Alpha" suffix. ' +
      'A correct resolver bridges variable typeId -> primary directly, ' +
      'not via a name-suffix heuristic. Got: ' + Line);
  finally
    Fixture.Free;
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
      '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
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
      Assert.IsTrue(StartLine.Contains('DebugTarget.dpr:15'),
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
      '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 15}}}' + sLineBreak +
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 19}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    Debugger.LoadDebugInfoFromExe(ExePath);
    TDebuggerThread.Create(Debugger, ExePath);

    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp
      Server.RunOnce; // continue (async)

      // Wait for breakpoint notification
      WaitForOutput(OutputWriter, 4);
      Assert.IsTrue(OutputWriter.GetLine(3).Contains('notifications/stopped'), 'Expected stopped notification after continue');

      // step_into (async)
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "step_into", "arguments": {}}}');
      Server.RunOnce;
      
      var StepRespFound := False;
      for var I := OutputWriter.GetCount - 1 downto 0 do
      begin
        if OutputWriter.GetLine(I).Contains('"id": 4') or OutputWriter.GetLine(I).Contains('"id":"4"') or OutputWriter.GetLine(I).Contains('"id":4') then
        begin
          Assert.IsTrue(OutputWriter.GetLine(I).Contains('Stepping into'), 'Step into should return immediately');
          StepRespFound := True;
          Break;
        end;
      end;
      Assert.IsTrue(StepRespFound, 'step_into response not found');

      // Call wait_until_paused. It should block until the step finishes.
      InputReader.FLines.Add('{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "wait_until_paused", "arguments": {"timeout_ms": 5000}}}');
      Server.RunOnce;
      
      // We expect the debugger notification(s) and the wait_until_paused response.
      WaitForOutput(OutputWriter, 7);
      
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
    Debugger.LoadDebugInfoFromExe(ExePath);
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
    Debugger.LoadDebugInfoFromExe(ExePath);
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
