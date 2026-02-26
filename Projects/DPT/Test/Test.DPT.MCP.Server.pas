unit Test.DPT.MCP.Server;

interface

uses
  DUnitX.TestFramework,
  DPT.Debugger,
  DPT.MCP.Server,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.SyncObjs;

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
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(const S: string); override;
    procedure WriteLine(const S: string); override;
    procedure Close; override;
    procedure Flush; override;
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
end;

destructor TStringTextWriter.Destroy;
begin
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
  FOutput.Add(S);
end;

{ TMcpServerTests }

procedure TMcpServerTests.TestMcpProtocolFlow;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  InputReader: TStringTextReader;
  OutputWriter: TStringTextWriter;
  InputStr: string;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputStr :=
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "get_stack_trace", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_stack_memory", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 6, "method": "tools/call", "params": {"name": "read_global_variable", "arguments": {"name": "DebugTarget.GGlobalInt", "size": 4}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 7, "method": "tools/call", "params": {"name": "get_stack_slots", "arguments": {}}}';

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    InputReader := TStringTextReader.Create(InputStr);
    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp
      Server.RunOnce; // continue
      Assert.IsTrue(OutputWriter.Output[2].Contains('Paused at DebugTarget.dpr:13'));

      Server.RunOnce; // get_stack_trace
      Assert.IsTrue(OutputWriter.Output[3].Contains('DeepProcedure'));

      Server.RunOnce; // get_stack_memory
      Assert.IsTrue(OutputWriter.Output[4].Contains('78 56 34 12'));

      Server.RunOnce; // read_global_variable
      Assert.IsTrue(OutputWriter.Output[5].Contains('44 33 22 11'));

      Server.RunOnce; // get_stack_slots
      Assert.IsTrue(OutputWriter.Output[6].Contains('12345678'));

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
  InputStr: string;
  LJSON: TJSONObject;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\ExceptionTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('ExceptionTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputStr := 
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}';

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    InputReader := TStringTextReader.Create(InputStr);
    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce;
      Server.RunOnce;
      Sleep(1000); 

      var ExceptionNotifFound := False;
      for var I := 0 to OutputWriter.Output.Count - 1 do
      begin
        if OutputWriter.Output[I].Contains('notifications/debugger_exception') then
        begin
          ExceptionNotifFound := True;
          LJSON := TJSONObject.ParseJSONValue(OutputWriter.Output[I]) as TJSONObject;
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
  InputStr: string;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputStr := 
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 22}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "step_over", "arguments": {}}}';

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    InputReader := TStringTextReader.Create(InputStr);
    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp
      Server.RunOnce; // continue -> reach line 22 (TargetProcedure;)
      Assert.IsTrue(OutputWriter.Output[2].Contains('Paused at DebugTarget.dpr:22'), 'Did not reach line 22: ' + OutputWriter.Output[2]);
      
      Server.RunOnce; // step_over -> should reach next line in main or finish.
      // We don't have many lines in main. Let's just verify it didn't hang.
      Assert.IsTrue(OutputWriter.Output[3].Contains('Stepped over') or OutputWriter.Output[3].Contains('Execution finished'), 'Step over failed: ' + OutputWriter.Output[3]);

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
  InputStr: string;
  LJSON: TJSONObject;
  ResultObj, Meta: TJSONObject;
  ExePath, MapFile: string;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  InputStr := 
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 13}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "get_stack_slots", "arguments": {}}}';

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    InputReader := TStringTextReader.Create(InputStr);
    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      Server.RunOnce; // init
      Server.RunOnce; // set_bp
      Server.RunOnce; // continue
      Server.RunOnce; // get_stack_slots

      Assert.AreEqual(4, OutputWriter.Output.Count);
      LJSON := TJSONObject.ParseJSONValue(OutputWriter.Output[3]) as TJSONObject;
      try
        ResultObj := LJSON.GetValue('result') as TJSONObject;
        Meta := ResultObj.GetValue('frame_metadata') as TJSONObject;
        Assert.IsNotNull(Meta, 'Frame metadata missing');
        Assert.AreEqual('DebugTarget.DeepProcedure', Meta.GetValue('procedure').Value);
        
        var LocalSize := (Meta.GetValue('local_variable_size') as TJSONNumber).AsInt;
        // DeepProcedure has local 'LocalInt' (4 bytes). We expect 4 bytes detected.
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

initialization
  TDUnitX.RegisterTestFixture(TMcpServerTests);

end.