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
  end;

implementation

type
  TStringTextReader = class(TTextReader)
  private
    FLines: TStringList;
    FIndex: Integer;
  public
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
    '{"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "set_breakpoint", "arguments": {"unit": "DebugTarget.dpr", "line": 17}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "get_stack_trace", "arguments": {}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "get_stack_memory", "arguments": {}}}';

  Debugger := TDebugger.Create;
  try
    Debugger.LoadMapFile(MapFile);
    TDebuggerThread.Create(Debugger, ExePath);

    InputReader := TStringTextReader.Create(InputStr);
    OutputWriter := TStringTextWriter.Create;
    Server := TMcpServer.Create(Debugger, InputReader, OutputWriter);
    try
      // Process initialize
      Server.RunOnce;
      Assert.AreEqual(1, OutputWriter.Output.Count, 'Initialize failed');

      // Process set_breakpoint
      Server.RunOnce;
      Assert.AreEqual(2, OutputWriter.Output.Count, 'SetBreakpoint failed');

      // Process continue (blocks until breakpoint)
      Server.RunOnce;
      Assert.AreEqual(3, OutputWriter.Output.Count, 'Continue failed');
      Assert.IsTrue(OutputWriter.Output[2].Contains('Paused at DebugTarget.dpr:17'), 'Wrong pause location: ' + OutputWriter.Output[2]);

      // Process get_stack_trace
      Server.RunOnce;
      Assert.AreEqual(4, OutputWriter.Output.Count, 'StackTrace failed');
      Assert.IsTrue(OutputWriter.Output[3].Contains('DeepProcedure'), 'DeepProcedure missing');

      // Process get_stack_memory
      Server.RunOnce;
      Assert.AreEqual(5, OutputWriter.Output.Count, 'StackMemory failed');
      // LocalInt value $12345678 (little endian: 78 56 34 12)
      Assert.IsTrue(OutputWriter.Output[4].Contains('78 56 34 12'), 'LocalInt missing in stack dump: ' + OutputWriter.Output[4]);

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