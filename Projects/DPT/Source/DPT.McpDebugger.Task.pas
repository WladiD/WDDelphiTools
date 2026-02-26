unit DPT.McpDebugger.Task;

interface

uses
  System.SysUtils,
  DPT.Types,
  DPT.Task,
  DPT.Debugger,
  DPT.MCP.Server;

type
  TDptMcpDebuggerTask = class(TDptTaskBase)
  private
    FExecutablePath: string;
    FRunArgs: string;
  public
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

{ TDptMcpDebuggerTask }

procedure TDptMcpDebuggerTask.Parse(CmdLine: TCmdLineConsumer);
begin
  inherited;
  FExecutablePath := CmdLine.CheckParameter('ExecutablePath');
  CmdLine.ConsumeParameter;
  
  FRunArgs := '';
  while CmdLine.HasParameter do
  begin
    FRunArgs := FRunArgs + ' ' + CmdLine.CheckParameter('Args');
    CmdLine.ConsumeParameter;
  end;
  FRunArgs := Trim(FRunArgs);
end;

procedure TDptMcpDebuggerTask.Execute;
var
  Debugger: TDebugger;
  Server: TMcpServer;
  MapFile: string;
begin
  if not FileExists(FExecutablePath) then
    raise Exception.Create('Executable not found: ' + FExecutablePath);

  Debugger := TDebugger.Create;
  try
    MapFile := ChangeFileExt(FExecutablePath, '.map');
    if FileExists(MapFile) then
      Debugger.LoadMapFile(MapFile)
    else
      Writeln('Warning: No map file found at ' + MapFile + '. Symbol resolution will not work.');

    // Start debugger in background thread so main thread can process MCP messages
    TDebuggerThread.Create(Debugger, FExecutablePath + ' ' + FRunArgs);
    
    Server := TMcpServer.Create(Debugger);
    try
      // We don't write generic logs to stdout anymore because it's an MCP Server now.
      // Any text output here could corrupt the JSON-RPC stream the client expects.
      Server.Run;
    finally
      Server.Free;
    end;
  finally
    Debugger.Free;
  end;
end;

end.