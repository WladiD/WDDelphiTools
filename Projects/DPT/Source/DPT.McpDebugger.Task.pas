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
  public
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

{ TDptMcpDebuggerTask }

procedure TDptMcpDebuggerTask.Parse(CmdLine: TCmdLineConsumer);
begin
  inherited;
  // No parameters needed anymore, MCP Server starts empty
end;

procedure TDptMcpDebuggerTask.Execute;
var
  Server: TMcpServer;
begin
  Server := TMcpServer.Create(nil); // Debugger will be created later by a tool
  try
    Server.Run;
  finally
    Server.Free;
  end;
end;

end.