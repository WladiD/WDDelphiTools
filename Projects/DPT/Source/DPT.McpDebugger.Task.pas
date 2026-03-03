// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.McpDebugger.Task;

interface

uses

  DPT.Task;

type

  TDptMcpDebuggerTask = class(TDptTaskBase)
  public
    procedure Execute; override;
  end;

implementation

uses

  System.SysUtils,

  DPT.Debugger,
  DPT.MCP.Server,
  DPT.Types;

{ TDptMcpDebuggerTask }

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
