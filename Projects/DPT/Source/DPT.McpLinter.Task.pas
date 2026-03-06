// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.McpLinter.Task;

interface

uses

  DPT.Task;

type

  TDptMcpLinterTask = class(TDptTaskBase)
  public
    procedure Execute; override;
  end;

implementation

uses

  DPT.McpLinter.Server;

{ TDptMcpLinterTask }

procedure TDptMcpLinterTask.Execute;
var
  Server: TMcpLinterServer;
begin
  Server := TMcpLinterServer.Create;
  try
    Server.Run;
  finally
    Server.Free;
  end;
end;

end.
