unit DPT.MCP.Server;

interface

uses

  mormot.core.base,

  Winapi.Windows,

  System.SysUtils,
  System.Classes,
  System.JSON,
  System.SyncObjs,
  System.Generics.Collections,

  mormot.core.json,

  DPT.Debugger;

type

  TDebugState = (dsNoSession, dsPaused, dsRunning, dsExited);

  TMcpServer = class
  private
    FDebugger          : TDebugger;
    FExitRequest       : Boolean;
    FInputReader       : TTextReader;
    FOutputLock        : TCriticalSection;
    FOutputWriter      : TTextWriter;
    FPendingBreakpoints: TObjectList<TBreakpoint>;
    FState             : TDebugState;
    function  GetBreakpointCount: Integer;
    function  MakeErrorResult(const AText: String): TJSONObject;
    function  MakeTextResult(const AText: String): TJSONObject;
    procedure ProcessMessage(const AMessage: String);
    function  RequireState(const AAllowed: array of TDebugState; out AResult: TJSONObject): Boolean;
    procedure SendError(const AID: TJSONValue; ACode: Integer; const AMessage: String);
    procedure SendNotification(const AMethod: String; AParams: TJSONObject);
    procedure SendResponse(const AID: TJSONValue; AResult: TJSONObject);
    procedure SendSamplingRequest(const AText: String);
    procedure WriteOutput(const AJSON: String);
  private // Tool handlers
    function HandleContinue(AParams: TJSONObject): TJSONObject;
    function HandleGetProcAsm(AParams: TJSONObject): TJSONObject;
    function HandleGetRegisters(AParams: TJSONObject): TJSONObject;
    function HandleGetStackMemory(AParams: TJSONObject): TJSONObject;
    function HandleGetStackSlots(AParams: TJSONObject): TJSONObject;
    function HandleGetStackTrace(AParams: TJSONObject): TJSONObject;
    function HandleGetState(AParams: TJSONObject): TJSONObject;
    function HandleListBreakpoints(AParams: TJSONObject): TJSONObject;
    function HandleListTools(AParams: TJSONObject): TJSONObject;
    function HandleReadGlobalVariable(AParams: TJSONObject): TJSONObject;
    function HandleReadMemory(AParams: TJSONObject): TJSONObject;
    function HandleRemoveBreakpoint(AParams: TJSONObject): TJSONObject;
    function HandleSetBreakpoint(AParams: TJSONObject): TJSONObject;
    function HandleStartDebugSession(AParams: TJSONObject): TJSONObject;
    function HandleStepInto(AParams: TJSONObject): TJSONObject;
    function HandleStepOver(AParams: TJSONObject): TJSONObject;
    function HandleStopDebugSession(AParams: TJSONObject): TJSONObject;
    function HandleTerminateDebugSession(AParams: TJSONObject): TJSONObject;
  private // Event handler
    procedure DebuggerBreakpointHandler(ASender: TObject; ABreakpoint: TBreakpoint);
    procedure DebuggerExceptionHandler(ASender: TObject; const AExceptionRecord: TExceptionRecord; const AFirstChance: Boolean; var AHandled: Boolean);
    procedure DebuggerProcessExitHandler(ASender: TObject; AExitCode: DWORD);
    procedure DebuggerSteppedHandler(ASender: TObject; ABreakpoint: TBreakpoint);
    procedure DisconnectDebuggerEvents;
  public
    constructor Create(ADebugger: TDebugger; AInput: TTextReader = nil; AOutput: TTextWriter = nil);
    destructor  Destroy; override;
    procedure Run;
    procedure RunOnce;
    property State: TDebugState read FState;
  end;

implementation

{ TMcpServer }

constructor TMcpServer.Create(ADebugger: TDebugger; AInput: TTextReader; AOutput: TTextWriter);
begin
  inherited Create;
  FDebugger := ADebugger;
  FExitRequest := False;
  FInputReader := AInput;
  FOutputWriter := AOutput;
  FOutputLock := TCriticalSection.Create;
  FPendingBreakpoints := TObjectList<TBreakpoint>.Create(True);

  if Assigned(FDebugger) then
  begin
    FDebugger.OnException := DebuggerExceptionHandler;
    FDebugger.OnBreakpoint := DebuggerBreakpointHandler;
    FDebugger.OnStepped := DebuggerSteppedHandler;
    FDebugger.OnProcessExit := DebuggerProcessExitHandler;
    FState := dsPaused;
  end
  else
    FState := dsNoSession;
end;

destructor TMcpServer.Destroy;
begin
  DisconnectDebuggerEvents;
  FPendingBreakpoints.Free;
  FOutputLock.Free;
  inherited Destroy;
end;

procedure TMcpServer.WriteOutput(const AJSON: String);
begin
  FOutputLock.Enter;
  try
    if Assigned(FOutputWriter) then
    begin
      FOutputWriter.WriteLine(AJSON);
      FOutputWriter.Flush;
    end
    else
    begin
      System.Write(AJSON + #13#10);
      System.Flush(System.Output);
    end;
  finally
    FOutputLock.Leave;
  end;
end;

procedure TMcpServer.SendResponse(const AID: TJSONValue; AResult: TJSONObject);
var
  Resp: TJSONObject;
begin
  Resp := TJSONObject.Create;
  try
    Resp.AddPair('jsonrpc', '2.0');
    if AID <> nil then
      Resp.AddPair('id', AID.Clone as TJSONValue);
    Resp.AddPair('result', AResult);
    WriteOutput(Resp.ToJSON);
  finally
    Resp.Free;
  end;
end;

procedure TMcpServer.SendError(const AID: TJSONValue; ACode: Integer; const AMessage: String);
var
  Err : TJSONObject;
  Resp: TJSONObject;
begin
  Resp := TJSONObject.Create;
  try
    Resp.AddPair('jsonrpc', '2.0');
    if AID <> nil then
      Resp.AddPair('id', AID.Clone as TJSONValue);

    Err := TJSONObject.Create;
    Err.AddPair('code', TJSONNumber.Create(ACode));
    Err.AddPair('message', AMessage);
    Resp.AddPair('error', Err);
    WriteOutput(Resp.ToJSON);
  finally
    Resp.Free;
  end;
end;

procedure TMcpServer.SendNotification(const AMethod: String; AParams: TJSONObject);
var
  Notif: TJSONObject;
begin
  Notif := TJSONObject.Create;
  try
    Notif.AddPair('jsonrpc', '2.0');
    Notif.AddPair('method', AMethod);
    Notif.AddPair('params', AParams);
    WriteOutput(Notif.ToJSON);
  finally
    Notif.Free;
  end;
end;

procedure TMcpServer.SendSamplingRequest(const AText: String);
var
  ContentObj: TJSONObject;
  IDStr     : String;
  MsgArr    : TJSONArray;
  MsgObj    : TJSONObject;
  Params    : TJSONObject;
  Req       : TJSONObject;
begin
  IDStr := 'smpl_' + IntToStr(GetTickCount);
  Req := TJSONObject.Create;
  try
    Req.AddPair('jsonrpc', '2.0');
    Req.AddPair('id', IDStr);
    Req.AddPair('method', 'sampling/createMessage');

    Params := TJSONObject.Create;
    MsgArr := TJSONArray.Create;
    MsgObj := TJSONObject.Create;
    MsgObj.AddPair('role', 'user');

    ContentObj := TJSONObject.Create;
    ContentObj.AddPair('type', 'text');
    ContentObj.AddPair('text', AText);

    MsgObj.AddPair('content', ContentObj);
    MsgArr.Add(MsgObj);

    Params.AddPair('messages', MsgArr);
    Params.AddPair('maxTokens', TJSONNumber.Create(1000));

    Req.AddPair('params', Params);
    WriteOutput(Req.ToJSON);
  finally
    Req.Free;
  end;
end;

function TMcpServer.MakeTextResult(const AText: String): TJSONObject;
var
  ContentArr: TJSONArray;
begin
  Result := TJSONObject.Create;
  ContentArr := TJSONArray.Create;
  ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', AText));
  Result.AddPair('content', ContentArr);
end;

function TMcpServer.MakeErrorResult(const AText: String): TJSONObject;
var
  ContentArr: TJSONArray;
begin
  Result := TJSONObject.Create;
  ContentArr := TJSONArray.Create;
  ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', AText));
  Result.AddPair('content', ContentArr);
  Result.AddPair('isError', TJSONBool.Create(True));
end;

function TMcpServer.RequireState(const AAllowed: array of TDebugState; out AResult: TJSONObject): Boolean;
var
  S         : TDebugState;
  StateNames: Array[TDebugState] of String;
begin
  StateNames[dsNoSession] := 'no_session';
  StateNames[dsPaused] := 'paused';
  StateNames[dsRunning] := 'running';
  StateNames[dsExited] := 'exited';

  for S in AAllowed do
    if FState = S then
      Exit(True);

  AResult := MakeErrorResult(Format('Error: Invalid state "%s" for this operation.', [StateNames[FState]]));
  Result := False;
end;

function TMcpServer.GetBreakpointCount: Integer;
begin
  Result := FPendingBreakpoints.Count;
  if Assigned(FDebugger) then
    Result := Result + FDebugger.Breakpoints.Count;
end;

procedure TMcpServer.DebuggerBreakpointHandler(ASender: TObject; ABreakpoint: TBreakpoint);
var
  Params: TJSONObject;
  LMsg  : String;
begin
  FState := dsPaused;
  Params := TJSONObject.Create;
  Params.AddPair('reason', 'breakpoint');
  if ABreakpoint <> nil then
  begin
    Params.AddPair('unit', ABreakpoint.UnitName);
    Params.AddPair('line', TJSONNumber.Create(ABreakpoint.LineNumber));
    LMsg := Format('The debugger stopped at a breakpoint in %s line %d.', [ABreakpoint.UnitName, ABreakpoint.LineNumber]);
  end
  else
    LMsg := 'The debugger stopped at a breakpoint.';

  SendNotification('notifications/stopped', Params);
  SendSamplingRequest(LMsg);
end;

procedure TMcpServer.DebuggerSteppedHandler(ASender: TObject; ABreakpoint: TBreakpoint);
var
  Params: TJSONObject;
  LMsg  : String;
begin
  FState := dsPaused;
  Params := TJSONObject.Create;
  Params.AddPair('reason', 'step');
  if ABreakpoint <> nil then
  begin
    Params.AddPair('unit', ABreakpoint.UnitName);
    Params.AddPair('line', TJSONNumber.Create(ABreakpoint.LineNumber));
    LMsg := Format('The debugger stopped after a step in %s line %d.', [ABreakpoint.UnitName, ABreakpoint.LineNumber]);
  end
  else
    LMsg := 'The debugger stopped after a step.';

  SendNotification('notifications/stopped', Params);
  SendSamplingRequest(LMsg);
end;

procedure TMcpServer.DebuggerProcessExitHandler(ASender: TObject; AExitCode: DWORD);
var
  Params: TJSONObject;
begin
  FState := dsExited;
  Params := TJSONObject.Create;
  Params.AddPair('reason', 'exited');
  Params.AddPair('exitCode', TJSONNumber.Create(AExitCode));
  SendNotification('notifications/stopped', Params);
end;

procedure TMcpServer.DebuggerExceptionHandler(ASender: TObject; const AExceptionRecord: TExceptionRecord; const AFirstChance: Boolean; var AHandled: Boolean);
var
  Params: TJSONObject;
  Stack : TArray<TStackFrame>;
begin
  FState := dsPaused;
  Params := TJSONObject.Create;
  Params.AddPair('code', Format('%08x', [AExceptionRecord.ExceptionCode]));
  Params.AddPair('address', Format('%p', [AExceptionRecord.ExceptionAddress]));
  Params.AddPair('firstChance', TJSONBool.Create(AFirstChance));

  Stack := FDebugger.GetStackTrace(FDebugger.LastThreadHit);
  if Length(Stack) > 0 then
  begin
    Params.AddPair('unit', Stack[0].UnitName);
    Params.AddPair('line', TJSONNumber.Create(Stack[0].LineNumber));
    Params.AddPair('procedure', Stack[0].ProcedureName);
  end;

  SendNotification('notifications/debugger_exception', Params);
  AHandled := True;
end;

procedure TMcpServer.DisconnectDebuggerEvents;
begin
  if Assigned(FDebugger) then
  begin
    FDebugger.OnException := nil;
    FDebugger.OnBreakpoint := nil;
    FDebugger.OnStepped := nil;
    FDebugger.OnProcessExit := nil;
  end;
end;

procedure TMcpServer.ProcessMessage(const AMessage: String);
var
  ID       : TJSONValue;
  JSON     : TJSONObject;
  Method   : String;
  MethodVal: TJSONValue;
  Params   : TJSONObject;
  ResultObj: TJSONObject;
begin
  try
    JSON := TJSONObject.ParseJSONValue(AMessage) as TJSONObject;
    if JSON = nil then
      Exit;
    try
      ID := JSON.GetValue('id');
      MethodVal := JSON.GetValue('method');
      if MethodVal = nil then Exit;
      Method := MethodVal.Value;
      Params := JSON.GetValue('params') as TJSONObject;

      if Method = 'initialize' then
      begin
        ResultObj := TJSONObject.Create;
        ResultObj.AddPair('protocolVersion', '2024-11-05');
        var CapObj := TJSONObject.Create;
        CapObj.AddPair('tools', TJSONObject.Create);
        ResultObj.AddPair('capabilities', CapObj);
        ResultObj.AddPair('serverInfo', TJSONObject.Create.AddPair('name', 'DPT-Debugger').AddPair('version', '1.0.0'));
        SendResponse(ID, ResultObj);
      end
      else if Method = 'notifications/initialized' then
      begin
        // Handled
      end
      else if Method = 'tools/list' then
        SendResponse(ID, HandleListTools(Params))
      else if Method = 'tools/call' then
      begin
        if Params = nil then
        begin
          SendError(ID, -32602, 'Missing params');
          Exit;
        end;
        var NameVal := Params.GetValue('name');
        if NameVal = nil then
        begin
          SendError(ID, -32602, 'Missing tool name');
          Exit;
        end;
        var ToolName := NameVal.Value;
        var ToolParams := Params.GetValue('arguments') as TJSONObject;

        if ToolName = 'set_breakpoint' then
          SendResponse(ID, HandleSetBreakpoint(ToolParams))
        else if ToolName = 'remove_breakpoint' then
          SendResponse(ID, HandleRemoveBreakpoint(ToolParams))
        else if ToolName = 'list_breakpoints' then
          SendResponse(ID, HandleListBreakpoints(ToolParams))
        else if ToolName = 'continue' then
          SendResponse(ID, HandleContinue(ToolParams))
        else if ToolName = 'get_stack_trace' then
          SendResponse(ID, HandleGetStackTrace(ToolParams))
        else if ToolName = 'read_memory' then
          SendResponse(ID, HandleReadMemory(ToolParams))
        else if ToolName = 'get_stack_memory' then
          SendResponse(ID, HandleGetStackMemory(ToolParams))
        else if ToolName = 'read_global_variable' then
          SendResponse(ID, HandleReadGlobalVariable(ToolParams))
        else if ToolName = 'get_registers' then
          SendResponse(ID, HandleGetRegisters(ToolParams))
        else if ToolName = 'get_stack_slots' then
          SendResponse(ID, HandleGetStackSlots(ToolParams))
        else if ToolName = 'get_proc_asm' then
          SendResponse(ID, HandleGetProcAsm(ToolParams))
        else if ToolName = 'start_debug_session' then
          SendResponse(ID, HandleStartDebugSession(ToolParams))
        else if ToolName = 'step_into' then
          SendResponse(ID, HandleStepInto(ToolParams))
        else if ToolName = 'step_over' then
          SendResponse(ID, HandleStepOver(ToolParams))
        else if ToolName = 'get_state' then
          SendResponse(ID, HandleGetState(ToolParams))
        else if ToolName = 'stop_debug_session' then
          SendResponse(ID, HandleStopDebugSession(ToolParams))
        else if ToolName = 'terminate_debug_session' then
          SendResponse(ID, HandleTerminateDebugSession(ToolParams))
        else
          SendError(ID, -32601, 'Tool not found');
      end
      else
        SendError(ID, -32601, 'Method not found');
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      SendError(nil, -32603, E.Message);
  end;
end;

function TMcpServer.HandleListTools(AParams: TJSONObject): TJSONObject;
begin
  var ToolsArr := TJSONArray.Create;

  var ToolSetBP := TJSONObject.Create;
  ToolSetBP.AddPair('name', 'set_breakpoint');
  ToolSetBP.AddPair('description', 'Sets a hardware breakpoint at a specific line in a Delphi unit. The "unit" parameter is the source file name (e.g. "MyUnit.pas" or just "MyUnit" - the .pas extension is added automatically if omitted). Can be called before start_debug_session (breakpoints will be applied automatically on session start) or during a session. Maximum 4 hardware breakpoints. Returns an error if the unit/line cannot be resolved to a code address. Typical workflow: set_breakpoint -> start_debug_session -> continue -> get_state (poll until paused) -> inspect with get_stack_trace etc.');
  var SchemaSetBP := TJSONObject.Create;
  SchemaSetBP.AddPair('type', 'object');
  var PropSetBP := TJSONObject.Create;
  PropSetBP.AddPair('unit', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Delphi source file name, e.g. "MyUnit.pas" or "MyUnit" (.pas is added if omitted)'));
  PropSetBP.AddPair('line', TJSONObject.Create.AddPair('type', 'integer'));
  SchemaSetBP.AddPair('properties', PropSetBP);
  var ReqArr := TJSONArray.Create;
  ReqArr.Add('unit');
  ReqArr.Add('line');
  SchemaSetBP.AddPair('required', ReqArr);
  ToolSetBP.AddPair('inputSchema', SchemaSetBP);
  ToolsArr.Add(ToolSetBP);

  var ToolRemoveBP := TJSONObject.Create;
  ToolRemoveBP.AddPair('name', 'remove_breakpoint');
  ToolRemoveBP.AddPair('description', 'Removes an existing hardware breakpoint by unit name and line number. Can be called before or during a debug session.');
  var SchemaRemoveBP := TJSONObject.Create;
  SchemaRemoveBP.AddPair('type', 'object');
  var PropRemoveBP := TJSONObject.Create;
  PropRemoveBP.AddPair('unit', TJSONObject.Create.AddPair('type', 'string'));
  PropRemoveBP.AddPair('line', TJSONObject.Create.AddPair('type', 'integer'));
  SchemaRemoveBP.AddPair('properties', PropRemoveBP);
  var ReqRemoveArr := TJSONArray.Create;
  ReqRemoveArr.Add('unit');
  ReqRemoveArr.Add('line');
  SchemaRemoveBP.AddPair('required', ReqRemoveArr);
  ToolRemoveBP.AddPair('inputSchema', SchemaRemoveBP);
  ToolsArr.Add(ToolRemoveBP);

  var ToolListBP := TJSONObject.Create;
  ToolListBP.AddPair('name', 'list_breakpoints');
  ToolListBP.AddPair('description', 'Lists all currently set hardware breakpoints. Each entry includes unit, line, and status ("pending" if set before session start, "active" if resolved to an address).');
  ToolListBP.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolListBP);

  var ToolContinue := TJSONObject.Create;
  ToolContinue.AddPair('name', 'continue');
  ToolContinue.AddPair('description', 'Resumes execution of the debugged process. Returns immediately (non-blocking). The debugger sends a JSON-RPC notification {"method":"notifications/stopped","params":{"reason":"breakpoint"|"exited",...}} when execution pauses again or the process exits. If notifications are not available, poll with get_state until state is "paused" or "exited". Only callable when state is "paused".');
  ToolContinue.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolContinue);

  var ToolStepInto := TJSONObject.Create;
  ToolStepInto.AddPair('name', 'step_into');
  ToolStepInto.AddPair('description', 'Steps into the next source line (enters function calls). Returns immediately (non-blocking). The debugger sends a JSON-RPC notification {"method":"notifications/stopped","params":{"reason":"step",...}} when the step completes. If notifications are not available, poll with get_state until state is "paused". Only callable when state is "paused".');
  ToolStepInto.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolStepInto);

  var ToolStepOver := TJSONObject.Create;
  ToolStepOver.AddPair('name', 'step_over');
  ToolStepOver.AddPair('description', 'Steps over the current source line (does not enter function calls). Returns immediately (non-blocking). The debugger sends a JSON-RPC notification {"method":"notifications/stopped","params":{"reason":"step",...}} when the step completes. If notifications are not available, poll with get_state until state is "paused". Only callable when state is "paused".');
  ToolStepOver.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolStepOver);

  var ToolStack := TJSONObject.Create;
  ToolStack.AddPair('name', 'get_stack_trace');
  ToolStack.AddPair('description', 'Returns the current call stack as a list of frames with unit name, line number, and address. Only callable when state is "paused".');
  ToolStack.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolStack);

  var ToolReadMem := TJSONObject.Create;
  ToolReadMem.AddPair('name', 'read_memory');
  ToolReadMem.AddPair('description', 'Reads raw memory bytes from the debugged process at a given address. Returns a hex dump. To interpret the result, consider the data type and byte order (little-endian on x86). For example, 4 bytes for a 32-bit integer, pointer, or single float. Only callable when state is "paused".');
  var SchemaReadMem := TJSONObject.Create;
  SchemaReadMem.AddPair('type', 'object');
  var PropReadMem := TJSONObject.Create;
  PropReadMem.AddPair('address', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Hex address string, e.g. "00401000"'));
  PropReadMem.AddPair('size', TJSONObject.Create.AddPair('type', 'integer'));
  SchemaReadMem.AddPair('properties', PropReadMem);
  var ReqReadMem := TJSONArray.Create;
  ReqReadMem.Add('address');
  ReqReadMem.Add('size');
  SchemaReadMem.AddPair('required', ReqReadMem);
  ToolReadMem.AddPair('inputSchema', SchemaReadMem);
  ToolsArr.Add(ToolReadMem);

  var ToolStackMem := TJSONObject.Create;
  ToolStackMem.AddPair('name', 'get_stack_memory');
  ToolStackMem.AddPair('description', 'Reads the raw memory of the current stack frame (from ESP to EBP). Returns a hex dump of the stack contents. Useful for inspecting local variables and parameters. Only callable when state is "paused".');
  ToolStackMem.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolStackMem);

  var ToolReadGlobal := TJSONObject.Create;
  ToolReadGlobal.AddPair('name', 'read_global_variable');
  ToolReadGlobal.AddPair('description', 'Reads raw bytes of a global variable by its qualified name (e.g. "UnitName.VarName"). Returns a hex dump. You must specify the expected size in bytes (e.g. 4 for Integer/Pointer, 8 for Int64/Double, 256 for ShortString). Interpret the hex result according to the variable''s Delphi type and little-endian byte order. Only callable when state is "paused".');
  var SchemaReadGlobal := TJSONObject.Create;
  SchemaReadGlobal.AddPair('type', 'object');
  var PropReadGlobal := TJSONObject.Create;
  PropReadGlobal.AddPair('name', TJSONObject.Create.AddPair('type', 'string'));
  PropReadGlobal.AddPair('size', TJSONObject.Create.AddPair('type', 'integer'));
  SchemaReadGlobal.AddPair('properties', PropReadGlobal);
  var ReqReadGlobal := TJSONArray.Create;
  ReqReadGlobal.Add('name');
  ReqReadGlobal.Add('size');
  SchemaReadGlobal.AddPair('required', ReqReadGlobal);
  ToolReadGlobal.AddPair('inputSchema', SchemaReadGlobal);
  ToolsArr.Add(ToolReadGlobal);

  var ToolRegs := TJSONObject.Create;
  ToolRegs.AddPair('name', 'get_registers');
  ToolRegs.AddPair('description', 'Returns the current x86 CPU register values (EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP, EIP, EFLAGS). Only callable when state is "paused".');
  ToolRegs.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolRegs);

  var ToolSlots := TJSONObject.Create;
  ToolSlots.AddPair('name', 'get_stack_slots');
  ToolSlots.AddPair('description', 'Returns a structured list of stack slots from the current stack frame, with heuristic type interpretation (pointer, string, integer, etc.). Useful for quickly understanding local variables and parameters without manual hex interpretation. Only callable when state is "paused".');
  ToolSlots.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolSlots);

  var ToolAsm := TJSONObject.Create;
  ToolAsm.AddPair('name', 'get_proc_asm');
  ToolAsm.AddPair('description', 'Returns the raw assembly bytes (machine code) of the current procedure from its entry point to the return instruction. Useful for low-level analysis when source-level debugging is insufficient. Only callable when state is "paused".');
  ToolAsm.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolAsm);

  var ToolStart := TJSONObject.Create;
  ToolStart.AddPair('name', 'start_debug_session');
  ToolStart.AddPair('description', 'Starts a new debug session for the specified Delphi executable. The process is launched and paused at the entry point (state becomes "paused"). Any breakpoints set before this call are applied automatically. After this, use continue or step_* to advance execution. Only callable when state is "no_session".');
  var SchemaStart := TJSONObject.Create;
  SchemaStart.AddPair('type', 'object');
  var PropStart := TJSONObject.Create;
  PropStart.AddPair('executable_path', TJSONObject.Create.AddPair('type', 'string'));
  PropStart.AddPair('arguments', TJSONObject.Create.AddPair('type', 'string'));
  SchemaStart.AddPair('properties', PropStart);
  var ReqStart := TJSONArray.Create;
  ReqStart.Add('executable_path');
  SchemaStart.AddPair('required', ReqStart);
  ToolStart.AddPair('inputSchema', SchemaStart);
  ToolsArr.Add(ToolStart);

  var ToolGetState := TJSONObject.Create;
  ToolGetState.AddPair('name', 'get_state');
  ToolGetState.AddPair('description', 'Returns the current debugger state as a JSON object with "state" ("no_session", "paused", "running", "exited") and, when paused, "unit" and "line" indicating the current source location. Use this to poll after continue/step_into/step_over until state transitions from "running" to "paused" or "exited". Can be called at any time.');
  ToolGetState.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolGetState);

  var ToolStopSession := TJSONObject.Create;
  ToolStopSession.AddPair('name', 'stop_debug_session');
  ToolStopSession.AddPair('description', 'Detaches from the debugged process and ends the debug session. The process continues running normally with all hardware breakpoints removed. Use this when you want to release the process without killing it (e.g. after verifying behavior, or to let it finish naturally). State transitions to "no_session". A new session can be started afterwards with start_debug_session. Only callable when state is "paused" or "running".');
  ToolStopSession.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolStopSession);

  var ToolTerminateSession := TJSONObject.Create;
  ToolTerminateSession.AddPair('name', 'terminate_debug_session');
  ToolTerminateSession.AddPair('description', 'Kills the debugged process and ends the debug session. Use this when the process should not continue running (e.g. after finding a bug, or when restarting with different breakpoints). State transitions to "no_session". A new session can be started afterwards with start_debug_session. Only callable when state is "paused" or "running".');
  ToolTerminateSession.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolTerminateSession);

  Result := TJSONObject.Create;
  Result.AddPair('tools', ToolsArr);
end;

function TMcpServer.HandleSetBreakpoint(AParams: TJSONObject): TJSONObject;
var
  LLine: Integer;
  LUnit: String;
begin
  LUnit := AParams.GetValue('unit').Value;
  LLine := (AParams.GetValue('line') as TJSONNumber).AsInt;

  if ExtractFileExt(LUnit) = '' then
    LUnit := LUnit + '.pas';

  if GetBreakpointCount >= 4 then
    Exit(MakeErrorResult('Error: Maximum of 4 hardware breakpoints reached.'));

  if FState = dsNoSession then
  begin
    FPendingBreakpoints.Add(TBreakpoint.Create(LUnit, LLine, nil));
    Result := MakeTextResult(Format('Breakpoint pending at %s:%d (will be applied when debug session starts)', [LUnit, LLine]));
  end
  else
  begin
    if not RequireState([dsPaused, dsRunning], Result) then
      Exit;
    if not FDebugger.SetBreakpoint(LUnit, LLine, True) then
    begin
      if FDebugger.Breakpoints.Count >= 4 then
        Exit(MakeErrorResult('Error: Maximum of 4 hardware breakpoints reached.'))
      else
        Exit(MakeErrorResult(Format('Error: Could not resolve address for %s:%d. Verify that the unit name and line number are correct and that debug info is available.', [LUnit, LLine])));
    end;
    Result := MakeTextResult(Format('Breakpoint set at %s:%d', [LUnit, LLine]));
  end;
end;

function TMcpServer.HandleRemoveBreakpoint(AParams: TJSONObject): TJSONObject;
var
  I    : Integer;
  LLine: Integer;
  LUnit: String;
begin
  LUnit := AParams.GetValue('unit').Value;
  LLine := (AParams.GetValue('line') as TJSONNumber).AsInt;

  if ExtractFileExt(LUnit) = '' then
    LUnit := LUnit + '.pas';

  if FState = dsNoSession then
  begin
    for I := FPendingBreakpoints.Count - 1 downto 0 do
    begin
      if SameText(FPendingBreakpoints[I].UnitName, LUnit) and (FPendingBreakpoints[I].LineNumber = LLine) then
        FPendingBreakpoints.Delete(I);
    end;
    Result := MakeTextResult(Format('Breakpoint removed at %s:%d', [LUnit, LLine]));
  end
  else
  begin
    if not RequireState([dsPaused, dsRunning], Result) then
      Exit;
    FDebugger.RemoveBreakpoint(LUnit, LLine);
    Result := MakeTextResult(Format('Breakpoint removed at %s:%d', [LUnit, LLine]));
  end;
end;

function TMcpServer.HandleListBreakpoints(AParams: TJSONObject): TJSONObject;
var
  BPArr: TJSONArray;
  BPObj: TJSONObject;
  I    : Integer;
begin
  BPArr := TJSONArray.Create;
  try
    for I := 0 to FPendingBreakpoints.Count - 1 do
    begin
      BPObj := TJSONObject.Create;
      BPObj.AddPair('unit', FPendingBreakpoints[I].UnitName);
      BPObj.AddPair('line', TJSONNumber.Create(FPendingBreakpoints[I].LineNumber));
      BPObj.AddPair('status', 'pending');
      BPArr.Add(BPObj);
    end;

    if Assigned(FDebugger) then
    begin
      FDebugger.BreakpointLock.Enter;
      try
        for I := 0 to FDebugger.Breakpoints.Count - 1 do
        begin
          BPObj := TJSONObject.Create;
          BPObj.AddPair('unit', FDebugger.Breakpoints[I].UnitName);
          BPObj.AddPair('line', TJSONNumber.Create(FDebugger.Breakpoints[I].LineNumber));
          BPObj.AddPair('address', Format('%p', [FDebugger.Breakpoints[I].Address]));
          BPObj.AddPair('status', 'active');
          BPArr.Add(BPObj);
        end;
      finally
        FDebugger.BreakpointLock.Leave;
      end;
    end;

    Result := MakeTextResult(BPArr.ToJSON);
  finally
    BPArr.Free;
  end;
end;

function TMcpServer.HandleStartDebugSession(AParams: TJSONObject): TJSONObject;
var
  Args       : String;
  ExePath    : String;
  I          : Integer;
  MapFile    : String;
  Unresolved : String;
begin
  if not RequireState([dsNoSession], Result) then
    Exit;

  ExePath := AParams.GetValue('executable_path').Value;
  if not FileExists(ExePath) then
    Exit(MakeErrorResult('Error: Executable not found: ' + ExePath));

  var ArgsVal := AParams.GetValue('arguments');
  if ArgsVal <> nil then
    Args := ArgsVal.Value
  else
    Args := '';

  FDebugger := TDebugger.Create;
  FDebugger.OnException := DebuggerExceptionHandler;
  FDebugger.OnBreakpoint := DebuggerBreakpointHandler;
  FDebugger.OnStepped := DebuggerSteppedHandler;
  FDebugger.OnProcessExit := DebuggerProcessExitHandler;

  MapFile := ChangeFileExt(ExePath, '.map');
  if FileExists(MapFile) then
    FDebugger.LoadMapFile(MapFile);

  for I := 0 to FPendingBreakpoints.Count - 1 do
    FDebugger.SetBreakpoint(FPendingBreakpoints[I].UnitName, FPendingBreakpoints[I].LineNumber);
  FPendingBreakpoints.Clear;

  TDebuggerThread.Create(FDebugger, Trim(ExePath + ' ' + Args));

  FDebugger.WaitForReady(5000);
  FState := dsPaused;

  Unresolved := '';
  for I := 0 to FDebugger.Breakpoints.Count - 1 do
  begin
    if FDebugger.Breakpoints[I].Address = nil then
    begin
      if Unresolved <> '' then
        Unresolved := Unresolved + ', ';
      Unresolved := Unresolved + Format('%s:%d', [FDebugger.Breakpoints[I].UnitName, FDebugger.Breakpoints[I].LineNumber]);
    end;
  end;

  if Unresolved <> '' then
    Result := MakeTextResult('Debug session started for ' + ExePath +
      '. WARNING: Could not resolve address for breakpoint(s): ' + Unresolved +
      '. These breakpoints will not trigger. Verify unit names and line numbers.')
  else
  begin
    if not FileExists(MapFile) then
      Result := MakeTextResult('Debug session started for ' + ExePath +
        '. WARNING: No .map file found! Source-level debugging (setting breakpoints by line, stack trace with lines) will NOT work. ' +
        'You can rebuild the project with DPT to generate the map file using the following parameter: /p:DCC_MapFile=3 ' +
        '(e.g., DPT.exe LATEST Build Project.dproj Win32 Debug "/p:DCC_MapFile=3"). Alternatively, terminate this session, rebuild, and restart.')
    else
      Result := MakeTextResult('Debug session started for ' + ExePath);
  end;
end;

function TMcpServer.HandleContinue(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  FDebugger.ResumeExecution;
  FState := dsRunning;
  Result := MakeTextResult('Execution resumed. Waiting for breakpoint or process exit.');
end;

function TMcpServer.HandleStepInto(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  FDebugger.StepInto;
  FState := dsRunning;
  Result := MakeTextResult('Stepping into. Waiting for step to complete.');
end;

function TMcpServer.HandleStepOver(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  FDebugger.StepOver;
  FState := dsRunning;
  Result := MakeTextResult('Stepping over. Waiting for step to complete.');
end;

function TMcpServer.HandleGetState(AParams: TJSONObject): TJSONObject;
var
  Stack     : TArray<TStackFrame>;
  StateNames: Array[TDebugState] of String;
  StateObj  : TJSONObject;
begin
  StateNames[dsNoSession] := 'no_session';
  StateNames[dsPaused] := 'paused';
  StateNames[dsRunning] := 'running';
  StateNames[dsExited] := 'exited';

  StateObj := TJSONObject.Create;
  try
    StateObj.AddPair('state', StateNames[FState]);

    if (FState = dsPaused) and Assigned(FDebugger) then
    begin
      Stack := FDebugger.GetStackTrace(FDebugger.LastThreadHit);
      if Length(Stack) > 0 then
      begin
        StateObj.AddPair('unit', Stack[0].UnitName);
        StateObj.AddPair('line', TJSONNumber.Create(Stack[0].LineNumber));
        StateObj.AddPair('procedure', Stack[0].ProcedureName);
      end;
    end;

    Result := MakeTextResult(StateObj.ToJSON);
  finally
    StateObj.Free;
  end;
end;

function TMcpServer.HandleStopDebugSession(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused, dsRunning], Result) then
    Exit;

  DisconnectDebuggerEvents;
  FDebugger.Detach;
  FState := dsNoSession;
  FDebugger := nil;
  Result := MakeTextResult('Debug session stopped. The process continues running.');
end;

function TMcpServer.HandleTerminateDebugSession(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused, dsRunning], Result) then
    Exit;

  DisconnectDebuggerEvents;
  FDebugger.Terminate;
  FState := dsNoSession;
  FDebugger := nil;
  Result := MakeTextResult('Debug session terminated. The process has been killed.');
end;

function TMcpServer.HandleGetStackTrace(AParams: TJSONObject): TJSONObject;
var
  Frame    : TStackFrame;
  FramesArr: TJSONArray;
  Stack    : TArray<TStackFrame>;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  Stack := FDebugger.GetStackTrace(FDebugger.LastThreadHit);
  FramesArr := TJSONArray.Create;
  try
    for Frame in Stack do
    begin
      var FrameObj := TJSONObject.Create;
      FrameObj.AddPair('address', Format('%p', [Frame.Address]));
      FrameObj.AddPair('unit', Frame.UnitName);
      FrameObj.AddPair('procedure', Frame.ProcedureName);
      FrameObj.AddPair('line', TJSONNumber.Create(Frame.LineNumber));
      FramesArr.Add(FrameObj);
    end;
    Result := MakeTextResult(FramesArr.ToJSON);
  finally
    FramesArr.Free;
  end;
end;

function TMcpServer.HandleReadMemory(AParams: TJSONObject): TJSONObject;
var
  Addr    : UIntPtr;
  AddrStr : String;
  Data    : TBytes;
  Hex     : String;
  Size    : Integer;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  AddrStr := AParams.GetValue('address').Value;
  Addr := UIntPtr(StrToInt64Def('$' + AddrStr, 0));
  Size := (AParams.GetValue('size') as TJSONNumber).AsInt;

  Data := FDebugger.ReadProcessMemory(Pointer(Addr), Size);

  if Length(Data) > 0 then
  begin
    Hex := '';
    for var B in Data do Hex := Hex + IntToHex(B, 2) + ' ';
    Result := MakeTextResult(Hex.Trim);
  end
  else
    Result := MakeErrorResult('Failed to read memory');
end;

function TMcpServer.HandleGetStackMemory(AParams: TJSONObject): TJSONObject;
var
  Data : TBytes;
  Hex  : String;
  LSize: NativeUInt;
  Regs : TRegisters;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  Regs := FDebugger.GetRegisters(FDebugger.LastThreadHit);

  if (Regs.Esp <> 0) and (Regs.Ebp >= Regs.Esp) then
  begin
    LSize := Regs.Ebp - Regs.Esp + 16;
    if LSize > 4096 then LSize := 4096;
    Data := FDebugger.ReadProcessMemory(Pointer(Regs.Esp), LSize);

    Hex := Format('ESP: %p, EBP: %p' + sLineBreak, [Pointer(Regs.Esp), Pointer(Regs.Ebp)]);
    for var I := 0 to Length(Data) - 1 do
    begin
      if I mod 16 = 0 then Hex := Hex + sLineBreak + IntToHex(Regs.Esp + UIntPtr(I), 8) + ': ';
      Hex := Hex + IntToHex(Data[I], 2) + ' ';
    end;

    Result := MakeTextResult(Hex.Trim);
  end
  else
    Result := MakeErrorResult('Invalid stack registers or process not paused');
end;

function TMcpServer.HandleReadGlobalVariable(AParams: TJSONObject): TJSONObject;
var
  Addr: Pointer;
  Data: TBytes;
  Hex : String;
  Name: String;
  Size: Integer;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  Name := AParams.GetValue('name').Value;
  Size := (AParams.GetValue('size') as TJSONNumber).AsInt;

  Addr := FDebugger.GetAddressFromSymbol(Name);

  if Addr <> nil then
  begin
    Data := FDebugger.ReadProcessMemory(Addr, Size);
    if Length(Data) > 0 then
    begin
      Hex := '';
      for var B in Data do Hex := Hex + IntToHex(B, 2) + ' ';
      Result := MakeTextResult(Format('Address: %p, Value: %s', [Addr, Hex.Trim]));
    end
    else
      Result := MakeErrorResult('Failed to read memory at ' + Format('%p', [Addr]));
  end
  else
    Result := MakeErrorResult('Symbol not found: ' + Name);
end;

function TMcpServer.HandleGetRegisters(AParams: TJSONObject): TJSONObject;
var
  RegObj: TJSONObject;
  Regs  : TRegisters;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  Regs := FDebugger.GetRegisters(FDebugger.LastThreadHit);

  RegObj := TJSONObject.Create;
  try
    RegObj.AddPair('eip', Format('%p', [Pointer(Regs.Eip)]));
    RegObj.AddPair('esp', Format('%p', [Pointer(Regs.Esp)]));
    RegObj.AddPair('ebp', Format('%p', [Pointer(Regs.Ebp)]));
    RegObj.AddPair('eax', Format('%p', [Pointer(Regs.Eax)]));
    RegObj.AddPair('edx', Format('%p', [Pointer(Regs.Edx)]));
    RegObj.AddPair('ecx', Format('%p', [Pointer(Regs.Ecx)]));
    Result := MakeTextResult(RegObj.ToJSON);
  finally
    RegObj.Free;
  end;
end;

function TMcpServer.HandleGetStackSlots(AParams: TJSONObject): TJSONObject;
var
  FrameInfo: TStackFrameInfo;
  MetaObj  : TJSONObject;
  Slot     : TStackSlot;
  Slots    : TArray<TStackSlot>;
  SlotsArr : TJSONArray;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  Slots := FDebugger.GetStackSlots(FDebugger.LastThreadHit);
  FrameInfo := FDebugger.GetStackFrameInfo(FDebugger.LastThreadHit);

  Result := TJSONObject.Create;

  MetaObj := TJSONObject.Create;
  MetaObj.AddPair('procedure', FrameInfo.ProcedureName);
  MetaObj.AddPair('start_address', Format('%p', [FrameInfo.StartAddress]));
  MetaObj.AddPair('local_variable_size', TJSONNumber.Create(FrameInfo.LocalSize));
  Result.AddPair('frame_metadata', MetaObj);

  SlotsArr := TJSONArray.Create;
  for Slot in Slots do
  begin
    var SlotObj := TJSONObject.Create;
    SlotObj.AddPair('offset', TJSONNumber.Create(Slot.Offset));
    SlotObj.AddPair('address', Format('%p', [Slot.Address]));
    SlotObj.AddPair('value', Format('%p', [Pointer(Slot.Value)]));
    SlotObj.AddPair('interpretation', Slot.Interpretation);
    SlotsArr.Add(SlotObj);
  end;

  var ContentArr := TJSONArray.Create;
  ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', SlotsArr.ToJSON));
  Result.AddPair('content', ContentArr);
end;

function TMcpServer.HandleGetProcAsm(AParams: TJSONObject): TJSONObject;
var
  Data     : TBytes;
  FrameInfo: TStackFrameInfo;
  Hex      : String;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  FrameInfo := FDebugger.GetStackFrameInfo(FDebugger.LastThreadHit);
  if FrameInfo.StartAddress <> nil then
  begin
    Data := FDebugger.ReadProcessMemory(FrameInfo.StartAddress, 64);
    Hex := '';
    for var B in Data do Hex := Hex + IntToHex(B, 2) + ' ';
    Result := MakeTextResult(Format('Procedure: %s, Start: %p, Bytes: %s', [FrameInfo.ProcedureName, FrameInfo.StartAddress, Hex.Trim]));
  end
  else
    Result := MakeErrorResult('Could not find current procedure start.');
end;

procedure TMcpServer.RunOnce;
var
  Line: String;
begin
  if Assigned(FInputReader) then
    Line := FInputReader.ReadLine
  else
    System.Readln(System.Input, Line);

  if Line <> '' then
    ProcessMessage(Line);
end;

procedure TMcpServer.Run;
begin
  while not FExitRequest do
  begin
    if Assigned(FInputReader) then
    begin
      if FInputReader.Peek = -1 then
        Break;
    end
    else if System.EOF(System.Input) then
      Break;

    RunOnce;
  end;
end;

end.
