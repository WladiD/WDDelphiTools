// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.MCP.Server;

interface

uses

  mormot.core.base,

  Winapi.Windows,

  System.Classes,
  System.JSON,
  System.SyncObjs,

  mormot.core.collections,
  mormot.core.json,

  DPT.Debugger;

type

  TDebugState = (dsNoSession, dsPaused, dsRunning, dsExited);

  TMcpServer = class
  private
    FCurrentExePath    : String;
    FDebugger          : TDebugger;
    FExitRequest       : Boolean;
    FInputReader       : TTextReader;
    FOutputCursorAtLastResume: Integer;
    FOwnsDebugger      : Boolean;
    FOutputLock        : TCriticalSection;
    FOutputWriter      : TTextWriter;
    FPendingBreakpoints: IList<TBreakpoint>;
    FPendingIgnoredExceptions: IList<String>;
    FPendingUnignoredExceptions: IList<String>;
    FState             : TDebugState;
    procedure ConnectDebuggerEvents;
    procedure DisconnectDebuggerEvents;
    function  GetBreakpointCount: Integer;
    function  MakeErrorResult(const AText: String): TJSONObject;
    function  MakeTextResult(const AText: String): TJSONObject;
    procedure ProcessMessage(const AMessage: String);
    function  RequireState(const AAllowed: Array of TDebugState; out AResult: TJSONObject): Boolean;
    function  CapturedOutputToJSONArray(const ALines: TArray<TCapturedOutputLine>): TJSONArray;
    procedure SendError(const AID: TJSONValue; ACode: Integer; const AMessage: String);
    procedure SendNotification(const AMethod: String; AParams: TJSONObject);
    procedure SendResponse(const AID: TJSONValue; AResult: TJSONObject);
    procedure SendSamplingRequest(const AText: String);
    procedure WriteOutput(const AJSON: String);
  private // Tool handlers
    function HandleContinue(AParams: TJSONObject): TJSONObject;
    function HandleGetLocals(AParams: TJSONObject): TJSONObject;
    function HandleGetOutput(AParams: TJSONObject): TJSONObject;
    function HandleGetProcAsm(AParams: TJSONObject): TJSONObject;
    function HandleGetRegisters(AParams: TJSONObject): TJSONObject;
    function HandleGetStackMemory(AParams: TJSONObject): TJSONObject;
    function HandleGetStackSlots(AParams: TJSONObject): TJSONObject;
    function HandleGetStackTrace(AParams: TJSONObject): TJSONObject;
    function HandleGetState(AParams: TJSONObject): TJSONObject;
    function HandleIgnoreException(AParams: TJSONObject): TJSONObject;
    function HandleListBreakpoints(AParams: TJSONObject): TJSONObject;
    function HandleListIgnoredExceptions(AParams: TJSONObject): TJSONObject;
    function HandleListThreads(AParams: TJSONObject): TJSONObject;
    function HandleListTools(AParams: TJSONObject): TJSONObject;
    function HandleEvaluate(AParams: TJSONObject): TJSONObject;
    function HandleReadMemory(AParams: TJSONObject): TJSONObject;
    function HandleRemoveBreakpoint(AParams: TJSONObject): TJSONObject;
    function HandleSetBreakpoint(AParams: TJSONObject): TJSONObject;
    function HandleStartDebugSession(AParams: TJSONObject): TJSONObject;
    function HandleStepInto(AParams: TJSONObject): TJSONObject;
    function HandleStepOver(AParams: TJSONObject): TJSONObject;
    function HandleStopDebugSession(AParams: TJSONObject): TJSONObject;
    function HandleSwitchThread(AParams: TJSONObject): TJSONObject;
    function HandleTerminateDebugSession(AParams: TJSONObject): TJSONObject;
    function HandleUnignoreException(AParams: TJSONObject): TJSONObject;
    function HandleWaitUntilPaused(AParams: TJSONObject): TJSONObject;
  private // Event handler
    procedure DebuggerBreakpointHandler(ASender: TObject; ABreakpoint: TBreakpoint);
    procedure DebuggerExceptionHandler(ASender: TObject; const AExceptionRecord: TExceptionRecord; const AFirstChance: Boolean; var AHandled: Boolean);
    procedure DebuggerProcessExitHandler(ASender: TObject; AExitCode: DWORD);
    procedure DebuggerSteppedHandler(ASender: TObject; ABreakpoint: TBreakpoint);
  public
    constructor Create(ADebugger: TDebugger; AInput: TTextReader = nil; AOutput: TTextWriter = nil);
    destructor  Destroy; override;
    procedure Run;
    procedure RunOnce;
    property State: TDebugState read FState;
  end;

implementation

uses

  System.SysUtils;

const

  DEBUG_STATE_NAMES: Array[TDebugState] of String = (
    'no_session', // dsNoSession
    'paused',     // dsPaused
    'running',    // dsRunning
    'exited'      // dsExited
  );

{ TMcpServer }

constructor TMcpServer.Create(ADebugger: TDebugger; AInput: TTextReader; AOutput: TTextWriter);
begin
  inherited Create;
  FDebugger := ADebugger;
  FOwnsDebugger := False;
  FExitRequest := False;
  FInputReader := AInput;
  FOutputWriter := AOutput;
  FOutputLock := TCriticalSection.Create;
  FPendingBreakpoints := Collections.NewList<TBreakpoint>;
  FPendingIgnoredExceptions := Collections.NewList<String>;
  FPendingUnignoredExceptions := Collections.NewList<String>;

  if Assigned(FDebugger) then
  begin
    ConnectDebuggerEvents;
    FState := dsPaused;
  end
  else
    FState := dsNoSession;
end;

destructor TMcpServer.Destroy;
begin
  DisconnectDebuggerEvents;
  if FOwnsDebugger then
    FDebugger.Free;
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

function TMcpServer.RequireState(const AAllowed: Array of TDebugState; out AResult: TJSONObject): Boolean;
var
  S: TDebugState;
begin
  for S in AAllowed do
    if FState = S then
      Exit(True);

  AResult := MakeErrorResult(Format('Error: Invalid state "%s" for this operation.', [DEBUG_STATE_NAMES[FState]]));
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
  Msg   : String;
  Params: TJSONObject;
begin
  FState := dsPaused;
  Params := TJSONObject.Create;
  Params.AddPair('reason', 'breakpoint');
  if ABreakpoint <> nil then
  begin
    Params.AddPair('unit', ABreakpoint.UnitName);
    Params.AddPair('line', TJSONNumber.Create(ABreakpoint.LineNumber));
    Msg := Format('The debugger stopped at a breakpoint in %s line %d.', [ABreakpoint.UnitName, ABreakpoint.LineNumber]);
  end
  else
    Msg := 'The debugger stopped at a breakpoint.';

  SendNotification('notifications/stopped', Params);
  SendSamplingRequest(Msg);
end;

procedure TMcpServer.DebuggerSteppedHandler(ASender: TObject; ABreakpoint: TBreakpoint);
var
  Msg   : String;
  Params: TJSONObject;
begin
  FState := dsPaused;
  Params := TJSONObject.Create;
  Params.AddPair('reason', 'step');
  if ABreakpoint <> nil then
  begin
    Params.AddPair('unit', ABreakpoint.UnitName);
    Params.AddPair('line', TJSONNumber.Create(ABreakpoint.LineNumber));
    Msg := Format('The debugger stopped after a step in %s line %d.', [ABreakpoint.UnitName, ABreakpoint.LineNumber]);
  end
  else
    Msg := 'The debugger stopped after a step.';

  SendNotification('notifications/stopped', Params);
  SendSamplingRequest(Msg);
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

procedure TMcpServer.ConnectDebuggerEvents;
begin
  if Assigned(FDebugger) then
  begin
    FDebugger.OnException := DebuggerExceptionHandler;
    FDebugger.OnBreakpoint := DebuggerBreakpointHandler;
    FDebugger.OnStepped := DebuggerSteppedHandler;
    FDebugger.OnProcessExit := DebuggerProcessExitHandler;
  end;
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
      if MethodVal = nil then
        Exit;
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
        ResultObj.AddPair('instructions',
          'DPT-Debugger - a hardware-breakpoint debugger for Delphi Win32/Win64 executables built with a .map file.' + sLineBreak +
          sLineBreak +
          'State model: no_session -> paused <-> running -> exited. Most inspection tools require "paused".' + sLineBreak +
          sLineBreak +
          'Typical workflow:' + sLineBreak +
          '  1. set_breakpoint(unit, line) - max 4 hardware breakpoints; can be queued before a session.' + sLineBreak +
          '  2. start_debug_session(executable_path) - process launches paused; pending breakpoints apply.' + sLineBreak +
          '  3. continue / step_into / step_over - non-blocking; always follow with wait_until_paused.' + sLineBreak +
          '  4. Inspect at a pause: get_stack_trace, get_registers, get_locals (named locals from TD32, preferred), get_stack_slots (heuristic fallback), read_memory, read_global_variable.' + sLineBreak +
          '     Output (Writeln/stderr/OutputDebugString) of the target is auto-attached to wait_until_paused/get_state as "recent_output" (only what was emitted since the last continue/step). Use get_output when you need older context than that delta.' + sLineBreak +
          '  5. stop_debug_session (detach) or terminate_debug_session (kill).' + sLineBreak +
          sLineBreak +
          'Addresses: hex without "0x" prefix; addresses from get_stack_trace / get_registers can be pasted directly into read_memory.' + sLineBreak +
          sLineBreak +
          'Architecture: x86 targets expose 32-bit e-registers; x64 targets expose 64-bit r-registers including r8-r15. Check the "arch" field in get_registers. Disassembly (get_proc_asm) returns raw opcode bytes - you must decode them yourself.' + sLineBreak +
          sLineBreak +
          'Requirements: a .map file next to the executable (build with /p:DCC_MapFile=3) is required for source-level breakpoints and named stack frames.');
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
        else if ToolName = 'ignore_exception' then
          SendResponse(ID, HandleIgnoreException(ToolParams))
        else if ToolName = 'unignore_exception' then
          SendResponse(ID, HandleUnignoreException(ToolParams))
        else if ToolName = 'list_ignored_exceptions' then
          SendResponse(ID, HandleListIgnoredExceptions(ToolParams))
        else if ToolName = 'list_threads' then
          SendResponse(ID, HandleListThreads(ToolParams))
        else if ToolName = 'switch_thread' then
          SendResponse(ID, HandleSwitchThread(ToolParams))
        else if ToolName = 'continue' then
          SendResponse(ID, HandleContinue(ToolParams))
        else if ToolName = 'get_stack_trace' then
          SendResponse(ID, HandleGetStackTrace(ToolParams))
        else if ToolName = 'read_memory' then
          SendResponse(ID, HandleReadMemory(ToolParams))
        else if ToolName = 'get_stack_memory' then
          SendResponse(ID, HandleGetStackMemory(ToolParams))
        else if ToolName = 'evaluate' then
          SendResponse(ID, HandleEvaluate(ToolParams))
        else if ToolName = 'get_registers' then
          SendResponse(ID, HandleGetRegisters(ToolParams))
        else if ToolName = 'get_stack_slots' then
          SendResponse(ID, HandleGetStackSlots(ToolParams))
        else if ToolName = 'get_locals' then
          SendResponse(ID, HandleGetLocals(ToolParams))
        else if ToolName = 'get_output' then
          SendResponse(ID, HandleGetOutput(ToolParams))
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
        else if ToolName = 'wait_until_paused' then
          SendResponse(ID, HandleWaitUntilPaused(ToolParams))
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
  ToolSetBP.AddPair('description', 'Sets a hardware breakpoint at a specific line in a Delphi unit. The "unit" parameter is the source file name (e.g. "MyUnit.pas" or just "MyUnit" - the .pas extension is added automatically if omitted). Can be called before start_debug_session (breakpoints will be applied automatically on session start) or during a session. Maximum 4 hardware breakpoints. Returns an error if the unit/line cannot be resolved to a code address. Typical workflow: set_breakpoint -> start_debug_session -> continue -> wait_until_paused -> inspect with get_stack_trace etc.');
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

  var ToolIgnoreExc := TJSONObject.Create;
  ToolIgnoreExc.AddPair('name', 'ignore_exception');
  ToolIgnoreExc.AddPair('description', 'Instructs the debugger to silently ignore a specific Delphi exception class (e.g. "EAbort") so it does not pause execution. "EAbort" is ignored by default. Can be called before or during a session.');
  var SchemaIgnoreExc := TJSONObject.Create;
  SchemaIgnoreExc.AddPair('type', 'object');
  var PropIgnoreExc := TJSONObject.Create;
  PropIgnoreExc.AddPair('class_name', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'The Delphi exception class name (e.g. "EAbort", "EZeroDivide")'));
  SchemaIgnoreExc.AddPair('properties', PropIgnoreExc);
  var ReqIgnoreExc := TJSONArray.Create;
  ReqIgnoreExc.Add('class_name');
  SchemaIgnoreExc.AddPair('required', ReqIgnoreExc);
  ToolIgnoreExc.AddPair('inputSchema', SchemaIgnoreExc);
  ToolsArr.Add(ToolIgnoreExc);

  var ToolUnignoreExc := TJSONObject.Create;
  ToolUnignoreExc.AddPair('name', 'unignore_exception');
  ToolUnignoreExc.AddPair('description', 'Instructs the debugger to stop ignoring a previously ignored Delphi exception class, causing it to pause execution again when raised.');
  var SchemaUnignoreExc := TJSONObject.Create;
  SchemaUnignoreExc.AddPair('type', 'object');
  var PropUnignoreExc := TJSONObject.Create;
  PropUnignoreExc.AddPair('class_name', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'The Delphi exception class name (e.g. "EAbort")'));
  SchemaUnignoreExc.AddPair('properties', PropUnignoreExc);
  var ReqUnignoreExc := TJSONArray.Create;
  ReqUnignoreExc.Add('class_name');
  SchemaUnignoreExc.AddPair('required', ReqUnignoreExc);
  ToolUnignoreExc.AddPair('inputSchema', SchemaUnignoreExc);
  ToolsArr.Add(ToolUnignoreExc);

  var ToolListIgnoredExc := TJSONObject.Create;
  ToolListIgnoredExc.AddPair('name', 'list_ignored_exceptions');
  ToolListIgnoredExc.AddPair('description', 'Lists all Delphi exception classes that are currently being ignored by the debugger.');
  ToolListIgnoredExc.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolListIgnoredExc);

  var ToolListThreads := TJSONObject.Create;
  ToolListThreads.AddPair('name', 'list_threads');
  ToolListThreads.AddPair('description', 'Lists all currently active thread IDs in the debugged process. Only callable when state is "paused".');
  ToolListThreads.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolListThreads);

  var ToolSwitchThread := TJSONObject.Create;
  ToolSwitchThread.AddPair('name', 'switch_thread');
  ToolSwitchThread.AddPair('description', 'Switches the debugger focus to a specific thread ID. Subsequent calls to get_stack_trace, get_registers, etc. will target this thread. Only callable when state is "paused".');
  var SchemaSwitchThread := TJSONObject.Create;
  SchemaSwitchThread.AddPair('type', 'object');
  var PropSwitchThread := TJSONObject.Create;
  PropSwitchThread.AddPair('thread_id', TJSONObject.Create.AddPair('type', 'integer').AddPair('description', 'The ID of the thread to switch to'));
  SchemaSwitchThread.AddPair('properties', PropSwitchThread);
  var ReqSwitchThread := TJSONArray.Create;
  ReqSwitchThread.Add('thread_id');
  SchemaSwitchThread.AddPair('required', ReqSwitchThread);
  ToolSwitchThread.AddPair('inputSchema', SchemaSwitchThread);
  ToolsArr.Add(ToolSwitchThread);

  var ToolContinue := TJSONObject.Create;
  ToolContinue.AddPair('name', 'continue');
  ToolContinue.AddPair('description', 'Resumes execution of the debugged process. Returns immediately (non-blocking). Call wait_until_paused next to block until execution pauses (breakpoint, exception) or the process exits. Only callable when state is "paused".');
  ToolContinue.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolContinue);

  var ToolStepInto := TJSONObject.Create;
  ToolStepInto.AddPair('name', 'step_into');
  ToolStepInto.AddPair('description', 'Steps into the next source line (enters function calls). Returns immediately (non-blocking). Call wait_until_paused next to block until the step completes. Only callable when state is "paused".');
  ToolStepInto.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolStepInto);

  var ToolStepOver := TJSONObject.Create;
  ToolStepOver.AddPair('name', 'step_over');
  ToolStepOver.AddPair('description', 'Steps over the current source line (does not enter function calls). Returns immediately (non-blocking). Call wait_until_paused next to block until the step completes. Only callable when state is "paused".');
  ToolStepOver.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolStepOver);

  var ToolStack := TJSONObject.Create;
  ToolStack.AddPair('name', 'get_stack_trace');
  ToolStack.AddPair('description', 'Returns the current call stack as a list of frames with unit name, line number, and address. Only callable when state is "paused".');
  ToolStack.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolStack);

  var ToolReadMem := TJSONObject.Create;
  ToolReadMem.AddPair('name', 'read_memory');
  ToolReadMem.AddPair('description', 'Reads raw memory bytes from the debugged process at a given address. Returns a hex dump (space-separated bytes). Byte order is little-endian. The address must be a hex string without "0x" prefix; 8-digit (x86) or 16-digit (x64) addresses from get_stack_trace/get_registers can be pasted directly. Only callable when state is "paused".');
  var SchemaReadMem := TJSONObject.Create;
  SchemaReadMem.AddPair('type', 'object');
  var PropReadMem := TJSONObject.Create;
  PropReadMem.AddPair('address', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Hex address string without "0x" prefix, e.g. "00401000" (x86) or "00007FF770C2CA4" (x64). Addresses from get_stack_trace and get_registers are in the correct format.'));
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

  var ToolEvaluate := TJSONObject.Create;
  ToolEvaluate.AddPair('name', 'evaluate');
  ToolEvaluate.AddPair('description', 'Evaluates a named variable (local or global) and returns its typed value. Searches locals first, then globals. Requires active paused debug session. Allowed types: "int" (4-byte signed), "int64" (8-byte signed), "string" (UnicodeString, the default Delphi string type), "ansistring", "widestring" (BSTR), "shortstring" (length-prefixed inline buffer; works for both locals and globals), "object" (returns "ClassName @ HexAddr" or "nil"). Returns an error when the variable name is unknown or the type is not in the supported list.');
  var SchemaEvaluate := TJSONObject.Create;
  SchemaEvaluate.AddPair('type', 'object');
  var PropEvaluate := TJSONObject.Create;
  PropEvaluate.AddPair('name', TJSONObject.Create.AddPair('type', 'string'));
  PropEvaluate.AddPair('type', TJSONObject.Create.AddPair('type', 'string').AddPair('description', 'Data type: "int", "int64", "string", or "object"'));
  SchemaEvaluate.AddPair('properties', PropEvaluate);
  var ReqEvaluate := TJSONArray.Create;
  ReqEvaluate.Add('name');
  ReqEvaluate.Add('type');
  SchemaEvaluate.AddPair('required', ReqEvaluate);
  ToolEvaluate.AddPair('inputSchema', SchemaEvaluate);
  ToolsArr.Add(ToolEvaluate);

  var ToolRegs := TJSONObject.Create;
  ToolRegs.AddPair('name', 'get_registers');
  ToolRegs.AddPair('description', 'Returns the current CPU register values of the focused thread as a JSON object. The "arch" field is "x86" or "x64". For x86 targets: eip, esp, ebp, eax, ebx, ecx, edx, esi, edi, eflags (8-digit hex). For x64 targets: rip, rsp, rbp, rax, rbx, rcx, rdx, rsi, rdi, r8..r15 (16-digit hex) and rflags (8-digit hex). Segment, XMM/YMM and FPU registers are not exposed. Only callable when state is "paused".');
  ToolRegs.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolRegs);

  var ToolSlots := TJSONObject.Create;
  ToolSlots.AddPair('name', 'get_stack_slots');
  ToolSlots.AddPair('description', 'Returns a structured list of stack slots from the current stack frame, with heuristic type interpretation (pointer, string, integer, etc.). Useful for quickly understanding local variables and parameters without manual hex interpretation. Only callable when state is "paused".');
  ToolSlots.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolSlots);

  var ToolOutput := TJSONObject.Create;
  ToolOutput.AddPair('name', 'get_output');
  ToolOutput.AddPair('description', 'Returns the most recent output the debugged process has produced (stdout, stderr, OutputDebugString) since the session started, as a JSON object with "total_buffered", "returned" and a "lines" array of {index, source, text}. The "source" field is one of "stdout"/"stderr"/"ods". Optional argument "tail" caps the number of returned lines (default 50; pass -1 for everything currently buffered). Optional argument "source_filter" is an array of strings (e.g. ["ods"]) to restrict the returned lines to specific sources. Lines are NOT removed from the buffer - the same output is also surfaced as "recent_output" inside wait_until_paused / get_state when the debugger pauses, scoped to the lines emitted since the last continue/step. Use get_output when you need a wider window (e.g. older context) than recent_output gives. Requires an active debug session.');
  var SchemaOutput := TJSONObject.Create;
  SchemaOutput.AddPair('type', 'object');
  var PropOutput := TJSONObject.Create;
  PropOutput.AddPair('tail', TJSONObject.Create
    .AddPair('type', 'integer')
    .AddPair('description', 'Maximum number of most-recent lines to return. Defaults to 50; pass -1 to return all currently buffered lines.'));
  PropOutput.AddPair('source_filter', TJSONObject.Create
    .AddPair('type', 'array')
    .AddPair('items', TJSONObject.Create.AddPair('type', 'string'))
    .AddPair('description', 'Optional array of source names ("stdout", "stderr", "ods") to filter the output.'));
  SchemaOutput.AddPair('properties', PropOutput);
  ToolOutput.AddPair('inputSchema', SchemaOutput);
  ToolsArr.Add(ToolOutput);

  var ToolLocals := TJSONObject.Create;
  ToolLocals.AddPair('name', 'get_locals');
  ToolLocals.AddPair('description', 'Returns the named local variables of the procedure that contains the current PC, as a JSON object with "procedure" and a "locals" array. Each local has "name", "bp_offset" (signed, EBP/RBP-relative) and "hex" (8 raw little-endian bytes read from the live process). Interpret the hex per the variable''s Delphi type: first 4 bytes for Integer/Cardinal, all 8 bytes for Int64/Pointer/Double. Requires TD32 debug info embedded in the EXE - this is the default for Debug builds (-V -$D+) but absent from -release- binaries. The tool returns a clear error message when no debug info is available, when the current PC is not inside a covered procedure (e.g. inside an RTL routine), or when the procedure has no recorded locals. Prefer this over get_stack_slots whenever possible: it gives source-level names and skips heuristic guessing. Only callable when state is "paused".');
  ToolLocals.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolLocals);

  var ToolAsm := TJSONObject.Create;
  ToolAsm.AddPair('name', 'get_proc_asm');
  ToolAsm.AddPair('description', 'Returns up to 64 raw machine-code bytes of the current procedure, starting at its entry point, as a hex string. These are NOT disassembled mnemonics - you must disassemble them yourself (x86/x64 opcode decoding). Useful only when you need low-level analysis and source-level debugging is insufficient. Only callable when state is "paused".');
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
  ToolGetState.AddPair('description', 'Returns the current debugger state instantly as a JSON object with "state" ("no_session", "paused", "running", "exited") and, when paused, "unit" and "line" indicating the current source location. Use wait_until_paused instead if you intend to poll after an execution movement command. Can be called at any time.');
  ToolGetState.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
  ToolsArr.Add(ToolGetState);

  var ToolWaitPaused := TJSONObject.Create;
  ToolWaitPaused.AddPair('name', 'wait_until_paused');
  ToolWaitPaused.AddPair('description', 'Waits for the debug session to reach a "paused" or "exited" state, then returns the same state object as get_state. Call this immediately after continue, step_into, or step_over. Eliminates the need for aggressive get_state polling. If the timeout expires before pausing, it returns the current state (e.g., "running").');
  var SchemaWaitPaused := TJSONObject.Create;
  SchemaWaitPaused.AddPair('type', 'object');
  var PropWaitPaused := TJSONObject.Create;
  PropWaitPaused.AddPair('timeout_ms', TJSONObject.Create.AddPair('type', 'integer').AddPair('description', 'Maximum time to wait in milliseconds. Defaults to 5000.'));
  SchemaWaitPaused.AddPair('properties', PropWaitPaused);
  ToolWaitPaused.AddPair('inputSchema', SchemaWaitPaused);
  ToolsArr.Add(ToolWaitPaused);

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
        Exit(MakeErrorResult('Error: Maximum of 4 hardware breakpoints reached.'));

      // The most common cause for an unresolvable line is a missing
      // .map file next to the executable. Detect that explicitly so
      // an AI agent does not waste a step verifying unit names.
      if (FCurrentExePath <> '') and
         (not FileExists(ChangeFileExt(FCurrentExePath, '.map'))) then
        Exit(MakeErrorResult(Format(
          'Error: Could not resolve address for %s:%d. ' +
          'No .map file was found next to the executable, which is the ' +
          'most likely cause: line-based breakpoints require it. ' +
          'Rebuild via DPT with /p:DCC_MapFile=3 ' +
          '(e.g. DPT.exe LATEST Build Project.dproj Win32 Debug "/p:DCC_MapFile=3"), ' +
          'or terminate this session, rebuild, and restart.',
          [LUnit, LLine])));

      Exit(MakeErrorResult(Format(
        'Error: Could not resolve address for %s:%d. ' +
        'A .map file is present, so verify that the unit name and line ' +
        'number are correct (the line must contain executable code, not a ' +
        'comment, blank line, or var declaration).',
        [LUnit, LLine])));
    end;
    Result := MakeTextResult(Format('Breakpoint set at %s:%d', [LUnit, LLine]));
  end;
end;

function TMcpServer.HandleRemoveBreakpoint(AParams: TJSONObject): TJSONObject;
var
  LLine: Integer;
  LUnit: String;
begin
  LUnit := AParams.GetValue('unit').Value;
  LLine := (AParams.GetValue('line') as TJSONNumber).AsInt;

  if ExtractFileExt(LUnit) = '' then
    LUnit := LUnit + '.pas';

  if FState = dsNoSession then
  begin
    for var I: Integer := FPendingBreakpoints.Count - 1 downto 0 do
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

function TMcpServer.HandleIgnoreException(AParams: TJSONObject): TJSONObject;
var
  LClass: String;
  I     : Integer;
begin
  LClass := AParams.GetValue('class_name').Value;
  if FState = dsNoSession then
  begin
    FPendingIgnoredExceptions.Add(LClass);
    for I := FPendingUnignoredExceptions.Count - 1 downto 0 do
      if SameText(FPendingUnignoredExceptions[I], LClass) then
        FPendingUnignoredExceptions.Delete(I);
    Result := MakeTextResult(Format('Exception %s will be ignored (pending session start)', [LClass]));
  end
  else
  begin
    if not RequireState([dsPaused, dsRunning], Result) then Exit;
    FDebugger.IgnoreException(LClass);
    Result := MakeTextResult(Format('Exception %s is now ignored', [LClass]));
  end;
end;

function TMcpServer.HandleUnignoreException(AParams: TJSONObject): TJSONObject;
var
  LClass: String;
  I     : Integer;
begin
  LClass := AParams.GetValue('class_name').Value;
  if FState = dsNoSession then
  begin
    FPendingUnignoredExceptions.Add(LClass);
    for I := FPendingIgnoredExceptions.Count - 1 downto 0 do
      if SameText(FPendingIgnoredExceptions[I], LClass) then
        FPendingIgnoredExceptions.Delete(I);
    Result := MakeTextResult(Format('Exception %s will not be ignored (pending session start)', [LClass]));
  end
  else
  begin
    if not RequireState([dsPaused, dsRunning], Result) then Exit;
    FDebugger.UnignoreException(LClass);
    Result := MakeTextResult(Format('Exception %s is no longer ignored', [LClass]));
  end;
end;

function TMcpServer.HandleListIgnoredExceptions(AParams: TJSONObject): TJSONObject;
var
  Arr     : TJSONArray;
  Defaults: IList<String>;
  I, J    : Integer;
  Found   : Boolean;
begin
  Arr := TJSONArray.Create;
  try
    if FState = dsNoSession then
    begin
      Defaults := Collections.NewList<String>;
      Defaults.Add('EAbort');
      for I := 0 to FPendingIgnoredExceptions.Count - 1 do
      begin
        Found := False;
        for J := 0 to Defaults.Count - 1 do
          if SameText(Defaults[J], FPendingIgnoredExceptions[I]) then
          begin
            Found := True;
            Break;
          end;
        if not Found then
          Defaults.Add(FPendingIgnoredExceptions[I]);
      end;

      for I := 0 to FPendingUnignoredExceptions.Count - 1 do
      begin
        for J := Defaults.Count - 1 downto 0 do
          if SameText(Defaults[J], FPendingUnignoredExceptions[I]) then
            Defaults.Delete(J);
      end;

      for I := 0 to Defaults.Count - 1 do
        Arr.Add(Defaults[I]);
    end
    else
    begin
      for I := 0 to FDebugger.IgnoredExceptions.Count - 1 do
        Arr.Add(FDebugger.IgnoredExceptions[I]);
    end;

    Result := MakeTextResult(Arr.ToJSON);
  finally
    Arr.Free;
  end;
end;

function TMcpServer.HandleStartDebugSession(AParams: TJSONObject): TJSONObject;
var
  Args       : String;
  ExePath    : String;
  HasMap     : Boolean;
  I          : Integer;
  MapFile    : String;
  Msg        : String;
  Unresolved : String;
begin
  if not RequireState([dsNoSession, dsExited], Result) then
    Exit;

  ExePath := AParams.GetValue('executable_path').Value;
  if not FileExists(ExePath) then
    Exit(MakeErrorResult('Error: Executable not found: ' + ExePath));

  var ArgsVal := AParams.GetValue('arguments');
  if ArgsVal <> nil then
    Args := ArgsVal.Value
  else
    Args := '';

  if FOwnsDebugger then
    FreeAndNil(FDebugger);

  FDebugger := TDebugger.Create;
  FOwnsDebugger := True;
  ConnectDebuggerEvents;
  FCurrentExePath := ExePath;
  FOutputCursorAtLastResume := 0;

  MapFile := ChangeFileExt(ExePath, '.map');
  HasMap := FileExists(MapFile);
  if HasMap then
    FDebugger.LoadMapFile(MapFile);

  // Best-effort load of TD32 debug info from the EXE itself; required by
  // get_locals. Silently no-ops when the EXE was stripped of debug info.
  FDebugger.LoadDebugInfoFromExe(ExePath);

  for I := 0 to FPendingBreakpoints.Count - 1 do
    FDebugger.SetBreakpoint(FPendingBreakpoints[I].UnitName, FPendingBreakpoints[I].LineNumber);
  FPendingBreakpoints.Clear;

  for I := 0 to FPendingIgnoredExceptions.Count - 1 do
    FDebugger.IgnoreException(FPendingIgnoredExceptions[I]);
  FPendingIgnoredExceptions.Clear;

  for I := 0 to FPendingUnignoredExceptions.Count - 1 do
    FDebugger.UnignoreException(FPendingUnignoredExceptions[I]);
  FPendingUnignoredExceptions.Clear;

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

  // Compose warnings cumulatively. The map-file warning is independent
  // of breakpoints: it always fires when the file is missing, so users
  // hear about it regardless of whether they tried to set breakpoints
  // already. When BOTH problems apply we present them as cause + effect
  // rather than two separate "verify names" hints.
  Msg := 'Debug session started for ' + ExePath;
  if not HasMap then
    Msg := Msg +
      '. WARNING: No .map file found next to the executable. ' +
      'Source-level debugging (setting breakpoints by line, named stack frames) will NOT work without it. ' +
      'Rebuild the project with DPT using /p:DCC_MapFile=3 ' +
      '(e.g., DPT.exe LATEST Build Project.dproj Win32 Debug "/p:DCC_MapFile=3"), ' +
      'or terminate this session, rebuild, and restart.';
  if Unresolved <> '' then
  begin
    if HasMap then
      Msg := Msg +
        '. WARNING: Could not resolve address for breakpoint(s): ' + Unresolved +
        '. These breakpoints will not trigger. Verify unit names and line numbers.'
    else
      Msg := Msg +
        ' As a consequence, the following breakpoint(s) will not trigger: ' + Unresolved + '.';
  end;
  Result := MakeTextResult(Msg);
end;

function TMcpServer.HandleContinue(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  // Snapshot the current output count: anything emitted from now until
  // the next pause becomes "recent_output" attached to that pause's
  // state response. Without this, every pause would re-deliver all
  // historical output.
  FOutputCursorAtLastResume := FDebugger.GetCapturedOutputCount;
  FDebugger.ResumeExecution;
  FState := dsRunning;
  Result := MakeTextResult('Execution resumed. Waiting for breakpoint or process exit.');
end;

function TMcpServer.HandleStepInto(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  FOutputCursorAtLastResume := FDebugger.GetCapturedOutputCount;
  FDebugger.StepInto;
  FState := dsRunning;
  Result := MakeTextResult('Stepping into. Waiting for step to complete.');
end;

function TMcpServer.HandleStepOver(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  FOutputCursorAtLastResume := FDebugger.GetCapturedOutputCount;
  FDebugger.StepOver;
  FState := dsRunning;
  Result := MakeTextResult('Stepping over. Waiting for step to complete.');
end;

function TMcpServer.HandleGetState(AParams: TJSONObject): TJSONObject;
var
  Stack    : TArray<TStackFrame>;
  StateObj : TJSONObject;
  RecentOut: TArray<TCapturedOutputLine>;
begin
  StateObj := TJSONObject.Create;
  try
    StateObj.AddPair('state', DEBUG_STATE_NAMES[FState]);

    if (FState = dsPaused) and Assigned(FDebugger) then
    begin
      StateObj.AddPair('thread_id', TJSONNumber.Create(FDebugger.LastThreadId));
      Stack := FDebugger.GetStackTrace(FDebugger.LastThreadHit);
      if Length(Stack) > 0 then
      begin
        StateObj.AddPair('unit', Stack[0].UnitName);
        StateObj.AddPair('line', TJSONNumber.Create(Stack[0].LineNumber));
        StateObj.AddPair('procedure', Stack[0].ProcedureName);
      end;

    end;

    // Delta-only output (paused or exited): include lines emitted since
    // the last continue/step. When nothing new arrived, the field is
    // omitted so the agent does not waste tokens on a redundant array.
    if (FState in [dsPaused, dsExited]) and Assigned(FDebugger) then
    begin
      RecentOut := FDebugger.GetCapturedOutput(FOutputCursorAtLastResume);
      if Length(RecentOut) > 0 then
        StateObj.AddPair('recent_output', CapturedOutputToJSONArray(RecentOut));
    end;

    Result := MakeTextResult(StateObj.ToJSON);
  finally
    StateObj.Free;
  end;
end;

function TMcpServer.HandleWaitUntilPaused(AParams: TJSONObject): TJSONObject;
var
  StartTick: UInt64;
  TimeoutMs: UInt64;
begin
  if not (Assigned(AParams) and AParams.TryGetValue<UInt64>('timeout_ms', TimeoutMs)) then
    TimeoutMs := 5000;

  StartTick := GetTickCount64;

  while not (FState in [dsPaused, dsExited]) do
  begin
    if (GetTickCount64 - StartTick) >= TimeoutMs then
      Break;
    Sleep(50);
  end;

  Result := HandleGetState(nil);
end;

function TMcpServer.HandleStopDebugSession(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused, dsRunning, dsExited], Result) then
    Exit;

  DisconnectDebuggerEvents;
  if FState <> dsExited then
    FDebugger.Detach;
  FState := dsNoSession;
  if FOwnsDebugger then
    FreeAndNil(FDebugger)
  else
    FDebugger := nil;
  FOwnsDebugger := False;
  FCurrentExePath := '';
  FOutputCursorAtLastResume := 0;
  Result := MakeTextResult('Debug session stopped. The process continues running.');
end;

function TMcpServer.HandleTerminateDebugSession(AParams: TJSONObject): TJSONObject;
begin
  if not RequireState([dsPaused, dsRunning, dsExited], Result) then
    Exit;

  DisconnectDebuggerEvents;
  if FState <> dsExited then
    FDebugger.Terminate;
  FState := dsNoSession;
  if FOwnsDebugger then
    FreeAndNil(FDebugger)
  else
    FDebugger := nil;
  FOwnsDebugger := False;
  FCurrentExePath := '';
  FOutputCursorAtLastResume := 0;
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

    if FDebugger.TargetIs32Bit then
      Hex := Format('ESP: %.8x, EBP: %.8x', [DWORD(Regs.Esp), DWORD(Regs.Ebp)]) + sLineBreak
    else
      Hex := Format('RSP: %.16x, RBP: %.16x', [Regs.Esp, Regs.Ebp]) + sLineBreak;
    for var I := 0 to Length(Data) - 1 do
    begin
      if I mod 16 = 0 then Hex := Hex + sLineBreak + IntToHex(Regs.Esp + UIntPtr(I), FDebugger.TargetPointerSize * 2) + ': ';
      Hex := Hex + IntToHex(Data[I], 2) + ' ';
    end;

    Result := MakeTextResult(Hex.Trim);
  end
  else
    Result := MakeErrorResult('Invalid stack registers or process not paused');
end;

function TMcpServer.HandleEvaluate(AParams: TJSONObject): TJSONObject;
var
  Name : String;
  VType: String;
  Value: String;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  Name := AParams.GetValue('name').Value;
  VType := AParams.GetValue('type').Value;

  if FDebugger.EvaluateVariable(Name, VType, Value) then
    Result := MakeTextResult(Format('Variable %s (%s): %s', [Name, VType, Value]))
  else
    Result := MakeErrorResult('Failed to evaluate variable: ' + Name);
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
    if FDebugger.TargetIs32Bit then
    begin
      RegObj.AddPair('arch', 'x86');
      RegObj.AddPair('eip', Format('%.8x', [DWORD(Regs.Eip)]));
      RegObj.AddPair('esp', Format('%.8x', [DWORD(Regs.Esp)]));
      RegObj.AddPair('ebp', Format('%.8x', [DWORD(Regs.Ebp)]));
      RegObj.AddPair('eax', Format('%.8x', [DWORD(Regs.Eax)]));
      RegObj.AddPair('ebx', Format('%.8x', [DWORD(Regs.Ebx)]));
      RegObj.AddPair('ecx', Format('%.8x', [DWORD(Regs.Ecx)]));
      RegObj.AddPair('edx', Format('%.8x', [DWORD(Regs.Edx)]));
      RegObj.AddPair('esi', Format('%.8x', [DWORD(Regs.Esi)]));
      RegObj.AddPair('edi', Format('%.8x', [DWORD(Regs.Edi)]));
      RegObj.AddPair('eflags', Format('%.8x', [Regs.EFlags]));
    end
    else
    begin
      RegObj.AddPair('arch', 'x64');
      RegObj.AddPair('rip', Format('%.16x', [Regs.Eip]));
      RegObj.AddPair('rsp', Format('%.16x', [Regs.Esp]));
      RegObj.AddPair('rbp', Format('%.16x', [Regs.Ebp]));
      RegObj.AddPair('rax', Format('%.16x', [Regs.Eax]));
      RegObj.AddPair('rbx', Format('%.16x', [Regs.Ebx]));
      RegObj.AddPair('rcx', Format('%.16x', [Regs.Ecx]));
      RegObj.AddPair('rdx', Format('%.16x', [Regs.Edx]));
      RegObj.AddPair('rsi', Format('%.16x', [Regs.Esi]));
      RegObj.AddPair('rdi', Format('%.16x', [Regs.Edi]));
      {$IFDEF CPUX64}
      RegObj.AddPair('r8',  Format('%.16x', [Regs.R8]));
      RegObj.AddPair('r9',  Format('%.16x', [Regs.R9]));
      RegObj.AddPair('r10', Format('%.16x', [Regs.R10]));
      RegObj.AddPair('r11', Format('%.16x', [Regs.R11]));
      RegObj.AddPair('r12', Format('%.16x', [Regs.R12]));
      RegObj.AddPair('r13', Format('%.16x', [Regs.R13]));
      RegObj.AddPair('r14', Format('%.16x', [Regs.R14]));
      RegObj.AddPair('r15', Format('%.16x', [Regs.R15]));
      {$ENDIF}
      RegObj.AddPair('rflags', Format('%.8x', [Regs.EFlags]));
    end;
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

function TMcpServer.CapturedOutputToJSONArray(
  const ALines: TArray<TCapturedOutputLine>): TJSONArray;
const
  SourceName: array[TCapturedOutputSource] of String = ('stdout', 'stderr', 'ods');
var
  I  : Integer;
  Obj: TJSONObject;
begin
  Result := TJSONArray.Create;
  for I := 0 to High(ALines) do
  begin
    Obj := TJSONObject.Create;
    Obj.AddPair('index', TJSONNumber.Create(ALines[I].Index));
    Obj.AddPair('source', SourceName[ALines[I].Source]);
    Obj.AddPair('text', ALines[I].Text);
    Result.Add(Obj);
  end;
end;

function TMcpServer.HandleGetOutput(AParams: TJSONObject): TJSONObject;
const
  DefaultTail = 50;
  SourceName: array[TCapturedOutputSource] of String = ('stdout', 'stderr', 'ods');
var
  Tail         : Integer;
  TailVal      : TJSONValue;
  FilterNode   : TJSONValue;
  FilterVal    : TJSONArray;
  FilterSources: set of TCapturedOutputSource;
  Lines        : TArray<TCapturedOutputLine>;
  StartIdx     : Integer;
  Sub          : TArray<TCapturedOutputLine>;
  ResultObj    : TJSONObject;
  ContentArr   : TJSONArray;
  I            : Integer;
  S            : String;
  Src          : TCapturedOutputSource;
begin
  if not Assigned(FDebugger) then
    Exit(MakeErrorResult(
      'No active debug session. Call start_debug_session before get_output.'));

  Tail := DefaultTail;
  FilterSources := [cosStdout, cosStderr, cosOds];

  if Assigned(AParams) then
  begin
    TailVal := AParams.GetValue('tail');
    if (TailVal <> nil) and (TailVal is TJSONNumber) then
      Tail := (TailVal as TJSONNumber).AsInt;

    FilterNode := AParams.GetValue('source_filter');
    if (FilterNode <> nil) and (FilterNode is TJSONArray) then
    begin
      FilterVal := FilterNode as TJSONArray;
      FilterSources := [];
      for I := 0 to FilterVal.Count - 1 do
      begin
        S := FilterVal.Items[I].Value;
        for Src := Low(TCapturedOutputSource) to High(TCapturedOutputSource) do
          if SameText(S, SourceName[Src]) then
            Include(FilterSources, Src);
      end;
    end;
  end;

  Lines := FDebugger.GetCapturedOutput(0);

  if FilterSources <> [cosStdout, cosStderr, cosOds] then
  begin
    SetLength(Sub, 0);
    for I := 0 to High(Lines) do
      if Lines[I].Source in FilterSources then
      begin
        SetLength(Sub, Length(Sub) + 1);
        Sub[High(Sub)] := Lines[I];
      end;
    Lines := Sub;
  end;

  if (Tail >= 0) and (Length(Lines) > Tail) then
  begin
    StartIdx := Length(Lines) - Tail;
    SetLength(Sub, Tail);
    for I := 0 to Tail - 1 do
      Sub[I] := Lines[StartIdx + I];
    Lines := Sub;
  end;

  ResultObj := TJSONObject.Create;
  try
    ResultObj.AddPair('total_buffered', TJSONNumber.Create(FDebugger.GetCapturedOutputCount));
    ResultObj.AddPair('returned', TJSONNumber.Create(Length(Lines)));
    ResultObj.AddPair('lines', CapturedOutputToJSONArray(Lines));

    Result := TJSONObject.Create;
    ContentArr := TJSONArray.Create;
    ContentArr.Add(TJSONObject.Create
      .AddPair('type', 'text')
      .AddPair('text', ResultObj.ToJSON));
    Result.AddPair('content', ContentArr);
  finally
    ResultObj.Free;
  end;
end;

function TMcpServer.HandleGetLocals(AParams: TJSONObject): TJSONObject;
var
  ContentArr: TJSONArray;
  Hex       : String;
  Loc       : TLocalVar;
  Locals    : TArray<TLocalVar>;
  LocalsArr : TJSONArray;
  LocObj    : TJSONObject;
  ProcName  : String;
  ResultObj : TJSONObject;
  I         : Integer;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  // Distinct error messages so an AI agent can branch on the cause:
  //   - no debug info loaded at all
  //   - PC outside any procedure with TD32 coverage (e.g. RTL or thunk)
  //   - procedure found but emitted no BPREL32 records
  if not Assigned(FDebugger.LocalsReader) or
     (FDebugger.LocalsReader.Procs.Count = 0) then
    Exit(MakeErrorResult(
      'No TD32 debug information available for the current executable. ' +
      'Rebuild the target as a Debug build (linker option -V together with ' +
      '-$D+) so the EXE carries an embedded FB09 stream. get_locals cannot ' +
      'work without it.'));

  ProcName := FDebugger.GetCurrentProcedureName(FDebugger.LastThreadHit);
  if ProcName = '' then
    Exit(MakeErrorResult(
      'Current PC is not inside any procedure that has TD32 debug info ' +
      '(typical inside RTL or import thunks). Step or continue until a ' +
      'breakpoint inside debugged user code, then call get_locals again.'));

  Locals := FDebugger.GetLocals(FDebugger.LastThreadHit);
  if Length(Locals) = 0 then
    Exit(MakeTextResult(Format(
      'Procedure "%s" has no recorded local variables (BPREL32 records). ' +
      'This is normal for parameterless leaf procedures or RTL stubs.',
      [ProcName])));

  ResultObj := TJSONObject.Create;
  try
    ResultObj.AddPair('procedure', ProcName);
    LocalsArr := TJSONArray.Create;
    for I := 0 to High(Locals) do
    begin
      Loc := Locals[I];
      Hex := '';
      if Length(Loc.RawBytes) > 0 then
      begin
        for var B in Loc.RawBytes do
          Hex := Hex + IntToHex(B, 2) + ' ';
        Hex := Hex.Trim;
      end;
      LocObj := TJSONObject.Create;
      LocObj.AddPair('name', Loc.Name);
      LocObj.AddPair('bp_offset', TJSONNumber.Create(Loc.BpOffset));
      LocObj.AddPair('hex', Hex);
      LocalsArr.Add(LocObj);
    end;
    ResultObj.AddPair('locals', LocalsArr);

    // Wrap in MCP tool-result content envelope; mirrors get_stack_slots.
    Result := TJSONObject.Create;
    ContentArr := TJSONArray.Create;
    ContentArr.Add(TJSONObject.Create
      .AddPair('type', 'text')
      .AddPair('text', ResultObj.ToJSON));
    Result.AddPair('content', ContentArr);
  finally
    ResultObj.Free;
  end;
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
    for var B in Data do
      Hex := Hex + IntToHex(B, 2) + ' ';
    Result := MakeTextResult(Format('Procedure: %s, Start: %p, Bytes: %s', [FrameInfo.ProcedureName, FrameInfo.StartAddress, Hex.Trim]));
  end
  else
    Result := MakeErrorResult('Could not find current procedure start.');
end;

function TMcpServer.HandleListThreads(AParams: TJSONObject): TJSONObject;
var
  Arr: TJSONArray;
  Tid: DWORD;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  Arr := TJSONArray.Create;
  try
    for Tid in FDebugger.GetThreadIds do
      Arr.Add(Tid);
    Result := MakeTextResult(Arr.ToJSON);
  finally
    Arr.Free;
  end;
end;

function TMcpServer.HandleSwitchThread(AParams: TJSONObject): TJSONObject;
var
  Tid: DWORD;
begin
  if not RequireState([dsPaused], Result) then
    Exit;

  Tid := (AParams.GetValue('thread_id') as TJSONNumber).AsInt64;

  if FDebugger.SetThreadFocus(Tid) then
    Result := MakeTextResult(Format('Successfully switched focus to thread %d', [Tid]))
  else
    Result := MakeErrorResult(Format('Error: Thread ID %d not found or not active', [Tid]));
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
