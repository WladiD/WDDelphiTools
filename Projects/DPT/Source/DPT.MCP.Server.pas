unit DPT.MCP.Server;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.JSON,
  mormot.core.base, mormot.core.json,
  DPT.Debugger;

type
  TMcpServer = class
  private
    FDebugger: TDebugger;
    FExitRequest: Boolean;
    FInputReader: TTextReader;
    FOutputWriter: TTextWriter;
    procedure ProcessMessage(const AMessage: string);
    procedure SendResponse(const AID: TJSONValue; AResult: TJSONObject);
    procedure SendError(const AID: TJSONValue; ACode: Integer; const AMessage: string);
    
    // Tool handlers
    function HandleSetBreakpoint(AParams: TJSONObject): TJSONObject;
    function HandleContinue(AParams: TJSONObject): TJSONObject;
    function HandleGetStackTrace(AParams: TJSONObject): TJSONObject;
    function HandleReadMemory(AParams: TJSONObject): TJSONObject;
    function HandleGetStackMemory(AParams: TJSONObject): TJSONObject;
    function HandleReadGlobalVariable(AParams: TJSONObject): TJSONObject;
    function HandleGetRegisters(AParams: TJSONObject): TJSONObject;
    function HandleGetStackSlots(AParams: TJSONObject): TJSONObject;
    function HandleStartDebugSession(AParams: TJSONObject): TJSONObject;

    procedure OnDebuggerException(Sender: TObject; const ExceptionRecord: TExceptionRecord; const FirstChance: Boolean; var Handled: Boolean);
  public
    constructor Create(ADebugger: TDebugger; AInput: TTextReader = nil; AOutput: TTextWriter = nil);
    destructor Destroy; override;
    procedure Run; // Main loop
    procedure RunOnce; // Processes one message
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

  if Assigned(FDebugger) then
    FDebugger.OnException := OnDebuggerException;
end;

destructor TMcpServer.Destroy;
begin
  inherited Destroy;
end;

procedure TMcpServer.SendResponse(const AID: TJSONValue; AResult: TJSONObject);
var
  Resp: TJSONObject;
begin
  Resp := TJSONObject.Create;
  Resp.AddPair('jsonrpc', '2.0');
  if AID <> nil then
    Resp.AddPair('id', AID.Clone as TJSONValue);
  Resp.AddPair('result', AResult);
  
  if Assigned(FOutputWriter) then
  begin
    FOutputWriter.WriteLine(Resp.ToJSON);
    FOutputWriter.Flush;
  end
  else
  begin
    System.Writeln(Resp.ToJSON);
    System.Flush(System.Output);
  end;
end;

procedure TMcpServer.SendError(const AID: TJSONValue; ACode: Integer; const AMessage: string);
var
  Resp, Err: TJSONObject;
begin
  Resp := TJSONObject.Create;
  Resp.AddPair('jsonrpc', '2.0');
  if AID <> nil then
    Resp.AddPair('id', AID.Clone as TJSONValue);
    
  Err := TJSONObject.Create;
  Err.AddPair('code', TJSONNumber.Create(ACode));
  Err.AddPair('message', AMessage);
  Resp.AddPair('error', Err);
  
  if Assigned(FOutputWriter) then
  begin
    FOutputWriter.WriteLine(Resp.ToJSON);
    FOutputWriter.Flush;
  end
  else
  begin
    System.Writeln(Resp.ToJSON);
    System.Flush(System.Output);
  end;
end;

procedure TMcpServer.OnDebuggerException(Sender: TObject; const ExceptionRecord: TExceptionRecord; const FirstChance: Boolean; var Handled: Boolean);
var
  Notif: TJSONObject;
  Params: TJSONObject;
  Stack: TArray<TStackFrame>;
begin
  Notif := TJSONObject.Create;
  Notif.AddPair('jsonrpc', '2.0');
  Notif.AddPair('method', 'notifications/debugger_exception');
  
  Params := TJSONObject.Create;
  Params.AddPair('code', Format('%08x', [ExceptionRecord.ExceptionCode]));
  Params.AddPair('address', Format('%p', [ExceptionRecord.ExceptionAddress]));
  Params.AddPair('firstChance', TJSONBool.Create(FirstChance));
  
  Stack := FDebugger.GetStackTrace(FDebugger.LastThreadHit);
  if Length(Stack) > 0 then
  begin
    Params.AddPair('unit', Stack[0].UnitName);
    Params.AddPair('line', TJSONNumber.Create(Stack[0].LineNumber));
    Params.AddPair('procedure', Stack[0].ProcedureName);
  end;
  
  Notif.AddPair('params', Params);
  
  if Assigned(FOutputWriter) then
  begin
    FOutputWriter.WriteLine(Notif.ToJSON);
    FOutputWriter.Flush;
  end
  else
  begin
    System.Writeln(Notif.ToJSON);
    System.Flush(System.Output);
  end;
  
  Handled := True;
end;

procedure TMcpServer.ProcessMessage(const AMessage: string);
var
  JSON: TJSONObject;
  ID: TJSONValue;
  MethodVal: TJSONValue;
  Method: string;
  Params: TJSONObject;
  ResultObj: TJSONObject;
begin
  try
    JSON := TJSONObject.ParseJSONValue(AMessage) as TJSONObject;
    if JSON = nil then Exit;
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
        ResultObj.AddPair('capabilities', TJSONObject.Create);
        ResultObj.AddPair('serverInfo', TJSONObject.Create.AddPair('name', 'DPT-Debugger').AddPair('version', '1.0.0'));
        SendResponse(ID, ResultObj);
      end
      else if Method = 'notifications/initialized' then
      begin
        // Handled
      end
      else if Method = 'tools/list' then
      begin
        // Return available tools
        var ToolsArr := TJSONArray.Create;
        
        var ToolSetBP := TJSONObject.Create;
        ToolSetBP.AddPair('name', 'set_breakpoint');
        ToolSetBP.AddPair('description', 'Sets a hardware breakpoint in a Delphi unit at a specific line');
        var SchemaSetBP := TJSONObject.Create;
        SchemaSetBP.AddPair('type', 'object');
        var PropSetBP := TJSONObject.Create;
        PropSetBP.AddPair('unit', TJSONObject.Create.AddPair('type', 'string'));
        PropSetBP.AddPair('line', TJSONObject.Create.AddPair('type', 'integer'));
        SchemaSetBP.AddPair('properties', PropSetBP);
        var ReqArr := TJSONArray.Create;
        ReqArr.Add('unit');
        ReqArr.Add('line');
        SchemaSetBP.AddPair('required', ReqArr);
        ToolSetBP.AddPair('inputSchema', SchemaSetBP);
        ToolsArr.Add(ToolSetBP);

        var ToolContinue := TJSONObject.Create;
        ToolContinue.AddPair('name', 'continue');
        ToolContinue.AddPair('description', 'Continues execution and waits for the next breakpoint or process exit');
        ToolContinue.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
        ToolsArr.Add(ToolContinue);

        var ToolStack := TJSONObject.Create;
        ToolStack.AddPair('name', 'get_stack_trace');
        ToolStack.AddPair('description', 'Returns the current call stack of the debugged process');
        ToolStack.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
        ToolsArr.Add(ToolStack);

        var ToolReadMem := TJSONObject.Create;
        ToolReadMem.AddPair('name', 'read_memory');
        ToolReadMem.AddPair('description', 'Reads a range of memory from the debugged process');
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
        ToolStackMem.AddPair('description', 'Reads the memory of the current stack frame (between ESP and EBP)');
        ToolStackMem.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
        ToolsArr.Add(ToolStackMem);

        var ToolReadGlobal := TJSONObject.Create;
        ToolReadGlobal.AddPair('name', 'read_global_variable');
        ToolReadGlobal.AddPair('description', 'Reads the value of a global variable by name (e.g. "UnitName.VarName")');
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
        ToolRegs.AddPair('description', 'Returns the current CPU registers');
        ToolRegs.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
        ToolsArr.Add(ToolRegs);

        var ToolSlots := TJSONObject.Create;
        ToolSlots.AddPair('name', 'get_stack_slots');
        ToolSlots.AddPair('description', 'Returns a list of stack slots with interpretation');
        ToolSlots.AddPair('inputSchema', TJSONObject.Create.AddPair('type', 'object').AddPair('properties', TJSONObject.Create));
        ToolsArr.Add(ToolSlots);

        var ToolStart := TJSONObject.Create;
        ToolStart.AddPair('name', 'start_debug_session');
        ToolStart.AddPair('description', 'Starts a new debug session for the specified executable');
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

        ResultObj := TJSONObject.Create;
        ResultObj.AddPair('tools', ToolsArr);
        SendResponse(ID, ResultObj);
      end
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
        else if ToolName = 'start_debug_session' then
          SendResponse(ID, HandleStartDebugSession(ToolParams))
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

function TMcpServer.HandleSetBreakpoint(AParams: TJSONObject): TJSONObject;
begin
  var LUnit := AParams.GetValue('unit').Value;
  var LLine := (AParams.GetValue('line') as TJSONNumber).AsInt;
  
  FDebugger.SetBreakpoint(LUnit, LLine);
  
  Result := TJSONObject.Create;
  var ContentArr := TJSONArray.Create;
  ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', Format('Breakpoint set at %s:%d', [LUnit, LLine])));
  Result.AddPair('content', ContentArr);
end;

function TMcpServer.HandleStartDebugSession(AParams: TJSONObject): TJSONObject;
var
  LExePath, LArgs, MapFile: string;
begin
  Result := TJSONObject.Create;
  var ContentArr := TJSONArray.Create;

  if Assigned(FDebugger) then
  begin
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', 'Error: A debug session is already active.'));
    Result.AddPair('content', ContentArr);
    Exit;
  end;

  LExePath := AParams.GetValue('executable_path').Value;
  if not FileExists(LExePath) then
  begin
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', 'Error: Executable not found: ' + LExePath));
    Result.AddPair('content', ContentArr);
    Exit;
  end;

  var ArgsVal := AParams.GetValue('arguments');
  if ArgsVal <> nil then
    LArgs := ArgsVal.Value
  else
    LArgs := '';

  FDebugger := TDebugger.Create;
  FDebugger.OnException := OnDebuggerException;

  MapFile := ChangeFileExt(LExePath, '.map');
  if FileExists(MapFile) then
    FDebugger.LoadMapFile(MapFile);

  TDebuggerThread.Create(FDebugger, Trim(LExePath + ' ' + LArgs));

  ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', 'Debug session started for ' + LExePath));
  Result.AddPair('content', ContentArr);
end;

function TMcpServer.HandleContinue(AParams: TJSONObject): TJSONObject;
var
  BP: TBreakpoint;
begin
  FDebugger.ResumeExecution;
  BP := FDebugger.WaitForBreakpoint;
  
  Result := TJSONObject.Create;
  var ContentArr := TJSONArray.Create;
  if BP <> nil then
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', Format('Paused at %s:%d', [BP.UnitName, BP.LineNumber])))
  else if FDebugger.LastException.ExceptionCode <> 0 then
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', Format('Paused due to exception: %08x', [FDebugger.LastException.ExceptionCode])))
  else
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', 'Execution finished or timed out'));
  Result.AddPair('content', ContentArr);
end;

function TMcpServer.HandleGetStackTrace(AParams: TJSONObject): TJSONObject;
var
  Stack: TArray<TStackFrame>;
  FramesArr: TJSONArray;
  Frame: TStackFrame;
begin
  Stack := FDebugger.GetStackTrace(FDebugger.LastThreadHit);
  
  Result := TJSONObject.Create;
  FramesArr := TJSONArray.Create;
  
  for Frame in Stack do
  begin
    var FrameObj := TJSONObject.Create;
    FrameObj.AddPair('address', Format('%p', [Frame.Address]));
    FrameObj.AddPair('unit', Frame.UnitName);
    FrameObj.AddPair('procedure', Frame.ProcedureName);
    FrameObj.AddPair('line', TJSONNumber.Create(Frame.LineNumber));
    FramesArr.Add(FrameObj);
  end;
  
  var ContentArr := TJSONArray.Create;
  ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', FramesArr.ToJSON));
  Result.AddPair('content', ContentArr);
end;

function TMcpServer.HandleReadMemory(AParams: TJSONObject): TJSONObject;
var
  LAddrStr: string;
  LAddr: UIntPtr;
  LSize: Integer;
  Data: TBytes;
  Hex: string;
begin
  LAddrStr := AParams.GetValue('address').Value;
  LAddr := UIntPtr(StrToInt64Def('$' + LAddrStr, 0));
  LSize := (AParams.GetValue('size') as TJSONNumber).AsInt;
  
  Data := FDebugger.ReadProcessMemory(Pointer(LAddr), LSize);
  
  Result := TJSONObject.Create;
  var ContentArr := TJSONArray.Create;
  
  if Length(Data) > 0 then
  begin
    Hex := '';
    for var B in Data do Hex := Hex + IntToHex(B, 2) + ' ';
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', Hex.Trim));
  end
  else
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', 'Failed to read memory'));
    
  Result.AddPair('content', ContentArr);
end;

function TMcpServer.HandleGetStackMemory(AParams: TJSONObject): TJSONObject;
var
  Regs: TRegisters;
  Data: TBytes;
  Hex: string;
  LSize: NativeUInt;
begin
  Regs := FDebugger.GetRegisters(FDebugger.LastThreadHit);
  
  Result := TJSONObject.Create;
  var ContentArr := TJSONArray.Create;
  
  if (Regs.Esp <> 0) and (Regs.Ebp >= Regs.Esp) then
  begin
    LSize := Regs.Ebp - Regs.Esp + 16; // Add some context after EBP
    if LSize > 4096 then LSize := 4096; // Limit to 4KB
    Data := FDebugger.ReadProcessMemory(Pointer(Regs.Esp), LSize);
    
    Hex := Format('ESP: %p, EBP: %p' + sLineBreak, [Pointer(Regs.Esp), Pointer(Regs.Ebp)]);
    for var I := 0 to Length(Data) - 1 do
    begin
      if I mod 16 = 0 then Hex := Hex + sLineBreak + IntToHex(Regs.Esp + UIntPtr(I), 8) + ': ';
      Hex := Hex + IntToHex(Data[I], 2) + ' ';
    end;
    
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', Hex.Trim));
  end
  else
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', 'Invalid stack registers or process not paused'));
    
  Result.AddPair('content', ContentArr);
end;

function TMcpServer.HandleReadGlobalVariable(AParams: TJSONObject): TJSONObject;
var
  LName: string;
  LSize: Integer;
  Addr: Pointer;
  Data: TBytes;
  Hex: string;
begin
  LName := AParams.GetValue('name').Value;
  LSize := (AParams.GetValue('size') as TJSONNumber).AsInt;
  
  Addr := FDebugger.GetAddressFromSymbol(LName);
  
  Result := TJSONObject.Create;
  var ContentArr := TJSONArray.Create;
  
  if Addr <> nil then
  begin
    Data := FDebugger.ReadProcessMemory(Addr, LSize);
    if Length(Data) > 0 then
    begin
      Hex := '';
      for var B in Data do Hex := Hex + IntToHex(B, 2) + ' ';
      ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', 
        Format('Address: %p, Value: %s', [Addr, Hex.Trim])));
    end
    else
      ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', 'Failed to read memory at ' + Format('%p', [Addr])));
  end
  else
    ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', 'Symbol not found: ' + LName));
    
  Result.AddPair('content', ContentArr);
end;

function TMcpServer.HandleGetRegisters(AParams: TJSONObject): TJSONObject;
var
  Regs: TRegisters;
  RegObj: TJSONObject;
begin
  Regs := FDebugger.GetRegisters(FDebugger.LastThreadHit);
  
  RegObj := TJSONObject.Create;
  RegObj.AddPair('eip', Format('%p', [Pointer(Regs.Eip)]));
  RegObj.AddPair('esp', Format('%p', [Pointer(Regs.Esp)]));
  RegObj.AddPair('ebp', Format('%p', [Pointer(Regs.Ebp)]));
  RegObj.AddPair('eax', Format('%p', [Pointer(Regs.Eax)]));
  RegObj.AddPair('edx', Format('%p', [Pointer(Regs.Edx)]));
  RegObj.AddPair('ecx', Format('%p', [Pointer(Regs.Ecx)]));
  
  Result := TJSONObject.Create;
  var ContentArr := TJSONArray.Create;
  ContentArr.Add(TJSONObject.Create.AddPair('type', 'text').AddPair('text', RegObj.ToJSON));
  Result.AddPair('content', ContentArr);
  RegObj.Free;
end;

function TMcpServer.HandleGetStackSlots(AParams: TJSONObject): TJSONObject;
var
  Slots: TArray<TStackSlot>;
  Slot: TStackSlot;
  SlotsArr: TJSONArray;
begin
  Slots := FDebugger.GetStackSlots(FDebugger.LastThreadHit);
  
  Result := TJSONObject.Create;
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

procedure TMcpServer.RunOnce;
var
  Line: string;
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
      if FInputReader.Peek = -1 then Break;
    end
    else if System.EOF(System.Input) then Break;
    
    RunOnce;
  end;
end;

end.