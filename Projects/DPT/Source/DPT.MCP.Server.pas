unit DPT.MCP.Server;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  mormot.core.base, mormot.core.json,
  DPT.Debugger;

type
  TMcpServer = class
  private
    FDebugger: TDebugger;
    FExitRequest: Boolean;
    procedure ProcessMessage(const AMessage: string);
    procedure SendResponse(const AID: TJSONValue; AResult: TJSONObject);
    procedure SendError(const AID: TJSONValue; ACode: Integer; const AMessage: string);
    
    // Tool handlers
    function HandleSetBreakpoint(AParams: TJSONObject): TJSONObject;
    function HandleContinue(AParams: TJSONObject): TJSONObject;
    function HandleGetStackTrace(AParams: TJSONObject): TJSONObject;
  public
    constructor Create(ADebugger: TDebugger);
    procedure Run; // Main loop reading from Stdin
  end;

implementation

{ TMcpServer }

constructor TMcpServer.Create(ADebugger: TDebugger);
begin
  inherited Create;
  FDebugger := ADebugger;
  FExitRequest := False;
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
  
  Writeln(Resp.ToJSON);
  Flush(Output);
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
  
  Writeln(Resp.ToJSON);
  Flush(Output);
end;

procedure TMcpServer.ProcessMessage(const AMessage: string);
var
  JSON: TJSONObject;
  ID: TJSONValue;
  Method: string;
  Params: TJSONObject;
  ResultObj: TJSONObject;
begin
  try
    JSON := TJSONObject.ParseJSONValue(AMessage) as TJSONObject;
    if JSON = nil then Exit;
    try
      ID := JSON.GetValue('id');
      Method := JSON.GetValue('method').Value;
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

        ResultObj := TJSONObject.Create;
        ResultObj.AddPair('tools', ToolsArr);
        SendResponse(ID, ResultObj);
      end
      else if Method = 'tools/call' then
      begin
        var ToolName := Params.GetValue('name').Value;
        var ToolParams := Params.GetValue('arguments') as TJSONObject;
        
        if ToolName = 'set_breakpoint' then
          SendResponse(ID, HandleSetBreakpoint(ToolParams))
        else if ToolName = 'continue' then
          SendResponse(ID, HandleContinue(ToolParams))
        else if ToolName = 'get_stack_trace' then
          SendResponse(ID, HandleGetStackTrace(ToolParams))
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

procedure TMcpServer.Run;
var
  Line: string;
begin
  while not FExitRequest do
  begin
    if EOF(Input) then Break;
    Readln(Input, Line);
    if Line = '' then Continue;
    ProcessMessage(Line);
  end;
end;

end.