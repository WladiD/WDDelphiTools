unit DPT.Debugger;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  JclDebug,
  DPT.Logger;

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall; external kernel32;

const
  THREAD_GET_CONTEXT = $0008;
  THREAD_SET_CONTEXT = $0010;

type
  TBreakpoint = class
  public
    UnitName: string;
    LineNumber: Integer;
    Address: Pointer;
    Slot: Integer; // 0..3 for DR0..DR3
    IsActive: Boolean;
    constructor Create(const AUnitName: string; ALineNumber: Integer; AAddress: Pointer);
  end;

  TStackFrame = record
    Address: Pointer;
    UnitName: string;
    ProcedureName: string;
    LineNumber: Integer;
  end;

  TRegisters = record
    Eip: UIntPtr;
    Esp: UIntPtr;
    Ebp: UIntPtr;
    Eax, Ebx, Ecx, Edx: UIntPtr;
    Esi, Edi: UIntPtr;
    EFlags: DWORD;
  end;

  TOnBreakpointEvent = procedure(Sender: TObject; Breakpoint: TBreakpoint) of object;

  TDebugger = class
  private
    FProcessId: DWORD;
    FThreadId: DWORD;
    FProcessHandle: THandle;
    FThreadHandle: THandle;
    FBaseAddress: UIntPtr;
    FMapScanner: TJclMapScanner;
    FBreakpoints: TObjectList<TBreakpoint>;
    FActiveThreads: TList<THandle>;
    FOnBreakpoint: TOnBreakpointEvent;
    
    FContinueEvent: TEvent;
    FBreakpointHitEvent: TEvent;
    FLastBreakpointHit: TBreakpoint;
    FLastThreadHit: THandle;
    
    procedure HandleException(const ADebugEvent: TDebugEvent; var AContinueStatus: DWORD);
    procedure HandleCreateProcess(const ADebugEvent: TDebugEvent);
    procedure HandleCreateThread(const ADebugEvent: TDebugEvent);
    procedure HandleExitThread(const ADebugEvent: TDebugEvent);
    procedure ApplyBreakpointsToThread(AThreadHandle: THandle);
    procedure SetHardwareBreakpointInContext(var AContext: TContext; AAddress: Pointer; ASlot: Integer);
    function ReadProcessMemoryPtr(AAddress: Pointer): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadMapFile(const AMapFileName: string);
    function GetAddressFromUnitLine(const AUnitName: string; ALineNumber: Integer): Pointer;
    
    procedure SetBreakpoint(const AUnitName: string; ALineNumber: Integer);
    procedure StartDebugging(const AExecutablePath: string);
    
    procedure ResumeExecution;
    function WaitForBreakpoint(Timeout: DWORD = INFINITE): TBreakpoint;
    
    function GetStackTrace(AThreadHandle: THandle): TArray<TStackFrame>;
    function GetRegisters(AThreadHandle: THandle): TRegisters;
    function GetAddressFromSymbol(const ASymbolName: string): Pointer;
    function ReadProcessMemory(AAddress: Pointer; ASize: NativeUInt): TBytes;
    
    property OnBreakpoint: TOnBreakpointEvent read FOnBreakpoint write FOnBreakpoint;
    property LastThreadHit: THandle read FLastThreadHit;
  end;

  TDebuggerThread = class(TThread)
  private
    FDebugger: TDebugger;
    FExecutablePath: string;
  protected
    procedure Execute; override;
  public
    constructor Create(ADebugger: TDebugger; const AExecutablePath: string);
  end;

implementation

{ TDebuggerThread }

constructor TDebuggerThread.Create(ADebugger: TDebugger; const AExecutablePath: string);
begin
  inherited Create(False);
  FDebugger := ADebugger;
  FExecutablePath := AExecutablePath;
  FreeOnTerminate := True;
end;

procedure TDebuggerThread.Execute;
begin
  try
    FDebugger.StartDebugging(FExecutablePath);
  except
    on E: Exception do
      Writeln('Debugger Thread Error: ' + E.Message);
  end;
end;

{ TBreakpoint }

constructor TBreakpoint.Create(const AUnitName: string; ALineNumber: Integer; AAddress: Pointer);
begin
  inherited Create;
  UnitName := AUnitName;
  LineNumber := ALineNumber;
  Address := AAddress;
  Slot := -1;
  IsActive := False;
end;

{ TDebugger }

constructor TDebugger.Create;
begin
  inherited Create;
  FBreakpoints := TObjectList<TBreakpoint>.Create(True);
  FActiveThreads := TList<THandle>.Create;
  FContinueEvent := TEvent.Create(nil, False, False, '');
  FBreakpointHitEvent := TEvent.Create(nil, False, False, '');
end;

destructor TDebugger.Destroy;
begin
  FBreakpointHitEvent.Free;
  FContinueEvent.Free;
  FActiveThreads.Free;
  FBreakpoints.Free;
  FMapScanner.Free;
  if FProcessHandle <> 0 then CloseHandle(FProcessHandle);
  if FThreadHandle <> 0 then CloseHandle(FThreadHandle);
  inherited Destroy;
end;

procedure TDebugger.ResumeExecution;
begin
  FContinueEvent.SetEvent;
end;

function TDebugger.WaitForBreakpoint(Timeout: DWORD): TBreakpoint;
begin
  FBreakpointHitEvent.ResetEvent;
  if FBreakpointHitEvent.WaitFor(Timeout) = wrSignaled then
    Result := FLastBreakpointHit
  else
    Result := nil;
end;

function TDebugger.ReadProcessMemoryPtr(AAddress: Pointer): Pointer;
var
  BytesRead: NativeUInt;
begin
  Result := nil;
  Winapi.Windows.ReadProcessMemory(FProcessHandle, AAddress, @Result, SizeOf(Pointer), BytesRead);
end;

function TDebugger.ReadProcessMemory(AAddress: Pointer; ASize: NativeUInt): TBytes;
var
  BytesRead: NativeUInt;
begin
  SetLength(Result, ASize);
  if (ASize > 0) and (not Winapi.Windows.ReadProcessMemory(FProcessHandle, AAddress, @Result[0], ASize, BytesRead)) then
    SetLength(Result, 0);
end;

function TDebugger.GetAddressFromSymbol(const ASymbolName: string): Pointer;
var
  DotPos: Integer;
  UnitName, SymbolName: string;
  VA: DWORD;
begin
  Result := nil;
  if not Assigned(FMapScanner) then Exit;

  DotPos := Pos('.', ASymbolName);
  if DotPos > 0 then
  begin
    UnitName := Copy(ASymbolName, 1, DotPos - 1);
    SymbolName := Copy(ASymbolName, DotPos + 1, MaxInt);
    VA := FMapScanner.VAFromUnitAndProcName(UnitName, SymbolName);
    if VA <> 0 then
      Result := Pointer(FBaseAddress + $1000 + VA);
  end;
end;

function TDebugger.GetRegisters(AThreadHandle: THandle): TRegisters;
var
  Context: TContext;
begin
  FillChar(Result, SizeOf(Result), 0);
  Context.ContextFlags := CONTEXT_FULL;
  if GetThreadContext(AThreadHandle, Context) then
  begin
    {$IFDEF CPUX64}
    Result.Eip := Context.Rip;
    Result.Esp := Context.Rsp;
    Result.Ebp := Context.Rbp;
    Result.Eax := Context.Rax;
    Result.Ebx := Context.Rbx;
    Result.Ecx := Context.Rcx;
    Result.Edx := Context.Rdx;
    Result.Esi := Context.Rsi;
    Result.Edi := Context.Rdi;
    {$ELSE}
    Result.Eip := Context.Eip;
    Result.Esp := Context.Esp;
    Result.Ebp := Context.Ebp;
    Result.Eax := Context.Eax;
    Result.Ebx := Context.Ebx;
    Result.Ecx := Context.Ecx;
    Result.Edx := Context.Edx;
    Result.Esi := Context.Esi;
    Result.Edi := Context.Edi;
    {$ENDIF}
    Result.EFlags := Context.EFlags;
  end;
end;

function TDebugger.GetStackTrace(AThreadHandle: THandle): TArray<TStackFrame>;
var
  Context: TContext;
  FramePtr, ReturnAddr: Pointer;
  Frame: TStackFrame;
  Frames: TList<TStackFrame>;
  RelativeVA: DWORD;
begin
  Frames := TList<TStackFrame>.Create;
  try
    Context.ContextFlags := CONTEXT_CONTROL or CONTEXT_INTEGER;
    if not GetThreadContext(AThreadHandle, Context) then
      Exit(nil);

    {$IFDEF CPUX64}
    FramePtr := Pointer(Context.Rbp);
    ReturnAddr := Pointer(Context.Rip);
    {$ELSE}
    FramePtr := Pointer(Context.Ebp);
    ReturnAddr := Pointer(Context.Eip);
    {$ENDIF}

    while (Frames.Count < 50) and (UIntPtr(ReturnAddr) > FBaseAddress) do
    begin
      Frame.Address := ReturnAddr;
      Frame.UnitName := 'unknown';
      Frame.ProcedureName := 'unknown';
      Frame.LineNumber := 0;

      if Assigned(FMapScanner) then
      begin
        // VA in JclMapScanner is usually relative to ModuleBase + $1000
        RelativeVA := UIntPtr(ReturnAddr) - FBaseAddress - $1000;
        
        Frame.UnitName := FMapScanner.ModuleNameFromAddr(RelativeVA);
        Frame.ProcedureName := FMapScanner.ProcNameFromAddr(RelativeVA);
        Frame.LineNumber := FMapScanner.LineNumberFromAddr(RelativeVA);
        
        Writeln(Format('  Stack Walking: Addr=%p, VA=%08x, Unit=%s, Proc=%s, Line=%d', 
          [Frame.Address, RelativeVA, Frame.UnitName, Frame.ProcedureName, Frame.LineNumber]));
      end;

      Frames.Add(Frame);

      // Walk to next frame (Win32 specific logic)
      if FramePtr = nil then Break;
      
      ReturnAddr := ReadProcessMemoryPtr(Pointer(UIntPtr(FramePtr) + SizeOf(Pointer)));
      FramePtr := ReadProcessMemoryPtr(FramePtr);
      
      if ReturnAddr = nil then Break;
    end;

    Result := Frames.ToArray;
  finally
    Frames.Free;
  end;
end;

procedure TDebugger.LoadMapFile(const AMapFileName: string);
begin
  FreeAndNil(FMapScanner);
  if FileExists(AMapFileName) then
    FMapScanner := TJclMapScanner.Create(AMapFileName, 0);
end;

function TDebugger.GetAddressFromUnitLine(const AUnitName: string; ALineNumber: Integer): Pointer;
var
  I: Integer;
  LineInfo: TJclMapLineNumber;
  UnitMatch: string;
  SearchUnit: string;
begin
  Result := nil;
  if not Assigned(FMapScanner) then Exit;

  SearchUnit := ChangeFileExt(AUnitName, ''); // Strip .pas or .dpr

  for I := 0 to FMapScanner.LineNumbersCnt - 1 do
  begin
    LineInfo := FMapScanner.LineNumberByIndex[I];
    if LineInfo.LineNumber = ALineNumber then
    begin
      UnitMatch := TJclMapScanner.MapStringToStr(LineInfo.UnitName);
      if SameText(UnitMatch, SearchUnit) or SameText(ExtractFileName(UnitMatch), SearchUnit) then
      begin
        // VA is relative to module base address + $1000 for Delphi 2005+ (Win32)
        Result := Pointer(FBaseAddress + $1000 + LineInfo.VA);
        Exit;
      end;
    end;
  end;
end;

procedure TDebugger.SetHardwareBreakpointInContext(var AContext: TContext; AAddress: Pointer; ASlot: Integer);
begin
  AContext.ContextFlags := AContext.ContextFlags or CONTEXT_DEBUG_REGISTERS;
  
  case ASlot of
    0: AContext.Dr0 := UIntPtr(AAddress);
    1: AContext.Dr1 := UIntPtr(AAddress);
    2: AContext.Dr2 := UIntPtr(AAddress);
    3: AContext.Dr3 := UIntPtr(AAddress);
  end;

  // DR7: Bit 0, 2, 4, 6 are Local Enable for DR0, DR1, DR2, DR3
  AContext.Dr7 := AContext.Dr7 or (1 shl (ASlot * 2));
  
  // DR7: Bits 16-31 control Condition and Size. 
  // For execution breakpoint: Condition = 00, Size = 00.
  AContext.Dr7 := AContext.Dr7 and not ($F shl (16 + ASlot * 4));
  
  // DR7: Bits 8 (LE) and 9 (GE) for exact match
  AContext.Dr7 := AContext.Dr7 or $300;
end;

procedure TDebugger.ApplyBreakpointsToThread(AThreadHandle: THandle);
var
  Context: TContext;
  I: Integer;
  BPCount: Integer;
begin
  Context.ContextFlags := CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS;
  if not GetThreadContext(AThreadHandle, Context) then Exit;

  BPCount := 0;
  for I := 0 to FBreakpoints.Count - 1 do
  begin
    if (FBreakpoints[I].Address <> nil) and (BPCount < 4) then
    begin
      FBreakpoints[I].Slot := BPCount;
      SetHardwareBreakpointInContext(Context, FBreakpoints[I].Address, BPCount);
      Inc(BPCount);
    end;
  end;

  SetThreadContext(AThreadHandle, Context);
end;

procedure TDebugger.SetBreakpoint(const AUnitName: string; ALineNumber: Integer);
var
  Addr: Pointer;
  BP: TBreakpoint;
  Thread: THandle;
begin
  if FBreakpoints.Count >= 4 then
  begin
    Writeln('Error: Maximum of 4 hardware breakpoints reached.');
    Exit;
  end;

  Addr := nil;
  if FBaseAddress <> 0 then
    Addr := GetAddressFromUnitLine(AUnitName, ALineNumber);

  BP := TBreakpoint.Create(AUnitName, ALineNumber, Addr);
  FBreakpoints.Add(BP);

  if Addr <> nil then
  begin
    for Thread in FActiveThreads do
      ApplyBreakpointsToThread(Thread);
  end;
end;

procedure TDebugger.HandleCreateProcess(const ADebugEvent: TDebugEvent);
var
  I: Integer;
begin
  FBaseAddress := UIntPtr(ADebugEvent.CreateProcessInfo.lpBaseOfImage);
  FProcessHandle := ADebugEvent.CreateProcessInfo.hProcess;
  FThreadHandle := ADebugEvent.CreateProcessInfo.hThread;

  if ADebugEvent.CreateProcessInfo.hFile <> 0 then
    CloseHandle(ADebugEvent.CreateProcessInfo.hFile);

  FActiveThreads.Add(ADebugEvent.CreateProcessInfo.hThread);

  // Map queued breakpoints to addresses now that we have the base address
  for I := 0 to FBreakpoints.Count - 1 do
  begin
    if FBreakpoints[I].Address = nil then
      FBreakpoints[I].Address := GetAddressFromUnitLine(FBreakpoints[I].UnitName, FBreakpoints[I].LineNumber);
  end;

  ApplyBreakpointsToThread(ADebugEvent.CreateProcessInfo.hThread);
end;

procedure TDebugger.HandleCreateThread(const ADebugEvent: TDebugEvent);
begin
  FActiveThreads.Add(ADebugEvent.CreateThread.hThread);
  ApplyBreakpointsToThread(ADebugEvent.CreateThread.hThread);
end;

procedure TDebugger.HandleExitThread(const ADebugEvent: TDebugEvent);
begin
  // Handle removal
end;

procedure TDebugger.HandleException(const ADebugEvent: TDebugEvent; var AContinueStatus: DWORD);
var
  I: Integer;
  BP: TBreakpoint;
  Context: TContext;
  CurrentThread: THandle;
begin
  AContinueStatus := DBG_EXCEPTION_NOT_HANDLED;

  if ADebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_SINGLE_STEP then
  begin
    CurrentThread := OpenThread(THREAD_GET_CONTEXT or THREAD_SET_CONTEXT, False, ADebugEvent.dwThreadId);
    if CurrentThread = 0 then Exit;
    try
      Context.ContextFlags := CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS;
      if GetThreadContext(CurrentThread, Context) then
      begin
        BP := nil;
        for I := 0 to FBreakpoints.Count - 1 do
        begin
          if (FBreakpoints[I].Slot >= 0) and ((Context.Dr6 and (1 shl FBreakpoints[I].Slot)) <> 0) then
          begin
            BP := FBreakpoints[I];
            Break;
          end;
        end;

        if BP <> nil then
        begin
          Writeln(Format('*** Hardware Breakpoint hit at %p (%s:%d) ***', [BP.Address, BP.UnitName, BP.LineNumber]));
          
          FLastBreakpointHit := BP;
          FLastThreadHit := CurrentThread;
          FBreakpointHitEvent.SetEvent;

          if Assigned(FOnBreakpoint) then
            FOnBreakpoint(Self, BP);

          // Wait for resume signal from MCP server
          FContinueEvent.ResetEvent;
          FContinueEvent.WaitFor(INFINITE);

          Context.EFlags := Context.EFlags or $10000; // RF
          Context.Dr6 := Context.Dr6 and not $F;
          
          SetThreadContext(CurrentThread, Context);
          AContinueStatus := DBG_CONTINUE;
        end
        else
          AContinueStatus := DBG_CONTINUE;
      end;
    finally
      CloseHandle(CurrentThread);
    end;
  end;
end;

procedure TDebugger.StartDebugging(const AExecutablePath: string);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  DebugEvent: TDebugEvent;
  ContinueStatus: DWORD;
  LRunning: Boolean;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);

  if not CreateProcess(nil, PChar(AExecutablePath), nil, nil, False, 
    DEBUG_ONLY_THIS_PROCESS or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
    raise Exception.Create('Failed to create process: ' + IntToStr(GetLastError));

  FProcessId := ProcessInfo.dwProcessId;
  FThreadId := ProcessInfo.dwThreadId;
  FProcessHandle := ProcessInfo.hProcess;
  FThreadHandle := ProcessInfo.hThread;

  LRunning := True;
  while LRunning do
  begin
    if not WaitForDebugEvent(DebugEvent, INFINITE) then Break;

    ContinueStatus := DBG_CONTINUE;

    case DebugEvent.dwDebugEventCode of
      CREATE_PROCESS_DEBUG_EVENT: HandleCreateProcess(DebugEvent);
      CREATE_THREAD_DEBUG_EVENT: HandleCreateThread(DebugEvent);
      EXIT_THREAD_DEBUG_EVENT: HandleExitThread(DebugEvent);
      EXCEPTION_DEBUG_EVENT: HandleException(DebugEvent, ContinueStatus);
      LOAD_DLL_DEBUG_EVENT: if DebugEvent.LoadDll.hFile <> 0 then CloseHandle(DebugEvent.LoadDll.hFile);
      EXIT_PROCESS_DEBUG_EVENT: LRunning := False;
    end;

    if not ContinueDebugEvent(DebugEvent.dwProcessId, DebugEvent.dwThreadId, ContinueStatus) then Break;
  end;
  
  FBreakpointHitEvent.SetEvent; // Signal exit
end;

end.