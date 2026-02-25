unit DPT.Debugger;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
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
    
    procedure HandleException(const ADebugEvent: TDebugEvent; var AContinueStatus: DWORD);
    procedure HandleCreateProcess(const ADebugEvent: TDebugEvent);
    procedure HandleCreateThread(const ADebugEvent: TDebugEvent);
    procedure HandleExitThread(const ADebugEvent: TDebugEvent);
    procedure ApplyBreakpointsToThread(AThreadHandle: THandle);
    procedure SetHardwareBreakpointInContext(var AContext: TContext; AAddress: Pointer; ASlot: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadMapFile(const AMapFileName: string);
    function GetAddressFromUnitLine(const AUnitName: string; ALineNumber: Integer): Pointer;
    
    procedure SetBreakpoint(const AUnitName: string; ALineNumber: Integer);
    procedure StartDebugging(const AExecutablePath: string);
    
    property OnBreakpoint: TOnBreakpointEvent read FOnBreakpoint write FOnBreakpoint;
  end;

implementation

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
end;

destructor TDebugger.Destroy;
begin
  FActiveThreads.Free;
  FBreakpoints.Free;
  FMapScanner.Free;
  if FProcessHandle <> 0 then CloseHandle(FProcessHandle);
  if FThreadHandle <> 0 then CloseHandle(FThreadHandle);
  inherited Destroy;
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
  Scanner: TJclMapScanner;
  TestVA: DWORD;
begin
  Result := nil;
  if not Assigned(FMapScanner) then Exit;
  Scanner := FMapScanner;

  SearchUnit := ChangeFileExt(AUnitName, ''); // Strip .pas or .dpr

  // Test VA mapping for a known function to see JCL's base assumption
  TestVA := Scanner.VAFromUnitAndProcName('FileVersion', 'ShowUsage');
  if TestVA <> 0 then
    Writeln(Format('  JCL VA for FileVersion.ShowUsage: %08x', [TestVA]));

  for I := 0 to Scanner.LineNumbersCnt - 1 do
  begin
    LineInfo := Scanner.LineNumberByIndex[I];
    if LineInfo.LineNumber = ALineNumber then
    begin
      UnitMatch := TJclMapScanner.MapStringToStr(LineInfo.UnitName);
      if SameText(UnitMatch, SearchUnit) or SameText(ExtractFileName(UnitMatch), SearchUnit) then
      begin
        Writeln(Format('  Found %s:%d -> Seg=%d, JCL_VA=%08x', [UnitMatch, ALineNumber, LineInfo.Segment, LineInfo.VA]));
        
        // Let's try adding $1000 instead of $10000.
        // If the MAP file says 0001:000D7768 and preferred base is 00400000, 
        // and .text starts at 00401000, then relative offset is $1000.
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

  Writeln(Format('  SetHardwareBreakpointInContext: Slot %d, Addr %p, DR7 %08x', [ASlot, AAddress, AContext.Dr7]));
end;

procedure TDebugger.ApplyBreakpointsToThread(AThreadHandle: THandle);
var
  Context: TContext;
  I: Integer;
  BPCount: Integer;
begin
  Writeln(Format('  ApplyBreakpointsToThread: Handle=%d', [AThreadHandle]));
  Context.ContextFlags := CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS;
  if not GetThreadContext(AThreadHandle, Context) then
  begin
    Writeln('  Error: GetThreadContext failed. Error: ' + IntToStr(GetLastError));
    Exit;
  end;

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

  if not SetThreadContext(AThreadHandle, Context) then
    Writeln('  Error: SetThreadContext failed. Error: ' + IntToStr(GetLastError))
  else
    Writeln('  SetThreadContext success.');
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
    Writeln(Format('Hardware breakpoint defined at %p (%s:%d)', [Addr, AUnitName, ALineNumber]));
    for Thread in FActiveThreads do
      ApplyBreakpointsToThread(Thread);
  end
  else
    Writeln(Format('Breakpoint queued for %s:%d (waiting for process start)', [AUnitName, ALineNumber]));
end;

procedure TDebugger.HandleCreateProcess(const ADebugEvent: TDebugEvent);
var
  I: Integer;
begin
  FBaseAddress := UIntPtr(ADebugEvent.CreateProcessInfo.lpBaseOfImage);
  FProcessHandle := ADebugEvent.CreateProcessInfo.hProcess;
  Writeln(Format('Target Base Address: %p', [Pointer(FBaseAddress)]));

  if ADebugEvent.CreateProcessInfo.hFile <> 0 then
    CloseHandle(ADebugEvent.CreateProcessInfo.hFile);

  FActiveThreads.Add(ADebugEvent.CreateProcessInfo.hThread);

  // Map queued breakpoints to addresses now that we have the base address
  for I := 0 to FBreakpoints.Count - 1 do
  begin
    if FBreakpoints[I].Address = nil then
    begin
      FBreakpoints[I].Address := GetAddressFromUnitLine(FBreakpoints[I].UnitName, FBreakpoints[I].LineNumber);
      if FBreakpoints[I].Address <> nil then
        Writeln(Format('Mapped queued breakpoint to %p (%s:%d)', 
          [FBreakpoints[I].Address, FBreakpoints[I].UnitName, FBreakpoints[I].LineNumber]))
      else
        Writeln(Format('Warning: Could not map %s:%d to an address.', 
          [FBreakpoints[I].UnitName, FBreakpoints[I].LineNumber]));
    end;
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
  // In a real debugger we would need to find the handle by ThreadId.
end;

procedure TDebugger.HandleException(const ADebugEvent: TDebugEvent; var AContinueStatus: DWORD);
var
  ExceptionAddr: Pointer;
  I: Integer;
  BP: TBreakpoint;
  Context: TContext;
  CurrentThread: THandle;
begin
  ExceptionAddr := ADebugEvent.Exception.ExceptionRecord.ExceptionAddress;
  AContinueStatus := DBG_EXCEPTION_NOT_HANDLED;

  // Log all exceptions for debugging
  Writeln(Format('  Exception: Code=%08x, Addr=%p, TID=%d', 
    [ADebugEvent.Exception.ExceptionRecord.ExceptionCode, ExceptionAddr, ADebugEvent.dwThreadId]));

  // Single step or Hardware Breakpoint trigger EXCEPTION_SINGLE_STEP
  if ADebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_SINGLE_STEP then
  begin
    CurrentThread := OpenThread(THREAD_GET_CONTEXT or THREAD_SET_CONTEXT, False, ADebugEvent.dwThreadId);
    if CurrentThread = 0 then Exit;
    try
      Context.ContextFlags := CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS;
      GetThreadContext(CurrentThread, Context);

      BP := nil;
      // Check DR6 to see which breakpoint hit (bits 0-3)
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
        
        if Assigned(FOnBreakpoint) then
          FOnBreakpoint(Self, BP);

        // To continue over a hardware breakpoint, we set the Resume Flag (RF) in EFlags.
        // Bit 16 is RF.
        Context.EFlags := Context.EFlags or $10000; 
        
        // Clear DR6 status bits
        Context.Dr6 := Context.Dr6 and not $F;
        
        SetThreadContext(CurrentThread, Context);
        AContinueStatus := DBG_CONTINUE;
      end
      else
      begin
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
end;

end.