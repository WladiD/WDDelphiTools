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
  TJclMapScannerCracker = class(TJclMapScanner);

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

  TStackSlot = record
    Offset: Integer; // Offset from EBP
    Address: Pointer;
    Value: UIntPtr;
    Interpretation: string;
  end;

  TStackFrameInfo = record
    ProcedureName: string;
    StartAddress: Pointer;
    LocalSize: Integer;
  end;

  TStepType = (stNone, stInto, stOver);

  TOnBreakpointEvent = procedure(Sender: TObject; Breakpoint: TBreakpoint) of object;
  TOnExceptionEvent = procedure(Sender: TObject; const ExceptionRecord: TExceptionRecord; const FirstChance: Boolean; var Handled: Boolean) of object;

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
    FOnException: TOnExceptionEvent;
    
    FContinueEvent: TEvent;
    FBreakpointHitEvent: TEvent;
    FReadyEvent: TEvent;
    FLastBreakpointHit: TBreakpoint;
    FLastThreadHit: THandle;
    FLastException: TExceptionRecord;
    FLastExceptionFirstChance: Boolean;
    FFirstBreak: Boolean;

    FStepType: TStepType;
    FStepStartDepth: Integer;
    FStepStartUnit: string;
    FStepStartLine: Integer;
    
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
    function GetAddressFromSymbol(const ASymbolName: string): Pointer;
    
    procedure SetBreakpoint(const AUnitName: string; ALineNumber: Integer);
    procedure RemoveBreakpoint(const AUnitName: string; ALineNumber: Integer);
    procedure ClearAllBreakpoints;
    procedure StartDebugging(const AExecutablePath: string);
    procedure Terminate;
    
    procedure ResumeExecution;
    procedure StepInto;
    procedure StepOver;
    procedure WaitForReady(Timeout: DWORD = INFINITE);
    function WaitForBreakpoint(Timeout: DWORD = INFINITE): TBreakpoint;
    
    function GetStackTrace(AThreadHandle: THandle): TArray<TStackFrame>;
    function GetRegisters(AThreadHandle: THandle): TRegisters;
    function GetStackSlots(AThreadHandle: THandle; AMaxSlots: Integer = 20): TArray<TStackSlot>;
    function GetStackFrameInfo(AThreadHandle: THandle): TStackFrameInfo;
    function ReadProcessMemory(AAddress: Pointer; ASize: NativeUInt): TBytes;
    
    property OnBreakpoint: TOnBreakpointEvent read FOnBreakpoint write FOnBreakpoint;
    property OnException: TOnExceptionEvent read FOnException write FOnException;
    property LastThreadHit: THandle read FLastThreadHit;
    property LastException: TExceptionRecord read FLastException;
    property LastExceptionFirstChance: Boolean read FLastExceptionFirstChance;
    property Breakpoints: TObjectList<TBreakpoint> read FBreakpoints;
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
    begin
      // Cannot use Writeln here, it would corrupt MCP stream
    end;
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
  FReadyEvent := TEvent.Create(nil, False, False, '');
  FStepType := stNone;
  FFirstBreak := True;
end;

destructor TDebugger.Destroy;
begin
  FReadyEvent.Free;
  FBreakpointHitEvent.Free;
  FContinueEvent.Free;
  FActiveThreads.Free;
  FBreakpoints.Free;
  FMapScanner.Free;
  if FProcessHandle <> 0 then CloseHandle(FProcessHandle);
  if FThreadHandle <> 0 then CloseHandle(FThreadHandle);
  inherited Destroy;
end;

procedure TDebugger.Terminate;
begin
  if FProcessHandle <> 0 then
    Winapi.Windows.TerminateProcess(FProcessHandle, 1);
end;

procedure TDebugger.ResumeExecution;
begin
  FStepType := stNone;
  FBreakpointHitEvent.ResetEvent;
  FContinueEvent.SetEvent;
end;

procedure TDebugger.StepInto;
var
  Stack: TArray<TStackFrame>;
begin
  FStepType := stInto;
  Stack := GetStackTrace(FLastThreadHit);
  if Length(Stack) > 0 then
  begin
    FStepStartUnit := Stack[0].UnitName;
    FStepStartLine := Stack[0].LineNumber;
    FStepStartDepth := Length(Stack);
  end;
  FBreakpointHitEvent.ResetEvent;
  FContinueEvent.SetEvent;
end;

procedure TDebugger.StepOver;
var
  Stack: TArray<TStackFrame>;
begin
  FStepType := stOver;
  Stack := GetStackTrace(FLastThreadHit);
  if Length(Stack) > 0 then
  begin
    FStepStartUnit := Stack[0].UnitName;
    FStepStartLine := Stack[0].LineNumber;
    FStepStartDepth := Length(Stack);
  end;
  FBreakpointHitEvent.ResetEvent;
  FContinueEvent.SetEvent;
end;

procedure TDebugger.WaitForReady(Timeout: DWORD);
begin
  FReadyEvent.WaitFor(Timeout);
end;

function TDebugger.WaitForBreakpoint(Timeout: DWORD): TBreakpoint;
begin
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

function TDebugger.GetStackSlots(AThreadHandle: THandle; AMaxSlots: Integer): TArray<TStackSlot>;
var
  Regs: TRegisters;
  I: Integer;
  Val: UIntPtr;
  ResultList: TList<TStackSlot>;
  Slot: TStackSlot;
  RelativeVA: DWORD;
  SymbolName: string;
begin
  Regs := GetRegisters(AThreadHandle);
  ResultList := TList<TStackSlot>.Create;
  try
    for I := 0 to AMaxSlots - 1 do
    begin
      Slot.Offset := -(I * SizeOf(Pointer));
      Slot.Address := PByte(Regs.Ebp) + Slot.Offset;
      
      if NativeInt(Slot.Address) < NativeInt(Regs.Esp) then Break;

      Val := UIntPtr(ReadProcessMemoryPtr(Slot.Address));
      Slot.Value := Val;
      Slot.Interpretation := '';

      if Val <> 0 then
      begin
        if (Val > FBaseAddress) and (Val < FBaseAddress + $1000000) then
        begin
          RelativeVA := Val - FBaseAddress - $1000;
          SymbolName := FMapScanner.ProcNameFromAddr(RelativeVA);
          if SymbolName <> '' then
            Slot.Interpretation := 'Points to ' + SymbolName;
        end;

        if (Slot.Interpretation = '') and (Val < $10000) then
          Slot.Interpretation := IntToStr(Val);
          
        if Slot.Interpretation = '' then
          Slot.Interpretation := '$' + IntToHex(Val, SizeOf(Pointer) * 2);
      end
      else
        Slot.Interpretation := '0';

      ResultList.Add(Slot);
    end;
    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TDebugger.GetStackFrameInfo(AThreadHandle: THandle): TStackFrameInfo;
var
  Regs: TRegisters;
  RelativeVA: DWORD;
  IdxOffset: Integer;
  Buf: TBytes;
  P: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  if not Assigned(FMapScanner) then Exit;

  Regs := GetRegisters(AThreadHandle);
  RelativeVA := Regs.Eip - FBaseAddress - $1000;

  Result.ProcedureName := FMapScanner.ProcNameFromAddr(RelativeVA, IdxOffset);
  if Result.ProcedureName <> '' then
  begin
    Result.StartAddress := Pointer(Regs.Eip - UIntPtr(IdxOffset));
    
    // Analyze prologue
    Buf := ReadProcessMemory(Result.StartAddress, 64);
    if Length(Buf) >= 3 then
    begin
      P := 0;
      // Skip push ebp (55) and mov ebp, esp (8B EC)
      if (Buf[P] = $55) then Inc(P);
      if (P < Length(Buf) - 1) and (Buf[P] = $8B) and (Buf[P+1] = $EC) then Inc(P, 2);
      
      if P < Length(Buf) then
      begin
        // push ecx (51) - common optimization for 4-byte local
        if (Buf[P] = $51) then
          Result.LocalSize := 4
        // sub esp, imm8 (83 EC XX)
        else if (Buf[P] = $83) and (Buf[P+1] = $EC) then
          Result.LocalSize := Buf[P+2]
        // sub esp, imm32 (81 EC XX XX XX XX)
        else if (Buf[P] = $81) and (Buf[P+1] = $EC) then
          Result.LocalSize := PLongWord(@Buf[P+2])^
        // add esp, -imm8 (83 C4 XX)
        else if (Buf[P] = $83) and (Buf[P+1] = $C4) and (ShortInt(Buf[P+2]) < 0) then
          Result.LocalSize := -ShortInt(Buf[P+2])
        // add esp, -imm32 (81 C4 XX XX XX XX)
        else if (Buf[P] = $81) and (Buf[P+1] = $C4) and (LongInt(PLongInt(@Buf[P+2])^) < 0) then
          Result.LocalSize := -LongInt(PLongInt(@Buf[P+2])^);
      end;
    end;
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
      Frame.UnitName := '';
      Frame.ProcedureName := '';
      Frame.LineNumber := 0;

      if Assigned(FMapScanner) and (UIntPtr(ReturnAddr) >= FBaseAddress + $1000) then
      begin
        RelativeVA := UIntPtr(ReturnAddr) - FBaseAddress - $1000;
        Frame.UnitName := FMapScanner.ModuleNameFromAddr(RelativeVA);
        Frame.ProcedureName := FMapScanner.ProcNameFromAddr(RelativeVA);
        Frame.LineNumber := FMapScanner.LineNumberFromAddr(RelativeVA);
      end;

      Frames.Add(Frame);

      if FramePtr = nil then Break;
      ReturnAddr := ReadProcessMemoryPtr(PByte(FramePtr) + SizeOf(Pointer));
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

  SearchUnit := ChangeFileExt(AUnitName, '');

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

procedure TDebugger.SetHardwareBreakpointInContext(var AContext: TContext; AAddress: Pointer; ASlot: Integer);
begin
  AContext.ContextFlags := AContext.ContextFlags or CONTEXT_DEBUG_REGISTERS;
  case ASlot of
    0: AContext.Dr0 := UIntPtr(AAddress);
    1: AContext.Dr1 := UIntPtr(AAddress);
    2: AContext.Dr2 := UIntPtr(AAddress);
    3: AContext.Dr3 := UIntPtr(AAddress);
  end;
  AContext.Dr7 := AContext.Dr7 or (1 shl (ASlot * 2));
  AContext.Dr7 := AContext.Dr7 and not ($F shl (16 + ASlot * 4));
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

  // Clear all hardware breakpoint enables in DR7 (Bits 0-7)
  Context.Dr7 := Context.Dr7 and not $FF;

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
begin
  if FBreakpoints.Count >= 4 then Exit;

  Addr := nil;
  if FBaseAddress <> 0 then
    Addr := GetAddressFromUnitLine(AUnitName, ALineNumber);

  BP := TBreakpoint.Create(AUnitName, ALineNumber, Addr);
  FBreakpoints.Add(BP);
end;

procedure TDebugger.RemoveBreakpoint(const AUnitName: string; ALineNumber: Integer);
var
  I: Integer;
begin
  for I := FBreakpoints.Count - 1 downto 0 do
  begin
    if SameText(FBreakpoints[I].UnitName, AUnitName) and (FBreakpoints[I].LineNumber = ALineNumber) then
    begin
      FBreakpoints.Delete(I);
    end;
  end;
end;

procedure TDebugger.ClearAllBreakpoints;
begin
  FBreakpoints.Clear;
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

  for I := 0 to FBreakpoints.Count - 1 do
  begin
    if FBreakpoints[I].Address = nil then
      FBreakpoints[I].Address := GetAddressFromUnitLine(FBreakpoints[I].UnitName, FBreakpoints[I].LineNumber);
  end;
end;

procedure TDebugger.HandleCreateThread(const ADebugEvent: TDebugEvent);
begin
  FActiveThreads.Add(ADebugEvent.CreateThread.hThread);
end;

procedure TDebugger.HandleExitThread(const ADebugEvent: TDebugEvent);
begin
end;

procedure TDebugger.HandleException(const ADebugEvent: TDebugEvent; var AContinueStatus: DWORD);
var
  I: Integer;
  BP: TBreakpoint;
  Context: TContext;
  CurrentThread: THandle;
  Handled: Boolean;
begin
  AContinueStatus := DBG_EXCEPTION_NOT_HANDLED;

  if ADebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_BREAKPOINT then
  begin
    if FFirstBreak then
    begin
      FFirstBreak := False;
      FReadyEvent.SetEvent;
      FContinueEvent.WaitFor(INFINITE);
      FContinueEvent.ResetEvent;
    end;
    AContinueStatus := DBG_CONTINUE;
  end
  else if ADebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_SINGLE_STEP then
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
          FLastBreakpointHit := BP;
          FLastThreadHit := CurrentThread;
          FBreakpointHitEvent.SetEvent;
          if Assigned(FOnBreakpoint) then FOnBreakpoint(Self, BP);
          
          FContinueEvent.ResetEvent;
          FContinueEvent.WaitFor(INFINITE);

          Context.EFlags := Context.EFlags or $10000; // RF
          Context.Dr6 := Context.Dr6 and not $F;
          if FStepType <> stNone then Context.EFlags := Context.EFlags or $100; // TF
          
          SetThreadContext(CurrentThread, Context);
          AContinueStatus := DBG_CONTINUE;
        end
        else if FStepType <> stNone then
        begin
          var Stack := GetStackTrace(CurrentThread);
          var StopStepping := False;

          if Length(Stack) > 0 then
          begin
            var CurUnit := Stack[0].UnitName;
            var CurLine := Stack[0].LineNumber;
            var CurDepth := Length(Stack);

            if (CurUnit <> '') and (CurLine > 0) then
            begin
              if FStepType = stInto then
              begin
                if (CurUnit <> FStepStartUnit) or (CurLine <> FStepStartLine) then StopStepping := True;
              end
              else if FStepType = stOver then
              begin
                if ((CurUnit <> FStepStartUnit) or (CurLine <> FStepStartLine)) and (CurDepth <= FStepStartDepth) then StopStepping := True;
              end;
            end
            else if (CurUnit = '') and (FStepType = stInto) then
            begin
              StopStepping := True;
            end;
          end;

          if StopStepping then
          begin
            FStepType := stNone;
            Context.EFlags := Context.EFlags and not $100; // Clear TF
            
            if Length(Stack) > 0 then
              FLastBreakpointHit := TBreakpoint.Create(Stack[0].UnitName, Stack[0].LineNumber, Stack[0].Address)
            else
              FLastBreakpointHit := TBreakpoint.Create('Unknown', 0, nil);

            FLastThreadHit := CurrentThread;
            FBreakpointHitEvent.SetEvent;
            FContinueEvent.ResetEvent;
            FContinueEvent.WaitFor(INFINITE);

            if FStepType <> stNone then Context.EFlags := Context.EFlags or $100;
            SetThreadContext(CurrentThread, Context);
            AContinueStatus := DBG_CONTINUE;
          end
          else
          begin
            Context.EFlags := Context.EFlags or $100; // Keep TF
            SetThreadContext(CurrentThread, Context);
            AContinueStatus := DBG_CONTINUE;
          end;
        end
        else
          AContinueStatus := DBG_CONTINUE;
      end;
    finally
      CloseHandle(CurrentThread);
    end;
  end
  else if (ADebugEvent.Exception.ExceptionRecord.ExceptionCode <> EXCEPTION_BREAKPOINT) and
          (ADebugEvent.Exception.ExceptionRecord.ExceptionCode <> $406D1388) then
  begin
    FLastException := ADebugEvent.Exception.ExceptionRecord;
    FLastExceptionFirstChance := ADebugEvent.Exception.dwFirstChance <> 0;
    CurrentThread := OpenThread(THREAD_GET_CONTEXT or THREAD_SET_CONTEXT, False, ADebugEvent.dwThreadId);
    if CurrentThread <> 0 then
    begin
      FLastThreadHit := CurrentThread;
      Handled := False;
      if Assigned(FOnException) then FOnException(Self, FLastException, FLastExceptionFirstChance, Handled);
      if Handled then
      begin
        FLastBreakpointHit := nil;
        FBreakpointHitEvent.SetEvent;
        FContinueEvent.ResetEvent;
        FContinueEvent.WaitFor(INFINITE);
        AContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
      end
      else
        AContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
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
  SA: TSecurityAttributes;
  hDevNull: THandle;
  I: Integer;
begin
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := nil;
  SA.bInheritHandle := True;

  // Open the NUL device to discard output
  hDevNull := CreateFile('NUL', GENERIC_WRITE, FILE_SHARE_WRITE, @SA, OPEN_EXISTING, 0, 0);

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  
  if hDevNull <> INVALID_HANDLE_VALUE then
  begin
    StartupInfo.dwFlags := STARTF_USESTDHANDLES;
    StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    StartupInfo.hStdOutput := hDevNull;
    StartupInfo.hStdError := hDevNull;
  end;

  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);

  if not CreateProcess(nil, PChar(AExecutablePath), nil, nil, True, // True to inherit handles
    DEBUG_ONLY_THIS_PROCESS or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
  begin
    if hDevNull <> INVALID_HANDLE_VALUE then CloseHandle(hDevNull);
    raise Exception.Create('Failed to create process: ' + IntToStr(GetLastError));
  end;

  FProcessId := ProcessInfo.dwProcessId;
  FThreadId := ProcessInfo.dwThreadId;
  FProcessHandle := ProcessInfo.hProcess;
  FThreadHandle := ProcessInfo.hThread;

  if hDevNull <> INVALID_HANDLE_VALUE then CloseHandle(hDevNull); // Process has its own copy now

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
    
    if LRunning then
    begin
      for I := 0 to FActiveThreads.Count - 1 do
        ApplyBreakpointsToThread(FActiveThreads[I]);
    end;

    if not ContinueDebugEvent(DebugEvent.dwProcessId, DebugEvent.dwThreadId, ContinueStatus) then Break;
  end;
  FBreakpointHitEvent.SetEvent;
end;

end.