// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Debugger;

interface

uses

  Winapi.Windows,

  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  System.SysUtils,

  JclDebug,
  mormot.core.collections,

  DPT.Logger;

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall; external kernel32;

const

  THREAD_GET_CONTEXT = $0008;
  THREAD_SET_CONTEXT = $0010;

type

  TJclMapScannerCracker = class(TJclMapScanner);

  TBreakpoint = class
  public
    Address   : Pointer;
    IsActive  : Boolean;
    LineNumber: Integer;
    Slot      : Integer; // 0..3 for DR0..DR3
    UnitName  : String;
    constructor Create(const AUnitName: string; ALineNumber: Integer; AAddress: Pointer);
  end;

  TStackFrame = record
    Address      : Pointer;
    LineNumber   : Integer;
    ProcedureName: String;
    UnitName     : String;
  end;

  TRegisters = record
    Eip   : UIntPtr;
    Esp   : UIntPtr;
    Ebp   : UIntPtr;
    Eax   : UIntPtr;
    Ebx   : UIntPtr;
    Ecx   : UIntPtr;
    Edx   : UIntPtr;
    Esi   : UIntPtr;
    Edi   : UIntPtr;
    EFlags: DWORD;
  end;

  TStackSlot = record
    Address       : Pointer;
    Interpretation: String;
    Offset        : Integer; // Offset from EBP
    Value         : UIntPtr;
  end;

  TStackFrameInfo = record
    LocalSize    : Integer;
    ProcedureName: String;
    StartAddress : Pointer;
  end;

  TStepType = (stNone, stInto, stOver);

  TOnBreakpointEvent = procedure(Sender: TObject; Breakpoint: TBreakpoint) of object;
  TOnSteppedEvent = procedure(Sender: TObject; Breakpoint: TBreakpoint) of object;
  TOnExceptionEvent = procedure(Sender: TObject; const ExceptionRecord: TExceptionRecord; const FirstChance: Boolean; var Handled: Boolean) of object;
  TOnProcessExitEvent = procedure(Sender: TObject; ExitCode: DWORD) of object;

  TDebugger = class
  private
    FActiveThreads           : TList<THandle>;
    FBaseAddress             : UIntPtr;
    FBreakpointHitEvent      : TEvent;
    FBreakpointLock          : TCriticalSection;
    FBreakpoints             : TObjectList<TBreakpoint>;
    FContinueEvent           : TEvent;
    FFinishedEvent           : TEvent;
    FFirstBreak              : Boolean;
    FLastBreakpointHit       : TBreakpoint;
    FLastException           : TExceptionRecord;
    FLastExceptionFirstChance: Boolean;
    FLastThreadHit           : THandle;
    FMapScanner              : TJclMapScanner;
    FOnBreakpoint            : TOnBreakpointEvent;
    FOnException             : TOnExceptionEvent;
    FOnProcessExit           : TOnProcessExitEvent;
    FOnStepped               : TOnSteppedEvent;
    FProcessHandle           : THandle;
    FProcessId               : DWORD;
    FReadyEvent              : TEvent;
    FStepReturnBP            : TBreakpoint;
    FStepStartDepth          : Integer;
    FStepStartLine           : Integer;
    FStepStartUnit           : String;
    FStepType                : TStepType;
    FTerminated              : Boolean;
    FThreadHandle            : THandle;
    FThreadId                : DWORD;
    procedure ApplyBreakpointsToThread(AThreadHandle: THandle);
    procedure HandleCreateProcess(const ADebugEvent: TDebugEvent);
    procedure HandleCreateThread(const ADebugEvent: TDebugEvent);
    procedure HandleException(const ADebugEvent: TDebugEvent; var AContinueStatus: DWORD);
    procedure HandleExitThread(const ADebugEvent: TDebugEvent);
    function  ReadProcessMemoryPtr(AAddress: Pointer): Pointer;
    procedure SetHardwareBreakpointInContext(var AContext: TContext; AAddress: Pointer; ASlot: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAllBreakpoints;
    procedure Detach;
    function  GetAddressFromSymbol(const ASymbolName: string): Pointer;
    function  GetAddressFromUnitLine(const AUnitName: string; ALineNumber: Integer): Pointer;
    function  GetRegisters(AThreadHandle: THandle): TRegisters;
    function  GetStackFrameInfo(AThreadHandle: THandle): TStackFrameInfo;
    function  GetStackSlots(AThreadHandle: THandle; AMaxSlots: Integer = 20): TArray<TStackSlot>;
    function  GetStackTrace(AThreadHandle: THandle): TArray<TStackFrame>;
    procedure LoadMapFile(const AMapFileName: string);
    function  ReadProcessMemory(AAddress: Pointer; ASize: NativeUInt): TBytes;
    procedure RemoveBreakpoint(const AUnitName: string; ALineNumber: Integer);
    procedure ResumeExecution;
    function  SetBreakpoint(const AUnitName: string; ALineNumber: Integer): Boolean; overload;
    function  SetBreakpoint(const AUnitName: string; ALineNumber: Integer; ARequireAddress: Boolean): Boolean; overload;
    procedure StartDebugging(const AExecutablePath: string);
    procedure StepInto;
    procedure StepOver;
    procedure Terminate;
    function  WaitForBreakpoint(Timeout: DWORD = INFINITE): TBreakpoint;
    procedure WaitForReady(Timeout: DWORD = INFINITE);
    property  BreakpointLock: TCriticalSection read FBreakpointLock;
    property  Breakpoints: TObjectList<TBreakpoint> read FBreakpoints;
    property  LastException: TExceptionRecord read FLastException;
    property  LastExceptionFirstChance: Boolean read FLastExceptionFirstChance;
    property  LastThreadHit: THandle read FLastThreadHit;
    property  OnBreakpoint: TOnBreakpointEvent read FOnBreakpoint write FOnBreakpoint;
    property  OnException: TOnExceptionEvent read FOnException write FOnException;
    property  OnProcessExit: TOnProcessExitEvent read FOnProcessExit write FOnProcessExit;
    property  OnStepped: TOnSteppedEvent read FOnStepped write FOnStepped;
  end;

  TDebuggerThread = class(TThread)
  private
    FDebugger      : TDebugger;
    FExecutablePath: String;
  protected
    procedure Execute; override;
  public
    constructor Create(ADebugger: TDebugger; const AExecutablePath: String);
  end;

implementation

{ TDebuggerThread }

constructor TDebuggerThread.Create(ADebugger: TDebugger; const AExecutablePath: String);
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

constructor TBreakpoint.Create(const AUnitName: String; ALineNumber: Integer; AAddress: Pointer);
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
  FBreakpointLock := TCriticalSection.Create;
  FContinueEvent := TEvent.Create(nil, False, False, '');
  FBreakpointHitEvent := TEvent.Create(nil, False, False, '');
  FReadyEvent := TEvent.Create(nil, False, False, '');
  FFinishedEvent := TEvent.Create(nil, True, False, '');
  FStepType := stNone;
  FFirstBreak := True;
  FTerminated := False;
end;

destructor TDebugger.Destroy;
begin
  Terminate;
  FFinishedEvent.Free;
  FReadyEvent.Free;
  FBreakpointHitEvent.Free;
  FContinueEvent.Free;
  FActiveThreads.Free;
  FBreakpointLock.Free;
  FBreakpoints.Free;
  FMapScanner.Free;
  if FProcessHandle <> 0 then
    CloseHandle(FProcessHandle);
  if FThreadHandle <> 0 then
    CloseHandle(FThreadHandle);
  inherited Destroy;
end;

procedure TDebugger.Terminate;
begin
  if FTerminated then
    Exit;
  FTerminated := True;

  if FProcessHandle <> 0 then
    Winapi.Windows.TerminateProcess(FProcessHandle, 1);

  FContinueEvent.SetEvent;
  FFinishedEvent.WaitFor(5000);
end;

procedure TDebugger.Detach;
var
  Context: TContext;
begin
  if FTerminated then Exit;
  FTerminated := True;

  for var I: Integer := 0 to FActiveThreads.Count - 1 do
  begin
    Context.ContextFlags := CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS;
    if GetThreadContext(FActiveThreads[I], Context) then
    begin
      Context.Dr0 := 0;
      Context.Dr1 := 0;
      Context.Dr2 := 0;
      Context.Dr3 := 0;
      Context.Dr7 := Context.Dr7 and not $FF;
      SetThreadContext(FActiveThreads[I], Context);
    end;
  end;

  DebugActiveProcessStop(FProcessId);
  FContinueEvent.SetEvent;
  FFinishedEvent.WaitFor(5000);
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
  Regs      : TRegisters;
  RelativeVA: DWORD;
  ResultList: IList<TStackSlot>; 
  Slot      : TStackSlot;
  SymbolName: String;
  Val       : UIntPtr;
begin
  Regs := GetRegisters(AThreadHandle);
  ResultList := Collections.NewPlainList<TStackSlot>;
  for var I: Integer := 0 to AMaxSlots - 1 do
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
  Result := ResultList.AsArray;
end;

function TDebugger.GetStackFrameInfo(AThreadHandle: THandle): TStackFrameInfo;
var
  Buf       : TBytes;
  IdxOffset : Integer;
  P         : Integer;
  Regs      : TRegisters;
  RelativeVA: DWORD;
begin
  FillChar(Result, SizeOf(Result), 0);
  if not Assigned(FMapScanner) then
    Exit;

  Regs := GetRegisters(AThreadHandle);
  RelativeVA := Regs.Eip - FBaseAddress - $1000;

  Result.ProcedureName := FMapScanner.ProcNameFromAddr(RelativeVA, IdxOffset);
  if Result.ProcedureName = '' then
    Exit;

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

function TDebugger.GetStackTrace(AThreadHandle: THandle): TArray<TStackFrame>;
var
  Context   : TContext;
  Frame     : TStackFrame;
  FramePtr  : Pointer;
  Frames    : TList<TStackFrame>;
  RelativeVA: DWORD;
  ReturnAddr: Pointer;
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
  LineInfo  : TJclMapLineNumber;
  SearchUnit: String;
  UnitMatch : String;
begin
  Result := nil;
  if not Assigned(FMapScanner) then Exit;

  SearchUnit := ChangeFileExt(AUnitName, '');

  for var I: Integer := 0 to FMapScanner.LineNumbersCnt - 1 do
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
  DotPos    : Integer;
  SymbolName: String;
  UnitName  : String;
  VA        : DWORD;
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
  BPCount: Integer;
  Context: TContext;
begin
  Context.ContextFlags := CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS;
  if not GetThreadContext(AThreadHandle, Context) then
    Exit;

  Context.Dr7 := Context.Dr7 and not $FF;

  FBreakpointLock.Enter;
  try
    BPCount := 0;
    for var I: Integer := 0 to FBreakpoints.Count - 1 do
    begin
      if Assigned(FBreakpoints[I].Address) and (BPCount < 4) then
      begin
        FBreakpoints[I].Slot := BPCount;
        SetHardwareBreakpointInContext(Context, FBreakpoints[I].Address, BPCount);
        Inc(BPCount);
      end;
    end;
  finally
    FBreakpointLock.Leave;
  end;

  SetThreadContext(AThreadHandle, Context);
end;

function TDebugger.SetBreakpoint(const AUnitName: string; ALineNumber: Integer): Boolean;
begin
  Result := SetBreakpoint(AUnitName, ALineNumber, False);
end;

function TDebugger.SetBreakpoint(const AUnitName: string; ALineNumber: Integer; ARequireAddress: Boolean): Boolean;
var
  Addr: Pointer;
  BP  : TBreakpoint;
begin
  FBreakpointLock.Enter;
  try
    if FBreakpoints.Count >= 4 then
      Exit(False);

    Addr := nil;
    if FBaseAddress <> 0 then
    begin
      Addr := GetAddressFromUnitLine(AUnitName, ALineNumber);
      if ARequireAddress and (Addr = nil) then
        Exit(False);
    end;

    BP := TBreakpoint.Create(AUnitName, ALineNumber, Addr);
    FBreakpoints.Add(BP);
    Result := True;
  finally
    FBreakpointLock.Leave;
  end;
end;

procedure TDebugger.RemoveBreakpoint(const AUnitName: string; ALineNumber: Integer);
begin
  FBreakpointLock.Enter;
  try
    for var I: Integer := FBreakpoints.Count - 1 downto 0 do
    begin
      if SameText(FBreakpoints[I].UnitName, AUnitName) and (FBreakpoints[I].LineNumber = ALineNumber) then
        FBreakpoints.Delete(I);
    end;
  finally
    FBreakpointLock.Leave;
  end;
end;

procedure TDebugger.ClearAllBreakpoints;
begin
  FBreakpoints.Clear;
end;

procedure TDebugger.HandleCreateProcess(const ADebugEvent: TDebugEvent);
begin
  FBaseAddress := UIntPtr(ADebugEvent.CreateProcessInfo.lpBaseOfImage);
  FProcessHandle := ADebugEvent.CreateProcessInfo.hProcess;
  FThreadHandle := ADebugEvent.CreateProcessInfo.hThread;

  if ADebugEvent.CreateProcessInfo.hFile <> 0 then
    CloseHandle(ADebugEvent.CreateProcessInfo.hFile);

  FActiveThreads.Add(ADebugEvent.CreateProcessInfo.hThread);

  for var I: Integer := 0 to FBreakpoints.Count - 1 do
  begin
    if not Assigned(FBreakpoints[I].Address) then
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
  BP           : TBreakpoint;
  Context      : TContext;
  CurrentThread: THandle;
  Handled      : Boolean;
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
        for var I: Integer := 0 to FBreakpoints.Count - 1 do
        begin
          if (FBreakpoints[I].Slot >= 0) and ((Context.Dr6 and (1 shl FBreakpoints[I].Slot)) <> 0) then
          begin
            BP := FBreakpoints[I];
            Break;
          end;
        end;

        if (BP <> nil) and (BP = FStepReturnBP) then
        begin
          FBreakpoints.Extract(FStepReturnBP);
          FreeAndNil(FStepReturnBP);

          var Stack := GetStackTrace(CurrentThread);
          var CurLine := 0;
          if Length(Stack) > 0 then
            CurLine := Stack[0].LineNumber;

          if CurLine > 0 then
          begin
            if (Stack[0].UnitName <> FStepStartUnit) or (CurLine <> FStepStartLine) then
            begin
              FStepType := stNone;
              Context.EFlags := Context.EFlags and not $100;
              Context.EFlags := Context.EFlags or $10000; // RF
              Context.Dr6 := Context.Dr6 and not $F;
              FLastBreakpointHit := TBreakpoint.Create(Stack[0].UnitName, CurLine, Stack[0].Address);
              FLastThreadHit := CurrentThread;
              FBreakpointHitEvent.SetEvent;
              if Assigned(FOnStepped) then FOnStepped(Self, FLastBreakpointHit);
              FContinueEvent.ResetEvent;
              FContinueEvent.WaitFor(INFINITE);
              if FStepType <> stNone then Context.EFlags := Context.EFlags or $100;
              SetThreadContext(CurrentThread, Context);
              AContinueStatus := DBG_CONTINUE;
            end
            else
            begin
              Context.EFlags := Context.EFlags or $10000; // RF
              Context.Dr6 := Context.Dr6 and not $F;
              Context.EFlags := Context.EFlags or $100; // TF
              SetThreadContext(CurrentThread, Context);
              AContinueStatus := DBG_CONTINUE;
            end;
          end
          else
          begin
            Context.EFlags := Context.EFlags or $10000; // RF
            Context.Dr6 := Context.Dr6 and not $F;
            Context.EFlags := Context.EFlags or $100; // TF
            SetThreadContext(CurrentThread, Context);
            AContinueStatus := DBG_CONTINUE;
          end;
        end
        else if BP <> nil then
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
            else if CurLine = 0 then
            begin
              var RetAddr := ReadProcessMemoryPtr(Pointer(Context.Esp));
              if RetAddr <> nil then
              begin
                FStepReturnBP := TBreakpoint.Create('', 0, RetAddr);
                FBreakpoints.Add(FStepReturnBP);
                Context.EFlags := Context.EFlags and not $100; // Clear TF
                SetThreadContext(CurrentThread, Context);
                AContinueStatus := DBG_CONTINUE;
              end
              else
              begin
                Context.EFlags := Context.EFlags or $100;
                SetThreadContext(CurrentThread, Context);
                AContinueStatus := DBG_CONTINUE;
              end;
              Exit;
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
            if Assigned(FOnStepped) then FOnStepped(Self, FLastBreakpointHit);
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
  ContinueStatus: DWORD;
  DebugEvent    : TDebugEvent;
  hDevNull      : THandle;
  I             : Integer;
  ProcessInfo   : TProcessInformation;
  Running       : Boolean;
  SA            : TSecurityAttributes;
  StartupInfo   : TStartupInfo;
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
    if hDevNull <> INVALID_HANDLE_VALUE then
      CloseHandle(hDevNull);
    raise Exception.Create('Failed to create process: ' + IntToStr(GetLastError));
  end;

  FProcessId := ProcessInfo.dwProcessId;
  FThreadId := ProcessInfo.dwThreadId;
  FProcessHandle := ProcessInfo.hProcess;
  FThreadHandle := ProcessInfo.hThread;

  if hDevNull <> INVALID_HANDLE_VALUE then
    CloseHandle(hDevNull); // Process has its own copy now

  Running := True;
  while Running do
  begin
    if not WaitForDebugEvent(DebugEvent, INFINITE) then
      Break;
    ContinueStatus := DBG_CONTINUE;
    case DebugEvent.dwDebugEventCode of
      CREATE_PROCESS_DEBUG_EVENT: HandleCreateProcess(DebugEvent);
      CREATE_THREAD_DEBUG_EVENT: HandleCreateThread(DebugEvent);
      EXIT_THREAD_DEBUG_EVENT: HandleExitThread(DebugEvent);
      EXCEPTION_DEBUG_EVENT: HandleException(DebugEvent, ContinueStatus);
      LOAD_DLL_DEBUG_EVENT:
        if DebugEvent.LoadDll.hFile <> 0 then
          CloseHandle(DebugEvent.LoadDll.hFile);
      EXIT_PROCESS_DEBUG_EVENT: 
      begin
        Running := False;
        FLastBreakpointHit := nil;
        FLastException.ExceptionCode := 0;
        if Assigned(FOnProcessExit) then
          FOnProcessExit(Self, DebugEvent.ExitProcess.dwExitCode);
      end;
    end;

    if Running then
    begin
      for I := 0 to FActiveThreads.Count - 1 do
        ApplyBreakpointsToThread(FActiveThreads[I]);
    end;

    if not ContinueDebugEvent(DebugEvent.dwProcessId, DebugEvent.dwThreadId, ContinueStatus) then
      Break;
  end;
  FBreakpointHitEvent.SetEvent;
  FFinishedEvent.SetEvent;
end;

end.
