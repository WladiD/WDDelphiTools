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
  System.SyncObjs,
  System.SysUtils,

  mormot.core.collections,

  DPT.MapFileParser,
  DPT.Rsm.LocalsReader;

function OpenThread(dwDesiredAccess: Cardinal; bInheritHandle: BOOL; dwThreadId: Cardinal): THandle; stdcall; external kernel32;

const

  EXCEPTION_DELPHI_LANGUAGE   = Cardinal($0EEDFADE);
  EXCEPTION_MS_VC_THREAD_NAME = Cardinal($406D1388);
  STATUS_WX86_BREAKPOINT      = Cardinal($4000001F);
  STATUS_WX86_SINGLE_STEP     = Cardinal($4000001E);
  THREAD_GET_CONTEXT          = $0008;
  THREAD_SET_CONTEXT          = $0010;

type

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
    EFlags: Cardinal;
    {$IFDEF CPUX64}
    R8 : UIntPtr;
    R9 : UIntPtr;
    R10: UIntPtr;
    R11: UIntPtr;
    R12: UIntPtr;
    R13: UIntPtr;
    R14: UIntPtr;
    R15: UIntPtr;
    {$ENDIF}
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

  TLocalVar = record
    BpOffset: Int32;
    Name    : String;
    /// <summary>
    ///   Eight raw bytes read from the target's stack frame at
    ///   <c>BpOffset</c> from EBP/RBP. Callers interpret the bytes as
    ///   little-endian according to the variable's source-level type
    ///   (Integer/Cardinal use the first 4 bytes, Int64/Pointer use all 8).
    ///   Empty when the read failed.
    /// </summary>
    RawBytes: TBytes;
    /// <summary>
    ///   How the local was reached: a base-pointer offset on the stack
    ///   (<c>lkBpRel</c>) or a CPU register (<c>lkRegister</c>). The
    ///   dotted-walk in <c>EvaluateVariable</c> needs this to decide
    ///   whether the first-segment instance pointer can be taken
    ///   directly from <c>RawBytes</c> (register-passed class param --
    ///   the register's value IS the instance pointer, with no slot
    ///   address to dereference) or must come from the stack slot
    ///   address through <c>ReadTargetPointer</c>.
    /// </summary>
    Kind    : TRsmLocalKind;
  end;

  TStepType = (stNone, stInto, stOver);

  TCapturedOutputSource = (cosStdout, cosStderr, cosOds);

  /// <summary>
  ///   A single line emitted by the debugged process. <c>Index</c> is a
  ///   monotonically increasing 1-based identifier; callers can pass it
  ///   to <see cref="TDebugger.GetCapturedOutput"/> as a cursor to retrieve
  ///   only lines newer than the previous read.
  /// </summary>
  TCapturedOutputLine = record
    Index : Integer;
    Source: TCapturedOutputSource;
    Text  : String;
  end;

  TOnBreakpointEvent = procedure(Sender: TObject; Breakpoint: TBreakpoint) of object;
  TOnSteppedEvent = procedure(Sender: TObject; Breakpoint: TBreakpoint) of object;
  TOnExceptionEvent = procedure(Sender: TObject; const ExceptionRecord: TExceptionRecord; const FirstChance: Boolean; var Handled: Boolean) of object;
  TOnProcessExitEvent = procedure(Sender: TObject; ExitCode: Cardinal) of object;

  TDebugger = class
  private
    FActiveThreads           : IKeyValue<Cardinal, THandle>;
    FBaseAddress             : UIntPtr;
    FBreakpointHitEvent      : TEvent;
    FBreakpointLock          : TCriticalSection;
    FBreakpoints             : IList<TBreakpoint>;
    FContinueEvent           : TEvent;
    FFinishedEvent           : TEvent;
    FFirstBreak              : Boolean;
    FIgnoredExceptions       : IList<String>;
    FLastBreakpointHit       : TBreakpoint;
    FLastException           : TExceptionRecord;
    FLastExceptionFirstChance: Boolean;
    FLastThreadHit           : THandle;
    FLastThreadId            : Cardinal;
    FLocalsReader            : TRsmLocalsReader;
    FMapScanner              : TMapFileParser;
    FOutputBuffer            : IList<TCapturedOutputLine>;
    FOutputLock              : TCriticalSection;
    FOutputNextIndex         : Integer;
    FOutputStdoutRead        : THandle;
    FOutputStdoutWrite       : THandle;
    FOutputStdoutThread      : TThread;
    FOutputStderrRead        : THandle;
    FOutputStderrWrite       : THandle;
    FOutputStderrThread      : TThread;
    FOnBreakpoint            : TOnBreakpointEvent;
    FOnException             : TOnExceptionEvent;
    FOnProcessExit           : TOnProcessExitEvent;
    FOnStepped               : TOnSteppedEvent;
    FProcessHandle           : THandle;
    FProcessId               : Cardinal;
    FReadyEvent              : TEvent;
    FStepReturnBP            : TBreakpoint;
    FStepStartDepth          : Integer;
    FStepStartLine           : Integer;
    FStepStartUnit           : String;
    FStepType                : TStepType;
    FTargetIs32Bit           : Boolean;
    FTargetPointerSize       : Integer;
    FTerminated              : Boolean;
    FThreadHandle            : THandle;
    FThreadId                : Cardinal;
    procedure ApplyBreakpointsToThread(AThreadHandle: THandle);
    {$IFDEF CPUX64}
    function  GetContextFlags(AFlags: Cardinal): Cardinal;
    {$ENDIF}
    function  GetTargetContext(AThreadHandle: THandle; AFlags: Cardinal; out AContext: TContext{$IFDEF CPUX64}; out AWow64Context: TWow64Context{$ENDIF}): Boolean;
    procedure HandleCreateProcess(const ADebugEvent: TDebugEvent);
    procedure HandleCreateThread(const ADebugEvent: TDebugEvent);
    procedure HandleException(const ADebugEvent: TDebugEvent; var AContinueStatus: Cardinal);
    procedure HandleExitThread(const ADebugEvent: TDebugEvent);
    procedure HandleOutputDebugString(const ADebugEvent: TDebugEvent);
    procedure AppendOutputLine(ASource: TCapturedOutputSource; const AText: String);
    procedure ConsumePipeBytes(ASource: TCapturedOutputSource; var APartial: TBytes;
      const ABytes: TBytes; ALength: Integer);
    procedure StartOutputReaders;
    procedure StopOutputReaders;
    procedure PatchRsmProcAddressesFromMap;
    function  ReadTargetPointer(AAddress: Pointer): UIntPtr;
    /// <summary>
    ///   Resolve a parameter's register slot to the same 8-byte raw-bytes
    ///   shape ReadProcessMemory produces for stack-slot locals, so that
    ///   the rest of GetLocals / Evaluate doesn't need a special-case
    ///   path. Delphi's register calling convention orders the first
    ///   scalar parameters as follows:
    ///   <list type="bullet">
    ///     <item>Win64 ABI: RCX, RDX, R8, R9.</item>
    ///     <item>Win32 default register: EAX, EDX, ECX.</item>
    ///   </list>
    ///   Registers beyond the four (Win64) or three (Win32) slots are
    ///   passed on the stack; we have no encoding for those yet, so the
    ///   caller will see an empty byte slice for them.
    /// </summary>
    function  RegisterParamBytes(const ARegs: TRegisters; ARegParamIdx: Byte): TBytes;
    function  SetTargetContext(AThreadHandle: THandle; var AContext: TContext{$IFDEF CPUX64}; var AWow64Context: TWow64Context{$ENDIF}): Boolean;
    procedure SetHardwareBreakpointInContext(var AContext: TContext; {$IFDEF CPUX64}var AWow64Context: TWow64Context;{$ENDIF} AAddress: Pointer; ASlot: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAllBreakpoints;
    procedure Detach;
    function  GetAddressFromSymbol(const ASymbolName: string): Pointer;
    function  GetAddressFromUnitLine(const AUnitName: string; ALineNumber: Integer): Pointer;
    function  GetRegisters(AThreadHandle: THandle): TRegisters;
    function  GetCapturedOutput(ASinceIndex: Integer = 0): TArray<TCapturedOutputLine>;
    function  GetCapturedOutputCount: Integer;
    function  GetCurrentProcedureName(AThreadHandle: THandle): String;
    function  GetLocals(AThreadHandle: THandle): TArray<TLocalVar>;
    /// <summary>
    ///   Resolves a named local of the procedure that contains the
    ///   current PC to its runtime address (EBP/RBP + signed offset,
    ///   with the Win64 frame-base correction applied). Returns
    ///   <c>False</c> when the PC is outside any TD32-covered procedure
    ///   or no local with the given name exists. Use this to read more
    ///   bytes than <see cref="GetLocals"/>'s fixed 8-byte slice would
    ///   provide (e.g. for inline value types like ShortString).
    /// </summary>
    function  GetLocalAddress(AThreadHandle: THandle; const AName: String;
      out AAddress: Pointer): Boolean;
    function  EvaluateVariable(const AName, AType: String; out AValue: String;
      AOnPhase: TProc<String> = nil): Boolean;
    function  GetStackFrameInfo(AThreadHandle: THandle): TStackFrameInfo;
    function  GetStackSlots(AThreadHandle: THandle; AMaxSlots: Integer = 20): TArray<TStackSlot>;
    function  GetStackTrace(AThreadHandle: THandle): TArray<TStackFrame>;
    function  GetThreadIds: TArray<Cardinal>;
    procedure IgnoreException(const AClassName: String);
    /// <summary>
    ///   Loads TD32 debug info embedded in the given Delphi PE executable
    ///   (built with linker option -VR / "Include remote debug symbols").
    ///   Required for <see cref="GetLocals"/>; safe to skip when only map
    ///   file based functionality is needed.
    /// </summary>
    procedure LoadDebugInfoFromExe(const AExePath: string;
      AOnPhase: TProc<String> = nil);
    procedure LoadMapFile(const AMapFileName: string);
    function  ReadExceptionClassName(const AExceptionRecord: TExceptionRecord): String;
    function  ReadProcessMemory(AAddress: Pointer; ASize: NativeUInt): TBytes;
    procedure RemoveBreakpoint(const AUnitName: string; ALineNumber: Integer);
    procedure ResumeExecution;
    function  SetBreakpoint(const AUnitName: string; ALineNumber: Integer): Boolean; overload;
    function  SetBreakpoint(const AUnitName: string; ALineNumber: Integer; ARequireAddress: Boolean): Boolean; overload;
    function  SetThreadFocus(AThreadId: Cardinal): Boolean;
    procedure StartDebugging(const AExecutablePath: string);
    procedure StepInto;
    procedure StepOver;
    procedure Terminate;
    procedure UnignoreException(const AClassName: String);
    function  WaitForBreakpoint(Timeout: Cardinal = INFINITE): TBreakpoint;
    procedure WaitForReady(Timeout: Cardinal = INFINITE);
    property  BaseAddress: UIntPtr read FBaseAddress;
    property  BreakpointLock: TCriticalSection read FBreakpointLock;
    property  Breakpoints: IList<TBreakpoint> read FBreakpoints;
    property  IgnoredExceptions: IList<String> read FIgnoredExceptions;
    property  LastException: TExceptionRecord read FLastException;
    property  LastExceptionFirstChance: Boolean read FLastExceptionFirstChance;
    property  LastThreadHit: THandle read FLastThreadHit;
    property  LastThreadId: Cardinal read FLastThreadId;
    property  LocalsReader: TRsmLocalsReader read FLocalsReader;
    property  OnBreakpoint: TOnBreakpointEvent read FOnBreakpoint write FOnBreakpoint;
    property  OnException: TOnExceptionEvent read FOnException write FOnException;
    property  OnProcessExit: TOnProcessExitEvent read FOnProcessExit write FOnProcessExit;
    property  OnStepped: TOnSteppedEvent read FOnStepped write FOnStepped;
    property  TargetIs32Bit: Boolean read FTargetIs32Bit;
    property  TargetPointerSize: Integer read FTargetPointerSize;
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

uses

  DPT.Logger;

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
  FBreakpoints := Collections.NewList<TBreakpoint>;
  FActiveThreads := Collections.NewKeyValue<Cardinal, THandle>;
  FBreakpointLock := TCriticalSection.Create;
  FIgnoredExceptions := Collections.NewList<String>;
  FIgnoredExceptions.Add('EAbort');
  FContinueEvent := TEvent.Create(nil, False, False, '');
  FBreakpointHitEvent := TEvent.Create(nil, False, False, '');
  FReadyEvent := TEvent.Create(nil, False, False, '');
  FFinishedEvent := TEvent.Create(nil, True, False, '');
  FOutputBuffer := Collections.NewPlainList<TCapturedOutputLine>;
  FOutputLock := TCriticalSection.Create;
  FOutputNextIndex := 1;
  FStepType := stNone;
  FFirstBreak := True;
  FTerminated := False;
  FTargetPointerSize := SizeOf(Pointer);
end;

procedure TDebugger.IgnoreException(const AClassName: String);
begin
  for var I: Integer := 0 to FIgnoredExceptions.Count - 1 do
    if SameText(FIgnoredExceptions[I], AClassName) then Exit;
  FIgnoredExceptions.Add(AClassName);
end;

procedure TDebugger.UnignoreException(const AClassName: String);
begin
  for var I: Integer := FIgnoredExceptions.Count - 1 downto 0 do
    if SameText(FIgnoredExceptions[I], AClassName) then
      FIgnoredExceptions.Delete(I);
end;

function TDebugger.ReadExceptionClassName(const AExceptionRecord: TExceptionRecord): String;
var
  Buffer          : TBytes;
  ClassNamePtr    : UIntPtr;
  Len             : Byte;
  ObjPtr          : UIntPtr;
  VMTClassNameOfs : Integer;
  VMTPtr          : UIntPtr;

begin
  Result := '';
  if (AExceptionRecord.ExceptionCode <> EXCEPTION_DELPHI_LANGUAGE) or (AExceptionRecord.NumberParameters < 2) then
    Exit;

  ObjPtr := UIntPtr(AExceptionRecord.ExceptionInformation[1]);
  if ObjPtr = 0 then
    Exit;

  VMTPtr := ReadTargetPointer(Pointer(ObjPtr));
  if VMTPtr = 0 then
    Exit;

  // vmtClassName offset depends on target bitness and CPP_ABI_SUPPORT
  // Win32: vmtClassName = -56 (no CPP_ABI_ADJUST)
  // Win64: vmtClassName = -112 - 3*8 = -136 (CPP_ABI_ADJUST = 24)
  if FTargetIs32Bit then
    VMTClassNameOfs := 56
  else
    VMTClassNameOfs := 136;

  ClassNamePtr := ReadTargetPointer(Pointer(VMTPtr - UIntPtr(VMTClassNameOfs)));
  if ClassNamePtr = 0 then
    Exit;

  Buffer := ReadProcessMemory(Pointer(ClassNamePtr), 256);
  if FTargetIs32Bit then
  begin
    // Win32: plain ShortString (Length: Byte + Data)
    if Length(Buffer) > 1 then
    begin
      Len := Buffer[0];
      if (Len > 0) and (1 + Integer(Len) <= Length(Buffer)) then
        Result := TEncoding.ANSI.GetString(Buffer, 1, Len);
    end;
  end
  else
  begin
    // Win64: plain ShortString (same as Win32)
    if Length(Buffer) > 1 then
    begin
      Len := Buffer[0];
      if (Len > 0) and (1 + Integer(Len) <= Length(Buffer)) then
        Result := TEncoding.ANSI.GetString(Buffer, 1, Len);
    end;
  end;
end;

destructor TDebugger.Destroy;
begin
  Terminate;
  StopOutputReaders;
  FFinishedEvent.Free;
  FReadyEvent.Free;
  FBreakpointHitEvent.Free;
  FContinueEvent.Free;
  FBreakpointLock.Free;
  FOutputLock.Free;
  FLocalsReader.Free;
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
  {$IFDEF CPUX64}
  Wow64Context: TWow64Context;
  {$ENDIF}
begin
  if FTerminated then
    Exit;
  FTerminated := True;

  for var e in FActiveThreads do
  begin
    var LThreadHandle := e.Value;
    if GetTargetContext(LThreadHandle, CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF}) then
    begin
      {$IFDEF CPUX64}
      if FTargetIs32Bit then
      begin
        Wow64Context.Dr0 := 0;
        Wow64Context.Dr1 := 0;
        Wow64Context.Dr2 := 0;
        Wow64Context.Dr3 := 0;
        Wow64Context.Dr7 := Wow64Context.Dr7 and not Cardinal($FF);
      end
      else
      {$ENDIF}
      begin
        Context.Dr0 := 0;
        Context.Dr1 := 0;
        Context.Dr2 := 0;
        Context.Dr3 := 0;
        Context.Dr7 := Context.Dr7 and not $FF;
      end;
      SetTargetContext(LThreadHandle, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
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

procedure TDebugger.WaitForReady(Timeout: Cardinal);
begin
  FReadyEvent.WaitFor(Timeout);
end;

function TDebugger.WaitForBreakpoint(Timeout: Cardinal): TBreakpoint;
begin
  if FBreakpointHitEvent.WaitFor(Timeout) = wrSignaled then
    Result := FLastBreakpointHit
  else
    Result := nil;
end;

{$IFDEF CPUX64}
function TDebugger.GetContextFlags(AFlags: Cardinal): Cardinal;
begin
  if FTargetIs32Bit then
  begin
    Result := 0;
    if (AFlags and CONTEXT_CONTROL) = CONTEXT_CONTROL then
      Result := Result or Cardinal(WOW64_CONTEXT_CONTROL);
    if (AFlags and CONTEXT_INTEGER) = CONTEXT_INTEGER then
      Result := Result or Cardinal(WOW64_CONTEXT_INTEGER);
    if (AFlags and CONTEXT_DEBUG_REGISTERS) = CONTEXT_DEBUG_REGISTERS then
      Result := Result or Cardinal(WOW64_CONTEXT_DEBUG_REGISTERS);
    if (AFlags and CONTEXT_FULL) = CONTEXT_FULL then
      Result := Result or Cardinal(WOW64_CONTEXT_FULL);
  end
  else
    Result := AFlags;
end;
{$ENDIF}

function TDebugger.GetTargetContext(AThreadHandle: THandle; AFlags: Cardinal;
  out AContext: TContext{$IFDEF CPUX64}; out AWow64Context: TWow64Context{$ENDIF}): Boolean;
{$IFDEF CPUX64}
var
  Buf: PContext;
{$ENDIF}
begin
  {$IFDEF CPUX64}
  FillChar(AWow64Context, SizeOf(AWow64Context), 0);
  if FTargetIs32Bit then
  begin
    AWow64Context.ContextFlags := GetContextFlags(AFlags);
    Result := Wow64GetThreadContext(AThreadHandle, AWow64Context);
    Exit;
  end;
  // TContext on x64 requires 16-byte alignment for XMM register area.
  // Delphi may not align stack variables to 16 bytes, so heap-allocate.
  GetMem(Buf, SizeOf(TContext) + 15);
  try
    var AlignedCtx := PContext((UIntPtr(Buf) + 15) and not UIntPtr(15));
    FillChar(AlignedCtx^, SizeOf(TContext), 0);
    AlignedCtx.ContextFlags := AFlags;
    Result := GetThreadContext(AThreadHandle, AlignedCtx^);
    if Result then
      Move(AlignedCtx^, AContext, SizeOf(TContext))
    else
      FillChar(AContext, SizeOf(AContext), 0);
  finally
    FreeMem(Buf);
  end;
  {$ELSE}
  FillChar(AContext, SizeOf(AContext), 0);
  AContext.ContextFlags := AFlags;
  Result := GetThreadContext(AThreadHandle, AContext);
  {$ENDIF}
end;

function TDebugger.SetTargetContext(AThreadHandle: THandle;
  var AContext: TContext{$IFDEF CPUX64}; var AWow64Context: TWow64Context{$ENDIF}): Boolean;
{$IFDEF CPUX64}
var
  Buf: PContext;
{$ENDIF}
begin
  {$IFDEF CPUX64}
  if FTargetIs32Bit then
    Exit(Wow64SetThreadContext(AThreadHandle, AWow64Context));
  GetMem(Buf, SizeOf(TContext) + 15);
  try
    var AlignedCtx := PContext((UIntPtr(Buf) + 15) and not UIntPtr(15));
    Move(AContext, AlignedCtx^, SizeOf(TContext));
    Result := SetThreadContext(AThreadHandle, AlignedCtx^);
  finally
    FreeMem(Buf);
  end;
  {$ELSE}
  Result := SetThreadContext(AThreadHandle, AContext);
  {$ENDIF}
end;

function TDebugger.ReadTargetPointer(AAddress: Pointer): UIntPtr;
var
  BytesRead: NativeUInt;
  Val32: Cardinal;
begin
  Result := 0;
  if FTargetIs32Bit then
  begin
    Val32 := 0;
    Winapi.Windows.ReadProcessMemory(FProcessHandle, AAddress, @Val32, 4, BytesRead);
    Result := Val32;
  end
  else
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

function TDebugger.RegisterParamBytes(const ARegs: TRegisters; ARegParamIdx: Byte): TBytes;
var
  Val: UInt64;
begin
  Val := 0;
  if FTargetIs32Bit then
  begin
    case ARegParamIdx of
      0: Val := UInt64(UInt32(ARegs.Eax));
      1: Val := UInt64(UInt32(ARegs.Edx));
      2: Val := UInt64(UInt32(ARegs.Ecx));
    else
      SetLength(Result, 0);
      Exit;
    end;
  end
  else
  begin
    {$IFDEF CPUX64}
    case ARegParamIdx of
      0: Val := UInt64(ARegs.Ecx);  // RCX
      1: Val := UInt64(ARegs.Edx);  // RDX
      2: Val := UInt64(ARegs.R8);
      3: Val := UInt64(ARegs.R9);
    else
      SetLength(Result, 0);
      Exit;
    end;
    {$ELSE}
    SetLength(Result, 0);
    Exit;
    {$ENDIF}
  end;
  SetLength(Result, 8);
  Move(Val, Result[0], 8);
end;

function TDebugger.GetRegisters(AThreadHandle: THandle): TRegisters;
var
  Context: TContext;
  {$IFDEF CPUX64}
  Wow64Context: TWow64Context;
  {$ENDIF}
begin
  FillChar(Result, SizeOf(Result), 0);
  if not GetTargetContext(AThreadHandle, CONTEXT_FULL, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF}) then
    Exit;
  {$IFDEF CPUX64}
  if FTargetIs32Bit then
  begin
    Result.Eip := Wow64Context.Eip;
    Result.Esp := Wow64Context.Esp;
    Result.Ebp := Wow64Context.Ebp;
    Result.Eax := Wow64Context.Eax;
    Result.Ebx := Wow64Context.Ebx;
    Result.Ecx := Wow64Context.Ecx;
    Result.Edx := Wow64Context.Edx;
    Result.Esi := Wow64Context.Esi;
    Result.Edi := Wow64Context.Edi;
    Result.EFlags := Wow64Context.EFlags;
  end
  else
  begin
    Result.Eip := Context.Rip;
    Result.Esp := Context.Rsp;
    Result.Ebp := Context.Rbp;
    Result.Eax := Context.Rax;
    Result.Ebx := Context.Rbx;
    Result.Ecx := Context.Rcx;
    Result.Edx := Context.Rdx;
    Result.Esi := Context.Rsi;
    Result.Edi := Context.Rdi;
    Result.EFlags := Context.EFlags;
    Result.R8  := Context.R8;
    Result.R9  := Context.R9;
    Result.R10 := Context.R10;
    Result.R11 := Context.R11;
    Result.R12 := Context.R12;
    Result.R13 := Context.R13;
    Result.R14 := Context.R14;
    Result.R15 := Context.R15;
  end;
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
  Result.EFlags := Context.EFlags;
  {$ENDIF}
end;

function TDebugger.GetStackSlots(AThreadHandle: THandle; AMaxSlots: Integer): TArray<TStackSlot>;
var
  Regs      : TRegisters;
  RelativeVA: UInt64;
  ResultList: IList<TStackSlot>;
  Slot      : TStackSlot;
  SymbolName: String;
  Val       : UIntPtr;
begin
  Regs := GetRegisters(AThreadHandle);
  ResultList := Collections.NewPlainList<TStackSlot>;
  for var I: Integer := 0 to AMaxSlots - 1 do
  begin
    Slot.Offset := -(I * FTargetPointerSize);
    Slot.Address := PByte(Regs.Ebp) + Slot.Offset;

    if NativeInt(Slot.Address) < NativeInt(Regs.Esp) then
      Break;

    Val := ReadTargetPointer(Slot.Address);
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
        Slot.Interpretation := '$' + IntToHex(Val, FTargetPointerSize * 2);
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
  RelativeVA: UInt64;
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
  if Length(Buf) < 3 then
    Exit;

  P := 0;
  if not FTargetIs32Bit then
  begin
    // Delphi x64 prologue: callee-saved register pushes, then
    // sub rsp, N, then mov rbp, rsp. The register-push set is variable
    // (push rbp is always there; push rdi / push rsi / push rbx / etc.
    // appear when the function uses those registers). Each non-extended
    // register push is one byte ($50..$57); each R8..R15 push is two
    // bytes ($41 $50..$57). Skip all of them before looking for sub rsp.
    while P < Length(Buf) do
    begin
      if Buf[P] in [$50..$57] then
        Inc(P)
      else if (Buf[P] = $41) and (P + 1 < Length(Buf)) and (Buf[P + 1] in [$50..$57]) then
        Inc(P, 2)
      else
        Break;
    end;
    // sub rsp, imm8 (48 83 EC XX)
    if (P + 3 < Length(Buf)) and (Buf[P] = $48) and (Buf[P+1] = $83) and (Buf[P+2] = $EC) then
      Result.LocalSize := Buf[P+3]
    // sub rsp, imm32 (48 81 EC XX XX XX XX)
    else if (P + 6 < Length(Buf)) and (Buf[P] = $48) and (Buf[P+1] = $81) and (Buf[P+2] = $EC) then
      Result.LocalSize := PLongWord(@Buf[P+3])^;
    // After sub rsp, the compiler emits mov rbp, rsp (48 8B EC or 48 89 E5)
    // We don't need to advance P further since LocalSize is already determined.
  end
  else
  begin
    // x86 (32-bit) prologue analysis
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
  Buf        : TBytes;
  Regs       : TRegisters;
  Frame      : TStackFrame;
  FramePtr   : UIntPtr;
  FrameSize  : Integer;
  Frames     : IList<TStackFrame>;
  IdxOffset  : Integer;
  P          : Integer;
  ProcStartVA: UInt64;
  RelativeVA : UInt64;
  ReturnAddr : UIntPtr;
begin
  try

  Regs := GetRegisters(AThreadHandle);
  if Regs.Eip = 0 then
    Exit(nil);

  FramePtr := Regs.Ebp;
  ReturnAddr := Regs.Eip;
  RelativeVA := 0;
  IdxOffset := 0;
  Frames := Collections.NewPlainList<TStackFrame>;

  while (Frames.Count < 50) and (ReturnAddr > FBaseAddress) do
  begin
    Frame.Address := Pointer(ReturnAddr);
    Frame.UnitName := '';
    Frame.ProcedureName := '';
    Frame.LineNumber := 0;

    if Assigned(FMapScanner) and (ReturnAddr >= FBaseAddress + $1000) and
       (ReturnAddr < FBaseAddress + $10000000) then
    begin
      RelativeVA := ReturnAddr - FBaseAddress - $1000;
      Frame.UnitName := FMapScanner.ModuleNameFromAddr(RelativeVA);
      Frame.ProcedureName := FMapScanner.ProcNameFromAddr(RelativeVA, IdxOffset);
      Frame.LineNumber := FMapScanner.LineNumberFromAddr(RelativeVA);
    end;

    Frames.Add(Frame);

    if FramePtr = 0 then Break;

    if (not FTargetIs32Bit) and Assigned(FMapScanner) and (Frame.ProcedureName <> '') then
    begin
      // x64: Delphi prologue is push rbp; sub rsp, N; mov rbp, rsp
      // So saved RBP is at [RBP + N] and return address at [RBP + N + 8]
      ProcStartVA := RelativeVA - UInt64(IdxOffset);
      FrameSize := 0;
      Buf := ReadProcessMemory(Pointer(FBaseAddress + $1000 + ProcStartVA), 16);
      if Length(Buf) >= 5 then
      begin
        P := 0;
        if Buf[P] = $55 then Inc(P); // push rbp
        if (P + 3 < Length(Buf)) and (Buf[P] = $48) and (Buf[P+1] = $83) and (Buf[P+2] = $EC) then
          FrameSize := Buf[P+3]
        else if (P + 6 < Length(Buf)) and (Buf[P] = $48) and (Buf[P+1] = $81) and (Buf[P+2] = $EC) then
          FrameSize := PLongWord(@Buf[P+3])^;
      end;
      ReturnAddr := ReadTargetPointer(PByte(FramePtr) + FrameSize + 8);
      FramePtr := ReadTargetPointer(PByte(FramePtr) + FrameSize);
    end
    else
    begin
      // x86: classic EBP chain - [EBP] = saved EBP, [EBP+4] = return address
      ReturnAddr := ReadTargetPointer(PByte(FramePtr) + FTargetPointerSize);
      FramePtr := ReadTargetPointer(Pointer(FramePtr));
    end;

    if ReturnAddr = 0 then Break;
  end;

  Result := Frames.AsArray;

  except
    // Protect against range check errors or memory read failures during stack walk
  end;
end;

function TDebugger.GetThreadIds: TArray<Cardinal>;
var
  i: Integer;
begin
  SetLength(Result, FActiveThreads.Count);
  i := 0;
  for var e in FActiveThreads do
  begin
    Result[i] := e.Key;
    Inc(i);
  end;
end;

function TDebugger.SetThreadFocus(AThreadId: Cardinal): Boolean;
var
  LThreadHandle: THandle;
begin
  if FActiveThreads.TryGetValue(AThreadId, LThreadHandle) then
  begin
    FLastThreadId := AThreadId;
    FLastThreadHit := LThreadHandle;
    Result := True;
  end
  else
    Result := False;
end;

procedure TDebugger.LoadMapFile(const AMapFileName: string);
begin
  FreeAndNil(FMapScanner);
  if FileExists(AMapFileName) then
    FMapScanner := TMapFileParser.Create(AMapFileName);
  PatchRsmProcAddressesFromMap;
end;

procedure TDebugger.LoadDebugInfoFromExe(const AExePath: string;
  AOnPhase: TProc<String>);
begin
  FreeAndNil(FLocalsReader);
  if not FileExists(AExePath) then
    Exit;
  FLocalsReader := TRsmLocalsReader.Create;
  FLocalsReader.OnPhase := AOnPhase;
  try
    FLocalsReader.LoadFromFile(AExePath);
  except
    FreeAndNil(FLocalsReader);
    raise;
  end;
  PatchRsmProcAddressesFromMap;
end;

procedure TDebugger.PatchRsmProcAddressesFromMap;
// Both Win32 and Win64 proc-address forms are now decoded in
// TRsmLocalsReader.LoadFromBytes, so under normal conditions every
// proc lands with a non-zero SegmentOffset. This routine remains as
// a defensive backstop: if the reader fails to recognize a proc's
// address bytes (e.g. when an unknown encoding variant is encountered
// or the code section exceeds the 2MB range the Win64 decoder
// currently covers), the proc would land at SegmentOffset = 0 and be
// invisible to FindProcContaining. The .map file lists every proc by
// name in the same segment-relative coordinates SegmentOffset uses,
// so we cross-reference: for each RSM proc with a missing offset,
// copy the .map's value into it. After patching, recompute proc
// Sizes by gap so FindProcContaining ranges become correct again.
var
  I       : Integer;
  P       : TRsmProc;
  VA      : UInt64;
  Patched : Boolean;
begin
  if (FLocalsReader = nil) or (FMapScanner = nil) then Exit;
  Patched := False;
  for I := 0 to FLocalsReader.Procs.Count - 1 do
  begin
    P := FLocalsReader.Procs[I];
    if P.SegmentOffset <> 0 then Continue;
    if P.Name = '' then Continue;
    VA := FMapScanner.VAFromSimpleName(P.Name);
    if VA = 0 then Continue;
    P.SegmentOffset := NativeUInt(VA);
    FLocalsReader.Procs[I] := P;
    Patched := True;
  end;
  if Patched then
    FLocalsReader.RecomputeProcSizes;
end;

function TDebugger.GetCurrentProcedureName(AThreadHandle: THandle): String;
const
  CodeSectionRVA = $1000;
var
  ProcIdx   : Integer;
  Regs      : TRegisters;
  SegmentOff: NativeUInt;
begin
  Result := '';
  if not Assigned(FLocalsReader) then
    Exit;

  Regs := GetRegisters(AThreadHandle);
  if Regs.Eip = 0 then
    Exit;
  if Regs.Eip < FBaseAddress + CodeSectionRVA then
    Exit;
  SegmentOff := NativeUInt(Regs.Eip - FBaseAddress - CodeSectionRVA);

  ProcIdx := FLocalsReader.FindProcContaining(SegmentOff);
  if ProcIdx >= 0 then
    Result := FLocalsReader.Procs[ProcIdx].Name;
end;

function TDebugger.GetLocalAddress(AThreadHandle: THandle; const AName: String;
  out AAddress: Pointer): Boolean;
const
  CodeSectionRVA = $1000;
var
  BaseAddr  : NativeInt;
  FrameInfo : TStackFrameInfo;
  ProcIdx   : Integer;
  Proc      : TRsmProc;
  Regs      : TRegisters;
  SegmentOff: NativeUInt;
  I         : Integer;
begin
  Result := False;
  AAddress := nil;
  if not Assigned(FLocalsReader) then
    Exit;

  Regs := GetRegisters(AThreadHandle);
  if Regs.Eip = 0 then
    Exit;
  if Regs.Eip < FBaseAddress + CodeSectionRVA then
    Exit;
  SegmentOff := NativeUInt(Regs.Eip - FBaseAddress - CodeSectionRVA);

  ProcIdx := FLocalsReader.FindProcContaining(SegmentOff);
  if ProcIdx < 0 then
    Exit;

  // Same Win64 frame-base correction as GetLocals: Delphi-x64's
  //   push rbp; sub rsp, N; mov rbp, rsp
  // leaves RBP at the bottom of the locals area, while TD32 emits
  // BPREL offsets relative to the logical top, requiring +LocalSize.
  BaseAddr := NativeInt(Regs.Ebp);
  if not FTargetIs32Bit then
  begin
    FrameInfo := GetStackFrameInfo(AThreadHandle);
    if FrameInfo.LocalSize > 0 then
      BaseAddr := BaseAddr + FrameInfo.LocalSize;
  end;

  Proc := FLocalsReader.Procs[ProcIdx];
  for I := 0 to Proc.Locals.Count - 1 do
    if SameText(Proc.Locals[I].Name, AName) then
    begin
      // Register-passed locals (Self, register params) have no stack
      // slot -- they live in a CPU register, so BpOffset is 0 (a
      // valid frame address that happens to point at the saved-EBP
      // slot). Returning that as the local's address would mislead
      // the caller into dereferencing the saved frame pointer as if
      // it were the parameter's value. The dotted-walk has a
      // dedicated path that sources the instance pointer from
      // <Locals[].RawBytes> for lkRegister; here we just decline.
      if Proc.Locals[I].Kind = lkRegister then
        Exit(False);
      AAddress := Pointer(BaseAddr + Proc.Locals[I].BpOffset);
      Exit(True);
    end;
end;

function TDebugger.GetLocals(AThreadHandle: THandle): TArray<TLocalVar>;
const
  // PE code section is conventionally placed at RVA $1000. TD32 stores
  // procedure offsets segment-relative, so to map current EIP/RIP into
  // a TD32 lookup we subtract image base + $1000.
  CodeSectionRVA = $1000;
  ReadSize       = 8;
var
  BaseAddr  : NativeInt;
  FrameInfo : TStackFrameInfo;
  Loc       : TLocalVar;
  ProcIdx   : Integer;
  Proc      : TRsmProc;
  Regs      : TRegisters;
  ResultList: IList<TLocalVar>;
  SegmentOff: NativeUInt;
  SlotAddr  : UIntPtr;
  I         : Integer;
begin
  Result := nil;
  if not Assigned(FLocalsReader) then
    Exit;

  Regs := GetRegisters(AThreadHandle);
  if Regs.Eip = 0 then
    Exit;
  if Regs.Eip < FBaseAddress + CodeSectionRVA then
    Exit;
  SegmentOff := NativeUInt(Regs.Eip - FBaseAddress - CodeSectionRVA);

  ProcIdx := FLocalsReader.FindProcContaining(SegmentOff);
  if ProcIdx < 0 then
    Exit;

  // Compute the base for BPREL resolution. On x86 Delphi the prologue is
  //   push ebp; mov ebp, esp; sub esp, N
  // so EBP points one slot above the locals and TD32 BPREL offsets are
  // negative offsets directly applicable to EBP.
  // On x64 Delphi the prologue is
  //   push rbp; sub rsp, N; mov rbp, rsp
  // so RBP points to the BOTTOM of the local area while TD32 still
  // emits BPREL offsets relative to the LOGICAL frame top (the slot
  // above saved RBP). To recover that anchor on x64 we add the prologue's
  // LocalSize to RBP, mirroring what the IDE debugger does.
  BaseAddr := NativeInt(Regs.Ebp);
  if not FTargetIs32Bit then
  begin
    FrameInfo := GetStackFrameInfo(AThreadHandle);
    if FrameInfo.LocalSize > 0 then
      BaseAddr := BaseAddr + FrameInfo.LocalSize;
  end;

  Proc := FLocalsReader.Procs[ProcIdx];
  ResultList := Collections.NewPlainList<TLocalVar>;
  for I := 0 to Proc.Locals.Count - 1 do
  begin
    Loc.BpOffset := Proc.Locals[I].BpOffset;
    Loc.Name     := Proc.Locals[I].Name;
    Loc.Kind     := Proc.Locals[I].Kind;
    if Proc.Locals[I].Kind = lkRegister then
      Loc.RawBytes := RegisterParamBytes(Regs, Proc.Locals[I].RegParamIdx)
    else
    begin
      SlotAddr := UIntPtr(BaseAddr + Loc.BpOffset);
      Loc.RawBytes := ReadProcessMemory(Pointer(SlotAddr), ReadSize);
    end;
    ResultList.Add(Loc);
  end;
  Result := ResultList.AsArray;
end;

function TDebugger.GetAddressFromUnitLine(const AUnitName: string; ALineNumber: Integer): Pointer;
var
  Ext       : String;
  LineInfo  : TMapLineNumber;
  SearchUnit: String;
begin
  Result := nil;
  if not Assigned(FMapScanner) then
    Exit;

  // Normalize input: strip path, then strip only .pas/.dpr extension.
  // ChangeFileExt must not be used unconditionally — dotted unit names like
  // 'My.Dotted.Unit' would otherwise have their last dot-segment stripped.
  SearchUnit := ExtractFileName(AUnitName);
  Ext := ExtractFileExt(SearchUnit);
  if SameText(Ext, '.pas') or SameText(Ext, '.dpr') then
    SearchUnit := Copy(SearchUnit, 1, Length(SearchUnit) - Length(Ext));

  for var I: Integer := 0 to FMapScanner.LineNumbersCnt - 1 do
  begin
    LineInfo := FMapScanner.LineNumberByIndex[I];
    if (LineInfo.LineNumber = ALineNumber) and SameText(LineInfo.UnitName, SearchUnit) then
    begin
      Result := Pointer(FBaseAddress + $1000 + LineInfo.VA);
      Exit;
    end;
  end;
end;

function TDebugger.GetAddressFromSymbol(const ASymbolName: string): Pointer;
var
  DotPos    : Integer;
  SymbolName: String;
  UnitName  : String;
  VA        : UInt64;
begin
  Result := nil;
  if not Assigned(FMapScanner) then
    Exit;

  DotPos := Pos('.', ASymbolName);
  if DotPos > 0 then
  begin
    UnitName := Copy(ASymbolName, 1, DotPos - 1);
    SymbolName := Copy(ASymbolName, DotPos + 1, MaxInt);
    VA := FMapScanner.VAFromUnitAndProcName(UnitName, SymbolName);
    if VA <> 0 then
      Result := Pointer(FBaseAddress + $1000 + VA);
  end
  else
  begin
    // Unqualified name: search every entry whose qualified name ends in
    // '.ASymbolName'. First hit wins, which is enough for unique global
    // variable names; ambiguous names should be passed in qualified form.
    VA := FMapScanner.VAFromSimpleName(ASymbolName);
    if VA <> 0 then
      Result := Pointer(FBaseAddress + $1000 + VA);
  end;
end;

procedure TDebugger.SetHardwareBreakpointInContext(var AContext: TContext;
  {$IFDEF CPUX64}var AWow64Context: TWow64Context;{$ENDIF} AAddress: Pointer; ASlot: Integer);
begin
  {$IFDEF CPUX64}
  if FTargetIs32Bit then
  begin
    AWow64Context.ContextFlags := AWow64Context.ContextFlags or Cardinal(WOW64_CONTEXT_DEBUG_REGISTERS);
    case ASlot of
      0: AWow64Context.Dr0 := Cardinal(UIntPtr(AAddress));
      1: AWow64Context.Dr1 := Cardinal(UIntPtr(AAddress));
      2: AWow64Context.Dr2 := Cardinal(UIntPtr(AAddress));
      3: AWow64Context.Dr3 := Cardinal(UIntPtr(AAddress));
    end;
    AWow64Context.Dr7 := AWow64Context.Dr7 or Cardinal(1 shl (ASlot * 2));
    AWow64Context.Dr7 := AWow64Context.Dr7 and not Cardinal($F shl (16 + ASlot * 4));
    AWow64Context.Dr7 := AWow64Context.Dr7 or $300;
    Exit;
  end;
  {$ENDIF}
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
  {$IFDEF CPUX64}
  Wow64Context: TWow64Context;
  {$ENDIF}
begin
  if not GetTargetContext(AThreadHandle, CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF}) then
    Exit;

  {$IFDEF CPUX64}
  if FTargetIs32Bit then
    Wow64Context.Dr7 := Wow64Context.Dr7 and not Cardinal($FF)
  else
  {$ENDIF}
    Context.Dr7 := Context.Dr7 and not $FF;

  FBreakpointLock.Enter;
  try
    BPCount := 0;
    for var I: Integer := 0 to FBreakpoints.Count - 1 do
    begin
      if Assigned(FBreakpoints[I].Address) and (BPCount < 4) then
      begin
        FBreakpoints[I].Slot := BPCount;
        SetHardwareBreakpointInContext(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} FBreakpoints[I].Address, BPCount);
        Inc(BPCount);
      end;
    end;
  finally
    FBreakpointLock.Leave;
  end;

  SetTargetContext(AThreadHandle, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
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
{$IFDEF CPUX64}
var
  Wow64: BOOL;
{$ENDIF}
begin
  FBaseAddress := UIntPtr(ADebugEvent.CreateProcessInfo.lpBaseOfImage);
  FProcessHandle := ADebugEvent.CreateProcessInfo.hProcess;
  FThreadHandle := ADebugEvent.CreateProcessInfo.hThread;

  {$IFDEF CPUX64}
  Wow64 := False;
  if IsWow64Process(FProcessHandle, Wow64) and Wow64 then
  begin
    FTargetIs32Bit := True;
    FTargetPointerSize := 4;
  end
  else
  begin
    FTargetIs32Bit := False;
    FTargetPointerSize := 8;
  end;
  {$ELSE}
  FTargetIs32Bit := True;
  FTargetPointerSize := 4;
  {$ENDIF}

  if ADebugEvent.CreateProcessInfo.hFile <> 0 then
    CloseHandle(ADebugEvent.CreateProcessInfo.hFile);

  FActiveThreads[ADebugEvent.dwThreadId] := ADebugEvent.CreateProcessInfo.hThread;

  for var I: Integer := 0 to FBreakpoints.Count - 1 do
  begin
    if not Assigned(FBreakpoints[I].Address) then
      FBreakpoints[I].Address := GetAddressFromUnitLine(FBreakpoints[I].UnitName, FBreakpoints[I].LineNumber);
  end;
end;

procedure TDebugger.HandleCreateThread(const ADebugEvent: TDebugEvent);
begin
  FActiveThreads[ADebugEvent.dwThreadId] := ADebugEvent.CreateThread.hThread;
end;

procedure TDebugger.HandleExitThread(const ADebugEvent: TDebugEvent);
begin
  FActiveThreads.Remove(ADebugEvent.dwThreadId);
end;

procedure TDebugger.HandleException(const ADebugEvent: TDebugEvent; var AContinueStatus: Cardinal);

  {$IFDEF CPUX64}
  procedure CtxSetEFlags(var AContext: TContext; var AWow64Context: TWow64Context; AValue: Cardinal);
  begin
    if FTargetIs32Bit then AWow64Context.EFlags := AValue
    else AContext.EFlags := AValue;
  end;
  function CtxGetEFlags(const AContext: TContext; const AWow64Context: TWow64Context): Cardinal;
  begin
    if FTargetIs32Bit then Result := AWow64Context.EFlags
    else Result := AContext.EFlags;
  end;
  procedure CtxClearDr6Bits(var AContext: TContext; var AWow64Context: TWow64Context);
  begin
    if FTargetIs32Bit then AWow64Context.Dr6 := AWow64Context.Dr6 and not Cardinal($F)
    else AContext.Dr6 := AContext.Dr6 and not $F;
  end;
  function CtxGetDr6(const AContext: TContext; const AWow64Context: TWow64Context): UIntPtr;
  begin
    if FTargetIs32Bit then Result := AWow64Context.Dr6
    else Result := AContext.Dr6;
  end;
  function CtxGetSp(const AContext: TContext; const AWow64Context: TWow64Context): UIntPtr;
  begin
    if FTargetIs32Bit then Result := AWow64Context.Esp
    else Result := AContext.Rsp;
  end;
  {$ELSE}
  procedure CtxSetEFlags(var AContext: TContext; AValue: Cardinal);
  begin
    AContext.EFlags := AValue;
  end;
  function CtxGetEFlags(const AContext: TContext): Cardinal;
  begin
    Result := AContext.EFlags;
  end;
  procedure CtxClearDr6Bits(var AContext: TContext);
  begin
    AContext.Dr6 := AContext.Dr6 and not $F;
  end;
  function CtxGetDr6(const AContext: TContext): UIntPtr;
  begin
    Result := AContext.Dr6;
  end;
  function CtxGetSp(const AContext: TContext): UIntPtr;
  begin
    Result := AContext.Esp;
  end;
  {$ENDIF}

var
  BP           : TBreakpoint;
  Context      : TContext;
  {$IFDEF CPUX64}
  Wow64Context : TWow64Context;
  {$ENDIF}
  CurrentThread: THandle;
  EF           : Cardinal;
  ExCode       : Cardinal;
  Handled      : Boolean;
begin
  AContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
  ExCode := ADebugEvent.Exception.ExceptionRecord.ExceptionCode;


  if (ExCode = EXCEPTION_BREAKPOINT) or (ExCode = STATUS_WX86_BREAKPOINT) then
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
  else if (ExCode = EXCEPTION_SINGLE_STEP) or (ExCode = STATUS_WX86_SINGLE_STEP) then
  begin
    CurrentThread := OpenThread(THREAD_GET_CONTEXT or THREAD_SET_CONTEXT, False, ADebugEvent.dwThreadId);
    if CurrentThread = 0 then Exit;
    try
      if GetTargetContext(CurrentThread, CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF}) then
      begin
        BP := nil;
        for var I: Integer := 0 to FBreakpoints.Count - 1 do
        begin
          if (FBreakpoints[I].Slot >= 0) and ((CtxGetDr6(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF}) and (1 shl FBreakpoints[I].Slot)) <> 0) then
          begin
            BP := FBreakpoints[I];
            Break;
          end;
        end;

        if (BP <> nil) and (BP = FStepReturnBP) then
        begin
          FBreakpoints.Remove(FStepReturnBP);
          FStepReturnBP := nil;

          var Stack := GetStackTrace(CurrentThread);
          var CurLine := 0;
          if Length(Stack) > 0 then
            CurLine := Stack[0].LineNumber;

          if CurLine > 0 then
          begin
            if (Stack[0].UnitName <> FStepStartUnit) or (CurLine <> FStepStartLine) then
            begin
              FStepType := stNone;
              EF := CtxGetEFlags(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
              EF := EF and not $100;
              EF := EF or $10000; // RF
              CtxSetEFlags(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} EF);
              CtxClearDr6Bits(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
              FLastBreakpointHit := TBreakpoint.Create(Stack[0].UnitName, CurLine, Stack[0].Address);
              FLastThreadHit := CurrentThread;
              FLastThreadId := ADebugEvent.dwThreadId;
              FBreakpointHitEvent.SetEvent;
              if Assigned(FOnStepped) then FOnStepped(Self, FLastBreakpointHit);
              FContinueEvent.ResetEvent;
              FContinueEvent.WaitFor(INFINITE);
              if FStepType <> stNone then
              begin
                EF := CtxGetEFlags(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
                CtxSetEFlags(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} EF or $100);
              end;
              SetTargetContext(CurrentThread, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
              AContinueStatus := DBG_CONTINUE;
            end
            else
            begin
              EF := CtxGetEFlags(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
              EF := EF or $10000 or $100; // RF + TF
              CtxSetEFlags(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} EF);
              CtxClearDr6Bits(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
              SetTargetContext(CurrentThread, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
              AContinueStatus := DBG_CONTINUE;
            end;
          end
          else
          begin
            EF := CtxGetEFlags(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
            EF := EF or $10000 or $100; // RF + TF
            CtxSetEFlags(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} EF);
            CtxClearDr6Bits(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
            SetTargetContext(CurrentThread, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
            AContinueStatus := DBG_CONTINUE;
          end;
        end
        else if BP <> nil then
        begin
          FLastBreakpointHit := BP;
          FLastThreadHit := CurrentThread;
          FLastThreadId := ADebugEvent.dwThreadId;
          FBreakpointHitEvent.SetEvent;
          if Assigned(FOnBreakpoint) then
            FOnBreakpoint(Self, BP);

          FContinueEvent.ResetEvent;
          FContinueEvent.WaitFor(INFINITE);

          EF := CtxGetEFlags(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
          EF := EF or $10000; // RF
          CtxClearDr6Bits(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
          if FStepType <> stNone then
            EF := EF or $100; // TF
          CtxSetEFlags(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} EF);

          SetTargetContext(CurrentThread, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
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
              case FStepType of
                stInto:
                  if (CurUnit <> FStepStartUnit) or (CurLine <> FStepStartLine) then
                    StopStepping := True;
                stOver:
                  if ((CurUnit <> FStepStartUnit) or (CurLine <> FStepStartLine)) and (CurDepth <= FStepStartDepth) then
                    StopStepping := True;
              end;
            end
            else if CurLine = 0 then
            begin
              var Sp := CtxGetSp(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
              var RetAddr := ReadTargetPointer(Pointer(Sp));
              if RetAddr <> 0 then
              begin
                FStepReturnBP := TBreakpoint.Create('', 0, Pointer(RetAddr));
                FBreakpoints.Add(FStepReturnBP);
                EF := CtxGetEFlags(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
                CtxSetEFlags(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} EF and not $100);
                SetTargetContext(CurrentThread, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
                AContinueStatus := DBG_CONTINUE;
              end
              else
              begin
                EF := CtxGetEFlags(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
                CtxSetEFlags(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} EF or $100);
                SetTargetContext(CurrentThread, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
                AContinueStatus := DBG_CONTINUE;
              end;
              Exit;
            end;
          end;

          if StopStepping then
          begin
            FStepType := stNone;
            EF := CtxGetEFlags(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
            EF := EF and not $100; // Clear TF

            if Length(Stack) > 0 then
              FLastBreakpointHit := TBreakpoint.Create(Stack[0].UnitName, Stack[0].LineNumber, Stack[0].Address)
            else
              FLastBreakpointHit := TBreakpoint.Create('Unknown', 0, nil);

            FLastThreadHit := CurrentThread;
            FLastThreadId := ADebugEvent.dwThreadId;
            FBreakpointHitEvent.SetEvent;
            if Assigned(FOnStepped) then
              FOnStepped(Self, FLastBreakpointHit);
            FContinueEvent.ResetEvent;
            FContinueEvent.WaitFor(INFINITE);

            if FStepType <> stNone then
              EF := EF or $100;
            CtxSetEFlags(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} EF);
            SetTargetContext(CurrentThread, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
            AContinueStatus := DBG_CONTINUE;
          end
          else
          begin
            EF := CtxGetEFlags(Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
            CtxSetEFlags(Context, {$IFDEF CPUX64}Wow64Context,{$ENDIF} EF or $100); // Keep TF
            SetTargetContext(CurrentThread, Context{$IFDEF CPUX64}, Wow64Context{$ENDIF});
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
  else if (ExCode <> EXCEPTION_BREAKPOINT) and (ExCode <> STATUS_WX86_BREAKPOINT) and
          (ExCode <> EXCEPTION_MS_VC_THREAD_NAME) then
  begin
    if ADebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_DELPHI_LANGUAGE then
    begin
      var ExClass := ReadExceptionClassName(ADebugEvent.Exception.ExceptionRecord);
      if ExClass <> '' then
      begin
        var LIsIgnored := False;
        for var I := 0 to FIgnoredExceptions.Count - 1 do
        begin
          if SameText(FIgnoredExceptions[I], ExClass) then
          begin
            LIsIgnored := True;
            Break;
          end;
        end;
        if LIsIgnored then
        begin
          AContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
          Exit;
        end;
      end;
    end;

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

procedure TDebugger.AppendOutputLine(ASource: TCapturedOutputSource; const AText: String);
var
  Line: TCapturedOutputLine;
begin
  if AText = '' then
    Exit;
  FOutputLock.Enter;
  try
    Line.Index := FOutputNextIndex;
    Line.Source := ASource;
    Line.Text := AText;
    FOutputBuffer.Add(Line);
    Inc(FOutputNextIndex);
  finally
    FOutputLock.Leave;
  end;
end;

procedure TDebugger.ConsumePipeBytes(ASource: TCapturedOutputSource;
  var APartial: TBytes; const ABytes: TBytes; ALength: Integer);
// Splits incoming bytes on LF, decodes each completed line as OEM (Windows
// console default for Delphi WriteLn) into a UTF-16 string, and appends
// to the output buffer. Bytes past the last LF are stashed in <APartial>
// (which is the per-stream accumulator passed in by the caller) so a
// write that straddles a buffer boundary still produces one line.
var
  Combined : TBytes;
  Total    : Integer;
  StartIdx : Integer;
  I        : Integer;
  EndIdx   : Integer;
  LineBytes: TBytes;
  Decoded  : String;
begin
  if ALength <= 0 then
    Exit;
  Total := Length(APartial) + ALength;
  SetLength(Combined, Total);
  if Length(APartial) > 0 then
    Move(APartial[0], Combined[0], Length(APartial));
  Move(ABytes[0], Combined[Length(APartial)], ALength);

  StartIdx := 0;
  I := 0;
  while I < Total do
  begin
    if Combined[I] = 10 then // LF
    begin
      EndIdx := I;
      // strip trailing CR if present
      if (EndIdx > StartIdx) and (Combined[EndIdx - 1] = 13) then
        Dec(EndIdx);
      SetLength(LineBytes, EndIdx - StartIdx);
      if Length(LineBytes) > 0 then
        Move(Combined[StartIdx], LineBytes[0], Length(LineBytes));
      Decoded := TEncoding.GetEncoding(CP_OEMCP).GetString(LineBytes);
      AppendOutputLine(ASource, Decoded);
      StartIdx := I + 1;
    end;
    Inc(I);
  end;

  if StartIdx < Total then
  begin
    SetLength(APartial, Total - StartIdx);
    Move(Combined[StartIdx], APartial[0], Length(APartial));
  end
  else
    SetLength(APartial, 0);
end;

procedure TDebugger.StartOutputReaders;

  function MakeReaderThread(AHandle: THandle; ASource: TCapturedOutputSource;
    APartialPtr: PByte): TThread;
  // Trick: pass the per-stream Partial buffer reference as raw pointer
  // captured in the closure, then alias it back inside the thread body.
  var
    LHandle: THandle;
    LSource: TCapturedOutputSource;
  begin
    LHandle := AHandle;
    LSource := ASource;
    Result := TThread.CreateAnonymousThread(
      procedure
      var
        Buf      : TBytes;
        BytesRead: DWORD;
        Partial  : TBytes;
      begin
        SetLength(Buf, 4096);
        SetLength(Partial, 0);
        while not FTerminated do
        begin
          BytesRead := 0;
          if not Winapi.Windows.ReadFile(LHandle, Buf[0], Length(Buf), BytesRead, nil) then
            Break;
          if BytesRead = 0 then
            Break;
          ConsumePipeBytes(LSource, Partial, Buf, BytesRead);
        end;
        // Flush trailing bytes (no LF) so a final partial line survives.
        if Length(Partial) > 0 then
        begin
          var Trailing := TEncoding.GetEncoding(CP_OEMCP).GetString(Partial);
          SetLength(Partial, 0);
          if Trailing <> '' then
            AppendOutputLine(LSource, Trailing);
        end;
      end);
    Result.FreeOnTerminate := False;
  end;

begin
  if not Assigned(FOutputStdoutThread) then
  begin
    FOutputStdoutThread := MakeReaderThread(FOutputStdoutRead, cosStdout, nil);
    FOutputStdoutThread.Start;
  end;
  if not Assigned(FOutputStderrThread) then
  begin
    FOutputStderrThread := MakeReaderThread(FOutputStderrRead, cosStderr, nil);
    FOutputStderrThread.Start;
  end;
end;

procedure TDebugger.StopOutputReaders;

  procedure CloseAndJoin(var AWriteH, AReadH: THandle; var AThread: TThread);
  begin
    // Closing the write end signals EOF to the blocked ReadFile call.
    if AWriteH <> 0 then
    begin
      CloseHandle(AWriteH);
      AWriteH := 0;
    end;
    if Assigned(AThread) then
    begin
      AThread.WaitFor;
      FreeAndNil(AThread);
    end;
    if AReadH <> 0 then
    begin
      CloseHandle(AReadH);
      AReadH := 0;
    end;
  end;

begin
  CloseAndJoin(FOutputStdoutWrite, FOutputStdoutRead, FOutputStdoutThread);
  CloseAndJoin(FOutputStderrWrite, FOutputStderrRead, FOutputStderrThread);
end;

procedure TDebugger.HandleOutputDebugString(const ADebugEvent: TDebugEvent);
var
  Address  : Pointer;
  Bytes    : TBytes;
  Decoded  : String;
  Len      : Integer;
  Trim     : Integer;
begin
  Address := ADebugEvent.DebugString.lpDebugStringData;
  Len := ADebugEvent.DebugString.nDebugStringLength;
  if (Address = nil) or (Len <= 0) then
    Exit;

  if ADebugEvent.DebugString.fUnicode <> 0 then
    Bytes := ReadProcessMemory(Address, NativeUInt(Len) * SizeOf(WideChar))
  else
    Bytes := ReadProcessMemory(Address, NativeUInt(Len));

  if Length(Bytes) = 0 then
    Exit;

  if ADebugEvent.DebugString.fUnicode <> 0 then
    Decoded := TEncoding.Unicode.GetString(Bytes)
  else
    Decoded := TEncoding.GetEncoding(CP_ACP).GetString(Bytes);

  // Strip trailing nulls and CR/LF that OutputDebugString often includes.
  Trim := Length(Decoded);
  while (Trim > 0) and CharInSet(Decoded[Trim], [#0, #10, #13]) do
    Dec(Trim);
  if Trim < Length(Decoded) then
    SetLength(Decoded, Trim);

  AppendOutputLine(cosOds, Decoded);
end;

function TDebugger.GetCapturedOutput(ASinceIndex: Integer): TArray<TCapturedOutputLine>;
var
  ResultList: IList<TCapturedOutputLine>;
  I         : Integer;
begin
  FOutputLock.Enter;
  try
    ResultList := Collections.NewPlainList<TCapturedOutputLine>;
    for I := 0 to FOutputBuffer.Count - 1 do
      if FOutputBuffer[I].Index > ASinceIndex then
        ResultList.Add(FOutputBuffer[I]);
    Result := ResultList.AsArray;
  finally
    FOutputLock.Leave;
  end;
end;

function TDebugger.GetCapturedOutputCount: Integer;
begin
  FOutputLock.Enter;
  try
    Result := FOutputBuffer.Count;
  finally
    FOutputLock.Leave;
  end;
end;

procedure TDebugger.StartDebugging(const AExecutablePath: string);
var
  ContinueStatus: Cardinal;
  DebugEvent    : TDebugEvent;
  ProcessInfo   : TProcessInformation;
  Running       : Boolean;
  SA            : TSecurityAttributes;
  StartupInfo   : TStartupInfo;
begin
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := nil;
  SA.bInheritHandle := True;

  // Two anonymous pipes: target's stdout writes into FOutputStdoutWrite,
  // stderr writes into FOutputStderrWrite. Two dedicated reader threads
  // drain the read ends and tag each line with the matching source so
  // the agent can later distinguish program output from error output.
  if not CreatePipe(FOutputStdoutRead, FOutputStdoutWrite, @SA, 64 * 1024) then
    raise Exception.Create('Failed to create stdout pipe: ' + IntToStr(GetLastError));
  if not CreatePipe(FOutputStderrRead, FOutputStderrWrite, @SA, 64 * 1024) then
  begin
    CloseHandle(FOutputStdoutRead);
    CloseHandle(FOutputStdoutWrite);
    FOutputStdoutRead := 0;
    FOutputStdoutWrite := 0;
    raise Exception.Create('Failed to create stderr pipe: ' + IntToStr(GetLastError));
  end;
  // Read ends stay on this side, must NOT be inherited by the child.
  SetHandleInformation(FOutputStdoutRead, HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation(FOutputStderrRead, HANDLE_FLAG_INHERIT, 0);

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput := FOutputStdoutWrite;
  StartupInfo.hStdError := FOutputStderrWrite;

  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);

  if not CreateProcess(nil, PChar(AExecutablePath), nil, nil, True, // True to inherit handles
    DEBUG_ONLY_THIS_PROCESS or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
  begin
    StopOutputReaders;
    raise Exception.Create('Failed to create process: ' + IntToStr(GetLastError));
  end;

  FProcessId := ProcessInfo.dwProcessId;
  FThreadId := ProcessInfo.dwThreadId;
  FProcessHandle := ProcessInfo.hProcess;
  FThreadHandle := ProcessInfo.hThread;

  StartOutputReaders;

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
      OUTPUT_DEBUG_STRING_EVENT: HandleOutputDebugString(DebugEvent);
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
      for var e in FActiveThreads do
        ApplyBreakpointsToThread(e.Value);
    end;

    if not ContinueDebugEvent(DebugEvent.dwProcessId, DebugEvent.dwThreadId, ContinueStatus) then
      Break;
  end;
  FBreakpointHitEvent.SetEvent;
  FFinishedEvent.SetEvent;
end;

function TDebugger.EvaluateVariable(const AName, AType: String; out AValue: String;
  AOnPhase: TProc<String>): Boolean;

  procedure Phase(const APhase: String);
  begin
    if Assigned(AOnPhase) then
      AOnPhase(APhase);
  end;

  // Reads a Delphi long string (UnicodeString/AnsiString/WideString) given
  // its dynamic-pointer payload. <APtrVal> is the value stored AT the
  // variable: 0 means empty/nil, otherwise it points at the character
  // data, with a 4-byte length field (in characters for UnicodeString /
  // AnsiString, in bytes for WideString) at offset -4.
  function ReadLongString(APtrVal: UIntPtr; ACharSize: Integer;
    AEncoding: TEncoding; ALengthIsBytes: Boolean; out AOut: String): Boolean;
  var
    LenBytes : TBytes;
    StrLen   : Integer;
    Content  : TBytes;
    ByteCount: Integer;
  begin
    Result := False;
    AOut := '';
    if APtrVal = 0 then
    begin
      Result := True;
      Exit;
    end;
    LenBytes := ReadProcessMemory(Pointer(APtrVal - 4), 4);
    if Length(LenBytes) <> 4 then
      Exit;
    Move(LenBytes[0], StrLen, 4);
    if (StrLen < 0) or (StrLen > 1024 * 1024) then
      Exit;
    if StrLen = 0 then
    begin
      Result := True;
      Exit;
    end;
    if ALengthIsBytes then
      ByteCount := StrLen
    else
      ByteCount := StrLen * ACharSize;
    Content := ReadProcessMemory(Pointer(APtrVal), ByteCount);
    if Length(Content) <> ByteCount then
      Exit;
    AOut := AEncoding.GetString(Content);
    Result := True;
  end;

  // Read a class name from a VMT pointer directly (no instance
  // dereference first). Used to walk the runtime ancestor chain
  // where each step gives us a parent VMT pointer rather than an
  // instance. Same offset constants as ReadRuntimeClassName below
  // -- they were chosen empirically to match the Delphi 12 layout
  // including the CPP_ABI_ADJUST shift on Win64.
  function ReadClassNameFromVMT(AVMTPtr: UIntPtr; out AClassName: String): Boolean;
  var
    ClassNamePtr  : UIntPtr;
    VMTClassNameOfs: Integer;
    Buf           : TBytes;
    Len           : Byte;
  begin
    Result := False;
    AClassName := '';
    if AVMTPtr = 0 then Exit;
    if FTargetIs32Bit then VMTClassNameOfs := 56 else VMTClassNameOfs := 136;
    ClassNamePtr := ReadTargetPointer(Pointer(AVMTPtr - UIntPtr(VMTClassNameOfs)));
    if ClassNamePtr = 0 then Exit;
    Buf := ReadProcessMemory(Pointer(ClassNamePtr), 256);
    if Length(Buf) <= 1 then Exit;
    Len := Buf[0];
    if (Len = 0) or (1 + Integer(Len) > Length(Buf)) then Exit;
    AClassName := TEncoding.ANSI.GetString(Buf, 1, Len);
    Result := True;
  end;

  // VMT walk: given a pointer that's expected to point at a class
  // instance, follow the VMT to read the runtime class name. Returns
  // False on nil pointer or any unreadable indirection.
  function ReadRuntimeClassName(APtrVal: UIntPtr; out AClassName: String): Boolean;
  var
    VMTPtr: UIntPtr;
  begin
    Result := False;
    AClassName := '';
    if APtrVal = 0 then Exit;
    VMTPtr := ReadTargetPointer(Pointer(APtrVal));
    if VMTPtr = 0 then Exit;
    Result := ReadClassNameFromVMT(VMTPtr, AClassName);
  end;

  // Live ancestor walk: when FindClassMember can't find AFieldName in
  // AStartClassName's RSM-derived chain, follow the runtime VMT's
  // vmtParent slot up through each ancestor and try the lookup again
  // on each parent's class name. Authoritative because the chain is
  // exactly what the Delphi runtime uses for class-name reporting --
  // no offset heuristics, no forward stubs. Stops when the field is
  // located, when the parent VMT is nil (TObject reached), or after a
  // defensive depth cap.
  function FindFieldViaVmtWalk(AObjPtr: UIntPtr; const AFieldName: String;
    out AMember: TRsmClassMember): Boolean;
  const
    MaxDepth = 32;

    // Try ReadTargetPointer at <VMTPtr - cand> for each candidate
    // offset; the first one that yields a non-zero pointer whose
    // own VMT-classname slot resolves to a valid class name is
    // taken as vmtParent. Delphi's layout puts vmtParent 8 bytes
    // (Win32) / 16 bytes (Win64) before vmtClassName, but
    // CPP_ABI_ADJUST and version-specific RTL additions shift the
    // exact offset, so we probe both.
    function TryReadParentVMT(AVMT: UIntPtr; out AParentVMT: UIntPtr;
      out AParentName: String): Boolean;
    var
      Ofs, SelfPtrOfs: Integer;
      Cand, Indirect, CandSelfPtr: UIntPtr;
      Name: String;
    begin
      Result := False;
      AParentVMT := 0;
      AParentName := '';
      // Delphi 12's vmtParent slot stores an indirection address, not
      // the parent VMT directly: VMT-48 (Win32) / VMT-120 (Win64)
      // holds the address of a slot whose contents are the actual
      // parent VMT pointer. The compiler emits this indirection so
      // cross-unit class references can be patched by the linker.
      // We confirm the resolved VMT by checking its self-anchor at
      // VMT-vmtSelfPtr (-88 / -176).
      if FTargetIs32Bit then
      begin
        Ofs        := 48;
        SelfPtrOfs := 88;
      end
      else
      begin
        Ofs        := 120;
        SelfPtrOfs := 176;
      end;
      if NativeInt(AVMT) <= Ofs then Exit;
      Indirect := ReadTargetPointer(Pointer(NativeInt(AVMT) - Ofs));
      if Indirect = 0 then Exit;
      // First try: the slot value IS the parent VMT (older Delphi
      // versions without the indirection).
      if NativeInt(Indirect) > SelfPtrOfs then
      begin
        CandSelfPtr := ReadTargetPointer(
          Pointer(NativeInt(Indirect) - SelfPtrOfs));
        if CandSelfPtr = Indirect then
          Cand := Indirect
        else
          Cand := 0;
      end
      else
        Cand := 0;
      // Second try: dereference once more (Delphi 12+ external class
      // indirection).
      if Cand = 0 then
      begin
        Cand := ReadTargetPointer(Pointer(Indirect));
        if Cand = 0 then Exit;
        if NativeInt(Cand) <= SelfPtrOfs then Exit;
        CandSelfPtr := ReadTargetPointer(Pointer(NativeInt(Cand) - SelfPtrOfs));
        if CandSelfPtr <> Cand then Exit;
      end;
      if not ReadClassNameFromVMT(Cand, Name) then Exit;
      if Length(Name) = 0 then Exit;
      AParentVMT := Cand;
      AParentName := Name;
      Result := True;
    end;

  var
    VMTPtr     : UIntPtr;
    ParentVMT  : UIntPtr;
    ParentName : String;
    Depth      : Integer;
  begin
    Result := False;
    AMember := Default(TRsmClassMember);
    if AObjPtr = 0 then Exit;
    if not Assigned(FLocalsReader) then Exit;
    VMTPtr := ReadTargetPointer(Pointer(AObjPtr));
    if VMTPtr = 0 then Exit;
    Depth := 0;
    while (VMTPtr <> 0) and (Depth < MaxDepth) do
    begin
      if not TryReadParentVMT(VMTPtr, ParentVMT, ParentName) then
        Exit; // TObject reached or unresolvable
      if FLocalsReader.FindClassMember(ParentName, AFieldName, AMember) then
        Exit(True);
      VMTPtr := ParentVMT;
      Inc(Depth);
    end;
  end;

var
  Segments       : TArray<String>;
  Locals         : TArray<TLocalVar>;
  Addr           : Pointer;
  FieldAddr      : Pointer;
  I              : Integer;
  IsLocal        : Boolean;
  PtrVal         : UIntPtr;
  IntVal         : Integer;
  Int64Val       : Int64;
  VMTPtr         : UIntPtr;
  ClassNamePtr   : UIntPtr;
  VMTClassNameOfs: Integer;
  LenByte        : Byte;
  RawBytes       : TBytes;
  ShortBuf       : TBytes;
  ObjPtr         : UIntPtr;
  ClsName        : String;
  Member         : TRsmClassMember;
  /// Byte width of the final resolved record/class field, propagated
  /// from the dotted-walk into the int / int64 interpretation so a
  /// 1-byte Boolean or 2-byte Word doesn't get reported as a 4-byte
  /// integer that visibly carries the next field's bytes in the high
  /// part. Stays 0 for non-dotted lookups (direct global / local
  /// access) and for variant-record overlay siblings whose size can't
  /// be derived from the offset chain alone -- both cases fall back
  /// to the user-requested type's width.
  FieldKnownSize : Integer;
begin
  Result := False;
  AValue := '';
  FieldKnownSize := 0;
  Phase('begin');

  if FLastThreadHit = 0 then Exit;

  Addr := nil;
  IsLocal := False;
  RawBytes := nil;

  // Resolve the variable. Two paths apply, tried in order:
  // 1. Treat the WHOLE name as a single identifier (matches a local, or
  //    a global with optional Unit.VarName qualification). This keeps
  //    the existing read_global_variable / single-local behavior intact.
  // 2. If that fails AND the name contains a dot, treat it as a dotted
  //    field chain: first segment is a local or global object reference,
  //    subsequent segments are field names looked up via TD32 class info.
  Locals := GetLocals(FLastThreadHit);
  Phase('after GetLocals');
  for I := 0 to High(Locals) do
  begin
    if SameText(Locals[I].Name, AName) then
    begin
      IsLocal := True;
      RawBytes := Locals[I].RawBytes;
      Break;
    end;
  end;
  Phase('after match-local');

  if not IsLocal then
  begin
    Addr := GetAddressFromSymbol(AName);
    Phase('after GetAddressFromSymbol');
    if Assigned(Addr) then
      RawBytes := ReadProcessMemory(Addr, 8);
    Phase('after global read');
  end;

  if (Length(RawBytes) = 0) and AName.Contains('.') then
  begin
    Phase('begin dotted-walk');
    // Path 2: dotted field-chain walk. The first segment must resolve
    // to an instance pointer (a local or global object reference); each
    // subsequent segment dereferences the current object, looks up the
    // named field via TD32 class info, and advances to that field's
    // address. The final segment's address is then read with the
    // user-specified type. Requires TD32 class layout (LoadDebugInfoFromExe).
    Segments := AName.Split(['.']);
    var FirstHopHasInstancePtr: Boolean := False;
    var FirstHopInstancePtr   : UIntPtr := 0;
    if (Length(Segments) >= 2) and Assigned(FLocalsReader) then
    begin
      // Step 1: locate first segment.
      //
      // Three source kinds need different handling for the first hop:
      //
      //   * Register-passed local (Self, register params): the CPU
      //     register's value IS the instance pointer; there is no
      //     stack slot to dereference. GetLocals already pulled the
      //     register value into <Locals[].RawBytes> via
      //     RegisterParamBytes. We surface that value as
      //     FirstHopInstancePtr and tell the class-hop branch below
      //     to skip the initial ReadTargetPointer.
      //
      //   * BP-relative local: BpOffset is the stack-slot address;
      //     the slot's contents are the instance pointer. Same
      //     storage shape as a global, so we keep the existing
      //     "FieldAddr = slot address, deref on first class-hop"
      //     path by handing back the slot address via Addr.
      //
      //   * Global: the symbol's VA points at the slot. Same as
      //     BPREL above. Also covers record-typed globals, which
      //     the priming block below detects and routes through the
      //     inline-record-hop path (no deref).
      for I := 0 to High(Locals) do
        if SameText(Locals[I].Name, Segments[0]) then
        begin
          if (Locals[I].Kind = lkRegister) and
             (Length(Locals[I].RawBytes) >= FTargetPointerSize) then
          begin
            Move(Locals[I].RawBytes[0], FirstHopInstancePtr, FTargetPointerSize);
            FirstHopHasInstancePtr := True;
            // Hand the walker a non-nil Addr sentinel so the
            // downstream "if not Assigned(Addr) then Exit" guard
            // doesn't reject this case. The pointer value itself
            // is consumed via FirstHopInstancePtr; Addr is only
            // used to seed FieldAddr, which the first class-hop
            // iteration overrides immediately.
            Addr := Pointer(FirstHopInstancePtr);
          end;
          Break;
        end;
      if not Assigned(Addr) then
        if not GetLocalAddress(FLastThreadHit, Segments[0], Addr) then
          Addr := GetAddressFromSymbol(Segments[0]);
      if not Assigned(Addr) then Exit;

      // Step 2: walk the dotted segments. The state we carry between
      // iterations is FieldAddr (where the next field's bytes will
      // be read) and PrevContextIdx (the index in FLocalsReader's
      // class/record list of the type we just resolved a member on,
      // or -1 if we don't know).
      //
      // Two hop kinds:
      //   * Class hop (default): FieldAddr holds an instance pointer.
      //     We deref, read the VMT class name, find the named member,
      //     and advance FieldAddr by Member.Offset.
      //   * Record hop (when the previous member resolved to a record
      //     type): FieldAddr already points inline within the parent
      //     record, no deref. We look up the named member in the
      //     record's member list and advance FieldAddr by its offset.
      //
      // The RSM reader populates Member.TypeIdx from the rich Format-A
      // field records (linked via the type registry), so transitions
      // between class-hop and record-hop happen via direct type-id
      // lookup rather than the older size+name heuristic.
      FieldAddr := Addr;
      var PrevContextIdx: Integer := -1;
      var ContextIsRecord : Boolean := False;
      // Prime the walk for record-typed globals. Without this, the
      // first hop unconditionally tries class semantics on
      // Segments[0] -- it reads the record's first bytes as if they
      // were a VMT pointer, fails the VMT-class-name read, and
      // returns "Failed to evaluate" even for a fully-resolved
      // global like GGlobalMixed.FMixedInt. The RSM reader exposes
      // the global's declared TypeIdx via FindGlobalTypeIdx; when
      // that id resolves to a record entry in FClasses, the first
      // hop is an inline record-hop (no deref).
      var GStructIdx: Integer := -1;
      var GTypeIdx: UInt32 := FLocalsReader.FindGlobalTypeIdx(Segments[0]);
      if GTypeIdx <> 0 then
        // The global's encoded type id is the RSM 2-byte form, not
        // the file-offset-based Classes[].TypeIdx token -- resolve
        // through the registry-backed map. This is the direct
        // route and works whenever both the global record and the
        // registry record use the same id space (DebugTarget, and
        // TFW impl-scope globals like MdtGlobal).
        GStructIdx := FLocalsReader.FindClassIdxByRsmTypeId(GTypeIdx);
      if GStructIdx < 0 then
        // Fallback for real-world binaries (TFW interface-scope and
        // impl-scope globals both hit this), where the global's
        // 2-byte type id at +3/+4 of the $20 record lives in an id
        // space we haven't bridged to the registry. The resolver
        // combines two structural signals that DON'T depend on the
        // encoded id: a "T<globalName>" naming-convention hint, and
        // proximity in the RSM byte stream between the global's $20
        // record and each record-type definition that carries the
        // named field. Either signal alone disambiguates the common
        // case; both together cover all five TFW repro variables
        // (AppCaps, MdtGlobal, GlobalKonsAplPortal and qualified
        // forms) without false positives on fields like Id / Name
        // that many records share.
        GStructIdx := FLocalsReader.FindBestRecordForGlobalAndField(
          Segments[0], Segments[1]);
      if (GStructIdx >= 0) and
         (FLocalsReader.Classes[GStructIdx].Kind = skRecord) then
      begin
        PrevContextIdx := GStructIdx;
        ContextIsRecord := True;
      end;
      for I := 1 to High(Segments) do
      begin
        if ContextIsRecord and (PrevContextIdx >= 0) then
        begin
          var Resolved: Boolean := False;
          for var Mi: Integer := 0 to FLocalsReader.Classes[PrevContextIdx].Members.Count - 1 do
            if SameText(FLocalsReader.Classes[PrevContextIdx].Members[Mi].Name, Segments[I]) then
            begin
              Member := FLocalsReader.Classes[PrevContextIdx].Members[Mi];
              FieldAddr := Pointer(NativeUInt(FieldAddr) + Member.Offset);
              Resolved := True;
              Break;
            end;
          if not Resolved then Exit;
        end
        else
        begin
          // First class-hop on a register-passed local: the instance
          // pointer is already in hand (the register value). Skip
          // the deref -- there is no slot to read from. Every later
          // class-hop runs the normal path because by then FieldAddr
          // points inside a heap object at a class-typed field slot.
          if (I = 1) and FirstHopHasInstancePtr then
            ObjPtr := FirstHopInstancePtr
          else
            ObjPtr := ReadTargetPointer(FieldAddr);
          if ObjPtr = 0 then
          begin
            if I = High(Segments) then
            begin
              AValue := 'nil';
              Result := True;
            end;
            Exit;
          end;
          if not ReadRuntimeClassName(ObjPtr, ClsName) then Exit;
          if not FLocalsReader.FindClassMember(ClsName, Segments[I], Member) then
          begin
            // The runtime class doesn't declare this field and the
            // RSM-derived ParentName chain didn't find it either --
            // either an intermediate ancestor has no own fields the
            // RSM scanner could lock onto, or the heuristic mis-paired
            // the chain. Walk the LIVE VMT instead: each parent VMT's
            // class name comes straight from the runtime, and a hit
            // on any ancestor in FClasses yields the correct field
            // offset (single-inheritance Delphi keeps field offsets
            // stable across the entire descent of the declaring class).
            if not FindFieldViaVmtWalk(ObjPtr, Segments[I], Member) then
              Exit;
          end;
          FieldAddr := Pointer(ObjPtr + Member.Offset);
        end;

        // Determine the next iteration's context purely from the
        // resolved member's TypeIdx. With the RSM reader's
        // Format-A type-id linking, structured-typed fields carry
        // a real TypeIdx that resolves to a record (record-hop next)
        // or a class (class-hop next); built-in-typed terminal
        // fields have TypeIdx = 0 and the loop just exits with
        // FieldAddr pointing at their bytes.
        if (Member.TypeIdx <> 0) and
           (FLocalsReader.FindStructByTypeIdx(Member.TypeIdx) >= 0) then
        begin
          PrevContextIdx := FLocalsReader.FindStructByTypeIdx(Member.TypeIdx);
          ContextIsRecord :=
            FLocalsReader.Classes[PrevContextIdx].Kind = skRecord;
        end
        else
        begin
          PrevContextIdx := -1;
          ContextIsRecord := False;
        end;
      end;

      // FieldAddr now points at the user-specified-type slot in the live
      // process. The downstream type-interpretation reads 8 bytes from
      // here for fixed-width types; ShortString re-reads 256 bytes via
      // its own dedicated path which uses Addr.
      Addr := FieldAddr;
      RawBytes := ReadProcessMemory(FieldAddr, 8);
      // Hand the field's declared width down to the int / int64
      // formatter. Member is the LAST resolved member at this point
      // (the terminal segment of the dotted path); its Size was
      // derived at parse time from the gap to the next field's
      // offset. A non-zero Size means we KNOW the field is narrower
      // than the user's requested type, and the read should be
      // clamped to that width instead of carrying neighbour bytes
      // into the high part of the result.
      FieldKnownSize := Integer(Member.Size);
    end;
    Phase('end dotted-walk');
  end;

  if Length(RawBytes) = 0 then Exit;
  Phase('format ' + AType);

  if SameText(AType, 'int') then
  begin
    // Read exactly FieldKnownSize bytes when we know the field is
    // narrower than 4 (Boolean = 1, Word / SmallInt = 2). Zero-extend
    // to 32 bits so a Word like TMdt.Id at offset 20 returns its
    // actual value (1) instead of getting concatenated with the next
    // field's bytes (1 | (1 shl 16) = 65537). Sign-extending signed
    // narrow types (SmallInt, ShortInt) would need the type's
    // signedness from RSM, which we don't track for primitives -- the
    // common case (unsigned Word / Byte / Boolean) is what users hit
    // in practice, so default to zero-extend.
    var ReadSize: Integer := 4;
    if (FieldKnownSize > 0) and (FieldKnownSize < 4) then
      ReadSize := FieldKnownSize;
    if Length(RawBytes) >= ReadSize then
    begin
      IntVal := 0;
      Move(RawBytes[0], IntVal, ReadSize);
      AValue := IntToStr(IntVal);
      Result := True;
    end;
  end
  else if SameText(AType, 'int64') then
  begin
    // Same clamp story as 'int', extended to 8-byte targets: a
    // Cardinal field (size 4) reported via type=int64 should read
    // 4 bytes and zero-extend to 64, not pull 8 and concatenate.
    var ReadSize: Integer := 8;
    if (FieldKnownSize > 0) and (FieldKnownSize < 8) then
      ReadSize := FieldKnownSize;
    if Length(RawBytes) >= ReadSize then
    begin
      Int64Val := 0;
      Move(RawBytes[0], Int64Val, ReadSize);
      AValue := IntToStr(Int64Val);
      Result := True;
    end;
  end
  else if SameText(AType, 'string') then
  begin
    if Length(RawBytes) >= FTargetPointerSize then
    begin
      PtrVal := 0;
      Move(RawBytes[0], PtrVal, FTargetPointerSize);
      Result := ReadLongString(PtrVal, 2, TEncoding.Unicode, False, AValue);
    end;
  end
  else if SameText(AType, 'ansistring') then
  begin
    // AnsiString: pointer-to-byte data, 4-byte length at -4 in CHARACTERS,
    // codepage at -12 (since D2009). Decoding uses the system ANSI
    // codepage; for non-ASCII content the codepage at -12 would give a
    // more accurate decode but ASCII content is identical either way.
    if Length(RawBytes) >= FTargetPointerSize then
    begin
      PtrVal := 0;
      Move(RawBytes[0], PtrVal, FTargetPointerSize);
      Result := ReadLongString(PtrVal, 1, TEncoding.ANSI, False, AValue);
    end;
  end
  else if SameText(AType, 'widestring') then
  begin
    // WideString is BSTR-compatible: 4-byte length at -4 in BYTES (not
    // chars), data is UTF-16. Pointer of size FTargetPointerSize.
    if Length(RawBytes) >= FTargetPointerSize then
    begin
      PtrVal := 0;
      Move(RawBytes[0], PtrVal, FTargetPointerSize);
      Result := ReadLongString(PtrVal, 2, TEncoding.Unicode, True, AValue);
    end;
  end
  else if SameText(AType, 'shortstring') then
  begin
    // ShortString is INLINE: the variable IS a length-prefixed buffer
    // (max 256 bytes), not a pointer. GetLocals only returns the first
    // 8 bytes per slot which truncates strings longer than 7 chars,
    // so for locals we resolve the runtime address explicitly via
    // GetLocalAddress and read up to 256 bytes from there. For globals
    // the same approach applies against Addr.
    var ShortAddr: Pointer := nil;
    if IsLocal then
      GetLocalAddress(FLastThreadHit, AName, ShortAddr)
    else
      ShortAddr := Addr;

    if Assigned(ShortAddr) then
    begin
      ShortBuf := ReadProcessMemory(ShortAddr, 256);
      if Length(ShortBuf) >= 1 then
      begin
        LenByte := ShortBuf[0];
        if Integer(LenByte) + 1 <= Length(ShortBuf) then
        begin
          if LenByte = 0 then
            AValue := ''
          else
            AValue := TEncoding.ANSI.GetString(ShortBuf, 1, LenByte);
          Result := True;
        end;
      end;
    end;
  end
  else if SameText(AType, 'single') then
  begin
    // IEEE 754 single-precision (4 bytes). The RawBytes slot is at
    // least 8 bytes; we consume the first 4 and ignore the rest.
    if Length(RawBytes) >= 4 then
    begin
      var SingleVal: Single := 0.0;
      Move(RawBytes[0], SingleVal, 4);
      AValue := FloatToStrF(SingleVal, ffGeneral, 7, 0,
        TFormatSettings.Invariant);
      Result := True;
    end;
  end
  else if SameText(AType, 'double') then
  begin
    // IEEE 754 double-precision (8 bytes). RawBytes is sized at 8.
    if Length(RawBytes) >= 8 then
    begin
      var DoubleVal: Double := 0.0;
      Move(RawBytes[0], DoubleVal, 8);
      AValue := FloatToStrF(DoubleVal, ffGeneral, 15, 0,
        TFormatSettings.Invariant);
      Result := True;
    end;
  end
  else if SameText(AType, 'extended') then
  begin
    // Delphi Extended is 80-bit (10 bytes) on both Win32 and Win64
    // by default (EXCESSPRECISION ON is the dcc64 default). We
    // re-read 10 bytes from Addr -- FieldAddr for dotted access,
    // the global-symbol address for top-level globals -- and decode
    // the 80-bit format manually into a Double for printing. The
    // manual decode avoids depending on the host DPT.exe's own
    // SizeOf(Extended), which can be 8 on builds with
    // EXCESSPRECISION OFF.
    if Assigned(Addr) then
    begin
      var ExtBuf: TBytes := ReadProcessMemory(Addr, 10);
      if Length(ExtBuf) = 10 then
      begin
        // 80-bit Extended layout (LE):
        //   bytes 0..7 = 64-bit mantissa with EXPLICIT leading 1 bit
        //   bytes 8..9 = 1-bit sign + 15-bit biased exponent (bias $3FFF)
        var Mantissa  : UInt64;
        var SignExp   : UInt16;
        Move(ExtBuf[0], Mantissa, 8);
        Move(ExtBuf[8], SignExp, 2);
        var Sign      : UInt64 := (SignExp shr 15) and 1;
        var Exp80     : Integer := SignExp and $7FFF;
        var DoubleBits: UInt64;
        if (Exp80 = 0) and (Mantissa = 0) then
          DoubleBits := Sign shl 63       // +/- zero
        else if Exp80 = $7FFF then
          // Inf / NaN: forward to Double Inf / NaN.
          DoubleBits := (Sign shl 63) or
                        (UInt64($7FF) shl 52) or
                        (UInt64(Ord(Mantissa <> $8000000000000000)) shl 51)
        else
        begin
          // Normalized: rebias exponent (80-bit bias $3FFF -> Double
          // bias $3FF) and drop the explicit leading-1 bit by
          // shifting the mantissa right by 11.
          var ExpD: Integer := Exp80 - $3FFF + $3FF;
          if ExpD <= 0 then
            DoubleBits := Sign shl 63          // underflow -> +/- zero
          else if ExpD >= $7FF then
            DoubleBits := (Sign shl 63) or
                          (UInt64($7FF) shl 52)  // overflow -> +/- Inf
          else
            DoubleBits := (Sign shl 63) or
                          (UInt64(ExpD) shl 52) or
                          ((Mantissa and $7FFFFFFFFFFFFFFF) shr 11);
        end;
        var DoubleVal: Double := 0.0;
        Move(DoubleBits, DoubleVal, 8);
        AValue := FloatToStrF(DoubleVal, ffGeneral, 18, 0,
          TFormatSettings.Invariant);
        Result := True;
      end;
    end;
  end
  else if SameText(AType, 'object') then
  begin
    if Length(RawBytes) >= FTargetPointerSize then
    begin
      PtrVal := 0;
      Move(RawBytes[0], PtrVal, FTargetPointerSize);
      if PtrVal = 0 then
      begin
        AValue := 'nil';
        Result := True;
      end
      else
      begin
        VMTPtr := ReadTargetPointer(Pointer(PtrVal));
        if VMTPtr <> 0 then
        begin
          if FTargetIs32Bit then VMTClassNameOfs := 56 else VMTClassNameOfs := 136;
          ClassNamePtr := ReadTargetPointer(Pointer(VMTPtr - UIntPtr(VMTClassNameOfs)));
          if ClassNamePtr <> 0 then
          begin
            var Buf := ReadProcessMemory(Pointer(ClassNamePtr), 256);
            if Length(Buf) > 1 then
            begin
              LenByte := Buf[0];
              if (LenByte > 0) and (1 + Integer(LenByte) <= Length(Buf)) then
              begin
                AValue := TEncoding.ANSI.GetString(Buf, 1, LenByte) + ' @ ' + Format('%.*x', [FTargetPointerSize * 2, PtrVal]);
                Result := True;
              end;
            end;
          end;
        end;
        if not Result then
        begin
          AValue := 'Object @ ' + Format('%.*x', [FTargetPointerSize * 2, PtrVal]);
          Result := True;
        end;
      end;
    end;
  end;
end;

end.
