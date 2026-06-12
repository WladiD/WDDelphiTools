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
  DPT.Rsm.Model,
  DPT.Rsm.Reader;

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
    ///   2-byte RSM type-id of the local's declared type, copied
    ///   from <c>TRsmLocal.TypeIdx</c> on the way out of
    ///   <c>GetLocals</c>. Used by <c>EvaluateVariable</c>'s
    ///   auto-detection path: when the caller omits the
    ///   <c>type</c> argument, this id is looked up against
    ///   <c>FPrimitiveTypeFormatters</c> (primitives like Integer,
    ///   string, Double, ...) and against the class registry
    ///   (instance-pointer locals).
    /// </summary>
    TypeIdx : UInt32;
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

  /// <summary>
  ///   Uniform contract for the per-type evaluate-output formatters.
  ///   Each formatter consumes the bytes the dotted walk (or whole-
  ///   name lookup) already fetched, plus the resolved field address
  ///   (used by formatters that need more than 8 bytes -- ShortString
  ///   re-reads up to 256, Extended re-reads 10). Returns True with
  ///   <c>AValue</c> set to the printable representation; False with
  ///   <c>AValue</c> empty to surface "Failed to evaluate variable".
  /// </summary>
  TEvaluateFormatter = function(const ARawBytes: TBytes; AAddr: Pointer;
    AFieldKnownSize: Integer; out AValue: String): Boolean of object;

  TDebugger = class
  private
    FActiveThreads           : IKeyValue<Cardinal, THandle>;
    /// Lowercased-type-name -> output formatter. Initialised once in
    /// Create with the built-in set ("int", "int64", "string",
    /// "ansistring", "widestring", "shortstring", "single", "double",
    /// "extended", "object"). Adding a new type means: one new
    /// FormatXxx method plus one registration line in Create. The
    /// dispatch in EvaluateVariable becomes a single TryGetValue.
    FFormatters              : IKeyValue<String, TEvaluateFormatter>;
    /// Well-known Delphi-compiler primitive type id -> formatter
    /// name. The Delphi compiler emits a fixed set of 2-byte ids
    /// for its built-in primitives (Integer, Word, Byte, Int64,
    /// string, AnsiString, WideString, Boolean, Single, Double,
    /// Extended, Currency, ...) inside the $2C field record's
    /// "$9C $09" reference block. <c>EvaluateVariable</c> consults
    /// this map when the caller omits the <c>type</c> argument and
    /// the resolved Member has a non-zero PrimitiveTypeId, so an
    /// auto-detected <c>evaluate Self.FName</c> picks the right
    /// formatter without the caller knowing the Delphi declaration.
    FPrimitiveTypeFormatters : IKeyValue<UInt16, String>;
    /// Per-evaluate-call scratch: the RSM 2-byte type id the
    /// auto-detection layer identified as an enum. <c>FormatEnum</c>
    /// reads this to resolve the raw byte value to its constant
    /// name (and stay within the uniform formatter contract that
    /// otherwise carries no type-id context).
    FLastEnumTypeId          : UInt32;
    FBaseAddress             : UIntPtr;
    FBreakpointHitEvent      : TEvent;
    FBreakpointLock          : TCriticalSection;
    FBreakpoints             : IList<TBreakpoint>;
    FContinueEvent           : TEvent;
    FFinishedEvent           : TEvent;
    FFirstBreak              : Boolean;
    FIgnoredExceptions       : IList<String>;
    FIgnoredExceptionCodes   : IList<Cardinal>;
    FExceptionLock           : TCriticalSection;
    FBreakOnNativeFirstChance: Boolean;
    FLastBreakpointHit       : TBreakpoint;
    FLastException           : TExceptionRecord;
    FLastExceptionClassName  : String;
    FLastExceptionFirstChance: Boolean;
    FLastPauseWasException   : Boolean;
    FLastThreadHit           : THandle;
    FLastThreadId            : Cardinal;
    FLocalsReader            : TRsmReader;
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
    /// Picks a formatter name for an <c>evaluate</c> call whose
    /// <c>type</c> argument was omitted. The resolution order is
    /// dotted-terminal primitive id -> dotted-terminal class id ->
    /// matched local's TypeIdx (primitive or class) -> whole-name
    /// global TypeIdx (primitive or class). Returns an empty string
    /// when nothing matches; the caller then either falls through
    /// to a record-terminal hint (when the value points at an
    /// inline record) or surfaces "Failed to evaluate variable".
    function  AutoDetectFormatterName(const AName: String; AIsLocal: Boolean;
      AMatchedLocalTypeIdx: UInt32;
      const AMember: TRsmClassMember): String;
    /// Returns True (with the record's type name) when <c>AName</c>
    /// is a record-typed local or global. Used by EvaluateVariable's
    /// record-terminal fallback so a whole-name evaluate of an inline
    /// record surfaces a navigable hint instead of an opaque failure.
    function  TryGetRecordTerminalName(const AName: String; AIsLocal: Boolean;
      AMatchedLocalTypeIdx: UInt32; out ARecTypeName: String): Boolean;
    /// Returns True (with the formatted <c>"ClassName @ HexAddr"</c>)
    /// when the raw bytes' leading pointer dereferences to a valid
    /// VMT. Used by EvaluateVariable's class-pointer probe fallback
    /// to auto-detect cross-unit / RTL class fields that Format-A
    /// linking didn't populate with a TypeIdx.
    function  TryProbeClassPointer(const ARawBytes: TBytes;
      out AFormatted: String): Boolean;
    /// Last-resort enum auto-detect: F<X> field name -> T<X> enum
    /// type, only when the raw bytes look like a small ordinal
    /// (high bytes zero). Bypasses the formatter dispatch and
    /// returns the pre-rendered <c>"name (ord)"</c> string.
    function  TryNameBasedEnumLookup(const AName: String;
      const ARawBytes: TBytes; out AFormatted: String): Boolean;
    /// <summary>
    ///   Derives the most likely Delphi enum-type name from a
    ///   variable identifier. Strips a leading 'G' or 'F' prefix
    ///   when followed by an uppercase letter (Delphi global /
    ///   field conventions), strips the TRAILING camelCase word
    ///   (the unit-scope suffix used to disambiguate same-name
    ///   types across sibling units, e.g. "GStatusAlpha" -> trim
    ///   "Alpha" -> "Status"), then prepends "T". Returns the
    ///   empty string when no plausible type name can be derived.
    /// </summary>
    function  DeriveTypeHintFromVariableName(const AName: String): String;
    /// Builds the hint message returned via <c>EvaluateVariable</c>'s
    /// <c>AHint</c> out parameter when auto-detection found nothing.
    /// The MCP layer surfaces this so the caller learns WHICH
    /// explicit <c>type=</c> arg to retry with.
    function  BuildAutoDetectHint(const AName: String;
      const ARawBytes: TBytes): String;
    /// Shared between FormatString / FormatAnsiString / FormatWideString.
    /// Reads a Delphi long string given the pointer payload stored at
    /// the field: 0 means empty/nil, otherwise points at the character
    /// data with a 4-byte length field at offset -4. <c>ALengthIsBytes</c>
    /// = True for WideString (BSTR), False for UnicodeString /
    /// AnsiString (length is in characters).
    function  ReadLongString(APtrVal: UIntPtr; ACharSize: Integer;
      AEncoding: TEncoding; ALengthIsBytes: Boolean; out AOut: String): Boolean;
    /// Read the class name pointed at by <c>AVMTPtr - vmtClassName</c>.
    /// Used by the dotted-walk's VMT walk and by FormatObject.
    function  ReadClassNameFromVMT(AVMTPtr: UIntPtr; out AClassName: String): Boolean;
    /// Per-type evaluate output formatters. Their uniform contract is
    /// captured by <see cref="TEvaluateFormatter"/>; they are
    /// dispatched through <see cref="FFormatters"/>.
    function  FormatInt        (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatInt64      (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatString     (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatAnsiString (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatWideString (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatShortString(const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatSingle     (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatDouble     (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatExtended   (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatBool       (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatCurrency   (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatObject     (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
    function  FormatEnum       (const ARawBytes: TBytes; AAddr: Pointer; AFieldKnownSize: Integer; out AValue: String): Boolean;
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
    /// <summary>
    ///   Recovers the frame home slot of a register-passed parameter by
    ///   reading the procedure prologue. Delphi receives <c>Self</c> and
    ///   the leading scalar parameters in registers (EAX/EDX/ECX on
    ///   Win32; RCX/RDX/R8/R9 on Win64) but, when the method has a stack
    ///   frame, immediately spills each to a home slot and reloads it
    ///   from there for every later access. The RSM only records the
    ///   register slot (no frame offset), so deep in the body the live
    ///   register is stale -- the home slot is the authoritative copy.
    ///   Scans the prologue for the spill store of the register that
    ///   carries <c>ARegParamIdx</c> and, on a match, returns its signed
    ///   displacement from the frame pointer (EBP/RBP) in
    ///   <c>ADispFromFramePtr</c>. Returns <c>False</c> when the
    ///   parameter is kept in its register (no spill emitted) or the
    ///   index has no register slot, in which case the caller falls back
    ///   to the live register value.
    /// </summary>
    function  TryFindRegParamSpillDisp(AProcStart: Pointer; ARegParamIdx: Byte;
      out ADispFromFramePtr: Integer): Boolean;
    /// <summary>
    ///   §6.35: tells whether an x86 procedure establishes a classic
    ///   <c>push ebp; mov ebp,esp</c> frame (locals anchored to EBP) or
    ///   is FRAMELESS -- the optimiser (STACKFRAMES OFF) form that saves
    ///   EBP as a callee-saved scratch register and addresses its locals
    ///   off ESP. The TD32 frame-relative offsets are anchored to ESP in
    ///   the frameless case, so <c>GetLocals</c>/<c>GetLocalAddress</c>
    ///   must read them off ESP, not the (now garbage) EBP. Returns
    ///   <c>True</c> (the historical EBP assumption) for x64 and whenever
    ///   the prologue is ambiguous, so a real EBP frame is never rebased.
    /// </summary>
    function  ProcUsesEbpFrame(AProcStart: Pointer): Boolean;
    /// <summary>
    ///   §6.35 reg→reg residual: in a FRAMELESS x86 proc a register
    ///   parameter is spilled into a callee-saved register (EBX/ESI/EDI)
    ///   rather than a frame slot — e.g. `mov esi,eax` (Self → ESI),
    ///   `mov ebx,edx` (2nd param → EBX). Scans the prologue's register-
    ///   move run for the spill of <c>ARegParamIdx</c>'s inbound register
    ///   and, on a match, returns the live value of the destination
    ///   register (8 LE bytes) from <c>ARegs</c>. Returns <c>False</c>
    ///   (x64, or no reg→reg spill found) so the caller falls back to the
    ///   memory-spill / live-inbound-register paths.
    /// </summary>
    function  TryFindRegParamRegSpill(AProcStart: Pointer; ARegParamIdx: Byte;
      const ARegs: TRegisters; out ABytes: TBytes): Boolean;
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
    ///   <c>False</c> when the PC is outside any RSM-covered procedure
    ///   or no local with the given name exists. Use this to read more
    ///   bytes than <see cref="GetLocals"/>'s fixed 8-byte slice would
    ///   provide (e.g. for inline value types like ShortString).
    /// </summary>
    function  GetLocalAddress(AThreadHandle: THandle; const AName: String;
      out AAddress: Pointer): Boolean;
    function  EvaluateVariable(const AName: String; var AType: String; out AValue, AHint: String;
      AOnPhase: TProc<String> = nil): Boolean;
    function  GetStackFrameInfo(AThreadHandle: THandle): TStackFrameInfo;
    function  GetStackSlots(AThreadHandle: THandle; AMaxSlots: Integer = 20): TArray<TStackSlot>;
    function  GetStackTrace(AThreadHandle: THandle): TArray<TStackFrame>;
    function  GetThreadIds: TArray<Cardinal>;
    procedure IgnoreException(const AClassName: String);
    procedure IgnoreExceptionCode(ACode: Cardinal);
    procedure UnignoreExceptionCode(ACode: Cardinal);
    function  IsExceptionIgnored(const AClassName: String): Boolean;
    function  IsExceptionCodeIgnored(ACode: Cardinal): Boolean;
    function  GetIgnoredExceptions: TArray<String>;
    function  GetIgnoredExceptionCodes: TArray<Cardinal>;
    /// <summary>
    ///   Loads the <c>.rsm</c> Remote Debug Symbols sidecar file
    ///   produced by the Delphi linker for the given executable
    ///   (built with linker options <c>-V -VR</c>, the latter
    ///   being "Include remote debug symbols"). The sidecar lives
    ///   next to the .exe, NOT embedded inside it. Required for
    ///   <see cref="GetLocals"/> and the dotted-field walk in
    ///   <see cref="EvaluateVariable"/>; safe to skip when only
    ///   map-file based functionality is needed.
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
    property  BreakOnNativeFirstChance: Boolean read FBreakOnNativeFirstChance write FBreakOnNativeFirstChance;
    property  LastException: TExceptionRecord read FLastException;
    property  LastExceptionClassName: String read FLastExceptionClassName;
    property  LastExceptionFirstChance: Boolean read FLastExceptionFirstChance;
    property  LastPauseWasException: Boolean read FLastPauseWasException;
    property  LastThreadHit: THandle read FLastThreadHit;
    property  LastThreadId: Cardinal read FLastThreadId;
    property  LocalsReader: TRsmReader read FLocalsReader;
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
  FExceptionLock := TCriticalSection.Create;
  FIgnoredExceptions := Collections.NewList<String>;
  FIgnoredExceptions.Add('EAbort');
  FIgnoredExceptionCodes := Collections.NewList<Cardinal>;
  // Default OFF. This is NOT a claim that native (non-Delphi) exceptions
  // are harmless -- an access violation (C0000005) is a real fault. It
  // is about FIRST chance vs SECOND chance: the OS, the RTL, third-party
  // DLLs and the app itself routinely raise native exceptions that their
  // own SEH handlers immediately catch (and some environments inject
  // extra startup ones, e.g. a C++ EH exception, code E06D7363). Breaking
  // on EVERY first-chance native exception therefore stops constantly on
  // exceptions that are handled and never become a crash. SECOND-chance
  // (i.e. unhandled) native faults -- the actual crashes -- still surface
  // regardless of this flag. Set BreakOnNativeFirstChance := True to also
  // stop at first chance (e.g. to catch an AV at the instruction that
  // raises it, before the app's handler runs).
  FBreakOnNativeFirstChance := False;
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

  // Per-type evaluate output dispatch table. Add a new type by
  // implementing one FormatXxx method matching TEvaluateFormatter
  // and registering it here -- nothing else needs to change.
  FFormatters := Collections.NewPlainKeyValue<String, TEvaluateFormatter>;
  FFormatters['int']         := FormatInt;
  FFormatters['int64']       := FormatInt64;
  FFormatters['string']      := FormatString;
  FFormatters['ansistring']  := FormatAnsiString;
  FFormatters['widestring']  := FormatWideString;
  FFormatters['shortstring'] := FormatShortString;
  FFormatters['single']      := FormatSingle;
  FFormatters['double']      := FormatDouble;
  FFormatters['extended']    := FormatExtended;
  FFormatters['bool']        := FormatBool;
  FFormatters['boolean']     := FormatBool;       // alias for the Delphi keyword
  FFormatters['currency']    := FormatCurrency;
  FFormatters['object']      := FormatObject;
  FFormatters['enum']        := FormatEnum;

  // Auto-detection: maps the Delphi compiler's built-in 2-byte
  // primitive type ids (captured by LinkFieldsFromFormatA into
  // Member.PrimitiveTypeId) to the formatter that handles them.
  // The ids are compiler-defined constants and stable across
  // binaries of the same Delphi version. Narrow integer types
  // (Byte / Word / SmallInt / ShortInt / Boolean) map to "int"
  // because the int formatter already width-clamps via Member.Size.
  FPrimitiveTypeFormatters := Collections.NewPlainKeyValue<UInt16, String>;
  FPrimitiveTypeFormatters[$03FD] := 'int';        // Integer / LongInt
  FPrimitiveTypeFormatters[$0411] := 'int';        // Byte
  FPrimitiveTypeFormatters[$0415] := 'int';        // Word
  FPrimitiveTypeFormatters[$0409] := 'int64';      // Int64
  FPrimitiveTypeFormatters[$0401] := 'string';     // UnicodeString
  FPrimitiveTypeFormatters[$0419] := 'single';     // Single
  FPrimitiveTypeFormatters[$041D] := 'double';     // Double
  FPrimitiveTypeFormatters[$0421] := 'extended';   // Extended
  FPrimitiveTypeFormatters[$0425] := 'bool';       // Boolean (body=15 form)
  FPrimitiveTypeFormatters[$0429] := 'currency';   // Currency (body=15 form)
  // Single-byte primitive ids. Three channels feed these, all sharing
  // the same compiler-built-in single-byte id space:
  //   * the LOCAL_TAG record's byte+3 slot (BPRel locals),
  //   * the $27 tag's byte+3 slot (top-level primitive globals),
  //   * §6.33: the body=9 managed-reference field record's After+3 slot
  //     (class/record string fields). Earlier this map keyed the
  //     managed primitives on the 2-byte values $001C/$081E/$100C read
  //     as "After+3 | After+4 shl 8", but After+4 is 2x the field
  //     offset, so those keys only matched DebugTarget's FAnsi@0 /
  //     FWide@4 / FShort@8 and missed the same types at any other
  //     offset. The linker now stores After+3 alone, so the single-byte
  //     ids below cover managed string fields at every offset.
  // This id space is independent of (and does not overlap) the 2-byte
  // range used by structured types and the body=14 UnicodeString id
  // $0401 above.
  FPrimitiveTypeFormatters[$02] := 'int';          // Integer / LongInt
  FPrimitiveTypeFormatters[$04] := 'string';       // UnicodeString
  FPrimitiveTypeFormatters[$08] := 'int64';        // Int64
  FPrimitiveTypeFormatters[$0A] := 'int';          // Cardinal / UInt32
  FPrimitiveTypeFormatters[$0C] := 'shortstring';  // ShortString
  FPrimitiveTypeFormatters[$1C] := 'ansistring';   // AnsiString
  FPrimitiveTypeFormatters[$1E] := 'widestring';   // WideString
end;

procedure TDebugger.IgnoreException(const AClassName: String);
begin
  // FIgnoredExceptions is read by the debug-event loop (HandleException,
  // on TDebuggerThread) and mutated here from the MCP request thread, so
  // every touch goes through FExceptionLock.
  FExceptionLock.Enter;
  try
    for var I: Integer := 0 to FIgnoredExceptions.Count - 1 do
      if SameText(FIgnoredExceptions[I], AClassName) then Exit;
    FIgnoredExceptions.Add(AClassName);
  finally
    FExceptionLock.Leave;
  end;
end;

procedure TDebugger.UnignoreException(const AClassName: String);
begin
  FExceptionLock.Enter;
  try
    for var I: Integer := FIgnoredExceptions.Count - 1 downto 0 do
      if SameText(FIgnoredExceptions[I], AClassName) then
        FIgnoredExceptions.Delete(I);
  finally
    FExceptionLock.Leave;
  end;
end;

procedure TDebugger.IgnoreExceptionCode(ACode: Cardinal);
begin
  FExceptionLock.Enter;
  try
    for var I: Integer := 0 to FIgnoredExceptionCodes.Count - 1 do
      if FIgnoredExceptionCodes[I] = ACode then Exit;
    FIgnoredExceptionCodes.Add(ACode);
  finally
    FExceptionLock.Leave;
  end;
end;

procedure TDebugger.UnignoreExceptionCode(ACode: Cardinal);
begin
  FExceptionLock.Enter;
  try
    for var I: Integer := FIgnoredExceptionCodes.Count - 1 downto 0 do
      if FIgnoredExceptionCodes[I] = ACode then
        FIgnoredExceptionCodes.Delete(I);
  finally
    FExceptionLock.Leave;
  end;
end;

function TDebugger.IsExceptionIgnored(const AClassName: String): Boolean;
begin
  Result := False;
  if AClassName = '' then Exit;
  FExceptionLock.Enter;
  try
    for var I: Integer := 0 to FIgnoredExceptions.Count - 1 do
      if SameText(FIgnoredExceptions[I], AClassName) then Exit(True);
  finally
    FExceptionLock.Leave;
  end;
end;

function TDebugger.IsExceptionCodeIgnored(ACode: Cardinal): Boolean;
begin
  Result := False;
  FExceptionLock.Enter;
  try
    for var I: Integer := 0 to FIgnoredExceptionCodes.Count - 1 do
      if FIgnoredExceptionCodes[I] = ACode then Exit(True);
  finally
    FExceptionLock.Leave;
  end;
end;

function TDebugger.GetIgnoredExceptions: TArray<String>;
begin
  FExceptionLock.Enter;
  try
    SetLength(Result, FIgnoredExceptions.Count);
    for var I: Integer := 0 to FIgnoredExceptions.Count - 1 do
      Result[I] := FIgnoredExceptions[I];
  finally
    FExceptionLock.Leave;
  end;
end;

function TDebugger.GetIgnoredExceptionCodes: TArray<Cardinal>;
begin
  FExceptionLock.Enter;
  try
    SetLength(Result, FIgnoredExceptionCodes.Count);
    for var I: Integer := 0 to FIgnoredExceptionCodes.Count - 1 do
      Result[I] := FIgnoredExceptionCodes[I];
  finally
    FExceptionLock.Leave;
  end;
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
  FExceptionLock.Free;
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

function TDebugger.TryFindRegParamSpillDisp(AProcStart: Pointer;
  ARegParamIdx: Byte; out ADispFromFramePtr: Integer): Boolean;
const
  // How far into the prologue to look for the spill store. Delphi emits
  // the spills immediately after the frame setup (verified: Win32 Self at
  // proc+4, Win64 Self at proc+8), so a small window suffices and keeps
  // us clear of body code that could contain coincidental byte runs.
  ScanWindow = 96;
var
  Buf  : TBytes;
  P    : Integer;
  Rex  : Byte;   // required REX prefix on x64 ($00 = none on x86)
  Op   : Byte;   // store opcode -- always $89 (MOV r/m, r) for these spills
  M8   : Byte;   // ModRM for the [framePtr + disp8]  form (mod=01, rm=EBP/RBP)
  M32  : Byte;   // ModRM for the [framePtr + disp32] form (mod=10, rm=EBP/RBP)
begin
  Result := False;
  ADispFromFramePtr := 0;

  // Map the register-parameter index to the spill store's opcode bytes.
  // The store is `mov [framePtr + disp], <reg>`; the ModRM reg field
  // selects the source register, the rm field (101) selects EBP/RBP, and
  // the mod field selects disp8 ($40-block) vs disp32 ($80-block).
  Op := $89;
  if FTargetIs32Bit then
  begin
    Rex := $00;
    case ARegParamIdx of
      0: begin M8 := $45; M32 := $85; end;  // EAX (Self)
      1: begin M8 := $55; M32 := $95; end;  // EDX
      2: begin M8 := $4D; M32 := $8D; end;  // ECX
    else
      Exit;  // 4th+ scalar param is stack-passed; no register spill
    end;
  end
  else
  begin
    // x64: RCX/RDX use REX.W ($48); R8/R9 add REX.R ($4C) which extends
    // the ModRM reg field, so R8 reuses EAX's reg encoding and R9 ECX's.
    case ARegParamIdx of
      0: begin Rex := $48; M8 := $4D; M32 := $8D; end;  // RCX (Self)
      1: begin Rex := $48; M8 := $55; M32 := $95; end;  // RDX
      2: begin Rex := $4C; M8 := $45; M32 := $85; end;  // R8
      3: begin Rex := $4C; M8 := $4D; M32 := $8D; end;  // R9
    else
      Exit;  // 5th+ scalar param is stack-passed; no register spill
    end;
  end;

  Buf := ReadProcessMemory(AProcStart, ScanWindow);
  if Length(Buf) < 4 then
    Exit;

  // Take the FIRST matching store: the prologue spill precedes any body
  // reload (which uses the MOV-load opcode $8B, not $89) and any later
  // store, so the earliest hit is the authoritative home slot.
  P := 0;
  while P < Length(Buf) - 3 do
  begin
    if FTargetIs32Bit then
    begin
      if Buf[P] = Op then
      begin
        if Buf[P + 1] = M8 then
        begin
          ADispFromFramePtr := ShortInt(Buf[P + 2]);
          Exit(True);
        end;
        if (Buf[P + 1] = M32) and (P + 6 <= Length(Buf)) then
        begin
          ADispFromFramePtr := PInteger(@Buf[P + 2])^;
          Exit(True);
        end;
      end;
    end
    else
    begin
      if (Buf[P] = Rex) and (Buf[P + 1] = Op) then
      begin
        if Buf[P + 2] = M8 then
        begin
          ADispFromFramePtr := ShortInt(Buf[P + 3]);
          Exit(True);
        end;
        if (Buf[P + 2] = M32) and (P + 7 <= Length(Buf)) then
        begin
          ADispFromFramePtr := PInteger(@Buf[P + 3])^;
          Exit(True);
        end;
      end;
    end;
    Inc(P);
  end;
end;

function TDebugger.ProcUsesEbpFrame(AProcStart: Pointer): Boolean;
var
  Buf: TBytes;
  P  : Integer;
begin
  // x86 Delphi establishes an EBP frame with `push ebp; mov ebp,esp`
  // (55 8B EC) right at the entry, before any other register pushes. The
  // FRAMELESS form (STACKFRAMES OFF / optimised) instead emits a run of
  // callee-saved register pushes (50..57, which may include `push ebp`
  // saving it merely as scratch) followed DIRECTLY by a stack allocation
  // with NO `mov ebp,esp`. So: skip the leading pushes, then a `mov
  // ebp,esp` means EBP frame, a bare stack-allocation means frameless.
  // Default True (EBP frame) for x64 and any ambiguous prologue, so a
  // genuine EBP frame is never mis-rebased onto ESP.
  Result := True;
  if not FTargetIs32Bit then
    Exit;
  Buf := ReadProcessMemory(AProcStart, 16);
  if Length(Buf) < 3 then
    Exit;
  P := 0;
  while (P < Length(Buf)) and (Buf[P] in [$50..$57]) do
    Inc(P);
  if P + 1 >= Length(Buf) then
    Exit;
  // mov ebp,esp == 8B EC (or 89 E5) -> genuine EBP frame.
  if ((Buf[P] = $8B) and (Buf[P + 1] = $EC)) or
     ((Buf[P] = $89) and (Buf[P + 1] = $E5)) then
    Exit(True);
  // A stack allocation with no preceding `mov ebp,esp` -> frameless:
  //   sub esp,imm8/imm32 (83 EC / 81 EC) or add esp,-imm8/-imm32
  //   (83 C4 <neg> / 81 C4 <neg>).
  if ((Buf[P] = $83) and (Buf[P + 1] = $EC)) or
     ((Buf[P] = $81) and (Buf[P + 1] = $EC)) then
    Exit(False);
  if (P + 2 < Length(Buf)) and (Buf[P] = $83) and (Buf[P + 1] = $C4) and
     (ShortInt(Buf[P + 2]) < 0) then
    Exit(False);
  if (P + 5 < Length(Buf)) and (Buf[P] = $81) and (Buf[P + 1] = $C4) and
     (PLongInt(@Buf[P + 2])^ < 0) then
    Exit(False);
  // Ambiguous: keep the historical EBP assumption.
end;

function TDebugger.TryFindRegParamRegSpill(AProcStart: Pointer;
  ARegParamIdx: Byte; const ARegs: TRegisters; out ABytes: TBytes): Boolean;
var
  Buf      : TBytes;
  P        : Integer;
  InboundRm: Byte;   // ModRM reg-code of the param's inbound register
  DestCode : Byte;   // ModRM reg-code of the move's destination register
  Val      : UIntPtr;
begin
  Result := False;
  ABytes := nil;
  if not FTargetIs32Bit then
    Exit;
  // x86 Delphi register convention: param 0=EAX(000), 1=EDX(010), 2=ECX(001).
  case ARegParamIdx of
    0: InboundRm := 0;  // EAX
    1: InboundRm := 2;  // EDX
    2: InboundRm := 1;  // ECX
  else
    Exit;  // 4th+ scalar param is stack-passed; no register
  end;
  Buf := ReadProcessMemory(AProcStart, 48);
  if Length(Buf) < 2 then
    Exit;
  P := 0;
  // Skip the leading callee-saved register pushes ($50..$57).
  while (P < Length(Buf)) and (Buf[P] in [$50..$57]) do
    Inc(P);
  // Skip the stack allocation (sub esp / add esp,-imm), if present.
  if (P + 2 < Length(Buf)) and (Buf[P] = $83) and (Buf[P + 1] in [$EC, $C4]) then
    Inc(P, 3)
  else if (P + 5 < Length(Buf)) and (Buf[P] = $81) and
          (Buf[P + 1] in [$EC, $C4]) then
    Inc(P, 6);
  // Inspect the run of register-direct MOVs that follows (the param
  // reg→reg spills, e.g. `8B DA 8B F0`). Stop at the first instruction that
  // is NOT such a MOV -- bounding the scan to the prologue so an immediate
  // byte further down can never be mis-read as a spill.
  while (P + 1 < Length(Buf)) and
        ((Buf[P] = $8B) or (Buf[P] = $89)) and ((Buf[P + 1] and $C0) = $C0) do
  begin
    DestCode := $FF;
    if Buf[P] = $8B then
    begin
      // 8B /r : reg field = destination, rm field = source.
      if (Buf[P + 1] and $07) = InboundRm then
        DestCode := (Buf[P + 1] shr 3) and $07;
    end
    else
    begin
      // 89 /r : reg field = source, rm field = destination.
      if ((Buf[P + 1] shr 3) and $07) = InboundRm then
        DestCode := Buf[P + 1] and $07;
    end;
    // Destination is a callee-saved register (EBX=3, ESI=6, EDI=7): the
    // param survives there across the body, so read its live value.
    if (DestCode = 3) or (DestCode = 6) or (DestCode = 7) then
    begin
      case DestCode of
        3: Val := ARegs.Ebx;
        6: Val := ARegs.Esi;
      else Val := ARegs.Edi;
      end;
      SetLength(ABytes, 8);
      FillChar(ABytes[0], 8, 0);
      PCardinal(@ABytes[0])^ := Cardinal(Val);
      Exit(True);
    end;
    Inc(P, 2);
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
  FLocalsReader := TRsmReader.Create;
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
// TRsmReader.LoadFromBytes, so under normal conditions every
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
  Proc := FLocalsReader.Procs[ProcIdx];

  BaseAddr := NativeInt(Regs.Ebp);
  if not FTargetIs32Bit then
  begin
    FrameInfo := GetStackFrameInfo(AThreadHandle);
    if FrameInfo.LocalSize > 0 then
      BaseAddr := BaseAddr + FrameInfo.LocalSize;
  end
  else if not ProcUsesEbpFrame(Pointer(NativeUInt(FBaseAddress) +
            CodeSectionRVA + Proc.SegmentOffset)) then
    // §6.35: frameless x86 proc reuses EBP as scratch and addresses its
    // locals off ESP (the frame bottom the TD32 offsets are anchored to).
    BaseAddr := NativeInt(Regs.Esp);

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
  Frameless : Boolean;
  FrameInfo : TStackFrameInfo;
  Loc       : TLocalVar;
  ProcIdx   : Integer;
  Proc      : TRsmProc;
  ProcStart : Pointer;
  RegBytes  : TBytes;
  Regs      : TRegisters;
  ResultList: IList<TLocalVar>;
  SegmentOff: NativeUInt;
  SlotAddr  : UIntPtr;
  SpillDisp : Integer;
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
  Proc := FLocalsReader.Procs[ProcIdx];
  ProcStart := Pointer(NativeUInt(FBaseAddress) + CodeSectionRVA + Proc.SegmentOffset);

  BaseAddr := NativeInt(Regs.Ebp);
  Frameless := False;
  if not FTargetIs32Bit then
  begin
    FrameInfo := GetStackFrameInfo(AThreadHandle);
    if FrameInfo.LocalSize > 0 then
      BaseAddr := BaseAddr + FrameInfo.LocalSize;
  end
  else if not ProcUsesEbpFrame(ProcStart) then
  begin
    // §6.35: frameless x86 proc reuses EBP as scratch and addresses its
    // locals off ESP (the frame bottom the TD32 offsets are anchored to).
    Frameless := True;
    BaseAddr := NativeInt(Regs.Esp);
  end;
  ResultList := Collections.NewPlainList<TLocalVar>;
  for I := 0 to Proc.Locals.Count - 1 do
  begin
    Loc.BpOffset := Proc.Locals[I].BpOffset;
    Loc.Name     := Proc.Locals[I].Name;
    Loc.Kind     := Proc.Locals[I].Kind;
    Loc.TypeIdx  := Proc.Locals[I].TypeIdx;
    if Proc.Locals[I].Kind = lkRegister then
    begin
      // Register-passed (Self / leading params): prefer the prologue's
      // frame home slot over the live register, which goes stale once the
      // body reuses the register. The spill displacement is relative to
      // the RAW frame pointer (EBP/RBP), not the LocalSize-corrected
      // BaseAddr, so apply it to Regs.Ebp directly. Fall back to the live
      // register when no spill was emitted (leaf / unspilled parameter).
      if TryFindRegParamSpillDisp(ProcStart, Proc.Locals[I].RegParamIdx, SpillDisp) then
      begin
        Loc.BpOffset := SpillDisp;
        SlotAddr := UIntPtr(NativeInt(Regs.Ebp) + SpillDisp);
        Loc.RawBytes := ReadProcessMemory(Pointer(SlotAddr), ReadSize);
      end
      else if Frameless and TryFindRegParamRegSpill(ProcStart,
                Proc.Locals[I].RegParamIdx, Regs, RegBytes) then
        // §6.35: a frameless proc spills a register param into a callee-saved
        // register (mov esi,eax / mov ebx,edx), not a frame slot -- read it
        // from that register; the live inbound register is stale at the BP.
        Loc.RawBytes := RegBytes
      else
        Loc.RawBytes := RegisterParamBytes(Regs, Proc.Locals[I].RegParamIdx);
    end
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
              FLastPauseWasException := False;
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
          FLastPauseWasException := False;
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
            FLastPauseWasException := False;
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
    // The ignore list (FIgnoredExceptions) only ever matches Delphi
    // language exceptions ($0EEDFADE), because only those carry a Delphi
    // class name. ReadExceptionClassName returns '' for any other code,
    // so a native/OS first-chance exception (e.g. C0000005 access
    // violation, E06D7363 C++ EH) can NEVER be suppressed by class name.
    // Three suppression paths are checked, in order:
    //   1) ignored Delphi class    (ignore_exception "EPrinter")
    //   2) ignored raw code        (ignore_exception_code C0000005)
    //   3) native first-chance, when break-on-native-first-chance is off
    var LIsDelphiExc := ExCode = EXCEPTION_DELPHI_LANGUAGE;
    var ExClass      := ReadExceptionClassName(ADebugEvent.Exception.ExceptionRecord);
    var LFirstChance := ADebugEvent.Exception.dwFirstChance <> 0;

    var LIsIgnored := False;
    if LIsDelphiExc and IsExceptionIgnored(ExClass) then
      LIsIgnored := True
    else if IsExceptionCodeIgnored(ExCode) then
      LIsIgnored := True
    else if (not LIsDelphiExc) and LFirstChance and (not FBreakOnNativeFirstChance) then
      LIsIgnored := True;

    if LIsIgnored then
    begin
      AContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
      Exit;
    end;

    FLastException := ADebugEvent.Exception.ExceptionRecord;
    FLastExceptionFirstChance := LFirstChance;
    FLastExceptionClassName := ExClass;
    FLastPauseWasException := True;
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

function TDebugger.EvaluateVariable(const AName: String; var AType: String;
  out AValue, AHint: String;
  AOnPhase: TProc<String>): Boolean;

  procedure Phase(const APhase: String);
  begin
    if Assigned(AOnPhase) then
      AOnPhase(APhase);
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

  // True when ATypeIdx (a Member's file-offset-based TypeIdx) resolves
  // to a class-kind struct in FClasses. Used by the Pfad-2 nil-refusal
  // discrimination to keep the "possible nil class pointer" hint for
  // genuine class-typed terminal members. (In practice auto-detect's
  // ClassLookup would already have formatted such a member as 'object'
  // before we reach the discrimination site, so this is a defensive
  // restatement of rule (a) rather than the load-bearing check -- the
  // load-bearing check is the record-hop branch flag.)
  function TerminalMemberResolvesToClass(ATypeIdx: UInt32): Boolean;
  var
    ClsIdx: Integer;
  begin
    Result := False;
    if (ATypeIdx = 0) or (not Assigned(FLocalsReader)) then Exit;
    ClsIdx := FLocalsReader.FindStructByTypeIdx(ATypeIdx);
    Result := (ClsIdx >= 0) and
              (FLocalsReader.Classes[ClsIdx].Kind = skClass);
  end;

var
  Segments       : TArray<String>;
  Locals         : TArray<TLocalVar>;
  Addr           : Pointer;
  FieldAddr      : Pointer;
  I              : Integer;
  IsLocal        : Boolean;
  /// RSM 2-byte type id of the matched whole-name local. Used by
  /// the auto-detection path so <c>evaluate LocalA</c> can pick a
  /// formatter without an explicit <c>type</c>. Stays 0 for
  /// non-local lookups (globals / dotted).
  MatchedLocalTypeIdx: UInt32;
  RawBytes       : TBytes;
  ObjPtr         : UIntPtr;
  ClsName        : String;
  Member         : TRsmClassMember;
  Formatter      : TEvaluateFormatter;
  /// Byte width of the final resolved record/class field, propagated
  /// from the dotted-walk into the int / int64 interpretation so a
  /// 1-byte Boolean or 2-byte Word doesn't get reported as a 4-byte
  /// integer that visibly carries the next field's bytes in the high
  /// part. Stays 0 for non-dotted lookups (direct global / local
  /// access) and for variant-record overlay siblings whose size can't
  /// be derived from the offset chain alone -- both cases fall back
  /// to the user-requested type's width.
  FieldKnownSize : Integer;
  /// True when the dotted walk's TERMINAL segment was resolved through
  /// the record-hop branch -- i.e. the final member is inline data
  /// sitting within a record, never a class-instance pointer slot.
  /// Drives the Pfad-2 nil-refusal discrimination (§6.20 R6-R9): a
  /// zero-valued record-field member that carries no RSM type metadata
  /// (TFW's strict-private TAd.Land -> TLandTyp is structurally
  /// undecodable per §6.20's five refuted rounds) is surfaced as a
  /// plain int instead of being mislabelled a possible nil class
  /// pointer. Stays False for non-dotted lookups and for terminal
  /// segments reached via a class-hop (where nil genuinely could be a
  /// nil object reference).
  DottedTerminalIsRecordField: Boolean;
begin
  Result := False;
  AValue := '';
  AHint  := '';
  FieldKnownSize := 0;
  DottedTerminalIsRecordField := False;
  FLastEnumTypeId := 0;
  Phase('begin');

  if FLastThreadHit = 0 then Exit;

  Addr := nil;
  IsLocal := False;
  RawBytes := nil;
  MatchedLocalTypeIdx := 0;
  // Stack-allocated record fields aren't zero-initialised in Delphi
  // (especially on Win64) -- a leftover PrimitiveTypeId / TypeIdx
  // from a prior call would trip the auto-detection's Path 1 / 2
  // and route a whole-name local or global through the wrong
  // formatter. Always start from a clean slate so Path 3 / 4 see
  // the real (empty) Member context.
  Member := Default(TRsmClassMember);

  // Resolve the variable. Two paths apply, tried in order:
  // 1. Treat the WHOLE name as a single identifier (matches a local, or
  //    a global with optional Unit.VarName qualification). This keeps
  //    the existing read_global_variable / single-local behavior intact.
  // 2. If that fails AND the name contains a dot, treat it as a dotted
  //    field chain: first segment is a local or global object reference,
  //    subsequent segments are field names looked up via RSM class info.
  Locals := GetLocals(FLastThreadHit);
  Phase('after GetLocals');
  for I := 0 to High(Locals) do
  begin
    if SameText(Locals[I].Name, AName) then
    begin
      IsLocal := True;
      RawBytes := Locals[I].RawBytes;
      MatchedLocalTypeIdx := Locals[I].TypeIdx;
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
    // named field via RSM class info, and advances to that field's
    // address. The final segment's address is then read with the
    // user-specified type. Requires RSM class layout (LoadDebugInfoFromExe).
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
      var FirstLocalTypeIdx: UInt32 := 0;
      for I := 0 to High(Locals) do
        if SameText(Locals[I].Name, Segments[0]) then
        begin
          FirstLocalTypeIdx := Locals[I].TypeIdx;
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
      // Prime the walk for record-typed first segments. Without this,
      // the first hop unconditionally tries class semantics on
      // Segments[0] -- it reads the record's first bytes as if they
      // were a VMT pointer, fails the VMT-class-name read, and
      // returns "Failed to evaluate" even for a fully-resolved
      // global like GGlobalMixed.FMixedInt. Two routes, tried in
      // order:
      //
      //   * Local-record (preferred when Segments[0] is a stack
      //     local). The RSM local record carries the local's
      //     declared type id, which TRsmReader stores as the
      //     file-offset-based TypeIdx -- so FindStructByTypeIdx
      //     gives us the record entry directly. This is what
      //     covers <LocalVariantHost.FInner.FLight> and
      //     <LocalEnumRec.FLight>.
      //   * Global-record (when the first segment is a global,
      //     or a local whose TypeIdx didn't resolve to a record).
      //     FindGlobalTypeIdx -> FindClassIdxByRsmTypeId is the
      //     direct route via the 2-byte registry id; the
      //     FindBestRecordForGlobalAndField fallback uses the
      //     "T<globalName>" naming convention + byte-stream
      //     proximity to recover the record for TFW-style
      //     globals whose stored id lives outside the registry
      //     id space.
      // VMT-priority override: if the first hop is a register-passed
      // local with a live instance pointer AND a VMT walk on it
      // succeeds against an FClasses entry, the local is a class
      // instance, NOT a record. Skip the record-hop priming below --
      // its RSM-TypeIdx-based resolution is unreliable across builds
      // because the 2-byte alias id can map to a different (record)
      // type in the live type registry than the live VMT reports.
      // Observed on the C:\MSE-26.04-Mongo build: Self's PARAM-record
      // TypeIdx 0x073D resolves to TMemoryPoolPos (skRecord), so the
      // priming wrongly flipped the walker to record-hop mode and
      // every Self.<field> dotted evaluate failed even though TFormAd
      // + FAd were correctly captured by the reader. For class
      // instances the VMT is authoritative and the class-hop branch
      // (which reads ClsName from the live VMT) trumps any RSM id
      // alias mismatch.
      // §6.32 generalises the §6.18 VMT-priority override from
      // register-passed locals to ALL object locals (BP-relative locals
      // and globals), because the same RSM-TypeIdx-aliases-to-a-record
      // hazard exists for them. A BP-relative inline-var local like
      //   var V := CJwksValidator.Create(...)
      // carries a per-proc reference id in its $20 record (NOT the type's
      // registry primary -- §4.2 design limit); that id can collide with
      // an unrelated record in the registry, so FindClassIdxByRsmTypeId
      // below would wrongly flip the walk to record-hop and every
      // V.<field> evaluate would fail -- even though the class
      // (CJwksValidator) and its fields are correctly discovered. The
      // live VMT is authoritative: deref the slot, and if its contents
      // VMT-resolve to a class in FClasses, this first segment is a class
      // instance -> skip record priming and let the class-hop branch use
      // the VMT-derived class name. A record-typed local/global derefs to
      // inline data whose VMT walk fails, so priming still runs for it.
      var SkipRecordPriming: Boolean := False;
      var ProbeInstancePtr : UIntPtr := 0;
      if FirstHopHasInstancePtr then
        ProbeInstancePtr := FirstHopInstancePtr        // register: value IS the ptr
      else if Assigned(Addr) then
        ProbeInstancePtr := ReadTargetPointer(Addr);   // BP-rel/global: slot holds the ptr
      if ProbeInstancePtr <> 0 then
      begin
        var VmtClsName: String;
        if ReadRuntimeClassName(ProbeInstancePtr, VmtClsName) and
           (FLocalsReader.FindClassByName(VmtClsName) >= 0) then
          SkipRecordPriming := True;
      end;
      var GStructIdx: Integer := -1;
      if not SkipRecordPriming then
      begin
        if FirstLocalTypeIdx <> 0 then
          // RSM local records carry the 2-byte RSM registry id, so
          // resolve through the registry-backed map -- same encoding
          // used by FindGlobalTypeIdx below. FindStructByTypeIdx
          // would mismatch because Classes[].TypeIdx is the
          // file-offset token, a different id space.
          GStructIdx := FLocalsReader.FindClassIdxByRsmTypeId(FirstLocalTypeIdx);
        if GStructIdx < 0 then
        begin
          var GTypeIdx: UInt32 := FLocalsReader.FindGlobalTypeIdx(Segments[0]);
          if GTypeIdx <> 0 then
            GStructIdx := FLocalsReader.FindClassIdxByRsmTypeId(GTypeIdx);
          if GStructIdx < 0 then
            GStructIdx := FLocalsReader.FindBestRecordForGlobalAndField(
              Segments[0], Segments[1]);
        end;
        if (GStructIdx >= 0) and
           (FLocalsReader.Classes[GStructIdx].Kind = skRecord) then
        begin
          PrevContextIdx := GStructIdx;
          ContextIsRecord := True;
        end;
      end;
      for I := 1 to High(Segments) do
      begin
        // Record which branch resolves THIS segment. After the loop the
        // flag reflects the terminal segment's hop kind and feeds the
        // Pfad-2 nil-refusal discrimination below: a member reached via
        // the record-hop branch is inline record data (never a nil-able
        // class-instance pointer slot).
        DottedTerminalIsRecordField := ContextIsRecord and (PrevContextIdx >= 0);
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
          if ReadRuntimeClassName(ObjPtr, ClsName) then
          begin
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
          end
          else
          begin
            // Class-hop's VMT walk failed -- ObjPtr does not point at
            // a Delphi class instance. The common case is that the
            // PREVIOUS field was a pointer-to-record (§4.2 / §6.18),
            // e.g. TFW's `TFormAd.FAd: PAd = ^TAd` or DebugTarget's
            // `TPtrToRecHost.FRecPtr: PMixedRec`. The RSM doesn't
            // populate a usable TypeIdx for these (the Format-A
            // linker doesn't translate the $2C payload into
            // Member.TypeIdx for record-typed fields generally), so
            // we cannot route into record-hop via the type registry
            // up front. Recover by name: search FClasses for the
            // single record whose Members contain Segments[I]; if
            // exactly one matches, treat ObjPtr as that record's
            // base address and resolve as a record-hop here.
            //
            // Pinned by
            // Test.DPT.MCP.Server.TestMcpEvaluateClassFieldPointerToRecordDeref.
            //
            // §6.19 closure: for canonical Delphi pointer aliases
            // (P<X>=^T<X>) the inter-segment context priming binds
            // Member.PointerTargetTypeIdx directly to the target
            // record's TypeIdx, so this fallback is no longer the
            // primary path -- the new branch above bypasses it
            // entirely. This fallback now serves as the backup for
            // non-canonical aliases (e.g. PFoo = ^TBar that breaks
            // the strict naming convention); the unique-match guard
            // still protects those from silent wrong-record picks.
            if ObjPtr = 0 then Exit;
            var FbFoundIdx  : Integer := -1;
            var FbMatchCount: Integer := 0;
            for var FbCi: Integer := 0 to FLocalsReader.Classes.Count - 1 do
            begin
              if FLocalsReader.Classes[FbCi].Kind <> skRecord then Continue;
              for var FbMi: Integer := 0 to FLocalsReader.Classes[FbCi].Members.Count - 1 do
                if SameText(FLocalsReader.Classes[FbCi].Members[FbMi].Name,
                            Segments[I]) then
                begin
                  if FbMatchCount = 0 then
                  begin
                    FbFoundIdx := FbCi;
                    Member := FLocalsReader.Classes[FbCi].Members[FbMi];
                  end;
                  Inc(FbMatchCount);
                  Break;
                end;
            end;
            if FbMatchCount <> 1 then Exit;
            FieldAddr := Pointer(ObjPtr + Member.Offset);
          end;
        end;

        // Determine the next iteration's context purely from the
        // resolved member's TypeIdx. With the RSM reader's
        // Format-A type-id linking, structured-typed fields carry
        // a real TypeIdx that resolves to a record (record-hop next)
        // or a class (class-hop next); built-in-typed terminal
        // fields have TypeIdx = 0 and the loop just exits with
        // FieldAddr pointing at their bytes.
        //
        // §6.19 pointer-to-record short-circuit. When the just-
        // resolved field is a pointer-to-record bound via the
        // P<X>=^T<X> convention (Format-A linker populated
        // Member.PointerTargetTypeIdx, Member.TypeIdx stayed 0),
        // dereference FieldAddr in place so the next iteration's
        // record-hop branch operates on the pointed-to record's
        // base address. This bypasses the §6.18 name-based
        // fallback (whose unique-match guard bails on member-name
        // collisions like TFW's Land on multiple records) and
        // produces a deterministic record-hop driven by the alias's
        // declared target type.
        //
        // Only deref when the current segment is NOT terminal --
        // the deref is purely there to prime the NEXT iteration's
        // record-hop. Doing it on the last segment would point
        // FieldAddr at the dereferenced record's base instead of
        // the field's slot, and the post-loop ReadProcessMemory
        // would then read the record's first bytes for a
        // user-typed read of the pointer itself (so
        // `evaluate Self.FAd (int)` would return the record's
        // first DWORD instead of the FAd pointer value).
        if (I < High(Segments)) and
           (Member.PointerTargetTypeIdx <> 0) and
           (FLocalsReader.FindStructByTypeIdx(Member.PointerTargetTypeIdx) >= 0) then
        begin
          var PtrVal: UIntPtr := ReadTargetPointer(FieldAddr);
          if PtrVal = 0 then Exit;
          FieldAddr := Pointer(PtrVal);
          PrevContextIdx := FLocalsReader.FindStructByTypeIdx(Member.PointerTargetTypeIdx);
          ContextIsRecord := True;
        end
        else if (Member.TypeIdx <> 0) and
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

  // Consolidate the address handle for IsLocal callers so the
  // ShortString / Extended formatters -- which need to re-read more
  // than RawBytes' 8 bytes -- can rely on a single Addr regardless
  // of whether the variable is local or global. Register-passed
  // locals correctly leave Addr at nil here (GetLocalAddress
  // declines them), which makes the > 8-byte formatters cleanly
  // fail rather than dereference a junk stack pointer.
  if IsLocal and (Addr = nil) then
    GetLocalAddress(FLastThreadHit, AName, Addr);

  // Auto-detection: empty AType means the caller wants the server
  // to pick a formatter based on the RSM-derived type information.
  // The actual lookup is delegated to AutoDetectFormatterName --
  // see its summary for the full resolution order. The resolved
  // formatter name is written back into AType (var parameter) so
  // the MCP server can surface it in the response label.
  if AType = '' then
    AType := AutoDetectFormatterName(AName, IsLocal, MatchedLocalTypeIdx, Member);

  if FFormatters.TryGetValue(LowerCase(AType), Formatter) then
  begin
    Result := Formatter(RawBytes, Addr, FieldKnownSize, AValue);
    Exit;
  end;

  // Class-pointer probe fallback: when the dotted walk resolved a
  // terminal Member but Format-A linking didn't populate its
  // TypeIdx (common for cross-unit / RTL-defined class types --
  // e.g. <Self.FNotification> on a VCL form, or <X.FList> where
  // FList is TStringList), the auto-detect paths above all bail
  // empty. Peek the 8 bytes as a class-instance pointer: dereference
  // to a candidate VMT and require the runtime VMT-classname read
  // to succeed. That strictness is what protects this fallback from
  // misclassifying primitive-typed fields whose value bytes happen
  // to fall in a readable memory range -- a primitive's "ptr" will
  // not yield a real class name via the VMT chain.
  if AType = '' then
  begin
    var ProbedName: String := '';
    if TryProbeClassPointer(RawBytes, ProbedName) then
    begin
      AValue := ProbedName;
      AType  := 'object';
      Result := True;
      Exit;
    end;
  end;

  // Name-based enum resolver: when the dotted walk resolved a
  // terminal Member with no type info AND the class-pointer probe
  // declined (bytes don't look like a class instance), try matching
  // the field name's F-prefix convention to an enum type name. Only
  // fire when the raw bytes look like a small enum ordinal -- if
  // higher-order bytes are non-zero, the value is probably a
  // pointer or wide integer, not an enum.
  if AType = '' then
  begin
    var EnumName: String := '';
    if TryNameBasedEnumLookup(AName, RawBytes, EnumName) then
    begin
      AValue := EnumName;
      AType  := 'enum';
      Result := True;
      Exit;
    end;
  end;

  // Scope-local enum resolver: same-compilation cross-unit enums
  // (e.g. <c>TStatus</c> declared in three sibling units pulled
  // into one program) carry a scope-local type alias in their $20
  // record's payload with hi-byte $1E -- a marker the name index
  // can't disambiguate. The $03 ENUM_DEF records the scanner
  // parsed give us the complete (unit, type, [elements]) tuples
  // needed to pick the right unit's enum based on the variable
  // name's trailing unit-suffix convention.
  //
  // Only fire for non-local globals where the registry type id
  // carries the scope-local marker ($1E hi byte). Other paths
  // already cover regular enums and primitives.
  if (AType = '') and (not IsLocal) and Assigned(FLocalsReader) then
  begin
    var GlobIdHere: UInt32 := FLocalsReader.FindGlobalTypeIdx(AName);
    if ((GlobIdHere shr 8) and $FF) = $1E then
    begin
      var ScopeName: String := '';
      var ScopeOrd : Integer := 0;
      if Length(RawBytes) >= 1 then
        ScopeOrd := RawBytes[0];
      // Path A -- strong-form bridge via the variable's stored
      // scope-local type id. RunPostProcess built a per-id map
      // from at least one conventionally-named anchor variable
      // per id, so any variable sharing that id resolves to the
      // correct EnumDef regardless of whether ITS OWN name
      // carries a unit hint. This is the principled bridge --
      // it doesn't rely on name-suffix heuristics on the current
      // variable name at all.
      if FLocalsReader.TryResolveByScopeLocalTypeId(
        GlobIdHere, ScopeOrd, ScopeName) then
      begin
        FLastEnumTypeId := 0;
        AValue := Format('%s (%d)', [ScopeName, ScopeOrd]);
        AType  := 'enum';
        Result := True;
        Exit;
      end;
      // Path B -- name-hint fallback when no anchor was found for
      // this type id (e.g. every variable of this type has a name
      // without a unit suffix). Derives a type-name hint from
      // the variable name and lets TryResolveScopeLocalEnum apply
      // its EndsWith unit-suffix matcher.
      var TypeHint: String := DeriveTypeHintFromVariableName(AName);
      if FLocalsReader.TryResolveScopeLocalEnum(
        AName, ScopeOrd, TypeHint, ScopeName) then
      begin
        FLastEnumTypeId := 0;
        AValue := Format('%s (%d)', [ScopeName, ScopeOrd]);
        AType  := 'enum';
        Result := True;
        Exit;
      end;
    end;
  end;

  // Record-terminal fallback: when auto-detection finds no scalar
  // formatter but the whole name resolves to a record-typed global
  // (or a record-typed local), surface the record's type name and
  // hint at sub-navigation. Without this, evaluate would just bail
  // with "Failed to evaluate" -- which hides the fact that the user
  // simply needs to add ".FieldName" to drill in.
  if (AType = '') and Assigned(FLocalsReader) then
  begin
    var RecTypeName: String := '';
    if TryGetRecordTerminalName(AName, IsLocal, MatchedLocalTypeIdx, RecTypeName) then
    begin
      AValue := Format('record %s (use .FieldName to inspect fields)', [RecTypeName]);
      AType  := 'record';
      Result := True;
      Exit;
    end;
  end;

  // All auto-detect paths declined. Populate a context-aware hint
  // so the MCP layer can surface a helpful message instead of an
  // opaque "Failed to evaluate". The two common shapes we recognise:
  //   * value is all-zero -> probably a nil class pointer that the
  //     class-pointer probe declined (nil is ambiguous between
  //     "nil object" and "Integer 0"). Suggest type=object.
  //   * non-zero value with no usable type info -> the field exists
  //     but its type wasn't recorded in the RSM Format-A registry
  //     and the value doesn't dereference cleanly. Suggest the
  //     usual primitive types.
  // Pfad 2 (§6.20 R6-R9): lift the nil-refusal for record-field
  // primitive/enum slots that carry no RSM type metadata. The five
  // refuted investigation rounds in Format.md §6.20 established that
  // TFW's strict-private record fields (the canonical case being
  // TAd.Land: TLandTyp) are NOT structurally bound to their type --
  // such a member resolves with Member.TypeIdx = PrimitiveTypeId =
  // PointerTargetTypeIdx all zero, so every auto-detect path above
  // declines and the value bytes (0 = ordinal of the first enum
  // element) look exactly like a nil pointer. The old behaviour
  // emitted the misleading "nil class pointer" hint here.
  //
  // Discrimination rule: refuse with that hint ONLY when the member
  // could genuinely be a class reference --
  //   (a) its TypeIdx resolves to a class in FClasses, OR
  //   (b) the terminal segment was reached through a class-hop (the
  //       member is a field slot in a live class instance and nil is a
  //       legitimate nil object).
  // For a terminal member reached through the record-hop branch it is
  // inline record data, never a nil-able class slot, so we surface the
  // raw bytes as a plain int. That doesn't recover the enum NAME (the
  // encoding still doesn't bind TAd.Land -> TLandTyp -- see §6.24 for
  // the future heuristic option), but it replaces the confusing error
  // with the usable ordinal the user can interpret manually.
  if (AType = '') and DottedTerminalIsRecordField and
     (Member.PointerTargetTypeIdx = 0) and
     (not TerminalMemberResolvesToClass(Member.TypeIdx)) then
  begin
    if FFormatters.TryGetValue('int', Formatter) and
       Formatter(RawBytes, Addr, FieldKnownSize, AValue) then
    begin
      AType  := 'int';
      Result := True;
      Exit;
    end;
  end;

  if AType = '' then
    AHint := BuildAutoDetectHint(AName, RawBytes);
end;

function TDebugger.BuildAutoDetectHint(const AName: String; const ARawBytes: TBytes): String;
// Generates a one-line hint that names the most likely concrete
// "type=" argument the user should try after auto-detection declined.
// The MCP layer appends this to the "Failed to evaluate" message so
// the caller learns WHAT to retry, not just THAT the call failed.
var
  PtrVal: UIntPtr;
begin
  Result := '';
  if Length(ARawBytes) < FTargetPointerSize then Exit;
  PtrVal := 0;
  Move(ARawBytes[0], PtrVal, FTargetPointerSize);
  if PtrVal = 0 then
    Result := Format(
      '%s holds 0 (nil pointer or zero-valued primitive). ' +
      'Auto-detection cannot pick a formatter for nil. ' +
      'Try type=object for a class reference, type=int for an integer, ' +
      'or type=string for a string.', [AName])
  else
    Result := Format(
      '%s could not be auto-typed (no RSM type metadata for this field). ' +
      'Try one of: type=object, type=int, type=int64, type=string.', [AName]);
end;

function TDebugger.DeriveTypeHintFromVariableName(const AName: String): String;
// Heuristic: "GStatusAlpha" -> "TStatus", "FThreadPriority" ->
// "TThreadPriority", "GMyVar" -> "TMyVar". Returns the empty
// string when the input is too short to plausibly carry both a
// type-name root and a unit-suffix segment.
var
  Stripped : String;
  Terminal : String;
  DotPos   : Integer;
  I        : Integer;
  LastUpper: Integer;
begin
  Result := '';
  Terminal := AName;
  DotPos := Terminal.LastIndexOf('.');
  if DotPos >= 0 then
    Terminal := Terminal.Substring(DotPos + 1);
  Stripped := Terminal;
  // Strip a leading G or F if followed by an uppercase letter.
  if (Length(Stripped) > 1) and
     ((Stripped[1] = 'G') or (Stripped[1] = 'F')) and
     CharInSet(Stripped[2], ['A'..'Z']) then
    Stripped := Copy(Stripped, 2, MaxInt);
  // Strip the trailing camelCase word: find the LAST uppercase
  // letter and chop from there. This isolates the type-name root
  // from the unit-suffix that Delphi-convention variable names use
  // to disambiguate same-named types across sibling units. Only
  // strip when the leading character is ALSO uppercase (otherwise
  // we'd erase the entire identifier).
  LastUpper := -1;
  for I := Length(Stripped) downto 2 do
    if CharInSet(Stripped[I], ['A'..'Z']) then
    begin
      LastUpper := I;
      Break;
    end;
  if (LastUpper > 1) and (Length(Stripped) - LastUpper >= 1) then
    Stripped := Copy(Stripped, 1, LastUpper - 1);
  if (Length(Stripped) >= 2) and CharInSet(Stripped[1], ['A'..'Z']) then
    Result := 'T' + Stripped;
end;

function TDebugger.TryNameBasedEnumLookup(const AName: String;
  const ARawBytes: TBytes; out AFormatted: String): Boolean;
// Last-resort enum auto-detect when the field has no usable type
// metadata. Mirrors Delphi's F<X> -> T<X> naming convention --
// strips a leading 'F' from the field name and looks up the
// resulting <c>T&lt;X&gt;</c> in the type-name registry. Only fires
// when the raw value bytes look like a small enum ordinal (the
// upper bytes are zero), so cross-unit class fields whose names
// would coincidentally match an enum-named type don't get
// misrouted -- the class-pointer probe handles those. When the
// primary id resolves to several alias secondaries, the call site
// passes the expected constant-name prefix (derived from the type
// name via Delphi's PascalCase-acronym convention, e.g.
// <c>TWindowState</c> -> <c>"ws"</c>) so the lookup picks the
// right enum's constants out of the ambiguous set.
var
  Terminal, Stripped: String;
  DotPos            : Integer;
  Candidate         : UInt32;
  Ordinal           : Integer;
  EnumName          : String;
  Prefix            : String;
  I                 : Integer;
begin
  Result     := False;
  AFormatted := '';
  if not Assigned(FLocalsReader) then Exit;
  if Length(ARawBytes) < 1 then Exit;
  // High bytes can legitimately carry data even for enum fields:
  // when reading 8 bytes from a 1-byte enum field, adjacent fields'
  // bytes leak in. The class-pointer probe (which ran first) has
  // already filtered out real class pointers, and the constant
  // lookup will reject out-of-range ordinals -- so it's safe to
  // proceed using byte 0 as the candidate ordinal without a
  // pre-filter on the upper bytes.
  Terminal := AName;
  DotPos := Terminal.LastIndexOf('.');
  if DotPos >= 0 then
    Terminal := Terminal.Substring(DotPos + 1);
  Stripped := Terminal;
  if (Length(Stripped) > 1) and (Stripped[1] = 'F') and
     CharInSet(Stripped[2], ['A'..'Z']) then
    Stripped := Copy(Stripped, 2, MaxInt);
  Candidate := FLocalsReader.FindTypeIdByName('T' + Stripped);
  if Candidate = 0 then Exit;
  if not FLocalsReader.IsEnumTypeId(Candidate) then Exit;
  Ordinal := ARawBytes[0];
  // Build the conventional constant-name prefix: take every uppercase
  // letter of the X in T<X> and lowercase it. TWindowState -> "ws",
  // TThreadPriority -> "tp", TFontStyle -> "fs". This matches the
  // Delphi convention where each enum element starts with this
  // acronym (wsNormal, tpHigher, fsBold).
  Prefix := '';
  for I := 1 to Length(Stripped) do
    if CharInSet(Stripped[I], ['A'..'Z']) then
      Prefix := Prefix + LowerCase(Stripped[I]);
  if FLocalsReader.TryGetEnumConstantName(Candidate, Ordinal, EnumName, Prefix) then
  begin
    AFormatted := Format('%s (%d)', [EnumName, Ordinal]);
    Result := True;
  end;
end;

function TDebugger.TryProbeClassPointer(const ARawBytes: TBytes;
  out AFormatted: String): Boolean;
// Treats the first FTargetPointerSize bytes as a class-instance
// pointer, dereferences to a candidate VMT slot, and only succeeds
// when ReadClassNameFromVMT yields a real class name (i.e. the
// pointer truly points at a class instance whose VMT layout matches
// Delphi's). Used by the dotted-walk terminal fallback to recover
// the "object" formatter when Format-A linking didn't populate the
// terminal Member's TypeIdx -- the same situation that defeats
// auto-detection of class-typed fields whose declared type lives in
// another unit (TFW: Self.FESAutoUpdate, VCL: Self.FNotification).
var
  PtrVal   : UIntPtr;
  VMTPtr   : UIntPtr;
  ClassName: String;
begin
  Result := False;
  AFormatted := '';
  if Length(ARawBytes) < FTargetPointerSize then Exit;
  PtrVal := 0;
  Move(ARawBytes[0], PtrVal, FTargetPointerSize);
  // Nil class pointers are ambiguous (could also be an uninitialized
  // primitive). Decline so the caller falls through to the record-
  // terminal hint / the explicit "Failed to evaluate" path -- the
  // user can still get the value with an explicit type=object.
  if PtrVal = 0 then Exit;
  VMTPtr := ReadTargetPointer(Pointer(PtrVal));
  if VMTPtr = 0 then Exit;
  if not ReadClassNameFromVMT(VMTPtr, ClassName) then Exit;
  AFormatted := ClassName + ' @ ' +
                Format('%.*x', [FTargetPointerSize * 2, PtrVal]);
  Result := True;
end;

function TDebugger.TryGetRecordTerminalName(const AName: String; AIsLocal: Boolean;
  AMatchedLocalTypeIdx: UInt32; out ARecTypeName: String): Boolean;
// Returns True (with the record's type name) when AName refers to a
// record-typed local or global. Used by EvaluateVariable's
// record-terminal fallback so a whole-name evaluate of a record
// surfaces a navigable hint instead of "Failed to evaluate".
var
  TypeId: UInt32;
  ClsIdx: Integer;
begin
  Result := False;
  ARecTypeName := '';
  if not Assigned(FLocalsReader) then Exit;
  if AIsLocal then
    TypeId := AMatchedLocalTypeIdx
  else
    TypeId := FLocalsReader.FindGlobalTypeIdx(AName);
  if TypeId = 0 then Exit;
  ClsIdx := FLocalsReader.FindClassIdxByRsmTypeId(TypeId);
  if (ClsIdx < 0) or (FLocalsReader.Classes[ClsIdx].Kind <> skRecord) then Exit;
  ARecTypeName := FLocalsReader.Classes[ClsIdx].Name;
  Result := True;
end;

/// <summary>
///   Resolution order for omitted <c>type</c> arguments:
///   <list type="number">
///     <item>Dotted-terminal field with a captured primitive type id
///       (Integer / Word / Double / string / Bool / Currency /
///       AnsiString / WideString / ShortString / ...).</item>
///     <item>Dotted-terminal field whose TypeIdx resolves to a class
///       -> <c>"object"</c>.</item>
///     <item>Whole-name local whose own TypeIdx is a primitive id or
///       resolves to a class.</item>
///     <item>Whole-name global whose registry type id is a primitive
///       id or resolves to a class.</item>
///   </list>
///   Returns an empty string when nothing matches.
/// </summary>
function TDebugger.AutoDetectFormatterName(const AName: String; AIsLocal: Boolean;
  AMatchedLocalTypeIdx: UInt32; const AMember: TRsmClassMember): String;

  function ClassLookup(ATypeIdx: UInt32; AUseRegistry: Boolean): String;
  // Returns 'object' when ATypeIdx resolves to a class in FClasses,
  // empty otherwise. AUseRegistry switches between the registry-id
  // lookup (for the 2-byte form globals carry) and the file-offset
  // lookup (for the form Member.TypeIdx carries).
  var
    ClsIdx: Integer;
  begin
    Result := '';
    if (ATypeIdx = 0) or (not Assigned(FLocalsReader)) then Exit;
    if AUseRegistry then
      ClsIdx := FLocalsReader.FindClassIdxByRsmTypeId(ATypeIdx)
    else
      ClsIdx := FLocalsReader.FindStructByTypeIdx(ATypeIdx);
    if (ClsIdx >= 0) and (FLocalsReader.Classes[ClsIdx].Kind = skClass) then
      Result := 'object';
  end;

  function EnumLookup(ATypeId: UInt32): String;
  // Returns 'enum' when the type id was seen in any $25 enum-constant
  // record. Side effect: stashes the type id on the debugger so the
  // 'enum' formatter (which gets the same uniform formatter args as
  // every other formatter and therefore can't take a type-id arg
  // directly) can resolve the ordinal to its identifier name.
  begin
    Result := '';
    if (ATypeId = 0) or (not Assigned(FLocalsReader)) then Exit;
    if FLocalsReader.IsEnumTypeId(ATypeId) then
    begin
      FLastEnumTypeId := ATypeId;
      Result := 'enum';
    end;
  end;

var
  GlobTypeIdx: UInt32;
begin
  Result := '';
  // Path 1: terminal primitive field captured during the field-scan.
  if AMember.PrimitiveTypeId <> 0 then
  begin
    if FPrimitiveTypeFormatters.TryGetValue(AMember.PrimitiveTypeId, Result) then
      Exit;
    Result := EnumLookup(AMember.PrimitiveTypeId);
    if Result <> '' then Exit;
  end;
  // Path 2: terminal class-typed field.
  Result := ClassLookup(AMember.TypeIdx, False);
  if Result <> '' then Exit;
  // Path 3: whole-name local. The local's encoded TypeIdx is the
  // 2-byte RSM id; try the primitive table, the enum registry,
  // then the class registry.
  if AIsLocal and (AMatchedLocalTypeIdx <> 0) then
  begin
    if FPrimitiveTypeFormatters.TryGetValue(UInt16(AMatchedLocalTypeIdx), Result) then
      Exit;
    Result := EnumLookup(AMatchedLocalTypeIdx);
    if Result <> '' then Exit;
    Result := ClassLookup(AMatchedLocalTypeIdx, True);
    if Result <> '' then Exit;
  end;
  // Path 4: whole-name global. Same triple lookup against the
  // global's registry type id.
  if (not AIsLocal) and Assigned(FLocalsReader) then
  begin
    GlobTypeIdx := FLocalsReader.FindGlobalTypeIdx(AName);
    if GlobTypeIdx <> 0 then
    begin
      if FPrimitiveTypeFormatters.TryGetValue(UInt16(GlobTypeIdx), Result) then
        Exit;
      Result := EnumLookup(GlobTypeIdx);
      if Result <> '' then Exit;
      Result := ClassLookup(GlobTypeIdx, True);
    end;
  end;

  // (Path 5 name-based enum resolver lives in
  // <see cref="TryNameBasedEnumLookup"/> -- invoked from
  // EvaluateVariable AFTER the class-pointer probe so we don't
  // misroute a cross-unit class field whose name happens to match
  // some enum's T-prefix convention.)
end;

/// <summary>
///   Reads a Delphi long string (UnicodeString / AnsiString /
///   WideString) given its dynamic-pointer payload. <c>APtrVal</c>
///   is the value stored AT the variable: 0 means empty/nil,
///   otherwise it points at the character data with a 4-byte
///   length field at offset -4. Length is in characters for
///   UnicodeString / AnsiString, in bytes for WideString
///   (<c>ALengthIsBytes</c> = True).
/// </summary>
function TDebugger.ReadLongString(APtrVal: UIntPtr; ACharSize: Integer;
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

/// <summary>
///   Read the class name pointed at by
///   <c>AVMTPtr - vmtClassName</c>. Used by the dotted-walk's VMT
///   walk and by <c>FormatObject</c>. Offsets chosen empirically to
///   match the Delphi 12 layout including the CPP_ABI_ADJUST shift
///   on Win64.
/// </summary>
function TDebugger.ReadClassNameFromVMT(AVMTPtr: UIntPtr;
  out AClassName: String): Boolean;
var
  ClassNamePtr   : UIntPtr;
  VMTClassNameOfs: Integer;
  Buf            : TBytes;
  Len            : Byte;
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

/// <summary>
///   Read exactly <c>AFieldKnownSize</c> bytes when we know the
///   field is narrower than 4 (Boolean = 1, Word / SmallInt = 2).
///   Zero-extend to 32 bits so a Word like <c>TMdt.Id</c> at offset
///   20 returns its actual value (1) instead of getting
///   concatenated with the next field's bytes
///   (1 | (1 shl 16) = 65537). Sign-extending signed narrow types
///   (SmallInt, ShortInt) would need the type's signedness from
///   RSM, which we don't track for primitives -- the common case
///   (unsigned Word / Byte / Boolean) is what users hit in
///   practice, so default to zero-extend.
/// </summary>
function TDebugger.FormatInt(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  ReadSize: Integer;
  IntVal  : Integer;
begin
  Result := False;
  AValue := '';
  ReadSize := 4;
  if (AFieldKnownSize > 0) and (AFieldKnownSize < 4) then
    ReadSize := AFieldKnownSize;
  if Length(ARawBytes) < ReadSize then Exit;
  IntVal := 0;
  Move(ARawBytes[0], IntVal, ReadSize);
  // Decimal first (the human-readable signed value), then the raw hex
  // pattern of exactly ReadSize bytes -- so a pointer-valued int like
  // -397207008 reads as "-397207008 (0xE8531A20)" without anyone having
  // to do the two's-complement conversion by hand. IntToHex on the
  // signed Integer already renders the bit pattern, and the ReadSize*2
  // width makes a clamped Word/Byte field's true width visible
  // (Word -> 0x0001, Byte -> 0xA5).
  AValue := IntToStr(IntVal) + ' (0x' + IntToHex(IntVal, ReadSize * 2) + ')';
  Result := True;
end;

/// <summary>
///   Same clamp story as <c>FormatInt</c>, extended to 8-byte
///   targets: a Cardinal field (size 4) reported via
///   <c>type=int64</c> should read 4 bytes and zero-extend to 64,
///   not pull 8 and concatenate.
/// </summary>
function TDebugger.FormatInt64(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  ReadSize: Integer;
  Int64Val: Int64;
begin
  Result := False;
  AValue := '';
  ReadSize := 8;
  if (AFieldKnownSize > 0) and (AFieldKnownSize < 8) then
    ReadSize := AFieldKnownSize;
  if Length(ARawBytes) < ReadSize then Exit;
  Int64Val := 0;
  Move(ARawBytes[0], Int64Val, ReadSize);
  // Same decimal + raw-hex shape as FormatInt; the ReadSize*2 width
  // gives a full-width Win64 pointer 16 hex digits and a clamped
  // 4-byte Cardinal exactly 8.
  AValue := IntToStr(Int64Val) + ' (0x' + IntToHex(Int64Val, ReadSize * 2) + ')';
  Result := True;
end;

/// <summary>
///   UnicodeString (the Delphi <c>string</c> default since D2009):
///   pointer to UTF-16 data, 4-byte length at -4 in characters.
/// </summary>
function TDebugger.FormatString(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  PtrVal: UIntPtr;
begin
  Result := False;
  AValue := '';
  if Length(ARawBytes) < FTargetPointerSize then Exit;
  PtrVal := 0;
  Move(ARawBytes[0], PtrVal, FTargetPointerSize);
  Result := ReadLongString(PtrVal, 2, TEncoding.Unicode, False, AValue);
end;

/// <summary>
///   AnsiString: pointer-to-byte data, 4-byte length at -4 in
///   CHARACTERS, codepage at -12 (since D2009). Decoding uses the
///   system ANSI codepage; for non-ASCII content the codepage at
///   -12 would give a more accurate decode but ASCII content is
///   identical either way.
/// </summary>
function TDebugger.FormatAnsiString(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  PtrVal: UIntPtr;
begin
  Result := False;
  AValue := '';
  if Length(ARawBytes) < FTargetPointerSize then Exit;
  PtrVal := 0;
  Move(ARawBytes[0], PtrVal, FTargetPointerSize);
  Result := ReadLongString(PtrVal, 1, TEncoding.ANSI, False, AValue);
end;

/// <summary>
///   WideString is BSTR-compatible: 4-byte length at -4 in BYTES
///   (not chars), data is UTF-16.
/// </summary>
function TDebugger.FormatWideString(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  PtrVal: UIntPtr;
begin
  Result := False;
  AValue := '';
  if Length(ARawBytes) < FTargetPointerSize then Exit;
  PtrVal := 0;
  Move(ARawBytes[0], PtrVal, FTargetPointerSize);
  Result := ReadLongString(PtrVal, 2, TEncoding.Unicode, True, AValue);
end;

/// <summary>
///   ShortString is INLINE: the variable IS a length-prefixed
///   buffer (max 256 bytes), not a pointer. Re-read up to 256 bytes
///   from <c>AAddr</c> -- which the caller consolidated to point at
///   the actual storage regardless of whether the source was a
///   local stack slot or a global.
/// </summary>
function TDebugger.FormatShortString(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  Buf    : TBytes;
  LenByte: Byte;
begin
  Result := False;
  AValue := '';
  if not Assigned(AAddr) then Exit;
  Buf := ReadProcessMemory(AAddr, 256);
  if Length(Buf) < 1 then Exit;
  LenByte := Buf[0];
  if Integer(LenByte) + 1 > Length(Buf) then Exit;
  if LenByte = 0 then
    AValue := ''
  else
    AValue := TEncoding.ANSI.GetString(Buf, 1, LenByte);
  Result := True;
end;

/// <summary>
///   IEEE 754 single-precision (4 bytes). The RawBytes slot is at
///   least 8 bytes; we consume the first 4 and ignore the rest.
/// </summary>
function TDebugger.FormatSingle(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  SingleVal: Single;
begin
  Result := False;
  AValue := '';
  if Length(ARawBytes) < 4 then Exit;
  SingleVal := 0.0;
  Move(ARawBytes[0], SingleVal, 4);
  AValue := FloatToStrF(SingleVal, ffGeneral, 7, 0,
    TFormatSettings.Invariant);
  Result := True;
end;

/// <summary>
///   IEEE 754 double-precision (8 bytes). RawBytes is sized at 8.
/// </summary>
function TDebugger.FormatDouble(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  DoubleVal: Double;
begin
  Result := False;
  AValue := '';
  if Length(ARawBytes) < 8 then Exit;
  DoubleVal := 0.0;
  Move(ARawBytes[0], DoubleVal, 8);
  AValue := FloatToStrF(DoubleVal, ffGeneral, 15, 0,
    TFormatSettings.Invariant);
  Result := True;
end;

/// <summary>
///   Delphi Extended is 80-bit (10 bytes) on both Win32 and Win64
///   by default (<c>EXCESSPRECISION ON</c> is the dcc64 default).
///   Re-read 10 bytes from <c>AAddr</c> and decode the 80-bit
///   format manually into a Double for printing. The manual decode
///   avoids depending on the host DPT.exe's own
///   <c>SizeOf(Extended)</c>, which can be 8 on builds with
///   <c>EXCESSPRECISION OFF</c>.
/// </summary>
/// <remarks>
///   80-bit Extended layout (LE):
///   <list type="bullet">
///     <item>bytes 0..7 = 64-bit mantissa with EXPLICIT leading 1 bit</item>
///     <item>bytes 8..9 = 1-bit sign + 15-bit biased exponent (bias $3FFF)</item>
///   </list>
/// </remarks>
function TDebugger.FormatExtended(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  ExtBuf    : TBytes;
  Mantissa  : UInt64;
  SignExp   : UInt16;
  Sign      : UInt64;
  Exp80     : Integer;
  ExpD      : Integer;
  DoubleBits: UInt64;
  DoubleVal : Double;
begin
  Result := False;
  AValue := '';
  if not Assigned(AAddr) then Exit;
  ExtBuf := ReadProcessMemory(AAddr, 10);
  if Length(ExtBuf) <> 10 then Exit;
  Mantissa := 0;
  SignExp := 0;
  Move(ExtBuf[0], Mantissa, 8);
  Move(ExtBuf[8], SignExp, 2);
  Sign := (SignExp shr 15) and 1;
  Exp80 := SignExp and $7FFF;
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
    // bias $3FF) and drop the explicit leading-1 bit by shifting
    // the mantissa right by 11.
    ExpD := Exp80 - $3FFF + $3FF;
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
  DoubleVal := 0.0;
  Move(DoubleBits, DoubleVal, 8);
  AValue := FloatToStrF(DoubleVal, ffGeneral, 18, 0,
    TFormatSettings.Invariant);
  Result := True;
end;

/// <summary>
///   Boolean field: read <c>AFieldKnownSize</c> bytes (default 1)
///   and emit <c>"True"</c> / <c>"False"</c>. Delphi's
///   <c>Boolean</c> / <c>ByteBool</c> are 1 byte, <c>WordBool</c>
///   is 2, <c>LongBool</c> is 4. Any non-zero raw value reads as
///   True; the formatter doesn't restrict itself to {0, 1} because
///   <c>LongBool(-1)</c> is the documented Win32-API True.
/// </summary>
function TDebugger.FormatBool(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  ReadSize: Integer;
  IntVal  : Integer;
begin
  Result := False;
  AValue := '';
  ReadSize := 1;
  if (AFieldKnownSize > 0) and (AFieldKnownSize <= 4) then
    ReadSize := AFieldKnownSize;
  if Length(ARawBytes) < ReadSize then Exit;
  IntVal := 0;
  Move(ARawBytes[0], IntVal, ReadSize);
  if IntVal = 0 then AValue := 'False' else AValue := 'True';
  Result := True;
end;

/// <summary>
///   Enum: read the ordinal byte from the raw value, then resolve it
///   to the declared identifier via the RSM enum registry. The output
///   is <c>"Identifier (Ordinal)"</c> so the caller keeps access to
///   both the readable form and the underlying numeric value.
/// </summary>
/// <remarks>
///   <c>FLastEnumTypeId</c> is the 2-byte enum type id the
///   auto-detection layer matched. Without it, the formatter can't
///   pick the right enum -- so callers must populate it before
///   dispatching; the auto-detection path in <c>EvaluateVariable</c>
///   takes care of that.
/// </remarks>
function TDebugger.FormatEnum(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
const
  Widths: array [0..2] of Integer = (1, 2, 4);
var
  Ordinal : Integer;
  EnumName: String;
  W       : Integer;
begin
  Result := False;
  AValue := '';
  if Length(ARawBytes) < 1 then Exit;
  // Enums commonly fit in 1 byte (up to 256 elements) but the Delphi
  // compiler widens to 2 / 4 bytes when the declared element set
  // demands it. AFieldKnownSize comes from Member.Size, which is
  // derived from the gap to the next field's offset -- correct for
  // dense record layouts but misleading for a 1-byte enum followed
  // by an aligned 4-byte field (Size becomes 4 because of the
  // padding gap). Probe widths smallest-first against the enum's
  // registered constants: the smallest width that resolves to a
  // known constant for FLastEnumTypeId wins. This recovers the
  // correct 1-byte read for default-size enums even when Member.Size
  // over-states it, while still widening for enums that genuinely
  // need 2 / 4 bytes (compiler-widened by element count or by
  // an explicit <c>{$MINENUMSIZE 4}</c>).
  if (FLastEnumTypeId <> 0) and Assigned(FLocalsReader) then
    for W in Widths do
    begin
      if Length(ARawBytes) < W then Continue;
      Ordinal := 0;
      Move(ARawBytes[0], Ordinal, W);
      if FLocalsReader.TryGetEnumConstantName(
        FLastEnumTypeId, Ordinal, EnumName) then
      begin
        AValue := Format('%s (%d)', [EnumName, Ordinal]);
        Exit(True);
      end;
    end;
  // No width resolved (no enum type id, no constants found, or
  // the value is genuinely out of the registered range -- could
  // happen on uninitialised stack memory or after a bad cast).
  // Surface the field-size ordinal so the caller still sees
  // something useful.
  W := AFieldKnownSize;
  if W <= 0 then W := 1;
  if W > 4 then W := 4;
  if Length(ARawBytes) < W then W := Length(ARawBytes);
  Ordinal := 0;
  Move(ARawBytes[0], Ordinal, W);
  AValue := Format('<unknown> (%d)', [Ordinal]);
  Result := True;
end;

/// <summary>
///   Currency: 8-byte scaled Int64 with implicit 4-decimal scaling
///   (the on-disk value is the literal Int64 of <c>Value * 10000</c>).
///   Output is fixed-point with 4 decimal places, period as decimal
///   separator (invariant formatting) so the result is locale-stable.
/// </summary>
function TDebugger.FormatCurrency(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  CurrVal: Currency;
begin
  Result := False;
  AValue := '';
  if Length(ARawBytes) < 8 then Exit;
  CurrVal := 0;
  Move(ARawBytes[0], CurrVal, 8);
  AValue := CurrToStrF(CurrVal, ffFixed, 4, TFormatSettings.Invariant);
  Result := True;
end;

/// <summary>
///   Read the field as an instance pointer, then walk the VMT to
///   read the runtime class name. Output is
///   <c>"ClassName @ HexAddr"</c> or <c>"nil"</c> for a null
///   pointer, or <c>"Object @ HexAddr"</c> when the pointer is
///   non-zero but the VMT walk fails (garbage pointer).
/// </summary>
function TDebugger.FormatObject(const ARawBytes: TBytes; AAddr: Pointer;
  AFieldKnownSize: Integer; out AValue: String): Boolean;
var
  PtrVal   : UIntPtr;
  VMTPtr   : UIntPtr;
  ClassName: String;
begin
  Result := False;
  AValue := '';
  if Length(ARawBytes) < FTargetPointerSize then Exit;
  PtrVal := 0;
  Move(ARawBytes[0], PtrVal, FTargetPointerSize);
  if PtrVal = 0 then
  begin
    AValue := 'nil';
    Exit(True);
  end;
  VMTPtr := ReadTargetPointer(Pointer(PtrVal));
  if (VMTPtr <> 0) and ReadClassNameFromVMT(VMTPtr, ClassName) then
    AValue := ClassName + ' @ ' +
              Format('%.*x', [FTargetPointerSize * 2, PtrVal])
  else
    AValue := 'Object @ ' +
              Format('%.*x', [FTargetPointerSize * 2, PtrVal]);
  Result := True;
end;

end.
