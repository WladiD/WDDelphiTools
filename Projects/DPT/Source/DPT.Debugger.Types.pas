// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Debugger.Types;

// Shared base types for the debugger's variable-evaluation layer.
//
// This unit exists to break the otherwise-circular reference between
// DPT.Debugger (owns TDebugger) and DPT.Debugger.Evaluate (owns
// TVariableEvaluator): the evaluator is driven through IEvalProcessAccess,
// whose signatures need TLocalVar and TEvaluateFormatter, while TDebugger
// needs to name TVariableEvaluator. Hoisting the shared types + the seam
// interface here makes the dependency chain linear:
//   DPT.Debugger.Types  <-  DPT.Debugger.Evaluate  <-  DPT.Debugger

interface

uses
  Winapi.Windows,
  System.SysUtils,

  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.Reader;

type

  /// <summary>
  ///   A named local variable resolved from the live stack frame plus the
  ///   .rsm symbols. <c>RawBytes</c> are 8 little-endian bytes read at
  ///   <c>BpOffset</c> from EBP/RBP; callers interpret them per the
  ///   source-level type. <c>TypeIdx</c> is the 2-byte RSM type id used by
  ///   the auto-detection path; <c>Kind</c> records whether the local was
  ///   reached as a BP-relative stack slot or a CPU register.
  /// </summary>
  TLocalVar = record
    BpOffset: Int32;
    Name    : String;
    RawBytes: TBytes;
    TypeIdx : UInt32;
    Kind    : TRsmLocalKind;
  end;

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

  /// <summary>
  ///   The live-process / symbol seam that <c>TVariableEvaluator</c> needs
  ///   from its host (<c>TDebugger</c>). It exposes exactly the primitives
  ///   the dotted-walk evaluator calls: raw target memory + pointer reads,
  ///   the VMT class-name read, local/global address resolution, the RTTI
  ///   property + reference-type recovery services, the formatter
  ///   registries, the per-call enum-type-id side channel, and the arch
  ///   flags. Implemented by TDebugger with no-op reference counting (it is
  ///   a plain TObject whose lifetime the owner controls).
  /// </summary>
  IEvalProcessAccess = interface
    ['{9F3A2C14-7B6E-4D58-AE21-3C9D0F5B8E42}']
    function GetTargetIs32Bit: Boolean;
    function GetTargetPointerSize: Integer;
    function GetLastThreadHit: THandle;
    function GetBaseAddress: UIntPtr;
    function GetCallResultSlot: UIntPtr;
    function GetLastEnumTypeId: UInt32;
    procedure SetLastEnumTypeId(AValue: UInt32);
    function GetLocalsReader: TRsmReader;
    function GetFormatters: IKeyValue<String, TEvaluateFormatter>;
    function GetPrimitiveTypeFormatters: IKeyValue<UInt16, String>;
    /// Target is a 32-bit (Win32) process.
    property TargetIs32Bit: Boolean read GetTargetIs32Bit;
    /// Pointer width of the target (4 or 8).
    property TargetPointerSize: Integer read GetTargetPointerSize;
    /// Handle of the thread that last hit a breakpoint (the paused frame).
    property LastThreadHit: THandle read GetLastThreadHit;
    /// Image base of the debuggee.
    property BaseAddress: UIntPtr read GetBaseAddress;
    /// Scratch slot holding a getter's string @Result after call injection.
    property CallResultSlot: UIntPtr read GetCallResultSlot;
    /// Per-call side channel: the RSM enum type id the auto-detect layer
    /// identified, read by the 'enum' formatter to resolve the constant name.
    property LastEnumTypeId: UInt32 read GetLastEnumTypeId write SetLastEnumTypeId;
    /// The .rsm symbol reader.
    property LocalsReader: TRsmReader read GetLocalsReader;
    /// Lowercased-type-name -> output formatter.
    property Formatters: IKeyValue<String, TEvaluateFormatter> read GetFormatters;
    /// Compiler primitive type id -> formatter name.
    property PrimitiveTypeFormatters: IKeyValue<UInt16, String>
      read GetPrimitiveTypeFormatters;
    /// Read ASize raw bytes from the target at AAddress.
    function ReadProcessMemory(AAddress: Pointer; ASize: NativeUInt): TBytes;
    /// Read one pointer-sized value from the target at AAddress.
    function ReadTargetPointer(AAddress: Pointer): UIntPtr;
    /// Read the class name pointed at by <c>AVMTPtr - vmtClassName</c>.
    function ReadClassNameFromVMT(AVMTPtr: UIntPtr; out AClassName: String): Boolean;
    /// Enumerate the named locals of the frame on AThreadHandle.
    function GetLocals(AThreadHandle: THandle): TArray<TLocalVar>;
    /// Resolve a named local's live address on AThreadHandle.
    function GetLocalAddress(AThreadHandle: THandle; const AName: String;
      out AAddress: Pointer): Boolean;
    /// Resolve a global symbol name to its live address.
    function GetAddressFromSymbol(const ASymbolName: string): Pointer;
    /// Resolve a published/extended-RTTI property on the live instance
    /// (field-backed, static getter, or virtual getter). §6.37 / §6.38.
    function TryResolveRttiProperty(AObjPtr: UIntPtr; const APropName: String;
      out AIsField: Boolean; out AFieldOffset: UInt32; out AGetterAddr: UIntPtr;
      out AIsStringResult: Boolean; out APrimIdHint: UInt16): Boolean;
    /// Name the runtime type behind a live reference slot (object or
    /// interface reference). §6.36.
    function TryRecoverReferenceType(ASlotPtr: UIntPtr; out ADesc: String): Boolean;
    /// Run a property getter in the target on the paused thread (call
    /// injection) and return its ordinal/string @Result. §6.37.
    function CallTargetFunction(AGetterAddr, ASelf: UIntPtr;
      AStringResult: Boolean; out AOrdinal: UInt64): Boolean;
    /// True (with the record's type name) when AName is a record-typed
    /// local or global; drives EvaluateVariable's record-terminal hint.
    function TryGetRecordTerminalName(const AName: String; AIsLocal: Boolean;
      AMatchedLocalTypeIdx: UInt32; out ARecTypeName: String;
      out AClsIdx: Integer): Boolean;
  end;

implementation

end.
