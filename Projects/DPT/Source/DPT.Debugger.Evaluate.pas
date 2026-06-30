// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Debugger.Evaluate;


interface

uses
  Winapi.Windows,
  System.SysUtils,

  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.Reader,
  DPT.Td32.Reader,
  DPT.Debugger.Types;

type

  TVariableEvaluator = class
  private
    FHost: IEvalProcessAccess;
    function GetReader: TRsmReader; inline;
    function GetTd32Reader: TTd32Reader; inline;
    function GetTargetIs32Bit: Boolean; inline;
    function GetTargetPointerSize: Integer; inline;
    function GetLastThreadHit: THandle; inline;
    function GetBaseAddress: UIntPtr; inline;
    function GetCallResultSlot: UIntPtr; inline;
    function GetFormatters: IKeyValue<String, TEvaluateFormatter>; inline;
    function GetPrimitiveTypeFormatters: IKeyValue<UInt16, String>; inline;
    function GetLastEnumTypeId: UInt32; inline;
    procedure SetLastEnumTypeId(AValue: UInt32); inline;
    function ReadProcessMemory(AAddress: Pointer; ASize: NativeUInt): TBytes; inline;
    function ReadTargetPointer(AAddress: Pointer): UIntPtr; inline;
    function ReadClassNameFromVMT(AVMTPtr: UIntPtr; out AClassName: String): Boolean; inline;
    function GetLocals(AThreadHandle: THandle): TArray<TLocalVar>; inline;
    function GetLocalAddress(AThreadHandle: THandle; const AName: String;
      out AAddress: Pointer): Boolean; inline;
    function GetAddressFromSymbol(const ASymbolName: string): Pointer; inline;
    function TryResolveRttiProperty(AObjPtr: UIntPtr; const APropName: String;
      out AIsField: Boolean; out AFieldOffset: UInt32; out AGetterAddr: UIntPtr;
      out AIsStringResult: Boolean; out APrimIdHint: UInt16): Boolean; inline;
    function TryRecoverReferenceType(ASlotPtr: UIntPtr; out ADesc: String): Boolean; inline;
    function CallTargetFunction(AGetterAddr, ASelf: UIntPtr;
      AStringResult: Boolean; out AOrdinal: UInt64): Boolean; inline;
    function TryGetRecordTerminalName(const AName: String; AIsLocal: Boolean;
      AMatchedLocalTypeIdx: UInt32; out ARecTypeName: String;
      out AClsIdx: Integer): Boolean; inline;
    // --- moved cluster: live-VMT helpers ------------------------------
    function ReadRuntimeClassName(APtrVal: UIntPtr; out AClassName: String): Boolean;
    function TryReadParentVMT(AVMT: UIntPtr; out AParentVMT: UIntPtr;
      out AParentName: String): Boolean;
    function FindFieldViaVmtWalk(AObjPtr: UIntPtr; const AFieldName: String;
      out AMember: TRsmClassMember): Boolean;
    function FindPropertyViaVmtWalk(AObjPtr: UIntPtr;
      const AStartClassName, APropName: String;
      out AProp: TRsmClassProperty; out AOwnerClass: String): Boolean;
    function TerminalMemberResolvesToClass(ATypeIdx: UInt32): Boolean;
    // --- moved cluster: auto-detect + hint helpers --------------------
    function BuildAutoDetectHint(const AName: String;
      const ARawBytes: TBytes; AAddr: Pointer): String;
    function BuildUnresolvedHint(const AName: String): String;
    function DeriveTypeHintFromVariableName(const AName: String): String;
    function TryNameBasedEnumLookup(const AName: String;
      const ARawBytes: TBytes; out AFormatted: String): Boolean;
    function TryProbeClassPointer(const ARawBytes: TBytes;
      out AFormatted: String): Boolean;
    function AutoDetectFormatterName(const AName: String; AIsLocal: Boolean;
      AMatchedLocalTypeIdx: UInt32; const AMember: TRsmClassMember): String;
    // --- TD32-preferred dotted walk (§6.45 follow-up) -----------------
    /// Authoritative, convention-free dotted-walk over the TD32 reader's
    /// real member->type graph. Resolves the terminal field's live address
    /// (plus its TD32 type-index and inferred byte width) by following the
    /// genuine type-indices the .rsm cannot carry for cross-unit records.
    /// Returns False -- so the caller falls back to the RSM heuristic walk
    /// -- whenever the TD32 reader is absent or cannot resolve a hop
    /// authoritatively. Never raises.
    function TryTd32DottedWalk(const ASegments: TArray<String>;
      const ALocals: TArray<TLocalVar>; out AFieldAddr: Pointer;
      out ATermTypeIdx: UInt32; out ATermSize: Integer): Boolean;
    /// Auto-detect formatter name for a TD32 terminal type-index: a struct
    /// resolves to 'object' (class) / 'record', a scalar CodeView primitive
    /// to its formatter ('int'/'int64'/'single'/'double'/'extended'/'bool'),
    /// else '' (caller falls through to the explicit-type / hint paths).
    function Td32TerminalFormatterName(ATypeIdx: UInt32): String;
    /// True when a TD32 terminal type-index resolves to a class (so a nil
    /// value is a legitimate nil object reference, not inline record data).
    function Td32TerminalIsClass(ATypeIdx: UInt32): Boolean;
    property LocalsReader: TRsmReader read GetReader;
    property Td32Reader: TTd32Reader read GetTd32Reader;
    property TargetIs32Bit: Boolean read GetTargetIs32Bit;
    property TargetPointerSize: Integer read GetTargetPointerSize;
    property LastThreadHit: THandle read GetLastThreadHit;
    property BaseAddress: UIntPtr read GetBaseAddress;
    property CallResultSlot: UIntPtr read GetCallResultSlot;
    property Formatters: IKeyValue<String, TEvaluateFormatter> read GetFormatters;
    property PrimitiveTypeFormatters: IKeyValue<UInt16, String> read GetPrimitiveTypeFormatters;
    property LastEnumTypeId: UInt32 read GetLastEnumTypeId write SetLastEnumTypeId;
  public
    constructor Create(const AHost: IEvalProcessAccess);
    /// The whole evaluate operation (formerly TDebugger.EvaluateVariable):
    /// resolve AName (local / global / dotted path), then format the value
    /// (or auto-detect the type when AType is empty). Returns False with a
    /// recovery hint in AHint on failure.
    function Evaluate(const AName: String; var AType: String;
      out AValue, AHint: String; AOnPhase: TProc<String> = nil): Boolean;
  end;

implementation

constructor TVariableEvaluator.Create(const AHost: IEvalProcessAccess);
begin
  inherited Create;
  FHost := AHost;
end;

function TVariableEvaluator.GetReader: TRsmReader;
begin
  Result := FHost.LocalsReader;
end;

function TVariableEvaluator.GetTd32Reader: TTd32Reader;
begin
  Result := FHost.Td32Reader;
end;

function TVariableEvaluator.GetTargetIs32Bit: Boolean;
begin 
  Result := FHost.TargetIs32Bit; 
end;

function TVariableEvaluator.GetTargetPointerSize: Integer;
begin 
  Result := FHost.TargetPointerSize; 
end;

function TVariableEvaluator.GetLastThreadHit: THandle;
begin 
  Result := FHost.LastThreadHit; 
end;

function TVariableEvaluator.GetBaseAddress: UIntPtr;
begin 
  Result := FHost.BaseAddress; 
end;

function TVariableEvaluator.GetCallResultSlot: UIntPtr;
begin 
  Result := FHost.CallResultSlot; 
end;

function TVariableEvaluator.GetFormatters: IKeyValue<String, TEvaluateFormatter>;
begin 
  Result := FHost.Formatters; 
end;

function TVariableEvaluator.GetPrimitiveTypeFormatters: IKeyValue<UInt16, String>;
begin 
  Result := FHost.PrimitiveTypeFormatters; 
end;

function TVariableEvaluator.GetLastEnumTypeId: UInt32;
begin 
  Result := FHost.LastEnumTypeId; 
end;

procedure TVariableEvaluator.SetLastEnumTypeId(AValue: UInt32);
begin 
  FHost.LastEnumTypeId := AValue; 
end;

function TVariableEvaluator.ReadProcessMemory(AAddress: Pointer; ASize: NativeUInt): TBytes;
begin 
  Result := FHost.ReadProcessMemory(AAddress, ASize); 
end;

function TVariableEvaluator.ReadTargetPointer(AAddress: Pointer): UIntPtr;
begin 
  Result := FHost.ReadTargetPointer(AAddress); 
end;

function TVariableEvaluator.ReadClassNameFromVMT(AVMTPtr: UIntPtr; out AClassName: String): Boolean;
begin 
  Result := FHost.ReadClassNameFromVMT(AVMTPtr, AClassName); 
end;

function TVariableEvaluator.GetLocals(AThreadHandle: THandle): TArray<TLocalVar>;
begin 
  Result := FHost.GetLocals(AThreadHandle); 
end;

function TVariableEvaluator.GetLocalAddress(AThreadHandle: THandle; const AName: String;
  out AAddress: Pointer): Boolean;
begin 
  Result := FHost.GetLocalAddress(AThreadHandle, AName, AAddress); 
end;

function TVariableEvaluator.GetAddressFromSymbol(const ASymbolName: string): Pointer;
begin 
  Result := FHost.GetAddressFromSymbol(ASymbolName); 
end;

function TVariableEvaluator.TryResolveRttiProperty(AObjPtr: UIntPtr; const APropName: String;
  out AIsField: Boolean; out AFieldOffset: UInt32; out AGetterAddr: UIntPtr;
  out AIsStringResult: Boolean; out APrimIdHint: UInt16): Boolean;
begin 
  Result := FHost.TryResolveRttiProperty(AObjPtr, APropName, AIsField, AFieldOffset,
    AGetterAddr, AIsStringResult, APrimIdHint); 
end;

function TVariableEvaluator.TryRecoverReferenceType(ASlotPtr: UIntPtr; out ADesc: String): Boolean;
begin 
  Result := FHost.TryRecoverReferenceType(ASlotPtr, ADesc); 
end;

function TVariableEvaluator.CallTargetFunction(AGetterAddr, ASelf: UIntPtr;
  AStringResult: Boolean; out AOrdinal: UInt64): Boolean;
begin 
  Result := FHost.CallTargetFunction(AGetterAddr, ASelf, AStringResult, AOrdinal); 
end;

function TVariableEvaluator.TryGetRecordTerminalName(const AName: String; AIsLocal: Boolean;
  AMatchedLocalTypeIdx: UInt32; out ARecTypeName: String; out AClsIdx: Integer): Boolean;
begin 
  Result := FHost.TryGetRecordTerminalName(AName, AIsLocal, AMatchedLocalTypeIdx,
  ARecTypeName, AClsIdx); 
end;

// VMT walk: given a pointer that's expected to point at a class
// instance, follow the VMT to read the runtime class name. Returns
// False on nil pointer or any unreadable indirection.
function TVariableEvaluator.ReadRuntimeClassName(APtrVal: UIntPtr; out AClassName: String): Boolean;
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

// Resolve one VMT's parent VMT + class name. Drives the same live-VMT
// ancestor chain for BOTH the field walk (FindFieldViaVmtWalk) and the
// property walk (FindPropertyViaVmtWalk).
//
// Try ReadTargetPointer at <VMTPtr - cand> for the candidate offset;
// a non-zero pointer whose own VMT-classname slot resolves to a valid
// class name is taken as vmtParent. Delphi's layout puts vmtParent
// 8 bytes (Win32) / 16 bytes (Win64) before vmtClassName, but
// CPP_ABI_ADJUST and version-specific RTL additions shift the exact
// offset, so we probe both the direct and the indirected form.
function TVariableEvaluator.TryReadParentVMT(AVMT: UIntPtr; out AParentVMT: UIntPtr;
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
  if TargetIs32Bit then
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

// Live ancestor walk: when FindClassMember can't find AFieldName in
// AStartClassName's RSM-derived chain, follow the runtime VMT's
// vmtParent slot up through each ancestor and try the lookup again
// on each parent's class name. Authoritative because the chain is
// exactly what the Delphi runtime uses for class-name reporting --
// no offset heuristics, no forward stubs. Stops when the field is
// located, when the parent VMT is nil (TObject reached), or after a
// defensive depth cap.
function TVariableEvaluator.FindFieldViaVmtWalk(AObjPtr: UIntPtr; const AFieldName: String;
  out AMember: TRsmClassMember): Boolean;
const
  MaxDepth = 32;
var
  VMTPtr     : UIntPtr;
  ParentVMT  : UIntPtr;
  ParentName : String;
  Depth      : Integer;
begin
  Result := False;
  AMember := Default(TRsmClassMember);
  if AObjPtr = 0 then Exit;
  if not Assigned(LocalsReader) then Exit;
  VMTPtr := ReadTargetPointer(Pointer(AObjPtr));
  if VMTPtr = 0 then Exit;
  Depth := 0;
  while (VMTPtr <> 0) and (Depth < MaxDepth) do
  begin
    if not TryReadParentVMT(VMTPtr, ParentVMT, ParentName) then
      Exit; // TObject reached or unresolvable
    if LocalsReader.FindClassMember(ParentName, AFieldName, AMember) then
      Exit(True);
    VMTPtr := ParentVMT;
    Inc(Depth);
  end;
end;

// Property analog of FindFieldViaVmtWalk. FindClassProperty alone walks
// only the RSM ParentName chain, which for RTL/VCL ancestors (e.g.
// TControl, where Caption is declared) is often not linked all the way
// up -- so an inherited property misses even though the inherited FIELD
// path already resolves via the live VMT. Mirror that: try
// FindClassProperty on the start class first, then on each live-VMT
// ancestor's class name. This is what lets TControl.Caption resolve on
// a TFormAd instance, the same way FindFieldViaVmtWalk resolves the
// inherited field TControl.FText.
function TVariableEvaluator.FindPropertyViaVmtWalk(AObjPtr: UIntPtr;
  const AStartClassName, APropName: String;
  out AProp: TRsmClassProperty; out AOwnerClass: String): Boolean;
const
  MaxDepth = 32;
var
  VMTPtr, ParentVMT: UIntPtr;
  ParentName       : String;
  Depth            : Integer;
begin
  Result := False;
  AProp := Default(TRsmClassProperty);
  AOwnerClass := '';
  if not Assigned(LocalsReader) then Exit;
  if LocalsReader.FindClassProperty(AStartClassName, APropName, AProp, AOwnerClass) then
    Exit(True);
  if AObjPtr = 0 then Exit;
  VMTPtr := ReadTargetPointer(Pointer(AObjPtr));
  if VMTPtr = 0 then Exit;
  Depth := 0;
  while (VMTPtr <> 0) and (Depth < MaxDepth) do
  begin
    if not TryReadParentVMT(VMTPtr, ParentVMT, ParentName) then
      Exit; // TObject reached or unresolvable
    if LocalsReader.FindClassProperty(ParentName, APropName, AProp, AOwnerClass) then
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
function TVariableEvaluator.TerminalMemberResolvesToClass(ATypeIdx: UInt32): Boolean;
var
  ClsIdx: Integer;
begin
  Result := False;
  if (ATypeIdx = 0) or (not Assigned(LocalsReader)) then Exit;
  ClsIdx := LocalsReader.FindStructByTypeIdx(ATypeIdx);
  Result := (ClsIdx >= 0) and
            (LocalsReader.Classes[ClsIdx].Kind = skClass);
end;

// === TD32-preferred dotted walk (§6.45 follow-up) ====================
//
// When a .tds / embedded TD32 is present (Td32Reader <> nil), this walk
// resolves a dotted chain through the AUTHORITATIVE TD32 member->type graph
// -- a real 16-bit type-index on every member -- instead of the .rsm's
// heuristic name conventions (§6.36/§6.41/§6.43/§6.44). It is therefore
// project-independent: it never guesses a record from a T<X> name.
//
// SCOPE -- PURE INLINE-RECORD chains only. TD32 owns navigation that is
// nothing but record offsets: a record or pointer-to-record ROOT
// (e.g. Computer: PComputer = ^TComputer), then record-typed hops (inline
// sub-records, or pointer-to-record fields like FAd: PAd). That is exactly
// the cross-unit nested-record edge the .rsm cannot carry (§6.45) and where
// there is no VMT, spill home or inheritance to reason about. The instant a
// hop would touch a CLASS instance (a class root such as Self / a
// register-passed object param, or a class-typed field), the walk DECLINES
// (Exit False) so the mature RSM walk handles it -- that path already owns
// the §6.30/§6.35/§6.40 register/spill-home/VMT machinery this short,
// authoritative path deliberately does not duplicate. Frame/local ADDRESS
// resolution likewise stays RSM-driven (GetLocalAddress); only the
// member->type navigation is TD32's. Also declines for an unknown member, a
// global first segment (no .tds BPREL record), or a nil pointer mid-chain.
function TVariableEvaluator.TryTd32DottedWalk(const ASegments: TArray<String>;
  const ALocals: TArray<TLocalVar>; out AFieldAddr: Pointer;
  out ATermTypeIdx: UInt32; out ATermSize: Integer): Boolean;

  // Byte width of member AMi within struct AStructIdx, inferred from the
  // gap to the next member's offset (0 when last -> caller uses the
  // requested type's width). Mirrors TRsmClassMember.Size semantics so the
  // int/int64 formatter can clamp a sub-DWORD terminal.
  function MemberSize(AStructIdx, AMi: Integer): Integer;
  var
    Members: IList<TTd32ClassMember>;
  begin
    Result := 0;
    Members := Td32Reader.Classes[AStructIdx].Members;
    if AMi + 1 < Members.Count then
      Result := Integer(Members[AMi + 1].Offset) - Integer(Members[AMi].Offset);
    if Result < 0 then Result := 0;
  end;

  // True iff ATypeIdx is a known TD32 struct AND that struct is a RECORD
  // (not a class). This is the pure-record policy gate: a class struct
  // (AIdx >= 0 but Kind = tkClass) returns False so the walk declines.
  function IsRecordStruct(ATypeIdx: UInt32; out AIdx: Integer): Boolean;
  begin
    AIdx := Td32Reader.FindStructByTypeIdx(ATypeIdx);
    Result := (AIdx >= 0) and (Td32Reader.Classes[AIdx].Kind = tkRecord);
  end;

var
  Td32      : TTd32Reader;
  Seg0      : String;
  Li, Si, Mi: Integer;
  HasRegInst: Boolean;
  RegInst   : UIntPtr;
  SlotAddr  : Pointer;
  Seg0Type  : UInt32;
  CurIdx    : Integer;
  CurAddr   : UIntPtr;
  Tgt       : UInt32;
  PtrVal    : UIntPtr;
  M         : TTd32ClassMember;
  FoundMi   : Integer;
  FieldAddr : UIntPtr;
  NextIdx   : Integer;
begin
  Result := False;
  AFieldAddr := nil;
  ATermTypeIdx := 0;
  ATermSize := 0;
  Td32 := Td32Reader;
  if (Td32 = nil) or (Length(ASegments) < 2) then Exit;
  Seg0 := ASegments[0];

  // --- First segment: establish a RECORD base (CurAddr, CurIdx). ---
  HasRegInst := False;
  RegInst    := 0;
  SlotAddr   := nil;
  for Li := 0 to High(ALocals) do
    if SameText(ALocals[Li].Name, Seg0) then
    begin
      if (ALocals[Li].Kind = lkRegister) and
         (Length(ALocals[Li].RawBytes) >= TargetPointerSize) then
      begin
        Move(ALocals[Li].RawBytes[0], RegInst, TargetPointerSize);
        HasRegInst := True;
      end;
      Break;
    end;

  // The first segment's authoritative declared type comes from the .tds
  // BPREL record. A global has no such record here -> decline -> RSM.
  if not FHost.TryGetTd32FrameLocalTypeIdx(LastThreadHit, Seg0, Seg0Type) then
    Exit;
  if Seg0Type = 0 then Exit;
  if not HasRegInst then
    if not GetLocalAddress(LastThreadHit, Seg0, SlotAddr) then
      Exit;

  if IsRecordStruct(Seg0Type, CurIdx) then
  begin
    // inline record local: the slot address IS the record base.
    if HasRegInst then Exit;  // register-resident inline record: not addressable
    CurAddr := UIntPtr(SlotAddr);
  end
  else if Td32.TryGetPointerTarget(Seg0Type, Tgt) and IsRecordStruct(Tgt, CurIdx) then
  begin
    // pointer-to-record local (e.g. Computer: PComputer): deref the slot
    // (or the register value) to the pointed-at record's base.
    if HasRegInst then PtrVal := RegInst
    else PtrVal := ReadTargetPointer(SlotAddr);
    if PtrVal = 0 then Exit;   // nil pointer first segment
    CurAddr := PtrVal;
  end
  else
    // class root, pointer-to-class root, or unknown type -> RSM owns it.
    Exit;
  if CurAddr = 0 then Exit;

  // --- Walk the remaining segments (records only). ---
  for Si := 1 to High(ASegments) do
  begin
    // resolve the member by name in the current record
    FoundMi := -1;
    for Mi := 0 to Td32.Classes[CurIdx].Members.Count - 1 do
      if SameText(Td32.Classes[CurIdx].Members[Mi].Name, ASegments[Si]) then
      begin
        FoundMi := Mi;
        Break;
      end;
    if FoundMi < 0 then Exit;
    M := Td32.Classes[CurIdx].Members[FoundMi];
    // record members carry absolute offsets (no VMT slot to skip).
    FieldAddr := CurAddr + M.Offset;

    if Si = High(ASegments) then
    begin
      // terminal: hand back the field address + type for formatting
      AFieldAddr   := Pointer(FieldAddr);
      ATermTypeIdx := M.TypeIdx;
      ATermSize    := MemberSize(CurIdx, FoundMi);
      Exit(True);
    end;

    // non-terminal: advance into the member's type, RECORDS ONLY.
    if IsRecordStruct(M.TypeIdx, NextIdx) then
    begin
      // inline record field: its address IS the nested record's base.
      CurAddr := FieldAddr;
      CurIdx  := NextIdx;
    end
    else if Td32.TryGetPointerTarget(M.TypeIdx, Tgt) and IsRecordStruct(Tgt, NextIdx) then
    begin
      // pointer-to-record field (e.g. FAd: PAd): deref to the target record.
      PtrVal := ReadTargetPointer(Pointer(FieldAddr));
      if PtrVal = 0 then Exit;
      CurAddr := PtrVal;
      CurIdx  := NextIdx;
    end
    else
      // a class-typed (or unknown) intermediate field -> RSM owns it.
      Exit;
  end;
end;

function TVariableEvaluator.Td32TerminalFormatterName(ATypeIdx: UInt32): String;
// Maps a TD32 terminal type-index to an auto-detect formatter name.
// Structured types resolve to 'object' (class) / 'record'; scalar CodeView
// primitives map to their formatter. Both the signed and unsigned variant
// of each width map to the same formatter (Delphi's Integer/Cardinal,
// Int64/UInt64 share one), and both the CV "basic" ($12/$13/$22/$23) and
// "extended-int" ($74..$77) encodings are covered so the result holds
// regardless of which the toolchain emits. String/char/array terminals
// (e.g. a ShortString[40]) fall through to '' on purpose -- the caller
// resolves those via an explicit type= like the §6.43/§6.44 residual.
var
  Idx: Integer;
begin
  Result := '';
  if (ATypeIdx = 0) or (Td32Reader = nil) then Exit;
  Idx := Td32Reader.FindStructByTypeIdx(ATypeIdx);
  if Idx >= 0 then
  begin
    if Td32Reader.Classes[Idx].Kind = tkClass then
      Result := 'object'
    else
      Result := 'record';
    Exit;
  end;
  // CodeView primitive type-indices (< $1000).
  case ATypeIdx of
    $0010, $0011, $0012, $0068, $0072, $0074,   // signed 8/16/32-bit ints
    $0020, $0021, $0022, $0069, $0073, $0075:   // unsigned 8/16/32-bit ints
      Result := 'int';
    $0013, $0023, $0076, $0077:                 // 64-bit ints (signed/unsigned)
      Result := 'int64';
    $0040: Result := 'single';
    $0041: Result := 'double';
    $0042: Result := 'extended';
    $0030: Result := 'bool';
  end;
end;

function TVariableEvaluator.Td32TerminalIsClass(ATypeIdx: UInt32): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  if (ATypeIdx = 0) or (Td32Reader = nil) then Exit;
  Idx := Td32Reader.FindStructByTypeIdx(ATypeIdx);
  Result := (Idx >= 0) and (Td32Reader.Classes[Idx].Kind = tkClass);
end;

function TVariableEvaluator.Evaluate(const AName: String; var AType: String;
  out AValue, AHint: String;
  AOnPhase: TProc<String>): Boolean;

  procedure Phase(const APhase: String);
  begin
    if Assigned(AOnPhase) then
      AOnPhase(APhase);
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
  LastEnumTypeId := 0;
  Phase('begin');

  if LastThreadHit = 0 then Exit;

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
  Locals := GetLocals(LastThreadHit);
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
    // Provisional recovery hint: the walk has many early Exit points
    // (field not found, nil mid-chain, record context not establishable),
    // any of which leaves the call a failure. Seed the dotted-navigation
    // hint up front so every one of those exits carries it; it is only
    // ever surfaced on failure (the MCP layer reads AHint only when the
    // call returns False) and is overridden by the auto-detect hint when
    // the walk succeeds but typing declines.
    AHint := BuildUnresolvedHint(AName);
    // Path 2: dotted field-chain walk. The first segment must resolve
    // to an instance pointer (a local or global object reference); each
    // subsequent segment dereferences the current object, looks up the
    // named field via RSM class info, and advances to that field's
    // address. The final segment's address is then read with the
    // user-specified type. Requires RSM class layout (LoadDebugInfoFromExe).
    Segments := AName.Split(['.']);
    // §6.45 follow-up: try the AUTHORITATIVE TD32 walk FIRST when a .tds /
    // embedded TD32 is loaded. It resolves cross-unit nested-record
    // member->type edges convention-free (the .rsm structurally cannot --
    // §6.45), so it is PREFERRED; the RSM heuristic walk below runs only
    // when TD32 is absent or declines. RSM remains the locals/frame source
    // either way (both sidecars coexist).
    var Td32Resolved: Boolean := False;
    if (Length(Segments) >= 2) and Assigned(Td32Reader) then
    begin
      var TdFieldAddr: Pointer;
      var TdTermType : UInt32;
      var TdTermSize : Integer;
      if TryTd32DottedWalk(Segments, Locals, TdFieldAddr, TdTermType, TdTermSize) then
      begin
        // Accept the TD32 result only when its terminal can be FORMATTED
        // here: an explicit type= was given, or the terminal auto-detects
        // to a TD32 formatter (scalar primitive / struct). A BARE evaluate
        // whose terminal TD32 cannot type (an enum / string / set -- TD32
        // does not decode those) defers to the RSM walk, whose auto-detect
        // can still name an enum. The navigation TD32 just performed was
        // authoritative either way; this gate only governs which path
        // FORMATS the already-located value, so no convention-free win is
        // lost (the address is identical; RSM only adds the type name).
        var TdTermFmt: String := '';
        if AType = '' then
          TdTermFmt := Td32TerminalFormatterName(TdTermType);
        if (AType <> '') or (TdTermFmt <> '') then
        begin
          Td32Resolved   := True;
          Addr           := TdFieldAddr;
          RawBytes       := ReadProcessMemory(TdFieldAddr, 8);
          FieldKnownSize := TdTermSize;
          // A TD32 terminal reached inside a record is inline data, never a
          // nil-able class slot -- feeds the §6.20 nil-refusal discrimination.
          DottedTerminalIsRecordField := not Td32TerminalIsClass(TdTermType);
          if AType = '' then
            AType := TdTermFmt;
          Phase('td32 dotted-walk resolved');
        end;
      end;
    end;
    var FirstHopHasInstancePtr: Boolean := False;
    var FirstHopInstancePtr   : UIntPtr := 0;
    if (not Td32Resolved) and (Length(Segments) >= 2) and Assigned(LocalsReader) then
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
      var FirstSegIsLocal  : Boolean := False;
      for I := 0 to High(Locals) do
        if SameText(Locals[I].Name, Segments[0]) then
        begin
          FirstLocalTypeIdx := Locals[I].TypeIdx;
          FirstSegIsLocal   := True;
          if (Locals[I].Kind = lkRegister) and
             (Length(Locals[I].RawBytes) >= TargetPointerSize) then
          begin
            Move(Locals[I].RawBytes[0], FirstHopInstancePtr, TargetPointerSize);
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
        if not GetLocalAddress(LastThreadHit, Segments[0], Addr) then
          Addr := GetAddressFromSymbol(Segments[0]);
      if not Assigned(Addr) then Exit;

      // Step 2: walk the dotted segments. The state we carry between
      // iterations is FieldAddr (where the next field's bytes will
      // be read) and PrevContextIdx (the index in LocalsReader's
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
           (LocalsReader.FindClassByName(VmtClsName) >= 0) then
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
          GStructIdx := LocalsReader.FindClassIdxByRsmTypeId(FirstLocalTypeIdx);
        // §6.41: discard a MIS-resolved alias. A record var-local's
        // per-proc 2-byte ref (§4.2 design limit) is NOT a registry
        // primary, so FindClassIdxByRsmTypeId can collide with an
        // unrelated registry record (KonsMis's $02E5 -> the unrelated
        // record TLogSharing). If the resolved record does not declare
        // the accessed field Segments[1], it is the wrong type -- drop
        // it so the routes below (the T<localName> bridge, then the
        // §6.36 field-name fallback) get a chance to recover the real
        // record. Gated to skRecord: a class alias is handled by the
        // class-hop/VMT path, not here.
        if (GStructIdx >= 0) and FirstSegIsLocal and (High(Segments) >= 1) and
           (LocalsReader.Classes[GStructIdx].Kind = skRecord) then
        begin
          var AliasM: TRsmClassMember;
          if not LocalsReader.FindClassMember(
               LocalsReader.Classes[GStructIdx].Name, Segments[1], AliasM) then
            GStructIdx := -1;
        end;
        if GStructIdx < 0 then
        begin
          var GTypeIdx: UInt32 := LocalsReader.FindGlobalTypeIdx(Segments[0]);
          if GTypeIdx <> 0 then
            GStructIdx := LocalsReader.FindClassIdxByRsmTypeId(GTypeIdx);
          // §6.42: re-apply the §6.41 discard to the GLOBAL-resolved alias.
          // A $20 stack-local also republishes (name -> per-proc id) into
          // the global map (§4.4), and that id can collide with an
          // unrelated registry record exactly like the FirstLocalTypeIdx
          // path guarded above -- TFW's Computer local republishes $F822,
          // which FindClassIdxByRsmTypeId maps to the unrelated record
          // TKlkKons (declares neither OS nor Name). Without this guard the
          // colliding record (>=0) pre-empts BOTH the
          // FindBestRecordForGlobalAndField proximity/T<global> recovery
          // (which resolves the correct TComputer) AND the T<localName>
          // bridge below, so the walk primes the wrong record and
          // Computer.OS.Name returns "Could not navigate". Mirror the §6.41
          // guard: a record alias that does not declare the accessed field
          // is the wrong type -- drop it so the recoveries run.
          if (GStructIdx >= 0) and (High(Segments) >= 1) and
             (LocalsReader.Classes[GStructIdx].Kind = skRecord) then
          begin
            var GAliasM: TRsmClassMember;
            if not LocalsReader.FindClassMember(
                 LocalsReader.Classes[GStructIdx].Name, Segments[1], GAliasM) then
              GStructIdx := -1;
          end;
          if GStructIdx < 0 then
            GStructIdx := LocalsReader.FindBestRecordForGlobalAndField(
              Segments[0], Segments[1]);
          if (GStructIdx < 0) and FirstSegIsLocal and (High(Segments) >= 1) then
          begin
            // §6.41 first hop: a pointer-to-record var-local whose
            // per-proc alias (§4.2) doesn't key the registry binds to
            // its record by the P<X>/T<X> naming convention -- a local
            // named X of type PX (= ^TX) resolves to the record TX. This
            // is tried BEFORE the §6.36 field-name fallback because it is
            // far less ambiguous: TFW's KonsMis -> the unique TKonsMis,
            // versus 321 records that all declare a "Name" field. Gated
            // to a discovered skRecord that actually declares the
            // accessed field; the live-pointer-deref probe below is the
            // structural cross-check that the slot really points at that
            // record. Class-typed locals never reach here -- the §6.32
            // VMT-priority override already set SkipRecordPriming.
            var TName: String := 'T' + Segments[0];
            var TIdx : Integer := LocalsReader.FindClassByName(TName);
            var ConvM: TRsmClassMember;
            if (TIdx >= 0) and
               (LocalsReader.Classes[TIdx].Kind = skRecord) and
               LocalsReader.FindClassMember(TName, Segments[1], ConvM) then
              GStructIdx := TIdx
            else
            begin
              // §6.36 first hop: a cross-unit record LOCAL carries
              // TypeIdx = 0 (no per-proc type id at all -- e.g.
              // <AdrLoc: DebugTarget.RecTypes.TXAdresse>), so neither
              // the registry-id route nor the global-proximity fallback
              // (locals have no $20 file offset) can find its record.
              // Recover by the accessed field name: if exactly one
              // skRecord declares Segments[1], that record IS the local's
              // type. The unique-match guard (same as §6.18/§6.19) keeps
              // a class-instance local whose VMT probe failed from being
              // mis-primed as a record on an ambiguous field name.
              var RecHits := LocalsReader.FindRecordsByMemberName(Segments[1]);
              if Length(RecHits) = 1 then
                GStructIdx := RecHits[0];
            end;
          end;
        end;
        if (GStructIdx >= 0) and
           (LocalsReader.Classes[GStructIdx].Kind = skRecord) then
        begin
          PrevContextIdx := GStructIdx;
          ContextIsRecord := True;
          // First-segment pointer-to-record deref. The priming above
          // resolves the record TYPE, but for a typed-pointer first
          // segment (e.g. <CxPtr: PComplexRec := @CxLoc>) the slot holds
          // a POINTER to the record, not the record inline -- so the
          // record-hop base must be *Addr, not Addr. A cross-unit pointer
          // local carries an unreliable per-proc type id (§4.2) -- its id
          // does NOT key into the type registry (CxPtr's $B1 is not
          // PComplexRec's registry id $8E80) -- so there is no type-based
          // "this is a pointer" signal; probe at runtime instead: if the
          // slot contents (*Addr = ProbeInstancePtr) form an address from
          // which the ENTIRE record reads back, the slot is a pointer --
          // deref it. An inline record's first-field bytes (CxLoc.CxR1
          // .C1Int = $C1C1C1C1, or AdrLoc.Name's shortstring length+chars)
          // are virtually never a readable record-sized region, so the
          // inline case keeps Addr as the base.
          //
          // Gated to FirstSegIsLocal: a record-typed GLOBAL is always
          // inline (its VA IS the record base), and some such globals have
          // a live pointer as their FIRST field (e.g. GGlobalPrim.FAnsi,
          // an AnsiString) whose readable target would otherwise trip the
          // probe. Locals are where pointer-to-record first segments occur
          // and where the inline first-field bytes are reliably non-address
          // sentinels. Register-passed first hops already hold the instance
          // pointer, so skip them.
          if FirstSegIsLocal and (not FirstHopHasInstancePtr) and
             (ProbeInstancePtr <> 0) then
          begin
            var RecExtent: NativeUInt := 0;
            for var Rm: Integer := 0 to
                LocalsReader.Classes[GStructIdx].Members.Count - 1 do
            begin
              var E: NativeUInt :=
                NativeUInt(LocalsReader.Classes[GStructIdx].Members[Rm].Offset) +
                LocalsReader.Classes[GStructIdx].Members[Rm].Size;
              if E > RecExtent then RecExtent := E;
            end;
            if (RecExtent > 0) and (Length(ReadProcessMemory(
                 Pointer(ProbeInstancePtr), RecExtent)) = Integer(RecExtent)) then
              FieldAddr := Pointer(ProbeInstancePtr);
          end;
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
          for var Mi: Integer := 0 to LocalsReader.Classes[PrevContextIdx].Members.Count - 1 do
            if SameText(LocalsReader.Classes[PrevContextIdx].Members[Mi].Name, Segments[I]) then
            begin
              Member := LocalsReader.Classes[PrevContextIdx].Members[Mi];
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
            if not LocalsReader.FindClassMember(ClsName, Segments[I], Member) then
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
              begin
                // §6.37 round 4 / §6.38 -- LIVE RTTI first. The
                // authoritative source for a PUBLISHED or PUBLIC property is
                // the running instance's runtime RTTI (VMT -> vmtTypeInfo ->
                // published PropData table AND the extended-RTTI PropDataEx
                // table), exactly what the Delphi IDE uses for Caption-style
                // (published) and IniName-style (public) properties. This is
                // collision-proof on large binaries where the RSM Format-A
                // $31/$2C/$2E id cross-references are unreliable (§6.37/§6.38).
                // Field-backed properties yield a field offset; getter-backed
                // ones yield the getter's code address, which we call-inject.
                var RIsField    : Boolean;
                var RFieldOfs   : UInt32;
                var RGetterAddr : UIntPtr;
                var RIsStr      : Boolean;
                var RPrimId     : UInt16;
                if TryResolveRttiProperty(ObjPtr, Segments[I], RIsField,
                     RFieldOfs, RGetterAddr, RIsStr, RPrimId) then
                begin
                  if RIsField then
                  begin
                    Member := Default(TRsmClassMember);
                    Member.PrimitiveTypeId := RPrimId;
                    FieldAddr := Pointer(ObjPtr + RFieldOfs);
                    PrevContextIdx  := -1;
                    ContextIsRecord := False;
                    Continue;
                  end
                  else if RGetterAddr <> 0 then
                  begin
                    var ROrd: UInt64;
                    if CallTargetFunction(RGetterAddr, ObjPtr, RIsStr, ROrd) then
                    begin
                      FieldAddr := Pointer(CallResultSlot);
                      Member := Default(TRsmClassMember);
                      Member.PrimitiveTypeId := RPrimId;
                      PrevContextIdx  := -1;
                      ContextIsRecord := False;
                      Continue;
                    end;
                  end;
                end;
                // §6.37 PROPERTY fallback (RSM). The segment is neither an
                // own field nor an inherited field on the runtime class
                // chain, and not a published property -- try it as a
                // property parsed from the $31 records into
                // TRsmClassInfo.Properties (§4.16). Reliable on small
                // binaries; on large binaries the id cross-refs collide
                // (§6.37) so the live-RTTI path above is preferred.
                //   * Field-backed (read FField): bridge to the
                //     underlying field's byte offset and continue the
                //     walk exactly as if the user had named the field.
                //     Covers the easy, deterministic case.
                //   * Getter-backed (read GetX): the value only exists
                //     after running the accessor in the target process,
                //     so there is nothing to read statically -- emit a
                //     precise hint instead of the generic "could not
                //     navigate" failure. (The canonical example is a VCL
                //     Caption, whose FText backing field is empty because
                //     the text lives in native window state; only call
                //     injection -- not yet supported -- can recover it.)
                var Prop: TRsmClassProperty;
                var OwnerClass: String;
                if not FindPropertyViaVmtWalk(ObjPtr, ClsName, Segments[I],
                         Prop, OwnerClass) then
                  Exit;
                if Prop.UnderlyingField = '' then
                begin
                  // Getter-backed: the value only exists once the accessor
                  // method runs. Resolve the getter's code address
                  // (declared on OwnerClass, e.g. TControl.GetText for a
                  // VCL Caption) and CALL-INJECT it on the paused thread.
                  // The result is deposited in the target-side scratch
                  // slot (the ordinal value, or -- for a managed string
                  // return -- the string pointer the getter wrote via the
                  // hidden @Result arg); point the terminal read at that
                  // slot so the normal formatter pipeline handles it.
                  var GetterAddr: UIntPtr := 0;
                  if (Prop.GetterName <> '') and (OwnerClass <> '') then
                  begin
                    var ProcIdx: Integer :=
                      LocalsReader.FindProcByName(OwnerClass + '.' + Prop.GetterName);
                    if ProcIdx >= 0 then
                      GetterAddr := BaseAddress + $1000 +
                        LocalsReader.Procs[ProcIdx].SegmentOffset;
                  end;
                  var IsStrResult: Boolean := Prop.PrimitiveTypeId = $04; // UnicodeString
                  var GetterOrdinal: UInt64 := 0;
                  if (GetterAddr <> 0) and
                     CallTargetFunction(GetterAddr, ObjPtr, IsStrResult, GetterOrdinal) then
                  begin
                    FieldAddr := Pointer(CallResultSlot);
                    Member := Default(TRsmClassMember);
                    Member.PrimitiveTypeId := Prop.PrimitiveTypeId;
                    // Property value is terminal data, not a navigable
                    // class/record context: reset the hop state so a
                    // following segment (object-returning getter) class-
                    // hops by dereferencing the slot.
                    PrevContextIdx  := -1;
                    ContextIsRecord := False;
                    Continue;
                  end;
                  // Getter not resolvable / call failed -- precise hint.
                  var GetterDisplay: String := Prop.GetterName;
                  if GetterDisplay = '' then GetterDisplay := '<unknown>';
                  AHint := Format(
                    'Property "%s" on %s is getter-backed (read accessor ' +
                    '%s): its value is computed at runtime. DPT resolves ' +
                    'this by calling the getter on the paused thread, but ' +
                    'that could not be completed here (getter address not ' +
                    'resolved or the call did not return). A naive backing ' +
                    'field (e.g. FText for a VCL Caption) is typically ' +
                    'empty because the value lives in native window/runtime ' +
                    'state, not the field.',
                    [Segments[I], ClsName, GetterDisplay]);
                  Exit;
                end;
                // Field-backed: resolve the underlying field by name on
                // the same runtime/RSM class chain, then fall through to
                // the FieldAddr advance below as if it had been named.
                if not LocalsReader.FindClassMember(
                         ClsName, Prop.UnderlyingField, Member) then
                  if not FindFieldViaVmtWalk(
                           ObjPtr, Prop.UnderlyingField, Member) then
                    Exit;
              end;
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
            for var FbCi: Integer := 0 to LocalsReader.Classes.Count - 1 do
            begin
              if LocalsReader.Classes[FbCi].Kind <> skRecord then Continue;
              for var FbMi: Integer := 0 to LocalsReader.Classes[FbCi].Members.Count - 1 do
                if SameText(LocalsReader.Classes[FbCi].Members[FbMi].Name,
                            Segments[I]) then
                begin
                  if FbMatchCount = 0 then
                  begin
                    FbFoundIdx := FbCi;
                    Member := LocalsReader.Classes[FbCi].Members[FbMi];
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
           (LocalsReader.FindStructByTypeIdx(Member.PointerTargetTypeIdx) >= 0) then
        begin
          var PtrVal: UIntPtr := ReadTargetPointer(FieldAddr);
          if PtrVal = 0 then Exit;
          FieldAddr := Pointer(PtrVal);
          PrevContextIdx := LocalsReader.FindStructByTypeIdx(Member.PointerTargetTypeIdx);
          ContextIsRecord := True;
        end
        else if (Member.TypeIdx <> 0) and
                (LocalsReader.FindStructByTypeIdx(Member.TypeIdx) >= 0) then
        begin
          PrevContextIdx := LocalsReader.FindStructByTypeIdx(Member.TypeIdx);
          ContextIsRecord :=
            LocalsReader.Classes[PrevContextIdx].Kind = skRecord;
        end
        else if ContextIsRecord and (I < High(Segments)) and
                (Member.TypeIdx = 0) and (Member.PointerTargetTypeIdx = 0) and
                (Member.Size > 0) then
        begin
          // §6.36 nested-record bridge. The just-resolved member is a
          // field WITHIN a record (this segment was a record-hop) that
          // carries no type id (§4.14 record-field-id-is-zero) yet the
          // walk continues -- so the member must itself be a nested
          // record (e.g. <TXAdresse.Anschrift: TXAnschrift>). Its Size
          // equals the nested record's total byte size; combined with
          // the next segment's field name FindRecordBySizeAndMemberName
          // resolves the nested record uniquely. FieldAddr already
          // points inline at the nested record's base (the record-hop
          // advanced it by Member.Offset, no deref), so only the
          // context needs priming for the next record-hop iteration.
          var NestedIdx: Integer :=
            LocalsReader.FindRecordBySizeAndMemberName(
              Member.Size, Segments[I + 1]);
          // §6.43 + §6.44: the size+next-field bridge above is AMBIGUOUS
          // (returns -1) when several same-size records declare the next
          // field -- common leaf names like "Name" (321 records in TFW carry
          // it) or "Version". That made Computer.OS.Name / Computer.GPU.Name
          // and User.RecHeader.Version fail while Computer.OS.MajorVersion (a
          // rare leaf -> unique) worked. The nested member carries no
          // resolvable type id at all (its $XXXX keys neither the registry
          // nor a struct offset -- the §4.2 design limit), so the only
          // leaf-name-independent signal is the Delphi NAME convention. Two
          // shapes, tried in order of specificity:
          //   §6.43  T<outerStem><member> -- a sub-record namespaced under
          //          its owner (TComputer.OS : TComputerOS, .CPU/.GPU/.DB).
          //   §6.44  T<member>            -- a SHARED record whose name IS
          //          the member name (TUser.RecHeader : TRecHeader,
          //          .RecChanges : TRecChanges); the same T<X> family as
          //          §6.19/§6.41, applied to a nested record member.
          // Each candidate is accepted only if it is a discovered skRecord
          // whose layout extent fits the member's parent slot (the same
          // <8-byte alignment tolerance FindRecordBySizeAndMemberName uses;
          // the parent may pad the slot to align the next field) AND declares
          // the next segment -- so a convention miss can never mis-prime, and
          // the more-specific §6.43 form wins when both happen to resolve.
          if NestedIdx < 0 then
          begin
            var OuterNm: String := LocalsReader.Classes[PrevContextIdx].Name;
            var Cands: array[0..1] of String;
            var NCand: Integer := 0;
            if (Length(OuterNm) >= 2) and (UpCase(OuterNm[1]) = 'T') then
            begin
              Cands[NCand] := 'T' + Copy(OuterNm, 2, MaxInt) + Member.Name;  // §6.43
              Inc(NCand);
            end;
            Cands[NCand] := 'T' + Member.Name;                               // §6.44
            Inc(NCand);
            for var Ci := 0 to NCand - 1 do
            begin
              var ConvIdx: Integer := LocalsReader.FindClassByName(Cands[Ci]);
              var ConvFld: TRsmClassMember;
              if (ConvIdx < 0) or
                 (LocalsReader.Classes[ConvIdx].Kind <> skRecord) or
                 not LocalsReader.FindClassMember(Cands[Ci], Segments[I + 1], ConvFld) then
                Continue;
              // Size leakage guard: the convention-named record's layout
              // extent must fit the member's parent slot.
              var Ext: UInt32 := 0;
              for var Cm := 0 to LocalsReader.Classes[ConvIdx].Members.Count - 1 do
              begin
                var E: UInt32 := LocalsReader.Classes[ConvIdx].Members[Cm].Offset +
                                 LocalsReader.Classes[ConvIdx].Members[Cm].Size;
                if E > Ext then Ext := E;
              end;
              if (Ext <= Member.Size) and (Member.Size - Ext < 8) then
              begin
                NestedIdx := ConvIdx;
                Break;
              end;
            end;
          end;
          if NestedIdx >= 0 then
          begin
            PrevContextIdx  := NestedIdx;
            ContextIsRecord := True;
          end
          else
          begin
            PrevContextIdx  := -1;
            ContextIsRecord := False;
          end;
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


  if Length(RawBytes) = 0 then
  begin
    // Nothing resolved: unknown whole name, or a dotted walk that found
    // no field / no context (the mid-walk Exits already set the dotted
    // hint provisionally; this covers the non-dotted unknown-name path).
    if AHint = '' then
      AHint := BuildUnresolvedHint(AName);
    Exit;
  end;
  Phase('format ' + AType);

  // Consolidate the address handle for IsLocal callers so the
  // ShortString / Extended formatters -- which need to re-read more
  // than RawBytes' 8 bytes -- can rely on a single Addr regardless
  // of whether the variable is local or global. Register-passed
  // locals correctly leave Addr at nil here (GetLocalAddress
  // declines them), which makes the > 8-byte formatters cleanly
  // fail rather than dereference a junk stack pointer.
  if IsLocal and (Addr = nil) then
    GetLocalAddress(LastThreadHit, AName, Addr);

  // Auto-detection: empty AType means the caller wants the server
  // to pick a formatter based on the RSM-derived type information.
  // The actual lookup is delegated to AutoDetectFormatterName --
  // see its summary for the full resolution order. The resolved
  // formatter name is written back into AType (var parameter) so
  // the MCP server can surface it in the response label.
  if AType = '' then
    AType := AutoDetectFormatterName(AName, IsLocal, MatchedLocalTypeIdx, Member);

  // §6.47: a whole-name LOCAL classified as 'enum' purely via the RSM
  // low-byte local type id is SUSPECT. The §6.36 compact stack-local form
  // stores only the LOW byte of the 2-byte type id, so a record- or
  // reference-typed local's truncated id can numerically collide with a
  // small enum-secondary id -- e.g. in Test.Tfw.exe a TGUID local's id
  // $0E collides with enum secondary $000E, so IsEnumTypeId returns True
  // even though the id resolves to NO record/class
  // (FindClassIdxByRsmTypeId < 0). Trust the enum classification only when
  // the live value actually resolves to one of that enum's constants (the
  // same width-probe FormatEnum uses); otherwise it is a collision
  // artefact, so drop it and let the record-terminal / hint fallbacks
  // below run instead of emitting a confidently-wrong "enum: <unknown>".
  if SameText(AType, 'enum') and IsLocal and (MatchedLocalTypeIdx <> 0) and
     (MatchedLocalTypeIdx < $100) and Assigned(LocalsReader) and
     (LocalsReader.FindClassIdxByRsmTypeId(MatchedLocalTypeIdx) < 0) then
  begin
    var EnumConfirmed: Boolean := False;
    for var W in [1, 2, 4] do
    begin
      if Length(RawBytes) < W then Continue;
      var Ord_: Integer := 0;
      Move(RawBytes[0], Ord_, W);
      var ConfName: String := '';
      if LocalsReader.TryGetEnumConstantName(LastEnumTypeId, Ord_, ConfName) then
      begin
        EnumConfirmed := True;
        Break;
      end;
    end;
    if not EnumConfirmed then
    begin
      AType := '';
      LastEnumTypeId := 0;
    end;
  end;

  if Formatters.TryGetValue(LowerCase(AType), Formatter) then
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
  if (AType = '') and (not IsLocal) and Assigned(LocalsReader) then
  begin
    var GlobIdHere: UInt32 := LocalsReader.FindGlobalTypeIdx(AName);
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
      if LocalsReader.TryResolveByScopeLocalTypeId(
        GlobIdHere, ScopeOrd, ScopeName) then
      begin
        LastEnumTypeId := 0;
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
      if LocalsReader.TryResolveScopeLocalEnum(
        AName, ScopeOrd, TypeHint, ScopeName) then
      begin
        LastEnumTypeId := 0;
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
  if (AType = '') and Assigned(LocalsReader) then
  begin
    var RecTypeName: String := '';
    var RecClsIdx  : Integer := -1;
    if TryGetRecordTerminalName(AName, IsLocal, MatchedLocalTypeIdx, RecTypeName,
         RecClsIdx) then
    begin
      // §6.36 layout-gated collision guard. The compact stack-local form
      // stores only the LOW byte of a 2-byte type id, so a cross-unit
      // reference-typed local (interface / cross-unit record) loses its
      // "page" byte and its low byte can collide with an unrelated
      // T-prefixed record's primary id (e.g. `Lst: ILazyUniqueList<TObject>`
      // -> $FA == `TICONDIR` $00FA). The record id is therefore SUSPECT for
      // a LOCAL. Trust it only when the live bytes are consistent with it;
      // detect the contradiction that proves a collision and recover the
      // live reference type instead of emitting a confidently-wrong record
      // (which `.FieldName` would then navigate into garbage). The guard is
      // principled, NOT the refuted blind slot-check: it fires ONLY when the
      // resolved record's FIRST member is a sub-pointer scalar (1-2 bytes at
      // offset 0 -- so a genuine instance's leading bytes CANNOT be a
      // pointer) yet the live slot holds a valid double-indirect reference
      // (slot -> [slot], both readable). A real scalar-first record can't
      // present that, so suppression never steals a legitimate record.
      // See DPT.Rsm.Format.md §6.36.
      var IsRefCollision: Boolean := False;
      var SlotPtr: UIntPtr := 0;
      if IsLocal and (RecClsIdx >= 0) and
         (LocalsReader.Classes[RecClsIdx].Members.Count > 0) and
         (LocalsReader.Classes[RecClsIdx].Members[0].Offset = 0) and
         (LocalsReader.Classes[RecClsIdx].Members[0].Size >= 1) and
         (LocalsReader.Classes[RecClsIdx].Members[0].Size <= 2) then
      begin
        if TargetIs32Bit then
        begin
          if Length(RawBytes) >= 4 then
            SlotPtr := PCardinal(@RawBytes[0])^;
        end
        else if Length(RawBytes) >= 8 then
          SlotPtr := UIntPtr(PUInt64(@RawBytes[0])^);
        if (SlotPtr <> 0) and (ReadTargetPointer(Pointer(SlotPtr)) <> 0) then
          IsRefCollision := True;
      end;

      if IsRefCollision then
      begin
        var RefDesc: String;
        if TryRecoverReferenceType(SlotPtr, RefDesc) then
        begin
          AValue := RefDesc;
          AType  := 'object';
          Result := True;
          Exit;
        end;
        // Provably a collision but the implementor couldn't be named.
        // Decline with a precise hint -- never emit the bogus record.
        AHint := Format('"%s" is a reference-typed local whose declared type ' +
          'is not recoverable from the .rsm (the compact stack-local type id ' +
          'collided into record %s -- see Format.md §6.36); the live slot at ' +
          '%x is an object/interface reference. Use type=object or read_memory.',
          [AName, RecTypeName, SlotPtr]);
        Exit;
      end;

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
    if Formatters.TryGetValue('int', Formatter) and
       Formatter(RawBytes, Addr, FieldKnownSize, AValue) then
    begin
      AType  := 'int';
      Result := True;
      Exit;
    end;
  end;

  if AType = '' then
    AHint := BuildAutoDetectHint(AName, RawBytes, Addr)
  // Explicit but unsupported type literal (e.g. type=foo): no formatter
  // matched and the auto-detect paths were all gated off by the non-empty
  // AType. Name the allowed set rather than failing opaquely.
  else if AHint = '' then
  begin
    var UnusedFmt: TEvaluateFormatter;
    if not Formatters.TryGetValue(LowerCase(AType), UnusedFmt) then
      AHint := Format(
        'Unsupported type "%s". Allowed: int, int64, string, ansistring, ' +
        'widestring, shortstring, single, double, extended, object.', [AType]);
  end;
end;

function TVariableEvaluator.BuildAutoDetectHint(const AName: String;
  const ARawBytes: TBytes; AAddr: Pointer): String;
// Generates a one-line hint for the auto-detect-declined failure class:
// the walk LOCATED the field (bytes were read) but no formatter could be
// inferred from the RSM type metadata. Beyond naming the "type=" args to
// retry, it includes the resolved bytes (LE) and -- when known -- the
// field address, so the caller has the data inline: it can read_memory
// there for the full value (e.g. a ShortString longer than 8 bytes) or
// just retry with an explicit type=, without re-deriving the address.
var
  PtrVal  : UIntPtr;
  HexBytes: String;
  AddrStr : String;
  I       : Integer;
begin
  Result := '';
  if Length(ARawBytes) < TargetPointerSize then Exit;

  HexBytes := '';
  for I := 0 to Length(ARawBytes) - 1 do
    HexBytes := HexBytes + IntToHex(ARawBytes[I], 2) + ' ';
  HexBytes := Trim(HexBytes);

  AddrStr := '';
  if Assigned(AAddr) then
    AddrStr := Format(' at address %s (read_memory there for the full value)',
      [IntToHex(NativeUInt(AAddr), TargetPointerSize * 2)]);

  PtrVal := 0;
  Move(ARawBytes[0], PtrVal, TargetPointerSize);
  if PtrVal = 0 then
    // Keep the literal substrings 'nil pointer' and 'type=object' -- the
    // auto-detect nil-global pin asserts on them.
    Result := Format(
      '%s holds 0 (nil pointer or zero-valued primitive); auto-detection ' +
      'cannot pick a formatter for nil. Resolved bytes (LE): %s%s. ' +
      'Retry with type=object (class reference), type=int, or type=string.',
      [AName, HexBytes, AddrStr])
  else
    Result := Format(
      '%s could not be auto-typed (no RSM type metadata for this field). ' +
      'Resolved bytes (LE): %s%s. Retry with an explicit type= ' +
      '(shortstring, int, int64, double, object, string, guid).',
      [AName, HexBytes, AddrStr]);
end;

function TVariableEvaluator.BuildUnresolvedHint(const AName: String): String;
// Hint for the OTHER failure class: nothing was read because the name /
// field could not be resolved. Points the caller at the right next tool
// per the failure shape -- a dotted path is a navigation failure (verify
// each field name; the first segment must be an object/record), a bare
// name is an unknown-symbol failure (get_locals / Unit.VarName). Both
// note the raw read_memory fallback. Names only tools that actually
// exist on the server (no read_global_variable); addresses come from
// get_registers / get_stack_trace / evaluate <obj> type=object.
begin
  if AName.Contains('.') then
    Result := Format(
      'Could not navigate "%s". Verify each field name exists on the ' +
      'resolved type (call get_locals for the valid first-segment / local ' +
      'names); the first segment must be an object reference (pointers are ' +
      'dereferenced) or a record (navigated inline). For raw inspection, ' +
      'get the base address from get_registers (EBP/RBP + the bp_offset ' +
      'from get_locals) or evaluate the first segment with type=object ' +
      '("@ <hex addr>"), then read_memory.', [AName])
  else
    Result := Format(
      'Unknown variable "%s". Call get_locals for in-scope local names, or ' +
      'qualify a global as Unit.VarName. When no symbols are available, use ' +
      'get_stack_slots / get_stack_memory, or get_registers + read_memory.',
      [AName]);
end;

function TVariableEvaluator.DeriveTypeHintFromVariableName(const AName: String): String;
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

function TVariableEvaluator.TryNameBasedEnumLookup(const AName: String;
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
  if not Assigned(LocalsReader) then Exit;
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
  Candidate := LocalsReader.FindTypeIdByName('T' + Stripped);
  if Candidate = 0 then Exit;
  if not LocalsReader.IsEnumTypeId(Candidate) then Exit;
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
  if LocalsReader.TryGetEnumConstantName(Candidate, Ordinal, EnumName, Prefix) then
  begin
    AFormatted := Format('%s (%d)', [EnumName, Ordinal]);
    Result := True;
  end;
end;

function TVariableEvaluator.TryProbeClassPointer(const ARawBytes: TBytes;
  out AFormatted: String): Boolean;
// Treats the first TargetPointerSize bytes as a class-instance
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
  if Length(ARawBytes) < TargetPointerSize then Exit;
  PtrVal := 0;
  Move(ARawBytes[0], PtrVal, TargetPointerSize);
  // Nil class pointers are ambiguous (could also be an uninitialized
  // primitive). Decline so the caller falls through to the record-
  // terminal hint / the explicit "Failed to evaluate" path -- the
  // user can still get the value with an explicit type=object.
  if PtrVal = 0 then Exit;
  VMTPtr := ReadTargetPointer(Pointer(PtrVal));
  if VMTPtr = 0 then Exit;
  if not ReadClassNameFromVMT(VMTPtr, ClassName) then Exit;
  AFormatted := ClassName + ' @ ' +
                Format('%.*x', [TargetPointerSize * 2, PtrVal]);
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
function TVariableEvaluator.AutoDetectFormatterName(const AName: String; AIsLocal: Boolean;
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
    if (ATypeIdx = 0) or (not Assigned(LocalsReader)) then Exit;
    if AUseRegistry then
      ClsIdx := LocalsReader.FindClassIdxByRsmTypeId(ATypeIdx)
    else
      ClsIdx := LocalsReader.FindStructByTypeIdx(ATypeIdx);
    if (ClsIdx >= 0) and (LocalsReader.Classes[ClsIdx].Kind = skClass) then
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
    if (ATypeId = 0) or (not Assigned(LocalsReader)) then Exit;
    if LocalsReader.IsEnumTypeId(ATypeId) then
    begin
      LastEnumTypeId := ATypeId;
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
    if PrimitiveTypeFormatters.TryGetValue(AMember.PrimitiveTypeId, Result) then
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
    if PrimitiveTypeFormatters.TryGetValue(UInt16(AMatchedLocalTypeIdx), Result) then
      Exit;
    Result := EnumLookup(AMatchedLocalTypeIdx);
    if Result <> '' then Exit;
    Result := ClassLookup(AMatchedLocalTypeIdx, True);
    if Result <> '' then Exit;
  end;
  // Path 4: whole-name global. Same triple lookup against the
  // global's registry type id.
  if (not AIsLocal) and Assigned(LocalsReader) then
  begin
    GlobTypeIdx := LocalsReader.FindGlobalTypeIdx(AName);
    if GlobTypeIdx <> 0 then
    begin
      if PrimitiveTypeFormatters.TryGetValue(UInt16(GlobTypeIdx), Result) then
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

end.
