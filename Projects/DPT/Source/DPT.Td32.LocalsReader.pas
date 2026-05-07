// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Td32.LocalsReader;

interface

uses

  System.Classes,
  System.SysUtils,

  mormot.core.collections;

type

  /// <summary>
  ///   A single local variable extracted from a TD32 BPREL32 symbol record.
  ///   The offset is signed and relative to the procedure's base pointer
  ///   (EBP on x86, RBP on x64).
  /// </summary>
  TTd32Local = record
    Name    : String;
    BpOffset: Int32;
    TypeIdx : UInt32;
  end;

  /// <summary>
  ///   A procedure scope extracted from a TD32 GPROC32 / LPROC32 record,
  ///   together with all its BPREL32 children up to the matching END.
  ///   <c>SegmentOffset</c> is segment-relative; for a Delphi PE this is
  ///   the RVA from the start of the code segment (typically image-base
  ///   plus $1000 yields the runtime VA).
  /// </summary>
  TTd32Proc = record
    Name         : String;
    SegmentOffset: UInt32;
    Size         : UInt32;
    Locals       : IList<TTd32Local>;
  end;

  /// <summary>
  ///   A field of a Delphi class extracted from a TD32 LF_MEMBER record:
  ///   the field's name, its byte offset within the class instance (where
  ///   offset 0 is the VMT pointer slot), and the TD32 type-index of the
  ///   field's type.
  /// </summary>
  TTd32ClassMember = record
    Name   : String;
    Offset : UInt32;
    TypeIdx: UInt32;
  end;

  /// <summary>
  ///   A class declared in the debugged binary: its short name (without
  ///   unit prefix, as Delphi emits it into the Borland TD32 type stream)
  ///   and the ordered list of its member fields.
  /// </summary>
  TTd32ClassInfo = record
    Name   : String;
    Members: IList<TTd32ClassMember>;
  end;

  /// <summary>
  ///   Minimal RSM/TD32 reader focused on extracting per-procedure local
  ///   variables (BPREL32 records). JCL's parser ships with full support
  ///   for the surrounding structure (directory walk, names table) but
  ///   does not handle BPREL32; this class wraps JCL's structural types
  ///   and constants and walks the symbol stream itself.
  /// </summary>
  TTd32LocalsReader = class
  private
    FProcs  : IList<TTd32Proc>;
    FClasses: IList<TTd32ClassInfo>;
    procedure ParseStream(AStreamPtr: Pointer; AStreamSize: Integer);
    function  ParseNamesSubsection(APtr: Pointer; ASize: Integer): TArray<String>;
    procedure WalkAlignSymbols(APtr: Pointer; ASize: Integer; const ANames: TArray<String>);
    procedure WalkGlobalTypes(APtr: Pointer; ASize: Integer; const ANames: TArray<String>);
  public
    constructor Create;
    /// <summary>
    ///   Loads TD32 debug info from a Delphi PE executable that was built
    ///   with the linker option "Include remote debug symbols" (-VR).
    ///   The TD32 stream is located inside the EXE itself; the sidecar
    ///   .rsm file uses a different (CSH7) container format and is not
    ///   parsed by this reader.
    /// </summary>
    procedure LoadFromFile(const AExePath: String);
    procedure LoadFromBytes(const ABytes: TBytes);
    /// <summary>
    ///   Returns the index of the procedure whose [SegmentOffset, SegmentOffset+Size)
    ///   range contains <c>ASegmentOffset</c>, or -1 when no match is found.
    /// </summary>
    function  FindProcContaining(ASegmentOffset: UInt32): Integer;
    /// <summary>
    ///   Returns the index of the first procedure whose name matches
    ///   <c>AName</c> exactly (case-insensitive), or -1 when none.
    /// </summary>
    function  FindProcByName(const AName: String): Integer;
    /// <summary>
    ///   Returns the index in <see cref="Classes"/> of the class declared
    ///   with the given short name (case-insensitive), or -1 if no such
    ///   class is in the parsed type stream.
    /// </summary>
    function  FindClassByName(const AName: String): Integer;
    /// <summary>
    ///   Looks up a member field on the class by name (case-insensitive).
    ///   Returns False if either the class or the field is unknown.
    /// </summary>
    function  FindClassMember(const AClassName, AFieldName: String;
      out AMember: TTd32ClassMember): Boolean;
    property Procs: IList<TTd32Proc> read FProcs;
    property Classes: IList<TTd32ClassInfo> read FClasses;
  end;

implementation

uses

  Winapi.Windows,

  JclTD32;

type

  // BPREL32 record payload (Borland TD32 layout). Differs from generic
  // CodeView 4 in that both TypeIndex and NameIndex are stored as DWORD
  // (matching the convention used by other Borland symbol records like
  // TSymbolDataInfo). Total payload is 12 bytes; total record including
  // the size+rectyp prefix is 16 bytes.
  PSymbolBpRelInfo = ^TSymbolBpRelInfo;
  TSymbolBpRelInfo = packed record
    Offset   : Int32;
    TypeIndex: UInt32;
    NameIndex: UInt32;
  end;

  PSymbolHeader = ^TSymbolHeader;
  TSymbolHeader = packed record
    Size   : Word; // record length excluding this size field
    SymType: Word;
  end;

  // JCL declares TSymbolProcInfo but not its pointer alias.
  PSymbolProcInfo = ^TSymbolProcInfo;

constructor TTd32LocalsReader.Create;
begin
  inherited Create;
  FProcs := Collections.NewPlainList<TTd32Proc>;
  FClasses := Collections.NewPlainList<TTd32ClassInfo>;
end;

procedure TTd32LocalsReader.LoadFromFile(const AExePath: String);
var
  Bytes : TBytes;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AExePath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Bytes, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Bytes[0], Stream.Size);
  finally
    Stream.Free;
  end;
  LoadFromBytes(Bytes);
end;

procedure TTd32LocalsReader.LoadFromBytes(const ABytes: TBytes);
const
  SigFB09: UInt32 = $39304246; // 'FB09' on disk in little-endian byte order
  SigFB0A: UInt32 = $41304246; // 'FB0A'
var
  Idx       : Integer;
  Probe     : UInt32;
  TD32Start : Integer;
  TD32Bytes : TBytes;
begin
  FProcs.Clear;
  if Length(ABytes) < 8 then
    Exit;

  // Locate the FB09 / FB0A header on a 4-byte boundary, skipping past the
  // DOS stub and PE optional header where the magic never legitimately
  // appears in Delphi binaries. The same signature also appears in the
  // 8-byte trailer at EOF, so taking the earliest hit guarantees the
  // header rather than the trailer.
  TD32Start := -1;
  Idx := 256;
  while Idx + 8 <= Length(ABytes) do
  begin
    Probe :=
      UInt32(ABytes[Idx]) or
      (UInt32(ABytes[Idx + 1]) shl  8) or
      (UInt32(ABytes[Idx + 2]) shl 16) or
      (UInt32(ABytes[Idx + 3]) shl 24);
    if (Probe = SigFB09) or (Probe = SigFB0A) then
    begin
      TD32Start := Idx;
      Break;
    end;
    Inc(Idx, 4);
  end;
  if TD32Start < 0 then
    Exit;

  SetLength(TD32Bytes, Length(ABytes) - TD32Start);
  Move(ABytes[TD32Start], TD32Bytes[0], Length(TD32Bytes));

  // JCL's validator reads the trailer from the end of the buffer to verify
  // a closed TD32 stream. Slicing from the header to EOF preserves both.
  if not TJclTD32InfoParser.IsTD32DebugInfoValid(@TD32Bytes[0], Length(TD32Bytes)) then
    Exit;

  ParseStream(@TD32Bytes[0], Length(TD32Bytes));
end;

procedure TTd32LocalsReader.ParseStream(AStreamPtr: Pointer; AStreamSize: Integer);
var
  pSig   : PJclTD32FileSignature;
  pDirHdr: PDirectoryHeader;
  Names  : TArray<String>;
  I      : Integer;
  EntryPtr: Pointer;
  function LfaToVa(Lfa: UInt32): Pointer;
  begin
    Result := Pointer(NativeUInt(AStreamPtr) + Lfa);
  end;
begin
  pSig := PJclTD32FileSignature(AStreamPtr);
  if pSig.Offset >= UInt32(AStreamSize) then
    Exit;

  // First pass: collect names from NAMES subsection so the symbol walk
  // can resolve NameIndex references.
  SetLength(Names, 0);
  pDirHdr := PDirectoryHeader(LfaToVa(pSig.Offset));
  while True do
  begin
    {$RANGECHECKS OFF}
    for I := 0 to Integer(pDirHdr.DirEntryCount) - 1 do
      with pDirHdr.DirEntries[I] do
      begin
        EntryPtr := LfaToVa(Offset);
        if SubsectionType = SUBSECTION_TYPE_NAMES then
          Names := ParseNamesSubsection(EntryPtr, Size);
      end;
    if pDirHdr.lfoNextDir <> 0 then
      pDirHdr := PDirectoryHeader(LfaToVa(pDirHdr.lfoNextDir))
    else
      Break;
  end;

  // Second pass: walk the symbol streams, now that we have names available.
  pDirHdr := PDirectoryHeader(LfaToVa(pSig.Offset));
  while True do
  begin
    for I := 0 to Integer(pDirHdr.DirEntryCount) - 1 do
      with pDirHdr.DirEntries[I] do
      begin
        EntryPtr := LfaToVa(Offset);
        if (SubsectionType = SUBSECTION_TYPE_ALIGN_SYMBOLS) or
           (SubsectionType = SUBSECTION_TYPE_SYMBOLS) or
           (SubsectionType = SUBSECTION_TYPE_GLOBAL_SYMBOLS) then
          WalkAlignSymbols(EntryPtr, Size, Names);
        if SubsectionType = SUBSECTION_TYPE_GLOBAL_TYPES then
          WalkGlobalTypes(EntryPtr, Size, Names);
      end;
    if pDirHdr.lfoNextDir <> 0 then
      pDirHdr := PDirectoryHeader(LfaToVa(pDirHdr.lfoNextDir))
    else
      Break;
  end;
  {$IFDEF RANGECHECKS_ON}
  {$RANGECHECKS ON}
  {$ENDIF}
end;

function TTd32LocalsReader.ParseNamesSubsection(APtr: Pointer; ASize: Integer): TArray<String>;
var
  Count: UInt32;
  P    : PByte;
  PEnd : PByte;
  Len  : Byte;
  Buf  : TBytes;
  I    : Integer;
begin
  // NAMES subsection: DWORD count, then for each name: BYTE namelen,
  // BYTE name[namelen], BYTE 0 (terminator). For names longer than 255
  // chars the stored length wraps; the canonical end-of-name marker is
  // the next null byte.
  // Index 0 is a reserved sentinel slot (kept empty); on-disk NameIndex
  // values are 1-based, so symbol records can use 0 to mean "no name".
  // JCL's parser stores names the same way; this matches its conventions.
  Result := nil;
  if ASize < 4 then
    Exit;
  Count := PUInt32(APtr)^;
  P := PByte(NativeUInt(APtr) + SizeOf(UInt32));
  PEnd := PByte(NativeUInt(APtr) + UInt32(ASize));
  SetLength(Result, Count + 1);
  Result[0] := '';
  for I := 1 to Integer(Count) do
  begin
    if NativeUInt(P) >= NativeUInt(PEnd) then
      Break;
    Len := P^;
    Inc(P);
    if Len > 0 then
    begin
      SetLength(Buf, Len);
      Move(P^, Buf[0], Len);
      Inc(P, Len);
    end
    else
      SetLength(Buf, 0);
    while (NativeUInt(P) < NativeUInt(PEnd)) and (P^ <> 0) do
      Inc(P, 256);
    Inc(P);
    Result[I] := TEncoding.ANSI.GetString(Buf);
  end;
end;

procedure TTd32LocalsReader.WalkAlignSymbols(APtr: Pointer; ASize: Integer;
  const ANames: TArray<String>);
const
  MaxScopeDepth = 64;
var
  P, PEnd      : PByte;
  Hdr          : PSymbolHeader;
  ScopeStack   : array[0..MaxScopeDepth - 1] of Integer;
  StackTop     : Integer;
  CurProcIdx   : Integer;
  Proc         : TTd32Proc;
  Loc          : TTd32Local;
  pProc        : PSymbolProcInfo;
  pBp          : PSymbolBpRelInfo;

  function NameAt(AIdx: UInt32): String;
  begin
    if AIdx < UInt32(Length(ANames)) then
      Result := ANames[AIdx]
    else
      Result := '';
  end;

  procedure PushProc(AIdx: Integer);
  begin
    if StackTop < High(ScopeStack) then
    begin
      Inc(StackTop);
      ScopeStack[StackTop] := AIdx;
    end;
    if AIdx >= 0 then
      CurProcIdx := AIdx;
  end;

  procedure PopScope;
  var
    K: Integer;
  begin
    if StackTop >= 0 then
    begin
      Dec(StackTop);
      CurProcIdx := -1;
      for K := StackTop downto 0 do
        if ScopeStack[K] >= 0 then
        begin
          CurProcIdx := ScopeStack[K];
          Break;
        end;
    end;
  end;

begin
  // Each align-symbols subsection begins with a DWORD signature, followed
  // by symbol records. Each record starts with a Word size (excluding the
  // size word itself) and a Word symbol type.
  if ASize < SizeOf(UInt32) then
    Exit;
  P := PByte(NativeUInt(APtr) + SizeOf(UInt32));
  PEnd := PByte(NativeUInt(APtr) + UInt32(ASize));
  StackTop := -1;
  CurProcIdx := -1;

  while NativeUInt(P) + SizeOf(TSymbolHeader) <= NativeUInt(PEnd) do
  begin
    Hdr := PSymbolHeader(P);
    if Hdr.Size = 0 then
      Break;
    if NativeUInt(P) + SizeOf(Word) + Hdr.Size > NativeUInt(PEnd) then
      Break;

    case Hdr.SymType of
      SYMBOL_TYPE_GPROC32, SYMBOL_TYPE_LPROC32:
        begin
          pProc := PSymbolProcInfo(NativeUInt(P) + SizeOf(TSymbolHeader));
          Proc.Name := NameAt(pProc.NameIndex);
          Proc.SegmentOffset := pProc.Offset;
          Proc.Size := pProc.Size;
          Proc.Locals := Collections.NewPlainList<TTd32Local>;
          FProcs.Add(Proc);
          PushProc(FProcs.Count - 1);
        end;
      SYMBOL_TYPE_BPREL32:
        if CurProcIdx >= 0 then
        begin
          pBp := PSymbolBpRelInfo(NativeUInt(P) + SizeOf(TSymbolHeader));
          Loc.Name := NameAt(pBp.NameIndex);
          Loc.BpOffset := pBp.Offset;
          Loc.TypeIdx := pBp.TypeIndex;
          // The IList<TTd32Proc> stores records by value, but the Locals
          // field is an interface (IList<TTd32Local>) that points at the
          // same backing list, so adding via Procs[idx].Locals reaches
          // the original record's collection without write-back.
          FProcs[CurProcIdx].Locals.Add(Loc);
        end;
      SYMBOL_TYPE_END:
        PopScope;
    end;

    Inc(P, SizeOf(Word) + Hdr.Size);
  end;
end;

procedure TTd32LocalsReader.WalkGlobalTypes(APtr: Pointer; ASize: Integer;
  const ANames: TArray<String>);
// SUBSECTION_TYPE_GLOBAL_TYPES layout (Borland TD32, empirically observed
// in Delphi 12 EXEs):
//
//   DWORD version      (= 1)
//   DWORD num_types
//   DWORD offsets[num_types]   -- byte offsets within this subsection
//                                  to each type record
//   { type records }            -- variable-size; each starts with
//                                  WORD reclen (excluding this size word)
//                                  WORD rectyp (leaf code)
//
// Type-index N maps to offset table entry [N - $1000]; type-indices below
// $1000 are reserved for built-in types and don't appear in the offset
// table.
//
// We decode three record kinds:
//
// LF_CLASS / LF_STRUCTURE (rectyp = $0004 / $0005):
//   payload start: WORD count, DWORD field_list_typeidx, ... (other slots
//   we don't need), and the DWORD 6 bytes before the record's end is the
//   class's NameIndex into NAMES.
//
// LF_FIELDLIST (rectyp = $0204) holds a sequence of sub-records, each
// starting with a WORD sub-leaf code. We only consume LF_MEMBER (0x0406);
// other sub-records (LF_VFUNCTAB, LF_BCLASS, LF_NESTTYPE) are skipped via
// their fixed sizes.
//
// LF_MEMBER (sub-leaf $0406, fixed Borland layout: 16 bytes):
//   WORD leaf
//   WORD type_idx_low                  -- type-index of the field's type
//                                         (uses the low word of the field
//                                         type-index; the high word is in
//                                         the next field below)
//   WORD attribute
//   WORD type_idx_high or attr_high
//   DWORD name_idx                     -- NameIndex into NAMES
//   DWORD offset                       -- byte offset of the field within
//                                         the class instance (logical
//                                         offset from start of user fields,
//                                         not including the VMT pointer
//                                         slot — see below)
//
// IMPORTANT: the offset Delphi emits is the **logical** offset within the
// class's user-field area, NOT the runtime byte offset relative to the
// instance pointer. To translate to a runtime offset:
//   runtime_offset = logical_offset + SizeOf(Pointer)
// because the VMT pointer always occupies the first slot of any TObject
// descendant. Callers (e.g. EvaluateVariable) must add the bitness-aware
// VMT-slot size when reading from the live process.

  function IsValidNameIdx(AIdx: UInt32): Boolean;
  begin
    Result := (AIdx > 0) and (AIdx < UInt32(Length(ANames)));
  end;

  function NameAt(AIdx: UInt32): String;
  begin
    if IsValidNameIdx(AIdx) then
      Result := ANames[AIdx]
    else
      Result := '';
  end;

  function DemangleClassName(const AMangled: String): String;
  // Delphi x64 emits class names in an Itanium-style mangled form:
  //   _ZTRN<len><unit><len><class>E
  // e.g., "_ZTRN11Debugtarget6TInnerE" -> "TInner".
  // Walk past the _ZTR... prefix to the 'N' nesting marker, then read
  // length-prefixed segments until 'E'; the last segment is the class
  // name. Returns AMangled unchanged when the input doesn't follow the
  // mangling scheme (e.g., on x86 builds where no mangling occurs).
  var
    P, NPos, Len: Integer;
    Last       : String;
  begin
    Result := AMangled;
    if not AMangled.StartsWith('_Z') then Exit;
    NPos := Pos('N', AMangled);
    if NPos = 0 then Exit;
    Last := '';
    P := NPos + 1;
    while P <= Length(AMangled) do
    begin
      if AMangled[P] = 'E' then Break;
      Len := 0;
      while (P <= Length(AMangled)) and CharInSet(AMangled[P], ['0'..'9']) do
      begin
        Len := Len * 10 + (Ord(AMangled[P]) - Ord('0'));
        Inc(P);
      end;
      if (Len = 0) or (P + Len - 1 > Length(AMangled)) then Break;
      Last := Copy(AMangled, P, Len);
      Inc(P, Len);
    end;
    if Last <> '' then Result := Last;
  end;

const
  TYPE_INDEX_BASE = $1000;
  LF_STRUCTURE    = $0005;
  LF_CLASS        = $0004;
  LF_FIELDLIST    = $0204;
  LF_MEMBER       = $0406;
var
  BB              : PByte;
  NumTypes        : UInt32;
  Offsets         : PByte;
  I               : Integer;
  RecOff          : UInt32;
  RecEnd          : UInt32;
  RecLen          : Word;
  RecTyp          : Word;
  ClassFieldList  : UInt32;
  ClassNameIdx    : UInt32;
  Info            : TTd32ClassInfo;
  Members         : IList<TTd32ClassMember>;
  Member          : TTd32ClassMember;
  FieldRecOff     : UInt32;
  FieldRecEnd     : UInt32;
  FieldRecLen     : Word;
  FieldRecTyp     : Word;
  P               : UInt32;
  SubLeaf         : Word;
  ClassFieldListIdx: Integer;
begin
  if ASize < 8 then
    Exit;
  BB := PByte(APtr);
  NumTypes := PUInt32(BB + 4)^;
  if (NumTypes = 0) or (NumTypes > UInt32(ASize) div 4) then
    Exit;
  Offsets := BB + 8;

  for I := 0 to Integer(NumTypes) - 1 do
  begin
    RecOff := PUInt32(Offsets + I * 4)^;
    if (RecOff + 4 > UInt32(ASize)) or (RecOff < 8 + NumTypes * 4) then
      Continue;
    RecLen := PWord(BB + RecOff)^;
    if RecOff + 2 + RecLen > UInt32(ASize) then
      Continue;
    RecTyp := PWord(BB + RecOff + 2)^;
    if (RecTyp <> LF_CLASS) and (RecTyp <> LF_STRUCTURE) then
      Continue;
    if RecLen < 24 then
      Continue;
    RecEnd := RecOff + 2 + RecLen;
    // class-record payload starts at RecOff + 4
    // count(2) + field_list(4) at offsets 0..5 of payload
    ClassFieldList := PUInt32(BB + RecOff + 6)^;
    // name_idx is the DWORD 6 bytes before the end of the record
    if RecEnd < 6 then
      Continue;
    ClassNameIdx := PUInt32(BB + RecEnd - 6)^;
    if not IsValidNameIdx(ClassNameIdx) then
      Continue;
    if ClassFieldList < TYPE_INDEX_BASE then
      Continue;
    ClassFieldListIdx := Integer(ClassFieldList - TYPE_INDEX_BASE);
    if (ClassFieldListIdx < 0) or (ClassFieldListIdx >= Integer(NumTypes)) then
      Continue;

    FieldRecOff := PUInt32(Offsets + ClassFieldListIdx * 4)^;
    if FieldRecOff + 4 > UInt32(ASize) then
      Continue;
    FieldRecLen := PWord(BB + FieldRecOff)^;
    if FieldRecOff + 2 + FieldRecLen > UInt32(ASize) then
      Continue;
    FieldRecTyp := PWord(BB + FieldRecOff + 2)^;
    if FieldRecTyp <> LF_FIELDLIST then
      Continue;
    FieldRecEnd := FieldRecOff + 2 + FieldRecLen;

    Members := Collections.NewPlainList<TTd32ClassMember>;
    P := FieldRecOff + 4;
    while P + 2 <= FieldRecEnd do
    begin
      // CV4 sub-records inside an LF_FIELDLIST may be followed by
      // LF_PAD0..LF_PAD15 alignment bytes (raw byte values $F0..$FF).
      // Skip them before reading the next sub-leaf.
      while (P < FieldRecEnd) and ((BB + P)^ >= $F0) do
        Inc(P);
      if P + 2 > FieldRecEnd then
        Break;
      SubLeaf := PWord(BB + P)^;
      case SubLeaf of
        LF_MEMBER:
          begin
            // Layout starting at P (Borland TD32, 20 bytes per record):
            //   leaf(2) type_idx(4) attr(2) name_idx(4) reserved(4) offset(2) pad(2)
            // The "offset" field (in user-field bytes within the class)
            // is actually a CV4 numeric leaf at bytes 16-17, NOT the
            // zero-fill DWORD at bytes 12-15. Pad bytes (LF_PAD0..15)
            // follow it to align the next sub-record.
            if P + 20 > FieldRecEnd then
              Break;
            Member.TypeIdx := PUInt32(BB + P + 2)^;
            Member.Name    := NameAt(PUInt32(BB + P + 8)^);
            Member.Offset  := PWord(BB + P + 16)^;
            if Member.Name <> '' then
              Members.Add(Member);
            Inc(P, 20);
          end;
        $040A {LF_VFUNCTAB}:
          // leaf(2) pad(2) typeidx(4) -- 8 bytes
          Inc(P, 8);
        $0400 {LF_BCLASS}:
          // leaf(2) attr(2) base_typeidx(4) offset(4) -- 12 bytes
          Inc(P, 12);
        $0409 {LF_NESTTYPE}:
          Inc(P, 8);
      else
        Break;
      end;
    end;

    if Members.Count > 0 then
    begin
      Info.Name := DemangleClassName(NameAt(ClassNameIdx));
      Info.Members := Members;
      FClasses.Add(Info);
    end;
  end;
end;

function TTd32LocalsReader.FindClassByName(const AName: String): Integer;
var
  I: Integer;
begin
  for I := 0 to FClasses.Count - 1 do
    if SameText(FClasses[I].Name, AName) then
      Exit(I);
  Result := -1;
end;

function TTd32LocalsReader.FindClassMember(const AClassName, AFieldName: String;
  out AMember: TTd32ClassMember): Boolean;
var
  ClsIdx, I: Integer;
  Info     : TTd32ClassInfo;
begin
  Result := False;
  AMember := Default(TTd32ClassMember);
  ClsIdx := FindClassByName(AClassName);
  if ClsIdx < 0 then
    Exit;
  Info := FClasses[ClsIdx];
  for I := 0 to Info.Members.Count - 1 do
    if SameText(Info.Members[I].Name, AFieldName) then
    begin
      AMember := Info.Members[I];
      Exit(True);
    end;
end;

function TTd32LocalsReader.FindProcContaining(ASegmentOffset: UInt32): Integer;
var
  I: Integer;
begin
  for I := 0 to FProcs.Count - 1 do
    if (FProcs[I].SegmentOffset <= ASegmentOffset) and
       (ASegmentOffset < FProcs[I].SegmentOffset + FProcs[I].Size) then
      Exit(I);
  Result := -1;
end;

function TTd32LocalsReader.FindProcByName(const AName: String): Integer;
var
  I       : Integer;
  Stored  : String;
  StripAt : String;
begin
  // Delphi emits program-level procedures with a leading '@' (its mangled
  // form). Match either with or without the prefix so callers can use
  // the source-level name.
  for I := 0 to FProcs.Count - 1 do
  begin
    Stored := FProcs[I].Name;
    if SameText(Stored, AName) then
      Exit(I);
    if (Length(Stored) > 0) and (Stored[1] = '@') then
    begin
      StripAt := Copy(Stored, 2, MaxInt);
      if SameText(StripAt, AName) then
        Exit(I);
    end;
  end;
  Result := -1;
end;

end.
