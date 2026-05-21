// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.Model;

// Data types and tag constants for the RSM (CSH7) reader. Kept
// dependency-light on purpose: no mORMot, no Windows, no I/O -- just
// the records, enums and tag byte values shared across the
// DPT.Rsm.Scanner and DPT.Rsm.Reader units (and across their test
// fixtures). Splitting the model out keeps the heavier reader/scanner
// units focused on logic.

interface

uses
  System.SysUtils,

  mormot.core.collections;

type

  /// <summary>
  ///   How a local/parameter is reached from the executing thread:
  ///   either as a BP-relative stack slot (lkBpRel) where the address
  ///   is FramePtr + BpOffset, or as a register parameter (lkRegister)
  ///   where the value lives in a CPU register at the time of the
  ///   call. Register parameters appear for the first few arguments of
  ///   a Delphi proc under the default "register" / Win64 ABI -- they
  ///   are NOT spilled to stack slots, so callers must read them out of
  ///   the thread's register file instead of from FramePtr + offset.
  /// </summary>
  TRsmLocalKind = (lkBpRel, lkRegister);

  /// <summary>
  ///   A single local variable extracted from an RSM symbol stream:
  ///   its source name, its signed offset relative to the procedure's
  ///   base pointer (EBP/RBP), and the RSM type-id of its declared
  ///   type.
  /// </summary>
  TRsmLocal = record
    Name    : String;
    BpOffset: Int32;
    TypeIdx : UInt32;
    Kind    : TRsmLocalKind;
    /// <summary>
    ///   Zero-based index of the parameter within the calling-
    ///   convention's register order: 0 = 1st reg param, 1 = 2nd, etc.
    ///   Only meaningful when Kind = lkRegister. The concrete register
    ///   is platform-specific (x86 Delphi default: EAX, EDX, ECX;
    ///   Win64 ABI: RCX, RDX, R8, R9), so the higher layer resolves
    ///   index -> register name when reading the value.
    /// </summary>
    RegParamIdx: Byte;
  end;

  /// <summary>
  ///   A procedure scope extracted from an RSM symbol stream, with
  ///   all of its locals attached. <c>SegmentOffset</c> is the RVA
  ///   of the procedure's first instruction within the executable's
  ///   code segment, in the same convention as the TD32 reader uses.
  /// </summary>
  TRsmProc = record
    Name         : String;
    SegmentOffset: NativeUInt;
    Size         : NativeUInt;
    Locals       : IList<TRsmLocal>;
  end;

  /// <summary>
  ///   A field of a Delphi class or record extracted from an RSM type
  ///   record: the field's name, its byte offset within the parent
  ///   instance, and the RSM type-id of the field's declared type.
  ///   For class fields the offset is relative to the instance pointer
  ///   (so the first field sits at offset SizeOf(Pointer), past the
  ///   VMT slot); for record fields the offset is relative to the
  ///   start of the record.
  /// </summary>
  TRsmClassMember = record
    Name   : String;
    Offset : UInt32;
    TypeIdx: UInt32;
    /// Byte width of this field as inferred from the gap to the next
    /// field's offset, or 0 when the field is the last in its
    /// structure and the size therefore can't be derived from the
    /// offset chain alone. Used by <c>EvaluateVariable</c> to clamp
    /// the read at a sub-DWORD field (e.g. Word at offset 20 in TMdt,
    /// where the user asks for type "int" -- without the clamp, the
    /// read pulls 4 bytes and visibly concatenates the next field's
    /// bytes into the result).
    Size   : UInt32;
    /// Well-known 2-byte primitive type id emitted by the Delphi
    /// compiler for built-in types (Integer = $03FD, Word = $0415,
    /// Double = $041D, etc.). Populated by
    /// <c>LinkFieldsFromFormatA</c> from the "$9C $09" primitive-
    /// reference block inside a $2C field record. Zero for class-
    /// or record-typed fields (use <c>TypeIdx</c> for those instead).
    /// Drives evaluate-tool auto-detection: when the caller omits
    /// the <c>type</c> argument, this id is mapped to a formatter
    /// name through a fixed compiler-built-in table.
    PrimitiveTypeId: UInt16;
  end;

  /// <summary>
  ///   Whether a structured type from the RSM type stream is a Delphi
  ///   class (with a VMT pointer at instance offset 0) or a Delphi
  ///   record (no VMT, fields start at offset 0). Decides whether
  ///   navigation through this type dereferences the holding slot
  ///   (class) or treats it as inline data (record).
  /// </summary>
  TRsmStructKind = (skClass, skRecord);

  /// <summary>
  ///   A class or record declared in the debugged binary, parsed from
  ///   the RSM type stream: its short name, its RSM type-id (used by
  ///   record-typed field navigation, where the holding field's static
  ///   TypeIdx is the only key to identify the inline record's layout),
  ///   the kind, and the ordered list of its member fields.
  /// </summary>
  TRsmClassInfo = record
    Name      : String;
    TypeIdx   : UInt32;
    Kind      : TRsmStructKind;
    Members   : IList<TRsmClassMember>;
    /// <summary>
    ///   Name of the immediate parent class for class-kind entries.
    ///   Empty when the class is at the top of the user-visible
    ///   hierarchy (the implicit TObject parent or, for records,
    ///   any inheritance at all). Populated by two complementary
    ///   passes: (1) a post-parse offset-matching pass that joins
    ///   classes whose instance layouts line up at the byte
    ///   boundary, and (2) a type-id resolution pass driven by
    ///   <see cref="ParentRawId"/> which handles cross-unit
    ///   inheritance the offset heuristic cannot bridge.
    /// </summary>
    ParentName: String;
    /// <summary>
    ///   The 16-bit RSM type-id sitting in the two bytes immediately
    ///   before the class-name length byte. Non-zero only when the
    ///   class declares a cross-unit parent (e.g. user code
    ///   inheriting <c>System.Classes.TComponent</c>); same-unit
    ///   inheritance encodes zero here and is resolved through the
    ///   offset heuristic instead. Looked up against the RSM type
    ///   registry to populate <see cref="ParentName"/> after the
    ///   type registry has been scanned.
    /// </summary>
    ParentRawId: UInt32;
  end;

  /// <summary>
  ///   Tag-byte constants used by the RSM symbol-stream scanner. Kept
  ///   together as a record-of-constants so the scanner and the
  ///   tests refer to one canonical set of values; the previous code
  ///   had these as a private const block inside ScanSymbolStream
  ///   where tests couldn't see them.
  /// </summary>
  TRsmTag = record
  public
  const
    /// Procedure-record tag. Followed by <NameLen> <Name> <addr payload>.
    PROC_TAG         = $28;
    /// BP-relative local / module-level global record tag.
    LOCAL_TAG        = $20;
    /// Open-array / stack-parameter record tag.
    PARAM_TAG        = $22;
    /// Register-passed parameter record tag (Self, AOwner, ... in
    /// methods whose params ride EAX/EDX/ECX or RCX/RDX/R8/R9).
    REGVAR_TAG       = $21;
    /// Top-level primitive global tag (Integer / string / Int64 /
    /// ShortString / object-typed nil). Distinct from LOCAL_TAG so
    /// the global-name -> primitive-id map can be populated.
    GLOBAL_PRIM_TAG  = $27;
    /// Enum-constant record. One per element of an enumerated type;
    /// carries (typeId, ordinal, name) for the enum formatter.
    ENUM_CONST_TAG   = $25;
    /// Type-registry entry. Carries the enum/class/record's primary
    /// 2-byte type id immediately after the name.
    TYPE_REGISTRY_TAG = $2A;
    /// Record-name sentinel (precedes every record name in the type
    /// stream). Classes don't have a sentinel.
    RECORD_SENTINEL  = $0E;
    /// Scope-end marker. Closes a proc scope when SCOPE_END fires
    /// after at least one local-shaped record has been read since
    /// the most recent PROC_TAG.
    SCOPE_END        = $63;
    /// Enum-type-definition record. Carries the complete element list
    /// of an enumerated type in declaration order plus the owning
    /// unit's name -- the canonical bridge from a (unit, type) pair
    /// to the ordered element names. Layout:
    ///   $03 <NL> <type-name>
    ///       $01 $00 $00 $00 $00 <max-ord> $00
    ///       $00 $00 $00 $00 $00 $00                (12-byte header)
    ///   (<elem-len> <elem-name>) * (max-ord + 1)
    ///   <unit-len> <unit-name>
    /// where <max-ord> = element-count - 1.
    ENUM_DEF_TAG     = $03;
    /// CSH7 file-header signature: 'CSH7' on disk in LE byte order.
    SigCSH7          = UInt32($37485343);
  end;

  /// <summary>
  ///   A single enum-element entry: its identifier name plus the
  ///   explicit ordinal value the source assigned to it. Delphi
  ///   enums need NOT start at 0 and are not required to be
  ///   contiguous (<c>type TFoo = (a = 1, b = 2, c = 5);</c> is a
  ///   legal declaration), so element index in the list is NOT a
  ///   reliable substitute for the actual ordinal -- callers must
  ///   look up by <see cref="Ordinal"/> rather than by list index.
  /// </summary>
  TRsmEnumElement = record
    Name   : String;
    Ordinal: Integer;
  end;

  /// <summary>
  ///   A complete enum-type definition extracted from the RSM's
  ///   <c>$03</c> ENUM_DEF record: the type name, the owning unit's
  ///   name, and the list of <see cref="TRsmEnumElement"/> entries
  ///   that name the enum's constants together with each one's
  ///   explicit ordinal value. This is the authoritative source for
  ///   "which constant lives at which ordinal of which (unit, type)"
  ///   -- the $25 records carry ordinals but not the canonical
  ///   declaration order, and the $2A registry carries the primary
  ///   type id but not the element list. Two enums that share a
  ///   type name (because they're declared in sibling units) produce
  ///   TWO separate entries, one per (unit, type) pair -- the name
  ///   index alone collapses them last-wins.
  /// </summary>
  TRsmEnumDef = record
    TypeName: String;
    UnitName: String;
    Elements: IList<TRsmEnumElement>;
    /// <summary>
    ///   Returns the element whose <see cref="TRsmEnumElement.Ordinal"/>
    ///   matches <paramref name="AOrdinal"/>, or False when no
    ///   element has that ordinal (the value is out of the enum's
    ///   declared range OR the enum is sparse and AOrdinal sits in
    ///   one of the gaps).
    /// </summary>
    function TryFindByOrdinal(AOrdinal: Integer; out AName: String): Boolean;
  end;

/// <summary>
///   Builds the conventional Delphi enum-constant name prefix from a
///   type name: every uppercase letter of the type's name (with any
///   leading 'T' already stripped) is taken and lowercased. So
///   <c>WindowState</c> -> <c>"ws"</c>, <c>ThreadPriority</c> ->
///   <c>"tp"</c>, <c>FontStyle</c> -> <c>"fs"</c>. Matches the
///   Delphi convention where each enum element starts with this
///   acronym (wsNormal, tpHigher, fsBold). Used by the name-based
///   enum resolver to pick the right constant when a primary type id
///   has been aliased to multiple secondaries.
/// </summary>
function BuildPascalAcronym(const AStrippedTypeName: String): String;

implementation

function BuildPascalAcronym(const AStrippedTypeName: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AStrippedTypeName) do
    if CharInSet(AStrippedTypeName[I], ['A'..'Z']) then
      Result := Result + LowerCase(AStrippedTypeName[I]);
end;

{ TRsmEnumDef }

function TRsmEnumDef.TryFindByOrdinal(AOrdinal: Integer;
  out AName: String): Boolean;
var
  I: Integer;
begin
  AName  := '';
  Result := False;
  if Elements = nil then Exit;
  for I := 0 to Elements.Count - 1 do
    if Elements[I].Ordinal = AOrdinal then
    begin
      AName  := Elements[I].Name;
      Result := True;
      Exit;
    end;
end;

end.
