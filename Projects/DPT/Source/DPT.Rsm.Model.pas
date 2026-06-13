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
  ///
  ///   <c>lkRegisterResident</c> (§6.35) is a non-parameter LOCAL that the
  ///   optimiser kept wholly in a CPU register with no stack home at all.
  ///   It is emitted with the <c>$16 00 00 &lt;type&gt; &lt;2*regindex&gt;</c>
  ///   LOCAL form (vs the stack form's <c>$66 00 00 &lt;type&gt;
  ///   &lt;2*offset&gt;</c>); the register is carried in <c>CpuRegIndex</c>
  ///   (not <c>RegParamIdx</c>, whose index space is the param-only
  ///   EAX/EDX/ECX order).
  /// </summary>
  TRsmLocalKind = (lkBpRel, lkRegister, lkRegisterResident);

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
    /// <summary>
    ///   §6.35: which CPU register holds the value when
    ///   <c>Kind = lkRegisterResident</c>. Zero-based index into the
    ///   allocatable x86 GP set <c>[EAX, ECX, EDX, EBX, ESI, EDI]</c>
    ///   (so 3 = EBX, 4 = ESI, 5 = EDI), decoded as the `$16` form's
    ///   byte+4 div 2. Meaningless for other kinds.
    /// </summary>
    CpuRegIndex: Byte;
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
    /// File offset of this proc's <c>$28</c> tag byte in the RSM
    /// symbol stream. Used to bind the proc to its declaring source
    /// file by nearest-preceding-<c>$70</c> proximity — see
    /// <c>TRsmReader.DeclaringUnitOfProc</c> and §4.18.
    StreamOffset : NativeUInt;
    /// Foreign key into <c>TRsmScanner.SourceFiles</c> naming the
    /// source file (and therefore the declaring unit) this proc was
    /// compiled from, stamped from the live <c>$70</c> source-file
    /// introducer cursor at scan time. <c>-1</c> for procs that
    /// precede the first <c>$70</c> introducer — in practice the
    /// linker-synthesised import thunks (<c>MoveFile</c>,
    /// <c>CloseHandle</c>, …), which genuinely have no Delphi
    /// declaring unit in the <c>.rsm</c>. See §4.18.
    SourceFileIdx: Integer;
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
    /// File-offset-based <c>TypeIdx</c> of the record this field's
    /// declared type POINTS AT, for pointer-to-record fields like
    /// <c>FRecPtr: PMixedRec</c> or <c>FAd: PAd</c>. Zero for every
    /// other shape (class-typed, inline-record-typed, primitive-typed).
    /// Populated by <c>TRsmFormatALinker.ScanTypeRegistry</c> /
    /// <c>LinkFieldsFromFormatA</c> when the field's declared type
    /// resolves to a Delphi pointer alias following the
    /// <c>P&lt;X&gt; = ^T&lt;X&gt;</c> naming convention. Drives the
    /// §6.19 closure in <c>DPT.Debugger.EvaluateVariable</c>: when the
    /// inter-segment context-priming sees this field is non-zero, it
    /// performs the pointer dereference and routes the next segment
    /// through the record-hop branch directly, bypassing the §6.18
    /// name-based fallback (whose unique-match guard would bail on
    /// member-name collisions like TFW's <c>Land</c>).
    PointerTargetTypeIdx: UInt32;
  end;

  /// <summary>
  ///   A read-accessible Delphi property declared on a class, parsed
  ///   from a <c>$31</c> property record in the RSM symbol stream.
  ///   Properties come in two flavours the compiler emits identically
  ///   at the record level but link differently:
  ///   <list type="bullet">
  ///     <item><c>property Foo: Integer read FFoo;</c> — the target
  ///       id (<see cref="TargetId"/>) equals the secondary id of an
  ///       existing <c>$2C</c> field record. The reader can resolve
  ///       <c>Obj.Foo</c> to the same byte offset as
  ///       <c>Obj.FFoo</c>.</item>
  ///     <item><c>property Foo: Integer read GetFoo;</c> — the
  ///       target id points at a getter method (no $2C field has it).
  ///       Live evaluate cannot read the property without calling
  ///       the getter, but the reader still surfaces the property
  ///       and the caller can fall back accordingly.</item>
  ///   </list>
  ///   <see cref="UnderlyingField"/> is set to the matching field
  ///   name when the bridge succeeds, empty otherwise.
  /// </summary>
  TRsmClassProperty = record
    Name           : String;
    /// 2-byte alias the Delphi linker emitted as the property's read
    /// target (byte +10..+11 of the $31 record body). Compared
    /// against each $2C field record's secondary id (byte +7..+8) to
    /// resolve field-backed properties.
    TargetId       : UInt16;
    /// 1-byte primitive type id sitting at $31-record body+3 (the
    /// same value the matching $2C field record carries). $02 =
    /// Integer, $04 = string, etc. Zero when the type is structured
    /// rather than primitive.
    PrimitiveTypeId: UInt16;
    /// Field name when <see cref="TargetId"/> matched a known field
    /// (i.e. the property is field-backed). Empty for getter-backed
    /// properties.
    UnderlyingField: String;
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
    ///   Properties declared on this class or record, parsed from
    ///   <c>$31</c> property records. Nil for classes/records that
    ///   declare no properties (the list is allocated lazily on
    ///   first attribution). Records carry $31 entries identically
    ///   to classes -- Delphi records have supported properties
    ///   since the language got method/operator overloading.
    ///   See <see cref="TRsmClassProperty"/> for the per-property
    ///   bridge to the underlying field.
    /// </summary>
    Properties: IList<TRsmClassProperty>;
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
  ///   Kind of a cross-unit reference entry inside a §6.21
  ///   <c>$64</c> unit-use segment: a type reference (<c>$66</c>),
  ///   a symbol reference (<c>$67</c>) such as an enum element or
  ///   a procedure / method, or a source-file reference
  ///   (<c>$70</c>) carrying the imported unit's <c>.pas</c> /
  ///   <c>.inc</c> file name. The three kinds share a common
  ///   wire layout (<c>tag NL Name 4-byte-token</c>) but differ in
  ///   what the name space resolves to.
  /// </summary>
  TRsmUnitUseKind = (uukType, uukSymbol, uukFile);

  /// <summary>
  ///   A single reference entry inside a §6.21 unit-use segment.
  ///   <c>LinkToken</c> is the 4-byte LE payload after the name. It is
  ///   an OPAQUE LINKER TOKEN of the same family as the §4.6.2
  ///   <c>$25</c> enum-constant token, NOT an in-image RVA (a former
  ///   §6.29 side-item mislabelled it): for a <c>$67 'ltInland'</c>
  ///   element it equals the canonical <c>$25 'ltInland'</c> block's
  ///   +3..+6 bytes (pinned), and its magnitude (e.g. <c>$62BC8138</c>
  ///   for <c>$66 'Boolean'</c> ≈ 1.66 GB) far exceeds any image, so it
  ///   cannot be an RVA. The decoder exposes it raw; no consumer does
  ///   address arithmetic on it. See §4.17.
  /// </summary>
  TRsmUnitUseRef = record
    Kind: TRsmUnitUseKind;
    Name: String;
    LinkToken: UInt32;
  end;

  /// <summary>
  ///   One <c>$64</c> cross-unit symbol-import segment (§6.21):
  ///   the imported unit's name plus the ordered list of type /
  ///   symbol / source-file references appearing between this
  ///   <c>$64</c> introducer and the next <c>$63 SCOPE_END</c>
  ///   (or the next non-{$66/$67/$70} byte that ends the run
  ///   without an explicit close).
  ///   The <c>StartOffset</c> is the file offset of the <c>$64</c>
  ///   tag itself -- consumers (the §6.20 closure path being one)
  ///   use it to associate a segment with an enclosing scope by
  ///   file-offset proximity (the same pattern §6.10's BlockOwner
  ///   walk and the §6.9 nearest-$2A bridge already use).
  /// </summary>
  TRsmUnitUseSegment = record
    UnitName     : String;
    StartOffset  : NativeUInt;
    /// Index into <see cref="TRsmScanner.SourceFiles"/> of the
    /// <c>$70</c> source-file record that introduces this segment's
    /// block (the unit whose <c>uses</c> this segment belongs to), or
    /// <c>-1</c> when the segment was seen before any <c>$70</c>
    /// introducer. This is a normalized foreign key: the importing
    /// unit's name lives ONCE in <c>SourceFiles[SourceFileIdx]</c>, not
    /// copied per segment. See §4.17.
    SourceFileIdx: Integer;
    Refs         : IList<TRsmUnitUseRef>;
  end;

  /// <summary>
  ///   One <c>$70 &lt;SourceFile&gt;</c> record (§4.17). In the RSM
  ///   stream each compilation unit is declared by its source-file
  ///   name; when the unit has a <c>uses</c> clause, the <c>$70</c>
  ///   record INTRODUCES the run of <c>$64</c> segments that follow
  ///   (those segments' <c>SourceFileIdx</c> points back here). The
  ///   record is named after what it carries on the wire -- a source
  ///   file -- not after the importing role it plays relative to the
  ///   following segments.
  /// </summary>
  TRsmSourceFile = record
    /// The raw <c>$70</c> name including directory and extension, e.g.
    /// <c>'DPT.Application.pas'</c> or
    /// <c>'..\..\sys\System.SysConst.pas'</c>.
    SourceFile : String;
    /// The Delphi unit name derived from <c>SourceFile</c> (directory
    /// prefix and <c>.pas</c>/<c>.inc</c> extension stripped), e.g.
    /// <c>'DPT.Application'</c>, <c>'System.SysConst'</c>.
    UnitName   : String;
    /// File offset of the <c>$70</c> tag byte.
    StartOffset: NativeUInt;
    /// The 4-byte LE payload following the name. An OPAQUE LINKER TOKEN
    /// of the same family as the <c>$66</c>/<c>$67</c> ref payloads
    /// (see <see cref="TRsmUnitUseRef.LinkToken"/>), NOT an image RVA.
    LinkToken  : UInt32;
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
    /// Property-declaration record tag (Format-A property). One per
    /// `property &lt;Name&gt;: &lt;Type&gt; read &lt;Field-or-Getter&gt;`
    /// clause. Layout:
    ///   $FF $31 &lt;NL&gt; &lt;Name&gt; $00 $02 $00 &lt;prim-type: u8&gt;
    ///       $FE $0F $00 $00 $00 $80 &lt;target-lo&gt; &lt;target-hi&gt; ...
    /// where &lt;target&gt; matches a $2C field record's secondary id
    /// (byte +7..+8) for field-backed properties, or points at a
    /// method otherwise.
    PROPERTY_TAG     = $31;
    /// Cross-unit symbol-import segment introducer (§6.21). Opens a
    /// per-unit symbol-import block:
    ///   $64 &lt;NL&gt; &lt;UnitName&gt; $00 $00 $00
    /// followed by zero or more $66 / $67 / $70 reference entries,
    /// closed by $63 SCOPE_END (or end-of-block).
    UNIT_USE_INTRO   = $64;
    /// Cross-unit type reference (§6.21). One per imported type
    /// referenced inside the surrounding $64 segment:
    ///   $66 &lt;NL&gt; &lt;TypeName&gt; &lt;4-byte LE token&gt;
    /// The 4-byte payload is an opaque linker token (NOT an image RVA),
    /// matching the $2A &lt;TypeName&gt; entry's body bytes +3..+6.
    UNIT_USE_TYPE    = $66;
    /// Cross-unit symbol reference (§6.21). Enum elements, procs,
    /// methods imported by the surrounding $64 segment:
    ///   $67 &lt;NL&gt; &lt;SymbolName&gt; &lt;4-byte LE token&gt;
    /// Same payload shape as $66; siblings of a single enum carry
    /// tokens stepping by +3 across the ordinals (the §4.6.2 family).
    UNIT_USE_SYMBOL  = $67;
    /// Used-unit list opener. Follows the program / package main
    /// file's $70 introducer (the .dpr / .dpk full-path source-file
    /// record) in place of the $64 import-segment opener that an
    /// imported unit's $70 introducer carries. Accepting this as a
    /// valid introducer-follower is what lets the source-file cursor
    /// advance onto the program module so its procs resolve to the
    /// program unit — see §4.18.
    USED_UNIT_LIST   = $65;
    /// Cross-unit source-file reference (§6.21) AND, in the proc
    /// stream, the per-unit source-file introducer that anchors
    /// proc → declaring-unit (§4.18). Carries the source-file name:
    ///   $70 &lt;NL&gt; &lt;FileName.pas|.inc|.dpr|.dpk&gt; &lt;4-byte LE token&gt;
    UNIT_USE_FILE    = $70;
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
    ///   True when this def was SYNTHESIZED by the same-compilation
    ///   <c>$2A</c> flush (<c>TRsmEnumDecoder.RecordTypeRegistry</c>)
    ///   rather than parsed from an authoritative <c>$03 ENUM_DEF</c>
    ///   record. The synthesis is necessary for sparse same-comp enums
    ///   (which the linker emits no <c>$03</c> for) but it also guesses,
    ///   so it can attribute another enum's pending <c>$25</c> constants
    ///   to a non-enum <c>$2A</c> (a class/record/interface), producing a
    ///   phantom enum. <c>$03</c>-sourced defs leave this False and are
    ///   authoritative; the reader's post-process drops synthesized defs
    ///   already covered by a <c>$03</c> (§6.25 R1).
    /// </summary>
    Synthesized: Boolean;
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

uses

  System.SysUtils;

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
