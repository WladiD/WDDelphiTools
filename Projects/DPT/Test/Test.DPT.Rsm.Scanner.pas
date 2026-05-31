// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Rsm.Scanner;

// Direct tests for TRsmScanner: loads the DebugTarget .rsm fixture
// and asserts the raw scan output (procs, classes, globals, enum
// constants, type aliases). These tests bypass the
// TRsmReader facade so a regression in the byte-stream walk
// can be pinned to the scanner and not the post-process passes
// that sit on top.

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TRsmScannerTests = class
  private
    function ResolveExePath(AUse64Bit: Boolean): String;
    procedure DoTestGlobalVADecodedFromGlobalRecord(AUse64Bit: Boolean);
    procedure DoTestNonFPrefixClassFieldsDiscovered(AUse64Bit: Boolean);
    procedure DoTestSimpleRecordHeaderFieldCount(AUse64Bit: Boolean);
  public
    /// Loading a non-existent EXE path leaves the scanner empty and
    /// does not raise. Mirrors the reader's behaviour.
    [Test]
    procedure TestLoadFromMissingFileLeavesEmpty;
    /// LoadFromBuffer on a buffer that does NOT start with the
    /// CSH7 magic returns without populating anything.
    [Test]
    procedure TestLoadFromGarbageBufferLeavesEmpty;

    /// Procs collected from a real Win32 RSM. Sanity checks the
    /// known top-level fixture procedures.
    [Test]
    procedure TestProcsCollected32;
    /// Classes / records collected from Win32 RSM. The DebugTarget
    /// fixture defines TBase, TDerived, TLightStatus host record and
    /// several other shapes; we assert the byte-level scan caught
    /// at least one of each kind.
    [Test]
    procedure TestStructsCollected32;
    /// Module-level global registered. GGlobalLight (typed
    /// TLightStatus) must appear in GlobalByName so the dotted-walk
    /// can route through it.
    [Test]
    procedure TestGlobalsRegistered32;
    /// Enum constants registered. The $25 program-local form
    /// puts lsGreen / lsYellow / lsRed under TLightStatus's $2E
    /// type id.
    [Test]
    procedure TestProgramLocalEnumConstantsRegistered32;
    /// Cross-unit enum aliasing. tpHigher (TThreadPriority,
    /// System.Classes) is emitted with the $25 $8A-prefix form;
    /// the scanner must capture it as a cross-unit enum id and
    /// the $2A registry entry must link the primary alias to it.
    [Test]
    procedure TestCrossUnitEnumIdRegistered32;

    /// Closure pin for the $2A-flush EnumDef over-collection (the
    /// RsmDesk "TColorRef (TAlphaColorRec.Create) = 285 elements"
    /// report; §6.25). Core invariant: a TRsmEnumDef's ordinals are
    /// UNIQUE -- a real Delphi enum cannot have two elements sharing an
    /// ordinal, so the over-collected pending buffer (ord 0 = crDefault
    /// AND mrNone AND sgiNoGesture, ...) is suppressed by the
    /// duplicate-ordinal synthesis guard. Concrete: cr* and vk* const
    /// families never co-occur in one def. Leakage guards: the legit
    /// $03 / same-comp enums survive (TTypeKind keeps 23 elements;
    /// TLightStatus + a same-comp TStatus remain) AND the total count
    /// does not collapse -- the latter guards the OPPOSITE failure of an
    /// earlier over-aggressive attempt that rejected class-method unit
    /// names and nuked legit enums (DPT.rsm 95 -> 22). NB: DebugTarget is
    /// too clean to exhibit that over-removal (it stayed ~127 either
    /// way); the real-binary collapse was caught manually against
    /// DPT.rsm, see §6.25.
    [Test]
    procedure TestEnumDefsNotOverCollected32;

    /// Pins the finding for the $2A "KindFlag" byte at body offset +0
    /// (RSM format reference, gap 6.6). It is NOT a type-kind
    /// discriminator: classes, records and enums each appear with
    /// MULTIPLE flag values across the file. What it does carry is a
    /// body-shape selector -- Flag=$00 marks a "narrow" entry whose
    /// payload after the primary id is a single $00 pad byte;
    /// Flag=$20 marks a "wide" entry whose payload at +5..+8 is a
    /// non-zero 4-byte block (the secondary candidate the existing
    /// scanner already reads at +7,+8 for enum-bridging). This test
    /// captures both shapes against the DebugTarget fixture.
    [Test]
    procedure Test2ATypeRegistryFlagIsBodyShapeNotKind32;

    /// Pins the $27 / $20 module-global VA decoding (RSM format
    /// gap §6.8 closed). The 4-byte slot after the type id encodes
    /// (Value shl 4) | $07, same scheme as $28 PROC_TAG. On Win32
    /// the decoded Value is the absolute VA (image base already
    /// included); on Win64 it is the RVA relative to image base
    /// $140000000. Compares against ground truth from the .map
    /// file for both $27 (primitive: GGlobalInt) and $20
    /// (structured: GFieldHost) forms.
    [Test]
    procedure TestGlobalVADecodedFromGlobalRecord32;
    [Test]
    procedure TestGlobalVADecodedFromGlobalRecord64;

    /// Pins the non-F-prefixed class-field decoding (RSM format
    /// §6.3 closed). The backward class-field walker no longer
    /// uses the Name[1]='F' heuristic; it now anchors on the
    /// 4-byte typeinfo prefix `$02 $00 <last-flag> $00` immediately
    /// after the field name. DebugTarget's TNoFPrefixHost class
    /// declares PlainInt + PlainLabel (no F-prefix). After scanning,
    /// both fields must surface in Classes[TNoFPrefixHost].Members.
    [Test]
    procedure TestNonFPrefixClassFieldsDiscovered32;
    [Test]
    procedure TestNonFPrefixClassFieldsDiscovered64;

    /// Pins the simple-shape record header (RSM format §4.13 / §6.4
    /// closure). The bytes between a record-name's size DWORD and the
    /// first $02 field tag follow:
    ///   byte 0       : managed-field count (N)
    ///   ...padding   : platform-specific
    ///   byte 5+N*K   : declared field count
    /// with K=8 on Win32 and K=16 on Win64. For each DebugTarget
    /// simple record (no nested sub-record headers), both quantities
    /// must match expectations. The elaborate TAppCaps-style shape is
    /// out of scope (see §6.4).
    [Test]
    procedure TestSimpleRecordHeaderFieldCount32;
    [Test]
    procedure TestSimpleRecordHeaderFieldCount64;

    /// <summary>
    ///   Pin test for §6.14 closure -- the class-field anchor byte +2
    ///   visibility/section marker taxonomy. Verifies that all seven
    ///   TNoFPrefixHost fields (one per visibility section: private,
    ///   strict private, protected×2, strict protected, public,
    ///   published) are discovered after the walker predicate widened
    ///   from `[$00..$02]` to `[$00..$0F]`, and pins the observed
    ///   marker values for each section against the byte stream.
    /// </summary>
    [Test]
    procedure TestVisibilityMarkerTaxonomy32;

    /// <summary>
    ///   Pin test for §6.6.2 closure -- the $2A body-flag
    ///   discriminator for records: a record gets `$20` iff it
    ///   has at least one RTTI-managed field OR it is referenced
    ///   as a non-variant field type of another record/class.
    ///   Variant-case references do NOT promote the flag (the
    ///   canonical case: TEnumHostRec is referenced only via
    ///   TEnumVariantHost.FInner inside a variant case branch,
    ///   and stays `$00`). Pins all 14 DebugTarget records'
    ///   flags against the rule.
    /// </summary>
    [Test]
    procedure TestRecordRttiFlagDiscriminator32;

    /// <summary>
    ///   Pin test for §6.13 closure -- the terminal field of a
    ///   record now reports a non-zero Size, derived from the
    ///   record's total byte size (4-byte DWORD between the
    ///   record name and the field-record stream).
    /// </summary>
    [Test]
    procedure TestTerminalRecordFieldSizeRecovered32;

    /// <summary>
    ///   Pin test for §6.6.1 closure -- the $2A wide-body ($20 flag)
    ///   slot at body+5..+8 is a Win32-style (VA shl 4) | $07
    ///   pointer into the type's RTTI structure. For each
    ///   $20-flagged class probe, decodes the DWORD, applies the
    ///   `(DW >> 4)` formula, and asserts the recovered VA sits in
    ///   the .text section's plausible window (image base + .text
    ///   RVA &lt;= VA &lt; image base + 256 MB) AND that consecutive
    ///   probes are monotonically increasing (the linker emits RTTI
    ///   in declaration order). This catches a regression that
    ///   re-interprets the slot as the old 2-byte SecCandidate at
    ///   +7/+8.
    /// </summary>
    [Test]
    procedure TestTypeRegistryWideBodyHoldsRttiVA32;

    /// <summary>
    ///   Pin test for §6.5 closure -- the 4-byte slot at body+3..+6
    ///   of the $25 cross-unit RTL form is NOT an RVA into the loaded
    ///   binary. The DebugTarget Win32 and Win64 .rsm files contain
    ///   byte-identical token values for the same source-level enum
    ///   constants (verified by dumping all 7 TThreadPriority
    ///   elements on both platforms), and the values follow the
    ///   linear pattern `base + ord * 3` across an enum's elements.
    ///   The slot is an opaque linker / DCU-internal token; the
    ///   decoder correctly skips it. This test pins the linear-
    ///   stride invariant so a future encoder change that breaks it
    ///   surfaces as a regression rather than silently shifting an
    ///   undocumented field.
    /// </summary>
    [Test]
    procedure TestCrossUnitRtlLinkerTokenIsLinearStride32;

    /// Pins the actual sparse-ordinal enum encoding (RSM format §6.1).
    /// The original "different $03 emission shape" hypothesis is
    /// REFUTED: the linker emits NO $03 ENUM_DEF record at all for
    /// sparse enums. The per-element ordinals come through the
    /// existing $25 program-local channel and decode correctly today
    /// (seAlpha=1, seBeta=5, seGamma=11 for TSparseEnum). The
    /// consequence is that <c>EnumDefs</c> does not list TSparseEnum;
    /// callers must use <c>TryGetEnumConstantName</c> or another
    /// resolver path that doesn't rely on the EnumDef list.
    [Test]
    procedure TestSparseEnumResolvesViaEnumConstNames32;

    /// §6.15 PIN. Register-passed parameters carry their typeId in a
    /// $21 REGVAR record whose payload is `$66 $00 $00 <typeIdLo>
    /// <typeIdHi> <pad> <next-record-tag>` for the 2-byte form, or
    /// `$66 $00 $00 <typeId> <pad> <next-record-tag>` for the 1-byte
    /// primitive form. The pre-§6.15 scanner gated the 2-byte read on
    /// `Hi in {$2E, $2F}` — true for program-local types but missed
    /// cross-unit RTL types whose per-binary alias id has a varying
    /// Hi byte (observed: $06 for TThreadPriority in DebugTarget).
    /// Without the fix the alias was truncated to its low byte ($71),
    /// colliding with whatever foreign type happened to be registered
    /// at id $71 in the target's class table.
    ///
    /// This test pins the structural-lookahead disambiguator against
    /// `TouchRegEnumParam(AStatusLight: TLightStatus; AStatusPriority:
    /// TThreadPriority)` (see DebugTarget.dpr line 516):
    ///   - AStatusLight (prog-local TLightStatus) keeps decoding to
    ///     TLightStatus's primary $2E81 — the $2E/$2F fast path stays
    ///     intact.
    ///   - AStatusPriority (cross-unit RTL TThreadPriority) now
    ///     decodes to the per-binary alias $0671 (full 2-byte read)
    ///     instead of the truncated $71. The alias→primary bridge
    ///     is the residual §6.15 follow-up — pinning the correct
    ///     2-byte read here closes the truncation collision, which
    ///     was the user-visible symptom (live evaluate returning
    ///     "record TSatz33" in TFW).
    [Test]
    procedure TestRegisterParamEnumTypeIdNotTruncated32;

    /// <summary>
    ///   §6.21 pin: the cross-unit symbol-import segment introducer
    ///   <c>$64 NL 'System' $00 $00 $00</c> opens a unit-use block
    ///   whose <c>$66</c> type-references include common
    ///   <c>System</c>-unit primitives that the Delphi linker
    ///   unconditionally imports. We assert that the FIRST <c>System</c>
    ///   segment in DebugTarget.rsm carries a <c>$66 'Boolean'</c>
    ///   reference (one of the first entries in the segment) with the
    ///   pinned canonical RVA <c>$62BC8138</c> -- the exact 4 bytes
    ///   the linker emitted at offset +11 of the <c>$64 'System'</c>
    ///   anchor (file offset 388467).
    /// </summary>
    [Test]
    procedure TestUnitUseSegmentSystemIsDecoded;
    /// <summary>
    ///   §6.21 pin: <c>$67</c> symbol-reference payloads follow the
    ///   +3 stride pattern across siblings of a single enum. We pin
    ///   the <c>System.UITypes</c> <c>vk*</c> sequence: <c>vkLButton
    ///   = $31E77DAF</c>, <c>vkRButton = $31E77DB2</c>, <c>vkCancel
    ///   = $31E77DB5</c>. The conserved high bytes plus the +3 LSB
    ///   stride are the structural finding from §6.20-Round-3.
    /// </summary>
    [Test]
    procedure TestUnitUseSymbolReferencePayloadHasPlusThreeStride;
    /// <summary>
    ///   §6.21 leakage guard: a long byte run of <c>$64</c> bytes
    ///   inside another record's payload must NOT spuriously open
    ///   a segment. We pass a synthetic buffer whose <c>$64</c>
    ///   byte is followed by a plausible name length but the three
    ///   trailing zero anchor bytes are absent; the scanner must
    ///   reject the shape and leave <c>UnitUseSegments</c> empty.
    /// </summary>
    [Test]
    procedure TestUnitUseFalsePositiveRejection;

    /// <summary>
    ///   §6.22 closure pin (Win32). Every <c>$28</c> PROC record on
    ///   Win32 carries a marker `<c>$04 ?? ?? ?? ?? ?? $11 $2E
    ///   &lt;owner-ref&gt;</c>` after its proc-address payload, where
    ///   the bytes after <c>$11 $2E</c> are either a single sentinel
    ///   <c>$00</c> (plain proc) or a 2-byte LE pair encoding the
    ///   owning class's <c>$2A</c> raw type-id (instance method).
    ///   See §4.1 for the full table. This pin asserts: plain
    ///   <c>TargetProcedure</c> ends with <c>$11 $2E $00</c>;
    ///   instance method <c>TDerived.TouchSelf</c> ends with
    ///   <c>$11 $2E &lt;TDerived's raw id LE&gt;</c>; siblings of
    ///   the same class (<c>TPropHost.Create</c> + <c>TPropHost.</c>
    ///   <c>GetCalcInt</c>) carry the SAME owner-ref pair (the
    ///   intra-class consistency check).
    /// </summary>
    [Test]
    procedure TestProcMarkerOwnerRefDecodes32;

    {$IFDEF CPUX64}
    /// <summary>
    ///   §6.22 closure pin (Win64). Same shape as the Win32 sibling
    ///   above but the platform-anchor byte is <c>$3D</c> instead of
    ///   <c>$11</c>, and the class raw-ids differ (Win64 typically
    ///   assigns ids in the <c>$2E00..$2FFF</c> range too but with
    ///   different per-class values). Asserts the plain-proc
    ///   <c>$00</c> sentinel + the intra-class consistency in the
    ///   Win64 fixture.
    /// </summary>
    [Test]
    procedure TestProcMarkerOwnerRefDecodes64;
    {$ENDIF}

    {$IFDEF CPUX64}
    /// Win64 sanity. Same as TestProcsCollected32 but on the Win64
    /// fixture; structures are encoded differently (Win64 trailer
    /// pattern with 8-byte zero pad) so the scanner must handle
    /// both inside the same code path.
    [Test]
    procedure TestProcsCollected64;
    [Test]
    procedure TestStructsCollected64;
    [Test]
    procedure TestGlobalsRegistered64;
    {$ENDIF}
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,

  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.Scanner;

function TRsmScannerTests.ResolveExePath(AUse64Bit: Boolean): String;
var
  Sub: String;
begin
  if AUse64Bit then
    Sub := 'Win64'
  else
    Sub := 'Win32';
  Result := ExpandFileName('Projects\DPT\Test\' + Sub + '\DebugTarget.exe');
  if not TFile.Exists(Result) then
    Result := ExpandFileName(Sub + '\DebugTarget.exe');
end;

procedure TRsmScannerTests.TestEnumDefsNotOverCollected32;

  // True when AName is a Delphi type identifier: T (class) / E (exception)
  // / I (interface) immediately followed by an uppercase letter. A
  // synthesised EnumDef's UnitName must be a unit, never such a type
  // ident (which would mean the forward-scan grabbed a "TClass.Method").
  function FirstSegLooksLikeType(const AUnit: String): Boolean;
  var
    Seg: String;
    D  : Integer;
  begin
    Seg := AUnit;
    D := Pos('.', Seg);
    if D > 0 then Seg := Copy(Seg, 1, D - 1);
    Result := (Length(Seg) >= 2) and CharInSet(Seg[1], ['T', 'E', 'I']) and
              CharInSet(Seg[2], ['A'..'Z']);
  end;

var
  S          : TRsmScanner;
  I, J, K    : Integer;
  Def        : TRsmEnumDef;
  HasTTypeKind, HasLight, HasStatus: Boolean;
  StatusUnit : String;
  HasCr, HasVk: Boolean;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.EnumDefs.Count > 0, 'no EnumDefs from DebugTarget');

    HasTTypeKind := False; HasLight := False; HasStatus := False;
    StatusUnit := '';
    for I := 0 to S.EnumDefs.Count - 1 do
    begin
      Def := S.EnumDefs[I];
      if Def.Elements = nil then Continue;

      // Unit-name invariant (§6.25): a synthesised EnumDef's UnitName is
      // a UNIT, never a "TClass.Method". The forward-scan grabbed the
      // first dotted proc after the $2A (a class method like
      // "TDcuDiff.ListEntries") instead of the unit-init proc; the fix
      // searches wider for the first clean dotted namespace.
      Assert.IsFalse(FirstSegLooksLikeType(Def.UnitName),
        Format('EnumDef "%s" UnitName "%s" is a TClass.Method, not a unit',
          [Def.TypeName, Def.UnitName]));

      // Core invariant: ordinals must be UNIQUE within a single enum.
      // A real Delphi enum cannot have two elements sharing an ordinal;
      // the over-collected pending buffer did (ord 0 = crDefault AND
      // mrNone AND sgiNoGesture). The duplicate-ordinal synthesis guard
      // suppresses those bogus defs.
      for J := 0 to Def.Elements.Count - 1 do
        for K := J + 1 to Def.Elements.Count - 1 do
          Assert.AreNotEqual<Integer>(Def.Elements[J].Ordinal,
            Def.Elements[K].Ordinal,
            Format('EnumDef "%s" (%s) has duplicate ordinal %d ' +
              '(elements "%s" and "%s") -- over-collected pending buffer',
              [Def.TypeName, Def.UnitName, Def.Elements[J].Ordinal,
               Def.Elements[J].Name, Def.Elements[K].Name]));

      // Concrete signature of the TColorRef over-collection: cr* and vk*
      // const families merged into one def.
      HasCr := False; HasVk := False;
      for J := 0 to Def.Elements.Count - 1 do
      begin
        if SameText(Def.Elements[J].Name, 'crDefault') then HasCr := True;
        if SameText(Def.Elements[J].Name, 'vkLButton') then HasVk := True;
      end;
      Assert.IsFalse(HasCr and HasVk,
        Format('EnumDef "%s" merges cr* and vk* const families', [Def.TypeName]));

      // Leakage guard: the legit $03-sourced / same-comp enums survive.
      if SameText(Def.TypeName, 'TTypeKind') and (Def.Elements.Count = 23) then
        HasTTypeKind := True;
      if SameText(Def.TypeName, 'TLightStatus') then HasLight := True;
      if SameText(Def.TypeName, 'TStatus') then
      begin
        HasStatus := True;
        StatusUnit := Def.UnitName;
      end;
    end;

    Assert.IsTrue(HasTTypeKind, 'legit TTypeKind (23 elements) lost');
    Assert.IsTrue(HasLight, 'legit TLightStatus lost');
    Assert.IsTrue(HasStatus, 'legit same-comp TStatus lost');
    // Concrete unit-name resolution on a controlled same-comp enum:
    // DebugTarget's sibling-unit TStatus must resolve to its DebugTarget
    // unit, not a method of some class.
    Assert.IsTrue(StatusUnit.StartsWith('DebugTarget'),
      Format('TStatus UnitName "%s" did not resolve to a DebugTarget unit',
        [StatusUnit]));
    // Over-removal regression guard. The fix must drop only the handful
    // of duplicate-ordinal over-collections, NOT a large slice of the
    // EnumDef list. (The first fix attempt also rejected any synthesised
    // def whose forward-scanned unit name looked like a TClass.Method --
    // which on a real binary nukes legit same-comp enums whose unit-init
    // proc the forward-scan missed; DebugTarget alone didn't reveal it,
    // DPT.rsm collapsed 95 -> 22. Guard against that direction here.)
    Assert.IsTrue(S.EnumDefs.Count >= 120,
      Format('EnumDef count %d too low -- the over-collection fix is ' +
        'removing legit enums, not just duplicate-ordinal garbage',
        [S.EnumDefs.Count]));
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestLoadFromMissingFileLeavesEmpty;
var
  S: TRsmScanner;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile('Z:\definitely-does-not-exist\nope.exe');
    Assert.AreEqual<Integer>(0, S.Procs.Count, 'Procs empty');
    Assert.AreEqual<Integer>(0, S.Classes.Count, 'Classes empty');
    Assert.IsNull(S.Buf, 'Buf nil after failed map');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestLoadFromGarbageBufferLeavesEmpty;
var
  S    : TRsmScanner;
  Junk : array[0..15] of Byte;
begin
  // Non-CSH7 header: scanner returns without populating.
  FillChar(Junk, SizeOf(Junk), $FF);
  S := TRsmScanner.Create;
  try
    S.LoadFromBuffer(@Junk[0], SizeOf(Junk));
    Assert.AreEqual<Integer>(0, S.Procs.Count);
    Assert.AreEqual<Integer>(0, S.Classes.Count);
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestProcsCollected32;
var
  S: TRsmScanner;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Procs.Count > 0, 'expected procs from DebugTarget');
    Assert.IsTrue(S.ProcByName.ContainsKey('locallaprocedure') or
                  S.ProcByName.ContainsKey('localsprocedure'),
      'expected a known fixture proc in ProcByName index');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestStructsCollected32;
var
  S   : TRsmScanner;
  HasClass, HasRecord: Boolean;
  I   : Integer;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    HasClass  := False;
    HasRecord := False;
    for I := 0 to S.Classes.Count - 1 do
    begin
      if S.Classes[I].Kind = skClass  then HasClass  := True;
      if S.Classes[I].Kind = skRecord then HasRecord := True;
      if HasClass and HasRecord then Break;
    end;
    Assert.IsTrue(HasClass, 'expected at least one class in DebugTarget');
    Assert.IsTrue(HasRecord, 'expected at least one record in DebugTarget');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestGlobalsRegistered32;
var
  S      : TRsmScanner;
  TypeId : UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    // GGlobalLight: TLightStatus -- typed enum global emitted via
    // the $20 form. Must appear in GlobalByName with a non-zero
    // type id (enum hi byte $2E).
    Assert.IsTrue(S.GlobalByName.TryGetValue('ggloballight', TypeId),
      'GGlobalLight missing from GlobalByName');
    Assert.IsTrue(TypeId <> 0, 'GGlobalLight type id is zero');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestProgramLocalEnumConstantsRegistered32;
var
  S         : TRsmScanner;
  Pair      : TPair<String, String>;
  FoundGreen: Boolean;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    // We don't know TLightStatus's secondary id up front; iterate
    // and confirm at least one $25-registered constant decoded as
    // "lsGreen" (TLightStatus's element-0 element).
    FoundGreen := False;
    for Pair in S.EnumConstNames do
      if SameText(Pair.Value, 'lsGreen') then
      begin
        FoundGreen := True;
        Break;
      end;
    Assert.IsTrue(FoundGreen, 'lsGreen not registered as an enum constant');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestCrossUnitEnumIdRegistered32;
var
  S         : TRsmScanner;
  Pair      : TPair<String, String>;
  FoundCross: Boolean;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    // tpHigher (TThreadPriority, System.Classes) is emitted via
    // the $25 $8A-prefix form; the scanner must capture it and
    // record the cross-unit secondary id.
    FoundCross := False;
    for Pair in S.EnumConstNames do
      if SameText(Pair.Value, 'tpHigher') then
      begin
        FoundCross := True;
        Break;
      end;
    Assert.IsTrue(FoundCross, 'tpHigher not registered as an enum constant');
    Assert.IsTrue(S.CrossUnitEnumIds.Count > 0,
      'CrossUnitEnumIds empty -- $25 $8A-prefix form not detected');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.Test2ATypeRegistryFlagIsBodyShapeNotKind32;
// Pins the body-shape vs. kind finding for the $2A entry byte at +0.
// We locate the registry entries for three DebugTarget-declared
// program-local types whose primary id hi byte is $2E:
//
//   TInner        - class  - expected Flag=$20 (wide body)
//   TLightStatus  - enum   - expected Flag=$00 (narrow body)
//   TEnumHostRec  - record - expected Flag=$00 (narrow body)
//
// The (class -> $20) and (enum -> $00) pair alone refutes any kind
// hypothesis: an enum and a record share the same Flag, and a class
// uses a different Flag than the same-kind-as-record TEnumHostRec.
// We also assert the shape correlate: Flag=$00 entries have body[5]
// == $00 (single pad byte before the next record); Flag=$20 entries
// have body[5] != $00 (the secondary-candidate / typeinfo-ref slot
// starts there).
type
  TFound = record
    Pos  : NativeInt;
    Flag : Byte;
    B5   : Byte;
    PriHi: Byte;
  end;
var
  S       : TRsmScanner;
  Hits    : IKeyValue<String, TFound>;

  function TryFindRegistryEntry(const AName: String;
    out AFound: TFound): Boolean;
  var
    P, EndPos: NativeInt;
    NL       : Byte;
    Name     : String;
    PriHi    : Byte;
  begin
    Result := False;
    EndPos := S.Sz - 12;
    P := 4;
    while P < EndPos do
    begin
      if S.ByteAt(P) = TRsmTag.TYPE_REGISTRY_TAG then
      begin
        NL := S.ByteAt(P + 1);
        if (NL = Length(AName)) and
           (P + 2 + NL + 10 < S.Sz) and
           (S.ByteAt(P + 2) = Byte(Ord('T'))) and
           (S.ByteAt(P + 2 + NL + 1) = $00) and
           (S.ByteAt(P + 2 + NL + 2) = $00) and
           S.ReadIdentifier(P + 1, Name) and
           SameText(Name, AName) then
        begin
          PriHi := S.ByteAt(P + 2 + NL + 4);
          // Filter to the program-local cluster: hi byte $2E. Any
          // false-positive $2A bytes scattered earlier in the file
          // won't carry $2E in this slot, so we skip them.
          if PriHi = $2E then
          begin
            AFound.Pos   := P;
            AFound.Flag  := S.ByteAt(P + 2 + NL + 0);
            AFound.B5    := S.ByteAt(P + 2 + NL + 5);
            AFound.PriHi := PriHi;
            Exit(True);
          end;
        end;
      end;
      Inc(P);
    end;
  end;

  procedure Expect(const AName: String);
  var
    F: TFound;
  begin
    Assert.IsTrue(TryFindRegistryEntry(AName, F),
      AName + ' has no $2A registry entry with PriHi=$2E');
    Hits[AName] := F;
  end;

var
  ClassEntry, EnumEntry, RecordEntry: TFound;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty -- need DebugTarget.rsm');

    Hits := Collections.NewPlainKeyValue<String, TFound>;
    Expect('TInner');
    Expect('TLightStatus');
    Expect('TEnumHostRec');

    ClassEntry  := Hits['TInner'];
    EnumEntry   := Hits['TLightStatus'];
    RecordEntry := Hits['TEnumHostRec'];

    // The decoded finding: enum and class disagree on Flag, but
    // enum and record AGREE. So Flag does NOT mark the type kind.
    Assert.AreNotEqual<Byte>(ClassEntry.Flag, EnumEntry.Flag,
      'class TInner and enum TLightStatus share the same Flag -- ' +
      'no longer disprovable as a kind discriminator');
    Assert.AreEqual<Byte>(EnumEntry.Flag, RecordEntry.Flag,
      'enum TLightStatus and record TEnumHostRec disagree on Flag -- ' +
      'kind-hypothesis would have required them to agree (both not ' +
      'class) -- need to revisit finding');

    // Pinned values for DebugTarget (program-local cluster). If the
    // linker changes its emission shape these will trip and the
    // doc/§4.8 needs revisiting.
    Assert.AreEqual<Byte>($20, ClassEntry.Flag,
      'TInner Flag drifted from $20');
    Assert.AreEqual<Byte>($00, EnumEntry.Flag,
      'TLightStatus Flag drifted from $00');
    Assert.AreEqual<Byte>($00, RecordEntry.Flag,
      'TEnumHostRec Flag drifted from $00');

    // Body-shape correlate: Flag=$00 entries have a single $00 pad
    // byte at body[5]; Flag=$20 entries open the wide payload at
    // body[5] with a non-zero byte (a secondary candidate the
    // existing scanner already reads at +7,+8).
    Assert.AreNotEqual<Byte>($00, ClassEntry.B5,
      'TInner is a wide-body entry but body[5] is $00');
    Assert.AreEqual<Byte>($00, EnumEntry.B5,
      'TLightStatus is a narrow-body entry but body[5] is non-zero');
    Assert.AreEqual<Byte>($00, RecordEntry.B5,
      'TEnumHostRec is a narrow-body entry but body[5] is non-zero');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.DoTestGlobalVADecodedFromGlobalRecord(
  AUse64Bit: Boolean);
// Pins the $27 / $20 module-global VA decoding for both forms.
// Ground truth is the .map file:
//   Win32: image base $00400000, .data $004E2000, .bss $004E7000.
//   Win64: image base $140000000, .data $140153000, .bss $140176000.
// The scanner exposes the decoded value as the absolute VA on
// Win32 (image base already included) and the RVA-from-image-base
// on Win64; both extracted as (DWORD shr 4) from the 4-byte slot.
var
  S            : TRsmScanner;
  ExpectedInt  : UInt32;  // GGlobalInt  ($27, Integer primitive)
  ExpectedLight: UInt32;  // GGlobalLight ($27, TLightStatus enum, 2-byte id)
  ExpectedField: UInt32;  // GFieldHost  ($20, TFieldStatusHost record)
  Va           : UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(AUse64Bit));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty');

    if AUse64Bit then
    begin
      // .map entries (segment:offset) + segment start (RVA):
      //   GGlobalInt   0002:00021EC4 + 0002 starts at RVA $154000
      //   GGlobalLight 0002:00021FCC
      //   GFieldHost   0003:0000C710 + 0003 starts at RVA $177000
      // Note: the segment-base RVAs drift by $1000 (one page) every
      // time DebugTarget.dpr grows past a page boundary in .data or
      // .bss. Cross-check Win64/DebugTarget.map after any fixture
      // additions; the offsets within each segment stay stable.
      ExpectedInt   := $00154000 + $00021EC4;  // $175EC4
      ExpectedLight := $00154000 + $00021FCC;  // $175FCC
      ExpectedField := $00177000 + $0000C710;  // $183710
    end
    else
    begin
      //   GGlobalInt   0003:00003BD4 + 0003 starts at VA $004E3000
      //   GGlobalLight 0003:00003BDC
      //   GFieldHost   0004:000068D0 + 0004 starts at VA $004E8000
      // Like the Win64 branch above, the segment-base VAs drift
      // by $1000 every time DebugTarget.dpr grows past a page
      // boundary -- cross-check Win32/DebugTarget.map after any
      // fixture addition.
      ExpectedInt   := $004E3000 + $00003BD4;  // $004E6BD4
      ExpectedLight := $004E3000 + $00003BDC;  // $004E6BDC
      ExpectedField := $004E8000 + $000068D0;  // $004EE8D0
    end;

    Assert.IsTrue(S.GlobalVa.TryGetValue('gglobalint', Va),
      'GGlobalInt VA not decoded');
    Assert.AreEqual<UInt32>(ExpectedInt, Va,
      'GGlobalInt VA mismatch vs .map');

    Assert.IsTrue(S.GlobalVa.TryGetValue('ggloballight', Va),
      'GGlobalLight VA not decoded');
    Assert.AreEqual<UInt32>(ExpectedLight, Va,
      'GGlobalLight VA mismatch vs .map');

    Assert.IsTrue(S.GlobalByName.ContainsKey('gfieldhost'),
      'GFieldHost not even registered by name -- $20 module-global ' +
      'handler did not run, likely due to FScanInProc still being True');
    Assert.IsTrue(S.GlobalVa.TryGetValue('gfieldhost', Va),
      'GFieldHost VA not decoded ($20 module-global form)');
    Assert.AreEqual<UInt32>(ExpectedField, Va,
      'GFieldHost VA mismatch vs .map');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestGlobalVADecodedFromGlobalRecord32;
begin
  DoTestGlobalVADecodedFromGlobalRecord(False);
end;

procedure TRsmScannerTests.TestGlobalVADecodedFromGlobalRecord64;
begin
  DoTestGlobalVADecodedFromGlobalRecord(True);
end;

procedure TRsmScannerTests.TestNonFPrefixClassFieldsDiscovered32;
begin
  DoTestNonFPrefixClassFieldsDiscovered(False);
end;

procedure TRsmScannerTests.TestNonFPrefixClassFieldsDiscovered64;
begin
  DoTestNonFPrefixClassFieldsDiscovered(True);
end;

procedure TRsmScannerTests.DoTestNonFPrefixClassFieldsDiscovered(
  AUse64Bit: Boolean);
// Pins the non-F-prefixed class-field decode for both platforms.
// After the structural-anchor swap (DPT.Rsm.StructDiscoverer:
// 4-byte typeinfo prefix + Off > 0 floor), DebugTarget's
// TNoFPrefixHost class must surface both its non-F-prefixed
// fields. The previous F-prefix heuristic dropped them silently;
// the regression guard for the broader change is also exercised
// here by asserting TDerived / TClassFieldHost keep their
// terminal fields (FDerivedLabel, FHostRtlList) which broke
// during the investigation when the anchor was too strict.
var
  S      : TRsmScanner;
  HostIdx: Integer;
  DerIdx : Integer;
  CFHIdx : Integer;

  function HasMember(AInfoIdx: Integer; const AName: String): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if AInfoIdx < 0 then Exit;
    for I := 0 to S.Classes[AInfoIdx].Members.Count - 1 do
      if SameText(S.Classes[AInfoIdx].Members[I].Name, AName) then
        Exit(True);
  end;

begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(AUse64Bit));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty');

    if not S.ClassByName.TryGetValue('tnofprefixhost', HostIdx) then
      HostIdx := -1;
    if not S.ClassByName.TryGetValue('tderived', DerIdx) then
      DerIdx := -1;
    if not S.ClassByName.TryGetValue('tclassfieldhost', CFHIdx) then
      CFHIdx := -1;

    Assert.IsTrue(HostIdx >= 0,
      'TNoFPrefixHost not discovered -- StructDiscoverer.Run missed ' +
      'the class anchor entirely');

    // Non-F fields the new structural anchor brings in:
    Assert.IsTrue(HasMember(HostIdx, 'PlainInt'),
      'TNoFPrefixHost.PlainInt not discovered -- 4-byte anchor ' +
      'check failed for the first non-F class field');
    Assert.IsTrue(HasMember(HostIdx, 'PlainLabel'),
      'TNoFPrefixHost.PlainLabel not discovered -- terminal non-F ' +
      'field lost despite the anchor accepting the broader pattern');

    // Phantom-Self regression guard: the previous F-prefix check
    // implicitly rejected "Self" because of the leading 'F'. The new
    // 4-byte anchor accepts arbitrary names, so the explicit Off>0
    // floor is what now keeps phantom <DWORD-off=0> 04 Self ... bytes
    // (emitted by every method's $21 REGVAR record) from leaking in.
    Assert.IsFalse(HasMember(HostIdx, 'Self'),
      'TNoFPrefixHost has a phantom "Self" member -- Off>0 floor ' +
      'regressed');
    if DerIdx >= 0 then
      Assert.IsFalse(HasMember(DerIdx, 'Self'),
        'TDerived has a phantom "Self" member');

    // Terminal-field-in-class-with-methods regression guard. These
    // failed under the strict 6-byte anchor (byte +4 carries
    // method-related data, not $00); the looser 4-byte anchor fixes
    // them while still rejecting random matches.
    if DerIdx >= 0 then
      Assert.IsTrue(HasMember(DerIdx, 'FDerivedLabel'),
        'TDerived.FDerivedLabel regressed -- terminal field of ' +
        'method-bearing class must still surface');
    if CFHIdx >= 0 then
      Assert.IsTrue(HasMember(CFHIdx, 'FHostRtlList'),
        'TClassFieldHost.FHostRtlList regressed -- terminal RTL-' +
        'typed field of method-bearing class must still surface');
  finally
    S.Free;
  end;
end;


procedure TRsmScannerTests.TestSimpleRecordHeaderFieldCount32;
begin
  DoTestSimpleRecordHeaderFieldCount(False);
end;

procedure TRsmScannerTests.TestSimpleRecordHeaderFieldCount64;
begin
  DoTestSimpleRecordHeaderFieldCount(True);
end;

procedure TRsmScannerTests.DoTestSimpleRecordHeaderFieldCount(
  AUse64Bit: Boolean);
// Locks in the simple-shape record header decoding: byte 0 = managed
// count (N), byte (5 + N * K) = declared field count (K=8/16 per
// platform). For each fixture record we know N and FC from the source
// declaration; the test reads the bytes directly from the .rsm and
// asserts both quantities. This is a doc-only closure -- the walker
// keeps its 4 KB scan as a safety net for the elaborate TAppCaps-style
// shape (§6.4) -- so the test reaches into S.ByteAt to verify the
// header bytes without depending on any walker behaviour.
type
  TExpectation = record
    Name        : String;
    ManagedCount: Byte;
    FieldCount  : Byte;
  end;
const
  Expectations: array[0..13] of TExpectation = (
    (Name: 'TPoint2D';         ManagedCount: 0; FieldCount: 2),
    (Name: 'TRect2D';          ManagedCount: 0; FieldCount: 2),
    (Name: 'TPoint3D';         ManagedCount: 0; FieldCount: 3),
    (Name: 'TWhdrHeader';      ManagedCount: 0; FieldCount: 2),
    (Name: 'TNarrowInts';      ManagedCount: 0; FieldCount: 5),
    (Name: 'TFloats';          ManagedCount: 0; FieldCount: 3),
    (Name: 'TEnumHostRec';     ManagedCount: 0; FieldCount: 2),
    (Name: 'TVariantSlot';     ManagedCount: 0; FieldCount: 6),
    (Name: 'TFieldStatusHost'; ManagedCount: 0; FieldCount: 8),
    (Name: 'TEnumVariantHost'; ManagedCount: 0; FieldCount: 3),
    (Name: 'TPair';            ManagedCount: 1; FieldCount: 2),
    (Name: 'TMixedRec';        ManagedCount: 1; FieldCount: 3),
    (Name: 'TWithHeader';      ManagedCount: 1; FieldCount: 4),
    (Name: 'TPrimitives';      ManagedCount: 2; FieldCount: 5)
  );
var
  S          : TRsmScanner;
  K          : Integer;
  I, J, P    : NativeInt;
  NL         : Byte;
  Name       : String;
  PStart     : NativeInt;
  ManagedByte: Byte;
  FCByte     : Byte;
  Exp        : TExpectation;
  Found      : Boolean;
begin
  if AUse64Bit then
    K := 16
  else
    K := 8;
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(AUse64Bit));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty -- need DebugTarget.rsm');

    for I := 0 to High(Expectations) do
    begin
      Exp := Expectations[I];
      Found := False;
      // Walk the byte stream for the $0E record sentinel followed by
      // a length-prefixed name that matches Exp.Name. Take the first
      // hit -- the linker emits the canonical record-name record only
      // once.
      P := 1;
      while P < S.Sz - 8 do
      begin
        if (S.ByteAt(P - 1) = $0E) then
        begin
          NL := S.ByteAt(P);
          if (NL = Length(Exp.Name)) and
             (P + 1 + NL + 4 < S.Sz) and
             S.ReadIdentifier(P, Name) and
             SameText(Name, Exp.Name) then
          begin
            PStart := P + 1 + NL + 4;
            ManagedByte := S.ByteAt(PStart);
            J := PStart + 5 + NativeInt(ManagedByte) * K;
            if J >= S.Sz then Break;
            FCByte := S.ByteAt(J);
            Assert.AreEqual<Byte>(Exp.ManagedCount, ManagedByte,
              Exp.Name + ': header byte 0 (managed count) mismatch');
            Assert.AreEqual<Byte>(Exp.FieldCount, FCByte,
              Exp.Name + ': header byte (5 + N*K) (field count) mismatch');
            Found := True;
            Break;
          end;
        end;
        Inc(P);
      end;
      Assert.IsTrue(Found,
        Exp.Name + ': no $0E-anchored record-name record found in RSM');
    end;
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestVisibilityMarkerTaxonomy32;
// §6.14 PIN. TNoFPrefixHost in DebugTarget declares one field per
// visibility section (private, strict private, protected×2, strict
// protected, public, published). All seven fields must appear in
// the discovered members list after the walker predicate widened
// from [$00..$02] to [$00..$0F]. The marker byte +2 of each field's
// anchor is also pinned against the observed taxonomy so a future
// Delphi linker change that re-numbers visibilities surfaces here
// as a regression.
type
  TProbe = record
    Name           : String;
    ExpectedMarker : Byte;  // anchor byte +2
    SectionKind    : String; // documentation only
  end;
const
  Probes: array[0..6] of TProbe = (
    (Name: 'PrivateInt';         ExpectedMarker: $00; SectionKind: 'private'),
    (Name: 'StrictPrivateInt';   ExpectedMarker: $01; SectionKind: 'strict private'),
    (Name: 'PlainInt';           ExpectedMarker: $01; SectionKind: 'protected'),
    (Name: 'PlainLabel';         ExpectedMarker: $01; SectionKind: 'protected (last in section)'),
    (Name: 'StrictProtectedInt'; ExpectedMarker: $02; SectionKind: 'strict protected'),
    (Name: 'PublicInt';          ExpectedMarker: $03; SectionKind: 'public ($M+ class)'),
    (Name: 'PublishedObj';       ExpectedMarker: $00; SectionKind: 'published (terminal)'));
var
  S       : TRsmScanner;
  ClassIdx: Integer;
  Members : IList<TRsmClassMember>;
  I, M    : Integer;
  Has     : Boolean;

  function ProbeMarkerInByteStream(const AName: String;
    var AMarker: Byte): Boolean;
  // Walk the byte stream for the structural pattern
  //   <DWORD-off> <NL> <name> $02 $00 <marker> $00
  // and return the marker byte. Used to assert the on-wire byte +2
  // matches the discovered member's section, independently of the
  // walker's internal logic.
  var
    P, EndP, NL: NativeInt;
    FoundName  : String;
    Off        : UInt32;
  begin
    Result := False;
    NL := Length(AName);
    EndP := S.Sz - NL - 8;
    P := 5;
    while P < EndP do
    begin
      if (S.ByteAt(P) = NL) and
         (P + 1 + NL + 3 < S.Sz) and
         (S.ByteAt(P + 1 + NL)     = $02) and
         (S.ByteAt(P + 1 + NL + 1) = $00) and
         (S.ByteAt(P + 1 + NL + 3) = $00) then
      begin
        Off := UInt32(S.ByteAt(P - 4)) or
               (UInt32(S.ByteAt(P - 3)) shl 8) or
               (UInt32(S.ByteAt(P - 2)) shl 16) or
               (UInt32(S.ByteAt(P - 1)) shl 24);
        if (Off > 0) and (Off <= $FFFF) and
           S.ReadIdentifier(P, FoundName) and
           SameText(FoundName, AName) then
        begin
          AMarker := S.ByteAt(P + 1 + NL + 2);
          Exit(True);
        end;
      end;
      Inc(P);
    end;
  end;

var
  ActualMarker: Byte;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty');

    Assert.IsTrue(S.ClassByName.TryGetValue('tnofprefixhost', ClassIdx),
      'TNoFPrefixHost not discovered -- general structural-anchor ' +
      'discovery regressed');
    Members := S.Classes[ClassIdx].Members;

    for I := Low(Probes) to High(Probes) do
    begin
      Has := False;
      for M := 0 to Members.Count - 1 do
        if SameText(Members[M].Name, Probes[I].Name) then
        begin
          Has := True;
          Break;
        end;
      Assert.IsTrue(Has,
        Format('Probe %s (%s) not in TNoFPrefixHost.Members -- the ' +
               'walker predicate likely rejected a marker the §6.14 ' +
               'closure widened in.',
          [Probes[I].Name, Probes[I].SectionKind]));

      Assert.IsTrue(ProbeMarkerInByteStream(Probes[I].Name, ActualMarker),
        Format('Probe %s field-record not found in byte stream',
          [Probes[I].Name]));
      Assert.AreEqual<Byte>(Probes[I].ExpectedMarker, ActualMarker,
        Format('Probe %s (%s) marker mismatch: expected $%.2X, got $%.2X. ' +
               'A change here means the Delphi linker re-numbered ' +
               'visibility encodings; update the §6.14 taxonomy.',
          [Probes[I].Name, Probes[I].SectionKind,
           Probes[I].ExpectedMarker, ActualMarker]));
    end;
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestRecordRttiFlagDiscriminator32;
// §6.6.2 PIN. The $2A body-flag for RECORDS in DebugTarget follows
// the rule:
//   $20  iff  (managed-field count > 0) OR (record appears as a
//             non-variant field type of another record/class)
//   $00  otherwise
// Variant-case references do NOT promote the flag (TEnumHostRec is
// the canonical witness: referenced only via TEnumVariantHost's
// `5: (FInner: TEnumHostRec)` branch, and stays $00).
type
  TProbe = record
    Name        : String;
    ExpectedFlag: Byte;
    Why         : String;
  end;
const
  Probes: array[0..13] of TProbe = (
    (Name: 'TPoint2D';         ExpectedFlag: $20; Why: 'referenced as field in TRect2D, TWithRec'),
    (Name: 'TRect2D';          ExpectedFlag: $20; Why: 'referenced as field in TWithRec'),
    (Name: 'TPair';            ExpectedFlag: $20; Why: 'managed FLabel:string'),
    (Name: 'TMixedRec';        ExpectedFlag: $20; Why: 'managed FMixedStr:string'),
    (Name: 'TPoint3D';         ExpectedFlag: $00; Why: 'no managed, no non-variant field-ref'),
    (Name: 'TWhdrHeader';      ExpectedFlag: $20; Why: 'referenced as field in TWithHeader'),
    (Name: 'TWithHeader';      ExpectedFlag: $20; Why: 'managed WhdrLongStr:string'),
    (Name: 'TVariantSlot';     ExpectedFlag: $00; Why: 'no managed, no non-variant field-ref'),
    (Name: 'TNarrowInts';      ExpectedFlag: $00; Why: 'packed, no managed, no non-variant field-ref'),
    (Name: 'TFloats';          ExpectedFlag: $00; Why: 'no managed, no non-variant field-ref'),
    (Name: 'TPrimitives';      ExpectedFlag: $20; Why: 'managed FAnsi:AnsiString, FWide:WideString'),
    (Name: 'TEnumHostRec';     ExpectedFlag: $00; Why: 'variant-case-only reference (TEnumVariantHost.FInner) -- does NOT promote'),
    (Name: 'TFieldStatusHost'; ExpectedFlag: $00; Why: 'packed, no managed, no non-variant field-ref'),
    (Name: 'TEnumVariantHost'; ExpectedFlag: $00; Why: 'packed variant, no managed, no non-variant field-ref'));
var
  S        : TRsmScanner;
  I        : Integer;
  P, EndPos: NativeInt;
  NL       : Byte;
  Name     : String;
  PriHi    : Byte;
  Flag     : Byte;
  Found    : Boolean;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty');

    for I := Low(Probes) to High(Probes) do
    begin
      Found := False;
      EndPos := S.Sz - 12;
      P := 4;
      while P < EndPos do
      begin
        if S.ByteAt(P) = TRsmTag.TYPE_REGISTRY_TAG then
        begin
          NL := S.ByteAt(P + 1);
          if (NL = Length(Probes[I].Name)) and
             (P + 2 + NL + 10 < S.Sz) and
             (S.ByteAt(P + 2 + NL + 1) = $00) and
             (S.ByteAt(P + 2 + NL + 2) = $00) and
             S.ReadIdentifier(P + 1, Name) and
             SameText(Name, Probes[I].Name) then
          begin
            PriHi := S.ByteAt(P + 2 + NL + 4);
            // Restrict to the program-local cluster (hi byte $2E)
            // so the same name in a cross-unit cluster doesn't
            // false-positive.
            if PriHi = $2E then
            begin
              Flag := S.ByteAt(P + 2 + NL + 0);
              Found := True;
              Break;
            end;
          end;
        end;
        Inc(P);
      end;
      Assert.IsTrue(Found,
        Format('Probe %s: no program-local $2A registry entry found',
          [Probes[I].Name]));
      Assert.AreEqual<Byte>(Probes[I].ExpectedFlag, Flag,
        Format('Probe %s: $2A body flag mismatch (%s). Expected $%.2X, ' +
               'got $%.2X. A change here means either the source ' +
               'fixture introduced a new field-reference / managed ' +
               'field, OR the Delphi compiler changed its record-' +
               'RTTI-emission rule -- re-derive the §6.6.2 ' +
               'discriminator from the new fixture and update §4.8.',
          [Probes[I].Name, Probes[I].Why,
           Probes[I].ExpectedFlag, Flag]));
    end;
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestTerminalRecordFieldSizeRecovered32;
// §6.13 PIN: terminal field of a record now reports the correct
// byte width. For each probe record we know the source-declared
// field set; assert the LAST member's Size matches the type's
// declared width.
type
  TProbe = record
    Recname     : String;
    LastName    : String;
    LastSize    : UInt32;
  end;
const
  // Probe set: simple integer records whose terminal field is
  // unambiguous on Win32. TPoint2D/TPoint3D Integer = 4 bytes,
  // TPair.FLabel = string pointer = 4 bytes on Win32.
  Probes: array[0..3] of TProbe = (
    (Recname: 'TPoint2D';    LastName: 'FY';      LastSize: 4),
    (Recname: 'TPoint3D';    LastName: 'FZ';      LastSize: 4),
    (Recname: 'TPair';       LastName: 'FLabel';  LastSize: 4),
    (Recname: 'TWhdrHeader'; LastName: 'WhdrVer'; LastSize: 4));
var
  S       : TRsmScanner;
  I       : Integer;
  ClassIdx: Integer;
  Members : IList<TRsmClassMember>;
  Last    : TRsmClassMember;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty');

    for I := Low(Probes) to High(Probes) do
    begin
      Assert.IsTrue(S.ClassByName.TryGetValue(
        LowerCase(Probes[I].Recname), ClassIdx),
        Format('Probe %s not discovered', [Probes[I].Recname]));
      Members := S.Classes[ClassIdx].Members;
      Assert.IsTrue(Members.Count >= 1,
        Format('Probe %s has no members', [Probes[I].Recname]));
      Last := Members[Members.Count - 1];
      Assert.AreEqual(Probes[I].LastName, Last.Name,
        Format('Probe %s last-member name mismatch', [Probes[I].Recname]));
      Assert.AreEqual<UInt32>(Probes[I].LastSize, UInt32(Last.Size),
        Format('Probe %s last-member %s Size: expected %d, got %d. ' +
               'A regression to Size=0 means the §6.13 terminal-' +
               'field recovery via record-size DWORD broke.',
          [Probes[I].Recname, Probes[I].LastName, Probes[I].LastSize, Last.Size]));
    end;
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestTypeRegistryWideBodyHoldsRttiVA32;
// §6.6.1 PIN. For each $20-flagged $2A entry (program-local class /
// record with RTTI emitted), decodes body bytes +5..+8 as Win32
// (VA shl 4) | $07 and asserts the recovered VA is in the .text
// window. Then asserts the VAs are monotonically increasing across
// the probe list (the linker emits RTTI in declaration order, so
// later-declared types land at higher VAs).
const
  // Probes ordered by declaration site in DebugTarget.dpr so we can
  // assert monotonic VA increase (the linker emits per-type RTTI in
  // declaration order). TPair / TWithRec / TMixedRec specifically:
  // lines 88 / 89 / 96 in DebugTarget.dpr -- TWithRec sits BETWEEN
  // TPair and TMixedRec, which the order below preserves.
  Probes: array[0..6] of String = (
    'TInner', 'TDerived', 'TPoint2D', 'TRect2D', 'TPair', 'TWithRec',
    'TMixedRec');
var
  S       : TRsmScanner;
  I, J    : Integer;
  P, EndP : NativeInt;
  NL      : Byte;
  Name    : String;
  Body    : NativeInt;
  Flag    : Byte;
  PriHi   : Byte;
  DW      : UInt32;
  VA      : UInt32;
  Found   : array of Boolean;
  VAs     : array of UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty');

    SetLength(Found, Length(Probes));
    SetLength(VAs,   Length(Probes));

    EndP := S.Sz - 16;
    P := 4;
    while P < EndP do
    begin
      if S.ByteAt(P) = TRsmTag.TYPE_REGISTRY_TAG then
      begin
        NL := S.ByteAt(P + 1);
        if (NL >= 2) and (NL <= 40) and
           (P + 2 + NL + 12 < S.Sz) and
           (S.ByteAt(P + 2) = Byte(Ord('T'))) and
           (S.ByteAt(P + 2 + NL + 1) = $00) and
           (S.ByteAt(P + 2 + NL + 2) = $00) and
           S.ReadIdentifier(P + 1, Name) then
        begin
          Body := P + 2 + NL;
          Flag := S.ByteAt(Body);
          PriHi := S.ByteAt(Body + 4);
          if (Flag = $20) and (PriHi = $2E) then
          begin
            for J := Low(Probes) to High(Probes) do
              if SameText(Probes[J], Name) and not Found[J] then
              begin
                DW := UInt32(S.ByteAt(Body + 5)) or
                      (UInt32(S.ByteAt(Body + 6)) shl 8) or
                      (UInt32(S.ByteAt(Body + 7)) shl 16) or
                      (UInt32(S.ByteAt(Body + 8)) shl 24);
                // Win32 wire format: low nibble must be $07.
                Assert.AreEqual<UInt32>(UInt32($07), DW and UInt32($0F),
                  Format('Probe %s: $20 wide-body slot byte +5 low nibble ' +
                         'must be $07 (Win32 (VA shl 4) | $07 format); ' +
                         'got DW=$%x', [Probes[J], DW]));
                VA := DW shr 4;
                Assert.IsTrue((VA >= $401000) and (VA < $00800000),
                  Format('Probe %s: decoded RTTI-VA $%x is outside the ' +
                         'plausible DebugTarget .text window ' +
                         '[$401000, $800000)', [Probes[J], VA]));
                Found[J] := True;
                VAs[J]   := VA;
                Break;
              end;
          end;
        end;
      end;
      Inc(P);
    end;

    for I := Low(Probes) to High(Probes) do
      Assert.IsTrue(Found[I],
        Format('Probe %s: no $20-flagged $2A entry with PriHi=$2E ' +
               'found in .rsm', [Probes[I]]));

    // Monotonic RTTI-VA increase across declaration-ordered probes.
    for I := Low(Probes) + 1 to High(Probes) do
      Assert.IsTrue(VAs[I] > VAs[I - 1],
        Format('Probe %s: RTTI-VA $%x is not greater than predecessor ' +
               '%s VA $%x. The linker emits per-type RTTI in declaration ' +
               'order, so a regression here means the slot is no longer ' +
               'the RTTI-pointer documented in §4.8 / §6.6.1.',
          [Probes[I], VAs[I], Probes[I - 1], VAs[I - 1]]));
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestCrossUnitRtlLinkerTokenIsLinearStride32;
// §6.5 PIN. For each TThreadPriority element (System.Classes
// cross-unit enum), finds the $25 cross-unit RTL record in the .rsm
// byte stream and reads the 4-byte LE DWORD at body offset +3..+6.
// Asserts the recovered values follow `base + ord * 3`: a linear
// stride that proves the slot is NOT a Win32/Win64 VA/RVA (those
// would diverge across platforms; the linker token does not) and
// matches no plausible in-image offset.
type
  TProbe = record
    Name   : String;
    Ordinal: Integer;
  end;
const
  // tpIdle=0, tpLowest=1, tpLower=2, tpNormal=3, tpHigher=4,
  // tpHighest=5, tpTimeCritical=6 (System.Classes.TThreadPriority).
  Probes: array[0..6] of TProbe = (
    (Name: 'tpIdle';         Ordinal: 0),
    (Name: 'tpLowest';       Ordinal: 1),
    (Name: 'tpLower';        Ordinal: 2),
    (Name: 'tpNormal';       Ordinal: 3),
    (Name: 'tpHigher';       Ordinal: 4),
    (Name: 'tpHighest';      Ordinal: 5),
    (Name: 'tpTimeCritical'; Ordinal: 6));
var
  S        : TRsmScanner;
  Probe    : TProbe;
  I        : Integer;
  P        : NativeInt;
  NL       : Byte;
  Name     : String;
  Body     : NativeInt;
  Token    : UInt32;
  Base     : UInt32;
  ProbeIdx : Integer;
  Found    : array of Boolean;
  Tokens   : array of UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty');

    SetLength(Found,  Length(Probes));
    SetLength(Tokens, Length(Probes));

    // Walk the byte stream for $25 records with the cross-unit RTL
    // anchor `$8A $00 $00` after the name.
    P := 4;
    while P < S.Sz - 16 do
    begin
      if (S.ByteAt(P) = $25) then
      begin
        NL := S.ByteAt(P + 1);
        if (NL >= 1) and (NL <= 64) and
           (P + 2 + NL + 11 < S.Sz) and
           (S.ByteAt(P + 2 + NL)     = $8A) and
           (S.ByteAt(P + 2 + NL + 1) = $00) and
           (S.ByteAt(P + 2 + NL + 2) = $00) and
           S.ReadIdentifier(P + 1, Name) then
        begin
          ProbeIdx := -1;
          for I := Low(Probes) to High(Probes) do
            if SameText(Probes[I].Name, Name) then
            begin
              ProbeIdx := I;
              Break;
            end;
          if ProbeIdx >= 0 then
          begin
            Body := P + 2 + NL;
            Token := UInt32(S.ByteAt(Body + 3)) or
                     (UInt32(S.ByteAt(Body + 4)) shl 8) or
                     (UInt32(S.ByteAt(Body + 5)) shl 16) or
                     (UInt32(S.ByteAt(Body + 6)) shl 24);
            Found[ProbeIdx]  := True;
            Tokens[ProbeIdx] := Token;
          end;
        end;
      end;
      Inc(P);
    end;

    // Require every probe to have appeared.
    for I := Low(Probes) to High(Probes) do
      Assert.IsTrue(Found[I],
        Format('Probe %s not found in .rsm; the test fixture must keep ' +
               'a reference to System.Classes.TThreadPriority alive ' +
               '(see DebugTarget.dpr GGlobalThPriHost).', [Probes[I].Name]));

    // Linear-stride invariant: Tokens[I] - Base == Ordinal * 3 for
    // some constant Base (the enum's first-element token). Anchor
    // Base = Tokens[0] (= tpIdle = ord 0).
    Base := Tokens[0];
    for I := Low(Probes) to High(Probes) do
    begin
      Probe := Probes[I];
      var Expected: UInt32 := Base + UInt32(Probe.Ordinal) * 3;
      Assert.AreEqual<UInt32>(Expected, Tokens[I],
        Format('Probe %s (ord=%d): linker-token $%x does not match the ' +
               'predicted linear-stride value $%x (= base $%x + ord * 3). ' +
               'A change in stride means the slot is no longer the opaque ' +
               '`base + ord * 3` token §6.5 documented -- re-investigate.',
          [Probe.Name, Probe.Ordinal, Tokens[I], Expected, Base]));
    end;
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestSparseEnumResolvesViaEnumConstNames32;
// Pins the sparse-ordinal enum decoding. DebugTarget declares
// TSparseEnum = (seAlpha = 1, seBeta = 5, seGamma = 11). The linker
// does NOT emit a $03 ENUM_DEF record for this enum (verified by
// walking the byte stream below). The $25 program-local channel
// however emits each constant with its explicit ordinal, so
// EnumConstNames carries seAlpha=1, seBeta=5, seGamma=11 -- the
// resolver layer that consumes typeId:ord lookups already works.
var
  S       : TRsmScanner;
  HasDef  : Boolean;
  HitVals : array[1..11] of String;

  function FindEnumDefByName(const AName: String): Boolean;
  var
    P, EndPos: NativeInt;
    NL       : Byte;
    Name     : String;
  begin
    Result := False;
    EndPos := S.Sz - 16;
    P := 4;
    while P < EndPos do
    begin
      if (S.ByteAt(P) = TRsmTag.ENUM_DEF_TAG) and
         (S.ByteAt(P + 1) = Length(AName)) then
      begin
        NL := S.ByteAt(P + 1);
        if (NL >= 2) and (NL <= 40) and (P + 2 + NL < S.Sz) and
           S.ReadIdentifier(P + 1, Name) and
           SameText(Name, AName) then
          Exit(True);
      end;
      Inc(P);
    end;
  end;

begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty -- need DebugTarget.rsm');

    // (a) Sparse enums get NO $03 ENUM_DEF record.
    HasDef := FindEnumDefByName('TSparseEnum');
    Assert.IsFalse(HasDef,
      'Linker emitted a $03 ENUM_DEF for TSparseEnum -- §6.1 ' +
      'finding needs revisiting; sparse enums were previously ' +
      'observed to be absent from the $03 channel');

    // (b) But the $25 channel DID emit one constant per element
    // with the explicit ordinal as the value. We can't predict
    // TSparseEnum's typeId statically (linker-assigned), so iterate
    // EnumConstNames and pick out the ordinals by the constant name.
    for var I := 1 to 11 do HitVals[I] := '';
    for var Pair in S.EnumConstNames do
    begin
      var ColonPos := Pos(':', Pair.Key);
      if ColonPos = 0 then Continue;
      var OrdinalStr := Copy(Pair.Key, ColonPos + 1, MaxInt);
      var Ord: Integer;
      if not TryStrToInt(OrdinalStr, Ord) then Continue;
      if (Ord < 1) or (Ord > 11) then Continue;
      if SameText(Pair.Value, 'seAlpha') or
         SameText(Pair.Value, 'seBeta')  or
         SameText(Pair.Value, 'seGamma') then
        HitVals[Ord] := Pair.Value;
    end;

    Assert.AreEqual('seAlpha', HitVals[1],
      'seAlpha did not surface at ordinal 1');
    Assert.AreEqual('seBeta', HitVals[5],
      'seBeta did not surface at ordinal 5');
    Assert.AreEqual('seGamma', HitVals[11],
      'seGamma did not surface at ordinal 11');
  finally
    S.Free;
  end;
end;

{$IFDEF CPUX64}
procedure TRsmScannerTests.TestProcsCollected64;
var
  S: TRsmScanner;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(True));
    Assert.IsTrue(S.Procs.Count > 0, 'expected procs from Win64 DebugTarget');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestStructsCollected64;
var
  S: TRsmScanner;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(True));
    Assert.IsTrue(S.Classes.Count > 0, 'expected classes from Win64');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestGlobalsRegistered64;
var
  S     : TRsmScanner;
  TypeId: UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(True));
    Assert.IsTrue(S.GlobalByName.TryGetValue('ggloballight', TypeId),
      'GGlobalLight missing in Win64 fixture');
  finally
    S.Free;
  end;
end;
{$ENDIF}

procedure TRsmScannerTests.TestRegisterParamEnumTypeIdNotTruncated32;
const
  TLightStatusPrimary : UInt32 = $2E81; // program-local enum
  // Per-binary alias the linker mints for the cross-unit RTL enum
  // TThreadPriority's $21 REGVAR record. NOT the $2A registry primary
  // ($3370) — the alias→primary bridge is the residual §6.15 work.
  TThreadPriorityAlias: UInt32 = $0671;
var
  S          : TRsmScanner;
  ProcIdx    : Integer;
  I          : Integer;
  Proc       : TRsmProc;
  LightFound : Boolean;
  PriFound   : Boolean;
  LightId    : UInt32;
  PriId      : UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.ProcByName.TryGetValue('touchregenumparam', ProcIdx),
      'TouchRegEnumParam must be in ProcByName -- DebugTarget.dpr ' +
      '§6.15 fixture missing? Rebuild DebugTarget.exe.');
    Proc := S.Procs[ProcIdx];
    LightFound := False; PriFound := False;
    LightId := 0; PriId := 0;
    for I := 0 to Proc.Locals.Count - 1 do
    begin
      if SameText(Proc.Locals[I].Name, 'AStatusLight') then
      begin
        LightFound := True;
        LightId := Proc.Locals[I].TypeIdx;
      end
      else if SameText(Proc.Locals[I].Name, 'AStatusPriority') then
      begin
        PriFound := True;
        PriId := Proc.Locals[I].TypeIdx;
      end;
    end;
    Assert.IsTrue(LightFound,
      'AStatusLight must be present as a Local on TouchRegEnumParam');
    Assert.IsTrue(PriFound,
      'AStatusPriority must be present as a Local on TouchRegEnumParam');
    // Fast path: $2E/$2F gate kept program-local enum unchanged.
    Assert.AreEqual<UInt32>(TLightStatusPrimary, LightId,
      Format('AStatusLight (TLightStatus) TypeIdx must be $%x (the ' +
             'program-local enum primary). Got $%x -- did the $2E/$2F ' +
             'fast path regress?', [TLightStatusPrimary, LightId]));
    // The bug fix: cross-unit RTL alias now reads as a full 2-byte
    // value instead of the 1-byte truncation $71 that previously
    // collided with random foreign types.
    Assert.AreNotEqual<UInt32>(UInt32($71), PriId,
      'AStatusPriority TypeIdx must NOT equal $71 (the pre-§6.15 ' +
      '1-byte truncation of the cross-unit RTL alias). Hitting $71 ' +
      'means the structural-lookahead disambiguator regressed.');
    Assert.AreEqual<UInt32>(TThreadPriorityAlias, PriId,
      Format('AStatusPriority (TThreadPriority) TypeIdx must be ' +
             'the per-binary alias $%x (2-byte structural read). Got ' +
             '$%x -- either the disambiguator missed the +6=$63 ' +
             '(SCOPE_END) continuation tag, or the linker shifted the ' +
             'alias and the fixture needs re-pinning.',
             [TThreadPriorityAlias, PriId]));
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestUnitUseSegmentSystemIsDecoded;
// §6.21 closure pin. Asserts the FIRST $64 'System' segment in
// DebugTarget.rsm decodes with the expected unit name and that one
// of its $66 type-references is 'Boolean' with the canonical 4-byte
// RVA payload the linker emitted (file offset 388467 in the current
// fixture).
const
  ExpectedRva: UInt32 = $62BC8138;
var
  S       : TRsmScanner;
  I, J    : Integer;
  Seg     : TRsmUnitUseSegment;
  Ref     : TRsmUnitUseRef;
  Found   : Boolean;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.UnitUseSegments.Count > 0,
      'UnitUseSegments empty -- $64 introducer never decoded');
    Found := False;
    for I := 0 to S.UnitUseSegments.Count - 1 do
    begin
      Seg := S.UnitUseSegments[I];
      if not SameText(Seg.UnitName, 'System') then Continue;
      if Seg.Refs = nil then Continue;
      for J := 0 to Seg.Refs.Count - 1 do
      begin
        Ref := Seg.Refs[J];
        if (Ref.Kind = uukType) and SameText(Ref.Name, 'Boolean') then
        begin
          Assert.AreEqual<UInt32>(ExpectedRva, Ref.Rva,
            Format('System.Boolean $66 payload must be the canonical ' +
                   'RVA $%x; got $%x. A drift here means either the ' +
                   'linker re-shuffled the imported RTL slot, or the ' +
                   'scanner mis-aligned the 4-byte payload read.',
                   [ExpectedRva, Ref.Rva]));
          Found := True;
          Break;
        end;
      end;
      if Found then Break;
    end;
    Assert.IsTrue(Found,
      'No $64 ''System'' segment with a $66 ''Boolean'' reference -- ' +
      'either the segment anchor failed, or the entry walk bailed early.');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestUnitUseSymbolReferencePayloadHasPlusThreeStride;
// §6.21 closure pin. The $67 symbol-reference payload bytes follow
// the +3 LSB stride across siblings of a single enum -- the
// signature §6.20-Round-3 used to identify the tag family. Pin
// the System.UITypes vk* run: vkLButton, vkRButton, vkCancel are
// consecutive elements of TVirtualKeyCode with explicit
// VK_LBUTTON / VK_RBUTTON / VK_CANCEL ordinals.
const
  ExpectedVkLButton: UInt32 = $31E77DAF;
  ExpectedVkRButton: UInt32 = $31E77DB2;
  ExpectedVkCancel : UInt32 = $31E77DB5;
var
  S       : TRsmScanner;
  I, J    : Integer;
  Seg     : TRsmUnitUseSegment;
  Ref     : TRsmUnitUseRef;
  GotLButton, GotRButton, GotCancel: Boolean;
  RvaL, RvaR, RvaC: UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    GotLButton := False; GotRButton := False; GotCancel := False;
    RvaL := 0; RvaR := 0; RvaC := 0;
    for I := 0 to S.UnitUseSegments.Count - 1 do
    begin
      Seg := S.UnitUseSegments[I];
      if Seg.Refs = nil then Continue;
      for J := 0 to Seg.Refs.Count - 1 do
      begin
        Ref := Seg.Refs[J];
        if Ref.Kind <> uukSymbol then Continue;
        if SameText(Ref.Name, 'vkLButton') then begin GotLButton := True; RvaL := Ref.Rva; end
        else if SameText(Ref.Name, 'vkRButton') then begin GotRButton := True; RvaR := Ref.Rva; end
        else if SameText(Ref.Name, 'vkCancel')  then begin GotCancel  := True; RvaC := Ref.Rva; end;
      end;
    end;
    Assert.IsTrue(GotLButton and GotRButton and GotCancel,
      Format('Missing $67 entries: vkLButton=%s vkRButton=%s vkCancel=%s',
        [BoolToStr(GotLButton, True), BoolToStr(GotRButton, True),
         BoolToStr(GotCancel, True)]));
    Assert.AreEqual<UInt32>(ExpectedVkLButton, RvaL,
      Format('vkLButton RVA mismatch: expected $%x, got $%x',
        [ExpectedVkLButton, RvaL]));
    Assert.AreEqual<UInt32>(ExpectedVkRButton, RvaR,
      Format('vkRButton RVA mismatch: expected $%x, got $%x',
        [ExpectedVkRButton, RvaR]));
    Assert.AreEqual<UInt32>(ExpectedVkCancel, RvaC,
      Format('vkCancel RVA mismatch: expected $%x, got $%x',
        [ExpectedVkCancel, RvaC]));
    // The +3 stride is the structural finding the decoder is built
    // on; pin it.
    Assert.AreEqual<UInt32>(UInt32(3), RvaR - RvaL,
      'vkRButton - vkLButton must be +3 (enum-element LSB stride)');
    Assert.AreEqual<UInt32>(UInt32(3), RvaC - RvaR,
      'vkCancel - vkRButton must be +3 (enum-element LSB stride)');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestUnitUseFalsePositiveRejection;
// §6.21 leakage guard. A $64 byte that ISN'T followed by the
// `NL UnitName $00 $00 $00` anchor must NOT open a segment.
// We synthesise a CSH7-magic buffer whose entire post-header
// content is a $64 byte followed by content that violates the
// anchor (no trailing zero triple). The scanner must leave
// UnitUseSegments empty -- otherwise an incidental $64 byte in a
// proc-address payload (the scenario the §6.20 Round-3 entry
// describes) would spuriously open garbage segments.
var
  S     : TRsmScanner;
  Buf   : TBytes;
begin
  SetLength(Buf, 64);
  // CSH7 magic
  Buf[0] := $43; Buf[1] := $53; Buf[2] := $48; Buf[3] := $37;
  // Pretend $64 with name length 4 -- but the bytes AFTER the
  // (would-be) name are NOT three zeros; they are random data.
  Buf[4]  := $64;
  Buf[5]  := $04;
  Buf[6]  := Ord('F'); Buf[7] := Ord('o'); Buf[8] := Ord('o'); Buf[9] := Ord('!');
  // The trailing three bytes after the name MUST be zero to anchor;
  // we make them nonzero to test rejection.
  Buf[10] := $AB; Buf[11] := $CD; Buf[12] := $EF;
  // Fill rest with arbitrary bytes that are NOT in the ref-tag set
  // so the dispatcher just walks them away.
  for var I := 13 to High(Buf) do
    Buf[I] := Byte(I);

  S := TRsmScanner.Create;
  try
    S.LoadFromBytes(Buf);
    Assert.AreEqual<Integer>(0, S.UnitUseSegments.Count,
      'Synthetic $64 without the $00 $00 $00 anchor opened a segment ' +
      '-- the structural anchor rejection regressed.');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestProcMarkerOwnerRefDecodes32;
// §6.22 closure pin (Win32). The byte immediately after the
// platform-anchor `$11 $2E` in every $28 PROC record encodes the
// owning class:
//   * plain proc            => single $00 sentinel
//   * instance method <C.M> => 2-byte LE = C's $2A raw type id
// Pins three intra-class+inter-class probes. Expected owner-id
// values come straight from §4.1's Win32 byte-evidence table;
// re-pin both together if the linker shifts class ids.
const
  Win32Anchor    : Byte   = $11;
  // Sentinel returned by the helper when the marker isn't found
  // within the search window (catches a regression that drops the
  // `$11 $2E` recognition entirely, vs. a regression that finds it
  // but reads the owner-ref wrong).
  NotFound       : UInt32 = $FFFFFFFF;
  TDerivedOwner  : UInt32 = $2E21;
  TPropHostOwner : UInt32 = $2ED5;
  TStaleSelfOwner: UInt32 = $2EE5;
var
  S: TRsmScanner;

  function FindProcStart(const AProcName: String): NativeInt;
  var
    NL: Byte; I, K: NativeInt; OK: Boolean;
  begin
    NL := Length(AProcName);
    Result := -1;
    if NL = 0 then Exit;
    for I := 0 to S.Sz - 2 - NL do
    begin
      if (S.ByteAt(I) = $28) and (S.ByteAt(I + 1) = NL) then
      begin
        OK := True;
        for K := 1 to NL do
          if S.ByteAt(I + 1 + K) <> Byte(AProcName[K]) then
          begin OK := False; Break; end;
        if OK then Exit(I);
      end;
    end;
  end;

  function ExtractOwnerRef(AProcStart: NativeInt; AAnchor: Byte): UInt32;
  // Returns $00 for plain procs (single sentinel after $2E),
  // the 2-byte LE owner-id for instance methods, or NotFound
  // when the marker isn't located within the search window.
  var
    NL: Byte; ScanFrom, ScanTo, P: NativeInt;
  begin
    Result := NotFound;
    if AProcStart < 0 then Exit;
    NL := S.ByteAt(AProcStart + 1);
    ScanFrom := AProcStart + 2 + NL;
    ScanTo := ScanFrom + 40;
    if ScanTo + 4 >= S.Sz then ScanTo := S.Sz - 4;
    for P := ScanFrom to ScanTo do
      if (S.ByteAt(P) = AAnchor) and (S.ByteAt(P + 1) = $2E) then
      begin
        if S.ByteAt(P + 2) = $00 then Exit(0);
        Result := UInt32(S.ByteAt(P + 2)) or (UInt32(S.ByteAt(P + 3)) shl 8);
        Exit;
      end;
  end;

var
  ProcOff : NativeInt;
  OwnerRef: UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));

    // 1. Plain proc: single $00 sentinel.
    ProcOff := FindProcStart('TargetProcedure');
    Assert.IsTrue(ProcOff >= 0, 'TargetProcedure $28 record not found');
    OwnerRef := ExtractOwnerRef(ProcOff, Win32Anchor);
    Assert.AreEqual<UInt32>(0, OwnerRef,
      Format('TargetProcedure (plain proc) owner-ref must be $00 ' +
             'sentinel; got $%x. NotFound=$%x means the $11 $2E ' +
             'anchor scan failed entirely.', [OwnerRef, NotFound]));

    // 2. Instance method on TDerived: owner-ref = TDerived's $2A id.
    ProcOff := FindProcStart('TDerived.TouchSelf');
    Assert.IsTrue(ProcOff >= 0, 'TDerived.TouchSelf $28 record not found');
    OwnerRef := ExtractOwnerRef(ProcOff, Win32Anchor);
    Assert.AreEqual<UInt32>(TDerivedOwner, OwnerRef,
      Format('TDerived.TouchSelf owner-ref must equal $%x ' +
             '(TDerived''s $2A raw id); got $%x. Drift means ' +
             'either the linker shifted class ids or the marker ' +
             'walk picked the wrong bytes after $11 $2E.',
             [TDerivedOwner, OwnerRef]));

    // 3. Multi-parameter constructor with the long-form middle
    // section (`04 ?? ?? 02 31 03 11 2E ??`): owner-ref must equal
    // TPropHost's $2A raw id. Note that TPropHost.GetCalcInt uses
    // the short-form marker (`04 ?? ?? 9C 02 ?? 2E` — no `$11 $2E`
    // middle anchor), which this pin intentionally doesn't cover;
    // re-pinning the short form is deferred until a §6 gap depends
    // on it.
    ProcOff := FindProcStart('TPropHost.Create');
    Assert.IsTrue(ProcOff >= 0, 'TPropHost.Create $28 record not found');
    OwnerRef := ExtractOwnerRef(ProcOff, Win32Anchor);
    Assert.AreEqual<UInt32>(TPropHostOwner, OwnerRef,
      Format('TPropHost.Create owner-ref must equal $%x ' +
             '(TPropHost''s $2A raw id); got $%x.',
             [TPropHostOwner, OwnerRef]));

    // 4. Different class, different owner-ref (leakage guard).
    ProcOff := FindProcStart('TStaleSelfHost.Probe');
    Assert.IsTrue(ProcOff >= 0, 'TStaleSelfHost.Probe $28 record not found');
    OwnerRef := ExtractOwnerRef(ProcOff, Win32Anchor);
    Assert.AreEqual<UInt32>(TStaleSelfOwner, OwnerRef,
      Format('TStaleSelfHost.Probe owner-ref must equal $%x; got $%x.',
             [TStaleSelfOwner, OwnerRef]));
    Assert.AreNotEqual<UInt32>(TDerivedOwner, OwnerRef,
      'TStaleSelfHost.Probe and TDerived.TouchSelf must NOT share an ' +
      'owner-ref (different classes).');
  finally
    S.Free;
  end;
end;

{$IFDEF CPUX64}
procedure TRsmScannerTests.TestProcMarkerOwnerRefDecodes64;
// §6.22 closure pin (Win64). Same shape as the Win32 sibling but
// the platform-anchor byte is $3D instead of $11; class ids are
// also different per-build. Round-1 observations:
//   * TargetProcedure → $00 sentinel (plain)
//   * TDerived.TouchSelf → $2E65 owner-ref
//   * TStaleSelfHost.Probe → $2FE1 owner-ref
const
  Win64Anchor    : Byte   = $3D;
  NotFound       : UInt32 = $FFFFFFFF;
  TDerivedOwner  : UInt32 = $2E65;
  TStaleSelfOwner: UInt32 = $2FE1;
var
  S: TRsmScanner;

  function FindProcStart(const AProcName: String): NativeInt;
  var
    NL: Byte; I, K: NativeInt; OK: Boolean;
  begin
    NL := Length(AProcName);
    Result := -1;
    if NL = 0 then Exit;
    for I := 0 to S.Sz - 2 - NL do
    begin
      if (S.ByteAt(I) = $28) and (S.ByteAt(I + 1) = NL) then
      begin
        OK := True;
        for K := 1 to NL do
          if S.ByteAt(I + 1 + K) <> Byte(AProcName[K]) then
          begin OK := False; Break; end;
        if OK then Exit(I);
      end;
    end;
  end;

  function ExtractOwnerRef(AProcStart: NativeInt; AAnchor: Byte): UInt32;
  var
    NL: Byte; ScanFrom, ScanTo, P: NativeInt;
  begin
    Result := NotFound;
    if AProcStart < 0 then Exit;
    NL := S.ByteAt(AProcStart + 1);
    ScanFrom := AProcStart + 2 + NL;
    ScanTo := ScanFrom + 40;
    if ScanTo + 4 >= S.Sz then ScanTo := S.Sz - 4;
    for P := ScanFrom to ScanTo do
      if (S.ByteAt(P) = AAnchor) and (S.ByteAt(P + 1) = $2E) then
      begin
        if S.ByteAt(P + 2) = $00 then Exit(0);
        Result := UInt32(S.ByteAt(P + 2)) or (UInt32(S.ByteAt(P + 3)) shl 8);
        Exit;
      end;
  end;

var
  ProcOff : NativeInt;
  OwnerRef: UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(True));

    ProcOff := FindProcStart('TargetProcedure');
    Assert.IsTrue(ProcOff >= 0, 'TargetProcedure $28 record not found (Win64)');
    OwnerRef := ExtractOwnerRef(ProcOff, Win64Anchor);
    Assert.AreEqual<UInt32>(0, OwnerRef,
      Format('TargetProcedure (plain, Win64) owner-ref must be $00; ' +
             'got $%x. NotFound=$%x means the $3D $2E anchor missed.',
             [OwnerRef, NotFound]));

    ProcOff := FindProcStart('TDerived.TouchSelf');
    Assert.IsTrue(ProcOff >= 0, 'TDerived.TouchSelf $28 record not found (Win64)');
    OwnerRef := ExtractOwnerRef(ProcOff, Win64Anchor);
    Assert.AreEqual<UInt32>(TDerivedOwner, OwnerRef,
      Format('TDerived.TouchSelf (Win64) owner-ref must equal $%x; got $%x.',
             [TDerivedOwner, OwnerRef]));

    ProcOff := FindProcStart('TStaleSelfHost.Probe');
    Assert.IsTrue(ProcOff >= 0, 'TStaleSelfHost.Probe $28 record not found (Win64)');
    OwnerRef := ExtractOwnerRef(ProcOff, Win64Anchor);
    Assert.AreEqual<UInt32>(TStaleSelfOwner, OwnerRef,
      Format('TStaleSelfHost.Probe (Win64) owner-ref must equal $%x; got $%x.',
             [TStaleSelfOwner, OwnerRef]));
    Assert.AreNotEqual<UInt32>(TDerivedOwner, OwnerRef,
      'TStaleSelfHost.Probe and TDerived.TouchSelf must NOT share an ' +
      'owner-ref on Win64 either.');
  finally
    S.Free;
  end;
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TRsmScannerTests);

end.
