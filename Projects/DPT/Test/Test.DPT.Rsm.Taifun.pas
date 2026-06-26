// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Rsm.Taifun;

// Tests that run against the developer-machine-only large binaries of
// the Taifun ecosystem: C:\MSE\TFW\TFW.exe and C:\MSE\TEST\Test.Lib.exe,
// each with its .rsm sidecar (+ .map). These .rsm files are huge
// (TFW ~800 MB, Test.Lib ~240 MB) and take many seconds to load, so the
// load happens ONCE per fixture in a shared `[SetupFixture]` and every
// test in that fixture reads back the one loaded reader / map / phase
// timings.
//
// Resource handling is unified in the base class TRsmHeavyFixtureTests:
// it owns the primary reader, the .map, the OnPhase timing capture and
// the skip machinery, and exposes `LoadPrimaryFixture(exe, label)` for
// each concrete fixture to call from its own `[SetupFixture]`. The
// concrete fixtures are:
//   * TRsmTfwTests     — TFW.exe (plus a Win64 variant for §6.2 RE)
//   * TRsmTestLibTests — Test.Lib.exe (the §6.31 C-prefix-class corpus)
//
// All tests skip silently (Assert.Pass + a WARNING line to stdout)
// when their fixture is missing, so the suite stays green on clean
// machines.

interface

uses
  System.Classes,
  System.Diagnostics,
  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,
  mormot.core.collections,

  DPT.MapFileParser,
  DPT.Rsm.Model,
  DPT.Rsm.Reader,
  DPT.Rsm.Scanner;

type

  /// Base for the heavy-fixture test classes. Owns the resources whose
  /// initialisation is expensive and so are loaded once per fixture and
  /// shared by every test in it: the primary RSM reader, the sibling
  /// .map, the OnPhase per-phase timing capture, and the skip state.
  /// Concrete fixtures call `LoadPrimaryFixture` from their own
  /// [SetupFixture] and `FreePrimaryFixture` from [TearDownFixture].
  /// NOT a [TestFixture] itself — it carries no [Test] methods.
  TRsmHeavyFixtureTests = class
  strict protected
    FReader      : TRsmReader;
    FMap         : TMapFileParser;
    FPhaseTable  : TStringList;
    FPhaseTimings: IKeyValue<String, Int64>;
    FTotalLoadMs : Int64;
    FFixtureRsm  : String;
    FFixtureMB   : Double;
    FFixtureLine : String;
    FSkipReason  : String;
    /// Loads `AExePath`'s .rsm into FReader once, capturing per-phase
    /// timings into FPhaseTimings/FPhaseTable and the total into
    /// FTotalLoadMs, then loads the sibling .map into FMap when present.
    /// Mirrors a one-line identity + per-phase summary to dpt.log and
    /// stdout. On a missing fixture it sets FSkipReason (tagged with
    /// `ALabel`), prints a WARNING, and returns False. Returns True when
    /// the reader loaded — the caller then runs any fixture-specific
    /// extras (TFW's spot-procs / Win64 reader, etc.).
    function LoadPrimaryFixture(const AExePath, ALabel: String): Boolean;
    /// Frees the primary reader / map / phase table (FreeAndNil-safe).
    procedure FreePrimaryFixture;
    /// Returns True (and `Assert.Pass(FSkipReason)`) when the fixture was
    /// missing, so a [Test] can bail before touching FReader / FMap.
    function ShouldSkip: Boolean;
  end;

  [TestFixture]
  TRsmTfwTests = class(TRsmHeavyFixtureTests)
  // Fixture location: the TFW corpus lives in the in-repo, project-
  // relative RsmTaifunData folder (resolved CWD-relative at runtime by
  // the unit-level TfwExePath / TfwMapPath / TfwWin64ExePath /
  // TfwWin64MapPath functions below, mirroring ResolveExePath's two-form
  // fallback). The .rsm sidecar + .map are derived from the exe path.
  strict private
    {$IFDEF CPUX64}
    /// Win64 reader / map. The reader exposes Procs[].SegmentOffset
    /// (decoded address) and its internal Scanner (Is64Bit + ByteAt
    /// for raw-byte diagnostics). The map provides ground-truth
    /// RVAs for the §6.2 pin and future Win64 sub-form investigations.
    FReaderWin64    : TRsmReader;
    FMapWin64       : TMapFileParser;
    FWin64SkipReason: String;
    /// Same shape as ShouldSkip but for the Win64 fixture.
    function ShouldSkipWin64: Boolean;
    {$ENDIF}
  public
    /// One-time fixture setup: loads TFW.rsm once with OnPhase timings
    /// captured into FPhaseTimings, then loads TFW.map. Subsequent
    /// [Test] methods read the shared state.
    [SetupFixture]
    procedure SetupFixture;
    /// Frees the shared reader / map / phase table.
    [TearDownFixture]
    procedure TearDownFixture;

    /// <summary>
    ///   Developer-machine-only perf-regression test for the RSM
    ///   load path against a real, multi-hundred-MB binary
    ///   (currently <c>TFW.exe</c> ~117 MB / <c>TFW.rsm</c> ~800 MB).
    ///   Reads the per-phase timings captured during [SetupFixture]
    ///   and asserts a total wall-time budget plus a per-phase
    ///   ceiling so that any return of the O(N^2) loops we removed
    ///   trips here with a specific message.
    /// </summary>
    /// <remarks>
    ///   When the fixture is absent, the test prints a visible
    ///   <c>WARNING</c> line to stdout and calls <c>Assert.Pass</c>
    ///   so the build stays green but the missing perf-net is
    ///   impossible to miss in the runner output. When a fixture
    ///   IS present but is much smaller than the calibrated TFW
    ///   baseline, the identity sanity-block (<c>.rsm</c> size,
    ///   proc count, class count) trips first instead of silently
    ///   passing every budget on a too-easy fixture.
    /// </remarks>
    [Test]
    procedure TestTfwLoadDiagnostic;

    /// <summary>
    ///   §6.27 PIN (large-binary / gap regime). On TFW the `$21` Self
    ///   register-param TypeIdx slot carries a 1-byte per-proc reference
    ///   (the hi byte is truncated by the structural-lookahead 1-byte
    ///   path), NOT the class's `$2A` registry primary. For
    ///   `TFormMain.Create` Self.TypeIdx = $84 while TFormMain's primary
    ///   is a wholly different id, so:
    ///   <list type="bullet">
    ///     <item>Self.TypeIdx ≠ FindTypeIdByName('TFormMain');</item>
    ///     <item>FindClassIdxByRsmTypeId(Self.TypeIdx) mis-resolves to an
    ///       UNRELATED class (the last writer of id $0084 —
    ///       TLayerCollectionAccess in this build), NOT TFormMain;</item>
    ///     <item>the proc-name split (FindClassByName('TFormMain')) is
    ///       the reliable static path; and</item>
    ///     <item>the per-proc ref is type-stable within the class
    ///       (TFormMain.Create and TFormMain.AfterMenuRebuild agree).</item>
    ///   </list>
    ///   Contrast with the small-binary clean regime pinned by
    ///   TestSelfTypeIdxResolvesInCleanRegime32. Skips when TFW absent.
    /// </summary>
    [Test]
    procedure TestTfwSelfTypeIdxIsPerProcRefNotRegistryId;

    /// <summary>
    ///   Developer-machine-only counterpart to TestFindGlobalTypeIdx32:
    ///   covers globals declared in the interface section of a real
    ///   large binary (TFW.exe), where the RSM encoder uses different
    ///   $20-record payload flags than the impl-scope globals our
    ///   DebugTarget fixture exercises.
    /// </summary>
    /// <remarks>
    ///   The agent's TFW repro showed <c>evaluate AppCaps.DbKindName</c>
    ///   failing with "Failed to evaluate variable" even though
    ///   <c>evaluate AppCaps</c> (bare global) returns sensible bytes.
    ///   We verify the two structural facts the dotted-walk relies on:
    ///   <list type="number">
    ///     <item>The interface-scope record types (TAppCaps, TKonsApl,
    ///       TMdt) actually land in FClasses (which is gated on the
    ///       record-field scanner accepting field names that don't
    ///       follow Delphi's 'F'-prefix convention).</item>
    ///     <item><c>FindRecordsByMemberName(field)</c> resolves a
    ///       field name back to the right record uniquely -- the
    ///       name-based fallback the evaluator falls back to when
    ///       a global's encoded type id doesn't match the registry
    ///       (still an open encoding question on TFW's interface-
    ///       scope globals).</item>
    ///   </list>
    /// </remarks>
    [Test]
    procedure TestTfwGlobalRecordResolves;

    /// <summary>
    ///   §6.16 CHARACTERIZATION: dotted field navigation on a large VCL
    ///   *class* instance (not a record global). Live-debugging TFW had
    ///   evaluate("Self.FAd") / "Self.FUDFProcess" fail for TFormAd. The
    ///   root cause is RSM-side: TFormAd is absent from FClasses because
    ///   TRsmStructDiscoverer.Run's class-trailer anchor
    ///   (<c>&lt;name&gt; 04 00 00 00 07 &lt;NL&gt; &lt;name&gt; 58 00 00 00</c>)
    ///   never matches its encoding -- the linker emits the trailer TAIL
    ///   (07 TFormAd 58 00 00 00) but not the adjacent HEAD. A control
    ///   record (TAppCaps) IS discovered. This test pins the current
    ///   broken state; flip the assertions to close the gap.
    /// </summary>
    [Test]
    procedure TestTfwClassInstanceFieldResolves;

    /// <summary>
    ///   §6.19 closure pin: TFormAd.FAd's
    ///   <c>Member.PointerTargetTypeIdx</c> must resolve to the
    ///   <c>TAd</c> record after the Format-A linker's pointer-alias
    ///   bridge runs (the <c>BindPointerAliasMembersByNameConvention</c>
    ///   pass). TFW exposes the §6.19 closure path the small
    ///   DebugTarget fixture can't: strict-private F-prefix fields
    ///   on a class have NO <c>$2C</c> record in TFW.rsm, so the
    ///   binding has to fire via the F-name + matching P-alias
    ///   convention rather than via the $2C linker. Without the
    ///   convention bridge, <c>evaluate Self.FAd.Land</c> falls into
    ///   the §6.18 name-based record fallback and bails on the
    ///   <c>Land</c>-on-TAd-and-Anschrift-siblings ambiguity.
    /// </summary>
    [Test]
    procedure TestTfwPointerAliasFAdBindsToTAd;

    /// <summary>
    ///   §6.24 closure pin. The heuristic name-convention enum
    ///   late-binding pass (<c>BindZeroIdFieldsByEnumNameConvention</c>)
    ///   recovers the enum NAME for record fields the structural decode
    ///   leaves with no type id. The canonical case is
    ///   <c>TAd.Land: TLandTyp</c>: no <c>$2C</c> / <c>$67</c> record
    ///   ties it to its type (§6.20 R6-R9), so before this pass
    ///   <c>Land.PrimitiveTypeId = 0</c> and <c>evaluate Self.FAd.Land</c>
    ///   could only surface the raw ordinal (the §6.20 Pfad-2 ceiling).
    ///   After the pass, <c>Land.PrimitiveTypeId</c> equals
    ///   <c>TLandTyp</c>'s 2-byte <c>$2A</c> id, <c>IsEnumTypeId</c> is
    ///   true, and ordinals 0 / 1 resolve to <c>ltInland</c> /
    ///   <c>ltAusland</c> through the same scope-local bridge the
    ///   §4.15 / §1E bridges use.
    ///
    ///   <para>Leakage guard: <c>Land</c>'s immediate neighbour
    ///   <c>Waehrung</c> (no <c>TWaehrung</c> / <c>TWaehrungTyp</c>
    ///   enum exists) MUST stay unbound (PrimitiveTypeId 0), and the
    ///   F-prefix field <c>FAd</c> keeps its §6.19 pointer-target
    ///   binding rather than being hijacked into an enum.</para>
    /// </summary>
    [Test]
    procedure TestTfwRecordFieldEnumNameConventionBindsLand;

    /// <summary>
    ///   Pin test for §6.4 (originally "elaborate record header,
    ///   TAppCaps-style nested sub-record shape" -- refuted). The
    ///   simple-shape record header documented in §4.13 covers every
    ///   TFW.exe record probed here, including the canonical "elaborate"
    ///   suspect TAppCaps. For each probe the gap between
    ///   <c>PStart = nameOff + 1 + NL + 4</c> and the first valid
    ///   <c>$02</c>-prefixed field record must equal the simple-shape
    ///   prediction <c>17 + managed * 8</c> (Win32).
    /// </summary>
    /// <remarks>
    ///   A regression that re-introduces an elaborate header (or
    ///   breaks the simple-shape decoder) trips here as a non-matching
    ///   gap. The probe list spans the records that the §6.3 work
    ///   already exercised, so the pin doubles as a structural
    ///   integrity check for TFW's interface-scope record layout.
    /// </remarks>
    [Test]
    procedure TestTfwSimpleRecordHeaderCoversTfwRecords;

    /// <summary>
    ///   §6.9 PIN: enum-typed $2C field bodies that match the
    ///   same-compilation cross-unit shape ($0C marker before the
    ///   "$9C $01" reference) must resolve their PrimitiveTypeId
    ///   to the OWNING-unit's primary via the EnumDecoder's
    ///   nearest-$2A-offset bridge -- NOT to the bogus 2-byte word
    ///   read of body bytes +3..+4 (which fabricates a non-
    ///   existent FieldId from the secondary-LOW byte and the per-
    ///   record slot index). TUserKonsOutlook.SyncDirection is the
    ///   canonical probe: its $2A primary is $7B7F (= 31615) for
    ///   TUserKonsOutlookDirection, and ukodBidirektional is the
    ///   ordinal-0 member.
    /// </summary>
    /// <remarks>
    ///   Round 5 of §6.9 tried a naive global LOW-byte map and
    ///   collapsed three same-LOW-byte enums onto one entry,
    ///   producing the wrong primary for SyncDirection. Round 6
    ///   adds the (LowByte, Primary, $2A file offset) triplet to
    ///   the bridge and picks the entry by argmin(|TagOff -
    ///   $2A_offset|); same-unit $2A entries are ~1.7 KB away
    ///   from the field, other same-LOW-byte enums sit megabytes
    ///   away, so the nearest-offset rule is a per-unit selector
    ///   by construction. A regression that drops the per-unit
    ///   scoping trips here as a wrong PrimitiveTypeId on
    ///   SyncDirection.
    /// </remarks>
    [Test]
    procedure TestTfwEnumTypedFieldResolvesToPrimary;

    {$IFDEF CPUX64}
    /// <summary>
    ///   Pin test for §6.7 (Win64 $A0 byte-7 = $03 sub-form).
    ///   Procs with a single $28 record whose $A0 form has byte 7
    ///   low-nibble = $03 (TStringList.Add, TObject.Create,
    ///   System.SysUtils.IntToStr) historically decoded to
    ///   SegmentOffset = 0 -- the only $A0 path was the
    ///   high-RVA (V shl 4)|$07 variant that rejects $03. With
    ///   TryWin64A0 Variant B (21-bit packed form at bytes 0..2 of
    ///   the address slot, same encoding the $20 sub-tag's
    ///   TryWin64 uses), the same procs decode byte-exact against
    ///   the .map. This pin asserts the decoded SegmentOffset
    ///   matches the map for three low-RVA Win64 probes.
    /// </summary>
    [Test]
    procedure TestTfwWin64ProcAddressDecodesViaA0Narrow;
    /// <summary>
    ///   Pin test for §6.2 (Win64 proc-address VAs above 2 MB).
    ///   Verifies that TFW.Win64 procs with .map-reported RVAs well
    ///   beyond the historical 21-bit / 2 MB cap decode to exactly
    ///   the right <c>SegmentOffset</c> via the new <c>TryWin64A0</c>
    ///   path (subtracts $1000 from the $A0 form's bytes 7..10
    ///   instead of the Win32-specific $401000 the old dispatcher
    ///   applied unconditionally). Asserts are byte-exact against
    ///   the .map ground truth for four named procs spanning the
    ///   [~94 MB, ~129 MB] RVA window.
    /// </summary>
    /// <remarks>
    ///   Probes deliberately stay above 2 MB: that's the window the
    ///   old decoder mishandled, so a regression that re-routes $A0
    ///   to TryWin32 (or drops the arch-detection step in
    ///   TRsmScanner.LoadFromFile) trips here immediately. Procs
    ///   with the $A0 byte-7 low nibble != $07 (TStringList.Add,
    ///   TObject.Create, IntToStr) are a SEPARATE Win64 sub-form
    ///   tracked under §6.7 and intentionally not asserted here.
    /// </remarks>
    [Test]
    procedure TestTfwWin64ProcAddressDecodesAboveCap;
    {$ENDIF}

    /// <summary>
    ///   §4.17 / §6.21 closure pin against TFW.rsm. The cross-unit
    ///   symbol-import table must decode at least one segment whose
    ///   <c>$66</c> entries include <c>TLandTyp</c> with the canonical
    ///   little-endian RVA <c>$7DA69008</c> (the bytes
    ///   <c>08 90 A6 7D</c> the linker emitted at body+3..+6 of the
    ///   canonical <c>$2A 'TLandTyp'</c> registry entry at file offset
    ///   54816881). The Round-3 PowerShell scan counted 68 <c>$66
    ///   'TLandTyp'</c> occurrences across TFW; we pin a lower bound
    ///   of 60 to allow the linker some slack without losing the
    ///   structural finding.
    /// </summary>
    [Test]
    procedure TestTfwUnitUseTableContainsTLandTyp;

    /// <summary>
    ///   §4.17 / §6.21 cross-validation pin against TFW.rsm. Asserts
    ///   that at least one <c>$67 'ltInland'</c> symbol-reference
    ///   entry carries the payload <c>$10A43981</c> (the bytes
    ///   <c>81 39 A4 10</c> §6.20 Round-3 observed) AND that those
    ///   same 4 bytes appear at file offset
    ///   <c>54816777 (= 54816766 + 11)</c> -- the
    ///   canonical <c>$25 'ltInland'</c> block's body. The match
    ///   anchors the §6.21 finding that the <c>$67</c> payload is
    ///   the symbol's canonical declaration RVA (or its in-stream
    ///   equivalent) for the enum-element family, and pins the
    ///   structural relationship between the <c>$67</c> use-site
    ///   table and the <c>$25</c> canonical block.
    /// </summary>
    [Test]
    procedure TestTfwUnitUseTableLtInlandTokenMatchesCanonicalBlock;

    /// <summary>
    ///   §6.27 REFUTATION PIN (closed-as-design-limit). The per-proc
    ///   `$21`/`$22` TypeIdx low byte is a per-(unit,type) ref number,
    ///   NOT a registry id (Hypotheses A/B/C, pinned by
    ///   TestTfwSelfTypeIdxIsPerProcRefNotRegistryId). This pin nails the
    ///   last decode lead — Hypothesis D: "the ref is an INDEX into the
    ///   owning unit's §4.17 `$64`/`$66` unit-use table" — as REFUTED, so
    ///   no decoder work could make a class-typed local/param statically
    ///   resolvable, and §6.27 is a design limit (not an open gap). Three
    ///   refuting facts, all computed from the loaded reader so they
    ///   survive rebuild drift:
    ///   <list type="number">
    ///     <item>AOwner is TComponent in BOTH TFormMain.Create and
    ///       TFormVBh.Create, yet its per-proc ref differs ($10 vs $66) —
    ///       not a stable type identifier.</item>
    ///     <item>TComponent IS present in Tfw.Main.Form's unit-use refs,
    ///       but at an index ≠ AOwner's ref ($10).</item>
    ///     <item>TFormMain (Self's type) is NOT in Tfw.Main.Form's
    ///       unit-use refs at all, yet Self carries a ref ($84) — so the
    ///       ref cannot be sourced from the import table.</item>
    ///   </list>
    /// </summary>
    [Test]
    procedure TestTfwPerProcRefIsNotUnitUseTableIndex;

    /// <summary>
    ///   §6.25 CLOSURE PIN (naming-convention phantom drop). The
    ///   synthesised-EnumDef phantom residual on TFW was LARGE (114
    ///   survivors), not the "handful of irreducible const-borrows" the
    ///   older write-up implied. The phantoms cluster by
    ///   definitively-non-enum naming convention; FilterPhantomEnumDefs
    ///   condition (c) now drops the high-confidence ones (interface
    ///   I&lt;Upper&gt;, pointer P&lt;Upper&gt;, leading-underscore,
    ///   tag&lt;Upper&gt;, ALL_CAPS Windows/API typedefs). This pin verifies:
    ///   <list type="bullet">
    ///     <item>DROPPED: QOS_OBJECT_HDR, tagXFORM, IRichChunk,
    ///       NET_STRING, DXGI_COLOR_SPACE_TYPE no longer surface as
    ///       EnumDefs.</item>
    ///     <item>NO surviving synthesised EnumDef matches the dropped
    ///       conventions (the pass left none behind).</item>
    ///     <item>LEAKAGE GUARDS: genuine enums survive — TPictureFormat
    ///       (T sparse), TZO (all-caps T-prefixed — the scariest case,
    ///       protected by the T&lt;Upper&gt; guard), and
    ///       TZUGFeRDXMLObjectTyp (genuine enum ALSO registered skRecord,
    ///       which is why filter (a) stays skClass-only; the stale
    ///       TThreadPriority example is refuted — it is not skRecord on
    ///       current TFW).</item>
    ///   </list>
    /// </summary>
    [Test]
    procedure TestTfwSynthEnumPhantomResidual;

    /// §6.25 TTokenKind-doubling closure pin. FilterPhantomEnumDefs
    /// condition (d) drops a SYNTHESISED EnumDef that shares its
    /// (TypeName, UnitName) with a $03-sourced def -- the same Delphi type,
    /// for which the $03 is authoritative. Asserts the invariant (NO synth
    /// survivor shares (name,unit) with a $03 def -- 24 such same-unit
    /// duplicates existed before the fix on TFW) AND the unit-scoping
    /// leakage guard (a synth whose same-name $03 sibling lives in ANOTHER
    /// unit -- a genuine cross-unit homonym, e.g. TDataType in
    /// VCLTee.TeeSpline vs bmpfilt -- still survives).
    [Test]
    procedure TestTfwEnumDefNoSameUnitSynthDuplicate;

    /// <summary>
    ///   §6.41 CLOSURE PRECONDITIONS (large binary). §6.41 is closed by a
    ///   consumer-side T&lt;localName&gt; record bridge in the dotted walk
    ///   (DPT.Debugger.EvaluateVariable); the reader side is unchanged
    ///   (the per-proc alias stays a §4.2 ref). This pins the reader-side
    ///   facts the bridge relies on, via relationships (not raw offsets/ids,
    ///   which drift per build): TKonsMis is a UNIQUE discovered record
    ///   carrying Name + Typ (the bridge's target); the KonsMis local has a
    ///   real in-frame offset (§6.40 holds, not the fallback); the local's
    ///   per-proc alias does NOT resolve to TKonsMis (the §6.18 collision
    ///   that makes the bridge necessary); and Name is ambiguous across
    ///   many records (so the §6.36 field-name fallback alone can't bind).
    ///   The end-to-end dotted walk (KonsMis.Name -&gt; 'Volltextsuche')
    ///   is verified live, not here -- DebugTarget's field names are
    ///   unambiguous so its §6.36 fallback binds without reaching the bridge.
    /// </summary>
    [Test]
    procedure TestKonsMisLocalRecordBridgePreconditions;
  end;

  /// Tests against the Taifun Test.Lib DUnitX runner (`Test.Lib.exe`,
  /// ~172 MB exe / ~240 MB .rsm). Shares the base's one-time reader /
  /// map / phase-timing machinery — additional Test.Lib tests added here
  /// reuse the single `FReader` loaded in [SetupFixture] rather than each
  /// re-loading the multi-second fixture.
  [TestFixture]
  TRsmTestLibTests = class(TRsmHeavyFixtureTests)
  // The §6.31 corpus: a real binary whose classes follow the codebase's
  // 'C'-prefix convention (CJwksValidator et al.) — TFW is all
  // 'T'-prefixed and so cannot exercise the convention-free
  // class-discovery gate. Lives in the in-repo, project-relative
  // RsmTaifunData folder, resolved at runtime by the unit-level
  // TestLibExePath function below.
  public
    /// One-time setup: loads Test.Lib.rsm (+ .map) once via the base.
    [SetupFixture]
    procedure SetupFixture;
    /// Frees the shared reader / map / phase table.
    [TearDownFixture]
    procedure TearDownFixture;

    /// <summary>
    ///   §6.31 PIN (large binary). The convention-free class-discovery
    ///   gate finds the 'C'-prefixed CJwksValidator in Test.Lib.rsm with
    ///   its fields at the right instance offsets (FCache @4,
    ///   FExpectedTenantId @16 — matching the live VMT layout that
    ///   "evaluate V.FExpectedTenantId" reads). Confirms the retired
    ///   'T'-prefix crutch fix holds at large-binary scale, not just on
    ///   the small DebugTarget fixture.
    /// </summary>
    [Test]
    procedure TestLargeBinaryNonTClassDiscovered;

    /// <summary>
    ///   §6.33 SHAPE PIN (large binary). Pins the decoded body=9
    ///   managed-reference $2C field-record layout for
    ///   CJwksValidator.FExpectedTenantId in Test.Lib.rsm: After+3 is
    ///   the single-byte UnicodeString id ($04) and After+4 is 2x the
    ///   field's instance offset (positional, not a type-id byte). The
    ///   end-to-end attribution that consumes this shape is pinned
    ///   separately by TestWideBlockNameSetBindsCJwksValidatorStrings
    ///   (§6.33-C); this pins the byte shape only.
    /// </summary>
    [Test]
    procedure TestExpectedTenantIdFieldRecordShapePinned;

    /// <summary>
    ///   §6.33-C PIN (large binary). The member-name-set pass attributes
    ///   CJwksValidator's wide-$2C field block (unit-local parent id
    ///   $0391, colliding across ~18 types) to the class via a unique
    ///   sorted member-name-set match, so its string fields carry the
    ///   single-byte UnicodeString id $04 and auto-type without type=.
    ///   Leakage guard: the class-typed FCache stays unstamped.
    /// </summary>
    [Test]
    procedure TestWideBlockNameSetBindsCJwksValidatorStrings;
  end;

implementation

// Heavy-fixture corpus location. The TFW / Test.Lib binaries (+ their
// .map / .rsm sidecars) live in-repo under Projects\DPT\Test\RsmTaifunData
// (current builds, committed alongside the tests) rather than at a
// machine-specific absolute path. Resolved CWD-relative with the same
// two-form fallback ResolveExePath uses: the build host runs with the
// repo root as CWD, a direct run from the Test dir does not.
function RsmTaifunDataPath(const AFile: String): String;
begin
  Result := ExpandFileName('Projects\DPT\Test\RsmTaifunData\' + AFile);
  if not TFile.Exists(Result) then
    Result := ExpandFileName('RsmTaifunData\' + AFile);
end;

function TfwExePath: String;      
begin 
  Result := RsmTaifunDataPath('TFW.exe'); 
end;

function TfwMapPath: String;      
begin 
  Result := RsmTaifunDataPath('TFW.map'); 
end;

function TfwWin64ExePath: String; 
begin 
  Result := RsmTaifunDataPath('TFW.Win64.Debug.exe'); 
end;

function TfwWin64MapPath: String; 
begin 
  Result := RsmTaifunDataPath('TFW.Win64.Debug.map'); 
end;

function TestLibExePath: String;  
begin 
  Result := RsmTaifunDataPath('Test.Lib.exe'); 
end;

{ TRsmHeavyFixtureTests }

function TRsmHeavyFixtureTests.ShouldSkip: Boolean;
begin
  Result := FSkipReason <> '';
  if Result then
    Assert.Pass(FSkipReason);
end;

function TRsmHeavyFixtureTests.LoadPrimaryFixture(
  const AExePath, ALabel: String): Boolean;
var
  TotalSW: TStopwatch;
  PhaseSW: TStopwatch;
  LogPath: String;
  W      : TStreamWriter;
  I      : Integer;
  MapPath: String;
begin
  FSkipReason := '';
  if not TFile.Exists(AExePath) then
  begin
    FSkipReason := Format(
      'WARNING: %s tests SKIPPED -- fixture not found at "%s". The ' +
      'large-binary pins / perf-regression budgets cannot be exercised ' +
      'without it.', [ALabel, AExePath]);
    Writeln(FSkipReason);
    Exit(False);
  end;

  // Announce the fixture identity before the multi-second load starts.
  // The same line goes into dpt.log so a post-mortem comparison across
  // runs can match different .rsm sizes to different timings.
  FFixtureRsm := ChangeFileExt(AExePath, '.rsm');
  FFixtureMB := 0;
  if TFile.Exists(FFixtureRsm) then
    FFixtureMB := TFile.GetSize(FFixtureRsm) / (1024 * 1024);
  FFixtureLine := Format('  fixture: exe=%s  rsm=%s (%.1f MB)',
    [AExePath, FFixtureRsm, FFixtureMB]);
  Writeln(FFixtureLine);

  FPhaseTable   := TStringList.Create;
  FPhaseTimings := Collections.NewPlainKeyValue<String, Int64>;
  FReader       := TRsmReader.Create;

  PhaseSW := TStopwatch.StartNew;
  FReader.OnPhase :=
    procedure(APhase: String)
    begin
      PhaseSW.Stop;
      FPhaseTable.Add(Format('  %-32s %d ms',
        [APhase, PhaseSW.ElapsedMilliseconds]));
      FPhaseTimings[APhase] := PhaseSW.ElapsedMilliseconds;
      PhaseSW := TStopwatch.StartNew;
    end;
  TotalSW := TStopwatch.StartNew;
  FReader.LoadFromFile(AExePath);
  TotalSW.Stop;
  FTotalLoadMs := TotalSW.ElapsedMilliseconds;

  // Mirror the timings into dpt.log (the file the user already tails for
  // live progress) and echo to stdout so the DUnitX runner captures them.
  var Summary: String := Format('%s [%s-diag] total=%d ms  procs=%d  classes=%d',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), ALabel,
     Integer(FTotalLoadMs), FReader.Procs.Count, FReader.Classes.Count]);
  LogPath := ChangeFileExt(ParamStr(0), '') + '.log';
  try
    W := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
    try
      W.WriteLine(Summary);
      W.WriteLine(FFixtureLine);
      for I := 0 to FPhaseTable.Count - 1 do
        W.WriteLine(FPhaseTable[I]);
    finally
      W.Free;
    end;
  except
    // diagnostics must never throw
  end;
  Writeln(Summary);
  for I := 0 to FPhaseTable.Count - 1 do
    Writeln(FPhaseTable[I]);

  // The sibling .map is shared by tests that tie a decoded value back to
  // ground-truth RVAs; load it once here when present.
  MapPath := ChangeFileExt(AExePath, '.map');
  if TFile.Exists(MapPath) then
    FMap := TMapFileParser.Create(MapPath);

  Result := True;
end;

procedure TRsmHeavyFixtureTests.FreePrimaryFixture;
begin
  FreeAndNil(FMap);
  FreeAndNil(FReader);
  FreeAndNil(FPhaseTable);
  FPhaseTimings := nil;
end;

{ TRsmTfwTests }

{$IFDEF CPUX64}
function TRsmTfwTests.ShouldSkipWin64: Boolean;
begin
  // Cascade: if the Win32 fixture is missing the setup bailed early
  // and FScannerWin64 / FReaderWin64 are nil too. Treat both
  // skip-paths uniformly so the test never dereferences a nil reader.
  if (FWin64SkipReason = '') and (FReaderWin64 = nil) then
    FWin64SkipReason := FSkipReason;
  Result := FWin64SkipReason <> '';
  if Result then
    Assert.Pass(FWin64SkipReason);
end;
{$ENDIF}

procedure TRsmTfwTests.SetupFixture;
const
  // Spot-check procs: a handful of named TFW procs whose presence
  // proves the proc decoder didn't silently drop them. The
  // diagnostic output of this list is what we used historically to
  // narrow down which encoding bucket a missing proc fell into.
  SpotProcs: array[0..3] of String = (
    'TFormMain.Create', 'TFormVBh.Create',
    'TFormVBh.CreateGsVBhBridge', 'TFormMain.AfterMenuRebuild');
var
  LogPath: String;
  W      : TStreamWriter;
begin
  // Shared resource load (reader + .map + phase timings + skip state).
  if not LoadPrimaryFixture(TfwExePath, 'TFW') then Exit;
  LogPath := ChangeFileExt(ParamStr(0), '') + '.log';

  // Spot-check the decoder on a handful of named procs from TFW.
  // The MCP-session diagnostic showed FindProcContaining missing
  // for the user's breakpoint, so we cross-reference what the
  // reader actually stored against the addresses the .map gives
  // for the same procs. Mismatch or SegmentOffset = 0 here proves
  // the decoder still drops them; a correct value indicates a
  // downstream issue (size table, lookup, sort).
  var SpotLine: String;
  for var Sp in SpotProcs do
  begin
    var Pi := FReader.FindProcByName(Sp);
    if Pi >= 0 then
    begin
      var SpProc := FReader.Procs[Pi];
      SpotLine := Format('  spot: %-36s idx=%d  seg=0x%x  size=%d  locals=%d',
        [Sp, Pi, SpProc.SegmentOffset, SpProc.Size, SpProc.Locals.Count]);
      Writeln(SpotLine);
      try
        W := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
        try W.WriteLine(SpotLine); finally W.Free; end;
      except end;
      for var Li := 0 to SpProc.Locals.Count - 1 do
      begin
        var L := SpProc.Locals[Li];
        var KindStr: String;
        case L.Kind of
          lkBpRel:    KindStr := 'bp';
          lkRegister: KindStr := 'reg';
        else
          KindStr := '?';
        end;
        SpotLine := Format('    local[%d] %-12s kind=%s  bp=%d  reg=%d  typeIdx=0x%x',
          [Li, L.Name, KindStr, L.BpOffset, L.RegParamIdx, L.TypeIdx]);
        Writeln(SpotLine);
        try
          W := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
          try W.WriteLine(SpotLine); finally W.Free; end;
        except end;
      end;
    end
    else
    begin
      SpotLine := Format('  spot: %-36s <not in reader>', [Sp]);
      Writeln(SpotLine);
      try
        W := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
        try W.WriteLine(SpotLine); finally W.Free; end;
      except end;
    end;
  end;

  {$IFDEF CPUX64}
  // Win64 fixture for §6.2 (proc-address VAs > 2 MB) RE. Loaded only
  // when the test exe is itself 64-bit -- the .rsm is ~1.17 GB and
  // the Win32 process already has the Win32 TFW .rsm mapped, so
  // mapping the Win64 .rsm on top would overrun a 32-bit address
  // space.
  if TFile.Exists(TfwWin64ExePath) then
  begin
    Writeln(Format('  win64-fixture: exe=%s', [TfwWin64ExePath]));
    FReaderWin64 := TRsmReader.Create;
    FReaderWin64.LoadFromFile(TfwWin64ExePath);
    if TFile.Exists(TfwWin64MapPath) then
      FMapWin64 := TMapFileParser.Create(TfwWin64MapPath);
    Writeln(Format('  win64-fixture: procs=%d  classes=%d  is64=%s',
      [FReaderWin64.Procs.Count, FReaderWin64.Classes.Count,
       BoolToStr(FReaderWin64.Scanner.Is64Bit, True)]));
  end
  else
  begin
    FWin64SkipReason := Format(
      'WARNING: §6.2 Win64 tests SKIPPED -- Win64 fixture not found ' +
      'at "%s". Without the > 2 MB-RVA corpus the proc-address ' +
      'decoder cannot be exercised.', [TfwWin64ExePath]);
    Writeln(FWin64SkipReason);
  end;
  {$ENDIF}
end;

procedure TRsmTfwTests.TearDownFixture;
begin
  {$IFDEF CPUX64}
  FreeAndNil(FMapWin64);
  FreeAndNil(FReaderWin64);
  {$ENDIF}
  FreePrimaryFixture;  // base: frees FMap / FReader / FPhaseTable
end;

procedure TRsmTfwTests.TestTfwLoadDiagnostic;
const
  // Total budget: the post-fix load was ~24 s on the developer
  // machine at the time of the original calibration. Allow 3x for
  // CI / cold-cache / slower disks but trip well below the 425 s
  // pre-fix value. A regression past this will get caught at the
  // next test run instead of surfacing as a frozen MCP session
  // minutes after deploy. Raised 90 -> 150 s post-§6.16 (cap raise
  // 128 -> 2048 in the field-discovery walk) + §6.21 (cross-unit
  // symbol-import table decoding adds modest per-byte cost in
  // ScanSymbolStream and per-segment alloc afterwards).
  HardLimit  = 150 * 1000;
  // Per-phase budgets, picked so each O(N^2) regression we just
  // killed (insertion-sort on 200k procs, per-byte String alloc
  // in the struct scan) trips its own assertion with a clear
  // message. Each is roughly 5-10x the observed post-fix value.
  // Names must match the strings emitted by OnPhase in
  // TRsmReader (ScanSymbolStream / RecomputeProcSizes /
  // DiscoverAndParseAllStructs / LinkMemberTypeIdsFromFormatA /
  // DeriveClassParents); a typo here silently passes.
  // Raised 45 -> 90 s post-§6.16 (field-discovery cap 128 -> 2048
  // lifts the trailer-scan cost into the 50-65 s band on cold-
  // cache Win32/Win64 runs against TFW.rsm) + §6.21 (the new
  // $64-anchored byte walk allocates segment lists this phase
  // then walks past). The 90 s budget still catches an O(N^2)
  // regression (the pre-fix value was ~200 s) while accepting
  // the cumulative feature cost.
  PhaseBudget_DiscoverAndParseAllStructs   = 90 * 1000;
  PhaseBudget_RecomputeProcSizes           = 20 * 1000;
  PhaseBudget_LinkMemberTypeIdsFromFormatA = 25 * 1000;
  PhaseBudget_DeriveClassParents           = 30 * 1000;
  // ScanSymbolStream now also processes $25 enum-constant records
  // (both program-local $0A and cross-unit $8A forms) and $2A
  // type-registry entries inline for the cross-unit enum alias
  // linking. Each adds modest per-byte work that on TFW-class
  // binaries (800MB+ RSM) lifts the phase from ~7s baseline to
  // ~13s; raise the budget to 20s so the test still catches a
  // genuine O(N^2) regression while accepting the new feature
  // cost.
  PhaseBudget_ScanSymbolStream             = 20 * 1000;
var
  PhaseMs: Int64;
begin
  if ShouldSkip then Exit;

  // Sanity-check the fixture identity before we trust the perf
  // budgets below. A "TFW.exe" pointing at a hello-world build
  // would silently pass every budget because its sub-MB .rsm
  // takes ~0 ms in every phase -- a useless green light. Require
  // a large sidecar AND a six-figure proc count so a wrong
  // fixture trips here with a clear message instead.
  Assert.IsTrue(FFixtureMB > 50,
    Format('Fixture .rsm is suspiciously small (%.1f MB). The ' +
           'perf-regression test needs a multi-hundred-MB sidecar ' +
           'to exercise the bulk-scan hot paths; the budgets below ' +
           'are meaningless on a tiny fixture.', [FFixtureMB]));

  Assert.IsTrue(FTotalLoadMs < HardLimit,
    Format('RSM load on TFW exceeded %d ms (took %d ms) -- ' +
           'check phase timings in dpt.log for the stalled section.',
      [HardLimit, FTotalLoadMs]));

  // Identity sanity-check (continued from the .rsm-size guard at
  // the top): a TFW-class fixture must surface six-figure proc
  // and four-figure class counts. The previous "> 1000" gate
  // would have happily accepted a much smaller binary as the
  // fixture, masking the fact that the perf budgets are no
  // longer exercising the bulk-scan paths they were tuned for.
  Assert.IsTrue(FReader.Procs.Count > 100000,
    Format('Fixture produced only %d procs; expected >100k. ' +
           'Likely pointing at a much smaller binary than the ' +
           'TFW-class fixture this test is calibrated for.',
      [FReader.Procs.Count]));
  Assert.IsTrue(FReader.Classes.Count > 1000,
    Format('Fixture produced only %d classes; expected >1000.',
      [FReader.Classes.Count]));

  // Per-phase regression guards. Each budget is well above the
  // post-fix value (~0.5-12 s) but far below the broken-state
  // value (~180-200 s) so a future O(N^2) creeping back into
  // any one phase trips here with a specific failure message
  // rather than a generic "load too slow". Parallel arrays
  // pair name and budget by index -- a small struct would be
  // tidier, but Delphi forbids declaring local record types
  // inline inside a procedure body.
  var BudgetNames: TArray<String> := [
    'ScanSymbolStream',
    'RecomputeProcSizes',
    'DiscoverAndParseAllStructs',
    'LinkMemberTypeIdsFromFormatA',
    'DeriveClassParents'
  ];
  var BudgetMs: TArray<Int64> := [
    PhaseBudget_ScanSymbolStream,
    PhaseBudget_RecomputeProcSizes,
    PhaseBudget_DiscoverAndParseAllStructs,
    PhaseBudget_LinkMemberTypeIdsFromFormatA,
    PhaseBudget_DeriveClassParents
  ];
  for var Bi := 0 to High(BudgetNames) do
  begin
    Assert.IsTrue(FPhaseTimings.TryGetValue(BudgetNames[Bi], PhaseMs),
      Format('Phase "%s" not reported by the reader -- the OnPhase ' +
             'name may have been renamed; update this test to match.',
        [BudgetNames[Bi]]));
    Assert.IsTrue(PhaseMs < BudgetMs[Bi],
      Format('Phase "%s" exceeded budget: took %d ms (budget %d ms). ' +
             'A perf regression likely reintroduced an O(N^2) loop -- ' +
             'inspect the phase against its post-fix baseline.',
        [BudgetNames[Bi], PhaseMs, BudgetMs[Bi]]));
  end;
end;

procedure TRsmTfwTests.TestKonsMisLocalRecordBridgePreconditions;
var
  KmCls, Pi, KmLocalIdx, AliasIdx: Integer;
  KmTypeIdx                      : UInt32;
  HasName, HasTyp                : Boolean;
begin
  if ShouldSkip then Exit;

  // 1. TKonsMis is discovered as a UNIQUE record carrying Name + Typ.
  //    (Members asserted by NAME, not offset, to stay build-stable.)
  KmCls := FReader.FindClassByName('TKonsMis');
  Assert.IsTrue(KmCls >= 0, 'TKonsMis must be discovered as a struct');
  Assert.AreEqual(Ord(skRecord), Ord(FReader.Classes[KmCls].Kind),
    'TKonsMis must be a record');
  HasName := False;
  HasTyp  := False;
  for var M := 0 to FReader.Classes[KmCls].Members.Count - 1 do
  begin
    if SameText(FReader.Classes[KmCls].Members[M].Name, 'Name') then HasName := True;
    if SameText(FReader.Classes[KmCls].Members[M].Name, 'Typ')  then HasTyp  := True;
  end;
  Assert.IsTrue(HasName and HasTyp,
    'TKonsMis must carry the Name and Typ members the dotted walk targets');
  var KmCount: Integer := 0;
  for var Q := 0 to FReader.Classes.Count - 1 do
    if SameText(FReader.Classes[Q].Name, 'TKonsMis') then Inc(KmCount);
  Assert.AreEqual(Integer(1), KmCount,
    'TKonsMis must be uniquely discovered (so a name-based bind, if ever ' +
    'added, would be unambiguous)');

  // 2. The KonsMis local exists with a real in-frame offset -- §6.40 holds
  //    (a small negative slot, NOT the -10000 synthesized fallback nor the
  //    pre-fix out-of-frame -1535).
  Pi := FReader.FindProcByName('CKonsMisDict.DataLoad');
  Assert.IsTrue(Pi >= 0, 'CKonsMisDict.DataLoad must be in the reader');
  KmTypeIdx  := 0;
  KmLocalIdx := -1;
  for var Li := 0 to FReader.Procs[Pi].Locals.Count - 1 do
    if SameText(FReader.Procs[Pi].Locals[Li].Name, 'KonsMis') then
    begin
      KmLocalIdx := Li;
      KmTypeIdx  := FReader.Procs[Pi].Locals[Li].TypeIdx;
    end;
  Assert.IsTrue(KmLocalIdx >= 0, 'KonsMis local must be present');
  Assert.IsTrue(FReader.Procs[Pi].Locals[KmLocalIdx].BpOffset > -1000,
    '§6.40: KonsMis must decode to a real in-frame slot, not the fallback');

  // 3. WHY THE BRIDGE IS NEEDED: the local''s per-proc alias does NOT
  //    bind to TKonsMis at the reader level -- it resolves to an unrelated
  //    struct (the §6.18 alias collision) or nowhere (a §4.2 design limit,
  //    not fixed). The §6.41 closure works around this consumer-side via
  //    the T<localName> bridge; this asserts the reader-level mis-binding
  //    that makes the bridge necessary stays as analysed.
  AliasIdx := FReader.FindClassIdxByRsmTypeId(KmTypeIdx);
  Assert.AreNotEqual(KmCls, AliasIdx,
    '§6.41: the per-proc alias must not resolve to TKonsMis at the reader ' +
    'level (the §4.2 limit the consumer T<localName> bridge compensates for)');

  // 4. The §6.36 field-name fallback cannot disambiguate: Name is shared
  //    by many records, so its unique-match guard can never fire.
  Assert.IsTrue(Length(FReader.FindRecordsByMemberName('Name')) > 1,
    'Name must be ambiguous across records (defeats the §6.36 fallback)');
end;

procedure TRsmTfwTests.TestTfwGlobalRecordResolves;

  procedure CheckFieldUniquelyOwnedBy(const AFieldName, AExpectedRecord: String);
  var
    Hits: TArray<Integer>;
  begin
    Hits := FReader.FindRecordsByMemberName(AFieldName);
    Assert.AreEqual(Integer(1), Integer(Length(Hits)),
      Format('FindRecordsByMemberName(%s) must return exactly one ' +
             'record, got %d. Ambiguity here would force the dotted-' +
             'walk to give up on the name-based fallback.',
        [AFieldName, Length(Hits)]));
    Assert.AreEqual(AExpectedRecord, FReader.Classes[Hits[0]].Name,
      Format('Field %s is owned by the wrong record -- expected %s.',
        [AFieldName, AExpectedRecord]));
  end;

var
  Idx: Integer;
begin
  if ShouldSkip then Exit;

  // Pinpoint diagnostic: surface each upstream layer's state
  // before the assertions, so a regression points at the broken
  // layer instead of just "lookup failed".
  var DumpCls: Integer;
  DumpCls := FReader.FindClassByName('TAppCaps');
  Writeln(Format('  tfw-global: TAppCaps    in FClasses idx=%d members=%d',
    [DumpCls, FReader.Classes[DumpCls].Members.Count]));
  Writeln(Format('  tfw-global: TAppCaps    TypeIdx=0x%x (record-name file offset)',
    [FReader.Classes[DumpCls].TypeIdx]));
  // Count how many TAppCaps occurrences are in FClasses -- if more
  // than one, the scanner may have picked the wrong layout.
  var TAppCapsCount: Integer := 0;
  for var Q: Integer := 0 to FReader.Classes.Count - 1 do
    if SameText(FReader.Classes[Q].Name, 'TAppCaps') then
      Inc(TAppCapsCount);
  Writeln(Format('  tfw-global: TAppCaps    occurrences in FClasses=%d',
    [TAppCapsCount]));
  for var M: Integer := 0 to FReader.Classes[DumpCls].Members.Count - 1 do
    Writeln(Format('    [%d] off=%d (0x%x) name=%s',
      [M, FReader.Classes[DumpCls].Members[M].Offset,
       FReader.Classes[DumpCls].Members[M].Offset,
       FReader.Classes[DumpCls].Members[M].Name]));
  Writeln(Format('  tfw-global: TKonsApl    in FClasses idx=%d',
    [FReader.FindClassByName('TKonsApl')]));
  Writeln(Format('  tfw-global: TMdt        in FClasses idx=%d',
    [FReader.FindClassByName('TMdt')]));
  Writeln(Format('  tfw-global: AppCaps             FindGlobalTypeIdx=0x%x',
    [FReader.FindGlobalTypeIdx('AppCaps')]));
  Writeln(Format('  tfw-global: GlobalKonsAplPortal FindGlobalTypeIdx=0x%x',
    [FReader.FindGlobalTypeIdx('GlobalKonsAplPortal')]));
  Writeln(Format('  tfw-global: MdtGlobal           FindGlobalTypeIdx=0x%x',
    [FReader.FindGlobalTypeIdx('MdtGlobal')]));

  // The interface-scope record types must land in FClasses,
  // which is gated on the structural-anchor field scanner
  // accepting non-F-prefixed field names like DbKindName.
  Assert.IsTrue(FReader.FindClassByName('TAppCaps') >= 0,
    'TAppCaps must be discovered as a record');
  Assert.IsTrue(FReader.FindClassByName('TKonsApl') >= 0,
    'TKonsApl must be discovered as a record');
  Assert.IsTrue(FReader.FindClassByName('TMdt') >= 0,
    'TMdt must be discovered as a record');

  // The dotted-walk's name-based fallback resolves
  // GlobalName.FieldName by searching every record for the
  // field name. The agent's repro used AppCaps.DbKindName --
  // that's the structural anchor we verify here.
  CheckFieldUniquelyOwnedBy('DbKindName', 'TAppCaps');

  // FindBestRecordForGlobalAndField is the actual resolver the
  // dotted-walk calls when the encoded-type-id path misses (which
  // is the entire TFW case). Each line below matches a row of the
  // agent's repro recipe: "evaluate <GlobalName>.<FieldName>"
  // must succeed, and "succeed" starts with picking the correct
  // record type here. AplMsgTable (Boolean inside TAppCaps), Id
  // (Word inside TMdt, also lives on many other records), and
  // Match (ShortString on TMdt) are the three shapes that have
  // to all hit the right record despite Id/Match being common
  // field names across the codebase.
  Idx := FReader.FindBestRecordForGlobalAndField('AppCaps', 'DbKindName');
  Assert.IsTrue(Idx >= 0,
    'AppCaps.DbKindName must resolve via FindBestRecordForGlobalAndField');
  Assert.AreEqual('TAppCaps', FReader.Classes[Idx].Name,
    'AppCaps.DbKindName resolved to wrong record');

  Idx := FReader.FindBestRecordForGlobalAndField('AppCaps', 'AplMsgTable');
  Assert.IsTrue(Idx >= 0,
    'AppCaps.AplMsgTable must resolve');
  Assert.AreEqual('TAppCaps', FReader.Classes[Idx].Name,
    'AppCaps.AplMsgTable resolved to wrong record');

  Idx := FReader.FindBestRecordForGlobalAndField('MdtGlobal', 'Id');
  Assert.IsTrue(Idx >= 0,
    'MdtGlobal.Id must resolve -- proximity fallback should pick TMdt');
  Assert.AreEqual('TMdt', FReader.Classes[Idx].Name,
    'MdtGlobal.Id resolved to wrong record (Id is shared across many ' +
    'records, so this is the proximity disambiguator at work)');

  Idx := FReader.FindBestRecordForGlobalAndField('MdtGlobal', 'Match');
  Assert.IsTrue(Idx >= 0,
    'MdtGlobal.Match must resolve');
  Assert.AreEqual('TMdt', FReader.Classes[Idx].Name,
    'MdtGlobal.Match resolved to wrong record');

  Idx := FReader.FindBestRecordForGlobalAndField('GlobalKonsAplPortal', 'Name');
  Assert.IsTrue(Idx >= 0,
    'GlobalKonsAplPortal.Name must resolve via proximity (the T-prefix ' +
    'hint does not apply -- the type is TKonsApl, not ' +
    'TGlobalKonsAplPortal)');
  Assert.AreEqual('TKonsApl', FReader.Classes[Idx].Name,
    'GlobalKonsAplPortal.Name resolved to wrong record');

  // End-to-end address-resolution check: the actual user-facing bug
  // was that GetAddressFromSymbol('AppCaps') returned the .itext VA
  // of the unit-init proc "Base.AppCaps.Base.AppCaps" instead of
  // the .bss VA of "Business.Root.AppCaps", because both share the
  // suffix "AppCaps" and the simple-name index used to pick the
  // first encountered. So `evaluate AppCaps.AnyField` read from
  // .text and returned x86 garbage. The TMapFileParser fix
  // (prefer DATA over CODE on suffix collisions) makes the
  // simple-name lookup return the .bss VA. Verify by checking
  // the resolved VA matches Business.Root.AppCaps's qualified
  // VA, not Base.AppCaps.Base.AppCaps's.
  Assert.IsNotNull(FMap,
    Format('Map fixture missing at "%s" -- the VA tie-break ' +
           'check needs it. Either restore the .map file or skip ' +
           'this test by removing TfwMapPath.', [TfwMapPath]));
  var Bss: UInt64 :=
    FMap.VAFromUnitAndProcName('Business.Root', 'AppCaps');
  var Itext: UInt64 :=
    FMap.VAFromUnitAndProcName('Base.AppCaps', 'Base.AppCaps');
  var Simple: UInt64 := FMap.VAFromSimpleName('AppCaps');
  Writeln(Format('  tfw-global: VA(Business.Root.AppCaps)=0x%x',  [Bss]));
  Writeln(Format('  tfw-global: VA(Base.AppCaps.Base.AppCaps)=0x%x', [Itext]));
  Writeln(Format('  tfw-global: VA(simple AppCaps)=0x%x',         [Simple]));
  Assert.AreEqual(Bss, Simple,
    'After the prefer-DATA tie-break, VAFromSimpleName(''AppCaps'') ' +
    'must return the Business.Root.AppCaps .bss VA, not the ' +
    'Base.AppCaps.Base.AppCaps .itext VA');
  Assert.AreNotEqual(Itext, Simple,
    'Simple-name lookup must NOT return the .itext unit-init ' +
    'proc VA -- that was the user-facing bug.');
end;

procedure TRsmTfwTests.TestTfwClassInstanceFieldResolves;
var
  M: TRsmClassMember;
begin
  if ShouldSkip then Exit;

  // --- Control: a RECORD type IS fully discovered (record-sentinel
  //     path), proving the fixture and member machinery work. TAppCaps
  //     is the same record the §6.3/§6.4 pins exercise.
  Assert.IsTrue(FReader.FindClassByName('TAppCaps') >= 0,
    'TAppCaps (record) must be discovered -- control for the gap below');

  // --- §6.16 CLOSURE PIN. TFormAd is a large VCL form class whose
  //     ~12.6 KB method/property block pushed its class trailer past the
  //     old 8 KB FindClassTrailerWithin window, so it was absent from
  //     FClasses and the dotted field walk (evaluate "Self.FAd") failed.
  //     Fixed by widening the window to 16 KB AND making the trailer scan
  //     defer to a closer same-name class-def (so a far/spurious
  //     occurrence can't steal the trailer -- that mis-anchor previously
  //     regressed TestMcpEvaluateInheritedFieldViaVmtWalk). TFormAd is now
  //     discovered with its published component fields.
  Assert.IsTrue(FReader.FindClassByName('TFormAd') >= 0,
    'TFormAd must be discovered (§6.16 fix). A -1 means the class-trailer ' +
    'window/discriminator regressed.');
  Assert.IsTrue(FReader.FindClassMember('TFormAd', 'BrwAdAp', M),
    'FindClassMember(TFormAd, BrwAdAp) must resolve -- a published component ' +
    'field, exactly what the evaluator dotted-walk needs');
  Assert.IsTrue(M.Offset > 0,
    Format('TFormAd.BrwAdAp must have a real (non-zero) offset, got %d', [M.Offset]));

  // §6.16 CLOSURE: strict-private "F"-prefixed instance fields ARE now
  // captured. TFormAd has ~496 fields; the old MaxFields=128 cap filled
  // its slots with the early published-control run and dropped every
  // later field, including FAd (the field the live evaluate("Self.FAd")
  // repro needs). Raising the cap to 2048 lets the backward scan reach
  // FAd. Pinned to the exact instance offset 0x0C5C recovered live via
  // read_memory at Tfw.Ad.Form:2274.
  Assert.IsTrue(FReader.FindClassMember('TFormAd', 'FAd', M),
    '§6.16: strict-private FAd must now be captured by the backward ' +
    'member scan (MaxFields raised from 128 to 2048).');
  Assert.AreEqual(UInt32($0C5C), M.Offset,
    Format('TFormAd.FAd must sit at instance offset 0x0C5C (recovered ' +
    'live via Self+0xC5C); got 0x%x.', [M.Offset]));

  // Leakage guard: raising the cap must NOT pull a neighbouring class's
  // fields into TFormAd. The TAdPriorityInfo helper (FPriorityInfoGUID /
  // FPriorityInfoTyp at offsets 0x4 / 0x14) sits within the 64 KB
  // backward window; AMinStartOff (previous class's TypeIdx) is what
  // keeps it out, not the field cap. If this fires, the cap raise
  // exposed a cross-class-leakage bug that AMinStartOff failed to bound.
  Assert.IsFalse(FReader.FindClassMember('TFormAd', 'FPriorityInfoGUID', M),
    '§6.16 leakage guard: FPriorityInfoGUID belongs to the TAdPriorityInfo ' +
    'helper, not TFormAd -- it must not leak in after the cap raise.');
end;

procedure TRsmTfwTests.TestTfwPointerAliasFAdBindsToTAd;
// §6.19 closure pin against TFW.rsm. The fix has three pieces:
//   1. ScanTypeRegistry captures P-prefix $2A entries (relaxed
//      first-byte filter from 'T' only to 'T' or 'P').
//   2. FAliasToTargetTypeIdx[PAd_id] = TAd.TypeIdx via the strict
//      P<X> = ^T<X> name-stripping convention.
//   3. BindPointerAliasMembersByNameConvention populates
//      Member.PointerTargetTypeIdx for TFormAd.FAd via the F-name
//      bridge (FAd has no $2C record in TFW so the $2C-driven path
//      that closes DebugTarget can't see it).
//
// If this pin fails, the message names which piece broke. The
// DebugTarget sibling
// Test.DPT.MCP.Server.TestMcpEvaluateAmbiguousMemberNameDisambiguation
// covers the $2C path with a poison-sentinel leakage guard.
var
  M       : TRsmClassMember;
  TAdIdx  : Integer;
  StructIx: Integer;
begin
  if ShouldSkip then Exit;

  TAdIdx := FReader.FindClassByName('TAd');
  Assert.IsTrue(TAdIdx >= 0,
    'TAd must be discovered as a record/class for the §6.19 binding ' +
    'to work. -1 means StructDiscoverer regressed on TFW.');

  Assert.IsTrue(FReader.FindTypeIdByName('PAd') <> 0,
    'ScanTypeRegistry must capture the PAd $2A entry (relaxed ' +
    'first-byte filter). 0 means the P-prefix gate regressed.');

  Assert.IsTrue(FReader.FindClassMember('TFormAd', 'FAd', M),
    'TFormAd.FAd must be captured (closed by §6.16). Without it ' +
    'the §6.19 bridge has nothing to bind.');
  Assert.AreNotEqual<UInt32>(0, M.PointerTargetTypeIdx,
    'TFormAd.FAd.PointerTargetTypeIdx must be populated by the ' +
    'BindPointerAliasMembersByNameConvention pass (FAd has no ' +
    '$2C record in TFW, so the $2C-driven path can''t close this).');
  StructIx := FReader.FindStructByTypeIdx(M.PointerTargetTypeIdx);
  Assert.AreEqual(TAdIdx, StructIx,
    'TFormAd.FAd.PointerTargetTypeIdx must resolve back to the ' +
    'same TAd index FindClassByName returns. A mismatch means the ' +
    'P-alias bridge picked a different record than the FClasses ' +
    'lookup.');
  Assert.AreEqual(skRecord, FReader.Classes[StructIx].Kind,
    'TFormAd.FAd''s target type must be a record (skRecord). ' +
    'Anything else means the strip-P-prepend-T convention bound ' +
    'to the wrong shape.');
end;

procedure TRsmTfwTests.TestTfwRecordFieldEnumNameConventionBindsLand;
var
  Land    : TRsmClassMember;
  Waehrung: TRsmClassMember;
  FAd     : TRsmClassMember;
  LandTypId: UInt32;
  EName   : String;
begin
  if ShouldSkip then Exit;

  LandTypId := FReader.FindTypeIdByName('TLandTyp');
  Assert.AreNotEqual<UInt32>(0, LandTypId,
    'TLandTyp must have a $2A registry id for the §6.24 bridge to bind ' +
    'against. 0 means ScanTypeRegistry regressed or the TFW build dropped ' +
    'TLandTyp.');

  // Positive: TAd.Land is bound to TLandTyp by name convention.
  Assert.IsTrue(FReader.FindClassMember('TAd', 'Land', Land),
    'TAd.Land must be discovered as a member.');
  Assert.AreEqual<UInt16>(UInt16(LandTypId), Land.PrimitiveTypeId,
    'TAd.Land.PrimitiveTypeId must equal TLandTyp''s 2-byte $2A id after ' +
    'the §6.24 name-convention bridge (Land -> TLandTyp). 0 means the pass ' +
    'did not fire; a different value means it bound the wrong enum.');
  Assert.IsTrue(FReader.IsEnumTypeId(Land.PrimitiveTypeId),
    'The bound id must be recognised as an enum (registered in the ' +
    'scope-local map by the §6.24 pass).');
  Assert.IsTrue(FReader.TryGetEnumConstantName(Land.PrimitiveTypeId, 0, EName)
    and (EName = 'ltInland'),
    'Land ordinal 0 must resolve to ltInland (got "' + EName + '").');
  Assert.IsTrue(FReader.TryGetEnumConstantName(Land.PrimitiveTypeId, 1, EName)
    and (EName = 'ltAusland'),
    'Land ordinal 1 must resolve to ltAusland (got "' + EName + '").');

  // Leakage guard 1: Land's immediate neighbour Waehrung has no
  // T<Name>/T<Name>Typ enum and MUST stay unbound -- a widening that
  // over-collects would pull it in.
  Assert.IsTrue(FReader.FindClassMember('TAd', 'Waehrung', Waehrung),
    'TAd.Waehrung must be discovered as a member.');
  Assert.AreEqual<UInt16>(0, Waehrung.PrimitiveTypeId,
    'TAd.Waehrung (no matching enum name) must stay unbound -- the §6.24 ' +
    'heuristic must not bind a field whose name has no unique $03 enum.');
  Assert.IsFalse(FReader.IsEnumTypeId(Waehrung.PrimitiveTypeId),
    'TAd.Waehrung must not be enum-typed.');

  // Leakage guard 2: the F-prefix field FAd keeps its §6.19
  // pointer-target binding; the §6.24 pass must skip F<Upper> names.
  Assert.IsTrue(FReader.FindClassMember('TFormAd', 'FAd', FAd),
    'TFormAd.FAd must be discovered (§6.16).');
  Assert.AreNotEqual<UInt32>(0, FAd.PointerTargetTypeIdx,
    'TFormAd.FAd must keep its §6.19 PointerTargetTypeIdx -- the §6.24 pass ' +
    'must not touch F-prefix fields.');
  Assert.AreEqual<UInt16>(0, FAd.PrimitiveTypeId,
    'TFormAd.FAd must not be given a primitive/enum id by the §6.24 pass.');
end;

procedure TRsmTfwTests.TestTfwSimpleRecordHeaderCoversTfwRecords;
// §6.4 PIN: the simple-shape record header (§4.13) covers every TFW
// record probed here. For each probe, the gap between PStart and the
// first valid $02-prefixed field record must equal the simple-shape
// prediction `17 + managed * 8` (Win32). The probe list spans the
// interface-scope records the §6.3 work exercised plus TAppCaps,
// which the StructDiscoverer comment historically singled out as
// "puts its first $02-field record ~500 bytes past the name". The
// diagnostic that led to this pin showed the gap is 33 bytes
// (= 17 + 2*8 for managed=2), refuting the elaborate-shape
// hypothesis.
const
  Probes: array[0..4] of String = (
    'TAppCaps',          // the alleged "elaborate-header" case
    'TKonsApl',
    'TMdt',
    'TUserKonsOutlook',
    'TKonsAplPortal');
var
  I        : Integer;
  ClassIdx : Integer;
  NameOff  : NativeInt;
  NameLen  : Byte;
  PStart   : NativeInt;
  PScan    : NativeInt;
  Found    : NativeInt;
  K        : NativeInt;
  Sc       : TRsmScanner;
  FoundFieldName : String;
  FieldNameLen   : Byte;
  ManagedB : Byte;
  SimpleHdrLen : Integer;
begin
  if ShouldSkip then Exit;
  Sc := FReader.Scanner;

  for I := Low(Probes) to High(Probes) do
  begin
    ClassIdx := FReader.FindClassByName(Probes[I]);
    Assert.IsTrue(ClassIdx >= 0,
      Format('Probe %s not discovered -- §6.3 / structural-anchor ' +
             'change regressed?', [Probes[I]]));

    NameOff := NativeInt(FReader.Classes[ClassIdx].TypeIdx);
    Assert.IsTrue((NameOff > 0) and (NameOff + Length(Probes[I]) + 8 < Sc.Sz),
      Format('Probe %s TypeIdx=0x%x out of range', [Probes[I], NameOff]));

    NameLen := Sc.ByteAt(NameOff);
    Assert.AreEqual(Byte(Length(Probes[I])), NameLen,
      Format('Probe %s NameLen byte mismatch -- TypeIdx must point at ' +
             'the length-prefixed name', [Probes[I]]));

    PStart := NameOff + 1 + NameLen + 4;
    ManagedB := Sc.ByteAt(PStart);
    SimpleHdrLen := 17 + Integer(ManagedB) * 8;

    // Locate the first $02-prefixed field record with a valid
    // typeinfo anchor, using the same walker contract as
    // ScanFieldsForwardFromRecordName.
    PScan := PStart + 4096;
    if PScan > Sc.Sz - 16 then PScan := Sc.Sz - 16;
    Found := -1;
    K := PStart;
    while K < PScan do
    begin
      if Sc.ByteAt(K) = $02 then
      begin
        FieldNameLen := Sc.ByteAt(K + 1);
        if (FieldNameLen >= 1) and (FieldNameLen <= 40) and
           Sc.ReadIdentifier(K + 1, FoundFieldName) and
           Sc.IsValidFieldTypeinfoPrefix(K + 2 + Length(FoundFieldName)) then
        begin
          Found := K;
          Break;
        end;
      end;
      Inc(K);
    end;
    Assert.IsTrue(Found >= 0,
      Format('Probe %s -- no $02 field record found within 4 KB of ' +
             'PStart=0x%x', [Probes[I], PStart]));

    Assert.AreEqual(Int64(SimpleHdrLen), Int64(Found - PStart),
      Format('Probe %s simple-shape header-length mismatch: ' +
             'expected %d (= 17 + %d * 8), actual gap %d. ' +
             'A non-matching gap means the elaborate-header ' +
             'hypothesis is back on the table -- re-open §6.4.',
        [Probes[I], SimpleHdrLen, ManagedB, Found - PStart]));

    // Cross-check: the field name the walker would discover at the
    // simple-shape offset must match what StructDiscoverer recorded
    // as Members[0]. A mismatch means our gap calculation found a
    // phantom $02 somewhere inside the header.
    Assert.IsTrue(FReader.Classes[ClassIdx].Members.Count >= 1,
      Format('Probe %s has no members -- discovery regressed?', [Probes[I]]));
    Assert.AreEqual(FReader.Classes[ClassIdx].Members[0].Name, FoundFieldName,
      Format('Probe %s -- scan-found first field "%s" does not match ' +
             'discovered Members[0]="%s"; the scan probably landed on a ' +
             'phantom $02 inside the header.',
        [Probes[I], FoundFieldName,
         FReader.Classes[ClassIdx].Members[0].Name]));
  end;
end;

procedure TRsmTfwTests.TestTfwEnumTypedFieldResolvesToPrimary;
// §6.9 PIN. See class-level docstring above for the round 6
// closure rationale.
const
  // The probe the round-4 RE pinpointed. Expected primary id is
  // read from the $2A body's bytes +3..+4 (little-endian word):
  //   TUserKonsOutlookDirection @ $A8E658B -> primary $7B7F
  ExpectedSyncDirectionPrimary: UInt16 = $7B7F;
var
  ClsIdx   : Integer;
  Info     : TRsmClassInfo;
  M        : Integer;
  Member   : TRsmClassMember;
  Found    : Boolean;
  SyncStat : TRsmClassMember;
  HasSyncSt: Boolean;
begin
  if ShouldSkip then Exit;

  ClsIdx := FReader.FindClassByName('TUserKonsOutlook');
  Assert.IsTrue(ClsIdx >= 0,
    'TUserKonsOutlook must be discovered as a class -- structural ' +
    'anchor regressed?');
  Info := FReader.Classes[ClsIdx];

  // Diagnostic: print every member's PrimitiveTypeId so a future
  // regression has the round-6 baseline values in the runner log.
  Writeln(Format('  §6.9: TUserKonsOutlook members=%d', [Info.Members.Count]));
  for M := 0 to Info.Members.Count - 1 do
  begin
    Member := Info.Members[M];
    Writeln(Format('    [%d] %-24s off=%d typeIdx=0x%x prim=0x%x',
      [M, Member.Name, Member.Offset, Member.TypeIdx,
       Integer(Member.PrimitiveTypeId)]));
  end;

  Found := False;
  Member := Default(TRsmClassMember);
  HasSyncSt := False;
  SyncStat := Default(TRsmClassMember);
  for M := 0 to Info.Members.Count - 1 do
  begin
    if SameText(Info.Members[M].Name, 'SyncDirection') then
    begin
      Member := Info.Members[M];
      Found := True;
    end
    else if SameText(Info.Members[M].Name, 'SyncStatus') then
    begin
      SyncStat := Info.Members[M];
      HasSyncSt := True;
    end;
  end;
  Assert.IsTrue(Found,
    'TUserKonsOutlook.SyncDirection must be present in Members -- ' +
    '§6.3 / structural-anchor regressed?');

  // Core pin: PrimitiveTypeId must equal the cross-unit enum's
  // PRIMARY ($7B7F), not the bogus 2-byte word read of the body's
  // FieldId slot (the round-5 mis-routes gave $E5FF / $FFF6 here,
  // the original 2-byte read gave $0D2A which doesn't match any
  // real type).
  Assert.AreEqual(UInt32(ExpectedSyncDirectionPrimary), UInt32(Member.PrimitiveTypeId),
    Format('SyncDirection.PrimitiveTypeId must be $%x ' +
           '(TUserKonsOutlookDirection''s primary) after the §6.9 ' +
           'nearest-$2A-offset bridge fired. Got $%x -- either the ' +
           'bridge didn''t fire (still reading the body''s +3..+4 ' +
           'word as a 2-byte primary), or the per-unit scoping ' +
           'collapsed onto a wrong same-LOW-byte enum.',
      [Integer(ExpectedSyncDirectionPrimary),
       Integer(Member.PrimitiveTypeId)]));

  // SyncStatus parallel probe: the second enum-typed field in
  // TUserKonsOutlook (TUserKonsOutlookSyncStatus, $2A body near
  // $A8E65BC per §6.9 doc). The pre-round-6 2-byte read produced
  // $0D2C (the spurious FieldId). After the bridge: a non-zero
  // primary that the EnumDecoder accepted. Without ground truth
  // for the exact primary we only assert non-zero AND not equal
  // to the FieldId-fabrication value -- a regression that
  // re-routes the resolver to the 2-byte read would trip both
  // arms here.
  if HasSyncSt then
  begin
    Assert.IsTrue(SyncStat.PrimitiveTypeId <> 0,
      'SyncStatus.PrimitiveTypeId must be non-zero after the §6.9 ' +
      'bridge resolves it. Zero means neither the bridge nor the ' +
      '2-byte-read fallback produced an id (corrupt body?).');
    Assert.AreNotEqual(UInt32($0D2C), UInt32(SyncStat.PrimitiveTypeId),
      'SyncStatus.PrimitiveTypeId must not equal $0D2C (the pre-' +
      'round-6 2-byte-read fabrication of the FieldId from the ' +
      'body''s +3..+4 word). Hitting $0D2C means the bridge regressed.');
    Writeln(Format('  §6.9: SyncStatus.PrimitiveTypeId=$%x (bridge-resolved)',
      [Integer(SyncStat.PrimitiveTypeId)]));
  end;
end;

{$IFDEF CPUX64}
procedure TRsmTfwTests.TestTfwWin64ProcAddressDecodesViaA0Narrow;
// §6.7 PIN: probes whose $28 record uses the $A0 byte-7=$03
// (narrow / 21-bit) sub-form must decode to the right SegmentOffset
// after TryWin64A0's Variant B path. The three probes here all have
// .map RVA < 2 MB and previously decoded to 0.
const
  Probes: array[0..2] of String = (
    'TStringList.Add',
    'TObject.Create',
    'IntToStr');
  ProbeUnits: array[0..2] of String = (
    'System.Classes',
    'System',
    'System.SysUtils');
var
  I        : Integer;
  ProcIdx  : Integer;
  DecodedSO: NativeUInt;
  MapSegOff: UInt64;
begin
  if ShouldSkipWin64 then Exit;
  Assert.IsNotNull(FMapWin64,
    Format('Win64 .map missing at "%s" -- the §6.7 pin needs ' +
           'ground-truth RVAs to compare against.', [TfwWin64MapPath]));

  for I := Low(Probes) to High(Probes) do
  begin
    MapSegOff := FMapWin64.VAFromUnitAndProcName(ProbeUnits[I], Probes[I]);
    Assert.IsTrue(MapSegOff > 0,
      Format('Probe %s.%s missing from Win64 .map -- pick another ' +
             'probe with the $A0 byte-7=$03 form.',
        [ProbeUnits[I], Probes[I]]));
    Assert.IsTrue(MapSegOff < $200000,
      Format('Probe %s map-RVA $%x is above the 2 MB cap; the §6.7 ' +
             'narrow form decodes only the low 21 bits, so probes ' +
             'must stay below 2 MB.', [Probes[I], MapSegOff]));

    ProcIdx := FReaderWin64.FindProcByName(Probes[I]);
    Assert.IsTrue(ProcIdx >= 0,
      Format('FindProcByName(%s) returned -1 on the Win64 reader.',
        [Probes[I]]));
    DecodedSO := FReaderWin64.Procs[ProcIdx].SegmentOffset;
    Assert.AreEqual(MapSegOff, UInt64(DecodedSO),
      Format('Proc %s SegmentOffset mismatch: decoded=$%x, .map=$%x. ' +
             'A zero decoded means TryWin64A0 Variant B regressed; a ' +
             'non-matching non-zero means the 21-bit packing changed.',
        [Probes[I], Integer(DecodedSO), MapSegOff]));
  end;
end;

procedure TRsmTfwTests.TestTfwWin64ProcAddressDecodesAboveCap;
// §6.2 PIN: probes with .map RVA well above 2 MB must decode to
// exactly the right SegmentOffset after the TryWin64A0 path replaces
// the unconditional TryWin32 routing for the $A0 sub-tag.
const
  Probes: array[0..3] of String = (
    'TFormMain.Create',
    'TFormMain.AfterMenuRebuild',
    'TFormVBh.Create',
    'TFormVBh.CreateGsVBhBridge');
  ProbeUnits: array[0..3] of String = (
    'Tfw.Main.Form',
    'Tfw.Main.Form',
    'Tfw.Vbh.Form',
    'Tfw.Vbh.Form');
var
  I        : Integer;
  ProcIdx  : Integer;
  DecodedSO: NativeUInt;
  MapSegOff: UInt64;
begin
  if ShouldSkipWin64 then Exit;

  // Sanity: the arch flag must be set or the dispatcher will still
  // route $A0 to TryWin32 and every probe below will be off by
  // exactly $400000.
  Assert.IsTrue(FReaderWin64.Scanner.Is64Bit,
    'FReaderWin64.Scanner.Is64Bit must be True after loading a Win64 ' +
    '.exe; arch detection failed (PE-header read in ' +
    'TRsmScanner.LoadFromFile / DetectIs64BitExe regressed?).');

  Assert.IsNotNull(FMapWin64,
    Format('Win64 .map missing at "%s" -- the §6.2 pin needs ' +
           'ground-truth RVAs to compare against.', [TfwWin64MapPath]));

  for I := Low(Probes) to High(Probes) do
  begin
    MapSegOff := FMapWin64.VAFromUnitAndProcName(ProbeUnits[I], Probes[I]);
    Assert.IsTrue(MapSegOff > $200000,
      Format('Probe %s.%s map-reported RVA $%x is at or below 2 MB; ' +
             'the §6.2 pin needs probes ABOVE the historical cap to ' +
             'be meaningful -- pick a higher-RVA proc.',
        [ProbeUnits[I], Probes[I], MapSegOff]));

    ProcIdx := FReaderWin64.FindProcByName(Probes[I]);
    Assert.IsTrue(ProcIdx >= 0,
      Format('FindProcByName(%s) returned -1; the proc is not in ' +
             'FReaderWin64.Procs.', [Probes[I]]));
    DecodedSO := FReaderWin64.Procs[ProcIdx].SegmentOffset;
    Assert.AreEqual(MapSegOff, UInt64(DecodedSO),
      Format('Proc %s SegmentOffset mismatch: decoded=$%x, .map=$%x, ' +
             'delta=$%x. A non-zero delta of exactly $400000 means the ' +
             '$A0 dispatcher fell back to TryWin32 -- check ' +
             'TRsmScanner.FIs64Bit propagation.',
        [Probes[I], Integer(DecodedSO), MapSegOff,
         Int64(MapSegOff) - Int64(DecodedSO)]));
  end;
end;
{$ENDIF}

procedure TRsmTfwTests.TestTfwUnitUseTableContainsTLandTyp;
// §4.17 / §6.21 closure pin against TFW.rsm. The new $64-anchored
// decoder must surface at least one $66 'TLandTyp' reference with
// the canonical little-endian LinkToken $7DA69008 (bytes 08 90 A6 7D,
// the canonical $2A 'TLandTyp' registry entry body at +3..+6 per
// §6.20-Round-3). This payload is an OPAQUE LINKER TOKEN, not an RVA
// (§6.29 side-item): $7DA69008 ≈ 2.1 GB far exceeds the TFW image, so
// it cannot be an offset into it. The Round-3 PS scan counted 68
// occurrences across TFW; a regression past ~60 means the decoder
// either bailed out of the segment walk too early or the structural
// anchor stopped matching for entire ranges of the corpus.
const
  ExpectedToken: UInt32 = $7DA69008;
  MinCount     : Integer = 60;
var
  Segments: IList<TRsmUnitUseSegment>;
  I, J    : Integer;
  Seg     : TRsmUnitUseSegment;
  Ref     : TRsmUnitUseRef;
  Hits    : Integer;
  SawCorrectToken: Boolean;
  AnyTLandTypToken: UInt32;
begin
  if ShouldSkip then Exit;
  Segments := FReader.UnitUseSegments;
  Assert.IsTrue(Segments.Count > 0,
    'UnitUseSegments empty on TFW.rsm -- the decoder never opened ' +
    'a single segment.');
  Hits := 0;
  SawCorrectToken := False;
  AnyTLandTypToken := 0;
  for I := 0 to Segments.Count - 1 do
  begin
    Seg := Segments[I];
    if Seg.Refs = nil then Continue;
    for J := 0 to Seg.Refs.Count - 1 do
    begin
      Ref := Seg.Refs[J];
      if (Ref.Kind = uukType) and (Ref.Name = 'TLandTyp') then
      begin
        Inc(Hits);
        AnyTLandTypToken := Ref.LinkToken;
        if Ref.LinkToken = ExpectedToken then
          SawCorrectToken := True;
      end;
    end;
  end;
  Assert.IsTrue(Hits >= MinCount,
    Format('Expected at least %d $66 ''TLandTyp'' references in TFW; ' +
           'got %d. Either the segment walk stops too early, or the ' +
           'TFW build dropped its TLandTyp imports.',
           [MinCount, Hits]));
  Assert.IsTrue(SawCorrectToken,
    Format('No $66 ''TLandTyp'' reference carries the canonical LinkToken ' +
           '$%x. A sample TLandTyp token I did see was $%x. Either the ' +
           '4-byte payload is being read at the wrong offset, or the ' +
           'canonical $2A ''TLandTyp'' moved on this TFW build (in ' +
           'which case the §6.21 spec needs updating).',
           [ExpectedToken, AnyTLandTypToken]));
  Assert.IsTrue(AnyTLandTypToken > UInt32($10000000),
    Format('The $66 ''TLandTyp'' payload ($%x) must exceed any in-image ' +
           'RVA ceiling -- it is an opaque linker token, NOT an RVA ' +
           '(§6.29 side-item closure).', [AnyTLandTypToken]));
end;

procedure TRsmTfwTests.TestTfwUnitUseTableLtInlandTokenMatchesCanonicalBlock;
// §4.17 / §6.21 cross-validation pin + §6.29-side-item not-RVA proof.
// Three assertions, layered so the pin survives offset drift between
// TFW rebuilds while still guarding the structural finding:
//   (1) At least one decoded $67 'ltInland' entry carries the
//       canonical-shape payload bytes "?? 39 A4 10" -- the
//       conserved tail §6.20-Round-3 observed across the +3 LSB
//       stride family.
//   (2) The canonical $25 'ltInland' record is locatable in the
//       byte stream and ITS body carries those same 4 bytes.
//   (3) The $67 payload and the $25 body payload agree
//       byte-for-byte. This is the decisive proof that the payload
//       is the §4.6.2 opaque $25 LINKER TOKEN, NOT an image RVA: the
//       $67 use-site reproduces the canonical $25 declaration's token
//       verbatim, and an address could not be shared like that across
//       every use-site of the symbol.
// The conserved tail "39 A4 10" plus the dynamic discovery of the
// canonical $25 block makes the pin survive linker rebuild drift
// (offsets shift; the structural relationship is what we want to
// pin, not the absolute file offset).
const
  // Bytes for the canonical $25 'ltInland' record header on the wire:
  // $25 $08 'l' 't' 'I' 'n' 'l' 'a' 'n' 'd'
  NEEDLE: array[0..9] of Byte = ($25, $08, $6C, $74, $49, $6E, $6C,
                                 $61, $6E, $64);
var
  Segments  : IList<TRsmUnitUseSegment>;
  Sc        : TRsmScanner;
  I, J      : Integer;
  Seg       : TRsmUnitUseSegment;
  Ref         : TRsmUnitUseRef;
  DecodedToken: UInt32;
  CanonToken  : UInt32;
  DecodedHit  : Boolean;
  CanonOff  : NativeInt;
  Found25Off: NativeInt;
  Cursor    : NativeInt;
begin
  if ShouldSkip then Exit;
  Sc := FReader.Scanner;
  Segments := FReader.UnitUseSegments;
  Assert.IsTrue(Segments.Count > 0,
    'UnitUseSegments empty on TFW.rsm -- the decoder never opened ' +
    'a single segment.');

  // (1) Find any decoded $67 'ltInland' entry and capture its payload.
  // §6.20-Round-3 documents the canonical bytes "81 39 A4 10" =
  // $10A43981; the conserved tail "39 A4 10" is the structural part.
  DecodedHit := False;
  DecodedToken := 0;
  for I := 0 to Segments.Count - 1 do
  begin
    Seg := Segments[I];
    if Seg.Refs = nil then Continue;
    for J := 0 to Seg.Refs.Count - 1 do
    begin
      Ref := Seg.Refs[J];
      if (Ref.Kind = uukSymbol) and (Ref.Name = 'ltInland') then
      begin
        DecodedToken := Ref.LinkToken;
        DecodedHit := True;
        Break;
      end;
    end;
    if DecodedHit then Break;
  end;
  Assert.IsTrue(DecodedHit,
    '$67 ''ltInland'' was never decoded -- the §6.20-Round-3 35 ' +
    'use-sites should each surface here.');
  // Assert the conserved tail "39 A4 10" sits in the top 24 bits of
  // the little-endian token. The LSB byte is per-ordinal (81 / 84 / 87
  // / 8A / 8D for ltInland / ltAusland / ... in §6.20-Round-3's
  // recorded snapshot); we don't pin that here because the absolute
  // token can drift with the build.
  Assert.AreEqual<UInt32>(UInt32($10A43900), DecodedToken and UInt32($FFFFFF00),
    Format('Decoded $67 ltInland token $%x lacks the conserved tail ' +
           '"39 A4 10" (high 24 bits). Either the structural finding ' +
           'is broken on this TFW build or the 4-byte payload read ' +
           'mis-aligned.', [DecodedToken]));

  // (2) Locate the canonical $25 'ltInland' record by scanning for
  // its on-wire 10-byte header. We only need the FIRST occurrence
  // (§6.20-Round-3 observed all 36 ltInland strings; only the FIRST
  // is the canonical $25 block, the rest are $67 use-sites).
  Found25Off := -1;
  Cursor := 0;
  while Cursor < Sc.Sz - SizeOf(NEEDLE) do
  begin
    if (Sc.ByteAt(Cursor) = NEEDLE[0]) and
       (Sc.ByteAt(Cursor + 1) = NEEDLE[1]) and
       (Sc.ByteAt(Cursor + 2) = NEEDLE[2]) then
    begin
      var MatchK: Integer := 3;
      var Ok: Boolean := True;
      while (MatchK < Length(NEEDLE)) and Ok do
      begin
        if Sc.ByteAt(Cursor + MatchK) <> NEEDLE[MatchK] then
          Ok := False
        else
          Inc(MatchK);
      end;
      if Ok then
      begin
        Found25Off := Cursor;
        Break;
      end;
    end;
    Inc(Cursor);
  end;
  Assert.IsTrue(Found25Off >= 0,
    'Canonical $25 ''ltInland'' block not found via byte-needle ' +
    'scan -- if this regresses, TFW no longer carries the enum.');

  // The canonical block's token payload sits at the first non-zero
  // little-endian DWORD starting at +10 (the byte after the name).
  // §6.20-Round-3 captured shape:
  //   $25 $08 'ltInland' <8A 00 00> <81 39 A4 10> ...
  // Modern TFW (this build) carries:
  //   $25 $08 'ltInland' <8A 00 00> <81 39 A4 10> ...
  // -- the body's first 3 bytes are an enum-private flag/size triple
  // and the payload starts at +13. We don't hard-code +13 because
  // that's the slot the brief's offsets diverged on; instead we
  // search for the "39 A4 10" tail within a 24-byte window past the
  // name and pin the discovered DWORD.
  CanonOff := -1;
  for var Probe := Found25Off + 10 to Found25Off + 10 + 16 do
  begin
    if Probe + 4 > Sc.Sz then Break;
    if (Sc.ByteAt(Probe + 1) = $39) and
       (Sc.ByteAt(Probe + 2) = $A4) and
       (Sc.ByteAt(Probe + 3) = $10) then
    begin
      CanonOff := Probe;
      Break;
    end;
  end;
  Assert.IsTrue(CanonOff >= 0,
    Format('Canonical $25 ''ltInland'' block at offset %d does not ' +
           'carry a 4-byte payload ending in "39 A4 10" within +10..+26 ' +
           'of the block start. The $67-to-$25 structural relationship ' +
           '§6.21 documents is broken or the linker reshaped the ' +
           'canonical body.', [Found25Off]));
  CanonToken := UInt32(Sc.ByteAt(CanonOff))            or
              (UInt32(Sc.ByteAt(CanonOff + 1)) shl 8)  or
              (UInt32(Sc.ByteAt(CanonOff + 2)) shl 16) or
              (UInt32(Sc.ByteAt(CanonOff + 3)) shl 24);

  // (3) Decoded $67 payload must EQUAL the canonical $25 body's
  // payload byte-for-byte. This is what proves the $67 entry carries
  // the canonical declaration's §4.6.2 linker TOKEN (not an RVA, and
  // not some other slot).
  Assert.AreEqual<UInt32>(CanonToken, DecodedToken,
    Format('Decoded $67 ltInland token $%x does not match the canonical ' +
           '$25 ltInland body token $%x at file offset %d. The §6.21 ' +
           'spec''s structural mapping between $67 use-sites and the ' +
           '$25 canonical block requires byte-identity here.',
           [DecodedToken, CanonToken, CanonOff]));
end;

procedure TRsmTfwTests.TestTfwSelfTypeIdxIsPerProcRefNotRegistryId;

  function SelfIdxOf(const AProcName: String): UInt32;
  var
    PIdx, I: Integer;
    Proc   : TRsmProc;
  begin
    Result := 0;
    PIdx := FReader.FindProcByName(AProcName);
    Assert.IsTrue(PIdx >= 0, AProcName + ' not found in TFW procs');
    Proc := FReader.Procs[PIdx];
    for I := 0 to Proc.Locals.Count - 1 do
      if (Proc.Locals[I].Kind = lkRegister) and
         SameText(Proc.Locals[I].Name, 'Self') then
        Exit(Proc.Locals[I].TypeIdx);
    Assert.Fail(AProcName + ' has no Self register-param ($21 REGVAR)');
  end;

var
  SelfId   : UInt32;
  Primary  : UInt32;
  MisIdx   : Integer;
  ClassIdx : Integer;
begin
  if ShouldSkip then Exit;

  SelfId := SelfIdxOf('TFormMain.Create');
  Primary := FReader.FindTypeIdByName('TFormMain');
  ClassIdx := FReader.FindClassByName('TFormMain');

  Assert.IsTrue(Primary <> 0,
    'TFormMain must have a $2A registry primary id');
  Assert.IsTrue(ClassIdx >= 0,
    'TFormMain must be a discovered class (proc-name-split reliable path)');

  // Gap regime: the per-proc ref the slot carries is NOT the registry id.
  Assert.IsTrue(SelfId <> Primary,
    Format('Gap regime: TFormMain.Create Self.TypeIdx ($%x) must NOT equal ' +
           'TFormMain''s $2A primary ($%x) on the large TFW binary -- the ' +
           'slot carries a per-proc ref, not the registry id. If they now ' +
           'match, the large-binary encoding changed; revisit §6.27.',
           [SelfId, Primary]));

  // The bug §6.27 documents: routing the per-proc ref through the
  // registry lands on an UNRELATED class, never TFormMain.
  MisIdx := FReader.FindClassIdxByRsmTypeId(SelfId);
  Assert.IsTrue(MisIdx <> ClassIdx,
    Format('FindClassIdxByRsmTypeId(Self.TypeIdx=$%x) must NOT resolve to ' +
           'the TFormMain class (idx %d); on TFW it mis-resolves to whatever ' +
           'type last registered the truncated 1-byte id. This is the §6.27 ' +
           'mis-resolution the consumer must avoid for class instances.',
           [SelfId, ClassIdx]));

  // Type-stable within the class: a second TFormMain method agrees.
  Assert.AreEqual<UInt32>(SelfId, SelfIdxOf('TFormMain.AfterMenuRebuild'),
    'TFormMain.Create and TFormMain.AfterMenuRebuild must carry the same ' +
    'Self per-proc ref (the ref is per-(unit,type), stable within a class).');
end;

procedure TRsmTfwTests.TestTfwPerProcRefIsNotUnitUseTableIndex;

  // TypeIdx of a named register-param/local in a named proc.
  function ParamTypeIdx(const AProc, AParam: String): UInt32;
  var
    PIdx, I: Integer;
    Proc   : TRsmProc;
  begin
    Result := 0;
    PIdx := FReader.FindProcByName(AProc);
    Assert.IsTrue(PIdx >= 0, AProc + ' not found in TFW procs');
    Proc := FReader.Procs[PIdx];
    for I := 0 to Proc.Locals.Count - 1 do
      if SameText(Proc.Locals[I].Name, AParam) then
        Exit(Proc.Locals[I].TypeIdx);
    Assert.Fail(Format('%s has no param/local named %s', [AProc, AParam]));
  end;

  // Concatenated 0-based index of the first uukType ref named ATypeName
  // among all unit-use segments whose IMPORTING unit is AUnit; -1 if
  // absent (concatenated across the unit's segments in stream order --
  // the natural "table index" Hyp D would require).
  function UnitUseTypeIndex(const AUnit, ATypeName: String): Integer;
  var
    Segs: IList<TRsmUnitUseSegment>;
    I, J: Integer;
    Seg : TRsmUnitUseSegment;
    SrcU: String;
    Ctr : Integer;
  begin
    Result := -1;
    Ctr := 0;
    Segs := FReader.UnitUseSegments;
    for I := 0 to Segs.Count - 1 do
    begin
      Seg := Segs[I];
      if Seg.Refs = nil then Continue;
      SrcU := '';
      if (Seg.SourceFileIdx >= 0) and
         (Seg.SourceFileIdx < FReader.SourceFiles.Count) then
        SrcU := FReader.SourceFiles[Seg.SourceFileIdx].UnitName;
      if not SameText(SrcU, AUnit) then Continue;
      for J := 0 to Seg.Refs.Count - 1 do
        if Seg.Refs[J].Kind = uukType then
        begin
          if SameText(Seg.Refs[J].Name, ATypeName) then Exit(Ctr);
          Inc(Ctr);
        end;
    end;
  end;

var
  AOwnerMain, AOwnerVBh, SelfMain: UInt32;
  TComponentIdx, TFormMainIdx    : Integer;
begin
  if ShouldSkip then Exit;

  AOwnerMain := ParamTypeIdx('TFormMain.Create', 'AOwner');
  AOwnerVBh  := ParamTypeIdx('TFormVBh.Create', 'AOwner');
  SelfMain   := ParamTypeIdx('TFormMain.Create', 'Self');

  // (1) Same type (TComponent) in both procs, yet the per-proc ref
  // differs -> it is not any stable (global/per-type) identifier.
  Assert.AreNotEqual<UInt32>(AOwnerMain, AOwnerVBh,
    Format('AOwner is TComponent in BOTH TFormMain.Create and ' +
           'TFormVBh.Create, yet its per-proc ref differs ($%x vs $%x). ' +
           'A unit-use/type table index would be stable for the same ' +
           'type; differing values confirm a per-proc number (Hyp D ' +
           'refuted).', [AOwnerMain, AOwnerVBh]));

  // (2) TComponent IS an import of Tfw.Main.Form (so it has a unit-use
  // index), but that index is NOT AOwner's ref -> the ref is not the
  // §4.17 table index for the param's type.
  TComponentIdx := UnitUseTypeIndex('Tfw.Main.Form', 'TComponent');
  Assert.IsTrue(TComponentIdx >= 0,
    'TComponent must appear in Tfw.Main.Form''s §4.17 unit-use type refs ' +
    '(it is an import of that unit) for this refutation to be meaningful.');
  Assert.AreNotEqual<Integer>(TComponentIdx, Integer(AOwnerMain),
    Format('Hyp D refuted: AOwner''s per-proc ref ($%x = %d) does NOT ' +
           'equal TComponent''s index (%d) in Tfw.Main.Form''s unit-use ' +
           'type refs.', [AOwnerMain, AOwnerMain, TComponentIdx]));

  // (3) TFormMain (Self's type) is NOT an import of its own unit, so it
  // has no unit-use index at all -- yet Self carries a non-zero ref.
  // The ref therefore cannot be sourced from the import table; it comes
  // from the un-decoded per-proc local type table (the design limit).
  TFormMainIdx := UnitUseTypeIndex('Tfw.Main.Form', 'TFormMain');
  Assert.AreEqual<Integer>(-1, TFormMainIdx,
    'TFormMain must NOT appear in its own unit''s (Tfw.Main.Form) ' +
    'unit-use import refs -- it is the unit''s own type. If it does, the ' +
    '§4.17 decode changed; revisit this refutation.');
  Assert.AreNotEqual<UInt32>(0, SelfMain,
    Format('TFormMain.Create Self carries a non-zero per-proc ref ($%x) ' +
           'even though TFormMain is absent from the unit-use table -- so ' +
           'the ref is sourced from the un-decoded per-proc local type ' +
           'table, not the §4.17 import table (Hyp D refuted).', [SelfMain]));
end;

procedure TRsmTfwTests.TestTfwSynthEnumPhantomResidual;
const
  Dropped: array[0..4] of String = (
    'QOS_OBJECT_HDR', 'tagXFORM', 'IRichChunk', 'NET_STRING',
    'DXGI_COLOR_SPACE_TYPE');

  function EnumDefIndex(const AName: String): Integer;
  var
    Defs: IList<TRsmEnumDef>;
    I   : Integer;
  begin
    Result := -1;
    Defs := FReader.EnumDefs;
    for I := 0 to Defs.Count - 1 do
      if SameText(Defs[I].TypeName, AName) then Exit(I);
  end;

  function ClassKind(const AName: String; out AFound: Boolean): TRsmStructKind;
  var
    Cx: Integer;
  begin
    Result := skClass;
    Cx := FReader.FindClassByName(AName);
    AFound := Cx >= 0;
    if AFound then Result := FReader.Classes[Cx].Kind;
  end;

  // Mirror of Reader.pas IsNonEnumTypeName -- the conventions condition
  // (c) drops. Used both to assert the dropped names are gone AND that
  // NO survivor still matches (the pass left none behind).
  function MatchesDroppedConvention(const AName: String): Boolean;
  var
    K        : Integer;
    AllCaps  : Boolean;
    HasLetter: Boolean;
  begin
    Result := False;
    if Length(AName) < 2 then Exit;
    if (AName[1] = 'T') and CharInSet(AName[2], ['A'..'Z']) then Exit;
    if (AName[1] = 'I') and CharInSet(AName[2], ['A'..'Z']) then Exit(True);
    if (AName[1] = 'P') and CharInSet(AName[2], ['A'..'Z']) then Exit(True);
    if AName[1] = '_' then Exit(True);
    if (Length(AName) >= 4) and (AName[1] = 't') and (AName[2] = 'a') and
       (AName[3] = 'g') and CharInSet(AName[4], ['A'..'Z']) then Exit(True);
    if Length(AName) >= 4 then
    begin
      AllCaps := True;
      HasLetter := False;
      for K := 1 to Length(AName) do
        if CharInSet(AName[K], ['A'..'Z']) then HasLetter := True
        else if not CharInSet(AName[K], ['0'..'9', '_']) then
        begin AllCaps := False; Break; end;
      if AllCaps and HasLetter then Exit(True);
    end;
  end;

var
  Defs   : IList<TRsmEnumDef>;
  I      : Integer;
  Zug    : Integer;
  Found  : Boolean;
  Kind   : TRsmStructKind;
  Leaked : String;
begin
  if ShouldSkip then Exit;
  Defs := FReader.EnumDefs;

  // (1) The representative phantoms are DROPPED -- no longer EnumDefs.
  for I := Low(Dropped) to High(Dropped) do
    Assert.AreEqual<Integer>(-1, EnumDefIndex(Dropped[I]),
      Format('§6.25 closure: phantom EnumDef "%s" must be dropped by the ' +
             'naming-convention pass (condition (c)). Still present => the ' +
             'drop regressed or the convention no longer matches it.',
             [Dropped[I]]));

  // (2) No SURVIVING synthesised EnumDef still matches a dropped
  // convention -- the pass left none behind. (Real $03 enums are never
  // synthesised, and T<Upper> names are protected, so any match here is
  // a phantom the pass should have removed.)
  Leaked := '';
  for I := 0 to Defs.Count - 1 do
    if Defs[I].Synthesized and MatchesDroppedConvention(Defs[I].TypeName) then
    begin
      Leaked := Defs[I].TypeName;
      Break;
    end;
  Assert.AreEqual('', Leaked,
    Format('§6.25 closure: a synthesised EnumDef ("%s") still matches a ' +
           'dropped naming convention -- the (c) pass missed it.', [Leaked]));

  // (3) LEAKAGE GUARDS -- genuine enums must survive.
  // TPictureFormat: a plain T-prefixed sparse enum.
  Assert.IsTrue(EnumDefIndex('TPictureFormat') >= 0,
    'Leakage guard: genuine sparse enum TPictureFormat (pfBMP/pfGIF/...) ' +
    'must survive -- the T<Upper> protection failed if it is gone.');
  // TZO: all-caps T-prefixed real enum (zoNone/zoBilanz/zoGuV) -- the
  // scariest case, caught by the ALL-CAPS rule WITHOUT the T<Upper>
  // guard. Its survival proves the guard order is correct.
  Assert.IsTrue(EnumDefIndex('TZO') >= 0,
    'Leakage guard: all-caps T-prefixed enum TZO must survive -- if it is ' +
    'gone, the ALL-CAPS rule fired before the T<Upper> protection.');

  // (4) skRecord kind-oracle stays UNSAFE: TZUGFeRDXMLObjectTyp is a
  // genuine large enum ALSO registered skRecord. It survives because (c)
  // protects T<Upper> -- this is why filter (a) is skClass-only (the
  // stale TThreadPriority example is refuted below).
  Zug := EnumDefIndex('TZUGFeRDXMLObjectTyp');
  Assert.IsTrue(Zug >= 0,
    'Leakage guard: genuine enum TZUGFeRDXMLObjectTyp must survive ' +
    '(T<Upper>-protected) despite also being skRecord.');
  Assert.IsTrue((Defs[Zug].Elements <> nil) and (Defs[Zug].Elements.Count >= 50),
    'TZUGFeRDXMLObjectTyp must carry its large enum element set (was 131).');
  Kind := ClassKind('TZUGFeRDXMLObjectTyp', Found);
  Assert.IsTrue(Found and (Kind = skRecord),
    'TZUGFeRDXMLObjectTyp must ALSO be skRecord in FClasses -- the dual ' +
    'registration that makes a skRecord-based phantom drop unsafe.');
  Kind := ClassKind('TThreadPriority', Found);
  Assert.IsFalse(Found and (Kind = skRecord),
    'TThreadPriority must NOT be skRecord on current TFW (the old ' +
    'FilterPhantomEnumDefs comment example is stale).');

  // (5) §6.25 R2 / §4.18 closure: a synthesised EnumDef's UnitName now
  // comes from the `$70` source-file introducer (FCurrentSourceFileIdx),
  // never the retired 1 MB name-search that could grab a class method.
  // INVARIANT: no surviving synth EnumDef has a "TClass.Method" unit
  // (first dotted segment a T/E/I+Upper type ident). Under the old
  // name-search this FAILED on ≥2 TFW enums (TNumColorsRange ->
  // "TPlannerColorArrayList.Add", TIdOnTLSNegotiationFailure ->
  // "TIdExplicitTLSServer.InitComponent"); the `$70` anchor gives their
  // real units (observed PlanObj / IdExplicitTLSClientServerBase) and the
  // C-prefix method overshoots the old T/E/I filter couldn't catch
  // (TZUGFeRDXMLObjectTyp -> "CZUGFeRD….GetSequence" -> Base.Xsd.ZUGFeRD.Types).
  Leaked := '';
  for I := 0 to Defs.Count - 1 do
    if Defs[I].Synthesized then
    begin
      var U: String := Defs[I].UnitName;
      var D: Integer := Pos('.', U);
      if D > 1 then
      begin
        var Seg: String := Copy(U, 1, D - 1);
        if (Length(Seg) >= 2) and CharInSet(Seg[1], ['T', 'E', 'I']) and
           CharInSet(Seg[2], ['A'..'Z']) then
        begin Leaked := Defs[I].TypeName + ' -> ' + U; Break; end;
      end;
    end;
  Assert.AreEqual('', Leaked,
    Format('§6.25 R2: synthesised EnumDef has a TClass.Method UnitName ' +
           '("%s") -- the $70 introducer source regressed to the old ' +
           'name-search heuristic.', [Leaked]));
end;

procedure TRsmTfwTests.TestTfwEnumDefNoSameUnitSynthDuplicate;
// §6.25 closure (d). A synthesised EnumDef that shares its (TypeName,
// UnitName) with a $03-sourced def is the SAME Delphi type and must be
// dropped (the $03 is authoritative). The diagnostic this replaced found
// 24 such same-unit synth duplicates on TFW (TLngTyp, TPictureFormat, ...,
// incl. zero-element-overlap pollution like TCalFirstWeekMode) plus ONE
// cross-unit homonym (TDataType: synth in VCLTee.TeeSpline, $03 in bmpfilt)
// that must SURVIVE.
var
  Defs       : IList<TRsmEnumDef>;
  Names03    : IKeyValue<String, Boolean>;  // lc name           -> $03 exists
  Names03Unit: IKeyValue<String, Boolean>;  // "lc name|lc unit"  -> $03 exists
  I          : Integer;
  Violation  : String;
  CrossUnitKept: Integer;
begin
  if ShouldSkip then Exit;
  Defs := FReader.EnumDefs;
  Names03     := Collections.NewPlainKeyValue<String, Boolean>;
  Names03Unit := Collections.NewPlainKeyValue<String, Boolean>;
  for I := 0 to Defs.Count - 1 do
    if not Defs[I].Synthesized then
    begin
      Names03[LowerCase(Defs[I].TypeName)] := True;
      Names03Unit[LowerCase(Defs[I].TypeName) + '|' +
                  LowerCase(Defs[I].UnitName)] := True;
    end;

  // (1) Invariant: no surviving SYNTHESISED def shares (name, unit) with a
  // $03-sourced def. (Before condition (d): 24 violations on TFW.)
  Violation := '';
  CrossUnitKept := 0;
  for I := 0 to Defs.Count - 1 do
    if Defs[I].Synthesized then
    begin
      if Names03Unit.ContainsKey(LowerCase(Defs[I].TypeName) + '|' +
                                 LowerCase(Defs[I].UnitName)) then
      begin
        if Violation = '' then
          Violation := Defs[I].TypeName + ' (' + Defs[I].UnitName + ')';
      end
      // (2) leakage / non-vacuity: a synth whose same-name $03 sibling(s)
      // live in OTHER units is a genuine cross-unit homonym -- it must NOT
      // be dropped (the drop is unit-scoped). Counting these also proves
      // the test isn't vacuously green from all doublings vanishing.
      else if Names03.ContainsKey(LowerCase(Defs[I].TypeName)) then
        Inc(CrossUnitKept);
    end;

  Assert.AreEqual('', Violation,
    Format('§6.25 (d): synthesised EnumDef "%s" still shares its unit with a ' +
           '$03-sourced def of the same name -- the same-type duplicate drop ' +
           'regressed.', [Violation]));
  Assert.IsTrue(CrossUnitKept > 0,
    'Leakage/non-vacuity: expected at least one synthesised EnumDef whose ' +
    'same-name $03 sibling lives in a DIFFERENT unit to survive (a genuine ' +
    'cross-unit homonym, e.g. TDataType). Zero means either over-drop or no ' +
    'doublings present at all.');
end;

{ TRsmTestLibTests }

procedure TRsmTestLibTests.SetupFixture;
begin
  LoadPrimaryFixture(TestLibExePath, 'Test.Lib');
end;

procedure TRsmTestLibTests.TearDownFixture;
begin
  FreePrimaryFixture;
end;

procedure TRsmTestLibTests.TestLargeBinaryNonTClassDiscovered;
var
  Member : TRsmClassMember;
begin
  if ShouldSkip then Exit;
  // Uses the shared FReader loaded once in SetupFixture.
  // The C-prefixed class is discovered at large-binary scale.
  Assert.IsTrue(FReader.FindClassByName('CJwksValidator') >= 0,
    'CJwksValidator (C-prefixed) must be discovered in the ~172 MB Test.Lib.rsm');
  // ...with its own fields at the live instance offsets (Win32 4-byte
  // slots: VMT@0, FCache@4, FExpectedAudience@8, FExpectedIssuer@12,
  // FExpectedTenantId@16, FExpectedProductId@20).
  Assert.IsTrue(FReader.FindClassMember('CJwksValidator', 'FCache', Member),
    'CJwksValidator.FCache must resolve');
  Assert.AreEqual<UInt32>(4, Member.Offset, 'FCache @4 (right after VMT)');
  Assert.IsTrue(FReader.FindClassMember('CJwksValidator', 'FExpectedTenantId', Member),
    'CJwksValidator.FExpectedTenantId must resolve');
  Assert.AreEqual<UInt32>(16, Member.Offset, 'FExpectedTenantId @16');
  // Leakage guard: a neighbouring T-class is still discovered too.
  Assert.IsTrue(FReader.FindClassByName('TJwtValidationResult') >= 0,
    'TJwtValidationResult (T-prefixed) must still be discovered');
end;

procedure TRsmTestLibTests.TestExpectedTenantIdFieldRecordShapePinned;
const
  // FExpectedTenantId @ instance offset 16 on CTestJwksValidator's
  // class CJwksValidator.
  CFieldName = 'FExpectedTenantId';
  CFieldOff  = 16;
var
  Sz, P    : NativeInt;
  NL       : Integer;
  Name     : String;
  After    : NativeInt;
  EndOff   : NativeInt;
  I        : NativeInt;
  Found    : Boolean;
begin
  if ShouldSkip then Exit;
  // §6.33 SHAPE PIN (large binary). Decodes the managed-reference $2C
  // field record for CJwksValidator.FExpectedTenantId in the current
  // Test.Lib.rsm (RsmTaifunData corpus) and pins the byte layout:
  //   $2C <NL> 'FExpectedTenantId' 00 <scope> 00 <typeLo> <2*off>
  //       9C 09 <6 secondary bytes> 07 00 00 08 <parentLo> <parentHi>
  //   * After+3 = $04 = the single-byte UnicodeString id (the WHOLE
  //     type id; NOT a 2-byte value) -- position unchanged
  //   * After+4 = $20 = 2 x the field's instance offset (16) -- a
  //     positional byte, NOT a type-id high byte. This is why folding
  //     it in produced the unmapped $2004 the old linker emitted.
  //   * BodyLen = 13, "$9C $09" marker at After+5..+6
  // The current corpus emits the longer body=13 / "$9C $09" managed-
  // reference form (a newer Delphi managed-field encoding -- the
  // secondary-id section grew from 2 to 6 bytes); the older builds
  // emitted body=9 / "$9C $01". The UnicodeString id stays at After+3
  // either way, which is what the §6.33-C attribution reads.
  // The end-to-end member resolution that consumes it -- the field
  // record's parent id $0391 is a unit-local id colliding across ~18
  // unrelated types -- is closed by §6.33-C
  // (BindUnresolvedWideBlocksByMemberNameSet) and pinned separately by
  // TestWideBlockNameSetBindsCJwksValidatorStrings. This test pins the
  // byte shape only.
  Sz    := FReader.Scanner.Sz;
  P     := 0;
  Found := False;
  while (P + 64 < Sz) and (not Found) do
  begin
    // Locate the $2C field record by name (offset-free: robust to
    // fixture rebuilds). The name length byte sits at TagOff+1.
    if FReader.Scanner.ByteAt(P) <> $2C then begin Inc(P); Continue; end;
    NL := FReader.Scanner.ByteAt(P + 1);
    if (NL <> Length(CFieldName)) or (P + 2 + NL + 12 >= Sz) then
    begin Inc(P); Continue; end;
    After := P + 2 + NL;
    if (FReader.Scanner.ByteAt(After) <> $00) or
       (FReader.Scanner.ByteAt(After + 2) <> $00) then
    begin Inc(P); Continue; end;
    if not FReader.Scanner.ReadIdentifier(P + 1, Name) then
    begin Inc(P); Continue; end;
    if not SameText(Name, CFieldName) then begin Inc(P); Continue; end;
    // Locate the "07 00 00 08" terminator to confirm body length.
    EndOff := -1;
    for I := After + 5 to After + 30 do
    begin
      if I + 5 > Sz then Break;
      if (FReader.Scanner.ByteAt(I) = $07) and
         (FReader.Scanner.ByteAt(I + 1) = $00) and
         (FReader.Scanner.ByteAt(I + 2) = $00) and
         (FReader.Scanner.ByteAt(I + 3) = $08) then
      begin EndOff := I; Break; end;
    end;
    if EndOff < 0 then begin Inc(P); Continue; end;
    // Pin the decoded shape.
    Assert.AreEqual<Integer>(13, EndOff - After,
      'FExpectedTenantId field record must be body=13 (managed reference, ' +
      'current $9C $09 form)');
    Assert.AreEqual<Byte>($9C, FReader.Scanner.ByteAt(After + 5),
      'managed-ref marker byte 1 at After+5');
    Assert.AreEqual<Byte>($09, FReader.Scanner.ByteAt(After + 6),
      'managed-ref marker byte 2 at After+6 (current $09 form)');
    Assert.AreEqual<Byte>($04, FReader.Scanner.ByteAt(After + 3),
      'After+3 is the single-byte UnicodeString id $04');
    Assert.AreEqual<Byte>(Byte(2 * CFieldOff), FReader.Scanner.ByteAt(After + 4),
      'After+4 is 2 x the field instance offset (positional, not a type byte)');
    Found := True;
  end;
  Assert.IsTrue(Found,
    'CJwksValidator.FExpectedTenantId $2C field record not found in Test.Lib.rsm');
end;

procedure TRsmTestLibTests.TestWideBlockNameSetBindsCJwksValidatorStrings;
var
  Member: TRsmClassMember;
begin
  if ShouldSkip then Exit;
  // §6.33-C PIN (large binary). BindUnresolvedWideBlocksByMemberNameSet
  // attributes CJwksValidator's wide-$2C field block to the class via
  // the unique sorted member-name-set match (its wide parent id $0391 is
  // a unit-local id colliding across ~18 unrelated types, so neither the
  // registry path nor the block-owner index can). The three string
  // fields now carry the single-byte UnicodeString id $04, so a bare
  // `evaluate ...FExpectedTenantId` auto-types as 'string' without an
  // explicit type=.
  Assert.IsTrue(FReader.FindClassMember('CJwksValidator', 'FExpectedTenantId', Member),
    'FExpectedTenantId resolves');
  Assert.AreEqual<UInt16>($04, Member.PrimitiveTypeId,
    'FExpectedTenantId -> UnicodeString $04 (attributed via member-name-set match)');
  Assert.IsTrue(FReader.FindClassMember('CJwksValidator', 'FExpectedAudience', Member),
    'FExpectedAudience resolves');
  Assert.AreEqual<UInt16>($04, Member.PrimitiveTypeId, 'FExpectedAudience -> $04');
  Assert.IsTrue(FReader.FindClassMember('CJwksValidator', 'FExpectedProductId', Member),
    'FExpectedProductId resolves');
  Assert.AreEqual<UInt16>($04, Member.PrimitiveTypeId, 'FExpectedProductId -> $04');

  // Leakage guard: FCache is a class-typed field (body=10, marker@+6) --
  // its FieldId is the unreliable unit-local id, so the pass deliberately
  // skips the enum/class shapes and leaves FCache's PrimitiveTypeId at 0
  // rather than stamping a bogus id.
  Assert.IsTrue(FReader.FindClassMember('CJwksValidator', 'FCache', Member),
    'FCache resolves as a member');
  Assert.AreEqual<UInt16>(0, Member.PrimitiveTypeId,
    'FCache (class-typed) must NOT be stamped with a primitive id');
end;

initialization
  TDUnitX.RegisterTestFixture(TRsmTfwTests);
  TDUnitX.RegisterTestFixture(TRsmTestLibTests);

end.
