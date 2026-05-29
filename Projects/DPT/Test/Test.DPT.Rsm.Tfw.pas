// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Rsm.Tfw;

// Tests that require the developer-machine-only TFW fixture
// (C:\MSE\TFW\TFW.exe + .rsm + .map). Loading TFW.rsm takes ~24 s on
// the developer machine and gets dramatically slower as the file
// grows, so every test that needs the loaded reader shares a single
// reader instance created in [SetupFixture]. The perf-regression
// assertions live in the same fixture and read back the per-phase
// timings captured by the OnPhase callback during that one load, so
// the perf net is not paying for the load twice.
//
// All tests skip silently (Assert.Pass + a WARNING line to stdout)
// when the fixture is missing, so the suite stays green on clean
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

  [TestFixture]
  TRsmTfwTests = class
  public const
    /// Single source of truth for the TFW fixture location. Both the
    /// .rsm sidecar (loaded by TRsmReader) and the .map file
    /// (loaded by TMapFileParser) are derived from this base path.
    TfwExePath      = 'C:\MSE\TFW\TFW.exe';
    TfwMapPath      = 'C:\MSE\TFW\TFW.map';
    /// Win64 variant of the fixture, used by §6.2 (Win64 proc-address
    /// VAs > 2 MB) reverse-engineering. Only consumed when the test
    /// exe is itself 64-bit (the .rsm at ~1.17 GB is too tight for a
    /// 32-bit process address space alongside the Win32 TFW load).
    TfwWin64ExePath = 'C:\MSE\TFW\TFW.Win64.Debug.exe';
    TfwWin64MapPath = 'C:\MSE\TFW\TFW.Win64.Debug.map';
  strict private
    FReader      : TRsmReader;
    FMap         : TMapFileParser;
    FPhaseTable  : TStringList;
    FPhaseTimings: IKeyValue<String, Int64>;
    FTotalLoadMs : Int64;
    FFixtureRsm  : String;
    FFixtureMB   : Double;
    FFixtureLine : String;
    FSkipReason  : String;
    {$IFDEF CPUX64}
    /// Win64 reader / map. The reader exposes Procs[].SegmentOffset
    /// (decoded address) and its internal Scanner (Is64Bit + ByteAt
    /// for raw-byte diagnostics). The map provides ground-truth
    /// RVAs for the §6.2 pin and future Win64 sub-form investigations.
    FReaderWin64    : TRsmReader;
    FMapWin64       : TMapFileParser;
    FWin64SkipReason: String;
    {$ENDIF}
    /// Sets FSkipReason and returns True when the fixture is missing.
    /// Both [Test] methods call this and Assert.Pass + Exit on True so
    /// they don't dereference FReader / FMap when the load was skipped.
    function ShouldSkip: Boolean;
    {$IFDEF CPUX64}
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
    procedure TestTfwUnitUseTableLtInlandRvaMatchesCanonicalBlock;
  end;

implementation

function TRsmTfwTests.ShouldSkip: Boolean;
begin
  Result := FSkipReason <> '';
  if Result then
    Assert.Pass(FSkipReason);
end;

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
  TotalSW: TStopwatch;
  PhaseSW: TStopwatch;
  LogPath: String;
  W      : TStreamWriter;
  I      : Integer;
begin
  FSkipReason := '';
  if not TFile.Exists(TfwExePath) then
  begin
    FSkipReason := Format(
      'WARNING: TFW tests SKIPPED -- fixture not found at "%s". ' +
      'The interface-scope global encoding and the perf-regression ' +
      'budgets cannot be exercised without it.', [TfwExePath]);
    Writeln(FSkipReason);
    Exit;
  end;

  // Announce the fixture identity before the multi-second load
  // starts. The same line goes into dpt.log so post-mortem
  // comparison across runs can match different .rsm sizes to
  // different timings.
  FFixtureRsm := ChangeFileExt(TfwExePath, '.rsm');
  FFixtureMB := 0;
  if TFile.Exists(FFixtureRsm) then
    FFixtureMB := TFile.GetSize(FFixtureRsm) / (1024 * 1024);
  FFixtureLine := Format('  fixture: exe=%s  rsm=%s (%.1f MB)',
    [TfwExePath, FFixtureRsm, FFixtureMB]);
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
  FReader.LoadFromFile(TfwExePath);
  TotalSW.Stop;
  FTotalLoadMs := TotalSW.ElapsedMilliseconds;

  // Mirror the timings into dpt.log so the file the user already
  // tails for live progress also carries the post-mortem summary.
  var Summary: String := Format('%s [tfw-diag] total=%d ms  procs=%d  classes=%d',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
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

  // Also echo summary to stdout so DUnitX's runner output captures it.
  Writeln(Summary);
  Writeln(FFixtureLine);
  for I := 0 to FPhaseTable.Count - 1 do
    Writeln(FPhaseTable[I]);

  // Map-file parser is needed by TestTfwGlobalRecordResolves's VA
  // tie-break check; load it once here so both tests share it.
  if TFile.Exists(TfwMapPath) then
    FMap := TMapFileParser.Create(TfwMapPath);

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
  FreeAndNil(FMap);
  FreeAndNil(FReader);
  FreeAndNil(FPhaseTable);
  FPhaseTimings := nil;
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
// the canonical little-endian RVA $7DA69008 (bytes 08 90 A6 7D, the
// canonical $2A 'TLandTyp' registry entry body at +3..+6 per
// §6.20-Round-3). The Round-3 PS scan counted 68 occurrences across
// TFW; a regression past ~60 means the decoder either bailed out of
// the segment walk too early or the structural anchor stopped
// matching for entire ranges of the corpus.
const
  ExpectedRva: UInt32 = $7DA69008;
  MinCount   : Integer = 60;
var
  Segments: IList<TRsmUnitUseSegment>;
  I, J    : Integer;
  Seg     : TRsmUnitUseSegment;
  Ref     : TRsmUnitUseRef;
  Hits    : Integer;
  SawCorrectRva: Boolean;
  AnyTLandTypRva: UInt32;
begin
  if ShouldSkip then Exit;
  Segments := FReader.UnitUseSegments;
  Assert.IsTrue(Segments.Count > 0,
    'UnitUseSegments empty on TFW.rsm -- the decoder never opened ' +
    'a single segment.');
  Hits := 0;
  SawCorrectRva := False;
  AnyTLandTypRva := 0;
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
        AnyTLandTypRva := Ref.Rva;
        if Ref.Rva = ExpectedRva then
          SawCorrectRva := True;
      end;
    end;
  end;
  Assert.IsTrue(Hits >= MinCount,
    Format('Expected at least %d $66 ''TLandTyp'' references in TFW; ' +
           'got %d. Either the segment walk stops too early, or the ' +
           'TFW build dropped its TLandTyp imports.',
           [MinCount, Hits]));
  Assert.IsTrue(SawCorrectRva,
    Format('No $66 ''TLandTyp'' reference carries the canonical RVA ' +
           '$%x. A sample TLandTyp Rva I did see was $%x. Either the ' +
           '4-byte payload is being read at the wrong offset, or the ' +
           'canonical $2A ''TLandTyp'' moved on this TFW build (in ' +
           'which case the §6.21 spec needs updating).',
           [ExpectedRva, AnyTLandTypRva]));
end;

procedure TRsmTfwTests.TestTfwUnitUseTableLtInlandRvaMatchesCanonicalBlock;
// §4.17 / §6.21 cross-validation pin. Three assertions, layered so
// the pin survives offset drift between TFW rebuilds while still
// guarding the structural finding:
//   (1) At least one decoded $67 'ltInland' entry carries the
//       canonical-shape payload bytes "?? 39 A4 10" -- the
//       conserved tail §6.20-Round-3 observed across the +3 LSB
//       stride family.
//   (2) The canonical $25 'ltInland' record is locatable in the
//       byte stream and ITS body carries those same 4 bytes.
//   (3) The $67 payload and the $25 body payload agree
//       byte-for-byte, which is what anchors "the $67 payload
//       references the canonical declaration's RVA slot".
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
  Ref       : TRsmUnitUseRef;
  DecodedRva: UInt32;
  CanonRva  : UInt32;
  DecodedHit: Boolean;
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
  DecodedRva := 0;
  for I := 0 to Segments.Count - 1 do
  begin
    Seg := Segments[I];
    if Seg.Refs = nil then Continue;
    for J := 0 to Seg.Refs.Count - 1 do
    begin
      Ref := Seg.Refs[J];
      if (Ref.Kind = uukSymbol) and (Ref.Name = 'ltInland') then
      begin
        DecodedRva := Ref.Rva;
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
  // the little-endian RVA. The LSB byte is per-ordinal (81 / 84 / 87
  // / 8A / 8D for ltInland / ltAusland / ... in §6.20-Round-3's
  // recorded snapshot); we don't pin that here because the absolute
  // RVA can drift with the build.
  Assert.AreEqual<UInt32>(UInt32($10A43900), DecodedRva and UInt32($FFFFFF00),
    Format('Decoded $67 ltInland RVA $%x lacks the conserved tail ' +
           '"39 A4 10" (high 24 bits). Either the structural finding ' +
           'is broken on this TFW build or the 4-byte payload read ' +
           'mis-aligned.', [DecodedRva]));

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

  // The canonical block's RVA payload sits at the first non-zero
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
  CanonRva := UInt32(Sc.ByteAt(CanonOff))            or
              (UInt32(Sc.ByteAt(CanonOff + 1)) shl 8)  or
              (UInt32(Sc.ByteAt(CanonOff + 2)) shl 16) or
              (UInt32(Sc.ByteAt(CanonOff + 3)) shl 24);

  // (3) Decoded $67 payload must EQUAL the canonical $25 body's
  // payload byte-for-byte. This is what proves the $67 entry carries
  // the canonical declaration's RVA, not some other slot.
  Assert.AreEqual<UInt32>(CanonRva, DecodedRva,
    Format('Decoded $67 ltInland RVA $%x does not match the canonical ' +
           '$25 ltInland body RVA $%x at file offset %d. The §6.21 ' +
           'spec''s structural mapping between $67 use-sites and the ' +
           '$25 canonical block requires byte-identity here.',
           [DecodedRva, CanonRva, CanonOff]));
end;

initialization
  TDUnitX.RegisterTestFixture(TRsmTfwTests);

end.
