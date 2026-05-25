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

    {$IFDEF CPUX64}
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
  // Total budget: the post-fix load is ~24 s on the developer
  // machine. Allow 3x for CI / cold-cache / slower disks but trip
  // well below the 425 s pre-fix value. A regression past this
  // will get caught at the next test run instead of surfacing as
  // a frozen MCP session minutes after deploy.
  HardLimit  = 90 * 1000;
  // Per-phase budgets, picked so each O(N^2) regression we just
  // killed (insertion-sort on 200k procs, per-byte String alloc
  // in the struct scan) trips its own assertion with a clear
  // message. Each is roughly 5-10x the observed post-fix value.
  // Names must match the strings emitted by OnPhase in
  // TRsmReader (ScanSymbolStream / RecomputeProcSizes /
  // DiscoverAndParseAllStructs / LinkMemberTypeIdsFromFormatA /
  // DeriveClassParents); a typo here silently passes.
  PhaseBudget_DiscoverAndParseAllStructs   = 45 * 1000;
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

{$IFDEF CPUX64}
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

initialization
  TDUnitX.RegisterTestFixture(TRsmTfwTests);

end.
