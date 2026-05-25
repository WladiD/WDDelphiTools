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
  DPT.Rsm.Reader;

type

  [TestFixture]
  TRsmTfwTests = class
  public const
    /// Single source of truth for the TFW fixture location. Both the
    /// .rsm sidecar (loaded by TRsmReader) and the .map file
    /// (loaded by TMapFileParser) are derived from this base path.
    TfwExePath = 'C:\MSE\TFW\TFW.exe';
    TfwMapPath = 'C:\MSE\TFW\TFW.map';
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
    /// Sets FSkipReason and returns True when the fixture is missing.
    /// Both [Test] methods call this and Assert.Pass + Exit on True so
    /// they don't dereference FReader / FMap when the load was skipped.
    function ShouldSkip: Boolean;
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
  end;

implementation

function TRsmTfwTests.ShouldSkip: Boolean;
begin
  Result := FSkipReason <> '';
  if Result then
    Assert.Pass(FSkipReason);
end;

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
end;

procedure TRsmTfwTests.TearDownFixture;
begin
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

initialization
  TDUnitX.RegisterTestFixture(TRsmTfwTests);

end.
