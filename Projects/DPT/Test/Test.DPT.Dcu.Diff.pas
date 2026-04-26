unit Test.DPT.Dcu.Diff;

interface

uses

  DUnitX.TestFramework,

  System.IOUtils,
  System.SysUtils,

  mormot.core.collections,

  DPT.Dcu.Analyzer,
  DPT.Dcu.Diff,
  DPT.Dcu.Types;

type

  [TestFixture]
  TTestDcuDiff = class
  private
    function FixtureDcu(const ABaseName: string): string;
    function MakeAnalysis(const APath, AUnitName: string;
      const AInterfaceUses, AImplUses: array of string): TDcuAnalysisResult;
  public
    [Test] procedure SelfCompareYieldsIdentical;
    [Test] procedure DifferentCommittedUnitsYieldDifferentUnits;
    [Test] procedure InterfaceUsesAdditionDetected;
    [Test] procedure ImplementationUsesRemovalDetected;
    [Test] procedure UsesOrderChangeDetectedSeparately;
    [Test] procedure HeaderOnlyChangeYieldsMetadataOnly;
    [Test] procedure StatusEscalatesToHighestSeverity;
    [Test] procedure DifferentUnitsBeatsAllOtherChanges;
  end;

implementation

{ TTestDcuDiff }

function TTestDcuDiff.FixtureDcu(const ABaseName: string): string;
begin
  Result := ExpandFileName('Projects\DPT\DCU\' + ABaseName + '.dcu');
  if not FileExists(Result) then
    Result := ExpandFileName('..\..\DCU\' + ABaseName + '.dcu');
  if not FileExists(Result) then
    Result := ExpandFileName('..\DCU\' + ABaseName + '.dcu');
  if not FileExists(Result) then
    Assert.Fail('Test fixture not found: ' + ABaseName + '.dcu');
end;

function TTestDcuDiff.MakeAnalysis(const APath, AUnitName: string;
  const AInterfaceUses, AImplUses: array of string): TDcuAnalysisResult;
var
  I: Integer;
begin
  // Build a synthetic analysis result so the diff engine can be tested
  // independently of the byte-level extraction. The fields not set here
  // default to empty/zero, which is fine for the diff inputs.
  Result := Default(TDcuAnalysisResult);
  Result.FilePath := APath;
  Result.IsDcu := True;
  Result.Header.UnitName := AUnitName;
  Result.Header.IncludeSources := Collections.NewPlainList<TDcuSourceRef>;
  Result.InterfaceUses := Collections.NewPlainList<TDcuUsesEntry>;
  Result.ImplementationUses := Collections.NewPlainList<TDcuUsesEntry>;
  Result.Diagnostics := Collections.NewList<string>;
  for I := 0 to High(AInterfaceUses) do
    Result.InterfaceUses.Add(
      TDcuUsesEntry.Create(AInterfaceUses[I], dusInterface, I));
  for I := 0 to High(AImplUses) do
    Result.ImplementationUses.Add(
      TDcuUsesEntry.Create(AImplUses[I], dusImplementation, I));
end;

procedure TTestDcuDiff.SelfCompareYieldsIdentical;
var
  Res   : TDcuAnalysisResult;
  Report: TDcuDiffReport;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Report := TDcuDiff.Compare(Res, Res);
  Assert.AreEqual(Ord(ddIdentical), Ord(Report.Status),
    'A DCU compared with itself must be reported as Identical');
  Assert.AreEqual(0, Report.HeaderChanges.Count);
  Assert.AreEqual(0, Report.InterfaceUsesAdded.Count);
  Assert.AreEqual(0, Report.InterfaceUsesRemoved.Count);
  Assert.AreEqual(0, Report.ImplUsesAdded.Count);
  Assert.AreEqual(0, Report.ImplUsesRemoved.Count);
  Assert.IsFalse(Report.InterfaceUsesOrderChange);
  Assert.IsFalse(Report.ImplUsesOrderChange);
end;

procedure TTestDcuDiff.DifferentCommittedUnitsYieldDifferentUnits;
var
  ResA, ResB: TDcuAnalysisResult;
  Report    : TDcuDiffReport;
begin
  ResA := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  ResB := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Report := TDcuDiff.Compare(ResA, ResB);
  Assert.AreEqual(Ord(ddDifferentUnits), Ord(Report.Status));
  Assert.AreEqual('DPT.Detection', Report.UnitNameA);
  Assert.AreEqual('DPT.Application', Report.UnitNameB);
end;

procedure TTestDcuDiff.InterfaceUsesAdditionDetected;
var
  ResA, ResB: TDcuAnalysisResult;
  Report    : TDcuDiffReport;
begin
  ResA := MakeAnalysis('A', 'Foo',
    ['System.Classes', 'DPT.Types'],
    []);
  ResB := MakeAnalysis('B', 'Foo',
    ['System.Classes', 'DPT.Types', 'NewUnit'],
    []);
  Report := TDcuDiff.Compare(ResA, ResB);
  Assert.AreEqual(Ord(ddUsesSetChange), Ord(Report.Status));
  Assert.AreEqual(1, Report.InterfaceUsesAdded.Count);
  Assert.AreEqual('NewUnit', Report.InterfaceUsesAdded[0]);
  Assert.AreEqual(0, Report.InterfaceUsesRemoved.Count);
end;

procedure TTestDcuDiff.ImplementationUsesRemovalDetected;
var
  ResA, ResB: TDcuAnalysisResult;
  Report    : TDcuDiffReport;
begin
  ResA := MakeAnalysis('A', 'Foo',
    [],
    ['System.SysUtils', 'System.Classes', 'DPT.Detection']);
  ResB := MakeAnalysis('B', 'Foo',
    [],
    ['System.SysUtils', 'System.Classes']);
  Report := TDcuDiff.Compare(ResA, ResB);
  Assert.AreEqual(Ord(ddUsesSetChange), Ord(Report.Status));
  Assert.AreEqual(0, Report.ImplUsesAdded.Count);
  Assert.AreEqual(1, Report.ImplUsesRemoved.Count);
  Assert.AreEqual('DPT.Detection', Report.ImplUsesRemoved[0]);
end;

procedure TTestDcuDiff.UsesOrderChangeDetectedSeparately;
var
  ResA, ResB: TDcuAnalysisResult;
  Report    : TDcuDiffReport;
begin
  // Same set, different order. Should be reported as UsesOrderChange,
  // NOT UsesSetChange - this is the harder bug to spot.
  ResA := MakeAnalysis('A', 'Foo',
    ['System.Classes', 'System.SysUtils', 'DPT.Types'],
    []);
  ResB := MakeAnalysis('B', 'Foo',
    ['DPT.Types', 'System.Classes', 'System.SysUtils'],
    []);
  Report := TDcuDiff.Compare(ResA, ResB);
  Assert.AreEqual(Ord(ddUsesOrderChange), Ord(Report.Status),
    'Same uses set in different order must be reported as UsesOrderChange');
  Assert.AreEqual(0, Report.InterfaceUsesAdded.Count);
  Assert.AreEqual(0, Report.InterfaceUsesRemoved.Count);
  Assert.IsTrue(Report.InterfaceUsesOrderChange);
end;

procedure TTestDcuDiff.HeaderOnlyChangeYieldsMetadataOnly;
var
  ResA, ResB: TDcuAnalysisResult;
  Report    : TDcuDiffReport;
begin
  ResA := MakeAnalysis('A', 'Foo', ['System.Classes'], ['System.SysUtils']);
  ResB := MakeAnalysis('B', 'Foo', ['System.Classes'], ['System.SysUtils']);
  // Synthetically perturb only the magic byte / compiler so HeaderChanges
  // populate but everything else stays equal.
  ResA.Header.MagicHex := '4D 03 00 24';
  ResA.Header.DetectedCompiler := dccDelphi12;
  ResB.Header.MagicHex := '4D 03 00 25';
  ResB.Header.DetectedCompiler := dccDelphi13;
  Report := TDcuDiff.Compare(ResA, ResB);
  Assert.AreEqual(Ord(ddMetadataOnly), Ord(Report.Status));
  Assert.IsTrue(Report.HeaderChanges.Count >= 2,
    'Should report magic + compiler change');
end;

procedure TTestDcuDiff.StatusEscalatesToHighestSeverity;
var
  ResA, ResB: TDcuAnalysisResult;
  Report    : TDcuDiffReport;
begin
  // Both header AND uses set differ. Status must be UsesSetChange (the
  // higher-severity one), not MetadataOnly.
  ResA := MakeAnalysis('A', 'Foo', ['System.Classes'], []);
  ResB := MakeAnalysis('B', 'Foo', ['System.Classes', 'System.SysUtils'], []);
  ResA.Header.MagicHex := 'AA';
  ResB.Header.MagicHex := 'BB';
  Report := TDcuDiff.Compare(ResA, ResB);
  Assert.AreEqual(Ord(ddUsesSetChange), Ord(Report.Status),
    'Status must escalate to UsesSetChange when both header and uses differ');
  Assert.IsTrue(Report.HeaderChanges.Count > 0);
end;

procedure TTestDcuDiff.DifferentUnitsBeatsAllOtherChanges;
var
  ResA, ResB: TDcuAnalysisResult;
  Report    : TDcuDiffReport;
begin
  ResA := MakeAnalysis('A', 'Foo', ['System.Classes'], []);
  ResB := MakeAnalysis('B', 'Bar', ['System.Classes', 'X'], []);
  Report := TDcuDiff.Compare(ResA, ResB);
  Assert.AreEqual(Ord(ddDifferentUnits), Ord(Report.Status),
    'Different unit names trump every other diff dimension');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDcuDiff);

end.
