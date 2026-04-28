unit Test.DPT.Exe.Analyzer;

interface

uses

  DUnitX.TestFramework,

  System.IOUtils,
  System.SysUtils,

  mormot.core.collections,

  DPT.Dcu.Index,
  DPT.Dcu.Types,
  DPT.Exe.Analyze.Task,
  DPT.Exe.Analyzer,
  DPT.Exe.Types;

type

  [TestFixture]
  TTestExeAnalyzer = class
  private
    function FixtureExe(const AVariant: string): string;
    function ContainsUnit(const AResult: TExeAnalysisResult;
      const AUnitName: string): Boolean;
    function MakeIndexEntry(const AUnitName, AFilePath: string;
      APlatform: TDcuPlatform): TDcuIndexEntry;
    function MakeSyntheticIndex(
      const AEntries: array of TDcuIndexEntry): TDcuIndex;
  public
    // PE-header parsing
    [Test] procedure RecognisesWin32DebugTarget;
    [Test] procedure RecognisesWin64DebugTarget;
    [Test] procedure ReportsCorrectPlatformPerVariant;
    [Test] procedure NonPeFileFailsGracefully;
    [Test] procedure MissingFileFailsGracefully;

    // Map-file detection
    [Test] procedure DetectsSiblingMapFile;
    [Test] procedure ExtractsKnownLinkedUnits;
    [Test] procedure UnitListIsDeduplicated;
    [Test] procedure SegmentBasedSizeWhenAvailable;

    // Index integration
    [Test] procedure ResolveAgainstIndexAttachesPaths;
    [Test] procedure ResolveAgainstIndexPrefersMatchingPlatform;
    [Test] procedure ResolveAgainstEmptyIndexLeavesPathsEmpty;

    // No-map fallback
    [Test] procedure NoMapFileResultsInEmptyUnitsAndDiagnostic;

    // Default order is by ascending VA (linker layout)
    [Test] procedure DefaultUnitOrderIsAscendingVA;
    [Test] procedure SortByNameIsCaseInsensitiveAlphabetical;
    [Test] procedure SortBySizeIsDescendingWithNameTieBreak;
  end;

implementation

{ TTestExeAnalyzer }

function TTestExeAnalyzer.FixtureExe(const AVariant: string): string;
begin
  // AVariant is "Win32" or "Win64"
  Result := ExpandFileName('Projects\DPT\Test\' + AVariant + '\DebugTarget.exe');
  if not FileExists(Result) then
    Result := ExpandFileName('..\..\Test\' + AVariant + '\DebugTarget.exe');
  if not FileExists(Result) then
    Result := ExpandFileName('..\' + AVariant + '\DebugTarget.exe');
  if not FileExists(Result) then
    Result := ExpandFileName(AVariant + '\DebugTarget.exe');
  if not FileExists(Result) then
    Assert.Fail(AVariant + '\DebugTarget.exe fixture not found - tried multiple paths');
end;

function TTestExeAnalyzer.ContainsUnit(const AResult: TExeAnalysisResult;
  const AUnitName: string): Boolean;
var
  Entry: TExeUnitEntry;
begin
  Result := False;
  for Entry in AResult.Units do
    if SameText(Entry.Name, AUnitName) then
      Exit(True);
end;

function TTestExeAnalyzer.MakeIndexEntry(const AUnitName, AFilePath: string;
  APlatform: TDcuPlatform): TDcuIndexEntry;
begin
  Result := Default(TDcuIndexEntry);
  Result.UnitName := AUnitName;
  Result.FilePath := AFilePath;
  Result.Compiler := dccDelphi13;
  Result.Platform := APlatform;
  Result.FileSize := 1024;
  Result.Includes := Collections.NewList<string>;
  Result.InterfaceUses := Collections.NewList<string>;
  Result.ImplementationUses := Collections.NewList<string>;
  Result.TypeRefs := Collections.NewList<string>;
  Result.MethodRefs := Collections.NewList<string>;
  Result.ExportedTypes := Collections.NewList<string>;
  Result.ExportedRoutines := Collections.NewList<string>;
end;

function TTestExeAnalyzer.MakeSyntheticIndex(
  const AEntries: array of TDcuIndexEntry): TDcuIndex;
var
  Entry: TDcuIndexEntry;
begin
  Result := Default(TDcuIndex);
  Result.Schema := DcuIndexSchemaVersion;
  Result.RootDirs := Collections.NewList<string>;
  Result.Units := Collections.NewPlainList<TDcuIndexEntry>;
  Result.ReverseImportIndex := Collections.NewKeyValue<string, IList<string>>;
  Result.SymbolToDefiningUnit := Collections.NewKeyValue<string, IList<string>>;
  for Entry in AEntries do
    Result.Units.Add(Entry);
end;

procedure TTestExeAnalyzer.RecognisesWin32DebugTarget;
var
  Res: TExeAnalysisResult;
begin
  Res := TExeAnalyzer.Analyze(FixtureExe('Win32'));
  Assert.IsTrue(Res.IsPE, 'Win32 DebugTarget.exe must be recognised as PE');
  Assert.AreEqual(Ord(dpWin32), Ord(Res.Platform));
end;

procedure TTestExeAnalyzer.RecognisesWin64DebugTarget;
var
  Res: TExeAnalysisResult;
begin
  Res := TExeAnalyzer.Analyze(FixtureExe('Win64'));
  Assert.IsTrue(Res.IsPE);
  Assert.AreEqual(Ord(dpWin64), Ord(Res.Platform));
end;

procedure TTestExeAnalyzer.ReportsCorrectPlatformPerVariant;
var
  Res32, Res64: TExeAnalysisResult;
begin
  Res32 := TExeAnalyzer.Analyze(FixtureExe('Win32'));
  Res64 := TExeAnalyzer.Analyze(FixtureExe('Win64'));
  // The two binaries are built from the same source - their unit lists
  // should overlap heavily but their platform must differ.
  Assert.AreNotEqual(Ord(Res32.Platform), Ord(Res64.Platform),
    'Win32 and Win64 fixtures must come back as different platforms');
end;

procedure TTestExeAnalyzer.NonPeFileFailsGracefully;
var
  Res     : TExeAnalysisResult;
  TempFile: string;
begin
  // Plain text file - no MZ signature.
  TempFile := TPath.Combine(TPath.GetTempPath, 'not-a-pe.bin');
  TFile.WriteAllText(TempFile, 'this is not an executable');
  try
    Res := TExeAnalyzer.Analyze(TempFile);
    Assert.IsFalse(Res.IsPE);
    Assert.IsTrue(Res.Diagnostics.Count > 0,
      'A non-PE input must produce a diagnostic');
  finally
    if FileExists(TempFile) then TFile.Delete(TempFile);
  end;
end;

procedure TTestExeAnalyzer.MissingFileFailsGracefully;
var
  Res: TExeAnalysisResult;
begin
  Res := TExeAnalyzer.Analyze('C:\No\Such\Path\app.exe');
  Assert.IsFalse(Res.IsPE);
  Assert.IsTrue(Res.Diagnostics.Count > 0);
end;

procedure TTestExeAnalyzer.DetectsSiblingMapFile;
var
  Res: TExeAnalysisResult;
begin
  Res := TExeAnalyzer.Analyze(FixtureExe('Win32'));
  Assert.AreEqual(Ord(edmMap), Ord(Res.Detection),
    'Sibling .map file must be detected');
  Assert.IsTrue(Res.MapFilePath <> '');
  Assert.IsTrue(FileExists(Res.MapFilePath));
end;

procedure TTestExeAnalyzer.ExtractsKnownLinkedUnits;
var
  Res: TExeAnalysisResult;
begin
  // DebugTarget.dpr uses System and SysUtils transitively, plus the
  // unit DebugTarget itself. These must all show up in the linked list.
  Res := TExeAnalyzer.Analyze(FixtureExe('Win32'));
  Assert.IsTrue(ContainsUnit(Res, 'System'),
    'System must be in the linked-units list');
  Assert.IsTrue(ContainsUnit(Res, 'System.SysUtils'),
    'System.SysUtils must be in the linked-units list');
  Assert.IsTrue(ContainsUnit(Res, 'DebugTarget'),
    'The main program unit DebugTarget must be in the linked-units list');
end;

procedure TTestExeAnalyzer.UnitListIsDeduplicated;
var
  Entry: TExeUnitEntry;
  Res  : TExeAnalysisResult;
  Seen : IKeyValue<string, Boolean>;
begin
  Res := TExeAnalyzer.Analyze(FixtureExe('Win32'));
  Seen := Collections.NewPlainKeyValue<string, Boolean>;
  for Entry in Res.Units do
  begin
    Assert.IsFalse(Seen.ContainsKey(LowerCase(Entry.Name)),
      'Duplicate unit in linked-units list: ' + Entry.Name);
    Seen.Add(LowerCase(Entry.Name), True);
  end;
end;

procedure TTestExeAnalyzer.SegmentBasedSizeWhenAvailable;
var
  AnyWithSize: Boolean;
  Entry      : TExeUnitEntry;
  Res        : TExeAnalysisResult;
begin
  // Delphi-built map files contain a Detailed-Segments section, so at
  // least some of the entries must come back with a non-zero CodeSize.
  Res := TExeAnalyzer.Analyze(FixtureExe('Win32'));
  AnyWithSize := False;
  for Entry in Res.Units do
    if Entry.CodeSize > 0 then
    begin
      AnyWithSize := True;
      Break;
    end;
  Assert.IsTrue(AnyWithSize,
    'Real map file must produce at least one unit with a non-zero CodeSize');
end;

procedure TTestExeAnalyzer.ResolveAgainstIndexAttachesPaths;
var
  Idx  : TDcuIndex;
  Res  : TExeAnalysisResult;
  Found: Boolean;
  Entry: TExeUnitEntry;
begin
  // Synthesise an index that contains the DebugTarget unit and
  // verify the analyzer wires the path through to ResolvedDcu.
  Idx := MakeSyntheticIndex([
    MakeIndexEntry('DebugTarget',
      'C:\Synth\DebugTarget.dcu', dpWin32),
    MakeIndexEntry('System',
      'C:\Synth\System.dcu', dpWin32)
  ]);
  Res := TExeAnalyzer.Analyze(FixtureExe('Win32'), Idx);
  Found := False;
  for Entry in Res.Units do
    if SameText(Entry.Name, 'DebugTarget') then
    begin
      Assert.AreEqual('C:\Synth\DebugTarget.dcu', Entry.ResolvedDcu);
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found, 'DebugTarget unit must be present in the result');
end;

procedure TTestExeAnalyzer.ResolveAgainstIndexPrefersMatchingPlatform;
var
  Idx  : TDcuIndex;
  Res  : TExeAnalysisResult;
  Entry: TExeUnitEntry;
begin
  // Two index entries with same unit name but different platforms.
  // Analyzer must pick the one whose platform matches the EXE.
  Idx := MakeSyntheticIndex([
    MakeIndexEntry('DebugTarget', 'C:\Wrong\Win64\DebugTarget.dcu', dpWin64),
    MakeIndexEntry('DebugTarget', 'C:\Right\Win32\DebugTarget.dcu', dpWin32)
  ]);
  Res := TExeAnalyzer.Analyze(FixtureExe('Win32'), Idx);
  for Entry in Res.Units do
    if SameText(Entry.Name, 'DebugTarget') then
    begin
      Assert.AreEqual('C:\Right\Win32\DebugTarget.dcu', Entry.ResolvedDcu,
        'Win32 EXE must prefer the Win32 index entry');
      Exit;
    end;
  Assert.Fail('DebugTarget not present in the result');
end;

procedure TTestExeAnalyzer.ResolveAgainstEmptyIndexLeavesPathsEmpty;
var
  Idx  : TDcuIndex;
  Res  : TExeAnalysisResult;
  Entry: TExeUnitEntry;
begin
  Idx := MakeSyntheticIndex([]);
  Res := TExeAnalyzer.Analyze(FixtureExe('Win32'), Idx);
  for Entry in Res.Units do
    Assert.AreEqual('', Entry.ResolvedDcu,
      'Empty index must leave every ResolvedDcu empty');
end;

procedure TTestExeAnalyzer.NoMapFileResultsInEmptyUnitsAndDiagnostic;
var
  ExePath  : string;
  MapPath  : string;
  Res      : TExeAnalysisResult;
  TempDir  : string;
begin
  // Copy DebugTarget.exe to a temp dir WITHOUT its .map file. The
  // analyzer must still produce a valid PE result with an empty
  // unit list and an explanatory diagnostic.
  TempDir := TPath.Combine(TPath.GetTempPath,
    'DPT.ExeAnalyze.' + GUIDToString(TGUID.NewGuid));
  ForceDirectories(TempDir);
  try
    ExePath := TPath.Combine(TempDir, 'NoMap.exe');
    TFile.Copy(FixtureExe('Win32'), ExePath);
    MapPath := ChangeFileExt(ExePath, '.map');
    if FileExists(MapPath) then TFile.Delete(MapPath);

    Res := TExeAnalyzer.Analyze(ExePath);
    Assert.IsTrue(Res.IsPE);
    Assert.AreEqual(Ord(edmNone), Ord(Res.Detection),
      'Without a sibling .map file Detection must be edmNone');
    Assert.AreEqual(0, Res.Units.Count);
    Assert.IsTrue(Res.Diagnostics.Count > 0,
      'No-map case must emit a diagnostic explaining the empty list');
  finally
    if DirectoryExists(TempDir) then TDirectory.Delete(TempDir, True);
  end;
end;

procedure TTestExeAnalyzer.DefaultUnitOrderIsAscendingVA;
var
  Prev : UInt64;
  Units: IList<TExeUnitEntry>;
begin
  // Apply the default sort (--SortBy=VA) and verify strict ascending
  // StartVA across the whole list - that is the linker-layout view
  // that lets a reader scan top-to-bottom.
  Units := Collections.NewPlainList<TExeUnitEntry>;
  for var Entry in TExeAnalyzer.Analyze(FixtureExe('Win32')).Units do
    Units.Add(Entry);
  TDptExeAnalyzeTask.SortUnits(Units, esbVA);

  Assert.IsTrue(Units.Count > 0);
  Prev := 0;
  for var Entry in Units do
  begin
    Assert.IsTrue(Entry.StartVA >= Prev,
      Format('VA order broken at %s ($%x < previous $%x)',
        [Entry.Name, Entry.StartVA, Prev]));
    Prev := Entry.StartVA;
  end;
end;

procedure TTestExeAnalyzer.SortByNameIsCaseInsensitiveAlphabetical;
var
  Prev : string;
  Units: IList<TExeUnitEntry>;
begin
  Units := Collections.NewPlainList<TExeUnitEntry>;
  for var Entry in TExeAnalyzer.Analyze(FixtureExe('Win32')).Units do
    Units.Add(Entry);
  TDptExeAnalyzeTask.SortUnits(Units, esbName);

  Prev := '';
  for var Entry in Units do
  begin
    if Prev <> '' then
      Assert.IsTrue(CompareText(Prev, Entry.Name) <= 0,
        Format('Name order broken: %s should not come before %s',
          [Prev, Entry.Name]));
    Prev := Entry.Name;
  end;
end;

procedure TTestExeAnalyzer.SortBySizeIsDescendingWithNameTieBreak;
var
  Prev : TExeUnitEntry;
  HavePrev: Boolean;
  Units: IList<TExeUnitEntry>;
begin
  Units := Collections.NewPlainList<TExeUnitEntry>;
  for var Entry in TExeAnalyzer.Analyze(FixtureExe('Win32')).Units do
    Units.Add(Entry);
  TDptExeAnalyzeTask.SortUnits(Units, esbSize);

  HavePrev := False;
  for var Entry in Units do
  begin
    if HavePrev then
    begin
      // Bigger first; on ties, name ascending.
      if Prev.CodeSize = Entry.CodeSize then
        Assert.IsTrue(CompareText(Prev.Name, Entry.Name) <= 0,
          Format('Tie-break broken at %s vs %s', [Prev.Name, Entry.Name]))
      else
        Assert.IsTrue(Prev.CodeSize >= Entry.CodeSize,
          Format('Size order broken: %s (%d) before %s (%d)',
            [Prev.Name, Prev.CodeSize, Entry.Name, Entry.CodeSize]));
    end;
    Prev := Entry;
    HavePrev := True;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestExeAnalyzer);

end.
