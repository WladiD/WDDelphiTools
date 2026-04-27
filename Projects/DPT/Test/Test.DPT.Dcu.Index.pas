unit Test.DPT.Dcu.Index;

interface

uses

  DUnitX.TestFramework,

  System.Classes,
  System.IOUtils,
  System.SysUtils,

  mormot.core.collections,

  DPT.Dcu.Index,
  DPT.Dcu.Index.Json,
  DPT.Dcu.Types;

type

  [TestFixture]
  TTestDcuIndex = class
  private
    function CommittedDcuDir: string;
    function MakeSyntheticEntry(const AUnitName: string;
      AKnownCompiler: TDcuKnownCompiler; APlatform: TDcuPlatform;
      const AInterfaceUses, AImplUses, ATypeRefs, AMethodRefs: array of string)
      : TDcuIndexEntry;
    function MakeSyntheticIndex(
      const AEntries: array of TDcuIndexEntry): TDcuIndex;
  public
    // Builder against the committed DCU corpus
    [Test] procedure BuildAgainstCommittedDirectoryIsNonEmpty;
    [Test] procedure BuildContainsExpectedKnownUnits;
    [Test] procedure BuildPopulatesPerCompilerCounts;
    [Test] procedure BuildReverseIndexMapsKnownImporters;
    [Test] procedure BuildOnEmptyDirYieldsEmptyIndex;
    [Test] procedure BuildPatternFilterRespected;

    // Query layer
    [Test] procedure QueryFindByUnitReturnsAllPlatformVariants;
    [Test] procedure QueryFindImportedByUsesReverseIndex;
    [Test] procedure QueryFindImportsOfUnionsBothScopes;
    [Test] procedure QueryFindReferencesScansBothKinds;
    [Test] procedure QueryReturnsEmptyForUnknownNames;

    // JSON round-trip
    [Test] procedure JsonRoundTripPreservesUnitsAndReverseIndex;
    [Test] procedure JsonReadFailsOnMalformedInput;
  end;

implementation

{ TTestDcuIndex }

function TTestDcuIndex.CommittedDcuDir: string;
begin
  Result := ExpandFileName('Projects\DPT\DCU');
  if not TDirectory.Exists(Result) then
    Result := ExpandFileName('..\..\DCU');
  if not TDirectory.Exists(Result) then
    Result := ExpandFileName('..\DCU');
  if not TDirectory.Exists(Result) then
    Assert.Fail('Committed DCU directory not found - tried multiple paths');
end;

function TTestDcuIndex.MakeSyntheticEntry(const AUnitName: string;
  AKnownCompiler: TDcuKnownCompiler; APlatform: TDcuPlatform;
  const AInterfaceUses, AImplUses, ATypeRefs, AMethodRefs: array of string)
  : TDcuIndexEntry;
var
  S: string;
begin
  Result := Default(TDcuIndexEntry);
  Result.UnitName := AUnitName;
  Result.FilePath := 'C:\Synth\' + AUnitName + '.dcu';
  Result.FileSize := 1024;
  Result.Magic := '4D 03 00 25';
  Result.Compiler := AKnownCompiler;
  Result.Platform := APlatform;
  Result.PrimarySource := AUnitName + '.pas';
  Result.Includes := Collections.NewList<string>;
  Result.InterfaceUses := Collections.NewList<string>;
  Result.ImplementationUses := Collections.NewList<string>;
  Result.TypeRefs := Collections.NewList<string>;
  Result.MethodRefs := Collections.NewList<string>;
  for S in AInterfaceUses do Result.InterfaceUses.Add(S);
  for S in AImplUses do Result.ImplementationUses.Add(S);
  for S in ATypeRefs do Result.TypeRefs.Add(S);
  for S in AMethodRefs do Result.MethodRefs.Add(S);
end;

function TTestDcuIndex.MakeSyntheticIndex(
  const AEntries: array of TDcuIndexEntry): TDcuIndex;
var
  Bucket : IList<string>;
  Entry  : TDcuIndexEntry;
  Imp    : string;
  LowKey : string;
begin
  Result := Default(TDcuIndex);
  Result.Schema := DcuIndexSchemaVersion;
  Result.RootDirs := Collections.NewList<string>;
  Result.Units := Collections.NewPlainList<TDcuIndexEntry>;
  Result.ReverseImportIndex := Collections.NewKeyValue<string, IList<string>>;
  for Entry in AEntries do
    Result.Units.Add(Entry);
  // Inline reverse-index build to mirror what the real Builder does.
  for Entry in Result.Units do
  begin
    for Imp in Entry.InterfaceUses do
    begin
      LowKey := LowerCase(Imp);
      if not Result.ReverseImportIndex.TryGetValue(LowKey, Bucket) then
      begin
        Bucket := Collections.NewList<string>;
        Result.ReverseImportIndex.Add(LowKey, Bucket);
      end;
      Bucket.Add(Entry.UnitName);
    end;
    for Imp in Entry.ImplementationUses do
    begin
      LowKey := LowerCase(Imp);
      if not Result.ReverseImportIndex.TryGetValue(LowKey, Bucket) then
      begin
        Bucket := Collections.NewList<string>;
        Result.ReverseImportIndex.Add(LowKey, Bucket);
      end;
      Bucket.Add(Entry.UnitName);
    end;
  end;
end;

procedure TTestDcuIndex.BuildAgainstCommittedDirectoryIsNonEmpty;
var
  Index: TDcuIndex;
begin
  Index := TDcuIndexBuilder.Build([CommittedDcuDir], True, True);
  Assert.IsTrue(Index.Units.Count > 0,
    'Committed DCU directory must produce at least one indexed unit');
end;

procedure TTestDcuIndex.BuildContainsExpectedKnownUnits;
var
  Found: Boolean;
  Index: TDcuIndex;
  Q    : TDcuIndexQuery;
  Wants: TArray<string>;
  W    : string;
begin
  Index := TDcuIndexBuilder.Build([CommittedDcuDir], True, True);
  Q := TDcuIndexQuery.Create(Index);
  try
    Wants := ['DPT.Detection', 'DPT.Application', 'DPT.Dcu.Analyzer',
              'mormot.core.collections'];
    for W in Wants do
    begin
      Found := Q.FindByUnit(W).Count > 0;
      Assert.IsTrue(Found, 'Expected indexed unit missing: ' + W);
    end;
  finally
    Q.Free;
  end;
end;

procedure TTestDcuIndex.BuildPopulatesPerCompilerCounts;
var
  Entry: TDcuIndexEntry;
  Index: TDcuIndex;
  Hit  : Integer;
begin
  Index := TDcuIndexBuilder.Build([CommittedDcuDir], True, True);
  Hit := 0;
  for Entry in Index.Units do
    if Entry.Compiler = dccDelphi13 then
      Inc(Hit);
  Assert.IsTrue(Hit > 0,
    'Committed DCUs are produced by Delphi 13 - at least some entries must be classified as such');
end;

procedure TTestDcuIndex.BuildReverseIndexMapsKnownImporters;
var
  Bucket : IList<string>;
  Found  : Boolean;
  Index  : TDcuIndex;
  Item   : string;
begin
  // mormot.core.collections is in the interface uses of DPT.Workflow,
  // DPT.Application etc. The reverse-index entry must list at least
  // one of them.
  Index := TDcuIndexBuilder.Build([CommittedDcuDir], True, True);
  Assert.IsTrue(Index.ReverseImportIndex.TryGetValue(
    'mormot.core.collections', Bucket),
    'Reverse-import key for mormot.core.collections must exist');
  Found := False;
  for Item in Bucket do
    if SameText(Item, 'DPT.Workflow') or SameText(Item, 'DPT.Application') then
    begin
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found,
    'At least one known importer of mormot.core.collections must be recorded');
end;

procedure TTestDcuIndex.BuildOnEmptyDirYieldsEmptyIndex;
var
  Index  : TDcuIndex;
  TempDir: string;
begin
  TempDir := TPath.Combine(TPath.GetTempPath,
    'DPT.IdxEmpty.' + GUIDToString(TGUID.NewGuid));
  ForceDirectories(TempDir);
  try
    Index := TDcuIndexBuilder.Build([TempDir], True, True);
    Assert.AreEqual(0, Index.Units.Count,
      'Empty directory must yield an index with zero entries');
    Assert.AreEqual(0, Index.ReverseImportIndex.Count,
      'Empty directory must yield an empty reverse index');
  finally
    if DirectoryExists(TempDir) then TDirectory.Delete(TempDir, True);
  end;
end;

procedure TTestDcuIndex.BuildPatternFilterRespected;
var
  Index  : TDcuIndex;
  TempDir: string;
begin
  // Pattern *.no_match must skip every committed DCU.
  TempDir := CommittedDcuDir;
  Index := TDcuIndexBuilder.Build([TempDir], True, True, '*.no_match');
  Assert.AreEqual(0, Index.Units.Count,
    'Pattern filter must keep non-matching files out of the index');
end;

procedure TTestDcuIndex.QueryFindByUnitReturnsAllPlatformVariants;
var
  Hits: IList<TDcuIndexEntry>;
  Idx : TDcuIndex;
  Q   : TDcuIndexQuery;
begin
  // Same unit name on two different platforms - both must come back.
  Idx := MakeSyntheticIndex([
    MakeSyntheticEntry('System.SysUtils', dccDelphi13, dpWin32,
      [], [], [], []),
    MakeSyntheticEntry('System.SysUtils', dccDelphi13, dpWin64,
      [], [], [], []),
    MakeSyntheticEntry('UnrelatedUnit', dccDelphi13, dpWin64, [], [], [], [])
  ]);
  Q := TDcuIndexQuery.Create(Idx);
  try
    Hits := Q.FindByUnit('System.SysUtils');
    Assert.AreEqual(2, Hits.Count,
      'FindByUnit must return all platform variants of the same unit');
  finally
    Q.Free;
  end;
end;

procedure TTestDcuIndex.QueryFindImportedByUsesReverseIndex;
var
  Hits: IList<string>;
  Idx : TDcuIndex;
  Q   : TDcuIndexQuery;
begin
  // Two units both importing TargetUnit, one not. Reverse lookup must
  // return exactly the two importers.
  Idx := MakeSyntheticIndex([
    MakeSyntheticEntry('Importer1', dccDelphi13, dpWin64,
      ['TargetUnit'], [], [], []),
    MakeSyntheticEntry('Importer2', dccDelphi13, dpWin64,
      [], ['TargetUnit'], [], []),
    MakeSyntheticEntry('Unrelated', dccDelphi13, dpWin64,
      ['Other'], [], [], []),
    MakeSyntheticEntry('TargetUnit', dccDelphi13, dpWin64, [], [], [], [])
  ]);
  Q := TDcuIndexQuery.Create(Idx);
  try
    Hits := Q.FindImportedBy('TargetUnit');
    Assert.AreEqual(2, Hits.Count, 'Two importers must be returned');
  finally
    Q.Free;
  end;
end;

procedure TTestDcuIndex.QueryFindImportsOfUnionsBothScopes;
var
  Hits: IList<string>;
  Idx : TDcuIndex;
  Q   : TDcuIndexQuery;
begin
  Idx := MakeSyntheticIndex([
    MakeSyntheticEntry('Subject', dccDelphi13, dpWin64,
      ['IfaceA', 'IfaceB'], ['ImplA'], [], [])
  ]);
  Q := TDcuIndexQuery.Create(Idx);
  try
    Hits := Q.FindImportsOf('Subject');
    Assert.AreEqual(3, Hits.Count,
      'FindImportsOf must union interface and implementation uses');
  finally
    Q.Free;
  end;
end;

procedure TTestDcuIndex.QueryFindReferencesScansBothKinds;
var
  Hits: IList<string>;
  Idx : TDcuIndex;
  Q   : TDcuIndexQuery;
begin
  // Same symbol referenced once as a type, once as a method -> 2 hits.
  Idx := MakeSyntheticIndex([
    MakeSyntheticEntry('TypeRefUser', dccDelphi13, dpWin64,
      [], [], ['TStringList'], []),
    MakeSyntheticEntry('MethodRefUser', dccDelphi13, dpWin64,
      [], [], [], ['TStringList']),
    MakeSyntheticEntry('NotARef', dccDelphi13, dpWin64,
      [], [], ['TFooBar'], ['DoFoo'])
  ]);
  Q := TDcuIndexQuery.Create(Idx);
  try
    Hits := Q.FindReferences('TStringList');
    Assert.AreEqual(2, Hits.Count,
      'Both kinds (type + method) must be scanned for references');
  finally
    Q.Free;
  end;
end;

procedure TTestDcuIndex.QueryReturnsEmptyForUnknownNames;
var
  Idx: TDcuIndex;
  Q  : TDcuIndexQuery;
begin
  Idx := MakeSyntheticIndex([
    MakeSyntheticEntry('Foo', dccDelphi13, dpWin64, [], [], [], [])
  ]);
  Q := TDcuIndexQuery.Create(Idx);
  try
    Assert.AreEqual(0, Q.FindByUnit('NoSuch').Count);
    Assert.AreEqual(0, Q.FindImportedBy('NoSuch').Count);
    Assert.AreEqual(0, Q.FindImportsOf('NoSuch').Count);
    Assert.AreEqual(0, Q.FindReferences('NoSuch').Count);
  finally
    Q.Free;
  end;
end;

procedure TTestDcuIndex.JsonRoundTripPreservesUnitsAndReverseIndex;
var
  Bucket   : IList<string>;
  Idx      : TDcuIndex;
  Json     : string;
  Loaded   : TDcuIndex;
  Original : TDcuIndexEntry;
  Round    : TDcuIndexEntry;
begin
  Idx := MakeSyntheticIndex([
    MakeSyntheticEntry('Foo', dccDelphi13, dpWin64,
      ['System.SysUtils'], ['System.Classes'],
      ['TStringList'], ['IntToStr']),
    MakeSyntheticEntry('Bar', dccDelphi12, dpWin32,
      ['Foo'], [], ['TFoo'], [])
  ]);

  Json := TDcuIndexJsonWriter.WriteToString(Idx);
  Loaded := TDcuIndexJsonReader.LoadFromString(Json);

  Assert.AreEqual(2, Loaded.Units.Count, 'Unit count must round-trip');
  Original := Idx.Units[0];
  Round := Loaded.Units[0];
  Assert.AreEqual(Original.UnitName, Round.UnitName);
  Assert.AreEqual(Ord(Original.Compiler), Ord(Round.Compiler),
    'Compiler enum must round-trip via name lookup');
  Assert.AreEqual(Ord(Original.Platform), Ord(Round.Platform));
  Assert.AreEqual(Original.InterfaceUses.Count, Round.InterfaceUses.Count);
  Assert.AreEqual('System.SysUtils', Round.InterfaceUses[0]);
  Assert.AreEqual('TStringList', Round.TypeRefs[0]);

  // Reverse-import index must round-trip.
  Assert.IsTrue(Loaded.ReverseImportIndex.TryGetValue('foo', Bucket),
    'Lower-cased reverse-import key must survive round-trip');
  Assert.AreEqual(1, Bucket.Count);
  Assert.AreEqual('Bar', Bucket[0]);
end;

procedure TTestDcuIndex.JsonReadFailsOnMalformedInput;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TDcuIndexJsonReader.LoadFromString('this is not json');
  except
    Raised := True;
  end;
  Assert.IsTrue(Raised, 'Malformed JSON input must raise');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDcuIndex);

end.
