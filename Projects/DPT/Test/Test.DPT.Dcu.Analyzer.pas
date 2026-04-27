unit Test.DPT.Dcu.Analyzer;

interface

uses

  DUnitX.TestFramework,

  System.IOUtils,
  System.SysUtils,

  DPT.Dcu.Analyzer,
  DPT.Dcu.Types;

type

  [TestFixture]
  TTestDcuAnalyzer = class
  private
    function FixtureDcu(const ABaseName: string): string;
    function ContainsUnit(const AResult: TDcuAnalysisResult; const AUnitName: string;
      AScope: TDcuUsesScope): Boolean;
    function ContainsSymbol(const AResult: TDcuAnalysisResult; const AName: string;
      AKind: TDcuSymbolKind): Boolean;
    function ContainsExport(const AResult: TDcuAnalysisResult; const AName: string;
      AKind: TDcuSymbolKind): Boolean;
    function CountKind(const AResult: TDcuAnalysisResult;
      AKind: TDcuSymbolKind): Integer;
  public
    [Test] procedure RecognisesCommittedDetectionDcu;
    [Test] procedure RecognisesCommittedApplicationDcu;
    [Test] procedure ExtractsUnitNameFromSource;
    [Test] procedure ProducesHexPreview;
    [Test] procedure ReportsIsDcuFalseForArbitraryFile;
    [Test] procedure ReportsIsDcuFalseForTooSmallFile;
    [Test] procedure ReportsIsDcuFalseForMissingFile;
    [Test] procedure DiagnosticsContainEntryWhenSourceMissing;
    [Test] procedure DcuMagicMatchesAcrossSampledFixtures;

    // Iteration 2: full uses table extraction
    [Test] procedure DetectionDcuExposesExpectedInterfaceUses;
    [Test] procedure DetectionDcuExposesExpectedImplementationUses;
    [Test] procedure DetectionDcuDoesNotMixScopes;
    [Test] procedure ApplicationDcuFindsManyImplementationUses;
    [Test] procedure ApplicationDcuInterfaceUsesContainsKnownUnits;
    [Test] procedure UsesEntriesAreNonEmptyAndUnique;
    [Test] procedure UsesParsedFlagReflectsAtLeastOneEntry;

    // Compiler + platform identification from magic bytes
    [Test] procedure CommittedDcusAreIdentifiedAsDelphi13Win64;
    [Test] procedure UnknownMagicGivesUnknownCompilerAndPlatform;

    // Built-in type names must never appear as uses entries even when
    // their byte sequence happens to align with the $63 $64/$65 marker.
    [Test] procedure UsesEntriesNeverContainBuiltinTypeNames;

    // Iteration 4: imported-symbol cross-reference extraction
    [Test] procedure DetectionDcuExposesExpectedTypeReferences;
    [Test] procedure DetectionDcuExposesExpectedMethodReferences;
    [Test] procedure ApplicationDcuFindsManyTypeAndMethodReferences;
    [Test] procedure SymbolEntriesAreDeduplicatedByKindAndName;
    [Test] procedure SymbolNamesAreNonEmptyAndPrintable;
    [Test] procedure SymbolsParsedFlagReflectsAtLeastOneEntry;
    [Test] procedure NonDcuFileYieldsNoSymbols;

    // Iteration 5: search-path-aware uses resolution
    [Test] procedure ResolveUsesAgainstCommittedDcuDirectory;
    [Test] procedure ResolveUsesLeavesPathEmptyForMissingUnits;
    [Test] procedure ResolveUsesAutoAddsDcuDirectory;
    [Test] procedure ResolveUsesHonorsExplicitSearchPathsInOrder;
    [Test] procedure ResolveUsesIsNoOpWhenNotInvoked;

    // Iteration 7: own-declared (exported) symbols
    [Test] procedure DetectionDcuExportsKnownTypes;
    [Test] procedure DetectionDcuExportsKnownRoutines;
    [Test] procedure DetectionDcuExportsClassMethods;
    [Test] procedure ExportedAndImportedDoNotConflate;
    [Test] procedure UnitOwnNameIsNotReportedAsExportedRoutine;
  end;

implementation

uses

  System.Classes,

  mormot.core.collections;

{ TTestDcuAnalyzer }

function TTestDcuAnalyzer.FixtureDcu(const ABaseName: string): string;
begin
  Result := ExpandFileName('Projects\DPT\DCU\' + ABaseName + '.dcu');
  if not FileExists(Result) then
    Result := ExpandFileName('..\..\DCU\' + ABaseName + '.dcu');
  if not FileExists(Result) then
    Result := ExpandFileName('..\DCU\' + ABaseName + '.dcu');
  if not FileExists(Result) then
    Assert.Fail('Test fixture not found: ' + ABaseName + '.dcu - tried multiple paths');
end;

function TTestDcuAnalyzer.ContainsUnit(const AResult: TDcuAnalysisResult;
  const AUnitName: string; AScope: TDcuUsesScope): Boolean;
var
  Entry: TDcuUsesEntry;
begin
  Result := False;
  case AScope of
    dusInterface:
      for Entry in AResult.InterfaceUses do
        if SameText(Entry.UnitName, AUnitName) then Exit(True);
    dusImplementation:
      for Entry in AResult.ImplementationUses do
        if SameText(Entry.UnitName, AUnitName) then Exit(True);
  end;
end;

function TTestDcuAnalyzer.ContainsSymbol(const AResult: TDcuAnalysisResult;
  const AName: string; AKind: TDcuSymbolKind): Boolean;
var
  Sym: TDcuSymbolRef;
begin
  Result := False;
  for Sym in AResult.Symbols do
    if (Sym.Kind = AKind) and (Sym.Origin = dsoImported)
      and SameText(Sym.Name, AName) then
      Exit(True);
end;

function TTestDcuAnalyzer.ContainsExport(const AResult: TDcuAnalysisResult;
  const AName: string; AKind: TDcuSymbolKind): Boolean;
var
  Sym: TDcuSymbolRef;
begin
  Result := False;
  for Sym in AResult.Symbols do
    if (Sym.Kind = AKind) and (Sym.Origin = dsoExported)
      and SameText(Sym.Name, AName) then
      Exit(True);
end;

function TTestDcuAnalyzer.CountKind(const AResult: TDcuAnalysisResult;
  AKind: TDcuSymbolKind): Integer;
var
  Sym: TDcuSymbolRef;
begin
  Result := 0;
  for Sym in AResult.Symbols do
    if Sym.Kind = AKind then
      Inc(Result);
end;

procedure TTestDcuAnalyzer.RecognisesCommittedDetectionDcu;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsTrue(Res.IsDcu, 'Committed DPT.Detection.dcu must be recognised as a DCU');
  Assert.IsTrue(Res.FileSize > 16, 'File size should be > 16 bytes');
end;

procedure TTestDcuAnalyzer.RecognisesCommittedApplicationDcu;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Assert.IsTrue(Res.IsDcu);
end;

procedure TTestDcuAnalyzer.ExtractsUnitNameFromSource;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.AreEqual('DPT.Detection', Res.Header.UnitName);
  Assert.AreEqual('DPT.Detection.pas', Res.Header.PrimarySource.FileName);
end;

procedure TTestDcuAnalyzer.ProducesHexPreview;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsTrue(Length(Res.FirstBytesPreview) > 0, 'Preview should not be empty');
  Assert.IsTrue(Pos(' ', Res.FirstBytesPreview) > 0,
    'Hex preview must contain spaces between bytes');
end;

procedure TTestDcuAnalyzer.ReportsIsDcuFalseForArbitraryFile;
var
  Bytes: TBytes;
  I    : Integer;
  Res  : TDcuAnalysisResult;
begin
  SetLength(Bytes, 256);
  for I := 0 to High(Bytes) do
    Bytes[I] := Byte(I);
  Res := TDcuAnalyzer.Analyze(Bytes);
  Assert.IsFalse(Res.IsDcu, 'Random byte sequence must not be classified as DCU');
end;

procedure TTestDcuAnalyzer.ReportsIsDcuFalseForTooSmallFile;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(TBytes.Create($4D, $03, $00));
  Assert.IsFalse(Res.IsDcu);
  Assert.IsTrue(Res.Diagnostics.Count > 0);
end;

procedure TTestDcuAnalyzer.ReportsIsDcuFalseForMissingFile;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze('C:\No\Such\Path\xyz.dcu');
  Assert.IsFalse(Res.IsDcu);
  Assert.AreEqual(Int64(0), Res.FileSize);
end;

procedure TTestDcuAnalyzer.DiagnosticsContainEntryWhenSourceMissing;
var
  Bytes : TBytes;
  Res   : TDcuAnalysisResult;
  I     : Integer;
begin
  SetLength(Bytes, 64);
  Bytes[0] := $4D;
  for I := 1 to High(Bytes) do
    Bytes[I] := $FF;
  Res := TDcuAnalyzer.Analyze(Bytes);
  Assert.IsTrue(Res.IsDcu, 'Magic byte still classifies it as DCU');
  Assert.AreEqual('', Res.Header.UnitName);
  Assert.IsTrue(Res.Diagnostics.Count > 0,
    'Should diagnose missing source reference');
end;

procedure TTestDcuAnalyzer.DcuMagicMatchesAcrossSampledFixtures;
var
  ResA, ResB: TDcuAnalysisResult;
begin
  ResA := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  ResB := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Assert.AreEqual(ResA.Header.MagicHex, ResB.Header.MagicHex);
end;

procedure TTestDcuAnalyzer.DetectionDcuExposesExpectedInterfaceUses;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  // DPT.Detection's interface uses Winapi.TlHelp32, Winapi.Windows, DPT.Types.
  Assert.IsTrue(ContainsUnit(Res, 'Winapi.Windows', dusInterface),
    'Interface uses must contain Winapi.Windows');
  Assert.IsTrue(ContainsUnit(Res, 'Winapi.TlHelp32', dusInterface),
    'Interface uses must contain Winapi.TlHelp32');
  Assert.IsTrue(ContainsUnit(Res, 'DPT.Types', dusInterface),
    'Interface uses must contain DPT.Types');
end;

procedure TTestDcuAnalyzer.DetectionDcuExposesExpectedImplementationUses;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  // Implementation uses System.StrUtils, System.SysUtils, JclIDEUtils.
  Assert.IsTrue(ContainsUnit(Res, 'System.StrUtils', dusImplementation));
  Assert.IsTrue(ContainsUnit(Res, 'System.SysUtils', dusImplementation));
  Assert.IsTrue(ContainsUnit(Res, 'JclIDEUtils', dusImplementation));
end;

procedure TTestDcuAnalyzer.DetectionDcuDoesNotMixScopes;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  // A unit listed in implementation must NOT also appear in the interface
  // uses list - the scope tag bytes are mutually exclusive.
  Assert.IsFalse(ContainsUnit(Res, 'System.StrUtils', dusInterface),
    'System.StrUtils is implementation-only');
  Assert.IsFalse(ContainsUnit(Res, 'JclIDEUtils', dusInterface),
    'JclIDEUtils is implementation-only');
  Assert.IsFalse(ContainsUnit(Res, 'Winapi.Windows', dusImplementation),
    'Winapi.Windows is interface-only');
end;

procedure TTestDcuAnalyzer.ApplicationDcuFindsManyImplementationUses;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  // DPT.Application's implementation uses many DPT.* task units. The
  // count is a sanity check that the whole-file scan worked, not just a
  // header window.
  Assert.IsTrue(Res.ImplementationUses.Count >= 10,
    Format('Expected at least 10 implementation uses, got %d',
      [Res.ImplementationUses.Count]));
end;

procedure TTestDcuAnalyzer.ApplicationDcuInterfaceUsesContainsKnownUnits;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Assert.IsTrue(ContainsUnit(Res, 'DPT.Task', dusInterface));
  Assert.IsTrue(ContainsUnit(Res, 'DPT.Types', dusInterface));
  Assert.IsTrue(ContainsUnit(Res, 'DPT.Workflow', dusInterface));
  Assert.IsTrue(ContainsUnit(Res, 'Slim.CmdUtils', dusInterface));
end;

procedure TTestDcuAnalyzer.UsesEntriesAreNonEmptyAndUnique;
var
  Entry: TDcuUsesEntry;
  Res  : TDcuAnalysisResult;
  Seen : IKeyValue<string, Boolean>;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Seen := Collections.NewPlainKeyValue<string, Boolean>;
  for Entry in Res.InterfaceUses do
  begin
    Assert.IsTrue(Length(Entry.UnitName) > 0, 'Empty unit name in interface uses');
    Assert.IsFalse(Seen.ContainsKey('I:' + LowerCase(Entry.UnitName)),
      'Duplicate interface uses entry: ' + Entry.UnitName);
    Seen.Add('I:' + LowerCase(Entry.UnitName), True);
  end;
  for Entry in Res.ImplementationUses do
  begin
    Assert.IsTrue(Length(Entry.UnitName) > 0, 'Empty unit name in implementation uses');
    Assert.IsFalse(Seen.ContainsKey('M:' + LowerCase(Entry.UnitName)),
      'Duplicate implementation uses entry: ' + Entry.UnitName);
    Seen.Add('M:' + LowerCase(Entry.UnitName), True);
  end;
end;

procedure TTestDcuAnalyzer.UsesParsedFlagReflectsAtLeastOneEntry;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsTrue(Res.UsesParsed, 'UsesParsed should be true when entries exist');
end;

procedure TTestDcuAnalyzer.CommittedDcusAreIdentifiedAsDelphi13Win64;
var
  Res: TDcuAnalysisResult;
begin
  // The DCU/ directory is regenerated whenever DPT itself is built. The
  // currently active compiler is Delphi 13 producing Win64 binaries, so
  // every committed DCU must come back with that identification.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.AreEqual(Ord(dccDelphi13), Ord(Res.Header.DetectedCompiler),
    'Committed DPT.Detection.dcu should be identified as Delphi 13');
  Assert.AreEqual(Ord(dpWin64), Ord(Res.Header.DetectedPlatform),
    'Committed DPT.Detection.dcu should be identified as Win64');

  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Assert.AreEqual(Ord(dccDelphi13), Ord(Res.Header.DetectedCompiler));
  Assert.AreEqual(Ord(dpWin64), Ord(Res.Header.DetectedPlatform));
end;

procedure TTestDcuAnalyzer.UsesEntriesNeverContainBuiltinTypeNames;
var
  Res: TDcuAnalysisResult;

  procedure AssertScopeIsClean(const AScopeLabel: string);
  var
    Entry  : TDcuUsesEntry;
    Builtin: string;
  begin
    for Entry in Res.InterfaceUses do
      for Builtin in DcuBuiltinNonUnitNames do
        Assert.IsFalse(SameText(Builtin, Entry.UnitName),
          Format('%s interface uses must not contain built-in type "%s"',
            [AScopeLabel, Entry.UnitName]));
    for Entry in Res.ImplementationUses do
      for Builtin in DcuBuiltinNonUnitNames do
        Assert.IsFalse(SameText(Builtin, Entry.UnitName),
          Format('%s implementation uses must not contain built-in type "%s"',
            [AScopeLabel, Entry.UnitName]));
  end;

begin
  // Both fixtures together exercise both committed DCUs. The bigger one
  // has more symbols, so accidental alignment for short type names is
  // more likely - the assertion guards against any of them slipping in.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  AssertScopeIsClean('DPT.Application');

  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  AssertScopeIsClean('DPT.Detection');
end;

procedure TTestDcuAnalyzer.UnknownMagicGivesUnknownCompilerAndPlatform;
var
  Bytes: TBytes;
  Res  : TDcuAnalysisResult;
  I    : Integer;
begin
  // A made-up byte stream that begins with the DCU magic byte but uses
  // platform/version bytes the analyzer has never seen must yield
  // dccUnknown/dpUnknown rather than be force-mapped to a known
  // compiler.
  SetLength(Bytes, 32);
  Bytes[0] := $4D;     // valid DCU magic byte
  Bytes[1] := $7E;     // unknown platform byte
  Bytes[2] := $00;
  Bytes[3] := $7F;     // unknown compiler-version byte
  for I := 4 to High(Bytes) do
    Bytes[I] := 0;

  Res := TDcuAnalyzer.Analyze(Bytes);
  Assert.IsTrue(Res.IsDcu);
  Assert.AreEqual(Ord(dccUnknown), Ord(Res.Header.DetectedCompiler));
  Assert.AreEqual(Ord(dpUnknown), Ord(Res.Header.DetectedPlatform));
end;

procedure TTestDcuAnalyzer.DetectionDcuExposesExpectedTypeReferences;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  // DPT.Detection imports Winapi.Windows / TlHelp32 / DPT.Types and
  // therefore must reference these well-known types.
  Assert.IsTrue(ContainsSymbol(Res, 'TObject', dskType),
    'TObject must appear as type reference');
  Assert.IsTrue(ContainsSymbol(Res, 'string', dskType),
    'string must appear as type reference');
  Assert.IsTrue(ContainsSymbol(Res, 'Boolean', dskType),
    'Boolean must appear as type reference');
  Assert.IsTrue(ContainsSymbol(Res, 'Integer', dskType),
    'Integer must appear as type reference');
  Assert.IsTrue(ContainsSymbol(Res, 'TProcessEntry32', dskType),
    'TProcessEntry32 from Winapi.TlHelp32 must appear as type reference');
  Assert.IsTrue(ContainsSymbol(Res, 'THandle', dskType),
    'THandle must appear as type reference');
  Assert.IsTrue(ContainsSymbol(Res, 'TDelphiVersion', dskType),
    'TDelphiVersion from DPT.Types must appear as type reference');
  Assert.IsTrue(ContainsSymbol(Res, 'TJclBorRADToolInstallations', dskType),
    'TJclBorRADToolInstallations must appear as type reference');
end;

procedure TTestDcuAnalyzer.DetectionDcuExposesExpectedMethodReferences;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  // The unit calls Winapi/JCL/SysUtils functions and references TObject
  // virtuals; the symbol scanner classifies all of these as $67.
  Assert.IsTrue(ContainsSymbol(Res, 'CreateToolhelp32Snapshot', dskMethod),
    'CreateToolhelp32Snapshot must appear as method reference');
  Assert.IsTrue(ContainsSymbol(Res, 'CloseHandle', dskMethod),
    'CloseHandle must appear as method reference');
  Assert.IsTrue(ContainsSymbol(Res, 'SameText', dskMethod),
    'SameText must appear as method reference');
  Assert.IsTrue(ContainsSymbol(Res, 'TObject.Equals', dskMethod),
    'TObject.Equals must appear as method reference');
  Assert.IsTrue(ContainsSymbol(Res, 'TJclBorRADToolInstallations.Create', dskMethod),
    'TJclBorRADToolInstallations.Create must appear as method reference');
end;

procedure TTestDcuAnalyzer.ApplicationDcuFindsManyTypeAndMethodReferences;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  // DPT.Application is a sizable unit that uses the full DPT.* tree
  // plus mORMot collections; both reference categories must be richly
  // populated.
  Assert.IsTrue(CountKind(Res, dskType) >= 30,
    Format('Expected at least 30 type references, got %d',
      [CountKind(Res, dskType)]));
  Assert.IsTrue(CountKind(Res, dskMethod) >= 50,
    Format('Expected at least 50 method references, got %d',
      [CountKind(Res, dskMethod)]));
end;

procedure TTestDcuAnalyzer.SymbolEntriesAreDeduplicatedByKindAndName;
var
  Res : TDcuAnalysisResult;
  Seen: IKeyValue<string, Boolean>;
  Sym : TDcuSymbolRef;
  Key : string;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Seen := Collections.NewPlainKeyValue<string, Boolean>;
  for Sym in Res.Symbols do
  begin
    Key := IntToStr(Ord(Sym.Kind)) + ':' + LowerCase(Sym.Name);
    Assert.IsFalse(Seen.ContainsKey(Key),
      'Duplicate symbol entry: ' + Sym.Name);
    Seen.Add(Key, True);
  end;
end;

procedure TTestDcuAnalyzer.SymbolNamesAreNonEmptyAndPrintable;
var
  C  : Char;
  Res: TDcuAnalysisResult;
  Sym: TDcuSymbolRef;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  for Sym in Res.Symbols do
  begin
    Assert.IsTrue(Length(Sym.Name) > 0, 'Empty symbol name found');
    for C in Sym.Name do
      Assert.IsTrue((Ord(C) >= 32) and (Ord(C) <= 126),
        'Non-printable byte in symbol name: ' + Sym.Name);
  end;
end;

procedure TTestDcuAnalyzer.SymbolsParsedFlagReflectsAtLeastOneEntry;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsTrue(Res.SymbolsParsed,
    'SymbolsParsed should be true when entries exist');
  Assert.IsTrue(Res.Symbols.Count > 0,
    'A real DCU must yield at least one imported symbol reference');
end;

procedure TTestDcuAnalyzer.NonDcuFileYieldsNoSymbols;
var
  Bytes: TBytes;
  Res  : TDcuAnalysisResult;
begin
  // Below the minimum-DCU-size guard. The analyzer must early-exit
  // and leave the Symbols list empty.
  SetLength(Bytes, 8);
  Res := TDcuAnalyzer.Analyze(Bytes);
  Assert.IsFalse(Res.IsDcu);
  Assert.AreEqual(0, Res.Symbols.Count);
  Assert.IsFalse(Res.SymbolsParsed);
end;

procedure TTestDcuAnalyzer.ResolveUsesAgainstCommittedDcuDirectory;
var
  DcuDir : string;
  Found  : Boolean;
  Entry  : TDcuUsesEntry;
  Res    : TDcuAnalysisResult;
begin
  // The committed DCU directory contains all DPT.* peer DCUs as well
  // as JclIDEUtils. Resolving against that directory must turn at least
  // those peers into populated ResolvedPath values.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  DcuDir := ExtractFilePath(FixtureDcu('DPT.Detection'));
  TDcuAnalyzer.ResolveUses(Res, [ExcludeTrailingPathDelimiter(DcuDir)]);

  Found := False;
  for Entry in Res.InterfaceUses do
    if SameText(Entry.UnitName, 'DPT.Types') then
    begin
      Assert.IsTrue(Entry.ResolvedPath <> '',
        'DPT.Types must resolve when its DCU sits next to DPT.Detection.dcu');
      Assert.IsTrue(FileExists(Entry.ResolvedPath),
        'ResolvedPath must point to an existing file');
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found, 'DPT.Types must appear in the interface uses');

  Found := False;
  for Entry in Res.ImplementationUses do
    if SameText(Entry.UnitName, 'JclIDEUtils') then
    begin
      Assert.IsTrue(Entry.ResolvedPath <> '',
        'JclIDEUtils must resolve in the committed DCU directory');
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found, 'JclIDEUtils must appear in the implementation uses');
end;

procedure TTestDcuAnalyzer.ResolveUsesLeavesPathEmptyForMissingUnits;
var
  Entry  : TDcuUsesEntry;
  Res    : TDcuAnalysisResult;
  TempDir: string;
begin
  // An empty temp directory ensures nothing can be resolved. Every
  // entry must come back with ResolvedPath = ''.
  TempDir := TPath.Combine(TPath.GetTempPath,
    'DPT.ResolveTest.' + GUIDToString(TGUID.NewGuid));
  ForceDirectories(TempDir);
  try
    Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
    TDcuAnalyzer.ResolveUses(Res, [TempDir]);
    // FilePath of DPT.Detection.dcu still adds its own directory, so
    // peer DCUs from there will resolve - but System.SysUtils etc. are
    // not in either location.
    for Entry in Res.ImplementationUses do
      if SameText(Entry.UnitName, 'System.StrUtils') then
      begin
        Assert.AreEqual('', Entry.ResolvedPath,
          'System.StrUtils must remain unresolved without RTL search path');
        Exit;
      end;
    Assert.Fail('System.StrUtils not found in implementation uses');
  finally
    if DirectoryExists(TempDir) then TDirectory.Delete(TempDir, True);
  end;
end;

procedure TTestDcuAnalyzer.ResolveUsesAutoAddsDcuDirectory;
var
  Entry: TDcuUsesEntry;
  Res  : TDcuAnalysisResult;
  Found: Boolean;
begin
  // Calling ResolveUses with an empty search-path array still has to
  // resolve peer DCUs because the analysed DCU's own directory is
  // implicitly prepended.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  TDcuAnalyzer.ResolveUses(Res, []);
  Found := False;
  for Entry in Res.InterfaceUses do
    if SameText(Entry.UnitName, 'DPT.Types') and (Entry.ResolvedPath <> '') then
    begin
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found,
    'Auto-added DCU directory must let peer DPT.Types resolve');
end;

procedure TTestDcuAnalyzer.ResolveUsesHonorsExplicitSearchPathsInOrder;
var
  Entry  : TDcuUsesEntry;
  Higher : string;
  Lower  : string;
  Res    : TDcuAnalysisResult;
begin
  // Build two temp dirs with a fake "Foo.dcu" in each. The auto-added
  // DCU directory does not contain it, so the resolver picks the first
  // explicit search-path hit - in our list "Higher" comes before
  // "Lower" so it must win.
  Higher := TPath.Combine(TPath.GetTempPath,
    'DPT.ResolveTestHigh.' + GUIDToString(TGUID.NewGuid));
  Lower := TPath.Combine(TPath.GetTempPath,
    'DPT.ResolveTestLow.' + GUIDToString(TGUID.NewGuid));
  ForceDirectories(Higher);
  ForceDirectories(Lower);
  try
    TFile.WriteAllText(TPath.Combine(Higher, 'Foo.dcu'), 'higher');
    TFile.WriteAllText(TPath.Combine(Lower, 'Foo.dcu'), 'lower');

    Res := Default(TDcuAnalysisResult);
    Res.FilePath := '';
    Res.InterfaceUses := Collections.NewPlainList<TDcuUsesEntry>;
    Res.ImplementationUses := Collections.NewPlainList<TDcuUsesEntry>;
    Res.InterfaceUses.Add(TDcuUsesEntry.Create('Foo', dusInterface, 0));

    TDcuAnalyzer.ResolveUses(Res, [Higher, Lower]);
    Entry := Res.InterfaceUses[0];
    Assert.AreEqual(
      ExcludeTrailingPathDelimiter(Higher) + PathDelim + 'Foo.dcu',
      Entry.ResolvedPath,
      'First search-path entry must win on a tie');
  finally
    if DirectoryExists(Higher) then TDirectory.Delete(Higher, True);
    if DirectoryExists(Lower) then TDirectory.Delete(Lower, True);
  end;
end;

procedure TTestDcuAnalyzer.ResolveUsesIsNoOpWhenNotInvoked;
var
  Entry: TDcuUsesEntry;
  Res  : TDcuAnalysisResult;
begin
  // Plain Analyze must not resolve anything; ResolvedPath stays empty
  // until the caller opts in via ResolveUses.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  for Entry in Res.InterfaceUses do
    Assert.AreEqual('', Entry.ResolvedPath,
      Format('Entry %s should not be auto-resolved', [Entry.UnitName]));
  for Entry in Res.ImplementationUses do
    Assert.AreEqual('', Entry.ResolvedPath,
      Format('Entry %s should not be auto-resolved', [Entry.UnitName]));
end;

procedure TTestDcuAnalyzer.DetectionDcuExportsKnownTypes;
var
  Res: TDcuAnalysisResult;
begin
  // DPT.Detection declares exactly one type: TProcessTreeScanner.
  // Other types referenced (TDelphiVersion, TAIMode, TObject, ...) are
  // imported from System / DPT.Types and must not appear as exports.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsTrue(ContainsExport(Res, 'TProcessTreeScanner', dskType),
    'TProcessTreeScanner must be reported as an exported type');
  Assert.IsFalse(ContainsExport(Res, 'TDelphiVersion', dskType),
    'TDelphiVersion is imported from DPT.Types, not exported here');
  Assert.IsFalse(ContainsExport(Res, 'TObject', dskType),
    'TObject is imported from System, not exported here');
end;

procedure TTestDcuAnalyzer.DetectionDcuExportsKnownRoutines;
var
  Res: TDcuAnalysisResult;
begin
  // Standalone routines declared in DPT.Detection's interface section.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsTrue(ContainsExport(Res, 'FindMostRecentDelphiVersion', dskMethod));
  Assert.IsTrue(ContainsExport(Res, 'IsValidDelphiVersion', dskMethod));
  Assert.IsTrue(ContainsExport(Res, 'IsLatestVersionAlias', dskMethod));
  Assert.IsTrue(ContainsExport(Res, 'DetectAIMode', dskMethod));
end;

procedure TTestDcuAnalyzer.DetectionDcuExportsClassMethods;
var
  Res: TDcuAnalysisResult;
begin
  // Methods of the declared TProcessTreeScanner class - emitted with
  // qualified names by the compiler.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsTrue(ContainsExport(Res, 'TProcessTreeScanner.Create', dskMethod));
  Assert.IsTrue(ContainsExport(Res, 'TProcessTreeScanner.Destroy', dskMethod));
  Assert.IsTrue(ContainsExport(Res, 'TProcessTreeScanner.GetProcessEntry', dskMethod));
  Assert.IsTrue(ContainsExport(Res, 'TProcessTreeScanner.DetectAIMode', dskMethod));
end;

procedure TTestDcuAnalyzer.ExportedAndImportedDoNotConflate;
var
  ExpFound, ImpFound: Boolean;
  Res                : TDcuAnalysisResult;
  Sym                : TDcuSymbolRef;
begin
  // TObject is imported (System.TObject); the analyzer must classify
  // it under dsoImported only - never as both.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  ExpFound := False;
  ImpFound := False;
  for Sym in Res.Symbols do
    if SameText(Sym.Name, 'TObject') and (Sym.Kind = dskType) then
      case Sym.Origin of
        dsoImported: ImpFound := True;
        dsoExported: ExpFound := True;
      end;
  Assert.IsTrue(ImpFound, 'TObject must appear as imported');
  Assert.IsFalse(ExpFound, 'TObject must NOT also appear as exported');
end;

procedure TTestDcuAnalyzer.UnitOwnNameIsNotReportedAsExportedRoutine;
var
  Res: TDcuAnalysisResult;
begin
  // The compiler emits a $28 sentinel entry whose name equals the unit's
  // own qualified name; the analyzer's bookkeeping filter must drop it
  // so the export list stays clean.
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsFalse(ContainsExport(Res, 'DPT.Detection', dskMethod),
    'Unit own name must not appear as an exported routine');
  Assert.IsFalse(ContainsExport(Res, 'Initialization', dskMethod),
    'Initialization sentinel must be filtered out');
  Assert.IsFalse(ContainsExport(Res, 'Finalization', dskMethod),
    'Finalization sentinel must be filtered out');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDcuAnalyzer);

end.
