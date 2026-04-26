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
  end;

implementation

uses

  System.Generics.Collections;

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
    if (Sym.Kind = AKind) and SameText(Sym.Name, AName) then
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
  Seen : TDictionary<string, Integer>;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Seen := TDictionary<string, Integer>.Create;
  try
    for Entry in Res.InterfaceUses do
    begin
      Assert.IsTrue(Length(Entry.UnitName) > 0, 'Empty unit name in interface uses');
      Assert.IsFalse(Seen.ContainsKey('I:' + LowerCase(Entry.UnitName)),
        'Duplicate interface uses entry: ' + Entry.UnitName);
      Seen.Add('I:' + LowerCase(Entry.UnitName), 1);
    end;
    for Entry in Res.ImplementationUses do
    begin
      Assert.IsTrue(Length(Entry.UnitName) > 0, 'Empty unit name in implementation uses');
      Assert.IsFalse(Seen.ContainsKey('M:' + LowerCase(Entry.UnitName)),
        'Duplicate implementation uses entry: ' + Entry.UnitName);
      Seen.Add('M:' + LowerCase(Entry.UnitName), 1);
    end;
  finally
    Seen.Free;
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
  Seen: TDictionary<string, Integer>;
  Sym : TDcuSymbolRef;
  Key : string;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Seen := TDictionary<string, Integer>.Create;
  try
    for Sym in Res.Symbols do
    begin
      Key := IntToStr(Ord(Sym.Kind)) + ':' + LowerCase(Sym.Name);
      Assert.IsFalse(Seen.ContainsKey(Key),
        'Duplicate symbol entry: ' + Sym.Name);
      Seen.Add(Key, 1);
    end;
  finally
    Seen.Free;
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

initialization
  TDUnitX.RegisterTestFixture(TTestDcuAnalyzer);

end.
