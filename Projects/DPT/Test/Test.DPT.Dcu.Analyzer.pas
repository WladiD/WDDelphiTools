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
  public
    [Test] procedure RecognisesCommittedDetectionDcu;
    [Test] procedure RecognisesCommittedApplicationDcu;
    [Test] procedure ExtractsUnitNameFromSource;
    [Test] procedure FindsSystemAsFirstUsesEntry;
    [Test] procedure ProducesHexPreview;
    [Test] procedure ReportsIsDcuFalseForArbitraryFile;
    [Test] procedure ReportsIsDcuFalseForTooSmallFile;
    [Test] procedure ReportsIsDcuFalseForMissingFile;
    [Test] procedure DiagnosticsContainEntryWhenSourceMissing;
    [Test] procedure InterfaceUsesEntriesAreNonEmpty;
    [Test] procedure DcuMagicMatchesAcrossSampledFixtures;
  end;

implementation

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

procedure TTestDcuAnalyzer.FindsSystemAsFirstUsesEntry;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsTrue(Res.UsesParsed, 'Uses table must be parsed');
  Assert.IsTrue(Res.InterfaceUses.Count > 0, 'Uses table must have at least one entry');
  Assert.AreEqual('System', Res.InterfaceUses[0].UnitName,
    'First interface uses entry is conventionally System');
end;

procedure TTestDcuAnalyzer.ProducesHexPreview;
var
  Res: TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  Assert.IsTrue(Length(Res.FirstBytesPreview) > 0, 'Preview should not be empty');
  // Preview should contain space-separated hex pairs
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
  Assert.IsTrue(Res.Diagnostics.Count > 0,
    'Diagnostics should explain why the file was rejected');
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
  // Magic byte present, but no source filename anywhere
  SetLength(Bytes, 64);
  Bytes[0] := $4D;
  for I := 1 to High(Bytes) do
    Bytes[I] := $FF;
  Res := TDcuAnalyzer.Analyze(Bytes);
  Assert.IsTrue(Res.IsDcu, 'Magic byte still classifies it as DCU');
  Assert.AreEqual('', Res.Header.UnitName);
  Assert.IsTrue(Res.Diagnostics.Count > 0, 'Should diagnose missing source reference');
end;

procedure TTestDcuAnalyzer.InterfaceUsesEntriesAreNonEmpty;
var
  Entry: TDcuUsesEntry;
  Res  : TDcuAnalysisResult;
begin
  Res := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Assert.IsTrue(Res.UsesParsed);
  for Entry in Res.InterfaceUses do
    Assert.IsTrue(Length(Entry.UnitName) > 0,
      'No empty unit name should appear in the uses table');
end;

procedure TTestDcuAnalyzer.DcuMagicMatchesAcrossSampledFixtures;
var
  ResA, ResB: TDcuAnalysisResult;
begin
  // Both DCUs were produced by the same Delphi installation that built DPT,
  // so their magic must agree. This is a cheap regression check in case the
  // analyzer ever starts reading bytes from the wrong offsets.
  ResA := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Detection'));
  ResB := TDcuAnalyzer.Analyze(FixtureDcu('DPT.Application'));
  Assert.AreEqual(ResA.Header.MagicHex, ResB.Header.MagicHex);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDcuAnalyzer);

end.
