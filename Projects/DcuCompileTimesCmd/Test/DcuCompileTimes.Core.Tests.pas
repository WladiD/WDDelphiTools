unit DcuCompileTimes.Core.Tests;

interface

uses

  System.SysUtils,
  DUnitX.TestFramework,
  DcuCompileTimes.Core;

type

  TMockFileProvider = class(TInterfacedObject, IFileProvider)
  private
    FFiles: TArray<TFileMeta>;
  protected // IFileProvider
    function GetFiles(const ASearchPath, ASearchMask: string): TArray<TFileMeta>;
  public
    procedure AddFileMeta(const APath: String; const ALastWriteTime: Int64);
  end;

  [TestFixture]
  TAnalyzerTests = class(TObject)
  public
    [Test]
    procedure SampleA;
  end;

implementation

const
  // Ticks per second = 10000 (TicksPerMillisecond) * 1000 (ms)
  TicksPerSecond = 10000000;

{ TMockFileProvider }

procedure TMockFileProvider.AddFileMeta(const APath: String; const ALastWriteTime: Int64);
begin
  var Index: Integer := Length(FFiles);
  SetLength(FFiles, Index + 1);
  FFiles[Index].Path := APath;
  FFiles[Index].LastWriteTime := ALastWriteTime;
end;

function TMockFileProvider.GetFiles(const ASearchPath, ASearchMask: string): TArray<TFileMeta>;
begin
  Result := FFiles;
end;

{ TAnalyzerTests }

procedure TAnalyzerTests.SampleA;
var
  Analyzer: TDcuAnalyzer;
begin
  var MockProviderObj: TMockFileProvider := TMockFileProvider.Create;
  MockProviderObj.AddFileMeta('C:\build\UnitC.dcu', TicksPerSecond * 30); // Newest file
  MockProviderObj.AddFileMeta('C:\build\UnitA.dcu', TicksPerSecond * 23); // Oldest file
  MockProviderObj.AddFileMeta('C:\build\UnitB.dcu', TicksPerSecond * 25); // Middle file
  var MockProvider: IFileProvider := MockProviderObj;

  Analyzer := TDcuAnalyzer.Create('C:\build', '*.dcu', MockProvider);
  try
    Analyzer.Execute;
    // The expected total time is the sum of the differences:
    // (TimeC - TimeB) + (TimeB - TimeA)
    // (30s - 25s) + (25s - 23s) = 5s + 2s = 7 seconds
    Assert.AreEqual(Int64(7 * TicksPerSecond), Analyzer.TotalTime, 'TotalTime should be the sum of all diffs');
    Assert.AreEqual('C:\build\UnitA.dcu', Analyzer.OldestFile.Path);
    Assert.AreEqual(3, Analyzer.Files.Count);
    Assert.AreEqual(Int64(5000), Analyzer.Files[0].Diff div TicksPerMillisecond);
    Assert.AreEqual(Int64(2000), Analyzer.Files[1].Diff div TicksPerMillisecond);
    Assert.AreEqual(Int64(0), Analyzer.Files[2].Diff);
  finally
    Analyzer.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TAnalyzerTests);

end.
