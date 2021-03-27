unit StringTools.Test;

interface

uses
  DUnitX.TestFramework,
  System.Classes,

  WDDT.StringTools;

type
  [TestFixture]
  TestCompareStringNaturalFunction = class
  public
    [Test]
    [TestCase('TestA','9,8,7,6,5,4,3,2,1|1,2,3,4,5,6,7,8,9', '|')]
    [TestCase('TestB','"Level: 10","Level: 1","Level: 5"|"Level: 1","Level: 5","Level: 10"', '|')]
    [TestCase('TestC','"Level: 10 C","Level: 1","Level: 10 B","Level: 10 A","Level: 5","Level: 2"|"Level: 1","Level: 2","Level: 5","Level: 10 A","Level: 10 B","Level: 10 C"', '|')]
    procedure Samples(const SortCommaText, ExpectedCommaText: string);
  end;

implementation

function CompareStringNaturalSL(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStringNatural(List[Index1], List[Index2]);
end;

{ TestCompareStringNaturalFunction }

procedure TestCompareStringNaturalFunction.Samples(const SortCommaText, ExpectedCommaText: string);

  function CreateStrings(const CommaText: string): TStringList;
  begin
    Result := TStringList.Create;
    Result.CommaText := CommaText;
  end;

var
  SourceSL, ExpectedSL: TStringList;
begin
  SourceSL := nil;
  ExpectedSL := nil;
  try
    SourceSL := CreateStrings(SortCommaText);
    ExpectedSL := CreateStrings(ExpectedCommaText);

    SourceSL.CustomSort(CompareStringNaturalSL);

    Assert.AreEqual(ExpectedSL, SourceSL);
  finally
    SourceSL.Free;
    ExpectedSL.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TestCompareStringNaturalFunction);

end.
