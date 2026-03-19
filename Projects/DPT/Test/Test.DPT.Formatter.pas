unit Test.DPT.Formatter;

interface

uses
  System.Classes,
  System.SysUtils,
  DUnitX.TestFramework,
  ParseTree.Core, ParseTree.Nodes, ParseTree.Parser, ParseTree.Writer,
  DPT.Formatter;

type
  [TestFixture]
  TDptFormatterTests = class
  private
    FParser: TParseTreeParser;
    FWriter: TSyntaxTreeWriter;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestClearTrivia;
    [Test]
    procedure TestAddLeadingTrivia;
    [Test]
    procedure TestAddTrailingTrivia;
  end;

implementation

{ TDptFormatterTests }

procedure TDptFormatterTests.Setup;
begin
  FParser := TParseTreeParser.Create;
  FWriter := TSyntaxTreeWriter.Create;
end;

procedure TDptFormatterTests.TearDown;
begin
  FWriter.Free;
  FParser.Free;
end;

procedure TDptFormatterTests.TestClearTrivia;
var
  LUnit: TCompilationUnitSyntax;
  LSource, LResult: string;
begin
  LSource := '  unit MyUnit; interface end.';
  LUnit := FParser.Parse(LSource);
  try
    Assert.IsNotNull(LUnit);
    TDptFormatter.ClearTrivia(LUnit.UnitKeyword);
    LResult := FWriter.GenerateSource(LUnit);
    Assert.IsTrue(LResult.StartsWith('unit'), 'Must clear space before unit');
  finally
    LUnit.Free;
  end;
end;

procedure TDptFormatterTests.TestAddLeadingTrivia;
var
  LUnit: TCompilationUnitSyntax;
  LSource, LResult: string;
begin
  LSource := 'unit MyUnit; interface end.';
  LUnit := FParser.Parse(LSource);
  try
    TDptFormatter.AddLeadingTrivia(LUnit.UnitKeyword, '// header' + sLineBreak);
    LResult := FWriter.GenerateSource(LUnit);
    Assert.IsTrue(LResult.StartsWith('// header' + sLineBreak + 'unit'), 'Must add leading trivia');
  finally
    LUnit.Free;
  end;
end;

procedure TDptFormatterTests.TestAddTrailingTrivia;
var
  LUnit: TCompilationUnitSyntax;
  LSource, LResult: string;
begin
  LSource := 'unit MyUnit; interface end.';
  LUnit := FParser.Parse(LSource);
  try
    TDptFormatter.ClearTrivia(LUnit.UnitKeyword);
    TDptFormatter.AddTrailingTrivia(LUnit.UnitKeyword, '  ');
    LResult := FWriter.GenerateSource(LUnit);
    Assert.IsTrue(LResult.StartsWith('unit   MyUnit'), 'Must add trailing trivia space');
  finally
    LUnit.Free;
  end;
end;

end.
