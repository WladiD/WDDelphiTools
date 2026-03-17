unit Test.DPT.Formatter.Dws;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  DUnitX.TestFramework,
  ParseTree.Core, ParseTree.Nodes, ParseTree.Parser, ParseTree.Writer,
  DPT.Formatter, DPT.Formatter.DWS;

type
  [TestFixture]
  TDptDwsFormatterTests = class
  private
    FParser: TParseTreeParser;
    FWriter: TSyntaxTreeWriter;
    FFormatter: TDptDwsFormatter;
    FScriptFile: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestDwsIntegration;
  end;

implementation

{ TDptDwsFormatterTests }

procedure TDptDwsFormatterTests.Setup;
begin
  FParser := TParseTreeParser.Create;
  FWriter := TSyntaxTreeWriter.Create;
  FFormatter := TDptDwsFormatter.Create;
  FScriptFile := TPath.Combine(TPath.GetTempPath, TGUID.NewGuid.ToString + '.pas');
end;

procedure TDptDwsFormatterTests.TearDown;
begin
  if TFile.Exists(FScriptFile) then
    TFile.Delete(FScriptFile);
  FFormatter.Free;
  FWriter.Free;
  FParser.Free;
end;

procedure TDptDwsFormatterTests.TestDwsIntegration;
var
  LUnit: TCompilationUnitSyntax;
  LSource, LResult, LScriptCache: string;
begin
  // A DWScript that modifies the AUses UsesKeyword
  LScriptCache :=
    'procedure OnVisitUsesClause(AUses: TUsesClauseSyntax);' + sLineBreak +
    'begin' + sLineBreak +
    '  ClearTrivia(GetUsesKeyword(AUses));' + sLineBreak +
    '  AddLeadingTrivia(GetUsesKeyword(AUses), ''// FORMATTED'' + #13#10);' + sLineBreak +
    '  AddTrailingTrivia(GetUsesKeyword(AUses), '' '');' + sLineBreak +
    'end;' + sLineBreak;

  TFile.WriteAllText(FScriptFile, LScriptCache);

  LSource := 'unit MyUnit; interface uses System.SysUtils; end.';
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptFile);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Check if the script correctly added the trivia
    Assert.IsTrue(LResult.Contains('// FORMATTED'), 'Script should add the // FORMATTED comment to uses');
  finally
    LUnit.Free;
  end;
end;

end.
