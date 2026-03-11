unit Test.DPT.Formatter.Taifun;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  DUnitX.TestFramework,
  ParseTree.Core, ParseTree.Nodes, ParseTree.Parser, ParseTree.Writer,
  DPT.Formatter.DWS;

type
  [TestFixture]
  TTestTaifunFormatter = class
  private
    FParser: TParseTreeParser;
    FWriter: TSyntaxTreeWriter;
    FFormatter: TDptDwsFormatter;
    FScriptPath: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestFormatUsesClause;
  end;

implementation

{ TTestTaifunFormatter }

procedure TTestTaifunFormatter.Setup;
begin
  FParser := TParseTreeParser.Create;
  FWriter := TSyntaxTreeWriter.Create;
  FFormatter := TDptDwsFormatter.Create;
  
  // Navigate back to the Format folder to find the real script
  FScriptPath := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\Format\TaifunFormat.pas'));
end;

procedure TTestTaifunFormatter.TearDown;
begin
  FFormatter.Free;
  FWriter.Free;
  FParser.Free;
end;

procedure TTestTaifunFormatter.TestFormatUsesClause;
var
  LUnit: TCompilationUnitSyntax;
  LSource, LResult: string;
begin
  LSource := 'unit MyUnit; interface uses System.SysUtils; end.';
  
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Expecting newline before the uses keyword, and newline after uses keyword
    Assert.IsTrue(LResult.Contains(#13#10 + #13#10 + 'uses' + #13#10), 'uses should have blank lines around it according to TaifunFormat.pas');
  finally
    LUnit.Free;
  end;
end;

end.
