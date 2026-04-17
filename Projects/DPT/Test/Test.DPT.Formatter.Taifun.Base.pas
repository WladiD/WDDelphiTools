unit Test.DPT.Formatter.Taifun.Base;

interface

uses
  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  ParseTree.Nodes,
  ParseTree.Parser,
  ParseTree.Writer,

  DPT.Formatter.DWS;

type

  TTestTaifunFormatterBase = class
  protected
    FParser: TParseTreeParser;
    FWriter: TSyntaxTreeWriter;
    FFormatter: TDptDwsFormatter;
    FScriptPath: string;
    function FormatSource(const ASource: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

implementation

{ TTestTaifunFormatterBase }

function TTestTaifunFormatterBase.FormatSource(const ASource: string): string;
var
  LUnit: TCompilationUnitSyntax;
  LSecondPass: string;
begin
  FFormatter.LoadScript(FScriptPath);

  LUnit := FParser.Parse(ASource);
  try
    FFormatter.FormatUnit(LUnit);
    Result := FWriter.GenerateSource(LUnit);
  finally
    LUnit.Free;
  end;

  // Built-in idempotence check: formatting twice must produce the same result
  LUnit := FParser.Parse(Result);
  try
    FFormatter.FormatUnit(LUnit);
    LSecondPass := FWriter.GenerateSource(LUnit);
  finally
    LUnit.Free;
  end;
  Assert.AreEqual(Result, LSecondPass, 'Formatting must be idempotent');
end;

procedure TTestTaifunFormatterBase.Setup;
begin
  FParser := TParseTreeParser.Create;
  FWriter := TSyntaxTreeWriter.Create;
  FFormatter := TDptDwsFormatter.Create;

  // Navigate back to the Format folder to find the real script
  FScriptPath := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\Format\TaifunFormat.pas'));
end;

procedure TTestTaifunFormatterBase.TearDown;
begin
  FFormatter.Free;
  FWriter.Free;
  FParser.Free;
end;

end.
