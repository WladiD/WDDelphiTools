unit Test.DPT.Formatter.Taifun.Base;

interface

uses
  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,

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
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

implementation

{ TTestTaifunFormatterBase }

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
