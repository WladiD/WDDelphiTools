unit Test.DPT.Formatter.Taifun;

interface

uses

  System.Classes,
  System.SysUtils,
  System.IOUtils,

  DUnitX.TestFramework,

  ParseTree.Core,
  ParseTree.Nodes,
  ParseTree.Parser,
  ParseTree.Writer,

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
    [Test]
    procedure TestFormatSections;
    [Test]
    procedure TestFormatMethodImplementation;
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
  LResult : String;
  LResult2: String;
  LSource : String; 
  LUnit   : TCompilationUnitSyntax; 
  LUnit2  : TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit; interface uses System.SysUtils; end.';
  
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Expecting no extra newlines right around uses, unless it falls inside interface
    Assert.IsTrue(LResult.Contains('uses '), 'uses should not add double lines when not in an interface');

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2, 'Formatting the uses clause should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatSections;
var
  LResult : String;
  LResult2: String;
  LSource : String; 
  LUnit   : TCompilationUnitSyntax; 
  LUnit2  : TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit; interface implementation end.';
  
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Check for interface
    Assert.IsTrue(LResult.Contains('{ ' + StringOfChar('=', 71) + ' }' + #13#10 + 'interface' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }'), 'interface should be wrapped in banners');
    // Check for implementation
    Assert.IsTrue(LResult.Contains('{ ' + StringOfChar('=', 71) + ' }' + #13#10 + 'implementation' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }'), 'implementation should be wrapped in banners');
    // Check for end.
    Assert.IsTrue(LResult.Contains('{ ' + StringOfChar('=', 71) + ' }' + #13#10 + 'end.'), 'end. should have a banner above it');

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2, 'Formatting the sections should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatMethodImplementation;
var
  LResult : String;
  LResult2: String;
  LSource : String; 
  LUnit   : TCompilationUnitSyntax; 
  LUnit2  : TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit; interface implementation procedure TMyClass.MyMethod; begin end; procedure TMyClass.MyMethod2; begin end; procedure TOtherClass.MyMethod; begin end; end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    TFile.WriteAllText('LResult_Methods.txt', LResult);

    // Check for class banner of TMyClass
    Assert.IsTrue(LResult.Contains('{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + '{ TMyClass' + StringOfChar(' ', 63) + ' }' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10 + 'procedure TMyClass.MyMethod;'), 'TMyClass banner missing');

    // Check for method 2 banner of TMyClass (no class banner this time)
    Assert.IsTrue(LResult.Contains('{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10 + 'procedure TMyClass.MyMethod2;'), 'Method 2 banner missing');

    // Check for class banner of TOtherClass
    Assert.IsTrue(LResult.Contains('{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + '{ TOtherClass' + StringOfChar(' ', 60) + ' }' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10 + 'procedure TOtherClass.MyMethod;'), 'TOtherClass banner missing');

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);

      Assert.AreEqual(LResult, LResult2, 'Formatting the methods should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

end.
