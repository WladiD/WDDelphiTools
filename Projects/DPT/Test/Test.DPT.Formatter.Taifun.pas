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
    procedure TestFormatSections_IdempotentWithResourceString;
    [Test]
    procedure TestFormatMethodImplementation;
    [Test]
    procedure TestFormatUnitHeader_CreatesNew;
    [Test]
    procedure TestFormatUnitHeader_CorrectsExisting;
    [Test]
    procedure TestFormatUnitHeader_PreservesPerfect;
    [Test]
    procedure TestNoRedundantSeparatorAfterImplementation;
    [Test]
    procedure TestFormatMethodImplementation_WithXmlDoc;
    [Test]
    procedure TestFormatMethodImplementation_NestedClass;
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
    TFile.WriteAllText('LResult_Uses.txt', LResult);
    
    // Expecting newline right around uses
    Assert.IsTrue(LResult.Contains('uses' + #13#10 + #13#10 + '  System.SysUtils;'), 'uses should be on its own line followed by an empty line');

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
    Assert.IsTrue(LResult.Contains(#13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + 'implementation' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }'), 'implementation should be wrapped in banners');
    // Check for end.
    Assert.IsTrue(LResult.Contains('{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10 + 'end.'), 'end. should have a banner above it');

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

procedure TTestTaifunFormatter.TestFormatSections_IdempotentWithResourceString;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit;' + #13#10 + 'interface' + #13#10 + 'implementation' + #13#10 + 'resourcestring' + #13#10 + '  SMyString = ''My String'';' + #13#10 + 'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    TFile.WriteAllText('LResult_Idempotent.txt', LResult);

    // Initial format should have one banner block after implementation
    Assert.IsTrue(LResult.Contains(
      #13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + 'implementation' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10 + 'resourcestring'
    ), 'Implementation should be followed by resourcestring with single banner correctly placed. Actual:' + #13#10 + LResult);

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      
      Assert.AreEqual(LResult, LResult2, 'Formatting implementation followed by resourcestring should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestNoRedundantSeparatorAfterImplementation;
var
  LResult: string;
  LSource: string;
  LUnit  : TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit; interface implementation procedure MyProc; begin end; end.';
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Check that there is NO simple separator '{ --- }' after implementation banner
    // The implementation footer has { === }\r\n\r\n
    // The procedure should follow immediately (possibly with its own newline)
    Assert.IsFalse(LResult.Contains('{ ' + StringOfChar('-', 71) + ' }'), 'Simple separator should not be present after implementation banner');
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
    Assert.IsTrue(LResult.Contains(#13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + '{ TMyClass' + StringOfChar(' ', 63) + ' }' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10 + 'procedure TMyClass.MyMethod;'), 'TMyClass banner missing');

    // Check for method 2 banner of TMyClass (no class banner this time)
    Assert.IsTrue(LResult.Contains(#13#10#13#10 + '{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10 + 'procedure TMyClass.MyMethod2;'), 'Method 2 banner missing');

    // Check for class banner of TOtherClass
    Assert.IsTrue(LResult.Contains(#13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + '{ TOtherClass' + StringOfChar(' ', 60) + ' }' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10 + 'procedure TOtherClass.MyMethod;'), 'TOtherClass banner missing');

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

procedure TTestTaifunFormatter.TestFormatUnitHeader_CreatesNew;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LExpectedHeader: string;
begin
  LSource := 'unit MyUnit; interface end.';
  LExpectedHeader := '''
    // ======================================================================
    //
    // MyUnit - Kurzbeschreibung der Unit
    //
    // Autor: Name
    //
    // ======================================================================

    {$I Tfw.Define.pas}

    unit MyUnit;
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    Assert.IsTrue(LResult.StartsWith(LExpectedHeader), 'Header was not created correctly: '#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_CorrectsExisting;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LExpectedHeader: string;
begin
  LSource := 
    '// ==== Blubb ====' + #13#10 +
    '// Autor: John Doe / Jane Doe' + #13#10 +
    '// ===============' + #13#10 +
    'unit MyUnit; interface end.';
    
  LExpectedHeader := '''
    // ======================================================================
    //
    // MyUnit - Kurzbeschreibung der Unit
    //
    // Autor: John Doe / Jane Doe
    //
    // ======================================================================

    {$I Tfw.Define.pas}

    unit MyUnit;
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    Assert.IsTrue(LResult.StartsWith(LExpectedHeader), 'Header was not corrected properly: '#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_PreservesPerfect;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LExpectedHeader: string;
begin
  LExpectedHeader := '''
    // ======================================================================
    //
    // MyUnit - Special description
    //
    // Autor: The Real Author
    //
    // ======================================================================

    {$I Base.Define.pas}

    unit MyUnit;
    ''';

  LSource := LExpectedHeader + ' interface end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    // Should preserve 'Special description', 'The Real Author', and 'Base.Define.pas' exactly
    Assert.IsTrue(LResult.StartsWith(LExpectedHeader), 'Perfect header was modified: '#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatMethodImplementation_WithXmlDoc;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    '/// <summary>My summary</summary>' + #13#10 +
    '/// <param name="A">Param A</param>' + #13#10 +
    '/// <returns>Result</returns>' + #13#10 +
    'procedure TMyClass.MyMethod;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    '/// <summary>Second summary</summary>' + #13#10 +
    'procedure TMyClass.MyMethod2;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains('/// <summary>My summary</summary>'), 'XML-DOC summary should be preserved');
    Assert.IsTrue(LResult.Contains('/// <param name="A">Param A</param>'), 'XML-DOC param should be preserved');
    Assert.IsTrue(LResult.Contains('/// <returns>Result</returns>'), 'XML-DOC returns should be preserved');
    Assert.IsTrue(LResult.Contains('/// <summary>Second summary</summary>'), 'Second XML-DOC summary should be preserved');

    Assert.IsTrue(LResult.Contains(
      '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
      '{ TMyClass' + StringOfChar(' ', 63) + ' }' + #13#10 +
      '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10 +
      '/// <summary>My summary'),
      'Class banner should be placed before the first XML-DOC comment'
    );

    Assert.IsTrue(LResult.Contains(
      '{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10 +
      '/// <summary>Second summary'),
      'Method banner should be placed before the second XML-DOC comment'
    );
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatMethodImplementation_NestedClass;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    '{ CAppConnectionProvider                                                  }' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'class function CAppConnectionProvider.TCacheKey.Create(ASrc, ADst: TLicenseModuleKind; ASrcMdt: Word): TCacheKey;' + #13#10 +
    'begin' + #13#10 +
    '  Result.SrcModule:=ASrc;' + #13#10 +
    '  Result.DstModule:=ADst;' + #13#10 +
    '  Result.SrcMdt:=ASrcMdt;' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
      '{ CAppConnectionProvider.TCacheKey                                        }' + #13#10 +
      '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
      #13#10 +
      'class function CAppConnectionProvider.TCacheKey.Create'),
      'Class banner should contain the full nested class name and MUST NOT contain fragments of old class banners' + #13#10 + 'Actual result:' + #13#10 + LResult
    );
  finally
    LUnit.Free;
  end;
end;

end.
