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
    procedure TestFormatUsesClause_WithCompilerDirectives;
    [Test]
    procedure TestFormatSections;
    [Test]
    procedure TestFormatSections_IdempotentWithResourceString;
    [Test]
    procedure TestFormatMethodImplementation;
    [Test]
    procedure TestFormatImplementation_OldClassBannerIsReplacedProperly;
    [Test]
    procedure TestFormatUnitHeader_CreatesNew;
    [Test]
    procedure TestFormatUnitHeader_CorrectsExisting;
    [Test]
    procedure TestFormatUnitHeader_PreservesPerfect;
    [Test]
    procedure TestFormatInterface_PreservesEmptyLineBeforeType;
    [Test]
    procedure TestFormatInterface_NoExtraEmptyLineBeforeUses;
    [Test]
    procedure TestFormatImplementation_NoExtraEmptyLineBeforeConst;
    [Test]
    procedure TestFormatImplementation_PreservesConstAroundIt;
    [Test]
    procedure TestNoRedundantSeparatorAfterImplementation;
    [Test]
    procedure TestFormatMethodImplementation_WithXmlDoc;
    [Test]
    procedure TestFormatMethodImplementation_NestedClass;
    [Test]
    procedure TestFormatMethodImplementation_NamespacedReturnType;
    [Test]
    procedure TestFormatImplementation_NoExtraEmptyLinesBeforeFirstClass;
    [Test]
    procedure TestFormatImplementation_ClassToFunctionTransition;
    [Test]
    procedure TestFormatUnitHeader_PreservesDirectivesAfterInclude;
    [Test]
    procedure TestFormatMethodImplementation_ClassConstructor;
    [Test]
    procedure TestFormatMethodImplementation_GenericClass;
    [Test]
    procedure TestFormatMethodImplementation_NestedProcedure;
    [Test]
    procedure TestFormatMethodImplementation_NestedProcedureInClassMethod;
    [Test]
    procedure TestFormatImplementation_NoExtraEmptyLineBeforeXmlDoc;
    [Test]
    procedure TestFormatUnitHeader_NoPlaceholderOnExisting;
    [Test]
    procedure TestFormatUnitHeader_PlaceholderOnNew;
    [Test]
    procedure TestFormatUnitHeader_PreservesDescriptionEvenIfUnitNameMismatched;
    [Test]
    procedure TestFormatImplementation_PreservesLeadingDirectives;
    [Test]
    procedure TestFormatImplementation_AvoidDuplicateClassNameComment;
    [Test]
    procedure TestFormatUnitEnd_PreservesTrailingDirectives;
    [Test]
    procedure TestFormatImplementation_NoExtraEmptyLineBeforeRegion;
    [Test]
    procedure TestFormatImplementation_PreservesTrailingComment;
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

procedure TTestTaifunFormatter.TestFormatUsesClause_WithCompilerDirectives;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit;' + #13#10 +
             'interface' + #13#10 +
             #13#10 +
             'uses' + #13#10 +
             #13#10 +
             '  {$IF DEFINED(TED) OR DEFINED(TES)}' + #13#10 +
             '  Base.Soap.Constants,' + #13#10 +
             '  {$ENDIF DEFINED(TED) OR DEFINED(TES)}' + #13#10 +
             #13#10 +
             '  Base.AppCaps;' + #13#10 +
             'implementation' + #13#10 +
             'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains('uses' + #13#10 + #13#10 + '  {$IF DEFINED(TED) OR DEFINED(TES)}'), 'Compiler directive should be preserved after uses keyword. Actual:' + #13#10 + LResult);
    Assert.IsTrue(LResult.Contains('  {$ENDIF DEFINED(TED) OR DEFINED(TES)}' + #13#10 + #13#10 + '  Base.AppCaps;'), 'Compiler directive should be preserved before next uses item. Actual:' + #13#10 + LResult);
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

procedure TTestTaifunFormatter.TestFormatInterface_PreservesEmptyLineBeforeType;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit;' + #13#10 + 'interface' + #13#10 + #13#10 + 'type' + #13#10 + '  TMyType = Integer;' + #13#10 + 'implementation' + #13#10 + 'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Initial format should preserve the empty line between interface and type
    Assert.IsTrue(LResult.Contains(
      #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + 'interface' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10 + 'type' + #13#10
    ), 'Interface block should be followed by an empty line before the type declaration. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatInterface_NoExtraEmptyLineBeforeUses;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit;' + #13#10 + 'interface' + #13#10 + #13#10 + 'uses' + #13#10 + '  System.Classes,' + #13#10 + '  System.SysUtils;' + #13#10 + 'implementation' + #13#10 + 'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Initial format should have exactly one empty line between interface banner and uses
    Assert.IsTrue(LResult.Contains(
      #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + 'interface' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10 + 'uses' + #13#10
    ), 'Interface block should be followed by exactly one empty line before the uses declaration. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_NoExtraEmptyLineBeforeConst;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit;' + #13#10 + 'interface' + #13#10 + 'implementation' + #13#10 + #13#10 + 'const' + #13#10 + '  MyConst = 1;' + #13#10 + 'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Initial format should have exactly one empty line between implementation banner and const
    Assert.IsTrue(LResult.Contains(
      #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + 'implementation' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10 + 'const' + #13#10
    ), 'Implementation block should be followed by exactly one empty line before the const declaration. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesConstAroundIt;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'const' + #13#10 +
    #13#10 +
    '  MsgNoAccess = ''Funktion nicht verfügbar, weil die Leseberechtigung fehlt.'';' + #13#10 +
    #13#10 +
    '{ ======================================================================= }' + #13#10 +
    'implementation' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'const' + #13#10 +
    #13#10 +
    '  AccessRightHelperNames : Array[Integer] of String' + #13#10 +
    '                         = (''-'',' + #13#10 +
    '                            ''Nein'',' + #13#10 +
    '                            ''Ja'');' + #13#10 +
    #13#10 +
    '{ ======================================================================= }' + #13#10 +
    '{ TAccessTypeHelper                                                       }' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'function TAccessTypeHelper.HasReadAccess: Boolean;' + #13#10 +
    'begin' + #13#10 +
    '  Result:=True;' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains('  MsgNoAccess = ''Funktion nicht verfügbar, weil die Leseberechtigung fehlt.'';' + #13#10 +
      #13#10 +
      '{ ======================================================================= }' + #13#10 +
      'implementation' + #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      'const' + #13#10 +
      #13#10 +
      '  AccessRightHelperNames : Array[Integer] of String'), 'Implementation banners should be properly inserted without mangling surrounding const declarations. Actual:' + #13#10 + LResult);
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

procedure TTestTaifunFormatter.TestFormatImplementation_OldClassBannerIsReplacedProperly;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit;' + #13#10 +
             'interface' + #13#10 +
             'implementation' + #13#10 +
             '{ ======================================================================= }' + #13#10 +
             '{ CBlacklist - Class                                                      }' + #13#10 +
             '{ ======================================================================= }' + #13#10 +
             #13#10 +
             'constructor CBlacklist.Create(ATblId: Word; AStt: PBlacklistStt);' + #13#10 +
             'begin' + #13#10 +
             '  inherited Create;' + #13#10 +
             'end;' + #13#10 +
             'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '{ ======================================================================= }' + #13#10 +
      'implementation' + #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      '{ ======================================================================= }' + #13#10 +
      '{ CBlacklist                                                              }' + #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      'constructor CBlacklist.Create(ATblId: Word; AStt: PBlacklistStt);'
    ), 'Old banner with suffixes like "- Class" must be completely replaced and no extra empty lines should be present before the new banner. Actual:' + #13#10 + LResult);
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
    // MyUnit
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

procedure TTestTaifunFormatter.TestFormatMethodImplementation_NamespacedReturnType;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'function CCopyWrongMdtBlobsInPrg.GetName: S_255;' + #13#10 +
    'begin' + #13#10 +
    '  Result:=''xxx'';' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'function CCopyWrongMdtBlobsInPrg.GetSortDate: Base.Types.DateTime.TDate;' + #13#10 +
    'begin' + #13#10 +
    '  Result:=Default(TDate);' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
      '{ CCopyWrongMdtBlobsInPrg                                                 }' + #13#10 +
      '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
      #13#10 +
      'function CCopyWrongMdtBlobsInPrg.GetName: S_255;'),
      'First method should have a full class banner. Actual:' + #13#10 + LResult
    );

    Assert.IsTrue(LResult.Contains(
      '{ ' + StringOfChar('-', 71) + ' }' + #13#10 +
      #13#10 +
      'function CCopyWrongMdtBlobsInPrg.GetSortDate: Base.Types.DateTime.TDate;'),
      'Second method should have a short method banner despite namespaced return type. Actual:' + #13#10 + LResult
    );
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_NoExtraEmptyLinesBeforeFirstClass;
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
    '{ CMongoBlobService                                                       }' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'constructor CMongoBlobService.Create(ASession: IDbSession; ATblId,AMdtId: Word);' + #13#10 +
    'var' + #13#10 +
    '  MongoSession: IMongoSession;' + #13#10 +
    'begin' + #13#10 +
    '  inherited Create(ASession,ATblId);' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '{ ======================================================================= }' + #13#10 +
      'implementation' + #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      '{ ======================================================================= }' + #13#10 +
      '{ CMongoBlobService                                                       }' + #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      'constructor CMongoBlobService.Create'),
      'Should not have extra blank lines between implementation banner and the first class banner. Actual:' + #13#10 + LResult
    );
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_ClassToFunctionTransition;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure CBlobDelThread.Execute;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'function BlobRef2String(const ABlobRef: TBlobRef): String; overload;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      'procedure CBlobDelThread.Execute;' + #13#10 +
      'begin' + #13#10 +
      'end;' + #13#10 +
      #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      'function BlobRef2String(const ABlobRef: TBlobRef): String; overload;'),
      'Transition from class method to unit-level function should use a double separator block. Actual:' + #13#10 + LResult
    );
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_PreservesDirectivesAfterInclude;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    '// ======================================================================' + #13#10 +
    '//' + #13#10 +
    '// Base.Bootstrapping.Isapi' + #13#10 +
    '//' + #13#10 +
    '// Autor: Mister X' + #13#10 +
    '//' + #13#10 +
    '// ======================================================================' + #13#10 +
    #13#10 +
    '{$I Base.Define.pas}' + #13#10 +
    '{$DENYPACKAGEUNIT} {This unit cannot be part of a package because it contains Web.WebBroker which cannot be part of a package }' + #13#10 +
    #13#10 +
    'unit Base.Bootstrapping.Isapi;' + #13#10 +
    #13#10 +
    '{ ======================================================================= }' + #13#10 +
    'interface' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains('{$I Base.Define.pas}' + #13#10 + '{$DENYPACKAGEUNIT}'), 'Compiler directives after include should be preserved. Actual: ' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatMethodImplementation_ClassConstructor;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'class constructor CBootstrapping.ClassCreate;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'class destructor CBootstrapping.ClassDestroy;' + #13#10 +
    'var' + #13#10 +
    '  LogPath: String;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '{ ======================================================================= }' + #13#10 +
      '{ CBootstrapping                                                          }' + #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      'class constructor CBootstrapping.ClassCreate;'),
      'Class constructor should be formatted properly. Actual:' + #13#10 + LResult
    );

    Assert.IsTrue(LResult.Contains(
      '{ ----------------------------------------------------------------------- }' + #13#10 +
      #13#10 +
      'class destructor CBootstrapping.ClassDestroy;'),
      'Class destructor should be formatted properly. Actual:' + #13#10 + LResult
    );
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatMethodImplementation_GenericClass;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'constructor CConcurrentDictionary<TKey,TValue>.Create;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'destructor CConcurrentDictionary<TKey,TValue>.Destroy;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'function CConcurrentDictionary<TKey,TValue>.Contains(const AKey: TKey): Boolean;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '{ ======================================================================= }' + #13#10 +
      '{ CConcurrentDictionary<TKey,TValue>                                      }' + #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      'constructor CConcurrentDictionary<TKey,TValue>.Create;'),
      'First generic class method should get the full banner. Actual:' + #13#10 + LResult
    );

    Assert.IsTrue(LResult.Contains(
      '{ ----------------------------------------------------------------------- }' + #13#10 +
      #13#10 +
      'destructor CConcurrentDictionary<TKey,TValue>.Destroy;'),
      'Second generic class method should get short banner. Actual:' + #13#10 + LResult
    );

    Assert.IsTrue(LResult.Contains(
      '{ ----------------------------------------------------------------------- }' + #13#10 +
      #13#10 +
      'function CConcurrentDictionary<TKey,TValue>.Contains(const AKey: TKey): Boolean;'),
      'Third generic class method should get short banner, not a new class banner. Actual:' + #13#10 + LResult
    );
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatMethodImplementation_NestedProcedure;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'function Outer: Integer;' + #13#10 +
    '  procedure Inner;' + #13#10 +
    '  begin' + #13#10 +
    '  end;' + #13#10 +
    'begin' + #13#10 +
    '  Inner;' + #13#10 +
    '  Result:=0;' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Outer should have double banner (transition or first method)
    Assert.IsTrue(LResult.Contains(
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      'function Outer: Integer;'),
      'Outer function should have a long separator. Actual:' + #13#10 + LResult
    );

    // Inner should have short banner with empty line BEFORE and AFTER
    Assert.IsTrue(LResult.Contains(
      #13#10#13#10 + '{ -------------------------- }' + #13#10 +
      #13#10 +
      'procedure Inner;'),
      'Inner procedure should have a short separator with empty line before and after. Actual:' + #13#10 + LResult
    );

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2, 'Formatting nested procedure should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatMethodImplementation_NestedProcedureInClassMethod;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'class procedure CBootstrapping.Finit;' + #13#10 +
    'var' + #13#10 +
    '  ClassName: String;' + #13#10 +
    'procedure LogExecution(AProc: TProc);' + #13#10 +
    'begin' + #13#10 +
    '  AProc;' + #13#10 +
    'end;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Finit should have normal class banner (assuming it's the first or just a method)
    Assert.IsTrue(LResult.Contains('{ CBootstrapping'), 'Finit should have a class banner. Actual result around method:'#13#10 + Copy(LResult, Pos('class procedure CBootstrapping.Finit', LResult) - 200, 400));
    Assert.IsTrue(LResult.Contains('{ ======================================================================= }' + #13#10 + #13#10 + 'class procedure CBootstrapping.Finit;'), 'Finit should have TWO empty lines before it. Actual result around method:'#13#10 + Copy(LResult, Pos('class procedure CBootstrapping.Finit', LResult) - 200, 400));

    // LogExecution should have SHORT banner, NOT a long transition banner
    Assert.IsFalse(LResult.Contains(
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      'procedure LogExecution(AProc: TProc);'),
      'Nested procedure should NOT have a long transition banner. Actual:' + #13#10 + LResult);

    Assert.IsTrue(LResult.Contains(
      #13#10#13#10 + '{ -------------------------- }' + #13#10 +
      #13#10 +
      'procedure LogExecution(AProc: TProc);'),
      'Nested procedure should have a short banner. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_NoExtraEmptyLineBeforeXmlDoc;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    '/// <summary>' + #13#10 +
    '///   Description' + #13#10 +
    '/// </summary>' + #13#10 +
    'function ComparePointers(Item1, Item2: Pointer): Integer;' + #13#10 +
    'begin' + #13#10 +
    '  Result:=0;' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // After implementation banner, there should be exactly ONE empty line before XML-DOC
    // implementation trailing trivia has { === }\r\n\r\n
    // So if the method leading trivia is just the XML-DOC, we get ONE empty line total.
    var LExpected :=
      '{ ======================================================================= }' + #13#10 +
      'implementation' + #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      '/// <summary>';

    Assert.IsTrue(LResult.Contains(LExpected), 'There should be exactly ONE empty line between implementation banner and XML-DOC. Actual result around implementation:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_NoPlaceholderOnExisting;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 
    '// ======================================================================' + #13#10 +
    '//' + #13#10 +
    '// MyUnit' + #13#10 +
    '//' + #13#10 +
    '// Autor: John Doe' + #13#10 +
    '//' + #13#10 +
    '// ======================================================================' + #13#10 +
    'unit MyUnit; interface end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Should NOT contain the placeholder 'Kurzbeschreibung der Unit' because a banner already existed
    Assert.IsFalse(LResult.Contains('Kurzbeschreibung der Unit'), 'Should not add placeholder to existing banner');
    Assert.IsFalse(LResult.Contains('// MyUnit - '), 'Should not contain the " - " separator when description is empty');
    Assert.IsTrue(LResult.Contains('// MyUnit' + #13#10), 'Should have description line with only unit name. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_PlaceholderOnNew;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  // No banner at all
  LSource := 'unit MyUnit; interface end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // SHOULD contain the placeholder 'Kurzbeschreibung der Unit' because it is a new banner
    Assert.IsTrue(LResult.Contains('Kurzbeschreibung der Unit'), 'Should add placeholder to new banner');
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_PreservesDescriptionEvenIfUnitNameMismatched;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 
    '// ======================================================================' + #13#10 +
    '//' + #13#10 +
    '// Base.Utils.Check -  Basis-Utils für System-Checks' + #13#10 +
    '//' + #13#10 +
    '// Autor: Mister X' + #13#10 +
    '//' + #13#10 +
    '// ======================================================================' + #13#10 +
    'unit Base.Db.Check; interface end.';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Should have updated unit name AND preserved description
    Assert.IsTrue(LResult.Contains('Base.Db.Check - Basis-Utils für System-Checks'), 'Should update unit name and preserve description. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesLeadingDirectives;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type IList = interface end;' + #13#10 +
    #13#10 +
    '{$ENDREGION ''PARTIAL''}' + #13#10 +
    'implementation' + #13#10 +
    'end.';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Should preserve the blank line before the directive AND have a blank line before the banner
    var LExpectedOrder := 
      'type IList = interface end;' + #13#10 +
      #13#10 +
      '{$ENDREGION ''PARTIAL''}' + #13#10 + 
      #13#10 + 
      '{ ======================================================================= }' + #13#10 +
      'implementation';

    Assert.IsTrue(LResult.Contains(LExpectedOrder), 'Compiler directive should preserve leading blank line and be placed before the implementation banner. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_AvoidDuplicateClassNameComment;
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
    '// CListEnumerator_Integer' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'function CListEnumerator_Integer.GetCurrent: Integer;' + #13#10 +
    'begin' + #13#10 +
    '  Result:=0;' + #13#10 +
    'end;' + #13#10 +
    'end.';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Should contain the class banner but NOT the duplicate '// CListEnumerator_Integer' comment
    Assert.IsTrue(LResult.Contains('{ CListEnumerator_Integer'), 'Should contain the class banner');
    Assert.IsFalse(LResult.Contains('// CListEnumerator_Integer'), 'Should NOT contain the redundant class name comment. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitEnd_PreservesTrailingDirectives;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure Test;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    '{$ENDREGION ''PARTIAL''}' + #13#10 +
    'end.';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Should preserve the directive before the final end. banner
    Assert.IsTrue(LResult.Contains('{$ENDREGION ''PARTIAL''}'), 'Compiler directive at the end of unit should be preserved. Actual result:'#13#10 + LResult);
    Assert.IsTrue(LResult.Contains('{$ENDREGION ''PARTIAL''}' + #13#10 + #13#10 + '{ ======================================================================= }' + #13#10 + #13#10 + 'end.'), 'Incorrect order or spacing at unit end. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_NoExtraEmptyLineBeforeRegion;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    '{$REGION ''TEST''}' + #13#10 +
    'procedure Test; begin end;' + #13#10 +
    'end.';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Expected: implementation banner then ONE blank line then region
    var LExpected := 
      'implementation' + #13#10 + 
      '{ ======================================================================= }' + #13#10 +
      #13#10 + 
      '{$REGION ''TEST''}';

    Assert.IsTrue(LResult.Contains(LExpected), 'Should have exactly ONE blank line between implementation banner and region. Actual result around region:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesTrailingComment;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'const' + #13#10 +
    '  ChunkSizeInByte = 261120; // 255 KiB, i.e. 255 * 1024 Byte' + #13#10 +
    #13#10 +
    'constructor CMongoGridFsService.Create; begin inherited Create; end;' + #13#10 +
    'end.';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // The comment should stay with the constant, NOT move below the class banner
    var LExpected := 
      'ChunkSizeInByte = 261120; // 255 KiB, i.e. 255 * 1024 Byte' + #13#10 +
      #13#10 +
      '{ ======================================================================= }' + #13#10 +
      '{ CMongoGridFsService';

    Assert.IsTrue(LResult.Contains(LExpected), 'Trailing comment should not move below the class banner. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

end.
