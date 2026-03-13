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

end.
