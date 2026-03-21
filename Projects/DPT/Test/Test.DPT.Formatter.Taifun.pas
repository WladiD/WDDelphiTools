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
    procedure TestFormatImplementation_OldClassBannerWithTypoIsReplacedProperly;
    [Test]
    procedure TestFormatUnitHeader_CreatesNew;
    [Test]
    procedure TestFormatUnitHeader_CorrectsExisting;
    [Test]
    procedure TestFormatUnitHeader_PreservesPerfect;
    [Test]
    procedure TestFormatUnitHeader_DoesNotOverwriteDescriptionWithLaterHyphens;
    [Test]
    procedure TestFormatUnitHeader_PreservesExtraComments;
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
    procedure TestFormatImplementation_AvoidDuplicateClassNameComment_WithSpacesInGenerics;
    [Test]
    procedure TestFormatImplementation_PreservesClassBannerWithDescriptiveText;
    [Test]
    procedure TestFormatImplementation_ReplacesIncorrectGenericClassBanner;
    [Test]
    procedure TestFormatImplementation_AvoidsBannersOnBodylessMethods;
    [Test]
    procedure TestFormatUnitEnd_PreservesTrailingDirectives;
    [Test]
    procedure TestFormatImplementation_NoExtraEmptyLineBeforeRegion;
    [Test]
    procedure TestFormatImplementation_PreservesTrailingComment;
    [Test]
    procedure TestFormatImplementation_PreservesCommentIndentation;
    [Test]
    procedure TestFormatImplementation_PreservesDirectivesBeforeBanner;
    [Test]
    procedure TestFormatImplementation_PreservesIfDirectiveBlockBeforeBanner;
    [Test]
    procedure TestFormatMethodImplementation_XmlDocNewline;
    [Test]
    procedure TestFormatUnit_NoExtraLineBeforeDirective;
    [Test]
    procedure TestFormatImplementation_PreservesInlineBanners;
    [Test]
    procedure TestFormatImplementation_PreservesMultiLineCommentsWrappedInSeparators;
    [Test]
    procedure TestFormatUnitHeader_ExtractsDescriptionWithHyphen;
    [Test]
    procedure TestFormatUnitHeader_ExtractsMultilineDescription;
    [Test]
    procedure TestFormatUnitHeader_ExtractsDescriptionWithEnDash;
    [Test]
    procedure TestFormatInterface_ReplacesSlashesBanner;
    [Test]
    procedure TestFormatImplementation_PreservesResourceDirectiveBeforeBanner;
    [Test]
    procedure TestFormatImplementation_PreservesDashSeparatorBeforeResourcestring;
    [Test]
    procedure TestFormatImplementation_StripsOrphanedClassNameFromOldBanner;
    [Test]
    procedure TestFormatImplementation_InsertsSectionBannerBeforeStandaloneProc;
    [Test]
    procedure TestFormatImplementation_CleansClassBannerInConstSection;
    [Test]
    procedure TestFormatUnitHeader_PreservesCommentsOutsideBanner;
    [Test]
    procedure TestFormatImplementation_LowercaseEndif;
    [Test]
    procedure TestFormatUnitHeader_UsesBaseDefineForBaseUnits;
    [Test]
    procedure TestFormatTypeSection_UsesShortBanner;
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
  LSource := '''
    unit MyUnit;
    interface

    uses

      {$IF DEFINED(TED) OR DEFINED(TES)}
      Base.Soap.Constants,
      {$ENDIF DEFINED(TED) OR DEFINED(TES)}

      Base.AppCaps;
    implementation
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    const

      MsgNoAccess = 'Funktion nicht verfügbar, weil die Leseberechtigung fehlt.';

    { ======================================================================= }
    implementation
    { ======================================================================= }

    const

      AccessRightHelperNames : Array[Integer] of String
                             = ('-',
                                'Nein',
                                'Ja');

    { ======================================================================= }
    { TAccessTypeHelper                                                       }
    { ======================================================================= }

    function TAccessTypeHelper.HasReadAccess: Boolean;
    begin
      Result:=True;
    end;
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ======================================================================= }
    { CBlacklist - Class                                                      }
    { ======================================================================= }

    constructor CBlacklist.Create(ATblId: Word; AStt: PBlacklistStt);
    begin
      inherited Create;
    end;
    end.
    ''';

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

procedure TTestTaifunFormatter.TestFormatImplementation_OldClassBannerWithTypoIsReplacedProperly;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ======================================================================= }
    { CHierachyObject - Class                                                 }
    { ======================================================================= }

    constructor CHierarchyObject.Create(AOwner: CHierarchyObject);
    begin
      inherited Create;
    end;
    end.
    ''';

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
      '{ CHierarchyObject                                                        }' + #13#10 +
      '{ ======================================================================= }' + #13#10 +
      #13#10 +
      'constructor CHierarchyObject.Create(AOwner: CHierarchyObject);'
    ), 'Old banner with typos and "- Class" suffix must be replaced by the correct new banner. Actual:' + #13#10 + LResult);
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
  LSource := '''
    // ==== Blubb ====
    // Autor: John Doe / Jane Doe
    // ===============
    unit MyUnit; interface end.
    ''';
    
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

procedure TTestTaifunFormatter.TestFormatUnitHeader_DoesNotOverwriteDescriptionWithLaterHyphens;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    // ======================================================================
    //
    // MyUnit - The real description
    //
    // Autor: John Doe
    //
    // This is a multi-line comment.
    // It has a - hyphen later on.
    //
    // ======================================================================
    unit MyUnit; interface end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Should extract "The real description" and NOT "hyphen later on."
    Assert.IsTrue(LResult.Contains('// MyUnit - The real description'), 'Should extract the correct description. Actual result:'#13#10 + LResult);
    Assert.IsFalse(LResult.Contains('// MyUnit - hyphen later on.'), 'Should not overwrite description with later hyphens. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_PreservesExtraComments;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    // ======================================================================
    //
    // MyUnit - Some valid description
    //
    // Autor: Max Mustermann
    //
    // The unicorn jumped over the rainbow
    // finding a pot of pure gold.
    //
    // ======================================================================
    unit MyUnit; interface end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Check if the extra comments about the unicorn were preserved
    Assert.IsTrue(LResult.Contains('// The unicorn jumped over the rainbow' + #13#10 + '// finding a pot of pure gold.'), 'Should preserve extra comments in the header banner. Actual result:'#13#10 + LResult);
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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    /// <summary>My summary</summary>
    /// <param name="A">Param A</param>
    /// <returns>Result</returns>
    procedure TMyClass.MyMethod;
    begin
    end;
    /// <summary>Second summary</summary>
    procedure TMyClass.MyMethod2;
    begin
    end;
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ======================================================================= }
    { CAppConnectionProvider                                                  }
    { ======================================================================= }

    class function CAppConnectionProvider.TCacheKey.Create(ASrc, ADst: TLicenseModuleKind; ASrcMdt: Word): TCacheKey;
    begin
      Result.SrcModule:=ASrc;
      Result.DstModule:=ADst;
      Result.SrcMdt:=ASrcMdt;
    end;
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    function CCopyWrongMdtBlobsInPrg.GetName: S_255;
    begin
      Result:='xxx';
    end;

    function CCopyWrongMdtBlobsInPrg.GetSortDate: Base.Types.DateTime.TDate;
    begin
      Result:=Default(TDate);
    end;
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ======================================================================= }
    { CMongoBlobService                                                       }
    { ======================================================================= }

    constructor CMongoBlobService.Create(ASession: IDbSession; ATblId,AMdtId: Word);
    var
      MongoSession: IMongoSession;
    begin
      inherited Create(ASession,ATblId);
    end;
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    procedure CBlobDelThread.Execute;
    begin
    end;

    function BlobRef2String(const ABlobRef: TBlobRef): String; overload;
    begin
    end;
    end.
    ''';

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
  LSource := '''
    // ======================================================================
    //
    // Base.Bootstrapping.Isapi
    //
    // Autor: Mister X
    //
    // ======================================================================

    {$I Base.Define.pas}
    {$DENYPACKAGEUNIT} {This unit cannot be part of a package because it contains Web.WebBroker which cannot be part of a package }

    unit Base.Bootstrapping.Isapi;

    { ======================================================================= }
    interface
    { ======================================================================= }
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    class constructor CBootstrapping.ClassCreate;
    begin
    end;

    class destructor CBootstrapping.ClassDestroy;
    var
      LogPath: String;
    begin
    end;
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    constructor CConcurrentDictionary<TKey,TValue>.Create;
    begin
    end;

    destructor CConcurrentDictionary<TKey,TValue>.Destroy;
    begin
    end;

    function CConcurrentDictionary<TKey,TValue>.Contains(const AKey: TKey): Boolean;
    begin
    end;
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    function Outer: Integer;
      procedure Inner;
      begin
      end;
    begin
      Inner;
      Result:=0;
    end;
    end.
    ''';

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
      '  procedure Inner;'),
      'Inner procedure should have a short separator with empty line before and after. Actual:' + #13#10 + LResult);

    Assert.IsTrue(LResult.Contains(
      '  end;' + #13#10 + #13#10 +
      '{ -------------------------- }' + #13#10 + #13#10 +
      'begin'),
      'Nested procedure should have a trailing short separator before the outer method block begins.');

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    class procedure CBootstrapping.Finit;
    var
      ClassName: String;
    procedure LogExecution(AProc: TProc);
    begin
      AProc;
    end;
    begin
    end;
    end.
    ''';

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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    /// <summary>
    ///   Description
    /// </summary>
    function ComparePointers(Item1, Item2: Pointer): Integer;
    begin
      Result:=0;
    end;
    end.
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // After implementation banner, there should be exactly ONE empty line before XML-DOC
    // implementation trailing trivia has { === }\r\n\r\n
    // So if the method leading trivia is just the XML-DOC, we get ONE empty line total.
    var LExpected := '''
      { ======================================================================= }
      implementation
      { ======================================================================= }

      /// <summary>
      ''';

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
  LSource := '''
    // ======================================================================
    //
    // MyUnit
    //
    // Autor: John Doe
    //
    // ======================================================================
    unit MyUnit; interface end.
    ''';

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
  LSource := '''
    // ======================================================================
    //
    // Base.Utils.Check -  Basis-Utils für System-Checks
    //
    // Autor: Mister X
    //
    // ======================================================================
    unit Base.Db.Check; interface end.
    ''';
    
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
  LSource := '''
    unit MyUnit;
    interface
    type IList = interface end;

    {$ENDREGION 'PARTIAL'}
    implementation
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Should preserve the blank line before the directive AND have a blank line before the banner
    var LExpectedOrder := '''
      type IList = interface end;

      {$ENDREGION 'PARTIAL'}

      { ======================================================================= }
      implementation
      ''';
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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ======================================================================= }
    // CListEnumerator_Integer
    { ======================================================================= }

    function CListEnumerator_Integer.GetCurrent: Integer;
    begin
      Result:=0;
    end;
    end.
    ''';
    
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

procedure TTestTaifunFormatter.TestFormatImplementation_AvoidDuplicateClassNameComment_WithSpacesInGenerics;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ======================================================================= }
    { CThreadPoolResult<TInput, TOutput>                                      }
    { ======================================================================= }

    function CThreadPoolResult<TInput,TOutput>.IsFaulted: Boolean;
    begin
      Result:=False;
    end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Should NOT contain a duplicate banner when the old one had spaces inside generics
    Assert.IsFalse(LResult.Contains('{ ----------------------------------------------------------------------- }'), 'Should NOT contain a short separator since the class banner should replace the old one. Actual result:'#13#10 + LResult);
    Assert.IsTrue(LResult.Contains('{ CThreadPoolResult<TInput,TOutput>'), 'Should contain the class banner without spaces inside generics');
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesClassBannerWithDescriptiveText;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ======================================================================= }
    { TVersionNumber - Operatoren                                             }
    { ======================================================================= }

    class operator TVersionNumber.Implicit(AValue: Cardinal): TVersionNumber;
    begin
    end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    Assert.IsTrue(LResult.Contains('{ TVersionNumber' + StringOfChar(' ', 57) + ' }'), 'Old banner with descriptive text should be replaced by a standard class banner. Actual result:'#13#10 + LResult);
    Assert.IsFalse(LResult.Contains('{ TVersionNumber - Operatoren'), 'The descriptive text "- Operatoren" should NOT be preserved. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_ReplacesIncorrectGenericClassBanner;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ======================================================================= }
    { CRecordTableCacheBase<TIdx, TRec>                                       }
    { ======================================================================= }

    constructor CRecordTableCacheBase.Create(ATblNo: Word);
    begin
    end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // We expect the old incorrect generic banner to be completely replaced by the correct non-generic banner
    Assert.IsTrue(LResult.Contains('{ CRecordTableCacheBase' + StringOfChar(' ', 50) + ' }'), 'Should generate correct non-generic class banner. Actual result:'#13#10 + LResult);
    Assert.IsFalse(LResult.Contains('{ CRecordTableCacheBase<TIdx, TRec>'), 'Should completely remove the old incorrect generic banner. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_AvoidsBannersOnBodylessMethods;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    function NetApiBufferFree(Buffer: Pointer): DWORD; stdcall; external 'NetAPI32.dll' name 'NetApiBufferFree';
    function NetShareEnum(ServerName: PWideChar; Level: DWORD; var BufPtr: Pointer; PrefMaxLen: DWORD; var EntriesRead: DWORD; var TotalEntries: DWORD; var ResumeHandle: DWORD): DWORD; stdcall; external 'NetAPI32.dll' name 'NetShareEnum';
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Should NOT contain a short separator between these bodyless methods
    Assert.IsFalse(LResult.Contains('{ ----------------------------------------------------------------------- }'), 'Should NOT generate banners for bodyless methods like external functions. Actual result:'#13#10 + LResult);
    Assert.IsTrue(LResult.Contains('function NetApiBufferFree(Buffer: Pointer): DWORD; stdcall; external ''NetAPI32.dll'' name ''NetApiBufferFree'';' + #13#10 + 'function NetShareEnum'), 'Should keep external functions close together without empty lines inserted. Actual result:'#13#10 + LResult);
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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    procedure Test;
    begin
    end;

    {$ENDREGION 'PARTIAL'}
    end.
    ''';
    
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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    {$REGION 'TEST'}
    procedure Test; begin end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Expected: implementation banner then ONE blank line then region
    var LExpected := '''
      implementation
      { ======================================================================= }

      {$REGION 'TEST'}
      ''';
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
  LSource := '''
    unit MyUnit;
    interface
    implementation
    const
      ChunkSizeInByte = 261120; // 255 KiB, i.e. 255 * 1024 Byte

    constructor CMongoGridFsService.Create; begin inherited Create; end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // The comment should stay with the constant, NOT move below the class banner
    var LExpected := '''
      ChunkSizeInByte = 261120; // 255 KiB, i.e. 255 * 1024 Byte

      { ======================================================================= }
      { CMongoGridFsService
      ''';

    Assert.IsTrue(LResult.Contains(LExpected), 'Trailing comment should not move below the class banner. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesCommentIndentation;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
      PoemVersion = 26000000;             //Rosen sind rot, Veilchen sind blau,
                                          //dieser Kommentar ist eingerückt,
                                          //das weiß ich genau.

    implementation
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // The indentation of the second comment line should be perfectly preserved
    var LExpected := '''
        PoemVersion = 26000000;             //Rosen sind rot, Veilchen sind blau,
                                            //dieser Kommentar ist eingerückt,
                                            //das weiß ich genau.

      { ======================================================================= }
      implementation
      ''';

    Assert.IsTrue(LResult.Contains(LExpected), 'The indentation of multi-line comments before the implementation banner should be preserved. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesDirectivesBeforeBanner;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    const
      {$IFDEF CPUX64}
      BtrieveDLLName  = 'W64BTRV.DLL';
      {$ELSE}
      BtrieveDLLName  = 'WBTRV32.DLL';
      {$ENDIF CPUX64}

    constructor CBtrieveSession.Create(AIdPool: CConcurrentBitPool);
    begin
      inherited Create;
    end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // The constant should end with a semicolon and a newline,
    // then the {$ENDIF} should follow on its own indented line.
    var LExpectedPart := '''
      BtrieveDLLName  = 'WBTRV32.DLL';
        {$ENDIF CPUX64}

      { ======================================================================= }
      ''';

    Assert.IsTrue(LResult.Contains(LExpectedPart), 'Compiler directive {$ENDIF} should stay on its own line and above the class banner. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesIfDirectiveBlockBeforeBanner;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation

    {$IF DEFINED(DEBUG)}
      {$SetPEFlags $20}
    {$ENDIF}

    { ======================================================================= }
    { CMyClass                                                                }
    { ======================================================================= }

    procedure CMyClass.DoSomething;
    begin
    end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    var LExpectedPart := '''
      {$IF DEFINED(DEBUG)}
        {$SetPEFlags $20}
      {$ENDIF}

      { ======================================================================= }
      { CMyClass
      ''';

    Assert.IsTrue(LResult.Contains(LExpectedPart), 'The $IF block should stay strictly above the class banner. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatMethodImplementation_XmlDocNewline;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ----------------------------------------------------------------------- }

    /// <summary> If the fields of this table have GUI (visual) controls assinged to them, reads the data from those GUI controls
    /// </summary>
    procedure CBaseTable.GetData;
    begin
    end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // The XML-DOC summary should end with a newline before the procedure keyword starts
    var LExpectedPart := 
      '/// </summary>' + #13#10 +
      'procedure CBaseTable.GetData;';

    Assert.IsTrue(LResult.Contains(LExpectedPart), 'There should be a newline between the XML-DOC summary and the procedure declaration. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnit_NoExtraLineBeforeDirective;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit Base.WinApi;

    {$ALIGN ON} // comment

    interface
    implementation
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // There should be exactly ONE blank line between the unit declaration and the directive.
    // unit Base.WinApi;\r\n\r\n{$ALIGN ON}
    var LExpectedPart := '''
      unit Base.WinApi;

      {$ALIGN ON}
      ''';

    Assert.IsTrue(LResult.Contains(LExpectedPart), 'There should be exactly one blank line before the directive. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesInlineBanners;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ----------------------------------------------------------------------- }
    { Prüfsummenberechnung                                                    }
    { ----------------------------------------------------------------------- }
    procedure Test;
    begin
    end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // The text inside the inline banner should be preserved
    Assert.IsTrue(LResult.Contains('{ Prüfsummenberechnung'), 'Inline banner text should be preserved. Actual result:'#13#10 + LResult);
    // The dashed lines around it should either be preserved or nicely formatted
    Assert.IsTrue(LResult.Contains('{ ----------------------------------------------------------------------- }' + #13#10 + '{ Prüfsummenberechnung'), 'Inline banner should be formatted correctly. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesMultiLineCommentsWrappedInSeparators;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ----------------------------------------------------------------------- }
    { Category A: XXX01234567890123456 (cpAlpha)                              }
    {             XXX01234567890123457 (cpBeta)                               }
    {             XXX01234567890123458 (cpGamma)                              }
    { ----------------------------------------------------------------------- }
    procedure Test;
    begin
    end;
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    var LExpectedBlock := '''
      { ----------------------------------------------------------------------- }
      { Category A: XXX01234567890123456 (cpAlpha)                              }
      {             XXX01234567890123457 (cpBeta)                               }
      {             XXX01234567890123458 (cpGamma)                              }
      { ----------------------------------------------------------------------- }
      ''';

    Assert.IsTrue(LResult.Contains(LExpectedBlock), 'The entire block should be preserved perfectly. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_ExtractsDescriptionWithHyphen;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    // ======================================================================
    //
    // Base.Kons.Common.Typ - Typdeklarationen: Konstanten (Programmübergreifend)
    //
    // Autor: WDE
    //
    // ======================================================================
    unit Base.Kons.Common.Typ; interface end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    Assert.IsTrue(LResult.Contains('// Base.Kons.Common.Typ - Typdeklarationen: Konstanten (Programmübergreifend)'), 'Description with hyphen should be preserved correctly. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_ExtractsMultilineDescription;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    // ======================================================================
    //
    // MyUnit.Foo - The first line of the description.
    //              The second line of the description.
    //
    // Autor: John Doe
    //
    // ======================================================================
    unit MyUnit.Foo; interface end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    Assert.IsTrue(LResult.Contains('// MyUnit.Foo - The first line of the description.' + #13#10 + '//              The second line of the description.'), 'Multiline description should be preserved correctly under the unit name. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_ExtractsDescriptionWithEnDash;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    // ======================================================================
    //
    // MyUnit.Validator – JWT/JWKS Validation
    //
    // Autor: Alice
    //
    // ======================================================================
    unit MyUnit.Validator; interface end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    Assert.IsTrue(LResult.Contains('// MyUnit.Validator - JWT/JWKS Validation'), 'Description with en-dash should be preserved and normalized. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatInterface_ReplacesSlashesBanner;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    // ======================================================================
    interface
    // ======================================================================
    end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // Check if the slash banner before and after the interface section was successfully replaced by the curly brace banner.
    Assert.IsTrue(LResult.Contains('{ ' + StringOfChar('=', 71) + ' }' + #13#10 + 'interface'), 'Slashes banner before interface should be replaced by standard curly brace banner. Actual result:'#13#10 + LResult);
    // The unit header also uses // ====, so we just make sure there's no // ==== directly before the interface banner
    Assert.IsFalse(LResult.Contains('// ======================================================================' + #13#10 + '{ ' + StringOfChar('=', 71)), 'The old slashes banner should be completely removed from the interface trivia. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesResourceDirectiveBeforeBanner;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit.Form;
    interface
    implementation

    {$R *.dfm}

    { ======================================================================= }
    { TMyForm                                                                 }
    { ======================================================================= }

    class function TMyForm.CanOpenForm: Boolean;
    begin
      Result := True;
    end;
    end.
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    var LExpectedPart := '''
      {$R *.dfm}

      { ======================================================================= }
      { TMyForm
      ''';

    Assert.IsTrue(LResult.Contains(LExpectedPart), '{$R *.dfm} should stay above the class banner. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_PreservesDashSeparatorBeforeResourcestring;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit.Form;
    interface
    implementation

    {$R *.DFM}

    { ----------------------------------------------------------------------- }

    resourcestring

      txtBack = '&Back';

    { ======================================================================= }
    { TMyForm                                                                 }
    { ======================================================================= }

    procedure TMyForm.DoSomething;
    begin
    end;
    end.
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '{ ' + StringOfChar('-', 71) + ' }' + #13#10 +
      #13#10 +
      'resourcestring'),
      '{ --- } separator before resourcestring should be preserved. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_StripsOrphanedClassNameFromOldBanner;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    type
      TMyActualClass = class
      end;
    implementation

    { ======================================================================= }
    { OldWrongClassName                                                       }
    { ======================================================================= }

    procedure TMyActualClass.DoSomething;
    begin
    end;
    end.
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsFalse(LResult.Contains('OldWrongClassName'),
      'Orphaned class name from old banner should be stripped. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_InsertsSectionBannerBeforeStandaloneProc;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit.Form;
    interface
    implementation

    {$R *.dfm}

    procedure DoSomething;
    begin
    end;
    end.
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '{$R *.dfm}' + #13#10 +
      #13#10 +
      '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
      #13#10 +
      'procedure DoSomething;'),
      'Section banner { === } should be inserted before standalone procedure. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_CleansClassBannerInConstSection;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation

    procedure TFoo.Bar;
    begin
    end;

    { ======================================================================= }
    { CToolButton - Class                                                     }
    { ======================================================================= }

    const
      InitRepeatPause = 400;

    { ----------------------------------------------------------------------- }

    constructor CToolButton.Create;
    begin
    end;

    { ----------------------------------------------------------------------- }

    destructor CToolButton.Destroy;
    begin
    end;
    end.
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    var LExpectedBanner :=
      '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
      '{ CToolButton' + StringOfChar(' ', 60) + ' }' + #13#10 +
      '{ ' + StringOfChar('=', 71) + ' }';

    Assert.IsTrue(LResult.Contains(LExpectedBanner),
      'Class banner should be { CToolButton } (without - Class suffix). Actual result:'#13#10 + LResult);

    Assert.IsFalse(LResult.Contains('CToolButton - Class'),
      '- Class suffix should be stripped from banner. Actual result:'#13#10 + LResult);

    var LBannerLine := '{ CToolButton' + StringOfChar(' ', 60) + ' }';
    var LFirstPos := Pos(LBannerLine, LResult);
    var LSecondPos := Pos(LBannerLine, LResult, LFirstPos + 1);
    Assert.AreEqual(0, LSecondPos,
      'CToolButton class banner should appear only once (no duplicate). Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_PreservesCommentsOutsideBanner;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    // ======================================================================
    //
    // MyUnit
    //
    // ======================================================================

    {$REGION 'Some region'}
    // This comment should not be pulled into the banner
    {$ENDREGION}
    unit MyUnit; interface end.
    ''';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // The comment should stay inside the region block, NOT inside the banner
    var LExpectedRegion := '''
      {$REGION 'Some region'}
      // This comment should not be pulled into the banner
      {$ENDREGION}
      ''';

    Assert.IsTrue(LResult.Contains(LExpectedRegion), 'The comment inside the $REGION should be preserved exactly outside the banner. Actual result:'#13#10 + LResult);
    
    // The banner should not contain the comment
    Assert.IsFalse(LResult.Contains('//' + #13#10 + '// This comment should not be pulled into the banner' + #13#10 + '//' + #13#10 + '// ===='), 'The banner should not have absorbed the comment.');
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatImplementation_LowercaseEndif;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := // TODO: Bitte als Multiline-String-Literal (''')
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    '{$if CompilerVersion >= 29}' + #13#10 +
    'function TApp.GetVersionString: string;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    '{$endif}' + #13#10 +
    #13#10 +
    '{$if CompilerVersion >= 32}' + #13#10 +
    'function TApp.Running: Boolean;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    '{$endif}' + #13#10 +
    'end.';
    
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);
    
    // The short separator for the second method should come AFTER the {$endif}
    var LExpectedPart := '''
      {$endif}

      { ----------------------------------------------------------------------- }

      {$if CompilerVersion >= 32}
      ''';

    Assert.IsTrue(LResult.Contains(LExpectedPart), 'The short separator should be placed AFTER the {$endif} of the previous method. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatUnitHeader_UsesBaseDefineForBaseUnits;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LExpectedHeader: string;
begin
  LSource := 'unit Base.Minimum; interface end.';

  LExpectedHeader := '''
    // ======================================================================
    //
    // Base.Minimum - Kurzbeschreibung der Unit
    //
    // Autor: Name
    //
    // ======================================================================

    {$I Base.Define.pas}

    unit Base.Minimum;
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.StartsWith(LExpectedHeader), 'Should generate header with Base.Define.pas. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter.TestFormatTypeSection_UsesShortBanner;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := '''
    unit Base.Minimum;
    interface
    uses
      System.Classes;
    type
      TFormAbort = class
      end;
    implementation
    end.
    ''';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // It should use a short banner `{ --- }` instead of a long one `{ === }` before `type` when following a uses clause
    Assert.IsTrue(LResult.Contains(
      #13#10 + '{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10 + 'type' + #13#10
    ), 'Type section should have a short banner when not suppressed. Actual result:'#13#10 + LResult);
    
    // Ensure it does not contain the long banner before type
    Assert.IsFalse(LResult.Contains(
      #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10 + 'type' + #13#10
    ), 'Type section should not have a long banner. Actual result:'#13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

end.
