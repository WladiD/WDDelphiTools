unit Test.DPT.Formatter.Taifun.ImplSection;

interface

uses
  System.Classes,
  System.SysUtils,

  DUnitX.TestFramework,

  ParseTree.Core,
  ParseTree.Nodes,

  Test.DPT.Formatter.Taifun.Base;

type

  [TestFixture]
  TTestTaifunFormatter_Implementation = class(TTestTaifunFormatterBase)
  public
    [Test]
    procedure TestFormatImplementation_NoExtraEmptyLineBeforeConst;
    [Test]
    procedure TestFormatImplementation_PreservesConstAroundIt;
    [Test]
    procedure TestFormatImplementation_OldClassBannerIsReplacedProperly;
    [Test]
    procedure TestFormatImplementation_OldClassBannerWithTypoIsReplacedProperly;
    [Test]
    procedure TestFormatImplementation_NoExtraEmptyLinesBeforeFirstClass;
    [Test]
    procedure TestFormatImplementation_ClassToFunctionTransition;
    [Test]
    procedure TestFormatImplementation_NoExtraEmptyLineBeforeXmlDoc;
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
    procedure TestFormatImplementation_PreservesInlineBanners;
    [Test]
    procedure TestFormatImplementation_PreservesMultiLineCommentsWrappedInSeparators;
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
    procedure TestFormatImplementation_LowercaseEndif;
    [Test]
    procedure TestFormatImplementation_TrailingBraceCommentBeforeTypeSection;
    [Test]
    procedure TestFormatImplementation_UnterminatedBraceInClassBanner;
    [Test]
    procedure TestFormatImplementation_MultiLineBraceCommentInBanner;
    [Test]
    procedure TestFormatImplementation_NoBannerAfterElseDirective;
    [Test]
    procedure TestFormatImplementation_BraceCommentAfterDashSeparator;
    [Test]
    procedure TestFormatImplementation_ShortDashSeparatorPreserved;
  end;

implementation

{ TTestTaifunFormatter_Implementation }

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_NoExtraEmptyLineBeforeConst;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesConstAroundIt;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_OldClassBannerIsReplacedProperly;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_OldClassBannerWithTypoIsReplacedProperly;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_NoExtraEmptyLinesBeforeFirstClass;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_ClassToFunctionTransition;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_NoExtraEmptyLineBeforeXmlDoc;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesLeadingDirectives;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_AvoidDuplicateClassNameComment;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_AvoidDuplicateClassNameComment_WithSpacesInGenerics;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesClassBannerWithDescriptiveText;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_ReplacesIncorrectGenericClassBanner;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_AvoidsBannersOnBodylessMethods;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_NoExtraEmptyLineBeforeRegion;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesTrailingComment;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesCommentIndentation;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesDirectivesBeforeBanner;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesIfDirectiveBlockBeforeBanner;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesInlineBanners;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesMultiLineCommentsWrappedInSeparators;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesResourceDirectiveBeforeBanner;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_PreservesDashSeparatorBeforeResourcestring;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_StripsOrphanedClassNameFromOldBanner;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_InsertsSectionBannerBeforeStandaloneProc;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_CleansClassBannerInConstSection;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_LowercaseEndif;
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

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_TrailingBraceCommentBeforeTypeSection;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // Reproducer: a trailing brace comment on a const value line, followed by
  // a type section, caused the comment to be expanded into a multi-line
  // banner that was placed directly after the semicolon without a newline,
  // breaking idempotence.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'const' + #13#10 +
    '  Names: array[0..1] of string = (''Alpha'',' + #13#10 +
    '    ''Beta''); { only for testing }' + #13#10 +
    #13#10 +
    '{ ----------------------------------------------------------------------- }' + #13#10 +
    #13#10 +
    'type' + #13#10 +
    '  TFoo = Integer;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // The trailing comment must NOT be glued to the semicolon on the same
    // line as a banner separator.  It should stay as a short trailing comment.
    Assert.IsFalse(LResult.Contains(');{ '),
      'Banner must not be glued to semicolon. Actual:' + #13#10 + LResult);

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2,
        'Trailing brace comment before type section should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_UnterminatedBraceInClassBanner;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // Reproducer: a class banner where the class-name line has an opening
  // brace but no closing brace (e.g. "{ ClassName - class" instead of
  // "{ ClassName - class }").  ProcessTrivia failed to recognise the line
  // as part of the banner, emitting an unterminated brace comment that
  // swallowed the following method on re-parse.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '    procedure GetIt;' + #13#10 +
    '    procedure SetIt;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    #13#10 +
    '{ ======================================================================= }' + #13#10 +
    '{ TFoo - class' + #13#10 +                            // <-- no closing }
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'procedure TFoo.GetIt;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    '{ ----------------------------------------------------------------------- }' + #13#10 +
    #13#10 +
    'procedure TFoo.SetIt;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Both methods must survive formatting
    Assert.IsTrue(LResult.Contains('procedure TFoo.GetIt;'),
      'GetIt must be preserved. Actual:' + #13#10 + LResult);
    Assert.IsTrue(LResult.Contains('procedure TFoo.SetIt;'),
      'SetIt must be preserved. Actual:' + #13#10 + LResult);

    // No unterminated brace comment in the output
    Assert.IsFalse(LResult.Contains('{ TFoo - class' + #13#10),
      'Unterminated brace comment must not appear in output. Actual:' + #13#10 + LResult);

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2,
        'Unterminated brace in class banner should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_MultiLineBraceCommentInBanner;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // Reproducer: a multi-line brace-comment block inside an { === } banner
  // followed by an empty brace padding line.  The peek for LNextIsBrace saw
  // the padding line as a real brace comment, so the closing { --- } was
  // omitted on the first pass but added on the second.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    #13#10 +
    '{ ======================================================================= }' + #13#10 +
    '{ Converts a string to an escape sequence                                 }' + #13#10 +
    '{                                                                         }' + #13#10 +
    '{ Lines starting with "$" are hex.                                        }' + #13#10 +
    '{ Lines starting with "#" are decimal.                                    }' + #13#10 +
    '{                                                                         }' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'function Str2Esc(S: String): String;' + #13#10 +
    'begin' + #13#10 +
    '  Result := S;' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2,
        'Multi-line brace comment in banner should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_NoBannerAfterElseDirective;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // When a method follows an {$ELSE} directive, no method banner should be
  // inserted between the directive and the function keyword.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '    procedure DoWork;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    #13#10 +
    '{$IFDEF TEST}' + #13#10 +
    'procedure TFoo.DoWork;' + #13#10 +
    'begin' + #13#10 +
    '{$ELSE TEST}' + #13#10 +
    'procedure TFoo.DoWork;' + #13#10 +
    'begin' + #13#10 +
    '{$ENDIF TEST}' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // No banner between {$ELSE} and the procedure
    Assert.IsFalse(
      LResult.Contains('{$ELSE TEST}' + #13#10 + #13#10 + '{ ---'),
      'No method banner should be inserted after {$ELSE}. Actual:' + #13#10 + LResult);

    // The function must follow the {$ELSE} directly (with at most a newline)
    Assert.IsTrue(
      LResult.Contains('{$ELSE TEST}' + #13#10 + 'procedure TFoo.DoWork;'),
      '{$ELSE} and procedure must be adjacent. Actual:' + #13#10 + LResult);

    // Idempotence
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2,
        'Method after {$ELSE} should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_BraceCommentAfterDashSeparator;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // A brace comment immediately following a { --- } separator (e.g. a section
  // header like "{ Verkaufs-Belegkopf }") must not be re-wrapped in additional
  // { --- } separators.  The preceding separator already serves as the border.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'const' + #13#10 +
    '  CMaxItems = 100;' + #13#10 +
    #13#10 +
    '{ ----------------------------------------------------------------------- }' + #13#10 +
    '{ Record definitions                                                      }' + #13#10 +
    '{ ----------------------------------------------------------------------- }' + #13#10 +
    #13#10 +
    'type' + #13#10 +
    '  TMyRec = record' + #13#10 +
    '    Value: Integer;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // No duplicate { --- } banner before the comment block
    Assert.IsFalse(LResult.Contains(
      '{ ----------------------------------------------------------------------- }' + #13#10 + #13#10 +
      '{ ----------------------------------------------------------------------- }'),
      'Must not have two consecutive { --- } banners. Actual:' + #13#10 + LResult);

    // Idempotence
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2,
        'Brace comment after dash separator should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_Implementation.TestFormatImplementation_ShortDashSeparatorPreserved;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // Short dash separators (e.g. 26 dashes) are sub-section dividers within
  // methods — they must NOT be treated as full-width banners and re-wrapped.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    #13#10 +
    'procedure TFoo.DoWork;' + #13#10 +
    #13#10 +
    '{ -------------------------- }' + #13#10 +
    '{ Adressen                   }' + #13#10 +
    '{ -------------------------- }' + #13#10 +
    #13#10 +
    'procedure DoSub;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // The short separator block must be preserved exactly
    Assert.IsTrue(LResult.Contains(
      '{ -------------------------- }' + #13#10 +
      '{ Adressen                   }' + #13#10 +
      '{ -------------------------- }'),
      'Short dash separator block must be preserved as-is. Actual:' + #13#10 + LResult);

    // Must NOT be re-wrapped into full-width 71-dash separators
    Assert.IsFalse(LResult.Contains('{ ----------------------------------------------------------------------- }' + #13#10 +
      '{ Adressen'),
      'Short separator must not be expanded to full-width. Actual:' + #13#10 + LResult);

    // Idempotence
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2,
        'Short dash separator handling should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

end.
