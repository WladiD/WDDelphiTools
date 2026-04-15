unit Test.DPT.Formatter.Taifun.VarSection;

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
  TTestTaifunFormatter_VarSection = class(TTestTaifunFormatterBase)
  public
    [Test]
    procedure TestFormatVarSection_SortsAlphabetically;
    [Test]
    procedure TestFormatVarSection_AlignsColons;
    [Test]
    procedure TestFormatVarSection_AbsoluteFollowsTarget;
    [Test]
    procedure TestFormatVarSection_Idempotent;
    [Test]
    procedure TestFormatVarSection_PreservesTrailingComment;
    [Test]
    procedure TestFormatVarSection_SplitsMultiVar;
    [Test]
    procedure TestFormatVarSection_SplitsMultiVar_ThreeVars;
    [Test]
    procedure TestFormatVarSection_SplitsAndSorts;
    [Test]
    procedure TestFormatVarSection_SkipsUnitLevel;
    [Test]
    procedure TestFormatVarSection_SingleVar;
  end;

implementation

{ TTestTaifunFormatter_VarSection }

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_SortsAlphabetically;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure Foo;' + #13#10 +
    'var' + #13#10 +
    '  Zebra: String;' + #13#10 +
    '  Alpha: Integer;' + #13#10 +
    '  Middle: Boolean;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      'var' + #13#10 +
      '  Alpha : Integer;' + #13#10 +
      '  Middle: Boolean;' + #13#10 +
      '  Zebra : String;'),
      'Var declarations should be sorted alphabetically. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_AlignsColons;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure Foo;' + #13#10 +
    'var' + #13#10 +
    '  A: Integer;' + #13#10 +
    '  LongVarName: String;' + #13#10 +
    '  Mid: Boolean;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Colons should be aligned to the longest name (LongVarName = 11 chars)
    Assert.IsTrue(LResult.Contains(
      'var' + #13#10 +
      '  A          : Integer;' + #13#10 +
      '  LongVarName: String;' + #13#10 +
      '  Mid        : Boolean;'),
      'Colons should be aligned to the longest variable name. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_AbsoluteFollowsTarget;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure Foo;' + #13#10 +
    'var' + #13#10 +
    '  TypRec: TTypRec absolute Key;' + #13#10 +
    '  Zebra: String;' + #13#10 +
    '  Key: TKey;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Key must come before TypRec (absolute Key), even though T > K alphabetically
    Assert.IsTrue(LResult.Contains(
      '  Key   : TKey;' + #13#10 +
      '  TypRec: TTypRec absolute Key;' + #13#10 +
      '  Zebra : String;'),
      'Absolute var must follow its target variable. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_Idempotent;
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
    'procedure Foo;' + #13#10 +
    'var' + #13#10 +
    '  Counter: Integer;' + #13#10 +
    '  Art: PArt;' + #13#10 +
    '  TblArt: ITableAccess;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2, 'Var section formatting should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_PreservesTrailingComment;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // Trailing comments on var declarations must be preserved after sorting
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure Foo;' + #13#10 +
    'var' + #13#10 +
    '  Count: Integer; // vergebene Preise' + #13#10 +
    '  Active: Boolean;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains('// vergebene Preise'),
      'Trailing comment must be preserved. Actual:' + #13#10 + LResult);
    Assert.IsTrue(LResult.Contains('Count : Integer; // vergebene Preise'),
      'Comment must stay on the Count line. Actual:' + #13#10 + LResult);

    // Idempotence
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2, 'Var with trailing comment should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_SplitsMultiVar;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  // "I, J: Integer" should be split into two separate declarations
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure Foo;' + #13#10 +
    'var' + #13#10 +
    '  I, J: Integer;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      'var' + #13#10 +
      '  I: Integer;' + #13#10 +
      '  J: Integer;'),
      'Multi-var should be split into one per line. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_SplitsMultiVar_ThreeVars;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  // Three variables on one line should all be split
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure Foo;' + #13#10 +
    'var' + #13#10 +
    '  X, Y, Z: Double;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      'var' + #13#10 +
      '  X: Double;' + #13#10 +
      '  Y: Double;' + #13#10 +
      '  Z: Double;'),
      'Three vars on one line should become three lines. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_SplitsAndSorts;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // Multi-var should be split, then everything sorted and aligned
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure Foo;' + #13#10 +
    'var' + #13#10 +
    '  Name: String;' + #13#10 +
    '  I, J: Integer;' + #13#10 +
    '  Active: Boolean;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // After split+sort: Active, I, J, Name — colons aligned to "Active" (6 chars)
    Assert.IsTrue(LResult.Contains(
      'var' + #13#10 +
      '  Active: Boolean;' + #13#10 +
      '  I     : Integer;' + #13#10 +
      '  J     : Integer;' + #13#10 +
      '  Name  : String;'),
      'Multi-var should be split, sorted and aligned. Actual:' + #13#10 + LResult);

    // Idempotence
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2, 'Split+sort+align should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_SkipsUnitLevel;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  // Unit-level var sections should NOT be sorted/aligned by this logic
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'var' + #13#10 +
    '  Zebra: String;' + #13#10 +
    '  Alpha: Integer;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Unit-level vars should keep their original order
    Assert.IsTrue(LResult.Contains('Zebra') and LResult.Contains('Alpha'),
      'Unit-level var section should be preserved. Actual:' + #13#10 + LResult);
    Assert.IsTrue(Pos('Zebra', LResult) < Pos('Alpha', LResult),
      'Unit-level vars should NOT be sorted. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_VarSection.TestFormatVarSection_SingleVar;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'procedure Foo;' + #13#10 +
    'var' + #13#10 +
    '  Result: String;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Single var should be formatted with proper indent, no extra padding
    Assert.IsTrue(LResult.Contains(
      'var' + #13#10 +
      '  Result: String;'),
      'Single var should have 2-space indent and colon right after name. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

end.
