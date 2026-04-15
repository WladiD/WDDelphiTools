unit Test.DPT.Formatter.Taifun.Structure;

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
  TTestTaifunFormatter_Structure = class(TTestTaifunFormatterBase)
  public
    [Test]
    procedure TestFormatSections;
    [Test]
    procedure TestFormatSections_IdempotentWithResourceString;
    [Test]
    procedure TestFormatInterface_PreservesEmptyLineBeforeType;
    [Test]
    procedure TestFormatInterface_NoExtraEmptyLineBeforeUses;
    [Test]
    procedure TestNoRedundantSeparatorAfterImplementation;
    [Test]
    procedure TestFormatUnitEnd_PreservesTrailingDirectives;
    [Test]
    procedure TestFormatUnit_NoExtraLineBeforeDirective;
    [Test]
    procedure TestFormatInterface_ReplacesSlashesBanner;
    [Test]
    procedure TestFormatTypeSection_UsesShortBanner;
  end;

implementation

{ TTestTaifunFormatter_Structure }

procedure TTestTaifunFormatter_Structure.TestFormatSections;
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

procedure TTestTaifunFormatter_Structure.TestFormatSections_IdempotentWithResourceString;
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

procedure TTestTaifunFormatter_Structure.TestFormatInterface_PreservesEmptyLineBeforeType;
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

procedure TTestTaifunFormatter_Structure.TestFormatInterface_NoExtraEmptyLineBeforeUses;
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

procedure TTestTaifunFormatter_Structure.TestNoRedundantSeparatorAfterImplementation;
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

procedure TTestTaifunFormatter_Structure.TestFormatUnitEnd_PreservesTrailingDirectives;
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

procedure TTestTaifunFormatter_Structure.TestFormatUnit_NoExtraLineBeforeDirective;
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

procedure TTestTaifunFormatter_Structure.TestFormatInterface_ReplacesSlashesBanner;
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

procedure TTestTaifunFormatter_Structure.TestFormatTypeSection_UsesShortBanner;
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
