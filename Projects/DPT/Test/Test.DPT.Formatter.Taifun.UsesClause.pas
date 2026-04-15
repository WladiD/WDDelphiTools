unit Test.DPT.Formatter.Taifun.UsesClause;

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
  TTestTaifunFormatter_UsesClause = class(TTestTaifunFormatterBase)
  public
    [Test]
    procedure TestFormatUsesClause;
    [Test]
    procedure TestFormatUsesClause_WithCompilerDirectives;
    [Test]
    procedure TestFormatUsesClause_SortsAlphabetically;
    [Test]
    procedure TestFormatUsesClause_GroupsByNamespace;
    [Test]
    procedure TestFormatUsesClause_SortsAndGroups;
    [Test]
    procedure TestFormatUsesClause_SortsAndGroups_Idempotent;
    [Test]
    procedure TestFormatUsesClause_BaseAndBaseUI_SeparateGroups;
    [Test]
    procedure TestFormatUsesClause_SkipsSortWithDirectives;
    [Test]
    procedure TestFormatUsesClause_ThirdPartyGrouped;
    [Test]
    procedure TestFormatUsesClause_DelphiRTL_SingleBlock;
    [Test]
    procedure TestFormatUsesClause_DelphiRTL_SortedAcrossNamespaces;
    [Test]
    procedure TestFormatUsesClause_MultipleAppNamespaces;
  end;

implementation

{ TTestTaifunFormatter_UsesClause }

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause;
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

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_WithCompilerDirectives;
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

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_SortsAlphabetically;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit; interface uses Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls; implementation end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      'uses' + #13#10 + #13#10 +
      '  Vcl.Controls,' + #13#10 +
      '  Vcl.ExtCtrls,' + #13#10 +
      '  Vcl.StdCtrls;'),
      'Units within the same group should be sorted alphabetically. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_GroupsByNamespace;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit; interface uses System.SysUtils, Vcl.Controls, Base.Types; implementation end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // System and Vcl belong to the same Delphi-RTL block (no blank line),
    // but Base is a separate group (blank line before it).
    Assert.IsTrue(LResult.Contains(
      'uses' + #13#10 + #13#10 +
      '  System.SysUtils,' + #13#10 +
      '  Vcl.Controls,' + #13#10 + #13#10 +
      '  Base.Types;'),
      'Delphi RTL should be one block, Base a separate group. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_SortsAndGroups;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  // Deliberately out of order: Base before System, unsorted within groups
  LSource := 'unit MyUnit; interface uses Base.Types, System.SysUtils, Vcl.Controls, System.Classes; implementation end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      'uses' + #13#10 + #13#10 +
      '  System.Classes,' + #13#10 +
      '  System.SysUtils,' + #13#10 +
      '  Vcl.Controls,' + #13#10 + #13#10 +
      '  Base.Types;'),
      'Uses should be sorted by group then alphabetically. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_SortsAndGroups_Idempotent;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit; interface uses Base.Types, System.SysUtils, Vcl.Controls, System.Classes; implementation end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Idempotence check: format the result again
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2, 'Uses clause sorting and formatting should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_BaseAndBaseUI_SeparateGroups;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit; interface uses Base.UI.Controls, Base.Types, Base.UI.Utils, Base.Classes; implementation end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '  Base.Classes,' + #13#10 +
      '  Base.Types,' + #13#10 + #13#10 +
      '  Base.UI.Controls,' + #13#10 +
      '  Base.UI.Utils;'),
      'Base.* and Base.UI.* should be in separate groups with blank line. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_SkipsSortWithDirectives;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'uses' + #13#10 +
    '  {$IFDEF MSWINDOWS}' + #13#10 +
    '  Winapi.Windows,' + #13#10 +
    '  {$ENDIF}' + #13#10 +
    '  System.SysUtils;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // With directives, sorting should be skipped; original order preserved
    Assert.IsTrue(LResult.Contains('Winapi.Windows'),
      'Unit with directive should be preserved. Actual:' + #13#10 + LResult);
    Assert.IsTrue(LResult.Contains('{$IFDEF MSWINDOWS}'),
      'Compiler directive should be preserved. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_ThirdPartyGrouped;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  LSource := 'unit MyUnit; interface uses VirtualTrees, Spring.Collections, System.SysUtils, AdvMenus; implementation end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // System first, then third-party sorted alphabetically
    Assert.IsTrue(LResult.Contains(
      '  System.SysUtils,' + #13#10 + #13#10 +
      '  AdvMenus,' + #13#10 +
      '  Spring.Collections,' + #13#10 +
      '  VirtualTrees;'),
      'Third-party units should be grouped together and sorted. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_DelphiRTL_SingleBlock;
var
  LResult: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
begin
  // System.*, Winapi.*, Vcl.* all belong to one Delphi-RTL block — no blank
  // lines between them — while third-party and project units are separate.
  LSource := 'unit MyUnit; interface uses Winapi.Windows, Vcl.Forms, System.SysUtils, Spring.Collections, Tfw.Utils; implementation end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      'uses' + #13#10 + #13#10 +
      '  System.SysUtils,' + #13#10 +
      '  Vcl.Forms,' + #13#10 +
      '  Winapi.Windows,' + #13#10 + #13#10 +
      '  Spring.Collections,' + #13#10 + #13#10 +
      '  Tfw.Utils;'),
      'System/Vcl/Winapi must be in one block without blank lines. Actual:' + #13#10 + LResult);
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_DelphiRTL_SortedAcrossNamespaces;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // Within the Delphi-RTL block all units are sorted alphabetically
  // regardless of their namespace prefix.  Data.* and FMX.* also belong here.
  LSource := 'unit MyUnit; interface uses Vcl.Forms, Data.DB, System.Classes, FMX.Types, Winapi.Messages, System.SysUtils; implementation end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      'uses' + #13#10 + #13#10 +
      '  Data.DB,' + #13#10 +
      '  FMX.Types,' + #13#10 +
      '  System.Classes,' + #13#10 +
      '  System.SysUtils,' + #13#10 +
      '  Vcl.Forms,' + #13#10 +
      '  Winapi.Messages;'),
      'All Delphi RTL units must be in one alphabetically sorted block. Actual:' + #13#10 + LResult);

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2, 'Delphi RTL single-block formatting should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

procedure TTestTaifunFormatter_UsesClause.TestFormatUsesClause_MultipleAppNamespaces;
var
  LResult: string;
  LResult2: string;
  LSource: string;
  LUnit: TCompilationUnitSyntax;
  LUnit2: TCompilationUnitSyntax;
begin
  // All application namespaces (Tfw, Tpm, Tos, Ted, Pb, ...) belong to the
  // same group at the end.  Within the block they are sorted alphabetically,
  // so Pb.* < Ted.* < Tfw.* < Tpm.*.
  LSource := 'unit MyUnit; interface uses Tfw.Utils, Tpm.Bridge, Base.Types, Pb.Core, Ted.Import, System.SysUtils; implementation end.';

  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptPath);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    Assert.IsTrue(LResult.Contains(
      '  System.SysUtils,' + #13#10 + #13#10 +
      '  Base.Types,' + #13#10 + #13#10 +
      '  Pb.Core,' + #13#10 +
      '  Ted.Import,' + #13#10 +
      '  Tfw.Utils,' + #13#10 +
      '  Tpm.Bridge;'),
      'All app namespaces should be in one block, sorted alphabetically. Actual:' + #13#10 + LResult);

    // Idempotence check
    LUnit2 := FParser.Parse(LResult);
    try
      FFormatter.FormatUnit(LUnit2);
      LResult2 := FWriter.GenerateSource(LUnit2);
      Assert.AreEqual(LResult, LResult2, 'Multiple app namespaces formatting should be idempotent');
    finally
      LUnit2.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

end.
