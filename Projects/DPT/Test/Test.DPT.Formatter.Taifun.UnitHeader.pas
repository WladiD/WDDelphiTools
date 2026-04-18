unit Test.DPT.Formatter.Taifun.UnitHeader;

interface

uses
  System.Classes,
  System.SysUtils,

  DUnitX.TestFramework,

  Test.DPT.Formatter.Taifun.Base;

type

  [TestFixture]
  TTestTaifunFormatter_UnitHeader = class(TTestTaifunFormatterBase)
  public
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
    procedure TestFormatUnitHeader_PreservesDirectivesAfterInclude;
    [Test]
    procedure TestFormatUnitHeader_NoPlaceholderOnExisting;
    [Test]
    procedure TestFormatUnitHeader_PlaceholderOnNew;
    [Test]
    procedure TestFormatUnitHeader_PreservesDescriptionEvenIfUnitNameMismatched;
    [Test]
    procedure TestFormatUnitHeader_ExtractsDescriptionWithHyphen;
    [Test]
    procedure TestFormatUnitHeader_ExtractsMultilineDescription;
    [Test]
    procedure TestFormatUnitHeader_ExtractsDescriptionWithEnDash;
    [Test]
    procedure TestFormatUnitHeader_PreservesCommentsOutsideBanner;
    [Test]
    procedure TestFormatUnitHeader_UsesBaseDefineForBaseUnits;
    [Test]
    procedure TestFormatUnitHeader_OldPartialUnitNameNotPreserved;
    [Test]
    procedure TestFormatUnitHeader_OldExtendedUnitNameNotPreserved;
    [Test]
    procedure TestFormatUnitHeader_OldUnitNameWithDifferentCaseNotPreserved;
    [Test]
    procedure TestFormatUnitHeader_EnglishAuthorLabel;
  end;

implementation

{ TTestTaifunFormatter_UnitHeader }

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_CreatesNew;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);
  Assert.IsTrue(LResult.StartsWith(LExpectedHeader), 'Header was not created correctly: '#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_CorrectsExisting;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);
  Assert.IsTrue(LResult.StartsWith(LExpectedHeader), 'Header was not corrected properly: '#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_PreservesPerfect;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);
  // Should preserve 'Special description', 'The Real Author', and 'Base.Define.pas' exactly
  Assert.IsTrue(LResult.StartsWith(LExpectedHeader), 'Perfect header was modified: '#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_DoesNotOverwriteDescriptionWithLaterHyphens;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  // Should extract "The real description" and NOT "hyphen later on."
  Assert.IsTrue(LResult.Contains('// MyUnit - The real description'), 'Should extract the correct description. Actual result:'#13#10 + LResult);
  Assert.IsFalse(LResult.Contains('// MyUnit - hyphen later on.'), 'Should not overwrite description with later hyphens. Actual result:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_PreservesExtraComments;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  // Check if the extra comments about the unicorn were preserved
  Assert.IsTrue(LResult.Contains('// The unicorn jumped over the rainbow' + #13#10 + '// finding a pot of pure gold.'), 'Should preserve extra comments in the header banner. Actual result:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_PreservesDirectivesAfterInclude;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains('{$I Base.Define.pas}' + #13#10 + '{$DENYPACKAGEUNIT}'), 'Compiler directives after include should be preserved. Actual: ' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_NoPlaceholderOnExisting;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  // Should NOT contain the placeholder 'Kurzbeschreibung der Unit' because a banner already existed
  Assert.IsFalse(LResult.Contains('Kurzbeschreibung der Unit'), 'Should not add placeholder to existing banner');
  Assert.IsFalse(LResult.Contains('// MyUnit - '), 'Should not contain the " - " separator when description is empty');
  Assert.IsTrue(LResult.Contains('// MyUnit' + #13#10), 'Should have description line with only unit name. Actual result:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_PlaceholderOnNew;
var
  LResult: string;
  LSource: string;
begin
  // No banner at all
  LSource := 'unit MyUnit; interface end.';

  LResult := FormatSource(LSource);

  // SHOULD contain the placeholder 'Kurzbeschreibung der Unit' because it is a new banner
  Assert.IsTrue(LResult.Contains('Kurzbeschreibung der Unit'), 'Should add placeholder to new banner');
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_PreservesDescriptionEvenIfUnitNameMismatched;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  // Should have updated unit name AND preserved description
  Assert.IsTrue(LResult.Contains('Base.Db.Check - Basis-Utils für System-Checks'), 'Should update unit name and preserve description. Actual result:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_ExtractsDescriptionWithHyphen;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains('// Base.Kons.Common.Typ - Typdeklarationen: Konstanten (Programmübergreifend)'), 'Description with hyphen should be preserved correctly. Actual result:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_ExtractsMultilineDescription;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains('// MyUnit.Foo - The first line of the description.' + #13#10 + '//              The second line of the description.'), 'Multiline description should be preserved correctly under the unit name. Actual result:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_ExtractsDescriptionWithEnDash;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains('// MyUnit.Validator - JWT/JWKS Validation'), 'Description with en-dash should be preserved and normalized. Actual result:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_PreservesCommentsOutsideBanner;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  // The comment should stay inside the region block, NOT inside the banner
  var LExpectedRegion := '''
    {$REGION 'Some region'}
    // This comment should not be pulled into the banner
    {$ENDREGION}
    ''';

  Assert.IsTrue(LResult.Contains(LExpectedRegion), 'The comment inside the $REGION should be preserved exactly outside the banner. Actual result:'#13#10 + LResult);

  // The banner should not contain the comment
  Assert.IsFalse(LResult.Contains('//' + #13#10 + '// This comment should not be pulled into the banner' + #13#10 + '//' + #13#10 + '// ===='), 'The banner should not have absorbed the comment.');
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_UsesBaseDefineForBaseUnits;
var
  LResult: string;
  LSource: string;
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

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.StartsWith(LExpectedHeader), 'Should generate header with Base.Define.pas. Actual result:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_OldPartialUnitNameNotPreserved;
var
  LResult: String;
  LSource: String;
begin
  // When the banner contains an old/partial unit name (e.g. Tfw.Dms.Inbox instead of
  // Tfw.Dms.Inbox.Form), the formatter should replace it, not keep it as extra comment.
  LSource :=
    '// ======================================================================' + #13#10 +
    '//' + #13#10 +
    '// Tfw.Dms.Inbox' + #13#10 +
    '//' + #13#10 +
    '// Autor: Mister X' + #13#10 +
    '//' + #13#10 +
    '// ======================================================================' + #13#10 +
    #13#10 +
    '{$I Tfw.Define.pas}' + #13#10 +
    #13#10 +
    'unit Tfw.Dms.Inbox.Form;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // The old partial name must not appear in the output
  Assert.IsFalse(LResult.Contains('// Tfw.Dms.Inbox' + #13#10),
    'Old partial unit name should not be preserved in header. Actual:'#13#10 + Copy(LResult, 1, 400));
  // The correct unit name must be present
  Assert.IsTrue(LResult.Contains('// Tfw.Dms.Inbox.Form'),
    'Correct unit name should be in header. Actual:'#13#10 + Copy(LResult, 1, 400));
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_OldExtendedUnitNameNotPreserved;
var
  LResult: String;
  LSource: String;
begin
  // Mirror of _OldPartialUnitNameNotPreserved: when the banner contains the
  // unit name with an additional dotted suffix (e.g. Base.UI.Controls.Db.Bars.Dlg
  // instead of Base.UI.Controls.Db.Bars), the formatter should drop the old
  // extended name, not keep it as an extra comment.
  LSource :=
    '// ======================================================================' + #13#10 +
    '//' + #13#10 +
    '// Base.UI.Controls.Db.Bars.Dlg' + #13#10 +
    '//' + #13#10 +
    '// Autor: Mister X' + #13#10 +
    '//' + #13#10 +
    '// ======================================================================' + #13#10 +
    #13#10 +
    '{$I Base.Define.pas}' + #13#10 +
    #13#10 +
    'unit Base.UI.Controls.Db.Bars;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // The old extended name must not appear in the output
  Assert.IsFalse(LResult.Contains('// Base.UI.Controls.Db.Bars.Dlg'),
    'Old extended unit name should not be preserved in header. Actual:'#13#10 + Copy(LResult, 1, 400));
  // The correct unit name must be present
  Assert.IsTrue(LResult.Contains('// Base.UI.Controls.Db.Bars' + #13#10),
    'Correct unit name should be in header. Actual:'#13#10 + Copy(LResult, 1, 400));
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_OldUnitNameWithDifferentCaseNotPreserved;
var
  LResult: String;
  LSource: String;
begin
  // Reproducer for the Soa.Mailserver.Utils batch failure: when the banner contains
  // the unit name with a different casing (e.g. "Soa.Mailserver.utils" vs actual
  // "Soa.Mailserver.Utils"), the formatter should replace it, not keep it as extra
  // comment.
  LSource :=
    '// ======================================================================' + #13#10 +
    '//' + #13#10 +
    '// Soa.Mailserver.utils' + #13#10 +
    '//' + #13#10 +
    '// Autor: Mister X' + #13#10 +
    '//' + #13#10 +
    '// ======================================================================' + #13#10 +
    #13#10 +
    '{$I Tfw.Define.pas}' + #13#10 +
    #13#10 +
    'unit Soa.Mailserver.Utils;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // The old-cased name must not appear in the output
  Assert.IsFalse(LResult.Contains('// Soa.Mailserver.utils'),
    'Old differently-cased unit name should not be preserved in header. Actual:'#13#10 + Copy(LResult, 1, 400));
  // The correctly cased unit name must be present
  Assert.IsTrue(LResult.Contains('// Soa.Mailserver.Utils'),
    'Correctly cased unit name should be in header. Actual:'#13#10 + Copy(LResult, 1, 400));
end;

procedure TTestTaifunFormatter_UnitHeader.TestFormatUnitHeader_EnglishAuthorLabel;
var
  LResult: string;
  LSource: string;
begin
  // Some source files use English "Author:" instead of German "Autor:".
  // The extractor must recognise both and normalise the label to "Autor:".
  LSource :=
    '// ======================================================================' + #13#10 +
    '//' + #13#10 +
    '// MyUnit - Some description' + #13#10 +
    '//' + #13#10 +
    '// Author: Jane Doe' + #13#10 +
    '//' + #13#10 +
    '// ======================================================================' + #13#10 +
    #13#10 +
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains('// Autor: Jane Doe'),
    'English "Author:" must be recognised and emitted as German "Autor:". Actual:' + #13#10 + LResult);
  Assert.IsFalse(LResult.Contains('// Author: Jane Doe'),
    'Original English "Author:" line must not be duplicated in output. Actual:' + #13#10 + LResult);
  Assert.IsFalse(LResult.Contains('// Autor: Name'),
    'Default "Name" placeholder must not appear when an author was parsed. Actual:' + #13#10 + LResult);
end;

end.
