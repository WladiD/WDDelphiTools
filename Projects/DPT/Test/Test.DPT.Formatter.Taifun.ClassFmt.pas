unit Test.DPT.Formatter.Taifun.ClassFmt;

interface

uses
  System.Classes,
  System.SysUtils,

  DUnitX.TestFramework,

  Test.DPT.Formatter.Taifun.Base;

type

  [TestFixture]
  TTestTaifunFormatter_Class = class(TTestTaifunFormatterBase)
  public
    [Test]
    procedure TestFormatClass_FieldsSortedAlphabetically;
    [Test]
    procedure TestFormatClass_ConstructorFirst;
    [Test]
    procedure TestFormatClass_DestructorAfterConstructor;
    [Test]
    procedure TestFormatClass_MethodsSortedAlphabetically;
    [Test]
    procedure TestFormatClass_PropertiesSortedAlphabetically;
    [Test]
    procedure TestFormatClass_OverloadsSortedBySignature;
    [Test]
    procedure TestFormatClass_FieldsBeforeMethodsBeforeProperties;
    [Test]
    procedure TestFormatClass_ClassMethodsAfterCtorBeforeRegularMethods;
    [Test]
    procedure TestFormatClass_ClassCtorBeforeRegularCtor;
    [Test]
    procedure TestFormatClass_AlignsProcedureFunctionToColumn10;
    [Test]
    procedure TestFormatClass_AlignsConstructorDestructorToColumn12;
    [Test]
    procedure TestFormatClass_AlignsClassMethodsToColumn16;
    [Test]
    procedure TestFormatClass_PropertyUsesSingleSpace;
    [Test]
    procedure TestFormatClass_AllFourAlignmentGroupsIndependent;
    [Test]
    procedure TestFormatClass_VisibilityKeyword3Spaces;
    [Test]
    procedure TestFormatClass_Member4Spaces;
    [Test]
    procedure TestFormatClass_NoBlankLinesInClassBody;
    [Test]
    procedure TestFormatClass_InImplementationSection;
    [Test]
    procedure TestFormatClass_SkipsFormClass;
    [Test]
    procedure TestFormatClass_VisibilityOrderPreserved;
    [Test]
    procedure TestFormatClass_SectionWithDirectiveNotSorted;
    [Test]
    procedure TestFormatClass_OtherSectionsFormattedDespiteDirective;
    [Test]
    procedure TestFormatClass_CommentMovesWithMethodOnSort;
    [Test]
    procedure TestFormatClass_AttributeMovesWithMethodOnSort;
    [Test]
    procedure TestFormatClass_TrailingBraceCommentOnMember;
    [Test]
    procedure TestFormatClass_Idempotent;
    [Test]
    procedure TestFormatClass_OnPropertiesAfterRegularProperties;
    [Test]
    procedure TestFormatClass_OnPropertiesSortedAlphabetically;
    [Test]
    procedure TestFormatClass_OnPropertyCommentMovesWithProperty;
    [Test]
    procedure TestFormatClass_MixedPropertiesIdempotent;
    [Test]
    procedure TestFormatClass_DirectiveIndentPreservedAtVisibilityBoundary;
    [Test]
    procedure TestFormatClass_TrailingLineCommentMovesWithMethod;
    [Test]
    procedure TestFormatClass_ClassVarPreserved;
  end;

implementation

{ TTestTaifunFormatter_Class }

procedure TTestTaifunFormatter_Class.TestFormatClass_FieldsSortedAlphabetically;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   strict private' + #13#10 +
    '    FZebra: Integer;' + #13#10 +
    '    FAlpha: String;' + #13#10 +
    '    FMiddle: Boolean;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains(
    '    FAlpha: String;' + #13#10 +
    '    FMiddle: Boolean;' + #13#10 +
    '    FZebra: Integer;'),
    'Fields should be sorted alphabetically. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_ConstructorFirst;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    procedure Zebra;' + #13#10 +
    '    constructor Create;' + #13#10 +
    '    procedure Alpha;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(Pos('constructor Create;', LResult) < Pos('procedure Alpha;', LResult),
    'Constructor must come before Alpha. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('procedure Alpha;', LResult) < Pos('procedure Zebra;', LResult),
    'Alpha must come before Zebra. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_DestructorAfterConstructor;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    destructor Destroy; override;' + #13#10 +
    '    procedure Alpha;' + #13#10 +
    '    constructor Create;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(Pos('constructor Create;', LResult) < Pos('destructor  Destroy', LResult),
    'Constructor must come before Destructor. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('destructor  Destroy', LResult) < Pos('procedure Alpha;', LResult),
    'Destructor must come directly after Constructor, before Alpha. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_MethodsSortedAlphabetically;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    procedure Zebra;' + #13#10 +
    '    procedure Alpha;' + #13#10 +
    '    procedure Middle;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(
    (Pos('procedure Alpha;', LResult) < Pos('procedure Middle;', LResult)) and
    (Pos('procedure Middle;', LResult) < Pos('procedure Zebra;', LResult)),
    'Methods should be sorted alphabetically. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_PropertiesSortedAlphabetically;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   strict private' + #13#10 +
    '    FName: String;' + #13#10 +
    '    FValue: Integer;' + #13#10 +
    '   public' + #13#10 +
    '    property Zebra: String read FName;' + #13#10 +
    '    property Alpha: Integer read FValue;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(Pos('property Alpha', LResult) < Pos('property Zebra', LResult),
    'Properties should be sorted alphabetically. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_OverloadsSortedBySignature;
var
  LResult: string;
  LSource: string;
begin
  // Overloads keep their original relative order (stable sort).
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    procedure Bar(A: String); overload;' + #13#10 +
    '    procedure Bar(A: Integer); overload;' + #13#10 +
    '    procedure Alpha;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Alpha comes first (alphabetical), then Bar overloads in ORIGINAL order.
  Assert.IsTrue(Pos('procedure Alpha;', LResult) < Pos('procedure Bar(A: String)', LResult),
    'Alpha must come before Bar. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('procedure Bar(A: String)', LResult) < Pos('procedure Bar(A: Integer)', LResult),
    'Overloads keep original relative order (String before Integer as in source). Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_FieldsBeforeMethodsBeforeProperties;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    property AProp: Integer read FA;' + #13#10 +
    '    procedure AMethod;' + #13#10 +
    '    FAField: Integer;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(Pos('FAField:', LResult) < Pos('procedure AMethod;', LResult),
    'Field must come before method. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('procedure AMethod;', LResult) < Pos('property AProp:', LResult),
    'Method must come before property. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_ClassMethodsAfterCtorBeforeRegularMethods;
var
  LResult: string;
  LSource: string;
begin
  // Class methods come after constructor/destructor but before regular methods
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    FField: Integer;' + #13#10 +
    '    procedure RegularMethod;' + #13#10 +
    '    class function Select: Boolean;' + #13#10 +
    '    constructor Create;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Order: Field -> constructor -> class function -> regular method
  Assert.IsTrue(Pos('FField:', LResult) < Pos('constructor Create;', LResult),
    'Field must come before constructor. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('constructor Create;', LResult) < Pos('class function', LResult),
    'Constructor must come before class function. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('class function', LResult) < Pos('procedure RegularMethod;', LResult),
    'Class function must come before regular method. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_ClassCtorBeforeRegularCtor;
var
  LResult: string;
  LSource: string;
begin
  // class constructor/destructor come before regular constructor/destructor
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    constructor Create;' + #13#10 +
    '    class constructor ClassCreate;' + #13#10 +
    '    destructor Destroy; override;' + #13#10 +
    '    class destructor ClassDestroy;' + #13#10 +
    '    procedure DoWork;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Order: class ctor -> class dtor -> constructor -> destructor -> regular method
  Assert.IsTrue(Pos('class constructor ClassCreate;', LResult) < Pos('class destructor', LResult),
    'Class constructor must come before class destructor. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('class destructor', LResult) < Pos('constructor Create;', LResult),
    'Class destructor must come before regular constructor. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('constructor Create;', LResult) < Pos('destructor  Destroy', LResult),
    'Regular constructor must come before destructor. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('destructor  Destroy', LResult) < Pos('procedure DoWork;', LResult),
    'Destructor must come before regular method. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_AlignsProcedureFunctionToColumn10;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    procedure DoA;' + #13#10 +
    '    function GetB: Integer;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // procedure(9)+1 = function(8)+2, both names at col 14 (incl. 4-space indent + keyword + padding)
  Assert.IsTrue(LResult.Contains(
    '    procedure DoA;' + #13#10 +
    '    function  GetB: Integer;'),
    'function should get 2 spaces to align with procedure. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_AlignsConstructorDestructorToColumn12;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    constructor Create;' + #13#10 +
    '    destructor Destroy; override;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // constructor(11)+1 space, destructor(10)+2 spaces
  Assert.IsTrue(LResult.Contains(
    '    constructor Create;' + #13#10 +
    '    destructor  Destroy; override;'),
    'destructor should get 2 spaces to align with constructor. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_AlignsClassMethodsToColumn16;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    class procedure DoA;' + #13#10 +
    '    class function GetB: Integer;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // class procedure(15)+1, class function(14)+2
  Assert.IsTrue(LResult.Contains(
    '    class procedure DoA;' + #13#10 +
    '    class function  GetB: Integer;'),
    'class function should get 2 spaces to align with class procedure. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_PropertyUsesSingleSpace;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   strict private' + #13#10 +
    '    FA: Integer;' + #13#10 +
    '   public' + #13#10 +
    '    procedure DoWork;' + #13#10 +
    '    property Name: Integer read FA;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // property always 1 space, not aligned with procedure
  Assert.IsTrue(LResult.Contains('    property Name:'),
    'property should have only 1 space between keyword and name. Actual:' + #13#10 + LResult);
  Assert.IsFalse(LResult.Contains('    property  Name:'),
    'property should NOT have 2 spaces (no alignment with methods). Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_AllFourAlignmentGroupsIndependent;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    FField: Integer;' + #13#10 +
    '    class procedure ClassProc;' + #13#10 +
    '    class function ClassFunc: Integer;' + #13#10 +
    '    constructor Create;' + #13#10 +
    '    destructor Destroy; override;' + #13#10 +
    '    procedure RegularProc;' + #13#10 +
    '    function RegularFunc: Integer;' + #13#10 +
    '    property RegularProp: Integer read FField;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Class method group: col 16
  Assert.IsTrue(LResult.Contains('class procedure ClassProc'),
    'class procedure 1 space. Actual:' + #13#10 + LResult);
  Assert.IsTrue(LResult.Contains('class function  ClassFunc'),
    'class function 2 spaces. Actual:' + #13#10 + LResult);

  // Constructor/destructor group: col 12
  Assert.IsTrue(LResult.Contains('constructor Create'),
    'constructor 1 space. Actual:' + #13#10 + LResult);
  Assert.IsTrue(LResult.Contains('destructor  Destroy'),
    'destructor 2 spaces. Actual:' + #13#10 + LResult);

  // Regular proc/func group: col 10
  Assert.IsTrue(LResult.Contains('procedure RegularProc'),
    'procedure 1 space. Actual:' + #13#10 + LResult);
  Assert.IsTrue(LResult.Contains('function  RegularFunc'),
    'function 2 spaces. Actual:' + #13#10 + LResult);

  // Property: 1 space
  Assert.IsTrue(LResult.Contains('property RegularProp'),
    'property 1 space. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_VisibilityKeyword3Spaces;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    'private' + #13#10 +
    '    FA: Integer;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains(#13#10 + '   private'),
    'Visibility keyword must have 3 spaces indent. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_Member4Spaces;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   private' + #13#10 +
    'FA: Integer;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains(#13#10 + '    FA:'),
    'Class member must have 4 spaces indent. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_NoBlankLinesInClassBody;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   strict private' + #13#10 +
    #13#10 +
    '    FA: Integer;' + #13#10 +
    #13#10 +
    '   public' + #13#10 +
    #13#10 +
    '    procedure DoWork;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // No double newlines inside class body (between visibility keyword and member)
  Assert.IsFalse(LResult.Contains('strict private' + #13#10 + #13#10),
    'No blank line after visibility keyword. Actual:' + #13#10 + LResult);
  Assert.IsFalse(LResult.Contains('FA: Integer;' + #13#10 + #13#10),
    'No blank line between members. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_InImplementationSection;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'implementation' + #13#10 +
    'type' + #13#10 +
    '  TPrivateFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    procedure Zebra;' + #13#10 +
    '    procedure Alpha;' + #13#10 +
    '  end;' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(Pos('procedure Alpha;', LResult) < Pos('procedure Zebra;', LResult),
    'Local class in implementation must also be sorted. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_SkipsFormClass;
var
  LResult: string;
  LSource: string;
begin
  // Form class: DFM-generated members BEFORE the first visibility keyword.
  // Only the default section (DFM members) must stay untouched.
  // Explicit visibility sections (like 'private') ARE still formatted.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFormFoo = class(TForm)' + #13#10 +
    '    edName: TEdit;' + #13#10 +
    '    lbCaption: TLabel;' + #13#10 +
    '   private' + #13#10 +
    '    FZebra: Integer;' + #13#10 +
    '    FAlpha: String;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // DFM components in default section must be preserved in original order
  Assert.IsTrue(Pos('edName: TEdit', LResult) < Pos('lbCaption: TLabel', LResult),
    'DFM components must keep original order. Actual:' + #13#10 + LResult);
  Assert.IsTrue(LResult.Contains('edName: TEdit;'),
    'DFM components must be preserved. Actual:' + #13#10 + LResult);

  // But the explicit 'private' section IS sorted: FAlpha before FZebra
  Assert.IsTrue(Pos('FAlpha', LResult) < Pos('FZebra', LResult),
    'Explicit visibility sections of form classes are still sorted. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_VisibilityOrderPreserved;
var
  LResult: string;
  LSource: string;
begin
  // Deliberately unusual order: public first, then strict private
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    procedure DoWork;' + #13#10 +
    '   strict private' + #13#10 +
    '    FA: Integer;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Public must still come BEFORE strict private (original order preserved)
  Assert.IsTrue(Pos('   public', LResult) < Pos('strict private', LResult),
    'Visibility order must NOT be reordered. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_SectionWithDirectiveNotSorted;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    procedure Zebra;' + #13#10 +
    '    {$IFDEF TEST}' + #13#10 +
    '    procedure Alpha;' + #13#10 +
    '    {$ENDIF}' + #13#10 +
    '    procedure Middle;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // With {$IFDEF}, section must not be sorted -- Zebra stays before Alpha
  Assert.IsTrue(Pos('Zebra', LResult) < Pos('Alpha', LResult),
    'Section with $IFDEF must not be sorted. Actual:' + #13#10 + LResult);
  Assert.IsTrue(LResult.Contains('{$IFDEF TEST}'),
    'Directive must be preserved. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_OtherSectionsFormattedDespiteDirective;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   strict private' + #13#10 +
    '    FZebra: Integer;' + #13#10 +
    '    FAlpha: String;' + #13#10 +
    '   public' + #13#10 +
    '    procedure Beta;' + #13#10 +
    '    {$IFDEF TEST}' + #13#10 +
    '    procedure Gamma;' + #13#10 +
    '    {$ENDIF}' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // strict private section MUST still be sorted (no directive there)
  Assert.IsTrue(Pos('FAlpha', LResult) < Pos('FZebra', LResult),
    'strict private section must be sorted despite directive in public. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_CommentMovesWithMethodOnSort;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    // Comment for Zebra method' + #13#10 +
    '    procedure Zebra;' + #13#10 +
    '    procedure Alpha;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Comment must travel with Zebra after sorting
  Assert.IsTrue(Pos('procedure Alpha;', LResult) < Pos('// Comment for Zebra', LResult),
    'Comment must move to the new position of Zebra. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('// Comment for Zebra', LResult) < Pos('procedure Zebra;', LResult),
    'Comment must stay right before Zebra. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_AttributeMovesWithMethodOnSort;
var
  LResult: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    [Deprecated]' + #13#10 +
    '    procedure Zebra;' + #13#10 +
    '    procedure Alpha;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Attribute must travel with Zebra after sorting
  Assert.IsTrue(Pos('procedure Alpha;', LResult) < Pos('[Deprecated]', LResult),
    'Attribute must move with Zebra. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('[Deprecated]', LResult) < Pos('procedure Zebra;', LResult),
    'Attribute must stay right before Zebra. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_TrailingBraceCommentOnMember;
var
  LResult: string;
  LResult2: string;
  LSource: string;
begin
  // A brace comment on the same line as a member's semicolon must be
  // preserved in place -- not moved to its own line or dropped.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   strict private const' + #13#10 +
    '    MyValue = 1.5; { important ratio }' + #13#10 +
    '   strict private' + #13#10 +
    '    FData: Integer;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains('MyValue = 1.5; { important ratio }'),
    'Trailing brace comment must stay on the same line as the member. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_Idempotent;
var
  LResult: string;
  LResult2: string;
  LSource: string;
begin
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   strict private' + #13#10 +
    '    FZebra: Integer;' + #13#10 +
    '    FAlpha: String;' + #13#10 +
    '   public' + #13#10 +
    '    constructor Create;' + #13#10 +
    '    procedure DoWork;' + #13#10 +
    '    function GetValue: Integer;' + #13#10 +
    '    destructor Destroy; override;' + #13#10 +
    '    property Alpha: String read FAlpha;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);
  LResult2 := FormatSource(LResult);
  Assert.AreEqual(LResult, LResult2, 'Class formatting should be idempotent');
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_OnPropertiesAfterRegularProperties;
var
  LResult: string;
  LSource: string;
begin
  // On* event-handler properties must be placed after regular properties
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    property OnClick: TNotifyEvent read FOnClick write FOnClick;' + #13#10 +
    '    property Name: String read FName;' + #13#10 +
    '    property OnChange: TNotifyEvent read FOnChange write FOnChange;' + #13#10 +
    '    property Value: Integer read FValue;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Regular properties first (Name, Value), then On* properties (OnChange, OnClick)
  Assert.IsTrue(Pos('property Name:', LResult) < Pos('property Value:', LResult),
    'Name must come before Value (alphabetical). Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('property Value:', LResult) < Pos('property OnChange:', LResult),
    'Value (regular) must come before OnChange (event). Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('property OnChange:', LResult) < Pos('property OnClick:', LResult),
    'OnChange must come before OnClick (alphabetical within events). Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_OnPropertiesSortedAlphabetically;
var
  LResult: string;
  LSource: string;
begin
  // On* properties within their group are sorted alphabetically
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    property OnZoom: TNotifyEvent read FOnZoom;' + #13#10 +
    '    property OnActivate: TNotifyEvent read FOnActivate;' + #13#10 +
    '    property OnResize: TNotifyEvent read FOnResize;' + #13#10 +
    '    property OnClose: TNotifyEvent read FOnClose;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  Assert.IsTrue(
    (Pos('OnActivate', LResult) < Pos('OnClose', LResult)) and
    (Pos('OnClose', LResult) < Pos('OnResize', LResult)) and
    (Pos('OnResize', LResult) < Pos('OnZoom', LResult)),
    'On* properties must be sorted alphabetically. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_OnPropertyCommentMovesWithProperty;
var
  LResult: string;
  LSource: string;
begin
  // Comments before On* properties must travel with the property during sort
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   public' + #13#10 +
    '    // Fires when clicked' + #13#10 +
    '    property OnClick: TNotifyEvent read FOnClick;' + #13#10 +
    '    property Name: String read FName;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Name (regular) must come first, then the comment + OnClick (event)
  Assert.IsTrue(Pos('property Name:', LResult) < Pos('// Fires when clicked', LResult),
    'Regular property must come before the On* comment. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('// Fires when clicked', LResult) < Pos('property OnClick:', LResult),
    'Comment must stay right before OnClick. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_MixedPropertiesIdempotent;
var
  LResult: string;
  LResult2: string;
  LSource: string;
begin
  // Full scenario: methods + regular props + On* props
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   strict private' + #13#10 +
    '    FName: String;' + #13#10 +
    '    FOnClick: TNotifyEvent;' + #13#10 +
    '   public' + #13#10 +
    '    property OnClick: TNotifyEvent read FOnClick;' + #13#10 +
    '    procedure DoWork;' + #13#10 +
    '    property Name: String read FName;' + #13#10 +
    '    property OnChange: TNotifyEvent read FOnClick;' + #13#10 +
    '    property Value: Integer read FName;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Verify ordering: method -> regular props -> On* props
  Assert.IsTrue(Pos('procedure DoWork;', LResult) < Pos('property Name:', LResult),
    'Method must come before properties. Actual:' + #13#10 + LResult);
  Assert.IsTrue(Pos('property Value:', LResult) < Pos('property OnChange:', LResult),
    'Regular properties before On* events. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_DirectiveIndentPreservedAtVisibilityBoundary;
var
  LResult: string;
  LResult2: string;
  LSource: string;
begin
  // A {$ENDIF} directive before a visibility keyword must keep its original
  // member-level indent (4 spaces), not be re-indented to the visibility
  // keyword level (3 spaces).
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   private' + #13#10 +
    '    {$IFDEF TEST}' + #13#10 +
    '    procedure ConditionalMethod;' + #13#10 +
    '    {$ENDIF TEST}' + #13#10 +
    '   public' + #13#10 +
    '    procedure DoWork;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // {$ENDIF} must have 4-space indent (member level), not 3-space (visibility level)
  Assert.IsTrue(LResult.Contains(#13#10 + '    {$ENDIF TEST}' + #13#10),
    '{$ENDIF} must keep 4-space member indent. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_TrailingLineCommentMovesWithMethod;
var
  LResult: string;
  LSource: string;
begin
  // A trailing // comment on a method must travel with that method when sorted,
  // not migrate to the next member.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TFoo = class' + #13#10 +
    '   strict private' + #13#10 +
    '    procedure RemoveObserver;' + #13#10 +
    '    function  Compare(const A, B: Pointer): Integer; // IEvaluator' + #13#10 +
    '    function  MatchesFilter(AData: Pointer): Boolean; // IEvaluator' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // After sorting: Compare, MatchesFilter, RemoveObserver
  // Each // IEvaluator must stay on its original method
  Assert.IsTrue(LResult.Contains('Compare(const A, B: Pointer): Integer; // IEvaluator'),
    'Comment must stay on Compare. Actual:' + #13#10 + LResult);
  Assert.IsTrue(LResult.Contains('MatchesFilter(AData: Pointer): Boolean; // IEvaluator'),
    'Comment must stay on MatchesFilter. Actual:' + #13#10 + LResult);
  Assert.IsFalse(LResult.Contains('RemoveObserver; // IEvaluator'),
    'Comment must NOT migrate to RemoveObserver. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_Class.TestFormatClass_ClassVarPreserved;
var
  LResult: string;
  LSource: string;
begin
  // "class var" fields inside a class must not be split into separate
  // "class" + "var" tokens with banners inserted between them.
  // Reproduces the exact structure from Base.Map.Query.pas:
  // - class forward declaration
  // - second class with strict private type section (record + pointer alias)
  // - strict private section with class var fields
  // - strict private section with class methods
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  CMapQuery = class;' + #13#10 +
    '  TPOITermin = record' + #13#10 +
    '   Address: String;' + #13#10 +
    '   Name: String;' + #13#10 +
    '  end;' + #13#10 +
    '  TPOITerminList = Array of TPOITermin;' + #13#10 +
    '  TGeoCoordListenerEntry = packed record' + #13#10 +
    '    Data: Pointer;' + #13#10 +
    '  end;' + #13#10 +
    '  IMapQuery = interface' + #13#10 +
    '    function Ad2Coord(const Anschrift: String): Integer; overload;' + #13#10 +
    '    function Ad2Coord(const Anschrift: String; var OutLatitude, OutLongitude: Double): Boolean; overload;' + #13#10 +
    '    function GeoCoordToAddress(const AGeoCoord: Double; ACtyToCountry: TFunc<String,String>; out AAnschrift: String): Boolean; overload;' + #13#10 +
    '    function GetBaseURL: String;' + #13#10 +
    '    property BaseURL: String read GetBaseURL;' + #13#10 +
    '  end;' + #13#10 +
    #13#10 +
    '  CMapQuery = class(TInterfacedObject,IMapQuery)' + #13#10 +
    '   strict private' + #13#10 +
    '    type' + #13#10 +
    '     TAnschriftOpt = Integer;' + #13#10 +
    '   strict private' + #13#10 +
    '    class var FCrc32Tab: array [0 .. 255] of Longint;' + #13#10 +
    '    class var FCache: Integer;' + #13#10 +
    '   strict private' + #13#10 +
    '    class procedure InitCache; static;' + #13#10 +
    '   public' + #13#10 +
    '    procedure DoWork;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // "class var" must stay together on one line
  Assert.IsTrue(LResult.Contains('class var FCrc32Tab: array [0 .. 255] of Longint;'),
    '"class var FCrc32Tab" must stay on one line. Actual:' + #13#10 + LResult);
  Assert.IsTrue(LResult.Contains('class var FCache: Integer;'),
    '"class var FCache" must stay on one line. Actual:' + #13#10 + LResult);

  // No method banners between class and var
  Assert.IsFalse(LResult.Contains('class' + #13#10 + #13#10 + '{ ---'),
    'No banner between class and var keywords. Actual:' + #13#10 + LResult);
end;

end.
