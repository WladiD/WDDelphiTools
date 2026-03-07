unit Test.ParseTree.Parser.ClassDecl;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Tokens,
  ParseTree.Parser,
  ParseTree.Nodes;

type
  [TestFixture]
  TParseTreeClassDeclTest = class
  private
    FParser: TParseTreeParser;
    function HasTriviaContaining(AToken: TSyntaxToken; const AText: string): Boolean;
    function GetFirstMemberToken(ASection: TVisibilitySectionSyntax; AMemberIndex: Integer): TSyntaxToken;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestParseClassDeclaration;
    [Test]
    procedure TestParseGenericClassDeclaration;
    [Test]
    procedure TestParseNestedClassDeclaration;
  end;

implementation

{ TParseTreeClassDeclTest }

procedure TParseTreeClassDeclTest.Setup;
begin
  FParser := TParseTreeParser.Create;
end;

procedure TParseTreeClassDeclTest.TearDown;
begin
  FParser.Free;
end;

function TParseTreeClassDeclTest.HasTriviaContaining(AToken: TSyntaxToken; const AText: string): Boolean;
var
  LTrivia: TSyntaxTrivia;
begin
  Result := False;
  if AToken = nil then Exit;
  for LTrivia in AToken.LeadingTrivia do
    if LTrivia.Text.Contains(AText) then
      Exit(True);
end;

function TParseTreeClassDeclTest.GetFirstMemberToken(ASection: TVisibilitySectionSyntax; AMemberIndex: Integer): TSyntaxToken;
begin
  Result := nil;
  if (AMemberIndex < ASection.Members.Count) and (ASection.Members[AMemberIndex].Tokens.Count > 0) then
    Result := ASection.Members[AMemberIndex].Tokens[0];
end;

procedure TParseTreeClassDeclTest.TestParseClassDeclaration;
const
  LSourceCode = '''
    unit Unit1;
    interface
    type
      /// <summary>Main application class</summary>
      TMyClass = class
      private
        /// <summary>Internal enumeration</summary>
        type
          TInnerEnum = (ieOne, ieTwo);
        /// <summary>Internal state flag</summary>
        var
          FInternalFlag: Boolean;
        /// <summary>Maximum number of items</summary>
        const
          {$IFDEF DEBUG}
          CMaxItems = 100;
          {$ELSE}
          CMaxItems = 200;
          {$ENDIF}
      strict private
        /// <summary>Strict private field for name storage</summary>
        FStrictField: string;
      protected
        /// <summary>Protected floating point field</summary>
        FProtField: Double;
        /// <summary>Updates internal state</summary>
        procedure InternalUpdate;
      strict protected
        /// <summary>Strict protected helper routine</summary>
        procedure StrictHelper;
      public
        /// <summary>Creates a new instance</summary>
        constructor Create;
        /// <summary>Creates from name and value</summary>
        constructor CreateWith(const AName: string; AValue: Integer);
        /// <summary>Releases all resources</summary>
        destructor Destroy; override;
        /// <summary>Performs the main action</summary>
        procedure DoSomething;
        /// <summary>Sets a single value</summary>
        procedure SetValue(AValue: Integer);
        /// <summary>Copies from another instance</summary>
        procedure CopyFrom(const ASource: TMyClass);
        /// <summary>Applies multiple changes at once</summary>
        procedure ApplyChanges(var ACounter: Integer; out AResult: string; const AFactor: Double);
        /// <summary>Initializes with optional count</summary>
        procedure Init(ACount: Integer = 10);
        /// <summary>Returns the current value</summary>
        function GetValue: Integer;
        /// <summary>Compares two strings with optional case sensitivity</summary>
        function Compare(const ALeft, ARight: string; AIgnoreCase: Boolean = False): Integer;
        /// <summary>Tries to retrieve an item by key</summary>
        function TryGetItem(const AKey: string; out AValue: Integer): Boolean;
        /// <summary>Factory method for creating instances</summary>
        class function CreateInstance: TMyClass;
        /// <summary>Frees all existing instances</summary>
        class procedure FreeAll;
        /// <summary>Initializes the class</summary>
        class constructor ClassCreate;
        /// <summary>Finalizes the class</summary>
        class destructor ClassDestroy;
        /// <summary>The current value</summary>
        property Value: Integer read GetValue;
      published
        /// <summary>Display name of this object</summary>
        property Name: string read FStrictField;
      end;
  ''';

var
  LTree: TCompilationUnitSyntax;
  LTypeSec: TTypeSectionSyntax;
  LTypeDecl: TTypeDeclarationSyntax;
  LVisSec: TVisibilitySectionSyntax;
begin
  LTree := FParser.Parse(LSourceCode);
  try
    Assert.IsNotNull(LTree.InterfaceSection, 'Interface missing');
    Assert.AreEqual(1, LTree.InterfaceSection.Declarations.Count);
    Assert.IsTrue(LTree.InterfaceSection.Declarations[0] is TTypeSectionSyntax);
    
    LTypeSec := TTypeSectionSyntax(LTree.InterfaceSection.Declarations[0]);
    Assert.AreEqual(1, LTypeSec.Declarations.Count, 'Should parse one type declaration');
    
    LTypeDecl := LTypeSec.Declarations[0];
    Assert.AreEqual('TMyClass', LTypeDecl.Identifier.Text);
    Assert.AreEqual('class', LTypeDecl.TypeTypeToken.Text);
    Assert.IsNotNull(LTypeDecl.EndKeyword, 'Should have end keyword');
    
    // Verify XML-Doc on class itself
    Assert.IsTrue(HasTriviaContaining(LTypeDecl.Identifier, '/// <summary>Main application class</summary>'),
      'TMyClass should have XML-Doc trivia');
    
    // Should have 6 visibility sections
    Assert.AreEqual(6, LTypeDecl.VisibilitySections.Count, 'Should have 6 visibility sections');
    
    // === Section 0: private ===
    LVisSec := LTypeDecl.VisibilitySections[0];
    Assert.AreEqual('private', LVisSec.VisibilityKeyword.Text);
    Assert.IsFalse(LVisSec.IsStrict, 'private should not be strict');
    Assert.AreEqual(4, LVisSec.Members.Count, 'private should have 4 members (type, var, const+IFDEF, const+ELSE)');
    // Check XML-Doc on private members
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Internal enumeration'),
      'type TInnerEnum should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Internal state flag'),
      'var FInternalFlag should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 2), 'Maximum number of items'),
      'const CMaxItems should have XML-Doc');
    // The {$IFDEF DEBUG} should be leading trivia of the first CMaxItems identifier (inside the const member)
    Assert.IsTrue(HasTriviaContaining(LVisSec.Members[2].Tokens[1], '{$IFDEF DEBUG}'),
      'CMaxItems should have {$IFDEF DEBUG} in trivia');
    // The {$ELSE} + second CMaxItems is parsed as a separate member (index 3)
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 3), '{$ELSE}'),
      'Second CMaxItems should have {$ELSE} in trivia');
    
    // === Section 1: strict private ===
    LVisSec := LTypeDecl.VisibilitySections[1];
    Assert.AreEqual('private', LVisSec.VisibilityKeyword.Text);
    Assert.IsTrue(LVisSec.IsStrict, 'strict private should be strict');
    Assert.AreEqual(1, LVisSec.Members.Count, 'strict private should have 1 member');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Strict private field for name storage'),
      'FStrictField should have XML-Doc');
    
    // === Section 2: protected ===
    LVisSec := LTypeDecl.VisibilitySections[2];
    Assert.AreEqual('protected', LVisSec.VisibilityKeyword.Text);
    Assert.IsFalse(LVisSec.IsStrict);
    Assert.AreEqual(2, LVisSec.Members.Count, 'protected should have 2 members');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Protected floating point field'),
      'FProtField should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Updates internal state'),
      'InternalUpdate should have XML-Doc');
    
    // === Section 3: strict protected ===
    LVisSec := LTypeDecl.VisibilitySections[3];
    Assert.AreEqual('protected', LVisSec.VisibilityKeyword.Text);
    Assert.IsTrue(LVisSec.IsStrict);
    Assert.AreEqual(1, LVisSec.Members.Count);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Strict protected helper routine'),
      'StrictHelper should have XML-Doc');
    
    // === Section 4: public ===
    LVisSec := LTypeDecl.VisibilitySections[4];
    Assert.AreEqual('public', LVisSec.VisibilityKeyword.Text);
    Assert.AreEqual(16, LVisSec.Members.Count, 'public should have 16 members');
    // 0: constructor Create;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Creates a new instance'),
      'constructor Create should have XML-Doc');
    // 1: constructor CreateWith(const AName: string; AValue: Integer);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Creates from name and value'),
      'constructor CreateWith should have XML-Doc');
    // 2: destructor Destroy; override;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 2), 'Releases all resources'),
      'destructor Destroy should have XML-Doc');
    // 3: procedure DoSomething;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 3), 'Performs the main action'),
      'DoSomething should have XML-Doc');
    // 4: procedure SetValue(AValue: Integer);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 4), 'Sets a single value'),
      'SetValue should have XML-Doc');
    // 5: procedure CopyFrom(const ASource: TMyClass);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 5), 'Copies from another instance'),
      'CopyFrom should have XML-Doc');
    // 6: procedure ApplyChanges(var ACounter: Integer; out AResult: string; const AFactor: Double);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 6), 'Applies multiple changes at once'),
      'ApplyChanges should have XML-Doc');
    // 7: procedure Init(ACount: Integer = 10);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 7), 'Initializes with optional count'),
      'Init should have XML-Doc');
    // 8: function GetValue: Integer;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 8), 'Returns the current value'),
      'GetValue should have XML-Doc');
    // 9: function Compare(const ALeft, ARight: string; AIgnoreCase: Boolean = False): Integer;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 9), 'Compares two strings with optional case sensitivity'),
      'Compare should have XML-Doc');
    // 10: function TryGetItem(const AKey: string; out AValue: Integer): Boolean;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 10), 'Tries to retrieve an item by key'),
      'TryGetItem should have XML-Doc');
    // 11: class function CreateInstance: TMyClass;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 11), 'Factory method for creating instances'),
      'CreateInstance should have XML-Doc');
    // 12: class procedure FreeAll;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 12), 'Frees all existing instances'),
      'FreeAll should have XML-Doc');
    // 13: class constructor ClassCreate;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 13), 'Initializes the class'),
      'class constructor ClassCreate should have XML-Doc');
    // 14: class destructor ClassDestroy;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 14), 'Finalizes the class'),
      'class destructor ClassDestroy should have XML-Doc');
    // 15: property Value: Integer read GetValue;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 15), 'The current value'),
      'property Value should have XML-Doc');

    // === Section 5: published ===
    LVisSec := LTypeDecl.VisibilitySections[5];
    Assert.AreEqual('published', LVisSec.VisibilityKeyword.Text);
    Assert.AreEqual(1, LVisSec.Members.Count, 'published should have 1 property');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Display name of this object'),
      'property Name should have XML-Doc');
  finally
    LTree.Free;
  end;
end;

procedure TParseTreeClassDeclTest.TestParseGenericClassDeclaration;
const
  LSourceCode = '''
    unit Unit1;
    interface
    type
      /// <summary>Generic container class</summary>
      TMyClass<T> = class
      private
        /// <summary>Stored value of generic type</summary>
        FValue: T;
        /// <summary>Internal list of items</summary>
        FItems: TList<T>;
      public
        /// <summary>Creates with initial value</summary>
        constructor Create(const AValue: T);
        /// <summary>Sets the stored value</summary>
        procedure SetValue(const AValue: T);
        /// <summary>Processes two items</summary>
        procedure Process(const AFirst, ASecond: T);
        /// <summary>Returns the stored value</summary>
        function GetValue: T;
        /// <summary>Transforms the value</summary>
        function Transform(const AInput: T): T;
        /// <summary>Tries to extract a value</summary>
        function TryExtract(out AResult: T): Boolean;
        /// <summary>The stored value property</summary>
        property Value: T read GetValue write SetValue;
        /// <summary>Read-only access to the items list</summary>
        property Items: TList<T> read FItems;
      end;
  ''';

var
  LTree: TCompilationUnitSyntax;
  LTypeSec: TTypeSectionSyntax;
  LTypeDecl: TTypeDeclarationSyntax;
  LVisSec: TVisibilitySectionSyntax;
begin
  LTree := FParser.Parse(LSourceCode);
  try
    Assert.IsNotNull(LTree.InterfaceSection, 'Interface missing');
    Assert.AreEqual(1, LTree.InterfaceSection.Declarations.Count);

    LTypeSec := TTypeSectionSyntax(LTree.InterfaceSection.Declarations[0]);
    Assert.AreEqual(1, LTypeSec.Declarations.Count, 'Should parse one type declaration');

    LTypeDecl := LTypeSec.Declarations[0];
    // Identifier should be TMyClass (the generic params <T> come after)
    Assert.AreEqual('TMyClass', LTypeDecl.Identifier.Text);
    Assert.AreEqual('class', LTypeDecl.TypeTypeToken.Text);
    Assert.IsNotNull(LTypeDecl.EndKeyword, 'Should have end keyword');

    // XML-Doc on class
    Assert.IsTrue(HasTriviaContaining(LTypeDecl.Identifier, 'Generic container class'),
      'TMyClass<T> should have XML-Doc trivia');

    // Should have 2 visibility sections: private + public
    Assert.AreEqual(2, LTypeDecl.VisibilitySections.Count, 'Should have 2 visibility sections');

    // === Section 0: private ===
    LVisSec := LTypeDecl.VisibilitySections[0];
    Assert.AreEqual('private', LVisSec.VisibilityKeyword.Text);
    Assert.AreEqual(2, LVisSec.Members.Count, 'private should have 2 field members');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Stored value of generic type'),
      'FValue should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Internal list of items'),
      'FItems should have XML-Doc');

    // === Section 1: public ===
    LVisSec := LTypeDecl.VisibilitySections[1];
    Assert.AreEqual('public', LVisSec.VisibilityKeyword.Text);
    Assert.AreEqual(8, LVisSec.Members.Count, 'public should have 8 members');
    // 0: constructor Create(const AValue: T);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Creates with initial value'),
      'Create should have XML-Doc');
    // 1: procedure SetValue(const AValue: T);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Sets the stored value'),
      'SetValue should have XML-Doc');
    // 2: procedure Process(const AFirst, ASecond: T);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 2), 'Processes two items'),
      'Process should have XML-Doc');
    // 3: function GetValue: T;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 3), 'Returns the stored value'),
      'GetValue should have XML-Doc');
    // 4: function Transform(const AInput: T): T;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 4), 'Transforms the value'),
      'Transform should have XML-Doc');
    // 5: function TryExtract(out AResult: T): Boolean;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 5), 'Tries to extract a value'),
      'TryExtract should have XML-Doc');
    // 6: property Value: T read GetValue write SetValue;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 6), 'The stored value property'),
      'property Value should have XML-Doc');
    // 7: property Items: TList<T> read FItems;
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 7), 'Read-only access to the items list'),
      'property Items should have XML-Doc');
  finally
    LTree.Free;
  end;
end;

procedure TParseTreeClassDeclTest.TestParseNestedClassDeclaration;
const
  LSourceCode = '''
    unit Unit1;
    interface
    type
      /// <summary>Outer class with nested types</summary>
      TOuterClass = class
      private
        /// <summary>Private helper class</summary>
        type
          TPrivateHelper = class
          private
            FHelperValue: Integer;
          public
            /// <summary>Executes the helper action</summary>
            procedure Execute;
          end;
        /// <summary>Private field</summary>
        FData: string;
      protected
        /// <summary>Protected item class</summary>
        type
          TProtectedItem = class
          private
            FItemName: string;
          public
            /// <summary>Returns the item name</summary>
            function GetName: string;
          end;
        /// <summary>Protected internal method</summary>
        procedure InternalProcess;
      public
        /// <summary>Performs the main work</summary>
        procedure DoWork;
      end;
  ''';

var
  LTree: TCompilationUnitSyntax;
  LTypeSec: TTypeSectionSyntax;
  LTypeDecl: TTypeDeclarationSyntax;
  LVisSec: TVisibilitySectionSyntax;
begin
  LTree := FParser.Parse(LSourceCode);
  try
    Assert.IsNotNull(LTree.InterfaceSection, 'Interface missing');
    LTypeSec := TTypeSectionSyntax(LTree.InterfaceSection.Declarations[0]);
    LTypeDecl := LTypeSec.Declarations[0];
    Assert.AreEqual('TOuterClass', LTypeDecl.Identifier.Text);
    Assert.IsNotNull(LTypeDecl.EndKeyword, 'Should have end keyword');
    Assert.IsTrue(HasTriviaContaining(LTypeDecl.Identifier, 'Outer class with nested types'),
      'TOuterClass should have XML-Doc');

    // Should have 3 visibility sections: private, protected, public
    Assert.AreEqual(3, LTypeDecl.VisibilitySections.Count, 'Should have 3 visibility sections');

    // === Section 0: private ===
    LVisSec := LTypeDecl.VisibilitySections[0];
    Assert.AreEqual('private', LVisSec.VisibilityKeyword.Text);
    // Member 0: type TPrivateHelper = class ... end;
    // Member 1: FData: string;
    Assert.AreEqual(2, LVisSec.Members.Count, 'private should have 2 members (nested type + field)');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Private helper class'),
      'type TPrivateHelper should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Private field'),
      'FData should have XML-Doc');

    // === Section 1: protected ===
    LVisSec := LTypeDecl.VisibilitySections[1];
    Assert.AreEqual('protected', LVisSec.VisibilityKeyword.Text);
    // Member 0: type TProtectedItem = class ... end;
    // Member 1: procedure InternalProcess;
    Assert.AreEqual(2, LVisSec.Members.Count, 'protected should have 2 members (nested type + method)');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Protected item class'),
      'type TProtectedItem should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Protected internal method'),
      'InternalProcess should have XML-Doc');

    // === Section 2: public ===
    LVisSec := LTypeDecl.VisibilitySections[2];
    Assert.AreEqual('public', LVisSec.VisibilityKeyword.Text);
    Assert.AreEqual(1, LVisSec.Members.Count, 'public should have 1 member');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Performs the main work'),
      'DoWork should have XML-Doc');
  finally
    LTree.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeClassDeclTest);

end.
