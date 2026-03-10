unit ParseTree.Nodes;

interface

uses
  System.Generics.Collections, ParseTree.Core, ParseTree.Tokens;

type
  { A syntax node that holds exactly one token (a leaf node element for syntactic structure) }
  TSyntaxTokenNode = class(TSyntaxNode)
  private
    FToken: TSyntaxToken;
  public
    constructor Create(AToken: TSyntaxToken);
    destructor Destroy; override;
    property Token: TSyntaxToken read FToken;
  end;

  { e.g. System.SysUtils or Unit1 in '..\Unit1.pas' }
  TUnitReferenceSyntax = class(TSyntaxNode)
  private
    FNamespaces: TObjectList<TSyntaxToken>;
    FDots: TObjectList<TSyntaxToken>;
    FInKeyword: TSyntaxToken;
    FStringLiteral: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property Namespaces: TObjectList<TSyntaxToken> read FNamespaces;
    property Dots: TObjectList<TSyntaxToken> read FDots;
    property InKeyword: TSyntaxToken read FInKeyword write FInKeyword;
    property StringLiteral: TSyntaxToken read FStringLiteral write FStringLiteral;
  end;

  { uses ...; }
  TUsesClauseSyntax = class(TSyntaxNode)
  private
    FUsesKeyword: TSyntaxToken;
    FUnitReferences: TObjectList<TUnitReferenceSyntax>;
    FCommas: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property UsesKeyword: TSyntaxToken read FUsesKeyword write FUsesKeyword;
    property UnitReferences: TObjectList<TUnitReferenceSyntax> read FUnitReferences;
    property Commas: TObjectList<TSyntaxToken> read FCommas;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { Base class for Declaration Sections in Interface/Implementation }
  TDeclarationSectionSyntax = class abstract(TSyntaxNode)
  end;

  { const Name = Value; }
  TConstDeclarationSyntax = class(TSyntaxNode)
  private
    FIdentifier: TSyntaxToken;
    FColonToken: TSyntaxToken;
    FTypeIdentifier: TSyntaxToken;
    FEqualsToken: TSyntaxToken;
    FValueTokens: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property ColonToken: TSyntaxToken read FColonToken write FColonToken;
    property TypeIdentifier: TSyntaxToken read FTypeIdentifier write FTypeIdentifier;
    property EqualsToken: TSyntaxToken read FEqualsToken write FEqualsToken;
    property ValueTokens: TObjectList<TSyntaxToken> read FValueTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { var Name: Type; }
  TVarDeclarationSyntax = class(TSyntaxNode)
  private
    FIdentifier: TSyntaxToken;
    FColonToken: TSyntaxToken;
    FTypeIdentifier: TSyntaxToken;
    FTypeExtraTokens: TObjectList<TSyntaxToken>; // e.g. <String> for TArray<String>
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property ColonToken: TSyntaxToken read FColonToken write FColonToken;
    property TypeIdentifier: TSyntaxToken read FTypeIdentifier write FTypeIdentifier;
    property TypeExtraTokens: TObjectList<TSyntaxToken> read FTypeExtraTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { Base class for members inside a class body (fields, methods, properties, nested sections) }
  TClassMemberSyntax = class(TSyntaxNode)
  private
    FTokens: TObjectList<TSyntaxToken>; // All tokens that make up this member
  public
    constructor Create;
    destructor Destroy; override;
    property Tokens: TObjectList<TSyntaxToken> read FTokens;
  end;

  { A visibility section inside a class (private, public, protected, published, strict private, strict protected) }
  TVisibilitySectionSyntax = class(TSyntaxNode)
  private
    FStrictKeyword: TSyntaxToken;
    FVisibilityKeyword: TSyntaxToken;
    FMembers: TObjectList<TClassMemberSyntax>;
  public
    constructor Create;
    destructor Destroy; override;

    function IsStrict: Boolean;
    property StrictKeyword: TSyntaxToken read FStrictKeyword write FStrictKeyword;
    property VisibilityKeyword: TSyntaxToken read FVisibilityKeyword write FVisibilityKeyword;
    property Members: TObjectList<TClassMemberSyntax> read FMembers;
  end;

  { e.g. TMyClass = class ... end; }
  TTypeDeclarationSyntax = class(TSyntaxNode)
  private
    FIdentifier: TSyntaxToken;
    FGenericParameterTokens: TObjectList<TSyntaxToken>; // e.g. <T>, <TKey, TValue>
    FEqualsToken: TSyntaxToken;
    FTypeTypeToken: TSyntaxToken; // e.g. class, interface, array, etc
    FTypeExtraTokens: TObjectList<TSyntaxToken>; // e.g. (stNone, stInto) for enums
    FBaseListTokens: TObjectList<TSyntaxToken>; // Holds '(' 'TObject' ')'
    FVisibilitySections: TObjectList<TVisibilitySectionSyntax>;
    FEndKeyword: TSyntaxToken;
    FTrailingTokens: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property GenericParameterTokens: TObjectList<TSyntaxToken> read FGenericParameterTokens;
    property EqualsToken: TSyntaxToken read FEqualsToken write FEqualsToken;
    property TypeTypeToken: TSyntaxToken read FTypeTypeToken write FTypeTypeToken;
    property TypeExtraTokens: TObjectList<TSyntaxToken> read FTypeExtraTokens;
    property BaseListTokens: TObjectList<TSyntaxToken> read FBaseListTokens;
    property VisibilitySections: TObjectList<TVisibilitySectionSyntax> read FVisibilitySections;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property TrailingTokens: TObjectList<TSyntaxToken> read FTrailingTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { type ... }
  TTypeSectionSyntax = class(TDeclarationSectionSyntax)
  private
    FTypeKeyword: TSyntaxToken;
    FDeclarations: TObjectList<TTypeDeclarationSyntax>;
  public
    constructor Create;
    destructor Destroy; override;
    property TypeKeyword: TSyntaxToken read FTypeKeyword write FTypeKeyword;
    property Declarations: TObjectList<TTypeDeclarationSyntax> read FDeclarations;
  end;

  { const ... }
  TConstSectionSyntax = class(TDeclarationSectionSyntax)
  private
    FConstKeyword: TSyntaxToken;
    FDeclarations: TObjectList<TConstDeclarationSyntax>;
  public
    constructor Create;
    destructor Destroy; override;
    property ConstKeyword: TSyntaxToken read FConstKeyword write FConstKeyword;
    property Declarations: TObjectList<TConstDeclarationSyntax> read FDeclarations;
  end;

  { var ... }
  TVarSectionSyntax = class(TDeclarationSectionSyntax)
  private
    FVarKeyword: TSyntaxToken;
    FDeclarations: TObjectList<TVarDeclarationSyntax>;
  public
    constructor Create;
    destructor Destroy; override;
    property VarKeyword: TSyntaxToken read FVarKeyword write FVarKeyword;
    property Declarations: TObjectList<TVarDeclarationSyntax> read FDeclarations;
  end;

  { interface ... }
  TInterfaceSectionSyntax = class(TSyntaxNode)
  private
    FInterfaceKeyword: TSyntaxToken;
    FUsesClause: TUsesClauseSyntax;
    FDeclarations: TObjectList<TDeclarationSectionSyntax>;
  public
    constructor Create;
    destructor Destroy; override;

    property InterfaceKeyword: TSyntaxToken read FInterfaceKeyword write FInterfaceKeyword;
    property UsesClause: TUsesClauseSyntax read FUsesClause write FUsesClause;
    property Declarations: TObjectList<TDeclarationSectionSyntax> read FDeclarations;
  end;

  { Base class for all statements (while, for, repeat, assignments, etc.) }
  TStatementSyntax = class abstract(TSyntaxNode)
  end;

  { while Condition do Statement; }
  TWhileStatementSyntax = class(TStatementSyntax)
  private
    FWhileKeyword: TSyntaxToken;
    FConditionTokens: TObjectList<TSyntaxToken>;
    FDoKeyword: TSyntaxToken;
    FStatement: TStatementSyntax;
    FBodyTokens: TObjectList<TSyntaxToken>; // Fallback if not fully parsed
  public
    constructor Create;
    destructor Destroy; override;
    property WhileKeyword: TSyntaxToken read FWhileKeyword write FWhileKeyword;
    property ConditionTokens: TObjectList<TSyntaxToken> read FConditionTokens;
    property DoKeyword: TSyntaxToken read FDoKeyword write FDoKeyword;
    property Statement: TStatementSyntax read FStatement write FStatement;
    property BodyTokens: TObjectList<TSyntaxToken> read FBodyTokens;
  end;

  { repeat Statements; until Condition; }
  TRepeatStatementSyntax = class(TStatementSyntax)
  private
    FRepeatKeyword: TSyntaxToken;
    FStatements: TObjectList<TStatementSyntax>;
    FUntilKeyword: TSyntaxToken;
    FConditionTokens: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
    FBodyTokens: TObjectList<TSyntaxToken>; // Fallback
  public
    constructor Create;
    destructor Destroy; override;
    property RepeatKeyword: TSyntaxToken read FRepeatKeyword write FRepeatKeyword;
    property Statements: TObjectList<TStatementSyntax> read FStatements;
    property UntilKeyword: TSyntaxToken read FUntilKeyword write FUntilKeyword;
    property ConditionTokens: TObjectList<TSyntaxToken> read FConditionTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
    property BodyTokens: TObjectList<TSyntaxToken> read FBodyTokens;
  end;

  { for I := Start to/downto End do Statement; }
  TForStatementSyntax = class(TStatementSyntax)
  private
    FForKeyword: TSyntaxToken;
    FVariableTokens: TObjectList<TSyntaxToken>;
    FAssignmentToken: TSyntaxToken;
    FStartTokens: TObjectList<TSyntaxToken>;
    FToDowntoKeyword: TSyntaxToken;
    FEndTokens: TObjectList<TSyntaxToken>;
    FInKeyword: TSyntaxToken;
    FCollectionTokens: TObjectList<TSyntaxToken>;
    FDoKeyword: TSyntaxToken;
    FStatement: TStatementSyntax;
    FBodyTokens: TObjectList<TSyntaxToken>; // Fallback
  public
    constructor Create;
    destructor Destroy; override;
    property ForKeyword: TSyntaxToken read FForKeyword write FForKeyword;
    property VariableTokens: TObjectList<TSyntaxToken> read FVariableTokens;
    property AssignmentToken: TSyntaxToken read FAssignmentToken write FAssignmentToken;
    property StartTokens: TObjectList<TSyntaxToken> read FStartTokens;
    property ToDowntoKeyword: TSyntaxToken read FToDowntoKeyword write FToDowntoKeyword;
    property EndTokens: TObjectList<TSyntaxToken> read FEndTokens;
    property InKeyword: TSyntaxToken read FInKeyword write FInKeyword;
    property CollectionTokens: TObjectList<TSyntaxToken> read FCollectionTokens;
    property DoKeyword: TSyntaxToken read FDoKeyword write FDoKeyword;
    property Statement: TStatementSyntax read FStatement write FStatement;
    property BodyTokens: TObjectList<TSyntaxToken> read FBodyTokens;
  end;

  { if Condition then Statement1 [else Statement2]; }
  TIfStatementSyntax = class(TStatementSyntax)
  private
    FIfKeyword: TSyntaxToken;
    FConditionTokens: TObjectList<TSyntaxToken>;
    FThenKeyword: TSyntaxToken;
    FThenStatement: TStatementSyntax;
    FElseKeyword: TSyntaxToken;
    FElseStatement: TStatementSyntax;
  public
    constructor Create;
    destructor Destroy; override;
    property IfKeyword: TSyntaxToken read FIfKeyword write FIfKeyword;
    property ConditionTokens: TObjectList<TSyntaxToken> read FConditionTokens;
    property ThenKeyword: TSyntaxToken read FThenKeyword write FThenKeyword;
    property ThenStatement: TStatementSyntax read FThenStatement write FThenStatement;
    property ElseKeyword: TSyntaxToken read FElseKeyword write FElseKeyword;
    property ElseStatement: TStatementSyntax read FElseStatement write FElseStatement;
  end;
  
  { Variable := Expression; }
  TAssignmentStatementSyntax = class(TStatementSyntax)
  private
    FLeftTokens: TObjectList<TSyntaxToken>;
    FColonEqualsToken: TSyntaxToken;
    FRightTokens: TObjectList<TSyntaxToken>;
  public
    constructor Create;
    destructor Destroy; override;
    property LeftTokens: TObjectList<TSyntaxToken> read FLeftTokens;
    property ColonEqualsToken: TSyntaxToken read FColonEqualsToken write FColonEqualsToken;
    property RightTokens: TObjectList<TSyntaxToken> read FRightTokens;
  end;

  { begin Statements end [;] }
  TBeginEndStatementSyntax = class(TStatementSyntax)
  private
    FBeginKeyword: TSyntaxToken;
    FStatements: TObjectList<TStatementSyntax>;
    FEndKeyword: TSyntaxToken;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property BeginKeyword: TSyntaxToken read FBeginKeyword write FBeginKeyword;
    property Statements: TObjectList<TStatementSyntax> read FStatements;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { try Statements finally/except Statements end [;] }
  TTryStatementSyntax = class(TStatementSyntax)
  private
    FTryKeyword: TSyntaxToken;
    FStatements: TObjectList<TStatementSyntax>;
    FFinallyKeyword: TSyntaxToken;
    FExceptKeyword: TSyntaxToken;
    FFinallyExceptStatements: TObjectList<TStatementSyntax>;
    FEndKeyword: TSyntaxToken;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property TryKeyword: TSyntaxToken read FTryKeyword write FTryKeyword;
    property Statements: TObjectList<TStatementSyntax> read FStatements;
    property FinallyKeyword: TSyntaxToken read FFinallyKeyword write FFinallyKeyword;
    property ExceptKeyword: TSyntaxToken read FExceptKeyword write FExceptKeyword;
    property FinallyExceptStatements: TObjectList<TStatementSyntax> read FFinallyExceptStatements;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { raise [Expression]; }
  TRaiseStatementSyntax = class(TStatementSyntax)
  private
    FRaiseKeyword: TSyntaxToken;
    FExpressionTokens: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property RaiseKeyword: TSyntaxToken read FRaiseKeyword write FRaiseKeyword;
    property ExpressionTokens: TObjectList<TSyntaxToken> read FExpressionTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { 1, 2: Statement; or 'a'..'z': Statement; }
  TCaseItemSyntax = class(TSyntaxNode)
  private
    FValueTokens: TObjectList<TSyntaxToken>;
    FColonToken: TSyntaxToken;
    FStatement: TStatementSyntax;
  public
    constructor Create;
    destructor Destroy; override;
    property ValueTokens: TObjectList<TSyntaxToken> read FValueTokens;
    property ColonToken: TSyntaxToken read FColonToken write FColonToken;
    property Statement: TStatementSyntax read FStatement write FStatement;
  end;

  { case Expression of Items [else Statements] end [;] }
  TCaseStatementSyntax = class(TStatementSyntax)
  private
    FCaseKeyword: TSyntaxToken;
    FExpressionTokens: TObjectList<TSyntaxToken>;
    FOfKeyword: TSyntaxToken;
    FCaseItems: TObjectList<TCaseItemSyntax>;
    FElseKeyword: TSyntaxToken;
    FElseStatements: TObjectList<TStatementSyntax>;
    FEndKeyword: TSyntaxToken;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property CaseKeyword: TSyntaxToken read FCaseKeyword write FCaseKeyword;
    property ExpressionTokens: TObjectList<TSyntaxToken> read FExpressionTokens;
    property OfKeyword: TSyntaxToken read FOfKeyword write FOfKeyword;
    property CaseItems: TObjectList<TCaseItemSyntax> read FCaseItems;
    property ElseKeyword: TSyntaxToken read FElseKeyword write FElseKeyword;
    property ElseStatements: TObjectList<TStatementSyntax> read FElseStatements;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { with Expression [, Expression] do Statement }
  TWithStatementSyntax = class(TStatementSyntax)
  private
    FWithKeyword: TSyntaxToken;
    FExpressionTokens: TObjectList<TSyntaxToken>;
    FDoKeyword: TSyntaxToken;
    FStatement: TStatementSyntax;
  public
    constructor Create;
    destructor Destroy; override;
    property WithKeyword: TSyntaxToken read FWithKeyword write FWithKeyword;
    property ExpressionTokens: TObjectList<TSyntaxToken> read FExpressionTokens;
    property DoKeyword: TSyntaxToken read FDoKeyword write FDoKeyword;
    property Statement: TStatementSyntax read FStatement write FStatement;
  end;

  { inherited; or inherited MethodName(Args); }
  TInheritedStatementSyntax = class(TStatementSyntax)
  private
    FInheritedKeyword: TSyntaxToken;
    FCallTokens: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property InheritedKeyword: TSyntaxToken read FInheritedKeyword write FInheritedKeyword;
    property CallTokens: TObjectList<TSyntaxToken> read FCallTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { Exit; or Exit(Value); }
  TExitStatementSyntax = class(TStatementSyntax)
  private
    FExitKeyword: TSyntaxToken;
    FExpressionTokens: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property ExitKeyword: TSyntaxToken read FExitKeyword write FExitKeyword;
    property ExpressionTokens: TObjectList<TSyntaxToken> read FExpressionTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { Foo.Bar(Baz); etc. }
  TProcedureCallStatementSyntax = class(TStatementSyntax)
  private
    FExpressionTokens: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property ExpressionTokens: TObjectList<TSyntaxToken> read FExpressionTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { var X: Integer; or var X := 42; (inline variable declaration) }
  TInlineVarStatementSyntax = class(TStatementSyntax)
  private
    FVarKeyword: TSyntaxToken;
    FDeclarationTokens: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property VarKeyword: TSyntaxToken read FVarKeyword write FVarKeyword;
    property DeclarationTokens: TObjectList<TSyntaxToken> read FDeclarationTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { ; (empty statement) }
  TEmptyStatementSyntax = class(TStatementSyntax)
  private
    FSemicolon: TSyntaxToken;
  public
    destructor Destroy; override;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { A statement that just holds tokens for roundtrip if not specifically parsed }
  TOpaqueStatementSyntax = class(TStatementSyntax)
  private
    FTokens: TObjectList<TSyntaxToken>;
  public
    constructor Create;
    destructor Destroy; override;
    property Tokens: TObjectList<TSyntaxToken> read FTokens;
  end;

  { Represents a generic unparsed block of tokens }
  TUnparsedDeclarationSyntax = class(TDeclarationSectionSyntax)
  private
    FTokens: TObjectList<TSyntaxToken>;
  public
    constructor Create;
    destructor Destroy; override;
    property Tokens: TObjectList<TSyntaxToken> read FTokens;
  end;

  { procedure TClass.Method; var ... begin ... end; }
  TMethodImplementationSyntax = class(TDeclarationSectionSyntax)
  private
    FClassKeyword: TSyntaxToken; // optional 'class' keyword
    FMethodTypeKeyword: TSyntaxToken; // e.g. procedure
    FSignatureTokens: TObjectList<TSyntaxToken>; // TDptMcpDebuggerTask.Execute
    FSignatureSemicolon: TSyntaxToken;
    FLocalDeclarations: TObjectList<TDeclarationSectionSyntax>; // var, const, etc
    FBeginKeyword: TSyntaxToken;
    FStatements: TObjectList<TStatementSyntax>; // Parsed statements
    FBodyTokens: TObjectList<TSyntaxToken>; // inner tokens for roundtrip if needed
    FEndKeyword: TSyntaxToken;
    FFinalSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property ClassKeyword: TSyntaxToken read FClassKeyword write FClassKeyword;
    property MethodTypeKeyword: TSyntaxToken read FMethodTypeKeyword write FMethodTypeKeyword;
    property SignatureTokens: TObjectList<TSyntaxToken> read FSignatureTokens;
    property SignatureSemicolon: TSyntaxToken read FSignatureSemicolon write FSignatureSemicolon;
    property LocalDeclarations: TObjectList<TDeclarationSectionSyntax> read FLocalDeclarations;
    property BeginKeyword: TSyntaxToken read FBeginKeyword write FBeginKeyword;
    property Statements: TObjectList<TStatementSyntax> read FStatements;
    property BodyTokens: TObjectList<TSyntaxToken> read FBodyTokens;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property FinalSemicolon: TSyntaxToken read FFinalSemicolon write FFinalSemicolon;
  end;

  { implementation ... }
  TImplementationSectionSyntax = class(TSyntaxNode)
  private
    FImplementationKeyword: TSyntaxToken;
    FUsesClause: TUsesClauseSyntax;
    FDeclarations: TObjectList<TDeclarationSectionSyntax>;
  public
    constructor Create;
    destructor Destroy; override;

    property ImplementationKeyword: TSyntaxToken read FImplementationKeyword write FImplementationKeyword;
    property UsesClause: TUsesClauseSyntax read FUsesClause write FUsesClause;
    property Declarations: TObjectList<TDeclarationSectionSyntax> read FDeclarations;
  end;

  { unit Unit1.Foo.Bar; }
  TCompilationUnitSyntax = class(TSyntaxNode)
  private
    FUnitKeyword: TSyntaxToken;
    FNamespaces: TObjectList<TSyntaxToken>;
    FDots: TObjectList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
    FInterfaceSection: TInterfaceSectionSyntax;
    FPreInterfaceDeclarations: TObjectList<TDeclarationSectionSyntax>;
    FImplementationSection: TImplementationSectionSyntax;
    FIntfImplDeclarations: TObjectList<TDeclarationSectionSyntax>;
    FPostImplementationDeclarations: TObjectList<TDeclarationSectionSyntax>;
    FFinalEndKeyword: TSyntaxToken;
    FFinalDotToken: TSyntaxToken;
    FEndOfFileToken: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property UnitKeyword: TSyntaxToken read FUnitKeyword write FUnitKeyword;
    property Namespaces: TObjectList<TSyntaxToken> read FNamespaces;
    property Dots: TObjectList<TSyntaxToken> read FDots;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
    property InterfaceSection: TInterfaceSectionSyntax read FInterfaceSection write FInterfaceSection;
    property PreInterfaceDeclarations: TObjectList<TDeclarationSectionSyntax> read FPreInterfaceDeclarations;
    property ImplementationSection: TImplementationSectionSyntax read FImplementationSection write FImplementationSection;
    property IntfImplDeclarations: TObjectList<TDeclarationSectionSyntax> read FIntfImplDeclarations;
    property PostImplementationDeclarations: TObjectList<TDeclarationSectionSyntax> read FPostImplementationDeclarations;
    property FinalEndKeyword: TSyntaxToken read FFinalEndKeyword write FFinalEndKeyword;
    property FinalDotToken: TSyntaxToken read FFinalDotToken write FFinalDotToken;
    property EndOfFileToken: TSyntaxToken read FEndOfFileToken write FEndOfFileToken;
  end;

implementation

{ TSyntaxTokenNode }

constructor TSyntaxTokenNode.Create(AToken: TSyntaxToken);
begin
  inherited Create;
  FToken := AToken;
end;

destructor TSyntaxTokenNode.Destroy;
begin
  FToken.Free;
  inherited;
end;

{ TUnitReferenceSyntax }

constructor TUnitReferenceSyntax.Create;
begin
  inherited Create;
  FNamespaces := TObjectList<TSyntaxToken>.Create(True);
  FDots := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TUnitReferenceSyntax.Destroy;
begin
  FNamespaces.Free;
  FDots.Free;
  FInKeyword.Free;
  FStringLiteral.Free;
  inherited;
end;

{ TUsesClauseSyntax }

constructor TUsesClauseSyntax.Create;
begin
  inherited Create;
  FUnitReferences := TObjectList<TUnitReferenceSyntax>.Create;
  FCommas := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TUsesClauseSyntax.Destroy;
begin
  FUsesKeyword.Free;
  FUnitReferences.Free;
  FCommas.Free;
  FSemicolon.Free;
  inherited;
end;

{ TClassMemberSyntax }

constructor TClassMemberSyntax.Create;
begin
  inherited Create;
  FTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TClassMemberSyntax.Destroy;
begin
  FTokens.Free;
  inherited;
end;

{ TVisibilitySectionSyntax }

constructor TVisibilitySectionSyntax.Create;
begin
  inherited Create;
  FMembers := TObjectList<TClassMemberSyntax>.Create;
end;

destructor TVisibilitySectionSyntax.Destroy;
begin
  FStrictKeyword.Free;
  FVisibilityKeyword.Free;
  FMembers.Free;
  inherited;
end;

function TVisibilitySectionSyntax.IsStrict: Boolean;
begin
  Result := Assigned(FStrictKeyword);
end;

{ TTypeDeclarationSyntax }

constructor TTypeDeclarationSyntax.Create;
begin
  inherited Create;
  FGenericParameterTokens := TObjectList<TSyntaxToken>.Create(True);
  FTypeExtraTokens := TObjectList<TSyntaxToken>.Create(True);
  FBaseListTokens := TObjectList<TSyntaxToken>.Create(True);
  FVisibilitySections := TObjectList<TVisibilitySectionSyntax>.Create;
  FTrailingTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TTypeDeclarationSyntax.Destroy;
begin
  FIdentifier.Free;
  FGenericParameterTokens.Free;
  FEqualsToken.Free;
  FTypeTypeToken.Free;
  FTypeExtraTokens.Free;
  FBaseListTokens.Free;
  FVisibilitySections.Free;
  FEndKeyword.Free;
  FTrailingTokens.Free;
  FSemicolon.Free;
  inherited;
end;

{ TTypeSectionSyntax }

constructor TTypeSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := TObjectList<TTypeDeclarationSyntax>.Create;
end;

destructor TTypeSectionSyntax.Destroy;
begin
  FTypeKeyword.Free;
  FDeclarations.Free;
  inherited;
end;

{ TConstDeclarationSyntax }

constructor TConstDeclarationSyntax.Create;
begin
  inherited Create;
  FValueTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TConstDeclarationSyntax.Destroy;
begin
  FIdentifier.Free;
  FColonToken.Free;
  FTypeIdentifier.Free;
  FEqualsToken.Free;
  FValueTokens.Free;
  FSemicolon.Free;
  inherited;
end;

{ TVarDeclarationSyntax }

constructor TVarDeclarationSyntax.Create;
begin
  inherited Create;
  FTypeExtraTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TVarDeclarationSyntax.Destroy;
begin
  FIdentifier.Free;
  FColonToken.Free;
  FTypeIdentifier.Free;
  FTypeExtraTokens.Free;
  FSemicolon.Free;
  inherited;
end;

{ TConstSectionSyntax }

constructor TConstSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := TObjectList<TConstDeclarationSyntax>.Create;
end;

destructor TConstSectionSyntax.Destroy;
begin
  FConstKeyword.Free;
  FDeclarations.Free;
  inherited;
end;

{ TVarSectionSyntax }

constructor TVarSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := TObjectList<TVarDeclarationSyntax>.Create;
end;

destructor TVarSectionSyntax.Destroy;
begin
  FVarKeyword.Free;
  FDeclarations.Free;
  inherited;
end;

{ TWhileStatementSyntax }

constructor TWhileStatementSyntax.Create;
begin
  inherited Create;
  FConditionTokens := TObjectList<TSyntaxToken>.Create(True);
  FBodyTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TWhileStatementSyntax.Destroy;
begin
  FWhileKeyword.Free;
  FConditionTokens.Free;
  FDoKeyword.Free;
  FStatement.Free;
  FBodyTokens.Free;
  inherited;
end;

{ TRepeatStatementSyntax }

constructor TRepeatStatementSyntax.Create;
begin
  inherited Create;
  FStatements := TObjectList<TStatementSyntax>.Create;
  FConditionTokens := TObjectList<TSyntaxToken>.Create(True);
  FBodyTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TRepeatStatementSyntax.Destroy;
begin
  FRepeatKeyword.Free;
  FStatements.Free;
  FUntilKeyword.Free;
  FConditionTokens.Free;
  FSemicolon.Free;
  FBodyTokens.Free;
  inherited;
end;

{ TForStatementSyntax }

constructor TForStatementSyntax.Create;
begin
  inherited Create;
  FVariableTokens := TObjectList<TSyntaxToken>.Create(True);
  FStartTokens := TObjectList<TSyntaxToken>.Create(True);
  FEndTokens := TObjectList<TSyntaxToken>.Create(True);
  FCollectionTokens := TObjectList<TSyntaxToken>.Create(True);
  FBodyTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TForStatementSyntax.Destroy;
begin
  FForKeyword.Free;
  FVariableTokens.Free;
  FAssignmentToken.Free;
  FStartTokens.Free;
  FToDowntoKeyword.Free;
  FEndTokens.Free;
  FInKeyword.Free;
  FCollectionTokens.Free;
  FDoKeyword.Free;
  FStatement.Free;
  FBodyTokens.Free;
  inherited;
end;

{ TIfStatementSyntax }

constructor TIfStatementSyntax.Create;
begin
  inherited Create;
  FConditionTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TIfStatementSyntax.Destroy;
begin
  FIfKeyword.Free;
  FConditionTokens.Free;
  FThenKeyword.Free;
  FThenStatement.Free;
  FElseKeyword.Free;
  FElseStatement.Free;
  inherited;
end;

{ TAssignmentStatementSyntax }

constructor TAssignmentStatementSyntax.Create;
begin
  inherited Create;
  FLeftTokens := TObjectList<TSyntaxToken>.Create(True);
  FRightTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TAssignmentStatementSyntax.Destroy;
begin
  FLeftTokens.Free;
  FColonEqualsToken.Free;
  FRightTokens.Free;
  inherited;
end;

{ TBeginEndStatementSyntax }

constructor TBeginEndStatementSyntax.Create;
begin
  inherited Create;
  FStatements := TObjectList<TStatementSyntax>.Create;
end;

destructor TBeginEndStatementSyntax.Destroy;
begin
  FBeginKeyword.Free;
  FStatements.Free;
  FEndKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TTryStatementSyntax }

constructor TTryStatementSyntax.Create;
begin
  inherited Create;
  FStatements := TObjectList<TStatementSyntax>.Create;
  FFinallyExceptStatements := TObjectList<TStatementSyntax>.Create;
end;

destructor TTryStatementSyntax.Destroy;
begin
  FTryKeyword.Free;
  FStatements.Free;
  FFinallyKeyword.Free;
  FExceptKeyword.Free;
  FFinallyExceptStatements.Free;
  FEndKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TRaiseStatementSyntax }

constructor TRaiseStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TRaiseStatementSyntax.Destroy;
begin
  FRaiseKeyword.Free;
  FExpressionTokens.Free;
  FSemicolon.Free;
  inherited;
end;

{ TCaseItemSyntax }

constructor TCaseItemSyntax.Create;
begin
  inherited Create;
  FValueTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TCaseItemSyntax.Destroy;
begin
  FValueTokens.Free;
  FColonToken.Free;
  FStatement.Free;
  inherited;
end;

{ TCaseStatementSyntax }

constructor TCaseStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := TObjectList<TSyntaxToken>.Create(True);
  FCaseItems := TObjectList<TCaseItemSyntax>.Create;
  FElseStatements := TObjectList<TStatementSyntax>.Create;
end;

destructor TCaseStatementSyntax.Destroy;
begin
  FCaseKeyword.Free;
  FExpressionTokens.Free;
  FOfKeyword.Free;
  FCaseItems.Free;
  FElseKeyword.Free;
  FElseStatements.Free;
  FEndKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TWithStatementSyntax }

constructor TWithStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TWithStatementSyntax.Destroy;
begin
  FWithKeyword.Free;
  FExpressionTokens.Free;
  FDoKeyword.Free;
  FStatement.Free;
  inherited;
end;

{ TProcedureCallStatementSyntax }

{ TInheritedStatementSyntax }

constructor TInheritedStatementSyntax.Create;
begin
  inherited Create;
  FCallTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TInheritedStatementSyntax.Destroy;
begin
  FInheritedKeyword.Free;
  FCallTokens.Free;
  FSemicolon.Free;
  inherited;
end;

{ TExitStatementSyntax }

constructor TExitStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TExitStatementSyntax.Destroy;
begin
  FExitKeyword.Free;
  FExpressionTokens.Free;
  FSemicolon.Free;
  inherited;
end;

{ TProcedureCallStatementSyntax }

constructor TProcedureCallStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TProcedureCallStatementSyntax.Destroy;
begin
  FExpressionTokens.Free;
  FSemicolon.Free;
  inherited;
end;

{ TOpaqueStatementSyntax }

{ TInlineVarStatementSyntax }

constructor TInlineVarStatementSyntax.Create;
begin
  inherited Create;
  FDeclarationTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TInlineVarStatementSyntax.Destroy;
begin
  FVarKeyword.Free;
  FDeclarationTokens.Free;
  FSemicolon.Free;
  inherited;
end;

{ TEmptyStatementSyntax }

destructor TEmptyStatementSyntax.Destroy;
begin
  FSemicolon.Free;
  inherited;
end;

{ TOpaqueStatementSyntax }

constructor TOpaqueStatementSyntax.Create;
begin
  inherited Create;
  FTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TOpaqueStatementSyntax.Destroy;
begin
  FTokens.Free;
  inherited;
end;

{ TInterfaceSectionSyntax }

constructor TInterfaceSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := TObjectList<TDeclarationSectionSyntax>.Create;
end;

destructor TInterfaceSectionSyntax.Destroy;
begin
  FInterfaceKeyword.Free;
  FUsesClause.Free;
  FDeclarations.Free;
  inherited;
end;

{ TUnparsedDeclarationSyntax }

constructor TUnparsedDeclarationSyntax.Create;
begin
  inherited Create;
  FTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TUnparsedDeclarationSyntax.Destroy;
begin
  FTokens.Free;
  inherited;
end;

{ TMethodImplementationSyntax }

constructor TMethodImplementationSyntax.Create;
begin
  inherited Create;
  FSignatureTokens := TObjectList<TSyntaxToken>.Create(True);
  FLocalDeclarations := TObjectList<TDeclarationSectionSyntax>.Create;
  FStatements := TObjectList<TStatementSyntax>.Create;
  FBodyTokens := TObjectList<TSyntaxToken>.Create(True);
end;

destructor TMethodImplementationSyntax.Destroy;
begin
  FMethodTypeKeyword.Free;
  FSignatureTokens.Free;
  FSignatureSemicolon.Free;
  FLocalDeclarations.Free;
  FBeginKeyword.Free;
  FStatements.Free;
  FBodyTokens.Free;
  FEndKeyword.Free;
  FFinalSemicolon.Free;
  inherited;
end;

{ TImplementationSectionSyntax }

constructor TImplementationSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := TObjectList<TDeclarationSectionSyntax>.Create;
end;

destructor TImplementationSectionSyntax.Destroy;
begin
  FImplementationKeyword.Free;
  FUsesClause.Free;
  FDeclarations.Free;
  inherited;
end;

{ TCompilationUnitSyntax }

constructor TCompilationUnitSyntax.Create;
begin
  inherited Create;
  FNamespaces := TObjectList<TSyntaxToken>.Create(True);
  FDots := TObjectList<TSyntaxToken>.Create(True);
  FPreInterfaceDeclarations := TObjectList<TDeclarationSectionSyntax>.Create;
  FIntfImplDeclarations := TObjectList<TDeclarationSectionSyntax>.Create;
  FPostImplementationDeclarations := TObjectList<TDeclarationSectionSyntax>.Create;
end;

destructor TCompilationUnitSyntax.Destroy;
begin
  FUnitKeyword.Free;
  FNamespaces.Free;
  FDots.Free;
  FSemicolon.Free;
  FInterfaceSection.Free;
  FPreInterfaceDeclarations.Free;
  FImplementationSection.Free;
  FIntfImplDeclarations.Free;
  FPostImplementationDeclarations.Free;
  FFinalEndKeyword.Free;
  FFinalDotToken.Free;
  FEndOfFileToken.Free;
  inherited;
end;

end.
