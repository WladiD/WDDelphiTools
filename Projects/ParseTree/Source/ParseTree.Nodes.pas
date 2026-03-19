// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit ParseTree.Nodes;

interface

uses

  mormot.core.collections,

  ParseTree.Core;

type

  /// <summary>A syntax node that holds exactly one token (a leaf node element for syntactic structure)</summary>
  TSyntaxTokenNode = class(TSyntaxNode)
  private
    FToken: TSyntaxToken;
  public
    constructor Create(AToken: TSyntaxToken);
    destructor Destroy; override;
    property Token: TSyntaxToken read FToken;
  end;

  /// <summary>e.g. System.SysUtils or Unit1 in '..\Unit1.pas'</summary>
  TUnitReferenceSyntax = class(TSyntaxNode)
  private
    FDots         : IList<TSyntaxToken>;
    FInKeyword    : TSyntaxToken;
    FNamespaces   : IList<TSyntaxToken>;
    FStringLiteral: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property Dots: IList<TSyntaxToken> read FDots;
    property InKeyword: TSyntaxToken read FInKeyword write FInKeyword;
    property Namespaces: IList<TSyntaxToken> read FNamespaces;
    property StringLiteral: TSyntaxToken read FStringLiteral write FStringLiteral;
  end;

  /// <summary>uses ...;</summary>
  TUsesClauseSyntax = class(TSyntaxNode)
  private
    FUsesKeyword: TSyntaxToken;
    FUnitReferences: IList<TUnitReferenceSyntax>;
    FCommas: IList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property UsesKeyword: TSyntaxToken read FUsesKeyword write FUsesKeyword;
    property UnitReferences: IList<TUnitReferenceSyntax> read FUnitReferences;
    property Commas: IList<TSyntaxToken> read FCommas;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  /// <summary>Base class for Declaration Sections in Interface/Implementation</summary>
  TDeclarationSectionSyntax = class abstract(TSyntaxNode);

  /// <summary>const Name = Value;</summary>
  TConstDeclarationSyntax = class(TSyntaxNode)
  private
    FColonToken    : TSyntaxToken;
    FEqualsToken   : TSyntaxToken;
    FIdentifier    : TSyntaxToken;
    FSemicolon     : TSyntaxToken;
    FTypeIdentifier: TSyntaxToken;
    FValueTokens   : IList<TSyntaxToken>;
  public
    constructor Create;
    destructor Destroy; override;
    
    property ColonToken: TSyntaxToken read FColonToken write FColonToken;
    property EqualsToken: TSyntaxToken read FEqualsToken write FEqualsToken;
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
    property TypeIdentifier: TSyntaxToken read FTypeIdentifier write FTypeIdentifier;
    property ValueTokens: IList<TSyntaxToken> read FValueTokens;
  end;

  /// <summary>var Name: Type;</summary>
  TVarDeclarationSyntax = class(TSyntaxNode)
  private
    FColonToken     : TSyntaxToken;
    FIdentifier     : TSyntaxToken;
    FSemicolon      : TSyntaxToken;
    FTypeExtraTokens: IList<TSyntaxToken>; // e.g. <String> for TArray<String>
    FTypeIdentifier : TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    
    property ColonToken: TSyntaxToken read FColonToken write FColonToken;
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
    property TypeExtraTokens: IList<TSyntaxToken> read FTypeExtraTokens;
    property TypeIdentifier: TSyntaxToken read FTypeIdentifier write FTypeIdentifier;
  end;

  /// <summary>
  ///  Base class for members inside a class body (fields, methods, properties, nested sections)
  /// </summary>
  TClassMemberSyntax = class(TSyntaxNode)
  private
    FTokens: IList<TSyntaxToken>; // All tokens that make up this member
  public
    constructor Create;
    property Tokens: IList<TSyntaxToken> read FTokens;
  end;

  /// <summary>
  ///  A visibility section inside a class
  ///  (private, public, protected, published, strict private, strict protected)
  /// </summary>
  TVisibilitySectionSyntax = class(TSyntaxNode)
  private
    FMembers          : IList<TClassMemberSyntax>;
    FStrictKeyword    : TSyntaxToken;
    FVisibilityKeyword: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    function IsStrict: Boolean;
    property Members: IList<TClassMemberSyntax> read FMembers;
    property StrictKeyword: TSyntaxToken read FStrictKeyword write FStrictKeyword;
    property VisibilityKeyword: TSyntaxToken read FVisibilityKeyword write FVisibilityKeyword;
  end;

  /// <summary>e.g. TMyClass = class ... end;</summary>
  TTypeDeclarationSyntax = class(TSyntaxNode)
  private
    FBaseListTokens        : IList<TSyntaxToken>; // Holds '(' 'TObject' ')'
    FEndKeyword            : TSyntaxToken;
    FEqualsToken           : TSyntaxToken;
    FGenericParameterTokens: IList<TSyntaxToken>; // e.g. <T>, <TKey, TValue>
    FIdentifier            : TSyntaxToken;
    FSemicolon             : TSyntaxToken;
    FTrailingTokens        : IList<TSyntaxToken>;
    FTypeExtraTokens       : IList<TSyntaxToken>; // e.g. (stNone, stInto) for enums
    FTypeTypeToken         : TSyntaxToken; // e.g. class, interface, array, etc
    FVisibilitySections    : IList<TVisibilitySectionSyntax>;
  public
    constructor Create;
    destructor Destroy; override;
    
    property BaseListTokens: IList<TSyntaxToken> read FBaseListTokens;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property EqualsToken: TSyntaxToken read FEqualsToken write FEqualsToken;
    property GenericParameterTokens: IList<TSyntaxToken> read FGenericParameterTokens;
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
    property TrailingTokens: IList<TSyntaxToken> read FTrailingTokens;
    property TypeExtraTokens: IList<TSyntaxToken> read FTypeExtraTokens;
    property TypeTypeToken: TSyntaxToken read FTypeTypeToken write FTypeTypeToken;
    property VisibilitySections: IList<TVisibilitySectionSyntax> read FVisibilitySections;
  end;

  /// <summary>e.g. TMyClass = class ... end;</summary>
  TClassDeclarationSyntax = class(TTypeDeclarationSyntax);

  /// <summary>e.g. TMyRecord = record ... end;</summary>
  TRecordDeclarationSyntax = class(TTypeDeclarationSyntax);

  /// <summary>type ...</summary>
  TTypeSectionSyntax = class(TDeclarationSectionSyntax)
  private
    FDeclarations: IList<TTypeDeclarationSyntax>;
    FTypeKeyword : TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property Declarations: IList<TTypeDeclarationSyntax> read FDeclarations;
    property TypeKeyword: TSyntaxToken read FTypeKeyword write FTypeKeyword;
  end;

  /// <summary>const ...</summary>
  TConstSectionSyntax = class(TDeclarationSectionSyntax)
  private
    FConstKeyword: TSyntaxToken;
    FDeclarations: IList<TConstDeclarationSyntax>;
  public
    constructor Create;
    destructor Destroy; override;
    property ConstKeyword: TSyntaxToken read FConstKeyword write FConstKeyword;
    property Declarations: IList<TConstDeclarationSyntax> read FDeclarations;
  end;

  /// <summary>var ...</summary>
  TVarSectionSyntax = class(TDeclarationSectionSyntax)
  private
    FDeclarations: IList<TVarDeclarationSyntax>;
    FVarKeyword  : TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property Declarations: IList<TVarDeclarationSyntax> read FDeclarations;
    property VarKeyword: TSyntaxToken read FVarKeyword write FVarKeyword;
  end;

  /// <summary>interface ...</summary>
  TInterfaceSectionSyntax = class(TSyntaxNode)
  private
    FDeclarations    : IList<TDeclarationSectionSyntax>;
    FInterfaceKeyword: TSyntaxToken;
    FUsesClause      : TUsesClauseSyntax;
  public
    constructor Create;
    destructor Destroy; override;

    property InterfaceKeyword: TSyntaxToken read FInterfaceKeyword write FInterfaceKeyword;
    property UsesClause: TUsesClauseSyntax read FUsesClause write FUsesClause;
    property Declarations: IList<TDeclarationSectionSyntax> read FDeclarations;
  end;

  { Base class for all statements (while, for, repeat, assignments, etc.) }
  TStatementSyntax = class abstract(TSyntaxNode)
  end;

  { while Condition do Statement; }
  TWhileStatementSyntax = class(TStatementSyntax)
  private
    FWhileKeyword: TSyntaxToken;
    FConditionTokens: IList<TSyntaxToken>;
    FDoKeyword: TSyntaxToken;
    FStatement: TStatementSyntax;
    FBodyTokens: IList<TSyntaxToken>; // Fallback if not fully parsed
  public
    constructor Create;
    destructor Destroy; override;
    property WhileKeyword: TSyntaxToken read FWhileKeyword write FWhileKeyword;
    property ConditionTokens: IList<TSyntaxToken> read FConditionTokens;
    property DoKeyword: TSyntaxToken read FDoKeyword write FDoKeyword;
    property Statement: TStatementSyntax read FStatement write FStatement;
    property BodyTokens: IList<TSyntaxToken> read FBodyTokens;
  end;

  { repeat Statements; until Condition; }
  TRepeatStatementSyntax = class(TStatementSyntax)
  private
    FRepeatKeyword: TSyntaxToken;
    FStatements: IList<TStatementSyntax>;
    FUntilKeyword: TSyntaxToken;
    FConditionTokens: IList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
    FBodyTokens: IList<TSyntaxToken>; // Fallback
  public
    constructor Create;
    destructor Destroy; override;
    property RepeatKeyword: TSyntaxToken read FRepeatKeyword write FRepeatKeyword;
    property Statements: IList<TStatementSyntax> read FStatements;
    property UntilKeyword: TSyntaxToken read FUntilKeyword write FUntilKeyword;
    property ConditionTokens: IList<TSyntaxToken> read FConditionTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
    property BodyTokens: IList<TSyntaxToken> read FBodyTokens;
  end;

  { for I := Start to/downto End do Statement; }
  TForStatementSyntax = class(TStatementSyntax)
  private
    FForKeyword: TSyntaxToken;
    FVariableTokens: IList<TSyntaxToken>;
    FAssignmentToken: TSyntaxToken;
    FStartTokens: IList<TSyntaxToken>;
    FToDowntoKeyword: TSyntaxToken;
    FEndTokens: IList<TSyntaxToken>;
    FInKeyword: TSyntaxToken;
    FCollectionTokens: IList<TSyntaxToken>;
    FDoKeyword: TSyntaxToken;
    FStatement: TStatementSyntax;
    FBodyTokens: IList<TSyntaxToken>; // Fallback
  public
    constructor Create;
    destructor Destroy; override;
    property ForKeyword: TSyntaxToken read FForKeyword write FForKeyword;
    property VariableTokens: IList<TSyntaxToken> read FVariableTokens;
    property AssignmentToken: TSyntaxToken read FAssignmentToken write FAssignmentToken;
    property StartTokens: IList<TSyntaxToken> read FStartTokens;
    property ToDowntoKeyword: TSyntaxToken read FToDowntoKeyword write FToDowntoKeyword;
    property EndTokens: IList<TSyntaxToken> read FEndTokens;
    property InKeyword: TSyntaxToken read FInKeyword write FInKeyword;
    property CollectionTokens: IList<TSyntaxToken> read FCollectionTokens;
    property DoKeyword: TSyntaxToken read FDoKeyword write FDoKeyword;
    property Statement: TStatementSyntax read FStatement write FStatement;
    property BodyTokens: IList<TSyntaxToken> read FBodyTokens;
  end;

  { if Condition then Statement1 [else Statement2]; }
  TIfStatementSyntax = class(TStatementSyntax)
  private
    FIfKeyword: TSyntaxToken;
    FConditionTokens: IList<TSyntaxToken>;
    FThenKeyword: TSyntaxToken;
    FThenStatement: TStatementSyntax;
    FElseKeyword: TSyntaxToken;
    FElseStatement: TStatementSyntax;
  public
    constructor Create;
    destructor Destroy; override;
    property IfKeyword: TSyntaxToken read FIfKeyword write FIfKeyword;
    property ConditionTokens: IList<TSyntaxToken> read FConditionTokens;
    property ThenKeyword: TSyntaxToken read FThenKeyword write FThenKeyword;
    property ThenStatement: TStatementSyntax read FThenStatement write FThenStatement;
    property ElseKeyword: TSyntaxToken read FElseKeyword write FElseKeyword;
    property ElseStatement: TStatementSyntax read FElseStatement write FElseStatement;
  end;
  
  { Variable := Expression; }
  TAssignmentStatementSyntax = class(TStatementSyntax)
  private
    FLeftTokens: IList<TSyntaxToken>;
    FColonEqualsToken: TSyntaxToken;
    FRightTokens: IList<TSyntaxToken>;
  public
    constructor Create;
    destructor Destroy; override;
    property LeftTokens: IList<TSyntaxToken> read FLeftTokens;
    property ColonEqualsToken: TSyntaxToken read FColonEqualsToken write FColonEqualsToken;
    property RightTokens: IList<TSyntaxToken> read FRightTokens;
  end;

  { begin Statements end [;] }
  TBeginEndStatementSyntax = class(TStatementSyntax)
  private
    FBeginKeyword: TSyntaxToken;
    FStatements: IList<TStatementSyntax>;
    FEndKeyword: TSyntaxToken;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property BeginKeyword: TSyntaxToken read FBeginKeyword write FBeginKeyword;
    property Statements: IList<TStatementSyntax> read FStatements;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { try Statements finally/except Statements end [;] }
  TTryStatementSyntax = class(TStatementSyntax)
  private
    FTryKeyword: TSyntaxToken;
    FStatements: IList<TStatementSyntax>;
    FFinallyKeyword: TSyntaxToken;
    FExceptKeyword: TSyntaxToken;
    FFinallyExceptStatements: IList<TStatementSyntax>;
    FEndKeyword: TSyntaxToken;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property TryKeyword: TSyntaxToken read FTryKeyword write FTryKeyword;
    property Statements: IList<TStatementSyntax> read FStatements;
    property FinallyKeyword: TSyntaxToken read FFinallyKeyword write FFinallyKeyword;
    property ExceptKeyword: TSyntaxToken read FExceptKeyword write FExceptKeyword;
    property FinallyExceptStatements: IList<TStatementSyntax> read FFinallyExceptStatements;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { raise [Expression]; }
  TRaiseStatementSyntax = class(TStatementSyntax)
  private
    FRaiseKeyword: TSyntaxToken;
    FExpressionTokens: IList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property RaiseKeyword: TSyntaxToken read FRaiseKeyword write FRaiseKeyword;
    property ExpressionTokens: IList<TSyntaxToken> read FExpressionTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { 1, 2: Statement; or 'a'..'z': Statement; }
  TCaseItemSyntax = class(TSyntaxNode)
  private
    FValueTokens: IList<TSyntaxToken>;
    FColonToken: TSyntaxToken;
    FStatement: TStatementSyntax;
  public
    constructor Create;
    destructor Destroy; override;
    property ValueTokens: IList<TSyntaxToken> read FValueTokens;
    property ColonToken: TSyntaxToken read FColonToken write FColonToken;
    property Statement: TStatementSyntax read FStatement write FStatement;
  end;

  { case Expression of Items [else Statements] end [;] }
  TCaseStatementSyntax = class(TStatementSyntax)
  private
    FCaseKeyword: TSyntaxToken;
    FExpressionTokens: IList<TSyntaxToken>;
    FOfKeyword: TSyntaxToken;
    FCaseItems: IList<TCaseItemSyntax>;
    FElseKeyword: TSyntaxToken;
    FElseStatements: IList<TStatementSyntax>;
    FEndKeyword: TSyntaxToken;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property CaseKeyword: TSyntaxToken read FCaseKeyword write FCaseKeyword;
    property ExpressionTokens: IList<TSyntaxToken> read FExpressionTokens;
    property OfKeyword: TSyntaxToken read FOfKeyword write FOfKeyword;
    property CaseItems: IList<TCaseItemSyntax> read FCaseItems;
    property ElseKeyword: TSyntaxToken read FElseKeyword write FElseKeyword;
    property ElseStatements: IList<TStatementSyntax> read FElseStatements;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { with Expression [, Expression] do Statement }
  TWithStatementSyntax = class(TStatementSyntax)
  private
    FWithKeyword: TSyntaxToken;
    FExpressionTokens: IList<TSyntaxToken>;
    FDoKeyword: TSyntaxToken;
    FStatement: TStatementSyntax;
  public
    constructor Create;
    destructor Destroy; override;
    property WithKeyword: TSyntaxToken read FWithKeyword write FWithKeyword;
    property ExpressionTokens: IList<TSyntaxToken> read FExpressionTokens;
    property DoKeyword: TSyntaxToken read FDoKeyword write FDoKeyword;
    property Statement: TStatementSyntax read FStatement write FStatement;
  end;

  { inherited; or inherited MethodName(Args); }
  TInheritedStatementSyntax = class(TStatementSyntax)
  private
    FInheritedKeyword: TSyntaxToken;
    FCallTokens: IList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property InheritedKeyword: TSyntaxToken read FInheritedKeyword write FInheritedKeyword;
    property CallTokens: IList<TSyntaxToken> read FCallTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { Exit; or Exit(Value); }
  TExitStatementSyntax = class(TStatementSyntax)
  private
    FExitKeyword: TSyntaxToken;
    FExpressionTokens: IList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property ExitKeyword: TSyntaxToken read FExitKeyword write FExitKeyword;
    property ExpressionTokens: IList<TSyntaxToken> read FExpressionTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { Foo.Bar(Baz); etc. }
  TProcedureCallStatementSyntax = class(TStatementSyntax)
  private
    FExpressionTokens: IList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property ExpressionTokens: IList<TSyntaxToken> read FExpressionTokens;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { var X: Integer; or var X := 42; (inline variable declaration) }
  TInlineVarStatementSyntax = class(TStatementSyntax)
  private
    FVarKeyword: TSyntaxToken;
    FDeclarationTokens: IList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property VarKeyword: TSyntaxToken read FVarKeyword write FVarKeyword;
    property DeclarationTokens: IList<TSyntaxToken> read FDeclarationTokens;
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
    FTokens: IList<TSyntaxToken>;
  public
    constructor Create;
    property Tokens: IList<TSyntaxToken> read FTokens;
  end;

  { Represents a generic unparsed block of tokens }
  TUnparsedDeclarationSyntax = class(TDeclarationSectionSyntax)
  private
    FTokens: IList<TSyntaxToken>;
  public
    constructor Create;
    property Tokens: IList<TSyntaxToken> read FTokens;
  end;

  { procedure TClass.Method; var ... begin ... end; }
  TMethodImplementationSyntax = class(TDeclarationSectionSyntax)
  private
    FClassKeyword: TSyntaxToken; // optional 'class' keyword
    FMethodTypeKeyword: TSyntaxToken; // e.g. procedure
    FSignatureTokens: IList<TSyntaxToken>; // TDptMcpDebuggerTask.Execute
    FSignatureSemicolon: TSyntaxToken;
    FLocalDeclarations: IList<TDeclarationSectionSyntax>; // var, const, etc
    FBeginKeyword: TSyntaxToken;
    FStatements: IList<TStatementSyntax>; // Parsed statements
    FBodyTokens: IList<TSyntaxToken>; // inner tokens for roundtrip if needed
    FEndKeyword: TSyntaxToken;
    FFinalSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property ClassKeyword: TSyntaxToken read FClassKeyword write FClassKeyword;
    property MethodTypeKeyword: TSyntaxToken read FMethodTypeKeyword write FMethodTypeKeyword;
    property SignatureTokens: IList<TSyntaxToken> read FSignatureTokens;
    property SignatureSemicolon: TSyntaxToken read FSignatureSemicolon write FSignatureSemicolon;
    property LocalDeclarations: IList<TDeclarationSectionSyntax> read FLocalDeclarations;
    property BeginKeyword: TSyntaxToken read FBeginKeyword write FBeginKeyword;
    property Statements: IList<TStatementSyntax> read FStatements;
    property BodyTokens: IList<TSyntaxToken> read FBodyTokens;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
    property FinalSemicolon: TSyntaxToken read FFinalSemicolon write FFinalSemicolon;
  end;

  { implementation ... }
  TImplementationSectionSyntax = class(TSyntaxNode)
  private
    FImplementationKeyword: TSyntaxToken;
    FUsesClause: TUsesClauseSyntax;
    FDeclarations: IList<TDeclarationSectionSyntax>;
  public
    constructor Create;
    destructor Destroy; override;

    property ImplementationKeyword: TSyntaxToken read FImplementationKeyword write FImplementationKeyword;
    property UsesClause: TUsesClauseSyntax read FUsesClause write FUsesClause;
    property Declarations: IList<TDeclarationSectionSyntax> read FDeclarations;
  end;

  { unit Unit1.Foo.Bar; }
  TCompilationUnitSyntax = class(TSyntaxNode)
  private
    FUnitKeyword: TSyntaxToken;
    FNamespaces: IList<TSyntaxToken>;
    FDots: IList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
    FInterfaceSection: TInterfaceSectionSyntax;
    FPreInterfaceDeclarations: IList<TDeclarationSectionSyntax>;
    FImplementationSection: TImplementationSectionSyntax;
    FIntfImplDeclarations: IList<TDeclarationSectionSyntax>;
    FPostImplementationDeclarations: IList<TDeclarationSectionSyntax>;
    FFinalEndKeyword: TSyntaxToken;
    FFinalDotToken: TSyntaxToken;
    FEndOfFileToken: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property UnitKeyword: TSyntaxToken read FUnitKeyword write FUnitKeyword;
    property Namespaces: IList<TSyntaxToken> read FNamespaces;
    property Dots: IList<TSyntaxToken> read FDots;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
    property InterfaceSection: TInterfaceSectionSyntax read FInterfaceSection write FInterfaceSection;
    property PreInterfaceDeclarations: IList<TDeclarationSectionSyntax> read FPreInterfaceDeclarations;
    property ImplementationSection: TImplementationSectionSyntax read FImplementationSection write FImplementationSection;
    property IntfImplDeclarations: IList<TDeclarationSectionSyntax> read FIntfImplDeclarations;
    property PostImplementationDeclarations: IList<TDeclarationSectionSyntax> read FPostImplementationDeclarations;
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
  FNamespaces := Collections.NewList<TSyntaxToken>;
  FDots := Collections.NewList<TSyntaxToken>;
end;

destructor TUnitReferenceSyntax.Destroy;
begin
  FInKeyword.Free;
  FStringLiteral.Free;
  inherited;
end;

{ TUsesClauseSyntax }

constructor TUsesClauseSyntax.Create;
begin
  inherited Create;
  FUnitReferences := Collections.NewList<TUnitReferenceSyntax>;
  FCommas := Collections.NewList<TSyntaxToken>;
end;

destructor TUsesClauseSyntax.Destroy;
begin
  FUsesKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TClassMemberSyntax }

constructor TClassMemberSyntax.Create;
begin
  inherited Create;
  FTokens := Collections.NewList<TSyntaxToken>;
end;

{ TVisibilitySectionSyntax }

constructor TVisibilitySectionSyntax.Create;
begin
  inherited Create;
  FMembers := Collections.NewList<TClassMemberSyntax>;
end;

destructor TVisibilitySectionSyntax.Destroy;
begin
  FStrictKeyword.Free;
  FVisibilityKeyword.Free;
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
  FGenericParameterTokens := Collections.NewList<TSyntaxToken>;
  FTypeExtraTokens := Collections.NewList<TSyntaxToken>;
  FBaseListTokens := Collections.NewList<TSyntaxToken>;
  FVisibilitySections := Collections.NewList<TVisibilitySectionSyntax>;
  FTrailingTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TTypeDeclarationSyntax.Destroy;
begin
  FIdentifier.Free;
  FEqualsToken.Free;
  FTypeTypeToken.Free;
  FEndKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TTypeSectionSyntax }

constructor TTypeSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := Collections.NewList<TTypeDeclarationSyntax>;
end;

destructor TTypeSectionSyntax.Destroy;
begin
  FTypeKeyword.Free;
  inherited;
end;

{ TConstDeclarationSyntax }

constructor TConstDeclarationSyntax.Create;
begin
  inherited Create;
  FValueTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TConstDeclarationSyntax.Destroy;
begin
  FIdentifier.Free;
  FColonToken.Free;
  FTypeIdentifier.Free;
  FEqualsToken.Free;
  FSemicolon.Free;
  inherited;
end;

{ TVarDeclarationSyntax }

constructor TVarDeclarationSyntax.Create;
begin
  inherited Create;
  FTypeExtraTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TVarDeclarationSyntax.Destroy;
begin
  FIdentifier.Free;
  FColonToken.Free;
  FTypeIdentifier.Free;
  FSemicolon.Free;
  inherited;
end;

{ TConstSectionSyntax }

constructor TConstSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := Collections.NewList<TConstDeclarationSyntax>;
end;

destructor TConstSectionSyntax.Destroy;
begin
  FConstKeyword.Free;
  inherited;
end;

{ TVarSectionSyntax }

constructor TVarSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := Collections.NewList<TVarDeclarationSyntax>;
end;

destructor TVarSectionSyntax.Destroy;
begin
  FVarKeyword.Free;
  inherited;
end;

{ TWhileStatementSyntax }

constructor TWhileStatementSyntax.Create;
begin
  inherited Create;
  FConditionTokens := Collections.NewList<TSyntaxToken>;
  FBodyTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TWhileStatementSyntax.Destroy;
begin
  FWhileKeyword.Free;
  FDoKeyword.Free;
  FStatement.Free;
  inherited;
end;

{ TRepeatStatementSyntax }

constructor TRepeatStatementSyntax.Create;
begin
  inherited Create;
  FStatements := Collections.NewList<TStatementSyntax>;
  FConditionTokens := Collections.NewList<TSyntaxToken>;
  FBodyTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TRepeatStatementSyntax.Destroy;
begin
  FRepeatKeyword.Free;
  FUntilKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TForStatementSyntax }

constructor TForStatementSyntax.Create;
begin
  inherited Create;
  FVariableTokens := Collections.NewList<TSyntaxToken>;
  FStartTokens := Collections.NewList<TSyntaxToken>;
  FEndTokens := Collections.NewList<TSyntaxToken>;
  FCollectionTokens := Collections.NewList<TSyntaxToken>;
  FBodyTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TForStatementSyntax.Destroy;
begin
  FForKeyword.Free;
  FAssignmentToken.Free;
  FToDowntoKeyword.Free;
  FInKeyword.Free;
  FDoKeyword.Free;
  FStatement.Free;
  inherited;
end;

{ TIfStatementSyntax }

constructor TIfStatementSyntax.Create;
begin
  inherited Create;
  FConditionTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TIfStatementSyntax.Destroy;
begin
  FIfKeyword.Free;
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
  FLeftTokens := Collections.NewList<TSyntaxToken>;
  FRightTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TAssignmentStatementSyntax.Destroy;
begin
  FColonEqualsToken.Free;
  inherited;
end;

{ TBeginEndStatementSyntax }

constructor TBeginEndStatementSyntax.Create;
begin
  inherited Create;
  FStatements := Collections.NewList<TStatementSyntax>;
end;

destructor TBeginEndStatementSyntax.Destroy;
begin
  FBeginKeyword.Free;
  FEndKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TTryStatementSyntax }

constructor TTryStatementSyntax.Create;
begin
  inherited Create;
  FStatements := Collections.NewList<TStatementSyntax>;
  FFinallyExceptStatements := Collections.NewList<TStatementSyntax>;
end;

destructor TTryStatementSyntax.Destroy;
begin
  FTryKeyword.Free;
  FFinallyKeyword.Free;
  FExceptKeyword.Free;
  FEndKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TRaiseStatementSyntax }

constructor TRaiseStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TRaiseStatementSyntax.Destroy;
begin
  FRaiseKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TCaseItemSyntax }

constructor TCaseItemSyntax.Create;
begin
  inherited Create;
  FValueTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TCaseItemSyntax.Destroy;
begin
  FColonToken.Free;
  FStatement.Free;
  inherited;
end;

{ TCaseStatementSyntax }

constructor TCaseStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := Collections.NewList<TSyntaxToken>;
  FCaseItems := Collections.NewList<TCaseItemSyntax>;
  FElseStatements := Collections.NewList<TStatementSyntax>;
end;

destructor TCaseStatementSyntax.Destroy;
begin
  FCaseKeyword.Free;
  FOfKeyword.Free;
  FElseKeyword.Free;
  FEndKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TWithStatementSyntax }

constructor TWithStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TWithStatementSyntax.Destroy;
begin
  FWithKeyword.Free;
  FDoKeyword.Free;
  FStatement.Free;
  inherited;
end;

{ TProcedureCallStatementSyntax }

{ TInheritedStatementSyntax }

constructor TInheritedStatementSyntax.Create;
begin
  inherited Create;
  FCallTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TInheritedStatementSyntax.Destroy;
begin
  FInheritedKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TExitStatementSyntax }

constructor TExitStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TExitStatementSyntax.Destroy;
begin
  FExitKeyword.Free;
  FSemicolon.Free;
  inherited;
end;

{ TProcedureCallStatementSyntax }

constructor TProcedureCallStatementSyntax.Create;
begin
  inherited Create;
  FExpressionTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TProcedureCallStatementSyntax.Destroy;
begin
  FSemicolon.Free;
  inherited;
end;

{ TOpaqueStatementSyntax }

{ TInlineVarStatementSyntax }

constructor TInlineVarStatementSyntax.Create;
begin
  inherited Create;
  FDeclarationTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TInlineVarStatementSyntax.Destroy;
begin
  FVarKeyword.Free;
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
  FTokens := Collections.NewList<TSyntaxToken>;
end;

{ TInterfaceSectionSyntax }

constructor TInterfaceSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := Collections.NewList<TDeclarationSectionSyntax>;
end;

destructor TInterfaceSectionSyntax.Destroy;
begin
  FInterfaceKeyword.Free;
  FUsesClause.Free;
  inherited;
end;

{ TUnparsedDeclarationSyntax }

constructor TUnparsedDeclarationSyntax.Create;
begin
  inherited Create;
  FTokens := Collections.NewList<TSyntaxToken>;
end;

{ TMethodImplementationSyntax }

constructor TMethodImplementationSyntax.Create;
begin
  inherited Create;
  FSignatureTokens := Collections.NewList<TSyntaxToken>;
  FLocalDeclarations := Collections.NewList<TDeclarationSectionSyntax>;
  FStatements := Collections.NewList<TStatementSyntax>;
  FBodyTokens := Collections.NewList<TSyntaxToken>;
end;

destructor TMethodImplementationSyntax.Destroy;
begin
  FMethodTypeKeyword.Free;
  FSignatureSemicolon.Free;
  FBeginKeyword.Free;
  FEndKeyword.Free;
  FFinalSemicolon.Free;
  inherited;
end;

{ TImplementationSectionSyntax }

constructor TImplementationSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := Collections.NewList<TDeclarationSectionSyntax>;
end;

destructor TImplementationSectionSyntax.Destroy;
begin
  FImplementationKeyword.Free;
  FUsesClause.Free;
  inherited;
end;

{ TCompilationUnitSyntax }

constructor TCompilationUnitSyntax.Create;
begin
  inherited Create;
  FNamespaces := Collections.NewList<TSyntaxToken>;
  FDots := Collections.NewList<TSyntaxToken>;
  FPreInterfaceDeclarations := Collections.NewList<TDeclarationSectionSyntax>;
  FIntfImplDeclarations := Collections.NewList<TDeclarationSectionSyntax>;
  FPostImplementationDeclarations := Collections.NewList<TDeclarationSectionSyntax>;
end;

destructor TCompilationUnitSyntax.Destroy;
begin
  FUnitKeyword.Free;
  FSemicolon.Free;
  FInterfaceSection.Free;
  FImplementationSection.Free;
  FFinalEndKeyword.Free;
  FFinalDotToken.Free;
  FEndOfFileToken.Free;
  inherited;
end;

end.
