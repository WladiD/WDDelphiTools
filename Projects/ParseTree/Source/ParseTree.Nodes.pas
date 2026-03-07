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
    FValueToken: TSyntaxToken; // Basic value for now
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property ColonToken: TSyntaxToken read FColonToken write FColonToken;
    property TypeIdentifier: TSyntaxToken read FTypeIdentifier write FTypeIdentifier;
    property EqualsToken: TSyntaxToken read FEqualsToken write FEqualsToken;
    property ValueToken: TSyntaxToken read FValueToken write FValueToken;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { var Name: Type; }
  TVarDeclarationSyntax = class(TSyntaxNode)
  private
    FIdentifier: TSyntaxToken;
    FColonToken: TSyntaxToken;
    FTypeIdentifier: TSyntaxToken; // Basic type for now
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property ColonToken: TSyntaxToken read FColonToken write FColonToken;
    property TypeIdentifier: TSyntaxToken read FTypeIdentifier write FTypeIdentifier;
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
    FEqualsToken: TSyntaxToken;
    FTypeTypeToken: TSyntaxToken; // e.g. class, interface, array, etc
    FBaseListTokens: TObjectList<TSyntaxToken>; // Holds '(' 'TObject' ')'
    FVisibilitySections: TObjectList<TVisibilitySectionSyntax>;
    FEndKeyword: TSyntaxToken;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property EqualsToken: TSyntaxToken read FEqualsToken write FEqualsToken;
    property TypeTypeToken: TSyntaxToken read FTypeTypeToken write FTypeTypeToken;
    property BaseListTokens: TObjectList<TSyntaxToken> read FBaseListTokens;
    property VisibilitySections: TObjectList<TVisibilitySectionSyntax> read FVisibilitySections;
    property EndKeyword: TSyntaxToken read FEndKeyword write FEndKeyword;
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

  { Represents a generic unparsed block of tokens }
  TUnparsedDeclarationSyntax = class(TDeclarationSectionSyntax)
  private
    FTokens: TObjectList<TSyntaxToken>;
  public
    constructor Create;
    destructor Destroy; override;
    property Tokens: TObjectList<TSyntaxToken> read FTokens;
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
    FImplementationSection: TImplementationSectionSyntax;
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
    property ImplementationSection: TImplementationSectionSyntax read FImplementationSection write FImplementationSection;
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
  FNamespaces := TObjectList<TSyntaxToken>.Create;
  FDots := TObjectList<TSyntaxToken>.Create;
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
  FCommas := TObjectList<TSyntaxToken>.Create;
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
  FTokens := TObjectList<TSyntaxToken>.Create;
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
  FBaseListTokens := TObjectList<TSyntaxToken>.Create;
  FVisibilitySections := TObjectList<TVisibilitySectionSyntax>.Create;
end;

destructor TTypeDeclarationSyntax.Destroy;
begin
  FIdentifier.Free;
  FEqualsToken.Free;
  FTypeTypeToken.Free;
  FBaseListTokens.Free;
  FVisibilitySections.Free;
  FEndKeyword.Free;
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
end;

destructor TConstDeclarationSyntax.Destroy;
begin
  FIdentifier.Free;
  FColonToken.Free;
  FTypeIdentifier.Free;
  FEqualsToken.Free;
  FValueToken.Free;
  FSemicolon.Free;
  inherited;
end;

{ TVarDeclarationSyntax }

constructor TVarDeclarationSyntax.Create;
begin
  inherited Create;
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
  FTokens := TObjectList<TSyntaxToken>.Create;
end;

destructor TUnparsedDeclarationSyntax.Destroy;
begin
  FTokens.Free;
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
  FNamespaces := TObjectList<TSyntaxToken>.Create;
  FDots := TObjectList<TSyntaxToken>.Create;
end;

destructor TCompilationUnitSyntax.Destroy;
begin
  FUnitKeyword.Free;
  FNamespaces.Free;
  FDots.Free;
  FSemicolon.Free;
  FInterfaceSection.Free;
  FImplementationSection.Free;
  FFinalEndKeyword.Free;
  FFinalDotToken.Free;
  FEndOfFileToken.Free;
  inherited;
end;

end.
