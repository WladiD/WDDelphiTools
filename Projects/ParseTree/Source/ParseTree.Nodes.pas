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
    FNamespaces: TList<TSyntaxToken>;
    FDots: TList<TSyntaxToken>;
    FInKeyword: TSyntaxToken;
    FStringLiteral: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property Namespaces: TList<TSyntaxToken> read FNamespaces;
    property Dots: TList<TSyntaxToken> read FDots;
    property InKeyword: TSyntaxToken read FInKeyword write FInKeyword;
    property StringLiteral: TSyntaxToken read FStringLiteral write FStringLiteral;
  end;

  { uses ...; }
  TUsesClauseSyntax = class(TSyntaxNode)
  private
    FUsesKeyword: TSyntaxToken;
    FUnitReferences: TList<TUnitReferenceSyntax>;
    FCommas: TList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property UsesKeyword: TSyntaxToken read FUsesKeyword write FUsesKeyword;
    property UnitReferences: TList<TUnitReferenceSyntax> read FUnitReferences;
    property Commas: TList<TSyntaxToken> read FCommas;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { Base class for Declaration Sections in Interface/Implementation }
  TDeclarationSectionSyntax = class abstract(TSyntaxNode)
  end;

  { type ... }
  TTypeSectionSyntax = class(TDeclarationSectionSyntax)
  private
    FTypeKeyword: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property TypeKeyword: TSyntaxToken read FTypeKeyword write FTypeKeyword;
  end;

  { const ... }
  TConstSectionSyntax = class(TDeclarationSectionSyntax)
  private
    FConstKeyword: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property ConstKeyword: TSyntaxToken read FConstKeyword write FConstKeyword;
  end;

  { var ... }
  TVarSectionSyntax = class(TDeclarationSectionSyntax)
  private
    FVarKeyword: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;
    property VarKeyword: TSyntaxToken read FVarKeyword write FVarKeyword;
  end;

  { interface ... }
  TInterfaceSectionSyntax = class(TSyntaxNode)
  private
    FInterfaceKeyword: TSyntaxToken;
    FUsesClause: TUsesClauseSyntax;
    FDeclarations: TList<TDeclarationSectionSyntax>;
  public
    constructor Create;
    destructor Destroy; override;

    property InterfaceKeyword: TSyntaxToken read FInterfaceKeyword write FInterfaceKeyword;
    property UsesClause: TUsesClauseSyntax read FUsesClause write FUsesClause;
    property Declarations: TList<TDeclarationSectionSyntax> read FDeclarations;
  end;

  { unit Unit1; interface ... }
  TCompilationUnitSyntax = class(TSyntaxNode)
  private
    FUnitKeyword: TSyntaxToken;
    FIdentifier: TSyntaxToken;
    FSemicolon: TSyntaxToken;
    FInterfaceSection: TInterfaceSectionSyntax;
  public
    constructor Create;
    destructor Destroy; override;

    property UnitKeyword: TSyntaxToken read FUnitKeyword write FUnitKeyword;
    property Identifier: TSyntaxToken read FIdentifier write FIdentifier;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
    property InterfaceSection: TInterfaceSectionSyntax read FInterfaceSection write FInterfaceSection;
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
  FNamespaces := TList<TSyntaxToken>.Create;
  FDots := TList<TSyntaxToken>.Create;
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
  FUnitReferences := TList<TUnitReferenceSyntax>.Create;
  FCommas := TList<TSyntaxToken>.Create;
end;

destructor TUsesClauseSyntax.Destroy;
var
  LRef: TUnitReferenceSyntax;
begin
  FUsesKeyword.Free;
  for LRef in FUnitReferences do
    LRef.Free;
  FUnitReferences.Free;
  FCommas.Free;
  FSemicolon.Free;
  inherited;
end;

{ TTypeSectionSyntax }

constructor TTypeSectionSyntax.Create;
begin
  inherited Create;
end;

destructor TTypeSectionSyntax.Destroy;
begin
  FTypeKeyword.Free;
  inherited;
end;

{ TConstSectionSyntax }

constructor TConstSectionSyntax.Create;
begin
  inherited Create;
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
end;

destructor TVarSectionSyntax.Destroy;
begin
  FVarKeyword.Free;
  inherited;
end;

{ TInterfaceSectionSyntax }

constructor TInterfaceSectionSyntax.Create;
begin
  inherited Create;
  FDeclarations := TList<TDeclarationSectionSyntax>.Create;
end;

destructor TInterfaceSectionSyntax.Destroy;
var
  LDecl: TDeclarationSectionSyntax;
begin
  FInterfaceKeyword.Free;
  FUsesClause.Free;
  for LDecl in FDeclarations do
    LDecl.Free;
  FDeclarations.Free;
  inherited;
end;

{ TCompilationUnitSyntax }

constructor TCompilationUnitSyntax.Create;
begin
  inherited Create;
end;

destructor TCompilationUnitSyntax.Destroy;
begin
  FUnitKeyword.Free;
  FIdentifier.Free;
  FSemicolon.Free;
  FInterfaceSection.Free;
  inherited;
end;

end.
