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

  { uses ...; }
  TUsesClauseSyntax = class(TSyntaxNode)
  private
    FUsesKeyword: TSyntaxToken;
    FIdentifiers: TList<TSyntaxToken>;
    FSemicolon: TSyntaxToken;
  public
    constructor Create;
    destructor Destroy; override;

    property UsesKeyword: TSyntaxToken read FUsesKeyword write FUsesKeyword;
    property Identifiers: TList<TSyntaxToken> read FIdentifiers;
    property Semicolon: TSyntaxToken read FSemicolon write FSemicolon;
  end;

  { interface ... }
  TInterfaceSectionSyntax = class(TSyntaxNode)
  private
    FInterfaceKeyword: TSyntaxToken;
    FUsesClause: TUsesClauseSyntax;
  public
    constructor Create;
    destructor Destroy; override;

    property InterfaceKeyword: TSyntaxToken read FInterfaceKeyword write FInterfaceKeyword;
    property UsesClause: TUsesClauseSyntax read FUsesClause write FUsesClause;
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

{ TUsesClauseSyntax }

constructor TUsesClauseSyntax.Create;
begin
  inherited Create;
  FIdentifiers := TList<TSyntaxToken>.Create;
end;

destructor TUsesClauseSyntax.Destroy;
begin
  FUsesKeyword.Free;
  FIdentifiers.Free;
  FSemicolon.Free;
  inherited;
end;

{ TInterfaceSectionSyntax }

constructor TInterfaceSectionSyntax.Create;
begin
  inherited Create;
end;

destructor TInterfaceSectionSyntax.Destroy;
begin
  FInterfaceKeyword.Free;
  FUsesClause.Free;
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
