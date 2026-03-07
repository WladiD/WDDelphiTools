unit ParseTree.Core;

interface

uses
  System.Generics.Collections;

type
  { Base class for all syntax nodes, tokens, and trivia }
  TSyntaxElement = class abstract
  end;

  { Diagnostic information (e.g., syntax errors) }
  TSyntaxDiagnostic = class
  public
    Message: string;
    constructor Create(const AMessage: string);
  end;

  { Base class for trivia (spaces, comments, newlines) }
  TSyntaxTrivia = class(TSyntaxElement)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    property Text: string read FText;
  end;

  { Base class for tokens (keywords, identifiers, punctuation) }
  TSyntaxToken = class(TSyntaxElement)
  private
    FKind: Integer; // Will map to TTokenKind
    FText: string;
    FLeadingTrivia: TObjectList<TSyntaxTrivia>;
    FTrailingTrivia: TObjectList<TSyntaxTrivia>;
  public
    constructor Create(AKind: Integer; const AText: string);
    destructor Destroy; override;

    property Kind: Integer read FKind;
    property Text: string read FText;
    property LeadingTrivia: TObjectList<TSyntaxTrivia> read FLeadingTrivia;
    property TrailingTrivia: TObjectList<TSyntaxTrivia> read FTrailingTrivia;
  end;

  { Base class for syntax nodes (statements, expressions, etc.) }
  TSyntaxNode = class abstract(TSyntaxElement)
  private
    FParent: TSyntaxNode;
  public
    property Parent: TSyntaxNode read FParent write FParent;
  end;

implementation

{ TSyntaxDiagnostic }

constructor TSyntaxDiagnostic.Create(const AMessage: string);
begin
  inherited Create;
  Message := AMessage;
end;

{ TSyntaxTrivia }

constructor TSyntaxTrivia.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

{ TSyntaxToken }

constructor TSyntaxToken.Create(AKind: Integer; const AText: string);
begin
  inherited Create;
  FKind := AKind;
  FText := AText;
  FLeadingTrivia := TObjectList<TSyntaxTrivia>.Create;
  FTrailingTrivia := TObjectList<TSyntaxTrivia>.Create;
end;

destructor TSyntaxToken.Destroy;
begin
  FLeadingTrivia.Free;
  FTrailingTrivia.Free;
  inherited;
end;

end.
