unit ParseTree.Core;

interface

uses
  System.Generics.Collections, ParseTree.Tokens;

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
    FKind: TTokenKind;
    FText: string;
    FLeadingTrivia: TObjectList<TSyntaxTrivia>;
    FTrailingTrivia: TObjectList<TSyntaxTrivia>;
    function GetLeadingTrivia: TObjectList<TSyntaxTrivia>;
    function GetTrailingTrivia: TObjectList<TSyntaxTrivia>;
    function GetHasLeadingTrivia: Boolean;
    function GetHasTrailingTrivia: Boolean;
  public
    constructor Create(AKind: TTokenKind; const AText: string);
    destructor Destroy; override;

    property Kind: TTokenKind read FKind;
    property Text: string read FText;
    property HasLeadingTrivia: Boolean read GetHasLeadingTrivia;
    property HasTrailingTrivia: Boolean read GetHasTrailingTrivia;
    property LeadingTrivia: TObjectList<TSyntaxTrivia> read GetLeadingTrivia;
    property TrailingTrivia: TObjectList<TSyntaxTrivia> read GetTrailingTrivia;
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

constructor TSyntaxToken.Create(AKind: TTokenKind; const AText: string);
begin
  inherited Create;
  FKind := AKind;
  FText := AText;
end;

destructor TSyntaxToken.Destroy;
begin
  FLeadingTrivia.Free;
  FTrailingTrivia.Free;
  inherited;
end;

function TSyntaxToken.GetHasLeadingTrivia: Boolean;
begin
  Result := (FLeadingTrivia <> nil) and (FLeadingTrivia.Count > 0);
end;

function TSyntaxToken.GetHasTrailingTrivia: Boolean;
begin
  Result := (FTrailingTrivia <> nil) and (FTrailingTrivia.Count > 0);
end;

function TSyntaxToken.GetLeadingTrivia: TObjectList<TSyntaxTrivia>;
begin
  if FLeadingTrivia = nil then FLeadingTrivia := TObjectList<TSyntaxTrivia>.Create;
  Result := FLeadingTrivia;
end;

function TSyntaxToken.GetTrailingTrivia: TObjectList<TSyntaxTrivia>;
begin
  if FTrailingTrivia = nil then FTrailingTrivia := TObjectList<TSyntaxTrivia>.Create;
  Result := FTrailingTrivia;
end;

end.
