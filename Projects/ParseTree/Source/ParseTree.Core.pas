// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit ParseTree.Core;

interface

uses

  System.Generics.Collections,

  ParseTree.Tokens;

type

  /// <summary>Base class for all syntax nodes, tokens, and trivia</summary>
  TSyntaxElement = class abstract;

  /// <summary>Diagnostic information (e.g., syntax errors)</summary>
  TSyntaxDiagnostic = class
  public
    Message: string;
    constructor Create(const AMessage: String);
  end;

  /// <summary>Base class for trivia (spaces, comments, newlines)</summary>
  TSyntaxTrivia = class(TSyntaxElement)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    property Text: string read FText;
  end;

  /// <summary>Base class for tokens (keywords, identifiers, punctuation)</summary>
  TSyntaxToken = class(TSyntaxElement)
  private
    FKind          : TTokenKind;
    FLeadingTrivia : TObjectList<TSyntaxTrivia>;
    FNextToken     : TSyntaxToken;
    FPrevToken     : TSyntaxToken;
    FText          : String;
    FTrailingTrivia: TObjectList<TSyntaxTrivia>;
    function GetHasLeadingTrivia: Boolean;
    function GetHasTrailingTrivia: Boolean;
    function GetLeadingTrivia: TObjectList<TSyntaxTrivia>;
    function GetTrailingTrivia: TObjectList<TSyntaxTrivia>;
  public
    constructor Create(AKind: TTokenKind; const AText: string);
    destructor Destroy; override;

    property HasLeadingTrivia: Boolean read GetHasLeadingTrivia;
    property HasTrailingTrivia: Boolean read GetHasTrailingTrivia;
    property Kind: TTokenKind read FKind;
    property LeadingTrivia: TObjectList<TSyntaxTrivia> read GetLeadingTrivia;
    property NextToken: TSyntaxToken read FNextToken write FNextToken;
    property PrevToken: TSyntaxToken read FPrevToken write FPrevToken;
    property Text: string read FText;
    property TrailingTrivia: TObjectList<TSyntaxTrivia> read GetTrailingTrivia;
  end;

  /// <summary>Base class for syntax nodes (statements, expressions, etc.)</summary>
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
  Result := Assigned(FLeadingTrivia) and (FLeadingTrivia.Count > 0);
end;

function TSyntaxToken.GetHasTrailingTrivia: Boolean;
begin
  Result := Assigned(FTrailingTrivia) and (FTrailingTrivia.Count > 0);
end;

function TSyntaxToken.GetLeadingTrivia: TObjectList<TSyntaxTrivia>;
begin
  if not Assigned(FLeadingTrivia) then
    FLeadingTrivia := TObjectList<TSyntaxTrivia>.Create;
  Result := FLeadingTrivia;
end;

function TSyntaxToken.GetTrailingTrivia: TObjectList<TSyntaxTrivia>;
begin
  if not Assigned(FTrailingTrivia) then
    FTrailingTrivia := TObjectList<TSyntaxTrivia>.Create;
  Result := FTrailingTrivia;
end;

end.
