unit ParseTree.Parser;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  ParseTree.Core,
  ParseTree.Nodes,
  ParseTree.Tokens,
  ParseTree.Lexer;

type
  { TParseTreeParser is the main entry point to parse Pascal source code into a CST }
  TParseTreeParser = class
  private
    FTokens: TList<TSyntaxToken>;
    FPosition: Integer;
    function Peek(AOffset: Integer = 0): TSyntaxToken;
    function Current: TSyntaxToken;
    function NextToken: TSyntaxToken;
    function MatchToken(AKind: TTokenKind): TSyntaxToken;
  public
    function Parse(const AText: string): TCompilationUnitSyntax;
    function ParseUsesClause: TUsesClauseSyntax;
    function ParseDeclarationSection: TDeclarationSectionSyntax;
    function ParseConstDeclaration: TConstDeclarationSyntax;
    function ParseVarDeclaration: TVarDeclarationSyntax;
    function ParseInterfaceSection: TInterfaceSectionSyntax;
  end;

implementation

{ TParseTreeParser }

function TParseTreeParser.Peek(AOffset: Integer): TSyntaxToken;
var
  LIndex: Integer;
begin
  LIndex := FPosition + AOffset;
  if (LIndex >= 0) and (LIndex < FTokens.Count) then
    Result := FTokens[LIndex]
  else if FTokens.Count > 0 then
    Result := FTokens[FTokens.Count - 1] // Return EOF token usually at the end
  else
    Result := nil;
end;

function TParseTreeParser.Current: TSyntaxToken;
begin
  Result := Peek(0);
end;

function TParseTreeParser.NextToken: TSyntaxToken;
begin
  Result := Current;
  Inc(FPosition);
end;

function TParseTreeParser.MatchToken(AKind: TTokenKind): TSyntaxToken;
begin
  if (Current <> nil) and (Current.Kind = AKind) then
    Result := NextToken
  else
    Result := nil; // In a full parser, we'd add a diagnostic here (e.g. "Expected X")
end;

function TParseTreeParser.ParseUsesClause: TUsesClauseSyntax;
var
  LUnitRef: TUnitReferenceSyntax;
begin
  Result := nil;
  if (Current = nil) or (Current.Kind <> tkUsesKeyword) then
    Exit;
    
  Result := TUsesClauseSyntax.Create;
  Result.UsesKeyword := MatchToken(tkUsesKeyword);
  
  while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
  begin
    LUnitRef := TUnitReferenceSyntax.Create;
    
    // Parse the unit identifier(s) and dots (e.g. System.SysUtils)
    while (Current <> nil) and 
          ((Current.Kind = tkIdentifier) or (Current.Kind = tkDot) or 
           (Current.Kind = tkUnitKeyword) or (Current.Kind = tkInterfaceKeyword) or
           (Current.Kind = tkImplementationKeyword) or (Current.Kind = tkUsesKeyword)) do
    begin
      if Current.Kind = tkDot then
        LUnitRef.Dots.Add(NextToken)
      else
        LUnitRef.Namespaces.Add(NextToken);
    end;
    
    // Parse optional 'in' clause (e.g. in '..\Unit1.pas')
    if (Current <> nil) and (Current.Kind = tkInKeyword) then
    begin
      LUnitRef.InKeyword := MatchToken(tkInKeyword);
      
      if (Current <> nil) and (Current.Kind = tkStringLiteral) then
        LUnitRef.StringLiteral := MatchToken(tkStringLiteral);
    end;
    
    Result.UnitReferences.Add(LUnitRef);
    
    // Look for comma separating units
    if (Current <> nil) and (Current.Kind = tkComma) then
      Result.Commas.Add(MatchToken(tkComma))
    else
      Break; // If no comma, we should be at a semicolon (or syntax error)
  end;
  
  Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseConstDeclaration: TConstDeclarationSyntax;
begin
  Result := TConstDeclarationSyntax.Create;
  Result.Identifier := MatchToken(tkIdentifier);
  
  if (Current <> nil) and (Current.Kind = tkColon) then
  begin
    Result.ColonToken := MatchToken(tkColon);
    Result.TypeIdentifier := MatchToken(tkIdentifier);
  end;
  
  if (Current <> nil) and (Current.Kind = tkEquals) then
  begin
    Result.EqualsToken := MatchToken(tkEquals);
    // Rough parsing for phase 4: assume the next token is the value
    // In a full implementation, we need an expression parser here.
    Result.ValueToken := NextToken;
  end;
  
  // Optional type annotation in const (e.g. const X: Integer = 5;) is skipped for now
  
  // fast forward to semicolon
  while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
    NextToken;
    
  Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseVarDeclaration: TVarDeclarationSyntax;
begin
  Result := TVarDeclarationSyntax.Create;
  Result.Identifier := MatchToken(tkIdentifier);
  
  if (Current <> nil) and (Current.Kind = tkColon) then
  begin
    Result.ColonToken := MatchToken(tkColon);
    Result.TypeIdentifier := MatchToken(tkIdentifier); // Basic support for now
  end;
  
  // fast forward to semicolon
  while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
    NextToken;
    
  Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseDeclarationSection: TDeclarationSectionSyntax;
var
  LTypeSec: TTypeSectionSyntax;
  LConstSec: TConstSectionSyntax;
  LVarSec: TVarSectionSyntax;
begin
  Result := nil;
  if Current = nil then Exit;

  if Current.Kind = tkTypeKeyword then
  begin
    LTypeSec := TTypeSectionSyntax.Create;
    LTypeSec.TypeKeyword := MatchToken(tkTypeKeyword);
    // Future Phase: Parse inside type block
    Result := LTypeSec;
  end
  else if Current.Kind = tkConstKeyword then
  begin
    LConstSec := TConstSectionSyntax.Create;
    LConstSec.ConstKeyword := MatchToken(tkConstKeyword);
    
    while (Current <> nil) and 
          (Current.Kind = tkIdentifier) do
    begin
      LConstSec.Declarations.Add(ParseConstDeclaration);
    end;
    
    Result := LConstSec;
  end
  else if Current.Kind = tkVarKeyword then
  begin
    LVarSec := TVarSectionSyntax.Create;
    LVarSec.VarKeyword := MatchToken(tkVarKeyword);

    while (Current <> nil) and 
          (Current.Kind = tkIdentifier) do
    begin
      LVarSec.Declarations.Add(ParseVarDeclaration);
    end;
    
    Result := LVarSec;
  end;
end;

function TParseTreeParser.ParseInterfaceSection: TInterfaceSectionSyntax;
var
  LDecl: TDeclarationSectionSyntax;
begin
  Result := nil;
  if (Current = nil) or (Current.Kind <> tkInterfaceKeyword) then
    Exit;
    
  Result := TInterfaceSectionSyntax.Create;
  Result.InterfaceKeyword := MatchToken(tkInterfaceKeyword);
  
  // Look for uses clause right after interface
  if (Current <> nil) and (Current.Kind = tkUsesKeyword) then
    Result.UsesClause := ParseUsesClause();

  // Parse interface declarations: type, const, var
  while (Current <> nil) and 
        (Current.Kind <> tkImplementationKeyword) and 
        (Current.Kind <> tkEOF) do
  begin
    if (Current.Kind = tkTypeKeyword) or 
       (Current.Kind = tkConstKeyword) or 
       (Current.Kind = tkVarKeyword) then
    begin
      LDecl := ParseDeclarationSection;
      if Assigned(LDecl) then
        Result.Declarations.Add(LDecl);
    end
    else
    begin
      // Skip token if it's not a known declaration keyword block for Phase 3
      // We will refine this in later phases as we drill down into declarations.
      NextToken;
    end;
  end;
end;

function TParseTreeParser.Parse(const AText: string): TCompilationUnitSyntax;
var
  LLexer: TParseTreeLexer;
begin
  Result := TCompilationUnitSyntax.Create;
  LLexer := TParseTreeLexer.Create(AText);
  try
    FTokens := LLexer.TokenizeAll;
    try
      FPosition := 0;
      
      // Parse unit declaration
      if (Current <> nil) and (Current.Kind = tkUnitKeyword) then
      begin
        Result.UnitKeyword := MatchToken(tkUnitKeyword);
        Result.Identifier := MatchToken(tkIdentifier);
        Result.Semicolon := MatchToken(tkSemicolon);
      end;
      
      // Fast-forward to interface section just in case there are comments
      while (Current <> nil) and (Current.Kind <> tkInterfaceKeyword) and (Current.Kind <> tkEOF) do
        NextToken;
        
      if (Current <> nil) and (Current.Kind = tkInterfaceKeyword) then
        Result.InterfaceSection := ParseInterfaceSection();
        
    finally
      FTokens.Free;
      FTokens := nil;
    end;
  finally
    LLexer.Free;
  end;
end;

end.
