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
    function ParseTypeDeclaration: TTypeDeclarationSyntax;
    function ParseConstDeclaration: TConstDeclarationSyntax;
    function ParseVarDeclaration: TVarDeclarationSyntax;
    function ParseInterfaceSection: TInterfaceSectionSyntax;
    function ParseMethodImplementation(const AFullSource: string = ''): TMethodImplementationSyntax;
    function ParseImplementationSection: TImplementationSectionSyntax;
    function ParseStatement: TStatementSyntax;
    function ParseWhileStatement: TWhileStatementSyntax;
    function ParseRepeatStatement: TRepeatStatementSyntax;
    function ParseForStatement: TForStatementSyntax;
    function ParseIfStatement: TIfStatementSyntax;
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
  else
    Result := nil;
end;

function TParseTreeParser.Current: TSyntaxToken;
begin
  Result := Peek(0);
end;

function TParseTreeParser.NextToken: TSyntaxToken;
begin
  if FPosition < FTokens.Count then
  begin
    Result := FTokens[FPosition];
    Inc(FPosition);
  end
  else
    Result := nil;
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
    // Read value tokens until semicolon
    while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
      Result.ValueTokens.Add(NextToken);
  end;
    
  Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseVarDeclaration: TVarDeclarationSyntax;
begin
  Result := TVarDeclarationSyntax.Create;
  Result.Identifier := MatchToken(tkIdentifier);
  
  if (Current <> nil) and (Current.Kind = tkColon) then
  begin
    Result.ColonToken := MatchToken(tkColon);
    Result.TypeIdentifier := MatchToken(tkIdentifier);
  end;
  
  // Collect remaining type tokens (e.g. <String> for TArray<String>)
  while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
    Result.TypeExtraTokens.Add(NextToken);
    
  Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseTypeDeclaration: TTypeDeclarationSyntax;

  function IsVisibilityKeyword(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (AToken.Kind = tkPrivateKeyword) or
      (AToken.Kind = tkProtectedKeyword) or
      (AToken.Kind = tkPublicKeyword) or
      (AToken.Kind = tkPublishedKeyword));
  end;

  function IsMemberStartToken(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (AToken.Kind = tkIdentifier) or
      (AToken.Kind = tkProcedureKeyword) or
      (AToken.Kind = tkFunctionKeyword) or
      (AToken.Kind = tkConstructorKeyword) or
      (AToken.Kind = tkDestructorKeyword) or
      (AToken.Kind = tkPropertyKeyword) or
      (AToken.Kind = tkClassKeyword) or
      (AToken.Kind = tkTypeKeyword) or
      (AToken.Kind = tkVarKeyword) or
      (AToken.Kind = tkConstKeyword));
  end;

  procedure ParseClassMember(ASection: TVisibilitySectionSyntax);
  var
    LMember: TClassMemberSyntax;
    LNestLevel: Integer;
    LClassNestLevel: Integer;
    LPrevKind: TTokenKind;
  begin
    LMember := TClassMemberSyntax.Create;
    LNestLevel := 0;
    LClassNestLevel := 0;
    LPrevKind := tkUnknown;
    
    // Parse tokens until semicolon at nesting level 0
    while (Current <> nil) and (Current.Kind <> tkEOF) do
    begin
      if (Current.Kind = tkOpenParen) or (Current.Kind = tkLessThan) then
        Inc(LNestLevel)
      else if (Current.Kind = tkCloseParen) or (Current.Kind = tkGreaterThan) then
        Dec(LNestLevel)
      // Track nested class/record declarations: "= class" pattern
      else if (Current.Kind = tkClassKeyword) and (LPrevKind = tkEquals) then
        Inc(LClassNestLevel)
      else if (Current.Kind = tkEndKeyword) and (LClassNestLevel > 0) then
        Dec(LClassNestLevel);
      
      if (Current.Kind = tkSemicolon) and (LNestLevel <= 0) and (LClassNestLevel <= 0) then
      begin
        LMember.Tokens.Add(NextToken); // consume semicolon
        
        // Loop to consume method modifiers like `override;`, `stdcall;`, `overload;`
        while (Current <> nil) and 
              (((Current.Kind = tkIdentifier) or (Current.Kind = tkOverrideKeyword)) and 
               (Peek(1) <> nil) and (Peek(1).Kind = tkSemicolon)) do
        begin
          LMember.Tokens.Add(NextToken); // consume modifier
          LMember.Tokens.Add(NextToken); // consume semicolon
        end;
        
        Break;
      end;
      
      LPrevKind := Current.Kind;
      LMember.Tokens.Add(NextToken);
    end;
    
    ASection.Members.Add(LMember);
  end;

  procedure ParseClassBody(ATypeDecl: TTypeDeclarationSyntax);
  var
    LVisSec: TVisibilitySectionSyntax;
  begin
    LVisSec := nil;
    
    while (Current <> nil) and (Current.Kind <> tkEndKeyword) and (Current.Kind <> tkEOF) do
    begin
      // Check for strict private / strict protected
      if (Current.Kind = tkStrictKeyword) and (Peek(1) <> nil) and IsVisibilityKeyword(Peek(1)) then
      begin
        LVisSec := TVisibilitySectionSyntax.Create;
        LVisSec.StrictKeyword := NextToken;
        LVisSec.VisibilityKeyword := NextToken;
        ATypeDecl.VisibilitySections.Add(LVisSec);
      end
      // Check for visibility keyword
      else if IsVisibilityKeyword(Current) then
      begin
        LVisSec := TVisibilitySectionSyntax.Create;
        LVisSec.VisibilityKeyword := NextToken;
        ATypeDecl.VisibilitySections.Add(LVisSec);
      end
      // Member token — needs a visibility section
      else if IsMemberStartToken(Current) then
      begin
        if LVisSec = nil then
        begin
          // Implicit default visibility (published for components or public)
          LVisSec := TVisibilitySectionSyntax.Create;
          ATypeDecl.VisibilitySections.Add(LVisSec);
        end;
        ParseClassMember(LVisSec);
      end
      else if Current.Kind = tkOpenBracket then
      begin
        if LVisSec = nil then
        begin
          LVisSec := TVisibilitySectionSyntax.Create;
          ATypeDecl.VisibilitySections.Add(LVisSec);
        end;
        LVisSec.Members.Add(TClassMemberSyntax.Create);
        LVisSec.Members.Last.Tokens.Add(NextToken); // [
        while (Current <> nil) and (Current.Kind <> tkCloseBracket) and (Current.Kind <> tkEOF) do
          LVisSec.Members.Last.Tokens.Add(NextToken);
        if (Current <> nil) and (Current.Kind = tkCloseBracket) then
          LVisSec.Members.Last.Tokens.Add(NextToken); // ]
      end
      else
      begin
        // If we really hit an unexpected token, maybe just add it to the last member or visibility section?
        // Let's create an empty member for it to achieve roundtrip.
        if LVisSec = nil then
        begin
          LVisSec := TVisibilitySectionSyntax.Create;
          ATypeDecl.VisibilitySections.Add(LVisSec);
        end;
        if LVisSec.Members.Count = 0 then
          LVisSec.Members.Add(TClassMemberSyntax.Create);
        LVisSec.Members.Last.Tokens.Add(NextToken);
      end;
    end;
    
    if (Current <> nil) and (Current.Kind = tkEndKeyword) then
      ATypeDecl.EndKeyword := MatchToken(tkEndKeyword);
  end;

var
  LNestLevel: Integer;
begin
  Result := TTypeDeclarationSyntax.Create;
  Result.Identifier := MatchToken(tkIdentifier);

  // Skip generic type parameters <T> or <T1, T2>
  if (Current <> nil) and (Current.Kind = tkLessThan) then
  begin
    NextToken; // consume '<'
    while (Current <> nil) and (Current.Kind <> tkGreaterThan) and (Current.Kind <> tkEOF) do
      NextToken; // consume type params and commas
    if (Current <> nil) and (Current.Kind = tkGreaterThan) then
      NextToken; // consume '>'
  end;
  
  if (Current <> nil) and (Current.Kind = tkEquals) then
  begin
    Result.EqualsToken := MatchToken(tkEquals);
    
    // Parse what kind of type it is (like class, interface, record)
    if (Current <> nil) and ((Current.Kind = tkClassKeyword) or (Current.Kind = tkInterfaceKeyword) or (Current.Kind = tkRecordKeyword) or (Current.Kind = tkIdentifier)) then
      Result.TypeTypeToken := NextToken;
      
    if (Result.TypeTypeToken <> nil) and 
       ((Result.TypeTypeToken.Kind = tkClassKeyword) or (Result.TypeTypeToken.Kind = tkInterfaceKeyword) or (Result.TypeTypeToken.Kind = tkRecordKeyword)) then
    begin
      // Parse base classes / interfaces: class(TObject, IInterface)
      if (Current <> nil) and (Current.Kind = tkOpenParen) then
      begin
        Result.BaseListTokens.Add(MatchToken(tkOpenParen)); // (
        while (Current <> nil) and (Current.Kind <> tkCloseParen) and (Current.Kind <> tkEOF) do
          Result.BaseListTokens.Add(NextToken); // Add identifiers and commas
        if (Current <> nil) and (Current.Kind = tkCloseParen) then
          Result.BaseListTokens.Add(MatchToken(tkCloseParen)); // )
      end;
      if (Current <> nil) and (Current.Kind = tkSemicolon) then
      begin
        // Forward declaration, no body
      end
      else
      begin
        ParseClassBody(Result);
      end;
    end
    else
    begin
      // simple type aliases, enums, procedure types, etc. - collect tokens for roundtrip fidelity
      // Track paren nesting so ';' inside '()' doesn't end the type prematurely
      LNestLevel := 0;
      while (Current <> nil) and (Current.Kind <> tkEOF) do
      begin
        if Current.Kind = tkOpenParen then
          Inc(LNestLevel)
        else if Current.Kind = tkCloseParen then
          Dec(LNestLevel)
        else if (Current.Kind = tkSemicolon) and (LNestLevel <= 0) then
          Break;
        Result.TypeExtraTokens.Add(NextToken);
      end;
    end;
  end;
  
  // Collect any remaining tokens before semicolon (track nesting)
  LNestLevel := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then
      Inc(LNestLevel)
    else if Current.Kind = tkCloseParen then
      Dec(LNestLevel)
    else if (Current.Kind = tkSemicolon) and (LNestLevel <= 0) then
      Break;
    Result.TypeExtraTokens.Add(NextToken);
  end;
    
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
    
    while (Current <> nil) and 
          (Current.Kind = tkIdentifier) do
    begin
      LTypeSec.Declarations.Add(ParseTypeDeclaration);
    end;
    
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
  LUnparsed: TUnparsedDeclarationSyntax;
  LNestLevel: Integer;
begin
  Result := nil;
  if (Current = nil) or (Current.Kind <> tkInterfaceKeyword) then
    Exit;
    
  Result := TInterfaceSectionSyntax.Create;
  Result.InterfaceKeyword := MatchToken(tkInterfaceKeyword);
  
  // Look for uses clause right after interface
  if (Current <> nil) and (Current.Kind = tkUsesKeyword) then
    Result.UsesClause := ParseUsesClause();

  LUnparsed := nil;
  LNestLevel := 0; // Track paren nesting
  // Parse interface declarations: type, const, var, and other constructs
  while (Current <> nil) and 
        (Current.Kind <> tkImplementationKeyword) and 
        (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then
      Inc(LNestLevel)
    else if Current.Kind = tkCloseParen then
      Dec(LNestLevel);
      
    if (LNestLevel <= 0) and 
       ((Current.Kind = tkTypeKeyword) or 
        (Current.Kind = tkConstKeyword) or 
        (Current.Kind = tkVarKeyword)) then
    begin
      LUnparsed := nil;
      LDecl := ParseDeclarationSection;
      if Assigned(LDecl) then
        Result.Declarations.Add(LDecl);
    end
    else
    begin
      // Collect unrecognized tokens (e.g. standalone function/procedure declarations)
      // into TUnparsedDeclarationSyntax to preserve them for roundtrip fidelity
      if LUnparsed = nil then
      begin
        LUnparsed := TUnparsedDeclarationSyntax.Create;
        Result.Declarations.Add(LUnparsed);
      end;
      LUnparsed.Tokens.Add(NextToken);
    end;
  end;
end;

function TParseTreeParser.ParseStatement: TStatementSyntax;
var
  LOpaque: TOpaqueStatementSyntax;
  LNest: Integer;
begin
  if Current = nil then Exit(nil);

  if Current.Kind = tkWhileKeyword then
    Exit(ParseWhileStatement)
  else if Current.Kind = tkRepeatKeyword then
    Exit(ParseRepeatStatement)
  else if Current.Kind = tkForKeyword then
    Exit(ParseForStatement)
  else if Current.Kind = tkIfKeyword then
    Exit(ParseIfStatement);

  // Fallback: collect until semicolon or block end
  LOpaque := TOpaqueStatementSyntax.Create;
  LNest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if (Current.Kind = tkBeginKeyword) or (Current.Kind = tkTryKeyword) or (Current.Kind = tkCaseKeyword) or (Current.Kind = tkAsmKeyword) then
      Inc(LNest)
    else if Current.Kind = tkEndKeyword then
    begin
      if LNest = 0 then Break;
      Dec(LNest);
    end
    else if (Current.Kind = tkSemicolon) and (LNest = 0) then
    begin
      LOpaque.Tokens.Add(NextToken);
      Break;
    end;
    
    // Safety: if we hit another keyword that starts a statement, break if we have ANY tokens
    if (LNest = 0) and (LOpaque.Tokens.Count > 0) and 
       ((Current.Kind = tkWhileKeyword) or (Current.Kind = tkForKeyword) or 
        (Current.Kind = tkRepeatKeyword) or (Current.Kind = tkIfKeyword) or
        (Current.Kind = tkElseKeyword)) then
      Break;

    LOpaque.Tokens.Add(NextToken);
  end;
  Result := LOpaque;
end;

function TParseTreeParser.ParseWhileStatement: TWhileStatementSyntax;
begin
  Result := TWhileStatementSyntax.Create;
  Result.WhileKeyword := MatchToken(tkWhileKeyword);
  
  while (Current <> nil) and (Current.Kind <> tkDoKeyword) and (Current.Kind <> tkEOF) do
    Result.ConditionTokens.Add(NextToken);
    
  if (Current <> nil) and (Current.Kind = tkDoKeyword) then
    Result.DoKeyword := MatchToken(tkDoKeyword);
    
  Result.Statement := ParseStatement;
end;

function TParseTreeParser.ParseRepeatStatement: TRepeatStatementSyntax;
begin
  Result := TRepeatStatementSyntax.Create;
  Result.RepeatKeyword := MatchToken(tkRepeatKeyword);
  
  while (Current <> nil) and (Current.Kind <> tkUntilKeyword) and (Current.Kind <> tkEOF) do
    Result.Statements.Add(ParseStatement);
    
  if (Current <> nil) and (Current.Kind = tkUntilKeyword) then
    Result.UntilKeyword := MatchToken(tkUntilKeyword);
    
  while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
    Result.ConditionTokens.Add(NextToken);
    
  if (Current <> nil) and (Current.Kind = tkSemicolon) then
    Result.ConditionTokens.Add(MatchToken(tkSemicolon));
end;

function TParseTreeParser.ParseForStatement: TForStatementSyntax;
begin
  Result := TForStatementSyntax.Create;
  Result.ForKeyword := MatchToken(tkForKeyword);
  
  while (Current <> nil) and (Current.Kind <> tkEquals) and (Current.Kind <> tkEOF) do
    Result.VariableTokens.Add(NextToken);
    
  if (Current <> nil) and (Current.Kind = tkEquals) then
    Result.AssignmentToken := MatchToken(tkEquals);
    
  while (Current <> nil) and (Current.Kind <> tkToKeyword) and (Current.Kind <> tkDowntoKeyword) and (Current.Kind <> tkEOF) do
    Result.StartTokens.Add(NextToken);
    
  if (Current <> nil) and ((Current.Kind = tkToKeyword) or (Current.Kind = tkDowntoKeyword)) then
    Result.ToDowntoKeyword := NextToken;
    
  while (Current <> nil) and (Current.Kind <> tkDoKeyword) and (Current.Kind <> tkEOF) do
    Result.EndTokens.Add(NextToken);
    
  if (Current <> nil) and (Current.Kind = tkDoKeyword) then
    Result.DoKeyword := MatchToken(tkDoKeyword);
    
  Result.Statement := ParseStatement;
end;

function TParseTreeParser.ParseIfStatement: TIfStatementSyntax;
begin
  Result := TIfStatementSyntax.Create;
  Result.IfKeyword := MatchToken(tkIfKeyword);
  
  while (Current <> nil) and (Current.Kind <> tkThenKeyword) and (Current.Kind <> tkEOF) do
    Result.ConditionTokens.Add(NextToken);
    
  if (Current <> nil) and (Current.Kind = tkThenKeyword) then
    Result.ThenKeyword := MatchToken(tkThenKeyword);
    
  Result.ThenStatement := ParseStatement;
  
  // if ThenStatement was an OpaqueStatement ending in a semicolon, 
  // and the next token is ELSE, then the semicolon was actually part of the THEN branch
  // BUT in Delphi, a semicolon before ELSE is technically a syntax error for the IF.
  // HOWEVER, our ParseStatement might consume it.
  
  if (Current <> nil) and (Current.Kind = tkElseKeyword) then
  begin
    Result.ElseKeyword := MatchToken(tkElseKeyword);
    Result.ElseStatement := ParseStatement;
  end;
end;

function TParseTreeParser.ParseMethodImplementation(const AFullSource: string): TMethodImplementationSyntax;
var
  LNestLevel: Integer;
  LDecl: TDeclarationSectionSyntax;
  LUnparsed: TUnparsedDeclarationSyntax;
begin
  // If source is provided, we need to tokenize it first (used by tests)
  if AFullSource <> '' then
  begin
    var LLexer := TParseTreeLexer.Create(AFullSource);
    try
      FTokens := LLexer.TokenizeAll;
      FPosition := 0;
    finally
      LLexer.Free;
    end;
  end;

  Result := TMethodImplementationSyntax.Create;
  LUnparsed := nil;
  
  if (Current <> nil) and (Current.Kind = tkClassKeyword) then
  begin
    Result.ClassKeyword := NextToken; // consume class
  end;
  
  Result.MethodTypeKeyword := NextToken; // procedure, function, etc.
  
  // consume signature until semicolon
  LNestLevel := 0; // Using LNestLevel to track parentheses
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then Inc(LNestLevel)
    else if Current.Kind = tkCloseParen then Dec(LNestLevel);
    
    if (Current.Kind = tkSemicolon) and (LNestLevel <= 0) then
      Break;
      
    Result.SignatureTokens.Add(NextToken);
  end;
    
  if (Current <> nil) and (Current.Kind = tkSemicolon) then
    Result.SignatureSemicolon := MatchToken(tkSemicolon);
    
  // Local declarations (var, const, type, nested procedures/functions)
  while (Current <> nil) and (Current.Kind <> tkBeginKeyword) and (Current.Kind <> tkAsmKeyword) and (Current.Kind <> tkEOF) do
  begin
    if (Current.Kind = tkVarKeyword) or (Current.Kind = tkConstKeyword) or (Current.Kind = tkTypeKeyword) then
    begin
      LDecl := ParseDeclarationSection;
      if Assigned(LDecl) then
        Result.LocalDeclarations.Add(LDecl);
    end
    else if (Current.Kind = tkProcedureKeyword) or (Current.Kind = tkFunctionKeyword) then
    begin
      // Nested procedure/function - parse recursively
      Result.LocalDeclarations.Add(ParseMethodImplementation);
    end
    else
    begin
      if LUnparsed = nil then
      begin
        LUnparsed := TUnparsedDeclarationSyntax.Create;
        Result.LocalDeclarations.Add(LUnparsed);
      end;
      LUnparsed.Tokens.Add(NextToken);
      
      // Stop if it's external or forward since it won't have a body
      if (LUnparsed.Tokens.Last.Kind = tkSemicolon) then
      begin
        var lident := False;
        for var k := 0 to LUnparsed.Tokens.Count - 1 do
        begin
          if SameText(LUnparsed.Tokens[k].Text, 'external') or SameText(LUnparsed.Tokens[k].Text, 'forward') then
          begin
            lident := True;
            Break;
          end;
        end;
        if lident then Break;
      end;
    end;
  end;
  
  if (Current <> nil) and ((Current.Kind = tkBeginKeyword) or (Current.Kind = tkAsmKeyword)) then
  begin
    Result.BeginKeyword := NextToken;
    LNestLevel := 1;
  end
  else
    LNestLevel := 0;
    
  // Body until matched end
  while (Current <> nil) and (LNestLevel > 0) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkEndKeyword then
    begin
      Dec(LNestLevel);
      if LNestLevel = 0 then
      begin
        Result.EndKeyword := MatchToken(tkEndKeyword);
        Break;
      end;
      // Fall through to parse regular statement if end is nested
    end;

    if (Current.Kind = tkBeginKeyword) or (Current.Kind = tkTryKeyword) or 
       (Current.Kind = tkCaseKeyword) or (Current.Kind = tkAsmKeyword) then
    begin
       // We keep treating nested blocks as Opaque for now in ParseStatement fallback, 
       // but we increment LNestLevel here if we don't call ParseStatement for blocks.
       // Actually, let's call ParseStatement and it will handle LNest internal to the opaque if needed.
    end;

    Result.Statements.Add(ParseStatement);
  end;
  
  if (Current <> nil) and (Current.Kind = tkSemicolon) then
    Result.FinalSemicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseImplementationSection: TImplementationSectionSyntax;
var
  LUnparsed: TUnparsedDeclarationSyntax;
  
  function IsMethodStartToken(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (AToken.Kind = tkProcedureKeyword) or
      (AToken.Kind = tkFunctionKeyword) or
      (AToken.Kind = tkConstructorKeyword) or
      (AToken.Kind = tkDestructorKeyword) or
      ((AToken.Kind = tkClassKeyword) and (Peek(1) <> nil) and 
       ((Peek(1).Kind = tkProcedureKeyword) or (Peek(1).Kind = tkFunctionKeyword))));
  end;

begin
  Result := TImplementationSectionSyntax.Create;
  Result.ImplementationKeyword := MatchToken(tkImplementationKeyword);
  
  if (Current <> nil) and (Current.Kind = tkUsesKeyword) then
  begin
    Result.UsesClause := ParseUsesClause();
  end;
  
  LUnparsed := nil;
  // Parse methods and other declarations
  while (Current <> nil) and 
        not ((Current.Kind = tkEndKeyword) and (Peek(1) <> nil) and (Peek(1).Kind = tkDot)) and 
        (Current.Kind <> tkEOF) do
  begin
    if IsMethodStartToken(Current) then
    begin
      LUnparsed := nil; // Reset unparsed stream
      Result.Declarations.Add(ParseMethodImplementation);
    end
    else if (Current.Kind = tkVarKeyword) or (Current.Kind = tkConstKeyword) or (Current.Kind = tkTypeKeyword) then
    begin
       LUnparsed := nil;
       Result.Declarations.Add(ParseDeclarationSection);
    end
    else
    begin
      if LUnparsed = nil then
      begin
        LUnparsed := TUnparsedDeclarationSyntax.Create;
        Result.Declarations.Add(LUnparsed); // Add instantly to maintain order
      end;
      LUnparsed.Tokens.Add(NextToken);
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
        Result.Namespaces.Add(MatchToken(tkIdentifier));
        while (Current <> nil) and (Current.Kind = tkDot) do
        begin
          Result.Dots.Add(MatchToken(tkDot));
          Result.Namespaces.Add(MatchToken(tkIdentifier));
        end;
        Result.Semicolon := MatchToken(tkSemicolon);
      end;
      
      var LUnp: TUnparsedDeclarationSyntax := nil;
      while (Current <> nil) and (Current.Kind <> tkInterfaceKeyword) and (Current.Kind <> tkEOF) do
      begin
        if LUnp = nil then
        begin
          LUnp := TUnparsedDeclarationSyntax.Create;
          Result.PreInterfaceDeclarations.Add(LUnp);
        end;
        LUnp.Tokens.Add(NextToken);
      end;
        
      if (Current <> nil) and (Current.Kind = tkInterfaceKeyword) then
        Result.InterfaceSection := ParseInterfaceSection();
        
      LUnp := nil;
      while (Current <> nil) and (Current.Kind <> tkImplementationKeyword) and (Current.Kind <> tkEOF) do
      begin
        if LUnp = nil then
        begin
          LUnp := TUnparsedDeclarationSyntax.Create;
          Result.IntfImplDeclarations.Add(LUnp);
        end;
        LUnp.Tokens.Add(NextToken);
      end;
        
      if (Current <> nil) and (Current.Kind = tkImplementationKeyword) then
        Result.ImplementationSection := ParseImplementationSection();
        
      LUnp := nil;
      while (Current <> nil) and 
            not ((Current.Kind = tkEndKeyword) and (Peek(1) <> nil) and (Peek(1).Kind = tkDot)) and 
            (Current.Kind <> tkEOF) do
      begin
        if LUnp = nil then
        begin
          LUnp := TUnparsedDeclarationSyntax.Create;
          Result.PostImplementationDeclarations.Add(LUnp);
        end;
        LUnp.Tokens.Add(NextToken);
      end;
        
      if (Current <> nil) and (Current.Kind = tkEndKeyword) then
      begin
        Result.FinalEndKeyword := MatchToken(tkEndKeyword);
        if (Current <> nil) and (Current.Kind = tkDot) then
          Result.FinalDotToken := MatchToken(tkDot);
      end;
          
      LUnp := nil;
      while (Current <> nil) and (Current.Kind <> tkEOF) do
      begin
        if LUnp = nil then
        begin
          LUnp := TUnparsedDeclarationSyntax.Create;
          Result.PostImplementationDeclarations.Add(LUnp);
        end;
        LUnp.Tokens.Add(NextToken);
      end;

      if (Current <> nil) and (Current.Kind = tkEOF) then
        Result.EndOfFileToken := MatchToken(tkEOF);
      
    finally
      FTokens.Free;
      FTokens := nil;
    end;
  finally
    LLexer.Free;
  end;
end;

end.
