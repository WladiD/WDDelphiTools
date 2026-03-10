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
    function ParseAssignmentStatement: TAssignmentStatementSyntax;
    function ParseBeginEndStatement: TBeginEndStatementSyntax;
    function ParseTryStatement: TTryStatementSyntax;
    function ParseRaiseStatement: TRaiseStatementSyntax;
    function ParseProcedureCallStatement: TProcedureCallStatementSyntax;
    function ParseCaseStatement: TCaseStatementSyntax;
    function ParseWithStatement: TWithStatementSyntax;
    function ParseInheritedStatement: TInheritedStatementSyntax;
    function ParseExitStatement: TExitStatementSyntax;
    function ParseInlineVarStatement: TInlineVarStatementSyntax;
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
    var LExpectDot: Boolean := False;
    while (Current <> nil) and 
          ((Current.Kind = tkIdentifier) or (Current.Kind = tkDot) or 
           (Current.Kind = tkUnitKeyword) or (Current.Kind = tkInterfaceKeyword) or
           (Current.Kind = tkImplementationKeyword) or (Current.Kind = tkUsesKeyword)) do
    begin
      if Current.Kind = tkDot then
      begin
        if not LExpectDot then Break;
        LUnitRef.Dots.Add(NextToken);
        LExpectDot := False;
      end
      else
      begin
        if LExpectDot then Break;
        LUnitRef.Namespaces.Add(NextToken);
        LExpectDot := True;
      end;
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
var
  LNestLevel: Integer;
  LTypeTokens: TList<TSyntaxToken>;
  LHasComplexType: Boolean;
  LFallbackOpaqueConst: Boolean;
begin
  Result := TConstDeclarationSyntax.Create;
  Result.Identifier := MatchToken(tkIdentifier);
  LFallbackOpaqueConst := False;
  
  if (Current <> nil) and (Current.Kind = tkColon) then
  begin
    Result.ColonToken := MatchToken(tkColon);
    LTypeTokens := TList<TSyntaxToken>.Create;
    try
      LNestLevel := 0;
      while (Current <> nil) and (Current.Kind <> tkEOF) do
      begin
        if Current.Kind in [tkOpenParen, tkOpenBracket, tkLessThan] then
          Inc(LNestLevel)
        else if Current.Kind in [tkCloseParen, tkCloseBracket, tkGreaterThan] then
          Dec(LNestLevel)
        else if (Current.Kind = tkEquals) and (LNestLevel <= 0) then
          Break
        else if (Current.Kind = tkSemicolon) and (LNestLevel <= 0) then
          Break;

        LTypeTokens.Add(NextToken);
      end;

      LHasComplexType := (LTypeTokens.Count <> 1) or (LTypeTokens[0].Kind <> tkIdentifier);
      if (LTypeTokens.Count > 0) and not LHasComplexType then
        Result.TypeIdentifier := LTypeTokens[0]
      else
      begin
        // Fallback for complex typed constants: preserve the entire right stream opaquely.
        LFallbackOpaqueConst := True;
        if Result.ColonToken <> nil then
        begin
          Result.ValueTokens.Add(Result.ColonToken);
          Result.ColonToken := nil;
        end;
        for LNestLevel := 0 to LTypeTokens.Count - 1 do
          Result.ValueTokens.Add(LTypeTokens[LNestLevel]);
      end;
    finally
      LTypeTokens.Free;
    end;
  end;
  
  if (Current <> nil) and (Current.Kind = tkEquals) then
  begin
    if LFallbackOpaqueConst then
      Result.ValueTokens.Add(MatchToken(tkEquals))
    else
      Result.EqualsToken := MatchToken(tkEquals);
    // Read value tokens until semicolon (respect nested parentheses/brackets)
    LNestLevel := 0;
    while (Current <> nil) and (Current.Kind <> tkEOF) do
    begin
      if (Current.Kind = tkOpenParen) or (Current.Kind = tkOpenBracket) then
        Inc(LNestLevel)
      else if (Current.Kind = tkCloseParen) or (Current.Kind = tkCloseBracket) then
        Dec(LNestLevel)
      else if (Current.Kind = tkSemicolon) and (LNestLevel <= 0) then
        Break;
      Result.ValueTokens.Add(NextToken);
    end;
  end;
    
  Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseVarDeclaration: TVarDeclarationSyntax;
var
  LNestLevel: Integer;

  function IsVarTypeModifierStart(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (AToken.Kind = tkOverrideKeyword) or
      ((AToken.Kind = tkIdentifier) and (
        SameText(AToken.Text, 'dispid') or
        SameText(AToken.Text, 'message') or
        SameText(AToken.Text, 'deprecated') or
        SameText(AToken.Text, 'platform') or
        SameText(AToken.Text, 'library') or
        SameText(AToken.Text, 'experimental') or
        SameText(AToken.Text, 'unimplemented') or
        SameText(AToken.Text, 'overload') or
        SameText(AToken.Text, 'override') or
        SameText(AToken.Text, 'reintroduce') or
        SameText(AToken.Text, 'virtual') or
        SameText(AToken.Text, 'dynamic') or
        SameText(AToken.Text, 'abstract') or
        SameText(AToken.Text, 'final') or
        SameText(AToken.Text, 'static') or
        SameText(AToken.Text, 'inline') or
        SameText(AToken.Text, 'assembler') or
        SameText(AToken.Text, 'cdecl') or
        SameText(AToken.Text, 'pascal') or
        SameText(AToken.Text, 'register') or
        SameText(AToken.Text, 'stdcall') or
        SameText(AToken.Text, 'safecall') or
        SameText(AToken.Text, 'varargs')
      )));
  end;
begin
  Result := TVarDeclarationSyntax.Create;
  Result.Identifier := MatchToken(tkIdentifier);
  
  if (Current <> nil) and (Current.Kind = tkColon) then
  begin
    Result.ColonToken := MatchToken(tkColon);
    if (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) then
      Result.TypeIdentifier := NextToken;
  end;
  
  // Collect remaining type tokens (e.g. function pointer params, generics, directives)
  LNestLevel := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if (Current.Kind = tkOpenParen) or (Current.Kind = tkOpenBracket) or (Current.Kind = tkLessThan) then
      Inc(LNestLevel)
    else if (Current.Kind = tkCloseParen) or (Current.Kind = tkCloseBracket) or (Current.Kind = tkGreaterThan) then
      Dec(LNestLevel)
    else if (Current.Kind = tkSemicolon) and (LNestLevel <= 0) then
    begin
      if IsVarTypeModifierStart(Peek(1)) then
      begin
        Result.TypeExtraTokens.Add(NextToken); // keep this semicolon inside type
        Continue;
      end;
      Break;
    end;
    Result.TypeExtraTokens.Add(NextToken);
  end;
    
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

  function IsTypeModifierStart(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (AToken.Kind = tkOverrideKeyword) or
      ((AToken.Kind = tkIdentifier) and (
        SameText(AToken.Text, 'dispid') or
        SameText(AToken.Text, 'message') or
        SameText(AToken.Text, 'deprecated') or
        SameText(AToken.Text, 'platform') or
        SameText(AToken.Text, 'library') or
        SameText(AToken.Text, 'experimental') or
        SameText(AToken.Text, 'unimplemented') or
        SameText(AToken.Text, 'overload') or
        SameText(AToken.Text, 'override') or
        SameText(AToken.Text, 'reintroduce') or
        SameText(AToken.Text, 'virtual') or
        SameText(AToken.Text, 'dynamic') or
        SameText(AToken.Text, 'abstract') or
        SameText(AToken.Text, 'final') or
        SameText(AToken.Text, 'static') or
        SameText(AToken.Text, 'inline') or
        SameText(AToken.Text, 'assembler') or
        SameText(AToken.Text, 'cdecl') or
        SameText(AToken.Text, 'pascal') or
        SameText(AToken.Text, 'register') or
        SameText(AToken.Text, 'stdcall') or
        SameText(AToken.Text, 'safecall') or
        SameText(AToken.Text, 'varargs')
      )));
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
    LDeclBlockNest: Integer;
    LPrevKind: TTokenKind;
    LScan: Integer;
    LHasColon: Boolean;
    LModifierSemicolon: Boolean;
    LCanHaveModifiers: Boolean;
    LIsModifierStart: Boolean;

    function IsForwardClassLikeDecl: Boolean;
    var
      LIdx: Integer;
      LParenNest: Integer;
    begin
      LIdx := FPosition + 1; // token right after class/interface/dispinterface
      LParenNest := 0;

      // Skip optional base list: class(...), interface(...)
      if (LIdx < FTokens.Count) and (FTokens[LIdx].Kind = tkOpenParen) then
      begin
        Inc(LParenNest);
        Inc(LIdx);
        while (LIdx < FTokens.Count) and (FTokens[LIdx].Kind <> tkEOF) do
        begin
          if FTokens[LIdx].Kind = tkOpenParen then
            Inc(LParenNest)
          else if FTokens[LIdx].Kind = tkCloseParen then
          begin
            Dec(LParenNest);
            if LParenNest = 0 then
            begin
              Inc(LIdx); // move past ')'
              Break;
            end;
          end;
          Inc(LIdx);
        end;
      end;

      // Forward declarations end directly with ';' after optional base list.
      Result := (LIdx < FTokens.Count) and (FTokens[LIdx].Kind = tkSemicolon);
    end;
  begin
    LMember := TClassMemberSyntax.Create;
    LNestLevel := 0;
    LDeclBlockNest := 0;
    LPrevKind := tkUnknown;
    
    // Parse tokens until semicolon at nesting level 0
    while (Current <> nil) and (Current.Kind <> tkEOF) do
    begin
      if (Current.Kind = tkOpenParen) or (Current.Kind = tkLessThan) then
        Inc(LNestLevel)
      else if (Current.Kind = tkCloseParen) or (Current.Kind = tkGreaterThan) then
        Dec(LNestLevel)
      // Track nested declaration blocks (e.g. record/case inside member types)
      else if (
        ((Current.Kind = tkClassKeyword) and (LPrevKind in [tkEquals, tkColon]) and
         ((Peek(1) = nil) or (Peek(1).Kind <> tkOfKeyword)) and not IsForwardClassLikeDecl) or
        ((Current.Kind = tkInterfaceKeyword) and (LPrevKind in [tkEquals, tkColon]) and not IsForwardClassLikeDecl) or
        ((Current.Kind = tkDispinterfaceKeyword) and (LPrevKind in [tkEquals, tkColon]) and not IsForwardClassLikeDecl) or
        ((Current.Kind = tkRecordKeyword) and (LPrevKind in [tkEquals, tkColon])) or
        ((Current.Kind = tkCaseKeyword) and (LDeclBlockNest > 0))
      ) then
        Inc(LDeclBlockNest)
      else if (Current.Kind = tkEndKeyword) and (LDeclBlockNest > 0) then
        Dec(LDeclBlockNest);
      
      if (Current.Kind = tkSemicolon) and (LNestLevel <= 0) and (LDeclBlockNest <= 0) then
      begin
        LMember.Tokens.Add(NextToken); // consume semicolon
        
        LCanHaveModifiers := False;
        for LScan := 0 to LMember.Tokens.Count - 1 do
          if LMember.Tokens[LScan].Kind in [tkProcedureKeyword, tkFunctionKeyword, tkConstructorKeyword, tkDestructorKeyword, tkPropertyKeyword] then
          begin
            LCanHaveModifiers := True;
            Break;
          end;

        if LCanHaveModifiers then
        begin
          // Consume trailing modifiers/directives (e.g. override;, stdcall;, dispid X;)
          while (Current <> nil) and (Current.Kind <> tkEOF) do
          begin
            // Hard stop when the next token clearly starts a new declaration/member.
            if IsVisibilityKeyword(Current) or
               ((Current.Kind = tkStrictKeyword) and (Peek(1) <> nil) and IsVisibilityKeyword(Peek(1))) or
               (Current.Kind = tkEndKeyword) or
               (Current.Kind = tkOpenBracket) or
               (Current.Kind = tkProcedureKeyword) or
               (Current.Kind = tkFunctionKeyword) or
               (Current.Kind = tkConstructorKeyword) or
               (Current.Kind = tkDestructorKeyword) or
               (Current.Kind = tkPropertyKeyword) or
               (Current.Kind = tkTypeKeyword) or
               (Current.Kind = tkVarKeyword) or
               (Current.Kind = tkConstKeyword) then
              Break;

            LIsModifierStart :=
              (Current.Kind = tkOverrideKeyword) or
              ((Current.Kind = tkIdentifier) and (
                SameText(Current.Text, 'dispid') or
                SameText(Current.Text, 'message') or
                SameText(Current.Text, 'deprecated') or
                SameText(Current.Text, 'platform') or
                SameText(Current.Text, 'library') or
                SameText(Current.Text, 'experimental') or
                SameText(Current.Text, 'unimplemented') or
                SameText(Current.Text, 'overload') or
                SameText(Current.Text, 'override') or
                SameText(Current.Text, 'reintroduce') or
                SameText(Current.Text, 'virtual') or
                SameText(Current.Text, 'dynamic') or
                SameText(Current.Text, 'abstract') or
                SameText(Current.Text, 'final') or
                SameText(Current.Text, 'static') or
                SameText(Current.Text, 'inline') or
                SameText(Current.Text, 'assembler') or
                SameText(Current.Text, 'cdecl') or
                SameText(Current.Text, 'pascal') or
                SameText(Current.Text, 'register') or
                SameText(Current.Text, 'stdcall') or
                SameText(Current.Text, 'safecall') or
                SameText(Current.Text, 'varargs')
              ));
            if not LIsModifierStart then
              Break;

            // Peek until next ';'. If a ':' appears first, we are likely at a new field member.
            LScan := FPosition;
            LHasColon := False;
            LModifierSemicolon := False;
            while (LScan < FTokens.Count) and (FTokens[LScan].Kind <> tkEOF) do
            begin
              if FTokens[LScan].Kind = tkColon then
                LHasColon := True;
              if FTokens[LScan].Kind = tkSemicolon then
              begin
                LModifierSemicolon := True;
                Break;
              end;
              if (FTokens[LScan].Kind = tkEndKeyword) then
                Break;
              Inc(LScan);
            end;

            if not LModifierSemicolon or LHasColon then
              Break;

            while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
              LMember.Tokens.Add(NextToken);
            if (Current <> nil) and (Current.Kind = tkSemicolon) then
              LMember.Tokens.Add(NextToken)
            else
              Break;
          end;
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
    Result.GenericParameterTokens.Add(NextToken); // consume '<'
    while (Current <> nil) and (Current.Kind <> tkGreaterThan) and (Current.Kind <> tkEOF) do
      Result.GenericParameterTokens.Add(NextToken); // consume type params and commas
    if (Current <> nil) and (Current.Kind = tkGreaterThan) then
      Result.GenericParameterTokens.Add(NextToken); // consume '>'
  end;
  
  if (Current <> nil) and (Current.Kind = tkEquals) then
  begin
    Result.EqualsToken := MatchToken(tkEquals);
    
    // Parse what kind of type it is (like class, interface, record)
    if (Current <> nil) and ((Current.Kind = tkClassKeyword) or (Current.Kind = tkInterfaceKeyword) or (Current.Kind = tkDispinterfaceKeyword) or (Current.Kind = tkRecordKeyword) or (Current.Kind = tkIdentifier)) then
      Result.TypeTypeToken := NextToken;
      
    if (Result.TypeTypeToken <> nil) and
       ((Result.TypeTypeToken.Kind = tkClassKeyword) or (Result.TypeTypeToken.Kind = tkInterfaceKeyword) or (Result.TypeTypeToken.Kind = tkDispinterfaceKeyword) or (Result.TypeTypeToken.Kind = tkRecordKeyword)) and
       not ((Result.TypeTypeToken.Kind = tkClassKeyword) and (Current <> nil) and (Current.Kind = tkOfKeyword)) then
    begin
      // Consume modifiers like 'abstract' or 'sealed' before the base list
      while (Current <> nil) and (Current.Kind = tkIdentifier) and
            (SameText(Current.Text, 'abstract') or SameText(Current.Text, 'sealed')) do
        Result.TypeExtraTokens.Add(NextToken);

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
        begin
          // Procedure/function type aliases may carry extra segments, e.g. "; stdcall;".
          // Keep those segments inside the same type declaration.
          if IsTypeModifierStart(Peek(1)) then
          begin
            Result.TypeExtraTokens.Add(NextToken); // keep this semicolon
            Continue;
          end;
          Break;
        end;
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

  if Current.Kind = tkSemicolon then
  begin
    var LEmpty := TEmptyStatementSyntax.Create;
    LEmpty.Semicolon := MatchToken(tkSemicolon);
    Exit(LEmpty);
  end;

  if Current.Kind = tkWhileKeyword then
    Exit(ParseWhileStatement)
  else if Current.Kind = tkRepeatKeyword then
    Exit(ParseRepeatStatement)
  else if Current.Kind = tkForKeyword then
    Exit(ParseForStatement)
  else if Current.Kind = tkIfKeyword then
    Exit(ParseIfStatement)
  else if Current.Kind = tkBeginKeyword then
    Exit(ParseBeginEndStatement)
  else if Current.Kind = tkTryKeyword then
    Exit(ParseTryStatement)
  else if Current.Kind = tkRaiseKeyword then
    Exit(ParseRaiseStatement)
  else if Current.Kind = tkCaseKeyword then
    Exit(ParseCaseStatement)
  else if Current.Kind = tkWithKeyword then
    Exit(ParseWithStatement)
  else if Current.Kind = tkInheritedKeyword then
    Exit(ParseInheritedStatement)
  else if Current.Kind = tkExitKeyword then
    Exit(ParseExitStatement)
  else if Current.Kind = tkVarKeyword then
    Exit(ParseInlineVarStatement);

  // Check for assignment: look ahead for := before ; or structural keyword
  var LIdx := 0;
  while (Peek(LIdx) <> nil) and (Peek(LIdx).Kind <> tkEOF) and (Peek(LIdx).Kind <> tkSemicolon) do
  begin
    if Peek(LIdx).Kind = tkColonEquals then
      Exit(ParseAssignmentStatement);
    if Peek(LIdx).Kind in [tkBeginKeyword, tkEndKeyword, tkTryKeyword, tkCaseKeyword,
       tkAsmKeyword, tkIfKeyword, tkWhileKeyword, tkForKeyword, tkRepeatKeyword,
       tkElseKeyword, tkUntilKeyword, tkThenKeyword, tkDoKeyword,
       tkFinallyKeyword, tkExceptKeyword, tkRaiseKeyword, tkOfKeyword,
       tkWithKeyword] then
      Break;
    Inc(LIdx);
  end;

  if (Current <> nil) and (Current.Kind in [tkIdentifier, tkWriteKeyword, tkReadKeyword,
     tkOpenParen]) then
    Exit(ParseProcedureCallStatement);

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
    else if ((Current.Kind = tkFinallyKeyword) or (Current.Kind = tkExceptKeyword)) and (LNest = 0) and (LOpaque.Tokens.Count > 0) then
      Break
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
  begin
    if Current.Kind in [tkEndKeyword, tkFinallyKeyword, tkExceptKeyword] then
      Break;
    Result.Statements.Add(ParseStatement);
  end;

  if (Current <> nil) and (Current.Kind = tkUntilKeyword) then
    Result.UntilKeyword := MatchToken(tkUntilKeyword);

  while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind in [tkEndKeyword, tkFinallyKeyword, tkExceptKeyword, tkElseKeyword] then
      Break;
    Result.ConditionTokens.Add(NextToken);
  end;

  if (Current <> nil) and (Current.Kind = tkSemicolon) then
    Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseForStatement: TForStatementSyntax;
begin
  Result := TForStatementSyntax.Create;
  Result.ForKeyword := MatchToken(tkForKeyword);

  while (Current <> nil) and (Current.Kind <> tkColonEquals) and
        (Current.Kind <> tkInKeyword) and (Current.Kind <> tkEOF) do
    Result.VariableTokens.Add(NextToken);

  if (Current <> nil) and (Current.Kind = tkInKeyword) then
  begin
    Result.InKeyword := MatchToken(tkInKeyword);

    while (Current <> nil) and (Current.Kind <> tkDoKeyword) and (Current.Kind <> tkEOF) do
      Result.CollectionTokens.Add(NextToken);
  end
  else
  begin
    if (Current <> nil) and (Current.Kind = tkColonEquals) then
      Result.AssignmentToken := MatchToken(tkColonEquals);

    while (Current <> nil) and (Current.Kind <> tkToKeyword) and
          (Current.Kind <> tkDowntoKeyword) and (Current.Kind <> tkEOF) do
      Result.StartTokens.Add(NextToken);

    if (Current <> nil) and ((Current.Kind = tkToKeyword) or (Current.Kind = tkDowntoKeyword)) then
      Result.ToDowntoKeyword := NextToken;

    while (Current <> nil) and (Current.Kind <> tkDoKeyword) and (Current.Kind <> tkEOF) do
      Result.EndTokens.Add(NextToken);
  end;

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

function TParseTreeParser.ParseAssignmentStatement: TAssignmentStatementSyntax;
begin
  Result := TAssignmentStatementSyntax.Create;
  while (Current <> nil) and (Current.Kind <> tkColonEquals) and (Current.Kind <> tkEOF) do
    Result.LeftTokens.Add(NextToken);
    
  if (Current <> nil) and (Current.Kind = tkColonEquals) then
    Result.ColonEqualsToken := MatchToken(tkColonEquals);
    
  var LNest := 0;
  var LBlockNest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if (Current.Kind = tkOpenParen) or (Current.Kind = tkOpenBracket) then Inc(LNest)
    else if (Current.Kind = tkCloseParen) or (Current.Kind = tkCloseBracket) then Dec(LNest);

    if (Current.Kind = tkBeginKeyword) or (Current.Kind = tkTryKeyword) or
       (Current.Kind = tkCaseKeyword) or (Current.Kind = tkAsmKeyword) then
      Inc(LBlockNest)
    else if Current.Kind = tkEndKeyword then
    begin
      if LBlockNest = 0 then Break;
      Dec(LBlockNest);
    end
    else if ((Current.Kind = tkFinallyKeyword) or (Current.Kind = tkExceptKeyword)) and
            (LNest = 0) and (LBlockNest = 0) and (Result.RightTokens.Count > 0) then
      Break;

    if (Current.Kind = tkSemicolon) and (LNest = 0) and (LBlockNest = 0) then
    begin
      Result.RightTokens.Add(MatchToken(tkSemicolon));
      Break;
    end;

    // Break if we hit a keyword that definitely starts a new statement
    if (LNest = 0) and (LBlockNest = 0) and (Result.RightTokens.Count > 0) and
       ((Current.Kind = tkIfKeyword) or (Current.Kind = tkWhileKeyword) or (Current.Kind = tkForKeyword) or 
        (Current.Kind = tkRepeatKeyword) or (Current.Kind = tkElseKeyword)) then
      Break;

    Result.RightTokens.Add(NextToken);
  end;
end;

function TParseTreeParser.ParseBeginEndStatement: TBeginEndStatementSyntax;
begin
  Result := TBeginEndStatementSyntax.Create;
  Result.BeginKeyword := MatchToken(tkBeginKeyword);
  
  while (Current <> nil) and (Current.Kind <> tkEndKeyword) and (Current.Kind <> tkEOF) do
    Result.Statements.Add(ParseStatement);
    
  if (Current <> nil) and (Current.Kind = tkEndKeyword) then
    Result.EndKeyword := MatchToken(tkEndKeyword);

  if (Current <> nil) and (Current.Kind = tkSemicolon) then
    Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseTryStatement: TTryStatementSyntax;
begin
  Result := TTryStatementSyntax.Create;
  Result.TryKeyword := MatchToken(tkTryKeyword);

  while (Current <> nil) and (Current.Kind <> tkFinallyKeyword) and (Current.Kind <> tkExceptKeyword)
        and (Current.Kind <> tkEndKeyword) and (Current.Kind <> tkEOF) do
    Result.Statements.Add(ParseStatement);

  if (Current <> nil) and (Current.Kind = tkFinallyKeyword) then
    Result.FinallyKeyword := MatchToken(tkFinallyKeyword)
  else if (Current <> nil) and (Current.Kind = tkExceptKeyword) then
    Result.ExceptKeyword := MatchToken(tkExceptKeyword);

  while (Current <> nil) and (Current.Kind <> tkEndKeyword) and (Current.Kind <> tkEOF) do
    Result.FinallyExceptStatements.Add(ParseStatement);

  if (Current <> nil) and (Current.Kind = tkEndKeyword) then
    Result.EndKeyword := MatchToken(tkEndKeyword);

  if (Current <> nil) and (Current.Kind = tkSemicolon) then
    Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseRaiseStatement: TRaiseStatementSyntax;
var
  LNest: Integer;
begin
  Result := TRaiseStatementSyntax.Create;
  Result.RaiseKeyword := MatchToken(tkRaiseKeyword);

  LNest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then
      Inc(LNest)
    else if Current.Kind = tkCloseParen then
      Dec(LNest);

    if (Current.Kind = tkSemicolon) and (LNest <= 0) then
    begin
      Result.Semicolon := MatchToken(tkSemicolon);
      Break;
    end;

    if (LNest <= 0) and (Current.Kind in [tkEndKeyword, tkElseKeyword,
       tkFinallyKeyword, tkExceptKeyword]) then
      Break;

    Result.ExpressionTokens.Add(NextToken);
  end;
end;

function TParseTreeParser.ParseProcedureCallStatement: TProcedureCallStatementSyntax;
var
  LNest: Integer;
  LDelimiterNest: Integer;
  LPreviousIsDot: Boolean;
begin
  Result := TProcedureCallStatementSyntax.Create;
  LNest := 0;
  LDelimiterNest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if (Current.Kind = tkOpenParen) or (Current.Kind = tkOpenBracket) then
      Inc(LDelimiterNest)
    else if (Current.Kind = tkCloseParen) or (Current.Kind = tkCloseBracket) then
    begin
      if LDelimiterNest > 0 then
        Dec(LDelimiterNest);
    end
    else if (Current.Kind = tkBeginKeyword) or (Current.Kind = tkTryKeyword) or
       (Current.Kind = tkCaseKeyword) or (Current.Kind = tkAsmKeyword) then
      Inc(LNest)
    else if Current.Kind = tkEndKeyword then
    begin
      if LNest = 0 then Break;
      Dec(LNest);
    end
    else if ((Current.Kind = tkFinallyKeyword) or (Current.Kind = tkExceptKeyword)) and
            (LNest = 0) and (LDelimiterNest = 0) and (Result.ExpressionTokens.Count > 0) then
      Break
    else if (Current.Kind = tkSemicolon) and (LNest = 0) and (LDelimiterNest = 0) then
    begin
      Result.Semicolon := MatchToken(tkSemicolon);
      Break;
    end;

    LPreviousIsDot := (Result.ExpressionTokens.Count > 0) and
      (Result.ExpressionTokens.Last.Kind = tkDot);
    if (LNest = 0) and (LDelimiterNest = 0) and (Result.ExpressionTokens.Count > 0) and
       not LPreviousIsDot and
       ((Current.Kind = tkWhileKeyword) or (Current.Kind = tkForKeyword) or
        (Current.Kind = tkRepeatKeyword) or (Current.Kind = tkIfKeyword) or
        (Current.Kind = tkElseKeyword) or (Current.Kind = tkCaseKeyword) or
        (Current.Kind = tkWithKeyword)) then
      Break;

    Result.ExpressionTokens.Add(NextToken);
  end;
end;

function TParseTreeParser.ParseCaseStatement: TCaseStatementSyntax;
var
  LItem: TCaseItemSyntax;
  LNest: Integer;
begin
  Result := TCaseStatementSyntax.Create;
  Result.CaseKeyword := MatchToken(tkCaseKeyword);

  while (Current <> nil) and (Current.Kind <> tkOfKeyword) and (Current.Kind <> tkEOF) do
    Result.ExpressionTokens.Add(NextToken);

  if (Current <> nil) and (Current.Kind = tkOfKeyword) then
    Result.OfKeyword := MatchToken(tkOfKeyword);

  while (Current <> nil) and (Current.Kind <> tkEndKeyword) and
        (Current.Kind <> tkElseKeyword) and (Current.Kind <> tkEOF) do
  begin
    LItem := TCaseItemSyntax.Create;

    LNest := 0;
    while (Current <> nil) and (Current.Kind <> tkEOF) do
    begin
      if Current.Kind = tkOpenParen then
        Inc(LNest)
      else if Current.Kind = tkCloseParen then
        Dec(LNest);

      if (Current.Kind = tkColon) and (LNest <= 0) then
      begin
        LItem.ColonToken := MatchToken(tkColon);
        Break;
      end;

      if (Current.Kind = tkEndKeyword) or (Current.Kind = tkElseKeyword) then
        Break;

      LItem.ValueTokens.Add(NextToken);
    end;

    if Assigned(LItem.ColonToken) then
      LItem.Statement := ParseStatement;

    Result.CaseItems.Add(LItem);
  end;

  if (Current <> nil) and (Current.Kind = tkElseKeyword) then
  begin
    Result.ElseKeyword := MatchToken(tkElseKeyword);
    while (Current <> nil) and (Current.Kind <> tkEndKeyword) and (Current.Kind <> tkEOF) do
      Result.ElseStatements.Add(ParseStatement);
  end;

  if (Current <> nil) and (Current.Kind = tkEndKeyword) then
    Result.EndKeyword := MatchToken(tkEndKeyword);

  if (Current <> nil) and (Current.Kind = tkSemicolon) then
    Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseWithStatement: TWithStatementSyntax;
begin
  Result := TWithStatementSyntax.Create;
  Result.WithKeyword := MatchToken(tkWithKeyword);

  while (Current <> nil) and (Current.Kind <> tkDoKeyword) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind in [tkEndKeyword, tkElseKeyword, tkFinallyKeyword, tkExceptKeyword] then
      Break;
    Result.ExpressionTokens.Add(NextToken);
  end;

  if (Current <> nil) and (Current.Kind = tkDoKeyword) then
    Result.DoKeyword := MatchToken(tkDoKeyword);

  if (Current <> nil) and not (Current.Kind in [tkEndKeyword, tkElseKeyword,
     tkFinallyKeyword, tkExceptKeyword, tkEOF]) then
    Result.Statement := ParseStatement;
end;

function TParseTreeParser.ParseInheritedStatement: TInheritedStatementSyntax;
var
  LNest: Integer;
begin
  Result := TInheritedStatementSyntax.Create;
  Result.InheritedKeyword := MatchToken(tkInheritedKeyword);

  LNest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then
      Inc(LNest)
    else if Current.Kind = tkCloseParen then
      Dec(LNest);

    if (LNest = 0) and (Current.Kind = tkSemicolon) then
    begin
      Result.Semicolon := MatchToken(tkSemicolon);
      Break;
    end;

    if (LNest = 0) and (Current.Kind in [tkEndKeyword, tkElseKeyword,
       tkFinallyKeyword, tkExceptKeyword]) then
      Break;

    Result.CallTokens.Add(NextToken);
  end;
end;

function TParseTreeParser.ParseExitStatement: TExitStatementSyntax;
var
  LNest: Integer;
begin
  Result := TExitStatementSyntax.Create;
  Result.ExitKeyword := MatchToken(tkExitKeyword);

  LNest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then
      Inc(LNest)
    else if Current.Kind = tkCloseParen then
      Dec(LNest);

    if (LNest = 0) and (Current.Kind = tkSemicolon) then
    begin
      Result.Semicolon := MatchToken(tkSemicolon);
      Break;
    end;

    if (LNest = 0) and (Current.Kind in [tkEndKeyword, tkElseKeyword,
       tkFinallyKeyword, tkExceptKeyword]) then
      Break;

    Result.ExpressionTokens.Add(NextToken);
  end;
end;

function TParseTreeParser.ParseInlineVarStatement: TInlineVarStatementSyntax;
begin
  Result := TInlineVarStatementSyntax.Create;
  Result.VarKeyword := MatchToken(tkVarKeyword);

  var LDelimiterNest := 0;
  var LBlockNest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if (Current.Kind = tkOpenParen) or (Current.Kind = tkOpenBracket) then
      Inc(LDelimiterNest)
    else if (Current.Kind = tkCloseParen) or (Current.Kind = tkCloseBracket) then
    begin
      if LDelimiterNest > 0 then
        Dec(LDelimiterNest);
    end;

    if (Current.Kind = tkBeginKeyword) or (Current.Kind = tkTryKeyword) or
       (Current.Kind = tkCaseKeyword) or (Current.Kind = tkAsmKeyword) then
      Inc(LBlockNest)
    else if Current.Kind = tkEndKeyword then
    begin
      if LBlockNest = 0 then
        Break;
      Dec(LBlockNest);
    end;

    if (Current.Kind = tkSemicolon) and (LDelimiterNest = 0) and (LBlockNest = 0) then
      Break;

    if (LDelimiterNest = 0) and (LBlockNest = 0) and
       (Current.Kind in [tkEndKeyword, tkElseKeyword, tkFinallyKeyword, tkExceptKeyword]) then
      Break;
    Result.DeclarationTokens.Add(NextToken);
  end;

  if (Current <> nil) and (Current.Kind = tkSemicolon) then
    Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseMethodImplementation(const AFullSource: string): TMethodImplementationSyntax;
var
  LNestLevel: Integer;
  LDecl: TDeclarationSectionSyntax;
  LUnparsed: TUnparsedDeclarationSyntax;
  LScan: Integer;
  LCurrentSignatureName: string;
  LCurrentSignatureShortName: string;

  function IsQualifiedMethodStart(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (((AToken.Kind = tkProcedureKeyword) or (AToken.Kind = tkFunctionKeyword) or
        (AToken.Kind = tkConstructorKeyword) or (AToken.Kind = tkDestructorKeyword)) and
       (Peek(1) <> nil) and (Peek(1).Kind = tkIdentifier) and
       (Peek(2) <> nil) and (Peek(2).Kind = tkDot)) or
      ((AToken.Kind = tkClassKeyword) and (Peek(1) <> nil) and
       ((Peek(1).Kind = tkProcedureKeyword) or (Peek(1).Kind = tkFunctionKeyword)) and
       (Peek(2) <> nil) and (Peek(2).Kind = tkIdentifier) and
       (Peek(3) <> nil) and (Peek(3).Kind = tkDot))
    );
  end;

  function IsMethodModifierStart(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (AToken.Kind = tkOverrideKeyword) or
      ((AToken.Kind = tkIdentifier) and (
        SameText(AToken.Text, 'dispid') or
        SameText(AToken.Text, 'message') or
        SameText(AToken.Text, 'deprecated') or
        SameText(AToken.Text, 'platform') or
        SameText(AToken.Text, 'library') or
        SameText(AToken.Text, 'experimental') or
        SameText(AToken.Text, 'unimplemented') or
        SameText(AToken.Text, 'overload') or
        SameText(AToken.Text, 'override') or
        SameText(AToken.Text, 'reintroduce') or
        SameText(AToken.Text, 'virtual') or
        SameText(AToken.Text, 'dynamic') or
        SameText(AToken.Text, 'abstract') or
        SameText(AToken.Text, 'final') or
        SameText(AToken.Text, 'static') or
        SameText(AToken.Text, 'inline') or
        SameText(AToken.Text, 'assembler') or
        SameText(AToken.Text, 'cdecl') or
        SameText(AToken.Text, 'pascal') or
        SameText(AToken.Text, 'register') or
        SameText(AToken.Text, 'stdcall') or
        SameText(AToken.Text, 'safecall') or
        SameText(AToken.Text, 'varargs')
      )));
  end;

  function HasDotBeforeSemicolon: Boolean;
  begin
    Result := False;
    LScan := FPosition;
    while (LScan < FTokens.Count) and (FTokens[LScan].Kind <> tkEOF) do
    begin
      if FTokens[LScan].Kind = tkDot then
        Exit(True);
      if FTokens[LScan].Kind = tkSemicolon then
        Exit(False);
      Inc(LScan);
    end;
  end;

  function ExtractSignatureNameFromTokens(ATokens: TObjectList<TSyntaxToken>): string;
  begin
    Result := '';
    if (ATokens = nil) or (ATokens.Count = 0) then
      Exit;
    if (ATokens.Count >= 3) and (ATokens[0].Kind = tkIdentifier) and
       (ATokens[1].Kind = tkDot) and (ATokens[2].Kind = tkIdentifier) then
      Exit(ATokens[0].Text + '.' + ATokens[2].Text);
    if ATokens[0].Kind = tkIdentifier then
      Exit(ATokens[0].Text);
  end;

  function UpcomingSignatureName: string;
  begin
    Result := '';
    if (Current = nil) then Exit;
    if (Current.Kind <> tkProcedureKeyword) and (Current.Kind <> tkFunctionKeyword) then
      Exit;
    if (Peek(1) <> nil) and (Peek(1).Kind = tkIdentifier) then
    begin
      if (Peek(2) <> nil) and (Peek(2).Kind = tkDot) and (Peek(3) <> nil) and (Peek(3).Kind = tkIdentifier) then
        Exit(Peek(1).Text + '.' + Peek(3).Text);
      Exit(Peek(1).Text);
    end;
  end;
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
  begin
    // Keep chained signature segments like "; stdcall;" in the signature stream.
    while (Current <> nil) and (Current.Kind = tkSemicolon) and IsMethodModifierStart(Peek(1)) do
    begin
      Result.SignatureTokens.Add(NextToken); // ;
      while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
        Result.SignatureTokens.Add(NextToken);
    end;
    Result.SignatureSemicolon := MatchToken(tkSemicolon);
  end;

  LCurrentSignatureName := ExtractSignatureNameFromTokens(Result.SignatureTokens);
  LCurrentSignatureShortName := LCurrentSignatureName;
  if Pos('.', LCurrentSignatureShortName) > 0 then
    LCurrentSignatureShortName := Copy(LCurrentSignatureShortName,
      Pos('.', LCurrentSignatureShortName) + 1, MaxInt);
    
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
      // Conditional alternative implementation signatures are often fully-qualified
      // and should stay as unparsed local declaration tokens before the shared body.
      if HasDotBeforeSemicolon or
         ((LCurrentSignatureName <> '') and
          (SameText(UpcomingSignatureName, LCurrentSignatureName) or
           SameText(UpcomingSignatureName, LCurrentSignatureShortName))) then
      begin
        LNestLevel := 0;
        if LUnparsed = nil then
        begin
          LUnparsed := TUnparsedDeclarationSyntax.Create;
          Result.LocalDeclarations.Add(LUnparsed);
        end;
        while (Current <> nil) and (Current.Kind <> tkEOF) do
        begin
          if Current.Kind = tkOpenParen then
            Inc(LNestLevel)
          else if Current.Kind = tkCloseParen then
            Dec(LNestLevel)
          else if (Current.Kind = tkSemicolon) and (LNestLevel <= 0) then
            Break;
          LUnparsed.Tokens.Add(NextToken);
        end;
        if (Current <> nil) and (Current.Kind = tkSemicolon) then
        begin
          LUnparsed.Tokens.Add(NextToken);
          while (Current <> nil) and (Current.Kind = tkIdentifier) and
                IsMethodModifierStart(Current) and (Peek(1) <> nil) and (Peek(1).Kind = tkSemicolon) do
          begin
            LUnparsed.Tokens.Add(NextToken);
            LUnparsed.Tokens.Add(NextToken);
          end;
        end;
      end
      else
      begin
        // Nested procedure/function - parse recursively
        Result.LocalDeclarations.Add(ParseMethodImplementation);
      end;
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
    // Recovery: a qualified method header at this level should belong to the
    // implementation section, not to the current method body.
    if IsQualifiedMethodStart(Current) then
      Break;

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
  LDeclBlockNest: Integer;
  LPrevKind: TTokenKind;
  
  function IsMethodStartToken(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (AToken.Kind = tkProcedureKeyword) or
      (AToken.Kind = tkFunctionKeyword) or
      ((AToken.Kind = tkConstructorKeyword) and (Peek(1) <> nil) and (Peek(1).Kind = tkIdentifier) and
       (Peek(2) <> nil) and (Peek(2).Kind = tkDot)) or
      ((AToken.Kind = tkDestructorKeyword) and (Peek(1) <> nil) and (Peek(1).Kind = tkIdentifier) and
       (Peek(2) <> nil) and (Peek(2).Kind = tkDot)) or
      ((AToken.Kind = tkClassKeyword) and (Peek(1) <> nil) and 
       ((Peek(1).Kind = tkProcedureKeyword) or (Peek(1).Kind = tkFunctionKeyword))));
  end;

  function IsQualifiedMethodStart(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (((AToken.Kind = tkProcedureKeyword) or (AToken.Kind = tkFunctionKeyword) or
        (AToken.Kind = tkConstructorKeyword) or (AToken.Kind = tkDestructorKeyword)) and
       (Peek(1) <> nil) and (Peek(1).Kind = tkIdentifier) and
       (Peek(2) <> nil) and (Peek(2).Kind = tkDot)) or
      ((AToken.Kind = tkClassKeyword) and (Peek(1) <> nil) and
       ((Peek(1).Kind = tkProcedureKeyword) or (Peek(1).Kind = tkFunctionKeyword)) and
       (Peek(2) <> nil) and (Peek(2).Kind = tkIdentifier) and
       (Peek(3) <> nil) and (Peek(3).Kind = tkDot))
    );
  end;

  function IsForwardClassLikeDecl: Boolean;
  var
    LIdx: Integer;
    LParenNest: Integer;
  begin
    LIdx := FPosition + 1; // after class/interface/dispinterface
    LParenNest := 0;

    if (LIdx < FTokens.Count) and (FTokens[LIdx].Kind = tkOpenParen) then
    begin
      Inc(LParenNest);
      Inc(LIdx);
      while (LIdx < FTokens.Count) and (FTokens[LIdx].Kind <> tkEOF) do
      begin
        if FTokens[LIdx].Kind = tkOpenParen then
          Inc(LParenNest)
        else if FTokens[LIdx].Kind = tkCloseParen then
        begin
          Dec(LParenNest);
          if LParenNest = 0 then
          begin
            Inc(LIdx);
            Break;
          end;
        end;
        Inc(LIdx);
      end;
    end;

    Result := (LIdx < FTokens.Count) and (FTokens[LIdx].Kind = tkSemicolon);
  end;

begin
  Result := TImplementationSectionSyntax.Create;
  Result.ImplementationKeyword := MatchToken(tkImplementationKeyword);
  LDeclBlockNest := 0;
  LPrevKind := tkUnknown;
  
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
    if IsQualifiedMethodStart(Current) then
    begin
      LDeclBlockNest := 0; // recover from stale nesting in unparsed declaration streams
      LUnparsed := nil;
      Result.Declarations.Add(ParseMethodImplementation);
    end
    else if (LDeclBlockNest = 0) and IsMethodStartToken(Current) then
    begin
      LUnparsed := nil; // Reset unparsed stream
      Result.Declarations.Add(ParseMethodImplementation);
    end
    else if (LDeclBlockNest = 0) and ((Current.Kind = tkVarKeyword) or (Current.Kind = tkConstKeyword) or (Current.Kind = tkTypeKeyword)) then
    begin
       LUnparsed := nil;
       Result.Declarations.Add(ParseDeclarationSection);
       LPrevKind := tkUnknown;
    end
    else
    begin
      if LUnparsed = nil then
      begin
        LUnparsed := TUnparsedDeclarationSyntax.Create;
        Result.Declarations.Add(LUnparsed); // Add instantly to maintain order
      end;

      if (
           ((Current.Kind = tkClassKeyword) and (LPrevKind in [tkEquals, tkColon]) and
            ((Peek(1) = nil) or (Peek(1).Kind <> tkOfKeyword)) and not IsForwardClassLikeDecl) or
           ((Current.Kind = tkInterfaceKeyword) and (LPrevKind in [tkEquals, tkColon]) and not IsForwardClassLikeDecl) or
           ((Current.Kind = tkDispinterfaceKeyword) and (LPrevKind in [tkEquals, tkColon]) and not IsForwardClassLikeDecl) or
           ((Current.Kind = tkRecordKeyword) and (LPrevKind in [tkEquals, tkColon])) or
           ((Current.Kind = tkCaseKeyword) and (LDeclBlockNest > 0))
         ) then
        Inc(LDeclBlockNest)
      else if (Current.Kind = tkEndKeyword) and (LDeclBlockNest > 0) then
        Dec(LDeclBlockNest);

      LPrevKind := Current.Kind;
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
