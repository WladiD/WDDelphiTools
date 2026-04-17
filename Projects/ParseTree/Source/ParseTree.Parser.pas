// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit ParseTree.Parser;

interface

uses

  mormot.core.collections,

  ParseTree.Core,
  ParseTree.Nodes,
  ParseTree.Tokens;

type

  /// <summary>TParseTreeParser is the main entry point to parse Pascal source code into a CST</summary>
  TParseTreeParser = class
  private
    FPosition: Integer;
    FTokens  : IList<TSyntaxToken>;
    function Peek(AOffset: Integer = 0): TSyntaxToken;
    function Current: TSyntaxToken;
    function NextToken: TSyntaxToken;
    function MatchToken(AKind: TTokenKind): TSyntaxToken;
    function HasDotAfterIdentifierOrGeneric(AStartIndex: Integer): Boolean;
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

uses

  System.Classes,
  System.SysUtils,

  ParseTree.Lexer;

{ TParseTreeParser }

function TParseTreeParser.Peek(AOffset: Integer): TSyntaxToken;
var
  Index: Integer;
begin
  Index := FPosition + AOffset;
  if (Index >= 0) and (Index < FTokens.Count) then
    Result := FTokens[Index]
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

function TParseTreeParser.HasDotAfterIdentifierOrGeneric(AStartIndex: Integer): Boolean;
var
  GenericDepth: Integer;
  Idx         : Integer;
  Token       : TSyntaxToken;
begin
  Idx := FPosition + AStartIndex;
  Token := Peek(AStartIndex);
  if not Assigned(Token) or (Token.Kind <> tkIdentifier) then
    Exit(False);
  
  Inc(Idx);
  Token := Peek(Idx - FPosition);
  
  if Assigned(Token) and (Token.Kind = tkLessThan) then
  begin
    GenericDepth := 1;
    Inc(Idx);
    while (GenericDepth > 0) do
    begin
      Token := Peek(Idx - FPosition);
      if Token = nil then Exit(False);
      
      if Token.Kind = tkLessThan then Inc(GenericDepth)
      else if Token.Kind = tkGreaterThan then Dec(GenericDepth);
      
      Inc(Idx);
    end;
    Token := Peek(Idx - FPosition);
  end;
  
  Result := (Token <> nil) and (Token.Kind = tkDot);
end;

function TParseTreeParser.ParseUsesClause: TUsesClauseSyntax;
var
  UnitRef: TUnitReferenceSyntax;
begin
  Result := nil;
  if (Current = nil) or (Current.Kind <> tkUsesKeyword) then
    Exit;
    
  Result := TUsesClauseSyntax.Create;
  Result.UsesKeyword := MatchToken(tkUsesKeyword);
  
  while (Current <> nil) and (Current.Kind <> tkSemicolon) and (Current.Kind <> tkEOF) do
  begin
    UnitRef := TUnitReferenceSyntax.Create;
    
    // Parse the unit identifier(s) and dots (e.g. System.SysUtils)
    var ExpectDot: Boolean := False;
    while
      (Current <> nil) and
      (Current.Kind in [tkIdentifier, tkDot, tkUnitKeyword, tkInterfaceKeyword,
        tkImplementationKeyword, tkUsesKeyword]) do
    begin
      if Current.Kind = tkDot then
      begin
        if not ExpectDot then
          Break;
        UnitRef.Dots.Add(NextToken);
        ExpectDot := False;
      end
      else
      begin
        if ExpectDot then
          Break;
        UnitRef.Namespaces.Add(NextToken);
        ExpectDot := True;
      end;
    end;
    
    // Parse optional 'in' clause (e.g. in '..\Unit1.pas')
    if (Current <> nil) and (Current.Kind = tkInKeyword) then
    begin
      UnitRef.InKeyword := MatchToken(tkInKeyword);
      
      if (Current <> nil) and (Current.Kind = tkStringLiteral) then
        UnitRef.StringLiteral := MatchToken(tkStringLiteral);
    end;
    
    Result.UnitReferences.Add(UnitRef);
    
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
  FallbackOpaqueConst: Boolean;
  HasComplexType     : Boolean;
  NestLevel          : Integer;
  TypeTokens         : IList<TSyntaxToken>;
begin
  Result := TConstDeclarationSyntax.Create;
  Result.Identifier := MatchToken(tkIdentifier);
  FallbackOpaqueConst := False;
  
  if (Current <> nil) and (Current.Kind = tkColon) then
  begin
    Result.ColonToken := MatchToken(tkColon);
    TypeTokens := Collections.NewList<TSyntaxToken>([loNoFinalize]);
    NestLevel := 0;
    while (Current <> nil) and (Current.Kind <> tkEOF) do
    begin
      if Current.Kind in [tkOpenParen, tkOpenBracket, tkLessThan] then
        Inc(NestLevel)
      else if Current.Kind in [tkCloseParen, tkCloseBracket, tkGreaterThan] then
        Dec(NestLevel)
      else if (Current.Kind = tkEquals) and (NestLevel <= 0) then
        Break
      else if (Current.Kind = tkSemicolon) and (NestLevel <= 0) then
        Break;

      TypeTokens.Add(NextToken);
    end;

    HasComplexType := (TypeTokens.Count <> 1) or (TypeTokens[0].Kind <> tkIdentifier);
    if (TypeTokens.Count > 0) and not HasComplexType then
      Result.TypeIdentifier := TypeTokens[0]
    else
    begin
      // Fallback for complex typed constants: preserve the entire right stream opaquely.
      FallbackOpaqueConst := True;
      if Result.ColonToken <> nil then
      begin
        Result.ValueTokens.Add(Result.ColonToken);
        Result.ColonToken := nil;
      end;
      for NestLevel := 0 to TypeTokens.Count - 1 do
        Result.ValueTokens.Add(TypeTokens[NestLevel]);
    end;
  end;
  
  if (Current <> nil) and (Current.Kind = tkEquals) then
  begin
    if FallbackOpaqueConst then
      Result.ValueTokens.Add(MatchToken(tkEquals))
    else
      Result.EqualsToken := MatchToken(tkEquals);
    // Read value tokens until semicolon (respect nested parentheses/brackets)
    NestLevel := 0;
    while (Current <> nil) and (Current.Kind <> tkEOF) do
    begin
      if (Current.Kind = tkOpenParen) or (Current.Kind = tkOpenBracket) then
        Inc(NestLevel)
      else if (Current.Kind = tkCloseParen) or (Current.Kind = tkCloseBracket) then
        Dec(NestLevel)
      else if (Current.Kind = tkSemicolon) and (NestLevel <= 0) then
        Break;
      Result.ValueTokens.Add(NextToken);
    end;
  end;
    
  Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseVarDeclaration: TVarDeclarationSyntax;

  function IsVarTypeModifierStart(AToken: TSyntaxToken): Boolean;
  begin
    Result :=
      Assigned(AToken) and
      (
        (AToken.Kind = tkOverrideKeyword) or
        (
          (AToken.Kind = tkIdentifier) and
          (
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
          )
        )
      );
  end;

var
  NestLevel: Integer;
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
  NestLevel := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if (Current.Kind in [tkOpenParen, tkOpenBracket, tkLessThan]) then
      Inc(NestLevel)
    else if (Current.Kind in [tkCloseParen, tkCloseBracket, tkGreaterThan]) then
      Dec(NestLevel)
    else if (Current.Kind = tkSemicolon) and (NestLevel <= 0) then
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
    Result := Assigned(AToken) and
      (AToken.Kind in [tkPrivateKeyword, tkProtectedKeyword, tkPublicKeyword, tkPublishedKeyword]);
  end;

  function IsTypeModifierStart(AToken: TSyntaxToken): Boolean;
  begin
    Result :=
      Assigned(AToken) and
      (
        (AToken.Kind = tkOverrideKeyword) or
        (
          (AToken.Kind = tkIdentifier) and
          (
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
          )
        )
      );
  end;

  function IsMemberStartToken(AToken: TSyntaxToken): Boolean;
  begin
    Result := Assigned(AToken) and
      (AToken.Kind in [
        tkIdentifier, tkProcedureKeyword, tkFunctionKeyword, tkConstructorKeyword,
        tkDestructorKeyword, tkPropertyKeyword, tkClassKeyword, tkTypeKeyword, tkVarKeyword,
        tkConstKeyword]);
  end;

  procedure ParseClassMember(ASection: TVisibilitySectionSyntax);
  var
    LCanHaveModifiers : Boolean;
    LDeclBlockNest    : Integer;
    LHasColon         : Boolean;
    LIsModifierStart  : Boolean;
    LMember           : TClassMemberSyntax;
    LModifierSemicolon: Boolean;
    LNestLevel        : Integer;
    LPrevKind         : TTokenKind;
    LScan             : Integer;

    function IsForwardClassLikeDecl: Boolean;
    var
      LIdx: Integer;
    begin
      LIdx := FPosition + 1; // token right after class/interface/dispinterface
      // Forward declarations end directly with ';' after optional modifiers and optional base list.
      // But we just skip until we see either '(' or ';' or 'end'.
      while (LIdx < FTokens.Count) and (FTokens[LIdx].Kind <> tkEOF) do
      begin
        if FTokens[LIdx].Kind = tkSemicolon then
        begin
          Result := True;
          Exit;
        end;
        if (FTokens[LIdx].Kind = tkEndKeyword) or 
           (FTokens[LIdx].Kind = tkOpenBracket) or
           (FTokens[LIdx].Kind = tkTypeKeyword) or
           (FTokens[LIdx].Kind = tkVarKeyword) or
           (FTokens[LIdx].Kind = tkConstKeyword) or
           (FTokens[LIdx].Kind = tkProcedureKeyword) or
           (FTokens[LIdx].Kind = tkFunctionKeyword) or
           (FTokens[LIdx].Kind = tkPropertyKeyword) or
           IsVisibilityKeyword(FTokens[LIdx]) then
        begin
          Result := False;
          Exit;
        end;
        Inc(LIdx);
      end;
      Result := False;
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
      else if (Current.Kind = tkCloseParen) or (Current.Kind = tkGreaterThan) or
              ((Current.Kind = tkGreaterOrEquals) and (LNestLevel > 0)) then
        Dec(LNestLevel)
      // Track nested declaration blocks (e.g. record/case inside member types).
      // Only at paren/generic nesting 0 — inside `<...>` or `(...)` the same
      // keywords are generic constraints (e.g. `<T: class>`) or param types,
      // not the start of a nested type body.
      else if (LNestLevel = 0) and (
        ((Current.Kind = tkClassKeyword) and (LPrevKind in [tkEquals, tkColon]) and
         ((Peek(1) = nil) or (Peek(1).Kind <> tkOfKeyword)) and not IsForwardClassLikeDecl) or
        ((Current.Kind = tkInterfaceKeyword) and (LPrevKind in [tkEquals, tkColon]) and not IsForwardClassLikeDecl) or
        ((Current.Kind = tkDispinterfaceKeyword) and (LPrevKind in [tkEquals, tkColon]) and not IsForwardClassLikeDecl) or
        ((Current.Kind = tkRecordKeyword) and ((LPrevKind in [tkEquals, tkColon]) or ((LPrevKind = tkIdentifier) and (LMember.Tokens.Count > 0) and SameText(LMember.Tokens[LMember.Tokens.Count - 1].Text, 'packed')))) or
        ((Current.Kind = tkCaseKeyword) and (LDeclBlockNest > 0))
      ) then
        Inc(LDeclBlockNest)
      else if (Current.Kind = tkEndKeyword) then
      begin
        if LDeclBlockNest > 0 then
          Dec(LDeclBlockNest)
        else
          Break; // end of class/record body — do not consume
      end;
      
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
        var LClassMember: TClassMemberSyntax:=TClassMemberSyntax.Create;
        LVisSec.Members.Add(LClassMember);
        LClassMember.Tokens.Add(NextToken); // [
        while (Current <> nil) and (Current.Kind <> tkCloseBracket) and (Current.Kind <> tkEOF) do
          LClassMember.Tokens.Add(NextToken);
        if (Current <> nil) and (Current.Kind = tkCloseBracket) then
          LClassMember.Tokens.Add(NextToken); // ]
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
        LVisSec.Members[LVisSec.Members.Count-1].Tokens.Add(NextToken);
      end;
    end;
    
    if (Current <> nil) and (Current.Kind = tkEndKeyword) then
      ATypeDecl.EndKeyword := MatchToken(tkEndKeyword);
  end;

var
  LNestLevel: Integer;
  LTypeKind: TTokenKind;
begin
  // Peek ahead to determine the specific type declaration class to instantiate
  LTypeKind := tkUnknown;
  if (Current <> nil) and (Current.Kind = tkIdentifier) then
  begin
    var LScan: Integer := FPosition + 1;
    // Skip generic type parameters <T>
    if (LScan < FTokens.Count) and (FTokens[LScan].Kind = tkLessThan) then
    begin
      Inc(LScan);
      while (LScan < FTokens.Count) and (FTokens[LScan].Kind <> tkGreaterThan) and (FTokens[LScan].Kind <> tkEOF) do
        Inc(LScan);
      if (LScan < FTokens.Count) and (FTokens[LScan].Kind = tkGreaterThan) then
        Inc(LScan);
    end;
    
    if (LScan < FTokens.Count) and (FTokens[LScan].Kind = tkEquals) then
    begin
      Inc(LScan);
      if (LScan < FTokens.Count) then
        LTypeKind := FTokens[LScan].Kind;
    end;
  end;

  if LTypeKind = tkClassKeyword then
    Result := TClassDeclarationSyntax.Create
  else if LTypeKind = tkRecordKeyword then
    Result := TRecordDeclarationSyntax.Create
  else
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
    
    // Handle 'packed record' / 'packed class': keep 'packed' as TypeTypeToken,
    // consume 'record'/'class' into TypeExtraTokens, but still parse as record/class body
    if (Current <> nil) and (Current.Kind = tkIdentifier) and SameText(Current.Text, 'packed') and
       (Peek(1) <> nil) and (Peek(1).Kind in [tkRecordKeyword, tkClassKeyword]) then
    begin
      Result.TypeTypeToken := NextToken; // packed
      Result.TypeExtraTokens.Add(NextToken); // record/class
    end
    // Parse what kind of type it is (like class, interface, record)
    else if (Current <> nil) and ((Current.Kind = tkClassKeyword) or (Current.Kind = tkInterfaceKeyword) or (Current.Kind = tkDispinterfaceKeyword) or (Current.Kind = tkRecordKeyword) or (Current.Kind = tkIdentifier)) then
      Result.TypeTypeToken := NextToken;

    if (Result.TypeTypeToken <> nil) and
       ((Result.TypeTypeToken.Kind = tkClassKeyword) or (Result.TypeTypeToken.Kind = tkInterfaceKeyword) or (Result.TypeTypeToken.Kind = tkDispinterfaceKeyword) or (Result.TypeTypeToken.Kind = tkRecordKeyword) or
        ((Result.TypeTypeToken.Kind = tkIdentifier) and SameText(Result.TypeTypeToken.Text, 'packed'))) and
       not ((Result.TypeTypeToken.Kind = tkClassKeyword) and (Current <> nil) and (Current.Kind = tkOfKeyword)) then
    begin
      // Consume modifiers like 'abstract' or 'sealed' before the base list
      while (Current <> nil) do
      begin
        if (Current.Kind = tkIdentifier) and
           (SameText(Current.Text, 'abstract') or SameText(Current.Text, 'sealed') or SameText(Current.Text, 'deprecated')) then
        begin
          Result.TypeExtraTokens.Add(NextToken);
          // 'deprecated' can be followed by a string literal message
          if (Current <> nil) and (Current.Kind = tkStringLiteral) then
            Result.TypeExtraTokens.Add(NextToken);
        end
        else
          Break;
      end;

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
      
    if Assigned(Result.EndKeyword) then
      Result.TrailingTokens.Add(NextToken)
    else
      Result.TypeExtraTokens.Add(NextToken);
  end;
    
  Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseDeclarationSection: TDeclarationSectionSyntax;
var
  ConstSec: TConstSectionSyntax;
  TypeSec : TTypeSectionSyntax;
  VarSec  : TVarSectionSyntax;
begin
  Result := nil;
  if Current = nil then Exit;

  if Current.Kind = tkTypeKeyword then
  begin
    TypeSec := TTypeSectionSyntax.Create;
    TypeSec.TypeKeyword := MatchToken(tkTypeKeyword);
    
    while (Current <> nil) and 
          (Current.Kind = tkIdentifier) do
    begin
      TypeSec.Declarations.Add(ParseTypeDeclaration);
    end;
    
    Result := TypeSec;
  end
  else if Current.Kind = tkConstKeyword then
  begin
    ConstSec := TConstSectionSyntax.Create;
    ConstSec.ConstKeyword := MatchToken(tkConstKeyword);
    
    while (Current <> nil) and 
          (Current.Kind = tkIdentifier) do
    begin
      ConstSec.Declarations.Add(ParseConstDeclaration);
    end;
    
    Result := ConstSec;
  end
  else if Current.Kind = tkVarKeyword then
  begin
    VarSec := TVarSectionSyntax.Create;
    VarSec.VarKeyword := MatchToken(tkVarKeyword);

    while (Current <> nil) and 
          (Current.Kind = tkIdentifier) do
    begin
      VarSec.Declarations.Add(ParseVarDeclaration);
    end;
    
    Result := VarSec;
  end;
end;

function TParseTreeParser.ParseInterfaceSection: TInterfaceSectionSyntax;
var
  Decl     : TDeclarationSectionSyntax;
  NestLevel: Integer;
  Unparsed : TUnparsedDeclarationSyntax;
begin
  Result := nil;
  if (Current = nil) or (Current.Kind <> tkInterfaceKeyword) then
    Exit;
    
  Result := TInterfaceSectionSyntax.Create;
  Result.InterfaceKeyword := MatchToken(tkInterfaceKeyword);
  
  // Look for uses clause right after interface
  if (Current <> nil) and (Current.Kind = tkUsesKeyword) then
    Result.UsesClause := ParseUsesClause();

  Unparsed := nil;
  NestLevel := 0; // Track paren nesting
  // Parse interface declarations: type, const, var, and other constructs
  while (Current <> nil) and 
        (Current.Kind <> tkImplementationKeyword) and 
        (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then
      Inc(NestLevel)
    else if Current.Kind = tkCloseParen then
      Dec(NestLevel);
      
    if (NestLevel <= 0) and (Current.Kind in [tkTypeKeyword, tkConstKeyword, tkVarKeyword]) then
    begin
      Unparsed := nil;
      Decl := ParseDeclarationSection;
      if Assigned(Decl) then
        Result.Declarations.Add(Decl);
    end
    else
    begin
      // Collect unrecognized tokens (e.g. standalone function/procedure declarations)
      // into TUnparsedDeclarationSyntax to preserve them for roundtrip fidelity
      if Unparsed = nil then
      begin
        Unparsed := TUnparsedDeclarationSyntax.Create;
        Result.Declarations.Add(Unparsed);
      end;
      Unparsed.Tokens.Add(NextToken);
    end;
  end;
end;

function TParseTreeParser.ParseStatement: TStatementSyntax;
var
  Opaque: TOpaqueStatementSyntax;
  Nest: Integer;
begin
  if Current = nil then
    Exit(nil);

  if Current.Kind = tkSemicolon then
  begin
    var LEmpty := TEmptyStatementSyntax.Create;
    LEmpty.Semicolon := MatchToken(tkSemicolon);
    Exit(LEmpty);
  end;

  case Current.Kind of
    tkWhileKeyword:     Exit(ParseWhileStatement);
    tkRepeatKeyword:    Exit(ParseRepeatStatement);
    tkForKeyword:       Exit(ParseForStatement);
    tkIfKeyword:        Exit(ParseIfStatement);
    tkBeginKeyword:     Exit(ParseBeginEndStatement);
    tkTryKeyword:       Exit(ParseTryStatement);
    tkRaiseKeyword:     Exit(ParseRaiseStatement);
    tkCaseKeyword:      Exit(ParseCaseStatement);
    tkWithKeyword:      Exit(ParseWithStatement);
    tkInheritedKeyword: Exit(ParseInheritedStatement);
    tkExitKeyword:      Exit(ParseExitStatement);
    tkVarKeyword:       Exit(ParseInlineVarStatement);
  end;

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
  Opaque := TOpaqueStatementSyntax.Create;
  Nest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if (Current.Kind in [tkBeginKeyword, tkTryKeyword, tkCaseKeyword, tkAsmKeyword]) then
      Inc(Nest)
    else if Current.Kind = tkEndKeyword then
    begin
      if Nest = 0 then Break;
      Dec(Nest);
    end
    else if ((Current.Kind in [tkFinallyKeyword, tkExceptKeyword])) and (Nest = 0) and (Opaque.Tokens.Count > 0) then
      Break
    else if (Current.Kind = tkSemicolon) and (Nest = 0) then
    begin
      Opaque.Tokens.Add(NextToken);
      Break;
    end;

    // Safety: if we hit another keyword that starts a statement, break if we have ANY tokens
    if (Nest = 0) and (Opaque.Tokens.Count > 0) and
       (Current.Kind in [tkWhileKeyword, tkForKeyword, tkRepeatKeyword, tkIfKeyword, tkElseKeyword]) then
      Break;

    Opaque.Tokens.Add(NextToken);
  end;
  Result := Opaque;
end;

function TParseTreeParser.ParseWhileStatement: TWhileStatementSyntax;
begin
  Result := TWhileStatementSyntax.Create;
  Result.WhileKeyword := MatchToken(tkWhileKeyword);
  
  while (Current <> nil) and (not (Current.Kind in [tkDoKeyword, tkEOF])) do
    Result.ConditionTokens.Add(NextToken);
    
  if (Current <> nil) and (Current.Kind = tkDoKeyword) then
    Result.DoKeyword := MatchToken(tkDoKeyword);
    
  Result.Statement := ParseStatement;
end;

function TParseTreeParser.ParseRepeatStatement: TRepeatStatementSyntax;
begin
  Result := TRepeatStatementSyntax.Create;
  Result.RepeatKeyword := MatchToken(tkRepeatKeyword);

  while (Current <> nil) and (not (Current.Kind in [tkUntilKeyword, tkEOF])) do
  begin
    if Current.Kind in [tkEndKeyword, tkFinallyKeyword, tkExceptKeyword] then
      Break;
    Result.Statements.Add(ParseStatement);
  end;

  if (Current <> nil) and (Current.Kind = tkUntilKeyword) then
    Result.UntilKeyword := MatchToken(tkUntilKeyword);

  while (Current <> nil) and (not (Current.Kind in [tkSemicolon, tkEOF])) do
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

  while (Current <> nil) and (not (Current.Kind in [tkColonEquals, tkInKeyword, tkEOF])) do
    Result.VariableTokens.Add(NextToken);

  if (Current <> nil) and (Current.Kind = tkInKeyword) then
  begin
    Result.InKeyword := MatchToken(tkInKeyword);

    while (Current <> nil) and (not (Current.Kind in [tkDoKeyword, tkEOF])) do
      Result.CollectionTokens.Add(NextToken);
  end
  else
  begin
    if (Current <> nil) and (Current.Kind = tkColonEquals) then
      Result.AssignmentToken := MatchToken(tkColonEquals);

    while (Current <> nil) and (not (Current.Kind in [tkToKeyword, tkDowntoKeyword, tkEOF])) do
      Result.StartTokens.Add(NextToken);

    if (Current <> nil) and (Current.Kind in [tkToKeyword, tkDowntoKeyword]) then
      Result.ToDowntoKeyword := NextToken;

    while (Current <> nil) and not (Current.Kind in [tkDoKeyword, tkEOF]) do
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

  while (Current <> nil) and not (Current.Kind in [tkThenKeyword, tkEOF]) do
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
  while (Current <> nil) and not (Current.Kind in [tkColonEquals, tkEOF]) do
    Result.LeftTokens.Add(NextToken);

  if (Current <> nil) and (Current.Kind = tkColonEquals) then
    Result.ColonEqualsToken := MatchToken(tkColonEquals);

  var LNest := 0;
  var LBlockNest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind in [tkOpenParen, tkOpenBracket] then Inc(LNest)
    else if Current.Kind in [tkCloseParen, tkCloseBracket] then Dec(LNest);

    if Current.Kind in [tkBeginKeyword, tkTryKeyword, tkCaseKeyword, tkAsmKeyword] then
      Inc(LBlockNest)
    else if Current.Kind = tkEndKeyword then
    begin
      if LBlockNest = 0 then Break;
      Dec(LBlockNest);
    end
    else if (Current.Kind in [tkFinallyKeyword, tkExceptKeyword]) and
            (LNest = 0) and (LBlockNest = 0) and (Result.RightTokens.Count > 0) then
      Break;

    if (Current.Kind = tkSemicolon) and (LNest = 0) and (LBlockNest = 0) then
    begin
      Result.RightTokens.Add(MatchToken(tkSemicolon));
      Break;
    end;

    // Break if we hit a keyword that definitely starts a new statement
    if (LNest = 0) and (LBlockNest = 0) and (Result.RightTokens.Count > 0) and
       (Current.Kind in [tkIfKeyword, tkWhileKeyword, tkForKeyword, tkRepeatKeyword, tkElseKeyword]) then
      Break;

    Result.RightTokens.Add(NextToken);
  end;
end;

function TParseTreeParser.ParseBeginEndStatement: TBeginEndStatementSyntax;
begin
  Result := TBeginEndStatementSyntax.Create;
  Result.BeginKeyword := MatchToken(tkBeginKeyword);

  while (Current <> nil) and not (Current.Kind in [tkEndKeyword, tkEOF]) do
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

  while (Current <> nil) and not (Current.Kind in [tkFinallyKeyword, tkExceptKeyword, tkEndKeyword, tkEOF]) do
    Result.Statements.Add(ParseStatement);

  if (Current <> nil) and (Current.Kind = tkFinallyKeyword) then
    Result.FinallyKeyword := MatchToken(tkFinallyKeyword)
  else if (Current <> nil) and (Current.Kind = tkExceptKeyword) then
    Result.ExceptKeyword := MatchToken(tkExceptKeyword);

  while (Current <> nil) and not (Current.Kind in [tkEndKeyword, tkEOF]) do
    Result.FinallyExceptStatements.Add(ParseStatement);

  if (Current <> nil) and (Current.Kind = tkEndKeyword) then
    Result.EndKeyword := MatchToken(tkEndKeyword);

  if (Current <> nil) and (Current.Kind = tkSemicolon) then
    Result.Semicolon := MatchToken(tkSemicolon);
end;

function TParseTreeParser.ParseRaiseStatement: TRaiseStatementSyntax;
var
  Nest: Integer;
begin
  Result := TRaiseStatementSyntax.Create;
  Result.RaiseKeyword := MatchToken(tkRaiseKeyword);

  Nest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then
      Inc(Nest)
    else if Current.Kind = tkCloseParen then
      Dec(Nest);

    if (Current.Kind = tkSemicolon) and (Nest <= 0) then
    begin
      Result.Semicolon := MatchToken(tkSemicolon);
      Break;
    end;

    if (Nest <= 0) and (Current.Kind in [tkEndKeyword, tkElseKeyword,
       tkFinallyKeyword, tkExceptKeyword]) then
      Break;

    Result.ExpressionTokens.Add(NextToken);
  end;
end;

function TParseTreeParser.ParseProcedureCallStatement: TProcedureCallStatementSyntax;
var
  DelimiterNest: Integer;
  Nest         : Integer;
  PreviousIsDot: Boolean;
begin
  Result := TProcedureCallStatementSyntax.Create;
  Nest := 0;
  DelimiterNest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind in [tkOpenParen, tkOpenBracket] then
      Inc(DelimiterNest)
    else if Current.Kind in [tkCloseParen, tkCloseBracket] then
    begin
      if DelimiterNest > 0 then
        Dec(DelimiterNest);
    end
    else if Current.Kind in [tkBeginKeyword, tkTryKeyword, tkCaseKeyword, tkAsmKeyword] then
      Inc(Nest)
    else if Current.Kind = tkEndKeyword then
    begin
      if Nest = 0 then Break;
      Dec(Nest);
    end
    else if (Current.Kind in [tkFinallyKeyword, tkExceptKeyword]) and
            (Nest = 0) and (DelimiterNest = 0) and (Result.ExpressionTokens.Count > 0) then
      Break
    else if (Current.Kind = tkSemicolon) and (Nest = 0) and (DelimiterNest = 0) then
    begin
      Result.Semicolon := MatchToken(tkSemicolon);
      Break;
    end;

    PreviousIsDot := (Result.ExpressionTokens.Count > 0) and
      (Result.ExpressionTokens[Result.ExpressionTokens.Count - 1].Kind = tkDot);
    if (Nest = 0) and (DelimiterNest = 0) and (Result.ExpressionTokens.Count > 0) and
       not PreviousIsDot and
       (Current.Kind in [tkWhileKeyword, tkForKeyword, tkRepeatKeyword, tkIfKeyword, tkElseKeyword, tkCaseKeyword, tkWithKeyword]) then
      Break;
    Result.ExpressionTokens.Add(NextToken);
  end;
end;

function TParseTreeParser.ParseCaseStatement: TCaseStatementSyntax;
var
  Item: TCaseItemSyntax;
  Nest: Integer;
begin
  Result := TCaseStatementSyntax.Create;
  Result.CaseKeyword := MatchToken(tkCaseKeyword);

  while (Current <> nil) and not (Current.Kind in [tkOfKeyword, tkEOF]) do
    Result.ExpressionTokens.Add(NextToken);

  if (Current <> nil) and (Current.Kind = tkOfKeyword) then
    Result.OfKeyword := MatchToken(tkOfKeyword);

  while (Current <> nil) and not (Current.Kind in [tkEndKeyword, tkElseKeyword, tkEOF]) do
  begin
    Item := TCaseItemSyntax.Create;

    Nest := 0;
    while (Current <> nil) and (Current.Kind <> tkEOF) do
    begin
      if Current.Kind = tkOpenParen then
        Inc(Nest)
      else if Current.Kind = tkCloseParen then
        Dec(Nest);

      if (Current.Kind = tkColon) and (Nest <= 0) then
      begin
        Item.ColonToken := MatchToken(tkColon);
        Break;
      end;

      if Current.Kind in [tkEndKeyword, tkElseKeyword] then
        Break;

      Item.ValueTokens.Add(NextToken);
    end;

    if Assigned(Item.ColonToken) then
      Item.Statement := ParseStatement;

    Result.CaseItems.Add(Item);
  end;

  if (Current <> nil) and (Current.Kind = tkElseKeyword) then
  begin
    Result.ElseKeyword := MatchToken(tkElseKeyword);
    while (Current <> nil) and not (Current.Kind in [tkEndKeyword, tkEOF]) do
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

  while (Current <> nil) and not (Current.Kind in [tkDoKeyword, tkEOF]) do
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
  Nest: Integer;
begin
  Result := TInheritedStatementSyntax.Create;
  Result.InheritedKeyword := MatchToken(tkInheritedKeyword);

  Nest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then
      Inc(Nest)
    else if Current.Kind = tkCloseParen then
      Dec(Nest);

    if (Nest = 0) and (Current.Kind = tkSemicolon) then
    begin
      Result.Semicolon := MatchToken(tkSemicolon);
      Break;
    end;

    if (Nest = 0) and (Current.Kind in [tkEndKeyword, tkElseKeyword,
       tkFinallyKeyword, tkExceptKeyword]) then
      Break;

    Result.CallTokens.Add(NextToken);
  end;
end;

function TParseTreeParser.ParseExitStatement: TExitStatementSyntax;
var
  Nest: Integer;
begin
  Result := TExitStatementSyntax.Create;
  Result.ExitKeyword := MatchToken(tkExitKeyword);

  Nest := 0;
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then
      Inc(Nest)
    else if Current.Kind = tkCloseParen then
      Dec(Nest);

    if (Nest = 0) and (Current.Kind = tkSemicolon) then
    begin
      Result.Semicolon := MatchToken(tkSemicolon);
      Break;
    end;

    if (Nest = 0) and (Current.Kind in [tkEndKeyword, tkElseKeyword,
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
    if Current.Kind in [tkOpenParen, tkOpenBracket] then
      Inc(LDelimiterNest)
    else if Current.Kind in [tkCloseParen, tkCloseBracket] then
    begin
      if LDelimiterNest > 0 then
        Dec(LDelimiterNest);
    end;

    if Current.Kind in [tkBeginKeyword, tkTryKeyword, tkCaseKeyword, tkAsmKeyword] then
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
  CurrentSignatureName     : string;
  CurrentSignatureShortName: string;
  Decl                     : TDeclarationSectionSyntax;
  NestLevel                : Integer;
  Scan                     : Integer;
  Unparsed                 : TUnparsedDeclarationSyntax;

  function IsQualifiedMethodStart(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      ((AToken.Kind in [tkProcedureKeyword, tkFunctionKeyword, tkConstructorKeyword, tkDestructorKeyword]) and
       (Peek(1) <> nil) and (Peek(1).Kind = tkIdentifier) and
       (Peek(2) <> nil) and (Peek(2).Kind = tkDot)) or
      ((AToken.Kind = tkClassKeyword) and (Peek(1) <> nil) and
       ((Peek(1).Kind = tkProcedureKeyword) or (Peek(1).Kind = tkFunctionKeyword) or
        (Peek(1).Kind = tkConstructorKeyword) or (Peek(1).Kind = tkDestructorKeyword) or
        ((Peek(1).Kind = tkIdentifier) and SameText(Peek(1).Text, 'operator'))) and
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
    Scan := FPosition;
    while (Scan < FTokens.Count) and (FTokens[Scan].Kind <> tkEOF) do
    begin
      if FTokens[Scan].Kind = tkDot then
        Exit(True);
      if FTokens[Scan].Kind = tkSemicolon then
        Exit(False);
      Inc(Scan);
    end;
  end;

  function ExtractSignatureNameFromTokens(const ATokens: IList<TSyntaxToken>): string;
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

  function HasConditionalElseInTrivia(AToken: TSyntaxToken): Boolean;
  var
    LTrivia: TSyntaxTrivia;
    LUpper : string;
  begin
    Result := False;
    if (AToken = nil) or (not AToken.HasLeadingTrivia) then Exit;
    for LTrivia in AToken.LeadingTrivia do
    begin
      if (Length(LTrivia.Text) < 7) or (LTrivia.Text[1] <> '{') then Continue;
      LUpper := UpperCase(LTrivia.Text);
      if (Copy(LUpper, 1, 7) = '{$ELSE ') or (Copy(LUpper, 1, 7) = '{$ELSE}') or
         (Copy(LUpper, 1, 9) = '{$ELSEIF ') or (Copy(LUpper, 1, 9) = '{$ELSEIF(') or
         (Copy(LUpper, 1, 9) = '{$ELSEIF}') then
        Exit(True);
    end;
  end;

  function CollectRawBodyTokens(AResult: TMethodImplementationSyntax): Boolean;
  // Raw-collection fallback for method bodies split by {$IFDEF}/{$ELSE}/{$ENDIF}
  // (e.g. an asm branch and a begin/end branch). The caller has determined that
  // the current `end` closes a conditional branch rather than the method, so we
  // preserve the remaining tokens verbatim in BodyTokens until we reach the end
  // that actually terminates the method.
  var
    LNest: Integer;
  begin
    Result := False;
    LNest := 0;

    // The `end` and `;` that would have closed the method belong to a branch body.
    if (Current <> nil) and (Current.Kind = tkEndKeyword) then
      AResult.BodyTokens.Add(NextToken);
    if (Current <> nil) and (Current.Kind = tkSemicolon) then
      AResult.BodyTokens.Add(NextToken);

    while (Current <> nil) and (Current.Kind <> tkEOF) do
    begin
      if IsQualifiedMethodStart(Current) and (LNest = 0) then
        Exit;

      if Current.Kind in [tkBeginKeyword, tkAsmKeyword, tkTryKeyword, tkCaseKeyword] then
      begin
        Inc(LNest);
        AResult.BodyTokens.Add(NextToken);
        Continue;
      end;

      if Current.Kind = tkEndKeyword then
      begin
        if LNest = 0 then
        begin
          // Candidate method end. If the token after the `;` still belongs to a
          // conditional alternate branch, keep collecting; otherwise this is
          // the method's real end.
          if (Peek(1) <> nil) and (Peek(1).Kind = tkSemicolon) and
             (Peek(2) <> nil) and HasConditionalElseInTrivia(Peek(2)) then
          begin
            AResult.BodyTokens.Add(NextToken); // end
            AResult.BodyTokens.Add(NextToken); // ;
            Continue;
          end;
          AResult.EndKeyword := MatchToken(tkEndKeyword);
          if (Current <> nil) and (Current.Kind = tkSemicolon) then
            AResult.FinalSemicolon := MatchToken(tkSemicolon);
          Exit(True);
        end;
        Dec(LNest);
        AResult.BodyTokens.Add(NextToken);
        Continue;
      end;

      AResult.BodyTokens.Add(NextToken);
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
  Unparsed := nil;
  
  if (Current <> nil) and (Current.Kind = tkClassKeyword) then
  begin
    Result.ClassKeyword := NextToken; // consume class
  end;
  
  Result.MethodTypeKeyword := NextToken; // procedure, function, etc.
  
  // consume signature until semicolon
  NestLevel := 0; // Using NestLevel to track parentheses
  while (Current <> nil) and (Current.Kind <> tkEOF) do
  begin
    if Current.Kind = tkOpenParen then Inc(NestLevel)
    else if Current.Kind = tkCloseParen then Dec(NestLevel);
    
    if (Current.Kind = tkSemicolon) and (NestLevel <= 0) then
      Break;
      
    Result.SignatureTokens.Add(NextToken);
  end;
    
  if (Current <> nil) and (Current.Kind = tkSemicolon) then
  begin
    // Keep chained signature segments like "; stdcall;" in the signature stream.
    while (Current <> nil) and (Current.Kind = tkSemicolon) and IsMethodModifierStart(Peek(1)) do
    begin
      Result.SignatureTokens.Add(NextToken); // ;
      while (Current <> nil) and not (Current.Kind in [tkSemicolon, tkEOF]) do
        Result.SignatureTokens.Add(NextToken);
    end;
    Result.SignatureSemicolon := MatchToken(tkSemicolon);
  end;

  CurrentSignatureName := ExtractSignatureNameFromTokens(Result.SignatureTokens);
  CurrentSignatureShortName := CurrentSignatureName;
  if Pos('.', CurrentSignatureShortName) > 0 then
    CurrentSignatureShortName := Copy(CurrentSignatureShortName,
      Pos('.', CurrentSignatureShortName) + 1, MaxInt);
    
  // Local declarations (var, const, type, nested procedures/functions)
  while (Current <> nil) and not (Current.Kind in [tkBeginKeyword, tkAsmKeyword, tkEOF]) do
  begin
    if Current.Kind in [tkVarKeyword, tkConstKeyword, tkTypeKeyword] then
    begin
      Unparsed := nil;
      Decl := ParseDeclarationSection;
      if Assigned(Decl) then
        Result.LocalDeclarations.Add(Decl);
    end
    else if Current.Kind in [tkProcedureKeyword, tkFunctionKeyword] then
    begin
      // Conditional alternative implementation signatures are often fully-qualified
      // and should stay as unparsed local declaration tokens before the shared body.
      if HasDotBeforeSemicolon or
         ((CurrentSignatureName <> '') and
          (SameText(UpcomingSignatureName, CurrentSignatureName) or
           SameText(UpcomingSignatureName, CurrentSignatureShortName))) then
      begin
        NestLevel := 0;
        if Unparsed = nil then
        begin
          Unparsed := TUnparsedDeclarationSyntax.Create;
          Result.LocalDeclarations.Add(Unparsed);
        end;
        while (Current <> nil) and (Current.Kind <> tkEOF) do
        begin
          if Current.Kind = tkOpenParen then
            Inc(NestLevel)
          else if Current.Kind = tkCloseParen then
            Dec(NestLevel)
          else if (Current.Kind = tkSemicolon) and (NestLevel <= 0) then
            Break;
          Unparsed.Tokens.Add(NextToken);
        end;
        if (Current <> nil) and (Current.Kind = tkSemicolon) then
        begin
          Unparsed.Tokens.Add(NextToken);
          while (Current <> nil) and (Current.Kind = tkIdentifier) and
                IsMethodModifierStart(Current) and (Peek(1) <> nil) and (Peek(1).Kind = tkSemicolon) do
          begin
            Unparsed.Tokens.Add(NextToken);
            Unparsed.Tokens.Add(NextToken);
          end;
        end;
      end
      else
      begin
        // Nested procedure/function - parse recursively
        Unparsed := nil;
        Result.LocalDeclarations.Add(ParseMethodImplementation);
      end;
    end
    else
    begin
      if Unparsed = nil then
      begin
        Unparsed := TUnparsedDeclarationSyntax.Create;
        Result.LocalDeclarations.Add(Unparsed);
      end;
      Unparsed.Tokens.Add(NextToken);
      
      // Stop if it's external or forward since it won't have a body
      if (Unparsed.Tokens[Unparsed.Tokens.Count - 1].Kind = tkSemicolon) then
      begin
        var lident := False;
        for var k := 0 to Unparsed.Tokens.Count - 1 do
        begin
          if SameText(Unparsed.Tokens[k].Text, 'external') or SameText(Unparsed.Tokens[k].Text, 'forward') then
          begin
            lident := True;
            Break;
          end;
        end;
        if lident then Break;
      end;
    end;
  end;
  
  if (Current <> nil) and (Current.Kind in [tkBeginKeyword, tkAsmKeyword]) then
  begin
    Result.BeginKeyword := NextToken;
    NestLevel := 1;
  end
  else
    NestLevel := 0;

  // Body until matched end
  while (Current <> nil) and (NestLevel > 0) and (Current.Kind <> tkEOF) do
  begin
    // Recovery: a qualified method header at this level should belong to the
    // implementation section, not to the current method body.
    if IsQualifiedMethodStart(Current) then
      Break;

    if Current.Kind = tkEndKeyword then
    begin
      Dec(NestLevel);
      if NestLevel = 0 then
      begin
        // If the `end;` closes a {$IFDEF} branch and another branch follows
        // via {$ELSE}/{$ELSEIF}, the method continues past this `end;`. Switch
        // to raw-collection mode to preserve all branches in BodyTokens.
        if (Peek(1) <> nil) and (Peek(1).Kind = tkSemicolon) and
           (Peek(2) <> nil) and HasConditionalElseInTrivia(Peek(2)) then
        begin
          CollectRawBodyTokens(Result);
          Exit;
        end;
        Result.EndKeyword := MatchToken(tkEndKeyword);
        Break;
      end;
      // Fall through to parse regular statement if end is nested
    end;

    if Current.Kind in [tkBeginKeyword, tkTryKeyword, tkCaseKeyword, tkAsmKeyword] then
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
  DeclBlockNest: Integer;
  PrevKind     : TTokenKind;
  Unparsed     : TUnparsedDeclarationSyntax;
  
  function IsMethodStartToken(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (AToken.Kind = tkProcedureKeyword) or
      (AToken.Kind = tkFunctionKeyword) or
      ((AToken.Kind = tkConstructorKeyword) and HasDotAfterIdentifierOrGeneric(1)) or
      ((AToken.Kind = tkDestructorKeyword) and HasDotAfterIdentifierOrGeneric(1)) or
      ((AToken.Kind = tkClassKeyword) and (Peek(1) <> nil) and
       ((Peek(1).Kind = tkProcedureKeyword) or (Peek(1).Kind = tkFunctionKeyword) or
        (Peek(1).Kind = tkConstructorKeyword) or (Peek(1).Kind = tkDestructorKeyword) or
        ((Peek(1).Kind = tkIdentifier) and SameText(Peek(1).Text, 'operator')))));
  end;

  function IsQualifiedMethodStart(AToken: TSyntaxToken): Boolean;
  begin
    Result := (AToken <> nil) and (
      (((AToken.Kind = tkProcedureKeyword) or (AToken.Kind = tkFunctionKeyword) or
        (AToken.Kind = tkConstructorKeyword) or (AToken.Kind = tkDestructorKeyword)) and
       HasDotAfterIdentifierOrGeneric(1)) or
      ((AToken.Kind = tkClassKeyword) and (Peek(1) <> nil) and
       ((Peek(1).Kind = tkProcedureKeyword) or (Peek(1).Kind = tkFunctionKeyword) or
        (Peek(1).Kind = tkConstructorKeyword) or (Peek(1).Kind = tkDestructorKeyword) or
        ((Peek(1).Kind = tkIdentifier) and SameText(Peek(1).Text, 'operator'))) and
       HasDotAfterIdentifierOrGeneric(2))
    );
  end;

  function IsForwardClassLikeDecl: Boolean;
  var
    Idx      : Integer;
    ParenNest: Integer;
  begin
    Idx := FPosition + 1; // after class/interface/dispinterface
    ParenNest := 0;

    if (Idx < FTokens.Count) and (FTokens[Idx].Kind = tkOpenParen) then
    begin
      Inc(ParenNest);
      Inc(Idx);
      while (Idx < FTokens.Count) and (FTokens[Idx].Kind <> tkEOF) do
      begin
        if FTokens[Idx].Kind = tkOpenParen then
          Inc(ParenNest)
        else if FTokens[Idx].Kind = tkCloseParen then
        begin
          Dec(ParenNest);
          if ParenNest = 0 then
          begin
            Inc(Idx);
            Break;
          end;
        end;
        Inc(Idx);
      end;
    end;

    Result := (Idx < FTokens.Count) and (FTokens[Idx].Kind = tkSemicolon);
  end;

begin
  Result := TImplementationSectionSyntax.Create;
  Result.ImplementationKeyword := MatchToken(tkImplementationKeyword);
  DeclBlockNest := 0;
  PrevKind := tkUnknown;
  
  if (Current <> nil) and (Current.Kind = tkUsesKeyword) then
  begin
    Result.UsesClause := ParseUsesClause();
  end;
  
  Unparsed := nil;
  // Parse methods and other declarations
  while (Current <> nil) and 
        not ((Current.Kind = tkEndKeyword) and (Peek(1) <> nil) and (Peek(1).Kind = tkDot)) and 
        (Current.Kind <> tkEOF) do
  begin
    if IsQualifiedMethodStart(Current) then
    begin
      DeclBlockNest := 0; // recover from stale nesting in unparsed declaration streams
      Unparsed := nil;
      Result.Declarations.Add(ParseMethodImplementation);
    end
    else if (DeclBlockNest = 0) and IsMethodStartToken(Current) then
    begin
      Unparsed := nil; // Reset unparsed stream
      Result.Declarations.Add(ParseMethodImplementation);
    end
    else if (DeclBlockNest = 0) and (Current.Kind in [tkVarKeyword, tkConstKeyword, tkTypeKeyword]) then
    begin
       Unparsed := nil;
       Result.Declarations.Add(ParseDeclarationSection);
       PrevKind := tkUnknown;
    end
    else
    begin
      if Unparsed = nil then
      begin
        Unparsed := TUnparsedDeclarationSyntax.Create;
        Result.Declarations.Add(Unparsed); // Add instantly to maintain order
      end;

      if (
           ((Current.Kind = tkClassKeyword) and (PrevKind in [tkEquals, tkColon]) and
            ((Peek(1) = nil) or (Peek(1).Kind <> tkOfKeyword)) and not IsForwardClassLikeDecl) or
           ((Current.Kind = tkInterfaceKeyword) and (PrevKind in [tkEquals, tkColon]) and not IsForwardClassLikeDecl) or
           ((Current.Kind = tkDispinterfaceKeyword) and (PrevKind in [tkEquals, tkColon]) and not IsForwardClassLikeDecl) or
           ((Current.Kind = tkRecordKeyword) and ((PrevKind in [tkEquals, tkColon]) or ((PrevKind = tkIdentifier) and (Unparsed.Tokens.Count > 0) and SameText(Unparsed.Tokens[Unparsed.Tokens.Count - 1].Text, 'packed')))) or
           ((Current.Kind = tkCaseKeyword) and (DeclBlockNest > 0))
         ) then
        Inc(DeclBlockNest)
      else if (Current.Kind = tkEndKeyword) and (DeclBlockNest > 0) then
        Dec(DeclBlockNest);

      PrevKind := Current.Kind;
      Unparsed.Tokens.Add(NextToken);
    end;
  end;
end;

function TParseTreeParser.Parse(const AText: string): TCompilationUnitSyntax;
var
  Lexer: TParseTreeLexer;
begin
  Result := TCompilationUnitSyntax.Create;
  Lexer := TParseTreeLexer.Create(AText);
  try
    FTokens := Lexer.TokenizeAll;
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
      while (Current <> nil) and not (Current.Kind in [tkInterfaceKeyword, tkEOF]) do
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
      while (Current <> nil) and not (Current.Kind in [tkImplementationKeyword, tkEOF]) do
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
      FTokens := nil;
    end;
  finally
    Lexer.Free;
  end;
end;

end.
