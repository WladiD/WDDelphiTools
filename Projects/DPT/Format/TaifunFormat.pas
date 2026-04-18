// DWScript for formatting Delphi units in the Taifun style

uses

  TaifunFormat.Banner,
  TaifunFormat.ClassDecl,
  TaifunFormat.Header,
  TaifunFormat.Trivia,
  TaifunFormat.UsesSort,
  TaifunFormat.Utils;


type
  TTaifunFormatter = class
  private
    FLastClassName: string;
    FExpectedTokenTextForSuppressedBanner: string;
    FSuppressNextMethodBanner: Boolean;
    FBanner: TTaifunBannerHelper;
    FTrivia: TTaifunTriviaHelper;
    FHeader: TTaifunHeaderHelper;

  public
    constructor Create;
    procedure ClearState;

    procedure FormatUnitStart(AUnit: TCompilationUnitSyntax);
    procedure FormatUnitEnd(AUnit: TCompilationUnitSyntax);
    procedure FormatInterfaceSection(ASection: TInterfaceSectionSyntax);
    procedure FormatImplementationSection(ASection: TImplementationSectionSyntax);
    procedure FormatUsesClause(AUses: TUsesClauseSyntax);
    procedure FormatMethodImplementation(AMethod: TMethodImplementationSyntax);
    procedure FormatConstSection(ASection: TConstSectionSyntax);
    procedure FormatTypeSection(ASection: TTypeSectionSyntax);
    procedure FormatVarSection(ASection: TVarSectionSyntax);
    procedure InsertNestedMethodEndBanner(AMethod: TMethodImplementationSyntax);
  end;

{ TTaifunFormatter }

constructor TTaifunFormatter.Create;
begin
  FBanner := TTaifunBannerHelper.Create;
  FTrivia := TTaifunTriviaHelper.Create;
  FHeader := TTaifunHeaderHelper.Create;
  ClearState;
end;

procedure TTaifunFormatter.ClearState;
begin
  FLastClassName := '';
  FExpectedTokenTextForSuppressedBanner := '';
  FSuppressNextMethodBanner := False;
end;

procedure SortAndFormatUsesItems(AUses: TUsesClauseSyntax);
var
  LCount: Integer;
  I, J: Integer;
  LLower: array of string;
  LGroups: array of Integer;
  LIndices: array of Integer;
  LTempIndex, LTempGroup: Integer;
  LTempLower: string;
  LOrder: string;
  LToken: TSyntaxToken;
  LPrevGroup: Integer;
  LTrivia: string;
begin
  LCount := GetUsesItemCount(AUses);
  if LCount <= 0 then
    Exit;

  // Single unit: just format trivia, no sorting needed
  if LCount = 1 then
  begin
    LToken := GetUsesItemToken(AUses, 0);
    if Assigned(LToken) then
    begin
      LTrivia := TrimLeadingCRLFSpace(GetLeadingTrivia(LToken));
      ClearTrivia(LToken);
      AddLeadingTrivia(LToken, #13#10#13#10 + '  ' + LTrivia);
    end;
    Exit;
  end;

  // Collect names and compute groups
  LLower.SetLength(LCount);
  LGroups.SetLength(LCount);
  LIndices.SetLength(LCount);

  for I := 0 to LCount - 1 do
  begin
    LLower[I] := LowerCase(GetUsesItemName(AUses, I));
    LGroups[I] := GetNamespaceGroup(GetUsesItemName(AUses, I));
    LIndices[I] := I;
  end;

  // Insertion sort by group, then alphabetically within group
  for I := 1 to LCount - 1 do
  begin
    LTempIndex := LIndices[I];
    LTempGroup := LGroups[I];
    LTempLower := LLower[I];
    J := I - 1;
    while (J >= 0) and ((LGroups[J] > LTempGroup) or
      ((LGroups[J] = LTempGroup) and (LLower[J] > LTempLower))) do
    begin
      LIndices[J + 1] := LIndices[J];
      LGroups[J + 1] := LGroups[J];
      LLower[J + 1] := LLower[J];
      Dec(J);
    end;
    LIndices[J + 1] := LTempIndex;
    LGroups[J + 1] := LTempGroup;
    LLower[J + 1] := LTempLower;
  end;

  // Build order string for ReorderUsesItems
  LOrder := '';
  for I := 0 to LCount - 1 do
  begin
    if I > 0 then
      LOrder := LOrder + ',';
    LOrder := LOrder + IntToStr(LIndices[I]);
  end;

  // Reorder AST nodes (also clears all trivia)
  ReorderUsesItems(AUses, LOrder);

  // Apply trivia to each item based on group boundaries
  LPrevGroup := -1;
  for I := 0 to LCount - 1 do
  begin
    LToken := GetUsesItemToken(AUses, I);
    if Assigned(LToken) then
    begin
      if (I = 0) or (LGroups[I] <> LPrevGroup) then
        // Blank line before first item or new group
        AddLeadingTrivia(LToken, #13#10#13#10 + '  ')
      else
        // Just newline + indent within same group
        AddLeadingTrivia(LToken, #13#10 + '  ');
    end;
    LPrevGroup := LGroups[I];
  end;
end;

procedure TTaifunFormatter.FormatUsesClause(AUses: TUsesClauseSyntax);
var
  LToken, LFirstItem: TSyntaxToken;
  LTrivia: string;
begin
  LToken := GetUsesKeyword(AUses);
  if Assigned(LToken) then
  begin
    LTrivia := TrimLeadingCRLFSpace(GetLeadingTrivia(LToken));
    ClearTrivia(LToken);
    AddLeadingTrivia(LToken, #13#10#13#10 + LTrivia);
    AddTrailingTrivia(LToken, '');

    if UsesClauseCanBeSorted(AUses) then
      SortAndFormatUsesItems(AUses)
    else
    begin
      // Fallback: only format first item trivia (preserves directives/comments)
      LFirstItem := GetUsesFirstItemToken(AUses);
      if Assigned(LFirstItem) then
      begin
        LTrivia := TrimLeadingCRLFSpace(GetLeadingTrivia(LFirstItem));
        ClearTrivia(LFirstItem);
        AddLeadingTrivia(LFirstItem, #13#10#13#10 + '  ' + LTrivia);
      end;
    end;
    FExpectedTokenTextForSuppressedBanner := '';
  end;
end;

procedure TTaifunFormatter.FormatMethodImplementation(AMethod: TMethodImplementationSyntax);
var
  LClassName: string;
  LOldTrivia: string;
  LComments: string;
  LTrailingPart: string;
  LIndent: string;
  LToken: TSyntaxToken;
  LIsSuppressed: Boolean;
  LLeadingNewlines: Integer;
  LPrefix: string;
  LBannerText: string;
begin
  if not GetMethodHasBody(AMethod) then Exit;

  LClassName := GetMethodClassName(AMethod);
  LToken := GetMethodStartToken(AMethod);
  if not Assigned(LToken) then Exit;

  var LIsFirstAfterSection: Boolean;
  LIsFirstAfterSection := (FExpectedTokenTextForSuppressedBanner <> '') and (LToken.Text = FExpectedTokenTextForSuppressedBanner);
  LIsSuppressed := LIsFirstAfterSection;
  FExpectedTokenTextForSuppressedBanner := '';

  if FSuppressNextMethodBanner then
  begin
    LIsSuppressed := True;
    FSuppressNextMethodBanner := False;
  end;

  LOldTrivia := GetLeadingTrivia(LToken);
  ClearTrivia(LToken);
  FTrivia.ProcessTrivia(LOldTrivia, LClassName, FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);

  if (Pos('{ -', LComments) > 0) or (Pos('{ =', LComments) > 0) then LIsSuppressed := True;

  // Suppress banner after {$ELSE} directives — the method is just an
  // alternative branch of the same conditional block.
  if Pos('{$ELSE', UpperCase(LTrailingPart)) > 0 then
  begin
    LIsSuppressed := True;
    LPrefix := LTrailingPart + #13#10;
  end
  else
    LPrefix := LTrailingPart + #13#10#13#10;

  if GetMethodDepth() > 1 then
  begin
    if not LIsSuppressed then
      AddLeadingTrivia(LToken, LPrefix + FBanner.CreateNestedMethodBanner() + LComments + LIndent)
    else
      AddLeadingTrivia(LToken, LPrefix + LComments + LIndent);
    InsertNestedMethodEndBanner(AMethod);
  end
  else
  begin
    if (LClassName <> '') and (LClassName <> FLastClassName) then
    begin
      LBannerText := FBanner.CreateClassBanner(LClassName);
      FLastClassName := LClassName;
    end
    else if (LClassName = '') and ((FLastClassName <> '') or (LIsFirstAfterSection and (LTrailingPart <> ''))) then
    begin
      LBannerText := FBanner.CreateSectionBanner('');
      FLastClassName := '';
    end
    else if not LIsSuppressed then
      LBannerText := FBanner.CreateMethodBanner()
    else
      LBannerText := '';

    AddLeadingTrivia(LToken, LPrefix + LBannerText + LComments);
  end;
end;

procedure TTaifunFormatter.FormatConstSection(ASection: TConstSectionSyntax);
var
  LToken: TSyntaxToken;
  LOldTrivia: string;
  LComments: string;
  LTrailingPart: string;
  LIndent: string;
  LLeadingNewlines: Integer;
  LExtractedClass: string;
  S: string;
  LLine: string;
  S2: string;
  P: Integer;
  I: Integer;
  LIsText: Boolean;
  LInBanner: Boolean;
  LPrefix: string;
begin
  LToken := GetConstKeyword(ASection);
  if not Assigned(LToken) then Exit;
  LOldTrivia := GetLeadingTrivia(LToken);
  if LOldTrivia = '' then Exit;

  LExtractedClass := '';
  S := LOldTrivia;
  LInBanner := False;
  while Length(S) > 0 do
  begin
    P := Pos(#10, S);
    if P > 0 then begin LLine := Copy(S, 1, P); Delete(S, 1, P); end
    else begin LLine := S; S := ''; end;

    LIsText := False;
    for I := 1 to Length(LLine) do
      if (LLine[I] <> ' ') and (LLine[I] <> #13) and (LLine[I] <> #10) and (LLine[I] <> #9) then
        begin LIsText := True; Break; end;

    if not LIsText then begin LInBanner := False; Continue; end;
    if Pos('{ ==', LLine) > 0 then begin LInBanner := True; Continue; end;

    if LInBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) then
    begin
      S2 := TrimCommentChars(LLine);
      if (S2 <> '') and (Length(S2) >= 2) and (Pos(S2[1], 'TCIE') > 0) and (S2[2] >= 'A') and (S2[2] <= 'Z') then
      begin
        P := Pos(' - ', S2);
        if P > 0 then S2 := Copy(S2, 1, P - 1);
        LExtractedClass := S2;
      end;
      LInBanner := False;
    end;
  end;

  if LExtractedClass = '' then Exit;

  ClearTrivia(LToken);
  FTrivia.ProcessTrivia(LOldTrivia, '', FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);
  LPrefix := LTrailingPart + #13#10#13#10;
  AddLeadingTrivia(LToken, LPrefix + FBanner.CreateClassBanner(LExtractedClass) + LComments);
  FLastClassName := LExtractedClass;
  FSuppressNextMethodBanner := True;
end;

procedure TTaifunFormatter.FormatTypeSection(ASection: TTypeSectionSyntax);
var
  LToken: TSyntaxToken;
  LOldTrivia, LComments, LTrailingPart, LIndent, LPrefix: string;
  LLeadingNewlines: Integer;
begin
  if not IsUnitLevel(ASection) then Exit;
  LToken := GetTypeKeyword(ASection);
  if not Assigned(LToken) then Exit;
  
  LOldTrivia := GetLeadingTrivia(LToken);
  ClearTrivia(LToken);
  FTrivia.ProcessTrivia(LOldTrivia, '', FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);
  
  LPrefix := LTrailingPart + #13#10#13#10;
  if (FExpectedTokenTextForSuppressedBanner = LToken.Text) or
     (Pos('{ -', LComments) > 0) or (Pos('{ =', LComments) > 0) then
    AddLeadingTrivia(LToken, LPrefix + LComments + LIndent)
  else
    AddLeadingTrivia(LToken, LPrefix + FBanner.CreateMethodBanner() + LComments + LIndent);

  FLastClassName := '';
  FExpectedTokenTextForSuppressedBanner := '';
end;

procedure SortAndAlignVarDecls(ASection: TVarSectionSyntax);
var
  LCount: Integer;
  I, J: Integer;
  LNames: array of string;
  LLower: array of string;
  LAbsTargets: array of string;
  LIndices: array of Integer;
  LTempIndex: Integer;
  LTempLower: string;
  LOrder: string;
  LMaxLen: Integer;
  LPadding: Integer;
  LPadStr: string;
  LIdToken: TSyntaxToken;
  LColonToken: TSyntaxToken;
  LTypeToken: TSyntaxToken;
  LIndent: string;
begin
  // Declarations of a method-level var section are indented by the method
  // nesting depth times 2 spaces (depth 1 = 2 spaces, depth 2 = 4 spaces, ...).
  LIndent := GetSep(' ', GetMethodDepth() * 2);

  LCount := GetVarDeclCount(ASection);
  if LCount <= 0 then
    Exit;

  // Collect names and absolute targets
  LNames.SetLength(LCount);
  LLower.SetLength(LCount);
  LAbsTargets.SetLength(LCount);
  LIndices.SetLength(LCount);

  for I := 0 to LCount - 1 do
  begin
    LNames[I] := GetVarDeclName(ASection, I);
    LLower[I] := LowerCase(LNames[I]);
    LAbsTargets[I] := LowerCase(GetVarDeclAbsoluteTarget(ASection, I));
    LIndices[I] := I;
  end;

  // Insertion sort alphabetically
  for I := 1 to LCount - 1 do
  begin
    LTempIndex := LIndices[I];
    LTempLower := LLower[I];
    J := I - 1;
    while (J >= 0) and (LLower[J] > LTempLower) do
    begin
      LIndices[J + 1] := LIndices[J];
      LLower[J + 1] := LLower[J];
      Dec(J);
    end;
    LIndices[J + 1] := LTempIndex;
    LLower[J + 1] := LTempLower;
  end;

  // Fix absolute dependencies: if a var references another local var via
  // absolute, it must come directly after that var.
  for I := 0 to LCount - 1 do
  begin
    if LAbsTargets[LIndices[I]] <> '' then
    begin
      // Find the referenced variable in the sorted order
      for J := 0 to I - 1 do
      begin
        if LLower[J] = LAbsTargets[LIndices[I]] then
        begin
          // Move I to position J+1 if not already there
          if I <> J + 1 then
          begin
            LTempIndex := LIndices[I];
            LTempLower := LLower[I];
            var K: Integer := I;
            while K > J + 1 do
            begin
              LIndices[K] := LIndices[K - 1];
              LLower[K] := LLower[K - 1];
              Dec(K);
            end;
            LIndices[J + 1] := LTempIndex;
            LLower[J + 1] := LTempLower;
          end;
          Break;
        end;
      end;
    end;
  end;

  // Build order string
  LOrder := '';
  for I := 0 to LCount - 1 do
  begin
    if I > 0 then
      LOrder := LOrder + ',';
    LOrder := LOrder + IntToStr(LIndices[I]);
  end;

  // Reorder AST nodes
  ReorderVarDecls(ASection, LOrder);

  // Calculate max name length for colon alignment
  LMaxLen := 0;
  for I := 0 to LCount - 1 do
  begin
    var LLen: Integer := Length(GetVarDeclName(ASection, I));
    if LLen > LMaxLen then
      LMaxLen := LLen;
  end;

  // Apply trivia to each declaration for proper indentation and alignment
  for I := 0 to LCount - 1 do
  begin
    LIdToken := GetVarDeclIdentifier(ASection, I);
    LColonToken := GetVarDeclColonToken(ASection, I);
    LTypeToken := GetVarDeclTypeToken(ASection, I);

    if Assigned(LIdToken) then
    begin
      ClearTrivia(LIdToken);
      AddLeadingTrivia(LIdToken, #13#10 + LIndent);
    end;

    if Assigned(LColonToken) then
    begin
      // Padding: longest name + 1 space, minus this name's length
      LPadding := LMaxLen - Length(GetVarDeclName(ASection, I));
      LPadStr := '';
      var P: Integer;
      for P := 1 to LPadding do
        LPadStr := LPadStr + ' ';

      ClearTrivia(LColonToken);
      AddLeadingTrivia(LColonToken, LPadStr);
    end;

    if Assigned(LTypeToken) then
    begin
      ClearTrivia(LTypeToken);
      AddLeadingTrivia(LTypeToken, ' ');
    end;
  end;
end;

procedure TTaifunFormatter.FormatVarSection(ASection: TVarSectionSyntax);
var
  LToken: TSyntaxToken;
  LOldTrivia, LComments, LTrailingPart, LIndent, LPrefix: string;
  LLeadingNewlines: Integer;
begin
  if not IsUnitLevel(ASection) then
  begin
    // Method-level var: fix trivia, split multi-var lines, then sort and align
    FixVarDeclTrailingComments(ASection);
    SplitMultiVarDeclarations(ASection);
    if VarSectionCanBeFormatted(ASection) then
      SortAndAlignVarDecls(ASection);
    Exit;
  end;

  // Unit-level var: banner handling (existing logic)
  LToken := GetVarKeyword(ASection);
  if not Assigned(LToken) then Exit;

  LOldTrivia := GetLeadingTrivia(LToken);
  ClearTrivia(LToken);
  FTrivia.ProcessTrivia(LOldTrivia, '', FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);

  LPrefix := LTrailingPart + #13#10#13#10;
  if (FExpectedTokenTextForSuppressedBanner = LToken.Text) or
     (Pos('{ -', LComments) > 0) or (Pos('{ =', LComments) > 0) then
    AddLeadingTrivia(LToken, LPrefix + LComments + LIndent)
  else
    AddLeadingTrivia(LToken, LPrefix + FBanner.CreateMethodBanner() + LComments + LIndent);

  FLastClassName := '';
  FExpectedTokenTextForSuppressedBanner := '';
end;

procedure TTaifunFormatter.InsertNestedMethodEndBanner(AMethod: TMethodImplementationSyntax);
var
  LEndToken: TSyntaxToken;
  LNextToken: TSyntaxToken;
  LNextText: string;
  LNextOldTrivia: string;
  LNextComments: string;
  LNextTrailingPart: string;
  LNextIndent: string;
  LNextLeadingNewlines: Integer;
begin
  LEndToken := GetMethodEndToken(AMethod);
  if not Assigned(LEndToken) then Exit;

  LNextToken := GetNextToken(LEndToken);
  if not Assigned(LNextToken) then Exit;

  LNextText := LowerCase(LNextToken.Text);
  if (LNextText = 'procedure') or (LNextText = 'function') or (LNextText = 'constructor') or (LNextText = 'destructor') or (LNextText = 'class') then
    Exit;

  LNextOldTrivia := GetLeadingTrivia(LNextToken);
  ClearTrivia(LNextToken);
  FTrivia.ProcessTrivia(LNextOldTrivia, '', FLastClassName, LNextTrailingPart, LNextComments, LNextLeadingNewlines, LNextIndent);

  AddLeadingTrivia(LNextToken, LNextTrailingPart + #13#10#13#10 + FBanner.CreateNestedMethodBanner() + LNextComments + LNextIndent);
end;

procedure TTaifunFormatter.FormatInterfaceSection(ASection: TInterfaceSectionSyntax);
var
  LToken: TSyntaxToken;
  LNext: TSyntaxToken;
  LComments: string;
  LTrailingPart: string;
  LOldTrivia: string;
  LBanner: string;
  LPrefix: string;
  LIndent: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetInterfaceKeyword(ASection);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    FTrivia.ProcessTrivia(LOldTrivia, '', FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);
    LBanner := '{ ' + GetSep('=', 71) + ' }' + #13#10;
    
    LPrefix := #13#10; if LLeadingNewlines >= 2 then LPrefix := #13#10#13#10;
    
    if LTrailingPart <> '' then 
    begin
       if LComments <> '' then 
       begin
          while (Length(LComments) > 0) and ((LComments[Length(LComments)] = #13) or (LComments[Length(LComments)] = #10)) do Delete(LComments, Length(LComments), 1);
          AddLeadingTrivia(LToken, LTrailingPart + LPrefix + LComments + #13#10#13#10 + LBanner);
       end
       else AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LBanner);
    end
    else 
    begin
       if LComments <> '' then 
       begin
          while (Length(LComments) > 0) and ((LComments[Length(LComments)] = #13) or (LComments[Length(LComments)] = #10)) do Delete(LComments, Length(LComments), 1);
          AddLeadingTrivia(LToken, LPrefix + LComments + #13#10#13#10 + LBanner);
       end
       else AddLeadingTrivia(LToken, #13#10#13#10 + LBanner);
    end;
    
    AddTrailingTrivia(LToken, #13#10 + '{ ' + GetSep('=', 71) + ' }');
    FLastClassName := '';
    LNext := GetNextToken(LToken); if Assigned(LNext) then FExpectedTokenTextForSuppressedBanner := FBanner.StripBanners(LNext);
  end;
end;

procedure TTaifunFormatter.FormatImplementationSection(ASection: TImplementationSectionSyntax);
var
  LToken: TSyntaxToken;
  LNext: TSyntaxToken;
  LComments: string;
  LTrailingPart: string;
  LOldTrivia: string;
  LBanner: string;
  LPrefix: string;
  LIndent: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetImplementationKeyword(ASection);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    FTrivia.ProcessTrivia(LOldTrivia, '', FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);
    LBanner := '{ ' + GetSep('=', 71) + ' }' + #13#10;
    
    LPrefix := #13#10; if LLeadingNewlines >= 2 then LPrefix := #13#10#13#10;
    
    if LTrailingPart <> '' then 
    begin
       if LComments <> '' then 
       begin
          while (Length(LComments) > 0) and ((LComments[Length(LComments)] = #13) or (LComments[Length(LComments)] = #10)) do Delete(LComments, Length(LComments), 1);
          AddLeadingTrivia(LToken, LTrailingPart + LPrefix + LComments + #13#10#13#10 + LBanner);
       end
       else AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LBanner);
    end
    else 
    begin
       if LComments <> '' then 
       begin
          while (Length(LComments) > 0) and ((LComments[Length(LComments)] = #13) or (LComments[Length(LComments)] = #10)) do Delete(LComments, Length(LComments), 1);
          AddLeadingTrivia(LToken, LPrefix + LComments + #13#10#13#10 + LBanner);
       end
       else AddLeadingTrivia(LToken, #13#10#13#10 + LBanner);
    end;
    
    AddTrailingTrivia(LToken, #13#10 + '{ ' + GetSep('=', 71) + ' }');
    FLastClassName := '';
    LNext := GetNextToken(LToken); if Assigned(LNext) then FExpectedTokenTextForSuppressedBanner := FBanner.StripBanners(LNext);
  end;
end;

procedure TTaifunFormatter.FormatUnitStart(AUnit: TCompilationUnitSyntax);
var
  LToken: TSyntaxToken;
  LSemicolon: TSyntaxToken;
  LUnitName: string;
  LTrivia: string;
  LDesc: string;
  LAuthor: string;
  LDirectives: string;
  LRule: string;
  LNewBanner: string;
  LDescLine: string;
  LExtra: string;
begin
  LToken := GetUnitKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LUnitName := GetUnitName(AUnit); LTrivia := GetLeadingTrivia(LToken);
    FHeader.ExtractHeaderInfo(LTrivia, LUnitName, LDesc, LAuthor, LDirectives, LExtra);
    LRule := '// ' + GetSep('=', 70); LDescLine := '// ' + LUnitName; if LDesc <> '' then LDescLine := LDescLine + ' - ' + LDesc;
    LNewBanner := LRule + #13#10 + '//' + #13#10 + LDescLine + #13#10 + '//' + #13#10 + '// Autor: ' + LAuthor + #13#10 + '//';
    if LExtra <> '' then LNewBanner := LNewBanner + #13#10 + LExtra + #13#10 + '//';
    LNewBanner := LNewBanner + #13#10 + LRule + #13#10 + #13#10 + LDirectives + #13#10 + #13#10;
    ClearTrivia(LToken); AddLeadingTrivia(LToken, LNewBanner);
    LSemicolon := GetUnitSemicolon(AUnit); if Assigned(LSemicolon) then begin ClearTrivia(LSemicolon); AddTrailingTrivia(LSemicolon, ''); end;
  end;
end;

procedure TTaifunFormatter.FormatUnitEnd(AUnit: TCompilationUnitSyntax);
var
  LToken: TSyntaxToken;
  LComments: string;
  LTrailingPart: string;
  LOldTrivia: string;
  LIndent: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetFinalEndKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    FTrivia.ProcessTrivia(LOldTrivia, '', FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);

    if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LComments + '{ ' + GetSep('=', 71) + ' }' + #13#10#13#10)
    else AddLeadingTrivia(LToken, #13#10#13#10 + LComments + '{ ' + GetSep('=', 71) + ' }' + #13#10#13#10);
  end;
end;

// -----------------------------------------------------------------------
// Global Instance and Entry Points
// -----------------------------------------------------------------------

var GlobalFormatter: TTaifunFormatter;

function GetFormatter: TTaifunFormatter;
begin
  if not Assigned(GlobalFormatter) then
    GlobalFormatter := TTaifunFormatter.Create;
  Result := GlobalFormatter;
end;

procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax);
begin
  GetFormatter.ClearState;
  GetFormatter.FormatUnitStart(AUnit);
end;

procedure OnVisitUsesClause(AUses: TUsesClauseSyntax);
begin
  GetFormatter.FormatUsesClause(AUses);
end;

procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax);
begin
  GetFormatter.FormatInterfaceSection(ASection);
end;

procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax);
begin
  GetFormatter.FormatImplementationSection(ASection);
end;

procedure OnVisitConstSection(ASection: TConstSectionSyntax);
begin
  GetFormatter.FormatConstSection(ASection);
end;

procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
begin
  GetFormatter.FormatMethodImplementation(AMethod);
end;

procedure OnVisitTypeSection(ASection: TTypeSectionSyntax);
begin
  GetFormatter.FormatTypeSection(ASection);
end;

procedure OnVisitVarSection(ASection: TVarSectionSyntax);
begin
  GetFormatter.FormatVarSection(ASection);
end;

procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax);
begin
  FormatClassDeclaration(AClass);
end;

procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
begin
  GetFormatter.FormatUnitEnd(AUnit);
end;
