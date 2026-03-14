// DWScript for formatting Delphi units in the Taifun style

var LastClassName: string;
var LastBannerWasDouble: Boolean;

function StringOfChar(C: String; Count: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Count do
    Result := Result + C;
end;

function PadRight(const S: string; ALen: Integer): string;
begin
  Result := S;
  while Length(Result) < ALen do
    Result := Result + ' ';
end;

function CreateClassBanner(const AClassName: string): string;
var
  LRule: string;
begin
  LRule := '{ ' + StringOfChar('=', 71) + ' }';
  Result := LRule + #13#10 +
            '{ ' + PadRight(AClassName, 71) + ' }' + #13#10 +
            LRule + #13#10;
end;

function CreateMethodBanner: string;
begin
  Result := '{ ' + StringOfChar('-', 71) + ' }' + #13#10;
end;

function CreateNestedMethodBanner: string;
begin
  Result := '{ ' + StringOfChar('-', 26) + ' }' + #13#10;
end;

function CreateSectionBanner(const AName: string): string;
begin
  if AName = '' then
    Result := '{ ' + StringOfChar('=', 71) + ' }' + #13#10
  else
    Result := '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
              AName + #13#10 +
              '{ ' + StringOfChar('=', 71) + ' }' + #13#10;
end;

procedure OnVisitUsesClause(AUses: TUsesClauseSyntax);
var
  LToken, LFirstItem: TSyntaxToken;
  LTrivia: string;
begin
  LToken := GetUsesKeyword(AUses);
  if Assigned(LToken) then
  begin
    LTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);

    // Restore leading trivia exactly as it was after StripBanners processed it,
    // or as it was originally. The previous token (like interface banner or unit declaration)
    // already provides the necessary trailing whitespace (e.g. #13#10#13#10).
    if Length(LTrivia) > 0 then
      AddLeadingTrivia(LToken, LTrivia);

    // The trailing trivia of uses should push the next line down
    AddTrailingTrivia(LToken, #13#10);
    
    LFirstItem := GetUsesFirstItemToken(AUses);
    if Assigned(LFirstItem) then
    begin
      LTrivia := GetLeadingTrivia(LFirstItem);
      ClearTrivia(LFirstItem);
      
      // We want an empty line after the uses keyword, and then some indentation.
      // But we must preserve any existing comments/directives in the trivia.
      // Usually, there might be existing spaces, we just prepend our required spacing.
      // But let's avoid adding too many newlines if they are already there.
      while (Length(LTrivia) > 0) and ((LTrivia[1] = #13) or (LTrivia[1] = #10) or (LTrivia[1] = ' ')) do
        Delete(LTrivia, 1, 1);
        
      AddLeadingTrivia(LFirstItem, #13#10 + '  ' + LTrivia);
    end;
  end;
end;

procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax);
begin
  LastBannerWasDouble := False;
end;

procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax);
begin
  LastBannerWasDouble := False;
end;

procedure OnVisitTypeSection(ASection: TTypeSectionSyntax);
begin
  LastBannerWasDouble := False;
end;

procedure OnVisitConstSection(ASection: TConstSectionSyntax);
begin
  LastBannerWasDouble := False;
end;

procedure OnVisitVarSection(ASection: TVarSectionSyntax);
begin
  LastBannerWasDouble := False;
end;

procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
var
  LClassName: string;
  LToken: TSyntaxToken;
  LOldTrivia, LComments, LLine, S: string;
  P, I: Integer;
  LIsText, LIsBanner: Boolean;
begin
  LClassName := GetMethodClassName(AMethod);
  LToken := GetMethodStartToken(AMethod);

  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);

    LComments := '';
    S := LOldTrivia;
    while Length(S) > 0 do
    begin
      P := Pos(#10, S);
      if P > 0 then
      begin
        LLine := Copy(S, 1, P);
        Delete(S, 1, P);
      end
      else
      begin
        LLine := S;
        S := '';
      end;
      
      LIsText := False;
      for I := 1 to Length(LLine) do
      begin
        if (LLine[I] <> ' ') and (LLine[I] <> #13) and (LLine[I] <> #10) and (LLine[I] <> #9) then
        begin
          LIsText := True;
          Break;
        end;
      end;

      if LIsText then
      begin
        LIsBanner := False;
        if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) then LIsBanner := True;
        
        // Check if the line looks like an old class banner: "{ SomeClassName }"
        // It starts with "{ ", ends with " }", and contains no other text or braces.
        if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) then
        begin
          // A valid banner line usually has lots of trailing spaces padding it to 71 chars.
          if (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then
            LIsBanner := True;
        end;
        
        if not LIsBanner then
          LComments := LComments + LLine;
      end;
    end;

    if GetMethodDepth(AMethod) > 1 then
    begin
      // Nested procedures/functions get a short banner and do NOT affect LastClassName
      AddLeadingTrivia(LToken, #13#10#13#10 + CreateNestedMethodBanner() + #13#10 + LComments);
    end
    else
    begin
      // Top-level methods
      if (LClassName <> '') and (LClassName <> LastClassName) then
      begin
        // If we are right after a double banner (e.g. implementation), its trailing trivia already provided #13#10#13#10.
        // If we add another #13#10#13#10 here, we get too many empty lines.
        if LastBannerWasDouble then
          AddLeadingTrivia(LToken, CreateClassBanner(LClassName) + #13#10 + LComments)
        else
          AddLeadingTrivia(LToken, #13#10#13#10 + CreateClassBanner(LClassName) + #13#10 + LComments);
        LastClassName := LClassName;
      end
      else if (LClassName = '') and (LastClassName <> '') then
      begin
        // Transition from a class method to a global function
        // Reset LastClassName so we don't treat subsequent global functions as new transitions
        LastClassName := '';
        if LastBannerWasDouble then
          AddLeadingTrivia(LToken, CreateSectionBanner('') + #13#10 + LComments)
        else
          AddLeadingTrivia(LToken, #13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10 + LComments);
      end
      else
      begin
        if not LastBannerWasDouble then
          AddLeadingTrivia(LToken, #13#10#13#10 + CreateMethodBanner() + #13#10 + LComments)
        else if (Pos(#10, LOldTrivia) > 0) or (LComments <> '') then
          AddLeadingTrivia(LToken, LComments);
      end;
      // Reset double banner flag after any TOP-LEVEL method implementation banner logic
      LastBannerWasDouble := False;
    end;
  end;
end;

procedure StripBanners(AToken: TSyntaxToken);
var
  LTrivia, LLine, S, LNewTrivia: string;
  P, I: Integer;
  LIsText, LIsBanner: Boolean;
begin
  if not Assigned(AToken) then Exit;
  LTrivia := GetLeadingTrivia(AToken);
  if LTrivia = '' then Exit;

  LNewTrivia := '';
  S := LTrivia;
  while Length(S) > 0 do
  begin
    P := Pos(#10, S);
    if P > 0 then
    begin
      LLine := Copy(S, 1, P);
      Delete(S, 1, P);
    end
    else
    begin
      LLine := S;
      S := '';
    end;

    LIsText := False;
    for I := 1 to Length(LLine) do
    begin
      if (LLine[I] <> ' ') and (LLine[I] <> #13) and (LLine[I] <> #10) and (LLine[I] <> #9) then
      begin
        LIsText := True;
        Break;
      end;
    end;

    LIsBanner := False;
    if LIsText then
    begin
      if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) then LIsBanner := True;
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then LIsBanner := True;
    end;

    if LIsBanner then
    begin
      // Skip the banner, but also try not to duplicate empty lines later.
      // We do nothing here so it's stripped.
    end
    else
    begin
      LNewTrivia := LNewTrivia + LLine;
    end;
  end;

  // We want to keep at most ONE empty line before the next token if there was any spacing.
  // The banner will append #13#10#13#10.
  // The remaining trivia here belongs to the token itself.
  // If we strip all newlines, it glues.
  // Let's strip excess newlines but leave up to ONE #13#10 if it originally existed and wasn't part of the banner.

  // Strip ALL leading newlines first
  while (Length(LNewTrivia) >= 2) and (Copy(LNewTrivia, 1, 2) = #13#10) do
    Delete(LNewTrivia, 1, 2);

  // If the original token had any leading spaces/newlines before we started stripping banners,
  // we restore exactly ONE newline so it isn't glued.
  // LNewTrivia now contains the comments/directives that are NOT banners.
  // We prepend a single newline only if there is actual text left, or if there was an empty line in the original code.
  if (Length(LNewTrivia) > 0) then
    LNewTrivia := #13#10 + LNewTrivia;

  ClearTrivia(AToken);
  if Length(LNewTrivia) > 0 then
    AddLeadingTrivia(AToken, LNewTrivia);
end;

function ExtractComments(const ATrivia: string): string;
var
  S, LLine: string;
  P, I: Integer;
  LIsText, LIsBanner: Boolean;
begin
  Result := '';
  S := ATrivia;
  while Length(S) > 0 do
  begin
    P := Pos(#10, S);
    if P > 0 then
    begin
      LLine := Copy(S, 1, P);
      Delete(S, 1, P);
    end
    else
    begin
      LLine := S;
      S := '';
    end;
    
    LIsText := False;
    for I := 1 to Length(LLine) do
    begin
      if (LLine[I] <> ' ') and (LLine[I] <> #13) and (LLine[I] <> #10) and (LLine[I] <> #9) then
      begin
        LIsText := True;
        Break;
      end;
    end;

    LIsBanner := False;
    if LIsText then
    begin
      if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) then LIsBanner := True;
      
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) then
      begin
        if (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then
          LIsBanner := True;
      end;
    end;
    
    if not LIsBanner then
      Result := Result + LLine;
  end;
  
  // Strip leading/trailing newlines and spaces
  while (Length(Result) > 0) and ((Result[1] = #13) or (Result[1] = #10) or (Result[1] = ' ')) do Delete(Result, 1, 1);
  while (Length(Result) > 0) and ((Result[Length(Result)] = #13) or (Result[Length(Result)] = #10) or (Result[Length(Result)] = ' ')) do Delete(Result, Length(Result), 1);
end;

procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax);
var
  LToken, LNext: TSyntaxToken;
  LComments: string;
  LBanner: string;
begin
  LToken := GetInterfaceKeyword(ASection);
  if Assigned(LToken) then
  begin
    LComments := ExtractComments(GetLeadingTrivia(LToken));
    ClearTrivia(LToken);
    
    LBanner := '{ ' + StringOfChar('=', 71) + ' }' + #13#10;
    
    if LComments <> '' then
      AddLeadingTrivia(LToken, #13#10#13#10 + LComments + #13#10#13#10 + LBanner)
    else
      AddLeadingTrivia(LToken, #13#10 + LBanner);
      
    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10);
    LastBannerWasDouble := True;

    LNext := GetNextToken(LToken);
    if Assigned(LNext) then
      StripBanners(LNext);
  end;
end;

procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax);
var
  LToken, LNext: TSyntaxToken;
  LComments: string;
  LBanner: string;
begin
  LToken := GetImplementationKeyword(ASection);
  if Assigned(LToken) then
  begin
    LComments := ExtractComments(GetLeadingTrivia(LToken));
    ClearTrivia(LToken);
    
    LBanner := '{ ' + StringOfChar('=', 71) + ' }' + #13#10;
    
    if LComments <> '' then
      AddLeadingTrivia(LToken, #13#10#13#10 + LComments + #13#10#13#10 + LBanner)
    else
      AddLeadingTrivia(LToken, #13#10#13#10 + LBanner);

    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10);
    LastBannerWasDouble := True;

    LNext := GetNextToken(LToken);
    if Assigned(LNext) then
      StripBanners(LNext);
  end;
end;

function ExtractHeaderInfo(const ATrivia: string; const AUnitName: string; var ADescription: string; var AAuthor: string; var ADirectives: string): Boolean;
var
  S, LLine: string;
  P, P2, P3: Integer;
  LIsDirective: Boolean;
  LFoundDesc: Boolean;
begin
  Result := Length(ATrivia) > 0;
  LFoundDesc := False;
  
  ADescription := '';
  AAuthor := 'Name';
  ADirectives := '';

  if Result then
  begin
    S := ATrivia;
    while Length(S) > 0 do
    begin
      P := Pos(#10, S);
      if P > 0 then
      begin
        LLine := Copy(S, 1, P - 1);
        if (Length(LLine) > 0) and (LLine[Length(LLine)] = #13) then
          LLine := Copy(LLine, 1, Length(LLine) - 1);
        Delete(S, 1, P);
      end
      else
      begin
        LLine := S;
        S := '';
      end;

      // Check Directives
      LIsDirective := False;
      for P2 := 1 to Length(LLine) do
      begin
        if LLine[P2] <> ' ' then
        begin
          if (LLine[P2] = '{') and (P2 < Length(LLine)) and (LLine[P2+1] = '$') then
            LIsDirective := True;
          Break;
        end;
      end;

      if LIsDirective then
      begin
        if ADirectives <> '' then
          ADirectives := ADirectives + #13#10 + LLine
        else
          ADirectives := LLine;
      end;

      // Check Author
      P2 := Pos('Autor:', LLine);
      if P2 > 0 then
      begin
        AAuthor := Copy(LLine, P2 + 6, Length(LLine));
        // Trim spaces
        while (Length(AAuthor) > 0) and (AAuthor[1] = ' ') do Delete(AAuthor, 1, 1);
        while (Length(AAuthor) > 0) and (AAuthor[Length(AAuthor)] = ' ') do Delete(AAuthor, Length(AAuthor), 1);
      end;

      // Check Description
      // We look for a line starting with '// ' followed by some text, then a hyphen.
      // But we must skip 'Autor:', '====', and empty '//' lines.
      if (Pos('// ', LLine) = 1) and (Pos('// Autor:', LLine) = 0) and (Pos('// ===', LLine) = 0) and (Length(LLine) > 3) then
      begin
        P3 := Pos('-', LLine);
        if P3 > 0 then
        begin
          ADescription := Copy(LLine, P3 + 1, Length(LLine));
          // Trim spaces
          while (Length(ADescription) > 0) and (ADescription[1] = ' ') do Delete(ADescription, 1, 1);
          while (Length(ADescription) > 0) and (ADescription[Length(ADescription)] = ' ') do Delete(ADescription, Length(ADescription), 1);
          LFoundDesc := True;
        end;
      end;
    end;
  end;
  
  if not LFoundDesc and not Result then
    ADescription := 'Kurzbeschreibung der Unit';
  
  if ADirectives = '' then
    ADirectives := '{$I Tfw.Define.pas}';
end;

procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax);
var
  LToken, LSemicolon: TSyntaxToken;
  LUnitName: string;
  LTrivia: string;
  LDesc, LAuthor, LDirectives: string;
  LRule: string;
  LNewBanner: string;
  LDescLine: string;
begin
  LToken := GetUnitKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LUnitName := GetUnitName(AUnit);
    LTrivia := GetLeadingTrivia(LToken);

    ExtractHeaderInfo(LTrivia, LUnitName, LDesc, LAuthor, LDirectives);

    LRule := '// ' + StringOfChar('=', 70);
    LDescLine := '// ' + LUnitName;
    if LDesc <> '' then
      LDescLine := LDescLine + ' - ' + LDesc;

    LNewBanner := LRule + #13#10 +
                  '//' + #13#10 +
                  LDescLine + #13#10 +
                  '//' + #13#10 +
                  '// Autor: ' + LAuthor + #13#10 +
                  '//' + #13#10 +
                  LRule + #13#10 +
                  #13#10 +
                  LDirectives + #13#10 +
                  #13#10;

    if LTrivia <> LNewBanner then
    begin
      ClearTrivia(LToken);
      AddLeadingTrivia(LToken, LNewBanner);
    end;

    // Ensure exactly one empty line after the unit statement (after the semicolon)
    LSemicolon := GetUnitSemicolon(AUnit);
    if Assigned(LSemicolon) then
    begin
      ClearTrivia(LSemicolon);
      AddTrailingTrivia(LSemicolon, #13#10);
    end;
  end;
end;

procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
var
  LToken: TSyntaxToken;
begin
  LToken := GetFinalEndKeyword(AUnit);
  if Assigned(LToken) then
  begin
    ClearTrivia(LToken);
    AddLeadingTrivia(LToken, #13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10);
  end;
end;
