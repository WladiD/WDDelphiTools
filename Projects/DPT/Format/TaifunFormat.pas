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
    if Length(LTrivia) > 0 then AddLeadingTrivia(LToken, LTrivia);
    AddTrailingTrivia(LToken, #13#10);
    
    LFirstItem := GetUsesFirstItemToken(AUses);
    if Assigned(LFirstItem) then
    begin
      LTrivia := GetLeadingTrivia(LFirstItem);
      ClearTrivia(LFirstItem);
      while (Length(LTrivia) > 0) and ((LTrivia[1] = #13) or (LTrivia[1] = #10) or (LTrivia[1] = ' ')) do
        Delete(LTrivia, 1, 1);
      AddLeadingTrivia(LFirstItem, #13#10 + '  ' + LTrivia);
    end;
  end;
end;

procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax); begin LastBannerWasDouble := False; end;
procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax); begin LastBannerWasDouble := False; end;
procedure OnVisitTypeSection(ASection: TTypeSectionSyntax); begin LastBannerWasDouble := False; end;
procedure OnVisitConstSection(ASection: TConstSectionSyntax); begin LastBannerWasDouble := False; end;
procedure OnVisitVarSection(ASection: TVarSectionSyntax); begin LastBannerWasDouble := False; end;

procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
var
  LClassName, LOldTrivia, S, LLine, LComments, LTrailingPart, S2: string;
  LToken: TSyntaxToken;
  P, I: Integer;
  LIsText, LIsBanner, LFirstLine: Boolean;
begin
  LClassName := GetMethodClassName(AMethod);
  LToken := GetMethodStartToken(AMethod);
  if not Assigned(LToken) then Exit;

  LOldTrivia := GetLeadingTrivia(LToken);
  ClearTrivia(LToken);

  LComments := '';
  LTrailingPart := '';
  S := LOldTrivia;
  LFirstLine := True;

  while Length(S) > 0 do
  begin
    P := Pos(#10, S);
    if P > 0 then begin LLine := Copy(S, 1, P); Delete(S, 1, P); end
    else begin LLine := S; S := ''; end;

    if LFirstLine and (Length(LOldTrivia) > 0) and (LOldTrivia[1] <> #13) and (LOldTrivia[1] <> #10) then
    begin
       LFirstLine := False;
       I := 1;
       while (I <= Length(LLine)) and ((LLine[I] = ' ') or (LLine[I] = #9)) do Inc(I);
       if (I <= Length(LLine)) and ((LLine[I] = '/') or (LLine[I] = '{')) then
       begin
          if (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) and (Pos('{$', LLine) = 0) and
             (Pos('{ ==', LLine) = 0) and (Pos('{ --', LLine) = 0) then
          begin
             S2 := LLine;
             while (Length(S2) > 0) and ((S2[1] = '/') or (S2[1] = '{') or (S2[1] = ' ')) do Delete(S2, 1, 1);
             while (Length(S2) > 0) and ((S2[Length(S2)] = '}') or (S2[Length(S2)] = #13) or (S2[Length(S2)] = #10) or (S2[Length(S2)] = ' ')) do Delete(S2, Length(S2), 1);
             if (LClassName = '') or (S2 <> LClassName) then
             begin
                LTrailingPart := LLine;
                Continue;
             end;
          end;
       end;
    end;
    LFirstLine := False;

    LIsText := False;
    for I := 1 to Length(LLine) do
      if (LLine[I] <> ' ') and (LLine[I] <> #13) and (LLine[I] <> #10) and (LLine[I] <> #9) then
        begin LIsText := True; Break; end;

    if LIsText then
    begin
      LIsBanner := False;
      if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) then LIsBanner := True;
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then LIsBanner := True;
      if not LIsBanner and (LClassName <> '') then
      begin
        S2 := LLine;
        while (Length(S2) > 0) and ((S2[1] = '/') or (S2[1] = '{') or (S2[1] = ' ')) do Delete(S2, 1, 1);
        while (Length(S2) > 0) and ((S2[Length(S2)] = '}') or (S2[Length(S2)] = #13) or (S2[Length(S2)] = #10) or (S2[Length(S2)] = ' ')) do Delete(S2, Length(S2), 1);
        if S2 = LClassName then LIsBanner := True;
      end;
      if not LIsBanner then LComments := LComments + LLine;
    end;
  end;

  if GetMethodDepth(AMethod) > 1 then
  begin
    if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10 + CreateNestedMethodBanner() + #13#10 + LComments)
    else AddLeadingTrivia(LToken, #13#10#13#10 + CreateNestedMethodBanner() + #13#10 + LComments);
  end
  else
  begin
    if (LClassName <> '') and (LClassName <> LastClassName) then
    begin
      if LastBannerWasDouble then AddLeadingTrivia(LToken, LTrailingPart + CreateClassBanner(LClassName) + #13#10 + LComments)
      else if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10 + CreateClassBanner(LClassName) + #13#10 + LComments)
      else AddLeadingTrivia(LToken, #13#10#13#10 + CreateClassBanner(LClassName) + #13#10 + LComments);
      LastClassName := LClassName;
    end
    else if (LClassName = '') and (LastClassName <> '') then
    begin
      LastClassName := '';
      if LastBannerWasDouble then AddLeadingTrivia(LToken, LTrailingPart + CreateSectionBanner('') + #13#10 + LComments)
      else if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10 + CreateSectionBanner('') + #13#10 + LComments)
      else AddLeadingTrivia(LToken, #13#10#13#10 + CreateSectionBanner('') + #13#10 + LComments);
    end
    else
    begin
      if not LastBannerWasDouble then
      begin
        if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10 + CreateMethodBanner() + #13#10 + LComments)
        else AddLeadingTrivia(LToken, #13#10#13#10 + CreateMethodBanner() + #13#10 + LComments);
      end
      else AddLeadingTrivia(LToken, LTrailingPart + LComments);
    end;
    LastBannerWasDouble := False;
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
  LNewTrivia := ''; S := LTrivia;
  while Length(S) > 0 do
  begin
    P := Pos(#10, S);
    if P > 0 then begin LLine := Copy(S, 1, P); Delete(S, 1, P); end else begin LLine := S; S := ''; end;
    LIsText := False;
    for I := 1 to Length(LLine) do if (LLine[I] <> ' ') and (LLine[I] <> #13) and (LLine[I] <> #10) and (LLine[I] <> #9) then begin LIsText := True; Break; end;
    LIsBanner := False;
    if LIsText then
    begin
      if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) then LIsBanner := True;
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then LIsBanner := True;
    end;
    if not LIsBanner then LNewTrivia := LNewTrivia + LLine;
  end;
  while (Length(LNewTrivia) >= 2) and (Copy(LNewTrivia, 1, 2) = #13#10) do Delete(LNewTrivia, 1, 2);
  ClearTrivia(AToken);
  if Length(LNewTrivia) > 0 then AddLeadingTrivia(AToken, LNewTrivia);
end;

function ExtractComments(const ATrivia: string): string;
var
  S, LLine: string;
  P, I: Integer;
  LIsText, LIsBanner: Boolean;
begin
  Result := ''; S := ATrivia;
  while Length(S) > 0 do
  begin
    P := Pos(#10, S);
    if P > 0 then begin LLine := Copy(S, 1, P); Delete(S, 1, P); end else begin LLine := S; S := ''; end;
    LIsText := False;
    for I := 1 to Length(LLine) do if (LLine[I] <> ' ') and (LLine[I] <> #13) and (LLine[I] <> #10) and (LLine[I] <> #9) then begin LIsText := True; Break; end;
    LIsBanner := False;
    if LIsText then
    begin
      if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) then LIsBanner := True;
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) then
      begin
        if (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then LIsBanner := True;
      end;
    end;
    if not LIsBanner then Result := Result + LLine;
  end;
  while (Length(Result) > 0) and ((Result[1] = #13) or (Result[1] = #10) or (Result[1] = ' ')) do Delete(Result, 1, 1);
  while (Length(Result) > 0) and ((Result[Length(Result)] = #13) or (Result[Length(Result)] = #10) or (Result[Length(Result)] = ' ')) do Delete(Result, Length(Result), 1);
end;

procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax);
var LToken, LNext: TSyntaxToken; LComments, LBanner: string;
begin
  LToken := GetInterfaceKeyword(ASection);
  if Assigned(LToken) then
  begin
    LComments := ExtractComments(GetLeadingTrivia(LToken));
    ClearTrivia(LToken);
    LBanner := '{ ' + StringOfChar('=', 71) + ' }' + #13#10;
    if LComments <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LComments + #13#10#13#10 + LBanner)
    else AddLeadingTrivia(LToken, #13#10 + LBanner);
    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10);
    LastBannerWasDouble := True;
    LNext := GetNextToken(LToken); if Assigned(LNext) then StripBanners(LNext);
  end;
end;

procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax);
var LToken, LNext: TSyntaxToken; LComments, LBanner: string;
begin
  LToken := GetImplementationKeyword(ASection);
  if Assigned(LToken) then
  begin
    LComments := ExtractComments(GetLeadingTrivia(LToken));
    ClearTrivia(LToken);
    LBanner := '{ ' + StringOfChar('=', 71) + ' }' + #13#10;
    if LComments <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LComments + #13#10#13#10 + LBanner)
    else AddLeadingTrivia(LToken, #13#10#13#10 + LBanner);
    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10);
    LastBannerWasDouble := True;
    LNext := GetNextToken(LToken); if Assigned(LNext) then StripBanners(LNext);
  end;
end;

function ExtractHeaderInfo(const ATrivia: string; const AUnitName: string; var ADescription: string; var AAuthor: string; var ADirectives: string): Boolean;
var S, LLine: string; P, P2, P3: Integer; LIsDirective, LFoundDesc: Boolean;
begin
  Result := Length(ATrivia) > 0; LFoundDesc := False; ADescription := ''; AAuthor := 'Name'; ADirectives := '';
  if Result then
  begin
    S := ATrivia;
    while Length(S) > 0 do
    begin
      P := Pos(#10, S);
      if P > 0 then begin LLine := Copy(S, 1, P - 1); if (Length(LLine) > 0) and (LLine[Length(LLine)] = #13) then LLine := Copy(LLine, 1, Length(LLine) - 1); Delete(S, 1, P); end else begin LLine := S; S := ''; end;
      LIsDirective := False;
      for P2 := 1 to Length(LLine) do if LLine[P2] <> ' ' then begin if (LLine[P2] = '{') and (P2 < Length(LLine)) and (LLine[P2+1] = '$') then LIsDirective := True; Break; end;
      if LIsDirective then begin if ADirectives <> '' then ADirectives := ADirectives + #13#10 + LLine else ADirectives := LLine; end;
      P2 := Pos('Autor:', LLine); if P2 > 0 then begin AAuthor := Copy(LLine, P2 + 6, Length(LLine)); while (Length(AAuthor) > 0) and (AAuthor[1] = ' ') do Delete(AAuthor, 1, 1); while (Length(AAuthor) > 0) and (AAuthor[Length(AAuthor)] = ' ') do Delete(AAuthor, Length(AAuthor), 1); end;
      if (Pos('// ', LLine) = 1) and (Pos('// Autor:', LLine) = 0) and (Pos('// ===', LLine) = 0) and (Length(LLine) > 3) then
      begin
        P3 := Pos('-', LLine); if P3 > 0 then begin ADescription := Copy(LLine, P3 + 1, Length(LLine)); while (Length(ADescription) > 0) and (ADescription[1] = ' ') do Delete(ADescription, 1, 1); while (Length(ADescription) > 0) and (ADescription[Length(ADescription)] = ' ') do Delete(ADescription, Length(ADescription), 1); LFoundDesc := True; end;
      end;
    end;
  end;
  if not LFoundDesc and not Result then ADescription := 'Kurzbeschreibung der Unit';
  if ADirectives = '' then ADirectives := '{$I Tfw.Define.pas}';
end;

procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax);
var LToken, LSemicolon: TSyntaxToken; LUnitName, LTrivia, LDesc, LAuthor, LDirectives, LRule, LNewBanner, LDescLine: string;
begin
  LToken := GetUnitKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LUnitName := GetUnitName(AUnit); LTrivia := GetLeadingTrivia(LToken);
    ExtractHeaderInfo(LTrivia, LUnitName, LDesc, LAuthor, LDirectives);
    LRule := '// ' + StringOfChar('=', 70); LDescLine := '// ' + LUnitName; if LDesc <> '' then LDescLine := LDescLine + ' - ' + LDesc;
    LNewBanner := LRule + #13#10 + '//' + #13#10 + LDescLine + #13#10 + '//' + #13#10 + '// Autor: ' + LAuthor + #13#10 + '//' + #13#10 + LRule + #13#10 + #13#10 + LDirectives + #13#10 + #13#10;
    if LTrivia <> LNewBanner then begin ClearTrivia(LToken); AddLeadingTrivia(LToken, LNewBanner); end;
    LSemicolon := GetUnitSemicolon(AUnit); if Assigned(LSemicolon) then begin ClearTrivia(LSemicolon); AddTrailingTrivia(LSemicolon, #13#10); end;
  end;
end;

procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
var LToken: TSyntaxToken; LComments: string;
begin
  LToken := GetFinalEndKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LComments := ExtractComments(GetLeadingTrivia(LToken)); ClearTrivia(LToken);
    if LComments <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LComments + #13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10)
    else AddLeadingTrivia(LToken, #13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10);
  end;
end;
