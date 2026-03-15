// DWScript for formatting Delphi units in the Taifun style

var LastClassName: string;
var ExpectedTokenTextForSuppressedBanner: string;

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
            LRule + #13#10 + #13#10;
end;

function CreateMethodBanner: string;
begin
  Result := '{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10;
end;

function CreateNestedMethodBanner: string;
begin
  Result := '{ ' + StringOfChar('-', 26) + ' }' + #13#10 + #13#10;
end;

function CreateSectionBanner(const AName: string): string;
begin
  if AName = '' then
    Result := '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10
  else
    Result := '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
              AName + #13#10 +
              '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10;
end;

procedure ProcessTrivia(const AOldTrivia: string; const AClassName: string; var ATrailingPart, AComments: string; var ALeadingNewlines: Integer);
var
  S, LLine, S2: string;
  P, I: Integer;
  LIsText, LIsBanner, LCollectingForPrevious: Boolean;
begin
  AComments := '';
  ATrailingPart := '';
  S := AOldTrivia;
  
  ALeadingNewlines := 0;
  for I := 1 to Length(AOldTrivia) do
  begin
    if AOldTrivia[I] = #10 then ALeadingNewlines := ALeadingNewlines + 1;
    if (AOldTrivia[I] <> ' ') and (AOldTrivia[I] <> #13) and (AOldTrivia[I] <> #10) and (AOldTrivia[I] <> #9) then Break;
  end;

  LCollectingForPrevious := True;

  while Length(S) > 0 do
  begin
    P := Pos(#10, S);
    if P > 0 then begin LLine := Copy(S, 1, P); Delete(S, 1, P); end
    else begin LLine := S; S := ''; end;

    LIsText := False;
    for I := 1 to Length(LLine) do
      if (LLine[I] <> ' ') and (LLine[I] <> #13) and (LLine[I] <> #10) and (LLine[I] <> #9) then
        begin LIsText := True; Break; end;

    if LIsText then
    begin
      LIsBanner := False;
      if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) then LIsBanner := True;
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then 
      begin
        S2 := LLine;
        while (Length(S2) > 0) and ((S2[1] = '/') or (S2[1] = '{') or (S2[1] = ' ')) do Delete(S2, 1, 1);
        while (Length(S2) > 0) and ((S2[Length(S2)] = '}') or (S2[Length(S2)] = #13) or (S2[Length(S2)] = #10) or (S2[Length(S2)] = ' ')) do Delete(S2, Length(S2), 1);
        
        if S2 = '' then LIsBanner := True
        else if (AClassName <> '') and ((S2 = AClassName) or (Pos(AClassName + ' ', S2) = 1) or (Pos(AClassName + '.', S2) = 1)) then LIsBanner := True
        else if (AClassName = '') and (LastClassName <> '') and ((S2 = LastClassName) or (Pos(LastClassName + ' ', S2) = 1)) then LIsBanner := True
        else if (Pos(' ', S2) = 0) and (Length(S2) >= 2) and (Pos(S2[1], 'TCIE') > 0) and (S2[2] >= 'A') and (S2[2] <= 'Z') then LIsBanner := True
        else 
        begin
          // Custom Inline-Banner: keep and format it
          LLine := '{ ' + StringOfChar('-', 71) + ' }' + #13#10 + '{ ' + PadRight(S2, 71) + ' }' + #13#10 + '{ ' + StringOfChar('-', 71) + ' }' + #13#10;
        end;
      end
      else if not LIsBanner and (AClassName <> '') then
      begin
        S2 := LLine;
        while (Length(S2) > 0) and ((S2[1] = '/') or (S2[1] = '{') or (S2[1] = ' ')) do Delete(S2, 1, 1);
        while (Length(S2) > 0) and ((S2[Length(S2)] = '}') or (S2[Length(S2)] = #13) or (S2[Length(S2)] = #10) or (S2[Length(S2)] = ' ')) do Delete(S2, Length(S2), 1);
        if S2 = AClassName then LIsBanner := True;
      end;

      if LCollectingForPrevious then
      begin
         if (Pos('{$ENDIF', LLine) > 0) or (Pos('{$ELSE', LLine) > 0) or (Pos('{$ENDREGION', LLine) > 0) or
            ((ATrailingPart = '') and (Length(AOldTrivia) > 0) and (AOldTrivia[1] <> #13) and (AOldTrivia[1] <> #10) and
             (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) and (Pos('{$REGION', LLine) = 0) and not LIsBanner) then
         begin
            ATrailingPart := ATrailingPart + LLine;
            Continue;
         end;
         LCollectingForPrevious := False;
      end;

      if not LIsBanner then AComments := AComments + LLine;
    end
    else
    begin
       if LCollectingForPrevious then ATrailingPart := ATrailingPart + LLine
       else AComments := AComments + LLine;
    end;
  end;

  while (Length(AComments) > 0) and ((AComments[1] = #13) or (AComments[1] = #10) or (AComments[1] = ' ')) do Delete(AComments, 1, 1);
  while (Length(AComments) > 0) and ((AComments[Length(AComments)] = #13) or (AComments[Length(AComments)] = #10) or (AComments[Length(AComments)] = ' ')) do Delete(AComments, Length(AComments), 1);
  
  while (Length(ATrailingPart) > 0) and ((ATrailingPart[Length(ATrailingPart)] = #13) or (ATrailingPart[Length(ATrailingPart)] = #10) or (ATrailingPart[Length(ATrailingPart)] = ' ')) do Delete(ATrailingPart, Length(ATrailingPart), 1);

  if AComments <> '' then 
  begin
    if AComments[Length(AComments)] = '}' then AComments := AComments + #13#10#13#10
    else AComments := AComments + #13#10;
  end;
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
    
    while (Length(LTrivia) > 0) and ((LTrivia[1] = #13) or (LTrivia[1] = #10) or (LTrivia[1] = ' ')) do Delete(LTrivia, 1, 1);
    AddLeadingTrivia(LToken, #13#10#13#10 + LTrivia);
    AddTrailingTrivia(LToken, '');
    
    LFirstItem := GetUsesFirstItemToken(AUses);
    if Assigned(LFirstItem) then
    begin
      LTrivia := GetLeadingTrivia(LFirstItem);
      ClearTrivia(LFirstItem);
      while (Length(LTrivia) > 0) and ((LTrivia[1] = #13) or (LTrivia[1] = #10) or (LTrivia[1] = ' ')) do Delete(LTrivia, 1, 1);
      AddLeadingTrivia(LFirstItem, #13#10#13#10 + '  ' + LTrivia);
    end;
    ExpectedTokenTextForSuppressedBanner := '';
  end;
end;

procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
var
  LClassName, LOldTrivia, LComments, LTrailingPart: string;
  LToken: TSyntaxToken;
  LIsSuppressed: Boolean;
  LLeadingNewlines: Integer;
begin
  LClassName := GetMethodClassName(AMethod);
  LToken := GetMethodStartToken(AMethod);
  if not Assigned(LToken) then Exit;

  LIsSuppressed := (ExpectedTokenTextForSuppressedBanner <> '') and (LToken.Text = ExpectedTokenTextForSuppressedBanner);
  ExpectedTokenTextForSuppressedBanner := ''; 

  LOldTrivia := GetLeadingTrivia(LToken);
  ClearTrivia(LToken);

  ProcessTrivia(LOldTrivia, LClassName, LTrailingPart, LComments, LLeadingNewlines);

  if (Pos('{ -', LComments) > 0) or (Pos('{ =', LComments) > 0) then LIsSuppressed := True;

  if GetMethodDepth(AMethod) > 1 then
  begin
    if LIsSuppressed then
    begin
       if LTrailingPart <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LTrailingPart + #13#10#13#10 + CreateNestedMethodBanner() + LComments)
       else AddLeadingTrivia(LToken, #13#10#13#10 + CreateNestedMethodBanner() + LComments);
    end
    else
    begin
       if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + CreateNestedMethodBanner() + LComments)
       else AddLeadingTrivia(LToken, #13#10#13#10 + CreateNestedMethodBanner() + LComments);
    end;
  end
  else
  begin
    if (LClassName <> '') and (LClassName <> LastClassName) then
    begin
       if LIsSuppressed then 
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LTrailingPart + #13#10#13#10 + CreateClassBanner(LClassName) + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + CreateClassBanner(LClassName) + LComments);
       end
       else
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + CreateClassBanner(LClassName) + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + CreateClassBanner(LClassName) + LComments);
       end;
       LastClassName := LClassName;
    end
    else if (LClassName = '') and (LastClassName <> '') then
    begin
       LastClassName := '';
       if LIsSuppressed then
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LTrailingPart + #13#10#13#10 + CreateSectionBanner('') + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + CreateSectionBanner('') + LComments);
       end
       else
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + CreateSectionBanner('') + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + CreateSectionBanner('') + LComments);
       end;
    end
    else
    begin
       if not LIsSuppressed then
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + CreateMethodBanner() + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + CreateMethodBanner() + LComments);
       end
       else
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LTrailingPart + #13#10#13#10 + LComments)
          else
          begin
             if LComments <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LComments)
             else AddLeadingTrivia(LToken, #13#10#13#10);
          end;
       end;
    end;
  end;
end;

procedure StripBanners(AToken: TSyntaxToken);
var
  LTrivia, LLine, S, S2, LNewTrivia: string;
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
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then 
      begin
        S2 := LLine;
        while (Length(S2) > 0) and ((S2[1] = '/') or (S2[1] = '{') or (S2[1] = ' ')) do Delete(S2, 1, 1);
        while (Length(S2) > 0) and ((S2[Length(S2)] = '}') or (S2[Length(S2)] = #13) or (S2[Length(S2)] = #10) or (S2[Length(S2)] = ' ')) do Delete(S2, Length(S2), 1);
        if S2 = '' then LIsBanner := True
        else if (Pos(' ', S2) = 0) and (Length(S2) >= 2) and (Pos(S2[1], 'TCIE') > 0) and (S2[2] >= 'A') and (S2[2] <= 'Z') then LIsBanner := True;
      end;
    end;
    if not LIsBanner then LNewTrivia := LNewTrivia + LLine;
  end;
  while (Length(LNewTrivia) > 0) and ((LNewTrivia[1] = #13) or (LNewTrivia[1] = #10) or (LNewTrivia[1] = ' ')) do Delete(LNewTrivia, 1, 1);
  ClearTrivia(AToken);
  if Length(LNewTrivia) > 0 then AddLeadingTrivia(AToken, #13#10#13#10 + LNewTrivia)
  else AddLeadingTrivia(AToken, #13#10#13#10);
  ExpectedTokenTextForSuppressedBanner := AToken.Text;
end;

procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax);
var LToken, LNext: TSyntaxToken; LComments, LTrailingPart, LOldTrivia, LBanner, LPrefix: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetInterfaceKeyword(ASection);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    ProcessTrivia(LOldTrivia, '', LTrailingPart, LComments, LLeadingNewlines);
    LBanner := '{ ' + StringOfChar('=', 71) + ' }' + #13#10;
    
    LPrefix := #13#10;
    if LLeadingNewlines >= 2 then LPrefix := #13#10#13#10;
    
    if LTrailingPart <> '' then 
    begin
       if LComments <> '' then AddLeadingTrivia(LToken, LTrailingPart + LPrefix + LComments + #13#10 + LBanner)
       else AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LBanner);
    end
    else 
    begin
       if LComments <> '' then AddLeadingTrivia(LToken, LPrefix + LComments + #13#10 + LBanner)
       else AddLeadingTrivia(LToken, #13#10#13#10 + LBanner);
    end;
    
    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }');
    LastClassName := '';
    LNext := GetNextToken(LToken); if Assigned(LNext) then StripBanners(LNext);
  end;
end;

procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax);
var LToken, LNext: TSyntaxToken; LComments, LTrailingPart, LOldTrivia, LBanner, LPrefix: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetImplementationKeyword(ASection);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    ProcessTrivia(LOldTrivia, '', LTrailingPart, LComments, LLeadingNewlines);
    LBanner := '{ ' + StringOfChar('=', 71) + ' }' + #13#10;
    
    LPrefix := #13#10;
    if LLeadingNewlines >= 2 then LPrefix := #13#10#13#10;
    
    if LTrailingPart <> '' then 
    begin
       if LComments <> '' then AddLeadingTrivia(LToken, LTrailingPart + LPrefix + LComments + #13#10 + LBanner)
       else AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LBanner);
    end
    else 
    begin
       if LComments <> '' then AddLeadingTrivia(LToken, LPrefix + LComments + #13#10 + LBanner)
       else AddLeadingTrivia(LToken, #13#10#13#10 + LBanner);
    end;
    
    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }');
    LastClassName := '';
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
    ClearTrivia(LToken); AddLeadingTrivia(LToken, LNewBanner);
    LSemicolon := GetUnitSemicolon(AUnit); if Assigned(LSemicolon) then begin ClearTrivia(LSemicolon); AddTrailingTrivia(LSemicolon, ''); end;
  end;
end;

procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
var LToken: TSyntaxToken; LComments, LTrailingPart, LOldTrivia: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetFinalEndKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    ProcessTrivia(LOldTrivia, '', LTrailingPart, LComments, LLeadingNewlines);
    
    if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LComments + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10)
    else AddLeadingTrivia(LToken, #13#10#13#10 + LComments + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10);
  end;
end;
