// DWScript for formatting Delphi units in the Taifun style

type
  TTaifunFormatter = class
  private
    FLastClassName: string;
    FExpectedTokenTextForSuppressedBanner: string;

    function GetSep(const C: string; Count: Integer): string;
    function PadRight(const S: string; ALen: Integer): string;

    function CreateClassBanner(const AClassName: string): string;
    function CreateMethodBanner: string;
    function CreateNestedMethodBanner: string;
    function CreateSectionBanner(const AName: string): string;
    function RemoveSpaces(const S: string): string;

    procedure ProcessTrivia(const AOldTrivia: string; const AClassName: string; var ATrailingPart, AComments: string; var ALeadingNewlines: Integer; var ATrailingIndent: string);
    procedure StripBanners(AToken: TSyntaxToken);
    function ExtractHeaderInfo(const ATrivia: string; const AUnitName: string; var ADescription: string; var AAuthor: string; var ADirectives: string; var AExtraComments: string): Boolean;

  public
    constructor Create;
    procedure ClearState;

    procedure FormatUnitStart(AUnit: TCompilationUnitSyntax);
    procedure FormatUnitEnd(AUnit: TCompilationUnitSyntax);
    procedure FormatInterfaceSection(ASection: TInterfaceSectionSyntax);
    procedure FormatImplementationSection(ASection: TImplementationSectionSyntax);
    procedure FormatUsesClause(AUses: TUsesClauseSyntax);
    procedure FormatMethodImplementation(AMethod: TMethodImplementationSyntax);
  end;

{ TTaifunFormatter }

constructor TTaifunFormatter.Create;
begin
  ClearState;
end;

procedure TTaifunFormatter.ClearState;
begin
  FLastClassName := '';
  FExpectedTokenTextForSuppressedBanner := '';
end;

function TTaifunFormatter.GetSep(const C: string; Count: Integer): string;
var
  I: Integer;
begin
  if (Count = 71) and (C = '=') then Exit('=======================================================================');
  if (Count = 71) and (C = '-') then Exit('-----------------------------------------------------------------------');
  if (Count = 70) and (C = '=') then Exit('======================================================================');
  if (Count = 26) and (C = '-') then Exit('--------------------------');

  Result := '';
  for I := 1 to Count do
    Result := Result + C;
end;

function TTaifunFormatter.PadRight(const S: string; ALen: Integer): string;
begin
  Result := S;
  while Length(Result) < ALen do
    Result := Result + ' ';
end;

function TTaifunFormatter.CreateClassBanner(const AClassName: string): string;
var
  LRule: string;
begin
  LRule := '{ ' + GetSep('=', 71) + ' }';
  Result := LRule + #13#10 +
            '{ ' + PadRight(AClassName, 71) + ' }' + #13#10 +
            LRule + #13#10 + #13#10;
end;

function TTaifunFormatter.CreateMethodBanner: string;
begin
  Result := '{ ' + GetSep('-', 71) + ' }' + #13#10 + #13#10;
end;

function TTaifunFormatter.CreateNestedMethodBanner: string;
begin
  Result := '{ ' + GetSep('-', 26) + ' }' + #13#10 + #13#10;
end;

function TTaifunFormatter.CreateSectionBanner(const AName: string): string;
begin
  if AName = '' then
    Result := '{ ' + GetSep('=', 71) + ' }' + #13#10 + #13#10
  else
    Result := '{ ' + GetSep('=', 71) + ' }' + #13#10 +
              AName + #13#10 +
              '{ ' + GetSep('=', 71) + ' }' + #13#10 + #13#10;
end;

function TTaifunFormatter.RemoveSpaces(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] <> ' ' then Result := Result + S[I];
end;

procedure TTaifunFormatter.ProcessTrivia(const AOldTrivia: string; const AClassName: string; var ATrailingPart, AComments: string; var ALeadingNewlines: Integer; var ATrailingIndent: string);
var
  S, LLine, S2, LPeek: string;
  P, I, LIfLevel, LPeekIdx: Integer;
  LCollectingForPrevious, LIsBanner, LIsText, LWasBraceComment, LIsBraceComment, LNextIsBrace: Boolean;
begin
  AComments := '';
  ATrailingPart := '';
  ATrailingIndent := '';
  S := AOldTrivia;

  ALeadingNewlines := 0;
  for I := 1 to Length(AOldTrivia) do
  begin
    if AOldTrivia[I] = #10 then ALeadingNewlines := ALeadingNewlines + 1;
    if (AOldTrivia[I] <> ' ') and (AOldTrivia[I] <> #13) and (AOldTrivia[I] <> #10) and (AOldTrivia[I] <> #9) then Break;
  end;

  // Extract trailing indent from original trivia
  I := Length(AOldTrivia);
  while (I > 0) and (AOldTrivia[I] = ' ') do Dec(I);
  if (I > 0) and ((AOldTrivia[I] = #10) or (AOldTrivia[I] = #13)) then
  begin
    ATrailingIndent := Copy(AOldTrivia, I + 1, Length(AOldTrivia) - I);
    S := Copy(AOldTrivia, 1, I);
  end;

  LCollectingForPrevious := True;
  LIfLevel := 0;
  LWasBraceComment := False;

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
      LIsBraceComment := False;
      LIsBanner := False;
      if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) or (Pos('// ==', LLine) > 0) or (Pos('// --', LLine) > 0) then LIsBanner := True;
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then 
      begin
        LIsBraceComment := True;
        S2 := LLine;
        while (Length(S2) > 0) and ((S2[1] = '/') or (S2[1] = '{') or (S2[1] = ' ')) do Delete(S2, 1, 1);
        while (Length(S2) > 0) and ((S2[Length(S2)] = '}') or (S2[Length(S2)] = #13) or (S2[Length(S2)] = #10) or (S2[Length(S2)] = ' ')) do Delete(S2, Length(S2), 1);
        
        var S2NoSpaces: string := RemoveSpaces(S2);
        var AClassNoSpaces: string := RemoveSpaces(AClassName);
        var FLastClassNoSpaces: string := RemoveSpaces(FLastClassName);

        if S2 = '' then LIsBanner := True
        else if (AClassName <> '') and ((S2NoSpaces = AClassNoSpaces) or (Pos(AClassNoSpaces + ' ', S2) = 1) or (Pos(AClassNoSpaces + '.', S2NoSpaces) = 1) or (Pos(AClassName + ' -', S2) = 1)) then LIsBanner := True
        else if (AClassName = '') and (FLastClassName <> '') and ((S2NoSpaces = FLastClassNoSpaces) or (Pos(FLastClassNoSpaces + ' ', S2) = 1) or (Pos(FLastClassName + ' -', S2) = 1)) then LIsBanner := True
        else if (Pos(' ', S2) = 0) and (Length(S2) >= 2) and (Pos(S2[1], 'TCIE') > 0) and (S2[2] >= 'A') and (S2[2] <= 'Z') then LIsBanner := True
        else if Pos(' - Class', S2) > 0 then LIsBanner := True
        else 
        begin
          LPeekIdx := 1;
          while (LPeekIdx <= Length(S)) and ((S[LPeekIdx] = ' ') or (S[LPeekIdx] = #13) or (S[LPeekIdx] = #10) or (S[LPeekIdx] = #9)) do Inc(LPeekIdx);
          LNextIsBrace := (LPeekIdx <= Length(S)) and (S[LPeekIdx] = '{') and 
            (Copy(S, LPeekIdx, 3) <> '{ -') and (Copy(S, LPeekIdx, 3) <> '{ =') and 
            (Copy(S, LPeekIdx, 2) <> '{!');
            
          var LPrevIsBrace: Boolean := False;
          var LLenA: Integer := Length(AComments);
          if (LLenA >= 3) and (AComments[LLenA - 1] = #13) and (AComments[LLenA] = #10) then
          begin
             var LCheckIdx: Integer := LLenA - 2;
             while (LCheckIdx > 0) and (AComments[LCheckIdx] = ' ') do Dec(LCheckIdx);
             if (LCheckIdx > 0) and (AComments[LCheckIdx] = '}') then LPrevIsBrace := True;
          end
          else if LLenA > 0 then
          begin
             var LCheckIdx: Integer := LLenA;
             while (LCheckIdx > 0) and (AComments[LCheckIdx] = ' ') do Dec(LCheckIdx);
             if (LCheckIdx > 0) and (AComments[LCheckIdx] = '}') then LPrevIsBrace := True;
          end;

          if LPrevIsBrace or LNextIsBrace then
          begin
             // It's a multiline comment block, keep as is, but ensure it's wrapped in separators
             if not LPrevIsBrace then
                LLine := '{ ' + GetSep('-', 71) + ' }' + #13#10 + LLine;
             if not LNextIsBrace then
                LLine := LLine + '{ ' + GetSep('-', 71) + ' }' + #13#10;
          end
          else
          begin
            // Custom Inline-Banner: keep and format it
            LLine := '{ ' + GetSep('-', 71) + ' }' + #13#10 + '{ ' + PadRight(S2, 71) + ' }' + #13#10 + '{ ' + GetSep('-', 71) + ' }' + #13#10;
          end;
        end;
      end
      else if not LIsBanner and (AClassName <> '') then
      begin
        S2 := LLine;
        while (Length(S2) > 0) and ((S2[1] = '/') or (S2[1] = '{') or (S2[1] = ' ')) do Delete(S2, 1, 1);
        while (Length(S2) > 0) and ((S2[Length(S2)] = '}') or (S2[Length(S2)] = #13) or (S2[Length(S2)] = #10) or (S2[Length(S2)] = ' ')) do Delete(S2, Length(S2), 1);
        if S2 = AClassName then LIsBanner := True;
      end;

      if Pos('{$IF', LLine) > 0 then Inc(LIfLevel);

      if LCollectingForPrevious then
      begin
         if (not LIsBanner) and ((LIfLevel > 0) or (Pos('{$ENDIF', LLine) > 0) or (Pos('{$ELSE', LLine) > 0) or (Pos('{$ENDREGION', LLine) > 0) or
            ((ATrailingPart = '') and (Length(AOldTrivia) > 0) and (AOldTrivia[1] <> #13) and (AOldTrivia[1] <> #10) and
             (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) and (Pos('{$REGION', LLine) = 0))) then
         begin
            ATrailingPart := ATrailingPart + LLine;
            if Pos('{$ENDIF', LLine) > 0 then Dec(LIfLevel);
            LWasBraceComment := LIsBraceComment;
            Continue;
         end;
         LCollectingForPrevious := False;
      end;
      
      if Pos('{$ENDIF', LLine) > 0 then Dec(LIfLevel);

      if not LIsBanner then AComments := AComments + LLine;
    end
    else
    begin
       if LCollectingForPrevious then ATrailingPart := ATrailingPart + LLine
       else AComments := AComments + LLine;
    end;
  end;

  while (Length(AComments) > 0) and ((AComments[1] = #13) or (AComments[1] = #10)) do Delete(AComments, 1, 1);
  while (Length(AComments) > 0) and ((AComments[Length(AComments)] = #13) or (AComments[Length(AComments)] = #10) or (AComments[Length(AComments)] = ' ')) do Delete(AComments, Length(AComments), 1);

  while (Length(ATrailingPart) > 0) and ((ATrailingPart[Length(ATrailingPart)] = #13) or (ATrailingPart[Length(ATrailingPart)] = #10) or (ATrailingPart[Length(ATrailingPart)] = ' ')) do Delete(ATrailingPart, Length(ATrailingPart), 1);

  if AComments <> '' then 
  begin
    if AComments[Length(AComments)] = '}' then AComments := AComments + #13#10#13#10
    else AComments := AComments + #13#10;
  end;
end;

procedure TTaifunFormatter.StripBanners(AToken: TSyntaxToken);
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
      if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) or (Pos('// ==', LLine) > 0) or (Pos('// --', LLine) > 0) then LIsBanner := True;
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then 
      begin
        S2 := LLine;
        while (Length(S2) > 0) and ((S2[1] = '/') or (S2[1] = '{') or (S2[1] = ' ')) do Delete(S2, 1, 1);
        while (Length(S2) > 0) and ((S2[Length(S2)] = '}') or (S2[Length(S2)] = #13) or (S2[Length(S2)] = #10) or (S2[Length(S2)] = ' ')) do Delete(S2, Length(S2), 1);
        if S2 = '' then LIsBanner := True
        else if (Pos(' ', S2) = 0) and (Length(S2) >= 2) and (Pos(S2[1], 'TCIE') > 0) and (S2[2] >= 'A') and (S2[2] <= 'Z') then LIsBanner := True
        else if Pos(' - Class', S2) > 0 then LIsBanner := True
        else if (Pos(' - ', S2) > 0) and (Length(S2) >= 2) and (Pos(S2[1], 'TCIE') > 0) and (S2[2] >= 'A') and (S2[2] <= 'Z') then LIsBanner := True;
      end;
    end;
    if not LIsBanner then LNewTrivia := LNewTrivia + LLine;
  end;
  while (Length(LNewTrivia) > 0) and ((LNewTrivia[1] = #13) or (LNewTrivia[1] = #10)) do Delete(LNewTrivia, 1, 1);
  ClearTrivia(AToken);
  if Length(LNewTrivia) > 0 then AddLeadingTrivia(AToken, #13#10#13#10 + LNewTrivia)
  else AddLeadingTrivia(AToken, #13#10#13#10);
  FExpectedTokenTextForSuppressedBanner := AToken.Text;
end;

function TTaifunFormatter.ExtractHeaderInfo(const ATrivia: string; const AUnitName: string; var ADescription: string; var AAuthor: string; var ADirectives: string; var AExtraComments: string): Boolean;
var S, LLine: string; P, P2, P3: Integer; LIsDirective, LFoundDesc, LFoundAuthor: Boolean;
begin
  Result := Length(ATrivia) > 0; LFoundDesc := False; LFoundAuthor := False; ADescription := ''; AAuthor := 'Name'; ADirectives := ''; AExtraComments := '';
  if Result then
  begin
    S := ATrivia;
    while Length(S) > 0 do
    begin
      P := Pos(#10, S);
      if P > 0 then begin LLine := Copy(S, 1, P - 1); if (Length(LLine) > 0) and (LLine[Length(LLine)] = #13) then LLine := Copy(LLine, 1, Length(LLine) - 1); Delete(S, 1, P); end else begin LLine := S; S := ''; end;
      LIsDirective := False;
      for P2 := 1 to Length(LLine) do if LLine[P2] <> ' ' then begin if (LLine[P2] = '{') and (P2 < Length(LLine)) and (LLine[P2+1] = '$') then LIsDirective := True; Break; end;
      if LIsDirective then begin if ADirectives <> '' then ADirectives := ADirectives + #13#10 + LLine else ADirectives := LLine; end
      else
      begin
        P2 := Pos('Autor:', LLine); 
        if P2 > 0 then 
        begin 
          AAuthor := Copy(LLine, P2 + 6, Length(LLine)); 
          while (Length(AAuthor) > 0) and (AAuthor[1] = ' ') do Delete(AAuthor, 1, 1); 
          while (Length(AAuthor) > 0) and (AAuthor[Length(AAuthor)] = ' ') do Delete(AAuthor, Length(AAuthor), 1); 
          LFoundAuthor := True;
        end
        else if (Pos('//', LLine) = 1) and (Pos('// Autor:', LLine) = 0) and (Pos('// ===', LLine) = 0) then
        begin
          if (Length(LLine) > 3) and not LFoundDesc and ((Pos('-', LLine) > 0) or (Pos('–', LLine) > 0) or (Pos('â€', LLine) > 0)) then
          begin
            P3 := Pos('-', LLine); 
            if P3 = 0 then P3 := Pos('–', LLine);
            if P3 = 0 then P3 := Pos('â€', LLine);

            // Find the character index after the dash. We handle the 3-byte 'â€“' if it was matched by 'â€'.
            if Pos('â€', LLine) = P3 then
              ADescription := Copy(LLine, P3 + Length('â€“'), Length(LLine))
            else if Pos('–', LLine) = P3 then
              ADescription := Copy(LLine, P3 + Length('–'), Length(LLine))
            else
              ADescription := Copy(LLine, P3 + 1, Length(LLine)); 
            
            while (Length(ADescription) > 0) and (ADescription[1] = ' ') do Delete(ADescription, 1, 1); 
            while (Length(ADescription) > 0) and (ADescription[Length(ADescription)] = ' ') do Delete(ADescription, Length(ADescription), 1); 
            LFoundDesc := True; 
          end
          else if (Length(LLine) > Length('// ' + AUnitName + ' ')) and not LFoundDesc and (Pos('// ' + AUnitName + ' ', LLine) = 1) then
          begin
            ADescription := Copy(LLine, Length('// ' + AUnitName + ' ') + 1, Length(LLine)); 
            while (Length(ADescription) > 0) and (ADescription[1] = ' ') do Delete(ADescription, 1, 1); 
            while (Length(ADescription) > 0) and (ADescription[Length(ADescription)] = ' ') do Delete(ADescription, Length(ADescription), 1); 
            if ADescription <> '' then LFoundDesc := True;
          end
          else if (LLine <> '// ' + AUnitName) and (Pos('// ' + AUnitName + ' -', LLine) <> 1) then
          begin
            if LFoundDesc and not LFoundAuthor and (LLine <> '//') and (LLine <> '// ') then
            begin
              ADescription := ADescription + #13#10 + LLine;
            end
            else
            begin
              if AExtraComments <> '' then AExtraComments := AExtraComments + #13#10 + LLine else AExtraComments := LLine;
            end;
          end;
        end;
      end;
    end;
  end;
  
  while (AExtraComments <> '') and ((Pos('//', AExtraComments) = 1) and (Length(AExtraComments) <= 3) or (Pos('//' + #13#10, AExtraComments) = 1) or (Pos('// ' + #13#10, AExtraComments) = 1)) do
  begin
    P := Pos(#10, AExtraComments);
    if P > 0 then Delete(AExtraComments, 1, P) else AExtraComments := '';
  end;
  
  while (AExtraComments <> '') and (
    (Copy(AExtraComments, Length(AExtraComments) - 3, 4) = #13#10 + '//') or 
    (Copy(AExtraComments, Length(AExtraComments) - 4, 5) = #13#10 + '// ')
  ) do
  begin
    P := Length(AExtraComments);
    while (P > 0) and (AExtraComments[P] <> #10) do Dec(P);
    if P > 1 then Delete(AExtraComments, P - 1, Length(AExtraComments)) else AExtraComments := '';
  end;
  
  if (AExtraComments = '//') or (AExtraComments = '// ') then AExtraComments := '';

  if not LFoundDesc and not Result then ADescription := 'Kurzbeschreibung der Unit';
  if ADirectives = '' then ADirectives := '{$I Tfw.Define.pas}';
end;

procedure TTaifunFormatter.FormatUsesClause(AUses: TUsesClauseSyntax);
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
    FExpectedTokenTextForSuppressedBanner := '';
  end;
end;

procedure TTaifunFormatter.FormatMethodImplementation(AMethod: TMethodImplementationSyntax);
var
  LClassName, LOldTrivia, LComments, LTrailingPart, LIndent: string;
  LToken: TSyntaxToken;
  LIsSuppressed: Boolean;
  LLeadingNewlines: Integer;
begin
  LClassName := GetMethodClassName(AMethod);
  LToken := GetMethodStartToken(AMethod);
  if not Assigned(LToken) then Exit;

  LIsSuppressed := (FExpectedTokenTextForSuppressedBanner <> '') and (LToken.Text = FExpectedTokenTextForSuppressedBanner);
  FExpectedTokenTextForSuppressedBanner := ''; 

  LOldTrivia := GetLeadingTrivia(LToken);
  ClearTrivia(LToken);

  ProcessTrivia(LOldTrivia, LClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);

  if (Pos('{ -', LComments) > 0) or (Pos('{ =', LComments) > 0) then LIsSuppressed := True;

  if GetMethodDepth(AMethod) > 1 then
  begin
    if LIsSuppressed then
    begin
       if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + CreateNestedMethodBanner() + LComments + LIndent)
       else AddLeadingTrivia(LToken, #13#10#13#10 + CreateNestedMethodBanner() + LComments + LIndent);
    end
    else
    begin
       if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + CreateNestedMethodBanner() + LComments + LIndent)
       else AddLeadingTrivia(LToken, #13#10#13#10 + CreateNestedMethodBanner() + LComments + LIndent);
    end;

    // Also append the nested method banner AFTER the end of the method block
    var LEndToken: TSyntaxToken := GetMethodEndToken(AMethod);
    if Assigned(LEndToken) then
    begin
      var LNextToken: TSyntaxToken := GetNextToken(LEndToken);
      var LNextText: string := '';
      if Assigned(LNextToken) then LNextText := LowerCase(LNextToken.Text);
      if (LNextText <> 'procedure') and (LNextText <> 'function') and (LNextText <> 'constructor') and (LNextText <> 'destructor') and (LNextText <> 'class') then
      begin
        if Assigned(LNextToken) then
        begin
          var LNextOldTrivia: string := GetLeadingTrivia(LNextToken);
          ClearTrivia(LNextToken);
          var LNextComments, LNextTrailingPart, LNextIndent: string;
          var LNextLeadingNewlines: Integer;
          ProcessTrivia(LNextOldTrivia, '', LNextTrailingPart, LNextComments, LNextLeadingNewlines, LNextIndent);
          
          if LNextTrailingPart <> '' then AddLeadingTrivia(LNextToken, LNextTrailingPart + #13#10#13#10 + CreateNestedMethodBanner() + LNextComments + LNextIndent)
          else AddLeadingTrivia(LNextToken, #13#10#13#10 + CreateNestedMethodBanner() + LNextComments + LNextIndent);
        end;
      end;
    end;
  end
  else
  begin
    if (LClassName <> '') and (LClassName <> FLastClassName) then
    begin
       if LIsSuppressed then 
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + CreateClassBanner(LClassName) + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + CreateClassBanner(LClassName) + LComments);
       end
       else
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + CreateClassBanner(LClassName) + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + CreateClassBanner(LClassName) + LComments);
       end;
       FLastClassName := LClassName;
    end
    else if (LClassName = '') and (FLastClassName <> '') then
    begin
       FLastClassName := '';
       if LIsSuppressed then
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + CreateSectionBanner('') + LComments)
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
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LComments)
          else
          begin
             if LComments <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LComments)
             else AddLeadingTrivia(LToken, #13#10#13#10);
          end;
       end;
    end;
  end;
end;

procedure TTaifunFormatter.FormatInterfaceSection(ASection: TInterfaceSectionSyntax);
var LToken, LNext: TSyntaxToken; LComments, LTrailingPart, LOldTrivia, LBanner, LPrefix, LIndent: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetInterfaceKeyword(ASection);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    ProcessTrivia(LOldTrivia, '', LTrailingPart, LComments, LLeadingNewlines, LIndent);
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
    LNext := GetNextToken(LToken); if Assigned(LNext) then StripBanners(LNext);
  end;
end;

procedure TTaifunFormatter.FormatImplementationSection(ASection: TImplementationSectionSyntax);
var LToken, LNext: TSyntaxToken; LComments, LTrailingPart, LOldTrivia, LBanner, LPrefix, LIndent: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetImplementationKeyword(ASection);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    ProcessTrivia(LOldTrivia, '', LTrailingPart, LComments, LLeadingNewlines, LIndent);
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
    LNext := GetNextToken(LToken); if Assigned(LNext) then StripBanners(LNext);
  end;
end;

procedure TTaifunFormatter.FormatUnitStart(AUnit: TCompilationUnitSyntax);
var LToken, LSemicolon: TSyntaxToken; LUnitName, LTrivia, LDesc, LAuthor, LDirectives, LRule, LNewBanner, LDescLine, LExtra: string;
begin
  LToken := GetUnitKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LUnitName := GetUnitName(AUnit); LTrivia := GetLeadingTrivia(LToken);
    ExtractHeaderInfo(LTrivia, LUnitName, LDesc, LAuthor, LDirectives, LExtra);
    LRule := '// ' + GetSep('=', 70); LDescLine := '// ' + LUnitName; if LDesc <> '' then LDescLine := LDescLine + ' - ' + LDesc;
    LNewBanner := LRule + #13#10 + '//' + #13#10 + LDescLine + #13#10 + '//' + #13#10 + '// Autor: ' + LAuthor + #13#10 + '//';
    if LExtra <> '' then LNewBanner := LNewBanner + #13#10 + LExtra + #13#10 + '//';
    LNewBanner := LNewBanner + #13#10 + LRule + #13#10 + #13#10 + LDirectives + #13#10 + #13#10;
    ClearTrivia(LToken); AddLeadingTrivia(LToken, LNewBanner);
    LSemicolon := GetUnitSemicolon(AUnit); if Assigned(LSemicolon) then begin ClearTrivia(LSemicolon); AddTrailingTrivia(LSemicolon, ''); end;
  end;
end;

procedure TTaifunFormatter.FormatUnitEnd(AUnit: TCompilationUnitSyntax);
var LToken: TSyntaxToken; LComments, LTrailingPart, LOldTrivia, LIndent: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetFinalEndKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    ProcessTrivia(LOldTrivia, '', LTrailingPart, LComments, LLeadingNewlines, LIndent);

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

procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
begin
  GetFormatter.FormatMethodImplementation(AMethod);
end;

procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
begin
  GetFormatter.FormatUnitEnd(AUnit);
end;