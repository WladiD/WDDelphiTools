unit TaifunFormat.Trivia;

interface

uses TaifunFormat.Utils;

type
  TTaifunTriviaHelper = class
  public
    procedure ProcessTrivia(const AOldTrivia: string; const AClassName: string; const ALastClassName: string; var ATrailingPart, AComments: string; var ALeadingNewlines: Integer; var ATrailingIndent: string);
  end;

implementation

procedure TTaifunTriviaHelper.ProcessTrivia(const AOldTrivia: string; const AClassName: string; const ALastClassName: string; var ATrailingPart, AComments: string; var ALeadingNewlines: Integer; var ATrailingIndent: string);
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

  I := Length(AOldTrivia);
  while (I > 0) and (AOldTrivia[I] = ' ') do Dec(I);
  if (I > 0) and ((AOldTrivia[I] = #10) or (AOldTrivia[I] = #13)) then
  begin
    ATrailingIndent := Copy(AOldTrivia, I + 1, Length(AOldTrivia) - I);
    S := Copy(AOldTrivia, 1, I);
  end;

  var LClassNoSpaces: string := RemoveSpaces(AClassName);
  var LLastClassNoSpaces: string := RemoveSpaces(ALastClassName);

  var LPrevWasSepBanner: Boolean := False;

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
      if (Pos('{ ==', LLine) > 0) or (Pos('{ --', LLine) > 0) or (Pos('// ==', LLine) > 0) or (Pos('// --', LLine) > 0) then
      begin
        LIsBanner := True;
        LPrevWasSepBanner := Pos('{ ==', LLine) > 0;
      end
      else if LPrevWasSepBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then
      begin
        LIsBanner := True;
        LPrevWasSepBanner := False;
      end
      else
        LPrevWasSepBanner := False;
      if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then 
      begin
        LIsBraceComment := True;
        S2 := TrimCommentChars(LLine);
        
        var S2NoSpaces: string := RemoveSpaces(S2);

        if S2 = '' then LIsBanner := True
        else if (AClassName <> '') and ((S2NoSpaces = LClassNoSpaces) or (Pos(LClassNoSpaces + ' ', S2) = 1) or (Pos(LClassNoSpaces + '.', S2NoSpaces) = 1) or (Pos(AClassName + ' -', S2) = 1) or (Pos(LClassNoSpaces + '<', S2NoSpaces) = 1)) then LIsBanner := True
        else if (AClassName = '') and (ALastClassName <> '') and ((S2NoSpaces = LLastClassNoSpaces) or (Pos(LLastClassNoSpaces + ' ', S2) = 1) or (Pos(ALastClassName + ' -', S2) = 1) or (Pos(LLastClassNoSpaces + '<', S2NoSpaces) = 1)) then LIsBanner := True
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
             if not LPrevIsBrace then
                LLine := '{ ' + GetSep('-', 71) + ' }' + #13#10 + LLine;
             if not LNextIsBrace then
                LLine := LLine + '{ ' + GetSep('-', 71) + ' }' + #13#10;
          end
          else
          begin
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

      var ULine: string := UpperCase(LLine);
      if Pos('{$IF', ULine) > 0 then Inc(LIfLevel);

      if LCollectingForPrevious then
      begin
         if (not LIsBanner) and ((LIfLevel > 0) or (Pos('{$ENDIF', ULine) > 0) or (Pos('{$ELSE', ULine) > 0) or (Pos('{$ENDREGION', ULine) > 0) or (Pos('{$R ', ULine) > 0) or
            ((ATrailingPart = '') and (Length(AOldTrivia) > 0) and (AOldTrivia[1] <> #13) and (AOldTrivia[1] <> #10) and
             (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) and (Pos('{$REGION', ULine) = 0))) then
         begin
            ATrailingPart := ATrailingPart + LLine;
            if Pos('{$ENDIF', ULine) > 0 then Dec(LIfLevel);
            LWasBraceComment := LIsBraceComment;
            Continue;
         end;
         LCollectingForPrevious := False;
      end;
      
      if Pos('{$ENDIF', ULine) > 0 then Dec(LIfLevel);

      if not LIsBanner then AComments := AComments + LLine;
    end
    else
    begin
       LPrevWasSepBanner := False;
       if LCollectingForPrevious then ATrailingPart := ATrailingPart + LLine
       else AComments := AComments + LLine;
    end;
  end;

  AComments := TrimLeadingCRLF(AComments);
  AComments := TrimTrailingCRLFSpace(AComments);
  ATrailingPart := TrimTrailingCRLFSpace(ATrailingPart);

  if AComments <> '' then 
  begin
    if AComments[Length(AComments)] = '}' then AComments := AComments + #13#10#13#10
    else AComments := AComments + #13#10;
  end;
end;

end.
