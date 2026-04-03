unit TaifunFormat.Header;

interface

uses TaifunFormat.Utils;

type
  TTaifunHeaderHelper = class
  public
    function ExtractHeaderInfo(const ATrivia: string; const AUnitName: string; var ADescription: string; var AAuthor: string; var ADirectives: string; var AExtraComments: string): Boolean;
  end;

implementation

function TTaifunHeaderHelper.ExtractHeaderInfo(const ATrivia: string; const AUnitName: string; var ADescription: string; var AAuthor: string; var ADirectives: string; var AExtraComments: string): Boolean;
var S, LLine, LTrimmed: string; P, P2, P3: Integer; LFoundDesc, LFoundAuthor, LInBanner: Boolean;
begin
  Result := Length(ATrivia) > 0; LFoundDesc := False; LFoundAuthor := False; ADescription := ''; AAuthor := 'Name'; ADirectives := ''; AExtraComments := ''; LInBanner := True;
  if Result then
  begin
    S := ATrivia;
    while Length(S) > 0 do
    begin
      P := Pos(#10, S);
      if P > 0 then begin LLine := Copy(S, 1, P - 1); if (Length(LLine) > 0) and (LLine[Length(LLine)] = #13) then LLine := Copy(LLine, 1, Length(LLine) - 1); Delete(S, 1, P); end else begin LLine := S; S := ''; end;
      
      if LInBanner then
      begin
        LTrimmed := LLine;
        while (Length(LTrimmed) > 0) and (LTrimmed[1] = ' ') do Delete(LTrimmed, 1, 1);
        if (Length(LTrimmed) = 0) or (Pos('//', LTrimmed) <> 1) then LInBanner := False;
      end;
      
      if not LInBanner then
      begin
        if ADirectives <> '' then ADirectives := ADirectives + #13#10 + LLine else ADirectives := LLine;
      end
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
        else if (Pos('//', LTrimmed) = 1) and (Pos('Autor:', LLine) = 0) and (Pos('// ===', LLine) = 0) then
        begin
          if (Length(LLine) > 3) and not LFoundDesc and ((Pos('-', LLine) > 0) or (Pos('–', LLine) > 0) or (Pos('â€', LLine) > 0)) then
          begin
            P3 := Pos('-', LLine); 
            if P3 = 0 then P3 := Pos('–', LLine);
            if P3 = 0 then P3 := Pos('â€', LLine);

            if Pos('â€', LLine) = P3 then
              ADescription := Copy(LLine, P3 + Length('â€"'), Length(LLine))
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
          else if (LLine <> '// ' + AUnitName) and (Pos('// ' + AUnitName + ' -', LLine) <> 1) and
                  (Pos(LLine + '.', '// ' + AUnitName) <> 1) then
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

  while (ADirectives <> '') and (Pos(#13#10, ADirectives) = 1) do Delete(ADirectives, 1, 2);
  while (ADirectives <> '') and (Copy(ADirectives, Length(ADirectives) - 1, 2) = #13#10) do Delete(ADirectives, Length(ADirectives) - 1, 2);

  if not LFoundDesc and not Result then ADescription := 'Kurzbeschreibung der Unit';
  if ADirectives = '' then
  begin
    if (Pos('Base.', AUnitName) = 1) or (AUnitName = 'Base') then
      ADirectives := '{$I Base.Define.pas}'
    else
      ADirectives := '{$I Tfw.Define.pas}';
  end;
end;

end.
