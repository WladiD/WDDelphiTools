unit TaifunFormat.Banner;

interface

uses DptFormatterAPI, TaifunFormat.Utils;

type
  TTaifunBannerHelper = class
  public
    function CreateClassBanner(const AClassName: string): string;
    function CreateMethodBanner: string;
    function CreateNestedMethodBanner: string;
    function CreateSectionBanner(const AName: string): string;
    function StripBanners(AToken: TSyntaxToken): string;
  end;

implementation

function TTaifunBannerHelper.CreateClassBanner(const AClassName: string): string;
var
  LRule: string;
begin
  LRule := '{ ' + GetSep('=', 71) + ' }';
  Result := LRule + #13#10 +
            '{ ' + PadRight(AClassName, 71) + ' }' + #13#10 +
            LRule + #13#10 + #13#10;
end;

function TTaifunBannerHelper.CreateMethodBanner: string;
begin
  Result := '{ ' + GetSep('-', 71) + ' }' + #13#10 + #13#10;
end;

function TTaifunBannerHelper.CreateNestedMethodBanner: string;
begin
  Result := '{ ' + GetSep('-', 26) + ' }' + #13#10 + #13#10;
end;

function TTaifunBannerHelper.CreateSectionBanner(const AName: string): string;
begin
  if AName = '' then
    Result := '{ ' + GetSep('=', 71) + ' }' + #13#10 + #13#10
  else
    Result := '{ ' + GetSep('=', 71) + ' }' + #13#10 +
              AName + #13#10 +
              '{ ' + GetSep('=', 71) + ' }' + #13#10 + #13#10;
end;

function TTaifunBannerHelper.StripBanners(AToken: TSyntaxToken): string;
var
  LTrivia, LLine, S, S2, LNewTrivia: string;
  P, I: Integer;
  LIsText, LIsBanner: Boolean;
begin
  Result := '';
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
  Result := AToken.Text;
end;

end.
