unit TaifunFormat.Banner;

interface

uses DptFormatterAPI, TaifunFormat.Utils;

type
  TTaifunBannerHelper = class
  private
    FCachedMethodBanner: string;
    FCachedNestedMethodBanner: string;
  public
    constructor Create;
    function CreateClassBanner(const AClassName: string): string;
    function CreateMethodBanner: string;
    function CreateNestedMethodBanner: string;
    function CreateSectionBanner(const AName: string): string;
    function StripBanners(AToken: TSyntaxToken): string;
  end;

implementation

constructor TTaifunBannerHelper.Create;
begin
  FCachedMethodBanner := '{ ' + GetSep('-', 71) + ' }' + #13#10 + #13#10;
  FCachedNestedMethodBanner := '{ ' + GetSep('-', 26) + ' }' + #13#10 + #13#10;
end;

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
  Result := FCachedMethodBanner;
end;

function TTaifunBannerHelper.CreateNestedMethodBanner: string;
begin
  Result := FCachedNestedMethodBanner;
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
  LIsText, LIsBanner, LPrevWasSepBanner: Boolean;
begin
  Result := '';
  if not Assigned(AToken) then Exit;
  LTrivia := GetLeadingTrivia(AToken);
  if LTrivia = '' then Exit;
  LNewTrivia := ''; S := LTrivia;
  LPrevWasSepBanner := False;
  while Length(S) > 0 do
  begin
    P := Pos(#10, S);
    if P > 0 then begin LLine := Copy(S, 1, P); Delete(S, 1, P); end else begin LLine := S; S := ''; end;
    LIsText := False;
    for I := 1 to Length(LLine) do if (LLine[I] <> ' ') and (LLine[I] <> #13) and (LLine[I] <> #10) and (LLine[I] <> #9) then begin LIsText := True; Break; end;
    LIsBanner := False;
    if LIsText then
    begin
      if (Pos('{ ==', LLine) > 0) or (Pos('// ==', LLine) > 0) or (Pos('// --', LLine) > 0) then
      begin
        LIsBanner := True;
        LPrevWasSepBanner := Pos('{ ==', LLine) > 0;
      end
      else
      begin
        if LPrevWasSepBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then
          LIsBanner := True;
        LPrevWasSepBanner := False;
        if not LIsBanner and (Pos('{ ', LLine) > 0) and (Pos(' }', LLine) > 0) and (Pos('///', LLine) = 0) and (Pos('{!', LLine) = 0) then 
        begin
          S2 := TrimCommentChars(LLine);
          if S2 = '' then LIsBanner := True
          else if (Pos(' ', S2) = 0) and (Length(S2) >= 2) and (Pos(S2[1], 'TCIE') > 0) and (S2[2] >= 'A') and (S2[2] <= 'Z') then LIsBanner := True
          else if Pos(' - Class', S2) > 0 then LIsBanner := True
          else if (Pos(' - ', S2) > 0) and (Length(S2) >= 2) and (Pos(S2[1], 'TCIE') > 0) and (S2[2] >= 'A') and (S2[2] <= 'Z') then LIsBanner := True;
        end;
      end;
    end
    else
      LPrevWasSepBanner := False;
    if not LIsBanner then LNewTrivia := LNewTrivia + LLine;
  end;
  LNewTrivia := TrimLeadingCRLF(LNewTrivia);
  ClearTrivia(AToken);
  if Length(LNewTrivia) > 0 then AddLeadingTrivia(AToken, #13#10#13#10 + LNewTrivia)
  else AddLeadingTrivia(AToken, #13#10#13#10);
  Result := AToken.Text;
end;

end.
