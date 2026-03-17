unit TaifunFormat.Utils;

interface

function GetSep(const C: string; Count: Integer): string;
function PadRight(const S: string; ALen: Integer): string;
function RemoveSpaces(const S: string): string;
function TrimLeadingCRLFSpace(const S: string): string;
function TrimLeadingCRLF(const S: string): string;
function TrimTrailingCRLFSpace(const S: string): string;
function TrimCommentChars(const S: string): string;

implementation

function GetSep(const C: string; Count: Integer): string;
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

function PadRight(const S: string; ALen: Integer): string;
begin
  Result := S;
  while Length(Result) < ALen do
    Result := Result + ' ';
end;

function RemoveSpaces(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] <> ' ' then Result := Result + S[I];
end;

function TrimLeadingCRLFSpace(const S: string): string;
var
  I: Integer;
begin
  I := 1;
  while (I <= Length(S)) and ((S[I] = #13) or (S[I] = #10) or (S[I] = ' ')) do Inc(I);
  if I > 1 then Result := Copy(S, I, Length(S) - I + 1)
  else Result := S;
end;

function TrimLeadingCRLF(const S: string): string;
var
  I: Integer;
begin
  I := 1;
  while (I <= Length(S)) and ((S[I] = #13) or (S[I] = #10)) do Inc(I);
  if I > 1 then Result := Copy(S, I, Length(S) - I + 1)
  else Result := S;
end;

function TrimTrailingCRLFSpace(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and ((S[I] = #13) or (S[I] = #10) or (S[I] = ' ')) do Dec(I);
  Result := Copy(S, 1, I);
end;

function TrimCommentChars(const S: string): string;
var
  LStart, LEnd: Integer;
begin
  LStart := 1;
  while (LStart <= Length(S)) and ((S[LStart] = '/') or (S[LStart] = '{') or (S[LStart] = ' ')) do Inc(LStart);
  LEnd := Length(S);
  while (LEnd >= LStart) and ((S[LEnd] = '}') or (S[LEnd] = #13) or (S[LEnd] = #10) or (S[LEnd] = ' ')) do Dec(LEnd);
  Result := Copy(S, LStart, LEnd - LStart + 1);
end;

end.
