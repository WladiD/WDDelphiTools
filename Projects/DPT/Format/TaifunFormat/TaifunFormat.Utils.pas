unit TaifunFormat.Utils;

interface

function GetSep(const C: string; Count: Integer): string;
function PadRight(const S: string; ALen: Integer): string;
function RemoveSpaces(const S: string): string;

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

end.
