unit WDDT.StringTools;

interface

// The following functions Empty and Defined are the better performing variants for the
// frequently used string comparison "Trim(X) = ''" (Empty) or "Trim(X) <> ''" (Defined)
// Depending on the input string the functions are faster by a factor of 5-100. The acceleration
// results from the short evaluation which is performed directly on the input string without a
// temporary generated string and by saving a function jump (no Trim).  Also is the memory manager
// not involved.
function Empty(const s: string): Boolean;
function Defined(const s: string): Boolean;

function IfEmpty(const Source, ReplaceWith: string): string;

implementation

// Returns True if no single visible character is contained, otherwise False
function Empty(const s: string): Boolean;
var
  cc: Integer;
begin
  cc := Length(s);
  while (cc > 0) and (s[cc] <= ' ') do
    Dec(cc);
  Result := cc = 0;
end;

// Returns True if any visible character is contained in the passed string, otherwise False
//
// Is the opposite of the Empty function.
function Defined(const s: string): Boolean;
var
  cc: Integer;
begin
  cc := Length(s);
  while (cc > 0) and (s[cc] <= ' ') do
    Dec(cc);
  Result := cc > 0;
end;

// If the param Source is empty (see function Empty), the result is the second parameter
// ReplaceWith, otherwise Source is returned.
function IfEmpty(const Source, ReplaceWith: string): string;
begin
  if Empty(Source) then
    Result := ReplaceWith
  else
    Result := Source;
end;

end.
