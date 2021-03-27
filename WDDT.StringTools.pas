unit WDDT.StringTools;

interface

uses
  System.SysUtils;

// The following functions Empty and Defined are the better performing variants for the
// frequently used string comparison "Trim(X) = ''" (Empty) or "Trim(X) <> ''" (Defined)
// Depending on the input string the functions are faster by a factor of 5-100. The acceleration
// results from the short evaluation which is performed directly on the input string without a
// temporary generated string and by saving a function jump (no Trim).  Also is the memory manager
// not involved.
function Empty(const s: string): Boolean;
function Defined(const s: string): Boolean;

function IfEmpty(const Source, ReplaceWith: string): string;

function CompareStringNatural(const StringA, StringB: string): Integer;

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

// Compare function for "Natural sort order"
//
// https://en.wikipedia.org/wiki/Natural_sort_order
//
// Based on https://github.com/slackydev/SimbaExt/blob/37416bbbc16343a5789c18857450848b5057d297/DLL%20Source/SimbaExt/src/sorting/TSASort.pas
function CompareStringNatural(const StringA, StringB: string): Integer;
type
  TNaturalString = record
     Text: string;
     Number: Integer;
     IsText: Boolean;
  end;
  TNaturalArray = array of TNaturalString;

  function IsDigit(AChar: Char): Boolean; inline;
  begin
    Result := CharInSet(AChar, ['0'..'9']);
  end;

  // Splits strings and numbers in to a TNaturalArray, outputing something like: ['asd',123,'cat'].
  function GetNaturalString(Str: string): TNaturalArray;
  var
    i, l, j: Int32;
    IsText: Boolean;
    Temp: String;
  begin
    Temp := '';
    L := Length(Str);
    j := 0;
    for i := 1 to L do
    begin
      IsText := not IsDigit(Str[i]);

      if IsText then
      begin
        Temp := Temp + Str[i];
        // Continue, when the next char is also *not* a digit
        if (i + 1 <= L) and not IsDigit(Str[i+1]) then
          Continue;
      end
      else
      begin
        Temp := Temp + Str[i];
        // Is the next char also a digit...then continue
        if (i + 1 <= L) and IsDigit(Str[i + 1]) then
          Continue;
      end;

      SetLength(Result, j + 1);
      Result[j].IsText := IsText;

      if IsText then
        Result[j].Text := Temp
      else
        Result[j].Number := StrToInt(Temp);

      Inc(j);
      Temp := '';
    end;
  end;

var
  cc: Integer;
  ListA, ListB: TNaturalArray;
  LengthA, LengthB, LengthMin: Integer;
begin
  ListA := GetNaturalString(StringA);
  ListB := GetNaturalString(StringB);

  LengthA := Length(ListA);
  LengthB := Length(ListB);

  if LengthA < LengthB then
    LengthMin := LengthA
  else
    LengthMin := LengthB;

  Result := 0;

  for cc := 0 to LengthMin do
  begin
    if (not ListA[cc].IsText) and (not ListB[cc].IsText) then
      Result := ListA[cc].Number - ListB[cc].Number
    else if ListA[cc].IsText and ListB[cc].IsText then
      Result := CompareStr(ListA[cc].Text, ListB[cc].Text)
    else if (not ListA[cc].IsText) and ListB[cc].IsText then
      Result := -1
    else if ListA[cc].IsText and (not ListB[cc].IsText) then
      Result := 1
    else
      Result := 0;

    if Result <> 0 then
      Exit(Result);
  end;

  if Result = 0 then
    Result := LengthA - LengthB;
end;

end.
