unit DPT.Lint.StyleValidator;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections;

type
  TDptLintStyleValidator = class
  public
    class procedure ValidateStyleFile(const AStylePath: string);
  end;

implementation

{ TDptLintStyleValidator }

class procedure TDptLintStyleValidator.ValidateStyleFile(const AStylePath: string);
var
  LLines: TArray<string>;
  I, J: Integer;
  LAnchorFound: Boolean;
  LCol2Idx, LCol3Idx: Integer;
  LLine: string;
begin
  if not TFile.Exists(AStylePath) then
    Exit;

  LLines := TFile.ReadAllLines(AStylePath, TEncoding.UTF8);
  if Length(LLines) = 0 then Exit;

  LAnchorFound := False;
  LCol2Idx := -1;
  LCol3Idx := -1;

  // 1. Find Anchor Line
  for I := 0 to High(LLines) do
  begin
    var LLineUpper := LLines[I].ToUpper;
    if LLineUpper.Contains('// START: STYLE-TEMPLATE') and
       LLineUpper.Contains('// START: AI-DESCRIPTIONS') and
       LLineUpper.Contains('// START: AI-GENERATED FITNESSE-TEST') then
    begin
      LCol2Idx := LLineUpper.IndexOf('// START: AI-DESCRIPTIONS');
      LCol3Idx := LLineUpper.IndexOf('// START: AI-GENERATED FITNESSE-TEST');

      LAnchorFound := True;
      Break;
    end;
  end;

  if not LAnchorFound then
    raise Exception.Create('Style file anchor line not found (missing // START: STYLE-TEMPLATE etc.).');

  // 2. Validate all following lines strictly
  for J := I + 1 to High(LLines) do
  begin
    LLine := LLines[J];

    // Skip completely empty lines at the end of file if they occur
    if (J = High(LLines)) and (LLine.Trim = '') then
      Continue;

    // Check Column 2
    if (LLine.Length < LCol2Idx + 2) or (LLine.Chars[LCol2Idx] <> '/') or (LLine.Chars[LCol2Idx+1] <> '/') then
      raise Exception.CreateFmt('Style alignment error in file "%s" at line %d. Missing or misaligned "//" at column %d (Column 2).',
        [ExtractFileName(AStylePath), J + 1, LCol2Idx + 1]);

    // Check Column 3
    if (LLine.Length < LCol3Idx + 2) or (LLine.Chars[LCol3Idx] <> '/') or (LLine.Chars[LCol3Idx+1] <> '/') then
      raise Exception.CreateFmt('Style alignment error in file "%s" at line %d. Missing or misaligned "//" at column %d (Column 3).',
        [ExtractFileName(AStylePath), J + 1, LCol3Idx + 1]);
  end;
end;

end.
