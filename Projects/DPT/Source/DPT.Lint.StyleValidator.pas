// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Lint.StyleValidator;

interface

uses

  System.Classes,
  System.SysUtils,
  System.IOUtils;

type

  TDptLintStyleValidator = class
  public
    class procedure ValidateStyleFile(const AStylePath: String);
  end;

implementation

{ TDptLintStyleValidator }

class procedure TDptLintStyleValidator.ValidateStyleFile(const AStylePath: String);
var
  AnchorFound: Boolean;
  Col2Idx    : Integer;
  Col3Idx    : Integer;
  I          : Integer;
  J          : Integer;
  Line       : String;
  Lines      : TArray<String>;
begin
  if not TFile.Exists(AStylePath) then
    Exit;

  Lines := TFile.ReadAllLines(AStylePath, TEncoding.UTF8);
  if Length(Lines) = 0 then
    Exit;

  AnchorFound := False;
  Col2Idx := -1;
  Col3Idx := -1;

  // 1. Find Anchor Line
  for I := 0 to High(Lines) do
  begin
    var LLineUpper := Lines[I].ToUpper;
    if LLineUpper.Contains('// START: STYLE-TEMPLATE') and
       LLineUpper.Contains('// START: AI-DESCRIPTIONS') and
       LLineUpper.Contains('// START: AI-GENERATED FITNESSE-TEST') then
    begin
      Col2Idx := LLineUpper.IndexOf('// START: AI-DESCRIPTIONS');
      Col3Idx := LLineUpper.IndexOf('// START: AI-GENERATED FITNESSE-TEST');

      AnchorFound := True;
      Break;
    end;
  end;

  if not AnchorFound then
    raise Exception.Create('Style file anchor line not found (missing // START: STYLE-TEMPLATE etc.).');

  // 2. Validate all following lines strictly
  for J := I + 1 to High(Lines) do
  begin
    Line := Lines[J];

    // Skip completely empty lines at the end of file if they occur
    if (J = High(Lines)) and (Line.Trim = '') then
      Continue;

    // Check Column 2
    if (Line.Length < Col2Idx + 2) or (Line.Chars[Col2Idx] <> '/') or (Line.Chars[Col2Idx+1] <> '/') then
      raise Exception.CreateFmt('Style alignment error in file "%s" at line %d. Missing or misaligned "//" at column %d (Column 2).',
        [ExtractFileName(AStylePath), J + 1, Col2Idx + 1]);

    // Check Column 3
    if (Line.Length < Col3Idx + 2) or (Line.Chars[Col3Idx] <> '/') or (Line.Chars[Col3Idx+1] <> '/') then
      raise Exception.CreateFmt('Style alignment error in file "%s" at line %d. Missing or misaligned "//" at column %d (Column 3).',
        [ExtractFileName(AStylePath), J + 1, Col3Idx + 1]);
  end;
end;

end.
