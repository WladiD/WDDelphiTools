unit Test.DPT.LintTask;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  DUnitX.TestFramework,
  DPT.Lint.StyleValidator;

type
  [TestFixture]
  TDptLintStyleValidatorTests = class
  private
    FTempDir: string;
    FStyleFile: string;
    procedure CreateStyleFile(const Content: string);
    function GetAnchorLine: string;
    function GetAlignedLine(const Col1, Col2, Col3: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ValidateStyle_Valid;
    [Test]
    procedure ValidateStyle_InvalidAlignment_Col2;
    [Test]
    procedure ValidateStyle_InvalidAlignment_Col3;
    [Test]
    procedure ValidateStyle_IgnoreCodeComments;
    [Test]
    procedure ValidateStyle_MissingAnchor;
  end;

implementation

{ TDptLintStyleValidatorTests }

procedure TDptLintStyleValidatorTests.Setup;
begin
  FTempDir := TPath.Combine(TPath.GetTempPath, TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempDir);
  FStyleFile := TPath.Combine(FTempDir, 'Style.pas');
end;

procedure TDptLintStyleValidatorTests.TearDown;
begin
  if TDirectory.Exists(FTempDir) then
    TDirectory.Delete(FTempDir, True);
end;

procedure TDptLintStyleValidatorTests.CreateStyleFile(const Content: string);
begin
  TFile.WriteAllText(FStyleFile, Content);
end;

function TDptLintStyleValidatorTests.GetAnchorLine: string;
begin
  // Using exact casing from TaifunUnitStyle.pas line 1
  Result := GetAlignedLine('// START: STYLE-TEMPLATE', '// START: AI-Descriptions', '// START: AI-Generated FitNesse-Test');
end;

function TDptLintStyleValidatorTests.GetAlignedLine(const Col1, Col2, Col3: string): string;
begin
  // Col 1 starts at 0
  // Col 2 starts at 40
  // Col 3 starts at 100 (wider to accommodate labels)
  Result := Col1 + StringOfChar(' ', 40 - Col1.Length) + 
            Col2 + StringOfChar(' ', 100 - (40 + Col2.Length)) + 
            Col3;
end;

procedure TDptLintStyleValidatorTests.ValidateStyle_Valid;
begin
  CreateStyleFile(
    GetAnchorLine + sLineBreak +
    GetAlignedLine('unit MyUnit;', '// Description', '// |script|test|') + sLineBreak +
    GetAlignedLine('interface', '// Interface part', '// |check|true|')
  );
  
  Assert.WillNotRaise(
    procedure
    begin
      TDptLintStyleValidator.ValidateStyleFile(FStyleFile);
    end);
end;

procedure TDptLintStyleValidatorTests.ValidateStyle_InvalidAlignment_Col2;
begin
  CreateStyleFile(
    GetAnchorLine + sLineBreak +
    'unit MyUnit;                        ' + '// Wrong Pos' + StringOfChar(' ', 40) + '// Col3'
  );

  Assert.WillRaise(
    procedure
    begin
      TDptLintStyleValidator.ValidateStyleFile(FStyleFile);
    end,
    Exception,
    'Should raise exception for misaligned 2nd column');
end;

procedure TDptLintStyleValidatorTests.ValidateStyle_InvalidAlignment_Col3;
begin
  CreateStyleFile(
    GetAnchorLine + sLineBreak +
    GetAlignedLine('unit MyUnit;', '// Correct', '') + ' // Wrong Pos'
  );

  Assert.WillRaise(
    procedure
    begin
      TDptLintStyleValidator.ValidateStyleFile(FStyleFile);
    end,
    Exception,
    'Should raise exception for misaligned 3rd column');
end;

procedure TDptLintStyleValidatorTests.ValidateStyle_IgnoreCodeComments;
begin
  // Line has a comment in Col 1 area, but valid markers at 40 and 100
  var LLine := 'var x: Integer; // comment here';
  LLine := LLine + StringOfChar(' ', 40 - LLine.Length) + '// Desc' + StringOfChar(' ', 100 - (40 + 7)) + '// Test';

  CreateStyleFile(
    GetAnchorLine + sLineBreak +
    LLine
  );

  Assert.WillNotRaise(
    procedure
    begin
      TDptLintStyleValidator.ValidateStyleFile(FStyleFile);
    end);
end;

procedure TDptLintStyleValidatorTests.ValidateStyle_MissingAnchor;
begin
  CreateStyleFile(
    '// Simple Header' + sLineBreak +
    'Code...             ' + '// Desc...           ' + '// Test'
  );

  Assert.WillRaise(
    procedure
    begin
      TDptLintStyleValidator.ValidateStyleFile(FStyleFile);
    end,
    Exception);
end;

end.