unit Test.DPT.Formatter.Dws;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  DUnitX.TestFramework,
  ParseTree.Core, ParseTree.Nodes, ParseTree.Parser, ParseTree.Writer,
  DPT.Formatter, DPT.Formatter.DWS;

type
  [TestFixture]
  TDptDwsFormatterTests = class
  private
    FParser: TParseTreeParser;
    FWriter: TSyntaxTreeWriter;
    FFormatter: TDptDwsFormatter;
    FScriptFile: string;
    function FormatWithScript(const AScript: string): string;
    function ScriptEmittingUnitTrivia(const AExpression: string): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestDwsIntegration;
    [Test]
    procedure TestGetEnvironmentVariable_ReturnsValue;
    [Test]
    procedure TestGetEnvironmentVariable_UnknownNameIsEmpty;
    [Test]
    procedure TestGetUserDisplayName_MatchesWindowsDisplayName;
  end;

implementation

const
  TestEnvVarName = 'DPT_TEST_FORMATTER_ENV_VAR';

// Independent oracle for the display name, deliberately not shared with the
// production code: GetUserNameEx(NameDisplay) with the login name as fallback,
// the same source the IDE expert "Unit-Header aktualisieren" uses.
function GetUserNameExW(NameFormat: DWORD; lpNameBuffer: LPWSTR; var nSize: DWORD): BOOL; stdcall;
  external 'secur32.dll';

function ExpectedUserDisplayName: string;
const
  NameDisplay = 3;
var
  LBuffer: array[0..1023] of Char;
  LLen: DWORD;
begin
  Result := '';
  LLen := Length(LBuffer);
  if GetUserNameExW(NameDisplay, LBuffer, LLen) then
    SetString(Result, LBuffer, LLen);
  if Trim(Result) = '' then
    Result := System.SysUtils.GetEnvironmentVariable('USERNAME');
end;

{ TDptDwsFormatterTests }

procedure TDptDwsFormatterTests.Setup;
begin
  FParser := TParseTreeParser.Create;
  FWriter := TSyntaxTreeWriter.Create;
  FFormatter := TDptDwsFormatter.Create;
  FScriptFile := TPath.Combine(TPath.GetTempPath, TGUID.NewGuid.ToString + '.pas');
end;

procedure TDptDwsFormatterTests.TearDown;
begin
  if TFile.Exists(FScriptFile) then
    TFile.Delete(FScriptFile);
  FFormatter.Free;
  FWriter.Free;
  FParser.Free;
end;

function TDptDwsFormatterTests.FormatWithScript(const AScript: string): string;
var
  LUnit: TCompilationUnitSyntax;
begin
  TFile.WriteAllText(FScriptFile, AScript);
  LUnit := FParser.Parse('unit MyUnit; interface end.');
  try
    FFormatter.LoadScript(FScriptFile);
    FFormatter.FormatUnit(LUnit);
    Result := FWriter.GenerateSource(LUnit);
  finally
    LUnit.Free;
  end;
end;

// Builds a script whose OnVisitUnitStart writes "// VALUE=[<expression>]" in
// front of the unit keyword, so the test can read a script-side value back
// from the formatted source.
function TDptDwsFormatterTests.ScriptEmittingUnitTrivia(const AExpression: string): string;
begin
  Result :=
    'procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax);' + sLineBreak +
    'begin' + sLineBreak +
    '  AddLeadingTrivia(GetUnitKeyword(AUnit), ''// VALUE=['' + ' + AExpression + ' + '']'' + #13#10);' + sLineBreak +
    'end;' + sLineBreak;
end;

procedure TDptDwsFormatterTests.TestDwsIntegration;
var
  LUnit: TCompilationUnitSyntax;
  LSource, LResult, LScriptCache: string;
begin
  // A DWScript that modifies the AUses UsesKeyword
  LScriptCache :=
    'procedure OnVisitUsesClause(AUses: TUsesClauseSyntax);' + sLineBreak +
    'begin' + sLineBreak +
    '  ClearTrivia(GetUsesKeyword(AUses));' + sLineBreak +
    '  AddLeadingTrivia(GetUsesKeyword(AUses), ''// FORMATTED'' + #13#10);' + sLineBreak +
    '  AddTrailingTrivia(GetUsesKeyword(AUses), '' '');' + sLineBreak +
    'end;' + sLineBreak;

  TFile.WriteAllText(FScriptFile, LScriptCache);

  LSource := 'unit MyUnit; interface uses System.SysUtils; end.';
  LUnit := FParser.Parse(LSource);
  try
    FFormatter.LoadScript(FScriptFile);
    FFormatter.FormatUnit(LUnit);
    LResult := FWriter.GenerateSource(LUnit);

    // Check if the script correctly added the trivia
    Assert.IsTrue(LResult.Contains('// FORMATTED'), 'Script should add the // FORMATTED comment to uses');
  finally
    LUnit.Free;
  end;
end;

procedure TDptDwsFormatterTests.TestGetEnvironmentVariable_ReturnsValue;
var
  LResult: string;
begin
  Winapi.Windows.SetEnvironmentVariable(PChar(TestEnvVarName), 'hello world');
  try
    LResult := FormatWithScript(ScriptEmittingUnitTrivia('GetEnvironmentVariable(''' + TestEnvVarName + ''')'));
  finally
    Winapi.Windows.SetEnvironmentVariable(PChar(TestEnvVarName), nil);
  end;

  Assert.IsTrue(LResult.Contains('// VALUE=[hello world]'),
    'Script must read the environment variable of the host process. Actual:' + sLineBreak + LResult);
end;

procedure TDptDwsFormatterTests.TestGetEnvironmentVariable_UnknownNameIsEmpty;
var
  LResult: string;
begin
  Winapi.Windows.SetEnvironmentVariable(PChar(TestEnvVarName), nil);

  LResult := FormatWithScript(ScriptEmittingUnitTrivia('GetEnvironmentVariable(''' + TestEnvVarName + ''')'));

  Assert.IsTrue(LResult.Contains('// VALUE=[]'),
    'An unknown environment variable must read as empty string. Actual:' + sLineBreak + LResult);
end;

procedure TDptDwsFormatterTests.TestGetUserDisplayName_MatchesWindowsDisplayName;
var
  LResult: string;
  LExpected: string;
begin
  LExpected := ExpectedUserDisplayName;
  Assert.IsNotEmpty(LExpected, 'Test precondition: Windows must report a user name');

  LResult := FormatWithScript(ScriptEmittingUnitTrivia('GetUserDisplayName'));

  Assert.IsTrue(LResult.Contains('// VALUE=[' + LExpected + ']'),
    'Script must see the Windows display name "' + LExpected + '". Actual:' + sLineBreak + LResult);
end;

end.
