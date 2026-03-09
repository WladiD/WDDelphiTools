unit Test.ParseTree.Lexer;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Tokens,
  ParseTree.Lexer;

type
  [TestFixture]
  TParseTreeLexerTest = class
  public
    [Test]
    [TestCase('While', 'while,tkWhileKeyword')]
    [TestCase('For', 'for,tkForKeyword')]
    [TestCase('To', 'to,tkToKeyword')]
    [TestCase('Downto', 'downto,tkDowntoKeyword')]
    [TestCase('Do', 'do,tkDoKeyword')]
    [TestCase('Repeat', 'repeat,tkRepeatKeyword')]
    [TestCase('Until', 'until,tkUntilKeyword')]
    procedure TestLoopKeywords(const ASource: string; const AExpectedKindStr: string);
    [Test]
    procedure TestNotEqualsOperator;
    [Test]
    procedure TestLessOrEqualsOperator;
    [Test]
    procedure TestGreaterOrEqualsOperator;
  end;

implementation

uses
  System.TypInfo;

procedure TParseTreeLexerTest.TestLoopKeywords(const ASource: string; const AExpectedKindStr: string);
var
  LLexer: TParseTreeLexer;
  LToken: TSyntaxToken;
  LExpectedKind: TTokenKind;
begin
  LExpectedKind := TTokenKind(GetEnumValue(TypeInfo(TTokenKind), AExpectedKindStr));
  LLexer := TParseTreeLexer.Create(ASource);
  try
    LToken := LLexer.NextToken;
    Assert.IsNotNull(LToken);
    Assert.AreEqual(LExpectedKind, LToken.Kind, 'Expected token kind mapping failure');
    Assert.AreEqual(ASource, LToken.Text, 'Expected token text mapping failure');
  finally
    LLexer.Free;
  end;
end;

procedure TParseTreeLexerTest.TestNotEqualsOperator;
var
  LLexer: TParseTreeLexer;
  LToken: TSyntaxToken;
begin
  LLexer := TParseTreeLexer.Create('<>');
  try
    LToken := LLexer.NextToken;
    Assert.IsNotNull(LToken);
    Assert.AreEqual(TTokenKind.tkNotEquals, LToken.Kind, 'Expected <> to be a single not-equals token');
    Assert.AreEqual('<>', LToken.Text);
  finally
    LLexer.Free;
  end;
end;

procedure TParseTreeLexerTest.TestLessOrEqualsOperator;
var
  LLexer: TParseTreeLexer;
  LToken: TSyntaxToken;
begin
  LLexer := TParseTreeLexer.Create('<=');
  try
    LToken := LLexer.NextToken;
    Assert.IsNotNull(LToken);
    Assert.AreEqual(TTokenKind.tkLessOrEquals, LToken.Kind, 'Expected <= to be a single less-or-equals token');
    Assert.AreEqual('<=', LToken.Text);
  finally
    LLexer.Free;
  end;
end;

procedure TParseTreeLexerTest.TestGreaterOrEqualsOperator;
var
  LLexer: TParseTreeLexer;
  LToken: TSyntaxToken;
begin
  LLexer := TParseTreeLexer.Create('>=');
  try
    LToken := LLexer.NextToken;
    Assert.IsNotNull(LToken);
    Assert.AreEqual(TTokenKind.tkGreaterOrEquals, LToken.Kind, 'Expected >= to be a single greater-or-equals token');
    Assert.AreEqual('>=', LToken.Text);
  finally
    LLexer.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeLexerTest);

end.
