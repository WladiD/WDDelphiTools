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
    [Test]
    procedure TestDispinterfaceKeyword;
    [Test]
    [TestCase('EscapedEnd', '&End')]
    [TestCase('EscapedRegister', '&Register')]
    [TestCase('EscapedType', '&Type')]
    procedure TestEscapedReservedWordIsSingleIdentifier(const ASource: string);
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

procedure TParseTreeLexerTest.TestDispinterfaceKeyword;
var
  LLexer: TParseTreeLexer;
  LToken: TSyntaxToken;
begin
  LLexer := TParseTreeLexer.Create('dispinterface');
  try
    LToken := LLexer.NextToken;
    Assert.IsNotNull(LToken);
    Assert.AreEqual(TTokenKind.tkDispinterfaceKeyword, LToken.Kind);
    Assert.AreEqual('dispinterface', LToken.Text);
  finally
    LLexer.Free;
  end;
end;

procedure TParseTreeLexerTest.TestEscapedReservedWordIsSingleIdentifier(const ASource: string);
var
  LLexer: TParseTreeLexer;
  LToken: TSyntaxToken;
  LNext : TSyntaxToken;
begin
  // An '&'-escaped reserved word (e.g. '&End') must lex as ONE identifier
  // token carrying the '&' prefix - not as a stray '&' followed by the
  // reserved keyword, which would derail the class-member parser.
  LLexer := TParseTreeLexer.Create(ASource);
  try
    LToken := LLexer.NextToken;
    Assert.IsNotNull(LToken);
    Assert.AreEqual(TTokenKind.tkIdentifier, LToken.Kind,
      'Escaped reserved word must lex as an identifier, not a keyword');
    Assert.AreEqual(ASource, LToken.Text,
      'Escaped identifier must keep its ''&'' prefix in the token text');

    LNext := LLexer.NextToken;
    Assert.AreEqual(TTokenKind.tkEOF, LNext.Kind,
      'Escaped reserved word must not split into multiple tokens');
  finally
    LLexer.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeLexerTest);

end.
