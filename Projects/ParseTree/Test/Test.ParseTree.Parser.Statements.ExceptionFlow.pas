unit Test.ParseTree.Parser.Statements.ExceptionFlow;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Nodes,
  ParseTree.Tokens,
  ParseTree.Parser,
  ParseTree.Writer;
type
  [TestFixture]
  TParseTreeParserStatementsExceptionFlowTest = class
  public
    [Test]
    procedure TestTryFinallySimple;
    [Test]
    procedure TestTryFinallyMultipleStatements;
    [Test]
    procedure TestTryExceptSimple;
    [Test]
    procedure TestTryExceptWithOnHandler;
    [Test]
    procedure TestTryFinallyNested;
    [Test]
    procedure TestTryFinallyInBeginEnd;
    [Test]
    procedure TestTryFinallyInIfThenElse;
    [Test]
    procedure TestTryExceptNestedInTryFinally;
    [Test]
    procedure TestTryFinallyWithBeginEndBodies;
    [Test]
    procedure TestTryFinallyEmpty;
    [Test]
    procedure TestTryFinallyDeepNesting;
    [Test]
    procedure TestRaiseSimple;
    [Test]
    procedure TestRaiseBare;
    [Test]
    procedure TestRaiseWithMethodCall;
    [Test]
    procedure TestRaiseWithNestedParens;
    [Test]
    procedure TestRaiseInExceptBlock;
    [Test]
    procedure TestRaiseInIfThenElse;
    [Test]
    procedure TestRaiseInBeginEnd;
  end;

implementation

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryFinallySimple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try x := 1; finally x := 0; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LTry.TryKeyword);
    Assert.AreEqual('try', LTry.TryKeyword.Text);

    Assert.AreEqual(1, LTry.Statements.Count, 'Try body should have 1 statement');
    Assert.IsTrue(LTry.Statements[0] is TAssignmentStatementSyntax);

    Assert.IsNotNull(LTry.FinallyKeyword);
    Assert.AreEqual('finally', LTry.FinallyKeyword.Text);
    Assert.IsNull(LTry.ExceptKeyword);

    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count, 'Finally body should have 1 statement');
    Assert.IsTrue(LTry.FinallyExceptStatements[0] is TAssignmentStatementSyntax);

    Assert.IsNotNull(LTry.EndKeyword);
    Assert.AreEqual('end', LTry.EndKeyword.Text);
    Assert.IsNotNull(LTry.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryFinallyMultipleStatements;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try a := 1; b := 2; c := 3; finally d := 4; e := 5; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LTry.Statements.Count, 'Try body should have 3 statements');
    Assert.AreEqual(2, LTry.FinallyExceptStatements.Count, 'Finally body should have 2 statements');

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryExceptSimple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try x := 1; except y := 0; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LTry.TryKeyword);

    Assert.AreEqual(1, LTry.Statements.Count, 'Try body should have 1 statement');
    Assert.IsTrue(LTry.Statements[0] is TAssignmentStatementSyntax);

    Assert.IsNull(LTry.FinallyKeyword);
    Assert.IsNotNull(LTry.ExceptKeyword);
    Assert.AreEqual('except', LTry.ExceptKeyword.Text);

    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count, 'Except body should have 1 statement');

    Assert.IsNotNull(LTry.EndKeyword);
    Assert.IsNotNull(LTry.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryExceptWithOnHandler;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try DoSomething; except on E: Exception do HandleError(E); end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LTry.ExceptKeyword);
    Assert.AreEqual(1, LTry.Statements.Count, 'Try body should have 1 statement');
    Assert.IsTrue(LTry.FinallyExceptStatements.Count >= 1, 'Except body should have at least 1 handler');

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryFinallyNested;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LOuterTry, LInnerTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try try x := 1; finally x := 0; end; finally y := 0; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LOuterTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LOuterTry.FinallyKeyword);
    Assert.AreEqual(1, LOuterTry.Statements.Count, 'Outer try body should have 1 statement');
    Assert.IsTrue(LOuterTry.Statements[0] is TTryStatementSyntax);

    LInnerTry := TTryStatementSyntax(LOuterTry.Statements[0]);
    Assert.IsNotNull(LInnerTry.FinallyKeyword);
    Assert.AreEqual(1, LInnerTry.Statements.Count, 'Inner try body should have 1 statement');
    Assert.AreEqual(1, LInnerTry.FinallyExceptStatements.Count, 'Inner finally body should have 1 statement');
    Assert.IsNotNull(LInnerTry.EndKeyword);
    Assert.IsNotNull(LInnerTry.Semicolon);

    Assert.AreEqual(1, LOuterTry.FinallyExceptStatements.Count, 'Outer finally body should have 1 statement');

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryFinallyInBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LBlock: TBeginEndStatementSyntax;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin begin try x := 1; finally x := 0; end; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TBeginEndStatementSyntax);

    LBlock := TBeginEndStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LBlock.Statements[0]);
    Assert.IsNotNull(LTry.FinallyKeyword);
    Assert.AreEqual(1, LTry.Statements.Count);
    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryFinallyInIfThenElse;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
  LTryThen, LTryElse: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin if x then try a := 1; finally a := 0; end else try b := 1; finally b := 0; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);

    Assert.IsTrue(LIf.ThenStatement is TTryStatementSyntax);
    LTryThen := TTryStatementSyntax(LIf.ThenStatement);
    Assert.IsNotNull(LTryThen.FinallyKeyword);
    Assert.AreEqual(1, LTryThen.Statements.Count);
    Assert.AreEqual(1, LTryThen.FinallyExceptStatements.Count);
    Assert.IsNull(LTryThen.Semicolon, 'No semicolon before else');

    Assert.IsNotNull(LIf.ElseKeyword);
    Assert.IsTrue(LIf.ElseStatement is TTryStatementSyntax);
    LTryElse := TTryStatementSyntax(LIf.ElseStatement);
    Assert.IsNotNull(LTryElse.FinallyKeyword);
    Assert.IsNotNull(LTryElse.Semicolon, 'Semicolon after else try-end');

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryExceptNestedInTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LOuterTry, LInnerTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try try x := 1; except y := 0; end; finally z := 0; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LOuterTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LOuterTry.FinallyKeyword, 'Outer should be try-finally');
    Assert.AreEqual(1, LOuterTry.Statements.Count);
    Assert.IsTrue(LOuterTry.Statements[0] is TTryStatementSyntax);

    LInnerTry := TTryStatementSyntax(LOuterTry.Statements[0]);
    Assert.IsNotNull(LInnerTry.ExceptKeyword, 'Inner should be try-except');
    Assert.AreEqual(1, LInnerTry.Statements.Count);
    Assert.AreEqual(1, LInnerTry.FinallyExceptStatements.Count);
    Assert.IsNotNull(LInnerTry.EndKeyword);
    Assert.IsNotNull(LInnerTry.Semicolon);

    Assert.AreEqual(1, LOuterTry.FinallyExceptStatements.Count);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryFinallyWithBeginEndBodies;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try begin a := 1; b := 2; end; finally begin c := 3; end; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LTry.Statements.Count, 'Try body: 1 begin-end block');
    Assert.IsTrue(LTry.Statements[0] is TBeginEndStatementSyntax);

    var LTryBlock := TBeginEndStatementSyntax(LTry.Statements[0]);
    Assert.AreEqual(2, LTryBlock.Statements.Count, 'Begin-end in try should have 2 assignments');

    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count, 'Finally body: 1 begin-end block');
    Assert.IsTrue(LTry.FinallyExceptStatements[0] is TBeginEndStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryFinallyEmpty;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try finally end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(0, LTry.Statements.Count, 'Empty try body');
    Assert.IsNotNull(LTry.FinallyKeyword);
    Assert.AreEqual(0, LTry.FinallyExceptStatements.Count, 'Empty finally body');
    Assert.IsNotNull(LTry.EndKeyword);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestTryFinallyDeepNesting;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
  LIf: TIfStatementSyntax;
  LBlock: TBeginEndStatementSyntax;
  LInnerTry: TTryStatementSyntax;
begin
  LSource :=
    'procedure Complex; ' + #13#10 +
    'begin' + #13#10 +
    '  try' + #13#10 +
    '    if a then' + #13#10 +
    '    begin' + #13#10 +
    '      try' + #13#10 +
    '        x := 1;' + #13#10 +
    '      except' + #13#10 +
    '        x := 0;' + #13#10 +
    '      end;' + #13#10 +
    '    end;' + #13#10 +
    '  finally' + #13#10 +
    '    y := 0;' + #13#10 +
    '  end;' + #13#10 +
    'end;';

  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LTry.FinallyKeyword);
    Assert.AreEqual(1, LTry.Statements.Count, 'Outer try body: 1 if statement');
    Assert.IsTrue(LTry.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LTry.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TBeginEndStatementSyntax);
    LBlock := TBeginEndStatementSyntax(LIf.ThenStatement);
    Assert.AreEqual(1, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TTryStatementSyntax);

    LInnerTry := TTryStatementSyntax(LBlock.Statements[0]);
    Assert.IsNotNull(LInnerTry.ExceptKeyword, 'Inner should be try-except');
    Assert.AreEqual(1, LInnerTry.Statements.Count);
    Assert.AreEqual(1, LInnerTry.FinallyExceptStatements.Count);

    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count, 'Finally body: 1 assignment');
    Assert.IsTrue(LTry.FinallyExceptStatements[0] is TAssignmentStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult, 'Roundtrip should be exact');
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestRaiseSimple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRaise: TRaiseStatementSyntax;
begin
  LSource := 'procedure Foo; begin raise Exception.Create(''Error''); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRaiseStatementSyntax);

    LRaise := TRaiseStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LRaise.RaiseKeyword);
    Assert.AreEqual('raise', LRaise.RaiseKeyword.Text);
    Assert.IsTrue(LRaise.ExpressionTokens.Count > 0, 'Should have expression tokens');
    Assert.AreEqual('Exception', LRaise.ExpressionTokens[0].Text);
    Assert.IsNotNull(LRaise.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestRaiseBare;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRaise: TRaiseStatementSyntax;
begin
  LSource := 'procedure Foo; begin raise; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRaiseStatementSyntax);

    LRaise := TRaiseStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LRaise.RaiseKeyword);
    Assert.AreEqual(0, LRaise.ExpressionTokens.Count, 'Bare raise should have no expression');
    Assert.IsNotNull(LRaise.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestRaiseWithMethodCall;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRaise: TRaiseStatementSyntax;
begin
  LSource := 'procedure Foo; begin raise EInvalidOperation.CreateFmt(''Error: %s'', [Msg]); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRaiseStatementSyntax);

    LRaise := TRaiseStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LRaise.RaiseKeyword);
    Assert.AreEqual('EInvalidOperation', LRaise.ExpressionTokens[0].Text);
    Assert.IsNotNull(LRaise.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestRaiseWithNestedParens;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRaise: TRaiseStatementSyntax;
begin
  LSource := 'procedure Foo; begin raise Exception.Create(Format(''%s (%d)'', [S, I])); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRaiseStatementSyntax);

    LRaise := TRaiseStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LRaise.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestRaiseInExceptBlock;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try DoWork; except raise; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LTry.ExceptKeyword);
    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count);
    Assert.IsTrue(LTry.FinallyExceptStatements[0] is TRaiseStatementSyntax);

    var LRaise := TRaiseStatementSyntax(LTry.FinallyExceptStatements[0]);
    Assert.AreEqual(0, LRaise.ExpressionTokens.Count, 'Should be bare raise');
    Assert.IsNotNull(LRaise.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestRaiseInIfThenElse;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure Foo; begin if x then raise Exception.Create(''A'') else raise Exception.Create(''B''); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TRaiseStatementSyntax, 'Then branch should be raise');
    var LThenRaise := TRaiseStatementSyntax(LIf.ThenStatement);
    Assert.IsNull(LThenRaise.Semicolon, 'No semicolon before else');

    Assert.IsNotNull(LIf.ElseKeyword);
    Assert.IsTrue(LIf.ElseStatement is TRaiseStatementSyntax, 'Else branch should be raise');
    var LElseRaise := TRaiseStatementSyntax(LIf.ElseStatement);
    Assert.IsNotNull(LElseRaise.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsExceptionFlowTest.TestRaiseInBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin begin x := 1; raise Exception.Create(''Error''); y := 2; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TBeginEndStatementSyntax);

    LBlock := TBeginEndStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsTrue(LBlock.Statements[1] is TRaiseStatementSyntax);
    Assert.IsTrue(LBlock.Statements[2] is TAssignmentStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsExceptionFlowTest);

end.
