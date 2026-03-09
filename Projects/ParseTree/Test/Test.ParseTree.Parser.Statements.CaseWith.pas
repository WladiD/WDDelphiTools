unit Test.ParseTree.Parser.Statements.CaseWith;

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
  TParseTreeParserStatementsCaseWithTest = class
  public
    [Test]
    procedure TestCaseSimple;
    [Test]
    procedure TestCaseMultipleItems;
    [Test]
    procedure TestCaseWithElse;
    [Test]
    procedure TestCaseCommaValues;
    [Test]
    procedure TestCaseRangeValues;
    [Test]
    procedure TestCaseStringValues;
    [Test]
    procedure TestCaseWithBeginEnd;
    [Test]
    procedure TestCaseNested;
    [Test]
    procedure TestCaseInIfThenElse;
    [Test]
    procedure TestCaseInBeginEnd;
    [Test]
    procedure TestCaseInTryFinally;
    [Test]
    procedure TestCaseEmpty;
    [Test]
    procedure TestCaseElseOnly;
    [Test]
    procedure TestCaseDeepNesting;
    [Test]
    procedure TestCaseItemWithEmptyStatement;
    [Test]
    procedure TestWithSimple;
    [Test]
    procedure TestWithQualifiedExpression;
    [Test]
    procedure TestWithMultipleExpressions;
    [Test]
    procedure TestWithBeginEnd;
    [Test]
    procedure TestWithNested;
    [Test]
    procedure TestWithInBeginEnd;
    [Test]
    procedure TestWithInIfThenElse;
    [Test]
    procedure TestWithInTryFinally;
    [Test]
    procedure TestWithInForLoop;
    [Test]
    procedure TestWithMethodCall;
    [Test]
    procedure TestWithAssignment;
    [Test]
    procedure TestWithDeepNesting;
  end;

implementation

procedure TParseTreeParserStatementsCaseWithTest.TestCaseSimple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of 1: DoA; 2: DoB; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCase.CaseKeyword);
    Assert.AreEqual('case', LCase.CaseKeyword.Text);
    Assert.AreEqual(1, LCase.ExpressionTokens.Count);
    Assert.AreEqual('X', LCase.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCase.OfKeyword);
    Assert.AreEqual('of', LCase.OfKeyword.Text);
    Assert.AreEqual(2, LCase.CaseItems.Count);
    Assert.IsNotNull(LCase.CaseItems[0].ColonToken);
    Assert.IsTrue(LCase.CaseItems[0].Statement is TProcedureCallStatementSyntax);
    Assert.IsNotNull(LCase.CaseItems[1].ColonToken);
    Assert.IsTrue(LCase.CaseItems[1].Statement is TProcedureCallStatementSyntax);
    Assert.IsNull(LCase.ElseKeyword);
    Assert.IsNotNull(LCase.EndKeyword);
    Assert.IsNotNull(LCase.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseMultipleItems;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of 1: A; 2: B; 3: C; 4: D; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(4, LCase.CaseItems.Count);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseWithElse;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of 1: DoA; 2: DoB; else DoDefault; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(2, LCase.CaseItems.Count);
    Assert.IsNotNull(LCase.ElseKeyword);
    Assert.AreEqual('else', LCase.ElseKeyword.Text);
    Assert.AreEqual(1, LCase.ElseStatements.Count);
    Assert.IsTrue(LCase.ElseStatements[0] is TProcedureCallStatementSyntax);
    Assert.IsNotNull(LCase.EndKeyword);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseCommaValues;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of 1, 2, 3: DoA; 4, 5: DoB; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(2, LCase.CaseItems.Count);
    Assert.AreEqual(5, LCase.CaseItems[0].ValueTokens.Count, '1 , 2 , 3 = 5 tokens');
    Assert.AreEqual(3, LCase.CaseItems[1].ValueTokens.Count, '4 , 5 = 3 tokens');

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseRangeValues;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of 1..10: DoA; 11..20: DoB; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(2, LCase.CaseItems.Count);
    Assert.IsNotNull(LCase.CaseItems[0].ColonToken);
    Assert.IsNotNull(LCase.CaseItems[1].ColonToken);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseStringValues;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case C of ''a'': DoA; ''b'', ''c'': DoB; else DoDefault; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(2, LCase.CaseItems.Count);
    Assert.IsNotNull(LCase.ElseKeyword);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseWithBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of 1: begin DoA; DoB; end; 2: DoC; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(2, LCase.CaseItems.Count);
    Assert.IsTrue(LCase.CaseItems[0].Statement is TBeginEndStatementSyntax);
    var LBlock := TBeginEndStatementSyntax(LCase.CaseItems[0].Statement);
    Assert.AreEqual(2, LBlock.Statements.Count);
    Assert.IsTrue(LCase.CaseItems[1].Statement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseNested;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase, LInner: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of 1: case Y of 10: DoA; 20: DoB; end; 2: DoC; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(2, LCase.CaseItems.Count);
    Assert.IsTrue(LCase.CaseItems[0].Statement is TCaseStatementSyntax);

    LInner := TCaseStatementSyntax(LCase.CaseItems[0].Statement);
    Assert.AreEqual(2, LInner.CaseItems.Count);
    Assert.IsNotNull(LInner.EndKeyword);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseInIfThenElse;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure Foo; begin if a then case X of 1: DoA; end else DoB; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TCaseStatementSyntax);
    var LCase := TCaseStatementSyntax(LIf.ThenStatement);
    Assert.IsNull(LCase.Semicolon, 'No semicolon before else');

    Assert.IsNotNull(LIf.ElseKeyword);
    Assert.IsTrue(LIf.ElseStatement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseInBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin begin case X of 1: DoA; end; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TBeginEndStatementSyntax);

    LBlock := TBeginEndStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TCaseStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseInTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try case X of 1: DoA; end; finally Cleanup; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LTry.Statements.Count);
    Assert.IsTrue(LTry.Statements[0] is TCaseStatementSyntax);
    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseEmpty;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(0, LCase.CaseItems.Count);
    Assert.IsNull(LCase.ElseKeyword);
    Assert.IsNotNull(LCase.EndKeyword);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseElseOnly;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of else DoDefault; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(0, LCase.CaseItems.Count);
    Assert.IsNotNull(LCase.ElseKeyword);
    Assert.AreEqual(1, LCase.ElseStatements.Count);
    Assert.IsNotNull(LCase.EndKeyword);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseDeepNesting;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource :=
    'procedure Complex; ' + #13#10 +
    'begin' + #13#10 +
    '  case Action of' + #13#10 +
    '    1:' + #13#10 +
    '      begin' + #13#10 +
    '        x := 1;' + #13#10 +
    '        case SubAction of' + #13#10 +
    '          10: DoA;' + #13#10 +
    '          20: DoB;' + #13#10 +
    '        end;' + #13#10 +
    '      end;' + #13#10 +
    '    2: DoC;' + #13#10 +
    '  else' + #13#10 +
    '    raise Exception.Create(''Unknown'');' + #13#10 +
    '  end;' + #13#10 +
    'end;';

  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(2, LCase.CaseItems.Count);
    Assert.IsTrue(LCase.CaseItems[0].Statement is TBeginEndStatementSyntax);
    Assert.IsTrue(LCase.CaseItems[1].Statement is TProcedureCallStatementSyntax);
    Assert.IsNotNull(LCase.ElseKeyword);
    Assert.AreEqual(1, LCase.ElseStatements.Count);
    Assert.IsTrue(LCase.ElseStatements[0] is TRaiseStatementSyntax);

    var LBlock := TBeginEndStatementSyntax(LCase.CaseItems[0].Statement);
    Assert.AreEqual(2, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsTrue(LBlock.Statements[1] is TCaseStatementSyntax);

    var LInner := TCaseStatementSyntax(LBlock.Statements[1]);
    Assert.AreEqual(2, LInner.CaseItems.Count);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult, 'Roundtrip should be exact');
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestCaseItemWithEmptyStatement;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCase: TCaseStatementSyntax;
begin
  LSource := 'procedure Foo; begin case X of 1: ; else HandleElse; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TCaseStatementSyntax);

    LCase := TCaseStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LCase.CaseItems.Count);
    Assert.IsTrue(LCase.CaseItems[0].Statement is TEmptyStatementSyntax);
    Assert.AreEqual(1, LCase.ElseStatements.Count);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithSimple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LWith: TWithStatementSyntax;
begin
  LSource := 'procedure Foo; begin with Obj do Bar; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TWithStatementSyntax);

    LWith := TWithStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LWith.WithKeyword);
    Assert.AreEqual('with', LWith.WithKeyword.Text);
    Assert.AreEqual(1, LWith.ExpressionTokens.Count);
    Assert.AreEqual('Obj', LWith.ExpressionTokens[0].Text);
    Assert.IsNotNull(LWith.DoKeyword);
    Assert.AreEqual('do', LWith.DoKeyword.Text);
    Assert.IsNotNull(LWith.Statement);
    Assert.IsTrue(LWith.Statement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithQualifiedExpression;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LWith: TWithStatementSyntax;
begin
  LSource := 'procedure Foo; begin with Self.FData do Clear; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TWithStatementSyntax);

    LWith := TWithStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LWith.ExpressionTokens.Count);
    Assert.AreEqual('Self', LWith.ExpressionTokens[0].Text);
    Assert.AreEqual('.', LWith.ExpressionTokens[1].Text);
    Assert.AreEqual('FData', LWith.ExpressionTokens[2].Text);
    Assert.IsNotNull(LWith.DoKeyword);
    Assert.IsTrue(LWith.Statement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithMultipleExpressions;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LWith: TWithStatementSyntax;
begin
  LSource := 'procedure Foo; begin with A, B do Bar; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TWithStatementSyntax);

    LWith := TWithStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LWith.ExpressionTokens.Count);
    Assert.AreEqual('A', LWith.ExpressionTokens[0].Text);
    Assert.AreEqual(',', LWith.ExpressionTokens[1].Text);
    Assert.AreEqual('B', LWith.ExpressionTokens[2].Text);
    Assert.IsNotNull(LWith.DoKeyword);
    Assert.IsTrue(LWith.Statement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LWith: TWithStatementSyntax;
begin
  LSource := 'procedure Foo; begin with Obj do begin X := 1; Y := 2; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TWithStatementSyntax);

    LWith := TWithStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LWith.DoKeyword);
    Assert.IsNotNull(LWith.Statement);
    Assert.IsTrue(LWith.Statement is TBeginEndStatementSyntax);
    Assert.AreEqual(2, TBeginEndStatementSyntax(LWith.Statement).Statements.Count);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithNested;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LWith: TWithStatementSyntax;
begin
  LSource := 'procedure Foo; begin with A do with B do Bar; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TWithStatementSyntax);

    LWith := TWithStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LWith.ExpressionTokens.Count);
    Assert.AreEqual('A', LWith.ExpressionTokens[0].Text);
    Assert.IsNotNull(LWith.Statement);
    Assert.IsTrue(LWith.Statement is TWithStatementSyntax);

    var LInner := TWithStatementSyntax(LWith.Statement);
    Assert.AreEqual(1, LInner.ExpressionTokens.Count);
    Assert.AreEqual('B', LInner.ExpressionTokens[0].Text);
    Assert.IsTrue(LInner.Statement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithInBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin X := 1; with Obj do Bar; Y := 2; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(3, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TWithStatementSyntax);
    Assert.IsTrue(LMethod.Statements[2] is TAssignmentStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithInIfThenElse;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure Foo; begin if X then with A do Bar else with B do Baz; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TWithStatementSyntax);
    Assert.IsTrue(LIf.ElseStatement is TWithStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithInTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try with Obj do Bar; finally Cleanup; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LTry.Statements.Count);
    Assert.IsTrue(LTry.Statements[0] is TWithStatementSyntax);
    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithInForLoop;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for I := 0 to 9 do with Items[I] do Process; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LFor.Statement is TWithStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithMethodCall;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LWith: TWithStatementSyntax;
begin
  LSource := 'procedure Foo; begin with GetObject(X) do Process; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TWithStatementSyntax);

    LWith := TWithStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(4, LWith.ExpressionTokens.Count);
    Assert.AreEqual('GetObject', LWith.ExpressionTokens[0].Text);
    Assert.AreEqual('(', LWith.ExpressionTokens[1].Text);
    Assert.AreEqual('X', LWith.ExpressionTokens[2].Text);
    Assert.AreEqual(')', LWith.ExpressionTokens[3].Text);
    Assert.IsNotNull(LWith.DoKeyword);
    Assert.IsTrue(LWith.Statement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithAssignment;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LWith: TWithStatementSyntax;
begin
  LSource := 'procedure Foo; begin with Obj do Name := ''Test''; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TWithStatementSyntax);

    LWith := TWithStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LWith.Statement);
    Assert.IsTrue(LWith.Statement is TAssignmentStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsCaseWithTest.TestWithDeepNesting;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LWith: TWithStatementSyntax;
begin
  LSource := 'procedure Foo; begin with A do begin with B do begin X := 1; with C do Y := 2; end; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TWithStatementSyntax);

    LWith := TWithStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('A', LWith.ExpressionTokens[0].Text);
    Assert.IsTrue(LWith.Statement is TBeginEndStatementSyntax);

    var LBlock := TBeginEndStatementSyntax(LWith.Statement);
    Assert.AreEqual(1, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TWithStatementSyntax);

    var LWith2 := TWithStatementSyntax(LBlock.Statements[0]);
    Assert.AreEqual('B', LWith2.ExpressionTokens[0].Text);
    Assert.IsTrue(LWith2.Statement is TBeginEndStatementSyntax);

    var LBlock2 := TBeginEndStatementSyntax(LWith2.Statement);
    Assert.AreEqual(2, LBlock2.Statements.Count);
    Assert.IsTrue(LBlock2.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsTrue(LBlock2.Statements[1] is TWithStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsCaseWithTest);

end.
