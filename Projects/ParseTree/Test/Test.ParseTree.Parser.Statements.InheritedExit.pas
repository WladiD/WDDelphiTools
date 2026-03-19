unit Test.ParseTree.Parser.Statements.InheritedExit;

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
  TParseTreeParserStatementsInheritedExitTest = class
  public
    [Test]
    procedure TestProcCallInherited;
    [Test]
    procedure TestProcCallInheritedWithArgs;
    [Test]
    procedure TestProcCallExit;
    [Test]
    procedure TestProcCallExitWithValue;
    [Test]
    procedure TestInheritedBare;
    [Test]
    procedure TestInheritedWithMethod;
    [Test]
    procedure TestInheritedWithMethodAndArgs;
    [Test]
    procedure TestInheritedWithMultipleArgs;
    [Test]
    procedure TestInheritedInConstructor;
    [Test]
    procedure TestInheritedInDestructor;
    [Test]
    procedure TestInheritedInIfThenElse;
    [Test]
    procedure TestInheritedInTryFinally;
    [Test]
    procedure TestInheritedBeforeEnd;
    [Test]
    procedure TestExitBare;
    [Test]
    procedure TestExitWithValue;
    [Test]
    procedure TestExitWithExpression;
    [Test]
    procedure TestExitInIfThen;
    [Test]
    procedure TestExitInTryExcept;
    [Test]
    procedure TestExitBeforeEnd;
    [Test]
    procedure TestExitWithNestedParens;
    [Test]
    procedure TestInheritedAndExitMixed;
  end;

implementation

procedure TParseTreeParserStatementsInheritedExitTest.TestProcCallInherited;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInheritedStatementSyntax;
begin
  LSource := 'procedure TMyClass.Destroy; begin inherited; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInheritedStatementSyntax);

    LStmt := TInheritedStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('inherited', LStmt.InheritedKeyword.Text);
    Assert.AreEqual(0, LStmt.CallTokens.Count);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestProcCallInheritedWithArgs;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInheritedStatementSyntax;
begin
  LSource := 'constructor TMyClass.Create(AOwner: TComponent); begin inherited Create(AOwner); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInheritedStatementSyntax);

    LStmt := TInheritedStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('inherited', LStmt.InheritedKeyword.Text);
    Assert.AreEqual('Create', LStmt.CallTokens[0].Text);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestProcCallExit;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TExitStatementSyntax;
begin
  LSource := 'procedure Foo; begin Exit; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TExitStatementSyntax);

    LStmt := TExitStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Exit', LStmt.ExitKeyword.Text);
    Assert.AreEqual(0, LStmt.ExpressionTokens.Count);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestProcCallExitWithValue;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TExitStatementSyntax;
begin
  LSource := 'function Foo: Integer; begin Exit(42); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TExitStatementSyntax);

    LStmt := TExitStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Exit', LStmt.ExitKeyword.Text);
    Assert.IsTrue(LStmt.ExpressionTokens.Count > 0);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedBare;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInheritedStatementSyntax;
begin
  LSource := 'procedure TFoo.Bar; begin inherited; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInheritedStatementSyntax);

    LStmt := TInheritedStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('inherited', LStmt.InheritedKeyword.Text);
    Assert.AreEqual(0, LStmt.CallTokens.Count);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedWithMethod;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInheritedStatementSyntax;
begin
  LSource := 'procedure TFoo.Bar; begin inherited DoSomething; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInheritedStatementSyntax);

    LStmt := TInheritedStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LStmt.CallTokens.Count);
    Assert.AreEqual('DoSomething', LStmt.CallTokens[0].Text);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedWithMethodAndArgs;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInheritedStatementSyntax;
begin
  LSource := 'constructor TFoo.Create(AOwner: TComponent); begin inherited Create(AOwner); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInheritedStatementSyntax);

    LStmt := TInheritedStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Create', LStmt.CallTokens[0].Text);
    Assert.AreEqual('(', LStmt.CallTokens[1].Text);
    Assert.AreEqual('AOwner', LStmt.CallTokens[2].Text);
    Assert.AreEqual(')', LStmt.CallTokens[3].Text);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedWithMultipleArgs;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInheritedStatementSyntax;
begin
  LSource := 'constructor TFoo.Create(A: Integer; B: string); begin inherited Create(A, B); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInheritedStatementSyntax);

    LStmt := TInheritedStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Create', LStmt.CallTokens[0].Text);
    Assert.IsTrue(LStmt.CallTokens.Count >= 5);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedInConstructor;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'constructor TFoo.Create; begin inherited Create; FData := TList.Create; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(2, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInheritedStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TAssignmentStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedInDestructor;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'destructor TFoo.Destroy; begin FData.Free; inherited; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(2, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TInheritedStatementSyntax);

    var LInherited := TInheritedStatementSyntax(LMethod.Statements[1]);
    Assert.AreEqual(0, LInherited.CallTokens.Count);
    Assert.IsNotNull(LInherited.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedInIfThenElse;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure TFoo.Bar; begin if Condition then inherited DoA else inherited DoB; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TInheritedStatementSyntax);
    Assert.IsTrue(LIf.ElseStatement is TInheritedStatementSyntax);

    var LThen := TInheritedStatementSyntax(LIf.ThenStatement);
    Assert.AreEqual(1, LThen.CallTokens.Count);
    Assert.AreEqual('DoA', LThen.CallTokens[0].Text);
    Assert.IsNull(LThen.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedInTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'destructor TFoo.Destroy; begin try FData.Free; finally inherited; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count);
    Assert.IsTrue(LTry.FinallyExceptStatements[0] is TInheritedStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedBeforeEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'destructor TFoo.Destroy; begin FData.Free; inherited end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(2, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[1] is TInheritedStatementSyntax);

    var LInherited := TInheritedStatementSyntax(LMethod.Statements[1]);
    Assert.AreEqual(0, LInherited.CallTokens.Count);
    Assert.IsNull(LInherited.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestExitBare;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TExitStatementSyntax;
begin
  LSource := 'procedure Foo; begin Exit; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TExitStatementSyntax);

    LStmt := TExitStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Exit', LStmt.ExitKeyword.Text);
    Assert.AreEqual(0, LStmt.ExpressionTokens.Count);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestExitWithValue;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TExitStatementSyntax;
begin
  LSource := 'function Foo: Integer; begin Exit(42); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TExitStatementSyntax);

    LStmt := TExitStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('(', LStmt.ExpressionTokens[0].Text);
    Assert.AreEqual('42', LStmt.ExpressionTokens[1].Text);
    Assert.AreEqual(')', LStmt.ExpressionTokens[2].Text);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestExitWithExpression;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TExitStatementSyntax;
begin
  LSource := 'function Foo(X: Integer): Boolean; begin Exit(X > 0); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TExitStatementSyntax);

    LStmt := TExitStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LStmt.ExpressionTokens.Count >= 4);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestExitInIfThen;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'function Foo: Integer; begin if X then Exit(1) else Exit(2); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TExitStatementSyntax);
    Assert.IsTrue(LIf.ElseStatement is TExitStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestExitInTryExcept;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'function Foo: Integer; begin try Exit(Compute); except Exit(0); end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LTry.Statements.Count);
    Assert.IsTrue(LTry.Statements[0] is TExitStatementSyntax);
    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count);
    Assert.IsTrue(LTry.FinallyExceptStatements[0] is TExitStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestExitBeforeEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TExitStatementSyntax;
begin
  LSource := 'procedure Foo; begin Exit end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TExitStatementSyntax);

    LStmt := TExitStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(0, LStmt.ExpressionTokens.Count);
    Assert.IsNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestExitWithNestedParens;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TExitStatementSyntax;
begin
  LSource := 'function Foo: Integer; begin Exit(Abs(X - Y)); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TExitStatementSyntax);

    LStmt := TExitStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LStmt.ExpressionTokens.Count >= 7);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInheritedExitTest.TestInheritedAndExitMixed;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'constructor TFoo.Create(AOwner: TComponent); begin inherited Create(AOwner); if AOwner = nil then Exit; FOwner := AOwner; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(3, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInheritedStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TIfStatementSyntax);
    Assert.IsTrue(LMethod.Statements[2] is TAssignmentStatementSyntax);

    var LIf := TIfStatementSyntax(LMethod.Statements[1]);
    Assert.IsTrue(LIf.ThenStatement is TExitStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsInheritedExitTest);

end.
