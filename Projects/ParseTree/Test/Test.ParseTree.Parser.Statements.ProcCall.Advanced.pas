unit Test.ParseTree.Parser.Statements.ProcCall.Advanced;

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
  TParseTreeParserStatementsProcCallAdvancedTest = class
  public
    [Test]
    procedure TestWriteAsProcCall;
    [Test]
    procedure TestReadAsProcCall;
    [Test]
    procedure TestParenCastCall;
    [Test]
    procedure TestParenCastCallWithArgs;
    [Test]
    procedure TestParenCastCallChained;
    [Test]
    procedure TestProcCallWithAnonymousProcedure;
    [Test]
    procedure TestProcCallWithAnonymousProcedureParameter;
    [Test]
    procedure TestProcCallWithAnonymousFunction;
    [Test]
    procedure TestProcCallWithAnonymousProcedureNestedBeginEnd;
    [Test]
    procedure TestProcCallWithAnonymousProcedureTryFinally;
    [Test]
    procedure TestProcCallWithAnonymousMethodAndExtraArguments;
    [Test]
    procedure TestTryFinallyWithAnonymousProcedureCall;
    [Test]
    procedure TestTryExceptWithAnonymousProcedureCall;
    [Test]
    procedure TestProcCallWithNestedAnonymousProcedures;
    [Test]
    procedure TestProcCallWithAnonymousFunctionTryFinally;
    [Test]
    procedure TestProcCallWithAnonymousProcedureAndGenerics;
    [Test]
    procedure TestProcCallWithMultipleAnonymousArguments;
    [Test]
    procedure TestTryFinallyAfterComplexAnonymousCall;
  end;

implementation

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestWriteAsProcCall;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin Write(X); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Write', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestReadAsProcCall;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin Read(X); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Read', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestParenCastCall;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin (Sender as TButton).Click; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('(', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestParenCastCallWithArgs;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin (Obj as TFoo).DoSomething(X, Y); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('(', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestParenCastCallChained;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin (Obj as TFoo).Bar; (Obj as TFoo).Baz; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(2, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithAnonymousProcedure;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin TTask.Run(procedure begin DoWork; end); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('TTask', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithAnonymousProcedureParameter;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
  LHasProcedureKeyword: Boolean;
begin
  LSource := 'procedure Foo; begin TParallel.For(0, 10, procedure(I: Integer) begin Consume(I); end); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCall.Semicolon);
    LHasProcedureKeyword := False;
    for var LToken in LCall.ExpressionTokens do
      if LToken.Kind = tkProcedureKeyword then
      begin
        LHasProcedureKeyword := True;
        Break;
      end;
    Assert.IsTrue(LHasProcedureKeyword, 'Expected anonymous procedure keyword in call tokens');

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithAnonymousFunction;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
  LHasFunctionKeyword: Boolean;
begin
  LSource := 'procedure Foo; begin RegisterFactory(function: Integer begin Result := 42; end); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCall.Semicolon);
    LHasFunctionKeyword := False;
    for var LToken in LCall.ExpressionTokens do
      if LToken.Kind = tkFunctionKeyword then
      begin
        LHasFunctionKeyword := True;
        Break;
      end;
    Assert.IsTrue(LHasFunctionKeyword, 'Expected anonymous function keyword in call tokens');

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithAnonymousProcedureNestedBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin Queue(procedure begin begin X := 1; end; Y := 2; end); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithAnonymousProcedureTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin Execute(procedure begin Lock.Enter; try Work; finally Lock.Leave; end; end); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithAnonymousMethodAndExtraArguments;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin TParallel.For(0, Length(A), procedure(I: Integer) begin Use(A[I]); end, TThreadPool.Default); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestTryFinallyWithAnonymousProcedureCall;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource :=
    'procedure Foo; begin ' +
    'Obj := TObject.Create; ' +
    'try ' +
    'TParallel.For(0, 1, procedure(I: Integer) begin Process(I); end, nil); ' +
    'if Failed then RaiseLastOSError; ' +
    'finally ' +
    'Obj.Free; ' +
    'end; ' +
    'end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(2, LMethod.Statements.Count, 'Assignment + try statement expected');
    Assert.IsTrue(LMethod.Statements[1] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[1]);
    Assert.IsNotNull(LTry.FinallyKeyword);
    Assert.AreEqual(2, LTry.Statements.Count, 'Call + if in try body');
    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count, 'One statement in finally body');
    Assert.IsTrue(LTry.FinallyExceptStatements[0] is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestTryExceptWithAnonymousProcedureCall;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource :=
    'procedure Foo; begin ' +
    'try ' +
    'RunWithGuard(procedure begin Work; end); ' +
    'except ' +
    'on E: Exception do begin Log(E.Message); end; ' +
    'end; ' +
    'end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LTry.ExceptKeyword);
    Assert.AreEqual(1, LTry.Statements.Count, 'One call in try body');
    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count, 'One handler statement expected');

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithNestedAnonymousProcedures;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin Execute(procedure begin Schedule(procedure begin Work; end); end); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithAnonymousFunctionTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin RegisterFactory(function(const X: Integer): Integer begin try Result := X * 2; finally Log(X); end; end); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithAnonymousProcedureAndGenerics;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin TThread.Queue(nil, procedure begin Handle<TPair<Integer, string>>(42); end); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestProcCallWithMultipleAnonymousArguments;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin WhenAll(procedure begin A; end, procedure begin B; end, 5000); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    var LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallAdvancedTest.TestTryFinallyAfterComplexAnonymousCall;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource :=
    'procedure Foo; begin ' +
    'try ' +
    'TParallel.For(0, Count - 1, procedure(I: Integer) begin try Process(I); except Log(I); end; end, TThreadPool.Default); ' +
    'FinalizeBatch; ' +
    'finally ' +
    'Cleanup; ' +
    'end; ' +
    'end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LTry.FinallyKeyword);
    Assert.AreEqual(2, LTry.Statements.Count, 'Expected call + finalize in try body');
    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count, 'Expected one cleanup statement');
    Assert.IsTrue(LTry.FinallyExceptStatements[0] is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsProcCallAdvancedTest);

end.
