unit Test.ParseTree.Parser.Statements.InlineVar;

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
  TParseTreeParserStatementsInlineVarTest = class
  public
    [Test]
    procedure TestInlineVarSimple;
    [Test]
    procedure TestInlineVarWithType;
    [Test]
    procedure TestInlineVarTypeInferred;
    [Test]
    procedure TestInlineVarWithTypeAndInit;
    [Test]
    procedure TestInlineVarMultiple;
    [Test]
    procedure TestInlineVarInBeginEnd;
    [Test]
    procedure TestInlineVarInForLoop;
    [Test]
    procedure TestInlineVarInIfThen;
    [Test]
    procedure TestInlineVarInTryFinally;
    [Test]
    procedure TestInlineVarStringInit;
    [Test]
    procedure TestInlineVarComplexExpression;
    [Test]
    procedure TestInlineVarBeforeEnd;
    [Test]
    procedure TestInlineVarWithAnonymousFunctionInitializer;
  end;

implementation

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarSimple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInlineVarStatementSyntax;
begin
  LSource := 'procedure Foo; begin var X: Integer; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInlineVarStatementSyntax);

    LStmt := TInlineVarStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LStmt.VarKeyword);
    Assert.AreEqual('var', LStmt.VarKeyword.Text);
    Assert.AreEqual(3, LStmt.DeclarationTokens.Count);
    Assert.AreEqual('X', LStmt.DeclarationTokens[0].Text);
    Assert.AreEqual(':', LStmt.DeclarationTokens[1].Text);
    Assert.AreEqual('Integer', LStmt.DeclarationTokens[2].Text);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarWithType;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInlineVarStatementSyntax;
begin
  LSource := 'procedure Foo; begin var S: string; S := ''Hello''; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(2, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInlineVarStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TAssignmentStatementSyntax);

    LStmt := TInlineVarStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('S', LStmt.DeclarationTokens[0].Text);
    Assert.AreEqual(':', LStmt.DeclarationTokens[1].Text);
    Assert.AreEqual('string', LStmt.DeclarationTokens[2].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarTypeInferred;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInlineVarStatementSyntax;
begin
  LSource := 'procedure Foo; begin var N := 42; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInlineVarStatementSyntax);

    LStmt := TInlineVarStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('N', LStmt.DeclarationTokens[0].Text);
    Assert.AreEqual(':=', LStmt.DeclarationTokens[1].Text);
    Assert.AreEqual('42', LStmt.DeclarationTokens[2].Text);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarWithTypeAndInit;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInlineVarStatementSyntax;
begin
  LSource := 'procedure Foo; begin var X: Integer := 10; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInlineVarStatementSyntax);

    LStmt := TInlineVarStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('X', LStmt.DeclarationTokens[0].Text);
    Assert.AreEqual(':', LStmt.DeclarationTokens[1].Text);
    Assert.AreEqual('Integer', LStmt.DeclarationTokens[2].Text);
    Assert.AreEqual(':=', LStmt.DeclarationTokens[3].Text);
    Assert.AreEqual('10', LStmt.DeclarationTokens[4].Text);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarMultiple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin var A := 1; var B := 2; var C := A + B; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(3, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInlineVarStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TInlineVarStatementSyntax);
    Assert.IsTrue(LMethod.Statements[2] is TInlineVarStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarInBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin X := 1; var Y: Integer; Y := X + 1; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(3, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TInlineVarStatementSyntax);
    Assert.IsTrue(LMethod.Statements[2] is TAssignmentStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarInForLoop;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin for var I := 0 to 9 do Process(I); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    var LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LFor.VariableTokens.Count >= 2);
    Assert.AreEqual('var', LFor.VariableTokens[0].Text);
    Assert.AreEqual('I', LFor.VariableTokens[1].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarInIfThen;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin if True then begin var X := 1; Process(X); end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    var LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TBeginEndStatementSyntax);
    var LBlock := TBeginEndStatementSyntax(LIf.ThenStatement);
    Assert.AreEqual(2, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TInlineVarStatementSyntax);
    Assert.IsTrue(LBlock.Statements[1] is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarInTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try var X := Create; finally X.Free; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LTry.Statements.Count);
    Assert.IsTrue(LTry.Statements[0] is TInlineVarStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarStringInit;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInlineVarStatementSyntax;
begin
  LSource := 'procedure Foo; begin var S := ''Hello World''; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInlineVarStatementSyntax);

    LStmt := TInlineVarStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('S', LStmt.DeclarationTokens[0].Text);
    Assert.AreEqual(':=', LStmt.DeclarationTokens[1].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarComplexExpression;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInlineVarStatementSyntax;
begin
  LSource := 'procedure Foo; begin var List := TList<Integer>.Create; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInlineVarStatementSyntax);

    LStmt := TInlineVarStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('List', LStmt.DeclarationTokens[0].Text);
    Assert.AreEqual(':=', LStmt.DeclarationTokens[1].Text);
    Assert.IsNotNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarBeforeEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LStmt: TInlineVarStatementSyntax;
begin
  LSource := 'procedure Foo; begin var X: Integer end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TInlineVarStatementSyntax);

    LStmt := TInlineVarStatementSyntax(LMethod.Statements[0]);
    Assert.IsNull(LStmt.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsInlineVarTest.TestInlineVarWithAnonymousFunctionInitializer;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin var Task: IFuture<String> := TTask.Future<String>(function: String begin Result := ''X''; end); Process(Task); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.IsTrue(LMethod.Statements.Count >= 1);
    Assert.IsTrue(LMethod.Statements[0] is TInlineVarStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsInlineVarTest);

end.
