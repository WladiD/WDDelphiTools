unit Test.ParseTree.Parser.Statements.ProcCall.Basic;

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
  TParseTreeParserStatementsProcCallBasicTest = class
  public
    [Test]
    procedure TestProcCallSimple;
    [Test]
    procedure TestProcCallWithArgs;
    [Test]
    procedure TestProcCallQualified;
    [Test]
    procedure TestProcCallQualifiedWithArgs;
    [Test]
    procedure TestProcCallNestedParens;
    [Test]
    procedure TestProcCallInc;
    [Test]
    procedure TestProcCallClassMethod;
    [Test]
    procedure TestProcCallMultipleConsecutive;
    [Test]
    procedure TestProcCallInIfThen;
    [Test]
    procedure TestProcCallInBeginEnd;
    [Test]
    procedure TestProcCallInTryFinally;
    [Test]
    procedure TestProcCallInForLoop;
    [Test]
    procedure TestProcCallWithBrackets;
  end;

implementation

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallSimple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin DoSomething; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LCall.ExpressionTokens.Count);
    Assert.AreEqual('DoSomething', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallWithArgs;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin Writeln(''Hello''); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Writeln', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallQualified;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin FList.Clear; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('FList', LCall.ExpressionTokens[0].Text);
    Assert.AreEqual('.', LCall.ExpressionTokens[1].Text);
    Assert.AreEqual('Clear', LCall.ExpressionTokens[2].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallQualifiedWithArgs;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin FList.Add(Item); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('FList', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallNestedParens;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin Writeln(Format(''%s (%d)'', [S, I])); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Writeln', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallInc;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin Inc(Result); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Inc', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallClassMethod;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin TDirectory.CreateDirectory(LPath); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('TDirectory', LCall.ExpressionTokens[0].Text);
    Assert.AreEqual('.', LCall.ExpressionTokens[1].Text);
    Assert.AreEqual('CreateDirectory', LCall.ExpressionTokens[2].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallMultipleConsecutive;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin Client.Connect; DoWork; Client.Disconnect; Client.Free; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(4, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TProcedureCallStatementSyntax);
    Assert.IsTrue(LMethod.Statements[2] is TProcedureCallStatementSyntax);
    Assert.IsTrue(LMethod.Statements[3] is TProcedureCallStatementSyntax);

    Assert.AreEqual('Client', TProcedureCallStatementSyntax(LMethod.Statements[0]).ExpressionTokens[0].Text);
    Assert.AreEqual('DoWork', TProcedureCallStatementSyntax(LMethod.Statements[1]).ExpressionTokens[0].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallInIfThen;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure Foo; begin if x then DoSomething; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TProcedureCallStatementSyntax);
    Assert.AreEqual('DoSomething', TProcedureCallStatementSyntax(LIf.ThenStatement).ExpressionTokens[0].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallInBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin begin Init; Process(Data); Cleanup; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TBeginEndStatementSyntax);

    LBlock := TBeginEndStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TProcedureCallStatementSyntax);
    Assert.IsTrue(LBlock.Statements[1] is TProcedureCallStatementSyntax);
    Assert.IsTrue(LBlock.Statements[2] is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallInTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try DoWork; finally Cleanup; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LTry.Statements.Count);
    Assert.IsTrue(LTry.Statements[0] is TProcedureCallStatementSyntax);
    Assert.AreEqual('DoWork', TProcedureCallStatementSyntax(LTry.Statements[0]).ExpressionTokens[0].Text);

    Assert.AreEqual(1, LTry.FinallyExceptStatements.Count);
    Assert.IsTrue(LTry.FinallyExceptStatements[0] is TProcedureCallStatementSyntax);
    Assert.AreEqual('Cleanup', TProcedureCallStatementSyntax(LTry.FinallyExceptStatements[0]).ExpressionTokens[0].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallInForLoop;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for I := 0 to 10 do Process(I); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LFor.Statement is TProcedureCallStatementSyntax);
    Assert.AreEqual('Process', TProcedureCallStatementSyntax(LFor.Statement).ExpressionTokens[0].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsProcCallBasicTest.TestProcCallWithBrackets;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LCall: TProcedureCallStatementSyntax;
begin
  LSource := 'procedure Foo; begin Items[0].Execute; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TProcedureCallStatementSyntax);

    LCall := TProcedureCallStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('Items', LCall.ExpressionTokens[0].Text);
    Assert.IsNotNull(LCall.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsProcCallBasicTest);

end.
