unit Test.ParseTree.Parser.Statements.Loops;

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
  TParseTreeParserStatementsLoopsTest = class
  public
    [Test]
    procedure TestWhileStatement;
    [Test]
    procedure TestRepeatStatement;
    [Test]
    procedure TestRepeatMultipleStatements;
    [Test]
    procedure TestRepeatEmpty;
    [Test]
    procedure TestRepeatComplexCondition;
    [Test]
    procedure TestRepeatNested;
    [Test]
    procedure TestRepeatInBeginEnd;
    [Test]
    procedure TestRepeatInIfThenElse;
    [Test]
    procedure TestRepeatInTryFinally;
    [Test]
    procedure TestRepeatInForLoop;
    [Test]
    procedure TestRepeatWithAssignment;
    [Test]
    procedure TestRepeatWithProcCall;
    [Test]
    procedure TestRepeatWithIfStatement;
    [Test]
    procedure TestRepeatDeepNesting;
    [Test]
    procedure TestForStatement;
    [Test]
    procedure TestForInSimple;
    [Test]
    procedure TestForInQualifiedCollection;
    [Test]
    procedure TestForInMethodCallCollection;
    [Test]
    procedure TestForInBeginEnd;
    [Test]
    procedure TestForInNested;
    [Test]
    procedure TestForInInBeginEnd;
    [Test]
    procedure TestForInInIfThenElse;
    [Test]
    procedure TestForInInTryFinally;
    [Test]
    procedure TestForInWithStatement;
    [Test]
    procedure TestForInArrayProperty;
    [Test]
    procedure TestForInString;
    [Test]
    procedure TestForToStillWorks;
  end;

implementation

procedure TParseTreeParserStatementsLoopsTest.TestWhileStatement;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LStmt: TStatementSyntax;
  LWhile: TWhileStatementSyntax;
begin
  // Given
  LParser := TParseTreeParser.Create;
  try
    LMethod := LParser.ParseMethodImplementation('procedure TMyClass.Foo; begin while True do Break; end;');
    Assert.IsNotNull(LMethod);
    Assert.AreEqual(1, LMethod.Statements.Count, 'Should have 1 parsed statement');
    
    // Then
    LStmt := LMethod.Statements[0];
    Assert.IsTrue(LStmt is TWhileStatementSyntax, 'Statement should be TWhileStatementSyntax');
    
    LWhile := TWhileStatementSyntax(LStmt);
    Assert.IsNotNull(LWhile.WhileKeyword);
    Assert.AreEqual(TTokenKind.tkWhileKeyword, LWhile.WhileKeyword.Kind);
    
    Assert.IsNotNull(LWhile.ConditionTokens);
    Assert.AreEqual(1, LWhile.ConditionTokens.Count);
    Assert.AreEqual('True', LWhile.ConditionTokens[0].Text);
    
    Assert.IsNotNull(LWhile.DoKeyword);
    Assert.AreEqual(TTokenKind.tkDoKeyword, LWhile.DoKeyword.Kind);
  finally
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatStatement;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRepeat: TRepeatStatementSyntax;
begin
  LSource := 'procedure Bar; begin repeat Inc(x); until x > 10; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRepeatStatementSyntax);

    LRepeat := TRepeatStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LRepeat.RepeatKeyword);
    Assert.AreEqual('repeat', LRepeat.RepeatKeyword.Text);
    Assert.AreEqual(1, LRepeat.Statements.Count);
    Assert.IsTrue(LRepeat.Statements[0] is TProcedureCallStatementSyntax);
    Assert.IsNotNull(LRepeat.UntilKeyword);
    Assert.AreEqual('until', LRepeat.UntilKeyword.Text);
    Assert.IsTrue(LRepeat.ConditionTokens.Count > 0);
    Assert.IsNotNull(LRepeat.Semicolon);
    Assert.AreEqual(';', LRepeat.Semicolon.Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatMultipleStatements;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRepeat: TRepeatStatementSyntax;
begin
  LSource := 'procedure Foo; begin repeat Inc(X); Dec(Y); DoWork; until Done; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRepeatStatementSyntax);

    LRepeat := TRepeatStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LRepeat.Statements.Count);
    Assert.IsTrue(LRepeat.Statements[0] is TProcedureCallStatementSyntax);
    Assert.IsTrue(LRepeat.Statements[1] is TProcedureCallStatementSyntax);
    Assert.IsTrue(LRepeat.Statements[2] is TProcedureCallStatementSyntax);
    Assert.IsNotNull(LRepeat.UntilKeyword);
    Assert.IsNotNull(LRepeat.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatEmpty;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRepeat: TRepeatStatementSyntax;
begin
  LSource := 'procedure Foo; begin repeat until False; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRepeatStatementSyntax);

    LRepeat := TRepeatStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(0, LRepeat.Statements.Count);
    Assert.IsNotNull(LRepeat.UntilKeyword);
    Assert.AreEqual(1, LRepeat.ConditionTokens.Count);
    Assert.AreEqual('False', LRepeat.ConditionTokens[0].Text);
    Assert.IsNotNull(LRepeat.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatComplexCondition;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRepeat: TRepeatStatementSyntax;
begin
  LSource := 'procedure Foo; begin repeat DoWork; until (X > 10) and (Y < 5); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRepeatStatementSyntax);

    LRepeat := TRepeatStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LRepeat.Statements.Count);
    Assert.IsNotNull(LRepeat.UntilKeyword);
    Assert.IsTrue(LRepeat.ConditionTokens.Count > 5);
    Assert.IsNotNull(LRepeat.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatNested;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRepeat: TRepeatStatementSyntax;
begin
  LSource := 'procedure Foo; begin repeat repeat Inc(X); until X > 5; until Y > 10; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRepeatStatementSyntax);

    LRepeat := TRepeatStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LRepeat.Statements.Count);
    Assert.IsTrue(LRepeat.Statements[0] is TRepeatStatementSyntax);

    var LInner := TRepeatStatementSyntax(LRepeat.Statements[0]);
    Assert.AreEqual(1, LInner.Statements.Count);
    Assert.IsNotNull(LInner.UntilKeyword);
    Assert.IsNotNull(LInner.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatInBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin X := 0; repeat Inc(X); until X > 10; Y := X; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(3, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TRepeatStatementSyntax);
    Assert.IsTrue(LMethod.Statements[2] is TAssignmentStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatInIfThenElse;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure Foo; begin if Cond then repeat DoA; until X else repeat DoB; until Y; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TRepeatStatementSyntax);
    Assert.IsTrue(LIf.ElseStatement is TRepeatStatementSyntax);

    var LThen := TRepeatStatementSyntax(LIf.ThenStatement);
    Assert.IsNotNull(LThen.UntilKeyword);
    Assert.IsNull(LThen.Semicolon);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatInTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try repeat DoWork; until Done; finally Cleanup; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LTry.Statements.Count);
    Assert.IsTrue(LTry.Statements[0] is TRepeatStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatInForLoop;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for I := 0 to 9 do repeat Process(I); until OK; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LFor.Statement is TRepeatStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatWithAssignment;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRepeat: TRepeatStatementSyntax;
begin
  LSource := 'procedure Foo; begin repeat X := X + 1; until X > 100; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRepeatStatementSyntax);

    LRepeat := TRepeatStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LRepeat.Statements.Count);
    Assert.IsTrue(LRepeat.Statements[0] is TAssignmentStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatWithProcCall;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRepeat: TRepeatStatementSyntax;
begin
  LSource := 'procedure Foo; begin repeat Application.ProcessMessages; until Terminated; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRepeatStatementSyntax);

    LRepeat := TRepeatStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LRepeat.Statements.Count);
    Assert.IsTrue(LRepeat.Statements[0] is TProcedureCallStatementSyntax);
    Assert.AreEqual(1, LRepeat.ConditionTokens.Count);
    Assert.AreEqual('Terminated', LRepeat.ConditionTokens[0].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatWithIfStatement;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRepeat: TRepeatStatementSyntax;
begin
  LSource := 'procedure Foo; begin repeat if X then Inc(Y); until Y > 10; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRepeatStatementSyntax);

    LRepeat := TRepeatStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LRepeat.Statements.Count);
    Assert.IsTrue(LRepeat.Statements[0] is TIfStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestRepeatDeepNesting;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LRepeat: TRepeatStatementSyntax;
begin
  LSource := 'procedure Foo; begin repeat repeat repeat Inc(X); until X > 3; until X > 6; until X > 9; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TRepeatStatementSyntax);

    LRepeat := TRepeatStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LRepeat.Statements.Count);
    Assert.IsTrue(LRepeat.Statements[0] is TRepeatStatementSyntax);

    var LMid := TRepeatStatementSyntax(LRepeat.Statements[0]);
    Assert.AreEqual(1, LMid.Statements.Count);
    Assert.IsTrue(LMid.Statements[0] is TRepeatStatementSyntax);

    var LInner := TRepeatStatementSyntax(LMid.Statements[0]);
    Assert.AreEqual(1, LInner.Statements.Count);
    Assert.IsTrue(LInner.Statements[0] is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForStatement;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LStmt: TStatementSyntax;
  LFor: TForStatementSyntax;
begin
  LParser := TParseTreeParser.Create;
  try
    LMethod := LParser.ParseMethodImplementation('procedure Baz; begin for I := 1 to 10 do Break; end;');
    Assert.AreEqual(1, LMethod.Statements.Count);
    LStmt := LMethod.Statements[0];
    Assert.IsTrue(LStmt is TForStatementSyntax);
    LFor := TForStatementSyntax(LStmt);
    
    Assert.IsNotNull(LFor.ForKeyword);
    Assert.AreEqual(TTokenKind.tkForKeyword, LFor.ForKeyword.Kind);
    Assert.IsNotNull(LFor.ToDowntoKeyword);
    Assert.AreEqual(TTokenKind.tkToKeyword, LFor.ToDowntoKeyword.Kind);
  finally
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInSimple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for Item in List do Process(Item); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LFor.ForKeyword);
    Assert.AreEqual('for', LFor.ForKeyword.Text);
    Assert.AreEqual(1, LFor.VariableTokens.Count);
    Assert.AreEqual('Item', LFor.VariableTokens[0].Text);
    Assert.IsNotNull(LFor.InKeyword);
    Assert.AreEqual('in', LFor.InKeyword.Text);
    Assert.AreEqual(1, LFor.CollectionTokens.Count);
    Assert.AreEqual('List', LFor.CollectionTokens[0].Text);
    Assert.IsNull(LFor.AssignmentToken);
    Assert.IsNull(LFor.ToDowntoKeyword);
    Assert.AreEqual(0, LFor.StartTokens.Count);
    Assert.AreEqual(0, LFor.EndTokens.Count);
    Assert.IsNotNull(LFor.DoKeyword);
    Assert.AreEqual('do', LFor.DoKeyword.Text);
    Assert.IsTrue(LFor.Statement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInQualifiedCollection;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for Item in Self.FItems do Process; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LFor.InKeyword);
    Assert.AreEqual(3, LFor.CollectionTokens.Count);
    Assert.AreEqual('Self', LFor.CollectionTokens[0].Text);
    Assert.AreEqual('.', LFor.CollectionTokens[1].Text);
    Assert.AreEqual('FItems', LFor.CollectionTokens[2].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInMethodCallCollection;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for Item in GetList(X) do Process; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LFor.InKeyword);
    Assert.AreEqual(4, LFor.CollectionTokens.Count);
    Assert.AreEqual('GetList', LFor.CollectionTokens[0].Text);
    Assert.AreEqual('(', LFor.CollectionTokens[1].Text);
    Assert.AreEqual('X', LFor.CollectionTokens[2].Text);
    Assert.AreEqual(')', LFor.CollectionTokens[3].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for Item in List do begin Process(Item); Count := Count + 1; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LFor.InKeyword);
    Assert.IsTrue(LFor.Statement is TBeginEndStatementSyntax);
    Assert.AreEqual(2, TBeginEndStatementSyntax(LFor.Statement).Statements.Count);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInNested;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for Outer in List1 do for Inner in List2 do Process; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LFor.InKeyword);
    Assert.AreEqual('List1', LFor.CollectionTokens[0].Text);
    Assert.IsTrue(LFor.Statement is TForStatementSyntax);

    var LInner := TForStatementSyntax(LFor.Statement);
    Assert.IsNotNull(LInner.InKeyword);
    Assert.AreEqual('List2', LInner.CollectionTokens[0].Text);
    Assert.IsTrue(LInner.Statement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInInBeginEnd;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure Foo; begin X := 0; for Item in List do Inc(X); Y := X; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(3, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsTrue(LMethod.Statements[1] is TForStatementSyntax);
    Assert.IsTrue(LMethod.Statements[2] is TAssignmentStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInInIfThenElse;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure Foo; begin if Condition then for Item in A do ProcessA else for Item in B do ProcessB; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TForStatementSyntax);
    Assert.IsNotNull(TForStatementSyntax(LIf.ThenStatement).InKeyword);
    Assert.IsTrue(LIf.ElseStatement is TForStatementSyntax);
    Assert.IsNotNull(TForStatementSyntax(LIf.ElseStatement).InKeyword);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInInTryFinally;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LTry: TTryStatementSyntax;
begin
  LSource := 'procedure Foo; begin try for Item in List do Process; finally Cleanup; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TTryStatementSyntax);

    LTry := TTryStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LTry.Statements.Count);
    Assert.IsTrue(LTry.Statements[0] is TForStatementSyntax);
    Assert.IsNotNull(TForStatementSyntax(LTry.Statements[0]).InKeyword);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInWithStatement;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for Item in List do with Item do Process; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LFor.InKeyword);
    Assert.IsTrue(LFor.Statement is TWithStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInArrayProperty;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for Item in Obj.Items[0] do Process; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LFor.InKeyword);
    Assert.AreEqual(6, LFor.CollectionTokens.Count);
    Assert.AreEqual('Obj', LFor.CollectionTokens[0].Text);
    Assert.AreEqual('.', LFor.CollectionTokens[1].Text);
    Assert.AreEqual('Items', LFor.CollectionTokens[2].Text);
    Assert.AreEqual('[', LFor.CollectionTokens[3].Text);
    Assert.AreEqual('0', LFor.CollectionTokens[4].Text);
    Assert.AreEqual(']', LFor.CollectionTokens[5].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForInString;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for Ch in S do Process(Ch); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LFor.VariableTokens.Count);
    Assert.AreEqual('Ch', LFor.VariableTokens[0].Text);
    Assert.IsNotNull(LFor.InKeyword);
    Assert.AreEqual(1, LFor.CollectionTokens.Count);
    Assert.AreEqual('S', LFor.CollectionTokens[0].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsLoopsTest.TestForToStillWorks;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
begin
  LSource := 'procedure Foo; begin for I := 0 to 9 do Process(I); end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);

    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LFor.AssignmentToken);
    Assert.IsNotNull(LFor.ToDowntoKeyword);
    Assert.AreEqual('to', LFor.ToDowntoKeyword.Text);
    Assert.IsNull(LFor.InKeyword);
    Assert.AreEqual(0, LFor.CollectionTokens.Count);
    Assert.IsNotNull(LFor.DoKeyword);
    Assert.IsTrue(LFor.Statement is TProcedureCallStatementSyntax);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsLoopsTest);

end.
