unit Test.ParseTree.Parser.Statements;

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
  TParseTreeParserStatementsTest = class
  private
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
    procedure TestIfStatement;
    [Test]
    procedure TestElseIfChain;
    [Test]
    procedure TestNestedIf;
    [Test]
    procedure TestComplexIfStatement;
    [Test]
    procedure TestIfStatementWithForbiddenSemicolon;
    [Test]
    procedure TestAssignmentStatement;
    [Test]
    procedure TestBeginEndSimple;
    [Test]
    procedure TestBeginEndWithMultipleStatements;
    [Test]
    procedure TestBeginEndNested;
    [Test]
    procedure TestBeginEndInIfThen;
    [Test]
    procedure TestBeginEndInIfThenElse;
    [Test]
    procedure TestBeginEndInWhileDo;
    [Test]
    procedure TestBeginEndInForDo;
    [Test]
    procedure TestBeginEndEmpty;
    [Test]
    procedure TestBeginEndDeepNesting;
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
    procedure TestProcCallInherited;
    [Test]
    procedure TestProcCallInheritedWithArgs;
    [Test]
    procedure TestProcCallExit;
    [Test]
    procedure TestProcCallExitWithValue;
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
    procedure TestWriteAsProcCall;
    [Test]
    procedure TestReadAsProcCall;
  end;

implementation

{ TParseTreeParserStatementsTest }

procedure TParseTreeParserStatementsTest.TestWhileStatement;
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

procedure TParseTreeParserStatementsTest.TestRepeatStatement;
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

procedure TParseTreeParserStatementsTest.TestRepeatMultipleStatements;
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

procedure TParseTreeParserStatementsTest.TestRepeatEmpty;
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

procedure TParseTreeParserStatementsTest.TestRepeatComplexCondition;
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

procedure TParseTreeParserStatementsTest.TestRepeatNested;
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

procedure TParseTreeParserStatementsTest.TestRepeatInBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestRepeatInIfThenElse;
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

procedure TParseTreeParserStatementsTest.TestRepeatInTryFinally;
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

procedure TParseTreeParserStatementsTest.TestRepeatInForLoop;
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

procedure TParseTreeParserStatementsTest.TestRepeatWithAssignment;
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

procedure TParseTreeParserStatementsTest.TestRepeatWithProcCall;
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

procedure TParseTreeParserStatementsTest.TestRepeatWithIfStatement;
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

procedure TParseTreeParserStatementsTest.TestRepeatDeepNesting;
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

procedure TParseTreeParserStatementsTest.TestForStatement;
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

procedure TParseTreeParserStatementsTest.TestIfStatement;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LStmt: TStatementSyntax;
  LIf: TIfStatementSyntax;
begin
  LParser := TParseTreeParser.Create;
  try
    LMethod := LParser.ParseMethodImplementation('procedure IfTest; begin if True then Break else Continue; end;');
    Assert.AreEqual(1, LMethod.Statements.Count);
    LStmt := LMethod.Statements[0];
    Assert.IsTrue(LStmt is TIfStatementSyntax);
    LIf := TIfStatementSyntax(LStmt);
    
    Assert.IsNotNull(LIf.IfKeyword, 'IfKeyword should not be null');
    Assert.AreEqual(TTokenKind.tkIfKeyword, LIf.IfKeyword.Kind);
    Assert.IsNotNull(LIf.ThenKeyword, 'ThenKeyword should not be null');
    Assert.AreEqual(TTokenKind.tkThenKeyword, LIf.ThenKeyword.Kind);
    Assert.IsNotNull(LIf.ElseKeyword, 'ElseKeyword should not be null');
    Assert.AreEqual(TTokenKind.tkElseKeyword, LIf.ElseKeyword.Kind);
    
    Assert.IsNotNull(LIf.ThenStatement, 'ThenStatement should not be null');
    Assert.IsNotNull(LIf.ElseStatement, 'ElseStatement should not be null');
  finally
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestElseIfChain;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LStmt: TStatementSyntax;
  LIf1, LIf2: TIfStatementSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 'procedure ElseIfTest; begin if a then b else if c then d else e; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    
    // Check 구조 (Struktur)
    LStmt := LMethod.Statements[0];
    Assert.IsTrue(LStmt is TIfStatementSyntax);
    LIf1 := TIfStatementSyntax(LStmt);
    Assert.AreEqual('a', LIf1.ConditionTokens[0].Text);
    
    Assert.IsNotNull(LIf1.ElseStatement);
    Assert.IsTrue(LIf1.ElseStatement is TIfStatementSyntax, 'Else branch should be another If');
    
    LIf2 := TIfStatementSyntax(LIf1.ElseStatement);
    Assert.AreEqual('c', LIf2.ConditionTokens[0].Text);
    Assert.IsNotNull(LIf2.ElseStatement);
    Assert.IsTrue(LIf2.ElseStatement is TProcedureCallStatementSyntax);
    
    // Roundtrip verification
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestNestedIf;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure Nested; begin if x then begin if y then z; end else if a then b; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    
    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LIf.ThenStatement);
    Assert.IsTrue(LIf.ThenStatement is TBeginEndStatementSyntax);
    
    Assert.IsNotNull(LIf.ElseStatement);
    Assert.IsTrue(LIf.ElseStatement is TIfStatementSyntax);
    
    // Roundtrip verification
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestComplexIfStatement;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource := 
    'procedure Complex; ' + #13#10 +
    'begin' + #13#10 +
    '  if a then' + #13#10 +
    '    if b then c' + #13#10 +
    '    else d' + #13#10 +
    '  else if e then' + #13#10 +
    '    begin' + #13#10 +
    '      if f then g;' + #13#10 +
    '    end' + #13#10 +
    '  else' + #13#10 +
    '    h;' + #13#10 +
    'end;';
    
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.IsNotNull(LMethod);
    
    // Validierung der Struktur:
    // LMethod.Statements[0] ist das große IF
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);
    
    var LIf1 := TIfStatementSyntax(LMethod.Statements[0]);
    // Then branch of LIf1 is the nested if b then c else d
    Assert.IsTrue(LIf1.ThenStatement is TIfStatementSyntax);
    
    // Else branch of LIf1 is the "else if e"
    Assert.IsTrue(LIf1.ElseStatement is TIfStatementSyntax);
    var LIfE := TIfStatementSyntax(LIf1.ElseStatement);
    Assert.IsTrue(LIfE.ThenStatement is TBeginEndStatementSyntax);
    
    // Else branch of LIfE is "else h;"
    Assert.IsTrue(LIfE.ElseStatement is TProcedureCallStatementSyntax);
    
    // Roundtrip verification:
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult, 'Roundtrip should be exact');
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestIfStatementWithForbiddenSemicolon;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  // Standard syntax error in Delphi: semicolon before else
  LSource := 'procedure SemicolonError; begin if a then b; else c; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.IsNotNull(LMethod);
    Assert.AreEqual(1, LMethod.Statements.Count);
    
    var LIf := TIfStatementSyntax(LMethod.Statements[0]);
    // Our parser (at this point) should be lenient and find the else.
    // The then statement (Opaque) will contain 'b;'
    Assert.IsTrue(LIf.ThenStatement is TProcedureCallStatementSyntax);
    Assert.AreEqual('b', TProcedureCallStatementSyntax(LIf.ThenStatement).ExpressionTokens[0].Text);
    Assert.IsNotNull(TProcedureCallStatementSyntax(LIf.ThenStatement).Semicolon);
    
    Assert.IsNotNull(LIf.ElseStatement);
    
    // Roundtrip verification: should reproduce the "syntax error" exactly
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestAssignmentStatement;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LAssign: TAssignmentStatementSyntax;
begin
  LSource := 'procedure Assign; begin x := 10; y := a + b; MyObj.Field := ''test''; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(3, LMethod.Statements.Count);
    
    Assert.IsTrue(LMethod.Statements[0] is TAssignmentStatementSyntax);
    LAssign := TAssignmentStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual('x', LAssign.LeftTokens[0].Text);
    Assert.AreEqual(':=', LAssign.ColonEqualsToken.Text);
    
    Assert.IsTrue(LMethod.Statements[1] is TAssignmentStatementSyntax);
    Assert.IsTrue(LMethod.Statements[2] is TAssignmentStatementSyntax);
    
    // Roundtrip
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestBeginEndSimple;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin begin x := 1; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TBeginEndStatementSyntax);
    
    LBlock := TBeginEndStatementSyntax(LMethod.Statements[0]);
    Assert.IsNotNull(LBlock.BeginKeyword);
    Assert.AreEqual('begin', LBlock.BeginKeyword.Text);
    Assert.IsNotNull(LBlock.EndKeyword);
    Assert.AreEqual('end', LBlock.EndKeyword.Text);
    Assert.IsNotNull(LBlock.Semicolon, 'Semicolon after end should be consumed');
    Assert.AreEqual(1, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TAssignmentStatementSyntax);
    
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestBeginEndWithMultipleStatements;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin begin x := 1; y := 2; z := 3; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TBeginEndStatementSyntax);
    
    LBlock := TBeginEndStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LBlock.Statements.Count, 'Should have 3 inner statements');
    Assert.IsTrue(LBlock.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsTrue(LBlock.Statements[1] is TAssignmentStatementSyntax);
    Assert.IsTrue(LBlock.Statements[2] is TAssignmentStatementSyntax);
    
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestBeginEndNested;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LOuter, LInner: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin begin begin x := 1; end; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TBeginEndStatementSyntax);
    
    LOuter := TBeginEndStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(1, LOuter.Statements.Count);
    Assert.IsTrue(LOuter.Statements[0] is TBeginEndStatementSyntax);
    
    LInner := TBeginEndStatementSyntax(LOuter.Statements[0]);
    Assert.AreEqual(1, LInner.Statements.Count);
    Assert.IsTrue(LInner.Statements[0] is TAssignmentStatementSyntax);
    
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestBeginEndInIfThen;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
  LBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin if True then begin x := 1; y := 2; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);
    
    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LIf.ThenStatement is TBeginEndStatementSyntax);
    
    LBlock := TBeginEndStatementSyntax(LIf.ThenStatement);
    Assert.AreEqual(2, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsTrue(LBlock.Statements[1] is TAssignmentStatementSyntax);
    Assert.IsNotNull(LBlock.Semicolon, 'Trailing semicolon should be consumed');
    
    Assert.IsNull(LIf.ElseKeyword, 'No else branch');
    
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestBeginEndInIfThenElse;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
  LThenBlock, LElseBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin if x then begin a := 1; end else begin b := 2; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);
    
    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    
    Assert.IsTrue(LIf.ThenStatement is TBeginEndStatementSyntax);
    LThenBlock := TBeginEndStatementSyntax(LIf.ThenStatement);
    Assert.AreEqual(1, LThenBlock.Statements.Count);
    Assert.IsNull(LThenBlock.Semicolon, 'No semicolon before else');
    
    Assert.IsNotNull(LIf.ElseKeyword);
    Assert.IsTrue(LIf.ElseStatement is TBeginEndStatementSyntax);
    LElseBlock := TBeginEndStatementSyntax(LIf.ElseStatement);
    Assert.AreEqual(1, LElseBlock.Statements.Count);
    Assert.IsNotNull(LElseBlock.Semicolon, 'Semicolon after else-end');
    
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestBeginEndInWhileDo;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LWhile: TWhileStatementSyntax;
  LBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin while x do begin y := y + 1; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TWhileStatementSyntax);
    
    LWhile := TWhileStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LWhile.Statement is TBeginEndStatementSyntax);
    
    LBlock := TBeginEndStatementSyntax(LWhile.Statement);
    Assert.AreEqual(1, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TAssignmentStatementSyntax);
    
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestBeginEndInForDo;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LFor: TForStatementSyntax;
  LBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin for I := 0 to 10 do begin x := I; end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TForStatementSyntax);
    
    LFor := TForStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LFor.Statement is TBeginEndStatementSyntax);
    
    LBlock := TBeginEndStatementSyntax(LFor.Statement);
    Assert.AreEqual(1, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TAssignmentStatementSyntax);
    
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestBeginEndEmpty;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LBlock: TBeginEndStatementSyntax;
begin
  LSource := 'procedure Foo; begin begin end; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TBeginEndStatementSyntax);
    
    LBlock := TBeginEndStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(0, LBlock.Statements.Count, 'Empty begin-end should have 0 statements');
    Assert.IsNotNull(LBlock.BeginKeyword);
    Assert.IsNotNull(LBlock.EndKeyword);
    
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestBeginEndDeepNesting;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
  LBlock: TBeginEndStatementSyntax;
  LInnerIf: TIfStatementSyntax;
  LInnerBlock: TBeginEndStatementSyntax;
begin
  LSource :=
    'procedure Complex; ' + #13#10 +
    'begin' + #13#10 +
    '  if a then' + #13#10 +
    '  begin' + #13#10 +
    '    x := 1;' + #13#10 +
    '    if b then' + #13#10 +
    '    begin' + #13#10 +
    '      y := 2;' + #13#10 +
    '    end;' + #13#10 +
    '    z := 3;' + #13#10 +
    '  end' + #13#10 +
    '  else' + #13#10 +
    '  begin' + #13#10 +
    '    w := 4;' + #13#10 +
    '  end;' + #13#10 +
    'end;';
    
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);
    
    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    
    // Then: begin x := 1; if b then begin y := 2; end; z := 3; end
    Assert.IsTrue(LIf.ThenStatement is TBeginEndStatementSyntax);
    LBlock := TBeginEndStatementSyntax(LIf.ThenStatement);
    Assert.AreEqual(3, LBlock.Statements.Count, 'Then-block should have 3 statements');
    Assert.IsTrue(LBlock.Statements[0] is TAssignmentStatementSyntax, 'x := 1');
    Assert.IsTrue(LBlock.Statements[1] is TIfStatementSyntax, 'if b then ...');
    Assert.IsTrue(LBlock.Statements[2] is TAssignmentStatementSyntax, 'z := 3');
    Assert.IsNull(LBlock.Semicolon, 'No semicolon before else');
    
    // Inner if: if b then begin y := 2; end;
    LInnerIf := TIfStatementSyntax(LBlock.Statements[1]);
    Assert.IsTrue(LInnerIf.ThenStatement is TBeginEndStatementSyntax);
    LInnerBlock := TBeginEndStatementSyntax(LInnerIf.ThenStatement);
    Assert.AreEqual(1, LInnerBlock.Statements.Count);
    Assert.IsTrue(LInnerBlock.Statements[0] is TAssignmentStatementSyntax);
    
    // Else: begin w := 4; end;
    Assert.IsNotNull(LIf.ElseKeyword);
    Assert.IsTrue(LIf.ElseStatement is TBeginEndStatementSyntax);
    LBlock := TBeginEndStatementSyntax(LIf.ElseStatement);
    Assert.AreEqual(1, LBlock.Statements.Count);
    Assert.IsTrue(LBlock.Statements[0] is TAssignmentStatementSyntax);
    Assert.IsNotNull(LBlock.Semicolon, 'Semicolon after else-end');
    
    // Roundtrip
    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult, 'Roundtrip should be exact');
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsTest.TestTryFinallySimple;
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

procedure TParseTreeParserStatementsTest.TestTryFinallyMultipleStatements;
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

procedure TParseTreeParserStatementsTest.TestTryExceptSimple;
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

procedure TParseTreeParserStatementsTest.TestTryExceptWithOnHandler;
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

procedure TParseTreeParserStatementsTest.TestTryFinallyNested;
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

procedure TParseTreeParserStatementsTest.TestTryFinallyInBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestTryFinallyInIfThenElse;
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

procedure TParseTreeParserStatementsTest.TestTryExceptNestedInTryFinally;
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

procedure TParseTreeParserStatementsTest.TestTryFinallyWithBeginEndBodies;
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

procedure TParseTreeParserStatementsTest.TestTryFinallyEmpty;
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

procedure TParseTreeParserStatementsTest.TestTryFinallyDeepNesting;
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

procedure TParseTreeParserStatementsTest.TestRaiseSimple;
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

procedure TParseTreeParserStatementsTest.TestRaiseBare;
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

procedure TParseTreeParserStatementsTest.TestRaiseWithMethodCall;
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

procedure TParseTreeParserStatementsTest.TestRaiseWithNestedParens;
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

procedure TParseTreeParserStatementsTest.TestRaiseInExceptBlock;
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

procedure TParseTreeParserStatementsTest.TestRaiseInIfThenElse;
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

procedure TParseTreeParserStatementsTest.TestRaiseInBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestProcCallSimple;
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

procedure TParseTreeParserStatementsTest.TestProcCallWithArgs;
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

procedure TParseTreeParserStatementsTest.TestProcCallQualified;
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

procedure TParseTreeParserStatementsTest.TestProcCallQualifiedWithArgs;
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

procedure TParseTreeParserStatementsTest.TestProcCallNestedParens;
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

procedure TParseTreeParserStatementsTest.TestProcCallInherited;
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

procedure TParseTreeParserStatementsTest.TestProcCallInheritedWithArgs;
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

procedure TParseTreeParserStatementsTest.TestProcCallExit;
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

procedure TParseTreeParserStatementsTest.TestProcCallExitWithValue;
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

procedure TParseTreeParserStatementsTest.TestProcCallInc;
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

procedure TParseTreeParserStatementsTest.TestProcCallClassMethod;
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

procedure TParseTreeParserStatementsTest.TestProcCallMultipleConsecutive;
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

procedure TParseTreeParserStatementsTest.TestProcCallInIfThen;
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

procedure TParseTreeParserStatementsTest.TestProcCallInBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestProcCallInTryFinally;
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

procedure TParseTreeParserStatementsTest.TestProcCallInForLoop;
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

procedure TParseTreeParserStatementsTest.TestProcCallWithBrackets;
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

procedure TParseTreeParserStatementsTest.TestCaseSimple;
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

procedure TParseTreeParserStatementsTest.TestCaseMultipleItems;
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

procedure TParseTreeParserStatementsTest.TestCaseWithElse;
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

procedure TParseTreeParserStatementsTest.TestCaseCommaValues;
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

procedure TParseTreeParserStatementsTest.TestCaseRangeValues;
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

procedure TParseTreeParserStatementsTest.TestCaseStringValues;
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

procedure TParseTreeParserStatementsTest.TestCaseWithBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestCaseNested;
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

procedure TParseTreeParserStatementsTest.TestCaseInIfThenElse;
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

procedure TParseTreeParserStatementsTest.TestCaseInBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestCaseInTryFinally;
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

procedure TParseTreeParserStatementsTest.TestCaseEmpty;
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

procedure TParseTreeParserStatementsTest.TestCaseElseOnly;
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

procedure TParseTreeParserStatementsTest.TestCaseDeepNesting;
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

procedure TParseTreeParserStatementsTest.TestWithSimple;
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

procedure TParseTreeParserStatementsTest.TestWithQualifiedExpression;
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

procedure TParseTreeParserStatementsTest.TestWithMultipleExpressions;
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

procedure TParseTreeParserStatementsTest.TestWithBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestWithNested;
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

procedure TParseTreeParserStatementsTest.TestWithInBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestWithInIfThenElse;
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

procedure TParseTreeParserStatementsTest.TestWithInTryFinally;
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

procedure TParseTreeParserStatementsTest.TestWithInForLoop;
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

procedure TParseTreeParserStatementsTest.TestWithMethodCall;
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

procedure TParseTreeParserStatementsTest.TestWithAssignment;
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

procedure TParseTreeParserStatementsTest.TestWithDeepNesting;
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

procedure TParseTreeParserStatementsTest.TestForInSimple;
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

procedure TParseTreeParserStatementsTest.TestForInQualifiedCollection;
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

procedure TParseTreeParserStatementsTest.TestForInMethodCallCollection;
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

procedure TParseTreeParserStatementsTest.TestForInBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestForInNested;
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

procedure TParseTreeParserStatementsTest.TestForInInBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestForInInIfThenElse;
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

procedure TParseTreeParserStatementsTest.TestForInInTryFinally;
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

procedure TParseTreeParserStatementsTest.TestForInWithStatement;
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

procedure TParseTreeParserStatementsTest.TestForInArrayProperty;
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

procedure TParseTreeParserStatementsTest.TestForInString;
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

procedure TParseTreeParserStatementsTest.TestForToStillWorks;
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

procedure TParseTreeParserStatementsTest.TestInheritedBare;
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

procedure TParseTreeParserStatementsTest.TestInheritedWithMethod;
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

procedure TParseTreeParserStatementsTest.TestInheritedWithMethodAndArgs;
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

procedure TParseTreeParserStatementsTest.TestInheritedWithMultipleArgs;
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

procedure TParseTreeParserStatementsTest.TestInheritedInConstructor;
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

procedure TParseTreeParserStatementsTest.TestInheritedInDestructor;
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

procedure TParseTreeParserStatementsTest.TestInheritedInIfThenElse;
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

procedure TParseTreeParserStatementsTest.TestInheritedInTryFinally;
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

procedure TParseTreeParserStatementsTest.TestInheritedBeforeEnd;
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

procedure TParseTreeParserStatementsTest.TestExitBare;
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

procedure TParseTreeParserStatementsTest.TestExitWithValue;
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

procedure TParseTreeParserStatementsTest.TestExitWithExpression;
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

procedure TParseTreeParserStatementsTest.TestExitInIfThen;
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

procedure TParseTreeParserStatementsTest.TestExitInTryExcept;
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

procedure TParseTreeParserStatementsTest.TestExitBeforeEnd;
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

procedure TParseTreeParserStatementsTest.TestExitWithNestedParens;
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

procedure TParseTreeParserStatementsTest.TestInheritedAndExitMixed;
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

procedure TParseTreeParserStatementsTest.TestInlineVarSimple;
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

procedure TParseTreeParserStatementsTest.TestInlineVarWithType;
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

procedure TParseTreeParserStatementsTest.TestInlineVarTypeInferred;
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

procedure TParseTreeParserStatementsTest.TestInlineVarWithTypeAndInit;
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

procedure TParseTreeParserStatementsTest.TestInlineVarMultiple;
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

procedure TParseTreeParserStatementsTest.TestInlineVarInBeginEnd;
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

procedure TParseTreeParserStatementsTest.TestInlineVarInForLoop;
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

procedure TParseTreeParserStatementsTest.TestInlineVarInIfThen;
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

procedure TParseTreeParserStatementsTest.TestInlineVarInTryFinally;
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

procedure TParseTreeParserStatementsTest.TestInlineVarStringInit;
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

procedure TParseTreeParserStatementsTest.TestInlineVarComplexExpression;
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

procedure TParseTreeParserStatementsTest.TestInlineVarBeforeEnd;
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

procedure TParseTreeParserStatementsTest.TestWriteAsProcCall;
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

procedure TParseTreeParserStatementsTest.TestReadAsProcCall;
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

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsTest);

end.
