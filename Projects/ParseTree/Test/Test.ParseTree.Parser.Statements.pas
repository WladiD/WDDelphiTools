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
  LStmt: TStatementSyntax;
  LRepeat: TRepeatStatementSyntax;
begin
  LParser := TParseTreeParser.Create;
  try
    LMethod := LParser.ParseMethodImplementation('procedure Bar; begin repeat Inc(x); until x > 10; end;');
    Assert.AreEqual(1, LMethod.Statements.Count);
    LStmt := LMethod.Statements[0];
    Assert.IsTrue(LStmt is TRepeatStatementSyntax);
    LRepeat := TRepeatStatementSyntax(LStmt);
    
    Assert.IsNotNull(LRepeat.RepeatKeyword);
    Assert.AreEqual(TTokenKind.tkRepeatKeyword, LRepeat.RepeatKeyword.Kind);
    Assert.IsNotNull(LRepeat.UntilKeyword);
    Assert.AreEqual(TTokenKind.tkUntilKeyword, LRepeat.UntilKeyword.Kind);
  finally
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
    Assert.IsTrue(LIf2.ElseStatement is TOpaqueStatementSyntax);
    
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
    Assert.IsTrue(LIfE.ElseStatement is TOpaqueStatementSyntax);
    
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
    Assert.IsTrue(LIf.ThenStatement is TOpaqueStatementSyntax);
    Assert.AreEqual('b;', TOpaqueStatementSyntax(LIf.ThenStatement).Tokens[0].Text + TOpaqueStatementSyntax(LIf.ThenStatement).Tokens[1].Text);
    
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

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsTest);

end.
