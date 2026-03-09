unit Test.ParseTree.Parser.Statements.ConditionalsBlocks;

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
  TParseTreeParserStatementsConditionalsBlocksTest = class
  public
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
    procedure TestIfStatementWithNotEqualsOperator;
    [Test]
    procedure TestIfStatementWithLessOrEqualsOperator;
    [Test]
    procedure TestIfStatementWithGreaterOrEqualsOperator;
    [Test]
    procedure TestAssignmentStatement;
    [Test]
    procedure TestAssignmentWithAnonymousFunction;
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
  end;

implementation

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestIfStatement;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestElseIfChain;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestNestedIf;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestComplexIfStatement;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestIfStatementWithForbiddenSemicolon;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestIfStatementWithNotEqualsOperator;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure CompareTest; begin if A <> B then DoIt; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LIf.ConditionTokens.Count);
    Assert.AreEqual('A', LIf.ConditionTokens[0].Text);
    Assert.AreEqual(TTokenKind.tkNotEquals, LIf.ConditionTokens[1].Kind);
    Assert.AreEqual('<>', LIf.ConditionTokens[1].Text);
    Assert.AreEqual('B', LIf.ConditionTokens[2].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestIfStatementWithLessOrEqualsOperator;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure CompareTest; begin if A <= B then DoIt; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LIf.ConditionTokens.Count);
    Assert.AreEqual('A', LIf.ConditionTokens[0].Text);
    Assert.AreEqual(TTokenKind.tkLessOrEquals, LIf.ConditionTokens[1].Kind);
    Assert.AreEqual('<=', LIf.ConditionTokens[1].Text);
    Assert.AreEqual('B', LIf.ConditionTokens[2].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestIfStatementWithGreaterOrEqualsOperator;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
  LIf: TIfStatementSyntax;
begin
  LSource := 'procedure CompareTest; begin if A >= B then DoIt; end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TIfStatementSyntax);

    LIf := TIfStatementSyntax(LMethod.Statements[0]);
    Assert.AreEqual(3, LIf.ConditionTokens.Count);
    Assert.AreEqual('A', LIf.ConditionTokens[0].Text);
    Assert.AreEqual(TTokenKind.tkGreaterOrEquals, LIf.ConditionTokens[1].Kind);
    Assert.AreEqual('>=', LIf.ConditionTokens[1].Text);
    Assert.AreEqual('B', LIf.ConditionTokens[2].Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestAssignmentStatement;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestAssignmentWithAnonymousFunction;
var
  LParser: TParseTreeParser;
  LMethod: TMethodImplementationSyntax;
  LWriter: TSyntaxTreeWriter;
  LSource, LResult: string;
begin
  LSource :=
    'procedure Foo; begin ' +
    'Result := function(Progress: Real): Real begin ' +
    'if Progress > 0 then Result := Progress else Result := -Progress; ' +
    'end; ' +
    'end;';
  LParser := TParseTreeParser.Create;
  LWriter := TSyntaxTreeWriter.Create;
  try
    LMethod := LParser.ParseMethodImplementation(LSource);
    Assert.AreEqual(1, LMethod.Statements.Count);
    Assert.IsTrue(LMethod.Statements[0] is TAssignmentStatementSyntax);

    var LAssign := TAssignmentStatementSyntax(LMethod.Statements[0]);
    Assert.IsTrue(LAssign.RightTokens.Count > 0);
    Assert.AreEqual('function', LAssign.RightTokens[0].Text);
    Assert.AreEqual(';', LAssign.RightTokens.Last.Text);

    LResult := LWriter.GenerateSource(LMethod);
    Assert.AreEqual(LSource, LResult);
  finally
    LWriter.Free;
    LParser.Free;
  end;
end;

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestBeginEndSimple;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestBeginEndWithMultipleStatements;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestBeginEndNested;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestBeginEndInIfThen;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestBeginEndInIfThenElse;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestBeginEndInWhileDo;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestBeginEndInForDo;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestBeginEndEmpty;
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

procedure TParseTreeParserStatementsConditionalsBlocksTest.TestBeginEndDeepNesting;
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

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsConditionalsBlocksTest);

end.
