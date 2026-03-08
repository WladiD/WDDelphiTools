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
    // Then statement should be TOpaque (begin ... end for now)
    Assert.IsTrue(LIf.ThenStatement is TOpaqueStatementSyntax);
    
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
    // Then branch of LIfE is a begin...end (Opaque for now)
    Assert.IsTrue(LIfE.ThenStatement is TOpaqueStatementSyntax);
    
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

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsTest);

end.
