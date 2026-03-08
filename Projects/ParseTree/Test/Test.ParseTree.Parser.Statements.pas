unit Test.ParseTree.Parser.Statements;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Nodes,
  ParseTree.Tokens,
  ParseTree.Parser;

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

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserStatementsTest);

end.
