program Test.ParseTree;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
{$SetPEFlags $0020} // IMAGE_FILE_LARGE_ADDRESS_AWARE
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,

  ParseTree.Core in '..\Source\ParseTree.Core.pas',
  ParseTree.Tokens in '..\Source\ParseTree.Tokens.pas',
  ParseTree.Nodes in '..\Source\ParseTree.Nodes.pas',
  ParseTree.Serializer in '..\Source\ParseTree.Serializer.pas',
  ParseTree.Writer in '..\Source\ParseTree.Writer.pas',
  ParseTree.Parser in '..\Source\ParseTree.Parser.pas',
  ParseTree.Lexer in '..\Source\ParseTree.Lexer.pas',
  ParseTree.Visitor in '..\Source\ParseTree.Visitor.pas',
  Test.ParseTree.Nodes in 'Test.ParseTree.Nodes.pas',
  Test.ParseTree.Parser in 'Test.ParseTree.Parser.pas',
  Test.ParseTree.Roundtrip in 'Test.ParseTree.Roundtrip.pas',
  Test.ParseTree.Utils in 'Test.ParseTree.Utils.pas',
  Test.ParseTree.Parser.ClassDecl in 'Test.ParseTree.Parser.ClassDecl.pas',
  Test.ParseTree.Parser.Statements.Loops in 'Test.ParseTree.Parser.Statements.Loops.pas',
  Test.ParseTree.Parser.Statements.ConditionalsBlocks in 'Test.ParseTree.Parser.Statements.ConditionalsBlocks.pas',
  Test.ParseTree.Parser.Statements.CaseWith in 'Test.ParseTree.Parser.Statements.CaseWith.pas',
  Test.ParseTree.Parser.Statements.ExceptionFlow in 'Test.ParseTree.Parser.Statements.ExceptionFlow.pas',
  Test.ParseTree.Parser.Statements.ProcCall.Basic in 'Test.ParseTree.Parser.Statements.ProcCall.Basic.pas',
  Test.ParseTree.Parser.Statements.InheritedExit in 'Test.ParseTree.Parser.Statements.InheritedExit.pas',
  Test.ParseTree.Parser.Statements.InlineVar in 'Test.ParseTree.Parser.Statements.InlineVar.pas',
  Test.ParseTree.Parser.Statements.ProcCall.Advanced in 'Test.ParseTree.Parser.Statements.ProcCall.Advanced.pas',
  Test.ParseTree.Lexer in 'Test.ParseTree.Lexer.pas',
  Test.ParseTree.Visitor in 'Test.ParseTree.Visitor.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    TDUnitX.CheckCommandLine;
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;
    runner.FailsOnNoAsserts := False;

    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;

    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
