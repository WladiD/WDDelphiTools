unit Test.ParseTree.Roundtrip;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Tokens,
  ParseTree.Nodes,
  ParseTree.Parser,
  ParseTree.Writer;

type
  [TestFixture]
  TParseTreeRoundtripTest = class
  private
    FParser: TParseTreeParser;
    procedure DoRoundtripTest(const AFilePath: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestDPTMcpDebuggerTask;
    [Test]
    procedure TestDPTLogger;
  end;

implementation

{ TParseTreeRoundtripTest }

procedure TParseTreeRoundtripTest.Setup;
begin
  FParser := TParseTreeParser.Create;
end;

procedure TParseTreeRoundtripTest.TearDown;
begin
  FParser.Free;
end;

procedure TParseTreeRoundtripTest.DoRoundtripTest(const AFilePath: string);
var
  LOriContent: string;
  LTree: TCompilationUnitSyntax;
  LTreeWriter: TSyntaxTreeWriter;
  LNewContent: string;
  LBaseDir: string;
  LOutputFolder: string;
  LOutputFile: string;
begin
  Assert.IsTrue(TFile.Exists(AFilePath), 'Source file does not exist: ' + AFilePath);
  
  LOriContent := TFile.ReadAllText(AFilePath, TEncoding.UTF8);
  LTree := FParser.Parse(LOriContent);
  try
    LTreeWriter := TSyntaxTreeWriter.Create;
    try
      LNewContent := LTreeWriter.GenerateSource(LTree);
    finally
      LTreeWriter.Free;
    end;
  finally
    LTree.Free;
  end;

  // Save to RoundTripTest output folder
  LBaseDir := ExtractFilePath(ParamStr(0));
  LOutputFolder := TPath.Combine(LBaseDir, 'RoundTripTest');
  if not TDirectory.Exists(LOutputFolder) then
    TDirectory.CreateDirectory(LOutputFolder);
    
  LOutputFile := TPath.Combine(LOutputFolder, ExtractFileName(AFilePath));
  TFile.WriteAllText(LOutputFile, LNewContent, TEncoding.UTF8);

  // Assert Exact Match
  Assert.AreEqual(LOriContent, LNewContent, 
    'Roundtrip parsing failed for ' + ExtractFileName(AFilePath) + 
    '. Check file ' + LOutputFile + ' for the generated output.');
end;

procedure TParseTreeRoundtripTest.TestDPTMcpDebuggerTask;
var
  LProjectsDir: string;
  LTargetFile: string;
begin
  // Calculate relative path to project dir
  LProjectsDir := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\..\'));
  LTargetFile := TPath.Combine(LProjectsDir, 'DPT\Source\DPT.McpDebugger.Task.pas');
  
  DoRoundtripTest(LTargetFile);
end;

procedure TParseTreeRoundtripTest.TestDPTLogger;
var
  LProjectsDir: string;
  LTargetFile: string;
begin
  LProjectsDir := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\..\'));
  LTargetFile := TPath.Combine(LProjectsDir, 'DPT\Source\DPT.Logger.pas');
  
  DoRoundtripTest(LTargetFile);
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeRoundtripTest);

end.
