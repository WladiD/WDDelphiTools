unit Test.ParseTree.Parser;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  DUnitX.TestFramework, ParseTree.Parser, ParseTree.Nodes, ParseTree.Serializer;

type
  [TestFixture]
  TParseTreeParserTest = class
  private
    FParser: TParseTreeParser;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestParseAllProjectFiles;

    [Test]
    procedure TestParserSerialization;
  end;

implementation

{ TParseTreeParserTest }

procedure TParseTreeParserTest.Setup;
begin
  FParser := TParseTreeParser.Create;
end;

procedure TParseTreeParserTest.TearDown;
begin
  FParser.Free;
end;

procedure TParseTreeParserTest.TestParseAllProjectFiles;
var
  LFiles: TArray<string>;
  LFile: string;
  LContent: string;
  LTree: TCompilationUnitSyntax;
  LFailedFiles: TStringList;
  LProjectsDir: string;
begin
  LProjectsDir := 'C:\WDC\WDDelphiTools\Projects\';
  if not TDirectory.Exists(LProjectsDir) then
    Assert.Pass('Projects directory not found, skipping integration test.');

  LFiles := TDirectory.GetFiles(LProjectsDir, '*.pas', TSearchOption.soAllDirectories);
  
  if Length(LFiles) = 0 then
    Assert.Pass('No .pas files found to test.');

  LFailedFiles := TStringList.Create;
  try
    for LFile in LFiles do
    begin
      try
        LContent := TFile.ReadAllText(LFile, TEncoding.UTF8); // Assuming UTF8, fallbacks could be needed
        LTree := FParser.Parse(LContent);
        if Assigned(LTree) then
          LTree.Free;
      except
        on E: Exception do
        begin
          LFailedFiles.Add(Format('%s (Error: %s)', [LFile, E.Message]));
        end;
      end;
    end;

    if LFailedFiles.Count > 0 then
      Assert.Fail(Format('Failed to parse %d files out of %d. First error: %s', 
        [LFailedFiles.Count, Length(LFiles), LFailedFiles[0]]));
        
  finally
    LFailedFiles.Free;
  end;
end;

procedure TParseTreeParserTest.TestParserSerialization;
const
  LSourceCode = 'unit Unit1;' + #13#10 +
                'interface' + #13#10 +
                'uses' + #13#10 +
                '  System.SysUtils;' + #13#10;
var
  LTree: TCompilationUnitSyntax;
  LSerializer: TSyntaxTreeSerializer;
  LJsonObj: System.JSON.TJSONObject;
  LJsonString: string;
begin
  LTree := FParser.Parse(LSourceCode);
  try
    LSerializer := TSyntaxTreeSerializer.Create;
    try
      LJsonObj := LSerializer.SerializeNode(LTree);
      try
        LJsonString := LJsonObj.ToString;

        System.Writeln('PARSED JSON:');
        System.Writeln(LJsonString);

        Assert.IsTrue(LJsonString.Contains('"NodeType":"CompilationUnit"'));
        Assert.IsTrue(LJsonString.Contains('"Text":"Unit1"'));
        
        // The parser returns each part of the uses clause separately
        Assert.IsTrue(LJsonString.Contains('"Text":"System"'));
        Assert.IsTrue(LJsonString.Contains('"Kind":"tkDot"'));
        Assert.IsTrue(LJsonString.Contains('"Text":"SysUtils"'));
      finally
        LJsonObj.Free;
      end;
    finally
      LSerializer.Free;
    end;
  finally
    LTree.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserTest);

end.
