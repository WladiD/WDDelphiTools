unit Test.ParseTree.Parser;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Threading,
  System.SyncObjs,
  DUnitX.TestFramework,
  ParseTree.Parser,
  ParseTree.Nodes,
  ParseTree.Serializer;

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
  LProjectsDir: string;
  LTasks: TArray<ITask>;
  LFailedFiles: TStringList;
  LLock: TCriticalSection;
  LTaskIndex: Integer;
begin
  LProjectsDir := 'C:\WDC\WDDelphiTools\Projects\';
  if not TDirectory.Exists(LProjectsDir) then
    Assert.Pass('Projects directory not found, skipping integration test.');

  LFiles := TDirectory.GetFiles(LProjectsDir, '*.pas', TSearchOption.soAllDirectories);
  
  if Length(LFiles) = 0 then
    Assert.Pass('No .pas files found to test.');

  LFailedFiles := TStringList.Create;
  LLock := TCriticalSection.Create;
  try
    SetLength(LTasks, Length(LFiles));
    
    // Create a future task for each file
    for LTaskIndex := Low(LFiles) to High(LFiles) do
    begin
      LFile := LFiles[LTaskIndex];
      
      LTasks[LTaskIndex] := TTask.Run(
        procedure
        var
          LLocalParser: TParseTreeParser;
          LLocalTree: TCompilationUnitSyntax;
          LLocalContent: string;
          LLocalFile: string;
        begin
          LLocalFile := LFile;
          try
            LLocalContent := TFile.ReadAllText(LLocalFile, TEncoding.UTF8);
            
            // Each thread MUST instantiate its own parser to avoid race conditions!
            LLocalParser := TParseTreeParser.Create;
            try
              LLocalTree := LLocalParser.Parse(LLocalContent);
              if Assigned(LLocalTree) then
                LLocalTree.Free;
            finally
              LLocalParser.Free;
            end;
            
          except
            on E: Exception do
            begin
              LLock.Enter;
              try
                LFailedFiles.Add(Format('%s (Error: %s)', [LLocalFile, E.Message]));
              finally
                LLock.Leave;
              end;
            end;
          end;
        end);
    end;

    // Wait for all tasks to complete
    TTask.WaitForAll(LTasks);

    if LFailedFiles.Count > 0 then
      Assert.Fail(Format('Failed to parse %d files out of %d. First error: %s', 
        [LFailedFiles.Count, Length(LFiles), LFailedFiles[0]]));
        
  finally
    LLock.Free;
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
