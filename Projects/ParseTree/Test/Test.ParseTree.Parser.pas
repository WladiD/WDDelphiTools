unit Test.ParseTree.Parser;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  System.IOUtils,
  System.JSON,
  System.Threading,
  System.SyncObjs,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Tokens,
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
    [Test]
    procedure TestParseVarDeclarations;
    [Test]
    procedure TestParseConstDeclarations;
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
  LTestProjectDir: string;
  LPathsFile: string;
  LOutputDir: string;
  LFailedFiles: TStringList;
  LLock: TCriticalSection;
  LPathLines: TStringList;
  LAllFiles: TStringList;
  LFilesInPath: TArray<string>;
  LPathLine: string;
  LPathLineTrimmed: string;
  LResolvedPath: string;
  LCheckedPathCount: Integer;
  LThreadPool: TThreadPool;
begin
  // ParamStr(0) points to Win32\Debug\Test.ParseTree.exe during test runs.
  // Relative paths from RoundTripTestPaths.txt are grounded to Projects\ParseTree\Test.
  LTestProjectDir := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\'));
  LPathsFile := TPath.Combine(LTestProjectDir, 'RoundTripTestPaths.txt');
  if not TFile.Exists(LPathsFile) then
    Assert.Fail('Roundtrip path config does not exist: ' + LPathsFile);

  LOutputDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'ProjectParseTrees');
  if TDirectory.Exists(LOutputDir) then
    TDirectory.Delete(LOutputDir, True);
  TDirectory.CreateDirectory(LOutputDir);

  LPathLines := TStringList.Create;
  LAllFiles := TStringList.Create;
  LAllFiles.Sorted := True;
  LAllFiles.Duplicates := dupIgnore;
  try
    LPathLines.LoadFromFile(LPathsFile, TEncoding.UTF8);
    LCheckedPathCount := 0;

    for LPathLine in LPathLines do
    begin
      LPathLineTrimmed := Trim(LPathLine);
      if (LPathLineTrimmed = '') or LPathLineTrimmed.StartsWith('#') then
        Continue;

      if TPath.IsPathRooted(LPathLineTrimmed) then
        LResolvedPath := TPath.GetFullPath(LPathLineTrimmed)
      else
        LResolvedPath := TPath.GetFullPath(TPath.Combine(LTestProjectDir, LPathLineTrimmed));

      if not TDirectory.Exists(LResolvedPath) then
        Assert.Fail(Format('Configured parser directory does not exist: "%s" (from "%s")',
          [LResolvedPath, LPathLineTrimmed]));

      LFilesInPath := TDirectory.GetFiles(LResolvedPath, '*.pas', TSearchOption.soTopDirectoryOnly);
      if Length(LFilesInPath) = 0 then
        Assert.Fail(Format('Configured parser directory contains no *.pas files: "%s" (from "%s")',
          [LResolvedPath, LPathLineTrimmed]));

      Inc(LCheckedPathCount);
      for var LFileIndex := Low(LFilesInPath) to High(LFilesInPath) do
        LAllFiles.Add(LFilesInPath[LFileIndex]);
    end;

    LFiles := LAllFiles.ToStringArray;
  finally
    LPathLines.Free;
    LAllFiles.Free;
  end;

  if Length(LFiles) = 0 then
    Assert.Fail('No .pas files found in configured parser paths: ' + LPathsFile);

  LFailedFiles := TStringList.Create;
  LLock := TCriticalSection.Create;
  LThreadPool := TThreadPool.Create;
  try
    LThreadPool.MaxWorkerThreads := 2;
    try
      TParallel.For(Low(LFiles), High(LFiles),
        procedure(LTaskIndex: Integer)
        var
          LTaskFile: string;
          LLocalParser: TParseTreeParser;
          LLocalSerializer: TSyntaxTreeSerializer;
          LLocalTree: TCompilationUnitSyntax;
          LLocalContent: string;
          LLocalJson: TJSONObject;
          LRelPath: string;
          LTargetFile: string;
          LSanitizedPath: string;
        begin
          LTaskFile := LFiles[LTaskIndex];
          try
            try
              LLocalContent := TFile.ReadAllText(LTaskFile, TEncoding.UTF8);
            except
              on EEncodingError do
                LLocalContent := TFile.ReadAllText(LTaskFile, TEncoding.Default);
            end;

            LLocalParser := TParseTreeParser.Create;
            LLocalSerializer := TSyntaxTreeSerializer.Create;
            try
              LLocalTree := LLocalParser.Parse(LLocalContent);
              try
                LLocalJson := LLocalSerializer.SerializeNode(LLocalTree);
                try
                  if LTaskFile.StartsWith(LTestProjectDir, True) then
                    LRelPath := ExtractRelativePath(LTestProjectDir, LTaskFile)
                  else
                  begin
                    LSanitizedPath := StringReplace(TPath.GetFullPath(LTaskFile), ':', '', [rfReplaceAll]);
                    while (Length(LSanitizedPath) > 0) and
                          ((LSanitizedPath[1] = '\') or (LSanitizedPath[1] = '/')) do
                      Delete(LSanitizedPath, 1, 1);
                    LRelPath := LSanitizedPath;
                  end;
                  LTargetFile := TPath.Combine(LOutputDir, TPath.ChangeExtension(LRelPath, '.json'));

                  LLock.Enter;
                  try
                    TDirectory.CreateDirectory(TPath.GetDirectoryName(LTargetFile));
                    TFile.WriteAllText(LTargetFile, LLocalJson.Format(2), TEncoding.UTF8);
                  finally
                    LLock.Leave;
                  end;
                finally
                  LLocalJson.Free;
                end;
              finally
                LLocalTree.Free;
              end;
            finally
              LLocalSerializer.Free;
              LLocalParser.Free;
            end;

          except
            on E: Exception do
            begin
              LLock.Enter;
              try
                LFailedFiles.Add(Format('%s (%s: %s)', [LTaskFile, E.ClassName, E.Message]));
              finally
                LLock.Leave;
              end;
            end;
          end;
        end,
        LThreadPool);
    except
      on E: EAggregateException do
      begin
        for var I := 0 to E.Count - 1 do
        begin
          LLock.Enter;
          try
            LFailedFiles.Add(Format('AggregateException[%d]: %s: %s', [I, E.InnerExceptions[I].ClassName, E.InnerExceptions[I].Message]));
          finally
            LLock.Leave;
          end;
        end;
      end;
    end;

    if LFailedFiles.Count > 0 then
      Assert.Fail(Format('Failed to parse %d files out of %d:'#13#10'%s',
        [LFailedFiles.Count, Length(LFiles), LFailedFiles.Text]));

  finally
    LThreadPool.Free;
    LLock.Free;
    LFailedFiles.Free;
  end;
end;

procedure TParseTreeParserTest.TestParserSerialization;
const
  LSourceCode = '''
    unit Unit1;
    interface
    uses
      System.SysUtils,
      {$IFDEF MSWINDOWS}
      Winapi.Windows,
      {$ENDIF}
      {$IF CompilerVersion >= 35.0}
      System.Generics.Collections,
      {$IFEND}
      My.Custom.Unit in '..\Path\My.Custom.Unit.pas';
      
    type
      TTest = class; // forward
      
    const
      MyConst = 42;
      
    var
      MyVar: Integer;
      
    implementation
  ''';
var
  LTree: TCompilationUnitSyntax;
  LSerializer: TSyntaxTreeSerializer;
  LJsonObj: System.JSON.TJSONObject;
  LJsonString: string;
  LHasIfDef, LHasIf: Boolean;
  LTrivia: TSyntaxTrivia;
begin
  LTree := FParser.Parse(LSourceCode);
  try
    LSerializer := TSyntaxTreeSerializer.Create;
    try
      LJsonObj := LSerializer.SerializeNode(LTree);
      try
        LJsonString := LJsonObj.ToString;

        Assert.IsTrue(LJsonString.Contains('"NodeType":"CompilationUnit"'));
        Assert.IsTrue(LJsonString.Contains('"Text":"Unit1"'));
        
        // Extensive AST Structure checks
        Assert.IsNotNull(LTree.InterfaceSection, 'Interface section should exist');
        Assert.IsNotNull(LTree.InterfaceSection.UsesClause, 'Uses clause should exist');
        Assert.AreEqual(4, LTree.InterfaceSection.UsesClause.UnitReferences.Count, 'Should find 4 unit references');
        
        // Check 1: System.SysUtils
        Assert.AreEqual(2, LTree.InterfaceSection.UsesClause.UnitReferences[0].Namespaces.Count, 'System.SysUtils has 2 namespaces');
        Assert.AreEqual('System', LTree.InterfaceSection.UsesClause.UnitReferences[0].Namespaces[0].Text);
        Assert.AreEqual('SysUtils', LTree.InterfaceSection.UsesClause.UnitReferences[0].Namespaces[1].Text);
        
        // Check 2: Winapi.Windows (with $IFDEF MSWINDOWS)
        Assert.AreEqual(2, LTree.InterfaceSection.UsesClause.UnitReferences[1].Namespaces.Count);
        Assert.AreEqual('Winapi', LTree.InterfaceSection.UsesClause.UnitReferences[1].Namespaces[0].Text);
        
        LHasIfDef := False;
        for LTrivia in LTree.InterfaceSection.UsesClause.UnitReferences[1].Namespaces[0].LeadingTrivia do
          if LTrivia.Text.Contains('{$IFDEF MSWINDOWS}') then LHasIfDef := True;
        Assert.IsTrue(LHasIfDef, 'Should capture {$IFDEF MSWINDOWS} as leading trivia');

        // Check 3: System.Generics.Collections (with $IF CompilerVersion)
        Assert.AreEqual(3, LTree.InterfaceSection.UsesClause.UnitReferences[2].Namespaces.Count);
        Assert.AreEqual('System', LTree.InterfaceSection.UsesClause.UnitReferences[2].Namespaces[0].Text);
        Assert.AreEqual('Generics', LTree.InterfaceSection.UsesClause.UnitReferences[2].Namespaces[1].Text);
        Assert.AreEqual('Collections', LTree.InterfaceSection.UsesClause.UnitReferences[2].Namespaces[2].Text);
        
        LHasIf := False;
        for LTrivia in LTree.InterfaceSection.UsesClause.UnitReferences[2].Namespaces[0].LeadingTrivia do
          if LTrivia.Text.Contains('{$IF CompilerVersion >= 35.0}') then LHasIf := True;
        Assert.IsTrue(LHasIf, 'Should capture {$IF CompilerVersion} as leading trivia');

        // Check 4: My.Custom.Unit in '...'
        Assert.AreEqual(3, LTree.InterfaceSection.UsesClause.UnitReferences[3].Namespaces.Count);
        Assert.AreEqual('My', LTree.InterfaceSection.UsesClause.UnitReferences[3].Namespaces[0].Text);
        Assert.AreEqual('Custom', LTree.InterfaceSection.UsesClause.UnitReferences[3].Namespaces[1].Text);
        Assert.AreEqual('Unit', LTree.InterfaceSection.UsesClause.UnitReferences[3].Namespaces[2].Text);
        Assert.IsNotNull(LTree.InterfaceSection.UsesClause.UnitReferences[3].InKeyword, 'Should have "in" keyword');
        Assert.IsNotNull(LTree.InterfaceSection.UsesClause.UnitReferences[3].StringLiteral, 'Should have string literal');
        Assert.AreEqual('''..\Path\My.Custom.Unit.pas''', LTree.InterfaceSection.UsesClause.UnitReferences[3].StringLiteral.Text);
        
        // Check Declarations (type, const, var)
        Assert.IsNotNull(LTree.InterfaceSection.Declarations, 'Declarations property must exist');
        Assert.AreEqual(3, LTree.InterfaceSection.Declarations.Count, 'Should find 3 declaration blocks');
        
        Assert.IsTrue(LTree.InterfaceSection.Declarations[0] is TTypeSectionSyntax);
        Assert.AreEqual('type', TTypeSectionSyntax(LTree.InterfaceSection.Declarations[0]).TypeKeyword.Text);

        // Check that '// forward' comment is captured as leading trivia on the const keyword
        Assert.IsTrue(LTree.InterfaceSection.Declarations[1] is TConstSectionSyntax);
        LHasIfDef := False;
        for LTrivia in TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).ConstKeyword.LeadingTrivia do
          if LTrivia.Text.Contains('// forward') then LHasIfDef := True;
        Assert.IsTrue(LHasIfDef, 'Should capture // forward as leading trivia on const keyword');

        Assert.IsTrue(LTree.InterfaceSection.Declarations[1] is TConstSectionSyntax);
        Assert.AreEqual('const', TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).ConstKeyword.Text);
        Assert.AreEqual(1, TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).Declarations.Count);
        Assert.AreEqual('MyConst', TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).Declarations[0].Identifier.Text);
        Assert.IsNotNull(TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).Declarations[0].EqualsToken);
        Assert.IsNotNull(TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).Declarations[0].ValueTokens[0]);

        Assert.IsTrue(LTree.InterfaceSection.Declarations[2] is TVarSectionSyntax);
        Assert.AreEqual('var', TVarSectionSyntax(LTree.InterfaceSection.Declarations[2]).VarKeyword.Text);
        Assert.AreEqual(1, TVarSectionSyntax(LTree.InterfaceSection.Declarations[2]).Declarations.Count);
        Assert.AreEqual('MyVar', TVarSectionSyntax(LTree.InterfaceSection.Declarations[2]).Declarations[0].Identifier.Text);
        Assert.IsNotNull(TVarSectionSyntax(LTree.InterfaceSection.Declarations[2]).Declarations[0].ColonToken);
        Assert.AreEqual('Integer', TVarSectionSyntax(LTree.InterfaceSection.Declarations[2]).Declarations[0].TypeIdentifier.Text);
        
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

procedure TParseTreeParserTest.TestParseVarDeclarations;
const
  LSourceCode = '''
    unit Unit1;
    interface
    
    var
      PlainVar: string;
      MultiVar1, MultiVar2: Integer;
  ''';
var
  LTree: TCompilationUnitSyntax;
  LVarSec: TVarSectionSyntax;
begin
  LTree := FParser.Parse(LSourceCode);
  try
    Assert.IsNotNull(LTree.InterfaceSection, 'Interface missing');
    Assert.IsNotNull(LTree.InterfaceSection.Declarations, 'Declarations missing');
    Assert.AreEqual(1, LTree.InterfaceSection.Declarations.Count);
    Assert.IsTrue(LTree.InterfaceSection.Declarations[0] is TVarSectionSyntax);
    
    LVarSec := TVarSectionSyntax(LTree.InterfaceSection.Declarations[0]);
    Assert.AreEqual(2, LVarSec.Declarations.Count, 'Should parse 2 variable blocks (including comma separated)');
    
    // First: PlainVar: string;
    Assert.AreEqual('PlainVar', LVarSec.Declarations[0].Identifier.Text);
    Assert.AreEqual('string', LVarSec.Declarations[0].TypeIdentifier.Text);
    
    // Second: MultiVar1, MultiVar2: Integer; => The parser right now just groups by identifier. 
    // We expect it to parse MultiVar1 correctly until the colon, even if commas exist.
    Assert.AreEqual('MultiVar1', LVarSec.Declarations[1].Identifier.Text);
  finally
    LTree.Free;
  end;
end;

procedure TParseTreeParserTest.TestParseConstDeclarations;
const
  LSourceCode = '''
    unit Unit1;
    interface
    
    const
      SimpleConst = 100;
      TypedConst: string = 'Hello World';
  ''';
var
  LTree: TCompilationUnitSyntax;
  LConstSec: TConstSectionSyntax;
begin
  LTree := FParser.Parse(LSourceCode);
  try
    Assert.IsNotNull(LTree.InterfaceSection, 'Interface missing');
    Assert.IsNotNull(LTree.InterfaceSection.Declarations, 'Declarations missing');
    Assert.AreEqual(1, LTree.InterfaceSection.Declarations.Count);
    Assert.IsTrue(LTree.InterfaceSection.Declarations[0] is TConstSectionSyntax);
    
    LConstSec := TConstSectionSyntax(LTree.InterfaceSection.Declarations[0]);
    Assert.AreEqual(2, LConstSec.Declarations.Count, 'Should parse 2 const lines');
    
    // SimpleConst = 100;
    Assert.AreEqual('SimpleConst', LConstSec.Declarations[0].Identifier.Text);
    Assert.AreEqual('100', LConstSec.Declarations[0].ValueTokens[0].Text);
    
    // TypedConst: string = 'Hello World';
    Assert.AreEqual('TypedConst', LConstSec.Declarations[1].Identifier.Text);
    Assert.AreEqual('''Hello World''', LConstSec.Declarations[1].ValueTokens[0].Text);
  finally
    LTree.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserTest);

end.
