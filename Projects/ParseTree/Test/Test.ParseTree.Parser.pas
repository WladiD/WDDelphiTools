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
    [Test]
    procedure TestParseClassDeclaration;
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
      TTest = class;
      
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

        System.Writeln('PARSED JSON:');
        System.Writeln(LJsonString);

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

        Assert.IsTrue(LTree.InterfaceSection.Declarations[1] is TConstSectionSyntax);
        Assert.AreEqual('const', TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).ConstKeyword.Text);
        Assert.AreEqual(1, TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).Declarations.Count);
        Assert.AreEqual('MyConst', TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).Declarations[0].Identifier.Text);
        Assert.IsNotNull(TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).Declarations[0].EqualsToken);
        Assert.IsNotNull(TConstSectionSyntax(LTree.InterfaceSection.Declarations[1]).Declarations[0].ValueToken);

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
    Assert.AreEqual('100', LConstSec.Declarations[0].ValueToken.Text);
    
    // TypedConst: string = 'Hello World';
    Assert.AreEqual('TypedConst', LConstSec.Declarations[1].Identifier.Text);
    Assert.AreEqual('''Hello World''', LConstSec.Declarations[1].ValueToken.Text);
  finally
    LTree.Free;
  end;
end;

procedure TParseTreeParserTest.TestParseClassDeclaration;
const
  LSourceCode = '''
    unit Unit1;
    interface
    type
      /// <summary>Main application class</summary>
      TMyClass = class
      private
        /// <summary>Internal enumeration</summary>
        type
          TInnerEnum = (ieOne, ieTwo);
        /// <summary>Internal state flag</summary>
        var
          FInternalFlag: Boolean;
        /// <summary>Maximum number of items</summary>
        const
          CMaxItems = 100;
      strict private
        /// <summary>Strict private field for name storage</summary>
        FStrictField: string;
      protected
        /// <summary>Protected floating point field</summary>
        FProtField: Double;
        /// <summary>Updates internal state</summary>
        procedure InternalUpdate;
      strict protected
        /// <summary>Strict protected helper routine</summary>
        procedure StrictHelper;
      public
        /// <summary>Creates a new instance</summary>
        constructor Create;
        /// <summary>Releases all resources</summary>
        destructor Destroy; override;
        /// <summary>Performs the main action</summary>
        procedure DoSomething;
        /// <summary>Returns the current value</summary>
        function GetValue: Integer;
        /// <summary>Factory method for creating instances</summary>
        class function CreateInstance: TMyClass;
        /// <summary>Frees all existing instances</summary>
        class procedure FreeAll;
        /// <summary>The current value</summary>
        property Value: Integer read GetValue;
      published
        /// <summary>Display name of this object</summary>
        property Name: string read FStrictField;
      end;
  ''';

  function HasTriviaContaining(AToken: TSyntaxToken; const AText: string): Boolean;
  var
    LTrivia: TSyntaxTrivia;
  begin
    Result := False;
    if AToken = nil then Exit;
    for LTrivia in AToken.LeadingTrivia do
      if LTrivia.Text.Contains(AText) then
        Exit(True);
  end;

  function GetFirstMemberToken(ASection: TVisibilitySectionSyntax; AMemberIndex: Integer): TSyntaxToken;
  begin
    Result := nil;
    if (AMemberIndex < ASection.Members.Count) and (ASection.Members[AMemberIndex].Tokens.Count > 0) then
      Result := ASection.Members[AMemberIndex].Tokens[0];
  end;

var
  LTree: TCompilationUnitSyntax;
  LTypeSec: TTypeSectionSyntax;
  LTypeDecl: TTypeDeclarationSyntax;
  LVisSec: TVisibilitySectionSyntax;
begin
  LTree := FParser.Parse(LSourceCode);
  try
    Assert.IsNotNull(LTree.InterfaceSection, 'Interface missing');
    Assert.AreEqual(1, LTree.InterfaceSection.Declarations.Count);
    Assert.IsTrue(LTree.InterfaceSection.Declarations[0] is TTypeSectionSyntax);
    
    LTypeSec := TTypeSectionSyntax(LTree.InterfaceSection.Declarations[0]);
    Assert.AreEqual(1, LTypeSec.Declarations.Count, 'Should parse one type declaration');
    
    LTypeDecl := LTypeSec.Declarations[0];
    Assert.AreEqual('TMyClass', LTypeDecl.Identifier.Text);
    Assert.AreEqual('class', LTypeDecl.TypeTypeToken.Text);
    Assert.IsNotNull(LTypeDecl.EndKeyword, 'Should have end keyword');
    
    // Verify XML-Doc on class itself
    Assert.IsTrue(HasTriviaContaining(LTypeDecl.Identifier, '/// <summary>Main application class</summary>'),
      'TMyClass should have XML-Doc trivia');
    
    // Should have 6 visibility sections
    Assert.AreEqual(6, LTypeDecl.VisibilitySections.Count, 'Should have 6 visibility sections');
    
    // === Section 0: private ===
    LVisSec := LTypeDecl.VisibilitySections[0];
    Assert.AreEqual('private', LVisSec.VisibilityKeyword.Text);
    Assert.IsFalse(LVisSec.IsStrict, 'private should not be strict');
    Assert.AreEqual(3, LVisSec.Members.Count, 'private should have 3 members (type, var, const)');
    // Check XML-Doc on private members
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Internal enumeration'),
      'type TInnerEnum should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Internal state flag'),
      'var FInternalFlag should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 2), 'Maximum number of items'),
      'const CMaxItems should have XML-Doc');
    
    // === Section 1: strict private ===
    LVisSec := LTypeDecl.VisibilitySections[1];
    Assert.AreEqual('private', LVisSec.VisibilityKeyword.Text);
    Assert.IsTrue(LVisSec.IsStrict, 'strict private should be strict');
    Assert.AreEqual(1, LVisSec.Members.Count, 'strict private should have 1 member');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Strict private field for name storage'),
      'FStrictField should have XML-Doc');
    
    // === Section 2: protected ===
    LVisSec := LTypeDecl.VisibilitySections[2];
    Assert.AreEqual('protected', LVisSec.VisibilityKeyword.Text);
    Assert.IsFalse(LVisSec.IsStrict);
    Assert.AreEqual(2, LVisSec.Members.Count, 'protected should have 2 members');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Protected floating point field'),
      'FProtField should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Updates internal state'),
      'InternalUpdate should have XML-Doc');
    
    // === Section 3: strict protected ===
    LVisSec := LTypeDecl.VisibilitySections[3];
    Assert.AreEqual('protected', LVisSec.VisibilityKeyword.Text);
    Assert.IsTrue(LVisSec.IsStrict);
    Assert.AreEqual(1, LVisSec.Members.Count);
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Strict protected helper routine'),
      'StrictHelper should have XML-Doc');
    
    // === Section 4: public ===
    LVisSec := LTypeDecl.VisibilitySections[4];
    Assert.AreEqual('public', LVisSec.VisibilityKeyword.Text);
    Assert.AreEqual(7, LVisSec.Members.Count, 'public should have 7 members');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Creates a new instance'),
      'constructor Create should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 1), 'Releases all resources'),
      'destructor Destroy should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 2), 'Performs the main action'),
      'DoSomething should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 3), 'Returns the current value'),
      'GetValue should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 4), 'Factory method for creating instances'),
      'CreateInstance should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 5), 'Frees all existing instances'),
      'FreeAll should have XML-Doc');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 6), 'The current value'),
      'property Value should have XML-Doc');

    // === Section 5: published ===
    LVisSec := LTypeDecl.VisibilitySections[5];
    Assert.AreEqual('published', LVisSec.VisibilityKeyword.Text);
    Assert.AreEqual(1, LVisSec.Members.Count, 'published should have 1 property');
    Assert.IsTrue(HasTriviaContaining(GetFirstMemberToken(LVisSec, 0), 'Display name of this object'),
      'property Name should have XML-Doc');
  finally
    LTree.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeParserTest);

end.
