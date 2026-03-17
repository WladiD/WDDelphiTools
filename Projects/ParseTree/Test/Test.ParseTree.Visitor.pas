unit Test.ParseTree.Visitor;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Nodes,
  ParseTree.Parser,
  ParseTree.Visitor;

type
  // A mock visitor to count how many times each node type is visited
  TMockVisitor = class(TParseTreeVisitor)
  public
    CompilationUnitCount: Integer;
    UsesClauseCount: Integer;
    InterfaceSectionCount: Integer;
    ImplementationSectionCount: Integer;
    TypeSectionCount: Integer;
    ClassDeclarationCount: Integer;
    RecordDeclarationCount: Integer;
    TypeDeclarationCount: Integer;
    MethodImplementationCount: Integer;

    procedure VisitCompilationUnit(ANode: TCompilationUnitSyntax); override;
    procedure VisitUsesClause(ANode: TUsesClauseSyntax); override;
    procedure VisitInterfaceSection(ANode: TInterfaceSectionSyntax); override;
    procedure VisitImplementationSection(ANode: TImplementationSectionSyntax); override;
    procedure VisitTypeSection(ANode: TTypeSectionSyntax); override;
    procedure VisitClassDeclaration(ANode: TClassDeclarationSyntax); override;
    procedure VisitRecordDeclaration(ANode: TRecordDeclarationSyntax); override;
    procedure VisitTypeDeclaration(ANode: TTypeDeclarationSyntax); override;
    procedure VisitMethodImplementation(ANode: TMethodImplementationSyntax); override;
  end;

  [TestFixture]
  TParseTreeVisitorTest = class
  private
    FParser: TParseTreeParser;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestVisitorTraversal;
  end;

implementation

{ TMockVisitor }

procedure TMockVisitor.VisitCompilationUnit(ANode: TCompilationUnitSyntax);
begin
  Inc(CompilationUnitCount);
  inherited;
end;

procedure TMockVisitor.VisitUsesClause(ANode: TUsesClauseSyntax);
begin
  Inc(UsesClauseCount);
  inherited;
end;

procedure TMockVisitor.VisitInterfaceSection(ANode: TInterfaceSectionSyntax);
begin
  Inc(InterfaceSectionCount);
  inherited;
end;

procedure TMockVisitor.VisitImplementationSection(ANode: TImplementationSectionSyntax);
begin
  Inc(ImplementationSectionCount);
  inherited;
end;

procedure TMockVisitor.VisitTypeSection(ANode: TTypeSectionSyntax);
begin
  Inc(TypeSectionCount);
  inherited;
end;

procedure TMockVisitor.VisitClassDeclaration(ANode: TClassDeclarationSyntax);
begin
  Inc(ClassDeclarationCount);
  inherited;
end;

procedure TMockVisitor.VisitRecordDeclaration(ANode: TRecordDeclarationSyntax);
begin
  Inc(RecordDeclarationCount);
  inherited;
end;

procedure TMockVisitor.VisitTypeDeclaration(ANode: TTypeDeclarationSyntax);
begin
  Inc(TypeDeclarationCount);
  inherited;
end;

procedure TMockVisitor.VisitMethodImplementation(ANode: TMethodImplementationSyntax);
begin
  Inc(MethodImplementationCount);
  inherited;
end;

{ TParseTreeVisitorTest }

procedure TParseTreeVisitorTest.Setup;
begin
  FParser := TParseTreeParser.Create;
end;

procedure TParseTreeVisitorTest.TearDown;
begin
  FParser.Free;
end;

procedure TParseTreeVisitorTest.TestVisitorTraversal;
const
  LSourceCode = '''
    unit Unit1;
    interface
    uses System.Classes;
    
    type
      TMyClass = class
      end;
      
      TMyRecord = record
      end;
      
      TMyEnum = (enOne, enTwo);
      
    implementation
    uses System.SysUtils;
    
    procedure TMyClass.DoSomething;
    begin
    end;
    
    end.
  ''';
var
  LUnit: TCompilationUnitSyntax;
  LVisitor: TMockVisitor;
begin
  LUnit := FParser.Parse(LSourceCode);
  try
    Assert.IsNotNull(LUnit);
    
    LVisitor := TMockVisitor.Create;
    try
      LVisitor.Visit(LUnit);
      
      Assert.AreEqual(1, LVisitor.CompilationUnitCount, 'CompilationUnitCount');
      Assert.AreEqual(1, LVisitor.InterfaceSectionCount, 'InterfaceSectionCount');
      Assert.AreEqual(1, LVisitor.ImplementationSectionCount, 'ImplementationSectionCount');
      Assert.AreEqual(2, LVisitor.UsesClauseCount, 'UsesClauseCount');
      Assert.AreEqual(1, LVisitor.TypeSectionCount, 'TypeSectionCount');
      Assert.AreEqual(1, LVisitor.ClassDeclarationCount, 'ClassDeclarationCount');
      Assert.AreEqual(1, LVisitor.RecordDeclarationCount, 'RecordDeclarationCount');
      // TypeDeclarationCount is 3 because Class and Record call inherited VisitTypeDeclaration by default
      Assert.AreEqual(3, LVisitor.TypeDeclarationCount, 'TypeDeclarationCount');
      Assert.AreEqual(1, LVisitor.MethodImplementationCount, 'MethodImplementationCount');
    finally
      LVisitor.Free;
    end;
  finally
    LUnit.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeVisitorTest);

end.