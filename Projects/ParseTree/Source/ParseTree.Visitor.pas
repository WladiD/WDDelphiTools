unit ParseTree.Visitor;

interface

uses
  ParseTree.Core,
  ParseTree.Nodes;

type
  TParseTreeVisitor = class
  public
    procedure Visit(ANode: TSyntaxNode); virtual;

    // Root and major sections
    procedure VisitCompilationUnit(ANode: TCompilationUnitSyntax); virtual;
    procedure VisitUsesClause(ANode: TUsesClauseSyntax); virtual;
    procedure VisitInterfaceSection(ANode: TInterfaceSectionSyntax); virtual;
    procedure VisitImplementationSection(ANode: TImplementationSectionSyntax); virtual;

    // Declarations
    procedure VisitTypeSection(ANode: TTypeSectionSyntax); virtual;
    procedure VisitConstSection(ANode: TConstSectionSyntax); virtual;
    procedure VisitVarSection(ANode: TVarSectionSyntax); virtual;

    procedure VisitTypeDeclaration(ANode: TTypeDeclarationSyntax); virtual;
    procedure VisitClassDeclaration(ANode: TClassDeclarationSyntax); virtual;
    procedure VisitRecordDeclaration(ANode: TRecordDeclarationSyntax); virtual;
    procedure VisitConstDeclaration(ANode: TConstDeclarationSyntax); virtual;
    procedure VisitVarDeclaration(ANode: TVarDeclarationSyntax); virtual;
    procedure VisitMethodImplementation(ANode: TMethodImplementationSyntax); virtual;

    // Statements
    procedure VisitStatement(ANode: TStatementSyntax); virtual;
  end;

implementation

{ TParseTreeVisitor }

procedure TParseTreeVisitor.Visit(ANode: TSyntaxNode);
begin
  if ANode = nil then Exit;

  if ANode is TCompilationUnitSyntax then
    VisitCompilationUnit(TCompilationUnitSyntax(ANode))
  else if ANode is TUsesClauseSyntax then
    VisitUsesClause(TUsesClauseSyntax(ANode))
  else if ANode is TInterfaceSectionSyntax then
    VisitInterfaceSection(TInterfaceSectionSyntax(ANode))
  else if ANode is TImplementationSectionSyntax then
    VisitImplementationSection(TImplementationSectionSyntax(ANode))
  else if ANode is TTypeSectionSyntax then
    VisitTypeSection(TTypeSectionSyntax(ANode))
  else if ANode is TConstSectionSyntax then
    VisitConstSection(TConstSectionSyntax(ANode))
  else if ANode is TVarSectionSyntax then
    VisitVarSection(TVarSectionSyntax(ANode))
  else if ANode is TClassDeclarationSyntax then
    VisitClassDeclaration(TClassDeclarationSyntax(ANode))
  else if ANode is TRecordDeclarationSyntax then
    VisitRecordDeclaration(TRecordDeclarationSyntax(ANode))
  else if ANode is TTypeDeclarationSyntax then
    VisitTypeDeclaration(TTypeDeclarationSyntax(ANode))
  else if ANode is TConstDeclarationSyntax then
    VisitConstDeclaration(TConstDeclarationSyntax(ANode))
  else if ANode is TVarDeclarationSyntax then
    VisitVarDeclaration(TVarDeclarationSyntax(ANode))
  else if ANode is TMethodImplementationSyntax then
    VisitMethodImplementation(TMethodImplementationSyntax(ANode))
  else if ANode is TStatementSyntax then
    VisitStatement(TStatementSyntax(ANode));
end;

procedure TParseTreeVisitor.VisitCompilationUnit(ANode: TCompilationUnitSyntax);
var
  LDecl: TDeclarationSectionSyntax;
begin
  if ANode.InterfaceSection <> nil then
    Visit(ANode.InterfaceSection);

  for LDecl in ANode.PreInterfaceDeclarations do
    Visit(LDecl);

  if ANode.ImplementationSection <> nil then
    Visit(ANode.ImplementationSection);

  for LDecl in ANode.IntfImplDeclarations do
    Visit(LDecl);

  for LDecl in ANode.PostImplementationDeclarations do
    Visit(LDecl);
end;

procedure TParseTreeVisitor.VisitUsesClause(ANode: TUsesClauseSyntax);
begin
  // Leaf node for our purposes, or we could visit unit references
end;

procedure TParseTreeVisitor.VisitInterfaceSection(ANode: TInterfaceSectionSyntax);
var
  LDecl: TDeclarationSectionSyntax;
begin
  if ANode.UsesClause <> nil then
    Visit(ANode.UsesClause);

  for LDecl in ANode.Declarations do
    Visit(LDecl);
end;

procedure TParseTreeVisitor.VisitImplementationSection(ANode: TImplementationSectionSyntax);
var
  LDecl: TDeclarationSectionSyntax;
begin
  if ANode.UsesClause <> nil then
    Visit(ANode.UsesClause);

  for LDecl in ANode.Declarations do
    Visit(LDecl);
end;

procedure TParseTreeVisitor.VisitTypeSection(ANode: TTypeSectionSyntax);
var
  LDecl: TTypeDeclarationSyntax;
begin
  for LDecl in ANode.Declarations do
    Visit(LDecl);
end;

procedure TParseTreeVisitor.VisitConstSection(ANode: TConstSectionSyntax);
var
  LDecl: TConstDeclarationSyntax;
begin
  for LDecl in ANode.Declarations do
    Visit(LDecl);
end;

procedure TParseTreeVisitor.VisitVarSection(ANode: TVarSectionSyntax);
var
  LDecl: TVarDeclarationSyntax;
begin
  for LDecl in ANode.Declarations do
    Visit(LDecl);
end;

procedure TParseTreeVisitor.VisitTypeDeclaration(ANode: TTypeDeclarationSyntax);
begin
  // Leaf node for our purposes
end;

procedure TParseTreeVisitor.VisitClassDeclaration(ANode: TClassDeclarationSyntax);
begin
  // By default, just do what TypeDeclaration does
  VisitTypeDeclaration(ANode);
end;

procedure TParseTreeVisitor.VisitRecordDeclaration(ANode: TRecordDeclarationSyntax);
begin
  // By default, just do what TypeDeclaration does
  VisitTypeDeclaration(ANode);
end;

procedure TParseTreeVisitor.VisitConstDeclaration(ANode: TConstDeclarationSyntax);
begin
  // Leaf node for our purposes
end;

procedure TParseTreeVisitor.VisitVarDeclaration(ANode: TVarDeclarationSyntax);
begin
  // Leaf node for our purposes
end;

procedure TParseTreeVisitor.VisitMethodImplementation(ANode: TMethodImplementationSyntax);
var
  LDecl: TDeclarationSectionSyntax;
  LStmt: TStatementSyntax;
begin
  for LDecl in ANode.LocalDeclarations do
    Visit(LDecl);

  for LStmt in ANode.Statements do
    Visit(LStmt);
end;

procedure TParseTreeVisitor.VisitStatement(ANode: TStatementSyntax);
begin
  // We don't need to traverse into statements for the formatter right now,
  // but if we did, we would dispatch to specific statement types here.
end;

end.
