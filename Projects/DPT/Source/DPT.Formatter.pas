unit DPT.Formatter;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  ParseTree.Core, ParseTree.Nodes, ParseTree.Visitor;

type
  {
    Base class for DPT formatting and DWScript integration,
    which traverses the ParseTree and calls formatting hooks.
  }
  TDptFormatter = class(TParseTreeVisitor)
  private
    FUnit: TCompilationUnitSyntax;
  protected
    // Hooks for descendant classes (e.g. TDptDwsFormatter)
    procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax); virtual;
    procedure OnVisitUsesClause(AUses: TUsesClauseSyntax); virtual;
    procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax); virtual;
    procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax); virtual;
    procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax); virtual;
    procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax); virtual;
    procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax); virtual;
    procedure OnVisitTypeSection(ASection: TTypeSectionSyntax); virtual;
    procedure OnVisitConstSection(ASection: TConstSectionSyntax); virtual;
    procedure OnVisitVarSection(ASection: TVarSectionSyntax); virtual;
    procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax); virtual;

  public
    // Overrides from TParseTreeVisitor
    procedure VisitUsesClause(ANode: TUsesClauseSyntax); override;
    procedure VisitClassDeclaration(ANode: TClassDeclarationSyntax); override;
    procedure VisitRecordDeclaration(ANode: TRecordDeclarationSyntax); override;
    procedure VisitMethodImplementation(ANode: TMethodImplementationSyntax); override;
    procedure VisitInterfaceSection(ANode: TInterfaceSectionSyntax); override;
    procedure VisitImplementationSection(ANode: TImplementationSectionSyntax); override;
    procedure VisitTypeSection(ANode: TTypeSectionSyntax); override;
    procedure VisitConstSection(ANode: TConstSectionSyntax); override;
    procedure VisitVarSection(ANode: TVarSectionSyntax); override;
    procedure VisitCompilationUnit(ANode: TCompilationUnitSyntax); override;

    // Starts the formatting and traversal
    procedure FormatUnit(AUnit: TCompilationUnitSyntax); virtual;
    
    // Helper methods for token/trivia manipulation
    class procedure ClearTrivia(AToken: TSyntaxToken);
    class function GetLeadingTrivia(AToken: TSyntaxToken): string;
    class procedure AddLeadingTrivia(AToken: TSyntaxToken; const ATriviaText: string);
    class procedure AddTrailingTrivia(AToken: TSyntaxToken; const ATriviaText: string);
  end;

implementation

{ TDptFormatter }

procedure TDptFormatter.FormatUnit(AUnit: TCompilationUnitSyntax);
begin
  FUnit := AUnit;
  if Assigned(FUnit) then
    Visit(FUnit);
end;

procedure TDptFormatter.OnVisitClassDeclaration(AClass: TClassDeclarationSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitImplementationSection(ASection: TImplementationSectionSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitTypeSection(ASection: TTypeSectionSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitConstSection(ASection: TConstSectionSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitVarSection(ASection: TVarSectionSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitUnitStart(AUnit: TCompilationUnitSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.OnVisitUsesClause(AUses: TUsesClauseSyntax);
begin
  // Do nothing by default
end;

procedure TDptFormatter.VisitClassDeclaration(ANode: TClassDeclarationSyntax);
begin
  OnVisitClassDeclaration(ANode);
  inherited;
end;

procedure TDptFormatter.VisitMethodImplementation(ANode: TMethodImplementationSyntax);
begin
  OnVisitMethodImplementation(ANode);
  inherited;
end;

procedure TDptFormatter.VisitInterfaceSection(ANode: TInterfaceSectionSyntax);
begin
  OnVisitInterfaceSection(ANode);
  inherited;
end;

procedure TDptFormatter.VisitImplementationSection(ANode: TImplementationSectionSyntax);
begin
  OnVisitImplementationSection(ANode);
  inherited;
end;

procedure TDptFormatter.VisitTypeSection(ANode: TTypeSectionSyntax);
begin
  OnVisitTypeSection(ANode);
  inherited;
end;

procedure TDptFormatter.VisitConstSection(ANode: TConstSectionSyntax);
begin
  OnVisitConstSection(ANode);
  inherited;
end;

procedure TDptFormatter.VisitVarSection(ANode: TVarSectionSyntax);
begin
  OnVisitVarSection(ANode);
  inherited;
end;

procedure TDptFormatter.VisitCompilationUnit(ANode: TCompilationUnitSyntax);
begin
  OnVisitUnitStart(ANode);
  inherited;
  OnVisitUnitEnd(ANode);
end;

procedure TDptFormatter.VisitRecordDeclaration(ANode: TRecordDeclarationSyntax);
begin
  OnVisitRecordDeclaration(ANode);
  inherited;
end;

procedure TDptFormatter.VisitUsesClause(ANode: TUsesClauseSyntax);
begin
  OnVisitUsesClause(ANode);
  inherited;
end;

class procedure TDptFormatter.ClearTrivia(AToken: TSyntaxToken);
begin
  if Assigned(AToken) then
  begin
    AToken.LeadingTrivia.Clear;
    AToken.TrailingTrivia.Clear;
  end;
end;

class function TDptFormatter.GetLeadingTrivia(AToken: TSyntaxToken): string;
var
  LTrivia: TSyntaxTrivia;
begin
  Result := '';
  if Assigned(AToken) and Assigned(AToken.LeadingTrivia) then
  begin
    for LTrivia in AToken.LeadingTrivia do
      Result := Result + LTrivia.Text;
  end;
end;

class procedure TDptFormatter.AddLeadingTrivia(AToken: TSyntaxToken; const ATriviaText: string);
begin
  if Assigned(AToken) then
    AToken.LeadingTrivia.Add(TSyntaxTrivia.Create(ATriviaText));
end;

class procedure TDptFormatter.AddTrailingTrivia(AToken: TSyntaxToken; const ATriviaText: string);
begin
  if Assigned(AToken) then
    AToken.TrailingTrivia.Add(TSyntaxTrivia.Create(ATriviaText));
end;

end.
