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
    procedure OnVisitUsesClause(AUses: TUsesClauseSyntax); virtual;
    procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax); virtual;
    procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax); virtual;
    procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax); virtual;

  public
    // Overrides from TParseTreeVisitor
    procedure VisitUsesClause(ANode: TUsesClauseSyntax); override;
    procedure VisitClassDeclaration(ANode: TClassDeclarationSyntax); override;
    procedure VisitRecordDeclaration(ANode: TRecordDeclarationSyntax); override;
    procedure VisitMethodImplementation(ANode: TMethodImplementationSyntax); override;

    // Starts the formatting and traversal
    procedure FormatUnit(AUnit: TCompilationUnitSyntax); virtual;
    
    // Helper methods for token/trivia manipulation
    class procedure ClearTrivia(AToken: TSyntaxToken);
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
  inherited;
  OnVisitClassDeclaration(ANode);
end;

procedure TDptFormatter.VisitMethodImplementation(ANode: TMethodImplementationSyntax);
begin
  inherited;
  OnVisitMethodImplementation(ANode);
end;

procedure TDptFormatter.VisitRecordDeclaration(ANode: TRecordDeclarationSyntax);
begin
  inherited;
  OnVisitRecordDeclaration(ANode);
end;

procedure TDptFormatter.VisitUsesClause(ANode: TUsesClauseSyntax);
begin
  inherited;
  OnVisitUsesClause(ANode);
end;

class procedure TDptFormatter.ClearTrivia(AToken: TSyntaxToken);
begin
  if Assigned(AToken) then
  begin
    AToken.LeadingTrivia.Clear;
    AToken.TrailingTrivia.Clear;
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
