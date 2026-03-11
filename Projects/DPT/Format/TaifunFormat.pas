// DWScript for formatting Delphi units in the Taifun style

procedure OnVisitUsesClause(AUses: TUsesClauseSyntax);
begin
  // Example rule: add a space after 'uses', and maybe a banner
  ClearTrivia(GetUsesKeyword(AUses));
  AddLeadingTrivia(GetUsesKeyword(AUses), #13#10 + '// FORMATTED BY DPT' + #13#10);
  AddTrailingTrivia(GetUsesKeyword(AUses), ' ');
end;

procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax);
begin
  // Rules for class declarations
end;

procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax);
begin
  // Rules for record declarations 
end;

procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
begin
  // Rules for method bodies
end;
