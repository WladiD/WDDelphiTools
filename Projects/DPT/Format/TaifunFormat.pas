// DWScript for formatting Delphi units in the Taifun style

function StringOfChar(C: String; Count: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Count do
    Result := Result + C;
end;

function PadRight(const S: string; ALen: Integer): string;
begin
  Result := S;
  while Length(Result) < ALen do
    Result := Result + ' ';
end;

function CreateClassBanner(const AClassName: string): string;
var
  LRule: string;
begin
  LRule := '{ ' + StringOfChar('=', 71) + ' }';
  Result := LRule + #13#10 +
            '{ ' + PadRight(AClassName, 71) + ' }' + #13#10 +
            LRule + #13#10;
end;

function CreateMethodBanner: string;
begin
  Result := '{ ' + StringOfChar('-', 71) + ' }' + #13#10;
end;

procedure OnVisitUsesClause(AUses: TUsesClauseSyntax);
var
  LToken: TSyntaxToken;
begin
  LToken := GetUsesKeyword(AUses);
  if Assigned(LToken) then
  begin
    ClearTrivia(LToken);
    // AddTrailingTrivia(LToken, #13#10); 
  end;
end;

// Note: Requires AST nodes to be exposed with Identifier and other properties.
// procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax);
// begin
//   if Assigned(AClass.Identifier) then
//   begin
//     ClearTrivia(AClass.Identifier);
//     AddLeadingTrivia(AClass.Identifier, #13#10 + CreateClassBanner(AClass.Identifier.Text) + #13#10);
//   end;
// end;

procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax);
begin
end;

procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax);
begin
end;

var
  LastClassName: string;

procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
var
  LClassName: string;
  LToken: TSyntaxToken;
begin
  LClassName := GetMethodClassName(AMethod);
  LToken := GetMethodStartToken(AMethod);

  if Assigned(LToken) then
  begin
    ClearTrivia(LToken);
    
    if (LClassName <> '') and (LClassName <> LastClassName) then
    begin
      AddLeadingTrivia(LToken, #13#10#13#10 + CreateClassBanner(LClassName) + #13#10);
      LastClassName := LClassName;
    end
    else
    begin
      AddLeadingTrivia(LToken, #13#10#13#10 + CreateMethodBanner() + #13#10);
    end;
  end;
end;

function CreateSectionBanner(const AName: string): string;
begin
  Result := '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
            AName + #13#10 +
            '{ ' + StringOfChar('=', 71) + ' }' + #13#10;
end;

procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax);
var
  LToken: TSyntaxToken;
begin
  LToken := GetInterfaceKeyword(ASection);
  if Assigned(LToken) then
  begin
    ClearTrivia(LToken);
    AddLeadingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10);
    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }'); 
  end;
end;

procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax);
var
  LToken: TSyntaxToken;
begin
  LToken := GetImplementationKeyword(ASection);
  if Assigned(LToken) then
  begin
    ClearTrivia(LToken);
    AddLeadingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10);
    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }');
  end;
end;

procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
var
  LToken: TSyntaxToken;
begin
  LToken := GetFinalEndKeyword(AUnit);
  if Assigned(LToken) then
  begin
    ClearTrivia(LToken);
    AddLeadingTrivia(LToken, #13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10#13#10);
  end;
end;
