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
  LToken, LFirstItem: TSyntaxToken;
begin
  LToken := GetUsesKeyword(AUses);
  if Assigned(LToken) then
  begin
    ClearTrivia(LToken);
    AddLeadingTrivia(LToken, #13#10);
    AddTrailingTrivia(LToken, #13#10);
    
    LFirstItem := GetUsesFirstItemToken(AUses);
    if Assigned(LFirstItem) then
    begin
      ClearTrivia(LFirstItem);
      // Ensure one empty line after uses keyword
      AddLeadingTrivia(LFirstItem, #13#10 + '  ');
    end;
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
    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10);
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
    AddTrailingTrivia(LToken, #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10);
  end;
end;

function ExtractHeaderInfo(const ATrivia: string; const AUnitName: string; var ADescription: string; var AAuthor: string; var AInclude: string): Boolean;
var
  S, LLine: string;
  P, P2: Integer;
begin
  ADescription := 'Kurzbeschreibung der Unit';
  AAuthor := 'Name';
  AInclude := '{$I Tfw.Define.pas}';
  Result := Length(ATrivia) > 0;

  if not Result then Exit;

  S := ATrivia;
  while Length(S) > 0 do
  begin
    P := Pos(#10, S);
    if P > 0 then
    begin
      LLine := Copy(S, 1, P - 1);
      if (Length(LLine) > 0) and (LLine[Length(LLine)] = #13) then
        LLine := Copy(LLine, 1, Length(LLine) - 1);
      Delete(S, 1, P);
    end
    else
    begin
      LLine := S;
      S := '';
    end;

    // Check Include
    if Pos('{$I ', LLine) > 0 then
    begin
      AInclude := LLine;
    end;

    // Check Author
    P2 := Pos('Autor:', LLine);
    if P2 > 0 then
    begin
      AAuthor := Copy(LLine, P2 + 6, Length(LLine));
      // Trim spaces
      while (Length(AAuthor) > 0) and (AAuthor[1] = ' ') do Delete(AAuthor, 1, 1);
      while (Length(AAuthor) > 0) and (AAuthor[Length(AAuthor)] = ' ') do Delete(AAuthor, Length(AAuthor), 1);
    end;

    // Check Description
    P2 := Pos('// ' + AUnitName + ' - ', LLine);
    if P2 = 1 then
    begin
      ADescription := Copy(LLine, Length('// ' + AUnitName + ' - ') + 1, Length(LLine));
      // Trim spaces
      while (Length(ADescription) > 0) and (ADescription[1] = ' ') do Delete(ADescription, 1, 1);
      while (Length(ADescription) > 0) and (ADescription[Length(ADescription)] = ' ') do Delete(ADescription, Length(ADescription), 1);
    end;
  end;
end;

procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax);
var
  LToken, LSemicolon: TSyntaxToken;
  LUnitName: string;
  LTrivia: string;
  LDesc, LAuthor, LInclude: string;
  LRule: string;
  LNewBanner: string;
begin
  LToken := GetUnitKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LUnitName := GetUnitName(AUnit);
    LTrivia := GetLeadingTrivia(LToken);

    ExtractHeaderInfo(LTrivia, LUnitName, LDesc, LAuthor, LInclude);

    LRule := '// ' + StringOfChar('=', 70);
    LNewBanner := LRule + #13#10 +
                  '//' + #13#10 +
                  '// ' + LUnitName + ' - ' + LDesc + #13#10 +
                  '//' + #13#10 +
                  '// Autor: ' + LAuthor + #13#10 +
                  '//' + #13#10 +
                  LRule + #13#10 +
                  #13#10 +
                  LInclude + #13#10 +
                  #13#10;

    if LTrivia <> LNewBanner then
    begin
      ClearTrivia(LToken);
      AddLeadingTrivia(LToken, LNewBanner);
    end;

    // Ensure exactly one empty line after the unit statement (after the semicolon)
    LSemicolon := GetUnitSemicolon(AUnit);
    if Assigned(LSemicolon) then
    begin
      ClearTrivia(LSemicolon);
      AddTrailingTrivia(LSemicolon, #13#10);
    end;
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
