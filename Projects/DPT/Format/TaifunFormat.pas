// DWScript for formatting Delphi units in the Taifun style

uses 

  TaifunFormat.Banner, 
  TaifunFormat.Header,
  TaifunFormat.Trivia, 
  TaifunFormat.Utils;


type
  TTaifunFormatter = class
  private
    FLastClassName: string;
    FExpectedTokenTextForSuppressedBanner: string;
    FBanner: TTaifunBannerHelper;
    FTrivia: TTaifunTriviaHelper;
    FHeader: TTaifunHeaderHelper;

  public
    constructor Create;
    procedure ClearState;

    procedure FormatUnitStart(AUnit: TCompilationUnitSyntax);
    procedure FormatUnitEnd(AUnit: TCompilationUnitSyntax);
    procedure FormatInterfaceSection(ASection: TInterfaceSectionSyntax);
    procedure FormatImplementationSection(ASection: TImplementationSectionSyntax);
    procedure FormatUsesClause(AUses: TUsesClauseSyntax);
    procedure FormatMethodImplementation(AMethod: TMethodImplementationSyntax);
    procedure InsertNestedMethodEndBanner(AMethod: TMethodImplementationSyntax);
  end;

{ TTaifunFormatter }

constructor TTaifunFormatter.Create;
begin
  FBanner := TTaifunBannerHelper.Create;
  FTrivia := TTaifunTriviaHelper.Create;
  FHeader := TTaifunHeaderHelper.Create;
  ClearState;
end;

procedure TTaifunFormatter.ClearState;
begin
  FLastClassName := '';
  FExpectedTokenTextForSuppressedBanner := '';
end;

procedure TTaifunFormatter.FormatUsesClause(AUses: TUsesClauseSyntax);
var
  LToken, LFirstItem: TSyntaxToken;
  LTrivia: string;
begin
  LToken := GetUsesKeyword(AUses);
  if Assigned(LToken) then
  begin
    LTrivia := TrimLeadingCRLFSpace(GetLeadingTrivia(LToken));
    ClearTrivia(LToken);
    AddLeadingTrivia(LToken, #13#10#13#10 + LTrivia);
    AddTrailingTrivia(LToken, '');
    
    LFirstItem := GetUsesFirstItemToken(AUses);
    if Assigned(LFirstItem) then
    begin
      LTrivia := TrimLeadingCRLFSpace(GetLeadingTrivia(LFirstItem));
      ClearTrivia(LFirstItem);
      AddLeadingTrivia(LFirstItem, #13#10#13#10 + '  ' + LTrivia);
    end;
    FExpectedTokenTextForSuppressedBanner := '';
  end;
end;

procedure TTaifunFormatter.FormatMethodImplementation(AMethod: TMethodImplementationSyntax);
var
  LClassName: string;
  LOldTrivia: string;
  LComments: string;
  LTrailingPart: string;
  LIndent: string;
  LToken: TSyntaxToken;
  LIsSuppressed: Boolean;
  LLeadingNewlines: Integer;
  LPrefix: string;
  LBannerText: string;
begin
  if not GetMethodHasBody(AMethod) then Exit;

  LClassName := GetMethodClassName(AMethod);
  LToken := GetMethodStartToken(AMethod);
  if not Assigned(LToken) then Exit;

  var LIsFirstAfterSection: Boolean;
  LIsFirstAfterSection := (FExpectedTokenTextForSuppressedBanner <> '') and (LToken.Text = FExpectedTokenTextForSuppressedBanner);
  LIsSuppressed := LIsFirstAfterSection;
  FExpectedTokenTextForSuppressedBanner := '';

  LOldTrivia := GetLeadingTrivia(LToken);
  ClearTrivia(LToken);
  FTrivia.ProcessTrivia(LOldTrivia, LClassName, FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);

  if (Pos('{ -', LComments) > 0) or (Pos('{ =', LComments) > 0) then LIsSuppressed := True;

  LPrefix := LTrailingPart + #13#10#13#10;

  if GetMethodDepth(AMethod) > 1 then
  begin
    AddLeadingTrivia(LToken, LPrefix + FBanner.CreateNestedMethodBanner() + LComments + LIndent);
    InsertNestedMethodEndBanner(AMethod);
  end
  else
  begin
    if (LClassName <> '') and (LClassName <> FLastClassName) then
    begin
      LBannerText := FBanner.CreateClassBanner(LClassName);
      FLastClassName := LClassName;
    end
    else if (LClassName = '') and ((FLastClassName <> '') or (LIsFirstAfterSection and (LTrailingPart <> ''))) then
    begin
      LBannerText := FBanner.CreateSectionBanner('');
      FLastClassName := '';
    end
    else if not LIsSuppressed then
      LBannerText := FBanner.CreateMethodBanner()
    else
      LBannerText := '';

    AddLeadingTrivia(LToken, LPrefix + LBannerText + LComments);
  end;
end;

procedure TTaifunFormatter.InsertNestedMethodEndBanner(AMethod: TMethodImplementationSyntax);
var
  LEndToken: TSyntaxToken;
  LNextToken: TSyntaxToken;
  LNextText: string;
  LNextOldTrivia: string;
  LNextComments: string;
  LNextTrailingPart: string;
  LNextIndent: string;
  LNextLeadingNewlines: Integer;
begin
  LEndToken := GetMethodEndToken(AMethod);
  if not Assigned(LEndToken) then Exit;

  LNextToken := GetNextToken(LEndToken);
  if not Assigned(LNextToken) then Exit;

  LNextText := LowerCase(LNextToken.Text);
  if (LNextText = 'procedure') or (LNextText = 'function') or (LNextText = 'constructor') or (LNextText = 'destructor') or (LNextText = 'class') then
    Exit;

  LNextOldTrivia := GetLeadingTrivia(LNextToken);
  ClearTrivia(LNextToken);
  FTrivia.ProcessTrivia(LNextOldTrivia, '', FLastClassName, LNextTrailingPart, LNextComments, LNextLeadingNewlines, LNextIndent);

  AddLeadingTrivia(LNextToken, LNextTrailingPart + #13#10#13#10 + FBanner.CreateNestedMethodBanner() + LNextComments + LNextIndent);
end;

procedure TTaifunFormatter.FormatInterfaceSection(ASection: TInterfaceSectionSyntax);
var
  LToken: TSyntaxToken;
  LNext: TSyntaxToken;
  LComments: string;
  LTrailingPart: string;
  LOldTrivia: string;
  LBanner: string;
  LPrefix: string;
  LIndent: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetInterfaceKeyword(ASection);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    FTrivia.ProcessTrivia(LOldTrivia, '', FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);
    LBanner := '{ ' + GetSep('=', 71) + ' }' + #13#10;
    
    LPrefix := #13#10; if LLeadingNewlines >= 2 then LPrefix := #13#10#13#10;
    
    if LTrailingPart <> '' then 
    begin
       if LComments <> '' then 
       begin
          while (Length(LComments) > 0) and ((LComments[Length(LComments)] = #13) or (LComments[Length(LComments)] = #10)) do Delete(LComments, Length(LComments), 1);
          AddLeadingTrivia(LToken, LTrailingPart + LPrefix + LComments + #13#10#13#10 + LBanner);
       end
       else AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LBanner);
    end
    else 
    begin
       if LComments <> '' then 
       begin
          while (Length(LComments) > 0) and ((LComments[Length(LComments)] = #13) or (LComments[Length(LComments)] = #10)) do Delete(LComments, Length(LComments), 1);
          AddLeadingTrivia(LToken, LPrefix + LComments + #13#10#13#10 + LBanner);
       end
       else AddLeadingTrivia(LToken, #13#10#13#10 + LBanner);
    end;
    
    AddTrailingTrivia(LToken, #13#10 + '{ ' + GetSep('=', 71) + ' }');
    FLastClassName := '';
    LNext := GetNextToken(LToken); if Assigned(LNext) then FExpectedTokenTextForSuppressedBanner := FBanner.StripBanners(LNext);
  end;
end;

procedure TTaifunFormatter.FormatImplementationSection(ASection: TImplementationSectionSyntax);
var
  LToken: TSyntaxToken;
  LNext: TSyntaxToken;
  LComments: string;
  LTrailingPart: string;
  LOldTrivia: string;
  LBanner: string;
  LPrefix: string;
  LIndent: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetImplementationKeyword(ASection);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    FTrivia.ProcessTrivia(LOldTrivia, '', FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);
    LBanner := '{ ' + GetSep('=', 71) + ' }' + #13#10;
    
    LPrefix := #13#10; if LLeadingNewlines >= 2 then LPrefix := #13#10#13#10;
    
    if LTrailingPart <> '' then 
    begin
       if LComments <> '' then 
       begin
          while (Length(LComments) > 0) and ((LComments[Length(LComments)] = #13) or (LComments[Length(LComments)] = #10)) do Delete(LComments, Length(LComments), 1);
          AddLeadingTrivia(LToken, LTrailingPart + LPrefix + LComments + #13#10#13#10 + LBanner);
       end
       else AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LBanner);
    end
    else 
    begin
       if LComments <> '' then 
       begin
          while (Length(LComments) > 0) and ((LComments[Length(LComments)] = #13) or (LComments[Length(LComments)] = #10)) do Delete(LComments, Length(LComments), 1);
          AddLeadingTrivia(LToken, LPrefix + LComments + #13#10#13#10 + LBanner);
       end
       else AddLeadingTrivia(LToken, #13#10#13#10 + LBanner);
    end;
    
    AddTrailingTrivia(LToken, #13#10 + '{ ' + GetSep('=', 71) + ' }');
    FLastClassName := '';
    LNext := GetNextToken(LToken); if Assigned(LNext) then FExpectedTokenTextForSuppressedBanner := FBanner.StripBanners(LNext);
  end;
end;

procedure TTaifunFormatter.FormatUnitStart(AUnit: TCompilationUnitSyntax);
var
  LToken: TSyntaxToken;
  LSemicolon: TSyntaxToken;
  LUnitName: string;
  LTrivia: string;
  LDesc: string;
  LAuthor: string;
  LDirectives: string;
  LRule: string;
  LNewBanner: string;
  LDescLine: string;
  LExtra: string;
begin
  LToken := GetUnitKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LUnitName := GetUnitName(AUnit); LTrivia := GetLeadingTrivia(LToken);
    FHeader.ExtractHeaderInfo(LTrivia, LUnitName, LDesc, LAuthor, LDirectives, LExtra);
    LRule := '// ' + GetSep('=', 70); LDescLine := '// ' + LUnitName; if LDesc <> '' then LDescLine := LDescLine + ' - ' + LDesc;
    LNewBanner := LRule + #13#10 + '//' + #13#10 + LDescLine + #13#10 + '//' + #13#10 + '// Autor: ' + LAuthor + #13#10 + '//';
    if LExtra <> '' then LNewBanner := LNewBanner + #13#10 + LExtra + #13#10 + '//';
    LNewBanner := LNewBanner + #13#10 + LRule + #13#10 + #13#10 + LDirectives + #13#10 + #13#10;
    ClearTrivia(LToken); AddLeadingTrivia(LToken, LNewBanner);
    LSemicolon := GetUnitSemicolon(AUnit); if Assigned(LSemicolon) then begin ClearTrivia(LSemicolon); AddTrailingTrivia(LSemicolon, ''); end;
  end;
end;

procedure TTaifunFormatter.FormatUnitEnd(AUnit: TCompilationUnitSyntax);
var
  LToken: TSyntaxToken;
  LComments: string;
  LTrailingPart: string;
  LOldTrivia: string;
  LIndent: string;
  LLeadingNewlines: Integer;
begin
  LToken := GetFinalEndKeyword(AUnit);
  if Assigned(LToken) then
  begin
    LOldTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    FTrivia.ProcessTrivia(LOldTrivia, '', FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);

    if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LComments + '{ ' + GetSep('=', 71) + ' }' + #13#10#13#10)
    else AddLeadingTrivia(LToken, #13#10#13#10 + LComments + '{ ' + GetSep('=', 71) + ' }' + #13#10#13#10);
  end;
end;

// -----------------------------------------------------------------------
// Global Instance and Entry Points
// -----------------------------------------------------------------------

var GlobalFormatter: TTaifunFormatter;

function GetFormatter: TTaifunFormatter;
begin
  if not Assigned(GlobalFormatter) then
    GlobalFormatter := TTaifunFormatter.Create;
  Result := GlobalFormatter;
end;

procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax);
begin
  GetFormatter.ClearState;
  GetFormatter.FormatUnitStart(AUnit);
end;

procedure OnVisitUsesClause(AUses: TUsesClauseSyntax);
begin
  GetFormatter.FormatUsesClause(AUses);
end;

procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax);
begin
  GetFormatter.FormatInterfaceSection(ASection);
end;

procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax);
begin
  GetFormatter.FormatImplementationSection(ASection);
end;

procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
begin
  GetFormatter.FormatMethodImplementation(AMethod);
end;

procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
begin
  GetFormatter.FormatUnitEnd(AUnit);
end;
