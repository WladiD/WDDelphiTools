// DWScript for formatting Delphi units in the Taifun style

uses TaifunFormat.Utils, TaifunFormat.Banner, TaifunFormat.Trivia, TaifunFormat.Header;

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
    LTrivia := GetLeadingTrivia(LToken);
    ClearTrivia(LToken);
    
    while (Length(LTrivia) > 0) and ((LTrivia[1] = #13) or (LTrivia[1] = #10) or (LTrivia[1] = ' ')) do Delete(LTrivia, 1, 1);
    AddLeadingTrivia(LToken, #13#10#13#10 + LTrivia);
    AddTrailingTrivia(LToken, '');
    
    LFirstItem := GetUsesFirstItemToken(AUses);
    if Assigned(LFirstItem) then
    begin
      LTrivia := GetLeadingTrivia(LFirstItem);
      ClearTrivia(LFirstItem);
      while (Length(LTrivia) > 0) and ((LTrivia[1] = #13) or (LTrivia[1] = #10) or (LTrivia[1] = ' ')) do Delete(LTrivia, 1, 1);
      AddLeadingTrivia(LFirstItem, #13#10#13#10 + '  ' + LTrivia);
    end;
    FExpectedTokenTextForSuppressedBanner := '';
  end;
end;

procedure TTaifunFormatter.FormatMethodImplementation(AMethod: TMethodImplementationSyntax);
var
  LClassName, LOldTrivia, LComments, LTrailingPart, LIndent: string;
  LToken: TSyntaxToken;
  LIsSuppressed: Boolean;
  LLeadingNewlines: Integer;
begin
  if not GetMethodHasBody(AMethod) then Exit;

  LClassName := GetMethodClassName(AMethod);
  LToken := GetMethodStartToken(AMethod);
  if not Assigned(LToken) then Exit;

  LIsSuppressed := (FExpectedTokenTextForSuppressedBanner <> '') and (LToken.Text = FExpectedTokenTextForSuppressedBanner);
  FExpectedTokenTextForSuppressedBanner := ''; 

  LOldTrivia := GetLeadingTrivia(LToken);
  ClearTrivia(LToken);

  FTrivia.ProcessTrivia(LOldTrivia, LClassName, FLastClassName, LTrailingPart, LComments, LLeadingNewlines, LIndent);

  if (Pos('{ -', LComments) > 0) or (Pos('{ =', LComments) > 0) then LIsSuppressed := True;

  if GetMethodDepth(AMethod) > 1 then
  begin
    if LIsSuppressed then
    begin
       if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + FBanner.CreateNestedMethodBanner() + LComments + LIndent)
       else AddLeadingTrivia(LToken, #13#10#13#10 + FBanner.CreateNestedMethodBanner() + LComments + LIndent);
    end
    else
    begin
       if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + FBanner.CreateNestedMethodBanner() + LComments + LIndent)
       else AddLeadingTrivia(LToken, #13#10#13#10 + FBanner.CreateNestedMethodBanner() + LComments + LIndent);
    end;

    var LEndToken: TSyntaxToken := GetMethodEndToken(AMethod);
    if Assigned(LEndToken) then
    begin
      var LNextToken: TSyntaxToken := GetNextToken(LEndToken);
      var LNextText: string := '';
      if Assigned(LNextToken) then LNextText := LowerCase(LNextToken.Text);
      if (LNextText <> 'procedure') and (LNextText <> 'function') and (LNextText <> 'constructor') and (LNextText <> 'destructor') and (LNextText <> 'class') then
      begin
        if Assigned(LNextToken) then
        begin
          var LNextOldTrivia: string := GetLeadingTrivia(LNextToken);
          ClearTrivia(LNextToken);
          var LNextComments, LNextTrailingPart, LNextIndent: string;
          var LNextLeadingNewlines: Integer;
          FTrivia.ProcessTrivia(LNextOldTrivia, '', FLastClassName, LNextTrailingPart, LNextComments, LNextLeadingNewlines, LNextIndent);
          
          if LNextTrailingPart <> '' then AddLeadingTrivia(LNextToken, LNextTrailingPart + #13#10#13#10 + FBanner.CreateNestedMethodBanner() + LNextComments + LNextIndent)
          else AddLeadingTrivia(LNextToken, #13#10#13#10 + FBanner.CreateNestedMethodBanner() + LNextComments + LNextIndent);
        end;
      end;
    end;
  end
  else
  begin
    if (LClassName <> '') and (LClassName <> FLastClassName) then
    begin
       if LIsSuppressed then 
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + FBanner.CreateClassBanner(LClassName) + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + FBanner.CreateClassBanner(LClassName) + LComments);
       end
       else
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + FBanner.CreateClassBanner(LClassName) + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + FBanner.CreateClassBanner(LClassName) + LComments);
       end;
       FLastClassName := LClassName;
    end
    else if (LClassName = '') and (FLastClassName <> '') then
    begin
       FLastClassName := '';
       if LIsSuppressed then
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + FBanner.CreateSectionBanner('') + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + FBanner.CreateSectionBanner('') + LComments);
       end
       else
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + FBanner.CreateSectionBanner('') + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + FBanner.CreateSectionBanner('') + LComments);
       end;
    end
    else
    begin
       if not LIsSuppressed then
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + FBanner.CreateMethodBanner() + LComments)
          else AddLeadingTrivia(LToken, #13#10#13#10 + FBanner.CreateMethodBanner() + LComments);
       end
       else
       begin
          if LTrailingPart <> '' then AddLeadingTrivia(LToken, LTrailingPart + #13#10#13#10 + LComments)
          else
          begin
             if LComments <> '' then AddLeadingTrivia(LToken, #13#10#13#10 + LComments)
             else AddLeadingTrivia(LToken, #13#10#13#10);
          end;
       end;
    end;
  end;
end;

procedure TTaifunFormatter.FormatInterfaceSection(ASection: TInterfaceSectionSyntax);
var LToken, LNext: TSyntaxToken; LComments, LTrailingPart, LOldTrivia, LBanner, LPrefix, LIndent: string;
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
var LToken, LNext: TSyntaxToken; LComments, LTrailingPart, LOldTrivia, LBanner, LPrefix, LIndent: string;
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
var LToken, LSemicolon: TSyntaxToken; LUnitName, LTrivia, LDesc, LAuthor, LDirectives, LRule, LNewBanner, LDescLine, LExtra: string;
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
var LToken: TSyntaxToken; LComments, LTrailingPart, LOldTrivia, LIndent: string;
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
