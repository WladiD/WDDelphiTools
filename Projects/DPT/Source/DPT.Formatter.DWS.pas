unit DPT.Formatter.DWS;

interface

uses

  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.SysUtils,
  System.Variants,
  
  dwsComp,
  dwsErrors,
  dwsExprs,
  dwsInfo,
  dwsRTTIExposer,

  ParseTree.Core,
  ParseTree.Nodes,
  
  DPT.Formatter;

type
  /// <summary>
  ///   Formatter that loads a DWScript file and delegates formatting tasks
  ///   to script functions.
  /// </summary>
  TDptDwsFormatter = class(TDptFormatter)
  private
    FExec: IdwsProgramExecution;
    FProgram: IdwsProgram;
    FScript: TDelphiWebScript;
    FUnit: TdwsUnit;
    
    procedure SetupScriptUnit;
    procedure CallScriptProc(const AProcName, AParamName: string; AObj: TObject);
    
    // DWScript function handlers
    procedure dwsClearTrivia(Info: TProgramInfo);
    procedure dwsGetLeadingTrivia(Info: TProgramInfo);
    procedure dwsAddLeadingTrivia(Info: TProgramInfo);
    procedure dwsAddTrailingTrivia(Info: TProgramInfo);
    
    // AST wrappers
    procedure dwsGetUsesKeyword(Info: TProgramInfo);
    procedure dwsGetUsesFirstItemToken(Info: TProgramInfo);
    procedure dwsGetInterfaceKeyword(Info: TProgramInfo);
    procedure dwsGetImplementationKeyword(Info: TProgramInfo);
    procedure dwsGetFinalEndKeyword(Info: TProgramInfo);
    procedure dwsGetUnitKeyword(Info: TProgramInfo);
    procedure dwsGetUnitSemicolon(Info: TProgramInfo);
    procedure dwsGetUnitName(Info: TProgramInfo);
    procedure dwsGetMethodClassName(Info: TProgramInfo);
    procedure dwsGetMethodName(Info: TProgramInfo);
    procedure dwsGetMethodStartToken(Info: TProgramInfo);
    procedure dwsGetNextToken(Info: TProgramInfo);
  protected
    procedure OnVisitUsesClause(AUses: TUsesClauseSyntax); override;
    procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax); override;
    procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax); override;
    procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax); override;
    procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax); override;
    procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax); override;
    procedure OnVisitTypeSection(ASection: TTypeSectionSyntax); override;
    procedure OnVisitConstSection(ASection: TConstSectionSyntax); override;
    procedure OnVisitVarSection(ASection: TVarSectionSyntax); override;
    procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax); override;
    procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax); override;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure FormatUnit(AUnit: TCompilationUnitSyntax); override;
    
    procedure LoadScript(const AScriptFile: string);
  end;

implementation

{ TDptDwsFormatter }

constructor TDptDwsFormatter.Create;
begin
  inherited Create;
  FScript := TDelphiWebScript.Create(nil);
  FUnit := TdwsUnit.Create(nil);
  FUnit.UnitName := 'DptFormatterAPI';
  FUnit.Script := FScript;
  SetupScriptUnit;
end;

destructor TDptDwsFormatter.Destroy;
begin
  FExec := nil;
  FProgram := nil;
  FUnit.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TDptDwsFormatter.SetupScriptUnit;
begin
  // Base elements first
  FUnit.ExposeRTTI(TypeInfo(TSyntaxToken), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TSyntaxTrivia), [eoExposePublic, eoNoFreeOnCleanup]);
  
  // Expose ParseTree Nodes to DWScript via RTTI
  FUnit.ExposeRTTI(TypeInfo(TUsesClauseSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TUnitReferenceSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TCompilationUnitSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TClassDeclarationSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TRecordDeclarationSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TVisibilitySectionSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TClassMemberSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TMethodImplementationSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TInterfaceSectionSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TImplementationSectionSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TTypeSectionSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TConstSectionSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  FUnit.ExposeRTTI(TypeInfo(TVarSectionSyntax), [eoExposePublic, eoNoFreeOnCleanup]);
  
  var LFunc := FUnit.Functions.Add('ClearTrivia');
  LFunc.Parameters.Add('AToken', 'TSyntaxToken');
  LFunc.OnEval := dwsClearTrivia;

  LFunc := FUnit.Functions.Add('GetLeadingTrivia');
  LFunc.Parameters.Add('AToken', 'TSyntaxToken');
  LFunc.ResultType := 'String';
  LFunc.OnEval := dwsGetLeadingTrivia;

  LFunc := FUnit.Functions.Add('AddLeadingTrivia');
  LFunc.Parameters.Add('AToken', 'TSyntaxToken');
  LFunc.Parameters.Add('ATriviaText', 'String');
  LFunc.OnEval := dwsAddLeadingTrivia;
  
  LFunc := FUnit.Functions.Add('AddTrailingTrivia');
  LFunc.Parameters.Add('AToken', 'TSyntaxToken');
  LFunc.Parameters.Add('ATriviaText', 'String');
  LFunc.OnEval := dwsAddTrailingTrivia;

  // AST Wrappers
  LFunc := FUnit.Functions.Add('GetUsesKeyword');
  LFunc.Parameters.Add('ANode', 'TUsesClauseSyntax');
  LFunc.ResultType := 'TSyntaxToken';
  LFunc.OnEval := dwsGetUsesKeyword;

  LFunc := FUnit.Functions.Add('GetUsesFirstItemToken');
  LFunc.Parameters.Add('ANode', 'TUsesClauseSyntax');
  LFunc.ResultType := 'TSyntaxToken';
  LFunc.OnEval := dwsGetUsesFirstItemToken;

  LFunc := FUnit.Functions.Add('GetInterfaceKeyword');
  LFunc.Parameters.Add('ANode', 'TInterfaceSectionSyntax');
  LFunc.ResultType := 'TSyntaxToken';
  LFunc.OnEval := dwsGetInterfaceKeyword;

  LFunc := FUnit.Functions.Add('GetImplementationKeyword');
  LFunc.Parameters.Add('ANode', 'TImplementationSectionSyntax');
  LFunc.ResultType := 'TSyntaxToken';
  LFunc.OnEval := dwsGetImplementationKeyword;

  LFunc := FUnit.Functions.Add('GetFinalEndKeyword');
  LFunc.Parameters.Add('ANode', 'TCompilationUnitSyntax');
  LFunc.ResultType := 'TSyntaxToken';
  LFunc.OnEval := dwsGetFinalEndKeyword;

  LFunc := FUnit.Functions.Add('GetUnitKeyword');
  LFunc.Parameters.Add('ANode', 'TCompilationUnitSyntax');
  LFunc.ResultType := 'TSyntaxToken';
  LFunc.OnEval := dwsGetUnitKeyword;

  LFunc := FUnit.Functions.Add('GetUnitSemicolon');
  LFunc.Parameters.Add('ANode', 'TCompilationUnitSyntax');
  LFunc.ResultType := 'TSyntaxToken';
  LFunc.OnEval := dwsGetUnitSemicolon;

  LFunc := FUnit.Functions.Add('GetUnitName');
  LFunc.Parameters.Add('ANode', 'TCompilationUnitSyntax');
  LFunc.ResultType := 'String';
  LFunc.OnEval := dwsGetUnitName;

  LFunc := FUnit.Functions.Add('GetMethodClassName');
  LFunc.Parameters.Add('ANode', 'TMethodImplementationSyntax');
  LFunc.ResultType := 'String';
  LFunc.OnEval := dwsGetMethodClassName;

  LFunc := FUnit.Functions.Add('GetMethodName');
  LFunc.Parameters.Add('ANode', 'TMethodImplementationSyntax');
  LFunc.ResultType := 'String';
  LFunc.OnEval := dwsGetMethodName;

  LFunc := FUnit.Functions.Add('GetMethodStartToken');
  LFunc.Parameters.Add('ANode', 'TMethodImplementationSyntax');
  LFunc.ResultType := 'TSyntaxToken';
  LFunc.OnEval := dwsGetMethodStartToken;

  LFunc := FUnit.Functions.Add('GetNextToken');
  LFunc.Parameters.Add('AToken', 'TSyntaxToken');
  LFunc.ResultType := 'TSyntaxToken';
  LFunc.OnEval := dwsGetNextToken;
end;

procedure TDptDwsFormatter.dwsClearTrivia(Info: TProgramInfo);
begin
  TDptFormatter.ClearTrivia(TSyntaxToken(Info.ParamAsObject[0]));
end;

procedure TDptDwsFormatter.dwsGetLeadingTrivia(Info: TProgramInfo);
begin
  Info.ResultAsString := TDptFormatter.GetLeadingTrivia(TSyntaxToken(Info.ParamAsObject[0]));
end;

procedure TDptDwsFormatter.dwsAddLeadingTrivia(Info: TProgramInfo);
begin
  TDptFormatter.AddLeadingTrivia(TSyntaxToken(Info.ParamAsObject[0]), Info.ParamAsString[1]);
end;

procedure TDptDwsFormatter.dwsAddTrailingTrivia(Info: TProgramInfo);
begin
  TDptFormatter.AddTrailingTrivia(TSyntaxToken(Info.ParamAsObject[0]), Info.ParamAsString[1]);
end;

procedure TDptDwsFormatter.dwsGetUsesKeyword(Info: TProgramInfo);
var
  LNode: TUsesClauseSyntax;
begin
  LNode := TUsesClauseSyntax(Info.ParamAsObject[0]);
  if Assigned(LNode) and Assigned(LNode.UsesKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(LNode.UsesKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetUsesFirstItemToken(Info: TProgramInfo);
var
  LNode: TUsesClauseSyntax;
  LRef: TUnitReferenceSyntax;
  LToken: TSyntaxToken;
begin
  LNode := TUsesClauseSyntax(Info.ParamAsObject[0]);
  LToken := nil;
  if Assigned(LNode) and (LNode.UnitReferences.Count > 0) then
  begin
    LRef := LNode.UnitReferences.List[0];
    if Assigned(LRef) and (LRef.Namespaces.Count > 0) then
      LToken := LRef.Namespaces.List[0];
  end;
  
  if Assigned(LToken) then
    Info.ResultAsVariant := Info.RegisterExternalObject(LToken, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetInterfaceKeyword(Info: TProgramInfo);
var
  LNode: TInterfaceSectionSyntax;
begin
  LNode := TInterfaceSectionSyntax(Info.ParamAsObject[0]);
  if Assigned(LNode) and Assigned(LNode.InterfaceKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(LNode.InterfaceKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetImplementationKeyword(Info: TProgramInfo);
var
  LNode: TImplementationSectionSyntax;
begin
  LNode := TImplementationSectionSyntax(Info.ParamAsObject[0]);
  if Assigned(LNode) and Assigned(LNode.ImplementationKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(LNode.ImplementationKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetFinalEndKeyword(Info: TProgramInfo);
var
  LNode: TCompilationUnitSyntax;
begin
  LNode := TCompilationUnitSyntax(Info.ParamAsObject[0]);
  if Assigned(LNode) and Assigned(LNode.FinalEndKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(LNode.FinalEndKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetUnitKeyword(Info: TProgramInfo);
var
  LNode: TCompilationUnitSyntax;
begin
  LNode := TCompilationUnitSyntax(Info.ParamAsObject[0]);
  if Assigned(LNode) and Assigned(LNode.UnitKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(LNode.UnitKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetUnitSemicolon(Info: TProgramInfo);
var
  LNode: TCompilationUnitSyntax;
begin
  LNode := TCompilationUnitSyntax(Info.ParamAsObject[0]);
  if Assigned(LNode) and Assigned(LNode.Semicolon) then
    Info.ResultAsVariant := Info.RegisterExternalObject(LNode.Semicolon, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetUnitName(Info: TProgramInfo);
var
  LNode: TCompilationUnitSyntax;
  I: Integer;
  LResultStr: string;
begin
  LNode := TCompilationUnitSyntax(Info.ParamAsObject[0]);
  LResultStr := '';
  if Assigned(LNode) and Assigned(LNode.Namespaces) and (LNode.Namespaces.Count > 0) then
  begin
    for I := 0 to LNode.Namespaces.Count - 1 do
    begin
      LResultStr := LResultStr + LNode.Namespaces.List[I].Text;
      if I < LNode.Dots.Count then
        LResultStr := LResultStr + LNode.Dots.List[I].Text;
    end;
  end;
  Info.ResultAsString := LResultStr;
end;

procedure TDptDwsFormatter.dwsGetMethodClassName(Info: TProgramInfo);
var
  LNode: TMethodImplementationSyntax;
  I, J, LDotIndex: Integer;
  LResultStr, LTokenLower: string;
begin
  LNode := TMethodImplementationSyntax(Info.ParamAsObject[0]);
  if Assigned(LNode) and Assigned(LNode.SignatureTokens) then
  begin
    LDotIndex := -1;
    for I := 0 to LNode.SignatureTokens.Count - 1 do
    begin
      if (LNode.SignatureTokens[I].Text = '(') or (LNode.SignatureTokens[I].Text = ':') or (LNode.SignatureTokens[I].Text = ';') then
        Break;
      if LNode.SignatureTokens[I].Text = '.' then
        LDotIndex := I;
    end;

    if LDotIndex >= 0 then
    begin
      LResultStr := '';
      for J := 0 to LDotIndex - 1 do
      begin
        LTokenLower := LowerCase(LNode.SignatureTokens[J].Text);
        if (LTokenLower = 'class') or (LTokenLower = 'procedure') or 
           (LTokenLower = 'function') or (LTokenLower = 'constructor') or 
           (LTokenLower = 'destructor') or (LTokenLower = 'operator') then
          Continue;
          
        LResultStr := LResultStr + LNode.SignatureTokens[J].Text;
      end;
      Info.ResultAsString := LResultStr;
      Exit;
    end;
  end;
  Info.ResultAsString := '';
end;

procedure TDptDwsFormatter.dwsGetMethodName(Info: TProgramInfo);
var
  LNode: TMethodImplementationSyntax;
  I, LDotIndex, LEndIndex: Integer;
begin
  LNode := TMethodImplementationSyntax(Info.ParamAsObject[0]);
  if Assigned(LNode) and Assigned(LNode.SignatureTokens) then
  begin
    LDotIndex := -1;
    LEndIndex := LNode.SignatureTokens.Count - 1;
    for I := 0 to LNode.SignatureTokens.Count - 1 do
    begin
      if (LNode.SignatureTokens[I].Text = '(') or (LNode.SignatureTokens[I].Text = ':') or (LNode.SignatureTokens[I].Text = ';') then
      begin
        LEndIndex := I - 1;
        Break;
      end;
      if LNode.SignatureTokens[I].Text = '.' then
        LDotIndex := I;
    end;

    if LDotIndex >= 0 then
    begin
      if LDotIndex < LEndIndex then
        Info.ResultAsString := LNode.SignatureTokens[LDotIndex + 1].Text
      else
        Info.ResultAsString := '';
    end
    else
    begin
      if LEndIndex >= 0 then
        Info.ResultAsString := LNode.SignatureTokens[LEndIndex].Text
      else if LNode.SignatureTokens.Count > 0 then
        Info.ResultAsString := LNode.SignatureTokens[0].Text
      else
        Info.ResultAsString := '';
    end;
  end
  else
    Info.ResultAsString := '';
end;

procedure TDptDwsFormatter.dwsGetMethodStartToken(Info: TProgramInfo);
var
  LNode: TMethodImplementationSyntax;
  LToken: TSyntaxToken;
begin
  LNode := TMethodImplementationSyntax(Info.ParamAsObject[0]);
  if Assigned(LNode) then
  begin
    if Assigned(LNode.ClassKeyword) then
      LToken := LNode.ClassKeyword
    else
      LToken := LNode.MethodTypeKeyword;
      
    if Assigned(LToken) then
      Info.ResultAsVariant := Info.RegisterExternalObject(LToken, False, False)
    else
      Info.ResultAsVariant := IUnknown(nil);
  end
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetNextToken(Info: TProgramInfo);
var
  LToken: TSyntaxToken;
begin
  LToken := TSyntaxToken(Info.ParamAsObject[0]);
  if Assigned(LToken) and Assigned(LToken.NextToken) then
    Info.ResultAsVariant := Info.RegisterExternalObject(LToken.NextToken, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.LoadScript(const AScriptFile: string);
var
  LSource: string;
begin
  if not FileExists(AScriptFile) then
    raise Exception.CreateFmt('Script file not found: %s', [AScriptFile]);
    
  LSource := TFile.ReadAllText(AScriptFile);
  
  FProgram := FScript.Compile(LSource);
  if FProgram.Msgs.HasErrors then
    raise Exception.Create('Script compilation failed: ' + FProgram.Msgs.AsInfo);
end;

procedure TDptDwsFormatter.FormatUnit(AUnit: TCompilationUnitSyntax);
begin
  if not Assigned(FProgram) then
    raise Exception.Create('DWScript program is not loaded.');

  FExec := FProgram.BeginNewExecution;
  FExec.BeginProgram;
  try
    inherited FormatUnit(AUnit);
  finally
    FExec.EndProgram;
    FExec := nil;
  end;
end;

procedure TDptDwsFormatter.CallScriptProc(const AProcName, AParamName: string; AObj: TObject);
var
  LFunc: IInfo;
begin
  if not Assigned(FExec) then 
    Exit;
  
  try
    LFunc := FExec.Info.Func[AProcName];
  except
    on E: Exception do
      if E.Message.Contains('not found') or E.Message.Contains('Nicht gefunden') then
        Exit
      else
        raise;
  end;

  if Assigned(LFunc) then
    LFunc.Call([FExec.Info.RegisterExternalObject(AObj, False, False)]);
end;

procedure TDptDwsFormatter.OnVisitUsesClause(AUses: TUsesClauseSyntax);
begin
  inherited;
  CallScriptProc('OnVisitUsesClause', 'AUses', AUses);
end;

procedure TDptDwsFormatter.OnVisitClassDeclaration(AClass: TClassDeclarationSyntax);
begin
  inherited;
  CallScriptProc('OnVisitClassDeclaration', 'AClass', AClass);
end;

procedure TDptDwsFormatter.OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax);
begin
  inherited;
  CallScriptProc('OnVisitRecordDeclaration', 'ARecord', ARecord);
end;

procedure TDptDwsFormatter.OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax);
begin
  inherited;
  CallScriptProc('OnVisitMethodImplementation', 'AMethod', AMethod);
end;

procedure TDptDwsFormatter.OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax);
begin
  inherited;
  CallScriptProc('OnVisitInterfaceSection', 'ASection', ASection);
end;

procedure TDptDwsFormatter.OnVisitImplementationSection(ASection: TImplementationSectionSyntax);
begin
  inherited;
  CallScriptProc('OnVisitImplementationSection', 'ASection', ASection);
end;

procedure TDptDwsFormatter.OnVisitTypeSection(ASection: TTypeSectionSyntax);
begin
  inherited;
  CallScriptProc('OnVisitTypeSection', 'ASection', ASection);
end;

procedure TDptDwsFormatter.OnVisitConstSection(ASection: TConstSectionSyntax);
begin
  inherited;
  CallScriptProc('OnVisitConstSection', 'ASection', ASection);
end;

procedure TDptDwsFormatter.OnVisitVarSection(ASection: TVarSectionSyntax);
begin
  inherited;
  CallScriptProc('OnVisitVarSection', 'ASection', ASection);
end;

procedure TDptDwsFormatter.OnVisitUnitStart(AUnit: TCompilationUnitSyntax);
begin
  inherited;
  CallScriptProc('OnVisitUnitStart', 'AUnit', AUnit);
end;

procedure TDptDwsFormatter.OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
begin
  inherited;
  CallScriptProc('OnVisitUnitEnd', 'AUnit', AUnit);
end;

end.
