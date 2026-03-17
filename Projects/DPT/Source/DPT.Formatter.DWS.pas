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
  dwsUnitSymbols,

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
    FExec     : IdwsProgramExecution;
    FProgram  : IdwsProgram;
    FScript   : TDelphiWebScript;
    FScriptDir: string;
    FUnit     : TdwsUnit;
    
    procedure SetupScriptUnit;
    procedure CallScriptProc(const AProcName, AParamName: string; AObj: TObject);
    function HandleNeedUnit(const AUnitName: string; var AUnitSource: string): IdwsUnit;
    
    // DWScript function handlers
    procedure dwsAddLeadingTrivia(Info: TProgramInfo);
    procedure dwsAddTrailingTrivia(Info: TProgramInfo);
    procedure dwsClearTrivia(Info: TProgramInfo);
    procedure dwsGetLeadingTrivia(Info: TProgramInfo);

    // AST wrappers
    procedure dwsGetFinalEndKeyword(Info: TProgramInfo);
    procedure dwsGetImplementationKeyword(Info: TProgramInfo);
    procedure dwsGetInterfaceKeyword(Info: TProgramInfo);
    procedure dwsGetMethodClassName(Info: TProgramInfo);
    procedure dwsGetMethodDepth(Info: TProgramInfo);
    procedure dwsGetMethodHasBody(Info: TProgramInfo);
    procedure dwsGetMethodName(Info: TProgramInfo);
    procedure dwsGetMethodStartToken(Info: TProgramInfo);
    procedure dwsGetMethodEndToken(Info: TProgramInfo);
    procedure dwsGetNextToken(Info: TProgramInfo);
    procedure dwsGetUnitKeyword(Info: TProgramInfo);
    procedure dwsGetUnitName(Info: TProgramInfo);
    procedure dwsGetUnitSemicolon(Info: TProgramInfo);
    procedure dwsGetUsesFirstItemToken(Info: TProgramInfo);
    procedure dwsGetUsesKeyword(Info: TProgramInfo);
  protected
    procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax); override;
    procedure OnVisitConstSection(ASection: TConstSectionSyntax); override;
    procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax); override;
    procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax); override;
    procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax); override;
    procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax); override;
    procedure OnVisitTypeSection(ASection: TTypeSectionSyntax); override;
    procedure OnVisitUnitEnd(AUnit: TCompilationUnitSyntax); override;
    procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax); override;
    procedure OnVisitUsesClause(AUses: TUsesClauseSyntax); override;
    procedure OnVisitVarSection(ASection: TVarSectionSyntax); override;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure FormatUnit(AUnit: TCompilationUnitSyntax); override;
    procedure LoadScript(const AScriptFile: String);
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
  
  var Func: TdwsFunction := FUnit.Functions.Add('ClearTrivia');
  Func.Parameters.Add('AToken', 'TSyntaxToken');
  Func.OnEval := dwsClearTrivia;

  Func := FUnit.Functions.Add('GetLeadingTrivia');
  Func.Parameters.Add('AToken', 'TSyntaxToken');
  Func.ResultType := 'String';
  Func.OnEval := dwsGetLeadingTrivia;

  Func := FUnit.Functions.Add('AddLeadingTrivia');
  Func.Parameters.Add('AToken', 'TSyntaxToken');
  Func.Parameters.Add('ATriviaText', 'String');
  Func.OnEval := dwsAddLeadingTrivia;
  
  Func := FUnit.Functions.Add('AddTrailingTrivia');
  Func.Parameters.Add('AToken', 'TSyntaxToken');
  Func.Parameters.Add('ATriviaText', 'String');
  Func.OnEval := dwsAddTrailingTrivia;

  // AST Wrappers
  Func := FUnit.Functions.Add('GetUsesKeyword');
  Func.Parameters.Add('ANode', 'TUsesClauseSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetUsesKeyword;

  Func := FUnit.Functions.Add('GetUsesFirstItemToken');
  Func.Parameters.Add('ANode', 'TUsesClauseSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetUsesFirstItemToken;

  Func := FUnit.Functions.Add('GetInterfaceKeyword');
  Func.Parameters.Add('ANode', 'TInterfaceSectionSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetInterfaceKeyword;

  Func := FUnit.Functions.Add('GetImplementationKeyword');
  Func.Parameters.Add('ANode', 'TImplementationSectionSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetImplementationKeyword;

  Func := FUnit.Functions.Add('GetFinalEndKeyword');
  Func.Parameters.Add('ANode', 'TCompilationUnitSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetFinalEndKeyword;

  Func := FUnit.Functions.Add('GetUnitKeyword');
  Func.Parameters.Add('ANode', 'TCompilationUnitSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetUnitKeyword;

  Func := FUnit.Functions.Add('GetUnitSemicolon');
  Func.Parameters.Add('ANode', 'TCompilationUnitSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetUnitSemicolon;

  Func := FUnit.Functions.Add('GetUnitName');
  Func.Parameters.Add('ANode', 'TCompilationUnitSyntax');
  Func.ResultType := 'String';
  Func.OnEval := dwsGetUnitName;

  Func := FUnit.Functions.Add('GetMethodClassName');
  Func.Parameters.Add('ANode', 'TMethodImplementationSyntax');
  Func.ResultType := 'String';
  Func.OnEval := dwsGetMethodClassName;

  Func := FUnit.Functions.Add('GetMethodDepth');
  Func.Parameters.Add('ANode', 'TMethodImplementationSyntax');
  Func.ResultType := 'Integer';
  Func.OnEval := dwsGetMethodDepth;

  Func := FUnit.Functions.Add('GetMethodHasBody');
  Func.Parameters.Add('ANode', 'TMethodImplementationSyntax');
  Func.ResultType := 'Boolean';
  Func.OnEval := dwsGetMethodHasBody;

  Func := FUnit.Functions.Add('GetMethodName');
  Func.Parameters.Add('ANode', 'TMethodImplementationSyntax');
  Func.ResultType := 'String';
  Func.OnEval := dwsGetMethodName;

  Func := FUnit.Functions.Add('GetMethodStartToken');
  Func.Parameters.Add('ANode', 'TMethodImplementationSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetMethodStartToken;

  Func := FUnit.Functions.Add('GetMethodEndToken');
  Func.Parameters.Add('ANode', 'TMethodImplementationSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetMethodEndToken;

  Func := FUnit.Functions.Add('GetNextToken');
  Func.Parameters.Add('AToken', 'TSyntaxToken');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetNextToken;
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
  Node: TUsesClauseSyntax;
begin
  Node := TUsesClauseSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.UsesKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.UsesKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetUsesFirstItemToken(Info: TProgramInfo);
var
  Node : TUsesClauseSyntax;
  Ref  : TUnitReferenceSyntax;
  Token: TSyntaxToken;
begin
  Node := TUsesClauseSyntax(Info.ParamAsObject[0]);
  Token := nil;
  if Assigned(Node) and (Node.UnitReferences.Count > 0) then
  begin
    Ref := Node.UnitReferences[0];
    if Assigned(Ref) and (Ref.Namespaces.Count > 0) then
      Token := Ref.Namespaces[0];
  end;
  
  if Assigned(Token) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Token, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetInterfaceKeyword(Info: TProgramInfo);
var
  Node: TInterfaceSectionSyntax;
begin
  Node := TInterfaceSectionSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.InterfaceKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.InterfaceKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetImplementationKeyword(Info: TProgramInfo);
var
  Node: TImplementationSectionSyntax;
begin
  Node := TImplementationSectionSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.ImplementationKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.ImplementationKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetFinalEndKeyword(Info: TProgramInfo);
var
  Node: TCompilationUnitSyntax;
begin
  Node := TCompilationUnitSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.FinalEndKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.FinalEndKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetUnitKeyword(Info: TProgramInfo);
var
  Node: TCompilationUnitSyntax;
begin
  Node := TCompilationUnitSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.UnitKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.UnitKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetUnitSemicolon(Info: TProgramInfo);
var
  Node: TCompilationUnitSyntax;
begin
  Node := TCompilationUnitSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.Semicolon) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.Semicolon, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetUnitName(Info: TProgramInfo);
var
  Node     : TCompilationUnitSyntax;
  ResultStr: String;
begin
  Node := TCompilationUnitSyntax(Info.ParamAsObject[0]);
  ResultStr := '';
  if Assigned(Node) and Assigned(Node.Namespaces) and (Node.Namespaces.Count > 0) then
  begin
    for var I: Integer := 0 to Node.Namespaces.Count - 1 do
    begin
      ResultStr := ResultStr + Node.Namespaces[I].Text;
      if I < Node.Dots.Count then
        ResultStr := ResultStr + Node.Dots[I].Text;
    end;
  end;
  Info.ResultAsString := ResultStr;
end;

procedure TDptDwsFormatter.dwsGetMethodClassName(Info: TProgramInfo);
var
  DotIndex  : Integer;
  Node      : TMethodImplementationSyntax;
  ResultStr : String;
  TokenLower: String;
begin
  Node := TMethodImplementationSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.SignatureTokens) then
  begin
    DotIndex := -1;
    for var I: Integer := 0 to Node.SignatureTokens.Count - 1 do
    begin
      if (Node.SignatureTokens[I].Text = '(') or (Node.SignatureTokens[I].Text = ':') or (Node.SignatureTokens[I].Text = ';') then
        Break;
      if Node.SignatureTokens[I].Text = '.' then
        DotIndex := I;
    end;

    if DotIndex >= 0 then
    begin
      ResultStr := '';
      for var J: Integer := 0 to DotIndex - 1 do
      begin
        TokenLower := LowerCase(Node.SignatureTokens[J].Text);
        // Skip common method prefixes
        if (TokenLower = 'class') or (TokenLower = 'procedure') or
           (TokenLower = 'function') or (TokenLower = 'constructor') or
           (TokenLower = 'destructor') or (TokenLower = 'operator') then
          Continue;
          
        ResultStr := ResultStr + Node.SignatureTokens[J].Text;
      end;
      Info.ResultAsString := ResultStr;
      Exit;
    end;
  end;
  Info.ResultAsString := '';
end;

procedure TDptDwsFormatter.dwsGetMethodDepth(Info: TProgramInfo);
begin
  Info.ResultAsInteger := FMethodDepth;
end;

procedure TDptDwsFormatter.dwsGetMethodHasBody(Info: TProgramInfo);
var
  Node : TMethodImplementationSyntax;
begin
  Node := TMethodImplementationSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) then
  begin
    Info.ResultAsBoolean := Assigned(Node.BeginKeyword);
  end
  else
    Info.ResultAsBoolean := False;
end;

procedure TDptDwsFormatter.dwsGetMethodName(Info: TProgramInfo);
var
  DotIndex: Integer;
  EndIndex: Integer;
  Node    : TMethodImplementationSyntax;
begin
  Node := TMethodImplementationSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.SignatureTokens) then
  begin
    DotIndex := -1;
    EndIndex := Node.SignatureTokens.Count - 1;
    for var I: Integer := 0 to Node.SignatureTokens.Count - 1 do
    begin
      if (Node.SignatureTokens[I].Text = '(') or (Node.SignatureTokens[I].Text = ':') or (Node.SignatureTokens[I].Text = ';') then
      begin
        EndIndex := I - 1;
        Break;
      end;
      if Node.SignatureTokens[I].Text = '.' then
        DotIndex := I;
    end;

    if DotIndex >= 0 then
    begin
      if DotIndex < EndIndex then
        Info.ResultAsString := Node.SignatureTokens[DotIndex + 1].Text
      else
        Info.ResultAsString := '';
    end
    else
    begin
      if EndIndex >= 0 then
        Info.ResultAsString := Node.SignatureTokens[EndIndex].Text
      else if Node.SignatureTokens.Count > 0 then
        Info.ResultAsString := Node.SignatureTokens[0].Text
      else
        Info.ResultAsString := '';
    end;
  end
  else
    Info.ResultAsString := '';
end;

procedure TDptDwsFormatter.dwsGetMethodStartToken(Info: TProgramInfo);
var
  Node : TMethodImplementationSyntax;
  Token: TSyntaxToken;
begin
  Node := TMethodImplementationSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) then
  begin
    if Assigned(Node.ClassKeyword) then
      Token := Node.ClassKeyword
    else
      Token := Node.MethodTypeKeyword;

    if Assigned(Token) then
      Info.ResultAsVariant := Info.RegisterExternalObject(Token, False, False)
    else
      Info.ResultAsVariant := IUnknown(nil);
  end
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetMethodEndToken(Info: TProgramInfo);
var
  Node : TMethodImplementationSyntax;
  Token: TSyntaxToken;
begin
  Node := TMethodImplementationSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) then
  begin
    if Assigned(Node.FinalSemicolon) then
      Token := Node.FinalSemicolon
    else
      Token := Node.EndKeyword;

    if Assigned(Token) then
      Info.ResultAsVariant := Info.RegisterExternalObject(Token, False, False)
    else
      Info.ResultAsVariant := IUnknown(nil);
  end
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetNextToken(Info: TProgramInfo);
var
  Token: TSyntaxToken;
begin
  Token := TSyntaxToken(Info.ParamAsObject[0]);
  if Assigned(Token) and Assigned(Token.NextToken) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Token.NextToken, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

function TDptDwsFormatter.HandleNeedUnit(const AUnitName: string; var AUnitSource: string): IdwsUnit;
var
  LPath: string;
begin
  Result := nil;
  LPath := TPath.Combine(FScriptDir, AUnitName + '.pas');
  if not FileExists(LPath) then
    LPath := TPath.Combine(TPath.Combine(FScriptDir, 'TaifunFormat'), AUnitName + '.pas');
  if FileExists(LPath) then
    AUnitSource := TFile.ReadAllText(LPath);
end;

procedure TDptDwsFormatter.LoadScript(const AScriptFile: String);
var
  Source: String;
begin
  if not FileExists(AScriptFile) then
    raise Exception.CreateFmt('Script file not found: %s', [AScriptFile]);

  FScriptDir := ExtractFilePath(AScriptFile);
  FScript.OnNeedUnit := HandleNeedUnit;
  Source := TFile.ReadAllText(AScriptFile);
  FProgram := FScript.Compile(Source);
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
  Func: IInfo;
begin
  if not Assigned(FExec) then 
    Exit;
  
  try
    Func := FExec.Info.Func[AProcName];
  except
    on E: Exception do
      if E.Message.Contains('not found') or E.Message.Contains('Nicht gefunden') then
        Exit
      else
        raise;
  end;

  if Assigned(Func) then
    Func.Call([FExec.Info.RegisterExternalObject(AObj, False, False)]);
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
