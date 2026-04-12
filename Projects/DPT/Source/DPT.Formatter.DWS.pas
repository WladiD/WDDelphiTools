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
  ParseTree.Tokens,
  
  DPT.Formatter;

type

  /// <summary>
  ///   Formatter that loads a DWScript file and delegates formatting tasks
  ///   to script functions.
  /// </summary>
  TDptDwsFormatter = class(TDptFormatter)
  private
    FAvailableProcs: TDictionary<string, Boolean>;
    FExec          : IdwsProgramExecution;
    FProgram       : IdwsProgram;
    FProcsCached   : Boolean;
    FScript        : TDelphiWebScript;
    FScriptDir     : string;
    FUnit          : TdwsUnit;
    
    procedure SetupScriptUnit;
    procedure CacheAvailableProcs;
    procedure CallScriptProc(const AProcName, AParamName: string; AObj: TObject);
    function HandleNeedUnit(const AUnitName: string; var AUnitSource: string): IdwsUnit;
    
    // DWScript function handlers
    procedure dwsAddLeadingTrivia(Info: TProgramInfo);
    procedure dwsAddTrailingTrivia(Info: TProgramInfo);
    procedure dwsClearTrivia(Info: TProgramInfo);
    procedure dwsGetLeadingTrivia(Info: TProgramInfo);

    // AST wrappers
    procedure dwsGetConstKeyword(Info: TProgramInfo);
    procedure dwsGetTypeKeyword(Info: TProgramInfo);
    procedure dwsGetVarKeyword(Info: TProgramInfo);
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
    procedure dwsGetUsesItemCount(Info: TProgramInfo);
    procedure dwsGetUsesItemName(Info: TProgramInfo);
    procedure dwsGetUsesItemToken(Info: TProgramInfo);
    procedure dwsGetUsesKeyword(Info: TProgramInfo);
    procedure dwsGetUsesSemicolon(Info: TProgramInfo);
    procedure dwsIsUnitLevel(Info: TProgramInfo);
    procedure dwsReorderUsesItems(Info: TProgramInfo);
    procedure dwsUsesClauseCanBeSorted(Info: TProgramInfo);

    // Var declaration helpers
    procedure dwsGetVarDeclCount(Info: TProgramInfo);
    procedure dwsGetVarDeclName(Info: TProgramInfo);
    procedure dwsGetVarDeclIdentifier(Info: TProgramInfo);
    procedure dwsGetVarDeclColonToken(Info: TProgramInfo);
    procedure dwsGetVarDeclTypeToken(Info: TProgramInfo);
    procedure dwsGetVarDeclAbsoluteTarget(Info: TProgramInfo);
    procedure dwsReorderVarDecls(Info: TProgramInfo);
    procedure dwsSplitMultiVarDeclarations(Info: TProgramInfo);
    procedure dwsVarSectionCanBeFormatted(Info: TProgramInfo);
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
  FAvailableProcs := TDictionary<string, Boolean>.Create;
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
  FAvailableProcs.Free;
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
  Func := FUnit.Functions.Add('GetConstKeyword');
  Func.Parameters.Add('ANode', 'TConstSectionSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetConstKeyword;

  Func := FUnit.Functions.Add('GetTypeKeyword');
  Func.Parameters.Add('ANode', 'TTypeSectionSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetTypeKeyword;

  Func := FUnit.Functions.Add('GetVarKeyword');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetVarKeyword;

  Func := FUnit.Functions.Add('GetUsesKeyword');
  Func.Parameters.Add('ANode', 'TUsesClauseSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetUsesKeyword;

  Func := FUnit.Functions.Add('IsUnitLevel');
  Func.Parameters.Add('ANode', 'TObject');
  Func.ResultType := 'Boolean';
  Func.OnEval := dwsIsUnitLevel;

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

  Func := FUnit.Functions.Add('GetUsesItemCount');
  Func.Parameters.Add('ANode', 'TUsesClauseSyntax');
  Func.ResultType := 'Integer';
  Func.OnEval := dwsGetUsesItemCount;

  Func := FUnit.Functions.Add('GetUsesItemName');
  Func.Parameters.Add('ANode', 'TUsesClauseSyntax');
  Func.Parameters.Add('AIndex', 'Integer');
  Func.ResultType := 'String';
  Func.OnEval := dwsGetUsesItemName;

  Func := FUnit.Functions.Add('GetUsesItemToken');
  Func.Parameters.Add('ANode', 'TUsesClauseSyntax');
  Func.Parameters.Add('AIndex', 'Integer');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetUsesItemToken;

  Func := FUnit.Functions.Add('GetUsesSemicolon');
  Func.Parameters.Add('ANode', 'TUsesClauseSyntax');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetUsesSemicolon;

  Func := FUnit.Functions.Add('ReorderUsesItems');
  Func.Parameters.Add('ANode', 'TUsesClauseSyntax');
  Func.Parameters.Add('ANewOrder', 'String');
  Func.OnEval := dwsReorderUsesItems;

  Func := FUnit.Functions.Add('UsesClauseCanBeSorted');
  Func.Parameters.Add('ANode', 'TUsesClauseSyntax');
  Func.ResultType := 'Boolean';
  Func.OnEval := dwsUsesClauseCanBeSorted;

  // Var declaration helpers
  Func := FUnit.Functions.Add('GetVarDeclCount');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.ResultType := 'Integer';
  Func.OnEval := dwsGetVarDeclCount;

  Func := FUnit.Functions.Add('GetVarDeclName');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.Parameters.Add('AIndex', 'Integer');
  Func.ResultType := 'String';
  Func.OnEval := dwsGetVarDeclName;

  Func := FUnit.Functions.Add('GetVarDeclIdentifier');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.Parameters.Add('AIndex', 'Integer');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetVarDeclIdentifier;

  Func := FUnit.Functions.Add('GetVarDeclColonToken');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.Parameters.Add('AIndex', 'Integer');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetVarDeclColonToken;

  Func := FUnit.Functions.Add('GetVarDeclTypeToken');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.Parameters.Add('AIndex', 'Integer');
  Func.ResultType := 'TSyntaxToken';
  Func.OnEval := dwsGetVarDeclTypeToken;

  Func := FUnit.Functions.Add('GetVarDeclAbsoluteTarget');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.Parameters.Add('AIndex', 'Integer');
  Func.ResultType := 'String';
  Func.OnEval := dwsGetVarDeclAbsoluteTarget;

  Func := FUnit.Functions.Add('ReorderVarDecls');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.Parameters.Add('ANewOrder', 'String');
  Func.OnEval := dwsReorderVarDecls;

  Func := FUnit.Functions.Add('SplitMultiVarDeclarations');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.OnEval := dwsSplitMultiVarDeclarations;

  Func := FUnit.Functions.Add('VarSectionCanBeFormatted');
  Func.Parameters.Add('ANode', 'TVarSectionSyntax');
  Func.ResultType := 'Boolean';
  Func.OnEval := dwsVarSectionCanBeFormatted;
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

procedure TDptDwsFormatter.dwsIsUnitLevel(Info: TProgramInfo);
begin
  Info.ResultAsBoolean := (FMethodDepth = 0);
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

procedure TDptDwsFormatter.dwsGetUsesItemCount(Info: TProgramInfo);
var
  Node: TUsesClauseSyntax;
begin
  Node := TUsesClauseSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) then
    Info.ResultAsInteger := Node.UnitReferences.Count
  else
    Info.ResultAsInteger := 0;
end;

procedure TDptDwsFormatter.dwsGetUsesItemName(Info: TProgramInfo);
var
  Node     : TUsesClauseSyntax;
  Ref      : TUnitReferenceSyntax;
  Idx      : Integer;
  ResultStr: String;
begin
  Node := TUsesClauseSyntax(Info.ParamAsObject[0]);
  Idx := Info.ParamAsInteger[1];
  ResultStr := '';
  if Assigned(Node) and (Idx >= 0) and (Idx < Node.UnitReferences.Count) then
  begin
    Ref := Node.UnitReferences[Idx];
    if Assigned(Ref) then
      for var I: Integer := 0 to Ref.Namespaces.Count - 1 do
      begin
        if I > 0 then
          ResultStr := ResultStr + '.';
        ResultStr := ResultStr + Ref.Namespaces[I].Text;
      end;
  end;
  Info.ResultAsString := ResultStr;
end;

procedure TDptDwsFormatter.dwsGetUsesItemToken(Info: TProgramInfo);
var
  Node : TUsesClauseSyntax;
  Ref  : TUnitReferenceSyntax;
  Token: TSyntaxToken;
  Idx  : Integer;
begin
  Node := TUsesClauseSyntax(Info.ParamAsObject[0]);
  Idx := Info.ParamAsInteger[1];
  Token := nil;
  if Assigned(Node) and (Idx >= 0) and (Idx < Node.UnitReferences.Count) then
  begin
    Ref := Node.UnitReferences[Idx];
    if Assigned(Ref) and (Ref.Namespaces.Count > 0) then
      Token := Ref.Namespaces[0];
  end;
  if Assigned(Token) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Token, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetUsesSemicolon(Info: TProgramInfo);
var
  Node: TUsesClauseSyntax;
begin
  Node := TUsesClauseSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.Semicolon) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.Semicolon, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsReorderUsesItems(Info: TProgramInfo);
var
  Node    : TUsesClauseSyntax;
  OrderStr: String;
  Indices : TArray<Integer>;
  OldRefs : TArray<TUnitReferenceSyntax>;
  I, N    : Integer;
  Ref     : TUnitReferenceSyntax;
  P       : Integer;
  Part    : String;
begin
  Node := TUsesClauseSyntax(Info.ParamAsObject[0]);
  OrderStr := Info.ParamAsString[1];
  if not Assigned(Node) or (OrderStr = '') then Exit;

  N := Node.UnitReferences.Count;

  // Parse comma-separated indices
  SetLength(Indices, N);
  I := 0;
  while (OrderStr <> '') and (I < N) do
  begin
    P := Pos(',', OrderStr);
    if P > 0 then
    begin
      Part := Copy(OrderStr, 1, P - 1);
      Delete(OrderStr, 1, P);
    end
    else
    begin
      Part := OrderStr;
      OrderStr := '';
    end;
    Indices[I] := StrToInt(Trim(Part));
    Inc(I);
  end;

  // Save originals
  SetLength(OldRefs, N);
  for I := 0 to N - 1 do
    OldRefs[I] := Node.UnitReferences[I];

  // Reorder in-place
  for I := 0 to N - 1 do
    Node.UnitReferences[I] := OldRefs[Indices[I]];

  // Clear all trivia on unit reference sub-tokens
  for I := 0 to N - 1 do
  begin
    Ref := Node.UnitReferences[I];
    for var J: Integer := 0 to Ref.Namespaces.Count - 1 do
      TDptFormatter.ClearTrivia(Ref.Namespaces[J]);
    for var J: Integer := 0 to Ref.Dots.Count - 1 do
      TDptFormatter.ClearTrivia(Ref.Dots[J]);
    if Assigned(Ref.InKeyword) then
      TDptFormatter.ClearTrivia(Ref.InKeyword);
    if Assigned(Ref.StringLiteral) then
      TDptFormatter.ClearTrivia(Ref.StringLiteral);
  end;

  // Clear trivia on commas and semicolon
  for I := 0 to Node.Commas.Count - 1 do
    TDptFormatter.ClearTrivia(Node.Commas[I]);
  if Assigned(Node.Semicolon) then
    TDptFormatter.ClearTrivia(Node.Semicolon);
end;

procedure TDptDwsFormatter.dwsUsesClauseCanBeSorted(Info: TProgramInfo);
var
  Node   : TUsesClauseSyntax;
  Ref    : TUnitReferenceSyntax;
  LTrivia: String;
  C      : Char;
begin
  Node := TUsesClauseSyntax(Info.ParamAsObject[0]);
  Info.ResultAsBoolean := False;
  if not Assigned(Node) or (Node.UnitReferences.Count = 0) then
    Exit;

  for var I: Integer := 0 to Node.UnitReferences.Count - 1 do
  begin
    Ref := Node.UnitReferences[I];
    if not Assigned(Ref) then
      Exit;

    // Units with 'in' keyword (dpr files) cannot be sorted
    if Assigned(Ref.InKeyword) then
      Exit;

    // Check leading trivia of first namespace token for non-whitespace
    if Ref.Namespaces.Count > 0 then
    begin
      LTrivia := TDptFormatter.GetLeadingTrivia(Ref.Namespaces[0]);
      for var J: Integer := 1 to Length(LTrivia) do
      begin
        C := LTrivia[J];
        if (C <> ' ') and (C <> #9) and (C <> #13) and (C <> #10) then
          Exit;
      end;
    end;
  end;

  Info.ResultAsBoolean := True;
end;

// ---------------------------------------------------------------------------
// Var declaration helpers
// ---------------------------------------------------------------------------

procedure TDptDwsFormatter.dwsGetVarDeclCount(Info: TProgramInfo);
var
  Node: TVarSectionSyntax;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) then
    Info.ResultAsInteger := Node.Declarations.Count
  else
    Info.ResultAsInteger := 0;
end;

procedure TDptDwsFormatter.dwsGetVarDeclName(Info: TProgramInfo);
var
  Node: TVarSectionSyntax;
  Idx : Integer;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  Idx := Info.ParamAsInteger[1];
  if Assigned(Node) and (Idx >= 0) and (Idx < Node.Declarations.Count) and
     Assigned(Node.Declarations[Idx].Identifier) then
    Info.ResultAsString := Node.Declarations[Idx].Identifier.Text
  else
    Info.ResultAsString := '';
end;

procedure TDptDwsFormatter.dwsGetVarDeclIdentifier(Info: TProgramInfo);
var
  Node : TVarSectionSyntax;
  Token: TSyntaxToken;
  Idx  : Integer;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  Idx := Info.ParamAsInteger[1];
  Token := nil;
  if Assigned(Node) and (Idx >= 0) and (Idx < Node.Declarations.Count) then
    Token := Node.Declarations[Idx].Identifier;
  if Assigned(Token) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Token, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetVarDeclColonToken(Info: TProgramInfo);
var
  Node : TVarSectionSyntax;
  Token: TSyntaxToken;
  Idx  : Integer;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  Idx := Info.ParamAsInteger[1];
  Token := nil;
  if Assigned(Node) and (Idx >= 0) and (Idx < Node.Declarations.Count) then
    Token := Node.Declarations[Idx].ColonToken;
  if Assigned(Token) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Token, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetVarDeclTypeToken(Info: TProgramInfo);
var
  Node : TVarSectionSyntax;
  Token: TSyntaxToken;
  Idx  : Integer;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  Idx := Info.ParamAsInteger[1];
  Token := nil;
  if Assigned(Node) and (Idx >= 0) and (Idx < Node.Declarations.Count) then
    Token := Node.Declarations[Idx].TypeIdentifier;
  if Assigned(Token) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Token, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetVarDeclAbsoluteTarget(Info: TProgramInfo);
var
  Node    : TVarSectionSyntax;
  Decl    : TVarDeclarationSyntax;
  Idx     : Integer;
  FoundAbs: Boolean;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  Idx := Info.ParamAsInteger[1];
  Info.ResultAsString := '';
  if not Assigned(Node) or (Idx < 0) or (Idx >= Node.Declarations.Count) then
    Exit;

  Decl := Node.Declarations[Idx];
  FoundAbs := False;
  for var I: Integer := 0 to Decl.TypeExtraTokens.Count - 1 do
  begin
    if FoundAbs then
    begin
      // The token after 'absolute' is the target variable name
      Info.ResultAsString := Decl.TypeExtraTokens[I].Text;
      Exit;
    end;
    if SameText(Decl.TypeExtraTokens[I].Text, 'absolute') then
      FoundAbs := True;
  end;
end;

procedure TDptDwsFormatter.dwsReorderVarDecls(Info: TProgramInfo);
var
  Node   : TVarSectionSyntax;
  OrderStr: String;
  Indices : TArray<Integer>;
  OldDecls: TArray<TVarDeclarationSyntax>;
  I, N    : Integer;
  P       : Integer;
  Part    : String;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  OrderStr := Info.ParamAsString[1];
  if not Assigned(Node) or (OrderStr = '') then Exit;

  N := Node.Declarations.Count;

  // Parse comma-separated indices
  SetLength(Indices, N);
  I := 0;
  while (OrderStr <> '') and (I < N) do
  begin
    P := Pos(',', OrderStr);
    if P > 0 then
    begin
      Part := Copy(OrderStr, 1, P - 1);
      Delete(OrderStr, 1, P);
    end
    else
    begin
      Part := OrderStr;
      OrderStr := '';
    end;
    Indices[I] := StrToInt(Trim(Part));
    Inc(I);
  end;

  // Save originals
  SetLength(OldDecls, N);
  for I := 0 to N - 1 do
    OldDecls[I] := Node.Declarations[I];

  // Reorder in-place
  for I := 0 to N - 1 do
    Node.Declarations[I] := OldDecls[Indices[I]];
end;

procedure TDptDwsFormatter.dwsSplitMultiVarDeclarations(Info: TProgramInfo);
var
  Node      : TVarSectionSyntax;
  Decl      : TVarDeclarationSyntax;
  NewDecl   : TVarDeclarationSyntax;
  I, J      : Integer;
  ColonIdx  : Integer;
  Names     : TArray<String>;
  NameCnt   : Integer;
  TypeName  : String;
  TypeKind  : TTokenKind;
  ExtraStart: Integer;
  ExtraKinds: TArray<TTokenKind>;
  ExtraTexts: TArray<String>;
  ExtraCnt  : Integer;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  if not Assigned(Node) then Exit;

  I := 0;
  while I < Node.Declarations.Count do
  begin
    Decl := Node.Declarations[I];

    // Only process multi-var declarations (ColonToken is nil when parser
    // saw a comma instead of a colon after the first identifier)
    if Assigned(Decl.ColonToken) then
    begin
      Inc(I);
      Continue;
    end;

    // Find the colon in TypeExtraTokens
    ColonIdx := -1;
    for J := 0 to Decl.TypeExtraTokens.Count - 1 do
      if Decl.TypeExtraTokens[J].Kind = tkColon then
      begin
        ColonIdx := J;
        Break;
      end;

    if ColonIdx < 0 then
    begin
      Inc(I);
      Continue;
    end;

    // Collect additional identifier names (between commas, before the colon)
    NameCnt := 0;
    SetLength(Names, ColonIdx);
    for J := 0 to ColonIdx - 1 do
      if Decl.TypeExtraTokens[J].Kind = tkIdentifier then
      begin
        Names[NameCnt] := Decl.TypeExtraTokens[J].Text;
        Inc(NameCnt);
      end;
    SetLength(Names, NameCnt);

    // Save type name and kind (token after the colon)
    TypeName := '';
    TypeKind := tkIdentifier;
    ExtraStart := ColonIdx + 1;
    if ExtraStart < Decl.TypeExtraTokens.Count then
    begin
      TypeName := Decl.TypeExtraTokens[ExtraStart].Text;
      TypeKind := Decl.TypeExtraTokens[ExtraStart].Kind;
      Inc(ExtraStart);
    end;

    // Save remaining extra tokens as plain values (generics etc.)
    ExtraCnt := Decl.TypeExtraTokens.Count - ExtraStart;
    SetLength(ExtraKinds, ExtraCnt);
    SetLength(ExtraTexts, ExtraCnt);
    for J := 0 to ExtraCnt - 1 do
    begin
      ExtraKinds[J] := Decl.TypeExtraTokens[ExtraStart + J].Kind;
      ExtraTexts[J] := Decl.TypeExtraTokens[ExtraStart + J].Text;
    end;

    // Clear the extras first (may free the token objects inside the list),
    // then create fresh tokens for the original declaration
    Decl.TypeExtraTokens.Clear;
    Decl.ColonToken := TSyntaxToken.Create(tkColon, ':');
    if TypeName <> '' then
      Decl.TypeIdentifier := TSyntaxToken.Create(TypeKind, TypeName);
    for J := 0 to ExtraCnt - 1 do
      Decl.TypeExtraTokens.Add(TSyntaxToken.Create(ExtraKinds[J], ExtraTexts[J]));

    // Create new declarations for each additional name
    for J := 0 to NameCnt - 1 do
    begin
      NewDecl := TVarDeclarationSyntax.Create;
      NewDecl.Identifier := TSyntaxToken.Create(tkIdentifier, Names[J]);
      NewDecl.ColonToken := TSyntaxToken.Create(tkColon, ':');
      if TypeName <> '' then
        NewDecl.TypeIdentifier := TSyntaxToken.Create(TypeKind, TypeName);
      for var K: Integer := 0 to ExtraCnt - 1 do
        NewDecl.TypeExtraTokens.Add(TSyntaxToken.Create(ExtraKinds[K], ExtraTexts[K]));
      NewDecl.Semicolon := TSyntaxToken.Create(tkSemicolon, ';');
      Node.Declarations.Insert(I + 1 + J, NewDecl);
    end;

    Inc(I, 1 + NameCnt);
  end;
end;

procedure TDptDwsFormatter.dwsVarSectionCanBeFormatted(Info: TProgramInfo);
var
  Node   : TVarSectionSyntax;
  Decl   : TVarDeclarationSyntax;
  LTrivia: String;
  C      : Char;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  Info.ResultAsBoolean := False;
  if not Assigned(Node) or (Node.Declarations.Count = 0) then
    Exit;

  for var I: Integer := 0 to Node.Declarations.Count - 1 do
  begin
    Decl := Node.Declarations[I];
    // Multi-variable declarations (e.g. "I, J: Integer") have no ColonToken
    if not Assigned(Decl.ColonToken) then
      Exit;
    // Check for non-whitespace trivia (comments, directives) on identifier
    if Assigned(Decl.Identifier) then
    begin
      LTrivia := TDptFormatter.GetLeadingTrivia(Decl.Identifier);
      for var J: Integer := 1 to Length(LTrivia) do
      begin
        C := LTrivia[J];
        if (C <> ' ') and (C <> #9) and (C <> #13) and (C <> #10) then
          Exit;
      end;
    end;
  end;

  Info.ResultAsBoolean := True;
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

procedure TDptDwsFormatter.dwsGetConstKeyword(Info: TProgramInfo);
var
  Node: TConstSectionSyntax;
begin
  Node := TConstSectionSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.ConstKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.ConstKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetTypeKeyword(Info: TProgramInfo);
var
  Node: TTypeSectionSyntax;
begin
  Node := TTypeSectionSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.TypeKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.TypeKeyword, False, False)
  else
    Info.ResultAsVariant := IUnknown(nil);
end;

procedure TDptDwsFormatter.dwsGetVarKeyword(Info: TProgramInfo);
var
  Node: TVarSectionSyntax;
begin
  Node := TVarSectionSyntax(Info.ParamAsObject[0]);
  if Assigned(Node) and Assigned(Node.VarKeyword) then
    Info.ResultAsVariant := Info.RegisterExternalObject(Node.VarKeyword, False, False)
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
  FProcsCached := False;
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
    if not FProcsCached then
      CacheAvailableProcs;
    inherited FormatUnit(AUnit);
  finally
    FExec.EndProgram;
    FExec := nil;
  end;
end;

procedure TDptDwsFormatter.CacheAvailableProcs;
const
  CProcNames: array[0..10] of string = (
    'OnVisitUnitStart', 'OnVisitUnitEnd', 'OnVisitUsesClause',
    'OnVisitInterfaceSection', 'OnVisitImplementationSection',
    'OnVisitMethodImplementation', 'OnVisitClassDeclaration',
    'OnVisitRecordDeclaration', 'OnVisitTypeSection',
    'OnVisitConstSection', 'OnVisitVarSection'
  );
begin
  FAvailableProcs.Clear;
  for var LName: string in CProcNames do
  begin
    try
      var LFunc: IInfo := FExec.Info.Func[LName];
      FAvailableProcs.Add(LName, Assigned(LFunc));
    except
      FAvailableProcs.Add(LName, False);
    end;
  end;
  FProcsCached := True;
end;

procedure TDptDwsFormatter.CallScriptProc(const AProcName, AParamName: string; AObj: TObject);
var
  LAvailable: Boolean;
  Func: IInfo;
begin
  if not Assigned(FExec) then
    Exit;
  if not FAvailableProcs.TryGetValue(AProcName, LAvailable) or not LAvailable then
    Exit;

  Func := FExec.Info.Func[AProcName];
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
