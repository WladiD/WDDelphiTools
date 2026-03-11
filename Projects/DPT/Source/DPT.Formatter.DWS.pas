unit DPT.Formatter.DWS;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.IOUtils, System.Variants,
  dwsComp, dwsExprs, dwsRTTIExposer, dwsInfo, dwsErrors,
  ParseTree.Core, ParseTree.Nodes, DPT.Formatter;

type
  {
    Formatter that loads a DWScript file and delegates formatting tasks
    to script functions.
  }
  TDptDwsFormatter = class(TDptFormatter)
  private
    FScript: TDelphiWebScript;
    FProgram: IdwsProgram;
    FExec: IdwsProgramExecution;
    FUnit: TdwsUnit;
    
    procedure SetupScriptUnit;
    procedure CallScriptProc(const AProcName, AParamName: string; AObj: TObject);
    
    // DWScript function handlers
    procedure dwsClearTrivia(Info: TProgramInfo);
    procedure dwsAddLeadingTrivia(Info: TProgramInfo);
    procedure dwsAddTrailingTrivia(Info: TProgramInfo);
    
    // AST wrappers
    procedure dwsGetUsesKeyword(Info: TProgramInfo);
    procedure dwsGetInterfaceKeyword(Info: TProgramInfo);
    procedure dwsGetImplementationKeyword(Info: TProgramInfo);
    procedure dwsGetFinalEndKeyword(Info: TProgramInfo);
  protected
    procedure OnVisitUsesClause(AUses: TUsesClauseSyntax); override;
    procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax); override;
    procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax); override;
    procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax); override;
    procedure OnVisitInterfaceSection(ASection: TInterfaceSectionSyntax); override;
    procedure OnVisitImplementationSection(ASection: TImplementationSectionSyntax); override;
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
  
  with FUnit.Functions.Add('ClearTrivia') do
  begin
    Parameters.Add('AToken', 'TSyntaxToken');
    OnEval := dwsClearTrivia;
  end;
  
  with FUnit.Functions.Add('AddLeadingTrivia') do
  begin
    Parameters.Add('AToken', 'TSyntaxToken');
    Parameters.Add('ATriviaText', 'String');
    OnEval := dwsAddLeadingTrivia;
  end;
  
  with FUnit.Functions.Add('AddTrailingTrivia') do
  begin
    Parameters.Add('AToken', 'TSyntaxToken');
    Parameters.Add('ATriviaText', 'String');
    OnEval := dwsAddTrailingTrivia;
  end;

  // AST Wrappers
  with FUnit.Functions.Add('GetUsesKeyword') do
  begin
    Parameters.Add('ANode', 'TUsesClauseSyntax');
    ResultType := 'TSyntaxToken';
    OnEval := dwsGetUsesKeyword;
  end;

  with FUnit.Functions.Add('GetInterfaceKeyword') do
  begin
    Parameters.Add('ANode', 'TInterfaceSectionSyntax');
    ResultType := 'TSyntaxToken';
    OnEval := dwsGetInterfaceKeyword;
  end;

  with FUnit.Functions.Add('GetImplementationKeyword') do
  begin
    Parameters.Add('ANode', 'TImplementationSectionSyntax');
    ResultType := 'TSyntaxToken';
    OnEval := dwsGetImplementationKeyword;
  end;

  with FUnit.Functions.Add('GetFinalEndKeyword') do
  begin
    Parameters.Add('ANode', 'TCompilationUnitSyntax');
    ResultType := 'TSyntaxToken';
    OnEval := dwsGetFinalEndKeyword;
  end;
end;

procedure TDptDwsFormatter.dwsClearTrivia(Info: TProgramInfo);
begin
  TDptFormatter.ClearTrivia(TSyntaxToken(Info.ParamAsObject[0]));
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
  if not Assigned(FExec) then Exit;
  
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
  begin
    LFunc.Call([FExec.Info.RegisterExternalObject(AObj, False, False)]);
  end;
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

procedure TDptDwsFormatter.OnVisitUnitEnd(AUnit: TCompilationUnitSyntax);
begin
  inherited;
  CallScriptProc('OnVisitUnitEnd', 'AUnit', AUnit);
end;

end.
