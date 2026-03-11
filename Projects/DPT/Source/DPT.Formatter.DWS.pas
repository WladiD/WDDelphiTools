unit DPT.Formatter.DWS;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.IOUtils, System.Variants,
  dwsComp, dwsExprs, dwsRTTIExposer, dwsInfo,
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
  protected
    procedure OnVisitUsesClause(AUses: TUsesClauseSyntax); override;
    procedure OnVisitClassDeclaration(AClass: TClassDeclarationSyntax); override;
    procedure OnVisitRecordDeclaration(ARecord: TRecordDeclarationSyntax); override;
    procedure OnVisitMethodImplementation(AMethod: TMethodImplementationSyntax); override;
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
  
  LFunc := FExec.Info.Func[AProcName];
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

end.
