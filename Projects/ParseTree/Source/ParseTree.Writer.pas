unit ParseTree.Writer;

interface

uses
  System.SysUtils,
  System.Classes,
  ParseTree.Core,
  ParseTree.Tokens,
  ParseTree.Nodes;

type
  { A simple visitor that rebuilds the source code by printing every token and its trivia }
  TSyntaxTreeWriter = class
  private
    FBuilder: TStringBuilder;
    procedure WriteTrivia(ATrivia: TSyntaxTrivia);
    procedure WriteToken(AToken: TSyntaxToken);
    procedure WriteUnitReference(ANode: TUnitReferenceSyntax);
    procedure WriteUsesClause(AClause: TUsesClauseSyntax);
    procedure WriteInterfaceSection(ASection: TInterfaceSectionSyntax);
    procedure WriteImplementationSection(ASection: TImplementationSectionSyntax);
    procedure WriteMethodImplementation(ADecl: TMethodImplementationSyntax);
    
    // Statements
    procedure WriteStatement(AStmt: TStatementSyntax);
    procedure WriteWhileStatement(AStmt: TWhileStatementSyntax);
    procedure WriteRepeatStatement(AStmt: TRepeatStatementSyntax);
    procedure WriteForStatement(AStmt: TForStatementSyntax);
    procedure WriteIfStatement(AStmt: TIfStatementSyntax);
    procedure WriteAssignmentStatement(AStmt: TAssignmentStatementSyntax);
    procedure WriteBeginEndStatement(AStmt: TBeginEndStatementSyntax);
    procedure WriteOpaqueStatement(AStmt: TOpaqueStatementSyntax);
    
    // Declarations
    procedure WriteTypeDeclaration(ADecl: TTypeDeclarationSyntax);
    procedure WriteConstDeclaration(ADecl: TConstDeclarationSyntax);
    procedure WriteVarDeclaration(ADecl: TVarDeclarationSyntax);
    procedure WriteUnparsedDeclaration(ADecl: TUnparsedDeclarationSyntax);
    
    // Sections
    procedure WriteTypeSection(ASection: TTypeSectionSyntax);
    procedure WriteConstSection(ASection: TConstSectionSyntax);
    procedure WriteVarSection(ASection: TVarSectionSyntax);
    
    procedure WriteCompilationUnit(ANode: TCompilationUnitSyntax);
    procedure WriteNode(ANode: TSyntaxNode);
  public
    constructor Create;
    destructor Destroy; override;
    function GenerateSource(ANode: TSyntaxNode): string;
  end;

implementation

{ TSyntaxTreeWriter }

constructor TSyntaxTreeWriter.Create;
begin
  inherited Create;
  FBuilder := TStringBuilder.Create;
end;

destructor TSyntaxTreeWriter.Destroy;
begin
  FBuilder.Free;
  inherited;
end;

procedure TSyntaxTreeWriter.WriteTrivia(ATrivia: TSyntaxTrivia);
begin
  if ATrivia <> nil then
    FBuilder.Append(ATrivia.Text);
end;

procedure TSyntaxTreeWriter.WriteToken(AToken: TSyntaxToken);
var
  LTrivia: TSyntaxTrivia;
begin
  if AToken = nil then Exit;
  for LTrivia in AToken.LeadingTrivia do
    WriteTrivia(LTrivia);
    
  FBuilder.Append(AToken.Text);
  
  for LTrivia in AToken.TrailingTrivia do
    WriteTrivia(LTrivia);
end;

procedure TSyntaxTreeWriter.WriteUnitReference(ANode: TUnitReferenceSyntax);
var
  I: Integer;
begin
  if ANode = nil then Exit;
  for I := 0 to ANode.Namespaces.Count - 1 do
  begin
    WriteToken(ANode.Namespaces[I]);
    if I < ANode.Dots.Count then
      WriteToken(ANode.Dots[I]);
  end;

  if Assigned(ANode.InKeyword) then
    WriteToken(ANode.InKeyword);

  if Assigned(ANode.StringLiteral) then
    WriteToken(ANode.StringLiteral);
end;

procedure TSyntaxTreeWriter.WriteUsesClause(AClause: TUsesClauseSyntax);
var
  I: Integer;
begin
  if AClause = nil then Exit;
  WriteToken(AClause.UsesKeyword);
  for I := 0 to AClause.UnitReferences.Count - 1 do
  begin
    WriteUnitReference(AClause.UnitReferences[I]);
    if I < AClause.Commas.Count then
      WriteToken(AClause.Commas[I]);
  end;
  WriteToken(AClause.Semicolon);
end;

procedure TSyntaxTreeWriter.WriteTypeDeclaration(ADecl: TTypeDeclarationSyntax);
var
  LVisSec: TVisibilitySectionSyntax;
  LMember: TClassMemberSyntax;
  LToken: TSyntaxToken;
begin
  if ADecl = nil then Exit;
  WriteToken(ADecl.Identifier);
  WriteToken(ADecl.EqualsToken);
  WriteToken(ADecl.TypeTypeToken);
  for LToken in ADecl.TypeExtraTokens do
    WriteToken(LToken);
  
  if Assigned(ADecl.BaseListTokens) then
  begin
    for LToken in ADecl.BaseListTokens do
      WriteToken(LToken);
  end;
  
  if Assigned(ADecl.VisibilitySections) then
  begin
    for LVisSec in ADecl.VisibilitySections do
    begin
      WriteToken(LVisSec.StrictKeyword);
      WriteToken(LVisSec.VisibilityKeyword);
      for LMember in LVisSec.Members do
      begin
        for LToken in LMember.Tokens do
          WriteToken(LToken);
      end;
    end;
  end;

  WriteToken(ADecl.EndKeyword);
  WriteToken(ADecl.Semicolon);
end;

procedure TSyntaxTreeWriter.WriteConstDeclaration(ADecl: TConstDeclarationSyntax);
var
  LToken: TSyntaxToken;
begin
  if ADecl = nil then Exit;
  WriteToken(ADecl.Identifier);
  WriteToken(ADecl.ColonToken);
  WriteToken(ADecl.TypeIdentifier);
  WriteToken(ADecl.EqualsToken);
  for LToken in ADecl.ValueTokens do
    WriteToken(LToken);
  WriteToken(ADecl.Semicolon);
end;

procedure TSyntaxTreeWriter.WriteVarDeclaration(ADecl: TVarDeclarationSyntax);
var
  LToken: TSyntaxToken;
begin
  if ADecl = nil then Exit;
  WriteToken(ADecl.Identifier);
  WriteToken(ADecl.ColonToken);
  WriteToken(ADecl.TypeIdentifier);
  for LToken in ADecl.TypeExtraTokens do
    WriteToken(LToken);
  WriteToken(ADecl.Semicolon);
end;

procedure TSyntaxTreeWriter.WriteTypeSection(ASection: TTypeSectionSyntax);
var
  LDecl: TTypeDeclarationSyntax;
begin
  if ASection = nil then Exit;
  WriteToken(ASection.TypeKeyword);
  for LDecl in ASection.Declarations do
    WriteTypeDeclaration(LDecl);
end;

procedure TSyntaxTreeWriter.WriteConstSection(ASection: TConstSectionSyntax);
var
  LDecl: TConstDeclarationSyntax;
begin
  if ASection = nil then Exit;
  WriteToken(ASection.ConstKeyword);
  for LDecl in ASection.Declarations do
    WriteConstDeclaration(LDecl);
end;

procedure TSyntaxTreeWriter.WriteVarSection(ASection: TVarSectionSyntax);
var
  LDecl: TVarDeclarationSyntax;
begin
  if ASection = nil then Exit;
  WriteToken(ASection.VarKeyword);
  for LDecl in ASection.Declarations do
    WriteVarDeclaration(LDecl);
end;

procedure TSyntaxTreeWriter.WriteInterfaceSection(ASection: TInterfaceSectionSyntax);
var
  LDecl: TDeclarationSectionSyntax;
begin
  if ASection = nil then Exit;
  WriteToken(ASection.InterfaceKeyword);
  WriteUsesClause(ASection.UsesClause);
  
  if Assigned(ASection.Declarations) then
  begin
    for LDecl in ASection.Declarations do
      WriteNode(LDecl);
  end;
end;

procedure TSyntaxTreeWriter.WriteImplementationSection(ASection: TImplementationSectionSyntax);
var
  LDecl: TDeclarationSectionSyntax;
begin
  if ASection = nil then Exit;
  WriteToken(ASection.ImplementationKeyword);
  WriteUsesClause(ASection.UsesClause);
  
  if Assigned(ASection.Declarations) then
  begin
    for LDecl in ASection.Declarations do
      WriteNode(LDecl);
  end;
end;

procedure TSyntaxTreeWriter.WriteUnparsedDeclaration(ADecl: TUnparsedDeclarationSyntax);
var
  LToken: TSyntaxToken;
begin
  if ADecl = nil then Exit;
  if Assigned(ADecl.Tokens) then
  begin
    for LToken in ADecl.Tokens do
      WriteToken(LToken);
  end;
end;

procedure TSyntaxTreeWriter.WriteMethodImplementation(ADecl: TMethodImplementationSyntax);
var
  LToken: TSyntaxToken;
  LDeclLocal: TDeclarationSectionSyntax;
  LStmt: TStatementSyntax;
begin
  if ADecl = nil then Exit;
  
  WriteToken(ADecl.ClassKeyword);
  WriteToken(ADecl.MethodTypeKeyword);
  for LToken in ADecl.SignatureTokens do
    WriteToken(LToken);
  WriteToken(ADecl.SignatureSemicolon);
  
  for LDeclLocal in ADecl.LocalDeclarations do
    WriteNode(LDeclLocal);
    
  WriteToken(ADecl.BeginKeyword);
  
  for LStmt in ADecl.Statements do
    WriteStatement(LStmt);

  for LToken in ADecl.BodyTokens do
    WriteToken(LToken);
    
  WriteToken(ADecl.EndKeyword);
  WriteToken(ADecl.FinalSemicolon);
end;

procedure TSyntaxTreeWriter.WriteStatement(AStmt: TStatementSyntax);
begin
  if AStmt = nil then Exit;
  
  if AStmt is TWhileStatementSyntax then
    WriteWhileStatement(TWhileStatementSyntax(AStmt))
  else if AStmt is TRepeatStatementSyntax then
    WriteRepeatStatement(TRepeatStatementSyntax(AStmt))
  else if AStmt is TForStatementSyntax then
    WriteForStatement(TForStatementSyntax(AStmt))
  else if AStmt is TIfStatementSyntax then
    WriteIfStatement(TIfStatementSyntax(AStmt))
  else if AStmt is TAssignmentStatementSyntax then
    WriteAssignmentStatement(TAssignmentStatementSyntax(AStmt))
  else if AStmt is TBeginEndStatementSyntax then
    WriteBeginEndStatement(TBeginEndStatementSyntax(AStmt))
  else if AStmt is TOpaqueStatementSyntax then
    WriteOpaqueStatement(TOpaqueStatementSyntax(AStmt));
end;

procedure TSyntaxTreeWriter.WriteWhileStatement(AStmt: TWhileStatementSyntax);
var
  LToken: TSyntaxToken;
begin
  WriteToken(AStmt.WhileKeyword);
  for LToken in AStmt.ConditionTokens do
    WriteToken(LToken);
  WriteToken(AStmt.DoKeyword);
  WriteStatement(AStmt.Statement);
  for LToken in AStmt.BodyTokens do
    WriteToken(LToken);
end;

procedure TSyntaxTreeWriter.WriteRepeatStatement(AStmt: TRepeatStatementSyntax);
var
  LToken: TSyntaxToken;
  LInnerStmt: TStatementSyntax;
begin
  WriteToken(AStmt.RepeatKeyword);
  for LInnerStmt in AStmt.Statements do
    WriteStatement(LInnerStmt);
  WriteToken(AStmt.UntilKeyword);
  for LToken in AStmt.ConditionTokens do
    WriteToken(LToken);
  for LToken in AStmt.BodyTokens do
    WriteToken(LToken);
end;

procedure TSyntaxTreeWriter.WriteForStatement(AStmt: TForStatementSyntax);
var
  LToken: TSyntaxToken;
begin
  WriteToken(AStmt.ForKeyword);
  for LToken in AStmt.VariableTokens do
    WriteToken(LToken);
  WriteToken(AStmt.AssignmentToken);
  for LToken in AStmt.StartTokens do
    WriteToken(LToken);
  WriteToken(AStmt.ToDowntoKeyword);
  for LToken in AStmt.EndTokens do
    WriteToken(LToken);
  WriteToken(AStmt.DoKeyword);
  WriteStatement(AStmt.Statement);
  for LToken in AStmt.BodyTokens do
    WriteToken(LToken);
end;

procedure TSyntaxTreeWriter.WriteIfStatement(AStmt: TIfStatementSyntax);
var
  LToken: TSyntaxToken;
begin
  WriteToken(AStmt.IfKeyword);
  for LToken in AStmt.ConditionTokens do
    WriteToken(LToken);
  WriteToken(AStmt.ThenKeyword);
  WriteStatement(AStmt.ThenStatement);
  if Assigned(AStmt.ElseKeyword) then
  begin
    WriteToken(AStmt.ElseKeyword);
    WriteStatement(AStmt.ElseStatement);
  end;
end;

procedure TSyntaxTreeWriter.WriteAssignmentStatement(AStmt: TAssignmentStatementSyntax);
var
  LToken: TSyntaxToken;
begin
  for LToken in AStmt.LeftTokens do
    WriteToken(LToken);

  if Assigned(AStmt.ColonEqualsToken) then
    WriteToken(AStmt.ColonEqualsToken);
    
  for LToken in AStmt.RightTokens do
    WriteToken(LToken);
end;

procedure TSyntaxTreeWriter.WriteBeginEndStatement(AStmt: TBeginEndStatementSyntax);
var
  LStmt: TStatementSyntax;
begin
  WriteToken(AStmt.BeginKeyword);
  for LStmt in AStmt.Statements do
    WriteStatement(LStmt);
  WriteToken(AStmt.EndKeyword);
  WriteToken(AStmt.Semicolon);
end;

procedure TSyntaxTreeWriter.WriteOpaqueStatement(AStmt: TOpaqueStatementSyntax);
var
  LToken: TSyntaxToken;
begin
  for LToken in AStmt.Tokens do
    WriteToken(LToken);
end;

procedure TSyntaxTreeWriter.WriteCompilationUnit(ANode: TCompilationUnitSyntax);
var
  I: Integer;
begin
  if ANode = nil then Exit;
  WriteToken(ANode.UnitKeyword);
  
  for I := 0 to ANode.Namespaces.Count - 1 do
  begin
    WriteToken(ANode.Namespaces[I]);
    if I < ANode.Dots.Count then
      WriteToken(ANode.Dots[I]);
  end;

  WriteToken(ANode.Semicolon);

  for var LDecl1 in ANode.PreInterfaceDeclarations do
    WriteNode(LDecl1);

  WriteInterfaceSection(ANode.InterfaceSection);

  for var LDeclMid in ANode.IntfImplDeclarations do
    WriteNode(LDeclMid);

  WriteImplementationSection(ANode.ImplementationSection);

  for var LDecl2 in ANode.PostImplementationDeclarations do
    WriteNode(LDecl2);

  WriteToken(ANode.FinalEndKeyword);
  WriteToken(ANode.FinalDotToken);
  WriteToken(ANode.EndOfFileToken);
end;

procedure TSyntaxTreeWriter.WriteNode(ANode: TSyntaxNode);
begin
  if ANode = nil then Exit;
  
  if ANode is TCompilationUnitSyntax then
    WriteCompilationUnit(TCompilationUnitSyntax(ANode))
  else if ANode is TInterfaceSectionSyntax then
    WriteInterfaceSection(TInterfaceSectionSyntax(ANode))
  else if ANode is TImplementationSectionSyntax then
    WriteImplementationSection(TImplementationSectionSyntax(ANode))
  else if ANode is TUnitReferenceSyntax then
    WriteUnitReference(TUnitReferenceSyntax(ANode))
  else if ANode is TUsesClauseSyntax then
    WriteUsesClause(TUsesClauseSyntax(ANode))
  else if ANode is TTypeSectionSyntax then
    WriteTypeSection(TTypeSectionSyntax(ANode))
  else if ANode is TConstSectionSyntax then
    WriteConstSection(TConstSectionSyntax(ANode))
  else if ANode is TVarSectionSyntax then
    WriteVarSection(TVarSectionSyntax(ANode))
  else if ANode is TTypeDeclarationSyntax then
    WriteTypeDeclaration(TTypeDeclarationSyntax(ANode))
  else if ANode is TConstDeclarationSyntax then
    WriteConstDeclaration(TConstDeclarationSyntax(ANode))
  else if ANode is TVarDeclarationSyntax then
    WriteVarDeclaration(TVarDeclarationSyntax(ANode))
  else if ANode is TMethodImplementationSyntax then
    WriteMethodImplementation(TMethodImplementationSyntax(ANode))
  else if ANode is TUnparsedDeclarationSyntax then
    WriteUnparsedDeclaration(TUnparsedDeclarationSyntax(ANode));
end;

function TSyntaxTreeWriter.GenerateSource(ANode: TSyntaxNode): string;
begin
  FBuilder.Clear;
  WriteNode(ANode);
  Result := FBuilder.ToString;
end;

end.
