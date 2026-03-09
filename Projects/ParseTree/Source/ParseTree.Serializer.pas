unit ParseTree.Serializer;

interface

uses
  System.JSON, ParseTree.Core, ParseTree.Nodes, ParseTree.Tokens;

type
  { Serializes a Syntax Tree representing CST into a JSON structured representation }
  TSyntaxTreeSerializer = class
  private
    function SerializeTrivia(ATrivia: TSyntaxTrivia): TJSONObject;
    function SerializeToken(AToken: TSyntaxToken): TJSONObject;
    function SerializeUnitReference(ANode: TUnitReferenceSyntax): TJSONObject;
    function SerializeUsesClause(AClause: TUsesClauseSyntax): System.JSON.TJSONObject;
    function SerializeInterfaceSection(ASection: TInterfaceSectionSyntax): System.JSON.TJSONObject;
    function SerializeImplementationSection(ASection: TImplementationSectionSyntax): System.JSON.TJSONObject;
    function SerializeMethodImplementation(AMethod: TMethodImplementationSyntax): System.JSON.TJSONObject;
    function SerializeUnparsedDeclaration(ADecl: TUnparsedDeclarationSyntax): System.JSON.TJSONObject;
    
    // Declaration Sections
    function SerializeTypeDeclaration(ADecl: TTypeDeclarationSyntax): System.JSON.TJSONObject;
    function SerializeConstDeclaration(ADecl: TConstDeclarationSyntax): System.JSON.TJSONObject;
    function SerializeVarDeclaration(ADecl: TVarDeclarationSyntax): System.JSON.TJSONObject;
    
    function SerializeTypeSection(ASection: TTypeSectionSyntax): System.JSON.TJSONObject;
    function SerializeConstSection(ASection: TConstSectionSyntax): System.JSON.TJSONObject;
    function SerializeVarSection(ASection: TVarSectionSyntax): System.JSON.TJSONObject;
    function SerializeCompilationUnit(ANode: TCompilationUnitSyntax): TJSONObject;

    // Statements
    function SerializeStatement(AStmt: TStatementSyntax): System.JSON.TJSONObject;
    function SerializeWhileStatement(AStmt: TWhileStatementSyntax): System.JSON.TJSONObject;
    function SerializeRepeatStatement(AStmt: TRepeatStatementSyntax): System.JSON.TJSONObject;
    function SerializeForStatement(AStmt: TForStatementSyntax): System.JSON.TJSONObject;
    function SerializeIfStatement(AStmt: TIfStatementSyntax): System.JSON.TJSONObject;
    function SerializeAssignmentStatement(AStmt: TAssignmentStatementSyntax): System.JSON.TJSONObject;
    function SerializeBeginEndStatement(AStmt: TBeginEndStatementSyntax): System.JSON.TJSONObject;
    function SerializeTryStatement(AStmt: TTryStatementSyntax): System.JSON.TJSONObject;
    function SerializeOpaqueStatement(AStmt: TOpaqueStatementSyntax): System.JSON.TJSONObject;
  public
    function SerializeNode(ANode: TSyntaxNode): TJSONObject;
  end;

implementation

uses
  System.Generics.Collections, System.SysUtils;

{ TSyntaxTreeSerializer }

function TSyntaxTreeSerializer.SerializeTrivia(ATrivia: TSyntaxTrivia): TJSONObject;
begin
  if ATrivia = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'Trivia');
  Result.AddPair('Text', TJSONString.Create(ATrivia.Text));
end;

function TSyntaxTreeSerializer.SerializeToken(AToken: TSyntaxToken): TJSONObject;
var
  LArray: TJSONArray;
  LTrivia: TSyntaxTrivia;
begin
  if AToken = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'Token');
  Result.AddPair('Kind', AToken.Kind.ToString);
  Result.AddPair('Text', TJSONString.Create(AToken.Text));

  if AToken.LeadingTrivia.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LTrivia in AToken.LeadingTrivia do
      LArray.AddElement(SerializeTrivia(LTrivia));
    Result.AddPair('LeadingTrivia', LArray);
  end;

  if AToken.TrailingTrivia.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LTrivia in AToken.TrailingTrivia do
      LArray.AddElement(SerializeTrivia(LTrivia));
    Result.AddPair('TrailingTrivia', LArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeUnitReference(ANode: TUnitReferenceSyntax): TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
begin
  if ANode = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'UnitReference');
  
  if ANode.Namespaces.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in ANode.Namespaces do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('Namespaces', LArray);
  end;

  if ANode.Dots.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in ANode.Dots do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('Dots', LArray);
  end;

  if Assigned(ANode.InKeyword) then
    Result.AddPair('InKeyword', SerializeToken(ANode.InKeyword));

  if Assigned(ANode.StringLiteral) then
    Result.AddPair('StringLiteral', SerializeToken(ANode.StringLiteral));
end;

function TSyntaxTreeSerializer.SerializeUsesClause(AClause: TUsesClauseSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
  LRef: TUnitReferenceSyntax;
begin
  if AClause = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'UsesClause');
  Result.AddPair('UsesKeyword', SerializeToken(AClause.UsesKeyword));
  
  if AClause.UnitReferences.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LRef in AClause.UnitReferences do
      LArray.AddElement(SerializeUnitReference(LRef));
    Result.AddPair('UnitReferences', LArray);
  end;

  if AClause.Commas.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in AClause.Commas do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('Commas', LArray);
  end;
  
  Result.AddPair('Semicolon', SerializeToken(AClause.Semicolon));
end;

function TSyntaxTreeSerializer.SerializeInterfaceSection(ASection: TInterfaceSectionSyntax): System.JSON.TJSONObject;
var
  LDecl: TDeclarationSectionSyntax;
  LDeclArray: TJSONArray;
begin
  if ASection = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'InterfaceSection');
  if Assigned(ASection.InterfaceKeyword) then
    Result.AddPair('InterfaceKeyword', SerializeToken(ASection.InterfaceKeyword));
  if Assigned(ASection.UsesClause) then
    Result.AddPair('UsesClause', SerializeUsesClause(ASection.UsesClause));
    
  if Assigned(ASection.Declarations) and (ASection.Declarations.Count > 0) then
  begin
    LDeclArray := TJSONArray.Create;
    for LDecl in ASection.Declarations do
      LDeclArray.AddElement(SerializeNode(LDecl));
    Result.AddPair('Declarations', LDeclArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeTypeSection(ASection: TTypeSectionSyntax): System.JSON.TJSONObject;
var
  LDecl: TTypeDeclarationSyntax;
  LDeclArray: TJSONArray;
begin
  if ASection = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'TypeSection');
  if Assigned(ASection.TypeKeyword) then
    Result.AddPair('TypeKeyword', SerializeToken(ASection.TypeKeyword));
    
  if Assigned(ASection.Declarations) and (ASection.Declarations.Count > 0) then
  begin
    LDeclArray := TJSONArray.Create;
    for LDecl in ASection.Declarations do
      LDeclArray.AddElement(SerializeNode(LDecl));
    Result.AddPair('Declarations', LDeclArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeConstSection(ASection: TConstSectionSyntax): System.JSON.TJSONObject;
var
  LDecl: TConstDeclarationSyntax;
  LDeclArray: TJSONArray;
begin
  if ASection = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'ConstSection');
  if Assigned(ASection.ConstKeyword) then
    Result.AddPair('ConstKeyword', SerializeToken(ASection.ConstKeyword));
    
  if Assigned(ASection.Declarations) and (ASection.Declarations.Count > 0) then
  begin
    LDeclArray := TJSONArray.Create;
    for LDecl in ASection.Declarations do
      LDeclArray.AddElement(SerializeNode(LDecl));
    Result.AddPair('Declarations', LDeclArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeVarSection(ASection: TVarSectionSyntax): System.JSON.TJSONObject;
var
  LDecl: TVarDeclarationSyntax;
  LDeclArray: TJSONArray;
begin
  if ASection = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'VarSection');
  if Assigned(ASection.VarKeyword) then
    Result.AddPair('VarKeyword', SerializeToken(ASection.VarKeyword));
    
  if Assigned(ASection.Declarations) and (ASection.Declarations.Count > 0) then
  begin
    LDeclArray := TJSONArray.Create;
    for LDecl in ASection.Declarations do
      LDeclArray.AddElement(SerializeNode(LDecl));
    Result.AddPair('Declarations', LDeclArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeTypeDeclaration(ADecl: TTypeDeclarationSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LVisSec: TVisibilitySectionSyntax;
  LMember: TClassMemberSyntax;
  LVisObj, LMemberObj: TJSONObject;
  LMemberArray, LTokenArray: TJSONArray;
  LToken: TSyntaxToken;
begin
  if ADecl = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'TypeDeclaration');
  if Assigned(ADecl.Identifier) then
    Result.AddPair('Identifier', SerializeToken(ADecl.Identifier));
  if Assigned(ADecl.EqualsToken) then
    Result.AddPair('EqualsToken', SerializeToken(ADecl.EqualsToken));
  if Assigned(ADecl.TypeTypeToken) then
    Result.AddPair('TypeTypeToken', SerializeToken(ADecl.TypeTypeToken));
    
  if Assigned(ADecl.VisibilitySections) and (ADecl.VisibilitySections.Count > 0) then
  begin
    LArray := TJSONArray.Create;
    for LVisSec in ADecl.VisibilitySections do
    begin
      LVisObj := TJSONObject.Create;
      LVisObj.AddPair('NodeType', 'VisibilitySection');
      if LVisSec.IsStrict then
        LVisObj.AddPair('StrictKeyword', SerializeToken(LVisSec.StrictKeyword));
      if Assigned(LVisSec.VisibilityKeyword) then
        LVisObj.AddPair('VisibilityKeyword', SerializeToken(LVisSec.VisibilityKeyword));
      
      LMemberArray := TJSONArray.Create;
      for LMember in LVisSec.Members do
      begin
        LMemberObj := TJSONObject.Create;
        LMemberObj.AddPair('NodeType', 'ClassMember');
        LTokenArray := TJSONArray.Create;
        for LToken in LMember.Tokens do
          LTokenArray.AddElement(SerializeToken(LToken));
        LMemberObj.AddPair('Tokens', LTokenArray);
        LMemberArray.AddElement(LMemberObj);
      end;
      LVisObj.AddPair('Members', LMemberArray);
      
      LArray.AddElement(LVisObj);
    end;
    Result.AddPair('VisibilitySections', LArray);
  end;

  if Assigned(ADecl.EndKeyword) then
    Result.AddPair('EndKeyword', SerializeToken(ADecl.EndKeyword));
  if Assigned(ADecl.Semicolon) then
    Result.AddPair('Semicolon', SerializeToken(ADecl.Semicolon));
end;

function TSyntaxTreeSerializer.SerializeConstDeclaration(ADecl: TConstDeclarationSyntax): System.JSON.TJSONObject;
begin
  if ADecl = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'ConstDeclaration');
  if Assigned(ADecl.Identifier) then
    Result.AddPair('Identifier', SerializeToken(ADecl.Identifier));
  if Assigned(ADecl.ColonToken) then
    Result.AddPair('ColonToken', SerializeToken(ADecl.ColonToken));
  if Assigned(ADecl.TypeIdentifier) then
    Result.AddPair('TypeIdentifier', SerializeToken(ADecl.TypeIdentifier));
  if Assigned(ADecl.EqualsToken) then
    Result.AddPair('EqualsToken', SerializeToken(ADecl.EqualsToken));

  if Assigned(ADecl.ValueTokens) and (ADecl.ValueTokens.Count > 0) then
  begin
    var LTokArr := TJSONArray.Create;
    for var Tok in ADecl.ValueTokens do
      LTokArr.AddElement(SerializeToken(Tok));
    Result.AddPair('ValueTokens', LTokArr);
  end;

  if Assigned(ADecl.Semicolon) then
    Result.AddPair('Semicolon', SerializeToken(ADecl.Semicolon));
end;

function TSyntaxTreeSerializer.SerializeVarDeclaration(ADecl: TVarDeclarationSyntax): System.JSON.TJSONObject;
var
  LToken: TSyntaxToken;
  LArray: TJSONArray;
begin
  if ADecl = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'VarDeclaration');
  if Assigned(ADecl.Identifier) then
    Result.AddPair('Identifier', SerializeToken(ADecl.Identifier));
  if Assigned(ADecl.ColonToken) then
    Result.AddPair('ColonToken', SerializeToken(ADecl.ColonToken));
  if Assigned(ADecl.TypeIdentifier) then
    Result.AddPair('TypeIdentifier', SerializeToken(ADecl.TypeIdentifier));
  if ADecl.TypeExtraTokens.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in ADecl.TypeExtraTokens do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('TypeExtraTokens', LArray);
  end;
  if Assigned(ADecl.Semicolon) then
    Result.AddPair('Semicolon', SerializeToken(ADecl.Semicolon));
end;

function TSyntaxTreeSerializer.SerializeCompilationUnit(ANode: TCompilationUnitSyntax): TJSONObject;
  var
    LArray: TJSONArray;
    LToken: TSyntaxToken;
  begin
    if ANode = nil then Exit(nil);
    Result := TJSONObject.Create;
    Result.AddPair('NodeType', 'CompilationUnit');
    if Assigned(ANode.UnitKeyword) then
      Result.AddPair('UnitKeyword', SerializeToken(ANode.UnitKeyword));
      
    if ANode.Namespaces.Count > 0 then
    begin
      LArray := TJSONArray.Create;
      for LToken in ANode.Namespaces do
        LArray.AddElement(SerializeToken(LToken));
      Result.AddPair('Namespaces', LArray);
    end;

    if ANode.Dots.Count > 0 then
    begin
      LArray := TJSONArray.Create;
      for LToken in ANode.Dots do
        LArray.AddElement(SerializeToken(LToken));
      Result.AddPair('Dots', LArray);
    end;
    
    if Assigned(ANode.Semicolon) then
      Result.AddPair('Semicolon', SerializeToken(ANode.Semicolon));
  
  if Assigned(ANode.InterfaceSection) then
    Result.AddPair('InterfaceSection', SerializeInterfaceSection(ANode.InterfaceSection));

  if Assigned(ANode.ImplementationSection) then
    Result.AddPair('ImplementationSection', SerializeImplementationSection(ANode.ImplementationSection));

  if Assigned(ANode.FinalEndKeyword) then
    Result.AddPair('FinalEndKeyword', SerializeToken(ANode.FinalEndKeyword));
  if Assigned(ANode.FinalDotToken) then
    Result.AddPair('FinalDotToken', SerializeToken(ANode.FinalDotToken));
  if Assigned(ANode.EndOfFileToken) then
    Result.AddPair('EndOfFileToken', SerializeToken(ANode.EndOfFileToken));
end;

function TSyntaxTreeSerializer.SerializeImplementationSection(ASection: TImplementationSectionSyntax): System.JSON.TJSONObject;
var
  LDecl: TDeclarationSectionSyntax;
  LDeclArray: TJSONArray;
begin
  if ASection = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'ImplementationSection');
  if Assigned(ASection.ImplementationKeyword) then
    Result.AddPair('ImplementationKeyword', SerializeToken(ASection.ImplementationKeyword));
  if Assigned(ASection.UsesClause) then
    Result.AddPair('UsesClause', SerializeUsesClause(ASection.UsesClause));

  if Assigned(ASection.Declarations) and (ASection.Declarations.Count > 0) then
  begin
    LDeclArray := TJSONArray.Create;
    for LDecl in ASection.Declarations do
      LDeclArray.AddElement(SerializeNode(LDecl));
    Result.AddPair('Declarations', LDeclArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeMethodImplementation(AMethod: TMethodImplementationSyntax): System.JSON.TJSONObject;
var
  LToken: TSyntaxToken;
  LDecl: TDeclarationSectionSyntax;
  LArray: TJSONArray;
begin
  if AMethod = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'MethodImplementation');

  if Assigned(AMethod.ClassKeyword) then
    Result.AddPair('ClassKeyword', SerializeToken(AMethod.ClassKeyword));

  if Assigned(AMethod.MethodTypeKeyword) then
    Result.AddPair('MethodTypeKeyword', SerializeToken(AMethod.MethodTypeKeyword));

  if AMethod.SignatureTokens.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in AMethod.SignatureTokens do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('SignatureTokens', LArray);
  end;

  if Assigned(AMethod.SignatureSemicolon) then
    Result.AddPair('SignatureSemicolon', SerializeToken(AMethod.SignatureSemicolon));

  if AMethod.LocalDeclarations.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LDecl in AMethod.LocalDeclarations do
      LArray.AddElement(SerializeNode(LDecl));
    Result.AddPair('LocalDeclarations', LArray);
  end;

  if Assigned(AMethod.BeginKeyword) then
    Result.AddPair('BeginKeyword', SerializeToken(AMethod.BeginKeyword));

  if AMethod.Statements.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for var LStmt in AMethod.Statements do
      LArray.AddElement(SerializeStatement(LStmt));
    Result.AddPair('Statements', LArray);
  end;

  if AMethod.BodyTokens.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in AMethod.BodyTokens do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('BodyTokens', LArray);
  end;

  if Assigned(AMethod.EndKeyword) then
    Result.AddPair('EndKeyword', SerializeToken(AMethod.EndKeyword));
  if Assigned(AMethod.FinalSemicolon) then
    Result.AddPair('FinalSemicolon', SerializeToken(AMethod.FinalSemicolon));
end;

function TSyntaxTreeSerializer.SerializeUnparsedDeclaration(ADecl: TUnparsedDeclarationSyntax): System.JSON.TJSONObject;
var
  LToken: TSyntaxToken;
  LArray: TJSONArray;
begin
  if ADecl = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'UnparsedDeclaration');

  if ADecl.Tokens.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in ADecl.Tokens do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('Tokens', LArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeStatement(AStmt: TStatementSyntax): System.JSON.TJSONObject;
begin
  if AStmt = nil then Exit(nil);
  if AStmt is TWhileStatementSyntax then
    Result := SerializeWhileStatement(TWhileStatementSyntax(AStmt))
  else if AStmt is TRepeatStatementSyntax then
    Result := SerializeRepeatStatement(TRepeatStatementSyntax(AStmt))
  else if AStmt is TForStatementSyntax then
    Result := SerializeForStatement(TForStatementSyntax(AStmt))
  else if AStmt is TIfStatementSyntax then
    Result := SerializeIfStatement(TIfStatementSyntax(AStmt))
  else if AStmt is TAssignmentStatementSyntax then
    Result := SerializeAssignmentStatement(TAssignmentStatementSyntax(AStmt))
  else if AStmt is TBeginEndStatementSyntax then
    Result := SerializeBeginEndStatement(TBeginEndStatementSyntax(AStmt))
  else if AStmt is TTryStatementSyntax then
    Result := SerializeTryStatement(TTryStatementSyntax(AStmt))
  else if AStmt is TOpaqueStatementSyntax then
    Result := SerializeOpaqueStatement(TOpaqueStatementSyntax(AStmt))
  else
  begin
    Result := TJSONObject.Create;
    Result.AddPair('NodeType', 'UnknownStatement');
  end;
end;

function TSyntaxTreeSerializer.SerializeWhileStatement(AStmt: TWhileStatementSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
begin
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'WhileStatement');
  Result.AddPair('WhileKeyword', SerializeToken(AStmt.WhileKeyword));
  
  LArray := TJSONArray.Create;
  for LToken in AStmt.ConditionTokens do
    LArray.AddElement(SerializeToken(LToken));
  Result.AddPair('ConditionTokens', LArray);
  
  if Assigned(AStmt.DoKeyword) then
    Result.AddPair('DoKeyword', SerializeToken(AStmt.DoKeyword));
  if Assigned(AStmt.Statement) then
    Result.AddPair('Statement', SerializeStatement(AStmt.Statement));
    
  if AStmt.BodyTokens.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in AStmt.BodyTokens do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('BodyTokens', LArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeRepeatStatement(AStmt: TRepeatStatementSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
  LInnerStmt: TStatementSyntax;
begin
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'RepeatStatement');
  Result.AddPair('RepeatKeyword', SerializeToken(AStmt.RepeatKeyword));
  
  LArray := TJSONArray.Create;
  for LInnerStmt in AStmt.Statements do
    LArray.AddElement(SerializeStatement(LInnerStmt));
  Result.AddPair('Statements', LArray);
  
  if Assigned(AStmt.UntilKeyword) then
    Result.AddPair('UntilKeyword', SerializeToken(AStmt.UntilKeyword));
    
  LArray := TJSONArray.Create;
  for LToken in AStmt.ConditionTokens do
    LArray.AddElement(SerializeToken(LToken));
  Result.AddPair('ConditionTokens', LArray);

  if AStmt.BodyTokens.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in AStmt.BodyTokens do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('BodyTokens', LArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeForStatement(AStmt: TForStatementSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
begin
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'ForStatement');
  Result.AddPair('ForKeyword', SerializeToken(AStmt.ForKeyword));
  
  LArray := TJSONArray.Create;
  for LToken in AStmt.VariableTokens do
    LArray.AddElement(SerializeToken(LToken));
  Result.AddPair('VariableTokens', LArray);
  
  if Assigned(AStmt.AssignmentToken) then
    Result.AddPair('AssignmentToken', SerializeToken(AStmt.AssignmentToken));
    
  LArray := TJSONArray.Create;
  for LToken in AStmt.StartTokens do
    LArray.AddElement(SerializeToken(LToken));
  Result.AddPair('StartTokens', LArray);
  
  if Assigned(AStmt.ToDowntoKeyword) then
    Result.AddPair('ToDowntoKeyword', SerializeToken(AStmt.ToDowntoKeyword));
    
  LArray := TJSONArray.Create;
  for LToken in AStmt.EndTokens do
    LArray.AddElement(SerializeToken(LToken));
  Result.AddPair('EndTokens', LArray);
  
  if Assigned(AStmt.DoKeyword) then
    Result.AddPair('DoKeyword', SerializeToken(AStmt.DoKeyword));
  if Assigned(AStmt.Statement) then
    Result.AddPair('Statement', SerializeStatement(AStmt.Statement));

  if AStmt.BodyTokens.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in AStmt.BodyTokens do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('BodyTokens', LArray);
  end;
end;

function TSyntaxTreeSerializer.SerializeIfStatement(AStmt: TIfStatementSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
begin
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'IfStatement');
  Result.AddPair('IfKeyword', SerializeToken(AStmt.IfKeyword));
  
  LArray := TJSONArray.Create;
  for LToken in AStmt.ConditionTokens do
    LArray.AddElement(SerializeToken(LToken));
  Result.AddPair('ConditionTokens', LArray);
  
  if Assigned(AStmt.ThenKeyword) then
    Result.AddPair('ThenKeyword', SerializeToken(AStmt.ThenKeyword));
  if Assigned(AStmt.ThenStatement) then
    Result.AddPair('ThenStatement', SerializeStatement(AStmt.ThenStatement));
    
  if Assigned(AStmt.ElseKeyword) then
  begin
    Result.AddPair('ElseKeyword', SerializeToken(AStmt.ElseKeyword));
    if Assigned(AStmt.ElseStatement) then
      Result.AddPair('ElseStatement', SerializeStatement(AStmt.ElseStatement));
  end;
end;

function TSyntaxTreeSerializer.SerializeAssignmentStatement(AStmt: TAssignmentStatementSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
begin
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'AssignmentStatement');
  
  LArray := TJSONArray.Create;
  for LToken in AStmt.LeftTokens do
    LArray.AddElement(SerializeToken(LToken));
  Result.AddPair('LeftTokens', LArray);
  
  if Assigned(AStmt.ColonEqualsToken) then
    Result.AddPair('ColonEqualsToken', SerializeToken(AStmt.ColonEqualsToken));
    
  LArray := TJSONArray.Create;
  for LToken in AStmt.RightTokens do
    LArray.AddElement(SerializeToken(LToken));
  Result.AddPair('RightTokens', LArray);
end;

function TSyntaxTreeSerializer.SerializeBeginEndStatement(AStmt: TBeginEndStatementSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LStmt: TStatementSyntax;
begin
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'BeginEndStatement');
  
  if Assigned(AStmt.BeginKeyword) then
    Result.AddPair('BeginKeyword', SerializeToken(AStmt.BeginKeyword));
  
  LArray := TJSONArray.Create;
  for LStmt in AStmt.Statements do
    LArray.AddElement(SerializeStatement(LStmt));
  Result.AddPair('Statements', LArray);
  
  if Assigned(AStmt.EndKeyword) then
    Result.AddPair('EndKeyword', SerializeToken(AStmt.EndKeyword));
    
  if Assigned(AStmt.Semicolon) then
    Result.AddPair('Semicolon', SerializeToken(AStmt.Semicolon));
end;

function TSyntaxTreeSerializer.SerializeTryStatement(AStmt: TTryStatementSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LStmt: TStatementSyntax;
begin
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'TryStatement');

  if Assigned(AStmt.TryKeyword) then
    Result.AddPair('TryKeyword', SerializeToken(AStmt.TryKeyword));

  LArray := TJSONArray.Create;
  for LStmt in AStmt.Statements do
    LArray.AddElement(SerializeStatement(LStmt));
  Result.AddPair('Statements', LArray);

  if Assigned(AStmt.FinallyKeyword) then
    Result.AddPair('FinallyKeyword', SerializeToken(AStmt.FinallyKeyword));

  if Assigned(AStmt.ExceptKeyword) then
    Result.AddPair('ExceptKeyword', SerializeToken(AStmt.ExceptKeyword));

  LArray := TJSONArray.Create;
  for LStmt in AStmt.FinallyExceptStatements do
    LArray.AddElement(SerializeStatement(LStmt));
  Result.AddPair('FinallyExceptStatements', LArray);

  if Assigned(AStmt.EndKeyword) then
    Result.AddPair('EndKeyword', SerializeToken(AStmt.EndKeyword));

  if Assigned(AStmt.Semicolon) then
    Result.AddPair('Semicolon', SerializeToken(AStmt.Semicolon));
end;

function TSyntaxTreeSerializer.SerializeOpaqueStatement(AStmt: TOpaqueStatementSyntax): System.JSON.TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
begin
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'OpaqueStatement');
  LArray := TJSONArray.Create;
  for LToken in AStmt.Tokens do
    LArray.AddElement(SerializeToken(LToken));
  Result.AddPair('Tokens', LArray);
end;

function TSyntaxTreeSerializer.SerializeNode(ANode: TSyntaxNode): TJSONObject;
begin
  if ANode = nil then Exit(nil);
  
  if ANode is TCompilationUnitSyntax then
    Result := SerializeCompilationUnit(TCompilationUnitSyntax(ANode))
  else if ANode is TInterfaceSectionSyntax then
    Result := SerializeInterfaceSection(TInterfaceSectionSyntax(ANode))
  else if ANode is TUnitReferenceSyntax then
    Result := SerializeUnitReference(TUnitReferenceSyntax(ANode))
  else if ANode is TUsesClauseSyntax then
    Result := SerializeUsesClause(TUsesClauseSyntax(ANode))
  else if ANode is TTypeSectionSyntax then
    Result := SerializeTypeSection(TTypeSectionSyntax(ANode))
  else if ANode is TConstSectionSyntax then
    Result := SerializeConstSection(TConstSectionSyntax(ANode))
  else if ANode is TVarSectionSyntax then
    Result := SerializeVarSection(TVarSectionSyntax(ANode))
  else if ANode is TTypeDeclarationSyntax then
    Result := SerializeTypeDeclaration(TTypeDeclarationSyntax(ANode))
  else if ANode is TConstDeclarationSyntax then
    Result := SerializeConstDeclaration(TConstDeclarationSyntax(ANode))
  else if ANode is TVarDeclarationSyntax then
    Result := SerializeVarDeclaration(TVarDeclarationSyntax(ANode))
  else if ANode is TImplementationSectionSyntax then
    Result := SerializeImplementationSection(TImplementationSectionSyntax(ANode))
  else if ANode is TMethodImplementationSyntax then
    Result := SerializeMethodImplementation(TMethodImplementationSyntax(ANode))
  else if ANode is TUnparsedDeclarationSyntax then
    Result := SerializeUnparsedDeclaration(TUnparsedDeclarationSyntax(ANode))
  else
  begin
    Result := TJSONObject.Create;
    Result.AddPair('NodeType', 'UnknownNode');
  end;
end;

end.
