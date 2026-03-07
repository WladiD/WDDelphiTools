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
    function SerializeUsesClause(ANode: TUsesClauseSyntax): TJSONObject;
    function SerializeInterfaceSection(ANode: TInterfaceSectionSyntax): TJSONObject;
    function SerializeCompilationUnit(ANode: TCompilationUnitSyntax): TJSONObject;
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

function TSyntaxTreeSerializer.SerializeUsesClause(ANode: TUsesClauseSyntax): TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
  LRef: TUnitReferenceSyntax;
begin
  if ANode = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'UsesClause');
  Result.AddPair('UsesKeyword', SerializeToken(ANode.UsesKeyword));
  
  if ANode.UnitReferences.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LRef in ANode.UnitReferences do
      LArray.AddElement(SerializeUnitReference(LRef));
    Result.AddPair('UnitReferences', LArray);
  end;

  if ANode.Commas.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in ANode.Commas do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('Commas', LArray);
  end;
  
  Result.AddPair('Semicolon', SerializeToken(ANode.Semicolon));
end;

function TSyntaxTreeSerializer.SerializeInterfaceSection(ANode: TInterfaceSectionSyntax): TJSONObject;
begin
  if ANode = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'InterfaceSection');
  Result.AddPair('InterfaceKeyword', SerializeToken(ANode.InterfaceKeyword));
  if Assigned(ANode.UsesClause) then
    Result.AddPair('UsesClause', SerializeUsesClause(ANode.UsesClause));
end;

function TSyntaxTreeSerializer.SerializeCompilationUnit(ANode: TCompilationUnitSyntax): TJSONObject;
begin
  if ANode = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'CompilationUnit');
  Result.AddPair('UnitKeyword', SerializeToken(ANode.UnitKeyword));
  Result.AddPair('Identifier', SerializeToken(ANode.Identifier));
  Result.AddPair('Semicolon', SerializeToken(ANode.Semicolon));
  
  if Assigned(ANode.InterfaceSection) then
    Result.AddPair('InterfaceSection', SerializeInterfaceSection(ANode.InterfaceSection));
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
  else
  begin
    Result := TJSONObject.Create;
    Result.AddPair('NodeType', 'UnknownNode');
  end;
end;

end.
