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
  Result.AddPair('Kind', TTokenKind(AToken.Kind).ToString);
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

function TSyntaxTreeSerializer.SerializeUsesClause(ANode: TUsesClauseSyntax): TJSONObject;
var
  LArray: TJSONArray;
  LToken: TSyntaxToken;
begin
  if ANode = nil then Exit(nil);
  Result := TJSONObject.Create;
  Result.AddPair('NodeType', 'UsesClause');
  Result.AddPair('UsesKeyword', SerializeToken(ANode.UsesKeyword));
  
  if ANode.Identifiers.Count > 0 then
  begin
    LArray := TJSONArray.Create;
    for LToken in ANode.Identifiers do
      LArray.AddElement(SerializeToken(LToken));
    Result.AddPair('Identifiers', LArray);
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
  else if ANode is TUsesClauseSyntax then
    Result := SerializeUsesClause(TUsesClauseSyntax(ANode))
  else
  begin
    Result := TJSONObject.Create;
    Result.AddPair('NodeType', 'UnknownNode');
  end;
end;

end.
