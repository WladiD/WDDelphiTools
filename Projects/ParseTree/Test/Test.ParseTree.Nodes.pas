unit Test.ParseTree.Nodes;

interface

uses
  System.JSON,
  DUnitX.TestFramework;

type
  [TestFixture]
  TParseTreeNodesTest = class
  public
    [Test]
    procedure TestCompilationUnitSerialization;
  end;

implementation

uses
  System.SysUtils,
  ParseTree.Core,
  ParseTree.Tokens,
  ParseTree.Nodes,
  ParseTree.Serializer;

procedure TParseTreeNodesTest.TestCompilationUnitSerialization;
var
  LTree: TCompilationUnitSyntax;
  LUnitRef: TUnitReferenceSyntax;
  LSerializer: TSyntaxTreeSerializer;
  LJsonObj: System.JSON.TJSONObject;
  LJson: string;
begin
  LTree := TCompilationUnitSyntax.Create;
  try
    LTree.UnitKeyword := TSyntaxToken.Create(tkUnitKeyword, 'unit');
    LTree.UnitKeyword.TrailingTrivia.Add(TSyntaxTrivia.Create(' '));
    LTree.Identifier := TSyntaxToken.Create(tkIdentifier, 'Unit1');
    LTree.Semicolon := TSyntaxToken.Create(tkSemicolon, ';');
    LTree.Semicolon.TrailingTrivia.Add(TSyntaxTrivia.Create(#13#10));
    LTree.Semicolon.TrailingTrivia.Add(TSyntaxTrivia.Create(#13#10));
    LTree.InterfaceSection := TInterfaceSectionSyntax.Create;
    LTree.InterfaceSection.InterfaceKeyword := TSyntaxToken.Create(tkInterfaceKeyword, 'interface');
    LTree.InterfaceSection.InterfaceKeyword.TrailingTrivia.Add(TSyntaxTrivia.Create(#13#10));
    LTree.InterfaceSection.UsesClause := TUsesClauseSyntax.Create;
    LTree.InterfaceSection.UsesClause.UsesKeyword := TSyntaxToken.Create(tkUsesKeyword, 'uses');
    LTree.InterfaceSection.UsesClause.UsesKeyword.TrailingTrivia.Add(TSyntaxTrivia.Create(' '));
    
    LUnitRef := TUnitReferenceSyntax.Create;
    LUnitRef.Namespaces.Add(TSyntaxToken.Create(tkIdentifier, 'System'));
    LUnitRef.Dots.Add(TSyntaxToken.Create(tkDot, '.'));
    LUnitRef.Namespaces.Add(TSyntaxToken.Create(tkIdentifier, 'SysUtils'));
    LTree.InterfaceSection.UsesClause.UnitReferences.Add(LUnitRef);
    
    LTree.InterfaceSection.UsesClause.Semicolon := TSyntaxToken.Create(tkSemicolon, ';');

    LSerializer := TSyntaxTreeSerializer.Create;
    try
      LJsonObj := LSerializer.SerializeNode(LTree);
      if Assigned(LJsonObj) then
      begin
        LJson := LJsonObj.ToJSON;
        Assert.IsTrue(LJson.Contains('"NodeType":"CompilationUnit"'));
        Assert.IsTrue(LJson.Contains('"Kind":"tkUnitKeyword"'));
        Assert.IsTrue(LJson.Contains('"Text":"Unit1"'));
        Assert.IsTrue(LJson.Contains('"Text":"System"'));
        Assert.IsTrue(LJson.Contains('"Text":"SysUtils"'));
        LJsonObj.Free;
      end;
    finally
      LSerializer.Free;
    end;
  finally
    LTree.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeNodesTest);

end.
