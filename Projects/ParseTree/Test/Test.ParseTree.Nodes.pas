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
  LSerializer: TSyntaxTreeSerializer;
  LJsonObj: System.JSON.TJSONObject;
  LJson: string;
begin
  LTree := TCompilationUnitSyntax.Create;
  try
    LTree.UnitKeyword := TSyntaxToken.Create(Integer(tkUnitKeyword), 'unit');
    LTree.UnitKeyword.TrailingTrivia.Add(TSyntaxTrivia.Create(' '));
    LTree.Identifier := TSyntaxToken.Create(Integer(tkIdentifier), 'Unit1');
    LTree.Semicolon := TSyntaxToken.Create(Integer(tkSemicolon), ';');
    LTree.Semicolon.TrailingTrivia.Add(TSyntaxTrivia.Create(#13#10));
    LTree.Semicolon.TrailingTrivia.Add(TSyntaxTrivia.Create(#13#10));
    LTree.InterfaceSection := TInterfaceSectionSyntax.Create;
    LTree.InterfaceSection.InterfaceKeyword := TSyntaxToken.Create(Integer(tkInterfaceKeyword), 'interface');
    LTree.InterfaceSection.InterfaceKeyword.TrailingTrivia.Add(TSyntaxTrivia.Create(#13#10));
    LTree.InterfaceSection.UsesClause := TUsesClauseSyntax.Create;
    LTree.InterfaceSection.UsesClause.UsesKeyword := TSyntaxToken.Create(Integer(tkUsesKeyword), 'uses');
    LTree.InterfaceSection.UsesClause.UsesKeyword.TrailingTrivia.Add(TSyntaxTrivia.Create(' '));
    LTree.InterfaceSection.UsesClause.Identifiers.Add(TSyntaxToken.Create(Integer(tkIdentifier), 'System.SysUtils'));
    LTree.InterfaceSection.UsesClause.Semicolon := TSyntaxToken.Create(Integer(tkSemicolon), ';');

    LSerializer := TSyntaxTreeSerializer.Create;
    try
      LJsonObj := LSerializer.SerializeNode(LTree);
      if Assigned(LJsonObj) then
      begin
        LJson := LJsonObj.ToJSON;
        Assert.IsTrue(LJson.Contains('"NodeType":"CompilationUnit"'));
        Assert.IsTrue(LJson.Contains('"Kind":"tkUnitKeyword"'));
        Assert.IsTrue(LJson.Contains('"Text":"Unit1"'));
        Assert.IsTrue(LJson.Contains('"Text":"System.SysUtils"'));
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
