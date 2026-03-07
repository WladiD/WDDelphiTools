unit ParseTree.Parser;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, 
  ParseTree.Core, ParseTree.Nodes, ParseTree.Tokens, ParseTree.Lexer;

type
  { TParseTreeParser is the main entry point to parse Pascal source code into a CST }
  TParseTreeParser = class
  public
    function Parse(const AText: string): TCompilationUnitSyntax;
  end;

implementation

{ TParseTreeParser }

function TParseTreeParser.Parse(const AText: string): TCompilationUnitSyntax;
var
  LLexer: TParseTreeLexer;
  LTokens: TList<TSyntaxToken>;
begin
  LLexer := TParseTreeLexer.Create(AText);
  try
    // For now, let's just tokenize everything to ensure the Lexer works 
    // without throwing exceptions on actual projects.
    LTokens := LLexer.TokenizeAll;
    try
      // TODO: Actually construct CST from Tokens
    finally
      LTokens.Free;
    end;
  finally
    LLexer.Free;
  end;
  
  // Return an empty node to satisfy the skeleton.
  Result := TCompilationUnitSyntax.Create;
end;

end.
