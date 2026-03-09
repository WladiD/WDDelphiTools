unit ParseTree.Lexer;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, ParseTree.Core, ParseTree.Tokens;

type
  { Reads characters from source text and groups them into Tokens and Trivia }
  TParseTreeLexer = class
  private
    FText: string;
    FPosition: Integer;
    FTextLength: Integer;

    function Peek(AOffset: Integer = 0): Char;
    function Current: Char;
    procedure Next;
    function ScanIdentifierOrKeyword: TSyntaxToken;
    function ScanNumericLiteral: TSyntaxToken;
    function ScanStringLiteral: TSyntaxToken;
    function ScanWhitespace: TSyntaxTrivia;
    function ScanComment: TSyntaxTrivia;
  public
    constructor Create(const AText: string);
    function NextToken: TSyntaxToken;
    function TokenizeAll: TList<TSyntaxToken>;
  end;

implementation

{ TParseTreeLexer }

constructor TParseTreeLexer.Create(const AText: string);
begin
  FText := AText;
  FTextLength := Length(FText);
  // Delphi strings are 1-based, we'll use 1-based index
  FPosition := 1;
end;

function TParseTreeLexer.Peek(AOffset: Integer): Char;
var
  LIndex: Integer;
begin
  LIndex := FPosition + AOffset;
  if (LIndex >= 1) and (LIndex <= FTextLength) then
    Result := FText[LIndex]
  else
    Result := #0;
end;

function TParseTreeLexer.Current: Char;
begin
  Result := Peek(0);
end;

procedure TParseTreeLexer.Next;
begin
  Inc(FPosition);
end;

function TParseTreeLexer.ScanWhitespace: TSyntaxTrivia;
var
  LStartPos: Integer;
begin
  LStartPos := FPosition;
  while (Current = ' ') or (Current = #9) or (Current = #13) or (Current = #10) do
    Next;

  Result := TSyntaxTrivia.Create(Copy(FText, LStartPos, FPosition - LStartPos));
end;

function TParseTreeLexer.ScanComment: TSyntaxTrivia;
var
  LStartPos: Integer;
  LIsMultiLine: Boolean;
begin
  LStartPos := FPosition;
  
  if (Current = '/') and (Peek(1) = '/') then
  begin
    // Single-line comment
    Next;
    Next;
    while (Current <> #0) and (Current <> #13) and (Current <> #10) do
      Next;
  end
  else if (Current = '{') or ((Current = '(') and (Peek(1) = '*')) then
  begin
    // Multi-line comment
    LIsMultiLine := (Current = '(');
    if LIsMultiLine then
    begin
      Next; // '('
      Next; // '*'
    end
    else
    begin
      Next; // '{'
    end;

    while Current <> #0 do
    begin
      if LIsMultiLine and (Current = '*') and (Peek(1) = ')') then
      begin
        Next;
        Next;
        Break;
      end
      else if (not LIsMultiLine) and (Current = '}') then
      begin
        Next;
        Break;
      end;
      Next;
    end;
  end;

  Result := TSyntaxTrivia.Create(Copy(FText, LStartPos, FPosition - LStartPos));
end;

function TParseTreeLexer.ScanNumericLiteral: TSyntaxToken;
var
  LStartPos: Integer;
begin
  LStartPos := FPosition;
  if Current = '$' then
  begin
    Next;
    while ((Current >= '0') and (Current <= '9')) or
          ((Current >= 'A') and (Current <= 'F')) or
          ((Current >= 'a') and (Current <= 'f')) do
      Next;
  end
  else
  begin
    while (Current >= '0') and (Current <= '9') do
      Next;
    if Current = '.' then
    begin
      if Peek(1) <> '.' then
      begin
        Next;
        while (Current >= '0') and (Current <= '9') do
          Next;
      end;
    end;
    if (Current = 'E') or (Current = 'e') then
    begin
      Next;
      if (Current = '+') or (Current = '-') then Next;
      while (Current >= '0') and (Current <= '9') do
        Next;
    end;
  end;
  Result := TSyntaxToken.Create(tkNumericLiteral, Copy(FText, LStartPos, FPosition - LStartPos));
end;

function TParseTreeLexer.ScanStringLiteral: TSyntaxToken;
var
  LStartPos: Integer;
begin
  LStartPos := FPosition;
  Next; // consume opening quote
  while (Current <> #0) do
  begin
    if Current = '''' then
    begin
      Next; // consume quote
      if Current = '''' then
        Next // escaped quote, keep going
      else
        Break; // end of string
    end
    else
      Next;
  end;
  Result := TSyntaxToken.Create(tkStringLiteral, Copy(FText, LStartPos, FPosition - LStartPos));
end;

function TParseTreeLexer.ScanIdentifierOrKeyword: TSyntaxToken;
var
  LStartPos: Integer;
  LText: string;
  LKind: TTokenKind;
  LUpper: string;
begin
  LStartPos := FPosition;
  // Advance past letters, digits, and underscores
  while ((Current >= 'A') and (Current <= 'Z')) or
        ((Current >= 'a') and (Current <= 'z')) or
        ((Current >= '0') and (Current <= '9')) or
        (Current = '_') do
  begin
    Next;
  end;

  LText := Copy(FText, LStartPos, FPosition - LStartPos);
  LUpper := UpperCase(LText);
  
  // Very basic keyword check. This matches TTokenKind mapping
  if LUpper = 'UNIT' then LKind := tkUnitKeyword
  else if LUpper = 'INTERFACE' then LKind := tkInterfaceKeyword
  else if LUpper = 'DISPINTERFACE' then LKind := tkDispinterfaceKeyword
  else if LUpper = 'IMPLEMENTATION' then LKind := tkImplementationKeyword
  else if LUpper = 'USES' then LKind := tkUsesKeyword
  else if LUpper = 'IN' then LKind := tkInKeyword
  else if LUpper = 'TYPE' then LKind := tkTypeKeyword
  else if LUpper = 'CONST' then LKind := tkConstKeyword
  else if LUpper = 'VAR' then LKind := tkVarKeyword
  else if LUpper = 'CLASS' then LKind := tkClassKeyword
  else if LUpper = 'RECORD' then LKind := tkRecordKeyword
  else if LUpper = 'BEGIN' then LKind := tkBeginKeyword
  else if LUpper = 'TRY' then LKind := tkTryKeyword
  else if LUpper = 'CASE' then LKind := tkCaseKeyword
  else if LUpper = 'ASM' then LKind := tkAsmKeyword
  else if LUpper = 'END' then LKind := tkEndKeyword
  else if LUpper = 'PRIVATE' then LKind := tkPrivateKeyword
  else if LUpper = 'PROTECTED' then LKind := tkProtectedKeyword
  else if LUpper = 'PUBLIC' then LKind := tkPublicKeyword
  else if LUpper = 'PUBLISHED' then LKind := tkPublishedKeyword
  else if LUpper = 'STRICT' then LKind := tkStrictKeyword
  else if LUpper = 'PROCEDURE' then LKind := tkProcedureKeyword
  else if LUpper = 'FUNCTION' then LKind := tkFunctionKeyword
  else if LUpper = 'CONSTRUCTOR' then LKind := tkConstructorKeyword
  else if LUpper = 'DESTRUCTOR' then LKind := tkDestructorKeyword
  else if LUpper = 'PROPERTY' then LKind := tkPropertyKeyword
  else if LUpper = 'READ' then LKind := tkReadKeyword
  else if LUpper = 'WRITE' then LKind := tkWriteKeyword
  else if LUpper = 'OVERRIDE' then LKind := tkOverrideKeyword
  else if LUpper = 'WHILE' then LKind := tkWhileKeyword
  else if LUpper = 'FOR' then LKind := tkForKeyword
  else if LUpper = 'TO' then LKind := tkToKeyword
  else if LUpper = 'DOWNTO' then LKind := tkDowntoKeyword
  else if LUpper = 'DO' then LKind := tkDoKeyword
  else if LUpper = 'REPEAT' then LKind := tkRepeatKeyword
  else if LUpper = 'UNTIL' then LKind := tkUntilKeyword
  else if LUpper = 'IF' then LKind := tkIfKeyword
  else if LUpper = 'THEN' then LKind := tkThenKeyword
  else if LUpper = 'ELSE' then LKind := tkElseKeyword
  else if LUpper = 'FINALLY' then LKind := tkFinallyKeyword
  else if LUpper = 'EXCEPT' then LKind := tkExceptKeyword
  else if LUpper = 'RAISE' then LKind := tkRaiseKeyword
  else if LUpper = 'OF' then LKind := tkOfKeyword
  else if LUpper = 'WITH' then LKind := tkWithKeyword
  else if LUpper = 'INHERITED' then LKind := tkInheritedKeyword
  else if LUpper = 'EXIT' then LKind := tkExitKeyword
  else LKind := tkIdentifier;

  Result := TSyntaxToken.Create(LKind, LText);
end;

function TParseTreeLexer.NextToken: TSyntaxToken;
var
  LLeadingTrivia: TList<TSyntaxTrivia>;
  LTrailingTrivia: TList<TSyntaxTrivia>;
  LTokenText: string;
begin
  if Current = #0 then
    Exit(TSyntaxToken.Create(tkEOF, ''));

  LLeadingTrivia := TList<TSyntaxTrivia>.Create;
  try
    // Scan leading trivia (whitespace and comments)
    while True do
    begin
      if (Current = ' ') or (Current = #9) or (Current = #13) or (Current = #10) then
        LLeadingTrivia.Add(ScanWhitespace)
      else if (Current = '/') and (Peek(1) = '/') then
        LLeadingTrivia.Add(ScanComment)
      else if (Current = '{') or ((Current = '(') and (Peek(1) = '*')) then
        LLeadingTrivia.Add(ScanComment)
      else
        Break;
    end;

    if Current = #0 then
    begin
      Result := TSyntaxToken.Create(tkEOF, '');
      Result.LeadingTrivia.AddRange(LLeadingTrivia);
      Exit;
    end;

    // Scan actual token
    if ((Current >= 'A') and (Current <= 'Z')) or
       ((Current >= 'a') and (Current <= 'z')) or
       (Current = '_') then
    begin
      Result := ScanIdentifierOrKeyword;
    end
    else if ((Current >= '0') and (Current <= '9')) or (Current = '$') then
    begin
      Result := ScanNumericLiteral;
    end
    else if Current = '''' then // String literal start
    begin
      Result := ScanStringLiteral;
    end
    else
    begin
      // Fallback: Just consume one char as some punctuation
      LTokenText := Current;
      Next;
      
      if LTokenText = ';' then
        Result := TSyntaxToken.Create(tkSemicolon, LTokenText)
      else if LTokenText = '.' then
        Result := TSyntaxToken.Create(tkDot, LTokenText)
      else if LTokenText = ',' then
        Result := TSyntaxToken.Create(tkComma, LTokenText)
      else if LTokenText = ':' then
      begin
        if Current = '=' then
        begin
          LTokenText := LTokenText + Current;
          Next;
          Result := TSyntaxToken.Create(tkColonEquals, LTokenText);
        end
        else
          Result := TSyntaxToken.Create(tkColon, LTokenText);
      end
      else if LTokenText = '=' then
        Result := TSyntaxToken.Create(tkEquals, LTokenText)
      else if LTokenText = '(' then
        Result := TSyntaxToken.Create(tkOpenParen, LTokenText)
      else if LTokenText = ')' then
        Result := TSyntaxToken.Create(tkCloseParen, LTokenText)
      else if LTokenText = '[' then
        Result := TSyntaxToken.Create(tkOpenBracket, LTokenText)
      else if LTokenText = ']' then
        Result := TSyntaxToken.Create(tkCloseBracket, LTokenText)
      else if LTokenText = '<' then
      begin
        if Current = '>' then
        begin
          LTokenText := LTokenText + Current;
          Next;
          Result := TSyntaxToken.Create(tkNotEquals, LTokenText);
        end
        else if Current = '=' then
        begin
          LTokenText := LTokenText + Current;
          Next;
          Result := TSyntaxToken.Create(tkLessOrEquals, LTokenText);
        end
        else
          Result := TSyntaxToken.Create(tkLessThan, LTokenText);
      end
      else if LTokenText = '>' then
      begin
        if Current = '=' then
        begin
          LTokenText := LTokenText + Current;
          Next;
          Result := TSyntaxToken.Create(tkGreaterOrEquals, LTokenText);
        end
        else
          Result := TSyntaxToken.Create(tkGreaterThan, LTokenText);
      end
      else
        Result := TSyntaxToken.Create(tkUnknown, LTokenText);
    end;

    Result.LeadingTrivia.AddRange(LLeadingTrivia);

  finally
    LLeadingTrivia.Free;
  end;
end;

function TParseTreeLexer.TokenizeAll: TList<TSyntaxToken>;
var
  LToken: TSyntaxToken;
begin
  Result := TList<TSyntaxToken>.Create;
  repeat
    LToken := NextToken;
    Result.Add(LToken);
  until LToken.Kind = tkEOF;
end;

end.
