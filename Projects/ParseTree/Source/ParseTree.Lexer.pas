// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit ParseTree.Lexer;

interface

uses

  System.Classes,
  System.Generics.Collections,
  System.SysUtils,

  ParseTree.Core,
  ParseTree.Tokens;

type

  /// <summary>Reads characters from source text and groups them into Tokens and Trivia</summary>
  TParseTreeLexer = class
  private
    FPosition  : Integer;
    FText      : String;
    FTextLength: Integer;

    function  Current: Char;
    procedure Next;
    function  Peek(AOffset: Integer = 0): Char;
    function  ScanComment: TSyntaxTrivia;
    function  ScanIdentifierOrKeyword: TSyntaxToken;
    function  ScanNumericLiteral: TSyntaxToken;
    function  ScanStringLiteral: TSyntaxToken;
    function  ScanWhitespace: TSyntaxTrivia;
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
  StartPos: Integer;
begin
  StartPos := FPosition;
  while (Current = ' ') or (Current = #9) or (Current = #13) or (Current = #10) do
    Next;

  Result := TSyntaxTrivia.Create(Copy(FText, StartPos, FPosition - StartPos));
end;

function TParseTreeLexer.ScanComment: TSyntaxTrivia;
var
  IsMultiLine: Boolean;
  StartPos   : Integer;
begin
  StartPos := FPosition;

  if (Current = '/') and (Peek(1) = '/') then // Single-line comment
  begin
    Next;
    Next;
    while (Current <> #0) and (Current <> #13) and (Current <> #10) do
      Next;
  end
  else if (Current = '{') or ((Current = '(') and (Peek(1) = '*')) then // Multi-line comment
  begin
    IsMultiLine := (Current = '(');
    if IsMultiLine then
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
      if IsMultiLine and (Current = '*') and (Peek(1) = ')') then
      begin
        Next;
        Next;
        Break;
      end
      else if (not IsMultiLine) and (Current = '}') then
      begin
        Next;
        Break;
      end;
      Next;
    end;
  end;

  Result := TSyntaxTrivia.Create(Copy(FText, StartPos, FPosition - StartPos));
end;

function TParseTreeLexer.ScanNumericLiteral: TSyntaxToken;
var
  StartPos: Integer;
begin
  StartPos := FPosition;
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
    if CharInSet(Current, ['E', 'e']) then
    begin
      Next;
      if CharInSet(Current, ['+', '-']) then
        Next;
      while (Current >= '0') and (Current <= '9') do
        Next;
    end;
  end;
  Result := TSyntaxToken.Create(tkNumericLiteral, Copy(FText, StartPos, FPosition - StartPos));
end;

function TParseTreeLexer.ScanStringLiteral: TSyntaxToken;
var
  StartPos: Integer;
begin
  StartPos := FPosition;
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
  Result := TSyntaxToken.Create(tkStringLiteral, Copy(FText, StartPos, FPosition - StartPos));
end;

function TParseTreeLexer.ScanIdentifierOrKeyword: TSyntaxToken;
var
  Kind    : TTokenKind;
  StartPos: Integer;
  Text    : String;
  Upper   : String;
begin
  StartPos := FPosition;
  // Advance past letters, digits, and underscores
  while ((Current >= 'A') and (Current <= 'Z')) or
        ((Current >= 'a') and (Current <= 'z')) or
        ((Current >= '0') and (Current <= '9')) or
        (Current = '_') do
  begin
    Next;
  end;

  Text := Copy(FText, StartPos, FPosition - StartPos);
  Upper := UpperCase(Text);

  // Very basic keyword check. This matches TTokenKind mapping
  if Upper = 'UNIT'                then Kind := tkUnitKeyword
  else if Upper = 'INTERFACE'      then Kind := tkInterfaceKeyword
  else if Upper = 'DISPINTERFACE'  then Kind := tkDispinterfaceKeyword
  else if Upper = 'IMPLEMENTATION' then Kind := tkImplementationKeyword
  else if Upper = 'USES'           then Kind := tkUsesKeyword
  else if Upper = 'IN'             then Kind := tkInKeyword
  else if Upper = 'TYPE'           then Kind := tkTypeKeyword
  else if Upper = 'CONST'          then Kind := tkConstKeyword
  else if Upper = 'VAR'            then Kind := tkVarKeyword
  else if Upper = 'CLASS'          then Kind := tkClassKeyword
  else if Upper = 'RECORD'         then Kind := tkRecordKeyword
  else if Upper = 'BEGIN'          then Kind := tkBeginKeyword
  else if Upper = 'TRY'            then Kind := tkTryKeyword
  else if Upper = 'CASE'           then Kind := tkCaseKeyword
  else if Upper = 'ASM'            then Kind := tkAsmKeyword
  else if Upper = 'END'            then Kind := tkEndKeyword
  else if Upper = 'PRIVATE'        then Kind := tkPrivateKeyword
  else if Upper = 'PROTECTED'      then Kind := tkProtectedKeyword
  else if Upper = 'PUBLIC'         then Kind := tkPublicKeyword
  else if Upper = 'PUBLISHED'      then Kind := tkPublishedKeyword
  else if Upper = 'STRICT'         then Kind := tkStrictKeyword
  else if Upper = 'PROCEDURE'      then Kind := tkProcedureKeyword
  else if Upper = 'FUNCTION'       then Kind := tkFunctionKeyword
  else if Upper = 'CONSTRUCTOR'    then Kind := tkConstructorKeyword
  else if Upper = 'DESTRUCTOR'     then Kind := tkDestructorKeyword
  else if Upper = 'PROPERTY'       then Kind := tkPropertyKeyword
  else if Upper = 'READ'           then Kind := tkReadKeyword
  else if Upper = 'WRITE'          then Kind := tkWriteKeyword
  else if Upper = 'OVERRIDE'       then Kind := tkOverrideKeyword
  else if Upper = 'WHILE'          then Kind := tkWhileKeyword
  else if Upper = 'FOR'            then Kind := tkForKeyword
  else if Upper = 'TO'             then Kind := tkToKeyword
  else if Upper = 'DOWNTO'         then Kind := tkDowntoKeyword
  else if Upper = 'DO'             then Kind := tkDoKeyword
  else if Upper = 'REPEAT'         then Kind := tkRepeatKeyword
  else if Upper = 'UNTIL'          then Kind := tkUntilKeyword
  else if Upper = 'IF'             then Kind := tkIfKeyword
  else if Upper = 'THEN'           then Kind := tkThenKeyword
  else if Upper = 'ELSE'           then Kind := tkElseKeyword
  else if Upper = 'FINALLY'        then Kind := tkFinallyKeyword
  else if Upper = 'EXCEPT'         then Kind := tkExceptKeyword
  else if Upper = 'RAISE'          then Kind := tkRaiseKeyword
  else if Upper = 'OF'             then Kind := tkOfKeyword
  else if Upper = 'WITH'           then Kind := tkWithKeyword
  else if Upper = 'INHERITED'      then Kind := tkInheritedKeyword
  else if Upper = 'EXIT'           then Kind := tkExitKeyword
  else Kind := tkIdentifier;

  Result := TSyntaxToken.Create(Kind, Text);
end;

function TParseTreeLexer.NextToken: TSyntaxToken;
var
  LeadingTrivia: TList<TSyntaxTrivia>;
  TokenText    : String;

  procedure InitLeadingTrivia;
  begin
    if not Assigned(LeadingTrivia) then
      LeadingTrivia := TList<TSyntaxTrivia>.Create;
  end;

begin
  if Current = #0 then
    Exit(TSyntaxToken.Create(tkEOF, ''));

  LeadingTrivia := nil;
  try
    // Scan leading trivia (whitespace and comments)
    while True do
    begin
      var LCurrent: Char := Current;
      if CharInSet(LCurrent, [' ', #9, #13, #10]) then
      begin
        InitLeadingTrivia;
        LeadingTrivia.Add(ScanWhitespace);
      end
      else if (LCurrent = '/') and (Peek(1) = '/') then
      begin
        InitLeadingTrivia;
        LeadingTrivia.Add(ScanComment);
      end
      else if (LCurrent = '{') or ((LCurrent = '(') and (Peek(1) = '*')) then
      begin
        InitLeadingTrivia;
        LeadingTrivia.Add(ScanComment);
      end
      else
        Break;
    end;

    if Current = #0 then
    begin
      Result := TSyntaxToken.Create(tkEOF, '');
      if Assigned(LeadingTrivia) then
        Result.LeadingTrivia.AddRange(LeadingTrivia);
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
      TokenText := Current;
      Next;

      if TokenText = ';' then
        Result := TSyntaxToken.Create(tkSemicolon, TokenText)
      else if TokenText = '.' then
        Result := TSyntaxToken.Create(tkDot, TokenText)
      else if TokenText = ',' then
        Result := TSyntaxToken.Create(tkComma, TokenText)
      else if TokenText = ':' then
      begin
        if Current = '=' then
        begin
          TokenText := TokenText + Current;
          Next;
          Result := TSyntaxToken.Create(tkColonEquals, TokenText);
        end
        else
          Result := TSyntaxToken.Create(tkColon, TokenText);
      end
      else if TokenText = '=' then
        Result := TSyntaxToken.Create(tkEquals, TokenText)
      else if TokenText = '(' then
        Result := TSyntaxToken.Create(tkOpenParen, TokenText)
      else if TokenText = ')' then
        Result := TSyntaxToken.Create(tkCloseParen, TokenText)
      else if TokenText = '[' then
        Result := TSyntaxToken.Create(tkOpenBracket, TokenText)
      else if TokenText = ']' then
        Result := TSyntaxToken.Create(tkCloseBracket, TokenText)
      else if TokenText = '<' then
      begin
        if Current = '>' then
        begin
          TokenText := TokenText + Current;
          Next;
          Result := TSyntaxToken.Create(tkNotEquals, TokenText);
        end
        else if Current = '=' then
        begin
          TokenText := TokenText + Current;
          Next;
          Result := TSyntaxToken.Create(tkLessOrEquals, TokenText);
        end
        else
          Result := TSyntaxToken.Create(tkLessThan, TokenText);
      end
      else if TokenText = '>' then
      begin
        if Current = '=' then
        begin
          TokenText := TokenText + Current;
          Next;
          Result := TSyntaxToken.Create(tkGreaterOrEquals, TokenText);
        end
        else
          Result := TSyntaxToken.Create(tkGreaterThan, TokenText);
      end
      else
        Result := TSyntaxToken.Create(tkUnknown, TokenText);
    end;

    if Assigned(LeadingTrivia) then
      Result.LeadingTrivia.AddRange(LeadingTrivia);
  finally
    LeadingTrivia.Free;
  end;
end;

function TParseTreeLexer.TokenizeAll: TList<TSyntaxToken>;
var
  Token: TSyntaxToken;
  Prev : TSyntaxToken;
begin
  Result := TList<TSyntaxToken>.Create;
  Prev := nil;
  repeat
    Token := NextToken;
    if Assigned(Prev) then
    begin
      Prev.NextToken := Token;
      Token.PrevToken := Prev;
    end;
    Prev := Token;
    Result.Add(Token);
  until Token.Kind = tkEOF;
end;

end.
