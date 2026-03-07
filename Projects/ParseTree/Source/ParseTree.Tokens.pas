unit ParseTree.Tokens;

interface

type
  { Defines the different types of tokens in the language }
  TTokenKind = (
    tkUnknown,
    tkEOF,
    tkUnitKeyword,
    tkInterfaceKeyword,
    tkImplementationKeyword,
    tkUsesKeyword,
    tkTypeKeyword,
    tkConstKeyword,
    tkVarKeyword,
    tkInKeyword,
    tkClassKeyword,
    tkBeginKeyword,
    tkTryKeyword,
    tkCaseKeyword,
    tkAsmKeyword,
    tkEndKeyword,
    tkPrivateKeyword,
    tkProtectedKeyword,
    tkPublicKeyword,
    tkPublishedKeyword,
    tkStrictKeyword,
    tkProcedureKeyword,
    tkFunctionKeyword,
    tkConstructorKeyword,
    tkDestructorKeyword,
    tkPropertyKeyword,
    tkReadKeyword,
    tkWriteKeyword,
    tkOverrideKeyword,
    tkIdentifier,
    tkNumericLiteral,
    tkStringLiteral,
    tkSemicolon,
    tkDot,
    tkComma,
    tkColon,
    tkEquals,
    tkOpenParen,
    tkCloseParen,
    tkOpenBracket,
    tkCloseBracket,
    tkLessThan,
    tkGreaterThan
  );

  TTokenKindHelper = record helper for TTokenKind
    function ToString: string;
  end;

implementation

uses
  System.SysUtils, System.TypInfo;

{ TTokenKindHelper }

function TTokenKindHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TTokenKind), Integer(Self));
end;

end.
