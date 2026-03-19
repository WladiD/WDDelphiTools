// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit ParseTree.Tokens;

interface

type

  /// <summary>Defines the different types of tokens in the language</summary>
  TTokenKind = (
    tkUnknown,
    tkEOF,
    tkUnitKeyword,
    tkInterfaceKeyword,
    tkDispinterfaceKeyword,
    tkImplementationKeyword,
    tkUsesKeyword,
    tkTypeKeyword,
    tkConstKeyword,
    tkVarKeyword,
    tkInKeyword,
    tkClassKeyword,
    tkRecordKeyword,
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
    tkWhileKeyword,
    tkForKeyword,
    tkToKeyword,
    tkDowntoKeyword,
    tkDoKeyword,
    tkRepeatKeyword,
    tkUntilKeyword,
    tkIfKeyword,
    tkThenKeyword,
    tkElseKeyword,
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
    tkGreaterThan,
    tkLessOrEquals,
    tkGreaterOrEquals,
    tkNotEquals,
    tkColonEquals,
    tkFinallyKeyword,
    tkExceptKeyword,
    tkRaiseKeyword,
    tkOfKeyword,
    tkWithKeyword,
    tkInheritedKeyword,
    tkExitKeyword
  );

  TTokenKindHelper = record helper for TTokenKind
    function ToString: string;
  end;

implementation

uses

  System.SysUtils,
  System.TypInfo;

{ TTokenKindHelper }

function TTokenKindHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TTokenKind), Integer(Self));
end;

end.
