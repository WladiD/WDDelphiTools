unit ParseTree.Tokens;

interface

type
  { Defines the different types of tokens in the language }
  TTokenKind = (
    tkUnknown,
    tkUnitKeyword,
    tkInterfaceKeyword,
    tkUsesKeyword,
    tkIdentifier,
    tkSemicolon
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
