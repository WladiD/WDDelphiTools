// START: STYLE-TEMPLATE===========================================================================
// ======================================================================
//
// TaifunUnitStyle - Kurzbeschreibung der Unit
//
// Autor: Auflistung von Hauptverantwortlichen
//
// ======================================================================

{$I Tfw.Define.pas}

unit TaifunUnitStyle;

{ ======================================================================= }
interface
{ ======================================================================= }

uses

  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  Winapi.Messages,

  Spring.Collections,
  VirtualTrees,

  Base.Cmd,
  Base.Types,
  Base.Utils.Msg,

  Base.UI.Icons,

  Business.Cty.Typ,

  Business.UI.Bank.Utils,

  Dms.Shared.App.Utils,
  Dms.Shared.Kw.Tree,

  Tfw.Ad.Typ,
  Tfw.VBh.Typ;

{ ----------------------------------------------------------------------- }

type

  TMyVariant1 = packed record
    SpecName : S_50;
    SpecValue: Integer;
  end;

  TMyVariant2 = packed record
    SpecName : S_50;
    SpecValue: S_255;
  end;

  TMyRecord = packed record
    GUID          : TGUID;
    Var1          : Integer;
    Var2          : TDate;
    Filler1       : Array[1..300] of Byte;
    case Byte of
      0: (Filler2 : Array[1..4096] of Byte);
      1: (Variant1: TMyVariant1);
      2: (Variant2: TMyVariant2);
  end;

  CSomeClass = class
   strict private
    FText: String;
    FValue: Word;
    FVarA: Integer;
    function  GetVarA: Integer;
    procedure SetVarA(AValue: Integer);
   strict private class var
    FClassVar: String;
   strict protected
    procedure DoSomething;
    function  IsSomething: Boolean;
   public
    constructor Create;
    destructor  Destroy; override;
    function  CalcValue: Word;
    procedure CalcVar1;
    property  Value: Word read FValue;
    property  VarA: Integer read GetVarA write SetVarA;
   published
    AnotherComponent: TComponent;
    SomeComponent: TComponent;
   public
    class procedure Init;
  end;

{ ======================================================================= }
implementation
{ ======================================================================= }

uses

  System.Math,

  Base.Utils.Str;

{ ----------------------------------------------------------------------- }

{ ======================================================================= }
{ CSomeClass                                                              }
{ ======================================================================= }

constructor CSomeClass.Create;
begin
  FText:='Initial value';
end;

{ ----------------------------------------------------------------------- }

destructor CSomeClass.Destroy;
begin
  if IsSomething
    then DoSomething;
  inherited;
end;

{ ----------------------------------------------------------------------- }

function CSomeClass.CalcValue: Word;
begin
  Result:=FValue;
  Result:=Result*2+150-10;
end;

{ ----------------------------------------------------------------------- }

procedure CSomeClass.CalcVar1;
var
  Age         : Byte;
  Name        : String;
  PlaceOfBirth: String;
  XyChromosome: Boolean;
begin

end;

{ ----------------------------------------------------------------------- }

procedure CSomeClass.DoSomething;
begin

end;


{ ----------------------------------------------------------------------- }

function CSomeClass.GetVarA: Integer;
begin
  Result:=FVarA;
end;

{ ----------------------------------------------------------------------- }

class procedure CSomeClass.Init;
begin

end;

{ ----------------------------------------------------------------------- }

function CSomeClass.IsSomething: Boolean;
begin
  Result:=true;
end;

{ ----------------------------------------------------------------------- }

procedure CSomeClass.SetVarA(AValue: Integer);
begin
  FVarA:=AValue;
end;

{ ======================================================================= }

end.
