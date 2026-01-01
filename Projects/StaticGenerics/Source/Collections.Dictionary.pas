unit Collections.Dictionary;

{ ======================================================================= }
interface
{ ======================================================================= }

uses

  System.Classes,
  System.SysUtils,

  mormot.core.base,
  mormot.core.data,
  mormot.core.json,
  mormot.core.os,
  mormot.core.rtti,

  Collections.Interfaces,
  System.Collections.Interfaces;

{ ----------------------------------------------------------------------- }

type

  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  CDictionaryBase = class(TInterfacedObject, IDictionary)
   protected type

    CCollection = class(TInterfacedObject)
     protected
      FSynDict: TSynDictionary;
     public
      constructor Create(ASynDict: TSynDictionary); virtual;
    end;

    CEnumerator = class(CCollection)
     protected
      FIndex: Integer;
     public
      constructor Create(ASynDict: TSynDictionary); override;
      function MoveNext: Boolean;
    end;

   protected
    FSynDict: TSynDictionary;
   protected // IDictionary member
    function  Any: Boolean;
    procedure Clear;
    function  GetCount: Integer;
    function  GetSynDict: TSynDictionary;
    function  IsEmpty: Boolean;
   public
    destructor Destroy; override;
  end;

{ ======================================================================= }
implementation
{ ======================================================================= }

{ ======================================================================= }
{ CDictionaryBase                                                         }
{ ======================================================================= }

destructor CDictionaryBase.Destroy;
begin
  FSynDict.Free;
  inherited;
end;

{ ----------------------------------------------------------------------- }

function CDictionaryBase.Any: Boolean;
begin
  Result:=FSynDict.Count>0;
end;

{ ----------------------------------------------------------------------- }

procedure CDictionaryBase.Clear;
begin
  FSynDict.DeleteAll;
end;

{ ----------------------------------------------------------------------- }

function CDictionaryBase.GetCount: Integer;
begin
  Result:=FSynDict.Count;
end;

{ ----------------------------------------------------------------------- }

function CDictionaryBase.GetSynDict: TSynDictionary;
begin
  Result:=FSynDict;
end;

{ ----------------------------------------------------------------------- }

function CDictionaryBase.IsEmpty: Boolean;
begin
  Result:=FSynDict.Count=0;
end;

{ ======================================================================= }
{ CDictionaryBase.CCollection                                             }
{ ======================================================================= }

constructor CDictionaryBase.CCollection.Create(ASynDict: TSynDictionary);
begin
  FSynDict:=ASynDict;
end;

{ ======================================================================= }
{ TDictionaryBase.CEnumerator                                             }
{ ======================================================================= }

constructor CDictionaryBase.CEnumerator.Create(ASynDict: TSynDictionary);
begin
  inherited;
  FIndex:=-1;
end;

{ ----------------------------------------------------------------------- }

function CDictionaryBase.CEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result:=FIndex<FSynDict.Count;
end;

{ ======================================================================= }

end.
