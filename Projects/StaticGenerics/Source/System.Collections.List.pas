// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit System.Collections.List;

interface

uses

  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.Types,

  mormot.core.base,
  mormot.core.collections,
  mormot.core.data,

  Collections.Interfaces,
  System.Collections.Interfaces;

type

  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  CObjectList = class(TObjectList, IInterface, IList_TObject, IEnumerable_TObject)
   strict private
    FEnumerableIntfGuid: TGUID;
    FListIntfGuid      : TGUID;
    FRefCountIntf      : IInterface;
   strict protected  // IInterface member
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
   strict protected // IList member
    function  Any: Boolean;
    procedure DeleteRange(AIndex, ACount: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function  GetCapacity: Integer;
    function  GetCount: Integer;
    function  IsEmpty: Boolean;
    procedure Reverse;
    procedure SetCapacity(AValue: Integer);
    procedure Sort(ACompare: TOnDynArraySortCompare);
   strict protected // IList_TObject member
    function  Add(const AItem: TObject): Integer;
    procedure AddRange(const AValues: Array of TObject);
    function  All(const APredicate: TPredicate_TObject): Boolean;
    function  Concat(const ASecond: IEnumerable_TObject): IList_TObject;
    function  Contains(const AValue: TObject): Boolean;
    function  Extract(const AItem: TObject): TObject;
    function  First: TObject;
    function  FirstOrDefault: TObject;
    function  GetEnumerator: IEnumerator_TObject;
    function  GetItem(AIndex: Integer): TObject;
    function  GetRange(AIndex, ACount: Integer): IList_TObject;
    function  IndexOf(const AItem: TObject): Integer;
    procedure Insert(AIndex: Integer; const AItem: TObject);
    function  Last: TObject;
    function  LastOrDefault: TObject;
    function  Remove(const AItem: TObject): Boolean;
    function  RemoveAll(const APredicate: TPredicate_TObject): Integer;
    procedure SetItem(AIndex: Integer; const AValue: TObject);
    function  ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TObject>;
    function  Where(const APredicate: TPredicate_TObject): IEnumerable_TObject;
   public
    constructor Create(AOwnsObjects: Boolean); overload;
    constructor Create(AOwnsObjects: Boolean; const AListIntfGuid: TGUID; const AEnumerableIntfGuid: TGUID); overload;
  end;

  CObjectListEnumerator = class(TInterfacedObject, IEnumerator_TObject)
   strict private
    FIndex: Integer;
    FList : CObjectList;
   protected // IEnumerator_TObject
    function GetCurrent: TObject;
    function MoveNext: Boolean;
   public
    constructor Create(AList: CObjectList);
  end;

implementation

{ CObjectList }

constructor CObjectList.Create(AOwnsObjects: Boolean; const AListIntfGuid: TGUID; const AEnumerableIntfGuid: TGUID);
begin
  FRefCountIntf := TInterfacedObject.Create;
  FListIntfGuid := AListIntfGuid;
  FEnumerableIntfGuid := AEnumerableIntfGuid;
  inherited Create(AOwnsObjects);
end;

constructor CObjectList.Create(AOwnsObjects: Boolean);
begin
  Create(AOwnsObjects, IList_TObject, IEnumerable_TObject);
end;

function CObjectList.Concat(const ASecond: IEnumerable_TObject): IList_TObject;
begin
  Result := CObjectList.Create(False, FListIntfGuid, FEnumerableIntfGuid);
  for var Item in ASecond do
    Result.Add(Item);
end;

function CObjectList.Contains(const AValue: TObject): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function CObjectList.Add(const AItem: TObject): Integer;
begin
  Result := inherited Add(AItem);
end;

procedure CObjectList.AddRange(const AValues: Array of TObject);
begin
  for var Value in AValues do
    inherited Add(Value);
end;

function CObjectList.All(const APredicate: TPredicate_TObject): Boolean;
begin
  for var Loop: Integer := 0 to GetCount - 1 do
  begin
    if not APredicate(GetItem(Loop)) then
      Exit(False);
  end;
  Result := True;
end;

function CObjectList.Any: Boolean;
begin
  Result := Count > 0;
end;

procedure CObjectList.DeleteRange(AIndex, ACount: Integer);
begin
  for var Loop: Integer := 1 to ACount do
    Delete(AIndex);
  { TODO : DeleteRange mit einem Test absichern, falls das zum Einsatz kommen sollte }
end;

procedure CObjectList.Exchange(AIndex1, AIndex2: Integer);
begin
  if (PtrUInt(AIndex1) >= PtrUInt(Count)) or
     (PtrUInt(AIndex2) >= PtrUInt(Count)) then
    raise EListError.Create('Index out of bounds');
  if AIndex1 <> AIndex2 then
    inherited Exchange(AIndex1, AIndex2);
end;

function CObjectList.Extract(const AItem: TObject): TObject;
begin
  Result := inherited Extract(AItem);
end;

function CObjectList.First: TObject;
begin
  Result := GetItem(0);
end;

function CObjectList.FirstOrDefault: TObject;
begin
  if IsEmpty then
    Result := nil
  else
    Result := GetItem(0);
end;

function CObjectList.GetCapacity: Integer;
begin
  Result := Capacity;
end;

function CObjectList.GetCount: Integer;
begin
  Result := Count;
end;

function CObjectList.GetEnumerator: IEnumerator_TObject;
begin
  Result := CObjectListEnumerator.Create(Self);
end;

function CObjectList.GetItem(AIndex: Integer): TObject;
begin
  Result := inherited Items[AIndex];
end;

function CObjectList.GetRange(AIndex, ACount: Integer): IList_TObject;
begin
  var ListObj: CObjectList := CObjectList.Create(False, FListIntfGuid, FEnumerableIntfGuid);
  for var Loop: Integer := 0 to Count-1 do
    ListObj.Add(Self.Items[Loop]);
  Result:=ListObj;
end;

function CObjectList.IndexOf(const AItem: TObject): Integer;
begin
  Result := inherited IndexOf(AItem);
end;

procedure CObjectList.Insert(AIndex: Integer; const AItem: TObject);
begin
  inherited Insert(AIndex, AItem);
end;

function CObjectList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function CObjectList.Last: TObject;
begin
  Result := GetItem(Count - 1);
end;

function CObjectList.LastOrDefault: TObject;
begin
  if IsEmpty then
    Result := nil
  else
    Result := GetItem(Count - 1);
end;

function CObjectList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if (IsEqualGUID(IID, FListIntfGuid) and GetInterface(IList_TObject, Obj)) or
     (IsEqualGUID(IID, FEnumerableIntfGuid) and GetInterface(IEnumerable_TObject, Obj)) or
     GetInterface(IID, Obj) then
    Result:=0
  else
    Result:=E_NOINTERFACE;
end;

function CObjectList.Remove(const AItem: TObject): Boolean;
begin
  Result := inherited Remove(Pointer(AItem)) >= 0;
end;

function CObjectList.RemoveAll(const APredicate: TPredicate_TObject): Integer;
begin
  Result := 0;
  for var Loop: Integer := Count - 1 downto 0 do
  begin
    if APredicate(Items[Loop]) then
    begin
      Delete(Loop);
      Inc(Result);
    end;
  end;
end;

procedure CObjectList.Reverse;
begin
  raise ENotImplemented.Create('CObjectList.Reverse');
end;

procedure CObjectList.SetCapacity(AValue: Integer);
begin
  Capacity := AValue;
end;

procedure CObjectList.SetItem(AIndex: Integer; const AValue: TObject);
begin
  inherited Items[AIndex] := AValue;
end;

procedure CObjectList.Sort(ACompare: TOnDynArraySortCompare);
begin
  raise ENotImplemented.Create('CObjectList.Sort');
end;

function CObjectList.ToArray(AOffset, ACount: Integer): TArray<TObject>;
var
  EndIndex: Integer;
begin
  if ACount > 0 then
  begin
    EndIndex:=AOffset+ACount-1;
    if EndIndex>=Count then
      EndIndex:=Count-1;
  end
  else
    EndIndex:=Count-1;
  SetLength(Result, EndIndex - AOffset + 1);
  for var Loop: Integer := AOffset to EndIndex
    do Result[Loop - AOffset] := Items[Loop];
end;

function CObjectList.Where(const APredicate: TPredicate_TObject): IEnumerable_TObject;
var
  ResultList: CObjectList;
begin
  ResultList := CObjectList.Create(False, FListIntfGuid, FEnumerableIntfGuid);
  for var Loop: Integer := 0 to Count - 1 do
  begin
    var Item: TObject := GetItem(Loop);
    if APredicate(Item) then
      ResultList.Add(Item);
  end;
  if ResultList.Any then
    Result:=ResultList as IEnumerable_TObject;
end;

function CObjectList._AddRef: Integer;
begin
  Result := FRefCountIntf._AddRef;
end;

function CObjectList._Release: Integer;
begin
  Result := FRefCountIntf._Release;
  if Result = 1 then // Eins, da wir selbst eine Referenz darauf halten
    Destroy;
end;

{ CObjectListEnumerator }

constructor CObjectListEnumerator.Create(AList: CObjectList);
begin
  FList := AList;
  FIndex := -1;
end;

function CObjectListEnumerator.GetCurrent: TObject;
begin
  Result := FList.GetItem(FIndex);
end;

function CObjectListEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

end.
