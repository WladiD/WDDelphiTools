// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit System.Collections.Factory;

interface

uses

  System.Classes,
  System.SysUtils,

  mormot.core.base,
  mormot.core.collections,
  mormot.core.data,
  mormot.core.json,
  mormot.core.os,
  mormot.core.rtti,

  Collections.Tools,
  Collections.Interfaces,
  Collections.List,
  Collections.Dictionary,
  System.Collections.List,
  System.Collections.Interfaces;

type

  TCollections = class
   public
    {$REGION 'INCLUDE-PARTIAL / System.List-factory-methods-interface.part.pas'}

    class function CreateList_Integer: IList_Integer; overload; static;
    class function CreateList_Integer(const ACopyFrom: IEnumerable_Integer): IList_Integer; overload; static;

    class function CreateList_String: IList_String; overload; static;
    class function CreateList_String(const ACopyFrom: IEnumerable_String): IList_String; overload; static;
    class function CreateList_String(const ACopyFrom: array of String): IList_String; overload; static;
    class function CreateListUnique_String: IList_String; overload; static;

    class function CreateList_TGUID: IList_TGUID; overload; static;
    class function CreateList_TGUID(const ACopyFrom: IEnumerable_TGUID): IList_TGUID; overload; static;

    class function CreateList_TObject(AOwnsObjects: Boolean = true): IList_TObject; overload; static;
    class function CreateList_TObject(const ACopyFrom: IEnumerable_TObject; AOwnsObjects: Boolean): IList_TObject; overload; static;

    class function CreateList_TComponent(AOwnsObjects: Boolean = true): IList_TComponent; overload; static;
    class function CreateList_TComponent(const ACopyFrom: IEnumerable_TComponent; AOwnsObjects: Boolean): IList_TComponent; overload; static;

    class function CreateList_Double: IList_Double; overload; static;
    class function CreateList_Double(const ACopyFrom: IEnumerable_Double): IList_Double; overload; static;
    class function CreateListUnique_Double: IList_Double; overload; static;

    {$ENDREGION 'INCLUDE-PARTIAL / System.List-factory-methods-interface.part.pas'}

    {$REGION 'INCLUDE-PARTIAL / System.Dictionary-factory-methods-interface.part.pas'}

    class function CreateDictionary_Integer_IInterface: IDictionary_Integer_IInterface; overload; static;
    class function CreateDictionary_Integer_String: IDictionary_Integer_String; overload; static;
    class function CreateDictionary_Integer_TGUID: IDictionary_Integer_TGUID; overload; static;
    class function CreateDictionary_Integer_TObject(AOwnValues: Boolean): IDictionary_Integer_TObject; overload; static;
    class function CreateDictionary_String_Integer: IDictionary_String_Integer; overload; static;
    class function CreateDictionary_String_String: IDictionary_String_String; overload; static;
    class function CreateDictionary_String_TGUID: IDictionary_String_TGUID; overload; static;
    class function CreateDictionary_String_TNotifyEvent: IDictionary_String_TNotifyEvent; overload; static;
    class function CreateDictionary_String_TObject(AOwnValues: Boolean): IDictionary_String_TObject; overload; static;
    class function CreateDictionary_TObject_TObject(AOwnKeys: Boolean; AOwnValues: Boolean): IDictionary_TObject_TObject; overload; static;
    class function CreateDictionary_TClass_TObject(AOwnValues: Boolean): IDictionary_TClass_TObject; overload; static;
    class function CreateDictionary_TGUID_Boolean: IDictionary_TGUID_Boolean; overload; static;
    class function CreateDictionary_TGUID_Integer: IDictionary_TGUID_Integer; overload; static;
    class function CreateDictionary_TGUID_String: IDictionary_TGUID_String; overload; static;
    class function CreateDictionary_TGUID_TGUID: IDictionary_TGUID_TGUID; overload; static;

    {$ENDREGION 'INCLUDE-PARTIAL / System.Dictionary-factory-methods-interface.part.pas'}
  end;

implementation

{$REGION 'INCLUDE-PARTIAL / System.List-implementation.part.pas'}

type

  CListEnumerator_Integer = class(CListEnumeratorBase, IEnumerator_Integer)
   strict protected
    function GetCurrent: Integer;
  end;

  CList_Integer = class(CList, IList_Integer, IEnumerable_Integer)
   protected
    function  Add(const AItem: Integer): Integer;
    procedure AddRange(const AValues: array of Integer);
    function  Contains(const AValue: Integer): Boolean;
    function  Extract(const AItem: Integer): Integer;
    function  First: Integer;
    function  FirstOrDefault: Integer;
    function  GetEnumerator: IEnumerator_Integer;
    function  GetItem(AIndex: Integer): Integer;
    function  GetRange(AIndex, ACount: Integer): IList_Integer;
    function  IndexOf(const AItem: Integer): Integer;
    procedure Insert(AIndex: Integer; const AItem: Integer);
    function  Last: Integer;
    function  LastOrDefault: Integer;
    function  Remove(const AItem: Integer): Boolean;
    procedure SetItem(AIndex: Integer; const AValue: Integer);
    function  ToArray(AOffset, ACount: Integer): TArray<Integer>;
  end;

{ ======================================================================= }
// CListEnumerator_Integer
{ ======================================================================= }

function CListEnumerator_Integer.GetCurrent: Integer;
begin
  Result:=CList_Integer(FList).GetItem(FIndex);
end;

{ ======================================================================= }
// CList_Integer
{ ======================================================================= }

function CList_Integer.Add(const AItem: Integer): Integer;
var
  Added: boolean;
begin
  Result:=DoAdd(AItem,Added);
  if Added
    then TArray<Integer>(fValue)[Result]:=AItem;
end;

{ ----------------------------------------------------------------------- }

procedure CList_Integer.AddRange(const AValues: array of Integer);
begin
  for var Value in AValues
    do Add(Value);
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.Contains(const AValue: Integer): Boolean;
begin
  Result:=IndexOf(AValue)>=0;
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.Extract(const AItem: Integer): Integer;
begin
  Result:=Integer(Extract(AItem));
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.First: Integer;
begin
  Result:=GetItem(0);
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.FirstOrDefault: Integer;
begin
  if IsEmpty
    then Result:=Default(Integer)
    else Result:=GetItem(0);
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.GetEnumerator: IEnumerator_Integer;
begin
  Result:=CListEnumerator_Integer.Create(Self);
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.GetItem(AIndex: Integer): Integer;
begin
  if AIndex>=fCount then
    RaiseGetItem(AIndex);
  Result:=TArray<Integer>(fValue)[AIndex];
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.GetRange(AIndex, ACount: Integer): IList_Integer;
begin
  Result:=TCollections.CreateList_Integer(GetRange(AIndex,ACount) as IEnumerable_Integer);
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.IndexOf(const AItem: Integer): Integer;
begin
  Result:=DoFind(AItem,nil);
end;

{ ----------------------------------------------------------------------- }

procedure CList_Integer.Insert(AIndex: Integer; const AItem: Integer);
begin
  DoInsert(AIndex,AItem);
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.Last: Integer;
begin
  Result:=GetItem(GetCount-1);
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.LastOrDefault: Integer;
begin
  if IsEmpty
    then Result:=Default(Integer)
    else Result:=GetItem(Count-1);
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.Remove(const AItem: Integer): Boolean;
begin
  Result:=DoRemove(AItem);
end;

{ ----------------------------------------------------------------------- }

procedure CList_Integer.SetItem(AIndex: Integer; const AValue: Integer);
begin
  if Assigned(fHasher) or (AIndex>=fCount)
    then RaiseSetItem(AIndex);
  TArray<Integer>(fValue)[AIndex]:=AValue;
end;

{ ----------------------------------------------------------------------- }

function CList_Integer.ToArray(AOffset, ACount: Integer): TArray<Integer>;
begin
  fDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ ----------------------------------------------------------------------- }

type

  CListEnumerator_String = class(CListEnumeratorBase, IEnumerator_String)
   strict protected
    function GetCurrent: String;
  end;

  CList_String = class(CList, IList_String, IEnumerable_String)
   protected
    function  Add(const AItem: String): Integer;
    procedure AddRange(const AValues: array of String);
    function  Contains(const AValue: String): Boolean;
    function  Extract(const AItem: String): String;
    function  First: String;
    function  FirstOrDefault: String;
    function  GetEnumerator: IEnumerator_String;
    function  GetItem(AIndex: Integer): String;
    function  GetRange(AIndex, ACount: Integer): IList_String;
    function  IndexOf(const AItem: String): Integer;
    procedure Insert(AIndex: Integer; const AItem: String);
    function  Last: String;
    function  LastOrDefault: String;
    function  Remove(const AItem: String): Boolean;
    procedure SetItem(AIndex: Integer; const AValue: String);
    function  ToArray(AOffset, ACount: Integer): TArray<String>;
  end;

{ ======================================================================= }
// CListEnumerator_String
{ ======================================================================= }

function CListEnumerator_String.GetCurrent: String;
begin
  Result:=CList_String(FList).GetItem(FIndex);
end;

{ ======================================================================= }
// CList_String
{ ======================================================================= }

function CList_String.Add(const AItem: String): Integer;
var
  Added: boolean;
begin
  Result:=DoAdd(AItem,Added);
  if Added
    then TArray<String>(fValue)[Result]:=AItem;
end;

{ ----------------------------------------------------------------------- }

procedure CList_String.AddRange(const AValues: array of String);
begin
  for var Value in AValues
    do Add(Value);
end;

{ ----------------------------------------------------------------------- }

function CList_String.Contains(const AValue: String): Boolean;
begin
  Result:=IndexOf(AValue)>=0;
end;

{ ----------------------------------------------------------------------- }

function CList_String.Extract(const AItem: String): String;
begin
  Result:=String(Extract(AItem));
end;

{ ----------------------------------------------------------------------- }

function CList_String.First: String;
begin
  Result:=GetItem(0);
end;

{ ----------------------------------------------------------------------- }

function CList_String.FirstOrDefault: String;
begin
  if IsEmpty
    then Result:=Default(String)
    else Result:=GetItem(0);
end;

{ ----------------------------------------------------------------------- }

function CList_String.GetEnumerator: IEnumerator_String;
begin
  Result:=CListEnumerator_String.Create(Self);
end;

{ ----------------------------------------------------------------------- }

function CList_String.GetItem(AIndex: Integer): String;
begin
  if AIndex>=fCount then
    RaiseGetItem(AIndex);
  Result:=TArray<String>(fValue)[AIndex];
end;

{ ----------------------------------------------------------------------- }

function CList_String.GetRange(AIndex, ACount: Integer): IList_String;
begin
  Result:=TCollections.CreateList_String(GetRange(AIndex,ACount) as IEnumerable_String);
end;

{ ----------------------------------------------------------------------- }

function CList_String.IndexOf(const AItem: String): Integer;
begin
  Result:=DoFind(AItem,nil);
end;

{ ----------------------------------------------------------------------- }

procedure CList_String.Insert(AIndex: Integer; const AItem: String);
begin
  DoInsert(AIndex,AItem);
end;

{ ----------------------------------------------------------------------- }

function CList_String.Last: String;
begin
  Result:=GetItem(GetCount-1);
end;

{ ----------------------------------------------------------------------- }

function CList_String.LastOrDefault: String;
begin
  if IsEmpty
    then Result:=Default(String)
    else Result:=GetItem(Count-1);
end;

{ ----------------------------------------------------------------------- }

function CList_String.Remove(const AItem: String): Boolean;
begin
  Result:=DoRemove(AItem);
end;

{ ----------------------------------------------------------------------- }

procedure CList_String.SetItem(AIndex: Integer; const AValue: String);
begin
  if Assigned(fHasher) or (AIndex>=fCount)
    then RaiseSetItem(AIndex);
  TArray<String>(fValue)[AIndex]:=AValue;
end;

{ ----------------------------------------------------------------------- }

function CList_String.ToArray(AOffset, ACount: Integer): TArray<String>;
begin
  fDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ ----------------------------------------------------------------------- }

type

  CListEnumerator_TGUID = class(CListEnumeratorBase, IEnumerator_TGUID)
   strict protected
    function GetCurrent: TGUID;
  end;

  CList_TGUID = class(CList, IList_TGUID, IEnumerable_TGUID)
   protected
    function  Add(const AItem: TGUID): Integer;
    procedure AddRange(const AValues: array of TGUID);
    function  Contains(const AValue: TGUID): Boolean;
    function  Extract(const AItem: TGUID): TGUID;
    function  First: TGUID;
    function  FirstOrDefault: TGUID;
    function  GetEnumerator: IEnumerator_TGUID;
    function  GetItem(AIndex: Integer): TGUID;
    function  GetRange(AIndex, ACount: Integer): IList_TGUID;
    function  IndexOf(const AItem: TGUID): Integer;
    procedure Insert(AIndex: Integer; const AItem: TGUID);
    function  Last: TGUID;
    function  LastOrDefault: TGUID;
    function  Remove(const AItem: TGUID): Boolean;
    procedure SetItem(AIndex: Integer; const AValue: TGUID);
    function  ToArray(AOffset, ACount: Integer): TArray<TGUID>;
  end;

{ ======================================================================= }
// CListEnumerator_TGUID
{ ======================================================================= }

function CListEnumerator_TGUID.GetCurrent: TGUID;
begin
  Result:=CList_TGUID(FList).GetItem(FIndex);
end;

{ ======================================================================= }
// CList_TGUID
{ ======================================================================= }

function CList_TGUID.Add(const AItem: TGUID): Integer;
var
  Added: boolean;
begin
  Result:=DoAdd(AItem,Added);
  if Added
    then TArray<TGUID>(fValue)[Result]:=AItem;
end;

{ ----------------------------------------------------------------------- }

procedure CList_TGUID.AddRange(const AValues: array of TGUID);
begin
  for var Value in AValues
    do Add(Value);
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.Contains(const AValue: TGUID): Boolean;
begin
  Result:=IndexOf(AValue)>=0;
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.Extract(const AItem: TGUID): TGUID;
begin
  Result:=TGUID(Extract(AItem));
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.First: TGUID;
begin
  Result:=GetItem(0);
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.FirstOrDefault: TGUID;
begin
  if IsEmpty
    then Result:=Default(TGUID)
    else Result:=GetItem(0);
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.GetEnumerator: IEnumerator_TGUID;
begin
  Result:=CListEnumerator_TGUID.Create(Self);
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.GetItem(AIndex: Integer): TGUID;
begin
  if AIndex>=fCount then
    RaiseGetItem(AIndex);
  Result:=TArray<TGUID>(fValue)[AIndex];
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.GetRange(AIndex, ACount: Integer): IList_TGUID;
begin
  Result:=TCollections.CreateList_TGUID(GetRange(AIndex,ACount) as IEnumerable_TGUID);
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.IndexOf(const AItem: TGUID): Integer;
begin
  Result:=DoFind(AItem,nil);
end;

{ ----------------------------------------------------------------------- }

procedure CList_TGUID.Insert(AIndex: Integer; const AItem: TGUID);
begin
  DoInsert(AIndex,AItem);
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.Last: TGUID;
begin
  Result:=GetItem(GetCount-1);
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.LastOrDefault: TGUID;
begin
  if IsEmpty
    then Result:=Default(TGUID)
    else Result:=GetItem(Count-1);
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.Remove(const AItem: TGUID): Boolean;
begin
  Result:=DoRemove(AItem);
end;

{ ----------------------------------------------------------------------- }

procedure CList_TGUID.SetItem(AIndex: Integer; const AValue: TGUID);
begin
  if Assigned(fHasher) or (AIndex>=fCount)
    then RaiseSetItem(AIndex);
  TArray<TGUID>(fValue)[AIndex]:=AValue;
end;

{ ----------------------------------------------------------------------- }

function CList_TGUID.ToArray(AOffset, ACount: Integer): TArray<TGUID>;
begin
  fDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ ----------------------------------------------------------------------- }

type

  CListEnumerator_Double = class(CListEnumeratorBase, IEnumerator_Double)
   strict protected
    function GetCurrent: Double;
  end;

  CList_Double = class(CList, IList_Double, IEnumerable_Double)
   protected
    function  Add(const AItem: Double): Integer;
    procedure AddRange(const AValues: array of Double);
    function  Contains(const AValue: Double): Boolean;
    function  Extract(const AItem: Double): Double;
    function  First: Double;
    function  FirstOrDefault: Double;
    function  GetEnumerator: IEnumerator_Double;
    function  GetItem(AIndex: Integer): Double;
    function  GetRange(AIndex, ACount: Integer): IList_Double;
    function  IndexOf(const AItem: Double): Integer;
    procedure Insert(AIndex: Integer; const AItem: Double);
    function  Last: Double;
    function  LastOrDefault: Double;
    function  Remove(const AItem: Double): Boolean;
    procedure SetItem(AIndex: Integer; const AValue: Double);
    function  ToArray(AOffset, ACount: Integer): TArray<Double>;
  end;

{ ======================================================================= }
// CListEnumerator_Double
{ ======================================================================= }

function CListEnumerator_Double.GetCurrent: Double;
begin
  Result:=CList_Double(FList).GetItem(FIndex);
end;

{ ======================================================================= }
// CList_Double
{ ======================================================================= }

function CList_Double.Add(const AItem: Double): Integer;
var
  Added: boolean;
begin
  Result:=DoAdd(AItem,Added);
  if Added
    then TArray<Double>(fValue)[Result]:=AItem;
end;

{ ----------------------------------------------------------------------- }

procedure CList_Double.AddRange(const AValues: array of Double);
begin
  for var Value in AValues
    do Add(Value);
end;

{ ----------------------------------------------------------------------- }

function CList_Double.Contains(const AValue: Double): Boolean;
begin
  Result:=IndexOf(AValue)>=0;
end;

{ ----------------------------------------------------------------------- }

function CList_Double.Extract(const AItem: Double): Double;
begin
  Result:=Double(Extract(AItem));
end;

{ ----------------------------------------------------------------------- }

function CList_Double.First: Double;
begin
  Result:=GetItem(0);
end;

{ ----------------------------------------------------------------------- }

function CList_Double.FirstOrDefault: Double;
begin
  if IsEmpty
    then Result:=Default(Double)
    else Result:=GetItem(0);
end;

{ ----------------------------------------------------------------------- }

function CList_Double.GetEnumerator: IEnumerator_Double;
begin
  Result:=CListEnumerator_Double.Create(Self);
end;

{ ----------------------------------------------------------------------- }

function CList_Double.GetItem(AIndex: Integer): Double;
begin
  if AIndex>=fCount then
    RaiseGetItem(AIndex);
  Result:=TArray<Double>(fValue)[AIndex];
end;

{ ----------------------------------------------------------------------- }

function CList_Double.GetRange(AIndex, ACount: Integer): IList_Double;
begin
  Result:=TCollections.CreateList_Double(GetRange(AIndex,ACount) as IEnumerable_Double);
end;

{ ----------------------------------------------------------------------- }

function CList_Double.IndexOf(const AItem: Double): Integer;
begin
  Result:=DoFind(AItem,nil);
end;

{ ----------------------------------------------------------------------- }

procedure CList_Double.Insert(AIndex: Integer; const AItem: Double);
begin
  DoInsert(AIndex,AItem);
end;

{ ----------------------------------------------------------------------- }

function CList_Double.Last: Double;
begin
  Result:=GetItem(GetCount-1);
end;

{ ----------------------------------------------------------------------- }

function CList_Double.LastOrDefault: Double;
begin
  if IsEmpty
    then Result:=Default(Double)
    else Result:=GetItem(Count-1);
end;

{ ----------------------------------------------------------------------- }

function CList_Double.Remove(const AItem: Double): Boolean;
begin
  Result:=DoRemove(AItem);
end;

{ ----------------------------------------------------------------------- }

procedure CList_Double.SetItem(AIndex: Integer; const AValue: Double);
begin
  if Assigned(fHasher) or (AIndex>=fCount)
    then RaiseSetItem(AIndex);
  TArray<Double>(fValue)[AIndex]:=AValue;
end;

{ ----------------------------------------------------------------------- }

function CList_Double.ToArray(AOffset, ACount: Integer): TArray<Double>;
begin
  fDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ ======================================================================= }
// TCollections
{ ======================================================================= }

class function TCollections.CreateList_TObject(AOwnsObjects: Boolean = true): IList_TObject;
begin
  var TempList: IList_TObject:=CObjectList.Create(AOwnsObjects,IList_TObject,IEnumerable_TObject);
  Result:=IList_TObject(TempList);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_TObject(const ACopyFrom: IEnumerable_TObject; AOwnsObjects: Boolean): IList_TObject;
begin
  Result:=CreateList_TObject(AOwnsObjects);
  for var AItem in ACopyFrom
    do Result.Add(AItem);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_TComponent(AOwnsObjects: Boolean = true): IList_TComponent;
begin
  var TempList: IList_TObject:=CObjectList.Create(AOwnsObjects,IList_TComponent,IEnumerable_TComponent);
  Result:=IList_TComponent(TempList);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_TComponent(const ACopyFrom: IEnumerable_TComponent; AOwnsObjects: Boolean): IList_TComponent;
begin
  Result:=CreateList_TComponent(AOwnsObjects);
  for var AItem in ACopyFrom
    do Result.Add(AItem);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_Integer: IList_Integer;
begin
  Result:=CList_Integer.Create(TypeInfo(TArray<Integer>),TypeInfo(Integer));
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_Integer(const ACopyFrom: IEnumerable_Integer): IList_Integer;
begin
  Result:=CreateList_Integer;
  for var AItem in ACopyFrom
    do Result.Add(AItem);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_String: IList_String;
begin
  Result:=CList_String.Create(TypeInfo(TArray<String>),TypeInfo(String));
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateListUnique_String: IList_String;
begin
  Result:=CList_String.Create(TypeInfo(TArray<String>),TypeInfo(String),[loCreateUniqueIndex]);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_String(const ACopyFrom: IEnumerable_String): IList_String;
begin
  Result:=CreateList_String;
  for var AItem in ACopyFrom
    do Result.Add(AItem);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_String(const ACopyFrom: array of String): IList_String;
begin
  Result:=CreateList_String;
  for var AItem in ACopyFrom
    do Result.Add(AItem);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_TGUID: IList_TGUID;
begin
  Result:=CList_TGUID.Create(TypeInfo(TArray<TGUID>),TypeInfo(TGUID));
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_TGUID(const ACopyFrom: IEnumerable_TGUID): IList_TGUID;
begin
  Result:=CreateList_TGUID;
  for var AItem in ACopyFrom
    do Result.Add(AItem);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_Double: IList_Double;
begin
  Result:=CList_Double.Create(TypeInfo(TArray<Double>),TypeInfo(Double));
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateListUnique_Double: IList_Double;
begin
  Result:=CList_Double.Create(TypeInfo(TArray<Double>),TypeInfo(Double),[loCreateUniqueIndex]);
end;

{ ----------------------------------------------------------------------- }

class function TCollections.CreateList_Double(const ACopyFrom: IEnumerable_Double): IList_Double;
begin
  Result:=CreateList_Double;
  for var AItem in ACopyFrom
    do Result.Add(AItem);
end;

{$ENDREGION 'INCLUDE-PARTIAL / System.List-implementation.part.pas'}

{$REGION 'INCLUDE-PARTIAL / System.Dictionary-implementation.part.pas'}

type

  CDictionary_Integer_IInterface = class(CDictionaryBase, IDictionary_Integer_IInterface)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_Integer_IInterface)
     public
      function GetCurrent: TPair_Integer_IInterface;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_Integer)
     public
      function GetCurrent: Integer;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_IInterface)
     public
      function GetCurrent: IInterface;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_Integer)
     public
      function GetEnumerator: IEnumerator_Integer;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Integer>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_IInterface)
     public
      function GetEnumerator: IEnumerator_IInterface;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<IInterface>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_Integer;
    function  GetValueEnumerable: IEnumerable_IInterface;
    procedure Add(const AKey: Integer; const AValue: IInterface);
    function  Contains(const AKey: Integer; const AValue: IInterface): Boolean;
    function  ContainsKey(const AKey: Integer): Boolean;
    function  Extract(const AKey: Integer): IInterface;
    function  Remove(const AKey: Integer): Boolean;
    function  GetEnumerator: IPairEnumerator_Integer_IInterface;
    function  GetItem(const AKey: Integer): IInterface;
    procedure SetItem(const AKey: Integer; const AValue: IInterface);
    function  TryGetValue(const AKey: Integer; out AValue: IInterface): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_Integer_IInterface.CPairEnumerator }

function CDictionary_Integer_IInterface.CPairEnumerator.GetCurrent: TPair_Integer_IInterface;
begin
  Result.Key:=Integer(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=IInterface(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_IInterface.CKeyEnumerator }

function CDictionary_Integer_IInterface.CKeyEnumerator.GetCurrent: Integer;
begin
  Result:=Integer(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_IInterface.CValueEnumerator }

function CDictionary_Integer_IInterface.CValueEnumerator.GetCurrent: IInterface;
begin
  Result:=IInterface(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_IInterface.CKeyEnumerable }

function CDictionary_Integer_IInterface.CKeyEnumerable.GetEnumerator: IEnumerator_Integer;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_Integer_IInterface.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<Integer>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_Integer_IInterface.CValueEnumerable }

function CDictionary_Integer_IInterface.CValueEnumerable.GetEnumerator: IEnumerator_IInterface;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_Integer_IInterface.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<IInterface>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_Integer_IInterface }

constructor CDictionary_Integer_IInterface.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<Integer>),TypeInfo(TArray<IInterface>));
end;

function CDictionary_Integer_IInterface.Contains(const AKey: Integer; const AValue: IInterface): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (IInterface(ValuePointer^)=AValue);
end;

function CDictionary_Integer_IInterface.ContainsKey(const AKey: Integer): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_Integer_IInterface.Extract(const AKey: Integer): IInterface;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_Integer_IInterface.Remove(const AKey: Integer): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_Integer_IInterface.GetEnumerator: IPairEnumerator_Integer_IInterface;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_Integer_IInterface.GetItem(const AKey: Integer): IInterface;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=IInterface(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_Integer_IInterface.SetItem(const AKey: Integer; const AValue: IInterface);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_Integer_IInterface.GetKeyEnumerable: IEnumerable_Integer;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_Integer_IInterface.GetValueEnumerable: IEnumerable_IInterface;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_Integer_IInterface.Add(const AKey: Integer; const AValue: IInterface);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_Integer_IInterface.TryGetValue(const AKey: Integer; out AValue: IInterface): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=IInterface(ValuePointer^);
end;

type

  CDictionary_Integer_String = class(CDictionaryBase, IDictionary_Integer_String)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_Integer_String)
     public
      function GetCurrent: TPair_Integer_String;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_Integer)
     public
      function GetCurrent: Integer;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_String)
     public
      function GetCurrent: String;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_Integer)
     public
      function GetEnumerator: IEnumerator_Integer;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Integer>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_String)
     public
      function GetEnumerator: IEnumerator_String;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_Integer;
    function  GetValueEnumerable: IEnumerable_String;
    procedure Add(const AKey: Integer; const AValue: String);
    function  Contains(const AKey: Integer; const AValue: String): Boolean;
    function  ContainsKey(const AKey: Integer): Boolean;
    function  Extract(const AKey: Integer): String;
    function  Remove(const AKey: Integer): Boolean;
    function  GetEnumerator: IPairEnumerator_Integer_String;
    function  GetItem(const AKey: Integer): String;
    procedure SetItem(const AKey: Integer; const AValue: String);
    function  TryGetValue(const AKey: Integer; out AValue: String): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_Integer_String.CPairEnumerator }

function CDictionary_Integer_String.CPairEnumerator.GetCurrent: TPair_Integer_String;
begin
  Result.Key:=Integer(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=String(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_String.CKeyEnumerator }

function CDictionary_Integer_String.CKeyEnumerator.GetCurrent: Integer;
begin
  Result:=Integer(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_String.CValueEnumerator }

function CDictionary_Integer_String.CValueEnumerator.GetCurrent: String;
begin
  Result:=String(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_String.CKeyEnumerable }

function CDictionary_Integer_String.CKeyEnumerable.GetEnumerator: IEnumerator_Integer;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_Integer_String.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<Integer>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_Integer_String.CValueEnumerable }

function CDictionary_Integer_String.CValueEnumerable.GetEnumerator: IEnumerator_String;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_Integer_String.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<String>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_Integer_String }

constructor CDictionary_Integer_String.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<Integer>),TypeInfo(TArray<String>));
end;

function CDictionary_Integer_String.Contains(const AKey: Integer; const AValue: String): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (String(ValuePointer^)=AValue);
end;

function CDictionary_Integer_String.ContainsKey(const AKey: Integer): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_Integer_String.Extract(const AKey: Integer): String;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_Integer_String.Remove(const AKey: Integer): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_Integer_String.GetEnumerator: IPairEnumerator_Integer_String;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_Integer_String.GetItem(const AKey: Integer): String;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=String(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_Integer_String.SetItem(const AKey: Integer; const AValue: String);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_Integer_String.GetKeyEnumerable: IEnumerable_Integer;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_Integer_String.GetValueEnumerable: IEnumerable_String;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_Integer_String.Add(const AKey: Integer; const AValue: String);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_Integer_String.TryGetValue(const AKey: Integer; out AValue: String): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=String(ValuePointer^);
end;

type

  CDictionary_Integer_TGUID = class(CDictionaryBase, IDictionary_Integer_TGUID)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_Integer_TGUID)
     public
      function GetCurrent: TPair_Integer_TGUID;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_Integer)
     public
      function GetCurrent: Integer;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TGUID)
     public
      function GetCurrent: TGUID;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_Integer)
     public
      function GetEnumerator: IEnumerator_Integer;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Integer>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TGUID)
     public
      function GetEnumerator: IEnumerator_TGUID;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TGUID>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_Integer;
    function  GetValueEnumerable: IEnumerable_TGUID;
    procedure Add(const AKey: Integer; const AValue: TGUID);
    function  Contains(const AKey: Integer; const AValue: TGUID): Boolean;
    function  ContainsKey(const AKey: Integer): Boolean;
    function  Extract(const AKey: Integer): TGUID;
    function  Remove(const AKey: Integer): Boolean;
    function  GetEnumerator: IPairEnumerator_Integer_TGUID;
    function  GetItem(const AKey: Integer): TGUID;
    procedure SetItem(const AKey: Integer; const AValue: TGUID);
    function  TryGetValue(const AKey: Integer; out AValue: TGUID): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_Integer_TGUID.CPairEnumerator }

function CDictionary_Integer_TGUID.CPairEnumerator.GetCurrent: TPair_Integer_TGUID;
begin
  Result.Key:=Integer(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=TGUID(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_TGUID.CKeyEnumerator }

function CDictionary_Integer_TGUID.CKeyEnumerator.GetCurrent: Integer;
begin
  Result:=Integer(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_TGUID.CValueEnumerator }

function CDictionary_Integer_TGUID.CValueEnumerator.GetCurrent: TGUID;
begin
  Result:=TGUID(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_TGUID.CKeyEnumerable }

function CDictionary_Integer_TGUID.CKeyEnumerable.GetEnumerator: IEnumerator_Integer;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_Integer_TGUID.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<Integer>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_Integer_TGUID.CValueEnumerable }

function CDictionary_Integer_TGUID.CValueEnumerable.GetEnumerator: IEnumerator_TGUID;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_Integer_TGUID.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<TGUID>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_Integer_TGUID }

constructor CDictionary_Integer_TGUID.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<Integer>),TypeInfo(TArray<TGUID>));
end;

function CDictionary_Integer_TGUID.Contains(const AKey: Integer; const AValue: TGUID): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (TGUID(ValuePointer^)=AValue);
end;

function CDictionary_Integer_TGUID.ContainsKey(const AKey: Integer): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_Integer_TGUID.Extract(const AKey: Integer): TGUID;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_Integer_TGUID.Remove(const AKey: Integer): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_Integer_TGUID.GetEnumerator: IPairEnumerator_Integer_TGUID;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_Integer_TGUID.GetItem(const AKey: Integer): TGUID;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=TGUID(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_Integer_TGUID.SetItem(const AKey: Integer; const AValue: TGUID);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_Integer_TGUID.GetKeyEnumerable: IEnumerable_Integer;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_Integer_TGUID.GetValueEnumerable: IEnumerable_TGUID;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_Integer_TGUID.Add(const AKey: Integer; const AValue: TGUID);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_Integer_TGUID.TryGetValue(const AKey: Integer; out AValue: TGUID): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=TGUID(ValuePointer^);
end;

type

  CDictionary_Integer_TObject = class(CDictionaryBase, IDictionary_Integer_TObject)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_Integer_TObject)
     public
      function GetCurrent: TPair_Integer_TObject;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_Integer)
     public
      function GetCurrent: Integer;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TObject)
     public
      function GetCurrent: TObject;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_Integer)
     public
      function GetEnumerator: IEnumerator_Integer;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Integer>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TObject)
     public
      function GetEnumerator: IEnumerator_TObject;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TObject>;
    end;

   strict private
    FOwnValues: Boolean;
    function  GetKeyEnumerable: IEnumerable_Integer;
    function  GetValueEnumerable: IEnumerable_TObject;
    procedure Add(const AKey: Integer; const AValue: TObject);
    function  Contains(const AKey: Integer; const AValue: TObject): Boolean;
    function  ContainsKey(const AKey: Integer): Boolean;
    function  Extract(const AKey: Integer): TObject;
    function  Remove(const AKey: Integer): Boolean;
    function  GetEnumerator: IPairEnumerator_Integer_TObject;
    function  GetItem(const AKey: Integer): TObject;
    procedure SetItem(const AKey: Integer; const AValue: TObject);
    function  TryGetValue(const AKey: Integer; out AValue: TObject): Boolean;
   public
    constructor Create(AOwnValues: Boolean);
  end;

{ CDictionary_Integer_TObject.CPairEnumerator }

function CDictionary_Integer_TObject.CPairEnumerator.GetCurrent: TPair_Integer_TObject;
begin
  Result.Key:=Integer(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=TObject(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_TObject.CKeyEnumerator }

function CDictionary_Integer_TObject.CKeyEnumerator.GetCurrent: Integer;
begin
  Result:=Integer(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_TObject.CValueEnumerator }

function CDictionary_Integer_TObject.CValueEnumerator.GetCurrent: TObject;
begin
  Result:=TObject(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_Integer_TObject.CKeyEnumerable }

function CDictionary_Integer_TObject.CKeyEnumerable.GetEnumerator: IEnumerator_Integer;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_Integer_TObject.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<Integer>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_Integer_TObject.CValueEnumerable }

function CDictionary_Integer_TObject.CValueEnumerable.GetEnumerator: IEnumerator_TObject;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_Integer_TObject.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<TObject>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_Integer_TObject }

constructor CDictionary_Integer_TObject.Create(AOwnValues: Boolean);
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<Integer>),TypeInfo(TArray<TObject>));
  FOwnValues:=AOwnValues;
  PDynArray(@FSynDict.Values).NoFinalize:=not FOwnValues;
end;

function CDictionary_Integer_TObject.Contains(const AKey: Integer; const AValue: TObject): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (TObject(ValuePointer^)=AValue);
end;

function CDictionary_Integer_TObject.ContainsKey(const AKey: Integer): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_Integer_TObject.Extract(const AKey: Integer): TObject;
begin
  var ValuesDynArray: PDynArray:=PDynArray(@FSynDict.Values);
  if FOwnValues
    then ValuesDynArray.NoFinalize:=true;
  FSynDict.FindAndExtract(AKey,Result);
  if FOwnValues
    then ValuesDynArray.NoFinalize:=false;
end;

function CDictionary_Integer_TObject.Remove(const AKey: Integer): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_Integer_TObject.GetEnumerator: IPairEnumerator_Integer_TObject;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_Integer_TObject.GetItem(const AKey: Integer): TObject;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=TObject(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_Integer_TObject.SetItem(const AKey: Integer; const AValue: TObject);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_Integer_TObject.GetKeyEnumerable: IEnumerable_Integer;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_Integer_TObject.GetValueEnumerable: IEnumerable_TObject;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_Integer_TObject.Add(const AKey: Integer; const AValue: TObject);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_Integer_TObject.TryGetValue(const AKey: Integer; out AValue: TObject): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=TObject(ValuePointer^);
end;

type

  CDictionary_String_Integer = class(CDictionaryBase, IDictionary_String_Integer)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_String_Integer)
     public
      function GetCurrent: TPair_String_Integer;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_String)
     public
      function GetCurrent: String;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_Integer)
     public
      function GetCurrent: Integer;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_String)
     public
      function GetEnumerator: IEnumerator_String;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_Integer)
     public
      function GetEnumerator: IEnumerator_Integer;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Integer>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_Integer;
    procedure Add(const AKey: String; const AValue: Integer);
    function  Contains(const AKey: String; const AValue: Integer): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): Integer;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_Integer;
    function  GetItem(const AKey: String): Integer;
    procedure SetItem(const AKey: String; const AValue: Integer);
    function  TryGetValue(const AKey: String; out AValue: Integer): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_String_Integer.CPairEnumerator }

function CDictionary_String_Integer.CPairEnumerator.GetCurrent: TPair_String_Integer;
begin
  Result.Key:=String(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=Integer(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_Integer.CKeyEnumerator }

function CDictionary_String_Integer.CKeyEnumerator.GetCurrent: String;
begin
  Result:=String(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_String_Integer.CValueEnumerator }

function CDictionary_String_Integer.CValueEnumerator.GetCurrent: Integer;
begin
  Result:=Integer(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_Integer.CKeyEnumerable }

function CDictionary_String_Integer.CKeyEnumerable.GetEnumerator: IEnumerator_String;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_String_Integer.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<String>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_Integer.CValueEnumerable }

function CDictionary_String_Integer.CValueEnumerable.GetEnumerator: IEnumerator_Integer;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_String_Integer.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<Integer>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_Integer }

constructor CDictionary_String_Integer.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<String>),TypeInfo(TArray<Integer>));
end;

function CDictionary_String_Integer.Contains(const AKey: String; const AValue: Integer): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (Integer(ValuePointer^)=AValue);
end;

function CDictionary_String_Integer.ContainsKey(const AKey: String): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_String_Integer.Extract(const AKey: String): Integer;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_String_Integer.Remove(const AKey: String): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_String_Integer.GetEnumerator: IPairEnumerator_String_Integer;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_String_Integer.GetItem(const AKey: String): Integer;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=Integer(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_String_Integer.SetItem(const AKey: String; const AValue: Integer);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_String_Integer.GetKeyEnumerable: IEnumerable_String;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_String_Integer.GetValueEnumerable: IEnumerable_Integer;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_String_Integer.Add(const AKey: String; const AValue: Integer);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_String_Integer.TryGetValue(const AKey: String; out AValue: Integer): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=Integer(ValuePointer^);
end;

type

  CDictionary_String_String = class(CDictionaryBase, IDictionary_String_String)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_String_String)
     public
      function GetCurrent: TPair_String_String;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_String)
     public
      function GetCurrent: String;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_String)
     public
      function GetCurrent: String;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_String)
     public
      function GetEnumerator: IEnumerator_String;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_String)
     public
      function GetEnumerator: IEnumerator_String;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_String;
    procedure Add(const AKey: String; const AValue: String);
    function  Contains(const AKey: String; const AValue: String): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): String;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_String;
    function  GetItem(const AKey: String): String;
    procedure SetItem(const AKey: String; const AValue: String);
    function  TryGetValue(const AKey: String; out AValue: String): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_String_String.CPairEnumerator }

function CDictionary_String_String.CPairEnumerator.GetCurrent: TPair_String_String;
begin
  Result.Key:=String(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=String(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_String.CKeyEnumerator }

function CDictionary_String_String.CKeyEnumerator.GetCurrent: String;
begin
  Result:=String(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_String_String.CValueEnumerator }

function CDictionary_String_String.CValueEnumerator.GetCurrent: String;
begin
  Result:=String(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_String.CKeyEnumerable }

function CDictionary_String_String.CKeyEnumerable.GetEnumerator: IEnumerator_String;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_String_String.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<String>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_String.CValueEnumerable }

function CDictionary_String_String.CValueEnumerable.GetEnumerator: IEnumerator_String;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_String_String.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<String>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_String }

constructor CDictionary_String_String.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<String>),TypeInfo(TArray<String>));
end;

function CDictionary_String_String.Contains(const AKey: String; const AValue: String): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (String(ValuePointer^)=AValue);
end;

function CDictionary_String_String.ContainsKey(const AKey: String): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_String_String.Extract(const AKey: String): String;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_String_String.Remove(const AKey: String): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_String_String.GetEnumerator: IPairEnumerator_String_String;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_String_String.GetItem(const AKey: String): String;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=String(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_String_String.SetItem(const AKey: String; const AValue: String);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_String_String.GetKeyEnumerable: IEnumerable_String;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_String_String.GetValueEnumerable: IEnumerable_String;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_String_String.Add(const AKey: String; const AValue: String);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_String_String.TryGetValue(const AKey: String; out AValue: String): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=String(ValuePointer^);
end;

type

  CDictionary_String_TGUID = class(CDictionaryBase, IDictionary_String_TGUID)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_String_TGUID)
     public
      function GetCurrent: TPair_String_TGUID;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_String)
     public
      function GetCurrent: String;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TGUID)
     public
      function GetCurrent: TGUID;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_String)
     public
      function GetEnumerator: IEnumerator_String;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TGUID)
     public
      function GetEnumerator: IEnumerator_TGUID;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TGUID>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_TGUID;
    procedure Add(const AKey: String; const AValue: TGUID);
    function  Contains(const AKey: String; const AValue: TGUID): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): TGUID;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_TGUID;
    function  GetItem(const AKey: String): TGUID;
    procedure SetItem(const AKey: String; const AValue: TGUID);
    function  TryGetValue(const AKey: String; out AValue: TGUID): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_String_TGUID.CPairEnumerator }

function CDictionary_String_TGUID.CPairEnumerator.GetCurrent: TPair_String_TGUID;
begin
  Result.Key:=String(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=TGUID(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_TGUID.CKeyEnumerator }

function CDictionary_String_TGUID.CKeyEnumerator.GetCurrent: String;
begin
  Result:=String(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_String_TGUID.CValueEnumerator }

function CDictionary_String_TGUID.CValueEnumerator.GetCurrent: TGUID;
begin
  Result:=TGUID(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_TGUID.CKeyEnumerable }

function CDictionary_String_TGUID.CKeyEnumerable.GetEnumerator: IEnumerator_String;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_String_TGUID.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<String>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_TGUID.CValueEnumerable }

function CDictionary_String_TGUID.CValueEnumerable.GetEnumerator: IEnumerator_TGUID;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_String_TGUID.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<TGUID>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_TGUID }

constructor CDictionary_String_TGUID.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<String>),TypeInfo(TArray<TGUID>));
end;

function CDictionary_String_TGUID.Contains(const AKey: String; const AValue: TGUID): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (TGUID(ValuePointer^)=AValue);
end;

function CDictionary_String_TGUID.ContainsKey(const AKey: String): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_String_TGUID.Extract(const AKey: String): TGUID;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_String_TGUID.Remove(const AKey: String): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_String_TGUID.GetEnumerator: IPairEnumerator_String_TGUID;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_String_TGUID.GetItem(const AKey: String): TGUID;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=TGUID(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_String_TGUID.SetItem(const AKey: String; const AValue: TGUID);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_String_TGUID.GetKeyEnumerable: IEnumerable_String;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_String_TGUID.GetValueEnumerable: IEnumerable_TGUID;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_String_TGUID.Add(const AKey: String; const AValue: TGUID);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_String_TGUID.TryGetValue(const AKey: String; out AValue: TGUID): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=TGUID(ValuePointer^);
end;

type

  CDictionary_String_TNotifyEvent = class(CDictionaryBase, IDictionary_String_TNotifyEvent)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_String_TNotifyEvent)
     public
      function GetCurrent: TPair_String_TNotifyEvent;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_String)
     public
      function GetCurrent: String;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TNotifyEvent)
     public
      function GetCurrent: TNotifyEvent;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_String)
     public
      function GetEnumerator: IEnumerator_String;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TNotifyEvent)
     public
      function GetEnumerator: IEnumerator_TNotifyEvent;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TNotifyEvent>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_TNotifyEvent;
    procedure Add(const AKey: String; const AValue: TNotifyEvent);
    function  Contains(const AKey: String; const AValue: TNotifyEvent): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): TNotifyEvent;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_TNotifyEvent;
    function  GetItem(const AKey: String): TNotifyEvent;
    procedure SetItem(const AKey: String; const AValue: TNotifyEvent);
    function  TryGetValue(const AKey: String; out AValue: TNotifyEvent): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_String_TNotifyEvent.CPairEnumerator }

function CDictionary_String_TNotifyEvent.CPairEnumerator.GetCurrent: TPair_String_TNotifyEvent;
begin
  Result.Key:=String(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=TNotifyEvent(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_TNotifyEvent.CKeyEnumerator }

function CDictionary_String_TNotifyEvent.CKeyEnumerator.GetCurrent: String;
begin
  Result:=String(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_String_TNotifyEvent.CValueEnumerator }

function CDictionary_String_TNotifyEvent.CValueEnumerator.GetCurrent: TNotifyEvent;
begin
  Result:=TNotifyEvent(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_TNotifyEvent.CKeyEnumerable }

function CDictionary_String_TNotifyEvent.CKeyEnumerable.GetEnumerator: IEnumerator_String;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_String_TNotifyEvent.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<String>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_TNotifyEvent.CValueEnumerable }

function CDictionary_String_TNotifyEvent.CValueEnumerable.GetEnumerator: IEnumerator_TNotifyEvent;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_String_TNotifyEvent.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<TNotifyEvent>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_TNotifyEvent }

constructor CDictionary_String_TNotifyEvent.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<String>),TypeInfo(TArray<TNotifyEvent>));
end;

function CDictionary_String_TNotifyEvent.Contains(const AKey: String; const AValue: TNotifyEvent): Boolean;
var 
  ArgValuePointer: Pointer absolute AValue;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (Pointer(ValuePointer^)=ArgValuePointer);
end;

function CDictionary_String_TNotifyEvent.ContainsKey(const AKey: String): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_String_TNotifyEvent.Extract(const AKey: String): TNotifyEvent;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_String_TNotifyEvent.Remove(const AKey: String): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_String_TNotifyEvent.GetEnumerator: IPairEnumerator_String_TNotifyEvent;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_String_TNotifyEvent.GetItem(const AKey: String): TNotifyEvent;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=TNotifyEvent(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_String_TNotifyEvent.SetItem(const AKey: String; const AValue: TNotifyEvent);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_String_TNotifyEvent.GetKeyEnumerable: IEnumerable_String;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_String_TNotifyEvent.GetValueEnumerable: IEnumerable_TNotifyEvent;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_String_TNotifyEvent.Add(const AKey: String; const AValue: TNotifyEvent);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_String_TNotifyEvent.TryGetValue(const AKey: String; out AValue: TNotifyEvent): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=TNotifyEvent(ValuePointer^);
end;

type

  CDictionary_String_TObject = class(CDictionaryBase, IDictionary_String_TObject)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_String_TObject)
     public
      function GetCurrent: TPair_String_TObject;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_String)
     public
      function GetCurrent: String;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TObject)
     public
      function GetCurrent: TObject;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_String)
     public
      function GetEnumerator: IEnumerator_String;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TObject)
     public
      function GetEnumerator: IEnumerator_TObject;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TObject>;
    end;

   strict private
    FOwnValues: Boolean;
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_TObject;
    procedure Add(const AKey: String; const AValue: TObject);
    function  Contains(const AKey: String; const AValue: TObject): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): TObject;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_TObject;
    function  GetItem(const AKey: String): TObject;
    procedure SetItem(const AKey: String; const AValue: TObject);
    function  TryGetValue(const AKey: String; out AValue: TObject): Boolean;
   public
    constructor Create(AOwnValues: Boolean);
  end;

{ CDictionary_String_TObject.CPairEnumerator }

function CDictionary_String_TObject.CPairEnumerator.GetCurrent: TPair_String_TObject;
begin
  Result.Key:=String(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=TObject(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_TObject.CKeyEnumerator }

function CDictionary_String_TObject.CKeyEnumerator.GetCurrent: String;
begin
  Result:=String(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_String_TObject.CValueEnumerator }

function CDictionary_String_TObject.CValueEnumerator.GetCurrent: TObject;
begin
  Result:=TObject(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_String_TObject.CKeyEnumerable }

function CDictionary_String_TObject.CKeyEnumerable.GetEnumerator: IEnumerator_String;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_String_TObject.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<String>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_TObject.CValueEnumerable }

function CDictionary_String_TObject.CValueEnumerable.GetEnumerator: IEnumerator_TObject;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_String_TObject.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<TObject>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_String_TObject }

constructor CDictionary_String_TObject.Create(AOwnValues: Boolean);
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<String>),TypeInfo(TArray<TObject>));
  FOwnValues:=AOwnValues;
  PDynArray(@FSynDict.Values).NoFinalize:=not FOwnValues;
end;

function CDictionary_String_TObject.Contains(const AKey: String; const AValue: TObject): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (TObject(ValuePointer^)=AValue);
end;

function CDictionary_String_TObject.ContainsKey(const AKey: String): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_String_TObject.Extract(const AKey: String): TObject;
begin
  var ValuesDynArray: PDynArray:=PDynArray(@FSynDict.Values);
  if FOwnValues
    then ValuesDynArray.NoFinalize:=true;
  FSynDict.FindAndExtract(AKey,Result);
  if FOwnValues
    then ValuesDynArray.NoFinalize:=false;
end;

function CDictionary_String_TObject.Remove(const AKey: String): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_String_TObject.GetEnumerator: IPairEnumerator_String_TObject;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_String_TObject.GetItem(const AKey: String): TObject;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=TObject(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_String_TObject.SetItem(const AKey: String; const AValue: TObject);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_String_TObject.GetKeyEnumerable: IEnumerable_String;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_String_TObject.GetValueEnumerable: IEnumerable_TObject;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_String_TObject.Add(const AKey: String; const AValue: TObject);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_String_TObject.TryGetValue(const AKey: String; out AValue: TObject): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=TObject(ValuePointer^);
end;

type

  CDictionary_TObject_TObject = class(CDictionaryBase, IDictionary_TObject_TObject)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_TObject_TObject)
     public
      function GetCurrent: TPair_TObject_TObject;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TObject)
     public
      function GetCurrent: TObject;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TObject)
     public
      function GetCurrent: TObject;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TObject)
     public
      function GetEnumerator: IEnumerator_TObject;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TObject>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TObject)
     public
      function GetEnumerator: IEnumerator_TObject;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TObject>;
    end;

   strict private
    FOwnKeys: Boolean;
    FOwnValues: Boolean;
    function  GetKeyEnumerable: IEnumerable_TObject;
    function  GetValueEnumerable: IEnumerable_TObject;
    procedure Add(const AKey: TObject; const AValue: TObject);
    function  Contains(const AKey: TObject; const AValue: TObject): Boolean;
    function  ContainsKey(const AKey: TObject): Boolean;
    function  Extract(const AKey: TObject): TObject;
    function  Remove(const AKey: TObject): Boolean;
    function  GetEnumerator: IPairEnumerator_TObject_TObject;
    function  GetItem(const AKey: TObject): TObject;
    procedure SetItem(const AKey: TObject; const AValue: TObject);
    function  TryGetValue(const AKey: TObject; out AValue: TObject): Boolean;
   public
    constructor Create(AOwnKeys: Boolean; AOwnValues: Boolean);
  end;

{ CDictionary_TObject_TObject.CPairEnumerator }

function CDictionary_TObject_TObject.CPairEnumerator.GetCurrent: TPair_TObject_TObject;
begin
  Result.Key:=TObject(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=TObject(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TObject_TObject.CKeyEnumerator }

function CDictionary_TObject_TObject.CKeyEnumerator.GetCurrent: TObject;
begin
  Result:=TObject(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_TObject_TObject.CValueEnumerator }

function CDictionary_TObject_TObject.CValueEnumerator.GetCurrent: TObject;
begin
  Result:=TObject(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TObject_TObject.CKeyEnumerable }

function CDictionary_TObject_TObject.CKeyEnumerable.GetEnumerator: IEnumerator_TObject;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_TObject_TObject.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<TObject>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TObject_TObject.CValueEnumerable }

function CDictionary_TObject_TObject.CValueEnumerable.GetEnumerator: IEnumerator_TObject;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_TObject_TObject.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<TObject>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TObject_TObject }

constructor CDictionary_TObject_TObject.Create(AOwnKeys: Boolean; AOwnValues: Boolean);
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<TObject>),TypeInfo(TArray<TObject>));
  FOwnKeys:=AOwnKeys;
  PDynArray(@FSynDict.Keys.InternalDynArray).NoFinalize:=not FOwnKeys;
  FOwnValues:=AOwnValues;
  PDynArray(@FSynDict.Values).NoFinalize:=not FOwnValues;
end;

function CDictionary_TObject_TObject.Contains(const AKey: TObject; const AValue: TObject): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (TObject(ValuePointer^)=AValue);
end;

function CDictionary_TObject_TObject.ContainsKey(const AKey: TObject): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_TObject_TObject.Extract(const AKey: TObject): TObject;
begin
  var ValuesDynArray: PDynArray:=PDynArray(@FSynDict.Values);
  if FOwnValues
    then ValuesDynArray.NoFinalize:=true;
  FSynDict.FindAndExtract(AKey,Result);
  if FOwnValues
    then ValuesDynArray.NoFinalize:=false;
end;

function CDictionary_TObject_TObject.Remove(const AKey: TObject): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_TObject_TObject.GetEnumerator: IPairEnumerator_TObject_TObject;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_TObject_TObject.GetItem(const AKey: TObject): TObject;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=TObject(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_TObject_TObject.SetItem(const AKey: TObject; const AValue: TObject);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_TObject_TObject.GetKeyEnumerable: IEnumerable_TObject;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_TObject_TObject.GetValueEnumerable: IEnumerable_TObject;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_TObject_TObject.Add(const AKey: TObject; const AValue: TObject);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_TObject_TObject.TryGetValue(const AKey: TObject; out AValue: TObject): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=TObject(ValuePointer^);
end;

type

  CDictionary_TClass_TObject = class(CDictionaryBase, IDictionary_TClass_TObject)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_TClass_TObject)
     public
      function GetCurrent: TPair_TClass_TObject;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TClass)
     public
      function GetCurrent: TClass;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TObject)
     public
      function GetCurrent: TObject;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TClass)
     public
      function GetEnumerator: IEnumerator_TClass;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TClass>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TObject)
     public
      function GetEnumerator: IEnumerator_TObject;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TObject>;
    end;

   strict private
    FOwnValues: Boolean;
    function  GetKeyEnumerable: IEnumerable_TClass;
    function  GetValueEnumerable: IEnumerable_TObject;
    procedure Add(const AKey: TClass; const AValue: TObject);
    function  Contains(const AKey: TClass; const AValue: TObject): Boolean;
    function  ContainsKey(const AKey: TClass): Boolean;
    function  Extract(const AKey: TClass): TObject;
    function  Remove(const AKey: TClass): Boolean;
    function  GetEnumerator: IPairEnumerator_TClass_TObject;
    function  GetItem(const AKey: TClass): TObject;
    procedure SetItem(const AKey: TClass; const AValue: TObject);
    function  TryGetValue(const AKey: TClass; out AValue: TObject): Boolean;
   public
    constructor Create(AOwnValues: Boolean);
  end;

{ CDictionary_TClass_TObject.CPairEnumerator }

function CDictionary_TClass_TObject.CPairEnumerator.GetCurrent: TPair_TClass_TObject;
begin
  Result.Key:=TClass(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=TObject(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TClass_TObject.CKeyEnumerator }

function CDictionary_TClass_TObject.CKeyEnumerator.GetCurrent: TClass;
begin
  Result:=TClass(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_TClass_TObject.CValueEnumerator }

function CDictionary_TClass_TObject.CValueEnumerator.GetCurrent: TObject;
begin
  Result:=TObject(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TClass_TObject.CKeyEnumerable }

function CDictionary_TClass_TObject.CKeyEnumerable.GetEnumerator: IEnumerator_TClass;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_TClass_TObject.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<TClass>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TClass_TObject.CValueEnumerable }

function CDictionary_TClass_TObject.CValueEnumerable.GetEnumerator: IEnumerator_TObject;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_TClass_TObject.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<TObject>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TClass_TObject }

constructor CDictionary_TClass_TObject.Create(AOwnValues: Boolean);
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<TClass>),TypeInfo(TArray<TObject>));
  FOwnValues:=AOwnValues;
  PDynArray(@FSynDict.Values).NoFinalize:=not FOwnValues;
end;

function CDictionary_TClass_TObject.Contains(const AKey: TClass; const AValue: TObject): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (TObject(ValuePointer^)=AValue);
end;

function CDictionary_TClass_TObject.ContainsKey(const AKey: TClass): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_TClass_TObject.Extract(const AKey: TClass): TObject;
begin
  var ValuesDynArray: PDynArray:=PDynArray(@FSynDict.Values);
  if FOwnValues
    then ValuesDynArray.NoFinalize:=true;
  FSynDict.FindAndExtract(AKey,Result);
  if FOwnValues
    then ValuesDynArray.NoFinalize:=false;
end;

function CDictionary_TClass_TObject.Remove(const AKey: TClass): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_TClass_TObject.GetEnumerator: IPairEnumerator_TClass_TObject;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_TClass_TObject.GetItem(const AKey: TClass): TObject;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=TObject(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_TClass_TObject.SetItem(const AKey: TClass; const AValue: TObject);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_TClass_TObject.GetKeyEnumerable: IEnumerable_TClass;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_TClass_TObject.GetValueEnumerable: IEnumerable_TObject;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_TClass_TObject.Add(const AKey: TClass; const AValue: TObject);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_TClass_TObject.TryGetValue(const AKey: TClass; out AValue: TObject): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=TObject(ValuePointer^);
end;

type

  CDictionary_TGUID_Boolean = class(CDictionaryBase, IDictionary_TGUID_Boolean)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_TGUID_Boolean)
     public
      function GetCurrent: TPair_TGUID_Boolean;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TGUID)
     public
      function GetCurrent: TGUID;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_Boolean)
     public
      function GetCurrent: Boolean;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TGUID)
     public
      function GetEnumerator: IEnumerator_TGUID;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TGUID>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_Boolean)
     public
      function GetEnumerator: IEnumerator_Boolean;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Boolean>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_TGUID;
    function  GetValueEnumerable: IEnumerable_Boolean;
    procedure Add(const AKey: TGUID; const AValue: Boolean);
    function  Contains(const AKey: TGUID; const AValue: Boolean): Boolean;
    function  ContainsKey(const AKey: TGUID): Boolean;
    function  Extract(const AKey: TGUID): Boolean;
    function  Remove(const AKey: TGUID): Boolean;
    function  GetEnumerator: IPairEnumerator_TGUID_Boolean;
    function  GetItem(const AKey: TGUID): Boolean;
    procedure SetItem(const AKey: TGUID; const AValue: Boolean);
    function  TryGetValue(const AKey: TGUID; out AValue: Boolean): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_TGUID_Boolean.CPairEnumerator }

function CDictionary_TGUID_Boolean.CPairEnumerator.GetCurrent: TPair_TGUID_Boolean;
begin
  Result.Key:=TGUID(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=Boolean(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_Boolean.CKeyEnumerator }

function CDictionary_TGUID_Boolean.CKeyEnumerator.GetCurrent: TGUID;
begin
  Result:=TGUID(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_Boolean.CValueEnumerator }

function CDictionary_TGUID_Boolean.CValueEnumerator.GetCurrent: Boolean;
begin
  Result:=Boolean(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_Boolean.CKeyEnumerable }

function CDictionary_TGUID_Boolean.CKeyEnumerable.GetEnumerator: IEnumerator_TGUID;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_TGUID_Boolean.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<TGUID>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TGUID_Boolean.CValueEnumerable }

function CDictionary_TGUID_Boolean.CValueEnumerable.GetEnumerator: IEnumerator_Boolean;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_TGUID_Boolean.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<Boolean>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TGUID_Boolean }

constructor CDictionary_TGUID_Boolean.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<TGUID>),TypeInfo(TArray<Boolean>));
end;

function CDictionary_TGUID_Boolean.Contains(const AKey: TGUID; const AValue: Boolean): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (Boolean(ValuePointer^)=AValue);
end;

function CDictionary_TGUID_Boolean.ContainsKey(const AKey: TGUID): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_TGUID_Boolean.Extract(const AKey: TGUID): Boolean;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_TGUID_Boolean.Remove(const AKey: TGUID): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_TGUID_Boolean.GetEnumerator: IPairEnumerator_TGUID_Boolean;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_TGUID_Boolean.GetItem(const AKey: TGUID): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=Boolean(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_TGUID_Boolean.SetItem(const AKey: TGUID; const AValue: Boolean);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_TGUID_Boolean.GetKeyEnumerable: IEnumerable_TGUID;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_TGUID_Boolean.GetValueEnumerable: IEnumerable_Boolean;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_TGUID_Boolean.Add(const AKey: TGUID; const AValue: Boolean);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_TGUID_Boolean.TryGetValue(const AKey: TGUID; out AValue: Boolean): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=Boolean(ValuePointer^);
end;

type

  CDictionary_TGUID_Integer = class(CDictionaryBase, IDictionary_TGUID_Integer)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_TGUID_Integer)
     public
      function GetCurrent: TPair_TGUID_Integer;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TGUID)
     public
      function GetCurrent: TGUID;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_Integer)
     public
      function GetCurrent: Integer;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TGUID)
     public
      function GetEnumerator: IEnumerator_TGUID;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TGUID>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_Integer)
     public
      function GetEnumerator: IEnumerator_Integer;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Integer>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_TGUID;
    function  GetValueEnumerable: IEnumerable_Integer;
    procedure Add(const AKey: TGUID; const AValue: Integer);
    function  Contains(const AKey: TGUID; const AValue: Integer): Boolean;
    function  ContainsKey(const AKey: TGUID): Boolean;
    function  Extract(const AKey: TGUID): Integer;
    function  Remove(const AKey: TGUID): Boolean;
    function  GetEnumerator: IPairEnumerator_TGUID_Integer;
    function  GetItem(const AKey: TGUID): Integer;
    procedure SetItem(const AKey: TGUID; const AValue: Integer);
    function  TryGetValue(const AKey: TGUID; out AValue: Integer): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_TGUID_Integer.CPairEnumerator }

function CDictionary_TGUID_Integer.CPairEnumerator.GetCurrent: TPair_TGUID_Integer;
begin
  Result.Key:=TGUID(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=Integer(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_Integer.CKeyEnumerator }

function CDictionary_TGUID_Integer.CKeyEnumerator.GetCurrent: TGUID;
begin
  Result:=TGUID(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_Integer.CValueEnumerator }

function CDictionary_TGUID_Integer.CValueEnumerator.GetCurrent: Integer;
begin
  Result:=Integer(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_Integer.CKeyEnumerable }

function CDictionary_TGUID_Integer.CKeyEnumerable.GetEnumerator: IEnumerator_TGUID;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_TGUID_Integer.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<TGUID>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TGUID_Integer.CValueEnumerable }

function CDictionary_TGUID_Integer.CValueEnumerable.GetEnumerator: IEnumerator_Integer;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_TGUID_Integer.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<Integer>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TGUID_Integer }

constructor CDictionary_TGUID_Integer.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<TGUID>),TypeInfo(TArray<Integer>));
end;

function CDictionary_TGUID_Integer.Contains(const AKey: TGUID; const AValue: Integer): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (Integer(ValuePointer^)=AValue);
end;

function CDictionary_TGUID_Integer.ContainsKey(const AKey: TGUID): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_TGUID_Integer.Extract(const AKey: TGUID): Integer;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_TGUID_Integer.Remove(const AKey: TGUID): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_TGUID_Integer.GetEnumerator: IPairEnumerator_TGUID_Integer;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_TGUID_Integer.GetItem(const AKey: TGUID): Integer;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=Integer(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_TGUID_Integer.SetItem(const AKey: TGUID; const AValue: Integer);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_TGUID_Integer.GetKeyEnumerable: IEnumerable_TGUID;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_TGUID_Integer.GetValueEnumerable: IEnumerable_Integer;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_TGUID_Integer.Add(const AKey: TGUID; const AValue: Integer);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_TGUID_Integer.TryGetValue(const AKey: TGUID; out AValue: Integer): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=Integer(ValuePointer^);
end;

type

  CDictionary_TGUID_String = class(CDictionaryBase, IDictionary_TGUID_String)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_TGUID_String)
     public
      function GetCurrent: TPair_TGUID_String;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TGUID)
     public
      function GetCurrent: TGUID;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_String)
     public
      function GetCurrent: String;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TGUID)
     public
      function GetEnumerator: IEnumerator_TGUID;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TGUID>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_String)
     public
      function GetEnumerator: IEnumerator_String;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_TGUID;
    function  GetValueEnumerable: IEnumerable_String;
    procedure Add(const AKey: TGUID; const AValue: String);
    function  Contains(const AKey: TGUID; const AValue: String): Boolean;
    function  ContainsKey(const AKey: TGUID): Boolean;
    function  Extract(const AKey: TGUID): String;
    function  Remove(const AKey: TGUID): Boolean;
    function  GetEnumerator: IPairEnumerator_TGUID_String;
    function  GetItem(const AKey: TGUID): String;
    procedure SetItem(const AKey: TGUID; const AValue: String);
    function  TryGetValue(const AKey: TGUID; out AValue: String): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_TGUID_String.CPairEnumerator }

function CDictionary_TGUID_String.CPairEnumerator.GetCurrent: TPair_TGUID_String;
begin
  Result.Key:=TGUID(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=String(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_String.CKeyEnumerator }

function CDictionary_TGUID_String.CKeyEnumerator.GetCurrent: TGUID;
begin
  Result:=TGUID(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_String.CValueEnumerator }

function CDictionary_TGUID_String.CValueEnumerator.GetCurrent: String;
begin
  Result:=String(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_String.CKeyEnumerable }

function CDictionary_TGUID_String.CKeyEnumerable.GetEnumerator: IEnumerator_TGUID;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_TGUID_String.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<TGUID>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TGUID_String.CValueEnumerable }

function CDictionary_TGUID_String.CValueEnumerable.GetEnumerator: IEnumerator_String;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_TGUID_String.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<String>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TGUID_String }

constructor CDictionary_TGUID_String.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<TGUID>),TypeInfo(TArray<String>));
end;

function CDictionary_TGUID_String.Contains(const AKey: TGUID; const AValue: String): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (String(ValuePointer^)=AValue);
end;

function CDictionary_TGUID_String.ContainsKey(const AKey: TGUID): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_TGUID_String.Extract(const AKey: TGUID): String;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_TGUID_String.Remove(const AKey: TGUID): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_TGUID_String.GetEnumerator: IPairEnumerator_TGUID_String;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_TGUID_String.GetItem(const AKey: TGUID): String;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=String(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_TGUID_String.SetItem(const AKey: TGUID; const AValue: String);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_TGUID_String.GetKeyEnumerable: IEnumerable_TGUID;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_TGUID_String.GetValueEnumerable: IEnumerable_String;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_TGUID_String.Add(const AKey: TGUID; const AValue: String);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_TGUID_String.TryGetValue(const AKey: TGUID; out AValue: String): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=String(ValuePointer^);
end;

type

  CDictionary_TGUID_TGUID = class(CDictionaryBase, IDictionary_TGUID_TGUID)
   strict private type

    CPairEnumerator = class(CDictionaryBase.CEnumerator, IPairEnumerator_TGUID_TGUID)
     public
      function GetCurrent: TPair_TGUID_TGUID;
    end;

    CKeyEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TGUID)
     public
      function GetCurrent: TGUID;
    end;

    CValueEnumerator = class(CDictionaryBase.CEnumerator, IEnumerator_TGUID)
     public
      function GetCurrent: TGUID;
    end;

    CKeyEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TGUID)
     public
      function GetEnumerator: IEnumerator_TGUID;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TGUID>;
    end;

    CValueEnumerable = class(CDictionaryBase.CCollection, IEnumerable_TGUID)
     public
      function GetEnumerator: IEnumerator_TGUID;
      function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TGUID>;
    end;

   strict private
    function  GetKeyEnumerable: IEnumerable_TGUID;
    function  GetValueEnumerable: IEnumerable_TGUID;
    procedure Add(const AKey: TGUID; const AValue: TGUID);
    function  Contains(const AKey: TGUID; const AValue: TGUID): Boolean;
    function  ContainsKey(const AKey: TGUID): Boolean;
    function  Extract(const AKey: TGUID): TGUID;
    function  Remove(const AKey: TGUID): Boolean;
    function  GetEnumerator: IPairEnumerator_TGUID_TGUID;
    function  GetItem(const AKey: TGUID): TGUID;
    procedure SetItem(const AKey: TGUID; const AValue: TGUID);
    function  TryGetValue(const AKey: TGUID; out AValue: TGUID): Boolean;
   public
    constructor Create;
  end;

{ CDictionary_TGUID_TGUID.CPairEnumerator }

function CDictionary_TGUID_TGUID.CPairEnumerator.GetCurrent: TPair_TGUID_TGUID;
begin
  Result.Key:=TGUID(FSynDict.Keys.ItemPtr(FIndex)^);
  Result.Value:=TGUID(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_TGUID.CKeyEnumerator }

function CDictionary_TGUID_TGUID.CKeyEnumerator.GetCurrent: TGUID;
begin
  Result:=TGUID(FSynDict.Keys.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_TGUID.CValueEnumerator }

function CDictionary_TGUID_TGUID.CValueEnumerator.GetCurrent: TGUID;
begin
  Result:=TGUID(FSynDict.Values.ItemPtr(FIndex)^);
end;

{ CDictionary_TGUID_TGUID.CKeyEnumerable }

function CDictionary_TGUID_TGUID.CKeyEnumerable.GetEnumerator: IEnumerator_TGUID;
begin
  Result:=CKeyEnumerator.Create(FSynDict);
end;

{ ----------------------------------------------------------------------- }

function CDictionary_TGUID_TGUID.CKeyEnumerable.ToArray(AOffset, ACount: Integer): TArray<TGUID>;
begin
  FSynDict.Keys.InternalDynArray.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TGUID_TGUID.CValueEnumerable }

function CDictionary_TGUID_TGUID.CValueEnumerable.GetEnumerator: IEnumerator_TGUID;
begin
  Result:=CValueEnumerator.Create(FSynDict);
end;

function CDictionary_TGUID_TGUID.CValueEnumerable.ToArray(AOffset, ACount: Integer): TArray<TGUID>;
begin
  FSynDict.Values.SliceAsDynArray(@Result,AOffset,ACount);
end;

{ CDictionary_TGUID_TGUID }

constructor CDictionary_TGUID_TGUID.Create;
begin
  FSynDict:=TSynDictionary.Create(TypeInfo(TArray<TGUID>),TypeInfo(TArray<TGUID>));
end;

function CDictionary_TGUID_TGUID.Contains(const AKey: TGUID; const AValue: TGUID): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  Result:=Assigned(ValuePointer) and (TGUID(ValuePointer^)=AValue);
end;

function CDictionary_TGUID_TGUID.ContainsKey(const AKey: TGUID): Boolean;
begin
  Result:=FSynDict.Exists(AKey);
end;

function CDictionary_TGUID_TGUID.Extract(const AKey: TGUID): TGUID;
begin
  FSynDict.FindAndExtract(AKey,Result);
end;

function CDictionary_TGUID_TGUID.Remove(const AKey: TGUID): Boolean;
begin
  Result:=FSynDict.Delete(AKey)>=0;
end;

function CDictionary_TGUID_TGUID.GetEnumerator: IPairEnumerator_TGUID_TGUID;
begin
  Result:=CPairEnumerator.Create(FSynDict);
end;

function CDictionary_TGUID_TGUID.GetItem(const AKey: TGUID): TGUID;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Assigned(ValuePointer)
    then Result:=TGUID(ValuePointer^)
    else raise Exception.Create('Key not found');
end;

procedure CDictionary_TGUID_TGUID.SetItem(const AKey: TGUID; const AValue: TGUID);
begin
  FSynDict.AddOrUpdate(AKey,AValue);
end;

function CDictionary_TGUID_TGUID.GetKeyEnumerable: IEnumerable_TGUID;
begin
  Result:=CKeyEnumerable.Create(FSynDict);
end;

function CDictionary_TGUID_TGUID.GetValueEnumerable: IEnumerable_TGUID;
begin
  Result:=CValueEnumerable.Create(FSynDict);
end;

procedure CDictionary_TGUID_TGUID.Add(const AKey: TGUID; const AValue: TGUID);
begin
  FSynDict.Add(AKey,AValue);
end;

function CDictionary_TGUID_TGUID.TryGetValue(const AKey: TGUID; out AValue: TGUID): Boolean;
begin
  var ValuePointer: Pointer:=FSynDict.FindValue(AKey);
  if Capture(Assigned(ValuePointer),Result)
    then AValue:=TGUID(ValuePointer^);
end;

{ TCollections }

class function TCollections.CreateDictionary_Integer_IInterface: IDictionary_Integer_IInterface;
begin
  Result:=CDictionary_Integer_IInterface.Create;
end;

class function TCollections.CreateDictionary_Integer_String: IDictionary_Integer_String;
begin
  Result:=CDictionary_Integer_String.Create;
end;

class function TCollections.CreateDictionary_Integer_TGUID: IDictionary_Integer_TGUID;
begin
  Result:=CDictionary_Integer_TGUID.Create;
end;

class function TCollections.CreateDictionary_Integer_TObject(AOwnValues: Boolean): IDictionary_Integer_TObject;
begin
  Result:=CDictionary_Integer_TObject.Create(AOwnValues);
end;

class function TCollections.CreateDictionary_String_Integer: IDictionary_String_Integer;
begin
  Result:=CDictionary_String_Integer.Create;
end;

class function TCollections.CreateDictionary_String_String: IDictionary_String_String;
begin
  Result:=CDictionary_String_String.Create;
end;

class function TCollections.CreateDictionary_String_TGUID: IDictionary_String_TGUID;
begin
  Result:=CDictionary_String_TGUID.Create;
end;

class function TCollections.CreateDictionary_String_TNotifyEvent: IDictionary_String_TNotifyEvent;
begin
  Result:=CDictionary_String_TNotifyEvent.Create;
end;

class function TCollections.CreateDictionary_String_TObject(AOwnValues: Boolean): IDictionary_String_TObject;
begin
  Result:=CDictionary_String_TObject.Create(AOwnValues);
end;

class function TCollections.CreateDictionary_TObject_TObject(AOwnKeys: Boolean; AOwnValues: Boolean): IDictionary_TObject_TObject;
begin
  Result:=CDictionary_TObject_TObject.Create(AOwnKeys,AOwnValues);
end;

class function TCollections.CreateDictionary_TClass_TObject(AOwnValues: Boolean): IDictionary_TClass_TObject;
begin
  Result:=CDictionary_TClass_TObject.Create(AOwnValues);
end;

class function TCollections.CreateDictionary_TGUID_Boolean: IDictionary_TGUID_Boolean;
begin
  Result:=CDictionary_TGUID_Boolean.Create;
end;

class function TCollections.CreateDictionary_TGUID_Integer: IDictionary_TGUID_Integer;
begin
  Result:=CDictionary_TGUID_Integer.Create;
end;

class function TCollections.CreateDictionary_TGUID_String: IDictionary_TGUID_String;
begin
  Result:=CDictionary_TGUID_String.Create;
end;

class function TCollections.CreateDictionary_TGUID_TGUID: IDictionary_TGUID_TGUID;
begin
  Result:=CDictionary_TGUID_TGUID.Create;
end;

{$ENDREGION 'INCLUDE-PARTIAL / System.Dictionary-implementation.part.pas'}

{ ======================================================================= }

end.
