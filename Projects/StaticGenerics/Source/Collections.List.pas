unit Collections.List;

interface

uses

  System.Classes,
  System.Contnrs,
  System.SysUtils,

  mormot.core.base,
  mormot.core.collections,
  mormot.core.data,
  mormot.core.rtti,

  Collections.Interfaces;

type

  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  CList = class(TIListParent, IList)
   strict protected // IList member
    function  Any: Boolean;
    procedure Delete(AIndex: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function  IsEmpty: Boolean;
    procedure Move(ASourceIndex, ATargetIndex: Integer);
    procedure Sort(ACompare: TOnDynArraySortCompare);
  end;

  CListEnumeratorBase = class(TInterfacedObject)
   strict protected
    FList: CList;
    FIndex: Integer;
    function MoveNext: Boolean;
   public
    constructor Create(AList: CList);
  end;

implementation


{ ======================================================================= }
{ CList                                                                   }
{ ======================================================================= }

function CList.Any: Boolean;
begin
  Result:=Count>0;
end;

{ ----------------------------------------------------------------------- }

procedure CList.Delete(AIndex: Integer);
begin
  inherited Delete(AIndex);
end;

{ ----------------------------------------------------------------------- }

procedure CList.DeleteRange(AIndex, ACount: Integer);
begin
  for var Loop:=1 to ACount
    do Delete(AIndex);
  { TODO : DeleteRange mit einem Test absichern, falls das zum Einsatz kommen sollte }
end;

{ ----------------------------------------------------------------------- }

procedure CList.Exchange(AIndex1, AIndex2: Integer);
var
  TempIndex: PtrInt;
begin
  if (PtrUInt(AIndex1)>=PtrUInt(Count)) or
     (PtrUInt(AIndex2)>=PtrUInt(Count))
    then raise EListError.Create('Index out of bounds');
  if AIndex1=AIndex2
    then Exit;
  TempIndex:=fDynArray.New;
  try
    fDynArray.ItemCopy(fDynArray.ItemPtr(AIndex1),fDynArray.ItemPtr(TempIndex)); // temp = item1
    fDynArray.ItemCopy(fDynArray.ItemPtr(AIndex2),fDynArray.ItemPtr(AIndex1));   // item1 = item2
    fDynArray.ItemCopy(fDynArray.ItemPtr(TempIndex),fDynArray.ItemPtr(AIndex2)); // item2 = temp
  finally
    fDynArray.Delete(TempIndex);
  end;
end;

{ ----------------------------------------------------------------------- }

function CList.IsEmpty: Boolean;
begin
  Result:=Count=0;
end;

{ ----------------------------------------------------------------------- }

procedure CList.Move(ASourceIndex, ATargetIndex: Integer);
var
  I: Integer;
begin
  if
    (PtrUInt(ASourceIndex)>=PtrUInt(Count)) or
    (PtrUInt(ATargetIndex)>=PtrUInt(Count))
    then raise EListError.Create('Index out of bounds');
  if ASourceIndex=ATargetIndex then
    Exit;
  if ASourceIndex<ATargetIndex then
  begin
    for I:=ASourceIndex to ATargetIndex-1
      do Exchange(I,I+1)
  end else
  begin
    for I:=ASourceIndex downto ATargetIndex+1
      do Exchange(I,I-1);
  end;
end;

{ ----------------------------------------------------------------------- }

procedure CList.Sort(ACompare: TOnDynArraySortCompare);
begin
  if Assigned(ACompare)
    then inherited Sort(ACompare)
    else inherited Sort(TDynArraySortCompare(nil));
end;

{ CListEnumeratorBase }

constructor CListEnumeratorBase.Create(AList: CList);
begin
  FList:=AList;
  FIndex:=-1;
end;

{ ----------------------------------------------------------------------- }

function CListEnumeratorBase.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result:=FIndex<FList.Count;
end;

end.
