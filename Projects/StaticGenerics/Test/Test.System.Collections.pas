unit Test.System.Collections;

interface

uses

  System.Classes,
  System.SysUtils,

  DUnitX.TestFramework,

  System.Collections.Factory,
  System.Collections.Interfaces;

type

  [TestFixture]
  TestList_String = class
   public
    [Test]
    procedure Constructors;
    [Test]
    procedure Exchange;
    [Test]
    procedure Move;
    [Test]
    procedure SimpleTests;
    [Test]
    procedure SortTest;
    [Test]
    procedure UniqueList;
  end;

  [TestFixture]
  TestList_Integer = class
   public
    [Test]
    procedure SimpleTests;
  end;

  [TestFixture]
  TestList_TObject = class
   public
    [Test]
    procedure Add;
    [Test]
    procedure Exchange;
    [Test]
    procedure Move;
  end;

  [TestFixture]
  TestList_TComponent = class
   public
    [Test]
    procedure Add;
    [Test]
    procedure CastToEnumerable;
    [Test]
    procedure ToArray;
  end;

  [TestFixture]
  TestDictionary_String_String = class
   public
    [Test]
    procedure SimpleTests;
  end;

  [TestFixture]
  TestDictionary_String_TObject = class
   public
    [Test]
    procedure OwnershipTests;
  end;

implementation

type

  CFreeNotifyObject = class
   strict private
    FOnFreeProc: TProc<TObject>;
   public
    constructor Create(const AOnFreeProc: TProc<TObject>);
    destructor Destroy; override;
  end;

{ ======================================================================= }
{ CFreeNotifyObject                                                       }
{ ======================================================================= }

constructor CFreeNotifyObject.Create(const AOnFreeProc: TProc<TObject>);
begin
  inherited Create;
  FOnFreeProc:=AOnFreeProc;
end;

{ ----------------------------------------------------------------------- }

destructor CFreeNotifyObject.Destroy;
begin
  if Assigned(FOnFreeProc)
    then FOnFreeProc(Self);
  inherited;
end;

{ ======================================================================= }
{ TestList_String                                                         }
{ ======================================================================= }

procedure TestList_String.Constructors;
const
  FirstItem ='Hello World!';
  SecondItem='Some random content';
  ThirdItem ='Lorem ipsum et delomet';
begin
  var Values: TArray<String>;
  SetLength(Values,3);
  Values[0]:=FirstItem;
  Values[1]:=SecondItem;
  Values[2]:=ThirdItem;

  var List:=TCollections.CreateList_String(Values);
  Assert.AreEqual(Length(Values),List.Count);
  Assert.AreEqual(FirstItem,List[0]);
  Assert.AreEqual(SecondItem,List[1]);
  Assert.AreEqual(ThirdItem,List[2]);

  var SecondList:=TCollections.CreateList_String(List as IEnumerable_String);
  Assert.AreEqual(List.Count,SecondList.Count);
  Assert.AreEqual(FirstItem,SecondList[0]);
  Assert.AreEqual(SecondItem,SecondList[1]);
  Assert.AreEqual(ThirdItem,SecondList[2]);
end;

{ ----------------------------------------------------------------------- }

procedure TestList_String.Exchange;
var
  List: IList_String;

{ -------------------------- }

  procedure CheckInvalidIndex(AIndex1, AIndex2: Integer);
  begin
    Assert.WillRaise(
      procedure
      begin
        List.Exchange(AIndex1,AIndex2);
      end,EListError);
  end;

{ -------------------------- }

begin
  List:=TCollections.CreateList_String;
  List.Add('A');
  List.Add('B');
  List.Add('C');
  List.Add('D');

  List.Exchange(1,2);
  Assert.AreEqual('A',List[0]);
  Assert.AreEqual('C',List[1]);
  Assert.AreEqual('B',List[2]);
  Assert.AreEqual('D',List[3]);

  List.Exchange(1,2);
  Assert.AreEqual('A',List[0]);
  Assert.AreEqual('B',List[1]);
  Assert.AreEqual('C',List[2]);
  Assert.AreEqual('D',List[3]);

  List.Exchange(0,3);
  Assert.AreEqual('D',List[0]);
  Assert.AreEqual('B',List[1]);
  Assert.AreEqual('C',List[2]);
  Assert.AreEqual('A',List[3]);

  List.Exchange(1, 1);
  Assert.AreEqual('D',List[0]);
  Assert.AreEqual('B',List[1]);
  Assert.AreEqual('C',List[2]);
  Assert.AreEqual('A',List[3]);

  CheckInvalidIndex(-1,1);
  CheckInvalidIndex(1,-1);
  CheckInvalidIndex(0,4);
  CheckInvalidIndex(1,4);
  CheckInvalidIndex(4,0);
  CheckInvalidIndex(4,1);
end;

{ ----------------------------------------------------------------------- }

procedure TestList_String.Move;
var
  List: IList_String;

{ -------------------------- }

  procedure CheckInvalidIndex(ASourceIndex, ATargetIndex: Integer);
  begin
    Assert.WillRaise(
      procedure
      begin
        List.Move(ASourceIndex,ATargetIndex);
      end,EListError);
  end;

{ -------------------------- }

begin
  List:=TCollections.CreateList_String;
  List.Add('A');
  List.Add('B');
  List.Add('C');
  List.Add('D');

  List.Move(0,2);
  Assert.AreEqual('B',List[0]);
  Assert.AreEqual('C',List[1]);
  Assert.AreEqual('A',List[2]);
  Assert.AreEqual('D',List[3]);

  List.Move(3,1);
  Assert.AreEqual('B',List[0]);
  Assert.AreEqual('D',List[1]);
  Assert.AreEqual('C',List[2]);
  Assert.AreEqual('A',List[3]);

  List.Move(1,1);
  Assert.AreEqual('B',List[0]);
  Assert.AreEqual('D',List[1]);
  Assert.AreEqual('C',List[2]);
  Assert.AreEqual('A',List[3]);

  List.Move(0,3);
  Assert.AreEqual('D',List[0]);
  Assert.AreEqual('C',List[1]);
  Assert.AreEqual('A',List[2]);
  Assert.AreEqual('B',List[3]);

  List.Move(2, 0);
  Assert.AreEqual('A',List[0]);
  Assert.AreEqual('D',List[1]);
  Assert.AreEqual('C',List[2]);
  Assert.AreEqual('B',List[3]);

  CheckInvalidIndex(-1,1);
  CheckInvalidIndex(1,-1);
  CheckInvalidIndex(0,4);
  CheckInvalidIndex(4,0);
end;

{ ----------------------------------------------------------------------- }

procedure TestList_String.SimpleTests;
const
  TestLoops = 1000;
begin
  var List:=TCollections.CreateList_String;
  Assert.IsNotNull(List);
  Assert.AreEqual('',List.FirstOrDefault);
  Assert.AreEqual('',List.LastOrDefault);
  Assert.IsTrue(List.IsEmpty);
  Assert.IsFalse(List.Any);

  var FirstItem:='Hello World!';
  List.Add(FirstItem);
  Assert.AreEqual(FirstItem,List.First);
  Assert.AreEqual(FirstItem,List.Last);
  Assert.AreEqual(1,List.Count);
  Assert.IsFalse(List.IsEmpty);
  Assert.IsTrue(List.Any);

  var SecondItem:='Some random content';
  List.Add(SecondItem);
  Assert.AreEqual(2,List.Count);
  Assert.AreEqual(FirstItem,List.First);
  Assert.AreEqual(SecondItem,List.Last);

  var ThirdItem:='Lorem ipsum et delomet';
  List.Add(ThirdItem);
  Assert.AreEqual(3,List.Count);
  Assert.AreEqual(FirstItem,List.First);
  Assert.AreEqual(ThirdItem,List.Last);

  var EnumCount:=0;
  for var CurItem in List do
  begin
    Assert.IsNotEmpty(CurItem);
    Inc(EnumCount);
  end;
  Assert.AreEqual(List.Count,EnumCount);

  Assert.AreEqual(0,List.IndexOf(FirstItem));
  Assert.AreEqual(1,List.IndexOf(SecondItem));
  Assert.AreEqual(2,List.IndexOf(ThirdItem));
  Assert.AreEqual(-1,List.IndexOf('An unbekannt item'));

  List.Clear;
  Assert.AreEqual(0,List.Count);
  Assert.IsTrue(List.IsEmpty);
  Assert.IsFalse(List.Any);

  for var Loop:=1 to TestLoops do
  begin
    var LoopValue:='MyContent-'+IntToStr(Loop);
    var AddedIndex:=List.Add(LoopValue);
    Assert.AreEqual(AddedIndex,List.IndexOf(LoopValue));
    Assert.AreEqual(LoopValue,List[AddedIndex]);
    Assert.IsTrue(List.Contains(LoopValue));
  end;

  Assert.AreEqual(TestLoops,List.Count);
  var EntriesArray:=List.ToArray;
  Assert.AreEqual(Length(EntriesArray),List.Count);

  for var CurItem in EntriesArray
    do Assert.IsTrue(List.Contains(CurItem));
end;

{ ----------------------------------------------------------------------- }

procedure TestList_String.SortTest;
begin
  var List:=TCollections.CreateList_String;
  List.Add('Cello');
  List.Add('Alpha');
  List.Add('Bello');
  List.Sort;
  Assert.AreEqual('Alpha',List[0]);
  Assert.AreEqual('Bello',List[1]);
  Assert.AreEqual('Cello',List[2]);
end;

{ ----------------------------------------------------------------------- }

procedure TestList_String.UniqueList;
begin
  var UniqueList:=TCollections.CreateListUnique_String;
  UniqueList.Add('Cello');
  Assert.AreEqual(1,UniqueList.Count);
  UniqueList.Add('Bello');
  Assert.AreEqual(2,UniqueList.Count);
  UniqueList.Add('Cello');
  UniqueList.Add('Bello');
  Assert.AreEqual(2,UniqueList.Count);

  var List:=TCollections.CreateList_String;
  List.Add('Cello');
  List.Add('Bello');
  List.Add('Cello');
  List.Add('Bello');
  Assert.AreEqual(4,List.Count);
end;

{ ======================================================================= }
{ TestList_Integer                                                        }
{ ======================================================================= }

procedure TestList_Integer.SimpleTests;
const
  TestLoops = 1000;
begin
  var List: IList_Integer:=TCollections.CreateList_Integer;
  Assert.IsFalse(List.Any);
  Assert.IsTrue(List.IsEmpty);

  Assert.AreEqual(0,List.Add(789));
  Assert.AreEqual(1,List.Add(-456));
  Assert.AreEqual(2,List.Add(123));
  Assert.AreEqual(3,List.Count);

  Assert.IsTrue(List.Contains(789));
  Assert.IsTrue(List.Contains(-456));
  Assert.IsTrue(List.Contains(123));

  Assert.IsFalse(List.Contains(0));
  Assert.IsFalse(List.Contains(1));
  Assert.IsFalse(List.Contains(951));

  Assert.IsTrue(List.Any);
  Assert.IsFalse(List.IsEmpty);

  List.Sort;
  Assert.AreEqual(0,List.IndexOf(-456));
  Assert.AreEqual(1,List.IndexOf(123));
  Assert.AreEqual(2,List.IndexOf(789));
end;

{ ======================================================================= }
{ TestList_TObject                                                        }
{ ======================================================================= }

procedure TestList_TObject.Add;
begin
  var O1: TObject:=TObject.Create;
  var O2: TObject:=TObject.Create;
  var O3: TObject:=TObject.Create;
  var OList: IList_TObject:=TCollections.CreateList_TObject(true);

  OList.Add(O1);
  OList.Add(O2);
  OList.Add(O3);

  Assert.AreEqual(3,OList.Count);
  Assert.AreEqual(O1,OList[0]);
  Assert.AreEqual(O2,OList[1]);
  Assert.AreEqual(O3,OList[2]);

  var IsCounter: Integer:=0;
  for var LoopItem in OList do
  begin
    if LoopItem is TObject
      then Inc(IsCounter);
  end;
  Assert.AreEqual(3,IsCounter);
end;

{ ----------------------------------------------------------------------- }

procedure TestList_TObject.Exchange;

{ -------------------------- }

  procedure CheckInvalidIndex(const AList: IList_TObject; AIndex1, AIndex2: Integer);
  begin
    Assert.WillRaise(
      procedure
      begin
        AList.Exchange(AIndex1,AIndex2);
      end,EListError);
  end;

{ -------------------------- }

begin
  var FreeCallCount: Integer:=0;
  var FreeCallProc: TProc<TObject>:=
    procedure(AObject: TObject)
    begin
      Inc(FreeCallCount);
    end;

  var ObjA: TObject:=CFreeNotifyObject.Create(FreeCallProc);
  var ObjB: TObject:=CFreeNotifyObject.Create(FreeCallProc);
  var ObjC: TObject:=CFreeNotifyObject.Create(FreeCallProc);
  var ObjD: TObject:=CFreeNotifyObject.Create(FreeCallProc);

  var List: IList_TObject:=TCollections.CreateList_TObject(true);
  try
    List.Add(ObjA);
    List.Add(ObjB);
    List.Add(ObjC);
    List.Add(ObjD);

    Assert.AreEqual(0,FreeCallCount);

    List.Exchange(1,2);
    Assert.AreEqual(ObjA,List[0]);
    Assert.AreEqual(ObjC,List[1]);
    Assert.AreEqual(ObjB,List[2]);
    Assert.AreEqual(ObjD,List[3]);
    Assert.AreEqual(0,FreeCallCount);

    List.Exchange(1,2);
    Assert.AreEqual(ObjA,List[0]);
    Assert.AreEqual(ObjB,List[1]);
    Assert.AreEqual(ObjC,List[2]);
    Assert.AreEqual(ObjD,List[3]);
    Assert.AreEqual(0,FreeCallCount);

    List.Exchange(0,3);
    Assert.AreEqual(ObjD,List[0]);
    Assert.AreEqual(ObjB,List[1]);
    Assert.AreEqual(ObjC,List[2]);
    Assert.AreEqual(ObjA,List[3]);
    Assert.AreEqual(0,FreeCallCount);

    List.Exchange(1, 1);
    Assert.AreEqual(ObjD,List[0]);
    Assert.AreEqual(ObjB,List[1]);
    Assert.AreEqual(ObjC,List[2]);
    Assert.AreEqual(ObjA,List[3]);
    Assert.AreEqual(0,FreeCallCount);

    CheckInvalidIndex(List,-1,1);
    CheckInvalidIndex(List,1,-1);
    CheckInvalidIndex(List,0,4);
    CheckInvalidIndex(List,1,4);
    CheckInvalidIndex(List,4,0);
    CheckInvalidIndex(List,4,1);
    Assert.AreEqual(0,FreeCallCount);
  finally
    List:=nil; // Explizite Freigabe
    Assert.AreEqual(4,FreeCallCount);
  end;
end;

{ ----------------------------------------------------------------------- }

procedure TestList_TObject.Move;

{ -------------------------- }

  procedure CheckInvalidIndex(const AList: IList_TObject; ASourceIndex, ATargetIndex: Integer);
  begin
    Assert.WillRaise(
      procedure
      begin
        AList.Move(ASourceIndex,ATargetIndex);
      end,EListError);
  end;

{ -------------------------- }

begin
  var FreeCallCount: Integer:=0;
  var FreeCallProc: TProc<TObject>:=
    procedure(AObject: TObject)
    begin
      Inc(FreeCallCount);
    end;

  var List: IList_TObject:=TCollections.CreateList_TObject(true);
  try
    var ObjA: TObject:=CFreeNotifyObject.Create(FreeCallProc);
    var ObjB: TObject:=CFreeNotifyObject.Create(FreeCallProc);
    var ObjC: TObject:=CFreeNotifyObject.Create(FreeCallProc);
    var ObjD: TObject:=CFreeNotifyObject.Create(FreeCallProc);
    List.Add(ObjA);
    List.Add(ObjB);
    List.Add(ObjC);
    List.Add(ObjD);

    Assert.AreEqual(0,FreeCallCount);

    List.Move(0,2);
    Assert.AreEqual(ObjB,List[0]);
    Assert.AreEqual(ObjC,List[1]);
    Assert.AreEqual(ObjA,List[2]);
    Assert.AreEqual(ObjD,List[3]);
    Assert.AreEqual(0,FreeCallCount);

    List.Move(3,1);
    Assert.AreEqual(ObjB,List[0]);
    Assert.AreEqual(ObjD,List[1]);
    Assert.AreEqual(ObjC,List[2]);
    Assert.AreEqual(ObjA,List[3]);
    Assert.AreEqual(0,FreeCallCount);

    List.Move(1,1);
    Assert.AreEqual(ObjB,List[0]);
    Assert.AreEqual(ObjD,List[1]);
    Assert.AreEqual(ObjC,List[2]);
    Assert.AreEqual(ObjA,List[3]);
    Assert.AreEqual(0,FreeCallCount);

    List.Move(0,3);
    Assert.AreEqual(ObjD,List[0]);
    Assert.AreEqual(ObjC,List[1]);
    Assert.AreEqual(ObjA,List[2]);
    Assert.AreEqual(ObjB,List[3]);
    Assert.AreEqual(0,FreeCallCount);

    List.Move(2, 0);
    Assert.AreEqual(ObjA,List[0]);
    Assert.AreEqual(ObjD,List[1]);
    Assert.AreEqual(ObjC,List[2]);
    Assert.AreEqual(ObjB,List[3]);
    Assert.AreEqual(0,FreeCallCount);

    CheckInvalidIndex(List,-1,1);
    CheckInvalidIndex(List,1,-1);
    CheckInvalidIndex(List,0,4);
    CheckInvalidIndex(List,4,0);
    Assert.AreEqual(0,FreeCallCount);
  finally
    List:=nil; // Explizite Freigabe
    Assert.AreEqual(4,FreeCallCount);
  end;
end;

{ ======================================================================= }
{ TestList_TComponent                                                     }
{ ======================================================================= }

procedure TestList_TComponent.CastToEnumerable;
begin
  var C1: TComponent:=TComponent.Create(nil);
  var C2: TComponent:=TComponent.Create(nil);
  var C3: TComponent:=TComponent.Create(nil);
  var CompList: IList_TComponent:=TCollections.CreateList_TComponent(true);
  CompList.Add(C1);
  CompList.Add(C2);
  CompList.Add(C3);

  Assert.IsTrue(Supports(CompList,IEnumerable_TObject));
  Assert.IsTrue(Supports(CompList,IEnumerable_TComponent));
end;

{ ----------------------------------------------------------------------- }

procedure TestList_TComponent.Add;
begin
  var C1: TComponent:=TComponent.Create(nil);
  var C2: TComponent:=TComponent.Create(nil);
  var C3: TComponent:=TComponent.Create(nil);
  var CompList: IList_TComponent:=TCollections.CreateList_TComponent(true);

  CompList.Add(C1);
  CompList.Add(C2);
  CompList.Add(C3);

  Assert.AreEqual(3,CompList.Count);
  Assert.AreEqual(C1,CompList[0]);
  Assert.AreEqual(C2,CompList[1]);
  Assert.AreEqual(C3,CompList[2]);

  var IsCounter: Integer:=0;
  for var LoopItem in CompList do
  begin
    if LoopItem is TComponent
      then Inc(IsCounter);
  end;
  Assert.AreEqual(3,IsCounter);
end;

{ ----------------------------------------------------------------------- }

procedure TestList_TComponent.ToArray;
var
  CompList: IList_TComponent;

{ -------------------------- }

  procedure CheckToArray(AOffset, ACount: Integer; AExpectedComponents: Array of TComponent);
  var
    Target: TArray<TComponent>;
  begin
    Target:=CompList.ToArray(AOffset,ACount);
    Assert.AreEqual(Length(AExpectedComponents),Length(Target));
    for var Loop: Integer:=0 to Length(AExpectedComponents)-1
      do Assert.AreEqual(AExpectedComponents[Loop],Target[Loop]);
  end;

{ -------------------------- }

begin
  var C1: TComponent:=TComponent.Create(nil);
  var C2: TComponent:=TComponent.Create(nil);
  var C3: TComponent:=TComponent.Create(nil);
  var C4: TComponent:=TComponent.Create(nil);
  var C5: TComponent:=TComponent.Create(nil);
  CompList:=TCollections.CreateList_TComponent(true);
  CompList.AddRange([C1,C2,C3,C4,C5]);
  Assert.AreEqual(5,CompList.Count);

  CheckToArray(0,1,[C1]);
  CheckToArray(0,2,[C1,C2]);
  CheckToArray(0,3,[C1,C2,C3]);
  CheckToArray(0,4,[C1,C2,C3,C4]);
  CheckToArray(0,5,[C1,C2,C3,C4,C5]);
  CheckToArray(0,6,[C1,C2,C3,C4,C5]); // Overflow

  CheckToArray(1,1,[C2]);
  CheckToArray(2,1,[C3]);
  CheckToArray(3,1,[C4]);
  CheckToArray(4,1,[C5]);
  CheckToArray(5,1,[]); // Overflow

  CheckToArray(3,2,[C4,C5]);
  CheckToArray(2,3,[C3,C4,C5]);
  CheckToArray(1,4,[C2,C3,C4,C5]);
  CheckToArray(1,5,[C2,C3,C4,C5]); // Overflow
end;

{ ======================================================================= }
{ TestDictionary_String_String                                            }
{ ======================================================================= }

procedure TestDictionary_String_String.SimpleTests;
const
  TestLoops = 1000;
begin
  var Dict: IDictionary_String_String:=TCollections.CreateDictionary_String_String;

  for var Loop:=1 to TestLoops
    do Dict.Add('MyKey-'+IntToStr(Loop),'MyContent-'+IntToStr(Loop));

  Assert.AreEqual(TestLoops,Dict.Count);

  for var Loop:=1 to TestLoops do
  begin
    var LoopKey:='MyKey-'+IntToStr(Loop);
    var LoopValue:='MyContent-'+IntToStr(Loop);

    Assert.IsTrue(Dict.ContainsKey(LoopKey));
    Assert.IsTrue(Dict.Contains(LoopKey,LoopValue));

    Assert.AreEqual(LoopValue,Dict[LoopKey]);

    var CurValue: String;
    Assert.IsTrue(Dict.TryGetValue(LoopKey,CurValue));
    Assert.AreEqual(CurValue,LoopValue);
  end;

  var LKeys: TArray<String>:=Dict.Keys.ToArray;
  var LValues: TArray<String>:=Dict.Values.ToArray;
  Assert.AreEqual(Length(LKeys),Length(LValues));

  for var Loop:=1 to TestLoops do
  begin
    var LoopKey:='MyKey-'+IntToStr(Loop);
    var LoopValue:='MyContent-'+IntToStr(Loop);
    var CurValue:=Dict.Extract(LoopKey);
    Assert.AreEqual(LoopValue,CurValue);
  end;

  Assert.AreEqual(0,Dict.Count);
  Assert.IsTrue(Dict.IsEmpty);
  Assert.IsFalse(Dict.Any);

  var SampleKey:='MyVeryCustomLongKeyForATest';
  var SampleData:='Das ist etwas Content für den Test der SetItem-Methode';
  Dict[SampleKey]:=SampleData;
  Assert.AreEqual(SampleData,Dict[SampleKey]);
  Assert.AreEqual(1,Dict.Count);

  Dict.Clear;
  Assert.IsFalse(Dict.ContainsKey(SampleKey));
  Assert.AreEqual(0,Dict.Count);
end;

{ ======================================================================= }
{ TestDictionary_String_TObject                                           }
{ ======================================================================= }

procedure TestDictionary_String_TObject.OwnershipTests;
var
  Dict         : IDictionary_String_TObject;
  FreeCallCount: Integer;
  FreeCallProc : TProc<TObject>;
  Obj1         : CFreeNotifyObject;
  Obj2         : CFreeNotifyObject;
  Obj3         : CFreeNotifyObject;

{ -------------------------- }

  procedure InitObjsAndAddToDict;
  begin
    Obj1:=CFreeNotifyObject.Create(FreeCallProc);
    Dict.Add('Object1',Obj1);
    Obj2:=CFreeNotifyObject.Create(FreeCallProc);
    Dict.Add('Object2',Obj2);
    Obj3:=CFreeNotifyObject.Create(FreeCallProc);
    Dict.Add('Object3',Obj3);
    Assert.IsTrue(Dict.Items['Object1']=Obj1);
    Assert.IsTrue(Dict.Items['Object2']=Obj2);
    Assert.IsTrue(Dict.Items['Object3']=Obj3);
  end;

{ -------------------------- }

begin
  Dict:=TCollections.CreateDictionary_String_TObject(true);
  FreeCallCount:=0;
  FreeCallProc:=
    procedure(AObject: TObject)
    begin
      Inc(FreeCallCount);
    end;

  InitObjsAndAddToDict;
  Dict:=nil;
  Assert.AreEqual(3,FreeCallCount);

  Dict:=TCollections.CreateDictionary_String_TObject({AOwnValues=}false);
  try
    InitObjsAndAddToDict;
    FreeCallCount:=0;
    Dict:=nil;
    Assert.AreEqual(0,FreeCallCount); // Es sollte keine Freigabe erfolgt sein
  finally
    Obj1.Free;
    Obj2.Free;
    Obj3.Free;
  end;
  Assert.AreEqual(3,FreeCallCount);

  Dict:=TCollections.CreateDictionary_String_TObject(true);
  InitObjsAndAddToDict;
  FreeCallCount:=0;
  Dict.Remove('Object1');
  Assert.AreEqual(1,FreeCallCount);
  var ExtractedObj:=Dict.Extract('Object2');
  Assert.AreEqual(1,FreeCallCount); // Extract darf das Objekt nicht freigeben, daher sollte sich der Counter nicht verändert haben
  ExtractedObj.Free;
  Assert.AreEqual(2,FreeCallCount);
  Dict.Clear;
  Assert.AreEqual(3,FreeCallCount);

  Dict:=TCollections.CreateDictionary_String_TObject(true);
  InitObjsAndAddToDict;
  FreeCallCount:=0;
  Dict.Clear;
  Assert.AreEqual(3,FreeCallCount);

  Dict:=TCollections.CreateDictionary_String_TObject(true);
  InitObjsAndAddToDict;
  FreeCallCount:=0;
  for var Key in Dict.Keys.ToArray
    do Dict.Remove(Key);
  Assert.AreEqual(3,FreeCallCount);
end;

end.
