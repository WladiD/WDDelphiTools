// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit System.Collections.Interfaces;

interface

uses

  System.Classes,

  mormot.core.data,
  mormot.core.json,

  Collections.Interfaces;

type

{$REGION 'INCLUDE-PARTIAL / System.List-interface.part.pas'}

  IEnumerator_Integer = interface
    ['{9EA4E62C-46A6-4566-91DA-6F8137609C85}']
    function GetCurrent: Integer;
    function MoveNext: Boolean;
    property Current: Integer read GetCurrent;
  end;

  IEnumerable_Integer = interface
    ['{E34A643D-E205-4BE1-A58F-0B1D6DCD70E5}']
    function GetEnumerator: IEnumerator_Integer;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Integer>;
  end;

  TPredicate_Integer = reference to function(const AValue: Integer): Boolean;

  IList_Integer = interface(IList)
    ['{E92E8434-8E81-421D-8EA2-8D780AC98B59}']
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
    function  ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Integer>;
    property  Items[AIndex: Integer]: Integer read GetItem write SetItem; default;
  end;

  IEnumerator_String = interface
    ['{F8417A49-C88E-4CA0-BA9D-CC6BB5C1C931}']
    function GetCurrent: String;
    function MoveNext: Boolean;
    property Current: String read GetCurrent;
  end;

  IEnumerable_String = interface
    ['{AD3E4097-E05F-4FF8-96F5-4D08E4697C84}']
    function GetEnumerator: IEnumerator_String;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
  end;

  TPredicate_String = reference to function(const AValue: String): Boolean;

  IList_String = interface(IList)
    ['{8B4D010D-EAF5-40FA-9220-1028F73658A3}']
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
    function  ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<String>;
    property  Items[AIndex: Integer]: String read GetItem write SetItem; default;
  end;

  IEnumerator_TGUID = interface
    ['{A1366364-7308-4168-9E31-694F1A927F3B}']
    function GetCurrent: TGUID;
    function MoveNext: Boolean;
    property Current: TGUID read GetCurrent;
  end;

  IEnumerable_TGUID = interface
    ['{6CD5359E-15B8-4C3F-A093-121B565AC7D9}']
    function GetEnumerator: IEnumerator_TGUID;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TGUID>;
  end;

  TPredicate_TGUID = reference to function(const AValue: TGUID): Boolean;

  IList_TGUID = interface(IList)
    ['{37F5B961-72E2-4081-A25D-99823D8D927C}']
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
    function  ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TGUID>;
    property  Items[AIndex: Integer]: TGUID read GetItem write SetItem; default;
  end;

  IEnumerator_TObject = interface
    ['{19FFFEE6-2098-49FE-A1C6-214F4ABCB827}']
    function GetCurrent: TObject;
    function MoveNext: Boolean;
    property Current: TObject read GetCurrent;
  end;

  IEnumerable_TObject = interface
    ['{12C6C1C8-7999-4C7C-9102-6E9FDF3D6E44}']
    function GetEnumerator: IEnumerator_TObject;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TObject>;
  end;

  TPredicate_TObject = reference to function(const AValue: TObject): Boolean;

  IList_TObject = interface(IList)
    ['{09193831-72AB-4C07-9A00-3AB7C6B2FE7C}']
    function  Add(const AItem: TObject): Integer;
    procedure AddRange(const AValues: array of TObject);
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
    property  Items[AIndex: Integer]: TObject read GetItem write SetItem; default;
  end;

  IEnumerator_TComponent = interface
    ['{7C23F0EA-1F5A-4A57-8E91-1A1E977B3DC1}']
    function GetCurrent: TComponent;
    function MoveNext: Boolean;
    property Current: TComponent read GetCurrent;
  end;

  IEnumerable_TComponent = interface
    ['{C0FCADA3-2D77-4773-9FDF-2F6E8F920619}']
    function GetEnumerator: IEnumerator_TComponent;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TComponent>;
  end;

  TPredicate_TComponent = reference to function(const AValue: TComponent): Boolean;

  IList_TComponent = interface(IList)
    ['{8F88FB61-CBA4-4199-B456-945E2833858C}']
    function  Add(const AItem: TComponent): Integer;
    procedure AddRange(const AValues: array of TComponent);
    function  All(const APredicate: TPredicate_TComponent): Boolean;
    function  Concat(const ASecond: IEnumerable_TComponent): IList_TComponent;
    function  Contains(const AValue: TComponent): Boolean;
    function  Extract(const AItem: TComponent): TComponent;
    function  First: TComponent;
    function  FirstOrDefault: TComponent;
    function  GetEnumerator: IEnumerator_TComponent;
    function  GetItem(AIndex: Integer): TComponent;
    function  GetRange(AIndex, ACount: Integer): IList_TComponent;
    function  IndexOf(const AItem: TComponent): Integer;
    procedure Insert(AIndex: Integer; const AItem: TComponent);
    function  Last: TComponent;
    function  LastOrDefault: TComponent;
    function  Remove(const AItem: TComponent): Boolean;
    function  RemoveAll(const APredicate: TPredicate_TComponent): Integer;
    procedure SetItem(AIndex: Integer; const AValue: TComponent);
    function  ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TComponent>;
    function  Where(const APredicate: TPredicate_TComponent): IEnumerable_TComponent;
    property  Items[AIndex: Integer]: TComponent read GetItem write SetItem; default;
  end;

  IEnumerator_Double = interface
    ['{A7CD8DD0-C093-4E4E-B8BA-57798E15E011}']
    function GetCurrent: Double;
    function MoveNext: Boolean;
    property Current: Double read GetCurrent;
  end;

  IEnumerable_Double = interface
    ['{1924448F-DEE6-4E2C-9EC7-2DEBB973B798}']
    function GetEnumerator: IEnumerator_Double;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Double>;
  end;

  TPredicate_Double = reference to function(const AValue: Double): Boolean;

  IList_Double = interface(IList)
    ['{798279A5-24C7-438B-894F-FB6DD990F6AE}']
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
    function  ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Double>;
    property  Items[AIndex: Integer]: Double read GetItem write SetItem; default;
  end;

{$ENDREGION 'INCLUDE-PARTIAL / System.List-interface.part.pas'}

{$REGION 'INCLUDE-PARTIAL / System.Dictionary-interface.part.pas'}

  TPair_Integer_IInterface = record
    Key: Integer;
    Value: IInterface;
  end;
  
  IPairEnumerator_Integer_IInterface = interface
    ['{1619AE79-66B9-4977-8F2A-7E8F659D7EA3}']
    function GetCurrent: TPair_Integer_IInterface;
    function MoveNext: Boolean;
    property Current: TPair_Integer_IInterface read GetCurrent;
  end;

  IEnumerator_IInterface = interface
    ['{945CB357-20D5-4DD8-9F3E-AAE55F4415C8}']
    function GetCurrent: IInterface;
    function MoveNext: Boolean;
    property Current: IInterface read GetCurrent;
  end;

  IEnumerable_IInterface = interface
    ['{6889034A-BE7F-4026-8DB9-080E3B3CCA72}']
    function GetEnumerator: IEnumerator_IInterface;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<IInterface>;
  end;
  
  IDictionary_Integer_IInterface = interface(IDictionary)
    ['{A416001F-3761-4B27-8B4D-2CD3CFA854FB}']
    procedure Add(const AKey: Integer; const AValue: IInterface);
    function  Contains(const AKey: Integer; const AValue: IInterface): Boolean;
    function  ContainsKey(const AKey: Integer): Boolean;
    function  Extract(const AKey: Integer): IInterface;
    function  Remove(const AKey: Integer): Boolean;
    function  GetEnumerator: IPairEnumerator_Integer_IInterface;
    function  TryGetValue(const AKey: Integer; out AValue: IInterface): Boolean;
    function  GetItem(const AKey: Integer): IInterface;    
    function  GetKeyEnumerable: IEnumerable_Integer;
    function  GetValueEnumerable: IEnumerable_IInterface;
    procedure SetItem(const AKey: Integer; const AValue: IInterface);
    property  Keys: IEnumerable_Integer read GetKeyEnumerable;
    property  Values: IEnumerable_IInterface read GetValueEnumerable;
    property  Items[const AKey: Integer]: IInterface read GetItem write SetItem; default;
  end;

  TPair_Integer_String = record
    Key: Integer;
    Value: String;
  end;
  
  IPairEnumerator_Integer_String = interface
    ['{08F551D4-4701-41B5-9149-06041477B0F1}']
    function GetCurrent: TPair_Integer_String;
    function MoveNext: Boolean;
    property Current: TPair_Integer_String read GetCurrent;
  end;

  IDictionary_Integer_String = interface(IDictionary)
    ['{FB086374-A5FD-46D3-AF15-0B294DFC788A}']
    procedure Add(const AKey: Integer; const AValue: String);
    function  Contains(const AKey: Integer; const AValue: String): Boolean;
    function  ContainsKey(const AKey: Integer): Boolean;
    function  Extract(const AKey: Integer): String;
    function  Remove(const AKey: Integer): Boolean;
    function  GetEnumerator: IPairEnumerator_Integer_String;
    function  TryGetValue(const AKey: Integer; out AValue: String): Boolean;
    function  GetItem(const AKey: Integer): String;    
    function  GetKeyEnumerable: IEnumerable_Integer;
    function  GetValueEnumerable: IEnumerable_String;
    procedure SetItem(const AKey: Integer; const AValue: String);
    property  Keys: IEnumerable_Integer read GetKeyEnumerable;
    property  Values: IEnumerable_String read GetValueEnumerable;
    property  Items[const AKey: Integer]: String read GetItem write SetItem; default;
  end;

  TPair_Integer_TGUID = record
    Key: Integer;
    Value: TGUID;
  end;
  
  IPairEnumerator_Integer_TGUID = interface
    ['{F6DEAED4-B31E-48A5-82A2-BC143A9562EC}']
    function GetCurrent: TPair_Integer_TGUID;
    function MoveNext: Boolean;
    property Current: TPair_Integer_TGUID read GetCurrent;
  end;

  IDictionary_Integer_TGUID = interface(IDictionary)
    ['{3088A1AC-A279-4BC5-86CF-A50B35CB687A}']
    procedure Add(const AKey: Integer; const AValue: TGUID);
    function  Contains(const AKey: Integer; const AValue: TGUID): Boolean;
    function  ContainsKey(const AKey: Integer): Boolean;
    function  Extract(const AKey: Integer): TGUID;
    function  Remove(const AKey: Integer): Boolean;
    function  GetEnumerator: IPairEnumerator_Integer_TGUID;
    function  TryGetValue(const AKey: Integer; out AValue: TGUID): Boolean;
    function  GetItem(const AKey: Integer): TGUID;    
    function  GetKeyEnumerable: IEnumerable_Integer;
    function  GetValueEnumerable: IEnumerable_TGUID;
    procedure SetItem(const AKey: Integer; const AValue: TGUID);
    property  Keys: IEnumerable_Integer read GetKeyEnumerable;
    property  Values: IEnumerable_TGUID read GetValueEnumerable;
    property  Items[const AKey: Integer]: TGUID read GetItem write SetItem; default;
  end;

  TPair_Integer_TObject = record
    Key: Integer;
    Value: TObject;
  end;
  
  IPairEnumerator_Integer_TObject = interface
    ['{7735204B-12D5-494A-BBDD-0615F18130A0}']
    function GetCurrent: TPair_Integer_TObject;
    function MoveNext: Boolean;
    property Current: TPair_Integer_TObject read GetCurrent;
  end;

  IDictionary_Integer_TObject = interface(IDictionary)
    ['{5B70136E-A767-4D4B-866A-EDA9111A04A5}']
    procedure Add(const AKey: Integer; const AValue: TObject);
    function  Contains(const AKey: Integer; const AValue: TObject): Boolean;
    function  ContainsKey(const AKey: Integer): Boolean;
    function  Extract(const AKey: Integer): TObject;
    function  Remove(const AKey: Integer): Boolean;
    function  GetEnumerator: IPairEnumerator_Integer_TObject;
    function  TryGetValue(const AKey: Integer; out AValue: TObject): Boolean;
    function  GetItem(const AKey: Integer): TObject;    
    function  GetKeyEnumerable: IEnumerable_Integer;
    function  GetValueEnumerable: IEnumerable_TObject;
    procedure SetItem(const AKey: Integer; const AValue: TObject);
    property  Keys: IEnumerable_Integer read GetKeyEnumerable;
    property  Values: IEnumerable_TObject read GetValueEnumerable;
    property  Items[const AKey: Integer]: TObject read GetItem write SetItem; default;
  end;

  TPair_String_Integer = record
    Key: String;
    Value: Integer;
  end;
  
  IPairEnumerator_String_Integer = interface
    ['{A935E1C4-B10C-45CF-A4DA-1DAD5F2AB5D8}']
    function GetCurrent: TPair_String_Integer;
    function MoveNext: Boolean;
    property Current: TPair_String_Integer read GetCurrent;
  end;

  IDictionary_String_Integer = interface(IDictionary)
    ['{128F814C-A5BB-42BD-95B5-C34F4B895986}']
    procedure Add(const AKey: String; const AValue: Integer);
    function  Contains(const AKey: String; const AValue: Integer): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): Integer;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_Integer;
    function  TryGetValue(const AKey: String; out AValue: Integer): Boolean;
    function  GetItem(const AKey: String): Integer;    
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_Integer;
    procedure SetItem(const AKey: String; const AValue: Integer);
    property  Keys: IEnumerable_String read GetKeyEnumerable;
    property  Values: IEnumerable_Integer read GetValueEnumerable;
    property  Items[const AKey: String]: Integer read GetItem write SetItem; default;
  end;

  TPair_String_String = record
    Key: String;
    Value: String;
  end;
  
  IPairEnumerator_String_String = interface
    ['{C689097F-357D-414D-90DD-F6797DB6A3C0}']
    function GetCurrent: TPair_String_String;
    function MoveNext: Boolean;
    property Current: TPair_String_String read GetCurrent;
  end;

  IDictionary_String_String = interface(IDictionary)
    ['{D8454CF5-C71E-4A8C-9257-FCB5E98E35E4}']
    procedure Add(const AKey: String; const AValue: String);
    function  Contains(const AKey: String; const AValue: String): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): String;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_String;
    function  TryGetValue(const AKey: String; out AValue: String): Boolean;
    function  GetItem(const AKey: String): String;    
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_String;
    procedure SetItem(const AKey: String; const AValue: String);
    property  Keys: IEnumerable_String read GetKeyEnumerable;
    property  Values: IEnumerable_String read GetValueEnumerable;
    property  Items[const AKey: String]: String read GetItem write SetItem; default;
  end;

  TPair_String_TGUID = record
    Key: String;
    Value: TGUID;
  end;
  
  IPairEnumerator_String_TGUID = interface
    ['{C4677C39-0DF6-475B-8C88-ED7174EB47C4}']
    function GetCurrent: TPair_String_TGUID;
    function MoveNext: Boolean;
    property Current: TPair_String_TGUID read GetCurrent;
  end;

  IDictionary_String_TGUID = interface(IDictionary)
    ['{533E1CD6-1BFF-4758-9D16-F8904B4658EE}']
    procedure Add(const AKey: String; const AValue: TGUID);
    function  Contains(const AKey: String; const AValue: TGUID): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): TGUID;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_TGUID;
    function  TryGetValue(const AKey: String; out AValue: TGUID): Boolean;
    function  GetItem(const AKey: String): TGUID;    
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_TGUID;
    procedure SetItem(const AKey: String; const AValue: TGUID);
    property  Keys: IEnumerable_String read GetKeyEnumerable;
    property  Values: IEnumerable_TGUID read GetValueEnumerable;
    property  Items[const AKey: String]: TGUID read GetItem write SetItem; default;
  end;

  TPair_String_TNotifyEvent = record
    Key: String;
    Value: TNotifyEvent;
  end;
  
  IPairEnumerator_String_TNotifyEvent = interface
    ['{BB0013B9-1092-4B3D-A889-8E3FE71F73A8}']
    function GetCurrent: TPair_String_TNotifyEvent;
    function MoveNext: Boolean;
    property Current: TPair_String_TNotifyEvent read GetCurrent;
  end;

  IEnumerator_TNotifyEvent = interface
    ['{520AB6CC-8880-4255-9190-55664570DCD8}']
    function GetCurrent: TNotifyEvent;
    function MoveNext: Boolean;
    property Current: TNotifyEvent read GetCurrent;
  end;

  IEnumerable_TNotifyEvent = interface
    ['{13430010-42AE-4A51-BDD4-7CFBA81EF3E1}']
    function GetEnumerator: IEnumerator_TNotifyEvent;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TNotifyEvent>;
  end;
  
  IDictionary_String_TNotifyEvent = interface(IDictionary)
    ['{C3D45CF2-B722-4909-8756-50C34262DD01}']
    procedure Add(const AKey: String; const AValue: TNotifyEvent);
    function  Contains(const AKey: String; const AValue: TNotifyEvent): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): TNotifyEvent;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_TNotifyEvent;
    function  TryGetValue(const AKey: String; out AValue: TNotifyEvent): Boolean;
    function  GetItem(const AKey: String): TNotifyEvent;    
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_TNotifyEvent;
    procedure SetItem(const AKey: String; const AValue: TNotifyEvent);
    property  Keys: IEnumerable_String read GetKeyEnumerable;
    property  Values: IEnumerable_TNotifyEvent read GetValueEnumerable;
    property  Items[const AKey: String]: TNotifyEvent read GetItem write SetItem; default;
  end;

  TPair_String_TObject = record
    Key: String;
    Value: TObject;
  end;
  
  IPairEnumerator_String_TObject = interface
    ['{122E5B0B-F7A0-4F28-9D75-34EBEE4CFFEE}']
    function GetCurrent: TPair_String_TObject;
    function MoveNext: Boolean;
    property Current: TPair_String_TObject read GetCurrent;
  end;

  IDictionary_String_TObject = interface(IDictionary)
    ['{9A4E0A6A-50BB-4D26-A0F9-1F2389CD06A0}']
    procedure Add(const AKey: String; const AValue: TObject);
    function  Contains(const AKey: String; const AValue: TObject): Boolean;
    function  ContainsKey(const AKey: String): Boolean;
    function  Extract(const AKey: String): TObject;
    function  Remove(const AKey: String): Boolean;
    function  GetEnumerator: IPairEnumerator_String_TObject;
    function  TryGetValue(const AKey: String; out AValue: TObject): Boolean;
    function  GetItem(const AKey: String): TObject;    
    function  GetKeyEnumerable: IEnumerable_String;
    function  GetValueEnumerable: IEnumerable_TObject;
    procedure SetItem(const AKey: String; const AValue: TObject);
    property  Keys: IEnumerable_String read GetKeyEnumerable;
    property  Values: IEnumerable_TObject read GetValueEnumerable;
    property  Items[const AKey: String]: TObject read GetItem write SetItem; default;
  end;

  TPair_TObject_TObject = record
    Key: TObject;
    Value: TObject;
  end;
  
  IPairEnumerator_TObject_TObject = interface
    ['{7FA93A71-4597-456B-8626-EBE727544431}']
    function GetCurrent: TPair_TObject_TObject;
    function MoveNext: Boolean;
    property Current: TPair_TObject_TObject read GetCurrent;
  end;

  IDictionary_TObject_TObject = interface(IDictionary)
    ['{1FCAC5C5-4DCE-4AD3-873F-C6A2DDCA25EB}']
    procedure Add(const AKey: TObject; const AValue: TObject);
    function  Contains(const AKey: TObject; const AValue: TObject): Boolean;
    function  ContainsKey(const AKey: TObject): Boolean;
    function  Extract(const AKey: TObject): TObject;
    function  Remove(const AKey: TObject): Boolean;
    function  GetEnumerator: IPairEnumerator_TObject_TObject;
    function  TryGetValue(const AKey: TObject; out AValue: TObject): Boolean;
    function  GetItem(const AKey: TObject): TObject;    
    function  GetKeyEnumerable: IEnumerable_TObject;
    function  GetValueEnumerable: IEnumerable_TObject;
    procedure SetItem(const AKey: TObject; const AValue: TObject);
    property  Keys: IEnumerable_TObject read GetKeyEnumerable;
    property  Values: IEnumerable_TObject read GetValueEnumerable;
    property  Items[const AKey: TObject]: TObject read GetItem write SetItem; default;
  end;

  TPair_TClass_TObject = record
    Key: TClass;
    Value: TObject;
  end;
  
  IPairEnumerator_TClass_TObject = interface
    ['{90A3A052-404F-45DA-A887-50BCD963408B}']
    function GetCurrent: TPair_TClass_TObject;
    function MoveNext: Boolean;
    property Current: TPair_TClass_TObject read GetCurrent;
  end;

  IEnumerator_TClass = interface
    ['{944165CC-D487-4BF4-9398-EF1B5FFCA20A}']
    function GetCurrent: TClass;
    function MoveNext: Boolean;
    property Current: TClass read GetCurrent;
  end;

  IEnumerable_TClass = interface
    ['{D4C74E72-027E-4EEE-B301-23BE3E1D14D3}']
    function GetEnumerator: IEnumerator_TClass;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<TClass>;
  end;

  IDictionary_TClass_TObject = interface(IDictionary)
    ['{749638E8-B2B0-4304-903C-F78D956B314A}']
    procedure Add(const AKey: TClass; const AValue: TObject);
    function  Contains(const AKey: TClass; const AValue: TObject): Boolean;
    function  ContainsKey(const AKey: TClass): Boolean;
    function  Extract(const AKey: TClass): TObject;
    function  Remove(const AKey: TClass): Boolean;
    function  GetEnumerator: IPairEnumerator_TClass_TObject;
    function  TryGetValue(const AKey: TClass; out AValue: TObject): Boolean;
    function  GetItem(const AKey: TClass): TObject;    
    function  GetKeyEnumerable: IEnumerable_TClass;
    function  GetValueEnumerable: IEnumerable_TObject;
    procedure SetItem(const AKey: TClass; const AValue: TObject);
    property  Keys: IEnumerable_TClass read GetKeyEnumerable;
    property  Values: IEnumerable_TObject read GetValueEnumerable;
    property  Items[const AKey: TClass]: TObject read GetItem write SetItem; default;
  end;

  TPair_TGUID_Boolean = record
    Key: TGUID;
    Value: Boolean;
  end;
  
  IPairEnumerator_TGUID_Boolean = interface
    ['{F088B0BE-16E3-4BFF-A5B1-B588BD714AA3}']
    function GetCurrent: TPair_TGUID_Boolean;
    function MoveNext: Boolean;
    property Current: TPair_TGUID_Boolean read GetCurrent;
  end;

  IEnumerator_Boolean = interface
    ['{17E5F975-0F7E-4FDB-B026-61E3F365EE67}']
    function GetCurrent: Boolean;
    function MoveNext: Boolean;
    property Current: Boolean read GetCurrent;
  end;

  IEnumerable_Boolean = interface
    ['{7459D103-E7AE-4850-B5AA-672EF876B997}']
    function GetEnumerator: IEnumerator_Boolean;
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<Boolean>;
  end;
  
  IDictionary_TGUID_Boolean = interface(IDictionary)
    ['{DBC9054A-3035-488F-8184-E577AE4DF526}']
    procedure Add(const AKey: TGUID; const AValue: Boolean);
    function  Contains(const AKey: TGUID; const AValue: Boolean): Boolean;
    function  ContainsKey(const AKey: TGUID): Boolean;
    function  Extract(const AKey: TGUID): Boolean;
    function  Remove(const AKey: TGUID): Boolean;
    function  GetEnumerator: IPairEnumerator_TGUID_Boolean;
    function  TryGetValue(const AKey: TGUID; out AValue: Boolean): Boolean;
    function  GetItem(const AKey: TGUID): Boolean;    
    function  GetKeyEnumerable: IEnumerable_TGUID;
    function  GetValueEnumerable: IEnumerable_Boolean;
    procedure SetItem(const AKey: TGUID; const AValue: Boolean);
    property  Keys: IEnumerable_TGUID read GetKeyEnumerable;
    property  Values: IEnumerable_Boolean read GetValueEnumerable;
    property  Items[const AKey: TGUID]: Boolean read GetItem write SetItem; default;
  end;

  TPair_TGUID_Integer = record
    Key: TGUID;
    Value: Integer;
  end;
  
  IPairEnumerator_TGUID_Integer = interface
    ['{EFCCE8E6-FC58-4C29-9442-26F2120EAE3E}']
    function GetCurrent: TPair_TGUID_Integer;
    function MoveNext: Boolean;
    property Current: TPair_TGUID_Integer read GetCurrent;
  end;

  IDictionary_TGUID_Integer = interface(IDictionary)
    ['{2374FBFB-01C5-42D9-B902-803CB58A4F4B}']
    procedure Add(const AKey: TGUID; const AValue: Integer);
    function  Contains(const AKey: TGUID; const AValue: Integer): Boolean;
    function  ContainsKey(const AKey: TGUID): Boolean;
    function  Extract(const AKey: TGUID): Integer;
    function  Remove(const AKey: TGUID): Boolean;
    function  GetEnumerator: IPairEnumerator_TGUID_Integer;
    function  TryGetValue(const AKey: TGUID; out AValue: Integer): Boolean;
    function  GetItem(const AKey: TGUID): Integer;    
    function  GetKeyEnumerable: IEnumerable_TGUID;
    function  GetValueEnumerable: IEnumerable_Integer;
    procedure SetItem(const AKey: TGUID; const AValue: Integer);
    property  Keys: IEnumerable_TGUID read GetKeyEnumerable;
    property  Values: IEnumerable_Integer read GetValueEnumerable;
    property  Items[const AKey: TGUID]: Integer read GetItem write SetItem; default;
  end;

  TPair_TGUID_String = record
    Key: TGUID;
    Value: String;
  end;
  
  IPairEnumerator_TGUID_String = interface
    ['{E6666FE5-6D48-4975-B64C-F430D882F3EB}']
    function GetCurrent: TPair_TGUID_String;
    function MoveNext: Boolean;
    property Current: TPair_TGUID_String read GetCurrent;
  end;

  IDictionary_TGUID_String = interface(IDictionary)
    ['{1333FC34-6548-46E6-B946-2FABBC829EF7}']
    procedure Add(const AKey: TGUID; const AValue: String);
    function  Contains(const AKey: TGUID; const AValue: String): Boolean;
    function  ContainsKey(const AKey: TGUID): Boolean;
    function  Extract(const AKey: TGUID): String;
    function  Remove(const AKey: TGUID): Boolean;
    function  GetEnumerator: IPairEnumerator_TGUID_String;
    function  TryGetValue(const AKey: TGUID; out AValue: String): Boolean;
    function  GetItem(const AKey: TGUID): String;    
    function  GetKeyEnumerable: IEnumerable_TGUID;
    function  GetValueEnumerable: IEnumerable_String;
    procedure SetItem(const AKey: TGUID; const AValue: String);
    property  Keys: IEnumerable_TGUID read GetKeyEnumerable;
    property  Values: IEnumerable_String read GetValueEnumerable;
    property  Items[const AKey: TGUID]: String read GetItem write SetItem; default;
  end;

  TPair_TGUID_TGUID = record
    Key: TGUID;
    Value: TGUID;
  end;
  
  IPairEnumerator_TGUID_TGUID = interface
    ['{17088497-CB0B-45DD-8D0C-5DC72321996B}']
    function GetCurrent: TPair_TGUID_TGUID;
    function MoveNext: Boolean;
    property Current: TPair_TGUID_TGUID read GetCurrent;
  end;

  IDictionary_TGUID_TGUID = interface(IDictionary)
    ['{7A1DE7F4-043B-4CE9-AC70-B20DD42C604E}']
    procedure Add(const AKey: TGUID; const AValue: TGUID);
    function  Contains(const AKey: TGUID; const AValue: TGUID): Boolean;
    function  ContainsKey(const AKey: TGUID): Boolean;
    function  Extract(const AKey: TGUID): TGUID;
    function  Remove(const AKey: TGUID): Boolean;
    function  GetEnumerator: IPairEnumerator_TGUID_TGUID;
    function  TryGetValue(const AKey: TGUID; out AValue: TGUID): Boolean;
    function  GetItem(const AKey: TGUID): TGUID;    
    function  GetKeyEnumerable: IEnumerable_TGUID;
    function  GetValueEnumerable: IEnumerable_TGUID;
    procedure SetItem(const AKey: TGUID; const AValue: TGUID);
    property  Keys: IEnumerable_TGUID read GetKeyEnumerable;
    property  Values: IEnumerable_TGUID read GetValueEnumerable;
    property  Items[const AKey: TGUID]: TGUID read GetItem write SetItem; default;
  end;

{$ENDREGION 'INCLUDE-PARTIAL / System.Dictionary-interface.part.pas'}

implementation

end.
