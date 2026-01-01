unit Collections.Interfaces;

interface

uses

  System.Classes,

  mormot.core.data,
  mormot.core.json;

type

  IList = interface
    ['{3777E0D6-9C4A-4A90-92C6-E519AA58C81A}']
    function  Any: Boolean;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    procedure Exchange(AIndex1, AIndex2: Integer);
    function  GetCapacity: Integer;
    function  GetCount: Integer;
    function  IsEmpty: Boolean;
    procedure Move(ASourceIndex, ATargetIndex: Integer);
    procedure Reverse;
    procedure SetCapacity(AValue: Integer);
    procedure Sort(ACompare: TOnDynArraySortCompare = nil);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
  end;

  IDictionary = interface
    ['{3777E0D6-9C4A-4A90-92C6-E519AA58C81A}']
    function  Any: Boolean;
    procedure Clear;
    function  GetCount: Integer;
    function  GetSynDict: TSynDictionary;
    function  IsEmpty: Boolean;
    property Count: Integer read GetCount;
    /// <summary>Zugriff auf das Low-Level-Dictionary für direkte, dafür aber nicht typsichere, Operationen.</summary>
    property SynDict: TSynDictionary read GetSynDict;
  end;

implementation

end.
