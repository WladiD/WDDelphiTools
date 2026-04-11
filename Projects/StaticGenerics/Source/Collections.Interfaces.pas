// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Collections.Interfaces;

interface

uses

  System.Classes,

  mormot.core.base,
  mormot.core.data,
  mormot.core.json;

type

  IList = interface
    ['{3777E0D6-9C4A-4A90-92C6-E519AA58C81A}']
    function  Any: Boolean;
    procedure Clear;
    procedure Delete(AIndex: PtrInt);
    procedure DeleteRange(AIndex, ACount: PtrInt);
    procedure Exchange(AIndex1, AIndex2: PtrInt);
    function  GetCapacity: PtrInt;
    function  GetCount: PtrInt;
    function  IsEmpty: Boolean;
    procedure Move(ASourceIndex, ATargetIndex: PtrInt);
    procedure Reverse;
    procedure SetCapacity(AValue: PtrInt);
    procedure Sort(ACompare: TOnDynArraySortCompare = nil);
    property  Capacity: PtrInt read GetCapacity write SetCapacity;
    property  Count: PtrInt read GetCount;
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
