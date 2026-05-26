// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.ScopeLocalEnumBridge;

interface

uses

  mormot.core.collections,

  DPT.Rsm.Model;

type

  /// <summary>
  ///   Post-process pass ("strong-form" bridge) that walks every
  ///   registered global, picks the ones whose stored type id carries
  ///   the scope-local enum marker ($1E hi-byte), and -- via unit-
  ///   suffix matching against the parsed <c>EnumDefs</c> -- binds
  ///   each distinct scope-local id to the EnumDef of the unit the
  ///   anchor variable's name implies. Each (unit, type) pair has
  ///   its own scope-local id assigned by the compiler at consumer-
  ///   unit compile time, and ALL variables of that (unit, type) pair
  ///   share the same id, so a single anchor variable per id is
  ///   enough to bind every other variable of that id to the correct
  ///   EnumDef -- even variables whose own name carries no unit hint
  ///   at all.
  /// </summary>
  TRsmScopeLocalEnumBridge = class
  private
    FGlobalByName              : IKeyValue<String, UInt32>;
    FEnumDefs                  : IList<TRsmEnumDef>;
    FScopeLocalTypeIdToEnumDef : IKeyValue<UInt32, Integer>;
    function UnitTrailingShort(const AUnitName: String): String;
  public
    constructor Create(
      AGlobalByName             : IKeyValue<String, UInt32>;
      AEnumDefs                 : IList<TRsmEnumDef>;
      AScopeLocalTypeIdToEnumDef: IKeyValue<UInt32, Integer>);
    procedure Run;
  end;

implementation

uses

  System.StrUtils,
  System.SysUtils;

{ TRsmScopeLocalEnumBridge }

constructor TRsmScopeLocalEnumBridge.Create(
  AGlobalByName             : IKeyValue<String, UInt32>;
  AEnumDefs                 : IList<TRsmEnumDef>;
  AScopeLocalTypeIdToEnumDef: IKeyValue<UInt32, Integer>);
begin
  inherited Create;
  FGlobalByName              := AGlobalByName;
  FEnumDefs                  := AEnumDefs;
  FScopeLocalTypeIdToEnumDef := AScopeLocalTypeIdToEnumDef;
end;

function TRsmScopeLocalEnumBridge.UnitTrailingShort(
  const AUnitName: String): String;
const
  StripPrefixes: array[0..3] of String = ('Enum', 'Module', 'Unit', 'Mod');
var
  DotPos: Integer;
  Tail  : String;
  PI    : Integer;
begin
  DotPos := AUnitName.LastIndexOf('.');
  if DotPos >= 0 then
    Tail := AUnitName.Substring(DotPos + 1)
  else
    Tail := AUnitName;
  for PI := 0 to High(StripPrefixes) do
    if StartsText(StripPrefixes[PI], Tail) and
       (Length(Tail) > Length(StripPrefixes[PI])) then
    begin
      Tail := Copy(Tail, Length(StripPrefixes[PI]) + 1, MaxInt);
      Break;
    end;
  Result := Tail;
end;

/// <summary>
///   Walks every (lowercased) name in <c>GlobalByName</c>; for ids
///   carrying the $1E marker that aren't yet bridged, uses the
///   unit-suffix matcher to find a matching def. The first bridge
///   built for an id wins -- typically the first conventionally-
///   named global declared with that type.
/// </summary>
procedure TRsmScopeLocalEnumBridge.Run;
var
  Pair: TPair<String, UInt32>;
begin
  if FEnumDefs.Count = 0 then Exit;
  for Pair in FGlobalByName do
  begin
    var TypeId: UInt32 := Pair.Value;
    if ((TypeId shr 8) and $FF) <> $1E then Continue;
    if FScopeLocalTypeIdToEnumDef.ContainsKey(TypeId) then Continue;
    var VarLower: String := Pair.Key; // already lowercased on insert
    var BestIdx : Integer := -1;
    var BestLen : Integer := 0;
    for var I: Integer := 0 to FEnumDefs.Count - 1 do
    begin
      var Def: TRsmEnumDef := FEnumDefs[I];
      var ShortName: String := UnitTrailingShort(Def.UnitName);
      if (Length(ShortName) >= 2) and
         VarLower.EndsWith(LowerCase(ShortName)) then
      begin
        if Length(ShortName) > BestLen then
        begin
          BestIdx := I;
          BestLen := Length(ShortName);
        end;
      end;
    end;
    if BestIdx >= 0 then
      FScopeLocalTypeIdToEnumDef[TypeId] := BestIdx;
  end;
end;

end.
