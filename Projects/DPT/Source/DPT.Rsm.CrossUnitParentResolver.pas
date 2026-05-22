// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.CrossUnitParentResolver;

interface

uses
  mormot.core.base,
  mormot.core.collections,

  DPT.Rsm.Model;

type

  /// <summary>
  ///   Post-process pass that bridges cross-unit inheritance (e.g. a
  ///   user class declared in DebugTarget inheriting from
  ///   System.Classes.TComponent) by resolving each class's stored
  ///   <c>ParentRawId</c> against the type-id-to-class map populated
  ///   by <see cref="TRsmFormatALinker"/>. The raw id was captured
  ///   during class discovery from the two bytes immediately
  ///   preceding the class-name length byte; same-unit inheritance
  ///   leaves those bytes zero and is handled by
  ///   <see cref="TRsmClassParentDeriver"/> instead, so this pass
  ///   only fills the gaps the offset heuristic left.
  /// </summary>
  TRsmCrossUnitParentResolver = class
  private
    FClasses             : IList<TRsmClassInfo>;
    FRsmTypeIdToClassIdx : IKeyValue<UInt32, Integer>;
  public
    constructor Create(AClasses: IList<TRsmClassInfo>;
      ARsmTypeIdToClassIdx: IKeyValue<UInt32, Integer>);
    procedure Run;
  end;

implementation

{ TRsmCrossUnitParentResolver }

constructor TRsmCrossUnitParentResolver.Create(AClasses: IList<TRsmClassInfo>;
  ARsmTypeIdToClassIdx: IKeyValue<UInt32, Integer>);
begin
  inherited Create;
  FClasses             := AClasses;
  FRsmTypeIdToClassIdx := ARsmTypeIdToClassIdx;
end;

/// <summary>
///   Walk every class; for those with non-zero ParentRawId and
///   empty ParentName, look up the raw id in the type-id map and
///   set ParentName from the matching class's Name. Classes whose
///   ParentName was already filled in by the offset heuristic are
///   left alone -- the offset pass is reliable for in-file chains
///   and this pass only fills gaps.
/// </summary>
procedure TRsmCrossUnitParentResolver.Run;
var
  I, ClsIdx: Integer;
  Info     : TRsmClassInfo;
begin
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
    if Info.Kind <> skClass then Continue;
    if Info.ParentName <> '' then Continue;
    if Info.ParentRawId = 0 then Continue;
    if not FRsmTypeIdToClassIdx.TryGetValue(Info.ParentRawId, ClsIdx) then
      Continue;
    if (ClsIdx < 0) or (ClsIdx >= FClasses.Count) or (ClsIdx = I) then
      Continue;
    Info.ParentName := FClasses[ClsIdx].Name;
    FClasses[I] := Info;
  end;
end;

end.
