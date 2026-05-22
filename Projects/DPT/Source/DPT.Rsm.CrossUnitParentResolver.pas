// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.CrossUnitParentResolver;

// Cross-unit class-parent resolution: fills <c>ParentName</c> for every
// class that has a non-zero <c>ParentRawId</c> and is still missing a
// parent after the offset-layout heuristic (TRsmClassParentDeriver) has
// run. The raw id was captured during class discovery from the two
// bytes immediately preceding the class-name length byte and is only
// meaningful AFTER the type registry has been populated by
// TRsmFormatALinker.

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
  ///   by <see cref="TRsmFormatALinker"/>.
  /// </summary>
  TRsmCrossUnitParentResolver = class
  private
    FClasses             : IList<TRsmClassInfo>;
    FRsmTypeIdToClassIdx : IKeyValue<UInt32, Integer>;
  public
    constructor Create(AClasses: IList<TRsmClassInfo>;
      ARsmTypeIdToClassIdx: IKeyValue<UInt32, Integer>);

    /// <summary>
    ///   Walk every class; for those with non-zero ParentRawId and
    ///   empty ParentName, look up the raw id in the type-id map and
    ///   set ParentName from the matching class's Name. Classes whose
    ///   ParentName was already filled in by the offset heuristic are
    ///   left alone -- the offset pass is reliable for in-file chains
    ///   and this pass only fills gaps.
    /// </summary>
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

procedure TRsmCrossUnitParentResolver.Run;
// Cross-unit inheritance the offset-matching heuristic cannot
// bridge: e.g. a user class declared in DebugTarget inheriting
// from System.Classes.TComponent. The RSM emits a 16-bit type-id
// in the two bytes immediately preceding the class-name length
// byte for such cross-unit parents; same-unit inheritance leaves
// those bytes zero and is handled by TRsmClassParentDeriver instead.
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
