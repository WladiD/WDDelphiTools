// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.ClassParentDeriver;

interface

uses
  mormot.core.collections,

  DPT.Rsm.Model;

type

  /// <summary>
  ///   Post-process pass that fills <c>ParentName</c> on every class
  ///   whose parent can be derived from instance LAYOUT. The RSM
  ///   symbol stream does not expose an explicit class -> parent
  ///   reference in any form we have identified; the compiler instead
  ///   bakes inheritance into the layout (a class C inheriting from P
  ///   starts its own fields at offset (P's instance size), with the
  ///   VMT pointer taking the very first slot), and the heuristic
  ///   matches own-field-end-of-P against own-field-start-of-C.
  ///   Conservative: on ambiguity leaves ParentName empty so a later
  ///   type-id-based pass can fill it without contradicting this one.
  /// </summary>
  TRsmClassParentDeriver = class
  private
    FClasses: IList<TRsmClassInfo>;
  public
    constructor Create(AClasses: IList<TRsmClassInfo>);

    procedure Run;
  end;

implementation

{ TRsmClassParentDeriver }

constructor TRsmClassParentDeriver.Create(AClasses: IList<TRsmClassInfo>);
begin
  inherited Create;
  FClasses := AClasses;
end;

/// <summary>
///   Reconstruct the class hierarchy by matching each class's first
///   own-field offset against any preceding class's instance size:
///   for each class C, find the class P whose own-fields end exactly
///   at C's first own field offset, and call that P the parent of C.
///   The heuristic does the right thing for layouts where each
///   candidate parent has a distinct instance size (the common case
///   for hand-written Delphi code, where sibling classes rarely
///   happen to reach the same byte-exact instance boundary). Where
///   two candidates collide -- e.g. two unrelated classes that both
///   end at offset $18 for a child to "choose between" -- ParentName
///   is left empty, since picking wrong would hide inherited fields
///   behind a foreign type. Records (skRecord) are skipped: Delphi
///   records cannot inherit, so the offset-matching test wouldn't
///   carry meaning for them.
/// </summary>
procedure TRsmClassParentDeriver.Run;
const
  WIN64_PTR_SIZE = 8;
  WIN32_PTR_SIZE = 4;
var
  I, J       : Integer;
  Info       : TRsmClassInfo;
  PtrSize    : UInt32;
  FirstOffs  : array of UInt32;
  LastEnds   : array of UInt32;
  Kinds      : array of TRsmStructKind;
  Cand, Match: Integer;

  function InstanceSize(AInfo: TRsmClassInfo; AFirstOff, ALastOff: UInt32): UInt32;
  var
    K       : Integer;
    PrevOff : UInt32;
    LastGap : UInt32;
  begin
    // Walk members in offset order to estimate the last field's
    // byte width from the gap to the next, falling back to one
    // pointer for the trailing field whose size we cannot infer
    // from a successor.
    if AInfo.Members.Count = 0 then Exit(AFirstOff);
    PrevOff := AFirstOff;
    LastGap := PtrSize;
    for K := 0 to AInfo.Members.Count - 1 do
    begin
      if AInfo.Members[K].Offset > PrevOff then
        LastGap := AInfo.Members[K].Offset - PrevOff;
      PrevOff := AInfo.Members[K].Offset;
    end;
    Result := ALastOff + LastGap;
  end;

begin
  if FClasses.Count = 0 then Exit;

  // Detect pointer size from the smallest first-field offset across
  // all class-kind entries. A 16-byte aligned proc-only binary still
  // gives us a sane fallback to 8 (Win64) because that's the modern
  // Delphi default.
  PtrSize := WIN64_PTR_SIZE;
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
    if (Info.Kind <> skClass) or (Info.Members.Count = 0) then Continue;
    if Info.Members[0].Offset = WIN32_PTR_SIZE then
    begin
      PtrSize := WIN32_PTR_SIZE;
      Break;
    end;
  end;

  // Cache the small per-class fields the inner parent-matching loop
  // needs into plain arrays. Indexing into an array of byte-wide
  // enums is a single load and replaces a full TRsmClassInfo copy
  // (Name + ParentName strings + Members IList, three refcount
  // atomic ops per access).
  SetLength(FirstOffs, FClasses.Count);
  SetLength(LastEnds, FClasses.Count);
  SetLength(Kinds, FClasses.Count);
  for I := 0 to FClasses.Count - 1 do
  begin
    Info := FClasses[I];
    Kinds[I] := Info.Kind;
    FirstOffs[I] := 0;
    LastEnds[I]  := 0;
    if Info.Kind <> skClass then Continue;
    if Info.Members.Count = 0 then Continue;
    FirstOffs[I] := Info.Members[0].Offset;
    for J := 1 to Info.Members.Count - 1 do
      if Info.Members[J].Offset < FirstOffs[I] then
        FirstOffs[I] := Info.Members[J].Offset;
    LastEnds[I] := 0;
    for J := 0 to Info.Members.Count - 1 do
      if Info.Members[J].Offset > LastEnds[I] then
        LastEnds[I] := Info.Members[J].Offset;
    LastEnds[I] := InstanceSize(Info, FirstOffs[I], LastEnds[I]);
  end;

  for I := 0 to FClasses.Count - 1 do
  begin
    if Kinds[I] <> skClass then Continue;
    if FirstOffs[I] <= PtrSize then Continue;
    Match := -1;
    // Walk candidates from I-1 downto 0 so the first hit is the
    // latest-declared class whose instance ends exactly where I's
    // own fields start.
    for Cand := I - 1 downto 0 do
    begin
      if Kinds[Cand] <> skClass then Continue;
      if LastEnds[Cand] = FirstOffs[I] then
      begin
        Match := Cand;
        Break;
      end;
    end;
    // Tolerance fallback: when no candidate's LastEnds matches
    // FirstOffs[I] exactly, an RTL ancestor like TComponent is the
    // typical reason -- accept the closest preceding match within
    // 16 bytes (tiebreaker: latest-declared).
    if Match < 0 then
    begin
      var BestGap: UInt32 := 17;
      for Cand := I - 1 downto 0 do
      begin
        if Kinds[Cand] <> skClass then Continue;
        if LastEnds[Cand] = 0 then Continue;
        if LastEnds[Cand] > FirstOffs[I] then Continue;
        var Gap: UInt32 := FirstOffs[I] - LastEnds[Cand];
        if Gap < BestGap then
        begin
          BestGap := Gap;
          Match := Cand;
        end;
      end;
      if BestGap > 16 then Match := -1;
    end;
    if Match >= 0 then
    begin
      Info := FClasses[I];
      Info.ParentName := FClasses[Match].Name;
      FClasses[I] := Info;
    end;
  end;
end;

end.
