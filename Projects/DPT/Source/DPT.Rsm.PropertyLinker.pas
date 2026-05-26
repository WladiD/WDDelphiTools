// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.PropertyLinker;

interface

uses
  System.SysUtils,

  mormot.core.base,
  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.BufferIO;

type

  /// <summary>
  ///   Walks the RSM byte stream for <c>$31</c> property records and
  ///   attaches the parsed <see cref="TRsmClassProperty"/> entries to
  ///   the owning class's <c>Properties</c> list.
  /// </summary>
  /// <remarks>
  ///   The Delphi compiler emits one <c>$31</c> record per
  ///   <c>property &lt;Name&gt;: &lt;Type&gt; read &lt;Field-or-Getter&gt;</c>
  ///   clause on every class that declares properties. The record's
  ///   on-disk shape is documented in §4.16 of
  ///   <c>DPT.Rsm.Format.md</c> and is summarised here for the parser:
  ///   <code>
  ///     [$FF]?  $31  &lt;NL&gt;  &lt;Name&gt;
  ///         $00 $02 $00            { Format-A anchor (same as $2C field) }
  ///         &lt;prim-type: u8&gt;       { $02=Integer, $04=string, ... }
  ///         $FE $0F $00 $00 $00 $80
  ///         &lt;target-lo&gt; &lt;target-hi&gt;
  ///         ...
  ///   </code>
  ///   The 2-byte target at body+10..+11 either matches a sibling
  ///   <c>$2C</c> field record's secondary id (body+7..+8) — making
  ///   the property field-backed — or points at a getter method
  ///   record otherwise.
  ///
  ///   <para>Bridging the target to a field requires walking every
  ///   <c>$2C</c> record once to build a (2-byte target -&gt; field
  ///   name) map. Owning-class attribution reuses the same anchor
  ///   block-owner pattern the <c>FormatALinker</c> documents — the
  ///   property record sits inside the same contiguous block as the
  ///   class's $2C field records, terminated by a <c>$0E</c> record-
  ///   sentinel marker on the parent class.</para>
  /// </remarks>
  TRsmPropertyLinker = class
  private
    FClasses             : IList<TRsmClassInfo>;
    FClassByName         : IKeyValue<String, Integer>;
    FRsmTypeIdToClassIdx : IKeyValue<UInt32, Integer>;
    FBuf                 : PByte;
    FSz                  : NativeInt;
    function ByteAt(AOffset: NativeInt): Byte; inline;
    function ReadIdentifier(AOffset: NativeInt; out AName: String): Boolean; inline;
  public
    constructor Create(
      AClasses             : IList<TRsmClassInfo>;
      AClassByName         : IKeyValue<String, Integer>;
      ARsmTypeIdToClassIdx : IKeyValue<UInt32, Integer>);
    procedure Run(ABuf: PByte; ASz: NativeInt);
  end;

implementation

{ TRsmPropertyLinker }

constructor TRsmPropertyLinker.Create(
  AClasses             : IList<TRsmClassInfo>;
  AClassByName         : IKeyValue<String, Integer>;
  ARsmTypeIdToClassIdx : IKeyValue<UInt32, Integer>);
begin
  inherited Create;
  FClasses             := AClasses;
  FClassByName         := AClassByName;
  FRsmTypeIdToClassIdx := ARsmTypeIdToClassIdx;
end;

function TRsmPropertyLinker.ByteAt(AOffset: NativeInt): Byte;
begin
  Result := RsmByteAt(FBuf, AOffset);
end;

function TRsmPropertyLinker.ReadIdentifier(AOffset: NativeInt;
  out AName: String): Boolean;
begin
  Result := RsmReadIdentifier(FBuf, FSz, AOffset, AName);
end;

procedure TRsmPropertyLinker.Run(ABuf: PByte; ASz: NativeInt);
// Single sequential walk over the byte stream. Three record kinds
// drive state:
//   $2A type-registry record -- builds a local (TypeId -> Name)
//                        lookup so records-with-methods that the
//                        StructDiscoverer's Format-B walker missed
//                        can still be synthesized when their
//                        properties surface.
//   $2C field record  -- updates the (alias id -> field name) map
//                        AND, when it carries a wide-encoded parent
//                        type id, sets LastClsIdx to the owning
//                        class so subsequent $31 records attach
//                        there.
//   $31 property record -- attaches a TRsmClassProperty to
//                        LastClsIdx (the class whose $2C block we
//                        most recently passed through).
//
// Wide-vs-narrow parent-id encoding follows the $2C body's
// terminator: after the "07 00 00 08" marker, byte+4..+5 is the
// parent's TRsmTypeId. Hi==$FF means narrow (1-byte per-unit local
// id) and we skip -- those classes won't carry property records
// either, and the block-owner heuristic for narrow ids lives in
// FormatALinker.BuildBlockOwnerIndex. Hi!=$FF means a wide
// $2A-registry id, which we look up via FRsmTypeIdToClassIdx.
// Misses there fall back to the local TypeIdToName map and
// synthesize a new skRecord TRsmClassInfo when the name is known.
var
  AliasIdToField : IKeyValue<UInt32, String>;
  TypeIdToName   : IKeyValue<UInt32, String>;
  P              : NativeInt;
  NL             : Byte;
  Name           : String;
  After          : NativeInt;
  EndOff         : NativeInt;
  I              : Integer;
  AliasId        : UInt32;
  ParentTypeId   : UInt32;
  LastClsIdx     : Integer;
  Cls            : TRsmClassInfo;
  Prop           : TRsmClassProperty;

  function ResolveOwningClass(AParentTypeId: UInt32): Integer;
  // First tries the read-only FClasses index via the type-id
  // map. If that misses, looks the name up in the local
  // TypeIdToName map and -- if a name is known but no FClasses
  // entry exists -- synthesizes a skRecord entry so property
  // attribution can proceed. Returns -1 only when neither the
  // type-id nor a name can be recovered.
  var
    SynName : String;
    LowName : String;
    NewCls  : TRsmClassInfo;
  begin
    if FRsmTypeIdToClassIdx.TryGetValue(AParentTypeId, Result) then Exit;
    if not TypeIdToName.TryGetValue(AParentTypeId, SynName) then
      Exit(-1);
    LowName := LowerCase(SynName);
    if FClassByName.TryGetValue(LowName, Result) then
    begin
      // Existing entry under that name -- bind the type-id so
      // subsequent calls hit the fast path.
      FRsmTypeIdToClassIdx[AParentTypeId] := Result;
      Exit;
    end;
    // Synthesize. records-with-methods that the StructDiscoverer
    // skipped land here. We don't have a member list (the walker
    // didn't run) but for property attribution that's fine -- the
    // Properties list is what the test asserts, and downstream
    // dotted-field navigation through this synthetic record stays
    // unsupported (out of scope for §4.16).
    NewCls := Default(TRsmClassInfo);
    NewCls.Name    := SynName;
    NewCls.Kind    := skRecord;
    NewCls.Members := Collections.NewPlainList<TRsmClassMember>;
    Result := FClasses.Count;
    FClasses.Add(NewCls);
    FClassByName[LowName] := Result;
    FRsmTypeIdToClassIdx[AParentTypeId] := Result;
  end;

begin
  FBuf := ABuf;
  FSz  := ASz;
  if (FBuf = nil) or (FSz < 16) then Exit;
  if FClasses.Count = 0 then Exit;

  AliasIdToField := Collections.NewPlainKeyValue<UInt32, String>;
  TypeIdToName   := Collections.NewPlainKeyValue<UInt32, String>;
  LastClsIdx     := -1;
  P              := 1;
  while P + 24 < FSz do
  begin
    var B: Byte := ByteAt(P);
    // ---- $2A type-registry record --------------------------
    // Build local (TypeId -> Name) so we can synthesize record
    // entries the StructDiscoverer missed (records-with-methods).
    if B = $2A then
    begin
      NL := ByteAt(P + 1);
      if (NL >= 2) and (NL <= 40) and
         (P + 2 + NL + 5 < FSz) and
         (ByteAt(P + 2) = Byte(Ord('T'))) and
         (ByteAt(P + 2 + NL + 1) = $00) and
         (ByteAt(P + 2 + NL + 2) = $00) and
         ReadIdentifier(P + 1, Name) then
      begin
        var TypeId: UInt32 := UInt32(ByteAt(P + 2 + NL + 3)) or
                              (UInt32(ByteAt(P + 2 + NL + 4)) shl 8);
        if (TypeId <> 0) and (not TypeIdToName.ContainsKey(TypeId)) then
          TypeIdToName[TypeId] := Name;
      end;
      Inc(P);
      Continue;
    end;
    // ---- $2C field record -----------------------------------
    if B = $2C then
    begin
      NL := ByteAt(P + 1);
      if (NL < 2) or (NL > 40) or (P + 2 + NL + 12 >= FSz) then
      begin
        Inc(P);
        Continue;
      end;
      After := P + 2 + NL;
      // Accept both anchor variants ($00 $02 $00 canonical, and
      // $00 $00 $00 for property-backing fields).
      if (ByteAt(After) <> $00) or (ByteAt(After + 2) <> $00) or
         ((ByteAt(After + 1) <> $02) and (ByteAt(After + 1) <> $00)) then
      begin
        Inc(P);
        Continue;
      end;
      if not ReadIdentifier(P + 1, Name) then
      begin
        Inc(P);
        Continue;
      end;
      // Capture the (alias-id -> field-name) bridge.
      AliasId := UInt32(ByteAt(After + 7)) or
                 (UInt32(ByteAt(After + 8)) shl 8);
      if (AliasId <> 0) and (not AliasIdToField.ContainsKey(AliasId)) then
        AliasIdToField[AliasId] := Name;
      // Walk forward looking for the "07 00 00 08" terminator
      // (parent id at terminator+4..+5).
      EndOff := -1;
      for I := After + 5 to After + 30 do
      begin
        if I + 5 > FSz then Break;
        if (ByteAt(I) = $07) and (ByteAt(I + 1) = $00) and
           (ByteAt(I + 2) = $00) and (ByteAt(I + 3) = $08) then
        begin
          EndOff := I;
          Break;
        end;
      end;
      if EndOff < 0 then
      begin
        Inc(P);
        Continue;
      end;
      // Resolve owning class via wide parent-id. Narrow ids
      // (Hi=$FF) belong to FormatALinker's per-unit pairing
      // logic and don't reach property records anyway.
      if ByteAt(EndOff + 5) <> $FF then
      begin
        ParentTypeId := UInt32(ByteAt(EndOff + 4)) or
                        (UInt32(ByteAt(EndOff + 5)) shl 8);
        I := ResolveOwningClass(ParentTypeId);
        if I >= 0 then LastClsIdx := I;
      end;
      P := EndOff + 6;
      Continue;
    end;
    // ---- $31 property record --------------------------------
    if B = $31 then
    begin
      NL := ByteAt(P + 1);
      if (NL < 2) or (NL > 64) or (P + 2 + NL + 12 >= FSz) then
      begin
        Inc(P);
        Continue;
      end;
      After := P + 2 + NL;
      if (ByteAt(After) <> $00) or (ByteAt(After + 1) <> $02) or
         (ByteAt(After + 2) <> $00) then
      begin
        Inc(P);
        Continue;
      end;
      // Fixed 6-byte marker at body+4..+9 disambiguates a real
      // $31 property record from a spurious tag-byte match.
      if (ByteAt(After + 4) <> $FE) or (ByteAt(After + 5) <> $0F) or
         (ByteAt(After + 6) <> $00) or (ByteAt(After + 7) <> $00) or
         (ByteAt(After + 8) <> $00) or (ByteAt(After + 9) <> $80) then
      begin
        Inc(P);
        Continue;
      end;
      if not ReadIdentifier(P + 1, Name) then
      begin
        Inc(P);
        Continue;
      end;
      // Attribute to the class whose $2C block we're inside.
      if (LastClsIdx < 0) or (LastClsIdx >= FClasses.Count) then
      begin
        Inc(P);
        Continue;
      end;
      // Both classes (skClass) and records (skRecord) carry $31
      // property records -- Delphi records have supported properties
      // since the language got method/operator overloading. The
      // attribution mechanism (wide parent-type-id via the last
      // $2C in the same block) works identically for both kinds.
      Cls := FClasses[LastClsIdx];
      if Cls.Properties = nil then
        Cls.Properties := Collections.NewPlainList<TRsmClassProperty>;
      Prop := Default(TRsmClassProperty);
      Prop.Name            := Name;
      Prop.PrimitiveTypeId := ByteAt(After + 3);
      Prop.TargetId        := UInt16(ByteAt(After + 10)) or
                              (UInt16(ByteAt(After + 11)) shl 8);
      // Bridge target-id -> field name. When the target matches a
      // known $2C field's alias id, the property is field-backed
      // and UnderlyingField names the field. Otherwise the
      // property is getter-backed and UnderlyingField stays empty.
      AliasIdToField.TryGetValue(Prop.TargetId, Prop.UnderlyingField);
      Cls.Properties.Add(Prop);
      FClasses[LastClsIdx] := Cls;
      Inc(P);
      Continue;
    end;
    Inc(P);
  end;
end;

end.
