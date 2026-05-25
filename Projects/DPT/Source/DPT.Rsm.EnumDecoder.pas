// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.EnumDecoder;

// Owns the enum-related lookup tables that the RSM scanner populates
// while walking $25 / $03 / $2A records. Pulled out of DPT.Rsm.Scanner
// so the enum state machine -- pending-buffer, primary/secondary
// bridge, ENUM_DEF synthesis -- has a single home that documents the
// cross-unit-vs-program-local-vs-sibling-unit dance in one place.
//
// The scanner still does the byte-level format parsing (variant-form
// dispatch, payload-shape validation, forward unit-name scan) and
// invokes the semantic Record* methods below; this decoder owns the
// resulting tables and exposes them as read-only properties.

interface

uses
  System.SysUtils,

  mormot.core.base,
  mormot.core.collections,

  DPT.Rsm.Model;

type

  /// <summary>
  ///   State machine that turns parsed RSM enum records into the
  ///   lookup tables the reader consumes. Owns six containers and
  ///   the per-enum pending-constant buffer.
  /// </summary>
  TRsmEnumDecoder = class
  private
    /// "&lt;enumTypeId&gt;:&lt;ordinal&gt;" -> enum-constant identifier
    /// name. Populated by program-local $25 records and by $2A flushes
    /// of buffered same-compilation $25 records.
    FEnumConstNames      : IKeyValue<String, String>;
    /// All RSM type ids that appear as enums (in $25 or via $2A alias
    /// linking). The "is this an enum?" oracle.
    FEnumTypeIds         : IKeyValue<UInt32, Boolean>;
    /// Subset populated ONLY from the cross-unit $25 form. Used as
    /// the filter set when scanning $2A bodies for secondaries; avoids
    /// false-matches with program-local enum ids whose $2E hi byte
    /// coincidentally appears in many class bodies.
    FCrossUnitEnumIds    : IKeyValue<UInt32, Boolean>;
    /// Maps an enum alias id (the type id used in $2A primary slot or
    /// in a class-field record) to the LIST of canonical ids under
    /// which (typeId, ordinal) -> name pairs might be stored in
    /// FEnumConstNames.
    FEnumAliasesByPrimary: IKeyValue<UInt32, IList<UInt32>>;
    /// All TRsmEnumDef records discovered via the $03 ENUM_DEF tag,
    /// plus the synthetic defs the $2A flush builds for same-
    /// compilation sibling-unit enums. Authoritative source for
    /// "this enum has these elements in this declaration order,
    /// declared in this unit".
    FEnumDefs            : IList<TRsmEnumDef>;
    /// Buffer of $25 enum-constant records seen since the last $2A
    /// type-registry entry. The same-compilation $25 form encodes
    /// elements with a SHARED secondary id (typically 0x0002) that
    /// collides across sibling-unit enums; we delay writing them
    /// into FEnumConstNames until the matching $2A surfaces a unique
    /// primary id, then flush all buffered constants keyed by that
    /// primary -- avoiding the last-wins collision on the secondary.
    FPendingConstants    : IList<TRsmEnumElement>;
  public
    constructor Create;

    /// <summary>
    ///   Clear all tables and the pending buffer. Called by the
    ///   scanner at the start of every LoadFromBuffer.
    /// </summary>
    procedure Reset;

    /// <summary>
    ///   Program-local form ($25 + $0A + ...): typeId is a unique
    ///   primary, so the constant registers immediately under
    ///   (typeId, ordinal).
    /// </summary>
    procedure RecordProgramLocalConstant(ATypeId: UInt32;
      AOrdinal: Integer; const AName: String);

    /// <summary>
    ///   Cross-unit RTL form ($25 + $8A + ..., typeId-hi &lt;&gt; 0):
    ///   typeId is a real cross-unit primary (e.g. 0x0441 for
    ///   TThreadPriority). Registered immediately AND added to the
    ///   cross-unit secondary set so subsequent $2A bridge checks
    ///   can recognise it.
    /// </summary>
    procedure RecordCrossUnitRtlConstant(ATypeId: UInt32;
      AOrdinal: Integer; const AName: String);

    /// <summary>
    ///   Same-compilation cross-unit form ($25 + $8A + ...,
    ///   typeId-hi = 0): typeId is a SHARED secondary that collides
    ///   across sibling-unit enums. The constant is BUFFERED until
    ///   the matching $2A primary surfaces; the secondary is recorded
    ///   in the cross-unit set so the $2A bridge can confirm it.
    /// </summary>
    procedure RecordCrossUnitSameCompConstant(ASecId: UInt32;
      AOrdinal: Integer; const AName: String);

    /// <summary>
    ///   Append a parsed $03 ENUM_DEF record. Two units declaring the
    ///   same type name produce TWO entries, NOT one.
    /// </summary>
    procedure RecordEnumDef(const ADef: TRsmEnumDef);

    /// <summary>
    ///   Apply a $2A type-registry entry. Performs three jobs in
    ///   order:
    ///   <list type="number">
    ///     <item>Bridge primary -> secondary alias-list when
    ///       <paramref name="ASecCandidate"/> is in the cross-unit
    ///       set.</item>
    ///     <item>Flush pending $25 constants under
    ///       <paramref name="APrimary"/>, keyed by
    ///       (Primary, ordinal).</item>
    ///     <item>Synthesise an EnumDef from the pending buffer when
    ///       <paramref name="AUnitNameSparse"/> is non-empty -- the
    ///       same-compilation flow needs this because the $2A flush
    ///       is the first point where a (unit, type) pair becomes
    ///       knowable.</item>
    ///   </list>
    ///   Clears the pending buffer regardless.
    /// </summary>
    procedure RecordTypeRegistry(APrimary, ASecCandidate: UInt32;
      const ATypeName, AUnitNameSparse: String);

    /// <summary>
    ///   True when at least one same-compilation $25 record has
    ///   been buffered since the last RecordTypeRegistry. The
    ///   scanner queries this to decide whether to do the costly
    ///   1024-byte forward-scan for the owning unit name: when
    ///   nothing is pending, the unit name has no consumer and
    ///   the scan is wasted work.
    /// </summary>
    function HasPendingConstants: Boolean;

    property EnumConstNames     : IKeyValue<String, String> read FEnumConstNames;
    property EnumTypeIds        : IKeyValue<UInt32, Boolean> read FEnumTypeIds;
    property CrossUnitEnumIds   : IKeyValue<UInt32, Boolean> read FCrossUnitEnumIds;
    property EnumAliasesByPrimary: IKeyValue<UInt32, IList<UInt32>> read FEnumAliasesByPrimary;
    property EnumDefs           : IList<TRsmEnumDef> read FEnumDefs;
  end;

implementation

{ TRsmEnumDecoder }

constructor TRsmEnumDecoder.Create;
begin
  inherited Create;
  FEnumConstNames       := Collections.NewPlainKeyValue<String, String>;
  FEnumTypeIds          := Collections.NewPlainKeyValue<UInt32, Boolean>;
  FCrossUnitEnumIds     := Collections.NewPlainKeyValue<UInt32, Boolean>;
  FEnumAliasesByPrimary := Collections.NewPlainKeyValue<UInt32, IList<UInt32>>;
  FEnumDefs             := Collections.NewPlainList<TRsmEnumDef>;
  FPendingConstants     := Collections.NewPlainList<TRsmEnumElement>;
end;

procedure TRsmEnumDecoder.Reset;
begin
  FEnumConstNames.Clear;
  FEnumTypeIds.Clear;
  FCrossUnitEnumIds.Clear;
  FEnumAliasesByPrimary.Clear;
  FEnumDefs.Clear;
  FPendingConstants.Clear;
end;

procedure TRsmEnumDecoder.RecordProgramLocalConstant(ATypeId: UInt32;
  AOrdinal: Integer; const AName: String);
begin
  FEnumTypeIds[ATypeId] := True;
  FEnumConstNames[IntToStr(ATypeId) + ':' + IntToStr(AOrdinal)] := AName;
end;

procedure TRsmEnumDecoder.RecordCrossUnitRtlConstant(ATypeId: UInt32;
  AOrdinal: Integer; const AName: String);
begin
  FEnumTypeIds[ATypeId] := True;
  FCrossUnitEnumIds[ATypeId] := True;
  FEnumConstNames[IntToStr(ATypeId) + ':' + IntToStr(AOrdinal)] := AName;
end;

procedure TRsmEnumDecoder.RecordCrossUnitSameCompConstant(ASecId: UInt32;
  AOrdinal: Integer; const AName: String);
var
  Pe: TRsmEnumElement;
begin
  FCrossUnitEnumIds[ASecId] := True;
  Pe.Name    := AName;
  Pe.Ordinal := AOrdinal;
  FPendingConstants.Add(Pe);
end;

procedure TRsmEnumDecoder.RecordEnumDef(const ADef: TRsmEnumDef);
begin
  FEnumDefs.Add(ADef);
end;

function TRsmEnumDecoder.HasPendingConstants: Boolean;
begin
  Result := FPendingConstants.Count > 0;
end;

procedure TRsmEnumDecoder.RecordTypeRegistry(APrimary, ASecCandidate: UInt32;
  const ATypeName, AUnitNameSparse: String);
var
  AliasList: IList<UInt32>;
  FI       : Integer;
  Fe       : TRsmEnumElement;
  Def      : TRsmEnumDef;
begin
  // Bridge: primary -> secondary alias-list, but only when the
  // secondary has appeared in a prior cross-unit $25 record.
  // Filtering on FCrossUnitEnumIds keeps us from chasing
  // coincidental 2-byte slots in $2A bodies of non-enum types.
  if (APrimary <> 0) and (ASecCandidate <> 0) and
     (ASecCandidate <> APrimary) and
     FCrossUnitEnumIds.ContainsKey(ASecCandidate) then
  begin
    FEnumTypeIds[APrimary] := True;
    if not FEnumAliasesByPrimary.TryGetValue(APrimary, AliasList) then
    begin
      AliasList := Collections.NewPlainList<UInt32>;
      FEnumAliasesByPrimary[APrimary] := AliasList;
    end;
    if AliasList.IndexOf(ASecCandidate) < 0 then
      AliasList.Add(ASecCandidate);
  end;
  // Flush pending same-compilation $25 records under THIS $2A's
  // primary. The pending buffer is the per-enum block emitted
  // right before this registry entry; the secondary id used to
  // key those records would collide across sibling-unit enums
  // (same secondary 0x0002 for all three TStatus's), but
  // primary-keying is unique per (unit, type) and so
  // disambiguates cleanly.
  if (APrimary <> 0) and (FPendingConstants.Count > 0) then
  begin
    for FI := 0 to FPendingConstants.Count - 1 do
    begin
      Fe := FPendingConstants[FI];
      FEnumConstNames[IntToStr(APrimary) + ':' + IntToStr(Fe.Ordinal)] := Fe.Name;
    end;
    FEnumTypeIds[APrimary] := True;

    // Synthesise an EnumDef when the caller recovered a unit name
    // via its forward-scan. The buffered constants carry their
    // explicit ordinals -- the def works for sparse enums (gaps
    // and non-zero starts) since lookup goes through
    // TRsmEnumDef.TryFindByOrdinal.
    if AUnitNameSparse <> '' then
    begin
      Def.TypeName := ATypeName;
      Def.UnitName := AUnitNameSparse;
      Def.Elements := Collections.NewPlainList<TRsmEnumElement>;
      for FI := 0 to FPendingConstants.Count - 1 do
        Def.Elements.Add(FPendingConstants[FI]);
      FEnumDefs.Add(Def);
    end;
  end;
  FPendingConstants.Clear;
end;

end.
