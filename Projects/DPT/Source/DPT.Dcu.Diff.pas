// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Diff;

interface

uses

  mormot.core.collections,

  DPT.Dcu.Types;

type

  /// <summary>
  ///   Verdict for a DCU diff. The order is "increasing severity": each
  ///   subsequent value implies all the differences detected by the
  ///   previous ones plus more.
  /// </summary>
  TDcuDiffStatus = (
    ddIdentical,        // No tracked field differs.
    ddMetadataOnly,     // Magic bytes / compiler / platform differ but
                        // the unit's content (uses/sources) is identical.
    ddSourceChange,     // Source file references differ.
    ddUsesOrderChange,  // Same uses set, different order. Init order
                        // and name resolution can change as a result.
    ddUsesSetChange,    // Uses set membership differs (added/removed).
    ddDifferentUnits    // The two DCUs describe different unit names.
  );

  /// <summary>
  ///   Detailed diff result. Each list captures one specific dimension
  ///   of difference; an empty list means "no change in that dimension".
  ///   <c>HeaderChanges</c> holds human-readable single-line entries.
  /// </summary>
  TDcuDiffReport = record
    Status                  : TDcuDiffStatus;
    PathA                   : string;
    PathB                   : string;
    UnitNameA               : string;
    UnitNameB               : string;

    HeaderChanges           : IList<string>;

    IncludesAdded           : IList<string>;
    IncludesRemoved         : IList<string>;

    InterfaceUsesAdded      : IList<string>;
    InterfaceUsesRemoved    : IList<string>;
    InterfaceUsesOrderChange: Boolean;

    ImplUsesAdded           : IList<string>;
    ImplUsesRemoved         : IList<string>;
    ImplUsesOrderChange     : Boolean;
  end;

  TDcuDiff = class
  private
    class function ListEntries(AList: IList<TDcuUsesEntry>): IList<string>;
    class procedure DiffUsesScope(AOld, ANew: IList<TDcuUsesEntry>;
      AAdded, ARemoved: IList<string>; out AOrderChange: Boolean);
    class procedure DiffSources(const AOld, ANew: TDcuHeaderInfo;
      AAdded, ARemoved: IList<string>);
    class procedure DiffHeader(const AOld, ANew: TDcuHeaderInfo;
      AChanges: IList<string>);
    class function DeriveStatus(const AReport: TDcuDiffReport): TDcuDiffStatus;
  public
    class function Compare(const AOld, ANew: TDcuAnalysisResult): TDcuDiffReport;
  end;

const
  DcuDiffStatusName: array[TDcuDiffStatus] of string = (
    'Identical',
    'MetadataOnly',
    'SourceChange',
    'UsesOrderChange',
    'UsesSetChange',
    'DifferentUnits'
  );

implementation

uses

  System.SysUtils;

{ TDcuDiff }

class function TDcuDiff.ListEntries(AList: IList<TDcuUsesEntry>): IList<string>;
var
  Entry: TDcuUsesEntry;
begin
  Result := Collections.NewList<string>;
  for Entry in AList do
    Result.Add(Entry.UnitName);
end;

class procedure TDcuDiff.DiffUsesScope(AOld, ANew: IList<TDcuUsesEntry>;
  AAdded, ARemoved: IList<string>; out AOrderChange: Boolean);
var
  I       : Integer;
  OldNames: IList<string>;
  NewNames: IList<string>;
begin
  OldNames := ListEntries(AOld);
  NewNames := ListEntries(ANew);

  // Set difference: anything in OldNames missing from NewNames -> removed,
  // anything in NewNames missing from OldNames -> added. Case-insensitive
  // because Delphi unit-name comparison is case-insensitive.
  for I := 0 to OldNames.Count - 1 do
  begin
    var Found := False;
    for var J := 0 to NewNames.Count - 1 do
      if SameText(OldNames[I], NewNames[J]) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      ARemoved.Add(OldNames[I]);
  end;
  for I := 0 to NewNames.Count - 1 do
  begin
    var Found := False;
    for var J := 0 to OldNames.Count - 1 do
      if SameText(NewNames[I], OldNames[J]) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      AAdded.Add(NewNames[I]);
  end;

  // Order change: same set, different sequence. We compare a
  // case-folded copy of the original-order list so that "DPT.Foo" and
  // "DPT.foo" do not register as an order change purely from casing.
  AOrderChange := False;
  if (AAdded.Count = 0) and (ARemoved.Count = 0) and
     (OldNames.Count = NewNames.Count)
  then
  begin
    for I := 0 to OldNames.Count - 1 do
      if not SameText(OldNames[I], NewNames[I]) then
      begin
        AOrderChange := True;
        Break;
      end;
  end;
end;

class procedure TDcuDiff.DiffSources(const AOld, ANew: TDcuHeaderInfo;
  AAdded, ARemoved: IList<string>);

  function CollectAll(const AHeader: TDcuHeaderInfo): IList<string>;
  var
    Inc: TDcuSourceRef;
  begin
    Result := Collections.NewList<string>;
    if AHeader.PrimarySource.FileName <> '' then
      Result.Add(AHeader.PrimarySource.FileName);
    for Inc in AHeader.IncludeSources do
      Result.Add(Inc.FileName);
  end;

var
  OldSrcs: IList<string>;
  NewSrcs: IList<string>;
  I, J   : Integer;
begin
  OldSrcs := CollectAll(AOld);
  NewSrcs := CollectAll(ANew);
  for I := 0 to OldSrcs.Count - 1 do
  begin
    var Found := False;
    for J := 0 to NewSrcs.Count - 1 do
      if SameText(OldSrcs[I], NewSrcs[J]) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      ARemoved.Add(OldSrcs[I]);
  end;
  for I := 0 to NewSrcs.Count - 1 do
  begin
    var Found := False;
    for J := 0 to OldSrcs.Count - 1 do
      if SameText(NewSrcs[I], OldSrcs[J]) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      AAdded.Add(NewSrcs[I]);
  end;
end;

class procedure TDcuDiff.DiffHeader(const AOld, ANew: TDcuHeaderInfo;
  AChanges: IList<string>);
begin
  if AOld.MagicHex <> ANew.MagicHex then
    AChanges.Add(Format('Magic: %s -> %s', [AOld.MagicHex, ANew.MagicHex]));
  if AOld.DetectedCompiler <> ANew.DetectedCompiler then
    AChanges.Add(Format('Compiler: %s -> %s',
      [DcuKnownCompilerName[AOld.DetectedCompiler],
       DcuKnownCompilerName[ANew.DetectedCompiler]]));
  if AOld.DetectedPlatform <> ANew.DetectedPlatform then
    AChanges.Add(Format('Platform: %s -> %s',
      [DcuPlatformName[AOld.DetectedPlatform],
       DcuPlatformName[ANew.DetectedPlatform]]));
end;

class function TDcuDiff.DeriveStatus(const AReport: TDcuDiffReport): TDcuDiffStatus;
begin
  if not SameText(AReport.UnitNameA, AReport.UnitNameB) then
    Exit(ddDifferentUnits);

  if (AReport.InterfaceUsesAdded.Count > 0) or
     (AReport.InterfaceUsesRemoved.Count > 0) or
     (AReport.ImplUsesAdded.Count > 0) or
     (AReport.ImplUsesRemoved.Count > 0)
  then
    Exit(ddUsesSetChange);

  if AReport.InterfaceUsesOrderChange or AReport.ImplUsesOrderChange then
    Exit(ddUsesOrderChange);

  if (AReport.IncludesAdded.Count > 0) or (AReport.IncludesRemoved.Count > 0) then
    Exit(ddSourceChange);

  if AReport.HeaderChanges.Count > 0 then
    Exit(ddMetadataOnly);

  Result := ddIdentical;
end;

class function TDcuDiff.Compare(const AOld, ANew: TDcuAnalysisResult): TDcuDiffReport;
begin
  Result := Default(TDcuDiffReport);
  Result.PathA := AOld.FilePath;
  Result.PathB := ANew.FilePath;
  Result.UnitNameA := AOld.Header.UnitName;
  Result.UnitNameB := ANew.Header.UnitName;

  Result.HeaderChanges := Collections.NewList<string>;
  Result.IncludesAdded := Collections.NewList<string>;
  Result.IncludesRemoved := Collections.NewList<string>;
  Result.InterfaceUsesAdded := Collections.NewList<string>;
  Result.InterfaceUsesRemoved := Collections.NewList<string>;
  Result.ImplUsesAdded := Collections.NewList<string>;
  Result.ImplUsesRemoved := Collections.NewList<string>;

  DiffHeader(AOld.Header, ANew.Header, Result.HeaderChanges);
  DiffSources(AOld.Header, ANew.Header,
    Result.IncludesAdded, Result.IncludesRemoved);
  DiffUsesScope(AOld.InterfaceUses, ANew.InterfaceUses,
    Result.InterfaceUsesAdded, Result.InterfaceUsesRemoved,
    Result.InterfaceUsesOrderChange);
  DiffUsesScope(AOld.ImplementationUses, ANew.ImplementationUses,
    Result.ImplUsesAdded, Result.ImplUsesRemoved,
    Result.ImplUsesOrderChange);

  Result.Status := DeriveStatus(Result);
end;

end.
