// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Rsm.Reader;

// Contract tests for TRsmReader's public lookups. The scanner-side
// tests (Test.DPT.Rsm.Scanner) cover the raw byte-stream output;
// these tests verify the value the reader's post-process passes
// (Format-A linking, class-parent derivation, cross-unit type-id
// resolution) add on top of that data via the Find/Is/TryGet API.
// Test.DPT.Rsm.LocalsReader (TRsmReaderLegacyTests) exercises the
// wider behavioural surface against the same TRsmReader class and
// is the regression fence; here we focus on what the facade itself
// contributes.

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TRsmReaderTests = class
  private
    function ResolveExePath(AUse64Bit: Boolean): String;
    procedure DoTestSourceFileAttribution(AUse64Bit: Boolean);
  public
    /// Loading a missing file leaves all lookups returning their
    /// sentinel "not found" values.
    [Test]
    procedure TestLoadFromMissingFileLeavesEmpty;

    /// Format-A field linking produces a non-zero TypeIdx on class
    /// fields whose declared type matches a class/record registered
    /// in the type registry. We don't pin the value (varies by
    /// build) but require that at least one Format-A member came
    /// through with a non-zero TypeIdx.
    [Test]
    procedure TestFormatALinkingPopulatesMemberTypeIds;

    /// Class-parent derivation joins TInner -> TDeepDerived chain via
    /// the layout-matching pass. FindClassMember resolves inherited
    /// fields by walking the chain.
    [Test]
    procedure TestInheritedFieldVisibleViaFindClassMember;

    /// Enum oracle. TLightStatus is a program-local enum; its primary
    /// 2-byte id must be marked as an enum and lsGreen at ordinal 2
    /// resolves to its identifier.
    [Test]
    procedure TestEnumOracleResolvesLightStatus;

    /// Cross-unit enum via the $25 $8A-prefix + $2A alias linking
    /// path: TThreadPriority's tpHigher (ordinal 4) resolves through
    /// the name-based prefix disambiguator.
    [Test]
    procedure TestEnumOracleResolvesCrossUnitThreadPriority;

    /// <summary>
    ///   §6.24 DebugTarget leakage guard. The heuristic name-convention
    ///   enum late-binding pass (TFW's TAd.Land -> TLandTyp, pinned in
    ///   Test.DPT.Rsm.Taifun) must NOT invent a phantom enum on the clean
    ///   DebugTarget fixture: <c>TMixedRec.FMixedInt</c> is an Integer
    ///   field with no matching <c>TMixedInt</c> / <c>TMixedIntTyp</c>
    ///   enum, so it must stay non-enum-typed. The genuine TLightStatus
    ///   enum must still resolve (lsGreen at ordinal 2), proving the
    ///   extended pass did not corrupt the shared scope-local map.
    /// </summary>
    [Test]
    procedure TestRecordFieldEnumNameConventionDoesNotPhantomBind32;

    /// Record-typed global proximity resolution. GGlobalEnumRec is a
    /// record-typed module-level global; the dotted-walk uses
    /// FindBestRecordForGlobalAndField to recover its type from
    /// byte-stream proximity. The result must be a skRecord index.
    [Test]
    procedure TestFindBestRecordForGlobalAndField;

    /// <summary>
    ///   §4.17 / §6.21 pin. <c>TRsmReader.UnitsDeclaringType</c>
    ///   aggregates every <c>$64</c> segment whose <c>$66</c> entries
    ///   include the requested type, deduplicates the declaring-unit
    ///   names case-insensitively, and preserves first-seen order.
    ///   We probe <c>'Boolean'</c> (declared in <c>System</c>) and
    ///   assert: at least one declaring unit comes back; the result
    ///   is deduplicated (every entry appears exactly once); every
    ///   name is non-empty (leakage guard against a partially-
    ///   decoded segment slipping through with an empty UnitName).
    ///   Negative probes: a nonsense type name and an empty string
    ///   both return an empty array.
    /// </summary>
    [Test]
    procedure TestUnitsDeclaringTypeAggregatesAcrossSegments;

    /// §6.25 R1 partial-closure pin: after RunPostProcess, no EnumDef's
    /// TypeName is a known VMT CLASS (skClass). The same-comp $2A flush
    /// synthesises a phantom "enum" whenever a non-enum $2A consumes the
    /// pending $25 constant buffer -- RsmDesk surfaced e.g.
    /// TPublishableVariantType (a class in System.TypInfo) carrying
    /// TTypeKind's values. FilterStructNamedEnumDefs drops every EnumDef
    /// whose name is a known skClass. Leakage guards key on CONTROLLED
    /// fixture enums (TLightStatus, TStatus) that the linker never
    /// dead-code-eliminates -- NOT on incidental RTL enums, whose
    /// presence depends on which System units happen to be linked.
    [Test]
    procedure TestEnumDefsExcludeClassNames;

    /// <summary>
    ///   §4.17 pin: the `$70 <SourceFile>` introducer is decoded into
    ///   the normalized <see cref="TRsmReader.SourceFiles"/> table, and
    ///   each `$64` segment is attributed to its importing unit via the
    ///   <see cref="TRsmUnitUseSegment.SourceFileIdx"/> foreign key (the
    ///   importer name stored ONCE, not per segment). Asserts:
    ///   (a) SourceFiles deduped (no repeated UnitName) and StripDirAndExt
    ///   applied (`Winapi.ImageHlp.pas` -> `Winapi.ImageHlp`);
    ///   (b) the `uses` relationship `Winapi.ImageHlp -> Winapi.Windows`
    ///   is attributable (a segment imported by Winapi.ImageHlp that
    ///   declares Winapi.Windows), with FK integrity;
    ///   (c) leakage: the lone `System.pas` (System imports nothing, no
    ///   `$64` follows) is NOT recorded as a SourceFile, and no segment
    ///   is orphaned (SourceFileIdx >= 0);
    ///   (d) UnitsImporting is the exact inverse of UnitsDeclaringType --
    ///   `Boolean` has one declaring unit (System) but many importers.
    ///   Offset-free / name-based, so it holds on Win32 and Win64 alike.
    /// </summary>
    [Test]
    procedure TestSourceFileAttribution32;
    {$IFDEF CPUX64}
    [Test]
    procedure TestSourceFileAttribution64;
    {$ENDIF}

    /// §6.33 PIN (defects A + B). Managed-reference string fields carry
    /// the offset-independent single-byte primitive id (FAnsi -> $1C,
    /// FWide -> $1E, FShort -> $0C), the relaxed payload guard admits a
    /// scope-byte-$00 field (TPropHost.FBackingStr -> $0401), and the
    /// change leaks onto neither a body=14 UnicodeString nor a non-string
    /// neighbour.
    [Test]
    procedure TestManagedStringFieldIdsOffsetIndependent32;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,

  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.Reader;

function TRsmReaderTests.ResolveExePath(AUse64Bit: Boolean): String;
var
  Sub: String;
begin
  if AUse64Bit then
    Sub := 'Win64'
  else
    Sub := 'Win32';
  Result := ExpandFileName('Projects\DPT\Test\' + Sub + '\DebugTarget.exe');
  if not TFile.Exists(Result) then
    Result := ExpandFileName(Sub + '\DebugTarget.exe');
end;

procedure TRsmReaderTests.TestLoadFromMissingFileLeavesEmpty;
var
  R: TRsmReader;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile('Z:\definitely-does-not-exist\nope.exe');
    Assert.AreEqual<Integer>(0, R.Procs.Count);
    Assert.AreEqual<Integer>(0, R.Classes.Count);
    Assert.AreEqual<Integer>(-1, R.FindProcByName('LocalsProcedure'));
    Assert.AreEqual<UInt32>(0, R.FindGlobalTypeIdx('GGlobalLight'));
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestFormatALinkingPopulatesMemberTypeIds;
var
  R    : TRsmReader;
  I, M : Integer;
  Any  : Boolean;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    Any := False;
    for I := 0 to R.Classes.Count - 1 do
    begin
      if R.Classes[I].Kind <> skClass then Continue;
      for M := 0 to R.Classes[I].Members.Count - 1 do
        if R.Classes[I].Members[M].TypeIdx <> 0 then
        begin
          Any := True;
          Break;
        end;
      if Any then Break;
    end;
    Assert.IsTrue(Any,
      'No class member came through Format-A linking with a non-zero TypeIdx');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestInheritedFieldVisibleViaFindClassMember;
var
  R     : TRsmReader;
  Member: TRsmClassMember;
  Found : Boolean;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    // TDeepDerived inherits TDerived inherits TInner; TInner's
    // FInnerInt should be reachable from TDeepDerived through the
    // parent chain that DeriveClassParents builds.
    Found := R.FindClassMember('TDeepDerived', 'FInnerInt', Member);
    Assert.IsTrue(Found,
      'TDeepDerived.FInnerInt not visible -- ' +
      'class-parent derivation did not link the inheritance chain');
    Assert.AreEqual('FInnerInt', Member.Name);
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestEnumOracleResolvesLightStatus;
var
  R     : TRsmReader;
  TypeId: UInt32;
  Name  : String;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    // GGlobalLight is typed TLightStatus -- its encoded type id must
    // appear in the enum oracle, and ordinal 2 must resolve to lsGreen.
    TypeId := R.FindGlobalTypeIdx('GGlobalLight');
    Assert.IsTrue(TypeId <> 0, 'GGlobalLight type id is zero');
    Assert.IsTrue(R.IsEnumTypeId(TypeId),
      'IsEnumTypeId returned False for TLightStatus');
    Assert.IsTrue(R.TryGetEnumConstantName(TypeId, 2, Name),
      'TryGetEnumConstantName failed for TLightStatus ordinal 2');
    Assert.AreEqual('lsGreen', Name);
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestEnumOracleResolvesCrossUnitThreadPriority;
var
  R     : TRsmReader;
  TypeId: UInt32;
  Name  : String;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    // The primary id for TThreadPriority is discovered through the
    // $2A registry; cross-unit linking joins it to the secondary
    // that carries the $25 $8A-prefix constants. The "tp" prefix
    // hint disambiguates against any aliased secondaries that may
    // share the same id space.
    TypeId := R.FindTypeIdByName('TThreadPriority');
    Assert.IsTrue(TypeId <> 0, 'TThreadPriority not registered by ScanTypeRegistry');
    Assert.IsTrue(R.IsEnumTypeId(TypeId),
      'IsEnumTypeId returned False for TThreadPriority');
    Assert.IsTrue(R.TryGetEnumConstantName(TypeId, 4, Name, 'tp'),
      'TryGetEnumConstantName failed for TThreadPriority ordinal 4 with prefix tp');
    Assert.AreEqual('tpHigher', Name);
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestUnitsDeclaringTypeAggregatesAcrossSegments;
// §4.17 / §6.21 closure pin. UnitsDeclaringType walks every $64
// segment, filters by $66 type-references, and returns a
// deduplicated list of declaring-unit names. Probe with 'Boolean'
// (declared in System.pas) plus a negative probe.
var
  R         : TRsmReader;
  Units     : TArray<String>;
  I, J      : Integer;
  Lower     : String;
  DupCount  : Integer;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));

    // (1) Positive: 'Boolean' must yield at least one declaring unit.
    Units := R.UnitsDeclaringType('Boolean');
    Assert.IsTrue(Length(Units) > 0,
      'UnitsDeclaringType(''Boolean'') returned empty -- either no ' +
      '$66 ''Boolean'' decoded, or the reverse map is broken.');

    // (2) Deduplication: every entry appears exactly once
    // (case-insensitive).
    DupCount := 0;
    for I := 0 to High(Units) do
    begin
      Lower := LowerCase(Units[I]);
      for J := I + 1 to High(Units) do
        if LowerCase(Units[J]) = Lower then
          Inc(DupCount);
    end;
    Assert.AreEqual<Integer>(0, DupCount,
      Format('UnitsDeclaringType returned duplicates (count=%d). The ' +
             'Seen-key dedup short-circuited.', [DupCount]));

    // (3) Every name returned is non-empty (leakage guard against a
    // partially-decoded segment slipping through with an empty
    // UnitName).
    for I := 0 to High(Units) do
      Assert.IsTrue(Units[I] <> '',
        Format('UnitsDeclaringType returned an empty name at index %d.',
               [I]));

    // (4) Negative path: a nonsense type name returns an empty array.
    Units := R.UnitsDeclaringType('TZzNoSuchTypeAnywhere_64217');
    Assert.AreEqual<Integer>(0, Length(Units),
      'UnitsDeclaringType for a nonsense type returned non-empty.');

    // (5) Empty-string guard: returns empty array, no crash.
    Units := R.UnitsDeclaringType('');
    Assert.AreEqual<Integer>(0, Length(Units),
      'UnitsDeclaringType('''') must return empty array.');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestRecordFieldEnumNameConventionDoesNotPhantomBind32;
var
  R     : TRsmReader;
  Member: TRsmClassMember;
  TypeId: UInt32;
  Name  : String;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));

    // No-phantom guard: FMixedInt is a plain Integer field, and there is
    // no TMixedInt / TMixedIntTyp enum, so the §6.24 name-convention pass
    // must leave it non-enum-typed.
    Assert.IsTrue(R.FindClassMember('TMixedRec', 'FMixedInt', Member),
      'TMixedRec.FMixedInt must be discovered as a member.');
    Assert.IsFalse(R.IsEnumTypeId(Member.PrimitiveTypeId),
      'TMixedRec.FMixedInt must NOT be enum-typed -- the §6.24 heuristic ' +
      'must not phantom-bind a field whose name has no matching enum.');
    Assert.AreEqual<UInt32>(0, R.FindTypeIdByName('TMixedInt'),
      'Premise check: there must be no TMixedInt enum for FMixedInt to ' +
      'match -- otherwise this guard is not exercising the no-match path.');

    // Regression guard: extending the FieldAliasEnumBridge with the
    // §6.24 pass must not corrupt the shared scope-local map -- a genuine
    // program-local enum still resolves.
    TypeId := R.FindGlobalTypeIdx('GGlobalLight');
    Assert.IsTrue(R.IsEnumTypeId(TypeId),
      'TLightStatus must still be enum-typed after the §6.24 pass.');
    Assert.IsTrue(R.TryGetEnumConstantName(TypeId, 2, Name) and (Name = 'lsGreen'),
      'TLightStatus ordinal 2 must still resolve to lsGreen.');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestFindBestRecordForGlobalAndField;
var
  R   : TRsmReader;
  Idx : Integer;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    // GGlobalEnumRec : TEnumHostRec, with field FLight. The
    // proximity-based resolver should find TEnumHostRec by walking
    // records whose body carries FLight closest to the global's
    // file offset.
    Idx := R.FindBestRecordForGlobalAndField('GGlobalEnumRec', 'FLight');
    Assert.IsTrue(Idx >= 0, 'No record matched for GGlobalEnumRec.FLight');
    Assert.IsTrue(R.Classes[Idx].Kind = skRecord, 'Matched class is not a record');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestEnumDefsExcludeClassNames;
var
  R          : TRsmReader;
  I, Ci      : Integer;
  HasLight, HasStatus: Boolean;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(R.EnumDefs.Count > 0, 'no EnumDefs from DebugTarget');
    HasLight := False; HasStatus := False;
    for I := 0 to R.EnumDefs.Count - 1 do
    begin
      // Core invariant: no surviving EnumDef shares its name with a real
      // VMT class (skClass). DCE-independent -- holds for whatever set of
      // enums the linker actually emitted.
      Ci := R.FindClassByName(R.EnumDefs[I].TypeName);
      Assert.IsFalse((Ci >= 0) and (R.Classes[Ci].Kind = skClass),
        Format('EnumDef "%s" (%s) is a known class, not an enum -- ' +
          'phantom synthesised def should have been filtered',
          [R.EnumDefs[I].TypeName, R.EnumDefs[I].UnitName]));
      if SameText(R.EnumDefs[I].TypeName, 'TLightStatus') then HasLight := True;
      if SameText(R.EnumDefs[I].TypeName, 'TStatus') then HasStatus := True;
    end;
    // Leakage guards keyed on CONTROLLED fixture enums only. TLightStatus
    // and TStatus are declared in DebugTarget and referenced by evaluated
    // globals (GGlobalLight, GStatusAlpha/Beta/Gamma), so the linker
    // never dead-code-eliminates them -- asserting their presence is
    // DCE-safe. (We deliberately do NOT assert incidental RTL enums like
    // TThreadPriority/TUnicodeBreak: those are only in the .rsm because a
    // System unit happens to be linked, so a future build could omit them
    // and the assertion would fail with no real regression.) TStatus is
    // also the enum the StructDiscoverer/property linker register as
    // skRecord under the same name, so its survival here is exactly what
    // proves the filter keys on skClass only, not skRecord; the live
    // cross-unit MCP tests (GStatusAlpha/GStatusBeta) are the primary
    // guard for that.
    Assert.IsTrue(HasLight, 'legit TLightStatus dropped by the class filter');
    Assert.IsTrue(HasStatus, 'legit same-comp TStatus dropped by the class filter');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.DoTestSourceFileAttribution(AUse64Bit: Boolean);
var
  R        : TRsmReader;
  Seen     : IKeyValue<String, Boolean>;
  I        : Integer;
  SF       : TRsmSourceFile;
  Seg      : TRsmUnitUseSegment;
  ImageHlpIdx: Integer;
  UsesEdgeFound: Boolean;
  Declarers, Importers: TArray<String>;
  Plat     : String;
  LowerName: String;
begin
  if AUse64Bit then Plat := 'Win64' else Plat := 'Win32';
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(AUse64Bit));
    Assert.IsTrue(R.SourceFiles.Count > 0,
      'No $70 SourceFiles decoded (' + Plat + ').');
    Assert.IsTrue(R.UnitUseSegments.Count > 0,
      'No $64 segments decoded (' + Plat + ').');

    // (a) SourceFiles deduped (stored once) + StripDirAndExt applied +
    //     leakage guard: no lone 'System' introducer.
    Seen := Collections.NewPlainKeyValue<String, Boolean>;
    ImageHlpIdx := -1;
    for I := 0 to R.SourceFiles.Count - 1 do
    begin
      SF := R.SourceFiles[I];
      LowerName := LowerCase(SF.UnitName);
      Assert.IsFalse(Seen.ContainsKey(LowerName),
        'SourceFiles must be deduped -- duplicate ' + SF.UnitName +
        ' (' + Plat + ').');
      Seen[LowerName] := True;
      Assert.IsFalse(SameText(SF.UnitName, 'System'),
        'The lone System.pas (no $64 follows) must NOT be recorded as a ' +
        'SourceFile introducer (' + Plat + ').');
      // UnitName must have the .pas/.inc stripped; SourceFile retains it.
      Assert.IsFalse(SF.UnitName.ToLower.EndsWith('.pas'),
        'UnitName must not keep the .pas extension: ' + SF.UnitName);
      if SameText(SF.UnitName, 'Winapi.ImageHlp') then
      begin
        ImageHlpIdx := I;
        Assert.IsTrue(SameText(SF.SourceFile, 'Winapi.ImageHlp.pas'),
          'Winapi.ImageHlp SourceFile must be Winapi.ImageHlp.pas, got: ' +
          SF.SourceFile);
      end;
    end;
    Assert.IsTrue(ImageHlpIdx >= 0,
      'Winapi.ImageHlp $70 introducer must be decoded into SourceFiles (' +
      Plat + ').');

    // (b) the uses-relationship Winapi.ImageHlp -> Winapi.Windows must be
    //     attributable, with FK integrity; AND (c) no orphan segments.
    UsesEdgeFound := False;
    for I := 0 to R.UnitUseSegments.Count - 1 do
    begin
      Seg := R.UnitUseSegments[I];
      Assert.IsTrue(Seg.SourceFileIdx >= 0,
        Format('Segment @%d (declares %s) is orphaned -- every segment must ' +
          'be attributed to a $70 introducer (' + Plat + ').',
          [Seg.StartOffset, Seg.UnitName]));
      Assert.IsTrue(Seg.SourceFileIdx < R.SourceFiles.Count,
        'SourceFileIdx out of range (' + Plat + ').');
      if SameText(R.SourceFiles[Seg.SourceFileIdx].UnitName, 'Winapi.ImageHlp')
         and SameText(Seg.UnitName, 'Winapi.Windows') then
      begin
        UsesEdgeFound := True;
        Assert.AreEqual(ImageHlpIdx, Seg.SourceFileIdx,
          'FK integrity: the Winapi.ImageHlp-imported segment must point at ' +
          'the Winapi.ImageHlp SourceFiles entry (' + Plat + ').');
      end;
    end;
    Assert.IsTrue(UsesEdgeFound,
      'The uses-edge Winapi.ImageHlp -> Winapi.Windows must be recoverable ' +
      'from the $70/$64 attribution (' + Plat + ').');

    // (d) UnitsImporting is the exact inverse of UnitsDeclaringType.
    Declarers := R.UnitsDeclaringType('Boolean');
    Importers := R.UnitsImporting('Boolean');
    Assert.IsTrue(Length(Declarers) > 0,
      'Boolean must have at least one declaring unit (' + Plat + ').');
    Assert.IsTrue(Length(Importers) > 1,
      'Boolean must have many importing units -- UnitsImporting returns the ' +
      'importer side, the inverse of declarers (' + Plat + ').');
    Assert.IsTrue(Length(Importers) > Length(Declarers),
      'Boolean''s importer set must be larger than its declarer set -- ' +
      'confirms the two queries are genuine inverses (' + Plat + ').');
    // Negative probes.
    Assert.AreEqual<Integer>(0, Length(R.UnitsImporting('TZzNoSuchType_4711')),
      'UnitsImporting for a nonsense type must be empty (' + Plat + ').');
    Assert.AreEqual<Integer>(0, Length(R.UnitsImporting('')),
      'UnitsImporting('''') must be empty (' + Plat + ').');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestSourceFileAttribution32;
begin
  DoTestSourceFileAttribution(False);
end;

{$IFDEF CPUX64}
procedure TRsmReaderTests.TestSourceFileAttribution64;
begin
  DoTestSourceFileAttribution(True);
end;
{$ENDIF}

procedure TRsmReaderTests.TestManagedStringFieldIdsOffsetIndependent32;
var
  R   : TRsmReader;
  Mem : TRsmClassMember;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    // §6.33 defect B: managed-reference string fields (body=9, "$9C $01"
    // marker at +5) carry the offset-INDEPENDENT single-byte primitive id
    // read from After+3 alone. The old "After+3 | (2*offset) shl 8" read
    // baked the field's instance offset into the id (After+4 = 2x offset),
    // so the same managed type at a different offset produced a different,
    // unmapped id -- the source of the §6.33 bare-evaluate failure.
    Assert.IsTrue(R.FindClassMember('TPrimitives', 'FAnsi', Mem), 'FAnsi resolves');
    Assert.AreEqual<UInt16>($1C, Mem.PrimitiveTypeId,
      'FAnsi@0 -> ansistring $1C (offset-independent; was the baked $001C)');
    Assert.IsTrue(R.FindClassMember('TPrimitives', 'FWide', Mem), 'FWide resolves');
    Assert.AreEqual<UInt16>($1E, Mem.PrimitiveTypeId,
      'FWide@4 -> widestring $1E (was the offset-baked $081E)');
    Assert.IsTrue(R.FindClassMember('TPrimitives', 'FShort', Mem), 'FShort resolves');
    Assert.AreEqual<UInt16>($0C, Mem.PrimitiveTypeId,
      'FShort@8 -> shortstring $0C (was the offset-baked $100C)');

    // §6.33 defect A: the relaxed payload guard ("$00 <scope> $00", not
    // the fixed "$00 $02 $00") admits a field record whose scope byte is
    // not $02. TPropHost.FBackingStr's record carries scope byte $00 and
    // was previously dropped entirely, leaving PrimitiveTypeId=0; it now
    // links to UnicodeString $0401.
    Assert.IsTrue(R.FindClassMember('TPropHost', 'FBackingStr', Mem),
      'FBackingStr resolves');
    Assert.AreEqual<UInt16>($0401, Mem.PrimitiveTypeId,
      'FBackingStr -> UnicodeString $0401 (now admitted by the relaxed guard)');

    // Leakage guard 1: a body=14 UnicodeString field is unaffected by the
    // body=9 single-byte change -- it still resolves to the 2-byte $0401,
    // not a single-byte managed id.
    Assert.IsTrue(R.FindClassMember('TInner', 'FInnerStr', Mem), 'FInnerStr resolves');
    Assert.AreEqual<UInt16>($0401, Mem.PrimitiveTypeId,
      'FInnerStr stays UnicodeString $0401 (body=14, not a managed single-byte id)');

    // Leakage guard 2: the widened guard / body=9 hoist must not stamp a
    // managed-string id onto a non-string neighbour. FMixedInt (plain
    // Integer) must keep a non-zero, non-managed-string primitive id.
    Assert.IsTrue(R.FindClassMember('TMixedRec', 'FMixedInt', Mem), 'FMixedInt resolves');
    Assert.AreNotEqual<UInt16>(0, Mem.PrimitiveTypeId,
      'FMixedInt keeps a primitive id');
    Assert.IsFalse(
      (Mem.PrimitiveTypeId = $04) or (Mem.PrimitiveTypeId = $0C) or
      (Mem.PrimitiveTypeId = $1C) or (Mem.PrimitiveTypeId = $1E),
      'FMixedInt (Integer) must not be mis-stamped with a managed-string id');
  finally
    R.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRsmReaderTests);

end.
