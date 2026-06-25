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

    /// <summary>
    ///   §4.17 LinkToken-semantics pin (the Step-1 investigation outcome).
    ///   Locks WHY a general token-keyed declaration lookup is NOT a useful
    ///   reader primitive, via four relationship assertions on
    ///   DebugTarget.rsm (no absolute token values — those are link-stamp
    ///   derived and drift per build):
    ///   (1) a <c>$66 'Boolean'</c> use-site token is byte-identical to the
    ///       canonical <c>$2A 'Boolean'</c> body+3..+6 token;
    ///   (2) that token's LOW WORD equals the <c>$2A</c> registry primary id
    ///       — so for types the 32-bit token is the primary id plus 16
    ///       redundant bits (a token lookup ≡ the existing primary lookup);
    ///   (3) a type ALIAS use-site resolves to the UNDERLYING type's token:
    ///       <c>$66 'DWORD'</c> carries the same token as <c>$66 'Cardinal'</c>;
    ///   (4) and that differs from the alias's OWN <c>$2A 'DWORD'</c> token —
    ///       so a name-keyed token lookup would mis-navigate aliases.
    ///   Together these refute the "collision-free declaration key" premise
    ///   for the type/const families; only the same-enum sibling case
    ///   (pinned in Test.DPT.Rsm.Taifun) is clean.
    /// </summary>
    [Test]
    procedure TestLinkTokenSemanticsNotAUsefulLookupKey32;

    /// <summary>
    ///   §6.36 closure pin (reader side). The cross-unit record local
    ///   <c>AdrLoc: DebugTarget.RecTypes.TXAdresse</c> in
    ///   <c>RecordLocalNestedProbe</c> carries a 2-byte linker-minted
    ///   per-proc type alias (Hi byte not $2E/$2F), which pushed its
    ///   BPREL offset one byte later than the classic 1-byte-type form;
    ///   the scanner used to leave the synthesized -10000 sentinel.
    ///   After the §6.36 offset-side decode it resolves to the real
    ///   frame offset -153 (the 153-byte record at the frame bottom).
    ///   We also pin that both cross-unit records reach FClasses with
    ///   their correct field layout -- the record-context the dotted
    ///   walk's name/size bridges navigate (discovery was never the
    ///   problem; the offset decode + walk priming were).
    /// </summary>
    [Test]
    procedure TestCrossUnitRecordLocalOffsetAndLayout32;

    /// <summary>
    ///   Reader-level pin for the cross-unit complex-record fixture
    ///   (TComplexRec / PComplexRec in DebugTarget.ComplexRec), covering
    ///   two decode fixes:
    ///   <list type="number">
    ///     <item>Nested-record layout: TComplexRec's CxR2 SLOT is 12
    ///       bytes (the parent pads to 8-align the Int64-bearing CxR3)
    ///       while TCxRec2's own extent is only 8 -- the alignment slack
    ///       that the §4.14 nested-record bridge must tolerate
    ///       (FindRecordBySizeAndMemberName: Total &lt;= slot, slack &lt; 8).
    ///       Leakage guard: a slot size with &gt;= 8 slack must NOT match.</item>
    ///     <item>§4.4 Shape-A offset decode: the cross-unit POINTER local
    ///       <c>CxPtr: PComplexRec</c> carries a 2-byte alias whose Hi
    ///       byte is $1E; its BPRel offset must decode to -4 (the pointer
    ///       sits just below EBP), NOT the -504 that Shape B produced
    ///       before $1E was admitted as a 2-byte-type marker. CxLoc (the
    ///       inline record) stays at -80.</item>
    ///   </list>
    /// </summary>
    [Test]
    procedure TestComplexRecLayoutAndPointerLocalOffset32;

    // §6.36-A static interface route: a user interface (IDbgRecoverable,
    // declared in DebugTarget.IfaceProbe) is discovered as an skInterface
    // struct synthesized from its $2A registry entry (no VMT trailer / no
    // $0E sentinel for the struct discoverer to find), admitted to the
    // type-id registry, and round-trips by id. Leakage guards: a real
    // record stays skRecord and a real class stays skClass.
    procedure DoTestInterfaceDiscoveredAsStruct(AUse64Bit: Boolean);
    [Test]
    procedure TestInterfaceDiscoveredAsStruct32;
    {$IFDEF CPUX64}
    [Test]
    procedure TestInterfaceDiscoveredAsStruct64;
    {$ENDIF}
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,

  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.Scanner,
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

procedure TRsmReaderTests.TestLinkTokenSemanticsNotAUsefulLookupKey32;
var
  R   : TRsmReader;
  Sc  : TRsmScanner;
  BoolUse, DwordUse, CardUse: UInt32;
  BoolCanon, BoolPrim, DwordCanon, DwordPrim: UInt32;
  HaveBoolUse, HaveDwordUse, HaveCardUse: Boolean;
  HaveBoolCanon, HaveDwordCanon: Boolean;

  // The $66 use-site tokens are already decoded into UnitUseSegments.
  function UseTokenOfType(const AName: String; out AToken: UInt32): Boolean;
  var
    I, J: Integer;
    Seg : TRsmUnitUseSegment;
    Ref : TRsmUnitUseRef;
  begin
    Result := False;
    for I := 0 to R.UnitUseSegments.Count - 1 do
    begin
      Seg := R.UnitUseSegments[I];
      if Seg.Refs = nil then Continue;
      for J := 0 to Seg.Refs.Count - 1 do
      begin
        Ref := Seg.Refs[J];
        if (Ref.Kind = uukType) and SameText(Ref.Name, AName) then
        begin
          AToken := Ref.LinkToken;
          Exit(True);
        end;
      end;
    end;
  end;

  // The canonical $2A token is NOT stored on any model record (that is
  // the §4.17 finding -- it would be redundant), so read it raw. Body =
  // P+2+NL; $00 $00 at +1/+2; 4-byte token at +3..+6, low word = primary.
  function Canon2A(const AName: String; out AToken, APrimary: UInt32): Boolean;
  var
    P  : NativeInt;
    NL : Integer;
    Nm : String;
    K  : Integer;
    B  : Byte;
  begin
    Result := False;
    P := 0;
    while P + 12 < Sc.Sz do
    begin
      if Sc.ByteAt(P) = $2A then
      begin
        NL := Sc.ByteAt(P + 1);
        if (NL >= 2) and (NL <= 40) and (P + 2 + NL + 5 < Sc.Sz) and
           (Sc.ByteAt(P + 2 + NL + 1) = 0) and (Sc.ByteAt(P + 2 + NL + 2) = 0) then
        begin
          Nm := '';
          for K := 0 to NL - 1 do
          begin
            B := Sc.ByteAt(P + 2 + K);
            if (B >= 32) and (B < 127) then Nm := Nm + Chr(B) else begin Nm := ''; Break; end;
          end;
          if SameText(Nm, AName) then
          begin
            APrimary := UInt32(Sc.ByteAt(P + 2 + NL + 3)) or
                        (UInt32(Sc.ByteAt(P + 2 + NL + 4)) shl 8);
            AToken := APrimary or
                      (UInt32(Sc.ByteAt(P + 2 + NL + 5)) shl 16) or
                      (UInt32(Sc.ByteAt(P + 2 + NL + 6)) shl 24);
            Exit(True);
          end;
        end;
      end;
      Inc(P);
    end;
  end;

begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    Sc := R.Scanner;
    Assert.IsTrue(R.UnitUseSegments.Count > 0,
      'No $64 segments decoded -- fixture/scanner regression, not a token finding');

    HaveBoolUse   := UseTokenOfType('Boolean', BoolUse);
    HaveDwordUse  := UseTokenOfType('DWORD', DwordUse);
    HaveCardUse   := UseTokenOfType('Cardinal', CardUse);
    HaveBoolCanon := Canon2A('Boolean', BoolCanon, BoolPrim);
    HaveDwordCanon:= Canon2A('DWORD', DwordCanon, DwordPrim);

    Assert.IsTrue(HaveBoolUse and HaveBoolCanon,
      'Boolean must appear both as a $66 use-site and a $2A registry entry');

    // (1) byte-identity: $66 use-site token == $2A body+3..+6 token.
    Assert.AreEqual<UInt32>(BoolCanon, BoolUse,
      '$66 Boolean use-site token must equal the canonical $2A token');
    // (2) the token's low word IS the registry primary id -> a type-side
    // token lookup is the existing primary lookup plus 16 redundant bits.
    Assert.AreEqual<UInt32>(BoolPrim, BoolCanon and UInt32($FFFF),
      'canonical token low word must equal the $2A registry primary id');

    // (3)+(4) the alias finding: only assert when the fixture carries the
    // RTL alias use-sites (it does on the current build; guard so a future
    // RTL reshuffle degrades gracefully rather than red).
    if HaveDwordUse and HaveCardUse then
      Assert.AreEqual<UInt32>(CardUse, DwordUse,
        '$66 DWORD use-site must resolve to the UNDERLYING Cardinal token ' +
        '(alias resolution) -- a token lookup would mis-navigate the alias');
    if HaveDwordUse and HaveDwordCanon then
      Assert.AreNotEqual<UInt32>(DwordUse, DwordCanon,
        'the alias''s own $2A DWORD token must differ from the use-site ' +
        'token -- confirming name-keyed token lookup is ambiguous for aliases');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestComplexRecLayoutAndPointerLocalOffset32;

  function MemberByName(const AInfo: TRsmClassInfo;
    const AName: String): TRsmClassMember;
  var
    M: Integer;
  begin
    Result := Default(TRsmClassMember);
    for M := 0 to AInfo.Members.Count - 1 do
      if SameText(AInfo.Members[M].Name, AName) then
        Exit(AInfo.Members[M]);
  end;

  function LocalByName(const AInfo: TRsmProc; const AName: String): TRsmLocal;
  var
    L: Integer;
  begin
    Result := Default(TRsmLocal);
    for L := 0 to AInfo.Locals.Count - 1 do
      if SameText(AInfo.Locals[L].Name, AName) then
        Exit(AInfo.Locals[L]);
  end;

var
  R       : TRsmReader;
  Ci, Pi  : Integer;
  Cx      : TRsmClassInfo;
  Prc     : TRsmProc;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));

    // (1) Aggregate layout. CxR2's SLOT is 12 (TCxRec2 is 8 bytes, the
    // parent inserts 4 alignment-padding bytes so the Int64-bearing CxR3
    // starts 8-aligned at offset 16). This is the slack the nested-record
    // bridge must tolerate.
    Ci := R.FindClassByName('TComplexRec');
    Assert.IsTrue(Ci >= 0, 'TComplexRec record not discovered');
    Cx := R.Classes[Ci];
    Assert.AreEqual<Integer>(Ord(skRecord), Ord(Cx.Kind), 'TComplexRec must be a record');
    Assert.AreEqual<UInt32>(0,  MemberByName(Cx, 'CxR1').Offset, 'CxR1 offset');
    Assert.AreEqual<UInt32>(4,  MemberByName(Cx, 'CxR2').Offset, 'CxR2 offset');
    Assert.AreEqual<UInt32>(12, MemberByName(Cx, 'CxR2').Size,
      'CxR2 SLOT size must be 12 (TCxRec2 is 8 + 4 parent alignment padding)');
    Assert.AreEqual<UInt32>(16, MemberByName(Cx, 'CxR3').Offset, 'CxR3 offset (8-aligned)');
    Assert.AreEqual<UInt32>(32, MemberByName(Cx, 'CxTag').Offset, 'CxTag offset');

    // (2) Nested-record bridge tolerates the alignment slack: the 12-byte
    // slot resolves to TCxRec2 via its member name.
    Assert.AreEqual<Integer>(R.FindClassByName('TCxRec2'),
      R.FindRecordBySizeAndMemberName(12, 'C2Int'),
      'CxR2 slot (12) must bridge to TCxRec2 despite the 4-byte alignment slack');
    // Leakage guard: TCxRec2's extent is 8; a slot whose slack would be
    // >= 8 (e.g. 16) must NOT match it -- the padding tolerance is bounded.
    Assert.AreEqual<Integer>(-1,
      R.FindRecordBySizeAndMemberName(16, 'C2Int'),
      'A 16-byte slot must NOT match the 8-byte TCxRec2 (slack >= 8 is not padding)');

    // (3) §4.4 Shape-A offset decode for the cross-unit POINTER local.
    Pi := R.FindProcByName('ComplexRecLocalProbe');
    Assert.IsTrue(Pi >= 0, 'ComplexRecLocalProbe proc not found');
    Prc := R.Procs[Pi];
    Assert.AreEqual<Int32>(-80, LocalByName(Prc, 'CxLoc').BpOffset,
      'CxLoc (inline record) must decode to BPRel -80');
    Assert.AreEqual<Int32>(-4, LocalByName(Prc, 'CxPtr').BpOffset,
      'CxPtr (PComplexRec, $1E-Hi 2-byte alias) must decode to BPRel -4; ' +
      'a value like -504 means Shape B read the alias Hi byte as the offset');
    // Both carry a 2-byte $1E-Hi cross-unit alias id (not a registry id).
    Assert.AreEqual<UInt32>($1E, LocalByName(Prc, 'CxPtr').TypeIdx shr 8,
      'CxPtr type id must be a 2-byte $1E-Hi cross-unit alias');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestCrossUnitRecordLocalOffsetAndLayout32;

  function MemberByName(const AInfo: TRsmClassInfo;
    const AName: String): TRsmClassMember;
  var
    M: Integer;
  begin
    Result := Default(TRsmClassMember);
    for M := 0 to AInfo.Members.Count - 1 do
      if SameText(AInfo.Members[M].Name, AName) then
        Exit(AInfo.Members[M]);
  end;

var
  R     : TRsmReader;
  Pi, Ci: Integer;
  Adr   : TRsmClassInfo;
  Ansch : TRsmClassInfo;
  Found : Boolean;
  Local : TRsmLocal;
  L     : Integer;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));

    // (1) §6.36 offset-side decode: AdrLoc's BPREL offset resolves to
    // the real frame slot, not the synthesized "couldn't decode" sentinel.
    Pi := R.FindProcByName('RecordLocalNestedProbe');
    Assert.IsTrue(Pi >= 0, 'RecordLocalNestedProbe proc not found');
    Found := False;
    for L := 0 to R.Procs[Pi].Locals.Count - 1 do
      if SameText(R.Procs[Pi].Locals[L].Name, 'AdrLoc') then
      begin
        Local := R.Procs[Pi].Locals[L];
        Found := True;
        Break;
      end;
    Assert.IsTrue(Found, 'AdrLoc local not found in RecordLocalNestedProbe');
    Assert.AreEqual<Integer>(Ord(lkBpRel), Ord(Local.Kind),
      'AdrLoc must be a BP-relative stack local');
    Assert.AreEqual<Int32>(-153, Local.BpOffset,
      'AdrLoc BPREL offset must decode to -153 (the 153-byte record at ' +
      'the frame bottom); a value <= -10000 means the §6.36 2-byte ' +
      'cross-unit-type offset shape was not decoded');

    // (2) Both cross-unit records reach FClasses with correct layout.
    Ci := R.FindClassByName('TXAdresse');
    Assert.IsTrue(Ci >= 0, 'TXAdresse record not discovered');
    Adr := R.Classes[Ci];
    Assert.AreEqual<Integer>(Ord(skRecord), Ord(Adr.Kind), 'TXAdresse must be a record');
    Assert.AreEqual<UInt32>(0,  MemberByName(Adr, 'Name').Offset,      'TXAdresse.Name offset');
    Assert.AreEqual<UInt32>(41, MemberByName(Adr, 'Anschrift').Offset, 'TXAdresse.Anschrift offset');
    // The nested-record member's Size == TXAnschrift's total byte size
    // (61 + 51): the structural key the dotted-walk nested-record bridge
    // (FindRecordBySizeAndMemberName) matches on.
    Assert.AreEqual<UInt32>(112, MemberByName(Adr, 'Anschrift').Size,
      'TXAdresse.Anschrift size must equal TXAnschrift total size (112)');

    Ci := R.FindClassByName('TXAnschrift');
    Assert.IsTrue(Ci >= 0, 'TXAnschrift record not discovered');
    Ansch := R.Classes[Ci];
    Assert.AreEqual<Integer>(Ord(skRecord), Ord(Ansch.Kind), 'TXAnschrift must be a record');
    Assert.AreEqual<UInt32>(0,  MemberByName(Ansch, 'Str').Offset, 'TXAnschrift.Str offset');
    Assert.AreEqual<UInt32>(61, MemberByName(Ansch, 'Ort').Offset, 'TXAnschrift.Ort offset');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.DoTestInterfaceDiscoveredAsStruct(AUse64Bit: Boolean);
var
  R       : TRsmReader;
  Ci, Rt  : Integer;
  Info    : TRsmClassInfo;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(AUse64Bit));

    // (1) The user interface IDbgRecoverable (declared in
    // DebugTarget.IfaceProbe) is discovered. The struct discoverer can't
    // find it -- it has no VMT class-trailer and no $0E record sentinel;
    // the only place its name + 2-byte id co-occur is the $2A registry,
    // from which ScanTypeRegistry now synthesizes an skInterface entry.
    Ci := R.FindClassByName('IDbgRecoverable');
    Assert.IsTrue(Ci >= 0,
      'IDbgRecoverable not discovered as a struct (interface synthesis missing)');
    Info := R.Classes[Ci];
    Assert.AreEqual<Integer>(Ord(skInterface), Ord(Info.Kind),
      'IDbgRecoverable must be skInterface');
    // It is NOT a record -- so the dotted walk can never record-hop into it
    // (the §6.36 mis-typing hazard) -- and NOT a class.
    Assert.AreNotEqual<Integer>(Ord(skRecord), Ord(Info.Kind),
      'an interface must not be classified skRecord');
    Assert.AreNotEqual<Integer>(Ord(skClass), Ord(Info.Kind),
      'an interface must not be classified skClass');

    // (2) Non-nil empty Members list (ClassParentDeriver /
    // FieldAliasEnumBridge read Members.Count unconditionally -- a nil list
    // would crash those passes).
    Assert.IsTrue(Info.Members <> nil, 'interface Members list must be non-nil');
    Assert.AreEqual<Integer>(0, Info.Members.Count,
      'synthesized interface carries no members');

    // (3) Admitted to the type-id registry and round-trips by id. The raw
    // id is per-build / per-platform (Win32 $4712 vs Win64 $0312), so we
    // assert the round-trip RELATIONSHIP, never the literal id.
    Assert.AreNotEqual<UInt32>(0, Info.TypeIdx, 'interface TypeIdx must be non-zero');
    Rt := R.FindClassIdxByRsmTypeId(Info.TypeIdx);
    Assert.AreEqual<Integer>(Ci, Rt,
      'IDbgRecoverable must round-trip: FindClassIdxByRsmTypeId(TypeIdx) = its index');

    // (4) Leakage guards -- interface admission must not reclassify real
    // types. A known record stays skRecord; a known class stays skClass.
    Ci := R.FindClassByName('TMixedRec');
    Assert.IsTrue(Ci >= 0, 'TMixedRec record not discovered (fixture regression)');
    Assert.AreEqual<Integer>(Ord(skRecord), Ord(R.Classes[Ci].Kind),
      'TMixedRec must remain a record');
    Ci := R.FindClassByName('TDerived');
    Assert.IsTrue(Ci >= 0, 'TDerived class not discovered (fixture regression)');
    Assert.AreEqual<Integer>(Ord(skClass), Ord(R.Classes[Ci].Kind),
      'TDerived must remain a class');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestInterfaceDiscoveredAsStruct32;
begin
  DoTestInterfaceDiscoveredAsStruct(False);
end;

{$IFDEF CPUX64}
procedure TRsmReaderTests.TestInterfaceDiscoveredAsStruct64;
begin
  DoTestInterfaceDiscoveredAsStruct(True);
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TRsmReaderTests);

end.
