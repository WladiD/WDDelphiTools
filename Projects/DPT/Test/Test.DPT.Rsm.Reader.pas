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

    /// Record-typed global proximity resolution. GGlobalEnumRec is a
    /// record-typed module-level global; the dotted-walk uses
    /// FindBestRecordForGlobalAndField to recover its type from
    /// byte-stream proximity. The result must be a skRecord index.
    [Test]
    procedure TestFindBestRecordForGlobalAndField;

    /// §6.20 Stage-1 diagnostic: dumps the raw Format-B field-record
    /// bytes around each field of a chosen probe record. The current
    /// scanner reads only NAME and OFFSET and treats the 6-byte
    /// `$02 $00 <last-flag> $00 $00 $00` typeinfo as a structural
    /// anchor only. This dump captures the WIDER context (~64 bytes
    /// around each field's anchor) so we can spot whether a type id
    /// of TLightStatus / Integer / String shows up at a stable offset
    /// — that's the encoding §6.20 wants to decode so the auto-detect
    /// path can pick formatters for record-host-no-$2C fields like
    /// TFW's TAd.Land. Probes records with KNOWN field types so the
    /// expected type-id bytes are deterministic (TLightStatus's $2A
    /// id, Integer = $03FD, etc.).
    /// Writes a TSV next to the binary; the test always passes — the
    /// artefact is the file.
    [Test]
    procedure TestDiagnoseRecordFieldTypeBytes;

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
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,

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

procedure TRsmReaderTests.TestDiagnoseRecordFieldTypeBytes;
// §6.20 Stage-1 diagnostic. Walks selected records' Format-B field
// streams, dumps wide byte context around each field anchor, and
// emits a TSV. The current scanner uses bytes
// `$02 $00 <last-flag> $00 $00 $00` as a STRUCTURAL anchor only and
// ignores everything after; we want to see whether a 2-byte type id
// (TLightStatus's $2A alias, Integer = $03FD, AnsiString = $0407,
// etc.) actually appears at a consistent offset after the anchor.
// If it does, §6.20 closes by teaching ScanFieldsForwardFromRecordName
// to read it and populate Member.PrimitiveTypeId / Member.TypeIdx.
// Probed records cover the type spectrum: TMixedRec (Integer/Int64/
// string), TNarrowInts (Word/Byte), TEnumHostRec (enum FLight:
// TLightStatus), TAmbig619Target (Integer fields under the §6.19
// fixture).
const
  CtxBefore = 4;
  CtxAfter  = 48;
  MaxFields = 16;
  ScanWin   = 4096;
var
  R         : TRsmReader;
  Lines     : TStringList;
  Probes    : TArray<String>;
  Pi        : Integer;
  Buf       : PByte;
  Sz        : NativeInt;
  TsvPath   : String;
  ExePath   : String;
  IdLandTyp : UInt32;
  IdLight   : UInt32;
  IdMixedRec: UInt32;
  IdAmbig   : UInt32;

  procedure DumpProbe(const AName: String);
  var
    ClsIdx    : Integer;
    AnchorOff : NativeInt;
    NameStart : NativeInt;
    HeaderEnd : NativeInt;
    NameLen   : Byte;
    RecSize   : UInt32;
    P, FieldP : NativeInt;
    FieldNL   : Byte;
    FieldName : String;
    HexRow    : String;
    AsciiRow  : String;
    K, J      : Integer;
    BB        : Byte;
    FieldCount: Integer;
  begin
    Lines.Add(Format('-- record %s --', [AName]));
    ClsIdx := R.FindClassByName(AName);
    if ClsIdx < 0 then
    begin
      Lines.Add('   NOT IN FClasses');
      Lines.Add('');
      Exit;
    end;
    AnchorOff := NativeInt(R.Classes[ClsIdx].TypeIdx);
    Lines.Add(Format('   TypeIdx=$%x', [AnchorOff]));
    NameStart := AnchorOff;
    if NameStart >= Sz then Exit;
    NameLen := Buf[NameStart];
    HeaderEnd := NameStart + 1 + NameLen + 4;
    if HeaderEnd >= Sz then Exit;
    RecSize := UInt32(Buf[NameStart + 1 + NameLen]) or
               (UInt32(Buf[NameStart + 1 + NameLen + 1]) shl 8) or
               (UInt32(Buf[NameStart + 1 + NameLen + 2]) shl 16) or
               (UInt32(Buf[NameStart + 1 + NameLen + 3]) shl 24);
    Lines.Add(Format('   RecordSize=$%x', [RecSize]));

    // Dump the header region: from RecordSize DWORD end up to the
    // first $02 field marker. Per §4.13 (simple-shape header) this
    // is 17 + N*8 bytes on Win32 where N is the managed-field count.
    // If the per-field type ids live anywhere, it's here.
    HexRow := '';
    AsciiRow := '';
    K := 0;
    while (HeaderEnd + K < Sz) and (K < ScanWin) do
    begin
      BB := Buf[HeaderEnd + K];
      // Stop one byte before the first $02 with valid field-record
      // structure (so we capture exactly the header).
      if (BB = $02) and (HeaderEnd + K + 8 < Sz) then
      begin
        var Nl2: Byte := Buf[HeaderEnd + K + 1];
        if (Nl2 >= 1) and (Nl2 <= 40) and
           (HeaderEnd + K + 2 + Nl2 + 5 < Sz) and
           (Buf[HeaderEnd + K + 2 + Nl2] = $02) and
           (Buf[HeaderEnd + K + 2 + Nl2 + 1] = $00) then Break;
      end;
      HexRow := HexRow + IntToHex(BB, 2) + ' ';
      if (BB >= 32) and (BB < 127) then
        AsciiRow := AsciiRow + Char(BB)
      else
        AsciiRow := AsciiRow + '.';
      Inc(K);
    end;
    Lines.Add(Format('   header(%d bytes after size DWORD): %s | %s',
      [K, HexRow, AsciiRow]));

    P := HeaderEnd;
    FieldCount := 0;
    while (P < HeaderEnd + ScanWin) and (P + 64 < Sz) and
          (FieldCount < MaxFields) do
    begin
      if Buf[P] = $02 then
      begin
        FieldNL := Buf[P + 1];
        if (FieldNL >= 1) and (FieldNL <= 40) and
           (P + 2 + FieldNL + 6 < Sz) and
           (Buf[P + 2 + FieldNL] = $02) and
           (Buf[P + 2 + FieldNL + 1] = $00) and
           ((Buf[P + 2 + FieldNL + 2] = $00) or
            (Buf[P + 2 + FieldNL + 2] = $02)) then
        begin
          SetString(FieldName, PAnsiChar(@Buf[P + 2]), FieldNL);
          FieldP := P;
          HexRow := '';
          AsciiRow := '';
          for K := -CtxBefore to CtxAfter do
          begin
            J := FieldP + K;
            if (J < 0) or (J >= Sz) then Continue;
            BB := Buf[J];
            HexRow := HexRow + IntToHex(BB, 2) + ' ';
            if (BB >= 32) and (BB < 127) then
              AsciiRow := AsciiRow + Char(BB)
            else
              AsciiRow := AsciiRow + '.';
          end;
          Lines.Add(Format('   [%d] off=$%x name=%-18s | %s | %s',
            [FieldCount, FieldP, FieldName, HexRow, AsciiRow]));
          Inc(FieldCount);
          if Buf[FieldP + 2 + FieldNL + 2] = $02 then Break;
          P := P + 2 + FieldNL + 6;
          Continue;
        end;
      end;
      Inc(P);
    end;
    Lines.Add('');
  end;

begin
  R := TRsmReader.Create;
  try
    ExePath := ResolveExePath(False);
    R.LoadFromFile(ExePath);
    Buf := R.Scanner.Buf;
    Sz  := R.Scanner.Sz;
    if (Buf = nil) or (Sz < 8) then
    begin
      Assert.Pass('No RSM loaded -- nothing to dump');
      Exit;
    end;

    IdLight    := R.FindTypeIdByName('TLightStatus');
    IdLandTyp  := R.FindTypeIdByName('TLandTyp');
    IdMixedRec := R.FindTypeIdByName('TMixedRec');
    IdAmbig    := R.FindTypeIdByName('TAmbig619Target');

    TsvPath := ExtractFilePath(ExePath) + 'rsm-6.20-record-field-bytes.tsv';
    Lines := TStringList.Create;
    try
      Lines.Add('6.20 diagnostic: Format-B record-field byte context.');
      Lines.Add(Format('  TLightStatus    alias id = $%x  (enum -- expect at FLight context)', [IdLight]));
      Lines.Add(Format('  TMixedRec       alias id = $%x  (control: record host id)', [IdMixedRec]));
      Lines.Add(Format('  TAmbig619Target alias id = $%x  (control: 6.19 record host id)', [IdAmbig]));
      Lines.Add(Format('  TLandTyp        alias id = $%x  (TFW-only -- 0 expected on DebugTarget)', [IdLandTyp]));
      Lines.Add('Known primitive ids: Integer=$3FD String=$407');
      Lines.Add(Format('Layout: %d bytes before + %d bytes after each field-name anchor.',
        [CtxBefore, CtxAfter]));
      Lines.Add('');

      Probes := TArray<String>.Create('TMixedRec', 'TNarrowInts',
        'TEnumHostRec', 'TAmbig619Target', 'TPoint3D');
      for Pi := 0 to High(Probes) do
        DumpProbe(Probes[Pi]);

      Lines.SaveToFile(TsvPath);
    finally
      Lines.Free;
    end;

    Assert.Pass('Wrote diagnostic dump to ' + TsvPath);
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

initialization
  TDUnitX.RegisterTestFixture(TRsmReaderTests);

end.
