// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Rsm.LocalsReader;

// Tests for the RSM (CSH7) symbol container produced by the Delphi
// linker option -VR. The reader is the only debug-info parser the
// project ships -- TD32 has been removed -- so these tests carry the
// full coverage of procedures, locals, classes and records that the
// higher layers (debugger, evaluator) rely on.

interface

uses

  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,
  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.Reader;

type

  [TestFixture]
  TRsmReaderLegacyTests = class
  private
    function ResolveRsmPath(AUse64Bit: Boolean): String;
    function ResolveExePath(AUse64Bit: Boolean): String;
    procedure DoTestParsesProcedures(AUse64Bit: Boolean);
    procedure DoTestParsesLocalsForLocalsProcedure(AUse64Bit: Boolean);
    procedure DoTestParsesClassMembers(AUse64Bit: Boolean);
    procedure DoTestParsesRecordMembers(AUse64Bit: Boolean);
    procedure DoTestRecordTypeIdxRoundTrip(AUse64Bit: Boolean);
    procedure DoTestClassFieldTypeIdxLinking(AUse64Bit: Boolean);
    procedure DoTestLocalsHaveDistinctOffsets(AUse64Bit: Boolean);
    procedure DoTestFindProcByName(AUse64Bit: Boolean);
    procedure DoTestFindProcContaining(AUse64Bit: Boolean);
    procedure DoTestDiscoversDerivedClass(AUse64Bit: Boolean);
    procedure DoTestDerivedInheritsBaseFields(AUse64Bit: Boolean);
    procedure DoTestDeepDerivedInheritsAllAncestors(AUse64Bit: Boolean);
    procedure DoTestNonComponentRtlInheritance(AUse64Bit: Boolean);
    procedure DoTestEdgeCaseLocalsAllDecoded(AUse64Bit: Boolean);
    procedure DoTestOpenArrayParamRecognized(AUse64Bit: Boolean);
  public
    [Test]
    procedure TestRsmFilePresent;
    [Test]
    procedure TestRsmStartsWithCsh7Magic;
    [Test]
    procedure TestLoadFromMissingFileLeavesEmpty;
    [Test]
    procedure TestLoadFromBytesWithoutSignatureLeavesEmpty;
    [Test]
    procedure TestParsesProcedures32;
    [Test]
    procedure TestParsesLocalsForLocalsProcedure32;
    [Test]
    procedure TestLocalsHaveDistinctOffsets32;
    [Test]
    procedure TestFindProcByName32;
    [Test]
    procedure TestFindProcContaining32;
    [Test]
    procedure TestParsesClassMembers32;
    [Test]
    procedure TestParsesRecordMembers32;
    [Test]
    procedure TestRecordTypeIdxRoundTrip32;
    [Test]
    procedure TestClassFieldTypeIdxLinking32;
    [Test]
    procedure TestDiscoversDerivedClass32;
    [Test]
    procedure TestDerivedInheritsBaseFields32;
    [Test]
    procedure TestDeepDerivedInheritsAllAncestors32;
    [Test]
    procedure TestNonComponentRtlInheritance32;
    [Test]
    procedure TestEdgeCaseLocalsAllDecoded32;
    [Test]
    procedure TestOpenArrayParamRecognized32;
    [Test]
    procedure TestFindGlobalTypeIdx32;
    /// §6.15 PIN. After
    /// <see cref="TRsmFieldAliasEnumBridge.Run"/> has registered the
    /// per-binary RTL type alias <c>$0671</c> against TThreadPriority's
    /// <c>$03</c> ENUM_DEF (via the F-prefix field-name convention),
    /// the Reader must recognize the alias as an enum AND resolve
    /// ordinals to element names.
    [Test]
    procedure TestFieldAliasEnumBridgeResolvesTThreadPriority32;
    /// §4.16 PIN. The <c>$31</c> property-record linker
    /// (<c>TRsmPropertyLinker</c>) must surface read-accessible
    /// properties on TPropHost, distinguishing field-backed from
    /// getter-backed declarations:
    /// <list type="bullet">
    ///   <item><c>PlainProp</c> is declared <c>read FPlainInt</c> so
    ///     the bridge resolves <see cref="TRsmClassProperty.UnderlyingField"/>
    ///     to <c>FPlainInt</c>. Live evaluate can read the property
    ///     via the underlying field's instance offset.</item>
    ///   <item><c>CalcProp</c> is declared <c>read GetCalcInt</c> —
    ///     no <c>$2C</c> field record matches the target alias, so
    ///     <c>UnderlyingField</c> stays empty (getter-backed).</item>
    ///   <item><c>Greeting</c> is declared <c>read GetGreeting</c>,
    ///     same treatment as CalcProp but with a string return type
    ///     for primitive-type-id coverage.</item>
    /// </list>
    [Test]
    procedure TestPropertyLinkerSurfacesFieldAndGetterBackedReads32;
    {$IFDEF CPUX64}
    [Test]
    procedure TestClassFieldTypeIdxLinking64;
    {$ENDIF}
    {$IFDEF CPUX64}
    [Test]
    procedure TestParsesProcedures64;
    [Test]
    procedure TestParsesLocalsForLocalsProcedure64;
    [Test]
    procedure TestLocalsHaveDistinctOffsets64;
    [Test]
    procedure TestFindProcByName64;
    [Test]
    procedure TestFindProcContaining64;
    [Test]
    procedure TestParsesClassMembers64;
    [Test]
    procedure TestParsesRecordMembers64;
    [Test]
    procedure TestRecordTypeIdxRoundTrip64;
    [Test]
    procedure TestDiscoversDerivedClass64;
    [Test]
    procedure TestDerivedInheritsBaseFields64;
    [Test]
    procedure TestEdgeCaseLocalsAllDecoded64;
    [Test]
    procedure TestOpenArrayParamRecognized64;
    {$ENDIF}
  end;

implementation

uses
  System.Classes;

function TRsmReaderLegacyTests.ResolveExePath(AUse64Bit: Boolean): String;
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

function TRsmReaderLegacyTests.ResolveRsmPath(AUse64Bit: Boolean): String;
begin
  Result := ChangeFileExt(ResolveExePath(AUse64Bit), '.rsm');
end;

procedure TRsmReaderLegacyTests.TestRsmFilePresent;
var
  Path: String;
begin
  // The .rsm sidecar is produced when DebugTarget is built with -VR.
  // If this assertion fails, the build configuration has lost the -VR
  // flag (Test.DptDebugger.dproj prebuild event) and every other RSM
  // test will fail in obscure ways.
  Path := ResolveRsmPath(False);
  Assert.IsTrue(TFile.Exists(Path),
    'Win32 RSM fixture missing (build with -V -VR?): ' + Path);
  {$IFDEF CPUX64}
  Path := ResolveRsmPath(True);
  Assert.IsTrue(TFile.Exists(Path),
    'Win64 RSM fixture missing (build with -V -VR?): ' + Path);
  {$ENDIF}
end;

procedure TRsmReaderLegacyTests.TestRsmStartsWithCsh7Magic;
var
  FS  : TFileStream;
  Buf : array[0..3] of Byte;
  Path: String;
begin
  Path := ResolveRsmPath(False);
  FS := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  try
    Assert.IsTrue(FS.Size >= 4, 'RSM smaller than 4 bytes: ' + Path);
    FS.ReadBuffer(Buf, 4);
  finally
    FS.Free;
  end;
  Assert.AreEqual(Integer($43), Integer(Buf[0]), 'RSM byte 0 must be ''C''');
  Assert.AreEqual(Integer($53), Integer(Buf[1]), 'RSM byte 1 must be ''S''');
  Assert.AreEqual(Integer($48), Integer(Buf[2]), 'RSM byte 2 must be ''H''');
  Assert.AreEqual(Integer($37), Integer(Buf[3]), 'RSM byte 3 must be ''7''');
end;

procedure TRsmReaderLegacyTests.TestLoadFromMissingFileLeavesEmpty;
var
  Reader: TRsmReader;
begin
  // Loading a non-existent EXE path must succeed silently (no .rsm
  // sidecar -> empty reader). The TD32 reader raises on a missing
  // file because it reads the EXE itself; the RSM reader skips
  // gracefully when the sidecar is absent.
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile('Z:\does-not-exist\nope.exe');
    Assert.IsTrue(Reader.Procs.Count = 0);
    Assert.IsTrue(Reader.Classes.Count = 0);
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.TestLoadFromBytesWithoutSignatureLeavesEmpty;
var
  Buf   : TBytes;
  Reader: TRsmReader;
begin
  SetLength(Buf, 1024);
  FillChar(Buf[0], Length(Buf), 0);
  Buf[0] := Ord('M');
  Buf[1] := Ord('Z');
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromBytes(Buf);
    Assert.IsTrue(Reader.Procs.Count = 0);
    Assert.IsTrue(Reader.Classes.Count = 0);
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestParsesProcedures(AUse64Bit: Boolean);
var
  Reader: TRsmReader;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));
    Assert.IsTrue(Reader.Procs.Count > 0,
      'Reader must extract at least one procedure from the RSM');
    Assert.IsTrue(Reader.FindProcByName('LocalsProcedure') >= 0,
      'LocalsProcedure must be present in the parsed procs');
    Assert.IsTrue(Reader.FindProcByName('DeepProcedure') >= 0,
      'DeepProcedure must be present in the parsed procs');
    Assert.IsTrue(Reader.FindProcByName('TargetProcedure') >= 0,
      'TargetProcedure must be present in the parsed procs');
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestParsesLocalsForLocalsProcedure(AUse64Bit: Boolean);
var
  Reader   : TRsmReader;
  ProcIdx  : Integer;
  Proc     : TRsmProc;
  HasA     : Boolean;
  HasB     : Boolean;
  HasC     : Boolean;
  I        : Integer;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));
    ProcIdx := Reader.FindProcByName('LocalsProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'LocalsProcedure not found');

    Proc := Reader.Procs[ProcIdx];
    Assert.IsTrue(Proc.Locals.Count >= 3,
      Format('Expected at least 3 locals in LocalsProcedure, got %d', [Proc.Locals.Count]));

    HasA := False; HasB := False; HasC := False;
    for I := 0 to Proc.Locals.Count - 1 do
    begin
      if SameText(Proc.Locals[I].Name, 'LocalA') then HasA := True;
      if SameText(Proc.Locals[I].Name, 'LocalB') then HasB := True;
      if SameText(Proc.Locals[I].Name, 'LocalC') then HasC := True;
    end;
    Assert.IsTrue(HasA, 'LocalA missing from LocalsProcedure locals');
    Assert.IsTrue(HasB, 'LocalB missing from LocalsProcedure locals');
    Assert.IsTrue(HasC, 'LocalC missing from LocalsProcedure locals');
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestLocalsHaveDistinctOffsets(AUse64Bit: Boolean);
var
  Reader : TRsmReader;
  ProcIdx: Integer;
  Proc   : TRsmProc;
  Seen   : TArray<Int32>;
  I, J   : Integer;
  RealOffsets, SynthOffsets: Integer;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));
    ProcIdx := Reader.FindProcByName('LocalsProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'LocalsProcedure not found');
    Proc := Reader.Procs[ProcIdx];

    SetLength(Seen, 0);
    RealOffsets := 0;
    SynthOffsets := 0;
    for I := 0 to Proc.Locals.Count - 1 do
      if Pos('Local', Proc.Locals[I].Name) = 1 then
      begin
        for J := 0 to High(Seen) do
          Assert.AreNotEqual(Seen[J], Proc.Locals[I].BpOffset,
            Format('Two locals share BP offset %d', [Proc.Locals[I].BpOffset]));
        SetLength(Seen, Length(Seen) + 1);
        Seen[High(Seen)] := Proc.Locals[I].BpOffset;
        // Synthesized fallback values sit at -10000 and below; real
        // BPRel offsets fit in a few-hundred-bytes window around 0.
        if Proc.Locals[I].BpOffset <= -5000 then
          Inc(SynthOffsets)
        else
          Inc(RealOffsets);
      end;
    Assert.IsTrue(Length(Seen) >= 3,
      Format('Expected at least 3 LocalA/B/C entries, got %d', [Length(Seen)]));
    Assert.AreEqual(0, SynthOffsets,
      Format('LocalsProcedure has %d locals that fell back to a ' +
             'synthesized BpOffset; the decoder did not recognize ' +
             'their encoding (real=%d)',
        [SynthOffsets, RealOffsets]));
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestFindProcByName(AUse64Bit: Boolean);
var
  Reader: TRsmReader;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));
    Assert.IsTrue(Reader.FindProcByName('localsprocedure') >= 0,
      'FindProcByName must be case-insensitive');
    Assert.AreEqual(-1, Reader.FindProcByName('ThisProcedureDoesNotExist__xyz'),
      'Unknown name must return -1');
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestParsesClassMembers(AUse64Bit: Boolean);
var
  Reader     : TRsmReader;
  Member     : TRsmClassMember;
  ExpectedPtr: UInt32;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));

    Assert.IsTrue(Reader.FindClassByName('TInner') >= 0,
      'TInner class must be parsed from the RSM type stream');
    Assert.IsTrue(Reader.FindClassByName('TOuter') >= 0,
      'TOuter class must be parsed from the RSM type stream');

    if AUse64Bit then ExpectedPtr := 8 else ExpectedPtr := 4;

    Assert.IsTrue(Reader.FindClassMember('TInner', 'FInnerInt', Member),
      'TInner.FInnerInt must be parsed');
    Assert.AreEqual(ExpectedPtr, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TInner', 'FInnerStr', Member),
      'TInner.FInnerStr must be parsed');
    Assert.AreEqual(ExpectedPtr * 2, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TOuter', 'FOuterInt', Member));
    Assert.AreEqual(ExpectedPtr, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TOuter', 'FOuterInner', Member));
    Assert.AreEqual(ExpectedPtr * 2, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TOuter', 'FOuterStr', Member));
    Assert.AreEqual(ExpectedPtr * 3, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('touter', 'fouterint', Member),
      'FindClassMember must be case-insensitive');
    Assert.IsFalse(Reader.FindClassMember('TNoSuchClass', 'X', Member));
    Assert.IsFalse(Reader.FindClassMember('TInner', 'NoSuchField', Member));
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestParsesRecordMembers(AUse64Bit: Boolean);
var
  Reader  : TRsmReader;
  Member  : TRsmClassMember;
  PointIdx: Integer;
  RectIdx : Integer;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));

    PointIdx := Reader.FindClassByName('TPoint2D');
    RectIdx  := Reader.FindClassByName('TRect2D');
    Assert.IsTrue(PointIdx >= 0, 'TPoint2D record must be parsed');
    Assert.IsTrue(RectIdx  >= 0, 'TRect2D record must be parsed');

    Assert.IsTrue(Reader.Classes[PointIdx].Kind = skRecord,
      'TPoint2D must have Kind = skRecord');
    Assert.IsTrue(Reader.Classes[RectIdx].Kind = skRecord,
      'TRect2D must have Kind = skRecord');

    Assert.IsTrue(Reader.FindClassMember('TPoint2D', 'FX', Member));
    Assert.AreEqual(UInt32(0), Member.Offset);
    Assert.IsTrue(Reader.FindClassMember('TPoint2D', 'FY', Member));
    Assert.AreEqual(UInt32(4), Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TRect2D', 'FTopLeft', Member));
    Assert.AreEqual(UInt32(0), Member.Offset);
    Assert.IsTrue(Reader.FindClassMember('TRect2D', 'FBottomRight', Member));
    Assert.AreEqual(UInt32(8), Member.Offset);

    // TMixedRec stresses the offset decoder by mixing field widths in
    // a single record: Int (4) + Int64 (8) + string (pointer-sized).
    // Offsets must come from the byte stream, NOT from "total size /
    // field count" which would only happen to fit homogeneous records.
    Assert.IsTrue(Reader.FindClassByName('TMixedRec') >= 0,
      'TMixedRec record must be parsed');
    Assert.IsTrue(Reader.Classes[Reader.FindClassByName('TMixedRec')].Kind = skRecord,
      'TMixedRec must have Kind = skRecord');
    Assert.IsTrue(Reader.FindClassMember('TMixedRec', 'FMixedInt', Member),
      'TMixedRec.FMixedInt must be parsed');
    Assert.AreEqual(UInt32(0), Member.Offset, 'FMixedInt at offset 0');
    Assert.IsTrue(Reader.FindClassMember('TMixedRec', 'FMixedInt64', Member),
      'TMixedRec.FMixedInt64 must be parsed');
    // Empirically Delphi aligns Int64 to 8 bytes regardless of platform
    // when it follows a smaller field, so the offset is 8 on both archs.
    Assert.AreEqual(UInt32(8), Member.Offset,
      'FMixedInt64 must be 8-byte-aligned after the leading Integer');
    Assert.IsTrue(Reader.FindClassMember('TMixedRec', 'FMixedStr', Member),
      'TMixedRec.FMixedStr must be parsed');
    Assert.AreEqual(UInt32(16), Member.Offset,
      'FMixedStr lands at 16 (after Int + 4 padding + Int64)');
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestRecordTypeIdxRoundTrip(AUse64Bit: Boolean);
var
  Reader  : TRsmReader;
  PointIdx: Integer;
  TypeIdx : UInt32;
  Member  : TRsmClassMember;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));

    PointIdx := Reader.FindClassByName('TPoint2D');
    Assert.IsTrue(PointIdx >= 0);
    TypeIdx := Reader.Classes[PointIdx].TypeIdx;
    Assert.IsTrue(TypeIdx <> 0,
      'TPoint2D must carry a non-zero RSM type-id for cross-reference');

    Assert.AreEqual(PointIdx, Reader.FindStructByTypeIdx(TypeIdx));

    Assert.IsTrue(Reader.FindStructMemberByTypeIdx(TypeIdx, 'FX', Member));
    Assert.AreEqual(UInt32(0), Member.Offset);
    Assert.IsTrue(Reader.FindStructMemberByTypeIdx(TypeIdx, 'FY', Member));
    Assert.AreEqual(UInt32(4), Member.Offset);

    Assert.IsFalse(Reader.FindStructMemberByTypeIdx($DEADBEEF, 'FX', Member));
    Assert.IsFalse(Reader.FindStructMemberByTypeIdx(TypeIdx, 'FNoSuchField', Member));
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestClassFieldTypeIdxLinking(AUse64Bit: Boolean);
var
  Reader        : TRsmReader;
  WithRecIdx    : Integer;
  Members       : IList<TRsmClassMember>;
  I             : Integer;
  FOriginTypeIdx: UInt32;
  FBoundsTypeIdx: UInt32;
  FPairTypeIdx  : UInt32;
  FNestedTypeIdx: UInt32;
  PointIdx      : Integer;
  RectIdx       : Integer;
  PairIdx       : Integer;
  InnerIdx      : Integer;
begin
  // After LinkMemberTypeIdsFromFormatA, every class field whose type
  // is itself a known class or record must have Member.TypeIdx set
  // to that type's surrogate. This is what removes the
  // size+name-heuristic dependency in EvaluateVariable's dotted walk.
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));

    WithRecIdx := Reader.FindClassByName('TWithRec');
    PointIdx   := Reader.FindClassByName('TPoint2D');
    RectIdx    := Reader.FindClassByName('TRect2D');
    PairIdx    := Reader.FindClassByName('TPair');
    InnerIdx   := Reader.FindClassByName('TInner');
    Assert.IsTrue(WithRecIdx >= 0);
    Assert.IsTrue(PointIdx   >= 0);
    Assert.IsTrue(RectIdx    >= 0);
    Assert.IsTrue(PairIdx    >= 0);
    Assert.IsTrue(InnerIdx   >= 0);

    FOriginTypeIdx := 0;
    FBoundsTypeIdx := 0;
    FPairTypeIdx   := 0;
    FNestedTypeIdx := 0;
    Members := Reader.Classes[WithRecIdx].Members;
    for I := 0 to Members.Count - 1 do
    begin
      if SameText(Members[I].Name, 'FOrigin')    then FOriginTypeIdx := Members[I].TypeIdx;
      if SameText(Members[I].Name, 'FBounds')    then FBoundsTypeIdx := Members[I].TypeIdx;
      if SameText(Members[I].Name, 'FPair')      then FPairTypeIdx   := Members[I].TypeIdx;
      if SameText(Members[I].Name, 'FNestedObj') then FNestedTypeIdx := Members[I].TypeIdx;
    end;

    // Each record-/class-typed field must now resolve back to the
    // declaring type via FindStructByTypeIdx.
    Assert.AreEqual(PointIdx, Reader.FindStructByTypeIdx(FOriginTypeIdx),
      'TWithRec.FOrigin must link to TPoint2D');
    Assert.AreEqual(RectIdx, Reader.FindStructByTypeIdx(FBoundsTypeIdx),
      'TWithRec.FBounds must link to TRect2D');
    Assert.AreEqual(PairIdx, Reader.FindStructByTypeIdx(FPairTypeIdx),
      'TWithRec.FPair must link to TPair');
    Assert.AreEqual(InnerIdx, Reader.FindStructByTypeIdx(FNestedTypeIdx),
      'TWithRec.FNestedObj must link to TInner');
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.TestClassFieldTypeIdxLinking32;          begin DoTestClassFieldTypeIdxLinking(False);         end;
{$IFDEF CPUX64}
procedure TRsmReaderLegacyTests.TestClassFieldTypeIdxLinking64;          begin DoTestClassFieldTypeIdxLinking(True);          end;
{$ENDIF}

procedure TRsmReaderLegacyTests.TestParsesProcedures32;                 begin DoTestParsesProcedures(False);                 end;
procedure TRsmReaderLegacyTests.TestParsesLocalsForLocalsProcedure32;   begin DoTestParsesLocalsForLocalsProcedure(False);   end;
procedure TRsmReaderLegacyTests.TestLocalsHaveDistinctOffsets32;        begin DoTestLocalsHaveDistinctOffsets(False);        end;
procedure TRsmReaderLegacyTests.TestFindProcByName32;                   begin DoTestFindProcByName(False);                   end;
procedure TRsmReaderLegacyTests.TestParsesClassMembers32;               begin DoTestParsesClassMembers(False);               end;
procedure TRsmReaderLegacyTests.TestParsesRecordMembers32;              begin DoTestParsesRecordMembers(False);              end;
procedure TRsmReaderLegacyTests.TestRecordTypeIdxRoundTrip32;           begin DoTestRecordTypeIdxRoundTrip(False);           end;

procedure TRsmReaderLegacyTests.DoTestFindProcContaining(AUse64Bit: Boolean);
var
  Reader : TRsmReader;
  ProcIdx: Integer;
  Proc   : TRsmProc;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));
    ProcIdx := Reader.FindProcByName('LocalsProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'LocalsProcedure not found');
    Proc := Reader.Procs[ProcIdx];

    Assert.IsTrue(Proc.SegmentOffset > 0,
      'LocalsProcedure must have a decoded SegmentOffset (RSM-only, no map patch)');

    Assert.AreEqual(ProcIdx,
      Reader.FindProcContaining(Proc.SegmentOffset + Proc.Size div 2));
    Assert.AreEqual(ProcIdx,
      Reader.FindProcContaining(Proc.SegmentOffset));
    Assert.AreNotEqual(ProcIdx,
      Reader.FindProcContaining(Proc.SegmentOffset + Proc.Size));
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.TestFindProcContaining32;                begin DoTestFindProcContaining(False);               end;

procedure TRsmReaderLegacyTests.DoTestDiscoversDerivedClass(AUse64Bit: Boolean);
var
  Reader     : TRsmReader;
  Member     : TRsmClassMember;
  ExpectedPtr: UInt32;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));

    Assert.IsTrue(Reader.FindClassByName('TDerived') >= 0,
      'TDerived must be discovered as a class');

    // Pointer width determines the slot the first own field sits in:
    // class instance layout is <VMT-ptr> followed by parent fields,
    // then own fields. TDerived inherits FInnerInt (offset Ptr) and
    // FInnerStr (offset Ptr*2), so its own fields start at Ptr*3.
    if AUse64Bit then ExpectedPtr := 8 else ExpectedPtr := 4;

    Assert.IsTrue(Reader.FindClassMember('TDerived', 'FDerivedExtra', Member),
      'TDerived.FDerivedExtra (own field) must be parsed');
    Assert.AreEqual(ExpectedPtr * 3, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TDerived', 'FDerivedLabel', Member),
      'TDerived.FDerivedLabel (own field) must be parsed');
    Assert.AreEqual(ExpectedPtr * 4, Member.Offset);
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestDerivedInheritsBaseFields(AUse64Bit: Boolean);
var
  Reader     : TRsmReader;
  Member     : TRsmClassMember;
  ExpectedPtr: UInt32;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));

    if AUse64Bit then ExpectedPtr := 8 else ExpectedPtr := 4;

    // The inherited TInner fields must be reachable through the
    // derived class -- otherwise expressions like
    // GGlobalDerived.FInnerInt can't be evaluated.
    Assert.IsTrue(Reader.FindClassMember('TDerived', 'FInnerInt', Member),
      'TDerived must inherit FInnerInt from TInner');
    Assert.AreEqual(ExpectedPtr, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TDerived', 'FInnerStr', Member),
      'TDerived must inherit FInnerStr from TInner');
    Assert.AreEqual(ExpectedPtr * 2, Member.Offset);
  finally
    Reader.Free;
  end;
end;

/// <summary>
///   Multi-level inheritance walk. <c>TDeepDerived</c> -&gt;
///   <c>TDerived</c> -&gt; <c>TInner</c> means that
///   <c>FindClassMember('TDeepDerived', 'FInnerInt')</c> must traverse
///   two ancestor links. A single broken link in the middle of the
///   chain is enough to make resolution fail silently with the
///   offset-collision heuristic; testing each level pinpoints which
///   hop drops.
/// </summary>
procedure TRsmReaderLegacyTests.DoTestDeepDerivedInheritsAllAncestors(
  AUse64Bit: Boolean);
var
  Reader     : TRsmReader;
  Member     : TRsmClassMember;
  ExpectedPtr: UInt32;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));

    if AUse64Bit then ExpectedPtr := 8 else ExpectedPtr := 4;

    // Own field of TDeepDerived: no walk required.
    Assert.IsTrue(Reader.FindClassMember('TDeepDerived', 'FDeepFlag', Member),
      'TDeepDerived.FDeepFlag (own field) must be parsed');
    Assert.AreEqual(ExpectedPtr * 5, Member.Offset);

    // One hop up to TDerived.
    Assert.IsTrue(Reader.FindClassMember('TDeepDerived', 'FDerivedExtra', Member),
      'TDeepDerived must inherit FDerivedExtra from TDerived (one hop up)');
    Assert.AreEqual(ExpectedPtr * 3, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TDeepDerived', 'FDerivedLabel', Member),
      'TDeepDerived must inherit FDerivedLabel from TDerived (one hop up)');
    Assert.AreEqual(ExpectedPtr * 4, Member.Offset);

    // Two hops up to TInner.
    Assert.IsTrue(Reader.FindClassMember('TDeepDerived', 'FInnerInt', Member),
      'TDeepDerived must inherit FInnerInt from TInner (two hops up)');
    Assert.AreEqual(ExpectedPtr, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TDeepDerived', 'FInnerStr', Member),
      'TDeepDerived must inherit FInnerStr from TInner (two hops up)');
    Assert.AreEqual(ExpectedPtr * 2, Member.Offset);
  finally
    Reader.Free;
  end;
end;

/// <summary>
///   Verification probe for a non-TComponent RTL hierarchy:
///   <c>TStringList -> TStrings -> TPersistent -> TObject</c>.
///   Establishes how much of the fix is structurally general vs
///   how much is bounded by Delphi's RSM emission shape for a
///   specific hierarchy. Asserts what currently holds:
///   <list type="bullet">
///     <item><c>TStrings</c> is discovered as a class (proves the
///       8 KB forward-scan window in the discovery is general --
///       <c>TStrings</c> sits ~5.6 KB between its first name and
///       its trailer in this binary, and the previous 2 KB window
///       missed it entirely).</item>
///     <item><c>TStringList.FCount</c> (an own field) resolves --
///       the leaf class's field-scan window is wide enough.</item>
///   </list>
///   Does NOT assert
///   <c>TStringList.FUpdateCount via TStrings</c>: the RSM emits
///   <c>TStrings</c>'s field records in a region the per-class
///   backward scan cannot currently reach (the records sit BEFORE
///   the previous discovered class's anchor, which the
///   AMinStartOff cap excludes to prevent cross-class leakage).
///   Closed as a documented design limitation in
///   <c>DPT.Rsm.Format.md</c> §4.14 -- the cap is load-bearing
///   against cross-class leakage on tightly-packed sibling pairs,
///   and every investigated relaxation either over-collects on
///   DebugTarget fixtures or requires an offset source the $2C
///   body shape does not carry.
/// </summary>
procedure TRsmReaderLegacyTests.DoTestNonComponentRtlInheritance(
  AUse64Bit: Boolean);
var
  Reader   : TRsmReader;
  Member   : TRsmClassMember;
  StrIdx   : Integer;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));

    Assert.IsTrue(Reader.FindClassByName('TStringList') >= 0,
      'TStringList must be discovered as a class');
    StrIdx := Reader.FindClassByName('TStrings');
    Assert.IsTrue(StrIdx >= 0,
      'TStrings must be discovered as a class (general discovery, ' +
      'not TComponent-specific)');

    // TStringList's OWN fields (the ones whose records sit close
    // enough behind the class anchor) are reachable via the chain
    // walker. This validates that the same FindClassMember walker
    // works regardless of which RTL family the hierarchy comes
    // from.
    Assert.IsTrue(
      Reader.FindClassMember('TStringList', 'FCount', Member),
      'TStringList.FCount (own field) must resolve');
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.DoTestEdgeCaseLocalsAllDecoded(AUse64Bit: Boolean);
// Exercises BPRel decoding against a deliberately diverse set of
// local types (Variant, IInterface, dynamic array, record, Double,
// Boolean, Char, set, pointer, class). Every such local must
// produce a decoded BpOffset within the plausible BP-relative
// window for that proc; if the decoder doesn't recognize an
// encoding form it falls back to a synthesized far-below-zero
// value, which this test fails on so the gap surfaces.
var
  Reader : TRsmReader;
  ProcIdx: Integer;
  Proc   : TRsmProc;
  I      : Integer;
  Synth  : Integer;
  Names  : String;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));
    ProcIdx := Reader.FindProcByName('EdgeCaseLocalsProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'EdgeCaseLocalsProcedure not found');
    Proc := Reader.Procs[ProcIdx];

    Assert.IsTrue(Proc.Locals.Count >= 5,
      Format('EdgeCaseLocalsProcedure should expose multiple locals; got %d',
        [Proc.Locals.Count]));

    Synth := 0;
    Names := '';
    for I := 0 to Proc.Locals.Count - 1 do
      if Pos('Local', Proc.Locals[I].Name) = 1 then
      begin
        if Proc.Locals[I].BpOffset <= -5000 then
        begin
          Inc(Synth);
          if Names <> '' then Names := Names + ', ';
          Names := Names + Proc.Locals[I].Name;
        end;
      end;
    Assert.AreEqual(0, Synth,
      Format('%d edge-case locals fell back to a synthesized BpOffset ' +
             '(unrecognized encoding): %s',
        [Synth, Names]));
  finally
    Reader.Free;
  end;
end;

/// <summary>
///   Asserts that an open-array parameter (declared in source as
///   <c>const AItems: array of string</c>) shows up in the proc's
///   Locals list as a register-passed entry rather than a missing
///   BP-relative slot.
/// </summary>
/// <remarks>
///   Open-array parameters live in CPU registers under Delphi's
///   default calling convention, not in the BP-relative stack
///   frame. The reader must therefore surface them via the
///   PARAM_TAG ($22) path and tag them as <c>lkRegister</c> with
///   the right <c>RegParamIdx</c>; otherwise the debugger silently
///   fails on a missing BpOffset when the agent asks for the
///   parameter value.
/// </remarks>
procedure TRsmReaderLegacyTests.DoTestOpenArrayParamRecognized(AUse64Bit: Boolean);
var
  Reader     : TRsmReader;
  ProcIdx, I : Integer;
  Proc       : TRsmProc;
  Found      : Boolean;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));
    ProcIdx := Reader.FindProcByName('OpenArrayStringProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'OpenArrayStringProcedure not found');
    Proc := Reader.Procs[ProcIdx];

    Found := False;
    for I := 0 to Proc.Locals.Count - 1 do
      if SameText(Proc.Locals[I].Name, 'AItems') then
      begin
        Found := True;
        Assert.IsTrue(Proc.Locals[I].Kind = lkRegister,
          'AItems must be flagged as a register parameter');
        Assert.AreEqual(Byte(0), Proc.Locals[I].RegParamIdx,
          'AItems is the first declared param, so RegParamIdx = 0');
      end;
    Assert.IsTrue(Found, 'AItems parameter missing from OpenArrayStringProcedure');
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.TestDiscoversDerivedClass32;             begin DoTestDiscoversDerivedClass(False);            end;
procedure TRsmReaderLegacyTests.TestDerivedInheritsBaseFields32;         begin DoTestDerivedInheritsBaseFields(False);        end;
procedure TRsmReaderLegacyTests.TestDeepDerivedInheritsAllAncestors32;   begin DoTestDeepDerivedInheritsAllAncestors(False);  end;
procedure TRsmReaderLegacyTests.TestNonComponentRtlInheritance32;        begin DoTestNonComponentRtlInheritance(False);       end;
procedure TRsmReaderLegacyTests.TestEdgeCaseLocalsAllDecoded32;          begin DoTestEdgeCaseLocalsAllDecoded(False);         end;
procedure TRsmReaderLegacyTests.TestOpenArrayParamRecognized32;          begin DoTestOpenArrayParamRecognized(False);         end;

/// <summary>
///   Reader-level pinpoint for the global-record dotted-walk path.
///   The higher-level evaluate test
///   (<c>TestMcpEvaluateGlobalRecordFieldNavigation</c>) exercises
///   end-to-end behaviour; this one isolates the three steps it
///   relies on so a regression makes the failure mode obvious:
///   <list type="number">
///     <item>The reader scanned the global-variable record at all.</item>
///     <item>The type id stored on that global resolves to a struct
///       entry.</item>
///     <item>That entry is the expected RECORD type for the global.</item>
///   </list>
/// </summary>
/// <remarks>
///   <c>FindStructByTypeIdx</c> is NOT the right call here: it
///   compares against <c>Classes[i].TypeIdx</c>, which holds the
///   internal file-offset token, not the RSM 2-byte type id we
///   store in <c>FGlobalByName</c>. <c>FindClassIdxByRsmTypeId</c>
///   is the registry-backed lookup that matches what the dotted-
///   walk in <c>TDebugger.EvaluateVariable</c> uses.
/// </remarks>
procedure TRsmReaderLegacyTests.TestFindGlobalTypeIdx32;
  procedure CheckGlobalResolvesTo(Reader: TRsmReader;
    const AGlobalName, AExpectedRecord: String);
  var
    TypeIdx : UInt32;
    StructIdx: Integer;
  begin
    TypeIdx := Reader.FindGlobalTypeIdx(AGlobalName);
    Assert.IsTrue(TypeIdx <> 0,
      Format('FindGlobalTypeIdx must surface %s -- the reader did not ' +
             'pick up the global''s $20-prefixed type-info record.',
        [AGlobalName]));

    StructIdx := Reader.FindClassIdxByRsmTypeId(TypeIdx);
    Assert.IsTrue(StructIdx >= 0,
      Format('Global RSM type id 0x%x for %s did not resolve to a ' +
             'struct via FindClassIdxByRsmTypeId -- the registry built ' +
             'in LinkMemberTypeIdsFromFormatA is missing that id.',
        [TypeIdx, AGlobalName]));

    Assert.IsTrue(Reader.Classes[StructIdx].Kind = skRecord,
      Format('%s must point at a RECORD entry, got kind=%d ' +
             '(class=0, record=1).',
        [AGlobalName, Ord(Reader.Classes[StructIdx].Kind)]));
    Assert.AreEqual(AExpectedRecord, Reader.Classes[StructIdx].Name,
      Format('%s resolves to the wrong record name (RSM id 0x%x).',
        [AGlobalName, TypeIdx]));
  end;
var
  Reader: TRsmReader;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(False));
    CheckGlobalResolvesTo(Reader, 'GGlobalMixed', 'TMixedRec');
    CheckGlobalResolvesTo(Reader, 'GGlobalP3D',   'TPoint3D');
  finally
    Reader.Free;
  end;
end;

{$IFDEF CPUX64}
procedure TRsmReaderLegacyTests.TestParsesProcedures64;                 begin DoTestParsesProcedures(True);                  end;
procedure TRsmReaderLegacyTests.TestParsesLocalsForLocalsProcedure64;   begin DoTestParsesLocalsForLocalsProcedure(True);    end;
procedure TRsmReaderLegacyTests.TestLocalsHaveDistinctOffsets64;        begin DoTestLocalsHaveDistinctOffsets(True);         end;
procedure TRsmReaderLegacyTests.TestFindProcByName64;                   begin DoTestFindProcByName(True);                    end;
procedure TRsmReaderLegacyTests.TestFindProcContaining64;               begin DoTestFindProcContaining(True);                end;
procedure TRsmReaderLegacyTests.TestParsesClassMembers64;               begin DoTestParsesClassMembers(True);                end;
procedure TRsmReaderLegacyTests.TestParsesRecordMembers64;              begin DoTestParsesRecordMembers(True);               end;
procedure TRsmReaderLegacyTests.TestRecordTypeIdxRoundTrip64;           begin DoTestRecordTypeIdxRoundTrip(True);            end;
procedure TRsmReaderLegacyTests.TestDiscoversDerivedClass64;            begin DoTestDiscoversDerivedClass(True);             end;
procedure TRsmReaderLegacyTests.TestDerivedInheritsBaseFields64;        begin DoTestDerivedInheritsBaseFields(True);         end;
procedure TRsmReaderLegacyTests.TestEdgeCaseLocalsAllDecoded64;         begin DoTestEdgeCaseLocalsAllDecoded(True);          end;
procedure TRsmReaderLegacyTests.TestOpenArrayParamRecognized64;         begin DoTestOpenArrayParamRecognized(True);          end;
{$ENDIF}

procedure TRsmReaderLegacyTests.TestFieldAliasEnumBridgeResolvesTThreadPriority32;
const
  // The $21 REGVAR record's typeId for AStatusPriority -- the same
  // value the linker uses in the $2C FThreadPriority field record
  // for TThPriHost.FThreadPriority. Read via the structural
  // disambiguator landed in the previous session.
  TThreadPriorityAlias: UInt32 = $0671;
var
  Reader : TRsmReader;
  Name   : String;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(Reader.IsEnumTypeId(TThreadPriorityAlias),
      Format('Reader.IsEnumTypeId(`$%x) must be True after the §6.15 ' +
             'field-alias bridge resolves the alias to TThreadPriority''s ' +
             'EnumDef via the FThreadPriority field convention.',
             [TThreadPriorityAlias]));
    Assert.IsTrue(Reader.TryGetEnumConstantName(
                    TThreadPriorityAlias, 4, Name),
      Format('TryGetEnumConstantName(`$%x, 4) must resolve to ' +
             'tpHigher via the field-alias bridge.',
             [TThreadPriorityAlias]));
    Assert.AreEqual('tpHigher', Name,
      'Ordinal 4 of TThreadPriority must resolve to ''tpHigher'' -- ' +
      'got: ' + Name);
    Assert.IsTrue(Reader.TryGetEnumConstantName(
                    TThreadPriorityAlias, 0, Name),
      Format('TryGetEnumConstantName(`$%x, 0) must resolve to tpIdle.',
             [TThreadPriorityAlias]));
    Assert.AreEqual('tpIdle', Name,
      'Ordinal 0 of TThreadPriority must resolve to ''tpIdle'' -- ' +
      'got: ' + Name);
    Assert.IsTrue(Reader.TryGetEnumConstantName(
                    TThreadPriorityAlias, 6, Name),
      Format('TryGetEnumConstantName(`$%x, 6) must resolve to tpTimeCritical.',
             [TThreadPriorityAlias]));
    Assert.AreEqual('tpTimeCritical', Name,
      'Ordinal 6 of TThreadPriority must resolve to ''tpTimeCritical'' -- ' +
      'got: ' + Name);
  finally
    Reader.Free;
  end;
end;

procedure TRsmReaderLegacyTests.TestPropertyLinkerSurfacesFieldAndGetterBackedReads32;
var
  Reader   : TRsmReader;
  Prop     : TRsmClassProperty;
  ClsIdx   : Integer;
  PropCount: Integer;
begin
  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(False));

    // Diagnostic: confirm TPropHost is discovered and report its
    // property count before the per-property assertions. Surfaces
    // whether the failure is at the class-discovery stage, the
    // PropertyLinker's $31-walk stage, or the field-bridge stage.
    ClsIdx := Reader.FindClassByName('TPropHost');
    Assert.IsTrue(ClsIdx >= 0,
      'TPropHost class not discovered -- StructDiscoverer regressed?');
    if Reader.Classes[ClsIdx].Properties = nil then
      PropCount := -1
    else
      PropCount := Reader.Classes[ClsIdx].Properties.Count;
    // Count total property attributions across ALL classes -- if 0,
    // the parser never matched any $31 record; if > 0 but TPropHost
    // got 0, attribution lost the records to a different class.
    var GlobalPropCount: Integer := 0;
    var FirstAttributedCls: String := '';
    for var I: Integer := 0 to Reader.Classes.Count - 1 do
      if Reader.Classes[I].Properties <> nil then
      begin
        Inc(GlobalPropCount, Reader.Classes[I].Properties.Count);
        if FirstAttributedCls = '' then
          FirstAttributedCls := Reader.Classes[I].Name;
      end;
    Assert.IsTrue(PropCount >= 3,
      Format('TPropHost.Properties.Count expected >= 3, got %d ' +
             '(-1 means nil). TPropHost class kind=%d typeIdx=0x%x. ' +
             'Global property count across ALL classes=%d (first ' +
             'attributed class: ''%s''). Diagnoses: 0/0 = parser ' +
             'broken; N/0 = attribution sends to wrong class; N/<3 = ' +
             'parser misses some records.',
             [PropCount, Ord(Reader.Classes[ClsIdx].Kind),
              Reader.Classes[ClsIdx].TypeIdx, GlobalPropCount,
              FirstAttributedCls]));

    // PlainProp -- field-backed (`property PlainProp: Integer
    // read FPlainInt`). UnderlyingField must resolve to the
    // backing field's name so live evaluate can read the property
    // via the field's instance offset.
    Assert.IsTrue(Reader.FindClassProperty('TPropHost', 'PlainProp', Prop),
      'TPropHost.PlainProp must be in the property list -- $31 ' +
      'record parser or block-owner attribution regressed?');
    Assert.AreEqual('PlainProp', Prop.Name,
      'PlainProp.Name round-trip mismatch');
    Assert.AreEqual<UInt16>($02, Prop.PrimitiveTypeId,
      Format('PlainProp.PrimitiveTypeId must be $02 (Integer). Got $%x',
             [Integer(Prop.PrimitiveTypeId)]));
    Assert.AreEqual('FPlainInt', Prop.UnderlyingField,
      'PlainProp must bridge to its backing field FPlainInt via the ' +
      'target-id -> $2C-alias map. UnderlyingField=''' +
      Prop.UnderlyingField + '''');

    // CalcProp -- getter-backed (`property CalcProp: Integer
    // read GetCalcInt`). No $2C field record carries CalcProp's
    // target alias, so UnderlyingField stays empty.
    Assert.IsTrue(Reader.FindClassProperty('TPropHost', 'CalcProp', Prop),
      'TPropHost.CalcProp must be in the property list');
    Assert.AreEqual<UInt16>($02, Prop.PrimitiveTypeId,
      Format('CalcProp.PrimitiveTypeId must be $02 (Integer). Got $%x',
             [Integer(Prop.PrimitiveTypeId)]));
    Assert.AreEqual('', Prop.UnderlyingField,
      'CalcProp is getter-backed -- UnderlyingField must stay empty. ' +
      'Got: ''' + Prop.UnderlyingField + ''' (this means a $2C field ' +
      'happens to share CalcProp''s target alias, which would point ' +
      'live evaluate at the wrong byte).');

    // Greeting -- getter-backed string property. Exercises a
    // different primitive-type-id ($04 = string) so the test
    // notices a regression where prim-type read shifts position.
    Assert.IsTrue(Reader.FindClassProperty('TPropHost', 'Greeting', Prop),
      'TPropHost.Greeting must be in the property list');
    Assert.AreEqual<UInt16>($04, Prop.PrimitiveTypeId,
      Format('Greeting.PrimitiveTypeId must be $04 (string). Got $%x ' +
             '-- did the prim-type byte position drift?',
             [Integer(Prop.PrimitiveTypeId)]));
    Assert.AreEqual('', Prop.UnderlyingField,
      'Greeting is getter-backed -- UnderlyingField must stay empty.');

    // A property of a class that doesn't exist must return False
    // cleanly rather than throwing.
    Assert.IsFalse(Reader.FindClassProperty('TPropHost', 'NoSuchProp', Prop),
      'Non-existent property must return False');
    Assert.IsFalse(Reader.FindClassProperty('TNoSuchClass', 'PlainProp', Prop),
      'Non-existent class must return False');
  finally
    Reader.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRsmReaderLegacyTests);

end.
