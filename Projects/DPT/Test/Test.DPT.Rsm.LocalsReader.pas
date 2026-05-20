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

  DPT.MapFileParser,
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
    procedure TestTfwLoadDiagnostic;
    [Test]
    procedure TestFindGlobalTypeIdx32;
    [Test]
    procedure TestTfwGlobalRecordResolves;
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
  System.Classes,
  System.Diagnostics;

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
///   Does NOT yet assert
///   <c>TStringList.FUpdateCount via TStrings</c>: the RSM emits
///   <c>TStrings</c>'s field records in a region the per-class
///   backward scan cannot currently reach (the records sit BEFORE
///   the previous discovered class's anchor, which the
///   AMinStartOff cap excludes to prevent cross-class leakage).
///   That gap is the next work item for non-TComponent hierarchies.
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

/// <summary>
///   Developer-machine-only counterpart to TestFindGlobalTypeIdx32:
///   covers globals declared in the interface section of a real
///   large binary (TFW.exe), where the RSM encoder uses different
///   $20-record payload flags than the impl-scope globals our
///   DebugTarget fixture exercises.
/// </summary>
/// <remarks>
///   The agent's TFW repro showed <c>evaluate AppCaps.DbKindName</c>
///   failing with "Failed to evaluate variable" even though
///   <c>evaluate AppCaps</c> (bare global) returns sensible bytes.
///   We verify the two structural facts the dotted-walk relies on:
///   <list type="number">
///     <item>The interface-scope record types (TAppCaps, TKonsApl,
///       TMdt) actually land in FClasses (which is gated on the
///       record-field scanner accepting field names that don't
///       follow Delphi's 'F'-prefix convention).</item>
///     <item><c>FindRecordsByMemberName(field)</c> resolves a
///       field name back to the right record uniquely -- the
///       name-based fallback the evaluator falls back to when
///       a global's encoded type id doesn't match the registry
///       (still an open encoding question on TFW's interface-
///       scope globals).</item>
///   </list>
///   Skipped silently when the TFW fixture is missing so the build
///   stays green on clean machines.
/// </remarks>
procedure TRsmReaderLegacyTests.TestTfwGlobalRecordResolves;
const
  TfwExePath = 'C:\MSE\TFW\TFW.exe';
  procedure CheckFieldUniquelyOwnedBy(Reader: TRsmReader;
    const AFieldName, AExpectedRecord: String);
  var
    Hits: TArray<Integer>;
  begin
    Hits := Reader.FindRecordsByMemberName(AFieldName);
    Assert.AreEqual(Integer(1), Integer(Length(Hits)),
      Format('FindRecordsByMemberName(%s) must return exactly one ' +
             'record, got %d. Ambiguity here would force the dotted-' +
             'walk to give up on the name-based fallback.',
        [AFieldName, Length(Hits)]));
    Assert.AreEqual(AExpectedRecord, Reader.Classes[Hits[0]].Name,
      Format('Field %s is owned by the wrong record -- expected %s.',
        [AFieldName, AExpectedRecord]));
  end;
var
  Reader: TRsmReader;
begin
  if not TFile.Exists(TfwExePath) then
  begin
    var SkipMsg: String := Format(
      'WARNING: TFW global-record-resolution test SKIPPED -- fixture ' +
      'not found at "%s". The interface-scope global encoding cannot ' +
      'be exercised without it.', [TfwExePath]);
    Writeln(SkipMsg);
    Assert.Pass(SkipMsg);
    Exit;
  end;

  Reader := TRsmReader.Create;
  try
    Reader.LoadFromFile(TfwExePath);

    // Pinpoint diagnostic: surface each upstream layer's state
    // before the assertions, so a regression points at the broken
    // layer instead of just "lookup failed".
    var DumpCls: Integer;
    DumpCls := Reader.FindClassByName('TAppCaps');
    Writeln(Format('  tfw-global: TAppCaps    in FClasses idx=%d members=%d',
      [DumpCls, Reader.Classes[DumpCls].Members.Count]));
    Writeln(Format('  tfw-global: TAppCaps    TypeIdx=0x%x (record-name file offset)',
      [Reader.Classes[DumpCls].TypeIdx]));
    // Count how many TAppCaps occurrences are in FClasses -- if more
    // than one, the scanner may have picked the wrong layout.
    var TAppCapsCount: Integer := 0;
    for var Q: Integer := 0 to Reader.Classes.Count - 1 do
      if SameText(Reader.Classes[Q].Name, 'TAppCaps') then
        Inc(TAppCapsCount);
    Writeln(Format('  tfw-global: TAppCaps    occurrences in FClasses=%d',
      [TAppCapsCount]));
    for var M: Integer := 0 to Reader.Classes[DumpCls].Members.Count - 1 do
      Writeln(Format('    [%d] off=%d (0x%x) name=%s',
        [M, Reader.Classes[DumpCls].Members[M].Offset,
         Reader.Classes[DumpCls].Members[M].Offset,
         Reader.Classes[DumpCls].Members[M].Name]));
    Writeln(Format('  tfw-global: TKonsApl    in FClasses idx=%d',
      [Reader.FindClassByName('TKonsApl')]));
    Writeln(Format('  tfw-global: TMdt        in FClasses idx=%d',
      [Reader.FindClassByName('TMdt')]));
    Writeln(Format('  tfw-global: AppCaps             FindGlobalTypeIdx=0x%x',
      [Reader.FindGlobalTypeIdx('AppCaps')]));
    Writeln(Format('  tfw-global: GlobalKonsAplPortal FindGlobalTypeIdx=0x%x',
      [Reader.FindGlobalTypeIdx('GlobalKonsAplPortal')]));
    Writeln(Format('  tfw-global: MdtGlobal           FindGlobalTypeIdx=0x%x',
      [Reader.FindGlobalTypeIdx('MdtGlobal')]));

    // The interface-scope record types must land in FClasses,
    // which is gated on the structural-anchor field scanner
    // accepting non-F-prefixed field names like DbKindName.
    Assert.IsTrue(Reader.FindClassByName('TAppCaps') >= 0,
      'TAppCaps must be discovered as a record');
    Assert.IsTrue(Reader.FindClassByName('TKonsApl') >= 0,
      'TKonsApl must be discovered as a record');
    Assert.IsTrue(Reader.FindClassByName('TMdt') >= 0,
      'TMdt must be discovered as a record');

    // The dotted-walk's name-based fallback resolves
    // GlobalName.FieldName by searching every record for the
    // field name. The agent's repro used AppCaps.DbKindName --
    // that's the structural anchor we verify here.
    CheckFieldUniquelyOwnedBy(Reader, 'DbKindName', 'TAppCaps');

    // FindBestRecordForGlobalAndField is the actual resolver the
    // dotted-walk calls when the encoded-type-id path misses (which
    // is the entire TFW case). Each line below matches a row of the
    // agent's repro recipe: "evaluate <GlobalName>.<FieldName>"
    // must succeed, and "succeed" starts with picking the correct
    // record type here. AplMsgTable (Boolean inside TAppCaps), Id
    // (Word inside TMdt, also lives on many other records), and
    // Match (ShortString on TMdt) are the three shapes that have
    // to all hit the right record despite Id/Match being common
    // field names across the codebase.
    var Idx: Integer;
    Idx := Reader.FindBestRecordForGlobalAndField('AppCaps', 'DbKindName');
    Assert.IsTrue(Idx >= 0,
      'AppCaps.DbKindName must resolve via FindBestRecordForGlobalAndField');
    Assert.AreEqual('TAppCaps', Reader.Classes[Idx].Name,
      'AppCaps.DbKindName resolved to wrong record');

    Idx := Reader.FindBestRecordForGlobalAndField('AppCaps', 'AplMsgTable');
    Assert.IsTrue(Idx >= 0,
      'AppCaps.AplMsgTable must resolve');
    Assert.AreEqual('TAppCaps', Reader.Classes[Idx].Name,
      'AppCaps.AplMsgTable resolved to wrong record');

    Idx := Reader.FindBestRecordForGlobalAndField('MdtGlobal', 'Id');
    Assert.IsTrue(Idx >= 0,
      'MdtGlobal.Id must resolve -- proximity fallback should pick TMdt');
    Assert.AreEqual('TMdt', Reader.Classes[Idx].Name,
      'MdtGlobal.Id resolved to wrong record (Id is shared across many ' +
      'records, so this is the proximity disambiguator at work)');

    Idx := Reader.FindBestRecordForGlobalAndField('MdtGlobal', 'Match');
    Assert.IsTrue(Idx >= 0,
      'MdtGlobal.Match must resolve');
    Assert.AreEqual('TMdt', Reader.Classes[Idx].Name,
      'MdtGlobal.Match resolved to wrong record');

    Idx := Reader.FindBestRecordForGlobalAndField('GlobalKonsAplPortal', 'Name');
    Assert.IsTrue(Idx >= 0,
      'GlobalKonsAplPortal.Name must resolve via proximity (the T-prefix ' +
      'hint does not apply -- the type is TKonsApl, not ' +
      'TGlobalKonsAplPortal)');
    Assert.AreEqual('TKonsApl', Reader.Classes[Idx].Name,
      'GlobalKonsAplPortal.Name resolved to wrong record');

    // End-to-end address-resolution check: the actual user-facing bug
    // was that GetAddressFromSymbol('AppCaps') returned the .itext VA
    // of the unit-init proc "Base.AppCaps.Base.AppCaps" instead of
    // the .bss VA of "Business.Root.AppCaps", because both share the
    // suffix "AppCaps" and the simple-name index used to pick the
    // first encountered. So `evaluate AppCaps.AnyField` read from
    // .text and returned x86 garbage. The TMapFileParser fix
    // (prefer DATA over CODE on suffix collisions) makes the
    // simple-name lookup return the .bss VA. Verify by checking
    // the resolved VA matches Business.Root.AppCaps's qualified
    // VA, not Base.AppCaps.Base.AppCaps's.
    var TfwMap: TMapFileParser;
    TfwMap := TMapFileParser.Create('C:\MSE\TFW\TFW.map');
    try
      var Bss: UInt64 :=
        TfwMap.VAFromUnitAndProcName('Business.Root', 'AppCaps');
      var Itext: UInt64 :=
        TfwMap.VAFromUnitAndProcName('Base.AppCaps', 'Base.AppCaps');
      var Simple: UInt64 := TfwMap.VAFromSimpleName('AppCaps');
      Writeln(Format('  tfw-global: VA(Business.Root.AppCaps)=0x%x',  [Bss]));
      Writeln(Format('  tfw-global: VA(Base.AppCaps.Base.AppCaps)=0x%x', [Itext]));
      Writeln(Format('  tfw-global: VA(simple AppCaps)=0x%x',         [Simple]));
      Assert.AreEqual(Bss, Simple,
        'After the prefer-DATA tie-break, VAFromSimpleName(''AppCaps'') ' +
        'must return the Business.Root.AppCaps .bss VA, not the ' +
        'Base.AppCaps.Base.AppCaps .itext VA');
      Assert.AreNotEqual(Itext, Simple,
        'Simple-name lookup must NOT return the .itext unit-init ' +
        'proc VA -- that was the user-facing bug.');
    finally
      TfwMap.Free;
    end;
  finally
    Reader.Free;
  end;
end;

/// <summary>
///   Developer-machine-only perf-regression test for the RSM
///   load path against a real, multi-hundred-MB binary
///   (currently <c>TFW.exe</c> ~117 MB / <c>TFW.rsm</c> ~800 MB).
///   Records each parsing phase's wall-clock time both in the
///   DUnitX runner output and in <c>dpt.log</c> next to the test
///   exe, then asserts a total wall-time budget plus a per-phase
///   ceiling so that any return of the O(N^2) loops we removed
///   trips here with a specific message.
/// </summary>
/// <remarks>
///   When the fixture is absent, the test prints a visible
///   <c>WARNING</c> line to stdout and calls <c>Assert.Pass</c>
///   so the build stays green but the missing perf-net is
///   impossible to miss in the runner output. When a fixture
///   IS present but is much smaller than the calibrated TFW
///   baseline, the identity sanity-block (<c>.rsm</c> size,
///   proc count, class count) trips first instead of silently
///   passing every budget on a too-easy fixture.
/// </remarks>
procedure TRsmReaderLegacyTests.TestTfwLoadDiagnostic;
const
  TfwExePath = 'C:\MSE\TFW\TFW.exe';
  // Total budget: the post-fix load is ~24 s on the developer
  // machine. Allow 3x for CI / cold-cache / slower disks but trip
  // well below the 425 s pre-fix value. A regression past this
  // will get caught at the next test run instead of surfacing as
  // a frozen MCP session minutes after deploy.
  HardLimit  = 90 * 1000;
  // Per-phase budgets, picked so each O(N^2) regression we just
  // killed (insertion-sort on 200k procs, per-byte String alloc
  // in the struct scan) trips its own assertion with a clear
  // message. Each is roughly 5-10x the observed post-fix value.
  // Names must match the strings emitted by OnPhase in
  // TRsmReader (ScanSymbolStream / RecomputeProcSizes /
  // DiscoverAndParseAllStructs / LinkMemberTypeIdsFromFormatA /
  // DeriveClassParents); a typo here silently passes.
  PhaseBudget_DiscoverAndParseAllStructs   = 45 * 1000;
  PhaseBudget_RecomputeProcSizes           = 20 * 1000;
  PhaseBudget_LinkMemberTypeIdsFromFormatA = 25 * 1000;
  PhaseBudget_DeriveClassParents           = 30 * 1000;
  // ScanSymbolStream now also processes $25 enum-constant records
  // (both program-local $0A and cross-unit $8A forms) and $2A
  // type-registry entries inline for the cross-unit enum alias
  // linking. Each adds modest per-byte work that on TFW-class
  // binaries (800MB+ RSM) lifts the phase from ~7s baseline to
  // ~13s; raise the budget to 20s so the test still catches a
  // genuine O(N^2) regression while accepting the new feature
  // cost.
  PhaseBudget_ScanSymbolStream             = 20 * 1000;
var
  Reader      : TRsmReader;
  TotalSW     : TStopwatch;
  PhaseSW     : TStopwatch;
  PhaseTable  : TStringList;
  PhaseTimings: IKeyValue<String, Int64>;
  LogPath     : String;
  W           : TStreamWriter;
  I           : Integer;
begin
  if not TFile.Exists(TfwExePath) then
  begin
    // Visible skip notice on stdout. Assert.Pass alone is invisible
    // in most DUnitX runner outputs (it just records "Passed"); a
    // Writeln guarantees the maintainer sees that the
    // perf-regression net is currently down on this machine and
    // can decide whether to point the test elsewhere.
    var SkipMsg: String := Format(
      'WARNING: large-RSM perf-regression test SKIPPED -- fixture not ' +
      'found at "%s". The hot-path budgets below cannot trip without ' +
      'it; restore the file or update TfwExePath.',
      [TfwExePath]);
    Writeln(SkipMsg);
    Assert.Pass(SkipMsg);
    Exit;
  end;

  // Announce which fixture we're about to drive through the reader,
  // before the multi-second load starts. The information also goes
  // into the per-run summary further down so dpt.log retains it for
  // post-mortem comparison across runs (different .rsm sizes
  // explain different timings).
  var FixtureRsm: String := ChangeFileExt(TfwExePath, '.rsm');
  var FixtureRsmMB: Double := 0;
  if TFile.Exists(FixtureRsm) then
    FixtureRsmMB := TFile.GetSize(FixtureRsm) / (1024 * 1024);
  var FixtureLine: String := Format('  fixture: exe=%s  rsm=%s (%.1f MB)',
    [TfwExePath, FixtureRsm, FixtureRsmMB]);
  Writeln(FixtureLine);

  // Sanity-check the fixture identity before we trust the perf
  // budgets below. A "TFW.exe" pointing at a hello-world build
  // would silently pass every budget because its sub-MB .rsm
  // takes ~0 ms in every phase -- a useless green light. Require
  // a large sidecar AND a six-figure proc count so a wrong
  // fixture trips here with a clear message instead.
  Assert.IsTrue(FixtureRsmMB > 50,
    Format('Fixture .rsm is suspiciously small (%.1f MB). The ' +
           'perf-regression test needs a multi-hundred-MB sidecar ' +
           'to exercise the bulk-scan hot paths; the budgets below ' +
           'are meaningless on a tiny fixture.', [FixtureRsmMB]));

  PhaseTable := TStringList.Create;
  PhaseTimings := Collections.NewPlainKeyValue<String, Int64>;
  Reader := TRsmReader.Create;
  try
    PhaseSW := TStopwatch.StartNew;
    Reader.OnPhase :=
      procedure(APhase: String)
      begin
        PhaseSW.Stop;
        PhaseTable.Add(Format('  %-32s %d ms',
          [APhase, PhaseSW.ElapsedMilliseconds]));
        PhaseTimings[APhase] := PhaseSW.ElapsedMilliseconds;
        PhaseSW := TStopwatch.StartNew;
      end;
    TotalSW := TStopwatch.StartNew;
    Reader.LoadFromFile(TfwExePath);
    TotalSW.Stop;

    // Mirror the timings into dpt.log so the file the user already
    // tails for live progress also carries the post-mortem summary.
    var Summary: String := Format('%s [tfw-diag] total=%d ms  procs=%d  classes=%d',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
       Integer(TotalSW.ElapsedMilliseconds), Reader.Procs.Count, Reader.Classes.Count]);
    LogPath := ChangeFileExt(ParamStr(0), '') + '.log';
    try
      W := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
      try
        W.WriteLine(Summary);
        W.WriteLine(FixtureLine);
        for I := 0 to PhaseTable.Count - 1 do
          W.WriteLine(PhaseTable[I]);
      finally
        W.Free;
      end;
    except
      // diagnostics must never throw
    end;

    // Spot-check the decoder on a handful of named procs from TFW.
    // The MCP-session diagnostic showed FindProcContaining missing
    // for the user's breakpoint, so we cross-reference what the
    // reader actually stored against the addresses the .map gives
    // for the same procs. Mismatch or SegmentOffset = 0 here proves
    // the decoder still drops them; a correct value indicates a
    // downstream issue (size table, lookup, sort).
    var Spot: TArray<String> := ['TFormMain.Create', 'TFormVBh.Create',
                                 'TFormVBh.CreateGsVBhBridge',
                                 'TFormMain.AfterMenuRebuild'];
    var SpotLine: String;
    for var Sp in Spot do
    begin
      var Pi := Reader.FindProcByName(Sp);
      if Pi >= 0 then
      begin
        var SpProc := Reader.Procs[Pi];
        SpotLine := Format('  spot: %-36s idx=%d  seg=0x%x  size=%d  locals=%d',
          [Sp, Pi, SpProc.SegmentOffset, SpProc.Size, SpProc.Locals.Count]);
        Writeln(SpotLine);
        try
          W := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
          try W.WriteLine(SpotLine); finally W.Free; end;
        except end;
        for var Li := 0 to SpProc.Locals.Count - 1 do
        begin
          var L := SpProc.Locals[Li];
          var KindStr: String;
          case L.Kind of
            lkBpRel:    KindStr := 'bp';
            lkRegister: KindStr := 'reg';
          else
            KindStr := '?';
          end;
          SpotLine := Format('    local[%d] %-12s kind=%s  bp=%d  reg=%d  typeIdx=0x%x',
            [Li, L.Name, KindStr, L.BpOffset, L.RegParamIdx, L.TypeIdx]);
          Writeln(SpotLine);
          try
            W := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
            try W.WriteLine(SpotLine); finally W.Free; end;
          except end;
        end;
      end
      else
      begin
        SpotLine := Format('  spot: %-36s <not in reader>', [Sp]);
        Writeln(SpotLine);
        try
          W := TStreamWriter.Create(LogPath, True, TEncoding.UTF8);
          try W.WriteLine(SpotLine); finally W.Free; end;
        except end;
      end;
    end;

    // Also echo to stdout so DUnitX's runner output captures it.
    Writeln(Summary);
    Writeln(FixtureLine);
    for I := 0 to PhaseTable.Count - 1 do
      Writeln(PhaseTable[I]);

    Assert.IsTrue(TotalSW.ElapsedMilliseconds < HardLimit,
      Format('RSM load on TFW exceeded %d ms (took %d ms) -- ' +
             'check phase timings above for the stalled section.',
        [HardLimit, TotalSW.ElapsedMilliseconds]));
    // Identity sanity-check (continued from the .rsm-size guard at
    // the top): a TFW-class fixture must surface six-figure proc
    // and four-figure class counts. The previous "> 1000" gate
    // would have happily accepted a much smaller binary as the
    // fixture, masking the fact that the perf budgets are no
    // longer exercising the bulk-scan paths they were tuned for.
    Assert.IsTrue(Reader.Procs.Count > 100000,
      Format('Fixture produced only %d procs; expected >100k. ' +
             'Likely pointing at a much smaller binary than the ' +
             'TFW-class fixture this test is calibrated for.',
        [Reader.Procs.Count]));
    Assert.IsTrue(Reader.Classes.Count > 1000,
      Format('Fixture produced only %d classes; expected >1000.',
        [Reader.Classes.Count]));

    // Per-phase regression guards. Each budget is well above the
    // post-fix value (~0.5-12 s) but far below the broken-state
    // value (~180-200 s) so a future O(N^2) creeping back into
    // any one phase trips here with a specific failure message
    // rather than a generic "load too slow". Parallel arrays
    // pair name and budget by index -- a small struct would be
    // tidier, but Delphi forbids declaring local record types
    // inline inside a procedure body.
    var BudgetNames: TArray<String> := [
      'ScanSymbolStream',
      'RecomputeProcSizes',
      'DiscoverAndParseAllStructs',
      'LinkMemberTypeIdsFromFormatA',
      'DeriveClassParents'
    ];
    var BudgetMs: TArray<Int64> := [
      PhaseBudget_ScanSymbolStream,
      PhaseBudget_RecomputeProcSizes,
      PhaseBudget_DiscoverAndParseAllStructs,
      PhaseBudget_LinkMemberTypeIdsFromFormatA,
      PhaseBudget_DeriveClassParents
    ];
    var PhaseMs: Int64 := 0;
    for var Bi := 0 to High(BudgetNames) do
    begin
      Assert.IsTrue(PhaseTimings.TryGetValue(BudgetNames[Bi], PhaseMs),
        Format('Phase "%s" not reported by the reader -- the OnPhase ' +
               'name may have been renamed; update this test to match.',
          [BudgetNames[Bi]]));
      Assert.IsTrue(PhaseMs < BudgetMs[Bi],
        Format('Phase "%s" exceeded budget: took %d ms (budget %d ms). ' +
               'A perf regression likely reintroduced an O(N^2) loop -- ' +
               'inspect the phase against its post-fix baseline.',
          [BudgetNames[Bi], PhaseMs, BudgetMs[Bi]]));
    end;
  finally
    Reader.Free;
    PhaseTable.Free;
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

initialization
  TDUnitX.RegisterTestFixture(TRsmReaderLegacyTests);

end.
