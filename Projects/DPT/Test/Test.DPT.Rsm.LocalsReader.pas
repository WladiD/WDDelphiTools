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

  DPT.Rsm.LocalsReader;

type

  [TestFixture]
  TRsmLocalsReaderTests = class
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
    procedure TestEdgeCaseLocalsAllDecoded32;
    [Test]
    procedure TestOpenArrayParamRecognized32;
    [Test]
    procedure TestTfwLoadDiagnostic;
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

function TRsmLocalsReaderTests.ResolveExePath(AUse64Bit: Boolean): String;
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

function TRsmLocalsReaderTests.ResolveRsmPath(AUse64Bit: Boolean): String;
begin
  Result := ChangeFileExt(ResolveExePath(AUse64Bit), '.rsm');
end;

procedure TRsmLocalsReaderTests.TestRsmFilePresent;
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

procedure TRsmLocalsReaderTests.TestRsmStartsWithCsh7Magic;
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

procedure TRsmLocalsReaderTests.TestLoadFromMissingFileLeavesEmpty;
var
  Reader: TRsmLocalsReader;
begin
  // Loading a non-existent EXE path must succeed silently (no .rsm
  // sidecar -> empty reader). The TD32 reader raises on a missing
  // file because it reads the EXE itself; the RSM reader skips
  // gracefully when the sidecar is absent.
  Reader := TRsmLocalsReader.Create;
  try
    Reader.LoadFromFile('Z:\does-not-exist\nope.exe');
    Assert.IsTrue(Reader.Procs.Count = 0);
    Assert.IsTrue(Reader.Classes.Count = 0);
  finally
    Reader.Free;
  end;
end;

procedure TRsmLocalsReaderTests.TestLoadFromBytesWithoutSignatureLeavesEmpty;
var
  Buf   : TBytes;
  Reader: TRsmLocalsReader;
begin
  SetLength(Buf, 1024);
  FillChar(Buf[0], Length(Buf), 0);
  Buf[0] := Ord('M');
  Buf[1] := Ord('Z');
  Reader := TRsmLocalsReader.Create;
  try
    Reader.LoadFromBytes(Buf);
    Assert.IsTrue(Reader.Procs.Count = 0);
    Assert.IsTrue(Reader.Classes.Count = 0);
  finally
    Reader.Free;
  end;
end;

procedure TRsmLocalsReaderTests.DoTestParsesProcedures(AUse64Bit: Boolean);
var
  Reader: TRsmLocalsReader;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.DoTestParsesLocalsForLocalsProcedure(AUse64Bit: Boolean);
var
  Reader   : TRsmLocalsReader;
  ProcIdx  : Integer;
  Proc     : TRsmProc;
  HasA     : Boolean;
  HasB     : Boolean;
  HasC     : Boolean;
  I        : Integer;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.DoTestLocalsHaveDistinctOffsets(AUse64Bit: Boolean);
var
  Reader : TRsmLocalsReader;
  ProcIdx: Integer;
  Proc   : TRsmProc;
  Seen   : TArray<Int32>;
  I, J   : Integer;
  RealOffsets, SynthOffsets: Integer;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.DoTestFindProcByName(AUse64Bit: Boolean);
var
  Reader: TRsmLocalsReader;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.DoTestParsesClassMembers(AUse64Bit: Boolean);
var
  Reader     : TRsmLocalsReader;
  Member     : TRsmClassMember;
  ExpectedPtr: UInt32;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.DoTestParsesRecordMembers(AUse64Bit: Boolean);
var
  Reader  : TRsmLocalsReader;
  Member  : TRsmClassMember;
  PointIdx: Integer;
  RectIdx : Integer;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.DoTestRecordTypeIdxRoundTrip(AUse64Bit: Boolean);
var
  Reader  : TRsmLocalsReader;
  PointIdx: Integer;
  TypeIdx : UInt32;
  Member  : TRsmClassMember;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.DoTestClassFieldTypeIdxLinking(AUse64Bit: Boolean);
var
  Reader        : TRsmLocalsReader;
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
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.TestClassFieldTypeIdxLinking32;          begin DoTestClassFieldTypeIdxLinking(False);         end;
{$IFDEF CPUX64}
procedure TRsmLocalsReaderTests.TestClassFieldTypeIdxLinking64;          begin DoTestClassFieldTypeIdxLinking(True);          end;
{$ENDIF}

procedure TRsmLocalsReaderTests.TestParsesProcedures32;                 begin DoTestParsesProcedures(False);                 end;
procedure TRsmLocalsReaderTests.TestParsesLocalsForLocalsProcedure32;   begin DoTestParsesLocalsForLocalsProcedure(False);   end;
procedure TRsmLocalsReaderTests.TestLocalsHaveDistinctOffsets32;        begin DoTestLocalsHaveDistinctOffsets(False);        end;
procedure TRsmLocalsReaderTests.TestFindProcByName32;                   begin DoTestFindProcByName(False);                   end;
procedure TRsmLocalsReaderTests.TestParsesClassMembers32;               begin DoTestParsesClassMembers(False);               end;
procedure TRsmLocalsReaderTests.TestParsesRecordMembers32;              begin DoTestParsesRecordMembers(False);              end;
procedure TRsmLocalsReaderTests.TestRecordTypeIdxRoundTrip32;           begin DoTestRecordTypeIdxRoundTrip(False);           end;

procedure TRsmLocalsReaderTests.DoTestFindProcContaining(AUse64Bit: Boolean);
var
  Reader : TRsmLocalsReader;
  ProcIdx: Integer;
  Proc   : TRsmProc;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.TestFindProcContaining32;                begin DoTestFindProcContaining(False);               end;

procedure TRsmLocalsReaderTests.DoTestDiscoversDerivedClass(AUse64Bit: Boolean);
var
  Reader     : TRsmLocalsReader;
  Member     : TRsmClassMember;
  ExpectedPtr: UInt32;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.DoTestDerivedInheritsBaseFields(AUse64Bit: Boolean);
var
  Reader     : TRsmLocalsReader;
  Member     : TRsmClassMember;
  ExpectedPtr: UInt32;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.DoTestEdgeCaseLocalsAllDecoded(AUse64Bit: Boolean);
// Exercises BPRel decoding against a deliberately diverse set of
// local types (Variant, IInterface, dynamic array, record, Double,
// Boolean, Char, set, pointer, class). Every such local must
// produce a decoded BpOffset within the plausible BP-relative
// window for that proc; if the decoder doesn't recognize an
// encoding form it falls back to a synthesized far-below-zero
// value, which this test fails on so the gap surfaces.
var
  Reader : TRsmLocalsReader;
  ProcIdx: Integer;
  Proc   : TRsmProc;
  I      : Integer;
  Synth  : Integer;
  Names  : String;
begin
  Reader := TRsmLocalsReader.Create;
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
procedure TRsmLocalsReaderTests.DoTestOpenArrayParamRecognized(AUse64Bit: Boolean);
var
  Reader     : TRsmLocalsReader;
  ProcIdx, I : Integer;
  Proc       : TRsmProc;
  Found      : Boolean;
begin
  Reader := TRsmLocalsReader.Create;
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

procedure TRsmLocalsReaderTests.TestDiscoversDerivedClass32;             begin DoTestDiscoversDerivedClass(False);            end;
procedure TRsmLocalsReaderTests.TestDerivedInheritsBaseFields32;         begin DoTestDerivedInheritsBaseFields(False);        end;
procedure TRsmLocalsReaderTests.TestEdgeCaseLocalsAllDecoded32;          begin DoTestEdgeCaseLocalsAllDecoded(False);         end;
procedure TRsmLocalsReaderTests.TestOpenArrayParamRecognized32;          begin DoTestOpenArrayParamRecognized(False);         end;

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
procedure TRsmLocalsReaderTests.TestTfwLoadDiagnostic;
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
  // TRsmLocalsReader (ScanSymbolStream / RecomputeProcSizes /
  // DiscoverAndParseAllStructs / LinkMemberTypeIdsFromFormatA /
  // DeriveClassParents); a typo here silently passes.
  PhaseBudget_DiscoverAndParseAllStructs   = 30 * 1000;
  PhaseBudget_RecomputeProcSizes           = 20 * 1000;
  PhaseBudget_LinkMemberTypeIdsFromFormatA = 25 * 1000;
  PhaseBudget_DeriveClassParents           = 30 * 1000;
  PhaseBudget_ScanSymbolStream             = 10 * 1000;
var
  Reader      : TRsmLocalsReader;
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
  Reader := TRsmLocalsReader.Create;
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
procedure TRsmLocalsReaderTests.TestParsesProcedures64;                 begin DoTestParsesProcedures(True);                  end;
procedure TRsmLocalsReaderTests.TestParsesLocalsForLocalsProcedure64;   begin DoTestParsesLocalsForLocalsProcedure(True);    end;
procedure TRsmLocalsReaderTests.TestLocalsHaveDistinctOffsets64;        begin DoTestLocalsHaveDistinctOffsets(True);         end;
procedure TRsmLocalsReaderTests.TestFindProcByName64;                   begin DoTestFindProcByName(True);                    end;
procedure TRsmLocalsReaderTests.TestFindProcContaining64;               begin DoTestFindProcContaining(True);                end;
procedure TRsmLocalsReaderTests.TestParsesClassMembers64;               begin DoTestParsesClassMembers(True);                end;
procedure TRsmLocalsReaderTests.TestParsesRecordMembers64;              begin DoTestParsesRecordMembers(True);               end;
procedure TRsmLocalsReaderTests.TestRecordTypeIdxRoundTrip64;           begin DoTestRecordTypeIdxRoundTrip(True);            end;
procedure TRsmLocalsReaderTests.TestDiscoversDerivedClass64;            begin DoTestDiscoversDerivedClass(True);             end;
procedure TRsmLocalsReaderTests.TestDerivedInheritsBaseFields64;        begin DoTestDerivedInheritsBaseFields(True);         end;
procedure TRsmLocalsReaderTests.TestEdgeCaseLocalsAllDecoded64;         begin DoTestEdgeCaseLocalsAllDecoded(True);          end;
procedure TRsmLocalsReaderTests.TestOpenArrayParamRecognized64;         begin DoTestOpenArrayParamRecognized(True);          end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TRsmLocalsReaderTests);

end.
