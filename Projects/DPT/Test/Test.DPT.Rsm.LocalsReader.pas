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
    procedure TestParsesClassMembers64;
    [Test]
    procedure TestParsesRecordMembers64;
    [Test]
    procedure TestRecordTypeIdxRoundTrip64;
    {$ENDIF}
  end;

implementation

uses
  System.Classes;

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
begin
  Reader := TRsmLocalsReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(AUse64Bit));
    ProcIdx := Reader.FindProcByName('LocalsProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'LocalsProcedure not found');
    Proc := Reader.Procs[ProcIdx];

    SetLength(Seen, 0);
    for I := 0 to Proc.Locals.Count - 1 do
      if Pos('Local', Proc.Locals[I].Name) = 1 then
      begin
        for J := 0 to High(Seen) do
          Assert.AreNotEqual(Seen[J], Proc.Locals[I].BpOffset,
            Format('Two locals share BP offset %d', [Proc.Locals[I].BpOffset]));
        SetLength(Seen, Length(Seen) + 1);
        Seen[High(Seen)] := Proc.Locals[I].BpOffset;
      end;
    Assert.IsTrue(Length(Seen) >= 3,
      Format('Expected at least 3 LocalA/B/C entries, got %d', [Length(Seen)]));
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

procedure TRsmLocalsReaderTests.TestFindProcContaining32;
var
  Reader : TRsmLocalsReader;
  ProcIdx: Integer;
  Proc   : TRsmProc;
begin
  Reader := TRsmLocalsReader.Create;
  try
    Reader.LoadFromFile(ResolveExePath(False));
    ProcIdx := Reader.FindProcByName('LocalsProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'LocalsProcedure not found');
    Proc := Reader.Procs[ProcIdx];

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

{$IFDEF CPUX64}
procedure TRsmLocalsReaderTests.TestParsesProcedures64;                 begin DoTestParsesProcedures(True);                  end;
procedure TRsmLocalsReaderTests.TestParsesLocalsForLocalsProcedure64;   begin DoTestParsesLocalsForLocalsProcedure(True);    end;
procedure TRsmLocalsReaderTests.TestLocalsHaveDistinctOffsets64;        begin DoTestLocalsHaveDistinctOffsets(True);         end;
procedure TRsmLocalsReaderTests.TestFindProcByName64;                   begin DoTestFindProcByName(True);                    end;
procedure TRsmLocalsReaderTests.TestParsesClassMembers64;               begin DoTestParsesClassMembers(True);                end;
procedure TRsmLocalsReaderTests.TestParsesRecordMembers64;              begin DoTestParsesRecordMembers(True);               end;
procedure TRsmLocalsReaderTests.TestRecordTypeIdxRoundTrip64;           begin DoTestRecordTypeIdxRoundTrip(True);            end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TRsmLocalsReaderTests);

end.
