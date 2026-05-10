// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Td32.LocalsReader;

interface

uses

  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  DPT.Td32.LocalsReader;

type

  [TestFixture]
  TTd32LocalsReaderTests = class
  private
    function ResolveRsmPath(AUse64Bit: Boolean): String;
    procedure DoTestParsesProcedures(AUse64Bit: Boolean);
    procedure DoTestParsesLocalsForLocalsProcedure(AUse64Bit: Boolean);
    procedure DoTestParsesClassMembers(AUse64Bit: Boolean);
    procedure DoTestParsesRecordMembers(AUse64Bit: Boolean);
    procedure DoTestRecordTypeIdxRoundTrip(AUse64Bit: Boolean);
    procedure DoTestLocalsHaveDistinctOffsets(AUse64Bit: Boolean);
    procedure DoTestFindProcByName(AUse64Bit: Boolean);
  public
    [Test]
    procedure TestParsesProcedures32;
    [Test]
    procedure TestParsesLocalsForLocalsProcedure32;
    [Test]
    procedure TestLocalsHaveDistinctOffsets32;
    [Test]
    procedure TestFindProcByName32;
    [Test]
    procedure TestLoadFromMissingFileLeavesEmpty;
    [Test]
    procedure TestLoadFromNonTd32FileLeavesEmpty;
    [Test]
    procedure TestLoadFromBytesWithoutSignatureLeavesEmpty;
    [Test]
    procedure TestLoadFromTooSmallBufferLeavesEmpty;
    [Test]
    procedure TestFindProcContaining32;
    [Test]
    procedure TestParsesClassMembers32;
    [Test]
    procedure TestParsesRecordMembers32;
    [Test]
    procedure TestRecordTypeIdxRoundTrip32;
    {$IFDEF CPUX64}
    [Test]
    procedure TestParsesClassMembers64;
    [Test]
    procedure TestParsesRecordMembers64;
    [Test]
    procedure TestRecordTypeIdxRoundTrip64;
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
    {$ENDIF}
  end;

implementation

function TTd32LocalsReaderTests.ResolveRsmPath(AUse64Bit: Boolean): String;
var
  Sub: String;
begin
  // The reader extracts TD32 from the EXE itself (not the .rsm sidecar);
  // the sidecar uses a different CSH7 container format. Method name kept
  // for backwards compatibility within this test fixture.
  if AUse64Bit then
    Sub := 'Win64'
  else
    Sub := 'Win32';
  Result := ExpandFileName('Projects\DPT\Test\' + Sub + '\DebugTarget.exe');
  if not TFile.Exists(Result) then
    Result := ExpandFileName(Sub + '\DebugTarget.exe');
end;

procedure TTd32LocalsReaderTests.DoTestParsesProcedures(AUse64Bit: Boolean);
var
  Reader  : TTd32LocalsReader;
  ExePath : String;
begin
  ExePath := ResolveRsmPath(AUse64Bit);
  Assert.IsTrue(TFile.Exists(ExePath), 'EXE fixture missing: ' + ExePath);

  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromFile(ExePath);
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

procedure TTd32LocalsReaderTests.DoTestParsesLocalsForLocalsProcedure(AUse64Bit: Boolean);
var
  Reader   : TTd32LocalsReader;
  ProcIdx  : Integer;
  Proc     : TTd32Proc;
  HasA     : Boolean;
  HasB     : Boolean;
  HasC     : Boolean;
  I        : Integer;
begin
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromFile(ResolveRsmPath(AUse64Bit));

    ProcIdx := Reader.FindProcByName('LocalsProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'LocalsProcedure not found');

    Proc := Reader.Procs[ProcIdx];
    Assert.IsTrue(Proc.Locals.Count >= 3,
      Format('Expected at least 3 locals in LocalsProcedure, got %d', [Proc.Locals.Count]));

    HasA := False;
    HasB := False;
    HasC := False;
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

procedure TTd32LocalsReaderTests.DoTestLocalsHaveDistinctOffsets(AUse64Bit: Boolean);
var
  Reader   : TTd32LocalsReader;
  ProcIdx  : Integer;
  Proc     : TTd32Proc;
  Seen     : TArray<Int32>;
  I, J     : Integer;
begin
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromFile(ResolveRsmPath(AUse64Bit));
    ProcIdx := Reader.FindProcByName('LocalsProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'LocalsProcedure not found');
    Proc := Reader.Procs[ProcIdx];

    SetLength(Seen, 0);
    for I := 0 to Proc.Locals.Count - 1 do
      if Pos('Local', Proc.Locals[I].Name) = 1 then
      begin
        for J := 0 to High(Seen) do
          Assert.AreNotEqual(Seen[J], Proc.Locals[I].BpOffset,
            Format('Two locals share BP offset %d in LocalsProcedure', [Proc.Locals[I].BpOffset]));
        SetLength(Seen, Length(Seen) + 1);
        Seen[High(Seen)] := Proc.Locals[I].BpOffset;
      end;
    Assert.IsTrue(Length(Seen) >= 3,
      Format('Expected at least 3 LocalA/B/C entries, got %d', [Length(Seen)]));
  finally
    Reader.Free;
  end;
end;

procedure TTd32LocalsReaderTests.DoTestFindProcByName(AUse64Bit: Boolean);
var
  Reader: TTd32LocalsReader;
begin
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromFile(ResolveRsmPath(AUse64Bit));
    // Case-insensitive lookup must succeed.
    Assert.IsTrue(Reader.FindProcByName('localsprocedure') >= 0,
      'FindProcByName must be case-insensitive');
    // Lookup of a non-existent name must return -1.
    Assert.AreEqual(-1, Reader.FindProcByName('ThisProcedureDoesNotExist__xyz'),
      'Unknown name must return -1');
  finally
    Reader.Free;
  end;
end;

procedure TTd32LocalsReaderTests.DoTestParsesClassMembers(AUse64Bit: Boolean);
var
  Reader     : TTd32LocalsReader;
  Member     : TTd32ClassMember;
  ExpectedPtr: UInt32;
begin
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromFile(ResolveRsmPath(AUse64Bit));

    // Both fixture classes must be discoverable. Note that the x64 build
    // emits Itanium-mangled class names ("_ZTRN<unit><class>E"); the
    // reader demangles them to the short form before storing.
    Assert.IsTrue(Reader.FindClassByName('TInner') >= 0,
      'TInner class must be parsed from GLOBAL_TYPES');
    Assert.IsTrue(Reader.FindClassByName('TOuter') >= 0,
      'TOuter class must be parsed from GLOBAL_TYPES');

    // Pointer-sized VMT slot occupies offset 0..(SizeOf(Pointer)-1), so
    // the first declared field lives at offset = SizeOf(Pointer).
    if AUse64Bit then ExpectedPtr := 8 else ExpectedPtr := 4;

    Assert.IsTrue(Reader.FindClassMember('TInner', 'FInnerInt', Member),
      'TInner.FInnerInt must be parsed');
    Assert.AreEqual(ExpectedPtr, Member.Offset,
      'FInnerInt must come right after the VMT slot');

    Assert.IsTrue(Reader.FindClassMember('TInner', 'FInnerStr', Member),
      'TInner.FInnerStr must be parsed');
    Assert.AreEqual(ExpectedPtr * 2, Member.Offset,
      'FInnerStr must come right after FInnerInt (pointer-aligned)');

    Assert.IsTrue(Reader.FindClassMember('TOuter', 'FOuterInt', Member),
      'TOuter.FOuterInt must be parsed');
    Assert.AreEqual(ExpectedPtr, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TOuter', 'FOuterInner', Member),
      'TOuter.FOuterInner must be parsed');
    Assert.AreEqual(ExpectedPtr * 2, Member.Offset);

    Assert.IsTrue(Reader.FindClassMember('TOuter', 'FOuterStr', Member),
      'TOuter.FOuterStr must be parsed');
    Assert.AreEqual(ExpectedPtr * 3, Member.Offset);

    // Lookup must be case-insensitive on both class and field names.
    Assert.IsTrue(Reader.FindClassMember('touter', 'fouterint', Member),
      'FindClassMember must be case-insensitive');
    // Non-existent class -> False.
    Assert.IsFalse(Reader.FindClassMember('TNoSuchClass', 'X', Member));
    // Existing class, non-existent field -> False.
    Assert.IsFalse(Reader.FindClassMember('TInner', 'NoSuchField', Member));
  finally
    Reader.Free;
  end;
end;

procedure TTd32LocalsReaderTests.DoTestParsesRecordMembers(AUse64Bit: Boolean);
var
  Reader     : TTd32LocalsReader;
  Member     : TTd32ClassMember;
  PointIdx   : Integer;
  RectIdx    : Integer;
  WithRecIdx : Integer;
  RecKind    : TTd32StructKind;
begin
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromFile(ResolveRsmPath(AUse64Bit));

    // Records (LF_STRUCTURE) must be picked up by the same walker that
    // handles classes (LF_CLASS). Their Kind must be skRecord so the
    // navigator knows not to dereference.
    PointIdx := Reader.FindClassByName('TPoint2D');
    RectIdx  := Reader.FindClassByName('TRect2D');
    Assert.IsTrue(PointIdx >= 0, 'TPoint2D record must be parsed from GLOBAL_TYPES');
    Assert.IsTrue(RectIdx  >= 0, 'TRect2D record must be parsed from GLOBAL_TYPES');

    RecKind := Reader.Classes[PointIdx].Kind;
    Assert.IsTrue(RecKind = skRecord, 'TPoint2D must have Kind = skRecord');
    RecKind := Reader.Classes[RectIdx].Kind;
    Assert.IsTrue(RecKind = skRecord, 'TRect2D must have Kind = skRecord');

    // Records have no VMT, so member offsets start at 0 (unlike classes,
    // where the VMT pointer occupies the first SizeOf(Pointer) bytes).
    Assert.IsTrue(Reader.FindClassMember('TPoint2D', 'FX', Member),
      'TPoint2D.FX must be parsed');
    Assert.AreEqual(UInt32(0), Member.Offset, 'TPoint2D.FX must live at offset 0');

    Assert.IsTrue(Reader.FindClassMember('TPoint2D', 'FY', Member),
      'TPoint2D.FY must be parsed');
    Assert.AreEqual(UInt32(4), Member.Offset, 'TPoint2D.FY must live at offset 4');

    // TRect2D nests two TPoint2D inline (each is 8 bytes on every target).
    Assert.IsTrue(Reader.FindClassMember('TRect2D', 'FTopLeft', Member),
      'TRect2D.FTopLeft must be parsed');
    Assert.AreEqual(UInt32(0), Member.Offset);
    Assert.IsTrue(Reader.FindClassMember('TRect2D', 'FBottomRight', Member),
      'TRect2D.FBottomRight must be parsed');
    Assert.AreEqual(UInt32(8), Member.Offset);

    // The class that holds the records: TWithRec. Its first record-typed
    // field FOrigin lives right after the VMT (offset SizeOf(Pointer)).
    // Subsequent fields follow inline: TPoint2D = 8 bytes, TRect2D = 16
    // bytes. Then a class field (pointer-sized) and a string (pointer).
    WithRecIdx := Reader.FindClassByName('TWithRec');
    Assert.IsTrue(WithRecIdx >= 0, 'TWithRec class must be parsed');
    Assert.IsTrue(Reader.Classes[WithRecIdx].Kind = skClass,
      'TWithRec must have Kind = skClass');

    Assert.IsTrue(Reader.FindClassMember('TWithRec', 'FOrigin', Member),
      'TWithRec.FOrigin must be parsed');
    if AUse64Bit then
      Assert.AreEqual(UInt32(8), Member.Offset, 'FOrigin must come after the x64 VMT slot')
    else
      Assert.AreEqual(UInt32(4), Member.Offset, 'FOrigin must come after the x86 VMT slot');

    Assert.IsTrue(Reader.FindClassMember('TWithRec', 'FBounds', Member),
      'TWithRec.FBounds must be parsed');
    if AUse64Bit then
      Assert.AreEqual(UInt32(16), Member.Offset, 'FBounds = VMT(8) + sizeof(TPoint2D)(8)')
    else
      Assert.AreEqual(UInt32(12), Member.Offset, 'FBounds = VMT(4) + sizeof(TPoint2D)(8)');

    // TPair is the next field: it holds an instance pointer (TInner) and
    // a string (UnicodeString = pointer-sized), so its size is 2 * pointer.
    Assert.IsTrue(Reader.FindClassMember('TWithRec', 'FPair', Member),
      'TWithRec.FPair must be parsed');
    if AUse64Bit then
      Assert.AreEqual(UInt32(32), Member.Offset, 'FPair = VMT(8) + 8 + sizeof(TRect2D)(16)')
    else
      Assert.AreEqual(UInt32(28), Member.Offset, 'FPair = VMT(4) + 8 + sizeof(TRect2D)(16)');

    Assert.IsTrue(Reader.FindClassMember('TWithRec', 'FNestedObj', Member),
      'TWithRec.FNestedObj must be parsed');
    if AUse64Bit then
      Assert.AreEqual(UInt32(48), Member.Offset, 'FNestedObj = FPair(32) + sizeof(TPair)(16) on x64')
    else
      Assert.AreEqual(UInt32(36), Member.Offset, 'FNestedObj = FPair(28) + sizeof(TPair)(8) on x86');

    // TPair record itself: FObj (pointer) at 0, FLabel (pointer) at sizeof(Pointer).
    Assert.IsTrue(Reader.FindClassByName('TPair') >= 0, 'TPair record must be parsed');
    Assert.IsTrue(Reader.Classes[Reader.FindClassByName('TPair')].Kind = skRecord,
      'TPair must have Kind = skRecord');
    Assert.IsTrue(Reader.FindClassMember('TPair', 'FObj', Member));
    Assert.AreEqual(UInt32(0), Member.Offset);
    Assert.IsTrue(Reader.FindClassMember('TPair', 'FLabel', Member));
    if AUse64Bit then
      Assert.AreEqual(UInt32(8), Member.Offset)
    else
      Assert.AreEqual(UInt32(4), Member.Offset);
  finally
    Reader.Free;
  end;
end;

procedure TTd32LocalsReaderTests.DoTestRecordTypeIdxRoundTrip(AUse64Bit: Boolean);
var
  Reader  : TTd32LocalsReader;
  PointIdx: Integer;
  TypeIdx : UInt32;
  Member  : TTd32ClassMember;
begin
  // The record-navigation path in EvaluateVariable looks up records by
  // their TD32 type-index (not by name), because record-typed fields
  // reference their declared type via TypeIdx in LF_MEMBER. Verify the
  // round-trip: TPoint2D's stored TypeIdx must resolve back to itself,
  // and member lookup by TypeIdx must yield the same offsets as by name.
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromFile(ResolveRsmPath(AUse64Bit));

    PointIdx := Reader.FindClassByName('TPoint2D');
    Assert.IsTrue(PointIdx >= 0);
    TypeIdx := Reader.Classes[PointIdx].TypeIdx;
    Assert.IsTrue(TypeIdx >= $1000, 'TypeIdx must be in the user range (>= $1000)');

    Assert.AreEqual(PointIdx, Reader.FindStructByTypeIdx(TypeIdx),
      'FindStructByTypeIdx must round-trip to the same record');

    Assert.IsTrue(Reader.FindStructMemberByTypeIdx(TypeIdx, 'FX', Member),
      'FindStructMemberByTypeIdx must locate TPoint2D.FX');
    Assert.AreEqual(UInt32(0), Member.Offset);
    Assert.IsTrue(Reader.FindStructMemberByTypeIdx(TypeIdx, 'FY', Member),
      'FindStructMemberByTypeIdx must locate TPoint2D.FY');
    Assert.AreEqual(UInt32(4), Member.Offset);

    // Unknown type-index must fail cleanly.
    Assert.IsFalse(Reader.FindStructMemberByTypeIdx($00FF, 'FX', Member),
      'Unknown TypeIdx must yield False');
    // Known type, unknown field, must also fail cleanly.
    Assert.IsFalse(Reader.FindStructMemberByTypeIdx(TypeIdx, 'FNoSuchField', Member),
      'Unknown field name on known TypeIdx must yield False');
  finally
    Reader.Free;
  end;
end;

procedure TTd32LocalsReaderTests.TestParsesClassMembers32;
begin
  DoTestParsesClassMembers(False);
end;

procedure TTd32LocalsReaderTests.TestParsesRecordMembers32;
begin
  DoTestParsesRecordMembers(False);
end;

procedure TTd32LocalsReaderTests.TestRecordTypeIdxRoundTrip32;
begin
  DoTestRecordTypeIdxRoundTrip(False);
end;

{$IFDEF CPUX64}
procedure TTd32LocalsReaderTests.TestParsesClassMembers64;
begin
  DoTestParsesClassMembers(True);
end;

procedure TTd32LocalsReaderTests.TestParsesRecordMembers64;
begin
  DoTestParsesRecordMembers(True);
end;

procedure TTd32LocalsReaderTests.TestRecordTypeIdxRoundTrip64;
begin
  DoTestRecordTypeIdxRoundTrip(True);
end;
{$ENDIF}

procedure TTd32LocalsReaderTests.TestParsesProcedures32;                begin DoTestParsesProcedures(False);                end;
procedure TTd32LocalsReaderTests.TestParsesLocalsForLocalsProcedure32;  begin DoTestParsesLocalsForLocalsProcedure(False);  end;
procedure TTd32LocalsReaderTests.TestLocalsHaveDistinctOffsets32;       begin DoTestLocalsHaveDistinctOffsets(False);       end;
procedure TTd32LocalsReaderTests.TestFindProcByName32;                  begin DoTestFindProcByName(False);                  end;

procedure TTd32LocalsReaderTests.TestLoadFromMissingFileLeavesEmpty;
var
  Reader: TTd32LocalsReader;
begin
  Reader := TTd32LocalsReader.Create;
  try
    // Loading a non-existent file must raise (file IO is the caller's
    // responsibility); the reader stays empty afterwards.
    Assert.WillRaise(
      procedure begin Reader.LoadFromFile('Z:\does-not-exist\nope.exe') end);
    Assert.IsTrue(Reader.Procs.Count = 0);
  finally
    Reader.Free;
  end;
end;

procedure TTd32LocalsReaderTests.TestLoadFromBytesWithoutSignatureLeavesEmpty;
// Synthetic 4 KB buffer that simulates a binary without any TD32 stream:
// MZ header at the start, then zeros. The reader's signature scan starts
// at offset 256 and walks the whole buffer; it must see no FB09/FB0A and
// produce zero procs. Verifies the all-zero path and proves the construct
// degrades gracefully when fed a binary that simply lacks debug info.
var
  Buf   : TBytes;
  Reader: TTd32LocalsReader;
begin
  SetLength(Buf, 4096);
  FillChar(Buf[0], Length(Buf), 0);
  Buf[0] := Ord('M');
  Buf[1] := Ord('Z');
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromBytes(Buf);
    Assert.IsTrue(Reader.Procs.Count = 0,
      Format('Buffer without TD32 signature must yield 0 procs (got %d)',
        [Reader.Procs.Count]));
  finally
    Reader.Free;
  end;
end;

procedure TTd32LocalsReaderTests.TestLoadFromTooSmallBufferLeavesEmpty;
// Defensive: feeding a buffer shorter than the minimum signature size must
// neither raise nor produce procs. Guards against future regressions where
// the scan loop accidentally reads past the buffer end.
var
  Tiny  : TBytes;
  Empty : TBytes;
  Reader: TTd32LocalsReader;
begin
  SetLength(Tiny, 4);
  FillChar(Tiny[0], Length(Tiny), 0);
  SetLength(Empty, 0);
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromBytes(Empty);
    Assert.IsTrue(Reader.Procs.Count = 0, 'Empty buffer must yield 0 procs');
    Reader.LoadFromBytes(Tiny);
    Assert.IsTrue(Reader.Procs.Count = 0, '4-byte buffer must yield 0 procs');
  finally
    Reader.Free;
  end;
end;

procedure TTd32LocalsReaderTests.TestLoadFromNonTd32FileLeavesEmpty;
var
  Reader: TTd32LocalsReader;
begin
  // The map file lives next to the EXE and does not contain TD32 data.
  // The reader must accept it without crashing and produce no procs.
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromFile(ChangeFileExt(ResolveRsmPath(False), '.map'));
    Assert.IsTrue(Reader.Procs.Count = 0,
      Format('A .map file must not be misread as a TD32 stream (got %d procs)',
        [Reader.Procs.Count]));
  finally
    Reader.Free;
  end;
end;

procedure TTd32LocalsReaderTests.TestFindProcContaining32;
var
  Reader : TTd32LocalsReader;
  ProcIdx: Integer;
  Proc   : TTd32Proc;
begin
  Reader := TTd32LocalsReader.Create;
  try
    Reader.LoadFromFile(ResolveRsmPath(False));
    ProcIdx := Reader.FindProcByName('LocalsProcedure');
    Assert.IsTrue(ProcIdx >= 0, 'LocalsProcedure not found');
    Proc := Reader.Procs[ProcIdx];

    // Mid-procedure offset must resolve to the same proc.
    Assert.AreEqual(ProcIdx,
      Reader.FindProcContaining(Proc.SegmentOffset + Proc.Size div 2),
      'Mid-procedure offset should resolve to the same proc');
    // First byte of the procedure must resolve.
    Assert.AreEqual(ProcIdx,
      Reader.FindProcContaining(Proc.SegmentOffset),
      'Start address of the procedure should resolve');
    // One byte past the end must NOT resolve to this proc.
    Assert.AreNotEqual(ProcIdx,
      Reader.FindProcContaining(Proc.SegmentOffset + Proc.Size),
      'Address past the procedure must not resolve to it');
  finally
    Reader.Free;
  end;
end;

{$IFDEF CPUX64}
procedure TTd32LocalsReaderTests.TestParsesProcedures64;                begin DoTestParsesProcedures(True);                 end;
procedure TTd32LocalsReaderTests.TestParsesLocalsForLocalsProcedure64;  begin DoTestParsesLocalsForLocalsProcedure(True);   end;
procedure TTd32LocalsReaderTests.TestLocalsHaveDistinctOffsets64;       begin DoTestLocalsHaveDistinctOffsets(True);        end;
procedure TTd32LocalsReaderTests.TestFindProcByName64;                  begin DoTestFindProcByName(True);                   end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TTd32LocalsReaderTests);

end.
