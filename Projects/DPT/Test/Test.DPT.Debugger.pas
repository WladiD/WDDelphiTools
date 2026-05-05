unit Test.DPT.Debugger;

interface

uses
  Winapi.Windows,
  DUnitX.TestFramework,
  DPT.Debugger,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.SyncObjs,
  System.StrUtils;

type
  [TestFixture]
  TDebuggerTests = class
  private
    FBreakpointHit: Boolean;
    FStackTrace: TArray<TStackFrame>;
    FExceptionHitCount: Integer;
    FWasTestExceptionCaught: Boolean;
    FCapturedLocals: TArray<TLocalVar>;
    procedure OnBreakpoint(Sender: TObject; Breakpoint: TBreakpoint);
    procedure OnBreakpointForStack(Sender: TObject; Breakpoint: TBreakpoint);
    procedure OnBreakpointForLocals(Sender: TObject; Breakpoint: TBreakpoint);
    procedure OnException(Sender: TObject; const AExceptionRecord: TExceptionRecord; const AFirstChance: Boolean; var AHandled: Boolean);
    function ResolveTargetPath(const AExeName: string; AUse64Bit: Boolean): string;
    function ResolveMapPath(AUse64Bit: Boolean): string;
    function WriteDottedMapFixture(AUse64Bit: Boolean): string;
    procedure DoTestBreakpoint(AUse64Bit: Boolean);
    procedure DoTestStackTrace(AUse64Bit: Boolean);
    procedure DoTestIgnoredException(AUse64Bit: Boolean);
    procedure DoTestTargetBitness(AUse64Bit: Boolean);
    procedure DoTestResolveDottedUnitLine(AUse64Bit: Boolean);
    procedure DoTestLocalVariables(AUse64Bit: Boolean);
  public
    [Test]
    procedure TestBreakpointInTarget32;
    [Test]
    procedure TestStackTrace32;
    [Test]
    procedure TestIgnoredException32;
    [Test]
    procedure TestTargetBitness32;
    // Regression: dotted unit names (e.g. 'My.Dotted.Unit') must resolve to
    // a breakpoint address. Previously ChangeFileExt stripped the trailing
    // dot-segment, turning 'My.Dotted.Unit' into 'My.Dotted' and breaking lookup.
    [Test]
    procedure TestResolveDottedUnitLine32;
    [Test]
    procedure TestLocalVariables32;
    [Test]
    procedure TestGetLocalsWithoutDebugInfoReturnsEmpty;
    [Test]
    procedure TestCapturesStdoutAndOdsAndStderr;
    [Test]
    procedure TestCapturedOutputCursor;
    [Test]
    procedure TestCapturedOutputEmptyBeforeProcessRuns;
    {$IFDEF CPUX64}
    [Test]
    procedure TestBreakpointInTarget64;
    [Test]
    procedure TestStackTrace64;
    [Test]
    procedure TestIgnoredException64;
    [Test]
    procedure TestTargetBitness64;
    [Test]
    procedure TestResolveDottedUnitLine64;
    [Test]
    procedure TestLocalVariables64;
    {$ENDIF}
  end;

implementation

procedure TDebuggerTests.OnBreakpoint(Sender: TObject; Breakpoint: TBreakpoint);
begin
  FBreakpointHit := True;
  (Sender as TDebugger).ResumeExecution;
end;

procedure TDebuggerTests.OnBreakpointForStack(Sender: TObject; Breakpoint: TBreakpoint);
var
  Debugger: TDebugger;
begin
  Debugger := Sender as TDebugger;
  FBreakpointHit := True;
  try
    FStackTrace := Debugger.GetStackTrace(Debugger.LastThreadHit);
  except
    // GetStackTrace may fail, but we still need to resume
  end;
  Debugger.ResumeExecution;
end;

procedure TDebuggerTests.OnBreakpointForLocals(Sender: TObject; Breakpoint: TBreakpoint);
var
  Debugger: TDebugger;
begin
  Debugger := Sender as TDebugger;
  FBreakpointHit := True;
  try
    FCapturedLocals := Debugger.GetLocals(Debugger.LastThreadHit);
  except
    // Even if extraction fails we must resume so the test can finish.
  end;
  Debugger.ResumeExecution;
end;

function TDebuggerTests.ResolveTargetPath(const AExeName: string; AUse64Bit: Boolean): string;
begin
  if AUse64Bit then
  begin
    Result := ExpandFileName('Projects\DPT\Test\Win64\' + AExeName);
    if not FileExists(Result) then
      Result := ExpandFileName('Win64\' + AExeName);
  end
  else
  begin
    Result := ExpandFileName('Projects\DPT\Test\Win32\' + AExeName);
    if not FileExists(Result) then
      Result := ExpandFileName('Win32\' + AExeName);
  end;
end;

function TDebuggerTests.ResolveMapPath(AUse64Bit: Boolean): string;
begin
  Result := ChangeFileExt(ResolveTargetPath('DebugTarget.exe', AUse64Bit), '.map');
end;

function TDebuggerTests.WriteDottedMapFixture(AUse64Bit: Boolean): string;
const
  OldHeader: RawByteString = 'Line numbers for DebugTarget(';
  NewHeader: RawByteString = 'Line numbers for My.Dotted.DebugTarget(';
var
  Bytes  : TBytes;
  Content: RawByteString;
  P      : Integer;
begin
  Bytes := TFile.ReadAllBytes(ResolveMapPath(AUse64Bit));
  SetLength(Content, Length(Bytes));
  if Length(Bytes) > 0 then
    Move(Bytes[0], Content[1], Length(Bytes));

  P := Pos(OldHeader, Content);
  Assert.IsTrue(P > 0, 'Fixture must contain "Line numbers for DebugTarget("');
  Delete(Content, P, Length(OldHeader));
  Insert(NewHeader, Content, P);

  Result := TPath.Combine(TPath.GetTempPath,
    Format('DPT.DebuggerDottedUnit.%s.map', [GUIDToString(TGUID.NewGuid)]));
  SetLength(Bytes, Length(Content));
  if Length(Content) > 0 then
    Move(Content[1], Bytes[0], Length(Content));
  TFile.WriteAllBytes(Result, Bytes);
end;

procedure TDebuggerTests.DoTestBreakpoint(AUse64Bit: Boolean);
var
  Debugger: TDebugger;
  ExePath, MapFile: string;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', AUse64Bit);
  MapFile := ChangeFileExt(ExePath, '.map');

  FBreakpointHit := False;
  Debugger := TDebugger.Create;
  try
    Debugger.OnBreakpoint := OnBreakpoint;
    Debugger.LoadMapFile(MapFile);

    // Line 17 is Writeln('Target') in TargetProcedure
    Debugger.SetBreakpoint('DebugTarget.dpr', 17);

    TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);
    Debugger.ResumeExecution;

    var StartTime := GetTickCount;
    while (GetTickCount - StartTime < 5000) and (not FBreakpointHit) do Sleep(100);

    Assert.IsTrue(FBreakpointHit, 'Breakpoint at line 17 not hit');
  finally
    Debugger.Free;
  end;
end;

procedure TDebuggerTests.DoTestStackTrace(AUse64Bit: Boolean);
var
  Debugger: TDebugger;
  ExePath, MapFile: string;
  FoundDeep, FoundTarget: Boolean;
  Frame: TStackFrame;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', AUse64Bit);
  MapFile := ChangeFileExt(ExePath, '.map');

  FBreakpointHit := False;
  SetLength(FStackTrace, 0);
  Debugger := TDebugger.Create;
  try
    Debugger.OnBreakpoint := OnBreakpointForStack;
    Debugger.LoadMapFile(MapFile);

    // Line 13 is Writeln('Deep') in DeepProcedure
    Debugger.SetBreakpoint('DebugTarget.dpr', 13);

    TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);
    Debugger.ResumeExecution;

    var StartTime := GetTickCount;
    while (GetTickCount - StartTime < 5000) and (not FBreakpointHit) do Sleep(100);

    Assert.IsTrue(FBreakpointHit, 'Breakpoint at line 13 not hit');
    Assert.IsTrue(Length(FStackTrace) > 0, 'Stack trace empty');

    FoundDeep := False;
    FoundTarget := False;

    for Frame in FStackTrace do
    begin
      if ContainsText(Frame.ProcedureName, 'DeepProcedure') then FoundDeep := True;
      if ContainsText(Frame.ProcedureName, 'TargetProcedure') then FoundTarget := True;
    end;

    Assert.IsTrue(FoundDeep, 'DeepProcedure missing in stack');
    Assert.IsTrue(FoundTarget, 'TargetProcedure missing in stack');
  finally
    Debugger.Free;
  end;
end;

procedure TDebuggerTests.OnException(Sender: TObject; const AExceptionRecord: TExceptionRecord; const AFirstChance: Boolean; var AHandled: Boolean);
var
  Debugger: TDebugger;
  ExName: string;
begin
  Debugger := Sender as TDebugger;
  ExName := Debugger.ReadExceptionClassName(AExceptionRecord);
  if ExName = 'Exception' then
    FWasTestExceptionCaught := True;
  Inc(FExceptionHitCount);
  AHandled := True;
  Debugger.ResumeExecution;
end;

procedure TDebuggerTests.DoTestIgnoredException(AUse64Bit: Boolean);
var
  Debugger: TDebugger;
  ExePath, MapFile: string;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', AUse64Bit);
  MapFile := ChangeFileExt(ExePath, '.map');

  FExceptionHitCount := 0;
  FWasTestExceptionCaught := False;
  Debugger := TDebugger.Create;
  try
    Debugger.OnException := OnException;
    Debugger.LoadMapFile(MapFile);
    // EAbort is ignored by default

    TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);
    Debugger.ResumeExecution;

    // Wait for the target to finish (it should finish naturally)
    var StartTime := GetTickCount;
    while (GetTickCount - StartTime < 5000) do
    begin
      Sleep(100);
    end;

    Assert.IsTrue(FWasTestExceptionCaught, 'The test Exception was not caught by the debugger');
    Assert.AreEqual(1, FExceptionHitCount, 'EAbort should have been ignored, so only 1 exception should have been caught');
  finally
    Debugger.Free;
  end;
end;

procedure TDebuggerTests.DoTestTargetBitness(AUse64Bit: Boolean);
var
  Debugger: TDebugger;
  ExePath, MapFile: string;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', AUse64Bit);
  MapFile := ChangeFileExt(ExePath, '.map');

  FBreakpointHit := False;
  Debugger := TDebugger.Create;
  try
    Debugger.OnBreakpoint := OnBreakpoint;
    Debugger.LoadMapFile(MapFile);
    Debugger.SetBreakpoint('DebugTarget.dpr', 17);

    TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);
    Debugger.ResumeExecution;

    var StartTime := GetTickCount;
    while (GetTickCount - StartTime < 5000) and (not FBreakpointHit) do Sleep(100);

    Assert.IsTrue(FBreakpointHit, 'Breakpoint not hit');

    if AUse64Bit then
    begin
      Assert.IsFalse(Debugger.TargetIs32Bit, 'Target should be detected as 64-bit');
      Assert.AreEqual(8, Debugger.TargetPointerSize, 'Pointer size should be 8 for 64-bit target');
    end
    else
    begin
      Assert.IsTrue(Debugger.TargetIs32Bit, 'Target should be detected as 32-bit');
      Assert.AreEqual(4, Debugger.TargetPointerSize, 'Pointer size should be 4 for 32-bit target');
    end;
  finally
    Debugger.Free;
  end;
end;

procedure TDebuggerTests.DoTestResolveDottedUnitLine(AUse64Bit: Boolean);
var
  AddrDotted : Pointer;
  AddrPath   : Pointer;
  AddrStem   : Pointer;
  AddrUnknown: Pointer;
  AddrWithPas: Pointer;
  Debugger   : TDebugger;
  FixturePath: string;
begin
  FixturePath := WriteDottedMapFixture(AUse64Bit);
  try
    Debugger := TDebugger.Create;
    try
      Debugger.LoadMapFile(FixturePath);

      // Dotted unit name must resolve verbatim.
      AddrDotted := Debugger.GetAddressFromUnitLine('My.Dotted.DebugTarget', 13);
      Assert.IsTrue(AddrDotted <> nil,
        'Dotted unit name must resolve (regression: ChangeFileExt stripped ".DebugTarget")');

      // Appending .pas must yield the same address (extension is stripped explicitly,
      // not via ChangeFileExt on the whole name).
      AddrWithPas := Debugger.GetAddressFromUnitLine('My.Dotted.DebugTarget.pas', 13);
      Assert.AreEqual(NativeUInt(AddrDotted), NativeUInt(AddrWithPas),
        'Appending .pas must not change the resolved address');

      // Full path with .pas must also resolve to the same address.
      AddrPath := Debugger.GetAddressFromUnitLine(
        'C:\SomeDir\My.Dotted.DebugTarget.pas', 13);
      Assert.AreEqual(NativeUInt(AddrDotted), NativeUInt(AddrPath),
        'Path-qualified .pas must resolve identically');

      // Truncated dotted name must NOT resolve (this was the buggy lookup result).
      AddrStem := Debugger.GetAddressFromUnitLine('My.Dotted', 13);
      Assert.IsTrue(AddrStem = nil,
        'Partial dotted name must not resolve');

      // Completely unknown unit must return nil.
      AddrUnknown := Debugger.GetAddressFromUnitLine('Does.Not.Exist', 13);
      Assert.IsTrue(AddrUnknown = nil,
        'Unknown unit must return nil');
    finally
      Debugger.Free;
    end;
  finally
    TFile.Delete(FixturePath);
  end;
end;

procedure TDebuggerTests.TestResolveDottedUnitLine32;
begin
  DoTestResolveDottedUnitLine(False);
end;

procedure TDebuggerTests.DoTestLocalVariables(AUse64Bit: Boolean);
const
  ExpectedLocalA: UInt32 = $12345678;
  ExpectedLocalB: Int64  = Int64($1122334455667788);
  ExpectedLocalC: UInt32 = $DEADBEEF;
  // Line 38 of DebugTarget.dpr is `Writeln('Locals ...);` inside
  // LocalsProcedure, after all three locals have been assigned.
  LocalsBreakpointLine = 38;
var
  Debugger : TDebugger;
  ExePath  : String;
  HasA     : Boolean;
  HasB     : Boolean;
  HasC     : Boolean;
  I        : Integer;
  Loc      : TLocalVar;
  StartTime: Cardinal;
  ValueA   : UInt32;
  ValueB   : Int64;
  ValueC   : UInt32;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', AUse64Bit);
  FBreakpointHit := False;
  SetLength(FCapturedLocals, 0);

  Debugger := TDebugger.Create;
  try
    Debugger.OnBreakpoint := OnBreakpointForLocals;
    Debugger.LoadMapFile(ChangeFileExt(ExePath, '.map'));
    Debugger.LoadDebugInfoFromExe(ExePath);
    Debugger.SetBreakpoint('DebugTarget.dpr', LocalsBreakpointLine);

    TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);
    Debugger.ResumeExecution;

    StartTime := GetTickCount;
    while (GetTickCount - StartTime < 5000) and (not FBreakpointHit) do
      Sleep(50);

    Assert.IsTrue(FBreakpointHit,
      Format('Breakpoint at line %d (LocalsProcedure) not hit', [LocalsBreakpointLine]));
    Assert.IsTrue(Length(FCapturedLocals) >= 3,
      Format('Expected at least 3 locals at the breakpoint, got %d', [Length(FCapturedLocals)]));

    HasA := False;
    HasB := False;
    HasC := False;
    ValueA := 0;
    ValueB := 0;
    ValueC := 0;

    for I := 0 to High(FCapturedLocals) do
    begin
      Loc := FCapturedLocals[I];
      Assert.IsTrue(Length(Loc.RawBytes) = 8,
        Format('Local "%s" must yield 8 bytes from process memory (got %d)',
          [Loc.Name, Length(Loc.RawBytes)]));
      if SameText(Loc.Name, 'LocalA') then
      begin
        HasA := True;
        ValueA := PUInt32(@Loc.RawBytes[0])^;
      end
      else if SameText(Loc.Name, 'LocalB') then
      begin
        HasB := True;
        ValueB := PInt64(@Loc.RawBytes[0])^;
      end
      else if SameText(Loc.Name, 'LocalC') then
      begin
        HasC := True;
        ValueC := PUInt32(@Loc.RawBytes[0])^;
      end;
    end;

    Assert.IsTrue(HasA, 'LocalA not present in captured locals');
    Assert.IsTrue(HasB, 'LocalB not present in captured locals');
    Assert.IsTrue(HasC, 'LocalC not present in captured locals');

    Assert.IsTrue(ValueA = ExpectedLocalA,
      Format('LocalA expected $%x, got $%x', [ExpectedLocalA, ValueA]));
    Assert.IsTrue(ValueB = ExpectedLocalB,
      Format('LocalB expected $%x, got $%x', [ExpectedLocalB, ValueB]));
    Assert.IsTrue(ValueC = ExpectedLocalC,
      Format('LocalC expected $%x, got $%x', [ExpectedLocalC, ValueC]));
  finally
    Debugger.Free;
  end;
end;

procedure TDebuggerTests.TestLocalVariables32;
begin
  DoTestLocalVariables(False);
end;

procedure TDebuggerTests.TestCapturesStdoutAndOdsAndStderr;
// Runs DebugTarget to completion and asserts that the captured-output
// buffer contains exemplars from each of the three sources we hook:
//   - stdout: the LocalsProcedure Writeln line
//   - stderr: the explicit Writeln(ErrOutput, 'stderr-tag-line')
//   - ods   : OutputDebugString('ods-tag-line')
var
  Debugger : TDebugger;
  ExePath  : String;
  StartTime: Cardinal;
  Lines    : TArray<TCapturedOutputLine>;
  HasStdout: Boolean;
  HasStderr: Boolean;
  HasOds   : Boolean;
  I        : Integer;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);

  Debugger := TDebugger.Create;
  try
    TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);
    Debugger.ResumeExecution;

    // Wait for the process to finish so all output has been captured.
    StartTime := GetTickCount;
    while (GetTickCount - StartTime < 5000) do
    begin
      if Debugger.GetCapturedOutputCount >= 3 then
        Break;
      Sleep(50);
    end;
    // Give the reader thread a moment to drain any trailing bytes.
    Sleep(200);

    Lines := Debugger.GetCapturedOutput(0);
    HasStdout := False;
    HasStderr := False;
    HasOds    := False;
    for I := 0 to High(Lines) do
    begin
      case Lines[I].Source of
        cosStdout: if Lines[I].Text.Contains('Locals')        then HasStdout := True;
        cosStderr: if Lines[I].Text.Contains('stderr-tag-line') then HasStderr := True;
        cosOds   : if Lines[I].Text.Contains('ods-tag-line')   then HasOds := True;
      end;
    end;

    Assert.IsTrue(HasStdout, 'stdout line "Locals ..." not captured');
    Assert.IsTrue(HasStderr, 'stderr line "stderr-tag-line" not captured');
    Assert.IsTrue(HasOds,    'OutputDebugString "ods-tag-line" not captured');
  finally
    Debugger.Free;
  end;
end;

procedure TDebuggerTests.TestCapturedOutputCursor;
// GetCapturedOutput(SinceIndex) must skip any lines whose Index is <=
// SinceIndex. After capturing all output, asking for "everything after
// the first line" yields strictly fewer lines than asking for everything.
var
  Debugger    : TDebugger;
  ExePath     : String;
  StartTime   : Cardinal;
  All         : TArray<TCapturedOutputLine>;
  AfterFirst  : TArray<TCapturedOutputLine>;
  AfterLast   : TArray<TCapturedOutputLine>;
begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);

  Debugger := TDebugger.Create;
  try
    TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);
    Debugger.ResumeExecution;

    StartTime := GetTickCount;
    while (GetTickCount - StartTime < 5000) do
    begin
      if Debugger.GetCapturedOutputCount >= 3 then
        Break;
      Sleep(50);
    end;
    Sleep(200);

    All := Debugger.GetCapturedOutput(0);
    Assert.IsTrue(Length(All) >= 3,
      Format('Expected at least 3 captured lines, got %d', [Length(All)]));

    AfterFirst := Debugger.GetCapturedOutput(All[0].Index);
    Assert.IsTrue(Length(AfterFirst) = Length(All) - 1,
      'Cursor at first line index should drop exactly one line');

    AfterLast := Debugger.GetCapturedOutput(All[High(All)].Index);
    Assert.IsTrue(Length(AfterLast) = 0,
      'Cursor at last line index should drop everything');
  finally
    Debugger.Free;
  end;
end;

procedure TDebuggerTests.TestCapturedOutputEmptyBeforeProcessRuns;
// A freshly constructed debugger has nothing in its buffer; the
// API must not crash and must return zero counts/lines.
var
  Debugger: TDebugger;
begin
  Debugger := TDebugger.Create;
  try
    Assert.IsTrue(Debugger.GetCapturedOutputCount = 0,
      'Newly created debugger must have empty output buffer');
    Assert.IsTrue(Length(Debugger.GetCapturedOutput(0)) = 0,
      'GetCapturedOutput on a fresh debugger must return empty array');
  finally
    Debugger.Free;
  end;
end;

procedure TDebuggerTests.TestGetLocalsWithoutDebugInfoReturnsEmpty;
var
  Debugger: TDebugger;
  Locals  : TArray<TLocalVar>;
begin
  // No LoadDebugInfoFromExe call: GetLocals must early-out instead of
  // crashing on a nil reader. The thread handle is unused in that case.
  Debugger := TDebugger.Create;
  try
    Locals := Debugger.GetLocals(0);
    Assert.IsTrue(Length(Locals) = 0,
      'GetLocals must return empty when no TD32 info is loaded');
  finally
    Debugger.Free;
  end;
end;

{$IFDEF CPUX64}
procedure TDebuggerTests.TestResolveDottedUnitLine64;
begin
  DoTestResolveDottedUnitLine(True);
end;

procedure TDebuggerTests.TestLocalVariables64;
begin
  DoTestLocalVariables(True);
end;
{$ENDIF}

procedure TDebuggerTests.TestBreakpointInTarget32;
begin
  DoTestBreakpoint(False);
end;

procedure TDebuggerTests.TestStackTrace32;
begin
  DoTestStackTrace(False);
end;

procedure TDebuggerTests.TestIgnoredException32;
begin
  DoTestIgnoredException(False);
end;

procedure TDebuggerTests.TestTargetBitness32;
begin
  DoTestTargetBitness(False);
end;

{$IFDEF CPUX64}
procedure TDebuggerTests.TestBreakpointInTarget64;
begin
  DoTestBreakpoint(True);
end;

procedure TDebuggerTests.TestStackTrace64;
begin
  DoTestStackTrace(True);
end;

procedure TDebuggerTests.TestIgnoredException64;
begin
  DoTestIgnoredException(True);
end;

procedure TDebuggerTests.TestTargetBitness64;
begin
  DoTestTargetBitness(True);
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TDebuggerTests);

end.
