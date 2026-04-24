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
    procedure OnBreakpoint(Sender: TObject; Breakpoint: TBreakpoint);
    procedure OnBreakpointForStack(Sender: TObject; Breakpoint: TBreakpoint);
    procedure OnException(Sender: TObject; const AExceptionRecord: TExceptionRecord; const AFirstChance: Boolean; var AHandled: Boolean);
    function ResolveTargetPath(const AExeName: string; AUse64Bit: Boolean): string;
    function ResolveMapPath(AUse64Bit: Boolean): string;
    function WriteDottedMapFixture(AUse64Bit: Boolean): string;
    procedure DoTestBreakpoint(AUse64Bit: Boolean);
    procedure DoTestStackTrace(AUse64Bit: Boolean);
    procedure DoTestIgnoredException(AUse64Bit: Boolean);
    procedure DoTestTargetBitness(AUse64Bit: Boolean);
    procedure DoTestResolveDottedUnitLine(AUse64Bit: Boolean);
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

{$IFDEF CPUX64}
procedure TDebuggerTests.TestResolveDottedUnitLine64;
begin
  DoTestResolveDottedUnitLine(True);
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
