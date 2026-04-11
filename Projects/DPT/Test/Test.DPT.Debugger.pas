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
    procedure DoTestBreakpoint(AUse64Bit: Boolean);
    procedure DoTestStackTrace(AUse64Bit: Boolean);
    procedure DoTestIgnoredException(AUse64Bit: Boolean);
    procedure DoTestTargetBitness(AUse64Bit: Boolean);
  public
    [Test]
    procedure TestBreakpointInTarget32;
    [Test]
    procedure TestStackTrace32;
    [Test]
    procedure TestIgnoredException32;
    [Test]
    procedure TestTargetBitness32;
    {$IFDEF CPUX64}
    [Test]
    procedure TestBreakpointInTarget64;
    [Test]
    procedure TestStackTrace64;
    [Test]
    procedure TestIgnoredException64;
    [Test]
    procedure TestTargetBitness64;
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
