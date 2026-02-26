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
    procedure OnBreakpoint(Sender: TObject; Breakpoint: TBreakpoint);
    procedure OnBreakpointForStack(Sender: TObject; Breakpoint: TBreakpoint);
  public
    [Test]
    procedure TestBreakpointInTarget;
    [Test]
    procedure TestStackTrace;
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
  FStackTrace := Debugger.GetStackTrace(Debugger.LastThreadHit);
  Debugger.ResumeExecution;
end;

procedure TDebuggerTests.TestBreakpointInTarget;
var
  Debugger: TDebugger;
  ExePath, MapFile: string;
  Thread: TDebuggerThread;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  FBreakpointHit := False;
  Debugger := TDebugger.Create;
  try
    Debugger.OnBreakpoint := OnBreakpoint;
    Debugger.LoadMapFile(MapFile);

    // Line 17 is Writeln('Target') in TargetProcedure
    Debugger.SetBreakpoint('DebugTarget.dpr', 17);

    Thread := TDebuggerThread.Create(Debugger, ExePath);
    Debugger.WaitForReady(5000);
    Debugger.ResumeExecution;

    var StartTime := GetTickCount;
    while (GetTickCount - StartTime < 5000) and (not FBreakpointHit) do Sleep(100);

    Assert.IsTrue(FBreakpointHit, 'Breakpoint at line 17 not hit');
  finally
    Debugger.Free;
  end;
end;

procedure TDebuggerTests.TestStackTrace;
var
  Debugger: TDebugger;
  ExePath, MapFile: string;
  Thread: TDebuggerThread;
  FoundDeep, FoundTarget: Boolean;
  Frame: TStackFrame;
begin
  ExePath := ExpandFileName('Projects\DPT\Test\DebugTarget.exe');
  if not FileExists(ExePath) then ExePath := ExpandFileName('DebugTarget.exe');
  MapFile := ChangeFileExt(ExePath, '.map');

  FBreakpointHit := False;
  SetLength(FStackTrace, 0);
  Debugger := TDebugger.Create;
  try
    Debugger.OnBreakpoint := OnBreakpointForStack;
    Debugger.LoadMapFile(MapFile);

    // Line 13 is Writeln('Deep') in DeepProcedure
    Debugger.SetBreakpoint('DebugTarget.dpr', 13);

    Thread := TDebuggerThread.Create(Debugger, ExePath);
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

initialization
  TDUnitX.RegisterTestFixture(TDebuggerTests);

end.
