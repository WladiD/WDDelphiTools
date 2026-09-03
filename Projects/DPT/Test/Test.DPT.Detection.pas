// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Detection;

interface

uses

  Winapi.TlHelp32,
  Winapi.Windows,

  System.SysUtils,

  DUnitX.TestFramework,

  DPT.Detection,
  DPT.Types;

type

  TFakeProcess = record
    PID: DWORD;
    ParentPID: DWORD;
    ExeFile: string;
  end;

  /// <summary>
  ///   <see cref="TProcessTreeScanner"/> over a hand-crafted process table
  ///   instead of the live Toolhelp snapshot, so the parent walk can be
  ///   exercised with shapes the real system produces only sporadically
  ///   (PID reuse cycles, vanished parents, ...).
  /// </summary>
  TFakeProcessTreeScanner = class(TProcessTreeScanner)
  private
    FTable: TArray<TFakeProcess>;
  protected
    function GetProcessEntry(AID: DWORD; out AEntry: TProcessEntry32): Boolean; override;
  public
    procedure Add(APID, AParentPID: DWORD; const AExeFile: string);
  end;

  /// <summary>
  ///   Exercises the parent-process walk of <see cref="TProcessTreeScanner.DetectAIMode"/>.
  /// </summary>
  [TestFixture]
  TTestProcessTreeScanner = class
  private
    FScanner: TFakeProcessTreeScanner;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // ---- Regular chains ----
    [Test]
    procedure Chain_WithoutAIHost_ReturnsNone;
    [Test]
    procedure Chain_EndingAtSystemProcess_ReturnsNone;
    [Test]
    procedure Chain_WithCursorAncestor_ReturnsCursorAndHostPID;
    [Test]
    procedure Chain_WithNodeAncestor_ReturnsGeminiAndHostPID;
    [Test]
    procedure Chain_WithClaudeAncestor_ReturnsClaudeAndHostPID;
    [Test]
    procedure Chain_WithClaudeFromVsCodeExtension_ReturnsClaude;
    [Test]
    procedure Chain_HostIsFoundBeforeCycle;
    [Test]
    procedure Chain_ParentMissingFromTable_ReturnsNone;

    // ---- Degenerate chains (GitHub issue #17) ----
    [Test]
    procedure Chain_WithPidReuseCycle_Terminates;
    [Test]
    procedure Chain_FromIssue17_ClaudeEndsWalkBeforeCycle;
    [Test]
    procedure Chain_WithSelfParent_Terminates;
    [Test]
    procedure Chain_StartPidIsItsOwnParent_Terminates;

    // ---- Live snapshot smoke test ----
    [Test]
    procedure LiveScanner_Terminates;
  end;

implementation

{ TFakeProcessTreeScanner }

procedure TFakeProcessTreeScanner.Add(APID, AParentPID: DWORD; const AExeFile: string);
var
  Entry: TFakeProcess;
begin
  Entry.PID := APID;
  Entry.ParentPID := AParentPID;
  Entry.ExeFile := AExeFile;
  FTable := FTable + [Entry];
end;

function TFakeProcessTreeScanner.GetProcessEntry(AID: DWORD; out AEntry: TProcessEntry32): Boolean;
begin
  for var Entry in FTable do
    if Entry.PID = AID then
    begin
      FillChar(AEntry, SizeOf(AEntry), 0);
      AEntry.dwSize := SizeOf(AEntry);
      AEntry.th32ProcessID := Entry.PID;
      AEntry.th32ParentProcessID := Entry.ParentPID;
      StrPLCopy(AEntry.szExeFile, Entry.ExeFile, High(AEntry.szExeFile));
      Exit(True);
    end;
  Result := False;
end;

{ TTestProcessTreeScanner }

procedure TTestProcessTreeScanner.Setup;
begin
  FScanner := TFakeProcessTreeScanner.Create;
end;

procedure TTestProcessTreeScanner.TearDown;
begin
  FScanner.Free;
end;

procedure TTestProcessTreeScanner.Chain_WithoutAIHost_ReturnsNone;
var
  HostPID: DWORD;
begin
  FScanner.Add(100, 200, 'DPT.exe');
  FScanner.Add(200, 300, 'cmd.exe');
  FScanner.Add(300, 0, 'explorer.exe');

  Assert.AreEqual(amNone, FScanner.DetectAIMode(100, HostPID));
  Assert.AreEqual<DWORD>(0, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_EndingAtSystemProcess_ReturnsNone;
var
  HostPID: DWORD;
begin
  FScanner.Add(100, 200, 'DPT.exe');
  FScanner.Add(200, 4, 'services.exe');
  FScanner.Add(4, 0, 'System');

  Assert.AreEqual(amNone, FScanner.DetectAIMode(100, HostPID));
  Assert.AreEqual<DWORD>(0, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_WithCursorAncestor_ReturnsCursorAndHostPID;
var
  HostPID: DWORD;
begin
  FScanner.Add(100, 200, 'DPT.exe');
  FScanner.Add(200, 300, 'cmd.exe');
  FScanner.Add(300, 400, 'cursor.EXE'); // case-insensitive match
  FScanner.Add(400, 0, 'explorer.exe');

  Assert.AreEqual(amCursor, FScanner.DetectAIMode(100, HostPID));
  Assert.AreEqual<DWORD>(300, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_WithNodeAncestor_ReturnsGeminiAndHostPID;
var
  HostPID: DWORD;
begin
  FScanner.Add(100, 200, 'DPT.exe');
  FScanner.Add(200, 300, 'node.exe');
  FScanner.Add(300, 0, 'explorer.exe');

  Assert.AreEqual(amGemini, FScanner.DetectAIMode(100, HostPID));
  Assert.AreEqual<DWORD>(200, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_WithClaudeAncestor_ReturnsClaudeAndHostPID;
var
  HostPID: DWORD;
begin
  // Chain observed live for a Claude Code terminal session: the CLI is a
  // native binary, so no node.exe appears anywhere above the tool process.
  FScanner.Add(41424, 30748, 'powershell.exe');
  FScanner.Add(30748, 29800, 'claude.exe');
  FScanner.Add(29800, 24228, 'cmd.exe');
  FScanner.Add(24228, 20052, 'doublecmd.exe');
  FScanner.Add(20052, 19532, 'explorer.exe'); // 19532 has exited

  Assert.AreEqual(amClaude, FScanner.DetectAIMode(41424, HostPID));
  Assert.AreEqual<DWORD>(30748, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_WithClaudeFromVsCodeExtension_ReturnsClaude;
var
  HostPID: DWORD;
begin
  // The VS Code extension launches the same native binary from its
  // extension folder; Code.exe itself must not be mistaken for a host.
  FScanner.Add(100, 200, 'DPT.exe');
  FScanner.Add(200, 300, 'cmd.exe');
  FScanner.Add(300, 400, 'claude.exe');
  FScanner.Add(400, 500, 'Code.exe');
  FScanner.Add(500, 600, 'Code.exe');
  FScanner.Add(600, 0, 'explorer.exe');

  Assert.AreEqual(amClaude, FScanner.DetectAIMode(100, HostPID));
  Assert.AreEqual<DWORD>(300, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_HostIsFoundBeforeCycle;
var
  HostPID: DWORD;
begin
  FScanner.Add(100, 200, 'DPT.exe');
  FScanner.Add(200, 300, 'Cursor.exe');
  FScanner.Add(300, 400, 'services.exe');
  FScanner.Add(400, 300, 'wininit.exe');

  Assert.AreEqual(amCursor, FScanner.DetectAIMode(100, HostPID));
  Assert.AreEqual<DWORD>(200, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_ParentMissingFromTable_ReturnsNone;
var
  HostPID: DWORD;
begin
  // The parent has exited and is no longer part of the snapshot.
  FScanner.Add(100, 200, 'DPT.exe');
  FScanner.Add(200, 999, 'cmd.exe');

  Assert.AreEqual(amNone, FScanner.DetectAIMode(100, HostPID));
  Assert.AreEqual<DWORD>(0, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_WithPidReuseCycle_Terminates;
var
  HostPID: DWORD;
begin
  // Shape reported in GitHub issue #17: wininit.exe still names its
  // long-gone smss.exe parent, whose PID has meanwhile been reused by
  // services.exe - a descendant of wininit.exe. The snapshot therefore
  // contains the cycle services -> wininit -> services -> ...
  // No AI host in the chain, so the walk must run into the cycle.
  FScanner.Add(41392, 36156, 'bash.exe');
  FScanner.Add(36156, 14108, 'cmd.exe');
  FScanner.Add(14108, 3352, 'sihost.exe');
  FScanner.Add(3352, 1840, 'svchost.exe');
  FScanner.Add(1840, 2020, 'services.exe');
  FScanner.Add(2020, 1840, 'wininit.exe');

  Assert.AreEqual(amNone, FScanner.DetectAIMode(41392, HostPID));
  Assert.AreEqual<DWORD>(0, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_FromIssue17_ClaudeEndsWalkBeforeCycle;
var
  HostPID: DWORD;
begin
  // The exact chain from GitHub issue #17. The reporter ran DPT from Claude
  // Code; with claude.exe recognised as a host the walk now stops there and
  // never reaches the services/wininit cycle further up.
  FScanner.Add(41392, 36156, 'bash.exe');
  FScanner.Add(36156, 31444, 'claude.exe');
  FScanner.Add(31444, 14108, 'claude.exe');
  FScanner.Add(14108, 3352, 'sihost.exe');
  FScanner.Add(3352, 1840, 'svchost.exe');
  FScanner.Add(1840, 2020, 'services.exe');
  FScanner.Add(2020, 1840, 'wininit.exe');

  Assert.AreEqual(amClaude, FScanner.DetectAIMode(41392, HostPID));
  Assert.AreEqual<DWORD>(36156, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_WithSelfParent_Terminates;
var
  HostPID: DWORD;
begin
  FScanner.Add(100, 200, 'DPT.exe');
  FScanner.Add(200, 200, 'cmd.exe');

  Assert.AreEqual(amNone, FScanner.DetectAIMode(100, HostPID));
  Assert.AreEqual<DWORD>(0, HostPID);
end;

procedure TTestProcessTreeScanner.Chain_StartPidIsItsOwnParent_Terminates;
var
  HostPID: DWORD;
begin
  FScanner.Add(100, 100, 'DPT.exe');

  Assert.AreEqual(amNone, FScanner.DetectAIMode(100, HostPID));
  Assert.AreEqual<DWORD>(0, HostPID);
end;

procedure TTestProcessTreeScanner.LiveScanner_Terminates;
var
  HostPID: DWORD;
  Scanner: TProcessTreeScanner;
begin
  // The result depends on who launched the test runner; the only invariant
  // is that the walk over the real snapshot returns at all.
  Scanner := TProcessTreeScanner.Create;
  try
    Scanner.DetectAIMode(HostPID);
    Assert.Pass;
  finally
    Scanner.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestProcessTreeScanner);

end.
