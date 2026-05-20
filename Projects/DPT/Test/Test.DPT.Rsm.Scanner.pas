// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Rsm.Scanner;

// Direct tests for TRsmScanner: loads the DebugTarget .rsm fixture
// and asserts the raw scan output (procs, classes, globals, enum
// constants, type aliases). These tests bypass the
// TRsmLocalsReader facade so a regression in the byte-stream walk
// can be pinned to the scanner and not the post-process passes
// that sit on top.

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TRsmScannerTests = class
  private
    function ResolveExePath(AUse64Bit: Boolean): String;
  public
    /// Loading a non-existent EXE path leaves the scanner empty and
    /// does not raise. Mirrors the reader's behaviour.
    [Test]
    procedure TestLoadFromMissingFileLeavesEmpty;
    /// LoadFromBuffer on a buffer that does NOT start with the
    /// CSH7 magic returns without populating anything.
    [Test]
    procedure TestLoadFromGarbageBufferLeavesEmpty;

    /// Procs collected from a real Win32 RSM. Sanity checks the
    /// known top-level fixture procedures.
    [Test]
    procedure TestProcsCollected32;
    /// Classes / records collected from Win32 RSM. The DebugTarget
    /// fixture defines TBase, TDerived, TLightStatus host record and
    /// several other shapes; we assert the byte-level scan caught
    /// at least one of each kind.
    [Test]
    procedure TestStructsCollected32;
    /// Module-level global registered. GGlobalLight (typed
    /// TLightStatus) must appear in GlobalByName so the dotted-walk
    /// can route through it.
    [Test]
    procedure TestGlobalsRegistered32;
    /// Enum constants registered. The $25 program-local form
    /// puts lsGreen / lsYellow / lsRed under TLightStatus's $2E
    /// type id.
    [Test]
    procedure TestProgramLocalEnumConstantsRegistered32;
    /// Cross-unit enum aliasing. tpHigher (TThreadPriority,
    /// System.Classes) is emitted with the $25 $8A-prefix form;
    /// the scanner must capture it as a cross-unit enum id and
    /// the $2A registry entry must link the primary alias to it.
    [Test]
    procedure TestCrossUnitEnumIdRegistered32;

    {$IFDEF CPUX64}
    /// Win64 sanity. Same as TestProcsCollected32 but on the Win64
    /// fixture; structures are encoded differently (Win64 trailer
    /// pattern with 8-byte zero pad) so the scanner must handle
    /// both inside the same code path.
    [Test]
    procedure TestProcsCollected64;
    [Test]
    procedure TestStructsCollected64;
    [Test]
    procedure TestGlobalsRegistered64;
    {$ENDIF}
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,

  mormot.core.collections,

  DPT.Rsm.Model,
  DPT.Rsm.Scanner;

function TRsmScannerTests.ResolveExePath(AUse64Bit: Boolean): String;
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

procedure TRsmScannerTests.TestLoadFromMissingFileLeavesEmpty;
var
  S: TRsmScanner;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile('Z:\definitely-does-not-exist\nope.exe');
    Assert.AreEqual<Integer>(0, S.Procs.Count, 'Procs empty');
    Assert.AreEqual<Integer>(0, S.Classes.Count, 'Classes empty');
    Assert.IsNull(S.Buf, 'Buf nil after failed map');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestLoadFromGarbageBufferLeavesEmpty;
var
  S    : TRsmScanner;
  Junk : array[0..15] of Byte;
begin
  // Non-CSH7 header: scanner returns without populating.
  FillChar(Junk, SizeOf(Junk), $FF);
  S := TRsmScanner.Create;
  try
    S.LoadFromBuffer(@Junk[0], SizeOf(Junk));
    Assert.AreEqual<Integer>(0, S.Procs.Count);
    Assert.AreEqual<Integer>(0, S.Classes.Count);
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestProcsCollected32;
var
  S: TRsmScanner;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Procs.Count > 0, 'expected procs from DebugTarget');
    Assert.IsTrue(S.ProcByName.ContainsKey('locallaprocedure') or
                  S.ProcByName.ContainsKey('localsprocedure'),
      'expected a known fixture proc in ProcByName index');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestStructsCollected32;
var
  S   : TRsmScanner;
  HasClass, HasRecord: Boolean;
  I   : Integer;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    HasClass  := False;
    HasRecord := False;
    for I := 0 to S.Classes.Count - 1 do
    begin
      if S.Classes[I].Kind = skClass  then HasClass  := True;
      if S.Classes[I].Kind = skRecord then HasRecord := True;
      if HasClass and HasRecord then Break;
    end;
    Assert.IsTrue(HasClass, 'expected at least one class in DebugTarget');
    Assert.IsTrue(HasRecord, 'expected at least one record in DebugTarget');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestGlobalsRegistered32;
var
  S      : TRsmScanner;
  TypeId : UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    // GGlobalLight: TLightStatus -- typed enum global emitted via
    // the $20 form. Must appear in GlobalByName with a non-zero
    // type id (enum hi byte $2E).
    Assert.IsTrue(S.GlobalByName.TryGetValue('ggloballight', TypeId),
      'GGlobalLight missing from GlobalByName');
    Assert.IsTrue(TypeId <> 0, 'GGlobalLight type id is zero');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestProgramLocalEnumConstantsRegistered32;
var
  S         : TRsmScanner;
  Pair      : TPair<String, String>;
  FoundGreen: Boolean;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    // We don't know TLightStatus's secondary id up front; iterate
    // and confirm at least one $25-registered constant decoded as
    // "lsGreen" (TLightStatus's element-0 element).
    FoundGreen := False;
    for Pair in S.EnumConstNames do
      if SameText(Pair.Value, 'lsGreen') then
      begin
        FoundGreen := True;
        Break;
      end;
    Assert.IsTrue(FoundGreen, 'lsGreen not registered as an enum constant');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestCrossUnitEnumIdRegistered32;
var
  S         : TRsmScanner;
  Pair      : TPair<String, String>;
  FoundCross: Boolean;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    // tpHigher (TThreadPriority, System.Classes) is emitted via
    // the $25 $8A-prefix form; the scanner must capture it and
    // record the cross-unit secondary id.
    FoundCross := False;
    for Pair in S.EnumConstNames do
      if SameText(Pair.Value, 'tpHigher') then
      begin
        FoundCross := True;
        Break;
      end;
    Assert.IsTrue(FoundCross, 'tpHigher not registered as an enum constant');
    Assert.IsTrue(S.CrossUnitEnumIds.Count > 0,
      'CrossUnitEnumIds empty -- $25 $8A-prefix form not detected');
  finally
    S.Free;
  end;
end;

{$IFDEF CPUX64}
procedure TRsmScannerTests.TestProcsCollected64;
var
  S: TRsmScanner;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(True));
    Assert.IsTrue(S.Procs.Count > 0, 'expected procs from Win64 DebugTarget');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestStructsCollected64;
var
  S: TRsmScanner;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(True));
    Assert.IsTrue(S.Classes.Count > 0, 'expected classes from Win64');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestGlobalsRegistered64;
var
  S     : TRsmScanner;
  TypeId: UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(True));
    Assert.IsTrue(S.GlobalByName.TryGetValue('ggloballight', TypeId),
      'GGlobalLight missing in Win64 fixture');
  finally
    S.Free;
  end;
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TRsmScannerTests);

end.
