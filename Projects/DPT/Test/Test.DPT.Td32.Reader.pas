// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Td32.Reader;

// Head-to-head deliverable: prove the recovered TD32 reader
// (TTd32Reader, parsing the embedded FB09/FB0A CodeView stream in
// DebugTarget.exe) resolves the nested record-member -> sub-record-TYPE
// edge that the RSM reader (TRsmReader, parsing the <exe>.rsm sidecar)
// CANNOT for cross-unit records.
//
// The edge under test, for each parent.field:
//   FindClassMember(parent, field, M) -> M.TypeIdx
//   FindStructByTypeIdx(M.TypeIdx)    -> Idx
//   Classes[Idx].Name                 = expected sub-record name
//
// Cases:
//   * SAME-UNIT sanity:   TWithHeader.WhdrHeader -> TWhdrHeader
//                         (field-id == primary; RSM resolves this too)
//   * CROSS-UNIT failures: TXAdresse.Anschrift   -> TXAnschrift
//                          TComplexRec.CxR1       -> TCxRec1
//                         (RSM returns M.TypeIdx = 0 / positional, so its
//                          FindStructByTypeIdx cannot reach the sub-record)

interface

uses

  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  DPT.Td32.Reader;

type

  [TestFixture]
  TTd32VsRsmCrossUnitEdgeTests = class
  private
    function ResolveTargetPath(const AExeName: String; AUse64Bit: Boolean): String;
  public
    [Test]
    procedure Td32ResolvesCrossUnitMemberTypeEdgeWhereRsmCannot;
    [Test]
    procedure Td32StandaloneTdsResolvesSameMemberTypeEdges;
  end;

implementation

uses

  DPT.Rsm.Model,
  DPT.Rsm.Reader;

function TTd32VsRsmCrossUnitEdgeTests.ResolveTargetPath(const AExeName: String;
  AUse64Bit: Boolean): String;
// Mirrors the helper used by Test.DPT.Rsm.Reader / Test.DPT.Debugger:
// try the repo-root-relative path first, then the cwd-relative one.
var
  Sub: String;
begin
  if AUse64Bit then
    Sub := 'Win64'
  else
    Sub := 'Win32';
  Result := ExpandFileName('Projects\DPT\Test\' + Sub + '\' + AExeName);
  if not TFile.Exists(Result) then
    Result := ExpandFileName(Sub + '\' + AExeName);
end;

procedure TTd32VsRsmCrossUnitEdgeTests.Td32ResolvesCrossUnitMemberTypeEdgeWhereRsmCannot;
var
  ExePath  : String;
  Td32     : TTd32Reader;
  Rsm      : TRsmReader;

  // Resolves parent.field -> sub-record name via the TD32 reader, logging
  // the raw TypeIdx and resolved name so a RED case is self-explanatory.
  function Td32Resolves(const AParent, AField, AExpected: String): Boolean;
  var
    M  : TTd32ClassMember;
    Idx: Integer;
    ResolvedName: String;
  begin
    Result := False;
    ResolvedName := '<unresolved>';
    if not Td32.FindClassMember(AParent, AField, M) then
    begin
      System.Writeln(Format(
        'TD32  %s.%s : FindClassMember FAILED (member not found)',
        [AParent, AField]));
      Exit;
    end;
    Idx := Td32.FindStructByTypeIdx(M.TypeIdx);
    if Idx >= 0 then
      ResolvedName := Td32.Classes[Idx].Name;
    Result := (Idx >= 0) and SameText(ResolvedName, AExpected);
    System.Writeln(Format(
      'TD32  %s.%s -> M.TypeIdx=$%x  FindStructByTypeIdx=%d  name="%s"  (expected "%s")  => %s',
      [AParent, AField, M.TypeIdx, Idx, ResolvedName, AExpected,
       BoolToStr(Result, True)]));
  end;

  // Same probe through the RSM reader. Returns whether RSM resolved the
  // edge; logs the raw M.TypeIdx / resolved name either way.
  function RsmResolves(const AParent, AField, AExpected: String): Boolean;
  var
    M  : TRsmClassMember;
    Idx: Integer;
    ResolvedName: String;
  begin
    Result := False;
    ResolvedName := '<unresolved>';
    if not Rsm.FindClassMember(AParent, AField, M) then
    begin
      System.Writeln(Format(
        'RSM   %s.%s : FindClassMember FAILED (member not found)',
        [AParent, AField]));
      Exit;
    end;
    Idx := Rsm.FindStructByTypeIdx(M.TypeIdx);
    if Idx >= 0 then
      ResolvedName := Rsm.Classes[Idx].Name;
    Result := (Idx >= 0) and SameText(ResolvedName, AExpected);
    System.Writeln(Format(
      'RSM   %s.%s -> M.TypeIdx=$%x  FindStructByTypeIdx=%d  name="%s"  (expected "%s")  => %s',
      [AParent, AField, M.TypeIdx, Idx, ResolvedName, AExpected,
       BoolToStr(Result, True)]));
  end;

begin
  ExePath := ResolveTargetPath('DebugTarget.exe', False);
  if not TFile.Exists(ExePath) then
  begin
    Assert.Pass('DebugTarget.exe fixture missing (' + ExePath +
      '); skipping TD32-vs-RSM cross-unit edge test.');
    Exit;
  end;

  Td32 := TTd32Reader.Create;
  Rsm  := TRsmReader.Create;
  try
    Td32.LoadFromFile(ExePath);
    Rsm.LoadFromFile(ExePath);

    Assert.IsTrue(Td32.Classes.Count > 0,
      'TD32 reader extracted no structured types from the embedded CodeView stream');

    // --- SAME-UNIT sanity: both readers should resolve this. ---
    Assert.IsTrue(Td32Resolves('TWithHeader', 'WhdrHeader', 'TWhdrHeader'),
      'TD32 must resolve same-unit TWithHeader.WhdrHeader -> TWhdrHeader');

    // --- CROSS-UNIT: the RSM-failure cases. TD32 must resolve them. ---
    Assert.IsTrue(Td32Resolves('TXAdresse', 'Anschrift', 'TXAnschrift'),
      'TD32 must resolve cross-unit TXAdresse.Anschrift -> TXAnschrift');
    Assert.IsTrue(Td32Resolves('TComplexRec', 'CxR1', 'TCxRec1'),
      'TD32 must resolve cross-unit TComplexRec.CxR1 -> TCxRec1');

    // --- Head-to-head: RSM does NOT resolve the cross-unit edge. ---
    // This is the deliverable: TD32 succeeds where RSM fails, on the
    // SAME binary. We log the RSM result for all three so the failure
    // pattern is visible, and assert that the cross-unit ones are NOT
    // resolved by RSM (its M.TypeIdx comes back positional/0).
    RsmResolves('TWithHeader', 'WhdrHeader', 'TWhdrHeader');  // expected: RSM resolves (same-unit)

    Assert.IsFalse(RsmResolves('TXAdresse', 'Anschrift', 'TXAnschrift'),
      'RSM is expected to FAIL the cross-unit TXAdresse.Anschrift edge; ' +
      'if this now passes, the RSM gap closed and the head-to-head premise changed');
    Assert.IsFalse(RsmResolves('TComplexRec', 'CxR1', 'TCxRec1'),
      'RSM is expected to FAIL the cross-unit TComplexRec.CxR1 edge; ' +
      'if this now passes, the RSM gap closed and the head-to-head premise changed');
  finally
    Rsm.Free;
    Td32.Free;
  end;
end;

procedure TTd32VsRsmCrossUnitEdgeTests.Td32StandaloneTdsResolvesSameMemberTypeEdges;
// Proves the DETACHABLE .tds path: dcc32's -VT detaches the TD32 CodeView
// stream into a standalone <project>.tds (FB09/FB0A at offset 0), leaving a
// lean EXE. The SAME member -> sub-record TYPE resolution must work when the
// reader loads that separate file via LoadFromTdsFile, not the embedded TD32.
var
  TdsPath: String;
  Td32   : TTd32Reader;

  // Resolves parent.field -> sub-record name via the TD32 reader loaded from
  // the standalone .tds; logs the raw TypeIdx + resolved name (raw evidence).
  function Td32Resolves(const AParent, AField, AExpected: String): Boolean;
  var
    M  : TTd32ClassMember;
    Idx: Integer;
    ResolvedName: String;
  begin
    Result := False;
    ResolvedName := '<unresolved>';
    if not Td32.FindClassMember(AParent, AField, M) then
    begin
      System.Writeln(Format(
        'TDS   %s.%s : FindClassMember FAILED (member not found)',
        [AParent, AField]));
      Exit;
    end;
    Idx := Td32.FindStructByTypeIdx(M.TypeIdx);
    if Idx >= 0 then
      ResolvedName := Td32.Classes[Idx].Name;
    Result := (Idx >= 0) and SameText(ResolvedName, AExpected);
    System.Writeln(Format(
      'TDS   %s.%s -> M.TypeIdx=$%x  FindStructByTypeIdx=%d  name="%s"  (expected "%s")  => %s',
      [AParent, AField, M.TypeIdx, Idx, ResolvedName, AExpected,
       BoolToStr(Result, True)]));
  end;

begin
  TdsPath := ChangeFileExt(ResolveTargetPath('DebugTarget.exe', False), '.tds');
  if not TFile.Exists(TdsPath) then
  begin
    Assert.Pass('DebugTarget.tds fixture missing (' + TdsPath +
      '); build DebugTarget with dcc32 -VT to emit it. Skipping standalone-TDS test.');
    Exit;
  end;

  Td32 := TTd32Reader.Create;
  try
    Td32.LoadFromTdsFile(TdsPath);

    Assert.IsTrue(Td32.Classes.Count > 0,
      'TD32 reader extracted no structured types from the standalone .tds');

    // SAME-UNIT control + the two CROSS-UNIT edges, exactly as the embedded
    // path proves them — but now sourced from the detached .tds file.
    Assert.IsTrue(Td32Resolves('TWithHeader', 'WhdrHeader', 'TWhdrHeader'),
      'TDS must resolve same-unit TWithHeader.WhdrHeader -> TWhdrHeader');
    Assert.IsTrue(Td32Resolves('TXAdresse', 'Anschrift', 'TXAnschrift'),
      'TDS must resolve cross-unit TXAdresse.Anschrift -> TXAnschrift');
    Assert.IsTrue(Td32Resolves('TComplexRec', 'CxR1', 'TCxRec1'),
      'TDS must resolve cross-unit TComplexRec.CxR1 -> TCxRec1');
  finally
    Td32.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTd32VsRsmCrossUnitEdgeTests);

end.
