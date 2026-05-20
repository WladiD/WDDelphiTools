// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Rsm.Reader;

// Contract tests for TRsmReader's public lookups. The scanner-side
// tests (Test.DPT.Rsm.Scanner) cover the raw byte-stream output;
// these tests verify the value the reader's post-process passes
// (Format-A linking, class-parent derivation, cross-unit type-id
// resolution) add on top of that data via the Find/Is/TryGet API.
// Test.DPT.Rsm.LocalsReader exercises the wider behavioural surface
// through the legacy alias and is the regression fence; here we
// focus on what the facade itself contributes.

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TRsmReaderTests = class
  private
    function ResolveExePath(AUse64Bit: Boolean): String;
  public
    /// Loading a missing file leaves all lookups returning their
    /// sentinel "not found" values.
    [Test]
    procedure TestLoadFromMissingFileLeavesEmpty;

    /// Format-A field linking produces a non-zero TypeIdx on class
    /// fields whose declared type matches a class/record registered
    /// in the type registry. We don't pin the value (varies by
    /// build) but require that at least one Format-A member came
    /// through with a non-zero TypeIdx.
    [Test]
    procedure TestFormatALinkingPopulatesMemberTypeIds;

    /// Class-parent derivation joins TInner -> TDeepDerived chain via
    /// the layout-matching pass. FindClassMember resolves inherited
    /// fields by walking the chain.
    [Test]
    procedure TestInheritedFieldVisibleViaFindClassMember;

    /// Enum oracle. TLightStatus is a program-local enum; its primary
    /// 2-byte id must be marked as an enum and lsGreen at ordinal 2
    /// resolves to its identifier.
    [Test]
    procedure TestEnumOracleResolvesLightStatus;

    /// Cross-unit enum via the $25 $8A-prefix + $2A alias linking
    /// path: TThreadPriority's tpHigher (ordinal 4) resolves through
    /// the name-based prefix disambiguator.
    [Test]
    procedure TestEnumOracleResolvesCrossUnitThreadPriority;

    /// Record-typed global proximity resolution. GGlobalEnumRec is a
    /// record-typed module-level global; the dotted-walk uses
    /// FindBestRecordForGlobalAndField to recover its type from
    /// byte-stream proximity. The result must be a skRecord index.
    [Test]
    procedure TestFindBestRecordForGlobalAndField;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,

  DPT.Rsm.Model,
  DPT.Rsm.Reader;

function TRsmReaderTests.ResolveExePath(AUse64Bit: Boolean): String;
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

procedure TRsmReaderTests.TestLoadFromMissingFileLeavesEmpty;
var
  R: TRsmReader;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile('Z:\definitely-does-not-exist\nope.exe');
    Assert.AreEqual<Integer>(0, R.Procs.Count);
    Assert.AreEqual<Integer>(0, R.Classes.Count);
    Assert.AreEqual<Integer>(-1, R.FindProcByName('LocalsProcedure'));
    Assert.AreEqual<UInt32>(0, R.FindGlobalTypeIdx('GGlobalLight'));
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestFormatALinkingPopulatesMemberTypeIds;
var
  R    : TRsmReader;
  I, M : Integer;
  Any  : Boolean;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    Any := False;
    for I := 0 to R.Classes.Count - 1 do
    begin
      if R.Classes[I].Kind <> skClass then Continue;
      for M := 0 to R.Classes[I].Members.Count - 1 do
        if R.Classes[I].Members[M].TypeIdx <> 0 then
        begin
          Any := True;
          Break;
        end;
      if Any then Break;
    end;
    Assert.IsTrue(Any,
      'No class member came through Format-A linking with a non-zero TypeIdx');
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestInheritedFieldVisibleViaFindClassMember;
var
  R     : TRsmReader;
  Member: TRsmClassMember;
  Found : Boolean;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    // TDeepDerived inherits TDerived inherits TInner; TInner's
    // FInnerInt should be reachable from TDeepDerived through the
    // parent chain that DeriveClassParents builds.
    Found := R.FindClassMember('TDeepDerived', 'FInnerInt', Member);
    Assert.IsTrue(Found,
      'TDeepDerived.FInnerInt not visible -- ' +
      'class-parent derivation did not link the inheritance chain');
    Assert.AreEqual('FInnerInt', Member.Name);
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestEnumOracleResolvesLightStatus;
var
  R     : TRsmReader;
  TypeId: UInt32;
  Name  : String;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    // GGlobalLight is typed TLightStatus -- its encoded type id must
    // appear in the enum oracle, and ordinal 2 must resolve to lsGreen.
    TypeId := R.FindGlobalTypeIdx('GGlobalLight');
    Assert.IsTrue(TypeId <> 0, 'GGlobalLight type id is zero');
    Assert.IsTrue(R.IsEnumTypeId(TypeId),
      'IsEnumTypeId returned False for TLightStatus');
    Assert.IsTrue(R.TryGetEnumConstantName(TypeId, 2, Name),
      'TryGetEnumConstantName failed for TLightStatus ordinal 2');
    Assert.AreEqual('lsGreen', Name);
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestEnumOracleResolvesCrossUnitThreadPriority;
var
  R     : TRsmReader;
  TypeId: UInt32;
  Name  : String;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    // The primary id for TThreadPriority is discovered through the
    // $2A registry; cross-unit linking joins it to the secondary
    // that carries the $25 $8A-prefix constants. The "tp" prefix
    // hint disambiguates against any aliased secondaries that may
    // share the same id space.
    TypeId := R.FindTypeIdByName('TThreadPriority');
    Assert.IsTrue(TypeId <> 0, 'TThreadPriority not registered by ScanTypeRegistry');
    Assert.IsTrue(R.IsEnumTypeId(TypeId),
      'IsEnumTypeId returned False for TThreadPriority');
    Assert.IsTrue(R.TryGetEnumConstantName(TypeId, 4, Name, 'tp'),
      'TryGetEnumConstantName failed for TThreadPriority ordinal 4 with prefix tp');
    Assert.AreEqual('tpHigher', Name);
  finally
    R.Free;
  end;
end;

procedure TRsmReaderTests.TestFindBestRecordForGlobalAndField;
var
  R   : TRsmReader;
  Idx : Integer;
begin
  R := TRsmReader.Create;
  try
    R.LoadFromFile(ResolveExePath(False));
    // GGlobalEnumRec : TEnumHostRec, with field FLight. The
    // proximity-based resolver should find TEnumHostRec by walking
    // records whose body carries FLight closest to the global's
    // file offset.
    Idx := R.FindBestRecordForGlobalAndField('GGlobalEnumRec', 'FLight');
    Assert.IsTrue(Idx >= 0, 'No record matched for GGlobalEnumRec.FLight');
    Assert.IsTrue(R.Classes[Idx].Kind = skRecord, 'Matched class is not a record');
  finally
    R.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRsmReaderTests);

end.
