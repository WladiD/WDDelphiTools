// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Rsm.Scanner;

// Direct tests for TRsmScanner: loads the DebugTarget .rsm fixture
// and asserts the raw scan output (procs, classes, globals, enum
// constants, type aliases). These tests bypass the
// TRsmReader facade so a regression in the byte-stream walk
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
    procedure DoTestGlobalVADecodedFromGlobalRecord(AUse64Bit: Boolean);
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

    /// Pins the finding for the $2A "KindFlag" byte at body offset +0
    /// (RSM format reference, gap 6.6). It is NOT a type-kind
    /// discriminator: classes, records and enums each appear with
    /// MULTIPLE flag values across the file. What it does carry is a
    /// body-shape selector -- Flag=$00 marks a "narrow" entry whose
    /// payload after the primary id is a single $00 pad byte;
    /// Flag=$20 marks a "wide" entry whose payload at +5..+8 is a
    /// non-zero 4-byte block (the secondary candidate the existing
    /// scanner already reads at +7,+8 for enum-bridging). This test
    /// captures both shapes against the DebugTarget fixture.
    [Test]
    procedure Test2ATypeRegistryFlagIsBodyShapeNotKind32;

    /// Pins the $27 / $20 module-global VA decoding (RSM format
    /// gap §6.8 closed). The 4-byte slot after the type id encodes
    /// (Value shl 4) | $07, same scheme as $28 PROC_TAG. On Win32
    /// the decoded Value is the absolute VA (image base already
    /// included); on Win64 it is the RVA relative to image base
    /// $140000000. Compares against ground truth from the .map
    /// file for both $27 (primitive: GGlobalInt) and $20
    /// (structured: GFieldHost) forms.
    [Test]
    procedure TestGlobalVADecodedFromGlobalRecord32;
    [Test]
    procedure TestGlobalVADecodedFromGlobalRecord64;

    /// Pins the actual sparse-ordinal enum encoding (RSM format §6.1).
    /// The original "different $03 emission shape" hypothesis is
    /// REFUTED: the linker emits NO $03 ENUM_DEF record at all for
    /// sparse enums. The per-element ordinals come through the
    /// existing $25 program-local channel and decode correctly today
    /// (seAlpha=1, seBeta=5, seGamma=11 for TSparseEnum). The
    /// consequence is that <c>EnumDefs</c> does not list TSparseEnum;
    /// callers must use <c>TryGetEnumConstantName</c> or another
    /// resolver path that doesn't rely on the EnumDef list.
    [Test]
    procedure TestSparseEnumResolvesViaEnumConstNames32;

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
  System.Classes,
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

procedure TRsmScannerTests.Test2ATypeRegistryFlagIsBodyShapeNotKind32;
// Pins the body-shape vs. kind finding for the $2A entry byte at +0.
// We locate the registry entries for three DebugTarget-declared
// program-local types whose primary id hi byte is $2E:
//
//   TInner        - class  - expected Flag=$20 (wide body)
//   TLightStatus  - enum   - expected Flag=$00 (narrow body)
//   TEnumHostRec  - record - expected Flag=$00 (narrow body)
//
// The (class -> $20) and (enum -> $00) pair alone refutes any kind
// hypothesis: an enum and a record share the same Flag, and a class
// uses a different Flag than the same-kind-as-record TEnumHostRec.
// We also assert the shape correlate: Flag=$00 entries have body[5]
// == $00 (single pad byte before the next record); Flag=$20 entries
// have body[5] != $00 (the secondary-candidate / typeinfo-ref slot
// starts there).
type
  TFound = record
    Pos  : NativeInt;
    Flag : Byte;
    B5   : Byte;
    PriHi: Byte;
  end;
var
  S       : TRsmScanner;
  Hits    : IKeyValue<String, TFound>;

  function TryFindRegistryEntry(const AName: String;
    out AFound: TFound): Boolean;
  var
    P, EndPos: NativeInt;
    NL       : Byte;
    Name     : String;
    PriHi    : Byte;
  begin
    Result := False;
    EndPos := S.Sz - 12;
    P := 4;
    while P < EndPos do
    begin
      if S.ByteAt(P) = TRsmTag.TYPE_REGISTRY_TAG then
      begin
        NL := S.ByteAt(P + 1);
        if (NL = Length(AName)) and
           (P + 2 + NL + 10 < S.Sz) and
           (S.ByteAt(P + 2) = Byte(Ord('T'))) and
           (S.ByteAt(P + 2 + NL + 1) = $00) and
           (S.ByteAt(P + 2 + NL + 2) = $00) and
           S.ReadIdentifier(P + 1, Name) and
           SameText(Name, AName) then
        begin
          PriHi := S.ByteAt(P + 2 + NL + 4);
          // Filter to the program-local cluster: hi byte $2E. Any
          // false-positive $2A bytes scattered earlier in the file
          // won't carry $2E in this slot, so we skip them.
          if PriHi = $2E then
          begin
            AFound.Pos   := P;
            AFound.Flag  := S.ByteAt(P + 2 + NL + 0);
            AFound.B5    := S.ByteAt(P + 2 + NL + 5);
            AFound.PriHi := PriHi;
            Exit(True);
          end;
        end;
      end;
      Inc(P);
    end;
  end;

  procedure Expect(const AName: String);
  var
    F: TFound;
  begin
    Assert.IsTrue(TryFindRegistryEntry(AName, F),
      AName + ' has no $2A registry entry with PriHi=$2E');
    Hits[AName] := F;
  end;

var
  ClassEntry, EnumEntry, RecordEntry: TFound;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty -- need DebugTarget.rsm');

    Hits := Collections.NewPlainKeyValue<String, TFound>;
    Expect('TInner');
    Expect('TLightStatus');
    Expect('TEnumHostRec');

    ClassEntry  := Hits['TInner'];
    EnumEntry   := Hits['TLightStatus'];
    RecordEntry := Hits['TEnumHostRec'];

    // The decoded finding: enum and class disagree on Flag, but
    // enum and record AGREE. So Flag does NOT mark the type kind.
    Assert.AreNotEqual<Byte>(ClassEntry.Flag, EnumEntry.Flag,
      'class TInner and enum TLightStatus share the same Flag -- ' +
      'no longer disprovable as a kind discriminator');
    Assert.AreEqual<Byte>(EnumEntry.Flag, RecordEntry.Flag,
      'enum TLightStatus and record TEnumHostRec disagree on Flag -- ' +
      'kind-hypothesis would have required them to agree (both not ' +
      'class) -- need to revisit finding');

    // Pinned values for DebugTarget (program-local cluster). If the
    // linker changes its emission shape these will trip and the
    // doc/§4.8 needs revisiting.
    Assert.AreEqual<Byte>($20, ClassEntry.Flag,
      'TInner Flag drifted from $20');
    Assert.AreEqual<Byte>($00, EnumEntry.Flag,
      'TLightStatus Flag drifted from $00');
    Assert.AreEqual<Byte>($00, RecordEntry.Flag,
      'TEnumHostRec Flag drifted from $00');

    // Body-shape correlate: Flag=$00 entries have a single $00 pad
    // byte at body[5]; Flag=$20 entries open the wide payload at
    // body[5] with a non-zero byte (a secondary candidate the
    // existing scanner already reads at +7,+8).
    Assert.AreNotEqual<Byte>($00, ClassEntry.B5,
      'TInner is a wide-body entry but body[5] is $00');
    Assert.AreEqual<Byte>($00, EnumEntry.B5,
      'TLightStatus is a narrow-body entry but body[5] is non-zero');
    Assert.AreEqual<Byte>($00, RecordEntry.B5,
      'TEnumHostRec is a narrow-body entry but body[5] is non-zero');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.DoTestGlobalVADecodedFromGlobalRecord(
  AUse64Bit: Boolean);
// Pins the $27 / $20 module-global VA decoding for both forms.
// Ground truth is the .map file:
//   Win32: image base $00400000, .data $004E2000, .bss $004E7000.
//   Win64: image base $140000000, .data $140153000, .bss $140176000.
// The scanner exposes the decoded value as the absolute VA on
// Win32 (image base already included) and the RVA-from-image-base
// on Win64; both extracted as (DWORD shr 4) from the 4-byte slot.
var
  S            : TRsmScanner;
  ExpectedInt  : UInt32;  // GGlobalInt  ($27, Integer primitive)
  ExpectedLight: UInt32;  // GGlobalLight ($27, TLightStatus enum, 2-byte id)
  ExpectedField: UInt32;  // GFieldHost  ($20, TFieldStatusHost record)
  Va           : UInt32;
begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(AUse64Bit));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty');

    if AUse64Bit then
    begin
      // .map entries (segment:offset) + segment start (RVA):
      //   GGlobalInt   0002:00021EC4 + 0002 starts at RVA $153000
      //   GGlobalLight 0002:00021FCC
      //   GFieldHost   0003:0000C710 + 0003 starts at RVA $176000
      ExpectedInt   := $00153000 + $00021EC4;  // $174EC4
      ExpectedLight := $00153000 + $00021FCC;  // $174FCC
      ExpectedField := $00176000 + $0000C710;  // $182710
    end
    else
    begin
      //   GGlobalInt   0003:00003BD4 + 0003 starts at VA $004E2000
      //   GGlobalLight 0003:00003BDC
      //   GFieldHost   0004:000068D0 + 0004 starts at VA $004E7000
      ExpectedInt   := $004E2000 + $00003BD4;  // $004E5BD4
      ExpectedLight := $004E2000 + $00003BDC;  // $004E5BDC
      ExpectedField := $004E7000 + $000068D0;  // $004ED8D0
    end;

    Assert.IsTrue(S.GlobalVa.TryGetValue('gglobalint', Va),
      'GGlobalInt VA not decoded');
    Assert.AreEqual<UInt32>(ExpectedInt, Va,
      'GGlobalInt VA mismatch vs .map');

    Assert.IsTrue(S.GlobalVa.TryGetValue('ggloballight', Va),
      'GGlobalLight VA not decoded');
    Assert.AreEqual<UInt32>(ExpectedLight, Va,
      'GGlobalLight VA mismatch vs .map');

    Assert.IsTrue(S.GlobalByName.ContainsKey('gfieldhost'),
      'GFieldHost not even registered by name -- $20 module-global ' +
      'handler did not run, likely due to FScanInProc still being True');
    Assert.IsTrue(S.GlobalVa.TryGetValue('gfieldhost', Va),
      'GFieldHost VA not decoded ($20 module-global form)');
    Assert.AreEqual<UInt32>(ExpectedField, Va,
      'GFieldHost VA mismatch vs .map');
  finally
    S.Free;
  end;
end;

procedure TRsmScannerTests.TestGlobalVADecodedFromGlobalRecord32;
begin
  DoTestGlobalVADecodedFromGlobalRecord(False);
end;

procedure TRsmScannerTests.TestGlobalVADecodedFromGlobalRecord64;
begin
  DoTestGlobalVADecodedFromGlobalRecord(True);
end;

procedure TRsmScannerTests.TestSparseEnumResolvesViaEnumConstNames32;
// Pins the sparse-ordinal enum decoding. DebugTarget declares
// TSparseEnum = (seAlpha = 1, seBeta = 5, seGamma = 11). The linker
// does NOT emit a $03 ENUM_DEF record for this enum (verified by
// walking the byte stream below). The $25 program-local channel
// however emits each constant with its explicit ordinal, so
// EnumConstNames carries seAlpha=1, seBeta=5, seGamma=11 -- the
// resolver layer that consumes typeId:ord lookups already works.
var
  S       : TRsmScanner;
  HasDef  : Boolean;
  HitVals : array[1..11] of String;

  function FindEnumDefByName(const AName: String): Boolean;
  var
    P, EndPos: NativeInt;
    NL       : Byte;
    Name     : String;
  begin
    Result := False;
    EndPos := S.Sz - 16;
    P := 4;
    while P < EndPos do
    begin
      if (S.ByteAt(P) = TRsmTag.ENUM_DEF_TAG) and
         (S.ByteAt(P + 1) = Length(AName)) then
      begin
        NL := S.ByteAt(P + 1);
        if (NL >= 2) and (NL <= 40) and (P + 2 + NL < S.Sz) and
           S.ReadIdentifier(P + 1, Name) and
           SameText(Name, AName) then
          Exit(True);
      end;
      Inc(P);
    end;
  end;

begin
  S := TRsmScanner.Create;
  try
    S.LoadFromFile(ResolveExePath(False));
    Assert.IsTrue(S.Sz > 8, 'RSM buffer empty -- need DebugTarget.rsm');

    // (a) Sparse enums get NO $03 ENUM_DEF record.
    HasDef := FindEnumDefByName('TSparseEnum');
    Assert.IsFalse(HasDef,
      'Linker emitted a $03 ENUM_DEF for TSparseEnum -- §6.1 ' +
      'finding needs revisiting; sparse enums were previously ' +
      'observed to be absent from the $03 channel');

    // (b) But the $25 channel DID emit one constant per element
    // with the explicit ordinal as the value. We can't predict
    // TSparseEnum's typeId statically (linker-assigned), so iterate
    // EnumConstNames and pick out the ordinals by the constant name.
    for var I := 1 to 11 do HitVals[I] := '';
    for var Pair in S.EnumConstNames do
    begin
      var ColonPos := Pos(':', Pair.Key);
      if ColonPos = 0 then Continue;
      var OrdinalStr := Copy(Pair.Key, ColonPos + 1, MaxInt);
      var Ord: Integer;
      if not TryStrToInt(OrdinalStr, Ord) then Continue;
      if (Ord < 1) or (Ord > 11) then Continue;
      if SameText(Pair.Value, 'seAlpha') or
         SameText(Pair.Value, 'seBeta')  or
         SameText(Pair.Value, 'seGamma') then
        HitVals[Ord] := Pair.Value;
    end;

    Assert.AreEqual('seAlpha', HitVals[1],
      'seAlpha did not surface at ordinal 1');
    Assert.AreEqual('seBeta', HitVals[5],
      'seBeta did not surface at ordinal 5');
    Assert.AreEqual('seGamma', HitVals[11],
      'seGamma did not surface at ordinal 11');
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
