// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Rsm.Model;

// Smoke tests for the DPT.Rsm.Model unit. These exist mostly as a
// guard against accidental edits to the tag-byte values or the
// acronym helper -- the RSM reader's correctness depends on both
// being stable, and a typo here would otherwise only surface as a
// reader-level test failure with no pointer back to the model.

interface

uses
  DUnitX.TestFramework,

  mormot.core.collections,

  DPT.Rsm.Model;

type

  [TestFixture]
  TTestRsmModel = class
  public
    /// Tag byte values are wire-format constants: changing one
    /// silently rewrites how the scanner interprets the byte stream.
    /// Pin them with explicit assertions so a typo cannot slip
    /// through code review.
    [Test]
    procedure TestTagConstants;

    /// Default-initialised records compile and zero-init as expected.
    /// The scanner relies on this (it calls Default(TRsmProc) etc.
    /// before populating fields), so a regression where a record gains
    /// a member with non-default zero-state would silently break the
    /// scan path.
    [Test]
    procedure TestDefaultInit;

    /// IList<T>-typed Members on TRsmClassInfo: a freshly created list
    /// is empty and assignable. The reader walks this list heavily;
    /// confirms the mORMot collection contract still holds.
    [Test]
    procedure TestClassInfoMembersList;

    /// PascalCase acronym derivation. This is the prefix the name-
    /// based enum resolver uses to disambiguate aliased secondaries
    /// (e.g. TWindowState -> "ws" picks wsNormal over the unrelated
    /// csNone that shares the same secondary id in TFW).
    [Test]
    procedure TestBuildPascalAcronym;
  end;

implementation

procedure TTestRsmModel.TestTagConstants;
begin
  Assert.AreEqual<Byte>($28, TRsmTag.PROC_TAG,         'PROC_TAG');
  Assert.AreEqual<Byte>($20, TRsmTag.LOCAL_TAG,        'LOCAL_TAG');
  Assert.AreEqual<Byte>($22, TRsmTag.PARAM_TAG,        'PARAM_TAG');
  Assert.AreEqual<Byte>($21, TRsmTag.REGVAR_TAG,       'REGVAR_TAG');
  Assert.AreEqual<Byte>($27, TRsmTag.GLOBAL_PRIM_TAG,  'GLOBAL_PRIM_TAG');
  Assert.AreEqual<Byte>($25, TRsmTag.ENUM_CONST_TAG,   'ENUM_CONST_TAG');
  Assert.AreEqual<Byte>($2A, TRsmTag.TYPE_REGISTRY_TAG,'TYPE_REGISTRY_TAG');
  Assert.AreEqual<Byte>($0E, TRsmTag.RECORD_SENTINEL,  'RECORD_SENTINEL');
  Assert.AreEqual<Byte>($63, TRsmTag.SCOPE_END,        'SCOPE_END');
  Assert.AreEqual<Byte>($64, TRsmTag.UNIT_USE_INTRO,   'UNIT_USE_INTRO');
  Assert.AreEqual<Byte>($66, TRsmTag.UNIT_USE_TYPE,    'UNIT_USE_TYPE');
  Assert.AreEqual<Byte>($67, TRsmTag.UNIT_USE_SYMBOL,  'UNIT_USE_SYMBOL');
  Assert.AreEqual<Byte>($70, TRsmTag.UNIT_USE_FILE,    'UNIT_USE_FILE');
  // 'CSH7' little-endian DWORD on disk.
  Assert.AreEqual<UInt32>($37485343, TRsmTag.SigCSH7, 'SigCSH7');
end;

procedure TTestRsmModel.TestDefaultInit;
var
  L: TRsmLocal;
  P: TRsmProc;
  M: TRsmClassMember;
  C: TRsmClassInfo;
begin
  L := Default(TRsmLocal);
  Assert.AreEqual('', L.Name);
  Assert.AreEqual<Int32>(0, L.BpOffset);
  Assert.AreEqual<UInt32>(0, L.TypeIdx);
  Assert.IsTrue(L.Kind = lkBpRel, 'default kind is lkBpRel');
  Assert.AreEqual<Byte>(0, L.RegParamIdx);

  P := Default(TRsmProc);
  Assert.AreEqual('', P.Name);
  Assert.AreEqual<NativeUInt>(0, P.SegmentOffset);
  Assert.AreEqual<NativeUInt>(0, P.Size);
  Assert.IsNull(P.Locals, 'Locals interface defaults to nil');

  M := Default(TRsmClassMember);
  Assert.AreEqual('', M.Name);
  Assert.AreEqual<UInt32>(0, M.Offset);
  Assert.AreEqual<UInt32>(0, M.TypeIdx);
  Assert.AreEqual<UInt32>(0, M.Size);
  Assert.AreEqual<UInt16>(0, M.PrimitiveTypeId);

  C := Default(TRsmClassInfo);
  Assert.AreEqual('', C.Name);
  Assert.AreEqual<UInt32>(0, C.TypeIdx);
  Assert.IsTrue(C.Kind = skClass, 'default kind is skClass');
  Assert.IsNull(C.Members, 'Members interface defaults to nil');
  Assert.AreEqual('', C.ParentName);
  Assert.AreEqual<UInt32>(0, C.ParentRawId);
end;

procedure TTestRsmModel.TestClassInfoMembersList;
var
  C: TRsmClassInfo;
  M: TRsmClassMember;
begin
  C := Default(TRsmClassInfo);
  C.Name := 'TFoo';
  C.Kind := skRecord;
  C.Members := Collections.NewPlainList<TRsmClassMember>;
  Assert.AreEqual<Integer>(0, C.Members.Count);

  M := Default(TRsmClassMember);
  M.Name := 'FBar';
  M.Offset := 4;
  C.Members.Add(M);
  Assert.AreEqual<Integer>(1, C.Members.Count);
  Assert.AreEqual('FBar', C.Members[0].Name);
  Assert.AreEqual<UInt32>(4, C.Members[0].Offset);
end;

procedure TTestRsmModel.TestBuildPascalAcronym;
begin
  // After the 'T' prefix has been stripped by the caller, the
  // remaining type name still starts with an uppercase letter --
  // the helper takes EVERY uppercase letter and lowercases it.
  Assert.AreEqual('ws', BuildPascalAcronym('WindowState'));
  Assert.AreEqual('tp', BuildPascalAcronym('ThreadPriority'));
  Assert.AreEqual('fs', BuildPascalAcronym('FontStyle'));
  Assert.AreEqual('ls', BuildPascalAcronym('LightStatus'));
  // Empty string -> empty acronym (no surprises for edge cases).
  Assert.AreEqual('', BuildPascalAcronym(''));
  // All-lowercase yields empty (defensive: helper never invents
  // letters that weren't capitalised in the source name).
  Assert.AreEqual('', BuildPascalAcronym('foobar'));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestRsmModel);

end.
