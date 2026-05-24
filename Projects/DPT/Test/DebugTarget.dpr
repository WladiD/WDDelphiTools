program DebugTarget;
{$APPTYPE CONSOLE}
{$O-}
{$D+}
{$STACKFRAMES ON}
uses System.SysUtils, System.Classes, Winapi.Windows, DebugTarget.EnumAlpha, DebugTarget.EnumBeta, DebugTarget.EnumGamma;
var
  GGlobalInt: Integer = Integer($87654321);
  GGlobalString: string = 'Hello Global';
  GGlobalObject: TStringList;
procedure DeepProcedure;
var LocalInt: Integer;
begin
  LocalInt := $12345678;
  Writeln('Deep ', LocalInt); Flush(Output); // Line 15
end;
procedure TargetProcedure;
begin
  Writeln('Target'); Flush(Output); // Line 19
  try
    Abort; // Raises EAbort
  except
    // Catch so it doesn't crash the app
  end;
  try
    raise Exception.Create('Test Exception'); // Line 26
  except
  end;
  DeepProcedure; // Line 29
end;
procedure LocalsProcedure;
var
  LocalA: Integer;
  LocalB: Int64;
  LocalC: Cardinal;
  LocalString: string; LocalShort: ShortString;
  LocalObject: TStringList;
begin
  LocalA := $12345678;
  LocalB := Int64($1122334455667788);
  LocalC := $DEADBEEF;
  LocalString := 'Hello Local'; LocalShort := 'Hello Local Short';
  LocalObject := TStringList.Create;
  try
    Writeln('Locals ', LocalA, ' ', LocalB, ' ', LocalC); Flush(Output); // Line 45 - locals breakpoint here
  finally
    LocalObject.Free;
  end;
end;
type
  TInner = class FInnerInt: Integer; FInnerStr: string; end;
  TDerived = class(TInner)
  public
    FDerivedExtra : Integer;
    FDerivedLabel : string;
    // Instance method so the evaluator can resolve <Self>.<FieldName>
    // at a breakpoint inside an instance method. Self is the implicit
    // first register-passed argument (EAX on x86, RCX on x64) -- the
    // GetLocalAddress / lkRegister fix already covers reading Self;
    // class-discovery in the RSM reader must also recognise TDerived
    // as a class even when it declares methods, so inherited and own
    // fields stay reachable via FindClassMember.
    procedure TouchSelf;
  end;
  TDeepDerived = class(TDerived)
  private
    FDeepFlag : Integer;
  end;
  // Stand-alone host for the "register-passed Self with a class-typed
  // field" auto-detect probe. Independent of TDerived so adding the
  // class-typed field here can't shift offsets the structural tests
  // baseline against the TInner / TDerived / TDeepDerived chain.
  TClassFieldHost = class
    FHostNested : TInner;
    FHostInt    : Integer;
    // RTL-defined class as a field type so the auto-detect probe
    // covers the cross-unit shape that real-world code uses (e.g.
    // VCL forms with TList<...> / TStringList / framework-defined
    // class fields). TStringList lives in System.Classes, so its
    // RSM type-id sits in a different namespace from this program's
    // own type registry.
    FHostRtlList : TStringList;
    procedure TouchSelf;
  end;
  TOuter = class FOuterInt: Integer; FOuterInner: TInner; FOuterStr: string; end;
  TPoint2D = record FX, FY: Integer; end;
  TRect2D  = record FTopLeft, FBottomRight: TPoint2D; end;
  TPair    = record FObj: TInner; FLabel: string; end;
  TWithRec = class
    FOrigin    : TPoint2D;
    FBounds    : TRect2D;
    FPair      : TPair;
    FNestedObj : TInner;
    FName      : string;
  end;
  TMixedRec = record
    FMixedInt   : Integer;
    FMixedInt64 : Int64;
    FMixedStr   : string;
  end;
  TPoint3D = record
    FX, FY, FZ : Integer;
  end;
  // Mirrors the "TRecHeader prefix" pattern observed in real-world
  // Delphi binaries. A correct field-offset computation must place 
  // WhdrId at offset 8 (after Magic+Ver), NOT at offset 0. 
  // The MCP-level evaluate test verifies that by reading back 
  // the sentinel values via dotted paths.
  TWhdrHeader = record
    WhdrMagic : UInt32;
    WhdrVer   : UInt32;
  end;
  TWithHeader = record
    WhdrHeader   : TWhdrHeader;
    WhdrId       : Integer;
    WhdrShortStr : ShortString;
    WhdrLongStr  : string;
  end;
  // Mirrors the "case Byte of" variant pattern from TFW's TKonsApl.
  // The RSM emits each variant case as its own $02-prefixed field
  // record but with overlapping offsets (next-offset DWORD does NOT
  // advance between siblings of the same case branch). A correct
  // scanner must surface every named variant field with its own
  // offset, so dotted paths into either branch resolve correctly
  // and the field-name fallback in the evaluator finds the right
  // owning record.
  TVariantSlot = record
    VrCommon : Integer;
    case Byte of
      0: (VrAsInt   : Integer);
      1: (VrAsByteA : Byte;
          VrAsByteB : Byte;
          VrAsByteC : Byte;
          VrAsByteD : Byte);
  end;
  // Mirrors the "TMdt prefix" pattern seen in real-world Delphi
  // binaries: a record whose first non-header fields are Word /
  // Byte. type:"int" reads on those fields must clamp to the
  // declared width or the result visibly concatenates the next
  // field's bytes (Word at offset 0 followed by another Word at
  // offset 2 = naive 4-byte read returns Lo or (Hi shl 16) instead
  // of just Lo). Sentinels chosen so the wrong-width path is
  // visibly different from the correct one.
  TNarrowInts = packed record
    NiWord    : Word;
    NiWord2   : Word;
    NiByte    : Byte;
    NiByte2   : Byte;
    NiInteger : Integer;
  end;
  // IEEE 754 float types. Sentinels chosen so they're representable
  // exactly (powers-of-2 sums) -- the evaluator's formatter must
  // produce the literal expected string regardless of locale-specific
  // FormatSettings (Invariant settings are required on the formatter
  // side). Extended is 80-bit on Win32 and aliased to Double on
  // Win64, so the formatter must consult FTargetIs32Bit to decide
  // whether to read 10 or 8 bytes.
  TFloats = record
    FSingle   : Single;     // 4 bytes
    FDouble   : Double;     // 8 bytes
    FExtended : Extended;   // 10 bytes (Win32) / 8 bytes (Win64)
  end;
  // Bundle the primitive types we don't already cover via TMixedRec
  // (Integer, Int64, string) or TFloats (Single, Double, Extended):
  // AnsiString, WideString, ShortString, Boolean and Currency. Used by
  // the evaluate-tool auto-detection tests to verify the
  // primitive-type-id discovery path works across the full range of
  // Delphi-built-in scalar types.
  TPrimitives = record
    FAnsi  : AnsiString;
    FWide  : WideString;
    FShort : ShortString;
    FBool  : Boolean;
    FCurr  : Currency;
  end;
// Repro point for the register-passed class-pointer dotted-walk bug.

type
  // Multi-level inheritance through an RTL ancestor. Mirrors the
  // TFW scenario where TFormMain inherits TComponent through several
  // intermediate classes: own fields on the leaf class work, but
  // fields declared on TComponent (FName, FTag, ...) are unreachable
  // because the field-lookup walker either stops at the leaf or hits
  // a forward-stub when it crosses into RTL-defined classes. The
  // RSM does contain TComponent (and TPersistent), so the chain CAN
  // be walked if FindClassMember recurses through ParentName
  // reliably for RTL classes.
  TMyComp = class(System.Classes.TComponent)
  public
    FCustomFlag : Integer;
  end;
  // Same hierarchy as TMyComp but with NO own fields. The RSM-parent
  // heuristic (offset matching, tolerant or not) cannot bridge this
  // class to TComponent because FirstOffs is 0 -- there is no own
  // field whose offset could anchor the match. Only a live-VMT
  // ancestor walk surfaces FName / FTag on this class. Mirrors the
  // TFW intermediate-class case where CMainForm / CBaseForm may
  // declare no own fields the RSM scanner can lock onto.
  TEmptyChild = class(System.Classes.TComponent)
  end;
// Repro point for the inherited-field-via-RTL-ancestor walk.
// AComp is a register-passed reference to a TComponent-derived
// instance. AComp.FCustomFlag is the leaf class's own field;
// AComp.FName / AComp.FTag are inherited from TComponent (two
// hops up: TMyComp -> TPersistent -> TComponent in modern Delphi
// (TComponent inherits TPersistent directly)).
procedure TouchRtlInheritedComp(AComp: TMyComp);
begin
  Writeln('TouchRIC ', AComp.FCustomFlag, ' ', AComp.Name); Flush(Output); // Line 209 - inherited-field bp here
end;
// Repro point for the VMT-walk inheritance fallback: AEmpty has no
// own fields, so the RSM-driven ParentName chain cannot reach
// TComponent for it. Resolving AEmpty.FName / AEmpty.FTag requires
// reading the live VMT's parent pointer at run time.
procedure TouchEmptyChild(AEmpty: TEmptyChild);
begin
  Writeln('TouchEC ', AEmpty.Name); Flush(Output); // Line 217 - empty-child inherited-field bp here
end;

// Repro point for the register-passed class-pointer dotted-walk bug.
// A plain procedure (no Self, no class methods) so the RSM reader's
// class-discovery heuristic stays on its current code path: <AInner>
// is a TInner reference passed in the first register slot (EAX on
// x86, RCX on x64) with NO stack slot. <AOther> is the second
// register-passed class param (EDX / RDX). Without the fix, the
// dotted-walk's first segment uses a stale base-pointer offset and
// dereferences a junk address; with the fix, the register value is
// used directly as the instance pointer.
procedure TouchRegClassParam(AInner: TInner; AOther: TInner);
begin
  Writeln('TouchRCP ', AInner.FInnerInt, ' ', AOther.FInnerInt); Flush(Output); // Line 231 - register-class-param bp here
end;
// Instance-method repro for Self.<Field> dotted navigation. Self is
// the implicit first register-passed argument; at a breakpoint placed
// on the first statement, EAX/RCX still hold the receiver and no
// stack slot exists for Self. Tests both own (FDerivedExtra,
// FDerivedLabel) and inherited (FInnerInt, FInnerStr) field access
// through the same register-Self code path.
procedure TDerived.TouchSelf;
begin
  Writeln('TouchSelf ', FInnerInt, ' ', FDerivedExtra); Flush(Output); // Line 241 - Self.<Field> bp here
end;
// Instance-method repro for auto-detection of "register-passed Self
// with a CLASS-typed terminal field" (Self.FHostNested). Mirrors the
// shape that VCL forms produce all the time (Self.FNotification,
// Self.FListBox etc.) and exercises the terminal Member.TypeIdx
// lookup against the RSM type registry.
procedure TClassFieldHost.TouchSelf;
begin
  Writeln('TouchSelfClass ', FHostInt); Flush(Output); // Line 250 - Self.<ClassField> bp here
end;
// Enum types for the enum-formatter probe. Placed AFTER the existing
// BP-bearing methods so adding them doesn't shift line 209 / 217 /
// 231 / 241 / 250 markers in the structural test suite.
type
  TLightStatus = (lsRed, lsYellow, lsGreen);
  TEnumHostRec = record
    FLight  : TLightStatus;
    FFlag   : Integer;
  end; TEnumHostRecPacked = packed record FLight: TLightStatus; FFlag: Integer; end; TEnumVariantHost = packed record FTag: Integer; case Integer of 0: (FFlag1: Boolean); 5: (FInner: TEnumHostRec); end;
type
  // Class holding a cross-unit enum-typed field whose name follows
  // the Delphi convention <c>F&lt;TypeName-without-T&gt;</c>. With
  // the name-based enum resolver, auto-detect picks the right enum
  // by inferring the type name from the field name.
  TThPriHost = class
    FThreadPriority : TThreadPriority;
  end;
var
  GGlobalLight    : TLightStatus = lsGreen;
  GGlobalEnumRec  : TEnumHostRec;
  GGlobalThPriHost: TThPriHost;
procedure EnumProbeProcedure;
var
  LocalLight : TLightStatus; LocalEnumRec : TEnumHostRec; LocalEnumRecPacked : TEnumHostRecPacked; LocalVariantHost : TEnumVariantHost;
begin
  LocalLight := lsYellow;
  GGlobalEnumRec.FLight := lsRed; LocalEnumRec.FLight := lsRed; LocalEnumRecPacked.FLight := lsRed; LocalVariantHost.FInner.FLight := lsRed;
  GGlobalEnumRec.FFlag  := Integer($EEEEEEEE);
  GGlobalThPriHost.FThreadPriority := tpHigher;
  Writeln('EnumProbe ', Ord(LocalLight), ' ',                           // Line 281 - enum bp here
          Ord(GGlobalLight), ' ', Ord(GGlobalEnumRec.FLight), ' ',
          Ord(GGlobalThPriHost.FThreadPriority), ' ', Ord(LocalEnumRec.FLight), ' ', Ord(LocalEnumRecPacked.FLight), ' ', Ord(LocalVariantHost.FInner.FLight)); Flush(Output);
end;
procedure OpenArrayStringProcedure(const AItems: array of string);
var
  LocalCount : Integer;
  LocalFirst : string;
begin
  LocalCount := Length(AItems);
  if LocalCount > 0 then
    LocalFirst := AItems[0]
  else
    LocalFirst := '';
  Writeln('OAS ', LocalCount, ' ', LocalFirst);
end;
procedure OpenArrayIntProcedure(const ANumbers: array of Integer);
var
  LocalSum: Integer;
  I       : Integer;
begin
  LocalSum := 0;
  for I := Low(ANumbers) to High(ANumbers) do
    LocalSum := LocalSum + ANumbers[I];
  Writeln('OAI ', LocalSum);
end;
procedure EdgeCaseLocalsProcedure;
type
  TFlags = set of (Flag1, Flag2, Flag3);
var
  LocalVariant : Variant;
  LocalIntf    : IInterface;
  LocalBytes   : TBytes;
  LocalPoint   : TPoint2D;
  LocalDouble  : Double;
  LocalBoolean : Boolean;
  LocalChar    : Char;
  LocalFlags   : TFlags;
  LocalPointer : Pointer;
  LocalInner   : TInner;
begin
  LocalVariant := 42;
  LocalIntf    := nil;
  SetLength(LocalBytes, 16);
  LocalPoint.FX := $11; LocalPoint.FY := $22;
  LocalDouble  := 3.14;
  LocalBoolean := True;
  LocalChar    := 'X';
  LocalFlags   := [Flag1, Flag3];
  LocalPointer := nil;
  LocalInner   := nil;
  if LocalBoolean then
    Writeln('Edge ', Length(LocalBytes), ' ', LocalPoint.FX, ' ',
            LocalDouble:0:2, ' ', LocalChar);
end;
// Cross-unit enum collision probe. Three separately-declared
// TStatus enums (from DebugTarget.EnumAlpha / .EnumBeta / .EnumGamma)
// share the type name "TStatus" -- the RSM name index is
// last-wins, so only one of them can be reached by name. Each
// variable's stored RSM type-id is unique though, so the
// evaluate path that goes "name -> type-id -> enum constants"
// should still return the right unit's enum.
//
// Alpha is contiguous (0..2); Beta and Gamma are sparse with
// explicit ordinal values, so the byte at the variable's VA
// equals the element's EXPLICIT ordinal -- not its list index.
var
  GStatusAlpha : DebugTarget.EnumAlpha.TStatus =
                   DebugTarget.EnumAlpha.saRunning;  // contiguous ord 1
  GStatusBeta  : DebugTarget.EnumBeta.TStatus =
                   DebugTarget.EnumBeta.sbStopped;   // sparse explicit 10
  GStatusGamma : DebugTarget.EnumGamma.TStatus =
                   DebugTarget.EnumGamma.scInit;     // sparse explicit 7
  // Unqualified declaration: resolves to the LAST unit in the uses
  // clause carrying TStatus, i.e. DebugTarget.EnumGamma.TStatus.
  GStatusUnq   : TStatus = scWorking;                // sparse explicit 13 (Gamma)
  // Bridge-gap probe. Variable name carries no unit-suffix hint
  // ("Toggle" doesn't end-match any of EnumAlpha/EnumBeta/EnumGamma),
  // so the name-based resolver in TRsmReader falls back to the
  // last-declared TStatus def with a matching ordinal. The byte
  // at the variable's VA is Alpha.saRunning (ord 1), but Beta also
  // has an element at ord 1 (sbIdle) and is declared LATER, so
  // the resolver returns the wrong unit's constant. This is the
  // "Variable-typeId -> Primary echter Bridge" gap.
  GStatusToggle: DebugTarget.EnumAlpha.TStatus =
                   DebugTarget.EnumAlpha.saRunning;  // contiguous ord 1
procedure CrossUnitEnumProbe;
begin
  // Re-assign explicitly to defeat any constant folding the
  // optimizer might apply -- the breakpoint reads the .data
  // values, which must be the four distinct ordinals above.
  GStatusAlpha := DebugTarget.EnumAlpha.saRunning;
  GStatusBeta  := DebugTarget.EnumBeta.sbStopped;
  GStatusGamma := DebugTarget.EnumGamma.scInit;
  GStatusUnq   := scWorking;
  GStatusToggle := DebugTarget.EnumAlpha.saRunning;
  Writeln('CUE ', Ord(GStatusAlpha), ' ', Ord(GStatusBeta), ' ',     // Line 377 - cross-unit-enum log (bp at line 372)
          Ord(GStatusGamma), ' ', Ord(GStatusUnq), ' ',
          Ord(GStatusToggle)); Flush(Output);
end;
// Repro for the auto-detect bugs we observed on TFW
// UserKonsOutlook. After a large unstructured field (CalendarID)
// the Format-A linker stops populating Member.PrimitiveTypeId for
// subsequent fields, so auto-detect can't reach the field's
// declared enum type via the structural path. Two failure shapes
// emerge depending on whether the field name happens to match an
// unrelated registered enum:
//
//   * SyncDirection -- T+"SyncDirection" = "TSyncDirection",
//     which IS registered (the dedicated red-herring enum below
//     anchored by GSyncDirectionAnchor). Name-based fallback
//     succeeds and returns "sdBeta (1)" -- the wrong enum's
//     constant -- instead of the field's actual type's
//     "fskDraft (1)". Mirrors UserKonsOutlook.SyncDirection.
//   * SyncStatus -- T+"SyncStatus" = "TSyncStatus" is NOT
//     registered, name-based fallback bails, and the evaluator
//     ends with the "no RSM type metadata" hint instead of
//     "fukActive (1)". Mirrors UserKonsOutlook.SyncStatus.
type
  // Red-herring enums anchored with a direct $25 record so
  // TryGetEnumConstantName succeeds. Constants use unique
  // prefixes so wrong-enum results are distinguishable from
  // right-enum results in the test assertion.
  TSyncDirection   = (sdAlpha, sdBeta, sdGamma);
  // Field's ACTUAL types. The name doesn't follow the
  // F<TypeNameNoT> convention, and the linker doesn't carry
  // their type id onto the Member after CalendarID's array gap.
  TFieldStatusKind = (fskHidden, fskDraft, fskPublished);
  TFieldUnregKind  = (fukIdle, fukActive, fukHalted);
  TFieldStatusHost = packed record
    AllowSync      : Boolean;
    ForceFirstSync : Boolean;
    SyncReminder   : Boolean;
    CalendarName   : string[255];
    CalendarID     : array[1..512] of Byte;
    SyncDirection  : TFieldStatusKind;
    SyncStatus     : TFieldUnregKind;
    Filler         : array[0..254] of Byte;
  end;
var
  GFieldHost           : TFieldStatusHost;
  // Anchors TSyncDirection in the $2A / $25 registry so the
  // name-based fallback can find it for the misleading match.
  GSyncDirectionAnchor : TSyncDirection;
procedure NameClashEnumProbe;
begin
  GFieldHost.SyncDirection := fskDraft;     // ord 1 of TFieldStatusKind
  GFieldHost.SyncStatus    := fukActive;    // ord 1 of TFieldUnregKind
  GSyncDirectionAnchor     := sdAlpha;      // anchor for TSyncDirection
  Writeln('NameClash ', Ord(GFieldHost.SyncDirection), ' ',                  // Line 430 - name-clash bp here
          Ord(GFieldHost.SyncStatus), ' ', Ord(GSyncDirectionAnchor)); Flush(Output);
end;
// Sparse / explicit-value enum. The current $03 ENUM_DEF decoder
// assigns sequential ordinals 0..N-1 (see RSM format gap §6.1); this
// fixture forces the linker to emit a shape that carries the
// explicit values 1, 5, 11 so the decoded ordinals can be pinned.
// Placed AFTER all BP markers (line 430 was the last) so adding it
// does not shift existing BP line numbers.
type
  TSparseEnum = (seAlpha = 1, seBeta = 5, seGamma = 11);
var
  GGlobalSparse : TSparseEnum;
procedure SparseEnumProbe;
begin
  GGlobalSparse := seGamma;
  Writeln('SparseProbe ', Ord(GGlobalSparse)); Flush(Output);
end;
// Class whose fields do NOT follow the conventional F-prefix
// naming (PlainInt instead of FPlainInt etc.). The current backward
// class-field walker rejects these via a hard `Name[1] = 'F'` guard
// (RSM format §6.3); this fixture forces the linker to emit a class
// definition the walker will miss until the guard is replaced with
// a structural anchor.
type
  TNoFPrefixHost = class
  protected
    PlainInt   : Integer;
    PlainLabel : string;
  end;
var
  GGlobalNoFPrefix : TNoFPrefixHost;
procedure NoFPrefixProbe;
begin
  GGlobalNoFPrefix := TNoFPrefixHost.Create;
  GGlobalNoFPrefix.PlainInt   := Integer($BADCAFE0);
  GGlobalNoFPrefix.PlainLabel := 'no-F-prefix';
  Writeln('NoFPrefixProbe ', GGlobalNoFPrefix.PlainInt, ' ',
          GGlobalNoFPrefix.PlainLabel); Flush(Output);
end;
var
  GGlobalInt64       : Int64       = Int64($1122334455667788);
  GGlobalAnsi        : AnsiString  = 'Hello Ansi';
  GGlobalWide        : WideString  = 'Hello Wide';
  GGlobalShort       : ShortString = 'Hello Short';
  GGlobalEmptyString : string      = '';
  GGlobalNilObject   : TObject     = nil;
  GGlobalOuter       : TOuter;
  GGlobalDerived     : TDerived;
  GGlobalDeep        : TDeepDerived;
  GGlobalClassHost   : TClassFieldHost;
  GGlobalWithRec     : TWithRec;
  GGlobalMixed       : TMixedRec;
  GGlobalP3D         : TPoint3D;
  GGlobalWithHeader  : TWithHeader;
  GGlobalVarRec      : TVariantSlot;
  GGlobalNarrow      : TNarrowInts;
  GGlobalFloats      : TFloats;
  GGlobalPrim        : TPrimitives;
  GGlobalComp        : TMyComp;
  GGlobalEmptyChild  : TEmptyChild;
begin
  GGlobalInt := $11223344;
  GGlobalObject := TStringList.Create;
  // Push two items so TStringList.FCount = 2 -- gives a verifiable
  // sentinel for the non-TComponent inheritance navigation test
  // (TStringList -> TStrings -> TPersistent -> TObject).
  GGlobalObject.Add('first-line');
  GGlobalObject.Add('second-line');
  GGlobalOuter := TOuter.Create;
  GGlobalOuter.FOuterInt := $11111111;
  GGlobalOuter.FOuterStr := 'Hello Outer';
  GGlobalOuter.FOuterInner := TInner.Create;
  GGlobalOuter.FOuterInner.FInnerInt := $22222222;
  GGlobalOuter.FOuterInner.FInnerStr := 'Hello Inner';
  GGlobalDerived := TDerived.Create;
  GGlobalDerived.FInnerInt     := Integer($C1C1C1C1);
  GGlobalDerived.FInnerStr     := 'Inherited Inner';
  GGlobalDerived.FDerivedExtra := Integer($C2C2C2C2);
  GGlobalDerived.FDerivedLabel := 'Derived-C3';
  GGlobalDeep := TDeepDerived.Create;
  GGlobalDeep.FInnerInt     := Integer($D1D1D1D1);
  GGlobalDeep.FInnerStr     := 'Deep Inner';
  GGlobalDeep.FDerivedExtra := Integer($D2D2D2D2);
  GGlobalDeep.FDerivedLabel := 'Deep Derived';
  GGlobalDeep.FDeepFlag     := Integer($D3D3D3D3);
  GGlobalClassHost := TClassFieldHost.Create;
  GGlobalClassHost.FHostNested := TInner.Create;
  GGlobalClassHost.FHostNested.FInnerInt := Integer($E1E1E1E1);
  GGlobalClassHost.FHostInt    := Integer($E2E2E2E2);
  GGlobalClassHost.FHostRtlList := TStringList.Create;
  GGlobalClassHost.FHostRtlList.Add('Host-RTL');
  GGlobalThPriHost := TThPriHost.Create;
  GGlobalWithRec := TWithRec.Create;
  GGlobalWithRec.FOrigin.FX := $11111111;
  GGlobalWithRec.FOrigin.FY := $22222222;
  GGlobalWithRec.FBounds.FTopLeft.FX     := $33333333;
  GGlobalWithRec.FBounds.FTopLeft.FY     := $44444444;
  GGlobalWithRec.FBounds.FBottomRight.FX := $55555555;
  GGlobalWithRec.FBounds.FBottomRight.FY := $66666666;
  GGlobalWithRec.FPair.FObj := TInner.Create;
  GGlobalWithRec.FPair.FObj.FInnerInt := $77777777;
  GGlobalWithRec.FPair.FObj.FInnerStr := 'Inner via Pair';
  GGlobalWithRec.FPair.FLabel := 'PairLabel';
  GGlobalWithRec.FNestedObj := TInner.Create;
  GGlobalWithRec.FNestedObj.FInnerInt := Integer($88888888);
  GGlobalWithRec.FNestedObj.FInnerStr := 'Inner via Record';
  GGlobalWithRec.FName := 'WithRec';
  GGlobalMixed.FMixedInt   := Integer($A1A1A1A1);
  GGlobalMixed.FMixedInt64 := Int64($A2A2A2A2A2A2A2A2);
  GGlobalMixed.FMixedStr   := 'Mixed-A3';
  GGlobalP3D.FX := Integer($B1B1B1B1);
  GGlobalP3D.FY := Integer($B2B2B2B2);
  GGlobalP3D.FZ := Integer($B3B3B3B3);
  // Header-prefixed record: WhdrId at offset 8, WhdrShortStr at
  // offset 12, WhdrLongStr right after the shortstring buffer.
  // The MCP-level evaluate test reads each via a dotted path and
  // checks the sentinel values come back, which only happens when
  // the field-offset arithmetic in the dotted walk is correct.
  GGlobalWithHeader.WhdrHeader.WhdrMagic := Integer($E1E1E1E1);
  GGlobalWithHeader.WhdrHeader.WhdrVer   := Integer($E2E2E2E2);
  GGlobalWithHeader.WhdrId                := Integer($E3E3E3E3);
  GGlobalWithHeader.WhdrShortStr          := 'whdr-short';
  GGlobalWithHeader.WhdrLongStr           := 'Whdr-Long-E5';
  // Variant record with case Byte of. VrCommon at offset 0,
  // VrAsInt and VrAsByteA-D share offset 4. The Int and Bytes
  // sentinels are chosen so test assertions can distinguish a
  // wrong-overlay-offset read from a correct one.
  GGlobalVarRec.VrCommon  := Integer($F0F0F0F0);
  GGlobalVarRec.VrAsInt   := Integer($F2F2F2F2);
  // Narrow-int record: NiWord sentinel = 1 (low byte 0x01, high
  // byte 0x00); NiWord2 sentinel = $1234. A naive 4-byte read at
  // NiWord's offset would return 1 or ($1234 shl 16) = $12340001;
  // a correct width-clamped read returns 1. NiByte sentinel = $A5,
  // NiByte2 = $5A so a wrong-width read on NiByte is visibly
  // different from $A5.
  GGlobalNarrow.NiWord    := 1;
  GGlobalNarrow.NiWord2   := $1234;
  GGlobalNarrow.NiByte    := $A5;
  GGlobalNarrow.NiByte2   := $5A;
  GGlobalNarrow.NiInteger := Integer($DEADBEEF);
  // Float sentinels: powers-of-2 sums so they're exactly
  // representable in IEEE 754 and survive any locale-specific
  // FormatSettings (the evaluator must use Invariant on output).
  GGlobalFloats.FSingle   := 1.5;
  GGlobalFloats.FDouble   := 2.25;
  GGlobalFloats.FExtended := 3.125;
  // Primitive bundle for auto-type-detection coverage.
  GGlobalPrim.FAnsi   := 'Prim-Ansi';
  GGlobalPrim.FWide   := 'Prim-Wide';
  GGlobalPrim.FShort  := 'Prim-Short';
  GGlobalPrim.FBool   := True;
  GGlobalPrim.FCurr   := 1234.5678;
  // Multi-level-inheritance instance whose own field lives on TMyComp
  // and whose Name / Tag fields live on TComponent in the System.Classes
  // RTL unit. Used by the inherited-RTL-field navigation test.
  GGlobalComp             := TMyComp.Create(nil);
  GGlobalComp.FCustomFlag := Integer($7E7E7E7E);
  GGlobalComp.Name        := 'CompName';
  GGlobalComp.Tag         := Integer($7D7D7D7D);
  // No-own-fields probe: forces the VMT-walk fallback to surface
  // TComponent's FName / FTag.
  GGlobalEmptyChild       := TEmptyChild.Create(nil);
  GGlobalEmptyChild.Name  := 'EmptyName';
  GGlobalEmptyChild.Tag   := Integer($7C7C7C7C);
  // Touch the value-type globals so the linker keeps them in .bss/.data
  // instead of dead-code-eliminating them. The values themselves are
  // already set by the typed-constant initializers above.
  if (GGlobalInt64 = 0) and (GGlobalShort = '') and (GGlobalNilObject = nil) then ;
  try
    Writeln(ErrOutput, 'stderr-tag-line'); Flush(ErrOutput);
    OutputDebugString('ods-tag-line');
    TargetProcedure;
    // Reach the body of TouchRegClassParam with both class parameters
    // live as register-passed locals. Used by the MCP-evaluate test
    // for class-field navigation through a register-passed object
    // pointer (the GetLocalAddress / lkRegister fix).
    TouchRegClassParam(GGlobalOuter.FOuterInner, GGlobalDerived);
    // Reach the body of TDerived.TouchSelf with Self live as the
    // first register-passed argument. Drives the Self.<Field> dotted
    // navigation test and exposes the RSM class-discovery gap for
    // classes that declare methods.
    GGlobalDerived.TouchSelf;
    // Reach TClassFieldHost.TouchSelf with Self holding a TClassFieldHost
    // instance in the first register slot. Drives the
    // Self.<ClassField> auto-detect test.
    GGlobalClassHost.TouchSelf;
    // Reach EnumProbeProcedure so the enum auto-detect test has
    // a live BP context with both a local enum and a record-nested
    // enum field initialised.
    EnumProbeProcedure;
    // Reach CrossUnitEnumProbe so the cross-unit-enum test has a
    // live BP context with the three same-name TStatus globals
    // (each from a different unit) plus the unqualified one whose
    // type was picked by uses-order.
    CrossUnitEnumProbe;
    // Reach NameClashEnumProbe so the misleading-name-fallback test
    // can hit the BP with both Status (clashes with registered
    // TStatus) and Whatever (no T+Name match anywhere) initialised
    // to ordinal 1 of their respective real types.
    NameClashEnumProbe;
    SparseEnumProbe;
    NoFPrefixProbe;
    // Reach the body of TouchRtlInheritedComp with AComp live as a
    // register-passed reference. Drives the inherited-RTL-field
    // navigation test (Name / Tag declared on TComponent, walked
    // via the FindClassMember ancestor chain).
    TouchRtlInheritedComp(GGlobalComp);
    // Reach TouchEmptyChild so the VMT-walk fallback test for
    // ancestors via the no-own-fields path has a live BP context.
    TouchEmptyChild(GGlobalEmptyChild);
    LocalsProcedure;
    if GGlobalInt = -1 then EdgeCaseLocalsProcedure;
    if GGlobalInt = -1 then OpenArrayStringProcedure(['A', 'B', 'C']);
    if GGlobalInt = -1 then OpenArrayIntProcedure([1, 2, 3, 4]);
  finally
    GGlobalOuter.FOuterInner.Free;
    GGlobalOuter.Free;
    GGlobalDerived.Free;
    GGlobalDeep.Free;
    GGlobalClassHost.FHostNested.Free;
    GGlobalClassHost.FHostRtlList.Free;
    GGlobalClassHost.Free;
    GGlobalThPriHost.Free;
    GGlobalWithRec.FPair.FObj.Free;
    GGlobalWithRec.FNestedObj.Free;
    GGlobalWithRec.Free;
    GGlobalComp.Free;
    GGlobalEmptyChild.Free;
    GGlobalObject.Free;
  end;
end.
