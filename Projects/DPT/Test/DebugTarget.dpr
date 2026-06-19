program DebugTarget;
{$APPTYPE CONSOLE}
{$O-}
{$D+}
{$STACKFRAMES ON}
uses System.SysUtils, System.Classes, Winapi.Windows, DebugTarget.EnumAlpha, DebugTarget.EnumBeta, DebugTarget.EnumGamma, DebugTarget.RecTypes;
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
  // Per-visibility sections so §6.14 can pin the anchor's byte +2
  // marker for every Delphi visibility (private, strict private,
  // protected, strict protected, public, published). Each section
  // carries exactly one field so the per-field anchor byte +2 is
  // unambiguously associated with the section's visibility. The
  // names are deliberately non-F-prefixed so the §6.3 structural-
  // anchor walker still has to find them (no name-shape shortcut).
  TNoFPrefixHost = class
  private
    PrivateInt        : Integer;
  strict private
    StrictPrivateInt  : Integer;
  protected
    PlainInt          : Integer;
    PlainLabel        : string;
  strict protected
    StrictProtectedInt: Integer;
  public
    PublicInt         : Integer;
    procedure TouchStrict;
  published
    // Delphi `published` accepts only class/interface fields (E2217),
    // so we use a class-typed field here.  The mere act of a non-
    // empty `published` section flips the class into {$M+} mode (the
    // Delphi compiler emits W1055 to that effect), so the linker
    // would treat earlier sections' fields as RTTI-eligible too.
    PublishedObj      : TObject;
  end;

procedure TNoFPrefixHost.TouchStrict;
begin
  StrictPrivateInt   := Integer($BADCAFE4);
  StrictProtectedInt := Integer($BADCAFE5);
  PlainInt           := PlainInt + StrictPrivateInt + StrictProtectedInt;
end;

var
  GGlobalNoFPrefix : TNoFPrefixHost;
procedure NoFPrefixProbe;
begin
  GGlobalNoFPrefix := TNoFPrefixHost.Create;
  GGlobalNoFPrefix.PrivateInt         := Integer($BADCAFE1);
  GGlobalNoFPrefix.PlainInt           := Integer($BADCAFE0);
  GGlobalNoFPrefix.PlainLabel         := 'no-F-prefix';
  GGlobalNoFPrefix.PublicInt          := Integer($BADCAFE2);
  GGlobalNoFPrefix.PublishedObj       := TObject.Create;
  GGlobalNoFPrefix.TouchStrict;
  GGlobalNoFPrefix.PlainInt           := GGlobalNoFPrefix.PlainInt +
    GGlobalNoFPrefix.PrivateInt +
    GGlobalNoFPrefix.PublicInt;
  Writeln('NoFPrefixProbe ', GGlobalNoFPrefix.PlainInt, ' ',
          GGlobalNoFPrefix.PlainLabel); Flush(Output);
end;
// Register-passed enum parameter probe (§6.15). Forces the linker to
// emit a $21-PARAM record whose payload encodes a same-unit enum's
// type id. The scanner's HandleParamRecord currently reads only the
// LOW byte unless Hi is $2E/$2F -- this proc exercises the
// non-$2E/$2F path so the bug surfaces.
procedure TouchRegEnumParam(AStatusLight: TLightStatus;
                            AStatusPriority: TThreadPriority);
begin
  Writeln('RegEnum ', Ord(AStatusLight), ' ',                          // Line 516 - register-enum-param bp here
          Ord(AStatusPriority)); Flush(Output);
end;
// Property fixture: a class exposing read access through both
// styles the Delphi compiler emits differently:
//
//   - `property PlainProp: Integer read FPlainInt;`
//       Compiler inlines reads as a direct field load -- so the
//       Reader's existing field-by-name resolution should find the
//       underlying FPlainInt and the test asserts that BOTH the
//       property name and the underlying field name resolve.
//   - `property CalcProp: Integer read GetCalcInt;`
//       Compiler emits a call to the getter. No backing field
//       directly visible; the property name cannot be resolved
//       through field navigation alone -- the Reader has to either
//       expose the getter method or fail cleanly.
//
// Both shapes get a sentinel value so the pin test can assert
// concrete byte values rather than "non-zero".
//
// Placed AFTER the last BP marker (line 516) per the rsm-expert
// skill's BP-line discipline; the new BP marker added below is at
// a higher line number and doesn't shift earlier markers.
type
  TPropHost = class
  private
    FPlainInt   : Integer;
    FBackingStr : string;
    FBackedInt  : Integer;
    function GetCalcInt: Integer;
    function GetGreeting: string;
  public
    constructor Create;
    /// Direct-field-backed property; compiler resolves reads to
    /// FPlainInt.
    property PlainProp: Integer read FPlainInt;
    /// Getter-backed integer property; compiler emits a call to
    /// GetCalcInt.
    property CalcProp: Integer read GetCalcInt;
    /// Getter-backed string property; exercises managed-return path.
    property Greeting: string read GetGreeting;
  end;
var
  GGlobalPropHost: TPropHost;
constructor TPropHost.Create;
begin
  inherited;
  FPlainInt   := Integer($AABBCCDD);
  FBackingStr := 'World';
  FBackedInt  := Integer($12345678);
end;
function TPropHost.GetCalcInt: Integer;
begin
  Result := FBackedInt + 1;     // = $12345679
end;
function TPropHost.GetGreeting: string;
begin
  Result := 'Hello, ' + FBackingStr;
end;
procedure PropertyProbe;
begin
  GGlobalPropHost := TPropHost.Create;
  Writeln('PropProbe ',                                                // Line 578 - property bp here
          GGlobalPropHost.PlainProp, ' ',
          GGlobalPropHost.CalcProp, ' ',
          GGlobalPropHost.Greeting); Flush(Output);
end;
// Record property fixture: Delphi records can declare properties
// since the language got operator overloading & method support.
// The compiler may emit $31 records for these too, but they belong
// to a record (skRecord), not a class (skClass). The PropertyLinker
// must surface these analogously to class properties.
type
  TPropRec = record
  private
    FRecPlain   : Integer;
    FRecBacking : string;
    FRecBacked  : Integer;
    function GetRecCalc: Integer;
    function GetRecLabel: string;
  public
    procedure Init;
    /// Direct-field-backed property; reads inline FRecPlain.
    property RecPlainProp: Integer read FRecPlain;
    /// Getter-backed integer property on a record.
    property RecCalcProp: Integer read GetRecCalc;
    /// Getter-backed string property on a record.
    property RecLabel: string read GetRecLabel;
  end;
var
  GGlobalPropRec: TPropRec;
procedure TPropRec.Init;
begin
  FRecPlain   := Integer($CAFEBABE);
  FRecBacking := 'Record';
  FRecBacked  := Integer($DEADC0DE);
end;
function TPropRec.GetRecCalc: Integer;
begin
  Result := FRecBacked + 2;     // = $DEADC0E0
end;
function TPropRec.GetRecLabel: string;
begin
  Result := 'Tag: ' + FRecBacking;
end;
procedure RecordPropertyProbe;
begin
  GGlobalPropRec.Init;
  Writeln('RecPropProbe ',                                             // Line 624 - record-property bp here
          GGlobalPropRec.RecPlainProp, ' ',
          GGlobalPropRec.RecCalcProp, ' ',
          GGlobalPropRec.RecLabel); Flush(Output);
end;
// Stale-Self-register repro. Self is the implicit first register-passed
// argument (EAX on Win32 / RCX on Win64); Delphi's prologue spills it to
// a frame-pointer home slot ([ebp-04] on Win32, [rbp+disp] on Win64) and
// every later field access reloads it from there. The breakpoint below
// sits on a line that does NOT reference Self, so the inbound register
// still holds the PRECEDING line's arithmetic result, not Self. Reading
// Self from the live register therefore yields garbage; the debugger must
// read it from the prologue spill home. The MCP-evaluate pin
// (TestMcpEvaluateSelfFromSpillHomeAfterRegisterClobber) breaks here on
// the unfixed reader and passes once Self is sourced from the home slot.
// Placed AFTER the last BP marker (line 624) per the rsm-expert BP-line
// discipline; the new BP marker is at a higher line and shifts nothing
// above it.
type
  TStaleSelfHost = class
    FMarker : Integer;
    procedure Probe;
  end;
var
  GGlobalStaleSelf : TStaleSelfHost;
procedure TStaleSelfHost.Probe;
var
  LScratch : Integer;
begin
  LScratch := GGlobalInt * 3 + 7;        // clobbers EAX/RCX with a non-Self value
  Writeln('StaleSelf ', LScratch);       // Line 654 - stale-Self bp here (no Self on this line)
  Writeln('StaleMarker ', FMarker); Flush(Output);  // Self referenced only AFTER the bp line
end;
// §6.18 fixture: pointer-to-record dotted-walk traversal. Mirrors TFW's
// `TFormAd.FAd: PAd` shape -- a class field whose declared type is a
// pointer alias to a record. The dotted walk currently navigates
// class -> class and class -> inline record, but has no
// "deref pointer-to-record, then record-hop into the pointed-to type"
// step, so `evaluate Self.FRecPtr.FMixedInt` fails at the FRecPtr hop.
// This fixture isolates that gap: TPtrToRecHost holds FRecPtr: PMixedRec
// pointing at GGlobalPtrToRecRec (a TMixedRec with a known sentinel).
// Once the walker grows the deref hop, the pin test in
// Test.DPT.MCP.Server flips from red to green.
type
  PMixedRec     = ^TMixedRec;
  TPtrToRecHost = class
    FRecPtr : PMixedRec;
    procedure Probe;
  end;
var
  GGlobalPtrToRecHost : TPtrToRecHost;
  GGlobalPtrToRecRec  : TMixedRec;
procedure TPtrToRecHost.Probe;
begin
  Writeln('PtrToRec ', FRecPtr^.FMixedInt); Flush(Output);  // Line 678 - ptr-to-rec bp here
end;
// §6.19 fixture: ambiguous member-name in the §6.18 record fallback.
// Two records (TAmbig619Target, TAmbig619Sibling) BOTH carry a
// field named FShared619. Once both are in the .rsm, the §6.18 name-
// based unique-match guard (FbMatchCount > 1) bails on
// `Self.FAmbig619Ptr.FShared619` and the chain returns "Failed to
// evaluate". Mirrors TFW's `TFormAd.FAd.Land` shape, where `Land`
// appears on TAd plus several Anschrift-style sibling records.
// The §6.19 closure populates Member.TypeIdx for pointer-to-record
// fields with the TARGET record's class idx so the next iteration's
// context-priming routes straight into a record-hop and the fallback
// is bypassed entirely.
//
// Sentinel layout: target = $19191919 (visible in the green case);
// sibling = $DEADBEEF (poison -- a wrong-record pick would surface
// $DEADBEEF and the pin would notice).
type
  TAmbig619Target = record
    FShared619       : Integer;
    FUniqueTarget619 : Integer;
  end;
  TAmbig619Sibling = record
    FShared619        : Integer;
    FUniqueSibling619 : Integer;
  end;
  PAmbig619Target = ^TAmbig619Target;
  TAmbig619Host = class
    FAmbig619Ptr : PAmbig619Target;
    procedure Probe;
  end;
var
  GGlobalAmbig619Host    : TAmbig619Host;
  GGlobalAmbig619Target  : TAmbig619Target;
  GGlobalAmbig619Sibling : TAmbig619Sibling;
procedure TAmbig619Host.Probe;
begin
  Writeln('Ambig619 ', FAmbig619Ptr^.FShared619); Flush(Output);  // Line 715 - §6.19 ambig-fallback bp here
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
// === §6.31 non-T-prefixed class fixture ===
// StructDiscoverer's hot-loop quick-reject only accepted class/record
// names starting with 'T', so a 'C'-prefixed class (this is how
// Test.Lib's CJwksValidator / CTestJwksValidator are named) was never
// discovered -- FindClassByName returned nothing and every
// "evaluate Obj.FField" on such an instance failed even when the local
// pointer itself was decoded correctly. See DPT.Rsm.Format.md §6.31.
type
  CNonTPrefixHost = class
  public
    FNonTInt : Integer;
    FNonTStr : string;
    FNonTObj : TInner;
  end;
var
  GGlobalNonTHost : CNonTPrefixHost;
// === §6.30 inline-var local fixture ===
// Delphi inline variable declarations ("var X := expr;" in the body)
// emit $20 LOCAL records with a DIFFERENT body anchor than classic
// "var" blocks: $66 $00 $01 $04 instead of $66 $00 $00, which pushes
// the BPRel offset byte one position to the right (payload +5 instead
// of +4). LocalsProcedure / EdgeCaseLocalsProcedure only exercise the
// classic form, so the scanner's mis-decode of the inline form went
// unpinned until Test.Lib.CTestJwksValidator.Validate_Success surfaced
// it (every local mis-decoded; "evaluate V.FExpectedTenantId" failed
// because V's offset came out ~128x too large). LocalIVBig forces at
// least one trailing inline var to a large frame offset so the wide /
// continuation offset form (if any) is exercised, not just the
// single-byte form. See DPT.Rsm.Format.md §4.4.
procedure InlineVarLocalsProcedure;
begin
  var LocalIVStr: string := 'Hello Inline';
  var LocalIVObj: TStringList := TStringList.Create;
  var LocalIVRec: TPoint2D;
  LocalIVRec.FX := $41; LocalIVRec.FY := $42;
  var LocalIVInt: Integer := Integer($1234ABCD);
  // 512-byte buffer so LocalIVTail below sits past the single-byte
  // ShortInt offset range (|2*offset| > 127), exercising the wide form.
  var LocalIVBig: array[0..127] of Integer;
  LocalIVBig[0] := Integer($5A5A5A5A); LocalIVBig[127] := Integer($A5A5A5A5);
  var LocalIVTail: string := 'Inline Tail';
  try
    Writeln('InlineVar ', LocalIVStr, ' ', LocalIVObj.Count, ' ',
            LocalIVRec.FX, ' ', LocalIVInt, ' ', LocalIVBig[0], ' ',
            LocalIVBig[127], ' ', LocalIVTail); Flush(Output); // Line 768 - inline-var locals bp here
  finally
    LocalIVObj.Free;
  end;
end;
// §6.30 companion: enough inline Integer vars that the trailing ones
// land past EBP-64, where a 1-byte primitive type pairs with the WIDE
// (LSB-continuation) BPRel offset form -- the (1-byte type, 2-byte
// offset) combo the InlineVarLocalsProcedure fixture above doesn't
// reach (there the only wide offset, LocalIVBig, is a 2-byte type).
procedure InlineVarManyIntsProcedure;
begin
  var IV01: Integer := 1;   var IV02: Integer := IV01 + 1;
  var IV03: Integer := IV02 + 1; var IV04: Integer := IV03 + 1;
  var IV05: Integer := IV04 + 1; var IV06: Integer := IV05 + 1;
  var IV07: Integer := IV06 + 1; var IV08: Integer := IV07 + 1;
  var IV09: Integer := IV08 + 1; var IV10: Integer := IV09 + 1;
  var IV11: Integer := IV10 + 1; var IV12: Integer := IV11 + 1;
  var IV13: Integer := IV12 + 1; var IV14: Integer := IV13 + 1;
  var IV15: Integer := IV14 + 1; var IV16: Integer := IV15 + 1;
  var IV17: Integer := IV16 + 1; var IV18: Integer := IV17 + 1;
  var IV19: Integer := IV18 + 1; var IV20: Integer := IV19 + 1;
  var IV21: Integer := IV20 + 1; var IV22: Integer := IV21 + 1;
  var IV23: Integer := IV22 + 1; var IV24: Integer := IV23 + 1;
  // Keep every var live to the BP so the compiler can't fold slots.
  Writeln('ManyInts ',
    IV01,IV02,IV03,IV04,IV05,IV06,IV07,IV08,IV09,IV10,IV11,IV12,
    IV13,IV14,IV15,IV16,IV17,IV18,IV19,IV20,IV21,IV22,IV23,IV24);
  Flush(Output); // Line 800 - inline-var many-ints bp here
end;
// §6.35 fixture: a FRAMELESS (ESP-addressed, no `push ebp`) procedure.
// Athens' x86 compiler elides the EBP frame under {$STACKFRAMES OFF}
// when the routine does not need it -- the same shape DUnitX's
// RTTI-invoked test methods compile to (observed live on Test.Lib's
// TestTVariantDbValue.Implicit: prologue `push ebx; sub esp,N`, locals
// at [esp+N], the Byte param spilled register-to-register `mov ebx,edx`).
// In such a proc EBP still holds the CALLER's frame, so the debugger's
// `Regs.Ebp + BpOffset` local read lands in the caller's frame and
// returns garbage; and a register parameter spilled to a callee-saved
// register is missed by the prologue memory-spill scanner
// (TryFindRegParamSpillDisp), which then reads the now-clobbered live
// register. TestMcpEvaluateFramelessProcLocalsDecoded breaks on the
// Writeln below and asserts the Int64 locals + the Byte register
// parameter read back their sentinels -- red on the EBP-only reader,
// green once the consumer detects the frameless frame and rebases local
// reads onto ESP. Placed AFTER the last proc / BP marker so no
// breakpoint line above shifts. See DPT.Rsm.Format.md §6.35.
{$IFOPT W+}{$DEFINE DT_FRAMELESS_W_ON}{$ENDIF}
{$IFOPT O-}{$DEFINE DT_FRAMELESS_O_OFF}{$ENDIF}
{$STACKFRAMES OFF}{$OPTIMIZATION ON}
type
  TFramelessHost = class
    FMarker : Integer;
    procedure Probe(ASelector: Byte);
  end;
var
  GGlobalFrameless : TFramelessHost;
procedure TFramelessHost.Probe(ASelector: Byte);
var
  LBig   : Int64;
  LB2    : Int64;
  LB3    : Int64;
  LB4    : Int64;
  LB5    : Int64;
  LB6    : Int64;
  LExtra : Integer;
begin
  LBig   := Int64($7BADF00DCAFE0042);   // frameless Int64-local sentinel
  LB2    := Int64($1234567855AA55AA);
  LB3    := Int64($00C0FFEE0BADF00D);
  LB4    := Int64($0102030405060708);
  LB5    := Int64($1122334455667788);
  LB6    := Int64($7EEEEEEE7DDDDDDD);
  LExtra := Integer($51510000) or ASelector;
  // The six Int64s stay live to the Writeln so the optimiser spills the
  // overflow (> available registers) to [esp+N] stack homes.
  // §6.35 reg->reg residual: clobber EAX/EDX (Self's and ASelector's inbound
  // registers) so the LIVE register is provably stale at the BP below, while
  // the optimiser keeps Self in callee-saved ESI (`mov esi,eax`) and
  // ASelector in EBX (`mov ebx,edx`). Reading Self off the stale EAX yields a
  // garbage pointer; the fix must source register params from the
  // callee-saved register they were spilled into.
  GGlobalInt := GGlobalInt * 7 + 3;
  Writeln('Frameless ', LBig, ' ', LB2, ' ', LB3, ' ', LB4, ' ', LB5,
    ' ', LB6, ' ', LExtra, ' ', ASelector, ' ', FMarker); Flush(Output); // frameless-local + reg-spill bp here
  if (LBig = 0) or (LB2 = 0) or (LB3 = 0) or (LB4 = 0) or (LB5 = 0) or
     (LB6 = 0) then Writeln(LExtra); // keep every local live past the bp
end;
{$IFDEF DT_FRAMELESS_O_OFF}{$OPTIMIZATION OFF}{$UNDEF DT_FRAMELESS_O_OFF}{$ENDIF}
{$IFDEF DT_FRAMELESS_W_ON}{$STACKFRAMES ON}{$UNDEF DT_FRAMELESS_W_ON}{$ENDIF}
// §6.35 residual (2) fixture: a normal EBP-frame method with a Byte
// register parameter. Delphi spills a sub-4-byte register param to its
// frame home with an 8-bit store -- `mov [ebp-NN],dl` = 88 55 NN -- which
// TryFindRegParamSpillDisp's 32-bit-only `89 55 NN` match misses. The body
// clobbers EAX/EDX (so DL no longer holds the param) BEFORE a BP on a line
// that does NOT reference AByteParam, so the live-register fallback returns
// garbage; AByteParam ($C7) survives only in its [ebp-NN] frame home. The
// pin goes red->green once the matcher recognises the byte store, reads one
// byte, and zero-extends it. Mirrors the TStaleSelfHost clobber technique.
type
  TByteParamHost = class
    FMarker : Integer;
    procedure Probe(AByteParam: Byte);
  end;
var
  GGlobalByteParam : TByteParamHost;
procedure TByteParamHost.Probe(AByteParam: Byte);
var
  LScratch : Integer;
begin
  LScratch := GGlobalInt * 5 + 1;                  // clobbers EAX/EDX (DL)
  Writeln('ByteParam ', LScratch, ' ', FMarker);   // bp here - AByteParam NOT referenced
  Writeln('ByteParamVal ', AByteParam); Flush(Output);  // param used only AFTER the bp
end;
// §6.35 round 2 fixture: a standalone {$O+}{$STACKFRAMES OFF} proc with NO
// Self, so the callee-saved registers EBX/ESI/EDI are all free for LOCALS.
// Three Integer locals kept live across a call get register-allocated into
// distinct callee-saved registers, each emitted as the `16` register-
// resident LOCAL form (§6.35). Distinct sentinels let the investigation
// correlate "which $16-payload byte == which CPU register" -- the open part
// of §6.35 a single sample (LExtra) could not pin. The BP references all
// three so they stay live; the surrounding clobber keeps EAX/EDX off them.
{$IFOPT W+}{$DEFINE DT_RR_W_ON}{$ENDIF}
{$IFOPT O-}{$DEFINE DT_RR_O_OFF}{$ENDIF}
{$STACKFRAMES OFF}{$OPTIMIZATION ON}
procedure RegResidentLocalsProbe;
var
  RR1 : Integer;
  RR2 : Integer;
  RR3 : Integer;
begin
  RR1 := Integer($AAAA0001);
  RR2 := Integer($BBBB0002);
  RR3 := Integer($CCCC0003);
  GGlobalInt := GGlobalInt * 3 + 5;   // clobber EAX/EDX scratch before the BP
  Writeln('RegResident ', RR1, ' ', RR2, ' ', RR3); Flush(Output); // reg-resident-locals bp here
  if (RR1 = 0) or (RR2 = 0) or (RR3 = 0) then Writeln(GGlobalInt); // keep live
end;
{$IFDEF DT_RR_O_OFF}{$OPTIMIZATION OFF}{$UNDEF DT_RR_O_OFF}{$ENDIF}
{$IFDEF DT_RR_W_ON}{$STACKFRAMES ON}{$UNDEF DT_RR_W_ON}{$ENDIF}
// §6.36 fixture: a classic var-block LOCAL of a CROSS-UNIT record
// (DebugTarget.RecTypes.TXAdresse: leading shortstring + nested record),
// mirroring Test.Lib's `Ad: TAdresse` (declared in Base.Types.Business).
// The cross-unit boundary is the trigger: the local's per-proc TypeIdx and
// the nested Anschrift member's TypeIdx get the §4.2/§4.15 unreliable alias
// and need not resolve to a record, so the dotted-walk record-hop priming
// fails -- `AdrLoc.Anschrift.Str` does not resolve. (An inline same-unit
// record resolves fine, which is why the earlier inline TAdrLike probe did
// NOT reproduce.) Sentinels match the Test.Lib repro.
procedure RecordLocalNestedProbe;
var
  AdrLoc : TXAdresse;
begin
  AdrLoc := Default(TXAdresse);
  AdrLoc.Name          := 'Firma X';
  AdrLoc.Anschrift.Str := 'Hauptstr.';
  AdrLoc.Anschrift.Ort := 'Berlin';
  Writeln('RecLocalNested ', AdrLoc.Name, ' ', AdrLoc.Anschrift.Str, ' ',
    AdrLoc.Anschrift.Ort); Flush(Output); // rec-local-nested bp here
end;
// §6.35 spill-home fixture for CONST STRING register parameters.
// Mirrors Test.Lib's `Test(const AWords,Input: String; ...)`: two const
// String params land in registers (EDX/ECX after Self=EAX). A CALL (the
// first Writeln) clobbers those registers; the BP line below references
// NEITHER param, so the live registers are stale there; and both params
// are used AGAIN afterwards, so in this {$O-} debug build the compiler
// keeps them in prologue spill homes. Reading the live register at the BP
// yields garbage -- the debugger must read the spill home (the IDE does
// the same on debug builds). Pins that `evaluate <constStrParam> (string)`
// and get_locals recover the ORIGINAL value at a post-clobber PC. Placed
// AFTER the last BP marker per the rsm-expert BP-line discipline.
type
  TConstStrParamHost = class
    FMarker : Integer;
    procedure Probe(const AFirst, ASecond: String);
  end;
var
  GGlobalConstStrParamHost : TConstStrParamHost;
procedure TConstStrParamHost.Probe(const AFirst, ASecond: String);
var
  LScratch : Integer;
begin
  LScratch := Length(AFirst) + Length(ASecond);
  Writeln('CSP-pre ', AFirst, ' ', ASecond);             // CALL clobbers EDX/ECX
  Writeln('CSP ', LScratch, ' ', FMarker); Flush(Output); // bp here - const-str params NOT on this line
  Writeln('CSP-post ', AFirst, ' ', ASecond); Flush(Output); // params used AFTER -> spilled/kept
end;
// §6.37 round-4 fixture: PUBLISHED properties resolved via the live
// instance's runtime RTTI (VMT -> vmtTypeInfo -> property table), the
// collision-proof path the Delphi IDE uses for Caption-style properties --
// NOT the RSM $31 records. {$M+} makes a plain class emit published
// property RTTI (same mechanism TPersistent/TComponent rely on).
type
  {$M+}
  TRttiPropHost = class
  private
    FRttiInt    : Integer;
    FRttiBacking: string;
    function GetRttiCalc: Integer;
    function GetRttiText: string;
  published
    /// Field-backed published property -> RTTI GetProc encodes a field offset.
    property RttiPlain: Integer read FRttiInt;
    /// Getter-backed Integer published property -> RTTI GetProc = static method addr.
    property RttiCalc: Integer read GetRttiCalc;
    /// Getter-backed string published property -> managed-return getter.
    property RttiText: string read GetRttiText;
  end;
  {$M-}
var
  GGlobalRttiPropHost: TRttiPropHost;
function TRttiPropHost.GetRttiCalc: Integer;
begin
  Result := FRttiInt + 1;     // = $5BEEF5B2
end;
function TRttiPropHost.GetRttiText: string;
begin
  Result := 'RTTI:' + FRttiBacking;   // = 'RTTI:World'
end;
procedure RttiPropertyProbe;
begin
  GGlobalRttiPropHost := TRttiPropHost.Create;
  GGlobalRttiPropHost.FRttiInt     := Integer($5BEEF5B1);
  GGlobalRttiPropHost.FRttiBacking := 'World';
  Writeln('RttiProp ',                                                 // rtti-property bp here
          GGlobalRttiPropHost.RttiPlain, ' ',
          GGlobalRttiPropHost.RttiCalc, ' ',
          GGlobalRttiPropHost.RttiText); Flush(Output);
end;
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
    // Reach TouchRegEnumParam with two enum-typed values live in the
    // first two register parameter slots. Drives the §6.15 register-
    // passed enum parameter auto-detect pin.
    TouchRegEnumParam(lsYellow, tpHigher);
    // Reach PropertyProbe with GGlobalPropHost live so the Reader's
    // property-read tests can hit a populated TPropHost instance.
    PropertyProbe;
    // Reach RecordPropertyProbe with GGlobalPropRec populated so
    // the record-property pin tests have a live record instance.
    RecordPropertyProbe;
    // Reach TStaleSelfHost.Probe with Self spilled to its frame home but
    // the inbound register clobbered, so the Self-from-spill-home fix has
    // a live BP context (stale-register repro).
    GGlobalStaleSelf := TStaleSelfHost.Create;
    GGlobalStaleSelf.FMarker := Integer($5E1F5E1F);
    GGlobalStaleSelf.Probe;
    // §6.18 fixture: reach TPtrToRecHost.Probe with FRecPtr pointing at
    // a TMixedRec whose FMixedInt carries a sentinel the pin verifies.
    GGlobalPtrToRecRec.FMixedInt   := Integer($1F2E3D4C);
    GGlobalPtrToRecRec.FMixedInt64 := Int64($D1D2D3D4D5D6D7D8);
    GGlobalPtrToRecRec.FMixedStr   := 'PtrToRec-Str';
    GGlobalPtrToRecHost := TPtrToRecHost.Create;
    GGlobalPtrToRecHost.FRecPtr := @GGlobalPtrToRecRec;
    GGlobalPtrToRecHost.Probe;
    // §6.19 fixture: reach TAmbig619Host.Probe with FAmbig619Ptr pointing
    // at GGlobalAmbig619Target. Both target ($19191919) and sibling
    // ($DEADBEEF poison) records carry FShared619 so the §6.18 fallback's
    // unique-match guard would bail without the §6.19 alias->target
    // resolution in the Format-A linker.
    GGlobalAmbig619Target.FShared619       := Integer($19191919);
    GGlobalAmbig619Target.FUniqueTarget619 := Integer($A9A9A9A9);
    GGlobalAmbig619Sibling.FShared619        := Integer($DEADBEEF);
    GGlobalAmbig619Sibling.FUniqueSibling619 := Integer($CAFEBABE);
    GGlobalAmbig619Host := TAmbig619Host.Create;
    GGlobalAmbig619Host.FAmbig619Ptr := @GGlobalAmbig619Target;
    GGlobalAmbig619Host.Probe;
    // Reach the body of TouchRtlInheritedComp with AComp live as a
    // register-passed reference. Drives the inherited-RTL-field
    // navigation test (Name / Tag declared on TComponent, walked
    // via the FindClassMember ancestor chain).
    TouchRtlInheritedComp(GGlobalComp);
    // Reach TouchEmptyChild so the VMT-walk fallback test for
    // ancestors via the no-own-fields path has a live BP context.
    TouchEmptyChild(GGlobalEmptyChild);
    // §6.31: a 'C'-prefixed class instance so the field-discovery test
    // has a non-T class with concrete field values to resolve.
    GGlobalNonTHost := CNonTPrefixHost.Create;
    GGlobalNonTHost.FNonTInt := Integer($CECECECE);
    GGlobalNonTHost.FNonTStr := 'NonT Field';
    GGlobalNonTHost.FNonTObj := TInner.Create;
    GGlobalNonTHost.FNonTObj.FInnerInt := Integer($CD000001);
    // §6.35 fixture: reach the frameless (ESP-addressed) Probe with a
    // known Byte register parameter ($5A) and Int64 stack locals live,
    // so the frameless-frame local-read pin has a live BP context.
    GGlobalFrameless := TFramelessHost.Create;
    GGlobalFrameless.FMarker := Integer($F3A3E1E5);
    GGlobalFrameless.Probe($5A);
    // §6.35 residual (2): reach the EBP-frame Byte-param Probe with a known
    // Byte parameter ($C7) whose inbound DL is clobbered before the BP.
    GGlobalByteParam := TByteParamHost.Create;
    GGlobalByteParam.FMarker := Integer($B7B7B7B7);
    GGlobalByteParam.Probe($C7);
    // §6.35 round 2: reach the standalone proc whose three Integer locals
    // are register-allocated into distinct callee-saved registers.
    RegResidentLocalsProbe;
    // §6.37 round 4: reach RttiPropertyProbe with a live published-property
    // instance so the VMT-RTTI property resolver pin has a BP context.
    RttiPropertyProbe;
    // §6.36: reach the complex record-local probe (TAdresse-like shape).
    RecordLocalNestedProbe;
    // §6.35: const-string register params read at a post-clobber PC.
    GGlobalConstStrParamHost := TConstStrParamHost.Create;
    GGlobalConstStrParamHost.FMarker := Integer($5A5A5A5A);
    GGlobalConstStrParamHost.Probe('Hallo', 'Welt');
    LocalsProcedure;
    InlineVarLocalsProcedure;
    InlineVarManyIntsProcedure;
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
    GGlobalPropHost.Free;
    GGlobalStaleSelf.Free;
    GGlobalPtrToRecHost.Free;
    GGlobalAmbig619Host.Free;
    GGlobalNonTHost.FNonTObj.Free;
    GGlobalNonTHost.Free;
    GGlobalFrameless.Free;
    GGlobalByteParam.Free;
    GGlobalRttiPropHost.Free;
  end;
end.
