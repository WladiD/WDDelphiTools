program DebugTarget;
{$APPTYPE CONSOLE}
{$O-}
{$D+}
{$STACKFRAMES ON}
uses System.SysUtils, System.Classes, Winapi.Windows;
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
  end;
  TDeepDerived = class(TDerived)
  private
    FDeepFlag : Integer;
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
  GGlobalWithRec     : TWithRec;
  GGlobalMixed       : TMixedRec;
  GGlobalP3D         : TPoint3D;
  GGlobalWithHeader  : TWithHeader;
  GGlobalVarRec      : TVariantSlot;
  GGlobalNarrow      : TNarrowInts;
begin
  GGlobalInt := $11223344;
  GGlobalObject := TStringList.Create;
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
  // Touch the value-type globals so the linker keeps them in .bss/.data
  // instead of dead-code-eliminating them. The values themselves are
  // already set by the typed-constant initializers above.
  if (GGlobalInt64 = 0) and (GGlobalShort = '') and (GGlobalNilObject = nil) then ;
  try
    Writeln(ErrOutput, 'stderr-tag-line'); Flush(ErrOutput);
    OutputDebugString('ods-tag-line');
    TargetProcedure;
    LocalsProcedure;
    if GGlobalInt = -1 then EdgeCaseLocalsProcedure;
    if GGlobalInt = -1 then OpenArrayStringProcedure(['A', 'B', 'C']);
    if GGlobalInt = -1 then OpenArrayIntProcedure([1, 2, 3, 4]);
  finally
    GGlobalOuter.FOuterInner.Free;
    GGlobalOuter.Free;
    GGlobalDerived.Free;
    GGlobalDeep.Free;
    GGlobalWithRec.FPair.FObj.Free;
    GGlobalWithRec.FNestedObj.Free;
    GGlobalWithRec.Free;
    GGlobalObject.Free;
  end;
end.
