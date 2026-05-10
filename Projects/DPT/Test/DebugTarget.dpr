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
var
  GGlobalInt64       : Int64       = Int64($1122334455667788);
  GGlobalAnsi        : AnsiString  = 'Hello Ansi';
  GGlobalWide        : WideString  = 'Hello Wide';
  GGlobalShort       : ShortString = 'Hello Short';
  GGlobalEmptyString : string      = '';
  GGlobalNilObject   : TObject     = nil;
  GGlobalOuter       : TOuter;
  GGlobalWithRec     : TWithRec;
begin
  GGlobalInt := $11223344;
  GGlobalObject := TStringList.Create;
  GGlobalOuter := TOuter.Create;
  GGlobalOuter.FOuterInt := $11111111;
  GGlobalOuter.FOuterStr := 'Hello Outer';
  GGlobalOuter.FOuterInner := TInner.Create;
  GGlobalOuter.FOuterInner.FInnerInt := $22222222;
  GGlobalOuter.FOuterInner.FInnerStr := 'Hello Inner';
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
  // Touch the value-type globals so the linker keeps them in .bss/.data
  // instead of dead-code-eliminating them. The values themselves are
  // already set by the typed-constant initializers above.
  if (GGlobalInt64 = 0) and (GGlobalShort = '') and (GGlobalNilObject = nil) then ;
  try
    Writeln(ErrOutput, 'stderr-tag-line'); Flush(ErrOutput);
    OutputDebugString('ods-tag-line');
    TargetProcedure;
    LocalsProcedure;
  finally
    GGlobalOuter.FOuterInner.Free;
    GGlobalOuter.Free;
    GGlobalWithRec.FPair.FObj.Free;
    GGlobalWithRec.FNestedObj.Free;
    GGlobalWithRec.Free;
    GGlobalObject.Free;
  end;
end.
