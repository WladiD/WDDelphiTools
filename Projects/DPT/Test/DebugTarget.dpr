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
var
  GGlobalInt64       : Int64       = Int64($1122334455667788);
  GGlobalAnsi        : AnsiString  = 'Hello Ansi';
  GGlobalWide        : WideString  = 'Hello Wide';
  GGlobalShort       : ShortString = 'Hello Short';
  GGlobalEmptyString : string      = '';
  GGlobalNilObject   : TObject     = nil;
begin
  GGlobalInt := $11223344;
  GGlobalObject := TStringList.Create;
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
    GGlobalObject.Free;
  end;
end.
