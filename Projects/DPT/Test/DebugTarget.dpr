program DebugTarget;
{$APPTYPE CONSOLE}
{$O-}
{$D+}
{$STACKFRAMES ON}
uses System.SysUtils, Winapi.Windows;
var
  GGlobalInt: Integer = Integer($87654321);
procedure DeepProcedure;
var LocalInt: Integer;
begin
  LocalInt := $12345678;
  Writeln('Deep'); Flush(Output); // Line 13
end;
procedure TargetProcedure;
begin
  Writeln('Target'); Flush(Output); // Line 17
  try
    Abort; // Raises EAbort
  except
    // Catch so it doesn't crash the app
  end;
  try
    raise Exception.Create('Test Exception'); // Line 24
  except
  end;
  DeepProcedure; // Line 27
end;
procedure LocalsProcedure;
var
  LocalA: Integer;
  LocalB: Int64;
  LocalC: Cardinal;
begin
  LocalA := $12345678;
  LocalB := Int64($1122334455667788);
  LocalC := $DEADBEEF;
  Writeln('Locals ', LocalA, ' ', LocalB, ' ', LocalC); Flush(Output); // Line 38 - locals breakpoint here
end;
begin
  GGlobalInt := $11223344;
  Writeln(ErrOutput, 'stderr-tag-line'); Flush(ErrOutput);
  OutputDebugString('ods-tag-line');
  TargetProcedure;
  LocalsProcedure;
end.
