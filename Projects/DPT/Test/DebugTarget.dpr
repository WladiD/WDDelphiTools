program DebugTarget;
{$APPTYPE CONSOLE}
{$O-}
{$D+}
{$STACKFRAMES ON}
uses System.SysUtils;
var
  GGlobalInt: Integer = Integer($87654321);
procedure DeepProcedure;
var LocalInt: Integer;
begin
  LocalInt := $12345678;
  Writeln('Deep'); // Line 13
end;
procedure TargetProcedure;
begin
  Writeln('Target'); // Line 17
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
  Writeln('Locals ', LocalA, ' ', LocalB, ' ', LocalC); // Line 38 - locals breakpoint here
end;
begin
  GGlobalInt := $11223344;
  TargetProcedure;
  LocalsProcedure;
end.
