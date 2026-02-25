program DebugTarget;
{$APPTYPE CONSOLE}
{$O-}
{$D+}
{$STACKFRAMES ON}
uses System.SysUtils;
var
  GGlobalInt: Integer = $87654321;
procedure DeepProcedure;
var LocalInt: Integer;
begin
  LocalInt := $12345678;
  Writeln('Deep'); // Line 13
end;
procedure TargetProcedure;
begin
  Writeln('Target'); // Line 17
  DeepProcedure;
end;
begin
  GGlobalInt := $11223344;
  TargetProcedure; // Line 22
end.
