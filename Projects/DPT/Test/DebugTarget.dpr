program DebugTarget;

{$APPTYPE CONSOLE}
{$O-}
{$D+}
{$STACKFRAMES ON}

uses
  System.SysUtils;

procedure DeepProcedure;
var
  LocalInt: Integer;
begin
  Writeln('Entering DeepProcedure'); // Line 14
  LocalInt := $12345678;             // Line 15
  Writeln('LocalInt set');           // Line 16
  Writeln('Value: ' + IntToHex(LocalInt, 8)); // Line 17
end;

procedure TargetProcedure;
begin
  Writeln('Inside TargetProcedure'); // Line 22
  DeepProcedure; // Line 23
end;

begin
  try
    Writeln('Starting DebugTarget'); // Line 28
    TargetProcedure; // Line 29
    Writeln('Finished DebugTarget'); // Line 30
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
