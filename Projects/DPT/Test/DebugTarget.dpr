program DebugTarget;

{$APPTYPE CONSOLE}
{$O-}
{$D+}
{$STACKFRAMES ON}

uses
  System.SysUtils;

procedure DeepProcedure;
begin
  Writeln('Inside DeepProcedure'); // Line 13
  Writeln('Still inside DeepProcedure'); // Line 14
end;

procedure TargetProcedure;
begin
  Writeln('Inside TargetProcedure'); // Line 19
  DeepProcedure; // Line 20
end;

begin
  try
    Writeln('Starting DebugTarget'); // Line 25
    TargetProcedure; // Line 26
    Writeln('Finished DebugTarget'); // Line 27
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
