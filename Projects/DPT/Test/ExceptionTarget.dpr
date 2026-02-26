program ExceptionTarget;

{$APPTYPE CONSOLE}
{$O-}
{$D+}
{$STACKFRAMES ON}

uses
  System.SysUtils;

procedure CrashProcedure;
var
  P: PInteger;
begin
  Writeln('About to crash...'); // Line 15
  P := nil;
  P^ := 42; // Line 17 - This will cause an Access Violation
  Writeln('Should not reach here');
end;

begin
  try
    Writeln('Starting ExceptionTarget'); // Line 23
    CrashProcedure; // Line 24
    Writeln('Finished ExceptionTarget');
  except
    on E: Exception do
      Writeln('Caught inside target: ', E.ClassName, ': ', E.Message); // We catch it so WER doesn't trigger
  end;
end.