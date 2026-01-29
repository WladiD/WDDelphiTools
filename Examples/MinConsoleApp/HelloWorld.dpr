(* HelloWorld-conf.json
{
  "Template": "ConsoleApp.TMPL.dproj"
}
*)
// TmplCodeGen HelloWorld

program HelloWorld;

{$APPTYPE CONSOLE}

uses

  System.SysUtils;

begin
  try
    Writeln('Hello World');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
