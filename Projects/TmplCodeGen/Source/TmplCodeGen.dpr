// ======================================================================
// Copyright (c) 2025 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

program TmplCodeGen;

{$APPTYPE CONSOLE}
{$R *.res}

uses

  System.Diagnostics,
  System.SysUtils,

  TmplCodeGen.Generator in 'TmplCodeGen.Generator.pas',
  TmplCodeGen.Includer in 'TmplCodeGen.Includer.pas',
  TmplCodeGen.PreProcess in 'TmplCodeGen.PreProcess.pas',
  TmplCodeGen.Utils in 'TmplCodeGen.Utils.pas';

procedure ProcessTemplate(const APrefix: String);
begin
  var CodeGen: TTmplCodeGen := TTmplCodeGen.Create(APrefix);
  try
    CodeGen.ProcessTemplate;
  finally
    CodeGen.Free;
  end;
end;

procedure IncludePartials(const ATargetFile: String);
begin
  var IncPartials: TIncludePartials := TIncludePartials.Create(ATargetFile);
  try
    IncPartials.Execute;
  finally
    IncPartials.Free;
  end;
end;

begin
  try
    if ParamCount < 1 then
    begin
      var ExeFileName: String := ExtractFileName(ParamStr(0));
      Writeln(Format('%s prefix', [ExeFileName]));
      Writeln(Format('%s include_partials target_file', [ExeFileName]));
      Exit;
    end;

    var Stopper: TStopwatch := TStopwatch.StartNew;
    var FirstParam: String := ParamStr(1);

    if SameText(FirstParam, 'include_partials') and (ParamCount = 2) then
      IncludePartials(ParamStr(2))
    else
      ProcessTemplate(FirstParam);

    Writeln(Format(
      'Success! (Duration: %.d ms.)',
      [Stopper.ElapsedMilliseconds]) + sLineBreak);
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 42;
    end;
  end;
end.