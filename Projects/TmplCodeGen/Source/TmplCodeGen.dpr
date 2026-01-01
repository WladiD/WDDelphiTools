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
  TmplCodeGen.Logger in 'TmplCodeGen.Logger.pas',
  TmplCodeGen.PreProcess in 'TmplCodeGen.PreProcess.pas',
  TmplCodeGen.Utils in 'TmplCodeGen.Utils.pas',
  TmplCodeGen.Common in 'TmplCodeGen.Common.pas';

procedure ProcessTemplate(const APrefix: String);
begin
  var Logger: ILogger := TConsoleLogger.Create;
  var CodeGen: TTmplCodeGen := TTmplCodeGen.Create(APrefix, Logger);
  try
    CodeGen.ProcessTemplate;
  finally
    CodeGen.Free;
  end;
end;

procedure IncludePartials(const ATargetFile: String);
begin
  var Logger: ILogger := TConsoleLogger.Create;
  var IncPartials: TIncludePartials := TIncludePartials.Create(ATargetFile, Logger);
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
