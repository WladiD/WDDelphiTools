program DcuCompileTimes;

{$APPTYPE CONSOLE}

uses

  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.Generics.Defaults,
  System.Types,
  System.Math,
  System.Masks,
  Winapi.Windows,
  DcuCompileTimes.Core in 'DcuCompileTimes.Core.pas',
  DcuCompileTimes.Console in 'DcuCompileTimes.Console.pas';

var
  Analyzer: TDcuAnalyzer;
  Options: TAppOptions;

function GetParams: TArray<String>;
var
  I: Integer;
begin
  SetLength(Result, ParamCount);
  for I := 1 to ParamCount do
    Result[I-1] := ParamStr(I);
end;

begin
  try
    Options := TCommandLineParser.Parse(GetParams);

    if Options.HelpRequested then
    begin
      TConsoleWriter.PrintHelp;
      Exit;
    end;

    Analyzer := TDcuAnalyzer.Create('.');
    try
      Analyzer.Execute;

      if Analyzer.Files.Count = 0 then
      begin
        Writeln('No files found matching mask: ' + '*.dcu');
        Exit;
      end;

      if Analyzer.IsIncompleteBuild then
      begin
        Writeln('Error: Incomplete build detected. Please perform a full, clean build before running the tool.');
        Writeln('Analysis aborted.');
        Exit;
      end;

      var Writer := TConsoleWriter.Create(Options, Analyzer);
      try
        Writer.PrintResults;
      finally
        Writer.Free;
      end;

    finally
      Analyzer.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
