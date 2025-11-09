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

const
  TicksPerMillisecond = 10000;

var
  Analyzer: TDcuAnalyzer;
  Options: TAppOptions;

function GetParams: TArray<string>;
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
      Writeln('DcuCompileTimes.exe [Options] [FilterMask]');
      Writeln;
      Writeln('Analyzes Delphi .dcu compilation times in the current directory.');
      Writeln;
      Writeln('Arguments:');
      Writeln('  [FilterMask]  Optional. A file mask (e.g., "Base.*", "Base.Db.*") to filter');
      Writeln('                the displayed files and calculate a "Mask time" summary.');
      Writeln;
      Writeln('Options:');
      Writeln('  --help, -h, ? Displays this help message.');
      Writeln('  --top-ns=n    Shows the top n namespaces by compile time.');
      Writeln('                n can be a number, "all", "0", or "off". Default is 10.');
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

      // Create a writer and print the results
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
