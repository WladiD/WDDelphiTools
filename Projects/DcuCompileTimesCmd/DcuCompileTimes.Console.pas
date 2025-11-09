unit DcuCompileTimes.Console;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  DcuCompileTimes.Core;

type
  TAppOptions = record
    FilterMask: String;
    TopNSValue: Integer;
    HelpRequested: Boolean;
  end;

  TCommandLineParser = class
  public
    class function Parse(const AParams: array of string): TAppOptions;
  end;

  TConsoleWriter = class
  private
    FAnalyzer: TDcuAnalyzer;
    FOptions: TAppOptions;
    FPrintList: TList<TFileInfo>;
    FMaskTotalDiff: Int64;
    procedure CreatePrintList;
    procedure PrintFiles;
    procedure PrintNamespaceStats;
    procedure PrintSummary;
  public
    constructor Create(const AOptions: TAppOptions; const AAnalyzer: TDcuAnalyzer);
    destructor Destroy; override;
    procedure PrintResults;
    class procedure PrintHelp;
  end;

implementation

uses
  System.Masks,
  System.DateUtils,
  System.Types,
  System.Math,
  System.IOUtils,
  System.Generics.Defaults;

const
  TicksPerMillisecond = 10000;

{ TCommandLineParser }

class function TCommandLineParser.Parse(const AParams: array of string): TAppOptions;
var
  I: Integer;
  Param, ValueStr: string;
  PotentialFilterMask: string;
begin
  Result.FilterMask := '';
  Result.TopNSValue := 10; // Default
  Result.HelpRequested := False;
  PotentialFilterMask := '';

  for I := 0 to High(AParams) do
  begin
    Param := AParams[I];
    if Param.StartsWith('--top-ns=') then
    begin
      ValueStr := Param.Substring(9);
      if SameText(ValueStr, 'off') then
        Result.TopNSValue := -1
      else if SameText(ValueStr, 'all') or (ValueStr = '0') then
        Result.TopNSValue := 0
      else if not TryStrToInt(ValueStr, Result.TopNSValue) or (Result.TopNSValue < 1) then
      begin
        Writeln('Error: Invalid value for --top-ns. Must be "off", "all", "0", or a positive integer.');
        // How to handle exit? Maybe raise exception. For now, just note it.
      end;
    end
    else if (Param = '--help') or (Param = '-h') or (Param = '?') then
    begin
      Result.HelpRequested := True;
    end
    else
    begin
      PotentialFilterMask := Param;
    end;
  end;

  Result.FilterMask := PotentialFilterMask;
end;

{ TConsoleWriter }

constructor TConsoleWriter.Create(const AOptions: TAppOptions; const AAnalyzer: TDcuAnalyzer);
begin
  FOptions := AOptions;
  FAnalyzer := AAnalyzer;
  FPrintList := nil;
  FMaskTotalDiff := 0;
end;

destructor TConsoleWriter.Destroy;
begin
  FPrintList.Free;
  inherited;
end;

procedure TConsoleWriter.CreatePrintList;
var
  HasFilterMask: Boolean;
  Loop: Integer;
begin
  HasFilterMask := FOptions.FilterMask <> '';
  Assert(HasFilterMask);

  FPrintList := TList<TFileInfo>.Create;
  FMaskTotalDiff := 0;
  for Loop := 0 to FAnalyzer.Files.Count - 1 do
  begin
    if MatchesMask(TPath.GetFileName(FAnalyzer.Files[Loop].Path), FOptions.FilterMask) then
    begin
      FMaskTotalDiff := FMaskTotalDiff + FAnalyzer.Files[Loop].Diff;
      FPrintList.Add(FAnalyzer.Files[Loop]);
    end;
  end;
end;

class procedure TConsoleWriter.PrintHelp;
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
end;

procedure TConsoleWriter.PrintFiles;
var
  I: Integer;
  Factor: Double;
begin
  Writeln('------------------------------------------------------------');
  Writeln('Files, sorted by generation time (ms), factor to median (x):');
  Writeln('------------------------------------------------------------');

  for I := 0 to FPrintList.Count - 1 do
  begin
    if FPrintList[I].Diff > 0 then
    begin
      Factor := 0;
      if FAnalyzer.MedianTime > 0 then
        Factor := FPrintList[I].Diff / FAnalyzer.MedianTime;
      Writeln(Format('%-40s (%.4f ms) (x%.2f)', [TPath.GetFileName(FPrintList[I].Path), FPrintList[I].Diff / TicksPerMillisecond, Factor]));
    end;
  end;

  var OldestFileInList := FAnalyzer.OldestFile;
  if (OldestFileInList.Path <> '') and
     ((FOptions.FilterMask = '') or MatchesMask(TPath.GetFileName(OldestFileInList.Path), FOptions.FilterMask)) then
    Writeln(Format('%s <- build start %s',
      [TPath.GetFileName(OldestFileInList.Path), DateTimeToStr(TFile.GetLastWriteTime(OldestFileInList.Path))]));
end;

procedure TConsoleWriter.PrintNamespaceStats;
var
  NamespaceStats: TDictionary<string, TNamespaceInfo>;
  StatList, FinalList: TList<TPair<string, TNamespaceInfo>>;
  Pair: TPair<string, TNamespaceInfo>;
  Info: TNamespaceInfo;
  Limit, I: Integer;

  function IsRedundant(const AStat: TPair<String, TNamespaceInfo>; const AStatList: TList<TPair<string, TNamespaceInfo>>): Boolean;
  var
    OtherStat: TPair<string, TNamespaceInfo>;
  begin
    Result := False;
    for OtherStat in AStatList do
    begin
      if (OtherStat.Key <> AStat.Key) and OtherStat.Key.StartsWith(AStat.Key + '.') then
      begin
        if (OtherStat.Value.TotalTime = AStat.Value.TotalTime) and (OtherStat.Value.FileCount = AStat.Value.FileCount) then
          Exit(True);
      end;
    end;
  end;

begin
  NamespaceStats := FAnalyzer.GetNamespaceStats;
  try
    if (FOptions.TopNSValue < 0) or (NamespaceStats = nil) or (NamespaceStats.Count = 0) then
      Exit;

    Writeln('------------------------------------------------------------');
    Writeln('Top Namespace Group Times:');
    Writeln('------------------------------------------------------------');

    StatList := TList<TPair<String, TNamespaceInfo>>.Create;
    FinalList := TList<TPair<String, TNamespaceInfo>>.Create;
    try
      StatList.AddRange(NamespaceStats.ToArray);
      StatList.Sort(TComparer<TPair<string, TNamespaceInfo>>.Construct(
        function(const Left, Right: TPair<string, TNamespaceInfo>): Integer
        begin
          Result := Sign(Right.Value.TotalTime - Left.Value.TotalTime);
        end
      ));

      for Pair in StatList do
        if not IsRedundant(Pair, StatList) then
          FinalList.Add(Pair);

      if (FOptions.TopNSValue = 0) then
        Limit := FinalList.Count
      else
        Limit := Min(FOptions.TopNSValue, FinalList.Count);

      for I := 0 to Limit - 1 do
      begin
        Pair := FinalList[I];
        Info := Pair.Value;
        if Info.FileCount > 1 then
          Writeln(Format('%-40s (%.4f ms)(%d files)', [Pair.Key + '.*', Pair.Value.TotalTime / TicksPerMillisecond, Pair.Value.FileCount]));
      end;
    finally
      FinalList.Free;
      StatList.Free;
    end;
  finally
    NamespaceStats.Free;
  end;
end;

procedure TConsoleWriter.PrintResults;
var
  HasFilterMask: Boolean;
begin
  HasFilterMask := FOptions.FilterMask <> '';
  if HasFilterMask then
    CreatePrintList
  else
  begin
    FPrintList := TList<TFileInfo>.Create;
    FPrintList.AddRAnge(FAnalyzer.Files);
  end;


  if FPrintList.Count >= 30 then
    PrintSummary;

  PrintFiles;
  PrintNamespaceStats;

  Writeln('------------------------------------------------------------');
  PrintSummary;
end;

procedure TConsoleWriter.PrintSummary;
var
  HasFilterMask: Boolean;
  Percentage: Double;
begin
  HasFilterMask := FOptions.FilterMask <> '';
  Writeln(Format('Total time  : %.4f ms', [FAnalyzer.TotalTime / TicksPerMillisecond]));
  if FAnalyzer.MedianTime > 0 then
    Writeln(Format('Median time : %.4f ms', [FAnalyzer.MedianTime / TicksPerMillisecond]));
  if HasFilterMask then
  begin
    Percentage := 0;
    if FAnalyzer.TotalTime > 0 then
      Percentage := (FMaskTotalDiff / FAnalyzer.TotalTime) * 100;
    Writeln(Format('Mask matches: %d',[FPrintList.Count]));
    Writeln(Format('Mask time   : %.4f ms (%.2f%%)', [FMaskTotalDiff / TicksPerMillisecond, Percentage]));
  end;
end;

end.
