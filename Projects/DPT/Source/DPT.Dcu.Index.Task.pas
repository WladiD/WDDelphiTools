// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Index.Task;

interface

uses

  DPT.Task,
  DPT.Types;

type

  TDcuIndexMode = (dimNone, dimBuild, dimQuery, dimStats);
  TDcuIndexQueryKind = (dqkNone, dqkUnit, dqkImportedBy, dqkImportsOf,
    dqkReferences, dqkDefinedIn);

  TDptDcuIndexTask = class(TDptTaskBase)
  private
    FMode       : TDcuIndexMode;
    // Build mode
    FRootDirs   : TArray<string>;
    FRecursive  : Boolean;
    FParallel   : Boolean;
    FOutputFile : string;
    FFilePattern: string;
    // Query / Stats modes
    FIndexFile  : string;
    FQueryKind  : TDcuIndexQueryKind;
    FQueryArg   : string;
    procedure ExecuteBuild;
    procedure ExecuteQuery;
    procedure ExecuteStats;
  public
    constructor Create; override;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

uses

  Winapi.Windows,

  System.Classes,
  System.Diagnostics,
  System.StrUtils,
  System.SysUtils,

  mormot.core.collections,

  DPT.Dcu.Index,
  DPT.Dcu.Index.Json,
  DPT.Dcu.Types;

{ TDptDcuIndexTask }

constructor TDptDcuIndexTask.Create;
begin
  inherited;
  FMode := dimNone;
  FRecursive := True;
  FParallel := True;
  FOutputFile := '';
  FFilePattern := '*.dcu';
  FQueryKind := dqkNone;
end;

procedure TDptDcuIndexTask.Parse(CmdLine: TCmdLineConsumer);
var
  ModeStr: string;
  Param  : string;
begin
  if not CmdLine.HasParameter then
    raise EInvalidParameter.Create(
      'DcuIndex requires a sub-mode: Build | Query | Stats');

  ModeStr := CmdLine.CheckParameter('Mode');
  if SameText(ModeStr, 'Build') then
    FMode := dimBuild
  else if SameText(ModeStr, 'Query') then
    FMode := dimQuery
  else if SameText(ModeStr, 'Stats') then
    FMode := dimStats
  else
    CmdLine.InvalidParameter('Unknown sub-mode: ' + ModeStr
      + ' (expected Build, Query or Stats)');
  CmdLine.ConsumeParameter;

  while CmdLine.HasParameter do
  begin
    Param := CmdLine.CheckParameter('Option');

    if SameText(Param, '--Recursive') then
    begin
      FRecursive := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--NoRecursive') then
    begin
      FRecursive := False;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Parallel') then
    begin
      FParallel := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--NoParallel') then
    begin
      FParallel := False;
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--Output=', True) then
    begin
      FOutputFile := ExpandFileName(Param.Substring(9).DeQuotedString('"'));
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--Pattern=', True) then
    begin
      FFilePattern := Param.Substring(10).DeQuotedString('"');
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--Unit=', True) then
    begin
      FQueryKind := dqkUnit;
      FQueryArg := Param.Substring(7).DeQuotedString('"');
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--ImportedBy=', True) then
    begin
      FQueryKind := dqkImportedBy;
      FQueryArg := Param.Substring(13).DeQuotedString('"');
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--ImportsOf=', True) then
    begin
      FQueryKind := dqkImportsOf;
      FQueryArg := Param.Substring(12).DeQuotedString('"');
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--References=', True) then
    begin
      FQueryKind := dqkReferences;
      FQueryArg := Param.Substring(13).DeQuotedString('"');
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--DefinedIn=', True) then
    begin
      FQueryKind := dqkDefinedIn;
      FQueryArg := Param.Substring(12).DeQuotedString('"');
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--') then
      CmdLine.InvalidParameter('Unknown option: ' + Param)
    else
    begin
      // Positional. For Build it's a root dir; for Query/Stats it's the
      // index file path (the first one).
      case FMode of
        dimBuild:
          begin
            SetLength(FRootDirs, Length(FRootDirs) + 1);
            FRootDirs[High(FRootDirs)] := ExpandFileName(Param);
          end;
        dimQuery, dimStats:
          if FIndexFile = '' then
            FIndexFile := ExpandFileName(Param)
          else
            CmdLine.InvalidParameter('Unexpected positional argument: ' + Param);
      end;
      CmdLine.ConsumeParameter;
    end;
  end;

  case FMode of
    dimBuild:
      if Length(FRootDirs) = 0 then
        raise EInvalidParameter.Create(
          'DcuIndex Build requires at least one directory');
    dimQuery:
      begin
        if FIndexFile = '' then
          raise EInvalidParameter.Create(
            'DcuIndex Query requires the index file path');
        if FQueryKind = dqkNone then
          raise EInvalidParameter.Create(
            'DcuIndex Query requires one of --Unit / --ImportedBy / '
            + '--ImportsOf / --References / --DefinedIn');
      end;
    dimStats:
      if FIndexFile = '' then
        raise EInvalidParameter.Create(
          'DcuIndex Stats requires the index file path');
  end;
end;

procedure TDptDcuIndexTask.ExecuteBuild;
var
  ElapsedMs : Int64;
  Failure   : TDcuIndexBuildFailure;
  Failures  : IList<TDcuIndexBuildFailure>;
  Index     : TDcuIndex;
  Stream    : TStream;
  Stopwatch : TStopwatch;
  Throughput: Double;
begin
  Failures := Collections.NewPlainList<TDcuIndexBuildFailure>;
  Stopwatch := TStopwatch.StartNew;
  Index := TDcuIndexBuilder.Build(FRootDirs, FRecursive, FParallel,
    FFilePattern, Failures);
  Stopwatch.Stop;
  ElapsedMs := Stopwatch.ElapsedMilliseconds;

  if FOutputFile = '' then
  begin
    // Stream straight to stdout in UTF-8 to keep memory bounded. Skip
    // the timing summary on this path because it would corrupt the
    // JSON stream the caller is going to parse.
    Stream := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE));
    try
      TDcuIndexJsonWriter.WriteToStream(Index, Stream);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    TDcuIndexJsonWriter.WriteToFile(Index, FOutputFile);
    if (ElapsedMs > 0) and (Index.Units.Count > 0) then
      Throughput := Index.Units.Count / (ElapsedMs / 1000)
    else
      Throughput := 0;
    Writeln(Format('Indexed %d DCUs into %s in %.2fs (%.0f DCUs/s, %s mode)',
      [Index.Units.Count, FOutputFile, ElapsedMs / 1000, Throughput,
       IfThen(FParallel, 'parallel', 'sequential')]));
  end;

  // Per-file failures are reported after the success line but do not
  // make the build itself fail. The user sees what could not be parsed
  // and decides whether to re-run with --NoParallel for clearer output.
  if Failures.Count > 0 then
  begin
    Writeln(Format('%d file(s) skipped due to errors:', [Failures.Count]));
    for Failure in Failures do
      Writeln('  ' + Failure.FilePath + ' -- ' + Failure.Reason);
  end;
end;

procedure TDptDcuIndexTask.ExecuteQuery;
var
  Entries: IList<TDcuIndexEntry>;
  Entry  : TDcuIndexEntry;
  Hits   : IList<string>;
  Hit    : string;
  Index  : TDcuIndex;
  Query  : TDcuIndexQuery;
begin
  if not FileExists(FIndexFile) then
    raise EDptRuntimeError.Create('Index file not found: ' + FIndexFile);
  Index := TDcuIndexJsonReader.LoadFromFile(FIndexFile);
  Query := TDcuIndexQuery.Create(Index);
  try
    case FQueryKind of
      dqkUnit:
        begin
          Entries := Query.FindByUnit(FQueryArg);
          if Entries.Count = 0 then
          begin
            System.ExitCode := 1;
            Exit;
          end;
          for Entry in Entries do
            Writeln(Format('%s [%s/%s] %s',
              [Entry.UnitName,
               DcuKnownCompilerName[Entry.Compiler],
               DcuPlatformName[Entry.Platform],
               Entry.FilePath]));
        end;
      dqkImportedBy:
        begin
          Hits := Query.FindImportedBy(FQueryArg);
          if Hits.Count = 0 then
          begin
            System.ExitCode := 1;
            Exit;
          end;
          for Hit in Hits do Writeln(Hit);
        end;
      dqkImportsOf:
        begin
          Hits := Query.FindImportsOf(FQueryArg);
          if Hits.Count = 0 then
          begin
            System.ExitCode := 1;
            Exit;
          end;
          for Hit in Hits do Writeln(Hit);
        end;
      dqkReferences:
        begin
          Hits := Query.FindReferences(FQueryArg);
          if Hits.Count = 0 then
          begin
            System.ExitCode := 1;
            Exit;
          end;
          for Hit in Hits do Writeln(Hit);
        end;
      dqkDefinedIn:
        begin
          Hits := Query.FindDefinitionOf(FQueryArg);
          if Hits.Count = 0 then
          begin
            System.ExitCode := 1;
            Exit;
          end;
          for Hit in Hits do Writeln(Hit);
        end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TDptDcuIndexTask.ExecuteStats;
var
  CompilerHits: array[TDcuKnownCompiler] of Integer;
  Entry       : TDcuIndexEntry;
  Index       : TDcuIndex;
  K           : TDcuKnownCompiler;
  P           : TDcuPlatform;
  PlatformHits: array[TDcuPlatform] of Integer;
  ReverseCount: Integer;
  TotalSize   : Int64;
begin
  if not FileExists(FIndexFile) then
    raise EDptRuntimeError.Create('Index file not found: ' + FIndexFile);
  Index := TDcuIndexJsonReader.LoadFromFile(FIndexFile);

  for K := Low(TDcuKnownCompiler) to High(TDcuKnownCompiler) do
    CompilerHits[K] := 0;
  for P := Low(TDcuPlatform) to High(TDcuPlatform) do
    PlatformHits[P] := 0;
  TotalSize := 0;
  for Entry in Index.Units do
  begin
    Inc(CompilerHits[Entry.Compiler]);
    Inc(PlatformHits[Entry.Platform]);
    Inc(TotalSize, Entry.FileSize);
  end;
  ReverseCount := 0;
  if Index.ReverseImportIndex <> nil then
    ReverseCount := Index.ReverseImportIndex.Count;

  Writeln('Index file: ' + FIndexFile);
  Writeln('Schema:     ' + IntToStr(Index.Schema));
  Writeln('Root dirs:');
  for var Dir in Index.RootDirs do
    Writeln('  - ' + Dir);
  Writeln(Format('DCU count:  %d (total %d bytes)',
    [Index.Units.Count, TotalSize]));
  Writeln('By compiler:');
  for K := Low(TDcuKnownCompiler) to High(TDcuKnownCompiler) do
    if CompilerHits[K] > 0 then
      Writeln(Format('  %-22s %d', [DcuKnownCompilerName[K], CompilerHits[K]]));
  Writeln('By platform:');
  for P := Low(TDcuPlatform) to High(TDcuPlatform) do
    if PlatformHits[P] > 0 then
      Writeln(Format('  %-22s %d', [DcuPlatformName[P], PlatformHits[P]]));
  Writeln(Format('Distinct imported units (reverse-index keys): %d',
    [ReverseCount]));
end;

procedure TDptDcuIndexTask.Execute;
begin
  case FMode of
    dimBuild: ExecuteBuild;
    dimQuery: ExecuteQuery;
    dimStats: ExecuteStats;
  else
    raise EDptRuntimeError.Create('No DcuIndex sub-mode set');
  end;
end;

end.
