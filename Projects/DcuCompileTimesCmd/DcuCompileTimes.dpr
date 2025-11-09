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
  Winapi.Windows;

type

  TFileInfo = record
    Path: String;
    LastWriteTime: Int64;
    Diff: Int64;
  end;

const
  TicksPerMillisecond = 10000;

function FileTimeToLargeInteger(const FileTime: TFileTime): Int64;
type
  TULARGE_INTEGER = record
    case Integer of
      0: (LowPart: DWORD; HighPart: DWORD);
      1: (QuadPart: Int64);
  end;
var
  LI: TULARGE_INTEGER;
begin
  LI.LowPart := FileTime.dwLowDateTime;
  LI.HighPart := FileTime.dwHighDateTime;
  Result := LI.QuadPart;
end;

function GetFileLastWriteTime(const FilePath: String): TFileTime;
var
  FindData: TWin32FindData;
  FindHandle: THandle;
begin
  FindHandle := FindFirstFile(PChar(FilePath), FindData);
  if FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := FindData.ftLastWriteTime;
    FindClose(FindHandle);
  end
  else
    RaiseLastOSError;
end;

function IsIncompleteBuild(const AFileList: TList<TFileInfo>; const AMedian: Int64): Boolean;
begin
  Result := False;

  if (AFileList.Count < 2) or (AMedian <= 0) then
    Exit;

  // Find the first gap, which is a diff that is much larger than the median
  for var I: Integer := 0 to AFileList.Count - 2 do
  begin
    // Heuristic: A gap is a diff > 100x the median, and also at least 10 seconds absolute.
    if (AFileList[I].Diff > AMedian * 100) and (AFileList[I].Diff > TicksPerMillisecond * 10000) then
      Exit(True);
  end;
end;

var
  FileInfo     : TFileInfo;
  FileList     : TList<TFileInfo>;
  PrintList    : TList<TFileInfo>;
  Files        : TStringDynArray;
  FilterMask   : String;
  HasFilterMask: Boolean;
  MaskTotalDiff: Int64;
  Median       : Int64;
  SearchMask   : String;
  TotalDiff    : Int64;

function CreatePrintList: TList<TFileInfo>;
begin
  Assert(HasFilterMask);

  Result := TList<TFileInfo>.Create;
  try
    for var Loop: Integer := 0 to FileList.Count - 1 do
    begin
      if MatchesMask(TPath.GetFileName(FileList[Loop].Path), FilterMask) then
      begin
        MaskTotalDiff := MaskTotalDiff + FileList[Loop].Diff;
        Result.Add(FileList[Loop]);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure PrintStats;
begin
  Writeln(Format('Total time  : %.4f ms', [TotalDiff / TicksPerMillisecond]));
  if Median > 0 then
    Writeln(Format('Median time : %.4f ms', [Median / TicksPerMillisecond]));
  if HasFilterMask then
  begin
    var Percentage: Double := 0;
    if TotalDiff > 0 then
      Percentage := (MaskTotalDiff / TotalDiff) * 100;
    Writeln(Format('Mask matches: %d',[PrintList.Count]));
    Writeln(Format('Mask time   : %.4f ms (%.2f%%)', [MaskTotalDiff / TicksPerMillisecond, Percentage]));
  end;
end;

var
  I: Integer;
begin
  try
    if ParamCount > 0 then
      FilterMask := ParamStr(1)
    else
      FilterMask := '';
    MaskTotalDiff := 0;
    HasFilterMask:=FilterMask <> '';

    if SameText(FilterMask, '--help') or SameText(FilterMask, '-h') or (FilterMask = '?') then
    begin
      Writeln('DcuCompileTimes.exe [FilterMask]');
      Writeln;
      Writeln('Analyzes Delphi .dcu compilation times in the current directory.');
      Writeln;
      Writeln('Arguments:');
      Writeln('  [FilterMask]  Optional. A file mask (e.g., "Base.*", "Base.Db.*") to filter');
      Writeln('                the displayed files and calculate a "Mask time" summary.');
      Writeln;
      Writeln('Options:');
      Writeln('  --help, -h, ? Displays this help message.');
      Exit;
    end;

    SearchMask := '*.dcu';

    PrintList := nil;
    FileList := TList<TFileInfo>.Create;
    try
      Files := TDirectory.GetFiles('.', SearchMask);

      for I := 0 to High(Files) do
      begin
        FileInfo.Path := Files[I];
        FileInfo.LastWriteTime := FileTimeToLargeInteger(GetFileLastWriteTime(FileInfo.Path));
        FileInfo.Diff := 0;
        FileList.Add(FileInfo);
      end;

      if FileList.Count = 0 then
      begin
        Writeln('No files found matching mask: ' + SearchMask);
        Exit;
      end;

      // Sort by last write time (descending, so oldest is last)
      FileList.Sort(
        TComparer<TFileInfo>.Construct(
          function(const Left, Right: TFileInfo): Integer
          begin
            Result := Sign(Right.LastWriteTime - Left.LastWriteTime);
          end));

      var OldestFileInList: TFileInfo;
      if FileList.Count > 0 then
        OldestFileInList := FileList.Last; // Store the oldest file before further sorting

      TotalDiff := 0;
      for I := 0 to FileList.Count - 2 do
      begin
        var CurrentFile := FileList[I];
        CurrentFile.Diff := Abs(FileList[I].LastWriteTime - FileList[I+1].LastWriteTime);
        FileList[I] := CurrentFile;
        TotalDiff := TotalDiff + CurrentFile.Diff;
      end;

      // Sort by diff (descending)
      FileList.Sort(
        TComparer<TFileInfo>.Construct(
          function(const Left, Right: TFileInfo): Integer
          begin
            Result := Sign(Right.Diff - Left.Diff);
          end));

      var MidIndex: Integer := FileList.Count div 2;
      if (FileList.Count mod 2) = 1 then
        Median := FileList[MidIndex].Diff
      else
        Median := (FileList[MidIndex - 1].Diff + FileList[MidIndex].Diff) div 2;

      if IsIncompleteBuild(FileList, Median) then
      begin
        Writeln('Error: Incomplete build detected. Please perform a full, clean build before running the tool.');
        Writeln('Analysis aborted.');
        Exit;
      end;

      if HasFilterMask then
        PrintList := CreatePrintList
      else
        PrintList := FileList;

      if PrintList.Count >= 30 then
        PrintStats;

      Writeln('------------------------------------------------------------');
      Writeln('Files, sorted by generation time (ms), factor to median (x):');
      Writeln('------------------------------------------------------------');

      for I := 0 to PrintList.Count - 1 do
      begin
        if PrintList[I].Diff > 0 then
        begin
          var Factor: Double := 0;
          if Median > 0 then
            Factor := PrintList[I].Diff / Median;
          Writeln(Format('%-40s (%.4f ms) (x%.2f)', [TPath.GetFileName(PrintList[I].Path), PrintList[I].Diff / TicksPerMillisecond, Factor]));
        end;
      end;

      // Use the stored OldestFileInList for the build start reference
      if OldestFileInList.Path <> '' then
        Writeln(Format('%s <- build start %s',
          [TPath.GetFileName(OldestFileInList.Path), DateTimeToStr(TFile.GetLastWriteTime(OldestFileInList.Path))]));

      Writeln('------------------------------------------------------------');
      PrintStats;

    finally
      PrintList.Free;
      if PrintList<>FileList then
        FileList.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
