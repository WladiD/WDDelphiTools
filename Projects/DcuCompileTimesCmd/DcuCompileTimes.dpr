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

function IsIncompleteBuild(const AFileList: TList<TFileInfo>): Boolean;
var
  DiffsSorted: TList<Int64>;
  MedianDiff : Int64;
  I          : Integer;
begin
  Result := False;
  DiffsSorted := TList<Int64>.Create;
  try
    if AFileList.Count > 1 then
    begin
      // List of the diffs to find the median diff
      for I := 0 to AFileList.Count - 2 do
        DiffsSorted.Add(AFileList[I].Diff);
      DiffsSorted.Sort;

      if DiffsSorted.Count > 0 then
        MedianDiff := DiffsSorted[DiffsSorted.Count div 2]
      else
        Exit;

      if MedianDiff > 0 then
      begin
        // Find the first gap, which is a diff that is much larger than the median
        for I := 0 to AFileList.Count - 2 do
        begin
          // Heuristic: A gap is a diff > 100x the median, and also at least 2 seconds absolute.
          if (AFileList[I].Diff > MedianDiff * 100) and (AFileList[I].Diff > TicksPerMillisecond * 2000) then
            Exit(True);
        end;
      end;
    end;
  finally
    DiffsSorted.Free;
  end;
end;

var
  DiffsSorted  : TList<Int64>;
  FileInfo     : TFileInfo;
  FileList     : TList<TFileInfo>;
  Files        : TStringDynArray;
  FilterMask   : String;
  HasFilterMask: Boolean;
  I            : Integer;
  MaskTotalDiff: Int64;
  Median       : Int64;
  MedianDiff   : Int64;
  OutlierIndex : Integer;
  SearchMask   : String;
  TotalDiff    : Int64;
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

      // Sort by last write time
      FileList.Sort(
        TComparer<TFileInfo>.Construct(
          function(const Left, Right: TFileInfo): Integer
          begin
            Result := Sign(Right.LastWriteTime - Left.LastWriteTime);
          end));

      TotalDiff := 0;
      for I := 0 to FileList.Count - 2 do
      begin
        var CurrentFile := FileList[I];
        CurrentFile.Diff := Abs(FileList[I].LastWriteTime - FileList[I+1].LastWriteTime);
        FileList[I] := CurrentFile;
        TotalDiff := TotalDiff + CurrentFile.Diff;
      end;

      if IsIncompleteBuild(FileList) then
      begin
        Writeln('Error: Incomplete build detected. Please perform a full, clean build before running the tool.');
        Writeln('Analysis aborted.');
        Exit;
      end;

      // Sort by diff
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

      Writeln('------------------------------------------------------------');
      Writeln('Files, sorted by generation time (ms), factor to median (x):');
      Writeln('------------------------------------------------------------');

      var PrintFile: Boolean := not HasFilterMask;
      for I := 0 to FileList.Count - 1 do
      begin
        if FileList[I].Diff > 0 then
        begin
          if HasFilterMask then
          begin
            PrintFile := MatchesMask(TPath.GetFileName(FileList[I].Path), FilterMask);
            if PrintFile then
              MaskTotalDiff := MaskTotalDiff + FileList[I].Diff;
          end;

          if PrintFile then
          begin
            var Factor: Double := 0;
            if Median > 0 then
              Factor := FileList[I].Diff / Median;
            Writeln(Format('%-40s (%.4f ms) (x%.2f)', [TPath.GetFileName(FileList[I].Path), FileList[I].Diff / TicksPerMillisecond, Factor]));
          end;
        end;
      end;

      Writeln('------------------------------------------------------------');

      Writeln(Format('Total time : %.4f ms', [TotalDiff / TicksPerMillisecond]));
      if Median > 0 then
        Writeln(Format('Median time: %.4f ms', [Median / TicksPerMillisecond]));
      if HasFilterMask then
      begin
        var Percentage: Double := 0;
        if TotalDiff > 0 then
          Percentage := (MaskTotalDiff / TotalDiff) * 100;
        Writeln(Format('Mask time  : %.4f ms (%.2f%%)', [MaskTotalDiff / TicksPerMillisecond, Percentage]));
      end;
    finally
      FileList.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
