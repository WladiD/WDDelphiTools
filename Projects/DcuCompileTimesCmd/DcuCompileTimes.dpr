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
  Winapi.Windows;

type

  TFileInfo = record
    Path: String;
    LastWriteTime: Int64;
    Diff: Int64;
  end;

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

const
  TicksPerMillisecond = 10000;

var
  FileInfo  : TFileInfo;
  FileList  : TList<TFileInfo>;
  Files     : TStringDynArray;
  I         : Integer;
  SearchMask: String;
  TotalDiff : Int64;
begin
  try
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
            if Left.LastWriteTime > Right.LastWriteTime then
              Result := -1
            else if Left.LastWriteTime < Right.LastWriteTime then
              Result := 1
            else
              Result := 0;
          end
        )
      );

      TotalDiff := 0;
      for I := 0 to FileList.Count - 2 do
      begin
        var CurrentFile := FileList[I];
        CurrentFile.Diff := Abs(FileList[I].LastWriteTime - FileList[I+1].LastWriteTime);
        FileList[I] := CurrentFile;
        TotalDiff := TotalDiff + CurrentFile.Diff;
      end;

      // Sort by diff
      FileList.Sort(
        TComparer<TFileInfo>.Construct(
          function(const Left, Right: TFileInfo): Integer
          begin
            if Left.Diff > Right.Diff then
              Result := -1
            else if Left.Diff < Right.Diff then
              Result := 1
            else
              Result := 0;
          end
        )
      );

      Writeln('Files, sorted by generation time:');
      Writeln('---------------------------------------------------');

      for I := 0 to FileList.Count - 1 do
      begin
        if FileList[I].Diff > 0 then
          Writeln(Format('%-40s (%.4f ms)', [TPath.GetFileName(FileList[I].Path), FileList[I].Diff / TicksPerMillisecond]));
      end;

      Writeln('---------------------------------------------------');
      Writeln(Format('Total time: %.4f ms', [TotalDiff / TicksPerMillisecond]));
    finally
      FileList.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.