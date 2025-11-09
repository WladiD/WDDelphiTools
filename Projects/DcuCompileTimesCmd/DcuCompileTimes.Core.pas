unit DcuCompileTimes.Core;

interface

uses

  System.SysUtils,
  System.Generics.Collections,
  Winapi.Windows;

type

  TFileInfo = record
    Path: String;
    LastWriteTime: Int64;
    Diff: Int64;
  end;

  TNamespaceInfo = record
    TotalTime: Int64;
    FileCount: Integer;
  end;

  TDcuAnalyzer = class
  private
    FFileList: TList<TFileInfo>;
    FTotalDiff: Int64;
    FMedian: Int64;
    FOldestFile: TFileInfo;
    FSearchPath: String;
    FSearchMask: String;
    function GetFileLastWriteTime(const FilePath: String): TFileTime;
    function FileTimeToLargeInteger(const FileTime: TFileTime): Int64;
  public
    constructor Create(const ASearchPath: String; const ASearchMask: String = '*.dcu');
    destructor Destroy; override;
    procedure Execute;
    function GetNamespaceStats(const AFileList: TList<TFileInfo> = nil): TDictionary<string, TNamespaceInfo>;
    function IsIncompleteBuild: Boolean;
    property Files: TList<TFileInfo> read FFileList;
    property TotalTime: Int64 read FTotalDiff;
    property MedianTime: Int64 read FMedian;
    property OldestFile: TFileInfo read FOldestFile;
  end;

implementation

uses
  System.IOUtils,
  System.Generics.Defaults,
  System.Math,
  System.DateUtils,
  System.Types;

{ TDcuAnalyzer }

constructor TDcuAnalyzer.Create(const ASearchPath, ASearchMask: string);
begin
  FSearchPath := ASearchPath;
  FSearchMask := ASearchMask;
  FFileList := TList<TFileInfo>.Create;
end;

destructor TDcuAnalyzer.Destroy;
begin
  FFileList.Free;
  inherited;
end;

procedure TDcuAnalyzer.Execute;
var
  Files: TArray<string>;
  I: Integer;
  FileInfo: TFileInfo;
  CurrentFile: TFileInfo;
  MidIndex: Integer;
begin
  Files := TDirectory.GetFiles(FSearchPath, FSearchMask);

  for I := 0 to High(Files) do
  begin
    FileInfo.Path := Files[I];
    FileInfo.LastWriteTime := FileTimeToLargeInteger(GetFileLastWriteTime(FileInfo.Path));
    FileInfo.Diff := 0;
    FFileList.Add(FileInfo);
  end;

  if FFileList.Count = 0 then
    Exit;

  // Sort by last write time (descending, so oldest is last)
  FFileList.Sort(
    TComparer<TFileInfo>.Construct(
      function(const Left, Right: TFileInfo): Integer
      begin
        Result := Sign(Right.LastWriteTime - Left.LastWriteTime);
      end));

  if FFileList.Count > 0 then
    FOldestFile := FFileList.Last; // Store the oldest file before further sorting

  FTotalDiff := 0;
  for I := 0 to FFileList.Count - 2 do
  begin
    CurrentFile := FFileList[I];
    CurrentFile.Diff := Abs(FFileList[I].LastWriteTime - FFileList[I+1].LastWriteTime);
    FFileList[I] := CurrentFile;
    FTotalDiff := FTotalDiff + CurrentFile.Diff;
  end;

  // Sort by diff (descending) to calculate median
  var TempList := TList<TFileInfo>.Create;
  TempList.AddRange(FFileList);
  try
    TempList.Sort(
      TComparer<TFileInfo>.Construct(
        function(const Left, Right: TFileInfo): Integer
        begin
          Result := Sign(Right.Diff - Left.Diff);
        end));

    MidIndex := TempList.Count div 2;
    if (TempList.Count mod 2) = 1 then
      FMedian := TempList[MidIndex].Diff
    else
      FMedian := (TempList[MidIndex - 1].Diff + TempList[MidIndex].Diff) div 2;
  finally
    TempList.Free;
  end;

  // Sort back by time for correct display order
  FFileList.Sort(
    TComparer<TFileInfo>.Construct(
      function(const Left, Right: TFileInfo): Integer
      begin
        Result := Sign(Right.Diff - Left.Diff);
      end));
end;

function TDcuAnalyzer.FileTimeToLargeInteger(const FileTime: TFileTime): Int64;
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

function TDcuAnalyzer.GetFileLastWriteTime(const FilePath: String): TFileTime;
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

function TDcuAnalyzer.GetNamespaceStats(const AFileList: TList<TFileInfo>): TDictionary<string, TNamespaceInfo>;
  procedure AddNamespaceStat(const ANamespacePrefix: String; const AFileRec: TFileInfo);
  var
    Info: TNamespaceInfo;
  begin
    if Result.TryGetValue(ANamespacePrefix, Info) then
    begin
      Info.TotalTime := Info.TotalTime + AFileRec.Diff;
      Inc(Info.FileCount);
    end
    else
    begin
      Info.TotalTime := AFileRec.Diff;
      Info.FileCount := 1;
    end;
    Result.AddOrSetValue(ANamespacePrefix, Info);
  end;
var
  FileName       : String;
  FileRec        : TFileInfo;
  J              : Integer;
  NamespacePrefix: String;
  Parts          : TArray<string>;
  SourceList     : TList<TFileInfo>;
begin
  Result := TDictionary<String, TNamespaceInfo>.Create;

  if Assigned(AFileList) then
    SourceList := AFileList
  else
    SourceList := FFileList;

  for FileRec in SourceList do
  begin
    if FileRec.Diff <= 0 then
      Continue;

    FileName := TPath.GetFileNameWithoutExtension(FileRec.Path);
    Parts := FileName.Split(['.']);
    if Length(Parts) <= 1 then
      Continue;

    for J := 0 to High(Parts) do
    begin
      if J > 0 then
        NamespacePrefix := NamespacePrefix + '.' + Parts[J]
      else
        NamespacePrefix := Parts[J];
      AddNamespaceStat(NamespacePrefix, FileRec);
    end;
  end;
end;

function TDcuAnalyzer.IsIncompleteBuild: Boolean;
const
  TicksPerMillisecond = 10000;
begin
  Result := False;

  if (FFileList.Count < 2) or (FMedian <= 0) then
    Exit;

  // Find the first gap, which is a diff that is much larger than the median
  for var I: Integer := 0 to FFileList.Count - 2 do
  begin
    // Heuristic: A gap is a diff > 100x the median, and also at least 10 seconds absolute.
    if (FFileList[I].Diff > FMedian * 100) and (FFileList[I].Diff > TicksPerMillisecond * 10000) then
      Exit(True);
  end;
end;

end.
