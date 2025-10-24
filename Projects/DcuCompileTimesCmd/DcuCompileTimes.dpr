program LatestDateTimeDiffs;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.Generics.Defaults,
  System.Types,
  System.Math;

type
  TFileInfo = record
    Path: string;
    LastWriteTime: TDateTime;
    Diff: Int64;
  end;

var
  FileList: TList<TFileInfo>;
  SearchMask: string;
  Files: TStringDynArray;
  I: Integer;
  FileInfo: TFileInfo;
  TotalDiff: Int64;

begin
  try
    if ParamCount < 1 then
    begin
      Writeln('Usage: LatestDateTimeDiffs.exe <filemask> [topcount]');
      Exit;
    end;

    SearchMask := ParamStr(1);

    FileList := TList<TFileInfo>.Create;
    try
      Files := TDirectory.GetFiles('.', SearchMask);

      for I := 0 to High(Files) do
      begin
        FileInfo.Path := Files[I];
        FileInfo.LastWriteTime := TFile.GetLastWriteTime(FileInfo.Path);
        FileInfo.Diff := 0; // Initialisieren
        FileList.Add(FileInfo);
      end;

      if FileList.Count = 0 then
      begin
        Writeln('No files found matching mask: ' + SearchMask);
        Exit;
      end;

      // Nach Datum sortieren, um die Differenzen zu berechnen
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
        CurrentFile.Diff := Abs(MilliSecondsBetween(FileList[I].LastWriteTime, FileList[I+1].LastWriteTime));
        FileList[I] := CurrentFile;
        TotalDiff := TotalDiff + CurrentFile.Diff;
      end;

//      Writeln('Files sorted by modification date (newest first):');
//      Writeln('---------------------------------------------------');
//      for I := 0 to FileList.Count - 2 do
//      begin
//        Writeln(Format('%-40s (%d ms)', [TPath.GetFileName(FileList[I].Path), FileList[I].Diff]));
//      end;
//
//      if FileList.Count > 0 then
//        Writeln(Format('%-40s', [TPath.GetFileName(FileList[FileList.Count - 1].Path)]));
//
//      Writeln('---------------------------------------------------');

      // Nach der größten Differenz sortieren
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
          Writeln(Format('%-40s (%d ms)', [TPath.GetFileName(FileList[I].Path), FileList[I].Diff]));
      end;

      Writeln('---------------------------------------------------');
      Writeln(Format('Total time: %d ms', [TotalDiff]));


    finally
      FileList.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
