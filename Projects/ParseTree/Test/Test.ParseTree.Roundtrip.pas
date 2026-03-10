unit Test.ParseTree.Roundtrip;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Threading,
  System.SyncObjs,
  System.Types,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Tokens,
  ParseTree.Nodes,
  ParseTree.Parser,
  ParseTree.Writer;

type
  [TestFixture]
  TParseTreeRoundtripTest = class
  private
    procedure DoRoundtripTest(const AFilePath: string);
  public
    [Test]
    procedure TestConfiguredPathsParallel;
  end;

implementation

{ TParseTreeRoundtripTest }

procedure TParseTreeRoundtripTest.DoRoundtripTest(const AFilePath: string);
var
  LParser: TParseTreeParser;
  LOriContent: string;
  LTree: TCompilationUnitSyntax;
  LTreeWriter: TSyntaxTreeWriter;
  LNewContent: string;
  LBaseDir: string;
  LOutputFolder: string;
  LOutputFile: string;
  LOriLines: TArray<string>;
  LNewLines: TArray<string>;
  I: Integer;
  LMsg: string;
begin
  Assert.IsTrue(TFile.Exists(AFilePath), 'Source file does not exist: ' + AFilePath);

  // Prefer UTF-8, but fall back to system default for legacy ANSI Delphi sources.
  try
    LOriContent := TFile.ReadAllText(AFilePath, TEncoding.UTF8);
  except
    on EEncodingError do
      LOriContent := TFile.ReadAllText(AFilePath, TEncoding.Default);
  end;

  LParser := TParseTreeParser.Create;
  try
    LTree := LParser.Parse(LOriContent);
    try
      LTreeWriter := TSyntaxTreeWriter.Create;
      try
        LNewContent := LTreeWriter.GenerateSource(LTree);
      finally
        LTreeWriter.Free;
      end;
    finally
      LTree.Free;
    end;
  finally
    LParser.Free;
  end;

  // Save to RoundTripTest output folder
  LBaseDir := ExtractFilePath(ParamStr(0));
  LOutputFolder := TPath.Combine(LBaseDir, 'RoundTripTest');
  if not TDirectory.Exists(LOutputFolder) then
    TDirectory.CreateDirectory(LOutputFolder);
    
  LOutputFile := TPath.Combine(LOutputFolder, ExtractFileName(AFilePath));
  TFile.WriteAllText(LOutputFile, LNewContent, TEncoding.UTF8);

  // Find and report first difference
  if LOriContent <> LNewContent then
  begin
    LOriLines := LOriContent.Split([#13#10, #13, #10]);
    LNewLines := LNewContent.Split([#13#10, #13, #10]);
    LMsg := Format('Roundtrip failed for %s. L_ORI=%d, L_RT=%d. Output: %s', 
      [ExtractFileName(AFilePath), Length(LOriContent), Length(LNewContent), LOutputFile]);

    var sOriHex := '';
    var sRtHex := '';
    for var k := 1 to 8 do
    begin
        if k <= Length(LOriContent) then sOriHex := sOriHex + IntToHex(Ord(LOriContent[k]), 2) + ' ' else sOriHex := sOriHex + '.. ';
        if k <= Length(LNewContent) then sRtHex := sRtHex + IntToHex(Ord(LNewContent[k]), 2) + ' ' else sRtHex := sRtHex + '.. ';
    end;
    LMsg := LMsg + sLineBreak + 'HEX Start ORI: ' + sOriHex;
    LMsg := LMsg + sLineBreak + 'HEX Start RT : ' + sRtHex;

    for I := 0 to Length(LOriLines) - 1 do
    begin
      if (I >= Length(LNewLines)) then
      begin
        LMsg := LMsg + sLineBreak + Format('Line %d missing in roundtrip (original has %d lines, roundtrip has %d)',
          [I + 1, Length(LOriLines), Length(LNewLines)]);
        Break;
      end;
      if LOriLines[I] <> LNewLines[I] then
      begin
        LMsg := LMsg + sLineBreak + Format('First diff at line %d:', [I + 1])
          + sLineBreak + '  ORI: [' + LOriLines[I] + ']'
          + sLineBreak + '  RT:  [' + LNewLines[I] + ']';
        Break;
      end;
    end;
    if (Length(LNewLines) > Length(LOriLines)) then
      LMsg := LMsg + sLineBreak + Format('Roundtrip has %d extra lines', [Length(LNewLines) - Length(LOriLines)]);
    Assert.Fail(LMsg);
  end;
end;

procedure TParseTreeRoundtripTest.TestConfiguredPathsParallel;
var
  LTestProjectDir: string;
  LPathsFile: string;
  LOutputFolder: string;
  LPathLines: TStringList;
  LAllFiles: TStringList;
  LFilesInPath: TStringDynArray;
  LPathLine: string;
  LPathLineTrimmed: string;
  LResolvedPath: string;
  LCheckedPathCount: Integer;
  LSummary: string;
  LThreadPool: TThreadPool;
  LFailedFiles: TStringList;
  LLock: System.SyncObjs.TCriticalSection;
begin
  // ParamStr(0) points to Win32\Debug\Test.ParseTree.exe during test runs.
  // Relative paths from RoundTripTestPaths.txt are grounded to Projects\ParseTree\Test.
  LTestProjectDir := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\'));
  LPathsFile := TPath.Combine(LTestProjectDir, 'RoundTripTestPaths.txt');
  LOutputFolder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'RoundTripTest');

  if TDirectory.Exists(LOutputFolder) then
    TDirectory.Delete(LOutputFolder, True);
  TDirectory.CreateDirectory(LOutputFolder);

  if not TFile.Exists(LPathsFile) then
    Assert.Fail('Roundtrip path config does not exist: ' + LPathsFile);

  LPathLines := TStringList.Create;
  LAllFiles := TStringList.Create;
  LAllFiles.Sorted := True;
  LAllFiles.Duplicates := dupIgnore;
  try
    LPathLines.LoadFromFile(LPathsFile, TEncoding.UTF8);
    LCheckedPathCount := 0;

    for LPathLine in LPathLines do
    begin
      LPathLineTrimmed := Trim(LPathLine);
      if (LPathLineTrimmed = '') or LPathLineTrimmed.StartsWith('#') then
        Continue;

      if TPath.IsPathRooted(LPathLineTrimmed) then
        LResolvedPath := TPath.GetFullPath(LPathLineTrimmed)
      else
        LResolvedPath := TPath.GetFullPath(TPath.Combine(LTestProjectDir, LPathLineTrimmed));

      if not TDirectory.Exists(LResolvedPath) then
        Assert.Fail(Format('Configured roundtrip directory does not exist: "%s" (from "%s")',
          [LResolvedPath, LPathLineTrimmed]));

      LFilesInPath := TDirectory.GetFiles(LResolvedPath, '*.pas', TSearchOption.soTopDirectoryOnly);
      if Length(LFilesInPath) = 0 then
        Assert.Fail(Format('Configured roundtrip directory contains no *.pas files: "%s" (from "%s")',
          [LResolvedPath, LPathLineTrimmed]));

      Inc(LCheckedPathCount);
      for var LFile in LFilesInPath do
        LAllFiles.Add(LFile);
    end;

    LSummary := Format('Checked %d files from %d paths', [LAllFiles.Count, LCheckedPathCount]);

    if LAllFiles.Count = 0 then
      Assert.Fail('No *.pas files found in configured roundtrip paths: ' + LPathsFile + sLineBreak + LSummary);
  finally
    LPathLines.Free;
  end;

  LFailedFiles := TStringList.Create;
  LLock := System.SyncObjs.TCriticalSection.Create;
  LThreadPool := TThreadPool.Create;
  try
    LThreadPool.MaxWorkerThreads := 2;
    try
      TParallel.For(0, LAllFiles.Count - 1,
        procedure(I: Integer)
        begin
          try
            DoRoundtripTest(LAllFiles[I]);
          except
            on E: Exception do
            begin
              LLock.Enter;
              try
                LFailedFiles.Add(Format('%s (%s: %s)', [ExtractFileName(LAllFiles[I]), E.ClassName, E.Message]));
              finally
                LLock.Leave;
              end;
            end;
          end;
        end,
        LThreadPool);
    except
      on E: EAggregateException do
      begin
        for var I := 0 to E.Count - 1 do
        begin
          LLock.Enter;
          try
            LFailedFiles.Add(Format('AggregateException[%d]: %s: %s', [I, E.InnerExceptions[I].ClassName, E.InnerExceptions[I].Message]));
          finally
            LLock.Leave;
          end;
        end;
      end;
    end;

    if LFailedFiles.Count > 0 then
      Assert.Fail(Format('%s'#13#10'Roundtrip failed for %d files:'#13#10'%s',
        [LSummary, LFailedFiles.Count, LFailedFiles.Text]))
    else
      System.Writeln(Format('Successfully checked %d files from %d paths.', [LAllFiles.Count, LCheckedPathCount]));
  finally
    LThreadPool.Free;
    LLock.Free;
    LFailedFiles.Free;
    LAllFiles.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeRoundtripTest);

end.
