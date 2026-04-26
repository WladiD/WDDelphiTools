unit Test.DPT.Dcu.MultiVersion;

interface

uses

  DUnitX.TestFramework,

  System.Classes,
  System.IOUtils,
  System.SysUtils,

  JclIDEUtils,

  mormot.core.collections,

  DPT.Dcu.Analyzer,
  DPT.Dcu.Types,
  DPT.Types;

type

  /// <summary>
  ///   Confirms the multi-version adapter promise: for every Delphi
  ///   installation present on the build machine, the iteration-1
  ///   analyzer must successfully classify a freshly produced DCU as a
  ///   DCU, extract the unit name, and at least find "System" in the
  ///   interface uses table.
  ///
  ///   When a future Delphi version changes the DCU layout in a way that
  ///   breaks one of these guarantees, this test fails with the offending
  ///   version reported in the assertion message - the signal that the
  ///   format adapter table needs another row.
  /// </summary>
  [TestFixture]
  TTestDcuMultiVersion = class
  private
    type
      TVersionCheckResult = record
        VersionLabel: string;
        Ok          : Boolean;
        Reason      : string;
        MagicHex    : string;
      end;
    function CompileFixture(AInstallation: TJclBorRADToolInstallation;
      const AOutputDir, AFixtureName: string; out AErrorOutput: string): Boolean;
    function FindRsvars(AInstallation: TJclBorRADToolInstallation): string;
    function MakeFixturePas(const ADir, AUnitName: string): string;
    function RunRedirectedCmd(const ACommandLine, AWorkingDir: string;
      out AOutput: string): Integer;
    function ExpectedCompilerFor(AInstallation: TJclBorRADToolInstallation): TDcuKnownCompiler;
    function VersionLabelOf(AInstallation: TJclBorRADToolInstallation): string;
  public
    [Test] procedure AnalyzerHandlesEveryInstalledDelphiVersion;
  end;

implementation

uses

  Winapi.Windows;

const
  FixtureUnitName = 'DptDcuFixture';
  // The fixture mentions one specific interface uses entry and one
  // specific implementation uses entry so the analyzer's scope
  // discrimination can be verified independently of the compiler.
  FixtureInterfaceUsesUnit = 'System.SysUtils';
  FixtureImplementationUsesUnit = 'System.Classes';
  FixtureSource =
    'unit ' + FixtureUnitName + ';' + sLineBreak +
    sLineBreak +
    'interface' + sLineBreak +
    sLineBreak +
    'uses' + sLineBreak +
    '  ' + FixtureInterfaceUsesUnit + ';' + sLineBreak +
    sLineBreak +
    'function GiveMessage: string;' + sLineBreak +
    sLineBreak +
    'implementation' + sLineBreak +
    sLineBreak +
    'uses' + sLineBreak +
    '  ' + FixtureImplementationUsesUnit + ';' + sLineBreak +
    sLineBreak +
    'function GiveMessage: string;' + sLineBreak +
    'var' + sLineBreak +
    '  L: TStringList;' + sLineBreak +
    'begin' + sLineBreak +
    '  L := TStringList.Create;' + sLineBreak +
    '  try' + sLineBreak +
    '    L.Add(IntToStr(1));' + sLineBreak +
    '    Result := L.Text;' + sLineBreak +
    '  finally' + sLineBreak +
    '    L.Free;' + sLineBreak +
    '  end;' + sLineBreak +
    'end;' + sLineBreak +
    sLineBreak +
    'end.' + sLineBreak;

{ TTestDcuMultiVersion }

function TTestDcuMultiVersion.MakeFixturePas(const ADir, AUnitName: string): string;
var
  Content: string;
begin
  ForceDirectories(ADir);
  Result := TPath.Combine(ADir, AUnitName + '.pas');
  Content := StringReplace(FixtureSource, FixtureUnitName, AUnitName, [rfReplaceAll]);
  TFile.WriteAllText(Result, Content, TEncoding.UTF8);
end;

function TTestDcuMultiVersion.FindRsvars(AInstallation: TJclBorRADToolInstallation): string;
begin
  Result := IncludeTrailingPathDelimiter(AInstallation.BinFolderName) + 'rsvars.bat';
  if not FileExists(Result) then
    Result := '';
end;

function TTestDcuMultiVersion.RunRedirectedCmd(const ACommandLine, AWorkingDir: string;
  out AOutput: string): Integer;
var
  Buffer       : array[0..4095] of Byte;
  BytesRead    : DWORD;
  Chunk        : AnsiString;
  Cmd          : string;
  PI           : TProcessInformation;
  ReadHandle   : THandle;
  SA           : TSecurityAttributes;
  SI           : TStartupInfo;
  WriteHandle  : THandle;
  Output       : AnsiString;
begin
  AOutput := '';
  FillChar(SA, SizeOf(SA), 0);
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  if not CreatePipe(ReadHandle, WriteHandle, @SA, 0) then
    RaiseLastOSError;

  try
    SetHandleInformation(ReadHandle, HANDLE_FLAG_INHERIT, 0);

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_HIDE;
    SI.hStdOutput := WriteHandle;
    SI.hStdError := WriteHandle;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);

    Cmd := ACommandLine;
    UniqueString(Cmd);

    if not CreateProcess(nil, PChar(Cmd), nil, nil, True, CREATE_NO_WINDOW,
      nil, PChar(AWorkingDir), SI, PI)
    then
      RaiseLastOSError;

    CloseHandle(WriteHandle);
    WriteHandle := 0;

    Output := '';
    repeat
      BytesRead := 0;
      if not ReadFile(ReadHandle, Buffer, SizeOf(Buffer), BytesRead, nil) then
        Break;
      if BytesRead > 0 then
      begin
        SetLength(Chunk, BytesRead);
        Move(Buffer[0], Chunk[1], BytesRead);
        Output := Output + Chunk;
      end;
    until BytesRead = 0;

    WaitForSingleObject(PI.hProcess, INFINITE);
    GetExitCodeProcess(PI.hProcess, DWORD(Result));
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);

    AOutput := string(Output);
  finally
    if ReadHandle <> 0 then
      CloseHandle(ReadHandle);
    if WriteHandle <> 0 then
      CloseHandle(WriteHandle);
  end;
end;

function TTestDcuMultiVersion.CompileFixture(AInstallation: TJclBorRADToolInstallation;
  const AOutputDir, AFixtureName: string; out AErrorOutput: string): Boolean;
var
  Cmd      : string;
  ExitCode : Integer;
  RsvarsBat: string;
  PasPath  : string;
  Dcu32Exe : string;
begin
  Result := False;
  AErrorOutput := '';

  RsvarsBat := FindRsvars(AInstallation);
  if RsvarsBat = '' then
  begin
    AErrorOutput := 'rsvars.bat not found in ' + AInstallation.BinFolderName;
    Exit;
  end;

  Dcu32Exe := IncludeTrailingPathDelimiter(AInstallation.BinFolderName) + 'dcc32.exe';
  if not FileExists(Dcu32Exe) then
  begin
    AErrorOutput := 'dcc32.exe not found at ' + Dcu32Exe;
    Exit;
  end;

  PasPath := MakeFixturePas(AOutputDir, AFixtureName);
  ForceDirectories(AOutputDir);

  Cmd := Format('cmd.exe /c ""%s" && "%s" -B -Q -N"%s" "%s""',
    [RsvarsBat, Dcu32Exe, AOutputDir, PasPath]);

  ExitCode := RunRedirectedCmd(Cmd, AOutputDir, AErrorOutput);
  if ExitCode <> 0 then
  begin
    AErrorOutput := Format('dcc32 exited with %d. Output:%s%s',
      [ExitCode, sLineBreak, AErrorOutput]);
    Exit;
  end;

  Result := FileExists(TPath.Combine(AOutputDir, AFixtureName + '.dcu'));
  if not Result then
    AErrorOutput := 'Compiled successfully but no .dcu produced in ' + AOutputDir;
end;

function TTestDcuMultiVersion.ExpectedCompilerFor(
  AInstallation: TJclBorRADToolInstallation): TDcuKnownCompiler;
begin
  // Mapping IDE version (BDS) -> known compiler for the empirical magic
  // byte table. Only versions that the analyzer can identify return a
  // non-Unknown value; everything else (older or newer than what has
  // been sampled) stays dccUnknown so the test does not impose a
  // mapping the analyzer cannot match yet.
  case AInstallation.IDEVersionNumber of
    22: Result := dccDelphi11;
    23: Result := dccDelphi12;
    37: Result := dccDelphi13;
  else
    Result := dccUnknown;
  end;
end;

function TTestDcuMultiVersion.VersionLabelOf(AInstallation: TJclBorRADToolInstallation): string;
begin
  Result := AInstallation.Name + ' (IDE ' + IntToStr(AInstallation.IDEVersionNumber) + ')';
end;

procedure TTestDcuMultiVersion.AnalyzerHandlesEveryInstalledDelphiVersion;
var
  Failures    : IList<TVersionCheckResult>;
  Inst        : TJclBorRADToolInstallation;
  Installations: TJclBorRADToolInstallations;
  ErrOut      : string;
  FixtureName : string;
  I           : Integer;
  TestOutDir  : string;
  Probed      : IList<TVersionCheckResult>;
  Res         : TDcuAnalysisResult;
  RootTempDir : string;
  Run         : TVersionCheckResult;
  UsesContainsSystem: Boolean;
  Entry       : TDcuUsesEntry;
begin
  Failures := Collections.NewPlainList<TVersionCheckResult>;
  Probed := Collections.NewPlainList<TVersionCheckResult>;

  RootTempDir := TPath.Combine(TPath.GetTempPath,
    'DPT.DcuMultiVersion.' + GUIDToString(TGUID.NewGuid));

  Installations := TJclBorRADToolInstallations.Create;
  try
    if Installations.Count = 0 then
    begin
      // No Delphi present — vacuous pass; nothing to verify.
      Assert.Pass('No Delphi installations detected; multi-version DCU corpus skipped');
      Exit;
    end;

    for I := 0 to Installations.Count - 1 do
    begin
      Inst := Installations[I];
      // We only deal with Delphi personalities (not C++Builder)
      if not (bpDelphi32 in Inst.Personalities) then
        Continue;

      Run.VersionLabel := VersionLabelOf(Inst);
      Run.MagicHex := '';
      Run.Ok := False;
      Run.Reason := '';

      FixtureName := 'DptDcuFixture_v' + IntToStr(Inst.IDEVersionNumber);
      TestOutDir := TPath.Combine(RootTempDir, FixtureName);

      if not CompileFixture(Inst, TestOutDir, FixtureName, ErrOut) then
      begin
        Run.Reason := 'Compilation failed: ' + ErrOut;
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;

      Res := TDcuAnalyzer.Analyze(TPath.Combine(TestOutDir, FixtureName + '.dcu'));
      Run.MagicHex := Res.Header.MagicHex;

      if not Res.IsDcu then
      begin
        Run.Reason := 'IsDcu=false (magic ' + Res.Header.MagicHex + ')';
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;
      // The fixture is compiled with dcc32, so the platform must always
      // resolve to Win32 if the empirical platform table is correct.
      if Res.Header.DetectedPlatform <> dpWin32 then
      begin
        Run.Reason := Format('Platform expected dpWin32, got %s (magic %s)',
          [DcuPlatformName[Res.Header.DetectedPlatform], Res.Header.MagicHex]);
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;
      // Only enforce compiler identification for versions whose byte-3
      // mapping has actually been wired in. Untracked versions keep the
      // run marked OK (they still passed IsDcu/uses checks) but emit a
      // hint in the corpus log.
      var Expected := ExpectedCompilerFor(Inst);
      if (Expected <> dccUnknown) and (Res.Header.DetectedCompiler <> Expected) then
      begin
        Run.Reason := Format('Compiler expected %s, got %s (magic %s)',
          [DcuKnownCompilerName[Expected],
           DcuKnownCompilerName[Res.Header.DetectedCompiler],
           Res.Header.MagicHex]);
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;
      if not SameText(Res.Header.UnitName, FixtureName) then
      begin
        Run.Reason := Format('UnitName mismatch (got "%s", expected "%s")',
          [Res.Header.UnitName, FixtureName]);
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;
      if not Res.UsesParsed then
      begin
        Run.Reason := 'UsesParsed=false';
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;

      // Iteration 2: verify the fixture's specific interface and
      // implementation uses entries are recovered with the right scope.
      UsesContainsSystem := False;
      for Entry in Res.InterfaceUses do
        if SameText(Entry.UnitName, FixtureInterfaceUsesUnit) then
        begin
          UsesContainsSystem := True;
          Break;
        end;
      if not UsesContainsSystem then
      begin
        Run.Reason := Format('Interface uses missing "%s"; got %d interface entries',
          [FixtureInterfaceUsesUnit, Res.InterfaceUses.Count]);
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;

      UsesContainsSystem := False;
      for Entry in Res.ImplementationUses do
        if SameText(Entry.UnitName, FixtureImplementationUsesUnit) then
        begin
          UsesContainsSystem := True;
          Break;
        end;
      if not UsesContainsSystem then
      begin
        Run.Reason := Format('Implementation uses missing "%s"; got %d impl entries',
          [FixtureImplementationUsesUnit, Res.ImplementationUses.Count]);
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;

      // Iteration 4: the fixture references TStringList, calls
      // IntToStr and uses Integer/string types. The symbol scanner
      // must surface at least one type and one method reference.
      if not Res.SymbolsParsed then
      begin
        Run.Reason := 'SymbolsParsed=false on a fixture that references types and methods';
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;
      var FoundType, FoundMethod: Boolean;
      FoundType := False;
      FoundMethod := False;
      for var Sym in Res.Symbols do
        case Sym.Kind of
          dskType  : FoundType := True;
          dskMethod: FoundMethod := True;
        end;
      if not (FoundType and FoundMethod) then
      begin
        Run.Reason := Format(
          'Symbols incomplete: type=%s, method=%s (total %d)',
          [BoolToStr(FoundType, True), BoolToStr(FoundMethod, True),
           Res.Symbols.Count]);
        Failures.Add(Run);
        Probed.Add(Run);
        Continue;
      end;

      Run.Ok := True;
      Run.MagicHex := Res.Header.MagicHex
        + ' -> ' + DcuKnownCompilerName[Res.Header.DetectedCompiler]
        + '/' + DcuPlatformName[Res.Header.DetectedPlatform];
      Probed.Add(Run);
    end;
  finally
    Installations.Free;
  end;

  // Always emit a per-version summary so the magic-byte table can be filled in
  // from the test log over time.
  Writeln('DCU multi-version corpus result:');
  for Run in Probed do
    if Run.Ok then
      Writeln(
        Format('  OK   %-30s magic=[%s]', [Run.VersionLabel, Run.MagicHex]))
    else
      Writeln(
        Format('  FAIL %-30s magic=[%s] - %s',
          [Run.VersionLabel, Run.MagicHex, Run.Reason]));

  // Best effort cleanup of compiled fixtures
  try
    if DirectoryExists(RootTempDir) then
      TDirectory.Delete(RootTempDir, True);
  except
    // Cleanup failures should not mask the assertion result
  end;

  if Probed.Count = 0 then
    Assert.Pass('No Delphi installations with Win32 personality detected; nothing verified')
  else if Failures.Count > 0 then
  begin
    var Msg := Format('%d of %d installed Delphi versions failed the DCU analyzer corpus:',
      [Failures.Count, Probed.Count]);
    for Run in Failures do
      Msg := Msg + sLineBreak + '  - ' + Run.VersionLabel + ': ' + Run.Reason;
    Assert.Fail(Msg);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDcuMultiVersion);

end.
