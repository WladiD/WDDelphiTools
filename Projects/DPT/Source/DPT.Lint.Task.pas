// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Lint.Task;

interface

uses

  System.Classes,
  System.Generics.Collections,
  System.IniFiles,
  System.IOUtils,
  System.SysUtils,

  IdTCPClient,
  JclSysUtils,
  mormot.core.collections,

  Slim.Server,

  DPT.Lint.Context,
  DPT.Lint.StyleValidator,
  DPT.Task,
  DPT.Types,
  DPT.Workflow;

type

  TDptLintTask = class(TDptTaskBase)
  public
    const SUITE_PAGE_NAME = 'DPT_LintSuite';
  private
    FFitNesseDir : String;
    FFitNesseRoot: String;
    FStyleFile   : String;
    FTargetFiles : IList<String>;
    FVerbose     : Boolean;
    function  ExtractTestFromStyle(const AStylePath: String): String;
    function  GetFreePort: Integer;
    function  GetLatestTestResultFile(const APageName: String): String;
    procedure RunFitNesse(APort: Integer; const ASuiteName: String);
    function  StyleViolationCompare(const A, B): Integer;
    procedure TryAddFailedTestFromLine(const ATrimmedLine: String; const AFailedTests: IList<String>);
  public
    constructor Create; override;
    procedure Execute; override;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    property FitNesseDir: String read FFitNesseDir write FFitNesseDir;
    property FitNesseRoot: String read FFitNesseRoot write FFitNesseRoot;
    property StyleFile: String read FStyleFile write FStyleFile;
    property TargetFiles: IList<String> read FTargetFiles;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

{ TDptLintTask }

constructor TDptLintTask.Create;
begin
  inherited;
  FTargetFiles := Collections.NewList<String>;
end;

procedure TDptLintTask.Parse(CmdLine: TCmdLineConsumer);
var
  FitNesseDir: String;
  Ini        : TIniFile;
  IniPath    : String;
  Param      : String;
begin
  FVerbose := False;
  FitNesseDir := '';
  FStyleFile := '';
  FTargetFiles.Clear;

  while CmdLine.HasParameter do
  begin
    Param := CmdLine.CheckParameter('Option/File');

    if SameText(Param, '--verbose') then
    begin
      FVerbose := True;
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--fitnesse-dir=', True) then
    begin
      FitNesseDir := Param.Substring(15).DeQuotedString('"');
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--') then
    begin
      CmdLine.InvalidParameter('Unknown option: ' + Param);
    end
    else
    begin
      // Positional arguments
      if FStyleFile = '' then
      begin
        FStyleFile := ExpandFileName(Param);
        CmdLine.ConsumeParameter;
      end
      else
      begin
        // Any subsequent parameter is treated as a target file
        FTargetFiles.Add(ExpandFileName(Param));
        CmdLine.ConsumeParameter;
      end;
    end;
  end;

  if FStyleFile = '' then
    CmdLine.InvalidParameter('Missing parameter: StyleFile');

  if FTargetFiles.Count = 0 then
    CmdLine.InvalidParameter('Missing parameter: TargetFile(s)');

  if FitNesseDir = '' then
  begin
    IniPath := TPath.Combine(ExtractFilePath(ParamStr(0)), DptConfigFileName);
    if not TFile.Exists(IniPath) then
      IniPath := FileSearch(DptConfigFileName, GetEnvironmentVariable('PATH'));

    if IniPath <> '' then
    begin
      Ini := TIniFile.Create(IniPath);
      try
        FitNesseDir := Ini.ReadString('FitNesse', 'Dir', '');
      finally
        Ini.Free;
      end;
    end;
  end;

  if FitNesseDir = '' then
    raise Exception.Create('FitNesse directory not configured.' + sLineBreak +
      'Please provide it via --fitnesse-dir="X:\Path" or create a ' + DptConfigFileName + ' in your PATH with:' + sLineBreak +
      '[FitNesse]' + sLineBreak +
      'Dir=C:\Path\To\FitNesse');

  FFitNesseDir := FitNesseDir;
  FFitNesseRoot := TPath.Combine(FitNesseDir, 'FitNesseRoot');
end;

procedure TDptLintTask.Execute;
var
  LTestContentTemplate: String;
  LPort               : Integer;
  LSlimServer         : TSlimServer;
  LFallbackStyleFile  : String;
  LSuiteDir           : String;
  LTestPageDir        : String;
  LTargetFile         : String;
  LSanitizedName      : String;
  LPageContent        : String;
  I                   : Integer;
  LEncodingNoBOM      : TEncoding;
begin
  // Fallback Logic for StyleFile
  if not TFile.Exists(FStyleFile) then
  begin
    LFallbackStyleFile := TPath.Combine(TPath.Combine(ExtractFilePath(ParamStr(0)), 'Lint'), ExtractFileName(FStyleFile));
    if TFile.Exists(LFallbackStyleFile) then
      FStyleFile := LFallbackStyleFile
    else
      raise Exception.Create('Style template not found: ' + FStyleFile);
  end;

  if FTargetFiles.Count = 0 then
    raise Exception.Create('No target files specified.');

  for LTargetFile in FTargetFiles do
  begin
    if not TFile.Exists(LTargetFile) then
      raise Exception.Create('Target file not found: ' + LTargetFile);
  end;

  TDptLintStyleValidator.ValidateStyleFile(FStyleFile);

  LTestContentTemplate := ExtractTestFromStyle(FStyleFile);

  // Setup Suite Directory
  LSuiteDir := TPath.Combine(FFitNesseRoot, SUITE_PAGE_NAME);
  if TDirectory.Exists(LSuiteDir) then
    TDirectory.Delete(LSuiteDir, True);
  TDirectory.CreateDirectory(LSuiteDir);

  Writeln(Format('Linting %d files using style %s...', [FTargetFiles.Count, ExtractFileName(FStyleFile)]));

  LEncodingNoBOM := TUTF8Encoding.Create(False);
  try
    for I := 0 to FTargetFiles.Count - 1 do
    begin
      LTargetFile := FTargetFiles[I];
      // Sanitize filename for FitNesse page name
      LSanitizedName := ExtractFileName(LTargetFile).Replace('.', '_').Replace(' ', '_');
      LTestPageDir := TPath.Combine(LSuiteDir, 'Test_' + (I + 1).ToString + '_' + LSanitizedName);
      TDirectory.CreateDirectory(LTestPageDir);

      // Create test content
      LPageContent := LTestContentTemplate.Replace('${TargetFile}', LTargetFile);
      TFile.WriteAllText(TPath.Combine(LTestPageDir, 'content.txt'), LPageContent, LEncodingNoBOM);
    end;

    LPort := GetFreePort;
    LSlimServer := TSlimServer.Create(nil);
    try
      LSlimServer.DefaultPort := LPort;
      LSlimServer.Active := True;

      if FVerbose then
        Writeln('Internal Slim server started on port ' + LPort.ToString);

      // Write Suite content.txt with definitions
      // Note: !path is usually handled by Slim server classpath, but here we run in-process logic (SlimServer is inside DPT).
      // We just need to define TEST_SYSTEM and SLIM_PORT.
      TFile.WriteAllText(TPath.Combine(LSuiteDir, 'content.txt'),
        '!contents -R2 -g -p -f -h' + sLineBreak +
        '!define TEST_SYSTEM {slim}' + sLineBreak +
        '!define SLIM_PORT {' + LPort.ToString + '}',
        LEncodingNoBOM);

      RunFitNesse(LPort, SUITE_PAGE_NAME);
    finally
      LSlimServer.Active := False;
      LSlimServer.Free;
    end;
  finally
    LEncodingNoBOM.Free;
  end;
end;

function TDptLintTask.ExtractTestFromStyle(const AStylePath: String): String;
var
  Anchor   : String;
  AnchorPos: Integer;
  Builder  : TStringBuilder;
  I        : Integer;
  Lines    : TArray<String>;
  PipePos  : Integer;
begin
  Lines := TFile.ReadAllLines(AStylePath, TEncoding.UTF8);
  Builder := TStringBuilder.Create;
  Anchor := '// START: AI-GENERATED FITNESSE-TEST';
  AnchorPos := -1;
  try
    for I := 0 to High(Lines) do
    begin
      var Line := Lines[I];
      if AnchorPos = -1 then
      begin
        AnchorPos := Line.ToUpper.IndexOf(Anchor);
        Continue;
      end;

      if (AnchorPos >= 0) and (Line.Length > AnchorPos) then
      begin
        var LSegment := Line.Substring(AnchorPos);
        PipePos := LSegment.IndexOf('|');
        if PipePos >= 0 then
        begin
          var LTestLine := LSegment.Substring(PipePos).Trim;
          // Prepend newline before new script tables (except if it's the very first line)
          if LTestLine.StartsWith('|script|', True) and (Builder.Length > 0) then
            Builder.AppendLine;

          Builder.AppendLine(LTestLine);
        end;
      end;
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TDptLintTask.GetFreePort: Integer;
var
  Client: TIdTCPClient;
begin
  Result := 9001;
  Client := TIdTCPClient.Create(nil);
  try
    while Result < 9100 do
    begin
      try
        Client.Port := Result;
        Client.Host := '127.0.0.1';
        Client.ConnectTimeout := 100;
        Client.Connect;
        Client.Disconnect;
        Inc(Result);
      except
        Exit; // Port is free
      end;
    end;
  finally
    Client.Free;
  end;
end;

procedure TDptLintTask.RunFitNesse(APort: Integer; const ASuiteName: String);
var
  FailedTests   : IList<String>;
  FilteredOutput: TStringBuilder;
  FitNesseJar   : String;
  I             : Integer;
  Lines         : TArray<String>;
  Output        : String;
begin
  // Path to FitNesse JAR
  FitNesseJar := TPath.Combine(FFitNesseDir, 'fitnesse-standalone.jar');

  if not TFile.Exists(FitNesseJar) then
    raise Exception.Create('FitNesse JAR not found: ' + FitNesseJar);

  if FVerbose then
    Writeln('Executing FitNesse suite "' + ASuiteName + '" against Slim server on port ' + APort.ToString + '...');

  // Reset error collector before run
  TDptLintContext.Clear;

  var LJavaCmd: String := Format('java -Dtest.system=slim -Dslim.port=%d -Dslim.pool.size=1 -jar "%s" -d "%s" -c "%s?suite&format=text"', [APort, FitNesseJar, FFitNesseDir, ASuiteName]);
  var LRunResult: Cardinal := JclSysUtils.Execute(LJavaCmd, Output);

  FailedTests := Collections.NewList<String>;
  if FVerbose then
    Writeln(Output)
  else
  begin
    // Filter output: only show relevant test result lines
    Lines := Output.Split([sLineBreak]);
    FilteredOutput := TStringBuilder.Create;
    try
      for I := 0 to High(Lines) do
      begin
        var LLine: String := Lines[I];
        var LTrimmed: String := LLine.Trim;

        var LIsTestResultFailLine: Boolean :=
            LTrimmed.StartsWith('F ') or
            LTrimmed.StartsWith('E ');

        var LIsTestResultLine: Boolean :=
          (
            LIsTestResultFailLine or
            LTrimmed.StartsWith('. ')
          ) and
          LTrimmed.Contains(' R:') and
          LTrimmed.Contains('W:') and
          LTrimmed.Contains('I:') and
          LTrimmed.Contains('E:');

        if LTrimmed.StartsWith('Executing command:') or
           LTrimmed.StartsWith('Starting Test System:') or
           LIsTestResultLine or
           LTrimmed.Contains('[Fail]') or
           LTrimmed.Contains('[Error]') or
           LTrimmed.StartsWith('--------') or
           LTrimmed.Contains('Tests,') or
           LTrimmed.Contains('Failures') then
          FilteredOutput.AppendLine(LLine);

        if LIsTestResultFailLine then
          TryAddFailedTestFromLine(LTrimmed, FailedTests);
      end;
      Writeln(FilteredOutput.ToString.Trim);
    finally
      FilteredOutput.Free;
    end;
  end;

  if LRunResult <> 0 then
  begin
    Writeln;
    if FailedTests.Count > 0 then
    begin
      Writeln('Error details:');
      for var LTestName in FailedTests do
      begin
        var LLatestFile := GetLatestTestResultFile(LTestName);
        if LLatestFile <> '' then
          Writeln('  ' + LLatestFile)
        else
          Writeln('  [No XML found for ' + LTestName + ']');
      end;
    end
    else
    begin
      // Fallback: show suite result if no individual failed tests identified
      var LLatestFile := GetLatestTestResultFile(ASuiteName);
      if LLatestFile <> '' then
        Writeln('Error details (Suite): ' + LLatestFile);
    end;
  end;

  // Print collected warnings
  if TDptLintContext.Warnings.Count > 0 then
  begin
    Writeln;
    Writeln('Linting Warnings:');
    Writeln('-----------------');
    for var W in TDptLintContext.Warnings do
      Writeln('  ' + W);
  end;

  // Print collected style violations at the very end, sorted
  if TDptLintContext.Violations.Count > 0 then
  begin
    TDptLintContext.Violations.Sort(StyleViolationCompare);

    Writeln;
    Writeln('Style Violations Summary:');
    Writeln('-------------------------');
    for var V in TDptLintContext.Violations do
      Writeln(Format('  %s(%d): %s', [V.FileSpec, V.Line, V.Message]));
  end;

  Writeln;
  if (LRunResult = 0) and (TDptLintContext.Violations.Count = 0) then
  begin
    Writeln('Linting passed.');
    if Assigned(WorkflowEngine) then
    begin
      for var LFile in FTargetFiles do
        TDptWorkflowEngine(WorkflowEngine).ReportLintResult(LFile, True);
    end;
  end
  else
  begin
    Writeln('Linting failed.');
    System.ExitCode := 1;
  end;
end;

function TDptLintTask.StyleViolationCompare(const A, B): Integer;
var
  AStyle: TStyleViolation absolute A;
  BStyle: TStyleViolation absolute B;
begin
  Result := CompareText(AStyle.FileSpec, BStyle.FileSpec);
  if Result = 0 then
    Result := AStyle.Line - BStyle.Line;
end;

procedure TDptLintTask.TryAddFailedTestFromLine(const ATrimmedLine: String; const AFailedTests: IList<String>);
begin
  // Identify failed tests for detail reporting
  // Format is typically: "F 07:55:05 R:21 W:3 I:0 E:0 TestName (Full.Path.To.Test) 0,325 seconds"
  var OpenParen: Integer := ATrimmedLine.IndexOf('(');
  var CloseParen: Integer := ATrimmedLine.IndexOf(')', OpenParen);

  if (OpenParen > 0) and (CloseParen > OpenParen) then
  begin
    var TestPath: String := ATrimmedLine.Substring(OpenParen + 1, CloseParen - OpenParen - 1).Trim;
    if (TestPath <> '') and (AFailedTests.IndexOf(TestPath) < 0) then
      AFailedTests.Add(TestPath);
  end
  else
  begin
    var Parts: TArray<String> := ATrimmedLine.Split([' ', #9]);
    if Length(Parts) > 0 then
    begin
      // Try to find the first part that looks like a name (not a status or time)
      for var LPart in Parts do
      begin
        var LP: String := LPart.Trim(['.']);
        if (LP = '') or
           (LP = 'F') or
           (LP = 'E') or
           (Pos(':', LP) > 0) or
           (LP.StartsWith('R:')) then
          Continue;
        if (AFailedTests.IndexOf(LP) < 0) then
          AFailedTests.Add(LP);
        Break;
      end;
    end;
  end;
end;

function TDptLintTask.GetLatestTestResultFile(const APageName: String): String;
var
  Files     : TArray<String>;
  ResultsDir: String;
begin
  Result := '';
  ResultsDir := TPath.Combine(FFitNesseRoot, 'files\testResults\' + APageName);
  if TDirectory.Exists(ResultsDir) then
  begin
    Files := TDirectory.GetFiles(ResultsDir, '*.xml');
    if Length(Files) > 0 then
    begin
      TArray.Sort<String>(Files);
      Result := Files[High(Files)];
    end;
  end;
end;

end.
