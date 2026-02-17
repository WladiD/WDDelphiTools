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

  Slim.Server,

  DPT.Lint.Context,
  DPT.Lint.StyleValidator,
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
    FTargetFiles : TStrings;
    FVerbose     : Boolean;
    function  ExtractTestFromStyle(const AStylePath: String): String;
    function  GetFreePort: Integer;
    procedure RunFitNesse(APort: Integer; const ASuiteName: String);
    function  StyleViolationCompare(const A, B): Integer;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure Execute; override;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    property FitNesseDir: String read FFitNesseDir write FFitNesseDir;
    property FitNesseRoot: String read FFitNesseRoot write FFitNesseRoot;
    property StyleFile: String read FStyleFile write FStyleFile;
    property TargetFiles: TStrings read FTargetFiles;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

{ TDptLintTask }

constructor TDptLintTask.Create;
begin
  inherited;
  FTargetFiles := TStringList.Create;
end;

destructor TDptLintTask.Destroy;
begin
  FTargetFiles.Free;
  inherited;
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
  LClient: TIdTCPClient;
begin
  Result := 9001;
  LClient := TIdTCPClient.Create(nil);
  try
    while Result < 9100 do
    begin
      try
        LClient.Port := Result;
        LClient.Host := '127.0.0.1';
        LClient.ConnectTimeout := 100;
        LClient.Connect;
        LClient.Disconnect;
        Inc(Result);
      except
        Exit; // Port is free
      end;
    end;
  finally
    LClient.Free;
  end;
end;

procedure TDptLintTask.RunFitNesse(APort: Integer; const ASuiteName: String);
var
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

  var LJavaCmd: String := Format('java -Dtest.system=slim -Dfitnesse.plugins=fitnesse.slim.SlimService -Dslim.port=%d -Dslim.pool.size=1 -jar "%s" -d "%s" -c "%s?suite&format=text"', [APort, FitNesseJar, FFitNesseDir, ASuiteName]);
  var LRunResult := JclSysUtils.Execute(LJavaCmd, Output);

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
        var LLine := Lines[I];
        var LTrimmed := LLine.Trim;
        if LTrimmed.StartsWith('Executing command:') or
           LTrimmed.StartsWith('Starting Test System:') or
           LTrimmed.Contains(' R:') or
           LTrimmed.StartsWith('--------') or
           LTrimmed.Contains('Tests,') or
           LTrimmed.Contains('Failures') then
        begin
          FilteredOutput.AppendLine(LLine);
        end;
      end;
      Writeln(FilteredOutput.ToString.Trim);
    finally
      FilteredOutput.Free;
    end;
  end;

  if LRunResult <> 0 then
  begin
    var LResultsDir := TPath.Combine(FFitNesseRoot, 'files\testResults\' + ASuiteName);
    var LLatestFile := '';
    if TDirectory.Exists(LResultsDir) then
    begin
      var LFiles := TDirectory.GetFiles(LResultsDir, '*.xml');
      if Length(LFiles) > 0 then
      begin
        TArray.Sort<String>(LFiles);
        LLatestFile := LFiles[High(LFiles)];
      end;
    end;

    if LLatestFile <> '' then
    begin
      Writeln;
      Writeln('Error details: ' + LLatestFile);
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
  if LRunResult = 0 then
  begin
    Writeln('Linting passed.');
    if Assigned(WorkflowEngine) then
    begin
      for var LFile in FTargetFiles do
        TDptWorkflowEngine(WorkflowEngine).ReportLintResult(LFile, True);
    end;
  end
  else if LRunResult = 1 then
  begin
    Writeln('Linting failed.');
    System.ExitCode := 1;
  end
  else
  begin
    raise Exception.Create('Failed to execute FitNesse batch (ExitCode: ' + LRunResult.ToString + ').');
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

end.
