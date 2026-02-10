// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Lint.Task;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  Slim.Server,
  DPT.Types,
  DPT.Lint.Context;

type

  TDptLintTask = class(TDptTaskBase)
  public
    const SUITE_PAGE_NAME = 'DPT_LintSuite';
  private
    FStyleFile: string;
    FTargetFiles: TStrings;
    FFitNesseDir: string;
    FFitNesseRoot: string;
    FVerbose: Boolean;
    function  ExtractTestFromStyle(const AStylePath: string): string;
    function  GetFreePort: Integer;
    procedure RunFitNesse(APort: Integer; const ASuiteName: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute; override;
    property StyleFile: string read FStyleFile write FStyleFile;
    property TargetFiles: TStrings read FTargetFiles;
    property FitNesseDir: string read FFitNesseDir write FFitNesseDir;
    property FitNesseRoot: string read FFitNesseRoot write FFitNesseRoot;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

uses
  IdTCPClient,
  System.Generics.Collections,
  System.Generics.Defaults,
  JclSysUtils,
  DPT.Lint.StyleValidator;

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

procedure TDptLintTask.Execute;
var
  LTestContentTemplate: string;
  LPort: Integer;
  LSlimServer: TSlimServer;
  LFallbackStyleFile: string;
  LSuiteDir: string;
  LTestPageDir: string;
  LTargetFile: string;
  LSanitizedName: string;
  LPageContent: string;
  I: Integer;
  LEncodingNoBOM: TEncoding;
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

function TDptLintTask.ExtractTestFromStyle(const AStylePath: string): string;
var
  LLines: TArray<string>;
  LBuilder: TStringBuilder;
  LAnchor: string;
  LAnchorPos: Integer;
  LPipePos: Integer;
  I: Integer;
begin
  LLines := TFile.ReadAllLines(AStylePath, TEncoding.UTF8);
  LBuilder := TStringBuilder.Create;
  LAnchor := '// START: AI-GENERATED FITNESSE-TEST';
  LAnchorPos := -1;
  try
    for I := 0 to High(LLines) do
    begin
      var Line := LLines[I];
      if LAnchorPos = -1 then
      begin
        LAnchorPos := Line.ToUpper.IndexOf(LAnchor);
        Continue;
      end;

      if (LAnchorPos >= 0) and (Line.Length > LAnchorPos) then
      begin
        var LSegment := Line.Substring(LAnchorPos);
        LPipePos := LSegment.IndexOf('|');
        if LPipePos >= 0 then
        begin
          var LTestLine := LSegment.Substring(LPipePos).Trim;
          // Prepend newline before new script tables (except if it's the very first line)
          if LTestLine.StartsWith('|script|', True) and (LBuilder.Length > 0) then
            LBuilder.AppendLine;

          LBuilder.AppendLine(LTestLine);
        end;
      end;
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
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

procedure TDptLintTask.RunFitNesse(APort: Integer; const ASuiteName: string);
var
  LFitNesseJar: string;
  LOutput: string;
  LLines: TArray<string>;
  LFilteredOutput: TStringBuilder;
  I: Integer;
begin
  // Path to FitNesse JAR
  LFitNesseJar := TPath.Combine(FFitNesseDir, 'fitnesse-standalone.jar');

  if not TFile.Exists(LFitNesseJar) then
    raise Exception.Create('FitNesse JAR not found: ' + LFitNesseJar);

  if FVerbose then
    Writeln('Executing FitNesse suite "' + ASuiteName + '" against Slim server on port ' + APort.ToString + '...');

  // Reset error collector before run
  TDptLintContext.Clear;

  var LJavaCmd: string := Format('java -Dtest.system=slim -Dfitnesse.plugins=fitnesse.slim.SlimService -Dslim.port=%d -Dslim.pool.size=1 -jar "%s" -d "%s" -c "%s?suite&format=text"', [APort, LFitNesseJar, FFitNesseDir, ASuiteName]);
  var LRunResult := JclSysUtils.Execute(LJavaCmd, LOutput);

  if FVerbose then
    Writeln(LOutput)
  else
  begin
    // Filter output: only show relevant test result lines
    LLines := LOutput.Split([sLineBreak]);
    LFilteredOutput := TStringBuilder.Create;
    try
      for I := 0 to High(LLines) do
      begin
        var LLine := LLines[I];
        var LTrimmed := LLine.Trim;
        if LTrimmed.StartsWith('Executing command:') or
           LTrimmed.StartsWith('Starting Test System:') or
           LTrimmed.Contains(' R:') or
           LTrimmed.StartsWith('--------') or
           LTrimmed.Contains('Tests,') or
           LTrimmed.Contains('Failures') then
        begin
          LFilteredOutput.AppendLine(LLine);
        end;
      end;
      Writeln(LFilteredOutput.ToString.Trim);
    finally
      LFilteredOutput.Free;
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
        TArray.Sort<string>(LFiles);
        LLatestFile := LFiles[High(LFiles)];
      end;
    end;

    if LLatestFile <> '' then
    begin
      Writeln;
      Writeln('Error details: ' + LLatestFile);
    end;
  end;

  // Print collected style violations at the very end, sorted
  if TDptLintContext.Violations.Count > 0 then
  begin
    TDptLintContext.Violations.Sort(TComparer<TStyleViolation>.Construct(
      function(const Left, Right: TStyleViolation): Integer
      begin
        Result := CompareText(Left.FileSpec, Right.FileSpec);
        if Result = 0 then
          Result := Left.Line - Right.Line;
      end));

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

end.
