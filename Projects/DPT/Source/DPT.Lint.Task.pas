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
  Slim.Server,
  DPT.Types,
  DPT.Lint.Context;

type

  TDptLintTask = class(TDptTaskBase)
  public
    const TEST_PAGE_NAME = 'DPT_LintTest';
  private
    FStyleFile: string;
    FTargetFile: string;
    FFitNesseDir: string;
    FFitNesseRoot: string;
    FVerbose: Boolean;
    function  ExtractTestFromStyle(const AStylePath: string): string;
    function  GetFreePort: Integer;
    procedure RunFitNesse(APort: Integer; const ATestContent: string);
  public
    procedure Execute; override;
    property StyleFile: string read FStyleFile write FStyleFile;
    property TargetFile: string read FTargetFile write FTargetFile;
    property FitNesseDir: string read FFitNesseDir write FFitNesseDir;
    property FitNesseRoot: string read FFitNesseRoot write FFitNesseRoot;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

uses
  IdTCPClient,
  System.SysUtils,
  System.Generics.Collections,
  JclSysUtils;

{ TDptLintTask }

procedure TDptLintTask.Execute;
var
  LTestContent: string;
  LPort: Integer;
  LSlimServer: TSlimServer;
  LFallbackStyleFile: string;
begin
  if not TFile.Exists(FStyleFile) then
  begin
    LFallbackStyleFile := TPath.Combine(TPath.Combine(ExtractFilePath(ParamStr(0)), 'Lint'), ExtractFileName(FStyleFile));
    if TFile.Exists(LFallbackStyleFile) then
      FStyleFile := LFallbackStyleFile
    else
      raise Exception.Create('Style template not found: ' + FStyleFile);
  end;

  if not TFile.Exists(FTargetFile) then
    raise Exception.Create('Target file not found: ' + FTargetFile);

  Writeln('Linting file: ' + FTargetFile + ' using style ' + ExtractFileName(FStyleFile));

  LTestContent := ExtractTestFromStyle(FStyleFile);
  LTestContent := LTestContent.Replace('${TargetFile}', FTargetFile);

  LPort := GetFreePort;
  LSlimServer := TSlimServer.Create(nil);
  try
    LSlimServer.DefaultPort := LPort;
    LSlimServer.Active := True;

    if FVerbose then
      Writeln('Internal Slim server started on port ' + LPort.ToString);

    // Run FitNesse (this blocks until FitNesse finishes)
    RunFitNesse(LPort, LTestContent);
  finally
    LSlimServer.Active := False;
    LSlimServer.Free;
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
  LAnchor := '// Start: AI-Generated FitNesse-Test';
  LAnchorPos := -1;
  try
    for I := 0 to High(LLines) do
    begin
      var Line := LLines[I];
      if LAnchorPos = -1 then
      begin
        LAnchorPos := Line.IndexOf(LAnchor);
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

procedure TDptLintTask.RunFitNesse(APort: Integer; const ATestContent: string);
var
  LTestFile: string;
  LFitNesseJar: string;
  LOutput: string;
  LTestPageDir: string;
  LLines: TArray<string>;
  LFilteredOutput: TStringBuilder;
  I: Integer;
begin
  LTestPageDir := TPath.Combine(FFitNesseRoot, TEST_PAGE_NAME);

  if not TDirectory.Exists(LTestPageDir) then
    TDirectory.CreateDirectory(LTestPageDir);

  LTestFile := TPath.Combine(LTestPageDir, 'content.txt');
  
  // Path to FitNesse JAR
  LFitNesseJar := TPath.Combine(FFitNesseDir, 'fitnesse-standalone.jar');

  if not TFile.Exists(LFitNesseJar) then
    raise Exception.Create('FitNesse JAR not found: ' + LFitNesseJar);

  // Write content.txt without BOM
  var LContentList: TStringList := TStringList.Create;
  try
    LContentList.WriteBOM := False;
    LContentList.Text := 
      '!define TEST_SYSTEM {slim}' + sLineBreak +
      '!define SLIM_PORT {' + APort.ToString + '}' + sLineBreak +
      sLineBreak +
      ATestContent;
    LContentList.SaveToFile(LTestFile, TEncoding.UTF8);
  finally
    LContentList.Free;
  end;

  if FVerbose then
    Writeln('Executing FitNesse tests against Slim server on port ' + APort.ToString + '...');

  // Reset error collector before run
  TDptLintContext.Clear;

  var LJavaCmd: string := Format('java -Dtest.system=slim -Dfitnesse.plugins=fitnesse.slim.SlimService -Dslim.port=%d -Dslim.pool.size=1 -jar "%s" -d "%s" -c "%s?test&format=text"', [APort, LFitNesseJar, FFitNesseDir, TEST_PAGE_NAME]);
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
    var LResultsDir := TPath.Combine(FFitNesseRoot, 'files\testResults\' + TEST_PAGE_NAME);
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
  
  // Print collected style violations at the very end
  if TDptLintContext.Violations.Count > 0 then
  begin
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
