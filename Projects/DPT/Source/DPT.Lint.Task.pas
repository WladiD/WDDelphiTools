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
  private
    FStyleFile: string;
    FTargetFile: string;
    FFitNesseDir: string;
    FFitNesseRoot: string;
    function  ExtractTestFromStyle(const AStylePath: string): string;
    function  GetFreePort: Integer;
    procedure RunFitNesse(APort: Integer; const ATestContent: string);
  public
    procedure Execute; override;
    property StyleFile: string read FStyleFile write FStyleFile;
    property TargetFile: string read FTargetFile write FTargetFile;
    property FitNesseDir: string read FFitNesseDir write FFitNesseDir;
    property FitNesseRoot: string read FFitNesseRoot write FFitNesseRoot;
  end;

implementation

uses
  IdTCPClient,
  System.SysUtils,
  JclSysUtils;

{ TDptLintTask }

procedure TDptLintTask.Execute;
var
  LTestContent: string;
  LPort: Integer;
  LSlimServer: TSlimServer;
begin
  if not TFile.Exists(FStyleFile) then
    raise Exception.Create('Style template not found: ' + FStyleFile);

  Writeln('Linting file: ' + FTargetFile + ' using style ' + ExtractFileName(FStyleFile));

  LTestContent := ExtractTestFromStyle(FStyleFile);
  LTestContent := LTestContent.Replace('${TargetFile}', FTargetFile);

  LPort := GetFreePort;
  LSlimServer := TSlimServer.Create(nil);
  try
    LSlimServer.DefaultPort := LPort;
    LSlimServer.Active := True;

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
begin
  LLines := TFile.ReadAllLines(AStylePath, TEncoding.UTF8);
  LBuilder := TStringBuilder.Create;
  LAnchor := '// Start: AI-Generated FitNesse-Test';
  LAnchorPos := -1;
  try
    for var Line in LLines do
    begin
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
begin
  LTestPageDir := TPath.Combine(FFitNesseRoot, 'DPT_LintTest');
  
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

  Writeln('Executing FitNesse tests against Slim server on port ' + APort.ToString + '...');

  // Reset error collector before run
  TDptLintContext.Clear;

  var LJavaCmd: string := Format('java -Dtest.system=slim -Dfitnesse.plugins=fitnesse.slim.SlimService -Dslim.port=%d -Dslim.pool.size=1 -jar "%s" -d "%s" -c "DPT_LintTest?test&format=text"', [APort, LFitNesseJar, FFitNesseDir]);
  var LRunResult := JclSysUtils.Execute(LJavaCmd, LOutput);
  
  Writeln(LOutput);
  
  // Print collected style violations at the very end
  if TDptLintContext.Violations.Count > 0 then
  begin
    Writeln;
    Writeln('Style Violations Summary:');
    Writeln('-------------------------');
    for var V in TDptLintContext.Violations do
      Writeln(Format('  %s(%d): %s', [V.FileSpec, V.Line, V.Message]));
    Writeln;
  end;

  if LRunResult = 0 then
  begin
    if LOutput.Contains('Assertions: 0 right') or LOutput.Contains('wrong') or LOutput.Contains('exceptions') then
    begin
      Writeln('Linting failed.');
      System.ExitCode := 1;
    end
    else
      Writeln('Linting passed.');
  end
  else
  begin
    raise Exception.Create('Failed to execute FitNesse batch (ExitCode: ' + LRunResult.ToString + ').');
  end;
end;

end.
