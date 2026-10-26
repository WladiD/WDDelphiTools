// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Workflow;

interface

uses

  Winapi.Windows,

  System.Classes,
  System.IOUtils,
  System.StrUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  DPT.Git,
  DPT.Types,
  DPT.Workflow;

type

  [TestFixture]
  TTestDptWorkflow = class
  private
    FTestDir: String;
    FWorkflowFile: String;
    FEngine: TDptWorkflowEngine;
    FOldGeminiVar: String;
    FDiagLines: TStringList;
    FGitDirs: TStringList;
    function CreateCheckout(const AName: String): String;
    function CreateTestFile(const AName, AContent: String; AEncoding: TEncoding = nil): String;
    function HasUtf8Bom(const AFileName: String): Boolean;
    function MockGitRunCommand(const ACommand, ADirectory: String; out AOutput: String): Integer;
    function CountDiag(const ASubstr: String): Integer;
    function WriteFile(const AFileName, AContent: String): String;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure FixLineEndings;
    [Test]
    procedure FixUtf8Bom;
    [Test]
    procedure FixBothLineEndingsAndBom;
    [Test]
    procedure TestFormatterAndIgnore;
    [Test]
    procedure FixWithIgnorePatternAndClear;
    [Test]
    procedure IncludeAndIgnorePatterns;
    [Test]
    procedure TestComplexWorkflow_IncludeIgnoreAndFormat;
    [Test]
    procedure ProcessInstructions_PreservesTextWithParentheses;
    [Test]
    procedure DProjPrintOutputFile;
    // Pins the lazy, once-only "Workflow file FOUND" announcement
    // (moved out of the constructor into CheckConditions).
    [Test]
    procedure WorkflowFoundNotAnnouncedAtConstruction;
    [Test]
    procedure WorkflowFoundAnnouncedOnceWhenProcessed;
    [Test]
    procedure WorkflowFoundNotAnnouncedWhenNoBlocksToProcess;
    // A build started from one checkout for a project of another checkout
    // (an AI agent in the main checkout building a git worktree project):
    // the workflow file, the git-modified files and the relative paths of the
    // workflow must follow the engine's base directory, not the current dir.
    [Test]
    procedure BaseDir_DefaultsToCurrentDir;
    [Test]
    procedure WorkflowFile_SearchedUpFromBaseDir_NotFromCurrentDir;
    [Test]
    procedure FixFunctions_ProcessBaseDirCheckout_NotCurrentDirCheckout;
    [Test]
    procedure FormatGitModifiedFiles_FormatsBaseDirCheckoutWithItsOwnScript;
    [Test]
    procedure DProjPrintOutputFile_ResolvedRelativeToWorkflowFile;
    [Test]
    procedure ProcessedFiles_ReportedWithFullPath;
  end;

implementation

function TTestDptWorkflow.CreateCheckout(const AName: String): String;
begin
  Result := TPath.Combine(FTestDir, AName);
  // An (empty) .git directory marks the checkout root for MockGitRunCommand
  TDirectory.CreateDirectory(TPath.Combine(Result, '.git'));
end;

function TTestDptWorkflow.CreateTestFile(const AName, AContent: String; AEncoding: TEncoding = nil): String;
begin
  Result := TPath.Combine(FTestDir, AName);
  if AEncoding = nil then
    TFile.WriteAllText(Result, AContent)
  else
    TFile.WriteAllText(Result, AContent, AEncoding);
end;

function TTestDptWorkflow.HasUtf8Bom(const AFileName: String): Boolean;
var
  Bytes: TBytes;
begin
  Bytes := TFile.ReadAllBytes(AFileName);
  Result := (Length(Bytes) >= 3) and (Bytes[0] = $EF) and (Bytes[1] = $BB) and (Bytes[2] = $BF);
end;

/// <summary>
///   Answers git like a real checkout would: <c>rev-parse --show-toplevel</c>
///   yields the nearest directory above <paramref name="ADirectory"/> that
///   holds a <c>.git</c> marker, <c>status</c> lists every file below that root
///   as modified. Records each directory git is run in.
/// </summary>
function TTestDptWorkflow.MockGitRunCommand(const ACommand, ADirectory: String; out AOutput: String): Integer;
var
  Files: TArray<string>;
  I: Integer;
  Root: String;
begin
  FGitDirs.Add(ADirectory);
  AOutput := '';

  Root := ExcludeTrailingPathDelimiter(ADirectory);
  while not TDirectory.Exists(TPath.Combine(Root, '.git')) do
  begin
    if (TPath.GetDirectoryName(Root) = '') or SameText(TPath.GetDirectoryName(Root), Root) then
      Exit(128); // fatal: not a git repository
    Root := TPath.GetDirectoryName(Root);
  end;

  Result := 0;
  if Pos('rev-parse', ACommand) > 0 then
  begin
    AOutput := Root;
  end
  else if Pos('status', ACommand) > 0 then
  begin
    Files := TDirectory.GetFiles(Root, '*.*', TSearchOption.soAllDirectories);
    for I := 0 to High(Files) do
    begin
      AOutput := AOutput + ' M ' + ExtractRelativePath(Root + '\', Files[I]).Replace('\', '/') + sLineBreak;
    end;
  end;
end;

function TTestDptWorkflow.WriteFile(const AFileName, AContent: String): String;
begin
  Result := AFileName;
  TDirectory.CreateDirectory(ExtractFilePath(AFileName));
  // ASCII: neither a BOM nor a re-encoding of the content
  TFile.WriteAllText(AFileName, AContent, TEncoding.ASCII);
end;

procedure TTestDptWorkflow.Setup;
begin
  FTestDir := TPath.Combine(TPath.GetTempPath, 'DptWorkflowTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
  // The test directory itself is a checkout for MockGitRunCommand
  TDirectory.CreateDirectory(TPath.Combine(FTestDir, '.git'));
  FWorkflowFile := TPath.Combine(FTestDir, '.DptAiWorkflow');
  TFile.WriteAllText(FWorkflowFile, ''); // Empty workflow file needed for discovery

  FGitDirs := TStringList.Create;
  TDptGit.MockRunCommand := MockGitRunCommand;

  // Force AI Mode via Environment Variable
  FOldGeminiVar := GetEnvironmentVariable('GEMINI_CLI');
  SetEnvironmentVariable('GEMINI_CLI', '1');
  
  // We need to set the current dir to test dir so it finds the workflow file
  SetCurrentDir(FTestDir);

  // Capture the engine's diagnostics ("Workflow file FOUND/NOT found")
  // instead of letting them hit stdout. Installed BEFORE the engine is
  // created so a construction-time emission would be recorded too.
  FDiagLines := TStringList.Create;
  TDptWorkflowEngine.DiagnosticHook :=
    procedure(const AText: String)
    begin
      FDiagLines.Add(AText);
    end;

  FEngine := TDptWorkflowEngine.Create('Test');
end;

procedure TTestDptWorkflow.TearDown;
begin
  TDptGit.MockRunCommand := nil;
  TDptWorkflowEngine.DiagnosticHook := nil;
  FEngine.Free;
  FreeAndNil(FDiagLines);
  FreeAndNil(FGitDirs);
  if FOldGeminiVar <> '' then
    SetEnvironmentVariable('GEMINI_CLI', PChar(FOldGeminiVar))
  else
    SetEnvironmentVariable('GEMINI_CLI', nil);

  // Leave the test directory, Windows refuses to delete the current directory
  SetCurrentDir(TPath.GetTempPath);
  if TDirectory.Exists(FTestDir) then
    TDirectory.Delete(FTestDir, True);
end;

procedure TTestDptWorkflow.FixLineEndings;
var
  FileMixed: String;
  FileUnix: String;
  FileWin: String;
  Instructions: String;
begin
  // 1. Create files with different line endings
  FileMixed := CreateTestFile('Mixed.txt', 'Line1'#13#10'Line2'#10'Line3');
  FileUnix := CreateTestFile('Unix.txt', 'Line1'#10'Line2'#10'Line3');
  FileWin := CreateTestFile('Windows.txt', 'Line1'#13#10'Line2'#13#10'Line3');

  // 2. Inject a workflow block that calls the fix function
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      BeforeDptGuard: FixLineEndingsWindowsInGitModifiedFiles()
      {
        Fixed: `GetLastFixedFiles()`
      }
    }
    ''');
  
  // Reload workflow
  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  // 4. Run CheckConditions
  FEngine.CheckConditions(Instructions);

  // 5. Verify Content
  Assert.AreEqual('Line1'#13#10'Line2'#13#10'Line3', TFile.ReadAllText(FileMixed), 'Mixed should be CRLF');
  Assert.AreEqual('Line1'#13#10'Line2'#13#10'Line3', TFile.ReadAllText(FileUnix), 'Unix should be CRLF');
  Assert.AreEqual('Line1'#13#10'Line2'#13#10'Line3', TFile.ReadAllText(FileWin), 'Windows should stay CRLF');

  // 6. Verify Output (GetLastFixedFiles)
  Assert.IsTrue(Instructions.Contains('Mixed.txt'), 'Output should contain Mixed.txt');
  Assert.IsTrue(Instructions.Contains('Unix.txt'), 'Output should contain Unix.txt');
  Assert.IsFalse(Instructions.Contains('Windows.txt'), 'Output should NOT contain Windows.txt');
end;

procedure TTestDptWorkflow.FixUtf8Bom;
var
  FileNoBom: String;
  FileWithBom: String;
  Instructions: String;
  Bytes: TBytes;
  BOM: TBytes;
begin
  BOM := TEncoding.UTF8.GetPreamble;

  // 1. Create files
  FileNoBom := TPath.Combine(FTestDir, 'NoBom.txt');
  TFile.WriteAllText(FileNoBom, 'Content', TEncoding.ASCII); 
  
  FileWithBom := CreateTestFile('WithBom.txt', 'Content', TEncoding.UTF8);

  // 2. Workflow
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      BeforeDptGuard: FixUtf8BomInGitModifiedFiles()
      {
        Fixed: `GetLastFixedFiles()`
      }
    }
    ''');

  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  // 3. Run
  FEngine.CheckConditions(Instructions);

  // 4. Verify Content
  Bytes := TFile.ReadAllBytes(FileNoBom);
  
  Assert.IsTrue((Length(Bytes) >= 3) and 
                (Bytes[0] = BOM[0]) and 
                (Bytes[1] = BOM[1]) and 
                (Bytes[2] = BOM[2]), 'NoBom should now have BOM');

  // 5. Verify Output
  Assert.IsTrue(Instructions.Contains('NoBom.txt'), 'Output should contain NoBom.txt');
  Assert.IsFalse(Instructions.Contains('WithBom.txt'), 'Output should NOT contain WithBom.txt');
end;

procedure TTestDptWorkflow.FixBothLineEndingsAndBom;
var
  FileBadLines: String;
  FileBadBom: String;
  FileBadBoth: String;
  Instructions: String;
  Bytes: TBytes;
  BOM: TBytes;
begin
  BOM := TEncoding.UTF8.GetPreamble;

  // 1. File 1: Bad Line Endings (LF), but WITH BOM
  FileBadLines := TPath.Combine(FTestDir, 'BadLines.txt');
  TFile.WriteAllText(FileBadLines, 'Line1'#10'Line2', TEncoding.UTF8); // Writes with BOM

  // 2. File 2: Bad BOM (No BOM), but correct CRLF (or no line breaks to avoid AdjustLineBreaks trigger)
  FileBadBom := TPath.Combine(FTestDir, 'BadBom.txt');
  TFile.WriteAllText(FileBadBom, 'Line1', TEncoding.ASCII); // Writes without BOM

  // 3. File 3: Bad Line Endings (LF) AND Bad BOM (No BOM)
  FileBadBoth := TPath.Combine(FTestDir, 'BadBoth.txt');
  TFile.WriteAllText(FileBadBoth, 'Line1'#10'Line2', TEncoding.ASCII); // Writes without BOM

  // 5. Workflow
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      BeforeDptGuard: FixLineEndingsWindowsInGitModifiedFiles()
      {
        - In folgenden Dateien wurden die Lineendings auf CRLF (Windows) gefixt:
          - `GetLastFixedFiles()`
      }

      BeforeDptGuard: FixUtf8BomInGitModifiedFiles()
      {
        - In folgenden Dateien wurde die fehlende UTF-8-BOM gefixt:
          - `GetLastFixedFiles()`
      }
    }
    ''');

  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  // 6. Run
  FEngine.CheckConditions(Instructions);

  // 7. Verify Content
  
  // FileBadLines should now be CRLF (and still BOM)
  var ContentLines := TFile.ReadAllText(FileBadLines, TEncoding.UTF8);
  Assert.AreEqual('Line1'#13#10'Line2', ContentLines, 'BadLines should be CRLF');

  // FileBadBom should now have BOM (and still same content)
  Bytes := TFile.ReadAllBytes(FileBadBom);
  Assert.IsTrue((Length(Bytes) >= 3) and 
                (Bytes[0] = BOM[0]) and 
                (Bytes[1] = BOM[1]) and 
                (Bytes[2] = BOM[2]), 'BadBom should have BOM');
  
  var ContentBom := TFile.ReadAllText(FileBadBom, TEncoding.UTF8);
  Assert.AreEqual('Line1', ContentBom, 'BadBom content should match');

  // FileBadBoth should now be CRLF AND BOM
  Bytes := TFile.ReadAllBytes(FileBadBoth);
  Assert.IsTrue((Length(Bytes) >= 3) and 
                (Bytes[0] = BOM[0]) and 
                (Bytes[1] = BOM[1]) and 
                (Bytes[2] = BOM[2]), 'BadBoth should have BOM');
  var ContentBoth := TFile.ReadAllText(FileBadBoth, TEncoding.UTF8);
  Assert.AreEqual('Line1'#13#10'Line2', ContentBoth, 'BadBoth should be CRLF');

  // 8. Verify Output
  var PosCrlf := Instructions.IndexOf('In folgenden Dateien wurden die Lineendings');
  var PosBom  := Instructions.IndexOf('In folgenden Dateien wurde die fehlende UTF-8-BOM');
  var PosBadLines := Instructions.IndexOf('BadLines.txt');
  var PosBadBom   := Instructions.IndexOf('BadBom.txt');
  var PosBadBoth  := Instructions.IndexOf('BadBoth.txt');

  Assert.IsTrue(PosCrlf >= 0, 'CRLF header missing. Actual: ' + Instructions);
  Assert.IsTrue(PosBom >= 0, 'BOM header missing');
  
  // BadLines: CRLF list
  Assert.IsTrue(PosBadLines > PosCrlf, 'BadLines should be after CRLF header');
  Assert.IsTrue(PosBadLines < PosBom, 'BadLines should be before BOM header');

  // BadBoth: CRLF list (because FixLineEndings handles it first and adds BOM)
  Assert.IsTrue(PosBadBoth > PosCrlf, 'BadBoth should be after CRLF header');
  Assert.IsTrue(PosBadBoth < PosBom, 'BadBoth should be before BOM header');

  // BadBom: BOM list
  Assert.IsTrue(PosBadBom > PosBom, 'BadBom should be after BOM header');
end;

procedure TTestDptWorkflow.TestFormatterAndIgnore;
var
  FileToFormat: String;
  FileToIgnore: String;
  ScriptFile: String;
  Instructions: String;
begin
  // 1. Create a minimal DWS script that does something visible
  // We'll just use a simple script that can be compiled.
  // To actually "format", we'd need to use the API exposed by TDptDwsFormatter.
  ScriptFile := TPath.Combine(FTestDir, 'TestFormat.pas');
  TFile.WriteAllText(ScriptFile, '''
    procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax);
    begin
      AddLeadingTrivia(GetUnitKeyword(AUnit), "{ Formatted } ");
    end;
    ''');

  // 2. Create files
  FileToFormat := CreateTestFile('ToFormat.pas', 'unit ToFormat; implementation end.');
  FileToIgnore := CreateTestFile('ToIgnore.pas', 'unit ToIgnore; implementation end.');
  var FileToIgnore2 := CreateTestFile('AlsoIgnore.pas', 'unit AlsoIgnore; implementation end.');

  // 3. Workflow: Ignore ToIgnore.pas and AlsoIgnore.pas and run formatter with our test script
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      BeforeDptGuard: IgnorePattern("ToIgnore.pas", "AlsoIgnore.pas")
      BeforeDptGuard: FormatGitModifiedFiles("TestFormat.pas")
      {
        Formatted: `GetLastFormattedFiles()`
      }
    }
    ''');

  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  // 4. Run
  FEngine.CheckConditions(Instructions);
  
  // 5. Verify
  var Content := TFile.ReadAllText(FileToFormat);
  Assert.IsTrue(Content.Contains('{ Formatted }'), 'ToFormat.pas should be formatted (added comment)');
  
  Content := TFile.ReadAllText(FileToIgnore);
  Assert.IsFalse(Content.Contains('{ Formatted }'), 'ToIgnore.pas should NOT be formatted');
  
  Content := TFile.ReadAllText(FileToIgnore2);
  Assert.IsFalse(Content.Contains('{ Formatted }'), 'AlsoIgnore.pas should NOT be formatted');

  Assert.IsTrue(Instructions.Contains('Formatted: ' + FileToFormat), 'Output should list ToFormat.pas with its full path. Actual: ' + Instructions);
  Assert.IsFalse(Instructions.Contains('ToIgnore.pas'), 'Output should NOT list ToIgnore.pas');
  Assert.IsFalse(Instructions.Contains('AlsoIgnore.pas'), 'Output should NOT list AlsoIgnore.pas');
end;

procedure TTestDptWorkflow.FixWithIgnorePatternAndClear;
var
  FileToFix: String;
  FileToIgnore: String;
  Instructions: String;
begin
  // 1. Create files
  FileToFix := CreateTestFile('ToFix.txt', 'Line1'#10'Line2');
  FileToIgnore := CreateTestFile('ToIgnore.txt', 'Line1'#10'Line2');

  // 2. Workflow
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      BeforeDptGuard: ClearIgnorePatterns() and IgnorePattern("ToIgnore.txt") and FixLineEndingsWindowsInGitModifiedFiles()
      {
        Fixed: `GetLastFixedFiles()`
      }
    }
    ''');

  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  // 3. Run
  FEngine.CheckConditions(Instructions);

  // 4. Verify Content
  Assert.AreEqual('Line1'#13#10'Line2', TFile.ReadAllText(FileToFix), 'ToFix should be CRLF');
  Assert.AreEqual('Line1'#10'Line2', TFile.ReadAllText(FileToIgnore), 'ToIgnore should NOT be fixed');

  // 5. Verify Output
  Assert.IsTrue(Instructions.Contains('ToFix.txt'), 'Output should contain ToFix.txt');
  Assert.IsFalse(Instructions.Contains('ToIgnore.txt'), 'Output should NOT contain ToIgnore.txt');
end;

procedure TTestDptWorkflow.IncludeAndIgnorePatterns;
var
  FileToFix: String;
  FileToIgnore: String;
  FileNotIncluded: String;
  Instructions: String;
begin
  // 1. Create files
  FileToFix := CreateTestFile('ToFix.txt', 'Line1'#10'Line2');
  FileToIgnore := CreateTestFile('ToIgnore.txt', 'Line1'#10'Line2');
  FileNotIncluded := CreateTestFile('NotIncluded.md', 'Line1'#10'Line2');

  // 2. Workflow
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      BeforeDptGuard: ClearIncludePatterns() and IncludePattern("*.txt") and IgnorePattern("ToIgnore.txt") and FixLineEndingsWindowsInGitModifiedFiles()
      {
        Fixed: `GetLastFixedFiles()`
      }
    }
    ''');

  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  // 3. Run
  FEngine.CheckConditions(Instructions);

  // 4. Verify Content
  Assert.AreEqual('Line1'#13#10'Line2', TFile.ReadAllText(FileToFix), 'ToFix should be CRLF');
  Assert.AreEqual('Line1'#10'Line2', TFile.ReadAllText(FileToIgnore), 'ToIgnore should NOT be fixed');
  Assert.AreEqual('Line1'#10'Line2', TFile.ReadAllText(FileNotIncluded), 'NotIncluded should NOT be fixed');

  // 5. Verify Output
  Assert.IsTrue(Instructions.Contains('ToFix.txt'), 'Output should contain ToFix.txt');
  Assert.IsFalse(Instructions.Contains('ToIgnore.txt'), 'Output should NOT contain ToIgnore.txt');
  Assert.IsFalse(Instructions.Contains('NotIncluded.md'), 'Output should NOT contain NotIncluded.md');
end;

procedure TTestDptWorkflow.ProcessInstructions_PreservesTextWithParentheses;
var
  Instructions: String;
begin
  // Workflow with text containing parentheses that looks similar to a function call if spaces aren't checked
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      BeforeDptGuard: 1
      {
        Dies ist Text mit (Klammern):
          - `1 + 1`
      }
    }
    ''');

  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  // Run
  FEngine.CheckConditions(Instructions);

  // Verify
  Assert.IsTrue(Instructions.Contains('Dies ist Text mit (Klammern):'), 'Text with parentheses should be preserved in instructions.');
  Assert.IsTrue(Instructions.Contains('- 2'), 'Backtick expression should still be evaluated.');
end;

procedure TTestDptWorkflow.TestComplexWorkflow_IncludeIgnoreAndFormat;
var
  FilePas: String;
  FileDpr: String;
  FileDfm: String;
  FileTxt: String;
  ScriptFile: String;
  Instructions: String;
begin
  // Create a minimal DWS script that does something visible
  ScriptFile := TPath.Combine(FTestDir, 'TaifunFormat.pas');
  TFile.WriteAllText(ScriptFile, '''
    procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax);
    begin
      AddLeadingTrivia(GetUnitKeyword(AUnit), "{ Formatted } ");
    end;
    ''');

  // Create files with LF only
  FilePas := CreateTestFile('Test.pas', 'unit Test;'#10'implementation'#10'end.');
  FileDpr := CreateTestFile('Test.dpr', 'program Test;'#10'begin'#10'end.');
  // These should be ignored by the filter
  FileDfm := CreateTestFile('Test.dfm', 'object Test: TTest'#10'end');
  FileTxt := CreateTestFile('Test.txt', 'Line1'#10'Line2');

  // Workflow
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: IsCurrentAction("Build") and
                    IncludePattern("*.pas", "*.dpr") and
                    IgnorePattern("*\TOOLS\*", "TOOLS\*") and
                    IgnorePattern("*\MISC\*", "MISC\*")
    {
      BeforeDptGuard: FixLineEndingsWindowsInGitModifiedFiles()
      {
        - In folgenden geänderten Dateien wurden die Lineendings auf 
          CRLF (Windows) gefixt:
          - `GetLastFixedFiles()`
      }

      BeforeDptGuard: FixUtf8BomInGitModifiedFiles()
      {
        - In folgenden geänderten Dateien wurde die fehlende UTF-8-BOM ergänzt:
          - `GetLastFixedFiles()`
      }

      BeforeDptGuard: FormatGitModifiedFiles("TaifunFormat.pas")
      {
        - Der DPT-Formatter hat folgende Dateien automatisch formatiert:
          - `GetLastFormattedFiles()`
      }
    }
    ''');

  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Build'); // Must match IsCurrentAction("Build")

  // Run
  FEngine.CheckConditions(Instructions);

  // Verify Content
  // PAS should be formatted and CRLF
  Assert.IsTrue(TFile.ReadAllText(FilePas).Contains('{ Formatted }'), 'PAS should be formatted');
  
  // DPR should have its line endings fixed (CRLF)
  Assert.AreEqual('program Test;'#13#10'begin'#13#10'end.', TFile.ReadAllText(FileDpr), 'DPR should be CRLF');
  
  // DFM and TXT should NOT be modified
  Assert.IsFalse(TFile.ReadAllText(FileDfm).Contains('{ Formatted }'), 'DFM should NOT be formatted');
  Assert.AreEqual('object Test: TTest'#10'end', TFile.ReadAllText(FileDfm), 'DFM should NOT have CRLF');
  Assert.AreEqual('Line1'#10'Line2', TFile.ReadAllText(FileTxt), 'TXT should NOT have CRLF');

  // Verify Output
  Assert.IsTrue(Instructions.Contains('Test.pas'), 'Output should contain Test.pas');
  Assert.IsTrue(Instructions.Contains('Test.dpr'), 'Output should contain Test.dpr');
  Assert.IsFalse(Instructions.Contains('Test.dfm'), 'Output should NOT contain Test.dfm');
  Assert.IsFalse(Instructions.Contains('Test.txt'), 'Output should NOT contain Test.txt');
end;

procedure TTestDptWorkflow.DProjPrintOutputFile;
var
  Instructions: String;
  ProjectContent: String;
  ProjectFile: String;
begin
  // 1. Create a mock dproj file
  ProjectContent := '''
    <?xml version="1.0" encoding="utf-8"?>
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <Config Condition="'$(Config)'==''">Release</Config>
        <Platform Condition="'$(Platform)'==''">Win32</Platform>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Config)'=='Release'">
        <DCC_ExeOutput>.\bin\Release</DCC_ExeOutput>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Config)'=='Debug'">
        <DCC_ExeOutput>.\bin\Debug</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''';
  ProjectFile := CreateTestFile('TestProject.dproj', ProjectContent);

  // 2. Create the workflow file testing the new function
  // We use backticks to evaluate the function and output the result
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      Output File Release: `DProjPrintOutputFile("TestProject.dproj", "Release", "Win32")`
      Output File Debug: `DProjPrintOutputFile("TestProject.dproj", "Debug", "Win32")`
      Output File Default: `DProjPrintOutputFile("TestProject.dproj")`
    }
    ''');

  // Reload workflow
  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  // 3. Run CheckConditions
  FEngine.CheckConditions(Instructions);

  // 4. Verify the output instructions contain the correct evaluated path
  Assert.IsTrue(Instructions.Contains('Output File Release:'), 'Output should contain Release label.');
  Assert.IsTrue(Instructions.Contains('bin\Release\TestProject.exe'), 'Output should contain the resolved project output file for Release. Actual: ' + Instructions);
  Assert.IsTrue(Instructions.Contains('Output File Debug:'), 'Output should contain Debug label.');
  Assert.IsTrue(Instructions.Contains('bin\Debug\TestProject.exe'), 'Output should contain the resolved project output file for Debug. Actual: ' + Instructions);
  Assert.IsTrue(Instructions.Contains('Output File Default:'), 'Output should contain Default label.');
  Assert.IsTrue(Instructions.Contains('Output File Default: ') and Instructions.Substring(Instructions.IndexOf('Output File Default: ')).Contains('bin\Release\TestProject.exe'), 'Output should contain the resolved project output file for Default (Release). Actual: ' + Instructions);
end;

function TTestDptWorkflow.CountDiag(const ASubstr: String): Integer;
var
  Line: String;
begin
  Result := 0;
  for Line in FDiagLines do
    if Line.Contains(ASubstr) then
      Inc(Result);
end;

procedure TTestDptWorkflow.WorkflowFoundNotAnnouncedAtConstruction;
begin
  // Setup constructed the engine with a discoverable .DptAiWorkflow
  // present (empty, but found). The "FOUND" line must NOT be emitted at
  // construction -- it is announced lazily, only when the workflow is
  // actually evaluated in CheckConditions.
  Assert.AreEqual(0, CountDiag('Workflow file FOUND'),
    'FOUND must not be announced at engine construction. Captured: ' + FDiagLines.Text);
end;

procedure TTestDptWorkflow.WorkflowFoundAnnouncedOnceWhenProcessed;
var
  Instructions: String;
begin
  // A workflow with a real, evaluable block.
  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      Hello
    }
    ''');
  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  // Nothing should have been announced merely by constructing.
  Assert.AreEqual(0, CountDiag('Workflow file FOUND'),
    'FOUND must not be announced at construction even with a non-empty workflow. Captured: ' + FDiagLines.Text);

  // CheckConditions is invoked once per guard phase by the dispatcher
  // (gtBefore before the action, gtAfter after it). The announcement
  // must fire exactly once across both.
  FEngine.CheckConditions(Instructions, gtBefore);
  FEngine.CheckConditions(Instructions, gtAfter);

  Assert.AreEqual(1, CountDiag('Workflow file FOUND'),
    'FOUND must be announced exactly once across multiple CheckConditions calls. Captured: ' + FDiagLines.Text);
  Assert.IsTrue(FDiagLines.Text.Contains(FWorkflowFile),
    'The FOUND line should reference the resolved workflow file path. Captured: ' + FDiagLines.Text);
end;

procedure TTestDptWorkflow.WorkflowFoundNotAnnouncedWhenNoBlocksToProcess;
var
  Instructions: String;
begin
  // Setup's .DptAiWorkflow is empty -> it is discovered but parses into
  // zero blocks, so CheckConditions has nothing to process and exits
  // early. "Actually processed" means at least one block ran, so FOUND
  // must stay silent here.
  FEngine.CheckConditions(Instructions, gtBefore);
  FEngine.CheckConditions(Instructions, gtAfter);

  Assert.AreEqual(0, CountDiag('Workflow file FOUND'),
    'FOUND must not be announced when the workflow has no blocks to process. Captured: ' + FDiagLines.Text);
end;

procedure TTestDptWorkflow.BaseDir_DefaultsToCurrentDir;
begin
  // Setup constructed the engine without a base directory
  Assert.AreEqual(GetCurrentDir, FEngine.BaseDir, 'BaseDir should default to the current directory');
  Assert.AreEqual(FWorkflowFile, FEngine.WorkflowFile, 'The workflow file should be found from the current directory');
end;

procedure TTestDptWorkflow.WorkflowFile_SearchedUpFromBaseDir_NotFromCurrentDir;
var
  MainDir: String;
  WtDir  : String;
begin
  MainDir := CreateCheckout('main');
  WtDir := CreateCheckout('wt');
  WriteFile(TPath.Combine(MainDir, '.DptAiWorkflow'), '');
  WriteFile(TPath.Combine(WtDir, '.DptAiWorkflow'), '');
  TDirectory.CreateDirectory(TPath.Combine(WtDir, 'Src\Sub'));

  // The caller sits in the main checkout, the project lives in the worktree
  SetCurrentDir(MainDir);
  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Build', TPath.Combine(WtDir, 'Src\Sub'));

  Assert.AreEqual(TPath.Combine(WtDir, 'Src\Sub'), FEngine.BaseDir);
  Assert.AreEqual(TPath.Combine(WtDir, '.DptAiWorkflow'), FEngine.WorkflowFile,
    'The workflow file must be searched upwards from the base directory, not from the current directory');
end;

procedure TTestDptWorkflow.FixFunctions_ProcessBaseDirCheckout_NotCurrentDirCheckout;
const
  Dirty = 'unit Foo;'#10'implementation'#10'end.';
  Workflow = '''
    BeforeDptGuard: IsCurrentAction("Build") and IncludePattern("*.pas")
    {
      BeforeDptGuard: FixLineEndingsWindowsInGitModifiedFiles()
      {
        Fixed: `GetLastFixedFiles()`
      }

      BeforeDptGuard: FixUtf8BomInGitModifiedFiles()
      {
        Bom: `GetLastFixedFiles()`
      }
    }
    ''';
var
  Dir         : String;
  Instructions: String;
  MainDir     : String;
  MainFoo     : String;
  WtDir       : String;
  WtFoo       : String;
begin
  // Main checkout and worktree carry the same uncommitted, unnormalized unit
  MainDir := CreateCheckout('main');
  WtDir := CreateCheckout('wt');
  WriteFile(TPath.Combine(MainDir, '.DptAiWorkflow'), Workflow);
  WriteFile(TPath.Combine(WtDir, '.DptAiWorkflow'), Workflow);
  MainFoo := WriteFile(TPath.Combine(MainDir, 'Src\Foo.pas'), Dirty);
  WtFoo := WriteFile(TPath.Combine(WtDir, 'Src\Foo.pas'), Dirty);

  // The caller sits in the main checkout and builds a worktree project
  SetCurrentDir(MainDir);
  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Build', TPath.Combine(WtDir, 'Src'));
  FGitDirs.Clear;

  FEngine.CheckConditions(Instructions);

  Assert.AreEqual('unit Foo;'#13#10'implementation'#13#10'end.', TFile.ReadAllText(WtFoo, TEncoding.UTF8),
    'The worktree unit should be CRLF');
  Assert.IsTrue(HasUtf8Bom(WtFoo), 'The worktree unit should have a BOM');

  Assert.AreEqual(Dirty, TFile.ReadAllText(MainFoo, TEncoding.ASCII), 'The main checkout unit must stay untouched');
  Assert.IsFalse(HasUtf8Bom(MainFoo), 'The main checkout unit must not get a BOM');

  Assert.IsTrue(FGitDirs.Count > 0, 'git should have been queried');
  for Dir in FGitDirs do
    Assert.IsTrue(Dir.StartsWith(WtDir + '\', True), 'git must run in the worktree, but ran in: ' + Dir);

  Assert.IsTrue(Instructions.Contains('Fixed: ' + WtFoo), 'Output should list the worktree unit. Actual: ' + Instructions);
  Assert.IsFalse(Instructions.Contains(MainFoo), 'Output must not list the main checkout unit. Actual: ' + Instructions);
end;

procedure TTestDptWorkflow.FormatGitModifiedFiles_FormatsBaseDirCheckoutWithItsOwnScript;
const
  Unformatted = 'unit Foo; implementation end.';
  Workflow = '''
    BeforeDptGuard: IsCurrentAction("Build") and IncludePattern("*.pas") and IgnorePattern("*\Format\*")
    {
      BeforeDptGuard: FormatGitModifiedFiles("Format\Fmt.pas")
      {
        Formatted: `GetLastFormattedFiles()`
      }
    }
    ''';

  function FormatScript(const AMarker: String): String;
  begin
    Result :=
      'procedure OnVisitUnitStart(AUnit: TCompilationUnitSyntax);' + sLineBreak +
      'begin' + sLineBreak +
      '  AddLeadingTrivia(GetUnitKeyword(AUnit), "' + AMarker + ' ");' + sLineBreak +
      'end;';
  end;

var
  Instructions: String;
  MainDir     : String;
  MainFoo     : String;
  WtContent   : String;
  WtDir       : String;
  WtFoo       : String;
begin
  MainDir := CreateCheckout('main');
  WtDir := CreateCheckout('wt');
  WriteFile(TPath.Combine(MainDir, '.DptAiWorkflow'), Workflow);
  WriteFile(TPath.Combine(WtDir, '.DptAiWorkflow'), Workflow);
  // Each checkout ships its own formatter script under the same relative path
  WriteFile(TPath.Combine(MainDir, 'Format\Fmt.pas'), FormatScript('{ Main }'));
  WriteFile(TPath.Combine(WtDir, 'Format\Fmt.pas'), FormatScript('{ Worktree }'));
  MainFoo := WriteFile(TPath.Combine(MainDir, 'Src\Foo.pas'), Unformatted);
  WtFoo := WriteFile(TPath.Combine(WtDir, 'Src\Foo.pas'), Unformatted);

  // The caller sits in the main checkout and builds a worktree project
  SetCurrentDir(MainDir);
  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Build', TPath.Combine(WtDir, 'Src'));

  FEngine.CheckConditions(Instructions);

  WtContent := TFile.ReadAllText(WtFoo);
  Assert.IsTrue(WtContent.Contains('{ Worktree }'), 'The worktree unit should be formatted by the worktree script. Actual: ' + WtContent);
  Assert.IsFalse(WtContent.Contains('{ Main }'), 'The main checkout script must not format the worktree unit');

  Assert.AreEqual(Unformatted, TFile.ReadAllText(MainFoo, TEncoding.ASCII), 'The main checkout unit must stay untouched');

  Assert.IsTrue(Instructions.Contains('Formatted: ' + WtFoo), 'Output should list the worktree unit. Actual: ' + Instructions);
  Assert.IsFalse(Instructions.Contains(MainFoo), 'Output must not list the main checkout unit. Actual: ' + Instructions);
end;

procedure TTestDptWorkflow.DProjPrintOutputFile_ResolvedRelativeToWorkflowFile;
var
  Instructions: String;
  MainDir     : String;
  WtDir       : String;
begin
  MainDir := CreateCheckout('main');
  WtDir := CreateCheckout('wt');
  WriteFile(TPath.Combine(WtDir, 'Proj.dproj'), '''
    <?xml version="1.0" encoding="utf-8"?>
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup Condition="'$(Config)'=='Release'">
        <DCC_ExeOutput>.\bin\Release</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');
  WriteFile(TPath.Combine(WtDir, '.DptAiWorkflow'), '''
    BeforeDptGuard: 1
    {
      Output File: `DProjPrintOutputFile("Proj.dproj", "Release", "Win32")`
    }
    ''');

  // The current directory holds no Proj.dproj -- only the workflow's own checkout does
  SetCurrentDir(MainDir);
  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test', WtDir);

  FEngine.CheckConditions(Instructions);

  Assert.IsTrue(Instructions.Contains('Output File: ' + TPath.Combine(WtDir, 'bin\Release\Proj.exe')),
    'The project path should be resolved relative to the workflow file. Actual: ' + Instructions);
end;

procedure TTestDptWorkflow.ProcessedFiles_ReportedWithFullPath;
var
  FileInSub   : String;
  Instructions: String;
begin
  FileInSub := WriteFile(TPath.Combine(FTestDir, 'Sub\Unix.txt'), 'Line1'#10'Line2');

  TFile.WriteAllText(FWorkflowFile, '''
    BeforeDptGuard: 1
    {
      BeforeDptGuard: IncludePattern("*.txt") and FixLineEndingsWindowsInGitModifiedFiles()
      {
        Fixed: `GetLastFixedFiles()`
      }
    }
    ''');
  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test');

  FEngine.CheckConditions(Instructions);

  // The full path tells which checkout was touched, a bare file name does not
  Assert.IsTrue(Instructions.Contains('Fixed: ' + FileInSub), 'Output should list the full path. Actual: ' + Instructions);
end;

end.
