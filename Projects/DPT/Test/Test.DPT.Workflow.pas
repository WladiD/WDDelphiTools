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

  DPT.Types,
  DPT.Workflow;

type

  [TestFixture]
  TTestDptWorkflow = class
  private
    FTestDir: String;
    FWorkflowFile: String;
    FSessionFile: String;
    FEngine: TDptWorkflowEngine;
    FOldGeminiVar: String;
    function CreateTestFile(const AName, AContent: String; AEncoding: TEncoding = nil): String;
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
  end;

implementation

function TTestDptWorkflow.CreateTestFile(const AName, AContent: String; AEncoding: TEncoding = nil): String;
begin
  Result := TPath.Combine(FTestDir, AName);
  if AEncoding = nil then
    TFile.WriteAllText(Result, AContent)
  else
    TFile.WriteAllText(Result, AContent, AEncoding);
end;

procedure TTestDptWorkflow.Setup;
begin
  FTestDir := TPath.Combine(TPath.GetTempPath, 'DptWorkflowTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
  FWorkflowFile := TPath.Combine(FTestDir, '.DptAiWorkflow');
  TFile.WriteAllText(FWorkflowFile, ''); // Empty workflow file needed for discovery
  
  // Force AI Mode via Environment Variable
  FOldGeminiVar := GetEnvironmentVariable('GEMINI_CLI');
  SetEnvironmentVariable('GEMINI_CLI', '1');
  
  // We need to set the current dir to test dir so it finds the workflow file
  SetCurrentDir(FTestDir);
  
  FEngine := TDptWorkflowEngine.Create('Test', 'Test');
end;

procedure TTestDptWorkflow.TearDown;
begin
  FEngine.Free;
  if FOldGeminiVar <> '' then
    SetEnvironmentVariable('GEMINI_CLI', PChar(FOldGeminiVar))
  else
    SetEnvironmentVariable('GEMINI_CLI', nil);

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

  // 2. Register files in session manually
  FEngine.StartSession; // Creates session file
  FEngine.AddFilesToSession([FileMixed, FileUnix, FileWin]);

  // 3. Inject a workflow block that calls the fix function
  TFile.WriteAllText(FWorkflowFile, 
    'BeforeDptGuard: 1' + sLineBreak +
    '{' + sLineBreak +
    '  BeforeDptGuard: FixLineEndingsWindowsInAiSessionFiles()' + sLineBreak +
    '  {' + sLineBreak +
    '    Fixed: `GetLastFixedFiles()`' + sLineBreak +
    '  }' + sLineBreak +
    '}');
  
  // Reload workflow
  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test', 'Test');

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

  // 2. Register files
  FEngine.StartSession;
  FEngine.AddFilesToSession([FileNoBom, FileWithBom]);

  // 3. Workflow
  TFile.WriteAllText(FWorkflowFile, 
    'BeforeDptGuard: 1' + sLineBreak +
    '{' + sLineBreak +
    '  BeforeDptGuard: FixUtf8BomInAiSessionFiles()' + sLineBreak +
    '  {' + sLineBreak +
    '    Fixed: `GetLastFixedFiles()`' + sLineBreak +
    '  }' + sLineBreak +
    '}');

  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test', 'Test');

  // 4. Run
  FEngine.CheckConditions(Instructions);

  // 5. Verify Content
  Bytes := TFile.ReadAllBytes(FileNoBom);
  
  Assert.IsTrue((Length(Bytes) >= 3) and 
                (Bytes[0] = BOM[0]) and 
                (Bytes[1] = BOM[1]) and 
                (Bytes[2] = BOM[2]), 'NoBom should now have BOM');

  // 6. Verify Output
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

  // 4. Register files
  FEngine.StartSession;
  FEngine.AddFilesToSession([FileBadLines, FileBadBom, FileBadBoth]);

  // 5. Workflow
  TFile.WriteAllText(FWorkflowFile, 
    'BeforeDptGuard: 1' + sLineBreak +
    '{' + sLineBreak +
    '  BeforeDptGuard: FixLineEndingsWindowsInAiSessionFiles()' + sLineBreak +
    '  {' + sLineBreak +
    '    - In folgenden Dateien wurden die Lineendings auf CRLF (Windows) gefixt:' + sLineBreak +
    '      - `GetLastFixedFiles()`' + sLineBreak +
    '  }' + sLineBreak +
    sLineBreak +
    '  BeforeDptGuard: FixUtf8BomInAiSessionFiles()' + sLineBreak +
    '  {' + sLineBreak +
    '    - In folgenden Dateien wurde die fehlende UTF-8-BOM gefixt:' + sLineBreak +
    '      - `GetLastFixedFiles()`' + sLineBreak +
    '  }' + sLineBreak +
    '}');

  FEngine.Free;
  FEngine := TDptWorkflowEngine.Create('Test', 'Test');

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

end.
