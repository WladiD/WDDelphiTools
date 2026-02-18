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

end.
