// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Git;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  DUnitX.TestFramework,
  DPT.Git;

type
  [TestFixture]
  TTestDptGit = class
  private
    FTestDir: string;
    FGitStatusOutput: string;
    FGitStatusExitCode: Integer;
    FGitRevParseOutput: string;
    FGitRevParseExitCode: Integer;
    function MockRunCommand(const ACommand, ADirectory: string; out AOutput: string): Integer;
    procedure CreateDummyFile(const ARelativePath: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestNoGitRepo;
    [Test]
    procedure TestStatusEmpty;
    [Test]
    procedure TestModifiedFilesWithoutQuotes;
    [Test]
    procedure TestModifiedFilesWithQuotes;
    [Test]
    procedure TestFilesDoNotExist;
  end;

implementation

{ TTestDptGit }

function TTestDptGit.MockRunCommand(const ACommand, ADirectory: string; out AOutput: string): Integer;
begin
  if Pos('status', ACommand) > 0 then
  begin
    AOutput := FGitStatusOutput;
    Result := FGitStatusExitCode;
  end
  else if Pos('rev-parse', ACommand) > 0 then
  begin
    AOutput := FGitRevParseOutput;
    Result := FGitRevParseExitCode;
  end
  else
  begin
    AOutput := '';
    Result := 1;
  end;
end;

procedure TTestDptGit.CreateDummyFile(const ARelativePath: string);
var
  Full: string;
begin
  Full := TPath.Combine(FTestDir, ARelativePath);
  ForceDirectories(ExtractFilePath(Full));
  TFile.WriteAllText(Full, 'dummy');
end;

procedure TTestDptGit.Setup;
begin
  FTestDir := TPath.Combine(TPath.GetTempPath, 'DptGitTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);

  FGitStatusOutput := '';
  FGitStatusExitCode := 0;
  FGitRevParseOutput := FTestDir;
  FGitRevParseExitCode := 0;

  // Set the class variable globally to our instance method
  TDptGit.MockRunCommand := MockRunCommand;
end;

procedure TTestDptGit.TearDown;
begin
  TDptGit.MockRunCommand := nil;
  if TDirectory.Exists(FTestDir) then
    TDirectory.Delete(FTestDir, True);
end;

procedure TTestDptGit.TestNoGitRepo;
begin
  FGitStatusExitCode := 128; // Standard git exit code if not in a repo
  FGitStatusOutput := 'fatal: not a git repository';

  var Res := TDptGit.GetModifiedFiles(FTestDir);

  Assert.AreEqual(0, Length(Res), 'Result should be empty if git status fails');
end;

procedure TTestDptGit.TestStatusEmpty;
begin
  FGitStatusExitCode := 0;
  FGitStatusOutput := ''; // clean working directory

  var Res := TDptGit.GetModifiedFiles(FTestDir);

  Assert.AreEqual(0, Length(Res), 'Result should be empty if git status is empty');
end;

procedure TTestDptGit.TestModifiedFilesWithoutQuotes;
begin
  CreateDummyFile('file1.pas');
  CreateDummyFile('subdir\file2.pas');

  FGitStatusExitCode := 0;
  FGitStatusOutput := 
    ' M file1.pas' + sLineBreak +
    '?? subdir/file2.pas' + sLineBreak;

  var Res := TDptGit.GetModifiedFiles(FTestDir);

  Assert.AreEqual(2, Length(Res), 'Should find 2 files');
  
  // They should be resolved to absolute paths and exist check should pass
  Assert.AreEqual(TPath.Combine(FTestDir, 'file1.pas'), Res[0], 'Path 1 mismatch');
  Assert.AreEqual(TPath.Combine(FTestDir, 'subdir\file2.pas'), Res[1], 'Path 2 mismatch');
end;

procedure TTestDptGit.TestModifiedFilesWithQuotes;
begin
  CreateDummyFile('dir with space\file.pas');

  FGitStatusExitCode := 0;
  FGitStatusOutput := ' M "dir with space/file.pas"' + sLineBreak;

  var Res := TDptGit.GetModifiedFiles(FTestDir);

  Assert.AreEqual(1, Length(Res), 'Should find 1 file');
  Assert.AreEqual(TPath.Combine(FTestDir, 'dir with space\file.pas'), Res[0], 'Quoted path mismatch');
end;

procedure TTestDptGit.TestFilesDoNotExist;
begin
  // We do NOT create dummy files here

  FGitStatusExitCode := 0;
  FGitStatusOutput := ' M deleted_but_staged_file.pas' + sLineBreak;

  var Res := TDptGit.GetModifiedFiles(FTestDir);

  // Since the file does not exist physically during the check, it should be ignored by GetModifiedFiles
  Assert.AreEqual(0, Length(Res), 'Non-existent files should be filtered out');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDptGit);
end.
