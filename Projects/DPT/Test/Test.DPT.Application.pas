// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Application;

interface

uses

  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  DPT.Application;

type

  /// <summary>
  ///   Exercises <see cref="TDptApplication.IsSlimServerStart"/>: the decision
  ///   whether a command line starts the embedded Slim server or a CLI action.
  /// </summary>
  [TestFixture]
  TTestSlimServerStart = class
  public
    // ---- Slim server requested ----
    [Test]
    procedure SlimPortSwitch_Alone_StartsServer;
    [Test]
    procedure SlimPortSwitch_AmongOtherParams_StartsServer;
    [Test]
    procedure SlimPortSwitch_IsCaseInsensitive;
    [Test]
    procedure SoleInteger_StartsServer;

    // ---- CLI action requested ----
    [Test]
    procedure NoParams_NoServer;
    [Test]
    procedure SoleNonInteger_NoServer;
    [Test]
    procedure SlimPortSwitch_WithoutNumber_NoServer;
    [Test]
    procedure BuildAndRun_WithRunArgsAfterSeparator_NoServer;
    [Test]
    procedure OpenUnit_GoToLine_NoServer;
    [Test]
    procedure VersionAlias_FollowedByInteger_NoServer;
  end;

  /// <summary>
  ///   Exercises <see cref="TDptApplication.WorkflowBaseDir"/>: the directory
  ///   the workflow engine works from. For a project action it must be the
  ///   project's directory, so a build of a git worktree project started from
  ///   the main checkout pre-processes the worktree, not the main checkout.
  /// </summary>
  [TestFixture]
  TTestWorkflowBaseDir = class
  private
    FCurrentDir : String;
    FProjectDir : String;
    FProjectFile: String;
    FTestDir    : String;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // ---- Project directory ----
    [Test]
    procedure Build_AbsoluteProjectPathInOtherCheckout_ReturnsProjectDir;
    [Test]
    procedure Build_ForwardSlashes_ReturnsProjectDir;
    [Test]
    procedure Build_RelativeProjectPath_ResolvedAgainstCurrentDir;
    [Test]
    procedure Build_WithoutVersion_ReturnsProjectDir;
    [Test]
    procedure Build_LatestAlias_ReturnsProjectDir;
    [Test]
    procedure Build_ActionIsCaseInsensitive;
    [Test]
    procedure Build_ProjectFileMissingButDirExists_ReturnsProjectDir;
    [Test]
    procedure Compile_ReturnsProjectDir;
    [Test]
    procedure BuildAndRun_ReturnsProjectDir;
    [Test]
    procedure CompileAndRun_ReturnsProjectDir;

    // ---- Current directory ----
    [Test]
    procedure NoParams_ReturnsCurrentDir;
    [Test]
    procedure Build_WithoutProjectArgument_ReturnsCurrentDir;
    [Test]
    procedure Build_EmptyProjectArgument_ReturnsCurrentDir;
    [Test]
    procedure Build_ProjectDirDoesNotExist_ReturnsCurrentDir;
    [Test]
    procedure NonProjectAction_ReturnsCurrentDir;
  end;

implementation

{ TTestSlimServerStart }

procedure TTestSlimServerStart.SlimPortSwitch_Alone_StartsServer;
var
  LPort: Integer;
begin
  Assert.IsTrue(TDptApplication.IsSlimServerStart(['--SlimPort=8085'], LPort));
  Assert.AreEqual(8085, LPort);
end;

procedure TTestSlimServerStart.SlimPortSwitch_AmongOtherParams_StartsServer;
var
  LPort: Integer;
begin
  Assert.IsTrue(TDptApplication.IsSlimServerStart(['--verbose', '--SlimPort=8085', 'x'], LPort));
  Assert.AreEqual(8085, LPort);
end;

procedure TTestSlimServerStart.SlimPortSwitch_IsCaseInsensitive;
var
  LPort: Integer;
begin
  Assert.IsTrue(TDptApplication.IsSlimServerStart(['--slimport=9001'], LPort));
  Assert.AreEqual(9001, LPort);
end;

procedure TTestSlimServerStart.SoleInteger_StartsServer;
var
  LPort: Integer;
begin
  // FitNesse convention: COMMAND_PATTERN + port, so the port is the only arg
  Assert.IsTrue(TDptApplication.IsSlimServerStart(['9066'], LPort));
  Assert.AreEqual(9066, LPort);
end;

procedure TTestSlimServerStart.NoParams_NoServer;
var
  LPort: Integer;
begin
  Assert.IsFalse(TDptApplication.IsSlimServerStart([], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.SoleNonInteger_NoServer;
var
  LPort: Integer;
begin
  Assert.IsFalse(TDptApplication.IsSlimServerStart(['Help'], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.SlimPortSwitch_WithoutNumber_NoServer;
var
  LPort: Integer;
begin
  Assert.IsFalse(TDptApplication.IsSlimServerStart(['--SlimPort=abc'], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.BuildAndRun_WithRunArgsAfterSeparator_NoServer;
var
  LPort: Integer;
begin
  // The reported regression: "-- 9066" are run arguments for the built exe,
  // but the trailing integer used to be taken as a Slim port and DPT started
  // its Slim server instead of running the build result.
  Assert.IsFalse(TDptApplication.IsSlimServerStart(
    ['Latest', 'BuildAndRun', 'TOOLS\SlimProxy\PAS\SlimProxy.dproj', 'Win32', 'Debug',
     '--OnlyIfChanged', '--', '9066'], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.OpenUnit_GoToLine_NoServer;
var
  LPort: Integer;
begin
  // Same shape without a separator: the number is the action's own argument
  Assert.IsFalse(TDptApplication.IsSlimServerStart(
    ['LATEST', 'OpenUnit', 'C:\Projects\MyUnit.pas', 'GoToLine', '42'], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.VersionAlias_FollowedByInteger_NoServer;
var
  LPort: Integer;
begin
  Assert.IsFalse(TDptApplication.IsSlimServerStart(['LATEST', '9000'], LPort));
  Assert.AreEqual(0, LPort);
end;

{ TTestWorkflowBaseDir }

procedure TTestWorkflowBaseDir.Setup;
begin
  // <TestDir>\main is where the caller sits, <TestDir>\wt\Src holds the project
  FTestDir := TPath.Combine(TPath.GetTempPath, 'DptBaseDirTest_' + TGUID.NewGuid.ToString);
  FCurrentDir := TPath.Combine(FTestDir, 'main');
  FProjectDir := TPath.Combine(FTestDir, 'wt\Src');
  FProjectFile := TPath.Combine(FProjectDir, 'Proj.dproj');
  TDirectory.CreateDirectory(FCurrentDir);
  TDirectory.CreateDirectory(FProjectDir);
  TFile.WriteAllText(FProjectFile, '<Project/>');
end;

procedure TTestWorkflowBaseDir.TearDown;
begin
  if TDirectory.Exists(FTestDir) then
    TDirectory.Delete(FTestDir, True);
end;

procedure TTestWorkflowBaseDir.Build_AbsoluteProjectPathInOtherCheckout_ReturnsProjectDir;
begin
  // The reported case: an AI agent sits in the main checkout and builds a
  // worktree project by its absolute path
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['D13', 'Build', FProjectFile], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.Build_ForwardSlashes_ReturnsProjectDir;
begin
  // Git-Bash callers pass forward slashes
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['D13', 'Build', FProjectFile.Replace('\', '/')], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.Build_RelativeProjectPath_ResolvedAgainstCurrentDir;
begin
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['D13', 'Build', 'wt\Src\Proj.dproj'], FTestDir));
end;

procedure TTestWorkflowBaseDir.Build_WithoutVersion_ReturnsProjectDir;
begin
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['Build', FProjectFile], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.Build_LatestAlias_ReturnsProjectDir;
begin
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['LATEST', 'Build', FProjectFile, 'Win32', 'Debug'], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.Build_ActionIsCaseInsensitive;
begin
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['D13', 'build', FProjectFile], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.Build_ProjectFileMissingButDirExists_ReturnsProjectDir;
begin
  // A non-existing project still belongs to its directory's checkout; the
  // build itself reports the missing file
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['D13', 'Build', TPath.Combine(FProjectDir, 'Missing.dproj')], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.Compile_ReturnsProjectDir;
begin
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['D13', 'Compile', FProjectFile], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.BuildAndRun_ReturnsProjectDir;
begin
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['D13', 'BuildAndRun', FProjectFile, '--', '9066'], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.CompileAndRun_ReturnsProjectDir;
begin
  Assert.AreEqual(FProjectDir,
    TDptApplication.WorkflowBaseDir(['D13', 'CompileAndRun', FProjectFile], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.NoParams_ReturnsCurrentDir;
begin
  Assert.AreEqual(FCurrentDir, TDptApplication.WorkflowBaseDir([], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.Build_WithoutProjectArgument_ReturnsCurrentDir;
begin
  Assert.AreEqual(FCurrentDir, TDptApplication.WorkflowBaseDir(['D13', 'Build'], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.Build_EmptyProjectArgument_ReturnsCurrentDir;
begin
  Assert.AreEqual(FCurrentDir, TDptApplication.WorkflowBaseDir(['D13', 'Build', ''], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.Build_ProjectDirDoesNotExist_ReturnsCurrentDir;
begin
  Assert.AreEqual(FCurrentDir,
    TDptApplication.WorkflowBaseDir(['D13', 'Build', TPath.Combine(FTestDir, 'NoSuchDir\Proj.dproj')], FCurrentDir));
end;

procedure TTestWorkflowBaseDir.NonProjectAction_ReturnsCurrentDir;
begin
  // Format and Lint take file lists, not a project: they keep the current directory
  Assert.AreEqual(FCurrentDir,
    TDptApplication.WorkflowBaseDir(['D13', 'Format', 'TaifunFormat.pas', FProjectFile], FCurrentDir));
  Assert.AreEqual(FCurrentDir,
    TDptApplication.WorkflowBaseDir(['D13', 'Lint', 'Style.pas', FProjectFile], FCurrentDir));
end;

end.
