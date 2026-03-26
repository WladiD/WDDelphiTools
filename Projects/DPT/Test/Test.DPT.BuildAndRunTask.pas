unit Test.DPT.BuildAndRunTask;

interface

uses

  System.Classes,
  System.SysUtils,
  System.IOUtils,

  DUnitX.TestFramework,

  DPT.Build.Task;

type

  // Stub class to access protected method
  TStubDptBuildAndRunTask = class(TDptBuildAndRunTask);

  [TestFixture]
  TDptBuildAndRunTaskTests = class
  private
    FProjectFile: String;
    FTask       : TStubDptBuildAndRunTask;
    FTempDir    : String;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Parse_NoWait;
    [Test]
    procedure Parse_Complex;
    [Test]
    procedure IsBuildNeeded_SearchPath;
    [Test]
    procedure IsBuildNeeded_SearchPath_NoBuildNeeded;
  end;

implementation

uses
  DPT.Types;

{ TDptBuildAndRunTaskTests }

procedure TDptBuildAndRunTaskTests.Setup;
var
  ProjectDir: String;
begin
  FTask := TStubDptBuildAndRunTask.Create;
  FTempDir := TPath.Combine(TPath.GetTempPath, TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempDir);

  ProjectDir := TPath.Combine(FTempDir, 'Project');
  TDirectory.CreateDirectory(ProjectDir);

  FProjectFile := TPath.Combine(ProjectDir, 'TestProject.dproj');
  FTask.ProjectFile := FProjectFile;
end;

procedure TDptBuildAndRunTaskTests.TearDown;
begin
  FTask.Free;
  if TDirectory.Exists(FTempDir) then
    TDirectory.Delete(FTempDir, True);
end;

procedure TDptBuildAndRunTaskTests.Parse_NoWait;
var
  CmdLine: TCmdLineConsumer;
begin
  CmdLine := TCmdLineConsumer.Create(['Test.dproj', '--NoWait']);
  try
    FTask.Parse(CmdLine);
    Assert.IsTrue(FTask.NoWait, 'NoWait should be True');
  finally
    CmdLine.Free;
  end;
end;

procedure TDptBuildAndRunTaskTests.Parse_Complex;
var
  CmdLine: TCmdLineConsumer;
begin
  CmdLine := TCmdLineConsumer.Create(['Test.dproj', 'Win64', 'Release', '--OnlyIfChanged', '--NoWait', '--', '-arg1', '-arg2']);
  try
    FTask.Parse(CmdLine);
    Assert.AreEqual('Win64', FTask.TargetPlatform);
    Assert.AreEqual('Release', FTask.Config);
    Assert.IsTrue(FTask.OnlyIfChanged, 'OnlyIfChanged should be True');
    Assert.IsTrue(FTask.NoWait, 'NoWait should be True');
    Assert.AreEqual('-arg1 -arg2', FTask.RunArgs);
  finally
    CmdLine.Free;
  end;
end;

procedure TDptBuildAndRunTaskTests.IsBuildNeeded_SearchPath;
var
  SearchPathDir: String;
  DprojContent : String;
  ExePath      : String;
  SourceFile   : String;
  NewerFile    : String;
begin
  SearchPathDir := TPath.Combine(FTempDir, 'ExternalSearchPath');
  TDirectory.CreateDirectory(SearchPathDir);

  DprojContent :=
    '<Project>' +
    '  <PropertyGroup>' +
    '    <DCC_UnitSearchPath>' + SearchPathDir + '</DCC_UnitSearchPath>' +
    '  </PropertyGroup>' +
    '</Project>';
  TFile.WriteAllText(FProjectFile, DprojContent);

  ExePath := TPath.Combine(TPath.GetDirectoryName(FProjectFile), 'TestExe.exe');
  TFile.WriteAllText(ExePath, 'dummy exe');

  // Ensure project file is older than exe to not trigger build
  TFile.SetLastWriteTime(FProjectFile, Now - 2);
  // Ensure exe is older than the source file we are about to create
  TFile.SetLastWriteTime(ExePath, Now - 1);

  SourceFile := TPath.Combine(SearchPathDir, 'ExternalUnit.pas');
  TFile.WriteAllText(SourceFile, 'unit ExternalUnit; interface implementation end.');

  FTask.Config := 'Debug';
  FTask.TargetPlatform := 'Win32';

  Assert.IsTrue(FTask.IsBuildNeeded(ExePath, NewerFile), 'Build should be needed because a file in search path is newer');
  Assert.AreEqual(SourceFile, NewerFile, 'NewerFile should match the external unit');
end;

procedure TDptBuildAndRunTaskTests.IsBuildNeeded_SearchPath_NoBuildNeeded;
var
  SearchPathDir: String;
  DprojContent : String;
  ExePath      : String;
  SourceFile   : String;
  NewerFile    : String;
begin
  SearchPathDir := TPath.Combine(FTempDir, 'ExternalSearchPath');
  TDirectory.CreateDirectory(SearchPathDir);

  DprojContent :=
    '<Project>' +
    '  <PropertyGroup>' +
    '    <DCC_UnitSearchPath>' + SearchPathDir + '</DCC_UnitSearchPath>' +
    '  </PropertyGroup>' +
    '</Project>';
  TFile.WriteAllText(FProjectFile, DprojContent);

  SourceFile := TPath.Combine(SearchPathDir, 'ExternalUnit.pas');
  TFile.WriteAllText(SourceFile, 'unit ExternalUnit; interface implementation end.');

  ExePath := TPath.Combine(TPath.GetDirectoryName(FProjectFile), 'TestExe.exe');
  TFile.WriteAllText(ExePath, 'dummy exe');

  // Ensure timestamp difference: Source is older than Exe
  TFile.SetLastWriteTime(SourceFile, Now - 1);

  FTask.Config := 'Debug';
  FTask.TargetPlatform := 'Win32';

  Assert.IsFalse(FTask.IsBuildNeeded(ExePath, NewerFile), 'Build should NOT be needed because executable is newer than search path file');
  Assert.IsEmpty(NewerFile, 'NewerFile should be empty');
end;

end.
