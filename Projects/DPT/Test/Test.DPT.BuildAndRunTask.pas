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

  // Metaclass so the custom-config assertion can be run against every concrete
  // build task without duplicating the setup for each.
  TDptBuildTaskClass = class of TDptBuildTask;

  [TestFixture]
  TDptBuildAndRunTaskTests = class
  private
    FProjectFile: String;
    FTask       : TStubDptBuildAndRunTask;
    FTempDir    : String;
    function DprojWithConfigs(const AConfigs: array of String): String;
    procedure CheckCustomConfigAccepted(ATaskClass: TDptBuildTaskClass; AWithPlatform: Boolean);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure Parse_NoWait;
    [Test]
    procedure Parse_Complex;
    // A custom (.dproj-defined) config must be honoured by all four concrete
    // build tasks, both when a platform is given and when it is omitted.
    [Test]
    procedure Parse_BuildTask_CustomConfig;
    [Test]
    procedure Parse_BuildTask_CustomConfig_NoPlatform;
    [Test]
    procedure Parse_CompileTask_CustomConfig;
    [Test]
    procedure Parse_CompileTask_CustomConfig_NoPlatform;
    [Test]
    procedure Parse_BuildAndRunTask_CustomConfig;
    [Test]
    procedure Parse_BuildAndRunTask_CustomConfig_NoPlatform;
    [Test]
    procedure Parse_CompileAndRunTask_CustomConfig;
    [Test]
    procedure Parse_CompileAndRunTask_CustomConfig_NoPlatform;
    [Test]
    procedure IsBuildNeeded_SearchPath;
    [Test]
    procedure IsBuildNeeded_SearchPath_NoBuildNeeded;
  end;

implementation

uses
  DPT.Types;

{ TDptBuildAndRunTaskTests }

function TDptBuildAndRunTaskTests.DprojWithConfigs(const AConfigs: array of String): String;
var
  Config: String;
  SB    : TStringBuilder;
begin
  // Minimal .dproj that only carries the <BuildConfiguration> item group, which
  // is what TDProjAnalyzer.GetConfigs reads to discover the available configs.
  SB := TStringBuilder.Create;
  try
    SB.AppendLine('<Project>');
    SB.AppendLine('  <ItemGroup>');
    for Config in AConfigs do
    begin
      SB.AppendLine('    <BuildConfiguration Include="' + Config + '">');
      SB.AppendLine('      <Key>' + Config + '</Key>');
      SB.AppendLine('    </BuildConfiguration>');
    end;
    SB.AppendLine('  </ItemGroup>');
    SB.AppendLine('</Project>');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

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

procedure TDptBuildAndRunTaskTests.CheckCustomConfigAccepted(ATaskClass: TDptBuildTaskClass; AWithPlatform: Boolean);
var
  CmdLine: TCmdLineConsumer;
  Params : TArray<String>;
  Task   : TDptBuildTask;
begin
  // A .dproj can define arbitrarily named build configurations. Every build task
  // must accept any of them as the Config argument -- not only
  // Debug/Release/FitNesse -- and must not misroute a valid config into the
  // MSBuild extra args, whether or not a platform is given.
  TFile.WriteAllText(FProjectFile, DprojWithConfigs(['Base', 'Debug', 'Release', 'ProjectBuilder']));

  if AWithPlatform then
    Params := [FProjectFile, 'Win32', 'ProjectBuilder']
  else
    Params := [FProjectFile, 'ProjectBuilder'];

  Task := ATaskClass.Create;
  CmdLine := TCmdLineConsumer.Create(Params);
  try
    Task.Parse(CmdLine);
    Assert.AreEqual('Win32', Task.TargetPlatform, 'Platform (default Win32 when omitted)');
    Assert.AreEqual('ProjectBuilder', Task.Config, 'Config should be taken from the dproj configurations');
    Assert.AreEqual('', Task.ExtraArgs, 'A valid config must not leak into MSBuild extra args');
  finally
    CmdLine.Free;
    Task.Free;
  end;
end;

procedure TDptBuildAndRunTaskTests.Parse_BuildTask_CustomConfig;
begin
  CheckCustomConfigAccepted(TDptBuildTask, True);
end;

procedure TDptBuildAndRunTaskTests.Parse_BuildTask_CustomConfig_NoPlatform;
begin
  CheckCustomConfigAccepted(TDptBuildTask, False);
end;

procedure TDptBuildAndRunTaskTests.Parse_CompileTask_CustomConfig;
begin
  CheckCustomConfigAccepted(TDptCompileTask, True);
end;

procedure TDptBuildAndRunTaskTests.Parse_CompileTask_CustomConfig_NoPlatform;
begin
  CheckCustomConfigAccepted(TDptCompileTask, False);
end;

procedure TDptBuildAndRunTaskTests.Parse_BuildAndRunTask_CustomConfig;
begin
  CheckCustomConfigAccepted(TDptBuildAndRunTask, True);
end;

procedure TDptBuildAndRunTaskTests.Parse_BuildAndRunTask_CustomConfig_NoPlatform;
begin
  CheckCustomConfigAccepted(TDptBuildAndRunTask, False);
end;

procedure TDptBuildAndRunTaskTests.Parse_CompileAndRunTask_CustomConfig;
begin
  CheckCustomConfigAccepted(TDptCompileAndRunTask, True);
end;

procedure TDptBuildAndRunTaskTests.Parse_CompileAndRunTask_CustomConfig_NoPlatform;
begin
  CheckCustomConfigAccepted(TDptCompileAndRunTask, False);
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
