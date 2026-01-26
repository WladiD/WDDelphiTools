unit Test.DptBuildAndRunTask;

interface

uses

  System.Classes,
  System.SysUtils,
  System.IOUtils,

  DUnitX.TestFramework,

  DPT.Tasks;

type

  // Stub class to access protected method
  TStubDptBuildAndRunTask = class(TDptBuildAndRunTask);

  [TestFixture]
  TDptBuildAndRunTaskTests = class
  private
    FProjectFile: String;
    FTask       : TStubDptBuildAndRunTask;
    FTempDir    : String;
    procedure CreateProjectFile(const Content: String);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure FindExeFile_Default;
    [Test]
    procedure FindExeFile_SimpleOutput;
    [Test]
    procedure FindExeFile_ConfigCondition;
    [Test]
    procedure FindExeFile_PlatformCondition;
    [Test]
    procedure FindExeFile_Variables;
    [Test]
    procedure FindExeFile_MultipleGroups;
  end;

implementation

{ TDptBuildAndRunTaskTests }

procedure TDptBuildAndRunTaskTests.Setup;
begin
  FTask := TStubDptBuildAndRunTask.Create;
  FTempDir := TPath.Combine(TPath.GetTempPath, TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempDir);
  FProjectFile := TPath.Combine(FTempDir, 'TestProject.dproj');
  FTask.ProjectFile := FProjectFile;
end;

procedure TDptBuildAndRunTaskTests.TearDown;
begin
  FTask.Free;
  if TDirectory.Exists(FTempDir) then
    TDirectory.Delete(FTempDir, True);
end;

procedure TDptBuildAndRunTaskTests.CreateProjectFile(const Content: String);
begin
  TFile.WriteAllText(FProjectFile, Content);
end;

procedure TDptBuildAndRunTaskTests.FindExeFile_Default;
begin
  CreateProjectFile('<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003"></Project>');
  FTask.Config := 'Debug';
  FTask.TargetPlatform := 'Win32';

  // Default: ProjectDir + ProjectName.exe
  Assert.AreEqual(ExpandFileName(TPath.Combine(FTempDir, 'TestProject.exe')), FTask.FindExeFile);
end;

procedure TDptBuildAndRunTaskTests.FindExeFile_SimpleOutput;
begin
  CreateProjectFile('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_ExeOutput>.\Bin</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');
  FTask.Config := 'Debug';
  FTask.TargetPlatform := 'Win32';

  Assert.AreEqual(ExpandFileName(TPath.Combine(FTempDir, 'Bin\TestProject.exe')), FTask.FindExeFile);
end;

procedure TDptBuildAndRunTaskTests.FindExeFile_ConfigCondition;
begin
  CreateProjectFile('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup Condition="'$(Config)'=='Debug'">
        <DCC_ExeOutput>.\DebugBin</DCC_ExeOutput>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Config)'=='Release'">
        <DCC_ExeOutput>.\ReleaseBin</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');

  FTask.Config := 'Debug';
  FTask.TargetPlatform := 'Win32';
  Assert.AreEqual(ExpandFileName(TPath.Combine(FTempDir, 'DebugBin\TestProject.exe')), FTask.FindExeFile);

  FTask.Config := 'Release';
  Assert.AreEqual(ExpandFileName(TPath.Combine(FTempDir, 'ReleaseBin\TestProject.exe')), FTask.FindExeFile);
end;

procedure TDptBuildAndRunTaskTests.FindExeFile_PlatformCondition;
begin
  CreateProjectFile('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup Condition="'$(Platform)'=='Win32'">
        <DCC_ExeOutput>.\Win32Bin</DCC_ExeOutput>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Platform)'=='Win64'">
        <DCC_ExeOutput>.\Win64Bin</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');

  FTask.Config := 'Debug';
  FTask.TargetPlatform := 'Win32';
  Assert.AreEqual(ExpandFileName(TPath.Combine(FTempDir, 'Win32Bin\TestProject.exe')), FTask.FindExeFile);

  FTask.TargetPlatform := 'Win64';
  Assert.AreEqual(ExpandFileName(TPath.Combine(FTempDir, 'Win64Bin\TestProject.exe')), FTask.FindExeFile);
end;

procedure TDptBuildAndRunTaskTests.FindExeFile_Variables;
begin
  CreateProjectFile('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_ExeOutput>.\$(Platform)\$(Config)</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
  ''');

  FTask.Config := 'Debug';
  FTask.TargetPlatform := 'Win32';
  Assert.AreEqual(ExpandFileName(TPath.Combine(FTempDir, 'Win32\Debug\TestProject.exe')), FTask.FindExeFile);
end;

procedure TDptBuildAndRunTaskTests.FindExeFile_MultipleGroups;
begin
  // Last match wins logic test
  CreateProjectFile('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_ExeOutput>.\Base</DCC_ExeOutput>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Config)'=='Debug'">
        <DCC_ExeOutput>.\Debug</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');

  FTask.Config := 'Debug';
  FTask.TargetPlatform := 'Win32';
  Assert.AreEqual(ExpandFileName(TPath.Combine(FTempDir, 'Debug\TestProject.exe')), FTask.FindExeFile);

  FTask.Config := 'Release';
  // Release doesn't match second group, so it should keep "Base" from first group
  Assert.AreEqual(ExpandFileName(TPath.Combine(FTempDir, 'Base\TestProject.exe')), FTask.FindExeFile);
end;

end.
