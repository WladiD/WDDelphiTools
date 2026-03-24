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
  end;

implementation

uses
  DPT.Types;

{ TDptBuildAndRunTaskTests }

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

end.
