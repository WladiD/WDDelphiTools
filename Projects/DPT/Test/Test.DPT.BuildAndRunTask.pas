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

end.
