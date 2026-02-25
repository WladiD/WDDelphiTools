unit DPT.Debugger.PoC.Task;

interface

uses
  System.SysUtils,
  DPT.Types,
  DPT.Task,
  DPT.Debugger;

type
  TDptDebugPoCTask = class(TDptTaskBase)
  private
    FExecutablePath: string;
  public
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

{ TDptDebugPoCTask }

procedure TDptDebugPoCTask.Parse(CmdLine: TCmdLineConsumer);
begin
  inherited;
  FExecutablePath := CmdLine.CheckParameter('ExecutablePath');
  CmdLine.ConsumeParameter;
end;

procedure TDptDebugPoCTask.Execute;
var
  Debugger: TDebugger;
  MapFile: string;
begin
  if not FileExists(FExecutablePath) then
    raise Exception.Create('Executable not found: ' + FExecutablePath);

  Debugger := TDebugger.Create;
  try
    MapFile := ChangeFileExt(FExecutablePath, '.map');
    if FileExists(MapFile) then
    begin
      Writeln('Loading Map file: ' + MapFile);
      Debugger.LoadMapFile(MapFile);
    end;

    // Set a test breakpoint in FileVersion.dpr at line 13 (Writeln in ShowUsage)
    Debugger.SetBreakpoint('FileVersion.dpr', 13);

    Writeln('Starting debugger...');
    Debugger.StartDebugging(FExecutablePath);
  finally
    Debugger.Free;
  end;
end;

end.