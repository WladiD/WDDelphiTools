// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.RegisterPackage.Task;

interface

uses

  System.SysUtils,

  JclIDEUtils,

  DPT.Types;

type

  TDptRegisterPackageTask = class(TDptTaskBase)
  public
    PathToBPL: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

  TDptIsPackageRegisteredTask = class(TDptTaskBase)
  public
    PackageFileName: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

{ TDptRegisterPackageTask }

procedure TDptRegisterPackageTask.Parse(CmdLine: TCmdLineConsumer);
begin
  PathToBPL := CmdLine.CheckParameter('PathToBPL');
  CmdLine.ConsumeParameter;
  Writeln(Format('Register design time package "%s"...', [PathToBPL]));
end;

procedure TDptRegisterPackageTask.Execute;
begin
  Installation.RegisterPackage(PathToBPL, '');
end;

{ TDptIsPackageRegisteredTask }

procedure TDptIsPackageRegisteredTask.Parse(CmdLine: TCmdLineConsumer);
begin
  PackageFileName := CmdLine.CheckParameter('PackageFileName');
  CmdLine.ConsumeParameter;
  Writeln(Format('Checking if package "%s" is registered...', [PackageFileName]));
end;

procedure TDptIsPackageRegisteredTask.Execute;
var
  Found     : Boolean;
  Loop      : Integer;
  Pkg       : String;
  TargetName: String;
begin
  Found := False;
  TargetName := ChangeFileExt(ExtractFileName(PackageFileName), '');

  for Loop := 0 to Installation.IdePackages.Count[IsX64] - 1 do
  begin
    Pkg := Installation.IdePackages.PackageFileNames[Loop, IsX64];
    if SameText(ChangeFileExt(ExtractFileName(Pkg), ''), TargetName) then
    begin
      Found := True;
      Break;
    end;
  end;

  if Found then
    Writeln('Package is registered.')
  else
  begin
    Writeln('Package is NOT registered.');
    System.ExitCode := 1;
  end;
end;

end.
