// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.Base;

interface

uses

  Winapi.Windows,

  System.Classes,
  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  TmplCodeGen.Logger,

  Test.Logger;

type

  TTestBase = class
  protected
    FLogger  : ILogger;
    FPrevPath: String;
    FTestPath: String;
  public
    [Setup]
    procedure Setup; virtual;
    [Teardown]
    procedure Teardown; virtual;
  end;

implementation

{ TTestBase }

procedure TTestBase.Setup;
begin
  FLogger := TTestLogger.Create;
  FTestPath := TPath.Combine(TPath.GetTempPath, 'TmplCodeGenTest_' + TGuid.NewGuid.ToString);
  ForceDirectories(FTestPath);
  FPrevPath := TDirectory.GetCurrentDirectory;
  TDirectory.SetCurrentDirectory(FTestPath);
end;

procedure TTestBase.Teardown;
begin
  TDirectory.SetCurrentDirectory(FPrevPath);
  if TDirectory.Exists(FTestPath) then
    TDirectory.Delete(FTestPath, True);
  FLogger := nil;
end;

end.
