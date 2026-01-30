// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.IdeControl.Task;

interface

uses

  Winapi.Windows,

  System.SysUtils,

  JclIDEUtils,

  DPT.IdeManager,
  DPT.Types;

type

  TDptStartTask = class(TDptTaskBase)
  public
    procedure Execute; override;
  end;

  TDptStopTask = class(TDptTaskBase)
  public
    procedure Execute; override;
  end;

implementation

{ TDptStartTask }

procedure TDptStartTask.Execute;
var
  BinPath    : String;
  FoundWnd   : HWND;
  FoundPID   : DWORD;
  ProcessInfo: TProcessInformation;
begin
  BinPath := Installation.BinFolderName;

  if TDptIdeManager.IsBdsRunning(BinPath, FoundWnd, FoundPID) then
  begin
    Writeln('IDE is already running.');
    TDptIdeManager.BringToFront(FoundWnd);
  end
  else
  begin
    TDptIdeManager.StartIDE(BinPath, ProcessInfo);
    CloseHandle(ProcessInfo.hProcess);
    TDptIdeManager.WaitForIDE(BinPath, FoundWnd, FoundPID);
    TDptIdeManager.BringToFront(FoundWnd);
    Writeln('IDE started and ready.');
  end;
end;

{ TDptStopTask }

procedure TDptStopTask.Execute;
var
  BinPath: String;
begin
  BinPath := Installation.BinFolderName;
  if TDptIdeManager.TerminateIDE(BinPath) then
    Writeln('IDE stopped.')
  else
    Writeln('IDE was not running or could not be stopped.');
end;

end.
