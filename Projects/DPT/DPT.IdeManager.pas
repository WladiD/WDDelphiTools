// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.IdeManager;

interface

uses

  Winapi.Messages,
  Winapi.PsAPI,
  Winapi.TlHelp32,
  Winapi.Windows,

  System.Classes,
  System.SysUtils;

type

  TDptIdeManager = class
  private
    class function  GetBdsProcessPath(AProcessID: DWORD): String;
    class procedure WaitForInputIdleOrTimeout(AProcessHandle: THandle; ATimeout: DWORD);
  public
    class procedure BringToFront(AWnd: HWND);
    class function  IsBdsRunning(const ABinPath: String; out AFoundWnd: HWND; out AFoundPID: DWORD): Boolean;
    class procedure StartIDE(const ABinPath: String; out AProcessInfo: TProcessInformation);
    class function  TerminateIDE(const ABinPath: String): Boolean;
    class procedure WaitForIDE(const ABinPath: String; out AFoundWnd: HWND; out AFoundPID: DWORD);
  end;

implementation

type

  TWindowInfo = record
    Handle   : HWND;
    IsEnabled: Boolean;
    IsVisible: Boolean;
    PID      : DWORD;
    Title    : String;
  end;

  TEnumContext = class
  public
    Candidates: TArray<TWindowInfo>;
    TargetPID : DWORD;
    constructor Create(APID: DWORD);
  end;

{ TEnumContext }

constructor TEnumContext.Create(APID: DWORD);
begin
  TargetPID := APID;
end;

function EnumWindowsProc(AHandle: HWND; ALParam: LPARAM): BOOL; stdcall;
var
  ClassName: Array[0..255] of Char;
  Context  : TEnumContext;
  Info     : TWindowInfo;
  ProcID   : DWORD;
  TitleBuf : Array[0..255] of Char;
begin
  Context := TEnumContext(ALParam);
  GetWindowThreadProcessId(AHandle, @ProcID);

  if ProcID = Context.TargetPID then
  begin
    GetClassName(AHandle, ClassName, 255);
    if SameText(ClassName, 'TAppBuilder') then
    begin
      Info.Handle := AHandle;
      Info.PID := ProcID;
      Info.IsVisible := IsWindowVisible(AHandle);
      Info.IsEnabled := IsWindowEnabled(AHandle);
      GetWindowText(AHandle, TitleBuf, 255);
      Info.Title := TitleBuf;

      SetLength(Context.Candidates, Length(Context.Candidates) + 1);
      Context.Candidates[High(Context.Candidates)] := Info;
    end;
  end;
  Result := True;
end;

{ TDptIdeManager }

class function TDptIdeManager.GetBdsProcessPath(AProcessID: DWORD): String;
var
  Buffer       : Array[0..MAX_PATH] of Char;
  ProcessHandle: THandle;
begin
  Result := '';
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessID);
  if ProcessHandle = 0 then
    Exit;

  try
    if GetModuleFileNameEx(ProcessHandle, 0, Buffer, MAX_PATH) > 0 then
      Result := Buffer;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

class function TDptIdeManager.IsBdsRunning(const ABinPath: String; out AFoundWnd: HWND; out AFoundPID: DWORD): Boolean;
var
  BestMatchHandle: HWND;
  Context        : TEnumContext;
  Entry          : TProcessEntry32;
  FullProcessPath: String;
  I              : Integer;
  Snapshot       : THandle;
  TargetBdsExe   : String;
begin
  Result := False;
  AFoundWnd := 0;
  AFoundPID := 0;
  TargetBdsExe := UpperCase(IncludeTrailingPathDelimiter(ABinPath) + 'bds.exe');

  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snapshot = INVALID_HANDLE_VALUE then
    Exit;

  try
    Entry.dwSize := SizeOf(Entry);
    if Process32First(Snapshot, Entry) then
    repeat
      if SameText(Entry.szExeFile, 'bds.exe') then
      begin
        FullProcessPath := UpperCase(GetBdsProcessPath(Entry.th32ProcessID));
        if FullProcessPath = TargetBdsExe then
        begin
          AFoundPID := Entry.th32ProcessID;
          Result := True;
          Break;
        end;
      end;
    until not Process32Next(Snapshot, Entry);
  finally
    CloseHandle(Snapshot);
  end;

  if Result and (AFoundPID > 0) then
  begin
    Context := TEnumContext.Create(AFoundPID);
    try
      EnumWindows(@EnumWindowsProc, LPARAM(Context));

      if Length(Context.Candidates) > 0 then
      begin
        BestMatchHandle := 0;

        // Strategy:
        // 1. Visible and Enabled
        for I := 0 to Length(Context.Candidates) - 1 do
          if Context.Candidates[I].IsVisible and Context.Candidates[I].IsEnabled then
          begin
            BestMatchHandle := Context.Candidates[I].Handle;
            Break;
          end;

        // 2. Visible
        if BestMatchHandle = 0 then
          for I := 0 to Length(Context.Candidates) - 1 do
            if Context.Candidates[I].IsVisible then
            begin
              BestMatchHandle := Context.Candidates[I].Handle;
              Break;
            end;

        // 3. Any
        if BestMatchHandle = 0 then
           BestMatchHandle := Context.Candidates[0].Handle;

        AFoundWnd := BestMatchHandle;
      end;
    finally
      Context.Free;
    end;
  end;
end;

class procedure TDptIdeManager.WaitForInputIdleOrTimeout(AProcessHandle: THandle; ATimeout: DWORD);
begin
  if WaitForInputIdle(AProcessHandle, ATimeout) = WAIT_FAILED then
    Sleep(ATimeout); // Fallback
end;

class procedure TDptIdeManager.StartIDE(const ABinPath: String; out AProcessInfo: TProcessInformation);
var
  BdsExe     : String;
  StartupInfo: TStartupInfo;
begin
  BdsExe := IncludeTrailingPathDelimiter(ABinPath) + 'bds.exe';
  Writeln('Starting BDS: ' + BdsExe);

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);

  if not CreateProcess(PChar(BdsExe), nil, nil, nil, False, 0, nil, PChar(ABinPath), StartupInfo, AProcessInfo) then
    raise Exception.Create('Failed to start BDS: ' + SysErrorMessage(GetLastError));

  CloseHandle(AProcessInfo.hThread);

  Writeln('Waiting for BDS to become ready...');
  WaitForInputIdleOrTimeout(AProcessInfo.hProcess, 60000); // Wait up to 60s for init
end;

class procedure TDptIdeManager.BringToFront(AWnd: HWND);
var
  Attach      : Boolean;
  CurThreadID : DWORD;
  FgThreadID  : DWORD;
begin
  if AWnd = 0 then
    Exit;

  CurThreadID := GetCurrentThreadId;
  FgThreadID := GetWindowThreadProcessId(GetForegroundWindow, nil);
  Attach := CurThreadID <> FgThreadID;

  if Attach then
    AttachThreadInput(FgThreadID, CurThreadID, True);
  try
    if IsIconic(AWnd) then
      ShowWindow(AWnd, SW_RESTORE)
    else
      ShowWindow(AWnd, SW_SHOW);
    SetForegroundWindow(AWnd);
    BringWindowToTop(AWnd);
  finally
    if Attach then
      AttachThreadInput(FgThreadID, CurThreadID, False);
  end;
end;

class procedure TDptIdeManager.WaitForIDE(const ABinPath: String; out AFoundWnd: HWND; out AFoundPID: DWORD);
var
  IsEn       : Boolean;
  IsVis      : Boolean;
  ProcessInfo: TProcessInformation;
  Retries    : Integer;
  Title      : Array[0..255] of Char;
begin
  Writeln('Waiting for main window to become visible and enabled...');
  Retries := 0;
  repeat
    if not IsBdsRunning(ABinPath, AFoundWnd, AFoundPID) then
    begin
       Writeln;
       Writeln('BDS process is gone (terminated?). Restarting...');
       StartIDE(ABinPath, ProcessInfo);
       CloseHandle(ProcessInfo.hProcess);
       Retries := 0; // Reset timeout
       AFoundWnd := 0;
    end;

    IsVis := False;
    IsEn := False;
    Title[0] := #0;

    if AFoundWnd <> 0 then
    begin
      IsVis := IsWindowVisible(AFoundWnd);
      IsEn := IsWindowEnabled(AFoundWnd);
      GetWindowText(AFoundWnd, Title, 255);

      if IsVis and IsEn then
      begin
        Writeln(Format(' Window %d is ready: "%s"', [AFoundWnd, Title]));
        Break;
      end;
    end;

    Sleep(1000);
    Inc(Retries);
    Write('.');
    if (Retries mod 5) = 0 then
       Write(Format('[Handle:%d Vis:%s En:%s Title:"%s"]',
         [AFoundWnd, BoolToStr(IsVis, True), BoolToStr(IsEn, True), Title]));
  until Retries > 120; // Wait up to 2 mins

  Writeln;
  if AFoundWnd = 0 then
    raise Exception.Create('BDS main window could not be found.');
  Sleep(2000); // Give it a bit more time to settle
end;

class function TDptIdeManager.TerminateIDE(const ABinPath: String): Boolean;
var
  FoundPID     : DWORD;
  FoundWnd     : HWND;
  ProcessHandle: THandle;
begin
  Result := False;
  if IsBdsRunning(ABinPath, FoundWnd, FoundPID) then
  begin
    Writeln(Format('Terminating BDS (PID: %d)...', [FoundPID]));
    ProcessHandle := OpenProcess(PROCESS_TERMINATE, False, FoundPID);
    if ProcessHandle <> 0 then
    begin
      try
        Result := TerminateProcess(ProcessHandle, 0);
        if Result then
          Writeln('Success.')
        else
          Writeln('Failed: ' + SysErrorMessage(GetLastError));
      finally
        CloseHandle(ProcessHandle);
      end
    end
    else
      Writeln('Failed to open process: ' + SysErrorMessage(GetLastError));
  end
  else
    Writeln('BDS is not running.');
end;

end.
