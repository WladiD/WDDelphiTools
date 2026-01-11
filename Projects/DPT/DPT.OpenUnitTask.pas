unit DPT.OpenUnitTask;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  Winapi.TlHelp32,
  Winapi.PsAPI,
  System.SysUtils,
  System.Classes,
  Generics.Collections,
  DPT.Types,
  SendInputHelper;

type
  TDPOpenUnitTask = class(TDPTaskBase)
  private
    function IsBdsRunning(const BinPath: string; out FoundWnd: HWND): Boolean;
    function GetBdsProcessPath(ProcessID: DWORD): string;
    procedure WaitForInputIdleOrTimeout(ProcessHandle: THandle; Timeout: DWORD);
  public
    FullPathToUnit: string;
    GoToLine: Integer;

    procedure Execute; override;
  end;

implementation

type
  TWindowInfo = record
    Handle: HWND;
    PID: DWORD;
    IsVisible: Boolean;
    IsEnabled: Boolean;
    Title: string;
  end;

  TEnumContext = class
    TargetPID: DWORD;
    Candidates: TList<TWindowInfo>;
    constructor Create(PID: DWORD);
    destructor Destroy; override;
  end;

{ TEnumContext }

constructor TEnumContext.Create(PID: DWORD);
begin
  TargetPID := PID;
  Candidates := TList<TWindowInfo>.Create;
end;

destructor TEnumContext.Destroy;
begin
  Candidates.Free;
  inherited;
end;

function EnumWindowsProc(Handle: HWND; LParam: LPARAM): BOOL; stdcall;
var
  ProcID: DWORD;
  ClassName: array[0..255] of Char;
  TitleBuf: array[0..255] of Char;
  Context: TEnumContext;
  Info: TWindowInfo;
begin
  Context := TEnumContext(LParam);
  GetWindowThreadProcessId(Handle, @ProcID);

  if ProcID = Context.TargetPID then
  begin
    GetClassName(Handle, ClassName, 255);
    if SameText(ClassName, 'TAppBuilder') then
    begin
      Info.Handle := Handle;
      Info.PID := ProcID;
      Info.IsVisible := IsWindowVisible(Handle);
      Info.IsEnabled := IsWindowEnabled(Handle);
      GetWindowText(Handle, TitleBuf, 255);
      Info.Title := TitleBuf;

      Context.Candidates.Add(Info);
    end;
  end;
  Result := True;
end;

{ TDPOpenUnitTask }

function TDPOpenUnitTask.GetBdsProcessPath(ProcessID: DWORD): string;
var
  ProcessHandle: THandle;
  Buffer: array[0..MAX_PATH] of Char;
begin
  Result := '';
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);
  if ProcessHandle <> 0 then
  begin
    try
      if GetModuleFileNameEx(ProcessHandle, 0, Buffer, MAX_PATH) > 0 then
        Result := Buffer;
    finally
      CloseHandle(ProcessHandle);
    end;
  end;
end;

function TDPOpenUnitTask.IsBdsRunning(const BinPath: string; out FoundWnd: HWND): Boolean;
var
  Snapshot: THandle;
  Entry: TProcessEntry32;
  FullProcessPath: string;
  TargetBdsExe: string;
  FoundPID: DWORD;
  Context: TEnumContext;
  I: Integer;
  BestMatchHandle: HWND;
begin
  Result := False;
  FoundWnd := 0;
  FoundPID := 0;
  TargetBdsExe := UpperCase(IncludeTrailingPathDelimiter(BinPath) + 'bds.exe');

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
          FoundPID := Entry.th32ProcessID;
          Result := True;
          Break;
        end;
      end;
    until not Process32Next(Snapshot, Entry);
  finally
    CloseHandle(Snapshot);
  end;

  if Result and (FoundPID > 0) then
  begin
    Context := TEnumContext.Create(FoundPID);
    try
      EnumWindows(@EnumWindowsProc, LPARAM(Context));

      if Context.Candidates.Count > 0 then
      begin
        BestMatchHandle := 0;

        // Strategy:
        // 1. Visible and Enabled
        for I := 0 to Context.Candidates.Count - 1 do
          if Context.Candidates[I].IsVisible and Context.Candidates[I].IsEnabled then
          begin
            BestMatchHandle := Context.Candidates[I].Handle;
            Break;
          end;

        // 2. Visible
        if BestMatchHandle = 0 then
          for I := 0 to Context.Candidates.Count - 1 do
            if Context.Candidates[I].IsVisible then
            begin
              BestMatchHandle := Context.Candidates[I].Handle;
              Break;
            end;

        // 3. Any
        if BestMatchHandle = 0 then
           BestMatchHandle := Context.Candidates[0].Handle;

        FoundWnd := BestMatchHandle;
      end;
    finally
      Context.Free;
    end;
  end;
end;

procedure TDPOpenUnitTask.WaitForInputIdleOrTimeout(ProcessHandle: THandle; Timeout: DWORD);
begin
  if WaitForInputIdle(ProcessHandle, Timeout) = WAIT_FAILED then
    Sleep(Timeout); // Fallback
end;

procedure TDPOpenUnitTask.Execute;
var
  BinPath: string;
  BdsExe: string;
  FoundWnd: HWND;
  SI: TSendInputHelper;
  Retries: Integer;
  IsVis, IsEn: Boolean;
  Title: array[0..255] of Char;
  ProcessInfo: TProcessInformation;

  procedure LaunchBDS;
  var
    StartupInfo: TStartupInfo;
  begin
    Writeln('Starting BDS: ' + BdsExe);
    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    StartupInfo.cb := SizeOf(StartupInfo);

    if not CreateProcess(PChar(BdsExe), nil, nil, nil, False, 0, nil, PChar(BinPath), StartupInfo, ProcessInfo) then
      raise Exception.Create('Failed to start BDS: ' + SysErrorMessage(GetLastError));

    CloseHandle(ProcessInfo.hThread);

    Writeln('Waiting for BDS to become ready...');
    WaitForInputIdleOrTimeout(ProcessInfo.hProcess, 60000); // Wait up to 60s for init
    CloseHandle(ProcessInfo.hProcess);
  end;

begin
  BinPath := Installation.BinFolderName;
  BdsExe := IncludeTrailingPathDelimiter(BinPath) + 'bds.exe';

  Writeln('Checking for running BDS instance...');

  if not IsBdsRunning(BinPath, FoundWnd) then
  begin
    LaunchBDS;

    // Poll for the window
    Retries := 0;
    repeat
      Sleep(1000);
      IsBdsRunning(BinPath, FoundWnd);
      Inc(Retries);
      Write('.');
    until (FoundWnd <> 0) or (Retries > 60);
    Writeln;
  end
  else
    Writeln('BDS is already running.');

  Writeln('Waiting for main window to become visible and enabled...');
  Retries := 0;
  repeat
    if not IsBdsRunning(BinPath, FoundWnd) then
    begin
       Writeln;
       Writeln('BDS process is gone (terminated?). Restarting...');
       LaunchBDS;
       Retries := 0; // Reset timeout
       FoundWnd := 0;
    end;

    IsVis := False;
    IsEn := False;
    Title[0] := #0;

    if FoundWnd <> 0 then
    begin
      IsVis := IsWindowVisible(FoundWnd);
      IsEn := IsWindowEnabled(FoundWnd);
      GetWindowText(FoundWnd, Title, 255);

      if IsVis and IsEn then 
      begin
        Writeln(Format(' Window %d is ready: "%s"', [FoundWnd, Title]));
        Break;
      end;
    end;

    Sleep(1000);
    Inc(Retries);
    Write('.');
    if (Retries mod 5) = 0 then
       Write(Format('[Handle:%d Vis:%s En:%s Title:"%s"]', [FoundWnd, BoolToStr(IsVis, True), BoolToStr(IsEn, True), Title]));

  until Retries > 120; // Wait up to 2 mins
  Writeln;

  if FoundWnd = 0 then
      raise Exception.Create('BDS main window could not be found.');

  if not IsWindowVisible(FoundWnd) then
    Writeln('Warning: Main window still not visible after timeout.');
  if not IsWindowEnabled(FoundWnd) then
    Writeln('Warning: Main window still not enabled after timeout (Splash screen stuck? Modal dialog open?).');

  // Give it a bit more time to settle (drawing UI etc)
  Sleep(2000);

  if IsIconic(FoundWnd) then
    ShowWindow(FoundWnd, SW_RESTORE);
  SetForegroundWindow(FoundWnd);
  // Sometimes a second kick is needed if it was deep in background
  Sleep(200);
  SetForegroundWindow(FoundWnd);

  Writeln('Sending input to open unit...');
  SI := TSendInputHelper.Create;
  try
    // Alt + d (Datei)
    SI.AddShortCut([ssAlt], 'd');
    SI.AddDelay(500);

    // f (Öffnen)
    SI.AddChar('f');
    SI.AddDelay(500);

    // Now we expect the Open File Dialog.
    // Type path
    SI.AddText(FullPathToUnit);
    SI.AddDelay(500);

    // Enter
    SI.AddVirtualKey(VK_RETURN);

    // Wait for file to open
    SI.AddDelay(2000);

    if GoToLine > 0 then
    begin
       // Alt + g (GoTo)
       SI.AddShortCut([ssAlt], 'g');
       SI.AddDelay(500);

       // Type Line Number
       SI.AddText(IntToStr(GoToLine));
       SI.AddDelay(200);

       // Enter
       SI.AddVirtualKey(VK_RETURN);
    end;

    SI.Flush;
    Writeln('Done.');
  finally
    SI.Free;
  end;
end;

end.
