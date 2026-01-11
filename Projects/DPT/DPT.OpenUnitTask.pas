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
    function IsMenuMode(WindowHandle: HWND): Boolean;
    function WaitForOpenDialog(OwnerPID: DWORD; TimeoutMs: DWORD): Boolean;
    function WaitForWindowCaptionContains(WindowHandle: HWND; const SubText: string; TimeoutMs: DWORD): Boolean;
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

type
  TDialogSearchContext = record
    TargetPID: DWORD;
    ResultHandle: HWND;
  end;
  PDialogSearchContext = ^TDialogSearchContext;

function EnumDialogSearchProc(Handle: HWND; LParam: LPARAM): BOOL; stdcall;
var
  Ctx: PDialogSearchContext;
  ProcID: DWORD;
  ClassName: array[0..255] of Char;
begin
  Ctx := PDialogSearchContext(LParam);
  GetWindowThreadProcessId(Handle, @ProcID);
  
  if ProcID = Ctx.TargetPID then
  begin
    GetClassName(Handle, ClassName, 255);
    // Standard Windows Dialog class is #32770
    if SameText(ClassName, '#32770') and IsWindowVisible(Handle) then
    begin
      Ctx.ResultHandle := Handle;
      Result := False; // Stop enumeration
      Exit;
    end;
  end;
  Result := True;
end;

function EnumMenuSearchProc(Handle: HWND; LParam: LPARAM): BOOL; stdcall;
var
  Ctx: PDialogSearchContext; // Reusing the context type
  ProcID: DWORD;
  ClassName: array[0..255] of Char;
begin
  Ctx := PDialogSearchContext(LParam);
  GetWindowThreadProcessId(Handle, @ProcID);
  
  if ProcID = Ctx.TargetPID then
  begin
    GetClassName(Handle, ClassName, 255);
    // TIDEStylePopupMenu is used in newer IDEs
    if IsWindowVisible(Handle) and 
       (SameText(ClassName, 'TIDEStylePopupMenu') or 
        SameText(ClassName, '#32768') or // Standard Menu
        (Pos('Popup', ClassName) > 0)) then 
    begin
      Ctx.ResultHandle := Handle;
      Result := False; // Stop enumeration
      Exit;
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

function TDPOpenUnitTask.IsMenuMode(WindowHandle: HWND): Boolean;
var
  ThreadID, ProcessID: DWORD;
  Ctx: TDialogSearchContext;
begin
  if WindowHandle = 0 then Exit(False);
  
  ThreadID := GetWindowThreadProcessId(WindowHandle, @ProcessID);
  
  Ctx.TargetPID := ProcessID;
  Ctx.ResultHandle := 0;
  
  EnumWindows(@EnumMenuSearchProc, LPARAM(@Ctx));
  
  Result := Ctx.ResultHandle <> 0;
end;

function TDPOpenUnitTask.WaitForOpenDialog(OwnerPID: DWORD; TimeoutMs: DWORD): Boolean;
var
  StartTime: DWORD;
  Ctx: TDialogSearchContext;
begin
  Result := False;
  StartTime := GetTickCount;
  Ctx.TargetPID := OwnerPID;
  
  repeat
    Ctx.ResultHandle := 0;
    EnumWindows(@EnumDialogSearchProc, LPARAM(@Ctx));
    
    if Ctx.ResultHandle <> 0 then
      Exit(True);
      
    Sleep(100);
  until (GetTickCount - StartTime) > TimeoutMs;
end;

function TDPOpenUnitTask.WaitForWindowCaptionContains(WindowHandle: HWND; const SubText: string; TimeoutMs: DWORD): Boolean;
var
  StartTime: DWORD;
  Title: array[0..255] of Char;
  CurrentTitle: string;
begin
  Result := False;
  StartTime := GetTickCount;

  repeat
    Title[0] := #0;
    GetWindowText(WindowHandle, Title, 255);
    CurrentTitle := Title;

    if Pos(LowerCase(SubText), LowerCase(CurrentTitle)) > 0 then
      Exit(True);

    Sleep(100);
  until (GetTickCount - StartTime) > TimeoutMs;
end;

procedure TDPOpenUnitTask.Execute;
var
  BinPath: string;
  BdsExe: string;
  FoundWnd: HWND;
  FoundPID: DWORD;
  SI: TSendInputHelper;
  Retries: Integer;
  IsVis, IsEn: Boolean;
  Title: array[0..255] of Char;
  ProcessInfo: TProcessInformation;
  I: Integer;
  MenuOpened: Boolean;
  UnitName: string;

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

  // Get PID for later dialog check
  GetWindowThreadProcessId(FoundWnd, @FoundPID);

  // Give it a bit more time to settle (drawing UI etc)
  Sleep(2000);

  if IsIconic(FoundWnd) then
    ShowWindow(FoundWnd, SW_RESTORE);
  SetForegroundWindow(FoundWnd);
  Sleep(200);
  SetForegroundWindow(FoundWnd);

  Writeln('Sending input to open unit...');
  SI := TSendInputHelper.Create;
  try
    // Step 1: Open Menu (Alt+d)
    MenuOpened := False;
    for I := 1 to 3 do
    begin
      SI.AddShortCut([ssAlt], 'd');
      SI.Flush;
      
      Sleep(500); // Wait for menu animation/processing
      
      if IsMenuMode(FoundWnd) then
      begin
        MenuOpened := True;
        Break;
      end;
      Writeln(Format(' Retry %d: "File" menu not detected...', [I]));
      
      // Retry strategy: Maybe focus was lost? Click on title bar? 
      // For now just ensure foreground and retry
      SetForegroundWindow(FoundWnd);
      Sleep(200);
    end;
    
    if not MenuOpened then
      Writeln('Warning: Failed to confirm "File" menu open state (Alt+d). Continuing anyway...');

    // Step 2: Open Dialog (f)
    SI.AddChar('f');
    SI.Flush;

    Writeln('Waiting for "Open File" dialog...');
    if not WaitForOpenDialog(FoundPID, 5000) then
      raise Exception.Create('Open File dialog did not appear within timeout.');

    // Step 3: Type Path
    SI.AddText(FullPathToUnit, True);
    SI.Flush;

    // Wait for file to open (check caption)
    UnitName := ChangeFileExt(ExtractFileName(FullPathToUnit), '');
    Writeln(Format('Waiting for unit "%s" to appear in caption...', [UnitName]));

    if not WaitForWindowCaptionContains(FoundWnd, UnitName, 10000) then // 10s timeout
      Writeln('Warning: Unit name did not appear in caption (maybe it opened too fast or caption behavior changed).');

    // Step 4: GoTo Line
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

       SI.Flush;
    end;

    Writeln('Done.');
  finally
    SI.Free;
  end;
end;

end.