// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.OpenUnitTask;

interface

uses

  Winapi.Messages,
  Winapi.PsAPI,
  Winapi.ShellAPI,
  Winapi.TlHelp32,
  Winapi.Windows,

  Generics.Collections,
  System.Classes,
  System.SysUtils,
  System.RegularExpressions,

  SendInputHelper,

  DPT.Types;

type

  TDPOpenUnitTask = class(TDPTaskBase)
  private
    function  GetBdsProcessPath(ProcessID: DWORD): String;
    function  IsBdsRunning(const BinPath: String; out FoundWnd: HWND): Boolean;
    function  IsMenuMode(WindowHandle: HWND): Boolean;
    procedure WaitForInputIdleOrTimeout(ProcessHandle: THandle; Timeout: DWORD);
    function  WaitForOpenDialog(OwnerPID: DWORD; TimeoutMs: DWORD): Boolean;
    function  WaitForWindowCaptionContains(WindowHandle: HWND; const SubText: String; TimeoutMs: DWORD): Boolean;
  public
    FullPathToUnit      : String;
    GoToLine            : Integer;
    MemberImplementation: String;
    procedure Execute; override;
  end;

implementation

type

  TOpenStrategy = record
    Name    : String;
    MenuVK  : Word;
    OpenChar: Char;
    constructor Create(const AName: String; AMenuVK: Word; AOpenChar: Char);
  end;

  TWindowInfo = record
    Handle   : HWND;
    IsEnabled: Boolean;
    IsVisible: Boolean;
    PID      : DWORD;
    Title    : String;
  end;

  TEnumContext = class
  public
    Candidates: TList<TWindowInfo>;
    TargetPID : DWORD;
    constructor Create(PID: DWORD);
    destructor Destroy; override;
  end;

{ TOpenStrategy }

constructor TOpenStrategy.Create(const AName: String; AMenuVK: Word; AOpenChar: Char);
begin
  Name := AName;
  MenuVK := AMenuVK;
  OpenChar := AOpenChar;
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
  ProcID   : DWORD;
  ClassName: Array[0..255] of Char;
  TitleBuf : Array[0..255] of Char;
  Context  : TEnumContext;
  Info     : TWindowInfo;
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
    ResultHandle: HWND;
    TargetPID   : DWORD;
  end;
  PDialogSearchContext = ^TDialogSearchContext;

function EnumDialogSearchProc(Handle: HWND; LParam: LPARAM): BOOL; stdcall;
var
  ClassName: Array[0..255] of Char;
  Ctx      : PDialogSearchContext;
  ProcID   : DWORD;
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
  ClassName: Array[0..255] of Char;
  Ctx      : PDialogSearchContext;
  ProcID   : DWORD;
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

function TDPOpenUnitTask.GetBdsProcessPath(ProcessID: DWORD): String;
var
  Buffer       : Array[0..MAX_PATH] of Char;
  ProcessHandle: THandle;
begin
  Result := '';
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);
  if ProcessHandle = 0 then
    Exit;

  try
    if GetModuleFileNameEx(ProcessHandle, 0, Buffer, MAX_PATH) > 0 then
      Result := Buffer;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

function TDPOpenUnitTask.IsBdsRunning(const BinPath: String; out FoundWnd: HWND): Boolean;
var
  BestMatchHandle: HWND;
  Context        : TEnumContext;
  Entry          : TProcessEntry32;
  FoundPID       : DWORD;
  FullProcessPath: String;
  I              : Integer;
  Snapshot       : THandle;
  TargetBdsExe   : String;
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
  Ctx      : TDialogSearchContext;
  ProcessID: DWORD;
  ThreadID : DWORD;
begin
  if WindowHandle = 0 then
    Exit(False);

  ThreadID := GetWindowThreadProcessId(WindowHandle, @ProcessID);
  Ctx.TargetPID := ProcessID;
  Ctx.ResultHandle := 0;
  EnumWindows(@EnumMenuSearchProc, LPARAM(@Ctx));
  Result := Ctx.ResultHandle <> 0;
end;

function TDPOpenUnitTask.WaitForOpenDialog(OwnerPID: DWORD; TimeoutMs: DWORD): Boolean;
var
  Ctx      : TDialogSearchContext;
  StartTime: DWORD;
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

function TDPOpenUnitTask.WaitForWindowCaptionContains(WindowHandle: HWND; const SubText: String; TimeoutMs: DWORD): Boolean;
var
  CurrentTitle: String;
  StartTime   : DWORD;
  Title       : Array[0..255] of Char;
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

function FindImplementationLine(const FileName, MemberName: String): Integer;
var
  Lines    : TStringList;
  ImplPos  : Integer;
  LineLoop : Integer;
  Pattern  : String;
begin
  Result := 0;
  if (MemberName = '') or not FileExists(FileName) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);

    // Step 1: Find 'implementation'
    ImplPos := -1;
    for LineLoop := 0 to Lines.Count - 1 do
    begin
      if TRegEx.IsMatch(Lines[LineLoop], '^\s*implementation\b', [roIgnoreCase]) then
      begin
        ImplPos := LineLoop;
        Break;
      end;
    end;

    if ImplPos = -1 then
      Exit;

    // Step 2: Find Class.Member
    Pattern := Format('^\s*(class\s+)?(procedure|function|constructor|destructor)\s+%s\b', [MemberName.Replace('.', '\.')]);

    for LineLoop := ImplPos + 1 to Lines.Count - 1 do
    begin
       if TRegEx.IsMatch(Lines[LineLoop], Pattern, [roIgnoreCase]) then
       begin
         Result := LineLoop + 1; // 1-based line number for IDE
         Exit;
       end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TDPOpenUnitTask.Execute;
var
  BdsExe     : String;
  BinPath    : String;
  DialogFound: Boolean;
  FoundPID   : DWORD;
  FoundWnd   : HWND;
  I          : Integer;
  IsEn       : Boolean;
  IsVis      : Boolean;
  MenuOpened : Boolean;
  ProcessInfo: TProcessInformation;
  Retries    : Integer;
  SI         : TSendInputHelper;
  Strategies : TArray<TOpenStrategy>;
  Strategy   : TOpenStrategy;
  Title      : array[0..255] of Char;
  UnitName   : String;

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
  if (GoToLine <= 0) and (MemberImplementation <> '') then
  begin
    GoToLine := FindImplementationLine(FullPathToUnit, MemberImplementation);
    if GoToLine > 0 then
      Writeln(Format('Found member "%s" at line %d.', [MemberImplementation, GoToLine]))
    else
      Writeln(Format('Warning: Member "%s" not found in implementation.', [MemberImplementation]));
  end;

  BinPath := Installation.BinFolderName;
  BdsExe := IncludeTrailingPathDelimiter(BinPath) + 'bds.exe';

  // Define Strategies:
  // 1. German: Alt+d (Datei) -> f (Öffnen)
  // 2. English: Alt+f (File) -> o (Open)
  Strategies := [
    TOpenStrategy.Create('German (Alt+d -> f)', Ord('D'), 'f'),
    TOpenStrategy.Create('English (Alt+f -> o)', Ord('F'), 'o')
  ];

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
    DialogFound := False;

    for Strategy in Strategies do
    begin
      Writeln(Format('Trying strategy: %s...', [Strategy.Name]));

      // Step 1: Open Menu
      MenuOpened := False;
      for I := 1 to 3 do
      begin
        SI.AddShortCut([ssAlt], Char(Strategy.MenuVK)); // Use MenuVK from strategy
        SI.Flush;

        Sleep(500);

        if IsMenuMode(FoundWnd) then
        begin
          MenuOpened := True;
          Break;
        end;
        Writeln(Format(' Retry %d: Menu not detected...', [I]));
        SetForegroundWindow(FoundWnd);
        Sleep(200);
      end;

      if MenuOpened then
      begin
        // Step 2: Open Dialog
        SI.AddChar(Strategy.OpenChar);
        SI.Flush;

        // Check if Dialog appeared
        if WaitForOpenDialog(FoundPID, 2000) then // Wait up to 2s for dialog
        begin
          Writeln(' "Open File" dialog detected.');
          DialogFound := True;
          Break; // Success!
        end
        else
        begin
          Writeln(' Dialog not detected. Aborting strategy...');
          // Cancel Menu (Escape)
          SI.AddVirtualKey(VK_ESCAPE);
          SI.Flush;
          Sleep(500);
        end;
      end;
    end;

    if not DialogFound then
      raise Exception.Create('Failed to open "Open File" dialog with any strategy.');

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
