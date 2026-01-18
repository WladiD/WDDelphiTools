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
  Winapi.Winsock,

  System.Classes,
  System.SysUtils,
  System.RegularExpressions,

  DPT.IdeClient,
  DPT.Types;

type

  TDPOpenUnitTask = class(TDPTaskBase)
  private
    function  GetBdsProcessPath(ProcessID: DWORD): String;
    function  GetListeningPorts(ProcessID: DWORD): TArray<Integer>;
    function  IsBdsRunning(const BinPath: String; out FoundWnd: HWND; out FoundPID: DWORD): Boolean;
    procedure WaitForInputIdleOrTimeout(ProcessHandle: THandle; Timeout: DWORD);
  public
    FullPathToUnit      : String;
    GoToLine            : Integer;
    MemberImplementation: String;
    procedure Execute; override;
  end;

implementation

const

  TCP_TABLE_OWNER_PID_LISTENER = 3;
  AF_INET = 2;

type
  PMibTcpRowOwnerPid = ^TMibTcpRowOwnerPid;
  TMibTcpRowOwnerPid = record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid: DWORD;
  end;

  PMibTcpTableOwnerPid = ^TMibTcpTableOwnerPid;
  TMibTcpTableOwnerPid = record
    dwNumEntries: DWORD;
    table: array [0..0] of TMibTcpRowOwnerPid;
  end;

function GetExtendedTcpTable(pTcpTable: Pointer; var pdwSize: DWORD; bOrder: BOOL; ulAf: ULONG; TableClass: Integer; Reserved: ULONG): DWORD; stdcall; external 'iphlpapi.dll';

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
    constructor Create(PID: DWORD);
  end;

{ TEnumContext }

constructor TEnumContext.Create(PID: DWORD);
begin
  TargetPID := PID;
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

      SetLength(Context.Candidates, Length(Context.Candidates) + 1);
      Context.Candidates[High(Context.Candidates)] := Info;
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

function TDPOpenUnitTask.GetListeningPorts(ProcessID: DWORD): TArray<Integer>;
var
  Size   : DWORD;
  Table  : PMibTcpTableOwnerPid;
  Res    : DWORD;
  Row    : PMibTcpRowOwnerPid;
begin
  Result := nil;
  Size := 0;
  GetExtendedTcpTable(nil, Size, False, AF_INET, TCP_TABLE_OWNER_PID_LISTENER, 0);

  if Size = 0 then Exit;

  GetMem(Table, Size);
  try
    Res := GetExtendedTcpTable(Table, Size, False, AF_INET, TCP_TABLE_OWNER_PID_LISTENER, 0);
    if Res <> NO_ERROR then
      Exit;

    for var Loop: Integer := 0 to Table.dwNumEntries - 1 do
    begin
      Row := PMibTcpRowOwnerPid(PByte(@Table.table[0]) + (Loop * SizeOf(TMibTcpRowOwnerPid)));
      if Row.dwOwningPid = ProcessID then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := ntohs(Row.dwLocalPort);
      end;
    end;
  finally
    FreeMem(Table);
  end;
end;

function TDPOpenUnitTask.IsBdsRunning(const BinPath: String; out FoundWnd: HWND; out FoundPID: DWORD): Boolean;
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
  FoundPID   : DWORD;
  FoundWnd   : HWND;
  IsEn       : Boolean;
  IsVis      : Boolean;
  ProcessInfo: TProcessInformation;
  Retries    : Integer;
  Title      : array[0..255] of Char;
  IdeClient  : TDptIdeClient;
  ListeningPorts: TArray<Integer>;
  Port       : Integer;

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

  // Try via IDE Plugin (Slim Server) - First Attempt with default logic
  IdeClient := TDptIdeClient.Create;
  try
    if IdeClient.TryOpenUnit(DelphiVersion, FullPathToUnit, GoToLine) then
    begin
      Writeln('Successfully opened unit via IDE Plugin.');
      Exit;
    end;
  finally
    IdeClient.Free;
  end;

  Writeln('IDE Plugin not reachable via standard port. Checking IDE status...');

  BinPath := Installation.BinFolderName;
  BdsExe := IncludeTrailingPathDelimiter(BinPath) + 'bds.exe';

  Writeln('Checking for running BDS instance...');

  if not IsBdsRunning(BinPath, FoundWnd, FoundPID) then
  begin
    LaunchBDS;

    // Poll for the window
    Retries := 0;
    repeat
      Sleep(1000);
      IsBdsRunning(BinPath, FoundWnd, FoundPID);
      Inc(Retries);
      Write('.');
    until (FoundWnd <> 0) or (Retries > 60);
    Writeln;
  end
  else
    Writeln('BDS is already running (PID: ' + IntToStr(FoundPID) + ').');

  Writeln('Waiting for main window to become visible and enabled...');
  Retries := 0;
  repeat
    if not IsBdsRunning(BinPath, FoundWnd, FoundPID) then
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

  // Give it a bit more time to settle
  Sleep(2000);

  if IsIconic(FoundWnd) then
    ShowWindow(FoundWnd, SW_RESTORE);
  SetForegroundWindow(FoundWnd);

  Writeln('IDE is ready. Scanning for listening Slim ports (9000-9100) on PID ' + IntToStr(FoundPID) + '...');

  IdeClient := TDptIdeClient.Create;
  try
    // Retry loop: expert might need time to start server
    for Retries := 1 to 30 do
    begin
       ListeningPorts := GetListeningPorts(FoundPID);
       for Port in ListeningPorts do
       begin
         if (Port >= 9000) and (Port <= 9100) then
         begin
           Writeln(Format(' Found candidate port %d. Trying to connect...', [Port]));
           if IdeClient.TryOpenUnitOnPort(Port, FullPathToUnit, GoToLine) then
           begin
             Writeln('Successfully opened unit via IDE Plugin.');
             Exit;
           end;
         end;
       end;

       Sleep(1000);
       Write('.');
    end;
  finally
    IdeClient.Free;
  end;

  Writeln;
  raise Exception.Create('Failed to connect to IDE Plugin (Slim Server). No matching listening port found or connection rejected.');
end;

end.
