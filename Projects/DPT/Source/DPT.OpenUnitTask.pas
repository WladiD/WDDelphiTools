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
  DPT.IdeManager,
  DPT.Task,
  DPT.Types;

type

  TDptOpenUnitTask = class(TDptTaskBase)
  private
    function  GetListeningPorts(ProcessID: DWORD): TArray<Integer>;
  public
    FullPathToUnit      : String;
    GoToLine            : Integer;
    MemberImplementation: String;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
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

{ TDptOpenUnitTask }

procedure TDptOpenUnitTask.Parse(CmdLine: TCmdLineConsumer);
var
  NextParam: String;
begin
  FullPathToUnit := CmdLine.CheckParameter('FullPathToUnit');
  CmdLine.ConsumeParameter; // Consume file path
  
  while CmdLine.HasParameter do
  begin
     NextParam := CmdLine.CheckParameter('Optional: GoToLine / GoToMemberImplementation');
     if SameText(NextParam, 'GoToLine') then
     begin
       CmdLine.ConsumeParameter; // Consume 'GoToLine' keyword
       GoToLine := StrToIntDef(CmdLine.CheckParameter('LineNumber'), 0);
       CmdLine.ConsumeParameter; // Consume line number
     end
     else if SameText(NextParam, 'GoToMemberImplementation') then
     begin
       CmdLine.ConsumeParameter; // Consume keyword
       MemberImplementation := CmdLine.CheckParameter('MemberName');
       CmdLine.ConsumeParameter; // Consume value
     end
     else
       Break;
  end;
end;

function TDptOpenUnitTask.GetListeningPorts(ProcessID: DWORD): TArray<Integer>;
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

procedure TDptOpenUnitTask.Execute;
var
  BinPath    : String;
  FoundPID   : DWORD;
  FoundWnd   : HWND;
  ProcessInfo: TProcessInformation;
  Retries    : Integer;
  IdeClient  : TDptIdeClient;
  ListeningPorts: TArray<Integer>;
  Port       : Integer;
begin
  BinPath := Installation.BinFolderName;

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
      // Ensure IDE is brought to front even if plugin handled the request
      if TDptIdeManager.IsBdsRunning(BinPath, FoundWnd, FoundPID) then
        TDptIdeManager.BringToFront(FoundWnd);
      Exit;
    end;
  finally
    IdeClient.Free;
  end;

  Writeln('IDE Plugin not reachable via standard port. Checking IDE status...');

  Writeln('Checking for running BDS instance...');

  if not TDptIdeManager.IsBdsRunning(BinPath, FoundWnd, FoundPID) then
  begin
    TDptIdeManager.StartIDE(BinPath, ProcessInfo);
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    Writeln('BDS is already running (PID: ' + IntToStr(FoundPID) + ').');

  TDptIdeManager.WaitForIDE(BinPath, FoundWnd, FoundPID);

  TDptIdeManager.BringToFront(FoundWnd);

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
