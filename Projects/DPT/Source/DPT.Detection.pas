// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Detection;

interface

uses

  Winapi.Windows,
  Winapi.TlHelp32,
  System.SysUtils,
  System.StrUtils;

type

  TAIMode = (amNone, amCursor, amGemini);

  TProcessTreeScanner = class
  private
    FSnapshot: THandle;
    function GetProcessEntry(AID: DWORD; out AEntry: TProcessEntry32): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetProcessName(AID: DWORD): string;
    function GetParentProcessID(AID: DWORD): DWORD;
    function DetectAIMode: TAIMode;
  end;

function DetectAIMode: TAIMode;

implementation

function DetectAIMode: TAIMode;
var
  Scanner: TProcessTreeScanner;
begin
  // First priority: Environment variable (reliable for Gemini CLI)
  if GetEnvironmentVariable('GEMINI_CLI') = '1' then
    Exit(amGemini);

  // Second priority: Process tree traversal (for Cursor or fallback)
  Scanner := TProcessTreeScanner.Create;
  try
    Result := Scanner.DetectAIMode;
  finally
    Scanner.Free;
  end;
end;

{ TProcessTreeScanner }

constructor TProcessTreeScanner.Create;
begin
  inherited Create;
  FSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
end;

destructor TProcessTreeScanner.Destroy;
begin
  if FSnapshot <> INVALID_HANDLE_VALUE then
    CloseHandle(FSnapshot);
  inherited Destroy;
end;

function TProcessTreeScanner.GetProcessEntry(AID: DWORD; out AEntry: TProcessEntry32): Boolean;
begin
  Result := False;
  if FSnapshot = INVALID_HANDLE_VALUE then
    Exit;

  AEntry.dwSize := SizeOf(AEntry);
  if Process32First(FSnapshot, AEntry) then
  repeat
    if AEntry.th32ProcessID = AID then
    begin
      Result := True;
      Break;
    end;
  until not Process32Next(FSnapshot, AEntry);
end;

function TProcessTreeScanner.GetParentProcessID(AID: DWORD): DWORD;
var
  Entry: TProcessEntry32;
begin
  if GetProcessEntry(AID, Entry) then
    Result := Entry.th32ParentProcessID
  else
    Result := 0;
end;

function TProcessTreeScanner.GetProcessName(AID: DWORD): string;
var
  Entry: TProcessEntry32;
begin
  if GetProcessEntry(AID, Entry) then
    Result := Entry.szExeFile
  else
    Result := '';
end;

function TProcessTreeScanner.DetectAIMode: TAIMode;
var
  CurrentPID: DWORD;
  ProcessName: string;
begin
  Result := amNone;
  CurrentPID := GetCurrentProcessId;
  
  // Traverse up the process tree
  while (CurrentPID <> 0) and (CurrentPID <> 4) do // 4 is System process
  begin
    CurrentPID := GetParentProcessID(CurrentPID);
    if CurrentPID = 0 then
      Break;

    ProcessName := GetProcessName(CurrentPID);
    
    if SameText(ProcessName, 'Cursor.exe') then
    begin
      Result := amCursor;
      Break;
    end
    else if SameText(ProcessName, 'node.exe') then
    begin
      Result := amGemini;
      Break;
    end;
  end;
end;

end.
