// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Detection;

interface

uses

  Winapi.TlHelp32,
  Winapi.Windows,

  System.StrUtils,
  System.SysUtils,

  JclIDEUtils,

  DPT.Types;

function FindMostRecentDelphiVersion: TDelphiVersion;
function IsValidDelphiVersion(VersionString: String; out DelphiVersion: TDelphiVersion): Boolean;
function IsLatestVersionAlias(const AValue: String): Boolean;

type

  TProcessTreeScanner = class
  private
    FSnapshot: THandle;
    function GetProcessEntry(AID: DWORD; out AEntry: TProcessEntry32): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetProcessName(AID: DWORD): string;
    function GetParentProcessID(AID: DWORD): DWORD;
    function DetectAIMode(out AHostPID: DWORD): TAIMode;
  end;

function DetectAIMode(out AHostPID: DWORD): TAIMode; overload;
function DetectAIMode: TAIMode; overload;

implementation

function DetectAIMode(out AHostPID: DWORD): TAIMode;
var
  Scanner: TProcessTreeScanner;
begin
  AHostPID := 0;

  Scanner := TProcessTreeScanner.Create;
  try
    Result := Scanner.DetectAIMode(AHostPID);
    
    // Fallback if environment says Gemini but traversal failed to find node.exe
    if (Result = amNone) and (GetEnvironmentVariable('GEMINI_CLI') = '1') then
      Result := amGemini;
  finally
    Scanner.Free;
  end;
end;

function DetectAIMode: TAIMode;
var
  DummyPID: DWORD;
begin
  Result := DetectAIMode(DummyPID);
end;

function IsValidDelphiVersion(VersionString: String; out DelphiVersion: TDelphiVersion): Boolean;
begin
  Result := True;
  for var Loop: Integer := 1 to Integer(High(TDelphiVersion)) do
  begin
    DelphiVersion := TDelphiVersion(Loop);
    if VersionString = DelphiVersionStringArray[DelphiVersion] then
      Exit;
  end;
  DelphiVersion := dvUnknown;
  Result := False;
end;

function FindMostRecentDelphiVersion: TDelphiVersion;
var
  Installations: TJclBorRADToolInstallations;
begin
  Result := dvUnknown;
  Installations := TJclBorRADToolInstallations.Create;
  try
    for var Loop: Integer := Integer(High(TDelphiVersion)) downto 1 do
    begin
      if Installations.DelphiVersionInstalled[DelphiVersionIntegerArray[TDelphiVersion(Loop)]] then
      begin
        Result := TDelphiVersion(Loop);
        Break;
      end;
    end;
  finally
    Installations.Free;
  end;
end;

function IsLatestVersionAlias(const AValue: String): Boolean;
begin
  Result := SameText(AValue, 'LATEST') or SameText(AValue, 'RECENT');
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

function TProcessTreeScanner.DetectAIMode(out AHostPID: DWORD): TAIMode;
var
  CurrentPID: DWORD;
  ProcessName: string;
begin
  Result := amNone;
  AHostPID := 0;
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
      AHostPID := CurrentPID;
      Break;
    end
    else if SameText(ProcessName, 'node.exe') then
    begin
      Result := amGemini;
      AHostPID := CurrentPID;
      Break;
    end;
  end;
end;

end.
