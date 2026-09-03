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

  DPT.Types;

function FindMostRecentDelphiVersion: TDelphiVersion;
function IsValidDelphiVersion(VersionString: String; out DelphiVersion: TDelphiVersion): Boolean;
function IsLatestVersionAlias(const AValue: String): Boolean;

type

  TProcessTreeScanner = class
  private
    FSnapshot: THandle;
  protected
    function GetProcessEntry(AID: DWORD; out AEntry: TProcessEntry32): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function GetProcessName(AID: DWORD): string;
    function GetParentProcessID(AID: DWORD): DWORD;
    function DetectAIMode(out AHostPID: DWORD): TAIMode; overload;
    function DetectAIMode(AStartPID: DWORD; out AHostPID: DWORD): TAIMode; overload;
  end;

function DetectAIMode(out AHostPID: DWORD): TAIMode; overload;
function DetectAIMode: TAIMode; overload;

implementation

uses

  System.StrUtils,
  System.SysUtils,

  JclIDEUtils;

function DetectAIMode(out AHostPID: DWORD): TAIMode;
var
  Scanner: TProcessTreeScanner;
begin
  AHostPID := 0;

  Scanner := TProcessTreeScanner.Create;
  try
    Result := Scanner.DetectAIMode(AHostPID);
    
    // Fallback if the environment names the agent but the traversal failed to
    // find its host process (e.g. DPT started through a launcher script)
    if (Result = amNone) and (GetEnvironmentVariable('GEMINI_CLI') = '1') then
      Result := amGemini
    else if (Result = amNone) and (GetEnvironmentVariable('CLAUDECODE') = '1') then
      Result := amClaude;
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

/// <summary>
///   Looks up one process of the snapshot. Virtual so that tests can
///   substitute a synthetic process table for the live Toolhelp snapshot.
/// </summary>
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
begin
  Result := DetectAIMode(GetCurrentProcessId, AHostPID);
end;

/// <summary>
///   Walks the parent chain starting at <paramref name="AStartPID"/> and reports
///   the first known AI host (Cursor.exe, node.exe, claude.exe) it meets.
///   Claude Code ships as a native executable, so unlike the Gemini CLI it is
///   not covered by the node.exe check.
/// </summary>
/// <remarks>
///   The parent PIDs in a Toolhelp snapshot are not guaranteed to form a tree:
///   a process keeps reporting the PID of a parent that has long exited, and
///   Windows may hand that PID to a new process - possibly one of the orphan's
///   own descendants (observed: wininit.exe -> services.exe -> wininit.exe,
///   GitHub issue #17). Without a guard the walk spins forever, so every PID
///   already seen ends the walk, and a depth cap acts as a second safety net.
/// </remarks>
function TProcessTreeScanner.DetectAIMode(AStartPID: DWORD; out AHostPID: DWORD): TAIMode;
const
  // A genuine chain is a dozen processes deep at most.
  MaxDepth = 64;
var
  CurrentPID: DWORD;
  Depth: Integer;
  ProcessName: string;
  Visited: array[0..MaxDepth - 1] of DWORD;

  function AlreadyVisited(APID: DWORD): Boolean;
  begin
    for var Loop := 0 to Depth - 1 do
      if Visited[Loop] = APID then
        Exit(True);
    Result := False;
  end;

begin
  Result := amNone;
  AHostPID := 0;
  CurrentPID := AStartPID;
  Depth := 0;

  // Traverse up the process tree
  while (CurrentPID <> 0) and (CurrentPID <> 4) and (Depth < MaxDepth) do // 4 is System process
  begin
    Visited[Depth] := CurrentPID;
    Inc(Depth);

    CurrentPID := GetParentProcessID(CurrentPID);
    if (CurrentPID = 0) or AlreadyVisited(CurrentPID) then
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
    end
    else if SameText(ProcessName, 'claude.exe') then
    begin
      Result := amClaude;
      AHostPID := CurrentPID;
      Break;
    end;
  end;
end;

end.
