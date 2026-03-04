// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Git;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TDptMockRunCommandFunc = reference to function(const ACommand, ADirectory: string; out AOutput: string): Integer;

  TDptGit = class
  public
    // For testing purposes
    class var MockRunCommand: TDptMockRunCommandFunc;
    
    /// <summary>
    /// Returns a list of absolute file paths for files that are modified or newly added
    /// to the git repository starting from the specified directory.
    /// This includes unstaged, staged, and untracked files.
    /// </summary>
    class function GetModifiedFiles(const ADirectory: string): TArray<string>;
    class function RunCommand(const ACommand, ADirectory: string; out AOutput: string): Integer;
  end;

implementation

uses
  Winapi.Windows,
  System.IOUtils,
  DPT.Detection;

{ TDptGit }

class function TDptGit.RunCommand(const ACommand, ADirectory: string; out AOutput: string): Integer;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: DWORD;
  Cmd: string;
  CurrDir: PChar;
begin
  Result := -1;
  AOutput := '';

  with SA do
  begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    SetHandleInformation(StdOutPipeRead, HANDLE_FLAG_INHERIT, 0);

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    Cmd := 'cmd.exe /c "' + ACommand + '"';
    UniqueString(Cmd);

    if ADirectory <> '' then
      CurrDir := PChar(ADirectory)
    else
      CurrDir := nil;

    WasOK := CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil, CurrDir, SI, PI);
    
    // Write pipe handle must be closed before reading, otherwise we deadlock
    CloseHandle(StdOutPipeWrite);
    StdOutPipeWrite := 0;

    if WasOK then
    begin
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            AOutput := AOutput + string(AnsiString(Buffer));
          end;
        until not WasOK or (BytesRead = 0);
        
        WaitForSingleObject(PI.hProcess, INFINITE);
        GetExitCodeProcess(PI.hProcess, DWORD(Result));
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
    end;
  finally
    if StdOutPipeWrite <> 0 then CloseHandle(StdOutPipeWrite);
    CloseHandle(StdOutPipeRead);
  end;
end;

class function TDptGit.GetModifiedFiles(const ADirectory: string): TArray<string>;
var
  LOutputStr: string;
  LOutputLines: TStringList;
  I: Integer;
  LStatusLine: string;
  LFilePathShort: string;
  LAbsolutePath: string;
  LResultList: TStringList;
begin
  SetLength(Result, 0);
  
  LOutputStr := '';
  if Assigned(MockRunCommand) then
  begin
    if MockRunCommand('git status --porcelain -uall', ADirectory, LOutputStr) <> 0 then
      Exit;
  end
  else
  begin
    if RunCommand('git status --porcelain -uall', ADirectory, LOutputStr) <> 0 then
      Exit; // Not a git repository or git not available
  end;

  if Trim(LOutputStr) = '' then
    Exit;

  LOutputLines := TStringList.Create;
  LResultList := TStringList.Create;
  try
    LOutputLines.Text := LOutputStr;
    for I := 0 to LOutputLines.Count - 1 do
    begin
      LStatusLine := LOutputLines[I];
      if Length(LStatusLine) < 4 then
        Continue;

      // Porcelain status lines are typically "XY filename"
      // Wait, there are cases where quotes are used if there are spaces.

      // Quick parsing: Remove the first three characters (status codes + space)
      LFilePathShort := Copy(LStatusLine, 4, MaxInt);
      
      // Remove quotes if present
      if (Length(LFilePathShort) > 0) and (LFilePathShort[1] = '"') and (LFilePathShort[Length(LFilePathShort)] = '"') then
        LFilePathShort := Copy(LFilePathShort, 2, Length(LFilePathShort) - 2);

      // Path is relative to the root of the repo. 
      // To correctly build the full path, it's safer to use git rev-parse to get the root dir, 
      // but 'git status --porcelain' usually returns paths relative to the current dir if run inside the repo, 
      // unless specified otherwise. Let's force it relative to the root or resolve correctly.
      
      var LRootDirStr: string;
      LRootDirStr := '';
      var LCmdRet: Integer;
      if Assigned(MockRunCommand) then
        LCmdRet := MockRunCommand('git rev-parse --show-toplevel', ADirectory, LRootDirStr)
      else
        LCmdRet := RunCommand('git rev-parse --show-toplevel', ADirectory, LRootDirStr);
        
      if LCmdRet = 0 then
      begin
        LRootDirStr := Trim(LRootDirStr);
        // Replace forward slashes with system specific if necessary
        LRootDirStr := StringReplace(LRootDirStr, '/', '\', [rfReplaceAll]);
        
        LAbsolutePath := TPath.Combine(LRootDirStr, LFilePathShort);
        
        // Let's normalize it to have consistent cases and slashes
        LAbsolutePath := StringReplace(LAbsolutePath, '/', '\', [rfReplaceAll]);
        
        // Add only files, skip directories if they somehow sneak in
        if TFile.Exists(LAbsolutePath) then
          LResultList.Add(LAbsolutePath);
      end;
    end;
    
    SetLength(Result, LResultList.Count);
    for I := 0 to LResultList.Count - 1 do
      Result[I] := LResultList[I];
      
  finally
    LResultList.Free;
    LOutputLines.Free;
  end;
end;

end.
