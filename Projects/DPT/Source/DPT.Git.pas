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

  TDptMockRunCommandFunc = reference to function(const ACommand, ADirectory: String; out AOutput: String): Integer;

  TDptGit = class
  public
    class var MockRunCommand: TDptMockRunCommandFunc; // For testing purposes
    class function GetModifiedFiles(const ADirectory: String): TArray<String>;
    class function RunCommand(const ACommand, ADirectory: String; out AOutput: String): Integer;
  end;

implementation

uses

  Winapi.Windows,

  System.IOUtils,

  DPT.Detection;

{ TDptGit }

class function TDptGit.RunCommand(const ACommand, ADirectory: String; out AOutput: String): Integer;
var
  Buffer         : Array[0..255] of AnsiChar;
  BytesRead      : DWORD;
  Cmd            : String;
  CurrDir        : PChar;
  PI             : TProcessInformation;
  SA             : TSecurityAttributes;
  SI             : TStartupInfo;
  StdOutPipeRead : THandle;
  StdOutPipeWrite: THandle;
  WasOK          : Boolean;
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
            AOutput := AOutput + string(PAnsiChar(@Buffer));
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

/// <summary>
/// Returns a list of absolute file paths for files that are modified or newly added
/// to the git repository starting from the specified directory.
/// This includes unstaged, staged, and untracked files.
/// </summary>
class function TDptGit.GetModifiedFiles(const ADirectory: String): TArray<String>;
var
  OutputStr    : String;
  OutputLines  : TStringList;
  I            : Integer;
  StatusLine   : String;
  FilePathShort: String;
  AbsolutePath : String;
  ResultList   : TStringList;
  LRootDirStr  : string;
  LCmdRet      : Integer;
begin
  SetLength(Result, 0);
  
  OutputStr := '';
  if Assigned(MockRunCommand) then
  begin
    if MockRunCommand('git -c core.quotePath=false status --porcelain -uall', ADirectory, OutputStr) <> 0 then
      Exit;
  end
  else
  begin
    if RunCommand('git -c core.quotePath=false status --porcelain -uall', ADirectory, OutputStr) <> 0 then
      Exit; // Not a git repository or git not available
  end;

  if Trim(OutputStr) = '' then
    Exit;

  // Retrieve root directory once
  LRootDirStr := '';
  if Assigned(MockRunCommand) then
    LCmdRet := MockRunCommand('git rev-parse --show-toplevel', ADirectory, LRootDirStr)
  else
    LCmdRet := RunCommand('git rev-parse --show-toplevel', ADirectory, LRootDirStr);

  if LCmdRet <> 0 then
    Exit;

  LRootDirStr := Trim(LRootDirStr);
  LRootDirStr := StringReplace(LRootDirStr, '/', '\', [rfReplaceAll]);

  OutputLines := TStringList.Create;
  ResultList := TStringList.Create;
  try
    OutputLines.Text := OutputStr;
    for I := 0 to OutputLines.Count - 1 do
    begin
      StatusLine := OutputLines[I];
      if Length(StatusLine) < 4 then
        Continue;

      // Quick parsing: Remove the first three characters (status codes + space)
      FilePathShort := Copy(StatusLine, 4, MaxInt);

      AbsolutePath := TPath.Combine(LRootDirStr, FilePathShort);

      // Let's normalize it to have consistent cases and slashes
      AbsolutePath := StringReplace(AbsolutePath, '/', '\', [rfReplaceAll]);

      // Add only files, skip directories if they somehow sneak in
      if TFile.Exists(AbsolutePath) then
        ResultList.Add(AbsolutePath);
    end;
    
    SetLength(Result, ResultList.Count);
    for I := 0 to ResultList.Count - 1 do
      Result[I] := ResultList[I];
      
  finally
    ResultList.Free;
    OutputLines.Free;
  end;
end;

end.
