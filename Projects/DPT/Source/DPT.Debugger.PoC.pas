unit DPT.Debugger.PoC;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows;

type
  TWin32DebuggerPoC = class
  private
    FProcessId: DWORD;
    FThreadId: DWORD;
    FProcessHandle: THandle;
    FThreadHandle: THandle;
    function GetDebugEventName(EventCode: DWORD): string;
  public
    constructor Create;
    destructor Destroy; override;
    
    /// <summary>
    /// Starts the specified executable under the debugger and enters the debug loop.
    /// This method blocks until the debugged process exits.
    /// </summary>
    procedure StartDebugging(const AExecutablePath: string);
  end;

implementation

{ TWin32DebuggerPoC }

constructor TWin32DebuggerPoC.Create;
begin
  inherited Create;
  FProcessId := 0;
  FThreadId := 0;
  FProcessHandle := 0;
  FThreadHandle := 0;
end;

destructor TWin32DebuggerPoC.Destroy;
begin
  if FProcessHandle <> 0 then
    CloseHandle(FProcessHandle);
  if FThreadHandle <> 0 then
    CloseHandle(FThreadHandle);
  inherited Destroy;
end;

function TWin32DebuggerPoC.GetDebugEventName(EventCode: DWORD): string;
begin
  case EventCode of
    EXCEPTION_DEBUG_EVENT: Result := 'EXCEPTION_DEBUG_EVENT';
    CREATE_THREAD_DEBUG_EVENT: Result := 'CREATE_THREAD_DEBUG_EVENT';
    CREATE_PROCESS_DEBUG_EVENT: Result := 'CREATE_PROCESS_DEBUG_EVENT';
    EXIT_THREAD_DEBUG_EVENT: Result := 'EXIT_THREAD_DEBUG_EVENT';
    EXIT_PROCESS_DEBUG_EVENT: Result := 'EXIT_PROCESS_DEBUG_EVENT';
    LOAD_DLL_DEBUG_EVENT: Result := 'LOAD_DLL_DEBUG_EVENT';
    UNLOAD_DLL_DEBUG_EVENT: Result := 'UNLOAD_DLL_DEBUG_EVENT';
    OUTPUT_DEBUG_STRING_EVENT: Result := 'OUTPUT_DEBUG_STRING_EVENT';
    RIP_EVENT: Result := 'RIP_EVENT';
  else
    Result := 'UNKNOWN_DEBUG_EVENT (' + IntToStr(EventCode) + ')';
  end;
end;

procedure TWin32DebuggerPoC.StartDebugging(const AExecutablePath: string);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  DebugEvent: TDebugEvent;
  ContinueStatus: DWORD;
  LRunning: Boolean;
begin
  Writeln('Debugger PoC: Starting ', AExecutablePath);

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);

  // DEBUG_ONLY_THIS_PROCESS indicates that the calling process is a debugger, 
  // and the new process is being debugged.
  if not CreateProcess(
    nil, 
    PChar(AExecutablePath), 
    nil, nil, False, 
    DEBUG_ONLY_THIS_PROCESS or NORMAL_PRIORITY_CLASS, 
    nil, nil, 
    StartupInfo, ProcessInfo) then
  begin
    Writeln('Debugger PoC: Failed to create process. Error: ', GetLastError);
    Exit;
  end;

  FProcessId := ProcessInfo.dwProcessId;
  FThreadId := ProcessInfo.dwThreadId;
  FProcessHandle := ProcessInfo.hProcess;
  FThreadHandle := ProcessInfo.hThread;

  Writeln('Debugger PoC: Process started (PID: ', FProcessId, ')');
  Writeln('Debugger PoC: Entering debug loop...');

  LRunning := True;
  while LRunning do
  begin
    // Wait for a debug event
    if not WaitForDebugEvent(DebugEvent, INFINITE) then
    begin
      Writeln('Debugger PoC: WaitForDebugEvent failed. Error: ', GetLastError);
      Break;
    end;

    ContinueStatus := DBG_CONTINUE;

    Writeln('  -> Event: ', GetDebugEventName(DebugEvent.dwDebugEventCode), 
            ' (TID: ', DebugEvent.dwThreadId, ')');

    case DebugEvent.dwDebugEventCode of
      EXCEPTION_DEBUG_EVENT:
        begin
          Writeln('     Exception Code: ', IntToHex(DebugEvent.Exception.ExceptionRecord.ExceptionCode, 8));
          Writeln('     Exception Address: ', IntToHex(UIntPtr(DebugEvent.Exception.ExceptionRecord.ExceptionAddress), 8));
          
          // First chance exceptions should typically not be handled by the debugger unless we set a breakpoint.
          // DBG_EXCEPTION_NOT_HANDLED tells Windows to pass the exception to the debuggee's exception handlers.
          if DebugEvent.Exception.dwFirstChance = 1 then
          begin
            Writeln('     (First chance exception)');
            ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
          end
          else
          begin
            Writeln('     (Second chance exception / Unhandled)');
            ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
          end;
          
          // A hardware/software breakpoint was hit (int 3 = EXCEPTION_BREAKPOINT)
          if DebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_BREAKPOINT then
          begin
            Writeln('     *** BREAKPOINT HIT ***');
            // Normally here we would inspect registers, wait for user input, and then resume.
            // For PoC, we just print and continue (which resumes execution).
            // Usually, Windows inserts an INT 3 (0xCC) when the process starts, 
            // known as the initial breakpoint.
            ContinueStatus := DBG_CONTINUE; 
          end;
        end;

      CREATE_THREAD_DEBUG_EVENT:
        begin
          Writeln('     Thread Base: ', IntToHex(UIntPtr(DebugEvent.CreateThread.lpStartAddress), 8));
        end;

      CREATE_PROCESS_DEBUG_EVENT:
        begin
          Writeln('     File Handle: ', UIntPtr(DebugEvent.CreateProcessInfo.hFile));
          Writeln('     Base Image Address: ', IntToHex(UIntPtr(DebugEvent.CreateProcessInfo.lpBaseOfImage), 8));
          Writeln('     Initial Breakpoint will be triggered shortly.');
          // Remember to close the file handle when done with it, otherwise the file stays locked.
          if DebugEvent.CreateProcessInfo.hFile <> 0 then
            CloseHandle(DebugEvent.CreateProcessInfo.hFile);
        end;

      EXIT_THREAD_DEBUG_EVENT:
        begin
          Writeln('     Exit Code: ', DebugEvent.ExitThread.dwExitCode);
        end;

      EXIT_PROCESS_DEBUG_EVENT:
        begin
          Writeln('     Exit Code: ', DebugEvent.ExitProcess.dwExitCode);
          Writeln('Debugger PoC: Target process exited.');
          LRunning := False;
        end;

      LOAD_DLL_DEBUG_EVENT:
        begin
          Writeln('     Base of DLL: ', IntToHex(UIntPtr(DebugEvent.LoadDll.lpBaseOfDll), 8));
          if DebugEvent.LoadDll.hFile <> 0 then
            CloseHandle(DebugEvent.LoadDll.hFile);
        end;

      UNLOAD_DLL_DEBUG_EVENT:
        begin
          Writeln('     Base of DLL: ', IntToHex(UIntPtr(DebugEvent.UnloadDll.lpBaseOfDll), 8));
        end;

      OUTPUT_DEBUG_STRING_EVENT:
        begin
          Writeln('     Length: ', DebugEvent.DebugString.nDebugStringLength);
          // In a real debugger, you would use ReadProcessMemory to get the string from the target.
        end;
    end;

    // Resume the thread that reported the debugging event
    if not ContinueDebugEvent(DebugEvent.dwProcessId, DebugEvent.dwThreadId, ContinueStatus) then
    begin
      Writeln('Debugger PoC: ContinueDebugEvent failed. Error: ', GetLastError);
      Break;
    end;
  end;

  Writeln('Debugger PoC: Debug loop finished.');
end;

end.