program MultiThreadTarget;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs;

var
  Ev1, Ev2: TEvent;
  Thread1Started, Thread1Done: TEvent;

type
  TMyThread1 = class(TThread)
  protected
    procedure Execute; override;
  end;

  TMyThread2 = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TMyThread1.Execute;
var
  A: Integer;
begin
  Thread1Started.SetEvent;
  Ev1.WaitFor(INFINITE); // Wait for signal from main thread
  A := 10;               // Breakpoint 1 (Line 29)
  Writeln('Thread 1 doing work: ', A);
  Thread1Done.SetEvent;
end;

procedure TMyThread2.Execute;
var
  B: Integer;
begin
  Thread1Started.WaitFor(INFINITE);
  Ev2.WaitFor(INFINITE); // Wait for signal from main thread
  Thread1Done.WaitFor(INFINITE); // Wait for Thread 1 to finish its BP
  B := 20;               // Breakpoint 2 (Line 40)
  Writeln('Thread 2 doing work: ', B);
end;

var
  T1: TMyThread1;
  T2: TMyThread2;
begin
  try
    Ev1 := TEvent.Create(nil, True, False, '');
    Ev2 := TEvent.Create(nil, True, False, '');
    Thread1Started := TEvent.Create(nil, True, False, '');
    Thread1Done := TEvent.Create(nil, True, False, '');

    T1 := TMyThread1.Create(True);
    T1.FreeOnTerminate := False;
    
    T2 := TMyThread2.Create(True);
    T2.FreeOnTerminate := False;

    T1.Start;
    T2.Start;

    Ev1.SetEvent; // Let thread 1 hit its breakpoint
    Ev2.SetEvent; // Let thread 2 proceed to wait for Thread 1

    T1.WaitFor;
    T2.WaitFor;

    T1.Free;
    T2.Free;
    Ev1.Free;
    Ev2.Free;
    Thread1Started.Free;
    Thread1Done.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
