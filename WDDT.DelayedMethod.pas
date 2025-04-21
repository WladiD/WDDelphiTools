unit WDDT.DelayedMethod;

interface

uses

  Winapi.Messages,
  Winapi.Windows,

  System.SysUtils,
  System.Classes,

  Vcl.Forms;


type
  TMethod = procedure of object;

  // Klasse für die einfache Verwendung von verzögernden Methoden
  //
  // Eine beliebige Methode ohne Parameter (TMethod) kann in einem Nachfolger von TComponent
  // implementiert werden und an die Klassenmethode TDelayedMethod.Execute übergeben werden.
  //
  // Der eigentliche Aufruf der verzögernden Methode erfolgt stets vom internen Timer aus, also
  // stets sauber aus dem obersten MessageHandler der Applikation.
  //
  // Eine eindeutige Methode kann stets nur einmal hinzugefügt bzw. ausgeführt werden.
  // Wurde zuvor die selbe Methode schon mal übergeben, so ersetzt der letzte Aufruf den vorherigen.
  //
  // Man kann also folgendes ausführen...
  //
  // <code>
  // for cc := 0 to 100 do
  // begin
  //   TDelayedMethod.Execute(MyObject.MethodA, 100);
  //   TDelayedMethod.Execute(MyObject.MethodB, 1000);
  // end;
  // </code>
  //
  // ...und trotzdem ist sichergestellt, dass die Methode MyObject.MethodA (nach 100ms) und
  // MyObject.MethodB (nach 1sek) nur einmal ausgeführt werden.
  //
  // Damit man sich über die Freigabe nicht kümmern braucht, wurde die Klasse von TComponent
  // abgeleitet, um vom dessen Observer-Pattern gebrauch zu nehmen.
  // Konkret heisst es: Sollte der Implementierer der Methode freigegeben werden, so ist
  // sichergestellt, dass keine seiner verzögernden Methoden ausgeführt wird und die zugehörigen
  // TDelayedMethod-Instanzen automatisch freigegeben werden.
  TDelayedMethod = class(TComponent)
  private
    FWindowHandle: HWND;
    FMethod: TMethod;
    FAnonProc: TProc;
    FAnonProcID: NativeInt;

    procedure TimerSetup(Delay: Integer);
    procedure WndProc(var Msg: TMessage);

    class function GetMethodComponent(Method: TMethod): TComponent;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    class procedure Execute(Method: TMethod; Delay: Integer = 0); overload;
    class procedure Stop(Method: TMethod); overload;

    class function Execute(AnonProc: TProc; Owner: TComponent; Delay: Integer = 0;
      AnonProcID: NativeInt = 0): NativeInt; overload;
    class procedure Stop(Owner: TComponent; AnonProcID: NativeInt); overload;
  end;

implementation

{ TDelayedMethod }

constructor TDelayedMethod.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FWindowHandle := System.Classes.AllocateHWnd(WndProc);
end;

destructor TDelayedMethod.Destroy;
begin
  KillTimer(FWindowHandle, 1);
  System.Classes.DeallocateHWnd(FWindowHandle);
  inherited Destroy;
end;

class function TDelayedMethod.GetMethodComponent(Method: TMethod): TComponent;
var
  SysMethod: System.TMethod;
begin
  Result := nil;
  if not Assigned(Method) then
    Exit;

  SysMethod := System.TMethod(Method);
  if Assigned(SysMethod.Data) and (TObject(SysMethod.Data) is TComponent) then
    Result := TComponent(SysMethod.Data);
end;

// Erstellt eine neue TDelayedMethod-Instanz mit der übergebenen Methode
//
// Der Parameter Delay gibt die Dauer der Verzögerung in ms an. Standardwert ist 0 und bedeutet,
// dass die Methode so schnell wie möglich (beim nächsten Durchlauf der Message-Queue) ausgeführt
// werden soll.
//
// Wichtig: Bitte die Beschreibung der Klasse beachten.
class procedure TDelayedMethod.Execute(Method: TMethod; Delay: Integer);
var
  Target: TComponent;
  DM: TDelayedMethod;
begin
  Target := GetMethodComponent(Method);
  if not Assigned(Target) then
    raise Exception.Create('Der Implementor der verzögernden Methode muss ein Nachfolger von TComponent sein.');

  Stop(Method);

  DM := TDelayedMethod.Create(Target);
  try
    DM.FMethod := Method;
    DM.TimerSetup(Delay);
  except
    DM.Free;
    raise
  end;
end;

// Gibt die TDelayedMethod-Instanz im Implementierer der Methode für diese Methode frei
class procedure TDelayedMethod.Stop(Method: TMethod);
var
  cc: Integer;
  TempComp: TComponent;
  Target: TComponent;
begin
  Target := GetMethodComponent(Method);
  if not Assigned(Target) then
    Exit;

  for cc := 0 to Target.ComponentCount - 1 do
  begin
    TempComp := Target.Components[cc];
    if Assigned(TempComp) and
      (TempComp is TDelayedMethod) and
      not (csDestroying in TempComp.ComponentState) and
      (@TDelayedMethod(TempComp).FMethod = @Method) then
    begin
      TempComp.Free;
      Exit; // Per se kann es nur einen Treffer geben
    end;
  end;
end;

// Erstellt eine TDelayedMethod-Instanz für die übergebene anonyme Methode
//
// Im Gegensatz zu einer klassischen Methode hat eine anonyme Methode keine implementierende
// Klasse. Daher ist die Angabe eines Owner notwendig. Die erstellte Instanz wird dem Owner
// hinzugefügt, um weiterhin vom zuverlässigen Observer-Pattern des TComponent zu
// profitieren.
// Eine weitere Eigenheit von anonymen Methoden ist, dass sie nicht zwingend eine Entität darstellt.
// D.h. eine scheinbar identische anonyme Methode hat nicht zwingend die gleiche Adresse (Pointer).
// Grob gesagt, es wird jedes mal eine neue Entität für ein und die selbe anonyme Methode erstellt,
// wenn sich der Wert der zu fangenden Variablen von einer Deklaration zur nächsten ändert.
// Aus diesem Grund kann optional eine beliebige ID im Parameter AnonProcID angegeben werden, wenn
// man sicherstellen möchte, dass diese Methode nur einmal verzögert aufgerufen werden soll.
// Wird AnonProcID nicht angegeben, wird der Pointer der Methode als ID verwendet und als Ergebnis
// zurückgeliefert.
class function TDelayedMethod.Execute(AnonProc: TProc; Owner: TComponent; Delay: Integer = 0;
  AnonProcID: NativeInt = 0): NativeInt;
var
  DM: TDelayedMethod;
begin
  if not Assigned(Owner) then
    raise Exception.Create('Owner wird benötigt');

  if AnonProcID = 0 then
    AnonProcID := NativeInt(@AnonProc);

  Stop(Owner, AnonProcID);

  DM := TDelayedMethod.Create(Owner);
  try
    DM.FAnonProc := AnonProc;
    DM.FAnonProcID := AnonProcID;
    DM.TimerSetup(Delay);
    Result := AnonProcID;
  except
    DM.Free;
    raise
  end;
end;

class procedure TDelayedMethod.Stop(Owner: TComponent; AnonProcID: NativeInt);
var
  cc: Integer;
  TempComp: TComponent;
begin
  if not Assigned(Owner) then
    Exit;

  for cc := 0 to Owner.ComponentCount - 1 do
  begin
    TempComp := Owner.Components[cc];
    if Assigned(TempComp) and
      (TempComp is TDelayedMethod) and
      not (csDestroying in TempComp.ComponentState) and
      (TDelayedMethod(TempComp).FAnonProcID = AnonProcID) then
    begin
      TempComp.Free;
      Exit;
    end;
  end;
end;

procedure TDelayedMethod.TimerSetup(Delay: Integer);
begin
  if Delay < 1 then
    PostMessage(FWindowHandle, WM_TIMER, 1, 0)
  else if SetTimer(FWindowHandle, 1, Delay, nil) = 0 then
    raise EOutOfResources.Create('Can''t create timer');
end;

procedure TDelayedMethod.WndProc(var Msg: TMessage);

  procedure TimerEventHandler;
  var
    LocalMethod: TMethod;
    LocalAnonMethod: TProc;
  begin
    if Assigned(FMethod) then
    begin
      LocalMethod := FMethod;
      Free;
      LocalMethod;
    end
    else if Assigned(FAnonProc) then
    begin
      LocalAnonMethod := FAnonProc;
      Free;
      LocalAnonMethod;
    end;
  end;

begin
  if Msg.Msg = WM_TIMER then
    try
      TimerEventHandler;
    except
      Application.HandleException(nil);
    end
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

end.
