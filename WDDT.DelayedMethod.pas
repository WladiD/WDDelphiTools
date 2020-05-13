unit WDDT.DelayedMethod;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Messages,
  Winapi.Windows;

type
  TMethod = procedure of object;

  // Klasse f�r die einfache Verwendung von verz�gernden Methoden
  //
  // Eine beliebige Methode ohne Parameter (TMethod) kann in einem Nachfolger von TComponent
  // implementiert werden und an die Klassenmethode TDelayedMethod.Execute �bergeben werden.
  //
  // Der eigentliche Aufruf der verz�gernden Methode erfolgt stets vom internen Timer aus, also
  // stets sauber aus dem obersten MessageHandler der Applikation.
  //
  // Eine eindeutige Methode kann stets nur einmal hinzugef�gt bzw. ausgef�hrt werden.
  // Wurde zuvor die selbe Methode schon mal �bergeben, so ersetzt der letzte Aufruf den vorherigen.
  //
  // Man kann also folgendes ausf�hren...
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
  // MyObject.MethodB (nach 1sek) nur einmal ausgef�hrt werden.
  //
  // Damit man sich �ber die Freigabe nicht k�mmern braucht, wurde die Klasse von TComponent
  // abgeleitet, um vom dessen Observer-Pattern gebrauch zu nehmen.
  // Konkret heisst es: Sollte der Implementierer der Methode freigegeben werden, so ist
  // sichergestellt, dass keine seiner verz�gernden Methoden ausgef�hrt wird und die zugeh�rigen
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

uses
  Forms;

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

// Erstellt eine neue TDelayedMethod-Instanz mit der �bergebenen Methode
//
// Der Parameter Delay gibt die Dauer der Verz�gerung in ms an. Standardwert ist 0 und bedeutet,
// dass die Methode so schnell wie m�glich (beim n�chsten Durchlauf der Message-Queue) ausgef�hrt
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
    raise Exception.Create('Der Implementor der verz�gernden Methode muss ein Nachfolger von TComponent sein.');

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

// Gibt die TDelayedMethod-Instanz im Implementierer der Methode f�r diese Methode frei
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

// Erstellt eine TDelayedMethod-Instanz f�r die �bergebene anonyme Methode
//
// Im Gegensatz zu einer klassischen Methode hat eine anonyme Methode keine implementierende
// Klasse. Daher ist die Angabe eines Owner notwendig. Die erstellte Instanz wird dem Owner
// hinzugef�gt, um weiterhin vom zuverl�ssigen Observer-Pattern des TComponent zu
// profitieren.
// Eine weitere Eigenheit von anonymen Methoden ist, dass sie nicht zwingend eine Entit�t darstellt.
// D.h. eine scheinbar identische anonyme Methode hat nicht zwingend die gleiche Adresse (Pointer).
// Grob gesagt, es wird jedes mal eine neue Entit�t f�r ein und die selbe anonyme Methode erstellt,
// wenn sich der Wert der zu fangenden Variablen von einer Deklaration zur n�chsten �ndert.
// Aus diesem Grund kann optional eine beliebige ID im Parameter AnonProcID angegeben werden, wenn
// man sicherstellen m�chte, dass diese Methode nur einmal verz�gert aufgerufen werden soll.
// Wird AnonProcID nicht angegeben, wird der Pointer der Methode als ID verwendet und als Ergebnis
// zur�ckgeliefert.
class function TDelayedMethod.Execute(AnonProc: TProc; Owner: TComponent; Delay: Integer = 0;
  AnonProcID: NativeInt = 0): NativeInt;
var
  DM: TDelayedMethod;
begin
  if not Assigned(Owner) then
    raise Exception.Create('Owner wird ben�tigt');

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
