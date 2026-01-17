unit DPT.Fixtures;

interface

uses
  System.SyncObjs,
  Slim.Fixture;

type
  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}
  [SlimFixture('TEchoFixture')]
  TEchoFixture = class(TSlimFixture)
  public
    function Echo(const Value: String): String;
    procedure StopServer;
  end;

var
  StopServerEvent: TEvent;

implementation

uses
  System.SysUtils;

function TEchoFixture.Echo(const Value: String): String;
begin
  Result := Value;
end;

procedure TEchoFixture.StopServer;
begin
  if Assigned(StopServerEvent) then
    StopServerEvent.SetEvent;
end;

initialization
  StopServerEvent := TEvent.Create(nil, True, False, '');
  RegisterSlimFixture(TEchoFixture);

finalization
  FreeAndNil(StopServerEvent);

end.
