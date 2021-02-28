unit WDDT.MobileTools;

interface

uses
  System.SysUtils
{$IFDEF ANDROID},
  FMX.Helpers.Android,
  Androidapi.JNI.App,
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText
{$ENDIF};

type
  TMobileTools = class
  protected
    class function GetKeepScreenOn: Boolean; static;
    class procedure SetKeepScreenOn(Value: Boolean); static;
  public
    class property KeepScreenOn: Boolean read GetKeepScreenOn write SetKeepScreenOn;
  end;

implementation

{ TMobileTools }

class function TMobileTools.GetKeepScreenOn: Boolean;
begin
{$IFDEF ANDROID}
  var FlagsResult: Integer;

  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      FlagsResult := TAndroidHelper.Activity.getWindow.getAttributes.flags;
    end);

  Result := (FlagsResult and TJWindowManager_LayoutParams.JavaClass.FLAG_KEEP_SCREEN_ON) <> 0;
{$ELSE}
  Result := False;
{$ENDIF}
end;

// Solution is based on <https://stackoverflow.com/questions/34802583/wanting-to-keep-the-screen-on-in-delphi-app-on-android>
class procedure TMobileTools.SetKeepScreenOn(Value: Boolean);
begin
{$IFDEF ANDROID}
  var PerformProc: TCallBack;

  if Value then
    PerformProc :=
      procedure
      begin
        TAndroidHelper.Activity.getWindow.addFlags(
          TJWindowManager_LayoutParams.JavaClass.FLAG_KEEP_SCREEN_ON);
      end
  else
    PerformProc :=
      procedure
      begin
        TAndroidHelper.Activity.getWindow.clearFlags(
          TJWindowManager_LayoutParams.JavaClass.FLAG_KEEP_SCREEN_ON);
      end;

  CallInUIThreadAndWaitFinishing(PerformProc);
{$ENDIF}
end;

end.
