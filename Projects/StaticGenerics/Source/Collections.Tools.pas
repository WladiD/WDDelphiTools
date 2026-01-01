unit Collections.Tools;

interface

function Capture(AValue: Boolean; out AAssignTo: Boolean): Boolean; inline; overload;
function Capture(AValue: Integer; out AAssignTo: Integer): Integer; inline; overload;

implementation

function Capture(AValue: Boolean; out AAssignTo: Boolean): Boolean;
begin
  Result:=AValue;
  AAssignTo:=AValue;
end;

function Capture(AValue: Integer; out AAssignTo: Integer): Integer;
begin
  Result:=AValue;
  AAssignTo:=AValue;
end;

end.
