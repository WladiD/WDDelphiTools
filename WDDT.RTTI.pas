unit WDDT.RTTI;

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  System.Rtti;

type
  TPropertyAcceptProc = reference to procedure(Target: TObject; PropInfo: PPropInfo; out Accept: Boolean);

procedure CopyObject(ObjFrom, ObjTo: TObject; AcceptProc: TPropertyAcceptProc = nil);
function HasAllRequiredProperties(RClass: TRttiInstanceType; RequiredProperties: array of string;
  out Properties: TArray<TRttiProperty>): Boolean;

implementation

procedure CopyObject(ObjFrom, ObjTo: TObject; AcceptProc: TPropertyAcceptProc);
var
  PropInfos: PPropList;
  PropInfo: PPropInfo;
  Count, Loop: Integer;
  OrdVal: Longint;
  StrVal: String;
  FloatVal: Extended;
  MethodVal: TMethod;
  PropertyAccepted: Boolean;
begin
  Count := GetPropList(ObjFrom.ClassInfo, tkAny, nil);
  GetMem(PropInfos, Count * SizeOf(PPropInfo));
  try
    GetPropList(ObjFrom.ClassInfo, tkAny, PropInfos);
    for Loop := 0 to Count - 1 do
    begin
      PropInfo := GetPropInfo(ObjTo.ClassInfo, string(PropInfos^[Loop]^.Name));

      PropertyAccepted := True;
      if Assigned(AcceptProc) then
      begin
        AcceptProc(ObjTo, PropInfo, PropertyAccepted);
        if not PropertyAccepted then
          Continue;
      end;

      case PropInfos^[Loop]^.PropType^.Kind of
        tkInteger, tkChar, tkEnumeration,
        tkSet, tkClass, tkWChar:
        begin
          OrdVal := GetOrdProp(ObjFrom, PropInfos^[Loop]);
          if Assigned(PropInfo) then
            SetOrdProp(ObjTo, PropInfo, OrdVal);
        end;
        tkFloat:
        begin
          FloatVal := GetFloatProp(ObjFrom, PropInfos^[Loop]);
          if Assigned(PropInfo) then
            SetFloatProp(ObjTo, PropInfo, FloatVal);
        end;
        tkWString,
        tkLString,
        tkString,
        tkUnicodeString:
        begin
          { Avoid copying 'Name' - components must have unique names }
          if UpperCase(string(PropInfos^[Loop]^.Name)) = 'NAME' then
            Continue;
          StrVal := GetStrProp(ObjFrom, PropInfos^[Loop]);
          if Assigned(PropInfo) then
            SetStrProp(ObjTo, PropInfo, StrVal);
        end;
        tkMethod:
        begin
          MethodVal := GetMethodProp(ObjFrom, PropInfos^[Loop]);
          if Assigned(PropInfo) then
            SetMethodProp(ObjTo, PropInfo, MethodVal);
        end
      end
    end
  finally
    FreeMem(PropInfos, Count * SizeOf(PPropInfo));
  end;
end;

// Überprüft, ob alle in RequiredProperties angegebenen Eigenschaften in RClass
// existieren. Falls dies der Fall ist (Result = True), dann liegen im Ausgabeparameter
// Properties an exakt den gleichen Positionen die TRttiProperty-Instanzen an, über welche
// wiederum auf die Zieleigenschaften einer Klasseninstanz zugegriffen werden kann.
//
// Wie man an den Parameter RClass ist im folgenden Beispiel ersichtlich. Bei Target handelt sich
// um die Instanz auf wessen Eigenschaften man zugreifen möchte.
// <code>
// RC := TRttiContext.Create;
// try
//   RClass := RC.GetType(Target.ClassType) as TRttiInstanceType;
// finally
//   RC.Free;
// end;
// </code>
//
// Wenn man die Properties schlussendlich hat, so wird wie folgt darauf zugegriffen:
// <code>
// if HasAllRequiredProperties(RClass, ['PropA', 'PropB'], Properties) then
// begin
//   Properties[0].SetValue(Target, 'My new value for PropA');
//   Properties[1].SetValue(Target, 'My new value for PropB');
// end;
// </code>
function HasAllRequiredProperties(RClass: TRttiInstanceType; RequiredProperties: array of string;
  out Properties: TArray<TRttiProperty>): Boolean;
var
  AllProperties: TArray<TRttiProperty>;
  Status: TBits;
  CurProperty: TRttiProperty;
  CurPropertyName: string;
  PropIndex, ReqPropCount, MatchCount: Integer;

  function GetPropertyParameterIndex(const PropertyName: string): Integer;
  var
    cc: Integer;
  begin
    for cc := 0 to ReqPropCount - 1 do
      if not Status[cc] and SameText(PropertyName, RequiredProperties[cc]) then
        Exit(cc);
    Result := -1;
  end;

begin
  Result := False;
  AllProperties := RClass.GetProperties;
  ReqPropCount := Length(RequiredProperties);
  if Length(AllProperties) < ReqPropCount then
    Exit;

  Status := TBits.Create;
  try
    Status.Size := ReqPropCount;
    SetLength(Properties, ReqPropCount);
    MatchCount := 0;

    for CurProperty in AllProperties do
    begin
      CurPropertyName := CurProperty.Name;
      PropIndex := GetPropertyParameterIndex(CurPropertyName);
      if PropIndex >= 0 then
      begin
        Properties[PropIndex] := CurProperty;
        Status[PropIndex] := True;
        Inc(MatchCount);

        if MatchCount = ReqPropCount then
          Exit(True);
      end;
    end;
  finally
    Status.Free;
  end;
end;

end.
