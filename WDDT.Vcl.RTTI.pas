unit WDDT.Vcl.RTTI;

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  System.Rtti,
  Vcl.Controls,

  WDDT.RTTI;

type
  TCloneProc = reference to procedure(Source, Target: TComponent);

function DeepCloneComponent(Component: TComponent; CustomParent: TWinControl = nil;
  AcceptProc: TPropertyAcceptProc = nil; AfterClone: TCloneProc = nil): TComponent;

implementation

var
  CloneComponentCallCounter: Integer;

function DeepCloneComponent(Component: TComponent; CustomParent: TWinControl;
  AcceptProc: TPropertyAcceptProc; AfterClone: TCloneProc): TComponent;
var
  OldName, NewName, TempName: string;
  ResultControl: TControl absolute Result;
  SourceWinControl: TWinControl absolute Component;
  ResultWinControl: TWinControl absolute Result;

  procedure AdjustControls;
  begin
    if not Assigned(CustomParent) then
      ResultControl.Parent := SourceWinControl.Parent
    else
      ResultControl.Parent := CustomParent;
  end;

  procedure AdjustWinControls;
  var
    cc: Integer;
  begin
    for cc := 0 to SourceWinControl.ControlCount - 1 do
      DeepCloneComponent(SourceWinControl.Controls[cc], ResultWinControl, AcceptProc, AfterClone);
  end;

begin
  try
    OldName := Component.Name;
    NewName := OldName + IntToStr(CloneComponentCallCounter);
    TempName := OldName + IntToStr(CloneComponentCallCounter + 1);
    Inc(CloneComponentCallCounter);

    Result := TComponentClass(Component.ClassType).Create(Component.Owner);

    Component.Name := TempName;
    Result.Name := NewName;
    CopyObject(Component, Result, AcceptProc);

    if Component is TControl then
      AdjustControls;

    if Component is TWinControl then
      AdjustWinControls;
  finally
    Component.Name := OldName;
  end;

  if Assigned(AfterClone) then
    AfterClone(Component, Result);
end;

end.
