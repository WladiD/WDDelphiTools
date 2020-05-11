unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,

  WDDT.Vcl.Controls;

type
  TMainForm = class(TForm)
    CtrlBackspaceExampleButton: TButton;
    CheckboxGroupExampleButton: TButton;
    procedure CtrlBackspaceExampleButtonClick(Sender: TObject);
    procedure CheckboxGroupExampleButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure ShowExample(ExampleForm: TForm);
  end;

var
  MainForm: TMainForm;

implementation

uses
  CtrlBackspaceExample,
  CheckboxGroupExample;

{$R *.dfm}

procedure TMainForm.CheckboxGroupExampleButtonClick(Sender: TObject);
begin
  ShowExample(CheckboxGroupExampleForm);
end;

procedure TMainForm.CtrlBackspaceExampleButtonClick(Sender: TObject);
begin
  ShowExample(CtrlBackspaceExampleForm);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  SenderForm: TCustomForm;
  FoundWinControl: TWinControl;
begin
  if Key = vkEscape then
  begin
    if Sender is TCustomForm then
      SenderForm := TCustomForm(Sender)
    else if (Sender is TControl) and
      HasParentControlByClass(TControl(Sender), TCustomForm, FoundWinControl) then
      SenderForm := TCustomForm(FoundWinControl)
    else
      SenderForm := nil;

    if Assigned(SenderForm) then
    begin
      SenderForm.Close;
      Key := 0;
    end;
  end;
end;

procedure TMainForm.ShowExample(ExampleForm: TForm);
begin
  ExampleForm.OnKeyDown := FormKeyDown;
  ExampleForm.KeyPreview := True;
  ExampleForm.PopupParent := Self;
  ExampleForm.Show;
end;

end.
