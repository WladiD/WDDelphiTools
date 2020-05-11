unit CheckboxGroupExample;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  WDDT.Vcl.Controls;

type
  TCheckboxGroupExampleForm = class(TForm)
    MainGridPanel: TGridPanel;
    FlowPanel1: TFlowPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Panel1: TPanel;
    CheckBoxF: TCheckBox;
    CheckBoxE: TCheckBox;
    CheckBoxD: TCheckBox;
    CheckBoxC: TCheckBox;
    CheckBoxB: TCheckBox;
    CheckBoxA: TCheckBox;
    procedure NumericCheckBoxClick(Sender: TObject);
    procedure AlphaCheckBoxClick(Sender: TObject);
  end;

var
  CheckboxGroupExampleForm: TCheckboxGroupExampleForm;

implementation

{$R *.dfm}

procedure TCheckboxGroupExampleForm.NumericCheckBoxClick(Sender: TObject);
begin
  HandleCheckboxGroup(Sender, [CheckBox1, CheckBox2, CheckBox3, CheckBox4, CheckBox5, CheckBox6]);
end;

procedure TCheckboxGroupExampleForm.AlphaCheckBoxClick(Sender: TObject);
begin
  HandleCheckboxGroup(Sender, [CheckBoxA, CheckBoxB, CheckBoxC, CheckBoxD, CheckBoxE, CheckBoxF]);
end;

end.
