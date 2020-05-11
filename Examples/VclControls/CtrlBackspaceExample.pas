unit CtrlBackspaceExample;

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
  TCtrlBackspaceExampleForm = class(TForm)
    GridPanel1: TGridPanel;
    Label1: TLabel;
    Label2: TLabel;
    HandledEdit: TEdit;
    UnhandledEdit: TEdit;
    HandledMemo: TMemo;
    UnhandledMemo: TMemo;
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditEnter(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TCtrlBackspaceExampleForm.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #127 then
  begin
    PerformCtrlBackspaceAction(TCustomEdit(Sender));
    Key := #0;
  end;
end;

procedure TCtrlBackspaceExampleForm.EditEnter(Sender: TObject);
var
  SenderEdit: TEdit absolute Sender;
begin
  SenderEdit.SelStart := Length(SenderEdit.Text);
  SenderEdit.SelLength := 0;
end;

end.
