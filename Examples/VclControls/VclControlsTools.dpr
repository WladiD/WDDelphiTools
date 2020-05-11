program VclControlsTools;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  WDDT.Vcl.Controls in '..\..\WDDT.Vcl.Controls.pas',
  CtrlBackspaceExample in 'CtrlBackspaceExample.pas' {CtrlBackspaceExampleForm},
  CheckboxGroupExample in 'CheckboxGroupExample.pas' {CheckboxGroupExampleForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCtrlBackspaceExampleForm, CtrlBackspaceExampleForm);
  Application.CreateForm(TCheckboxGroupExampleForm, CheckboxGroupExampleForm);
  Application.Run;
end.
