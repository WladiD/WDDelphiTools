program VclControlsTools;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  WDDT.Vcl.Controls in '..\..\WDDT.Vcl.Controls.pas',
  CtrlBackspaceExample in 'CtrlBackspaceExample.pas' {CtrlBackspaceExampleForm},
  CheckboxGroupExample in 'CheckboxGroupExample.pas' {CheckboxGroupExampleForm},
  WDDT.Vcl.RTTI in '..\..\WDDT.Vcl.RTTI.pas',
  DeepCloneComponentExample in 'DeepCloneComponentExample.pas' {DeepCloneComponentExampleForm},
  WDDT.RTTI in '..\..\WDDT.RTTI.pas',
  WDDT.StringTools in '..\..\WDDT.StringTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
