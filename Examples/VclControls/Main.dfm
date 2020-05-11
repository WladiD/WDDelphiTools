object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Examples for unit WDDT.Vcl.Controls'
  ClientHeight = 310
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object CtrlBackspaceExampleButton: TButton
    AlignWithMargins = True
    Left = 3
    Top = 10
    Width = 322
    Height = 40
    Margins.Top = 10
    Align = alTop
    Caption = 'PerformCtrlBackspaceAction'
    TabOrder = 0
    OnClick = CtrlBackspaceExampleButtonClick
  end
  object CheckboxGroupExampleButton: TButton
    AlignWithMargins = True
    Left = 3
    Top = 56
    Width = 322
    Height = 40
    Align = alTop
    Caption = 'HandleCheckboxGroup'
    TabOrder = 1
    OnClick = CheckboxGroupExampleButtonClick
    ExplicitLeft = 6
    ExplicitTop = 18
  end
end
