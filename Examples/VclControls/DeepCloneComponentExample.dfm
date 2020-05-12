object DeepCloneComponentExampleForm: TDeepCloneComponentExampleForm
  Left = 0
  Top = 0
  Caption = 'Example for DeepCloneComponent'
  ClientHeight = 412
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 371
    Width = 465
    Height = 41
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 0
    object CloneButton: TButton
      AlignWithMargins = True
      Left = 352
      Top = 3
      Width = 110
      Height = 33
      Align = alRight
      Caption = 'Clone ItemPanel'
      TabOrder = 0
      OnClick = CloneButtonClick
    end
  end
  object MainScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 465
    Height = 371
    VertScrollBar.Increment = 50
    VertScrollBar.Tracking = True
    Align = alClient
    TabOrder = 1
    object ItemPanel: TPanel
      Left = 0
      Top = 0
      Width = 461
      Height = 90
      Align = alTop
      BevelEdges = [beBottom]
      BevelKind = bkSoft
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      object MainLabel: TLabel
        AlignWithMargins = True
        Left = 106
        Top = 0
        Width = 50
        Height = 88
        Margins.Top = 0
        Align = alLeft
        Caption = 'Main Label'
        Layout = tlCenter
        ExplicitHeight = 13
      end
      object CheckBox1: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 97
        Height = 82
        Align = alLeft
        Caption = 'Activate'
        TabOrder = 0
        ExplicitHeight = 81
      end
      object SubPanel: TPanel
        Left = 159
        Top = 0
        Width = 302
        Height = 88
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 182
        ExplicitTop = -1
        ExplicitWidth = 282
        ExplicitHeight = 87
        DesignSize = (
          302
          88)
        object Button1: TButton
          Left = 208
          Top = 16
          Width = 75
          Height = 26
          Anchors = [akTop, akRight, akBottom]
          Caption = 'Details'
          TabOrder = 0
          ExplicitLeft = 188
          ExplicitHeight = 25
        end
        object ComboBox1: TComboBox
          Left = 136
          Top = 48
          Width = 145
          Height = 21
          Anchors = [akTop, akRight, akBottom]
          ItemIndex = 0
          TabOrder = 1
          Text = 'Item #1'
          Items.Strings = (
            'Item #1'
            'Item #2'
            'Item #3')
          ExplicitLeft = 116
        end
      end
    end
  end
end
