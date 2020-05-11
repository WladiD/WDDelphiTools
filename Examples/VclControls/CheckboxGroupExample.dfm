object CheckboxGroupExampleForm: TCheckboxGroupExampleForm
  Left = 0
  Top = 0
  Caption = 'Example for HandleCheckboxGroup'
  ClientHeight = 320
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object MainGridPanel: TGridPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 321
    Height = 307
    Margins.Left = 10
    Margins.Top = 10
    Align = alClient
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = FlowPanel1
        Row = 0
      end
      item
        Column = 0
        Control = Panel1
        Row = 1
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    TabOrder = 0
    ExplicitLeft = 32
    ExplicitTop = 8
    ExplicitWidth = 265
    ExplicitHeight = 105
    object FlowPanel1: TFlowPanel
      Left = 0
      Top = 0
      Width = 321
      Height = 153
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 56
      ExplicitTop = 24
      ExplicitWidth = 185
      ExplicitHeight = 41
      object CheckBox1: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 97
        Height = 17
        Caption = 'CheckBox1'
        TabOrder = 0
        OnClick = NumericCheckBoxClick
      end
      object CheckBox2: TCheckBox
        AlignWithMargins = True
        Left = 106
        Top = 3
        Width = 97
        Height = 17
        Caption = 'CheckBox2'
        TabOrder = 1
        OnClick = NumericCheckBoxClick
      end
      object CheckBox3: TCheckBox
        AlignWithMargins = True
        Left = 209
        Top = 3
        Width = 97
        Height = 17
        Caption = 'CheckBox3'
        TabOrder = 2
        OnClick = NumericCheckBoxClick
      end
      object CheckBox4: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 26
        Width = 97
        Height = 17
        Caption = 'CheckBox4'
        TabOrder = 3
        OnClick = NumericCheckBoxClick
      end
      object CheckBox5: TCheckBox
        AlignWithMargins = True
        Left = 106
        Top = 26
        Width = 97
        Height = 17
        Caption = 'CheckBox5'
        TabOrder = 4
        OnClick = NumericCheckBoxClick
      end
      object CheckBox6: TCheckBox
        AlignWithMargins = True
        Left = 209
        Top = 26
        Width = 97
        Height = 17
        Caption = 'CheckBox6'
        TabOrder = 5
        OnClick = NumericCheckBoxClick
      end
    end
    object Panel1: TPanel
      Left = 0
      Top = 153
      Width = 321
      Height = 154
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 112
      ExplicitTop = 184
      ExplicitWidth = 185
      ExplicitHeight = 41
      object CheckBoxF: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 118
        Width = 315
        Height = 17
        Align = alTop
        Caption = 'CheckBox F'
        TabOrder = 5
        OnClick = AlphaCheckBoxClick
        ExplicitLeft = 128
        ExplicitTop = 64
        ExplicitWidth = 97
      end
      object CheckBoxE: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 95
        Width = 315
        Height = 17
        Align = alTop
        Caption = 'CheckBox E'
        TabOrder = 4
        OnClick = AlphaCheckBoxClick
        ExplicitLeft = 6
        ExplicitTop = 11
      end
      object CheckBoxD: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 72
        Width = 315
        Height = 17
        Align = alTop
        Caption = 'CheckBox D'
        TabOrder = 3
        OnClick = AlphaCheckBoxClick
        ExplicitLeft = 6
        ExplicitTop = 11
      end
      object CheckBoxC: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 49
        Width = 315
        Height = 17
        Align = alTop
        Caption = 'CheckBox C'
        TabOrder = 2
        OnClick = AlphaCheckBoxClick
        ExplicitLeft = 6
        ExplicitTop = 11
      end
      object CheckBoxB: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 26
        Width = 315
        Height = 17
        Align = alTop
        Caption = 'CheckBox B'
        TabOrder = 1
        OnClick = AlphaCheckBoxClick
        ExplicitLeft = 6
        ExplicitTop = 11
      end
      object CheckBoxA: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 315
        Height = 17
        Align = alTop
        Caption = 'CheckBox A'
        TabOrder = 0
        OnClick = AlphaCheckBoxClick
        ExplicitLeft = 6
        ExplicitTop = 11
      end
    end
  end
end
