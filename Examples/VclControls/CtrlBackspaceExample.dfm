object CtrlBackspaceExampleForm: TCtrlBackspaceExampleForm
  Left = 0
  Top = 0
  Caption = 'Try [Ctrl] + [Backspace] in the input controls'
  ClientHeight = 300
  ClientWidth = 555
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
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 555
    Height = 300
    Align = alClient
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = Label1
        Row = 0
      end
      item
        Column = 1
        Control = Label2
        Row = 0
      end
      item
        Column = 0
        Control = HandledEdit
        Row = 1
      end
      item
        Column = 1
        Control = UnhandledEdit
        Row = 1
      end
      item
        Column = 0
        Control = HandledMemo
        Row = 2
      end
      item
        Column = 1
        Control = UnhandledMemo
        Row = 2
      end>
    RowCollection = <
      item
        Value = 14.285714285714280000
      end
      item
        Value = 21.428571428571430000
      end
      item
        Value = 64.285714285714290000
      end>
    TabOrder = 0
    DesignSize = (
      555
      300)
    object Label1: TLabel
      Left = 60
      Top = 14
      Width = 156
      Height = 13
      Anchors = []
      Caption = 'This edit and memo are handled:'
      ExplicitLeft = 58
    end
    object Label2: TLabel
      Left = 326
      Top = 14
      Width = 180
      Height = 13
      Anchors = []
      Caption = 'This edit and memo are NOT handled:'
      ExplicitLeft = 323
    end
    object HandledEdit: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 45
      Width = 271
      Height = 21
      Align = alTop
      AutoSelect = False
      TabOrder = 0
      Text = 'Hello world! Lorem ipsum dolor sit amet.'
      OnEnter = EditEnter
      OnKeyPress = EditKeyPress
    end
    object UnhandledEdit: TEdit
      AlignWithMargins = True
      Left = 280
      Top = 45
      Width = 272
      Height = 21
      Align = alTop
      AutoSelect = False
      TabOrder = 2
      Text = 'Hello world! Lorem ipsum dolor sit amet.'
      OnEnter = EditEnter
    end
    object HandledMemo: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 109
      Width = 271
      Height = 188
      Align = alClient
      Lines.Strings = (
        'Lorem ipsum dolor sit amet, consetetur sadipscing '
        'elitr, sed diam nonumy eirmod tempor invidunt ut '
        'labore et dolore magna aliquyam erat, sed diam '
        'voluptua. At vero eos et accusam et justo duo '
        'dolores et ea rebum. Stet clita kasd gubergren, no '
        'sea takimata sanctus est Lorem ipsum dolor sit amet. '
        'Lorem ipsum dolor sit amet, consetetur sadipscing '
        'elitr, sed diam nonumy eirmod tempor invidunt ut '
        'labore et dolore magna aliquyam erat, sed diam '
        'voluptua. At vero eos et accusam et justo duo '
        'dolores et ea rebum. Stet clita kasd gubergren, no '
        'sea takimata sanctus est Lorem ipsum dolor ')
      TabOrder = 1
      OnKeyPress = EditKeyPress
    end
    object UnhandledMemo: TMemo
      AlignWithMargins = True
      Left = 280
      Top = 109
      Width = 272
      Height = 188
      Align = alClient
      Lines.Strings = (
        'Lorem ipsum dolor sit amet, consetetur sadipscing '
        'elitr, sed diam nonumy eirmod tempor invidunt ut '
        'labore et dolore magna aliquyam erat, sed diam '
        'voluptua. At vero eos et accusam et justo duo '
        'dolores et ea rebum. Stet clita kasd gubergren, no '
        'sea takimata sanctus est Lorem ipsum dolor sit amet. '
        'Lorem ipsum dolor sit amet, consetetur sadipscing '
        'elitr, sed diam nonumy eirmod tempor invidunt ut '
        'labore et dolore magna aliquyam erat, sed diam '
        'voluptua. At vero eos et accusam et justo duo '
        'dolores et ea rebum. Stet clita kasd gubergren, no '
        'sea takimata sanctus est Lorem ipsum dolor ')
      TabOrder = 3
    end
  end
end
