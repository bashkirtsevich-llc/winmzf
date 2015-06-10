object wndMain: TwndMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Skin engine'
  ClientHeight = 342
  ClientWidth = 713
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object tvNavigate: TTreeView
    Left = 8
    Top = 8
    Width = 185
    Height = 325
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = tvNavigateChange
    Items.NodeData = {
      0101000000230000000000000000000000FFFFFFFFFFFFFFFF00000000090000
      0005490063006F006E007300270000000000000000000000FFFFFFFFFFFFFFFF
      000000000000000007620074006E005F00610064006400290000000000000000
      000000FFFFFFFFFFFFFFFF000000000000000008620074006E005F006F007000
      65006E002F0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      0B620074006E005F0065007800740072006100630074002D0000000000000000
      000000FFFFFFFFFFFFFFFF00000000000000000A620074006E005F0064006500
      6C00650074006500290000000000000000000000FFFFFFFFFFFFFFFF00000000
      0000000008620074006E005F0063006F00700079002900000000000000000000
      00FFFFFFFFFFFFFFFF000000000000000008620074006E005F006D006F007600
      6500290000000000000000000000FFFFFFFFFFFFFFFF00000000000000000862
      0074006E005F0069006E0066006F002B0000000000000000000000FFFFFFFFFF
      FFFFFF000000000000000009620074006E005F0063006C006F00730065002900
      00000000000000000000FFFFFFFFFFFFFFFF000000000000000008620074006E
      005F007400650073007400}
  end
  object gbMain: TGroupBox
    Left = 199
    Top = 8
    Width = 505
    Height = 97
    Caption = 'Parameters'
    TabOrder = 1
    object lbType: TLabel
      Left = 8
      Top = 24
      Width = 32
      Height = 13
      Caption = 'lbType'
    end
    object lbName: TLabel
      Left = 8
      Top = 40
      Width = 49
      Height = 13
      Caption = 'File name:'
    end
    object lbImageSize: TLabel
      Left = 8
      Top = 80
      Width = 32
      Height = 13
      Caption = 'Size=?'
    end
    object edFileName: TEdit
      Left = 8
      Top = 56
      Width = 449
      Height = 21
      TabOrder = 0
      Text = 'edFileName'
    end
    object btnBrows: TButton
      Left = 463
      Top = 56
      Width = 30
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowsClick
    end
  end
  object gbConfig: TGroupBox
    Left = 199
    Top = 111
    Width = 400
    Height = 222
    Caption = 'Configurations'
    TabOrder = 2
    object lbAuthor: TLabel
      Left = 8
      Top = 16
      Width = 37
      Height = 13
      Caption = 'Author:'
    end
    object lbVersion: TLabel
      Left = 8
      Top = 56
      Width = 57
      Height = 13
      Caption = 'Skin version'
    end
    object lbComment: TLabel
      Left = 8
      Top = 152
      Width = 49
      Height = 13
      Caption = 'Comment:'
    end
    object lbButtonsSize: TLabel
      Left = 8
      Top = 96
      Width = 62
      Height = 13
      Caption = 'Buttons size:'
    end
    object lbHeight: TLabel
      Left = 24
      Top = 114
      Width = 35
      Height = 13
      Caption = 'Height:'
    end
    object lbWidth: TLabel
      Left = 24
      Top = 134
      Width = 32
      Height = 13
      Caption = 'Width:'
    end
    object edAuthor: TEdit
      Left = 8
      Top = 32
      Width = 385
      Height = 21
      TabOrder = 0
      Text = 'No name'
    end
    object meVersion: TMaskEdit
      Left = 8
      Top = 72
      Width = 385
      Height = 21
      EditMask = '00\.00\.00\.00\'
      MaxLength = 11
      TabOrder = 1
      Text = '01.00.00.00'
    end
    object memComment: TMemo
      Left = 8
      Top = 171
      Width = 385
      Height = 41
      TabOrder = 4
    end
    object edHeight: TEdit
      Left = 64
      Top = 112
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '48'
      OnKeyPress = edHeightKeyPress
    end
    object edWidth: TEdit
      Left = 64
      Top = 136
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '48'
      OnKeyPress = edHeightKeyPress
    end
    object track1: TUpDown
      Left = 185
      Top = 112
      Width = 16
      Height = 21
      Associate = edHeight
      Min = 16
      Max = 256
      Position = 48
      TabOrder = 5
    end
    object track2: TUpDown
      Left = 185
      Top = 136
      Width = 16
      Height = 21
      Associate = edWidth
      Min = 16
      Max = 256
      Position = 48
      TabOrder = 6
    end
  end
  object btnBuild: TBitBtn
    Left = 611
    Top = 215
    Width = 94
    Height = 25
    Caption = 'Build'
    TabOrder = 3
    OnClick = btnBuildClick
  end
  object btnOpen: TBitBtn
    Left = 611
    Top = 246
    Width = 94
    Height = 25
    Caption = 'Extract'
    TabOrder = 4
    OnClick = btnOpenClick
  end
  object btnSave: TBitBtn
    Left = 611
    Top = 277
    Width = 94
    Height = 25
    Caption = 'Save'
    Enabled = False
    TabOrder = 5
  end
  object btnExit: TBitBtn
    Left = 611
    Top = 308
    Width = 94
    Height = 25
    Caption = 'Exit'
    TabOrder = 6
    OnClick = btnExitClick
  end
  object gbPreview: TGroupBox
    Left = 611
    Top = 111
    Width = 94
    Height = 98
    Caption = 'Preview'
    TabOrder = 7
    object imgPreview: TImage
      Left = 11
      Top = 15
      Width = 72
      Height = 72
      Center = True
      Proportional = True
      Stretch = True
    end
  end
  object mnuMain: TMainMenu
    Left = 8
    Top = 304
    object Skin1: TMenuItem
      Caption = 'Skin'
      object Build1: TMenuItem
        Caption = 'Build'
        OnClick = btnBuildClick
      end
      object Extract1: TMenuItem
        Caption = 'Extract'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = btnExitClick
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object dlgOpen: TOpenDialog
    Left = 40
    Top = 304
  end
  object dlgSave: TSaveDialog
    Left = 72
    Top = 304
  end
end
