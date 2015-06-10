object DlgMZFCompressOptions: TDlgMZFCompressOptions
  Left = 192
  Top = 122
  BorderStyle = bsDialog
  Caption = 'WinMZF Compressed advantage options'
  ClientHeight = 281
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TBitBtn
    Left = 160
    Top = 248
    Width = 73
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 240
    Top = 248
    Width = 73
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object gbLZMAConfig: TGroupBox
    Left = 8
    Top = 8
    Width = 305
    Height = 233
    Caption = 'LZMA compression configurations'
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 76
      Height = 13
      Caption = 'Dictitionary size:'
    end
    object Label2: TLabel
      Left = 16
      Top = 64
      Width = 56
      Height = 13
      Caption = 'Math finder:'
    end
    object Label3: TLabel
      Left = 16
      Top = 104
      Width = 88
      Height = 13
      Caption = 'Literal context bits:'
    end
    object Label4: TLabel
      Left = 16
      Top = 144
      Width = 70
      Height = 13
      Caption = 'Literal pos bits:'
    end
    object Label5: TLabel
      Left = 16
      Top = 184
      Width = 51
      Height = 13
      Caption = 'Fast bytes:'
    end
    object Label6: TLabel
      Left = 160
      Top = 24
      Width = 40
      Height = 13
      Caption = 'Pos bits:'
    end
    object Label7: TLabel
      Left = 160
      Top = 64
      Width = 85
      Height = 13
      Caption = 'Encode algorithm:'
    end
    object Label8: TLabel
      Left = 160
      Top = 104
      Width = 34
      Height = 13
      Caption = 'Priority:'
    end
    object Label9: TLabel
      Left = 160
      Top = 152
      Width = 129
      Height = 41
      AutoSize = False
      Caption = 'Compute optimal compression parameters (recomendet)'
      WordWrap = True
    end
    object cbDictSize: TComboBox
      Left = 16
      Top = 40
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 23
      TabOrder = 0
      Text = '8 MB'
      Items.Strings = (
        '1 B'
        '2 B'
        '4 B'
        '8 B'
        '16 B'
        '32 B'
        '64 B'
        '128 B'
        '256 B'
        '512 B'
        '1 KB'
        '2 KB'
        '4 KB'
        '8 KB'
        '16 KB'
        '32 KB'
        '64 KB'
        '128 KB'
        '256 KB'
        '512 KB'
        '1 MB'
        '2 MB'
        '4 MB'
        '8 MB'
        '16 MB'
        '32 MB'
        '64 MB'
        '128 MB'
        '256 MB')
    end
    object cbMathFinder: TComboBox
      Left = 16
      Top = 80
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 1
      Text = 'BT4'
      Items.Strings = (
        'BT2'
        'BT4'
        'BT4b')
    end
    object cbLitContextBits: TComboBox
      Left = 16
      Top = 120
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 2
      Text = '3'
      Items.Strings = (
        '0'
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8')
    end
    object cbLitPosBits: TComboBox
      Left = 16
      Top = 160
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 3
      Text = '0'
      Items.Strings = (
        '0'
        '1'
        '2'
        '3'
        '4')
    end
    object cbFastBytes: TComboBox
      Left = 16
      Top = 200
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
    end
    object cbPosBits: TComboBox
      Left = 160
      Top = 40
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 5
      Text = '2'
      Items.Strings = (
        '0'
        '1'
        '2'
        '3'
        '4')
    end
    object cbAlgorithm: TComboBox
      Left = 160
      Top = 80
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 6
      Text = 'AES-256'
      Items.Strings = (
        'AES-256'
        'MAD-7'
        'IDEA')
    end
    object cbPriority: TComboBox
      Left = 160
      Top = 120
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 7
      Text = 'Normal'
      Items.Strings = (
        'Idle'
        'Lowest'
        'Normal'
        'High'
        'Real time')
    end
    object BtnCompute: TBitBtn
      Left = 160
      Top = 200
      Width = 129
      Height = 21
      Caption = 'Compute optimal'
      TabOrder = 8
      OnClick = BtnComputeClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF84848400000000000000000000000000000000
        0000000000000000000000000000000000000000848484FFFFFFFFFFFF000000
        F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0
        F0F0F0F0000000FFFFFFFFFFFF000000F0F0F0F0F0F0000000000000F0F0F000
        0000000000F0F0F0000000000000F0F0F0F0F0F0000000FFFFFFFFFFFF000000
        F0F0F0F0F0F0000000000000F0F0F0000000000000F0F0F0000000000000F0F0
        F0F0F0F0000000FFFFFFFFFFFF000000F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0
        F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0000000FFFFFFFFFFFF000000
        F0F0F0F0F0F0000000000000F0F0F0000000000000F0F0F0000000000000F0F0
        F0F0F0F0000000FFFFFFFFFFFF000000F0F0F0F0F0F0000000000000F0F0F000
        0000000000F0F0F0000000000000F0F0F0F0F0F0000000FFFFFFFFFFFF000000
        F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0
        F0F0F0F0000000FFFFFFFFFFFF000000F0F0F0840000FF0000FF0000FF0000FF
        0000FF0000FF0000FF0000FF0000FF0000F0F0F0000000FFFFFFFFFFFF000000
        F0F0F0840000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF00
        00F0F0F0000000FFFFFFFFFFFF000000F0F0F0840000FF0000FF0000FF0000FF
        0000FF0000FF0000FF0000FF0000FF0000F0F0F0000000FFFFFFFFFFFF000000
        F0F0F08400008400008400008400008400008400008400008400008400008400
        00F0F0F0000000FFFFFFFFFFFF000000F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0
        F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0000000FFFFFFFFFFFF848484
        0000000000000000000000000000000000000000000000000000000000000000
        00000000848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    end
  end
end
