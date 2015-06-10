object DlgDeleteFiles: TDlgDeleteFiles
  Left = 192
  Top = 122
  BorderStyle = bsDialog
  Caption = 'Delete files'
  ClientHeight = 257
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BtnStart: TBitBtn
    Left = 264
    Top = 216
    Width = 81
    Height = 25
    Caption = 'Delete'
    Default = True
    TabOrder = 0
    OnClick = BtnStartClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object BtnAbort: TBitBtn
    Left = 352
    Top = 216
    Width = 81
    Height = 25
    TabOrder = 1
    OnClick = BtnAbortClick
    Kind = bkCancel
  end
  object gbDeleteOptions: TGroupBox
    Left = 16
    Top = 16
    Width = 201
    Height = 137
    Caption = 'Delete options'
    TabOrder = 2
    object cbFillZeroBytes: TCheckBox
      Left = 16
      Top = 24
      Width = 169
      Height = 17
      Caption = 'Fill file zero byte'
      TabOrder = 0
      OnClick = cbFillZeroBytesClick
    end
    object cbUseFilter: TCheckBox
      Left = 16
      Top = 48
      Width = 169
      Height = 17
      Caption = 'Use filter'
      TabOrder = 1
      OnClick = cbUseFilterClick
    end
  end
  object gbFilter: TGroupBox
    Left = 232
    Top = 16
    Width = 201
    Height = 137
    Caption = 'Filter'
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 3
    object cbExtFilter: TComboBox
      Left = 16
      Top = 48
      Width = 169
      Height = 21
      Enabled = False
      ItemHeight = 13
      TabOrder = 1
      Text = '*'
    end
    object cbUseExtFilter: TCheckBox
      Left = 16
      Top = 24
      Width = 169
      Height = 17
      Caption = 'Ext filter'
      Enabled = False
      TabOrder = 0
      OnClick = cbUseExtFilterClick
    end
    object cbSizeFilter: TCheckBox
      Left = 16
      Top = 80
      Width = 169
      Height = 17
      Caption = 'Size filter'
      Enabled = False
      TabOrder = 2
      OnClick = cbSizeFilterClick
    end
    object edSize: TEdit
      Left = 16
      Top = 104
      Width = 65
      Height = 21
      Enabled = False
      TabOrder = 3
      Text = '0'
      OnKeyPress = edSizeKeyPress
    end
    object cbSizeDivide: TComboBox
      Left = 88
      Top = 104
      Width = 49
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 4
      Text = 'Bytes'
      Items.Strings = (
        'Bytes'
        'KB'
        'MB')
    end
    object cbMode: TComboBox
      Left = 144
      Top = 104
      Width = 41
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 5
      Text = '='
      Items.Strings = (
        '='
        '<'
        '>'
        '<='
        '>='
        '<>')
    end
  end
  object FillGauge: TProgressBar
    Left = 16
    Top = 160
    Width = 417
    Height = 17
    Smooth = True
    TabOrder = 4
    Visible = False
  end
  object GeneralGauge: TProgressBar
    Left = 16
    Top = 184
    Width = 417
    Height = 17
    Smooth = True
    TabOrder = 5
  end
end
