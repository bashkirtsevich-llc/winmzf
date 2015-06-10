object DlgCopyMoveFiles: TDlgCopyMoveFiles
  Left = 192
  Top = 122
  BorderStyle = bsDialog
  Caption = 'Copy & Move files'
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BtnCopyOrMove: TBitBtn
    Left = 264
    Top = 216
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = BtnCopyOrMoveClick
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
  object BtnCancel: TBitBtn
    Left = 352
    Top = 216
    Width = 81
    Height = 25
    Caption = 'Abort'
    TabOrder = 1
    OnClick = BtnCancelClick
    Kind = bkCancel
  end
  object gbOptions: TGroupBox
    Left = 16
    Top = 16
    Width = 417
    Height = 129
    Caption = 'Options'
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 70
      Height = 13
      Caption = 'Destination dir:'
    end
    object edDestDir: TEdit
      Left = 16
      Top = 40
      Width = 353
      Height = 21
      TabOrder = 0
    end
    object btnBrows: TBitBtn
      Left = 380
      Top = 40
      Width = 20
      Height = 20
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowsClick
    end
    object cbUpdateModifyTime: TCheckBox
      Left = 16
      Top = 72
      Width = 385
      Height = 17
      Caption = 'Update modify time'
      TabOrder = 2
    end
    object cbUpdateAccessTime: TCheckBox
      Left = 16
      Top = 96
      Width = 385
      Height = 17
      Caption = 'Update access time'
      TabOrder = 3
    end
  end
  object CopyGauge: TProgressBar
    Left = 16
    Top = 160
    Width = 417
    Height = 17
    Smooth = True
    TabOrder = 3
  end
  object GeneralGauge: TProgressBar
    Left = 16
    Top = 184
    Width = 417
    Height = 17
    Smooth = True
    TabOrder = 4
  end
end
