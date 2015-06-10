object DlgComment: TDlgComment
  Left = 192
  Top = 122
  BorderStyle = bsDialog
  Caption = 'Archive comment'
  ClientHeight = 257
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object MemComment: TMemo
    Left = 8
    Top = 8
    Width = 449
    Height = 209
    Lines.Strings = (
      'Archive commentary here !!!')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BtnOk: TBitBtn
    Left = 384
    Top = 224
    Width = 73
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
end
