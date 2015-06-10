object dlgViewer: TdlgViewer
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Extract skin'
  ClientHeight = 313
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TBitBtn
    Left = 367
    Top = 280
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 448
    Top = 280
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object cblFiles: TCheckListBox
    Left = 8
    Top = 8
    Width = 513
    Height = 266
    ItemHeight = 13
    TabOrder = 2
  end
end
