object dlgInfo: TdlgInfo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Skin info'
  ClientHeight = 233
  ClientWidth = 433
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
  object lbComment: TLabel
    Left = 152
    Top = 8
    Width = 45
    Height = 13
    Caption = 'Comment'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 179
    Width = 417
    Height = 9
    Shape = bsBottomLine
  end
  object btnOk: TBitBtn
    Left = 269
    Top = 200
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 350
    Top = 200
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object leFileName: TLabeledEdit
    Left = 8
    Top = 27
    Width = 138
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 45
    EditLabel.Height = 13
    EditLabel.Caption = 'File name'
    ReadOnly = True
    TabOrder = 2
  end
  object leAuthor: TLabeledEdit
    Left = 8
    Top = 64
    Width = 138
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 33
    EditLabel.Height = 13
    EditLabel.Caption = 'Author'
    ReadOnly = True
    TabOrder = 3
  end
  object leVersion: TLabeledEdit
    Left = 8
    Top = 104
    Width = 138
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'Version'
    ReadOnly = True
    TabOrder = 4
  end
  object leDate: TLabeledEdit
    Left = 8
    Top = 144
    Width = 138
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 23
    EditLabel.Height = 13
    EditLabel.Caption = 'Date'
    ReadOnly = True
    TabOrder = 5
  end
  object memComment: TMemo
    Left = 152
    Top = 27
    Width = 273
    Height = 138
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
  end
end
