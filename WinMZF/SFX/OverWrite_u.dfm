object dlgOverWrite: TdlgOverWrite
  Left = 193
  Top = 123
  BorderStyle = bsDialog
  Caption = 'File overwrite prompt'
  ClientHeight = 221
  ClientWidth = 289
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
  object gbInfo: TGroupBox
    Left = 8
    Top = 8
    Width = 273
    Height = 153
    Caption = 'File already exists:'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 32
      Height = 13
      Caption = 'Label2'
    end
    object Label3: TLabel
      Left = 8
      Top = 48
      Width = 32
      Height = 13
      Caption = 'Label3'
    end
    object Label4: TLabel
      Left = 8
      Top = 64
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label5: TLabel
      Left = 8
      Top = 80
      Width = 32
      Height = 13
      Caption = 'Label5'
    end
    object Label6: TLabel
      Left = 8
      Top = 96
      Width = 32
      Height = 13
      Caption = 'Label6'
    end
    object Label7: TLabel
      Left = 8
      Top = 112
      Width = 32
      Height = 13
      Caption = 'Label7'
    end
    object Label8: TLabel
      Left = 8
      Top = 136
      Width = 103
      Height = 13
      Caption = 'Overwrite current file?'
    end
  end
  object btnYes: TBitBtn
    Left = 8
    Top = 168
    Width = 89
    Height = 21
    Caption = '&Yes'
    Default = True
    ModalResult = 6
    TabOrder = 1
    OnClick = BtnYesClick
    NumGlyphs = 2
  end
  object btnNo: TBitBtn
    Left = 8
    Top = 192
    Width = 89
    Height = 21
    Cancel = True
    Caption = '&No'
    ModalResult = 7
    TabOrder = 2
    OnClick = BtnNoClick
    NumGlyphs = 2
  end
  object btnYessToAll: TBitBtn
    Left = 100
    Top = 168
    Width = 89
    Height = 21
    Caption = 'Yes to all'
    ModalResult = 6
    TabOrder = 3
    OnClick = btnYessToAllClick
    NumGlyphs = 2
  end
  object btnNoToAll: TBitBtn
    Left = 100
    Top = 192
    Width = 89
    Height = 21
    Caption = 'No to all'
    ModalResult = 7
    TabOrder = 4
    OnClick = btnNoToAllClick
  end
  object btnCancel: TBitBtn
    Left = 192
    Top = 168
    Width = 89
    Height = 21
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object btnHelp: TBitBtn
    Left = 192
    Top = 192
    Width = 89
    Height = 21
    Caption = 'Help'
    TabOrder = 6
  end
end
