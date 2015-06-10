object Form1: TForm1
  Left = 192
  Top = 122
  Width = 870
  Height = 500
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edSource: TEdit
    Left = 112
    Top = 72
    Width = 193
    Height = 21
    TabOrder = 0
    Text = 'edSource'
  end
  object btnBrows: TButton
    Left = 312
    Top = 72
    Width = 41
    Height = 25
    Caption = 'btnBrows'
    TabOrder = 1
    OnClick = btnBrowsClick
  end
  object edDest: TEdit
    Left = 112
    Top = 96
    Width = 193
    Height = 21
    TabOrder = 2
    Text = 'edDest'
  end
  object btnConvert: TButton
    Left = 112
    Top = 128
    Width = 75
    Height = 25
    Caption = 'btnConvert'
    TabOrder = 3
    OnClick = btnConvertClick
  end
  object gbInfo: TGroupBox
    Left = 392
    Top = 56
    Width = 241
    Height = 161
    Caption = 'gbInfo'
    TabOrder = 4
    object lbCaption: TLabel
      Left = 8
      Top = 24
      Width = 44
      Height = 13
      Caption = 'lbCaption'
    end
    object lbExtractDir: TLabel
      Left = 8
      Top = 64
      Width = 54
      Height = 13
      Caption = 'lbExtractDir'
    end
    object edCaption: TEdit
      Left = 8
      Top = 40
      Width = 225
      Height = 21
      TabOrder = 0
      Text = 'There is a test SFX archive'
    end
    object edExtractDir: TEdit
      Left = 8
      Top = 80
      Width = 225
      Height = 21
      TabOrder = 1
      Text = 'c:\Popka'
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'MZF|*.mzf'
    Left = 312
    Top = 104
  end
end
