object dlgExtract: TdlgExtract
  Left = 192
  Top = 122
  BorderStyle = bsSingle
  Caption = 'UnRAR by M.A.D.M.A.N.'
  ClientHeight = 145
  ClientWidth = 273
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnExtract: TButton
    Left = 184
    Top = 112
    Width = 73
    Height = 25
    Caption = 'E.x.t.r.a.c.t.'
    TabOrder = 0
    OnClick = btnExtractClick
  end
  object edArchive: TEdit
    Left = 16
    Top = 16
    Width = 209
    Height = 21
    TabOrder = 1
    Text = 'edArchive'
  end
  object btnBrows: TButton
    Left = 232
    Top = 16
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = btnBrowsClick
  end
  object edDestDir: TEdit
    Left = 16
    Top = 48
    Width = 241
    Height = 21
    TabOrder = 3
    Text = 'edDestDir'
  end
  object pbDone: TProgressBar
    Left = 16
    Top = 80
    Width = 241
    Height = 17
    TabOrder = 4
  end
  object dlgOpen: TOpenDialog
    Filter = 'Rar|*.RAR'
    Left = 16
    Top = 104
  end
end
