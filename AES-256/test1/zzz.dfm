object Form1: TForm1
  Left = 192
  Top = 122
  Caption = 'Form1'
  ClientHeight = 318
  ClientWidth = 619
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 168
    Top = 21
    Width = 39
    Height = 13
    Caption = 'First key'
  end
  object Label2: TLabel
    Left = 168
    Top = 67
    Width = 57
    Height = 13
    Caption = 'Second key'
  end
  object Button1: TButton
    Left = 312
    Top = 38
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 312
    Top = 69
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 168
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 168
    Top = 86
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Edit2'
  end
  object Memo1: TMemo
    Left = 168
    Top = 120
    Width = 219
    Height = 97
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
  end
end
