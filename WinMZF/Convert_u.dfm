object dlgConvert: TdlgConvert
  Left = 183
  Top = 100
  BorderStyle = bsDialog
  Caption = 'Convert archive'
  ClientHeight = 218
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 8
    Top = 122
    Width = 75
    Height = 13
    Caption = 'Source archive:'
  end
  object Label4: TLabel
    Left = 8
    Top = 149
    Width = 64
    Height = 13
    Caption = 'Dest archive:'
  end
  object gbConverterOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 353
    Height = 97
    Caption = 'Converter options'
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 31
      Width = 28
      Height = 13
      Caption = 'From:'
    end
    object Label2: TLabel
      Left = 167
      Top = 31
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object cbConvertFrom: TComboBox
      Left = 50
      Top = 28
      Width = 108
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'MZF'
      Items.Strings = (
        'MZF'
        'RAR'
        '7Z'
        'ZIP'
        'BH'
        'ZOO'
        'GZ'
        'LHA'
        'CAB'
        'ARJ'
        'PKG5'
        'ACE')
    end
    object cbConvertTo: TComboBox
      Left = 189
      Top = 28
      Width = 108
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'MZF'
      OnChange = cbConvertToChange
      Items.Strings = (
        'MZF'
        'LZMA [7Z]'
        'ZIP'
        'Black Hole [BH]'
        'LHA'
        'JAR'
        'Make Cab [CAB]'
        'TAR')
    end
    object btnCompressOptions: TBitBtn
      Left = 303
      Top = 28
      Width = 36
      Height = 21
      Caption = 'Ratio'
      TabOrder = 2
      OnClick = btnCompressOptionsClick
    end
    object cbDeleteSource: TCheckBox
      Left = 16
      Top = 64
      Width = 275
      Height = 17
      Caption = 'Delete source archive'
      TabOrder = 3
    end
  end
  object btnConvert: TBitBtn
    Left = 209
    Top = 185
    Width = 73
    Height = 25
    Caption = 'Convert'
    Default = True
    TabOrder = 0
    OnClick = btnConvertClick
    Glyph.Data = {
      36060000424D3606000000000000360000002800000020000000100000000100
      18000000000000060000110B0000110B00000000000000000000D8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9EC00CC00006600D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDC787878D8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9EC00CC00009900009900006600D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4787878D8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      00CC00009900009900009900009900006600D8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B4B4B4B478
      7878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00CC00
      009900009900009900009900009900009900006600D8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4
      B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00CC00
      00990000990000660000CC00009900009900009900006600D8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4787878DCDCDCB4B4B4B4
      B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00CC00
      009900006600D8E9ECD8E9EC00CC00009900009900009900006600D8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4787878D8E9ECD8E9ECDCDCDCB4
      B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00CC00
      006600D8E9ECD8E9ECD8E9ECD8E9EC00CC00009900009900009900006600D8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDC787878D8E9ECD8E9ECD8E9ECD8E9ECDC
      DCDCB4B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00CC000099000099000099000066
      00D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00CC000099000099000099
      00006600D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00CC000099000099
      00006600D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00CC000099
      00006600D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4787878D8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC00CC
      00006600D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDC787878D8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC}
    NumGlyphs = 2
  end
  object btnCancel: TBitBtn
    Left = 288
    Top = 185
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    Glyph.Data = {
      36060000424D3606000000000000360000002800000020000000100000000100
      18000000000000060000C30E0000C30E00000000000000000000D8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC0000FF
      000099000099000099D8E9ECD8E9ECD8E9ECD8E9ECD8E9EC0000FF0000990000
      99000099D8E9ECD8E9ECD8E9ECDCDCDC787878787878787878D8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECDCDCDC787878787878787878D8E9ECD8E9ECD8E9EC0000FF
      0000CC0000CC0000CC000099D8E9ECD8E9ECD8E9EC0000FF0000CC0000CC0000
      CC000099D8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8E9ECD8
      E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9EC
      0000FF0000CC0000CC0000CC000099D8E9EC0000FF0000CC0000CC0000CC0000
      99D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8
      E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9EC0000FF0000CC0000CC0000CC0000990000CC0000CC0000CC000099D8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B478
      7878B4B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9EC0000FF0000CC0000CC0000CC0000CC0000CC000099D8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4
      B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9EC0000FF0000CC0000CC0000CC000099D8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4
      B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9EC0000FF0000CC0000CC0000CC0000CC0000CC000099D8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4
      B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9EC0000FF0000CC0000CC0000CC0000990000CC0000CC0000CC000099D8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B478
      7878B4B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      0000FF0000CC0000CC0000CC000099D8E9EC0000FF0000CC0000CC0000CC0000
      99D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8
      E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9ECD8E9EC0000FF
      0000CC0000CC0000CC000099D8E9ECD8E9ECD8E9EC0000FF0000CC0000CC0000
      CC000099D8E9ECD8E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8E9ECD8
      E9ECD8E9ECDCDCDCB4B4B4B4B4B4B4B4B4787878D8E9ECD8E9ECD8E9EC0000FF
      0000FF0000FF0000FFD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC0000FF0000FF0000
      FF0000FFD8E9ECD8E9ECD8E9ECDCDCDCDCDCDCDCDCDCDCDCDCD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECDCDCDCDCDCDCDCDCDCDCDCDCD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC
      D8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9
      ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8
      E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9ECD8E9EC}
    NumGlyphs = 2
  end
  object edSource: TEdit
    Left = 89
    Top = 119
    Width = 240
    Height = 21
    TabOrder = 3
  end
  object edTarget: TEdit
    Left = 89
    Top = 146
    Width = 240
    Height = 21
    TabOrder = 4
  end
  object btnBrows0: TBitBtn
    Left = 335
    Top = 119
    Width = 26
    Height = 21
    Caption = '...'
    TabOrder = 5
    OnClick = btnBrows0Click
  end
  object btnBrows1: TBitBtn
    Tag = 1
    Left = 335
    Top = 146
    Width = 26
    Height = 21
    Caption = '...'
    TabOrder = 6
    OnClick = btnBrows0Click
  end
  object dlgOpen: TOpenDialog
    Title = 'Open archive'
    Left = 8
    Top = 176
  end
  object dlgSave: TSaveDialog
    Title = 'Save archive'
    Left = 40
    Top = 176
  end
end