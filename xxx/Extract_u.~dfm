object DlgExtract: TDlgExtract
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Extract files'
  ClientHeight = 321
  ClientWidth = 473
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
  object BtnOk: TBitBtn
    Left = 312
    Top = 288
    Width = 73
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object BtnCancel: TBitBtn
    Left = 392
    Top = 288
    Width = 73
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object MainControl: TPageControl
    Left = 8
    Top = 8
    Width = 457
    Height = 273
    ActivePage = GeneralSheet
    HotTrack = True
    TabOrder = 2
    object GeneralSheet: TTabSheet
      Caption = 'General'
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 230
        Height = 13
        Caption = 'Destination path (will be created if does not exist)'
      end
      object cbDestDir: TComboBox
        Left = 8
        Top = 32
        Width = 433
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
      object rgUpdateMode: TRadioGroup
        Left = 8
        Top = 56
        Width = 201
        Height = 81
        Caption = 'Update mode'
        ItemIndex = 2
        Items.Strings = (
          'Overwrite existing'
          'Skip existing'
          'Overwrite prompt')
        TabOrder = 1
      end
      object DirTree: TShellTreeView
        Left = 216
        Top = 60
        Width = 225
        Height = 177
        ObjectTypes = [otFolders]
        Root = 'rfDesktop'
        UseShellImages = True
        AutoRefresh = False
        Indent = 19
        ParentColor = False
        RightClickSelect = True
        ShowRoot = False
        TabOrder = 2
        OnChange = DirTreeChange
      end
      object rgExtractMode: TRadioGroup
        Left = 8
        Top = 144
        Width = 201
        Height = 69
        Caption = 'Extract mode'
        Items.Strings = (
          'Extract all'
          'Extract selected')
        TabOrder = 3
      end
      object cbIgnoreCRCCheck: TCheckBox
        Left = 8
        Top = 218
        Width = 201
        Height = 17
        Caption = 'Ignore CRC check'
        TabOrder = 4
      end
    end
    object AdvancedSheet: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      object gbFileTime: TGroupBox
        Left = 8
        Top = 8
        Width = 209
        Height = 81
        Caption = 'File time'
        TabOrder = 0
        object cbUpdateModifyTime: TCheckBox
          Left = 8
          Top = 24
          Width = 193
          Height = 17
          Caption = 'Update modification time'
          TabOrder = 0
        end
        object cbUpdateAccessTime: TCheckBox
          Left = 8
          Top = 48
          Width = 193
          Height = 17
          Caption = 'Update last access time'
          TabOrder = 1
        end
      end
      object rgDeleteArchive: TRadioGroup
        Left = 224
        Top = 8
        Width = 217
        Height = 81
        Caption = 'Delete archive'
        ItemIndex = 0
        Items.Strings = (
          'Never'
          'Always')
        TabOrder = 1
      end
    end
  end
end
