object DlgAdd: TDlgAdd
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Add files in archive'
  ClientHeight = 289
  ClientWidth = 449
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
    Left = 288
    Top = 256
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = BtnOkClick
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
    Left = 368
    Top = 256
    Width = 73
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object pcMain: TPageControl
    Left = 8
    Top = 8
    Width = 433
    Height = 245
    ActivePage = tsGeneral
    HotTrack = True
    TabOrder = 2
    object tsGeneral: TTabSheet
      Caption = 'General'
      object Label2: TLabel
        Left = 200
        Top = 56
        Width = 46
        Height = 13
        Caption = 'Password'
      end
      object Label3: TLabel
        Left = 8
        Top = 168
        Width = 69
        Height = 13
        Caption = 'Encode mode:'
      end
      object Label4: TLabel
        Left = 200
        Top = 119
        Width = 84
        Height = 13
        Caption = 'Compress method'
      end
      object FileNameCombo: TComboBox
        Left = 8
        Top = 32
        Width = 409
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
      object gbCompression: TGroupBox
        Left = 8
        Top = 56
        Width = 185
        Height = 105
        Caption = 'Compression'
        TabOrder = 2
        object Label1: TLabel
          Left = 8
          Top = 16
          Width = 85
          Height = 13
          Caption = 'Compression level'
        end
        object tbLevel: TTrackBar
          Left = 8
          Top = 32
          Width = 169
          Height = 25
          Max = 4
          Position = 2
          TabOrder = 0
          ThumbLength = 15
          OnChange = tbLevelChange
        end
        object cbEncodeHead: TCheckBox
          Left = 8
          Top = 64
          Width = 169
          Height = 17
          Caption = 'Encode head'
          TabOrder = 1
          OnClick = cbEncodeHeadClick
        end
        object cbEncodeFile: TCheckBox
          Left = 8
          Top = 80
          Width = 169
          Height = 17
          Caption = 'Encode file'
          TabOrder = 2
          OnClick = cbEncodeHeadClick
        end
      end
      object edPassword: TEdit
        Left = 200
        Top = 72
        Width = 217
        Height = 21
        Enabled = False
        PasswordChar = '*'
        TabOrder = 3
      end
      object cbShowPassword: TCheckBox
        Left = 200
        Top = 96
        Width = 217
        Height = 17
        Caption = 'Display password'
        Enabled = False
        TabOrder = 4
        OnClick = cbShowPasswordClick
      end
      object btnBrows: TBitBtn
        Left = 360
        Top = 8
        Width = 57
        Height = 21
        Caption = 'Browse'
        TabOrder = 1
        OnClick = btnBrowsClick
      end
      object gbArchOptions: TGroupBox
        Left = 200
        Top = 136
        Width = 217
        Height = 73
        Caption = 'Archive options'
        TabOrder = 6
        object cbArchiveReadOnly: TCheckBox
          Left = 8
          Top = 16
          Width = 201
          Height = 17
          Caption = 'Set read only property'
          TabOrder = 0
        end
        object cbArchiveHidden: TCheckBox
          Left = 8
          Top = 32
          Width = 201
          Height = 17
          Caption = 'Set hidden property'
          TabOrder = 1
        end
        object cbArchiveArchived: TCheckBox
          Left = 8
          Top = 48
          Width = 201
          Height = 17
          Caption = 'Set archived property'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
      end
      object rgEncode: TComboBox
        Left = 8
        Top = 188
        Width = 185
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 7
        Text = 'Encode after compressing'
        Items.Strings = (
          'Encode before compressing'
          'Encode after compressing'
          'Encode before and after')
      end
      object cbCompressMethod: TComboBox
        Left = 296
        Top = 116
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 5
        Text = 'MZF'
        OnChange = cbCompressMethodChange
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
      object btnAdvance: TBitBtn
        Left = 395
        Top = 116
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 8
        OnClick = btnAdvanceClick
      end
    end
    object tsFiles: TTabSheet
      Caption = 'Files'
      ImageIndex = 1
      object lvFiles: TListView
        Left = 8
        Top = 8
        Width = 337
        Height = 201
        Columns = <
          item
            Caption = 'File name'
            Width = 310
          end>
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object BtnAdd: TBitBtn
        Left = 352
        Top = 8
        Width = 65
        Height = 25
        Caption = 'Add'
        TabOrder = 1
        OnClick = BtnAddClick
      end
      object BtnDel: TBitBtn
        Left = 352
        Top = 40
        Width = 65
        Height = 25
        Caption = 'Delete'
        TabOrder = 2
        OnClick = BtnDelClick
      end
      object BtnClear: TBitBtn
        Left = 352
        Top = 72
        Width = 65
        Height = 25
        Caption = 'Clear'
        TabOrder = 3
        OnClick = BtnClearClick
      end
    end
    object tsComment: TTabSheet
      Caption = 'Comment'
      ImageIndex = 3
      object CommentMemo: TMemo
        Left = 8
        Top = 8
        Width = 409
        Height = 201
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object DlgAdd: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect]
    Title = 'Add files'
    Left = 40
    Top = 256
  end
  object dlgSave: TSaveDialog
    Filter = 'MZF Archive (*.mzf)|*.MZF'
    Title = 'Save archive'
    Left = 4
    Top = 256
  end
end
