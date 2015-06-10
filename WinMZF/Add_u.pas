unit Add_u;
        
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ShellAPI, Compress_u,Advanced;

type
  TdlgAddFiles = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    tab: TPageControl;
    tabGeneral: TTabSheet;
    tabFiles: TTabSheet;
    tabComment: TTabSheet;
    cbFileName: TComboBox;
    btnBrows: TBitBtn;
    dlgSave: TSaveDialog;
    gbCompression: TGroupBox;
    Label1: TLabel;
    tbLevel: TTrackBar;
    cbEncodeHead: TCheckBox;
    cbEncodeFile: TCheckBox;
    gbArchOptions: TGroupBox;
    cbArchiveReadOnly: TCheckBox;
    cbArchiveHidden: TCheckBox;
    cbArchiveArchived: TCheckBox;
    Label4: TLabel;
    cbCompressMethod: TComboBox;
    cbShowPassword: TCheckBox;
    edPassword: TEdit;
    Label2: TLabel;
    cbCreateSFX: TCheckBox;
    gbCompressOptions: TGroupBox;
    lvFiles: TListView;
    btnAdd: TBitBtn;
    btnDelete: TBitBtn;
    btnClear: TBitBtn;
    Label3: TLabel;
    DlgAdd: TOpenDialog;
    memComment: TMemo;
    cbWordWrap: TCheckBox;
    Label5: TLabel;
    cbDictSize: TComboBox;
    Label6: TLabel;
    cbMathFinder: TComboBox;
    Label7: TLabel;
    cbLitContextBits: TComboBox;
    Label8: TLabel;
    cbLitPosBits: TComboBox;
    Label9: TLabel;
    cbFastBytes: TComboBox;
    Label10: TLabel;
    cbPosBits: TComboBox;
    Label11: TLabel;
    cbAlgorithm: TComboBox;
    Label12: TLabel;
    cbPriority: TComboBox;
    procedure cbCompressMethodChange(Sender: TObject);
    procedure cbEncodeFileClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cbWordWrapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbDictSizeChange(Sender: TObject);
    procedure btnBrowsClick(Sender: TObject);
    procedure tbLevelChange(Sender: TObject);
    procedure cbShowPasswordClick(Sender: TObject);
    procedure ReadLanguage;
  private
    procedure WMDROPFILES (var Msg: TMessage); message WM_DROPFILES;
    { Private declarations }
  public
    InSideLevel:TMZFCompressionLevel;
    { Public declarations }
  end;

var
  dlgAddFiles: TdlgAddFiles;

implementation

{$R *.dfm}

procedure TdlgAddFiles.WMDROPFILES (var Msg: TMessage);
var i,amount,
  size : integer;
  Filename: PChar;
  Temp:String;
begin 
  //inherited;
  Amount :=DragQueryFile(Msg.WParam, $FFFFFFFF, Filename, 255);
  for i :=0 to (Amount - 1) do
    begin
      size :=DragQueryFile(Msg.WParam, i , nil, 0) + 1;
      Filename:=StrAlloc(size);
      DragQueryFile(Msg.WParam,i , Filename, size);
      Temp:=StrPas(Filename);
      If DirectoryExists(Temp) Then Temp:=Temp+'\*.*';
      lvFiles.Items.add.Caption:=Temp;
      StrDispose(Filename);
    end;
  DragFinish(Msg.WParam);
  Temp:='';
  Inherited;
end;

procedure TdlgAddFiles.cbCompressMethodChange(Sender: TObject);
Var State:Boolean;
begin
  cbEncodeHead.Checked:=False;
  cbEncodeFile.Checked:=False;
  With Sender As TComboBox Do
    Begin
      Case ItemIndex Of
        0:Begin
            State:=(cbEncodeHead.Checked)Or(cbEncodeFile.Checked);
            edPassword.Enabled:=State;
            cbShowPassword.Enabled:=State;
            cbEncodeFile.Enabled:=True;
            cbEncodeHead.Enabled:=True;
            tbLevel.Max:=4;
            gbCompressOptions.Visible:=True;
          End;
        1:Begin
            cbEncodeHead.Enabled:=False;
            cbEncodeFile.Enabled:=False;
            edPassword.Enabled:=(cbEncodeHead.Checked)Or(cbEncodeFile.Checked);
            cbShowPassword.Enabled:=(cbEncodeHead.Checked)Or(cbEncodeFile.Checked);;
            tbLevel.Max:=3;
            gbCompressOptions.Visible:=False;
          End;
        2,3,5:Begin
                edPassword.Enabled:=(cbEncodeHead.Checked)Or(cbEncodeFile.Checked);;
                cbShowPassword.Enabled:=(cbEncodeHead.Checked)Or(cbEncodeFile.Checked);;
                cbEncodeHead.Enabled:=True;
                cbEncodeFile.Enabled:=False;
                tbLevel.Max:=3;
                gbCompressOptions.Visible:=False;
              End;
      Else
        Begin
          edPassword.Enabled:=False;
          cbShowPassword.Enabled:=False;
          cbEncodeFile.Enabled:=False;
          cbEncodeHead.Enabled:=False;
          tbLevel.Max:=3;
          gbCompressOptions.Visible:=False;
        End;
      End;
      //If FileNameCombo.Text='' Then Exit;
    End;
  Case cbCompressMethod.ItemIndex Of
    0:Begin If cbFileName.Text<>'' Then cbFileName.Text:=ChangeFileExt(cbFileName.Text,'.MZF'); dlgSave.Filter:='MZF Archive (*.mzf)|*.MZF'; End;
    1:Begin If cbFileName.Text<>'' Then cbFileName.Text:=ChangeFileExt(cbFileName.Text,'.7Z'); dlgSave.Filter:='7Z Archive (*.7Z)|*.7Z'; End;
    2:Begin If cbFileName.Text<>'' Then cbFileName.Text:=ChangeFileExt(cbFileName.Text,'.ZIP'); dlgSave.Filter:='ZIP Archive (*.zip)|*.ZIP'; End;
    3:Begin If cbFileName.Text<>'' Then cbFileName.Text:=ChangeFileExt(cbFileName.Text,'.BH'); dlgSave.Filter:='BH Archive (*.bh)|*.BH'; End;
    4:Begin If cbFileName.Text<>'' Then cbFileName.Text:=ChangeFileExt(cbFileName.Text,'.LHA'); dlgSave.Filter:='LHA Archive (*.lha)|*.LHA'; End;
    5:Begin If cbFileName.Text<>'' Then cbFileName.Text:=ChangeFileExt(cbFileName.Text,'.JAR'); dlgSave.Filter:='JAR Archive (*.jar)|*.JAR'; End;
    6:Begin If cbFileName.Text<>'' Then cbFileName.Text:=ChangeFileExt(cbFileName.Text,'.CAB'); dlgSave.Filter:='CAB Archive (*.cab)|*.CAB'; End;
    7:Begin If cbFileName.Text<>'' Then cbFileName.Text:=ChangeFileExt(cbFileName.Text,'.TAR'); dlgSave.Filter:='TAR Archive (*.tar)|*.TAR'; End;
  End;

end;

procedure TdlgAddFiles.cbEncodeFileClick(Sender: TObject);
var State:Boolean;
begin
  State:=((cbEncodeHead.Checked)Or(cbEncodeFile.Checked))And
         ((cbCompressMethod.ItemIndex=0) Or (cbCompressMethod.ItemIndex=1) Or
          (cbCompressMethod.ItemIndex=2) Or (cbCompressMethod.ItemIndex=4));
  edPassword.Enabled:=State;
  cbShowPassword.Enabled:=State;
  If Not State Then
    edPassword.Clear;
end;

procedure TdlgAddFiles.btnAddClick(Sender: TObject);
Var Index:Integer;
begin
  If Not DlgAdd.Execute(Handle) Then Exit;
  For Index:=0 To DlgAdd.Files.Count-1 Do
    lvFiles.Items.Add.Caption:=DlgAdd.Files.Strings[Index];

end;

procedure TdlgAddFiles.btnDeleteClick(Sender: TObject);
begin
  lvFiles.DeleteSelected;
end;

procedure TdlgAddFiles.btnClearClick(Sender: TObject);
begin
  lvFiles.Clear;
end;

procedure TdlgAddFiles.btnOkClick(Sender: TObject);
begin
  If (DirectoryExists(ExtractFileDir(cbFileName.Text)))And
     (ExtractFileName(cbFileName.Text)<>'') Then
    ModalResult:=mrOk Else
    MessageBox(Handle,'Please enter archive file name or check file list!','Error',MB_OK+MB_ICONEXCLAMATION);
end;

procedure TdlgAddFiles.cbWordWrapClick(Sender: TObject);
begin
  memComment.WordWrap:=cbWordWrap.Checked;
end;

procedure TdlgAddFiles.FormCreate(Sender: TObject);
Var i:Word;
begin
  DragAcceptFiles({lvFiles.}Handle, true);
  For i:=5 To 273 Do
    cbFastBytes.Items.Add(IntToStr(i));
  cbFastBytes.ItemIndex:=176;
  ReadLanguage;
end;

procedure TdlgAddFiles.cbDictSizeChange(Sender: TObject);
begin
  InSideLevel.EncodeAlgorithm:=TEncodeAlgorithm(cbAlgorithm.ItemIndex);
  InSideLevel.DictitionarySize:=cbDictSize.ItemIndex;
  InSideLevel.MathFinder:=cbMathFinder.ItemIndex;
  InSideLevel.LiteralContextBits:=cbLitContextBits.ItemIndex;
  InSideLevel.LiteralPosBits:=cbLitPosBits.ItemIndex;
  InSideLevel.FastBytes:=cbFastBytes.ItemIndex+5;
  InSideLevel.PosBits:=cbPosBits.ItemIndex;
  InSideLevel.Priority:=cbPriority.ItemIndex;
end;

procedure TdlgAddFiles.btnBrowsClick(Sender: TObject);
Var Temp:String;
begin
  If Not DlgSave.Execute{(Handle)} Then Exit;
  Case cbCompressMethod.ItemIndex Of
    0:Temp:='.MZF';
    1:Temp:='.7Z';
    2:Temp:='.ZIP';
    3:Temp:='.BH';
    4:Temp:='.LHA';
    5:Temp:='.JAR';
    6:Temp:='.CAB';
    7:Temp:='.TAR';
  End;
  If UpperCase(ExtractFileExt(DlgSave.FileName))<>Temp Then
    cbFileName.Text:=DlgSave.FileName+Temp Else
    cbFileName.Text:=DlgSave.FileName;
end;

procedure TdlgAddFiles.tbLevelChange(Sender: TObject);
begin
  If cbCompressMethod.ItemIndex=0 Then
    Begin
      Case tbLevel.Position Of
        0:Begin
            cbDictSize.ItemIndex:=0;
            cbLitContextBits.ItemIndex:=0;
            cbMathFinder.ItemIndex:=0;
            cbLitPosBits.ItemIndex:=0;
            cbFastBytes.ItemIndex:=0;
            cbPosBits.ItemIndex:=0;
            Label1.Caption:=ReadFromLanguage('Levels','Level0','Compression level: Fast');
          End;
        1:Begin
            cbDictSize.ItemIndex:=13;
            cbLitContextBits.ItemIndex:=1;
            cbMathFinder.ItemIndex:=0;
            cbLitPosBits.ItemIndex:=1;
            cbFastBytes.ItemIndex:=88;
            cbPosBits.ItemIndex:=3;
            Label1.Caption:=ReadFromLanguage('Levels','Level1','Compression level: Good');
          End;
        2:Begin
            cbDictSize.ItemIndex:=18;
            cbLitContextBits.ItemIndex:=2;
            cbMathFinder.ItemIndex:=1;
            cbLitPosBits.ItemIndex:=2;
            cbFastBytes.ItemIndex:=176;
            cbPosBits.ItemIndex:=4;
            Label1.Caption:=ReadFromLanguage('Levels','Level2','Compression level: Normal');
          End;
        3:Begin
            cbDictSize.ItemIndex:=24;
            cbLitContextBits.ItemIndex:=4;
            cbMathFinder.ItemIndex:=1;
            cbLitPosBits.ItemIndex:=4;
            cbFastBytes.ItemIndex:=264;
            cbPosBits.ItemIndex:=4;
            Label1.Caption:=ReadFromLanguage('Levels','Level3','Compression level: Best');
          End;
        4:Begin
            cbDictSize.ItemIndex:=26;
            cbLitContextBits.ItemIndex:=8;
            cbMathFinder.ItemIndex:=1;
            cbLitPosBits.ItemIndex:=8;
            cbFastBytes.ItemIndex:=268;
            cbPosBits.ItemIndex:=8;
            Label1.Caption:=ReadFromLanguage('Levels','Level4','Compression level: Ultra');
          End;
      End;
      cbDictSizeChange(Sender);
    End;
  Case tbLevel.Position Of
    0:Label1.Caption:=ReadFromLanguage('Levels','Level0','Compression level: Fast');
    1:Label1.Caption:=ReadFromLanguage('Levels','Level1','Compression level: Good');
    2:Label1.Caption:=ReadFromLanguage('Levels','Level2','Compression level: Normal');
    3:Label1.Caption:=ReadFromLanguage('Levels','Level3','Compression level: Best');
    4:Label1.Caption:=ReadFromLanguage('Levels','Level4','Compression level: Ultra');
  End;
end;

procedure TdlgAddFiles.cbShowPasswordClick(Sender: TObject);
begin
  If cbShowPassword.Checked Then
    edPassword.PasswordChar:=#0 Else
    edPassword.PasswordChar:='*';
end;

procedure TdlgAddFiles.ReadLanguage;
begin
  With dlgAddFiles Do
    Begin
      Caption:=ReadFromLanguage('Windows','wndAdd',Caption);
      Label1.Caption:=ReadFromLanguage('Levels','Level2','Compression level: Normal');
      btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
      tabGeneral.Caption:=ReadFromLanguage('Tabs','tbGeneral',tabGeneral.Caption);
      tabFiles.Caption:=ReadFromLanguage('Tabs','tbFiles',tabFiles.Caption);
      tabComment.Caption:=ReadFromLanguage('Tabs','tbComment',tabComment.Caption);
      dlgSave.Title:=ReadFromLanguage('Dialogs','dlgSave',dlgSave.Title);
      DlgAdd.Title:=ReadFromLanguage('Dialogs','dlgAdd',dlgAdd.Title);
      BtnBrows.Caption:=ReadFromLanguage('Buttons','btnBrows',btnBrows.Caption);
      Label2.Caption:=ReadFromLanguage('Labels','lbPassword',Label2.Caption);
      cbShowPassword.Caption:=ReadFromLanguage('Labels','lbShowPass',cbShowPassword.Caption);
      Label4.Caption:=ReadFromLanguage('Labels','lbCompressMethod',Label4.Caption);
      gbCompression.Caption:=ReadFromLanguage('Labels','lbCompression',gbCompression.Caption);
      cbEncodeHead.Caption:=ReadFromLanguage('Labels','lbEncodeHead',cbEncodeHead.Caption);
      cbEncodeFile.Caption:=ReadFromLanguage('Labels','lbEncodeFile',cbEncodeFile.Caption);
      gbArchOptions.Caption:=ReadFromLanguage('Labels','lbArchOptions',gbArchOptions.Caption);
      cbArchiveReadOnly.Caption:=ReadFromLanguage('Labels','lbArchReadOnly',cbArchiveReadOnly.Caption);
      cbArchiveHidden.Caption:=ReadFromLanguage('Labels','lbArchHidden',cbArchiveHidden.Caption);
      cbArchiveArchived.Caption:=ReadFromLanguage('Labels','lbArchArchived',cbArchiveArchived.Caption);
      cbCreateSFX.Caption:=ReadFromLanguage('Labels','lbCreateSFX',cbCreateSFX.Caption);
      gbCompressOptions.Caption:=ReadFromLanguage('Labels','lbCompressOptions',gbCompressOptions.Caption);
      Label3.Caption:=ReadFromLanguage('Labels','lbDropHere',Label3.Caption);
      lvFiles.Columns[0].Caption:=ReadFromLanguage('Labels','lbFileName',lvFiles.Columns[0].Caption);
      btnAdd.Caption:=ReadFromLanguage('Buttons','btnAdd',btnAdd.Caption);
      btnDelete.Caption:=ReadFromLanguage('Buttons','btnDelete',btnDelete.Caption);
      btnClear.Caption:=ReadFromLanguage('Buttons','btnClear',btnClear.Caption);
      cbWordWrap.Caption:=ReadFromLanguage('Labels','lbWordWrap',cbWordWrap.Caption);
    End;
end;

end.
