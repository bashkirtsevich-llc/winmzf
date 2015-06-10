unit Add_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Compress_u, Advanced;

type
  TDlgAdd = class(TForm)
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    DlgAdd: TOpenDialog;
    pcMain: TPageControl;
    tsGeneral: TTabSheet;
    tsFiles: TTabSheet;
    lvFiles: TListView;
    BtnAdd: TBitBtn;
    BtnDel: TBitBtn;
    BtnClear: TBitBtn;
    FileNameCombo: TComboBox;
    gbCompression: TGroupBox;
    tbLevel: TTrackBar;
    Label1: TLabel;
    cbEncodeHead: TCheckBox;
    cbEncodeFile: TCheckBox;
    Label2: TLabel;
    edPassword: TEdit;
    cbShowPassword: TCheckBox;
    btnBrows: TBitBtn;
    dlgSave: TSaveDialog;
    tsComment: TTabSheet;
    CommentMemo: TMemo;
    gbArchOptions: TGroupBox;
    cbArchiveReadOnly: TCheckBox;
    cbArchiveHidden: TCheckBox;
    cbArchiveArchived: TCheckBox;
    rgEncode: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    cbCompressMethod: TComboBox;
    btnAdvance: TBitBtn;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDelClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure cbShowPasswordClick(Sender: TObject);
    procedure btnBrowsClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure cbEncodeHeadClick(Sender: TObject);
    procedure cbCompressMethodChange(Sender: TObject);
    procedure btnAdvanceClick(Sender: TObject);
    procedure tbLevelChange(Sender: TObject);
  private
    { Private declarations }
  public
    InsideCompressLevel:TMZFCompressionLevel;
    { Public declarations }
  end;

var
  DlgAdd: TDlgAdd;

implementation

uses AdvancedConfig_u;

{$R *.dfm}

procedure TDlgAdd.BtnAddClick(Sender: TObject);
Var Index:Integer;
begin
  If Not DlgAdd.Execute Then Exit;
  For Index:=0 To DlgAdd.Files.Count-1 Do
    lvFiles.Items.Add.Caption:=DlgAdd.Files.Strings[Index];
end;

procedure TDlgAdd.BtnDelClick(Sender: TObject);
begin
  lvFiles.DeleteSelected;
end;

procedure TDlgAdd.BtnClearClick(Sender: TObject);
begin
  lvFiles.Clear;
end;

procedure TDlgAdd.cbShowPasswordClick(Sender: TObject);
begin
  If cbShowPassword.Checked Then
    edPassword.PasswordChar:=#0
  Else
    edPassword.PasswordChar:='*';
end;

procedure TDlgAdd.btnBrowsClick(Sender: TObject);
var Temp:String;
begin
  If Not DlgSave.Execute Then Exit;
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
    FileNameCombo.Text:=DlgSave.FileName+Temp Else
    FileNameCombo.Text:=DlgSave.FileName;
end;

procedure TDlgAdd.BtnOkClick(Sender: TObject);
begin
  If FileNameCombo.Text<>''Then
    ModalResult:=mrOk Else 
    MessageBox(Handle,'Empty archive file name.','Error',MB_OK+MB_ICONSTOP);
end;

procedure TDlgAdd.cbEncodeHeadClick(Sender: TObject);
var State:Boolean;
begin
  State:=((cbEncodeHead.Checked)Or(cbEncodeFile.Checked))And
         ((cbCompressMethod.ItemIndex=0) Or (cbCompressMethod.ItemIndex=1) Or
          (cbCompressMethod.ItemIndex=2) Or (cbCompressMethod.ItemIndex=4));
  edPassword.Enabled:=State;
  cbShowPassword.Enabled:=State;
  rgEncode.Enabled:=State;
  If Not State Then
    edPassword.Clear;
end;

procedure TDlgAdd.cbCompressMethodChange(Sender: TObject);
Var State:Boolean;
begin
  With Sender As TComboBox Do
    Begin
      Case ItemIndex Of
        0:Begin
            State:=(cbEncodeHead.Checked)Or(cbEncodeFile.Checked);
            edPassword.Enabled:=State;
            cbShowPassword.Enabled:=State;
            rgEncode.Enabled:=State;
            cbEncodeFile.Enabled:=True;
            cbEncodeHead.Enabled:=True;
            btnAdvance.Enabled:=True;
            tbLevel.Max:=4;
          End;
        1:Begin
            cbEncodeHead.Enabled:=False;
            cbEncodeFile.Enabled:=False;
            edPassword.Enabled:=True;
            cbShowPassword.Enabled:=True;
            btnAdvance.Enabled:=True;
            tbLevel.Max:=3;
          End;
        2,3,5:Begin
                edPassword.Enabled:=True;
                cbShowPassword.Enabled:=True;
                cbEncodeHead.Enabled:=True;
                rgEncode.Enabled:=False;
                cbEncodeFile.Enabled:=False;
                btnAdvance.Enabled:=False;
                tbLevel.Max:=3;
              End;
      Else
        Begin
          edPassword.Enabled:=False;
          cbShowPassword.Enabled:=False;
          rgEncode.Enabled:=False;
          cbEncodeFile.Enabled:=False;
          cbEncodeHead.Enabled:=False;
          btnAdvance.Enabled:=False;
          tbLevel.Max:=3;
        End;
      End;
      //If FileNameCombo.Text='' Then Exit;
    End;
  Case cbCompressMethod.ItemIndex Of
    0:Begin If FileNameCombo.Text<>'' Then FileNameCombo.Text:=ChangeFileExt(FileNameCombo.Text,'.MZF'); dlgSave.Filter:='MZF Archive (*.mzf)|*.MZF'; End;
    1:Begin If FileNameCombo.Text<>'' Then FileNameCombo.Text:=ChangeFileExt(FileNameCombo.Text,'.7Z'); dlgSave.Filter:='7Z Archive (*.7Z)|*.7Z'; End;
    2:Begin If FileNameCombo.Text<>'' Then FileNameCombo.Text:=ChangeFileExt(FileNameCombo.Text,'.ZIP'); dlgSave.Filter:='ZIP Archive (*.zip)|*.ZIP'; End;
    3:Begin If FileNameCombo.Text<>'' Then FileNameCombo.Text:=ChangeFileExt(FileNameCombo.Text,'.BH'); dlgSave.Filter:='BH Archive (*.bh)|*.BH'; End;
    4:Begin If FileNameCombo.Text<>'' Then FileNameCombo.Text:=ChangeFileExt(FileNameCombo.Text,'.LHA'); dlgSave.Filter:='LHA Archive (*.lha)|*.LHA'; End;
    5:Begin If FileNameCombo.Text<>'' Then FileNameCombo.Text:=ChangeFileExt(FileNameCombo.Text,'.JAR'); dlgSave.Filter:='JAR Archive (*.jar)|*.JAR'; End;
    6:Begin If FileNameCombo.Text<>'' Then FileNameCombo.Text:=ChangeFileExt(FileNameCombo.Text,'.CAB'); dlgSave.Filter:='CAB Archive (*.cab)|*.CAB'; End;
    7:Begin If FileNameCombo.Text<>'' Then FileNameCombo.Text:=ChangeFileExt(FileNameCombo.Text,'.TAR'); dlgSave.Filter:='TAR Archive (*.tar)|*.TAR'; End;
  End;
end;

procedure TDlgAdd.btnAdvanceClick(Sender: TObject);
var Index:LongWord;
begin
  tbLevelChange(Sender);
  If cbCompressMethod.ItemIndex<>0 Then
    Exit;
  Application.CreateForm(TDlgMZFCompressOptions,DlgMZFCompressOptions);
  With DlgMZFCompressOptions Do
    Begin
      If lvFiles.Items.Count>0 Then
        For Index:=0 To lvFiles.Items.Count-1 Do
          AFiles.Add(lvFiles.Items.Item[Index].Caption);
      BtnCompute.Enabled:=lvFiles.Items.Count>0;
      cbDictSize.ItemIndex:=InsideCompressLevel.DictitionarySize;
      cbMathFinder.ItemIndex:=InsideCompressLevel.MathFinder;
      cbLitContextBits.ItemIndex:=InsideCompressLevel.LiteralContextBits;
      cbLitPosBits.ItemIndex:=InsideCompressLevel.LiteralPosBits;
      cbFastBytes.ItemIndex:=InsideCompressLevel.FastBytes-5;
      cbPosBits.ItemIndex:=InsideCompressLevel.PosBits;
      Case InsideCompressLevel.EncodeAlgorithm Of
        eaAES256:cbAlgorithm.ItemIndex:=0;
        eaMAD7:cbAlgorithm.ItemIndex:=1;
        eaIDEA:cbAlgorithm.ItemIndex:=2;
        //eaRC5:cbAlgorithm.ItemIndex:=3;
        //eaRC6:cbAlgorithm.ItemIndex:=4;
      End;
      cbPriority.ItemIndex:=InsideCompressLevel.Priority;
      cbAlgorithm.ItemIndex
    End;
  If DlgMZFCompressOptions.ShowModal=mrOk Then
    Begin
      With DlgMZFCompressOptions Do
        Begin
          InsideCompressLevel.DictitionarySize:=cbDictSize.ItemIndex;
          InsideCompressLevel.MathFinder:=cbMathFinder.ItemIndex;
          InsideCompressLevel.LiteralContextBits:=cbLitContextBits.ItemIndex;
          InsideCompressLevel.LiteralPosBits:=cbLitPosBits.ItemIndex;
          InsideCompressLevel.FastBytes:=cbFastBytes.ItemIndex+5;
          InsideCompressLevel.PosBits:=cbPosBits.ItemIndex;
          Case cbAlgorithm.ItemIndex Of
            0:InsideCompressLevel.EncodeAlgorithm:=eaAES256;
            1:InsideCompressLevel.EncodeAlgorithm:=eaMAD7;
            2:InsideCompressLevel.EncodeAlgorithm:=eaIDEA;
            //3:InsideCompressLevel.EncodeAlgorithm:=eaRC5;
            //4:InsideCompressLevel.EncodeAlgorithm:=eaRC6;
          End;
          InsideCompressLevel.Priority:=cbPriority.ItemIndex;
        End;
    End;
  DlgMZFCompressOptions.Free;
end;

procedure TDlgAdd.tbLevelChange(Sender: TObject);
begin
  If cbCompressMethod.ItemIndex<>0 Then Exit;
  Case tbLevel.Position Of
    0:Begin
        InsideCompressLevel.DictitionarySize:=16;
        InsideCompressLevel.MathFinder:=0;
        InsideCompressLevel.FastBytes:=5;
        InsideCompressLevel.LiteralContextBits:=8;
        InsideCompressLevel.LiteralPosBits:=4;
        InsideCompressLevel.PosBits:=4;
        InsideCompressLevel.EncodeAlgorithm:=eaAES256;
        InsideCompressLevel.Priority:=2;
      End;
    1:Begin
        InsideCompressLevel.DictitionarySize:=20;
        InsideCompressLevel.MathFinder:=0;
        InsideCompressLevel.FastBytes:=80;
        InsideCompressLevel.LiteralContextBits:=6;
        InsideCompressLevel.LiteralPosBits:=2;
        InsideCompressLevel.PosBits:=2;
        InsideCompressLevel.EncodeAlgorithm:=eaAES256;
        InsideCompressLevel.Priority:=2;
      End;
    2:Begin
        InsideCompressLevel.DictitionarySize:=24;
        InsideCompressLevel.MathFinder:=1;
        InsideCompressLevel.FastBytes:=128;
        InsideCompressLevel.LiteralContextBits:=3;
        InsideCompressLevel.LiteralPosBits:=1;
        InsideCompressLevel.PosBits:=2;
        InsideCompressLevel.EncodeAlgorithm:=eaAES256;
        InsideCompressLevel.Priority:=3;
      End;
    3:Begin
        InsideCompressLevel.DictitionarySize:=25;
        InsideCompressLevel.MathFinder:=1;
        InsideCompressLevel.FastBytes:=200;
        InsideCompressLevel.LiteralContextBits:=2;
        InsideCompressLevel.LiteralPosBits:=0;
        InsideCompressLevel.PosBits:=1;
        InsideCompressLevel.EncodeAlgorithm:=eaAES256;
        InsideCompressLevel.Priority:=3;
      End;
    4:Begin
        InsideCompressLevel.DictitionarySize:=28;
        InsideCompressLevel.MathFinder:=2;
        InsideCompressLevel.FastBytes:=273;
        InsideCompressLevel.LiteralContextBits:=0;
        InsideCompressLevel.LiteralPosBits:=0;
        InsideCompressLevel.PosBits:=0;
        InsideCompressLevel.EncodeAlgorithm:=eaAES256;
        InsideCompressLevel.Priority:=4;
      End;
  End;
end;

end.
