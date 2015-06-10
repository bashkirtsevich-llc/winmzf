unit EncodeDecode_u;            

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, Advanced;

type
  TdlgEncodeDecode = class(TForm)
    pcMain: TPageControl;
    tsGeneral: TTabSheet;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    edPassword: TEdit;
    Label2: TLabel;
    edConfirmPassword: TEdit;
    cbShowPassword: TCheckBox;
    tdOptions: TTabSheet;
    cbCopyToDestDir: TCheckBox;
    Label3: TLabel;
    edDestDir: TEdit;
    btnBrows: TButton;
    Label4: TLabel;
    cbAlgorithm: TComboBox;
    cbSetFileExtention: TCheckBox;
    edExtentions: TEdit;
    Label5: TLabel;
    cbMode: TComboBox;
    Label6: TLabel;
    procedure cbShowPasswordClick(Sender: TObject);
    procedure cbCopyToDestDirClick(Sender: TObject);
    procedure cbSetFileExtentionClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnBrowsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgEncodeDecode: TdlgEncodeDecode;

implementation

{$R *.dfm}

procedure TdlgEncodeDecode.cbShowPasswordClick(Sender: TObject);
begin
  If Not cbShowPassword.Checked Then
    Begin
      edPassword.PasswordChar:='*';
      edConfirmPassword.PasswordChar:='*';
    End Else
    Begin
      edPassword.PasswordChar:=#0;
      edConfirmPassword.PasswordChar:=#0;
    End;
end;

procedure TdlgEncodeDecode.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Windows','wndCoder',Caption);
  tsGeneral.Caption:=ReadFromLanguage('Tabs','tbGeneral',tsGeneral.Caption);
  tdOptions.Caption:=ReadFromLanguage('Tabs','tbOptions',tdOptions.Caption);
  Label1.Caption:=ReadFromLanguage('Labels','lbPassword',Label1.Caption);
  Label2.Caption:=ReadFromLanguage('Labels','lbConfirmPass',Label2.Caption);
  cbShowPassword.Caption:=ReadFromLanguage('Labels','lbShowPass',cbShowPassword.Caption);
  cbCopyToDestDir.Caption:=ReadFromLanguage('Labels','lbCopyToDestDir',cbCopyToDestDir.Caption);
  Label3.Caption:=ReadFromLanguage('Labels','lbDestDir',Label3.Caption);
  Label5.Caption:=ReadFromLanguage('Labels','lbMode',Label5.Caption);
  Label4.Caption:=ReadFromLanguage('Labels','lbEncAlg',Label4.Caption);
  cbSetFileExtention.Caption:=ReadFromLanguage('Labels','lbSetExt',cbSetFileExtention.Caption);
  Label6.Caption:=ReadFromLanguage('Labels','lbExt',Label6.Caption);
  cbMode.Items[0]:=ReadFromLanguage('Items','itEncode',cbMode.Items[0]);
  cbMode.Items[1]:=ReadFromLanguage('Items','itDecode',cbMode.Items[1]);
end;

procedure TdlgEncodeDecode.cbCopyToDestDirClick(Sender: TObject);
begin
  edDestDir.Enabled:=cbCopyToDestDir.Checked;
  Label3.Enabled:=cbCopyToDestDir.Checked;
  btnBrows.Enabled:=cbCopyToDestDir.Checked;
end;

procedure TdlgEncodeDecode.cbSetFileExtentionClick(Sender: TObject);
begin
  edExtentions.Enabled:=cbSetFileExtention.Checked;
  Label6.Enabled:=cbSetFileExtention.Checked;
end;

procedure TdlgEncodeDecode.btnOkClick(Sender: TObject);
begin
  If (edDestDir.Enabled)And(edDestDir.Text='') Then
    Begin
      MessageBox(Handle,'You must write destination dir','Error',MB_OK+MB_ICONEXCLAMATION);
      Exit;
    End;
  If (edPassword.Text='')Or(edConfirmPassword.Text='')Or
     (edPassword.Text<>edConfirmPassword.Text) Then
    Begin
      MessageBox(Handle,'Password error','Error',MB_OK+MB_ICONEXCLAMATION);
      Exit;
    End;
  ModalResult:=mrOk;
end;

procedure TdlgEncodeDecode.btnBrowsClick(Sender: TObject);
begin
  edDestDir.Text:=GetSelectedDir;
end;

end.
