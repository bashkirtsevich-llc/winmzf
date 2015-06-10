unit MakeSFX_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Advanced;

type
  TdlgMakeSFX = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    gbMain: TGroupBox;
    edSourceArchive: TEdit;
    edDestName: TEdit;
    gbAdvanced: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    btnOpenArchive: TButton;
    btnSaveSFX: TButton;
    DlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    edSFXCaption: TEdit;
    edExtractDir: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    procedure btnOpenArchiveClick(Sender: TObject);
    procedure btnSaveSFXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgMakeSFX: TdlgMakeSFX;

implementation

{$R *.dfm}

procedure TdlgMakeSFX.btnOpenArchiveClick(Sender: TObject);
begin
  If DlgOpen.Execute Then
    Begin
      edSourceArchive.Text:=DlgOpen.FileName;
      edDestName.Text:=ChangeFileExt(edSourceArchive.Text,'.exe');
    End;
end;

procedure TdlgMakeSFX.btnSaveSFXClick(Sender: TObject);
begin
  If dlgSave.Execute Then
    edDestName.Text:=dlgSave.FileName;
end;

procedure TdlgMakeSFX.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Windows','wndMakeSFX',Caption);
  gbMain.Caption:=ReadFromLanguage('Labels','lbSFXOptions',gbMain.Caption);
  Label1.Caption:=ReadFromLanguage('Labels','lbSource',Label1.Caption);
  Label2.Caption:=ReadFromLanguage('Labels','lbSFXName',Label2.Caption);
  gbAdvanced.Caption:=ReadFromLanguage('Labels','lbAdvanced',gbAdvanced.Caption);
  Label3.Caption:=ReadFromLanguage('Labels','lbSFXCaption',Label3.Caption);
  Label4.Caption:=ReadFromLanguage('Labels','lbExtractDir',Label4.Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  dlgOpen.Title:=ReadFromLanguage('Dialogs','dlgOpen',dlgOpen.Title);
  dlgSave.Title:=ReadFromLanguage('Dialogs','dlgSavePE',dlgSave.Title);
end;

end.
