unit Info_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Advanced;

type
  TdlgInfo = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    leFileName: TLabeledEdit;
    leAuthor: TLabeledEdit;
    leVersion: TLabeledEdit;
    leDate: TLabeledEdit;
    lbComment: TLabel;
    memComment: TMemo;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgInfo: TdlgInfo;

implementation

{$R *.dfm}

procedure TdlgInfo.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Skin','Caption',Caption);
  btnOk.Caption:=ReadFromLanguage('Buttons','btnOk',btnOk.Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  leFileName.EditLabel.Caption:=ReadFromLanguage('Skin','lbSkin',leFileName.EditLabel.Caption);
  leAuthor.EditLabel.Caption:=ReadFromLanguage('Skin','lbAuthor',leAuthor.EditLabel.Caption);
  leVersion.EditLabel.Caption:=ReadFromLanguage('Skin','lbVersion',leVersion.EditLabel.Caption);
  leDate.EditLabel.Caption:=ReadFromLanguage('Skin','lbCreate',leDate.EditLabel.Caption);
  lbComment.Caption:=ReadFromLanguage('Skin','lbComment',lbComment.Caption);
end;

end.
