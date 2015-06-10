unit Info_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgInfo: TdlgInfo;

implementation

{$R *.dfm}

end.
