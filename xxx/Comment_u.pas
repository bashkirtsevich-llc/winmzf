unit Comment_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TDlgComment = class(TForm)
    MemComment: TMemo;
    BtnOk: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgComment: TDlgComment;

procedure ShowComment(Comment:String);

implementation

{$R *.dfm}

procedure ShowComment(Comment:String);
var DlgComment: TDlgComment;
begin
  DlgComment:=TDlgComment.Create(Application);
  DlgComment.MemComment.Clear;
  DlgComment.MemComment.Text:=Comment;
  DlgComment.ShowModal;
  DlgComment.Free;
end;

end.
