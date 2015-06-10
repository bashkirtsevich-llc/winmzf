unit Report_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons, ImgList, ClipBrd, Advanced;

type
  TwndMessages = class(TForm)
    lvMessages: TListView;
    pnlBtnPlace: TPanel;
    sbStatus: TStatusBar;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    btnClose: TBitBtn;
    btnCopy: TBitBtn;
    imglistIcons: TImageList;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCopyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  wndMessages: TwndMessages;
  
const
  IT_EVENT =0;
  IT_INFO  =1;
  IT_ERROR =2;

procedure DisplayMessage(Msg:String;ImgType:Byte);  
  
implementation

{$R *.dfm}

procedure DisplayMessage(Msg:String;ImgType:Byte);
var Item:TListItem;
begin
  wndMessages.Show;
  if Msg<>'' then
    Begin
      Item:=wndMessages.lvMessages.Items.Add;
      Item.Caption:=TimeToStr(Time);
      Item.SubItems.Add(Msg);
      Item.ImageIndex:=ImgType;
    End;
end;

procedure TwndMessages.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TwndMessages.btnCopyClick(Sender: TObject);
Var Index:LongWord;
    Text:String;
begin
  ClipBoard.Clear;
  Text:='';
  for Index := 0 to Self.lvMessages.Items.Count - 1 do
    With Self.lvMessages.Items[Index] Do
      Text:=Text+Caption+'|'+Subitems[0]+#13;
  ClipBoard.AsText:=Text;
end;

procedure TwndMessages.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.lvMessages.Clear;
end;

procedure TwndMessages.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Windows','wndMessages',Caption);
  btnCopy.Caption:=ReadFromLanguage('Buttons','btnCopy',btnCopy.Caption);
  btnClose.Caption:=ReadFromLanguage('Buttons','bntClose',btnClose.Caption);
  lvMessages.Columns[0].Caption:=ReadFromLanguage('ListItems','liTime',lvMessages.Columns[0].Caption);
  lvMessages.Columns[1].Caption:=ReadFromLanguage('ListItems','liMessage',lvMessages.Columns[1].Caption);
end;

end.
