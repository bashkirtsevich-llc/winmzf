unit DragAndDrop_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, CommCtrl;

type
  TDlgOperation = class(TForm)
    Label1: TLabel;
    Separator: TBevel;
    BtnOpen: TSpeedButton;
    BtnAdd: TSpeedButton;
    BtnCopy: TSpeedButton;
    BtnCancel: TSpeedButton;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgOperation: TDlgOperation;

implementation

{$R *.dfm}

procedure TDlgOperation.BtnCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TDlgOperation.BtnOpenClick(Sender: TObject);
begin
  ModalResult:=mrYes;
end;

procedure TDlgOperation.BtnAddClick(Sender: TObject);
begin
  ModalResult:=mrAll;
end;

procedure TDlgOperation.BtnCopyClick(Sender: TObject);
begin
  Modalresult:=mrYesToAll;
end;

procedure TDlgOperation.FormCreate(Sender: TObject);
{Var
  himlIconsBIG: HIMAGELIST;
  hIconLib: HModule;
  ICON: HICON;
  ImgList:TImageList;
procedure AddIconToBIGList(IconName: PChar);
begin
  ICON:=LoadIcon(hIconLib, IconName);                
  ImageList_AddIcon(himlIconsBIG, ICON);
end;}
begin
{  ImgList:=TImageList.CreateSize(32,32);
  HIconLib:=hInstance;
  himlIconsBig := ImageList_Create(32, 32, ILC_COLOR32 or ILC_MASK, 4, 0);
  AddIconToBIGList('Z_BTNOPENENABLE');
  AddIconToBIGList('Z_BTNADDENABLE');
  AddIconToBIGList('Z_BTNCOPYENABLE');
  ImgList.Handle:=himlIconsBIG;
  ImgList.Draw(BtnOpen.Glyph.Canvas,0,0,0);
  ImgList.Draw(BtnAdd.Glyph.Canvas,0,0,1);
  ImgList.Draw(BtnCopy.Glyph.Canvas,0,0,2);}
end;

end.
