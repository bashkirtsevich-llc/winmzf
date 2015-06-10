unit About_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Advanced;

type
  TdlgAbout = class(TForm)
    gbCopyRight: TGroupBox;
    imgIcon: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    imgLogo: TImage;
    btnOk: TBitBtn;
    Label1: TLabel;
    memThanks: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgAbout: TdlgAbout;

implementation

{$R *.dfm}
{$R Logo.res}

procedure TdlgAbout.FormCreate(Sender: TObject);
begin
  //imgIcon.Picture.Icon.Handle:=Application.Icon.Handle;
  imgLogo.Picture.Bitmap.LoadFromResourceName(hInstance,'LOGO');
  Caption:=ReadFromLanguage('Menu','mnuAbout',Caption)+' WinMZF';
  Label2.Caption:=ReadFromLanguage('Labels','lbCopyright',Label2.Caption);
  Label3.Caption:=ReadFromLanguage('Labels','lbThanks',Label3.Caption);
end;

end.
