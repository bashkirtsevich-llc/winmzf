unit Tips_u;
                                                               
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, IniFiles, ExtCtrls, Advanced;

type
  TdlgTips = class(TForm)
    BtnNext: TBitBtn;
    BtnClose: TBitBtn;
    pnlMain: TPanel;
    shpBar: TShape;
    lbCaption: TLabel;
    lbTip: TLabel;
    ImgLamp: TImage;
    procedure FormCreate(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgTips: TdlgTips;
  Count:LongInt;
  TipNum:LongInt;
implementation

{$R *.dfm}

procedure ReadTips;
var Ini:TIniFile;
begin
  Ini:=TIniFile.Create(ExtractFileDir(Application.Exename)+'\Tips.ini');
  Count:=Ini.ReadInteger('Tips','Count',0);
  dlgTips.lbCaption.Caption:=Ini.ReadString('Tips','Caption','Did you know that...');
  dlgTips.BtnNext.Enabled:=(Count>1);
  if Count>0 then
    dlgTips.lbTip.Caption:=Ini.ReadString('Tips','Tip0','Tips not found!');
  Ini.Free;
end;

procedure ReadTip;
var Ini:TIniFile;
begin
  Ini:=TIniFile.Create(ExtractFileDir(Application.Exename)+'\Tips.ini');
    dlgTips.lbTip.Caption:=Ini.ReadString('Tips','Tip'+IntToStr(TipNum),'Tip not found!');
  Ini.Free;
end;

procedure TdlgTips.BtnNextClick(Sender: TObject);
begin
  Inc(TipNum);
  if TipNum>=Count then
    TipNum:=0;
  ReadTip;
end;

procedure TdlgTips.FormCreate(Sender: TObject);
begin
  TipNum:=0;
  Count:=0;
  ReadTips;
  Caption:=ReadFromLanguage('Windows','wndTips',Caption);
  btnClose.Caption:=ReadFromLanguage('Buttons','btnClose',btnClose.Caption);
  btnNext.Caption:=ReadFromLanguage('Buttons','btnNext',btnNext.Caption);
  lbCaption.Caption:=ReadFromLanguage('Labels','lbTips',lbCaption.Caption);
  //lbTip.Caption:=ReadFromLanguage('Labels','lbTipPlace',lbTip.Caption);
end;

end.
