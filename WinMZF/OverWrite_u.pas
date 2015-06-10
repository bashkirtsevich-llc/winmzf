unit OverWrite_u;
                                                                  
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Advanced;

type
  TdlgOverWrite = class(TForm)
    gbInfo: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    btnYes: TBitBtn;
    btnNo: TBitBtn;
    btnYessToAll: TBitBtn;
    btnNoToAll: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    procedure btnYesClick(Sender: TObject);
    procedure btnNoClick(Sender: TObject);
    procedure btnYessToAllClick(Sender: TObject);
    procedure btnNoToAllClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    OverWrite,All:Boolean;
    { Public declarations }
  end;

var
  dlgOverWrite: TdlgOverWrite;

implementation

{$R *.dfm}

procedure TdlgOverWrite.btnYesClick(Sender: TObject);
begin
  OverWrite:=True;
  Close;
end;

procedure TdlgOverWrite.btnNoClick(Sender: TObject);
begin
  OverWrite:=False;
  Close;
end;

procedure TdlgOverWrite.btnYessToAllClick(Sender: TObject);
begin
  OverWrite:=True;
  All:=True;
  Close;
end;

procedure TdlgOverWrite.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Windows','wndOverWrite',Caption);
  gbInfo.Caption:=ReadFromLanguage('Labels','lbFileInfo',gbInfo.Caption);
  Label8.Caption:=ReadFromLanguage('Labels','lbOverWrite',Label8.Caption);
  btnYes.Caption:=ReadFromLanguage('Buttons','btnYes',btnYes.Caption);
  btnNo.Caption:=ReadFromLanguage('Buttons','btnNo',btnNo.Caption);
  btnYessToAll.Caption:=ReadFromLanguage('Buttons','btnYesAll',btnYessToAll.Caption);
  btnNoToAll.Caption:=ReadFromLanguage('Buttons','btnNoAll',btnNoToAll.Caption);
  btnHelp.Caption:=ReadFromLanguage('Buttons','btnHelp',btnHelp.Caption);
end;

procedure TdlgOverWrite.btnNoToAllClick(Sender: TObject);
begin
  OverWrite:=True;
  All:=True;
  Close;
end;

procedure TdlgOverWrite.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.
