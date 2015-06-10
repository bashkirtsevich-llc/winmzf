unit OverWrite_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TDlgOverWrite = class(TForm)
    BtnCancel: TBitBtn;
    cbSetToAll: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    gbFileInfo: TGroupBox;
    BtnYes: TSpeedButton;
    BtnNo: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure BtnYesClick(Sender: TObject);
    procedure BtnNoClick(Sender: TObject);
  private
    { Private declarations }
  public
    OverWrite:Boolean;
    { Public declarations }
  end;

var
  DlgOverWrite: TDlgOverWrite;

implementation

{$R *.dfm}

procedure TDlgOverWrite.BtnYesClick(Sender: TObject);
begin
  OverWrite:=True;
  Close;
end;

procedure TDlgOverWrite.BtnNoClick(Sender: TObject);
begin
  OverWrite:=False;
  Close;
end;

end.
