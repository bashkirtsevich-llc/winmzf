unit OverWrite_u;
                                                                  
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TdlgOverWrite = class(TForm)
    gbInfo: TGroupBox;
    Label1: TLabel;
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
