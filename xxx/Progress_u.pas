unit Progress_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Gauges, ComCtrls;

type
  TDlgProgress = class(TForm)
    BtnCancel: TBitBtn;
    AnimOperation: TAnimate;
    gFile: TProgressBar;
    gArchive: TProgressBar;
    procedure BtnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    Abort:Boolean;
    { Public declarations }
  end;

var
  DlgProgress: TDlgProgress;

implementation

{$R *.dfm}
{$R AVIS.RES}

procedure TDlgProgress.BtnCancelClick(Sender: TObject);
begin
  Abort:=True;
  Close;
  Self.Parent.Enabled:=True;
end;

procedure TDlgProgress.FormCreate(Sender: TObject);
begin
  Abort:=False;
end;

procedure TDlgProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  BtnCancelClick(Sender);
end;

end.
