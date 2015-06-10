unit Progress_u;

interface                                                      

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, Advanced;

type
  TdlgProgress = class(TForm)
    btnAbort: TBitBtn;
    pbArchive: TProgressBar;
    pbFile: TProgressBar;
    lbCurrent: TLabel;
    lbFileIndex: TLabel;
    lbTotal: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
  private
    { Private declarations }
  public
    Abort:Boolean;
    { Public declarations }
  end;

var
  dlgProgress: TdlgProgress;

implementation

{$R *.dfm}

procedure TdlgProgress.FormCreate(Sender: TObject);
begin
  Abort:=False;
  btnAbort.Caption:=ReadFromLanguage('Buttons','btnAbort',btnAbort.Caption);
  lbFileIndex.Caption:=Format(ReadFromLanguage('Status','FileIndex','File index %d'),[0]);
  lbTotal.Caption:=Format(ReadFromLanguage('Status','TotalProgress','Total progress %d'),[0])+'%';
  lbCurrent.Caption:=Format(ReadFromLanguage('Status','CurrentProgress','Current progress %d'),[0])+'%';
end;

procedure TdlgProgress.btnAbortClick(Sender: TObject);
begin
  Abort:=True;
end;

end.
