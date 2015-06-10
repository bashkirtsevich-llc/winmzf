unit EncodeProgress_u;

interface
                                                                
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons, Advanced;

type
  TdlgOperationsProgress = class(TForm)
    lbCurrentFile: TLabel;
    pbEncoding: TProgressBar;
    btnCancel: TBitBtn;
    lbProgerss: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    Abort:Boolean;
    { Public declarations }
  end;

var
  dlgOperationsProgress: TdlgOperationsProgress;

implementation

{$R *.dfm}

procedure TdlgOperationsProgress.FormCreate(Sender: TObject);
begin
  Abort:=False;
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
end;

procedure TdlgOperationsProgress.btnCancelClick(Sender: TObject);
begin
  Abort:=True;
end;

end.
