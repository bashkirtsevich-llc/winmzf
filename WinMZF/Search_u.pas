unit Search_u;                                              

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Advanced;

type
  TdlgFindConfig = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    gbCriteria: TGroupBox;
    edFileName: TEdit;
    btnBrows: TButton;
    edDir: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    procedure btnBrowsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgFindConfig: TdlgFindConfig;

implementation

{$R *.dfm}

procedure TdlgFindConfig.btnBrowsClick(Sender: TObject);
var Temp:String;
begin
  Temp:=GetSelectedDir;
  If Temp='' Then
    Exit;
  If Temp[Length(Temp)]<>'\' Then
    Temp:=Temp+'\';
  edDir.Text:=Temp;
end;

procedure TdlgFindConfig.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Windows','wndFindFile',Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  Label1.Caption:=ReadFromLanguage('Labels','lbFileName',Label1.Caption);
  Label2.Caption:=ReadFromLanguage('Labels','lbDir',Label2.Caption);
end;

end.
