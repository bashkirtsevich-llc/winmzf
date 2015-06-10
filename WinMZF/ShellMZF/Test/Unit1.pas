unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
 i,j: Word;
begin
j:=ParamCount;
 if j > 0 then
  for i:=1 to j do Memo1.Lines.Add('Param '+IntToStr(i)+': '+ParamStr(i))
  else MEMo1.Lines.Add('NO PARAMS')
end;

end.
 