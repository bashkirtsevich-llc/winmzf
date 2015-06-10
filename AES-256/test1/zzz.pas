unit zzz;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, aes_type, aes_cbc, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TKey128 = array[0..31] of char;

const
  BufSize = $4000;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  SampleIV: TAESBlock;
  buffer  : array[0..BufSize-1] of byte;
  InStream,OutStream:TMemoryStream;
  Key:TKey128;
  i:Byte;
  Err:integer;
  ctx: TAESContext;
  n:Word;
  s:string;
begin
  randomize;
  for i := 0 to 15 do
    SampleIV[i] := Byte(Edit1.Text[i]);
  for i := 0 to 31 do
    Key[i]:=Edit2.Text[i];
  InStream:=TMemoryStream.Create;
  OutStream:=TMemoryStream.Create;
  //InStream.LoadFromFile('InFile');
  s:=memo1.Text;
  InStream.Write(s,Length(s));
  InStream.Position:=0;

  err := AES_CBC_Init_Encr(key, 256, SampleIV, ctx);
  if err<>0 then
    Begin
      ShowMessage('Encrypt initialize error');
      Exit;
    End;
    //OutStream.Write(SampleIV,SizeOf(TAESBlock));
  while instream.Position<>InStream.Size Do
    Begin
      n:=InStream.Read(buffer,BufSize);
      err := AES_CBC_Encrypt(@buffer, @buffer, n, ctx);
      If err<>0 Then
        Begin
          ShowMessage('Error');
          Exit;
        End;
      OutStream.Write(buffer,n);
    End;

  OutStream.SaveToFile('Result.txt');
  instream.Free;
  outstream.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  instream,outstream:TMemoryStream;
  err:integer;
  ctx: TAESContext;
  IV: TAESBlock;
  key: TKey128;
  n:integer;
  buffer  : array[0..BufSize-1] of byte;
begin
  key:='12345678901234511111111111111112';
  instream:=TMemoryStream.Create;
  OutStream:=TMemoryStream.Create;
  InStream.LoadFromFile('popka.txt');

  instream.Read(IV,SizeOf(TAESBlock));
  err := AES_CBC_Init_Decr(Key, 256, IV, ctx);
  if err<>0 then begin
    ShowMessage('Decrypt init error: ');
    Exit;
  end;
  while instream.Position<>InStream.Size Do
    Begin
      n:=InStream.Read(Buffer,BufSize);
      err := AES_CBC_Decrypt(@buffer, @buffer, n, ctx);
      If err<>0 Then
        Begin
          showmessage('Error');
          exit;
        End;
      OutStream.Write(buffer,n);
    End;

  outstream.SaveToFile('popentsia.txt');
  instream.Free;
  outstream.Free;
end;

end.
