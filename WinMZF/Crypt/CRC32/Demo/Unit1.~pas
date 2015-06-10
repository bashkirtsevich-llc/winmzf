unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    btnFileCRC: TButton;
    Memo1: TMemo;
    btnClearMemo: TButton;
    Edit2: TEdit;
    btnMemoryCRC: TButton;
    procedure Button2Click(Sender: TObject);
    procedure btnClearMemoClick(Sender: TObject);
    procedure btnFileCRCClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnMemoryCRCClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses SZCRC32;

{$R *.dfm}
          
procedure TForm1.Button2Click(Sender: TObject);
begin
  OpenDialog1.FileName:=Edit1.Text;

  if OpenDialog1.Execute then
  begin
    Edit1.Text:=OpenDialog1.FileName;
  end;
end;

procedure TForm1.btnClearMemoClick(Sender: TObject);
begin
  Memo1.Clear
end;

procedure TForm1.btnFileCRCClick(Sender: TObject);
var
  c0,c1,c2,c3: DWORD;
  p: int64;
  MemStream: TMemoryStream;
  Filename: string;

begin

  if SZCRC32Test
    then Memo1.Lines.Add('CRC32 algorithm PASS.')
    else Memo1.Lines.Add('CRC32 algorithm FAILS!');

  Memo1.Lines.Add('');

  FileName:=Edit1.Text;

  c0:=SZCRC32File(FileName);


  Memo1.Lines.Add(
    format('File is %s',[FileName]));

  Memo1.Lines.Add('');

  Memo1.Lines.Add(
  format('CRC32 is: $%.8x',[c0]));

  Memo1.Lines.Add('');
  ///////////////////////////////////////////
  // Partial Calculating CRC32 of the file //
  //      and CRC32 update testing         //
  ///////////////////////////////////////////
 
  MemStream:=TMemoryStream.Create;

  try
    MemStream.LoadFromFile(FileName);

    p:=MemStream.Size div 2;

    // CRC32 from middle position to the end of the file
    MemStream.Position := p ;
    c2:=SZCrc32FullStream(MemStream);

    // CRC32 from begining to the middle of the file
    MemStream.Position:=0;
    // Set size of the stream to the middle and calculate
    MemStream.SetSize(p);
    c1:= SZCrc32FullStream(MemStream);

    ////////////////////////
    // Partial CRC32 test //
    ////////////////////////

    // Read the file again
    MemStream.LoadFromFile(FileName);
    // Get the first crc and prepare to update and
    // update from the middle
    // CRC32 from middle position to the end of the file
    MemStream.Position := p ;
    c3:= SZCRC32UpdateStream(MemStream, not c1);
    // Finish with PKzip comaptibility
    c3:= not c3;

    Memo1.Lines.Add(
      format('The first  part is: $%.8x',[c1]));

    Memo1.Lines.Add(
      format('The second part is: $%.8x',[c2]));

    Memo1.Lines.Add('');

    Memo1.Lines.Add(
      format('Updating CRC32 result is: $%.8x',[c3]));

    Memo1.Lines.Add('');
    
    if c0=c3 then
      Memo1.Lines.Add('Updating CRC32 algorithm PASS.')
    else
      Memo1.Lines.Add('Updating CRC32 algorithm FAILS!')


  finally
    MemStream.free;
  end;

  Memo1.Lines.Add(StringOfChar('-',40));


end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  btnFileCRC.Click
end;

procedure TForm1.btnMemoryCRCClick(Sender: TObject);
// Testing the 32-bit CRC algorithm
// for memory data
var
  CRC32: DWORD;
  P : Pointer;
begin

  P:= @Edit2.text[1];

  CRC32 := SZCRC32Full(P, length(Edit2.text));

  Memo1.Lines.Add(Edit2.text);
  Memo1.Lines.Add('');
  Memo1.Lines.Add(format('CRC of a text above is: %.8x',[CRC32]));
  Memo1.Lines.Add(StringOfChar('-',40));
end;


end.
