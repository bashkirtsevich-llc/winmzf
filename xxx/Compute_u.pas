unit Compute_u;

interface

uses
  SysUtils, Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeEngine, Series, ExtCtrls, TeeProcs, Chart, StdCtrls, Buttons,
  Advanced, Compress_u, ULZMAEncoder, ULZMACommon;

type
  TDlgCompute = class(TForm)
    LZMAChart: TChart;
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    gbOptions: TGroupBox;
    gbCurrentMode: TGroupBox;
    Label1: TLabel;
    cbPriority: TComboBox;
    btnCompute: TBitBtn;
    Level: TBarSeries;
    procedure FormCreate(Sender: TObject);
    procedure SearchDir(Dir: string);
    procedure btnComputeClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    function CompressStream(InStream,OutStream:Tstream;CompressLevel:TMZFCompressionLevel):Boolean;
  private
    { Private declarations }
  public
    SelectedStamp:TMZFCompressionLevel;
    Files:TStringList;
    { Public declarations }
  end;

var
  DlgCompute: TDlgCompute;
  Stop:Boolean;
implementation

{$R *.dfm}

procedure TDlgCompute.SearchDir(Dir: string);
var 
  SR: TSearchRec; 
  FindRes: Integer; 
begin 
  FindRes := FindFirst(Dir + '*.*', faAnyFile, SR); 
       while FindRes = 0 do 
          begin
            if ((SR.Attr and faDirectory) = faDirectory) and 
            ((SR.Name = '.') or (SR.Name = '..')) then 
               begin 
                 FindRes := FindNext(SR); 
                 Continue; 
               end; 
            if ((SR.Attr and faDirectory) = faDirectory) then 
              begin 
                SearchDir(Dir + SR.Name + '\');
                FindRes := FindNext(SR); 
                Continue; 
              end; 
            Files.Add(Dir + SR.Name);//Add to list
            FindRes := FindNext(SR);
          end;
  FindClose(FindRes);
end;

procedure TDlgCompute.FormCreate(Sender: TObject);
begin
  Stop:=False;
  Files:=TStringList.Create;
end;

function TDlgCompute.CompressStream(InStream,OutStream:Tstream;CompressLevel:TMZFCompressionLevel):Boolean;
Var Data:Byte;
    encoder:TLZMAEncoder;
    i:Byte;
Begin
  InStream.Position:=0;

  encoder:=TLZMAEncoder.Create;
  encoder.SetAlgorithm(1);
  encoder.SetDictionarySize(CompressLevel.DictitionarySize);
  encoder.SetMatchFinder(CompressLevel.MathFinder);
  encoder.SeNumFastBytes(CompressLevel.FastBytes);
  encoder.SetLcLpPb(CompressLevel.LiteralContextBits,CompressLevel.LiteralPosBits,CompressLevel.PosBits);
  
  encoder.WriteCoderProperties(outStream);
  for i := 0 to 7 do
    WriteByte(outStream,(InStream.Size shr (8 * i)) and $FF);
  encoder.Code(InStream,OutStream,-1,-1);

  Data:=0;
End;

procedure TDlgCompute.btnComputeClick(Sender: TObject);
Var InStream,OutStream,WAD:TMemoryStream;
    FileIndex:LongWord;
    aTPriority,aPPriority:Integer;
    FileDir:String;
    OperationIndex:Byte;
    Results:Array[0..5]Of Byte;
    InSize,OutSize:LongWord;
begin
  BtnOk.Enabled:=False;
  BtnCompute.Enabled:=False;
  cbPriority.Enabled:=False;
  Case cbPriority.ItemIndex Of
    0:Begin aTPriority:=THREAD_PRIORITY_IDLE; aPPriority:=IDLE_PRIORITY_CLASS; End;
    1:Begin aTPriority:=THREAD_PRIORITY_BELOW_NORMAL; aPPriority:=NORMAL_PRIORITY_CLASS; End;
    2:Begin aTPriority:=THREAD_PRIORITY_NORMAL; aPPriority:=NORMAL_PRIORITY_CLASS; End;
    3:Begin aTPriority:=THREAD_PRIORITY_HIGHEST; aPPriority:=HIGH_PRIORITY_CLASS; End;
    4:Begin aTPriority:=THREAD_PRIORITY_TIME_CRITICAL; aPPriority:=REALTIME_PRIORITY_CLASS; End;
  End;
  SetPriority(aPPriority,aTPriority);
  For FileIndex:=0 To Files.Count-1 Do
    Begin
      If Stop Then Exit;
      FileDir:=ExtractFileDir(Files.Strings[FileIndex]);
      If FileDir[Length(FileDir)]<>'\' Then FileDir:=FileDir+'\';
      If ExtractFileExt(Files.Strings[FileIndex])='.*' Then
        Begin
          //showmessage(FileDir);
          SearchDir(FileDir);
          //Files.Delete(FileIndex);
        End;
    End;
  FileIndex:=0;
  While FileIndex<=Files.Count-1 Do
    Begin
      If Stop Then Exit;
      If ExtractFileExt(Files.Strings[FileIndex])='.*' Then
        Begin
          Files.Delete(FileIndex);
        End Else
      Inc(FileIndex);
    End;
  WAD:=TMemoryStream.Create;

  For FileIndex:=0 To Files.Count-1 Do
    Begin
      If Stop Then Exit;
      InStream:=TMemoryStream.Create;
      InStream.LoadFromFile(Files.Strings[FileIndex]);
      InStream.SaveToStream(WAD);
      //WAD.Position:=WAD.Size;
      InStream.Free;
    End;
  Application.ProcessMessages;
  InStream:=TMemoryStream.Create;
  Wad.Position:=0;
  CompressStream(WAD,InStream,SelectedStamp);
  Results[0]:=100-GetPercentDone(0,InStream.Size,WAD.Size);
  Level.Add(Results[0],'Current',clBlue);
  InStream.Free;
  Application.ProcessMessages;
  For OperationIndex:=1 To 5 Do
    Begin
      Case OperationIndex Of
      1:Begin
          SelectedStamp.DictitionarySize:=16;
          SelectedStamp.MathFinder:=0;
          SelectedStamp.FastBytes:=5;
          SelectedStamp.LiteralContextBits:=8;
          SelectedStamp.LiteralPosBits:=4;
          SelectedStamp.PosBits:=4;
          SelectedStamp.EncodeAlgorithm:=eaAES256;
          SelectedStamp.Priority:=2;
        End;
      2:Begin
          SelectedStamp.DictitionarySize:=20;
          SelectedStamp.MathFinder:=0;
          SelectedStamp.FastBytes:=80;
          SelectedStamp.LiteralContextBits:=6;
          SelectedStamp.LiteralPosBits:=2;
          SelectedStamp.PosBits:=2;
          SelectedStamp.EncodeAlgorithm:=eaAES256;
          SelectedStamp.Priority:=2;
        End;
      3:Begin
          SelectedStamp.DictitionarySize:=24;
          SelectedStamp.MathFinder:=1;
          SelectedStamp.FastBytes:=128;
          SelectedStamp.LiteralContextBits:=3;
          SelectedStamp.LiteralPosBits:=1;
          SelectedStamp.PosBits:=2;
          SelectedStamp.EncodeAlgorithm:=eaAES256;
          SelectedStamp.Priority:=3;
        End;
      4:Begin
          SelectedStamp.DictitionarySize:=25;
          SelectedStamp.MathFinder:=1;
          SelectedStamp.FastBytes:=200;
          SelectedStamp.LiteralContextBits:=2;
          SelectedStamp.LiteralPosBits:=0;
          SelectedStamp.PosBits:=1;
          SelectedStamp.EncodeAlgorithm:=eaAES256;
          SelectedStamp.Priority:=3;
        End;
      5:Begin
          SelectedStamp.DictitionarySize:=28;
          SelectedStamp.MathFinder:=2;
          SelectedStamp.FastBytes:=273;
          SelectedStamp.LiteralContextBits:=0;
          SelectedStamp.LiteralPosBits:=0;
          SelectedStamp.PosBits:=0;
          SelectedStamp.EncodeAlgorithm:=eaAES256;
          SelectedStamp.Priority:=4;
        End;
      End;
      Wad.Position:=0;
      Application.ProcessMessages;
      InStream:=TMemoryStream.Create;
      CompressStream(WAD,InStream,SelectedStamp);
      Results[OperationIndex]:=100-GetPercentDone(0,InStream.Size,WAD.Size);
      Level.Add(Results[OperationIndex],Format('Level %d',[OperationIndex]),clRed);
      InStream.Free;
    End;



  WAD.Free;
  SetPriority(NORMAL_PRIORITY_CLASS,THREAD_PRIORITY_NORMAL);
  BtnOk.Enabled:=True;
end;

procedure TDlgCompute.BtnCancelClick(Sender: TObject);
begin
  Stop:=True;
  ModalResult:=mrCancel;
end;

end.
