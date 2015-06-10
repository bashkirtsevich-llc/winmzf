unit AdvancedConfig_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Advanced;

type
  TDlgMZFCompressOptions = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    gbLZMAConfig: TGroupBox;
    Label1: TLabel;
    cbDictSize: TComboBox;
    Label2: TLabel;
    cbMathFinder: TComboBox;
    Label3: TLabel;
    cbLitContextBits: TComboBox;
    Label4: TLabel;
    cbLitPosBits: TComboBox;
    Label5: TLabel;
    cbFastBytes: TComboBox;
    Label6: TLabel;
    cbPosBits: TComboBox;
    Label7: TLabel;
    cbAlgorithm: TComboBox;
    Label8: TLabel;
    cbPriority: TComboBox;
    BtnCompute: TBitBtn;
    Label9: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnComputeClick(Sender: TObject);
  private
    { Private declarations }
  public
    AFiles:TStringList;
    { Public declarations }
  end;

var
  DlgMZFCompressOptions: TDlgMZFCompressOptions;

implementation

uses Compute_u;

{$R *.dfm}

procedure TDlgMZFCompressOptions.FormCreate(Sender: TObject);
Var i:Word;
begin
  AFiles:=TStringList.Create;
  For i:=5 To 273 Do
    cbFastBytes.Items.Add(IntToStr(i));
  cbFastBytes.ItemIndex:=0;
end;

procedure TDlgMZFCompressOptions.BtnComputeClick(Sender: TObject);
var Index:LongWord;
begin
  Application.CreateForm(TDlgCompute,DlgCompute);
  With DlgCompute Do
    Begin
      SelectedStamp.DictitionarySize:=cbDictSize.ItemIndex;
      SelectedStamp.MathFinder:=cbMathFinder.ItemIndex;
      SelectedStamp.LiteralContextBits:=cbLitContextBits.ItemIndex;
      SelectedStamp.LiteralPosBits:=cbLitPosBits.ItemIndex;
      SelectedStamp.FastBytes:=cbFastBytes.ItemIndex+5;
      SelectedStamp.PosBits:=cbPosBits.ItemIndex;
      Case cbAlgorithm.ItemIndex Of
        0:SelectedStamp.EncodeAlgorithm:=eaAES256;
        1:SelectedStamp.EncodeAlgorithm:=eaMAD7;
        2:SelectedStamp.EncodeAlgorithm:=eaIDEA;
        //3:SelectedStamp.EncodeAlgorithm:=eaRC5;
        //4:SelectedStamp.EncodeAlgorithm:=eaRC6;
      End;
      SelectedStamp.Priority:=cbPriority.ItemIndex;
    End;
  If AFiles.Count>0 Then
    For Index:=0 To AFiles.Count-1 Do
      DlgCompute.Files.Add(AFiles.Strings[Index]);
  DlgCompute.ShowModal;
  DlgCompute.Free;
end;

end.
