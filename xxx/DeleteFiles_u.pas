unit DeleteFiles_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Gauges,ExtCtrls,Advanced, ComCtrls;

type
  TDlgDeleteFiles = class(TForm)
    BtnStart: TBitBtn;
    BtnAbort: TBitBtn;
    gbDeleteOptions: TGroupBox;
    cbFillZeroBytes: TCheckBox;
    cbUseFilter: TCheckBox;
    gbFilter: TGroupBox;
    cbExtFilter: TComboBox;
    cbUseExtFilter: TCheckBox;
    cbSizeFilter: TCheckBox;
    edSize: TEdit;
    cbSizeDivide: TComboBox;
    cbMode: TComboBox;
    FillGauge: TProgressBar;
    GeneralGauge: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnAbortClick(Sender: TObject);
    procedure cbUseFilterClick(Sender: TObject);
    procedure cbFillZeroBytesClick(Sender: TObject);
    procedure cbUseExtFilterClick(Sender: TObject);
    procedure edSizeKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbSizeFilterClick(Sender: TObject);
  private
    { Private declarations }
  public
    FileList:TStringList;
    DeleteAll:Boolean;
    SourceDir:String;
    ProgressTimer:TTimer;
    FMax,FPos,ListPos:LongWord;
    procedure DeleteFiles;
    procedure ShowProgress(Sender:TObject);
    { Public declarations }
  end;

var
  DlgDeleteFiles: TDlgDeleteFiles;
  Abort:Boolean;
implementation

{$R *.dfm}

procedure RemoveFileTree(const Path: string);
var Found: integer;
    SearchRec: TSearchRec;
    FileName: string;
begin
  Found:= FindFirst(Path + '\*.*', faAnyFile, SearchRec);
  while Found = 0 do
  begin
    if ((SearchRec.Attr and faDirectory) = faDirectory)
    then
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
        then RemoveFileTree(Path+'\'+SearchRec.Name)
      else
    else
    begin
      FileName:= Path+'\'+SearchRec.Name+#0;
      DeleteFile(PChar(FileName));
    end;
  Found:= FindNext(SearchRec);
  end;
  SysUtils.FindClose(SearchRec);
  RemoveDir(Path);
end;

Procedure TDlgDeleteFiles.DeleteFiles;
const EQUALS=0;
      LESS=1;
      BIG=2;
      LESS_EQUALS=3;
      BIG_EQUALS=4;
      NOT_EQUALS=5;
Var FileStream:TMemoryStream;
    FileName,Dir:String;
    Index:LongWord;
    Pos:LongWord;
    Data:Byte;
    EtalonSize:LongWord;
    sr:TSearchRec;
    {DirList:TStringList;}
Begin
  If FileList.Count<=0 Then Exit;
  If (cbSizeFilter.Enabled)And(cbSizeFilter.Checked) Then
    Case cbSizeDivide.ItemIndex Of
      0:EtalonSize:=StrToInt(edSize.Text);
      1:EtalonSize:=StrToInt(edSize.Text)*1024;
      2:EtalonSize:=StrToInt(edSize.Text)*1048576;
    End;
  ProgressTimer.Enabled:=True;
  {DirList:=TStringList.Create;}
  For Index:=0 To FileList.Count-1 Do
    Begin
      FileName:=FileList.Strings[Index];
      //ShowMessage(FileName);
      FindFirst(FileName, faAnyFile, SR);
      If cbUseExtFilter.Checked Then
      If Not(UpperCase(ExtractFileExt(FileName))='.'+cbExtFilter.Text) Then  Continue;
      If (cbSizeFilter.Enabled)And(cbSizeFilter.Checked) Then
      Case cbMode.ItemIndex Of
        EQUALS:If Not (EtalonSize=sr.Size)Then Continue;
        LESS:If Not (EtalonSize<sr.Size)Then Continue;
        BIG:If Not (EtalonSize>sr.Size)Then Continue;
        LESS_EQUALS:If Not (EtalonSize<=sr.Size)Then Continue;
        BIG_EQUALS:If Not (EtalonSize>=sr.Size)Then Continue;
        NOT_EQUALS:If Not (EtalonSize<>sr.Size)Then Continue;
      End;

      FileSetAttr(FileName,$0);
      ListPos:=Index;
      If cbFillZeroBytes.Checked Then
        Begin
          FileStream:=TMemoryStream.Create;
          FileStream.LoadFromFile(FileName);
          FMax:=FileStream.Size;
          //!Attention!!Correction!
          For Pos:=0 To FMax Do
            Begin
              FileStream.Position:=Pos;
              FPos:=Pos;
              Data:=$FF;
              FileStream.Write(Data,SizeOf(Byte));
              FileStream.Position:=Pos;
              Data:=$0;
              FileStream.Write(Data,SizeOf(Byte));
              Application.ProcessMessages;
              If Abort Then
                Begin
                  FileStream.Free;
                  ProgressTimer.Enabled:=False;
                  Exit;
                End;
            End;
          FileStream.SaveToFile(FileName);
          FileStream.Free;
        End;
      DeleteFile(FileName);
      {Dir:=ExtractFileDir(FileName);
      //Delete(Dir,Length(Dir),1);
      //showmessage(dir);
      If Not TextInList(Dir,DirList) Then
        DirList.Add(Dir);
      {ShowMessage(Dir);
      FileSetAttr(Dir,0);
      RemoveDir(Dir); }
      Application.ProcessMessages;
      If Abort Then Exit;
    End;
  ProgressTimer.Enabled:=False;
  {If Not DirList.Count<=0 Then
  For Index:=0 To DirList.Count-1 Do
    RemoveDir(DirList.Strings[Index]);
  DirList.Free; }
  If DeleteAll Then
    RemoveFileTree(SourceDir);
End;

Procedure TDlgDeleteFiles.ShowProgress(Sender:TObject);
Begin
  FillGauge.Position:=GetPercentDone(0,FPos,FMax);
  GeneralGauge.Position:=GetPercentDone(0,ListPos,FileList.Count-1);
End;

procedure TDlgDeleteFiles.FormCreate(Sender: TObject);
begin
  FileList:=TStringList.Create;
  Abort:=False;
  ProgressTimer:=TTimer.Create(Nil);
  ProgressTimer.Enabled:=False;
  ProgressTimer.Interval:=1;
  ProgressTimer.OnTimer:=ShowProgress;
end;

procedure TDlgDeleteFiles.FormDestroy(Sender: TObject);
begin
  FileList.Free;
  ProgressTimer.Free;
end;

procedure TDlgDeleteFiles.BtnStartClick(Sender: TObject);
begin
  BtnStart.Enabled:=False;
  gbDeleteOptions.Enabled:=False;
  gbFilter.Enabled:=False;
  DeleteFiles;
  Close;
end;

procedure TDlgDeleteFiles.BtnAbortClick(Sender: TObject);
begin
  Abort:=True;
  Close;
end;

procedure TDlgDeleteFiles.cbUseFilterClick(Sender: TObject);
begin
  cbUseExtFilter.Enabled:=cbUseFilter.Checked;
  cbSizeFilter.Enabled:=cbUseFilter.Checked;
  cbExtFilter.Enabled:=cbUseExtFilter.Checked;
  edSize.Enabled:=cbSizeFilter.Checked;
  cbSizeDivide.Enabled:=cbSizeFilter.Checked;
  cbMode.Enabled:=cbSizeFilter.Checked;
  If Not cbUseFilter.Checked Then
    Begin
      cbExtFilter.Enabled:=False;
      edSize.Enabled:=False;
      cbSizeDivide.Enabled:=False;
      cbMode.Enabled:=False;
    End;

end;

procedure TDlgDeleteFiles.cbFillZeroBytesClick(Sender: TObject);
begin
  FillGauge.Visible:=cbFillZeroBytes.Checked;
end;

procedure TDlgDeleteFiles.cbUseExtFilterClick(Sender: TObject);
begin
  cbExtFilter.Enabled:=cbUseExtFilter.Checked;
end;

procedure TDlgDeleteFiles.edSizeKeyPress(Sender: TObject; var Key: Char);
begin
  If Not (Key in ['0'..'9']) Then
    Key:=#0;
end;

procedure TDlgDeleteFiles.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Abort:=True;
end;

procedure TDlgDeleteFiles.cbSizeFilterClick(Sender: TObject);
begin
  edSize.Enabled:=cbSizeFilter.Checked;
  cbSizeDivide.Enabled:=cbSizeFilter.Checked;
  cbMode.Enabled:=cbSizeFilter.Checked;
end;

end.
