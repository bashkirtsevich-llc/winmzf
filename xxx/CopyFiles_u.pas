unit CopyFiles_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Gauges, Advanced, ExtCtrls, FileCtrl,
  ComCtrls;

type TCopyMode=(cmCopy,cmReplace);

type
  TDlgCopyMoveFiles = class(TForm)
    BtnCopyOrMove: TBitBtn;
    BtnCancel: TBitBtn;
    gbOptions: TGroupBox;
    Label1: TLabel;
    edDestDir: TEdit;
    btnBrows: TBitBtn;
    cbUpdateModifyTime: TCheckBox;
    cbUpdateAccessTime: TCheckBox;
    CopyGauge: TProgressBar;
    GeneralGauge: TProgressBar;
    Procedure OnTimer(Sender:TObject);
    Procedure CopyFiles(DestDir:String;List:TStringList;
                        UpdateModifyTime,UpdateAccessTime:Boolean;CopyMode:TCopyMode);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnCopyOrMoveClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure btnBrowsClick(Sender: TObject);

  private
    FilePos,FileIndex:LongWord;
    Size,Count:LongWord;
    Timer:TTimer;
    Abort:Boolean;
    { Private declarations }
  public
    FilesList:TStringList;
    Mode:TCopyMode;
    { Public declarations }
  end;

var
  DlgCopyMoveFiles: TDlgCopyMoveFiles;

implementation

{$R *.dfm}

Procedure TDlgCopyMoveFiles.OnTimer(Sender:TObject);
Begin
  CopyGauge.Position:=GetPercentDone(0,FilePos,Size);
  GeneralGauge.Position:=GetPercentDone(0,FileIndex,Count);
End;

Procedure TDlgCopyMoveFiles.CopyFiles(DestDir:String;List:TStringList;
                                      UpdateModifyTime,UpdateAccessTime:Boolean;
                                      CopyMode:TCopyMode);
Var InStream,OutStream:TMemoryStream;
    Index:LongWord;
    Attr:Word;
    Data:Byte;
    FName:String;
    CreationTime,ModifyTime,AccessTime:TDateTime;
    DominateDir:String;
    DirList:TStringList;
Begin
  FilePos:=$0;
  FileIndex:=$0;
  Count:=List.Count-1;
  DirList:=TStringList.Create;
  For Index:=0 To List.Count-1 Do
    If Not TextInList(ExtractFileDir(List.Strings[Index]),DirList) Then
      DirList.Add(ExtractFileDir(List.Strings[Index]));
  DominateDir:=GetDominateDir(DirList);
  Timer.Enabled:=True;
  For Index:=0 To List.Count-1 Do
    Begin
      FilePos:=$0;
      FileIndex:=Index;
      FName:=List.Strings[Index];
      Attr:=FileGetAttr(FName);
      CreationTime:=FileDateToDateTime(FileCreationTime(FName));
      ModifyTime:=FileDateToDateTime(FileModifyTime(FName));
      AccessTime:=FileDateToDateTime(FileAccessTime(FName));
      If CopyMode=cmReplace Then
        FileSetAttr(FName,0);
      InStream:=TMemoryStream.Create;
      OutStream:=TMemoryStream.Create;
      InStream.LoadFromFile(FName);
      InStream.Position:=$0;
      Size:=InStream.Size;
      If Abort Then
        Begin
          InStream.Free;
          OutStream.Free;
          Exit;
        End;
      While FilePos<>InStream.Size Do
        Begin
          InStream.Read(Data,1);
          OutStream.Write(Data,1);
          Inc(FilePos);
          If Abort Then
            Begin
              InStream.Free;
              OutStream.Free;
              Exit;
            End;
          Application.ProcessMessages;
        End;
      InStream.Free;
      If Not DirectoryExists(DestDir+'\'+SubStractString(DominateDir,ExtractFileDir(FName))) Then
        MakeDir(DestDir+'\'+SubStractString(DominateDir,ExtractFileDir(FName)));
      OutStream.SaveToFile(DestDir+'\'+SubStractString(DominateDir,ExtractFileDir(FName))+'\'+ExtractFileName(FName));
      OutStream.Free;
      FileSetAttr(DestDir+ExtractFileName(FName),Attr);
      If CopyMode=cmReplace Then
        DeleteFile(FName);
      SetFileDateTime(DestDir+'\'+SubStractString(DominateDir,ExtractFileDir(FName))+'\'+ExtractFileName(FName),
                      CreationTime,ModifyTime,AccessTime,
                      UpdateModifyTime,UpdateAccessTime);
    End;
  Timer.Enabled:=False;
End;

procedure TDlgCopyMoveFiles.FormCreate(Sender: TObject);
begin
  FilesList:=TStringList.Create;
  Abort:=False;
  Timer:=TTimer.Create(Self);
  Timer.Interval:=1;
  Timer.Enabled:=False;
  Timer.OnTimer:=OnTimer;
end;

procedure TDlgCopyMoveFiles.FormDestroy(Sender: TObject);
begin
  Timer.Free;
end;

procedure TDlgCopyMoveFiles.BtnCopyOrMoveClick(Sender: TObject);
begin
  If edDestDir.Text='' Then
    Begin
      MessageBox(Handle,'Please select destination dir','Attention!',MB_OK+MB_ICONEXCLAMATION);
      Exit;
    End;
  BtnCopyOrMove.Enabled:=False;
  edDestDir.Enabled:=False;
  btnBrows.Enabled:=False;
  cbUpdateModifyTime.Enabled:=False;          
  cbUpdateAccessTime.Enabled:=False;
  CopyFiles(edDestDir.Text,FilesList,
            cbUpdateModifyTime.Checked,
            cbUpdateAccessTime.Checked,
            Mode);
  Close;
end;

procedure TDlgCopyMoveFiles.BtnCancelClick(Sender: TObject);
begin
  Abort:=True;
  Close;
end;

procedure TDlgCopyMoveFiles.btnBrowsClick(Sender: TObject);
var Dir:String;
begin
  SelectDirectory('Select destination directory', '', Dir);
  edDestDir.Text:=Dir;
end;

end.
