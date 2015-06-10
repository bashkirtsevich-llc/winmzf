unit TestArchive_u;

interface                                                      

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ztvBase, ztvGbls, ztvZipCheck, Err_msgs, ztvRegister,
  SignatureDetect_u, Advanced, LZMA_u, ArchView_u, StdCtrls, Buttons,
  ComCtrls, Viewer_u;

type
  TdlgTestArchive = class(TForm)
    btnTest: TBitBtn;
    btnCancel: TBitBtn;
    lvResult: TListView;
    pbArchive: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    procedure UniCheckerOnProgress(Sender:TObject;ProgressByFile,ProgressByArchive:Byte);
    procedure UniCheckerOnGetPass(Sender: TObject; FN: String;
                                  Var Password: String; Var TryAgain: BOOLEAN);
    procedure UniCheckOnStatus(Sender: TObject; FN: String; PassFail: BOOLEAN);
    procedure UniCheckOnError(Sender: TObject; FN, MsgEx,
                              VolumeID: String; ECode: Integer);
    procedure AddItem(FileName,State:String);
    procedure MZFStatus(Sender:TObject;FileName:String; Failed:Boolean);
    procedure MZFOnProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
    procedure MZFGetPass(Sender:TObject;Var Pass:ShortString);
    { Private declarations }
  public
    ArchiveFile:String;
    ArcType:TFileType;
    FileSpec:TStringList;
    Abort:Boolean;
    procedure CheckArchive;
    { Public declarations }
  end;

var
  dlgTestArchive: TdlgTestArchive;

implementation

{$R *.dfm}

procedure TdlgTestArchive.btnCancelClick(Sender: TObject);
begin
  Abort:=True;
end;

procedure TdlgTestArchive.btnTestClick(Sender: TObject);
begin
  btnTest.Enabled:=False;
  pbArchive.Position:=0;
  CheckArchive;
  btnTest.Enabled:=True;
end;

procedure TdlgTestArchive.FormCreate(Sender: TObject);
begin
  FileSpec:=TStringList.Create;
  Abort:=False;
  //
  Caption:=ReadFromLanguage('Windows','wndCheckArchive',Caption);
  btnTest.Caption:=ReadFromLanguage('Buttons','btnTest',btnTest.Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  lvResult.Columns[0].Caption:=ReadFromLanguage('ListItems','liFile',lvResult.Columns[0].Caption);
  lvResult.Columns[1].Caption:=ReadFromLanguage('ListItems','liType',lvResult.Columns[1].Caption);
  lvResult.Columns[2].Caption:=ReadFromLanguage('ListItems','liStatus',lvResult.Columns[2].Caption);
end;

procedure TdlgTestArchive.AddItem(FileName: string; State: string);
var Item:TListItem;
begin
  Item:=lvResult.Items.Add;
  Item.Caption:=FileName;
  Item.SubItems.Add(ExtractFileExt(FileName));
  Item.SubItems.Add(State);
end;

procedure TdlgTestArchive.UniCheckerOnProgress(Sender:TObject;ProgressByFile,ProgressByArchive:Byte);
begin
  pbArchive.Position:=ProgressByArchive;
end;

procedure TdlgTestArchive.UniCheckerOnGetPass(Sender: TObject; FN: String;
   Var Password: String; Var TryAgain: BOOLEAN);
var Temp:String;
begin
  if InputQuery('WinMZF',ReadFromLanguage('Messages','Password','Enter password'),Temp) then
    Password:=Temp
  Else
    TryAgain:=False;
end;

procedure TDlgTestArchive.UniCheckOnStatus(Sender: TObject; FN: string; PassFail: Boolean);
begin
  if Not PassFail then
    AddItem((FN),ReadFromLanguage('Status','Failed','Failed!'))
  Else
    AddItem((FN),ReadFromLanguage('Status','Normal','Ok!'));
end;

procedure TDlgTestArchive.MZFStatus(Sender:TObject;FileName:String; Failed:Boolean);
begin
  if Failed then
    AddItem((FileName),ReadFromLanguage('Status','Failed','Failed!'))
  Else
    AddItem((FileName),ReadFromLanguage('Status','Normal','Ok!'));  
end;

procedure TDlgTestArchive.UniCheckOnError(Sender: TObject; FN: string; MsgEx: string; VolumeID: string; ECode: Integer);
begin
  AddItem((FN),Format('Error-%s error code %d',[MsgEx,ECode]));
end;

procedure TDlgTestArchive.MZFOnProgress(Sender: TObject; FProgress: Byte; AProgress: Byte; var Abort: Boolean);
begin
  pbArchive.Position:=AProgress;
end;

procedure TDlgTestArchive.MZFGetPass(Sender: TObject; var Pass: OpenString);
var Temp:String;
begin
  if InputQuery('WinMZF',ReadFromLanguage('Messages','Password','Enter password'),Temp) then
    Pass:=Temp;  
end;

procedure TDlgTestArchive.CheckArchive;
var MZFViewer:TMZFViewer;
    UniversalCheck:TZipCheck;
    Index:LongWord;
begin
  case Self.ArcType of
    ftMZF:
      Begin
        MZFViewer:=TMZFViewer.Create;
        MZFViewer.ArchiveName:=Self.ArchiveFile;
        MZFViewer.OnCheck:=MZFStatus;
        MZFViewer.OnProgress:=MZFOnProgress;
        MZFViewer.OnPassQuest:=MZFGetPass;
        MZFViewer.CheckArchive;
        MZFViewer.Free;
      End;
    ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,ftArj,ftPKG5,ftACE2:
      Begin
        UniversalCheck:=TZipCheck.Create(Nil);
        UniversalCheck.ArchiveFile:=Self.ArchiveFile;
        for Index := 0 to FileSpec.Count - 1 do
          UniversalCheck.FileSpec.Add(FileSpec[Index]);
        //Apply properies
        UniversalCheck.OnProgress:=UniCheckerOnProgress;
        UniversalCheck.OnGetPassword:=UniCheckerOnGetPass;
        UniversalCheck.OnStatus:=UniCheckOnStatus;
        UniversalCheck.OnError:=UniCheckOnError;
        //
        UniversalCheck.Activate;
        UniversalCheck.Free;
      End;
    ftRar: ;
    ft7z: ;
  end;
end;

end.
