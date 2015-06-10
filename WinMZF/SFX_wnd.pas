unit SFX_wnd;
                      
interface                                    

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, Decompress_u, Advanced, 
  UniversalUnpacker_u, SignatureDetect_u, RarUnit, OverWrite_u, XPMan;

//type TArchType=(atNone,atMZF,atZipTV);
function ChangeVolProc(ArcName:PChar; Mode:integer):integer; cdecl;
                                          
type
  TSFXhead=packed record
    Signature:Array[0..2] Of Char;
    Vendor:Array[0..5] Of Char;
    ArchType:TFileType;
    WndCaption:ShortString;
    ExtractDir:ShortString;
  end;

type
  TdlgSFXExtract = class(TForm)
    btnExtract: TBitBtn;
    btnCancel: TBitBtn;
    memComment: TMemo;
    pbProgress: TProgressBar;
    imgLogo: TImage;
    lineBottom: TBevel;
    lbCopyRight: TLabel;
    cbDestDir: TComboBox;
    lbProgress: TLabel;
    lbDestDir: TLabel;
    btnBrows: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
    procedure btnBrowsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
    procedure GetPass(Sender:TObject;Var Pass:ShortString);
    procedure OnError(Sender:TObject;ErrorMsg:ShortString);
    procedure RewritePromt(Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode);
    procedure ExtractArchive(ArchiveFIle:TStream;DestDir:String;
      RewriteMode:TRewriteMode;ExtractAll,_Delete,UpdateModTime,UpdateAccessTime:Boolean);
    procedure btnExtractClick(Sender: TObject);
    procedure OnBegin(Sender:TObject);
    procedure OnEnd(Sender:TObject);
  private
    procedure OnExtract(Sender:TObject;_FileName:ShortString);
    { Private declarations }
  public
    _Abort:Boolean;
    ArcType:TFileType;
    { Public declarations }
  end;

const
  Sign='MSi';
  VID='MADMAN';

var
  dlgSFXExtract: TdlgSFXExtract;
  MZFArchive:TMemoryStream;
  
implementation

{$R *.dfm}

function ChangeVolProc(ArcName:PChar; Mode:integer):integer;
var s:string;
    p:pchar;
begin
  ChangeVolProc:=1;
  p:=@s[1];
  if (Mode=RAR_VOL_ASK) then
  begin
    s:='Insert next volume';
    if Application.messagebox(p,'Attention',mb_okcancel)=idcancel
       then ChangeVolProc:=0
  end;
end;

procedure RarExtract(ArcName:pchar;DestDir:String);
var
  hArcData:THandle;
  RHCode,PFCode:integer;
  CmtBuf:array[1..16384] of char;
  HeaderData:RARHeaderData;
  OpenArchiveData:RAROpenArchiveData;
  {GlobalUnPackSize,Maximum,}FileCount,Current:LongWord;
begin

  OpenArchiveData.ArcName:=ArcName;
  OpenArchiveData.CmtBuf:=@CmtBuf[1];
  OpenArchiveData.CmtBufSize:=sizeof(CmtBuf);
  OpenArchiveData.OpenMode:=RAR_OM_EXTRACT;
  hArcData:=RAROpenArchive(OpenArchiveData);
  if (OpenArchiveData.OpenResult<>0) then
     begin
      MessageBox(dlgSFXExtract.Handle,'Archive is corrupt!','Error',MB_OK+MB_ICONEXCLAMATION);
      RARCloseArchive(hArcData);
      exit;
     end;
  RARSetChangeVolProc(hArcData,ChangeVolProc);
  HeaderData.CmtBuf:=NIL;
  {Maximum:=0;
  GlobalUnPackSize:=0; }
  FileCount:=0;
  Current:=0;
  While RARReadHeader(hArcData, HeaderData) <> ERAR_END_ARCHIVE do
    Begin
      RARProcessFile(hArcData,RAR_OM_LIST,Nil,Nil);
      Inc(FileCount);
    End;
  hArcData:=RAROpenArchive(OpenArchiveData);
  RARSetChangeVolProc(hArcData,ChangeVolProc);
  HeaderData.CmtBuf:=NIL;
  RHCode:=RARReadHeader(hArcData,HeaderData);
  {Inc(GlobalUnPackSize, HeaderData.UnpSize);}
  Inc(Current);
  while RHCode<>ERAR_END_ARCHIVE do
  begin
    If dlgSFXExtract._Abort Then
      Begin
        //dlgProgress.Close;
        //dlgProgress.Free;
        //Self.Enabled:=True;
        RARCloseArchive(hArcData);
        Exit;
      End;
    {If (Not GetBoolOptions('Config','EnableSecurity',False))Then
        If (Not NameInMask(GetStringOptions('Config','ExcludeExt','*.exe;*.com;*.bat'),HeaderData.FileName)) Then}
          dlgSFXExtract.OnExtract(dlgSFXExtract,ShortString(HeaderData.FileName));
          PFCode:=RARProcessFile(hArcData,RAR_EXTRACT,PChar(DestDir),Nil);
    {Inc(GlobalUnPackSize, HeaderData.PackSize);}
    Inc(Current);
    if PFCode<>0 then
       begin
        MessageBox(dlgSFXExtract.Handle,'Coud not open file','Error',MB_OK+MB_ICONSTOP);
        RARCloseArchive(hArcData);
        exit;
       end;
    RHCode:=RARReadHeader(hArcData,HeaderData);
    {dlgProgress.pbFile.Position:=GetPercentDone(0,GlobalUnPackSize,Maximum);}
    {dlgProgress.pbArchive.Position:=GetPercentDone(0,Current,FileCount);
    dlgProgress.lbFileIndex.Caption:=Format('File index %d',[Current]);
    dlgProgress.lbCurrent.Caption:='';
    dlgProgress.lbTotal.Caption:=Format('Total progress %d',[dlgProgress.pbArchive.Position]);
    dlgProgress.Update;}
    dlgSFXExtract.pbProgress.Position:=GetPercentDone(0,Current,FileCount);
    Application.ProcessMessages;
  end;
  if (RHCode=ERAR_BAD_DATA) then
    MessageBox(dlgSFXExtract.Handle,'Archive is corrupt','Attention',MB_OK+MB_ICONEXCLAMATION);
  RARCloseArchive(hArcData);
  {dlgProgress.Close;
  dlgProgress.Free;
  Self.Enabled:=True;}
End;

procedure TdlgSFXExtract.OnBegin(Sender:TObject);
begin
  btnExtract.Enabled:=False;
end;

procedure TdlgSFXExtract.OnEnd(Sender:TObject);
begin
  btnExtract.Enabled:=True;
end;

procedure TdlgSFXExtract.ExtractArchive(ArchiveFile:TStream;DestDir:String;
  RewriteMode:TRewriteMode;ExtractAll,_Delete,UpdateModTime,UpdateAccessTime:Boolean);
var MZFDecompressor:TMZFDecompressor;
    UnPacker:TUnPacker;
    //SevenZipDecompressor:TSevenZipViewer;
    Index:Byte;
    Temp:String;               
begin
  Case ArcType Of
    ftMZF:Begin
            MZFDecompressor                     :=TMZFDecompressor.Create;
            //MZFDecompressor.ArchiveFile         :=ArchiveFile;
            MZFDecompressor.DestinationDir      :=DestDir;
            MZFDecompressor.ExtractAll          :=ExtractAll;
            MZFDecompressor.OnBegin             :=OnBegin;
            MZFDecompressor.OnEnd               :=OnEnd;
            MZFDecompressor.OnProgress          :=OnProgress;
            MZFDecompressor.OnGetPass           :=GetPass;
            MZFDecompressor.OnError             :=OnError;
            MZFDecompressor.UpdateModifyTime    :=UpdateModTime;
            MZFDecompressor.UpdateLastAccesstime:=UpdateAccessTime;
            MZFDecompressor.FileOverwriteMode   :=RewriteMode;
            MZFDecompressor.OnOverwritePrompt   :=RewritePromt;
            MZFDecompressor.DeleteAfterExtract  :=_Delete;
            MZFDecompressor.OnExtract           :=OnExtract;
            MZFDecompressor.ExtractFromStream(ArchiveFile);
            MZFDecompressor.Free;
          End;
    {ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,
    ftTar,ftACE2,ftJar,ftArc,ftArj,
    ftPKG5:Begin
          UnPacker:=TUnPacker.Create;
          UnPacker.ArchiveFile:=ArchiveFIle;
          UnPacker.ArchiveType:=GetArchiveTypeBySignature(ArchiveFile);
          UnPacker.DestinationDir:=DestDir;
          If ExtractAll Then
            UnPacker.FileSpec.Add('*.*')
          Else
            For Index:=0 To Files.Count-1 Do
              Begin
                Temp:=Files.Strings[Index];
                //Delete(Temp,1,1);
                UnPacker.FileSpec.Add(Temp);
              End;
          UnPacker.OnProgress:=ArchiverProgress;
          UnPacker.OnGetPassword:=GetPass;
          UnPacker.OnFileExists:=RewritePromt;
          UnPacker.OnBegin:=ArchiverOnBegin;
          UnPacker.OnEnd:=ArchiverOnEnd;
          UnPacker.DeleteAfterExtract:=_Delete;
          UnPacker.OverWriteMode:=RewriteMode;
          UnPacker.Extract;
          UnPacker.Free;
        End;
      ft7z:Begin
          SevenZipDecompressor:=TSevenZipViewer.Create;
          SevenZipDecompressor.FileName:=ArchiveFile;
          SevenZipDecompressor.DestDir:=DestDir;
          If ExtractAll Then
            SevenZipDecompressor.FileSpec.Clear;
          SevenZipDecompressor.OnBegin:=ArchiverOnBegin;
          SevenZipDecompressor.OnEnd:=ArchiverOnEnd;
          SevenZipDecompressor.OnProgress:=ArchiverProgress;
          SevenZipDecompressor.OnOverWrite:=RewritePromt;
          SevenZipDecompressor.OverWriteMode:=RewriteMode;
          SevenZipDecompressor.Extract;
          SevenZipDecompressor.Free;
        End;
      ftRar:
        Begin
          LoadDll(hInstance);
          RarExtract(PChar(WorkingArchive.Archive),PChar(DestDir));
          FreeDll
        End;}
  End;
end;

procedure TdlgSFXExtract.btnCancelClick(Sender: TObject);
begin
  _Abort:=True;
  Close;
end;

procedure TdlgSFXExtract.btnExtractClick(Sender: TObject);
begin
  memComment.Clear;
  ExtractArchive(MZFArchive,cbDestDir.Text,omRewrite,True,False,False,False);
  Application.Terminate;
end;

procedure TdlgSFXExtract.OnProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
begin
  pbProgress.Position:=AProgress;
  Abort:=_Abort;
end;

procedure TdlgSFXExtract.OnExtract(Sender:TObject;_FileName:ShortString);
begin
  memComment.Lines.Add(_FileName);
end;

procedure TdlgSFXExtract.GetPass(Sender:TObject;Var Pass:ShortString);
Var Temp:String;
Begin
  Temp:='';
  If InputQuery('Password','Enter password',Temp) Then
    Pass:=Temp
End;

procedure TdlgSFXExtract.OnError(Sender:TObject;ErrorMsg:ShortString);
Var S:string;
Begin
  S:=ErrorMsg;
  MessageBox(Handle,PChar(S),PChar('WinMZF'),MB_OK+MB_ICONEXCLAMATION);
End;

procedure TdlgSFXExtract.RewritePromt(Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode);
Begin
  Application.CreateForm(TDlgOverWrite,DlgOverWrite);
  DlgOverWrite.Label2.Caption:=Format('File name: %s',[ExtractFileName(AFile.LongFileName)]);
  DlgOverWrite.Label3.Caption:=Format('File size: %d',[AFile.Size]);
  DlgOverWrite.Label4.Caption:=Format('File attr: %s',[GetAttributesAsString(AFile.Attr)]);
  DlgOverWrite.Label5.Caption:=Format('File creation date: %s',[FormatDateTime(TimeFormat,AFile.FileCreateDate)]);
  DlgOverWrite.Label6.Caption:=Format('File modify date: %s',[FormatDateTime(TimeFormat,AFile.FileModifyDate)]);
  DlgOverWrite.Label7.Caption:=Format('File access date: %s',[FormatDateTime(TimeFormat,AFile.FileOpenDate)]);

  DlgOverWrite.ShowModal;
  If DlgOverWrite.OverWrite Then
    Begin
      If DlgOverWrite.All Then
        Mode:=omOverwriteAll Else
        Mode:=omRewrite;
    End Else
      Begin
        If DlgOverWrite.All Then
          Mode:=omSkipAll Else
          Mode:=omSkip;
      End;
  DlgOverWrite.Free;
End;

procedure TdlgSFXExtract.btnBrowsClick(Sender: TObject);
var Temp:String;
begin
  Temp:=GetSelectedDir;
  If Temp<>'' Then
    cbDestDir.Text:=Temp;
end;

procedure TdlgSFXExtract.FormCreate(Sender: TObject);
var _Self:TMemoryStream;
    SFXHead:TSFXhead;
    _Pos:LongWord;
    Temp:TStream;
    _S:TArcHead;
    z:array[0..2]of char;
begin
  _Abort:=False;
  SFXHead.ArchType:=ftAnyFile;
  Caption:='WinMZF Self extraction archive';
  Application.Title:=Caption;
  cbDestDir.Text:=ExtractFileDir(Application.ExeName);
  _Self:=TMemoryStream.Create;
  _Self.LoadFromFile(Application.ExeName);
  _Self.Position:=0;
  For _Pos:=0 To _Self.Size Do
    Begin
      _Self.Read(SFXHead,SizeOf(TSFXhead));
      If (SFXHead.Signature=Sign)And(SFXHead.Vendor=VID) Then
        Break;
      _Self.Position:=_Pos;
    End;
  If (SFXHead.Signature<>Sign)Or(SFXHead.Vendor<>VID) Then
    Halt;
  Caption:=SFXHead.WndCaption;
  Application.Title:=Caption;
  If SFXHead.ExtractDir<>'' Then
    cbDestDir.Text:=SFXHead.ExtractDir;
  ArcType:=SFXHead.ArchType;
  Case SFXHead.ArchType Of
    ftMZF:
      Begin
        MZFArchive:=TMemoryStream.Create;
        MZFArchive.CopyFrom(_Self,_Self.Size-(_Pos+SizeOf(TSFXhead)-1));
        MZFArchive.Position:=0;
        //MZFArchive.SaveToFile('d:\123.mzf');
      End;
    {ftAce2,ftArc,ftArj,ftBh,ftCab,ftGZip,ftJar,ftLha,ftTar,ftZip,ftZoo:;
    ftRar:;}
    ftAnyFile:Halt;
  End;
end;

end.
