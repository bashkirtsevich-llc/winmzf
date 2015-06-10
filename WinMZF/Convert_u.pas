unit Convert_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Add_u, Compress_u, SevenZip_Compressor_u,
  UniversalUnpacker_u, Decompress_u, UniversalCompressor_u, LZMA_u, Advanced,
  SignatureDetect_u, RarUnit, Report_u;

function ChangeVolProc(ArcName:PChar; Mode:integer):integer; cdecl;

type
  TdlgConvert = class(TForm)
    gbConverterOptions: TGroupBox;
    cbConvertFrom: TComboBox;
    cbConvertTo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    btnConvert: TBitBtn;
    btnCancel: TBitBtn;
    btnCompressOptions: TBitBtn;
    cbDeleteSource: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    edSource: TEdit;
    edTarget: TEdit;
    btnBrows0: TBitBtn;
    btnBrows1: TBitBtn;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure btnBrows0Click(Sender: TObject);
    procedure btnCompressOptionsClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbConvertToChange(Sender: TObject);
  private
    MZFLevel:TMZFCompressionLevel;
    Level:Byte;
    _Pass:String;
    _encFile,_encHead:Boolean;
    _r,_h,_a:Boolean;
    _comment:String;
    procedure ConverterOnBegin(Sender:TObject);
    procedure ConverterOnEnd(Sender:TObject);
    procedure ConverterOnGetPass(Sender:TObject;Var Pass:ShortString);
    procedure ConverterOnProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
    procedure OnError(Sender:TObject;ErrorMsg:ShortString);
    procedure RarExtract(ArcName:pchar;DestDir:String);
    { Private declarations }
  public
    procedure ConvertArchive(Source,Target:String;TargetType:TFileType;Password:String;_MZFLevel:TMZFCompressionLevel;_OtherLevel:Byte;
                EncodeFile,EncodeHead,R,H,A:Boolean;Comment:String);
    { Public declarations }
  end;

var
  dlgConvert: TdlgConvert;
  dlgCaption,ResName:String;
implementation

uses Progress_u;

{$R *.dfm}

function ChangeVolProc(ArcName:pchar; Mode:integer):integer;
var s:string;
    p:pchar;
begin
  ChangeVolProc:=1;
  s:=ReadFromLanguage('Messages','NextVolume','Insert next volume');
  p:=@s[1];
  if (Mode=RAR_VOL_ASK) then
  begin
    if Application.messagebox(p,'WinMZF',mb_okcancel)=idcancel
       then ChangeVolProc:=0
  end;
end;

procedure TdlgConvert.RarExtract(ArcName:pchar;DestDir:String);
var
  hArcData:THandle;
  RHCode,PFCode:integer;
  CmtBuf:array[1..16384] of char;
  HeaderData:RARHeaderData;
  OpenArchiveData:RAROpenArchiveData;
  {GlobalUnPackSize,Maximum,}FileCount,Current:LongWord;
begin
  Self.Enabled:=False;
  Application.CreateForm(TdlgProgress,dlgProgress);
  //dlgProgress.pbArchive.Hide;
  dlgProgress.pbFile.Hide;
  //dlgProgress.Anime.ResName:='DECOMPRESSING';
  //dlgProgress.Anime.Active:=True;
  dlgProgress.Show;
  dlgProgress.Update;
  OpenArchiveData.ArcName:=ArcName;
  OpenArchiveData.CmtBuf:=@CmtBuf[1];
  OpenArchiveData.CmtBufSize:=sizeof(CmtBuf);
  OpenArchiveData.OpenMode:=RAR_OM_EXTRACT;
  hArcData:=RAROpenArchive(OpenArchiveData);
  if (OpenArchiveData.OpenResult<>0) then
     begin
      //MessageBox(Handle,'Archive is corrupt!','Error',MB_OK+MB_ICONEXCLAMATION);
      DisplayMessage(ReadFromLanguage('Messages','Corrupt','Archive is corrupt!'),IT_INFO);
      dlgProgress.Close;
      dlgProgress.Free;
      Self.Enabled:=True;
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
      //Inc(Maximum, HeaderData.PackSize);
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
    If dlgProgress.Abort Then     
      Begin
        dlgProgress.Close;
        dlgProgress.Free;
        Self.Enabled:=True;
        RARCloseArchive(hArcData);
        Exit;
      End;
    PFCode:=RARProcessFile(hArcData,RAR_EXTRACT,PChar(DestDir),Nil);
    {Inc(GlobalUnPackSize, HeaderData.PackSize);}
    Inc(Current);
    if PFCode<>0 then
       begin
        //MessageBox(Handle,'Coud not open file','Error',MB_OK+MB_ICONSTOP);
        DisplayMessage(ReadFromLanguage('Messages','CantOpen','Coud not open file'),IT_ERROR);
        dlgProgress.Close;
        dlgProgress.Free;
        Self.Enabled:=True;
        RARCloseArchive(hArcData);
        exit;
       end;
    RHCode:=RARReadHeader(hArcData,HeaderData);
    {dlgProgress.pbFile.Position:=GetPercentDone(0,GlobalUnPackSize,Maximum);}
    dlgProgress.pbArchive.Position:=GetPercentDone(0,Current,FileCount);
    dlgProgress.lbFileIndex.Caption:=Format(ReadFromLanguage('Status','FileIndex','File index %d'),[Current]);
    dlgProgress.lbCurrent.Caption:='';
    dlgProgress.lbTotal.Caption:=Format(ReadFromLanguage('Status','TotalProgress','Total progress %d'),[dlgProgress.pbArchive.Position]);
    dlgProgress.Update;
    Application.ProcessMessages;
  end;
  if (RHCode=ERAR_BAD_DATA) then
    //MessageBox(Handle,'Archive is corrupt','Attention',MB_OK+MB_ICONEXCLAMATION);
    DisplayMessage(ReadFromLanguage('Messages','Corrupt','Archive is corrupt!'),IT_INFO);
  RARCloseArchive(hArcData);
  dlgProgress.Close;
  dlgProgress.Free;
  Self.Enabled:=True;
End;

procedure TdlgConvert.ConverterOnBegin(Sender:TObject);
begin
  Application.CreateForm(TdlgProgress,dlgProgress);
  Self.Enabled:=False;
  dlgProgress.Show;
  dlgProgress.Caption:=dlgCaption;
  //dlgProgress.Anime.ResName:=ResName;
  //dlgProgress.Anime.Active:=True;
  dlgProgress.Update;
  dlgProgress.pbArchive.Position:=0;
  dlgProgress.pbFile.Position:=0;
end;

procedure TdlgConvert.ConverterOnEnd(Sender:TObject);
begin
  Self.Enabled:=True;
  dlgProgress.Hide;
  dlgProgress.Free;
end;

procedure TdlgConvert.OnError(Sender:TObject;ErrorMsg:ShortString);
Var S:string;
Begin
  S:=ErrorMsg;
  //MessageBox(Handle,PChar(S),PChar('WinMZF'),MB_OK+MB_ICONEXCLAMATION);
  DisplayMessage(S,IT_INFO);
End;

procedure TdlgConvert.ConverterOnGetPass(Sender:TObject;Var Pass:ShortString);
Var Temp:String;
Begin
  Temp:='';
  If InputQuery('WinMZF',ReadFromLanguage('Messages','Password','Enter password'),Temp) Then
    Pass:=Temp
end;

procedure TdlgConvert.ConverterOnProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
begin
  dlgProgress.pbArchive.Position:=AProgress;
  dlgProgress.pbFile.Position:=FProgress;
  dlgProgress.lbFileIndex.Caption:='';
  dlgProgress.lbCurrent.Caption:=Format(ReadFromLanguage('Status','CurrentProgress','Current progress %d'),[FProgress])+'%';
  dlgProgress.lbTotal.Caption:=Format(ReadFromLanguage('Status','TotalProgress','Total progress %d'),[AProgress])+'%';
  Abort:=dlgProgress.Abort;
  dlgProgress.Update;
end;  

procedure TdlgConvert.ConvertArchive(Source,Target:String;TargetType:TFileType;Password:String;_MZFLevel:TMZFCompressionLevel;_OtherLevel:Byte;
  EncodeFile,EncodeHead,R,H,A:Boolean;Comment:String);
var MZFCompressor:TMzfCompressor;
    UniversalCompressor:TUniversalCompressor;
    SevenZipCompressor:TSevenZipCompressor;
    //Decompressors;
    MZFDecompressor:TMZFDecompressor;
    UnPacker:TUnPacker;
    SevenZipDecompressor:TSevenZipViewer;
    _Dir:String;
begin
  _Dir:=GetTempDir;
  dlgCaption:=ReadFromLanguage('Status','Decompressing','Progress-Decompressing');
  ResName:='DECOMPRESSING';
  If Not FileExists(Source) Then Exit;
  If Target='' Then Exit;
  RemoveFileTree(_Dir);
  Case GetArchiveTypeBySignature(Source) Of
    ftMZF:
      Begin
        MZFDecompressor:=TMZFDecompressor.Create;
        MZFDecompressor.ArchiveFile:=Source;
        MZFDecompressor.OnBegin:=ConverterOnBegin;
        MZFDecompressor.OnEnd:=ConverterOnEnd;
        MZFDecompressor.OnProgress:=ConverterOnProgress;
        MZFDecompressor.OnGetPass:=ConverterOnGetPass;
        MZFDecompressor.OnError:=OnError;
        MZFDecompressor.DestinationDir:=_Dir;
        MZFDecompressor.ExtractAll:=True;
        MZFDecompressor.FileOverwriteMode:=omOverwriteAll;
        MZFDecompressor.Extract;
        MZFDecompressor.Free;
      End;
    ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,
    ftTar,ftACE2,ftJar,ftArc,ftArj,
    ftPKG5:
      Begin
        UnPacker:=TUnPacker.Create;
        UnPacker.ArchiveFile:=Source;
        UnPacker.ArchiveType:=GetArchiveTypeBySignature(Source);
        UnPacker.DestinationDir:=_Dir;
        UnPacker.FileSpec.Add('*.*');
        UnPacker.OnProgress:=ConverterOnProgress;
        UnPacker.OnGetPassword:=ConverterOnGetPass;
        UnPacker.OnBegin:=ConverterOnBegin;
        UnPacker.OnEnd:=ConverterOnEnd;
        UnPacker.OverWriteMode:=omOverwriteAll;
        UnPacker.Extract;
        UnPacker.Free;
      End;
    ft7Z:
      Begin
        SevenZipDecompressor:=TSevenZipViewer.Create;
        SevenZipDecompressor.FileName:=Source;
        SevenZipDecompressor.DestDir:=_Dir;
        SevenZipDecompressor.FileSpec.Clear;
        SevenZipDecompressor.OnBegin:=ConverterOnBegin;
        SevenZipDecompressor.OnEnd:=ConverterOnEnd;
        SevenZipDecompressor.OnProgress:=ConverterOnProgress;
        SevenZipDecompressor.OnGetPaasword:=ConverterOnGetPass;
        //SevenZipDecompressor.OnOverWrite:=RewritePromt;
        SevenZipDecompressor.OverWriteMode:=omOverwriteAll;
        SevenZipDecompressor.Extract;
        SevenZipDecompressor.Free;
      End;
    ftRar:
      Begin
        RarExtract(PChar(Source),_Dir);
      End;
  End;
  //Create archive so happy
  dlgCaption:=ReadFromLanguage('Status','Compressing','Progress-Compressing');
  ResName:='COMPRESSING';
  Case TargetType Of
    ftMZF:
      Begin
        MZFCompressor:=TMzfCompressor.Create;
        MZFCompressor.OnBegin:=ConverterOnBegin;
        MZFCompressor.OnEnd:=ConverterOnEnd;
        MZFCompressor.OnProgress:=ConverterOnProgress;
        MZFCompressor.OnGetPassword:=ConverterOnGetPass;
        MZFCompressor.ArchiveFileName:=Target;
        MZFCompressor.Password:=Password;
        MZFCompressor.Commentary:=Comment;
        MZFCompressor.EncodeHeadInfo:=EncodeHead;
        MZFCompressor.EncodeFileContain:=EncodeFile;
        MZFCompressor.ArchReadOnly:=R;
        MZFCompressor.ArcHidden:=H;
        MZFCompressor.ArcArchive:=A;
        MZFCompressor.Level:=_MZFLevel;
        MZFCompressor.FileList.Add(_Dir+'\*.*');
        MZFCompressor.CreateArchive;
        MZFCompressor.Free;
      End;
    ftBH,ftGZip,ftLha,ftJar,ftCab,ftTar,ftZip:
      Begin
        UniversalCompressor:=TUniversalCompressor.Create;
        UniversalCompressor.ArchiveFile:=Target;
        UniversalCompressor.FileType:=TargetType;
        UniversalCompressor.FileSpec.Add(_Dir+'\*.*');
        UniversalCompressor.Archived:=A;
        UniversalCompressor.ReadingOnly:=R;
        UniversalCompressor.Hidden:=H;
        UniversalCompressor.OnBegin:=ConverterOnBegin;
        UniversalCompressor.OnEnd:=ConverterOnEnd;
        UniversalCompressor.OnProgress:=ConverterOnProgress;
        UniversalCompressor.Password:=Password;
        UniversalCompressor.EncodeHeader:=EncodeHead;
        UniversalCompressor.Comment:=Comment;
        UniversalCompressor.CompressValue:=TCompressValue(_OtherLevel);
        UniversalCompressor.Compress;
        UniversalCompressor.Free;
      End;
    ft7z:
      Begin
        SevenZipCompressor:=TSevenZipCompressor.Create;
        SevenZipCompressor.FileName:=Target;
        SevenZipCompressor.RootDir:=_Dir;
        SevenZipCompressor.FileSpec.Add(_Dir+'\*.*');
        SevenZipCompressor.CompressType:=ctLZMA;
        SevenZipCompressor.CompressLevel:=TCompressLevel(_OtherLevel);
        SevenZipCompressor.OnBegin:=ConverterOnBegin;
        SevenZipCompressor.OnEnd:=ConverterOnEnd;
        SevenZipCompressor.OnProgress:=ConverterOnProgress;
        SevenZipCompressor.Password:=Password;
        SevenZipCompressor.ReadingOnly:=R;
        SevenZipCompressor.Archived:=A;
        SevenZipCompressor.Hidden:=H;
        SevenZipCompressor.Compress;
        SevenZipCompressor.Free;
      End;
  End;
  //Finalization
  If cbDeleteSource.Checked Then
    Begin
      FileSetAttr(Source,0);
      DeleteFile(Source);
    End;
  RemoveFileTree(_Dir);
end;

procedure TdlgConvert.btnBrows0Click(Sender: TObject);
begin
  case cbConvertFrom.ItemIndex of
    0:dlgOpen.Filter:='MZF archives (*.mzf)|*.MZF';
    1:dlgOpen.Filter:='RAR archives (*.rar)|*.RAR';
    2:dlgOpen.Filter:='7Z archives (*.7z)|*.7Z';
    3:dlgOpen.Filter:='ZIP archives (*.zip)|*.ZIP';
    4:dlgOpen.Filter:='BH archives (*.bh)|*.BH';
    5:dlgOpen.Filter:='ZOO archives (*.zoo)|*.ZOO';
    6:dlgOpen.Filter:='GZ archives (*.gz)|*.GZ';
    7:dlgOpen.Filter:='LHA archives (*.lha)|*.LHA';
    8:dlgOpen.Filter:='CAB archives (*.cab)|*.CAB';
    9:dlgOpen.Filter:='ARJ archives (*.arj)|*.ARJ';
    10:dlgOpen.Filter:='PKG5 archives (*.pkg5)|*.PKG5';
    11:dlgOpen.Filter:='ACE archives (*.ace)|*.ACE';
  end;
  case cbConvertTo.ItemIndex of
    0:dlgSave.Filter:='MZF archives (*.mzf)|*.MZF';
    1:dlgSave.Filter:='7Z archives (*.7z)|*.7Z';
    2:dlgSave.Filter:='ZIP archives (*.zip)|*.ZIP';
    3:dlgSave.Filter:='BH archives (*.bh)|*.BH';
    4:dlgSave.Filter:='LHA archives (*.lha)|*.LHA';
    5:dlgSave.Filter:='JAR archives (*.jar)|*.JAR';
    6:dlgSave.Filter:='CAB archives (*.cab)|*.CAB';
    7:dlgSave.Filter:='TAR archives (*.tar)|*.TAR';
  end;
  case (Sender as TBitBtn).Tag of
    0:If dlgOpen.Execute{(Handle)} Then edSource.Text:=dlgOpen.FileName;
    1:If dlgSave.Execute{(Handle)} Then edTarget.Text:=dlgSave.FileName;
  end;
end;

procedure TdlgConvert.btnCompressOptionsClick(Sender: TObject);
begin
  Application.CreateForm(TdlgAddFiles,dlgAddFiles);
  dlgAddFiles.tabFiles.Free;
  dlgAddFiles.tab.ActivePage:=dlgAddFiles.tabGeneral;
  dlgAddFiles.cbCompressMethod.ItemIndex:=cbConvertTo.ItemIndex;
  dlgAddFiles.cbCompressMethodChange(dlgAddFiles.cbCompressMethod);
  dlgAddFiles.tbLevelChange(dlgAddFiles.tbLevel);
  dlgAddFiles.cbFileName.Text:=edTarget.Text;
  if dlgAddFiles.ShowModal=mrOk then
    Begin
      MZFLevel:=dlgAddFiles.InSideLevel;
      Level:=dlgAddFiles.tbLevel.Position;
      edTarget.Text:=dlgAddFiles.cbFileName.Text;
      cbConvertTo.ItemIndex:=dlgAddFiles.cbCompressMethod.ItemIndex;
      cbConvertToChange(cbConvertTo);
    End;
  dlgAddFiles.Free;
end;

procedure TdlgConvert.btnConvertClick(Sender: TObject);
var DestType:TFileType;
begin
  Case cbConvertTo.ItemIndex Of
    0:DestType:=ftMZF;
    1:DestType:=ft7z;
    2:DestType:=ftZip;
    3:DestType:=ftBh;
    4:DestType:=ftLha;
    5:DestType:=ftJar;
    6:DestType:=ftCab;
    7:DestType:=ftTar;
  End;
  ConvertArchive(edSource.Text,edTarget.Text,DestType,_Pass,MZFLevel,         
                 Level,_encFile,_EncHead,_r,_h,_a,_comment);
  Close;                 
end;

procedure TdlgConvert.FormCreate(Sender: TObject);
begin
  MZFLevel.EncodeAlgorithm:=TEncodeAlgorithm(0);
  MZFLevel.DictitionarySize:=13;
  MZFLevel.MathFinder:=1;
  MZFLevel.LiteralContextBits:=3;
  MZFLevel.LiteralPosBits:=0;
  MZFLevel.FastBytes:=181;
  MZFLevel.PosBits:=2;
  MZFLevel.Priority:=2;
  Level:=3;
  _pass:='';
  Caption:=ReadFromLanguage('Windows','wndConvert',Caption);
  btnConvert.Caption:=ReadFromLanguage('Buttons','btnConvert',btnConvert.Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  Label3.Caption:=ReadFromLanguage('Labels','lbSource',Label3.Caption);
  Label4.Caption:=ReadFromLanguage('Labels','lbDest',Label4.Caption);
  Label1.Caption:=ReadFromLanguage('Labels','lbFrom',Label1.Caption);
  Label2.Caption:=ReadFromLanguage('Labels','lbTo',Label2.Caption);
  cbDeleteSource.Caption:=ReadFromLanguage('Labels','lbDeleteSource',cbDeleteSource.Caption);
  dlgOpen.Title:=ReadFromLanguage('Dialogs','dlgOpen',dlgOpen.Title);
  dlgSave.Title:=ReadFromLanguage('Dialogs','dlgSave',dlgSave.Title);
  btnCompressOptions.Caption:=ReadFromLanguage('Buttons','btnRatio',btnCompressOptions.Caption);
  gbConverterOptions.Caption:=ReadFromLanguage('Labels','lbConverterOptions',gbConverterOptions.Caption);
end;

procedure TdlgConvert.cbConvertToChange(Sender: TObject);
begin
  Case cbConvertTo.ItemIndex Of
    0:edTarget.Text:=ChangeFileExt(edTarget.Text,'.mzf');
    1:edTarget.Text:=ChangeFileExt(edTarget.Text,'.7z');
    2:edTarget.Text:=ChangeFileExt(edTarget.Text,'.zip');
    3:edTarget.Text:=ChangeFileExt(edTarget.Text,'.bh');
    4:edTarget.Text:=ChangeFileExt(edTarget.Text,'.lha');
    5:edTarget.Text:=ChangeFileExt(edTarget.Text,'.jar');
    6:edTarget.Text:=ChangeFileExt(edTarget.Text,'.cab');
    7:edTarget.Text:=ChangeFileExt(edTarget.Text,'.tar');
  End;
end;

end.
