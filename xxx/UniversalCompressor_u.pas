unit UniversalCompressor_u;

{
  Universal ZIP, BH, and so on compressor
  Powered by M.A.D.M.A.N.
}

interface

uses Classes,Windows,SysUtils,ztvtar,ztvMakeCab,ztvZip,ztvJar,ztvLha,ztvGZip,
     ztvregister,ztvBase,ztvBlakHole,ztvStreams,SignatureDetect_u{,Forms};

Type TCompressorProgress=procedure (Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean) of object;
Type TCompressValue=(cvFastest,cvNormal,cvGood,cvExtra);

Type TUniversalCompressor=Class(TComponent)
  private
    //Cmpressors
    BHCompressor: TBlakHole;
    GZipCompressor: TGZip;
    LhaCompressor: TLha;
    JarCompressor: TJar;
    MakeCabCompressor: TMakeCab;
    TarCompressor: TTar;
    ZipCompressor: TZip;
    //Type evants
    fOnBegin:TNotifyEvent;
    fOnEnd:TNotifyEvent;
    fOnProgress:TCompressorProgress;
    AFileName:String;
    AFileSpec:TStringList;
    ACompressValue:TCompressValue;
    APassword:String;
    AFileType:TFileType;
    Abort:Boolean;
    AComment:String;
    AEncodeHead:Boolean;
    AArchiveReadOnly,AArchiveHidden,AArchiveArchived:Boolean;
    procedure BeginOperation;
    procedure EndOperation;
    procedure CompressorsOnProgress(Sender: TObject; ProgressByFile,
      ProgressByArchive: Byte);
  public
    constructor Create;
    property OnBegin:TNotifyEvent read fOnBegin write fOnBegin;
    property OnEnd:TNotifyEvent read fOnEnd write fOnEnd;
    property OnProgress:TCompressorProgress read fOnProgress write fOnProgress;
    property ArchiveFile:String read AFileName write AFileName;
    property FileSpec:TStringList read AFileSpec write AFileSpec;
    property CompressValue:TCompressValue read ACompressValue write ACompressValue default cvNormal;
    property Password:String read APassword write APassword;
    property FileType:TFileType read AFileType write AFileType;
    property EncodeHeader:Boolean read AEncodeHead write AEncodeHead;
    property ReadingOnly:Boolean read AArchiveReadOnly write AArchiveReadOnly;
    property Hidden:Boolean read AArchiveHidden write AArchiveHidden;
    property Archived:Boolean read AArchiveArchived write AArchiveArchived;
    property Comment:String read AComment write AComment;
    procedure Compress;
    procedure Free;
    destructor Destroy;
  end;

implementation

constructor TUniversalCompressor.Create;
Begin
  BHCompressor:=TBlakHole.Create(Nil);
  GZipCompressor:=TGZip.Create(Nil);
  LhaCompressor:=TLha.Create(Nil);
  JarCompressor:=TJar.Create(Nil);
  MakeCabCompressor:=TMakeCab.Create(Nil);
  TarCompressor:=TTar.Create(Nil);
  ZipCompressor:=TZip.Create(Nil);
  AFileSpec:=TStringList.Create;
  BHCompressor.OnProgress:=CompressorsOnProgress;
  GZipCompressor.OnProgress:=CompressorsOnProgress;
  LhaCompressor.OnProgress:=CompressorsOnProgress;
  JarCompressor.OnProgress:=CompressorsOnProgress;
  MakeCabCompressor.OnProgress:=CompressorsOnProgress;
  TarCompressor.OnProgress:=CompressorsOnProgress;
  ZipCompressor.OnProgress:=CompressorsOnProgress;
  abort:=false;
End;

procedure TUniversalCompressor.CompressorsOnProgress(Sender: TObject; ProgressByFile,
  ProgressByArchive: Byte);
begin
  If Assigned(fOnProgress) Then
    fOnProgress(Self,ProgressByFile,ProgressByArchive,Abort);
  BHCompressor.Cancel:=Abort;
  GZipCompressor.Cancel:=Abort;
  LhaCompressor.Cancel:=Abort;
  JarCompressor.Cancel:=Abort;                               
  MakeCabCompressor.Cancel:=Abort;
  TarCompressor.Cancel:=Abort;
  ZipCompressor.Cancel:=Abort;
  //Application.ProcessMessages;
end;

Procedure TUniversalCompressor.Compress;
Var Index:LongWord;
    DeflateType:TDeflateType;
Begin
  BeginOperation;
  For Index:=0 To AFileSpec.Count-1 Do
    Begin
      BHCompressor.FileSpec.Add(AFileSpec.Strings[Index]);
      GZipCompressor.FileSpec.Add(AFileSpec.Strings[Index]);
      LhaCompressor.FileSpec.Add(AFileSpec.Strings[Index]);
      JarCompressor.FileSpec.Add(AFileSpec.Strings[Index]);
      MakeCabCompressor.FileSpec.Add(AFileSpec.Strings[Index]);
      TarCompressor.FileSpec.Add(AFileSpec.Strings[Index]);
      ZipCompressor.FileSpec.Add(AFileSpec.Strings[Index]);
    End;
  BHCompressor.Password:=APassword;
  JarCompressor.Password:=APassword;
  ZipCompressor.Password:=APassword;
  // AFileName
  Case ACompressValue Of
    cvFastest:DeflateType:=dtDeflateF;
    cvNormal:DeflateType:=dtDeflateN;
    cvGood:DeflateType:=dtDeflateS;
    cvExtra:DeflateType:=dtDeflateX;
  End;
  BHCompressor.DeflateType:=DeflateType;
  GZipCompressor.DeflateType:=DeflateType;
  LhaCompressor.DeflateType:=DeflateType;
  JarCompressor.DeflateType:=DeflateType;
  MakeCabCompressor.DeflateType:=DeflateType;
  TarCompressor.DeflateType:=DeflateType;
  ZipCompressor.DeflateType:=DeflateType;
  //
  BHCompressor.EncryptHeaders:=AEncodeHead;
  GZipCompressor.EncryptHeaders:=AEncodeHead;
  LhaCompressor.EncryptHeaders:=AEncodeHead;
  JarCompressor.EncryptHeaders:=AEncodeHead;
  MakeCabCompressor.EncryptHeaders:=AEncodeHead;
  TarCompressor.EncryptHeaders:=AEncodeHead;
  ZipCompressor.EncryptHeaders:=AEncodeHead;
  //
  BHCompressor.ArchiveFile:=AFileName;
  GZipCompressor.ArchiveFile:=AFileName;
  LhaCompressor.ArchiveFile:=AFileName;
  JarCompressor.ArchiveFile:=AFileName;
  MakeCabCompressor.ArchiveFile:=AFileName;
  TarCompressor.ArchiveFile:=AFileName;
  ZipCompressor.ArchiveFile:=AFileName;
  //
  Case AFileType Of
    ftBH:BHCompressor.Compress;
    ftGZip:GZipCompressor.Compress;
    ftLha:LhaCompressor.Compress;
    ftJar:JarCompressor.Compress;
    ftCab:MakeCabCompressor.Compress;
    ftTar:TarCompressor.Compress;
    ftZip:ZipCompressor.Compress;
  End;
  //
  {BHCompressor.SetComment(0,PChar(Comment),Length(AComment));
  GZipCompressor.SetComment(0,PChar(Comment),Length(AComment));
  LhaCompressor.SetComment(0,PChar(Comment),Length(AComment));
  JarCompressor.SetComment(0,PChar(Comment),Length(AComment));
  ZipCompressor.SetComment(0,PChar(Comment),Length(AComment));
  MakeCabCompressor.SetComment(0,PChar(Comment),Length(AComment));
  TarCompressor.SetComment(0,PChar(Comment),Length(AComment));   }
  EndOperation;
  If AArchiveReadOnly Then SetFileAttributes(PChar(AFileName),faReadOnly);
  If AArchiveHidden Then SetFileAttributes(PChar(AFileName),faHidden);
  If AArchiveArchived Then SetFileAttributes(PChar(AFileName),faArchive);
End;

procedure TUniversalCompressor.BeginOperation;
Begin
  If Assigned(fOnBegin) Then
    fOnBegin(Self);
End;

procedure TUniversalCompressor.EndOperation;
Begin
  If Assigned(fOnEnd) Then
    fOnEnd(Self);
End;

procedure TUniversalCompressor.Free;
Begin
  If Self<>Nil Then
    Destroy;
End;

destructor TUniversalCompressor.Destroy;
Begin
  //NULL Expected!!!
  BHCompressor.Free;
  GZipCompressor.Free;
  LhaCompressor.Free;
  JarCompressor.Free;
  MakeCabCompressor.Free;
  TarCompressor.Free;
  ZipCompressor.Free;
End;

end.
 