unit SevenZip_Compressor_u;

interface

Uses SevenZipVCL,SysUtils,Advanced,Classes;

Type TCompressType=(ctLZMA,ctPPMD);
Type TCompressLevel=(clFast,clNormal,clMaximum,clUltra);
Type TSevenZipCompressorProgress=procedure (Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean) of object;

Type TSevenZipCompressor=Class (TComponent)
  Private
    aFileSpec:TStringList;
    aFileName:String;
    Core:TSevenZip;
    fOnBegin:TNotifyEvent;
    fOnEnd:TNotifyEvent;
    fOnProgress:TSevenZipCompressorProgress;
    fMaxProgress:LongWord;
    Abort:Boolean;
    fVolumeSize:LongWord;
    fCompressType:TCompressType;
    fCompressLevel:TCompressLevel;
    fHidden,fReadOnly,fArchived:Boolean;
    fPassword:String;
    Procedure CorePreProgress(Sender: TObject; MaxProgress: Int64);
    procedure CoreProgress(Sender: TObject; Filename: WideString;
      FilePosArc, FilePosFile: Int64);
  Public
    Constructor Create;
    Property FileSpec:TStringList read aFileSpec write aFileSpec;
    Property FileName:String read aFileName write aFileName;
    Property CompressType:TCompressType read fCompressType write fCompressType default ctLZMA;
    Property CompressLevel:TCompressLevel read fCompressLevel write fCompressLevel;
    Property OnBegin:TNotifyEvent read fOnBegin write fOnBegin;
    Property OnEnd:TNotifyEvent read fOnEnd write fOnEnd;
    Property OnProgress:TSevenZipCompressorProgress read fOnProgress write fOnProgress;
    Property VolumeSize:LongWord read fVolumeSize write fVolumeSize;
    Property Password:String read fPassword write fPassword;
    Property ReadingOnly:Boolean read fReadOnly write fReadOnly;
    Property Archived:Boolean read fArchived write fArchived default True;
    Property Hidden:Boolean read fHidden write fHidden;
    Procedure Compress;
    Procedure Free;
    Destructor Destroy;
  End;

implementation

Constructor TSevenZipCompressor.Create;
Begin
  Core:=TSevenZip.Create(Self);
  aFileSpec:=TStringList.Create;
  Abort:=False;
End;

Procedure TSevenZipCompressor.Compress;
Var Index:LongWord;
Begin
  If Assigned(fOnBegin) Then
    fOnBegin(Self);
  Core.SZFileName:=aFileName;
  Core.OnPreProgress:=CorePreProgress;
  Core.OnProgress:=CoreProgress;
  Core.Files.Clear;
  Case fCompressType Of
    ctLZMA:Core.LZMACompressType:=LZMA;
    ctPPMD:Core.LZMACompressType:=PPMD;
  End;
  Case fCompressLevel Of
    clFast:Core.LZMACompressStrength:=FAST;
    clNormal:Core.LZMACompressStrength:=NORMAL;
    clMaximum:Core.LZMACompressStrength:=MAXIMUM;
    clUltra:Core.LZMACompressStrength:=ULTRA;
  End;
  Core.Password:=fPassword;
  Core.VolumeSize:=fVolumeSize;
  Core.AddOptions:=[AddRecurseDirs];
  Core.AddRootDir:=ExtractFileDir(aFileName);
  If aFileSpec.Count>0 Then
    For Index:=0 To aFileSpec.Count-1 Do
      Core.Files.AddString(aFileSpec.Strings[Index]);
  Core.Add;
  If Assigned(fOnEnd) Then
    fOnEnd(Self);
  If fHidden Then FileSetAttr(aFileName,FileGetAttr(aFileName) And faHidden);
  If fReadOnly Then FileSetAttr(aFileName,FileGetAttr(aFileName) And faReadOnly);
  If fArchived Then FileSetAttr(aFileName,FileGetAttr(aFileName) And faArchive);
End;

procedure TSevenZipCompressor.CoreProgress(Sender: TObject; Filename: WideString;
      FilePosArc, FilePosFile: Int64);
Begin
  If Assigned(fOnProgress) Then
    fOnProgress(Self,FilePosFile,GetPercentDone(0,FilePosArc,fMaxProgress),Abort);
End;

Procedure TSevenZipCompressor.CorePreProgress(Sender: TObject; MaxProgress: Int64);
Begin
  fMaxProgress:=MaxProgress;
End;

Procedure TSevenZipCompressor.Free;
Begin
  If Self<>Nil Then
    Destroy;
End;

Destructor TSevenZipCompressor.Destroy;
Begin
  Core.Free;
  aFileSpec.Free;
End;

end.
