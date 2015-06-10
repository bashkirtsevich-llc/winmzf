unit LZMA_u;

interface

Uses SevenZipVCL,Classes,SysUtils,Advanced,Viewer_u;

Type T7ZipFileInfo=Packed Record
    FileName,Pach:String;
    Size:LongWord;
    PackedSize:LongWord;
    CRC:LongWord;
    CreationTime,
    ModifyTime,
    AccessTime:TDateTime;
    Attr:Word;
    ID:LongWord;
  End;

//Type TShowFileInfo=procedure (Sender:TObject;FileInfo:T7ZipFileInfo)Of Object;
Type TDecompressorProgress=procedure (Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean) of object;
Type TOverWritePrompt=procedure (Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode)of object;
Type TOnGetPassword=procedure (Sender:TObject;Var Pass:ShortString)of object;

Type TSevenZipViewer=Class(TComponent)
  Private
    Core:TSevenZip;
    aFileName:String;
    aDirs:TStringList;
    SplitDir:String;
    aOnShowFileInfo:TSowFileInfo;
    aFileSpec:TStringList;
    aDestDir:String;
    aOnBegin:TNotifyEvent;
    aOnEnd:TNotifyEvent;
    aOnProgress:TDecompressorProgress;
    aMaxProgress:LongWord;
    aOnOverWrite:TOverWritePrompt;
    aPassword:ShortString;
    Abort:Boolean;
    Mode:TReWriteMode;
    aOnGetPass:TOnGetPassword;
    Procedure FillDirsList(Sender: TObject; Filename: WideString;
        Fileindex, FileSizeU, FileSizeP, Fileattr, Filecrc: Cardinal;
        Filemethod: WideString; FileTime: Double);
    Procedure ShowInfo(Sender: TObject; Filename: WideString;
        Fileindex, FileSizeU, FileSizeP, Fileattr, Filecrc: Cardinal;
        Filemethod: WideString; FileTime: Double);
    Procedure CorePreProgress(Sender: TObject; MaxProgress: Int64);
    procedure CoreProgress(Sender: TObject; Filename: WideString;
        FilePosArc, FilePosFile: Int64);
    procedure CoreOverWrite(Sender: TObject;
        FileName: WideString; var DoOverwrite: Boolean);
    Procedure OnGetFileInfo(Sender: TObject; Filename: WideString;
        Fileindex, FileSizeU, FileSizeP, Fileattr, Filecrc: Cardinal;
        Filemethod: WideString; FileTime: Double);
  Public
    Constructor Create;
    Property FileName:String read aFileName write aFileName;
    Property Directories:TStringList read aDirs write aDirs;
    Property OnShowFileInfo:TSowFileInfo read aOnShowFileInfo write aOnShowFileInfo;
    Property FileSpec:TStringList read aFileSpec write aFileSpec;
    Property DestDir:String read aDestDir write aDestDir;
    Property OnBegin:TNotifyEvent read aOnBegin write aOnBegin;
    Property OnEnd:TNotifyEvent read aOnEnd write aOnEnd;
    Property OnProgress:TDecompressorProgress read aOnProgress write aOnProgress;
    Property OnOverWrite:TOverWritePrompt read aOnOverWrite write aOnOverWrite;
    Property OverWriteMode:TReWriteMode read Mode write Mode;
    Property OnGetPaasword:TOnGetPassword read aOnGetPass write aOnGetPass;
    Procedure Get7zArchiveInfo(Var Version,AComment:String;Var FileCount:LongWord;Var UnpackedSize,ArcSize:Int64);
    Procedure Extract;
    Procedure GetDirs;
    Procedure GetFilesFromDir(Directory:String);
    Procedure Free;
    Destructor Destroy;
  End; 

implementation

var
  Count:LongWord;
  UnPackSize{,ASize}:Int64;

Constructor TSevenZipViewer.Create;
Begin
  Core:=TSevenZip.Create(Self);
  aDirs:=TStringList.Create;
  aFileSpec:=TStringList.Create;
  Abort:=False;
  aPassword:='';
End;

Procedure TSevenZipViewer.GetDirs;
Begin
  Core.OnListfile:=FillDirsList;
  Core.SZFileName:=aFileName;
  Core.List;
End;

Procedure TSevenZipViewer.GetFilesFromDir(Directory:String);
Begin
  Core.OnListfile:=ShowInfo;
  Core.SZFileName:=aFileName;
  SplitDir:=Directory;
  Core.List;
End;

Procedure TSevenZipViewer.Extract;
Var Index:LongWord;
Begin
  If Assigned(aOnBegin)Then
    aOnBegin(Self);
  Core.SZFileName:=aFileName;
  Core.ExtrBaseDir:=aDestDir;
  Core.OnPreProgress:=CorePreProgress;
  //Core.Password:=aPassword;
  Core.OnProgress:=CoreProgress;
  Core.OnExtractOverwrite:=CoreOverWrite;
  Core.ExtractOptions:=[ExtractOverwrite];
  Core.Files.Clear;
  If aFileSpec.Count>0 Then
  For Index:=0 To aFileSpec.Count-1 Do
    Core.Files.AddString(aFileSpec.Strings[Index]);
  Core.Extract();
  If Assigned(aOnEnd)Then
    aOnEnd(Self);
End;

Procedure TSevenZipViewer.Get7zArchiveInfo(Var Version,AComment:String;Var FileCount:LongWord;Var UnpackedSize,ArcSize:Int64);
Var FileStream:TFileStream;
Begin
  FileStream:=TFileStream.Create(aFileName,fmOpenRead);
  ArcSize:=FileStream.Size;
  FileStream.Free;
  Core.SZFileName:=aFileName;
  AComment:=Core.SevenZipComment;
  //Version:=Core.SFXModule;
  Version:='7z';
  UnPackSize:=0;
  //ASize:=0;
  Count:=0;
  Core.OnListfile:=OnGetFileInfo;
  Core.List;
  //Count:=0;
  FileCount:=Count;
  UnpackedSize:=UnPackSize;
End;

Procedure TSevenZipViewer.OnGetFileInfo(Sender: TObject; Filename: WideString;
  Fileindex, FileSizeU, FileSizeP, Fileattr, Filecrc: Cardinal;
  Filemethod: WideString; FileTime: Double);
Begin
  If (Fileattr Xor faDirectory<>0) Then
    Begin
      Inc(UnPackSize,FileSizeU);
      Inc(Count);
    End;
End;

Procedure TSevenZipViewer.CorePreProgress(Sender: TObject; MaxProgress: Int64);
Begin
 If Maxprogress > 0 Then aMaxProgress := Maxprogress;
End;

procedure TSevenZipViewer.CoreProgress(Sender: TObject; Filename: WideString;
  FilePosArc, FilePosFile: Int64);
begin
  If Assigned(aOnProgress)Then
    aOnProgress(Self,fileposfile,GetPercentDone(0,fileposArc,aMaxProgress),Abort);
  If Abort Then
    Core.Cancel;
 {progressbar2.Position := fileposArc;
 progressbar1.Position := fileposfile;
 application.ProcessMessages;   }
end;

procedure TSevenZipViewer.CoreOverWrite(Sender: TObject;
  FileName: WideString; var DoOverwrite: Boolean);
Var AFile:TFileHead;
begin
   //DOOverwrite := true
  AFile.LongFileName:=FileName;
  Afile.Attr:=FileGetAttr(FileName);
  AFile.CRC:=0;
  AFile.Size:=0;
  AFile.PackedSize:=0;
  AFile.FileCreateDate:=0;
  AFile.FileModifyDate:=0;
  AFile.FileOpenDate:=0;
  If Mode=omOverwriteAll Then
    Begin
      DoOverwrite:=True;
      Exit;
    End;
  If Mode=omSkipAll Then
    Begin
      DoOverwrite:=False;
      Exit;
    End;
  If Assigned(aOnOverWrite) Then
    Begin
      aOnOverWrite(Self,AFile,Mode);
      If Mode=omRewrite Then
        DoOverwrite:=True Else
        DoOverwrite:=False;
    End;
end;

Procedure TSevenZipViewer.FillDirsList(Sender: TObject; Filename: WideString;
  Fileindex, FileSizeU, FileSizeP, Fileattr, Filecrc: Cardinal;
  Filemethod: WideString; FileTime: Double);
Var Root:String;
Begin
  {If FileAttr=faDirectory Then
    Begin}
      Root:=Format(RootCaption,[ExtractFileName(aFileName)]);
      If Not TextInList(Root+'\'+ExtractFileDir(FileName),aDirs) Then
        aDirs.Add(Root+'\'+ExtractFileDir(FileName));
    {End;}
End;

Procedure TSevenZipViewer.ShowInfo(Sender: TObject; Filename: WideString;
  Fileindex, FileSizeU, FileSizeP, Fileattr, Filecrc: Cardinal;
  Filemethod: WideString; FileTime: Double);
Var FileInfo:TFileHead;
Begin
  If (SplitDir<>'%ALL%')And(ExtractFileDir(FileName)<>SplitDir) Then Exit;
  If (Fileattr And faDirectory)=faDirectory Then Exit;
  FileInfo.LongFileName:=(FileName);
  FileInfo.Size:=FileSizeU;
  FileInfo.PackedSize:=FileSizeP;
  FileInfo.CRC:=Filecrc;
  FileInfo.FileCreateDate:=FileTime;
  FileInfo.FileModifyDate:=FileTime;
  FileInfo.FileOpenDate:=FileTime;
  FileInfo.Attr:=Fileattr;
  If Assigned(aOnShowFileInfo) Then
    aOnShowFileInfo(Self,FileInfo,FileIndex);
End;

Procedure TSevenZipViewer.Free;
Begin
  If Self<>Nil Then
    Destroy;
End;

Destructor TSevenZipViewer.Destroy;
Begin
  Core.Free;
  aDirs.Free;
  aFileSpec.Free;
End;

end.
