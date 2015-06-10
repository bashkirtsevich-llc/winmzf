unit ArchView_u;

interface

uses Windows,ztvBase, ztvGbls, ztvZipTV,SysUtils,Classes,Advanced,ztvZip,
     SignatureDetect_u,ztvBlakHole, ztvLha, ztvJar, ztvTar;

type TFileInfo=Packed Record
    Size:LongWord;
    PackedSize:LongWord;
    CRC:LongWord;
    CreationTime,
    ModifyTime,
    AccessTime:TDateTime;
    Attr:Word;
  End;

procedure GetFilesAndDirs(ADirList,AFileList:TStringList;AFile:String;Var AType,Comment:String);
procedure GetFileInfo(Archive,FileName:String;Var FileInfo:TFileInfo);
procedure ZTVDeleteFiles(FileName:String;AFiles:TStringList;ArcType:TFileType);
procedure GetArchiveInfo(FileName:String;Var Version,AComment:String;Var FileCount:LongWord;Var UnpackedSize,ArcSize:Int64);

implementation

procedure ZTVDeleteFiles(FileName:String;AFiles:TStringList;ArcType:TFileType);
var CompressComponent: TCompBase;
    Index:LongWord;
Begin
  If AFiles.Count<=0 Then
    Exit;
  Case ArcType Of
    ftZip:CompressComponent:=TZip.Create(Nil);
    ftBH:CompressComponent:=TBlakHole.Create(Nil);
    ftLha:CompressComponent:=TLha.Create(Nil);
    ftJar:CompressComponent:=TJar.Create(Nil);
    ftTar:CompressComponent:=TTar.Create(Nil);
    Else Exit;
  End;
  CompressComponent.ArchiveFile:=FileName;
  CompressComponent.FileSpec.Clear;
  For Index:=0 To AFiles.Count-1 Do
    CompressComponent.FileSpec.Add(AFiles.Strings[Index]);
  CompressComponent.Switch:=swDelete;
  CompressComponent.Compress;
  CompressComponent.Free;
End;

procedure GetArchiveInfo(FileName:String;Var Version,AComment:String;Var FileCount:LongWord;Var UnpackedSize,ArcSize:Int64);
Var Viewer:TZipTV;
    Files:TStringList;
    Index:LongWord;
    FileStream:TFileStream;
Begin
  FileStream:=TFileStream.Create(FileName,fmOpenRead);
  //FileStream.LoadFromFile(FileName);
  ArcSize:=FileStream.Size;
  FileStream.Free;
  Viewer:=TZipTV.Create(Nil);
  Viewer.ArchiveFile:=FileName;
  Viewer.FileSpec.Clear;
  Viewer.FileSpec.Add('*.*');
  AComment:=Viewer.FileComment;
  Version:=ArcTypeNames[Viewer.ArcType];
  UnPackedSize:=Viewer.fUnpackedSize;
  Files:=TStringList.Create;
  Viewer.FilesInArchive(Files);
  FileCount:=Files.Count;
  For Index:=0 To Files.Count-1 Do
    Begin
      Viewer.GetFileInfo(Files.Strings[Index]);
      Inc(UnpackedSize,Viewer.GetFileUnpackedSize(Files.Strings[Index]));
    End;    
  //UnPackedSize:=Viewer.UnpackedSize;
  Viewer.Free;
  Files.Free;
  //FileStream.Free;
End;

procedure GetFilesAndDirs(ADirList,AFileList:TStringList;AFile:String;Var AType,Comment:String);
Var Viewer: TZipTV;
    Index:LongWord;
    Root:String;
Begin
  Viewer:=TZipTV.Create(Nil);
  Viewer.ArchiveFile:=AFile;
  Viewer.FileSpec.Clear;
  Viewer.FileSpec.Add('*.*');
  AType:=ArcTypeNames[Viewer.ArcType];
  Viewer.FilesInArchive(AFileList);
  Comment:=Viewer.FileComment;
  Root:=Format(RootCaption,[ExtractFileName(AFile)]);
  ADirList.Add(Root);
  For Index:=0 To AFileList.Count-1 Do
    If Not TextInList(Root+'\'+ExtractFileDir(AFileList.Strings[Index]),ADirList) Then
      ADirList.Add(Root+'\'+ExtractFileDir(AFileList.Strings[Index]));
  Viewer.Free;
End;

procedure GetFileInfo(Archive,FileName:String;Var FileInfo:TFileInfo);
Var Viewer: TZipTV;
Begin
  Viewer:=TZipTV.Create(Nil);
  Viewer.ArchiveFile:=Archive;
  Viewer.FileSpec.Add('*.*');
  Viewer.GetFileInfo(FileName);
  FileInfo.Size:=Viewer.GetFileUnpackedSize(FileName);
  FileInfo.PackedSize:=Viewer.GetFilePackedSize(FileName);
  FileInfo.CRC:=Viewer.GetFileCrc(FileName);
  FileInfo.CreationTime:=Viewer.GetFileDate(FileName);
  FileInfo.ModifyTime:=Viewer.GetFileDate(FileName);
  FileInfo.AccessTime:=Viewer.GetFileDate(FileName);
  FileInfo.Attr:=$0;{Viewer.UBFI.FileAttr;}
  Viewer.Free;
End;

end.
 