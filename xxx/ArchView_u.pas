unit ArchView_u;

interface

uses Windows,ztvBase, ztvGbls, ztvZipTV,SysUtils,Classes,Advanced;

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

implementation

procedure GetFilesAndDirs(ADirList,AFileList:TStringList;AFile:String;Var AType,Comment:String);
Var Viewer: TZipTV;
    Index:LongWord;
    Root:String;
Begin
  Viewer:=TZipTV.Create(Nil);
  Viewer.ArchiveFile:=AFile;
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
 