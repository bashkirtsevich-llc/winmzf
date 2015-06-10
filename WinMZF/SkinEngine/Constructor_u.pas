unit Constructor_u;

interface

uses Classes, SysUtils, Zlib, Windows, Graphics, Dialogs;

type
  TFile = packed record
    FileName:String;
    Name:ShortString;
  end;

type
  TBuilderInfo = packed record
    files:array[0..17]of TFile;
  end;

type
  TSkinHead = packed record
    Signature:Array[0..2]Of Char;
    Version:Array[0..3]Of Char;
  end;

type
  TSkinInfo = packed record
    Author:ShortString;
    SkinVersion:ShortString;
    Create:TDateTime;
    Comment:ShortString;
    Height,Width:Byte;
  end;

type
  TPictureInfo = packed record
    Size{,}
    {PackedSize}:LongWord;
    Name:ShortString;
  end;

type
  TSize = packed record
    Height,Width:Byte;
  end;

function GetImgSize(FileName:String):TSize;
procedure GetSkinInfo(FileName:String;var Author,Version,Create,Comment:String; var Height,Width:Byte);
function GetDataByIndex(FileName:String; ImgIndex:Byte; Data:TStream):Boolean;
procedure ViewSkin(FileName:String;var Author,Version,Create,Comment:String; Height,Width:Byte; var List:TStringList);
procedure BuildSkin(SkinName:String;BuilderInfo:TBuilderInfo;SkinInfo:TSkinInfo);

const
  _Signature='MSK';
  _Version='1.00';

implementation

procedure CompressStream(inStream, outStream :TStream; const Level : TCompressionLevel = clMax);
begin
  InStream.Position:=0;
 with TCompressionStream.Create(TCompressionLevel(Level), outStream) do
  try
   CopyFrom(inStream, inStream.Size);
  finally
   Free;
  end;
end;

procedure ExpandStream(inStream, outStream :TStream);
const
 BufferSize = 4096;
var
 Count: integer;
 ZStream: TDecompressionStream;
 Buffer: array[0..BufferSize-1] of Byte;
begin
  InStream.Position:=0;
 ZStream:=TDecompressionStream.Create(InStream);
 try
  while true do
   begin
    Count:=ZStream.Read(Buffer, BufferSize);
    if Count<>0
    then OutStream.WriteBuffer(Buffer, Count)
    else Break;
   end;
  finally
   ZStream.Free;
  end;
end;

procedure BuildSkin(SkinName:String;BuilderInfo:TBuilderInfo;SkinInfo:TSkinInfo);
var SkinStream:TMemoryStream;
    FileStream:TStream;
    Temp:TStream;
    Picture:TFileStream;
    PicInfo:TPictureInfo;                                          
    //SkinInfo:TSkinInfo;
    FileHead:TSkinHead;
    Index:Byte;
    Icon:TIcon;
    Bitmap:TBitMap;
begin
  FileHead.Signature:=_Signature;
  FileHead.Version:=_Version;
  SkinStream:=TMemoryStream.Create;
  SkinStream.Write(FileHead,SizeOf(TSkinHead));
  SkinStream.Write(SkinInfo,SizeOf(TSkinInfo));
  for Index:=0 to 8 do
    begin
      if BuilderInfo.files[Index].FileName<>'' then
        begin
          {Icon:=TIcon.Create;
          Icon.LoadFromFile(BuilderInfo.files[Index].FileName);
          Temp:=TMemoryStream.Create;
          Icon.SaveToStream(Temp);
          Icon.Free;  }
          FileStream:=TMemoryStream.Create;
          Icon:=TIcon.Create;
          Icon.LoadFromFile(BuilderInfo.files[Index].FileName);
          Icon.SaveToStream(FileStream);
          Icon.Free;
          Temp:=TMemoryStream.Create;
          CompressStream(FileStream,Temp);
          FileStream.Free;
          Temp.Position:=0;
          PicInfo.Size:=Temp.Size;
          //PicInfo.PackedSize:=Temp.Size;
          PicInfo.Name:=BuilderInfo.files[Index].Name;
          SkinStream.Write(PicInfo,SizeOf(TPictureInfo));
          SkinStream.CopyFrom(Temp,Temp.Size);
          Temp.Free;
        end else
        begin
          PicInfo.Size:=0;
          SkinStream.Write(PicInfo,SizeOf(TPictureInfo));
        end;   
    end;
  {for Index := 8 to 14 do
    begin
      if BuilderInfo.files[Index].FileName<>'' then
        begin
          BitMap:=TBitMap.Create;
          BitMap.LoadFromFile(BuilderInfo.files[Index].FileName);
          Temp:=TMemoryStream.Create;
          BitMap.SaveToStream(Temp);
          Temp.Position:=0;
          Bitmap.Free;
          PicInfo.Size:=Temp.Size;   
          //PicInfo.PackedSize:=Temp.Size;
          PicInfo.Name:=BuilderInfo.files[Index].Name;
          SkinStream.Write(PicInfo,SizeOf(TPictureInfo));
          SkinStream.CopyFrom(Temp,Temp.Size);
          Temp.Free;
        end else
        begin
          PicInfo.Size:=0;
          SkinStream.Write(PicInfo,SizeOf(TPictureInfo));
        end;
    end;}
  SkinStream.SaveToFile(SkinName);
  SkinStream.Free;
end;

procedure ViewSkin(FileName:String;var Author,Version,Create,Comment:String; Height,Width:Byte; var List:TStringList);
var SkinStream:TFileStream;
    PicInfo:TPictureInfo;
    SkinInfo:TSkinInfo;
    FileHead:TSkinHead;
    Index:Byte;
begin
  If Not FileExists(FileName) Then Exit;
  SkinStream:=TFileStream.Create(FileName,fmOpenRead);
  SkinStream.Read(FileHead,SizeOf(TSkinHead));
  if (FileHead.Signature<>_Signature) or (FileHead.Version<>_Version) then
    begin
      SkinStream.Free;
      Exit;
    end;
  SkinStream.Read(SkinInfo,SizeOf(TSkinInfo));
  try
    Author:=SkinInfo.Author;
    Version:=SkinInfo.SkinVersion;
    Create:=DateTimeToStr(SkinInfo.Create);
    Comment:=SkinInfo.Comment;
    Height:=SkinInfo.Height;
    Width:=SkinInfo.Width;
  except
    //NULL
  end;
  for Index := 0 to 8 do
    begin
      SkinStream.Read(PicInfo,SizeOf(TPictureInfo));
      SkinStream.Seek(PicInfo.Size,soFromCurrent);
      List.Add(Format('Name=%s Size=%d Packed size=%d',[PicInfo.Name,PicInfo.Size,PicInfo.Size]));
    end;
  SkinStream.Free;
end;

procedure GetSkinInfo(FileName:String;var Author,Version,Create,Comment:String; var Height,Width:Byte);
var SkinStream:TFileStream;
    PicInfo:TPictureInfo;
    SkinInfo:TSkinInfo;
    FileHead:TSkinHead;
begin
  If Not FileExists(FileName) Then Exit;
  SkinStream:=TFileStream.Create(FileName,fmOpenRead);
  SkinStream.Read(FileHead,SizeOf(TSkinHead));
  //showmessage(filehead.Signature+'='+filehead.Version);
  if (FileHead.Signature<>_Signature) or (FileHead.Version<>_Version) then
    begin
      SkinStream.Free;
      Exit;
    end;
  SkinStream.Read(SkinInfo,SizeOf(TSkinInfo));
  try
    Author:=SkinInfo.Author;
    Version:=SkinInfo.SkinVersion;
    Create:=DateTimeToStr(SkinInfo.Create);
    Comment:=SkinInfo.Comment;
    Height:=SkinInfo.Height;
    Width:=SkinInfo.Width;
  except
    //NULL
  end;
  SkinStream.Free;
end;

function GetImgSize(FileName:String):TSize;
var SkinStream:TFileStream;
    PicInfo:TPictureInfo;
    SkinInfo:TSkinInfo;
    FileHead:TSkinHead;
    Index:Byte;
begin
  Result.Height:=48;
  Result.Width:=48;
  If Not FileExists(FileName) Then Exit;
  SkinStream:=TFileStream.Create(FileName,fmOpenRead);
  SkinStream.Read(FileHead,SizeOf(TSkinHead));
  if (FileHead.Signature<>_Signature) or (FileHead.Version<>_Version) then
    begin
      SkinStream.Free;
      Exit;
    end;
  SkinStream.Read(SkinInfo,SizeOf(TSkinInfo));
  //showmessage(format('h=%d w=%d',[SkinInfo.Height,SkinInfo.Width]));
  Result.Height:=SkinInfo.Height;
  Result.Width:=SkinInfo.Width;
  SkinStream.Free;
end;

function GetDataByIndex(FileName:String; ImgIndex:Byte; Data:TStream):Boolean;
var SkinStream:TFileStream;
    PicInfo:TPictureInfo;
    SkinInfo:TSkinInfo;
    FileHead:TSkinHead;
    Temp:TStream;
    Index:Byte;
begin
  If Not FileExists(FileName) Then Exit;
  SkinStream:=TFileStream.Create(FileName,fmOpenRead);
  SkinStream.Read(FileHead,SizeOf(TSkinHead));
  if (FileHead.Signature<>_Signature) or (FileHead.Version<>_Version) then
    begin
      SkinStream.Free;
      Result:=False;
      Exit;
    end;
  SkinStream.Read(SkinInfo,SizeOf(TSkinInfo));    
  For Index:=0 To 8 Do
    Begin
      SkinStream.Read(PicInfo,SizeOf(TPictureInfo));
      //ShowMessage(inttostr(picinfo.Size));
      If ImgIndex=Index Then
        begin
          Temp:=TMemoryStream.Create;
          Temp.CopyFrom(SkinStream,PicInfo.Size);                                          
          ExpandStream(Temp,Data);
          Temp.Free;
        end
      Else                          
        SkinStream.Seek(PicInfo.Size,soFromCurrent);
    End;
  SkinStream.Free;
  Result:=True;
end;

end.
