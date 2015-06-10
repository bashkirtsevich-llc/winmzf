unit Compress_u;

interface

uses Classes,ExtCtrls,Advanced,SysUtils,ImageHlp,Windows,forms,CRC32,Dialogs,
     ULZMAEncoder,ULZMACommon;

//Type TCompressionLevel=(clNone,clFastest,clDefault,clMax);
Type TPassQuest=procedure (Sender:TObject;Var Pass:ShortString)of object;
Type TCompressorProgress=procedure (Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean) of object;

Type TMZFCompressionLevel=Packed Record
    EncodeAlgorithm:TEncodeAlgorithm;
    DictitionarySize:Byte;
    MathFinder:Byte;
    LiteralContextBits:Byte;
    LiteralPosBits:Byte;
    FastBytes:Word;
    PosBits:Byte;
    Priority:Byte; 
  End;

Type TMZFCompressor=class (TComponent)
  private
    FileName:String;
    Files:TStringList;
    DisplayTimer:TTimer;
    GeneralProgress,FileProgress:LongWord;
    fCompressorProgress:TCompressorProgress;
    fOnBegin:TNotifyEvent;
    fOnEnd:TNotifyEvent;
    fOnPassQuest:TPassQuest;
    FMax,AMax:LongWord;
    ReadingOnly:Boolean;
    Hidden:Boolean;
    Archive:Boolean;
    CompressLevel:TMZFCompressionLevel;
    EncodeHead:Boolean;
    EncodeFile:Boolean;
    //EncodingMode:TEncodeMode;
    Pass:ShortString;
    Comment:WideString;
    fEncodeAlgorithm:TEncodeAlgorithm;
    procedure ShowProgress(Sender:TObject);
    function CompressStream(InStream,OutStream:Tstream):Boolean;
    procedure SearchDir(Dir,Ext:String);
    procedure CompressorBegin;
    procedure CompressorEnd;
    function IsMZFArchive:Boolean;
    procedure GetPassword;
    procedure OnLZMAProgress(const Action:TLZMAProgressAction;const Value:int64);
  public
    Abort:Boolean;
    constructor Create;
    property ArchReadOnly:Boolean read ReadingOnly write ReadingOnly default False;
    property ArcHidden:Boolean read Hidden write Hidden default False;
    property ArcArchive:Boolean read Archive write Archive default True;
    property Level:TMZFCompressionLevel read CompressLevel write CompressLevel;
    property ArchiveFileName:String read FileName write FileName;
    property FileList:TStringList read Files write Files;
    property Commentary:WideString read Comment write Comment;
    property Password:ShortString read Pass write Pass;
    property EncodeHeadInfo:Boolean read EncodeHead write EncodeHead default False;
    property EncodeFileContain:boolean read EncodeFile write EncodeFile default False;
    //property EncodeMode:TEncodeMode read EncodingMode write EncodingMode default emAfterCompress;
    property EncodeAlgorithm:TEncodeAlgorithm read fEncodeAlgorithm write fEncodeAlgorithm default eaAES256;
    property OnProgress:TCompressorProgress read fCompressorProgress write fCompressorProgress;
    property OnBegin:TNotifyEvent read fOnBegin write fOnBegin;
    property OnEnd:TNotifyEvent read fOnEnd write fOnEnd;
    property OnGetPassword:TPassQuest read fOnPassQuest write fOnPassQuest;
    procedure CreateArchive;
    procedure Free;
    destructor Destroy;
  protected
  end;

  procedure register;

implementation

//{$R Compress_u.DCR} LZMA_StreamCoderDecoder_u

procedure register;
begin
  RegisterComponents('M.A.D.M.A.N.', [TMZFCompressor]);
end;

Procedure CompressHead(InStream,OutStream:TStream);
Var encoder:TLZMAEncoder;
    i:Byte;
    s:byte;
    //a:array[0..4]of byte;
Begin
  InStream.Position:=0;
  OutStream.Position:=0;

  encoder:=TLZMAEncoder.Create;
  encoder.SetAlgorithm(1);  //1
  encoder.SetDictionarySize(0);//0
  encoder.SeNumFastBytes(273);    //273
  encoder.SetLcLpPb(0,0,0);       //0,0,0
  encoder.SetMatchFinder(2);      //0
  s:=inStream.Size;
  OutStream.Write(s,SizeOf(byte));
  {for i := 0 to 7 do
    WriteByte(outStream,(InStream.Size shr (8 * i)) and $FF);}
  encoder.Code(InStream,OutStream,-1,-1);

  encoder.Free;                        
  //showmessage(Format('in %d out %d',[instream.Size,outstream.Size]));
  InStream.Position:=0;
  OutStream.Position:=0;
End;

constructor TMZFCompressor.Create;
Begin
  Files:=TStringList.Create;
  DisplayTimer:=TTimer.Create(self);
  DisplayTimer.Interval:=1;
  DisplayTimer.Enabled:=False;
  DisplayTimer.OnTimer:=ShowProgress;
End;

procedure TMZFCompressor.OnLZMAProgress(const Action:TLZMAProgressAction;const Value:int64);
Begin
  If Action=LPAMax Then
    Begin
      //Progressbar1.Max:=Value;
      FMax:=value;
      Exit;
    End;
  FileProgress:=Value;
  Application.ProcessMessages;
End;

procedure TMZFCompressor.ShowProgress(Sender:TObject);
Var A,F:Byte;
Begin
  A:=0;
  F:=0;
  A:=GetPercentDone(0,GeneralProgress,AMax);
  F:=GetPercentDone(0,FileProgress,FMax);
  if Assigned(fCompressorProgress) then
    fCompressorProgress(Self,F,A,Abort);
End;

function TMZFCompressor.CompressStream(InStream,OutStream:Tstream):Boolean;
Var Data:Byte;
    encoder:TLZMAEncoder;
    i:Byte;
    Size:LongWord;
Begin
  InStream.Position:=0;
  FMax:=InStream.Size;
  FileProgress:=0;

  encoder:=TLZMAEncoder.Create;
  encoder.OnProgress:=OnLZMAProgress;
  encoder.SetAlgorithm(1);
  {!!!}//encoder.SetDictionarySize(CompressLevel.DictitionarySize);
  encoder.SetDictionarySize({(1 Shl GetDictBySize(InStream.Size)){ Or }(1 Shl CompressLevel.DictitionarySize));
  encoder.SetMatchFinder(CompressLevel.MathFinder);
  encoder.SeNumFastBytes(CompressLevel.FastBytes);
  encoder.SetLcLpPb(CompressLevel.LiteralContextBits,CompressLevel.LiteralPosBits,CompressLevel.PosBits);
  encoder.WriteCoderProperties(OutStream);
  Size:=InStream.Size;
  OutStream.Write(Size,SizeOf(LongWord));
  {for i := 0 to 7 do
    WriteByte(outStream,(InStream.Size shr (8 * i)) and $FF);}
  encoder.Code(InStream,OutStream,-1,-1);
  encoder.Free;
  Data:=0;
  FMax:=0;
  //showmessage(Format('in %d out %d',[instream.Size,outstream.Size]));
  Result:=True;
End;

procedure TMZFCompressor.SearchDir(Dir,Ext: string);
var
  SR: TSearchRec;
  FindRes: Integer;
begin
  FindRes := FindFirst(Dir + '*.*', faAnyFile, SR);
       while FindRes = 0 do
          begin
            if ((SR.Attr and faDirectory) = faDirectory) and
            ((SR.Name = '.') or (SR.Name = '..')) then
               begin
                 FindRes := FindNext(SR);
                 Continue;
               end;
            if ((SR.Attr and faDirectory) = faDirectory) then
              begin
                SearchDir(Dir + SR.Name + '\',Ext);
                FindRes := FindNext(SR);
                Continue;
              end;
            //Files.Add(Dir + SR.Name);//Add to list
          if FileMaskEquate(SR.Name, Ext) then 
             Files.Add(Dir+SR.Name);
            //showmessage(dir+sr.Name);
            FindRes := FindNext(SR);
          end;
  FindClose(FindRes);
end;

function TMZFCompressor.IsMZFArchive:Boolean;
Var AHead:TArcHead;
    FHead:TFileHead;
    ArchiveStream:TMemoryStream;
    CommentSize:LongWord;
Begin
  ArchiveStream:=TMemoryStream.Create;
  ArchiveStream.LoadFromFile(FileName);
  ArchiveStream.Position:=$0;
  ArchiveStream.Read(AHead,SizeOf(TArcHead));
  Result:=CheckHead(AHead);
  ArchiveStream.Free;
End;

procedure TMZFCompressor.GetPassword;
begin
  If Assigned(fOnPassQuest) Then
    fOnPassQuest(Self,Pass);
end;

procedure TMZFCompressor.CreateArchive;
Var AHead:TArcHead;
    AConfig:TArcConfig;
    FHead:TFileHead;
    SaveStream:TFileStream;
    TargetFile:TFileStream;
    PackedFile:TStream;
    PackedHead,Head,PackedComment:TStream;
    FileIndex:Integer;
    C{,I}:Cardinal;
    CommentSize:LongWord;
    HeadSize:Word;
    Temp:TStream;
    Dir:String;
    Index:LongWord;
    //Dirs:TStringList;
    //S:string;
    FileDir:String;
    IsMZF:Boolean;
    LongNameSize:Word;
    aLongName:String;
    aTPriority,aPPriority:Integer;
    h:THandle;
    //FileHeadCRC32:Cardinal;
    Label Adding;
Begin
  //SetPriority(REALTIME_PRIORITY_CLASS,THREAD_PRIORITY_TIME_CRITICAL);
  fEncodeAlgorithm:=CompressLevel.EncodeAlgorithm;
  Case CompressLevel.Priority Of
    0:Begin aTPriority:=THREAD_PRIORITY_IDLE; aPPriority:=IDLE_PRIORITY_CLASS; End;
    1:Begin aTPriority:=THREAD_PRIORITY_BELOW_NORMAL; aPPriority:=NORMAL_PRIORITY_CLASS; End;
    2:Begin aTPriority:=THREAD_PRIORITY_NORMAL; aPPriority:=NORMAL_PRIORITY_CLASS; End;
    3:Begin aTPriority:=THREAD_PRIORITY_HIGHEST; aPPriority:=HIGH_PRIORITY_CLASS; End;
    4:Begin aTPriority:=THREAD_PRIORITY_TIME_CRITICAL; aPPriority:=REALTIME_PRIORITY_CLASS; End;
  End;
  SetPriority(aPPriority,aTPriority);
  CompressorBegin;
  IsMZF:=(FileExists(FileName) And IsMZFArchive); 
  For FileIndex:=0 To Files.Count-1 Do
    Begin
      FileDir:=ExtractFileDir(Files.Strings[FileIndex]);
      If FileDir[Length(FileDir)]<>'\' Then FileDir:=FileDir+'\';
      If Pos('*',Files.Strings[FileIndex])<>0 Then
        Begin
          //showmessage(FileDir);
          SearchDir(FileDir,ExtractFileName(Files.Strings[FileIndex]));
          //Files.Delete(FileIndex);
        End;
    End;
  FileIndex:=0;
  While FileIndex<=Files.Count-1 Do
    Begin
      If Pos('*',Files.Strings[FileIndex])<>0 Then
        Begin
          Files.Delete(FileIndex);
        End Else
      Inc(FileIndex);
    End;
  If Not FileExists(FileName) Then
    begin
      h:=CreateFile(PChar(FileName),GENERIC_WRITE,FILE_SHARE_WRITE, nil, CREATE_NEW, 0, 0);
      CloseHandle(h);
    end;
  SaveStream:=TFileStream.Create(FileName,fmOpenReadWrite);
  FillHead(AHead);
  AConfig.EncodeHead:=EncodeHead;
  AConfig.EncodeFile:=EncodeFile;                    
  //AConfig.EncodeMode:=EncodingMode;
  AConfig.UseComment:=Self.Comment<>'';
  AConfig.EncodeAlgorithm:=fEncodeAlgorithm;
  SaveStream.Write(AHead,SizeOf(TArcHead));
  SaveStream.Write(AConfig,SizeOf(TArcConfig));
  //Work with comment
  If AConfig.UseComment Then
    Begin
      PackedComment:=TMemoryStream.Create;  //Start comment
      WriteWideString(PackedComment,Comment);
      //PackedComment.Write(Comment,Length(Comment)+1);
      Temp:=TMemoryStream.Create;
      CompressHead(PackedComment,Temp);
      PackedComment:=TMemoryStream.Create;
      Temp.Position:=0;
      PackedComment.CopyFrom(Temp,Temp.Size);
      Temp.Free;
      PackedComment.Position:=0;
      //write comment size
      CommentSize:=PackedComment.Size;
      SaveStream.Write(CommentSize,SizeOf(LongWord));
      //Write comment
      SaveStream.CopyFrom(PackedComment,PackedComment.Size);   //End comment
    End;
  //End of comment
  If IsMZF Then
    Begin
      //SaveStream.Clear;
      //SaveStream.LoadFromFile(FileName);
      SaveStream.Free;
      SaveStream:=TFileStream.Create(FileName,fmOpenReadWrite);
      SaveStream.Position:=0;
      SaveStream.Read(AHead,SizeOf(TArcHead));
      //Set Encode Mode And So On...
      Self.EncodeHead:=AConfig.EncodeHead;
      Self.EncodeFile:=AConfig.EncodeFile;
      //Self.EncodingMode:=AConfig.EncodeMode;
      SaveStream.Seek(0,soFromEnd);
    End;
  //Adding:; //Black Label
  AMax:=Files.Count;
  GeneralProgress:=0;
  DisplayTimer.Enabled:=True;
  {Dirs:=TStringList.Create;
  For Index:=0 To Files.Count-1 Do 
    //If Not TextInList(ExtractFileDir(Files.Strings[Index]),Dirs) Then
      Dirs.Add(ExtractFileDir(Files.Strings[Index]));  }

  Dir:={GetLastDir}(GetDominateDir(Files));

  //showmessage(dir);
  {If Dir='' Then
    Dir:=GetLastDir(ExtractFileDir(Files.Strings[Index]));  }
  //Dirs.Free;
  For FileIndex:=0 To Files.Count-1 Do         
    Begin
      //ShowMessage(Files.Strings[FileIndex]);//
      //continue;
      TargetFile:=TFileStream.Create(Files.Strings[FileIndex],fmOpenRead);
      //TargetFile.LoadFromFile(Files.Strings[FileIndex]);
      C:=0;
      {I:=0;}
      //CheckSumMappedFile(TargetFile.Memory, TargetFile.Size, @I, @C);
(*CRC*)C:=StreamCRC32(TargetFile);
      //Compress File
      PackedFile:=TMemoryStream.Create;
      //Compressing
      TargetFile.Position:=0;
      PackedFile.Position:=0;
      If TargetFile.Size>0 Then
      If (Abort) Or (Not CompressStream(TargetFile,PackedFile)) Then
        Begin
          SaveStream.Free;
          TargetFile.Free;
          PackedFile.Free;
          CompressorEnd;
          Exit;
        End;
      //End compressing
      If Self.EncodeFile Then
        Begin
          If Pass='' Then
            GetPassword;
          Temp:=TMemoryStream.Create;
          TargetFile.Position:=0;
          EncodeStream(PackedFile,Temp,Pass,fEncodeAlgorithm);
          PackedFile:=TMemoryStream.Create;
          Temp.Position:=0;
          If Temp.Size>0 Then
            PackedFile.CopyFrom(Temp,Temp.Size);
          PackedFile.Position:=0;
          Temp.Free;
        End;
      //End Compression                                                            
      
      FHead.LongFileName:='';                    
      FHead.LongFileName:=SubStractString(ExtractFileDir(Files.Strings[FileIndex]),GetLastDir(Dir))+'\'+
                                          ExtractFileName(Files.Strings[FileIndex]);
      
      FHead.Size:=TargetFile.Size;
      FHead.PackedSize:=PackedFile.Size;
      //Work with file date and time
      FHead.FileCreateDate:=FileDateToDateTime(FileCreationTime(Files.Strings[FileIndex]));
      FHead.FileModifyDate:=FileDateToDateTime(FileModifyTime(Files.Strings[FileIndex]));
      FHead.FileOpenDate:=FileDateToDateTime(FileAccessTime(Files.Strings[FileIndex]));
      //
      ///S:=ExtractFileDir(Files.Strings[FileIndex]);
      //Delete(S,Length(S)-1,1);
      //showmessage(s);
      {FHead.DirCreateDate:=FileDateToDateTime(FileCreationTime(S));
      FHead.DirModifyDate:=FileDateToDateTime(FileModifyTime(S));
      FHead.DirAccessDate:=FileDateToDateTime(FileAccessTime(S)); }
      //End of work
      FHead.Attr:=GetFileAttributes(PChar(Files.Strings[FileIndex]));
      FHead.CRC:=C;
      //CompressHead
      PackedHead:=TMemoryStream.Create;
      Head:=TMemoryStream.Create;
      Head.Write(FHead,SizeOf(TFileHead)-4);         
      WriteWideString(Head,fHead.LongFileName);
      {LongNameSize:=Length(FHead.LongFileName);
      Head.Write(LongNameSize,SizeOf(Word));
      aLongName:=FHead.LongFileName;
      Head.Write(aLongName,LongNameSize);}
      //Encode
      //Compressing
      CompressHead(Head,PackedHead);
      PackedHead.Position:=0;
      //FileHeadCRC32:=StreamCRC32(PackedHead);
      PackedHead.Position:=0;
      //End Of Compressing
      If Self.EncodeHead Then
        Begin
          If Pass='' Then
            GetPassword;
          Temp:=TMemoryStream.Create;
          PackedHead.Position:=0;
          EncodeStream(PackedHead,Temp,Pass,fEncodeAlgorithm);
          PackedHead.Free;
          PackedHead:=TMemoryStream.Create;
          Temp.Position:=0;
          If Temp.Size>0 Then
            PackedHead.CopyFrom(Temp,Temp.Size);
          PackedHead.Position:=0;
          Temp.Free;
        End;
      //Encode
      HeadSize:=PackedHead.Size;
      SaveStream.Write(HeadSize,SizeOf(Word));
      SaveStream.CopyFrom(PackedHead,PackedHead.Size);
      //SaveStream.Write(FileHeadCRC32,SizeOf(Cardinal));
      PackedFile.Position:=0;
      SaveStream.CopyFrom(PackedFile,PackedFile.Size);
      //End of compress head
      TargetFile.Free;
      Head.Free;
      PackedHead.Free;
      Inc(GeneralProgress,1);
      application.ProcessMessages;
    End;
  //SaveStream.SaveToFile(FileName);
  {Set attr to archive}
  If ReadingOnly Then SetFileAttributes(PChar(FileName),faReadOnly);
  If Hidden Then SetFileAttributes(PChar(FileName),faHidden);
  If Archive Then SetFileAttributes(PChar(FileName),faArchive);
  // End of setting
  SaveStream.Free;
  GeneralProgress:=0;
  FileProgress:=0;
  DisplayTimer.Enabled:=False;
  CompressorEnd;
  SetPriority(NORMAL_PRIORITY_CLASS,THREAD_PRIORITY_NORMAL);
End;

procedure TMZFCompressor.CompressorBegin;
begin
  If Assigned(fOnBegin) Then
    fOnBegin(Self);
end;

procedure TMZFCompressor.CompressorEnd;
begin
  If Assigned(fOnEnd) Then
    fOnEnd(Self);
end;

procedure TMZFCompressor.Free;
Begin
  If Self<>Nil Then
    Begin
      Files.Free;
      DisplayTimer.Free;
      Destroy;
    End;
End;

destructor TMZFCompressor.Destroy;
Begin
  //
End;

end.
