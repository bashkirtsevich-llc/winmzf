unit Viewer_u;

interface

uses Classes,ExtCtrls,Advanced,SysUtils,ImageHlp,Windows,Messages_u,dialogs,CRC32,
     ULZMADecoder, Forms;

Type TViewerProgress=procedure (Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean) of object;
Type TSowFileInfo=procedure (Sender:TObject;Var FileHead:TFileHead; Id:LongWord)of object;
Type TPassQuest=procedure (Sender:TObject;Var Pass:ShortString)of object;
Type TError=procedure (Sender:TObject;ErrorMsg:ShortString)of object;
Type TCheckStatus=procedure (Sender:TObject;FileName:String; Failed:Boolean)of object;

Type TMZFViewer=class (TComponent)
  private
    FileName:String;
    fOnSowFileInfo:TSowFileInfo;
    fOnPassQuest:TPassQuest;
    fOnError:TError;
    fOnBegin:TNotifyEvent;
    fOnEnd:TNotifyEvent;
    fOnCheck:TCheckStatus;
    fOnProgress:TViewerProgress;
    fShowAll:Boolean;
    Password:ShortString;
    Comment:WideString;
    FileList:TStringList;
    Directories:TStringList;
    FilterPath:String;
    Abort:Boolean;
    procedure GetPassword;
    procedure Error(Msg:ShortString);
    procedure ViewBegin;
    procedure ViewEnd;
    procedure UploadStatus(FName:String;_Failed:Boolean);
    procedure ShowFileInformation(Var FileHead:TFileHead);
    function ExpandFile(InFile,OutFile:TStream):Boolean;
  public
    constructor Create;
    procedure Free;
    destructor Destroy;
    property ArchiveName:String read FileName write FileName;
    property Files:TStringList read FileList;
    property OnShowFileINfo:TSowFileInfo read fOnSowFileInfo write fOnSowFileInfo;
    property OnPassQuest:TPassQuest read fOnPassQuest write fOnPassQuest;
    property OnError:TError read fOnError write fOnError;
    property OnBegin:TNotifyEvent read fOnBegin write fOnBegin;
    property OnEnd:TNotifyEvent read fOnEnd write fOnEnd;
    property OnCheck:TCheckStatus read fOnCheck write fOnCheck;
    property Commentary:WideString read Comment;
    property DirList:TStringList read Directories;
    property Path:String read FilterPath write FilterPath;
    property ShowAll:Boolean read fShowAll write fShowAll default false;
    property OnProgress:TViewerProgress read fOnProgress write fOnProgress;
    procedure ShowFiles;
    procedure DeleteFiles(DeleteFileList:TStringList);
    procedure RenameFile(OldName,NewName:String);
    procedure CheckArchive;
    function GetFileCount(Var AComment:String;Var UnpackedSize:Int64;Var Version:String;Var UnPacked:Int64):LongWord;
  protected
  end;

  procedure register;
  //LZMA_StreamCoderDecoder_u

implementation

procedure register;
begin
  RegisterComponents('M.A.D.M.A.N.', [TMZFViewer]);
end;

constructor TMZFViewer.Create;
Begin
  FileList:=TStringList.Create;
  Directories:=TStringList.Create;
  Password:='';
End;

procedure TMZFViewer.ViewBegin;
begin
  If Assigned(fOnBegin) Then
    fOnBegin(Self);
end;

function TMZFViewer.ExpandFile(InFile,OutFile:TStream):Boolean;
var properties:array[0..4] of byte;
    decoder:TLZMADecoder;
    OutSize:LongWord;
    i,v:Byte;
begin
  InFile.Seek(0,soFromBeginning);
  OutFile.Seek(0,soFromBeginning);

  InFile.Read(Properties,5);

  decoder:=TLZMADecoder.Create;
  If Not decoder.SetDecoderProperties(properties) Then
    Begin
      Result:=False;
      Decoder.Free;
      Exit;
    End;

  outSize := 0;
  {for i := 0 to 7 do
    begin
      v := (ReadByte(InFile));
      if v < 0 then
        raise Exception.Create('Can''t read stream size');
      outSize := outSize or v shl (8 * i);
    end; }
  InFile.Read(outSize,SizeOf(LongWord));
  decoder.Code(InFile, OutFile, OutSize);
  decoder.Free;

  InFile.Position:=0;
  OutFile.Position:=0;
  Result:=True;
End;

function TMZFViewer.GetFileCount(Var AComment:String;Var UnpackedSize:Int64;Var Version:String;Var UnPacked:Int64):LongWord;
var ArchiveStream:TFileStream;
    AConfig:TArcConfig;
    ArcHead:TArcHead;
    FileHead:TFileHead;
    HeadSize:Word;
    HeadStream:TStream;
    Temp,CommentStream:TStream;
    CommentSize:LongWord;
    LongNameSize:Word;
    aLongFileName:String;
begin
  UnpackedSize:=0;
  Result:=0;
  AComment:='';
  If FileName='' Then Exit;
  If Not FileExists(FileName) Then
    Begin
      Error(rsNoFile);
      Exit;
    End;
  ViewBegin;
  Comment:='';
  LongNameSize:=$0;
  Directories.Clear;
  ArchiveStream:=TFileStream.Create(FileName,fmOpenRead);
  UnPacked:=ArchiveStream.Size;
  //ArchiveStream.LoadFromFile(FileName);
  ArchiveStream.Position:=0;
  ArchiveStream.Read(ArcHead,SizeOf(TArcHead));
  Version:=ArcHead.Version;
  ArchiveStream.Read(AConfig,SizeOf(TArcConfig));
  //Check head
  If Not CheckHead(ArcHead) Then
    Begin
      Error(rsCorrupt);
      ArchiveStream.Free;
      ViewEnd;
      Exit;
    End;
  //Get comment
  IF AConfig.UseComment Then
    Begin
      Try
      ArchiveStream.Read(CommentSize,SizeOf(LongWord));
      CommentStream:=TMemoryStream.Create;
      CommentStream.CopyFrom(ArchiveStream,CommentSize);
      Temp:=TMemoryStream.Create;
      If Not ExpandStream(CommentStream,Temp) Then
        Begin
          Error(rsCorrupt);
          ArchiveStream.Free;
          Temp.Free;
          CommentStream.Free;
          ViewEnd;
          Exit;
        End;
      CommentStream:=TMemoryStream.Create;
      Temp.Position:=0;
      CommentStream.CopyFrom(Temp,Temp.Size);
      CommentStream.Position:=0;
      Temp.Free;
      //CommentStream.Read(Comment,CommentStream.Size);
      Comment:=ReadWideString(CommentStream);
      CommentStream.Free;
      AComment:=Comment;
      Except
        //Exception.Create('Error');
        Error(rsCorrupt);
        ArchiveStream.Free;
        //CommentStream.Free;
        Exit;
      End;
    End;
    While ArchiveStream.Position<>ArchiveStream.Size Do
    Begin
      Try
        ArchiveStream.Read(HeadSize,SizeOf(Word));
        HeadStream:=TMemoryStream.Create;
        HeadStream.CopyFrom(ArchiveStream,HeadSize);
        HeadStream.Position:=0;
        If (AConfig.EncodeHead) Then
          Begin
            If Password='' Then
              GetPassword;
            If Password='' Then
              Begin
                HeadStream.Free;
                ArchiveStream.Free;
                ViewEnd;
                Exit;
              End;
            Temp:=TMemoryStream.Create;
            HeadStream.Position:=0;
            DecodeStream(HeadStream,Temp,Password,AConfig.EncodeAlgorithm);
            Temp.Position:=0;
            HeadStream:=TMemoryStream.Create;
            HeadStream.CopyFrom(Temp,Temp.Size);
            Temp.Free;
          End;
        Temp:=TMemoryStream.Create;
        HeadStream.Position:=0;
        If Not ExpandStream(HeadStream,Temp) Then
          Begin
            Error(rsInvalidPass);
            ArchiveStream.Free;
            Temp.Free;
            HeadStream.Free;
            ViewEnd;
            Exit;
          End;
        /////////////////////////////////////
        HeadStream:=TMemoryStream.Create;
        Temp.Position:=0;
        HeadStream.CopyFrom(Temp,Temp.Size);
        Temp.Free;
        HeadStream.Position:=0;
        HeadStream.Read(FileHead,SizeOf(TFileHead)-4);
        Inc(UnpackedSize,FileHead.Size);
        ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent);
        Inc(Result);
      Except
        Error(rsInvalidPass);
        ArchiveStream.Free;
        HeadStream.Free;
        Exit;
      End;

    End;
  ArchiveStream.Free;
  ViewEnd;

end;

procedure TMZFViewer.ShowFiles;
var ArchiveStream:TFileStream;
    AConfig:TArcConfig;
    ArcHead:TArcHead;
    FileHead:TFileHead;
    HeadSize:Word;
    HeadStream:TStream;
    Temp,CommentStream:TStream;
    CommentSize:LongWord;
    LongNameSize:Word;
    aLongFileName:String;
    //FileHeadCRC32:Cardinal;
Begin
  If FileName='' Then Exit;
  If Not FileExists(FileName) Then
    Begin
      Error(rsNoFile);
      Exit;
    End;
  ViewBegin;
  Comment:='';
  LongNameSize:=$0;
  Directories.Clear;
  ArchiveStream:=TFileStream.Create(FileName,fmOpenRead);
  //ArchiveStream.LoadFromFile(FileName);
  ArchiveStream.Position:=0;
  ArchiveStream.Read(ArcHead,SizeOf(TArcHead));
  ArchiveStream.Read(AConfig,SizeOf(TArcConfig));
  //Check head
  If Not CheckHead(ArcHead) Then
    Begin
      Error(rsCorrupt);
      ArchiveStream.Free;
      ViewEnd;
      Exit;
    End;
  //Get comment
  IF AConfig.UseComment Then
    Begin
      Try
      ArchiveStream.Read(CommentSize,SizeOf(LongWord));
      CommentStream:=TMemoryStream.Create;
      CommentStream.CopyFrom(ArchiveStream,CommentSize);
      Temp:=TMemoryStream.Create;
      If Not ExpandStream(CommentStream,Temp) Then
        Begin
          Error(rsCorrupt);
          ArchiveStream.Free;
          Temp.Free;
          CommentStream.Free;
          ViewEnd;
          Exit;
        End;
      CommentStream:=TMemoryStream.Create;
      Temp.Position:=0;
      CommentStream.CopyFrom(Temp,Temp.Size);
      CommentStream.Position:=0;
      Temp.Free;
      //CommentStream.Read(Comment,CommentStream.Size);
      Comment:=ReadWideString(CommentStream);
      CommentStream.Free;
      Except
        //Exception.Create('Error');
        Error(rsCorrupt);
        ArchiveStream.Free;
        //CommentStream.Free;
        Exit;
      End;
    End;
  While ArchiveStream.Position<>ArchiveStream.Size Do
    Begin
      Try
      
        ArchiveStream.Read(HeadSize,SizeOf(Word));
        HeadStream:=TMemoryStream.Create;
        HeadStream.CopyFrom(ArchiveStream,HeadSize);
        //ArchiveStream.Read(FileHeadCRC32,SizeOf(Cardinal));
        HeadStream.Position:=0;
        //Detect encode mode
        // If Encode And EncodeMode=After
        /////////////////////////////////////
        If (AConfig.EncodeHead) Then
          Begin
            If Password='' Then
              GetPassword;
            If Password='' Then
              Begin
                HeadStream.Free;
                ArchiveStream.Free;
                ViewEnd;
                Exit;
              End;
            Temp:=TMemoryStream.Create;
            HeadStream.Position:=0;
            DecodeStream(HeadStream,Temp,Password,AConfig.EncodeAlgorithm);
            Temp.Position:=0;
            HeadStream:=TMemoryStream.Create;
            HeadStream.CopyFrom(Temp,Temp.Size);
            Temp.Free;
          End;
        //Extract Head Stream to Temp stream
        Temp:=TMemoryStream.Create;
        /////////////////////////////////////
        HeadStream.Position:=0;
      {If StreamCRC32(HeadStream)<>FileHeadCRC32 Then
        Begin
          Error(rsInvalidPass);
          ArchiveStream.Free;
          HeadStream.Free;
          Temp.Free;
          Exit;
        End;
      HeadStream.Position:=0;  }
        If Not ExpandStream(HeadStream,Temp) Then
          Begin
            Error(rsInvalidPass);
            ArchiveStream.Free;
            Temp.Free;
            HeadStream.Free;
            ViewEnd;
            Exit;
          End;
        /////////////////////////////////////
        HeadStream:=TMemoryStream.Create;
        Temp.Position:=0;
        HeadStream.CopyFrom(Temp,Temp.Size);
        Temp.Free;
        /////////////////////////////////////
        /////////////////////////////////////

        HeadStream.Position:=0;
        HeadStream.Read(FileHead,SizeOf(TFileHead)-4);
        FileHead.LongFileName:='';
        FileHead.LongFileName:=ReadWideString(HeadStream);

        FileHead.LongFileName:=Format(RootCaption,[ExtractFileName(FileName)])+FileHead.LongFileName;
        //End create root
        //If Not TextInList(ExtractFileDir(FileHead.LongFileName),Directories) Then
          Directories.Add(ExtractFileDir(FileHead.LongFileName));
        FileList.Add(FileHead.LongFileName);
      {If Not TextInList(ExtractFileDir(FileHead.LongFileName),Directories) Then
        Directories.Add(ExtractFileDir(FileHead.LongFileName));}
        If (ExtractFileDir(FileHead.LongFileName)=FilterPath) Or fShowAll Then
          ShowFileInformation(FileHead);
        ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent);
      
      Except
        //Exception.Create('Error');
        Error(rsInvalidPass);
        ArchiveStream.Free;
        HeadStream.Free;
        Exit;
      End;

    End;
  ArchiveStream.Free;
  ViewEnd;
End;

procedure TMZFViewer.DeleteFiles(DeleteFileList:TStringList);
var ArchiveStream:TMemoryStream;
    AConfig:TArcConfig;
    ArcHead:TArcHead;
    FileHead:TFileHead;
    HeadSize:Word;
    HeadStream:TStream;
    Temp,CommentStream:TStream;
    CommentSize:LongWord;
    SaveStream:TMemoryStream;
    MemPos,ContPos:LongWord;
    Attr:Word;
    LongNameSize:Word;
    aLongFileName:String;
    //FileHeadCRC32:Cardinal;
Begin
  If FileName='' Then Exit;
  If Not FileExists(FileName) Then
    Begin
      Error(rsNoFile);
      Exit;
    End;
  ViewBegin;
  LongNameSize:=$0;
  SaveStream:=TMemoryStream.Create;
  ArchiveStream:=TMemoryStream.Create;
  ArchiveStream.LoadFromFile(FileName);
  ArchiveStream.Position:=0;
  ArchiveStream.Read(ArcHead,SizeOf(TArcHead));
  ArchiveStream.Read(AConfig,SizeOf(TArcConfig));
  //Check head
  If Not CheckHead(ArcHead) Then
    Begin
      Error(rsCorrupt);
      ArchiveStream.Free;
      ViewEnd;
      Exit;
    End;
  //Get comment
  If AConfig.UseComment Then
    Begin
      ArchiveStream.Read(CommentSize,SizeOf(LongWord));
      ArchiveStream.Seek(CommentSize,soFromCurrent);
    End;        

  MemPos:=ArchiveStream.Position;
  ArchiveStream.Position:=0;
  SaveStream.CopyFrom(ArchiveStream,MemPos);
  While ArchiveStream.Position<>ArchiveStream.Size Do
    Begin
      Try
        MemPos:=ArchiveStream.Position;
        ArchiveStream.Read(HeadSize,SizeOf(Word));
        HeadStream:=TMemoryStream.Create;
        HeadStream.CopyFrom(ArchiveStream,HeadSize);
        //ArchiveStream.Read(FileHeadCRC32,SizeOf(Cardinal));
        HeadStream.Position:=0;
        //Detect encode mode
        // If Encode And EncodeMode=After
        /////////////////////////////////////
        If (AConfig.EncodeHead)Then
          Begin
            If Password='' Then
              GetPassword;
            If Password='' Then
              Begin
                HeadStream.Free;
                ArchiveStream.Free;
                ViewEnd;
                Exit;
              End;
            Temp:=TMemoryStream.Create;
            HeadStream.Position:=0;
            DecodeStream(HeadStream,Temp,Password,AConfig.EncodeAlgorithm);
            Temp.Position:=0;
            HeadStream:=TMemoryStream.Create;
            HeadStream.CopyFrom(Temp,Temp.Size);
            Temp.Free;
          End;
        //Extract Head Stream to Temp stream
        Temp:=TMemoryStream.Create;
        /////////////////////////////////////
        HeadStream.Position:=0;
      {If StreamCRC32(HeadStream)<>FileHeadCRC32 Then
        Begin
          Error(rsInvalidPass);
          ArchiveStream.Free;
          HeadStream.Free;
          Temp.Free;
          Exit;
        End;
      HeadStream.Position:=0; }

        If Not ExpandStream(HeadStream,Temp) Then
          Begin
            ArchiveStream.Free;
            Temp.Free;
            HeadStream.Free;
            Exit;
          End;
        /////////////////////////////////////
        HeadStream:=TMemoryStream.Create;
        Temp.Position:=0;
        HeadStream.CopyFrom(Temp,Temp.Size);
        Temp.Free;
        /////////////////////////////////////
        /////////////////////////////////////
        HeadStream.Position:=0;
        HeadStream.Read(FileHead,SizeOf(TFileHead)-4);
        FileHead.LongFileName:='';
        FileHead.LongFileName:=ReadWideString(HeadStream);

        ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent);
        ContPos:=ArchiveStream.Position;
        //showmessage(filehead.LongFileName);
        If Not TextInList(FileHead.LongFileName,DeleteFileList) Then
          Begin
            ArchiveStream.Position:=MemPos;
            SaveStream.CopyFrom(ArchiveStream,ContPos-MemPos);
            ArchiveStream.Position:=ContPos;
          End;
      Except
        //Exception.Create('Error');
        Error(rsCorrupt);
        ArchiveStream.Free;
        SaveStream.Free;
        Exit;
      End;
      End;
  Attr:=FileGetAttr(FileName);
  ArchiveStream.Clear;
  ArchiveStream.SaveToFile(FileName);
  ArchiveStream.Free;
  FileSetAttr(FileName,0);
  SaveStream.SaveToFile(FileName);
  SaveStream.Free;
  FileSetAttr(FileName,Attr);
  ViewEnd;
End;

procedure TMZFViewer.RenameFile(OldName,NewName:String);
Var ArchiveStream:TMemoryStream;
    SaveStream:TMemoryStream;
    Temp,HeadStream:TStream;
    FileContainer:TStream;
    ArcHead:TArcHead;
    AConfig:TArcConfig;
    FileHead:TFileHead;
    MemPos,ContPos:LongWord;
    HeadSize:Word;
    CommentSize:LongWord;
    Attr:Word;
    //FileHeadCRC32:Cardinal;    
Begin
  If FileName='' Then Exit;
  If Not FileExists(FileName) Then
    Begin
      Error(rsNoFile);
      Exit;
    End;
  ViewBegin;
  ArchiveStream:=TMemoryStream.Create;
  ArchiveStream.LoadFromFile(FileName);
  ArchiveStream.Read(ArcHead,SizeOf(TArcHead));
  ArchiveStream.Read(AConfig,SizeOf(TArcConfig));
  If Not CheckHead(ArcHead) Then
    Begin
      Error(rsCorrupt);
      ArchiveStream.Free;
      ViewEnd;
      Exit;
    End;
  //Get comment
  If AConfig.UseComment Then
    Begin
      ArchiveStream.Read(CommentSize,SizeOf(LongWord));
      ArchiveStream.Seek(CommentSize,soFromCurrent);
    End;
  SaveStream:=TMemoryStream.Create;
  MemPos:=ArchiveStream.Position;
  ArchiveStream.Position:=0;
  SaveStream.CopyFrom(ArchiveStream,MemPos);
  ArchiveStream.Position:=MemPos;
  While ArchiveStream.Position<>ArchiveStream.Size Do
    Begin
    Try
      MemPos:=ArchiveStream.Position;
      ArchiveStream.Read(HeadSize,SizeOf(Word));
      HeadStream:=TMemoryStream.Create;
      HeadStream.CopyFrom(ArchiveStream,HeadSize);
      //ArchiveStream.Read(FileHeadCRC32,SizeOf(Cardinal));
      HeadStream.Position:=0;
      //////////////////////
        //expand
      Temp:=TMemoryStream.Create;
      /////////////////////////////////////
      HeadStream.Position:=0;
      {If StreamCRC32(HeadStream)<>FileHeadCRC32 Then
        Begin
          Error(rsInvalidPass);
          ArchiveStream.Free;
          HeadStream.Free;
          Temp.Free;
          Exit;
        End;
      HeadStream.Position:=0;}

      If Not ExpandStream(HeadStream,Temp) Then
        Begin
          ArchiveStream.Free;
          Temp.Free;
          HeadStream.Free;
          Exit;
        End;
      HeadStream.Free;
      HeadStream:=TMemoryStream.Create;
      Temp.Position:=0;
      HeadStream.CopyFrom(Temp,Temp.Size);
      HeadStream.Position:=0;
        //End expand;
      If AConfig.EncodeHead Then
        Begin
        If Password='' Then
            GetPassword;
          If Password='' Then
            Begin
              HeadStream.Free;
              ArchiveStream.Free;
              ViewEnd;
              Exit;
            End;
          Temp:=TMemoryStream.Create;
          Temp.CopyFrom(HeadStream,HeadStream.Size);
          HeadStream:=TMemoryStream.Create;
          DecodeStream(Temp,HeadStream,Password,AConfig.EncodeAlgorithm);
          HeadStream.Position:=0;
          Temp.Free;
        End;
      //////////////////////
      HeadStream.Read(FileHead,SizeOf(TFileHead)-4);
      FileHead.LongFileName:='';
      FileHead.LongFileName:=ReadWideString(HeadStream);

      If UpperCase(OldName)=UpperCase(FileHead.LongFileName) Then
        Begin
          FileContainer:=TMemoryStream.Create;
          FIleContainer.CopyFrom(ArchiveStream,FileHead.PackedSize);
          FileContainer.Position:=0;
          FileHead.LongFileName:='';
          FileHead.LongFileName:=NewName;
          HeadStream:=TMemoryStream.Create;
          HeadStream.Write(FileHead,SizeOf(TFileHead)-4);
          WriteWideString(HeadStream,FileHead.LongFileName);
          HeadStream.Position:=0;
          Temp:=TMemoryStream.Create;
          Temp.CopyFrom(HeadStream,HeadStream.Size);
          Temp.Position:=0;
          HeadStream.Free;
          HeadStream:=TMemoryStream.Create;
          CompressStream(Temp,HeadStream);
          HeadStream.Position:=0;
          ////
          If AConfig.EncodeHead Then
            Begin
              If Password='' Then
                GetPassword;
              If Password='' Then
                Begin
                  HeadStream.Free;
                  ArchiveStream.Free;
                  ViewEnd;
                  Exit;
                End;
              Temp:=TMemoryStream.Create;
              Temp.CopyFrom(HeadStream,HeadStream.Size);
              Temp.Position:=0;
              HeadStream:=TMemoryStream.Create;
              EncodeStream(Temp,HeadStream,Password,AConfig.EncodeAlgorithm);
              HeadStream.Position:=0;
            End;

        HeadSize:=HeadStream.Size;
        SaveStream.Write(HeadSize,SizeOf(Word));
        SaveStream.CopyFrom(HeadStream,HeadSize);
        SaveStream.CopyFrom(FileContainer,FileContainer.Size);
          
        End Else
        Begin
          ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent);
          ContPos:=ArchiveStream.Position;
          ArchiveStream.Position:=MemPos;
          SaveStream.CopyFrom(ArchiveStream,ContPos-MemPos);
          ArchiveStream.Position:=ContPos;
        End;
      Except
        //Exception.Create('Error');
        Error(rsCorrupt);
        ArchiveStream.Free;
        SaveStream.Free;
        Exit;
      End;
    End;
  Attr:=FileGetAttr(FileName);
  ArchiveStream.Clear;
  ArchiveStream.SaveToFile(FileName);
  ArchiveStream.Free;
  FileSetAttr(FileName,0);
  SaveStream.SaveToFile(FileName);
  SaveStream.Free;
  FileSetAttr(FileName,Attr);
  ViewEnd;
End;

procedure TMZFViewer.CheckArchive;
Var ArchiveStream:TFileStream;
    EtalonFile:TMemoryStream;
    ArcHead:TArcHead;
    AConfig:TArcConfig;
    FileHead:TFileHead;
    HeadSize:Word;
    HeadStream:TStream;
    Temp:TStream;
    CommentSize:LongWord;
    TempFileName:String;
    LongNameSize:Word;
    //FileHeadCRC32:Cardinal;
Begin
  If FileName='' Then
    Begin
      UploadStatus(FileName,True);
      Exit;
    End;
  If Not FileExists(FileName) Then
    Begin
      UploadStatus(FileName,True);
      Error(rsNoFile);
      Exit;
    End;
  ArchiveStream:=TFileStream.Create(FileName,fmOpenRead);
  //ArchiveStream.LoadFromFile(FileName);
  ArchiveStream.Position:=0;
  ArchiveStream.Read(ArcHead,SizeOf(TArcHead));
  ArchiveStream.Read(AConfig,SizeOf(TArcConfig));
  //Check head
  If Not CheckHead(ArcHead) Then
    Begin
      Error(rsCorrupt);
      ArchiveStream.Free;
      Exit;
    End;
  //Get comment
  If AConfig.UseComment Then
    Begin
      ArchiveStream.Read(CommentSize,SizeOf(LongWord));
      ArchiveStream.Seek(CommentSize,soFromCurrent);
    End;
  While ArchiveStream.Position<>ArchiveStream.Size Do
    Begin
      If Abort Then
        Begin
          UploadStatus(FileName,True);
          Exit;
        End;
      Try
      ArchiveStream.Read(HeadSize,SizeOf(Word));
      HeadStream:=TMemoryStream.Create;
      HeadStream.CopyFrom(ArchiveStream,HeadSize);
      //ArchiveStream.Read(FileHeadCRC32,SizeOf(Cardinal));
      HeadStream.Position:=0;
      //Detect encode mode
      // If Encode And EncodeMode=After
      /////////////////////////////////////      
      If (AConfig.EncodeHead) Then
        Begin
          If Password='' Then
          GetPassword;
          If Password='' Then
            Begin
              UploadStatus(FileName,True);
              HeadStream.Free;
              ArchiveStream.Free;
              Exit;
            End;
          Temp:=TMemoryStream.Create;
          HeadStream.Position:=0;
          DecodeStream(HeadStream,Temp,Password,AConfig.EncodeAlgorithm);
          Temp.Position:=0;
          HeadStream:=TMemoryStream.Create;
          HeadStream.CopyFrom(Temp,Temp.Size);
          Temp.Free;
        End;
      //Extract Head Stream to Temp stream
      Temp:=TMemoryStream.Create;
      /////////////////////////////////////
      HeadStream.Position:=0;
      {If StreamCRC32(HeadStream)<>FileHeadCRC32 Then
        Begin
          Error(rsInvalidPass);
          ArchiveStream.Free;
          HeadStream.Free;
          Temp.Free;
          Exit;
        End;
      HeadStream.Position:=0;}
//Decode
      If Not ExpandStream(HeadStream,Temp) Then
        Begin
          UploadStatus(FileName,True);
          Error(rsInvalidPass);
          ArchiveStream.Free;
          Temp.Free;
          HeadStream.Free;
          Exit;
        End;
//Decode
      /////////////////////////////////////
      HeadStream:=TMemoryStream.Create;
      Temp.Position:=0;
      HeadStream.CopyFrom(Temp,Temp.Size);
      Temp.Free;
      /////////////////////////////////////
      /////////////////////////////////////
        HeadStream.Position:=0;
        HeadStream.Read(FileHead,SizeOf(TFileHead)-4);
        FileHead.LongFileName:=ReadWideString(HeadStream);
        {HeadStream.Read(LongNameSize,SizeOf(Word));
        HeadStream.Read(FileHead.LongFileName,LongNameSize); }
        //Create root dir
        TempFileName:=FileHead.LongFileName;
        //Delete(TempFileName,1,1);
        //Work on file
        //Copy file in stream
      EtalonFile:=TMemoryStream.Create;
      If FileHead.PackedSize>0 Then
        EtalonFile.CopyFrom(ArchiveStream,FileHead.PackedSize); 
      EtalonFile.Position:=0;
      //error(tempfilename);
            //If encode mode
            If (AConfig.EncodeFile) Then
              Begin
                If Password='' Then
                  GetPassword;
                If Password='' Then
                  Begin
                    HeadStream.Free;
                    ArchiveStream.Free;
                    EtalonFile.Free;
                    Exit;
                  End;
                Temp:=TMemoryStream.Create;
                DecodeStream(EtalonFile,Temp,Password,AConfig.EncodeAlgorithm);
                Temp.Position:=0;
                EtalonFile:=TMemoryStream.Create;
                EtalonFile.CopyFrom(Temp,Temp.Size);
                EtalonFile.Position:=0;
                Temp.Free;
              End;
            //Try to extract
            Temp:=TMemoryStream.Create;
            If FileHead.PackedSize>0 Then
            If (Abort) Or (Not ExpandFile(EtalonFile,Temp)) Then
              Begin
                UploadStatus(FileHead.LongFileName,True);
                Error(rsInvalidFile);
                EtalonFile.Free;
                Temp.Free;
                ArchiveStream.Free;
                Exit;
              End;
            EtalonFile:=TMemoryStream.Create;
            EtalonFile.CopyFrom(Temp,Temp.Size);
            Temp.Free;
            EtalonFile.Position:=0;
            //Check CRC
            //Error(inttostr(filehead.CRC));
            If StreamCRC32(EtalonFile)<>FileHead.CRC Then
              Begin
                UploadStatus(FileHead.LongFileName,True);
                Error(rsBadCRC);
                EtalonFile.Free;
                ArchiveStream.Free;
                Exit;
              End;
            UploadStatus(FileHead.LongFileName,False);
            // Find file
          { Else
          ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent); }
      EtalonFile.Free;//Free etalon file
      if Assigned(fOnProgress) then
        fOnProgress(Self,0,GetPercentDone(0,ArchiveStream.Position,ArchiveStream.Size),Abort);
      Application.ProcessMessages;
      Except
        UploadStatus(FileName,True);
        Error(rsInvalidPass);
        ArchiveStream.Free;
        Exit;
      End;
    End;
  ArchiveStream.Free;
end;

procedure TMZFViewer.UploadStatus(FName:String;_Failed: Boolean);
begin
  if Assigned(fOnCheck) then
    fOnCheck(Self,FName,_Failed);
end;

procedure TMZFViewer.ViewEnd;
begin
  If Assigned(fOnEnd) Then
    fOnEnd(Self);
end;

procedure TMZFViewer.GetPassword;
begin
  If Assigned(fOnPassQuest) Then
    fOnPassQuest(Self,Password);
end;

procedure TMZFViewer.Error(Msg:ShortString);
begin
  If Assigned(fOnError) Then
    fOnError(Self,Msg);
end;

procedure TMZFViewer.ShowFileInformation(Var FileHead:TFileHead);
begin
  If Assigned(fOnSowFileInfo) Then
        fOnSowFileInfo(Self,FileHead,0);
end;

procedure TMZFViewer.Free;
begin
  If Self<>Nil Then
    Begin
      FileList.Free;
      Directories.Free;
      Destroy;
    End;
end;

destructor TMZFViewer.Destroy;
Begin

End;

end.
