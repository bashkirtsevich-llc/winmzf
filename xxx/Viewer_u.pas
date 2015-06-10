unit Viewer_u;

interface

uses Classes,ExtCtrls,Advanced,SysUtils,ImageHlp,Windows,Messages_u,dialogs,CRC32;

Type TSowFileInfo=procedure (Sender:TObject;Var FileHead:TFileHead)of object;
Type TPassQuest=procedure (Sender:TObject;Var Pass:ShortString)of object;
Type TError=procedure (Sender:TObject;ErrorMsg:ShortString)of object;

Type TMZFViewer=class (TComponent)
  private
    FileName:String;
    fOnSowFileInfo:TSowFileInfo;
    fOnPassQuest:TPassQuest;
    fOnError:TError;
    fOnBegin:TNotifyEvent;
    fOnEnd:TNotifyEvent;
    Password:ShortString;
    Comment:WideString;
    FileList:TStringList;
    Directories:TStringList;
    FilterPath:String;
    procedure GetPassword;
    procedure Error(Msg:ShortString);
    procedure ViewBegin;
    procedure ViewEnd;
    procedure ShowFileInformation(Var FileHead:TFileHead);
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
    property Commentary:WideString read Comment;
    property DirList:TStringList read Directories;
    property Path:String read FilterPath write FilterPath;
    procedure ShowFiles;
    procedure DeleteFiles(DeleteFileList:TStringList);
    procedure RenameFile(OldName,NewName:String);
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

procedure TMZFViewer.ShowFiles;
var ArchiveStream:TMemoryStream;
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
        If (AConfig.EncodeHead) And ((AConfig.EncodeMode=emAfterCompress)Or
                                    ((AConfig.EncodeMode=emBeforeAfter))) Then
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
        If (AConfig.EncodeHead) And ((AConfig.EncodeMode=emBeforeCompress)Or
                                    ((AConfig.EncodeMode=emBeforeAfter))) Then
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
        /////////////////////////////////////

        HeadStream.Position:=0;
        HeadStream.Read(FileHead,SizeOf(TFileHead)-4);
        FileHead.LongFileName:='';
        FileHead.LongFileName:=ReadWideString(HeadStream);

        FileHead.LongFileName:=Format(RootCaption,[ExtractFileName(FileName)])+FileHead.LongFileName;
        //End create root
        If Not TextInList(ExtractFileDir(FileHead.LongFileName),Directories) Then
          Directories.Add(ExtractFileDir(FileHead.LongFileName));
        FileList.Add(FileHead.LongFileName);
      {If Not TextInList(ExtractFileDir(FileHead.LongFileName),Directories) Then
        Directories.Add(ExtractFileDir(FileHead.LongFileName));}
        If ExtractFileDir(FileHead.LongFileName)=FilterPath Then
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
        If (AConfig.EncodeHead) And ((AConfig.EncodeMode=emAfterCompress)Or
                                    ((AConfig.EncodeMode=emBeforeAfter))) Then
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
        If (AConfig.EncodeHead) And ((AConfig.EncodeMode=emBeforeCompress)Or
                                    ((AConfig.EncodeMode=emBeforeAfter))) Then
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
        /////////////////////////////////////
        HeadStream.Position:=0;
        HeadStream.Read(FileHead,SizeOf(TFileHead)-4);
        FileHead.LongFileName:='';
        FileHead.LongFileName:=ReadWideString(HeadStream);

        ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent);
        ContPos:=ArchiveStream.Position;
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
      If AConfig.EncodeHead And ((AConfig.EncodeMode=emBeforeCompress)Or
                                 (AConfig.EncodeMode=emBeforeAfter)) Then
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
      If AConfig.EncodeHead And ((AConfig.EncodeMode=emAfterCompress)Or
                                 (AConfig.EncodeMode=emBeforeAfter)) Then
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
          If AConfig.EncodeHead And ((AConfig.EncodeMode=emBeforeCompress)Or
                                 (AConfig.EncodeMode=emBeforeAfter)) Then
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
          Temp:=TMemoryStream.Create;
          Temp.CopyFrom(HeadStream,HeadStream.Size);
          Temp.Position:=0;
          HeadStream.Free;
          HeadStream:=TMemoryStream.Create;
          CompressStream(Temp,HeadStream);
          HeadStream.Position:=0;
          ////
          If AConfig.EncodeHead And ((AConfig.EncodeMode=emAfterCompress)Or
                                 (AConfig.EncodeMode=emBeforeAfter)) Then
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
        fOnSowFileInfo(Self,FileHead);
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
 