unit MZF_decompress;

interface

uses Classes,ExtCtrls,Advanced,CRC32,SysUtils,Messages_u,Forms,Windows,
             ULZMADecoder,ULZMACommon;

Type TDecompressorProgress=procedure (Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean) of object;
Type TPassQuest=procedure (Sender:TObject;Var Pass:ShortString)of object;
Type TOverWritePrompt=procedure (Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode)of object;
Type TError=procedure (Sender:TObject;ErrorMsg:ShortString)of object;
Type TOnExtract=procedure (Sender:TObject;_FileName:ShortString)of object;

Type TMZFDecompressor=class (TComponent)
  Private
    FileName:String;
    ExtractList:TStringList;
    ExtractAllFiles:Boolean;
    ProgressTimer:TTimer;
    Dir:String;
    fOnBegin:TNotifyEvent;
    fOnEnd:TNotifyEvent;
    fOnProgress:TDecompressorProgress;
    fOnGetPass:TPassQuest;
    fOnOverwritePrompt:TOverWritePrompt;
    fOnError:TError;
    fOnExtract:TOnExtract;
    fSecurityMode:Boolean;
    fExcludeSpec:String;
    FileProgress,ArchiveProgress:LongWord;
    ArchiveSize,FileSize:LongWord;
    Abort:Boolean;
    Pass:ShortString;
    OverwriteMode:TReWriteMode;
    UpdateModifycationTime:Boolean;
    UpdateLastAccess:Boolean;
    CRCIgnore:Boolean;
    DelAfterExtract:Boolean;
    procedure BeginEvent;
    procedure EndEvent;
    procedure OnTimer(Sender:TObject);
    procedure GetPassword;
    procedure GetOverwriteMode(Var FileHead:TFileHead;Var Mode:TReWriteMode);
    procedure Error(Msg:ShortString);
    procedure EndExtract;
    procedure BeginExtract;
    function ExpandFile(InFile,OutFile:TStream):Boolean;
    procedure OnLZMAProgress(const Action:TLZMAProgressAction;const Value:int64);
  Public
    constructor Create;
    property ArchiveFile:String read FileName write FileName;
    property DestinationDir:String read Dir write Dir;
    property OnBegin:TNotifyEvent read fOnBegin write fOnBegin;
    property OnEnd:TNotifyEvent read fOnEnd write fOnEnd;
    property OnProgress:TDecompressorProgress read fOnProgress write fOnProgress;
    property OnGetPass:TPassQuest read fOnGetPass write fOnGetPass;
    property Password:ShortString read Pass write Pass;
    property OnOverwritePrompt:TOverWritePrompt read fOnOverwritePrompt write fOnOverwritePrompt;
    property OnError:TError read fOnError write fOnError;
    property FileList:TStringList read ExtractList write ExtractList;
    property UpdateModifyTime:Boolean read UpdateModifycationTime write UpdateModifycationTime default False;
    property UpdateLastAccesstime:Boolean read UpdateLastAccess write UpdateLastAccess default False;
    property IgnoreCRCCheck:Boolean read CRCIgnore write CRCIgnore;
    property ExtractAll:Boolean read ExtractAllFiles write ExtractAllFiles;
    property FileOverwriteMode:TReWriteMode read OverwriteMode write OverwriteMode;
    property DeleteAfterExtract:Boolean read DelAfterExtract write DelAfterExtract default False;
    property ExcludeSpec:String read fExcludeSpec write fExcludeSpec;
    property EnableSecurityMode:Boolean read fSecurityMode write fSecurityMode;
    property OnExtract:TOnExtract read fOnExtract write fOnExtract;
    procedure Extract;
    procedure ExtractFromStream(ArchiveStream:TStream);
    procedure Free;
    destructor Destroy;
  Protected
  End;

  procedure register;
  //LZMA_StreamCoderDecoder_u

implementation

procedure register;
begin
  RegisterComponents('M.A.D.M.A.N.', [TMZFDecompressor]);
end;

constructor TMZFDecompressor.Create;
Begin
  ExtractList:=TStringList.Create;
  ProgressTimer:=TTimer.Create(Self);
  ProgressTimer.Enabled:=False;
  ProgressTimer.Interval:=1;
  ProgressTimer.OnTimer:=OnTimer;
  OverwriteMode:=omUnknown;
  ArchiveProgress:=0;
End;

procedure TMZFDecompressor.Error(Msg:ShortString);
begin
  If Assigned(fOnError) Then
    fOnError(Self,Msg);
end;

procedure TMZFDecompressor.BeginExtract;
begin
  If Assigned(fOnBegin) Then
    Begin
      ProgressTimer.Enabled:=true;
      fOnBegin(Self);
    End;
end;

procedure TMZFDecompressor.OnLZMAProgress(const Action:TLZMAProgressAction;const Value:int64);
Begin
  If Action=LPAMax Then
    Begin
      FileSize:=Value;
      Exit;
    End;
  FileProgress:=Value;
  ArchiveProgress:=ArchiveProgress+Value;
  application.ProcessMessages;
End;

function TMZFDecompressor.ExpandFile(InFile,OutFile:TStream):Boolean;
var properties:array[0..4] of byte;
    decoder:TLZMADecoder;
    OutSize:LongWord;
    i,v:Byte;
begin
  FileProgress:=0;
  FileSize:=InFile.Size;
  InFile.Seek(0,soFromBeginning);
  OutFile.Seek(0,soFromBeginning);

  InFile.Read(Properties,5);

  decoder:=TLZMADecoder.Create;
  decoder.OnProgress:=OnLZMAProgress;
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
  FileProgress:=0;
  Result:=True;
End;

procedure TMZFDecompressor.Extract;  //Note!!! Delete text from List: "Root-[%s]"
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
      EndExtract;
      Exit;
    End;
  If (Not FileExists(FileName)) Then
    Begin
      Error(rsNoFile);
      EndExtract;
      Exit;
    End;
  BeginExtract;
  ArchiveSize:=ExtractList.Count-1;
  ArchiveStream:=TFileStream.Create(FileName,fmOpenRead);
  //ArchiveStream.LoadFromFile(FileName);
  ArchiveSize:=ArchiveStream.Size;
  ArchiveStream.Position:=0;
  ArchiveStream.Read(ArcHead,SizeOf(TArcHead));
  ArchiveStream.Read(AConfig,SizeOf(TArcConfig));
  //Check head
  If Not CheckHead(ArcHead) Then
    Begin
      Error(rsCorrupt);
      ArchiveStream.Free;
      EndExtract;
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
          EndExtract;
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
          If Pass='' Then
          GetPassword;
          If Password='' Then
            Begin
              HeadStream.Free;
              ArchiveStream.Free;
              EndExtract;
              Exit;
            End;
          Temp:=TMemoryStream.Create;
          HeadStream.Position:=0;
          DecodeStream(HeadStream,Temp,Pass,AConfig.EncodeAlgorithm);
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
          Error(rsInvalidPass);
          ArchiveStream.Free;
          Temp.Free;
          HeadStream.Free;
          EndExtract;
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
        if Assigned(fOnExtract) then
          fOnExtract(Self,FileHead.LongFileName);
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
        If (TextInList(TempFileName,ExtractList)Or ExtractAll) Then   //If want extract
          Begin
            //If encode mode
            If (AConfig.EncodeFile) Then
              Begin
                If Pass='' Then
                  GetPassword;
                If Password='' Then
                  Begin
                    HeadStream.Free;
                    ArchiveStream.Free;
                    EtalonFile.Free;
                    EndExtract;
                    Exit;
                  End;
                Temp:=TMemoryStream.Create;
                DecodeStream(EtalonFile,Temp,Pass,AConfig.EncodeAlgorithm);
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
                Error(rsInvalidFile);
                EtalonFile.Free;
                Temp.Free;
                ArchiveStream.Free;
                EndExtract;
                Exit;
              End;
            EtalonFile:=TMemoryStream.Create;
            EtalonFile.CopyFrom(Temp,Temp.Size);
            Temp.Free;
            EtalonFile.Position:=0;
            //Check CRC
            //Error(inttostr(filehead.CRC));
            If Not CRCIgnore Then
              If StreamCRC32(EtalonFile)<>FileHead.CRC Then
                Begin
                  Error(rsBadCRC);
                  EtalonFile.Free;
                  ArchiveStream.Free;
                  EndExtract;
                  Exit;
                End;  
            // Find file
            If Dir[Length(Dir)]<>'\' Then Dir:=Dir+'\';
            //DestFile:=TMemoryStream.Create;
            //DestFile.CopyFrom(EtalonFile,EtalonFile.Size);

            If FileExists(Dir+TempFileName) Then
            Begin
              If (OverWriteMode=omUnknown)Or
                 (OverWriteMode=omRewrite)Or
                 (OverWriteMode=omSkip) Then
                    GetOverwriteMode(FileHead,OverWriteMode);
            If (OverWriteMode=omOverwriteAll)Or(OverWriteMode=omRewrite)Then
              Begin
                If Not DirectoryExists(Dir+ExtractFileDir(FileHead.LongFileName)) Then
                  MakeDir(Dir+ExtractFileDir(FileHead.LongFileName));
                EtalonFile.SaveToFile(Dir+TempFileName);
                SetFileAttributes(PChar(Dir+TempFileName),FileHead.Attr);
                SetFileDateTime(Dir+TempFileName,
                      FileHead.FileCreateDate,
                      FileHead.FileModifyDate,
                      FileHead.FileOpenDate,
                      UpdateModifycationTime,
                      UpdateLastAccess);
                //
                {SetFileDateTime(Dir+'\'+FileHead.Path,
                      FileHead.DirCreateDate,
                      FileHead.DirModifyDate,
                      FileHead.DirAccessDate,
                      UpdateModifycationTime,
                      UpdateLastAccess);  }
              End;{ Else ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent);}
         End Else
          Begin
            If Not DirectoryExists(Dir+ExtractFileDir(FileHead.LongFileName)) Then
              MakeDir(Dir+ExtractFileDir(FileHead.LongFileName));
            //If Not fSecurityMode Then
              If (fSecurityMode)And(Not NameInMask(fExcludeSpec,ExtractFileName(FileHead.LongFileName))) Then
                Begin
                  EtalonFile.SaveToFile(Dir+TempFileName);
                  SetFileAttributes(PChar(Dir+TempFileName),FileHead.Attr);
                  SetFileDateTime(Dir+TempFileName,
                      FileHead.FileCreateDate,
                      FileHead.FileModifyDate,
                      FileHead.FileOpenDate,
                      UpdateModifycationTime,
                      UpdateLastAccess);
                End Else
                Begin
                  EtalonFile.SaveToFile(Dir+TempFileName);
                  SetFileAttributes(PChar(Dir+TempFileName),FileHead.Attr);
                  SetFileDateTime(Dir+TempFileName,
                      FileHead.FileCreateDate,
                      FileHead.FileModifyDate,
                      FileHead.FileOpenDate,
                      UpdateModifycationTime,
                      UpdateLastAccess);
                End;
            //
            {SetFileDateTime(Dir+'\'+FileHead.Path,
                      FileHead.DirCreateDate,
                      FileHead.DirModifyDate,
                      FileHead.DirAccessDate,
                      UpdateModifycationTime,
                      UpdateLastAccess); }
          End;
      End;
          { Else
          ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent); }
      EtalonFile.Free;//Free etalon file
      ArchiveProgress:=ArchiveStream.Position;
      Application.ProcessMessages;
      Except
        Error(rsInvalidPass);
        ArchiveStream.Free;
        Exit;
      End;

    End;
  ArchiveStream.Free;
  If DelAfterExtract Then
    Begin
      SetFileAttributes(PChar(FileName),FILE_ATTRIBUTE_NORMAL);
      DeleteFile(PChar(FileName));
    End;
  EndExtract;
End;

procedure TMZFDecompressor.ExtractFromStream(ArchiveStream:TStream);  //Note!!! Delete text from List: "Root-[%s]"
Var EtalonFile:TMemoryStream;
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
  BeginExtract;
  ArchiveSize:=ExtractList.Count-1;
  //ArchiveStream.LoadFromFile(FileName);
  ArchiveStream.Position:=0;
  ArchiveSize:=ArchiveStream.Size;
  ArchiveStream.Read(ArcHead,SizeOf(TArcHead));
  ArchiveStream.Read(AConfig,SizeOf(TArcConfig));
  //Check head
  If Not CheckHead(ArcHead) Then
    Begin
      Error(rsCorrupt);
      //ArchiveStream.Free;
      EndExtract;
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
          EndExtract;
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
          If Pass='' Then
          GetPassword;
          If Password='' Then
            Begin
              HeadStream.Free;
              //ArchiveStream.Free;
              EndExtract;
              Exit;
            End;
          Temp:=TMemoryStream.Create;
          HeadStream.Position:=0;
          DecodeStream(HeadStream,Temp,Pass,AConfig.EncodeAlgorithm);
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
          Error(rsInvalidPass);
          //ArchiveStream.Free;
          Temp.Free;
          HeadStream.Free;
          EndExtract;
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
        if Assigned(fOnExtract) then
          fOnExtract(Self,FileHead.LongFileName);
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
        If (TextInList(TempFileName,ExtractList)Or ExtractAll) Then   //If want extract
          Begin
            //If encode mode
            If (AConfig.EncodeFile) Then
              Begin
                If Pass='' Then
                  GetPassword;
                If Password='' Then
                  Begin
                    HeadStream.Free;
                    //ArchiveStream.Free;
                    EtalonFile.Free;
                    EndExtract;
                    Exit;
                  End;
                Temp:=TMemoryStream.Create;
                DecodeStream(EtalonFile,Temp,Pass,AConfig.EncodeAlgorithm);
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
                Error(rsInvalidFile);
                EtalonFile.Free;
                Temp.Free;
                //ArchiveStream.Free;
                EndExtract;
                Exit;
              End;
            EtalonFile:=TMemoryStream.Create;
            EtalonFile.CopyFrom(Temp,Temp.Size);
            Temp.Free;
            EtalonFile.Position:=0;
            //Check CRC
            //Error(inttostr(filehead.CRC));
            If Not CRCIgnore Then
              If StreamCRC32(EtalonFile)<>FileHead.CRC Then
                Begin
                  Error(rsBadCRC);
                  EtalonFile.Free;
                  //ArchiveStream.Free;
                  EndExtract;
                  Exit;
                End;  
            // Find file
            If Dir[Length(Dir)]<>'\' Then Dir:=Dir+'\';
            //DestFile:=TMemoryStream.Create;
            //DestFile.CopyFrom(EtalonFile,EtalonFile.Size);

            If FileExists(Dir+TempFileName) Then
            Begin
              If (OverWriteMode=omUnknown)Or
                 (OverWriteMode=omRewrite)Or
                 (OverWriteMode=omSkip) Then
                    GetOverwriteMode(FileHead,OverWriteMode);
            If (OverWriteMode=omOverwriteAll)Or(OverWriteMode=omRewrite)Then
              Begin
                If Not DirectoryExists(Dir+ExtractFileDir(FileHead.LongFileName)) Then
                  MakeDir(Dir+ExtractFileDir(FileHead.LongFileName));
                EtalonFile.SaveToFile(Dir+TempFileName);
                SetFileAttributes(PChar(Dir+TempFileName),FileHead.Attr);
                SetFileDateTime(Dir+TempFileName,
                      FileHead.FileCreateDate,
                      FileHead.FileModifyDate,
                      FileHead.FileOpenDate,
                      UpdateModifycationTime,
                      UpdateLastAccess);
                //
                {SetFileDateTime(Dir+'\'+FileHead.Path,
                      FileHead.DirCreateDate,
                      FileHead.DirModifyDate,
                      FileHead.DirAccessDate,
                      UpdateModifycationTime,
                      UpdateLastAccess);  }
              End;{ Else ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent);}
         End Else
          Begin
            If Not DirectoryExists(Dir+ExtractFileDir(FileHead.LongFileName)) Then
              MakeDir(Dir+ExtractFileDir(FileHead.LongFileName));
            //If Not fSecurityMode Then
              If (fSecurityMode)And(Not NameInMask(fExcludeSpec,ExtractFileName(FileHead.LongFileName))) Then
                Begin
                  EtalonFile.SaveToFile(Dir+TempFileName);
                  SetFileAttributes(PChar(Dir+TempFileName),FileHead.Attr);
                  SetFileDateTime(Dir+TempFileName,
                      FileHead.FileCreateDate,
                      FileHead.FileModifyDate,
                      FileHead.FileOpenDate,
                      UpdateModifycationTime,
                      UpdateLastAccess);
                End Else
                Begin
                  EtalonFile.SaveToFile(Dir+TempFileName);
                  SetFileAttributes(PChar(Dir+TempFileName),FileHead.Attr);
                  SetFileDateTime(Dir+TempFileName,
                      FileHead.FileCreateDate,
                      FileHead.FileModifyDate,
                      FileHead.FileOpenDate,
                      UpdateModifycationTime,
                      UpdateLastAccess);
                End;
            //
            {SetFileDateTime(Dir+'\'+FileHead.Path,
                      FileHead.DirCreateDate,
                      FileHead.DirModifyDate,
                      FileHead.DirAccessDate,
                      UpdateModifycationTime,
                      UpdateLastAccess); }
          End;
      End;
          { Else
          ArchiveStream.Seek(FileHead.PackedSize,soFromCurrent); }
      EtalonFile.Free;//Free etalon file
      ArchiveProgress:=ArchiveStream.Position;
      Application.ProcessMessages;
      Except
        Error(rsInvalidPass);
        //ArchiveStream.Free;
        Exit;
      End;

    End;
  EndExtract;
End; 

procedure TMZFDecompressor.EndExtract;
begin
  If Assigned(fOnEnd) Then
    Begin
      ProgressTimer.Enabled:=False;
      fOnEnd(Self);
    End;
end;

procedure TMZFDecompressor.GetOverwriteMode(Var FileHead:TFileHead;Var Mode:TReWriteMode);
Begin
  If Assigned(fOnOverwritePrompt) Then
    fOnOverwritePrompt(Self,FileHead,Mode);
End;

procedure TMZFDecompressor.OnTimer(Sender:TObject);
Var A,F:Byte;
Begin
  A:=0;
  F:=0;
  A:=GetPercentDone(0,ArchiveProgress,ArchiveSize);
  F:=GetPercentDone(0,FileProgress,FileSize);
  if Assigned(fOnProgress) then
    fOnProgress(Self,F,A,Abort);
End;

procedure TMZFDecompressor.BeginEvent;
Begin
  If Assigned(fOnBegin)Then
    Begin
      fOnBegin(Self);
      ProgressTimer.Enabled:=True;
    End;
End;

procedure TMZFDecompressor.EndEvent;
Begin
  If Assigned(fOnEnd)Then
    Begin
      fOnEnd(Self);
      ProgressTimer.Enabled:=False;
    End;
End;

procedure TMZFDecompressor.GetPassword;
Begin
  If Assigned(fOnGetPass) Then
    fOnGetPass(Self,Pass);
End;

procedure TMZFDecompressor.Free;
Begin
  If Self<>Nil Then
    Begin
      ExtractList.Free;
      Destroy;
    End;
end;

destructor TMZFDecompressor.Destroy;
Begin
  // NULL
End;

end.
