unit ThrdEncode_u;

interface

uses
  SysUtils, Windows, Classes, EncodeProgress_u, Advanced, CRC32, MD5, Forms, AES_256_u,
  MAD7_u, IDEA, Dialogs;

type
  TEncodeMode=(emEncode,emDecode);

type
  THash=Packed Array[0..15] of byte;

type
  TEncodeFileHead=Packed Record
    Signature:Packed Array[0..2] Of Char;
    Version:Packed Array[0..2] Of Char;
    //FileName:String;
    FileCRC32:Cardinal;
    EncAlg:TEncodeAlgorithm;
    FileHash:THash;
  End;

type
  TEncodeThread = class(TThread)
  private
    { Private declarations }
    AFileList:TStringList;
    AMode:TEncodeMode;
    ADestDir:String;
    APutIntoDir:Boolean;
    AAlgorithm:TEncodeAlgorithm;
    APassword:String;
    AModifyExtention:Boolean;
    AExtention:String;
  public
    {constructor Create;}
    property FileList:TStringList read AFileList write AFileList;
    property Mode:TEncodeMode read AMode write AMode;
    property DestDir:String read ADestDir write ADestDir;
    property PutIntoDir:Boolean read APutIntoDir write APutIntoDir;
    property Algorithm:TEncodeAlgorithm read AAlgorithm write AAlgorithm;
    property Password:String read APassword write APassword;
    property ModifyExtention:Boolean read AModifyExtention write AModifyExtention;
    property Extention:String read AExtention write AExtention;
    procedure SearchDir(Dir,Ext: string);
    procedure Initialize;
    procedure Execute;
  end;

implementation

Uses Main_u;

Const
  efhSignature='MNC';
  efhVersion='1.0';

procedure Convert(Inp:MD5Digest; Var Outp:THash);
Var Index:Byte;
Begin
  For Index:=0 To 15 Do
    Outp[Index]:=Inp[Index];
End;

function Compare(Hash:THash;MD5d:MD5Digest):Boolean;
Var Index:Byte;
Begin
  For Index:=0 To 15 Do
    If Hash[Index]<>MD5d[Index] Then
      Begin
        Result:=False;
        Exit;
      End;
  Result:=True;
End;

{constructor TEncodeThread.Create;
Begin
  AFileList:=TStringList.Create;
End; }

procedure TEncodeThread.Initialize;
Begin
  AFileList:=TStringList.Create;
  AFileList.Clear;
End;

procedure TEncodeThread.SearchDir(Dir,Ext: string);
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
          if FileMaskEquate(SR.Name, Ext) = true then begin
             AFileList.Add(Dir+SR.Name);
            end;
            //showmessage(dir+sr.Name);
            FindRes := FindNext(SR);
          end;
  FindClose(FindRes);
end;

procedure TEncodeThread.Execute;
var Index:LongWord;
    FileHead:TEncodeFileHead;
    HeadHash:THash;
    Wnd:TdlgOperationsProgress;
    InStream:TFileStream;
    OutStream:TMemoryStream;
    Temp:TStream;
    DominateDir:String;
    FileDir:String;
    HeadSize,ContPos:Byte;
    DirList:TStringList;
begin
  If AFileList.Count<=0 Then
    Exit;
  For Index:=0 To AFileList.Count-1 Do
    Begin
      FileDir:=ExtractFileDir(AFileList.Strings[Index]);
      If FileDir[Length(FileDir)]<>'\' Then FileDir:=FileDir+'\';
      If Pos('*',AFileList.Strings[Index])<>0 Then
        SearchDir(FileDir,ExtractFileName(AFileList.Strings[Index]));
    End;
  Index:=0;
  While Index<=AFileList.Count-1 Do
    Begin
      If Pos('*',AFileList.Strings[Index])<>0 Then
        Begin
          AFileList.Delete(Index);
        End Else
      Inc(Index);
    End;
  Wnd:=TdlgOperationsProgress.Create(Application);
  Case AMode Of
    emEncode:
      Begin
        DirList:=TStringList.Create;
        For Index:=0 To AFileList.Count-1 Do
          //If Not TextInList(ExtractFileDir(AFileList.Strings[Index]),DirList) Then
            DirList.Add(ExtractFileDir(AFileList.Strings[Index]));
        DominateDir:=GetDominateDir(DirList);
        DirList.Free;
        Wnd.Caption:=ReadFromLanguage('Windows','wndEncodeFiles','Encode files');
        Wnd.lbCurrentFile.Caption:=ReadFromLanguage('Status','PrepareToEncode','Prepare to encoding!!!');
        Wnd.lbProgerss.Caption:=ReadFromLanguage('Status','Progress0','Pregress 0');
        Wnd.Show;
        Wnd.Update;
        For Index:=0 To AFileList.Count-1 Do
          Begin
            if Not FileExists(AFileList[Index]) then
              Continue;
            Wnd.Update;
            Wnd.lbCurrentFile.Caption:=Format(ReadFromLanguage('Status','Encoding','Encoding: %s'),[ExtractFileName(AFileList.Strings[Index])]);
            Wnd.pbEncoding.Position:=GetPercentDone(0,Index,AFileList.Count-1);
            Wnd.lbProgerss.Caption:=Format(ReadFromLanguage('Status','Progressing','Progress %d'),[Wnd.pbEncoding.Position]);
            Application.ProcessMessages;
            If Wnd.Abort Then
              Begin
                Wnd.Free;
                Exit;
              End;
            OutStream:=TMemoryStream.Create;
            InStream:=TFileStream.Create(AFileList.Strings[Index],fmOpenRead);
            FileHead.Signature:=efhSignature;
            FileHead.Version:=efhVersion;
            //FileHead.FileName:=ExtractFileName(AFileList.Strings[Index]);
            FileHead.EncAlg:=AAlgorithm;
            FileHead.FileCRC32:=StreamCRC32(InStream);
            Convert(MD5Stream(InStream),FileHead.FileHash);
            OutStream.Write(FileHead,SizeOf(TEncodeFileHead));
            Convert(MD5Stream(OutStream),HeadHash);
            OutStream.Write(HeadHash,SizeOf(THash));
            InStream.Position:=0;
            Temp:=TMemoryStream.Create;
            Case AAlgorithm Of
              eaAES256:AES256_EncodeStream(InStream,Temp,APassword);
              eaMAD7:MAD7_EncodeStream(InStream,Temp,APassword);
              eaIDEA:IDEA_EncryptCopy(Temp,InStream,InStream.Size,APassword);
            End;
            OutStream.CopyFrom(Temp,Temp.Size);
            Temp.Free;
            InStream.Free;
            // Save to file =)
            {ShowMessage(DestDir+#13+ExtractFileDir(AFileList.Strings[Index])+#13+DominateDir+#13+
              DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir)));}
            If APutIntoDir Then
              Begin
                If AModifyExtention Then
                  Begin
                    If Not DirectoryExists(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),DominateDir)) Then
                      MakeDir(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+'\');
                    OutStream.SaveToFile(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+
                                         '\'+ChangeFileExt(AFileList.Strings[Index],AExtention));
                  End
                  Else
                    Begin
                      If Not DirectoryExists(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))) Then
                        MakeDir(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+'\');
                      OutStream.SaveToFile(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+
                                           '\'+ExtractFileName(AFileList.Strings[Index]));
                    End;
              End Else
              Begin
                If AModifyExtention Then
                  Begin
                    FileSetAttr(AFileList.Strings[Index],0);
                    SysUtils.DeleteFile(AFileList.Strings[Index]);
                    OutStream.SaveToFile(ChangeFileExt(AFileList.Strings[Index],AExtention));
                  End Else
                  Begin
                    FileSetAttr(AFileList.Strings[Index],0);
                    SysUtils.DeleteFile(AFileList.Strings[Index]);
                    OutStream.SaveToFile(AFileList.Strings[Index]);
                  End;
              End;
            //
            OutStream.Free;
          End;
        Wnd.Free;
      End;
    emDecode:
      Begin
        DirList:=TStringList.Create;
        For Index:=0 To AFileList.Count-1 Do
          //If Not TextInList(ExtractFileDir(AFileList.Strings[Index]),DirList) Then
            DirList.Add(ExtractFileDir(AFileList.Strings[Index]));
        DominateDir:=GetDominateDir(DirList);
        DirList.Free;
        Wnd.Caption:=ReadFromLanguage('Windows','wndDecodeFiles','Decode files');
        Wnd.lbCurrentFile.Caption:=ReadFromLanguage('Status','PrepareToDecode','Prepare to decoding!!!');
        Wnd.Show;
        Wnd.Update;
        For Index:=0 To AFileList.Count-1 Do
          Begin
            if Not FileExists(AFileList[Index]) then
              Continue;
            Wnd.Update;
            Wnd.lbCurrentFile.Caption:=Format(ReadFromLanguage('Status','Decoding','Decoding: %s'),[ExtractFileName(AFileList.Strings[Index])]);
            Wnd.pbEncoding.Position:=GetPercentDone(0,Index,AFileList.Count-1);
            Application.ProcessMessages;
            If Wnd.Abort Then
              Begin
                Wnd.Free;
                Exit;
              End;
            OutStream:=TMemoryStream.Create;
            InStream:=TFileStream.Create(AFileList.Strings[Index],fmOpenRead);
            HeadSize:=InStream.Read(FileHead,SizeOf(TEncodeFileHead));
            If (FileHead.Signature<>efhSignature)Or(FileHead.Version<>efhVersion)Then
              Begin
                MessageBox(frmMain.Handle,PChar(Format(ReadFromLanguage('Messages','CantDecodeFile','Coud not decode file "%s"'),[ExtractFileName(AFileList.Strings[Index])])),
                           'WinMZF',MB_OK+MB_ICONSTOP);
                OutStream.Free;
                InStream.Free;
                Continue;
              End;
            InStream.Read(HeadHash,SizeOf(THash));
            ContPos:=InStream.Position;
            Temp:=TMemoryStream.Create;
            InStream.Position:=0;
            Temp.CopyFrom(InStream,HeadSize);
            If Not Compare(HeadHash,MD5Stream(Temp))Then
              Begin
                MessageBox(frmMain.Handle,PChar(Format(ReadFromLanguage('Messages','InvalidFileAndPass','File "%s" is corrupt, invalid password'),[ExtractFileName(AFileList.Strings[Index])])),
                           'WinMZF',MB_OK+MB_ICONSTOP);
                Temp.Free;
                InStream.Free;
                OutStream.Free;
                Continue;
              End;
            Temp.Free;
            Temp:=TMemoryStream.Create;
            InStream.Position:=ContPos;
            Temp.CopyFrom(InStream,InStream.Size-InStream.Position);
            InStream.Free;
            Case FileHead.EncAlg Of
              eaAES256:AES256_DecodeStream(Temp,OutStream,APassword);
              eaMAD7:MAD7_DecodeStream(Temp,OutStream,APassword);
              eaIDEA:IDEA_DecryptCopy(OutStream,Temp,Temp.Size,APassword);
            End;
            If Not Compare(FileHead.FileHash,MD5Stream(OutStream)) Then
              Begin
                MessageBox(frmMain.Handle,PChar(Format(ReadFromLanguage('Messages','CorruptFile','File "%s" is corrupt'),[ExtractFileName(AFileList.Strings[Index])])),
                           'WinMZF',MB_OK+MB_ICONSTOP);
                Temp.Free;
                OutStream.Free;
                Continue;
              End;   
            Temp.Free;
            If APutIntoDir Then
              Begin
                If AModifyExtention Then
                  Begin
                    If Not DirectoryExists(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),DominateDir)) Then
                      MakeDir(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+'\');
                    OutStream.SaveToFile(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+
                                         '\'+ChangeFileExt(AFileList.Strings[Index],AExtention));
                  End
                  Else
                    Begin
                      If Not DirectoryExists(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))) Then
                        MakeDir(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+'\');
                      OutStream.SaveToFile(DestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+
                                           '\'+ExtractFileName(AFileList.Strings[Index]));
                    End;
              End Else
              Begin
                If AModifyExtention Then
                  Begin
                    FileSetAttr(AFileList.Strings[Index],0);
                    SysUtils.DeleteFile(AFileList.Strings[Index]);
                    OutStream.SaveToFile(AFileList.Strings[Index]);
                  End Else
                  Begin
                    FileSetAttr(AFileList.Strings[Index],0);
                    SysUtils.DeleteFile(AFileList.Strings[Index]);
                    OutStream.SaveToFile(AFileList.Strings[Index]);
                  End;
              End;
            OutStream.Free;
          End;
        Wnd.Free;
      End;
  End;
  Self.Free;
end;

end.
