unit ThrdCopy_u;

interface

uses
  Classes, Forms, SysUtils, Windows, Advanced, EncodeProgress_u, Dialogs;

type
  TThrdCopy = class(TThread)
  private
    { Private declarations }
    aReplace:Boolean;
    aFileList:TStringList;
    aDestDir:String;
    procedure CopyFileWithStream(InFile,OutFile:String);
    procedure SearchDir(Dir,Ext: string);
  public
    property Replace:Boolean read aReplace write aReplace default False;
    property FilesList:TStringList read aFileList write aFileList;
    property DestinationDir:String read aDestDir write aDestDir;
    procedure Initialize;
    procedure Execute; override;
  end;

implementation

procedure TThrdCopy.SearchDir(Dir,Ext: string);
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
             aFileList.Add(Dir+SR.Name);
            end;
            //showmessage(dir+sr.Name);
            FindRes := FindNext(SR);
          end;
  FindClose(FindRes);
end;

procedure TThrdCopy.CopyFileWithStream(InFile,OutFile:String);
var InStream:TFileStream;
    OutStream:TMemoryStream;
    Buff:Array[0..511]Of Byte;
    Attr:Integer;
    ReadCount:Word;
    CreationTime,ModifyTime,AccessTime:TDateTime;
begin
  Attr:=0;
  CreationTime:=FileDateToDateTime(FileCreationTime(InFile));
  ModifyTime:=FileDateToDateTime(FileModifyTime(InFile));
  AccessTime:=FileDateToDateTime(FileAccessTime(InFile));
  If Not FileExists(InFile) Then
    Exit;
  Attr:=GetFileAttributes(Pchar(InFile));
  InStream:=TFileStream.Create(InFile,fmOpenRead);
  OutStream:=TMemoryStream.Create;
  While InStream.Position<>InStream.Size Do
    Begin
      ReadCount:=InStream.Read(Buff,SizeOf(Buff));
      OutStream.Write(Buff,ReadCount);
      Application.ProcessMessages;
    End;
  InStream.Free;
  OutStream.SaveToFile(OutFile);
  FileSetAttr(OutFile,Attr);
  OutStream.Free;
  If aReplace Then
    Begin
      FileSetAttr(InFile,0);
      DeleteFile(PChar(InFile));
    End;
  SetFileDateTime(OutFile,CreationTime,ModifyTime,AccessTime,False,False);
end;

procedure TThrdCopy.Initialize;
begin
  aFileList:=TStringList.Create;
End;

procedure TThrdCopy.Execute;
var Index:LongWord;
    Wnd:TdlgOperationsProgress;
    DominateDir:String;
    FileDir:String;
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
  DirList:=TStringList.Create;
  For Index:=0 To AFileList.Count-1 Do
    //If Not TextInList(ExtractFileDir(AFileList.Strings[Index]),DirList) Then
      DirList.Add(ExtractFileDir(AFileList.Strings[Index]));
  DominateDir:=GetDominateDir(DirList);
  DirList.Free;
  If Not aReplace Then
    Wnd.Caption:=ReadFromLanguage('Windows','wndCopyFiles','Copy files')
  Else
    Wnd.Caption:=ReadFromLanguage('Windows','wndReplaceFiles','Replace files');
  If Not aReplace Then
    Wnd.lbCurrentFile.Caption:=ReadFromLanguage('Status','PrepareToCopy','Prepare to copy!!!')
  Else
    Wnd.lbCurrentFile.Caption:=ReadFromLanguage('Status','PrepareToReplace','Prepare to replace!!!');
  Wnd.lbProgerss.Caption:=ReadFromLanguage('Status','Progress0','Pregress 0');
  Wnd.Show;
  Wnd.Update;
  For Index:=0 To AFileList.Count-1 Do
    Begin
      Wnd.Update;
      Wnd.lbCurrentFile.Caption:=Format(ReadFromLanguage('Status','Processing','Processing: %s'),[ExtractFileName(AFileList.Strings[Index])]);
      Wnd.pbEncoding.Position:=GetPercentDone(0,Index,AFileList.Count-1);
      Wnd.lbProgerss.Caption:=Format(ReadFromLanguage('Status','_Progress','Progress %d'),[Wnd.pbEncoding.Position]);
      Application.ProcessMessages;
      If Wnd.Abort Then
        Begin
          Wnd.Free;
          Exit;
        End;
      If Not DirectoryExists(aDestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),DominateDir)) Then
        MakeDir(aDestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+'\');
      CopyFileWithStream(AFileList.Strings[Index],aDestDir+SubStractString(ExtractFileDir(AFileList.Strings[Index]),GetLastDir(DominateDir))+
                                           '\'+ExtractFileName(AFileList.Strings[Index]));
    End;
  Wnd.Free;
  Self.Free;
end;

end.
