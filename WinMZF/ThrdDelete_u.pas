unit ThrdDelete_u;

interface

uses
  Classes, EncodeProgress_u, Forms, Windows, SysUtils, Advanced, Dialogs;

type
  TThrdDelete = class(TThread)
  private
    { Private declarations }
  public
    AFiles:TStringList;
    AFillZero:Boolean;
    procedure Initialize;
    property Files:TStringList read AFiles write AFiles;
    property FillZeroBytes:Boolean read AFillZero write AFillZero;
    procedure Execute; override;
  protected
    procedure SearchDir(Dir,Ext: string);
  end;

implementation

procedure TThrdDelete.Initialize;
Begin
  AFiles:=TStringList.Create;
End;

procedure TThrdDelete.SearchDir(Dir,Ext: string);
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
             Files.Add(Dir+SR.Name);
            end;
            //showmessage(dir+sr.Name);
            FindRes := FindNext(SR);
          end;
  Windows.FindClose(FindRes);
end;

procedure TThrdDelete.Execute;
var Wnd:TdlgOperationsProgress;
    FileStream:TFileStream;
    Index:LongWord;
    FileDir:String;
    FF:Byte;
begin
  If AFiles.Count<=0 Then
    Exit;
  For Index:=0 To AFiles.Count-1 Do
    Begin
      FileDir:=ExtractFileDir(AFiles.Strings[Index]);
      If FileDir[Length(FileDir)]<>'\' Then FileDir:=FileDir+'\';
      If Pos('*',AFiles.Strings[Index])<>0 Then
        SearchDir(FileDir,ExtractFileName(AFiles.Strings[Index]));
    End;
  Index:=0;
  While Index<=AFiles.Count-1 Do
    Begin
      If Pos('*',AFiles.Strings[Index])<>0 Then
        Begin
          AFiles.Delete(Index);
        End Else
      Inc(Index);
    End;
  FF:=$FF;  
  Wnd:=TdlgOperationsProgress.Create(Application);
  Wnd.Caption:=ReadFromLanguage('Windows','wndDeleteFiles','Delete files');
  Wnd.lbCurrentFile.Caption:=ReadFromLanguage('Status','PrepareToDelete','Prepare to deleting!!!');
  Wnd.lbProgerss.Caption:=ReadFromLanguage('Status','Progress0','Progress 0');
  Wnd.Show;
  Wnd.Update;

  For Index:=0 To AFiles.Count-1 Do
    Begin
      FileSetAttr(AFiles.Strings[Index],0);
      If AFillZero Then
        Begin
          Wnd.Update;
          Wnd.lbCurrentFile.Caption:=Format(ReadFromLanguage('Status','Deleting','Deleting: %s'),[ExtractFileName(AFiles.Strings[Index])]);
          Wnd.pbEncoding.Position:=GetPercentDone(0,Index,AFiles.Count-1);
          Wnd.lbProgerss.Caption:=Format(ReadFromLanguage('Status','_Progress','Progress %d'),[Wnd.pbEncoding.Position]);
          Application.ProcessMessages;
          If Wnd.Abort Then
           Begin
            Wnd.Free;
            Exit;
           End;
          FileStream:=TFileStream.Create(AFiles.Strings[Index],fmOpenWrite);
          While FileStream.Position<>FileStream.Size Do
            Begin
              Application.ProcessMessages;
              FileStream.Write(FF,SizeOf(FF));
            End;
          FileStream.Free;
        End;
      {showmessage(AFiles.Strings[Index]);}
      If Not DeleteFile(AFiles.Strings[Index])Then
        MessageBox(Wnd.Handle,PChar(Format(ReadFromLanguage('Messages','CantDelete','Coud not delete file "%s"'),[ExtractFileName(AFiles.Strings[Index])])),'WinMZF',MB_OK+MB_ICONEXCLAMATION);
    End;
  For Index:=0 To AFiles.Count-1 Do
    Begin
      FileSetAttr(ExtractFileDir(AFiles.Strings[Index]),0);
      RemoveDir(ExtractFileDir(AFiles.Strings[Index]));
      {showmessage(ExtractFileDir(AFiles.Strings[Index]));}
    End;
  Wnd.Free;
  Self.Free;
end;

end.
