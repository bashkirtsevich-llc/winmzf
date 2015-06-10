program WinMZF;

uses
  Forms,
  SysUtils,
  SignatureDetect_u,
  Classes,
  Controls,
  Advanced,
  Decompress_u,
  LZMA_u,
  UniversalUnpacker_u,
  Windows,
  Dialogs,
  Compress_u,
  UniversalCompressor_u,
  SevenZip_Compressor_u,
  Main_u in 'Main_u.pas' {frmMain},
  Add_u in 'Add_u.pas' {dlgAddFiles},
  Extract_u in 'Extract_u.pas' {dlgExtract},
  OverWrite_u in 'OverWrite_u.pas' {dlgOverWrite},
  Progress_u in 'Progress_u.pas' {dlgProgress},
  EncodeDecode_u in 'EncodeDecode_u.pas' {dlgEncodeDecode},
  ThrdEncode_u in 'ThrdEncode_u.pas',
  EncodeProgress_u in 'EncodeProgress_u.pas' {dlgOperationsProgress},
  ThrdDelete_u in 'ThrdDelete_u.pas',
  SeletFolder_u in 'SeletFolder_u.pas' {dlgSelectFolder},
  ThrdCopy_u in 'ThrdCopy_u.pas',
  FileInfo_u in 'FileInfo_u.pas' {dlgFileInfo},
  Favorite_u in 'Favorite_u.pas' {dlgFavorite},
  Search_u in 'Search_u.pas' {dlgFindConfig},
  FindResult_u in 'FindResult_u.pas' {dlgSearchResults},
  Config_u in 'Config_u.pas' {dlgConfig},
  Tips_u in 'Tips_u.pas' {dlgTips},
  TestArchive_u in 'TestArchive_u.pas' {dlgTestArchive},
  Convert_u in 'Convert_u.pas' {dlgConvert},
  MakeSFX_u in 'MakeSFX_u.pas' {dlgMakeSFX},
  Report_u in 'Report_u.pas' {wndMessages},
  About_u in 'About_u.pas' {dlgAbout},
  Constructor_u in 'SKinEngine\Constructor_u.pas',
  Info_u in 'Info_u.pas' {dlgInfo};

{$R *.res}

Type TConsoleManager=Object
  Private
    ResName:String;
    dlgCaption:String;
    Files:TStringList;
    MZFLevel:TMZFCompressionLevel;
    Level:Byte;
    Comment:String;
  Public
    procedure ArchiverOnBegin(Sender:TObject);
    procedure ArchiverProgress(Sender:TObject;FProgress,AProgress:Byte;
                               Var Abort:Boolean);
    procedure ArchiverOnEnd(Sender:TObject);
    procedure ExtractArchive(ArchiveFIle:String;DestDir:String;
                             RewriteMode:TRewriteMode;
                             ExtractAll,_Delete,UpdateModTime,
                             UpdateAccessTime:Boolean);
    procedure CreateArchive(ArchiveFile:String;ArchCompressMethod:TFileType;
                            Password:String;EncodeHead,EncodeFile:Boolean;
                            ReadOnly,Archived,Hidden:Boolean);
    procedure GetPass(Sender:TObject;Var Pass:ShortString);
    procedure OnError(Sender:TObject;ErrorMsg:ShortString);
    procedure RewritePromt(Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode);
  End;

procedure TConsoleManager.ArchiverOnBegin(Sender:TObject);
begin
  dlgProgress:=TdlgProgress.Create(Application);
  dlgProgress.Position:=poScreenCenter;
  dlgProgress.Show;
  dlgProgress.Caption:=dlgCaption;
  //dlgProgress.Anime.ResName:=ResName;
  //dlgProgress.Anime.Active:=True;
  dlgProgress.Update;
  dlgProgress.pbArchive.Position:=0;
  dlgProgress.pbFile.Position:=0;
end;

procedure TConsoleManager.ArchiverProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
begin
  dlgProgress.pbArchive.Position:=AProgress;
  dlgProgress.pbFile.Position:=FProgress;
  dlgProgress.lbFileIndex.Caption:='';
  dlgProgress.lbCurrent.Caption:=Format('Current progress %d',[FProgress])+'%';
  dlgProgress.lbTotal.Caption:=Format('Total progress %d',[AProgress])+'%';
  Abort:=dlgProgress.Abort;
  dlgProgress.Update;
end;

procedure TConsoleManager.ArchiverOnEnd(Sender:TObject);
begin
  dlgProgress.Hide;
  dlgProgress.Free;
end;

procedure TConsoleManager.ExtractArchive(ArchiveFIle:String;DestDir:String;
  RewriteMode:TRewriteMode;ExtractAll,_Delete,UpdateModTime,UpdateAccessTime:Boolean);
var MZFDecompressor:TMZFDecompressor;
    UnPacker:TUnPacker;
    SevenZipDecompressor:TSevenZipViewer;
    Index:Byte;
    Temp:String;
begin
  dlgCaption:='Progress-Deompressing';
  ResName:='DECOMPRESSING';
  Case GetArchiveTypeBySignature(ArchiveFile) Of
    ftMZF:Begin
            MZFDecompressor                     :=TMZFDecompressor.Create;
            MZFDecompressor.ArchiveFile         :=ArchiveFile;
            MZFDecompressor.DestinationDir      :=DestDir;
            If Not ExtractAll Then
              For Index:=0 To Files.Count-1 Do
                Begin
                  Temp:=Files.Strings[Index];
                  MZFDecompressor.FileList.Add(Temp);
                End;
            MZFDecompressor.ExtractAll          :=ExtractAll;
            MZFDecompressor.OnBegin             :=ArchiverOnBegin;
            MZFDecompressor.OnEnd               :=ArchiverOnEnd;
            MZFDecompressor.OnProgress          :=ArchiverProgress;
            MZFDecompressor.OnGetPass           :=GetPass;
            MZFDecompressor.OnError             :=OnError;
            MZFDecompressor.UpdateModifyTime    :=UpdateModTime;
            MZFDecompressor.UpdateLastAccesstime:=UpdateAccessTime;
            MZFDecompressor.FileOverwriteMode   :=RewriteMode;
            MZFDecompressor.OnOverwritePrompt   :=RewritePromt;
            MZFDecompressor.DeleteAfterExtract  :=_Delete;
            MZFDecompressor.Extract;
            MZFDecompressor.Free;
          End;
    ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,
    ftTar,ftACE2,ftJar,ftArc,ftArj,
    ftPKG5:Begin
          UnPacker:=TUnPacker.Create;
          UnPacker.ArchiveFile:=ArchiveFIle;
          UnPacker.ArchiveType:=GetArchiveTypeBySignature(ArchiveFile);
          UnPacker.DestinationDir:=DestDir;
          If ExtractAll Then
            UnPacker.FileSpec.Add('*.*')
          Else
            For Index:=0 To Files.Count-1 Do
              Begin
                Temp:=Files.Strings[Index];
                //Delete(Temp,1,1);
                UnPacker.FileSpec.Add(Temp);
              End;
          UnPacker.OnProgress:=ArchiverProgress;
          UnPacker.OnGetPassword:=GetPass;
          UnPacker.OnFileExists:=RewritePromt;
          UnPacker.OnBegin:=ArchiverOnBegin;
          UnPacker.OnEnd:=ArchiverOnEnd;
          UnPacker.DeleteAfterExtract:=_Delete;
          UnPacker.OverWriteMode:=RewriteMode;
          UnPacker.Extract;
          UnPacker.Free;
        End;
      ft7z:Begin
          SevenZipDecompressor:=TSevenZipViewer.Create;
          SevenZipDecompressor.FileName:=ArchiveFile;
          SevenZipDecompressor.DestDir:=DestDir;
          If ExtractAll Then
            SevenZipDecompressor.FileSpec.Clear;
          SevenZipDecompressor.OnBegin:=ArchiverOnBegin;
          SevenZipDecompressor.OnEnd:=ArchiverOnEnd;
          SevenZipDecompressor.OnProgress:=ArchiverProgress;
          SevenZipDecompressor.OnOverWrite:=RewritePromt;
          SevenZipDecompressor.OverWriteMode:=RewriteMode;
          SevenZipDecompressor.Extract;
          SevenZipDecompressor.Free;
        End;
      ftRar:
        Begin
          //LoadDll(hInstance);
          //RarExtract(PChar(WorkingArchive.Archive),PChar(DestDir));
          //FreeDll
        End;
  End;
end;

procedure TConsoleManager.CreateArchive(ArchiveFile: string; ArchCompressMethod: TFileType;
  Password: string; EncodeHead: Boolean; EncodeFile: Boolean; ReadOnly: Boolean;
  Archived: Boolean; Hidden: Boolean);
var MZFCompressor:TMzfCompressor;
    UniversalCompressor:TUniversalCompressor;
    SevenZipCompressor:TSevenZipCompressor;
    Index:LongWord;
begin
  dlgCaption:='Progress-Compressing';
  ResName:='COMPRESSING';
  case ArchCompressMethod of
    ftMZF: Begin
              MZFCompressor:=TMzfCompressor.Create;
              MZFCompressor.ArchiveFileName:=ArchiveFile;
              MZFCompressor.OnBegin:=ArchiverOnBegin;
              MZFCompressor.OnProgress:=ArchiverProgress;
              MZFCompressor.OnEnd:=ArchiverOnEnd;
              MZFCompressor.OnGetPassword:=GetPass;
              MZFCompressor.Commentary:=Comment;
              MZFCompressor.EncodeFileContain:=EncodeFile;
              MZFCompressor.EncodeHeadInfo:=EncodeHead;
              MZFCompressor.ArchReadOnly:=ReadOnly;
              MZFCompressor.ArcHidden:=Hidden;
              MZFCompressor.ArcArchive:=Archived;
              MZFCompressor.Password:=Password;
              MZFCompressor.Level:=MZFLevel;
              for Index := 0 to Files.Count - 1 do
                MZFCompressor.FileList.Add(Files[Index]);
              MZFCompressor.CreateArchive;
              MZFCompressor.Free;
           End;
    ftBH,ftGZip,ftLha,ftJar,ftCab,ftTar,ftZip:
        Begin
          UniversalCompressor:=TUniversalCompressor.Create;
          UniversalCompressor.ArchiveFile:=ArchiveFile;
          UniversalCompressor.FileType:=ArchCompressMethod;
          UniversalCompressor.OnBegin:=ArchiverOnBegin;
          UniversalCompressor.OnEnd:=ArchiverOnEnd;
          UniversalCompressor.OnProgress:=ArchiverProgress;
          UniversalCompressor.Password:=Password;
          For Index:=0 To Files.Count-1 Do
            UniversalCompressor.FileSpec.Add(Files[Index]);
          UniversalCompressor.EncodeHeader:=EncodeHead;
          UniversalCompressor.CompressValue:=TCompressValue(Level);
          UniversalCompressor.ReadingOnly:=ReadOnly;
          UniversalCompressor.Archived:=Archived;
          UniversalCompressor.Hidden:=Hidden;
          UniversalCompressor.Comment:=Comment;
          //
          UniversalCompressor.Compress;
          UniversalCompressor.Free;
        End;
    ft7z: Begin
            SevenZipCompressor:=TSevenZipCompressor.Create;
            SevenZipCompressor.FileName:=ArchiveFile;
            SevenZipCompressor.CompressLevel:=TCompressLevel(Level);
            For Index:=0 To Files.Count-1 Do
              SevenZipCompressor.FileSpec.Add(Files[Index]);
            SevenZipCompressor.OnBegin:=ArchiverOnBegin;
            SevenZipCompressor.OnEnd:=ArchiverOnEnd;
            SevenZipCompressor.OnProgress:=ArchiverProgress;
            SevenZipCompressor.Password:=Password;
            SevenZipCompressor.Archived:=Archived;
            SevenZipCompressor.ReadingOnly:=ReadOnly;
            SevenZipCompressor.Hidden:=Hidden;
            SevenZipCompressor.Compress;
            SevenZipCompressor.Free;
          End;
  end;
end;

procedure TConsoleManager.GetPass(Sender:TObject;Var Pass:ShortString);
Var Temp:String;
Begin
  Temp:='';
  If InputQuery('Password','Enter password',Temp) Then
    Pass:=Temp;
end;

procedure TConsoleManager.OnError(Sender:TObject;ErrorMsg:ShortString);
Var S:string;
Begin
  S:=ErrorMsg;
  MessageBox(Application.Handle,PChar(S),PChar('WinMZF'),MB_OK+MB_ICONEXCLAMATION);
End;

procedure TConsoleManager.RewritePromt(Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode);
Begin
  DlgOverWrite:=TDlgOverWrite.Create(Application);
  DlgOverWrite.Position:=poScreenCenter;
  DlgOverWrite.Label2.Caption:=Format('File name: %s',[ExtractFileName(AFile.LongFileName)]);
  DlgOverWrite.Label3.Caption:=Format('File size: %d',[AFile.Size]);
  DlgOverWrite.Label4.Caption:=Format('File attr: %s',[GetAttributesAsString(AFile.Attr)]);
  DlgOverWrite.Label5.Caption:=Format('File creation date: %s',[FormatDateTime(TimeFormat,AFile.FileCreateDate)]);
  DlgOverWrite.Label6.Caption:=Format('File modify date: %s',[FormatDateTime(TimeFormat,AFile.FileModifyDate)]);
  DlgOverWrite.Label7.Caption:=Format('File access date: %s',[FormatDateTime(TimeFormat,AFile.FileOpenDate)]);

  DlgOverWrite.ShowModal;
  If DlgOverWrite.OverWrite Then
    Begin
      If DlgOverWrite.All Then
        Mode:=omOverwriteAll Else
        Mode:=omRewrite;
    End Else
      Begin
        If DlgOverWrite.All Then
          Mode:=omSkipAll Else
          Mode:=omSkip;
      End;
  DlgOverWrite.Free;
End;


// MAIN PART OF APPLICATION
var FName:String;

procedure AnalyseParameters;
var Temp:String;
    Manager:TConsoleManager;
    ThrdCoder:TEncodeThread;
    Index:LongWord;
begin
  If ParamCount=0 Then Exit;
  Temp:=ParamStr(1);
  Case Temp[1] Of
    '-':Begin
        If UpperCase(Copy(Temp,1,2))='-A' Then
          Begin
            if ParamCount<2 then
              Begin
                Halt;
              End;
            dlgAddFiles:=TdlgAddFiles.Create(Application);
            if ExtractFileDir(ParamStr(2))='' then
              dlgAddFiles.cbFileName.Text:=GetStringOptions('Config','DefArchFolder',GetSystemPath(TSystemPath(0)))+'\'+ParamStr(2)
            else
              dlgAddFiles.cbFileName.Text:=ParamStr(2);
            If UpperCase(ExtractFileExt(ParamStr(2))) = '.MZF' Then
              dlgAddFiles.cbCompressMethod.ItemIndex:=0 Else
            If UpperCase(ExtractFileExt(ParamStr(2))) = '.7Z' Then
              dlgAddFiles.cbCompressMethod.ItemIndex:=1 Else
            If UpperCase(ExtractFileExt(ParamStr(2))) = '.ZIP' Then
              dlgAddFiles.cbCompressMethod.ItemIndex:=2 Else
            If UpperCase(ExtractFileExt(ParamStr(2))) = '.BH' Then
              dlgAddFiles.cbCompressMethod.ItemIndex:=3 Else
            If UpperCase(ExtractFileExt(ParamStr(2))) = '.LHA' Then
              dlgAddFiles.cbCompressMethod.ItemIndex:=4 Else
            If UpperCase(ExtractFileExt(ParamStr(2))) = '.JAR' Then
              dlgAddFiles.cbCompressMethod.ItemIndex:=5 Else
            If UpperCase(ExtractFileExt(ParamStr(2))) = '.CAB' Then
              dlgAddFiles.cbCompressMethod.ItemIndex:=6 Else
            If UpperCase(ExtractFileExt(ParamStr(2))) = '.TAR' Then
              dlgAddFiles.cbCompressMethod.ItemIndex:=7 Else
            dlgAddFiles.cbCompressMethod.ItemIndex:=0;
            dlgAddFiles.cbCompressMethodChange(dlgAddFiles.cbCompressMethod);
            if ParamCount>=3 then
              for Index := 3 to ParamCount  do
                Begin
                  {if ParamStr(Index)='*.*' then
                    dlgAddFiles.lvFiles.Items.Add.Caption:=}
                  dlgAddFiles.lvFiles.Items.Add.Caption:=ParamStr(Index);
                End;
            if DlgAddFiles.ShowModal=mrOk then
              Begin
                Manager.Files:=TStringList.Create;
                Manager.MZFLevel:=dlgAddFiles.InSideLevel;
                Manager.Comment:=dlgAddFiles.memComment.Text;
                Manager.Level:=dlgAddFiles.tbLevel.Position;
                for Index := 0 to dlgAddFiles.lvFiles.Items.Count - 1 do
                  Manager.Files.Add(dlgAddFiles.lvFiles.Items[Index].Caption);  
                Manager.CreateArchive(DlgAddFiles.cbFileName.Text,TFileType(dlgAddFiles.cbCompressMethod.ItemIndex),
                                      dlgAddFiles.edPassword.Text,dlgAddFiles.cbEncodeHead.Checked,
                                      dlgAddFiles.cbEncodeFile.Checked,dlgAddFiles.cbArchiveReadOnly.Checked,
                                      dlgAddFiles.cbArchiveArchived.Checked,dlgAddFiles.cbArchiveArchived.Checked);
                Manager.Files.Free;
              End;
            dlgAddFiles.Free;
            Halt;
          End;
        If UpperCase(Copy(Temp,1,2))='-X' Then
          Begin
            if ParamCount<2 then
              Begin
                Halt;
              End;
            If Not FileExists(ParamStr(2)) Then
              Begin
                Halt;
              End;
            If GetArchiveTypeBySignature(ParamStr(2))=ftAnyFile Then Halt;
            dlgExtract:=TdlgExtract.Create(Application);
            dlgExtract.cbDestDir.Text:=GetStringOptions('Config','DefExtractFolder',GetSystemPath(TSystemPath(0)));
            dlgExtract.Position:=poScreenCenter;
            dlgExtract.rgExtractMode.Items.Delete(1);
            dlgExtract.rgExtractMode.ItemIndex:=0;
            If dlgExtract.ShowModal=mrOk Then
              Begin
                Manager.Files:=TStringList.Create;
                If ParamCount>=3 Then
                  For Index:=3 To ParamCount Do
                    Manager.Files.Add(ParamStr(Index));
                Manager.ExtractArchive(ParamStr(2),dlgExtract.cbDestDir.Text,omUnknown,True,dlgExtract.rgDeleteArchive.ItemIndex=1,
                                       dlgExtract.cbUpdateModifyTime.Checked,dlgExtract.cbUpdateAccessTime.Checked);
                Manager.Files.Free;
              End;
            dlgExtract.Free;
            Halt;
          End;
        If (UpperCase(Copy(Temp,1,2))='-E')Or(UpperCase(Copy(Temp,1,2))='-D') Then //Encode
          Begin
            if ParamCount<2 then
              Begin
                Halt;
              End;
            dlgEncodeDecode:=TdlgEncodeDecode.Create(Application);
            if UpperCase(Copy(Temp,1,2))='-E' then
              dlgEncodeDecode.cbMode.ItemIndex:=0
            else
              dlgEncodeDecode.cbMode.ItemIndex:=1;
            dlgEncodeDecode.Caption:='WinMZF File encoder\decoder';
            If dlgEncodeDecode.ShowModal=mrOk Then
              Begin
                ThrdCoder:=TEncodeThread.Create(True);
                ThrdCoder.Initialize;
                ThrdCoder.Mode:=TEncodeMode(dlgEncodeDecode.cbMode.ItemIndex);
                ThrdCoder.PutIntoDir:=dlgEncodeDecode.cbCopyToDestDir.Checked;
                ThrdCoder.DestDir:=dlgEncodeDecode.edDestDir.Text;
                ThrdCoder.Password:=dlgEncodeDecode.edPassword.Text;
                ThrdCoder.Algorithm:=TEncodeAlgorithm(dlgEncodeDecode.cbAlgorithm.ItemIndex);
                ThrdCoder.ModifyExtention:=dlgEncodeDecode.cbSetFileExtention.Checked;
                ThrdCoder.Extention:=dlgEncodeDecode.edExtentions.Text;
                for Index := 2 to ParamCount do                  
                  ThrdCoder.FileList.Add(ParamStr(Index));
                ThrdCoder.Execute;
              End;
            dlgEncodeDecode.Free;
            Halt;
          End;
        If (UpperCase(Copy(Temp,1,2))='-T') Then
          Begin
            if ParamCount<2 then
              Begin
                Halt;
              End;
            if Not FileExists(ParamStr(2)) then
              Begin
                Halt;
              End;
            dlgTestArchive:=TdlgTestArchive.Create(Application);
            dlgTestArchive.ArchiveFile:=ParamStr(2);
            dlgTestArchive.ArcType:=GetArchiveTypeBySignature(ParamStr(2));
            dlgTestArchive.FileSpec.Add('*.*');
            dlgTestArchive.ShowModal;
            dlgTestArchive.Free;
            Halt;
          End;
      End;
    Else FName:=Temp;
  End;
end;

begin
  Application.Title := 'WinMZF';
  FName:='';
  AnalyseParameters;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TwndMessages,wndMessages);
  If (FName<>'') Then
    If FileExists(FName) Then
    Begin
      frmMain.WorkingArchive.Archive:=FName;
      frmMAin.WorkingArchive.ArcType:=GetArchiveTypeBySignature(FName);
      frmMain.WorkingArchive.FilterPath:='';
      frmMain.WorkingArchive.Opened:=True;
      frmMain.OpenFromConsole:=True;
      {frmMain.OpenArchive(False);}
    End;
  Application.Run;
end.
