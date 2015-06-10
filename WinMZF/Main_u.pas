unit Main_u;
                                   
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MZF_ShellCtrls_u, ToolWin, ComCtrls, XPMan, ExtCtrls, StdCtrls,
  ImgList, Advanced, Compress_u, Decompress_u, Viewer_u, ToolBarIcons_u, CommCtrl,
  SignatureDetect_u, Menus, RarView_u, LZMA_u, ArchView_u, UniversalCompressor_u,
  SevenZip_Compressor_u, Progress_u, Extract_u, UniversalUnpacker_u, RarUnit, ThrdEncode_u,
  ThrdDelete_u, ShellApi, ThrdCopy_u, ShellContextMenu, Registry, Favorite_u, IniFiles,
  Masks, Tips_u, Report_u, Constructor_u, Info_u;                  
                                     
function ChangeVolProc(ArcName:PChar; Mode:integer):integer; cdecl;  
procedure ShellOpenArchive(FileName:String;FType:TFileType);
                                                   
type                                                 
  TfrmMain = class(TForm)                              
    barMain: TCoolBar;                       
    MainButtons: TToolBar;
    barAddress: TToolBar;                           
    btnAdd: TToolButton;
    btnOpen: TToolButton;
    btnExtract: TToolButton;                               
    tbSeparator: TToolButton;
    sbMain: TStatusBar;
    imglistAddress: TImageList;
    DlgOpen: TOpenDialog;
    mnuInArchive: TMainMenu;
    itemFile: TMenuItem;
    DirTreeIcons: TImageList;
    Savebackupcopyas1: TMenuItem;
    N1: TMenuItem;
    Selectall1: TMenuItem;
    Deselectall1: TMenuItem;
    Invertselected1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Commands1: TMenuItem;
    Addfiles1: TMenuItem;
    Extract1: TMenuItem;
    Test1: TMenuItem;
    N4: TMenuItem;
    Executefile1: TMenuItem;
    Delete1: TMenuItem;
    Closearchive1: TMenuItem;
    New1: TMenuItem;
    BrowserIcons: TImageList;
    BrowserIconsBig: TImageList;
    mnuBrowser: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    New2: TMenuItem;
    N5: TMenuItem;
    Selectall2: TMenuItem;
    Deselectall2: TMenuItem;
    Invertselected2: TMenuItem;
    Openfile1: TMenuItem;
    Commands2: TMenuItem;
    Copyselected1: TMenuItem;
    Replaceselected1: TMenuItem;
    Deleteselected1: TMenuItem;
    N8: TMenuItem;
    Findfile1: TMenuItem;
    Search1: TMenuItem;
    Decode1: TMenuItem;
    Operations2: TMenuItem;
    Convertarchive1: TMenuItem;
    CreateSXF1: TMenuItem;
    Configure1: TMenuItem;
    Configurations1: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    Favorites1: TMenuItem;
    Addfavorite1: TMenuItem;
    Managefavorite1: TMenuItem;
    Tips1: TMenuItem;
    Content1: TMenuItem;
    N9: TMenuItem;
    About1: TMenuItem;
    Tips2: TMenuItem;
    Content2: TMenuItem;
    N10: TMenuItem;
    About2: TMenuItem;
    BigIcons: TImageList;
    btnDelete: TToolButton;
    Encode2: TMenuItem;
    Renamefile1: TMenuItem;
    N6: TMenuItem;
    Exit2: TMenuItem;
    btnCopy: TToolButton;
    btnReplace: TToolButton;
    btnInfo: TToolButton;
    N7: TMenuItem;
    Fileinfo1: TMenuItem;
    N11: TMenuItem;
    Fileinfo2: TMenuItem;
    N12: TMenuItem;
    Recent2: TMenuItem;
    N3: TMenuItem;
    Recent1: TMenuItem;
    N13: TMenuItem;
    Favorites2: TMenuItem;
    Homepage1: TMenuItem;
    Update1: TMenuItem;
    N14: TMenuItem;
    Homepage2: TMenuItem;
    Update2: TMenuItem;
    N15: TMenuItem;
    Setup1: TMenuItem;
    ImportExport1: TMenuItem;
    N16: TMenuItem;
    Filelist1: TMenuItem;
    hemes1: TMenuItem;
    Setup2: TMenuItem;
    ImportExport2: TMenuItem;
    N17: TMenuItem;
    FileList2: TMenuItem;
    hemes2: TMenuItem;
    List1: TMenuItem;
    Report1: TMenuItem;
    N18: TMenuItem;
    Smallicons1: TMenuItem;
    Report2: TMenuItem;
    N19: TMenuItem;
    List2: TMenuItem;
    Smallicons2: TMenuItem;
    N20: TMenuItem;
    Language1: TMenuItem;
    N21: TMenuItem;
    Language2: TMenuItem;
    Bigicons1: TMenuItem;
    Bigicons2: TMenuItem;
    dlgSave: TSaveDialog;
    btnCheck: TToolButton;
    procedure barAddressResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShellFilesListDblClick(Sender:TObject);
    procedure DirTreeChange(Sender: TObject; Node: TTreeNode);
    procedure btnGoToClick(Sender: TObject);
    procedure cbAddressKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnOpenClick(Sender: TObject);
    procedure OpenArchive(Reopen:Boolean);
    procedure ExtractArchive(Files:TStringList;DestDir:String;RewriteMode:TRewriteMode;ExtractAll,_Delete,UpdateModTime,UpdateAccessTime:Boolean);
    procedure ExecuteFile(FileName:String;ExtractAll:Boolean;Const WaitForClose:Boolean=False;Const Clean:Boolean=False);
    Procedure CloseArchive;
    procedure Closearchive1Click(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnLevelUpClick(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure Selectall2Click(Sender: TObject);
    procedure Deselectall2Click(Sender: TObject);
    procedure Deselectall1Click(Sender: TObject);
    procedure Invertselected2Click(Sender: TObject);
    procedure Invertselected1Click(Sender: TObject);
    procedure Decode1Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure Renamefile1Click(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure Replaceselected1Click(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure Selectall1Click(Sender: TObject);
    procedure Executefile1Click(Sender: TObject);
    procedure Managefavorite1Click(Sender: TObject);
    procedure Addfavorite1Click(Sender: TObject);
    procedure Findfile1Click(Sender: TObject);
    procedure Report2Click(Sender: TObject);
    procedure Setup1Click(Sender: TObject);
    procedure Openfile1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Tips1Click(Sender: TObject);
    procedure Savebackupcopyas1Click(Sender: TObject);
    procedure Test1Click(Sender: TObject);
    procedure Convertarchive1Click(Sender: TObject);
    procedure CreateSXF1Click(Sender: TObject);
    procedure Homepage1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure cbAddressChange(Sender:TObject);
  private
    { Private declarations }
    dlgProgress:TdlgProgress;
    dlgCaption,ResName:String;
    procedure LoadToolBarIcons;
    procedure ArchiverProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
    procedure ArchiverOnBegin(Sender:TObject);
    procedure ArchiverOnEnd(Sender:TObject);
    procedure DirTreeChanging(Sender: TObject; Node: TTreeNode;
                              var AllowChange: Boolean);
    procedure ViewerOnShowInfo(Sender:TObject;Var FileHead:TFileHead; Id:LongWord);
    procedure OnError(Sender:TObject;ErrorMsg:ShortString);
    procedure GetPass(Sender:TObject;Var Pass:ShortString);
    procedure GoToNode(SubDir:String);
    procedure RewritePromt(Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode);
    procedure RarExtract(ArcName:pchar;DestDir:String);
    procedure FilesListRenameFile(Sender: TObject; Item: TListItem;var S: String);
    procedure DeleteFiles(AFileList:TStringList);
    function GetRarArcInfo(FileName:String;Var FileCount:LongWord;Var ArcSize,UnPacked:Int64):Cardinal;
    procedure FilesListDblCLick(Sender:TObject);
    procedure FilesListKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure RecentClick(Sender:TObject);
    procedure FavClick(Sender:TObject);
    procedure KillRecent(Sender:TObject);
    procedure OnLangSelect(Sender:TObject);
    procedure OnSkinSelect(Sender:TObject);
    procedure ReadLanguage;
    procedure DecompressorOnExtract(Sender:TObject;_FileName:ShortString);
    procedure WMSYSCOMMAND(var Msg : TMessage);message WM_SYSCOMMAND;
  public
    ShellTree:TMadDirBrowser;
    ShellFilesList:TMadFileBrowser;
    cbAddress:TMZFDirCombo;
    Splitter:TSplitter;
    DirTree:TTreeView;
    FilesList:TListView;
    WorkingArchive:TWorkingFile;
    Comment:WideString;
    ShellDirectory:String;
    OpenFromConsole:Boolean;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  Recent,Favorites,LangList,SkinList:TStringList;
implementation

uses Add_u, OverWrite_u, EncodeDecode_u, FileInfo_u, Search_u,
  FindResult_u, Config_u, TestArchive_u, Convert_u, MakeSFX_u, About_u;

{$R *.dfm}
//{$R AVIS.res}
{$R Associate.res}

procedure TfrmMain.WMSYSCOMMAND(var Msg : TMessage);
begin
  if (msg.WParam=WM_USER+10) then
    begin
      About1Click(Nil);
    end;
  inherited;
 end;

Procedure WriteParams;
Var RegIni:TRegIniFile;
    Index:Byte;
Begin
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  RegIni.WriteBool('Main','Full_Screen',frmMain.WindowState=wsMaximized);
  If frmMain.WindowState<>wsMaximized Then
    Begin
      RegIni.WriteInteger('Main','Left',frmMain.Left);
      RegIni.WriteInteger('Main','Top',frmMain.Top);
      RegIni.WriteInteger('Main','Height',frmMain.Height);
      RegIni.WriteInteger('Main','Width',frmMain.Width);
    End;
  If Recent.Count<=15 Then
    RegIni.WriteInteger('Recent','Count',Recent.Count)
  Else
    RegIni.WriteInteger('Recent','Count',15);
  If Recent.Count<>0 Then
    For Index:=0 To Recent.Count-1 Do
      Begin
        If Index>=15 Then Break;
        RegIni.WriteString('Recent','File'+IntToStr(Index),Recent[Index]);
      End;
  RegIni.Free;
End;

Procedure ReadParams;
Var RegIni:TRegIniFile;
    FullScr:Boolean;
    RecentCount,Index:Byte;
Begin
  FullScr:=False;
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;

  frmMain.Left:=RegIni.ReadInteger('Main','Left',(Screen.Width Div 2)-(frmMain.Width Div 2));
  frmMain.Top:=RegIni.ReadInteger('Main','Top',(Screen.Height Div 2)-(frmMain.Height Div 2));
  frmMain.Height:=RegIni.ReadInteger('Main','Height',frmMain.Height);
  frmMain.Width:=RegIni.ReadInteger('Main','Width',frmMain.Width);

  FullScr:=RegIni.ReadBool('Main','Full_Screen',False);
  If FullScr Then frmMain.WindowState:=wsMaximized;
  RecentCount:=RegIni.ReadInteger('Recent','Count',0);
  If RecentCount>0 Then
    For Index:= 0 To RecentCount-1 Do
      Recent.Add(RegIni.ReadString('Recent','File'+IntToStr(Index),''));
  RegIni.Free;
End;

procedure ReadFavorites;
Var Fav:TIniFile;
    Count:Integer;
    Index:Integer;
    Item:TMenuItem;
Begin
  Fav:=TIniFile.Create(ExtractFileDir(Application.Exename)+'\Fav.ini');
  frmMain.Favorites2.Clear;
  Count:=Fav.ReadInteger('Main','Count',0);
  If Count>0 Then
    For Index:=0 To Count-1 Do
      Begin
        Item:=TMenuItem.Create(frmMain.Favorites2);
        Item.Caption:=Fav.ReadString('Favorite'+IntToStr(Index),'Caption','');
        Favorites.Add(Fav.ReadString('Favorite'+IntToStr(Index),'Pach','')+Fav.ReadString('Favorite'+IntToStr(Index),'Caption',''));
        Item.Tag:=Index;
        Item.OnClick:=frmMain.FavClick;
        frmMain.Favorites2.Add(Item);
      End;
  Fav.Free;
End;

procedure ReadConfig;
Begin
  case GetIntegerOptions('Config','ViewStyle',0) of
    0:Begin
        frmMain.ShellFilesList.ViewStyle:=vsReport;
        frmMain.FilesList.ViewStyle:=vsReport;
      End;
    1:Begin
        frmMain.ShellFilesList.ViewStyle:=vsList;
        frmMain.FilesList.ViewStyle:=vsList;
      End;
    2:Begin
        frmMain.ShellFilesList.ViewStyle:=vsSmallIcon;
        frmMain.FilesList.ViewStyle:=vsSmallIcon;
      End;
    3:Begin
        frmMain.ShellFilesList.ViewStyle:=vsIcon;
        frmMain.FilesList.ViewStyle:=vsIcon;
      End;
  end;
  frmMain.ShellFilesList.RowSelect:=GetBoolOptions('Config','RowSelect',True);
  frmMain.FilesList.RowSelect:=GetBoolOptions('Config','RowSelect',True);
  frmMain.MainButtons.ShowCaptions:=GetBoolOptions('Config','ShowToolBarCaptions',True);
End;

procedure TfrmMain.FavClick(Sender:TObject);
Begin
  With Sender As TMenuItem Do
    Begin
      WorkingArchive.FilterPath:=Format(RootCaption,[Caption]);
      WorkingArchive.Archive:=Favorites[Tag];
      WorkingArchive.ArcType:=GetArchiveTypeBySignature(WorkingArchive.Archive);
      WorkingArchive.Opened:=True;
      OpenArchive(False);
    End;
End;

Procedure TfrmMain.OpenArchive(Reopen:Boolean);
Var MZFViewer:TMZFViewer;
    RarViewer:TRarViewer;
    SevenZipViewer:TSevenZipViewer;
    Dirs,Files,TempList:TStringList;
    Index:LongWord;
    Temp:String;
    AType,AComment:String;
    Item:TListItem;
    FileInfo:TFileInfo;
    RecentIndex:Byte;
Begin
  //btnOpen.Caption:='Close';
  If Not Reopen Then
    Begin
      If Not WorkingArchive.Opened Then Exit;
      If Not FileExists(WorkingArchive.Archive) Then
        Begin
          CloseArchive;
          Exit;
        End;                   
      If WorkingArchive.ArcType=ftAnyFile Then
        Begin
          ShellOpenFile(Handle,WorkingArchive.Archive);
          WorkingArchive.Archive:='';
          WorkingArchive.ArcType:=ftAnyFile;
          WorkingArchive.FilterPath:='';
          WorkingArchive.Opened:=False;
          Exit;
        End;
      {If Not Reopen Then
        cbAddress.Items.Clear;}
      If Not Reopen Then
      If Not TextInList(WorkingArchive.Archive,Recent) Then
        Begin
          //Recent.Add(WorkingArchive.Archive);
          Recent.Add(WorkingArchive.Archive);
          TempList:=TStringList.Create;
          For RecentIndex:=0 To Recent.Count-1 Do
            TempList.Add(Recent[RecentIndex]);
          Recent.Clear;
          For RecentIndex:=TempList.Count-1 DownTo 0 Do
            Begin
              Recent.Add(TempList[RecentIndex]);
              //cbAddress.Items.Add(Recent[RecentIndex]);
            End;
          TempList.Free;
        End;
      {For RecentIndex:=0 To Favorites.Count-1 Do
        cbAddress.Items.Add(Favorites[RecentIndex]);}
      ShellDirectory:=cbAddress.Text;
      Caption:=Format(Title,[ExtractFileName(WorkingArchive.Archive)]);
      Application.Title:=Caption;
      Self.Menu:=mnuInArchive;

      DirTree.Items.Clear;
      FilesList.Items.Clear;

      ShellFilesList.Hide;
      ShellTree.Hide;

      DirTree.Show;
      FilesList.Show;
      Splitter.Left:=DirTree.Width+1;
      //Temp:=WorkingArchive.FilterPath;
      //Delete(Temp,1,Pos(']',Temp));
      //cbAddress.Text:=Temp;
      cbAddress.path:=workingarchive.Archive;
      cbAddress.Text:=WorkingArchive.Archive{+'\'};
      //Temp:='';
      //btnArcClose.Enabled:=True;
      btnOpen.ImageIndex:=7;
      btnOpen.Caption:=ReadFromLanguage('Buttons','btnClose','Close');
      btnCheck.Visible:=True;
      ////////////////////////
    End;
  FilesList.Items.Clear;
  Case WorkingArchive.ArcType Of
    ftMZF:Begin
            sbMain.Panels[1].Text:='MZF';
            MZFViewer:=TMZFViewer.Create;
            MZFViewer.ArchiveName:=WorkingArchive.Archive;
            MZFViewer.Path:=WorkingArchive.FilterPath;
            MZFViewer.OnShowFileINfo:=ViewerOnShowInfo;
            MZFViewer.OnError:=OnError;
            MZFViewer.OnPassQuest:=GetPass;
            MZFViewer.ShowFiles;
            FillTreeViewWithDirs(DirTree,MZFViewer.DirList);
            {If Not Reopen Then
            For Index:=0 To MZFViewer.DirList.Count-1 Do
              Begin
                Temp:=MZFViewer.DirList[Index];
                Delete(Temp,1,Pos(']',Temp));
                cbAddress.Items.Add(WorkingArchive.Archive+Temp);
              End; }
            Comment:=MZFViewer.Commentary;
            MZFViewer.Free;
          End;
    ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,ftArj,ftPKG5,ftACE2:
          Begin
            Dirs:=TStringList.Create;
            Files:=TStringList.Create;
            GetFilesAndDirs(Dirs,Files,WorkingArchive.Archive,AType,AComment);
            sbMain.Panels[1].Text:=AType;
            If Not Reopen Then
              Begin
                FillTreeViewWithDirs(DirTree,Dirs);
                Comment:=AComment;
              End;
            Temp:=WorkingArchive.FilterPath;
            Delete(Temp,1,Pos(']',Temp)+1);
            {If Not Reopen Then
            For Index:=0 To Dirs.Count-1 Do
              Begin
                Temp:=Dirs[Index];
                Delete(Temp,1,Pos(']',Temp));
                cbAddress.Items.Add(WorkingArchive.Archive+Temp);
              End;}
            //ShowMessage(temp);
            For Index:=0 To Files.Count-1 Do
              Begin
                //showmessage(files.Strings[index]);
                If ExtractFileDir(Files.Strings[Index])=Temp Then
                  Begin
                    Item:=FilesList.Items.Add;
                    GetFileInfo(WorkingArchive.Archive,Files.Strings[Index],FileInfo);
                    Item.Caption:=ExtractFileName(Files.Strings[Index]);
                    Item.SubItems.Add(IntToStr(FileInfo.Size));
                    Item.SubItems.Add(IntToStr(FileInfo.PackedSize));
                    Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.CreationTime));
                    Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.ModifyTime));
                    Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.AccessTime));
                    Item.SubItems.Add({GetAttributesAsString(FileInfo.Attr)}'NULL');
                    Item.SubItems.Add(IntToHex(FileInfo.CRC,8));
                    Item.SubItems.Add(ExtractFileDir(Files.Strings[Index]));
                  End;
              End;
          End;
    ftRar:Begin
            sbMain.Panels[1].Text:='RAR';
            RarViewer:=TRarViewer.Create;
            RarViewer.FileName:=WorkingArchive.Archive;
            RarViewer.OnShowFileInfo:=ViewerOnShowInfo;
            If Not Reopen Then
              Begin
                RarViewer.GetDirs;
                FillTreeViewWithDirs(DirTree,RarViewer.Dirs);
                Comment:=RarViewer.Comment;
              End;
            {If Not Reopen Then
            For Index:=0 To RarViewer.Dirs.Count-1 Do
              Begin
                Temp:=RarViewer.Dirs[Index];
                Delete(Temp,1,Pos(']',Temp));
                cbAddress.Items.Add(WorkingArchive.Archive+Temp);
              End; }
            Temp:=WorkingArchive.FilterPath;
            Delete(Temp,1,Pos(']',Temp)+1);
            RarViewer.GetFilesFromDir(Temp);
            RarViewer.Free;
          End;
    ft7Z:Begin
          sbMain.Panels[1].Text:='7Z';
          SevenZipViewer:=TSevenZipViewer.Create;
          SevenZipViewer.OnShowFileInfo:=ViewerOnShowInfo;
          SevenZipViewer.FileName:=WorkingArchive.Archive;
          If Not Reopen Then
            Begin
              SevenZipViewer.GetDirs;
              FillTreeViewWithDirs(DirTree,SevenZipViewer.Directories);
            End;
          {If Not Reopen Then
          For Index:=0 To SevenZipViewer.Directories.Count-1 Do
              Begin
                Temp:=SevenZipViewer.Directories[Index];
                Delete(Temp,1,Pos(']',Temp));
                cbAddress.Items.Add(WorkingArchive.Archive+Temp);
              End;}
          Temp:=WorkingArchive.FilterPath;
          Delete(Temp,1,Pos(']',Temp)+1);
          SevenZipViewer.GetFilesFromDir(Temp);
          SevenZipViewer.Free;
         End;
  End;
  Case WorkingArchive.ArcType Of
    ftMZF:
      Begin
        btnAdd.Enabled:=True;
        btnExtract.Enabled:=True;
        btnOpen.Enabled:=True;
        btnCopy.Enabled:=True;
        btnDelete.Enabled:=True;
        btnReplace.Enabled:=True;
        Addfiles1.Enabled:=True;
        RenameFile1.Enabled:=True;
        Test1.Enabled:=True;
        Delete1.Enabled:=True;
        FilesList.ReadOnly:=False;
      End;
    ft7z:
      Begin
        btnAdd.Enabled:=False;
        btnExtract.Enabled:=True;
        btnOpen.Enabled:=True;
        btnReplace.Enabled:=False;
        btnDelete.Enabled:=False;
        Addfiles1.Enabled:=False;
        RenameFile1.Enabled:=False;
        Test1.Enabled:=True;
        Delete1.Enabled:=False;
        FilesList.ReadOnly:=True;
      End;
    ftZip,ftBH,ftLha,ftJar,ftTar:
      Begin
        btnAdd.Enabled:=True;
        btnExtract.Enabled:=True;
        btnOpen.Enabled:=True;
        btnCopy.Enabled:=True;
        btnDelete.Enabled:=True;
        btnReplace.Enabled:=True;
        Addfiles1.Enabled:=True;
        RenameFile1.Enabled:=True;
        Test1.Enabled:=True;
        Delete1.Enabled:=True;
        FilesList.ReadOnly:=False;
      End;
    ftZoo,ftGZip,ftCab,ftArj,ftPKG5,ftACE2:
      Begin
        btnAdd.Enabled:=False;
        btnExtract.Enabled:=True;
        btnOpen.Enabled:=True;
        btnReplace.Enabled:=False;
        btnDelete.Enabled:=False;
        Addfiles1.Enabled:=False;
        RenameFile1.Enabled:=False;
        Test1.Enabled:=True;
        Delete1.Enabled:=False;
        FilesList.ReadOnly:=True;
      End;
    ftRar:
      Begin
        btnAdd.Enabled:=False;
        btnExtract.Enabled:=True;
        btnOpen.Enabled:=True;
        btnReplace.Enabled:=False;
        btnDelete.Enabled:=False;
        Addfiles1.Enabled:=False;
        RenameFile1.Enabled:=False;
        Test1.Enabled:=True;
        Delete1.Enabled:=False;
        FilesList.ReadOnly:=True;
      End;
  End;
End;

procedure TfrmMain.DecompressorOnExtract(Sender: TObject; _FileName: ShortString);
begin
  sbMain.Panels[2].Text:=Format(ReadFromLanguage('Status','Extracting','Extracting: %s'),[_FileName]);//Extracting: %s
end;

procedure TfrmMain.Openfile1Click(Sender: TObject);
begin
  if ShellFilesList.ItemIndex<>-1 then
    Begin
      if FileExists(ShellFilesList.Folders[ShellFilesList.ItemIndex].PathName) then
        ShellOpenFile(Handle,ShellFilesList.Folders[ShellFilesList.ItemIndex].PathName)
      Else
        if dlgOpen.Execute(Handle) then
          ShellOpenFile(Handle,dlgOpen.FileName);
    End else
    Begin
      dlgOpen.FilterIndex:=14;
      if dlgOpen.Execute(Handle) then
        ShellOpenFile(Handle,dlgOpen.FileName);
      dlgOpen.FilterIndex:=0;
    End;
end;

Procedure TfrmMain.CloseArchive;
var Item:TMenuItem;
    Index:Byte;
Begin
  //btnOpen.Caption:='Open';
  //cbAddress.Items.Clear;
  sbMain.Panels[1].Text:='Browser';
  WorkingArchive.Archive:='';
  WorkingArchive.ArcType:=ftAnyFile;
  WorkingArchive.FilterPath:='';
  WorkingArchive.Opened:=False;
  Caption:=EmptyTitle;
  Application.Title:=Caption;

  DirTree.Hide;
  FilesList.Hide;
  ShellTree.Show;
  ShellFilesList.Show;
  Splitter.Left:=ShellTree.Width+1;

  cbAddress.Text:=ShellDirectory;
  Self.Menu:=mnuBrowser;
  btnAdd.Enabled:=True;
  btnExtract.Enabled:=True;
  btnOpen.Enabled:=True;
  Recent1.Clear;
  Recent2.Clear;
  If Recent.Count>0 Then
  For Index:=0 To Recent.Count-1 Do
    Begin
      //cbAddress.Items.Add(Recent[Index]);
      Item:=TMenuItem.Create(Recent2);
      Item.Caption:=ExtractFileName(Recent[Index]);
      Item.Tag:=Index;
      Item.OnClick:=RecentClick;
      recent2.Add(Item);

      //
      Item:=TMenuItem.Create(Recent1);
      Item.Caption:=ExtractFileName(Recent[Index]);
      Item.Tag:=Index;
      Item.OnClick:=RecentClick;
      recent1.Add(Item);
    End;
  Item:=TMenuItem.Create(Recent1);
  Item.Caption:='-';
  Recent1.Add(Item);

  Item:=TMenuItem.Create(Recent1);
  Item.Caption:=ReadFromLanguage('Menu','mnuClear','Clear');
  Item.OnClick:=KillRecent;
  Recent1.Add(Item);

  Item:=TMenuItem.Create(Recent2);
  Item.Caption:='-';
  Recent2.Add(Item);

  Item:=TMenuItem.Create(Recent2);
  Item.Caption:=ReadFromLanguage('Menu','mnuClear','Clear');
  Item.OnClick:=KillRecent;
  Recent2.Add(Item);
  //btnArcClose.Enabled:=False;
End;

procedure TfrmMain.ExecuteFile(FileName:String;ExtractAll:Boolean;Const WaitForClose:Boolean=False;Const Clean:Boolean=False);
var Files:TStringList;
    Dir:String;
Begin
  Files:=TStringList.Create;
  Files.Add(FileName);
  MakeDir(GetTempDir);
  ExtractArchive(Files,GetTempDir,omOverwriteAll,ExtractAll,False,False,False);
  Dir:=GetTempDir;
  if Dir[Length(Dir)]<>'\' then Dir:=Dir+'\';
  ShellExecuteFile(Handle,Dir+FileName,WaitForClose);
  If Clean Then RemoveFileTree(GetTempDir);
  Files.Free;
End;

function ChangeVolProc(ArcName:PChar; Mode:integer):integer;
var s:string;
    p:pchar;
begin
  ChangeVolProc:=1;
  s:=ReadFromLanguage('Messages','NextVolume','Insert next volume');
  p:=@s[1];
  if (Mode=RAR_VOL_ASK) then
  begin
    if Application.messagebox(p,'WinMZF',mb_okcancel)=idcancel
       then ChangeVolProc:=0
  end;
end;

function TfrmMain.GetRarArcInfo(FileName:String;Var FileCount:LongWord;Var ArcSize,UnPacked:Int64):Cardinal;
var
  hArcData:THandle;
  RHCode,PFCode:integer;
  CmtBuf:array[1..16384] of char;
  HeaderData:RARHeaderData;
  OpenArchiveData:RAROpenArchiveData;
  FileStream:TFileStream;
begin
  LoadDll(hInstance);
  FileStream:=TFileStream.Create(FileName,fmOpenRead);
  ArcSize:=FileStream.Size;
  FileStream.Free;
  OpenArchiveData.ArcName:=PChar(FileName);
  OpenArchiveData.CmtBuf:=@CmtBuf[1];
  OpenArchiveData.CmtBufSize:=sizeof(CmtBuf);
  OpenArchiveData.OpenMode:=RAR_OM_EXTRACT;
  hArcData:=RAROpenArchive(OpenArchiveData);
  if (OpenArchiveData.OpenResult<>0) then
     begin
      //MessageBox(Handle,'Archive is corrupt!','Error',MB_OK+MB_ICONEXCLAMATION);
      DisplayMessage(ReadFromLanguage('Messages','Corrupt','Archive is corrupt!'),IT_INFO);
      dlgProgress.Close;
      dlgProgress.Free;           
      Self.Enabled:=True;                      
      RARCloseArchive(hArcData);
      exit;                              
     end;
  RARSetChangeVolProc(hArcData,ChangeVolProc);
  HeaderData.CmtBuf:=NIL;
  {Maximum:=0;
  GlobalUnPackSize:=0; }
  FileCount:=0;
  UnPacked:=0;
  While RARReadHeader(hArcData, HeaderData) <> ERAR_END_ARCHIVE do
    Begin
      RARProcessFile(hArcData,RAR_OM_LIST,Nil,Nil);
      If (HeaderData.FileAttr Xor faDirectory=0)Then Continue;
      //Inc(Maximum, HeaderData.PackSize);
      Inc(FileCount);
      //Inc(ArcSize,HeaderData.PackSize);
      Inc(UnPacked,HeaderData.UnpSize);
    End;
  Result:=headerdata.UnpVer;
  RARCloseArchive(hArcData);
  FreeDll;
end;

procedure TfrmMain.RarExtract(ArcName:pchar;DestDir:String);
var
  hArcData:THandle;
  RHCode,PFCode:integer;
  CmtBuf:array[1..16384] of char;
  HeaderData:RARHeaderData;
  OpenArchiveData:RAROpenArchiveData;
  {GlobalUnPackSize,Maximum,}FileCount,Current:LongWord;
begin
  LoadDll(hInstance);
  Self.Enabled:=False;
  Application.CreateForm(TdlgProgress,dlgProgress);
  //dlgProgress.pbArchive.Hide;
  dlgProgress.pbFile.Hide;
  //dlgProgress.Anime.ResName:='DECOMPRESSING';
  //dlgProgress.Anime.Active:=True;
  dlgProgress.Show;
  dlgProgress.Update;
  OpenArchiveData.ArcName:=ArcName;
  OpenArchiveData.CmtBuf:=@CmtBuf[1];
  OpenArchiveData.CmtBufSize:=sizeof(CmtBuf);
  OpenArchiveData.OpenMode:=RAR_OM_EXTRACT;
  hArcData:=RAROpenArchive(OpenArchiveData);
  if (OpenArchiveData.OpenResult<>0) then
     begin
      //MessageBox(Handle,'Archive is corrupt!','Error',MB_OK+MB_ICONEXCLAMATION);
      DisplayMessage(ReadFromLanguage('Messages','Corrupt','Archive is corrupt!'),IT_INFO);
      dlgProgress.Close;
      dlgProgress.Free;
      Self.Enabled:=True;
      RARCloseArchive(hArcData);
      exit;
     end;
  RARSetChangeVolProc(hArcData,ChangeVolProc);
  HeaderData.CmtBuf:=NIL;
  {Maximum:=0;
  GlobalUnPackSize:=0; }
  FileCount:=0;
  Current:=0;
  While RARReadHeader(hArcData, HeaderData) <> ERAR_END_ARCHIVE do
    Begin
      If Not(GetBoolOptions('Config','EnableSecurity',False))Then
        If (Not NameInMask(GetStringOptions('Config','ExcludeExt','*.exe;*.com;*.bat'),HeaderData.FileName)) Then
        Begin
          RARProcessFile(hArcData,RAR_OM_LIST,Nil,Nil);
          //Inc(Maximum, HeaderData.PackSize);
        End;
      Inc(FileCount);
    End;
  hArcData:=RAROpenArchive(OpenArchiveData);
  RARSetChangeVolProc(hArcData,ChangeVolProc);
  HeaderData.CmtBuf:=NIL;
  RHCode:=RARReadHeader(hArcData,HeaderData);
  {Inc(GlobalUnPackSize, HeaderData.UnpSize);}
  Inc(Current);
  while RHCode<>ERAR_END_ARCHIVE do
  begin
    If dlgProgress.Abort Then     
      Begin
        dlgProgress.Close;
        dlgProgress.Free;
        Self.Enabled:=True;
        RARCloseArchive(hArcData);
        Exit;
      End;
    If (Not GetBoolOptions('Config','EnableSecurity',False))Then
        If (Not NameInMask(GetStringOptions('Config','ExcludeExt','*.exe;*.com;*.bat'),HeaderData.FileName)) Then
          PFCode:=RARProcessFile(hArcData,RAR_EXTRACT,PChar(DestDir),Nil);
    {Inc(GlobalUnPackSize, HeaderData.PackSize);}
    Inc(Current);
    if PFCode<>0 then
       begin
        //MessageBox(Handle,'Coud not open file','Error',MB_OK+MB_ICONSTOP);
        DisplayMessage(ReadFromLanguage('Messages','CantOpen','Can not open file as archive'),IT_ERROR);
        dlgProgress.Close;
        dlgProgress.Free;
        Self.Enabled:=True;
        RARCloseArchive(hArcData);
        exit;
       end;
    RHCode:=RARReadHeader(hArcData,HeaderData);
    {dlgProgress.pbFile.Position:=GetPercentDone(0,GlobalUnPackSize,Maximum);}
    dlgProgress.pbArchive.Position:=GetPercentDone(0,Current,FileCount);
    dlgProgress.lbFileIndex.Caption:=Format(ReadFromLanguage('Status','FileIndex','File index %d'),[Current]);//File index %d
    dlgProgress.lbCurrent.Caption:='';
    dlgProgress.lbTotal.Caption:=Format(ReadFromLanguage('Status','TotalProgress','Total progress %d'),[dlgProgress.pbArchive.Position]);
    dlgProgress.Update;
    Application.ProcessMessages;
  end;
  if (RHCode=ERAR_BAD_DATA) then
    //MessageBox(Handle,'Archive is corrupt','Attention',MB_OK+MB_ICONEXCLAMATION);
    DisplayMessage(ReadFromLanguage('Messages','Corrupt','Archive is corrupt'),IT_INFO);
  RARCloseArchive(hArcData);
  dlgProgress.Close;
  dlgProgress.Free;
  Self.Enabled:=True;
  FreeDll;
End;

procedure ShellOpenArchive(FileName:String;FType:TFileType);
Begin
  frmMain.WorkingArchive.Archive:=FileName;
  frmMain.WorkingArchive.ArcType:=FType;
  frmMain.WorkingArchive.FilterPath:=Format(RootCaption,[ExtractFileName(FileName)]);
  frmMain.WorkingArchive.Opened:=True;
  frmMain.OpenArchive(False);
End;

procedure TfrmMain.DeleteFiles(AFileList:TStringList);
Var MZFViewer:TMZFViewer;
    Index:LongWord;
    DelFileList:TStringList;
    Temp:String;
Begin   
  Case WorkingArchive.ArcType Of
    ftMZF:
      Begin
        MZFViewer:=TMZFViewer.Create;
        MZFViewer.ArchiveName:=WorkingArchive.Archive;
        MZFViewer.DeleteFiles(AFileList);
        MZFViewer.Free;
      End;
    ftZip,ftBH,ftLha,ftJar,ftTar:
      Begin
        DelFileList:=TStringList.Create;
        For Index:=0 To AFileList.Count-1 Do
          Begin
            Temp:=AFileList.Strings[Index];
            Delete(Temp,1,1);
            DelFileList.Add(Temp);
          End;
        ZTVDeleteFiles(WorkingArchive.Archive,DelFileList,WorkingArchive.ArcType);
        DelFileList.Free;
        Temp:='';
      End;
  End;
  OpenArchive(True);
End;

procedure TfrmMain.FilesListRenameFile(Sender: TObject; Item: TListItem;
  var S: String);
Var Viewer:TMZFViewer;
    UnPacker:TUnPacker;
    UniversalCompressor:TUniversalCompressor;
    Temp,TempDir:String;
    List:TStringList;
Begin
  If (Not WorkingArchive.Opened)Then Exit;
  Case WorkingArchive.ArcType Of
    ftMZF:
      Begin
        Viewer:=TMZFViewer.Create;
        Viewer.ArchiveName:=WorkingArchive.Archive;
        Temp:=cbAddress.Text+'\';
        Viewer.RenameFile(Temp+FilesList.Items.Item[FilesList.ItemIndex].Caption,Temp+S);
        Viewer.Free;
      End;
    ftZip,ftBH,ftLha,ftJar,ftTar:
      Begin
       UnPacker:=TUnPacker.Create;
       UnPacker.ArchiveFile:=WorkingArchive.Archive;
       UnPacker.ArchiveType:=WorkingArchive.ArcType;
       TempDir:=GetTempDir;
       UnPacker.DestinationDir:=TempDir;
       Temp:=FilesList.Items.Item[FilesList.ItemIndex].SubItems.Strings[7]+'\'+
              FilesList.Items.Item[FilesList.ItemIndex].Caption;
       Delete(Temp,1,Pos(']',Temp));     //???? UnKnow +1 or not
       UnPacker.FileSpec.Add(Temp);
       UnPacker.Extract;
       UnPacker.Free;
       List:=TStringList.Create;
       List.Add(Temp);
       ZTVDeleteFiles(WorkingArchive.Archive,List,WorkingArchive.ArcType);
       List.Free;
       RenameFile(TempDir+temp,ExtractFileDir(TempDir+temp)+'\'+S);
       UniversalCompressor:=TUniversalCompressor.Create;
       UniversalCompressor.ArchiveFile:=WorkingArchive.Archive;
       UniversalCompressor.FileType:=WorkingArchive.ArcType;
       UniversalCompressor.FileSpec.Add(TempDir+'\*.*');
       UniversalCompressor.Compress;
       UniversalCompressor.Free;
       FileSetAttr(ExtractFileDir(TempDir+temp)+'\'+S,0);
       DeleteFile(ExtractFileDir(TempDir+temp)+'\'+S);
       RemoveFileTree(TempDir);
      End;
  End;

End;

procedure TfrmMain.FilesListDblCLick(Sender:TObject);
var IsPe:Boolean;
    Ext:String;
    Temp:String;
    a:Word;
Begin
  If FilesList.SelCount<=0 Then
    Exit;
  Ext:=ExtractFileName(FilesList.Items.Item[FilesList.ItemIndex].Caption);
  {IsPE:=(Ext='.EXE')Or(Ext='.BAT')Or(Ext='.COM')Or(Ext='.SCR')Or(Ext='.HTM')Or(Ext='.HTML');}
  IsPe:=NameInMask(GetStringOptions('Config','UnpackFor','*.exe;*.com;*.htm;*.bat;*.html'),Ext);
  Temp:=FilesList.Items.Item[FilesList.ItemIndex].SubItems[7];
  a:=Pos(']',Temp);
  If a<>0 Then
    Delete(Temp,1,a);
  ExecuteFile(Temp+'\'+FilesList.Items.Item[FilesList.ItemIndex].Caption
    ,IsPE,IsPE,IsPE);
End;

procedure TfrmMAin.FilesListKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
Begin
  If Key=VK_RETURN Then
    FilesListDblCLick(Sender);
  If Key=VK_DELETE Then                                      
    btnDeleteClick(Sender);
End;

procedure TfrmMain.ViewerOnShowInfo(Sender:TObject;Var FileHead:TFileHead; Id:LongWord);
Var Item:TListItem;
Begin
  Item:=FilesList.Items.Add;
  Item.ImageIndex:=0;
  Item.Caption:=ExtractFileName(FileHead.LongFileName);
  Item.SubItems.Add(IntToStr(FileHead.Size));
  Item.SubItems.Add(IntToStr(FileHead.PackedSize));
  Item.SubItems.Add(FormatDateTime(TimeFormat,FileHead.FileCreateDate));
  Item.SubItems.Add(FormatDateTime(TimeFormat,FileHead.FileModifyDate));
  Item.SubItems.Add(FormatDateTime(TimeFormat,FileHead.FileOpenDate));
  Item.SubItems.Add(GetAttributesAsString(FileHead.Attr));
  Item.SubItems.Add(IntToHex(FileHead.CRC,8));
  Item.SubItems.Add(ExtractFileDir(FileHead.LongFileName));
  Item.SubItems.Add(IntToStr(Id));
End;

procedure TfrmMain.OnError(Sender:TObject;ErrorMsg:ShortString);
Var S:string;
Begin
  S:=ErrorMsg;
  //MessageBox(Handle,PChar(S),PChar('WinMZF'),MB_OK+MB_ICONEXCLAMATION);
  DisplayMessage(S,IT_INFO);
End;

procedure TfrmMain.GetPass(Sender:TObject;Var Pass:ShortString);
Var Temp:String;
Begin
  Temp:='';
  If InputQuery('WinMZF',ReadFromLanguage('Messages','PassWord','Enter password'),Temp) Then
    Pass:=Temp
End;

procedure TfrmMain.RewritePromt(Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode);
Begin
  Application.CreateForm(TDlgOverWrite,DlgOverWrite);
  DlgOverWrite.Label2.Caption:=Format(ReadFromLanguage('Status','FileName','File name: %s'),[ExtractFileName(AFile.LongFileName)]);
  DlgOverWrite.Label3.Caption:=Format(ReadFromLanguage('Status','FileSize','File size: %d'),[AFile.Size]);
  DlgOverWrite.Label4.Caption:=Format(ReadFromLanguage('Status','FileAttr','File atte: %s'),[GetAttributesAsString(AFile.Attr)]);
  DlgOverWrite.Label5.Caption:=Format(ReadFromLanguage('Status','Create','File creation time: %s'),[FormatDateTime(TimeFormat,AFile.FileCreateDate)]);
  DlgOverWrite.Label6.Caption:=Format(ReadFromLanguage('Status','Modify','File modify time: %s'),[FormatDateTime(TimeFormat,AFile.FileModifyDate)]);
  DlgOverWrite.Label7.Caption:=Format(ReadFromLanguage('Status','Access','File access time: %s'),[FormatDateTime(TimeFormat,AFile.FileOpenDate)]);

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

procedure TfrmMain.ArchiverProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
Begin
  dlgProgress.pbArchive.Position:=AProgress;
  dlgProgress.pbFile.Position:=FProgress;
  dlgProgress.lbFileIndex.Caption:='';
  dlgProgress.lbCurrent.Caption:=Format(ReadFromLanguage('Status','CurrentProgress','Current progress %d'),[FProgress])+'%';
  dlgProgress.lbTotal.Caption:=Format(ReadFromLanguage('Status','TotalProgress','Total progress %d'),[AProgress])+'%';
  sbMain.Panels[0].Text:=Format(ReadFromLanguage('Status','Progress','Progress %d'),[AProgress])+'%';
  Abort:=dlgProgress.Abort;
  dlgProgress.Update;
  //Application.ProcessMessages;
End;

procedure TfrmMain.ArchiverOnBegin(Sender:TObject);
Begin
  sbMain.Panels[0].Text:=ReadFromLanguage('Status','Prepare','Prepare to work');
  Application.CreateForm(TdlgProgress,dlgProgress);
  Self.Enabled:=False;
  dlgProgress.Show;
  dlgProgress.Caption:=dlgCaption;
  //dlgProgress.Anime.ResName:=ResName;
  //dlgProgress.Anime.Active:=True;
  dlgProgress.Update;
  dlgProgress.pbArchive.Position:=0;
  dlgProgress.pbFile.Position:=0;
End;

procedure TfrmMain.ArchiverOnEnd(Sender:TObject);
Begin
  sbMain.Panels[0].Text:=ReadFromLanguage('Status','Finalizing','Finalizing');
  Self.Enabled:=True;
  dlgProgress.Hide;
  dlgProgress.Free;
  sbMain.Panels[0].Text:=ReadFromLanguage('Status','Ready','Ready');
End;

Procedure TfrmMain.DirTreeChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
Var Temp:String;
Begin
  WorkingArchive.FilterPath:=GetPathToNode(Node,'\');
  Temp:=WorkingArchive.FilterPath;
  Delete(Temp,1,Pos(']',Temp));
  cbAddress.Text:=WorkingArchive.Archive+Temp;
  OpenArchive(True);
End;

procedure TfrmMain.LoadToolBarIcons;
Label SetIcons;
Var Temp:TStream;
    Icon:TIcon;
    BitMap:TBitmap;
    himlIconsBIG: HIMAGELIST;
    hIconLib: HModule;
    Index:Byte;
    _size:TSize;
Begin
  HIconLib:=hInstance;
  If (Not GetBoolOptions('Main','UseSkin',False))Or
    (Not FileExists(GetStringOptions('Main','Skin',''))) Then
  begin
  himlIconsBig := ImageList_Create(48, 48, ILC_COLOR32 or ILC_MASK, 0, 0);
  For Index:=0 To 8 do
    Begin
      Temp:=TMemoryStream.Create;
      Case Index Of
        0:Temp.Write(b_add_ondata,SizeOf(b_add_ondata));
        1:Temp.Write(b_opn_ondata,SizeOf(b_opn_ondata));
        2:Temp.Write(b_ext_ondata,SizeOf(b_ext_ondata));
        3:Temp.Write(b_del_ondata,SizeOf(b_del_ondata));
        4:Temp.Write(b_copy_ondata,SizeOf(b_copy_ondata));
        5:Temp.Write(b_replace_ondata,SizeOf(b_replace_ondata));
        6:Temp.Write(b_info_ondata,SizeOf(b_info_ondata));
        7:Temp.Write(b_close_ondata,SizeOf(b_close_ondata));
        8:Temp.Write(b_testdata,SizeOf(b_testdata));
      End;
      Temp.Position:=0;
      Icon:=TIcon.Create;
      Icon.LoadFromStream(Temp);
      //Icon.SaveToFile('c:\'+inttostr(index)+'.ico');
      ImageList_AddIcon(himlIconsBIG,Icon.Handle);
      Icon.Free;
      Temp.Free;
    End;
  end
    Else
    Begin //Load icons from skin pack
      if Not FileExists(GetStringOptions('Main','Skin','')) then
        GoTo SetIcons;      
      _size:=GetImgSize(GetStringOptions('Main','Skin',''));
      MainButtons.Images:=Nil;
      BigIcons.Height:=_size.Height;
      BigIcons.Width:=_size.Width;
      himlIconsBig := ImageList_Create(_size.Height, _size.Width, ILC_COLOR32 or ILC_MASK, 0, 0);
      //MainButtons.Height:=btnAdd.Height;
      For Index:=0 To 8 Do
        Begin
          Temp:=TMemoryStream.Create;
          GetDataByIndex(GetStringOptions('Main','Skin',''),Index,Temp);
          Temp.Position:=0;
          Icon:=TIcon.Create;
          Icon.LoadFromStream(Temp);
          ImageList_AddIcon(himlIconsBIG,Icon.Handle);
          Icon.Free;
          Temp.Free;
        End;
    End;
  SetIcons:;
  BigIcons.Handle:=himlIconsBIG;
  MainButtons.Images:=BigIcons;
  MainButtons.Height:=btnAdd.Height;
  barMain.Height:=MainButtons.Height+barAddress.Height+8;
End;

procedure TfrmMain.ShellFilesListDblClick(Sender:TObject);
Begin
  //cbAddress.Text:=ShellFilesList.RootFolder.PathName;
End;

procedure TfrmMain.DirTreeChange(Sender: TObject; Node: TTreeNode);
Begin
  If (Not WorkingArchive.Opened)And(DirectoryExists(ShellTree.Path)) Then
    begin
      //cbAddress.Text:=ShellTree.Path;
      cbAddress.Path:=ShellTree.Path;
    end;
End;

procedure TfrmMain.barAddressResize(Sender: TObject);
begin
  cbAddress.Width:=barAddress.Width-52;
end;

procedure TfrmMain.RecentClick(Sender:TObject);
Begin
  With Sender As TMenuItem Do
    Begin
      //Closearchive1Click(Sender);
      WorkingArchive.FilterPath:=Format(RootCaption,[Caption]);
      WorkingArchive.Archive:=Recent[Tag];
      WorkingArchive.ArcType:=GetArchiveTypeBySignature(WorkingArchive.Archive);
      WorkingArchive.Opened:=True;
      OpenArchive(False);
    End;
End;

Procedure TfrmMain.KillRecent(Sender:TObject);
Var RegIni:TRegIniFile;
    Index:Byte;
    Item:TMenuItem;
Begin
  If MessageBox(Handle,Pchar(ReadFromLanguage('Messages','Shure','Are you shure?')),'WinMZF',MB_YESNO+MB_ICONQUESTION)<>IDYES Then Exit;
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  RegIni.DeleteKey('Recent','Count');
  //cbAddress.Items.Clear;
  For Index:= 0 To 15 Do
    RegIni.DeleteKey('Recent','File'+IntToStr(Index));
  Recent1.Clear;
  Recent2.Clear;
  RegIni.Free;
  Recent.Clear;

  Item:=TMenuItem.Create(Recent1);
  Item.Caption:=ReadFromLanguage('Menu','mnuClear','Clear');
  Item.OnClick:=KillRecent;
  Recent1.Add(Item);

  Item:=TMenuItem.Create(Recent2);
  Item.Caption:=ReadFromLanguage('Menu','mnuClear','Clear');
  Item.OnClick:=KillRecent;
  Recent2.Add(Item);
End;

procedure TfrmMain.OnLangSelect(Sender: TObject);
var temp:String;
begin
  With Sender As TMenuItem Do
    Begin
      Temp:=Caption;
      System.Delete(Temp,1,1);
      SetStringOptions('Config','LanguageFile',
        ExtractFileDir(Application.ExeName)+'\Lang\'+Temp+'.ini');
    End;
  ReadLanguage;
end;

procedure TfrmMain.OnSkinSelect(Sender: TObject);
var Temp:String;
    //SkinInfo:String;
    Author,Version,Create,Comment:String;
    Height,Width:Byte;
    procedure DeleteAnd(Var Text:String);
    var Ind:Byte;
    begin
      Ind:=1;
      while Ind<Length(Text) do
        begin
          If Text[Ind]='&' Then
            begin
              Delete(Text,Ind,1);
              Dec(Ind);
            end;
          Inc(Ind);
        end;                       
    end;
begin
  If (Sender As TMenuItem).Tag=0 Then
    begin
      SetBoolOptions('Main','UseSkin',False);
      LoadToolBarIcons;
      Exit;
    end;
  Temp:=(Sender As TMenuItem).Caption;
  DeleteAnd(Temp);
  SetBoolOptions('Main','UseSkin',True);
  SetStringOptions('Main','Skin',
      ExtractFileDir(Application.ExeName)+'\Skins\'+
      Temp+'.msk');
  //Show Skin Info
  GetSkinInfo(ExtractFileDir(Application.ExeName)+'\Skins\'+
      Temp+'.msk',Author,Version,Create,Comment,Height,Width);
      
  Application.CreateForm(TdlgInfo,dlgInfo);
  dlgInfo.leFileName.Text:=ExtractFileName(ExtractFileDir(Application.ExeName)+'\Skins\'+
      Temp+'.msk');
  dlgInfo.leAuthor.Text:=Author;
  dlgInfo.leVersion.Text:=Version;
  dlgInfo.leDate.Text:=Create;
  dlgInfo.memComment.Text:=Comment;
  dlgInfo.ShowModal;
  dlgInfo.Free;
  LoadToolBarIcons;
end;

procedure TfrmMain.cbAddressChange(Sender:TObject);
begin
  If DirectoryExists(cbAddress.Path)And(Not fileexists(ExtractArchiveNameExtended(cbAddress.Text))) Then
    {begin}
      if WorkingArchive.Opened then
        begin
          CloseArchive;
          btnOpen.ImageIndex:=1;
          btnCheck.Visible:=False;
          btnOpen.Caption:=ReadFromLanguage('Buttons','btnOpen',btnOpen.Caption);
        end;
    {end;}{ else
    begin
      ShellTree.Path:=cbAddress.Path;
    end; }
  btnGoToClick(Sender);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
Var Column:TListColumn;
    Index:Byte;
    Item:TMenuItem;
    btnLevelUp,btnGoTo:TToolButton;
begin
  appendmenu(getsystemmenu(handle,false),mf_string,sc_separator,nil);
  appendmenu(getsystemmenu(handle,false),mf_string,WM_USER+10,PChar(ReadFromLanguage('Menu','mnuAbout',About1.Caption)));
                                        
  cbAddress:=TMZFDirCombo.Create(Self);
  cbAddress.Parent:=barAddress;
  cbAddress.OnKeyDown:=cbAddressKeyDown;
  cbAddress.OnCloseUp:=cbAddressChange;
  cbAddress.OnSelect:=cbAddressChange;

  OpenFromConsole:=False;
  ReadLanguage;
  If Not FileExists(ReadFromLanguage('Help','HelpFile','English.chm')) Then
    Begin
      Content1.Enabled:=False;
      Content2.Enabled:=False;
    End;
  Recent:=TStringList.Create;
  Favorites:=TStringList.Create;
  ReadFavorites;
  ReadParams;
  dlgCaption:=ReadFromLanguage('Status','Progress','Progress');
  If Recent.Count>0 Then
  For Index:=0 To Recent.Count-1 Do
    Begin
      Item:=TMenuItem.Create(Recent2);
      Item.Caption:=ExtractFileName(Recent[Index]);
      Item.Tag:=Index;
      Item.OnClick:=RecentClick;
      recent2.Add(Item);

      Item:=TMenuItem.Create(Recent1);
      Item.Caption:=ExtractFileName(Recent[Index]);
      Item.Tag:=Index;
      Item.OnClick:=RecentClick;
      recent1.Add(Item);
    End;
  Item:=TMenuItem.Create(Recent1);
  Item.Caption:='-';
  Recent1.Add(Item);

  Item:=TMenuItem.Create(Recent1);
  Item.Caption:=ReadFromLanguage('Menu','mnuClear','Clear');
  Item.OnClick:=KillRecent;
  Recent1.Add(Item);

  Item:=TMenuItem.Create(Recent2);
  Item.Caption:='-';
  Recent2.Add(Item);

  Item:=TMenuItem.Create(Recent2);
  Item.Caption:=ReadFromLanguage('Menu','mnuClear','Clear');
  Item.OnClick:=KillRecent;                                    
  Recent2.Add(Item);
  // Search Languages//
  LangList:=TStringList.Create;
  SearchDir(ExtractFileDir(Application.ExeName)+'\Lang\','*.ini',LangList);
  If LangList.Count<>0 Then
  For Index:=0 To LangList.Count-1 Do
    Begin
      Item:=TMenuItem.Create(Language1);
      Item.Caption:=ExtractFileName(ChangeFileExt(LangList[Index],''));
      Item.OnClick:=OnLangSelect;
      Item.Tag:=Index;
      Language1.Add(Item);
      //
      Item:=TMenuItem.Create(Language2);
      Item.Caption:=ExtractFileName(ChangeFileExt(LangList[Index],''));
      Item.OnClick:=OnLangSelect;
      Item.Tag:=Index;
      Language2.Add(Item);
    End;

  SkinList:=TStringList.Create;
  SearchDir(ExtractFileDir(Application.ExeName)+'\Skins\','*.msk',SkinList);
  If SkinList.Count<>0 Then
    For Index:=0 To SkinList.Count-1 Do
      Begin
        Item:=TMenuItem.Create(hemes1);
        Item.Caption:=ExtractFileName(ChangeFileExt(SkinList[Index],''));
        Item.OnClick:=OnSkinSelect;
        Item.Tag:=Index+1;
        hemes1.Add(Item);
        //
        Item:=TMenuItem.Create(hemes2);
        Item.Caption:=ExtractFileName(ChangeFileExt(SkinList[Index],''));
        Item.OnClick:=OnSkinSelect;
        Item.Tag:=Index+1;
        hemes2.Add(Item);
      End;

  Item:=TMenuItem.Create(hemes1);
  Item.Caption:='-';
  Item.Tag:=0;
  hemes1.Add(Item);

  Item:=TMenuItem.Create(hemes2);
  Item.Caption:='-';
  Item.Tag:=0;
  hemes2.Add(Item);

  Item:=TMenuItem.Create(hemes1);
  Item.Caption:=ReadFromLanguage('Menu','mnuDefaultSkin','Default skin');
  Item.Tag:=0;
  Item.OnClick:=OnSkinSelect;
  hemes1.Add(Item);

  Item:=TMenuItem.Create(hemes2);
  Item.Caption:=ReadFromLanguage('Menu','mnuDefaultSkin','Default skin');
  Item.Tag:=0;
  Item.OnClick:=OnSkinSelect;
  hemes2.Add(Item);
  ResName:='';
  LoadToolBarIcons;

  DirTree:=TTreeView.Create(Self);
  DirTree.DoubleBuffered:=True;
  FilesList:=TListView.Create(Self);
  FilesList.DoubleBuffered:=True;
  DirTree.Parent:=frmMain;
  FilesList.Parent:=frmMain;
  DirTree.Align:=alLeft;
  DirTree.Width:=170;
  FilesList.Align:=alClient;
  FilesList.ViewStyle:=vsReport;
  FilesList.LargeImages:=BrowserIconsBig;
  FilesList.SmallImages:=BrowserIcons;
  FilesList.IconOptions.AutoArrange:=True;
  FilesList.MultiSelect:=True;
  FilesList.OnDblClick:=FilesListDblCLick;
  FilesList.OnKeyDown:=FilesListKeyDown;
  DirTree.Images:=DirTreeIcons;
  DirTree.OnChanging:=DirTreeChanging;
  For Index:=0 To 8 Do
    Begin
      Column:=FilesList.Columns.Add;
      Case Index Of
        0:Begin
            Column.Caption:=ReadFromLanguage('ListItems','liFile','File name');
            Column.Width:=150;
          End;
        1:Begin
            Column.Caption:=ReadFromLanguage('ListItems','liSize','Size');
            Column.Width:=70;
          End;
        2:Begin
            Column.Caption:=ReadFromLanguage('ListItems','liPackSize','Packed size');
            Column.Width:=70;
          End;
        3:Begin
            Column.Caption:=ReadFromLanguage('ListItems','liCreate','Create');
            Column.Width:=80;
          End;
        4:Begin
            Column.Caption:=ReadFromLanguage('ListItems','liModify','Modify');
            Column.Width:=80;
          End;
        5:Begin
            Column.Caption:=ReadFromLanguage('ListItems','liOpen','Open');
            Column.Width:=80;
          End;
        6:Begin
            Column.Caption:=ReadFromLanguage('ListItems','liAttr','Attr');
            Column.Width:=45;
          End;
        7:Begin
            Column.Caption:=ReadFromLanguage('ListItems','liCRC','CRC32');
            Column.Width:=80;
          End;
        8:Begin
            Column.Caption:=ReadFromLanguage('ListItems','liPath','Path');              
            Column.Width:=90;
          End;
      End;
    End;
  FilesList.RowSelect:=True;
  FilesList.OnEdited:=FilesListRenameFile;
  DirTree.ReadOnly:=True;
  DirTree.Hide;
  FilesList.Hide;

  ShellTree:=TMadDirBrowser.Create(Self);
  ShellFilesList:=TMadFileBrowser.Create(Self);
  //ShellTree.AutoRefresh:=True;
  ShellTree.DoubleBuffered:=True;
  ShellTree.Parent:=frmMain;
  ShellTree.Align:=alLeft;
  ShellFilesList.Align:=alClient;
  ShellTree.Width:=170;
  ShellTree.OnChange:=DirTreeChange;
  //ShellTree.ObjectTypes:=[otFolders,otNonFolders,otHidden];
  ///
  //ShellFilesList.AutoRefresh:=True;
  ShellFilesList.DoubleBuffered:=True;
  ShellFilesList.Parent:=frmMain;
  
  ShellFilesList.ViewStyle:=vsReport;
  ShellFilesList.RowSelect:=True;
  ShellFilesList.MultiSelect:=True;
  ShellFilesList.ObjectTypes:=[otFolders,otNonFolders,otHidden];
  ShellFilesList.OnDblClick:=ShellFilesListDblClick;
  ///
  ShellTree.ShellListView:=ShellFilesList;
  ///
  Splitter:=TSplitter.Create(Self);
  Splitter.Parent:=frmMain;
  Splitter.Left:=170;
  Splitter.Width:=1;
  Splitter.ResizeStyle:=rsUpdate;
  sbMain.Panels[0].Text:=ReadFromLanguage('Status','Ready','Ready');
  {If ParamCount<>0 Then
    if FileExists(ParamStr(1)) then
      Begin
      End; }
  ReadConfig;
  //cbAddress.ShellTreeView:=ShellTree;
  //cbAddress.ShellListView:=ShellFilesList;
  //btnLevelUpClick
  //btnGoToClick
  btnLevelUp:=TToolButton.Create(barAddress);
  btnLevelUp.Parent:=barAddress;
  btnLevelUp.OnClick:=btnLevelUpClick;
  btnLevelUp.ImageIndex:=0;

  btnGoTo:=TToolButton.Create(barAddress);
  btnGoTo.Parent:=barAddress;
  btnGoTo.OnClick:=btnGoToClick;
  btnGoTo.ImageIndex:=1;
  btnGoTo.Left:=barAddress.Width;
  cbAddress.DoubleBuffered:=True;
end;

procedure TfrmMain.ExtractArchive(Files:TStringList;DestDir:String;RewriteMode:TRewriteMode;ExtractAll,_Delete,UpdateModTime,UpdateAccessTime:Boolean);
Var MZFDecompressor:TMZFDecompressor;
    UnPacker:TUnPacker;
    SevenZipDecompressor:TSevenZipViewer;
    Index:Byte;
    Temp:String;
Begin
  //showmessage(inttostr(byte(workingarchive.ArcType)));
  dlgCaption:=ReadFromLanguage('Status','Decompressing','Progress-Decompressing');
  ResName:='DECOMPRESSING';
  Case WorkingArchive.ArcType Of
    ftMZF:Begin
            MZFDecompressor:=TMZFDecompressor.Create;
            MZFDecompressor.ArchiveFile:=WorkingArchive.Archive;
            MZFDecompressor.DestinationDir:=DestDir;
            If Not ExtractAll Then
              For Index:=0 To Files.Count-1 Do
                Begin
                  Temp:=Files.Strings[Index];
                  MZFDecompressor.FileList.Add(Temp);
                End;
            MZFDecompressor.EnableSecurityMode:=GetBoolOptions('Config','EnableSecurity',False);
            MZFDecompressor.ExcludeSpec:=GetStringOptions('Config','ExcludeExt','*.exe;*.com;*.bat');
            MZFDecompressor.ExtractAll:=ExtractAll;
            MZFDecompressor.OnBegin:=ArchiverOnBegin;
            MZFDecompressor.OnEnd:=ArchiverOnEnd;
            MZFDecompressor.OnProgress:=ArchiverProgress;
            MZFDecompressor.OnGetPass:=GetPass;
            MZFDecompressor.OnError:=OnError;
            MZFDecompressor.UpdateModifyTime:=UpdateModTime;
            MZFDecompressor.UpdateLastAccesstime:=UpdateAccessTime;
            MZFDecompressor.FileOverwriteMode:=RewriteMode;
            MZFDecompressor.OnOverwritePrompt:=RewritePromt;
            MZFDecompressor.DeleteAfterExtract:=_Delete;
            MZFDecompressor.OnExtract:=DecompressorOnExtract;
            MZFDecompressor.Extract;
            MZFDecompressor.Free;
          End;
      ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,
      ftTar,ftACE2,ftJar,ftArc,ftArj,
      ftPKG5:
        Begin
          UnPacker:=TUnPacker.Create;
          UnPacker.EnableSecurity:=GetBoolOptions('Config','EnableSecurity',False);
          UnPacker.ExcludeExt:=GetStringOptions('Config','ExcludeExt','*.exe;*.com;*.bat');
          UnPacker.ArchiveFile:=WorkingArchive.Archive;
          UnPacker.ArchiveType:=WorkingArchive.ArcType;
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
      ft7Z:
        Begin
          SevenZipDecompressor:=TSevenZipViewer.Create;
          SevenZipDecompressor.FileName:=WorkingArchive.Archive;
          SevenZipDecompressor.DestDir:=DestDir;
          If ExtractAll Then
            SevenZipDecompressor.FileSpec.Clear;
          If (FilesList.SelCount>0)And(WorkingArchive.Opened) Then
          For Index:=0 To FilesList.Items.Count-1 Do
            Begin
              If FilesList.Items.Item[Index].Selected Then
                SevenZipDecompressor.FileSpec.Add(FilesList.Items.Item[Index].SubItems[8]);
            End;
          SevenZipDecompressor.OnBegin:=ArchiverOnBegin;
          SevenZipDecompressor.OnEnd:=ArchiverOnEnd;
          SevenZipDecompressor.OnProgress:=ArchiverProgress;
          SevenZipDecompressor.OnOverWrite:=RewritePromt;
          SevenZipDecompressor.OverWriteMode:=RewriteMode;
          SevenZipDecompressor.OnGetPaasword:=GetPass;
          SevenZipDecompressor.Extract;
          SevenZipDecompressor.Free;
        End;
      ftRar:
        Begin
          RarExtract(PChar(WorkingArchive.Archive),PChar(DestDir));
        End;
  End;
  dlgCaption:=ReadFromLanguage('Status','Progress','Progress');
  ResName:='';
End;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ShellTree.Free;
  ShellFilesList.Free;
  Splitter.Free;
  RemoveFileTree(GetTempDir);
  WriteParams;
  Recent.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  ShellFilesList.AutoRefresh:=True;
  ShellTree.AutoRefresh:=True;
  if OpenFromConsole then
    Begin
      OpenArchive(False);
    End;
  OpenFromConsole:=False;
end;

Procedure TFrmMain.GoToNode(SubDir:String);
Var Noddy:TTreeNode;
    Searching:Boolean;
Begin
DirTree.Items.BeginUpdate;
Noddy := DirTree.Items[0];
Searching := true;
while (Searching) and (Noddy <> nil) do
begin
  if UpperCase(Noddy.text) = UpperCase(SubDir) then
  begin
    Searching := False;
    Noddy.Expanded:=True;
    //Noddy.Expanded:=False;
    Noddy.Selected:=True;
    DirTree.Selected := Noddy;
    {TreeView1.SetFocus;
    ListView1.SetFocus;}         //Установить фокус обратно на TListView
  end
  else
  begin
    Noddy := Noddy.GetNext;
    //Noddy.HasChildren := true;
  end;
end;
DirTree.Items.EndUpdate;
      //Open tree node
End;

procedure TfrmMain.btnGoToClick(Sender: TObject);
label Start;
begin
  Start:;
  If Not WorkingArchive.Opened Then
    Begin
      //If Not DirectoryExists(cbAddress.Text) Then Exit;
      If FileExists(ExtractArchiveNameExtended(cbAddress.Text)) Then
        Begin
          WorkingArchive.Archive:=ExtractArchiveNameExtended(cbAddress.Text);
          WorkingArchive.Opened:=True;
          WorkingArchive.ArcType:=GetArchiveTypeBySignature(ExtractArchiveNameExtended(cbAddress.Text));
          WorkingArchive.FilterPath:=Format(RootCaption,[ExtractFileName(WorkingArchive.Archive)])+
                                            SubStractString(ExtractArchiveNameExtended(cbAddress.Text),cbAddress.Text);

          OpenArchive(False);
          if WorkingArchive.FilterPath<>'' then
            GoToNode(ExtractFileName(WorkingArchive.FilterPath));          
          Exit;
        End;
      //ShellFilesList
      If DirectoryExists(cbAddress.Text) Then
        ShellTree.Path:=cbAddress.Text
      Else
        ShellTree.Path:=cbAddress.Path;
    End Else
    Begin //Go to archive node
      If DirectoryExists(cbAddress.Text) Then
        begin
          CloseArchive;
          btnOpen.ImageIndex:=1;
          btnCheck.Visible:=False;
          btnOpen.Caption:=ReadFromLanguage('Buttons','btnOpen',btnOpen.Caption);
          GoTo Start;
        end;
      WorkingArchive.FilterPath:=Format(RootCaption,[ExtractFileName(WorkingArchive.Archive)])+
                                        SubStractString(cbAddress.Text,WorkingArchive.Archive){cbAddress.Text};
      GoToNode(ExtractFileName(WorkingArchive.FilterPath){getlastdirname(WorkingArchive.FilterPath)});
      OpenArchive(True);
    End;
end;

procedure TfrmMain.cbAddressKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If Key=VK_RETURN Then
    btnGoToClick(Sender);
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
Var FName:String;
    Dir:String;
begin
  If WorkingArchive.Opened Then
    Begin
      CloseArchive;
      btnOpen.ImageIndex:=1;
      btnOpen.Caption:=ReadFromLanguage('Buttons','btnOpen',btnOpen.Caption);
      btnCheck.Visible:=False;
      //ExecFile!!!
      {If FilesList.SelCount<=0 Then
        Exit;
      ExecuteFile(cbAddress.Text+'\'+FilesList.Items.Item[FilesList.ItemIndex].Caption,True,True);     }
      //FilesListDblCLick(Sender);
      Exit;
    End;
  If (ShellFilesList.ItemIndex<>-1)And(cbAddress.Text<>'') Then
    Begin
      FName:=ShellFilesList.Folders[ShellFilesList.ItemIndex].PathName;
      If Not FileExists(FName) Then Exit;
      WorkingArchive.FilterPath:=Format(RootCaption,[ExtractFileName(FName)]);
      WorkingArchive.Archive:=FName;
      WorkingArchive.ArcType:=GetArchiveTypeBySignature(WorkingArchive.Archive);
      WorkingArchive.Opened:=True;
      OpenArchive(False);
      FName:='';
      Exit;
    End;
  Dir:=ShellTree.Path;
  DlgOpen.InitialDir:=Dir;
  ShellDirectory:=cbAddress.Text;
  If Not DlgOpen.Execute(Handle) Then
    Begin
      Dir:='';
      Exit;
    End;

  WorkingArchive.FilterPath:=Format(RootCaption,[ExtractFileName(DlgOpen.FileName)]);
  WorkingArchive.Archive:=DlgOpen.FileName;
  WorkingArchive.ArcType:=GetArchiveTypeBySignature(WorkingArchive.Archive);
  WorkingArchive.Opened:=True;
  OpenArchive(False);

  Dir:='';
end;

procedure TfrmMain.Closearchive1Click(Sender: TObject);
begin
  CloseArchive;
end;

procedure TfrmMain.btnAddClick(Sender: TObject);
var MZFCompressor:TMzfCompressor;
    UniversalCompressor:TUniversalCompressor;
    SevenZipCompressor:TSevenZipCompressor;
    Index:LongWord;
    ArchCompressMethod:TFileType;
    Temp:String;
begin
  application.CreateForm(tdlgAddFiles,dlgAddFiles);
  If ShellFilesList.SelCount>0 Then
  For Index:=0 To ShellFilesList.Items.Count-1 Do
    Begin
      if not WorkingArchive.Opened then     
        If ShellFilesList.Items.Item[Index].Selected Then
          If DirectoryExists(ShellFilesList.Folders[Index].PathName) Then
            dlgAddFiles.lvFiles.Items.Add.Caption:=ShellFilesList.Folders[Index].PathName+'\*.*' Else
            dlgAddFiles.lvFiles.Items.Add.Caption:=ShellFilesList.Folders[Index].PathName;
    End Else
      if Not WorkingArchive.Opened then      
        dlgAddFiles.lvFiles.Items.Add.Caption:=ShellTree.Path+'\*.*';{cbAddress.Text+'\*.*'};
  if Not WorkingArchive.Opened then
  Begin;
  If ShellFilesList.SelCount<=0 Then
    Begin
      Temp:=cbAddress.Path;
      if Length(Temp)<>0 then
        If Temp[Length(Temp)]<>'\' Then
          Temp:=Temp+'\';
      dlgAddFiles.cbFileName.Text:=Temp+'NewArchive.MZF';
    End Else
    Begin
      Temp:=ShellFilesList.Folders[ShellFilesList.ItemIndex].PathName+'.MZF';
      dlgAddFiles.cbFileName.Text:=Temp;
    End;
  End Else
  Begin
    dlgAddFiles.cbFileName.Text:=WorkingArchive.Archive;
  End;
  If dlgAddFiles.showmodal = mrCancel Then
    Begin
      dlgAddFiles.Free;
      Exit;
    End;
  //Main Compression Block !!!
  Case dlgAddFiles.cbCompressMethod.ItemIndex Of
    0:ArchCompressMethod:=ftMZF;
    1:ArchCompressMethod:=ft7Z;
    2:ArchCompressMethod:=ftZIP;
    3:ArchCompressMethod:=ftBH;
    4:ArchCompressMethod:=ftLHA;
    5:ArchCompressMethod:=ftJAR;
    6:ArchCompressMethod:=ftCAB;
    7:ArchCompressMethod:=ftTAR;
  End;
  //The Self Compressing
  dlgCaption:=ReadFromLanguage('Status','Compressing','Progress-Compressing');
  ResName:='COMPRESSING';
  Case ArchCompressMethod Of
    ftMZF:
      Begin
        MZFCompressor:=TMzfCompressor.Create;
        MZFCompressor.ArchiveFileName:=dlgAddFiles.cbFileName.Text;
        MZFCompressor.OnBegin:=ArchiverOnBegin;
        MZFCompressor.OnProgress:=ArchiverProgress;
        MZFCompressor.OnEnd:=ArchiverOnEnd;
        MZFCompressor.OnGetPassword:=GetPass;
        MZFCompressor.Commentary:=dlgAddFiles.memComment.Text;
        MZFCompressor.EncodeFileContain:=dlgAddFiles.cbEncodeFile.Checked;
        MZFCompressor.EncodeHeadInfo:=dlgAddFiles.cbEncodeHead.Checked;
        MZFCompressor.ArchReadOnly:=dlgAddFiles.cbArchiveReadOnly.Checked;
        MZFCompressor.ArcHidden:=dlgAddFiles.cbArchiveHidden.Checked;
        MZFCompressor.ArcArchive:=dlgAddFiles.cbArchiveArchived.Checked;
        MZFCompressor.Password:=dlgAddFiles.edPassword.Text;
        //
        MZFCompressor.Level:=dlgAddFiles.InSideLevel;
        For Index:=0 To dlgAddFiles.lvFiles.Items.Count-1 Do
          MZFCompressor.FileList.Add(dlgAddFiles.lvFiles.Items.Item[Index].Caption);

        MZFCompressor.CreateArchive;
        //Finalize :-)
        MZFCompressor.Free;
      End;
    ftBH,ftGZip,ftLha,ftJar,ftCab,ftTar,ftZip:
      Begin
        UniversalCompressor:=TUniversalCompressor.Create;
        UniversalCompressor.ArchiveFile:=dlgAddFiles.cbFileName.Text;
        UniversalCompressor.FileType:=ArchCompressMethod;
        UniversalCompressor.OnBegin:=ArchiverOnBegin;
        UniversalCompressor.OnEnd:=ArchiverOnEnd;
        UniversalCompressor.OnProgress:=ArchiverProgress;
        UniversalCompressor.Password:=dlgAddFiles.edPassword.Text;
        For Index:=0 To dlgAddFiles.lvFiles.Items.Count-1 Do
          UniversalCompressor.FileSpec.Add(dlgAddFiles.lvFiles.Items.Item[Index].Caption);
        UniversalCompressor.EncodeHeader:=dlgAddFiles.cbEncodeHead.Checked;
        UniversalCompressor.CompressValue:=TCompressValue(dlgAddFiles.tbLevel.Position);
        {Case dlgAddFiles.tbLevel.Position Of
          0:UniversalCompressor.CompressValue:=cvFastest;
          1:UniversalCompressor.CompressValue:=cvNormal;
          2:UniversalCompressor.CompressValue:=cvGood;
          3:UniversalCompressor.CompressValue:=cvExtra;
        End;}
        UniversalCompressor.ReadingOnly:=dlgAddFiles.cbArchiveReadOnly.Checked;
        UniversalCompressor.Archived:=dlgAddFiles.cbArchiveArchived.Checked;
        UniversalCompressor.Hidden:=dlgAddFiles.cbArchiveHidden.Checked;
        UniversalCompressor.Comment:=dlgAddFiles.memComment.Text;
        //
        UniversalCompressor.Compress;
        UniversalCompressor.Free;
      End;
    ft7z:
      Begin
        SevenZipCompressor:=TSevenZipCompressor.Create;
        SevenZipCompressor.FileName:=dlgAddFiles.cbFileName.Text;
        SevenZipCompressor.CompressLevel:=TCompressLevel(dlgAddFiles.tbLevel.Position);
        {Case dlgAddFiles.tbLevel.Position Of
          0:SevenZipCompressor.CompressLevel:=clFast;
          1:SevenZipCompressor.CompressLevel:=clNormal;
          2:SevenZipCompressor.CompressLevel:=clMaximum;
          3:SevenZipCompressor.CompressLevel:=clUltra;
        End;}
        For Index:=0 To dlgAddFiles.lvFiles.Items.Count-1 Do
          SevenZipCompressor.FileSpec.Add(dlgAddFiles.lvFiles.Items.Item[Index].Caption);
        SevenZipCompressor.RootDir:=GetLastDir(GetDominateDir(SevenZipCompressor.FileSpec));
        SevenZipCompressor.OnBegin:=ArchiverOnBegin;
        SevenZipCompressor.OnEnd:=ArchiverOnEnd;
        SevenZipCompressor.OnProgress:=ArchiverProgress;
        SevenZipCompressor.Password:=dlgAddFiles.edPassword.Text;
        SevenZipCompressor.Archived:=dlgAddFiles.cbArchiveArchived.Checked;
        SevenZipCompressor.ReadingOnly:=dlgAddFiles.cbArchiveReadOnly.Checked;
        SevenZipCompressor.Hidden:=dlgAddFiles.cbArchiveArchived.Checked;
        SevenZipCompressor.Compress;
        SevenZipCompressor.Free;
      End;
  End;
  dlgAddFiles.free;
  dlgCaption:=ReadFromLanguage('Status','Progress','Progress');
  ResName:='';
end;

procedure TfrmMain.btnLevelUpClick(Sender: TObject);
begin
  {If Length(cbAddress.Text)<=2 Then Exit Else
    If Length(cbAddress.Text)<=0 Then Exit;}
  If Not WorkingArchive.Opened Then
    begin
      If DirectoryExists(getlastdir(cbAddress.Path)) Then
        cbAddress.Path:=(getlastdir(cbAddress.Path))
    end
  Else
    cbAddress.Text:=getlastdir(cbAddress.Text);
  btnGoToClick(Sender);
  SetCurrentDir(cbAddress.Text);
end;

procedure TfrmMain.Exit2Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnExtractClick(Sender: TObject);
var Files:TStringList;
    Index:LongWord;
    Temp:String;
    Mode:TReWriteMode;
begin
  Case WorkingArchive.Opened Of
    True:Begin
            Application.CreateForm(TdlgExtract,dlgExtract);
            dlgExtract.cbDestDir.Text:=GetStringOptions('Config','DefExtractFolder',GetSystemPath(TSystemPath(0)));
            If FilesList.SelCount=0 Then dlgExtract.rgExtractMode.ItemIndex:=0 Else
              dlgExtract.rgExtractMode.ItemIndex:=1;
            If dlgExtract.ShowModal=mrOk Then
              Begin
                Case dlgExtract.rgUpdateMode.ItemIndex Of
                  0:Mode:=omOverwriteAll;
                  1:Mode:=omSkipAll;
                  2:Mode:=omUnknown;
                End;
                Files:=TStringList.Create;
                If FilesList.SelCount>0 Then
                For Index:=0 To FilesList.Items.Count-1 Do
                  If FilesList.Items.Item[Index].Selected Then
                    Begin
                      Temp:=FilesList.Items.Item[Index].SubItems.Strings[7]+'\'+
                      FilesList.Items.Item[Index].Caption;
                      Delete(Temp,1,Pos(']',Temp));     //???? UnKnow +1 or not
                      Files.Add(temp);
                    End;
                ExtractArchive(Files,dlgExtract.cbDestDir.Text,Mode,dlgExtract.rgExtractMode.ItemIndex=0
                               ,dlgExtract.rgDeleteArchive.ItemIndex=1,dlgExtract.cbUpdateModifyTime.Checked
                               ,dlgExtract.cbUpdateAccessTime.Checked);
                Files.Free;
              End;
            dlgExtract.Free;    
         End;
    False:Begin
            If ShellFilesList.ItemIndex=-1 Then
              Begin
                MessageBox(Handle,Pchar(ReadFromLanguage('Messages','NeedArchive','You must select or open archive!')),'WinMZF',MB_OK+MB_ICONEXCLAMATION);
                Exit;
              End;
            If GetArchiveTypeBySignature(ShellFilesList.Folders[ShellFilesList.ItemIndex].PathName)=ftAnyFile Then
              Begin
                //MessageBox(Handle,'This is not archive!','Error',MB_OK+MB_ICONEXCLAMATION);
                DisplayMessage(Format(ReadFromLanguage('Messages','CantOpenFile','Can not open file %s as archive'),
                  [ShellFilesList.Folders[ShellFilesList.ItemIndex].PathName]),IT_EVENT);
                Exit;
              End;
            Application.CreateForm(TdlgExtract,dlgExtract);
            dlgExtract.cbDestDir.Text:=GetStringOptions('Config','DefExtractFolder',GetSystemPath(TSystemPath(0)));
            dlgExtract.rgExtractMode.Items.Delete(1);
            dlgExtract.rgExtractMode.ItemIndex:=0;
            If dlgExtract.ShowModal=mrOk Then
              Begin
                WorkingArchive.Archive:=ShellFilesList.Folders[ShellFilesList.ItemIndex].PathName;
                WorkingArchive.ArcType:=GetArchiveTypeBySignature(ShellFilesList.Folders[ShellFilesList.ItemIndex].PathName);
                ExtractArchive(Nil,dlgExtract.cbDestDir.Text,Mode,dlgExtract.rgExtractMode.ItemIndex=0
                               ,dlgExtract.rgDeleteArchive.ItemIndex=1,dlgExtract.cbUpdateModifyTime.Checked
                               ,dlgExtract.cbUpdateAccessTime.Checked);
                WorkingArchive.Archive:='';
                WorkingArchive.ArcType:=ftAnyFile;
              End;
            dlgExtract.Free;
          End;
  End;
end;

procedure TfrmMain.Selectall2Click(Sender: TObject);
begin
  ShellFilesList.SelectAll;
end;

procedure TfrmMain.Deselectall2Click(Sender: TObject);
var Index:LongWord;
begin
  If ShellFilesList.Items.Count>0 Then
    For Index:=0 To ShellFilesList.Items.Count-1 Do
      ShellFilesList.Items.Item[Index].Selected:=False;
end;

procedure TfrmMain.Deselectall1Click(Sender: TObject);
var Index:LongWord;
begin
  If FilesList.Items.Count>0 Then
    For Index:=0 To FilesList.Items.Count-1 Do
      FilesList.Items.Item[Index].Selected:=False;
end;

procedure TfrmMain.Invertselected2Click(Sender: TObject);
var Index:LongWord;
begin
  If ShellFilesList.Items.Count>0 Then
    For Index:=0 To ShellFilesList.Items.Count-1 Do
      ShellFilesList.Items.Item[Index].Selected:=Not ShellFilesList.Items.Item[Index].Selected;
end;

procedure TfrmMain.Test1Click(Sender: TObject);
var Index:LongWord;
begin
  Application.CreateForm(TdlgTestArchive,dlgTestArchive);
  //
  dlgTestArchive.ArchiveFile:=WorkingArchive.Archive;
  dlgTestArchive.ArcType:=WorkingArchive.ArcType;
  if FilesList.SelCount<>0 then
    for Index := 0 to FilesList.Items.Count - 1 do
      dlgTestArchive.FileSpec.Add(FilesList.Items[Index].SubItems[7]+'\'+FilesList.Items[Index].Caption)
    Else
      dlgTestArchive.FileSpec.Add('*.*');
  dlgTestArchive.ShowModal;
  dlgTestArchive.Free;
end;

procedure TfrmMain.Tips1Click(Sender: TObject);
begin
  Application.CreateForm(TdlgTips,dlgTips);
  dlgTips.ShowModal;
  dlgTips.Free;
end;                                   

procedure TfrmMain.Invertselected1Click(Sender: TObject);
var Index:LongWord;
begin
  If FilesList.Items.Count>0 Then
    For Index:=0 To FilesList.Items.Count-1 Do
      FilesList.Items.Item[Index].Selected:=Not FilesList.Items.Item[Index].Selected;
end;

procedure TfrmMain.Decode1Click(Sender: TObject);
var ThrdCoder:TEncodeThread;
    Index:LongWord;
begin
  If ShellFilesList.Items.Count<=0 Then
    Begin
      MessageBox(Handle,Pchar(ReadFromLanguage('Messages','NeedFile','You must select file(s)')),'WinMZF',MB_OK+MB_ICONEXCLAMATION);
      Exit;
    End;
  With Sender As TMenuItem Do
    Begin
      Application.CreateForm(TdlgEncodeDecode,dlgEncodeDecode);
      dlgEncodeDecode.Caption:=ReadFromLanguage('Windows','Encoder','WinMZF File encoder\decoder');
      dlgEncodeDecode.cbMode.ItemIndex:=Tag;
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
          For Index:=0 To ShellFilesList.Items.Count-1 Do
            If ShellFilesList.Items.Item[Index].Selected Then
              Begin
                If (Not FileExists(ShellFilesList.Folders[Index].PathName))And
                   (DirectoryExists(ShellFilesList.Folders[Index].PathName)) Then
                      ThrdCoder.FileList.Add(ShellFilesList.Folders[Index].PathName+'\*.*')
                    Else
                      ThrdCoder.FileList.Add(ShellFilesList.Folders[Index].PathName);
                  End;
                ThrdCoder.Execute;
              End;
        End;
      dlgEncodeDecode.Free;
  SetCurrentDir(cbAddress.Text);
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
var DeleteThrd:TThrdDelete;
    Temp:String;
    Index:LongWord;
    AFilesList:TStringList;
begin
  If MessageBox(Handle,Pchar(ReadFromLanguage('Messages','DeleteFiles','Delete this file(s)')),'WinMZF',MB_YESNO+MB_ICONQUESTION)<>IDYES Then
    Exit;
  If WorkingArchive.Opened Then
    Begin //Delete insode archive
      If FilesList.SelCount<=0 Then
        Begin
          MessageBox(Handle,PChar(ReadFromLanguage('Messages','NeedFile','You must select file(s)')),'WinMZF',MB_OK+MB_ICONEXCLAMATION);
          Exit;
        End;
      AFilesList:=TStringList.Create;
      For Index:=0 To FilesList.Items.Count-1 Do
        If FilesList.Items.Item[Index].Selected Then begin
          AFilesList.Add(cbAddress.Text+'\'+FilesList.Items.Item[Index].Caption);   end;
      DeleteFiles(AFilesList);
      AFilesList.Free;
    End Else
    Begin //Delete inside FAT
      If Not DirectoryExists(cbAddress.Text) Then
        Begin
          //MessageBox(Handle,'Coud not find direcoty','Error',MB_OK+MB_ICONSTOP);
          DisplayMessage(ReadFromLanguage('Messages','DirNotEx','Coud not find directory'),IT_INFO);
          Exit;
        End;
      Temp:=cbAddress.Text;
      If Temp[Length(Temp)]<>'\' Then
        Temp:=Temp+'\';
      DeleteThrd:=TThrdDelete.Create(True);
      {DeleteThrd.Priority:=tpNormal;}
      DeleteThrd.Initialize;
      If ShellFilesList.SelCount<=0 Then
        DeleteThrd.Files.Add(Temp+'*.*')
      Else
        For Index:=0 To ShellFilesList.Items.Count-1 Do
          Begin
            If ShellFilesList.Items[Index].Selected Then
              If FileExists(ShellFilesList.Folders[Index].PathName) Then
                DeleteThrd.Files.Add(ShellFilesList.Folders[Index].PathName)
              Else
                DeleteThrd.Files.Add(ShellFilesList.Folders[Index].PathName+'\*.*');
          End;
      DeleteThrd.FillZeroBytes:=False;
      DeleteThrd.Execute;
    End;
  SetCurrentDir(GetLastDir(cbAddress.Text));
end;

procedure TfrmMain.Renamefile1Click(Sender: TObject);
var NewName:String;
begin
  If FilesList.SelCount<=0 Then
    Begin
      MessageBox(Handle,Pchar(ReadFromLanguage('Messages','NeedToRename','You must select file to rename')),'WinMZF',MB_OK+MB_ICONEXCLAMATION);
      Exit;
    End;
  NewName:=FilesList.ItemFocused.Caption;
  If InputQuery('WinMZF',ReadFromLanguage('Messages','NewName','Enter the new file name'),NewName) Then
    FilesListRenameFile(Sender,FilesList.ItemFocused,NewName);
  OpenArchive(True);
end;

procedure TfrmMain.btnCopyClick(Sender: TObject);
var FIndex:LongWord;
    Files:TStringList;
    DestDir,Temp:String;
    ThrdCopy:TThrdCopy;
begin
  With Sender As TToolButton Do          
  Begin
    Case WorkingArchive.Opened Of
      True:
        Begin
          If (FilesList.SelCount<=0)And(WorkingArchive.ArcType<>ftRar) Then
            Begin
              MessageBox(Handle,PChar(ReadFromLanguage('Messages','NeedSelect','You must select minimum 1 file')),'WinMZF',MB_OK+MB_ICONEXCLAMATION);
              Exit;
            End;
          DestDir:=GetSelectedDir;        
          If DestDir='' Then Exit;
          Files:=TStringList.Create;
          If FilesList.SelCount>0 Then
          For FIndex:=0 To FilesList.Items.Count-1 Do
            If FilesList.Items.Item[FIndex].Selected Then
              Begin
                Temp:=FilesList.Items.Item[FIndex].SubItems.Strings[7]+'\'+
                FilesList.Items.Item[FIndex].Caption;
                Delete(Temp,1,Pos(']',Temp));     //???? UnKnow +1 or not
                Files.Add(temp);
              End;
          ExtractArchive(Files,DestDir,omUnknown,False,False,False,False);
          If (Tag=1) Then
            DeleteFiles(Files);
          Files.Free;
        End;                    
      False:
        Begin //Work with FAT
          DestDir:=GetSelectedDir;
          If DestDir='' Then Exit;
          Temp:=cbAddress.Text;
          If Temp[Length(Temp)]<>'\' Then
            Temp:=Temp+'\';
          ThrdCopy:=TThrdCopy.Create(True);
          ThrdCopy.Initialize;
          ThrdCopy.Replace:=Tag=1;
          ThrdCopy.DestinationDir:=DestDir;
          If ShellFilesList.SelCount<=0 Then
            ThrdCopy.FilesList.Add(Temp+'\*.*')
          Else
            For FIndex:=0 To ShellFilesList.Items.Count-1 Do
                If ShellFilesList.Items.Item[FIndex].Selected Then
                  Begin
                    If (Not FileExists(ShellFilesList.Folders[FIndex].PathName))And
                       (DirectoryExists(ShellFilesList.Folders[FIndex].PathName)) Then
                      ThrdCopy.FilesList.Add(ShellFilesList.Folders[FIndex].PathName+'\*.*')
                    Else
                      ThrdCopy.FilesList.Add(ShellFilesList.Folders[FIndex].PathName);
                  End;
        ThrdCopy.Execute;
        End;
    End;
  End;
  SetCurrentDir(cbAddress.Text);
end;

procedure TfrmMain.Replaceselected1Click(Sender: TObject);
begin
  With Sender As TMenuItem Do
    Begin
      Case Tag Of
        0:btnCopyClick(btnCopy);
        1:btnCopyClick(btnReplace);
      End;
    End;
end;

procedure TfrmMain.btnInfoClick(Sender: TObject);
var Index:LongWord;
    MZFViewer:TMZFViewer;
    SevenZipViewer:TSevenZipViewer;
    AComment,Version:String;
    FileCount:LongWord;
    UnpackedSize,ArcSize:Int64;
begin     
  FileCount:=0;
  AComment:='';
  Version:='';
  UnpackedSize:=0;
  ArcSize:=0;
  Case WorkingArchive.Opened Of
    True:
      Begin
        Case WorkingArchive.ArcType Of
          ftMZF:
            Begin
              MZFViewer:=TMZFViewer.Create;
              MZFViewer.OnPassQuest:=GetPass;
              MZFViewer.OnError:=OnError;
              MZFViewer.ArchiveName:=WorkingArchive.Archive;
              FileCount:=MZFViewer.GetFileCount(AComment,UnpackedSize,Version,ArcSize);
              MZFViewer.Free;
            End;
          ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,ftArj,ftPKG5,ftACE2:
            Begin
              GetArchiveInfo(WorkingArchive.Archive,Version,AComment,FileCount,UnpackedSize,ArcSize);
            End;
          ftRar:
            Begin
              Version:=IntToStr(GetRarArcInfo(WorkingArchive.Archive,FileCount,ArcSize,UnpackedSize));
            End;
          ft7z:
            Begin
              SevenZipViewer:=TSevenZipViewer.Create;
              SevenZipViewer.FileName:=WorkingArchive.Archive;
              SevenZipViewer.Get7zArchiveInfo(Version,AComment,FileCount,UnPackedSize,ArcSize);
              SevenZipViewer.Free;
            End;
        End;
        Application.CreateForm(TdlgFileInfo,dlgFileInfo);
        If FilesList.SelCount<>0 Then
          Begin
            //dlgFileInfo.tsGeneral.Visible:=True;
            dlgFileInfo.lbFileName.Caption:=FilesList.Items.Item[FilesList.ItemIndex].Caption;
            dlgFileInfo.lbFileFolder.Caption:=FilesList.Items.Item[FilesList.ItemIndex].SubItems.Strings[7];
            dlgFileInfo.lbFileSize.Caption:=FilesList.Items.Item[FilesList.ItemIndex].SubItems.Strings[0]+' Byte(s)';
            dlgFileInfo.lbFilePackedSize.Caption:=FilesList.Items.Item[FilesList.ItemIndex].SubItems.Strings[1]+' Byte(s)';
            dlgFileInfo.lbFileCreationTIme.Caption:=FilesList.Items.Item[FilesList.ItemIndex].SubItems.Strings[2];
            dlgFileInfo.lbFileModifyTime.Caption:=FilesList.Items.Item[FilesList.ItemIndex].SubItems.Strings[3];
            dlgFileInfo.lbFileLastAccessTime.Caption:=FilesList.Items.Item[FilesList.ItemIndex].SubItems.Strings[4];
            dlgFileInfo.lbFileAttr.Caption:=FilesList.Items.Item[FilesList.ItemIndex].SubItems.Strings[5];
            dlgFileInfo.lbFileCRC.Caption:=FilesList.Items.Item[FilesList.ItemIndex].SubItems.Strings[6];
          End Else dlgFileInfo.tsGeneral.Free;
        dlgFileInfo.lbArchiveVersion.Caption:=Version;
        dlgFileInfo.lbArchiveFileCount.Caption:=IntToStr(FileCount);
        dlgFileInfo.lbArchiveSize.Caption:=IntToStr(ArcSize)+' Byte(s)';
        dlgFileInfo.lbArchiveUnPacked.Caption:=IntToStr(UnpackedSize)+' Byte(s)';
        dlgFileInfo.pbRatio.Position:=GetPercentDone(0,ArcSize,UnPackedSize);
        dlgFileInfo.lbRatio.Caption:=IntToStr(dlgFileInfo.pbRatio.Position)+'%';
        dlgFileInfo.lbArchiveComment.Caption:=AComment;
        //
        dlgFileInfo.ShowModal;
        dlgFileInfo.Free;
      End;
    False:
      Begin
        If ShellFilesList.SelCount<=0 Then
          ExecuteContextMenu(ShellTree.Path,'properties',Handle)
        Else
          Begin
            For Index:=0 To ShellFilesList.Items.Count-1 Do
              If ShellFilesList.Items.Item[Index].Focused Then //Rewrite Focused on Selected
                ExecuteContextMenu(ShellFilesList.Folders[Index].PathName,
                                   'properties',Handle);
          End;

      End;
  End;
end;

procedure TfrmMain.Savebackupcopyas1Click(Sender: TObject);
begin
  dlgSave.Filter:='Archive|*'+ExtractFIleExt(WorkingArchive.Archive);
  if dlgSave.Execute(Handle) then
    Begin
      if ExtractFileExt(dlgSave.FileName)<>ExtractFIleExt(WorkingArchive.Archive) then
        dlgSave.FileName:=dlgSave.FileName+ExtractFIleExt(WorkingArchive.Archive);
      CopyFile(PChar(WorkingArchive.Archive),PChar(dlgSave.FileName),False);
    End;
end;

procedure TfrmMain.Selectall1Click(Sender: TObject);
var Index:LongWord;
begin
  If FilesList.Items.Count>0 Then
    For Index:=0 To FilesList.Items.Count-1 Do
      FilesList.Items.Item[Index].Selected:=True;
end;

procedure TfrmMain.Executefile1Click(Sender: TObject);
begin
  FilesListDblCLick(Sender);
end;

procedure TfrmMain.Managefavorite1Click(Sender: TObject);
begin
  Application.CreateForm(TdlgFavorite,dlgFavorite);
  If dlgFavorite.ShowModal=mrOk Then
    Begin

    End;
  dlgFavorite.Free;
end;

procedure TfrmMain.Addfavorite1Click(Sender: TObject);

procedure AddToFavorite(FileName:String);
var Fav:TIniFile;
    Count:Integer;
    FileStream:TFileStream;
    Size:LongWord;
Begin
  FileStream:=TFileStream.Create(FileName,fmOpenRead);
  Size:=FileStream.Size;
  FileStream.Free;
  Fav:=TIniFile.Create(ExtractFileDir(Application.Exename)+'\Fav.ini');
  Count:=Fav.ReadInteger('Main','Count',0);
  Inc(Count);
  Fav.WriteInteger('Main','Count',Count);
  Fav.WriteString('Favorite'+IntToStr(Count-1),'Caption',ExtractFileName(FileName));
  Fav.WriteInteger('Favorite'+IntToStr(Count-1),'ImageIndex',GetImgIndexByExt(ExtractFileExt(FileName)));
  Fav.WriteInteger('Favorite'+IntToStr(Count-1),'Size',Size);
  Fav.WriteString('Favorite'+IntToStr(Count-1),'Pach',ExtractFileDir(FileName)+'\');
  Fav.Free;
  Size:=0;                               
End;

begin
  Case WorkingArchive.Opened Of
    True:AddToFavorite(WorkingArchive.Archive);
    False:
      Begin
        If DlgOpen.Execute(Handle) Then
          AddToFavorite(DlgOpen.FileName);
      End;
  End;
end;

procedure TfrmMain.Findfile1Click(Sender: TObject);
var Temp:String;
begin
  Application.CreateForm(TdlgFindConfig,dlgFindConfig);
  If WorkingArchive.Opened Then
    Begin
      dlgFindConfig.edDir.Text:='';
      dlgFindConfig.edDir.Enabled:=False;
      dlgFindConfig.btnBrows.Enabled:=False;
    End
  Else
    Begin
      Temp:=ShellTree.Path;
      If Temp[Length(Temp)]<>'\' Then
        Temp:=Temp+'\';
      dlgFindConfig.edDir.Text:=Temp;
      dlgFindConfig.edDir.Enabled:=True;
      dlgFindConfig.btnBrows.Enabled:=True;
    End;
  If dlgFindConfig.ShowModal=mrOk Then
    Begin
      If WorkingArchive.Opened Then
        Begin //Search in ARCHIVE
          Application.CreateForm(TdlgSearchResults,dlgSearchResults);
          dlgSearchResults._Search:=True;
          dlgSearchResults._Archive:=True;
          dlgSearchResults._Dir:=WorkingArchive.Archive;
          dlgSearchResults._fType:=WorkingArchive.ArcType;
          dlgSearchResults._Ext:=dlgFindConfig.edFileName.Text;
          dlgSearchResults.ShowModal;
          dlgSearchResults.Free;
        End Else
        Begin //Search in FAT
          Application.CreateForm(TdlgSearchResults,dlgSearchResults);
          dlgSearchResults._Search:=True;
          dlgSearchResults._Dir:=dlgFindConfig.edDir.Text;
          dlgSearchResults._Ext:=dlgFindConfig.edFileName.Text;
          dlgSearchResults.ShowModal;
          dlgSearchResults.Free;
        End;
    End;
  dlgFindConfig.Free;
end;

procedure TfrmMain.Report2Click(Sender: TObject);
begin
  With Sender As TMenuItem Do
    Begin
      Case Tag Of
        0:Begin
            ShellFilesList.ViewStyle:=vsReport;
            FilesList.ViewStyle:=vsReport;
          End;
        1:Begin
            ShellFilesList.ViewStyle:=vsList;
            FilesList.ViewStyle:=vsList;
          End;
        2:Begin
            ShellFilesList.ViewStyle:=vsSmallIcon;
            FilesList.ViewStyle:=vsSmallIcon;
          End;
        3:Begin
            ShellFilesList.ViewStyle:=vsIcon;
            FilesList.ViewStyle:=vsIcon;
          End;
        End;
      SetIntegerOptions('Config','ViewStyle',Tag);
    End;
end;

procedure TfrmMain.Setup1Click(Sender: TObject);
begin
  Application.CreateForm(TdlgConfig,dlgConfig);
  dlgConfig.ReadParams;
  If dlgConfig.ShowModal=mrOk Then
    Begin
      dlgConfig.WriteParams;
      ReadConfig;
      if dlgConfig.cbAssociate.Checked then
        RegisterApplication
      else
        UnregisterApplication;
    End;
  dlgConfig.Free;
end;

procedure TfrmMain.Convertarchive1Click(Sender: TObject);
begin
  Application.CreateForm(TdlgConvert,dlgConvert);
  If WorkingArchive.Opened Then
    Begin
      dlgConvert.edSource.Text:=WorkingArchive.Archive;
      dlgConvert.edTarget.Text:=ChangeFileExt(WorkingArchive.Archive,'.mzf');
      Case WorkingArchive.ArcType Of
        ftMZF:dlgConvert.cbConvertFrom.ItemIndex:=0;
        ftRar:dlgConvert.cbConvertFrom.ItemIndex:=1;
        ft7z:dlgConvert.cbConvertFrom.ItemIndex:=2;
        ftZip:dlgConvert.cbConvertFrom.ItemIndex:=3;
        ftBh:dlgConvert.cbConvertFrom.ItemIndex:=4;
        ftZoo:dlgConvert.cbConvertFrom.ItemIndex:=5;
        ftGZip:dlgConvert.cbConvertFrom.ItemIndex:=6;
        ftLha:dlgConvert.cbConvertFrom.ItemIndex:=7;
        ftCab:dlgConvert.cbConvertFrom.ItemIndex:=8;
        ftArj:dlgConvert.cbConvertFrom.ItemIndex:=9;
        ftPKG5:dlgConvert.cbConvertFrom.ItemIndex:=10;
        ftACE2:dlgConvert.cbConvertFrom.ItemIndex:=11;
      End;
    End;
  dlgConvert.ShowModal;
  dlgConvert.Free;
end;

procedure TfrmMain.CreateSXF1Click(Sender: TObject);
var Module:String;
begin
  If Not FileExists(ExtractFileDir(Application.ExeName)+'\def_mzf.sfx') Then
    Begin
      //MessageBox(Handle,'Coud not find SFX module','Error',MB_OK+MB_ICONSTOP);
      DisplayMessage(ReadFromLanguage('Messages','SFXNotFound','Coud not found "SFX" module'),IT_ERROR);
      Exit;
    End;
  Application.CreateForm(TdlgMakeSFX,dlgMakeSFX);
  dlgMakeSFX.edSourceArchive.Text:=WorkingArchive.Archive;
  dlgMakeSFX.edDestName.Text:=ChangeFileExt(WorkingArchive.Archive,'.exe');
  dlgMakeSFX.edSFXCaption.Text:=ReadFromLanguage('Windows','SFX','WinMZF Self Extraction Archive');
  dlgMakeSFX.edExtractDir.Text:='';
  If dlgMakeSFX.ShowModal=mrOk Then
    CreateSFX(dlgMakeSFX.edSourceArchive.Text,
              dlgMakeSFX.edDestName.Text,
              dlgMakeSFX.edSFXCaption.Text,
              dlgMakeSFX.edExtractDir.Text,
              ExtractFileDir(Application.ExeName)+'\def_mzf.sfx');
  dlgMakeSFX.Free;
end;

procedure TfrmMain.Homepage1Click(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://madmansoftware.ucoz.ru/',nil,nil,SW_SHOW);
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  Application.CreateForm(TdlgAbout, dlgAbout);
  dlgAbout.ShowModal;
  dlgAbout.Free;
end;

procedure TfrmMain.ReadLanguage;
begin
  sbMain.Panels[0].Text:=     ReadFromLanguage('Status','Init','Initializing');
  btnAdd.Caption:=            ReadFromLanguage('Buttons','btnAdd',btnAdd.Caption);
  btnCopy.Caption:=           ReadFromLanguage('Buttons','btnCopy',btnCopy.Caption);
  btnDelete.Caption:=         ReadFromLanguage('Buttons','btnDelete',btnDelete.Caption);
  btnExtract.Caption:=        ReadFromLanguage('Buttons','btnExtract',btnExtract.Caption);
  btnInfo.Caption:=           ReadFromLanguage('Buttons','btnInfo',btnInfo.Caption);
  btnOpen.Caption:=           ReadFromLanguage('Buttons','btnOpen',btnOpen.Caption);
  btnReplace.Caption:=        ReadFromLanguage('Buttons','btnReplace',btnReplace.Caption);
  btnCheck.Caption:=          ReadFromLanguage('Buttons','btnTest',btnCheck.Caption);
  DlgOpen.Title:=             ReadFromLanguage('Dialogs','dlgOpen',dlgOpen.Title);
  dlgSave.Title:=             ReadFromLanguage('Dialogs','dlgSave',dlgSave.Title);
  File1.Caption:=             ReadFromLanguage('Menu','mnuFile',File1.Caption);
  itemFile.Caption:=          File1.Caption;
  New2.Caption:=              ReadFromLanguage('Menu','mnuNew',New1.Caption);
  New1.Caption:=              New2.Caption;
  Savebackupcopyas1.Caption:= ReadFromLanguage('Menu','mnuBackUp',Savebackupcopyas1.Caption);
  Closearchive1.Caption:=     ReadFromLanguage('Menu','mnuCloseArchive',Closearchive1.Caption);
  Selectall2.Caption:=        ReadFromLanguage('Menu','mnuSelectAll',SelectAll1.Caption);
  Selectall1.Caption:=        Selectall2.Caption;
  Deselectall2.Caption:=      ReadFromLanguage('Menu','mnuDeselectAll',Deselectall1.Caption);
  Deselectall1.Caption:=      Deselectall2.Caption;
  Invertselected2.Caption:=   ReadFromLanguage('Menu','mnuInvertSelection',Invertselected1.Caption);
  Invertselected1.Caption:=   Invertselected2.Caption;
  Exit2.Caption:=             ReadFromLanguage('Menu','mnuExit',Exit1.Caption);
  Exit1.Caption:=             Exit2.Caption;
  Recent2.Caption:=           ReadFromLanguage('Menu','mnuRecent',Recent1.Caption);
  Recent1.Caption:=           Recent2.Caption;
  Commands2.Caption:=         ReadFromLanguage('Menu','mnuCommands',Commands1.Caption);
  Commands1.Caption:=         Commands2.Caption;
  Addfiles1.Caption:=         ReadFromLanguage('Menu','mnuAdd',Addfiles1.Caption);
  Renamefile1.Caption:=       ReadFromLanguage('Menu','mnuRenameFile',RenameFIle1.Caption);
  Extract1.Caption:=          ReadFromLanguage('Menu','mnuExtract',Extract1.Caption);
  Test1.Caption:=             ReadFromLanguage('Menu','mnuTest',Test1.Caption);
  Search1.Caption:=           ReadFromLanguage('Menu','mnuSearch',Search1.Caption);
  Fileinfo2.Caption:=         ReadFromLanguage('Menu','mnuFileInfo',FileInfo2.Caption);
  Executefile1.Caption:=      ReadFromLanguage('Menu','mnuExec',ExecuteFile1.Caption);
  Delete1.Caption:=           ReadFromLanguage('Menu','mnuDelete',Delete1.Caption);
  Operations2.Caption:=       ReadFromLanguage('Menu','mnuOperations',Operations2.Caption);
  Convertarchive1.Caption:=   ReadFromLanguage('Menu','mnuConvertArchive',Convertarchive1.Caption);
  CreateSXF1.Caption:=        ReadFromLanguage('Menu','mnuCreateSFX',CreateSXF1.Caption);
  Configurations1.Caption:=   ReadFromLanguage('Menu','mnuConfigure',Configurations1.Caption);
  Configure1.Caption:=        Configurations1.Caption;
  Setup1.Caption:=            ReadFromLanguage('Menu','mnuSetup',Setup1.Caption);
  Setup2.Caption:=            Setup1.Caption;
  ImportExport1.Caption:=     ReadFromLanguage('Menu','mnuImportExport',ImportExport1.Caption);
  ImportExport2.Caption:=     ImportExport1.Caption;
  Filelist1.Caption:=         ReadFromLanguage('Menu','mnuFlieList',FileList1.Caption);
  FileList2.Caption:=         Filelist1.Caption;
  Report1.Caption:=           ReadFromLanguage('Menu','mnuReport',Report1.Caption);
  Report2.Caption:=           Report1.Caption;
  List1.Caption:=             ReadFromLanguage('Menu','mnuList',List1.Caption);
  List2.Caption:=             List1.Caption;
  Smallicons1.Caption:=       ReadFromLanguage('Menu','mnuSmallIcons',Smallicons1.Caption);
  Smallicons2.Caption:=       Smallicons1.Caption;
  Bigicons1.Caption:=         ReadFromLanguage('Menu','mnuBigIcons',BigIcons1.Caption);
  Bigicons2.Caption:=         Bigicons1.Caption;
  hemes1.Caption:=            ReadFromLanguage('Menu','mnuThemes',hemes1.Caption);
  hemes2.Caption:=            hemes1.Caption;
  Language1.Caption:=         ReadFromLanguage('Menu','mnuLanguage',Language1.Caption);
  Language2.Caption:=         Language1.Caption;
  Help1.Caption:=             ReadFromLanguage('Menu','mnuHelp',Help1.Caption);
  Help2.Caption:=             Help1.Caption;
  Tips1.Caption:=             ReadFromLanguage('Menu','mnuTips',Tips1.Caption);
  Tips2.Caption:=             Tips1.Caption;
  Content1.Caption:=          ReadFromLanguage('Menu','mnuContent',Content1.Caption);
  Content2.Caption:=          Content1.Caption;
  Homepage1.Caption:=         ReadFromLanguage('Menu','mnuHomePage',Homepage1.Caption);
  Homepage2.Caption:=         Homepage1.Caption;
  Update1.Caption:=           ReadFromLanguage('Menu','mnuUpdate',Update1.Caption);
  Update2.Caption:=           Update1.Caption;
  About1.Caption:=            ReadFromLanguage('Menu','mnuAbout',About1.Caption);
  About2.Caption:=            About1.Caption;
  Open1.Caption:=             ReadFromLanguage('Menu','mnuOpen',Open1.Caption);
  Openfile1.Caption:=         ReadFromLanguage('Menu','mnuOpenFile',Openfile1.Caption);
  Encode2.Caption:=           ReadFromLanguage('Menu','mnuEncode',Encode2.Caption);
  Decode1.Caption:=           ReadFromLanguage('Menu','mnuDecode',Decode1.Caption);
  Copyselected1.Caption:=     ReadFromLanguage('Menu','mnuCopySelected',CopySelected1.Caption);
  Replaceselected1.Caption:=  ReadFromLanguage('Menu','mnuReplaceSelected',ReplaceSelected1.Caption);
  Deleteselected1.Caption:=   ReadFromLanguage('Menu','mnuDeleteSelected',DeleteSelected1.Caption);
  Fileinfo1.Caption:=         ReadFromLanguage('Menu','mnuFileInfo',FileInfo1.Caption);
  Findfile1.Caption:=         ReadFromLanguage('Menu','mnuFindFile',FindFile1.Caption);
  Favorites1.Caption:=        ReadFromLanguage('Menu','mnuFavorites',Favorites1.Caption);
  Favorites2.Caption:=        Favorites1.Caption;
  Addfavorite1.Caption:=      ReadFromLanguage('Menu','mnuAddFavorite',Addfavorite1.Caption);
  Managefavorite1.Caption:=   ReadFromLanguage('Menu','mnuManageFavorite',ManageFavorite1.Caption);
  //btnArcClose.Caption:=       ReadFromLanguage('Buttons','btnClose',btnArcClose.Caption);
end;

end.

