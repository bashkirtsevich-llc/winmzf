unit Main_u;

interface            
                                               
uses
  Windows,Messages,SysUtils,Variants,Classes,Graphics,Controls,Forms,
  Dialogs,Compress_u,ComCtrls,ToolWin,Add_u,StdCtrls,Gauges,Advanced,
  Viewer_u,Progress_u,ExtCtrls,XPMan,ImgList,Decompress_u,Buttons,Menus,
  FileBrowser_u,PNGToolBar_u,CommCtrl,SignatureDetect_u,ArchView_u,
  UniversalUnpacker_u,UniversalCompressor_u,Comment_u,ShellApi,RarView_u,
  RarExtract_u,LZMA_u,SevenZip_Compressor_u,DragDrop,DropSource,
  DragDropFile, DropTarget,PngImage;

type                                         
  TFrmMain = class(TForm)
    MainBar: TCoolBar;
    GeneralBar: TToolBar;
    BtnAdd: TToolButton;
    BtnOpen: TToolButton;
    WndPanel: TPanel;
    DirTree: TTreeView;
    FilesList: TListView;
    FolderImg: TImageList;
    WndSplit: TSplitter;
    Status: TStatusBar;
    BtnExtract: TToolButton;
    ToolBarEnableImg: TImageList;               
    pnNavigation: TPanel;
    cbAddress: TComboBox;
    Label1: TLabel;
    pnlButton: TPanel;
    BtnGoToAddress: TSpeedButton;         
    BtnShowTree: TSpeedButton;
    btnListType: TSpeedButton;
    mnuListType: TPopupMenu;
    N1231: TMenuItem;
    List1: TMenuItem;
    Report1: TMenuItem;
    Smallicons1: TMenuItem;
    N1: TMenuItem;
    DlgOpen: TOpenDialog;
    BrowserIcons: TImageList;
    BrowserIconsBig: TImageList;
    MnuMain: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    BtnLevelUp: TSpeedButton;
    Open1: TMenuItem;
    N2: TMenuItem;
    Closearchive1: TMenuItem;
    Recentfiles1: TMenuItem;
    N3: TMenuItem;
    Exit2: TMenuItem;
    ToolBarDisableImg: TImageList;
    BrowserBar: TToolBar;
    BtnCopy: TToolButton;
    BtnReplace: TToolButton;
    BtnDelete: TToolButton;
    BtnSearch: TToolButton;
    mnuBrowser: TPopupMenu;
    Commands1: TMenuItem;
    Addtoarchive1: TMenuItem;
    Extractfiles1: TMenuItem;
    N4: TMenuItem;
    Copyfles1: TMenuItem;
    Replacefiles1: TMenuItem;
    Deletefiles1: TMenuItem;
    Archivecomment1: TMenuItem;
    Selectall1: TMenuItem;
    N5: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    Cut1: TMenuItem;
    Rename1: TMenuItem;
    N01: TMenuItem;
    mnuDirTreeInBrowser: TPopupMenu;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    Delete2: TMenuItem;
    N6: TMenuItem;
    Renamre1: TMenuItem;
    DropManager: TDropFileSource;
    FolderIcon: TImageList;
    DropFileTargetManager: TDropFileTarget;
    Updateshell1: TMenuItem;
    mnuArchiver: TPopupMenu;
    Open2: TMenuItem;
    Extract1: TMenuItem;
    Rename2: TMenuItem;
    Delete3: TMenuItem;
    N7: TMenuItem;
    Selectall2: TMenuItem;
    procedure BtnAddClick(Sender: TObject);
    procedure ShowProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
    procedure BtnOpenClick(Sender: TObject);
    procedure OnShowInfo(Sender:TObject;Var FileHead:TFileHead);
    procedure OnError(Sender:TObject;ErrorMsg:ShortString);
    procedure CompressorOnBegin(Sender:TObject);
    procedure CompressorOnEnd(Sender:TObject);
    procedure ViewerGetPass(Sender:TObject;Var Pass:ShortString);
    procedure DirTreeChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure BtnExtractClick(Sender: TObject);
    procedure DeCompressorOnBegin(Sender:TObject);
    procedure OverWritePrompt(Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode);
    procedure BtnGoToAddressClick(Sender: TObject);
    procedure BtnShowTreeClick(Sender: TObject);
    procedure pnNavigationResize(Sender: TObject);
    procedure btnListTypeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Smallicons1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FilesListDblClick(Sender: TObject);
    procedure FilesListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DirTreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure BtnLevelUpClick(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure Closearchive1Click(Sender: TObject);
    procedure cbAddressKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnSearchClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure Archivecomment1Click(Sender: TObject);
    procedure Selectall1Click(Sender: TObject);
    procedure mnuBrowserPopup(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnReplaceClick(Sender: TObject);
    procedure FilesListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FilesListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DirTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Copy2Click(Sender: TObject);
    procedure DropFileTargetManagerDrop(Sender: TObject;
      ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
    procedure Rename2Click(Sender: TObject);
  private
    Procedure GoToNode(SubDir:String);
    procedure LoadIcons;
    procedure DropFiles (var Msg: TMessage); message WM_DROPFILES;
    procedure CloseArchive;
    procedure RarShowFiles(Sender:TObject;FileInfo:TRarFileInfo);
    procedure SevenZipShowFiles(Sender:TObject;FileInfo:T7ZipFileInfo);
    { Private declarations }
  public
    procedure OpenArchive(ArchiveFileName:String);
    procedure AddIntoArchive(Dir:String;List:TStringList);
    procedure CopyIntoDirectory(List:TStringList;DestDir:String);
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;
  DlgProgress:TDlgProgress;
  Path,FileName,Archive,Comment:String;
  ViewMode:TDisplayMode;
  ArchiveType:TFileType;
  {LogoResource:TResourceStream;
  PNG:TPNGObject;}
implementation

uses Extract_u, OverWrite_u,DeleteFiles_u, DragAndDrop_u, CopyFiles_u;

{$R *.dfm}
{$R Img\Buttons.res}
{$R BrowserImages.res}

(*{$SETPEFlAGS IMAGE_FILE_RELOCS_STRIPPED or IMAGE_FILE_DEBUG_STRIPPED or
  IMAGE_FILE_LINE_NUMS_STRIPPED or IMAGE_FILE_LOCAL_SYMS_STRIPPED or
  IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP or IMAGE_FILE_NET_RUN_FROM_SWAP}   *)
//{$R PngLogo.res}

procedure TFrmMain.DropFiles(var Msg: TMessage); 
var i,amount,size:integer;
    Filename: PChar;
    List:TStringList;
begin 
{  inherited;
  If DropManager.Files.Count<>0 Then Exit;
  Path:=cbAddress.Text;
  Application.CreateForm(TDlgOperation,DlgOperation);
  List:=TStringList.Create;
  Amount := DragQueryFile(Msg.WParam, $FFFFFFFF, Filename, 255);
  for i := 0 to (Amount - 1) do
    begin
      size := DragQueryFile(Msg.WParam, i , nil, 0) + 1;
      Filename:= StrAlloc(size);
      DragQueryFile(Msg.WParam,i , Filename, size);
      List.add(StrPas(Filename));
      StrDispose(Filename);
    end;
  If (List.Count>1)Or(Not FileExists(List.Strings[0])) Then
    DlgOperation.BtnOpen.Enabled:=False;
  If (ViewMode=dmArchiver)Or(Path='') Then
    DlgOperation.BtnCopy.Enabled:=False;
  DragFinish(Msg.WParam);
  Case DlgOperation.ShowModal Of
    mrYes:OpenArchive(List.Strings[0]);//Open file
    mrAll:AddIntoArchive('',List);//Add into archive
    mrYesToAll:CopyIntoDirectory(List,Path);//Copy here  
  End;
  DlgOperation.Free;
  List.Free;   }
end;

Procedure TFrmMain.CopyIntoDirectory(List:TStringList;DestDir:String);
Var Index:LongWord;
    //Dir:String;
begin
  If ViewMode<>dmBrowser Then Exit;
  If cbAddress.Text='' Then Exit;
  //Files:=TStringList.Create;
  Application.CreateForm(TDlgCopyMoveFiles,DlgCopyMoveFiles);
  //Dir:=cbAddress.Text;
  //If Dir[Length(Dir)]<>'\' Then Dir:=Dir+'\';

  For Index:=0 To List.Count-1 Do
    Begin
      If Not FileExists(List.Strings[Index]) Then SearchDir(List.Strings[Index],List) Else
      DlgCopyMoveFiles.FilesList.Add(List.strings[Index]);
    End;
  DlgCopyMoveFiles.Mode:=cmCopy;
  DlgCopyMoveFiles.Caption:='Copy files';
  DlgCopyMoveFiles.BtnCopyOrMove.Caption:='Copy';

  //Files.Free;
  DlgCopyMoveFiles.edDestDir.Text:=DestDir;
  DlgCopyMoveFiles.ShowModal;
  DlgCopyMoveFiles.Free;
End;

Procedure TFrmMain.CloseArchive;
Begin
    DirTree.Items.Clear;
  FilesList.Clear;
  ViewMode:=dmBrowser;
  Status.Panels.Items[1].Text:='Browser';
  BtnCopy.Enabled:=True;
  BtnReplace.Enabled:=True;
  Copyfles1.Enabled:=True;
  Replacefiles1.Enabled:=True;
  DirTree.ShowRoot:=False;
  Archivecomment1.Enabled:=False;
  //BtnSearch.ImageIndex:=7;
  cbAddress.Clear;
  GetDirTree;
  Comment:='';
End;

Procedure TFrmMain.AddIntoArchive(Dir:String;List:TStringList);
Var Compressor:TMZFCompressor;
    UniversalCompressor:TUniversalCompressor;
    SevenZipCompressor:TSevenZipCompressor;
    Index:Integer;
    ArchCompressMethod:TFileType;
    Temp:String;
begin
  Application.CreateForm(TDlgAdd, DlgAdd);
  DlgAdd.FileNameCombo.Text:=Dir;
    For Index:=0 To List.Count-1 Do
      Begin
        Temp:=List.Strings[Index];
        If Not FileExists(Temp) Then Temp:=Temp+'\*.*';
        DlgAdd.lvFiles.Items.Add.Caption:=Temp;
      End;
  If DlgAdd.ShowModal=mrOk Then
    Begin
    //!Attention! !Correction!
      Case DlgAdd.cbCompressMethod.ItemIndex Of
        0:ArchCompressMethod:=ftMZF;
        1:ArchCompressMethod:=ftZIP;
        2:ArchCompressMethod:=ftBH;
        3:ArchCompressMethod:=ftLHA;
        4:ArchCompressMethod:=ftJAR;
        5:ArchCompressMethod:=ftCAB;
        6:ArchCompressMethod:=ftTAR;
      End;
    //
      Case ArchCompressMethod Of
        ftMZF: Begin
          Compressor:=TMZFCompressor.Create;
          Compressor.OnBegin:=CompressorOnBegin;
          Compressor.OnEnd:=CompressorOnEnd;
          Compressor.OnProgress:=ShowProgress;
          Compressor.OnGetPassword:=ViewerGetPass;
          Compressor.ArchiveFileName:=DlgAdd.FileNameCombo.Text;
          //Compressor.EncodeMode:=
          Compressor.Commentary:=DlgAdd.CommentMemo.Text;
          Compressor.EncodeFileContain:=DlgAdd.cbEncodeFile.Checked;
          Compressor.EncodeHeadInfo:=DlgAdd.cbEncodeHead.Checked;
          Compressor.ArchReadOnly:=DlgAdd.cbArchiveReadOnly.Checked;
          Compressor.ArcHidden:=DlgAdd.cbArchiveHidden.Checked;
          Compressor.ArcArchive:=DlgAdd.cbArchiveArchived.Checked;
          Case DlgAdd.rgEncode.ItemIndex Of
            0:Compressor.EncodeMode:=emBeforeCompress;
            1:Compressor.EncodeMode:=emBeforeCompress;
            2:Compressor.EncodeMode:=emBeforeAfter;
          End;
          Compressor.Password:=DlgAdd.edPassword.Text;
          {Case DlgAdd.tbLevel.Position Of
            0:Compressor.Level:=clNone;
            1:Compressor.Level:=clFastest;
            2:Compressor.Level:=clDefault;
            3:Compressor.Level:=clMax;
          End; } //!Attention! !Correction!
          For Index:=0 To DlgAdd.lvFiles.Items.Count-1 Do
            Compressor.FileList.Add(DlgAdd.lvFiles.Items.Item[Index].Caption);
          Compressor.CreateArchive;
          Compressor.Free;   //Attention mystic error
        End;
        ftZIP,ftBH,ftLHA,ftJAR,ftCAB,ftTAR:Begin                  
          UniversalCompressor:=TUniversalCompressor.Create;
          For Index:=0 To DlgAdd.lvFiles.Items.Count-1 Do
            UniversalCompressor.FileSpec.Add(DlgAdd.lvFiles.Items.Item[Index].Caption);
          UniversalCompressor.OnBegin:=CompressorOnBegin;
          UniversalCompressor.OnEnd:=CompressorOnEnd;
          UniversalCompressor.OnProgress:=ShowProgress;
          UniversalCompressor.FileType:=ArchCompressMethod;
          UniversalCompressor.ReadingOnly:=DlgAdd.cbArchiveReadOnly.Checked;
          UniversalCompressor.Hidden:=DlgAdd.cbArchiveHidden.Checked;
          UniversalCompressor.Archived:=DlgAdd.cbArchiveArchived.Checked;
          UniversalCompressor.ArchiveFile:=DlgAdd.FileNameCombo.Text;
          UniversalCompressor.Comment:=DlgAdd.CommentMemo.Text;
          UniversalCompressor.EncodeHeader:=DlgAdd.cbEncodeHead.Checked;
          UniversalCompressor.Password:=DlgAdd.edPassword.Text;
          Case DlgAdd.tbLevel.Position Of
            0:UniversalCompressor.CompressValue:=cvFastest;
            1:UniversalCompressor.CompressValue:=cvNormal;
            2:UniversalCompressor.CompressValue:=cvGood;
            3:UniversalCompressor.CompressValue:=cvExtra;
          End;
          UniversalCompressor.Compress;
          UniversalCompressor.Free;
        End;
        ft7Z:
        Begin
          SevenZipCompressor:=TSevenZipCompressor.Create;
          SevenZipCompressor.FileName:= DlgAdd.FileNameCombo.Text;
          SevenZipCompressor.OnBegin:=CompressorOnBegin;
          SevenZipCompressor.OnEnd:=CompressorOnEnd;
          SevenZipCompressor.OnProgress:=ShowProgress;
          SevenZipCompressor.ReadingOnly:=DlgAdd.cbArchiveReadOnly.Checked;
          SevenZipCompressor.Hidden:=DlgAdd.cbArchiveHidden.Checked;
          SevenZipCompressor.Archived:=DlgAdd.cbArchiveArchived.Checked;
          SevenZipCompressor.Password:=DlgAdd.edPassword.Text;
          For Index:=0 To DlgAdd.lvFiles.Items.Count-1 Do
            SevenZipCompressor.FileSpec.Add(DlgAdd.lvFiles.Items.Item[Index].Caption);
          Case DlgAdd.tbLevel.Position Of
            0:SevenZipCompressor.CompressLevel:=clFast;
            1:SevenZipCompressor.CompressLevel:=clNormal;
            2:SevenZipCompressor.CompressLevel:=clMaximum;
            3:SevenZipCompressor.CompressLevel:=clUltra;
          End;
          SevenZipCompressor.Compress;
          SevenZipCompressor.Free;
        End;
      End;
    End;
  DlgAdd.Free;
End;

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

procedure TFrmMain.BtnAddClick(Sender: TObject);
Var Compressor:TMZFCompressor;
    UniversalCompressor:TUniversalCompressor;
    SevenZipCompressor:TSevenZipCompressor;
    Index:Integer;
    Temp:String;
    ArchCompressMethod:TFileType;
begin
  Application.CreateForm(TDlgAdd, DlgAdd);
  If (ViewMode=dmBrowser)And(cbAddress.Text<>'') Then
  If (FilesList.SelCount>0) Then
    For Index:=0 To FilesList.Items.Count-1 Do
      Begin
        If FilesList.Items.Item[Index].Selected Then
          Begin
            If CharInString('D',FilesList.Items.Item[Index].SubItems.Strings[5]) Then
              DlgAdd.lvFiles.Items.Add.Caption:= FilesList.Items.Item[Index].SubItems.Strings[7]+
                                                 FilesList.Items.Item[Index].Caption+'\*.*'
                Else
              DlgAdd.lvFiles.Items.Add.Caption:= FilesList.Items.Item[Index].SubItems.Strings[7]+
                                                 FilesList.Items.Item[Index].Caption;
          End;
        Temp:=cbAddress.Text;
        Temp:=Temp+'\';
        DlgAdd.FileNameCombo.Text:=Temp+FilesList.Selected.Caption+'.MZF';
      End Else
        Begin
          DlgAdd.lvFiles.Items.Add.Caption:=cbAddress.Text+'\*.*';
        End;
  If DlgAdd.ShowModal=mrOk Then
    Begin
    //!Attention! !Correction!
      Case DlgAdd.cbCompressMethod.ItemIndex Of
        0:ArchCompressMethod:=ftMZF;
        1:ArchCompressMethod:=ft7Z;
        2:ArchCompressMethod:=ftZIP;
        3:ArchCompressMethod:=ftBH;
        4:ArchCompressMethod:=ftLHA;
        5:ArchCompressMethod:=ftJAR;
        6:ArchCompressMethod:=ftCAB;
        7:ArchCompressMethod:=ftTAR;
      End;
    //
      Case ArchCompressMethod Of
        ftMZF: Begin
          Compressor:=TMZFCompressor.Create;
          Compressor.OnBegin:=CompressorOnBegin;
          Compressor.OnEnd:=CompressorOnEnd;
          Compressor.OnProgress:=ShowProgress;
          Compressor.OnGetPassword:=ViewerGetPass;
          Compressor.ArchiveFileName:=DlgAdd.FileNameCombo.Text;
          //Compressor.EncodeMode:=
          Compressor.Commentary:=DlgAdd.CommentMemo.Text;
          Compressor.EncodeFileContain:=DlgAdd.cbEncodeFile.Checked;
          Compressor.EncodeHeadInfo:=DlgAdd.cbEncodeHead.Checked;
          Compressor.ArchReadOnly:=DlgAdd.cbArchiveReadOnly.Checked;
          Compressor.ArcHidden:=DlgAdd.cbArchiveHidden.Checked;
          Compressor.ArcArchive:=DlgAdd.cbArchiveArchived.Checked;
          Case DlgAdd.rgEncode.ItemIndex Of
            0:Compressor.EncodeMode:=emBeforeCompress;
            1:Compressor.EncodeMode:=emBeforeCompress;
            2:Compressor.EncodeMode:=emBeforeAfter;
          End;
          Compressor.Password:=DlgAdd.edPassword.Text;
          {Case DlgAdd.tbLevel.Position Of
            0:Compressor.Level:=clNone;
            1:Compressor.Level:=clFastest;
            2:Compressor.Level:=clDefault;
            3:Compressor.Level:=clMax;
          End;}
          Compressor.Level:=DlgAdd.InsideCompressLevel; //!Attention!
          For Index:=0 To DlgAdd.lvFiles.Items.Count-1 Do
            Compressor.FileList.Add(DlgAdd.lvFiles.Items.Item[Index].Caption);
          Compressor.CreateArchive;
          Compressor.Free;   //Attention mystic error
        End;
        ftZIP,ftBH,ftLHA,ftJAR,ftCAB,ftTAR:Begin                  
          UniversalCompressor:=TUniversalCompressor.Create;
          For Index:=0 To DlgAdd.lvFiles.Items.Count-1 Do
            UniversalCompressor.FileSpec.Add(DlgAdd.lvFiles.Items.Item[Index].Caption);
          UniversalCompressor.OnBegin:=CompressorOnBegin;            
          UniversalCompressor.OnEnd:=CompressorOnEnd;
          UniversalCompressor.OnProgress:=ShowProgress;                 
          UniversalCompressor.FileType:=ArchCompressMethod;    
          UniversalCompressor.ReadingOnly:=DlgAdd.cbArchiveReadOnly.Checked;
          UniversalCompressor.Hidden:=DlgAdd.cbArchiveHidden.Checked;
          UniversalCompressor.Archived:=DlgAdd.cbArchiveArchived.Checked;
          UniversalCompressor.ArchiveFile:=DlgAdd.FileNameCombo.Text;
          UniversalCompressor.Comment:=DlgAdd.CommentMemo.Text;
          UniversalCompressor.EncodeHeader:=DlgAdd.cbEncodeHead.Checked;
          UniversalCompressor.Password:=DlgAdd.edPassword.Text;
          Case DlgAdd.tbLevel.Position Of
            0:UniversalCompressor.CompressValue:=cvFastest;
            1:UniversalCompressor.CompressValue:=cvNormal;
            2:UniversalCompressor.CompressValue:=cvGood;
            3:UniversalCompressor.CompressValue:=cvExtra;
          End;
          UniversalCompressor.Compress;
          UniversalCompressor.Free;
        End;
        ft7Z:
        Begin
          SevenZipCompressor:=TSevenZipCompressor.Create;
          SevenZipCompressor.FileName:= DlgAdd.FileNameCombo.Text;
          SevenZipCompressor.OnBegin:=CompressorOnBegin;
          SevenZipCompressor.OnEnd:=CompressorOnEnd;
          SevenZipCompressor.OnProgress:=ShowProgress;
          SevenZipCompressor.ReadingOnly:=DlgAdd.cbArchiveReadOnly.Checked;
          SevenZipCompressor.Hidden:=DlgAdd.cbArchiveHidden.Checked;
          SevenZipCompressor.Archived:=DlgAdd.cbArchiveArchived.Checked;
          SevenZipCompressor.Password:=DlgAdd.edPassword.Text;
          For Index:=0 To DlgAdd.lvFiles.Items.Count-1 Do
            SevenZipCompressor.FileSpec.Add(DlgAdd.lvFiles.Items.Item[Index].Caption);
          Case DlgAdd.tbLevel.Position Of
            0:SevenZipCompressor.CompressLevel:=clFast;
            1:SevenZipCompressor.CompressLevel:=clNormal;
            2:SevenZipCompressor.CompressLevel:=clMaximum;
            3:SevenZipCompressor.CompressLevel:=clUltra;
          End;
          SevenZipCompressor.Compress;
          SevenZipCompressor.Free;
        End;
      End;
    End;
  DlgAdd.Free;
  BtnGoToAddressClick(Sender);
end;

procedure TFrmMain.CompressorOnBegin(Sender:TObject);
begin
  DlgProgress:=TDlgProgress.Create(Self);
  Self.Enabled:=False;
  DlgProgress.AnimOperation.ResName:='COMPRESSING';
  DlgProgress.AnimOperation.Active:=True;
  DlgProgress.Show;
  DlgProgress.Update;
end;

procedure TFrmMain.OverWritePrompt(Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode);
Begin
  //Mode:=omOverWriteAll;
  Application.CreateForm(TDlgOverWrite,DlgOverWrite);
  DlgOverWrite.Label2.Caption:=Format('File name: %s',[ExtractFileName(AFile.LongFileName)]);
  DlgOverWrite.Label3.Caption:=Format('File size: %d',[AFile.Size]);
  DlgOverWrite.Label4.Caption:=Format('File attr: %s',[GetAttributesAsString(AFile.Attr)]);
  DlgOverWrite.Label5.Caption:=Format('File creation date: %s',[FormatDateTime(TimeFormat,AFile.FileCreateDate)]);
  DlgOverWrite.Label6.Caption:=Format('File modify date: %s',[FormatDateTime(TimeFormat,AFile.FileModifyDate)]);
  DlgOverWrite.Label7.Caption:=Format('File access date: %s',[FormatDateTime(TimeFormat,AFile.FileOpenDate)]);

  DlgOverWrite.ShowModal;
  If DlgOverWrite.OverWrite Then
    Begin
      If DlgOverWrite.cbSetToAll.Checked Then
        Mode:=omOverwriteAll Else
        Mode:=omRewrite;
    End Else
      Begin
        If DlgOverWrite.cbSetToAll.Checked Then
          Mode:=omSkipAll Else
          Mode:=omSkip;
      End;
  DlgOverWrite.Free;
End;

procedure TFrmMain.DeCompressorOnBegin(Sender:TObject);
begin
  DlgProgress:=TDlgProgress.Create(Self);
  Self.Enabled:=False;
  DlgProgress.AnimOperation.ResName:='DECOMPRESSING';
  DlgProgress.AnimOperation.Active:=True;
  DlgProgress.Show;
  DlgProgress.Update;
end;

procedure TFrmMain.CompressorOnEnd(Sender:TObject);
begin
  DlgProgress.Free;
  Self.Enabled:=True;
end;

procedure TFrmMain.ShowProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
Begin
  DlgProgress.gFile.Position:=FProgress;
  DlgProgress.gArchive.Position:=AProgress;
  Abort:=DlgProgress.Abort;
  DlgProgress.Update;
End;

procedure TFrmMain.OnShowInfo(Sender:TObject;Var FileHead:TFileHead);
Var //ItemIndex:Word;
    Item:TListItem;
begin                                                   
  If (ExtractFileDir(FileHead.LongFileName)=Path)Or(Not DirTree.Visible) Then
    Begin
      Item:=FilesList.Items.Add;
      Item.ImageIndex:=0;
      Item.Caption:=ExtractFileName(FileHead.LongFileName);
      //ItemIndex:=FilesList.Items.Count-1;
      Item.SubItems.Add(IntToStr(FileHead.Size));
      Item.SubItems.Add(IntToStr(FileHead.PackedSize));
      Item.SubItems.Add(FormatDateTime(TimeFormat,FileHead.FileCreateDate));
      Item.SubItems.Add(FormatDateTime(TimeFormat,FileHead.FileModifyDate));
      Item.SubItems.Add(FormatDateTime(TimeFormat,FileHead.FileOpenDate));
      Item.SubItems.Add(GetAttributesAsString(FileHead.Attr));
      Item.SubItems.Add(IntToHex(FileHead.CRC,8));
      Item.SubItems.Add(ExtractFileDir(FileHead.LongFileName));
    End;
end;

procedure TFrmMain.OnError(Sender:TObject;ErrorMsg:ShortString);
begin
  ShowMessage(ErrorMsg);
end;

procedure TFrmMain.ViewerGetPass(Sender:TObject;Var Pass:ShortString);
Var temp:String;
begin
  //pass:='111';
  Temp:='';
  If InputQuery('Password','Enter password',Temp) Then
    Pass:=Temp  
end;

procedure TFrmMain.RarShowFiles(Sender:TObject;FileInfo:TRarFileInfo);
Var Item:TListItem;
Begin
  Item:=FilesList.Items.Add;
  Item.Caption:=FileInfo.FileName;
  Item.SubItems.Add(IntToStr(FileInfo.Size));
  Item.SubItems.Add(IntToStr(FileInfo.PackedSize));
  Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.CreationTime));
  Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.ModifyTime));
  Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.AccessTime));
  Item.SubItems.Add(FileInfo.Attr);
  Item.SubItems.Add(IntToHex(FileInfo.CRC,8));
  Item.SubItems.Add(Path);
End;

procedure TFrmMain.SevenZipShowFiles(Sender:TObject;FileInfo:T7ZipFileInfo);
Var Item:TListItem;
Begin
  Item:=FilesList.Items.Add;
  Item.Caption:=FileInfo.FileName;
  Item.SubItems.Add(IntToStr(FileInfo.Size));
  Item.SubItems.Add(IntToStr(FileInfo.PackedSize));
  Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.CreationTime));
  Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.ModifyTime));
  Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.AccessTime));
  Item.SubItems.Add(GetAttributesAsString(FileInfo.Attr));
  Item.SubItems.Add(IntToHex(FileInfo.CRC,8));
  Item.SubItems.Add(Path);
  Item.SubItems.Add(IntToStr(FileInfo.ID));
End;

procedure TFrmMain.OpenArchive(ArchiveFileName:String);
var Viewer:TMZFViewer;
    RarViewer:TRarViewer;
    SevenZipViewer:TSevenZipViewer;
    Icons:TBitmap;
    Dirs,Files:TStringList;
    AType:String;
    Index:LongWord;
    Item:TListItem;
    FileInfo:TFileInfo;
begin
  //If Not Selected;
  If ArchiveFileName='' Then Exit;
  If Not FileExists(ArchiveFileName) Then Exit;
  ArchiveType:=GetArchiveTypeBySignature(ArchiveFileName);
  If ArchiveType=ftAnyFile Then
    Begin
      ShellOpenFile(Handle,ArchiveFileName);
      ViewMode:=dmBrowser;
      If ViewMode<>dmBrowser Then Exit;
      //GetDirTree;           
      Exit;                                                       
    End;
    CloseArchive;
    BtnCopy.Enabled:=False;
    BtnReplace.Enabled:=False;
    Copyfles1.Enabled:=False;
    Replacefiles1.Enabled:=False;
    Archivecomment1.Enabled:=True;
    //BtnSearch.ImageIndex:=6;
  FileName:=ArchiveFileName;
    BrowserIcons.BkColor:=FilesList.Color;
    BrowserIconsBig.BkColor:=FilesList.Color;
    BrowserIcons.BlendColor:=FilesList.Color;
    BrowserIconsBig.BlendColor:=FilesList.Color;
    Icons:=TBitmap.Create;
    Icons.LoadFromResourceName(hInstance,'BROWSERBIG');
    BrowserIconsBig.Clear;
    BrowserIconsBig.Add(Icons,Nil);
    Icons.LoadFromResourceName(hInstance,'BROWSERLARGE');
    BrowserIcons.Clear;
    BrowserIcons.Add(Icons,Nil);
    Icons.Free;
  ViewMode:=dmArchiver;
  DirTree.Items.Clear;
  FilesList.Clear;
  DirTree.ShowRoot:=True;
  Archive:='';
  cbAddress.Clear;
  //
  Path:=Format(RootCaption,[ExtractFileName(ArchiveFileName)]);
  Caption:=Format(Title,[ExtractFileName(ArchiveFileName)]);
  Application.Title:=Caption;
  //
  Case ArchiveType Of
    ftMZF:Begin
      Viewer:=TMZFViewer.Create();
      Viewer.ArchiveName:=ArchiveFileName;
      Archive:=Viewer.ArchiveName;
      Viewer.Path:=Path;
      //cbAddress.Text:=Path;
      Viewer.OnShowFileINfo:=OnShowInfo;
      Viewer.OnError:=OnError;
      Viewer.OnPassQuest:=ViewerGetPass;
      Viewer.ShowFiles;
      Comment:=Viewer.Commentary;
      //Label1.Caption:=Viewer.Commentary;
      FillTreeViewWithDirs(DirTree,Viewer.DirList); //<<<Show files
      Status.Panels.Items[1].Text:='MZF';
      Viewer.Free;
    End;
    ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,ftArj,ftPKG5,ftACE2:
      Begin
        Dirs:=TStringList.Create;
        Files:=TStringList.Create;
        GetFilesAndDirs(Dirs,Files,ArchiveFileName,AType,Comment);
        Archive:=ArchiveFileName;
        Caption:=Format(Title,[ExtractFileName(ArchiveFileName)]);
        Application.Title:=Caption;
        Status.Panels.Items[1].Text:=AType;
        FillTreeViewWithDirs(DirTree,Dirs);
        //Path:=Format(RootCaption,[ExtractFileName(ArchiveFileName)]);
        For Index:=0 To Files.Count-1 Do
          Begin
            //showmessage(files.Strings[index]);
            If ExtractFileDir(Files.Strings[Index])='' Then
              Begin
                Item:=FilesList.Items.Add;
                GetFileInfo(ArchiveFileName,Files.Strings[Index],FileInfo);
                Item.Caption:=ExtractFileName(Files.Strings[Index]);
                Item.SubItems.Add(IntToStr(FileInfo.Size));
                Item.SubItems.Add(IntToStr(FileInfo.PackedSize));
                Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.CreationTime));
                Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.ModifyTime));
                Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.AccessTime));
                Item.SubItems.Add({GetAttributesAsString(FileInfo.Attr)}'NULL');
                Item.SubItems.Add(IntToHex(FileInfo.CRC,8));
                Item.SubItems.Add(Path);
              End;
          End;
        Dirs.Free;
        Files.Free;
      End;
    ftRar:
      Begin
        RarViewer:=TRarViewer.Create;
        RarViewer.FileName:=ArchiveFileName;
        RarViewer.OnShowFileInfo:=RarShowFiles;
        RarViewer.GetDirs;
        FillTreeViewWithDirs(DirTree,RarViewer.Dirs);
        Comment:=RarViewer.Comment;
        RarViewer.GetFilesFromDir('');
        RarViewer.Free;
        Archive:=ArchiveFileName;
      End;
    ft7Z:
      Begin
        SevenZipViewer:=TSevenZipViewer.Create;
        SevenZipViewer.OnShowFileInfo:=SevenZipShowFiles;
        Archive:=ArchiveFileName;
        SevenZipViewer.FileName:=Archive;
        SevenZipViewer.GetDirs;
        SevenZipViewer.GetFilesFromDir('');
        FillTreeViewWithDirs(DirTree,SevenZipViewer.Directories);
        SevenZipViewer.Free;
      End;
  End;
  Case ArchiveType Of
    ftBH,ftLha,ftJar,ftTar,ftZip,ftGZip,ftMZF:Begin
        BtnAdd.Enabled:=True;
        BtnDelete.Enabled:=True;
        Deletefiles1.Enabled:=True;
        Addtoarchive1.Enabled:=True;
      End;
    ftArj,ftZoo,ftRar,ftCab,ftArc,ftAce2,ft7Z:Begin
        BtnAdd.Enabled:=False;
        BtnDelete.Enabled:=False;
        Deletefiles1.Enabled:=False;
        Addtoarchive1.Enabled:=False;
      End;
  End;
End;

procedure TFrmMain.BtnOpenClick(Sender: TObject);
var Temp:String;
begin
  If (FilesList.ItemIndex<>-1)And(ViewMode=dmBrowser) Then
    Begin
      Temp:=cbAddress.Text+'\'+FilesList.Items.Item[FilesList.ItemIndex].Caption;
      If FileExists(Temp) Then
        Begin
          OpenArchive(Temp);
          cbAddress.Clear;
          Exit;
        End;
    End;
  DlgOpen.InitialDir:=cbAddress.Text;
  If Not DlgOpen.Execute Then Exit;
  OpenArchive(dlgopen.FileName);
end;

procedure TFrmMain.DirTreeChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
var Viewer:TMZFViewer;
    RarViewer:TRarViewer;
    SevenZipViewer:TSevenZipViewer;
    Temp:String;
    Dirs,Files:TStringList;
    AType:String;
    Index:LongWord;
    Item:TListItem;
    FileInfo:TFileInfo;
begin
  //If Node<>DirTree.Selected Then Exit;
  If ViewMode=dmArchiver Then
    Begin
      If DirTree.Tag<>0 Then Exit;
      Path:=GetPathToNode(Node,'\');
      FilesList.Clear;
      Node.Expanded:=True;
      Temp:=Path;
      Delete(temp,1,Pos(']',Temp));
      cbAddress.Text:=Temp;
      Case ArchiveType Of
      ftMZF:Begin
          Viewer:=TMZFViewer.Create();                                           
          Viewer.ArchiveName:=FileName;
          Viewer.Path:=Path;
          Viewer.OnShowFileINfo:=OnShowInfo;
          Viewer.OnError:=OnError;
          Viewer.OnPassQuest:=ViewerGetPass;
          Viewer.ShowFiles;                                  
          Viewer.Free;
        End;
      ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,ftArj,ftPKG5,ftACE2:Begin
        Dirs:=TStringList.Create;
        Files:=TStringList.Create;
        GetFilesAndDirs(Dirs,Files,FileName,AType,Comment);
        FillTreeViewWithDirs(DirTree,Dirs);
        Delete(Path,1,Pos(']',Path)+1);
        For Index:=0 To Files.Count-1 Do                     
          Begin                                            
            If ExtractFileDir(Files.Strings[Index])=Path Then
              Begin
                Item:=FilesList.Items.Add;
                Item.Caption:=ExtractFileName(Files.Strings[Index]);
                GetFileInfo(FileName,Files.Strings[Index],FileInfo);
                Item.SubItems.Add(IntToStr(FileInfo.Size));
                Item.SubItems.Add(IntToStr(FileInfo.PackedSize));
                Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.CreationTime));
                Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.ModifyTime));
                Item.SubItems.Add(FormatDateTime(TimeFormat,FileInfo.AccessTime));
                Item.SubItems.Add({GetAttributesAsString(FileInfo.Attr)}'NULL');
                Item.SubItems.Add(IntToHex(FileInfo.CRC,8));
                Item.SubItems.Add(Path);
              End;
            End;
        End;
      ftRar:
        Begin
          RarViewer:=TRarViewer.Create;
          RarViewer.FileName:=FileName;
          RarViewer.OnShowFileInfo:=RarShowFiles;
          //RarViewer.GetDirs;
          //FillTreeViewWithDirs(DirTree,RarViewer.Dirs);
          Delete(Temp,1,1);
          //ShowMessage(Temp);
          RarViewer.GetFilesFromDir(Temp);
          RarViewer.Free;
        End;
      ft7Z:
        Begin
          SevenZipViewer:=TSevenZipViewer.Create;
          SevenZipViewer.OnShowFileInfo:=SevenZipShowFiles;
          Archive:=FileName;
          SevenZipViewer.FileName:=Archive;
          Delete(Temp,1,1);
          SevenZipViewer.GetFilesFromDir(Temp);
          SevenZipViewer.Free;
        End;
      End;
    End Else
      ChangeTree(Node);
end;

procedure TFrmMain.BtnExtractClick(Sender: TObject);
Var Decompressor:TMZFDecompressor;
    UnPacker:TUnPacker;
    RarDecompressor:TRarDecompressor;
    SevenZipDecompressor:TSevenZipViewer;
    Index:LongWord;
    Temp:String;
begin
  If Archive='' Then Exit;
  Application.CreateForm(TDlgExtract, DlgExtract);
  If FilesList.SelCount>0 Then
    DlgExtract.rgExtractMode.ItemIndex:=1
  Else
    DlgExtract.rgExtractMode.ItemIndex:=0;
  If DlgExtract.ShowModal=mrOk Then
    Begin
      Case ArchiveType Of
        ftMZF:Begin
          Decompressor:=TMZFDecompressor.Create;
          If FilesList.SelCount>0 Then
          For Index:=0 To FilesList.Items.Count-1 Do
            If FilesList.Items.Item[Index].Selected Then
              Begin
                Temp:=FilesList.Items.Item[Index].SubItems.Strings[7]+'\'+
                      FilesList.Items.Item[Index].Caption;
                Delete(Temp,1,Pos(']',Temp));     //???? UnKnow +1 or not
                Decompressor.FileList.Add(temp);
                //showmessage(temp);
                Temp:='';
              End;
          Case DlgExtract.rgUpdateMode.ItemIndex Of
            0:Decompressor.FileOverwriteMode:=omOverwriteAll;
            1:Decompressor.FileOverwriteMode:=omSkipAll;
            2:Decompressor.FileOverwriteMode:=omUnknown;
          End;
          Decompressor.UpdateModifyTime:=DlgExtract.cbUpdateModifyTime.Checked;
          Decompressor.UpdateLastAccesstime:=DlgExtract.cbUpdateAccessTime.Checked;
          Decompressor.DeleteAfterExtract:=DlgExtract.rgDeleteArchive.ItemIndex>=1;
          Decompressor.ArchiveFile:=Archive;
          Decompressor.OnBegin:=DeCompressorOnBegin;
          Decompressor.OnEnd:=CompressorOnEnd;
          Decompressor.OnProgress:=ShowProgress;
          Decompressor.OnGetPass:=ViewerGetPass;
          Decompressor.OnOverwritePrompt:=OverWritePrompt;
          Decompressor.OnError:=OnError;
          Decompressor.IgnoreCRCCheck:=DlgExtract.cbIgnoreCRCCheck.Checked;
          Decompressor.ExtractAll:=DlgExtract.rgExtractMode.ItemIndex=0; // !Attention! !Correction!
          Decompressor.DestinationDir:=DlgExtract.cbDestDir.Text;
          Decompressor.Extract;
          Decompressor.Free;
        End;
        ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,
        ftTar,ftACE2,ftJar,ftArc,ftArj,
        ftPKG5:Begin
          UnPacker:=TUnPacker.Create;
          UnPacker.ArchiveType:=ArchiveType;
          UnPacker.ArchiveFile:=Archive;
          UnPacker.DeleteAfterExtract:=DlgExtract.rgDeleteArchive.ItemIndex>=1;
          //Set Events!!!
          UnPacker.OnBegin:=DeCompressorOnBegin;
          UnPacker.OnEnd:=CompressorOnEnd;
          UnPacker.OnProgress:=ShowProgress;
          UnPacker.OnGetPassword:=ViewerGetPass;
          UnPacker.OnFileExists:=OverWritePrompt;

          Case DlgExtract.rgUpdateMode.ItemIndex Of  //!Attention! !Correction!
            0:UnPacker.OverWriteMode:=(omRewrite);
            1,2:UnPacker.OverWriteMode:=(omSkip);
          End;

          UnPacker.DestinationDir:=DlgExtract.cbDestDir.Text;
          //
          If DlgExtract.rgExtractMode.ItemIndex=0 Then
            UnPacker.FileSpec.Add('*.*');
          If (FilesList.SelCount>0)And(DlgExtract.rgExtractMode.ItemIndex<>0) Then
          For Index:=0 To FilesList.Items.Count-1 Do
            If FilesList.Items.Item[Index].Selected Then
              Begin
                Temp:=FilesList.Items.Item[Index].SubItems.Strings[7]+'\'+
                      FilesList.Items.Item[Index].Caption;
                Delete(Temp,1,Pos(']',Temp)+1);     //???? UnKnow +1 or not
                UnPacker.FileSpec.Add(Temp);
                //showmessage(temp);
                Temp:='';
              End;
          UnPacker.Extract;
        End;
        ft7Z:
          Begin
            SevenZipDecompressor:=TSevenZipViewer.Create;
            SevenZipDecompressor.FileName:=Archive;
            SevenZipDecompressor.DestDir:=DlgExtract.cbDestDir.Text;
            SevenZipDecompressor.OnBegin:=DeCompressorOnBegin;
            SevenZipDecompressor.OnEnd:=CompressorOnEnd;
            SevenZipDecompressor.OnProgress:=ShowProgress;
            SevenZipDecompressor.OnOverWrite:=OverWritePrompt;
            Case DlgExtract.rgUpdateMode.ItemIndex Of  //!Attention! !Correction!
              0:SevenZipDecompressor.OverWriteMode:=(omOverwriteAll);
              1,2:SevenZipDecompressor.OverWriteMode:=(omSkipAll);
            End;
            If DlgExtract.rgExtractMode.ItemIndex=0 Then
              SevenZipDecompressor.FileSpec.Clear;
            If (FilesList.SelCount>0)And(DlgExtract.rgExtractMode.ItemIndex<>0) Then
              For Index:=0 To FilesList.Items.Count-1 Do
                If FilesList.Items.Item[Index].Selected Then
                  Begin
                    {Temp:=FilesList.Items.Item[Index].SubItems.Strings[7]+'\'+
                          FilesList.Items.Item[Index].Caption;
                    Delete(Temp,1,Pos(']',Temp)+1);     //???? UnKnow +1 or not
                    ShowMessage(Temp);}
                    SevenZipDecompressor.FileSpec.Add(FilesList.Items.Item[Index].SubItems[8]);
                    //showmessage(temp);
                    Temp:='';
                  End;
            SevenZipDecompressor.Extract;
          End;
        ftRar:
          Begin
            RarDecompressor:=TRarDecompressor.Create;
            RarDecompressor.FileName:=Archive;
            RarDecompressor.DestDir:=DlgExtract.cbDestDir.Text;
            RarDecompressor.OnBegin:=DeCompressorOnBegin;
            RarDecompressor.OnEnd:=CompressorOnEnd;
            RarDecompressor.OnProgress:=ShowProgress;
            RarDecompressor.OnPassword:=ViewerGetPass;
            RarDecompressor.OnOverWritePrompt:=OverWritePrompt;
            Case DlgExtract.rgUpdateMode.ItemIndex Of  //!Attention! !Correction!
              0:RarDecompressor.OverWriteMode:=(omRewrite);
              1,2:RarDecompressor.OverWriteMode:=(omSkip);
            End;
            If DlgExtract.rgExtractMode.ItemIndex=0 Then
              RarDecompressor.FileSpec.Clear;
            If (FilesList.SelCount>0)And(DlgExtract.rgExtractMode.ItemIndex<>0) Then
              For Index:=0 To FilesList.Items.Count-1 Do
                If FilesList.Items.Item[Index].Selected Then
                  Begin
                    Temp:=FilesList.Items.Item[Index].SubItems.Strings[7]+'\'+
                          FilesList.Items.Item[Index].Caption;
                    Delete(Temp,1,Pos(']',Temp)+1);     //???? UnKnow +1 or not
                    RarDecompressor.FileSpec.Add(Temp);
                    //showmessage(temp);
                    Temp:='';
                  End;
            RarDecompressor.Extract;
            RarDecompressor.Free;
          End;
      End;
    End;
  DlgExtract.Free;
end;

procedure TFrmMain.BtnGoToAddressClick(Sender: TObject);
var Index:Word;
    Temp:String;
    a:Boolean;
begin
  If ViewMode=dmArchiver Then
    Begin
      If Path='' Then Exit;
      DirTree.Tag:=1;
      //DirTree.Selected:=DirTree.Items.GetFirstNode;
      Temp:=cbAddress.Text;
      Temp:=Copy(Path,1,Pos(']',Path))+Temp;
      if Temp[length(Temp)]<>'\' then Temp:=Temp+'\';
      index:=0;
      while pos('\',Temp)<>0 do
        begin
          Index:=pos('\',Temp);
          GoToNode(copy(Temp,1,Index-1));
          delete(Temp,1,Index);
        end;
      DirTree.Tag:=0;
      DirTreeChanging(Sender,DirTree.Selected,a);
    End Else
      Begin
        FilesList.Clear;
        GoToAddress;
      End;
end;

procedure TFrmMain.BtnShowTreeClick(Sender: TObject);
begin
  WndSplit.Visible:= Not WndSplit.Visible;
  DirTree.Visible:= Not DirTree.Visible;
  {If ViewMode=dmArchiver Then
    BtnGoToAddressClick(Sender);}
end;

procedure TFrmMain.pnNavigationResize(Sender: TObject);
begin
  cbAddress.Width:=pnNavigation.Width-
                   cbAddress.Left-
                   pnlButton.Width-2;
end;                                                    

procedure TFrmMain.btnListTypeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p:TPoint;
begin
  If Button<>mbLeft Then Exit;
  GetCursorPos(P);
  mnuListType.Popup(p.X-X,p.Y-Y+btnListType.Height);
  btnListType.Update;
end;

procedure TFrmMain.Smallicons1Click(Sender: TObject);
begin
  With Sender As TmenuItem Do  
    Case Tag Of
      0:FilesList.ViewStyle:=vsReport;                          
      1:FilesList.ViewStyle:=vsIcon;
      2:FilesList.ViewStyle:=vsList;
      3:FilesList.ViewStyle:=vsSmallIcon;
    End;
  FilesList.Realign;
end;               

procedure TFrmMain.LoadIcons;
Const ImgsEnable:Array[0..6]Of String=('Z_BTNADDENABLE','Z_BTNOPENENABLE',
                                       'Z_BTNEXTRACTENABLE','Z_BTNCOPYENABLE',
                                       'Z_BTNREPLACEENABLE','Z_BTNDELETEENABLE',
                                       'Z_BTNSEARCH');
      ImgsDisable:Array[0..5]Of String=('Z_BTNADDDISABLE','Z_BTNOPENDISABLE',
                                        'Z_BTNEXTRACTDISABLE','Z_BTNCOPYDISABLE',
                                        'Z_BTNREPLACEDISABLE','Z_BTNDELETEDISABLE');

var
  himlIconsBIG: HIMAGELIST;                                       
  hIconLib: HModule;
  ICON: HICON;
  Index:Byte;

procedure AddIconToBIGList(IconName: PChar);
begin
  ICON:=LoadIcon(hIconLib, IconName);
  ImageList_AddIcon(himlIconsBIG, ICON);
end;

begin
  HIconLib:=hInstance;

  himlIconsBig := ImageList_Create(48, 48, ILC_COLOR32 or ILC_MASK, 0, 0);
  For Index:=0 To High(ImgsEnable) Do
    AddIconToBIGList(PChar(ImgsEnable[Index]));
  ToolBarEnableImg.Handle:=himlIconsBIG;
  //
  himlIconsBig := ImageList_Create(48, 48, ILC_COLOR32 or ILC_MASK, 0, 0);
  For Index:=0 To High(ImgsDisable) Do
    AddIconToBIGList(PChar(ImgsDisable[Index]));
  ToolBarDisableImg.Handle:=himlIconsBIG;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  {LogoResource:=TResourceStream.Create(hInstance,'MZFLOGO','PNG');
  LogoResource.Position:=0;
  PNG:=TPNGObject.Create;
  PNG.LoadFromStream(LogoResource);  }
  Caption:=EmptyTitle;
  Application.Title:=Caption;
  //DragAcceptFiles(Self.Handle, true);
  LoadIcons;
  Application.Icon.Handle:=LoadIcon(hInstance,'APPLICATIONICON');
  Self.Icon.Handle:=LoadIcon(hInstance,'APPLICATIONICON');
  If ParamCount>0 Then
    Begin
      OpenArchive(ParamStr(1));
      //ViewMode:=dmArchiver;
      Exit;
    End;
  ViewMode:=dmBrowser;
  If ViewMode<>dmBrowser Then Exit;
  GetDirTree;
end;

procedure TFrmMain.FilesListDblClick(Sender: TObject);
var Temp:String;
begin
  If ViewMode<>dmBrowser Then Exit;
  FileListDblClick;
  {If Not CharInString('D',FilesList.Items.Item[FrmMain.FilesList.ItemIndex].SubItems.Strings[5]) Then
    Begin
      Temp:=cbAddress.Text+'\'+FilesList.Items.Item[FrmMain.FilesList.ItemIndex].Caption;
        Case GetArchiveTypeBySignature(Temp) Of
          ftMZF:;
          ftAnyFile:;
        End;
    End; }
end;

procedure TFrmMain.FilesListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If Key=VK_RETURN Then
    If ViewMode<>dmBrowser Then Exit Else
      FileListDblClick;
  {If Key=VK_DELETE Then
    BtnDeleteClick(Sender); }
end;

procedure TFrmMain.DirTreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  If ViewMode<>dmBrowser Then Exit;
  Expand(Node);
end;

procedure TFrmMain.BtnLevelUpClick(Sender: TObject);
begin
  If ViewMode=dmBrowser Then
    If Length(cbAddress.Text)<=2 Then Exit Else
      If Length(cbAddress.Text)<=0 Then Exit;
  cbAddress.Text:=getlastdir(cbAddress.Text);
  BtnGoToAddressClick(Sender);
end;

procedure TFrmMain.Exit2Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TFrmMain.Closearchive1Click(Sender: TObject);
begin
  CloseArchive;
end;

procedure TFrmMain.cbAddressKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If Key=VK_RETURN Then
    BtnGoToAddressClick(Sender);
end;                                                                

procedure TFrmMain.BtnSearchClick(Sender: TObject);
begin
  If ViewMode=dmBrowser Then //Search in Hard drive
    Begin

    End Else                             
    Begin //Search in archive

    End;
end;

procedure TFrmMain.BtnDeleteClick(Sender: TObject);
var Index:LongWord;
    Temp:String;
    List:TStringList;
    Viewer:TMZFViewer;
begin
    If ViewMode=dmBrowser Then //Delete in Hard drive
    Begin
      If cbAddress.Text='' Then Exit;
      If (MessageBox(Handle,'You really confirm this operation?','Question',MB_YESNO+MB_ICONQUESTION)<>IDYES) Then Exit;

      Temp:=cbAddress.Text;
      If Temp[Length(Temp)]<>'\' Then
        Temp:=Temp+'\';
      Application.CreateForm(TDlgDeleteFiles,DlgDeleteFiles);
      DlgDeleteFiles.SourceDir:=Temp{cbAddress.Text};
      DlgDeleteFiles.DeleteAll:=FilesList.SelCount=0;
      If FilesList.SelCount>0 Then
        Begin
          For Index:=0 To FilesList.Items.Count-1 Do
            If FilesList.Items.Item[Index].Selected Then
              DlgDeleteFiles.FileList.Add(Temp+FilesList.Items.Item[Index].Caption);
        End Else
          SearchDir(Temp,DlgDeleteFiles.FileList);
      DlgDeleteFiles.ShowModal;
      DlgDeleteFiles.Free;
      FilesList.Clear;
      BtnGoToAddressClick(Sender);
    End Else                                
    Begin //Delete in archive
      If FilesList.SelCount<=0 Then
        Begin
          MessageBox(Handle,'No files selected. You need select files for this operation','Attention',MB_OK+MB_ICONEXCLAMATION);
          Exit;
        End;
      If (MessageBox(Handle,'You really confirm this operation?','Question',MB_YESNO+MB_ICONQUESTION)<>IDYES) Then Exit;
      Case ArchiveType Of
        ftMZF:Begin
            List:=TStringList.Create;
            Viewer:=TMZFViewer.Create;
            For Index:=0 To FilesList.Items.Count-1 Do
              If FilesList.Items.Item[Index].Selected Then
                List.Add(cbAddress.Text+'\'+FilesList.Items.Item[Index].Caption);
            Viewer.ArchiveName:=Archive;
            Viewer.DeleteFiles(List); //!Attention! !Correction!
            OpenArchive(Archive);
          End;        
      End;
    End;
end;

procedure TFrmMain.Archivecomment1Click(Sender: TObject);
begin
  ShowComment(Comment);
end;

procedure TFrmMain.Selectall1Click(Sender: TObject);
begin
  FilesList.SelectAll;
end;

procedure TFrmMain.mnuBrowserPopup(Sender: TObject);
begin
  Rename1.Enabled:=True;
  If ViewMode<>dmArchiver Then
    Begin
      Cut1.Enabled:=True;
      Copy1.Enabled:=True;
      Paste1.Enabled:=True;
      Delete1.Enabled:=True;
      If cbAddress.Text='' Then
        Paste1.Enabled:=False;
      If FilesList.SelCount=0 Then
        Begin
          Cut1.Enabled:=False;
          Copy1.Enabled:=False;
        End;
      If FilesList.Items.Count=0 Then
        Begin
          Cut1.Enabled:=False;
          Copy1.Enabled:=False;
          Delete1.Enabled:=False;
          Rename1.Enabled:=False;
        End;
    End;
end;

procedure TFrmMain.Rename1Click(Sender: TObject);
var {FileName:String;}
    Index:LongWord;
    FileName,OldName:String;
begin
  If (ViewMode=dmBrowser) Then
    Begin
      FileName:=cbAddress.Text;
      If FileName[Length(FileName)]<>'\' Then FileName:=FileName+'\';
      Index:=FilesList.ItemIndex;
      OldName:=FileName+FilesList.Items.Item[Index].Caption;
      FileName:=OldName;
      If InputQuery('Rename','Rename file',FileName) Then
        Begin
          If Not RenameFile(OldName,FileName) Then
            MessageBox(Handle,PChar(Format('Could not rename file "%s"',[OldName])),'Error',MB_OK+MB_ICONSTOP);
        End;

      FilesList.Clear;
      BtnGoToAddressClick(Sender);
    End;
end;

procedure TFrmMain.Copy1Click(Sender: TObject);
var Index:LongWord;
    Temp:TStringList;
    fName:String;
    ItemCaption:String;
begin
  If ViewMode<>dmBrowser Then Exit;
  If FilesList.SelCount=0 Then Exit;
  Temp:=TStringList.Create;
  For Index :=0 To FilesList.Items.Count-1 Do
    If FilesList.Items.Item[Index].Selected Then
      Begin
        fName:=cbAddress.Text;
        ItemCaption:=FilesList.items.Item[Index].Caption;
        If fName[Length(fName)]<>'\' Then fName:=fName+'\';
        fName:=fName+ItemCaption;
        Temp.Add(fName);
      End;
  fName:='';
  For Index:=0 To Temp.Count-1 Do
    fName:=Temp.Strings[Index]+#0+fName;
  CopyFilesToClipboard(fName);
end;

procedure TFrmMain.BtnCopyClick(Sender: TObject);
{var List:TStringList;
    Temp:String; }
begin
  {If ViewMode<>dmBrowser Then Exit;
  Temp:=cbAddress.Text;
  If Length(Temp)=0 Then Exit;
  If Temp[Length(Temp)]<>'\' Then Temp:=Temp+'\';
  List:=TStringList.Create;
  If FilesList.SelCount=0 Then
    SearchDir(Temp,List);    }
end;

procedure TFrmMain.BtnReplaceClick(Sender: TObject);
//Var Files:TStringList;
Var Index:LongWord;
    Dir:String;
begin
  If ViewMode<>dmBrowser Then Exit;
  If cbAddress.Text='' Then Exit;
  //Files:=TStringList.Create;
  Application.CreateForm(TDlgCopyMoveFiles,DlgCopyMoveFiles);
  Dir:=cbAddress.Text;
  If Dir[Length(Dir)]<>'\' Then Dir:=Dir+'\';
  If FilesList.SelCount=0 Then SearchDir(Dir,DlgCopyMoveFiles.FilesList)
  Else
  For Index:=0 To FilesList.Items.Count-1 Do
    If FilesList.Items.Item[Index].Selected Then
      DlgCopyMoveFiles.FilesList.Add(Dir+FilesList.Items.Item[Index].Caption);
  If Sender.ClassType = TMenuItem Then
    With Sender As (TMenuItem) Do
      Begin
        Case Tag Of
        1:Begin
            DlgCopyMoveFiles.Mode:=cmCopy;
            DlgCopyMoveFiles.Caption:='Copy files';
            DlgCopyMoveFiles.BtnCopyOrMove.Caption:='Copy';
          End;
        2:Begin
            DlgCopyMoveFiles.Mode:=cmReplace;
            DlgCopyMoveFiles.Caption:='Replace files';
            DlgCopyMoveFiles.BtnCopyOrMove.Caption:='Replace';
          End;
        End;
      End Else
  With Sender As (TToolButton) Do
    Begin
      Case Tag Of
        1:Begin
            DlgCopyMoveFiles.Mode:=cmCopy;
            DlgCopyMoveFiles.Caption:='Copy files';
            DlgCopyMoveFiles.BtnCopyOrMove.Caption:='Copy';
          End;
        2:Begin
            DlgCopyMoveFiles.Mode:=cmReplace;
            DlgCopyMoveFiles.Caption:='Replace files';
            DlgCopyMoveFiles.BtnCopyOrMove.Caption:='Replace';
          End;
      End;
    End;
  //Files.Free;
  DlgCopyMoveFiles.edDestDir.Text:=cbAddress.Text;
  DlgCopyMoveFiles.ShowModal;
  DlgCopyMoveFiles.Free;
end;

procedure TFrmMain.FilesListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var MousePos:TPoint;
    Index:LongWord;
begin
  If (ViewMode=dmBrowser) Then
    Begin
      If (Button=mbRight) Then
        Begin
          GetCursorPos(MousePos);
          mnuBrowser.Popup(MousePos.X,MousePos.Y);
        End Else
        Begin
          If FilesList.SelCount=0 Then Exit;
          if (DragDetectPlus(TWinControl(Sender))) then
            Begin
              DropManager.Files.Clear;
              DropManager.Images:=BrowserIconsBig;
              DropManager.ImageIndex:=FilesList.ItemIndex;
              For Index:=0 To FilesList.Items.Count-1 Do
                If FilesList.Items.Item[Index].Selected Then
                  DropManager.Files.Add(FilesList.Items.Item[Index].SubItems[7]+
                                        FilesList.Items.Item[Index].Caption);
              DropManager.Execute();
              DropManager.Files.Clear;
            End;
        End;
      End Else
        If Button=mbRight Then
          Begin
            GetCursorPos(MousePos);
            Open2.Enabled:=True;
            Rename2.Enabled:=True;
            Delete3.Enabled:=True;
            If FilesList.ItemIndex=-1 Then
              Begin
                Open2.Enabled:=False;
                Rename2.Enabled:=False;
                Delete3.Enabled:=False;
              End;
            If FilesList.SelCount>1 Then
              Rename2.Enabled:=False;
            mnuArchiver.Popup(MousePos.X,MousePos.Y);
          End;
end;

procedure TFrmMain.FilesListMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  {if (FilesList.GetHitTestInfoAt(X, Y) * [htOnItem, htOnIcon, htOnLabel, htOnStateIcon] <> []) then
    FilesList.Cursor := crHandPoint
  else
    FilesList.Cursor := crDefault;}
end;

procedure TFrmMain.DirTreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var MousePos:TPoint;
    Index:LongWord;
begin
  If (ViewMode<>dmBrowser) Then Exit;
  If (Button=mbRight) Then
    Begin
      GetCursorPos(MousePos);
      mnuDirTreeInBrowser.Popup(MousePos.X,MousePos.Y);
    End Else
    Begin
      if (DragDetectPlus(TWinControl(Sender))) then
        Begin
          DropManager.Files.Clear;
          DropManager.Images:=FolderIcon;
          DropManager.Files.Add(GetPathToNode(DirTree.Selected,'\'));
          DropManager.Execute();
          DropManager.Files.Clear;
        End;
    End;
end;

procedure TFrmMain.Copy2Click(Sender: TObject);
var {Index:LongWord;
    Temp:TStringList;}
    fName:String;
    {ItemCaption:String;}
begin
  If ViewMode<>dmBrowser Then Exit;
  fName:=GetPathToNode(DirTree.Selected,'\'){cbAddress.Text}+#0#0;
  {For Index:=0 To Temp.Count-1 Do
    fName:=Temp.Strings[Index]+#0+fName;}
  CopyFilesToClipboard(fName);

end;

procedure TFrmMain.DropFileTargetManagerDrop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var i{,amount,size}:integer;
    Filename: PChar;
    List:TStringList;
begin
  Path:=cbAddress.Text;
  Application.CreateForm(TDlgOperation,DlgOperation);
  List:=TStringList.Create;
  for i := 0 to DropFileTargetManager.Files.Count-1 do
    begin
      List.add(DropFileTargetManager.Files.Strings[i]);
    end;
  If (List.Count>1)Or(Not FileExists(List.Strings[0])) Then
    DlgOperation.BtnOpen.Enabled:=False;
  If (ViewMode=dmArchiver)Or(Path='') Then
    DlgOperation.BtnCopy.Enabled:=False;
  Case DlgOperation.ShowModal Of
    mrYes:OpenArchive(List.Strings[0]);//Open file
    mrAll:AddIntoArchive('',List);//Add into archive
    mrYesToAll:CopyIntoDirectory(List,Path);//Copy here  
  End;
  DlgOperation.Free;
  List.Free;
end;

procedure TFrmMain.Rename2Click(Sender: TObject);
var OldName,FileName:String;
    Viewer:TMZFViewer;
begin
  OldName:=cbAddress.Text+'\'+FilesList.Items.Item[FilesList.ItemIndex].Caption;
  FileName:=OldName;
  If InputQuery('Rename','Rename file',FileName) Then
    Begin
      If FileName[1]<>'\' Then FileName:='\'+FileName;
      Viewer:=TMZFViewer.Create;
      Viewer.ArchiveName:=Archive;
      Viewer.RenameFile(OldName,FileName);
      Viewer.Free;
      OpenArchive(Archive);
    End;
  FilesList.Clear;
  BtnGoToAddressClick(Sender);
end;

end.


