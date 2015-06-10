unit FindResult_u;

interface                                                        

uses
  SysUtils, Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ComCtrls, ImgList, CommCtrl, ToolBarIcons_u, ExtCtrls,
  Advanced, Viewer_u, RarView_u, LZMA_u, ArchView_u, SignatureDetect_u,
  ShellContextMenu;

type
  TdlgSearchResults = class(TForm)
    lvFiles: TListView;
    BigIcons: TImageList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    pnlMain: TPanel;
    tbMain: TToolBar;
    btnOpen: TToolButton;
    btnInfo: TToolButton;
    Bevel5: TBevel;
    Bevel6: TBevel;
    BrowserIcons: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FileInfo(Sender:TObject;Var FileHead:TFileHead; Id:LongWord);
    procedure btnOpenClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure lvFilesDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    _Dir,_Ext:String;
    _Search,_Archive:Boolean;
    _fType:TFileType;
    _FileSize:LongWord;
    _fList:TStringList;
    procedure SearchDir(Dir,Ext: string);
    procedure SearchInArchive(Archive,Mask:String;ArcType:TFileType);
    { Public declarations }
  end;

var
  dlgSearchResults: TdlgSearchResults;

implementation

{$R *.dfm}

procedure TdlgSearchResults.SearchDir(Dir,Ext: string);
var
  SR: TSearchRec;
  FindRes: Integer;
  Item:TListItem;
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
             Item         := lvFiles.Items.Add;
             Item.Caption := SR.Name;
             Item.SubItems.Add(IntToStr(SR.Size));
             Item.SubItems.Add(Dir);
            end;
            //showmessage(dir+sr.Name);
            FindRes := FindNext(SR);
          end;
  FindClose(FindRes);
end;

procedure TdlgSearchResults.FormCreate(Sender: TObject);
Var Temp:TStream;
    Icon:TIcon;
    himlIconsBIG: HIMAGELIST;
    hIconLib: HModule;
    Index:Byte;
Begin
  _fList:=TStringList.Create;
  HIconLib:=hInstance;
  himlIconsBig := ImageList_Create(48, 48, ILC_COLOR32 or ILC_MASK, 0, 0);
  For Index:=0 To 1 do
    Begin
      Temp:=TMemoryStream.Create;
      Case Index Of
        0:Temp.Write(b_opn_ondata,SizeOf(b_opn_ondata));
        1:Temp.Write(b_info_ondata,SizeOf(b_info_ondata));
      End;
      Temp.Position:=0;
      Icon:=TIcon.Create;
      Icon.LoadFromStream(Temp);
      ImageList_AddIcon(himlIconsBIG,Icon.Handle);
      Icon.Free;
      Temp.Free;
    End;
  BigIcons.Handle:=himlIconsBIG;
  //Load language
  Caption:=ReadFromLanguage('Windows','wndSearchResults',Caption);
  btnOpen.Caption:=ReadFromLanguage('Buttons','btnOpen',btnOpen.Caption);
  btnInfo.Caption:=ReadFromLanguage('Buttons','btnInfo',btnInfo.Caption);
  lvFiles.Columns[0].Caption:=ReadFromLanguage('ListItems','liFile',lvFiles.Columns[0].Caption);
  lvFiles.Columns[1].Caption:=ReadFromLanguage('ListItems','liSize',lvFiles.Columns[1].Caption);
  lvFiles.Columns[2].Caption:=ReadFromLanguage('ListItems','liPath',lvFiles.Columns[2].Caption);
end;

procedure TdlgSearchResults.FileInfo(Sender:TObject;Var FileHead:TFileHead; Id:LongWord);
Var Item:TListItem;
Begin
/////
  If FileMaskEquate(FileHead.LongFileName,_Ext) Then
    Begin
      Item:=lvFiles.Items.Add;
      Item.Caption:=ExtractFileName(FIleHead.LongFileName);
      Item.SubItems.Add(IntToStr(FileHead.Size));
      Item.SubItems.Add(ExtractFileDir(FileHead.LongFileName));
    End;
End;

procedure TdlgSearchResults.SearchInArchive(Archive,Mask:String;ArcType:TFileType);
var MZFViewer:TMZFViewer;
    RarViewer:TRarViewer;
    SevenZipViewer:TSevenZipViewer;
    Files,Dirs:TStringList;
    Index:LongWord;
    Item:TListItem;
    _fInfo:TFileInfo;
    _a,_c:String;
begin
  Case ArcType Of
    ftMZF:
      Begin
        MZFViewer:=TMZFViewer.Create;
        MZFViewer.ArchiveName:=Archive;
        MZFViewer.OnShowFileINfo:=FileInfo;
        MZFViewer.ShowAll:=True;
        MZFViewer.ShowFiles;
        MZFViewer.Free;
      End;
    ftZip,ftZoo,ftBH,ftGZip,ftLha,ftCab,ftArj,ftPKG5,ftACE2:
      Begin
        Files:=TStringList.Create;
        Dirs:=TStringList.Create;
        GetFilesAndDirs(Dirs,Files,Archive,_a,_c);
        For Index:=0 To Files.Count-1 Do
          If FileMaskEquate(Files[Index],Mask) Then
            Begin
              //GetFileInfo(Archive,Files[Index],_fInfo);
              Item:=lvFiles.Items.Add;
              Item.Caption:=ExtractFileName(Files[Index]);
              Item.SubItems.Add({IntToStr(_fInfo.Size)}'');
              Item.SubItems.Add(ExtractFileDir(Files[Index]));
            End;
        Files.Free;
        Dirs.Free;
      End;
    ftRar:
      Begin
        RarViewer:=TRarViewer.Create;
        RarViewer.FileName:=Archive;
        RarViewer.OnShowFileInfo:=FileInfo;
        RarViewer.GetFilesFromDir('%ALL%');
        RarViewer.Free;
      End;
    ft7Z:
      Begin
        SevenZipViewer:=TSevenZipViewer.Create;
        SevenZipViewer.OnShowFileInfo:=FileInfo;
        SevenZipViewer.FileName:=Archive;
        SevenZipViewer.GetFilesFromDir('%ALL%');
        SevenZipViewer.Free;
      End;
  End;
end;

procedure TdlgSearchResults.FormShow(Sender: TObject);
begin
  If (_Search)And(Not _Archive) Then
    SearchDir(_Dir,_Ext)
  Else
    SearchInArchive(_Dir,_Ext,_fType);
end;

procedure TdlgSearchResults.btnOpenClick(Sender: TObject);
begin
  If _Archive Then Exit;
  If lvFiles.ItemIndex<0 Then Exit;
  ShellOpenFile(Handle,lvFiles.Items[lvFiles.ItemIndex].SubItems[1]+lvFiles.Items[lvFiles.ItemIndex].Caption);
end;

procedure TdlgSearchResults.btnInfoClick(Sender: TObject);
begin
  If _Archive Then Exit;
  ExecuteContextMenu(lvFiles.Items[lvFiles.ItemIndex].SubItems[1]+
                     lvFiles.Items[lvFiles.ItemIndex].Caption,'properties',Handle);
end;

procedure TdlgSearchResults.lvFilesDblClick(Sender: TObject);
begin
  If lvFiles.ItemIndex<>-1 Then
    btnOpenClick(Sender);
end;

end.
