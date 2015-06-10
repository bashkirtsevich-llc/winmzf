unit Main_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, Constructor_u, StdCtrls, ExtCtrls, Buttons, Mask,
  View_u, Info_u;

type
  TwndMain = class(TForm)
    mnuMain: TMainMenu;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    tvNavigate: TTreeView;
    gbMain: TGroupBox;
    lbType: TLabel;
    lbName: TLabel;
    edFileName: TEdit;
    btnBrows: TButton;
    lbImageSize: TLabel;
    gbConfig: TGroupBox;
    btnBuild: TBitBtn;
    btnOpen: TBitBtn;
    btnSave: TBitBtn;
    btnExit: TBitBtn;
    lbAuthor: TLabel;
    edAuthor: TEdit;
    lbVersion: TLabel;
    meVersion: TMaskEdit;
    lbComment: TLabel;
    memComment: TMemo;
    Skin1: TMenuItem;
    Build1: TMenuItem;
    Extract1: TMenuItem;
    Help1: TMenuItem;
    gbPreview: TGroupBox;
    imgPreview: TImage;
    lbButtonsSize: TLabel;
    lbHeight: TLabel;
    lbWidth: TLabel;
    edHeight: TEdit;
    edWidth: TEdit;
    track1: TUpDown;
    track2: TUpDown;
    About1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    procedure tvNavigateChange(Sender: TObject; Node: TTreeNode);
    procedure btnBrowsClick(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure edHeightKeyPress(Sender: TObject; var Key: Char);
    procedure btnOpenClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  wndMain: TwndMain;
  _Index:Byte;
  _IsIco:Boolean;
  IconsArray   :Array[0..9] Of String;
  {BitmapsArray :Array[0..7] Of String;}

const
  IconsNames:Array[0..7] Of ShortString=
    ('btn_add','bnt_open','btn_extract',
     'btn_delete','btn_copy','btn_move',
     'btn_info','btn_close');
  {BitmapsNames:Array[0..7] Of ShortString=
    ('btn_levelup','btn_link','img_folder',
     'img_file','img_folder_big','img_file_big',
     'img_foldertree_close','img_foldertree_expand'); }

implementation

uses About_u;

{$R *.dfm}

procedure GetImgParams(FileName:String);
begin
  wndMain.imgPreview.Picture:=Nil;
  if Not FileExists(FileName) then Exit;  
  wndMain.imgPreview.Picture.LoadFromFile(FileName);
  wndMain.lbImageSize.Caption:=Format('Height=%d Width=%d',
        [wndMain.ImgPreview.Picture.Height,wndMain.ImgPreview.Picture.Width]);
end;

function GetPathToNode(Node: TTreeNode; Separator: Char): string;
begin
  Result := '';
  if Node = nil then
    exit;
  while Node <> nil do
  begin
    Result := Node.Text + Separator + Result;
    Node := Node.Parent;
  end;
  Delete(Result, length(Result), 1);
end;

function GetParentDir(Dir:String):String;
var Index:Byte;
begin
  for Index := 1 to Length(Dir) do
    if Dir[Index]<>'\' then Continue else
      begin
        Result:=Copy(Dir,1,Index-1);
        Exit;
      end;       
end;

procedure TwndMain.About1Click(Sender: TObject);
begin
  Application.CreateForm(TAboutBox, AboutBox);
  AboutBox.ShowModal;
  AboutBox.Free;
end;

procedure TwndMain.btnBrowsClick(Sender: TObject);
begin
  if Not dlgOpen.Execute(Handle) then Exit;
  edFileName.Text:=dlgOpen.FileName;
  GetImgParams(dlgOpen.FileName);
  {if _isIco then }
    IconsArray[_Index]:=edFileName.Text
  {else
    BitmapsArray[_Index]:=EdFileName.Text; }
end;

procedure TwndMain.btnBuildClick(Sender: TObject);         
var Index:Byte;
    BuilderInfo:TBuilderInfo;
    SkinInfo:TSkinInfo;
begin
  dlgSave.Filter:='WinMZF Skins (*.msk)|*.MSK';
  if not dlgSave.Execute(Handle) then Exit;
  if UpperCase(ExtractFileExt(dlgSave.FileName))<>'.MSK' then
    dlgSave.FileName:=dlgSave.FileName+'.msk';
  SkinInfo.Author:=edAuthor.Text;
  SkinInfo.SkinVersion:=meVersion.Text;        
  SkinInfo.Create:=Date+Time;
  SkinInfo.Comment:=memComment.Text;
  SkinInfo.Height:=StrToInt(edHeight.Text);
  SkinInfo.Width:=StrToInt(edWidth.Text);
  For Index:=0 To 8 Do
    begin
      {If Index<8 Then
        begin  }
          BuilderInfo.files[Index].FileName:=IconsArray[Index];
          BuilderInfo.files[Index].Name:=IconsNames[Index];
        {end
      Else
        begin
          BuilderInfo.files[Index].FileName:=BitmapsArray[Index-8];
          BuilderInfo.files[Index].Name:=BitmapsNames[Index-8];                                     
        end; }
    end;
  BuildSkin(dlgSave.FileName,BuilderInfo,SkinInfo);
end;

procedure TwndMain.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TwndMain.btnOpenClick(Sender: TObject);
var List:TStringList;
    Index:Byte;
    Author,Version,Create,Comment:String;
    Height,Width:Byte;
    SaveStream:TMemoryStream;
begin
  dlgOpen.Filter:='WinMZF Skinf (*.msk)|*.MSK';
  If Not dlgOpen.Execute Then Exit;
  List:=TStringList.Create;
  ViewSkin(dlgOpen.FileName,Author,Version,Create,Comment,Height,Width,List);

  Application.CreateForm(TdlgInfo,dlgInfo);
  dlgInfo.leFileName.Text:=ExtractFileName(dlgOpen.FileName);
  dlgInfo.leAuthor.Text:=Author;
  dlgInfo.leVersion.Text:=Version;
  dlgInfo.leDate.Text:=Create;
  dlgInfo.memComment.Text:=Comment;
  dlgInfo.ShowModal;
  dlgInfo.Free;

  Application.CreateForm(TdlgViewer,dlgViewer);
  For Index:=0 To List.COunt-1 Do
    dlgViewer.cblFiles.Items.Add(List[Index]);

  if dlgViewer.ShowModal=mrOk then
    begin
      CreateDir(ExtractFileDir(dlgOpen.FileName)+'\'+ExtractFileName(dlgOpen.FileName)+'_Data');
      For Index:=0 To List.Count-1 Do
        If dlgViewer.cblFiles.Checked[Index] Then
          Begin
            SaveStream:=TMemoryStream.Create;
            GetDataByIndex(dlgOpen.FileName,Index,SaveStream);
            SaveStream.SaveToFile(ExtractFileDir(dlgOpen.FileName)+'\'+ExtractFileName(dlgOpen.FileName)+'_Data\'+IntToStr(Index)+'.ico');
            SaveStream.Free;
          End;
    end;
  dlgViewer.Free;
  List.Free;
end;

procedure TwndMain.edHeightKeyPress(Sender: TObject; var Key: Char);
begin
  If Not (Key in ['0'..'9',#8]) Then Key:=#0;
end;

procedure TwndMain.tvNavigateChange(Sender: TObject; Node: TTreeNode);
var Temp:String;
    Index:Byte;
begin
  Temp:=UpperCase(GetParentDir(GetPathToNode(Node,'\')));
  Index:=node.Index;
  _Index:=Index;
  lbType.Caption:=Temp;
  edFileName.Text:='';
  If Temp='ICONS' Then
    Begin
      edFileName.Enabled:=True;
      btnBrows.Enabled:=True;
      dlgOpen.Filter:='Icon files (*.ico)|*.ICO';
      edFileName.Text:=IconsArray[Index];
      GetImgParams(IconsArray[Index]);
      _isIco:=True;
    End
  Else
    {if Temp='BITMAPS' then
      Begin
        edFileName.Enabled:=True;
        btnBrows.Enabled:=True;
        dlgOpen.Filter:='Bitmap files (*.bmp)|*.BMP';
        edFileName.Text:=BitmapsArray[Index];
        GetImgParams(BitmapsArray[Index]);
        _isIco:=False;
      End Else }
      Begin
        edFileName.Enabled:=False;
        btnBrows.Enabled:=False;
      End;
end;

end.
