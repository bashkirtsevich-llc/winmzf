unit Favorite_u;

interface

uses                                         
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, IniFiles, StdCtrls, Buttons, ComCtrls, Advanced;

type
  TdlgFavorite = class(TForm)
    imgListExt: TImageList;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    lvFavorites: TListView;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnClear: TSpeedButton;
    DlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgFavorite: TdlgFavorite;

implementation

{$R *.dfm}

procedure DeleteFormFav(Index:LongWord);
var Fav:TIniFile;
    Count:Integer;
Begin
  Fav:=TIniFile.Create(ExtractFileDir(Application.Exename)+'\Fav.ini');
  Count:=Fav.ReadInteger('Main','Count',0);
  Dec(Count);
  Fav.WriteInteger('Main','Count',Count);
  Fav.DeleteKey('Favorite'+IntToStr(Index),'Caption');
  Fav.DeleteKey('Favorite'+IntToStr(Index),'ImageIndex');
  Fav.DeleteKey('Favorite'+IntToStr(Index),'Size');
  Fav.DeleteKey('Favorite'+IntToStr(Index),'Pach');
  Fav.Free;
End;

procedure AddToFavorite(FileName:String);
var Fav:TIniFile;
    Count:Integer;
    FileStream:TFileStream;
    Size:LongWord;
    Pach:String;
Begin
  FileSetAttr(ExtractFileDir(Application.Exename)+'\Fav.ini',faArchive);
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
  Pach:=ExtractFileDir(FileName);
  If Pach[Length(Pach)]<>'\' Then Pach:=Pach+'\';
  Fav.WriteString('Favorite'+IntToStr(Count-1),'Pach',Pach);
  Fav.Free;
  Size:=0;
End;

procedure AddToFavoriteValuesOnly(FName,FSize,Pach:String;ImgIndex:Byte);
var Fav:TIniFile;
    Count:Integer;
Begin
  Fav:=TIniFile.Create(ExtractFileDir(Application.Exename)+'\Fav.ini');
  Count:=Fav.ReadInteger('Main','Count',0);
  Inc(Count);
  Fav.WriteInteger('Main','Count',Count);
  Fav.WriteString('Favorite'+IntToStr(Count-1),'Caption',FName);
  Fav.WriteInteger('Favorite'+IntToStr(Count-1),'ImageIndex',ImgIndex);
  Fav.WriteString('Favorite'+IntToStr(Count-1),'Size',FSize);
  Fav.WriteString('Favorite'+IntToStr(Count-1),'Pach',Pach);
  Fav.Free;
End;

procedure ReadFavorites;
Var Fav:TIniFile;
    Count:Integer;
    Index:Integer;
    Item:TListItem;
Begin
  Fav:=TIniFile.Create(ExtractFileDir(Application.Exename)+'\Fav.ini');
  Count:=Fav.ReadInteger('Main','Count',0);
  If Count>0 Then
    For Index:=0 To Count-1 Do
      Begin
        Item:=dlgFavorite.lvFavorites.Items.Add;
        Item.Caption:=Fav.ReadString('Favorite'+IntToStr(Index),'Caption','');
        Item.ImageIndex:=Fav.ReadInteger('Favorite'+IntToStr(Index),'ImageIndex',0);
        Item.SubItems.Add(IntToStr(Fav.ReadInteger('Favorite'+IntToStr(Index),'Size',0)));
        Item.SubItems.Add(Fav.ReadString('Favorite'+IntToStr(Index),'Pach',''));
      End;
  Fav.Free;
End;

procedure TdlgFavorite.FormCreate(Sender: TObject);
begin
  ReadFavorites;
  imgListExt.BkColor:=lvFavorites.Color;
  imgListExt.BlendColor:=lvFavorites.Color;
  Caption:=ReadFromLanguage('Windows','wndFavorites',Caption);
  btnAdd.Caption:=ReadFromLanguage('Buttons','btnAdd',btnAdd.Caption);
  btnDelete.Caption:=ReadFromLanguage('Buttons','btnDelete',btnDelete.Caption);
  btnClear.Caption:=ReadFromLanguage('Buttons','btnClear',btnClear.Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  lvFavorites.Columns[0].Caption:=ReadFromLanguage('ListItems','liFile',lvFavorites.Columns[0].Caption);
  lvFavorites.Columns[1].Caption:=ReadFromLanguage('ListItems','liSize',lvFavorites.Columns[1].Caption);
  lvFavorites.Columns[2].Caption:=ReadFromLanguage('ListItems','liPath',lvFavorites.Columns[2].Caption);
  DlgOpen.Title:=ReadFromLanguage('Dialogs','dlgOpen',DlgOpen.Title);
end;

procedure TdlgFavorite.btnClearClick(Sender: TObject);
begin
  lvFavorites.Clear;
  FileSetAttr(ExtractFileDir(Application.Exename)+'\Fav.ini',faArchive);
  DeleteFile(ExtractFileDir(Application.Exename)+'\Fav.ini');
end;

procedure TdlgFavorite.btnAddClick(Sender: TObject);
begin
  If DlgOpen.Execute(Handle) Then
    Begin
      AddToFavorite(DlgOpen.FileName);
      lvFavorites.Clear;
      ReadFavorites;
    End;
end;

procedure TdlgFavorite.btnDeleteClick(Sender: TObject);
Var Index:LongWord;
begin
  If lvFavorites.SelCount<>0 Then
    If MessageBox(Handle,Pchar(ReadFromLanguage('Messages','Shure','Are you shure?')),'WinMZF',MB_YESNO+MB_ICONQUESTION)=IDYES Then
      Begin
        FileSetAttr(ExtractFileDir(Application.Exename)+'\Fav.ini',faArchive);
        DeleteFile(ExtractFileDir(Application.Exename)+'\Fav.ini');
        lvFavorites.DeleteSelected;
        For Index:=0 To lvFavorites.Items.Count-1 Do
          AddToFavoriteValuesOnly(lvFavorites.Items[Index].Caption,
                                  lvFavorites.Items[Index].SubItems[0],
                                  lvFavorites.Items[Index].SubItems[1],
                                  lvFavorites.Items[Index].ImageIndex);
      End;
end;

end.
