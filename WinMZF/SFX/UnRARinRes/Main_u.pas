unit Main_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnRAR, StdCtrls, ComCtrls;

function ChangeVolProc(ArcName:PChar; Mode:integer):integer; cdecl;

type
  TdlgExtract = class(TForm)
    btnExtract: TButton;
    edArchive: TEdit;
    btnBrows: TButton;
    edDestDir: TEdit;
    pbDone: TProgressBar;
    dlgOpen: TOpenDialog;
    procedure btnExtractClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgExtract: TdlgExtract;

implementation

{$R *.dfm}

function SolveForY(X, Z: LongWord): Byte;
begin
  if Z = 0 then Result := 0
  else Result := Byte(Trunc( (X * 100.0) / Z ));
end;

function GetPercentDone(FMinValue,FCurValue,FMaxValue:LongWord): Byte;
begin
  Result := SolveForY(FCurValue - FMinValue, FMaxValue - FMinValue);
end;

function ChangeVolProc(ArcName:PChar; Mode:integer):integer;
var s:string;
    p:pchar;
begin
  ChangeVolProc:=1;
  p:=@s[1];
  if (Mode=RAR_VOL_ASK) then
  begin
    s:='Insert next volume';
    if Application.messagebox(p,'Attention',mb_okcancel)=idcancel
       then ChangeVolProc:=0
  end;
end;

procedure RarExtract(ArcName:pchar;DestDir:String);
var
  hArcData:THandle;
  RHCode,PFCode:integer;
  CmtBuf:array[1..16384] of char;
  HeaderData:RARHeaderData;
  OpenArchiveData:RAROpenArchiveData;
  {GlobalUnPackSize,Maximum,}FileCount,Current:LongWord;
begin

  OpenArchiveData.ArcName:=ArcName;
  OpenArchiveData.CmtBuf:=@CmtBuf[1];
  OpenArchiveData.CmtBufSize:=sizeof(CmtBuf);
  OpenArchiveData.OpenMode:=RAR_OM_EXTRACT;
  hArcData:=RAROpenArchive(OpenArchiveData);
  if (OpenArchiveData.OpenResult<>0) then
     begin
      MessageBox(dlgExtract.Handle,'Archive is corrupt!','Error',MB_OK+MB_ICONEXCLAMATION);
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
      RARProcessFile(hArcData,RAR_OM_LIST,Nil,Nil);
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
    {If dlgProgress.Abort Then
      Begin
        dlgProgress.Close;
        dlgProgress.Free;
        Self.Enabled:=True;
        RARCloseArchive(hArcData);
        Exit;
      End;}
    {If (Not GetBoolOptions('Config','EnableSecurity',False))Then
        If (Not NameInMask(GetStringOptions('Config','ExcludeExt','*.exe;*.com;*.bat'),HeaderData.FileName)) Then}
          PFCode:=RARProcessFile(hArcData,RAR_EXTRACT,PChar(DestDir),Nil);
    {Inc(GlobalUnPackSize, HeaderData.PackSize);}
    Inc(Current);
    if PFCode<>0 then
       begin
        MessageBox(dlgExtract.Handle,'Coud not open file','Error',MB_OK+MB_ICONSTOP);
        RARCloseArchive(hArcData);
        exit;
       end;
    RHCode:=RARReadHeader(hArcData,HeaderData);
    {dlgProgress.pbFile.Position:=GetPercentDone(0,GlobalUnPackSize,Maximum);}
    {dlgProgress.pbArchive.Position:=GetPercentDone(0,Current,FileCount);
    dlgProgress.lbFileIndex.Caption:=Format('File index %d',[Current]);
    dlgProgress.lbCurrent.Caption:='';
    dlgProgress.lbTotal.Caption:=Format('Total progress %d',[dlgProgress.pbArchive.Position]);
    dlgProgress.Update;}
    dlgExtract.pbDone.Position:=GetPercentDone(0,Current,FileCount);
    Application.ProcessMessages;
  end;
  if (RHCode=ERAR_BAD_DATA) then
    MessageBox(dlgExtract.Handle,'Archive is corrupt','Attention',MB_OK+MB_ICONEXCLAMATION);
  RARCloseArchive(hArcData);
  {dlgProgress.Close;
  dlgProgress.Free;
  Self.Enabled:=True;}
End;

procedure TdlgExtract.btnExtractClick(Sender: TObject);
begin
  LoadDll(hInstance);
  RarExtract(PChar(edArchive.Text),edDestDir.Text);
  FreeDll;
end;

procedure TdlgExtract.FormCreate(Sender: TObject);
begin
  edDestDir.Text:=ExtractFileDir(Application.ExeName);
  edArchive.Text:='';
end;

procedure TdlgExtract.btnBrowsClick(Sender: TObject);
begin
  If dlgOpen.Execute Then
    edArchive.Text:=dlgOpen.FileName;
end;

end.
 