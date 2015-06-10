unit RarView_u;

interface

Uses SysUtils, DFUnRar, Classes, Advanced;

Type TRarFileInfo=Packed Record
    FileName,Pach:String;
    Size:LongWord;
    PackedSize:LongWord;
    CRC:LongWord;
    CreationTime,
    ModifyTime,
    AccessTime:TDateTime;
    Attr:String;
  End;

Type TShowFileInfo=procedure (Sender:TObject;FileInfo:TRarFileInfo)Of Object;

Type TRarViewer=Class(TComponent)
  Private
    dfUnRar:TDFUnRar;
    fArchiveFileName:String;
    FilterDir:String;
    fComment:String;
    DirList:TStringList;
    fOnShowInfo:TShowFileInfo;
    procedure FillDirectories(Sender: TObject;
                        hdrData: TDFRARHeaderData; status: Integer);
    procedure ShowFiles(Sender: TObject;
                        hdrData: TDFRARHeaderData; status: Integer);
  Public
    constructor Create;
    property FileName:String read fArchiveFileName write fArchiveFileName;
    property Dirs:TStringList read DirList write DirList;
    property OnShowFileInfo:TShowFileInfo read fOnShowInfo write fOnShowInfo;
    property Comment:String read fComment write fComment;
    procedure GetDirs;
    procedure GetFilesFromDir(Dir:String);
    procedure Free;
    destructor Destroy;
  End;

implementation

Constructor TRarViewer.Create;
Begin
  dfUnRar:=TDFUnRar.Create(Self);
  DirList:=TStringList.Create;
End;

Procedure TRarViewer.GetDirs;
Begin
  dfUnRar.FileName:=fArchiveFileName;
  fComment:=dfUnRar.ArchivComment;  
  dfUnRar.OnFileProcessing:=FillDirectories;
  dfUnRar.Mode:=DFRAR_LIST;
  dfUnRar.StopProcessing:=False;
  dfUnRar.OverrideEvent:=OR_EVENT;
  dfUnRar.Extract;
End;

Procedure TRarViewer.GetFilesFromDir(Dir:String);
Begin
  dfUnRar.FileName:=fArchiveFileName;
  FilterDir:=Dir;
  dfUnRar.OnFileProcessing:=ShowFiles;
  dfUnRar.Mode:=DFRAR_LIST;
  dfUnRar.StopProcessing:=False;
  dfUnRar.OverrideEvent:=OR_EVENT;
  dfUnRar.Extract;
End;

procedure TRarViewer.ShowFiles(Sender: TObject;
  hdrData: TDFRARHeaderData; status: Integer);
Var FileInfo:TRarFileInfo;
    fAttr:String;
begin
  If UpperCase(ExtractFileDir(hdrData.FileName))<>UpperCase(FilterDir) Then Exit;
  {showmessage(filterdir);
  showmessage(ExtractFileDir(hdrData.FileName));}
  If hdrData.FADirectory Then Exit;
  fAttr:='';
  if hdrData.FAArchive    then FAttr := FAttr + 'A';
  if hdrData.FACompressed then FAttr := FAttr + 'C';
  if hdrData.FADirectory  then FAttr := FAttr + 'D';
  if hdrData.FAHidden     then FAttr := FAttr + 'H';
  if hdrData.FANormal     then FAttr := FAttr + 'N';
  if hdrData.FAOffLine    then FAttr := FAttr + 'O';
  if hdrData.FAReadOnly   then FAttr := FAttr + 'R';
  if hdrData.FASystem     then FAttr := FAttr + 'S';
  if hdrData.FATempporary then FAttr := FAttr + 'T';
  //
  FileInfo.FileName:=ExtractFileName(hdrData.FileName);
  FileInfo.Pach:=ExtractFilePath(hdrData.FileName);
  FileInfo.Size:=hdrData.UnpSize;
  FileInfo.PackedSize:=hdrData.PackSize;
  FileInfo.CRC:=StrToInt('$'+hdrData.FileCRC);
  FileInfo.CreationTime:=hdrData.FileTime;
  FileInfo.ModifyTime:=hdrData.FileTime;
  FileInfo.AccessTime:=hdrData.FileTime;
  FileInfo.Attr:=fAttr;
  If Assigned(fOnShowInfo) Then
    fOnShowInfo(Self,FileInfo);
end;

procedure TRarViewer.FillDirectories(Sender: TObject;
  hdrData: TDFRARHeaderData; status: Integer);
var Root:String;
begin
  Root:=Format(RootCaption,[ExtractFileName(fArchiveFileName)]);
  If DirList.Count=0 Then
    DirList.Add(Root);
  If Not TextInList(Root+'\'+ExtractFileDir(hdrData.FileName),DirList) Then
    DirList.Add(Root+'\'+ExtractFileDir(hdrData.FileName))
end;

Procedure TRarViewer.Free;
Begin
  If Self<> Nil Then
    Destroy;
End;

Destructor TRarViewer.Destroy;
Begin
  dfUnRar.Free;
  DirList.Free;
End;

end.
