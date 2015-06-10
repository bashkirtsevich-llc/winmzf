unit FileInfo_u;

interface                                        

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls, Advanced;

type
  TdlgFileInfo = class(TForm)
    pcMain: TPageControl;
    tsGeneral: TTabSheet;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    lbName: TLabel;
    lbFolder: TLabel;
    lbSize: TLabel;
    lbPackedSize: TLabel;
    lbCreate: TLabel;
    lbModify: TLabel;
    lbOpen: TLabel;
    lbAttr: TLabel;
    lbCRC: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    tsArchive: TTabSheet;
    lbFileName: TLabel;
    lbFileFolder: TLabel;
    lbFileSize: TLabel;
    lbFilePackedSize: TLabel;
    lbFileCreationTIme: TLabel;
    lbFileModifyTime: TLabel;
    lbFileLastAccessTime: TLabel;
    lbFileAttr: TLabel;
    lbFileCRC: TLabel;
    pbRatio: TProgressBar;
    lbRatio: TLabel;
    lbVersion: TLabel;
    lbFileCount: TLabel;
    lbArchUnpacked: TLabel;
    lbComment: TLabel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    lbArchiveVersion: TLabel;
    lbArchiveFileCount: TLabel;
    lbArchiveSize: TLabel;
    lbArchiveComment: TLabel;
    lbUnPackSize: TLabel;
    lbArchiveUnPacked: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgFileInfo: TdlgFileInfo;

implementation

{$R *.dfm}

procedure TdlgFileInfo.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Windows','wndFileInfo',Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  tsGeneral.Caption:=ReadFromLanguage('Tabs','tbGeneral',tsGeneral.Caption);
  tsArchive.Caption:=ReadFromLanguage('Tabs','tbArchive',tsArchive.Caption);
  lbName.Caption:=ReadFromLanguage('Labels','lbFileName',lbName.Caption);
  lbFolder.Caption:=ReadFromLanguage('Labels','lbPath',lbFolder.Caption);
  lbSize.Caption:=ReadFromLanguage('Labels','lbSize',lbSize.Caption);
  lbPackedSize.Caption:=ReadFromLanguage('Labels','lbPackedSize',lbPackedSize.Caption);
  lbCreate.Caption:=ReadFromLanguage('Labels','lbCreate',lbCreate.Caption);
  lbModify.Caption:=ReadFromLanguage('Labels','lbModify',lbModify.Caption);
  lbOpen.Caption:=ReadFromLanguage('Labels','lbOpen',lbOpen.Caption);
  lbAttr.Caption:=ReadFromLanguage('Labels','lbAttr',lbAttr.Caption);
  lbCRC.Caption:=ReadFromLanguage('Labels','lbCRC',lbCRC.Caption);
  lbVersion.Caption:=ReadFromLanguage('Labels','lbVersion',lbVersion.Caption);
  lbFileCount.Caption:=ReadFromLanguage('Labels','lbFileCount',lbFileCount.Caption);
  lbArchUnpacked.Caption:=ReadFromLanguage('Labels','lbArchUnpacked',lbArchUnpacked.Caption);
  lbUnPackSize.Caption:=ReadFromLanguage('Labels','lbUnPackSize',lbUnPackSize.Caption);
  lbComment.Caption:=ReadFromLanguage('Labels','lbComment',lbComment.Caption);
end;

end.
