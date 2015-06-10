unit Config_u;
               
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Registry, Advanced;

type
  TdlgConfig = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    pcMain: TPageControl;
    tgGeneral: TTabSheet;
    gbInterface: TGroupBox;
    cbToolBarCaptions: TCheckBox;
    cbRowSelect: TCheckBox;
    rgViewStyle: TRadioGroup;
    tbFolders: TTabSheet;
    gbTempDir: TGroupBox;
    edTempDir: TEdit;
    BitBtn1: TBitBtn;
    tsView: TTabSheet;
    gbFileTypes: TGroupBox;
    edFileExts: TEdit;
    tsSecurity: TTabSheet;
    gbExcludeFiles: TGroupBox;
    cbEnableMode: TCheckBox;
    edExcludeExts: TEdit;
    tsIntegration: TTabSheet;
    gbFolders: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edDefaultFolder: TEdit;
    btnBrows: TBitBtn;
    edDefaultExtractDir: TEdit;
    btnBrows1: TBitBtn;
    gbAssociation: TGroupBox;
    cbAssociate: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    Procedure WriteParams;
    Procedure ReadParams;
    { Public declarations }
  end;

var
  dlgConfig: TdlgConfig;

Const
    Section='Config';

implementation

{$R *.dfm}

Procedure TdlgConfig.WriteParams;
Var RegIni:TRegIniFile;
    Index:Byte;
Begin
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  RegIni.WriteBool(Section,'ShowToolBarCaptions',dlgConfig.cbToolBarCaptions.Checked);
  RegIni.WriteBool(Section,'RowSelect',dlgConfig.cbRowSelect.Checked);
  RegIni.WriteInteger(Section,'ViewStyle',dlgConfig.rgViewStyle.ItemIndex);
  RegIni.WriteString(Section,'DefArchFolder',dlgConfig.edDefaultFolder.Text);
  RegIni.WriteString(Section,'DefExtractFolder',dlgConfig.edDefaultExtractDir.Text);
  RegIni.WriteString(Section,'TempDir',dlgConfig.edTempDir.Text);
  RegIni.WriteString(Section,'UnpackFor',dlgConfig.edFileExts.Text);
  RegIni.WriteBool(Section,'EnableSecurity',dlgConfig.cbEnableMode.Checked);
  RegIni.WriteString(Section,'ExcludeExt',dlgCOnfig.edExcludeExts.Text);
  RegIni.WriteBool(Section,'Associate',dlgCOnfig.cbAssociate.Checked);
  RegIni.Free;
End;

procedure TdlgConfig.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Windows','wndConfig',Caption);
  tgGeneral.Caption:=ReadFromLanguage('Tabs','tbGeneral',tgGeneral.Caption);
  tbFolders.Caption:=ReadFromLanguage('Tabs','tbFolders',tbFolders.Caption);
  tsView.Caption:=ReadFromLanguage('Tabs','tbView',tsView.Caption);
  tsSecurity.Caption:=ReadFromLanguage('Tabs','tbSecurity',tsSecurity.Caption);
  tsIntegration.Caption:=ReadFromLanguage('Tabs','tbIntegration',tsIntegration.Caption);
  gbInterface.Caption:=ReadFromLanguage('Labels','lbInterface',gbInterface.Caption);
  cbToolBarCaptions.Caption:=ReadFromLanguage('Labels','lbShowCaptions',cbToolBarCaptions.Caption);
  cbRowSelect.Caption:=ReadFromLanguage('Labels','lbRowSelect',cbRowSelect.Caption);
  rgViewStyle.Caption:=ReadFromLanguage('Labels','lbViewStyle',rgViewStyle.Caption);
  gbFolders.Caption:=ReadFromLanguage('Labels','lbDefExtractFolder',gbFolders.Caption);
  Label2.Caption:=ReadFromLanguage('Labels','lbDefExtractFolder',Label2.Caption);
  Label1.Caption:=ReadFromLanguage('Labels','lbDefFolder',Label1.Caption);
  gbTempDir.Caption:=ReadFromLanguage('Labels','lbTempDir',gbTempDir.Caption);
  gbFileTypes.Caption:=ReadFromLanguage('Labels','lbFileTypes',gbFileTypes.Caption);
  gbExcludeFiles.Caption:=ReadFromLanguage('Labels','lbExcludeFiles',gbExcludeFiles.Caption);
  cbEnableMode.Caption:=ReadFromLanguage('Labels','lbEnableSecurity',cbEnableMode.Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  rgViewStyle.Items[0]:=ReadFromLanguage('Menu','mnuReport',rgViewStyle.Items[0]);
  rgViewStyle.Items[1]:=ReadFromLanguage('Menu','mnuList',rgViewStyle.Items[1]);
  rgViewStyle.Items[2]:=ReadFromLanguage('Menu','mnuSmallIcons',rgViewStyle.Items[2]);
  rgViewStyle.Items[3]:=ReadFromLanguage('Menu','mnuBigIcons',rgViewStyle.Items[3]);
  gbAssociation.Caption:=ReadFromLanguage('Labels','lbAssociation',gbAssociation.Caption);
  cbAssociate.Caption:=ReadFromLanguage('Labels','lbAssociate',cbAssociate.Caption);
end;

Procedure TdlgConfig.ReadParams;
var RegIni:TRegIniFile;
    Index:Byte;
begin
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  dlgConfig.cbToolBarCaptions.Checked:=RegIni.ReadBool(Section,'ShowToolBarCaptions',True);
  dlgConfig.cbRowSelect.Checked:=RegIni.ReadBool(Section,'RowSelect',True);
  dlgConfig.rgViewStyle.ItemIndex:=RegIni.ReadInteger(Section,'ViewStyle',0);
  dlgConfig.edDefaultFolder.Text:=RegIni.ReadString(Section,'DefArchFolder',GetSystemPath(TSystemPath(0)));
  dlgConfig.edDefaultExtractDir.Text:=RegIni.ReadString(Section,'DefExtractFolder',GetSystemPath(TSystemPath(0)));
  dlgConfig.edTempDir.Text:=RegIni.ReadString(Section,'TempDir',GetTempDir);
  dlgConfig.edFileExts.Text:=RegIni.ReadString(Section,'UnpackFor','*.exe;*.com;*.htm;*.bat;*.html');
  dlgConfig.cbEnableMode.Checked:=RegIni.ReadBool(Section,'EnableSecurity',False);
  dlgConfig.edExcludeExts.Enabled:=dlgConfig.cbEnableMode.Checked;
  dlgCOnfig.edExcludeExts.Text:=RegIni.ReadString(Section,'ExcludeExt','*.exe;*.com;*.bat');
  dlgCOnfig.cbAssociate.Checked:=RegIni.ReadBool(Section,'Associate',False);
  if RegIni.ReadBool(Section,'Associate',False) then
    RegisterApplication;

  RegIni.Free;
end;

end.
