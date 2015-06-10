unit Extract_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, ShellCtrls, Advanced;

type
  TdlgExtract = class(TForm)
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    MainControl: TPageControl;
    GeneralSheet: TTabSheet;
    AdvancedSheet: TTabSheet;
    cbDestDir: TComboBox;
    rgUpdateMode: TRadioGroup;
    Label1: TLabel;
    DirTree: TShellTreeView;
    rgExtractMode: TRadioGroup;
    gbFileTime: TGroupBox;
    cbUpdateModifyTime: TCheckBox;
    cbUpdateAccessTime: TCheckBox;
    rgDeleteArchive: TRadioGroup;
    procedure DirTreeChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgExtract: TdlgExtract;

implementation

{$R *.dfm}

procedure TdlgExtract.DirTreeChange(Sender: TObject; Node: TTreeNode);
begin
  cbDestDir.Text:=DirTree.Path;
end;

procedure TdlgExtract.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Windows','wndEtract',Caption);
  GeneralSheet.Caption:=ReadFromLanguage('Tabs','tbGeneral',GeneralSheet.Caption);
  AdvancedSheet.Caption:=ReadFromLanguage('Tabs','tbAdvanced',AdvancedSheet.Caption);
  Label1.Caption:=ReadFromLanguage('Labels','lbExtractDir',Label1.Caption);
  rgUpdateMode.Caption:=ReadFromLanguage('Labels','lbUpdateMode',rgUpdateMode.Caption);
  rgExtractMode.Caption:=ReadFromLanguage('Labels','lbExtractMode',rgExtractMode.Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  gbFileTime.Caption:=ReadFromLanguage('Labels','lbFileTime',gbFileTime.Caption);
  rgDeleteArchive.Caption:=ReadFromLanguage('Labels','lbDeleteArchive',rgDeleteArchive.Caption);
  cbUpdateModifyTime.Caption:=ReadFromLanguage('Labels','lbUpdateMod',cbUpdateModifyTime.Caption);
  cbUpdateAccessTime.Caption:=ReadFromLanguage('Labels','lbUpdateAccess',cbUpdateAccessTime.Caption);
  //
  rgUpdateMode.Items.Strings[0]:=ReadFromLanguage('Items','itOverwrite',rgUpdateMode.Items.Strings[0]);
  rgUpdateMode.Items.Strings[1]:=ReadFromLanguage('Items','itSkip',rgUpdateMode.Items.Strings[1]);
  rgUpdateMode.Items.Strings[2]:=ReadFromLanguage('Items','itOverwritePrompt',rgUpdateMode.Items.Strings[2]);
  //
  rgExtractMode.Items.Strings[0]:=ReadFromLanguage('Items','itExtractAll',rgExtractMode.Items.Strings[0]);
  rgExtractMode.Items.Strings[1]:=ReadFromLanguage('Items','itExtractSelected',rgExtractMode.Items.Strings[1]);
  //
  rgDeleteArchive.Items.Strings[0]:=ReadFromLanguage('Items','itNever',rgDeleteArchive.Items.Strings[0]);
  rgDeleteArchive.Items.Strings[1]:=ReadFromLanguage('Items','itAlways',rgDeleteArchive.Items.Strings[1]);
end;

end.
