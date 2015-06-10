unit Extract_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, ShellCtrls;

type
  TDlgExtract = class(TForm)
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
    cbIgnoreCRCCheck: TCheckBox;
    procedure DirTreeChange(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgExtract: TDlgExtract;

implementation

{$R *.dfm}

procedure TDlgExtract.DirTreeChange(Sender: TObject; Node: TTreeNode);
begin
  cbDestDir.Text:=DirTree.Path;
end;

end.
