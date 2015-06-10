unit SeletFolder_u;

interface                                                         

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShellCtrls, Buttons;

type
  TdlgSelectFolder = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edDestFolder: TEdit;
    ShellDirTree: TShellTreeView;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    procedure ShellDirTreeChange(Sender: TObject; Node: TTreeNode);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgSelectFolder: TdlgSelectFolder;

implementation

uses Advanced;

{$R *.dfm}

procedure TdlgSelectFolder.ShellDirTreeChange(Sender: TObject;
  Node: TTreeNode);
begin
  edDestFolder.Text:=ShellDirTree.Path;
end;

procedure TdlgSelectFolder.btnOkClick(Sender: TObject);
begin
  {If Not DirectoryExists(edDestFolder.Text) Then
    Begin
      MessageBox(Handle,'There are is not directory, please try again','Error',MB_OK+MB_ICONEXCLAMATION);
      Exit;
    End;}
  ModalResult:=mrOk;
end;

procedure TdlgSelectFolder.FormCreate(Sender: TObject);
begin
  Caption:=ReadFromLanguage('Windows','wndSelectDir',Caption);
  btnCancel.Caption:=ReadFromLanguage('Buttons','btnCancel',btnCancel.Caption);
  Label1.Caption:=ReadFromLanguage('Labels','lbSelectDir',Label1.Caption);
  Label2.Caption:=ReadFromLanguage('Labels','bFolder',Label2.Caption);
end;

end.
