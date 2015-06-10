unit View_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, Buttons;

type
  TdlgViewer = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    cblFiles: TCheckListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgViewer: TdlgViewer;

implementation

{$R *.dfm}

end.
