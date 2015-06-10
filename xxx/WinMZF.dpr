program WinMZF;

uses
  Forms,
  Main_u in 'Main_u.pas' {FrmMain},
  Add_u in 'Add_u.pas' {DlgAdd},
  Progress_u in 'Progress_u.pas' {DlgProgress},
  Extract_u in 'Extract_u.pas' {DlgExtract},
  OverWrite_u in 'OverWrite_u.pas' {DlgOverWrite},
  DeleteFiles_u in 'DeleteFiles_u.pas' {DlgDeleteFiles},
  Comment_u in 'Comment_u.pas' {DlgComment},
  DragAndDrop_u in 'DragAndDrop_u.pas' {DlgOperation},
  CopyFiles_u in 'CopyFiles_u.pas' {DlgCopyMoveFiles},
  AdvancedConfig_u in 'AdvancedConfig_u.pas' {DlgMZFCompressOptions},
  Compute_u in 'Compute_u.pas' {DlgCompute};

//{$R *.res}

begin
  Application.Initialize;
  //Application.Title := 'The "XXX"';
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
