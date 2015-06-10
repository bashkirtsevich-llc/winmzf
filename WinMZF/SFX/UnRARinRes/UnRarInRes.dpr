program UnRarInRes;

uses
  Forms,
  Main_u in 'Main_u.pas' {dlgExtract},
  UnRAR in 'UnRAR.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdlgExtract, dlgExtract);
  Application.Run;
end.
