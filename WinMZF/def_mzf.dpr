program def_mzf;

uses
  Forms,
  SFX_wnd in 'SFX_wnd.pas' {dlgSFXExtract};

{$E sfx}
{$R Icon.res}

begin
  Application.Initialize;
  Application.CreateForm(TdlgSFXExtract, dlgSFXExtract);
  Application.Run;
end.
