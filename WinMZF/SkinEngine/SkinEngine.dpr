program SkinEngine;

uses
  Forms,
  Main_u in 'Main_u.pas' {wndMain},
  Constructor_u in 'Constructor_u.pas',
  View_u in 'View_u.pas' {dlgViewer},
  Info_u in 'Info_u.pas' {dlgInfo},
  About_u in 'About_u.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'WinMZF Skin editor';
  Application.CreateForm(TwndMain, wndMain);
  Application.Run;
end.
