program UnMZF;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MZF_decompress;

const
  Help='For extract use next format:'#13+
       'UnMZF -Archive -Dest dir'#13+
       'Exsample: UnMZF "Archive.mzf" "Dest dir"';

Type TConsoleManager=Object                     
  Public
    procedure ArchiverOnBegin(Sender:TObject);
    procedure ArchiverProgress(Sender:TObject;FProgress,AProgress:Byte;
                               Var Abort:Boolean);
    procedure ArchiverOnEnd(Sender:TObject);
    procedure GetPass(Sender:TObject;Var Pass:ShortString);
    procedure OnError(Sender:TObject;ErrorMsg:ShortString);
    procedure OnExtract(Sender:TObject;_FileName:ShortString);
  End;

procedure TConsoleManager.ArchiverOnBegin(Sender:TObject);
begin
  Writeln('Extract');
end;

procedure TConsoleManager.ArchiverProgress(Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean);
begin
  If FProgress Mod 3 = 0 Then
    Write('*');
end;

procedure TConsoleManager.ArchiverOnEnd(Sender:TObject);
begin
  Writeln('Done');
end;

procedure TConsoleManager.GetPass(Sender: TObject; var Pass: OpenString);
begin
  Writeln('Please enter password!!!');
  Readln(Pass);
end;

procedure TConsoleManager.OnError(Sender: TObject; ErrorMsg: ShortString);
begin
  Writeln(ErrorMSG);
end;

procedure TConsoleManager.OnExtract(Sender: TObject; _FileName: ShortString);
begin
  Writeln(ExtractFileName(_FileName),'-Extracted');
end;

var Decompressor:TMZFDecompressor;
    a:TConsoleManager;
begin
  Writeln('WinMZF Extractor v.1.3 by M.A.D.M.A.N.');
  if ParamCount<1 then
    Begin
      Writeln(Help);
      Exit;
    End;
  try
    Writeln('Extract:',ParamStr(1),' To:',ParamStr(2));
    Decompressor:=TMZFDecompressor.Create;
    Decompressor.ArchiveFile:=ParamStr(1);
    Decompressor.DestinationDir:=ParamStr(2);
    Decompressor.ExtractAll:=True;
    Decompressor.FileList.Add('*.*');
    Decompressor.OnBegin:=a.ArchiverOnBegin;
    Decompressor.OnEnd:=a.ArchiverOnEnd;
    Decompressor.OnGetPass:=a.GetPass;
    Decompressor.OnError:=a.OnError;
    Decompressor.OnExtract:=a.OnExtract;
    Decompressor.Extract;
    Decompressor.Free;
  except
    (*on E:Exception do
      Writeln(E.Classname, ': ', E.Message);*)
  end;
end.
