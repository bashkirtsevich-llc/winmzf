program Rc6Test;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  RC6, Classes;
Var InStream,OutStream:TMemoryStream;
begin
  Write('RC6 Stream tester by M.A.D.M.A.N.');
  Writeln('[e\d] InFile OutFile Password');
  If ParamCount=0 Then
    Begin
      Writeln('NULL command parameters!!!');
      Exit;
    End;
  If Not FileExists(ParamStr(2)) Then
    Begin
      Writeln('Coud not find file!');
      Exit;
    End;
  InStream:=TMemoryStream.Create;
  InStream.LoadFromFile(ParamStr(2));
  OutStream:=TMemoryStream.Create;
  If UpperCase(ParamStr(1))='E' Then
    EncryptCopy(OutStream,InStream,InStream.Size,ParamStr(4)) Else
  If UpperCase(ParamStr(1))='D' Then
    DecryptCopy(OutStream,InStream,InStream.Size,ParamStr(4))
    Else
    Begin
      Writeln('Encode or decode?');
      InStream.Free;
      OutStream.Free;
      Exit;
    End;

  OutStream.SaveToFile(ParamStr(3));
  InStream.Free;
  OutStream.Free;
  Writeln('All done!');

end.
