(*

  Copyright...2008...MADMAN
      ___           ___           ___           ___           ___           ___
     /\__\         /\  \         /\  \         /\__\         /\  \         /\__\
    /::|  |       /::\  \       /::\  \       /::|  |       /::\  \       /::|  |
   /:|:|  |      /:/\:\  \     /:/\:\  \     /:|:|  |      /:/\:\  \     /:|:|  |
  /:/|:|__|__   /::\~\:\  \   /:/  \:\__\   /:/|:|__|__   /::\~\:\  \   /:/|:|  |__
 /:/ |::::\__\ /:/\:\ \:\__\ /:/__/ \:|__| /:/ |::::\__\ /:/\:\ \:\__\ /:/ |:| /\__\
 \/__/~~/:/  / \/__\:\/:/  / \:\  \ /:/  / \/__/~~/:/  / \/__\:\/:/  / \/__|:|/:/  /
       /:/  /       \::/  /   \:\  /:/  /        /:/  /       \::/  /      |:/:/  /
      /:/  /        /:/  /     \:\/:/  /        /:/  /        /:/  /       |::/  /
     /:/  /        /:/  /       \::/__/        /:/  /        /:/  /        /:/  /
     \/__/         \/__/         ~~            \/__/         \/__/         \/__/

  NOTE!!!...Set buffer size equils length of password.
*)
unit AES_256_u;

interface

Uses aes_type, aes_cbc, SysUtils, Classes, Windows;

Type
  TKey128 = array[0..31] of Byte;

Const
  BufSize = $512;

  Procedure AES256_EncodeStream(InStream,OutStream:TStream;Password:String);
  Procedure AES256_DecodeStream(InStream,OutStream:TStream;Password:String);

implementation

Uses md5,Main_u;

Procedure Convert(Source:MD5Digest;Var Dest:TAESBlock);
Var Index:Byte;
Begin
  For Index:=0 To 15 Do
    Dest[Index]:=Source[Index];
End;

Procedure AES256_EncodeStream(InStream,OutStream:TStream;Password:String);
Var
  IV: TAESBlock;
  buffer  : array[0..BufSize-1] of byte;
  Key:TKey128;
  i:Byte;
  Err:integer;
  ctx: TAESContext;
  n:Word;
  Temp:TStream;
Begin
  //Randomize;
  InStream.Position:=0;
  OutStream.Position:=0;
  Convert(MD5String(Password),IV);
  {For i:=0 to 15 do
    IV[i] := Random($FF);    }

  For i:=0 To 31 Do
    Begin
      If i<=Length(Password) Then
        Key[i]:=Byte(Password[i+1]) Else
        Key[i]:=0;
    End;

  Err := AES_CBC_Init_Encr(key, $100, IV, ctx);
  If Err<>0 then
    Begin
      //writeln(format('Error %x',[err]));
      MessageBox(FrmMain.Handle,PChar(Format('Encrypt initialize error, code "%x"',[Err])),'Error',MB_OK+MB_ICONHAND);
      Exit;
    End;
  {OutStream.Write(IV,SizeOf(TAESBlock));}
  While Instream.Position<>InStream.Size Do
    Begin
      n:=InStream.Read(buffer,BufSize{Length(Password)});
      Temp:=TMemoryStream.Create;
      Temp.WriteBuffer(buffer,n);
      Err := AES_CBC_Encrypt(@buffer, @buffer, n, ctx);
      Convert(MD5Stream(Temp),IV);
      Err := AES_CBC_Init_Encr(key, 256, IV, ctx);
      temp.Free;
      If err<>0 Then
        Begin
          //writeln(format('Error %x',[err]));
          MessageBox(FrmMain.Handle,PChar(Format('Encrypt error, code "%x"',[Err])),'Error',MB_OK+MB_ICONHAND);
          Exit;
        End;
      OutStream.Write(buffer,n);
    End;
  InStream.Position:=0;
  OutStream.Position:=0;
End;

Procedure AES256_DecodeStream(InStream,OutStream:TStream;Password:String);
Var
  i:Byte;
  Err:integer;
  ctx: TAESContext;
  IV: TAESBlock;
  key: TKey128;
  n:integer;
  buffer  : array[0..BufSize-1] of byte;
  Temp:TStream;
Begin
  InStream.Position:=0;
  OutStream.Position:=0;

  Convert(MD5String(Password),IV);
  
  For i:=0 To 31 Do
    Begin
      If i<=Length(Password) Then
        Key[i]:=Byte(Password[i+1]) Else
        Key[i]:=0;
    End;


  {Instream.Read(IV,SizeOf(TAESBlock));}
  Err := AES_CBC_Init_Decr(Key, $100, IV, ctx);
  If Err<>0 then
    Begin
      writeln(format('Error %x',[err]));
      //MessageBox(FrmMain.Handle,PChar(Format('Decrypt initialize error, code "%x"',[Err])),'Error',MB_OK+MB_ICONHAND);
      Exit;
    End;   
  While InStream.Position<>InStream.Size Do
    Begin
      n:=InStream.Read(Buffer,BufSize{Length(Password)});
      Temp:=TMemoryStream.Create;
      Err := AES_CBC_Decrypt(@buffer, @buffer, n, ctx);
      Temp.WriteBuffer(Buffer,n);
      Convert(MD5Stream(Temp),IV);
      Err := AES_CBC_Init_Decr(Key, 256, IV, ctx);
      Temp.Free;
      If err<>0 Then
        Begin
          writeln(format('Error %x',[err]));
          //MessageBox(FrmMain.Handle,PChar(Format('Decrypt error, code "%x"',[Err])),'Error',MB_OK+MB_ICONHAND);
          exit;
        End;
      OutStream.Write(buffer,n);
    End;
  InStream.Position:=0;
  OutStream.Position:=0;
End;

end.
