program CryptXOR;
{$APPTYPE CONSOLE}

uses Windows;

var
  key, text, longkey, result : string;
  i : integer;
  toto, c : char;
  F : TextFile;
begin
  writeln('Enter the key:');
  readln(key);
  writeln('Enter the text:');
  readln(text);

  for i := 0 to (length(text) div length(key)) do
    longkey := longkey + key;

  for i := 1 to length(text) do
  begin
    // XOR ��������
    toto := chr((ord(text[i]) xor ord(longkey[i])));
    result := result + toto;
  end;
  writeln('The crypted text is:');
  writeln(result);
  write('Should i save it to result.txt ?');
  read(c);
  if c in ['Y','y'] then
  begin
    AssignFile(F,'result.txt');
    Rewrite(F);
    Writeln(F,result);
    CloseFile(F);
  end;
end.