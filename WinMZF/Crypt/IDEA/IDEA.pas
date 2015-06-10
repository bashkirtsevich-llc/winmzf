{ *********************************************************************** }
{                                                                         }
{ Delphi �ncryption Library                                               }
{ �ncryption / Decryption stream - IDEA                                   }
{                                                                         }
{ Copyright (c) 2004 by Matveev Igor Vladimirovich                        }
{ With offers and wishes write: teap_leap@mail.ru                         }
{                                                                         }
{ *********************************************************************** }

unit IDEA;

interface

uses
  SysUtils, Classes, Math;

const
  Rounds    = 8;
  KeyLength = (Rounds * 6) + 4;
  Maxim     = 65537;

type
  TIDEAKey   = array[0..KeyLength-1] of Word;
  TIDEABlock = array[1..4] of Word;

var
  Z : TIDEAKey;
  K : TIDEAKey;

  FBlockSize  : Integer;
  FKey        : string;
  FBufferSize : Integer;
  FKeySize    : Integer;
  FKeyPtr     : PChar;

////////////////////////////////////////////////////////////////////////////////
// �������������� �������

procedure Initialize(AKey: string);           // �������������
procedure CalculateSubKeys;                   // ���������� ���������
function  EncipherBlock(var Block): Boolean;  // �������� ����� (8 ����) 
function  DecipherBlock(var Block): Boolean;  // ���������� �����

////////////////////////////////////////////////////////////////////////////////
// �������� �������

function EncryptCopy(DestStream, SourseStream : TStream; Count: Int64;
  Key : string): Boolean;    // ����������� ������ �� ������ ������ � ������
  
function DecryptCopy(DestStream, SourseStream : TStream; Count: Int64;
  Key : string): Boolean;    // ������������ ������ �� ������ ������ � ������

function EncryptStream(DataStream: TStream; Count: Int64;
  Key: string): Boolean;     // ����������� ���������� ������

function DecryptStream(DataStream: TStream; Count: Int64;
  Key: string): Boolean;     // ������������ ���������� ������

implementation

////////////////////////////////////////////////////////////////////////////////

function ROL(a, s: LongWord): LongWord;
asm
  mov    ecx, s
  rol    eax, cl
end;

////////////////////////////////////////////////////////////////////////////////

procedure InvolveKey;
var
  TempKey : string;
  i, j    : Integer;
  K1, K2  : LongWord;
begin
 // �������������� ����� �� ������ 51 ������
 TempKey := FKey;
 i := 1;
 while ((Length(TempKey) mod FKeySize) <> 0) do
   begin
     TempKey := TempKey + TempKey[i];
     Inc(i);
   end;

 // Now shorten the key down to one KeySize block by combining the bytes
 i := 1;
 j := 0;
 while (i < Length(TempKey)) do
   begin
     Move((FKeyPtr+j)^, K1, 4);
     Move(TempKey[i], K2, 4);
     K1 := ROL(K1, K2) xor K2;
     Move(K1, (FKeyPtr+j)^, 4);
     j := (j + 4) mod FKeySize;
     Inc(i, 4);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

{$R-,Q-}
procedure ExpandKeys;
var
  i : Integer;
begin
 // ����������� ����� � Z
 Move(FKeyPtr^, Z, FKeySize);

 // ��������� �������� ������������
 for i := 8 to KeyLength-1 do
   begin
     if (((i+2) mod 8) = 0) then Z[i] := (Z[i- 7] shl 9) xor (Z[i-14] shr 7)
       else if (((i+1) mod 8) = 0) then Z[i] := (Z[i-15] shl 9) xor (Z[i-14] shr 7)
	 else Z[i] := (Z[i- 7] shl 9) xor (Z[i- 6] shr 7);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure InvertKeys;
type
  PWord	= ^Word;
var
  j          : Integer;
  pz, pp     : PWord;
  t1, t2, t3 : Word;

////////////////////////////////////////

  function Inv(I: Integer): Integer;
  var
    n1, n2, q, r, b1, b2, t : Integer;
  begin
   if (I = 0) then
     Result := 0 else
       begin
         n1 := Maxim;
         n2 := I;
         b2 := 1;
         b1 := 0;
         repeat
         r := (n1 mod n2);
         q := (n1-r) div n2;
         if (r = 0) then
           begin
             if (b2 < 0) then b2 := Maxim + b2;
           end else
               begin
                 n1 := n2;
                 n2 := r;
                 t  := b2;
                 b2 := b1 - q * b2;
                 b1 := t;
               end;
         until (r = 0);
         Result := b2;
       end;
  Result := (Result and $ffff);
  end;

////////////////////////////////////////

begin
    pz := @Z;
    pp := @K;
    Inc(pp, KeyLength);

//  t1 = inv(*Z++);
    t1 := Inv(pz^);
    Inc(pz);

//  t2 = -*Z++;
    t2 := -pz^;
    Inc(pz);

//  t3 = -*Z++;
    t3 := -pz^;
    Inc(pz);

//  *--p = inv(*Z++);
    Dec(pp);
    pp^ := Inv(pz^);
    Inc(pz);

//  *--p = t3;
    Dec(pp);
    pp^ := t3;

//  *--p = t2;
    Dec(pp);
    pp^ := t2;

//  *--p = t1;
    Dec(pp);
    pp^ := t1;

    for j := 1 to Rounds-1 do
      begin
//      t1 = *Z++;
        t1 := pz^;
        Inc(pz);

//      *--p = *Z++;
        Dec(pp);
        pp^ := pz^;
        Inc(pz);

//      *--p = t1;
        Dec(pp);
        pp^ := t1;

//      t1 = inv(*Z++);
        t1 := Inv(pz^);
        Inc(pz);

//      t2 = -*Z++;
        t2 := -pz^;
        Inc(pz);

//      t3 = -*Z++;
        t3 := -pz^;
        Inc(pz);

//      *--p = inv(*Z++);
        Dec(pp);
        pp^ := Inv(pz^);
        Inc(pz);

//      *--p = t2;
        Dec(pp);
        pp^ := t2;

//      *--p = t3;
        Dec(pp);
        pp^ := t3;

//      *--p = t1;
        Dec(pp);
        pp^ := t1;
      end;

//  t1 = *Z++;
    t1 := pz^;
    Inc(pz);

//  *--p = *Z++;
    Dec(pp);
    pp^ := pz^;
    Inc(pz);

//  *--p = t1;
    Dec(pp);
    pp^ := t1;

//  t1 = inv(*Z++);
    t1 := Inv(pz^);
    Inc(pz);

//  t2 = -*Z++;
    t2 := -pz^;
    Inc(pz);

//  t3 = -*Z++;
    t3 := -pz^;
    Inc(pz);

//  *--p = inv(*Z++);
    Dec(pp);
    pp^ := Inv(pz^);

//  *--p = t3;
    Dec(pp);
    pp^ := t3;

//  *--p = t2;
    Dec(pp);
    pp^ := t2;

//  *--p = t1;
    Dec(pp);
    pp^ := t1;
end;
{$R+,Q+}

////////////////////////////////////////////////////////////////////////////////

procedure CalculateSubKeys;
begin
 ExpandKeys;
 InvertKeys;
end;

////////////////////////////////////////////////////////////////////////////////

procedure Initialize(AKey: string);
begin
 FBlockSize  := 8;
 FBufferSize := 2048;
 FKey        := AKey;
 FKeySize    := 32;

 FillChar(Z, SizeOf(Z), 0);
 FillChar(K, SizeOf(K), 0);

 GetMem(FKeyPtr, FKeySize);
 FillChar(FKeyPtr^, FKeySize, #0);

 InvolveKey;
end;

////////////////////////////////////////////////////////////////////////////////

{$R-,Q-}
procedure Cipher(var Block: TIDEABlock; const Keys: TIDEAKey);
var
  x1, x2, x3, x4 : Word;
  t1, t2         : Word;
  pz             : ^Word;
  r	             : Integer;

////////////////////////////////////////

  function Mul(a,b: Word): Word;
  var
    p : LongWord;
  begin
   if (a > 0) then
   begin
     if (b > 0) then
     begin
       p := LongWord(a)*b;
       b := p and $ffff;
       a := p shr 16;
       Result := ((b - a) + Ord(b < a));
     end else Result := 1 - a;
   end else Result := 1 - b;
  end;

////////////////////////////////////////

begin
//  x1 = *in++;  x2 = *in++;
    x1 := Block[1];
    x2 := Block[2];
//  x3 = *in++;  x4 = *in;
    x3 := Block[3];
    x4 := Block[4];

    pz := @Keys;
    for r := 1 to Rounds do
      begin
//      MUL(x1,*Z++);
        x1 := Mul(x1, pz^);
        Inc(pz);

//      x2 += *Z++;
        x2 := x2 + pz^;
        Inc(pz);

//      x3 += *Z++;
        x3 := x3 + pz^;
        Inc(pz);

//      MUL(x4, *Z++);
        x4 := Mul(x4, pz^);
        Inc(pz);

//      t2 = x1^x3;
        t2 := x1 xor x3;

//      MUL(t2, *Z++);
        t2 := Mul(t2, pz^);
        Inc(pz);

//      t1 = t2 + (x2^x4);
        t1 := t2 + (x2 xor x4);

//      MUL(t1, *Z++);
        t1 := Mul(t1, pz^);
        Inc(pz);

//      t2 = t1+t2;
        t2 := (t1 + t2);

//      x1 ^= t1;
        x1 := x1 xor t1;

//      x4 ^= t2;
        x4 := x4 xor t2;

//      t2 ^= x2;
        t2 := t2 xor x2;

//      x2 = x3^t1;
        x2 := x3 xor t1;

//      x3 = t2;
        x3 := t2;
      end;

//  MUL(x1, *Z++);
    x1 := Mul(x1, pz^);
    Inc(pz);

//  *out++ = x1;
    Block[1] := x1;

//  *out++ = x3 + *Z++;
    Block[2] := x3 + pz^;
    Inc(pz);

//  *out++ = x2 + *Z++;
    Block[3] := x2 + pz^;
    Inc(pz);

//  MUL(x4, *Z);
    x4 := Mul(x4, pz^);

//  *out = x4;
    Block[4] := x4;
end;
{$R+,Q+}

////////////////////////////////////////////////////////////////////////////////

function EncipherBlock(var Block): Boolean;
begin
 Cipher(TIDEABlock(Block), Z);
 Result := TRUE;
end;

////////////////////////////////////////////////////////////////////////////////

function DecipherBlock(var Block): Boolean;
begin
 Cipher(TIDEABlock(Block), K);
 Result := TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// ������� ������� ...

function EncryptCopy(DestStream, SourseStream : TStream; Count: Int64;
  Key : string): Boolean;
var
  Buffer   : TIDEABlock;
  PrCount  : Int64;
  AddCount : Byte;
begin
 Result := True;
 try
   if Key = '' then
     begin
       DestStream.CopyFrom(SourseStream, Count);
       Exit;
     end;
   Initialize(Key);
   CalculateSubKeys;
   PrCount := 0;
   while Count - PrCount >= 8 do
     begin
       SourseStream.Read(Buffer, SizeOf(TIDEABlock));
       EncipherBlock(Buffer);
       DestStream.Write(Buffer, SizeOf(TIDEABlock));
       Inc(PrCount, 8);
     end;

   AddCount := Count - PrCount;
   if Count - PrCount <> 0 then
     begin
       SourseStream.Read(Buffer, AddCount);
       DestStream.Write(Buffer, AddCount);
     end;
 except
   Result := False;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function DecryptCopy(DestStream, SourseStream : TStream; Count: Int64;
  Key : string): Boolean;
var
  Buffer   : TIDEABlock;
  PrCount  : Int64;
  AddCount : Byte;
begin
 Result := True;
 try
   if Key = '' then
     begin
       DestStream.CopyFrom(SourseStream, Count);
       Exit;
     end;
   Initialize(Key);
   CalculateSubKeys;
   PrCount := 0;
   while Count - PrCount >= 8 do
     begin
       SourseStream.Read(Buffer, SizeOf(TIDEABlock));
       DecipherBlock(Buffer);
       DestStream.Write(Buffer, SizeOf(TIDEABlock));
       Inc(PrCount, 8);
     end;

   AddCount := Count - PrCount;
   if Count - PrCount <> 0 then
     begin
       SourseStream.Read(Buffer, AddCount);
       DestStream.Write(Buffer, AddCount);
     end;
 except
   Result := False;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function EncryptStream(DataStream: TStream; Count: Int64; Key: string): Boolean;
var
  Buffer   : TIDEABlock;
  PrCount  : Int64;
  AddCount : Byte;
begin
 Result := True;
 try
   if Key = '' then
     begin
       DataStream.Seek(Count, soFromCurrent);
       Exit;
     end;
   Initialize(Key);
   CalculateSubKeys;
   PrCount := 0;
   while Count - PrCount >= 8 do
     begin
       DataStream.Read(Buffer, SizeOf(TIDEABlock));
       EncipherBlock(Buffer);
       DataStream.Seek(-SizeOf(TIDEABlock), soFromCurrent);
       DataStream.Write(Buffer, SizeOf(TIDEABlock));
       Inc(PrCount, 8);
     end;
 except
   Result := False;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function DecryptStream(DataStream: TStream; Count: Int64; Key: string): Boolean;
var
  Buffer   : TIDEABlock;
  PrCount  : Int64;
begin
 Result := True;
 try
   if Key = '' then
     begin
       DataStream.Seek(Count, soFromCurrent);
       Exit;
     end;
   Initialize(Key);
   CalculateSubKeys;
   PrCount := 0;
   while Count - PrCount >= 8 do
     begin
       DataStream.Read(Buffer, SizeOf(TIDEABlock));
       DecipherBlock(Buffer);
       DataStream.Seek(-SizeOf(TIDEABlock), soFromCurrent);
       DataStream.Write(Buffer, SizeOf(TIDEABlock));
       Inc(PrCount, 8);
     end;
 except
   Result := False;
 end;
end;

// ���������� ������� ������� ...
////////////////////////////////////////////////////////////////////////////////

end.