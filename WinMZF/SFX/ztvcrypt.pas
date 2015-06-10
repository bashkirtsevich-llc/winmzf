(**********************************************************************

  Copyright 1998-2003,  Microchip Data Systems / Carl Bunton

  Under license agreement, this source module may be used only on a
  single computer.

  No portion of this module may be reproduced, copied, revised, edited,
  distributed or transmitted via electronic means except in compiled
  application format.

  Web-site:  http://www.ziptv.com
  Email:     custsupt@ziptv.com

**********************************************************************)
Unit ztvCrypt;


Interface



Uses
   Windows,
   Classes,
   SysUtils,
   ztvStreams;

{$I ZipTV.inc}


Type
   Tak = Array[0..2] Of longint;
   pak = ^Tak;

Const
   DefK: Tak = (305419896, 591751049, 878082192); // do not change

Var
   k: Tak;


Procedure ztvDecodeBuf(p: Pointer; size: Integer);
Procedure ztvDecodeByte(Var C: Byte);
Procedure ztvEncodeBuf(p: Pointer; size: Integer);
Function ztvEncodeByte(C: Byte): Byte;
Function ztvDecryptByte: Byte;

Procedure update_keys(C: Byte);
Function seed_keys(Password: String): Integer;
Function decrypt_pw(Encrypt_Head: PChar; BitFlag: word; CRC,
   FileDate: longint; Password: String): Boolean;
Function ztvEncryptHead(passwd: String; Seed: Integer; CRC: longint): String;
Function ztvEncryptStream(InStrm, OutStrm: TStream32; Len: Int64; Password:
	ShortString; Var cHeader: ShortString; WriteHeader: Boolean): u_long;
Procedure ztvDecryptStreamCRC(InStrm, OutStrm: TStream32; Password: String; CRC: u_long);
Function EncryptBuffer(inbufptr, outbufptr: Pointer; Len: Integer;
   Password: String; WriteHeader: Boolean): u_long;



Implementation


Uses
   ztvRegister,
   ztvBase,
   ztvGbls;



//-------------------------------------------------------------
{ Update the encryption keys with the next Byte of plain text }

Procedure update_keys(C: Byte);
Begin
   k[0] :=
      Crc32Table[u_long((k[0] Xor C) And $00FF)]
      Xor u_long((k[0] Shr 8) And $00FFFFFF);
   k[1] := k[1] + k[0] And $FF;
   k[1] := k[1] * 134775813 + 1;
   k[2] :=
      Crc32Table[u_long(k[2] Xor k[1] Shr 24 {HiByte(HiWord(k[1]))}) And $00FF]
      Xor (u_long(k[2] Shr 8) And $00FFFFFF);
End;
//-------------------------------------------------------------

Function seed_keys(Password: String): Integer;
Var
   i: Byte;
Begin
   k := DefK;
   For i := 1 To Length(Password) Do
      update_keys(Byte(Password[i]));

   Result := vKey;
End;
//-------------------------------------------------------------
{ Return the next Byte in the pseudo-Random sequence }

Function ztvDecryptByte: Byte;
Var
   t: word;
Begin
   t := word(k[2] Or 2);
   Result := Byte(word((t * (t Xor 1)) Shr 8) And $FF);
End;
//-------------------------------------------------------------

Procedure ztvDecodeBuf(p: Pointer; size: Integer);
Var
   i: Integer;
   pByte: ^Byte Absolute p;
Begin
   For i := 1 To size Do
   Begin
      pByte^ := pByte^ Xor ztvDecryptByte();
      update_keys(pByte^);
      inc(pByte);
   End;
End;
//-------------------------------------------------------------
(* return the encoded value of the Byte c of the plain text *)

Procedure ztvDecodeByte(Var C: Byte);
Begin
   C := C Xor ztvDecryptByte();
   update_keys(C);
End;
//-------------------------------------------------------------
(* return the encoded value of the Byte c of the plain text *)

Procedure ztvEncodeBuf(p: Pointer; size: Integer);
Var
   i: Integer;
   Temp: word;
   pByte: ^Byte Absolute p;
Begin
   For i := 1 To size Do
   Begin
      Temp := ztvDecryptByte();
      update_keys(pByte^);
      pByte^ := Temp Xor pByte^;
      inc(pByte);
   End;
End;
//-------------------------------------------------------------
(* return the encoded value of the Byte c of the plain text *)

Function ztvEncodeByte(C: Byte): Byte;
Var
   Temp: word;
Begin
   Temp := ztvDecryptByte();
   update_keys(C);
   Result := Temp Xor C;
End;
//-------------------------------------------------------------

Function ztvEncryptHead(passwd: String; Seed: Integer; CRC: longint): String;
Var
   i: Byte;
  //t: TTimeStamp;
Begin
   If seed_keys(passwd) <> 0 Then Exit;
   Randomize;                           //RandSeed := t.Time; {Seed}

   SetLength(Result, RAND_HEAD_LEN);
   For i := 1 To 10 Do
      Result[i] := char(ztvEncodeByte(Byte(Random($7FFF) Shr 7)));

   Result[11] := char(ztvEncodeByte(LoByte(HiWord(CRC)))); //char(crc SHR 16);
   Result[12] := char(ztvEncodeByte(HiByte(HiWord(CRC)))); //char(crc SHR 24);
End;
//-------------------------------------------------------------

Function decrypt_pw(Encrypt_Head: PChar; BitFlag: word; CRC,
   FileDate: longint; Password: String): Boolean;
Var
   i, C, b: Byte;
Begin
   Result := False;
   If Password = '' Then Exit;

   If seed_keys(Password) <> 0 Then Exit;
   For i := 0 To RAND_HEAD_LEN - 1 Do
   Begin
      C := Byte(Encrypt_Head[i + RAND_HEAD_LEN]) Xor ztvDecryptByte();
      update_keys(C);
      Encrypt_Head[i] := char(C);
   End;

 (* version pre 2.0 -- (currently not used) *)
 //c := Byte(Encrypt_Head[EncHead_len - 2]);

 (* version 2.0+ *)
   b := Byte(Encrypt_Head[RAND_HEAD_LEN - 1]);

   If Not ((BitFlag And 8) = 8) Then
      Result := b = HiByte(HiWord(CRC))   //(CRC SHR 24) THEN
   Else
   	//IF c = LoByte(LoWord(FileDate)) THEN    (* version pre 2.0 -- (currently not used) *)
      Result := b = LoWord(FileDate) Shr 8; //HiByte(LoWord(FileDate)) THEN
End;
//-------------------------------------------------------------

{
Untested...

Function DecryptBuffer( inBufPtr, outBufPtr: Pointer; Var Len: Integer;
 Password: String ): u_long;
Var
 //DS: TDecryptStream;
 inStrm, outStrm: TStream32;
Begin
   Result := CRC_MASK;

   If ( outBufPtr = Nil) Or ( inBufPtr = Nil) Or ( Password = '' ) Or
     ( Len < 1 ) Then
    Exit;

   inStrm := TMemoryStream.Create();
   Try
    outStrm := TMemoryStream.Create();
      Try
   inStrm.WriteBuffer( inBufPtr^, Len );
         // encrypt the stream
         ztvDecryptStreamCRC( inStrm, outStrm, Password, Result );
         //DS := TDecryptStream.Create( inStrm, outStrm, Password, Result );
         //DS.Free();

         outStrm.ReadBuffer( outBufPtr^, outstrm.Size );
      Finally
       outStrm.Free();
      End;
   Finally
    inStrm.Free();
   End;
End;}
//-------------------------------------------------------------
// returns calculated crc of the buffer
// returns Length of encrypted buffer in parameter "Len"
//
// ALWAYS allocate memory outBufPtr with an additional RAND_HEAD_LEN bytes,
// if calling with WriteHeader = True!
//
// the value of the parameter "Len" should be passed without including the
// size of RAND_HEAD_LEN... always call with actual buffer length.
//
// WriteHeader: include the crypted header in resulting buffer?  The return
// value of the parameter "Len" will be it's calling value + RAND_HEAD_LEN

Function EncryptBuffer(inbufptr, outbufptr: Pointer; Len: Integer;
   Password: String; WriteHeader: Boolean): u_long;
Var
   ES: TEncryptStream;
   InStrm, OutStrm: TStream32;
Begin
   Result := CRC_MASK;

   If (outbufptr = Nil) Or (inbufptr = Nil) Or (Password = '') Or
      (Len < 1) Then
      Exit;

   InStrm := TMemoryStream32.Create();
   Try
      OutStrm := TMemoryStream32.Create();
      Try
         InStrm.WriteBuffer(inbufptr^, Len);

         // encrypt the stream
       	//Result := ztvEncryptStream( inStrm, outStrm, Password, WriteHeader );
         ES := TEncryptStream.Create(InStrm, OutStrm, InStrm.Size, Password, WriteHeader);
         Result := ES.CRC;
         ES.Free();

         OutStrm.ReadBuffer(outbufptr^, OutStrm.size);
      Finally
         OutStrm.Free();
      End;
   Finally
      InStrm.Free();
   End;
End;
//-------------------------------------------------------------
// returns the crc of encrypted stream

Function ztvEncryptStream(InStrm, OutStrm: TStream32; Len: Int64; Password: ShortString;
	Var cHeader: ShortString; WriteHeader: Boolean): u_long;
Var
   ES: TEncryptStream;
Begin
   ES := TEncryptStream.Create(InStrm, OutStrm, Len, Password, WriteHeader);
   Result := ES.CRC;
   cHeader := ES.CryptHDR;
   ES.Free();
End;
//-------------------------------------------------------------

// must be called with crc value!
Procedure ztvDecryptStreamCRC(InStrm, OutStrm: TStream32; Password: String; CRC: u_long);
Var
   ES: TDecryptStreamCRC;
Begin
   ES := TDecryptStreamCRC.Create(InStrm, OutStrm, Password, CRC);
   ES.Free();
End;
//-------------------------------------------------------------


End.
