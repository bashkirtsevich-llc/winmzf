(**********************************************************************

  Copyright 1998-2003,  Microchip Data Systems / Carl Bunton

  Under license agreement, this source module may be used only on a
  single computer.

  No portion of this module may be reproduced, copied, revised, edited,
  distributed or transmitted via electronic means except in compiled
  application format.

  Web-site:  http://www.ziptv.com
  Email:     custsupt@ziptv.com

  Version 4.7.2: revised:
  	1. Var                          Var:
  		  	LzhInBuf: Arj_Buf_PTR;    		LzhInBuf: Arj_Buf_Array;
   	  	LzhOutBuf: Arj_Buf_PTR;   		LzhOutBuf: Arj_Buf_Array;

	2. Var								  Var
   		c_len: C_LEN_PTR;   	     		c_len: C_LEN_ARRAY;
   		pt_len: PT_LEN_PTR;   			pt_len: C_LEN_ARRAY;
   		pt_table: PT_TABLE_PTR;  		pt_table: PT_TABLE_ARRAY;
   		left: LFT_PTR;   					left: LFT_ARRAY;
   		right: RGT_PTR;   				right: RGT_ARRAY;
   		c_table: C_TABLE_PTR;   		c_table: C_TABLE_ARRAY;

   3. Deinitialize procedure deleted.  No longer required.

**********************************************************************)
Unit ztvLzh3;

Interface

Uses
   Windows,
   Messages,
   SysUtils,
   Graphics,
   Controls,
   Dialogs,
   ztvGbls,
   ztvBase,
   ztvCrypt,
   Err_Msgs,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines

Const
   CODE_BIT = 16;
   CHAR_BIT = 8;

Type
	// v4.8.4
   Buf_Array = Array[0..(WSIZE * 2) + 1 {BUFSIZE}] Of Byte;
   Buf_PTR = ^Buf_Array;

Var
   bitbuf: word;
   LzhBufSize: Integer;
   LzhInBuf: Buf_Array;
   LzhOutBuf: Buf_Array;

Procedure lzh_initialize(crc_bit_size, aDICBIT, aPBIT, CM: Byte);
Procedure init_getbits(Sender: TUnBASE; Infile: TStream32);
Procedure fillbuf(Sender: TUnBASE; Infile: TStream32; n: ShortInt);
Procedure lzh_decode(Sender: TUnBASE; Infile: TStream32; Var Outfile: TStream32; IR: TInflateRec);

Implementation

(* CryptMethod = 1: BlakHole, 2: Arj,	3:	LHA *)

(* alphabet = {0, 1, 2, ..., NC - 1} *)
Const
   CTABLESIZE = 4096;
   CBIT = 9;
   TBIT = 5;
   BITBUFSIZ = (CHAR_BIT * SizeOf(bitbuf));
   THRESHOLD = 3;
   UCHAR_MAX = 255;
   MAXMATCH = UCHAR_MAX + 1;
   NC = (UCHAR_MAX + MAXMATCH + 2 - THRESHOLD);
   NT = (CODE_BIT + 3);

Type
   C_LEN_PTR = ^C_LEN_ARRAY;
   C_LEN_ARRAY = Array[0..NC] Of word;  //Byte; 041900
   PT_LEN_PTR = ^PT_LEN_ARRAY;
   PT_LEN_ARRAY = Array[0..{16} 19] Of word; //Byte;	041900	(* ARRAY[0..NPT] OF Byte; *)
   PT_TABLE_PTR = ^PT_TABLE_ARRAY;
   PT_TABLE_ARRAY = Array[0..255] Of word;
   LFT_PTR = ^LFT_ARRAY;
   LFT_ARRAY = Array[0..(2 * NC - 1)] Of word;
   RGT_PTR = ^RGT_ARRAY;
   RGT_ARRAY = Array[0..(2 * NC - 1)] Of word;
   C_TABLE_PTR = ^C_TABLE_ARRAY;
   C_TABLE_ARRAY = Array[0..CTABLESIZE] Of word;

Var
   c_len: C_LEN_ARRAY;
   pt_len: C_LEN_ARRAY; 
   pt_table: PT_TABLE_ARRAY;
   left: LFT_ARRAY;
   right: RGT_ARRAY;
   c_table: C_TABLE_ARRAY;
   blocksize: word;
   inptr: u_long;
   bitcount: ShortInt;                  //Word; 042000 //SMALLINT;
   InBufSize,
      FileProgressPos: u_long;
   CryptMethod,
      crc_size,
      PBIT,
      subbitbuf,
      DICBIT: Byte;

   //-------------------------------------------------------------

Procedure lzh_initialize(crc_bit_size, aDICBIT, aPBIT, CM: Byte);
Begin
   DICBIT := aDICBIT;
   PBIT := aPBIT;
   CryptMethod := CM;

   If CryptMethod >= 5 Then
   	// v4.8.4
      LzhBufSize := {32768}65536
   Else
      LzhBufSize := 26624;

   crc_size := crc_bit_size;            (* assign 16/32 to global *)
End;
//-------------------------------------------------------------

Function GetNextByte(Sender: TUnBASE; Infile: TStream32): Byte;
Var
	LocalBufSize: DWord;
Begin
   With Sender Do
   Begin
      If inptr >= (InBufSize - 1) Then
      Begin
         // use the following block, instead of the min function...
         // the min function fails with files > 4 gig.
         //LocalBufSize := Min(Bytes_To_Go, LzhBufSize);
         If Bytes_To_Go > LzhBufSize Then
            LocalBufSize := LzhBufSize
         Else
            LocalBufSize := Bytes_To_Go;

         InBufSize :=
         	ReadBlock(Infile, Nil, LzhInBuf[0], (CryptMethod = 1) Or
            	(CryptMethod = 2), 0, LocalBufSize, dtData);

         If InBufSize = 0 Then
         Begin
            Result := 0;
            Exit;
         End;

         //Dec(Bytes_To_Go, InBufSize);
         inptr := 0;
      End
      Else
         inc(inptr);

      Dec(Bytes_To_Go);
      Result := Byte(LzhInBuf[inptr]);
   End;
End;
//-------------------------------------------------------------
(* Shift bitbuf n bits left, read n bits *)

Procedure fillbuf(Sender: TUnBASE; Infile: TStream32; n: ShortInt);
Begin
   (* lose the first n bits *)
   bitbuf := (bitbuf Shl n) And $FFFF;
   While n > bitcount Do
   Begin
      n := n - bitcount;
      bitbuf := bitbuf Or (subbitbuf Shl n);

      subbitbuf := GetNextByte(Sender, Infile);
      bitcount := CHAR_BIT;
   End;
   bitcount := bitcount - n;
   bitbuf := bitbuf Or subbitbuf Shr bitcount;
End;
//-------------------------------------------------------------

Procedure init_getbits(Sender: TUnBASE; Infile: TStream32);
Begin
   bitbuf := 0;
   bitcount := 0;
   blocksize := 0;
   subbitbuf := 0;
   inptr := WSIZE;                      {make it greater than InBufSize to force original read}
   InBufSize := 1;
   fillbuf(Sender, Infile, BITBUFSIZ);
End;
//-------------------------------------------------------------

Procedure lzh_decode(Sender: TUnBASE; Infile: TStream32; Var Outfile: TStream32; IR: TInflateRec);
Var
   {j				: INTEGER;}{remaining bytes to copy}
   {i				: word;}{needs to be static for decode function}

   decoded: Boolean;
   //DICSIZ 		: word;
   NP: Byte;
   //NT				: Byte;
   //NPT			: Byte;
    //-------------------------------------------------------------

   Function getbits(n: smallint): word;
   Var
      x: word;
   Begin
      x := bitbuf Shr (BITBUFSIZ - n);
      fillbuf(Sender, Infile, n);
      Result := x;
   End;
   //-------------------------------------------------------------
   (* Max value for tablesize = 8182 *)

   Function make_table(nchar: smallint; Var bitlen: Array Of word; //Byte; 041900
      tablebits: smallint; Var TABLE: Array Of word; tablesize: smallint): Boolean;
   Var
      Count: Array[1..16] Of word;
      weight: Array[1..16] Of word;
      START: Array[1..17] Of word;

      p: ^smallint;
      Len: smallint;
      i, k, ch, jutbits,
      	avail, Mask, nextcode: word;
   Begin
      Result := True;

      For i := 1 To 16 Do
         Count[i] := 0;

      For i := 0 To nchar - 1 Do
         inc(Count[bitlen[i]]);

      START[1] := 0;
      For i := 1 To 17 Do
         START[i + 1] := START[i] + (Count[i] Shl (16 - i));

      If (START[17] <> word(1 Shl 16)) Then
      Begin
         Sender.RaiseErrorStr(Sender.ArchiveFile, '', '0', E_BADTABLE);
         Result := False;
         Exit;
      End;

      jutbits := 16 - tablebits;

      For i := 1 To tablebits Do
      Begin
         START[i] := START[i] Shr jutbits;
         weight[i] := word(1 Shl (tablebits - i));
      End;

      {WHILE (i <= 16) DO
      BEGIN
         weight[i] := 1 SHL (16 - i);
         i := i + 1;
      END;}

      For i := (tablebits + 1) To 16 Do
         weight[i] := word(1 Shl (16 - i));

      i := START[tablebits + 1] Shr jutbits;

      If i <> word(1 Shl 16) Then
      Begin
         k := word(1 Shl tablebits);
         While (i <> k) Do
         Begin
            TABLE[i] := 0;
            i := i + 1;
         End;
      End;

      avail := nchar;
      Mask := word(1 Shl (15 - tablebits));
      For ch := 0 To nchar - 1 Do
      Begin
         Len := {word} bitlen[ch];      (* 7/28/01 *)
         If Len = 0 Then
            Continue;

         k := START[Len];
         nextcode := k + weight[Len];
         If Len <= tablebits Then
         Begin
            For i := START[Len] To nextcode - 1 Do
               TABLE[i] := ch;
         End
         Else
         Begin
            p := @TABLE[k Shr jutbits];
            i := Len - tablebits;
            While (i <> 0) Do
            Begin
               If p^ = 0 Then
               Begin
                  right[avail] := 0;
                  left[avail] := 0;
                  p^ := avail;
                  avail := avail + 1;
               End;

               If (k And Mask) <> 0 Then
                  p := @right[p^]
               Else
                  p := @left[p^];

               k := k Shl 1;
               i := i - 1;
            End;
            p^ := ch;
         End;
         START[Len] := nextcode;
      End;
   End;
   //-------------------------------------------------------------

   Function read_pt_len(nn, nbit: Byte; i_special: ShortInt): Boolean;
   Var
      i, n: smallint;
      C: ShortInt;
      Mask: smallint;
   Begin

      Result := True;
      n := getbits(nbit);

      If n = 0 Then
      Begin
         C := getbits(nbit);

         For i := 0 To nn - 1 Do
            pt_len[i] := 0;

         For i := 0 To 255 Do
            pt_table[i] := C;

      End
      Else
      Begin

         i := 0;
         While (i < n) Do
         Begin
            C := bitbuf Shr (BITBUFSIZ - 3);
            If C = 7 Then
            Begin
               Mask := 1 Shl (BITBUFSIZ - 1 - 3);
               While (Mask And bitbuf) <> 0 Do
               Begin
                  Mask := Mask Shr 1;
                  C := C + 1;
               End;
            End;

            If C < 7 Then
               fillbuf(Sender, Infile, 3)
            Else
               fillbuf(Sender, Infile, C - 3);

            pt_len[i] := C;
            i := i + 1;
            If i = i_special Then
            Begin
               C := getbits(2);
               C := C - 1;
               While C >= 0 Do
               Begin
                  pt_len[i] := 0;
                  i := i + 1;
                  C := C - 1;
               End;
            End;
         End;

         While i < nn Do
         Begin
            pt_len[i] := 0;
            i := i + 1;
         End;

         Result := make_table(nn, pt_len, 8, pt_table, SizeOf(pt_table));
      End;
   End;
   //-------------------------------------------------------------

   Function read_c_len: Boolean;
   Var
      i, C, n: smallint;
      Mask: word;
   Begin
      Result := True;

      n := getbits(CBIT);
      If n = 0 Then
      Begin
         C := getbits(CBIT);

         For i := 0 To NC - 1 Do
            c_len[i] := 0;

         For i := 0 To CTABLESIZE - 1 Do
            c_table[i] := C;

      End
      Else
      Begin
         i := 0;
         While i < n Do
         Begin
            C := pt_table[bitbuf Shr (BITBUFSIZ - 8)];
            If C >= NT Then
            Begin
               Mask := 1 Shl (BITBUFSIZ - 1 - 8);
               Repeat
                  If (bitbuf And Mask) <> 0 Then
                     C := right[C]
                  Else
                     C := left[C];

                  Mask := Mask Shr 1;
               Until (C < NT);
            End;

            fillbuf(Sender, Infile, smallint(pt_len[C]));

            If C <= 2 Then
            Begin
               If C = 0 Then
                  C := 1
               Else
                  If C = 1 Then
                     C := getbits(4) + 3
                  Else
                     C := getbits(CBIT) + 20;

               C := C - 1;

               While C >= 0 Do
               Begin
                  c_len[i] := 0;
                  C := C - 1;
                  i := i + 1;
               End;
            End
            Else
            Begin
               c_len[i] := Byte(C - 2);
               i := i + 1;
            End;
         End;

         While i < NC Do
         Begin
            c_len[i] := 0;
            i := i + 1;
         End;

         Result := make_table(NC, c_len, 12, c_table, SizeOf(c_table));
      End;
   End;
   //-------------------------------------------------------------

   Function decode_c: word;
   Var
      j, Mask: word;
   Begin
      If blocksize = 0 Then
      Begin
         blocksize := getbits(16);
         If (blocksize = 0) Or
            (Not
            (read_pt_len(NT, TBIT, 3) And
            read_c_len() And
            read_pt_len(NP, PBIT, -1))) Then
         Begin
            decoded := True;
            Result := 0;
            Exit;
         End;
      End;

      blocksize := blocksize - 1;
      j := c_table[bitbuf Shr 4];

      If j >= NC Then
      Begin
         Mask := 1 Shl (BITBUFSIZ - 1 - 12);
         Repeat
            If (bitbuf And Mask) > 0 Then
               j := right[j]
            Else
               j := left[j];

            Mask := Mask Shr 1;
         Until (j < NC);
      End;
      fillbuf(Sender, Infile, smallint(c_len[j]));
      Result := j;
   End;
   //-------------------------------------------------------------

   Function decode_p: word;
   Var
      j, Mask: word;
   Begin
      j := pt_table[bitbuf Shr (BITBUFSIZ - 8)];
      If (j >= NP) Then
      Begin
         Mask := word(1 Shl (BITBUFSIZ - 1 - 8));
         Repeat
            If (bitbuf And Mask) > 0 Then
               j := right[j]
            Else
               j := left[j];

            Mask := Mask Shr 1;
         Until (j < NP);
      End;

      fillbuf(Sender, Infile, smallint(pt_len[j]));

      If (j <> 0) Then
      Begin
         Dec(j);
         j := (word(1 Shl (j {-1}))) + getbits(smallint((j)));
      End;

      Result := j;
   End;
   //-------------------------------------------------------------
Var
   i, j, C, R: Integer;
   Counter: DWord;
Begin

   With Sender Do
   Try

      R := 0;
      Counter := 0;
      FileProgressPos := InflateRec.UnpackedSize;
      //DICSIZ := word(1 SHL DICBIT);
      NP := (DICBIT + 1);
      //NT := (CODE_BIT  + 3);
      //IF NT > NP THEN
      //   NPT := NT
      //ELSE
      //   NPT := NP;
      init_getbits(Sender, Infile);
      decoded := False;

      Try

         While (Counter < InflateRec.UnpackedSize) Do
         Begin
            C := decode_c;
            If decoded Then             // 080700: added this block
               break;

            If (C <= UCHAR_MAX) Then
            Begin
               LzhOutBuf[R] := C;
               inc(Counter);
               inc(R);
               If (R >= LzhBufSize) Then
               Begin
                  R := ExtractWriteBlock(Outfile, LzhOutBuf, False, crc_size, R, dtData);
                  If R = 0 Then
                     RaiseError(E_RAISE, ArchiveFile, '', '0', E_FWRITE);

                  Dec(FileProgressPos, R);
                  ProgressPosition := ProgressPosition - R;
                  doBranchProgress(InflateRec.UnpackedSize - FileProgressPos, InflateRec.UnpackedSize, fTotalUnpackedSize);

                  R := 0;
               End;
            End
            Else
            Begin
               j := C - (UCHAR_MAX + 1 - THRESHOLD);
               inc(Counter, j);

               i := decode_p;
               i := R - i - 1;

               If (i < 0) Then
                  inc(i, LzhBufSize);

               If (R > i) And (R < LzhBufSize - MAXMATCH - 1) Then
               Begin
                  Dec(j);
                  While (j >= 0) Do
                  Begin
                     LzhOutBuf[R] := LzhOutBuf[i];
                     Dec(j);
                     inc(R);
                     inc(i);
                  End;
               End
               Else
               Begin
                  Dec(j);
                  While (j >= 0) Do
                  Begin
                     LzhOutBuf[R] := LzhOutBuf[i];
                     inc(R);
                     If (R >= LzhBufSize) Then
                     Begin
                        R := ExtractWriteBlock(Outfile, LzhOutBuf, False, crc_size, R, dtData);
                        If R = 0 Then
                           RaiseError(E_RAISE, ArchiveFile, '', '0', E_FWRITE);

                        Dec(FileProgressPos, R);
                        ProgressPosition := ProgressPosition - R;
                        doBranchProgress(InflateRec.UnpackedSize - FileProgressPos, InflateRec.UnpackedSize, fTotalUnpackedSize);

                        R := 0;
                     End;

                     Dec(j);
                     inc(i);
                     If (i >= LzhBufSize) Then
                        i := 0;
                  End;
               End;
            End;
         End;
      Finally
         If (R > 0) Then
         Begin
            R := ExtractWriteBlock(Outfile, LzhOutBuf, False, crc_size, R, dtData);
            If R = 0 Then
               RaiseError(E_RAISE, ArchiveFile, '', '0', E_FWRITE);

            Dec(FileProgressPos, R);
            ProgressPosition := ProgressPosition - R;
            doBranchProgress(InflateRec.UnpackedSize - FileProgressPos, InflateRec.UnpackedSize, fTotalUnpackedSize);
            //r := 0;
         End;
      End;

   Except
      Raise;                            // required!  See TUnARJ
   End;
End;
//-------------------------------------------------------------

End.
