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
Unit ztvDelzw;

Interface

Uses
   Windows,
   SysUtils,
   ztvBase,
   ztvGbls,
   ztvStreams,
   Err_Msgs;

{$I ZipTV.inc}                          //Declare the compiler defines

Function UnLzw(Sender: TUnBASE; Infile: TStream32; Var Outfile: TStream32; PackedSize: DWord): Boolean;

Implementation

Type
   char_type = Char;
   code_int = LongInt;
   count_int = LongInt;
   cmp_code_int = LongInt;
   w_unsigned = Word;

Const
   BITS = 16;
   INIT_BITS = 9;                       // Initial number of bits per code
   INBUFSIZ = $8000;                    // input buffer size
   INBUF_EXTRA = 64;                    // required by UnLzw
   OUTBUFSIZ = 16384;                   // output buffer size
   OUTBUF_EXTRA = 2048;                 // required by UnLzw
   DIST_BUFSIZE = $8000;                // buffer for distances

   // Mask $20 is reserved to mean a fourth header byte, and $40 is free.
   // It's a pity that old uncompress does not check bit $20. That makes
   // extension of the format actually undesirable because old compress
   // would just crash on the new format instead of giving a meaningful
   // error message. It does check the number of bits, but it's more
   // helpful to say "unsupported format, get a new version" than
   // "can only handle 16 bits".
   BIT_MASK = $1F;                      // Mask for 'number of compression bits'

   // Block compression: if table is full and compression rate is dropping,
   // clear the dictionary.
   BLOCK_MODE = $80;
   LZW_RESERVED = $60;                  // reserved bits
   Clear = 256;                         // flush the dictionary
   FIRST = Clear + 1;                   // first free entry

Type
   inbufptr = ^inbufbuf;
   inbufbuf = Array[0..INBUFSIZ + INBUF_EXTRA] Of Char;
   outbufptr = ^outbufbuf;
   outbufbuf = Array[0..OUTBUFSIZ + OUTBUF_EXTRA] Of Char;
   d_bufptr = ^d_bufbuf;
   d_bufbuf = Array[0..DIST_BUFSIZE] Of ush;
   windowptr = ^windowbuf;
   windowbuf = Array[0..2 * WSIZE] Of Char;
   tab_prefixptr = ^tab_prefixbuf;
   tab_prefixbuf = Array[0..(2 * WSIZE) {(1 SHL BITS)}] Of ush;

Var
   inbuf: inbufptr;
   outbuf: outbufptr;
   d_buf: d_bufptr;
   tab_prefix: tab_prefixptr;
   tab_suffix: windowptr
   ;
   rsize: DWord;
   insize: DWord;
   bytes_in: DWord;                     // number of input bytes
   bytes_out: DWord;                    // number of output bytes
   inptr: w_unsigned;                   // index of next byte to be processed in inbuf

//-------------------------------------------------------------

Function MAXCODE(n: Integer): LongInt;
Begin
   Result := 1 Shl n;
End;
//-------------------------------------------------------------

Function Input(b: PChar; Var o: DWord; n: smallint; m: w_unsigned): code_int;
Var
   p: DWord;
   z: Array[0..2] Of Byte;
Begin
   p := o Shr 3;
   CopyMem(@b[p], @z[0], 3);

   If (rsize = 0) And (DWord(p + 3) > insize) Then
   Begin
      FillChar(z[insize - p], insize - p - 1, 0);
   End;
   Result := (((z[0] Or (z[1] Shl 8) Or (z[2] Shl 16)) Shr (o And $7)) And m);
   Inc(o, n);
End;
//-------------------------------------------------------------
// Fill input buffer. Called only when the buffer is empty
//-------------------------------------------------------------

Function fill_inbuf(UnBase: TUnBASE; Infile: TStream32): Byte; // v4.1.11 changed
Begin
   insize := UnBase.ReadBlock(Infile, Nil, inbuf[insize], False, 0, INBUFSIZ, dtHeader);

   If (insize = 0) Then
   Begin
      Result := 0;
      Exit;
   End;

   bytes_in := bytes_in + insize;
   inptr := 1;
   Result := Byte(inbuf[0]);
End;
//-------------------------------------------------------------

Function get_byte(UnBase: TUnBASE; Infile: TStream32): Byte;	// v4.1.11 changed
Begin
   If inptr < insize Then
   Begin
      Result := Byte(inbuf[inptr]); // v4.1.11 changed;
      Inc(inptr);
   End
   Else
      Result := fill_inbuf(UnBase, Infile) {(0)};
End;
//-------------------------------------------------------------

Function de_stack: PChar;
Begin
   Result := @(d_buf[DIST_BUFSIZE - 1]);
End;
//-------------------------------------------------------------

// ==================================================================
// Decompress in to out.  This routine adapts to the codes in the
// file building the "string" table on-the-fly; requiring no table to
// be stored in the compressed file.
// IN assertions: the buffer inbuf contains already the beginning of
//   the compressed data, from offsets iptr to insize-1 included.
//   The magic header has already been checked and skipped.
//   bytes_in and bytes_out have been initialized.

Function UnLzw(Sender: TUnBASE; Infile: TStream32; Var Outfile: TStream32; PackedSize: DWord): Boolean;
Var
   stackp: PChar;
   code: code_int;
   finchar: Integer;
   oldcode: code_int;
   incode: code_int;
   inbits: DWord;
   posbits: DWord;
   outpos: DWord;
   bitmask: w_unsigned;
   free_ent: code_int;
   maxcode1: code_int;
   maxmaxcode: code_int;
   n_bits: smallint;
   i: Integer;
   e, o: DWord;
   maxbits: Integer;                    // max bits per code for LZW
   MAGIC: String[2];
   block_mode1: LongInt;
Begin

   inbuf := Nil;
   outbuf := Nil;
   d_buf := Nil;
   tab_prefix := Nil;
   tab_suffix := Nil;

   Result := False;
   Try
      New(inbuf);
      New(outbuf);
      New(d_buf);
      New(tab_prefix);
      New(tab_suffix);
      Try
         inptr := 2;
         insize := 0;
         Sender.Bytes_To_Go := PackedSize;

         MAGIC[1] := char_type(get_byte(Sender, Infile));
         MAGIC[2] := char_type(get_byte(Sender, Infile));
         maxbits := get_byte(Sender, Infile);
         block_mode1 := maxbits And BLOCK_MODE;

         If ((maxbits And LZW_RESERVED) <> 0) Then
            Raise E_RAISE.Create(LoadStr(E_CODESET));

         maxbits := maxbits And BIT_MASK;
         maxmaxcode := MAXCODE(maxbits);

         //If (maxbits > BITS) THEN
         //  Raise E_RAISE.Create( LoadStr( E_SHANNONFANO ) );;

         rsize := insize;
         n_bits := INIT_BITS;
         maxcode1 := MAXCODE(n_bits) - 1;

         bitmask := (1 Shl n_bits) - 1;
         oldcode := -1;
         finchar := 0;
         outpos := 0;
         posbits := inptr Shl 3;
         bytes_out := 0;
         bytes_in := 0;

         If block_mode1 > 0 Then
            free_ent := FIRST
         Else
            free_ent := 256;

         FillChar(tab_prefix[0], 256 * SizeOf(ush), 0);

         For code := 255 Downto 0 Do
            tab_suffix[code] := char_type(code);

         Repeat

            o := posbits Shr 3;
            e := insize - o;

            CopyMem(@inbuf[o], @inbuf[0], e);
            //For i := 0 To e - 1  Do
            //	  inbuf[i] := inbuf[ i + o ];

            insize := e;
            posbits := 0;

            Dec(Sender.Bytes_To_Go, o {insize});
            If Sender.Bytes_To_Go < 1 Then
               Sender.Bytes_To_Go := 0;

            // Single file compressed file only
            Sender.PercentByFile := CalcProgress64(PackedSize - Sender.Bytes_To_Go, PackedSize);
            Sender.DoProgress(Sender.PercentByFile, Sender.PercentByFile);

            If (insize < INBUF_EXTRA) Then
            Begin
               rsize := Sender.ReadBlock(Infile, Nil, inbuf[insize], False, 0, INBUFSIZ, dtHeader);
               If rsize = 0 Then ;      //RAISE E_RAISE.Create( LoadStr( E_FREAD ) );

               Inc(insize, rsize);
               bytes_in := bytes_in + rsize;
            End;

            If insize > 0 Then          // v4.1.5 added (see taxa*.gz)
            Begin
               If rsize <> 0 Then
                  inbits := (insize - insize Mod DWord(n_bits)) Shl 3
               Else
                  inbits := DWord(insize Shl 3) - DWord(n_bits - 1);
            End
            Else
               inbits := 0;

            While (inbits > posbits) Do
            Begin

               If (free_ent > maxcode1) Then
               Begin
                  posbits := ((posbits - 1) +
                     (DWord(n_bits Shl 3) -
                     (posbits - 1 + DWord(n_bits Shl 3)) Mod
                     DWord(n_bits Shl 3)
                     )
                     );

                  Inc(n_bits);

                  If (n_bits = maxbits) Then
                     maxcode1 := maxmaxcode
                  Else
                     maxcode1 := MAXCODE(n_bits) - 1;

                  bitmask := (1 Shl n_bits) - 1;
                  break;
                  Continue;
               End;

               code := Input(inbuf^, posbits, n_bits, bitmask);

               If (oldcode = -1) Then
               Begin
                  If (code >= 256) Then
                     Raise E_RAISE.Create(LoadStr(E_CORRUPTINPUT));

                  oldcode := code;
                  finchar := Integer(oldcode);
                  outbuf[outpos] := char_type(finchar);
                  Inc(outpos);

                  Continue;
               End;

               If (code = Clear) And (block_mode1 > 0) Then
               Begin
                  FillChar(tab_prefix[0], 256 * SizeOf(ush), 0);

                  free_ent := FIRST - 1;
                  posbits := ((posbits - 1) +
                     (DWord(n_bits Shl 3) -
                     (posbits - 1 + DWord(n_bits Shl 3)) Mod
                     DWord(n_bits Shl 3)
                     )
                     );

                  n_bits := INIT_BITS;
                  maxcode1 := MAXCODE(n_bits) - 1;
                  bitmask := (1 Shl n_bits) - 1;
                  break;
                  Continue;
               End;

               incode := code;
               stackp := de_stack;

               // Special case for KwKwK string
               If (code >= free_ent) Then
               Begin
                  If (code > free_ent) Then
                  Begin
                     If (outpos > 0) Then
                     Begin
                        Sender.ExtractWriteBlock(Outfile, outbuf^, False, 32, outpos, dtData);
                        bytes_out := bytes_out + outpos;
                     End;
                     Exit;              // corrupt input
                  End;

                  Dec(stackp);
                  stackp^ := char_type(finchar);

                  code := oldcode;
               End;

               While (cmp_code_int(code) >= 256 {cmp_code_int(256)}) Do
               Begin
                  // Generate output characters in reverse order
                  Dec(stackp);
                  stackp^ := tab_suffix[code];
                  code := tab_prefix[code];
               End;

               finchar := Integer(tab_suffix[code]);
               Dec(stackp);
               stackp^ := char_type(finchar);

               // put them out in forward order
               i := (de_stack - stackp);

               If (outpos + DWord(i) >= OUTBUFSIZ) Then
               Begin

                  Repeat
                     If (DWord(i) > OUTBUFSIZ - outpos) Then
                        i := OUTBUFSIZ - outpos;

                     If i > 0 Then
                     Begin
                        If i > 1 Then
                           CopyMem(stackp, @outbuf[outpos], i)
                        Else
                           outbuf[outpos] := stackp^;

                        Inc(outpos, i);
                     End;

                     If (outpos >= OUTBUFSIZ) Then
                     Begin
                        Sender.ExtractWriteBlock(Outfile, outbuf^, False, 32, outpos, dtData);
                        bytes_out := bytes_out + outpos;
                        outpos := 0;
                     End;

                     Inc(stackp, i);
                     i := (de_stack - stackp);

                  Until i = 0;

               End
               Else
               Begin

                  If i > 1 Then
                     CopyMem(stackp, @outbuf[outpos], i)
                  Else
                     outbuf[outpos] := stackp^;

                  Inc(outpos, i);
               End;

               code := free_ent;

               // Generate the new entry
               If (free_ent < maxmaxcode) Then
               Begin
                  tab_prefix[code] := oldcode;
                  tab_suffix[code] := char_type(finchar);
                  free_ent := code + 1;
               End;

               // Remember previous code
               oldcode := incode;

            End;
         Until rsize = 0;

         If (outpos > 0) Then
         Begin
            Sender.ExtractWriteBlock(Outfile, outbuf^, False, 32, outpos, dtData);
            bytes_out := bytes_out + outpos;
         End;

         Result := True;
      Finally
         If inbuf <> Nil Then dispose(inbuf);
         If outbuf <> Nil Then dispose(outbuf);
         If d_buf <> Nil Then dispose(d_buf);
         If tab_prefix <> Nil Then dispose(tab_prefix);
         If tab_suffix <> Nil Then dispose(tab_suffix);
      End;
   Except
   End;

End;
//-------------------------------------------------------------

End.
