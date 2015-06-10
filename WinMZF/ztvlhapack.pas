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
Unit ztvLhaPack;

Interface

Uses
   Windows,
   Classes,
   SysUtils,
   ztvBase,
   ztvGbls,
   ztvStreams,
   Err_Msgs;

{$I ZipTV.inc}                          //Declare the compiler defines

Procedure LzhPack(Sender: TCompBase; Infile, Outfile: TStream32; InStream, OutStream: TStream32);
Procedure LZHUnpack(Sender: TUnBASE; Infile: TStream32; Var Outfile: TStream32; InStream: TStream32; IR: TInflateRec);

Implementation

Const
   (* LZSS Parameters *)
   OUTBUFSIZE = 4096;                   (* size of string buffer *)
   lookahead = 60;                      (* size of look-ahead buffer *)
   THRESHOLD = 2;
   NODENIL = OUTBUFSIZE;                (* end of tree's node  *)

   (* Huffman coding parameters *)
   N_Char = (256 - THRESHOLD + lookahead);

   (* Character code ( := 0..N_Char-1 ) *)
   t = (N_Char * 2 - 1);                (* size of table *)
   R = (t - 1);                         (* root position *)

   (* update when cumulative frequency *)
   (* reaches to this value *)
   MAX_FREQ = $8000;
   //getbuf           : WORD = 0;		// v4.0 rem'd
   //putbuf           : WORD = 0;      // v4.0 rem'd
   //getlen           : BYTE = 0;		// v4.0 rem'd
   //putlen           : BYTE = 0;		// v4.0 rem'd
   //codesize         : SMALLINT = 0;	// v4.0 rem'd
   //match_length     : SMALLINT = 0;	// v4.0 rem'd
   //match_position   : SMALLINT = 0;	// v4.0 rem'd

Var
   getbuf: word;                        //v4.0
   putbuf: word;                        //v4.0
   getlen: Byte;                        //v4.0
   putlen: Byte;                        //v4.0
   codesize: smallint;                  //v4.0
   match_length: smallint;              //v4.0
   match_position: smallint;            //v4.0


Type
   TFreq = Array[0..t] Of word;
   PFreqPtr = ^TFreq;
   TParent = Array[0..Pred(t + N_Char)] Of smallint;
   PParent = ^TParent;
   TSon = Array[0..Pred(t)] Of smallint;
   PSon = ^TSon;
   TTextBuf = Array[0..OUTBUFSIZE + lookahead - 2] Of Byte;
   PTextBuf = ^TTextBuf;
   TDad = Array[0..OUTBUFSIZE] Of smallint;
   PDad = ^TDad;
   TRson = Array[0..OUTBUFSIZE + 256] Of smallint;
   PRson = ^TRson;
   LZHBuf = Array[1..WSIZE + 16] Of Byte;
   pLZHBuf = ^LZHBuf;

Var
   Text_Buf: PTextBuf;
   dad: PDad;
   son: PSon;
   lson: PDad;
   rson: PRson;
   freq: PFreqPtr;
   Parent: PParent;
   SRCBuf: pLZHBuf;
   DSTBuf: pLZHBuf;
   Buf: DWord;
   PosnR: word;
   PosnW: word;
   fSize: DWord;
   FileProgressPos: u_long;

//-------------------------------------------------------------

Procedure PackGetBlock(Sender: TCompBase; Infile: TStream32; DummyStream: TStream32; Var Target; NoBytes: word; Var Actual_Bytes: word);
Begin
   If (PosnR > Buf) Or (PosnR + NoBytes > SUCC(Buf)) Then
   Begin
      If PosnR > Buf Then
         Buf :=
         	Sender.ReadBlock(Infile, Nil, SRCBuf^, False, 16, WSIZE, dtData)
      Else
      Begin
         CopyMem(@SRCBuf^[PosnR], @SRCBuf^[1], Buf - PosnR);
         Buf :=
         	Sender.ReadBlock(Infile, Nil, SRCBuf^[Buf - PosnR], False, 16,
            	WSIZE - (Buf - PosnR), dtData);
         Buf := Buf - PosnR;
      End;

      If Buf = 0 Then
      Begin
         Actual_Bytes := 0;
         Exit;
      End;

      With Sender Do
      Begin
         Dec(Bytes_To_Go, Buf);
         ProgressPosition := ProgressPosition - Buf;
         doBranchProgress(fSize - Bytes_To_Go, fSize, fTotalPackedSize);
      End;

      PosnR := 1;
   End;

   CopyMem(@SRCBuf^[PosnR], @Target, NoBytes);
   inc(PosnR, NoBytes);

   If PosnR > SUCC(Buf) Then
      Actual_Bytes := NoBytes - (PosnR - SUCC(Buf))
   Else
      Actual_Bytes := NoBytes;
End;
//-------------------------------------------------------------

Procedure PackPutBlock(Sender: TCompBase; Outf: TStream32; Var source; NoBytes: word; Var Actual_Bytes: word);
Begin

   If (NoBytes = 0) Then
   Begin
      //Inc( Sender.aWrite, Sender.CompressWriteBlock( {Sender,} Outf, DSTBuf^, False, 0, PosnW - 1, dtData ) );
      Sender.CompressWriteBlock( {Sender,} Outf, DSTBuf^, False, 0, PosnW - 1, dtData );
      Exit;
   End;

   If (PosnW > WSIZE) Or
      (PosnW + NoBytes > SUCC(WSIZE)) Then
   Begin
      //Inc( Sender.aWrite, Sender.CompressWriteBlock( {Sender,} Outf, OutStream, DSTBuf^, False, 0, PosnW - 1, dtData ) );
      Sender.CompressWriteBlock( Outf, DSTBuf^, False, 0, PosnW - 1, dtData );
      //If NoBytes = 0 THEN Exit;
      PosnW := 1;
   End;

   CopyMem(@source, @DSTBuf^[PosnW], NoBytes);
   inc(PosnW, NoBytes);
   Actual_Bytes := NoBytes;

End;
//-------------------------------------------------------------

Procedure UnpackGetBlock(Sender: TUnBASE; Infile: TStream32; Var Target; NoBytes: word; Var Actual_Bytes: word);
Var
	BufSize: Integer;
Begin
   If (PosnR > Buf) Or (PosnR + NoBytes > SUCC(Buf)) Then
   Begin
      With Sender Do
      Begin
         If PosnR > Buf Then
         Begin
            // use the following block, instead of the min function...
            // the min function fails with files > 4 gig.
            //BufSize := Min(Bytes_To_Go, WSIZE);
            If Bytes_To_Go > WSIZE Then
               BufSize := WSIZE
            Else
               BufSize := Bytes_To_Go;

            Buf :=
            	ReadBlock(Infile, Nil, SRCBuf^,
               	False, 0, BufSize, dtHeader)
         End Else Begin

            // use the following block, instead of the min function...
            // the min function fails with files > 4 gig.
				//BufSize := Min(Bytes_To_Go, WSIZE - (Buf - PosnR))
            If Bytes_To_Go > WSIZE - (Buf - PosnR) Then
               BufSize := WSIZE - (Buf - PosnR)
            Else
               BufSize := Bytes_To_Go;

            CopyMem(@SRCBuf^[PosnR], @SRCBuf^[1], Buf - PosnR);

            Buf :=
            	ReadBlock(Infile, Nil, SRCBuf^, False, 0, BufSize, dtHeader);

            Buf := Buf - PosnR;
         End;
      End;

      If Buf = 0 Then
      Begin
         Actual_Bytes := 0;
         Exit;
      End;
      PosnR := 1;
   End;

   CopyMem(@SRCBuf^[PosnR], @Target, NoBytes);
   inc(PosnR, NoBytes);

   If PosnR > SUCC(Buf) Then
      Actual_Bytes := NoBytes - (PosnR - SUCC(Buf))
   Else
      Actual_Bytes := NoBytes;

End;
//-------------------------------------------------------------

Procedure UnpackPutBlock(Sender: TUnBASE; Outf: TStream32; Var source;
   NoBytes: word; Var Actual_Bytes: word);
Var
   Counter: Integer;
Begin

   With Sender Do
   Begin

      If NoBytes = 0 Then               (* Flush condition *)
      Begin
         Counter := ExtractWriteBlock(Outf, DSTBuf^, False, 16, PosnW - 1, dtData);
         If Counter = 0 Then
            RaiseError(E_RAISE, fFileName, '', '0', E_FWRITE);

         ProgressPosition := ProgressPosition - Counter;
         Dec(FileProgressPos, Counter);
         doBranchProgress(InflateRec.UnpackedSize - FileProgressPos,
            InflateRec.UnpackedSize, fTotalUnpackedSize);
      End;

      If (PosnW > WSIZE) Or (PosnW + NoBytes > SUCC(WSIZE)) Then
      Begin
         Crc32Val := Crc16Val;
         Counter := ExtractWriteBlock(Outf, DSTBuf^, False, 16, Pred(PosnW), dtData);
         PosnW := 1;
         If Counter = 0 Then
            RaiseError(E_RAISE, fFileName, '', '0', E_FWRITE);

         ProgressPosition := ProgressPosition - Counter;
         Dec(FileProgressPos, Counter);
         doBranchProgress(InflateRec.UnpackedSize - FileProgressPos,
            InflateRec.UnpackedSize, fTotalUnpackedSize);
      End;

   End;

   CopyMem(@source, @DSTBuf^[PosnW], NoBytes);

   inc(PosnW, NoBytes);
   Actual_Bytes := NoBytes;
End;
//-------------------------------------------------------------

Procedure InsertNode(R: smallint);      (* Inserting node to the tree *)
Var
   C, cmp, i, p, tmp: smallint;
   key: PTextBuf;
Begin

   cmp := 1;
   key := @Text_Buf^[R];
   p := SUCC(OUTBUFSIZE) + key^[0];
   rson^[R] := NODENIL;
   lson^[R] := NODENIL;
   match_length := 0;

   While match_length < lookahead Do
   Begin
      If (cmp >= 0) Then
      Begin
         If (rson^[p] <> NODENIL) Then
            p := rson^[p]
         Else
         Begin
            rson^[p] := R;
            dad^[R] := p;
            Exit;
         End;
      End
      Else
      Begin
         If (lson^[p] <> NODENIL) Then
            p := lson^[p]
         Else
         Begin
            lson^[p] := R;
            dad^[R] := p;
            Exit;
         End;
      End;

      i := 0;
      cmp := 0;

      While (i < lookahead) And (cmp = 0) Do
      Begin
         inc(i);
         cmp := key^[i] - Text_Buf^[p + i];
      End;

      If (i > THRESHOLD) Then
      Begin
         tmp := Pred((R - p) And Pred(OUTBUFSIZE));

         If (i > match_length) Then
         Begin
            match_position := tmp;
            match_length := i;
         End;

         If (match_length < lookahead) And (i = match_length) Then
         Begin
            C := tmp;
            If (C < match_position) Then
               match_position := C;
         End;
      End;
   End;                                 (* while true do *)

   dad^[R] := dad^[p];
   lson^[R] := lson^[p];
   rson^[R] := rson^[p];
   dad^[lson^[p]] := R;
   dad^[rson^[p]] := R;

   If (rson^[dad^[p]] = p) Then
      rson^[dad^[p]] := R
   Else
      lson^[dad^[p]] := R;

   dad^[p] := NODENIL;                  (* remove p *)

End;
//-------------------------------------------------------------

Procedure DeleteNode(p: smallint);      (* Deleting node from the tree *)
Var
   q: smallint;
Begin
   If (dad^[p] = NODENIL) Then
      Exit;                             (* unregistered *)

   If (rson^[p] = NODENIL) Then
      q := lson^[p]
   Else
      If (lson^[p] = NODENIL) Then
         q := rson^[p]
      Else
      Begin
         q := lson^[p];

         If (rson^[q] <> NODENIL) Then
         Begin
            Repeat
               q := rson^[q];
            Until (rson^[q] = NODENIL);
            rson^[dad^[q]] := lson^[q];
            dad^[lson^[q]] := dad^[q];
            lson^[q] := lson^[p];
            dad^[lson^[p]] := q;
         End;

         rson^[q] := rson^[p];
         dad^[rson^[p]] := q;
      End;

   dad^[q] := dad^[p];

   If (rson^[dad^[p]] = p) Then
      rson^[dad^[p]] := q
   Else
      lson^[dad^[p]] := q;

   dad^[p] := NODENIL;
End;
//-------------------------------------------------------------
(* initialize freq tree *)

Procedure StartHuff;
Var
   i, j: smallint;
Begin

   For i := 0 To Pred(N_Char) Do
   Begin
      freq^[i] := 1;
      son^[i] := i + t;
      Parent^[i + t] := i;
   End;

   i := 0;
   j := N_Char;

   While (j <= R) Do
   Begin
      freq^[j] := freq^[i] + freq^[i + 1];
      son^[j] := i;
      Parent^[i] := j;
      Parent^[i + 1] := j;
      inc(i, 2);
      inc(j);
   End;

   freq^[t] := $FFFF;
   Parent^[R] := 0;
End;
//-------------------------------------------------------------
(* reConstruct freq tree *)

Procedure reConst;
Var
   i, j, k, tmp: smallint;
   f, l: word;
Begin

   (* halven cumulative freq for leaf nodes *)
   j := 0;
   For i := 0 To Pred(t) Do
   Begin
      If (son^[i] >= t) Then
      Begin
         freq^[j] := SUCC(freq^[i]) Div 2; (* @@ Bug Fix MOD -> div @@ *)
         son^[j] := son^[i];
         inc(j);
      End;
   End;

   (* make a tree : first, connect children nodes *)
   i := 0;
   j := N_Char;

   While (j < t) Do
   Begin
      k := SUCC(i);
      f := freq^[i] + freq^[k];
      freq^[j] := f;
      k := Pred(j);
      While f < freq^[k] Do
         Dec(k);
      inc(k);
      l := (j - k) Shl 1;
      tmp := SUCC(k);
      CopyMem(@freq^[k], @freq^[tmp], l);
      freq^[k] := f;
      CopyMem(@son^[k], @son^[tmp], l);
      son^[k] := i;
      inc(i, 2);
      inc(j);
   End;

   (* connect Parent nodes *)
   For i := 0 To Pred(t) Do
   Begin
      k := son^[i];
      If (k >= t) Then
         Parent^[k] := i
      Else
      Begin
         Parent^[k] := i;
         Parent^[SUCC(k)] := i;
      End;
   End;
End;
//-------------------------------------------------------------
(* update freq tree *)

Procedure update(C: smallint);
Var
   i, j, l: smallint;
   k: word;                             //SmallInt (* 7/28/98 *)
Begin
   If (freq^[R] = MAX_FREQ) Then
      reConst;

   C := Parent^[C + t];

   Repeat
      inc(freq^[C]);
      k := freq^[C];
      (* swap nodes to keep the 	*)
      l := SUCC(C);                     (*	tree freq-ordered 		*)
      If (k > freq^[l]) Then
      Begin

         While (k > freq^[l]) Do
            inc(l);

         Dec(l);
         freq^[C] := freq^[l];
         freq^[l] := k;
         i := son^[C];
         Parent^[i] := l;
         If (i < t) Then Parent^[SUCC(i)] := l;
         j := son^[l];
         son^[l] := i;
         Parent^[j] := C;
         If (j < t) Then Parent^[SUCC(j)] := C;
         son^[C] := j;
         C := l;
      End;

      C := Parent^[C];
   Until (C = 0);                       (* repeat it until reaching the root *)
End;
//-------------------------------------------------------------

Procedure InitLZH;
Begin
   Buf := 0;
   PosnR := 1;
   PosnW := 1;
   getbuf := 0;
   getlen := 0;
   putlen := 0;
   putbuf := 0;
   codesize := 0;
   match_position := 0;
   match_length := 0;

   New(lson);
   New(dad);
   New(rson);
   New(Text_Buf);
   New(freq);
   New(Parent);
   New(son);
End;
//-------------------------------------------------------------

Procedure endLZH;
Begin
   dispose(son);
   dispose(Parent);
   dispose(freq);
   dispose(Text_Buf);
   dispose(rson);
   dispose(dad);
   dispose(lson);
End;
//-------------------------------------------------------------

Procedure LzhPack(Sender: TCompBase; Infile, Outfile: TStream32; InStream, OutStream: TStream32);

Const
   (**************************************
    * Tables for encoding upper 6 bits of
    * sliding dictionary Pointer
    **************************************)
   (* encoder table *)
   p_len: Array[0..63] Of Byte =
   ($03, $04, $04, $04, $05, $05, $05, $05,
      $05, $05, $05, $05, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $08, $08, $08, $08, $08, $08, $08, $08,
      $08, $08, $08, $08, $08, $08, $08, $08);

   p_code: Array[0..63] Of Byte =
   ($00, $20, $30, $40, $50, $58, $60, $68,
      $70, $78, $80, $88, $90, $94, $98, $9C,
      $A0, $A4, $A8, $AC, $B0, $B4, $B8, $BC,
      $C0, $C2, $C4, $C6, $C8, $CA, $CC, $CE,
      $D0, $D2, $D4, $D6, $D8, $DA, $DC, $DE,
      $E0, $E2, $E4, $E6, $E8, $EA, $EC, $EE,
      $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7,
      $F8, $F9, $FA, $FB, $FC, $FD, $FE, $FF);
   //-------------------------------------------------------------

   Procedure InitTree;                  (* Initializing tree *)
   Var
      i: smallint;
   Begin
      For i := OUTBUFSIZE + 1 To OUTBUFSIZE + 256 Do
         rson^[i] := NODENIL;           (* root *)
      For i := 0 To OUTBUFSIZE Do
         dad^[i] := NODENIL;            (* node *)
   End;
   //-------------------------------------------------------------

   Procedure Putcode(l: smallint; C: word); (* output c bits *)
   Var
      Got: word;
      b: Byte;
   Begin
      putbuf := putbuf Or (C Shr putlen);
      inc(putlen, l);

      If (putlen >= 8) Then
      Begin
         b := putbuf Shr 8;
         PackPutBlock(Sender, Outfile, b, 1, Got);
         Dec(putlen, 8);

         If (putlen >= 8) Then
         Begin
            b := Lo(putbuf);
            PackPutBlock(Sender, Outfile, b, 1, Got);
            inc(codesize, 2);
            Dec(putlen, 8);
            putbuf := C Shl (l - putlen);
         End
         Else
         Begin
            putbuf := putbuf Shl 8;
            inc(codesize);
         End;
      End;
   End;
   //-------------------------------------------------------------
Var
   code, Len: word;

   Procedure EncodeChar(C: word);
   Var
      i: word;
      j, k: smallint;
   Begin
      i := 0;
      j := 0;
      k := Parent^[C + t];
      (* search connections from leaf
         node to the root *)
      Repeat
         i := i Shr 1;

         (* If node's address is odd, output 1 else output 0 *)
         If Boolean(k And 1) Then inc(i, $8000);
         inc(j);
         k := Parent^[k];
      Until (k = R);

      Putcode(j, i);
      code := i;
      Len := j;
      update(C);
   End;
   //-------------------------------------------------------------

   Procedure EncodePosition(C: word);
   Var
      i, j: word;
   Begin
      (* output upper 6 bits With encoding *)
      i := C Shr 6;
      j := p_code[i];
      Putcode(p_len[i], j Shl 8);
      (* output lower 6 bits directly *)
      Putcode(6, (C And $3F) Shl 10);
   End;
   //-------------------------------------------------------------

   Procedure EncodeEnd;
   Var
      b: Byte;
      Got: word;
   Begin
      If Boolean(putlen) Then
      Begin
         b := Lo(putbuf Shr 8);
         PackPutBlock(Sender, Outfile, b, 1, Got);
         inc(codesize);
      End;
   End;
   //-------------------------------------------------------------
Var
   ct: Byte;
   Got: word;
   i, len1, R, s, last_match_length: smallint;
Begin

   InitLZH();
   fSize := Sender.Bytes_To_Go;

   SRCBuf := Nil; New(SRCBuf);
   DSTBuf := Nil; New(DSTBuf);
   Try
      StartHuff();
      InitTree();
      s := 0;
      R := OUTBUFSIZE - lookahead;
      FillChar(Text_Buf^[0], R, ' ');
      len1 := 0;
      Got := 1;

      With Sender Do
         While (len1 < lookahead) And (Got <> 0) Do
         Begin
            PackGetBlock(Sender, Infile, Nil, ct, 1, Got);
            If Got <> 0 Then
            Begin
               Text_Buf^[R + len1] := ct;
               inc(len1);
            End;
         End;

      //TextSize := len1;

      For i := 1 To lookahead Do
         InsertNode(R - i);

      InsertNode(R);

      Repeat
         If (match_length > len1) Then
            match_length := len1;

         If (match_length <= THRESHOLD) Then
         Begin
            match_length := 1;
            EncodeChar(Text_Buf^[R]);
         End
         Else
         Begin
            EncodeChar(255 - THRESHOLD + match_length);
            EncodePosition(match_position);
         End;

         last_match_length := match_length;
         i := 0;
         Got := 1;

         With Sender Do
            While (i < last_match_length) And (Got <> 0) Do
            Begin
               PackGetBlock(Sender, Infile, Nil, ct, 1, Got);
               If Got <> 0 Then
               Begin
                  DeleteNode(s);
                  Text_Buf^[s] := ct;
                  If (s < Pred(lookahead)) Then
                     Text_Buf^[s + OUTBUFSIZE] := Byte(ct);
                  s := SUCC(s) And Pred(OUTBUFSIZE);
                  R := SUCC(R) And Pred(OUTBUFSIZE);
                  InsertNode(R);
                  inc(i);
               End;
            End;

         //Inc( TextSize, i );

         While (i < last_match_length) Do
         Begin
            inc(i);
            DeleteNode(s);
            s := SUCC(s) And Pred(OUTBUFSIZE);
            R := SUCC(R) And Pred(OUTBUFSIZE);
            Dec(len1);
            If Boolean(len1) Then InsertNode(R);
         End;
      Until (len1 <= 0);

      EncodeEnd();
   Finally
      Sender.CompressWriteBlock(Outfile, DSTBuf^, False, 0, PosnW - 1, dtData );
      endLZH();
      If SRCBuf <> Nil Then dispose(SRCBuf);
      If DSTBuf <> Nil Then dispose(DSTBuf);
   End;
End;

//-------------------------------------------------------------

Procedure LZHUnpack(Sender: TUnBASE; Infile: TStream32; Var Outfile: TStream32; InStream: TStream32; IR: TInflateRec);
Const
   (**************************************
    * Tables for decoding upper 6 bits of
    * sliding dictionary Pointer
    **************************************)
     { decoder table }
   d_code: Array[0..255] Of Byte =
   ($00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00,
      $01, $01, $01, $01, $01, $01, $01, $01,
      $01, $01, $01, $01, $01, $01, $01, $01,
      $02, $02, $02, $02, $02, $02, $02, $02,
      $02, $02, $02, $02, $02, $02, $02, $02,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $08, $08, $08, $08, $08, $08, $08, $08,
      $09, $09, $09, $09, $09, $09, $09, $09,
      $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A,
      $0B, $0B, $0B, $0B, $0B, $0B, $0B, $0B,
      $0C, $0C, $0C, $0C, $0D, $0D, $0D, $0D,
      $0E, $0E, $0E, $0E, $0F, $0F, $0F, $0F,
      $10, $10, $10, $10, $11, $11, $11, $11,
      $12, $12, $12, $12, $13, $13, $13, $13,
      $14, $14, $14, $14, $15, $15, $15, $15,
      $16, $16, $16, $16, $17, $17, $17, $17,
      $18, $18, $19, $19, $1A, $1A, $1B, $1B,
      $1C, $1C, $1D, $1D, $1E, $1E, $1F, $1F,
      $20, $20, $21, $21, $22, $22, $23, $23,
      $24, $24, $25, $25, $26, $26, $27, $27,
      $28, $28, $29, $29, $2A, $2A, $2B, $2B,
      $2C, $2C, $2D, $2D, $2E, $2E, $2F, $2F,
      $30, $31, $32, $33, $34, $35, $36, $37,
      $38, $39, $3A, $3B, $3C, $3D, $3E, $3F);

   d_len: Array[0..255] Of Byte =
   ($03, $03, $03, $03, $03, $03, $03, $03,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $03, $03, $03, $03, $03, $03, $03, $03,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $04, $04, $04, $04, $04, $04, $04, $04,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $05, $05, $05, $05, $05, $05, $05, $05,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $06, $06, $06, $06, $06, $06, $06, $06,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $07, $07, $07, $07, $07, $07, $07, $07,
      $08, $08, $08, $08, $08, $08, $08, $08,
      $08, $08, $08, $08, $08, $08, $08, $08);
   //-------------------------------------------------------------

   Function GetBit: smallint;
   Var
      i: Byte;
      I2: smallint;
      Wresult: word;
   Begin
      While (getlen <= 8) Do
      Begin
         UnpackGetBlock(Sender, Infile, i, 1, Wresult);

         If Wresult = 1 Then
            I2 := i
         Else
            I2 := 0;
         getbuf := getbuf Or (I2 Shl (8 - getlen));
         inc(getlen, 8);
      End;
      I2 := getbuf;
      getbuf := getbuf Shl 1;
      Dec(getlen);
      Result := smallint((I2 < 0));
   End;
   //-------------------------------------------------------------

   Function DecodePosition: word;

      Function GetByte: smallint;
      Var
         j: Byte;
         i, Wresult: word;
      Begin
         While (getlen <= 8) Do
         Begin
            UnpackGetBlock(Sender, Infile, j, 1, Wresult);

            If Wresult = 1 Then
               i := j
            Else
               i := 0;

            getbuf := getbuf Or (i Shl (8 - getlen));
            inc(getlen, 8);
         End;
         i := getbuf;
         getbuf := getbuf Shl 8;
         Dec(getlen, 8);
         Result := smallint(i Shr 8);
      End;
   Var
      i, j, C: word;
   Begin
      (* decode upper 6 bits from given table *)
      i := GetByte;
      C := word(d_code[i]) Shl 6;
      j := d_len[i];

      (* input lower 6 bits directly *)
      Dec(j, 2);
      While j > 0 Do
      Begin
         Dec(j);
         i := (i Shl 1) Or GetBit;
      End;
      Result := C Or (i And $3F);
   End;
   //-------------------------------------------------------------

   Function DecodeChar: word;
   Var
      C: word;
   Begin
      C := son[R];
      {-start searching tree from the root to leaves.
       choose node #( son[ ] ) IF input bit = 0
       else choose #( son[ ]+1 ) ( input bit = 1 )}
      While C < t Do
         C := son[C + GetBit];
      Dec(C, t);
      update(C);
      Result := C
   End;
   //-------------------------------------------------------------

Var
   c2: Byte;
   Put: word;
   size: u_long;
   C, i, j, k, R: smallint;
Begin

   Try

      InitLZH;

      //Sender.aWrite := 0;
      Sender.InflateRec := IR;
      FileProgressPos := Sender.InflateRec.UnpackedSize;

      SRCBuf := Nil;
      New(SRCBuf);
      DSTBuf := Nil;
      New(DSTBuf);
      size := 0;
      Try
         StartHuff();

         R := OUTBUFSIZE - lookahead;
         FillChar(Text_Buf^[0], R, ' ');

         While size < Sender.InflateRec.UnpackedSize Do
         Begin
            C := DecodeChar;
            If (C < 256) Then
            Begin
               c2 := Lo(C);
               UnpackPutBlock(Sender, Outfile, c2, 1, Put);
               Text_Buf^[R] := C;
               inc(R);
               R := R And Pred(OUTBUFSIZE);
               inc(size);
            End
            Else
            Begin                       {c >= 256 }
               i := (R - SUCC(DecodePosition)) And Pred(OUTBUFSIZE);
               j := C - 255 + THRESHOLD;

               For k := 0 To Pred(j) Do
               Begin
                  C := Text_Buf^[(i + k) And Pred(OUTBUFSIZE)];
                  c2 := Lo(C);
                  UnpackPutBlock(Sender, Outfile, c2, 1, Put);
                  Text_Buf^[R] := C;
                  inc(R);
                  R := R And Pred(OUTBUFSIZE);
                  inc(size);
               End;
            End;
         End;
      Finally
         With Sender Do
         Begin
            size := ExtractWriteBlock(Outfile, DSTBuf^, False, 16, PosnW - 1, dtData);
            ProgressPosition := ProgressPosition - size;
            Dec(FileProgressPos, size);
            doBranchProgress(InflateRec.UnpackedSize - FileProgressPos,
               InflateRec.UnpackedSize, fTotalUnpackedSize);
         End;
         endLZH;
         If SRCBuf <> Nil Then dispose(SRCBuf);
         If DSTBuf <> Nil Then dispose(DSTBuf);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

End.
