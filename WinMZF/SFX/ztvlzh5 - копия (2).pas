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
Unit ztvlzh5;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   ztvBase,
   ztvGbls,
   ztvCrypt,
   ztvStreams,
   Err_Msgs;

{$I ZipTV.Inc}                          //Declare the compiler defines

Var
   PBIT: Byte;
   DICBIT: Byte;
   DICSIZ: Integer;
   NP: Integer;
   BufferSize: DWord;
   BitSize: Byte;

Function ztvEncode(Sender: TCompBase; Infile, Outfile: TStream32; Encrypted: Boolean): Boolean;

Implementation

//-------------------------------------------------------------

Function ztvEncode(Sender: TCompBase; Infile, Outfile: TStream32; Encrypted: Boolean): Boolean;
Const
   //BUFFERSIZE   = WSIZE;
   CHAR_BIT = 8;
   UCHAR_MAX = 255;
   MAXMATCH = UCHAR_MAX + 1;
   THRESHOLD = 3;
   NC = (UCHAR_MAX + MAXMATCH + 2 - THRESHOLD); // alphabet = {0, 1, 2, ..., NC - 1}
   CBIT = 9;
   CODE_BIT = 16;                       //codeword length
   NT = (CODE_BIT + 3);
   TBIT = 5;                            // smallest Integer such that (1U << TBIT) > NT
   MATCHBIT = 8;                        // bits for MAXMATCH - THRESHOLD
   PERC_FLAG = $8000;
   NUL = 0;
   MAX_HASH_VAL = (3 * {32768} 16384 {DICSIZ} + ({32768} 16384 {DICSIZ} Div 512 + 1) * UCHAR_MAX);
   //#IF NT > NP
   NPT = NT;
   //#ELSE
   //	NPT 		 = NP;
   //#endif

Type
   //#IF MAXMATCH <= (UCHAR_MAX + 1)
   //  pLevel 	= ^aLevel;
   //  aLevel 	= ARRAY[0..DICSIZ + UCHAR_MAX + 1] Of Byte;
   //#ELSE
   pLevel = ^aLevel;
   aLevel = Array[0..0] Of word;
   //#endif
   pPrev = ^aPrev;
   aPrev = Array[0..0] Of word;
   pNext = ^aNext;
   aNext = Array[0..0] Of word;
   PParent = ^aParent;
   aParent = Array[0..0] Of word;
   pPosition = ^aPosition;
   aPosition = Array[0..0] Of smallint;


Var
   level: pLevel;
   Text: PChar;
   ChildCount: PChar;
   Position: pPosition;
   Parent: PParent;
   prev: pPrev;
   Next: pNext;
   inbuf: PChar;
   outbuf: PChar;
   subbitbuf: Byte;
   cPos: word;
   OutputPos: word;
   OutputMask: word;
   Available: word;
   depth: smallint;
   Remainder: Integer;
   Poss: Integer;
   MatchPos: Integer;
   MatchLen: Integer;
   bitcount: Integer;
   n: Integer;
   CompSize: DWord;
   heapsize: Integer;
   fSize: DWord;
   OriginalSize: DWord;
   outbufptr: DWord;
   SortPTR: ^word;
   cLen: Array[0..NC] Of Byte;
   PtLen: Array[0..NPT] Of Byte;
   cFreq: Array[0..2 * NC - 1] Of word;
   pFreq: Array[0..2 * 17 {NP} - 1] Of word;
   TFreq: Array[0..2 * NT - 1] Of word;
   cCode: Array[0..NC] Of word;
   PtCode: Array[0..NPT] Of word;
   left: Array[0..2 * NC - 1] Of word;
   right: Array[0..2 * NC - 1] Of word;
   heap: Array[0..NC + 1] Of Integer;
   LenCount: Array[0..17] Of word;

   //-------------------------------------------------------------

   Function Hash(p: Integer; C: Byte): Integer;
   Begin
      Result := p + C Shl (DICBIT - 9) + DICSIZ * 2;
   End;
   //-------------------------------------------------------------

   Procedure InitializeSlide;
   Var
      i: Integer;
   Begin
      For i := DICSIZ To (DICSIZ + UCHAR_MAX) Do
      Begin
         level[i] := 1;
         { #IF PERCOLATE}
         Position[i] := NUL;            (* sentinel *)
         { #endif}
      End;

      For i := DICSIZ To (DICSIZ * 2) - 1 Do
         Parent[i] := NUL;

      For i := 1 To DICSIZ - 1 Do
         Next[i] := i + 1;

      Next[DICSIZ - 1] := NUL;

      For i := DICSIZ * 2 To MAX_HASH_VAL Do
         Next[i] := NUL;
   End;
   //-------------------------------------------------------------

   Function Child(q: word; C: Byte): word;
      (* q's Child for character c (nil if not found) *)
   Var
      R: word;
   Begin
      R := Next[Hash(q, C)];
      Parent[NUL] := q;                 (* sentinel *)
      While (Parent[R] <> q) Do
         R := Next[R];

      Result := R;
   End;
   //-------------------------------------------------------------

   Procedure MakeChild(q: word {SmallInt 041900}; C: Byte; R: word);
      (* Let r be q's Child for character c. *)
   Var
      H, t: word;
   Begin
      H := Hash(q, C);
      t := Next[H];

      Next[H] := R;
      Next[R] := t;

      prev[t] := R;
      prev[R] := H;

      Parent[R] := q;
      inc(ChildCount[q]);
   End;
   //-------------------------------------------------------------

   Procedure Split(old: word);
   Var
      new1, t: word;
   Begin
      new1 := Available;
      Available := Next[new1];
      ChildCount[new1] := #0;
      t := prev[old];
      prev[new1] := t;
      Next[t] := new1;
      t := Next[old];
      Next[new1] := t;
      prev[t] := new1;
      Parent[new1] := Parent[old];
      level[new1] := MatchLen;
      Position[new1] := Poss;
      MakeChild(new1, Byte(Text[MatchPos + MatchLen]), old);
      MakeChild(new1, Byte(Text[Poss + MatchLen]), Poss);
   End;
   //-------------------------------------------------------------

   Procedure InsertNode;
   Var
      C: Byte;
      q, R, j, t: word;
      t1, t2: ^Byte;
   Begin
      If (MatchLen >= 4) Then
      Begin
         Dec(MatchLen);
         R := (MatchPos + 1) Or DICSIZ;

         While Parent[R] = NUL Do
            R := Next[R];

         q := Parent[R];

         While (level[q] >= MatchLen) Do
         Begin
            R := q;
            q := Parent[q];
         End;

         //#IF PERCOLATE
         t := q;
         While (Position[t] < 0) Do
         Begin
            Position[t] := Poss;
            t := Parent[t];
         End;
         If (t < DICSIZ) Then
            Position[t] := Poss Or PERC_FLAG;
         //#ELSE
         //	t = q;
         //	WHILE (t < DICSIZ) DO
         //   BEGIN
         //		Position[t] := Poss;
         //    t := Parent[t];
         //	END;
         //#endif
      End
      Else
      Begin
         q := Byte(Text[Poss]) + DICSIZ;
         C := Byte(Text[Poss + 1]);

         R := Child(q, C);
         If (R = NUL) Then
         Begin
            MakeChild(q, C, Poss);
            MatchLen := 1;
            Exit;
         End;

         MatchLen := 2;
      End;

      Repeat
         If (R >= DICSIZ) Then
         Begin
            j := MAXMATCH;
            MatchPos := R;
         End
         Else
         Begin
            j := level[R];

            (* PERC_FLAG - 1 is equal to ones complement *)
            (* of ~PERC_FLAG.  Must use ones complement  *)
            (* of PERC_FLAG here.                        *)
            MatchPos := Position[R] And (PERC_FLAG - 1);
         End;

         If (MatchPos >= Poss) Then
            Dec(MatchPos, DICSIZ);

         t1 := @Text[Poss + MatchLen];
         t2 := @Text[MatchPos + MatchLen];

         While (MatchLen < j) Do
         Begin
            If (t1^ <> t2^) Then
            Begin
               Split(R);
               Exit;
            End;
            inc(MatchLen);
            inc(t1);
            inc(t2);
         End;

         If (MatchLen >= MAXMATCH) Then
            break;

         Position[R] := Poss;
         q := R;

         R := Child(q, t1^);
         If (R = NUL) Then
         Begin
            MakeChild(q, t1^, Poss);
            Exit;
         End;

         inc(MatchLen);
      Until 1 <> 1;

      t := prev[R];
      prev[Poss] := t;
      Next[t] := Poss;
      t := Next[R];
      Next[Poss] := t;
      prev[t] := Poss;
      Parent[Poss] := q;
      Parent[R] := NUL;
      Next[R] := Poss;                  (* special use of Next[] *)
   End;
   //-------------------------------------------------------------

   Procedure DeleteNode;
   Var
      //#IF PERCOLATE
      q, R, s, t, u: word;
      //#ELSE
      //	r, s, t, u: SmallInt;
      //#endif
   Begin
      If (Parent[Poss] = NUL) Then
         Exit;

      R := prev[Poss];
      s := Next[Poss];
      Next[R] := s;
      prev[s] := R;
      R := Parent[Poss];
      Parent[Poss] := NUL;

      Dec(ChildCount[R]);
      If (R >= DICSIZ) Or (Byte(ChildCount[R]) > 1) Then
         Exit;

      //#IF PERCOLATE
         (* PERC_FLAG - 1 is equal to ones complement *)
         (* of ~PERC_FLAG.  Must use ones complement  *)
         (* of PERC_FLAG here.                        *)
      t := Position[R] And (PERC_FLAG - 1);
      //#ELSE
      //	t := Position[r];
      //#endif

      If (t >= Poss) Then
         Dec(t, DICSIZ);

      //#IF PERCOLATE
      s := t;
      q := Parent[R];
      u := Position[q];
      While ((u And PERC_FLAG) <> 0) Do
      Begin
         (* PERC_FLAG - 1 is equal to ones complement *)
         (* of ~PERC_FLAG.  Must use ones complement  *)
         (* of PERC_FLAG here.                        *)
         u := u And (PERC_FLAG - 1);
         If (u >= Poss) Then Dec(u, DICSIZ);
         If (u > s) Then s := u;
         Position[q] := (s Or DICSIZ);
         q := Parent[q];
         u := Position[q];
      End;

      If (q < DICSIZ) Then
      Begin
         If (u >= Poss) Then
            Dec(u, DICSIZ);
         If (u > s) Then
            s := u;
         Position[q] := s Or DICSIZ Or PERC_FLAG;
      End;
      //#endif

      s := Child(R, Byte(Text[t + level[R]]));
      t := prev[s];
      u := Next[s];
      Next[t] := u;
      prev[u] := t;
      t := prev[R];
      Next[t] := s;
      prev[s] := t;
      t := Next[R];
      prev[t] := s;
      Next[s] := t;
      Parent[s] := Parent[R];
      Parent[R] := NUL;
      Next[R] := Available;
      Available := R;
   End;
   //-------------------------------------------------------------

   Function ReadFile(Var Buffer; Len: longint): longint;
   Begin
      With Sender Do
      Begin
         Result := ReadBlock(Infile, Nil, Buffer, False, BitSize, Len, dtData);
         Dec(Bytes_To_Go, Result);
         ProgressPosition := ProgressPosition - Result;
         doBranchProgress(fSize - Bytes_To_Go, fSize, fTotalUnpackedSize);
      End;
      OriginalSize := OriginalSize + DWord(Result);
   End;
   //-------------------------------------------------------------

   Procedure GetNextMatch;
   Var
      n: Integer;
   Begin
      Dec(Remainder);
      inc(Poss);
      If (Poss = DICSIZ * 2) Then
      Begin
         //CopyMem( Text[DICSIZ], Text[0], DICSIZ + MAXMATCH );
         Move(Text[DICSIZ], Text[0], DICSIZ + MAXMATCH);
         n := ReadFile(Text[DICSIZ + MAXMATCH], DICSIZ);
         inc(Remainder, n);
         Poss := DICSIZ;
      End;
      DeleteNode();
      InsertNode();
   End;
   //-------------------------------------------------------------

   Procedure DownHeap(i: Integer; freq: Array Of word);
   Var
      j, k: Integer;
   Begin
      k := heap[i];
      j := 2 * i;
      While (j <= heapsize) Do
      Begin
         If (j < heapsize) And (freq[heap[j]] > freq[heap[j + 1]]) Then
            inc(j);

         If (freq[k] <= freq[heap[j]]) Then break;

         heap[i] := heap[j];
         i := j;
         j := 2 * i;
      End;
      heap[i] := k;
   End;
   //-------------------------------------------------------------

   Procedure CountLen(i: Integer);
   Begin
      If (i < n) Then
      Begin
         If depth < 16 Then
            inc(LenCount[depth])
         Else
            inc(LenCount[16]);
      End
      Else
      Begin
         inc(depth);
         CountLen(left[i]);
         CountLen(right[i]);
         Dec(depth);
      End;
   End;
   //-------------------------------------------------------------

   Procedure MakeLen(Root: Integer; Var Len: Array Of Byte);
   Var
      i, k: Integer;
      cum: Cardinal;
   Begin
      i := 0;
      While i <= 16 Do
      Begin
         LenCount[i] := 0;
         inc(i);
      End;

      CountLen(Root);
      cum := 0;

      i := 16;
      While i > 0 Do
      Begin
         cum := cum + LenCount[i] Shl (16 - i);
         Dec(i);
      End;

      While (cum <> 1 Shl 16) Do
      Begin
         Dec(LenCount[16]);
         i := 15;
         While i > 0 Do
         Begin
            If LenCount[i] <> 0 Then
            Begin
               Dec(LenCount[i]);
               LenCount[i + 1] := LenCount[i + 1] + 2;
               break;
            End;
            Dec(i);
         End;
         Dec(cum);
      End;

      For i := 16 Downto 1 Do
      Begin
         k := LenCount[i];
         Dec(k);

         While (k >= 0) Do
         Begin
            Len[SortPTR^] := i;
            inc(SortPTR);
            Dec(k);
         End;
      End;
   End;
   //-------------------------------------------------------------

   Procedure MakeCode(n: Integer; Len: Array Of Byte; Var code: Array Of word);
   Var
      i: Integer;
      START: Array[0..18] Of word;
   Begin
      START[1] := 0;
      For i := 1 To 16 Do
         START[i + 1] := (START[i] + LenCount[i]) Shl 1;

      For i := 0 To n - 1 Do
      Begin
         code[i] := START[Len[i]];
         inc(START[Len[i]]);
      End;
   End;
   //-------------------------------------------------------------

   Function MakeTree(nparm: Integer; Var FreqParm: Array Of word;
      Var LenParm: Array Of Byte; Var CodeParm: Array Of word): Integer;
   Var
      i, j, k, Available: Integer;
   Begin

      n := nparm;
      //freq := @FreqParm;
      //len1 := @LenParm;
      Available := n;
      heapsize := 0;
      heap[1] := 0;

      For i := 0 To n - 1 Do
      Begin
         LenParm[i] {len1[i]} := 0;
         If FreqParm[i] {freq[i]} <> 0 Then
         Begin
            inc(heapsize);
            heap[heapsize] := i;
         End;
      End;

      If (heapsize < 2) Then
      Begin
         CodeParm[heap[1]] := 0;
         Result := heap[1];
         Exit;
      End;

      i := heapsize Div 2;
      While i >= 1 Do
      Begin
         DownHeap(i, FreqParm);         (* make priority queue *)
         Dec(i);
      End;

      SortPTR := @CodeParm;

      Repeat                            (* while queue has at least two entries *)
         i := heap[1];                  (* take out least-freq entry *)
         If (i < n) Then
         Begin
            SortPTR^ := i;
            inc(SortPTR);
         End;
         heap[1] := heap[heapsize];
         Dec(heapsize);

         DownHeap(1, FreqParm);
         j := heap[1];                  (* Next least-freq entry *)
         If (j < n) Then
         Begin
            SortPTR^ := j;
            inc(SortPTR);
         End;

         k := Available;                (* generate new node *)
         inc(Available);

         FreqParm[k] {freq[k]} := FreqParm[i] {freq[i]} + FreqParm[j] {freq[j]};

         heap[1] := k;
         DownHeap(1, FreqParm);         (* put into queue *)
         left[k] := i;
         right[k] := j;
      Until heapsize <= 1;

      SortPTR := @CodeParm;
      MakeLen(k, LenParm);
      MakeCode(nparm, LenParm, CodeParm);
      Result := k;                      (* return Root *)
   End;
   //-------------------------------------------------------------

   Procedure count_t_freq;
   Var
      i, k, n, Count: Integer;
   Begin
      For i := 0 To NT - 1 Do
         TFreq[i] := 0;

      n := NC;

      While (n > 0) And (cLen[n - 1] = 0) Do
         Dec(n);

      i := 0;

      While (i < n) Do
      Begin

         k := cLen[i];
         inc(i);

         If (k = 0) Then
         Begin

            Count := 1;
            While (i < n) And (cLen[i] = 0) Do
            Begin
               inc(i);
               inc(Count);
            End;

            If (Count <= 2) Then
               inc(TFreq[0], Count)
            Else
               If (Count <= 18) Then
                  inc(TFreq[1])
               Else
                  If (Count = 19) Then
                  Begin
                     inc(TFreq[0]);
                     inc(TFreq[1]);
                  End
                  Else
                     inc(TFreq[2]);
         End
         Else
            inc(TFreq[k + 2]);
      End;
   End;
   //-------------------------------------------------------------
   (* Write rightmost n bits of x *)

   Procedure PutBits(n: Integer; x: word);
   Begin
      If (n < bitcount) Then
      Begin
         Dec(bitcount, n);
         subbitbuf := subbitbuf Or (x Shl bitcount);
      End
      Else
      Begin
         If (CompSize < OriginalSize) Then
         Begin
            Dec(n, bitcount);

            outbuf[outbufptr] := char(subbitbuf Or (x Shr n));

            If outbufptr >= BufferSize Then
            Begin
               (* Add one to OutBufPtr (option base 0) *)
               With Sender Do
                  CompressWriteBlock(Outfile, outbuf^, Encrypted,
                  	0, outbufptr + 1, dtData);
               outbufptr := 0;
            End
            Else
               inc(outbufptr);

            inc(CompSize);
         End
         Else
            Sender.Unpackable := True;  {1}

         If (n < CHAR_BIT) Then
         Begin
            bitcount := CHAR_BIT - n;
            subbitbuf := x Shl bitcount;
         End
         Else
         Begin
            If (CompSize < OriginalSize) Then
            Begin
               outbuf[outbufptr] := char(x Shr (n - CHAR_BIT));

               If outbufptr >= BufferSize Then
               Begin
                  (* Add one to OutBufPtr (option base 0) *)
                  With Sender Do
                     CompressWriteBlock(Outfile, outbuf^, Encrypted,
                     	0, outbufptr + 1, dtData);
                  outbufptr := 0;
               End
               Else
                  inc(outbufptr);

               inc(CompSize);
            End
            Else
               Sender.Unpackable := True;

            bitcount := 2 * CHAR_BIT - n;
            subbitbuf := x Shl bitcount;
         End;
      End;
   End;
   //-------------------------------------------------------------

   Procedure write_pt_len(n, nbit, i_special: smallint);
   Var
      i, k: smallint;
   Begin
      While (n > 0) And (PtLen[n - 1] = 0) Do
         Dec(n);

      PutBits(nbit, n);

      i := 0;

      While (i < n) Do
      Begin
         k := PtLen[i];
         inc(i);

         If (k <= 6) Then
            PutBits(3, k)
         Else
            PutBits(k - 3, (1 Shl (k - 3)) - 2);

         If (i = i_special) Then
         Begin
            While (i < 6) And (PtLen[i] = 0) Do
               inc(i);

            PutBits(2, (i - 3) And 3);
         End;
      End;
   End;
   //-------------------------------------------------------------

   Procedure write_c_len;
   Var
      i, k, n, Count: smallint;
   Begin
      n := NC;
      While (n > 0) And (cLen[n - 1] = 0) Do
         Dec(n);

      PutBits(CBIT, n);
      i := 0;

      While (i < n) Do
      Begin

         k := cLen[i];
         inc(i);

         If (k = 0) Then
         Begin

            Count := 1;
            While (i < n) And (cLen[i] = 0) Do
            Begin
               inc(i);
               inc(Count);
            End;

            If (Count <= 2) Then
            Begin
               For k := 0 To Count - 1 Do
                  PutBits(PtLen[0], PtCode[0])
            End
            Else
               If (Count <= 18) Then
               Begin
                  PutBits(PtLen[1], PtCode[1]);
                  PutBits(4, Count - 3);
               End
               Else
                  If (Count = 19) Then
                  Begin
                     PutBits(PtLen[0], PtCode[0]);
                     PutBits(PtLen[1], PtCode[1]);
                     PutBits(4, 15);
                  End
                  Else
                  Begin
                     PutBits(PtLen[2], PtCode[2]);
                     PutBits(CBIT, Count - 20);
                  End;
         End
         Else
            PutBits(PtLen[k + 2], PtCode[k + 2]);
      End;
   End;
   //-------------------------------------------------------------

   Procedure EncodeChar(C: Integer);
   Begin
      PutBits(cLen[C], cCode[C]);
   End;
   //-------------------------------------------------------------

   Procedure SendBlock;

      Procedure encode_p(p: word);
      Var
         C, q: word;
      Begin
         C := 0;
         q := p;

         While (q <> 0) Do
         Begin
            q := q Shr 1;
            inc(C);
         End;

         PutBits(PtLen[C], PtCode[C]);

         If (C > 1) Then
            PutBits(C - 1, p And ($FFFF Shr (17 - C)));
      End;
   Var
      flags, i, k, Root,
         Poss, size: word;
   Begin
      Root := MakeTree(NC, cFreq, cLen, cCode);
      size := cFreq[Root];
      PutBits(16, size);
      If (Root >= NC) Then
      Begin
         count_t_freq;

         Root := MakeTree(NT, TFreq, PtLen, PtCode);
         If (Root >= NT) Then
            write_pt_len(NT, TBIT, 3)
         Else
         Begin
            PutBits(TBIT, 0);
            PutBits(TBIT, Root);
         End;

         write_c_len;
      End
      Else
      Begin
         PutBits(TBIT, 0);
         PutBits(TBIT, 0);
         PutBits(CBIT, 0);
         PutBits(CBIT, Root);
      End;

      Root := MakeTree(NP, pFreq, PtLen, PtCode);
      If (Root >= NP) Then
         write_pt_len(NP, PBIT, -1)
      Else
      Begin
         PutBits(PBIT, 0);
         PutBits(PBIT, Root);
      End;

      Poss := 0;

      flags := 0;
      For i := 0 To size - 1 Do
      Begin
         If (i Mod CHAR_BIT) = 0 Then
         Begin
            flags := word(inbuf[Poss]);
            inc(Poss);
         End
         Else
            flags := flags Shl 1;

         If (flags And (1 Shl (CHAR_BIT - 1))) <> 0 Then
         Begin
            EncodeChar(Byte(inbuf[Poss]) + (1 Shl CHAR_BIT));
            inc(Poss);
            k := Byte(inbuf[Poss]) Shl CHAR_BIT;
            inc(Poss);
            inc(k, Byte(inbuf[Poss]));
            inc(Poss);
            encode_p(k);
         End
         Else
         Begin
            EncodeChar(Byte(inbuf[Poss]));
            inc(Poss);
         End;
         If Sender.Unpackable Then Exit;
      End;

      For i := 0 To NC - 1 Do
         cFreq[i] := 0;

      For i := 0 To NP - 1 Do
         pFreq[i] := 0;
   End;
   //-------------------------------------------------------------

   Procedure HuffEncodeEnd;
   Begin
      If Not Sender.Unpackable Then
      Begin
         SendBlock();
         If Sender.Unpackable Then Exit;
         (* flush remaining bits *)
         PutBits(CHAR_BIT - 1, 0);
      End;
   End;
   //-------------------------------------------------------------

   Procedure InitializePutBits;
   Begin
      bitcount := CHAR_BIT;
      subbitbuf := 0;
   End;
   //-------------------------------------------------------------

   Procedure HuffEncodeStart;
   Var
      i: Integer;
   Begin
      inbuf[0] := #0;
      CompSize := 0;
      OriginalSize := 0;
      outbufptr := 0;
      depth := 0;
      cPos := 0;
      OutputPos := 0;
      OutputMask := 0;
      MatchLen := 0;
      Poss := DICSIZ;
      Remainder := 0;
      MatchPos := 0;
      Available := 1;
      Sender.Unpackable := False;

      For i := 0 To NC - 1 Do
         cFreq[i] := 0;                 //fillchar(cFreq[0], (NC-1) * sizeof(cFreq[0]), #0);
      For i := 0 To NP - 1 Do
         pFreq[i] := 0;                 //fillchar(pFreq[0], (NC-1) * sizeof(pFreq[0]), #0);

      OutputMask := 0;
      OutputPos := 0;

      InitializePutBits();
   End;
   //-------------------------------------------------------------

   Procedure OutPut(C, p: word);
   Begin
      OutputMask := OutputMask Shr 1;
      If (OutputMask = 0) Then
      Begin
         OutputMask := 1 Shl (CHAR_BIT - 1);
         If (OutputPos >= BufferSize - 3 * CHAR_BIT) Then
         Begin
            SendBlock();
            If Sender.Unpackable Then Exit;
            OutputPos := 0;
         End;
         cPos := OutputPos;
         inc(OutputPos);
         inbuf[cPos] := #0;
      End;

      inbuf[OutputPos] := char(C);
      inc(OutputPos);
      inc(cFreq[C]);

      If (C >= (1 Shl CHAR_BIT)) Then
      Begin
         inbuf[cPos] := char(Byte(inbuf[cPos]) Or OutputMask);
         inbuf[OutputPos] := char(p Shr CHAR_BIT);
         inc(OutputPos);
         inbuf[OutputPos] := char(p);
         inc(OutputPos);

         (* this loop results are the same as Byte(c) ????? *)
         C := 0;
         While (p <> 0) Do
         Begin
            p := p Shr 1;
            inc(C);
         End;

         inc(pFreq[C]);

      End;
   End;
   //-------------------------------------------------------------

Var
   LastMatchlen,
      LastMatchPos: Integer;
Begin

   fSize := Sender.Bytes_To_Go;

   Try
      GetMem(prev, ((DICSIZ + 1) * 2) * SizeOf(word));
      GetMem(Next, (MAX_HASH_VAL + 1) * SizeOf(word));
      GetMem(level, (DICSIZ + UCHAR_MAX + 1) * SizeOf(word));
      GetMem(Parent, ((DICSIZ + 1) * 2) * SizeOf(word));
      GetMem(Position, (DICSIZ + UCHAR_MAX + 1) * SizeOf(smallint));
      GetMem(Text, (DICSIZ * 2 + MAXMATCH) + 1);
      GetMem(inbuf, BufferSize + 1);
      GetMem(outbuf, BufferSize + 1);
      GetMem(ChildCount, DICSIZ + UCHAR_MAX + 1);

      Try
         InitializeSlide();
         HuffEncodeStart();
         Remainder := ReadFile(Text[DICSIZ], DICSIZ + MAXMATCH);
         InsertNode();

         If (MatchLen > Remainder) Then
            MatchLen := Remainder;

         While (Remainder > 0) And (Not Sender.Unpackable) And (Not Sender.Cancel) Do
         Begin
            LastMatchlen := MatchLen;
            LastMatchPos := MatchPos;
            GetNextMatch();

            If (MatchLen > Remainder) Then
               MatchLen := Remainder;

            If (MatchLen > LastMatchlen) Or (LastMatchlen < THRESHOLD) Then
            Begin
               OutPut(Byte(Text[Poss - 1]), 0);

               If OutputPos > OriginalSize Then
               Begin
                  Sender.Unpackable := True;
                  break;
               End;

            End
            Else
            Begin
               OutPut(LastMatchlen + (UCHAR_MAX + 1 - THRESHOLD),
                  (Poss - LastMatchPos - 2) And (DICSIZ - 1));

               Dec(LastMatchlen);
               While (LastMatchlen > 0) Do
               Begin
                  GetNextMatch();
                  Dec(LastMatchlen);

                  If OutputPos > OriginalSize Then
                  Begin
                     Sender.Unpackable := True;
                     break;
                  End;
               End;

               If (MatchLen > Remainder) Then
                  MatchLen := Remainder;
            End;
         End;

         HuffEncodeEnd();

         With Sender Do
            If (Not Unpackable) Then
               CompressWriteBlock(Outfile, outbuf^, Encrypted, 0, outbufptr, dtData);
      Finally
      	Result := (Not Sender.Cancel);
         FreeMem(ChildCount);
         FreeMem(outbuf);
         FreeMem(inbuf);
         FreeMem(Text);
         FreeMem(Position);
         FreeMem(Parent);
         FreeMem(level);
         FreeMem(Next);
         FreeMem(prev);
      End;
   Except
      //on E: Exception DO ShowMessage(e.message);
      Result := False;
   End;
End;
//-------------------------------------------------------------

End.
