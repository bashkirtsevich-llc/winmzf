(**********************************************************************

 Copyright 1998-2003,  Microchip Data Systems / Carl Bunton

  Under license agreement, this source module may be used only on a
  single computer.

  No partion of this module may be reproduced, copied, revised, edited,
  distributed or transmited via electronic means except in compiled
  application format.

  Web-site:  http://www.ziptv.com
  Email:     custsupt@ziptv.com

**********************************************************************)
Unit ztvgzinflate;

Interface


Uses
   Windows, Classes, ztvBase, ztvStreams;

{$I ZipTV.inc}	//Declare the compiler defines
{$define PKZIP_BUG_WORKAROUND}

Function ExtractFile( UnBase: TUnBASE; Infile: TStream32; Var Outfile: TStream32; IR: TInflateRec ): Boolean;

Implementation

Uses
   ztvGbls, Err_Msgs;

Const
   (* Tables for deflate from PKZIP's appnote.txt. *)
   cplens: Array[0..30] Of Word =      (* Copy lengths for literal codes 257..285 *)
   ( 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35,
      43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0 );
   (* note: see note #13 above about the 258 in this list. *)
   cpdist: Array[0..29] Of Word =      (* Copy offsets for distance codes 0..29 *)
   ( 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257,
      385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289,
      16385, 24577 );
   cplext: Array[0..30] Of Byte =      (* Extra bits for literal codes 257..285 *)
   ( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4,
      4, 4, 5, 5, 5, 5, 0, 99, 99 );              (* 99==invalid *)
   cpdext: Array[0..29] Of Byte =      (* Extra bits for distance codes *)
   ( 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9,
      10, 10, 11, 11, 12, 12, 13, 13 );

   (* AND'ing with mask[ n ] masks the lower n bits *)
   maskr: Array[0..16] Of Word = ( $0000, $0001, $0003, $0007, $000F,
      $001F, $003F, $007F, $00FF, $01FF, $03FF, $07FF, $0FFF,
      $1FFF, $3FFF, $7FFF, $FFFF );

   lbits = 9;
   dbits = 6;
   N_MAX = 288;

Type
   PT = ^Thuft;
   Thuft = Packed Record
   	e, b: Byte {shortint};
      n: Word;
      next: PT;
   End;

   ReleaseMem = Packed Record
   	pq: pointer;
      TableSize: Word;
   End;

   prm = ^ReleaseMem;
   //BufPtr       = ^BufType;
   //BufType      =  ARRAY[ 0..WSIZE ] OF Byte;

Var
   hufts        : Word;
   bb           : LONGINT;                    (* bit buffer ( Static )*)
   bk           : {Byte; //} Word;            (* bits in bit buffer ( Static )*)
   InBuf        : Array[0..WSIZE] Of Byte;    //BufPtr;
   Slide        : Array[0..WSIZE] Of Byte;    //BufPtr;
   InPTR        : Integer;                    (* Index for ZipFile input buffer 			*)
   ZipCount     : Integer;                    (* Count of bytes in ZipFile input buffer 	*)
   WP           : Integer;                    (* Static Global *)
   fixed_tl,
      fixed_td  : PT;                         (* Static Global *)
   fixed_List   : TList;
   EOF          : Boolean;
   SizeOfThuft  : Byte;

//-------------------------------------------------------------
Procedure huft_free;
Var
   i            : Integer;
   arm          : prm;
Begin
   For i := 0 To Pred( fixed_List.Count ) Do
   Begin
      arm := Fixed_List.Items[i];
      FreeMem( arm^.pq, arm^.TableSize );
      FreeMem( arm );
   End;
   Fixed_List.Clear;
End;

Function inflate_block( UnBase: TUnBASE; Infile: TStream32; Var Outfile: TStream32; Var e: Integer ): Short; (* e = last block flag *)
Var
   t: Word;                       (* block type *)
   k: {Byte; //} Word;            (* number of bits in bit buffer *)
   b: LONGINT;                    (* bit buffer *)
   //-------------------------------------------------------------
   Procedure DUMPBITS( n: Byte );
   Begin
      b := b Shr n;
      Dec( k, n );
   End;
   //-------------------------------------------------------------
   Function NEEDBITS( n: Byte ): Boolean;
      Function Get_Compressed( UnBase: TUnBASE; Infile: TStream32 ): Integer;
      Var
         BufSize: Integer;
      Begin
         With UnBase Do
         Begin

            If Cancel Then
            Begin
               Bytes_To_Go := 0;
               ZipCount := 0;
               EOF := True;
               Result := -1;
               Exit;
            End;

            Try
               If Bytes_To_Go <= 0 Then
               Begin
                  ZipCount := 0;
                  Result := -1;
               End Else Begin
                  If InPTR >= ZipCount Then
                  Begin

                     // use the following block, instead of the min function...
                     // the min function fails with files > 4 gig.
                     //BufSize := Min(Bytes_To_Go, WSIZE )
                     If Bytes_To_Go > WSIZE Then
                        BufSize := WSIZE
                     Else
                        BufSize := Bytes_To_Go;

                     With UnBase Do
                        ZipCount :=
                           ReadBlock(
                              Infile,
                              Nil,
                              Inbuf,
                              (InflateRec.BitFlag And 1) = 1,
                              0,
                              BufSize,
                              dtData );

                     InPtr := 0;
                  End;

                  If ZipCount = 0 Then
                  Begin
                     //Bytes_To_Go := 0;
                     Inptr := 0;
                     Result := -1;
                  End Else Begin
                     Result := InBuf[InPTR];
                     Inc( InPTR );
                     Dec( Bytes_To_Go );
                  End;
               End;
            Finally
               EOF := ZipCount = 0;
            End;
         End;
      End;
   Var
      c         : Integer;
   Begin
      Result := True;
      With UnBase Do
         While ( k < n ) Do
         Begin
            c := Get_Compressed( UnBase, Infile );
            If c = -1 {EOF} Then
            Begin
               If Cancel Then Exit;
               If ( Not Cancel ) And ( Not EOF ) Then
                  RaiseError( E_RAISE, Filename, '', '0', E_BADBLOCK )
               Else Begin
                  Result := False;
                  Exit;
               End;
            End;
            b := b Or LONGINT( c ) Shl k;         (* no parens!! *)
            Inc( k, 8 );
         End;
   End;
   //-------------------------------------------------------------

   Function huft_build( Var b: Array Of Word;
      n, s: Word;
      Var d: Array Of Word;
      Var e: Array Of Byte;
      Var t, HF: PT;
      Var m: Integer ): Integer;

   Const
      BMAX          = 16;

   Var
      a         : Word;
      c         : Array[0..BMAX] Of Word;     (* bit length count table *)
      el        : Word;
      f         : Word;
      g         : Integer;                    (* maximum code length *)
      h         : Integer;                    (* table level *)
      i         : Word;                       (* counter, current code / counter *)
      j         : Word;                       (* counter *)
      kk        : Integer;                    (* number of bits in current code *)
      lx        : Array[-1..BMAX + 1] Of Integer;
      p         : ^Word;
      q         : PT;
      r         : Thuft;
      u         : Array[0..BMAX] Of PT;
      v         : Array[0..N_MAX] Of Word;    (* values in order of bit length *)
      w         : Word;
      x         : Array[0..BMAX + 1] Of Word; (* bit offsets, then code stack *)
      xp        : ^Word;
      y         : Integer;
      z         : Word;
      arm       : prm;
   Begin

      (* Generate counts for each bit length *)
      If n > 256 Then                             (* set length of EOB code, if any *)
         el := b[256]
      Else
         el := BMAX;

      ZeroMemory( @c[0], SizeOf( c ) );

      p := @b;
      i := n;

      Repeat
         Inc( c[p^] );                            (* assume all entries <= BMAX *)
         Inc( p );
         Dec( i );
      Until ( i <= 0 );

      (* null input--all zero length codes *)
      If c[0] = n Then
      Begin
         t := Nil;
         m := 0;
         Result := 0;
         Exit;
      End;

      (* Find minimum and maximum length, bound *m by those *)
      For j := 1 To BMAX Do
         If c[j] <> 0 Then
            Break;

      kk := j;                                    (* minimum code length *)

      If ( m < j ) Then m := j;

      For i := BMAX Downto 1 Do
         If c[i] <> 0 Then
            Break;

      g := i;                                     (* maximum code length *)

      If Word( m ) > g Then
         m := Integer( g );

      (* Adjust last length count to fill out codes, if needed *)
      y := 1 Shl j;
      For j := j To Pred( g ) Do
      Begin
         Dec( y, c[j] );
         If y < 0 Then
         Begin
            Result := 2;                          (* bad input: more codes than bits *)
            Exit;
         End;
         y := y Shl 1;
      End;

      Dec( y, c[i] );
      If y < 0 Then
      Begin
         Result := 2;
         Exit;
      End;

      Inc( c[i], y );

      (* Generate starting offsets into the value table for each length *)
      x[1] := 0;
      j := 0;

      p := @c[1];
      xp := @x[2];

      Dec( i );                                   (* note that i = g from above *)
      While ( i > 0 ) Do
      Begin
         Inc( j, p^ );
         xp^ := j;
         Inc( p );
         Inc( xp );
         Dec( i );
      End;

      (* Make a table of values in order of bit lengths *)
      p := @b;
      i := 0;
      Repeat
         j := p^;
         If ( j <> 0 ) Then
         Begin
            v[x[j]] := i;
            Inc( x[j] );
         End;
         Inc( p );
         Inc( i );
      Until i >= n;

      (* Generate the Huffman codes and for each, make the table entries *)
      h := -1;                                    (* no tables yet--level -1 *)
      i := 0;
      lx[-1] := 0;                                (* ditto *)
      p := @v;                                    (* grab values in bit order *)
      q := Nil;                                   (* ditto *)
      t := Nil;
      u[0] := Nil;                                (* just to keep compilers happy *)
      w := 0;                                     (* no bits decoded yet *)
      x[0] := 0;                                  (* first Huffman code is zero *)
      z := 0;                                     (* ditto *)

      (* go through the bit lengths ( kk already is bits in shortest code ) *)
      For kk := kk To g Do
      Begin
         a := c[kk];
         While ( a <> 0 ) Do
         Begin
            Dec( a );

            (* here i is the Huffman code of length kk bits for value *p *)
            (* make tables up to required level *)
            While kk > ( w + lx[h] ) Do
            Begin
               Inc( w, lx[h] );                   (* add bits already decoded *)
               Inc( h );

               (* compute minimum size table less than or equal to *m bits *)
               z := g - w;                        (* upper limit *)
               If z > m Then
                  z := m;

               j := kk - w;
               f := 1 Shl j;
               If f > Succ( a ) Then              (* try a kk-w bit table *)
               Begin                              (* too few codes for kk-w bit table *)
                  Dec( f, Succ( a ) );            (* deduct codes from patterns left *)
                  xp := @c[kk];
                  Inc( j );
                  While ( j < z ) Do              (* try smaller tables up to z bits *)
                  Begin
                     Inc( xp );
                     f := f Shl 1;
                     If f <= xp^ Then
                        Break;                    (* enough codes to use up j bits *)
                     Dec( f, xp^ );               (* else deduct codes from patterns *)
                     Inc( j );
                  End;
               End;

               If ( ( w + j ) > el ) And ( w < el ) Then
                  j := el - w;                    (* make EOB code end at table *)

               z := 1 Shl j;                      (* table entries for j-bit table *)
               lx[h] := j;                        (* set table size in stack *)

               (* allocate and link in new table *)
               New( arm );
               Try
                  arm^.TableSize := Succ( z ) * SizeOfThuft;

                  GetMem( q, arm^.TableSize );
                  If q = Nil Then
                     UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_MEMERR );

                  arm^.pq := q;
               Finally
                  fixed_List.Add( arm );
               End;

               Inc( hufts, Succ( z ) );           (* track memory usage *)

               r.next := HF;
               q^.next := HF;
               Inc( q );
               HF := q;
               u[h] := q;

               If t = Nil Then
                  t := q;

               (* connect to last table, if there is one *)
               If h > 0 Then
               Begin
                  x[h] := i;                      (* save pattern for backing up *)

                  r.b := lx[Pred( h )];           (* bits to dump before this table *)
                  r.e := ( 16 + j );              (* bits in this table *)
                  r.next := q;                    (* pointer to this table *)
                  j := ( i And ( Pred( 1 Shl w ) ) ) Shr ( w - lx[Pred( h )] );

                  (* connect to last table *)
                  (*****************************************************************
                     Use the following line in the debugger to verify the allocated
                     memory boundries with data being inserted.

                     *->   ( LONGINT( u[ h-1 ] )+( j*SizeOfThuft ) ) - LONGINT( u[ h-1 ] )   <-*
                   *****************************************************************)
                  move( r, pointer( LONGINT( u[ h-1 ] ) + ( j*SizeOfThuft ) )^, SizeOfThuft );
                  //CopyMem( pointer( LONGINT( u[Pred( h )] ) + ( j * SizeOfThuft ) ), @r, SizeOfThuft );
               End;
            End;

            (* set up table entry in r *)
            r.b := kk - w;                        //shortint( kk - w );

            (*IF ( LONGINT( addr( p^ ) ) >= LONGINT( addr( v[ n ] ) ) )*)
            If ( LONGINT( p ) >= LONGINT( @v[n] ) ) Then
               r.e := 99                          (* out of values--invalid code *)
            Else
               If ( p^ < s ) Then
               Begin
                  If p^ < 256 Then                (* 256 is end-of-block code *)
                     r.e := 16
                  Else
                     r.e := 15;

                  r.n := p^;                      (* simple code is just the value *)
                  Inc( p );
               End Else Begin
                  r.e := e[p^ - s];               (* non-simple--look up in lists *)
                  r.n := d[p^ - s];
                  Inc( p );
               End;

            (* fill code-like entries with r *)
            f := 1 Shl ( kk - w );
            j := i Shr w;
            While ( j < z ) Do
            Begin
               (*****************************************************************
                  Use the following line in the debugger to verify the allocated
                  memory boundries with data being inserted.

                  *->   ( LONGINT( q )+( j*SizeOfThuft ) ) - LONGINT( q )    <-*
                *****************************************************************)
               move( r, pointer( LONGINT( q ) + ( j * SizeOfThuft ) )^, SizeOfThuft );
               //CopyMem( pointer( LONGINT( q ) + ( j * SizeOfThuft ) ), @r, SizeOfThuft );
               Inc( j, f );
            End;

            (* backwards increment the kk-bit code i *)
            j := 1 Shl ( kk - 1 );
            While ( i And j ) <> 0 Do             (* added...   <> 0 *)
            Begin
               i := i Xor j;                      (* bitwise exclusive or *)
               j := j Shr 1;
            End;

            i := i Xor j;                         (* bitwise exclusive or *)

            (* backup over finished tables *)
            While i And Pred( 1 Shl w ) <> x[h] Do
            Begin
               Dec( h );
               Dec( w, lx[h] );                   (* don't need to update q *)
            End;
         End;
      End;

      (* return actual size of base table *)
      m := Integer( lx[0] );

      If ( y <> 0 ) Then
         y := 1
      Else
         y := 0;

      If ( g <> 1 ) Then
         g := 1
      Else
         g := 0;

      Result := ( y And g );

      (* Return True ( 1 ) if we were given an incomplete table *)
      //Result := (( y <> 0 ) AND  ( g <> 1 ));
   End;
   //-------------------------------------------------------------

   Function inflate_codes( Var tl, td: PT; bl, bd: Integer ): Byte;
      (* tl, td:   literal/length and distance decoder tables  *)
      (* bl, bd:   number of bits decoded by tl[  ] and td[  ] *)

      (* inflate ( decompress ) the codes in a deflated ( compressed ) block.
         Return an error code or zero if it all goes ok. *)
   Var
      e         : Word;                       (* table entry flag/number of extra bits *)
      n, d      : Word;                       (* length and index for copy *)
      w         : Integer;                    (* current window position *)
      t         : PT;                         (* pointer to table entry *)
      ml, md    : Word;                       (* masks for bl and bd bits *)

   Begin

      Result := 1;                                (* Default = failed *)

      (* make local copies of globals *)
      b := bb;                                    (* initialize bit buffer *)
      k := bk;
      w := wp;                                    (* initialize window position *)

      (* inflate the coded data *)
      ml := maskr[bl];                            (* precompute masks for speed *)
      md := maskr[bd];
      Repeat
         If UnBase.Cancel Then Exit;

         If Not NEEDBITS( bl ) Then Exit;

         t := pointer( LONGINT( tl ) + ( ( Word( b ) And ml ) * SizeOfThuft ) );
         //t := ptr( seg( tl^ ), ofs( tl^ )+ ( ( Word( b ) AND ml ) * SizeOfThuft ) );

         e := t^.e;
         If ( e > 16 ) Then
            While ( e > 16 ) Do
            Begin
               If ( e = 99 ) Then Exit;
               Dec( e, 16 );
               DUMPBITS( t^.b );
               If Not NEEDBITS( e ) Then Exit;
               t := pointer( LONGINT( t^.next ) + ( ( b And maskr[e] ) * SizeOfThuft ) );
               e := t^.e;
            End;

         DUMPBITS( t^.b );
         If ( e = 16 ) Then                       (* ...then it's a literal *)
         Begin
            slide[w] := t^.n;
            Inc( w );

            With UnBase Do
               If w = WSIZE Then
               Begin
                  w := ExtractWriteBlock( Outfile, Slide, False, 32, w, dtData );
                  If w = 0 Then
                     RaiseError( E_RAISE, UnBase.Filename, '', '0', E_FWRITE );

                  ProgressPosition := Bytes_To_Go;
                  DoBranchProgress( InflateRec.PackedSize - Bytes_To_Go,
                     InflateRec.PackedSize, InflateRec.PackedSize );

                  w := 0;
               End;
         End Else Begin                                    (* it's an EOB or a length *)
            (* Exit if end of block *)
            If ( e = 15 ) Then Break;

            (* get length of block to copy *)
            If Not NEEDBITS( e ) Then Exit;

            n := t^.n + ( Word( b ) And maskr[e] );
            (*n := t^.n + ( b AND maskr[ e ] );*)

            DUMPBITS( e );

            (* decode distance of block to copy *)
            If Not NEEDBITS( bd ) Then Exit;
            t := pointer( LONGINT( td ) + ( ( b And md ) * SizeOfThuft ) );

            e := t^.e;
            If e > 16 Then
               Repeat
                  If ( e = 99 ) Then Exit;
                  Dec( e, 16 );
                  DUMPBITS( t^.b );
                  If Not NEEDBITS( e ) Then Exit;

                  //t := pointer( LONGINT( t^.next ) + ( ( Word( b ) AND maskr[ e ] ) * SizeOfThuft ) );
                  t := pointer( LONGINT( t^.next ) + ( ( b And maskr[e] ) * SizeOfThuft ) );
                  e := t^.e;
               Until ( e <= 16 );

            DUMPBITS( t^.b );
            If Not NEEDBITS( e ) Then
               Exit;

            d := Word( w - t^.n - ( b And maskr[e] ) );

            DUMPBITS( e );

            (* do the copy *)
            Repeat
               d := ( d And ( WSIZE - 1 ) );

               If ( d > w ) Then
                  e := WSIZE - d
               Else
                  e := WSIZE - w;

               If ( e > n ) Then
                  e := n;

               Dec( n, e );

               (* incrementing w by e bytes below... do same with bytes_to_go
                  prior to value e changing *)
               If ( ( w - d ) >= e ) Then         (* this test assumes unsigned comparison *)
               Begin
                  move( slide[ d ], slide[ w ], e );
                  //CopyMem( @slide[w], @slide[d], e );
                  Inc( w, e );
                  Inc( d, e );
               End Else                               (* do it slow to avoid memcpy( ) overlap *)
                  Repeat
                     slide[w] := slide[d];
                     Inc( w );
                     Inc( d );
                     Dec( e );
                  Until ( e <= 0 );

               With UnBase Do
                  If ( w = Word( WSIZE ) ) Then
                  Begin

                     w := ExtractWriteBlock( OutFile, Slide, False, 32, w, dtData );
                     If w = 0 Then
                        //wp := 0;
                        RaiseError( E_RAISE, UnBase.Filename, '', '0', E_MEMERR );

                     ProgressPosition := Bytes_To_Go;
                     DoBranchProgress( InflateRec.PackedSize - Bytes_To_Go,
                        InflateRec.PackedSize, InflateRec.PackedSize );

                     w := 0;
                  End;

               If UnBase.Cancel Then Exit;

            Until n = 0;
         End;
      Until ( 1 <> 1 );

      (* restore the globals from the locals *)
      wp := w;                                    (* restore global window pointer   *)
      bb := b;                                    (* restore global bit buffer       *)
      bk := k;

      Result := 0;
   End;
   //-------------------------------------------------------------
     (* "decompress" an inflated type 0 ( stored ) block. *)

   Function inflate_stored: Short;
   Var
      n         : Word;                       (* number of bytes in block *)
      w         : Integer;                    (* current window position *)
      tmp:	Word;
   Begin
      Result := 1;	//E_BADBLOCK

      (* make local copies of globals *)
      b := bb;                                    (* initialize bit buffer *)
      k := bk;
      w := wp;                                    (* initialize window position *)

      (* go to Byte boundary *)
      n := k And 7;
      DUMPBITS( n );

      (* get the length and its complement *)
      If Not NEEDBITS( 16 ) Then Exit;

      n := ( Word( b ) And $FFFF );
      DUMPBITS( 16 );
      If Not NEEDBITS( 16 ) Then Exit;

      //If ( n <> ( Not Word( b ) ) And $FFFF ) Then
      tmp := Word(((not b) and ($ffff)));
      if (n <> tmp) then
      Begin
         wp := 0;
         Exit;
         //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_BADBLOCK );
      End;

      DUMPBITS( 16 );

      (* read and output the compressed data *)
      While ( n <> 0 ) Do
      Begin
         Dec( n );

         If Not NEEDBITS( 8 ) Then Exit;

         //slide[w] := Word( b );
         slide[w] := ShortInt( b );
         Inc( w );

         With UnBase Do
            //If ( w = Word( WSIZE ) ) Then
            If ( w = WSIZE ) Then
            Begin
               w := ExtractWriteBlock( OutFile, Slide, False, 32, w, dtData );
               If w = 0 Then
                  //wp := 0;
                  RaiseError( E_RAISE, UnBase.Filename, '', '0', E_FWRITE );

               ProgressPosition := Bytes_To_Go;
               DoBranchProgress( InflateRec.PackedSize - Bytes_To_Go,
                  InflateRec.PackedSize, InflateRec.PackedSize );

               w := 0;
            End;

         DUMPBITS( 8 );
         If UnBase.Cancel Then Break;

      End;

      (* restore the globals from the locals *)
      wp := w;                                    (* restore global window pointer *)
      bb := b;                                    (* restore global bit buffer *)
      bk := k;
      Result := 0;
   End;
   //-------------------------------------------------------------

   Function inflate_fixed: Short;
      (* decompress an inflated TYPE 1 ( fixed Huffman codes ) block.  We should
        either replace this with a custom decoder, or at least precompute the
        Huffman tables. *)
   Var
      i         : Integer;                    (* temporary variable *)
      l         : Array[0..287] Of Word;      (* length list for huft_build *)
      fixed_bl, fixed_bd: Integer;
      HFTD, HFTL  : PT;
   Begin

      (* If first time, set up tables for fixed blocks *)
      If ( fixed_tl = Nil ) Then
      Begin

         { set up literal table }
         for i := 0 to 143 do
           l[i] := 8;
         for I := 144 to 255 do
           l[i] := 9;
         for i := 256 to 279 do
           l[i] := 7;
         for i := 280 to 287 do          { make a complete, but wrong code set }
           l[i] := 8;

         fixed_bl := 7;
         i := huft_build( l, 288, 257, cplens, cplext, fixed_tl, HFTL, fixed_bl );
         If ( i <> 0 ) Then
         Begin
            fixed_tl := Nil;
            Result := i;
            //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_CODESET );
            Exit;
         End;

         (* distance table *)
         For i := 0 To 29 Do                      (* make an incomplete code set *)
            l[i] := 5;

         fixed_bd := 5;

         i := huft_build( l, 30, 0, cpdist, cpdext, fixed_td, HFTD, fixed_bd );
         If ( i > 1 ) Then
         Begin
            Result := i;
            Exit;
            //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_CODESET );
            //Exit;
         End;

      End;

      (* decompress until an end-of-block code *)
      If ( inflate_codes( fixed_tl, fixed_td, fixed_bl, fixed_bd ) <> 0 ) And ( Not EOF ) Then
      begin
         //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_BADBLOCK );
         Result := 1;
         Exit;
      end;

      Result := 0;

   End;
   //-------------------------------------------------------------
     (* decompress an inflated type 2 ( dynamic Huffman codes ) block. *)

   Function inflate_dynamic: Short;
   Var
      i         : Integer;                    (* temporary variables *)
      j         : Word;
      l         : Word;                       (* last length *)
      m         : Word;                       (* mask for bit lengths table *)
      n         : Word;                       (* number of lengths to get *)
      tl        : PT;                         (* literal/length code table *)
      td        : PT;                         (* distance code table *)
      HFTL, HFTD  : PT;
      bl        : Integer;                    (* lookup bits for tl *)
      bd        : Integer;                    (* lookup bits for td *)
      nb        : Word;                       (* number of bit length codes *)
      nl        : Word;                       (* number of literal/length codes *)
      nd        : Word;                       (* number of distance codes *)
      {$IFDEF PKZIP_BUG_WORKAROUND}
      ll        : Array[0..288 + 32] Of Word;
      {$ELSE}
      ll        : Array[0..286 + 30] Of Word;
      {$ENDIF}

   Const
      border    : Array[0..18] Of Byte =      (* Order of the bit length code lengths *)
      ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );
      dummy1    : Array[0..30] Of Word =
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );
      dummy2    : Array[0..30] Of Byte =      (* Extra bits for literal codes 257..285 *)
      ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ); (* 99==invalid *)
   Begin
      Result := 1;

      (* make local bit buffer *)
      b := bb;
      k := bk;

      (* read in table lengths *)
      If Not NEEDBITS( 5 ) Then Exit;

      nl := 257 + ( Word( b ) And $1F );          (* number of literal/length codes *)

      DUMPBITS( 5 );
      If Not NEEDBITS( 5 ) Then Exit;

      nd := 1 + ( Word( b ) And $1F );            (* number of distance codes *)
      DUMPBITS( 5 );
      If Not NEEDBITS( 4 ) Then Exit;
      nb := 4 + ( Word( b ) And $F );             (* number of bit length codes *)
      DUMPBITS( 4 );

      {$IFDEF PKZIP_BUG_WORKAROUND}
      If ( ( nl > 288 ) Or ( nd > 32 ) ) Then
         {$ELSE}
      If ( ( nl > 286 ) Or ( nd > 30 ) ) Then
         {$ENDIF}
         Exit;	//Result equals 1;
         //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_INVALIDLEN );

      (* read in bit-length-code lengths *)
      For j := 0 To nb - 1 Do
      Begin
         If Not NEEDBITS( 3 ) Then Exit;
         ll[border[j]] := Word( b ) And 7;
         DUMPBITS( 3 );
      End;

      For j := nb To 18 Do
         ll[border[j]] := 0;

      (* build decoding table for trees--single level, 7 bit lookup *)
      bl := 7;
      HFTL := Nil;
      i := huft_build( ll, 19, 19, dummy1, dummy2, tl, HFTL, bl );
      If ( i <> 0 ) Then
      Begin
         Result := i;                   { incomplete code set }
         Exit;
         //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_CODESET );
      End;


      (* read in literal and distance code lengths *)
      n := nl + nd;
      m := maskr[bl];
      i := 0;
      l := 0;

      While ( Word( i ) < n ) Do
      Begin

         If UnBase.Cancel Then Exit;

         If Not NEEDBITS( Word( bl ) ) Then Exit;
         td := pointer( LONGINT( tl ) + ( ( b And m ) * SizeOfThuft ) );

         j := td^.b;
         DUMPBITS( j );

         j := td^.n;
         If ( j < 16 ) Then                       (* length of code in bits ( 0..15 ) *)
         Begin
            ll[i] := j;
            l := j;                               (* save last length in l *)
            Inc( i );
         End Else
            If ( j = 16 ) Then                    (* repeat last length 3 TO 6 times *)
            Begin
               If Not NEEDBITS( 2 ) Then Exit;

               j := 3 + ( Word( b ) And 3 );
               DUMPBITS( 2 );

               If ( Word( i ) + j > n ) Then
                  Exit;	//Result equals 1;
                  //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_CODESET );

               While ( j <> 0 ) Do
               Begin
                  ll[i] := l;
                  Inc( i );
                  Dec( j );
               End
            End Else
               If ( j = 17 ) Then                 (* 3 TO 10 zero length codes *)
               Begin
                  If Not NEEDBITS( 3 ) Then Exit;

                  j := 3 + ( Word( b ) And 7 );

                  DUMPBITS( 3 );
                  If ( Word( i ) + j > n ) Then
                     Exit;	//Result equals 1
                     //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_CODESET );

                  While ( j <> 0 ) Do
                  Begin
                     ll[i] := 0;
                     Inc( i );
                     Dec( j );
                  End;
                  l := 0;
               End Else Begin                              (* j == 18: 11 to 138 zero length codes *)
                  If Not NEEDBITS( 7 ) Then Exit;

                  j := 11 + ( Word( b ) And $7F );

                  DUMPBITS( 7 );
                  If ( Word( i ) + j > n ) Then
                     Exit;	//Result equals 1
                     //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_CODESET );

                  While ( j <> 0 ) Do
                  Begin
                     ll[i] := 0;
                     Inc( i );
                     Dec( j );
                  End;
                  l := 0;
               End;
      End;

      (* free decoding table for trees *)
      huft_free();                                  //(HFTL);

      (* restore the global bit buffer *)
      bb := b;
      bk := k;

      (* build the decoding tables for literal/length and distance codes *)
      bl := lbits;
      HFTL := Nil;
      i := huft_build( ll, nl, 257, cplens, cplext, tl, HFTL, bl );
      If ( i <> 0 ) Then
      Begin
         Result := i;                   { incomplete code set }
         Exit;
         //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_CODESET );
      End;

      bd := dbits;
      HFTD := Nil;
      i := huft_build( ll[nl], nd, 0, cpdist, cpdext, td, HFTD, bd );
      If ( i <> 0 ) Then
      Begin
         Result := i;
         Exit;
      End;

      (* decompress until an end-of-block code *)
      If ( ( inflate_codes( tl, td, bl, bd ) ) <> 0 ) And ( Not EOF ) Then
         Exit;	//Result equals 1
         //UnBase.RaiseError( E_RAISE, UnBase.Filename, '', '0', E_CODESET );


      (* free the decoding tables, return *)
      huft_free();                                  //(HFTL) (HFTD);
      Result := 0;

   End;
   //-------------------------------------------------------------

Begin
   Result := 2; // invalid code set

   (* make local bit buffer *)
   b := bb;
   k := bk;

   (* read in last block bit *)
   If Not NEEDBITS( 1 ) Then Exit;

   e := Integer( b ) And 1;

   DUMPBITS( 1 );

   (* read in block type *)
   If Not NEEDBITS( 2 ) Then Exit;

   t := Word( b ) And 3;

   DUMPBITS( 2 );

   (* restore the global bit buffer *)
   bb := b;
   bk := k;

   (* inflate that block type *)
   Case t Of
      0: Result := inflate_stored;
      1: Result := inflate_fixed;
      2: Result := inflate_dynamic;
   Else
      //UnBase.RaiseErrorStr( UnBase.Filename, '', '0', E_BADBLOCK );

      With UnBase Do
      Begin
         ProgressPosition := Bytes_To_Go;
         DoBranchProgress( InflateRec.PackedSize - Bytes_To_Go,
            InflateRec.PackedSize, InflateRec.PackedSize );
      End;

      Exit;
   End;

   // Undo too much lookahead. The next read will be Byte aligned so we
   // can discard unused bits in the last meaningful Byte.
   While bk >= 8 Do
   Begin
      Dec( bk, 8 );
      Dec( inptr );
   End;

End;

Procedure Write_Out_Block( Sender: TUnBASE; Var Outfile: TStream32 );
Begin
   With Sender Do
      If wp > 0 Then
      Begin
         If ExtractWriteBlock( OutFile, Slide, False, 32, wp, dtData ) = 0 Then
            RaiseError( E_RAISE, Filename, '', '0', E_FWRITE );

         ProgressPosition := Bytes_To_Go;
         DoBranchProgress( Sender.InflateRec.PackedSize - Bytes_To_Go,
            Sender.InflateRec.PackedSize, Sender.InflateRec.PackedSize );

         wp := 0;
      End;
End;


(* The file position is set prior to entering this unit. *)
Function ExtractFile( UnBase: TUnBASE; Infile: TStream32; Var Outfile: TStream32; IR: TInflateRec ): Boolean;
Var
   e: Integer;                    (* last block flag *)
   h: Word;                       (* maximum struct huft's malloc'ed *)
   DataLength: Integer;
   //FCrc: u_long;
   LCrc: ^u_long;
   Len: ^Integer;
Begin
   Result := True;
   fixed_List := TList.Create;
   EOF := False;
   SizeOfThuft := SizeOf( Thuft );

   With UnBase Do
   Try
      Try
         InPTR := 0;
         ZipCount := 0;
         InflateRec := IR;
         Crc32Val := CRC_MASK;

         (* remove the sizeof extra encryption header from PackedSize *)
         If ( InflateRec.BitFlag And 1 ) > 0 Then
            Dec( InflateRec.PackedSize, RAND_HEAD_LEN );

         Bytes_To_Go := InflateRec.PackedSize;

         Repeat
            fixed_tl := Nil;
            fixed_td := Nil;

            (* initialize window, bit buffer *)
            wp := 0;
            bk := 0;
            bb := 0;

            (* decompress until the last block *)
            h := 0;
            Try
               Try
                  Repeat
                     hufts := 0;
                     If inflate_block( UnBase, Infile, Outfile, e ) <> 0 Then
                     Begin
                        Write_Out_Block( UnBase, Outfile );
                        Exit;
                     End;
                     //If Not inflate_block( UnBase, Infile, Outfile, e ) Then
                     //Begin
                     //	Cancel := True;
                     //	Break;
                     //End;

                     If ( hufts > h ) Then
                        h := hufts;

                  Until ( e <> 0 ) Or EOF Or Cancel;
               Finally
                  Huft_Free;
               End;
            Except
               //ON E: E_RAISE DO ShowMessage( E.Message );
            End;

            If ( ArcType = atGZip ) And ( Not EOF ) And ( Not Cancel ) Then
            Begin

               If Bytes_To_Go < 8 Then Break;

               DataLength := WP;         //assign prior to writing data... (WP will be zero'd)
               Write_Out_Block( UnBase, Outfile );

               (* Assign values to FCrc & Len from buffer *)
               LCrc := @Inbuf[Inptr];
               Len := @Inbuf[Inptr + SizeOf( FCrc )];

               (* Check the stored data length against the processed length *)
               If Len^ <> DataLength Then ;

               If ( LCrc^ Xor CRC_MASK = Crc32VAL ) Or ( LCrc^ = Crc32Val ) Then
               Begin
                  If ( LCrc^ Xor CRC_MASK <> FCrc ) And ( LCrc^ <> FCrc ) Then
                     Crc32Val := CRC_MASK
                  Else
                     Exit;
               End Else
                  ;                      //Crc check failed...

               Inc( Inptr, 8 );          //crc and filesize
               Dec( Bytes_To_Go, 8 );

               //see 3\gz\bugs\old_access_log.2000Aug.gz
               Inc( Inptr, GZipHdr_Size );
               Dec( Bytes_To_Go, GZipHdr_Size );

               If Inptr > WSize Then
               	Infile.Seek(Inptr - WSize, soCurrent);

            End Else
               Break;

         Until ( Bytes_To_Go <= 0 ) Or EOF Or Cancel;

      Except
         //ON E: E_RAISE DO ShowMessage( E.Message );
      End;
   Finally
      Write_Out_Block( UnBase, Outfile );
      huft_free;
      fixed_List.Free;
   End;
End;
//-------------------------------------------------------------

End.
