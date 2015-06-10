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
Unit ztvUnArc;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   ztvGbls,
   ztvBase,
   ztvStreams,
   Err_Msgs;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TUnArc = Class(TUnBASE)
   Private
      FileProgressPos: u_long;
      Function TranslateFilename: String;
      Function BuildHeadArray(Infile: TStream32): Integer;
      Function OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
      Procedure ProcessHeaders(Infile, Outfile: TStream32);
   Protected
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure ExtractIT(Var Infile: TStream32; Outfile: TStream32); Override;
   Published
      Property ArcType;
      Property ConfirmOverwrites;
      Property CreateStoredDirs;
      Property DateAttribute;
      Property ExtractDir;
      Property FileSpec;
      Property OverwriteMode;
      Property RestoreFileAttr;
      Property UseStoredDirs;
      Property OnActivate;
      Property OnDeactivate;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnProgress;
      Property OnRenameFile;
   End;

Implementation


Const
   blocksize = 32767 {512};             (* I/O block size *)
   Signature = 26;                      (* special archive marker *)
   //arcver = 9;            		(* max archive header version code *)

Type
   nd = Packed Record
      Child: Array[0..1] Of smallint
   End;

   Entry = Packed Record
      Used: Boolean;
      Nexth: smallint;
      Predecessor: word;                //SMALLINT;  (* 7/28/98 *)
      Follower: Byte
   End;

   //************************************
   // definitions for uncrunch / unsquash
   //************************************
Const

   BITSM1 = 12;
   Clear = 256;
   FIRST = 257;
   INIT_BITS = 9;
   HSIZEM1 = 8191;
   CRUNCH_BITS = 12;
   SQUASH_BITS = 13;
   TABSIZEM1 = 4095;
   TABLE_SIZE = 4096;
   Empty: word = $FFFF;
   NO_PRED: word = $FFFF;
   RMASK: Array[0..8] Of Byte = ($00, $01, $03, $07, $0F, $1F, $3F, $7F, $FF);

   //************************************
   // definitions for unsqueeze
   //************************************
Const
   Error = -1;
   SPEOF = 256;
   NUMVALS = 256;                       (* 1 less than the number of values *)

   //************************************
   // definitions for unpack
   //************************************
Const
   DLE = $90;

Var
   ArcBuf: Array[1..blocksize + 1] Of Byte;
   ExtBuf: Array[1..blocksize + 1] Of Byte;

   Buffer: Array[0..BITSM1] Of Byte;
   node: Array[0..NUMVALS] Of nd;
   Stack: Array[0..TABSIZEM1] Of Byte;
   Prefix: Array[0..HSIZEM1] Of smallint;
   Suffix: Array[0..HSIZEM1] Of Byte;
   Stack1: Array[0..HSIZEM1] Of Byte;
   StringTable: Array[0..TABSIZEM1] Of Entry;
   inbufptr: Integer;
   state: (NOHIST, INREP);
   FirstChar, FirstC, NewHash, EndOfFile: Boolean;
   BITS, NBits, MaxCodeMax, Offset, ArcPTR, ExtPTR: smallint;
   SP, CodeCount, oldcode, finchar, FreeEnt, LastC: smallint;
   SizeX, bPos, CurrentPTR, NumNodes, MAXCODE, ClearFlag: smallint;

//-------------------------------------------------------------

Constructor TUnArc.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TUnArc.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function Read_Block(UnBase: TUnArc; Infile: TStream32): Integer;
Begin
   Result := UnBase.ReadBlock(Infile, Nil, ArcBuf[1], False, 0, blocksize, dtData);
   If Result = 0 Then EndOfFile := True;
   ArcPTR := 1
End;
//-------------------------------------------------------------

Procedure Write_Block(Sender: TUnArc; Outfile: TStream32);
Var
   size: Integer;
Begin
   With Sender Do
   Begin
      size := ExtractWriteBlock(Outfile, ExtBuf[1], False, 16, ExtPTR, dtData);
      If size = 0 Then
         RaiseError(E_RAISE, Sender.FileName, '', '0', E_FWRITE);

      ProgressPosition := ProgressPosition - size;
      Dec(FileProgressPos, size);
      doBranchProgress(InflateRec.UnpackedSize - FileProgressPos,
         InflateRec.UnpackedSize, fTotalUnpackedSize);

   End;

   ExtPTR := 1;
End;
//-------------------------------------------------------------
//  read char from archive
//-------------------------------------------------------------

Function GetChar(Sender: TUnArc; Infile: TStream32): Byte;
Begin
   If EndOfFile Then
      Result := 0
   Else
   Begin
      If ArcPTR = blocksize Then
         Read_Block(Sender, Infile)
      Else
         inc(ArcPTR);

      Result := ArcBuf[ArcPTR];
      Dec(Sender.Bytes_To_Go);
   End;
End;
//-------------------------------------------------------------
//  read record from archive
//-------------------------------------------------------------

Procedure fread(Sender: TUnArc; Infile: TStream32; Var Buffer; RecLen: smallint);
Var
   i: smallint;
   b: Array[1..MaxInt] Of Byte Absolute Buffer;
Begin
   For i := 1 To RecLen Do
      b[i] := GetChar(Sender, Infile);
End;
//-------------------------------------------------------------
// write char to extracted file
//-------------------------------------------------------------

Procedure put_ext(Sender: TUnArc; Outfile: TStream32; C: Byte);
Begin
   ExtBuf[ExtPTR] := C;

   If ExtPTR = blocksize Then
      Write_Block(Sender, Outfile)
   Else
      inc(ExtPTR);
End;
//-------------------------------------------------------------
//  Decrunch: calculate hash value for LZW compression
//-------------------------------------------------------------

Function H(Pred, Foll: smallint): smallint;
Var
   Local: longint;
Begin
   If Not NewHash Then
      Local := (Pred + Foll) Or $0800
   Else
      Local := (Pred + Foll) * 15073;

   Result := smallint(Local And $0FFF);
End;
//-------------------------------------------------------------
// Decrunch:  find end of an LZW chain
//-------------------------------------------------------------

Function EOList(Index: smallint): smallint;
Var
   Temp: smallint;
Begin
   Temp := StringTable[Index].Nexth;

   While Temp <> 0 Do
   Begin
      Index := Temp;
      Temp := StringTable[Index].Nexth
   End;

   Result := Index
End;
//-------------------------------------------------------------
// Decrunch:  add Pred/succ pair to LZW hash table
//-------------------------------------------------------------

Function Hash(Pred, Foll: smallint): smallint;
Var
   Local, TempNext: smallint;
Begin
   Local := H(Pred, Foll);

   If Not StringTable[Local].Used Then
      Hash := Local
   Else
   Begin
      Local := EOList(Local);
      TempNext := (Local + 101) And $0FFF;

      While StringTable[TempNext].Used Do
      Begin
         inc(TempNext);
         If TempNext = TABLE_SIZE Then
            TempNext := 0
      End;

      StringTable[Local].Nexth := TempNext;
      Hash := TempNext
   End
End;
//-------------------------------------------------------------
// Decrunch:  update LZW Hash table Entry
//-------------------------------------------------------------

Procedure UpdateTable(Pred, Foll: smallint);
Begin
   With StringTable[Hash(Pred, Foll)] Do
   Begin
      Used := True;
      Nexth := 0;
      Predecessor := Pred;
      Follower := Foll
   End
End;
//-------------------------------------------------------------
// Decrunch:  initialize LZW string table
//-------------------------------------------------------------

Procedure InitStringTable;
Var
   i: smallint;
Begin
   FillChar(StringTable, SizeOf(StringTable), 0);

   For i := 0 To 255 Do
      UpdateTable(NO_PRED, i);

   inbufptr := Empty;
End;
//-------------------------------------------------------------
// Decrunch:  init LZW routines
//-------------------------------------------------------------

Procedure InitLZW(i: smallint);
Begin
   NewHash := i = 1;
   SP := 0;
   InitStringTable;
   CodeCount := TABLE_SIZE - 256;
   FirstC := True
End;
//-------------------------------------------------------------
// Depack:  get one char from buffer
//-------------------------------------------------------------

Function GetCharUnp(Sender: TUnArc; Infile: TStream32): smallint;
Begin
   If Sender.Bytes_To_Go = 0 Then
      Result := -1
   Else
      Result := smallint(GetChar(Sender, Infile));
End;
//-------------------------------------------------------------
// Decrunch:
//-------------------------------------------------------------

Function GoCode(Sender: TUnArc; Infile: TStream32): smallint;
Var
   LocalBufPTR, RetVal: smallint;
Begin
   If inbufptr = Empty Then
   Begin
      LocalBufPTR := GetCharUnp(Sender, Infile);

      If LocalBufPTR = -1 Then
      Begin
         Result := -1;
         Exit;
      End;

      LocalBufPTR := LocalBufPTR And $00FF;
      inbufptr := GetCharUnp(Sender, Infile);
      If inbufptr = -1 Then
      Begin
         Result := -1;
         Exit;
      End;

      inbufptr := inbufptr And $00FF;
      RetVal := ((LocalBufPTR Shl 4) And $0FF0) + ((inbufptr Shr 4) And $000F);
      inbufptr := inbufptr And $000F
   End
   Else
   Begin
      LocalBufPTR := GetCharUnp(Sender, Infile);
      If LocalBufPTR = -1 Then
      Begin
         Result := -1;
         Exit;
      End;

      LocalBufPTR := LocalBufPTR And $00FF;
      RetVal := LocalBufPTR + ((inbufptr Shl 8) And $0F00);
      inbufptr := Empty
   End;
   Result := RetVal;
End;
//-------------------------------------------------------------
// Decrunch:  push a char onto LZW 'pending' Stack
//-------------------------------------------------------------

Procedure Push(Sender: TUnArc; C: smallint);
Begin
   Stack[SP] := C;
   SP := SP + 1;

   If SP >= TABLE_SIZE Then
      Sender.RaiseError(E_RAISE, Sender.FileName, '', '0', E_STACK);
End;
//-------------------------------------------------------------
// pop a character from LZW 'pending' Stack
//-------------------------------------------------------------

Function Pop: smallint;
Begin
   If SP > 0 Then
   Begin
      Dec(SP);
      Result := Stack[SP]
   End
   Else
      Result := Empty
End;
//-------------------------------------------------------------
// get Nexth (uncompressed) LZW character
//-------------------------------------------------------------

Function GetCharDec(Sender: TUnArc; Infile: TStream32): smallint;
Var
   code, NewCode: smallint;
Begin
   If FirstC Then
   Begin
      FirstC := False;
      oldcode := GoCode(Sender, Infile);
      finchar := StringTable[oldcode].Follower;
      Result := finchar;
      Exit;
   End;

   If SP = 0 Then
   Begin
      NewCode := GoCode(Sender, Infile);
      code := NewCode;
      If code = -1 Then
      Begin
         Result := -1;
         Exit;
      End;

      If Not StringTable[code].Used Then
      Begin
         code := oldcode;
         Push(Sender, finchar)
      End;

      While StringTable[code].Predecessor <> NO_PRED Do
         With StringTable[code] Do
         Begin
            Push(Sender, Follower);
            code := Predecessor
         End;

      finchar := StringTable[code].Follower;
      Push(Sender, finchar);

      If CodeCount <> 0 Then
      Begin
         UpdateTable(oldcode, finchar);
         CodeCount := CodeCount - 1
      End;

      oldcode := NewCode
   End;
   Result := Pop;
End;
//-------------------------------------------------------------
// Decrunch:
//-------------------------------------------------------------

Function GetCode(Sender: TUnArc; Infile: TStream32): smallint;
Label
   Nexth;
Var
   bp: Byte;
   i, code, r_off, BitSx: smallint;
Begin
   If FirstChar Then
   Begin
      Offset := 0;
      SizeX := 0;
      FirstChar := False;
   End;

   bp := 0;

   If (ClearFlag > 0) Or (Offset >= SizeX) Or (FreeEnt > MAXCODE) Then
   Begin
      If FreeEnt > MAXCODE Then
      Begin
         NBits := NBits + 1;
         If NBits = BITS Then
            MAXCODE := MaxCodeMax
         Else
            MAXCODE := (1 Shl NBits) - 1;
      End;

      If ClearFlag > 0 Then
      Begin
         NBits := INIT_BITS;
         MAXCODE := (1 Shl NBits) - 1;
         ClearFlag := 0;
      End;

      //FOR SizeX := 0 TO NBits-1 DO
      For i := 0 To NBits - 1 Do
      Begin
         SizeX := i;                    (* added *)
         code := GetCharUnp(Sender, Infile);
         If code = -1 Then
            Goto Nexth
         Else
            Buffer[i] := code;
      End;

      //SizeX := SizeX + 1;
      SizeX := NBits + 1;

      Nexth:
      If SizeX <= 0 Then
      Begin
         Result := -1;
         Exit;
      End;

      Offset := 0;
      SizeX := (SizeX Shl 3) - (NBits - 1);
   End;

   r_off := Offset;
   BitSx := NBits;

   // ****************
   //  get first byte
   // ****************
   bp := bp + (r_off Shr 3);
   r_off := r_off And 7;

   // ********************************
   //  get first part (low order Bits)
   // ********************************
   code := Buffer[bp] Shr r_off;
   bp := bp + 1;
   BitSx := BitSx - (8 - r_off);
   r_off := 8 - r_off;

   If BitSx >= 8 Then
   Begin
      code := code Or (Buffer[bp] Shl r_off);
      bp := bp + 1;
      r_off := r_off + 8;
      BitSx := BitSx - 8;
   End;

   code := code Or ((Buffer[bp] And RMASK[BitSx]) Shl r_off);
   Offset := Offset + NBits;
   Result := code;
End;
//-------------------------------------------------------------
// Depack:  write char to buffer
//-------------------------------------------------------------

Procedure PutCharUnp(Sender: TUnArc; Outfile: TStream32; C: smallint);
Begin
   put_ext(Sender, Outfile, C)
End;
//-------------------------------------------------------------
// Depack:  write char to buffer, run-length comp checking
//-------------------------------------------------------------

Procedure PutCharDec(Sender: TUnArc; Outfile: TStream32; C: smallint);
Begin
   Case state Of
      NOHIST:
         If C = DLE Then
            state := INREP
         Else
         Begin
            LastC := C;
            PutCharUnp(Sender, Outfile, C)
         End;
      INREP:
         Begin
            If C = 0 Then
               PutCharUnp(Sender, Outfile, DLE)
            Else
            Begin
               C := C - 1;
               While (C <> 0) Do
               Begin
                  PutCharUnp(Sender, Outfile, LastC);
                  C := C - 1
               End
            End;

            state := NOHIST
         End;
   End;
End;
//-------------------------------------------------------------
// Decrunch:  decompress a file with LZW
//-------------------------------------------------------------

Procedure Decompress(Sender: TUnArc; Infile, Outfile: TStream32; SquashFlag: smallint);
Label
   Nexth;
Var
   stackp, finchar,
      code, oldcode, incode: smallint;
Begin
   ExtPTR := 1;
   ArcPTR := blocksize;

   If SquashFlag = 0 Then
      BITS := CRUNCH_BITS
   Else
      BITS := SQUASH_BITS;

   If FirstChar Then
      MaxCodeMax := 1 Shl BITS;

   If SquashFlag = 0 Then
   Begin
      code := GetCharUnp(Sender, Infile);
      If code <> BITS Then
      Begin
         Sender.RaiseErrorStr(Sender.FileName, '', '0', E_BITSERR);
         Exit;
      End;
   End;

   ClearFlag := 0;
   NBits := INIT_BITS;
   MAXCODE := (1 Shl NBits) - 1;

   For code := 255 Downto 0 Do
   Begin
      Prefix[code] := 0;
      Suffix[code] := code;
   End;

   FreeEnt := FIRST;
   oldcode := GetCode(Sender, Infile);
   finchar := oldcode;

   If oldcode = -1 Then Exit;

   If SquashFlag = 0 Then
      PutCharDec(Sender, Outfile, finchar)
   Else
      PutCharUnp(Sender, Outfile, finchar);

   stackp := 0;

   code := GetCode(Sender, Infile);
   While (code > -1) Do
   Begin
      If code = Clear Then
      Begin
         For code := 255 Downto 0 Do
            Prefix[code] := 0;

         ClearFlag := 1;
         FreeEnt := FIRST - 1;
         code := GetCode(Sender, Infile);
         If code = -1 Then
            Goto Nexth;
      End;
      Nexth:
      incode := code;
      If code >= FreeEnt Then
      Begin
         Stack1[stackp] := finchar;
         stackp := stackp + 1;
         code := oldcode;
      End;

      While (code >= 256) Do
      Begin
         Stack1[stackp] := Suffix[code];
         stackp := stackp + 1;
         code := Prefix[code];
      End;

      finchar := Suffix[code];
      Stack1[stackp] := finchar;
      stackp := stackp + 1;
      Repeat
         stackp := stackp - 1;
         If SquashFlag = 0 Then
            PutCharDec(Sender, Outfile, Stack1[stackp])
         Else
            PutCharUnp(Sender, Outfile, Stack1[stackp]);
      Until stackp <= 0;

      code := FreeEnt;

      If code < MaxCodeMax Then
      Begin
         Prefix[code] := oldcode;
         Suffix[code] := finchar;
         FreeEnt := code + 1;
      End;

      oldcode := incode;
      code := GetCode(Sender, Infile);
   End;
End;
//-------------------------------------------------------------
// Unsqueeze:  initialize
//-------------------------------------------------------------

Procedure InitUnsqueeze(Sender: TUnArc; Infile: TStream32);
Var
   i: smallint;
Begin
   bPos := 99;

   fread(Sender, Infile, NumNodes, SizeOf(NumNodes));
   If (NumNodes < 0) Or (NumNodes > NUMVALS) Then
      Sender.RaiseError(E_RAISE, Sender.FileName, '', '0', E_DECTREE);

   node[0].Child[0] := -(SPEOF + 1);
   node[0].Child[1] := -(SPEOF + 1);

   For i := 0 To NumNodes - 1 Do
   Begin
      fread(Sender, Infile, node[i].Child[0], SizeOf(smallint));
      fread(Sender, Infile, node[i].Child[1], SizeOf(smallint))
   End;
End;
//-------------------------------------------------------------
// Unsqueeze:
//-------------------------------------------------------------

Function GetCharUsq(Sender: TUnArc; Infile: TStream32): smallint;
Var
   i: smallint;
Begin
   i := 0;

   While i >= 0 Do
   Begin
      bPos := bPos + 1;

      If bPos > 7 Then
      Begin
         CurrentPTR := GetCharUnp(Sender, Infile);
         If CurrentPTR = Error Then
         Begin
            GetCharUsq := Error;
            Exit;
         End;
         bPos := 0;
         i := node[i].Child[1 And CurrentPTR]
      End
      Else
      Begin
         CurrentPTR := CurrentPTR Shr 1;
         i := node[i].Child[1 And CurrentPTR]
      End
   End;

   i := -(i + 1);

   If i = SPEOF Then
      GetCharUsq := -1
   Else
      GetCharUsq := i;

End;
//-------------------------------------------------------------

Function TUnArc.TranslateFilename: String;
Var
   ArcDir: String;
Begin
   With ArcHeader Do
   Begin

      (* Get filename prior to checking next 		*)
      (* CompressType byte.                  		*)
      ActualFilename := StrPas(@aFilename);

      (* Header is an appended directory entry 		*)
      If CompressType = 30 Then
      Begin
         ArcDir := AppendDirTail(ActualFilename);
         ActualFilename := '';
         PackedSize := 0;
         UnpackedSize := 0;
      End
      Else
         ArcDir := '';

      (* Retain dir name across file boundries.		*)
      (*	Arc602 stores the dir as a separate header*)
      (* prior to files in that dir.  No dirs are  *)
      (* stored in compressed file headers.        *)
      ActualFilename := ArcDir + ActualFilename;
      ActualFilename := OemToCharFilter(ActualFilename, fTransOemChar);
      FileName := ActualFilename;
   End;
End;
//-------------------------------------------------------------

Function TUnArc.OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
Var
   C: smallint;
Begin

   Result := False;
   With ArcHeader Do
      If doOnBegin(CompressType = 31) Then
      Begin
         Try
            Try
               Crc16Val := 0;
               state := NOHIST;
               FirstChar := True;
               Bytes_To_Go := PackedSize;
               FileProgressPos := InflateRec.UnpackedSize;

               If Open_OutFile(Outfile, FileName, ActualFilename) Then
               Begin
                  Try
                     Case CompressType Of
                        1: Unstore(Infile, Outfile, 16, '0', InflateRec);

                        2:
                           Begin
   									ExtPTR := 1;
   									ArcPTR := blocksize;
                              C := GetCharUnp(Self, Infile);
                              While C <> -1 Do
                              Begin
                                 PutCharUnp(Self, Outfile, C);
                                 C := GetCharUnp(Self, Infile);
                              End;
                           End;

                        3:
                           Begin
                              C := GetCharUnp(Self, Infile);
                              While C <> -1 Do
                              Begin
                                 PutCharDec(Self, Outfile, C);
                                 C := GetCharUnp(Self, Infile);
                              End;
                           End;

                        4:              // Unsqueeze
                           Begin
                              InitUnsqueeze(Self, Infile);
                              C := GetCharUsq(Self, Infile);
                              While C <> -1 Do
                              Begin
                                 PutCharDec(Self, Outfile, C);
                                 C := GetCharUsq(Self, Infile);
                              End;
                           End;

                        5:
                           Begin
                              InitLZW(0);
                              C := GetCharDec(Self, Infile);
                              While C <> -1 Do
                              Begin
                                 PutCharUnp(Self, Outfile, C);
                                 C := GetCharDec(Self, Infile);
                              End;
                           End;

                        6:
                           Begin
                              InitLZW(0);
                              C := GetCharDec(Self, Infile);
                              While C <> -1 Do
                              Begin
                                 PutCharDec(Self, Outfile, C);
                                 C := GetCharDec(Self, Infile);
                              End;
                           End;

                        7:
                           Begin
                              InitLZW(1);
                              C := GetCharDec(Self, Infile);
                              While C <> -1 Do
                              Begin
                                 PutCharDec(Self, Outfile, C);
                                 C := GetCharDec(Self, Infile);
                              End;
                           End;

                        8: Decompress(Self, Infile, Outfile, 0); (* Uncrunch *)

                        9: Decompress(Self, Infile, Outfile, 1); (* Unsquash *)
                     Else
                        AdjustProgress(InflateRec);
                        RaiseErrorStr(FileName, '', '0', E_UNKNMETH);
                     End;
                  Finally
                     Dec(ExtPTR);
                     If (ExtPTR > 0) Then
                        Write_Block(Self, Outfile);

                     CloseAndSetDate(Outfile, FileName, FileDate, 32);
                  End;
               End Else Begin
                  RaiseErrorStr(fFileName, '', '0', E_FOPEN);
                  AdjustProgress(InflateRec);
                  Dec(Count);
               End;
            Finally
               Result := doOnEnd(16, CRC16);
            End;
         Except
            //ON e: exception DO ShowMessage( e.message );
         End;
      End
      Else
         AdjustProgress(InflateRec);
End;
//-------------------------------------------------------------

Procedure TUnArc.ProcessHeaders(Infile, Outfile: TStream32);
Var
   i: Integer;
Begin
   ProgressPosition := fTotalUnpackedSize;

   With ArcHeader Do
      For i := 0 To HeaderList.FileCount - 1 Do
      Begin
         If Cancel Then break;

         pUBFI := HeaderList.FileLocationData(i);
         With pUBFI^ Do
         Begin
         	Infile.Seek(OffsetOfLocalHeader, soBeginning);
            If ReadBlock(Infile, Nil, ArcHeader, False, 0, ArcHdr_Size, dtHeader) <> ArcHdr_Size Then
               break
            Else
               FileDate := SwapWords(FileDate);

            // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
            GlobalDate := FileDate;

            If (marker = 26) And (CompressType < 31) Then
            Begin
               TranslateFilename();
               InflateRec.BitFlag := 0; //no password protection
               InflateRec.CompressType := CompressType;
               InflateRec.PackedSize := PackedSize;
               InflateRec.UnpackedSize := UnpackedSize;
               OpenAndExtractFile(Infile, Outfile, FileAttr);
            End;
         End;
      End;
End;
//-------------------------------------------------------------

Function TUnArc.BuildHeadArray(Infile: TStream32): Integer;
Var
   CurrentPos: Integer;
   UBFI: TUnBaseFileInfo;
Begin
   With ArcHeader Do
   Try

   	ZipTimer.Suspend();
      EndOfFile := False;
      CurrentPos := fOffsetStart;

      Infile.Seek(CurrentPos, soBeginning);
      ReadBlock(Infile, Nil, ArcHeader, False, 0, ArcHdr_Size, dtHeader);
      While (marker = 26) And (CompressType > 0) And (CompressType < 31) Do
      Begin
         If Cancel Then break;

         TranslateFilename();
         If (ArcHeader.CompressType <> 30 {directory attribute}) And
         	CheckWildCard2(ActualFilename, FileSpec, ExcludeSpec, RecurseDirs) Then
         Begin
            With UBFI Do
            Begin
               DiskWithThisFile := 0;
               OffsetOfLocalHeader := CurrentPos;
               FileAttr := 32;
            End;
            HeaderList.AddItem(UBFI, Nil, 0);

            fTotalPackedSize := fTotalPackedSize + PackedSize;
            fTotalUnpackedSize := fTotalUnpackedSize + UnpackedSize;

            If ArcHeader.FileDate > fMaxAge Then
               fMaxAge := ArcHeader.FileDate;

         End;

         inc(CurrentPos, ArcHdr_Size + PackedSize);
         ZeroMemory(@ArcHeader, ArcHdr_Size);
      	Infile.Seek(CurrentPos, soBeginning);
         ReadBlock(Infile, Nil, ArcHeader, False, 0, ArcHdr_Size, dtHeader);
      End;
   Finally
      Result := HeaderList.FileCount;
   	ZipTimer.Resume();
   End;
End;
//-------------------------------------------------------------

Procedure TUnArc.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
Begin
   Try
      HeaderList := TUnBaseHeaderObj.Create();
      Try
         HeaderList.INIT();
         Try
            If BuildHeadArray(Infile) > 0 Then
               ProcessHeaders(Infile, Outfile);
         Finally
            HeaderList.DONE();
         End;
      Finally
         HeaderList.Free();
      End;
   Except
   End;
End;
//-------------------------------------------------------------

End.
