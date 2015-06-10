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
Unit ztvUnZoo;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   ztvBase,
   ztvHeaders,
   ztvStreams;

{$I ZipTV.Inc}                          //Declare the compiler defines

Type
   TUnZoo = Class(TUnBASE)
   Private
      //ZooHeader: TZooDir;
      //ZooDirHeader: TZooDirEntry;
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
      Property ExtractDir;
      Property FileSpec;
      Property OverwriteMode;
      Property RestoreFileAttr;
      Property DateAttribute;
      Property UseStoredDirs;
      Property OnActivate;
      Property OnDeactivate;
      Property OnProgress;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnRenameFile;
   End;

Implementation

Uses
   ztvGbls,
   ztvLzh3,
   Err_Msgs;

Const
   ZOO_LO_TAG = $A7DC;
   ZOO_HI_TAG = $FDC4;
   SPARE = 4;
   STACKSIZE = 2000;
   MEM_BLOCK_SIZE = 8192;

Const
   MASKS: Array[0..13] Of word = (0, 0, 0, 0, 0, 0, 0, 0, 0, $1FF, $3FF, $7FF, $FFF, $1FFF);
   maxbits = 13;
   MAXMAX = 8192;                       (* max code + 1 *)
   IN_BUF_SIZE = 4096;
   OUT_BUF_SIZE = 4096;
   INBUFSIZ = (IN_BUF_SIZE - SPARE);
   OUTBUFSIZ = (OUT_BUF_SIZE - SPARE);
   FIRST_FREE = 258;                    (* first free code *)
   Clear = 256;                         (* clear code *)
   Z_EOF = 257;                         (* end of file marker *)

Type
   TabEntry = Packed Record
      Next: word;
      z_ch: char;
   End;

   LzdStackPtr = ^LzdStackType;
   LzdStackType = Array[0..STACKSIZE + SPARE] Of Byte;
   outbufptr = ^OutbufType;
   OutbufType = Array[0..MEM_BLOCK_SIZE] Of Byte;

Var
   TABLE: Array[0..MAXMAX + 1] Of TabEntry;
   InbufAddr: ^Byte;
   OutputOffset: Integer;
   NBits: smallint;
   fin_char: smallint;
   MAXCODE: word;
   FreeCode: word;
   BitOffset: word;
   LzdSp: word;
   incode: word;
   CurrentCode: word;
   oldcode: word;
   ZooLocal: TZooDir;
   NextPtr: Integer;
   LzdStack: LzdStackPtr;
   OutbufAddr: outbufptr;

   //-------------------------------------------------------------

Constructor TUnZoo.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TUnZoo.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure lzd(Sender: TUnBASE; Infile: TStream32; Var Outfile: TStream32);
Var
   k: Byte;
   FileProgressPos: u_long;
   //-----------------------------------------------------------

   Procedure init_dtab;
   Begin
      NBits := 9;
      MAXCODE := 512;
      FreeCode := FIRST_FREE;
   End;
   //-----------------------------------------------------------
   (* rd_dcode reads code from input file and returns its value *)

   Function rd_dcode: word;
   Var
      PtrA, PtrB: ^Byte;                (* miscellaneous pointers 	*)
      Worad: word;                      (* first 16 bits in buffer *)
      ByteOffset: word;
      NextChar: Byte;                   (* Next 8 bits in buffer	*)
      OffsetInByte: word;               (* offset within Byte		*)
      BufSize,
      	SpaceLeft: Integer;
   Begin
      OffsetInByte := BitOffset Mod 8;
      ByteOffset := BitOffset Div 8;
      BitOffset := BitOffset + NBits;

      If (ByteOffset >= INBUFSIZ - 5) Then
      Begin
         BitOffset := OffsetInByte + NBits;
         SpaceLeft := INBUFSIZ - ByteOffset;

         PtrB := @OutbufAddr^[OUT_BUF_SIZE];
         inc(PtrB, ByteOffset);

         PtrA := @OutbufAddr^[OUT_BUF_SIZE];

         (* we now move the remaining characters	*)
         (* down buffer beginning 						*)
         While (SpaceLeft > 0) Do
         Begin
            PtrA^ := PtrB^;
            inc(PtrA);
            inc(PtrB);
            Dec(SpaceLeft);
         End;

         // use the following block, instead of the min function...
         // the min function fails with files > 4 gig.
         //BufSize := Min(Sender.Bytes_To_Go, ByteOffset);
         If Sender.Bytes_To_Go > ByteOffset Then
            BufSize := ByteOffset
         Else
            BufSize := Sender.Bytes_To_Go;

         Sender.ReadBlock(Infile, Nil, PtrA^, False, 0, BufSize, dtHeader);
         ByteOffset := 0;
      End;

      PtrA := @OutbufAddr^[OUT_BUF_SIZE];
      inc(PtrA, ByteOffset);
      Worad := word(PtrA^);

      inc(PtrA);
      Worad := Worad Or (PtrA^) Shl 8;

      inc(PtrA);
      NextChar := PtrA^;
      (* shift NextChar right by OffsetInByte bits *)
      (* and shift those bits right into Word; *)
      If (OffsetInByte <> 0) Then
         Worad := (Worad Shr OffsetInByte) Or (NextChar Shl (16 - OffsetInByte));

      Result := (Worad And MASKS[NBits]);
   End;
   //-----------------------------------------------------------

   Procedure wr_dchar(ch: Byte);
   Begin
      (* if buffer full *)
      If (OutputOffset >= OUTBUFSIZ) Then
      Begin
         OutputOffset := Sender.ExtractWriteBlock(Outfile, OutbufAddr^, False, 16, OutputOffset, dtData);
         If OutputOffset = 0 Then
            Raise E_RAISE.Create(LoadStr(E_FWRITE));

         With Sender Do
         Begin
            ProgressPosition := ProgressPosition - OutputOffset;
            Dec(FileProgressPos, OutputOffset);
            doBranchProgress(InflateRec.UnpackedSize - FileProgressPos,
               InflateRec.UnpackedSize, fTotalUnpackedSize);
         End;

         (* restore empty buffer *)
         OutputOffset := 0;
      End;
      (* store character *)
      OutbufAddr^[OutputOffset] := ch;
      inc(OutputOffset);
   End;
   //-----------------------------------------------------------

   Procedure Push(ch: char);
   Begin
      LzdStack^[LzdSp] := word(ch);
      LzdSp := LzdSp + 1;
      If (LzdSp >= STACKSIZE) Then
         //prterror ( 'f', "Stack overflow IN lzd( )\n", ( Char *) 0, ( Char *) 0 );
   End;
   //-----------------------------------------------------------

   Function Pop: Byte;
   Begin
      LzdSp := LzdSp - 1;
      Result := LzdStack^[LzdSp];
   End;
   //-----------------------------------------------------------

   Procedure ad_dcode(k: Byte);         (* adds a code to Table *)
   Begin
      TABLE[FreeCode].z_ch := char(k);  (* save suffix Char *)
      TABLE[FreeCode].Next := oldcode;  (* save prefix code *)
      FreeCode := FreeCode + 1;
      If (FreeCode >= MAXCODE) Then
      Begin
         If (NBits < maxbits) Then
         Begin
            NBits := NBits + 1;
            MAXCODE := MAXCODE Shl 1;   (* double MaxCode *)
         End;
      End;
   End;
   //-----------------------------------------------------------
Var
	BufSize: Integer;
Begin
   LzdStack := Nil;
   OutbufAddr := Nil;
   Try
      New(LzdStack);
      New(OutbufAddr);
      Try

         FileProgressPos := Sender.Bytes_To_Go;

         InbufAddr := @OutbufAddr^[OUT_BUF_SIZE];

         NBits := 9;
         LzdSp := 0;

         BitOffset := 0;
         OutputOffset := 0;
         MAXCODE := 512;
         FreeCode := FIRST_FREE;

         // use the following block, instead of the min function...
         // the min function fails with files > 4 gig.
         //BufSize := Min(Sender.Bytes_To_Go, INBUFSIZ);
         If Sender.Bytes_To_Go > INBUFSIZ Then
            BufSize := INBUFSIZ
         Else
            BufSize := Sender.Bytes_To_Go;

         //BytesRead :=
         	Sender.ReadBlock(Infile, Nil, InbufAddr^, False, 0, BufSize, dtHeader);

         init_dtab();                   (* initialize Table *)

         Repeat
            CurrentCode := rd_dcode;

            If (CurrentCode = Z_EOF) Then
            Begin

               If (OutputOffset <> 0) Then
               Begin
                  OutputOffset := Sender.ExtractWriteBlock(Outfile, OutbufAddr^, False, 16, OutputOffset, dtData);
                  If OutputOffset = 0 Then
                     Raise E_RAISE.Create(LoadStr(E_FWRITE));

                  With Sender Do
                  Begin
                     ProgressPosition := ProgressPosition - OutputOffset;
                     Dec(FileProgressPos, OutputOffset);
                     doBranchProgress(InflateRec.UnpackedSize - FileProgressPos,
                        InflateRec.UnpackedSize, fTotalUnpackedSize);
                  End;
               End;

               break;
            End;

            If (CurrentCode = Clear) Then
            Begin
               init_dtab;
               CurrentCode := rd_dcode;
               fin_char := CurrentCode;
               k := CurrentCode;
               oldcode := CurrentCode;
               wr_dchar(k);
               Continue;
            End;

            incode := CurrentCode;
            If (CurrentCode >= FreeCode) Then (* if code not in Table ( k<w>k<w>k ) *)
            Begin
               CurrentCode := oldcode;  (* previous code becomes current *)
               Push(char(fin_char));
            End;

            While (CurrentCode > 255) Do (* if code, not character *)
            Begin
               Push(TABLE[CurrentCode].z_ch); (* Push suffix Char *)
               CurrentCode := TABLE[CurrentCode].Next; (* <w> := <w>.code *)
            End;

            fin_char := CurrentCode;
            k := CurrentCode;
            Push(char(k));

            While (LzdSp <> 0) Do
               wr_dchar(Pop());

            ad_dcode(k);
            oldcode := incode;
         Until 0 > 1;
      Finally
         If LzdStack <> Nil Then
            dispose(LzdStack);

         If OutbufAddr <> Nil Then
            dispose(OutbufAddr);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Function TUnZoo.OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
Begin

   Result := False;
   With ZooDirHeader Do
      If doOnBegin(False) Then
      Begin
         Crc16Val := 0;
         Try
            Try
               If Open_OutFile(Outfile, FileName, ActualFilename) Then
               Begin
                  Try

                     Bytes_To_Go := PackedSize;
                     //seek( InFile, ZooDirHeader.Offset );

                     Case CompressType Of
                        0: Unstore(Infile, Outfile, 16, '0', InflateRec);
                        1: lzd(Self, Infile, Outfile);
                        2:
                           Begin
                              lzh_initialize(16, 13, 4, 0);
                              lzh_decode(Self, Infile, Outfile, InflateRec);
                           End;
                     Else
                        RaiseError(E_RAISE, fFileName, '', '0', E_UNKNMETH);
                     End;
                  Finally
                     CloseAndSetDate(Outfile, fFileName, FileDate, 32);
                  End
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

Procedure TUnZoo.ProcessHeaders(Infile, Outfile: TStream32);
Var
   i: Integer;
   //Dir              : String;  v4.01.02
   //pFilename        : PChar;	v4.01.02
Begin
   //GetMem( pFilename, 256 );	v4.01.02
   //Try									v4.01.02
   ProgressPosition := fTotalUnpackedSize;
   For i := 0 To HeaderList.FileCount - 1 Do
      With ZooDirHeader Do
      Begin
         If Cancel Then break;

         pUBFI := HeaderList.FileLocationData(i);
         With pUBFI^ Do
         Begin
         	Infile.Seek(OffsetOfLocalHeader, soBeginning);

               (* Read Header & Filename *)
            ReadBlock(Infile, Nil, ZooDirHeader, False, 0, ZooDirHdr_Size, dtHeader);
            If Next < 1 Then break;

            FileDate := SwapWords(FileDate);

               // v4.01.02: rem'd... see 1.zoo.  No stored path support
               //Dir := '';
               //If DirLen > 0 Then
               //Begin
               //   If ReadFilename( InFile, pFilename, DirLen ) = DirLen Then
               //      Dir := AppendDirTail( UnixToDosFilename( StrPas( pFilename ) ) );
               //End;

               // v4.01.02: changed
               //ActualFilename := Dir + StrPas( @ZooDirHeader.ZooFilename[1] );
            ActualFilename := StrPas(@ZooDirHeader.ZooFilename[1]);

            ActualFilename := OemToCharFilter(ActualFilename, fTransOemChar);
            FileName := ActualFilename;

            If (lo_tag <> ZOO_LO_TAG) Or (hi_tag <> ZOO_HI_TAG) Then
            Begin
               RaiseErrorStr(ActualFilename, '', '0', E_BADHEADR);
               Continue;
            End;

            InflateRec.BitFlag := 0;
            InflateRec.CompressType := CompressType;
            InflateRec.PackedSize := PackedSize;
            InflateRec.UnpackedSize := UnpackedSize;

             // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
            GlobalDate := FileDate;

         	Infile.Seek(ZooDirHeader.Offset, soBeginning);
            OpenAndExtractFile(Infile, Outfile, FileAttr);
         End;
      End;
   //Finally
   //   FreeMem( pFilename, 256 );
   //End;
End;
//-------------------------------------------------------------

Function TUnZoo.BuildHeadArray(Infile: TStream32): Integer;

   Function ReadFirst: Boolean;
   Begin
      Infile.Seek(0, soBeginning);
      Result := ReadBlock(Infile, Nil, ZooLocal, False, 0, ZooHdr_Size,
      	dtHeader) = ZooHdr_Size;

      If Result Then
         With ZooLocal Do
         Begin
            NextPtr := START;
            Result := (START + Minus) = 0;
         End;
   End;

   Function ReadNext: Boolean;
   Begin
      Infile.Seek(NextPtr, soBeginning);
      Result := ReadBlock(Infile, Nil, ZooDirHeader, False, 0, ZooDirHdr_Size,
      	dtHeader) = ZooDirHdr_Size;

      If Result Then
         With ZooDirHeader Do
         Begin
            FileDate := SwapWords(FileDate);

            If (lo_tag <> ZOO_LO_TAG) Or (hi_tag <> ZOO_HI_TAG) Then
               RaiseError(E_RAISE, ArchiveFile, '', '0', E_BADHEADR);

            Result := (Next <> 0);
         End;
   End;
Var
   pFilename: PChar;
   UBFI: TUnBaseFileInfo;               //HeaderData
Begin

   ZipTimer.Suspend();

   GetMem(pFilename, 256);
   Try
      If ReadFirst Then
         With ZooDirHeader Do
            While ReadNext Do
            Begin
               If Cancel Then break;

               // v4.1.5: removed dir support
               //ReadFilename( Infile, pFilename, DirLen );
               //If ( ZooDirHeader.DirLen > 0 ) Then
               //	fFilename := AppendDirTail( UnixToDosFilename(StrPas(pFilename)) )
               //Else
               //	fFilename := '';
               //
               //fFilename := fFilename + StrPas( @ZooFilename[1] );
               //ActualFilename := OemToCharFilter( fFilename, fTransOemChar );
               fFileName := UnixToDosFilename(StrPas(@ZooFilename[1]));
               ActualFilename := OemToCharFilter(fFileName, fTransOemChar);

               If CheckWildCard2(ActualFilename, FileSpec, ExcludeSpec,
                  RecurseDirs) Then
               Begin
                  With UBFI Do
                  Begin
                     DiskWithThisFile := 0;
                     OffsetOfLocalHeader := NextPtr;
                     FileAttr := FILE_ATTRIBUTE_NORMAL;
                  End;
                  HeaderList.AddItem(UBFI, Nil, 0);

                  fTotalPackedSize := fTotalPackedSize + PackedSize;
                  fTotalUnpackedSize := fTotalUnpackedSize + UnpackedSize;

                  If FileDate > fMaxAge Then
                     fMaxAge := FileDate;

               End;

               NextPtr := Next;
            End;
   Finally
      Result := HeaderList.FileCount;
      ZipTimer.Resume();
      FreeMem(pFilename, 256);
   End;
End;
//-------------------------------------------------------------

Procedure TUnZoo.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
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
