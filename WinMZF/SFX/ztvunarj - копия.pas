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
Unit ztvUnARJ;

Interface

Uses
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Dialogs,
   ztvBase,
   ztvHeaders,
   ztvStreams,
   ztvGbls;
   //ztvFileIo;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TUnArj = Class(TUnBASE)
   Private
      CurrentPos: u_long;
      SavePassword: String;
      IsFirstHeader: Boolean;
      IsVolumeArchive: Boolean;
      Function BuildHeadArray(Infile: TStream32): Integer;
		Function DecodeFast(Sender: TUnBASE; Infile, Outfile: TStream32): Boolean;
		Function OpenAndExtractFile(Infile, Outfile: TStream32;
      	FileAttr: Integer): Boolean;
      Function RequestPassword: Boolean;
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
      Property Password;
      //PROPERTY  Passwords;
      Property PasswordAttempts;
      Property DateAttribute;
      Property RestoreFileAttr;
      Property UseStoredDirs;
      Property OnActivate;
      Property OnBegin;
      Property OnDeactivate;
      Property OnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnGetPassword;
      Property OnProgress;
      Property OnNextVolume;
      Property OnRenameFile;
   End;

Implementation

Uses
   Err_Msgs,
   ztvLzh3;

Const
   STRTL = 0;
   STRTP = 9;
   STOPL = 7;
   STOPP = 13;
   PTABLESIZE = 256;
   //DDICSIZ   	   = 26624;

   FNAME_MAX = 512;
   FIRST_HDR_SIZE = 30;
   COMMENT_MAX = 2048;
   HEADERSIZE_MAX = FIRST_HDR_SIZE + 10 + FNAME_MAX + COMMENT_MAX;

   BIN_TYPE = 0;                        (* This must line up with binary/text strings *)
   TXT_TYPE = 1;
   CMNT_TYPE = 2;
   DIR_TYPE = 3;
   VOL_TYPE = 4;

   OS = 0;                              (* dos/windows *)
   ARJ_X_VERSION = 3;                   (* decoder version *)
   ARJ_M_VERSION = 6;                   (* ARJ version that supports modified date *)
   MAXMETHOD = 4;
   MAXSFX = 25000;
   HEADER_ID_HI = $EA;
   HEADER_ID_LO = $60;

   GARBLE_FLAG = $01;
   VOLUME_FLAG = $04;
   EXTFILE_FLAG = $08;
   PATHSYM_FLAG = $10;
   BACKUP_FLAG = $20;

   CRCPOLY = $EDB88320;
   HEADER_ID = $EA60;

   //-------------------------------------------------------------

Constructor TUnArj.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TUnArj.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TUnArj.DecodeFast(Sender: TUnBASE; Infile, Outfile: TStream32): Boolean;
Var
   getbuf: smallint;
   getlen: smallint;
   FileProgressPos: u_long;

   //-------------------------------------------------------------

   Procedure BFil;
   Begin
      getbuf := getbuf Or bitbuf Shr getlen;
      fillbuf(Sender, Infile, CODE_BIT - getlen);
      getlen := CODE_BIT;
   End;
   //-------------------------------------------------------------

   Procedure GetBits2(Var C: smallint; l: smallint);
   Begin
      If getlen < l Then BFil;
      C := word(getbuf) Shr (CODE_BIT - l);
      getbuf := getbuf Shl l;
      Dec(getlen, l);
   End;
   //-------------------------------------------------------------

   Procedure GetBit(Var C: smallint);
   Begin
      If getlen <= 0 Then BFil;
      C := smallint((getbuf And $8000) <> 0);
      getbuf := getbuf Shl 1;
      Dec(getlen);
   End;
   //-------------------------------------------------------------

   Function decode_len: smallint;
   Var
      C,
         Width,
         plus,
         pwr: smallint;
   Begin
      plus := 0;
      pwr := 1 Shl STRTL;
      For Width := STRTL To Pred(STOPL) Do
      Begin
         GetBit(C);
         If C = 0 Then break;
         inc(plus, pwr);
         pwr := pwr Shl 1;
      End;

      If Width <> 0 Then
         GetBits2(C, Width);

      inc(C, plus);
      Result := C;
   End;
   //-------------------------------------------------------------

   Function decode_ptr: smallint;
   Var
      C,
         Width,
         plus,
         pwr: smallint;
   Begin
      plus := 0;
      pwr := 1 Shl STRTP;
      For Width := STRTP To Pred(STOPP) Do
      Begin
         GetBit(C);
         If C = 0 Then break;
         inc(plus, pwr);
         pwr := pwr Shl 1;
      End;

      If Width <> 0 Then
         GetBits2(C, Width);

      inc(C, plus);
      Result := C;
   End;
   //-------------------------------------------------------------

   Function fwrite_txt_crc(p: PChar; n: Integer): Boolean;
   Var
      C: PChar;
      size: Integer;
   Begin
      With ArjHeader Do
         If (FileType = TXT_TYPE) Then
         Begin
            While (n > 0) Do
            Begin
               C := p;
               inc(p);
               Dec(n);

               If (HostOS <> OS) Then
                  //FIX_PARITY( c )
                  ;

               size := ExtractWriteBlock(Outfile, C, False, 32, SizeOf(C^), dtData);
               If size = 0 Then
                  RaiseError(E_RAISE, FileName, '', IntToStr(fVolNum), E_FWRITE);

               Dec(FileProgressPos, size);
               ProgressPosition := ProgressPosition - size;
               doBranchProgress(InflateRec.UnpackedSize - FileProgressPos, InflateRec.UnpackedSize, fTotalUnpackedSize);
            End;
         End
         Else
         Begin
            size := ExtractWriteBlock(Outfile, p[0], False, 32, n, dtData);
            If size = 0 Then
               RaiseError(E_RAISE, FileName, '', IntToStr(fVolNum), E_FWRITE);

            Dec(FileProgressPos, size);
            ProgressPosition := ProgressPosition - size;
            doBranchProgress(InflateRec.UnpackedSize - FileProgressPos, InflateRec.UnpackedSize, fTotalUnpackedSize);

         End;

      Result := True;
   End;
   //-------------------------------------------------------------

Var
   bCount: DWord;
   i, R: Integer;
   j, C, IPos: smallint;
Const
   THRESHOLD = 3;
Begin
   Result := False;

   //New(LzhInBuf);
   //New(LzhOutBuf);
   Try

      bitbuf := 0;
      init_getbits(Self, Infile);
      R := 0;
      getbuf := 0;
      getlen := 0;
      bCount := 0;
      FileProgressPos := ArjHeader.UnpackedSize;

      While (u_long(bCount) < ArjHeader.UnpackedSize) Do
      Begin
         C := decode_len;
         If C = 0 Then
         Begin
            GetBits2(C, smallint(CHAR_BIT));
            LzhOutBuf[R] := C;
            inc(bCount);
            inc(R);
            If (R >= WSIZE) Then
            Begin
               R := 0;
               If Not fwrite_txt_crc(@LzhOutBuf[0], WSIZE) Then
                  Exit;
            End;
         End
         Else
         Begin
            j := C - 1 + THRESHOLD;
            inc(bCount, j);
            IPos := decode_ptr;
            i := R - IPos - 1;

            If (i < 0) Then
               i := i + WSIZE;

            While (j > 0) Do
            Begin
               j := j - 1;
               LzhOutBuf[R] := LzhOutBuf[i];
               R := R + 1;
               If (R >= WSIZE) Then
               Begin
                  R := 0;
                  If Not fwrite_txt_crc(@LzhOutBuf[0], WSIZE) Then
                     Exit;
               End;

               i := i + 1;

               If (i >= WSIZE) Then
                  i := 0;
            End;
         End;
      End;

      If (R <> 0) Then
         If Not fwrite_txt_crc(@LzhOutBuf[0], R) Then
            Exit;

      Result := True;
   Finally
   	//If LzhInBuf <> Nil Then Dispose(LzhInBuf);
   	//If LzhOutBuf <> Nil Then Dispose(LzhOutBuf);
   End;
End;
//-------------------------------------------------------------

Function TUnArj.RequestPassword: Boolean;
Var
   Action: Boolean;
Begin

   // TUnARJ does not process a StringList of passwords (FPasswords)
   ZipTimer.Suspend;
   fPassword := SavePassword;

   Try

      If (fPassword = '') Then
      Begin
         If Assigned(OnGetPassword) Then
         Begin
            Action := True;
            OnGetPassword(Self, FileName, fPassword, Action);
            Result := Action;
         End
         Else
         Begin
            Result := False;
            RaiseErrorStr(FileName, 'OnGetPassword', '0', E_REQUIREDEVENT);
         End;
      End
      Else
         Result := True;

      If Result Then
      Begin
         fPassptr := 1;
         SavePassword := fPassword;
         fPassword := CPassword(0, fPassword, ArjHeader.xor_value);
      End;

   Finally
      ZipTimer.Resume;
   End;

End;
//-------------------------------------------------------------

Function TUnArj.OpenAndExtractFile(Infile, Outfile: TStream32;	FileAttr: Integer): Boolean;
Var
   i: Integer;
   PwdCanceled: Boolean;
Begin

   Result := False;
   PwdCanceled := False;

   fEncrypted := (InflateRec.BitFlag And PW_PROTECTED > 0);

   With ArjHeader Do
      For i := 1 To PasswordAttempts Do
      Begin

         If fEncrypted And (Not PasswordAttemptFailed) Then
            If Not RequestPassword() Then
            Begin
               PwdCanceled := True;
               PasswordAttemptFailed := True;
            End;

         Infile.Seek(pUBFI^.OffsetOfLocalHeader +
         				SizeOf(HeadId) +
                     SizeOf(HdrSize) +
                     HdrSize + 4 + 2,
                     soBeginning);

         If doOnBegin((FileType = DIR_TYPE) Or (FileType = VOL_TYPE)) Then
         Begin

            Crc32Val := CRC_MASK;
            Try
               If Open_OutFile(Outfile, FileName, ActualFilename) Then
                  Try
                     If (WriteMethod = faAppend) Then
                        WriteMethod := faFile;

                     If fEncrypted And PasswordAttemptFailed Then
                        AdjustProgress(InflateRec)
                     Else
                     Try

                        Case FileType Of
                           BIN_TYPE,
                              TXT_TYPE:
                              Begin
                                 Bytes_To_Go := PackedSize;

                                 Case CompressType Of

                                    0: Unstore(Infile, Outfile, 32, IntToStr(fVolNum), InflateRec);

                                    1..3:
                                       Begin
                                          If fEncrypted Then
                                             lzh_initialize(32, 16, 5, 2)
                                          Else
                                             lzh_initialize(32, 16, 5, 0);

                                          lzh_decode(Self, Infile, Outfile, InflateRec);
                                       End;

                                    4:
                                       Begin
                                          LzhBufSize := WSIZE;
                                          DecodeFast(Self, Infile, Outfile);
                                       End;

                                 Else
                                    RaiseError(E_RAISE, fFileName, '', IntToStr(fVolNum), E_UNKNTYPE);
                                 End;      (* case *)
                              End;         (* begin *)
                           DIR_TYPE:
                              Begin
                                 If CreateStoredDirs And WriteToFile Then
                                    CreateDirEx(AppendDirTail(ExtractFileDir(FileName)));

                                 Crc32Val := 0;
                              End;
                           VOL_TYPE: ;
                        Else
                           RaiseError(E_RAISE, fFileName, '', IntToStr(fVolNum), E_UNKNTYPE);
                        End;               (* case *)

                     Except
                     End;
                  Finally
                     If fEncrypted And PasswordAttemptFailed Then
                        fFileName := ActualFilename
                     Else
                        CloseAndSetDate(Outfile, fFileName, FileDate, 32);
                  End
               Else
               Begin
                  RaiseErrorStr(fFileName, '', '0', E_FOPEN);
                  AdjustProgress(InflateRec);
                  Dec(Count);
               End;

            Finally
               Result := doOnEnd(32, CRC32);
               If fEncrypted And (Not Result) Then
                  Dec(Count);
            End;

         End
         Else
            AdjustProgress(InflateRec);

         If Result Or (Not fEncrypted) Or PwdCanceled Then
            break;

         If fEncrypted Then
            SavePassword := '';

      End;                              (* PasswordAttempts *)

   If fEncrypted And (Not Result) Then
      PasswordAttemptFailed := True;

End;
//-------------------------------------------------------------

Procedure TUnArj.ProcessHeaders(Infile, Outfile: TStream32);
Var
   i: Integer;
   pFilename: PChar;
Begin

   GetMem(pFilename, 256);
   Try
      ProgressPosition := fTotalUnpackedSize;
      For i := 0 To HeaderList.FileCount - 1 Do
         With ArjHeader Do
         Begin
            If Cancel Then break;

            pUBFI := HeaderList.FileLocationData(i);
            With pUBFI^ Do
            Begin
         		Infile.Seek(OffsetOfLocalHeader, soBeginning);

               (* Read Header & Filename *)
               HeadId := 0;
               ReadBlock(Infile, Nil, ArjHeader, False, 0, SizeOf(ArjHeader), dtHeader);

               (* If valid header... *)
               If (HeadId = $EA60) And (HeadSize > 0) Then
               Begin
         			Infile.Seek(SUCC(OffsetOfLocalHeader) +
                  				SizeOf(HeadId) +
                              SizeOf(HeadSize) +
                     			HeadSize,
                              soBeginning);

                  ReadFilename(Infile, pFilename, 255);
                  ActualFilename := OemToCharFilter(StrPas(pFilename), fTransOemChar);
                  FileName := ActualFilename;

                  InflateRec.BitFlag := ArjFlag;
                  InflateRec.CompressType := CompressType;
                  InflateRec.PackedSize := PackedSize;
                  InflateRec.UnpackedSize := UnpackedSize;

                // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
                  GlobalDate := FileDate;

                  OpenAndExtractFile(Infile, Outfile, FileAttr);
               End
               Else
               Begin
                  RaiseErrorStr(FileName, '', IntToStr(fVolNum), E_BADHEADR);
                  break;
               End;
            End;
         End;
   Finally
      FreeMem(pFilename, 256);
   End;
End;
//-------------------------------------------------------------

Function TUnArj.BuildHeadArray(Infile: TStream32): Integer;
Var
   pFilename: PChar;
   SkipHeader: Boolean;
   UBFI: TUnBaseFileInfo;               //HeaderData

   Function ReadHeader: Boolean;
   Var
      BytesRead: Integer;
   Begin
      With ArjHeader Do
      Begin
         BytesRead :=
         	ReadBlock(Infile, Nil, ArjHeader, False, 0, SizeOf(ArjHeader), dtHeader);

         Result :=
         	(BytesRead = SizeOf(ArjHeader)) And (HeadId = $EA60) And (HeadSize > 0);
      End;
   End;

Begin

   ZipTimer.Suspend;
   IsFirstHeader := True;

   //rem'd v4.8.6
   //fOffsetBegin := fOffsetStart;

   GetMem(pFilename, 256);
   Try
      Infile.Seek(CurrentPos, soBeginning);

      While ReadHeader Do
         With ArjHeader Do
         Begin
            If Cancel Then break;

      		Infile.Seek(SUCC(CurrentPos) +
            				SizeOf(HeadId) +
                        SizeOf(HeadSize) +
                        HeadSize,
                        soBeginning);

            ReadFilename(Infile, pFilename, 255);

            If IsFirstHeader Then
            Begin
               VolumeName := ExtractFilePath(fVolumeName) + StrPas(pFilename);
               IsVolumeArchive := (ArjFlag And VOLUME_FLAG > 0);
            End
            Else
               ActualFilename := OemToCharFilter(StrPas(pFilename), fTransOemChar);

            If IsFirstHeader Then
               IsFirstHeader := False
            Else
            Begin

               If CheckWildCard2(ActualFilename, FileSpec, ExcludeSpec, RecurseDirs) Then
               Begin

                  SkipHeader := False;
                  If (MinVerNum > ARJ_X_VERSION) Then
                  Begin
                     RaiseErrorStr(ActualFilename, '', '0', E_VERSIONERR);
                     SkipHeader := True;
                  End;

                  If (CompressType > MAXMETHOD) Or ((CompressType = 4) And (VerNum = 1)) Then
                  Begin
                     RaiseErrorStr(ActualFilename, '', '0', E_UNKNMETH);
                     SkipHeader := True;
                  End;

                  If (Not SkipHeader) Then
                  Begin
                     With UBFI Do
                     Begin
                        DiskWithThisFile := 0;
                        OffsetOfLocalHeader := CurrentPos;
                        FileAttr := ExternalAttr;
                     End;
                     HeaderList.AddItem(UBFI, Nil, 0);

                     fTotalPackedSize := fTotalPackedSize + PackedSize;
                     fTotalUnpackedSize := fTotalUnpackedSize + UnpackedSize;
                     If FileDate > fMaxAge Then fMaxAge := FileDate;
                  End;

               End;

               inc(CurrentPos, PackedSize);
            End;

            inc(CurrentPos, SizeOf(HeadId) + SizeOf(HdrSize) + HdrSize + 4 + 2);
         	Infile.Seek(CurrentPos, soBeginning);
         End;

   Finally
      Result := HeaderList.FileCount;
      ZipTimer.Resume;
      FreeMem(pFilename, 256);
   End;
End;
//-------------------------------------------------------------

Procedure TUnArj.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
Var
   SaveArchiveFile: String;
Begin
   SaveArchiveFile := fArchiveFile;
   SavePassword := '';
   fPassword := '';
   If PasswordAttempts > 10 Then
      PasswordAttempts := 10;

   Try
      HeaderList := TUnBaseHeaderObj.Create();
      Try
         HeaderList.INIT();
         Try
   			//rem'd v4.8.6
            //CurrentPos := fOffsetBegin - 1;
            CurrentPos := fOffsetStart;

            With ArjHeader Do
               Repeat

                  If BuildHeadArray(Infile) > 0 Then
                     ProcessHeaders(Infile, Outfile);

                  If ArjFlag And VOLUME_FLAG > 0 Then
                  Begin
                  	Infile.Free();

                     fVolumeName := GetNextVolumeName(fVolumeName, fVolNum);
                     If GetNextVolume(fVolumeName, fVolNum) Then
                     Begin
                        If (WriteMethod = faFile) Then
                           WriteMethod := faAppend;

                        Infile :=
                           TFileStream32.Create(fVolumeName, fmOpenRead Or
                              fmShareDenyNone);

                        If (TFileStream32(Infile).Handle < 0) Then
                        Begin
                           ArjFlag := 0;
                           fLOF := 0;
                           fOffsetEnd := 0;
                           RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN);
                           Exit;
                        End;

                        fLOF := Infile.Size; //ztvGetFileSize(Infile);
                        fOffsetEnd := fLOF;

                     End Else Begin

                        If Cancel Then
                        	Exit;

                        Infile :=
                           TFileStream32.Create(fArchiveFile, fmOpenRead Or
                              fmShareDenyNone);

                        If (TFileStream32(Infile).Handle < 0) Then
                        Begin
                           ArjFlag := 0;
                           fLOF := 0;
                           fOffsetEnd := 0;
                           RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN);
                           Break;
                        End;

                     End;
                  End;

                  CurrentPos := 0;
                  HeaderList.CLEAR_LIST();
                  fTotalUnpackedSize := 0;
               Until ArjFlag And VOLUME_FLAG = 0;

         Finally
            HeaderList.DONE();
         End;
      Finally
         fArchiveFile := SaveArchiveFile;
         HeaderList.Free();
      End;
   Except
   End;
End;
//-------------------------------------------------------------

End.
