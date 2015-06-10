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
Unit ztvUUEncode;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   ztvBase,
   ztvGbls,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines


Type
   TUUEncode = Class(TCompBase)
   Private
      fMode: Integer;
      fKBytesPerVolume: Integer;
      FVolumeLongFilenames: Boolean;
      FNextVolumeName: TNextVolumeName;
      fOutFilename: String;
      fWriteTableToFile: Boolean;
      Function encode_file(Infile, Outfile: TStream32; Var fSize: Integer):
         Integer;
      Procedure EncodeIT(Infile, Outfile: TStream32);
      Procedure SetExtension(SE: String);
   Protected
      Procedure CompressIT(pHeaderObj: pCompHeaderObj); Override;
      Function GetHeadSize: word; Override;
      Procedure SetArchiveFile(SFN: String); Override;
      Procedure SetArcType(SAT: TArcType); Override;
   Public
      Procedure Loaded; Override;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
   Published
      Property ArcType;
      Property CompressMethod;
      Property DefaultExt;
      Property KBytesPerVolume: Integer Read fKBytesPerVolume Write fKBytesPerVolume;
      Property OutFilename: String Read fOutFilename Write SetExtension;
      Property WriteTableToFile: Boolean Read fWriteTableToFile Write fWriteTableToFile;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnNonWriteableArchive;
      Property OnProgress;
   End;

Implementation

Uses
   Err_Msgs;

Const
   //iUUlen            = 77;               (* 'M' *)
   //EOF               = -1;
   //UNDEF_CH          = -2;               (* undefined ch value in character map *)
   MAXLINELEN = 80;
   BIT6MASK = $3F;
   INLEN = 45;                          (* # chars in line to be encoded, must be multiple of 3 *)
   INLEN_B64 = 57;
   OUTLEN = 60;                         (* INLEN original bytes is OUTLEN encoded bytes *)
   system_id = 'ZipTV components: www.ziptv.com';
   EOL_STR = #13#10;                    (* end of line marking *)

   UUETABLE =
      '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'; //0-64
   XXETABLE =
      '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'; //0-63
   B64TABLE =
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'; //0-63

Var
   sMethod: Array[0..8] Of Char;
   sXXtable: Array[0..64] Of Char;
   InLength: Integer;

   //iXXlen,                               (* 'h' *)
   BytesRead,
      InBufPos,
      CurInbufPos,
      OutBufPos,
      cLine: Integer;
   abLine,
      inbuf,
      outbuf: PChar;
   MaxMem: word;
   EndOfFile: Boolean;

   //-------------------------------------------------------------

Constructor TUUEncode.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fMasterExt := '.UUE';
   DefaultExt := fMasterExt;
   FNextVolumeName := nvChangeExt;
   FVolumeLongFilenames := True;
   CompressMethodState := [cmXXEncode, cmUUEncode, cmBase64];
   CompressMethod := cmUUEncode;
   fArcType := atNA;
End;
//-------------------------------------------------------------

Destructor TUUEncode.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TUUEncode.Loaded;
Begin
   Inherited Loaded;
End;
//-------------------------------------------------------------

Procedure TUUEncode.SetArchiveFile(SFN: String);
Begin
   If (csLoading In ComponentState) Then
      If Not FileExists(SFN) Then
      Begin
         SetLength(fArchiveFile, 0);
         Exit;
      End;

   //CheckLoadingState( SFN );
   SFN := UnixToDosFilename(SFN);

   fArchiveFile := SFN;

   fOutFilename := Copy(SFN, 1, Length(SFN) -
      Length(ExtractFileExt(SFN))) + fDefaultExt;

   //rem'd v4.8.6
   //fOffsetBegin := 1;
   fOffsetStart := 0;
   pCancel^ := False;
End;
//-------------------------------------------------------------

Procedure TUUEncode.SetArcType(SAT: TArcType);
Begin
   If fArchiveFile = '' Then
      fArcType := atNA
   Else
   Begin
      fArcType := atUUE;

      If Not (csLoading In ComponentState) Then
         If ExtractFileExt(fArchiveFile) = '' Then
            fArchiveFile := fArchiveFile + fDefaultExt;
   End;
End;
//-------------------------------------------------------------

Procedure TUUEncode.SetExtension(SE: String);
Begin
   If Pos('.', SE) = 0 Then
      SE := SE + fDefaultExt;

   If SE <> '.' Then
      fOutFilename := SE
   Else
      fOutFilename := '';
End;
//-------------------------------------------------------------

Function TUUEncode.encode_file(Infile, Outfile: TStream32; Var fSize: Integer):
   Integer;
//-------------------------------------------------------------

   Procedure WriteBuf(Buf: PChar; Len: Integer);
   Begin
      If OutBufPos + Len > MaxMem Then
      Begin
         If CompressWriteBlock(Outfile, outbuf^, False, 0,
            OutBufPos, dtData) <> Cardinal(OutBufPos) Then doWriteError;

         fTotalPackedSize := fTotalPackedSize + OutBufPos;
         OutBufPos := 0;
      End;

      CopyMem(@Buf[0], @outbuf[OutBufPos], Len);
      Inc(OutBufPos, Len);
   End;
   //-------------------------------------------------------------

   Function fget(abLine: PChar; Len: Integer): Integer;
      //-------------------------------------------------------------

      Function cGetNextChar: Byte;
      Var
      	BufSize: Integer;
      Begin
         If CurInbufPos + 1 >= BytesRead Then
         Begin

            // use the following block, instead of the min function...
            // the min function fails with files > 4 gig.
            //Result := Min(Bytes_To_Go, WSIZE);
            If Bytes_To_Go > WSIZE Then
               BufSize := WSIZE
            Else
               BufSize := Bytes_To_Go;

            BytesRead :=
            	ReadBlock(Infile, Nil, inbuf[0], False, 32, BufSize, dtData);

            If BytesRead = 0 Then
            Begin
               Bytes_To_Go := 0;
               EndOfFile := True;
               Result := 0;             //EOF;
               Exit;
            End;

            Dec(Bytes_To_Go, BytesRead);
            ProgressPosition := ProgressPosition - BytesRead;
            doBranchProgress(fLOF - Bytes_To_Go, fLOF, fLOF);

            fTotalUnpackedSize := fTotalUnpackedSize + BytesRead;
            CurInbufPos := 0;
         End
         Else
            Inc(CurInbufPos);

         //Dec( Bytes_To_Go );
         //PercentByFile := CalcProgress( InBufPos, fLOF );
         //DoProgress( PercentByFile, PercentByFile );

         Inc(InBufPos);
         Result := Byte(inbuf[CurInbufPos]);
      End;
      //-------------------------------------------------------------
   Var
      C, i: Integer;
   Begin

      i := 0;
      While i <= Pred(Len) Do
      Begin
         C := cGetNextChar();
         If Not EndOfFile {Bytes_To_Go > 0} Then
            abLine[i] := Char(C)
         Else
         Begin
            //Dec( i );
            //Inc( i );
            break;
         End;
         Inc(i);
      End;

      //IF c = EOF THEN Dec( i );
      abLine[i] := #0;
      Result := i;
   End;
Var
   Buf: PChar;
   i,
      j,
      Len,
      chbytes3,
      mxlines,
      nbytes3: Integer;                 (* # decoded bytes (so 4 * nbytes / 3 when encoded) *)
Begin                                   {encode_file}
   mxlines := KBytesPerVolume * 16;

   If (CompressMethod = cmBase64) Then
   Begin
      Buf := @abLine[0];
      InLength := INLEN_B64;
   End
   Else
   Begin
      Buf := @abLine[1];                (* 1st Char of abLine is quant., rest is buffer *)
      InLength := INLEN;
   End;

   chbytes3 := Byte(sXXtable[InLength {INLEN}]);

   Repeat

		If Cancel Then
      Begin
      	Result := 0;
      	Exit;
      End;

      (* get INLEN bytes from the input file *)
      nbytes3 := fget(Buf, InLength {INLEN});

      fSize := fSize + nbytes3;

      If (nbytes3 = InLength {INLEN}) And (CompressMethod <> cmBase64) Then
      Begin
         abLine[0] := Char(chbytes3);
         Len := OUTLEN;
      End
      Else
      Begin
         (* store quantity Char *)
         If (CompressMethod <> cmBase64) Then
            abLine[0] := sXXtable[nbytes3];

         For i := nbytes3 To Pred(InLength {INLEN}) Do
            Buf[i] := Char(0);

         (* take ceiling of div *)
         nbytes3 := (nbytes3 + 2) Div 3;

         (* # of encoded bytes *)
         Len := 4 * nbytes3;

         (* translate from 3-byte packets to bytes *)
         nbytes3 := nbytes3 * 3;

      End;

      (* encode *)
      i := nbytes3 - 3;
      j := Len - 4;
      While i >= 0 Do
      Begin
         Buf[j + 3] := sXXtable[Byte(Buf[i + 2]) And BIT6MASK];
         Buf[j + 2] := sXXtable[((Byte(Buf[i + 1]) Shl 2) + (Byte(Buf[i + 2]
            ) Shr 6)) And BIT6MASK];
         Buf[j + 1] := sXXtable[((Byte(Buf[i]) Shl 4) + (Byte(Buf[i + 1])
            Shr 4)) And BIT6MASK];
         Buf[j] := sXXtable[(Byte(Buf[i]) Shr 2) And BIT6MASK];
         Dec(i, 3);
         Dec(j, 4);
      End;

      {ifdef WIN95}
      Buf[Len] := #13;                  (* file opened binary --> #10#13 for MSDOS/Windows *)
      Inc(Len);
      {endif}
      Buf[Len] := #10;                  (* add end of line Character *)
      Inc(Len);

      If (CompressMethod = cmBase64) Then
         WriteBuf(abLine, Len)
      Else
         WriteBuf(abLine, Len + 1);

      Inc(cLine);                       (* increment linecounter *)

   Until (nbytes3 <= 0) Or (cLine = mxlines);

   If OutBufPos > 0 Then
   Begin
      If CompressWriteBlock(Outfile, outbuf^, False,
         0, OutBufPos, dtData) <> Cardinal(OutBufPos) Then doWriteError;

      OutBufPos := 0;
   End;
   Result := nbytes3;
End;
//-------------------------------------------------------------

Function GetContentType(Const FileName: String): String;
Var
   Ext: String[4];
Begin
   Ext := UpperCase(ExtractFileExt(FileName));
   If Ext = '.HTM' Then
   	Result := 'text/html'
   Else If Ext = '.TXT' Then
   	Result := 'text/plain; Charset=US-ASCII'
   Else If Ext = '.GIF' Then
   	Result := 'image/gif'
   Else If Ext = '.JPG' Then
   	Result := 'image/jpeg'
   Else If Ext = '.AIF' Then
      Result := 'audio/aiff'
   Else If (Ext = '.AU') Or (Ext = '.SND') Then
   	Result := 'audio/basic'
   Else If Ext = '.AVI' Then
   	Result := 'video/avi'
   Else If Ext = '.RTF' Then
   	Result := 'text/rtf'
   Else
   	Result := 'application/octet-stream';
End;
//-------------------------------------------------------------

Procedure TUUEncode.EncodeIT(Infile, Outfile: TStream32);
Var
	inHandle: THandle;
   fnum,
   	fSize,
      BytesToWrite: Integer;
   str, sOutfile, sVersion: String;
Begin
   fnum := 0;
   fSize := 0;

   CurInbufPos := 0;
   InBufPos := 0;
   OutBufPos := 0;
   BytesRead := 0;
   EndOfFile := False;

   ReadMethod := faFile;
   WriteMethod := faFile;

   Try

      //ArchiveFile := 'c:\hold1\cs110ps2.xls';
      //ArchiveFile := 'c:\windows\support.txt';
      //OutFilename := 'c:\hold1\support.mim';
      //CompressMethod := cmBase64;

      inHandle := TFileStream32(Infile).Handle;
      If Not OpenArchive(inHandle, fArchiveFile) Then
         RaiseError(E_RAISE, fArchiveFile, '', '0', E_FOPEN);

      fLOF := Infile.Size;

      Bytes_To_Go := fLOF;
      ProgressPosition := fLOF;

      // use the following block, instead of the min function...
      // the min function fails with files > 4 gig.
      //MaxMem := Min(Bytes_To_Go, WSIZE + 1);
      If Bytes_To_Go > WSIZE + 1 Then
         MaxMem := WSIZE + 1
      Else
         MaxMem := Bytes_To_Go;

      GetMem(inbuf, MaxMem + 1);
      GetMem(outbuf, MaxMem + 1);
      GetMem(abLine, MAXLINELEN + 1);

      Case CompressMethod Of
         cmBase64:
            Begin
               fMasterExt := '.MIM';
               DefaultExt := fMasterExt;
               sMethod := 'B64Encode';
               sXXtable := B64TABLE;
               fMode := 0664;
               //iXXlen := iUUlen;           (* M *)
            End;
         cmUUEncode:
            Begin
               fMasterExt := '.UUE';
               DefaultExt := fMasterExt;
               sMethod := 'UUEncode ';
               sXXtable := UUETABLE;
               //iXXlen := iUUlen;           (* M *)
               fMode := 0644;
            End;
         cmXXEncode:
            Begin
               fMasterExt := '.XXE';
               fDefaultExt := fMasterExt;
               sMethod := 'XXEncode ';
               sXXtable := XXETABLE;
               //iXXlen := 104;              (* 'h' *)
               fMode := 0644;
            End;
      End;

      Outfile :=
      	TFileStream32.Create(OutFilename, fmOpenWrite);

      //Outfile := ztvOpenFileWrite(Self, PChar(OutFilename), True);
      //If Outfile = INVALID_HANDLE_VALUE Then
      //   RaiseError(E_RAISE, OutFilename, '', '0', E_FOPEN);

      Try

         If ExtractFileExt(fArchiveFile) = '' Then
            fArchiveFile := fArchiveFile + fDefaultExt;

         sVersion := IntToStr(FVersionMin) + '.' + IntToStr(FVersionMax);
         str := sMethod + ' ' + sVersion + ' (' + system_id + ')' +
            EOL_STR + EOL_STR;

         BytesToWrite := Length(str);
         If CompressWriteBlock(Outfile, str[1], False,
            0, BytesToWrite, dtHeader) <> Cardinal(BytesToWrite) Then
            doWriteError;

         If fWriteTableToFile Then
         Begin
            str := 'table' + EOL_STR;
            If CompressWriteBlock(Outfile, str[1], False,
               0, 7, dtHeader) <> Cardinal(7) Then doWriteError;

            If CompressWriteBlock(Outfile, sXXtable[0], False,
               0, 32, dtHeader) <> Cardinal(32) Then doWriteError;

            str := EOL_STR;
            If CompressWriteBlock(Outfile, str[1], False,
               0, 2, dtHeader) <> Cardinal(2) Then doWriteError;

            If CompressWriteBlock(Outfile, sXXtable[32], False,
               0, 32, dtHeader) <> Cardinal(32) Then doWriteError;

            str := EOL_STR;
            If CompressWriteBlock(Outfile, str[1], False,
               0, 2, dtHeader) <> Cardinal(2) Then doWriteError;
         End;

         If (CompressMethod = cmBase64) Then
         Begin

            str := 'Content-Type: ' + GetContentType(fArchiveFile) + '; name="' +
               ExtractFilename(fArchiveFile) + '";'#13#10; // +
            //' x-mac-type="584C5334"; x-mac-creator="5843454C"' + #13#10;

            BytesToWrite := Length(str);
            If CompressWriteBlock(Outfile, str[1], False, 0, BytesToWrite,
               dtHeader) <> Cardinal(BytesToWrite) Then
               doWriteError;

            str := 'Content-Transfer-Encoding: base64' + #13#10;
            BytesToWrite := Length(str);
            If CompressWriteBlock(Outfile, str[1], False, 0, BytesToWrite,
               dtHeader) <> Cardinal(BytesToWrite) Then
               doWriteError;

            str := 'Content-Disposition: attachment; filename="' +
               ExtractFilename(fArchiveFile) + '"'#13#10#13#10;
            BytesToWrite := Length(str);
            If CompressWriteBlock(Outfile, str[1], False, 0, BytesToWrite,
               dtHeader) <> Cardinal(BytesToWrite) Then
               doWriteError;

         End
         Else
         Begin
            str := 'begin ' + IntToStr(fMode) + ' ' +
               ExtractFilename(fArchiveFile) + EOL_STR;

            BytesToWrite := Length(str);
            If CompressWriteBlock(Outfile, str[1], False, 0, BytesToWrite,
               dtHeader) <> Cardinal(BytesToWrite) Then
               doWriteError;

         End;

         cLine := 3;                    (* current line number in input file *)
         OutBufPos := 0;

         While encode_file(Infile, Outfile, fSize) > 0 Do
         Begin

            If Cancel Then Exit;

            If DateAttribute = daFileDate Then
            Try
               FileSetDate(TFileStream32(Outfile).Handle, FileAge(fArchiveFile));
            Except
            End;

            Close_OutFile(Outfile);
            // v4.0... working here.  rem'd
              //If WriteMethod = faFileStream Then
              //  If Not DoMove( TempFilename, OutFilename ) Then
              //    DoWriteError;

            Inc(fnum);
            sOutfile := GetNextVolName(OutFilename, fnum, FNextVolumeName,
               FVolumeLongFilenames);

            //Outfile := ztvOpenFileWrite(Self, PChar(sOutfile), True);
            Outfile := TFileStream32.Create(sOutfile, fmOpenWrite);

            If TFileStream32(Outfile).Handle = INVALID_HANDLE_VALUE Then
               RaiseError(E_RAISE, OutFilename, '', '0', E_FOPEN);

            str := 'part ' + IntToStr(fnum) + EOL_STR + 'begin 644 ' +
               ExtractFilename(fArchiveFile) + EOL_STR;
            BytesToWrite := Length(str);

            If CompressWriteBlock(Outfile, str[1], False,
               0, BytesToWrite, dtHeader) <> Cardinal(BytesToWrite) Then
               doWriteError;

            cLine := 3;
         End;

         str := 'end' + EOL_STR + IntToStr(fSize) + ' bytes' + EOL_STR;
         BytesToWrite := Length(str);
         If CompressWriteBlock(Outfile, str[1], False,
            0, BytesToWrite, dtHeader) <> Cardinal(BytesToWrite) Then
            doWriteError;

         If fnum > 0 Then
         Begin
            str := 'encoded into ' + IntToStr(fnum) + ' file(s)' + EOL_STR;
            BytesToWrite := Length(str);
            If CompressWriteBlock(Outfile, str[1], False,
               0, BytesToWrite, dtHeader) <> Cardinal(BytesToWrite) Then
               doWriteError;
         End;
      Finally
         If abLine <> Nil Then FreeMem(abLine, MAXLINELEN + 1);
         If inbuf <> Nil Then FreeMem(inbuf, MaxMem + 1);
         If outbuf <> Nil Then FreeMem(outbuf, MaxMem + 1);

         //TempFilename := TempStream.Filename;
         Close_OutFile(Outfile);

          // v4.0... working here.  rem'd
          //If WriteMethod = faFileStream Then
          //  If Not DoMove( TempFilename, OutFilename ) Then
          //    DoWriteError;

          Infile.Free();	//CloseHandle(Infile);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Function TUUEncode.GetHeadSize: word;
Begin
	Result := 0;
End;
//-------------------------------------------------------------

Procedure TUUEncode.CompressIT(pHeaderObj: pCompHeaderObj);
Var
   DummyCompFileInfo: TCompFileInfo;
   ArcFile, TempFile: TStream32;
Begin

   Try

      If OutFilename = '' Then
         RaiseError(E_RAISE, 'OutFile', '', '0', E_INVALIDFN);

      If (Switch = swDelete) Then
         RaiseError(E_RAISE, fArchiveFile, '', '0', E_INVALIDARC);


      If FileExists(OutFilename) Then
      Begin

         If fOverwriteMode = omOverwrite Then
         Begin
            If Not EraseFile(OutFilename, DeleteOptions) Then
            Begin
               RaiseErrorStr(OutFilename, '', '0', E_FWRITE);
               Exit;
            End;
         End
         Else
         Begin
            If Assigned(OnFileExists) Then
               OnFileExists(Self,
                  OutFilename,
                  ztvConvertDate(FileAge(OutFilename)),
                  fOverwriteMode)
            Else
            Begin
               RaiseErrorStr(OutFilename, 'OnFileExists', '0', E_REQUIREDEVENT);
               Exit;
            End;

            If (Not HandleNonWriteableFile(OutFilename)) Then
               Exit;
         End;
      End;


      If fOverwriteMode = omOverwrite Then
      Begin
         If Assigned(OnActivate) Then OnActivate(Self);
         If Assigned(OnElapsedTime) Then ZipTimer.START;

         pCBFI := @DummyCompFileInfo;
         pCBFI^.Status := hsRename;

         Try
            (* The DoOnbegin function simply activates the OnBegin and	*)
            (* OnRenameFile events.  The return value of this	*)
            (* function is the boolean parameter of the OnBegin event.  *)
            If doOnBegin(fOutFilename, 1, pHeaderObj) Then
            Begin
               EncodeIT(ArcFile, TempFile);
               If Assigned(OnEnd) Then
                  OnEnd(Self, OutFilename, Cancel = False);
            End;
         Finally
            If Assigned(OnDeactivate) Then
               OnDeactivate(Self);

            If Assigned(OnElapsedTime) Then
            Begin
               ZipTimer.Stop;
               OnElapsedTime(Self, ZipTimer.ElapsedTime);
            End;
         End;
      End;

   Except
      If Assigned(OnEnd) Then OnEnd(Self, OutFilename, False);
   End;
End;
//-------------------------------------------------------------

End.
