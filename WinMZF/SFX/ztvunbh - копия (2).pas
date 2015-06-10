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
Unit ztvUnBH;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   Dialogs,
   ztvRegister,
   ztvBase,
   ztvHeaders,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TUnBh = Class(TUnBASE)
   Private
      FileProgressPos: Integer;
      Function ExtractInflate(Instream: TStream32; Var Outfile: TStream32;
         MAX_WBITS: smallint): Boolean;
      Function BuildHeadArray(Infile: TStream32): Integer;
      Function OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
		Procedure ProcessHeaders(Infile, Outfile: TStream32);
   Protected
      Function VerifyPassword(Buffer: PChar): Boolean; Override;
      //Function RequestPassword( f: THandle ): Boolean;
      //Procedure UpdateEncryptBuffer( IsEncrypted: Boolean; Var Buf; Countt: INTEGER ); Override;
   Public
      (* Create / Destroy are needed to use the create method within code *)
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
      Property PasswordAttempts;
      Property Passwords;
      Property RestoreFileAttr;
      Property UseStoredDirs;
      Property OnDeactivate;
      Property OnProgress;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnGetPassword;
      Property OnRenameFile;
   End;


Implementation

Uses
   ztvGbls,
   ztvInflate,
   ztvLzh3,
   ztvCrypt,
   Err_Msgs,
   Forms;

//Var
//   FileProgressPos: Integer;

//-------------------------------------------------------------

Constructor TUnBh.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TUnBh.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TUnBh.ExtractInflate(Instream: TStream32; Var Outfile: TStream32;
   MAX_WBITS: smallint): Boolean;
Var
   Count: Integer;
   Buffer: Pointer;
   size, BufSize, FileSize: Integer;
   Stream: TCustomStream;
   //InStream: THandleStream32;
Begin
   Result := False;
   //InStream := THandleStream32.Create(Infile);

   //Try
      Stream := TDecompressStream.Create(InStream, MAX_WBITS);
      Try
         Stream.FZRec.cb.pCancel := @fCancel;

         With BhHeader Do
         Begin
            FileSize := UnpackedSize;
            Stream.FZRec.cb.Protect := BitFlag And PW_PROTECTED > 0;
         End;

         size := FileSize;

         // use the following block, instead of the min function...
         // the min function fails with files > 4 gig.
         //BufSize := Min(size, WSIZE{4096});
         If Size > WSIZE{4096} Then
            BufSize := WSIZE
         Else
            BufSize := Size;

         GetMem(Buffer, BufSize + 1);
         Try

            While True Do
            Begin
               If (size <= 0) Or Cancel Then break;
               Application.ProcessMessages();

               // use the following block, instead of the min function...
               // the min function fails with files > 4 gig.
               //BufSize := Min(size, WSIZE{4096});
               If Size > WSIZE{4096} Then
                  BufSize := WSIZE
               Else
                  BufSize := Size;

               Try
                  Count := Stream.Read(Buffer^, BufSize);

                  If Count > 0 Then
                     //OutStream.WriteBuffer( Buffer^, Count );
                     If ExtractWriteBlock(Outfile, Buffer^,
                        False, 32, Count, dtData) = 0 Then
                        RaiseError(E_RAISE, FileName, '', '0', E_FWRITE);

                  If Count = 0 Then
                  Begin
                     ProgressPosition := ProgressPosition - size;
                     Dec(FileProgressPos, size);
                  End
                  Else
                  Begin
                     ProgressPosition := ProgressPosition - Count;
                     Dec(FileProgressPos, Count);
                  End;

                  doBranchProgress(size - FileProgressPos, size,
                     fTotalUnpackedSize);

                  If Count = 0 Then
                     break

               Except
               	On EWriteError Do
               		Raise;
               End;

               Dec(size, BufSize);
            End;
         Finally
            FreeMem(Buffer);
         End;
      Finally
         Stream.Free();
      End;
   //Finally
   //   InStream.Free();
   //End;
End;
//-------------------------------------------------------------

Function TUnBh.VerifyPassword(Buffer: PChar): Boolean;
Begin
   With BhHeader Do
      Result := decrypt_pw(Buffer, {RAND_HEAD_LEN,} BitFlag, CRC32,
         FileDate, fPassword);
End;
//-------------------------------------------------------------

Function TUnBh.OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
Begin

   Result := False;

   If doOnBegin({( FileAttr And faVolumeID > 0 ) Or}(FileAttr And FILE_ATTRIBUTE_DIRECTORY > 0)) Then
   Begin

      // v4.7.2 moved the following RequestPassword block inside doOnBegin
      (* To keep search (TZipSearch & TTurboSearch components) and Validity
         checks (TZipCheck component) speed at a maximum while attempting to
         not bypass files, the variable "PasswordAttemptFailed" is used.  If
         a password attempt failed, bypass all remaining protected files in
         the archive but continue with non-protected files. *)
      fEncrypted := (InflateRec.BitFlag And PW_PROTECTED > 0);
      If fEncrypted And (Not PasswordAttemptFailed) Then
         If Not RequestPassword(Infile) Then
            PasswordAttemptFailed := True;


      With BhHeader Do
      Try

         TRY
            If (FileAttr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
            Begin
               If WriteToFile() Then
               Begin
                  If CreateStoredDirs Then
                     CreateDirEx(fFileName);
               End Else Begin
                  Crc32Val := 0;
                  Result := True;
               End;
            End
            Else
            Begin

               Bytes_To_Go := PackedSize;

               If fEncrypted And (VerNum < 3) Then //<--- older password protected BlakHole
                  seed_keys(fPassword);    //     archives requires these lines

               (* If PasswordAttemptFailed = true, the DoOnBegin event
                  returns true without actually opening the output file *)
               If Open_OutFile(Outfile, FileName, ActualFilename) Then
               Try
                  Crc32Val := CRC_MASK;

                  If fEncrypted And PasswordAttemptFailed Then
                  Begin
                     Dec(Count);
                     AdjustProgress(InflateRec);
                  End
                  Else
                     Case CompressType Of

                        0:              // mtStored
                           Begin
                              If (VerNum < 3) Then
                                 InflateRec.BitFlag := 0;

                              Unstore(Infile, Outfile, 32, '0', InflateRec);
                           End;

                        1:              // mtFused method 1
                           Begin
                              (* crypt flag *)
                              If fEncrypted Then
                                 lzh_initialize(32, 13, 4, 1)
                              Else
                                 lzh_initialize(32, 13, 4, 0);

                              lzh_decode(Self, Infile, Outfile, InflateRec);
                           End;
                        3:              // mtFused method 3
                           Begin
                              If fEncrypted Then
                                 lzh_initialize(32, 15, 5, 1)
                              Else
                                 lzh_initialize(32, 15, 5, 0 {5});

                              lzh_decode(Self, Infile, Outfile, InflateRec);
                           End;

                        2, 8:           // mtDeflate
                           Begin
                              FileProgressPos := BhHeader.UnpackedSize;
                              is64Bit := False;
                              {$IFDEF DEFLATE_BUG_WORKAROUND}
                              MAXDISTS := 32;
                              {$ELSE}
                              MAXDISTS := 30;
                              {$ENDIF DEFLATE_BUG_WORKAROUND}
                              Result := ExtractInflate(Infile, Outfile, MAX_WBITS);
                           End;

                     Else
                        RaiseError(E_RAISE, FileName, '', '0', E_UNKNMETH);
                     End;
               Finally
                  If fEncrypted And PasswordAttemptFailed Then
                     fFileName := ActualFilename
                  Else
                     CloseAndSetDate(Outfile, FileName, FileDate, FileAttr);
               End
               Else
               Begin
                  RaiseErrorStr(fFileName, '', '0', E_FOPEN);
                  AdjustProgress(InflateRec);
                  Dec(Count);
               End;
            End;
         Finally
            Result := doOnEnd(32, CRC32);
         End;
      Except
         //ON e: exception DO ShowMessage( e.message );
      End;
   End
   Else
      AdjustProgress(InflateRec);
End;
//-------------------------------------------------------------

Procedure TUnBh.ProcessHeaders(Infile, Outfile: TStream32);
Var
   i: Integer;
   pFilename: PChar;
Begin

   GetMem(pFilename, 256);
   With BhHeader Do
   Try
      ProgressPosition := fTotalUnpackedSize;

      For i := 0 To HeaderList.FileCount - 1 Do
      Begin
         If Cancel Then break;

         pUBFI := HeaderList.FileLocationData(i);
         With pUBFI^ Do
         Begin
         	Infile.Seek(OffsetOfLocalHeader, soBeginning);

            (* Read Header & Filename *)
            ReadBlock(Infile, Nil, BhHeader, False, 0, SizeOf(TBh), dtHeader);

            If ReadFilename(Infile, pFilename, FileNameLen) = 0 Then
               Continue;

            If CommentLen > 0 Then
         		Infile.Seek(CommentLen, soCurrent);

            ActualFilename := OemToCharFilter(StrPas(pFilename), fTransOemChar);
            If WriteToFile() Then
            	FileName := ActualFilename  	// format filename
            Else
            	fFileName := ActualFilename;	// non-format filename


            InflateRec.BitFlag := BitFlag;
            If CompressType = 2 Then    //Early versions stored the deflate as method 2
               CompressType := 8;       //Deflate

            InflateRec.CompressType := CompressType;
            InflateRec.PackedSize := PackedSize;
            InflateRec.UnpackedSize := UnpackedSize;

            // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
            GlobalDate := FileDate;

            OpenAndExtractFile(Infile, Outfile, FileAttr);
         End;
      End;
   Finally
      FreeMem(pFilename, 256);
   End;
End;
//-------------------------------------------------------------

{Procedure TUnBH.UpdateEncryptBuffer( IsEncrypted: Boolean; Var Buf; Countt: INTEGER );
Var
   p                : ^BYTE;
   i                : INTEGER;
Begin
   If IsEncrypted Then
   Begin
      p := @Buf;
      For i := 1 To Countt Do
      Begin
         p^ := p^ Xor zDecryptByte;
         update_keys( p^ );
         inc( p );
      End;
   End;
End;}
//-------------------------------------------------------------

Function TUnBh.BuildHeadArray(Infile: TStream32): Integer;
Var
   pFilename: PChar;
   CurrentPos: u_long;
   UBFI: TUnBaseFileInfo;               //HeaderData
Begin

   ZipTimer.Suspend;
   GetMem(pFilename, 256);

   CurrentPos := fOffsetStart;
   //If CurrentPos > 0 Then Dec(CurrentPos);

   With BhHeader Do
   Try
      Infile.Seek(CurrentPos, soBeginning);
      If ReadBlock(Infile, Nil, BhHeader, False, 0, SizeOf(TBh), dtHeader) = SizeOf(TBh) Then
      Begin

         While ((SignAtr = BLAKHOLE_SIGNATURE) And (HdrSize > 0)) Do
         Begin
            If Cancel Then break;

            If FileNameLen < 256 Then
            Begin
               ReadFilename(Infile, pFilename, FileNameLen);
               ActualFilename := OemToCharFilter(StrPas(pFilename), fTransOemChar);

               If CheckWildCard2(ActualFilename, FileSpec, ExcludeSpec, RecurseDirs) Then
               Begin

                  With UBFI Do
                  Begin
                     DiskWithThisFile := 0;
                     OffsetOfLocalHeader := CurrentPos;
                     FileAttr := ExternalAttr;
                  End;
                  HeaderList.AddItem(UBFI, Nil, 0);

                  If FileDate > fMaxAge Then
                     fMaxAge := FileDate;

                  fTotalPackedSize := fTotalPackedSize + PackedSize;
                  fTotalUnpackedSize := fTotalUnpackedSize + UnpackedSize;

                  If BitFlag And 1 > 0 Then
                     fTotalPackedSize := fTotalPackedSize - RAND_HEAD_LEN;

               End;
            End;

            inc(CurrentPos, SizeOf(TBh) + FileNameLen + CommentLen + PackedSize);
      		Infile.Seek(CommentLen + PackedSize, soCurrent);

            ZeroMemory(@BhHeader, SizeOf(TBh));
            ReadBlock(Infile, Nil, BhHeader, False, 0, SizeOf(TBh), dtHeader);
         End;
      End;
   Finally
      Result := HeaderList.FileCount;
      FreeMem(pFilename, 256);
      ZipTimer.Resume;
   End;
End;
//-------------------------------------------------------------

Procedure TUnBh.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
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
