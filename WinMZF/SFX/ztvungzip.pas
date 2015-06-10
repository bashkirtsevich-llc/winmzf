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
Unit ztvUnGZip;


Interface


Uses
   Windows,
   SysUtils,
   Classes,
   ztvRegister,
   ztvBase,
   ztvGbls,
   ztvStreams,
   ztvUnTAR;

{$I ZipTV.inc}                          //Declare the compiler defines
{$define use_gzip_inflate}

Type
   TUnGZip = Class(TUnBASE)
   Private
      CompressedSize: u_long;
      FileProgressPos: Cardinal;
      OnEndPtr: TOnEnd;
      EWB: TWriteBlock;
      FNLen: Byte;
      fDeleteOptions: TDeleteOptions;
      fUnTar: TUnTar;
      OriginalWM: TFileAccessMethod;
      UncompressedSize: u_long;
      Function ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
         size: Byte; Cnt: DWord; WriteType: TDataType): DWord;
      Function BuildHeadArray(Infile: TStream32): Integer;
{$ifndef use_gzip_inflate}
      Function ExtractInflate(Infile: THandle; Var Outfile: THandle;
         MAX_WBITS: smallint): Boolean;
{$endif use_gzip_inflate}
      Procedure InitializeHeader(Infile: TStream32);
      Function OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
      Procedure ProcessHeaders(Infile, Outfile: TStream32);
   Protected
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure ExtractIT(Var Infile: TStream32; Outfile: TStream32); Override;
   Published
      Property ArcType;
      Property AsciiTranslation;
      Property ConfirmOverwrites;
      Property GzTarDeleteOptions: TDeleteOptions Read fDeleteOptions Write fDeleteOptions;
      Property DateAttribute;
      Property FileSpec;
      Property OverwriteMode;
     //PROPERTY  Passwords;
      Property RestoreFileAttr;
      Property TempDir;
      Property UseStoredDirs;
      Property ExtractDir;
      Property OnActivate;
      Property OnDeactivate;
      Property OnProgress;
     //PROPERTY  OnGetPassword;
      Property OnBegin;
      Property OnChangeArchive;
      Property OnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnNestedTarFile;
      Property OnRenameFile;
   End;



Implementation


Uses
   Forms,
{$ifdef use_gzip_inflate}
   ztvGZInflate,
{$endif use_gzip_inflate}
   ztvDelzw,
   Err_Msgs;


Const

  (* GZIP ExtraFLAGS *)
   ASCII_FLAG = $01;                    // bit 0 set: file probably ascii text
   CONTINUATION = $02;                  // bit 1 set: continuation of multi-part gzip file
   EXTRA_FIELD = $04;                   // bit 2 set: extra field present
   ORIG_NAME = $08;                     // bit 3 set: original file name present
   Comment = $10;                       // bit 4 set: file comment present
   gzENCRYPTED = $20;                   // bit 5 set: file is encrypted
   Reserved = $C0;                      // bit 6, 7:   reserved

//-------------------------------------------------------------

Function IsGZipSignAttr(Attr: Integer): Boolean;
Begin
   Result :=
      (Attr = GZIP_MAGIC) Or (Attr = OLD_GZIP_MAGIC) Or
      (Attr = LZW_MAGIC) Or (Attr = LZH_MAGIC) Or
      (Attr = PACK_MAGIC);
End;
//-------------------------------------------------------------

Constructor TUnGZip.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TUnGZip.Destroy;
Begin
   Inherited Destroy();
End;
//-------------------------------------------------------------
(* Intercepts the first write buffer only, to determine if the compressed
   file is a tar archive... all subsequent buffer writes are directed to the
   original write routines.
 *)

Function TUnGZip.ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
	size: Byte; Cnt: DWord; WriteType: TDataType): DWord;
Var
   NewFilename: String;
Begin

   IsGzTarArchive := False;
   Try
     (* Validate the decompressed buffer against the tar header *)
      If Cnt > SizeOf(TarHeader) Then
         IsGzTarArchive := ValidateTarHeader(@Buf);

      If IsGzTarArchive Then
      Begin
         IsGzTarArchive := False;
         If Assigned(OnNestedTarFile) Then
            OnNestedTarFile(Self, ActualFilename, IsGzTarArchive)
         Else
            RaiseErrorStr(FileName, 'OnNestedTarFile', '0', E_REQUIREDEVENT);

      //WriteMethod = faVerify: file wasn't previously opened
         If IsGzTarArchive And (Not WriteToFile) Then
         Begin
            fFileName := AppendDirTail(TempDir) + ExtractFilename(fFileName);

            If Assigned(OnFileExists) Then
            Begin
               fOverwriteMode := omOverwrite;
               While (Not fCancel) And (fOverwriteMode = omOverwrite) And FileExists(fFileName) Do
               Begin

                  NewFilename := '';
                  OnFileExists(Self, fFileName, NewFilename, fOverwriteMode);
                  If (fOverwriteMode = omOverwrite) Then
                  Begin
                     If NewFilename <> '' Then
                        If CompareText(fFileName, NewFilename) <> 0 Then
                           fFileName := NewFilename
                        Else
                           break;
                  End
                  Else
                     IsGzTarArchive := False;

               End
            End
            Else
            Begin
               IsGzTarArchive := False;
               RaiseErrorStr(FileName, 'OnFileExists', '0', E_REQUIREDEVENT);
            End;

            If IsGzTarArchive Then
            Try
               WriteMethod := faFile;
               Crc32Val := CRC_MASK;
               IsGzTarArchive :=
               	Open_OutFile(f, FileName, ActualFilename);
            Finally
               If Not IsGzTarArchive Then
               Begin
                  WriteMethod := OriginalWM;
                  RaiseErrorStr(fFileName, '', '0', E_FOPEN);
               End;
            End;
         End;
      End;
   Finally
      If WriteToFile Then
         ExtractWriteBlock := ExtractWriteProc
      Else
         ExtractWriteBlock := EWB;

     // Unassign the OnEnd event to prevent the result display of the
     // .gz archive being extracted to verify the internal tar archive.
      If IsGzTarArchive Then
         OnEnd := Nil;

      Result := ExtractWriteBlock(f, Buf, IsEncrypted, size, Cnt, WriteType);
   End;
End;
//-------------------------------------------------------------

{$ifndef use_gzip_inflate}
Function TUnGZip.ExtractInflate(Infile: THandle; Var Outfile: THandle;
   MAX_WBITS: smallint): Boolean;
Var
   Count: Integer;
   Buffer: Pointer;
   BufSize: Integer;
   size, FileSize: Cardinal;
   Stream: TCustomStream;
   InStream: THandleStream32;
Begin
   Result := False;
   InStream := THandleStream32.Create(Infile);
   Try
      Stream := TDecompressStream.Create(InStream, MAX_WBITS);
      Try
         Stream.FZRec.cb.pCancel := @fCancel;
         If GZipHeader.BitFlag And PW_PROTECTED > 0 Then
         Begin
         	RaiseErrorStr(fArchiveFile, '', '0', E_PROTECTED);
         	Exit;
         End Else
         	Result := True;  // default

         FileSize := UncompressedSize;
         Stream.FZRec.cb.Protect := False;

         size := FileSize;
         BufSize := Min(size, WSIZE (*4096*));

         GetMem(Buffer, BufSize + 1);
         Try

            While True Do
            Begin
               If (size <= 0) Or fCancel Then break;
               Application.ProcessMessages();
               BufSize := Min(size, WSIZE (*4096*));

               Try
                  Count := Stream.Read(Buffer^, BufSize);

                  If Count > 0 Then
                  Begin
                     If ExtractWriteBlock(Outfile, Nil, Buffer^,
                        False, 32, Count, dtData) = 0 Then
                        RaiseError(E_RAISE, FileName, '', '0', E_FWRITE);
                  End;

                  If Count < 1 Then
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

                  If (Count = 0) Then
                     break
                  Else
                  	// see file \3\gz\bugs\garbage_at_eof - log.tar.gz
                     If Stream.FZRec.cb.Eof And (Size - Cardinal(BufSize) > 0) Then
                     Begin
                        RaiseErrorStr(fArchiveFile, '', '0', E_EOFNOTFOUND);
                        fCancel := True;
                        Result := False;
                     End;

               Except
                  //Raise EFileCorruption.Create( Format( FileCorrupted, [loop] ) );
               End;

               Dec(size, BufSize);
            End;
         Finally
            FreeMem(Buffer);
         End;
      Finally
         Stream.Free();
      End;
   Finally
      InStream.Free();
   End;
End;
{$endif use_gzip_inflate}
//-------------------------------------------------------------

Function TUnGZip.OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
Var
   AC: Boolean;
   pArcType: ^TArcType;
Begin

   Result := False;
   With GZipHeader Do
      If doOnBegin(False) Then
      Begin
         Try
            Try
               AC := AsciiTranslation;
               If ExtraFlags And 2 > 0 Then
                  AsciiTranslation := False;

               Try
                  If Open_OutFile(Outfile, FileName, ActualFilename) Then
                  Begin
                     Try
                        Bytes_To_Go := CompressedSize;

                        Case SignAtr Of

                           GZIP_MAGIC,     // Deflate
                              OLD_GZIP_MAGIC:
                              Begin

                                 If BitFlag And CONTINUATION = CONTINUATION Then
                                 Begin
                                    RaiseErrorStr(FileName, '', '0', E_MULTIPARTGZIP);
                                    Exit;
                                 End
                                 Else
                                    If BitFlag And gzENCRYPTED = gzENCRYPTED Then
                                    Begin
                                       RaiseErrorStr(FileName, '', '0', E_PROTECTED);
                                       Exit;
                                    End;

                                 InflateRec.PackedSize := fLOF;
                                 InflateRec.UnpackedSize := fLOF;
                                 FileProgressPos := fLOF;
                                 ProgressPosition := fLOF;
                                 fTotalUnpackedSize := fLOF;

                                 // following two lines use unit ztvInflate_old.pas
                                 //ztvSetFilePointer( InFile, 16, FILE_BEGIN );
                                 //ExtractInflate( Self, Infile, Outfile );

   {$ifdef use_gzip_inflate}
                                 ExtractFile( Self, InFile, OutFile, InflateRec );
                                 FCRC := Crc32Val Xor CRC_MASK;
   {$else}
                                 //Crc32Val := CRC_MASK;
                                 ExtractInflate(Infile, Outfile, maxbits);
                                 fCRC := Crc32Val Xor CRC_MASK;
   {$endif}
                              End;

                           LZW_MAGIC:

                              Begin

                                 Crc32Val := 0;
                                 Infile.Seek(0, soBeginning);

                                 If unlzw(Self, Infile, Outfile, Bytes_To_Go) Then
                                    fCRC := Crc32Val (* Successful extraction	*)
                                 Else
                                    fCRC := 999; (* Failed extraction			*)

                              End;
                        Else
                       //LZH_MAGIC:
                       //PACK_MAGIC:
                           fCRC := CRC_MASK;
                           RaiseErrorStr(fArchiveFile, '', '0', E_UNKNMETH);
                        End;
                     Finally
                     	If WriteMethod = faFile Then
                        	Outfile.Seek(0, soBeginning);
                        CloseAndSetDate(Outfile, fFileName, FileDate, 32);
                     End
                  End Else Begin
                  	Dec(Count);
                     RaiseError(E_RAISE, fFileName, '', '0', E_FOPEN);
                  End;

               Finally
                  AsciiTranslation := AC;
                  Result := doOnEnd(32, fCRC);
               End;
            Except
               Result := False;
            End;

            If Result And IsGzTarArchive And WriteToFile Then
            Begin
               If FileExists(FileName) Then
               Begin
                  fUnTar := TUnTar.Create(Nil);
                  Try
                     fUnTar.ArchiveFile := FileName;
                     If fUnTar.ArcType = atTar Then
                     Begin
                        pArcType := @ArcType; //send back to calling component... revised
                        pArcType^ := atTar;

                        fUnTar.WriteMethod := faFile;
                        fUnTar.UseStoredDirs := False; //do not change this value!  See Brahms-0.97.2.tgz
                        fUnTar.ExtractDir := ExtractDir;
                        fUnTar.FileSpec := FileSpec;
                        fUnTar.RecurseDirs := RecurseDirs;
                        fUnTar.OnProgress := OnProgress;
                        fUnTar.OnBegin := OnBegin;
                        fUnTar.OnEnd := OnEndPtr;
                        fUnTar.OnFileExists := OnFileExists;
                        fUnTar.TempDir := TempDir;

                        Try
                           fUnTar.ExtractWriteBlock := EWB;
                           Case OriginalWM Of
                              faSearch,
                              	faVerify: fUnTar.ExtractToVerify();
                              faFile{,
                                 faFileStream}: fUnTar.Extract();
                           Else
                           End;
                        Finally
                           Count := fUnTar.Count;
                           If (Count > 0) And Assigned(OnChangeArchive) Then
                              OnChangeArchive(Self, FileName, pArcType^);
                        End;
                     End;
                  Finally
                     fUnTar.Destroy();
                  End;
               End
               Else
                  RaiseErrorStr(fFileName, '', '0', E_FOPEN);

            End;
         Except
         End;
      End;
End;
//-------------------------------------------------------------

Procedure TUnGZip.InitializeHeader(Infile: TStream32);
Var
   Ext: String;
   pFilename: PChar;
   i, ExtraFieldLen: word;

   Function IsAsciiFile: String;
   Begin
      If (CompareText(Ext, '.tar') = 0) Then
         Result := ActualFilename
      Else
      Begin
         If CompareText(Ext, ExtractFileExt(fArchiveFile)) = 0 Then
            Result := Copy(ActualFilename, 1, ExtractFilenameOnly(@ActualFilename[1]))
         Else
            Result := ActualFilename;

         If (GZipHeader.BitFlag And ASCII_FLAG > 0) Then
            Result := Result + '.txt';
      End;
   End;
Begin

   GetMem(pFilename, 256);
   With GZipHeader Do
   Try
      ExtraFieldLen := 0;
      FNLen := 0;
      fCRC := 0;
      CompressedSize := fLOF;
      UncompressedSize := fLOF;

      Case SignAtr Of
        //LZW_MAGIC: ;
        //LZH_MAGIC: ;
        //PACK_MAGIC: ;
         GZIP_MAGIC, OLD_GZIP_MAGIC:
            Begin

              (* Get value for ExtraFieldLen *)
               If (BitFlag And EXTRA_FIELD > 0) Then
               Begin
                  ReadBlock(Infile, Nil, ExtraFieldLen, False, 0, SizeOf(ExtraFieldLen), dtHeader);
         			Infile.Seek(ExtraFieldLen, soCurrent);
               End;

               If (BitFlag And ORIG_NAME > 0) Then
               Begin
                  ReadFilename(Infile, pFilename, 255);
                  ActualFilename := OemToCharFilter(UnixToDosFilename(StrPas(pFilename)), fTransOemChar);
                  FNLen := Length(ActualFilename);
                  If FNLen > 0 Then inc(FNLen);
               End
               Else
               Begin
                  ActualFilename := ExtractFilename(fArchiveFile);
                  ActualFilename := Copy(ActualFilename, 1, Length(ActualFilename) - Length(ExtractFileExt(ActualFilename)));
               End;

               Try
                  If FileDate = 0 Then
                     FileDate := FileAge(fArchiveFile)
                  Else
                     FileDate := DateTimeToFileDate(UnixDateToDos(FileDate));
               Except
                  FileDate := 2162720; //29221.000694;  // 01/01/1980 12:01 am
               End;

                       (* Read CRC & Date at EOF - 8 			*)
         		Infile.Seek(fLOF - (2 * SizeOf(Integer)), soBeginning);

               ReadBlock(Infile, Nil, fCRC, False, 0, SizeOf(fCRC), dtHeader);
               ReadBlock(Infile, Nil, UncompressedSize, False, 0, SizeOf(UncompressedSize), dtHeader);

               If (SignAtr <> fCRC) Then
                  CompressedSize := CompressedSize - u_long(FileNameLen + (2 * SizeOf(Integer)) + ExtraFieldLen - 8)
               Else
                  CompressedSize := CompressedSize - u_long(ExtraFieldLen - 8);
            End;
      Else                              (* case *)
         Try
            FileDate := FileAge(fArchiveFile);
         Except
            FileDate := 2162720;  //29221.000694;    // 01/01/80 12:01 am
         End;

         CompressedSize := fLOF;
         UncompressedSize := CompressedSize;
         ActualFilename := ExtractFilename(fArchiveFile);
      End;

      Infile.Seek(GZipHdr_Size + ExtraFieldLen + FNLen, soBeginning);

      Ext := LowerCase(ExtractFileExt(ActualFilename));
      If Ext <> '' Then
      Begin
         If (CompareText(Ext, '.tgz') = 0) Then
            ActualFilename :=
               Copy(ActualFilename, 1, ExtractFilenameOnly(@ActualFilename[1]))
               + '.tar'
         Else
            ActualFilename := IsAsciiFile;
      End
      Else
         ActualFilename := IsAsciiFile;


     (* See FLY-1_6_5_tar.gz
        This archive stores the filename internally as fly-1.6.5.tar
        Attempting to open a file with multiple dot chars fails.
        Revise "." chars in the filename part as "_".	*)
      If DiskManager.VolumeInfo.FileSys = 'FAT16' Then
         For i := 1 To Length(ActualFilename) - Length(ExtractFileExt(ActualFilename)) Do
            If ActualFilename[i] = '.' Then
               ActualFilename[i] := '_';

   Finally
      FreeMem(pFilename, 256);
   End;
End;
//-------------------------------------------------------------

Procedure TUnGZip.ProcessHeaders(Infile, Outfile: TStream32);
Var
   i: Integer;
Begin
   IsGzTarArchive := False;
   ProgressPosition := fTotalUnpackedSize;
   For i := 0 To HeaderList.FileCount - 1 Do
      With GZipHeader Do
      Begin
         If fCancel Then break;

         pUBFI := HeaderList.FileLocationData(i);
         With pUBFI^ Do
         Begin
         	Infile.Seek(pUBFI^.OffsetOfLocalHeader, soBeginning);

        		// Read Header & Filename
            ReadBlock(Infile, Nil, GZipHeader, False, 0, GZipHdr_Size, dtHeader);

            If IsGZipSignAttr(SignAtr) Then
            Begin
               InitializeHeader(Infile); //assign ActualFilename, date, packed & unpacked sizes...
               FileName := ActualFilename;

               UncompressedSize := 202552;
               InflateRec.BitFlag := BitFlag; //no password protection
               InflateRec.CompressType := CompressType;
               InflateRec.PackedSize := CompressedSize;
               InflateRec.UnpackedSize := UncompressedSize;

          		// Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
               GlobalDate := FileDate;
               OpenAndExtractFile(Infile, Outfile, 32);
            End;
         End;
      End;
End;
//-------------------------------------------------------------

Function TUnGZip.BuildHeadArray(Infile: TStream32): Integer;
Var
   UBFI: TUnBaseFileInfo;               //HeaderData
Begin

   Result := 0;
   ZipTimer.Suspend();
   OnEndPtr := OnEnd;
   OriginalWM := WriteMethod;           //save pointer to initial write method.  It is
                               //changed if the calling component is TZipCheck,
                               //TZipSearch, or TTurboSearch and the compressed
                               //file is a tar archive.  When one of these
                               //components, calls TUnGZip with WriteMethod <>
                               //faFile AND WriteMethod <> faFileStream
                               //(essentially a memory decompression) the
                               //compressed tar archive is extracted to disk
                               //and becomes the active archive.

   Try
  		//Intercept the first write buffer to test if compressed file is
      //a tar archive.
      //
      //Get pointer to the current write procedure so as to redirect
      //subsequent buffer writes to the proper procedure.
      //
      //The receiving value of ExtractWriteBlock can very depending on
      //the source component.  Example: the TZipCheck component intercepts
      //the write
      EWB := ExtractWriteBlock;
      ExtractWriteBlock := ArcPutBlock;

      Infile.Seek(0, soBeginning);

      ReadBlock(Infile, Nil, GZipHeader, False, 0, GZipHdr_Size, dtHeader);
      With GZipHeader Do
         If IsGZipSignAttr(SignAtr) Then
         Begin
            If fCancel Then Exit;

            //assign ActualFilename, date, packed & unpacked sizes...
            InitializeHeader(Infile);

            If {ExtraFlags <> 2 And}
            	CheckWildCard2(
               	ActualFilename,
                  FileSpec,
                  ExcludeSpec,
                  RecurseDirs) Then
            Begin
               With UBFI Do
               Begin
                  DiskWithThisFile := 0;
                  OffsetOfLocalHeader := 0;
                  FileAttr := FILE_ATTRIBUTE_NORMAL;
               End;
               HeaderList.AddItem(UBFI, Nil, 0);

               If FileDate > fMaxAge Then
                  fMaxAge := FileDate;

               fTotalPackedSize := fTotalPackedSize + CompressedSize;
               fTotalUnpackedSize := fTotalUnpackedSize + UncompressedSize;
            End;
         End;
   Finally
      Result := HeaderList.FileCount;
      ZipTimer.Resume();
   End;
End;
//-------------------------------------------------------------

Procedure TUnGZip.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
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
