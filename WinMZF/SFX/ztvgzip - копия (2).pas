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
Unit ztvGZip;

Interface

Uses
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   ztvRegister,
   ztvBase,
   ztvGbls,
   ztvFileScan,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines
{DEFINE LOCALDEBUG}

Type
   TGZipExt =
      (exNone, exAppendGZ, exUnderScore);

   TGZip = Class(TCompBase)
   Private
      //FPS: Integer;
      CRC32: u_long;
      CompressDir: String;
      CBFI: TCompFileInfo;
      fMinVersion: Word;
      fExtension: TGZipExt;
      FileScan: TztvFileScan;
      ArcStream,
      	TempStream: TFileStream32;
      Function CompressionMethodUsed(method: word): String;
      Function Deflate(InStream, OutStream: TStream32): Boolean;
      Function PackGzFile(InFilename, OutFilename: String; FileObject: TFileObject): Boolean;
   Protected
      //Function Write64BitFieldHdr(s: TStream32; CFI: pCompFileInfo;
      //	HType: THeaderType): Integer; Override;
      Procedure SetArchiveFile(SFN: String); Override;
   Public
   	isTarGZip: Boolean;	// this variable is used internally only... DO NOT USE!
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Function Compress: Integer; Override;
      Procedure DoUpdateFilesList(Const Dir: String; FindData: TWin32FindData;
         pHeaderObj: pCompHeaderObj);
   Published
      Property ArcType;
      Property AttributesEx;
      Property DefaultExt;
      Property DeflateType;
      Property CompressMethod;
      Property CompressionMethod;
      Property ConfirmOverwrites;
      Property Extension: TGZipExt Read fExtension Write fExtension Default exAppendGZ;
      Property FileSpec;
      Property Switch;
      //Property TempDir;
      Property OnNonWriteableArchive;
      Property OnProgress;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnRead;
      Property OnFileExists;
   End;

Implementation

Uses
   ztvCrypt,
   ztvDeflate,
   Err_Msgs;

Const
   VerNum = 400;
   MinVerNum = 112;
   SRCH_ARCSTREAM = 0;
   SRCH_COMPSTREAM = 1;


//-------------------------------------------------------------

Constructor TGZip.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fMasterExt := '.gz';
   DeleteOptions := doAllowUndo;
   DefaultExt := fMasterExt;
   DeflateType := dtDeflateN;
   CompressMethodState := [cmDeflate];
   CompressMethod := cmDeflate;
   ConfirmOverwrites := True;
   fArcType := atGZip;
   fExtension := exAppendGZ;
   doSearchRecAction := SearchRecProc;
   Switch := swAdd;
   isTarGZip := False;
End;
//-------------------------------------------------------------

Destructor TGZip.Destroy;
Begin
   Inherited;
End;
//-------------------------------------------------------------

Function TGZip.Compress: Integer;
Var
   i: Integer;
   aDate: TDateTime;
   FileObject: TFileObject;
   FileDoesExist: Boolean;
   InFilename, OutFilename: String;
   LocalConfirmOverwrites: Boolean;
Begin
   (* do not change until future versions *)
   ReadMethod := faFile;
   WriteMethod := faFile;
   Result := 0;

   If ((fArchiveFile = '') And (DefaultDir = '')) Or
      	(FileSpec.Count = 0) Then
      Exit
   Else
      If (Switch = swDelete) Then
      Begin
         RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
         Exit;
      End;

   If Switch = swRead Then
   Begin
      //ReadFileInfo();
      Exit;
   End;

   If Not DiskManager.GetDriveInfo(
   								fArchiveFile,
                           RaiseError,
                           RaiseErrorStr,
   								OnDiskWriteProtectErr,
                           OnDiskInDrvErr) Then
   	Exit;


   Try
      If Assigned(OnActivate) Then OnActivate(Self);
      If Assigned(OnElapsedTime) Then ZipTimer.START;
      LocalConfirmOverwrites := ConfirmOverwrites;

      Try
      	// single file compression... directory attr
         // not acceptable
         SetAttributeEx( fsDirectory, True ); 	// default = False

         FileScan := TztvFileScan.Create(Self);
         Try
            FileScan.pHeaderObj := Nil; //pHeaderObj;
            FileScan.FileSpec := FileSpec;
            FileScan.Attributes := Attributes;
            FileScan.AttributesEx := AttributesEx;
            FileScan.UpdateFilesList := DoUpdateFilesList;
            FileScan.RecurseDirs := RecurseDirs;
            //FileScan.OnFinished := FinishedEvent;
            FileScan.OnRootDirChange := RootDirChangeEvent;
            FileScan.OnScanFile := OnFileScanStatus;
            FileScan.OnRecurseDir := OnRecurseDir;
            FileScan.pCancel := @fCancel;
            FileScan.Scan();

            CBFI.Status := hsAdd;
            pCBFI := @CBFI;

            If DefaultDir <> '' Then
            	CompressDir := DefaultDir
            Else
            	If fArchiveFile <> '' Then
            		CompressDir := ExtractFilePath(fArchiveFile)
               Else
               	CompressDir := GetCurrentDir();

            CompressDir := AppendDirTail(CompressDir);

            ProgressPosition := FileScan.FilesSize;
            fTotalUnpackedSize := FileScan.FilesSize;

            For i := 0 To FileScan.Files.Count - 1 Do
            Begin
               Application.ProcessMessages;
               If Cancel Then break;

               FileObject := FileScan.Files[i];

               InFilename := FileObject.FullPath + FileObject.FileName;

               If isTarGZip Then
               	FileObject.FileName := ExtractFileName(ArchiveFile);

               If isTarGZip Then
               	OutFilename :=
                  	CompressDir + ExtractFileName(InFileName) + DefaultExt
               Else
               	OutFilename :=
                  	CompressDir + ExtractFileName(InFileName);


               If doOnBegin(InFilename, i + 1, Nil) Then
               Begin
                  Try
                     //fFileName := CharToUnixOemFilter(fFileName, fTransOemChar);
                     //OutFilename := CompressDir + ExtractFileName(InFileName) + '.gz';
                     Case Extension Of
                     	exNone:
                        	// file-ext remains unchanged if InFileName and
                           // OutFileName differs.
                        	If OutFilename = InFileName Then
                           	OutFilename := _ChangeFileExt(OutFileName, DefaultExt);
                        exAppendGZ:	OutFilename := _ChangeFileExt(OutFileName, DefaultExt);
                        exUnderscore: OutFilename[Length(OutFilename)] := '_';
                     End;

                     If (Not ConfirmOverwrites) Then
                        fOverwriteMode := omOverwrite;

                     FileDoesExist := FileExists(OutFilename);
                     If ConfirmOverwrites And FileDoesExist Then
                     Begin
                        If Assigned(OnFileExists) Then
                        Begin
                           aDate := FileDateToDateTime(FileAge(InFilename));
                           OnFileExists(Self,
                              OutFilename,
                              aDate,
                              //ConvertDate( DateTimeToFileDate( UnixDateToDos( FileObject.FileTime ) ) ),
                              fOverwriteMode);
                        End
                        Else
                        Begin
                           RaiseErrorStr(OutFilename, 'OnFileExists', '0', E_REQUIREDEVENT);
                           Exit;
                        End;

                        If (fOverwriteMode = omOverwrite) And
                           (Not HandleNonWriteableFile(OutFilename)) Then
                           Exit;

                     End
                     Else
                        fOverwriteMode := omOverwrite;

                     If fOverwriteMode = omOverwrite Then
                     Begin
                        If FileDoesExist Then
                           EraseFile(OutFilename, DeleteOptions);

                        If PackGzFile(InFilename, OutFilename, FileObject) Then
                        	Inc( Result );
                     End;

                  Finally
                     If Assigned(OnEnd) Then
                        If (fOverwriteMode = omOverwrite) Then
                           OnEnd(Self, OutFileName, Crc32Val = CRC32 Xor CRC_MASK)
                        Else
                           OnEnd(Self, OutFileName, True);
                  End;
               End;

             //doBranchProgress( i + 1, pHeadObj^.FileCount, pHeadObj^.FileCount );
            End;

         Finally
            FileScan.Files.ClearList();
            FileScan.Free();
         End;

      Finally
         If Assigned(OnElapsedTime) Then
         Begin
            ZipTimer.Stop();
            OnElapsedTime(Self, ZipTimer.ElapsedTime);
         End;

         ConfirmOverwrites := LocalConfirmOverwrites;

         If Assigned(OnDeactivate) Then
            OnDeactivate(Self);
      End;

   Except
   End;
End;
//-------------------------------------------------------------

Function TGZip.Deflate(InStream, OutStream: TStream32): Boolean;
Const
   ORIG_NAME = $08;                     (* bit 3 set: original file name present *)
Var
   size: longint;
   DefType: TDeflateType;
   zsp: ztv_stream_plus;
 //EncryptedStream,
   CompressStream: TStream32;
Begin
   Crc32Val := CRC_MASK;
   Bytes_To_Go := InStream.size;


   If InStream.size < 1 Then
      DefType := dtDeflateS
   Else
      DefType := DeflateType;

   If CompressionMethod = cmInMemory Then
      CompressStream :=
      	TTempMemStream.Create(Self, {TempDir,} '', fmCreate)
   Else
      CompressStream :=
      	TTempFileStream.Create(Self, TempDir, '', fmCreate);

   Try
      If (InStream.size = 0) Then
      Begin
         fEncrypted := False;

         // prevent changing the BitFlag variable in
         // SetDeflateBitFlag call in FileHeaderData
         fGlobalCompressType := ZTV_STORED;
      End
      Else
      Begin
         fEncrypted := False;
         fGlobalCompressType := Z_DEFLATED;
      End;

      Crc32Val := CRC_MASK;
      fUnpackedSize := InStream.size;

      With zsp Do
      Begin
         Protect := False;
         CRC := CRC_MASK;
         pCancel := @fCancel;
         pArchivePos := @ProgressPosition;
      End;

      If fGlobalCompressType = ZTV_STORED Then
         Result := True
      Else
      Begin
         Result :=
            ztvCompressStreamProc(InStream, CompressStream, zsp,
            Crc32Val, DefType, GlobalProgress, maxbits);

         If (Not Result) Then Exit;
      End;

      // the only other way CompressMethod can equal ZTV_STORE
      // (other than if InStream.Size = 0 as defined above.
      If (CompressStream.size >= InStream.size) And
         (DefType <> dtDeflateS) And (Not fEncrypted) Then
         fGlobalCompressType := ZTV_STORED;

      Try
         GZipHeader.SignAtr := GZIP_HEADER_SIGNATURE;
         GZipHeader.CompressType := 8;  // Deflate
         //GZipHeader.BitFlag := 0;
         GZipHeader.BitFlag := ORIG_NAME;
         GZipHeader.FileNameLen := Length(fFileName);
         OutStream.WriteBuffer(GZipHeader, GZipHdr_Size);
         OutStream.WriteBuffer(fFileName[1], Length(fFileName) + 1);

      	OutStream.CancelCallBackProc := Nil;
      	OutStream.ProgressCallBackProc := Nil;
         If (fGlobalCompressType = ZTV_STORED) Then
            OutStream.CopyFrom(InStream, 0)
         Else
            OutStream.CopyFrom(CompressStream, 0);

         //CRC32 := Crc32Val Xor CRC_MASK;
         CRC32 := Crc32Val;             // v4.1.5 revised for DOS gzip v1.2.4 crc validataion
         OutStream.WriteBuffer(CRC32, SizeOf(CRC32));

         size := InStream.size;
         OutStream.WriteBuffer(size, SizeOf(size));
      Except
         Result := False;
      End;
   Finally
      CompressStream.Free();
   End;
End;
//-------------------------------------------------------------

Function TGZip.PackGzFile(InFilename, OutFilename: String; FileObject: TFileObject): Boolean;
Begin

	Result := False;

	Try
      ArcStream :=
         TFileStream32.Create(InFilename, fmOpenRead Or fmShareDenyNone);

      If (ArcStream.Handle < 0) Then
      Begin
         RaiseErrorStr(InFilename, '', '0', E_FOPEN);
         Exit;
      End;

      Try
         TempStream := TFileStream32.Create(OutFilename,
         	fmCreate Or fmShareExclusive);

         If (TempStream.Handle < 0) Then
         Begin
            RaiseErrorStr(OutFilename, '', '0', E_FOPEN);
            Exit;
         End;

         Try

            With GZipHeader Do
            Begin
               FileDate := FileObject.FileTime; //FileGetDate( inf );
               FileDate := GetDateTime(FileDate);
   {$IFDEF LOCALDEBUG}
               ShowMessage(FormatDateTime('mmddyy  hh-mm-ss', FileDateToDateTime(FileDate)));
   {$ENDIF}

               FileDate := DosDateToUnix(FileDateToDateTime(FileDate));
   {$IFDEF LOCALDEBUG}
               ShowMessage(FormatDateTime('mmddyy  hh-mm-ss', UnixDateToDos(FileDate)));
   {$ENDIF}

               // compress stream
               If Deflate(ArcStream, TempStream) Then
               Begin
               	Result := True;
               	If Assigned(OnRead) Then
                  Begin
                     fVersionMadeBy := word(VerNum);

                     If FileDate > 0 Then
                        GlobalDate := UnixDateToDos(FileDate);

                     FileName := OutFilename;
                     fMinVersion := word(MinVerNum);
                     fPackedSize := TempStream.size; //FPS;
                     fUnpackedSize := ArcStream.size; //FUS;
                     fCRC := CRC32;
                     fEncrypted := False;
                     fsCompressType := CompressionMethodUsed(CompressType);
                     fwCompressType := CompressType;
                     fRatio := CalcRatio(PackedSize, UnpackedSize);

                     OnRead(Self, 0, 1);
                  End;
               End Else
                  Result := False;

            End;
         Finally
            FileSetDate(
               THandleStream32(TempStream).Handle,
               DateTimeToFileDate(UnixDateToDos(GZipHeader.FileDate)));
            TempStream.Free();
         End;
      Finally
         ArcStream.Free();
         If Switch = swMove Then
            EraseFile(InFilename, DeleteOptions);
      End;
   Except
   	Result := False;
   End;
End;
//-------------------------------------------------------------

//Function TGZip.Write64BitFieldHdr(s: TStream32; CFI: pCompFileInfo;
//	HType: THeaderType): Integer;
//Begin
//	Result := 0;  // virtual method... do not delete.
//End;
//-------------------------------------------------------------

Procedure TGZip.SetArchiveFile(SFN: String);
Var
   FileExt: String;
Begin
   If (csLoading In ComponentState) Then
      If Not FileExists(SFN) Then
      Begin
         SetLength(fArchiveFile, 0);
         Exit;
      End;

   //CheckLoadingState( SFN );
   SFN := UnixToDosFilename(SFN);

   FileExt := ExtractFileExt(SFN);

   If Length(FileExt) = 0 Then
      fArchiveFile := Copy(SFN, 1, Length(SFN) -
         Length(ExtractFileExt(SFN))) + fDefaultExt
   Else
      fArchiveFile := SFN;

   //rem'd v4.8.6
   //fOffsetBegin := 1;
   fOffsetStart := 0;
   pCancel^ := False;
End;
//-------------------------------------------------------------

Procedure TGZip.DoUpdateFilesList(Const Dir: String; FindData: TWin32FindData;
   pHeaderObj: pCompHeaderObj);
Var
   DateTime: Integer;
Begin
   DateTime := ztvGbls.FileTimeToInt(FindData.ftLastWriteTime);
   //ShowMessage( FormatDateTime( 'mmddyy hh:mm', FileDateToDateTime( DateTime )) );

   If (DateTime > fMaxAge) Then
      fMaxAge := DateTime;

   FileScan.DoUpdateFileList(Dir, FindData, pHeaderObj);
End;
//-------------------------------------------------------------

Function TGZip.CompressionMethodUsed(method: word): String;
Var
   s: String;
Begin
   Case method Of
      0: s := 'Stored';
      8: s := GetDeflateMethodStr(GZipHeader.BitFlag);
   Else
      s := 'Unknown';
   End;
   Result := s;
End;
//-------------------------------------------------------------


End.
