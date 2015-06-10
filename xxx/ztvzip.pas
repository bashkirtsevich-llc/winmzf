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

(* NOTES:
   OnDiskSpanFileExists:
    1. The TDiskSpanObj object hooks into the OnFileExists event to alert
     the user that an existing file already exists on the diskette.
    2. This event is NOT activated when a matching file is found in an
      existing file when adding/refreshing files to an archive... use the
      OnRenameDupeFile event for this purpose.
   *)
Unit ztvZip;

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
   ztvCrypt,
   ztvGbls,
   ztvFileIo,
   ztvHeaders,
   ztvStreams;


{$I ZipTV.inc}


Type
   TDiskSpanObj = Class;
   pStream = ^TStream32;

   TZipBase = Class(TCompBase)
   Private
      DiskSpanObj: TDiskSpanObj;
      EndHeaderPos: Cardinal;
      FilesSkipped: Integer;
      fLeaveSpaceOnFirstDisk: Integer;
      fOnFileExists: TOnFileExists;
      HeadObj: TCompHeaderObj;
		Central64Hdr: TCentral64Hdr;
   	Is64BitArchive: Boolean;
      pHeadCommentObj: pCompHeaderObj;
   Protected
      Function AdjPtr(p: PChar): Pointer;
      Function GetFirst(Var FileName: String; s: TStream32; Var HeadType: Byte;
         pHeaderObj: pCompHeaderObj): Boolean; Override;
      Function GetNext(Var FileName: String; strm: TStream32; HeadType: Byte):
         Boolean; Override;
      Function GetHeadSize: word; Override;
      Function GetLocalHeaderSize: Integer; Override;
      Function GetHeadPtr: Pointer; Override;
      Function ReadHeader(s: TStream32; Var FileName: String;
         HeadType: Byte): Boolean; Override;
      Function WriteCentralDirectory(strm: TStream32; pHeaderObj:
         pCompHeaderObj): Boolean;
      Function Write64BitFieldHdr(s: TStream32; pCBFI: pCompFileInfo;
      	HType: THeaderType): Integer; Override;
      Procedure doOnEnd; Override;
      Procedure doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
         pHeaderObj: pCompHeaderObj); Override;
      Procedure FillHeaderData(InStreamSize, CompressStreamSize: Int64); Override;
      Procedure InitializeHeader(pCBFI: pCompFileInfo); Override;
      Procedure PackFile(Outfile: TStream32; Index: Integer; pHeaderObj:
         pCompHeaderObj; Var Result: Boolean); Override;
      Procedure RefreshHeader(Index, ExtAttr: Integer; pHeaderObj:
         pCompHeaderObj); Override;
      Procedure SetArcType(SAT: TArcType); Override;
      Procedure SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo); Override;
      Procedure WriteZeroByteZipHeader(TempStream: TStream32); Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure CommentObjDone; Override;
      Procedure SetArchiveComment(Comment: PChar; CommentLen: word); Override;
      Procedure GetComment(Index: Integer; Var FileName: String;
         Var Comment: PChar; Var CommentLen: word); Override;
      Procedure SetComment(Index: Integer; Comment: PChar; CommentLen: word); Override;
      Procedure ArcToList(s: TStream32; pHeaderObj: pCompHeaderObj); Override;
      Procedure doFinish(pHeaderObj: pCompHeaderObj); Override;
      Function CommentObjInit: Integer; Override;
      Function doCleanUp(TempFile: TStream32; pHeaderObj: pCompHeaderObj): Boolean; Override;
      Function GetArchiveComment(Var Comment: PChar; Var Len: word): PChar; Override;
      Function GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo; Override;
      Function GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
         Pointer): Int64; Override;
   Published
      Property ArcType;
      Property AttributesEx;
      Property CompressMethod;
      Property DefaultExt;
      Property DeflateType;
      Property EncryptHeaders;
      Property IncludeHiddenDirs;
      Property LeaveSpaceOnFirstDisk: Integer Read fLeaveSpaceOnFirstDisk
         Write fLeaveSpaceOnFirstDisk Default 0;
      Property Password;
      Property FileSpec;
      //Property OnRecurseDir;	// v4.8 replaced by OnFileScanStatus
      //Property StoreAlreadyCompressedFiles;	//v4.6.8 removed.  Added StoreFilesOfType property
      Property StoredDirNames;
      Property StoreFilesOfType;
      Property StoreEmptySubDirs;
      Property TempDir;
      Property VerifyBeforeDelete;
      Property OnBegin;
      Property OnClearDisk;
		Property OnDiskWriteProtectErr;
      Property OnDiskInDrvErr;
      Property OnDiskSpanFileExists: TOnFileExists Read fOnFileExists Write fOnFileExists;
      Property OnEnd;
      Property OnError;
      Property OnGetPassword;
      Property OnInsertDisk;
      Property OnNonWriteableArchive;
      Property OnProgress;
      Property OnRead;
      //Property OnRenameFile;	// rem'd until revised for better understanding
      Property OnRenameDupeFile;
      Property OnReplaceFile;
      Property OnTmpFileMoveBegin;
      Property OnTmpFileMoveEnd;
  	End;

   TZip = Class(TZipBase)
   Private
   Protected
   Public
   Published
      Property CompressionMethod;
      Property Switch;
		Property OnTmpFileProgress;
   End;

   TDiskSpanObj = Class(TDiskCommon)
   Private
      fOverwriteMode: TOverwriteMode;
      fRequiredSpace: Cardinal;
      fSerialNumber: DWord;
      fVolumeNumber: Integer;
      pInternalCancel: pBoolean;
      pHeadObj: pCompHeaderObj;
      pDiskMan: pDiskManager;
      pVolumeInfo: ^TVolumeInformation;
      _OnClearDisk: TOnClearDisk;
      _OnInsertDisk: TOnInsertDisk;
      _OnFileExists: TOnFileExists;
      _RaiseErrorStr: TRaiseErrorStr;
      InStream: TStream32;
      OutStream: TFileStream32;
      ZipObj: TZip;
      Function ClearDiskProc: Boolean;
      Function GetMinSize(Const size: Integer): Integer;
      Function InsertDiskProc: Boolean;
      Function OpenOutputFile: Boolean;
      Function RequestFirst: Boolean;
      Function RequestNext: Boolean;
      Function WriteDiskLabel: Boolean;
      Function WriteToFloppy(strm: TStream32; Var WriteObj; size: Integer): Boolean;
   Protected
   Public
      Constructor Create(ZO, pDrv, ps, pDM, pHO, pC: Pointer); Virtual;
      Destructor Destroy; Override;
      Function Activate: Boolean;
      Function ChangeDiskLabel(NewLabel: String): Boolean;
      Function CopyBlock(size: Integer): Boolean;
   End;


Const
   REQUIRED_FREE_SPACE = 10000;         // required diskette space for data writes


Implementation


Uses
   ztvDeflate,
   Err_Msgs;

Const
   LOCAL_HEAD = 0;
   CENTRAL_HEAD = 1;
   VERSIONNUM = 20;
   MINVERSIONNUM = 20;


//------------------------------------------------------------

Constructor TZipBase.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fMasterExt := '.ZIP';
   DefaultExt := fMasterExt;
   fArcType := atZip;
   fLeaveSpaceOnFirstDisk := 0;
   EncryptHeaders := False;
   CompressMethodState := [cmStore, cmDeflate];
   CompressMethod := cmDeflate;
End;
//------------------------------------------------------------

Destructor TZipBase.Destroy;
Begin
   Inherited;
End;
//-------------------------------------------------------------

Procedure TZipBase.GetComment(Index: Integer; Var FileName: String;
   Var Comment: PChar; Var CommentLen: word);
Var
   pCBFI: pCompFileInfo;
Begin
   With pHeadCommentObj^ Do
      If FileCount > 0 Then
      Begin
         CentralZipHeader := TCentral(Header[Index]^);
         FileName := FileList.Strings[Index];

         pCBFI := FileLocationData(Index);
         CommentLen := pCBFI^.FileCommentLen;
         If CommentLen > 0 Then
         Begin
            Comment := pCBFI^.FileComment;
            Comment[CommentLen] := #0;
         End
         Else
            Comment := Nil;

      End
      Else
      Begin
         SetLength(FileName, 0);
         Comment := #0;
         CommentLen := 0;
      End;
End;
//------------------------------------------------------------

Function TZipBase.GetArchiveComment(Var Comment: PChar; Var Len: word): PChar;
Begin
   With pHeadCommentObj^ Do
   Begin
      If (ArchiveCommentLen > 0) Then
         Comment := ArchiveComment
      Else
         Comment := Nil;

      Len := ArchiveCommentLen;
   End;
   Result := Comment;
End;
//------------------------------------------------------------

Procedure TZipBase.SetArchiveComment(Comment: PChar; CommentLen: word);
Var
   Changed: Boolean;
Begin
   If (Comment <> Nil) Then
      With pHeadCommentObj^ Do
      Begin
         Changed := (CommentLen <> ArchiveCommentLen) Or
            (Not CompareMem(Comment, ArchiveComment, CommentLen));

         CommentChanged := CommentChanged Or Changed;
         If (Not Changed) Then Exit;

         If (ArchiveCommentLen > 0) Then
            FreeMem(ArchiveComment, ArchiveCommentLen + 1);

         ArchiveCommentLen := CommentLen;
         If ArchiveCommentLen > 0 Then
         Begin
            GetMem(ArchiveComment, ArchiveCommentLen + 1);
            ZeroMemory(ArchiveComment, ArchiveCommentLen + 1);
            CopyMem(Comment, ArchiveComment, ArchiveCommentLen);
         End;
      End;
End;
//------------------------------------------------------------

Procedure TZipBase.SetComment(Index: Integer; Comment: PChar; CommentLen: word);
Var
   pCBFI: pCompFileInfo;
   Changed: Boolean;
Begin

   With pHeadCommentObj^ Do
      If (FileCount > 0) And (Index <= FileCount) Then
      Begin
         pCBFI := FileLocationData(Index);

         // compare integer lengths
         Changed := (CommentLen <> pCBFI^.FileCommentLen) Or

          // integer lengths match, compare the actual comments
         (Not CompareMem(Comment, pCBFI^.FileComment, CommentLen));

         CommentChanged := CommentChanged Or Changed;
         If (Not Changed) Then Exit;

         TCentral(Header[Index]^).CommentLen := CommentLen;
         If pCBFI^.FileCommentLen > 0 Then
            FreeMem(pCBFI^.FileComment, pCBFI^.FileCommentLen);

         pCBFI^.FileCommentLen := CommentLen;
         If CommentLen > 0 Then
         Begin
            GetMem(pCBFI^.FileComment, CommentLen);
            CopyMem(Comment, pCBFI^.FileComment, CommentLen);
         End
         Else
            pCBFI^.FileComment := Nil;

      End;
End;
//------------------------------------------------------------

Function TZipBase.CommentObjInit: Integer;
Var
   HoldSwitch: TSwitch;
   ArcFile: TFileStream32;
Begin
   Result := 0;
   Count := 0;
   If (ArchiveFile = '') Or (Not (ArcType In Zip_ArcType)) Then Exit;

   ArcFile := TFileStream32.Create(fArchiveFile, fmOpenRead);
   If (ArcFile.Handle < 0) Then
   Begin
      RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN);
      Exit;
   End;

   Try
      HoldSwitch := Switch;
      Try
         CopySuccess := False;
         Switch := swAdd;
         HeadObj := TCompHeaderObj.Create();
         pHeadCommentObj := @HeadObj;
         pHeadCommentObj^.INIT();
         ArcToList(ArcFile, pHeadCommentObj);
      Finally
         Switch := HoldSwitch;
         Result := pHeadCommentObj^.FileCount;
      End;
   Finally
      ArcFile.Free();
   End;
End;
//------------------------------------------------------------

Procedure TZipBase.CommentObjDone;
Var
   ArcFile: TFileStream32;
   TempFile: TTempFileStream;
Begin
   Try
      If (Cancel) Or (Not pHeadCommentObj^.CommentChanged) Then
         Exit;

      TempFile :=
      	TTempFileStream.Create(Self, TempDir, fArchiveFile, fmCreate);

      TempFile.DeleteOptions := DeleteOptions;
      TempFile.CancelCallBackProc := Nil;
      TempFile.ProgressCallBackProc := Nil;

      Try
         ArcFile :=
         	TFileStream32.Create(fArchiveFile, fmOpenRead);

         If (ArcFile.Handle < 0) Then
         Begin
            RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN);
            Exit;
         End;

         Try
            ArcFile.Position := 0;
            TempFile.CopyFrom(ArcFile, EndZipHeader.CentralDirOffset);
            Count := pHeadCommentObj^.FileList.Count;
         Finally
            ArcFile.Free();
         End;

         doCleanUp(TempFile, pHeadCommentObj);
      Finally
         TempFile.Free();
      End;
   Finally
      pHeadCommentObj^.DONE();
      pHeadCommentObj.Free();
   End;
End;
//------------------------------------------------------------

Procedure TZipBase.PackFile(Outfile: TStream32; Index: Integer; pHeaderObj:
   pCompHeaderObj; Var Result: Boolean);
Begin
   Result := False;
   Try
      InitializeHeader(pCompFileInfo(pHeaderObj^.DataLocation[Index]));
      HPtr := GetHeadPtr();
      HSize := GetLocalHeaderSize();
      If CompressMethod = cmStore Then
         DeflateType := dtDeflateS;

      Inherited;
   Except
      Raise;
      {n EOutOfMemory Do
      	Raise;

      On EOutOfResources Do
      	Raise;}
   End;
End;
//------------------------------------------------------------

Procedure TZipBase.RefreshHeader(Index, ExtAttr: Integer; pHeaderObj: pCompHeaderObj);
Begin
   ZeroMemory(@CentralZipHeader, SizeOf(TCentral));
   With CentralZipHeader Do
   Begin

      SignAtr := DefSig(htCentral, pCBFI^.EncryptedHeader);
      VerNum := VERSIONNUM;
      MinVerNum := MINVERSIONNUM;

      If EncryptHeaders Then
      Begin
         DecodeHeader(@LocalZipHeader, htLocal);
         // do not decode fFileName.  The fFileName variable is
         // no longer encoded.  A replacement variable is used
         // for this variable to encode the filename.
      End;

      CopyMem(@LocalZipHeader.zc, @CentralZipHeader.zc, SizeOf(Tzc));

		zc.ExtraFieldLen := pCBFI^.ExtraFieldLen;
      CommentLen := 0;
      DiskNumberStart := 0;
      ExternalAttr := ExtAttr;
      InternalAttr := 1;
      RelativeOffsetOfLocalHeader := pCBFI^.Offset;
      CopyMem(@CentralZipHeader, pHeaderObj^.Header[Index], SizeOf(TCentral));
   End;
End;
//------------------------------------------------------------

Function TZipBase.ReadHeader(s: TStream32; Var FileName: String; HeadType: Byte): Boolean;

	Function GetFilename(Var FileName: String; Len: Integer; Encrypted: Boolean): Boolean;
   Begin
      SetLength(FileName, Len);
      If Len > 0 Then
      Begin
      	ZeroMemory(@FileName[1], Len + 1);
      	Result := s.Read(FileName[1], Len) = Len;
         If Result Then
         Begin
            If Encrypted Then
               DecodeFilename(@FileName[1], Len);

            If Result Then
               FileName := OemToCharFilter(FileName, fTransOemChar);
      	End;
      End Else
      	Result := False;
   End;

Var
   Encrypted: Boolean;
Begin
   s.Position := LastPos;

   If HeadType = LOCAL_HEAD Then
      With LocalZipHeader Do
      Begin
         SignAtr := 0;
         Result := s.Read(LocalZipHeader, SizeOf(TLocal)) = SizeOf(TLocal);

         If Result Then
         Begin
            If VerSig(SignAtr, htLocal, Encrypted) = htLocal Then
            Begin
               If Encrypted Then
                  DecodeHeader(@LocalZipHeader, htLocal);

               Result := GetFilename(FileName, zc.FileNameLen, Encrypted);
   				Central64Hdr :=
               	Read64BitFieldHdr(
                  	s,
                     zc.ExtraFieldLen,
                     htCentral);

               If Result Then
                  LastPos :=
                     ((Int64(Central64Hdr.HiPackedSize) Shl 32) Or
                     zc.PackedSize) + SizeOf(TLocal) + Length(FileName) +
                     zc.ExtraFieldLen + LastPos;

            End Else
               Result := False;

         End;

      End
   Else
      With CentralZipHeader Do
      Begin                                            
         SignAtr := 0;
         Result := s.Read(CentralZipHeader, SizeOf(TCentral)) = SizeOf(TCentral);
         If Result Then
         Begin
            If (VerSig(SignAtr, htCentral, Encrypted) = htCentral) Then
            Begin
               If Encrypted Then
                  DecodeHeader(@CentralZipHeader, htCentral);

               Result := GetFilename(FileName, zc.FileNameLen, Encrypted);
   				Central64Hdr :=
               	Read64BitFieldHdr(
                  	s,
                     zc.ExtraFieldLen,
                     htCentral);

            End Else
               Result := False;

            {If Result Then} LastPos := s.Position + {zc.ExtraFieldLen +} CommentLen;
         End;
      End;

End;
//------------------------------------------------------------

// write the local extended field header.
Function TZipBase.Write64BitFieldHdr(s: TStream32; pCBFI: pCompFileInfo;
	HType: THeaderType): Integer;
Var
	pExtFileHeader: ^TCentral64Hdr;
Begin
   If (pCBFI^.HighUnpackedSize > 0) Or (pCBFI^.Offset > MAXDWORD) Then
   Begin

      // must be SizeOf(TCentral64Hdr), even with a htLocal write.  The
      // reason is to retain this value for when this files central
      // header is written to the stream, after all files are compressed.
      // This value actually has no baring to the local header write.
      pCBFI^.ExtraFieldLen := SizeOf(TCentral64Hdr);
      If pCBFI^.ExtraField = Nil Then
         GetMem(pCBFI^.ExtraField, pCBFI^.ExtraFieldLen);

      pExtFileHeader := pCBFI^.ExtraField;

      pExtFileHeader^.ExtendedFieldHdr.HeadID := SIXTYFOUR_BIT_HDR_ID_ZIPTV;
      pExtFileHeader^.HiPackedSize := pCBFI^.HighPackedSize;
      pExtFileHeader^.HiUnpackedSize := pCBFI^.HighUnpackedSize;
      pExtFileHeader^.HiOffsetToLocal := pCBFI^.Offset Div MAXDWORD;

      // TLocal64Hdr is actually TCentral64Hdr minus the HiOffsetToLocal
      // variable.  With a local header write, write SizeOf(TCentral64Hdr) -
      // SizeOf(TLocal64Hdr.HiOffsetToLocal)
      pExtFileHeader^.ExtendedFieldHdr.HeadSize :=
      	SizeOf(TLocal64Hdr) - SizeOf(TExtendedFieldHdr);

      s.Write(pExtFileHeader^, SizeOf(TLocal64Hdr));

      // set HeaderSize for central dir write later on.
      pExtFileHeader^.ExtendedFieldHdr.HeadSize :=
      	SizeOf(TCentral64Hdr) - SizeOf(TExtendedFieldHdr);
   End;
End;
//------------------------------------------------------------

Function TZipBase.GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
	Pointer): Int64;
Var
   Encrypted: Boolean;
Begin
	ZeroMemory(@LocalZipHeader, SizeOf(TLocal));
   ArcFile.Read(LocalZipHeader, SizeOf(TLocal));
   If VerSig(LocalZipHeader.SignAtr, htLocal, Encrypted) <> htLocal Then
   Begin
      Result := 0;
      Exit;
   End;

   If LocalZipHeader.zc.PackedSize = 0 Then
   	LocalZipHeader.zc.PackedSize := TCentral(pHeader^).zc.PackedSize;

   If LocalZipHeader.zc.UnpackedSize = 0 Then
   	LocalZipHeader.zc.UnpackedSize := TCentral(pHeader^).zc.UnpackedSize;

   If Encrypted Then
      DecodeHeader(@LocalZipHeader, htLocal);

	ArcFile.Seek(LocalZipHeader.zc.FileNameLen, soCurrent);
   Central64Hdr :=
   	Read64BitFieldHdr(
      	ArcFile,
         LocalZipHeader.zc.ExtraFieldLen,
         htLocal);

   TCentral(pHeader^).RelativeOffsetOfLocalHeader :=
      TempFile.size;

   With LocalZipHeader Do
      Result :=
         ((Int64(Central64Hdr.HiPackedSize) Shl 32) Or zc.PackedSize) +
         GetLocalHeaderSize() +
         zc.FileNameLen +
         zc.ExtraFieldLen;
End;
//------------------------------------------------------------

Function TZipBase.WriteCentralDirectory(strm: TStream32; pHeaderObj:
	pCompHeaderObj): Boolean;
Var
   i: Integer;
   Comment: PChar;
Begin
   Result := False;

   If Is64BitArchive Then  // defined in calling routine
   Begin
      EndZipHeader64.DiskWithStartOfCentral := EndZipHeader64.NumberOfThisDisk;
      EndZipHeader64.CentralDirOffset := strm.size;
   End Else Begin
   	EndZipHeader.DiskWithStartOfCentral := EndZipHeader.NumberOfThisDisk;
      EndZipHeader.CentralDirOffset := strm.size;
   End;

   For i := 0 To (pHeaderObj^.FileCount - 1) Do
   Begin
      pCBFI := pHeaderObj^.FileLocationData(i);
      If pCBFI^.Status = hsSkip Then
      Begin
         inc(FilesSkipped);
         Continue;
      End;

      CentralZipHeader := TCentral(pHeaderObj^.Header[i]^);
      CentralZipHeader.SignAtr := DefSig(htCentral, pCBFI^.EncryptedHeader);

      If pCBFI^.EncryptedHeader Then
         EncodeHeader(@CentralZipHeader, htCentral);

      If (DiskManager.DriveType = dtFloppy) Then
         DiskSpanObj.WriteToFloppy(strm, CentralZipHeader, SizeOf(TCentral))
      Else
         If strm.Write(CentralZipHeader, SizeOf(TCentral)) <> SizeOf(TCentral) Then
            Exit;

      fFileName := CharToUnixOemFilter(pHeaderObj^.FileList.Strings[i], fTransOemChar);

      If pCBFI^.EncryptedHeader Then
         EncodeFilename(@fFileName[1], Length(fFileName));

      If (DiskManager.DriveType = dtFloppy) Then
         DiskSpanObj.WriteToFloppy(strm, fFileName[1], Length(fFileName))
      Else
         If strm.Write(fFileName[1], Length(fFileName)) <> Length(fFileName) Then
            Exit;

      If pCBFI^.ExtraFieldLen > 0 Then
         If (DiskManager.DriveType = dtFloppy) Then
            DiskSpanObj.WriteToFloppy(strm, pCBFI^.ExtraField^,
               pCBFI^.ExtraFieldLen)
         Else
            If strm.Write(pCBFI^.ExtraField^, pCBFI^.ExtraFieldLen) <> pCBFI^.ExtraFieldLen Then
               Exit;

      If pCBFI^.FileCommentLen > 0 Then
      Begin
         Comment := pCBFI^.FileComment;
       	//If EncryptHeaders Then
         //	EncodeComment( @Comment, pCBFI^.FileCommentLen );
         If (DiskManager.DriveType = dtFloppy) Then
            DiskSpanObj.WriteToFloppy(strm, Comment[0], pCBFI^.FileCommentLen)
         Else
            If strm.Write(Comment[0], pCBFI^.FileCommentLen) <> pCBFI^.FileCommentLen Then
               Exit;
      End;

   End;

   If Is64BitArchive Then
      EndZipHeader64.SizeOfCentralDir :=
         strm.Position - EndZipHeader64.CentralDirOffset
   Else
      EndZipHeader.SizeOfCentralDir :=
         strm.Position - EndZipHeader.CentralDirOffset;

   Result := pHeaderObj^.FileCount > FilesSkipped;
End;
//------------------------------------------------------------

Function TZipBase.GetFirst(Var FileName: String; s: TStream32; Var HeadType: Byte;
   pHeaderObj: pCompHeaderObj): Boolean;

   // returns HeadType to search
   Function FindHeader: ShortInt;
   Var
      cLen: Word;
      pl: ^Integer;
      pBlock: PChar;
      Encrypted: Boolean;
      x,
      	BufSize: Integer;
   Begin
      Result := LOCAL_HEAD;
      ZeroMemory(@EndZipHeader, SizeOf(TEnd));

      // use the following block, instead of the min function...
      // the min function fails with files > 4 gig.
      //BufSize := Min(s.size, WSIZE);
      If s.size > WSIZE Then
         BufSize := WSIZE
      Else
         BufSize := s.size;

      GetMem(pBlock, BufSize + 1);
      Try

         (* set entry pointer into file *)
         s.Position := s.size - BufSize;
         If (s.Read(pBlock^, BufSize) <> BufSize) Then
            Exit;

         // here we don't have to be concerned with either a 32-bit or 64-bit
         // ending header... we're using only the SignAtr variable at the
         // beginning of TEnd which matches the same variable in size, record
         // placement, and name in TEnd64.
         For x := (BufSize - (SizeOf(TEnd))) Downto 1 Do
         Begin
            pl := @pBlock[x];
            If (VerSig(pl^, htEnding, Encrypted) = htEnding) Then
            Begin

              	// the start of EndZipHeader is located at the following position:
            	If is64BitEndingHdr(pl^) Then
               Begin
               	CopyMem(@pBlock[x], @EndZipHeader64, SizeOf(TZipTV_End64));
               	CopyMem(@pBlock[x], @EndZipHeader, SizeOf(EndZipHeader.SignAtr));
                  cLen := EndZipHeader64.CommentLen;
               End Else Begin
               	CopyMem(@pBlock[x], @EndZipHeader, SizeOf(TEnd));
                  cLen := EndZipHeader.CommentLen;
               End;

               EndHeaderPos := s.size - BufSize + x;
               If cLen > 0 Then
               Begin
                  GetMem(pHeaderObj.ArchiveComment, cLen + 1);
                  ZeroMemory(pHeaderObj.ArchiveComment, cLen + 1);
                  pHeaderObj.ArchiveCommentLen := cLen;

            		If is64BitEndingHdr(pl^) Then
                  Begin
                  	s.Position := EndHeaderPos + SizeOf(TZipTV_End64);
                  	s.Read(pHeaderObj.ArchiveComment[0], EndZipHeader64.CommentLen);
                  End Else Begin
                  	s.Position := EndHeaderPos + SizeOf(TEnd);
                  	s.Read(pHeaderObj.ArchiveComment[0], EndZipHeader.CommentLen);
                  End;
               End;

               If Encrypted Then
            		If is64BitEndingHdr(pl^) Then
                  	DecodeHeader(@EndZipHeader64, htEnding)
                  Else
                  	DecodeHeader(@EndZipHeader, htEnding);

               If is64BitEndingHdr(pl^) Then
               	s.Position := EndZipHeader64.CentralDirOffset
               Else
               	s.Position := EndZipHeader.CentralDirOffset;

               If (s.Read(CentralZipHeader, SizeOf(TCentral)) <> SizeOf(TCentral)) Then
                  Exit;

               VerSig(CentralZipHeader.SignAtr, htCentral, Encrypted);
               If Encrypted Then
                  DecodeHeader(@CentralZipHeader, htCentral);

               // seek past filename
               s.seek(CentralZipHeader.zc.FileNameLen, soCurrent);
               Central64Hdr :=
                  Read64BitFieldHdr(
                  	s,
                     CentralZipHeader.zc.ExtraFieldLen,
                     htCentral);

               fOffsetStart :=
               	(Int64(Central64Hdr.HiOffsetToLocal) Shl 32) Or
                  CentralZipHeader.RelativeOffsetOfLocalHeader;

               Break;
            End;
         End;
      Finally
         FreeMem(pBlock, BufSize + 1);
      End;

      If VerSig(EndZipHeader.SignAtr, htEnding, Encrypted) = htEnding Then
      	Result := CENTRAL_HEAD;
   End;

Begin
   HeadType := FindHeader();

   Case HeadType Of
		LOCAL_HEAD:
      	Begin
      		LastPos := 0;
            Try
               Result := ReadHeader(s, FileName, HeadType);
            Except
               Result := False;
            End;
         End;
      CENTRAL_HEAD:
      	Begin
         	If is64BitEndingHdr(EndZipHeader.SignAtr) Then
      			LastPos := EndZipHeader64.CentralDirOffset
            Else
      			LastPos := EndZipHeader.CentralDirOffset;

            Try
               Result := ReadHeader(s, FileName, HeadType);
            Except
               Result := False;
            End;
         End;
   Else
   	Result := False;
   End;
End;
//------------------------------------------------------------

Function TZipBase.GetNext(Var FileName: String; strm: TStream32;
   HeadType: Byte): Boolean;
Begin
   PrevPos := LastPos;
   If LastPos = 0 Then
      Result := False
   Else
      Result := ReadHeader(strm, FileName, HeadType);
End;
//-------------------------------------------------------------
Procedure TZipBase.ArcToList(s: TStream32; pHeaderObj: pCompHeaderObj);
Var
   HeadType: Byte;
   FileName: String;
   CBFInew: TCompFileInfo;
   LocalPos,
   	DeletedBytes: Int64;
Begin

   DeletedBytes := 0;
   LocalPos := LastPos;
   If GetFirst(FileName, s, HeadType, pHeaderObj) Then
      Repeat
         If (Not CopySuccess) Then
         Begin

            CBFInew.Status := hsCopy; // default
            If (Switch = swDelete) Then
            Begin

               If CheckWildCard1(FileName, FileSpec, ExcludeSpec) Then
               Begin
                  // if HeadType = LOCAL_HEAD then LocalZipHeader
                  // has already been read
                  If HeadType = CENTRAL_HEAD Then
                  Begin
                     s.Position :=
                        ((Int64(Central64Hdr.HiOffsetToLocal) Shl 32) Or
                        CentralZipHeader.RelativeOffsetOfLocalHeader);

                  	s.Read(LocalZipHeader, SizeOf(TLocal));
                  End;

                  DeletedBytes :=
							((Int64(Central64Hdr.HiPackedSize) Shl 32) Or LocalZipHeader.zc.PackedSize) +
                  	SizeOf(TLocal) +
                     LocalZipHeader.zc.FileNameLen +
                     LocalZipHeader.zc.ExtraFieldLen +
                     DeletedBytes;

                  inc(FilesDeleted);

                  CBFInew.Offset := 0;
                  CBFInew.FileCommentLen := 0;
                  CBFInew.ExtraFieldLen := 0;

                  If HeadType = LOCAL_HEAD Then
                  	CBFInew.EncryptedHeader := IsHeaderEncrypted(LocalZipHeader.SignAtr)
                  Else
                  	CBFInew.EncryptedHeader := IsHeaderEncrypted(CentralZipHeader.SignAtr);

                  CBFInew.Status := hsSkip;

                  pHeaderObj^.AddItem(CBFInew, FileName, @CentralZipHeader,
                     Length(fRootDir), SizeOf(TCentral));

                  Continue;
               End;
            End;

            Case HeadType Of
            	LOCAL_HEAD:
               	With LocalZipHeader Do
                  Begin
            			CBFInew.Offset := LocalPos;
                     CBFInew.EncryptedHeader := IsHeaderEncrypted(SignAtr);
                     CBFInew.FileComment := Nil;

                  	CBFInew.FileCommentLen := 0;
                  	CBFInew.ExtraFieldLen := zc.ExtraFieldLen;

                     If zc.ExtraFieldLen > 0 Then
                     Begin
                        GetMem(CBFInew.ExtraField, zc.ExtraFieldLen + 1);
                        s.Position := LastPos - zc.ExtraFieldLen;
                        s.Read(CBFInew.ExtraField^, zc.ExtraFieldLen);
                     End Else
                        CBFInew.ExtraField := Nil;

                     fTotalUnpackedSize :=
                        fTotalUnpackedSize + SizeOf(TLocal) + zc.FileNameLen +
                        zc.ExtraFieldLen + zc.PackedSize;

                  	CentralZipHeader.SignAtr := CENTRAL_FILE_HEADER_SIGNATURE;
                     CentralZipHeader.RelativeOffsetOfLocalHeader := LocalPos;
                     Move(LocalZipHeader.zc, CentralZipHeader.zc, SizeOf(Tzc));

                  End;
               CENTRAL_HEAD:
               	With CentralZipHeader Do
                  Begin

                     CBFInew.FileCommentLen := CommentLen;
                     CBFInew.ExtraFieldLen := zc.ExtraFieldLen;
                     CBFInew.EncryptedHeader := IsHeaderEncrypted(SignAtr);

          				CBFInew.HighPackedSize := Central64Hdr.HiPackedSize;
         				CBFInew.HighUnpackedSize := Central64Hdr.HiUnpackedSize;

							CBFInew.Offset :=
                        ((Int64(Central64Hdr.HiOffsetToLocal) Shl 32) Or
                        RelativeOffsetOfLocalHeader);

                     RelativeOffsetOfLocalHeader :=
                        ((Int64(Central64Hdr.HiOffsetToLocal) Shl 32) Or
                        RelativeOffsetOfLocalHeader) - DeletedBytes;

                     If zc.ExtraFieldLen > 0 Then
                     Begin
                        GetMem(CBFInew.ExtraField, zc.ExtraFieldLen + 1);
                        s.Position := LastPos - zc.ExtraFieldLen;
                        s.Read(CBFInew.ExtraField^, zc.ExtraFieldLen);

                        //revise the ExtraField which is written to the
                        //central header
                        If (Switch = swDelete) And (DeletedBytes > 0) Then
									TCentral64Hdr(CBFInew.ExtraField^).HiOffsetToLocal :=
                        		(CBFInew.Offset - DeletedBytes) Div MAXDWORD;
                     End
                     Else
                        CBFInew.ExtraField := Nil;

                     If CommentLen > 0 Then
                     Begin
                        GetMem(CBFInew.FileComment, CommentLen + 1);
                        s.Position := LastPos - (zc.ExtraFieldLen + CommentLen);
                        s.Read(CBFInew.FileComment[0], CommentLen);
                     End
                     Else
                        CBFInew.FileComment := Nil;

                     fTotalUnpackedSize :=
                        ((Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or
                        zc.UnpackedSize) + SizeOf(TLocal) + zc.FileNameLen +
                        zc.ExtraFieldLen + fTotalUnpackedSize;
                  End;

            End;

            pHeaderObj^.AddItem(CBFInew, FileName, @CentralZipHeader,
               Length(fRootDir), SizeOf(TCentral));

         End
         Else
            pHeaderObj^.FileList.Add(FileName);

         LocalPos := LastPos;
      Until (Not GetNext(FileName, s, HeadType));
End;
//------------------------------------------------------------

Function TZipBase.doCleanUp(TempFile: TStream32; pHeaderObj: pCompHeaderObj): Boolean;
Begin
	FilesSkipped := 0;
   Result := False;
   DoProgress(0, 0);
   CopySuccess := False;

   Try
      If (Count > 0) Then
      Begin
      	Is64BitArchive := TempFile.size > MAXDWORD;

         If Is64BitArchive Then
         Begin
         	ZeroMemory(@EndZipHeader64, SizeOf(TZipTV_End64));	{ leave the CommentLen variable unchanged }
         	EndZipHeader64.SignAtr := END_OF_CENTRAL64_HEADER_SIGNATURE //DefSig( htEnding );
         End Else Begin
         	ZeroMemory(@EndZipHeader, SizeOf(TEnd));	{ leave the CommentLen variable unchanged }
         	EndZipHeader.SignAtr := END_OF_CENTRAL_HEADER_SIGNATURE; //DefSig( htEnding );
         End;


         If (DiskManager.DriveType = dtFloppy) Then
         Begin
            Result := True;

            { archive comment not supported with disk-spanning }
            //EndZipHeader.CommentLen := 0;

            DiskSpanObj :=
               TDiskSpanObj.Create(@Self,
               @fArchiveFile,
               @TempFile,
               @DiskManager,
               pHeaderObj,
               @fCancel);

            Try
               DiskSpanObj.pVolumeInfo := @DiskManager.VolumeInfo;
               DiskSpanObj._RaiseErrorStr := RaiseErrorStr;
               DiskSpanObj._OnInsertDisk := OnInsertDisk;
               DiskSpanObj._OnClearDisk := OnClearDisk;
               DiskSpanObj._OnFileExists := OnDiskSpanFileExists;

               If (Not DiskSpanObj.Activate()) Then
               Begin
                  FilesCompressed := 0;
                  pHeaderObj^.CLEAR_LIST;
               End;
            Finally
               DiskSpanObj.Free();
            End;

         End
         Else
         Begin

            CopySuccess := False;

            // No central dir writes necessary if all files were deleted
            If (Switch = swDelete) And (FilesDeleted > 0) And
               (FilesDeleted = pHeaderObj^.FileList.Count) Then
               Exit;

            If (Not WriteCentralDirectory(TempFile, pHeaderObj)) Then
            Begin
               RaiseErrorStr(fArchiveFile, '', '0', E_FWRITE);
               Exit;
            End;

            If Is64BitArchive Then
            Begin
               With EndZipHeader64 Do
               Begin
                  NumberOfThisDisk := 0;
                  DiskWithStartOfCentral := 0;
                  EntriesOnDisk := pHeaderObj^.FileCount - FilesSkipped;
                  TotalEntries := pHeaderObj^.FileCount - FilesSkipped;
                  CommentLen := pHeaderObj^.ArchiveCommentLen;

                  If TempFile.Write(EndZipHeader64, SizeOf(TZipTV_End64)) <> SizeOf(TZipTV_End64) Then
                  Begin
                     RaiseErrorStr(fArchiveFile, '', '0', E_FWRITE);
                     Exit;
                  End;

                  With pHeaderObj^ Do
                     If (ArchiveCommentLen > 0) Then
                        If TempFile.Write(ArchiveComment[0], ArchiveCommentLen) <> ArchiveCommentLen Then
                           RaiseErrorStr(fArchiveFile, '', '0', E_FWRITE);
               End;
            End Else
               With EndZipHeader Do
               Begin
                  NumberOfThisDisk := 0;
                  DiskWithStartOfCentral := 0;
                  EntriesOnDisk := pHeaderObj^.FileCount - FilesSkipped;
                  TotalEntries := pHeaderObj^.FileCount - FilesSkipped;
                  CommentLen := pHeaderObj^.ArchiveCommentLen;

                  If TempFile.Write(EndZipHeader, SizeOf(TEnd)) <> SizeOf(TEnd) Then
                  Begin
                     RaiseErrorStr(fArchiveFile, '', '0', E_FWRITE);
                     Exit;
                  End;

                  With pHeaderObj^ Do
                     If (ArchiveCommentLen > 0) Then
                        If TempFile.Write(ArchiveComment[0], ArchiveCommentLen) <> ArchiveCommentLen Then
                           RaiseErrorStr(fArchiveFile, '', '0', E_FWRITE);
               End;
         End;
      End;
   Finally
   	//Result := Result And (Not fCancel);
   End;
End;
//------------------------------------------------------------

Procedure TZipBase.doFinish(pHeaderObj: pCompHeaderObj);
Var
   i: Integer;
Begin

   fTotalPackedSize := 0;
   fTotalUnpackedSize := 0;

   With pHeaderObj^ Do
   Begin
      If (FilesDeleted > 0) And (FilesDeleted = FileCount) Then Exit;

      For i := 0 To FileCount - 1 Do
      Begin
         //If Cancel Then Break;
         pCBFI := FileLocationData(i);

         If pCBFI^.Status = hsSkip Then Continue;
         CentralZipHeader := TCentral(Header[i]^);

         fFileName := FileList.Strings[i];
         With CentralZipHeader Do
         Begin

            fTotalPackedSize :=
            	(Int64(pCBFI^.HighPackedSize) Shl 32) Or zc.PackedSize +
					fTotalPackedSize;

            fTotalUnpackedSize :=
               (Int64(pCBFI^.HighUnpackedSize) Shl 32) Or zc.UnpackedSize +
            	fTotalUnpackedSize;

            If (Switch <> swDelete) And (pCBFI^.Status = hsAdd) Then
            Begin
               fVersionMadeBy := word(VerNum);
               fMinVersion := word(MinVerNum);

               fEncrypted := (zc.BitFlag And 1) > 0;


               If fEncrypted And ((Int64(pCBFI^.HighPackedSize) Shl 32) Or zc.PackedSize >= RAND_HEAD_LEN) Then
                  fPackedSize := (Int64(pCBFI^.HighPackedSize) Shl 32) Or zc.PackedSize - RAND_HEAD_LEN
               Else
                  fPackedSize := (Int64(pCBFI^.HighPackedSize) Shl 32) Or zc.PackedSize;


               fUnpackedSize := (Int64(pCBFI^.HighUnpackedSize) Shl 32) Or zc.UnpackedSize;
               fCRC := zc.CRC32;
               fsCompressType := GetCompressMethodStr(zc.CompressType, zc.BitFlag);
               fwCompressType := zc.CompressType;
               fExternalAttr := ExternalAttr;
               fInternalAttr := InternalAttr;
               fRatio :=
               	CalcRatio(
                  	(Int64(pCBFI^.HighPackedSize) Shl 32) Or zc.PackedSize,
                     (Int64(pCBFI^.HighUnpackedSize) Shl 32) Or zc.UnpackedSize);
               Try
                  If zc.FileDate > 0 Then
                     GlobalDate := FileDateToDateTime(zc.FileDate);
               Except
               End;

               If (fExternalAttr And ZTV_FILE_ATTRIBUTE_DIRECTORY) > 0 Then
                  fFileName := AppendDirTail(fFileName);

               If Assigned(OnRead) Then
               Begin
                	(* The FileComment us used in the TZipTV component and  *)
                  (* commonly referenced in the OnRead event.  Initialize *)
                  (* the FileComment to prevent an error if used in this  *)
                  (* event.                                               *)
                  GetMem(FileComment, 1);
                  Try
                     FileComment[0] := #0;
                     OnRead(Self, pCBFI^.Offset, i)
                  Finally
                     FreeMem(FileComment, 1);
                  End;
               End
               Else
                  Break;
            End;
         End;
      End;
   End;
End;
//------------------------------------------------------------

Function TZipBase.GetHeadSize: word;
Begin
	Result := SizeOf(TCentral);
End;
//------------------------------------------------------------

Function TZipBase.AdjPtr(p: PChar): Pointer;
Begin
   Result := Pointer(ptr2int(p) + SizeOf(Integer));
End;
//------------------------------------------------------------

Procedure TZipBase.InitializeHeader(pCBFI: pCompFileInfo);
Begin
   ZeroMemory(@LocalZipHeader, SizeOf(TLocal));
   With LocalZipHeader Do
   Begin
      SignAtr := DefSig(htLocal, pCBFI^.EncryptedHeader);
      VerNum := VERSIONNUM;
      zc.FileDate := GetDateTime(HeadInfo.Date);
      zc.UnpackedSize := HeadInfo.uSize;
{$IFDEF DEFLATE64}
      zc.CompressType := Z_DEFLATE64;
{$ELSE}
      zc.CompressType := Z_DEFLATED;
{$ENDIF}
   End;
End;
//------------------------------------------------------------

Procedure TZipBase.FillHeaderData(InStreamSize, CompressStreamSize: Int64);
Begin
   If (InStreamSize = 0) And (Crc32Val = CRC_MASK) Then
      Crc32Val := 0;

   With LocalZipHeader Do
   Begin

      If (pCBFI^.HighUnpackedSize > 0) Or (pCBFI^.Offset > MAXDWORD) Then
      Begin
   		Dec(CompressStreamSize, SizeOf(TLocal64Hdr));
         Inc(zc.ExtraFieldLen, SizeOf(TLocal64Hdr));
      End Else
      	zc.ExtraFieldLen := ExtraFieldLen;

      zc.FileNameLen := Length(fFileName);
      zc.UnpackedSize := InStreamSize;
      zc.CRC32 := Crc32Val;

      If fEncrypted Then
         zc.BitFlag := PW_PROTECTED;

      // fCompressType = ZTV_STORE is defined in ztvBase, function AddFileProc
      If fGlobalCompressType = ZTV_STORED Then
         zc.CompressType := ZTV_STORED
      Else
         zc.BitFlag := SetDeflateBitFlag(zc.BitFlag);

      zc.PackedSize := CompressStreamSize;

      If EncryptHeaders Then
      Begin
         EncodeHeader(@LocalZipHeader, htLocal);
         EncodeFilename(@fFileName[1], Length(fFileName));
      End;

   End;
End;
//------------------------------------------------------------

Function TZipBase.GetLocalHeaderSize: Integer;
Begin
	Result := SizeOf(TLocal);
End;
//-------------------------------------------------------------

Function TZipBase.GetHeadPtr: Pointer;
Begin
	Result := @LocalZipHeader;
End;
//-------------------------------------------------------------

Procedure TZipBase.doOnEnd;
Begin
   With LocalZipHeader Do
      If Assigned(OnEnd) Then
         OnEnd(Self, fFileName, Crc32Val = zc.CRC32);
End;
//-------------------------------------------------------------

Procedure TZipBase.doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
   pHeaderObj: pCompHeaderObj);
Var
   FileName: String;
   pCentral: ^TCentral;
   eFieldLen: word;
   Encrypted: Boolean;
   PackedSize: Cardinal;
Begin

   With pHeaderObj^ Do
   Begin
      pCBFI := FileLocationData(Index);

      pCentral := Header[Index];
      pCentral^.RelativeOffsetOfLocalHeader := TempFile.size;
      pCentral^.zc.FileNameLen := Length(FileList.Strings[Index]);

      (* write header *)
      If (ArcFile.Read(LocalZipHeader, SizeOf(TLocal)) <> SizeOf(TLocal)) Then
         doWriteError();

      VerSig(LocalZipHeader.SignAtr, htLocal, Encrypted);
      If Encrypted Then
         DecodeHeader(@LocalZipHeader, htLocal);

      (* Set file pointer after filename *)
      ArcFile.Position := pCBFI^.Offset + SizeOf(TLocal) +
         LocalZipHeader.zc.FileNameLen;

      FileName := FileList.Strings[Index];
      PackedSize := LocalZipHeader.zc.PackedSize;
      LocalZipHeader.zc.FileNameLen := Length(FileName);
      eFieldLen := LocalZipHeader.zc.ExtraFieldLen; // save before encoding the header

      If Encrypted Then
      Begin
         EncodeHeader(@LocalZipHeader, htLocal);
         EncodeFilename(@FileName[1], Length(FileName));
      End;

      If (TempFile.Write(LocalZipHeader, SizeOf(TLocal)) <> SizeOf(TLocal)) Then
         doWriteError();

      If (TempFile.Write(FileName[1], Length(FileName)) <> Length(FileName)) Then
         doWriteError();

      TempFile.CancelCallBackProc := Nil;
      TempFile.ProgressCallBackProc := Nil;

      (* write ExtraField *)
      If (eFieldLen > 0) And (TempFile.CopyFrom(ArcFile, eFieldLen) <> eFieldLen) Then
         doWriteError();

      (* write compressed data *)
      If (TempFile.CopyFrom(ArcFile, PackedSize) <> longint(PackedSize)) Then
         doWriteError();

   End;
End;
//------------------------------------------------------------

Function TZipBase.GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo;
Begin
   With TCentral(p^) Do
   Begin
      Result.Date := zc.FileDate;
      Result.Attr := VerifyAttr(ExternalAttr);
      Result.pSize := (Int64(pCBFI^.HighUnpackedSize) Shl 32) Or zc.PackedSize;
      Result.uSize := (Int64(pCBFI^.HighUnpackedSize) Shl 32) Or zc.UnpackedSize;
   End;
End;
//------------------------------------------------------------

Procedure TZipBase.SetArcType(SAT: TArcType);
Begin
   fArcType := atZip;
   CompressArcType();
End;
//------------------------------------------------------------

Procedure TZipBase.SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo);
Begin
   With TCentral(pHeader^) Do
   Begin
      zc.FileDate := HI^.Date;
      zc.UnpackedSize := HI^.uSize;
      zc.PackedSize := HI^.pSize;
      ExternalAttr := HI^.Attr;
   End;
End;
//-------------------------------------------------------------

Procedure TZipBase.WriteZeroByteZipHeader(TempStream: TStream32);
Var
   EndZipHeader: TEnd;
Begin
   ZeroMemory(@EndZipHeader, SizeOf(TEnd));
   EndZipHeader.SignAtr := DefSig(htEnding, False);
   If TempStream.Write(EndZipHeader.SignAtr, SizeOf(TEnd)) <> SizeOf(TEnd) Then
      RaiseErrorStr(fArchiveFile, '', '0', E_FWRITE);
End;
//-------------------------------------------------------------

Constructor TDiskSpanObj.Create(ZO, pDrv, ps, pDM, pHO, pC: Pointer);
Begin
   ZipObj := TZip(ZO^);
   pInternalCancel := pC;
   pHeadObj := pHO;
   pDiskMan := pDM;
   InStream := pStream(ps)^;

   // --------------------------------------------
   // delete the TempFile (used to read from after
   // compression) only... do not rename.  If this
   // FileMode variable is not set to 0, the temp
   // file (already compressed to the temp dir) will
   // overwrite the OutStream already existant on
   // the diskette.
   // --------------------------------------------
   TTempFileStream(InStream).FileMode := 0;
End;
//-------------------------------------------------------------

Destructor TDiskSpanObj.Destroy;
Begin
   Inherited;
End;
//-------------------------------------------------------------

Function TDiskSpanObj.GetMinSize(Const size: Integer): Integer;
Begin
   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //Result := Min(pDiskMan^.FreeSpace, size);
   If pDiskMan^.FreeSpace > size Then
      Result := size
   Else
      Result := pDiskMan^.FreeSpace;
End;
//-------------------------------------------------------------

Function TDiskSpanObj.WriteToFloppy(strm: TStream32; Var WriteObj;
   size: Integer): Boolean;
Var
   BufSize, BytesWritten: Integer;
Begin
   Result := False;
   BufSize := GetMinSize(size);

   With pDiskMan^ Do
   Try
      While (size > 0) Do
      Begin
         BytesWritten := strm.Write(WriteObj, BufSize);
         Dec(size, BytesWritten);
         FreeSpace := FreeSpace - BytesWritten;

         If (size > 0) Then
         Begin
            If (Not RequestNext()) Then Exit;
            BufSize := GetMinSize(size);
         End;
      End;
   Finally
      Result := (size = 0);
   End;
End;
//------------------------------------------------------------

Function TDiskSpanObj.WriteDiskLabel: Boolean;
Var
   NewLabel: String;
Begin
   NewLabel := PKBACK + Format('%.3d', [fVolumeNumber]);
   Result := {pDiskMan^.} ChangeDiskLabel(NewLabel);
   If (Not Result) Then
      _RaiseErrorStr(ZipObj.fArchiveFile, '', NewLabel, E_DISKLABELERROR);
End;
//------------------------------------------------------------

Function TDiskSpanObj.ClearDiskProc: Boolean;
Begin
   Result := False;

   With pDiskMan^, ZipObj Do
      If Assigned(_OnClearDisk) Then
      Begin
         Repeat
            Result := True;

            _OnClearDisk(Self, DriveChar, fRequiredSpace, FreeSpace, TotalSpace,
               Result);

            Result := (Not Result);
            If Not Result Then Break;
         Until
         	GetDriveInfo(
            	fArchiveFile,
               RaiseError,
               RaiseErrorStr,
               fWriteProtectErr,
               fDiskInDrvErr) And (fRequiredSpace < FreeSpace);

      End Else Begin

         With ZipObj Do
         If pDiskMan^.GetDriveInfo(
            		fArchiveFile,
               	RaiseError,
               	RaiseErrorStr,
               	OnDiskWriteProtectErr,
                  OnDiskInDrvErr) And (FreeSpace > fRequiredSpace) Then
            Result := True
         Else
            _RaiseErrorStr(
            	fArchiveFile,
               'OnClearDisk',
               IntToStr(fVolumeNumber),
               E_REQUIREDEVENT);

      End;
End;
//------------------------------------------------------------

Function TDiskSpanObj.OpenOutputFile: Boolean;
Begin
   Try
      OutStream := TFileStream32.Create(ZipObj.fArchiveFile, fmCreate);
      If (OutStream.Handle < 0) Then
      Begin
      	Result := False;
      	Exit;
      End;

      WriteDiskLabel();

      With ZipObj Do
      Begin
         Result :=
         	pDiskMan^.GetDriveInfo(fArchiveFile, RaiseError, RaiseErrorStr,
            	fWriteProtectErr, fDiskInDrvErr);

         If fVolumeNumber = 1 Then
   			pDiskMan^.FreeSpace := pDiskMan^.FreeSpace - ZipObj.fLeaveSpaceOnFirstDisk;

      End;

      fSerialNumber := pDiskMan^.VolumeInfo.Serial;
   Except
      Result := False;
   End;

End;
//------------------------------------------------------------

Function TDiskSpanObj.InsertDiskProc: Boolean;
Var
   aDate: TDateTime;
Begin

   With pDiskMan^ Do
      If Assigned(_OnInsertDisk) Then
      Begin

         If (Not FileExists(ZipObj.fArchiveFile)) Then
            If (FreeSpace >= fRequiredSpace) Then
            Begin
               Result := OpenOutputFile();
               If Result Then
                  Exit;
            End;


         Try
            Repeat                      // until FreeSpace >= fRequiredSpace

               Repeat                   (* until disk-changed *)
                  Result := True;

               (* return value of Result = user canceled or event proc is blank *)
                  _OnInsertDisk(Self, IntToStr(fVolumeNumber), Result);
                  Result := (Not Result);
                  If (Not Result) Then Exit;

                  With ZipObj Do
							Result := GetDriveInfo(
            					fArchiveFile,
               				RaiseError,
               				RaiseErrorStr,
               				fWriteProtectErr,
                           fDiskInDrvErr);

                  If FileExists(ZipObj.fArchiveFile) Then
                  Begin
                     fOverwriteMode := omOverwrite;

                     If Assigned(_OnFileExists) Then
                     Begin
                        aDate := FileDateToDateTime(FileAge(ZipObj.fArchiveFile));
                        _OnFileExists(ZipObj, ZipObj.fArchiveFile, aDate, fOverwriteMode);
                        If fOverwriteMode = omSkip Then pInternalCancel^ := True;
                        Result := (Not pInternalCancel^);
                        If Not Result Then Exit;
                     End
                     Else
                        _RaiseErrorStr(ZipObj.fArchiveFile, 'OnFileExists',
                           IntToStr(fVolumeNumber), E_REQUIREDEVENT);

                     If (Result And (fOverwriteMode = omOverwrite)) Then
                        Result := EraseFile(ZipObj.fArchiveFile,
                           ZipObj.DeleteOptions);

                     If Result Then
                        With ZipObj Do
                           Result := GetDriveInfo(
                                 fArchiveFile,
                                 RaiseError,
                                 RaiseErrorStr,
                                 fWriteProtectErr,
                                 fDiskInDrvErr);
                  End;

               Until Result And
                  ((VolumeInfo.Serial = 0) Or
                  (VolumeInfo.Serial <> fSerialNumber));
                  // And ( pDiskMan^.ZipVolNum <> pEndZipHeader^.NumberOfThisDisk );


               If (FreeSpace < TotalSpace) Then
                  Result := ClearDiskProc();

            Until (FreeSpace >= fRequiredSpace);
         Finally
         	If Result Then
            	Result := OpenOutputFile();
         End;

      End
      Else
      Begin
         Result := False;
         _RaiseErrorStr(ZipObj.fArchiveFile, 'OnInsertDisk',
            IntToStr(fVolumeNumber), E_REQUIREDEVENT);
      End;

End;
//------------------------------------------------------------

Function TDiskSpanObj.RequestFirst: Boolean;
Begin
   fVolumeNumber := 1;
   fSerialNumber := 0;
   ZipObj.EndZipHeader.NumberOfThisDisk := 0;
   fRequiredSpace := ZipObj.fLeaveSpaceOnFirstDisk + REQUIRED_FREE_SPACE;
   pDiskMan^.FreeSpace := pDiskMan^.FreeSpace - ZipObj.fLeaveSpaceOnFirstDisk;
   Result := InsertDiskProc();
End;
//------------------------------------------------------------

Function TDiskSpanObj.RequestNext: Boolean;
Begin
   inc(fVolumeNumber);
   inc(ZipObj.EndZipHeader.NumberOfThisDisk);
   OutStream.Free();
   OutStream := Nil;
   fRequiredSpace := REQUIRED_FREE_SPACE;
   Result := InsertDiskProc();
End;
//------------------------------------------------------------

Function TDiskSpanObj.ChangeDiskLabel(NewLabel: String): Boolean;
Begin
   Result := ( pVolumeInfo^.DLabel = NewLabel ) Or
   	SetVolumeLabel( PChar( AppendDirTail( pDiskMan^.DriveChar ) ), PChar( NewLabel ) );
End;
//------------------------------------------------------------

Function TDiskSpanObj.CopyBlock(size: Integer): Boolean;
Var
   BufSize: Integer;
Begin

	Result := False;
	With pDiskMan^ Do
   Try
      BufSize := GetMinSize(size);
      While (size > 0) Do
      Begin
      	OutStream.CancelCallBackProc := Nil;
      	OutStream.ProgressCallBackProc := Nil;
         OutStream.CopyFrom(InStream, BufSize);

         Dec(size, BufSize);
         FreeSpace := FreeSpace - BufSize;

         If (size > 0) Then
         Begin
            If (Not RequestNext()) Then
            	Exit;

            BufSize := GetMinSize(size);
         End;
      End;
   Finally
      Result := (size = 0);
   End;
End;
//------------------------------------------------------------

Function TDiskSpanObj.Activate: Boolean;
Var
   Encrypted: Boolean;
   pLocalZipHeader: ^TLocal;
   pCentralZipHeader: ^TCentral;
   i, blocksize, Signature: Integer;
   LastPos: Int64;
Begin

   Result := False;
   ZipObj.DoProgress(0, 0);

   If RequestFirst() Then
   Try
      ZipObj.ProgressPosition := pHeadObj^.FileCount + 1;

      If pDiskMan^.FreeSpace < (InStream.size + REQUIRED_FREE_SPACE) Then
      Begin
         Signature := MULTIVOL_HEADER_SIGNATURE;
         OutStream.Write(Signature, SizeOf(Signature));
         pDiskMan^.FreeSpace := pDiskMan^.FreeSpace - SizeOf(Signature);
         Signature := 0;
      End;

      LastPos := 0;
      InStream.Position := LastPos;
      For i := 0 To pHeadObj^.FileCount - 1 Do
      Begin
         If pInternalCancel^ Then Exit;

         If pHeadObj^.FileLocationData(i).Status = hsSkip Then
            Continue;

         pCentralZipHeader := pHeadObj^.Header[i];
         If ZipObj.VerSig(pCentralZipHeader^.SignAtr, htCentral,
            Encrypted) = htNone Then
            Exit;

         pLocalZipHeader := @ZipObj.LocalZipHeader;
         With pLocalZipHeader^ Do
         Begin

            InStream.Position := LastPos;

            // read & verify local header
            ZeroMemory(pLocalZipHeader, SizeOf(TLocal));
            InStream.Read(pLocalZipHeader^, SizeOf(TLocal));
            If ZipObj.VerSig(SignAtr, htLocal, Encrypted) = htNone Then
               Exit;

            SetLength(ZipObj.fFileName, pLocalZipHeader^.zc.FileNameLen);
            InStream.Read(ZipObj.fFileName[1], pLocalZipHeader^.zc.FileNameLen);

            inc(ZipObj.EndZipHeader.EntriesOnDisk);
            inc(ZipObj.EndZipHeader.TotalEntries);

            pCentralZipHeader^.DiskNumberStart :=
               ZipObj.EndZipHeader.NumberOfThisDisk;

            pCentralZipHeader^.RelativeOffsetOfLocalHeader :=
               OutStream.size;

            InStream.Position := LastPos;
            If (Not CopyBlock(SizeOf(TLocal))) Then Exit;

            ZipObj.ProgressPosition := pHeadObj^.FileCount - (i + 1);
            ZipObj.doBranchProgress(i + 1, pHeadObj^.FileCount,
               pHeadObj^.FileCount);

            blocksize := zc.FileNameLen + zc.ExtraFieldLen + zc.PackedSize;
         End;

         If (Not CopyBlock(blocksize)) Then Exit;
         LastPos := InStream.Position;
         Application.ProcessMessages();
      End;

      If (Not ZipObj.WriteCentralDirectory(OutStream, pHeadObj)) Then Exit;

      If OutStream.Write(ZipObj.EndZipHeader, SizeOf(TEnd)) <> SizeOf(TEnd) Then
         Exit;

      Result := True;
   Finally
      If OutStream <> Nil Then
         OutStream.Free();
   End;
End;
//------------------------------------------------------------


End.
