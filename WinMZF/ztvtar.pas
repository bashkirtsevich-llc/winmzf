(**********************************************************************

  Copyright 1998-2003,  Microchip Data Systems / Carl Bunton

  Under license agreement, this source module may be used only on a
  single computer.

  No portion of this module may be reproduced, copied, revised, edited,
  distributed or transmitted via electronic means except in compiled
  application format.

  Web-site:  http://www.ziptv.com
  Email:     custsupt@ziptv.com


  NOTES:
  	1. A .tar archive has a limitation on the length of a filename of
   100 characters.  Keep this limitation in mind when planning your
   development which includes paths in a filename.

**********************************************************************)
Unit ztvtar;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   Dialogs,
   ztvBase,
   ztvGbls,
   ztvGZip,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TTar = Class(TCompBase)
   Private
      fUID: Byte;
      fGID: Byte;
      fGname: String;  // v4.8.2 changed ShortString
      fUname: String;  // v4.8.2 changed ShortString
      fHeaderSize: Integer;
      fMinVersion: word;
   Protected
      Function GetHeadPtr: Pointer; Override;
      Function GetHeadSize: word; Override;
      Function GetLocalHeaderSize: Integer; Override;
      Function ReadHeader(s: TStream32; Var FileName: String; HeadType:
         Byte): Boolean; Override;
      //Function Write64BitFieldHdr(s: TStream32; CFI: pCompFileInfo;
      //	HType: THeaderType): Integer; Override;
      //Procedure doOnEnd; Override;
      Procedure doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
         pHeaderObj: pCompHeaderObj); Override;
      Procedure PackFile(outFile: TStream32; Index: Integer; pHeaderObj:
         pCompHeaderObj; Var Result: Boolean); Override;
      Procedure RefreshHeader(Index, ExtAttr: Integer; pHeaderObj:
         pCompHeaderObj); Override;
      Procedure SetArcType(SAT: TArcType); Override;
      Procedure SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo); Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Function doCleanUp(outFile: TStream32; pHeaderObj: pCompHeaderObj):
         Boolean; Override;
      Function GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo; Override;
      Function GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
         Pointer): Int64; Override;
      Procedure ArcToList(s: TStream32; pHeaderObj: pCompHeaderObj); Override;
      Procedure doFinish(pHeaderObj: pCompHeaderObj); Override;
      Procedure doTarGzip(NewArchiveFile: String); Override;
   Published
      Property ArcType;
      Property DefaultExt;
      Property CompressMethod;
      Property CompressionMethod;
      Property Switch;
      //PROPERTY Password;
      Property StoredDirNames;
      Property StoreEmptySubDirs;
      Property FileSpec;
      Property RecurseDirs;
      Property TempDir;
      Property UnixUID: Byte Read fUID Write fUID Default 2;
      Property UnixGID: Byte Read fGID Write fGID Default 2;
      Property UnixUName: String Read fUname Write fUname;	// v4.8.2 changed ShortString
      Property UnixGName: String Read fGname Write fGname;	// v4.8.2 changed ShortString
      Property OnNonWriteableArchive;
      Property OnProgress;
      //PROPERTY OnGetPassword;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnRead;
      Property OnRenameFile;
      Property OnRenameDupeFile;
      Property OnReplaceFile;
      Property OnTmpFileMoveBegin;
      Property OnTmpFileMoveEnd;
		Property OnTmpFileProgress;
   End;

Implementation

Uses
   ztvRegister,
   ztvHeaders,
   Err_Msgs;

//-------------------------------------------------------------

Constructor TTar.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fHeaderSize := SizeOf(TTarHeader);
   fMasterExt := '.TAR';
   DefaultExt := fMasterExt;
   CompressMethodState := [cmTarred, cmTarGzip];
   CompressMethod := cmTarred;
   fArcType := atTar;
   fUID := 2;
   fGID := 2;
   fUname := 'root';
   fGname := 'root';
End;
//-------------------------------------------------------------

Destructor TTar.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TTar.RefreshHeader(Index, ExtAttr: Integer; pHeaderObj: pCompHeaderObj);
Begin
   CopyMem(HPtr, pHeaderObj^.Header[Index], fHeaderSize {HSize});
End;
//-------------------------------------------------------------

Procedure TTar.PackFile(outFile: TStream32; Index: Integer; pHeaderObj:
   pCompHeaderObj; Var Result: Boolean);
Var
   Len: word;
   MemBuf: PChar;
   aWrite,
      HeadPos,
      DosDate,
      UnixDate,
      CheckSum: Integer;
   zsp: ztv_stream_plus;
   DataStream: TFileStream32;
   StoreStream: TStoreStream;
Begin
   Result := False;
	//DLB code altered to allow directory storage option for UNIX compatibility

   // v4.1.1 changed
   //If ((Not StoreEmptySubDirs) And (HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY > 0)) Then
   //   Exit;

   With pHeaderObj^, TarHeader Do
   Try
      fFileName := CharToOemFilter(FileList.Strings[Index], fTransOemChar);
      fRootDir := Copy(fFileName, 0, Integer(RootDirLen[Index]));

      CheckSum := 0;
      If doOnBegin(fFileName, Count, pHeaderObj) Then
      Try

         If HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY > 0 Then // v4.1.1 changed
            DataStream := Nil
         Else Begin
            DataStream :=
               TFileStream32.Create(
               pHeaderObj^.FileList.Strings[Index],
               fmOpenRead Or fmShareDenyNone);

            If (DataStream.Handle < 0) Then
            Begin
               RaiseErrorStr(pHeaderObj^.FileList.Strings[Index], '', '0', E_FOPEN);
               Exit;
            End;

         End;

      	HPtr := GetHeadPtr();

         GetMem(MemBuf, 1024);
         Try
            ZeroMemory(HPtr, fHeaderSize);

            (* The DoOnbegin function simply activates the OnBegin and	*)
            (* OnRenameFile events.  The return value of this	*)
            (* function is the boolean parameter of the OnBegin event.  *)
            pCBFI^.Offset := outFile.size;

            CheckSum := 0;

            // v4.1.1 changed
            If (HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
               DosDate := HeadInfo.Date
            Else
               DosDate := GetDateTime(FileGetDate(DataStream.Handle));

            UnixDate := DosDateToUnix(FileDateToDateTime(DosDate));
//DLB header values for UNIX, normally these might vary [Public fields would be good, an array better]
            //Uname := fUName;
            //Gname := fGName;

            If Length(fUname) > SizeOf(UName) Then
               SetLength(fUname, SizeOf(UName));

            CopyMem(@fUname[1], @UName[0], Length(fUname));

            If Length(fGname) > SizeOf(GName) Then
               SetLength(fGname, SizeOf(GName));

            CopyMem(@fGname[1], @GName[0], Length(fGname));
            MAGIC := 'ustar  ' + #0;

//DLB code to handle mode variation to allow folders to store [header only objects]
    // v4.1.1 changed
            If HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY > 0 Then
            Begin
               mode := '0040555' + #0;  // mode actually varies file by file... similar to dos attribs
               LinkFlag := LF_DIR;
            End Else Begin
               mode := '0100444' + #0;  // mode actually varies file by file... similar to dos attribs
               LinkFlag := LF_NORMAL;
            End;

            fFileName := DOSToUnixFilename(fFileName);

//DLB special handling for directories when stored in TAR files
    			// v4.1.1 changed
            If (HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
               fFileName := AppendDirTail(fFileName); // all directories stored with trailing slash

            CopyMem(@fFileName[1], @TarHeader.TarFilename[0], Length(fFileName));
            HeadPos := outFile.Position;
            Crc32Val := CRC_MASK;

            If DataStream <> Nil Then
               DataStream.Position := 0;

//DLB another code change for folder storage, no data...
    			// v4.1.1 changed
            If (HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
               aWrite := 0
            Else
               aWrite := DataStream.size;

            IntToOctStr(aWrite, SizeOf(size), True, MemBuf);
            CopyMem(@MemBuf[0], @size[0], SizeOf(size));
            size[SizeOf(size) - 1] := #0;

            IntToOctStr(UnixDate, SizeOf(MTime), True, MemBuf);
            CopyMem(@MemBuf[0], @MTime[0], SizeOf(MTime));
            MTime[SizeOf(MTime) - 1] := #0;

//DLB replaced with UNIX values above
            //ZeroMemory(@Mode[0], SizeOf(Mode)); 100666 octal ?
            //IntToOctStr( 33206, SizeOf( Mode ), True, MemBuf );
            //CopyMem( @MemBuf[0], @Mode[0], SizeOf( Mode ) );

            IntToOctStr(fUID, SizeOf(UID), True, MemBuf);
            CopyMem(@MemBuf[0], @UID[0], SizeOf(UID));
            UID[SizeOf(UID) - 1] := #0;

            IntToOctStr(fGID, SizeOf(GID), True, MemBuf);
            CopyMem(@MemBuf[0], @GID[0], SizeOf(GID));
            GID[SizeOf(GID) - 1] := #0;

            CheckSum := CalcCheckSum(TarHeader);
//DLB note: I Think ChkSum should be Array of Char [0..6] not [0..7],
//        and there is a filler Byte before flag
//        it is reserved space for expansion of flag types,
//        Checksum value differs from UNIX TAR???
            IntToOctStr(CheckSum, SizeOf(ChkSum), True, MemBuf);

            //CopyMem( @MemBuf[1], @MemBuf[0], SizeOf( ChkSum ) );
//DLB note, if header structure is fixed, the kluge for zapping space can probably go too
            CopyMem(@MemBuf[0], @ChkSum[0], SizeOf(ChkSum));
            //CopyMem( #0' ', ChkSum[6], 2 );
            //ChkSum[6] := #0;	(* Zap the space *)

            outFile.Position := HeadPos;

            //If CompressWriteBlock(oFile, Nil, TarHeader, False,
            //   0, fHeaderSize, dtHeader) <> Cardinal(fHeaderSize) Then
            //   doWriteError;
            If outFile.Write(TarHeader, fHeaderSize) <> fHeaderSize Then
            	doWriteError();


            If (HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY = 0) And (DataStream <> Nil) Then
            Begin
               DataStream.Position := 0;
               // v4.1.8 changed
               //outFile.CopyFrom(DataStream, 0, @fCancel, GlobalProgress);
					//StoreFile( Self, THandleStream32(DataStream).Handle, oFile, 32, DataStream.Size, False );

               With zsp Do
               Begin
                  Protect := False;
                  CRC := CRC_MASK;
                  pCancel := @fCancel;
                  pArchivePos := @ProgressPosition;
               End;

               fGlobalCompressType := ZTV_STORED;
               fUnpackedSize := DataStream.Size;

               StoreStream :=
               	TStoreStream.Create(outFile, 32, zsp, @fCancel, GlobalProgress);
               Try
                  Result :=
                     StoreStream.CopyStream(DataStream) = DataStream.Size;
               Finally
                  Crc32Val := StoreStream.FZRec.cb.Crc;
                  StoreStream.Free();
               End;


            End;

//DLB Folders store no DATA other than header fields in TAR files
            If LinkFlag = LF_NORMAL Then
            Begin
               If (outFile.Position Mod 512) <> 0 Then
               Begin
                  Len := (((outFile.Position + 511) Div 512) *
                     512) - outFile.Position;

                  ZeroMemory(@MemBuf[0], Len);
                  //If CompressWriteBlock(oFile, Nil, MemBuf[0], False,
                  //   0, Len, dtHeader) <> Cardinal(Len) Then doWriteError;
            		If outFile.Write(MemBuf[0], Len) <> Len Then
            			doWriteError();
               End;
            End;

            RefreshHeader(Index, HeadInfo.Attr, pHeaderObj);
            Result := True;
         Finally
            FreeMem(MemBuf, 1024);
            If DataStream <> Nil Then
               DataStream.Free();
         End;
      Finally
         If Assigned(OnEnd) Then
            OnEnd(Self, fFileName, CheckSum > 0);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Function TTar.GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
   Pointer): Int64;
Var
   Filler, BytesToSave: u_long;
Begin
   BytesToSave := u_long(GetLocalHeaderSize()) + HeadInfo.pSize;

   If (pCBFI^.Offset + BytesToSave) Mod 512 > 0 Then
      Filler := ((((pCBFI^.Offset + BytesToSave) +
         511) Div 512) * 512) - (pCBFI^.Offset +
         BytesToSave)
   Else
      Filler := 0;

   Result := u_long(GetLocalHeaderSize()) + HeadInfo.pSize + Filler;
End;
//-------------------------------------------------------------

Function TTar.ReadHeader(s: TStream32; Var FileName: String;
   HeadType: Byte): Boolean;
Var
   CheckSum: Integer;
   OrigCheckSum: Integer;
   BytesRead: longint;
Begin
   s.Position := LastPos;
   ZeroMemory(@TarHeader, fHeaderSize);
   BytesRead := s.Read(TarHeader, fHeaderSize);

   If ValidateTarHeader(@TarHeader) And (BytesRead = longint(fHeaderSize)) Then
   Begin
      OrigCheckSum := OctStrToInt(TarHeader.ChkSum);
      CheckSum := CalcCheckSum(TarHeader);
      Result := CheckSum = OrigCheckSum;
      If Result Then
      Begin
         FileName := UnixToDosFilename(String(TarHeader.TarFilename));
         LastPos := s.Position + OctStrToInt(TarHeader.size);
         If (LastPos Mod 512) <> 0 Then
            LastPos := ((LastPos + 511) Div 512) * 512;
      End;
   End
   Else
      Result := False;
End;
//-------------------------------------------------------------

Procedure TTar.ArcToList(s: TStream32; pHeaderObj: pCompHeaderObj);
Var
	HeadType: Byte;
   CBFInew: TCompFileInfo;
   FileName: String;
Begin
   FilesDeleted := 0;

   With TarHeader Do
      If GetFirst(FileName, s, HeadType{0}, pHeaderObj) Then
         Repeat

            FileName := OemToCharFilter(FileName, fTransOemChar);

            If (Not CopySuccess) Then
            Begin

               CBFInew.Status := hsCopy; // default

               If (Switch = swDelete) Then
               Begin

                  If CheckWildCard1(FileName, FileSpec, ExcludeSpec) Then
                  Begin
                     inc(FilesDeleted);
                     CBFInew.Status := hsSkip;

                     pHeaderObj^.AddItem(CBFInew, FileName, @TarHeader,
                        Length(fRootDir), fHeaderSize);

                     Continue;
                  End;
               End;

               CBFInew.Offset := PrevPos;
               CBFInew.FileCommentLen := 0;
               CBFInew.ExtraFieldLen := 0;
               CBFInew.EncryptedHeader := False;

               CBFInew.FileComment := Nil;
               CBFInew.ExtraField := Nil;

               pHeaderObj^.AddItem(CBFInew, FileName, @TarHeader,
                  Length(fRootDir), fHeaderSize);

               fTotalUnpackedSize :=
                  fTotalUnpackedSize +
                  fHeaderSize +
                  OctStrToInt(size);

            End
            Else
               pHeaderObj^.FileList.Add(FileName);

         Until (Not GetNext(FileName, s, 0));
End;
//-------------------------------------------------------------

Function TTar.doCleanUp(outFile: TStream32; pHeaderObj: pCompHeaderObj): Boolean;
Var
   j: Integer;
	//DLB added ClrBuff Pointer for memory allocation for padding
   ClrBuff: Pointer;
Begin
   Result := True;

	//DLB Special handling for TAR files, for compatibility they need padding to a 10K boundary
	//DLB it appears there is some problem with file positioning, we get junk before padding
   j := outFile.Position;
   If ((j Mod 10240) > 0) Then
   Begin
      j := 10240 - (j Mod 10240);
      //HeapAllocFlags := GPTR;	// v4.1.5 rem'd.  D6 reports "Symbol 'HeapAllocFlags' is specific to a platform
      GetMem(ClrBuff, j + 50);
      Try
         ZeroMemory(ClrBuff, j + 30);
         If outFile.Write(ClrBuff^, j) <> j Then
         	doWriteError();
      Finally
       	//HeapAllocFlags := GMEM_MOVEABLE;  // v4.1.5 rem'd.  D6 reports "Symbol 'HeapAllocFlags' is specific to a platform
         FreeMem(ClrBuff, j + 50);
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TTar.doFinish(pHeaderObj: pCompHeaderObj);
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

         pCBFI := FileLocationData(i);

         If pCBFI^.Status = hsSkip Then Continue;
         TarHeader := TTarHeader(Header[i]^);

         With TarHeader Do
         Begin

            fTotalPackedSize := fTotalPackedSize + OctStrToInt(size);
            fTotalUnpackedSize := fTotalPackedSize;

            If (Switch <> swDelete) And (pCBFI^.Status = hsAdd) Then
            Begin
               fFileName := OemToCharFilter(FileList.Strings[i], fTransOemChar);
               GlobalDate := UnixDateToDos(OctStrToInt(MTime));
               //fDate := OctStrToInt( MTime );
               fVersionMadeBy := 0;
               fMinVersion := 0;
               fPackedSize := OctStrToInt(size);
               fUnpackedSize := fPackedSize;
               fCRC := OctStrToInt(ChkSum);
               fEncrypted := False;
               fsCompressType := 'Tarred';
               fwCompressType := 0;
               fExternalAttr := 32;
               fRatio := 0;             //CalcRatio( PackedSize, UnpackedSize );

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
                  break;

            End;
         End;
      End;
   End;

   //If fArcType = atTar Then
   //If CompressMethod = cmTarGzip Then
   //Begin
   //   doTarGZip(fArchiveFile + '.gz');
   //   fArchiveFile := fArchiveFile + '.gz';
   //End;
End;
//-------------------------------------------------------------

Procedure TTar.doTarGzip(NewArchiveFile: String);
Var
   GZip: TGZip;
Begin
   If CompressMethod = cmTarGzip Then
   Try
      GZip := TGZip.Create(Nil);
      Try
         GZip.pCancel := pCancel;
         //GZip.ArchiveFile := fArchiveFile + GZip.DefaultExt;
         //GZip.ArchiveFile := _ChangeFileExt(ArchiveFile, GZip.DefaultExt);
         GZip.ArchiveFile := NewArchiveFile; //ArchiveFile + GZip.DefaultExt;
         //TArc2Arc(GZip).NewArchive := NewArchiveFile;
         GZip.isTarGZip := True;
         GZip.FileSpec.Clear();
         GZip.FileSpec.Add(ArchiveFile);
         GZip.Switch := swAdd; //swMove;
         GZip.Extension := exNone;	//exAppendGZ;
         GZip.RecurseDirs := False;
         GZip.StoredDirNames := sdNone; //StoredDirNames;
         GZip.OnBegin := OnBegin;
         GZip.OnEnd := OnEnd;
         GZip.OnRead := OnRead;
         GZip.OnProgress := OnProgress;
         GZip.OnError := OnError;
         GZip.DefaultDir := '';

         //GZip.ExcludedSpec.Clear();					// v4.7 added
         GZip.TranslateOemChar := TranslateOemChar;	// v4.7 added
         GZip.RecurseDirs := False;             // v4.7 added
         GZip.DateAttribute := DateAttribute;   // v4.7 added
         GZip.OnActivate := OnActivate;         // v4.7 added
         GZip.OnDeactivate := OnDeactivate;     // v4.7 added
         GZip.OnFileExists := OnFileExists;		// v4.7 added
         GZip.Compress();	// activate compression

         (* Currently, GZip doesn't delete files via swMove *)
         //If ztvFileExists(GZip.ArchiveFile) Then
         //Begin
         //   //DeleteFile(ArchiveFile);
         //   fArchiveFile := GZip.fArchiveFile;
         //End;

      Finally
         GZip.Destroy();
      End;
   Except
   End;
End;
//-------------------------------------------------------------

//Function TTar.Write64BitFieldHdr(s: TStream32; CFI: pCompFileInfo;
//	HType: THeaderType): Integer;
//Begin
//	Result := 0;  // virtual method... do not delete.
//End;
//-------------------------------------------------------------

Function TTar.GetHeadPtr: Pointer;
Begin
   Result := @TarHeader;
End;
//-------------------------------------------------------------

Function TTar.GetHeadSize: word;
Begin
   Result := fHeaderSize;
End;
//-------------------------------------------------------------

Function TTar.GetLocalHeaderSize: Integer;
Begin
   Result := fHeaderSize;
End;
//-------------------------------------------------------------

{Procedure TTar.doOnEnd;
Begin
   With TarHeader Do
      If Assigned(OnEnd) Then
         OnEnd(Self, fFileName, True);
End;}
//-------------------------------------------------------------

Procedure TTar.doPCopy(ArcFile, TempFile: TStream32; Index: Integer; pHeaderObj:
   pCompHeaderObj);
Var
   MemBuf: PChar;
   //pTarHeader: ^TTarHeader;
   //tFileHandle: THandle;
   i, CheckSum: Integer;
   Bytes, Filler: u_long;
Begin

   (* Read existing header *)
   ArcFile.Read(TarHeader, fHeaderSize);

   Bytes := u_long(fHeaderSize) + HeadInfo.pSize;
   With pCBFI^ Do
      If (Offset + Bytes) Mod 512 > 0 Then
         Filler := ((((Offset + Bytes) + 511) Div 512) * 512) - (Offset + Bytes)
      Else
         Filler := 0;

   (* Set relational header fields *)
   GetMem(MemBuf, 1024);
   With TarHeader Do
   Try
      (* Set new filename *)
      FileName := DOSToUnixFilename(pHeaderObj^.FileList.Strings[Index]);

      For i := 1 To Length(TarHeader.TarFilename) Do
         TarHeader.TarFilename[i] := #0;

      CopyMem(@FileName[1], @TarHeader.TarFilename[0], Length(fFileName) + 1);

      (* Calculate new checksum for storage in header *)
      CheckSum := CalcCheckSum(TarHeader);
      IntToOctStr(CheckSum, SizeOf(ChkSum), True, MemBuf);
      CopyMem(@MemBuf[0], @ChkSum[0], SizeOf(ChkSum));
      Move(#0' ', ChkSum[6], 2);        //ChkSum[6] := #0;	(* Zap the space *)
   Finally
      FreeMem(MemBuf);
   End;

   (* Write revised header *)
   // v4.1.8 changed
   If TempFile.Write(TarHeader, fHeaderSize) <> fHeaderSize Then
      doWriteError();
   //tFileHandle := THandleStream32( TempFile ).Handle;
   //pTarHeader := @TarHeader;
   //If CompressWriteBlock(
   //	tFileHandle,
   //   Nil, pTarHeader, False,
   //   0, fHeaderSize, dtHeader ) <> DWord(fHeaderSize) Then
   //   doWriteError();

   (* Write compressed data *)
   // v4.1.8 changed
   TempFile.CancelCallBackProc := @fCancel;
   TempFile.ProgressCallBackProc := GlobalProgress;
   TempFile.CopyFrom(ArcFile, HeadInfo.pSize + Filler);
   //Bytes := HeadInfo.pSize + Filler;
   //If Bytes <> StoreFile( Self,
   //               THandleStream32( ArcFile ).Handle,
   //               tFileHandle,
   //               0, Bytes, False ) Then
   //   doWriteError();

End;
//-------------------------------------------------------------

Procedure TTar.SetArcType(SAT: TArcType);
Begin
   fArcType := atTar;
   CompressArcType();
End;
//-------------------------------------------------------------

Function TTar.GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo;
Begin
   With Result, TTarHeader(p^) Do
   Begin
      Date := DateTimeToFileDate(UnixDateToDos(OctStrToInt(MTime)));
      If LinkFlag = LF_DIR Then
         Attr := FILE_ATTRIBUTE_DIRECTORY
      Else
         Attr := FILE_ATTRIBUTE_ARCHIVE;

      pSize := OctStrToInt(size);
      uSize := Result.pSize;
   End;
End;
//------------------------------------------------------------

Procedure TTar.SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo);
Var
   MemBuf: PChar;
   UnixDate: Integer;
Begin
   GetMem(MemBuf, 1024);
   Try
      With TTarHeader(pHeader^) Do
      Begin
         //UnixDate := DosDateToUnix( FileDateToDateTime( HI^.Date ) );
         UnixDate := DosDateToUnix(ConvertDate(HI^.Date));
         IntToOctStr(UnixDate, SizeOf(MTime), True, MemBuf);
         CopyMem(@MemBuf[0], @MTime[0], SizeOf(MTime));
         MTime[SizeOf(MTime) - 1] := #0;

         IntToOctStr(HI^.uSize, SizeOf(size), True, MemBuf);
         CopyMem(@MemBuf[0], @size[0], SizeOf(size));
         size[SizeOf(size) - 1] := #0;

         If (HI^.Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
         Begin
            mode := '0040555' + #0;     // mode actually varies file by file... similar to dos attribs
            LinkFlag := LF_DIR;
         End
         Else
         Begin
            mode := '0100444' + #0;     // mode actually varies file by file... similar to dos attribs
            LinkFlag := LF_NORMAL;
         End;
      End;
   Finally
      FreeMem(MemBuf, 1024);
   End;
End;
//-------------------------------------------------------------


End.
