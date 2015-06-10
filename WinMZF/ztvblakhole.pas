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
Unit ztvBlakHole;

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
   ztvHeaders,
   ztvGbls,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TBlakHole = Class(TCompBase)
   Private
      Packable: Boolean;
      fHeaderSize: Integer;
      fMinVersion: Word;
   Protected
      Function GetHeadPtr: Pointer; Override;
      Function GetHeadSize: Word; Override;
      Function GetLocalHeaderSize: Integer; Override;
      Function ReadHeader(s: TStream32; Var FileName: String;
         HeadType: Byte): Boolean; Override;
      //Function Write64BitFieldHdr(s: TStream32; CFI: pCompFileInfo): Integer; Override;
      Procedure doOnEnd; Override;
      Procedure FillHeaderData(InStreamSize, CompressStreamSize: Int64); Override;
      Procedure RefreshHeader(Index, ExtAttr: Integer; pHeaderObj:
         pCompHeaderObj); Override;
      Procedure doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
         pHeaderObj: pCompHeaderObj); Override;
      Procedure InitializeHeader(pCBFI: pCompFileInfo); Override;
      Procedure PackFile(Outfile: TStream32; Index: Integer; pHeaderObj:
         pCompHeaderObj; Var Result: Boolean); Override;
      Procedure SetArcType(SAT: TArcType); Override;
      Procedure SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew:
      	TCompFileInfo); Override;
      Procedure WEI(OutStream: TStream32); Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Function doCleanUp(Outfile: TStream32; pHeaderObj: pCompHeaderObj):
         Boolean; Override;
      Function GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo; Override;
      Function GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
         Pointer): Int64; Override;
      Procedure ArcToList(s: TStream32; pHeaderObj: pCompHeaderObj); Override;
      Procedure doFinish(pHeaderObj: pCompHeaderObj); Override;
   Published
      Property ArcType;
      Property AttributesEx;
      Property CompressMethod;
      Property CompressionMethod;
      Property DefaultExt;
      Property DeflateType;
      Property FileSpec;
      Property IncludeHiddenDirs;
      Property Password;
      Property RecurseDirs;
      //Property StoreAlreadyCompressedFiles; // v4.6.8 removed.  Added StoreFilesOfType property
      Property StoredDirNames;
      Property StoreEmptySubDirs;
      Property StoreFilesOfType;
      Property Switch;
      Property TempDir;
      Property VerifyBeforeDelete;
      Property OnNonWriteableArchive;
      Property OnProgress;
      Property OnGetPassword;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnRead;
      //Property OnRenameFile;	// rem'd until revised for better understanding
      Property OnRenameDupeFile;
      Property OnReplaceFile;
      Property OnTmpFileMoveBegin;
      Property OnTmpFileMoveEnd;
		Property OnTmpFileProgress;
   End;

Implementation

Uses
   ztvLzh5,
   ztvDeflate,
   ztvCrypt,
   Err_Msgs;

//-------------------------------------------------------------

Constructor TBlakHole.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fArcType := atBh;
   fMasterExt := '.BH';
   fDefaultExt := fMasterExt;
   fHeaderSize := SizeOf(TBh);
   CompressMethodState := [{cmStore,} cmFuse, cmDeflate];
   CompressMethod := cmDeflate;
End;
//-------------------------------------------------------------

Destructor TBlakHole.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TBlakHole.GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo;
Begin
   With Result, TBh(p^) Do
   Begin
      Date := FileDate;
      Attr := ExternalAttr;
      pSize := PackedSize;
      uSize := UnpackedSize;
   End;
End;
//-------------------------------------------------------------

//Function TBlakHole.Write64BitFieldHdr(s: TStream32; CFI: pCompFileInfo): Integer;
//Begin
//	// virtual method... do not delete.
//End;
//-------------------------------------------------------------

Function TBlakHole.GetHeadPtr: Pointer;
Begin
   Result := @BhHeader;
End;
//------------------------------------------------------------

Function TBlakHole.GetHeadSize: Word;
Begin
   Result := fHeaderSize;
End;
//------------------------------------------------------------

Function TBlakHole.GetLocalHeaderSize: Integer;
Begin
   Result := fHeaderSize;
End;
//-------------------------------------------------------------

Procedure TBlakHole.doOnEnd;
Begin
   With BhHeader Do
      If Assigned(OnEnd) Then
         OnEnd(Self, fFileName, Crc32Val = CRC32);
End;
//------------------------------------------------------------

// Returns boolean value: true = use original uncompressed stream.  false =
// use compressed stream.  Occasionally, with very small files, the compressed
// size is larger than the uncompressed data... in this case, use the smallest
// uncompressed stream.

Procedure TBlakHole.FillHeaderData(InStreamSize, CompressStreamSize: Int64);
Begin

   If Crc32Val = CRC_MASK Then
      Crc32Val := 0;

   With BhHeader Do
   Begin
      FileNameLen := Length(fFileName);
      UnpackedSize := InStreamSize;
      CommentLen := 0;
      CRC32 := Crc32Val;

      If fEncrypted Then
         BitFlag := PW_PROTECTED;

      HdrSize := HeadSize + FileNameLen + CommentLen;
      PackedSize := CompressStreamSize;

      Case fGlobalCompressType Of
         ZTV_STORED:
            Begin
               CompressType := ZTV_STORED;

               {If CompressStreamSize = 0 Then
               Begin
               	BitFlag := 0;
                  PackedSize := 0;
               End Else Begin
      				PackedSize := CompressStreamSize;
                  If fEncrypted Then
                  Begin
                  	BitFlag := Byte(SetDeflateBitFlag(BitFlag));
                  	PackedSize := CompressStreamSize + RAND_HEAD_LEN;
                  End Else
                  	PackedSize := CompressStreamSize;
               End;}
            End;
         Z_DEFLATED:
            Begin
            	PackedSize := CompressStreamSize;
               BitFlag :=
               	Byte(SetDeflateBitFlag(BitFlag));
            End;
         ZTV_FUSE6:
            Begin
               CompressType := ZTV_FUSE6;
               PackedSize := CompressStreamSize;
               If fEncrypted Then
               Begin
                  BitFlag := Byte(SetDeflateBitFlag(BitFlag));
      			   Inc(PackedSize, RAND_HEAD_LEN);
               End;
            End;
      End;
   End;
End;
//------------------------------------------------------------

Procedure TBlakHole.InitializeHeader(pCBFI: pCompFileInfo);
Begin
   ZeroMemory(@BhHeader, fHeaderSize);
   With BhHeader Do
   Begin
      SignAtr := BLAKHOLE_SIGNATURE;
      HeadSize := GetHeadSize(); 
      VerNum := FVersionMin;
      MinVerNum := FVersionMax;
      FileDate := GetDateTime(HeadInfo.Date); //DateTimeToFileDate( Now );
      ExternalAttr := HeadInfo.Attr;
      UnpackedSize := HeadInfo.uSize;
      CommentLen := 0;
      CompressType := Z_DEFLATED;
   End;
End;
//-------------------------------------------------------------

Procedure TBlakHole.PackFile(Outfile: TStream32; Index: Integer; pHeaderObj:
   pCompHeaderObj; Var Result: Boolean);
Var
   SaveCrc32Val: u_long;
   zsp: ztv_stream_plus;
   DataStream: TStream32;
	outStreamPos, DataPos: Cardinal;
   CryptHDR: String[RAND_HEAD_LEN];
Begin
   Result := False;
   Try
      InitializeHeader(pCompFileInfo(pHeaderObj^.DataLocation[Index]));
      HPtr := GetHeadPtr();
      HSize := fHeaderSize;

      Case CompressMethod Of
         cmFuse:
            Begin
               fFileName := pHeaderObj^.FileList.Strings[Index]; //non-formating assignment
               If doOnBegin(fFileName, Count, pHeaderObj) Then
               Begin
                  pCBFI^.Offset := Outfile.size;

                  DataStream :=
                  	TFileStream32.Create(pHeaderObj^.FileList.Strings[Index],
                     	fmOpenRead);

                  If (TFileStream32(DataStream).Handle < 0) Then
                  Begin
                     RaiseErrorStr(pHeaderObj^.FileList.Strings[Index], '',
                        '0', E_FOPEN);

                     Exit;
                  End;

                  Try
                  	fEncrypted := Length(Password) > 0;
                     Bytes_To_Go := DataStream.size;
                     Crc32Val := CRC_MASK;

         				outStreamPos := Outfile.Position;
                     Outfile.WriteBuffer(HPtr^, HSize);
                     Outfile.WriteBuffer(fFileName[1], Length(fFileName));

                     With zsp Do
                     Begin
                        Protect := fEncrypted;
                        CRC := Crc32Val;
                        pCancel := @fCancel;
                        pArchivePos := @ProgressPosition;
                     End;

                     SaveCrc32Val := Crc32Val;
                     If fEncrypted Then
                     Begin
                        Crc32Val :=
                        	CalcStreamCRC32(DataStream, DataStream.Size,
                           	zsp, @fCancel, GlobalProgress);

                        CryptHDR := ztvEncryptHead(Password, 0, Crc32Val);
                     	SaveCrc32Val := Crc32Val;
                        Crc32Val := CRC_MASK;
                        Outfile.WriteBuffer(CryptHDR[1], RAND_HEAD_LEN);
                        DataStream.Position := 0;
                     End;
                     DataPos := Outfile.Position;

                     BufferSize := WSIZE;
                     PBIT := 5;
                     DICBIT := 14;
                     DICSIZ := 1 Shl DICBIT;
                     BitSize := 32;
                     NP := (DICBIT + 1);
                     ztvEncode(Self, DataStream, Outfile, False);

                     // ------------------------------
                     // using the ztvFused.pas unit...
                     // ------------------------------
                     // Packable := ztvEncode( Self,
                     // 	THandleStream32( DataStream ).Handle,
                     //    THandleStream32(Outfile).Handle,
                     //    Nil, Nil, fEncrypted );
                     // ------------------------------

                     Packable := (DataStream.Size > Outfile.Size - DataPos) And
                        (Not Unpackable);

                     If Packable Then
                        fGlobalCompressType := ZTV_FUSE6
                     Else
                     Begin
                        Crc32Val := SaveCrc32Val;
                        fGlobalCompressType := ZTV_STORED;
                     End;

                     If Packable Then
                        FillHeaderData(DataStream.size, Outfile.Size - DataPos)
                     Else Begin
                     	If DataStream.Size = 0 Then
                        Begin
                        	zsp.Protect := False;
                        	If fEncrypted Then
                           Begin
                        		fEncrypted := False;
										Outfile.Size := DataPos - RAND_HEAD_LEN;
                           End Else
                           	OutFile.Size := DataPos;
                        End Else
                        	Outfile.Size := DataPos;

                        StoreStream(DataStream, Outfile, zsp);
                        If fEncrypted Then
                        	FillHeaderData(DataStream.size, DataStream.size + RAND_HEAD_LEN)
                        Else
                        	FillHeaderData(DataStream.size, DataStream.size)

                     End;

                     Outfile.Position := outStreamPos;
                     Outfile.WriteBuffer(HPtr^, HSize);
                     If fEncrypted Then
                     Begin
                        Outfile.WriteBuffer(fFileName[1], Length(fFileName));
                        Outfile.WriteBuffer(CryptHDR[1], RAND_HEAD_LEN);
                     End;

                     Outfile.Position := Outfile.Size;
                     RefreshHeader(Index, HeadInfo.Attr, pHeaderObj);
                     Result := True;
                  Finally
                  	DataStream.Free();
                  End;
               End;
               Exit;
            End;
         cmStore: DeflateType := dtDeflateS;
      End;

      Inherited;
   Except
      Result := False;
   End;
End;
//-------------------------------------------------------------

Function TBlakHole.GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
         Pointer): Int64;
Begin
   Result :=
      u_long(GetLocalHeaderSize()) +
      HeadInfo.pSize +
      u_long(TBh(pHeader^).FileNameLen) +
      u_long(TBh(pHeader^).CommentLen);
End;
//-------------------------------------------------------------

Function TBlakHole.ReadHeader(s: TStream32; Var FileName: String;
   HeadType: Byte): Boolean;

   Function GetFilename(Var FileName: String; Len: Integer): Boolean;
   Begin
      SetLength(FileName, Len);
      Result := (Len > 0) And (s.Read(FileName[1], Len) = Len);
      If Result Then
      	FileName := OemToCharFilter(FileName, fTransOemChar);
   End;
Begin
   s.Position := LastPos;
   With BhHeader Do
   Begin
      SignAtr := 0;
      Result :=
         (s.Read(BhHeader, fHeaderSize) = fHeaderSize) And
      (SignAtr = BLAKHOLE_SIGNATURE) And
         GetFilename(FileName, FileNameLen);

      LastPos := u_long(s.Position) + PackedSize;
   End;
End;
//-------------------------------------------------------------

Procedure TBlakHole.SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo);
Begin
   With TBh(pHeader^) Do
   Begin
      FileDate := HI^.Date;
      UnpackedSize := HI^.uSize;
      PackedSize := HI^.pSize;
      ExternalAttr := HI^.Attr;
   End;
End;
//------------------------------------------------------------

Procedure TBlakHole.WEI(OutStream: TStream32);
Begin
 // virtual method... do not delete
End;
//------------------------------------------------------------

Procedure TBlakHole.ArcToList(s: TStream32; pHeaderObj: pCompHeaderObj);
Var
   CBFInew: TCompFileInfo;
   FileName: String;
   HeadType: Byte;
Begin
   FilesDeleted := 0;

   If GetFirst(FileName, s, HeadType{1}, pHeaderObj) Then
      With BhHeader Do
         Repeat
            CBFInew.Status := hsCopy;   // default
            If (Not CopySuccess) Then
            Begin

               CBFInew.Offset := PrevPos;
               CBFInew.ExtraFieldLen := 0;
               CBFInew.EncryptedHeader := False;
               CBFInew.FileComment := Nil;
               CBFInew.ExtraField := Nil;
               CBFInew.FileCommentLen := CommentLen;

               If (Switch = swDelete) Then
               Begin
                  If CheckWildCard1(FileName, FileSpec, ExcludeSpec) Then
                  Begin
                     inc(FilesDeleted);
                     CBFInew.Status := hsSkip;
                     pHeaderObj^.AddItem(CBFInew, FileName, @LzhHeader,
                        Length(fRootDir), SizeOf(TLzh));
                     Continue;
                  End;
               End;

               If CommentLen > 0 Then
               Begin
                  GetMem(CBFInew.FileComment, CommentLen + 1);
                  s.Position := LastPos - CommentLen;
                  s.Read(CBFInew.FileComment[0], CommentLen);
               End
               Else
                  CBFInew.FileComment := Nil;

               pHeaderObj^.AddItem(CBFInew, FileName, @BhHeader,
                  Length(fRootDir), fHeaderSize);

               fTotalUnpackedSize :=
                  fTotalUnpackedSize +
                  fHeaderSize +
                  FileNameLen +
                  PackedSize;

            End
            Else
               pHeaderObj^.FileList.Add(FileName);

         Until (Not GetNext(FileName, s, 1));
End;
//-------------------------------------------------------------

Procedure TBlakHole.doFinish(pHeaderObj: pCompHeaderObj);
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
         BhHeader := TBh(Header[i]^);

         With BhHeader Do
         Begin

            fTotalPackedSize := fTotalPackedSize + PackedSize;
            fTotalUnpackedSize := fTotalUnpackedSize + UnpackedSize;

            If (Switch <> swDelete) And (pCBFI^.Status = hsAdd) Then
            Begin
               fFileName := FileList.Strings[i];
               fVersionMadeBy := Word(VerNum);
               fMinVersion := Word(MinVerNum);

               If fEncrypted And (PackedSize >= RAND_HEAD_LEN) Then
                  fPackedSize := PackedSize - RAND_HEAD_LEN
               Else
                  fPackedSize := PackedSize;

               fUnpackedSize := UnpackedSize;
               fCRC := CRC32;
               fEncrypted := (BitFlag And 1) > 0;
               fsCompressType := GetCompressMethodStr(CompressType, BitFlag);
               fwCompressType := CompressType;
               fExternalAttr := ExternalAttr;
               fInternalAttr := InternalAttr;
               fRatio := CalcRatio(PackedSize, UnpackedSize);
               Try
                  If FileDate > 0 Then
                     GlobalDate := FileDateToDateTime(FileDate);
               Except
               End;

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
End;
//-------------------------------------------------------------

Function TBlakHole.doCleanUp(Outfile: TStream32; pHeaderObj: pCompHeaderObj): Boolean;
Begin
   Result := False;
End;
//-------------------------------------------------------------

Procedure TBlakHole.RefreshHeader(Index, ExtAttr: Integer; pHeaderObj: pCompHeaderObj);
Begin
   CopyMem(HPtr, pHeaderObj^.Header[Index], HSize);
End;
//-------------------------------------------------------------

Procedure TBlakHole.doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
   pHeaderObj: pCompHeaderObj);
Var
   FileName: String;
   pBhHeader: ^TBh;
Begin

   With pHeaderObj^ Do
   Begin

      pCBFI := FileLocationData(Index);

      pBhHeader := Header[Index];
      pBhHeader^.FileNameLen := Length(FileList.Strings[Index]);

      (* write header *)
      If (ArcFile.Read(BhHeader, fHeaderSize) <> fHeaderSize) Then
         doWriteError();

      (* Set file pointer after filename *)
      ArcFile.Position :=
         pCBFI^.Offset + u_long(fHeaderSize + BhHeader.FileNameLen);

      BhHeader.FileNameLen := Length(FileList.Strings[Index]);
      If (TempFile.Write(BhHeader, fHeaderSize) <> fHeaderSize) Then
         doWriteError();

      (* write Local Filename *)
      FileName := FileList.Strings[Index];
      If (TempFile.Write(FileName[1], Length(FileName)) <> Length(FileName)) Then
         doWriteError();

      (* write compressed data *)
      With pBhHeader^ Do
      Begin
   		//TempFile.CancelCallBackProc := Nil;
			//TempFile.ProgressCallBackProc := Nil;
         If (TempFile.CopyFrom(ArcFile, PackedSize) <> longint(PackedSize)) Then
            doWriteError();
      End;

   End;
End;
//-------------------------------------------------------------

Procedure TBlakHole.SetArcType(SAT: TArcType);
Begin
   fArcType := atBh;
   CompressArcType();
End;
//-------------------------------------------------------------

End.
