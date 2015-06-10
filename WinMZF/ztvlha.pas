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
Unit ztvLha;

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
   ztvBase,
   ztvHeaders,
   ztvGbls,
   ztvStreams;

{$i ZipTV.inc}                          //Declare the compiler defines

Type
   TLha = Class(TCompBase)
   Private
      fHeaderSize: Integer;
      fMinVersion: word;
      Function CalcSum: Byte;
      Procedure WriteHeader(Outf: TStream32);
   Protected
      Procedure doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
         pHeaderObj: pCompHeaderObj); Override;
      Function GetHeadPtr: Pointer; Override;
      Function GetHeadSize: word; Override;
      Function GetLocalHeaderSize: Integer; Override;
      Function ReadHeader(strm: TStream32; Var FileName: String; HeadType:
         Byte): Boolean; Override;
     	Procedure doOnEnd; Override;
      Procedure PackFile(Outfile: TStream32; Index: Integer; pHeaderObj:
         pCompHeaderObj; Var Result: Boolean); Override;
      Procedure RefreshHeader(Index, ExtAttr: Integer; pHeaderObj:
         pCompHeaderObj); Override;
      Procedure SetArcType(SAT: TArcType); Override;
      Procedure SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo); Override;
      Procedure WEI(OutStream: TStream32); Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Function GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo; Override;
      Function GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
         Pointer): Int64; Override;
      Procedure ArcToList(s: TStream32; pHeaderObj: pCompHeaderObj); Override;
      Procedure doFinish(pHeaderObj: pCompHeaderObj); Override;
   Published
      Property ArcType;
      Property CompressMethod;
      Property CompressionMethod;
      Property DefaultExt;
      Property FileSpec;
      Property IncludeHiddenDirs;
      Property RecurseDirs;
      //Property StoreAlreadyCompressedFiles // v4.6.8 removed.  Added StoreFilesOfType property
      Property StoredDirNames;
      Property StoreEmptySubDirs;
      Property StoreFilesOfType;
      Property Switch;
      Property TempDir;
      Property OnNonWriteableArchive;
      Property OnProgress;
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
{$IFDEF FROZEN1_SUPPORT}	// defined in ZipTV.inc
   ztvLhaPack,
{$ENDIF}
   ztvLzh5,
   Err_Msgs;

Const
   LHA_VERNUM = 350;
   LHA_MINVERNUM = 112;

   //-------------------------------------------------------------

Constructor TLha.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fMasterExt := '.LHA';
   DefaultExt := fMasterExt;
   //fHeaderSize := LZHHdr_Size + SizeOf( LzhHeader.crc16 );
   fHeaderSize := SizeOf(TLzh);
{$IFDEF FROZEN1_SUPPORT}
   CompressMethodState := [cmStore, cmFrozen1, cmFrozen5, cmFrozen6 {, cmFrozen7}];
{$ELSE}
   CompressMethodState := [cmStore, cmFrozen5, cmFrozen6 {, cmFrozen7}];
{$ENDIF}
   CompressMethod := cmFrozen6;
   fArcType := atLha;
End;
//-------------------------------------------------------------

Destructor TLha.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TLha.WriteHeader(Outf: TStream32);
Begin
   With LzhHeader Do
      If (Outf.Write(LzhHeader, LZHHdr_Size) <> LZHHdr_Size) Or
         (Outf.Write(fFileName[1], FileNameLen) <> FileNameLen) Or
         (Outf.Write(CRC16, SizeOf(CRC16)) <> SizeOf(CRC16)) Then
         doWriteError();
End;
//-------------------------------------------------------------

Function TLha.CalcSum: Byte;
Type
   HdrArray = Array[0..$FFFF] Of Byte;
Var
   i, CheckSum: Integer;
   HeaderPtr: ^HdrArray;
Begin
   CheckSum := 0;
   HeaderPtr := GetHeadPtr();
   For i := 2 To LZHHdr_Size - 1 Do
      inc(CheckSum, HeaderPtr^[i]);

   For i := 1 To LzhHeader.FileNameLen Do
      inc(CheckSum, Byte({LzhFilename} fFileName[i]));

   inc(CheckSum, Lo(LzhHeader.CRC16) + HI(LzhHeader.CRC16));
   Result := CheckSum;
End;
//-------------------------------------------------------------

Function TLha.GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
   Pointer): Int64;
Begin
   With TLzh(pHeader^) Do
      Case level Of
         0, 1: Result := HeadLen + 2 + PackedSize;
      Else
         Result := HeadLen + PackedSize;
      End;
End;
//-------------------------------------------------------------

Procedure TLha.doOnEnd;
Begin
   With LzhHeader Do
      If Assigned(OnEnd) Then
         OnEnd(Self, fFileName, Crc16Val = CRC16);
End;
//-------------------------------------------------------------

Procedure TLha.PackFile(Outfile: TStream32; Index: Integer; pHeaderObj:
   pCompHeaderObj; Var Result: Boolean);
Var
   i: Integer;
   Ext: String;
   CM: TCompressMethod;
   SaveFilePos: Cardinal;
   zsp: ztv_stream_plus;
   DataStream: TFileStream32;
   StoreStream: TStoreStream;
   Packable: Boolean;

Begin
   Result := False;
   fFileName := pHeaderObj^.FileList.Strings[Index]; //non-formating assignment
   fFileName := CharToOemFilter(fFileName, fTransOemChar);

   HPtr := GetHeadPtr();

   With LzhHeader Do
   Begin

      pCBFI^.Offset := Outfile.size;

      If (HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
      Begin

         If StoreEmptySubDirs Then
         Begin

         	If Not IsDirEmpty(pHeaderObj^.FileList.Strings[Index]) Then
            	Exit;

            // v4.1 revised
            FileName := AppendDirTail(pHeaderObj^.FileList.Strings[Index]);
            If fFileName = '' Then
               Exit;

            SignBegin := Byte('-');
            l := Byte('l');
            H := Byte('h');
            SignEnd := Byte('-');

            FileNameLen := Length(fFileName);
            HeadLen := LZHHdr_Size + Length(fFileName);
            ExternalAttr := FILE_ATTRIBUTE_DIRECTORY;
            CompressType := 100;
            PackedSize := 0;
            UnpackedSize := 0;
            CRC16 := 0;
            FileDate := GetDateTime(HeadInfo.Date);
            Headchk := CalcSum();
            WriteHeader(Outfile);
            RefreshHeader(Index, HeadInfo.Attr, pHeaderObj);
            Result := True;
         End;

      End
      Else
      Begin

         Try
            If doOnBegin(fFileName, Count, pHeaderObj) Then
            Try
               pCBFI^.Offset := Outfile.size;

               DataStream :=
                  TFileStream32.Create(
                  pHeaderObj^.FileList.Strings[Index],
                  fmOpenRead Or fmShareDenyNone);

               If (DataStream.Handle < 0) Then
               Begin
                  RaiseErrorStr(pHeaderObj^.FileList.Strings[Index], '', '0', E_FOPEN);
                  Exit;
               End;

               Try
                  Bytes_To_Go := DataStream.size;
                  Crc32Val := CRC_MASK;
                  fEncrypted := False; //Length( Password ) > 0;

                  //If fEncrypted Then
                  //Begin
                  //   Crc32Val := CalcStreamCRC( DataStream );
                  //   CryptHDR := EncryptHead( Password, 0, Crc32Val );
                  //   Crc32Val := CRC_MASK;
                  //   DataStream.Position := 0;
                  //End;

                  HeadLen := LZHHdr_Size + Length(fFileName);
                  Headchk := 0;
                  SignBegin := Byte('-');
                  l := Byte('l');
                  H := Byte('h');
                  SignEnd := Byte('-');
                  UnpackedSize := DataStream.size; //ztvGetFileSize( DataFile );
                  //ckedSize := DataStream.size;
                  FileNameLen := Length(fFileName);
                  FileDate := GetDateTime(HeadInfo.Date);
                  ExternalAttr := HeadInfo.Attr;

                  SaveFilePos := OutFile.Size;
                  CM := CompressMethod;

                  If (DataStream.size > 0) Then
                  Begin

                     // v4.6.8 removed StoreAlreadyCompressedFiles.  Added StoreFilesOfType property
                     If (CM <> cmStore) {And StoreAlreadyCompressedFiles} Then
                     Begin
                        Ext := ExtractFileExt(fFileName);
                        If (Ext <> '') {And (UpperCase(Ext) <> '.EXE')} Then
                           //For i := 0 To MaxExtArray Do
                           For i := 0 To StoreFilesOfType.Count - 1 Do
                              //If CompareText(Ext, ExtArray[i]) = 0 Then
                              If CompareText(Ext, StoreFilesOfType.Strings[i]) = 0 Then
                              Begin
                                 CM := cmStore;
                                 break;
                              End;
                     End;

                     Crc16Val := 0;
                     WriteHeader(Outfile);

                     Unpackable := False;
                     DataStream.Position := 0;

                     Try
                        Case CM Of

                           cmStore:
                              Begin
                                 Unpackable := True;
                                 Result := True;
                              End;

{$IFDEF FROZEN1_SUPPORT}
                           cmFrozen1:
                              Begin
                                 CompressType := ztv_FROZEN1;
                                 LzhPack(Self, TFileStream32(DataStream).Handle, Outfile, Nil, Nil);
                                 Result := True;
                              End;
{$ENDIF}

                           cmFrozen5:
                              Begin
                                 CompressType := ztv_FROZEN5;
                                 BufferSize := WSIZE;
                                 PBIT := 4;
                                 DICBIT := 13;
                                 DICSIZ := 1 Shl DICBIT;
                                 NP := (DICBIT + 1);
                                 BitSize := 16;
                                 Result :=
                                 	ztvEncode(Self, DataStream, Outfile, False);
                              End;

                           cmFrozen6:
                              Begin
                                 CompressType := ztv_FROZEN6;
                                 BufferSize := WSIZE; //26624;
                                 PBIT := 5; //4
                                 DICBIT := 14;
                                 DICSIZ := 1 Shl DICBIT;
                                 NP := (DICBIT + 1);
                                 BitSize := 16;
                                 Result :=
                                 	ztvEncode(Self, DataStream, Outfile, False);
                              End;

                           //cmFrozen7:
                           //	Begin
                           //      CompressType := ztv_FROZEN7;
                           //      BufferSize := WSIZE {* 2};
                           //      PBIT := 5;
                           //      DICBIT := 16;
                           //      DICSIZ := 1 Shl DICBIT;
                           //      NP := (DICBIT + 1);
                           //      BitSize := 16;
                           //      Result := ztvEncode(Self, TFileStream32(DataStream).Handle, TFileStream32(Outfile).Handle, Nil, Nil, False);
                           //   End;

                        End;
                     Except
                        pCBFI^.Status := hsSkip; //ON e : exception DO ShowMessage( e.message);
                     End;

                     If (Not Result) Then Exit;

                     Packable :=
                        (DataStream.size > Outfile.Size - SaveFilePos) And
                        (Not Unpackable);

                     level := 0;

                     If Packable Then
                     Begin
                        CRC16 := Crc16Val;
                        LzhHeader.PackedSize := Outfile.Size - SaveFilePos - LZhHdr_Size - FileNameLen - SizeOf(Crc16);
                        Headchk := CalcSum();
                        OutFile.Position := SaveFilePos;
                        WriteHeader(Outfile);
                     End
                     Else
                     Begin
                        CompressType := ztv_FROZEN0; // stored

                        // Resize file if previous compress attempt resulted
                        // in a compressed size > original file size.
                        Outfile.Size := SaveFilePos + LZHHdr_Size + FileNameLen + SizeOf(Crc16);
                        Outfile.Position := Outfile.Size;

                        With zsp Do
                        Begin
                           Protect := False;
                           CRC := 0; //CRC_MASK;
                           pCancel := @fCancel;
                           pArchivePos := @ProgressPosition;
                        End;

                        StoreStream :=
                        	TStoreStream.Create(Outfile, 16, zsp, @fCancel,
                              GlobalProgress);
                        Try
                           Result :=
                              StoreStream.CopyStream(DataStream) = DataStream.Size;
                        Finally
                           Crc16Val := StoreStream.FZRec.cb.Crc;
                           Crc16 := Crc16Val;
                           StoreStream.Free();
                        End;

                        If Result Then
                        Begin
                           LzhHeader.PackedSize := DataStream.Size;
                           Headchk := CalcSum();
                           OutFile.Position := SaveFilePos;
                           WriteHeader(Outfile);
                        End;
                     End;
                  End
                  Else
                  Begin
                     Crc16Val := 0;
                     CRC16 := Crc16Val;
                     CompressType := ztv_FROZEN0; // stored
                     LzhHeader.PackedSize := 0;
                     Headchk := CalcSum();
                     Outfile.Position := SaveFilePos;
                     WriteHeader(Outfile);
                     Result := True;
                  End;
                  RefreshHeader(Index, HeadInfo.Attr, pHeaderObj);
               Finally
               	OutFile.Position := OutFile.Size;
                  DataStream.Free();
               End;
            Finally
            	Result := Result And True;
               doOnEnd();
            End;
         Except
            On EFOpenError Do           // Stream Read Error
               RaiseErrorStr(pHeaderObj^.FileList.Strings[Index], '', '0', E_FOPEN)
         Else
            Raise;
         End;
      End;
   End;
End;
//-------------------------------------------------------------

Function TLha.GetHeadPtr: Pointer;
Begin
   Result := @LzhHeader;
End;
//------------------------------------------------------------

Function TLha.GetLocalHeaderSize: Integer;
Begin
   Result := LZHHdr_Size + SizeOf(LzhHeader.CRC16); //fHeaderSize;
End;
//-------------------------------------------------------------

Function TLha.GetHeadSize: word;
Begin
   Result := fHeaderSize;
End;
//-------------------------------------------------------------

Function TLha.ReadHeader(strm: TStream32; Var FileName: String; HeadType: Byte):
   Boolean;
Var
   pWord: ^word;
	HeadSize: Word;
   Dir: AnsiString;
   pFilename: PChar;
Begin
   With LzhHeader Do
   Begin
      strm.Position := LastPos;
      ZeroMemory(@LzhHeader, SizeOf(TLzh));
      strm.Read(LzhHeader, LZHHdr_Size);
      Result := (HeadLen <> 0);

      If Result Then
      Begin
         GetMem(pFilename, 256);
         Try
            ZeroMemory(pFilename, 256);
            Case level Of
               0, 1:                    // do nothing
               	Begin
                     strm.Read(pFilename^, FileNameLen);
                     FileName := StrPas(pFileName);
                     strm.Read(CRC16, 5);
                  End;
               2:                       // adjust beginning offset
                  Begin
                     strm.Position := strm.Position - 1;
                     strm.Read(CRC16, SizeOf(CRC16));
                     strm.Read(OS, SizeOf(OS));
                     strm.Read(FileNameLen, SizeOf(FileNameLen));
                     strm.Seek(2, soCurrent);

                     //Strm.Position := Strm.Position + 5; //Inc( fFilePos, 5 );
                     If FileNameLen > 3 Then Dec(FileNameLen, 3);
            			strm.Read(pFilename^, FileNameLen);
               		FileName := StrPas(pFilename);
                     If OS = LHA_UNIXOS Then
								FileDate :=
                  			DateTimeToFileDate(UnixDateToDos(FileDate));
                  End;
            Else
            End;


            HeadSize := ExtHeaderSize;
            While (HeadSize > 0) And (HeadSize < 256) Do
            Begin
               Strm.Read(pFileName^, HeadSize);
               Case Byte(pFileName^) Of
                  0: ;
                  1: ;
                  2:
                     Begin
                        Dir := DecodeDir(StrPas(pFileName));
                        FileName := AppendDirTail(Dir) + FileName;
                     End;
                  $40: ;
               End;

               pWord := @pFileName[HeadSize - 2];
                HeadSize := pWord^;
            End;

            If level = 2 Then
               strm.Read(HeadSize, SizeOf(ExtHeaderSize));

            Case level Of
               0, 1:
                  LastPos := LastPos + HeadLen + PackedSize + 2 {+ PackedMinus};
               2: LastPos := LastPos + HeadLen + PackedSize {+ PackedMinus};
            Else
               inc(LastPos, HeadLen {+ PackedMinus});
            End;

         Finally
            FreeMem(pFilename, 256);
         End;
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TLha.ArcToList(s: TStream32; pHeaderObj: pCompHeaderObj);
Var
   HeadType: Byte;
   CBFInew: TCompFileInfo;
   FileName: String;
Begin
   FilesDeleted := 0;

   If GetFirst(FileName, s, HeadType{0}, pHeaderObj) Then
      With LzhHeader Do
         Repeat

            FileName := OemToCharFilter(FileName, fTransOemChar);

            CBFInew.Status := hsCopy;   // default
            If (Not CopySuccess) Then
            Begin

               CBFInew.Offset := PrevPos;
               CBFInew.ExtraFieldLen := 0;
               CBFInew.EncryptedHeader := False;
               CBFInew.FileComment := Nil;
               CBFInew.ExtraField := Nil;
               CBFInew.FileCommentLen := 0;

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

               pHeaderObj^.AddItem(CBFInew, FileName, @LzhHeader,
                  Length(fRootDir), SizeOf(TLzh));

               fTotalUnpackedSize :=
                  fTotalUnpackedSize +
                  LZHHdr_Size +         //SizeOf( TLzh ) +
               //SizeOf( Crc16 ) +
               FileNameLen +
                  PackedSize;

            End
            Else
               pHeaderObj^.FileList.Add(FileName);

         Until (Not GetNext(FileName, s, 0));

End;
//-------------------------------------------------------------

Procedure TLha.doFinish(pHeaderObj: pCompHeaderObj);
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
         LzhHeader := TLzh(Header[i]^);

         With LzhHeader Do
         Begin

            fTotalPackedSize := fTotalPackedSize + PackedSize;
            fTotalUnpackedSize := fTotalUnpackedSize + UnpackedSize;

            If (Switch <> swDelete) And (pCBFI^.Status = hsAdd) Then
            Begin
               fFileName := OemToCharFilter(FileList.Strings[i], fTransOemChar);
               fVersionMadeBy := word(LHA_VERNUM);
               fMinVersion := word(LHA_MINVERNUM);
               fPackedSize := PackedSize;
               fUnpackedSize := UnpackedSize;
               fCRC := CRC16;
               fEncrypted := False;
               fsCompressType := GetCompressMethodStr(CompressType, 0);
               fwCompressType := CompressType;
               fExternalAttr := ExternalAttr;
               fInternalAttr := InternalAttr;
               fRatio := CalcRatio(PackedSize, UnpackedSize);
               If FileDate > 0 Then
                  GlobalDate := FileDateToDateTime(FileDate);

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

Procedure TLha.doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
   pHeaderObj: pCompHeaderObj);
Var
   OldLen, NewLen: word;
   Bytes: Integer;
Begin
   // Read existing header
   ArcFile.Read(LzhHeader, LZHHdr_Size);

   NewLen := Length(pHeaderObj^.FileList.Strings[Index]);
   OldLen := LzhHeader.FileNameLen;

   // Set relational header fields
   With LzhHeader Do
   Begin
      HeadLen := HeadLen + NewLen - OldLen;
      fFileName := pHeaderObj^.FileList.Strings[Index];
      FileNameLen := NewLen;
      If OffsetToCompressed > 0 Then
         OffsetToCompressed := OffsetToCompressed + NewLen - OldLen;

      CRC16 := Crc16Val;
      Headchk := CalcSum();

      // Set file pointer after filename... retrieve stored crc value
      ArcFile.Position := pCBFI^.Offset + LZHHdr_Size + OldLen;
      ArcFile.Read(CRC16, SizeOf(CRC16));
   End;

   // Write revised header
   WriteHeader(TempFile);

   // Set file pointer after header
   ArcFile.Position :=
      pCBFI^.Offset +
      LZHHdr_Size +
      OldLen +
      SizeOf(LzhHeader.CRC16);

   // Copy comment
   // comments not supported

   // Write compressed data
   Bytes := HeadInfo.pSize;

   TempFile.CancelCallBackProc := Nil;
   TempFile.ProgressCallBackProc := Nil;
   If Bytes <> TempFile.CopyFrom(ArcFile, Bytes) Then
      doWriteError();
End;
//-------------------------------------------------------------

Procedure TLha.RefreshHeader(Index, ExtAttr: Integer; pHeaderObj: pCompHeaderObj);
Begin
   CopyMem(HPtr, pHeaderObj^.Header[Index], fHeaderSize {HSize});
End;
//-------------------------------------------------------------

Procedure TLha.SetArcType(SAT: TArcType);
Begin
   fArcType := atLha;
   CompressArcType;
End;
//-------------------------------------------------------------

Function TLha.GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo;
Begin
   With Result, TLzh(p^) Do
   Begin
   	Date := FileDate;
      Attr := ExternalAttr;
      If CompressType = 100 Then
      	Attr := Attr Or ZTV_FILE_ATTRIBUTE_DIRECTORY;
      pSize := PackedSize;
      uSize := UnpackedSize;
   End;
End;
//-------------------------------------------------------------

Procedure TLha.SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo);
Begin
   With TLzh(pHeader^) Do
   Begin
      FileDate := HI^.Date;
      UnpackedSize := HI^.uSize;
      PackedSize := HI^.pSize;
      ExternalAttr := HI^.Attr;
   End;
End;
//-------------------------------------------------------------

Procedure TLha.WEI(OutStream: TStream32);
Begin
 // virtual method... do not delete
End;
//------------------------------------------------------------

End.
