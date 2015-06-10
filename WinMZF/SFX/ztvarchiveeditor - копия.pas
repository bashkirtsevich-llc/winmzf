(**********************************************************************

 Copyright 1998-2003,  Microchip Data Systems / Carl Bunton

  Under license agreement, this source module may be used only on a
  single computer.

  No portion of this module may be reproduced, copied, revised, edited,
  distributed or transmitted via electronic means except in compiled
  application format.

  Web-site:  http://www.ziptv.com
  Email:     custsupt@ziptv.com

  ------------------------------------------------------------------

  Operational Notes:

    	1. Assign the ArchiveFile property.
      2. Call the StartEdit method for the ArchiveFile assigned in #1.
      After a call to StartEdit, RollBack or CommitUpdates MUST be called
      to free memory allocated which is allocated in the StartEdit method.
      3. Use any of the following properties to revise an archive's record.

      	Property FileAttr[x: Integer]      // get/set file's attribute (type integer)
      	Property FileAttrs[x: Integer]     // get/set file's attribute (type TztvFileAttrs)
      	Property FileDate[x: Integer]      // get/set file's date      (type integer)
      	Property FileDateStr[x: Integer]   // get/set file's date      (type string)
      	Property FileDateTime[x: Integer]  // get/set file's date      (type TDateTime)
      	Property FileName[x: Integer]      // get/set file's name      (type string)

      4. Method Rollback: do not save changes to the archive.  Free all
      allocated memory.  Either RollBack or CommitUpdates MUST be called
      after a StartEdit to free allocated memory.
      5. Method CommitUpdates: save changes to the archive.  After changes
      have been saved, CommitUpdates calls the RollBack method to free all
      allocated memory.  Either RollBack or CommitUpdates MUST be called
      after a StartEdit to free allocated memory.


      Additional properties, methods, and functions
      1. Property RecordChanged[x: Integer]: (ReadOnly) returns if a specific
      record has been changed prior to a call to CommitUpdates or RollBack.
      2. Method ArchiveChanged: (ReadOnly) returns true/false, if any record
      in the archive has been revised prior to a call to CommitUpdates or
      RollBack.
      3. Property Attributes: (type TztvFileAttrs) see demo for usage.
		4. Method BatchRenameFiles: refer to demo ArchiveEditor\BatchRename\
      project1.dpr for usage notes.
      5. Method RestoreRecord(Index: Integer): prior to any calls to RollBack
      or CommitUpdates, this method resets a revised record back to it's
      original unchanged state.  For example, say you have revised a set of
      compressed file's names, dates, and attributes... you've changed your
      mind about one of the revisions.  Instead of losing all edits and
      restarting from scratch, this method allows you to reset the one
      specific record back to it's original state.

      Events:
      1. OnGetCompressedFileName: retrieve the current compressed file's name,
      with a call to the StartEdit method.

      This event is generally used to fill a TListBox, TListView, or spread-
      sheet with filenames from the archive.  Refer to demo ArchiveEditor\
      ByHeader\project1.dpr for demo usage.

      2. OnBatchSkipFile: activates with a call to the BatchRenameFiles
      function.  It is simular to OnGetCompressedFileName in that it provides
      the name of each compressed file, but differs in that it contains a
      var parameter (Var SkipChange) which allows a specific file be skipped
      when renaming a range or a batch of files.  See BatchRenameFiles above.

      Final Notes:
      1. The Unix .tar format contains file attributes not compatible with
      windows.  For this reason, this component does not support the revision
      of .tar file attributes.  

**********************************************************************)
Unit ztvArchiveEditor;

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
	ztvStreams,
   ztvZipTV;

{$i ZipTV.inc}                          //Declare the compiler defines

Type
   TOnBatchSkipFile =
      Procedure(Sender: TObject; NewFileName: String; Index: Integer;
      Var SkipChange: Boolean)
      Of Object;

	TOnGetCompressedFileName =
   	Procedure(Sender: TObject; FileName: String)
      Of Object;

   TArchiveEditor = Class(TZipCommon)
   Private
      MasterCRC: u_long;
		ArchiveCRC: u_long;
      CompressComponent: TCompBase;
      Crc32List: TList;
   	//EncryptedList: TList;	  // rem'd for future addition
      HeadObj: TCompHeaderObj;
      pHeaderObj: pCompHeaderObj;
      fOnBatchSkipFile: TOnBatchSkipFile;
      //fCaseSensitive: Boolean;	// rem'd for future addition
      fOnGetCompressedFileName: TOnGetCompressedFileName;
      Function GetFileAttr(x: Integer): Integer;
      Function GetFileAttrs(x: Integer): TztvFileAttrs;
      Function GetFileDate(x: Integer): Integer;
      Function GetFileDateStr(x: Integer): String;
      Function GetFileDateTime(x: Integer): TDateTime;
      Function GetFileNames(x: Integer): String;
      Function GetRecordChanged(x: Integer): Boolean;
      Procedure SetFileAttr(x: Integer; Attr: Integer);
      Procedure SetFileAttrs(x: Integer; Attr: TztvFileAttrs);
      Procedure SetFileDate(x: Integer; Date: Integer);
      Procedure SetFileDateStr(x: Integer; Date: String);
      Procedure SetFileDateTime(x: Integer; Date: TDateTime);
      Procedure SetFileNames(x: Integer; FilesName: String);
   Protected
      Procedure SetArchiveFile(SAF: String); Override;
		Procedure CalcFieldCrc(p: Pointer; Len: Integer; Var Crc: u_long);
   Public
      FilesCount: Integer;
      isInitialized: Boolean;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
		Function ArchiveChanged: Boolean;
		Function BatchRenameFiles(OldSpec, NewSpec, Path: String): Integer;
		Function GetFileCount: Integer;
      Function RestoreRecord(Index: Integer): Boolean;
		Procedure CommitUpdates;
		Procedure RollBack;
		Procedure StartEdit;
      Property Attributes;
      Property FileAttr[x: Integer]: Integer Read GetFileAttr Write SetFileAttr;
      Property FileAttrs[x: Integer]: TztvFileAttrs Read GetFileAttrs Write SetFileAttrs;
      Property FileDate[x: Integer]: Integer Read GetFileDate Write SetFileDate;
      Property FileDateStr[x: Integer]: String Read GetFileDateStr Write SetFileDateStr;
      Property FileDateTime[x: Integer]: TDateTime Read GetFileDateTime Write SetFileDateTime;
      Property FileName[x: Integer]: String Read GetFileNames Write SetFileNames;
      Property RecordChanged[x: Integer]: Boolean Read GetRecordChanged;
   Published
      Property ArchiveFile;
      Property ArcType;
      //Property CaseSensitive: Boolean Read fCaseSensitive Write fCaseSensitive;
      Property DeleteOptions;
   	Property OnError;
      Property RecurseDirs;
      Property TranslateOemChar;

      // activated once per compressed file when the StartEdit() method is called
      Property OnGetCompressedFileName: TOnGetCompressedFileName Read
      	fOnGetCompressedFileName Write fOnGetCompressedFileName;
      Property OnBatchSkipFile: TOnBatchSkipFile Read fOnBatchSkipFile
      	Write fOnBatchSkipFile;
   End;


Implementation


Uses
   Err_Msgs,
   ztvHeaders,
   ztvBlakHole,
   ztvGZip,
   ztvJar,
   ztvLha,
   ztvMakeCab,
   ztvTar,
   ztvZip;


//-------------------------------------------------------------

Constructor TArchiveEditor.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   //EncryptedList := TList.Create();
   Crc32List := TList.Create();
   FilesCount := 0;
	isInitialized := False;
   RecurseDirs := False;
End;
//-------------------------------------------------------------

Destructor TArchiveEditor.Destroy;
Begin
   Crc32List.Free();
   //EncryptedList.Free();
	Inherited Destroy;
End;
//-------------------------------------------------------------

Function TArchiveEditor.GetFileAttrs(x: Integer): TztvFileAttrs;
Var
	TempAttr: Integer;
Begin
   Attributes := [];
   Try
      TempAttr := GetFileAttr(x);
      SetAttribute(fsZeroAttr,   TempAttr = 0);
      SetAttribute(fsArchive,    TempAttr And FILE_ATTRIBUTE_ARCHIVE > 0);
      SetAttribute(fsReadOnly,   TempAttr And FILE_ATTRIBUTE_READONLY > 0);
      SetAttribute(fsHidden,     TempAttr And FILE_ATTRIBUTE_HIDDEN > 0);
   	SetAttribute(fsVolumeID,   TempAttr And SysUtils.faVolumeID > 0);
      SetAttribute(fsSysFile,    TempAttr And FILE_ATTRIBUTE_SYSTEM > 0);
      SetAttribute(fsDirectory,  TempAttr And FILE_ATTRIBUTE_DIRECTORY > 0);
      SetAttribute(fsCompressed, TempAttr And FILE_ATTRIBUTE_COMPRESSED > 0);
   Finally
   	Result := Attributes;
   End;
End;
//-------------------------------------------------------------

// v6.3.1 revised
Function TArchiveEditor.GetFileAttr(x: Integer): Integer;
Var
   HI: THeaderInfo;
Begin
   pCBFI := pHeaderObj^.FileLocationData(x);
   HI :=	CompressComponent.GetHeaderInfo(pHeaderObj^.Header[x], pCBFI);
   Result := HI.Attr;
End;
//-------------------------------------------------------------

Function TArchiveEditor.GetFileCount: Integer;
Begin
	Result := pHeaderObj.FileList.Count;
End;
//-------------------------------------------------------------

Function TArchiveEditor.GetFileDateStr(x: Integer): String;
Var
	DateTime: TDateTime;
Begin
   If isInitialized Then
   Begin
   	DateTime := GetFileDateTime(x);
      Result := DateTimeToStr(DateTime);
   End
End;
//-------------------------------------------------------------

Function TArchiveEditor.GetFileDateTime(x: Integer): TDateTime;
Begin
   Result := ztvConvertDate(GetFileDate(x));
End;
//-------------------------------------------------------------

Function TArchiveEditor.GetFileDate(x: Integer): Integer;
Var
   HI: THeaderInfo;
Begin
   pCBFI := pHeaderObj^.FileLocationData(x);
   HI :=	CompressComponent.GetHeaderInfo(pHeaderObj^.Header[x], pCBFI);
   Result := HI.Date;
End;
//-------------------------------------------------------------

Function TArchiveEditor.GetFileNames(x: Integer): String;
Begin
	Result := pHeaderObj.FileList.Strings[x];
End;
//-------------------------------------------------------------

Function TArchiveEditor.GetRecordChanged(x: Integer): Boolean;
Var
   _FileName: String;
   _FileAttr: Integer;
   _FileDate: Integer;
Begin
   If ArchiveChanged() Then
   Begin
      _FileName := FileName[x];
      _FileAttr := FileAttr[x];
      _FileDate := FileDate[x];

      Crc32Val := CRC_MASK;
      //If fCaseSensitive Then
      //   CalcFieldCrc(PChar(_FileName), Length(_FileName), Crc32Val)
      //Else
         CalcFieldCrc(PChar(UpperCase(_FileName)), Length(_FileName), Crc32Val);

      CalcFieldCrc(@_FileAttr, SizeOf(_FileAttr), Crc32Val);
      CalcFieldCrc(@_FileDate, SizeOf(FileDate[x]), Crc32Val);
      Result := Crc32Val <> u_long(Crc32List.Items[x]);
   End Else
   	Result := False;
End;
//-------------------------------------------------------------

Procedure TArchiveEditor.SetFileAttrs(x: Integer; Attr: TztvFileAttrs);
Begin
	SetFileAttr(x, AttributesToInt(Attr));
End;
//-------------------------------------------------------------

Procedure TArchiveEditor.SetFileAttr(x: Integer; Attr: Integer);
Var
	_FileAttr: Integer;
Begin
	// revision of .tar file attributes is not support
	If ArcType = atTar Then
   	Exit;

	If isInitialized And
   	(Attr And SysUtils.faVolumeID = 0) And
   	(Attr And FILE_ATTRIBUTE_DIRECTORY = 0) Then
   Begin

		_FileAttr := FileAttr[x];

      // Lha can set a dir attribute using the above assignment _FileAttr := FileAttr[x].
      // Recheck... exit if it is a directory.
      If _FileAttr And ZTV_FILE_ATTRIBUTE_DIRECTORY > 0 Then
      	Exit;

      Case ArcType Of
         atBH:	TBh(pHeaderObj^.Header[x]^).ExternalAttr := Attr;
         atJAR: TCentral(pHeaderObj^.Header[x]^).ExternalAttr := Attr;
         atLHA,
            atLZH: TLzh(pHeaderObj^.Header[x]^).ExternalAttr := Attr;
         atTAR: Exit; // .tar file attributes are not editable
         atZIP: TCentral(pHeaderObj^.Header[x]^).ExternalAttr := Attr;
      Else
      	Exit;
      End;

      Crc32Val := CRC_MASK;
      CalcFieldCrc(@_FileAttr, SizeOf(_FileAttr), Crc32Val);
      ArchiveCRC := ArchiveCRC - Crc32Val;

      Crc32Val := CRC_MASK;
      CalcFieldCrc(@Attr, SizeOf(Attr), Crc32Val);
      ArchiveCRC := ArchiveCRC + Crc32Val;
   End;
End;
//-------------------------------------------------------------

Procedure TArchiveEditor.SetFileDateStr(x: Integer; Date: String);
   Function IsDateValid(Const S: String; Var Date: TDateTime): BOOLEAN;
   Begin
     Try
       Date := StrToDateTime(S);
       Result := True;
     Except
       Result := False;
     End;
   End;
Var
   iDate: Integer;
	DateTime: TDateTime;
Begin
   If isInitialized And (Date <> '') And (IsDateValid(Date, DateTime)) Then
   Begin
      iDate := DateTimeToFileDate(DateTime);
      SetFileDate(x, iDate);
   End
End;
//-------------------------------------------------------------

Procedure TArchiveEditor.SetFileDateTime(x: Integer; Date: TDateTime);
Begin
	If isInitialized Then
      SetFileDate(x, DateTimeToFileDate(Date));
End;
//-------------------------------------------------------------

Procedure TArchiveEditor.SetFileDate(x: Integer; Date: Integer);
Var
   MemBuf: PChar;
   _FileDate: Integer;
Begin
	If isInitialized Then
   Begin

      _FileDate := FileDate[x];

      Case ArcType Of
         atBH:	TBh(pHeaderObj^.Header[x]^).FileDate := Date;
         atJAR: TCentral(pHeaderObj^.Header[x]^).zc.FileDate := Date;
         atLHA,
            atLZH: TLzh(pHeaderObj^.Header[x]^).FileDate := Date;
         atTAR:
         	Begin
            	GetMem(MemBuf, 1024);
               Try
                  IntToOctStr(
                  	DosDateToUnix(FileDateToDateTime(Date)),
                  	SizeOf(TarHeader.MTime), True, MemBuf);

                  CopyMem(@MemBuf[0], @TTarHeader(pHeaderObj^.Header[x]^).MTime, SizeOf(TarHeader.MTime));
               Finally
               	FreeMem(MemBuf, 1024);
               End;
            End;
         atZIP: TCentral(pHeaderObj^.Header[x]^).zc.FileDate := Date;
      Else
      	Exit;
      End;

      Crc32Val := CRC_MASK;
      CalcFieldCrc(@_FileDate, SizeOf(_FileDate), Crc32Val);
      ArchiveCRC := ArchiveCRC - Crc32Val;

      Crc32Val := CRC_MASK;
      CalcFieldCrc(@Date, SizeOf(Date), Crc32Val);
      ArchiveCRC := ArchiveCRC + Crc32Val;
   End;
End;
//-------------------------------------------------------------

Procedure TArchiveEditor.SetFileNames(x: Integer; FilesName: String);
Var
	_FileName: String;
Begin
	If isInitialized And (pHeaderObj.FileList.Strings[x] <> '') Then
   Begin
      Crc32Val := CRC_MASK;
      _FileName := FileName[x];

      // prevent edit of lha path names
      If (ArcType = atLha) And
      	(TLzh(pHeaderObj^.Header[x]^).CompressType = 100) Then
      	FilesName := FileName[x];

      // - addition for a later date
      //If CaseSensitive Then
      //Begin
      //   If (pHeaderObj.FileList.Strings[x] <> FilesName) Then
      //   Begin
      //      pHeaderObj.FileList.Strings[x] := FilesName;
      //
      //      CalcFieldCrc(PChar(_FileName), Length(_FileName), Crc32Val);
      //      ArchiveCRC := ArchiveCRC - Crc32Val;
      //      Crc32Val := CRC_MASK;
      //      CalcFieldCrc(PChar(FilesName), Length(FilesName), Crc32Val);
      //      ArchiveCRC := ArchiveCRC + Crc32Val;
      //   End;
      //End Else Begin
         pHeaderObj.FileList.Strings[x] := FilesName;
         CalcFieldCrc(PChar(UpperCase(_FileName)), Length(_FileName), Crc32Val);
         ArchiveCRC := ArchiveCRC - Crc32Val;
         Crc32Val := CRC_MASK;
         CalcFieldCrc(PChar(UpperCase(FilesName)), Length(FilesName), Crc32Val);
         ArchiveCRC := ArchiveCRC + Crc32Val;
      //End;
   End;
End;
//-------------------------------------------------------------

Procedure TArchiveEditor.SetArchiveFile(SAF: String);
Var
	s: TFileStream32;
Begin

 	(* Do NOT use "Count := 0" in this procedure! *)
   If Not FileExists(SAF) Then
   Begin
      fArcType := atNA;
      fArchiveFile := '';
      Exit;
   End;

   SAF := UnixToDosFilename(SAF);
   fArchiveFile := SAF;

   s := TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyWrite );
   If (s.Handle < 0) Then
   Begin
      fArcType := atNA;
      Raise EFOpenError.CreateFmt(LoadStr(E_FOPEN), [fArchiveFile]);
   End;

   Try
   	fArcType := GetArcType(s);
   Finally
   	s.Free();
   End;

   If (fArcType In Compress_ArcType) And (Not (fArcType In Invalid_ArcType)) Then
   	InitializeVolumeSet()
   Else
   	fArcType := atNA;

End;
//------------------------------------------------------------

Function TArchiveEditor.ArchiveChanged: Boolean;
//Var
//	Index: Integer;
Begin
   //If pHeaderObj <> Nil Then
   //   For Index := 0 To pHeaderObj^.FileCount - 1 Do
   //      If RecordChanged[Index] Then
   //      Begin
   //         Result := True;
   //         Exit;
   //      End;
   //Result := False;
   Result := ArchiveCRC <> MasterCRC;
End;
//------------------------------------------------------------

Procedure TArchiveEditor.CalcFieldCrc(p: Pointer; Len: Integer; Var Crc: u_long);
Begin
	Crc32_buf(p, Len, Crc);
End;
//-------------------------------------------------------------

Procedure TArchiveEditor.StartEdit;
Var
   Index: Integer;
   _FileName: String;
   _FileAttr,
   	_FileDate: Integer;
	ArcFile: TFileStream32;
Begin
	RollBack();
   If ArchiveFile = '' Then
   	Exit;

	FilesCount := 0;
   Count := 0;
   pHeaderObj := Nil;
   CompressComponent := Nil;
	Crc32List.Clear();
   //EncryptedList.Clear();

   ArcFile := TFileStream32.Create(fArchiveFile, fmOpenRead);
   If (ArcFile.Handle < 0) Then
      RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN)
   Else Begin
   	Try
         If (ArcType In Compress_ArcType) Then
         Begin
            Case ArcType Of
               atBH:	CompressComponent := TBlakHole.Create(Nil);
               atJAR: CompressComponent := TJar.Create(Nil);
               atLHA,
                  atLZH: CompressComponent := TLha.Create(Nil);
               atTAR: CompressComponent := TTar.Create(Nil);
               atZIP: CompressComponent := TZip.Create(Nil);
            Else
               Begin
               	CompressComponent := Nil;
                  RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
                  Exit;
               End;
            End;
         End Else Begin
            RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
            Exit;
         End;

         CompressComponent.TranslateOemChar := TranslateOemChar;

         Try
            HeadObj := TCompHeaderObj.Create();
            HeadObj.Init();
            pHeaderObj := @HeadObj;
            CompressComponent.ArcToList(ArcFile, pHeaderObj);
         Finally
            FilesCount := pHeaderObj^.FileCount;

            For Index := 0 To FilesCount - 1 Do
            Begin
   				_FileName := FileName[Index];
               _FileAttr := FileAttr[Index];
   				_FileDate := FileDate[Index];

               Crc32Val := CRC_MASK;
               //If fCaseSensitive Then
               //	CalcFieldCrc(PChar(_FileName), Length(_FileName), Crc32Val)
               //Else
               	CalcFieldCrc(PChar(UpperCase(_FileName)), Length(_FileName), Crc32Val);

               CalcFieldCrc(@_FileAttr, SizeOf(_FileAttr), Crc32Val);
               CalcFieldCrc(@_FileDate, SizeOf(_FileDate), Crc32Val);
					Crc32List.Add(Pointer(Crc32Val));

               ArchiveCRC := ArchiveCRC + Crc32Val;

               If Assigned(OnGetCompressedFileName) Then
               	fOnGetCompressedFileName(Self, _FileName);
            End;

            MasterCRC := ArchiveCRC;

         End;
      Finally
   		isInitialized := True;
      	ArcFile.Free();
      End;
   End;
End;
//------------------------------------------------------------

Procedure TArchiveEditor.RollBack;
Begin
	If (Not isInitialized) Then
   	Exit;

	FilesCount := 0;
   Count := 0;
	Crc32List.Clear();
   //EncryptedList.Clear();
   isInitialized := False;

   If pHeaderObj <> Nil Then
   Begin
      pHeaderObj^.DONE();
      pHeaderObj.Free();
      pHeaderObj := Nil;
   End;

   If CompressComponent <> Nil Then
   Begin
      CompressComponent.Free();
      CompressComponent := Nil;
   End;
End;
//------------------------------------------------------------

Procedure TArchiveEditor.CommitUpdates;
Var
   MemBuf: PChar;
	UnixDate,
   	Index,
   	TarCheckSum: Integer;
   _FileName: String;
   _FileAttr: Integer;
   _FileDate: Integer;
   BytesToSave: u_long;
   ArcFile: TFileStream32;
   OrigFileNameLen: Word;
   TempFile: TTempFileStream;
Begin
   If Cancel Or (Not isInitialized) Or (Not ArchiveChanged()) Then
      	Exit;

   Try
		isInitialized := False;
		FilesCount := 0;
   	Count := 0;

      TempFile :=
         TTempFileStream.Create(Self, TempDir, fArchiveFile, fmCreate);

      TempFile.CancelCallBackProc := @CompressComponent.fCancel;
      TempFile.ProgressCallBackProc := Nil;

      Try
         ArcFile := TFileStream32.Create(fArchiveFile, fmOpenRead);

         If (ArcFile.Handle < 0) Then
      		RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN)
         Else
            Try
               ArcFile.Position := fOffsetStart;
               //EncryptedList.Clear();
               For Index := 0 To pHeaderObj^.FileCount - 1 Do
               Begin
                  CompressComponent.pCBFI :=
                     pHeaderObj.FileLocationData(Index);

                  CompressComponent.pCBFI^.Status := hsCopy;
                  //CommentLen := CompressComponent.pCBFI^.FileCommentLen;

                  CompressComponent.HeadInfo :=
                     CompressComponent.GetHeaderInfo(
                        pHeaderObj^.Header[Index],
                        pCBFI{pCBFI^.Status});

                  _FileName := pHeaderObj.FileList.Strings[Index];
                  CompressComponent.Bytes_To_Go :=
                     CompressComponent.HeadInfo.pSize;

                  ArcFile.Position :=
                     CompressComponent.pCBFI^.Offset;

                  BytesToSave :=
                     CompressComponent.GetTotalRecordSize(
                        ArcFile, TempFile, pHeaderObj^.Header[Index]);

                  Case ArcType Of
                     atBH:
                        Begin
                           CompressComponent.BhHeader := TBh(pHeaderObj^.Header[Index]^);
                           OrigFileNameLen := CompressComponent.BhHeader.FileNameLen;

                           CompressComponent.BhHeader.FileNameLen := Length(_FileName);
                           CompressComponent.BhHeader.FileDate := FileDate[Index];
                           CompressComponent.BhHeader.ExternalAttr := FileAttr[Index];

                           TempFile.Write(CompressComponent.BhHeader, SizeOf(TBh));
                           ArcFile.Seek(SizeOf(TBh), soCurrent);
                           Dec(BytesToSave, SizeOf(TBh));

                           TempFile.Write(_FileName[1], Length(_FileName));
                           Dec(BytesToSave, OrigFileNameLen);
                           ArcFile.Seek(OrigFileNameLen, soCurrent);
                        End;

                     atLHA,
                        atLZH:
                        Begin
                           // zero entire header (not just LZHHdr_Size)
                           ZeroMemory(@CompressComponent.LzhHeader, SizeOf(TLzh));

                           CompressComponent.LzhHeader :=
                              TLzh(pHeaderObj^.Header[Index]^);

                           If CompressComponent.LzhHeader.HeadLen > 0 Then
                           Begin
                              OrigFileNameLen :=
                                 CompressComponent.LzhHeader.FileNameLen;

                              If CompressComponent.LzhHeader.CompressType = 100 Then // directory
                                 _FileName := ''
                              Else
                                 If CompressComponent.LzhHeader.CompressType = 100 Then // directory
                                    _FileName := ExtractFileName(_FileName);

                              If Length(_FileName) > 0 Then
                              Begin
                                 CompressComponent.LzhHeader.HeadLen :=
                                    CompressComponent.LzhHeader.HeadLen -
                                    OrigFileNameLen +
                                    Length(_FileName);

                                 Case CompressComponent.LzhHeader.level Of
                                    0, 1: CompressComponent.LzhHeader.FileNameLen :=
                                             Length(_FileName);

                                       2: Begin
                                             CompressComponent.LzhHeader.FileNameLen :=
                                                Length(_FileName) + 3;
                                          End;
                                 End;
                              End;

                              // if operating system was UNIX, convert date
                              // back to unix date format.
                              If (CompressComponent.LzhHeader.OS = LHA_UNIXOS) Then
                                 CompressComponent.LzhHeader.FileDate :=
                                    DosDateToUnix(FileDateToDateTime(FileDate[Index]))
                              Else
                                 CompressComponent.LzhHeader.FileDate := FileDate[Index];

                              If CompressComponent.LzhHeader.CompressType <> 100 Then // directory
                                 CompressComponent.LzhHeader.ExternalAttr :=
                                    FileAttr[Index];


                              Case CompressComponent.LzhHeader.level Of
                                 0, 1:	Begin
                                          TempFile.Write(CompressComponent.LzhHeader, LZHHdr_Size);
                                          ArcFile.Seek(LZHHdr_Size, soCurrent);
                                          Dec(BytesToSave, LZHHdr_Size);
                                       End;
                                    2: Begin
                                          TempFile.Write(CompressComponent.LzhHeader, LZHHdr_Size-1);
                                          Dec(BytesToSave, LZHHdr_Size-1);

                                          // move pointer past header
                                          ArcFile.Seek(LZHHdr_Size - 1, soCurrent);

                                          ArcFile.Read(
                                             CompressComponent.LzhHeader.Crc16,
                                             SizeOf(LzhHeader.Crc16));

                                          TempFile.Write(
                                             CompressComponent.LzhHeader.Crc16,
                                             SizeOf(LzhHeader.Crc16));

                                          ArcFile.Read(
                                             CompressComponent.LzhHeader.OS,
                                             SizeOf(LzhHeader.OS));

                                          TempFile.Write(
                                             CompressComponent.LzhHeader.OS,
                                             SizeOf(LzhHeader.OS));

                                          // skip the reading of the FileNameLen
                                          // field.  This value was previously
                                          // assigned and could have changed
                                          // if the compressed file-name was
                                          // edited.
                                          ArcFile.Seek(
                                             SizeOf(LzhHeader.FileNameLen),
                                             soCurrent);

                                          TempFile.Write(
                                             CompressComponent.LzhHeader.FileNameLen,
                                             SizeOf(LzhHeader.FileNameLen));

                                          ArcFile.Read(
                                             CompressComponent.LzhHeader.ExtHeaderSize,
                                             SizeOf(LzhHeader.ExtHeaderSize));

                                          TempFile.Write(
                                             CompressComponent.LzhHeader.ExtHeaderSize,
                                             SizeOf(LzhHeader.ExtHeaderSize));

                                          Dec(BytesToSave,
                                             SizeOf(LzhHeader.CRC16) +
                                             SizeOf(LzhHeader.OS) +
                                             SizeOf(LzhHeader.FileNameLen) +
                                             SizeOf(LzhHeader.ExtHeaderSize));

                                       End;
                              End;

                              If CompressComponent.LzhHeader.CompressType <> 100 Then // directory
                              Begin
                                 TempFile.Write(_FileName[1], Length(_FileName));
                                 ArcFile.Seek(OrigFileNameLen, soCurrent);
                                 Dec(BytesToSave, OrigFileNameLen);
                              End;
                           End;
                        End;

                     atTAR:
                        Begin
                           ZeroMemory(
                              @CompressComponent.TarHeader,
                              SizeOf(TarHeader));

                           CompressComponent.TarHeader :=
                              TTarHeader(pHeaderObj^.Header[Index]^);

                           // Copy _FileName to header
                           _FileName := DOSToUnixFilename(_FileName);
                           ZeroMemory(@CompressComponent.TarHeader.TarFileName,
                              SizeOf(TarHeader.TarFilename));

                           CopyMem(@_FileName[1],
                              @CompressComponent.TarHeader.TarFileName[0],
                              Length(_FileName));

                           // copy date to header
                           GetMem(MemBuf, 1024);
                           Try

                              UnixDate :=
                                 DosDateToUnix(FileDateToDateTime(FileDate[Index]));

                              IntToOctStr(UnixDate,
                                 SizeOf(CompressComponent.TarHeader.MTime),
                                 True, MemBuf);

                              CopyMem(@MemBuf[0],
                                 @CompressComponent.TarHeader.MTime[0],
                                 SizeOf(TarHeader.MTime));

                              TarCheckSum :=
                                 CalcCheckSum(CompressComponent.TarHeader);

                              IntToOctStr(TarCheckSum, SizeOf(TarHeader.ChkSum),
                                 True, MemBuf);

                              CopyMem(@MemBuf[0],
                                 @CompressComponent.TarHeader.ChkSum[0],
                                 SizeOf(TarHeader.ChkSum));
                           Finally
                              FreeMem(MemBuf, 1024);
                           End;

                           TempFile.Write(TarHeader, SizeOf(TTarHeader));
                           Dec(BytesToSave, SizeOf(TTarHeader));
                           //v6.3.1 rem'd
                           //ArcFile.Seek(SizeOf(TTarHeader), soCurrent);
                        End;

                     atZIP,
                        atJAR:
                        Begin

                           // Notes:
                           // 1. CompressComponent.LocalZipHeader is defined
                           // in the prior GetTotalRecordSize function call.
                           // 2. when debugging LocalZipHeader & CentralZipHeader
                           // values, use CompressComponent.LocalZipHeader (not
                           // just LocalZipHeader.
                           Dec(BytesToSave,
                              CompressComponent.LocalZipHeader.zc.FileNameLen);

                           CompressComponent.LocalZipHeader.zc.FileNameLen :=
                              Length(_FileName);

                           // The ExternalAttr field isn't present in the
                           // local header, only need to set the revised
                           // date.
                           CompressComponent.LocalZipHeader.zc.FileDate :=
                              FileDate[Index];

                           If pCBFI^.EncryptedHeader Then
                              EncodeHeader(
                                 @CompressComponent.LocalZipHeader,
                                 htLocal);

                           TempFile.Write(CompressComponent.LocalZipHeader,
                              SizeOf(TLocal));

                           Dec(BytesToSave, SizeOf(TLocal));

                           TempFile.Write(_FileName[1], Length(_FileName));
                           TCentral(pHeaderObj^.Header[Index]^).zc.FileNameLen :=
                              Length(_FileName);
                        End;
                  End;

                  //EncryptedList.Add(Pointer(Encrypted));
                  If BytesToSave > 0 Then
                     TempFile.CopyFrom(ArcFile, BytesToSave);

               End;
            Finally
               ArcFile.Free();
            End;
      Finally
         CompressComponent.Count := pHeaderObj^.FileCount;
         CompressComponent.doCleanUp(TempFile, pHeaderObj);
         TempFile.Free();
      End;
   Finally

      Try
      	// Generate the new MasterCRC value with revised
         // archive.
         FilesCount := pHeaderObj^.FileCount;
         ArchiveCrc := CRC_MASK;
         Try
            For Index := 0 To FilesCount - 1 Do
            Begin
               _FileName := FileName[Index];
               _FileAttr := FileAttr[Index];
               _FileDate := FileDate[Index];

               Crc32Val := CRC_MASK;
               //If fCaseSensitive Then
               //	CalcFieldCrc(PChar(_FileName), Length(_FileName), Crc32Val)
               //Else
                  CalcFieldCrc(PChar(UpperCase(_FileName)), Length(_FileName), Crc32Val);

               CalcFieldCrc(@_FileAttr, SizeOf(_FileAttr), Crc32Val);
               CalcFieldCrc(@_FileDate, SizeOf(_FileDate), Crc32Val);
               Crc32List.Add(Pointer(Crc32Val));

               ArchiveCRC := ArchiveCRC + Crc32Val;

               If Assigned(OnGetCompressedFileName) Then
                  fOnGetCompressedFileName(Self, _FileName);
            End;
         Finally
            MasterCRC := ArchiveCRC;
         End;
      Finally
   		RollBack();  // free-up allocated memory
      End;
   End;
End;
//------------------------------------------------------------

Function TArchiveEditor.RestoreRecord(Index: Integer): Boolean;
Var
   i: Integer;
   pWord: ^word;
   HeadSize: Word;
   Dir: AnsiString;
   pFilename: PChar;
   _FileName: String;
   Encrypted: Boolean;
	ArcFile: TFileStream32;

	Function GetFileName(Var _FileName: String; Len: Integer; Encrypted: Boolean): Boolean;
   Begin
      SetLength(_FileName, Len);
      If Len > 0 Then
      Begin
      	ZeroMemory(@_FileName[1], Len {+ 1});
      	Result := ArcFile.Read(_FileName[1], Len) = Len;
         If Result Then
         Begin
            If Encrypted Then
               DecodeFilename(@_FileName[1], Len);

            If Result Then
               _FileName := OemToCharFilter(_FileName, fTransOemChar);
      	End;
      End Else
      	Result := False;
   End;

Begin
	Result := False;

	If (Not isInitialized) Or (Not RecordChanged[Index]) Then
   	Exit;

   Try
      ArcFile := Nil;
      Try
         ArcFile := TFileStream32.Create(fArchiveFile, fmOpenRead);
         If (ArcFile.Handle < 0) Then
      		RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN)
         Else Begin

            Case ArcType Of
               atBH:
               	Begin
               		pCBFI := pHeaderObj.FileLocationData(Index);
               		ArcFile.Position := pCBFI^.Offset;
                     ZeroMemory(@BhHeader, SizeOf(TBh));
                     If ArcFile.Read(BhHeader, SizeOf(TBh)) = SizeOf(TBh) Then
                     Begin
                     	If BhHeader.SignAtr = BLAKHOLE_SIGNATURE Then
                        Begin
                        	If GetFileName(_FileName, BhHeader.FileNameLen, False) Then
                           Begin
                        		FileName[Index] := _FileName;
                           	FileAttr[Index] := BhHeader.ExternalAttr;
                           	FileDate[Index] := BhHeader.FileDate;

                              // The following line must appear after the setting
                              // of FileName[], FileAttr[], and FileDate[].  These
                              // functions do crc calculations on prior current
                              // values.  After CopyMem, all values are restored,
                              // so if it appears prior, the crc calculations will
                              // be
                     			CopyMem(@BhHeader, pHeaderObj^.Header[Index], SizeOf(TBh));
                           	Result := True;
                           End;
                        End Else Begin
                           // display error message here
                           Exit;
                        End;
                     End;
                  End;

               atLHA,
                  atLZH:
                  Begin
               		pCBFI := pHeaderObj.FileLocationData(Index);
               		ArcFile.Position := pCBFI^.Offset;
                     ZeroMemory(@LzhHeader, LZHHdr_Size);
                     If ArcFile.Read(LzhHeader, LZHHdr_Size) = LZHHdr_Size Then
                     Begin
                        GetMem(pFilename, 256);
                        Try
                           ZeroMemory(pFilename, 256);
                           Case LzhHeader.level Of
                              0, 1:                    // do nothing
                                 Begin
                                    ArcFile.Read(pFilename^, LzhHeader.FileNameLen);
                                    _FileName := StrPas(pFileName);
                                    ArcFile.Read(LzhHeader.CRC16, 5);
                                 End;
                              2:                       // adjust beginning offset
                                 Begin
                                    ArcFile.Seek(-1 , soCurrent);
                                    ArcFile.Read(LzhHeader.CRC16, SizeOf(LzhHeader.CRC16));
                                    ArcFile.Read(LzhHeader.OS, SizeOf(LzhHeader.OS));
                                    ArcFile.Read(LzhHeader.FileNameLen, SizeOf(LzhHeader.FileNameLen));
                                    ArcFile.Seek(2, soCurrent);

                                    If LzhHeader.FileNameLen > 3 Then
                                    	Dec(LzhHeader.FileNameLen, 3);

                                    ArcFile.Read(pFilename^, LzhHeader.FileNameLen);
                                    _FileName := StrPas(pFilename);
                                 End;
                           Else
                           End;

                           HeadSize := LzhHeader.ExtHeaderSize;
               				//While (LzhHeader.ExtHeaderSize > 0) And (LzhHeader.ExtHeaderSize < 256) Do
               				While (HeadSize > 0) And (HeadSize < 256) Do
                           Begin
                              //ArcFile.Read(pFileName^, LzhHeader.ExtHeaderSize);
                              ArcFile.Read(pFileName^, HeadSize);
                              Case Byte(pFileName^) Of
                                 0: ;
                                 1: ;
                                 2:
                                    Begin
                                       //Dir := StrPas(pFileName);
                                       //LzhHeader.ExtHeader := DecodeDir(Dir);
                                       //_FileName := AppendDirTail(LzhHeader.ExtHeader) + _FileName;
                                       Dir := DecodeDir(StrPas(pFileName));
                                       _FileName := AppendDirTail(Dir) + _FileName;
                                    End;
                                 $40: ;
                              End;

                              //pWord := @pFileName[LzhHeader.ExtHeaderSize - 2];
                              //LzhHeader.ExtHeaderSize := pWord^;
                              pWord := @pFileName[HeadSize - 2];
                              HeadSize := pWord^;
                           End;

                           If LzhHeader.level = 2 Then
                              //ArcFile.Read(LzhHeader.ExtHeaderSize, SizeOf(LzhHeader.ExtHeaderSize));
                              ArcFile.Read(HeadSize, SizeOf(LzhHeader.ExtHeaderSize));

                           FileName[Index] := _FileName;
                           FileAttr[Index] := LzhHeader.ExternalAttr;
                           //If LzhHeader.OS = UNIX
                           FileDate[Index] := LzhHeader.FileDate;

                           CopyMem(@LzhHeader, pHeaderObj^.Header[Index], SizeOf(TLzh));
                           Result := True;

                        Finally
                           FreeMem(pFilename, 256);
                        End;
                     End;

                  End;
               atTAR:
               	Begin
               		pCBFI := pHeaderObj.FileLocationData(Index);
               		ArcFile.Position := pCBFI^.Offset;
                     ZeroMemory(@TarHeader, SizeOf(TTarHeader));
                     If ArcFile.Read(TarHeader, SizeOf(TTarHeader)) = SizeOf(TTarHeader) Then
                     Begin
            				If ValidateTarHeader(@TarHeader) Then
                        Begin
                  			FileName[Index] := UnixToDosFilename(StrPas(@TarHeader.TarFilename));
									FileDate[Index] := DateTimeToFileDate(UnixDateToDos(OctStrToInt(TarHeader.MTime)));
                           FileAttr[Index] := 0; //OctStrToInt(TarHeader.Mode);

                           // The following line must appear after the setting
                           // of FileName[], FileAttr[], and FileDate[].  These
                           // functions do crc calculations on prior current
                           // values.  After CopyMem, all values are restored,
                           // so if it appears prior, the crc calculations will
                           // be
                           CopyMem(@TarHeader, pHeaderObj^.Header[Index], SizeOf(TTarHeader));
                           Result := True;
                        End Else Begin
                           // display error message here
                           Exit;
                        End;
                     End;
                  End;
               atJAR,
               	atZIP:
                  If htEnding In HeaderTypeState Then
                  Begin
                     //HeaderTypeState  // rem'd for possible future usage
                     ArcFile.Position := EndZipHeader.CentralDirOffset;
                     For i := 0 To Index - 1 Do
                     Begin
                        If ArcFile.Read(CentralZipHeader, SizeOf(TCentral)) = SizeOf(TCentral) Then
                        Begin
                           If VerSig(CentralZipHeader.SignAtr, htCentral,
                              Encrypted) = htCentral Then
                           Begin
                              If Encrypted Then
                                 DecodeHeader(@CentralZipHeader, htCentral);

                              ArcFile.Seek(CentralZipHeader.ZC.FileNameLen, soCurrent);
                              ArcFile.Seek(CentralZipHeader.ZC.ExtraFieldLen, soCurrent);
                              ArcFile.Seek(CentralZipHeader.CommentLen, soCurrent);
                           End;
                        End Else Begin
                           // display error message here
                           Exit;
                        End;
                     End;

                     // now read & reset the requested index info
                     If ArcFile.Read(CentralZipHeader, SizeOf(TCentral)) = SizeOf(TCentral) Then
                     Begin
                        If VerSig(CentralZipHeader.SignAtr, htCentral,
                           Encrypted) = htCentral Then
                        Begin
                           If Encrypted Then
                              DecodeHeader(@CentralZipHeader, htCentral);

                           If GetFileName(_FileName, CentralZipHeader.ZC.FileNameLen, Encrypted) Then
                           Begin
                              FileName[Index] := _FileName;
                           	FileAttr[Index] := CentralZipHeader.ExternalAttr;
                           	FileDate[Index] := CentralZipHeader.zc.FileDate;

                              // The following line must appear after the setting
                              // of FileName[], FileAttr[], and FileDate[].  These
                              // functions do crc calculations on prior current
                              // values.  After CopyMem, all values are restored,
                              // so if it appears prior, the crc calculations will
                              // be
                           	CopyMem(@CentralZipHeader, pHeaderObj^.Header[Index], SizeOf(TCentral));
                           	Result := True;
                        	End;
                        End;
                     End Else Begin
                        // display error message here
                        Exit;
                     End;
                  End Else
                     ; // display error message here
            Else

            End;
         End;
      Finally
      	ArcFile.Free();
      End;
   Except
   	Result := False;
   End;
End;
//-------------------------------------------------------------

Function TArchiveEditor.BatchRenameFiles(OldSpec, NewSpec, Path: String): Integer;

	Function ChangeFileNameProc(str, SearchCard, WildCard: String; Var NameChanged: Boolean): String;
   Var
      r: String;
   	i, j: Word;
      ExtPos: Byte;
   Begin
      j := 1;
      r := '';
      ExtPos := 0;
      NameChanged := False;

      If (Str = '') Or (SearchCard = WildCard) Then
      Begin
      	Result := Str;
      	Exit;
      End;

      If WildCard = '' Then
      	WildCard := Str;

      For i := 1 To Length(Str) Do
      	If Str[i] = '.' Then
         	ExtPos := i;

      i := 0;
      While i < Length(WildCard) Do
      Begin
      	i := i + 1;
         Case WildCard[i] Of
         	'*':
            	Begin
               	i := i + 1;
                  NameChanged := True;

                  While j < Length(Str) Do
                  Begin
                  	r := r + Str[j];
                  	If j = ExtPos - 1  Then
                     Begin
                     	j := j + 1;
                     	If (WildCard[i] <> '.') Then
                        Begin
                        	While i < Length(WildCard) Do
                           Begin
                        		r := r + WildCard[i];
                              If WildCard[i + 1] = '.' Then
                                 Break
                              Else
                              	i := i + 1;
                           End;
                        End Else
                        	i := i - 1;

                     	Break;
                     End;
                     J := j + 1
                  End;
               End;
            '?':
            	Begin
               	NameChanged := True;
               	r := r + Str[j];
                  j := j + 1;
               End;
         Else
         	Begin
            	r := r + WildCard[i];
               j := j + 1;
            End;
         End;
      End;
   	Result := r;
   End;

Var
   Index: Integer;
   _FileName,
   	NewFileName: String;
   SkipNameChange,
   	NameChanged: Boolean;
Begin
	Result := 0;
	If (Not FileExists(ArchiveFile)) Or (OldSpec = '') Or (NewSpec = '') Then
   	Exit;

   If FilesCount = 0 Then Exit;
   If Not RecurseDirs Then
   	OldSpec := AppendDirTail(Path) + ExtractFileName(OldSpec);

   FileSpec.Clear();
   FileSpec.Add(OldSpec);
   ExcludeSpec.Clear();

   For Index := 0 To FilesCount - 1 {pHeaderObj^.FileCount - 1} Do
   Begin
   	_FileName := FileName[Index];

      If CheckWildCard2(_FileName, FileSpec, Nil, RecurseDirs) Then
      Begin
         NewFileName := ChangeFileNameProc(_FileName, OldSpec, NewSpec, NameChanged);
         //If Not NameChanged Then
         //	ShowMessage('Name unchanged');
         //If NameChanged Then
         //Begin
            SkipNameChange := False;
            If Assigned(fOnBatchSkipFile) Then
               fOnBatchSkipFile(Self, NewFileName{_FileName}, Index, SkipNameChange);

            If Not SkipNameChange Then
            	FileName[Index] := NewFileName;
         //End;
      End;
   End;
End;
//-------------------------------------------------------------


End.
