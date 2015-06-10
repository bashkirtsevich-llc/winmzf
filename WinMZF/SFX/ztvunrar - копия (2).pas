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
Unit ztvUnRar;


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
   ztvGbls,
   ztvHeaders,
   ztvStreams,
   ztvFileIo;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TRarFileData = Packed Record
      FileNameLen: Integer;
   	FileName: pWideChar;
   End;

   pRarFileData = ^TRarFileData;
   pRarHugeFile = ^TRarHugeFile;

   THighFileSizes = Class(TObject)
   Public
      MemSize: Byte;
      Count: Integer;
      FileNameData: TList;
      DataLocation: TList;
      Procedure INIT;
      Procedure DONE;
      Procedure CLEAR_LIST;
		Function AddItem(FN: AnsiString; Len: Integer; InRec: TRarHugeFile): Boolean;
      Function FileNameDataLocation(Index: Integer): pRarFileData;
      Function FileDataLocation(Index: Integer): pRarHugeFile;
   End;

   TRAROpenArchiveData = Packed Record
      ArcName: PChar;
      OpenMode: Cardinal;
      OpenResult: Cardinal;
      CmtBuf: PChar;
      CmtBufSize: Cardinal;
      CmtSize: Cardinal;
      CmtState: Cardinal;
   End;

 	TRAROpenArchiveDataEx = Packed Record
      ArcName: PChar;
      ArcNameW: PWideChar;
      OpenMode: Cardinal;
      OpenResult: Cardinal;
      CmtBuf: PChar;
      CmtBufSize: Cardinal;
      CmtSize: Cardinal;
      CmtState: Cardinal;
      Flags: Cardinal;
      Reserved: Array[0..31] Of Cardinal;
   End;

   TUnRar = Class(TUnBASE)
   Private
   	HighFileSizes: THighFileSizes;
	   RarHuge: TRarHugeFile;
      pRarHuge: pRarHugeFile;
      RarFileDataPtr: pRarFileData;
      fNextVolSize: Int64;
      fPassword: String;
      ModoOp: Byte;
      RarFilePos: Int64;
      RarTotalSize: Int64;
      Rar1Header: TRar1;
		Function GetFirstHeader(Var Strm: TFileStream32): Boolean;
		Function GetNextHeader(Var Strm: TFileStream32): Boolean;
      Function LocalOnEnd(FN: String; PFcode: Integer): Boolean;
      Function OutProcessFileError(Error: Integer): Integer;
      Procedure GetTotalSize(ArcName: String);
      Procedure ExtractFile(ArcName: PAnsiChar; mode: Integer);
   Protected
   	CurrentCount: Integer;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure ExtractIT(Var Infile: TStream32; Outfile: TStream32); Override;
		Function RARGetDllVersion: Integer;
   Published
      Property ArcType;
      Property ConfirmOverwrites;
      Property CreateStoredDirs;
      Property ExtractDir;
      Property FileSpec;
      Property OverwriteMode;
      Property Password: String Read fPassword Write fPassword;
      //Property PasswordAttempts;  // removed in version 4.5.6... no longer valid
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

Const
   UNRARDLL = 'unrar3.dll';


{$IFDEF DYNLOADUNRARDLL}
Type
   TRAROpenArchive   = Function(Var ArchiveData: TRAROpenArchiveData): THandle; Stdcall;
	TRarOpenArchiveEx = Function(Var ArchiveData: TRAROpenArchiveDataEx): THandle; Stdcall;
   TRARCloseArchive  = Function(hArcData: THandle): Integer; Stdcall;
   TRARReadHeader    = Function(hArcData: THandle; Var HeaderData: TRarHeadData): Integer; Stdcall;
   TRARReadHeaderEx  = Function(hArcData: THandle; Var HeaderData: TRarHeadDataEx): Integer; Stdcall;
   TRARProcessFile   = Function(hArcData: THandle; Operation: Integer; DestPath, DestName: PChar): Integer; Stdcall;
   TRARProcessFileW  = Function(hArcData: THandle; Operation: Integer; DestPath, DestName: PWideChar): Integer; Stdcall;
  	TUnrarCallback    = Function(Msg: UINT; UserData, P1, P2: Integer): Integer; Stdcall;
	TRARSetCallback   = Procedure(hArcData: THandle; UnrarCallback: TUnrarCallback; UserData: Integer); Stdcall;


Var
   RAROpenArchive: TRAROpenArchive;
   RAROpenArchiveEx: TRAROpenArchiveEx;
   RARCloseArchive: TRARCloseArchive;
   RARReadHeader: TRARReadHeader;
   RARReadHeaderEx: TRARReadHeaderEx;
   RARProcessFile: TRARProcessFile;
   RARProcessFileW: TRARProcessFileW;
   RarSetCallback: TRarSetCallback;

   IsUNRARDLLAvailable: Boolean = False; //v4.0  (* TRUE if UNRAR.DLL available. *)

{$ELSE}

Type
  	TUnrarCallback = Function(Msg: UINT; UserData, P1, P2: Integer): Integer; stdcall;

// Returns: -1 = DLL not found; 0 = old ver. (C-style callbacks); >0 = new ver.
Function RARGetDllVersion: Integer;
Function RAROpenArchive(Var ArchiveData: TRAROpenArchiveData): THandle;
	Stdcall; External UNRARDLL;
Function RAROpenArchiveEx(Var ArchiveData: TRAROpenArchiveDataEx): THandle;
	Stdcall; External UNRARDLL;
Function RARCloseArchive(hArcData: THandle): Integer;
	Stdcall; External UNRARDLL;
Function RARReadHeader(hArcData: THandle; Var HeaderData: TRarHeadData): Integer;
	Stdcall; External UNRARDLL;
Function RARReadHeaderEx(hArcData: THandle; Var HeaderData: TRarHeadDataEx): Integer;
	Stdcall; External UNRARDLL;
Function RARProcessFile(hArcData: THandle; Operation: Integer; DestPath, DestName: PChar): Integer;
	Stdcall; External UNRARDLL;
Procedure RARSetCallback(hArcData: THandle; UnrarCallback: TUnrarCallback; UserData:Integer);
  	Stdcall; External UNRARDLL;

Var
  IsUNRARDLLAvailable: Boolean = True; //v4.0 (* UNRAR3.DLL assumed available. *);

{$ENDIF}


Implementation


Uses
{$IFDEF DYNLOADUNRARDLL}
   ztvLoadLib,
{$ENDIF}
   Err_Msgs;



Const
   ERAR_END_ARCHIVE = 10;
   ERAR_NO_MEMORY = 11;
   ERAR_BAD_DATA = 12;
   ERAR_BAD_ARCHIVE = 13;
   ERAR_UNKNOWN_FORMAT = 14;
   ERAR_EOPEN = 15;
   ERAR_ECREATE = 16;
   ERAR_ECLOSE = 17;
   ERAR_EREAD = 18;
   ERAR_EWRITE = 19;
   ERAR_SMALL_BUF = 20;
   ERAR_BROKEN_INDEX = 21;
   ERAR_INVALID_PASSWRD = 22;

   RAR_OM_LIST = 0;
   RAR_OM_EXTRACT = 1;

   RAR_SKIP = 0;
   RAR_TEST = 1;
   RAR_EXTRACT = 2;

Const
   CEXTRACT = 0;
   CTEST = 1;
   CPRINT = 2;
   CREAD = 3;

Const
  UCM_CHANGEVOLUME =  0;
  UCM_PROCESSDATA = 1;
  UCM_NEEDPASSWORD = 2;


Var
   UnRARDLLHandle: THandle;             //v4.0  (* Handle for UNRAR.DLL *)
   Me: TUnRar;

//-------------------------------------------------------------

Procedure THighFileSizes.INIT;
Begin
   MemSize := SizeOf(TRarHugeFile);
   FileNameData := TList.Create();
   DataLocation := TList.Create();      //!! Create-Check: TRY/FINALLY
   CLEAR_LIST();
End;
//-------------------------------------------------------------

Procedure THighFileSizes.CLEAR_LIST;
Var
   i: Integer;
Begin
   For i := 0 To DataLocation.Count - 1 Do
      Dispose(FileDataLocation(i));

   For i := 0 To FileNameData.Count - 1 Do
      Dispose(FileNameDataLocation(i));

   DataLocation.Clear();
   FileNameData.Clear();

   Count := 0;
End;
//-------------------------------------------------------------

Function THighFileSizes.AddItem(FN: AnsiString; Len: Integer; InRec: TRarHugeFile): Boolean;
Var
	DataLocationPtr,
   	FileDataPtr: Pointer;
Begin
   Result := True;
   Try
      GetMem(DataLocationPtr, MemSize);
      CopyMem(@InRec, DataLocationPtr, MemSize);
      { Integer := } DataLocation.Add(DataLocationPtr);

      If Len > 0 Then
      Begin
         GetMem(FileDataPtr, SizeOf(Len) + (Len));
         CopyMem(@Len, FileDataPtr, SizeOf(Len));
         CopyMem(@FN[1], Pointer(Ptr2Int(FileDataPtr) + SizeOf(Len)), Len);
         FileNameData.Add(FileDataPtr);
      End Else
         FileNameData.Add(Nil);

      Inc(Count);
   Except
      Result := False;
   End;
End;
//-------------------------------------------------------------

Function THighFileSizes.FileNameDataLocation(Index: Integer): pRarFileData;
Begin
   Result := pRarFileData(FileNameData[Index]);
End;
//-------------------------------------------------------------

Function THighFileSizes.FileDataLocation(Index: Integer): pRarHugeFile;
Begin
   Result := pRarHugeFile(DataLocation[Index]);
End;
//-------------------------------------------------------------

Procedure THighFileSizes.DONE;
Begin
   CLEAR_LIST();
   FileNameData.Free();
   DataLocation.Free();
End;
//-------------------------------------------------------------

Constructor TUnRar.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
	pRarHuge := @RarHuge;
End;
//-------------------------------------------------------------

Destructor TUnRar.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TUnRar.OutProcessFileError(Error: Integer): Integer;
Begin
   Case Error Of
      ERAR_END_ARCHIVE: Result := E_NOEOF;
      ERAR_NO_MEMORY: Result := E_MEMERR;
      ERAR_EOPEN: Result := E_FOPEN;
      ERAR_BAD_ARCHIVE: Result := E_INVALIDARC;
      ERAR_BAD_DATA: Result := E_BADBLOCK;
      ERAR_UNKNOWN_FORMAT: Result := E_INVALIDARC;
      ERAR_ECREATE: Result := E_FWRITE;
      ERAR_ECLOSE: Result := E_FCLOSE;
      ERAR_EREAD: Result := E_FREAD;
      ERAR_EWRITE: Result := E_FWRITE;
      ERAR_BROKEN_INDEX: Result := E_BROKENINDEX;
      ERAR_INVALID_PASSWRD: Result := M_INVALIDPW;
   Else
      Result := 0;                          //satisfy compiler
   End;
End;
//-------------------------------------------------------------

// Returns:
//  -1 = DLL not found; 0 = old ver. (C-style callbacks); >0 = new ver.
Function TUnRar.RARGetDllVersion: Integer;
Type
  TRARGetDllVersion = Function: integer; Stdcall;
Var
  	f: TRARGetDllVersion;
Begin
  	If UnRARDLLHandle = 0 Then
  	begin
  		Result:= -1;
  		Exit;
	end;
  	f := GetProcAddress(UnRARDLLHandle, 'RARGetDllVersion');
  	If @f = Nil Then
   	Result := 0
   Else
   	Result:= f;
End;
//-------------------------------------------------------------

Procedure TUnRar.GetTotalSize(ArcName: String);
Var
   Strm: TFileStream32;
   FN: String;
   i, Attr: Integer;
   EmptyDirList: TStrings;
Begin

   fTotalUnpackedSize := 0;
   fTotalPackedSize := 0;
   CurrentCount := -1;
   HighFileSizes.CLEAR_LIST();

   Strm :=
      TFileStream32.Create(ArcName, fmOpenRead Or fmShareDenyWrite);

   If (Strm.Handle > -1) Then
   Begin

      FN := fFileName;
   	EmptyDirList := TStringList.Create();

      Try
         If GetFirstHeader(Strm) Then
         Begin
            Repeat

            	If ((RarHeader.HeadFlags And SKIP_IF_UNKNOWN) > 0) Or
                  (RarHeader.HeadType <> FILE_HEAD) Then
                  Continue;


            	If RarHeader.HeadFlags And LHD_UNICODE = 0 Then
            		HighFileSizes.AddItem('', 0, RarHuge)
               Else
            		HighFileSizes.AddItem(ActualFileName, RarHeader.FileNameLen, RarHuge);

               If RarHeader.HostOS = word(ord(osUnix)) Then
                  If (RarHeader.ExternalAttr And $4000 = 0) Then (* Unix Dir attr *)
                     Attr := 32
                  Else
                     Attr := FILE_ATTRIBUTE_DIRECTORY
               Else
                  Attr := RarHeader.ExternalAttr;

               If (Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
               Begin

            		If RarHeader.HeadFlags And LHD_UNICODE = 0 Then
                  	If WriteToFile And CreateStoredDirs Then
                     	EmptyDirList.Add(fFileName);

               End
               Else
               Begin

                  //If WriteToFile And CreateStoredDirs Then
                  //Begin
                  //   i := EmptyDirList.IndexOf(ExtractFileDir(fFileName));
                  //   If i > -1 Then
                  //      EmptyDirList.Delete(i);
                  //End;

                  If CheckWildCard2(ActualFileName, FileSpec, ExcludeSpec, RecurseDirs) Then
               	//IF (HeadFlags AND LHD_SPLIT_BEFORE) = 0 THEN
                  Begin
                     If (RarHeader.HeadFlags And LHD_SPLIT_BEFORE) = 0 Then
                     Begin
                        inc(FilesToExtract);
                        fNextVolSize := fNextVolSize +
            					((Int64(RarHuge.HighUnpackedSize) Shl 32) Or RarHeader.UnpackedSize);

                        fTotalPackedSize := fTotalPackedSize +
            					((Int64(RarHuge.HighPackedSize) Shl 32) Or RarHeader.PackedSize);

                        fTotalUnpackedSize := fTotalUnpackedSize +
            					((Int64(RarHuge.HighUnpackedSize) Shl 32) Or RarHeader.UnpackedSize);

                        If Cardinal(RarHeader.FileDate) > Cardinal(fMaxAge) Then
                           fMaxAge := RarHeader.FileDate;
                     End
                     Else
                        RarHeaderDataEx.CRC32 := RarHeader.CRC32;
                  End;

               End;

            Until Not GetNextHeader(Strm);

            If WriteToFile And CreateStoredDirs Then
               For i := 0 To EmptyDirList.Count - 1 Do
                  CreateDirEx({SlashSep(ExtractDir,} EmptyDirList[i]);

         End;
      Finally
         EmptyDirList.Free();
         Strm.Free();
         fFileName := FN;
      End;

   End
   Else
      RaiseError(E_RAISE, fVolumeName, '', '0', E_FOPEN)

End;
//-------------------------------------------------------------

Function TUnRar.LocalOnEnd(FN: String; PFcode: Integer): Boolean;
Begin
   Result := (RarHeaderDataEx.CRC32 = Crc32Val) Or (RarHeaderDataEx.CRC32 Xor CRC_MASK = Crc32Val);
   If Assigned(OnEnd) Then
   Begin
      If WriteToFile Then
         OnEnd(Self, FileName, Result)
      Else
         OnEnd(Self, FN, Result);
   End
   Else
      If Not Result Then
         RaiseErrorStr(FileName, '', '0', E_CRCERROR);
End;
//-------------------------------------------------------------

Function TUnRar.GetFirstHeader(Var Strm: TFileStream32): Boolean;
Var
   BytesRead: DWord;
   FN: Array[0..6] Of Char;
Begin
   RarFilePos := fOffsetStart;
   Strm.Seek(RarFilePos, soBeginning);
   BytesRead := Strm.Read(FN[1], 7);

   RarFilePos := RarFilePos + BytesRead;
   BytesRead := Strm.Read(Rar1Header, 13);
   RarFilePos := RarFilePos + BytesRead;

   If BytesRead <> 13 Then
   Begin
   	Result := False;
      RaiseErrorStr(fVolumeName, '', '0', E_FREAD);
	End Else
   	Result := GetNextHeader(Strm);
End;
//-------------------------------------------------------------

Function TUnRar.GetNextHeader(Var Strm: TFileStream32): Boolean;
Var
   FilePos: Int64;
Begin

   Result := False;
   Try
      With RarHeader Do
      Begin
         ZeroMemory(@RarHeader, SizeOf(RarHeader));

         Strm.Seek(RarFilePos, soBeginning);
         {BytesRead :=} Strm.Read(RarHeader, SizeOf(RarHeader));

         If HeadFlags And LHD_LARGE > 0 Then
            {BytesRead :=} Strm.Read(RarHuge, SizeOf(TRarHugeFile))
         Else
            ZeroMemory(@RarHuge, SizeOf(TRarHugeFile));


         If (HeadType = COMM_HEAD) Then	// skip comment header
         Begin
            RarFilePos :=
               RarFilePos +
               HeadSize;

            Result := True;
         End Else
            If ((HeadFlags And LONG_BLOCK) <> LONG_BLOCK) Or
               (HeadSize = 0) Or Cancel Then

               RarFilePos := fLOF
            Else
               If ((HeadFlags And SKIP_IF_UNKNOWN) > 0) Or
                  (HeadType <> FILE_HEAD) Then
               Begin
                  (* ---------------- *)
                  (* skip this header *)
                  (* ---------------- *)
                  RarFilePos :=
                     RarFilePos +
                     HeadSize +
            			((Int64(RarHuge.HighPackedSize) Shl 32) Or PackedSize);

                  Result := HeadSize > 0;
               End
               Else
               Begin

                  FilePos := Strm.Position;
               	ActualFileName :=
                  	ReadFilename_DefinedLen(Strm, FilePos, FileNameLen);

                  If RarHeader.HeadFlags And LHD_UNICODE > 0 Then
                  	fFileName := DecodeRarFileName(ActualFileName, FileNameLen)
                  Else
            			ActualFileName := OemToCharFilter(ActualFilename, fTransOemChar);

                  //BytesRead := Length(ActualFileName);
                  RarFilePos :=
                  	RarFilePos + HeadSize +
                     ((Int64(RarHuge.HighPackedSize) Shl 32) Or PackedSize);

                  Result := True;
               End;
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Function CallbackProc(msg: UINT; UserData, DataPtr, Size: integer): Integer; Stdcall;
Var
   Outfile: TStream32;
   FN: String;
   ArcName: PChar;
   EventResult: Boolean;
Begin
  	Result := 0;
   With Me Do
   Begin
      Case msg Of
         UCM_CHANGEVOLUME:
            Begin
            	ArcName := PChar(DataPtr);
               FN := StrPas(ArcName);

               // if the OnNextVolume event is not assigned,
               // it will be handled in GetNextVolume
               If GetNextVolume(FN, fVolNum) Then
               Begin
                  fVolumeName := FN;
                  StrPCopy(ArcName, FN)
               End Else
                  Result := -1;

               If Result = 0 Then
               Begin
                  fVolumeName := StrPas(ArcName);
						GetTotalSize(StrPas(ArcName));
               End Else
                  DoProgress(100, 100);
            End;

  			UCM_NEEDPASSWORD:
         	Begin
					If Assigned(OnGetPassword) Then
               Begin
               	If (fPassword <> '') Then
                  Begin
                     // include the null terminating char (+ 1)
               		Move(fPassword[1], PChar(DataPtr)^, Length(fPassword) + 1);
                     Exit;
                  End;

               	Try
                     ActualFilename := StrPas(@RarHeaderDataEx.FileName);
                     ActualFilename := OemToCharFilter(ActualFilename, fTransOemChar);
                     FileName := ActualFilename;

                     If fPassword = '' Then
                     Begin
                        EventResult := True;
                        OnGetPassword(Me, FileName, fPassword, EventResult);
                        If Not EventResult Then Exit;
                     End;

                     // include the null terminating char (+ 1)
               		Move(fPassword[1], PChar(DataPtr)^, Length(fPassword) + 1);
                  Except
                  End;
               End Else
                  RaiseErrorStr(FileName, '', '0', OutProcessFileError(ERAR_INVALID_PASSWRD));
            End;

         UCM_PROCESSDATA:
            Begin
               If (Not Cancel) Then
               Begin

                  If ProgressPosition <= 0 Then
                  Begin
                     DoProgress(0, 0);
                     RarTotalSize := fNextVolSize;
                     ProgressPosition := RarTotalSize;
                  End;

                  If ModoOp <> RAR_SKIP Then
                  Begin

                     If (WriteMethod = faFile) {Or (WriteMethod = faFileStream)} Or
                        	(WriteMethod = faAppend) Then
                     	Crc32_buf(PChar(DataPtr), size, Crc32Val);

                     If Cancel Then
                        Result := 0
                     Else
                     Begin

                        If fNextVolSize > 0 Then
                        Begin
                        	Bytes_To_Go := Bytes_To_Go - size;
                           fNextVolSize := fNextVolSize - size;
                           ProgressPosition := ProgressPosition - size;
                           doBranchProgress(
                           	((Int64(pRarHuge^.HighUnpackedSize) Shl 32) Or RarHeaderDataEx.UnpackedSize) - Bytes_To_Go,
                              ((Int64(pRarHuge^.HighUnpackedSize) Shl 32) Or RarHeaderDataEx.UnpackedSize),
                              RarTotalSize);
                        End;

                        If (Not WriteToFile) Then
                           ExtractWriteBlock(Outfile, PChar(DataPtr)^, False,
                           	32, size, dtData);

                        Result := 0;
                     End;
                  End;
                  //Else
                  //Begin
                  //   Result := 0;
                  //   ProgressPosition := ProgressPosition - size;
                  //   doBranchProgress(0, 0, RarTotalSize);
                  //End;
               End Else
                  Result := -1;

            End;
      End {case};
   End;
End;
//-------------------------------------------------------------

Procedure TUnRar.ExtractFile(ArcName: PAnsiChar; mode: Integer);
Var
   hArcHandle: THandle;
   //Dir: String;

	Function ProcessOp(op: Byte): Integer;
   Begin
   	Case op Of
   		RAR_SKIP:
         	// if RAR_TEST is not used here, split-volume / multi-volume
            // archives do not active the OnNextVolume event.
         	Result :=
            	RARProcessFile(hArcHandle, {RAR_SKIP}RAR_TEST, Nil, Nil);

   		RAR_TEST:
         	Result :=
            	RARProcessFile(hArcHandle, RAR_TEST, Nil, Nil);

   		RAR_EXTRACT:
         	Begin
               If SetFileAttributes(PChar(fFileName), FILE_ATTRIBUTE_NORMAL) Then
               	DeleteFile( fFilename );

               SetCurrentDir(ExtractDir);

            	If RarHeaderDataEx.HeadFlags And LHD_UNICODE > 0 Then
               Begin
                  //Dir := ExtractFileDir(fFileName);
                  //If Not _DirectoryExists(Dir) Then
                  //   CreateDirEx(Dir);
                  //SetCurrentDir(Dir);


                  // the unrar.dll api RARProcessFile does not support
                  // passing of double byte char set filenames via the
                  // DestPath & DestName parameters.  Rem'd until unrar.dll
                  // api supports pwidechar's for these two parameters. See
                  // arabic.rar for example.
                  //Case UseStoredDirs Of
                  //	True:
                  //      Result :=
                  //         RARProcessFile(
                  //            hArcHandle,
                  //            RAR_EXTRACT,
                  //            PChar(ExtractDir),
                  //            @RarHeaderDataEx.FileName
                  //            );
                  //	False:

//                  	GetOemCP	//GetKBCodePage
//                     GetACP
//                     GetCPInfo
//                     GetLocaleInfo
//                     SetLocaleInfo
//                     CodePage
//    SetFileApisToOEM();


            					//fFileName := OemToCharFilter(ActualFilename, True);
            					//fFileName := OemToCharFilter(fFileName, True);

            					//fFileName := CharToOemFilter(ActualFileName, True);
            					fFileName := CharToOemFilter(fFileName, True);
            					//fFileName := OemToCharFilter(fFileName, True);

                           Result :=
                              RARProcessFile(
                                 hArcHandle,
                                 RAR_EXTRACT,
                        			Nil,
                        			//PChar(ExtractFileName(ActualFileName))
                                 //PChar(@RarHeaderDataEx.FileName)
                                 //PChar(ExtractFileName(fFileName))
                                 //PChar(ExtractFilename(fFileName))
                                 //PChar(ActualFileName)
                                 //PWideChar(@RarHeaderDataEx.FileNameW)
                                 Nil
                                 );
                  //End;

               End Else Begin
						//fFileName := CharToOemFilter(fFileName, True);
               	//fFileName := OemToCharFilter(fFileName, True);

                  Result :=
                     RARProcessFile(
                     	hArcHandle,
                        RAR_EXTRACT,
                        Nil,
                        PChar(fFileName)
                        //PChar(ExtractFileName(fFileName))
                        //PChar(WideCharToString(PWideChar(fFileName)))
                        );

               End;
            End;
      Else
      	Result := 12;   // bad code
      End;

      If (Result <> 0) Then
      	RaiseErrorStr(ActualFilename, '', '0', OutProcessFileError(Result));

   End;

Var
   OutFile: TStream32;
      Attr,
   	RHCode,
   	PFcode: Integer;
  	CmtBuf: Array[1..16384] Of Char;
   OpenArchiveDataEx: TRAROpenArchiveDataEx;
   TranslateOEM: Boolean;
   pFileName: PWideChar;
Begin
   Try
      fNextVolSize := 0;
      ProgressPosition := 0;
      Count := 0;
      GetTotalSize(StrPas(ArcName));

      ZeroMemory(@OpenArchiveDataEx, SizeOf(TRarOpenArchiveDataEx));

      GetMem(pFileName, 2048);
      Try
      	OpenArchiveDataEx.ArcNameW :=
      		StringToWideChar(StrPas(ArcName), pFileName, 2048);

         OpenArchiveDataEx.CmtBuf := @CmtBuf[1];
         OpenArchiveDataEx.CmtBufSize := SizeOf( CmtBuf );
         OpenArchiveDataEx.OpenMode := RAR_OM_EXTRACT;

         hArcHandle := RAROpenArchiveEx(OpenArchiveDataEx);
         If OpenArchiveDataEx.OpenResult <> 0 Then
            RaiseError(E_RAISE, ArcName, '', '0',
               OutProcessFileError(OpenArchiveDataEx.OpenResult));

      Finally
      	FreeMem(pFileName);
      End;

      (* Set the processdata and changevol procedures *)
    	RARSetCallback(hArcHandle, CallbackProc, Ptr2Int(@Mode));

   	RarHeaderDataEx.CmtBuf := @CmtBuf[1];
      RarHeaderDataEx.CmtBufSize := SizeOf( CmtBuf );

     	(* Show the archive comment *)
   	//If OpenArchiveDataEx.CmtState = 1 THEN
     	//	ShowComment(@CmtBuf[1]);

      ZeroMemory(@RarHeaderDataEx, SizeOf(RarHeaderDataEx));
      Try
      	RHCode := RARReadHeaderEx(hArcHandle, RarHeaderDataEx);
         //If (RHCode = ERAR_BAD_DATA) And (RarHeaderData.HostOS <> word(ord(osUnix))) Then
         If (RHCode <> 0) Then
         Begin
         	RaiseErrorStr(PChar(ArcName), '', '0', E_BADHEADR);
         	Exit;
         End;

         While (RHCode = 0) Do
         Begin
         	If Cancel Then Break;
            Inc(CurrentCount);
            pRarHuge := HighFileSizes.FileDataLocation(CurrentCount);

           	Bytes_To_Go :=
					((Int64(pRarHuge^.HighUnpackedSize) Shl 32) Or RarHeaderDataEx.UnpackedSize);

            ModoOP := RAR_SKIP;	// default

            If RarHeaderDataEx.HeadFlags And LHD_UNICODE = 0 Then
            Begin
               ActualFilename := TrimRight(RarHeaderDataEx.FileName);
               ActualFilename := OemToCharFilter(ActualFilename, fTransOemChar);

               If WriteToFile() Then
                  FileName := ActualFilename  	// format filename
               Else
                  fFileName := ActualFilename;	// non-format filename
            End Else Begin
               RarFileDataPtr := HighFileSizes.FileNameDataLocation(CurrentCount);

               SetLength(ActualFileName, RarFileDataPtr^.FileNameLen);
               Move(RarFileDataPtr^.FileName, ActualFileName[1], RarFileDataPtr^.FileNameLen);
               ActualFileName := DecodeRarFileName(ActualFileName, RarFileDataPtr^.FileNameLen); //fFileName;

               SetLength(fFileName, RarFileDataPtr^.FileNameLen);
               //ZeroMemory(@fFileName[1], RarFileDataPtr^.FileNameLen * SizeOf(WideChar));
               ZeroMemory(@fFileName[1], RarFileDataPtr^.FileNameLen);
               //Move(RarFileDataPtr.FileName, fFileName[1], RarFileDataPtr^.FileNameLen * SizeOf(WideChar));
               Move(RarFileDataPtr^.FileName, fFileName[1], RarFileDataPtr^.FileNameLen);

               If WriteToFile() Then
                  // format filename
                  FileName := DecodeRarFileName(fFileName, RarFileDataPtr^.FileNameLen)
               Else
                  // non-format filename
                  fFileName := DecodeRarFileName(fFileName, RarFileDataPtr^.FileNameLen);

         	End;

            If RarHeaderDataEx.HostOS = word(ord(osUnix)) Then
               If (RarHeaderDataEx.FileAttr And $4000 = 0) Then (* Unix Dir attr *)
                  Attr := 32
               Else
                  Attr := FILE_ATTRIBUTE_DIRECTORY
            Else
               Attr := RarHeaderDataEx.FileAttr;

            PFCode := 0;
				If ((Attr And FILE_ATTRIBUTE_DIRECTORY) = 0) And
               CheckWildCard2(
               	ActualFilename,
                  FileSpec,
                  ExcludeSpec,
                  RecurseDirs) Then
            Begin
               // the following call to Open_Outfile() doesn't acutally
               // open a file.  With a faMemoryStream, it allows the
               // addition of a TStreamHeader for each file to the
               // ztvMemoryStream object.  Without this call, TUnRAR
               // can't support the WriteMetod:faMemoryStream set
               // member.
               If WriteMethod = faMemoryStream Then
               Begin
                  InflateRec.UnpackedSize := RarheaderDataEx.UnpackedSize;
                  Open_OutFile(Outfile, FileName, ActualFilename);
               End;

               Crc32Val := CRC_MASK;

               // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
               GlobalDate := RarHeaderDataEx.FileDate;

               TranslateOem := TranslateOemChar;
               Try

            		If RarHeaderDataEx.HeadFlags And LHD_UNICODE > 0 Then
                  	TranslateOemChar := False;


                  If doOnBegin(Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
                  Begin

            			If RarHeaderDataEx.HeadFlags And LHD_UNICODE = 0 Then
                        If WriteToFile() Then
                           If Not DirExists(ExtractFileDir(FileName)) Then
                              CreateDirEx(ExtractFileDir(FileName));

                     If mode = CEXTRACT Then
                        ModoOp := RAR_EXTRACT
                     Else
                        ModoOp := RAR_TEST;

                     PFCode := ProcessOp(ModoOp);
                     If Not LocalOnEnd(ActualFilename, PFcode) Then
                        Dec(Count);

                  End Else
                     PFCode := ProcessOp(ModoOp); // should be RAR_SKIP
            	Finally
               	TranslateOemChar := TranslateOem;
               End;

            End Else
               If (Attr And FILE_ATTRIBUTE_DIRECTORY = 0) Then
               	PFCode := ProcessOp(ModoOp) // should be RAR_SKIP
               Else
						If CreateStoredDirs Then
               		PFCode := ProcessOp(ModoOp); // should be RAR_SKIP

				If (PFcode <> 0) Then
            	Break;

            RHCode := RARReadHeaderEx(hArcHandle, RarHeaderDataEx);
            If (CurrentCount >= HighFileSizes.Count) Then Break;
         End;
      Finally
         PFCode := RARCloseArchive(hArcHandle);
         If PFcode <> 0 Then ;          //satisfy compiler
      End;
   Except
      On e: E_RAISE Do ;                //ShowMessage( E.Message );
   End;
End;
//-------------------------------------------------------------

Procedure TUnRar.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
Var
	CurDir: String;
Begin
   Try
   	HighFileSizes := THighFileSizes.Create();
   	HighFileSizes.INIT();
      CurDir := GetCurrentDir();
      Try
//      	If WriteToFile And (Not SetCurrentDir(ExtractDir)) Then
//         	Exit;

         If Not IsUNRARDLLAvailable Then
            RaiseError(E_RAISE, fVolumeName, '', '0', E_NORARDLL);

         Me := Self;
         If WriteToFile() Then
            ExtractFile(@fVolumeName[1], CEXTRACT)
         Else
            ExtractFile(@fVolumeName[1], CTEST);
      Finally
			HighFileSizes.DONE();
			HighFileSizes.Free();
         SetCurrentDir(CurDir);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

{$IFDEF DYNLOADUNRARDLL}

(* These are dummy stub routines which do nothing.  In case someone *)
(* tries to use the TUNCAB, TUnRAR, and TUnAce components without   *)
(* checking the value of IsUN{CAB}{RAR}{ACE}DLLAvailable.           *)

//-------------------------------------------------------------

Function DummyRAROpenArchive(Var ArchiveData: TRAROpenArchiveData): THandle; Stdcall;
Begin
   DummyRAROpenArchive := ERAR_EOPEN;
End;
//-------------------------------------------------------------

Function DummyRAROpenArchiveEx(Var ArchiveData: TRAROpenArchiveDataEx): THandle; Stdcall;
Begin
   DummyRAROpenArchiveEx := ERAR_EOPEN;
End;
//-------------------------------------------------------------

Function DummyRARCloseArchive(hArcData: THandle): Integer; Stdcall;
Begin
   Result := 0;
End;
//-------------------------------------------------------------

Function DummyRARReadHeader(hArcData: THandle; Var RarHeaderData: TRarHeadData): Integer; Stdcall;
Begin
   Result := ERAR_BAD_DATA;
End;
//-------------------------------------------------------------

Function DummyRARReadHeaderEx(hArcData: THandle; Var RarHeaderData: TRarHeadDataEx): Integer; Stdcall;
Begin
   Result := ERAR_BAD_DATA;
End;
//-------------------------------------------------------------

Function DummyRARProcessFile(hArcData: THandle; Operation: Integer; DestPath, DestName: PChar): Integer; Stdcall;
Begin
   Result := ERAR_BAD_ARCHIVE;
End;
//-------------------------------------------------------------

Procedure DummyRarSetCallback(hArcData: THandle; UnrarCallback: TUnrarCallback;
   	UserData:Integer); Stdcall;
Begin
	;
End;
//-------------------------------------------------------------


Initialization
                                   (* Try loading unrar.dll . *)
   UnRARDLLHandle := MyLoadLibrary(UNRARDLL);

   If (UnRARDLLHandle <> 0) Then
   Begin
      RAROpenArchive  := GetProcAddress(UnRARDLLHandle, 'RAROpenArchive');
      RarOpenArchiveEx:= GetProcAddress(UnRARDLLHandle, 'RAROpenArchiveEx');
      RARCloseArchive := GetProcAddress(UnRARDLLHandle, 'RARCloseArchive');
      RARReadHeader   := GetProcAddress(UnRARDLLHandle, 'RARReadHeader');
      RARReadHeaderEx := GetProcAddress(UnRARDLLHandle, 'RARReadHeaderEx');
      RARProcessFile  := GetProcAddress(UnRARDLLHandle, 'RARProcessFile');
      RarSetCallback  := GetProcAddress(UnRARDLLHandle, 'RARSetCallback');
      IsUNRARDLLAvailable := True;
   End
   Else
   Begin
      RAROpenArchive   := @DummyRAROpenArchive;
      RarOpenArchiveEx := @DummyRAROpenArchiveEx;
      RARCloseArchive  := @DummyRARCloseArchive;
      RARReadHeader    := @DummyRARReadHeader;
      RARReadHeaderEx  := @DummyRARReadHeaderEx;
      RARProcessFile   := @DummyRARProcessFile;
      RARSetCallback   := @DummyRARSetCallback;
      IsUNRARDLLAvailable := False;
   End;

Finalization
                                  (* If unrar.dll was loaded, unload it. *)
   Try
      If (UnRARDLLHandle <> 0) Then
         FreeLibrary(UnRARDLLHandle);
   Except;
   End;

   UnRARDLLHandle := 0;
   IsUNRARDLLAvailable := False;
{$ENDIF}

End (* UNRAR *).
