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

(* ------------------------------------------------------------------
 This is a DLL wrapper component which encapsulates cabinet.dll.  This
 dll can be obtained from the following link:

 http://msdn.microsoft.com/workshop/management/cab/cabdl.asp
  --------------------------------------------------------------------
 v4.7.1: Modified by Alexey Chernobaev, April 06, 02 for proper handling
 of nested (archive in archive) .cab files.
 -------------------------------------------------------------------- *)

Unit ztvUnCab;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   ztvbase,
   ztvGbls,
   ztvCabGbls,
   ztvStreams,
   Err_Msgs,
   ztvUnCabTypes;

Type
  (* Action to take when copying file *)
   TFileCopyAction = (fcaAbort, fcaSkip, fcaCopy, fcaDefaultCopy);

   TCabStruct = Packed Record
      CabinetName, CabinetDisk, CabinetPath: String;
      setID, CabinetNumber: longint;
   End;

   TCabinetInfoEvent = Procedure(Sender: TObject; Const CabinetName,
      CabinetDisk, CabinetPath: String; setID, CabinetNumber: longint;
      Var Abort: Boolean) Of Object;

   TUnCab = Class(TUnBase)
   Protected
      oFile: TStream32;
      fContext: HFDI;
      BitFlags: Integer;
      CabUncompressedSize: DWord;
      AbortFlag, SaveCabStruct: Boolean;
      CabStruct: TCabStruct;
      CabAction: TFileCopyAction;
      FErrorBuffer: TERF; // TERF.fError must be LongBool, not Boolean!
      Procedure CabCheck(Result: Boolean);
      Procedure BuildRecoveryHeadArray(ArcName: String);
      Procedure ContextNeeded;
      Procedure DoCabinetInfo(Const CabinetName, CabinetDisk,
         CabinetPath: String; setID, CabinetNumber: longint;
         Var Abort: Boolean); Virtual;
      Procedure DoExtractFile(Var CabFilename: String;
         UncompressedSize: longint; aDate, aTime, Attribs: SmallInt;
         Var Action: TFileCopyAction; Var DestFileHandle: Integer); Virtual;
      Procedure DoCloseOutFile(Const CabFilename: String;
         FileHandle: Integer; Date, Time, Attribs: SmallInt;
         FolderIndex: Integer; Execute: Boolean; Var Abort: Boolean); Virtual;
      Procedure DoNextCabinet(Var NextCabinetName, CabinetPath: String;
         ErrorIndication: TFDIERROR; Var Abort: Boolean); Virtual;
   Public
      Destructor Destroy; Override;
      Property Context: HFDI Read fContext;
      Function IsCabinet(Const FileName: String; Var CabInfo: TFDICABINETINFO):
         Boolean;
      Procedure ExtractIT(Var InFile: TStream32; Outfile: TStream32); Override;
   Published
      Property ArcType;
      Property ConfirmOverwrites;
      Property ExtractDir;
      Property FileSpec;
      Property OverwriteMode;
      Property RestoreFileAttr;
      Property UseStoredDirs;
      Property OnActivate;
      Property OnDeactivate;
      Property OnProgress;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnNextVolume;
      Property OnRenameFile;
   End;

Var
   IsUNCABDLLAvailable: Boolean = false; (* True if cabinet.dll available. *)

Implementation

Var
   UnCABDLLHandle: THandle = 0;

   FDICreate: TFDICreate = Nil;
   FDIIsCabinet: TFDIIsCabinet = Nil;
   FDICopy: TFDICopy = Nil;
   FDIDestroy: TFDIDestroy = Nil;

   GlobCabinetReaders: TList = Nil;
   FileHandler: TStreamCabinetFileHandler = Nil;

Procedure GlobCabinetReaders_Pop;
Begin
   GlobCabinetReaders.Delete(GlobCabinetReaders.Count - 1);
End;

Function GlobCabinetReader: TUnCab;
Begin
   Result := GlobCabinetReaders[GlobCabinetReaders.Count - 1];
End;

Function DummyFDICreate(PFNALLOC: PFNALLOC; PFNFREE: PFNFREE; pfnopen: pfnopen;
   pfnread: pfnread; pfnwrite: pfnwrite; pfnclose: pfnclose; pfnseek: pfnseek;
   CpuType: Integer; PERF: PERF): HFDI; Cdecl;
Begin
   Result := Nil;
End;

Function DummyFDIIsCabinet(HFDI: HFDI; hf: Integer; pfdici: TPFDICABINETINFO):
   Boolean; Cdecl;
Begin
   Result := True;
End;

Function DummyFDICopy(HFDI: HFDI; pszCabinet: pChar; pszCabPath: pChar;
   Flags: Integer; pfnfdin: PFNFDINOTIFY; pfnfdid: PFNFDIDECRYPT;
   pvUser: PVoid): Boolean; Cdecl;
Begin
   Result := True;
End;

Function DummyFDIDestroy(HFDI: HFDI): Boolean; Cdecl;
Begin
   Result := True;
End;

Function StdFdiOpen(pszFile: pChar; oflag: Integer; pmode: Integer): Integer;
   Cdecl;
Var
   ErrorCode: Integer;
Begin
   Result := FileHandler.Open(String(pszFile), oflag, pmode, ErrorCode,
      ftCabinet);
End;

Function StdFdiRead(hf: Integer; memory: PVoid; cb: TUINT): TUINT; Cdecl;
Var
   ErrorCode: Integer;
Begin
   Result := FileHandler.Read(hf, memory^, cb, ErrorCode);
End;

Function StdFdiWrite(hf: Integer; memory: Pointer; cb: TUINT): TUINT; Cdecl;
Begin
  //Result := cb;
  //If cabReadDir then Exit;
   With GlobCabinetReader Do
   Try
    //Dec(Bytes_To_Go, cb);
    //ProgressPosition := ProgressPosition - cb;
    //doBranchProgress(CabUncompressedSize - Bytes_To_Go, CabUncompressedSize,
    //  fTotalUnpackedSize);
   Finally
      Result := ExtractWriteBlock(oFile, memory^, false, 32, cb, dtData);
   End;
End;

Function StdFdiClose(hf: Integer): Integer; Cdecl;
Var
   ErrorCode: Integer;
Begin
   Result := FileHandler.Close(hf, ErrorCode);
End;

Function StdFdiSeek(hf: Integer; dist: longint; SeekType: Integer): longint;
   Cdecl;
Var
   ErrorCode: Integer;
Begin
   Result := FileHandler.Seek(hf, dist, SeekType, ErrorCode);
End;

Function StdFdiNotifyCollectFileData(fdint: TFDINOTIFICATIONTYPE; pfdin:
   PFDINOTIFICATION): Integer; Cdecl;
Var
   UnCab: TUnCab;
Begin
   UnCab := pfdin^.pv;
   Case fdint Of
//!!    fdintCABINET_INFO: ;
//!!    fdintPARTIAL_FILE: ;
      fdintCOPY_FILE:
         With UnCab Do
            If CheckWildCard2(String(pfdin^.psz1), FileSpec, ExcludeSpec,
               RecurseDirs) And Not Cancel Then
            Begin
               fTotalPackedSize := fTotalPackedSize + pfdin^.cb;
               fTotalUnpackedSize := fTotalUnpackedSize + pfdin^.cb;
               If Makelong(pfdin^.Time, pfdin^.Date) > fMaxAge Then
                  fMaxAge := Makelong(pfdin^.Time, pfdin^.Date);
            End;
   End;
   Result := 0;
   If UnCab.Cancel Then Result := -1;
End;

Function StdFdiNotify(fdint: TFDINOTIFICATIONTYPE; pfdin: PFDINOTIFICATION):
   Integer; Cdecl;
Var
   Abort: Boolean;
   Handle: Integer;
   CabPath, FileName: String;
   UnCab: TUnCab;
Begin
   Abort := false;
   Result := 0;
   UnCab := pfdin^.pv;
   GlobCabinetReaders.Add(UnCab);
   Try
      Case fdint Of
         fdintCABINET_INFO:
            UnCab.DoCabinetInfo(String(pfdin^.psz1), String(pfdin^.psz2),
               String(pfdin^.psz3), pfdin^.setID, pfdin^.iCabinet, Abort);
         fdintCOPY_FILE:
            Begin
               UnCab.CFFile.UnpackedSize := pfdin^.cb;
               UnCab.CFFile.FileDate := Makelong(pfdin^.Time, pfdin^.Date);
               UnCab.CFFile.ExternalAttr := pfdin^.Attribs;

               UnCab.ActualFilename := OemToCharFilter(String(pfdin^.psz1),
                  UnCab.fTransOemChar);

               UnCab.DoExtractFile(UnCab.ActualFilename,
                  pfdin^.cb, pfdin^.Date, pfdin^.Time, pfdin^.Attribs,
                     UnCab.CabAction, Handle);

               If UnCab.CabAction = fcaCopy Then
               Begin
                  StrPCopy(pfdin^.psz1, UnCab.ActualFilename);
                  Result := Handle;
               End;
               exit;
            End;
         fdintCLOSE_FILE_INFO:
            Begin
               UnCab.DoCloseOutFile(String(pfdin^.psz1), pfdin^.hf, pfdin^.Date,
                  pfdin^.Time, pfdin^.Attribs, pfdin^.iFolder, pfdin^.cb = 1,
                     Abort);
               If Not Abort Then Result := 1;
               exit;
            End;
         fdintNEXT_CABINET:
            Begin
               FileName := String(pfdin^.psz1);
               CabPath := String(pfdin^.psz3);
               UnCab.DoNextCabinet(FileName, CabPath, pfdin^.fdie, Abort);
               StrLCopy(pfdin^.psz1, pChar(FileName), 256);
               StrLCopy(pfdin^.psz3, pChar(CabPath), 256);
            End;
      End;
      If Abort Or UnCab.Cancel Then
         Result := -1;
   Finally
      GlobCabinetReaders_Pop;
   End;
End;

(* Cabinet procs *)

Type
   TCabinetProcRec = Packed Record
    (* FCI/FDI *)
      FciAlloc: PFNFCIALLOC;
      FciFree: PFNFCIFREE;
    (* FDI *)
      FdiOpen: pfnopen;
      FdiRead: pfnread;
      FdiWrite: pfnwrite;
      FdiClose: pfnclose;
      FdiSeek: pfnseek;
      FdiNotify: PFNFDINOTIFY;
      FdiNotifyCollectFileData: PFNFDINOTIFY;
   End;

Const
   StdCabinetProcs: TCabinetProcRec = (
      FciAlloc: StdFciAlloc;
      FciFree: StdFciFree;
      FdiOpen: StdFdiOpen;
      FdiRead: StdFdiRead;
      FdiWrite: StdFdiWrite;
      FdiClose: StdFdiClose;
      FdiSeek: StdFdiSeek;
      FdiNotify: StdFdiNotify;
      FdiNotifyCollectFileData: StdFdiNotifyCollectFileData);

(* TUnCAB *)

Destructor TUnCab.Destroy;
Begin
   If fContext <> Nil Then
      FDIDestroy(fContext);
   Inherited Destroy;
End;

Procedure TUnCab.CabCheck(Result: Boolean);

   Procedure CheckErf(Erf: TERF);
   Begin
      If bool(Erf.fError) Then
         If (Erf.erfOper = 11) {And AbortFlag} Then  // OnGetNextVolume set Abort = True (Not actually a user abort)
            AbortFlag := false
         Else If (Erf.erfOper = 4) Then
            RaiseErrorStr(fArchiveFile, ActualFilename, '', E_CORRUPTINPUT)
         Else
            RaiseCabinetError(Erf.erfOper, Erf.erfType);
   End;

Begin
   If Not Result Then
      CheckErf(FErrorBuffer);
End;

Procedure TUnCab.ContextNeeded;
Begin
   If fContext = Nil Then
   Begin
      GlobCabinetReaders.Add(Self);
      Try
         With StdCabinetProcs Do
            fContext := FDICreate(FciAlloc, FciFree, FdiOpen, FdiRead, FdiWrite,
               FdiClose, FdiSeek, Integer(CpuType) - 1, @FErrorBuffer);
         CabCheck(fContext <> Nil);
      Finally
         GlobCabinetReaders_Pop;
      End;
   End;
End;

Function TUnCab.IsCabinet(Const FileName: String; Var CabInfo: TFDICABINETINFO):
   Boolean;
Var
   ErrorCode, FileHandle: Integer;
Begin
   ContextNeeded;
   FileHandle := FileHandler.Open(FileName, _O_RDONLY Or _O_BINARY, 0,
      ErrorCode, ftCabinet);
   If ErrorCode <> 0 Then
      RaiseError(E_RAISE, FileName, '', '0', E_FOPEN);

   GlobCabinetReaders.Add(Self);
   Try
      Result := FDIIsCabinet(fContext, FileHandle, @CabInfo);
   Finally
      GlobCabinetReaders_Pop;
      FileHandler.Close(FileHandle, ErrorCode);
      If ErrorCode <> 0 Then
         RaiseError(E_RAISE, FileName, '', '0', E_FCLOSE);
   End;
End;

Procedure TUnCab.DoCabinetInfo(Const CabinetName, CabinetDisk,
   CabinetPath: String; setID, CabinetNumber: longint; Var Abort: Boolean);
Begin
   If SaveCabStruct Then
   Begin
      CabStruct.CabinetName := CabinetName;
      CabStruct.CabinetDisk := CabinetDisk;
      CabStruct.CabinetPath := CabinetPath;
      CabStruct.setID := setID;
      CabStruct.CabinetNumber := CabinetNumber;
      SaveCabStruct := false;
   End;
   Abort := false;
End;

Procedure TUnCab.DoExtractFile(Var CabFilename: String; UncompressedSize:
   longint; aDate, aTime, Attribs: SmallInt; Var Action: TFileCopyAction;
   Var DestFileHandle: Integer);
Var
   OpenedOK: Boolean;
Begin
   If CheckWildCard2(CabFilename, FileSpec, ExcludeSpec, RecurseDirs) And
      Not Cancel Then
   Begin
      CabUncompressedSize := UncompressedSize;
      Bytes_To_Go := UncompressedSize;
      FileName := CabFilename;
      If WriteToFile Then
         CreateDirEx(ExtractFilePath({Open} FileName));

      //If FOverwriteMode = omCancel Then Exit;
      ActualFilename := CabFilename;

      // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
      GlobalDate := Makelong(aTime, aDate);
      If doOnBegin({( Attribs And faVolumeID > 0 ) Or}(Attribs And
         FILE_ATTRIBUTE_DIRECTORY > 0)) Then
      Begin
         FileName := ActualFilename; //in case ActualFilename changed in DoOnBegin
         OpenedOK := Open_OutFile(oFile, FileName, CabFilename);
         If OpenedOK Then
         Begin
            Action := fcaCopy;
            If WriteToFile Then
               DestFileHandle := TFileStream32(oFile).Handle
            Else
            (* Dummy value to prevent the API from terminating	*)
            (* when an archive is opened for mode other than	*)
            (* to write to a disk file.  Search components use	*)
            (* this component, but does Not write to a file.	*)
               DestFileHandle := 1;
         End
         Else
         Begin
          //RaiseErrorStr( {Open} Filename, '', CabStruct.CabinetName, E_FOPEN );
            Action := fcaSkip;
            Cancel := True;
         End;
      End
      Else
      Begin
         Action := fcaSkip;
        //ProgressPosition := ProgressPosition - UncompressedSize;
        //doBranchProgress(0, 0, fTotalUnpackedSize);
      End;
   End
   Else
      Action := fcaSkip;
End;

Procedure TUnCab.DoCloseOutFile(Const CabFilename: String; FileHandle: Integer;
   Date, Time, Attribs: SmallInt; FolderIndex: Integer; Execute: Boolean; Var
      Abort: Boolean);
Var
   CRC_PASS: Boolean;
Begin
   CRC_PASS := CabAction = fcaCopy;
   If Assigned(OnEnd) Then
   Begin
      If WriteToFile Then
         OnEnd(Self, FileName, CRC_PASS)
      Else
         OnEnd(Self, CabFilename, CRC_PASS);
   End
   Else If Not CRC_PASS Then
      RaiseErrorStr(CabFilename, '', '0', E_CRCERROR);

   CloseAndSetDate(oFile, FileName, Makelong(Time, Date), Attribs);
End;

Procedure TUnCab.DoNextCabinet(Var NextCabinetName, CabinetPath: String;
   ErrorIndication: TFDIERROR; Var Abort: Boolean);
Begin
   fVolumeName := AppendDirTail(CabinetPath) + NextCabinetName;
   If Not GetNextVolume(fVolumeName, NextCabinetName) Then
   Begin
      AbortFlag := True;
      Abort := True;
   End
   Else
   Begin
      NextCabinetName := ExtractFileName(fVolumeName);
      CabinetPath := ExtractFilePath(fVolumeName);
   End;
End;

Procedure TUnCab.BuildRecoveryHeadArray(ArcName: String);
Begin
   GlobCabinetReaders.Add(Self);
   Try
    //cabReadDir := True;
      SaveCabStruct := false;
      CabCheck(
         FDICopy(fContext, pChar(ExtractFileName(ArcName)),
         pChar(ExtractFilePath(ArcName)), 0,
            StdCabinetProcs.FdiNotifyCollectFileData,
         Nil,                           // Decryption Function pointer here
         Self));
   Finally
      GlobCabinetReaders_Pop;
    //cabReadDir := False;
      ProgressPosition := fTotalUnpackedSize;
   End;
End;

Procedure TUnCab.ExtractIT(Var InFile: TStream32; Outfile: TStream32);
Type
   _EXTRACT_FILE = Record
      Hwnd: Hwnd;
      pbEntry: ^Byte;
      pbFile: ^Byte;
   End;
Var
   Vol_Name: String;
   CabInfo: TFDICABINETINFO;
Begin
   If Not IsUNCABDLLAvailable Then
      RaiseError(E_RAISE, fArchiveFile, '', '0', E_NOCABDLL);

   Crc32Val := {0; //} CRC_MASK;
   Vol_Name := fArchiveFile;
   AbortFlag := false;
   If Not (FileExists(fArchiveFile) And IsCabinet(fArchiveFile, CabInfo)) Then
      exit;

   BuildRecoveryHeadArray(fArchiveFile);
   Repeat
      SaveCabStruct := True;
      GlobCabinetReaders.Add(Self);
      Try
         CabCheck(
            FDICopy(fContext, pChar(ExtractFileName(Vol_Name)),
            pChar(ExtractFilePath(Vol_Name)), 0, StdCabinetProcs.FdiNotify,
            Nil,                        // Decryption Function pointer here
            Self));
      Finally
         GlobCabinetReaders_Pop;
      End;

    //If fErrorBuffer.erfOper = 11 Then ;
    //If fErrorBuffer.erfType = 0 Then ;
    //If fErrorBuffer.fError = True Then ;
      fCancel := fCancel Or FErrorBuffer.fError;

      If Cancel Then
         Break
      Else If (CabStruct.CabinetName <> '') And
         (ExtractFileName(Vol_Name) <> CabStruct.CabinetName) Then
      Begin
         Vol_Name := ExtractFilePath(Vol_Name) + CabStruct.CabinetName;
         If Not GetNextVolume(Vol_Name, ExtractFileName(Vol_Name)) Then
            Break;
         fTotalPackedSize := 0;
         fTotalUnpackedSize := 0;
         BuildRecoveryHeadArray(Vol_Name);
      End
      Else
         Break;
   Until ExtractFileName(Vol_Name) = '';
End;

Initialization
   UnCABDLLHandle := LoadLibrary('cabinet.dll');
   If UnCABDLLHandle <> 0 Then
   Begin
      FDICreate := GetProcAddress(UnCABDLLHandle, 'FDICreate');
      FDIIsCabinet := GetProcAddress(UnCABDLLHandle, 'FDIIsCabinet');
      FDICopy := GetProcAddress(UnCABDLLHandle, 'FDICopy');
      FDIDestroy := GetProcAddress(UnCABDLLHandle, 'FDIDestroy');
      IsUNCABDLLAvailable := True;
      GlobCabinetReaders := TList.Create;
      FileHandler := TStreamCabinetFileHandler.Create(Nil);
   End
   Else
   Begin
      FDICreate := @DummyFDICreate;
      FDIIsCabinet := @DummyFDIIsCabinet;
      FDICopy := @DummyFDICopy;
      FDIDestroy := @DummyFDIDestroy;
      IsUNCABDLLAvailable := false;
   End;

Finalization
   If UnCABDLLHandle <> 0 Then
   Begin
      FreeLibrary(UnCABDLLHandle);
      FileHandler.Free();
      GlobCabinetReaders.Free();
   End;
End.
