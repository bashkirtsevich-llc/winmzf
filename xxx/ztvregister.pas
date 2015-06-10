Unit ztvregister;

Interface

Uses
   Controls,
   Dialogs,
   Windows,
   Classes,
   Forms,
   ztvHeaders,
   ztvStreams,
   ztvGbls,
   ztvCrypt,
   ztvFileIo;


{$I ZipTV.Inc}
{$J+} { Writeable Typed Constants }     // v4.0 added

Type
	TTempFileStream = Class;

   TDeleteOptions =
   	(doFinal, doAllowUndo);

   TTempFileMove =
   	Procedure(Sender: TTempFileStream; ToFileName, FromFileName: AnsiString;
		SameDrive: Boolean; Size: Int64)
      Of Object;

	TTempFileMoveBegin =
   	Procedure(Sender: TObject; Archive, TempFileName: AnsiString)
      Of Object;

   TTempFileStream = Class(TFileStream32)
   Private
		fProgressCallBackProc: TNotifyEvent;
      fAutoDelete: Boolean;
      fNewFilename: String;
   Protected
   Public
      fLocalObj: TObject;
      CancelCallBackProc: Pointer;
      DeleteOptions: TDeleteOptions;
      FileMode: word;
      ProgressProc: TNotifyEvent;
      TempFileMoveProc: TTempFileMove;
      TempFileMoveBeginProc: TTempFileMoveBegin;
      TempFileMoveEndProc: TNotifyEvent;
      fTempFileName: String;
		Constructor Create(Sender: TObject; TempDir, FileName: String; mode: word);
      Destructor Destroy; Override;
    	Function CopyFrom(Source: TStream32; Count: Int64): Int64; Virtual;
      Function Read(Var Buffer; Count: longint): longint; Override;
      Property ProgressCallBackProc: TNotifyEvent Read fProgressCallBackProc
      	Write fProgressCallBackProc;
   End;

   TTempMemStream = Class(TMemoryStream32)
   Private
		fProgressCallBackProc: TNotifyEvent;
      fAutoDelete: Boolean;
      fNewFilename: String;
   Protected
   Public
      fLocalObj: TObject;
      CancelCallBackProc: Pointer;
      FileMode: word;
      ProgressProc: TNotifyEvent;
      TempFileMoveProc: TTempFileMove;
      TempFileMoveBeginProc: TTempFileMoveBegin;
      TempFileMoveEndProc: TNotifyEvent;
      fTempFileName: String;
		Constructor Create(Sender: TObject; FileName: String; mode: word);
      Destructor Destroy; Override;
    	Function CopyFrom(Source: TStream32; Count: Int64): Int64; Virtual;
      Function Read(Var Buffer; Count: longint): longint; Override;
      Property ProgressCallBackProc: TNotifyEvent Read fProgressCallBackProc
      	Write fProgressCallBackProc;
   End;

   TExtStruct = Packed Record
      a: Array[0..3] Of Pointer;
   End;

   TRegisterZipTV = Class(TComponent)
   Private
      fAbout: String;
   	fExtraFieldLen: Word;
      fEncryptHeaders: Boolean;
      fOnDiskInDrvErr: TOnDiskError;
      fOnWriteProtectErr: TOnDiskError;
      fZipCmntBufSize: Integer;
      Procedure SetAbout(s: String);
      Function GetZipCompatible: Boolean;
      Function GetEncryptHeaders: Boolean;
      Procedure SetZipCmntBufSize(i: Integer);
      Function GetAbout: String;
   Protected
      fZipCompatible: Boolean;
      Function GetArchiveFile: String; Virtual;
      Function DefSig(HT: THeaderType; Encrypted: Boolean): Integer;
      Function VerSig(Sig: Integer; HT: THeaderType;
         Var Encrypted: Boolean): THeaderType;
      Function doByteSearch(s: TStream32; Var FAT: TArcType; StartOffset:
      	Cardinal): Cardinal;
		Function IsSFXZipped(s: TStream32): TArcType;
      Function OpenArchive(Var f: THandle; FileName: String): Boolean;
      Procedure DecodeFilename(p: Pointer; Len: Integer);
      Procedure DecodeComment(p: Pointer; Len: Integer);
      Procedure DecodeHeader(p: Pointer; HType: THeaderType);
      Procedure EncodeFilename(p: Pointer; Len: Integer);
      Procedure EncodeComment(p: Pointer; Len: Integer);
      Procedure EncodeHeader(p: Pointer; HType: THeaderType);
      Procedure InitializeVolumeSet;
      Procedure SetArchiveFile(SAF: String); Virtual;
      Procedure SetArcType(ArcType: TArcType); Virtual;
      Procedure WEI(strm: TStream32); Virtual;
      Property ZipCompatible: Boolean Read GetZipCompatible Write
         fZipCompatible Default True;
   Public
      ArchiveCommentPos: Int64;
      ZipSFX_OffsetAdjustment: fs_long;
      fArchiveDate: Integer;
      fArchiveFile: String;
      fArcType: TArcType;
      fLOF: Int64;
      fOffsetStart: Int64;
      fVolNum: Integer;
      fVolBegChar: Byte;

      LocalZipHeader: TLocal;
      CentralZipHeader: TCentral;
      EndZipHeader: TEnd;
      EndZipHeader64: TZipTV_End64;
      WZip_End64: TWZIP_END64;
      WZipLocator: TWZipLocator;

      TarHeader: TTarHeader;
      CFFile: TCAB_CFFILE;
      HeaderTypeState: THeaderTypeState;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
		Function GetArcType(s: TStream32): TArcType;
      Function GetCentralDirOffset(s: TStream32): Int64;
      Function IsHeaderEncrypted(SignAtr: Integer): Boolean;
      Function IsZipCompatible(SignAtr: Integer): Boolean;
      Function maxbits: ShortInt;
      Property ArchiveDate: Integer Read fArchiveDate Write fArchiveDate;
      Property ArchiveFile: String Read GetArchiveFile Write SetArchiveFile; 
      Property ArcType: TArcType Read fArcType Write SetArcType Default atNA;
      Property EncryptHeaders: Boolean Read GetEncryptHeaders Write
         fEncryptHeaders Default False;
      Property ZipCmntBufSize: Integer Read fZipCmntBufSize Write
         SetZipCmntBufSize Stored False;
      Property ExtraFieldLen: Word Read fExtraFieldLen Write fExtraFieldLen;
      Property OnDiskInDrvErr: TOnDiskError Read fOnDiskInDrvErr Write fOnDiskInDrvErr;
      Property OnDiskWriteProtectErr: TOnDiskError Read fOnWriteProtectErr
      	Write fOnWriteProtectErr;
   Published
      Property About: String Read GetAbout Write SetAbout Stored False;
   End;

Const
   cMINVER = 6;
   cMAXVER = 3;
   cRevision = 2;
   MAX_WBITS = -15;
   MaxBufSize = $186A0;

Var
   fCrcCalced: Boolean = False;
   TZInfo: TTimeZoneInformation;
   dwTimeZoneMode: Integer;
   fEI: TExtStruct;
   pEI: ^TExtStruct;
   p: Pointer;
   HKeyed: Tak;
 	CF_COMPRESSED_DATA: Integer;

Function CopyStreamFromClipboard(Stream: TMemoryStream32): Boolean;
Function CopyStreamToClipboard(Stream: TMemoryStream32; Len: Integer): Boolean;
Function DosDateToUnix(tm: TDateTime): Integer;
Function IsLeapYear(Year: word): Boolean;
Function LocalTimeToUniversal(LT: TDateTime): TDateTime;
Function OctStrToInt(s: String): Integer;
Function UniversalTimeToLocal(UT: TDateTime): TDateTime;
Function UnixDateToDos(UnixDate: Integer): TDateTime;
Function vKey: Integer;
Procedure IntToOctStr(value, digs: Integer; StrTerminate: Boolean; Where: PChar);
Procedure DecodeRarFN(Name: pChar; EncName: PChar; EncSize: Integer;
   NameW: PWideChar; MaxDecSize: Integer);

Implementation

Uses
   ClipBrd,
   ShellApi,
   Buttons,
   StdCtrls,
   SysUtils,
   ERR_MSGS,
   ztvBase;



(*************************************************************)
(*************************************************************)
(*                      TTempMemStream                       *)
(*************************************************************)
(*************************************************************)

Constructor TTempMemStream.Create(Sender: TObject; {TempDir,} FileName: String;
	mode: word);
Begin
   fLocalObj := Sender;

   CancelCallBackProc := Nil;
   ProgressCallBackProc := Nil;

   TempFileMoveProc := TZipCommon(Sender).TempFileMoveFile;
   TempFileMoveBeginProc := TZipCommon(Sender).TempFileMoveBegin;
   TempFileMoveEndProc := TZipCommon(Sender).TempFileMoveEnd;
	ProgressProc := TZipCommon(Sender).TempFileProgress;

   If (mode And fmCreate = fmCreate) Then
   Begin
      fTempFileName := '';
      fNewFilename := FileName;
      fAutoDelete := True;
   End
   Else
   Begin
      fTempFileName := FileName;
      fNewFileName := '';
      fAutoDelete := False;
   End;

   Inherited Create();
   FileMode := mode;
End;
//-------------------------------------------------------------

Destructor TTempMemStream.Destroy;
Var
	ECode: DWord;
	OutStream: TFileStream32;
Begin
   If (Not fAutoDelete) Or (FileMode And fmOpenRead > 0) Then
   	Inherited Destroy
   Else
      Try
         If (FileMode <> 0) And (fNewFilename <> '') Then
         Begin
            Try
               If (FileMode And fmCreate > 0) Then
               Begin

                  OutStream := TFileStream32.Create(fNewFileName, fmCreate Or
                  	fmShareExclusive);
                  OutStream.CancelCallBackProc := Nil;
                  OutStream.ProgressCallBackProc := ProgressProc;

                  If (OutStream.Handle < 0) Then
                  Begin
                     OutStream.Free();
                     Raise EFOpenError.CreateFmt(LoadStr(E_FOPEN), [fNewFileName]);
                     Exit;
                  End;

                  With TZipCommon(fLocalObj) Do
                  Try
      					TempFileMoveBeginProc(Self, fNewFileName, fTempFileName);

                     fUnpackedSize := Size;
                     fTotalUnpackedSize := Size;
                     ProgressPosition := Size;
                     Position := 0;

                     OutStream.CopyFrom(Self, Size);
      					TempFileMoveEndProc(Self);
                  Finally
                     OutStream.Free();
                  End;
               End;
            Except
               ECode := GetLastError();
   				If ECode = ERROR_DISK_FULL Then
               	With TZipCommon(fLocalObj) Do
               		RaiseErrorStr(ArchiveFile, '', '0', E_DISKFULL);

               Raise;
            End;
         End;
      Finally
         Inherited Destroy();
      End;
End;
//-------------------------------------------------------------

Function TTempMemStream.CopyFrom(Source: TStream32; Count: Int64): Int64;
Var
   Buffer: PChar;
	pCancel: ^Boolean;
	TempCancel: Boolean;
   BufSize, n: Integer;
Begin
   If Count = 0 Then
   Begin
      Source.Position := 0;
      Count := Source.Size;
   End;

   Result := Count;
   If Count > MaxBufSize Then
      BufSize := MaxBufSize
   Else
      BufSize := Count;

   pCancel := CancelCallBackProc;
   If pCancel = Nil Then
   Begin
   	TempCancel := False;
      pCancel := @TempCancel;
   End;

   OnProgress := ProgressCallBackProc;

   GetMem(Buffer, BufSize);
   Try
      While (Count <> 0) And (Not pCancel^) Do
      Begin
         If Count > BufSize Then
            n := BufSize
         Else
            n := Count;

         Application.ProcessMessages();
         Source.ReadBuffer(Buffer^, n);
         WriteBuffer(Buffer^, n);
         Dec(Count, n);
      End;
   Finally
      FreeMem(Buffer, BufSize);
   End;
End;
//-------------------------------------------------------------

Function TTempMemStream.Read(Var Buffer; Count: longint): longint;
Begin
   Result := Inherited Read(Buffer, Count);

   TZipCommon(fLocalObj).ProgressPosition :=
   	TZipCommon(fLocalObj).ProgressPosition - Count;

	ProgressProc(Self);
End;


(*************************************************************)
(*************************************************************)
(*                      TTempFileStream                      *)
(*************************************************************)
(*************************************************************)

Constructor TTempFileStream.Create(Sender: TObject; TempDir, FileName: String;
	mode: word);
Begin
   fLocalObj := Sender;

   CancelCallBackProc := Nil;
   ProgressCallBackProc := Nil;

   TempFileMoveProc := TZipCommon(Sender).TempFileMoveFile;
   TempFileMoveBeginProc := TZipCommon(Sender).TempFileMoveBegin;
   TempFileMoveEndProc := TZipCommon(Sender).TempFileMoveEnd;
	ProgressProc := TZipCommon(Sender).TempFileProgress;


   If (mode And fmCreate = fmCreate) Then
   Begin
      fTempFileName := GetTempFilenameStr(TempDir);
      fNewFilename := FileName;
      fAutoDelete := True;
   End
   Else
   Begin
      fTempFileName := FileName;
      fNewFileName := '';
      fAutoDelete := False;
   End;

   Inherited Create(fTempFileName, mode);

   FileMode := mode;
   DeleteOptions := doFinal;
End;
//-------------------------------------------------------------

Destructor TTempFileStream.Destroy;
   Function CompareDrives(TempDir: String): Boolean;
   Var
    	dPos, fPos: Integer;
   Begin
    	dPos := Pos(':', TempDir);
    	If dPos > 0 Then
    	Begin
       	fPos := Pos(':', fNewFileName);
       	If fPos > 0 Then
          	Result :=
            	UpCase(TempDir[1]) = UpCase(fNewFileName[1])
       	Else
          	Result := False;
    	End Else
       	Result := False;
   End;
Var
	ECode: DWord;
   SameDrive: Boolean;
Begin

   If (Not fAutoDelete) Or (FileMode And fmOpenRead > 0) Then
   	Inherited Destroy
   Else
      Try
         If (FileMode <> 0) And (fNewFilename <> '') Then
         Begin
            Try
               If (FileMode And fmCreate > 0) Then
               Begin
                  TempFileMoveBeginProc(fLocalObj, fNewFileName, fTempFileName);
                  Try
                     SameDrive := CompareDrives(TZipCommon(fLocalObj).TempDir);
                     If SameDrive Then
                     Begin
                        If fHandle >= 0 Then
                           If CloseHandle(THandle(fHandle)) Then
                              fHandle := -1;
                     End;

                     TempFileMoveProc(Self, fNewFileName, fTempFileName,
                        SameDrive, Size);

                  Finally
                     TempFileMoveEndProc(fLocalObj);
                  End;
               End;
            Except
               ECode := GetLastError();
   				If ECode = ERROR_DISK_FULL Then
               	With TZipCommon(fLocalObj) Do
               		RaiseErrorStr(ArchiveFile, '', '0', E_DISKFULL);

               Raise;
            End;
         End;
      Finally
         Inherited Destroy;
         DeleteFile(fTempFileName);
      End;
End;
//-------------------------------------------------------------

Function TTempFileStream.CopyFrom(Source: TStream32; Count: Int64):
	Int64;
Var
   Buffer: PChar;
	pCancel: ^Boolean;
	TempCancel: Boolean;
   BufSize, n: Integer;
Begin
   If Count = 0 Then
   Begin
      Source.Position := 0;
      Count := Source.Size;
   End;

   Result := Count;
   If Count > MaxBufSize Then
      BufSize := MaxBufSize
   Else
      BufSize := Count;

   pCancel := CancelCallBackProc;
   If pCancel = Nil Then
   Begin
   	TempCancel := False;
      pCancel := @TempCancel;
   End;

   OnProgress := ProgressCallBackProc;

   GetMem(Buffer, BufSize);
   Try
      While (Count <> 0) And (Not pCancel^) Do
      Begin
         If Count > BufSize Then
            n := BufSize
         Else
            n := Count;

         Application.ProcessMessages();
         Source.ReadBuffer(Buffer^, n);
         WriteBuffer(Buffer^, n);
         Dec(Count, n);
      End;
   Finally
      FreeMem(Buffer, BufSize);
   End;
End;
//-------------------------------------------------------------

Function TTempFileStream.Read(Var Buffer; Count: longint): longint;
Begin
  	If Not ReadFile(THandle(Handle), Buffer, Count, LongWord(Result), Nil) then
   	Result := 0;

   TZipCommon(fLocalObj).ProgressPosition :=
   	TZipCommon(fLocalObj).ProgressPosition - Count;

	ProgressProc(Self);
End;
//-------------------------------------------------------------

Function Crc32(p: PChar; Len: Integer): u_long;
Var
   i: Byte;
	ptr: _pBytef;
Begin
   Result := CRC_MASK;
	ptr := @p[0];
   Try
      For i := 0 To Len - 1 Do
      Begin
         Result := Crc32Table[(Result Xor u_long(ptr^)) And $00FF] Xor
            ((Result Shr 8) And $00FFFFFF);

         Inc(ptr);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Constructor TRegisterZipTV.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   ExtraFieldLen := 0;
   fEncryptHeaders := False;
   If Not fCrcCalced Then
   Begin
      HKeyed[0] := DefK[0] And DefK[2];
      HKeyed[1] := DefK[1] And DefK[0];
      HKeyed[2] := DefK[2] And DefK[1];
   End;

   fZipCompatible := True;
   pEI := @fEI;
End;
//-------------------------------------------------------------

Destructor TRegisterZipTV.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TRegisterZipTV.maxbits: ShortInt;
Begin
   If GetZipCompatible() Then
      Result := MAX_WBITS
   Else
      Result := Abs(MAX_WBITS);
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.SetZipCmntBufSize(i: Integer);
Begin
   If i > 32767 Then
      i := 32767
   Else If i < 2000 Then
   	i := 2000;

   fZipCmntBufSize := i;
End;
//-------------------------------------------------------------

Function TRegisterZipTV.GetAbout: String;
Begin
   Result := '(MCS)';
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.SetAbout(s: String);
Begin
   fAbout := '(MCS)';
End;
//-------------------------------------------------------------

Function TRegisterZipTV.GetArchiveFile: String;
Begin
	Result := fArchiveFile;
End;
//-------------------------------------------------------------

Function TRegisterZipTV.GetZipCompatible: Boolean;
Begin
	Result := True;
End;
//-------------------------------------------------------------

Function TRegisterZipTV.GetEncryptHeaders: Boolean;
Begin
	Result := fEncryptHeaders;
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.SetArchiveFile(SAF: String);
Var
	s: TFileStream32;
Begin
   If Not FileExists(SAF) Then
   Begin
      fArcType := atNA;
      fArchiveFile := '';
      Exit;
   End;

   SAF := UnixToDosFilename(SAF);
   fArchiveFile := SAF;

   If (SAF <> '') Then
   Begin

   	s := TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyWrite );
      If (s.Handle < 0) Then
      Begin
      	fArcType := atNA;
         Exit;
      End;

      Try
      	fArcType := GetArcType(s);
      Finally
      	s.Free();
      End;

      If Not (fArcType In Invalid_ArcType) Then
         InitializeVolumeSet();
   End Else
      fArcType := atNA;
End;
//-------------------------------------------------------------

Function TRegisterZipTV.IsHeaderEncrypted(SignAtr: Integer): Boolean;
Begin
   Result :=
      (SignAtr = LOCAL_FILE_ENCRPT_SIGNATURE) Or
      (SignAtr = CENTRAL_FILE_ENCRPT_SIGNATURE) Or
      (SignAtr = END_OF_CENTRAL_ENCRPT_SIGNATURE) Or
      (SignAtr = END_OF_CENTRAL64_ENCRPT_SIGNATURE) Or
      (SignAtr = LOCAL_CUST_ENCRPT_SIGNATURE) Or
      (SignAtr = CENTRAL_CUST_ENCRPT_SIGNATURE);
End;
//-------------------------------------------------------------

Function TRegisterZipTV.IsZipCompatible(SignAtr: Integer): Boolean;
Begin
   Result :=
      (SignAtr = LOCAL_FILE_HEADER_SIGNATURE) Or
      (SignAtr = LOCAL_FILE_ENCRPT_SIGNATURE) Or
      (SignAtr = CENTRAL_FILE_HEADER_SIGNATURE) Or
      (SignAtr = CENTRAL_FILE_ENCRPT_SIGNATURE);
End;
//-------------------------------------------------------------

Function TRegisterZipTV.GetCentralDirOffset(s: TStream32): Int64;
Var
   b: ^Byte;
   i, j: Integer;
   Buffer: PChar;
   IsHeader,
   	Encrypted: Boolean;
   pSignattr: ^Integer;
   pEndZipHeader64: ^TZipTV_End64;
   pWZip_End64: ^TWZIP_END64;
   pWZipLocator: ^TWZipLocator;
   CentralHeadPos,
   	EndHeadPos: Int64;
   BufSize, BytesRead: Integer;
Begin
   Result := 0;

   If s.size = 0 Then Exit;

   If (s.size > WSIZE) Then
      BufSize := WSIZE
   Else
      BufSize := s.size - 1;

   HeaderTypeState := [];

   GetMem(Buffer, BufSize);
   Try
      s.Position := s.size - BufSize;
      BytesRead := s.Read(Buffer[0], BufSize);

      While (BytesRead > 0) Do
      Begin

         pEndZipHeader64 := @Buffer[BytesRead];
         If VerSig(pEndZipHeader64^.SignAtr, htEnding, Encrypted) = htEnding Then
         Begin
            HeaderTypeState := [htEnding];

            If Encrypted Then
               DecodeHeader(pEndZipHeader64, htEnding);

            WZipLocator.SignAtr := 0;
            WZip_End64.SignAtr := 0;

            pWZipLocator := @Buffer[BytesRead - SizeOf(TWZipLocator)];
            If pWZipLocator^.SignAtr = CENTRAL_WZIP_HEADER_LOCATOR Then
            Begin
            	Include(HeaderTypeState, htLocator);
            	pWZip_End64 := @Buffer[BytesRead - (SizeOf(TWZipLocator) + SizeOf(TWZIP_END64))];
               If pWZip_End64^.SignAtr = END_OF_CENTRAL_WZIP_HEADER_SIGNATURE Then
            		Include(HeaderTypeState, htEnding64);
            End;

         	EndHeadPos := s.size - BufSize + BytesRead;

            If is64BitEndingHdr(pEndZipHeader64^.SignAtr) Then
            Begin
            	CopyMem(pEndZipHeader64, @EndZipHeader64, SizeOf(TZipTV_End64));
            	CopyMem(pEndZipHeader64, @EndZipHeader, SizeOf(EndZipHeader.SignAtr));
            	ArchiveCommentPos := EndHeadPos + SizeOf(TZipTV_End64);
            	CentralHeadPos := EndZipHeader64.CentralDirOffset;
            End Else Begin
               ArchiveCommentPos := EndHeadPos + SizeOf(TEnd);

            	If (htLocator In HeaderTypeState) Then
            		CopyMem(pWZipLocator, @WZipLocator, SizeOf(TWZipLocator));

            	If (htEnding64 In HeaderTypeState) Then
               Begin
            		CopyMem(pWZip_End64, @WZip_End64, SizeOf(TWZip_End64));
               	CentralHeadPos :=
                  	pWZipLocator^.RelativeOffsetOfCentralHeader -
                     pWZip_End64^.SizeOfCentralDir;

               End Else Begin
            		CopyMem(pEndZipHeader64, @EndZipHeader, SizeOf(TEnd));
            		CentralHeadPos := EndHeadPos - EndZipHeader.SizeOfCentralDir;
               End;
            End;

            s.Position := CentralHeadPos;
            BytesRead := s.Read(CentralZipHeader, SizeOf(TCentral));

            If BytesRead <> SizeOf(TCentral) Then
            Begin
            	CentralHeadPos :=
               	EndHeadPos - Cardinal(EndZipHeader.SizeOfCentralDir);
            	s.Position := CentralHeadPos;
            	BytesRead := s.Read(CentralZipHeader, SizeOf(TCentral));
            	If BytesRead <> SizeOf(TCentral) Then break;
            End;

            IsHeader := VerSig(CentralZipHeader.SignAtr, htCentral, Encrypted) =
            	htCentral;

            If Not IsHeader Then
            Begin
               CentralHeadPos :=
                  EndHeadPos - Cardinal(EndZipHeader.SizeOfCentralDir);

            	s.Position := CentralHeadPos;
            	BytesRead := s.Read(CentralZipHeader, SizeOf(TCentral));
            	If BytesRead <> SizeOf(TCentral) Then break;

            	IsHeader :=
               	VerSig(CentralZipHeader.SignAtr, htCentral, Encrypted) =
                  htCentral;

            	If IsHeader Then
            		ZipSFX_OffsetAdjustment :=
            			CentralHeadPos - EndZipHeader.CentralDirOffset
               Else
            		ZipSFX_OffsetAdjustment := 0;

            End Else
            	ZipSFX_OffsetAdjustment := 0;

            If IsHeader Then
            Begin
               Include(HeaderTypeState, htCentral);
               Result := CentralHeadPos;

               If Encrypted Then
                  DecodeHeader(@CentralZipHeader, htCentral);

               s.Position :=
               	CentralZipHeader.RelativeOffsetOfLocalHeader +
                  ZipSFX_OffsetAdjustment;

               BytesRead := s.Read(LocalZipHeader, SizeOf(TLocal));
               If BytesRead <> SizeOf(TLocal) Then break;

               If (BytesRead = SizeOf(TLocal)) And
                  (VerSig(LocalZipHeader.SignAtr, htLocal, Encrypted) = htLocal) Then
                  Include(HeaderTypeState, htLocal);
            End
            Else
            Begin

               s.Position := 0;
               For i := 0 To 3 Do    //search upto 32k * 4 for local signature
               Begin
                  BytesRead := s.Read(Buffer[0], BufSize);
                  If (BytesRead = 0) Then
                     Exit;

                  Result := -1;
                  b := @Buffer[0];
                  For j := 0 To BytesRead Do
                  Begin
                     If b^ = 80 Then
                     Begin
                        pSignattr := @Buffer[j];
                        If VerSig(pSignattr^, htLocal, Encrypted) = htLocal Then
                        Begin
                           Result := 0 + (i * BufSize) + j;
                           HeaderTypeState := [htLocal];
                           break;
                        End;
                     End;
                     Inc(b);
                  End;
                  If (Result > -1) Then break;
               End;
            End;

            break;
         End;
         Dec(BytesRead);
      End;
   Finally
      FreeMem(Buffer);
   End;
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.SetArcType(ArcType: TArcType);
Var
	s: TFileStream32;
Begin
   If fArchiveFile <> '' Then
   Begin
      s := TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyWrite);
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
   End;
End;
//------------------------------------------------------------

Function TRegisterZipTV.OpenArchive(Var f: THandle; FileName: String): Boolean;
Begin
   Result := False;
   Try
      f := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, Nil,
         OPEN_EXISTING, faAnyFile, 0);

      Result := f <> INVALID_HANDLE_VALUE;
   Finally
      If Result Then
         fLOF := ztvGetFileSize(f)
      Else
         fLOF := 0;
   End;
End;
//-------------------------------------------------------------

Function TRegisterZipTV.doByteSearch(s: TStream32; Var FAT: TArcType;
	StartOffset: Cardinal): Cardinal;
Var
   b: ^Byte;
   Buffer: PChar;
   Signattr: ^Integer;
   Signattr64: ^Int64;
   i, j, BytesRead: Integer;
	pArjHeader: ^TARJHEADER;
Begin
   Result := 0;
   FAT := atNA;
   Try
      GetMem(Buffer, WSIZE);
      Try
         If (fLOF > StartOffset) Then
         Begin
            s.Position := StartOffset;
            For i := 0 To 10 Do
            Begin
               BytesRead := s.Read(Buffer[0], WSIZE);
               If BytesRead = 0 Then Exit;

               b := @Buffer[0];
               For j := 0 To BytesRead Do
               Begin
                  Case b^ Of
                     42:                (* atAce *)
                        If (Integer(StartOffset) + (i * WSIZE) + j + 5 < s.size) Then
                        Begin
                           If (Buffer[j + 1] = '*') And
                              (Buffer[j + 2] = 'A') And
                              (Buffer[j + 3] = 'C') And
                              (Buffer[j + 4] = 'E') Then
                           Begin
                              Result :=
                              	StartOffset + Cardinal((i * WSIZE) + (j - 7));

                              If Result > 0 Then
                              	FAT := atAceExe
                              Else
                              	FAT := atAce;
                              break;
                           End;
                        End;
                     77:                (* CabExe *)
                        If (StartOffset + Cardinal((i * WSIZE) + j + 5)) < fLOF Then
                     	Begin
                           Signattr64 := @Buffer[j];
                           If (Signattr64^ = CAB_SIGNATURE) Then
                           Begin
                           	If StartOffset > 0 Then
                              Begin
                              	Result := StartOffset + Cardinal((i * WSIZE) + j);
                                 If Result > 0 Then
                                    FAT := atCabExe
                                 Else
                                    FAT := atCab;
                              End;
                              Break;
                           End;
                        End;
                     45:                (* Lha / Lzh *)
                        If (StartOffset + Cardinal((i * WSIZE) + j + 5)) < fLOF Then
                        Begin
                           If (Buffer[j + 1] = 'l') And
                              (Buffer[j + 2] = 'h') And
                              (Buffer[j + 4] = '-') Then
                           Begin
                              Result := StartOffset + Cardinal((i * WSIZE) + j - 2);
                              If Result > 0 Then
                              	FAT := atLhaExe
                              Else
                              	FAT := atLha;
                              break;
                           End;
                        End;
                     66:                (* BlakHole *)
                        Begin
                           Signattr := @Buffer[j];
                           If (Signattr^ = BLAKHOLE_SIGNATURE) Then
                           Begin
                              Inc(Signattr);
                              If Signattr^ <> Integer($84C3940F) Then
                              Begin
                                 Result := StartOffset + Cardinal((i * WSIZE) + j);
                              	If Result > 0 Then
                                 	FAT := atBhExe
                                 Else
                                 	FAT := atBh;
                                 break;
                              End;
                           End;
                        End;
                     80:                (* Looking for an uppercase P in PkZIP *)
                        Begin
                           Signattr := @Buffer[j];
                           If (Signattr^ = LOCAL_FILE_HEADER_SIGNATURE) Then
                           Begin
                              Inc(Signattr);
                              If Signattr^ <> $3D0B74 Then
                              Begin
                              	If StartOffset > 0 Then
                                 	FAT := atZipExe
                                 Else
                                 	FAT := atZip;

                                 Result := GetCentralDirOffset(s);
                                 If Result = 0 Then
                                    Result := StartOffset + Cardinal((i * WSIZE) + j);

                                 break;
                              End;
                           End;
                        End;
                     82:                (* Looking for an uppercase R in RAR *)
                        Begin
                           Signattr := @Buffer[j];
                           If (Signattr^ = MAIN_RAR_HEADER_SIGNATURE) Then
                           Begin
                              Result := StartOffset + Cardinal((i * WSIZE) + j);
                              If Result > 0 Then
                              	FAT := atRarExe
                              Else
                              	FAT := atRar;
                              break;
                           End;
                        End;

                     96:         (* Arj *)
                      	Begin
                        	pArjHeader := @Buffer[j];
                           If pArjHeader^.HeadID = ARJ_SIGNATURE Then
                           Begin
                              If j + pArjHeader^.HdrSize + 10 < WSIZE Then
                              Begin
                              	pArjHeader := @Buffer[j + pArjHeader^.HdrSize + 10];
                                 If pArjHeader^.HeadID = ARJ_SIGNATURE Then
                                 Begin
                              		Result := StartOffset + Cardinal((i * WSIZE) + j);
                                    If Result > 0 Then
                                       FAT := atArjExe
                                    Else
                                       FAT := atArj;

                                    Break;
                                 End;
                              End;
                           End;
                        End;
                  End;
                  Inc(b);
               End;

               If Result > 0 Then
               	break;
            End;
         End;
      Finally
         FreeMem(Buffer);
      End;
   Except
   End;
End;

(* ***********************************************************
 Check offset:
     7612	: WinZip 16-bit SFX
     15770 : PkZip using Zip2Exe.exe
     25669 : WinZip 32-bit SFX
 ************************************************************* *)

Function TRegisterZipTV.IsSFXZipped(s: TStream32): TArcType;
Const
   SixteenBitPkZip = 15770;
   SixteenBitWinZip = 7612;
   ThirtyTwoBitWinZip = 25661;
Var
	Buffer: PChar;
   BytesRead: DWord;
   i,
   	Signattr: Integer;
Begin

   Result := atNA;

   s.Position := 6674;
   For i := 0 To 4 Do
   Begin

      Case i Of

         0:
            If s.Size > 6676 Then	// add 2 for read of sizeof(word) bytes
               s.Position := 6674
            Else
               Break;					// post loop file-positions are greater

         1:
            If s.Size > 6869 Then	// add 2 for read of sizeof(word) bytes
               s.Position := 6867
            Else
               Break;					// post loop file-positions are greater

         2:
            If s.Size > 7718 Then	// add 2 for read of sizeof(word) bytes
               s.Position := 7716
            Else
               Break;					// post loop file-positions are greater

         3:
            If s.Size > 8603 Then	// add 2 for read of sizeof(word) bytes
               s.Position := 8601
            Else
               Break;					// post loop file-positions are greater

         4:
            If s.Size > 13140 Then	// add 2 for read of sizeof(word) bytes
               s.Position := 13138
            Else
               Break;					// post loop file-positions are greater

      End;

      Signattr := 0;
      BytesRead := s.Read(Signattr, SizeOf(word));
      If (BytesRead = SizeOf(word)) Then
         If	(Signattr = $81A) Or    		// pkarc.exe (makesfx.com sfx-stub)
            (Signattr = $91A) Then  		//
         Begin
            fOffsetStart := s.Position - SizeOf(Word);
            Result := atArcExe;
            Exit;
         End Else
            If (Signattr = $a1a) Or    	// pak151.exe, pak210.exe
               (Signattr = $b1a) Then
            Begin
               fOffsetStart := s.Position - SizeOf(Word);
               Result := atPakExe;
               Exit;
            End;

   End;

   // lha213.exe
   If s.size >	1949 Then
   Begin
   	GetMem(Buffer, 6);
      Try
      	Signattr := 0;
         s.Position := 1947;
      	BytesRead := s.Read(Buffer[0], 6);
         If BytesRead = 6 Then
         	If (Buffer[0] = '-') And
         		(Buffer[1] = 'l') And
            	(Buffer[2] = 'h') And
               (Buffer[4] = '-') Then
            Begin
               fOffsetStart := 1945;
               Result := atLhaExe;
               Exit;
            End;
      Finally
      	FreeMem(Buffer);
      End;
   End;

   (* ********************** *)
   (* Winzip 6.3 16 bit SFX? *)
   (* ********************** *)
   If (s.size > SixteenBitWinZip + SizeOf(Integer)) Then
   Begin
      s.Position := SixteenBitWinZip;
      BytesRead := s.Read(Signattr, SizeOf(Integer));
      If (BytesRead = SizeOf(Integer)) And
         (Signattr = LOCAL_FILE_HEADER_SIGNATURE) Then
      Begin
         fOffsetStart := SixteenBitWinZip;
         Result := atZipExe;
         Exit;
      End;
   End;

   (* ********************** *)
   (* Winzip 6.3 32 bit SFX? *)
   (* ********************** *)
   If (s.size > ThirtyTwoBitWinZip + SizeOf(Integer)) Then
   Begin
      s.Position := ThirtyTwoBitWinZip;
      BytesRead := s.Read(Signattr, SizeOf(Integer));
      If (BytesRead = SizeOf(Integer)) And
         (Signattr = LOCAL_FILE_HEADER_SIGNATURE) Then
      Begin
         fOffsetStart := ThirtyTwoBitWinZip;
         Result := atZipExe;
         Exit;
      End;
   End;

   If (Result = atZipExe) Then
   Begin
      GetCentralDirOffset(s);
      HeaderTypeState := [htLocal];
   End Else
      Result := atNA;

End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.InitializeVolumeSet;
Var
   Ext: String;
   i: Byte;
Begin

   fVolNum := 0;
   Case ArcType Of
      atAce: fVolBegChar := Byte('C');
      atArj: fVolBegChar := Byte('a');
      atRar,
         atRarExe: fVolBegChar := Byte('q');
      atJar,
         atJarExe:
         	fVolBegChar := Byte('j');

   Else
      Exit;
   End;

   If fArchiveFile = '' Then Exit;

   Ext := ExtractFileExt(fArchiveFile);
   If Ext <> '' Then
      System.Delete(Ext, 1, 1)
   Else
      Case ArcType Of
         atAce: Ext := 'Ace';
         atArj: Ext := 'Arj';
         atRar,
            atRarExe: Ext := 'Rar';
         atZip,
         	atZipExe: Ext := 'Zip';
         atJar,
         	atJarExe: Ext := 'Jar';
      End;

   fVolNum := -1;
   For i := Length(Ext) Downto 1 Do
      If (Byte(Ext[i]) >= 48) And (Byte(Ext[i]) <= 57) Then
      Begin
         fVolNum := StrToInt(StrPas(@Ext[i]));
         If i = Length(Ext) Then fVolBegChar := Byte(Ext[1]);
      End;

   If fVolNum = -1 Then
   Begin
      If (ArcType In [atArj, atArjExe]) Then
         fVolNum := 1
      Else
         fVolNum := 0;
   End Else
      Inc(fVolNum);

End;
//------------------------------------------------------------

Function TRegisterZipTV.DefSig(HT: THeaderType; Encrypted: Boolean): Integer;
Begin
   Case HT Of

      htLocal:
         If Encrypted Then
         Begin
            If ZipCompatible Then
               Result := LOCAL_FILE_ENCRPT_SIGNATURE
            Else
               Result := LOCAL_CUST_ENCRPT_SIGNATURE;
         End
         Else
         Begin
            If ZipCompatible Then
               Result := LOCAL_FILE_HEADER_SIGNATURE
            Else
               Result := LOCAL_CUST_HEADER_SIGNATURE;
         End;

      htCentral:
         If Encrypted Then
         Begin
            If ZipCompatible Then
               Result := CENTRAL_FILE_ENCRPT_SIGNATURE
            Else
               Result := CENTRAL_CUST_ENCRPT_SIGNATURE
         End
         Else
         Begin
            If ZipCompatible Then
               Result := CENTRAL_FILE_HEADER_SIGNATURE
            Else
               Result := CENTRAL_CUST_HEADER_SIGNATURE;
         End;

      htEnding:
         Result := END_OF_CENTRAL_HEADER_SIGNATURE;
   Else
      Result := MULTIVOL_HEADER_SIGNATURE;
   End;
End;
//------------------------------------------------------------

Function TRegisterZipTV.VerSig(Sig: Integer; HT: THeaderType;
   Var Encrypted: Boolean): THeaderType;
Begin
   Result := htNone;
   Encrypted := False;

   Case HT Of
      htLocal:
         Begin
            Case Sig Of
               LOCAL_FILE_HEADER_SIGNATURE: ; // do not remove
               LOCAL_FILE_ENCRPT_SIGNATURE: Encrypted := True;
               LOCAL_CUST_HEADER_SIGNATURE: ; // do not remove
               LOCAL_CUST_ENCRPT_SIGNATURE: Encrypted := True;
            Else
               Exit;
            End;
            Result := htLocal;
         End;
      htCentral:
         Begin
            Case Sig Of
               CENTRAL_FILE_HEADER_SIGNATURE: ; // do not remove
               CENTRAL_FILE_ENCRPT_SIGNATURE: Encrypted := True;
               CENTRAL_CUST_HEADER_SIGNATURE: ; // do not remove
               CENTRAL_CUST_ENCRPT_SIGNATURE: Encrypted := True;
            Else
               Exit;
            End;
            Result := htCentral;
         End;
      htEnding:
         Begin
            Case Sig Of
               END_OF_CENTRAL_ENCRPT_SIGNATURE: Encrypted := True;
               END_OF_CENTRAL_HEADER_SIGNATURE: ; 		// do not remove
               END_OF_CENTRAL64_ENCRPT_SIGNATURE: Encrypted := True;
               END_OF_CENTRAL64_HEADER_SIGNATURE: ; 	// do not remove
            Else
               Exit;
            End;
            Result := htEnding;
         End;
   End;
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.DecodeFilename(p: Pointer; Len: Integer);
Begin
   ztvDecodeBuf(p, Len);
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.DecodeComment(p: Pointer; Len: Integer);
Begin
   ztvDecodeBuf(p, Len);
End;
//-------------------------------------------------------------

Function GetCryptHeadLen(p: Pointer; HType: THeaderType): Integer;
Begin
   Case HType Of
      htLocal: Result := SizeOf(TLocal);
      htCentral: Result := SizeOf(TCentral);
      htEnding:
      	Begin
         	If (TEnd(p^).SignAtr = END_OF_CENTRAL_HEADER_SIGNATURE) Or
         		(TEnd(p^).SignAtr = END_OF_CENTRAL_ENCRPT_SIGNATURE) Then
         		Result := SizeOf(TEnd)
            Else
         		Result := SizeOf(TZipTV_End64);
         End;
   Else
      Result := 0;
      Exit;
   End;
   Dec(Result, SizeOf(Integer));
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.DecodeHeader(p: Pointer; HType: THeaderType);
Begin
   k := HKeyed;
   p := Pointer(ptr2int(p) + SizeOf(Integer));
   ztvDecodeBuf(p, GetCryptHeadLen(p, HType));
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.EncodeFilename(p: Pointer; Len: Integer);
Begin
   ztvEncodeBuf(p, Len);
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.EncodeComment(p: Pointer; Len: Integer);
Begin
   ztvEncodeBuf(p, Len);
End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.EncodeHeader(p: Pointer; HType: THeaderType);
Begin
   k := HKeyed;
   p := Pointer(ptr2int(p) + SizeOf(Integer));
   ztvEncodeBuf(p, GetCryptHeadLen(p, HType));
End;
//-------------------------------------------------------------

Function TRegisterZipTV.GetArcType(s: TStream32): TArcType;

Type
   Header = Packed Record
      HeadId: word;                     { 60000 }
      SIG1: word;                       { Basic Header Size }
   End;

   ImageInfo = Packed Record
      ExeId: Array[0..1] Of char;
      Remainder,
         Size: word
   End;

Var
   H: Header;
   p, C: PChar;
   Buffer: PChar;
   hLib: THandle;
   BytesRead: DWord;
   IInfo: ImageInfo;
   Signattr: ^Integer;
   DiskManager: TDiskManager;
   i, code, aOffset: Integer;

   Function GetZipVolumeStatus: TArcType;
   Begin
   	DiskManager := TDiskManager.Create();
      With DiskManager Do
      Try
         If GetDriveInfo(fArchiveFile, Nil, Nil, Nil, Nil) Then
         Begin
            If (DriveType = dtFloppy) And
            	(UpperCase(ztvGbls.GetToken(DiskLabel,
            	' ')) = TrimRight(PKBACK)) Then
               Result := atZipDS
            Else
               Result := atZipMV;
      	End Else
         	Result := atNA;

      Finally
         DiskManager.Free();
      End;
   End;

   Function TAT(method: Byte; Signattr: Integer): TArcType;
   Begin

      Result := atNA;

      Case Signattr Of
         DCP_SIGNATURE:
            Result := atUnknown;

         CAB_SIGNATURE:
            Case method Of
               0: Result := atCabExe;
               1: Result := atCab;
            End;

         BLAKHOLE_SIGNATURE:
            Case method Of
               0: Result := atBhExe;
               1: Result := atBh;
            End;

         MS_GZ_HEADER_SIGNATURE:
            Case method Of
               0: Result := atUnsupported;
               1: Result := atMsGZ;
            End;

         MAIN_RAR_HEADER_SIGNATURE:
            Case method Of
               0: Result := atRarExe;
               1: Result := atRar;
            End;

         $5754DFB7:
            Case method Of
               0:
                  Begin
                     Result := atArcExe;
                     fOffsetStart := 7716;
                  End;
               1: Result := atArc;
            End;

      Else
         Case HiWord(Signattr) Of
            LHA_SIGNATURE:
               Case method Of
                  0: Result := atLhaExe;
                  1: Result := atLha;
               End;
         Else
            Case LoWord(Signattr) Of
               $4B50, $4C50, $4D50, $4E50:
                  Case HiWord(Signattr) Of
                     $807:              // Multivolume Zip Header Signature
                     	Begin
                        	Result := GetZipVolumeStatus();
                        End;

                     $201:              // CentralZipHeader found on beginning of last disk
                        Begin
                        	Result := GetZipVolumeStatus();
                           GetCentralDirOffset(s);
                        End;
                     $3447: ;           // do nothing... could be a .dcp file.
                     $4943: ;           // do nothing... could be an imate.dat file.
                  Else
                  	Begin
                        Case method Of
                           0: Result := atZipExe;
                           1: Result := atZip;
                        End;
                     End;
                  End;

               $4148:
                  Case method Of
                     0: Result := atUnsupported;
                     1: Result := atHA;
                  End;

               $8B1F,                   // GZip 1.2.4
                  $9D1F,                // GZip LZW
                  $9E1F,                // Old_GZip
                  $1E1F,                // GZip Pack
                  8485,                 // GZip Pack
                  $A01F:                // GZip LZH
                  Case method Of
                     0: Result := atUnsupported;
                     1: Result := atGZip;
                  End;

               ARJ_SIGNATURE:
                  Case method Of
                     0: Result := atArjExe;
                     1: Result := atArj;
                  End;

               ZOO_SIGNATURE:
                  Case method Of
                     0: Result := atUnsupported;
                     1: Result := atZoo;
                  End;

               $A1A,                    // ver 1.51, 2.10, 2.51
                  $B1A:
                  Case method Of
                     0: Result := atPakExe;
                     1: Result := atPak;
                  End;

               $1E1A,                   // Arc602 with stored paths
                  538:
                  Case method Of
                     0: Result := atUnsupported;
                     1: Result := atArc;
                  End;

               $081A,                   // pkarc36
                  $091A:
                  Case method Of
                     0: Result := atArcExe;
                     1: Result := atArc;
                  End;

               $92B7,                   // Arc602.exe only
                  $DFB7:
                  Begin
                     Result := atArcExe;
                     fOffsetStart := 7716;
                  End;

            End;                        { Case LoWord( Signattr ) Of }
         End;
      End;                              { Case Signattr Of }
   End;

   Function TestUUE(s: TStream32): TArcType;
   Const
      MAXBYTES = 5000;
   Var
      i: Integer;
      Buffer: PChar;
      InBegin: Boolean;
      BufSize,
      	BytesRead: Integer;
   Begin

      Result := atNA;
      If (s.size < MAXBYTES) Then
         BufSize := s.size
      Else
         BufSize := MAXBYTES;

      Try
         GetMem(Buffer, BufSize);
         Try

            s.Position := 0;
            BytesRead := s.Read(Buffer[0], BufSize);
            If BytesRead = 0 Then
               Exit;

            i := 0;
            InBegin := False;

            While (i < BytesRead) Do
            Begin

               If ((i + 2) < BytesRead) Then
               Begin
                  If (i = 0) Or ((i > 0) And (Buffer[i - 1] = #10)) Then
                  Begin
                     If (StrLComp(Buffer + i, 'begin 6', 7) = 0) Or
                        (StrLComp(Buffer + i, 'begin 7', 7) = 0) Then
                     Begin
                        Result := atUUE;
                        Exit;
                     End Else
                     	If (StrLComp(Buffer + i, 'begin', 5) = 0) Then
                        	InBegin := True
                        Else If InBegin Then
                     		If (StrLComp(Buffer + i, 'M', 1) = 0) Then
                           Begin
                           	Result := atUUE;
                              Exit;
                           End;
                  End;
               End Else
                  Exit;

               Inc(i);
            End;
         Finally
            FreeMem(Buffer, BufSize);
         End;
      Except
      End;
   End;

   Function TestTAR(s: TStream32): TArcType;
   Type
      HdrArray = Array[0..SizeOf(TTarHeader)] Of Byte;
   Const
      LF_OLDNORMAL = #0; 	// Normal disk FILE, Unix compat
      LF_NORMAL = '0'; 		// Normal disk FILE
      LF_LINK = '1'; 		// Link to previously dumped FILE
      LF_SYMLINK = '2'; 	// Symbolic link
      LF_CHR = '3'; 			// Character special FILE
      LF_BLK = '4'; 			// Block special FILE
      LF_DIR = '5'; 			// Directory
      LF_FIFO = '6'; 		// FIFO special FILE
      LF_CONTIG = '7'; 		// Contiguous FILE
      LF_VOL = 'V'; 			// First record is volume label
   Var
      BytesRead: DWord;
      i, CheckSum: Integer;
      OrigCheckSum: Integer;
      HeaderPtr: ^HdrArray;
   Begin
      Result := atNA;

      If SizeOf(TarHeader) < s.size Then
      Begin
         s.Position := 0;
         BytesRead := s.Read(TarHeader, SizeOf(TTarHeader));
         If (BytesRead = 0) Then Exit;

         If (TarHeader.LinkFlag = LF_VOL) Then
         Begin
            fOffsetStart := s.Position;
            If ((fOffsetStart + SizeOf(TTarHeader)) < s.size) Then
            Begin
               If (s.Read(TarHeader, SizeOf(TTarHeader)) <> SizeOf(TTarHeader)) Then
                  Exit;
            End Else
               Exit;
         End;

         OrigCheckSum := OctStrToInt(TarHeader.ChkSum);
         CheckSum := 0;
         HeaderPtr := @TarHeader;

         For i := 0 To SizeOf(TarHeader) - 1 Do
            If (Integer(@HeaderPtr^[i]) < Integer(@TarHeader.ChkSum)) Or
               (Integer(@HeaderPtr^[i]) >= (Integer(@TarHeader.ChkSum) +
               SizeOf(TarHeader.ChkSum))) Then
               Inc(CheckSum, HeaderPtr^[i])
            Else
               Inc(CheckSum, ord(' '));

         If CheckSum = OrigCheckSum Then
            Result := atTar
         Else
            fOffsetStart := -1;
      End;
   End;

   Function IsArcValid(AT: TArcType): Boolean;
   Begin
      Result := Not (AT In Invalid_ArcType);
   End;

Const
   BHSFX = 1;
   ZIPSFX = 2;
   ARJSFX = 3;
   LHASFX = 4;
   JARSFX = 5;
   ACESFX = 6;
   RARSFX = 7;
   EXE_START_SEARCH_OFFSET = 17000;     (* Some rar archives start at pos 17721 *)

Var
   fOffsetBegin: Int64;

Begin	{GetArcType}

   Result := atNA;
   If s = Nil Then Exit;

   Try
      ArchiveCommentPos := 0;
		ZipSFX_OffsetAdjustment := 0;

      fOffsetStart := -1;
      HeaderTypeState := [];
      fLOF := s.size;

      Try
         If (s.size > 19) Then
         Begin

            GetMem(C, WSIZE);
            Try
               s.Position := 0;
               BytesRead := s.Read(C[0], WSIZE - 1);

               Signattr := @C[0];
               If LoWord(Signattr^) = $5A4D Then
               Begin

                  Result := IsSFXZipped(s);
                  If IsArcValid(Result) Then
                     Exit;

                  hLib := LoadLibraryEx(PChar(fArchiveFile), 0,
                  	LOAD_LIBRARY_AS_DATAFILE);

                  If hLib <> 0 Then
                  Begin

                     GetMem(Buffer, 12);
                     Try

                        For i := BHSFX To RARSFX Do
                        Begin

                           LoadString(hLib, i, Buffer, 6);
                           If Strlen(Buffer) > 0 Then
                           Begin

                              Try
                                 Val(Buffer, fOffsetStart, code);
                                 If code <> 0 Then break;
                              Except
                                 break;
                              End;

                              If fOffsetStart >= s.size Then break;
                              s.Position := fOffsetStart;

                              Try
                                 BytesRead := s.Read(C^, 14);
                                 If StrLComp(@C[7], '**ACE**', 7) = 0 Then
                                 Begin
                                    Result := atAceExe;
                                    Exit;
                                 End;

                                 If BytesRead = 14 Then
                                 Begin
                                    Signattr := @C[0];
                                    Result := TAT(0, Signattr^);

                                    (* Found archive type *)
                                    If (Result <> atNA) Then
                                    Begin
                                       If (Result In Zipped_ArcType) Then
                                       Begin
                                          fOffsetBegin := fOffsetStart;
                                          fOffsetStart :=
                                             GetCentralDirOffset(s);

                                          If HeaderTypeState = [] Then
                                             fOffsetStart := fOffsetBegin;
                                       End;
                                       Exit;
                                    End;
                                 End;

                              Except
                              End;

                           End
                           Else
                              break; (* Not a resource String created SFX *)

                        End;
                     Finally
                        FreeMem(Buffer, 12);
                        FreeLibrary(hLib);
                     End;
                  End;

                  CopyMem(@C, @IInfo, 6);

                  aOffset := Integer(IInfo.size - 1) * 512 + IInfo.Remainder;
                  If (aOffset > 0) And (aOffset + SizeOf(H) < s.size) Then
                  Begin

                     s.Position := aOffset;
                     BytesRead := s.Read(H, SizeOf(H));

                     If BytesRead = SizeOf(H) Then
                     Begin

                        { add 2 bytes for ARJ241}
                        fOffsetStart := aOffset + (ord(Boolean(H.SIG1 = ARJ_SIGNATURE)) * 2);
                        If (fOffsetStart + SizeOf(Integer)) < fLOF Then
                        Begin
                           s.Position := fOffsetStart;
                           BytesRead := s.Read(C^, SizeOf(Integer));
                           Signattr := @C[0];

                           If (BytesRead = SizeOf(Integer)) And
                              (Signattr^ <> LOCAL_FILE_HEADER_SIGNATURE) Then
                           Begin
                              Result := TAT(0, Signattr^);
                              If IsArcValid(Result) Then
                                 Exit;
                           End;

                        End;
                     End;
                  End;

                  EndZipHeader.SignAtr := 0;
                  If Not IsArcValid(Result) Then
                  Begin
                     fOffsetStart :=
                        GetCentralDirOffset(s);

                     If fOffsetStart > 0 Then
                        Result := atZipExe;
                  End;

                  If Not IsArcValid(Result) Then
                     fOffsetStart :=
                        doByteSearch(s, Result, EXE_START_SEARCH_OFFSET);

               End
               Else
               Begin

                 (* Some SFXs are incorrectly converted back to their		*)
                 (* original archive formats.  The majority of these      *)
                 (* coverted SFXs contain spaces at the frontend of the   *)
                 (* archive.  Adjust for these files with the p pointer.  *)
                  p := C;
                  If C[0] = #0 Then
                  Begin

                     For i := 0 To BytesRead - 1 Do
                        If p^ = #0 Then
                           Inc(p)
                        Else
                           break;

                     // for next block example see: 3\lha\japan\siin108d.lzh
                     BytesRead := BytesRead + DWord(c - p);
                     If BytesRead > 128 + SizeOf(SignAttr) Then
                     Begin
                        SignAttr := @p[128 + (c - p)];
                        If HiWord(Signattr^) = LHA_SIGNATURE Then
                        Begin
                           Result := atLha;
                           fOffsetStart := 128;
                           Exit;
                        End;
                     End;

                     Signattr := @p[0];
                  End;

                  Result := TAT(1, Signattr^);

                  Case Result Of
                     atNA:
                        Begin
                           If fOffsetStart = 0 Then
                              fOffsetStart := doByteSearch(s, Result, 0);
                        End;
                     atUnknown,
                        atFileOpenErr: Exit;

                     atZipDS..atZipMV:
                        Begin
                           fOffsetStart :=
                              GetCentralDirOffset(s);

                           If HeaderTypeState = [] Then
                              HeaderTypeState := [htLocal];
                        End;

                     atZip..atZipExe,
                        atJar..atJarExe:
                        Begin
                           fOffsetStart :=
                              GetCentralDirOffset(s);

                           If (Not (htLocal In HeaderTypeState)) And
                           	(fOffsetStart = 0) Then
                           Begin
                              Inc(fOffsetStart, p - C);
                              Include(HeaderTypeState, htLocal);
                           End;
                        End;
                  Else
                     If StrLComp(@C[7], '**ACE**', 7) = 0 Then
                        Result := atAce;
                  End;

                  If Not IsArcValid(Result) Then
                  Begin
                     If StrLComp(@C[7], '**ACE**', 7) = 0 Then
                        Result := atAce
                     Else Begin
                        Result := TestUUE(s);
                        If (Result <> atUUE) Then
                        Begin
                           Result := TestTAR(s);
                           If (Result <> atTar) Then
                           Begin
                              fOffsetStart :=
                                 GetCentralDirOffset(s);

                              If fOffsetStart > 0 Then
                              Begin
                                 If EndZipHeader.NumberOfThisDisk = 0 Then
                                    Result := atZip
                                 Else
                                    Result := GetZipVolumeStatus()
                              End Else
                                 (* See \3\arj\bug.arj *)
                                 fOffsetStart :=
                                    doByteSearch(s, Result, 0);
                           End;
                        End;
                     End;
                  End;
               End;
            Finally
               FreeMem(C, WSIZE);

               If IsArcValid(Result) Then
               Begin
                  If (fOffsetStart = -1) Then
                     fOffsetStart := 0;

                  If Result = atZip Then
                     If LowerCase(ExtractFileExt(fArchiveFile)) = '.jar' Then
                        Result := atJar;
               End;
            End;
         End;
      Finally
      	Try
         	fArchiveDate := ztvGetFileDate(TFileStream32(s).Handle, dtCreate);
         	If fArchiveDate = -1 Then fArchiveDate := 2162720;
         Except
         	fArchiveDate := 2162720;
         End;
      End;
   Except
   End;

End;
//-------------------------------------------------------------

Procedure TRegisterZipTV.WEI(strm: TStream32);
Var
   e: Pointer;
   l: Integer;
Begin
	If ExtraFieldLen = 0 Then
   	Exit;

   l := SizeOf(fEI);
   GetMem(e, l);
   Try
      fEI.a[0] := ptr(BLAKHOLE_SIGNATURE);
      k := HKeyed;
      ztvEncodeBuf(pEI, SizeOf(fEI));
      strm.Write(e^, l);
   Finally
      FreeMem(e, l);
   End;
End;
//------------------------------------------------------------

Function vKey: Integer;
Begin
   CopyMem(p, @fEI.a[1], SizeOf(fEI.a) - SizeOf(Pointer));
   Result := 0;
End;
//-------------------------------------------------------------

Function IsLeapYear(Year: word): Boolean;
Begin
   Result := (Year Mod 4 = 0) And ((Year Mod 100 <> 0) Or (Year Mod 400 = 0));
End;
//-------------------------------------------------------------

Type
   PDayTable = ^TDayTable;
   TDayTable = Array[1..12] Of word;

Function DaylightSavings(dt: TDateTime): Boolean;

   Function IncMonth(Const Date: TDateTime; NumberOfMonths: Integer): TDateTime;
   Const
      MonthDays: Array[Boolean] Of TDayTable =
      ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
         (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));
   Var
      DayTable: PDayTable;
      Year, Month, Day: word;
      Sign: Integer;
   Begin
      If NumberOfMonths >= 0 Then
         Sign := 1
      Else
         Sign := -1;
      DecodeDate(Date, Year, Month, Day);
      Year := Year + (NumberOfMonths Div 12);
      NumberOfMonths := NumberOfMonths Mod 12;
      Inc(Month, NumberOfMonths);
      If word(Month - 1) > 11 Then
      Begin
         Inc(Year, Sign);
         Inc(Month, -12 * Sign);
      End;
      DayTable := @MonthDays[IsLeapYear(Year)];
      If Day > DayTable^[Month] Then Day := DayTable^[Month];
      Result := EncodeDate(Year, Month, Day) + Frac(Date);
   End;
Var
   d, m, y, WeekNo: word;
   DTBegins, STBegins: TDateTime;
Begin
   DecodeDate(dt, y, m, d);

   If (TZInfo.DaylightDate.wMonth = 0) Then
      Result := False
   Else
   Begin
      If (TZInfo.StandardDate.wYear = 0) Then
      Begin
         DTBegins := EncodeDate(y, TZInfo.DaylightDate.wMonth, 1);
         Case TZInfo.DaylightDate.wDay Of
            1, 2, 3,
               4:
               Begin
                  While (SysUtils.DayOfWeek(DTBegins) - 1) <>
                     TZInfo.DaylightDate.wDayOfWeek Do
                     DTBegins := DTBegins + 1;
                  WeekNo := 1;
                  If TZInfo.DaylightDate.wDay <> 1 Then
                     Repeat
                        DTBegins := DTBegins + 7;
                        Inc(WeekNo);
                     Until (WeekNo = TZInfo.DaylightDate.wDay);

                  With TZInfo.DaylightDate Do
                     DTBegins := DTBegins + EncodeTime(wHour, wMinute, 0, 0);
               End;
            5:
               Begin
                  DTBegins := IncMonth(DTBegins, 1);
                  DTBegins := DTBegins - 1;
                  While SysUtils.DayOfWeek(DTBegins) - 1 <>
                     TZInfo.DaylightDate.wDayOfWeek Do
                     DTBegins := DTBegins - 1;

                  With TZInfo.DaylightDate Do
                     DTBegins := DTBegins + EncodeTime(wHour, wMinute, 0, 0);
               End;
         End;

         STBegins := EncodeDate(y, TZInfo.StandardDate.wMonth, 1);
         Case TZInfo.StandardDate.wDay Of
            1, 2, 3,
               4:
               Begin
                  While (SysUtils.DayOfWeek(STBegins) - 1) <>
                     TZInfo.StandardDate.wDayOfWeek Do
                     STBegins := STBegins + 1;
                  WeekNo := 1;
                  If TZInfo.StandardDate.wDay <> 1 Then
                     Repeat
                        STBegins := STBegins + 7;
                        Inc(WeekNo);
                     Until (WeekNo = TZInfo.StandardDate.wDay);

                  With TZInfo.StandardDate Do
                     STBegins := STBegins + EncodeTime(wHour, wMinute, 0, 0);
               End;
            5:
               Begin
                  STBegins := IncMonth(STBegins, 1);
                  STBegins := STBegins - 1;
                  While SysUtils.DayOfWeek(STBegins) - 1 <>
                     TZInfo.StandardDate.wDayOfWeek Do
                     STBegins := STBegins - 1;

                  With TZInfo.StandardDate Do
                     STBegins := STBegins + EncodeTime(wHour, wMinute, 0, 0);
               End;
         End;
      End
      Else
      Begin
         With TZInfo.DaylightDate Do
         Begin
            DTBegins := EncodeDate(wYear, wMonth, wDay);
            DTBegins := DTBegins + EncodeTime(wHour, wMinute, 0, 0);
         End;
         With TZInfo.StandardDate Do
         Begin
            STBegins := EncodeDate(wYear, wMonth, wDay);
            STBegins := STBegins + EncodeTime(wHour, wMinute, 0, 0);
         End;
      End;

      If (TZInfo.DaylightDate.wMonth < TZInfo.StandardDate.wMonth) Then
         Result := (dt >= DTBegins) And (dt < STBegins)
      Else
         Result := (dt < STBegins) Or (dt >= DTBegins);
   End;
End;
//-------------------------------------------------------------

Function LocalTimeToUniversal(LT: TDateTime): TDateTime;
Var
   TZOffset: Integer;
Begin
   If DaylightSavings(LT) Then
      TZOffset := TZInfo.Bias + TZInfo.DaylightBias
   Else
      TZOffset := TZInfo.Bias + TZInfo.StandardBias;

   If (TZOffset > 0) Then
      Result := LT + EncodeTime(TZOffset Div 60, TZOffset Mod 60, 0, 0)
   Else If (TZOffset = 0) Then
   	Result := LT
   Else If (TZOffset < 0) Then
      Result := LT - EncodeTime(Abs(TZOffset) Div 60, Abs(TZOffset) Mod 60, 0, 0)
   Else
      Result := LT;

End;
//-------------------------------------------------------------

Function UniversalTimeToLocal(UT: TDateTime): TDateTime;
Var
   TZOffset: Integer;
Begin

   { Value returned by GetTimeZoneInformation identifies time zone setting  }
   { for system date and time; provided here for informational purposes     }

   { Determine offset in effect for DateTime UT: }
   If DaylightSavings(UT) Then
      TZOffset := TZInfo.Bias + TZInfo.DaylightBias
   Else
      TZOffset := TZInfo.Bias + TZInfo.StandardBias;

   If (TZOffset > 0) Then               { Time Zones west of Greenwich }
      Result := UT - EncodeTime(TZOffset Div 60, TZOffset Mod 60, 0, 0)
   Else If (TZOffset = 0) Then          { Time Zone = Greenwich }
   	Result := UT
   Else If (TZOffset < 0) Then          { Time Zones east of Greenwich }
      Result := UT + EncodeTime(Abs(TZOffset) Div 60, Abs(TZOffset) Mod 60, 0, 0)
   Else
      Result := UT;

End;
//------------------------------------------------------------

Function DosDateToUnix(tm: TDateTime): Integer;
Type
   PDBTable = ^TDBTable;
   TDBTable = Array[1..12] Of word;
Const
   DBTable: Array[Boolean] Of TDBTable =
   ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
      (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));
Var
   Year, Month, Day, Hour, Min, Sec, MSec, DayCount: word;
   pDaysBetweenTable: PDBTable;
Begin
   tm := LocalTimeToUniversal(tm);

   DecodeTime(tm, Hour, Min, Sec, MSec);
   DecodeDate(tm, Year, Month, Day);

   pDaysBetweenTable := @DBTable[IsLeapYear(Year)];

   DayCount := 365 * (Year - 1970) +
      ((Year - 1969) Div 4) +
      pDaysBetweenTable^[Month] +
      Day;

   Dec(DayCount);
   Result := (DayCount * 24 * 60 * 60 + Hour * 60 * 60 + Min * 60 + Sec);
End;
//-------------------------------------------------------------

Function UnixDateToDos(UnixDate: Integer): TDateTime;
Const
   Secs_Per_Year = 31536000;
   Secs_Per_Leap_Year = 31622400;
   Secs_Per_Day = 86400;
   Secs_Per_Hour = 3600;
   Secs_Per_Minute = 60;

Const
   Days_Per_Month: Array[1..12] Of word =
   (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

Var
   RDate, t: Integer;
   Year, Month, Day, Hour, Mins, Secs: word;

Begin
   If UnixDate = 0 Then
   Begin
      Result := EncodeDate(1980, 1, 1) + EncodeTime(0, 1, 0, 0);
      Exit;
   End;

   RDate := Abs(UnixDate);

   Year := 1970;
   Month := 1;
   t := 0;

   While (RDate > 0) Do
   Begin
      If (Year Mod 4) = 0 Then
         t := Secs_Per_Leap_Year
      Else
         t := Secs_Per_Year;

      RDate := RDate - t;
      Inc(Year);
   End;

   RDate := RDate + t;
   Dec(Year);

   If (Year Mod 4) = 0 Then
      Day := 29
   Else
      Day := 28;

   CopyMem(@Day, @Days_Per_Month[2], SizeOf(word));

   While (RDate > 0) Do
   Begin
      t := Integer(Days_Per_Month[Month]) * Integer(Secs_Per_Day);
      RDate := RDate - t;
      Inc(Month);
   End;

   RDate := RDate + t;
   Dec(Month);

   Day := Trunc(int((RDate + Pred(Secs_Per_Day)) / Secs_Per_Day));
   RDate := RDate - Pred(Day) * Secs_Per_Day;
   Hour := Trunc(int(RDate / Secs_Per_Hour));
   RDate := RDate - Integer(Hour) * Secs_Per_Hour;

   Mins := Trunc(int(RDate / Secs_Per_Minute));
   Secs := Trunc(RDate - Integer(Mins) * Secs_Per_Minute);

   If Secs > 59 Then
      Secs := 59;

   If Hour = 24 Then
   Begin
      If (Day + 1 > Days_Per_Month[Month]) Then
      Begin
         If (Month = 12) Then
         Begin
            Inc(Year);
            Month := 1;
         End
         Else
            Inc(Month);

         Day := 1;
      End
      Else
         Inc(Day);

      Hour := 0;
      Mins := 0;
      Secs := 1;
   End;

   Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Mins, Secs, 0);
   Result := UniversalTimeToLocal(Result);

End;
//-------------------------------------------------------------

Function OctStrToInt(s: String): Integer;
Var
   i: Integer;
Begin
   Result := 0;
   If (s <> '') Then
      For i := 1 To Length(s) Do
         Case s[i] Of
            '0'..'7': Result := Result * 8 + ord(s[i]) - ord('0');
         End;
End;
//-------------------------------------------------------------

Procedure IntToOctStr(value, digs: Integer; StrTerminate: Boolean; Where: PChar);
Begin
   FillChar(Where[0], digs, ord('0'));
   Where[digs - 1] := ' ';
   If digs = 0 Then Exit;

   If StrTerminate Then
   Begin
      Dec(digs);
      Where[digs] := #0;
   End
   Else
      Dec(digs);

   Repeat
      Where[digs - 1] := char(Byte('0') + (value And 7));
      Dec(digs);
      value := value Shr 3;
   Until (digs <= 0) Or (value = 0);
End;
//-------------------------------------------------------------

// IMPORTANT: Do not delete the buffer you GlobalAlloc().  Once you
// put it into the clipboard, it's up to the clipboard to dispose
// of it.  When retrieving it, do not delete the buffer you
// retrieve -- just make a copy of the contents.
Function CopyStreamFromClipboard(Stream: TMemoryStream32): Boolean;
Var
   hbuf: THandle;
   bufptr: Pointer;
Begin
   Result := False;
   Clipboard.Open();
   Try
      hbuf := Clipboard.GetAsHandle(CF_COMPRESSED_DATA);
      If (hbuf <> 0) Then
      Begin
         bufptr := GlobalLock(hbuf);
         If (bufptr <> Nil) Then
         Begin
            Try
               If (Stream.Write(bufptr^, GlobalSize(hbuf)) = 0) Then
                  Exit;

               Stream.Position := 0;
               Result := True;
            Finally
               GlobalUnlock(hbuf);
            End;
         End;
      End;
   Finally
      Clipboard.Close();
   End;
End;

//------------------------------------------------------------

Procedure RegisterCBClass;
Var
   RegClassName: String;
Begin
   RegClassName := 'ZipTV_CompressedData';
   CF_COMPRESSED_DATA := RegisterClipboardFormat(PChar(RegClassName));
End;
//-------------------------------------------------------------

// IMPORTANT: Do not delete the buffer you GlobalAlloc().  Once you
// put it into the clipboard, it's up to the clipboard to dispose
// of it.  When retrieving it, do not delete the buffer you
// retrieve -- just make a copy of the contents.
Function CopyStreamToClipboard(Stream: TMemoryStream32; Len: Integer): Boolean;
Var
   hbuf: THandle;
   bufptr: Pointer;
Begin
   Result := True;
   RegisterCBClass();
   hbuf := GlobalAlloc(GMEM_MOVEABLE, Len);
   Try
      bufptr := GlobalLock(hbuf);
      Try
         CopyMem(Stream.memory, bufptr, Len);
         Clipboard.SetAsHandle(CF_COMPRESSED_DATA, hbuf);
      Finally
         GlobalUnlock(hbuf);
      End;
   Except
      Result := False;
      GlobalFree(hbuf);
   End;
End;
//------------------------------------------------------------

Procedure DecodeRarFN(Name: pChar; EncName: PChar; EncSize: Integer; NameW:
   PWideChar; MaxDecSize: Integer);
Var
   Len,
      EncPos,
      DecPos,
   	FlagBits,
      F: Integer;
   Flags,
      HighByte,
      Correction: Byte;
Begin
   EncPos := 0;
   DecPos := 0;

   HighByte := Byte(EncName[EncPos]);
   Inc(EncPos);
   FlagBits := 0;

   While ((EncPos < EncSize) And (DecPos < MaxDecSize)) Do
   Begin
      If FlagBits = 0 Then
      Begin
         Flags := Byte(EncName[EncPos]);
         Inc(EncPos);
         FlagBits := 8;
      End;

      F := (Flags Shr 6);
      Case F Of
         0:
            Begin
               NameW[DecPos] := WideChar(EncName[EncPos]);
               Inc(DecPos);
               Inc(EncPos);
            End;
         1:
            Begin
               NameW[DecPos] := WideChar(Byte(EncName[EncPos]) + (HighByte Shl 8));
               Inc(DecPos);
               Inc(EncPos);
            End;
         2:
            Begin
               NameW[DecPos] := WideChar(Byte(EncName[EncPos]) + Byte(EncName[EncPos+1]) Shl 8);
               Inc(DecPos);
               Inc(EncPos, 2);
            End;
         3:
            Begin
            	Len := Byte(EncName[EncPos]);
               Inc(EncPos);

               If (Len And $80) > 0 Then
               Begin
               	// v6.3: haven't found a .rar archive which activates this block
                  // of code yet.
               	Correction := Byte(EncName[EncPos]);
                  Inc(EncPos);

                  While ((Len > 0) And (DecPos < MaxDecSize)) Do
                  Begin
               		NameW[DecPos] := WideChar(((Byte(Name[DecPos]) +
                     	Correction) And $ff) + (HighByte Shl 8));
                     Dec(Len);
                     Inc(DecPos);
                  End;
               End Else Begin
                  Len := Len + 2;
                  While ((Len > 0) And (DecPos < MaxDecSize)) Do
                  Begin
                  	NameW[DecPos] := WideChar(Name[DecPos]);
                     Dec(Len);
                     Inc(DecPos);
                  End;
               End;
            End;
      End;

      Flags := Flags Shl 2;
      FlagBits := FlagBits - 2;
   End;

   If DecPos < MaxDecSize Then
      NameW[DecPos] := #0
   Else
      NameW[MaxDecSize - 1] := #0;

End;
//------------------------------------------------------------

Initialization
   p := @k;

End.

