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
Unit ztvFileIo;

Interface

Uses
   Windows,
   SysUtils,
   Controls,
   ztvGbls;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TDateMethod = (dtCreate, dtLastWrite, dtLastAccess);

   TRaiseError =
   	Procedure(Const EClass: ExceptClass; FileName, ExtendedMsg,
      VolumeID: String; ECode: Integer)
      Of Object;

   TRaiseErrorStr =
      Procedure(FileName, MsgEx, VolumeID: String; ECode: Integer)
      Of Object;

   TDiskCommon = Class(TObject)
   Protected
      fRaiseError: TRaiseError;
      fRaiseErrorStr: TRaiseErrorStr;
      fWriteProtectErr: TOnDiskError;
      fDiskInDrvErr: TOnDiskError;
   End;

   TVolumeInformation = Packed Record
      Serial: DWord;
      DirLen: DWord;
      FileSystemFlags: DWord;
      DLabel: Array[0..256] Of char;
      FileSys: Array[0..256] Of char;
      { 'FAT12' 12-bit FAT file system }
      { 'FAT16' 16-bit FAT file system }
      { 'FAT32' 32-bit FAT file system }
      { 'CDROM' High Sierra file system}
      { 'CD001' ISO9660 file system    }
      { 'CDAUDIO' Audio disk           }
   End;

   TDiskManager = Class(TDiskCommon)
   Private
      DriveFlags: DWord;
      DriveName: String;
      DiskSerial: String;
      DiskSerialNo: DWord;
      FileSystem: String;
      FloppyDriveType: TztvDriveType;
   Public
      ZipVolNum: Word;
      DiskLabel: String;
      DriveChar: String;
      DriveType: TztvDriveType;
      FreeSpace: TInteger8;
      TotalSpace: TInteger8;
      VolumeInfo: TVolumeInformation;
      pCancel: pBool;
      ComponentLen: Integer;
      Procedure GetDiscFreeSpace(Drive: String);
      Function GetDriveInfo(FileName: String; fRaiseError: TRaiseError;
			fRaiseErrorStr: TRaiseErrorStr; WriteProtectErrProc,
         DiskInDrvErrProc: TOnDiskError): Boolean;
      Function CheckDrive(FileName: String): Boolean;
      Function ExamineDrive(Drive: String; RaiseError: TRaiseError;
			RaiseErrorStr: TRaiseErrorStr; WriteProtectErrProc, DiskInDrvErrProc:
         TOnDiskError): Boolean;
      Function FloppyDriveSize(DriveChar: String): TztvDriveType;
      Function GetDriveType(Drive: String): TztvDriveType;
      Function GetFloppyInfo(DriveChar: String): Boolean;
      Function GetVolumeInfo(DriveChar: String): Boolean;
      Function GetDriveProtectionStatus(Const Drive: char): Boolean;
      Function Retry(DriveChar: String): Boolean;
      Function ZipDiskLabelToNum(s: String): Integer;
      //Property DiskLabel: String Read GetDiskLabel;
   End;
   pDiskManager = ^TDiskManager;


Function DiskInDrive(Const Drive: char): Boolean;
Function ztvOpenFileAppend(Sender: TObject; FileName: PChar; Overwrite: Boolean): THandle;
Function ztvOpenFileReadWrite(Sender: TObject; FileName: PChar): THandle;
Function ztvOpenFile(Sender: TObject; FileName: PChar; DesiredAccess,
   ShareMode, CreationDistribution, FlagsAndAttributes: DWord): THandle;
Function ztvGetFilePos(f: THandle): u_long; //ztvDWord;
Function ztvSetFilePointer(f: THandle; Offset: u_long {ztvDWord};
   MoveMethod: word): Boolean;
Function ztvGetFileSize(f: THandle): u_long; //ztvDWord;
Function ztvGetFileDate(f: THandle; dt: TDateMethod): Integer;
Function ztvOpenFileRead(Sender: TObject; FileName: PChar): THandle;
Function ztvOpenFileWrite(Sender: TObject; FileName: PChar; Overwrite:
   Boolean): THandle;

Implementation

Uses
	Forms,
   ztvBase,
   Err_Msgs;


(*************************************************************)
(*************************************************************)
(*                        TDiskManger                        *)
(*************************************************************)
(*************************************************************)

Function TDiskManager.GetDriveProtectionStatus(Const Drive: char): Boolean;
Var
   ErrorMode: Word;
   PathName: String;
   TempName: String;
Begin
   ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
   Try
      PathName := Drive + ':\';         // example:  'A:\'
      SetLength(TempName, max_path + 1);
      Windows.GetTempFilename(PChar(PathName), 'ztv', 0, PChar(TempName));

      // GetLastError could be ERROR_PATH_NOT_FOUND but that is ignored here.
      Result := (GetLastError() = Windows.ERROR_WRITE_PROTECT);

      // Delete temporary file.  Error if it does not exist.
      If Not Result Then
         // If file cannot be deleted, the disk is either write protected
         // or the media is absent
         If FileExists(TempName) Then
         	Result := Not DeleteFile(TempName);
   Finally
      SetErrorMode(ErrorMode)
   End
End;
//-------------------------------------------------------------

Function DiskInDrive(Const Drive: char): Boolean;
Var
   DriveNumber: Byte;
   ErrorMode: Word;
Begin
   Result := False;
   DriveNumber := ord(UpCase(Drive));
   ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
   Try
      // 'A'=1, 'B'=2, ... in DiskSize call
      If DiskSize(DriveNumber - ord('A') + 1) <> -1 Then
         Result := True
   Finally
      SetErrorMode(ErrorMode)
   End;
End;
//-------------------------------------------------------------

Function TDiskManager.CheckDrive(FileName: String): Boolean;
Var
   b: Byte;
   Drive: String;
Begin
   Result := True;
   b := Length(FileName);
   If b = 0 Then
      Exit
   Else If (b > 2) And (FileName[2] = ':') Then
   Begin
      Drive := ExtractFileDrive(FileName);
      ZeroMemory(@VolumeInfo, SizeOf(TVolumeInformation));
      While (Not GetVolumeInfo(Drive)) And Result Do
         Result := Retry(Drive);
   End;
End;
//-------------------------------------------------------------

Function TDiskManager.ExamineDrive(Drive: String; RaiseError: TRaiseError;
	RaiseErrorStr: TRaiseErrorStr; WriteProtectErrProc, DiskInDrvErrProc:
   TOnDiskError): Boolean;
Var
	Abort: Boolean;
Begin
   Result := False;
   DriveChar := Drive;
   fRaiseError := RaiseError;
   fRaiseErrorStr := RaiseErrorStr;
   fWriteProtectErr := WriteProtectErrProc;
   fDiskInDrvErr := DiskInDrvErrProc;
   Try
      With VolumeInfo Do
      Begin

         // Check if disk is in drive
         If Not IsUncPath(Drive) Then
         Begin
            While Not DiskInDrive(Drive[1]) Do
            Begin
               If Assigned(fDiskInDrvErr) Then
               Begin
                  Abort := True;
                  fDiskInDrvErr(Self, Abort);
                  If Abort Then
                  Begin
                     If Assigned(fRaiseErrorStr) Then
                     	fRaiseErrorStr(Drive, '0', '0', E_USERCANCEL);
                     Exit;
                  End;
               End Else Begin
               	If Assigned(fRaiseErrorStr) Then
                  	fRaiseErrorStr(Drive, '0', '0', E_DRIVEERR);
                  Exit;
               End;
            End;

            // Disk protect error handler block
            While GetDriveProtectionStatus(Drive[1]) Do
            Begin
               If Assigned(fWriteProtectErr) Then
               Begin
                  Abort := True;
                  fWriteProtectErr(Self, Abort);
                  If Abort Then
                  Begin
                     If Assigned(fRaiseErrorStr) Then
                     	fRaiseErrorStr(Drive, '0', '0', E_USERCANCEL);
                     Exit;
                  End;
               End Else Begin
               	If Assigned(fRaiseErrorStr) Then
                  	fRaiseErrorStr(Drive, '0', '0', E_DRVPROTECTED);
                  Exit;
               End;
            End;
         End;

         While (Not GetVolumeInfo(Drive)) Do
         Begin
            If Not Retry(Drive) Then
            Begin
            	If Assigned(fRaiseErrorStr) Then
      				fRaiseErrorStr(Drive, '0', '0', E_DRIVEERR);
            	Exit;
            End;
         End;

         DiskLabel := DLabel;
         ZipVolNum := ZipDiskLabelToNum(DLabel);
         DiskSerialNo := Serial;
         DiskSerial := IntToHex(Serial, 8);
         ComponentLen := DirLen;
         FileSystem := FileSys;

         DriveType := GetDriveType(Drive);
         If (DriveType = dtNoDrive) Then
         Begin
         	If Assigned(fRaiseErrorStr) Then
      			fRaiseErrorStr(Drive, '0', '0', E_DRIVEERR);
            Exit;
         End;

         GetDiscFreeSpace(DriveChar);
         DriveName := DriveNames[TztvDriveType(0)]; // Unknown

         If DriveType = dtFloppy Then
         Begin
            FloppyDriveType := FloppyDriveSize(DriveChar);
            DriveName := DriveNames[FloppyDriveType];
         End
         Else
            DriveName := DriveNames[DriveType];

         // NOTES: -----------------------------------
         // DriveFlags AND FS_CASE_IS_PRESERVED
         // DriveFlags AND FS_CASE_SENSITIVE
         // DriveFlags AND FS_UNICODE_STORED_ON_DISK
         // DriveFlags AND FS_PERSISTENT_ACLS
         // DriveFlags AND FS_FILE_COMPRESSION
         // DriveFlags AND FS_VOL_IS_COMPRESSED
         // ------------------------------------------
         DriveFlags := FileSystemFlags;
         Result := True;
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Function TDiskManager.FloppyDriveSize(DriveChar: String): TztvDriveType;
Type
   PDIOC_REG = ^TDIOC_Registers;
   TDIOC_Registers = Record
      Reg_EBX, Reg_EDX, Reg_ECX, Reg_EAX, Reg_EDI, Reg_ESI, Reg_Flags: DWord
   End;
Const
   VWIN32_DIOC_DOS_INT13 = 4;           // Performs Interrupt 13h commands.
Var
   H: THandle;
   R: TDIOC_Registers;
   C: DWord;
Begin
   Result := dtUnknown;
   H := CreateFile('\\.\VWIN32', 0, 0, Nil, 0, 0, 0);
   If H <> INVALID_HANDLE_VALUE Then
   Try
      R.Reg_EAX := $800;                // service 8 in AH
      R.Reg_EDX := ord(UpCase(DriveChar[1])) - ord('A'); // drive number in DL
      If DeviceIOControl(H, VWIN32_DIOC_DOS_INT13, @R, SizeOf(R), @R, SizeOf(R), C, Nil)
         And (R.Reg_Flags And 1 = 0) Then // clear CF indicates success

         If R.Reg_EBX And $FF < 3 Then  // drive type in BL
            Result := dtFloppy5         // 1 = 360K, 2 = 1.2MB
         Else
            Result := dtFloppy3         // 3 = 720K  4 = 1.44MB  5 = 2.88MB
   Finally
      CloseHandle(H);
   End
End;
//-------------------------------------------------------------

Procedure TDiskManager.GetDiscFreeSpace(Drive: String);
Var
   DriveNumber: Byte;
Begin
   DriveNumber := ord(UpCase(Drive[1])) - ord('A') + 1;

   If (DriveType = dtNetwork) And (Drive[2] <> ':') Then
      Exit;

   FreeSpace := DiskFree(DriveNumber);
   TotalSpace := DiskSize(DriveNumber);
End;
//-------------------------------------------------------------

Function TDiskManager.GetDriveInfo(FileName: String; fRaiseError: TRaiseError;
	fRaiseErrorStr: TRaiseErrorStr; WriteProtectErrProc, DiskInDrvErrProc:
   TOnDiskError): Boolean;
Var
   c: String;
Begin
   If (Pos('\\', FileName) > 0) Or (Pos(':', FileName) > 1) Then
      c := ExtractFileDrive(FileName)
   Else
      c := ExtractFileDrive(GetCurrentDir);

   Result :=
   	ExamineDrive(c, fRaiseError, fRaiseErrorStr, WriteProtectErrProc,
      	DiskInDrvErrProc);
End;

//-------------------------------------------------------------

Function TDiskManager.GetDriveType(Drive: String): TztvDriveType;
Begin
   If (Drive <> '') Then
      Result := TztvDriveType(Windows.GetDriveType(StringAsPchar(ztvGbls.AppendDirTail(Drive))))
   Else
      Result := dtNoDrive;
End;
//-------------------------------------------------------------

Function TDiskManager.GetFloppyInfo(DriveChar: String): Boolean;
Begin
   Result := False;
   If (DriveType = dtFloppy) Then
   Begin
      FloppyDriveType := FloppyDriveSize(DriveChar);
      GetDiscFreeSpace(DriveChar);
      Result := True;
   End;
End;
//-------------------------------------------------------------

Function TDiskManager.GetVolumeInfo(DriveChar: String): Boolean;
Var
   o: Integer;
Begin
   o := SetErrorMode(SEM_FAILCRITICALERRORS);
   Try
      With VolumeInfo Do
         If (DriveChar <> '') Then
            Result := Windows.GetVolumeInformation(
               StringAsPChar(ztvGbls.AppendDirTail(DriveChar)), @DLabel, 256, @Serial,
               DirLen, FileSystemFlags, @FileSys, 256)
         Else
            Result := False;

   Finally
      SetErrorMode(o)
   End;
End;
//-------------------------------------------------------------

Function TDiskManager.Retry(DriveChar: String): Boolean;
   Function DiscErrorMessage(DriveChar: String): String;
   Begin
      Result := Format('%s' + LoadStr(E_DRIVEERR) + #13#10#13#10 + '%s',
         [UpperCase(AppendDirTail(DriveChar)), SysErrorMessage(GetLastError())]);
   End;
Begin
   Result := Application.MessageBox(
      PChar(DiscErrorMessage(DriveChar)),
      PChar(Application.Title),
      mb_RetryCancel Or mb_IconError) = idRetry;
End;
//-------------------------------------------------------------

Function TDiskManager.ZipDiskLabelToNum(s: String): Integer;
   Function IsNumeric(str: String): Boolean;
   Var
      b, i: Byte;
   Begin
      Result := True;
      For i := 1 To Length(str) Do
      Begin
         b := Byte(str[i]);
         If (b < 48) Or (b > 57) Then
         Begin
            Result := False;
            Exit;
         End;
      End;
   End;
Var
   VolName: String;
Begin
   Result := 0;
   If Pos(PKBACK, s) > 0 Then
   Begin
      While s <> '' Do
         VolName := ztvGbls.GetToken(s, PKBACK);

      If IsNumeric(VolName) Then
         Result := StrToInt(VolName);
   End;
End;

//-------------------------------------------------------------

{FUNCTION ztvFileRead( f: THandle; VAR Buffer; BytesToRead: ztvDWord ): DWord;
BEGIN
 ReadFile( f, Buffer, BytesToRead, Result, NIL );
END;}
//-------------------------------------------------------------

{FUNCTION ztvFileWrite( f: THandle; VAR Buffer; BytesToWrite: ztvDWord ): DWord;
BEGIN
 WriteFile( f, Buffer, BytesToWrite, Result, NIL );
END;}
//-------------------------------------------------------------

Function ztvOpenFileReadWrite(Sender: TObject; FileName: PChar): THandle;
Begin
   Result := ztvOpenFile(Sender, FileName,
      GENERIC_READ Or GENERIC_WRITE,
      {FILE_SHARE_WRITE Or} FILE_SHARE_READ,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL)
End;
(* ------------------------------------------------------------------------- *)
(* No sharing *)

Function ztvOpenFileAppend(Sender: TObject; FileName: PChar; Overwrite: Boolean): THandle;
Begin
   SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_NORMAL);
   Result := ztvOpenFile(Sender, FileName, GENERIC_WRITE, 0, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL);

   If (Result <> INVALID_HANDLE_VALUE) Then
      If (Not ztvSetFilePointer(Result, 0, FILE_END)) Then
      Begin
         CloseHandle(Result);
         Result := INVALID_HANDLE_VALUE;
      End;
End;
//-------------------------------------------------------------
(* No sharing *)

Function ztvOpenFileWrite(Sender: TObject; FileName: PChar; Overwrite: Boolean): THandle;
Begin
   If Overwrite Then
   Begin
  		//SetFileAttributes( PChar( Filename ), 0{FILE_ATTRIBUTE_NORMAL} );
      Result := ztvOpenFile(Sender, FileName, GENERIC_WRITE, 0,
         CREATE_ALWAYS, 0 {FILE_ATTRIBUTE_NORMAL})
   End
   Else
      (* fails if file already exists *)
      Result := ztvOpenFile(Sender, FileName, GENERIC_WRITE,
         0, CREATE_NEW, 0 {FILE_ATTRIBUTE_NORMAL})
End;
//-------------------------------------------------------------

Function ztvOpenFileRead(Sender: TObject; FileName: PChar): THandle;
//Const
//   FILE_ATTRIBUTE_COMPRESSED = $800;
Begin
   Result := ztvOpenFile(Sender,
      FileName,
      GENERIC_READ,
      FILE_SHARE_READ,
      OPEN_EXISTING,
      faAnyFile
      {faArchive Or FILE_ATTRIBUTE_COMPRESSED Or faHidden Or
      faReadOnly Or faSysFile Or FILE_ATTRIBUTE_TEMPORARY});
End;
//-------------------------------------------------------------
(* Use CloseHandle to close this handle *)

Function ztvOpenFile(Sender: TObject; FileName: PChar; DesiredAccess,
   ShareMode, CreationDistribution, FlagsAndAttributes: DWord): THandle;
Begin
   Result := CreateFile(FileName, DesiredAccess, ShareMode, Nil,
      CreationDistribution, FlagsAndAttributes, 0);

   If Result = INVALID_HANDLE_VALUE Then
      //TZipCommon( Sender ).RaiseError( E_RAISE, StrPas( Filename ), '', '0', E_FOPEN );
      TZipCommon(Sender).RaiseErrorStr(StrPas(FileName), '', '0', E_FOPEN);
End;
//-------------------------------------------------------------

Function ztvGetFilePos(f: THandle): u_long;
Begin
   Result := SetFilePointer(f, 0, Nil, FILE_CURRENT);
End;
//-------------------------------------------------------------

Function ztvSetFilePointer(f: THandle; Offset: u_long; MoveMethod: word): Boolean;
Var
   d: u_long;
   //dwSizeHigh: DWord;
Begin
   d := SetFilePointer(f, Offset, Nil {@dwSizeHigh}, MoveMethod);
   Result := Not ((d = $FFFFFFFF) And
      (GetLastError <> NO_ERROR));
End;
//-------------------------------------------------------------

Function ztvGetFileSize(f: THandle): u_long;
Var
   dwSizeHigh: DWord;
Begin
   Result := GetFileSize(f, @dwSizeHigh);
   If (Result = $FFFFFFFF) And (GetLastError <> NO_ERROR) Then
      Result := 0
   Else
      Result := dwSizeHigh * MAXDWORD + Result;
End;
//-------------------------------------------------------------

Function ztvGetFileDate(f: THandle; dt: TDateMethod): Integer;
Var
   Date, Time: word;
   FileTime: TFileTime;
Begin
   Case dt Of
      dtCreate: GetFileTime(f, @FileTime, Nil, Nil);
      dtLastAccess: GetFileTime(f, Nil, @FileTime, Nil);
      dtLastWrite: GetFileTime(f, Nil, Nil, @FileTime);
   End;
   FileTimeToLocalFileTime( FileTime, FileTime );
   FileTimeToDosDateTime(FileTime, word(Date), word(Time));
   Result := Makelong(Time, Date);
End;
//-------------------------------------------------------------

End.
