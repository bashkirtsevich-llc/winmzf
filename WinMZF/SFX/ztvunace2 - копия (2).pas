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
Unit ztvUnACE2;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   Dialogs,
   ztvBase,
   ztvFileIo,
   ztvGbls,
   ztvHeaders,
   ztvStreams;


{$I ZipTV.inc}                          //Declare the compiler defines
{.$define UseAceDLL}	// do not use this conditional!

Type
   TUnACE = Class(TUnBASE)
   Private
   	LocalDate: Integer;
		FileSpecList: PChar;
      IsRenamedFile: Boolean;
      IsValidFilespec: Boolean;
      fFileCount: Integer;
      fExtractFilename: String;
      fFileBytesProcessed: Int64;
      fExtractDirExists: Boolean;
		Function GetArchiveTotals(ArcName: String): Boolean;
   Protected
      Procedure doOnEnd_Local(CrcPass: Boolean; FileAttr: u_long);
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure ExtractIT(Var Infile: TStream32; Outfile: TStream32); Override;
   Published
      Property ArcType;
      Property ConfirmOverwrites;
      Property CreateStoredDirs;
      Property ExtractDir;
      Property FileSpec;
      Property OverwriteMode;
      Property Password;
      Property PasswordAttempts;
      Property RestoreFileAttr;
      Property UseStoredDirs;
      Property OnActivate;
      Property OnDeactivate;
      Property OnProgress;
      Property OnGetPassword;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnNextVolume;
      Property OnRenameFile;
   End;



Implementation


Uses
   ztvLoadLib,
   Err_Msgs;


Type
   pACECommentStruc = ^tACECommentStruc;
   tACECommentStruc = Packed Record
      Buf: PByteArray;
      BufSize: Integer;
      state: Integer;
   End;

   pACEGlobalDataStruc = ^tACEGlobalDataStruc;
   tACEGlobalDataStruc = Packed Record
      obj: Pointer;
      MaxArchiveTestBytes: u_long;
      MaxFileBufSize: u_long;
      Comment: tACECommentStruc;
      DecryptPassword: PChar;
      reserved1: Array[0..63] Of char;
      EncryptPassword: PChar;
      TempDir: PChar;
      KeyPath: PChar;
      UserAV: PChar;
      IsGeneralKey: PChar;
      OwnerWindow: HWND;
      CompressionLevel: u_long;
      reserved2: Array[0..55] Of char;
      InfoCallbackProc: Pointer;
      ErrorCallbackProc: Pointer;
      RequestCallbackProc: Pointer;
      StateCallbackProc: Pointer;
      reserved3: Array[0..63] Of char;
   End;

   pACEArchiveDataStruc = ^tACEArchiveDataStruc;
   tACEArchiveDataStruc = Packed Record
      ArchiveName: PChar;
      VolumeNumber,
         flags,
         HostCreated,
         TimeCreated,                   // in MS-DOS format
         VersionCreated,
         VersionExtract: u_long;        // version needed to extract files
      AV: PChar;
      Reserved: Array[0..63] Of char;
   End;

   pACEReadArchiveDataStruc = ^tACEReadArchiveDataStruc;
   tACEReadArchiveDataStruc = Packed Record
      ArchiveData: tACEArchiveDataStruc;
      Reserved: Array[0..63] Of char;
   End;

   pACEFileDataStruc = ^tACEFileDataStruc;
   tACEFileDataStruc = Packed Record
      SourceFileName: PChar;
      DestinationFileName: PChar;
      flags,
         CRC32,
         method,                        // 0=stored, 1=LZ77, 2=V20Compression
         dictionary: u_long;            // DictionarySize = 2^Dictionary
      CompressedSize,
         size: Int64;
      Time,
         Attributes: u_long;
      Reserved: Array[0..63] Of char;
   End;

   pACECopyInfoStruc = ^tACECopyInfoStruc;
   tACECopyInfoStruc = Packed Record
      SourceFileName: PChar;
      DestinationFileName: PChar;
      CopiedBytes,
         FileSize: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACEProgressDataStruc = ^tACEProgressDataStruc;
   tACEProgressDataStruc = Packed Record
      Addr: PChar;
      size: u_long;
      TotalProcessedSize: Cardinal;
      TotalCompressedSize: Cardinal;
      TotalSize: Cardinal;
      FileProcessedSize: Cardinal;
      FileCompressedSize: Cardinal;
      FileSize: Cardinal;
   End;

   pACEFilesStruc = ^tACEFilesStruc;
   tACEFilesStruc = Packed Record
      SourceDir: PChar;
      FileList: PChar;
      ExcludeList: PChar;
      FullMatch: LongBool;
      RecurseSubDirs: LongBool;
      Reserved: Array[0..59] Of char;
   End;

   // this component does not use the following structure
   tACEV20CompressionStruc = Packed Record
      DoUse,
         DoUseDelta,
         DoUseExe,
         DoUsePic,
         DoUseSound: LongBool;
      Reserved: Array[0..63] Of char;
   End;

   // this component does not use the following structure
   tACECompressParamsStruc = Packed Record
      level: u_long;
      dictionary: u_long;
      V20Compression: tACEV20CompressionStruc;
      TestAfter: LongBool;
      Reserved: Array[0..63] Of char;
   End;

   pACECallbackGlobalStruc = ^tACECallbackGlobalStruc;
   tACECallbackGlobalStruc = Packed Record
      code: u_long;
      Operation: u_long;
      GlobalData: pACEGlobalDataStruc;
   End;

   pACECallbackArchiveStruc = ^tACECallbackArchiveStruc;
   tACECallbackArchiveStruc = Packed Record
      code: u_long;
      Operation: u_long;
      GlobalData: pACEGlobalDataStruc;
      ArchiveData: pACEArchiveDataStruc;
   End;

   pACECallbackArchivedFileStruc = ^tACECallbackArchivedFileStruc;
   tACECallbackArchivedFileStruc = Packed Record
      code: u_long;
      Operation: u_long;
      GlobalData: pACEGlobalDataStruc;
      ArchiveData: pACEArchiveDataStruc;
      FileData: pACEFileDataStruc;
   End;

   pACECallbackRealFileStruc = ^tACECallbackRealFileStruc;
   tACECallbackRealFileStruc = Packed Record
      code: u_long;
      Operation: u_long;
      GlobalData: pACEGlobalDataStruc;
      ArchiveData: pACEArchiveDataStruc;
      FileName: PChar;
   End;

   pACECallbackSpaceStruc = ^tACECallbackSpaceStruc;
   tACECallbackSpaceStruc = Packed Record
      code: u_long;
      Operation: u_long;
      GlobalData: pACEGlobalDataStruc;
      ArchiveData: pACEArchiveDataStruc;
      Directory: PChar;
      ArchiveSize: Cardinal;
   End;

   pACECallbackSFXFileStruc = ^tACECallbackSFXFileStruc;
   tACECallbackSFXFileStruc = Packed Record
      code: u_long;
      Operation: u_long;
      GlobalData: pACEGlobalDataStruc;
      ArchiveData: pACEArchiveDataStruc;
      SFXFileName: PChar;
   End;

   pACECallbackCopyStruc = ^tACECallbackCopyStruc;
   tACECallbackCopyStruc = Packed Record
      code: u_long;
      Operation: u_long;
      GlobalData: pACEGlobalDataStruc;
      ArchiveData: pACEArchiveDataStruc;
      CopyData: pACECopyInfoStruc;
   End;

   pACECallbackProgressStruc = ^tACECallbackProgressStruc;
   tACECallbackProgressStruc = Packed Record
      code: u_long;
      Operation: u_long;
      GlobalData: pACEGlobalDataStruc;
      ArchiveData: pACEArchiveDataStruc;
      FileData: pACEFileDataStruc;
      ProgressData: pACEProgressDataStruc;
   End;

   pACECallbackCRCCheckStruc = ^tACECallbackCRCCheckStruc;
   tACECallbackCRCCheckStruc = Packed Record
      code: u_long;
      Operation: u_long;
      GlobalData: pACEGlobalDataStruc;
      ArchiveData: pACEArchiveDataStruc;
      FileData: pACEFileDataStruc;
      CRCOk: LongBool;
   End;

   pACEInfoCallbackProcStruc = ^tACEInfoCallbackProcStruc;
   tACEInfoCallbackProcStruc = Packed Record
      Case StructureType: u_long Of
         0: (Global: tACECallbackGlobalStruc);
         1: (Archive: tACECallbackArchiveStruc);
         2: (RealFile: tACECallbackRealFileStruc);
         3: (Copy: tACECallbackCopyStruc);
   End;

   pACEErrorCallbackProcStruc = ^tACEErrorCallbackProcStruc;
   tACEErrorCallbackProcStruc = Packed Record
      Case StructureType: u_long Of
         0: (Global: tACECallbackGlobalStruc);
         1: (Archive: tACECallbackArchiveStruc);
         2: (ArchivedFile: tACECallbackArchivedFileStruc);
         3: (RealFile: tACECallbackRealFileStruc);
         4: (Space: tACECallbackSpaceStruc);
         5: (SFXFile: tACECallbackSFXFileStruc);
   End;

   pACERequestCallbackProcStruc = ^tACERequestCallbackProcStruc;
   tACERequestCallbackProcStruc = Packed Record
      Case StructureType: u_long Of
         0: (Global: tACECallbackGlobalStruc);
         1: (Archive: tACECallbackArchiveStruc);
         2: (ArchivedFile: tACECallbackArchivedFileStruc);
         3: (RealFile: tACECallbackRealFileStruc);
   End;

   pACEStateCallbackProcStruc = ^tACEStateCallbackProcStruc;
   tACEStateCallbackProcStruc = Packed Record
      Case StructureType: u_long Of
         0: (Archive: tACECallbackArchiveStruc);
         1: (ArchivedFile: tACECallbackArchivedFileStruc);
         2: (RealFile: tACECallbackArchivedFileStruc);
         3: (Progress: tACECallbackProgressStruc);
         4: (CrcCheck: tACECallbackCRCCheckStruc);
   End;

   pACEInitDllStruc = ^tACEInitDllStruc;
   tACEInitDllStruc = Packed Record
      GlobalData: tACEGlobalDataStruc;
      Reserved: Array[0..63] Of char;
   End;

   pACEListStruct = ^tACEListStruct;
   tACEListStruct = Packed Record
      Files: tACEFilesStruc;
      Reserved: Array[0..63] Of char;
   End;

   pACETestStruct = ^tACETestStruct;
   tACETestStruct = Packed Record
      Files: tACEFilesStruc;
      DecryptPassword: PChar;
      Reserved: Array[0..63] Of char;
   End;

   pACEExtractStruct = ^tACEExtractStruct;
   tACEExtractStruct = Packed Record
      Files: tACEFilesStruc;
      DestinationDir: PChar;
      ExcludePath: LongBool;
      DecryptPassword: PChar;
      Reserved: Array[0..63] Of char;
   End;



{$IFDEF UseAceDLL}
   pACEDeleteStruc = ^tACEDeleteStruc;
   tACEDeleteStruc = Packed Record
      Files: tACEFilesStruc;
      DecryptPassword: PChar;
      CompressParams: tACECompressParamsStruc;
      TimeSet: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACEAddStruc = ^tACEAddStruc;
   tACEAddStruc = Packed Record
      Files: tACEFilesStruc;
      DestinationDir: PChar;
      mode: Cardinal;
      SavePath: Cardinal;
      EncryptPassword: PChar;
      DecryptPassword: PChar;
      CompressParams: tACECompressParamsStruc;
      MainComment: BOOL;
      Solid: BOOL;
      Lock: BOOL;
      RecoveryRecord: BOOL;
      AV: BOOL;
      VolumeSize: Cardinal;
      SFXName: PChar;
      TimeSet: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACEReadSFXDataStruc = ^tACEReadSFXDataStruc;
   tACEReadSFXDataStruc = Packed Record
      LanguageIndex: Cardinal;
      ShortTitle: PChar;
      MediumTitle: PChar;
      LongTitle: PChar;
      Description: PChar;
      START: Cardinal;
      size: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACERepairStruc = ^tACERepairStruc;
   tACERepairStruc = Packed Record
      RepairedArchiveName: PChar;
      Reserved: Array[0..63] Of char;
   End;

   pACESetCommentsStruc = ^tACESetCommentsStruc;
   tACESetCommentsStruc = Packed Record
      Files: tACEFilesStruc;
      TimeSet: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACEEncryptFilesStruc = ^tACEEncryptFilesStruc;
   tACEEncryptFilesStruc = Packed Record
      Files: tACEFilesStruc;
      EncryptPassword: PChar;
      DecryptPassword: PChar;
      TimeSet: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACEAddSFXStruc = ^tACEAddSFXStruc;
   tACEAddSFXStruc = Packed Record
      SFXName: PChar;
      SFXArchiveName: PChar;
      TimeSet: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACELockStruc = ^pACELockStruc;
   tACELockStruc = Packed Record
      TimeSet: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACEAddRecoveryRecordStruc = ^tACEAddRecoveryRecordStruc;
   tACEAddRecoveryRecordStruc = Packed Record
      TimeSet: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACEAddAVStruc = ^tACEAddAVStruc;
   tACEAddAVStruc = Packed Record
      TimeSet: Cardinal;
      Reserved: Array[0..63] Of char;
   End;

   pACERegisterStruc = ^tACERegisterStruc;
   tACERegisterStruc = Packed Record
      RegCode: Array[0..32] Of char;
      Reserved: Array[0..63] Of char;
   End;
{$ENDIF}


Type
   TACEInitDllProc = Function(DllDate: pACEInitDllStruc): Integer; Stdcall;
   TACEReadArchiveDataProc = Function(ArchiveName: PChar; ArchiveData: pACEReadArchiveDataStruc): Integer; Stdcall;
   TACEListProc = Function(ArchiveName: PChar; List: pACEListStruct): Integer; Stdcall;
   TACETestProc = Function(ArchiveName: PChar; List: pACETestStruct): Integer; Stdcall;
   TACEExtractProc = Function(ArchiveName: PChar; Extract: pACEExtractStruct): Integer; Stdcall;

{$IFDEF UseAceDLL}
   TACEDelete = Function(ArchiveName: PChar; Delete: pACEDeleteStruc): Integer; Stdcall;
   TACEAdd = Function(ArchiveName: PChar; Add: pACEAddStruc): Integer; Stdcall;
   TACEReadSFXData = Function(SFXName: PChar; SFXData: pACEReadSFXDataStruc): Integer; Stdcall;
   TACERepair = Function(ArchiveName: PChar; Repair: pACERepairStruc): Integer; Stdcall;
   TACESetComments = Function(ArchiveName: PChar; SetComments: pACESetCommentsStruc): Integer; Stdcall;
   TACEEncryptFiles = Function(ArchiveName: PChar; EncryptFiles: pACEEncryptFilesStruc): Integer; Stdcall;
   TACEAddSFX = Function(ArchiveName: PChar; AddSFXModule: pACEAddSFXStruc): Integer; Stdcall;
   TACELock = Function(ArchiveName: PChar; Lock: pACELockStruc): Integer; Stdcall;
   TACEAddRecoveryRecord = Function(ArchiveName: PChar; AddRecoveryRecord: pACEAddRecoveryRecordStruc): Integer; Stdcall;
   TACEAddAV = Function(ArchiveName: PChar; AddAV: pACEAddAVStruc): Integer; Stdcall;
   TACERegister = Function(Register: pACERegisterStruc): Integer; Stdcall;
   TACEShowRegisterDialog = Function: Integer; Stdcall;
{$ENDIF}

Const
   COMMENTBUFSIZE = 8192;
	MaxFileBufSize = $2FFFF;  //$7a120;  adjust desired file buffersize here

{$IFDEF UseAceDLL}
   AceDLL = 'ace.dll';
{$ELSE}
   AceDLL = 'unacev2.dll';
{$ENDIF}

   ACE_COMMENT_OK = 0;
   ACE_COMMENT_SMALLBUF = 1;
   ACE_COMMENT_NONE = 255;

   ACE_ARCFLAG_MAINCOMMENT = 2;
   ACE_ARCFLAG_SFX = 512;
   ACE_ARCFLAG_LIMITSFXJR = 1024;       // is an SFX archive that supports 256k Dictionary only
   ACE_ARCFLAG_MULTIVOLUME = 2048;
   ACE_ARCFLAG_AV = 4096;
   ACE_ARCFLAG_RECOVERYREC = 8192;
   ACE_ARCFLAG_LOCK = 16384;
   ACE_ARCFLAG_SOLID = 32768;

   // Host system used to create an archive. Used at
   // tACEArchiveDataStruc.HostCreated field.
   ACE_HOST_MSDOS = 0;                  // archive created by MSDOS ACE archiver
   ACE_HOST_OS2 = 1;                    // created by OS2 ACE
   ACE_HOST_WIN32 = 2;                  // created by Win32 ACE

   ACE_FILEFLAG_FILECOMMENT = 2;
   ACE_FILEFLAG_SPLITBEFORE = 4096;
   ACE_FILEFLAG_SPLITAFTER = 8192;
   ACE_FILEFLAG_PASSWORD = 16384;
   ACE_FILEFLAG_SOLID = 32768;

   ACE_LEVEL_STORE = 0;
   ACE_LEVEL_FASTEST = 1;
   ACE_LEVEL_FAST = 2;
   ACE_LEVEL_NORMAL = 3;
   ACE_LEVEL_GOOD = 4;
   ACE_LEVEL_BEST = 5;

   ACE_CALLBACK_OPERATION_LIST = 0;
   ACE_CALLBACK_OPERATION_TEST = 1;
   ACE_CALLBACK_OPERATION_ANALYZE = 2;
   ACE_CALLBACK_OPERATION_EXTRACT = 3;
   ACE_CALLBACK_OPERATION_ADD = 4;
   ACE_CALLBACK_OPERATION_REPACK = 5;
   ACE_CALLBACK_OPERATION_DELETE = 6;
   ACE_CALLBACK_OPERATION_REPAIR = 7;
   ACE_CALLBACK_OPERATION_SETCMT = 8;
   ACE_CALLBACK_OPERATION_ENCRYPT = 9;
   ACE_CALLBACK_OPERATION_KEEP = 10;
   ACE_CALLBACK_OPERATION_RECOVER = 11;
   ACE_CALLBACK_OPERATION_HEADSEARCH = 12;
   ACE_CALLBACK_OPERATION_RECRECSEARCH = 13;
   ACE_CALLBACK_OPERATION_ADDSFX = 14;
   ACE_CALLBACK_OPERATION_LOCK = 15;
   ACE_CALLBACK_OPERATION_ADDAV = 16;
   ACE_CALLBACK_OPERATION_ADDRECOVREC = 17;
   ACE_CALLBACK_OPERATION_REGISTER = 18;

   ACE_CALLBACK_RETURN_OK = 0;
   ACE_CALLBACK_RETURN_NO = 1;
   ACE_CALLBACK_RETURN_CANCEL = 2;

   ACE_CALLBACK_TYPE_GLOBAL = 0;
   ACE_CALLBACK_TYPE_ARCHIVE = 1;
   ACE_CALLBACK_TYPE_ARCHIVEDFILE = 2;
   ACE_CALLBACK_TYPE_REALFILE = 3;
   ACE_CALLBACK_TYPE_SPACE = 4;         // not used in this component
   ACE_CALLBACK_TYPE_SFXFILE = 5;       // not used in this component
   ACE_CALLBACK_TYPE_COPY = 6;          // not used in this component
   ACE_CALLBACK_TYPE_PROGRESS = 7;
   ACE_CALLBACK_TYPE_CRCCHECK = 8;

   ACE_CALLBACK_INFO_GENERALKEY = $100; // not used in this component
   ACE_CALLBACK_INFO_TMPARCCREATE = $110; // not used in this component
   ACE_CALLBACK_INFO_TMPARCCREATEEND = $111; // not used in this component
   ACE_CALLBACK_INFO_ADDRECREC = $112;  // not used in this component
   ACE_CALLBACK_INFO_ADDRECRECEND = $113; // not used in this component
   ACE_CALLBACK_INFO_RECREC = $114;     // not used in this component
   ACE_CALLBACK_INFO_NORECREC = $115;   // not used in this component
   ACE_CALLBACK_INFO_RECOVERED = $116;  // not used in this component
   ACE_CALLBACK_INFO_NODAMAGE = $117;
   ACE_CALLBACK_INFO_FNDMAINHEAD = $118;
   ACE_CALLBACK_INFO_FILELISTCREATE = $119;
   ACE_CALLBACK_INFO_FILELISTCREATEEND = $11A;
   ACE_CALLBACK_INFO_FILESORT = $11B;
   ACE_CALLBACK_INFO_FILESORTEND = $11C;
   ACE_CALLBACK_INFO_COPYEND = $11D;
   ACE_CALLBACK_INFO_FILELISTADD = $140;
   ACE_CALLBACK_INFO_COPY = $150;

   ACE_CALLBACK_ERROR_MEMORY = $200;
   ACE_CALLBACK_ERROR_REGISTER = $201;
   ACE_CALLBACK_ERROR_READKEY = $202;
   ACE_CALLBACK_ERROR_WRITEKEY = $203;
   ACE_CALLBACK_ERROR_NOWINACEKEY = $204;
   ACE_CALLBACK_ERROR_NOACTIVEACEKEY = $205;
   ACE_CALLBACK_ERROR_UNCSPACE = $206;
   ACE_CALLBACK_ERROR_MODIFYVOLUME = $220;
   ACE_CALLBACK_ERROR_MODIFYLOCKEDARCHIVE = $221;
   ACE_CALLBACK_ERROR_AV = $222;
   ACE_CALLBACK_ERROR_TOODAMAGED = $223;
   ACE_CALLBACK_ERROR_ARCHIVEEXISTS = $224;
   ACE_CALLBACK_ERROR_OPENREPAIRARCHIVE = $225;
   ACE_CALLBACK_ERROR_OPENARCHIVEREAD = $226;
   ACE_CALLBACK_ERROR_OPENARCHIVEWRITE = $227;
   ACE_CALLBACK_ERROR_READARCHIVE = $228;
   ACE_CALLBACK_ERROR_WRITEARCHIVE = $229;
   ACE_CALLBACK_ERROR_ALREADYSFX = $22A;
   ACE_CALLBACK_ERROR_ADDSFXTOVOLUME = $22B;
   ACE_CALLBACK_ERROR_ARCHIVEBROKEN = $22C;
   ACE_CALLBACK_ERROR_ARCHIVESAVE = $22D;
   ACE_CALLBACK_ERROR_NOFILES = $22E;
   ACE_CALLBACK_ERROR_ISNOTANARCHIVE = $22F;
   ACE_CALLBACK_ERROR_TEMPDIRCREATE = $230;
   ACE_CALLBACK_ERROR_HIGHERVERSION = $231;
   ACE_CALLBACK_ERROR_CREATIONNAMEINUSE = $240;
   ACE_CALLBACK_ERROR_ENCRYPTIONCRC = $242;
   ACE_CALLBACK_ERROR_READ = $243;
   ACE_CALLBACK_ERROR_WRITE = $244;
   ACE_CALLBACK_ERROR_OPENREAD = $245;
   ACE_CALLBACK_ERROR_OPENWRITE = $246;
   ACE_CALLBACK_ERROR_FILENAMETOOLONG = $247;
   ACE_CALLBACK_ERROR_REPACKCRC = $248;
   ACE_CALLBACK_ERROR_EXCLUDEPATH = $249;
   ACE_CALLBACK_ERROR_METHOD = $24A;
   ACE_CALLBACK_ERROR_EXTRACTSPACE = $24B;
   ACE_CALLBACK_ERROR_CREATION = $24C;
   ACE_CALLBACK_ERROR_OVERWRITEDELETE = $24D;
   ACE_CALLBACK_ERROR_MOVEDELETE = $260;
   ACE_CALLBACK_ERROR_TEMPDIRSPACE = $270;
   ACE_CALLBACK_ERROR_ARCHIVESPACE = $271;
   ACE_CALLBACK_ERROR_READINGSFXFILE = $280;

   ACE_CALLBACK_REQUEST_REGISTER = $300;
   ACE_CALLBACK_REQUEST_MARKASSOLID = $320;
   ACE_CALLBACK_REQUEST_CHANGEVOLUME = $321;
   ACE_CALLBACK_REQUEST_ARCHIVEEXISTS = $322;
   ACE_CALLBACK_REQUEST_OVERWRITE = $340;
   ACE_CALLBACK_REQUEST_DELARCHIVEDSYSFILE = $341;
   ACE_CALLBACK_REQUEST_ADDBROKENFILE = $342;
   ACE_CALLBACK_REQUEST_PASSWORD = $343;
   ACE_CALLBACK_REQUEST_OVERWRITESYSFILE = $344;
   ACE_CALLBACK_REQUEST_MOVEDELREALSYSFILE = $360;

   ACE_CALLBACK_STATE_STARTARCHIVE = $400;
   ACE_CALLBACK_STATE_STARTFILE = $410;
   ACE_CALLBACK_STATE_ENDNOCRCCHECK = $411;
   ACE_CALLBACK_STATE_PROGRESS = $420;
   ACE_CALLBACK_STATE_ENDCRCCHECK = $430;

   ACE_ERROR_NOERROR = 0;               // no error; operation succesful
   ACE_ERROR_MEM = 1;                   // insufficient memory
   ACE_ERROR_FILES = 2;                 // no files specified
   ACE_ERROR_FOUND = 3;                 // specified archive not found
   ACE_ERROR_FULL = 4;                  // disk full
   ACE_ERROR_OPEN = 5;                  // could not open file
   ACE_ERROR_READ = 6;                  // read error
   ACE_ERROR_WRITE = 7;                 // write error
   ACE_ERROR_CLINE = 8;                 // invalid command line
   ACE_ERROR_CRC = 9;                   // CRC error
   ACE_ERROR_OTHER = 10;                // other error
   ACE_ERROR_EXISTS = 11;               // file already exists
   ACE_ERROR_USER = 255;                // user break (application
   ACE_ERROR_PARAM = 128;               // might be used later

Var
   Me: TUnACE;
   AceDllInstance: THandle;
   FilesListBufSize: Integer;
   CommentBuf: Array[0..COMMENTBUFSIZE - 1] Of char;

   AceInitDllProc: TACEInitDllProc;
   AceReadArchiveDataProc: TACEReadArchiveDataProc;
   AceListProc: TACEListProc;
   AceTestProc: TACETestProc;
   AceExtractProc: TACEExtractProc;
{$IFDEF UseAceDLL}
   AceDeleteProc: TACEDelete;
   AceAddProc: TACEAdd;
   AceReadSFXDataProc: TACEReadSFXData;
   AceRepairProc: TACERepair;
   AceSetCommentsProc: TACESetComments;
   AceEncryptFilesProc: TACEEncryptFiles;
   AceAddSFXProc: TACEAddSFX;
   AceLockProc: TACELock;
   AceAddRecoveryRecordProc: TACEAddRecoveryRecord;
   AceAddAVProc: TACEAddAV;
   AceRegisterProc: TACERegister;
   AceShowRegisterDialogProc: TACEShowRegisterDialog;
{$ENDIF}


//Function CallACEList(ArchiveName: PChar): Integer; Forward;
Function CallACEVerify(ArchiveName: PChar): Integer; Forward;
Function CallACEExtract(ArchiveName: PChar; DestinationDir: String;
   DecryptPassword: String; DoExcludePath: LongBool): Integer; Forward;
Function StateProc(state: pACEStateCallbackProcStruc): Integer; Stdcall; Forward;


//------------------------------------------------------------
//------------------------------------------------------------

Function LoadAceDll: Boolean;
Begin
   AceDllInstance := MyLoadLibrary(AceDLL);
   Result := AceDllInstance <> 0;
   If Result Then
   Begin
      AceInitDllProc := GetProcAddress(AceDllInstance, 'ACEInitDll');
      AceReadArchiveDataProc := GetProcAddress(AceDllInstance, 'ACEReadArchiveData');
      AceListProc := GetProcAddress(AceDllInstance, 'ACEList');
      AceTestProc := GetProcAddress(AceDllInstance, 'ACETest');
      AceExtractProc := GetProcAddress(AceDllInstance, 'ACEExtract');
{$IFDEF UseAceDLL}
      AceDeleteProc := GetProcAddress(AceDllInstance, 'ACEDelete');
      AceAddProc := GetProcAddress(AceDllInstance, 'ACEAdd');
      AceReadSFXDataProc := GetProcAddress(AceDllInstance, 'ACEReadSFXData');
      AceRepairProc := GetProcAddress(AceDllInstance, 'ACERepair');
      AceSetCommentsProc := GetProcAddress(AceDllInstance, 'ACESetComments');
      AceEncryptFilesProc := GetProcAddress(AceDllInstance, 'ACEEncryptFiles');
      AceAddSFXProc := GetProcAddress(AceDllInstance, 'ACEAddSFX');
      AceLockProc := GetProcAddress(AceDllInstance, 'ACELock');
      AceAddRecoveryRecordProc := GetProcAddress(AceDllInstance, 'ACEAddRecoveryRecord');
      AceAddAVProc := GetProcAddress(AceDllInstance, 'ACEAddAV');
      AceRegisterProc := GetProcAddress(AceDllInstance, 'ACERegister');
      AceShowRegisterDialogProc := GetProcAddress(AceDllInstance, 'ACEShowRegisterDialog');
{$ENDIF}

      If (@AceInitDllProc = Nil) Or
         (@AceReadArchiveDataProc = Nil) Or
         (@AceListProc = Nil) Or
         (@AceTestProc = Nil) Or
         (@AceExtractProc = Nil)
{$IFDEF UseAceDLL}
      Or
         (@AceDeleteProc = Nil) Or
         (@AceAddProc = Nil) Or
         (@AceReadSFXDataProc = Nil) Or
         (@AceRepairProc = Nil) Or
         (@AceSetCommentsProc = Nil) Or
         (@AceEncryptFilesProc = Nil) Or
         (@AceAddSFXProc = Nil) Or
         (@AceLockProc = Nil) Or
         (@AceAddRecoveryRecordProc = Nil) Or
         (@AceAddAVProc = Nil) Or
         (@AceRegisterProc = Nil) Or
         (@AceShowRegisterDialogProc = Nil)
{$ENDIF} Then
      Begin
         FreeLibrary(AceDllInstance);
			Result := False;
         AceDllInstance := 0;
      End;
   End;
End;
//------------------------------------------------------------

Procedure UnLoadAceDll;
Begin
   If AceDllInstance <> 0 Then
      FreeLibrary(AceDllInstance);
End;
//------------------------------------------------------------

Function InfoProc(Info: pACEInfoCallbackProcStruc): Integer; Stdcall;
Begin

   If Me.Cancel Then
      Result := ACE_CALLBACK_RETURN_CANCEL
   Else
   Begin

      Case Info^.Global.Operation Of

         ACE_CALLBACK_OPERATION_LIST:

            Case Info^.Global.code Of
               ACE_CALLBACK_INFO_FILELISTCREATE: // 'Creating file list'
                  Begin
                     Me.fFileCount := 0;
                  End;

               ACE_CALLBACK_INFO_FILELISTCREATEEND: // 'Finished creating file list'
                  Begin
                  End;
            End;

         ACE_CALLBACK_OPERATION_TEST:

            Case Info^.Global.code Of
               ACE_CALLBACK_INFO_FILELISTCREATE: // 'Creating file list'
                  Begin
                     Me.fFileCount := 0;
                  End;

               ACE_CALLBACK_INFO_FILELISTCREATEEND: // 'Finished creating file list'
                  Begin
                  End;
            End;

         ACE_CALLBACK_OPERATION_ANALYZE, //: ;
          //{Case Info^.Global.Code Of
            //End}

         ACE_CALLBACK_OPERATION_EXTRACT:

            Case Info^.Global.code Of
               ACE_CALLBACK_INFO_FILELISTCREATE: // 'Creating file list'
                     Me.fFileCount := 0;

         		ACE_CALLBACK_INFO_FILELISTADD: ;

               ACE_CALLBACK_INFO_FILELISTCREATEEND: ; // 'Finished creating file list';
            End;
      End;


      Case Info^.Global.code Of
         ACE_CALLBACK_INFO_FILELISTCREATE: ;
         ACE_CALLBACK_INFO_FILELISTCREATEEND:
               Me.fFileCount := 0;

         ACE_CALLBACK_INFO_FILELISTADD:
            Begin
            	//Info^.RealFile.Filename;
               inc(Me.fFileCount);
            End;
      Else
         Result := ACE_CALLBACK_RETURN_NO;
         Exit;
      End;
      Result := ACE_CALLBACK_RETURN_OK;
   End;
End;
//------------------------------------------------------------

Function CallAceInitDll(p, IProcPtr, EProcPtr, RProcPtr, SProcPtr: Pointer): Integer;
Var
   DllData: tACEInitDllStruc;
   zTempDir: Array[0..255] Of char;
Begin
   ZeroMemory(@DllData, SizeOf(DllData));
   DllData.GlobalData.obj := p;
   DllData.GlobalData.MaxArchiveTestBytes := $1FFFF;
   DllData.GlobalData.MaxFileBufSize := MaxFileBufSize;
   DllData.GlobalData.Comment.BufSize := SizeOf(CommentBuf) - 1;
   DllData.GlobalData.Comment.Buf := @CommentBuf;
   GetTempPath(255, @zTempDir[0]);
   DllData.GlobalData.TempDir := @zTempDir;
   DllData.GlobalData.InfoCallbackProc := IProcPtr;
   DllData.GlobalData.ErrorCallbackProc := EProcPtr;
   DllData.GlobalData.RequestCallbackProc := RProcPtr;
   DllData.GlobalData.StateCallbackProc := SProcPtr;
   Result := AceInitDllProc(@DLLData);
End;
//------------------------------------------------------------

Function RequestProc(Request: pACERequestCallbackProcStruc): Integer; Stdcall;

   Function doRequestArchive(ArcRequest: pACECallbackArchiveStruc): Integer;
   Begin
      If ArcRequest^.code = ACE_CALLBACK_REQUEST_CHANGEVOLUME Then
      Begin
         If Assigned(Me.OnNextVolume) Then
         Begin

            Me.VolumeName := Me.GetNextVolumeName(Me.fVolumeName,
               ArcRequest^.ArchiveData^.VolumeNumber-1);

            If Me.GetNextVolume(Me.fVolumeName,
               Integer(ArcRequest^.ArchiveData^.VolumeNumber)) Then
            Begin
               StrPCopy(ArcRequest^.ArchiveData^.ArchiveName, Me.fVolumeName);
               Me.fVolNum := ArcRequest^.ArchiveData^.VolumeNumber;

               If FileExists(Me.fVolumeName) Then
               Begin

                  If Me.GetArchiveTotals(Me.fVolumeName) Then
                  	Result := ACE_CALLBACK_RETURN_OK
                  Else
                  	Result := ACE_CALLBACK_RETURN_CANCEL;


               End Else
                  Result := ACE_CALLBACK_RETURN_NO;

            End
            Else
               Result := ACE_CALLBACK_RETURN_CANCEL;

         End
         Else
         Begin
            Me.RaiseErrorStr(Me.ArchiveFile, 'OnNextVolume', '0',
               E_REQUIREDEVENT);

            Result := ACE_CALLBACK_RETURN_CANCEL;
         End;

      End
      Else
         Result := ACE_CALLBACK_RETURN_CANCEL; // 'unknown request'

   End;                                 {doRequestArchive}


   Function doRequestArchivedFile(Request: pACECallbackArchivedFileStruc): Integer;
   Var
      i: Integer;
      TryAgain: Boolean;
   Begin

      Case Request^.code Of

         ACE_CALLBACK_REQUEST_OVERWRITE:
            Begin
               If Me.fOverwriteMode = omOverwrite Then
               Begin
                  SetFileAttributes(PChar(Request^.FileData^.DestinationFileName), FILE_ATTRIBUTE_NORMAL);
                  Result := ACE_CALLBACK_RETURN_OK
               End
               Else
                  Result := ACE_CALLBACK_RETURN_NO

            End;

         ACE_CALLBACK_REQUEST_PASSWORD:
            Try

               If Assigned(Me.OnGetPassword) Then
               Begin

                  TryAgain := True;     //default
                  For i := 0 To Me.PasswordAttempts - 1 Do
                  Begin

                     Me.OnGetPassword(Me, Request^.FileData^.SourceFileName,
                        Me.fPassword, TryAgain);

                     If (Not TryAgain) Or Me.Cancel Then
                     Begin
                        Me.RaiseErrorStr(Request^.FileData^.SourceFileName,
                           '', '0', M_INVALIDPW);

                        break;
                     End
                     Else
                        If (Me.fPassword = '') And (i = Me.PasswordAttempts - 1) Then
                           Me.RaiseErrorStr(Request^.FileData^.SourceFileName,
                              '', '0', M_PASSWORDFAILED)
                        Else
                           If (Me.fPassword <> '') Then
                           Begin
                              Result := ACE_CALLBACK_RETURN_OK;
                              Request^.GlobalData^.DecryptPassword := PChar(Me.fPassword);
                              Exit;
                           End;

                  End;

               End
               Else
                  Me.RaiseErrorStr(Request^.FileData^.SourceFileName,
                     'OnGetPassword', '0', E_REQUIREDEVENT);

               Result := ACE_CALLBACK_RETURN_CANCEL;

            Except
               Result := ACE_CALLBACK_RETURN_NO;
               Request^.GlobalData^.DecryptPassword := '';
            End;

      Else
         Result := ACE_CALLBACK_RETURN_CANCEL; // 'unknown request'
      End;
   End;                                 {doRequestArchivedFile}


Begin {RequestProc}
   Case Request^.StructureType Of
      ACE_CALLBACK_TYPE_GLOBAL:
         Result := ACE_CALLBACK_RETURN_CANCEL; // unknown request

      ACE_CALLBACK_TYPE_ARCHIVE:
         Result := doRequestArchive(@Request^.Archive);

      ACE_CALLBACK_TYPE_ARCHIVEDFILE:
         Begin
            Result := doRequestArchivedFile(@Request^.ArchivedFile);
            If (Result = ACE_CALLBACK_RETURN_OK) Then
            Begin
               Me.ActualFilename :=
                  ExtractFilePath(Me.ActualFilename) +
                  ExtractFilename(Me.FileName);
            End;
         End;

      ACE_CALLBACK_TYPE_REALFILE:
         Result := ACE_CALLBACK_RETURN_CANCEL; // unknown request

   Else
      Result := ACE_CALLBACK_RETURN_CANCEL;
   End;
End; {RequestProc}
//------------------------------------------------------------

Function StateProc(state: pACEStateCallbackProcStruc): Integer; Stdcall;

   Procedure doTypeArchive(dTA: tACECallbackArchiveStruc);
   Var
      TestStr: String;
      ArcFlag: Integer;
   Begin
      With dTA Do
         If (code = ACE_CALLBACK_STATE_STARTARCHIVE) And
            (Operation = ACE_CALLBACK_OPERATION_EXTRACT) Then
         Begin
            TestStr := ArchiveData^.ArchiveName;
            ArcFlag := state^.Archive.ArchiveData^.flags;
            If ArcFlag <> 0 Then ;
         End;
   End;                                 {doTypeArchive}

   Procedure doTypeArchivedFile(dTAF: tACECallbackArchivedFileStruc);
   Begin
      With dTAF Do
      Begin
         Case code Of

            ACE_CALLBACK_STATE_STARTFILE:
               Case Operation Of
                  ACE_CALLBACK_OPERATION_LIST:
                     Begin
                        Me.ActualFilename := FileData^.SourceFileName;
               			If CheckWildCard2(
                        	Me.ActualFilename,
                           Me.FileSpec,
                           Me.ExcludeSpec,
                           Me.RecurseDirs) Then
                        Begin
                           Me.fTotalPackedSize := Me.fTotalPackedSize + FileData^.CompressedSize;
                           Me.fTotalUnpackedSize := Me.fTotalUnpackedSize + FileData^.Size;
                           //Me.ProgressPosition := Me.fTotalUnpackedSize;
                           If FileData^.Time > Me.fMaxAge Then
                           	Me.fMaxAge := FileData^.Time;

                        	//StrCat(Me.FileSpecList, FileData^.SourceFileName);
                           //StrCat(Me.FileSpecList, #$d);
                           inc(Me.fFileCount);
                        End;
                     End;

                  ACE_CALLBACK_OPERATION_TEST:
                     Begin
                        Me.ActualFilename := StrPas(FileData^.SourceFileName);
                        Me.IsValidFilespec :=
                        	CheckWildCard2(
                              Me.ActualFilename,
                              Me.FileSpec,
                              Me.ExcludeSpec,
                              Me.RecurseDirs);

                        If Me.IsValidFilespec Then
                           If Me.doOnBegin(FileData^.Attributes And
                              FILE_ATTRIBUTE_DIRECTORY > 0) Then
                           Begin
                              Me.FileName := Me.ActualFilename;
                              Me.fFileBytesProcessed := 0;
                           End Else
                              Me.IsValidFilespec := False;

                     End;

                  ACE_CALLBACK_OPERATION_ANALYZE:
                     Begin
                        If code <> 0 Then ; // just for debug stop purposes
                      	//ShowMessage( StrPas( FileData^.SourceFilename ) );
                     End;

                  ACE_CALLBACK_OPERATION_EXTRACT:
                     Begin
                        Me.ActualFilename := StrPas(FileData^.SourceFileName);
                        // formatted filename
                        Me.FileName := Me.ActualFilename;
                        Me.IsRenamedFile := False;
                        Me.fExtractFilename := Me.FileName; // must appear prior to doOnBegin call

                        //ShowMessage(DateTimeToStr(FileDateToDateTime(FileData^.Time)));

                        // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
                        Me.GlobalDate := ztvConvertDate(FileData^.Time);
                        Me.LocalDate := FileData^.Time;
                        Me.AceFHeader.Size := FileData^.size;
                        Me.AceFHeader.PSize := FileData^.CompressedSize;
                        Me.AceFHeader.FTime := FileData^.Time;
                        Me.fExtractDirExists := _DirectoryExists(ExtractFilePath(Me.fExtractFileName));

                        If Not Me.doOnBegin(FileData^.Attributes And
                           FILE_ATTRIBUTE_DIRECTORY > 0) Then
                           Exit;

                        Me.IsRenamedFile :=
                           CompareText(Me.fFileName, Me.fExtractFilename) <> 0;

                     End;
               End;

            ACE_CALLBACK_STATE_ENDNOCRCCHECK:
               Begin
                  //ShowMessage('Here');
               End;
         End;
      End;
   End;                                 {doTypeArchivedFile}

   Procedure doTypeProgress(dTP: tACECallbackProgressStruc);
   Var
      f: TStream32;
   Begin

      With dTP Do
         Case Operation Of
            ACE_CALLBACK_OPERATION_LIST: ;

            ACE_CALLBACK_OPERATION_TEST:
               If Me.IsValidFilespec And (ProgressData^.Addr <> Nil) Then
               Begin
                  // for buffer searches, we need to route the decompressed
                  // buffer to the ExtractWriteBlock.
                  Me.ExtractWriteBlock(f, ProgressData^.Addr^, False, 0,
                     ProgressData^.size, dtData);

                  Me.ProgressPosition := Me.ProgressPosition -
                     u_long(ProgressData^.size);

                  Me.fFileBytesProcessed := Me.fFileBytesProcessed +
                     ProgressData^.size;

                  Me.doBranchProgress(Me.fFileBytesProcessed,
                     u_long(FileData^.size),
                     Me.fTotalUnpackedSize);

               End;

            ACE_CALLBACK_OPERATION_ANALYZE:
               Begin
                  If code <> 0 Then ;   // just for debug stop purposes
               End;

            ACE_CALLBACK_OPERATION_EXTRACT:
               Begin
                  If {Me.IsValidFilespec And} (ProgressData^.Addr <> Nil) Then
                  Begin

                     Me.ProgressPosition := Me.ProgressPosition -
                        u_long(ProgressData^.size);

                     Me.fFileBytesProcessed := Me.fFileBytesProcessed +
                        ProgressData^.size;

                     Me.doBranchProgress(Me.fFileBytesProcessed,
                        u_long(FileData^.size),
                        Me.fTotalUnpackedSize);

                  End;
               End;
         End;
   End;                                 {doTypeProgress}

   Procedure doTypeCrc(dTC: tACECallbackCRCCheckStruc);

   	// This function removes the dir tree which was created by the
      // extracted file (if the compressed filename had a dir).  This
      // function is safe in the removal of dir, in that if a file
      // exists in a dir which was not extracted, the RemoveDir
      // function fails and exists the loop.
      Procedure RemoveCreatedDirs;
      Var
         Dir, Dir1: String;
      Begin
      	Dir := ExtractFileDir(Me.fExtractFileName);
      	Dir1 := RemoveDirTail(Me.ExtractDir);
         While CompareText(Dir, Dir1) <> 0 Do
         Begin
         	If Not RemoveDir(Dir) Then
            	Break;  // dir contained a file... abort the process
            Dir := ExtractFileDir(Dir);
         End;
      End;

   Begin

      With dTC Do
         Case Operation Of
            ACE_CALLBACK_OPERATION_LIST:
               Begin
               End;

            ACE_CALLBACK_OPERATION_TEST:
               If Me.IsValidFilespec Then
                  Me.doOnEnd_Local(CRCOk, FILE_ATTRIBUTE_NORMAL);

            ACE_CALLBACK_OPERATION_ANALYZE:
               Begin
                  If code <> 0 Then ;   // just for debug stop purposes
               End;

            ACE_CALLBACK_OPERATION_EXTRACT:
               Begin

                  If Me.IsRenamedFile Then
                  Begin
                     // Assuming the following:
                     // 1. Original dir\file from archive:
                     //       c:\hold1\old_filename.txt  (fExtractFilename)
                     // 2. The renamed dir\file:
                     //       c:\hold2\new_filename.txt (fFileName)

{$IFDEF MSWINDOWS}
                    	MoveFileEx(
                       	PChar(Me.fExtractFilename),
                       	PChar(Me.fFileName),
                       	MOVEFILE_COPY_ALLOWED Or MOVEFILE_REPLACE_EXISTING);
{$ELSE}
                     // if new_filename.txt already exists, delete it (the
                     // following Delphi "RenameFile" function will not
                     // overwrite an existing file).
							DeleteFile(Me.fFileName);  // Kept
                     // rename the already decompressed and existing file
                     // old_filename.txt to new_filename.txt
                     MoveFile(PChar(Me.fExtractFilename), PChar(Me.fFileName)); // Kept
                     // delete old_filename.txt (new_filename now exists
                     // on disk.
                     DeleteFile(Me.fExtractFilename);
{$ENDIF}

                     // if old_filename is the last file in the dir,
                     // removed the dir.  When an .ace archive file-name
                     // also contains a dir-name, the dir will be created
                     // (if your Unace1.UseStoredDirs property is set to
                     // true).
                     //
                     // remove the created dir.
                     // Even though fExtractFilename has already been
                     // deleted by the previous call to DeleteFile, it's
                     // dir will still exist.
                     If (Not Me.fExtractDirExists) Then
								RemoveCreatedDirs();

                     // new_filename.txt
                     If Not Me.RestoreFileAttr Then
                        SetFileAttributes(
                           //PChar(Me.fExtractFilename),
                           // changed 05.15.03
                           PChar(Me.fFilename),
                           FILE_ATTRIBUTE_NORMAL)
                     Else
                        SetFileAttributes(
                           //PChar(Me.fExtractFilename),
                           // changed 05.15.03
                           PChar(Me.fFilename),
                           FileData^.Attributes);
                  End Else Begin
                     If Not Me.RestoreFileAttr Then
                        SetFileAttributes(
                           PChar(Me.fFileName),
                           FILE_ATTRIBUTE_NORMAL)
                     Else
                        SetFileAttributes(
                           PChar(Me.fFileName),
                           FileData^.Attributes);
                  End;

                  Me.doOnEnd_Local(CRCOk, FileData^.Attributes);

               End;

         End;                           {Case}
   End;                                 {doTypeCrc}

Begin	{StateProc}
   If Me.Cancel Then
      Result := ACE_CALLBACK_RETURN_CANCEL
   Else
   Begin
      Case state^.StructureType Of
         ACE_CALLBACK_TYPE_ARCHIVE:
            doTypeArchive(state^.Archive);

         ACE_CALLBACK_TYPE_ARCHIVEDFILE:
         	doTypeArchivedFile(state^.ArchivedFile);

         ACE_CALLBACK_TYPE_PROGRESS:
         	doTypeProgress(state^.Progress);

         ACE_CALLBACK_TYPE_CRCCHECK:
         	doTypeCrc(state^.CrcCheck);
      End;
      Result := ACE_CALLBACK_RETURN_OK;
   End;
End; {StateProc}
//-------------------------------------------------------------

Function ErrorProc(Error: pACEErrorCallbackProcStruc): Integer; Stdcall;
Begin
   Result := ACE_CALLBACK_RETURN_CANCEL;
End;
//------------------------------------------------------------

{Function CallACEList(ArchiveName: PChar): Integer;
Var
   i: Integer;
   List: tACEListStruct;
Begin
	i := CallAceInitDll(
   	@List,
      @InfoProc,
      @ErrorProc,
      @RequestProc,
   	@StateProc);

   If (i <> ACE_CALLBACK_RETURN_OK) Then
   Begin
      Result := ACE_CALLBACK_RETURN_CANCEL;
      Me.RaiseErrorStr(StrPas(ArchiveName), '', '0', E_ACEDLLERR);
   End Else Begin
      ZeroMemory(@List, SizeOf(List));
      List.Files.SourceDir := '';
      List.Files.FileList := '';
      List.Files.ExcludeList := '';
      List.Files.FullMatch := True;
      Result := AceListProc(ArchiveName, @List);
   End;
End;}
//------------------------------------------------------------

Function CallACEVerify(ArchiveName: PChar): Integer;
Var
	i: Integer;
   Verify: tACETestStruct;
Begin
	i := CallAceInitDll(
   	@Verify,
      @InfoProc,
      @ErrorProc,
      @RequestProc,
   	@StateProc);

   If (i <> ACE_CALLBACK_RETURN_OK) Or
      (Not Me.GetArchiveTotals(StrPas(ArchiveName))) Then
   Begin
      Result := ACE_CALLBACK_RETURN_CANCEL;
      Me.RaiseErrorStr(StrPas(ArchiveName), '', '0', E_ACEDLLERR);
   End Else Begin
      ZeroMemory(@Verify, SizeOf(Verify));
      Verify.Files.SourceDir := '';
      Verify.Files.FileList := Me.FileSpecList;         //p;
      Verify.Files.ExcludeList := '';
      Verify.Files.FullMatch := True;
      Verify.DecryptPassword := PChar(Me.Password);
      Result := AceTestProc(ArchiveName, @Verify);
   End;
End;
//------------------------------------------------------------

Function CallACEExtract(ArchiveName: PChar; DestinationDir: String;
   DecryptPassword: String; DoExcludePath: LongBool): Integer;
Var
	i: Integer;
   Extract: tACEExtractStruct;
   zDestinationDir: Array[0..255] Of char;
   zDecryptPassword: Array[0..255] Of char;
Begin
	i := CallAceInitDll(
   	@Extract,
      @InfoProc,
      @ErrorProc,
      @RequestProc,
   	@StateProc);

   If (i <> ACE_CALLBACK_RETURN_OK) Or
      (Not Me.GetArchiveTotals(StrPas(ArchiveName))) Then
   Begin
      Result := ACE_CALLBACK_RETURN_CANCEL;
      Me.RaiseErrorStr(StrPas(ArchiveName), '', '0', E_ACEDLLERR);
   End Else Begin
      ZeroMemory(@Extract, SizeOf(Extract));
      Extract.Files.SourceDir := '';
      Extract.Files.FileList := Me.FileSpecList;
      Extract.Files.ExcludeList := '';
      Extract.Files.FullMatch := True;     //False;
      Extract.DestinationDir := StrPCopy(zDestinationDir, DestinationDir);
      Extract.ExcludePath := DoExcludePath;
      Extract.DecryptPassword := StrPCopy(zDecryptPassword, DecryptPassword);
      Result := AceExtractProc(ArchiveName, @Extract);
   End;
End;
//-------------------------------------------------------------

Constructor TUnACE.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TUnACE.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TUnACE.doOnEnd_Local(CrcPass: Boolean; FileAttr: u_long);
Var
   f: THandle;
Begin
   If Assigned(OnEnd) Then
   Begin
      If WriteToFile Then
      Begin
         Try
				f := ztvOpenFileReadWrite(Self, PChar(fFileName));
            If f <> INVALID_HANDLE_VALUE Then
               Try
                  FileSetDate(f, GetDateTime(LocalDate));
               Finally
                  CloseHandle(f);
               End;

            If RestoreFileAttr And (FileAttr <> FILE_ATTRIBUTE_NORMAL) Then
               SetFileAttributes(PChar(fFileName), FileAttr);
         Finally
            OnEnd(Self, fFileName, CrcPass)
         End;
      End
      Else
         OnEnd(Self, ActualFilename, CrcPass);

   End
   Else
      If Not CrcPass Then
         RaiseErrorStr(fFileName, '', '0', E_CRCERROR);
End;
//-------------------------------------------------------------

Function TUnACE.GetArchiveTotals(ArcName: String): Boolean;
Var
   f: THandle;
   i: Integer;
   FileName: AnsiString;
   AceMHeader: TAceMHead;
   AceFHeader: TAceFHead;
   EmptyDirList: TStrings;
   SavePos: Cardinal;
   strm: THandleStream32;
Begin
   Result := OpenArchive(f, ArcName);
   If Result Then
   Begin
   	Try
         Try

            EmptyDirList := TStringList.Create();
            strm := THandleStream32.Create(f);
            Try

               fFileCount := 0;
               FilesListBufSize := 0;
               fTotalPackedSize := 0;
               fTotalUnpackedSize := 0;

               strm.Position := fOffsetStart;
               strm.Read(AceMHeader, SizeOf(AceMHeader));
               strm.Seek((fOffsetStart) + AceMHeader.AC.Head_Size + 4, soBeginning);

               While strm.Position < strm.size Do
               Begin
                  SavePos := Strm.Position;
                  strm.Read(AceFHeader, SizeOf(AceFHeader));

                  If AceFHeader.AC.HEAD_TYPE = ACE_FILE_BLK Then
                  Begin
                  	FileName :=
                     	ReadFilename_DefinedLen(strm, AceFHeader.FNAME_SIZE);

                     // 05.14.03
                     //SetLength(FileName, AceFHeader.FNAME_SIZE);
                     //strm.Read(FileName[1], AceFHeader.FNAME_SIZE);

                     If (AceFHeader.Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
                     Begin
                        If WriteToFile And CreateStoredDirs Then
                           EmptyDirList.Add(FileName);

                        With AceFHeader Do
                           Strm.Seek(SavePos + AC.Head_Size + pSize + 4, soBeginning);
                     End
                     Else
                     Begin
                        Try
                           If WriteToFile And CreateStoredDirs Then
                           Begin
                              i := EmptyDirList.IndexOf(ExtractFileDir(FileName));
                              If i > -1 Then
                                 EmptyDirList.Delete(i);
                           End;

                           If CheckWildCard2(FileName, FileSpec, ExcludeSpec, RecurseDirs) Then
                           Begin
                           	//StrCat(Me.FileSpecList, PChar(FileName));
                           	//StrCat(Me.FileSpecList, #$d);

                              fTotalPackedSize := fTotalPackedSize + AceFHeader.pSize;
                              fTotalUnpackedSize := fTotalUnpackedSize + AceFHeader.size;
                              ProgressPosition := fTotalUnpackedSize;
                              inc(fFileCount);
                              If Cardinal(AceFHeader.FTIME) > Cardinal(fMaxAge) Then
                                 fMaxAge := AceFHeader.FTIME;

                           End;
                        Finally
                           With AceFHeader Do
                              Strm.Seek(SavePos + AC.Head_Size + pSize + 4, soBeginning);
                        End;
                     End;
                  End
                  Else
                     break;

               End;

               If WriteToFile And CreateStoredDirs Then
                  For i := 0 To EmptyDirList.Count - 1 Do
                     CreateDirEx(SlashSep(ExtractDir, EmptyDirList[i]));

            Finally
               strm.Free();
               EmptyDirList.Free();
            End;
         Finally
            CloseHandle(f);
         End;
   	Except
      	Result := False;
      End;
   End Else Begin
   	Result := False;
      RaiseErrorStr(ArcName, '', '0', E_FOPEN);
   End;

End;
//-------------------------------------------------------------

Procedure TUnAce.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
Var
	i: Integer;
Begin
   Me := Self;
   FilesListBufSize := 0;
   fVolumeName := ArchiveFile;

   IsRenamedFile := False;
   IsValidFilespec := False;
   Me.doBranchProgress(0, 0, 0);

   If LoadAceDll() Then
   Begin
   	GetMem(FileSpecList, MaxFileBufSize);
      FileSpecList[0] := #0;

      If FileSpec.Count = 0 Then
      	StrCat(FileSpecList, '*')
      Else
         For i := 0 To FileSpec.Count - 1 Do
         Begin
            StrCat(FileSpecList, PChar(FileSpec.Strings[i]));
            StrCat(FileSpecList, #$d);
         End;

      Try
         If WriteToFile Then
            {i :=} CallACEExtract(PChar(fVolumeName), ExtractDir, Password,
               Not UseStoredDirs)
         Else
            {i :=} CallACEVerify(PChar(fVolumeName));

      Finally
      	FreeMem(FileSpecList);
         UnLoadAceDll();
      End
   End Else
      RaiseErrorStr(ArchiveFile, '', '0', E_ACEDLLERR);

End;
//------------------------------------------------------------

End.
