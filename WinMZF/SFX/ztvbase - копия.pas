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
Unit ztvBase;

Interface

Uses
   Windows,
   Dialogs,
   Controls,
   SysUtils,
   Forms,
   Classes,
   ztvRegister,
   ztvGbls,
   ztvHeaders,
   ztvSearchEngine,
   ztvStreams,
   ztvCrypt,
   ztvFileIo;


{$I ZipTV.inc}

Type
   TztvFileAttr = (fsZeroAttr, fsReadOnly, fsHidden, fsSysFile, fsVolumeID,
      fsDirectory, fsArchive, fsCompressed, fsEncrypted);
   TztvFileAttrs = Set Of TztvFileAttr;

   TCompressionMethod =
      (cmInMemory, cmTempFile);

   TCompressMethod =
      (cmStore,
{.$IFNDEF DEFLATE64}
		cmDeflate,
{.$ELSE}
// 		cmDeflate64,
{.$ENDIF}
      cmFuse,
{$IFDEF FROZEN1_SUPPORT}
      cmFrozen1,
{$ENDIF}
      cmFrozen5, cmFrozen6, {cmFrozen7,} cmMsZip, cmLzx, cmQuantum, cmUUEncode,
      cmXXEncode, cmBase64, cmTarred, cmTarGzip);

   TCompressMethodState =
      Set Of TCompressMethod;

   TCpuType =
      (cptAuto, cpt80286, cpt80386, cpt80486);

   TCustomSizeType =
   	(stBytes, stKB, stMB);

   TDataType =                          // Read/write types
   	(dtHeader, dtData);

   TDateAttribute =
      (daFileDate, daSysDate, daMaxFileDate);

   TFileAccessMethod =
      (faFile, faMemoryStream, faUserStream, faPointer, faNul, faSearch,
      faVerify, faAppend);

   THeaderStatus =
      (hsAdd, hsCopy, hsSkip, hsRename);

   THostOS =
      (osMsdos, osOS2, osWin32, osUnix, osMAC, osWinNT, osPrimos, osAppleGS,
      osAtari, osVaxVms, osNone);

   TOverwriteMode = (omOverwrite, omSkip);

   TPartType =
   	(pt1mb, ptFloppy_120mb, ptFloppy_144mb, ptFloppy_288mb,
      ptZipDisk_100mb, ptZipDisk_250mb, ptCDRom_650mb, ptCDRom_700mb,
      ptOtherSize, ptNoSplit);
   
   TReadType =
      (FromFile, FromMemoryStream, FromFileStream, FromPointer);

   TStoredDirNames =
      (	sdNone,
      	sdAbsolute,				// full dir
       	sdAbsoluteNoDrv,		// full dir minus the drive (ie.. c:)
       	sdAbsoluteNoRoot,		// full dir minus the root drv/dir
       	sdRelative,
      	sdRelativeStoreStart,// store first dir in a recurse dir operation.
         sdExplorer_Auto,     // automatically defines DefaultDir
         sdExplorer_UserDefineDefaultDir
                              // must set DefaultDir prior to compress. This
         							// is the method generally used in a drag drop
                              // operation because all file specs derive from
                              // a common root-dir.  Ie.. drag-drop operations
                              // from windows explorer, do no allow draging from
                              // multiple directories.  Set DefaultDir as the
                              // directory being dragged.
         {, sdCustom});

   TSwitch =
      (swAdd, swRefresh, swMove, swDelete, swRead);

   TNextVolumeName =
   	(nvChangeFilename, nvChangeExt);

   TztvSortOptions =
      (soUnsort, soByName, soByType, soBySize, soByTime);

Type
   TOnBegin =
      Procedure(Sender: TObject; FileName: AnsiString; Count: Integer;
      Var Extract: Boolean)
      Of Object;

   TOnChangeArchive =
      Procedure(Sender: TObject; ArchiveName: AnsiString; ArcType: TArcType)
      Of Object;

   TOnClearDisk =
      Procedure(Sender: TObject; Drive: AnsiString; RequiredSpace, FreeSpace,
      TotalSpace: TInteger8; Var Cancel: Boolean)
      Of Object;

   TOnCorruptZipHeader =
      Procedure(Sender: TObject; HeadFlag: THeaderTypeState;
      Var Cancel: Boolean)
      Of Object;

   //TOnCustomFilename =
   //	Procedure( Sender: TObject; Dir: AnsiString; Var Filename: AnsiString );

   TOnDeleteFile =
      Procedure(Sender: TObject; FileName: AnsiString; Count, MaxCount: Integer)
      Of Object;

   TOnElapsedTime =
      Procedure(Sender: TObject; ElapsedTime: Single)
      Of Object;

   TOnEnd =
      Procedure(Sender: TObject; FileName: AnsiString; CRC_PASS: Boolean)
      Of Object;

   TOnError =
      Procedure(Sender: TObject; FileName, ExtendedMsg,
      VolumeID: AnsiString; ECode: Integer)
      Of Object;

   TOnExcludeFile =
      Procedure(Sender: TObject; FileName: AnsiString)
      Of Object;

   TOnGetPassword =
      Procedure(Sender: TObject; FileName: AnsiString; Var Password: AnsiString;
      Var TryAgain: Boolean)
      Of Object;

   TOnGetZipFirstDisk =
      Procedure(Sender: TObject; Var Cancel: Boolean)
      Of Object;

   TOnGetZipNextDisk =
      Procedure(Sender: TObject; VolumeName: AnsiString; Var Cancel: Boolean)
      Of Object;

   TOnGetZipLastDisk =
      Procedure(Sender: TObject; Var Cancel: Boolean)
      Of Object;

   TOnFileExists =
      Procedure(Sender: TObject; FileName: AnsiString; FileDate: TDateTime;
      Var OverwriteMode: TOverwriteMode)
      Of Object;

   TUnBase_OnFileExists =
      Procedure(Sender: TObject; FileName: AnsiString; Var NewFileName: AnsiString;
      Var OverwriteMode: TOverwriteMode)
      Of Object;

   TOnInsertDisk =
      Procedure(Sender: TObject; VolumeName: AnsiString; Var Cancel: Boolean)
      Of Object;

	TOnNewVolume =
   	Procedure(Sender: TObject; NewVolumeName: String)
      Of Object;

   TOnNestedTarFile =
      Procedure(Sender: TObject; FileName: AnsiString; Var doUnTar: Boolean)
      Of Object;

   TOnNextVolume =
      Procedure(Sender: TObject; Var VolumeName: AnsiString; VolumeID: AnsiString;
      fExists: Boolean; Var Cancel: Boolean)
      Of Object;

   TOnNonWriteableArchive =
      Procedure(Sender: TObject; ArchiveFile: AnsiString; Var WriteToFile: Boolean)
      Of Object;

   TOnProgress =
      Procedure(Sender: TObject; ProgressByFile, ProgressByArchive: Byte)
      Of Object;

   TOnRead =
      Procedure(Sender: TObject; Offset, Filenum: Integer)
      Of Object;

   TOnRecurseDir =
      Procedure(Sender: TObject; Directory: AnsiString)
      Of Object;

   TOnRemoveTempfile =
      Procedure(Sender: TObject; FileName: AnsiString)
      Of Object;

   TOnRenameFile =
      Procedure(Sender: TObject; FileName: AnsiString; Var NewFileName: AnsiString)
      Of Object;

   TOnRenameDupeFile =
      Procedure(Sender: TObject; FileName: AnsiString; Var NewFileName: AnsiString;
      Var Rename: Boolean)
      Of Object;

   TOnReplaceFile =
      Procedure(Sender: TObject; FileName, NewFileName: AnsiString;
      Date, NewDate: TDateTime; FileSize, NewFileSize: Int64;
      Attr, NewAttr: Integer; Var Replace: Boolean)
      Of Object;

   TOnScanFileEvent =
      Procedure(Sender: TObject; FileName: AnsiString; FilesCount: longint;
      FilesSize: Int64)
      Of Object;

   TOnTotals =
      Procedure(Sender: TObject; UnpackSize, PackSize: Int64; Ratio,
      NumFiles: Integer)
      Of Object;

   TReadBlock =
      Function(f: TStream32; Stream: TStream32; Var Buf; IsEncrypted: Boolean;
      Size: Byte; Count: DWord; ReadType: TDataType): DWord
      Of Object;

   TProgress =
      Procedure(ProgressByFile, ProgressByArchive: Byte)
      Of Object;

   TReportDirChange =
      Procedure(CurrentDirectory: AnsiString)
      Of Object;

   TWriteBlock =
      Function(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
      Size: Byte; Count: DWord; WriteType: TDataType): DWord
      Of Object;


Type
   pFileListRec = ^TFileListRec;
   TFileListRec = Packed Record
      BitFlag: Word;
      CompressType: Cardinal;
      LocalHdrOffset: Int64;
      PackedSize: Int64;
      UnpackedSize: Int64;
      CRC: u_long;
      ExternalAttr: Integer;
   End;

   pInflateRec = ^TInflateRec;
   TInflateRec = Packed Record
      BitFlag: Word;
      CompressType: Cardinal;
      PackedSize: Int64;
      UnpackedSize: Int64;
   End;

   pHeaderInfo = ^THeaderInfo;
   THeaderInfo = Packed Record
      uSize: Int64;	// must be in64 for comparison between old and new files
      pSize: Int64;	// must be in64 for comparison between old and new files
      Date: Integer;
      Attr: Integer;
   End;

   pCompFileInfo = ^TCompFileInfo;
   TCompFileInfo = Packed Record
      FileName: AnsiString;
      Status: THeaderStatus;
      Offset: Int64;
      HighUnpackedSize: Word;
      HighPackedSize: Word;
      EncryptedHeader: Boolean;
      FileCommentLen: Integer;
      FileComment: PChar;
      ExtraFieldLen: Word;
      ExtraField: Pointer;
   End;

   THeaderObj = Class(TObject)
   Public
      MemSize: Byte;
      FileCount: Integer;
      Header: TList;
      DataLocation: TList;	//for a good example of TList sorting see VListView.pas
   End;

   pCompHeaderObj = ^TCompHeaderObj;
   TCompHeaderObj = Class(THeaderObj)
      FileList: TStringList;
      RootDirLen: TList;
      ArchiveComment: PChar;
      ArchiveCommentLen: Word;
      CommentChanged: Boolean;
      Procedure INIT;
      Procedure DONE;
      Procedure CLEAR_LIST;
      Function AddItem(InRec: TCompFileInfo; FileName: AnsiString;
         pHeader: Pointer; RootLength: smallint; HeadSize: Word): Boolean;
      Function FileLocationData(Index: Integer): pCompFileInfo;
   End;

   pUnBaseFileInfo = ^TUnBaseFileInfo;
   TUnBaseFileInfo = Packed Record
      FileAttr: Integer;
      DiskWithThisFile: Word;
      OffsetOfLocalHeader: Int64;
   End;

   TUnBaseHeaderObj = Class(THeaderObj)
      Procedure INIT;
      Procedure DONE;
      Procedure CLEAR_LIST;
      Function AddItem(InRec: TUnBaseFileInfo; pHeader: Pointer; HeadSize: Word): Boolean;
      Function FileLocationData(Index: Integer): pUnBaseFileInfo;
   End;

   TZipCommon = Class;
   TCompBase = Class;
   TUnBase = Class;

   TSearchRecAction =
   	Procedure(Index: Integer; Dir: AnsiString; FindData: TWin32FindData;
      	pHeaderObj: pCompHeaderObj)
      	Of Object;

   TDiskSpannObj = Class(TDiskCommon)
   Private
   Public
      ZipObj: TZipCommon;
      VolNum: Word;
		Function GetDisk(Var Strm: TStream32): Boolean;
      Procedure INIT(ZO: TZipCommon; RaiseError: TRaiseError;
			RaiseErrorStr: TRaiseErrorStr; WriteProtectErrProc, DiskInDrvErrProc:
         TOnDiskError; Var Abort: Boolean);
   End;

   TStreamHeader = Record
      Date: Integer;
      Attr: Integer;
      Size: Int64;
      Offset: Int64; 
      FileName: String[255];
   End;

   TDecompMemoryStream = Class(TMemoryStream32)
   Private
   Protected
      MemSize: Integer;
      FileList: TList;
      Function GetFileOffset(x: Integer): Int64;
      Function GetFileDate(x: Integer): Integer;
      Function GetFileSize(x: Integer): Int64;
      Function GetFileName(x: Integer): AnsiString;
   Public
      FileCount: Integer;
      Function AddItem(Const InRec: TStreamHeader): Boolean;
      Procedure ClearList;
      Procedure ClearMemory;
      Property FileOffset[x: Integer]: Int64 Read GetFileOffset;
      Property FileDate[x: Integer]: Integer Read GetFileDate;
      Property FileSize[x: Integer]: Int64 Read GetFileSize;
      Property FileName[x: Integer]: AnsiString Read GetFileName;
      Constructor Create;
      Destructor Destroy; Override;
   End;

   TztvTimer = Class(TObject)
   Private
      t1, t2, t3: Integer;
   Public
      ElapsedTime: Single;
      Procedure Start;
      Procedure Stop;
      Procedure Suspend;
      Procedure Resume;
   End;

   TZipCommon = Class(TRegisterZipTV)
   Private
      fAsciiTranslate: Boolean;
      fAttributes: TztvFileAttrs;
      fAttributesEx: TztvFileAttrs;
      fConfirmOverwrites: Boolean;
      fDateAttribute: TDateAttribute;
      fDeleteOptions: TDeleteOptions;
      fExcludeExts: TStrings;
      fExcludeSpec: TStrings;
      fExtractDir: AnsiString;
      fFileSpec: TStrings;
      fIncludeHiddenDirs: Boolean;
      fOnActivate: TNotifyEvent;
      fOnBegin: TOnBegin;
      fOnCorruptZipHeader: TOnCorruptZipHeader;
      //fOnCustomFilename: TOnCustomFilename;
      fOnDeactivate: TNotifyEvent;
      fOnDeleteFile: TOnDeleteFile;
      fOnEnd: TOnEnd;
      fOnElapsedTime: TOnElapsedTime;
      fOnError: TOnError;
      fOnExcludeFile: TOnExcludeFile;
      fOnFileExists: TOnFileExists;
      fOnGetPassword: TOnGetPassword;
      fOnGetZipFirstDisk: TOnGetZipFirstDisk;
      fOnGetZipLastDisk: TOnGetZipLastDisk;
      fOnGetZipNextDisk: TOnGetZipNextDisk;
      fOnInsertDisk: TOnInsertDisk;
      fOnNestedTarFile: TOnNestedTarFile;
      fOnNextVolume: TOnNextVolume;
      fOnProgress: TOnProgress;
      fOnRenameFile: TOnRenameFile;
      fOnRead: TOnRead;
      fOnRemoveTempfile: TOnRemoveTempfile;
      fOnTmpFileProgress: TOnProgress;
      fOnTmpFileMoveBegin: TTempFileMoveBegin;
      fOnTmpFileMoveEnd: TNotifyEvent;
      fPwAttempts: Integer;
      fRecurseDirs: Boolean;
      fStoredDirNames: TStoredDirNames;
      fUseStoredDirs: Boolean;
      Function GetCancel: Boolean; Virtual;
      Function FileInExcludeList(FileName, DefDir: AnsiString): Boolean;
      Function WinAttrToString(Attr: Integer; PadChar: Byte): AnsiString;
      Procedure SetCancel(SC: Boolean); Virtual;
      Procedure SetExcludeSpec(SES: TStrings);
      Procedure SetExtractDir(SED: AnsiString);
      Procedure SetFileSpec(SFS: TStrings);
      Procedure SetPasswords(SP: TStringList);
   Protected
      DiskManager: TDiskManager;
      doSearchRecAction: TSearchRecAction;
      doReportDirChange: TReportDirChange;
      fExternalAttr: Integer;
      fOverwriteMode: TOverwriteMode;
      fPassword: AnsiString;
      fPassptr: Word;
      fTempDir: AnsiString;
      fTransOemChar: Boolean;
      fVersionMin: Byte;
      fVersionMax: Byte;
      TempFilename: AnsiString;
      ReadMethod: TFileAccessMethod;
      ztvReadPTR: Pointer;
      ztvWritePTR: Pointer;
      Function Close_OutFile(s: TStream32): Boolean;
      Function CompressWriteProc(Var s: TStream32; Var Buf; IsEncrypted: Boolean;
      	Size: Byte; iCount: DWord; WriteType: TDataType): DWord; Virtual;
      Function ConvertDate(Date: Integer): TDateTime; Virtual;
		Function DecodeRarFileName(FileName: AnsiString; FileNameLen: Word): WideString;
      Function doGetNextVolume(Var s: TStream32; Var sName: AnsiString): Boolean;
      Function ExtractWriteProc(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
      	Size: Byte; iCount: DWord; WriteType: TDataType): DWord; Virtual;
      Function FormatFileName(sFilename, Dir: AnsiString): AnsiString;
		Function GetCustomSizeType: TCustomSizeType; Virtual;
      Function GetNextVolume(Var VolName: AnsiString; VolNum: Variant): Boolean;
      Function GetNextVolumeName(FileName: AnsiString; VolNum: Integer): AnsiString; Overload; Virtual;
		Function GetNextVolumeName(VolNum, LastVolumeInSet: Integer): AnsiString; Overload;
		Function GetNextZipVolume(Var LocalStream: TTempFileStream; VolNum, LastVolumeInSet:
       	Integer): Boolean; Virtual;
		Function GetPartType: TPartType; Virtual;
		Function GetPartSize: Cardinal; Virtual;
      Function GetVolumeSizeNames: TStrings; Virtual;
      Function ReadFilename(s: TStream32; pFilename: PChar; Len: Integer): Integer;
      Function ReadProc(s: TStream32; DummyStrm: TStream32; Var Buf; IsEncrypted:
         Boolean; Size: Byte; iCount: DWord; ReadType: TDataType): DWord; Virtual;
      Function ReadTempDir: AnsiString;
		Function Read64BitFieldHdr(s: TStream32; ExtFieldLen: Word;
      	HType: THeaderType): TCentral64Hdr;
      Function UnixAttrToStr(a: Integer; PadChar: Byte): AnsiString;
      Function WriteData(s: TStream32; Var Buf; Size: Byte; iCount: DWord;
      	WriteType: TDataType): DWord;
      Procedure doWriteError;
      Procedure SetCustomSizeType(SCST: TCustomSizeType); Virtual;
      Procedure SetExcludeExts(SEE: TStrings); Virtual;
      Procedure SetFilename(SFN: AnsiString); Virtual;
      Procedure SetPartType(SFS: TPartType); Virtual;
      Procedure SetPartSize(SKB: Cardinal); Virtual;
      Procedure SetTempDir(STD: AnsiString);
      Procedure SetVolumeSizeNames(SVSN: TStrings); Virtual;
      Procedure UpdateCrcBuffer(Size: Byte; Var Buf; iCount: Integer);
      Procedure UpdateEncryptBuffer(IsEncrypted: Boolean; Var Buf; iCount: Integer); Virtual;
      Property DeleteOptions: TDeleteOptions Read fDeleteOptions Write fDeleteOptions;
      Property ExcludeExts: TStrings Read fExcludeExts Write SetExcludeExts;
      Property FileSpec: TStrings Read fFileSpec Write SetFileSpec;
      Property OnExcludeFile: TOnExcludeFile Read fOnExcludeFile Write fOnExcludeFile;
      Property OnDeleteFile: TOnDeleteFile Read fOnDeleteFile Write fOnDeleteFile;
      Property RecurseDirs: Boolean Read fRecurseDirs Write fRecurseDirs;
      Property StoredDirNames: TStoredDirNames Read fStoredDirNames Write fStoredDirNames Default sdRelative;
      Property UseStoredDirs: Boolean Read fUseStoredDirs Write fUseStoredDirs Default True;
   Public
      fUnpackedSize: Int64;
      fPackedSize: Int64;
      Bytes_To_Go: Int64;
      CompressWriteBlock: TWriteBlock;
      Count: Integer;
      DefaultDir: AnsiString;
      DoProgress: TProgress;
      ExtractWriteBlock: TWriteBlock;
      fCRC: u_long;
      fEncrypted: Boolean;
      fFileName: AnsiString;
      fFilePos: Int64;
      FileComment: PChar;               // used by the TZipTV component
      fInternalAttr: Word;
      fMaxAge: Integer;
      fMinVersion: Word;
      fOffsetEnd: Int64;
      fPasswords: TStringList;
      fRatio: Byte;
      fsCompressType: AnsiString;
      fTotalPackedSize: Int64;
      fTotalUnpackedSize: Int64;
      fTotalRatio: Byte;
      fVolumeName: AnsiString;
      fVersionMadeBy: Word;
      fwCompressType: Word;
      GlobalDate: TDateTime;
      InflateRec: TInflateRec;
      PercentByFile: Byte;
      PercentByArchive: Byte;
      ProgressPosition: Int64;
      ReadBlock: TReadBlock;
      ZipTimer: TztvTimer;
      WriteMethod: TFileAccessMethod;
      UBFI: TUnBaseFileInfo;
      pUBFI: pUnBaseFileInfo;
      pCBFI: pCompFileInfo;
      pCancel: pBool;
      fCancel: Boolean;
      AceMHeader: TAceMHead;
      ArcHeader: TARC;
      ArjHeader: TARJHEADER;
      pArjHeader: ^TARJHEADER;
      GZipHeader: TGZipHeader;
      RarHeader: TRarHeader;			//used by TZipTV

      //RarHeaderData: TRARHeadData;	//used by TUnRar
      RarHeaderDataEx: TRARHeadDataEx;
      AceHeader: TAceHead;
      AceFHeader: TAceFHead;
      BhHeader: TBh;
      HAHeader: THA;
      LzhHeader: TLzh;

      ZooHeader: TZooDir;
      ZooDirHeader: TZooDirEntry;
      MSGZ: TMSGZ;
      MSGZMain: TMSGZMain;
      MsGZ_DirList: TStringList;
      PakHeader: TPak;
      Rar1Header: TRar1;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Function ArcTypeName: AnsiString;
      Function FileAttrToString(Attr: Integer; PadChar: Byte): AnsiString;
      Function GetCompressMethodStr(method, BitFlag: Word): AnsiString;
      Function GetDateTime(dt: Integer): Integer;
      Function GetFileType(FileName: AnsiString): AnsiString;
      Function GetVolumeSizeInBytesStr: String; Virtual;
      Function IsArcValid(ArcType: TArcType): Boolean;
      Function IsArcCompressable(ArcType: TArcType): Boolean;
      Function IsArcDecompressable(ArcType: TArcType): Boolean;
      Function IsArcSearchable(ArcType: TArcType): Boolean;
      Function IsArcVerifyable(ArcType: TArcType): Boolean;
      Function IsArcSplitable(ArcType: TArcType): Boolean;
      Function IsArchiveExt(Extension: AnsiString): Integer;
      Function IsCommentEditable(ArcType: TArcType): Boolean;
      Function IsZipped(ArcType: TArcType): Boolean;
      Function SearchFile(szFilename, szSrchStr: shortstring): Integer;
		Function SearchFileEx(szFileName: ShortString; szSrchStr: TStrings): Boolean;
      Function Unstore(Infile: TStream32; Var Outfile: TStream32; BitSize: Byte;
         Vol_Count: AnsiString; IR: TInflateRec): Int64;
      Function WriteToFile: Boolean;
      Procedure doBranchProgress(FileCurrent, FileTotal, ArchiveTotal: Int64);
      Procedure doTarGzip(OutFileName: AnsiString); Virtual;
      Procedure GlobalProgress(Sender: TObject);
      Procedure RaiseError(Const EClass: ExceptClass; FileName,
      	ExtendedMsg, VolumeID: String; ECode: Integer); Virtual;
      Procedure RaiseErrorStr(FileName, ExtendedMsg, VolumeID:
      	String; ECode: Integer); Virtual;
      Procedure RetProgress(ProgressByFile, ProgressByArchive: Byte);
      Procedure SetAttribute(Attr: TztvFileAttr; value: Boolean);
      Procedure SetAttributeEx(Attr: TztvFileAttr; value: Boolean);
      Procedure TempFileMoveFile(Sender: TTempFileStream; ToFileName, FromFileName:
      	AnsiString; SameDrive: Boolean; Size: Int64); Virtual;
      Procedure TempFileMoveBegin(Sender: TObject; Archive, TempFilename: AnsiString);
      Procedure TempFileMoveEnd(Sender: TObject);
		Procedure TempFileProgress(Sender: TObject);
      Property AsciiTranslation: Boolean Read fAsciiTranslate
         Write fAsciiTranslate Default False;
      Property Cancel: Boolean Read GetCancel Write SetCancel Default False;
      Property ConfirmOverwrites: Boolean Read fConfirmOverwrites
         Write fConfirmOverwrites Default False;
      Property CRC: u_long Read fCRC;
      Property CustomSizeType: TCustomSizeType Read GetCustomSizeType Write SetCustomSizeType Default stBytes;
      Property Date: TDateTime Read GlobalDate;
      Property DateAttribute: TDateAttribute Read fDateAttribute
         Write fDateAttribute Default daFileDate;
      Property Encrypted: Boolean Read fEncrypted Default False;
      Property ExcludeSpec: TStrings Read fExcludeSpec Write SetExcludeSpec;
      Property ExtractDir: AnsiString Read fExtractDir Write SetExtractDir;  //Stored False;
      Property ExternalAttr: Integer Read fExternalAttr Stored False;
      Property Attributes: TztvFileAttrs Read fAttributes Write fAttributes; //Stored False;
      Property AttributesEx: TztvFileAttrs Read fAttributesEx Write fAttributesEx; //Stored False;
      Property FileName: AnsiString Read fFileName Write SetFilename Stored False;
      Property IncludeHiddenDirs: Boolean Read fIncludeHiddenDirs Write fIncludeHiddenDirs Default False;
      Property InternalAttr: Word Read fInternalAttr;
      Property LengthOfFile: Int64 Read fLOF Write fLOF Stored False;
      Property MinVersion: Word Read fMinVersion;
      Property OnActivate: TNotifyEvent Read fOnActivate Write fOnActivate;
      Property OnBegin: TOnBegin Read fOnBegin Write fOnBegin;
      Property OnDeactivate: TNotifyEvent Read fOnDeactivate
         Write fOnDeactivate;
      Property OnEnd: TOnEnd Read fOnEnd Write fOnEnd;
      Property OnError: TOnError Read fOnError Write fOnError;
      Property OnFileExists: TOnFileExists Read fOnFileExists
         Write fOnFileExists;
      Property OnGetPassword: TOnGetPassword Read fOnGetPassword
         Write fOnGetPassword;
      Property OnGetZipFirstDisk: TOnGetZipFirstDisk Read fOnGetZipFirstDisk
         Write fOnGetZipFirstDisk;
      Property OnGetZipNextDisk: TOnGetZipNextDisk Read fOnGetZipNextDisk
         Write fOnGetZipNextDisk;
      Property OnGetZipLastDisk: TOnGetZipLastDisk Read fOnGetZipLastDisk
         Write fOnGetZipLastDisk;
      Property OnInsertDisk: TOnInsertDisk Read fOnInsertDisk
         Write fOnInsertDisk;
      Property OnNestedTarFile: TOnNestedTarFile Read fOnNestedTarFile
         Write fOnNestedTarFile;
      Property OnNextVolume: TOnNextVolume Read fOnNextVolume Write fOnNextVolume;
      Property OnProgress: TOnProgress Read fOnProgress Write fOnProgress;
      Property OnRead: TOnRead Read fOnRead Write fOnRead;
      Property OnRemoveTempfile: TOnRemoveTempfile Read fOnRemoveTempfile
         Write fOnRemoveTempfile;
      Property OnRenameFile: TOnRenameFile Read fOnRenameFile
      	Write fOnRenameFile;
      Property OnTmpFileMoveBegin: TTempFileMoveBegin Read fOnTmpFileMoveBegin
      	Write fOnTmpFileMoveBegin;
      Property OnTmpFileMoveEnd: TNotifyEvent Read fOnTmpFileMoveEnd
      	Write fOnTmpFileMoveEnd;
      Property OnTmpFileProgress: TOnProgress Read fOnTmpFileProgress
      	Write fOnTmpFileProgress;
      Property OnCorruptZipHeader: TOnCorruptZipHeader Read fOnCorruptZipHeader
         Write fOnCorruptZipHeader;
      //Property OnCustomFilename: TOnCustomFilename Read fOnCustomFilename Write fOnCustomFilename;
      Property OverwriteMode: TOverwriteMode Read fOverwriteMode
         Write fOverwriteMode Default omOverwrite;
      Property PackedSize: Int64 Read fPackedSize;
      Property PartType: TPartType Read GetPartType Write SetPartType Default ptNoSplit; //vsOtherSize;
		Property PartSize: Cardinal Read GetPartSize Write SetPartSize Default 65536;
      Property Password: String Read fPassword Write fPassword;
      Property Passwords: TStringList Read fPasswords Write SetPasswords;
      Property PasswordAttempts: Integer Read fPwAttempts Write fPwAttempts Default 3;
      Property Ratio: Byte Read fRatio;
      Property sCompressionMethod: String Read fsCompressType Stored False;
      Property TempDir: AnsiString Read ReadTempDir Write SetTempDir Stored False;
      Property TotalRatio: Byte Read fTotalRatio Stored False;
      Property TotalPackedSize: Int64 Read fTotalPackedSize Write
         fTotalPackedSize Stored False;
      Property TotalUnpackedSize: Int64 Read fTotalUnpackedSize Write
         fTotalUnpackedSize Stored False;
      Property VersionMadeBy: Word Read fVersionMadeBy;
      Property UnpackedSize: Int64 Read fUnpackedSize Stored False;
      Property VolumeName: AnsiString Read fVolumeName Write fVolumeName
         Stored False;
      Property VolumeSizeNames: TStrings Read GetVolumeSizeNames Write SetVolumeSizeNames;
      Property wCompressionMethod: Word Read fwCompressType Stored False;
   Published
      Property TranslateOemChar: Boolean Read fTransOemChar Write fTransOemChar
         Default True;
      Property OnElapsedTime: TOnElapsedTime Read fOnElapsedTime Write
         fOnElapsedTime;
   End;

   TCompBase = Class(TZipCommon)
   Private
	   CBFInew: TCompFileInfo;
      DeleteFileList: TStringList;
      fCompressMethod: TCompressMethod;
      fCompressMethodState: TCompressMethodState;
      fDeflateType: TDeflateType;
      fAddFiles: Boolean;
      fFileScanStatus: TOnScanFileEvent;
      fOnClearDisk: TOnClearDisk;
      fOnNonWriteableArchive: TOnNonWriteableArchive;
      fOnRecurseDir: TOnRecurseDir;
      fOnRenameDupeFile: TOnRenameDupeFile;
      fOnReplaceFile: TOnReplaceFile;
      // v4.6.8 removed.  Added StoreFilesOfType property
      //fStoreAlreadyCompressedFiles: Boolean;
      fStoreEmptySubDirs: Boolean;
      fStoreFilesOfType: TStrings;
      fSwitch: TSwitch;
      fVerifyBeforeDelete: Boolean;
   	HeadSize: Word;
      Procedure SetCompressMethod(SCM: TCompressMethod);
      Procedure SetRootDir(SID: AnsiString);
      Function doCompress: Integer;
		Function GetDefaultDir: AnsiString;
      Function SetDefaultValues: Boolean;
   Protected
      CFHeader: TCAB_FILE_HEADER;
      CopySuccess: Boolean;
      fCompressionMethod: TCompressionMethod;
      fDefaultExt: AnsiString;
      fGlobalCompressType: Word;
      FilesCompressed: Integer;
      FilesDeleted: Integer;
      fMasterExt: AnsiString;
      fRootDir: AnsiString;
      HPtr: Pointer;                    // pointer to compressors file header
      HSize: Integer;
      isNewArchive: Boolean;
      LastPos: Int64;
      PrevPos: Int64;
      Procedure CompressIT(pHeaderObj: pCompHeaderObj); Virtual;
      Procedure DecypherHeader(FileName: AnsiString); Virtual;
      Procedure doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
         pHeaderObj: pCompHeaderObj); Virtual;
      Procedure doRenameFile(Var sFilename: AnsiString;
         pHeaderObj: pCompHeaderObj);
      Procedure FileInArchive(FileSpecIndex: Integer; DiskFilename: AnsiString;
      	FindData: TWin32FindData; pHeaderObj: pCompHeaderObj);
      Procedure RefreshHeader(Index, ExtAttr: Integer; pHeaderObj:
         pCompHeaderObj); Virtual;
      Procedure FillHeaderData(InStreamSize, CompressStreamSize: Int64); Virtual;
      Procedure InitializeHeader(pCBFI: pCompFileInfo); Virtual;
      Procedure PackFile(Outfile: TStream32; Index: Integer;
         pHeaderObj: pCompHeaderObj; Var Result: Boolean); Virtual; //ABSTRACT;
      Procedure SearchRecProc(Index: Integer; Dir: AnsiString; FindData:
         TWin32FindData; pHeaderObj: pCompHeaderObj);
      Procedure SetArchiveFile(SFN: AnsiString); Override;
      Procedure SetDefaultExt(SDE: AnsiString);
      Procedure SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo); Virtual;
      Procedure SetFilename(SFN: AnsiString); Override;
      Procedure WriteZeroByteZipHeader(TempStream: TStream32); Virtual;
      Function AddFileProc(outStream: TStream32; Index: Integer; pHeaderObj:
         pCompHeaderObj): Boolean; Virtual;
      Function doOnBegin(Var sFilename: AnsiString; FileNumber: Integer;
         pHeaderObj: pCompHeaderObj): Boolean;
      Procedure doOnEnd; Virtual;
      Function GetFirst(Var FileName: AnsiString; s: TStream32; Var HeadType: Byte;
         pHeaderObj: pCompHeaderObj): Boolean; Virtual;
      Function GetHeadSize: Word; Virtual;
      Function GetLocalHeaderSize: Integer; Virtual;
      Function GetNext(Var FileName: AnsiString; strm: TStream32; HeadType: Byte):
         Boolean; Virtual;
      Function GetHeadPtr: Pointer; Virtual;
      Function HandleNonWriteableFile(FileName: AnsiString): Boolean;
      Function ReadHeader(strm: TStream32; Var FileName: AnsiString;
         HeadType: Byte): Boolean; Virtual;
      Function SetDeflateBitFlag(BitFlag: Word): Byte;
		Function StoreStream(inStream, outStream: TStream32; zsp:
			ztv_stream_plus): Boolean;
      Function Write64BitFieldHdr(s: TStream32; pCBFI: pCompFileInfo;
      	HType: THeaderType): Integer; Virtual;
   Public
      HeadInfo: THeaderInfo;
      Unpackable: Boolean;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure ArcToList(strm: TStream32; pHeaderObj: pCompHeaderObj); Virtual;
      //Function CalcChkSum: Integer; Virtual;
      Procedure CommentObjDone; Virtual;
      Procedure CompressArcType;
      Procedure doFinish(pHeaderObj: pCompHeaderObj); Virtual;
      Procedure ExecuteCompression(Var ArcFile: TStream32; TempFile: TStream32;
         pHeaderObj: pCompHeaderObj);
      Procedure RootDirChangeEvent(Sender: TObject; Dir: AnsiString);
      Procedure DoUpdateFileList(Const Dir: AnsiString; FindData: TWin32FindData;
         pHeaderObj: pCompHeaderObj);
      Procedure GetComment(Index: Integer; Var FileName: AnsiString; Var Comment:
         PChar; Var CommentLen: Word); Virtual;
      Procedure ReadFileInfo(pHeaderObj: pCompHeaderObj);
      Procedure SetComment(Index: Integer; Comment: PChar; CommentLen: Word);
         Virtual;
      Procedure SetArchiveComment(Comment: PChar; CommentLen: Word); Virtual;
      Function CommentObjInit: Integer; Virtual;
      Function doCleanUp(Outfile: TStream32; pHeaderObj: pCompHeaderObj): Boolean; Virtual;
      Function GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo; Virtual;
      Function GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
         Pointer): Int64; Virtual;
      Function Compress: Integer; Virtual;
      Function GetArchiveComment(Var Comment: PChar; Var CommentLen: Word):
         PChar; Virtual;
      Function GetFileComment(FileName: AnsiString; Comment: PChar; CommentLen: Word):
         Integer; Virtual;
      Function IsPasswordSupported(ArcType: TArcType): Boolean;

      // MUST contain '.' char.  Example: StoreFilesOfType.Add('.ZIP');
      Property StoreFilesOfType: TStrings Read fStoreFilesOfType Write fStoreFilesOfType;
      Property CompressionMethod: TCompressionMethod Read fCompressionMethod Write fCompressionMethod Default cmTempFile;
      Property CompressMethod: TCompressMethod Read fCompressMethod Write
         SetCompressMethod;
      Property CompressMethodState: TCompressMethodState Read
         fCompressMethodState Write fCompressMethodState;
      Property DefaultExt: AnsiString Read fDefaultExt Write SetDefaultExt;
      Property DeflateType: TDeflateType Read fDeflateType Write
         fDeflateType Default dtDeflateN;
      Property FileName: AnsiString Read fFileName Write SetFilename Stored False;
      Property RootDir: AnsiString Read fRootDir Write SetRootDir
         Stored False;
      Property OnClearDisk: TOnClearDisk Read fOnClearDisk Write fOnClearDisk;
      Property OnRenameDupeFile: TOnRenameDupeFile Read fOnRenameDupeFile Write
         fOnRenameDupeFile;
      Property OnReplaceFile: TOnReplaceFile Read fOnReplaceFile Write
         fOnReplaceFile;
      Property OnNonWriteableArchive: TOnNonWriteableArchive Read
         fOnNonWriteableArchive Write fOnNonWriteableArchive;
      // v4.6.8 removed StoreAlreadyCompressedFiles.  Added StoreFilesOfType property
      //Property StoreAlreadyCompressedFiles: Boolean Read		// v4.6.8 removed.  Added StoreFilesOfType property
      //   fStoreAlreadyCompressedFiles Write fStoreAlreadyCompressedFiles
      //   Default True;
      Property Switch: TSwitch Read fSwitch Write fSwitch;
      Property VerifyBeforeDelete: Boolean Read fVerifyBeforeDelete Write
         fVerifyBeforeDelete Default False;
      Property FileSpec;
      Property OnBegin;
      Property OnEnd;
      Property OnProgress;
      Property OnRead;
      Property StoredDirNames;
      Property StoreEmptySubDirs: Boolean Read fStoreEmptySubDirs Write
         fStoreEmptySubDirs Default False;
   Published
      Property ArchiveFile;
      Property Attributes;
      Property DateAttribute;
      Property DeleteOptions;
      Property ExcludeSpec;
      Property RecurseDirs Default True;
      Property OnActivate;
      Property OnDeactivate;
      Property OnDeleteFile;
      Property OnError;
      Property OnFileScanStatus: TOnScanFileEvent Read fFileScanStatus Write fFileScanStatus;
      Property OnRecurseDir: TOnRecurseDir Read fOnRecurseDir Write
         fOnRecurseDir;
   End;

   TUnBase = Class(TZipCommon)
   Private
      fCpuType: TCpuType;
      fCreateStoredDirs: Boolean;
      fOnChangeArchive: TOnChangeArchive;
      fOnFileExists: TUnBase_OnFileExists;
      fRestoreFileAttr: Boolean;
		Procedure Reset;
		Function UnBase_Execute(Var inStream: TStream32; Outfile: TStream32): Integer;
   Protected
      PasswordAttemptFailed: Boolean;
      HeaderList: TUnBaseHeaderObj;
		Procedure DestroyArcFile(Var inStream: TStream32; Outfile: TStream32);
      Procedure ExtractIT(Var Infile: TStream32; Outfile: TStream32); Overload; Virtual;
      Procedure SetArchiveFile(SAF: String); Override;
      Function doOnBegin(BypassReturnTrue: Boolean): Boolean;
      Function doOnEnd(BitSize: Byte; CRC: u_long): Boolean; Virtual;
		Function InitializeArcFile(Var inStream: TStream32; Outfile: TStream32): Boolean;
      Function Open_OutFile(Var Outfile: TStream32; FileName,
      	OriginalFilename: AnsiString): Boolean;
      Function RequestPassword(Infile: TStream32): Boolean;
      Function VerifyPassword(Buffer: PChar): Boolean; Virtual;
   Public
      ActualFilename: AnsiString;
      FilesToExtract: Integer;
      IsGzTarArchive: Boolean;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure AdjustProgress(InflateRec: TInflateRec);
      Procedure CloseAndSetDate(s: TStream32; FileName: AnsiString;
         dt, fa: Integer);
      Function Extract: Integer;
{$ifndef DEL6_OR_HIGHER}
      Function ExtractStreamToDisk(Source: TStream): Integer; Overload;
      Function ExtractStreamToDisk(Source: TStream32): Integer; Overload;
{$else}
      Function ExtractStreamToDisk(Source: TStream32): Integer;
{$endif}
      Function ExtractToNul: Integer;
      Function ExtractToVerify: Integer;
      Function ExtractToSearch: Integer;
      Function ExtractToPointer(FileName: AnsiString; p: Pointer): Integer;
      Function ExtractToMemoryStream(MemStrm: TStream32{TDecompMemoryStream}): Integer; //Overload;
      Function ExtractToFileStream(FileName: AnsiString; fStrm: TFileStream32): Integer;
      Property ConfirmOverwrites;
      Property CreateStoredDirs: Boolean Read fCreateStoredDirs Write
         fCreateStoredDirs Default False;
      Property OnChangeArchive: TOnChangeArchive Read fOnChangeArchive
         Write fOnChangeArchive;
      Property OnFileExists: TUnBase_OnFileExists Read fOnFileExists Write
         fOnFileExists;
      Property RestoreFileAttr: Boolean Read fRestoreFileAttr Write
         fRestoreFileAttr Default True;
      Property UseStoredDirs Stored True;
      Property ExtractDir;
      Property FileSpec;
      Property ZipCmntBufSize;
   Published
      Property ArchiveFile;
      Property CpuType: TCpuType Read fCpuType Write fCpuType;
      Property RecurseDirs Default True;
   End;


   (* Header Signatures *)
Const
   LHA_SIGNATURE = $6C2D;
   ZOO_SIGNATURE = $4F5A;               //dec value = 20314   79/90
   ARJ_SIGNATURE = $EA60;
   DCP_SIGNATURE = $35474B50;

   MS_GZ_HEADER_SIGNATURE = Integer(-1939514093);
   BLAKHOLE_SIGNATURE = $7054842;
   MAIN_RAR_HEADER_SIGNATURE = $21726152;
   //ZLIB_HEADER_SIGNATURE = DCP_SIGNATURE;

   LOCAL_FILE_HEADER_SIGNATURE = $04034B50;
   LOCAL_FILE_ENCRPT_SIGNATURE = $04034C50;
   LOCAL_CUST_HEADER_SIGNATURE = $04034D50;
   LOCAL_CUST_ENCRPT_SIGNATURE = $04034E50;

   CENTRAL_FILE_HEADER_SIGNATURE = $02014B50;
   CENTRAL_FILE_ENCRPT_SIGNATURE = $02014C50;
   CENTRAL_CUST_HEADER_SIGNATURE = $02014D50;
   CENTRAL_CUST_ENCRPT_SIGNATURE = $02014E50;

   CENTRAL_FILE_HEADER_DIGITAL = $05054b50;

   END_OF_CENTRAL_HEADER_SIGNATURE = $06054B50;
   END_OF_CENTRAL_ENCRPT_SIGNATURE = $06054C50;
   END_OF_CENTRAL64_HEADER_SIGNATURE = $06055B50;
   END_OF_CENTRAL64_ENCRPT_SIGNATURE = $06055C50;

   END_OF_CENTRAL_WZIP_HEADER_SIGNATURE = $06064B50;
   CENTRAL_WZIP_HEADER_LOCATOR = $07064b50;
   MULTIVOL_HEADER_SIGNATURE = $08074B50;

   CAB_SIGNATURE = $4643534D;
   CAB_SIGNATURE64 = $3e0c140048d104d;
   GZIP_HEADER_SIGNATURE = GZIP_MAGIC;
   ZIPTV_SFX_SIGNATURE = $5846535A;

Const
	SIXTYFOUR_BIT_HDR_ID_WZIP = $001;
	SIXTYFOUR_BIT_HDR_ID_ZIPTV = $0064;

   LDISK = $00;
   FDISK = $01;
   NDISK = $02;

   Z_NO_COMPRESSION = 0;
   Z_BEST_SPEED = 1;
   Z_BEST_COMPRESSION = 9;
   Z_DEFAULT_COMPRESSION = (-1);

   //( dtDeflateS, dtDeflateF, dtDeflateN, dtDeflateX );
   ZLevels: Array[TDeflateType] Of ShortInt = (
      Z_NO_COMPRESSION,
      Z_BEST_SPEED,
      Z_DEFAULT_COMPRESSION,
      Z_BEST_COMPRESSION
      );

   DriveNames: Array[TztvDriveType] Of String[9] = (
   	'Unknown', 'None', 'Floppy', 'Fixed', 'Network', 'CD-ROM', 'RAM',
      '3½ Floppy', '5¼ Floppy'
      );

Var
   Crc16Val: u_long;
   Crc32Val: u_long;
   is64Bit: Boolean;
   MaxDists: Byte;


Function AttributesToInt(Attr: TztvFileAttrs): Integer;
Function CheckWildCard1(FileName: AnsiString; FileSpec, ExcludeSpec: TStrings): Boolean;
Function CheckWildCard2(FileName: AnsiString; FileSpec, ExcludeSpec: TStrings;
	RecurseDirs: Boolean): Boolean;
Function CalcCheckSum(Header: TTarHeader): Integer;
Function CPassword(Op: Byte; pw: AnsiString; xor_value: Byte): AnsiString;
Function EraseFile(Const fName: AnsiString; method: TDeleteOptions): Boolean;
Function GetTempFilenameStr(TempDir: AnsiString): AnsiString;
Function GetNextVolName(s: AnsiString; fnum: Integer; FNextVolumeName: TNextVolumeName;
	FVolumeLongFilenames: Boolean): AnsiString;
Function is64BitEndingHdr(SignAtr: Integer): Boolean;
Function ValidateTarHeader(Buf: Pointer): Boolean;
Function ztvClosedImageIndex(FileName: AnsiString): Integer;
Function ztvOpenImageIndex(FileName: AnsiString): Integer;
Function ztvSystemImageIndex(FileName: AnsiString): Integer;
Procedure Crc16_buf(str: PChar; Len: Integer; Var CRC: u_long);
Procedure Crc32_buf(str: PChar; Len: Integer; Var CRC: u_long);


Implementation

Uses
   ShellApi,
   Err_Msgs,
   ztvInflate,
   ztvDeflate,
   ztvFileScan
   {$IFDEF DEL6_OR_HIGHER}
   ,Variants
   {$ENDIF};




(*************************************************************)
(*************************************************************)
(*                        TztvTimer                       	  *)
(*************************************************************)
(*************************************************************)

Procedure TztvTimer.Start;
Begin
   ElapsedTime := 0.0;
   t1 := GetTickCount();
End;
//-------------------------------------------------------------

Procedure TztvTimer.Stop;
Begin
   If t1 > 0 Then
   Begin
      t2 := GetTickCount();
      ElapsedTime := ((t2 - t1) / 1000);
      t1 := 0;
   End;
End;
//-------------------------------------------------------------

Procedure TztvTimer.Suspend;
Begin
   If t1 > 0 Then
   	t3 := GetTickCount();
End;
//-------------------------------------------------------------

Procedure TztvTimer.Resume;
Begin
   If (t1 > 0) And (t3 > 0) Then
   	t1 := t1 + Integer(GetTickCount()) - t3;
End;
//-------------------------------------------------------------


(*************************************************************)
(*************************************************************)
(*                      TDiskSpannObj                        *)
(*************************************************************)
(*************************************************************)

Procedure TDiskSpannObj.INIT(ZO: TZipCommon; RaiseError:
	TRaiseError; RaiseErrorStr: TRaiseErrorStr; WriteProtectErrProc,
   DiskInDrvErrProc: TOnDiskError; Var Abort: Boolean);
Begin
	fRaiseError := RaiseError;
	fRaiseErrorStr := RaiseErrorStr;
   fDiskInDrvErr := DiskInDrvErrProc;
   fWriteProtectErr := WriteProtectErrProc;
   VolNum := LDISK;
   ZipObj := ZO;
   ZipObj.pCancel := @Abort;
End;
//-------------------------------------------------------------

Function TDiskSpannObj.GetDisk(Var Strm: TStream32): Boolean;

   //--

   Function TestHead(Offset: Int64): Boolean;
   Var
      BytesRead: Integer;
   	Strm: TFileStream32;
   Begin
      Result := False;

      With ZipObj Do
      Try

         // initializes newly inserted diskette
         DiskManager.GetDriveInfo(fArchiveFile, RaiseError, RaiseErrorStr,
            OnDiskWriteProtectErr, OnDiskInDrvErr);

         Strm :=
            TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyWrite);

         If (Strm.Handle > -1) Then
            Try
               If Offset + SizeOf(TLocal) > Strm.Size Then
               Begin
                  Case fArcType Of
                     // get the volume
                     atZipDS:
                        If DiskManager.GetDriveInfo(
                        		fArchiveFile, RaiseError,
                        		RaiseErrorStr, OnDiskWriteProtectErr,
                              OnDiskInDrvErr) Then
                           Result := DiskManager.ZipVolNum = VolNum
                        Else
                           Result := False;

                     atZipMV: Result := True;
                  End;
               End Else Begin
                  Strm.Position := Offset;
                  With LocalZipHeader Do
                  Begin
                     BytesRead := Strm.Read(LocalZipHeader, SizeOf(SignAtr));
                     If BytesRead = SizeOf(SignAtr) Then
                        Result := (SignAtr = MULTIVOL_HEADER_SIGNATURE) Or
                           (SignAtr = LOCAL_FILE_HEADER_SIGNATURE) Or
                           (SignAtr = CENTRAL_FILE_HEADER_SIGNATURE);
                  End;
               End;
            Finally
               Strm.Free();
            End
         Else
            RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN);

      Except
      End;
   End;
   //--

   Function GetFirstDisk: Boolean;
   Var
      doCancel: Boolean;
   Begin
      With ZipObj Do
      Begin
         Result := False;
         While Not TestHead(pUBFI^.OffsetOfLocalHeader) Do
         Begin
            doCancel := True;
            fOnGetZipFirstDisk(Self, doCancel);
            If doCancel Then Exit;
         End;
         Inc(VolNum);
         Result := True;
      End;
   End;
   //--

   Function GetNextDisk: Boolean;
   Var
      doCancel: Boolean;
   Begin
   	Result := False;

      With ZipObj Do
      Begin
{$ifdef USELASTDISKEVENT}
         If (VolNum = EndZipHeader.NumberOfThisDisk + 1) Then
         Begin
            While (Not TestHead(pUBFI^.OffsetOfLocalHeader)) Do
            Begin
               doCancel := True;
               fOnGetZipLastDisk(Self, doCancel);
               If doCancel Then Exit;
            End;
            VolNum := LDISK;
         End Else Begin
{$endif USELASTDISKEVENT}
            While (Not TestHead(pUBFI^.OffsetOfLocalHeader)) Do
            Begin
               doCancel := True;
               fOnGetZipNextDisk(Self, IntToStr(VolNum), doCancel);
               If doCancel Then Exit;
            End;
            Inc(VolNum);
{$ifdef USELASTDISKEVENT}
         End;
{$endif USELASTDISKEVENT}
         Result := True;
      End;
	End;
   //--

   Function GetLastDisk: Boolean;
   Var
      doCancel: Boolean;
   Begin
      Result := False;
      With ZipObj Do
      Try
         fArcType := atNA;

         // clear header type
         HeaderTypeState := [];
         ArchiveFile := fArchiveFile;

         While (Not (htEnding In HeaderTypeState)) Do
         Begin
            doCancel := True;
            fOnGetZipLastDisk(Self, doCancel);
            If doCancel Then Exit;
            ArchiveFile := fArchiveFile;
         End;

         Result := IsArcValid(fArcType);
      Finally
         If Result Then
            VolNum := FDISK;
      End;
   End;
   //--

   Function isDiskEventAssigned: Boolean;
   Const
      DEvent: Array[LDISK..NDISK] Of AnsiString =
      ('OnGetZipLastDisk', 'OnGetZipFirstDisk', 'OnGetZipNextDisk');
   Var
      DiskMethod: Byte;
   Begin
   	With ZipObj Do
      Begin

         If VolNum = LDISK Then
         Begin
            Result := Assigned(fOnGetZipLastDisk);
            DiskMethod := LDISK;
         End Else Begin

            If VolNum = FDISK Then
            Begin
               DiskMethod := FDISK;
               Result := Assigned(fOnGetZipFirstDisk);
            End Else Begin
{$ifdef USELASTDISKEVENT}
               If (VolNum = EndZipHeader.NumberOfThisDisk + 1) Then
               Begin
                  DiskMethod := LDISK;
                  Result := Assigned(fOnGetZipLastDisk);
               End Else Begin
{$endif USELASTDISKEVENT}
                  DiskMethod := NDISK;
                  Result := Assigned(fOnGetZipNextDisk);
{$ifdef USELASTDISKEVENT}
               End;
{$endif USELASTDISKEVENT}
            End;
         End;

         If (Not Result) Then
            RaiseErrorStr(ArchiveFile, DEvent[DiskMethod], IntToStr(VolNum),
               E_REQUIREDEVENT);

      End {With ZipObj};
   End;

Begin	{ GetDisk(...) }
   Result := False;
   Strm.Free();
   Strm := Nil;

   With ZipObj Do
   Begin

   	If isDiskEventAssigned() Then
         Case VolNum Of
            0:	Result := GetLastDisk();
            1: Result := GetFirstDisk();
         Else
            Result := GetNextDisk();
         End;

      If Result Then
      Begin
            Strm :=
               TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyWrite);

            If (TFileStream(Strm).Handle < 0) Then
            Begin
               RaiseErrorStr(ArchiveFile, '', '0', E_FOPEN);
               Result := False;
               Exit;
            End;

            fLOF := Strm.Size;
      End Else
         pCancel^ := True;

   End;
End;

(*************************************************************)
(*************************************************************)
(*                       TCompHeaderObj                      *)
(*************************************************************)
(*************************************************************)

Procedure TCompHeaderObj.INIT;
Begin
   MemSize := SizeOf(TCompFileInfo);
   DataLocation := TList.Create();      //!! Create-Check: TRY/FINALLY
   Header := TList.Create();
   FileList := TStringList.Create();
   RootDirLen := TList.Create();
   CLEAR_LIST();
End;
//-------------------------------------------------------------

Procedure TCompHeaderObj.CLEAR_LIST;
Var
   i: Integer;
Begin
   For i := 0 To Header.Count - 1 Do
      Dispose(Header[i]);

   For i := 0 To DataLocation.Count - 1 Do
   Begin
      If FileLocationData( i )^.ExtraFieldLen > 0 Then
      	FreeMem( FileLocationData( i )^.ExtraField, FileLocationData( i )^.ExtraFieldLen + 1 );

      If FileLocationData(i)^.FileCommentLen > 0 Then
         FreeMem(FileLocationData(i)^.FileComment, FileLocationData(i)^.FileCommentLen + 1);

      Dispose(FileLocationData(i));
   End;

   If ArchiveCommentLen > 0 Then
   Begin
      FreeMem(ArchiveComment, ArchiveCommentLen);
      ArchiveCommentLen := 0;
   End;
   CommentChanged := False;

   Header.Clear();
   FileList.Clear();
   RootDirLen.Clear();
   DataLocation.Clear();
   FileCount := 0;
End;
//-------------------------------------------------------------

Procedure TCompHeaderObj.DONE;
Begin
   CLEAR_LIST();
   Header.Free();
   FileList.Free();
   RootDirLen.Free();
   DataLocation.Free();
End;
//-------------------------------------------------------------

Function TCompHeaderObj.AddItem(InRec: TCompFileInfo; FileName:
   AnsiString; pHeader: Pointer; RootLength: smallint; HeadSize: Word): Boolean;
Var
   DataPtr, DataPtr1: Pointer;
Begin
   Result := True;
   Try
      FileList.Add(FileName);
      RootDirLen.Add(Pointer(RootLength));

      GetMem(DataPtr, MemSize);
      CopyMem(@InRec, DataPtr, MemSize);
      { Integer := } DataLocation.Add(DataPtr);

      If (HeadSize > 0) And (pHeader <> Nil) Then
      Begin
         GetMem(DataPtr1, HeadSize);
         CopyMem(pHeader, DataPtr1, HeadSize);
         { Integer := } Header.Add(DataPtr1);
      End;

      Inc(FileCount);
   Except
      Result := False;
   End;
End;
//-------------------------------------------------------------

Function TCompHeaderObj.FileLocationData(Index: Integer): pCompFileInfo;
Begin
   Result := pCompFileInfo(DataLocation[Index]);
End;
//-------------------------------------------------------------

(*************************************************************)
(*************************************************************)
(*                      TUnbaseHeaderObj                  	  *)
(*************************************************************)
(*************************************************************)

Procedure TUnBaseHeaderObj.INIT;
Begin
   MemSize := SizeOf(TUnBaseFileInfo);
   DataLocation := TList.Create();      //!! Create-Check: TRY/FINALLY
   Header := TList.Create();
   CLEAR_LIST();
End;
//-------------------------------------------------------------

Function TUnBaseHeaderObj.AddItem(InRec: TUnBaseFileInfo; pHeader: Pointer;
   HeadSize: Word): Boolean;
Var
   DataPtr, DataPtr1: Pointer;
Begin
   Result := True;
   Try
      GetMem(DataPtr, MemSize);
      CopyMem(@InRec, DataPtr, MemSize);
      { Integer := } DataLocation.Add(DataPtr);

      If (HeadSize > 0) And (pHeader <> Nil) Then
      Begin
         GetMem(DataPtr1, HeadSize);
         CopyMem(pHeader, DataPtr1, HeadSize);
         { Integer := } Header.Add(DataPtr1);
      End;

      Inc(FileCount);
   Except
      Result := False;
   End;
End;
//-------------------------------------------------------------

Function TUnBaseHeaderObj.FileLocationData(Index: Integer): pUnBaseFileInfo;
Begin
   Result := pUnBaseFileInfo(DataLocation[Index]);
End;
//-------------------------------------------------------------

Procedure TUnBaseHeaderObj.CLEAR_LIST;
Var
   i: Integer;
Begin
   For i := 0 To Header.Count - 1 Do
      Dispose(Header[i]);

   For i := 0 To DataLocation.Count - 1 Do
      Dispose(FileLocationData(i));

   Header.Clear();
   DataLocation.Clear();
   FileCount := 0;
End;
//-------------------------------------------------------------

Procedure TUnBaseHeaderObj.DONE;
Begin
   CLEAR_LIST();
   Header.Free();
   DataLocation.Free();
End;

(*************************************************************)
(*************************************************************)
(*                      TDecompMemoryStream                 	  *)
(*************************************************************)
(*************************************************************)

Constructor TDecompMemoryStream.Create;
Begin
   Inherited Create;
   FileList := TList.Create();          {!! Create-Check: TRY/FINALLY }
   MemSize := SizeOf(TStreamHeader);
End;
//-------------------------------------------------------------

Destructor TDecompMemoryStream.Destroy;
Begin
   ClearList();
   FileList.Free();
   FileCount := 0;
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TDecompMemoryStream.GetFileOffset(x: Integer): Int64;
Begin
   Result := TStreamHeader(FileList.Items[x]^).Offset;
End;
//-------------------------------------------------------------

Function TDecompMemoryStream.GetFileDate(x: Integer): Integer;
Begin
   Result := TStreamHeader(FileList.Items[x]^).Date;
End;
//-------------------------------------------------------------

Function TDecompMemoryStream.GetFileSize(x: Integer): Int64;
Begin
   Result := TStreamHeader(FileList.Items[x]^).Size;
End;
//-------------------------------------------------------------

Function TDecompMemoryStream.GetFileName(x: Integer): AnsiString;
Begin
   Result := TStreamHeader(FileList.Items[x]^).FileName;
End;
//-------------------------------------------------------------

Procedure TDecompMemoryStream.ClearList;
Var
   i: Integer;
Begin
   For i := 0 To FileCount - 1 Do
      If FileList.Items[i] <> Nil Then
         FreeMem(FileList.Items[i], MemSize);
   FileList.Clear();
   FileCount := 0;
End;
//-------------------------------------------------------------

Function TDecompMemoryStream.AddItem(Const InRec: TStreamHeader): Boolean;
Var
   DataPtr: Pointer;
Begin
   GetMem(DataPtr, MemSize);
   CopyMem(@InRec, DataPtr, MemSize);   { move Data to Pointer                 }
   { Integer := } FileList.Add(DataPtr); {!! wrap in TRY / FINALLY and give FNC-Result True/False on ADD error }
   { If Add-Ok then } Inc(FileCount);   { Increase total positions counter     }
   AddItem := True;                     {!! see Add-Check above }
End;
//-------------------------------------------------------------

Procedure TDecompMemoryStream.ClearMemory;
Begin
   ClearList();
   Clear();
End;
//-------------------------------------------------------------



(*************************************************************)
(*************************************************************)
(*                        TZipCommon                     	  *)
(*************************************************************)
(*************************************************************)

Constructor TZipCommon.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   //PartType := ptNoSplit;
   //PartSize := 65536;
   CompressWriteBlock := CompressWriteProc;
   DeleteOptions := doAllowUndo;
   DefaultDir := '';
   DiskManager := TDiskManager.Create();
   DoProgress := RetProgress;
   ExtractWriteBlock := ExtractWriteProc;
   fCancel := False;
   fAsciiTranslate := False;
   fAttributes := [fsZeroAttr, fsArchive, fsReadOnly, fsCompressed, fsEncrypted];
   fConfirmOverwrites := False;
   fDateAttribute := daFileDate;
   fExcludeSpec := TStringList.Create();
   fExcludeExts := TStringList.Create();
   fFileSpec := TStringList.Create();
   fIncludeHiddenDirs := False;
   fOverwriteMode := omOverwrite;
   fPasswords := TStringList.Create();
   fPwAttempts := 3;
   fStoredDirNames := sdRelative;
   fTransOemChar := True;
   fUseStoredDirs := True;
   fVersionMax := cMAXVER;
   fVersionMin := cMINVER;
   pCancel := @fCancel;
   ReadBlock := ReadProc;
   //TempDir := GetTempPathStr;  //DO NOT CHANGE TempDir to FTempDir... will cause a major problem!
   ZipTimer := TztvTimer.Create();
End;
//-------------------------------------------------------------

Destructor TZipCommon.Destroy;
Begin
   ZipTimer.Free();
   fPasswords.Free();
   fFileSpec.Free();
   fExcludeSpec.Free();
   fExcludeExts.Free();
   DiskManager.Free();
   Inherited Destroy();
End;
//-------------------------------------------------------------

Function TZipCommon.Read64BitFieldHdr(s: TStream32; ExtFieldLen: Word;
	HType: THeaderType): TCentral64Hdr;
Begin
   ZeroMemory(@Result, SizeOf(TCentral64Hdr));
   While ExtFieldLen > 0 Do
   Begin
      // ============
      // read 2 parts
      // ============
      // 1. HeadID & Head-Size (TExtendedFldID). Suppose to be common
      //    to all data placed in the extendedfield header area
      // 2. TCentral64Hdr.HiPackedSize, TCentral64Hdr.HiUnpackedSize, and
      //    TCentral64Hdr.HiOffsetToLocal (all 3 variables should be type
      //    word)
      // =========================================================
      s.Read(Result.ExtendedFieldHdr, SizeOf(TExtendedFieldHdr));
      Dec(ExtFieldLen, SizeOf(TExtendedFieldHdr));

      Case Result.ExtendedFieldHdr.HeadID Of
         SIXTYFOUR_BIT_HDR_ID_WZIP:		// Huge file extended data HeadID
         	Begin
            	// WinZip compatibility issue
            	// WinZip's extended header (for filesizes greater than
               // 4 gig) differs from ZipTV's as follows.
               //
               // ZipTV's - huge file - extended header record:
               //   HeaderID: word;   		(equals $64)
               //   HeadSize: word;   		(2 bytes)
               //   HiPackedSize: Word;   (2 bytes)
      			//   HiUnpackedSize: Word; (2 bytes)
               //
               //
               // WinZip's - huge file - extended header record:
               // NOTE: central header entended record does not always match
               //       the local extended header)
               //   HeaderID: word;    	(equals $1)
               //   HeadSize: word;    	(2 bytes)
               //   PackedSize: Int64; 	(8 bytes)
               //   UnpackedSize: Int64; 	(8 bytes)
               //
               // The reason I include the above records is that in the following
               // code block, we make an adjustment for the differences between
               // the HiPackedSize & PackedSize and HiUnpackedSize & UnpackedSize
               // variable sizes: Integer vs Word (not Word vs Int64) as it
               // appears by viewing the above records.  See the following
               // code block.

               // Of the 8 bytes (size for PackedSize & UnpackedSize) we read
               // actually read 6... an cardinal (4 bytes) for the high order
               // portion of the PackedSize/UnpackedSize varibles + a word (2
               // bytes) for the low order of these variables.
               //
               // I feel confortable with this conversion in that it will work
               // until a files size exceeds 9223372036854775807 * 65536.  I
               // really don't expect to see file sizes this large during my
               // lifetime.
               //
               Case HType Of
               	htLocal:
                  	With LocalZipHeader Do
                  	Begin
                     	If zc.UnpackedSize = MAXDWORD Then
                        Begin
                        	s.Read(zc.UnpackedSize, SizeOf(zc.UnpackedSize));
                        	s.Read(Result.HiUnpackedSize, SizeOf(Result.HiUnpackedSize));
                        	s.Seek(SizeOf(Word), soCurrent);
                        End;

                     	If zc.PackedSize = MAXDWORD Then
                        Begin
                        	s.Read(zc.PackedSize, SizeOf(zc.PackedSize));
                        	s.Read(Result.HiPackedSize, SizeOf(Result.HiPackedSize));
                        	s.Seek(SizeOf(Word), soCurrent);
                        End;
                     End;
                  htCentral:
                     With CentralZipHeader Do
                  	Begin
                     	If zc.UnpackedSize = MAXDWORD Then
                        Begin
                           s.Read(zc.UnpackedSize,
                              SizeOf(zc.UnpackedSize));

                        	s.Read(Result.HiUnpackedSize, SizeOf(Result.HiUnpackedSize));
                        	s.Seek(SizeOf(Word), soCurrent);
                        End;

                     	If zc.PackedSize = MAXDWORD Then
                        Begin
                           s.Read(zc.PackedSize, SizeOf(zc.PackedSize));

                           s.Read(Result.HiPackedSize, SizeOf(Result.HiPackedSize));
                           s.Seek(SizeOf(Word), soCurrent);
                        End;

                        If RelativeOffsetOfLocalHeader = MAXDWORD Then
                        Begin
                           s.Read(RelativeOffsetOfLocalHeader,
                              SizeOf(RelativeOffsetOfLocalHeader));

                           s.Read(Result.HiOffsetToLocal, SizeOf(Result.HiOffsetToLocal));
                           s.Seek(SizeOf(Word), soCurrent);
                        End;

                     End;
               End;
            End;

         SIXTYFOUR_BIT_HDR_ID_ZIPTV:		// Huge file extended data HeadID
            Begin
            	// read remainder of the TCentral64Hdr record
            	Case HType Of
               	htLocal:
                     s.Read(
                        Result.HiPackedSize,
                        SizeOf(TLocal64Hdr) - SizeOf(TExtendedFieldHdr));
                  htCentral:
                     s.Read(
                        Result.HiPackedSize,
                        SizeOf(TCentral64Hdr) - SizeOf(TExtendedFieldHdr));
               End;
               //Break;  // not supporting any other extended headers
            End;
         Else
            // read next extended field header (if .HeadSize < remaining ExtFieldLen)
            If Result.ExtendedFieldHdr.HeadSize <= ExtFieldLen Then
               s.Seek(
                  Result.ExtendedFieldHdr.HeadSize,
                  soCurrent)
            Else Begin
               // determined not to be a valid header identifier... set file-pointer
               // to end of extra field and exit
               s.Seek(ExtFieldLen, soCurrent);
               Break;
            End;
      End;
      Dec(ExtFieldLen, Result.ExtendedFieldHdr.HeadSize);
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.SearchFile(szFilename, szSrchStr: shortstring): Integer;
Var
   Simple: TSingleFindSearchObj;
Begin
   Simple := TSingleFindSearchObj.Create(Nil);
   Simple.INIT_FILE_SEARCH(szSrchStr, True, False);
   Try
      Result := Simple.SEARCH_FILE_FIRSTMATCH(fArchiveFile);
   Finally
      Simple.DONE();
      Simple.Free();
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.SearchFileEx(szFileName: ShortString; szSrchStr: TStrings): Boolean;
Var
   i, FoundPos: Integer;
   FoundStrNum: Byte;
   CaseSensitive: Boolean;
   MultiSearch: TMultiTurboSearchObj;
Begin
   MultiSearch := TMultiTurboSearchObj.Create( Nil );
   MultiSearch.INIT_MULTIFILE_SEARCH( TSSort_None, True );
   Try
   	CaseSensitive := False;
   	For i := 0 to szSrchStr.Count - 1 Do
   		MultiSearch.ADD_SEARCHSTRING(szSrchStr[i], TSCombi_MUST, CaseSensitive);

      Result := MultiSearch.SEARCH_FILE_FIRSTMATCH( szFileName, FoundPos, FoundStrNum );
   	{ShowMessage(
      	'Pos: ' +
         ztvGbls.FixedLengthStr(FoundPos) + ' "' +
      	MultiSearch.GETSEARCHSTRING_BY_NUMBER(FoundStrNum) +
      	'" at File: ' +
         szFileName);}
   Finally
      MultiSearch.DONE( );
      MultiSearch.Free( );
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.Unstore(Infile: TStream32; Var Outfile: TStream32; BitSize: Byte;
   Vol_Count: AnsiString; IR: TInflateRec): Int64;
Var
   Size: Int64;
   Buffer: PChar;
   MemSize,
   	MemSize1: Integer;
   BytesRead: Cardinal;
Begin
   Size := IR.UnpackedSize;

   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //MemSize := Min(Size, WSIZE);
   If Size > WSIZE{4096} Then
      MemSize := WSIZE
   Else
      MemSize := Size;

   GetMem(Buffer, MemSize + 1);
   Try
      While Size > 0 Do
      Begin

         // use the following block, instead of the min function... the min
         // function fails with files > 4 gig.
         If Size > WSIZE Then
            MemSize1 := WSIZE
         Else
            MemSize1 := Size;

         If (ArcType In [atArj, atArjExe]) Then
            BytesRead := ReadBlock(Infile, Nil, Buffer[0], IR.BitFlag And 1 > 0,
               0, MemSize1, dtData)
         Else
            BytesRead := ReadBlock(Infile, Nil, Buffer[0], False,
               0, MemSize1, dtData);

         If BytesRead = 0 Then
         	Break;

         Dec(Size, BytesRead);
         ProgressPosition := ProgressPosition - BytesRead;

         If (ArcType In [atBh..atBhExe, atZip..atZipMV, atJar..atJarExe]) Then
            ExtractWriteBlock(Outfile, Buffer[0], IR.BitFlag And 1 > 0,
               BitSize, BytesRead, dtData)
         Else
            ExtractWriteBlock(Outfile, Buffer[0], False, BitSize,
            	BytesRead, dtData);

         If IR.UnpackedSize > Size Then
            doBranchProgress(IR.UnpackedSize - Size, IR.UnpackedSize, fTotalPackedSize);

         If Cancel Then Break;
      End;
   Finally
      FreeMem(Buffer, MemSize + 1);
      Result := Size;
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.WriteToFile: Boolean;
Begin
   Result := (WriteMethod = faFile);
End;
//-------------------------------------------------------------

Function TZipCommon.GetPartType: TPartType;
Begin
	Result := ptNoSplit;  // virtual method... do not delete!
End;
//-------------------------------------------------------------

Function TZipCommon.GetPartSize: Cardinal;
Begin
	Result := 65536;		// virtual method... do not delete!
End;
//-------------------------------------------------------------

Function TZipCommon.GetVolumeSizeNames: TStrings;
Begin
	Result := Nil;		  // virtual method... do not delete!
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetPartType(SFS: TPartType);
Begin
	// virtual method... do not delete!
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetPartSize(SKB: Cardinal);
Begin
	// virtual method... do not delete!
End;
//------------------------------------------------------------

Function TZipCommon.ReadFilename(s: TStream32; pFilename: PChar; Len: Integer): Integer;
Begin
   If Len > 255 Then Len := 255;
   ZeroMemory(pFilename, 255);
   Result := s.Read(pFileName^, Len);
End;
//------------------------------------------------------------

Procedure TZipCommon.UpdateCrcBuffer(Size: Byte; Var Buf; iCount: Integer);
Begin
   Case Size Of
      16: Crc16_buf(@Buf, iCount, Crc16Val);
      32: Crc32_buf(@Buf, iCount, Crc32Val);
   End;
End;
//-------------------------------------------------------------

Procedure TZipCommon.UpdateEncryptBuffer(IsEncrypted: Boolean; Var Buf;
   iCount: Integer);
Var
   p: ^Byte;
   i: Integer;
Begin
   If IsEncrypted Then
   Begin
      Case ArcType Of
         atBh..atBhExe,
            atJar..atJarExe,
            atZip..atZipMV:
            Begin
               p := @Buf;
               For i := 1 To iCount Do
               Begin
                  p^ := p^ Xor ztvDecryptByte();
                  update_keys(p^);
                  Inc(p);
               End;
            End;
         atArj,
            atArjExe:
            Begin
               If (fPassword <> '') Then
               Begin
                  p := @Buf;
                  For i := 1 To iCount Do
                  Begin
                     p^ := p^ Xor Byte(fPassword[fPassptr]);
                     If fPassptr = Length(fPassword) Then
                        fPassptr := 1
                     Else
                        Inc(fPassptr);
                     Inc(p);
                  End;
               End;
            End;
      End;
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.ReadProc(s: TStream32; DummyStrm: TStream32; Var Buf;
   IsEncrypted: Boolean; Size: Byte; iCount: DWord; ReadType: TDataType): DWord;
Begin
   Result := 0;
   If iCount = 0 Then Exit;

   Try
   	Result := s.Read(Buf, iCount);
   Except
      On EReadError Do                  (* Stream Write Error *)
         Result := 0;
   End;

   UpdateEncryptBuffer(IsEncrypted, Buf, Result);
   UpdateCrcBuffer(Size, Buf, Result);
End;
//-------------------------------------------------------------

Function TZipCommon.WriteData(s: TStream32; Var Buf; Size: Byte; iCount: DWord;
	WriteType: TDataType): DWord;
Const
   wCRLF = 2573;
   sCRLF: Array[0..1] Of char = #13#10;
Var
   w: ^Word;
   i: Integer;
   C, SOL: ^Byte;
   BytesWritten: Cardinal;
   ECode,
   	j, x{, y}: DWord;
Begin

   j := 0;
   BytesWritten := 0;

   If AsciiTranslation Then
   Begin

      C := @Buf;
      w := @C;

      For i := 1 To iCount Do
      Begin

         // for compatibility with the gzip.exe utility, rem the
         // following (w^ <> wCRLF) comparison
         If (w^ <> wCRLF) And (C^ = 10) Then
         Begin

            x := 0;
            SOL := Pointer(DWord(C) - j);
            If w^ <> wCRLF Then
               Case WriteMethod Of
                  faNul: ;
                  faFile:
                     Begin
                     	x := s.Write(SOL^, j);
                        {y :=} s.Write(sCRLF, SizeOf(sCRLF));
                     End;
                  faUserStream:
                     Begin
                     	x := s.Write(SOL^, j);
                        {y :=} s.Write(sCRLF, SizeOf(sCRLF));
                     End;
                  faMemoryStream:
                     Begin
                     	x := s.Write(SOL^, j);
                        {y :=} s.Write(sCRLF, SizeOf(sCRLF));
                     End;
                  faPointer:
                     Begin
                     End;
               End;

            j := 0;
            Inc(BytesWritten, x);
            Dec(iCount, x);

         End
         Else
            Inc(j);

         Inc(C);
         w := Pointer(longint(C) - 1);
      End;

      SOL := Pointer(Cardinal(C) - j);

      x := s.Write(SOL^, j);
      If (x <> j) Then
         Result := 0
      Else
         Result := iCount + BytesWritten;

   End
   Else If iCount > 0 Then
   Begin
      Case WriteMethod Of
         faNul: Result := 0;
         faFile: Result := s.Write(Buf, iCount);
         faUserStream: Result := s.Write(Buf, iCount);
         faMemoryStream: Result := s.Write(Buf, iCount);
         faPointer:
            Begin
               CopyMem(@Buf, @ztvWritePTR^, iCount);
               ztvWritePTR := Pointer(DWord(ztvWritePTR) + iCount);
               Result := iCount;
            End;
      Else
         Result := 0;
      End;

      Inc(Result, BytesWritten);
   End;

   ECode := GetLastError();

   If (ECode <> 0) Or (Result <> (iCount + BytesWritten)) Then
   Begin
   	If ECode = ERROR_DISK_FULL Then
      Begin
      	Count := 0;
         RaiseError(E_RAISE, fArchiveFile, FileName, '0', E_DISKFULL);
      End Else
   		If Result <> (iCount + BytesWritten) Then
         Begin
            If Count > 0 Then
               Dec(Count);

         	RaiseErrorStr(fArchiveFile, FileName, '0', E_FWRITE);
         End Else
      		If WriteType = dtData Then
         		UpdateCrcBuffer(Size, Buf, Result);
   End Else
      If WriteType = dtData Then
         UpdateCrcBuffer(Size, Buf, Result);

End;
//-------------------------------------------------------------

Function TZipCommon.ExtractWriteProc(Var f: TStream32; Var Buf;
	IsEncrypted: Boolean; Size: Byte; iCount: DWord;
   WriteType: TDataType): DWord;
Begin
   If IsEncrypted And (WriteType = dtData) And
      (ArcType In [atBh..atBhExe, atZip..atZipMV, atJar..atJarExe]) Then
   Begin
      ztvDecodeBuf(@Buf, iCount);
   End;
   Result := WriteData(f, Buf, Size, iCount, WriteType);
End;
//-------------------------------------------------------------

Function TZipCommon.CompressWriteProc(Var s: TStream32; Var Buf;
	IsEncrypted: Boolean; Size: Byte; iCount: DWord;
   WriteType: TDataType): DWord;
Var
   i: Integer;
   p: ^Byte;
Begin
   If IsEncrypted And (WriteType = dtData) And
      (ArcType In [atBh..atBhExe, atZip..atZipMV, atJar..atJarExe]) Then
   Begin
      p := @Buf;
      For i := 1 To iCount Do
      Begin
         p^ := ztvEncodeByte(p^);
         Inc(p);
      End;
   End;
   Result := WriteData(s, Buf, Size, iCount, WriteType);
End;
//-------------------------------------------------------------

Function TZipCommon.IsArchiveExt(Extension: AnsiString): Integer;
Var
   i: Integer;
Begin
   Result := -1;
   For i := 0 To MaxExtArray Do
      If CompareText(Extension, ExtArray[i]) = 0 Then
      Begin
         Result := i;
         Break;
      End;
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetCustomSizeType(SCST: TCustomSizeType);
Begin
	// virtual method... do not delete!
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetExcludeExts(SEE: TStrings);
Begin
   fExcludeExts.Clear();
End;
//-------------------------------------------------------------

Function TZipCommon.Close_OutFile(s: TStream32): Boolean;
Begin
   Case WriteMethod Of
      faFile:
      	Try
         	s.Free();
            Result := True;
         Except
         	Result := False;
         End;
      faUserStream: Result := True;     //developer frees this stream in app
      faMemoryStream: Result := True;   //developer frees this stream in app
      faPointer: Result := True;
   Else
      Result := False;
   End;
End;
//-------------------------------------------------------------

Procedure TZipCommon.RetProgress(ProgressByFile, ProgressByArchive: Byte);
Begin
   Application.ProcessMessages;
   If Assigned(OnProgress) Then
      OnProgress(Self, ProgressByFile, ProgressByArchive);
End;
//-------------------------------------------------------------

Function TZipCommon.DecodeRarFileName(FileName: AnsiString; FileNameLen: Word): WideString;
Var
	iLen: Integer;
   WideFileName: pWideChar;
Begin
   iLen := StrLen(PChar(FileName)) + 1;
   GetMem(WideFileName, 1024);
   Try
      DecodeRarFN(PChar(FileName),
         Pointer(ptr2int(FileName) + iLen),
         FileNameLen - iLen,
         WideFileName,
         FileNameLen - 1);
   Finally
      Result := AnsiString(WideFileName);
      FreeMem(WideFileName);
   End;
End;
//-------------------------------------------------------------

// Note: 2162720 is the integer value of 1/1/1980 12:01 am
Function TZipCommon.ConvertDate(Date: Integer): TDateTime;
Begin
   Try
   	If Date = 0 Then
      	Date := 2162720;

      Result := ztvConvertDate(Date);
   Except
      Result := 29221.000694;            		// 1/1/1980 12:01 am
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.WinAttrToString(Attr: Integer; PadChar: Byte): AnsiString;
Begin
   SetLength(Result, 5);
   FillChar(Result[1], 5, Char(PadChar));
   If ((Attr And ZTV_FILE_ATTRIBUTE_ARCHIVE) > 0) Then Result[1] := 'a'; // define before 'd'
   If ((Attr And ZTV_FILE_ATTRIBUTE_DIRECTORY) > 0) Then Result[1] := 'd'; // define before 'v'
   //If ( ( Attr And faVolumeID ) > 0 ) Then Result[1] := 'v';
   If ((Attr And ZTV_FILE_ATTRIBUTE_READONLY) > 0) Then Result[2] := 'r';
   If ((Attr And ZTV_FILE_ATTRIBUTE_SYSTEM) > 0) Then Result[3] := 's';
   If ((Attr And ZTV_FILE_ATTRIBUTE_HIDDEN) > 0) Then Result[4] := 'h';
   If ((Attr And ZTV_FILE_ATTRIBUTE_ENCRYPTED) > 0) Then Result[4] := 'e';
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetPasswords(SP: TStringList);
Begin
   fPasswords.Assign(SP);
End;
//-------------------------------------------------------------

Function TZipCommon.UnixAttrToStr(a: Integer; PadChar: Byte): AnsiString;
Var
   i: Integer;
Begin

   SetLength(Result, 10);
   For i := 1 To 10 Do
      Result[i] := char(PadChar);

   If (a And $4000 > 0) Then Result[1] := 'd';
   If (a And $0100 > 0) Then Result[2] := 'r';
   If (a And $0080 > 0) Then Result[3] := 'w';

   //(a & $0040) ? ((a & $0800) ? 's':'x'):((a & $0800) ? 'S':'-');
   If (a And $0040 > 0) Then
   Begin
      If (a And $0800 > 0) Then
         Result[4] := 's'
      Else
         Result[4] := 'x'
   End
   Else If (a And $0800 > 0) Then
      Result[4] := 'S';

   If (a And $0020 > 0) Then Result[5] := 'r';
   If (a And $0010 > 0) Then Result[6] := 'w';
   If (a And $0008 > 0) Then
   Begin
      If (a And $0400 > 0) Then
         Result[7] := 's'
      Else
         Result[7] := 'x'
   End
   Else If (a And $0400 > 0) Then
      Result[7] := 'S';

   If (a And $0004 > 0) Then Result[8] := 'r';
   If (a And $0002 > 0) Then Result[9] := 'w';
   If (a And $0001 > 0) Then Result[10] := 'x';
End;
//-------------------------------------------------------------

Function GetNextVolName(s: AnsiString; fnum: Integer; FNextVolumeName: TNextVolumeName;
	FVolumeLongFilenames: Boolean): AnsiString;
Var
   ExtLen: Byte;
   FN: Array[0..256] Of Char;
   Ext,
      Incr: Array[0..4] Of Char;
   p: PChar;
Begin
   Try
      Try
         ExtLen := 0;
         If fnum Div 100 > 0 Then
         Begin
            Incr[ExtLen] := Char(fnum Div 100 + 48);
            Inc(ExtLen);
            fnum := fnum - ((fnum Div 100) * 100);
         End;

         If (fnum Div 10 > 0) Or (ExtLen = 1) Then
         Begin
            Incr[ExtLen] := Char(fnum Div 10 + 48);
            Inc(ExtLen);
         End;

         Incr[ExtLen] := Char(fnum Mod 10 + 48);
         Inc(ExtLen);
         Incr[ExtLen] := #0;

         StrPCopy(FN, s);
         p := StrRScan(FN, '.');
         If p <> Nil Then
            CopyMem(@p[0], @Ext[0], Strlen(p));

         Case FNextVolumeName Of

            nvChangeExt:

               If p = Nil Then
                  StrCat(FN, Incr)
               Else
               Begin
                  StrLCopy(FN, FN, Strlen(FN) - Strlen(p) + Cardinal(4 - { aPos } ExtLen));
                  StrCat(FN, Incr);
               End;

            nvChangeFilename:

               Begin
                  If FVolumeLongFilenames Then
                     StrLCopy(FN, FN, 8 - ExtLen)
                  Else
                     StrLCopy(FN, FN, Strlen(FN) - Strlen(p) - ExtLen);

                  StrCat(FN, Incr);
                  StrCat(FN, Ext);
               End;
         End;
      Finally
         Result := StrPas(FN);
      End;
   Except
      Result := '';
   End;
End;
//-------------------------------------------------------------

Function GetTempFilenameStr(TempDir: AnsiString): AnsiString;
Var
   a: Array[0..MAX_PATH] Of Char;
Begin
	Result := '';

   If (TempDir <> '') And ztvGbls.DirExists(TempDir) Then
      StrPCopy(a, TempDir)
   Else
      Windows.GetTempPath(1024, a);

   If Not DirExists(StrPas(a)) Then
      CreateDirEx(StrPas(a));

   Windows.GetTempFilename(a, 'ztv', 0, a);
   Result := StrPas(a);
End;
//-------------------------------------------------------------

Function ztvClosedImageIndex(FileName: AnsiString): Integer;
Var
   SHFileInfo: TSHFileInfo;
Begin
   FillChar(SHFileInfo, SizeOf(SHFileInfo), #0);
   ShGetFileInfo(PChar(ExtractFilename(FileName)),
      0,
      SHFileInfo,
      SizeOf(TSHFileInfo),
		SHGFI_SYSICONINDEX or
      SHGFI_SMALLICON);

   Result := SHFileInfo.iIcon;
End;

Function ztvOpenImageIndex(FileName: AnsiString): Integer;
Var
   SHFileInfo: TSHFileInfo;
Begin
   FillChar(SHFileInfo, SizeOf(SHFileInfo), #0);
   ShGetFileInfo(PChar(ExtractFilename(FileName)),
      0,
      SHFileInfo,
      SizeOf(TSHFileInfo),
		SHGFI_SYSICONINDEX Or
      SHGFI_SMALLICON Or
      SHGFI_OPENICON);

   Result := SHFileInfo.iIcon;
End;

Function ztvSystemImageIndex(FileName: AnsiString): Integer;
Var
   SHFileInfo: TSHFileInfo;
Begin
   FillChar(SHFileInfo, SizeOf(SHFileInfo), #0);
   ShGetFileInfo(PChar(ExtractFilename(FileName)),
      0,
      SHFileInfo,
      SizeOf(TSHFileInfo),
      SHGFI_USEFILEATTRIBUTES Or
      SHGFI_SYSICONINDEX Or
      SHGFI_EXETYPE);

   Result := SHFileInfo.iIcon;
End;

//-------------------------------------------------------------

Function TZipCommon.FileAttrToString(Attr: Integer; PadChar: Byte): AnsiString;
Var
   OS: THostOS;
Begin
   OS := THostOS(osWin32);
   If Not IsArcValid(ArcType) Then
      OS := osNone
   Else
      Case ArcType Of
         atAce:
            If (AceMHeader.HOST_CR = Word(ord(osUnix))) Then
               OS := THostOS(osUnix);
         atRar,
            atRarExe:
            If (RarHeader.HostOS = Word(ord(osUnix))) Then
               OS := THostOS(osUnix);
      End;

   Case OS Of
      osUnix: Result := UnixAttrToStr(Attr, PadChar);
      osWin32: Result := WinAttrToString(Attr, PadChar);
   Else
      SetLength(Result, 0);
   End;
End;
//-------------------------------------------------------------


Function TZipCommon.FormatFileName(sFilename, Dir: AnsiString): AnsiString;

	Function IsRootDir: Boolean;
   Begin
      Result := (Length(Dir) > 3) And (StrComp(@Dir[2], ':\') = 0);
   End;
Var
   p: Word;
   TempDirectory: AnsiString;
   CharPos: Byte;

Begin
   Result := sFilename;

//SDN:
   Case StoredDirNames Of

      sdNone:
         Result := ExtractFilename(Result);

      sdAbsolute:                       // unformated dir = absolute dir path
         ;

      sdAbsoluteNoDrv,
         sdAbsoluteNoRoot:
         Begin
            p := Pos(':', Result);
            If p > 0 Then
               System.Delete(Result, 1, p)
            Else If IsUncPath(Result) Then
            Begin
               // remove network identifier - preceeding double forward slashes
               Result := Copy(Result, 3, Length(Result));

               // remove network drivename, network drive letter
               For p := 1 Downto 0 Do
               Begin
                  CharPos := Pos('\', Result);
                  If (CharPos > 0) And (CharPos + p <= Length(Result)) Then
                     Result := Copy(Result, CharPos + p, Length(Result));
               End;
            End;

            If (StoredDirNames = sdAbsoluteNoRoot) And
               (Result <> '') And
               (Result[1] = '\') Then
               Result := Copy(Result, 2, Length(Result));
         End;

      sdRelative:
         Begin
            If (Dir <> '') And
               (CompareText(Dir, Copy(Result, 1, Length(Dir))) = 0) Then
               System.Delete(Result, 1, Length(Dir));
         End;

      sdRelativeStoreStart:
         Begin
            TempDirectory :=
               AppendDirTail(ExtractFilename(RemoveDirTail(Dir)));

            Delete(Result, 1, Length(Dir) - Length(TempDirectory));
            If IsRootDir() Then Result := '\' + Result;
         End;

      sdExplorer_UserDefineDefaultDir,
      	//Begin
         //   If FileSpec.Count > 1 Then
         //      StoredDirNames := sdExplorer_Auto
         //   Else
         //      StoredDirNames := sdRelativeStoreStart;
         //
         //   FormatFileName(sFilename, Dir);
         //End;

      sdExplorer_Auto:
         Begin
            If (Length(Dir) > 0) And (Length(DefaultDir) > 0) And
               (CompareText(AppendDirTail(Dir),
               AppendDirTail(DefaultDir)) = 0) Then
            Begin
               (* File's folder and default folder are the same, DON'T add folder info *)
               If (Dir <> '') And
                  (CompareText(Dir, Copy(Result, 1, Length(Dir))) = 0) Then
                  System.Delete(Result, 1, Length(Dir));
            End
            Else
            Begin
               (* File's folder and default folder are NOT the same, ADD folder info *)
               If CompareMem(PChar(DefaultDir), PChar(Result), Length(DefaultDir)) Then
               	Delete(Result, 1, Length(AppendDirTail(DefaultDir)));
               If IsRootDir() Then Result := '\' + Result;
            End;
         End;

      {sdCustom:
       Begin
          If Assigned( OnCustomFilename ) Then
             OnCustomFilename( Self, Dir, FN )
            Else Begin
             StoredDirNames := sdRelative;
               Goto SDN;
            End;
         End;}

   End;
End;
//-------------------------------------------------------------

Function TZipCommon.GetCustomSizeType: TCustomSizeType;
Begin
	Result := stBytes; 	// virtual method... do not delete!
End;
//-------------------------------------------------------------

Function TZipCommon.GetDateTime(dt: Integer): Integer;
Begin
   Case fDateAttribute Of
      daFileDate: Result := dt;
      daSysDate: Result := DateTimeToFileDate(Now);
      daMaxFileDate: Result := fMaxAge;
      //ShowMessage( DateTimeToStr( FileDateToDateTime(FMaxAge) ) );
   Else
      Result := dt;
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.FileInExcludeList(FileName, DefDir: AnsiString): Boolean;
Var
   i: Integer;
   ExcludeDir,
   FileDir,
   	LocalFileName: AnsiString;
Begin
   Result := False;
   If (fExcludeSpec.Count = 0) Then Exit;
   For i := 0 To fExcludeSpec.Count - 1 Do
   Begin

   	If fRecurseDirs Then
      Begin
      	If Pos('\', ExcludeSpec[i]) = 0 Then
         Begin
         	FileName := ExtractFileName(FileName);
            LocalFileName := ExcludeSpec[i];
         End Else Begin
            ExcludeDir := ExtractFilePath(FormatFileName(ExcludeSpec[i], DefDir));
            FileDir := ExtractFilePath(FileName);

            If CompareText(Copy(FileDir, 1, Length(ExcludeDir)), ExcludeDir) = 0 Then
            Begin
               LocalFileName := ExtractFileName(ExcludeSpec[i]);
               FileName := ExtractFileName(FileName);
            End Else
               Exit;

         End;
      End Else
      	LocalFileName := FormatFileName(ExcludeSpec[i], DefDir);

      If MatchesMask(FileName, LocalFileName) Then
      Begin
         Result := True;
         Break;
      End;
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.GetCancel: Boolean;
Begin
   Result := pCancel^;
End;
//-------------------------------------------------------------

Function TZipCommon.GetNextVolumeName(FileName: AnsiString; VolNum:
   Integer): AnsiString;

   Function GetNextVolumeExt(FN: AnsiString; VolNum: Integer): AnsiString;
   Var
   	NumLen: Byte;
   	Ext,
      	ExtPart: AnsiString;
   Begin
      If (VolNum < 0) Then
         SetLength(Result, 0)
      Else If (ArcType = atRar) Or (ArcType = atRarExe) Then
      Begin
         Ext := ExtractFileExt(FN);
         If Length(Ext) > 5 Then
         Begin
         	ExtPart := Copy(Ext, 1, 5);
            //Ext := Copy(Ext, 6, Length(Ext));
            If LowerCase(ExtPart) = '.part' Then
            Begin
            	NumLen := Length(Ext) - 5;
               If NumLen = 1 Then
            		Result := ExtPart + IntToStr(VolNum + 2) + '.rar'
               Else If NumLen = 2 Then
               Begin
      				If VolNum + 2 < 10 Then
         				Result := ExtPart + '0' + IntToStr(VolNum + 2) + '.rar'
                  Else
            			Result := ExtPart + IntToStr(VolNum + 2) + '.rar';
               End Else If NumLen = 3 Then
               Begin
      				If VolNum + 2 < 10 Then
         				Result := ExtPart + '00' + IntToStr(VolNum + 2) + '.rar'
                  Else If VolNum + 2 < 100 Then
         				Result := ExtPart + '0' + IntToStr(VolNum + 2) + '.rar'
                  Else
            			Result := ExtPart + IntToStr(VolNum + 2) + '.rar';
               End;
               Exit;
            End;
         End;

         VolNum := VolNum Mod 100;
         If VolNum = 0 Then
            Inc(fVolBegChar);

      End;

      If VolNum < 10 Then
         Result := '.' + char(fVolBegChar) + '0' + IntToStr(VolNum)
      Else If VolNum < 100 Then
         Result := '.' + char(fVolBegChar) + IntToStr(VolNum)
      Else
         Result := '.' + IntToStr(VolNum);
   End;
Var

   Dir, Ext: AnsiString;
Begin
   If (FileName = '') Or (VolNum < 0) Then
      SetLength(Result, 0)
   Else
   Begin
      Dir := ExtractFilePath(FileName);
      FileName := ExtractFilename(FileName);
      Result := Dir + Copy(FileName, 1, ExtractFilenameOnly(PChar(FileName)));
      Ext := GetNextVolumeExt(Result, VolNum);

		If (ArcType = atRar) Or (ArcType = atRarExe) Then
      	If (Length(Ext) > 5) And (LowerCase(Copy(Ext, 1, 5)) = '.part') Then
         	Result := Copy(Result, 1, ExtractFilenameOnly(PChar(Result)));

      Result := Result + Ext;
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.GetNextVolumeName(VolNum, LastVolumeInSet: Integer): AnsiString;
Begin
   If VolNum < LastVolumeInSet Then
      Result := ChangeFileExt(fVolumeName, format('.z%2.2d',[VolNum + 1]))
   Else
      Result := fArchiveFile;
End;
//-------------------------------------------------------------

Function TZipCommon.GetCompressMethodStr(method, BitFlag: Word): AnsiString;

   Function GetTypeFromTCOMP(tc: smallint): Integer;
   Const
      tcompMASK_TYPE = $000F;
   Begin
      Result := ((tc) And tcompMASK_TYPE);
   End;

Const
   szAce: Array[0..6] Of AnsiString = ('Fastest', 'Fast', 'Normal', 'Good',
      'Best', 'Lzw', 'Unsupported');
   szArc: Array[1..10] Of AnsiString = ('Stored', 'Stored', 'Packed', 'Squeezed',
      'Crunched', 'Crunched', 'Squashed', 'Pack10', 'Pack11', 'Pack12');
   szArj: Array[0..4] Of AnsiString = ('Stored', 'Most', '2nd Most', '2nd Fast',
      'Fastest');
   szBh: Array[0..3] Of AnsiString = ('Stored', 'Fused1', 'Deflate', 'Fused3');
   szCab: Array[0..3] Of AnsiString = ('Stored', 'MsZIP', 'Quantum', 'Lzx');
   szCommon: Array[0..2] Of AnsiString = ('<Dir>', 'Unsupported', 'Special');
   szHa: Array[0..2] Of AnsiString = ('Cpy', 'Asc', 'HSC');
   szLH: Array[48..55] Of AnsiString = ('Stored', 'Frozen-1', 'Frozen-2',
      'Frozen-3', 'Frozen-4', 'Frozen-5', 'Frozen-6','Frozen-7' );
   szPak: Array[1..11] Of AnsiString = ('Stored', 'Stored', 'RLE', 'Squeezed',
      'FixedLen', 'FixedLen', 'FixedLen', 'Varible', 'Varible', 'Crushed',
      'Distill');
   szRar: Array[48..53] Of AnsiString = ('Stored', 'Fastest', 'Fast', 'Normal',
      'Good', 'Best');
   szTar: Array[0..0] Of AnsiString = ('Tarred');
   szZip: Array[0..7] Of AnsiString = ('Stored', 'Shrunk', 'Reduce-1', 'Reduce-2',
      'Reduce-3', 'Reduce-4', 'Imploded', 'Tokened');
   szZoo: Array[0..2] Of AnsiString =
   ('Stored', 'Fastest' (* 'LZD' *), 'Greatest' (* 'LZH' *));

Var
   s: AnsiString;
Begin

   s := szCommon[1];                    //default = Unsupported

   Case ArcType Of

      atAce, atAceExe:
         s := szAce[method];

      atArc, atArcExe:
         If (method > 0) And (method < 12) Then
            s := szArc[method]
         Else If ArcHeader.CompressType = 30 Then
            s := szCommon[0];

      atArj, atArjExe:
         If method < 4 Then
            s := szArj[method]
         Else If pArjHeader^.FileType = 3 Then
            s := szCommon[0];

      atBh, atBhExe:
         If method < 4 Then
            s := szBh[method]
         Else If method = 8 Then
            s := GetDeflateMethodStr(BitFlag);

      atCab,
      	atCabExe:
         Begin
            method := GetTypeFromTCOMP(method);
            If method < 4 Then
               s := szCab[method]
            Else If method < 21 Then
               s := szCab[3] + IntToStr(method);
         End;

      atGZip:
         Case method Of
            0: s := 'Stored';
            (* Display of DeflateType used to compress is not supported *)
            (* because every bit in gzip's bitflag is used, thus can't  *)
            (* be revised with the specific DeflateType.                *)
            8: s := 'Deflate';          // GZIP_MAGIC & OLD_GZIP_MAGIC
            144: s := 'LzwMAGIC';       // LZH_MAGIC
         Else
            Case GZipHeader.SignAtr Of
               LZH_MAGIC: s := 'LzhMAGIC';
               PACK_MAGIC: s := 'PackMAGIC';
            End;
         End;

      atHA:
         If method < 3 Then
            s := szHa[method]
         Else If method = $E Then
            s := szCommon[0]
         Else If method = $F Then
            s := szCommon[2];           //special

      atLha, atLzh, atLhaExe, atLzhExe:
         If method < 56 Then
            s := szLH[method]
         Else If method = 100 Then
            s := szCommon[0];

      atPak, atPakExe:
         If (method > 1) And (method < 12) Then
            s := szPak[method];

      atRar, atRarExe:
         If (RarHeader.ExternalAttr And FILE_ATTRIBUTE_DIRECTORY) > 0 Then
            s := szCommon[0]
         Else If (method > 47) And (method < 54) Then
            s := szRar[method];

      atTar:
         If method = 0 Then
            s := szTar[method]
         Else If ExtractFilename(fFileName) = '' Then
            s := szCommon[0];

      atZoo:
         If method < 3 Then
            s := szZoo[method];

      atJar..atJarExe,
      	atZip..atZipMV:
         If method < 8 Then
            s := szZip[method]
         Else If method = 8 Then
            s := GetDeflateMethodStr(BitFlag)
         Else If method = 9 Then
            s := GetDeflateMethodStr64(BitFlag)
         Else If method = 10 Then
            s := 'DCL-implode'
         Else If (LoWord(CentralZipHeader.ExternalAttr) And
            FILE_ATTRIBUTE_DIRECTORY) > 0 Then
            s := szCommon[0];

   End;
   Result := s;
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetCancel(SC: Boolean);
Begin
   If Self <> Nil Then
   Begin
      pCancel^ := SC;
      If pCancel^ Then
         RaiseErrorStr(fArchiveFile, FileName, '0', E_USERCANCEL);
   End;
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetExcludeSpec(SES: TStrings);
Begin
   fExcludeSpec.Assign(SES);
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetFileSpec(SFS: TStrings);
Begin
   fFileSpec.Assign(SFS);
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetExtractDir(SED: AnsiString);
Begin
   If SED = '' Then
   Begin
      If fArchiveFile <> '' Then
         fExtractDir := ExtractFilePath(fArchiveFile)
      Else
      	fExtractDir := '';
   End
   Else
      fExtractDir := ExtractFilePath(AppendDirTail(UnixToDosFilename(SED)));
End;
//-------------------------------------------------------------
(* Do not use FileName in this routine... will only cause an endless
recursive loop.  Use variable FFileName instead! *)

Procedure TZipCommon.SetFilename(SFN: AnsiString);
Var
   s: AnsiString;
Begin

   If (SFN <> '') Then
   Begin
      fFileName := SFN;
      If fExtractDir = '' Then
      Begin
         ExtractDir := ExtractFileDir(fArchiveFile);

      	If fExtractDir = '' Then
         	ExtractDir := GetCurrentDir();
      End;

      If UseStoredDirs Then
      Begin
         If Pos(':', fFileName) > 0 Then
         Begin

            If Pos('\', fFileName) = 3 Then
               fFileName := Copy(fFileName, 4, Length(fFileName) - 3)
            Else
               fFileName := Copy(fFileName, 3, Length(fFileName) - 2);

         End
         Else If Length(fFileName) > 0 Then
            If fFileName[1] = '\' Then
               fFileName := Copy(fFileName, 2, Length(fFileName) - 1);

         s := AppendDirTail(ExtractDir) + ExtractFilePath(fFileName);
      End
      Else
         s := fExtractDir;

      If Pos(':', s) = 0 Then
         If s[1] <> '\' Then
            s := '\' + s;

      fFileName := AppendDirTail(s) + ExtractFileName(fFileName);
   End
   Else
   	fFileName := '';

End;
//-------------------------------------------------------------

Function TZipCommon.GetNextVolume(Var VolName: AnsiString; VolNum: Variant):
   Boolean;
Var
   sInt: AnsiString;
   fExists: Boolean;
   CancelNext: Boolean;
Begin
   Result := False;

   If VarType(VolNum) = VarInteger Then
      sInt := IntToStr(VolNum)
   Else
      sInt := VolNum;

   If VarType(VolNum) = VarInteger Then
      If VolNum > 999 Then
      Begin
         RaiseErrorStr(VolName, '', sInt, E_MAXVOL);
         Exit;
      End;

   If VolName = '' Then
   Begin
      RaiseErrorStr(VolName, '', sInt, E_INVALIDFN);
      Exit;
   End;

   Try
      If ExtractFileDir(VolName) = '' Then
         VolName := AppendDirTail(GetCurrentDir) + VolName;

      fExists := FileExists({Dir +} VolName);
      Repeat
         CancelNext := False;

         If Assigned(OnNextVolume) Then
            OnNextVolume(Self, VolName, sInt, fExists, CancelNext)
         Else Begin
            If fExists Then
               Result := True
            Else Begin
               CancelNext := True;
               RaiseErrorStr(VolName, 'OnNextVolume', IntToStr(VolNum), E_REQUIREDEVENT);
            End;

            Break;
         End;

         If CancelNext Then
         Begin
            Cancel := CancelNext;
            Exit;
         End;

         fExists := FileExists(VolName);
      Until fExists;
      Result := True;

   Finally
      If Not Result Then
         fFilePos := fOffsetEnd
      Else
         Inc(fVolNum);
   End;

End;
//-------------------------------------------------------------

Function TZipCommon.doGetNextVolume(Var s: TStream32; Var sName: AnsiString):
   Boolean;
Begin
   If Assigned(OnNextVolume) Then
   Begin
      sName := GetNextVolumeName(sName, fVolNum);
      Result := GetNextVolume(sName, fVolNum);
      If Result Then
      Begin
         s.Free();
         s := Nil;
         s := TFileStream32.Create(sName, fmOpenRead Or fmShareDenyWrite);

 			If (TFileStream(s).Handle < 0) Then
         Begin
            RaiseErrorStr(ArchiveFile, '', '0', E_FOPEN);
            fLOF := 0;
            Exit;
         End;

         fLOF := s.Size;
      End;
   End
   Else
      Result := False;
End;
//-------------------------------------------------------------

Function TZipCommon.ArcTypeName: AnsiString;
Begin
   Result := ArcTypeNames[ArcType];
End;
//-------------------------------------------------------------

Procedure TZipCommon.doTarGzip(OutFileName: AnsiString);
Begin
   (* Virtual (dont convert to abstract!) method - do not delete  *)
End;
//------------------------------------------------------------

Procedure TZipCommon.GlobalProgress(Sender: TObject);
Begin
   If Assigned(OnProgress) Then
      doBranchProgress(
         TStream32(Sender).Position,
         fUnpackedSize,
         fTotalUnpackedSize);
End;
//-------------------------------------------------------------

Procedure TZipCommon.TempFileMoveBegin(Sender: TObject; Archive, TempFileName: AnsiString);
Begin
   If Assigned(fOnTmpFileMoveBegin) Then
   	fOnTmpFileMoveBegin(Sender, Archive, TempFileName);
End;
//-------------------------------------------------------------

Procedure TZipCommon.TempFileMoveFile(Sender: TTempFileStream; ToFileName, FromFileName:
	AnsiString; SameDrive: Boolean; Size: Int64);
Var
	OutStream: TFileStream32;
Begin
	If SameDrive Then
   Begin

//{$IFDEF MSWINDOWS}
//		MoveFileEx(
//   		PChar(FromFileName),
//      	PChar(ToFileName),
//      	MOVEFILE_COPY_ALLOWED Or MOVEFILE_REPLACE_EXISTING);
//{$ELSE}
   	DeleteFile(ToFileName);
   	MoveFile(
   		PChar(FromFileName),
      	PChar(ToFileName));

   	DeleteFile(FromFileName);
//{$ENDIF}

   End Else Begin
      OutStream := TFileStream32.Create(ToFileName, fmCreate Or fmShareExclusive);
      If (OutStream.Handle < 0) Then
      Begin
         OutStream.Free();
         Raise EFOpenError.CreateFmt(LoadStr(E_FOPEN), [ToFileName]);
         Exit;
      End;

      Try
         fUnpackedSize := Size;
         fTotalUnpackedSize := Size;
         ProgressPosition := Size;
         Sender.Seek(0, soBeginning);
         OutStream.CancelCallBackProc := TZipCommon(Sender.fLocalObj).pCancel;
         OutStream.ProgressCallBackProc := TTempFileStream(Sender.fLocalObj).ProgressProc;
         OutStream.CopyFrom(Sender, Size);
      Finally
         OutStream.Free();
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TZipCommon.TempFileMoveEnd(Sender: TObject);
Begin
   If Assigned(fOnTmpFileMoveEnd) Then
   	fOnTmpFileMoveEnd(Sender);
End;
//-------------------------------------------------------------

Procedure TZipCommon.TempFileProgress(Sender: TObject);
Begin
   If Assigned(OnTmpFileProgress) Then
      doBranchProgress(
         TStream32(Sender).Position,
         fUnpackedSize,
         fTotalUnpackedSize);
End;
//-------------------------------------------------------------

Procedure TZipCommon.doWriteError;
Begin
   RaiseError(E_RAISE, fFileName, '', '0', E_FWRITE);
End;
//-------------------------------------------------------------

Function TZipCommon.ReadTempDir: AnsiString;
Begin
   If (fTempDir = '') Or (Not ztvGbls.DirExists(fTempDir)) Then
      fTempDir := ztvGbls.GetTempPathStr();
   Result := fTempDir;
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetTempDir(STD: AnsiString);
Begin
   If Not (csLoading In ComponentState) Then
   Begin
      If STD = '' Then
         STD := GetTempPathStr();

      STD := AppendDirTail(STD);

      If Not DirExists(STD) Then
         If messageDlg(STD + #13#13 + LoadStr(M_DIRNOTEXIST),
            mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
            CreateDirEx(STD)
         Else
            SetLength(STD, 0);

      If (Length(STD) = 0) Or (Not DirExists(STD)) Then
         STD := GetTempPathStr();
   End;

   fTempDir := STD;
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetVolumeSizeNames(SVSN: TStrings);
Begin
	// virtual method... do not delete!
End;
//-------------------------------------------------------------

Function TZipCommon.GetFileType(FileName: AnsiString): AnsiString;
Var
   ShInfo: TSHFileInfo;
Begin
   ShGetFileInfo(
      PChar(FileName),
      FILE_ATTRIBUTE_NORMAL, ShInfo,
      SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX Or SHGFI_TYPENAME Or SHGFI_USEFILEATTRIBUTES);

   If (ShInfo.szTypeName = '') Then
   Begin
      Result := UpperCase(ExtractFileExt(FileName)) + ' File';
      Delete(Result, 1, 1);
   End
   Else
      Result := ShInfo.szTypeName;
End;
//-------------------------------------------------------------

Function TZipCommon.IsCommentEditable(ArcType: TArcType): Boolean;
Begin
   Result := (ArcType In Comment_Support_ArcType); //defined in ztvgbls
End;
//-------------------------------------------------------------

Function TZipCommon.IsZipped(ArcType: TArcType): Boolean;
Begin
	Result := (ArcType In Zipped_ArcType);
End;
//-------------------------------------------------------------

Function TZipCommon.GetNextZipVolume(Var LocalStream: TTempFileStream; VolNum,
	LastVolumeInSet: Integer): Boolean;
Begin
   If Assigned(OnNextVolume) Then
   Begin
      LocalStream.Free();
      LocalStream := Nil;

      fVolumeName := GetNextVolumeName(VolNum, LastVolumeInSet);

      // if fArchiveFile was a specific volume (ie test.z01), the first defined
      // fVolumeName is going to be test.z01.
      If HeaderTypeState = [htLocal] Then
         If CompareText(fVolumeName, fArchiveFile) = 0 Then
         Begin
            VolNum := VolNum + 1;
            fVolumeName := GetNextVolumeName(VolNum, 9999);
         End;


      Repeat
         OnNextVolume(Self, fVolumeName, IntToStr(VolNum + 1),
            ztvFileExists(fVolumeName),  fCancel);

         Result := (Not fCancel) And ztvFileExists(fVolumeName);
      Until Result Or fCancel;

      fLOF := 0;
      If Result Then
      Begin
         fVolNum := VolNum + 1;

         LocalStream :=
         	TTempFileStream.Create(Self, '', fVolumeName, fmOpenRead Or
            fmShareDenyWrite);

         fLOF := LocalStream.Size;
      End Else
         If fCancel Then
            RaiseErrorStr(fVolumeName, '', IntToStr(VolNum + 1), E_USERCANCEL)
         Else
            RaiseErrorStr(fVolumeName, '', IntToStr(VolNum + 1), E_FOPEN);

   End Else Begin
      Result := False;
      Cancel := True;
      RaiseErrorStr(fVolumeName, 'OnNextVolume', IntToStr(VolNum + 1), E_REQUIREDEVENT);
   End;
End;
//-------------------------------------------------------------

Function TZipCommon.GetVolumeSizeInBytesStr: String;
Begin
	Result := '';  // virtual method... do not delete!
End;
//-------------------------------------------------------------

Function TZipCommon.IsArcValid(ArcType: TArcType): Boolean;
Begin
   Result := Not (ArcType In Invalid_ArcType); //defined in ztvgbls
End;
//-------------------------------------------------------------

Function TZipCommon.IsArcCompressable(ArcType: TArcType): Boolean;
Begin
   Result := (ArcType In Compress_ArcType); // OR ( AT IN Encode_ArcType ); //defined in ztvgbls
End;
//-------------------------------------------------------------

Function TZipCommon.IsArcDecompressable(ArcType: TArcType): Boolean;
Begin
   Result := ArcType In Decompress_ArcType; //defined in ztvgbls
End;
//-------------------------------------------------------------

Function TZipCommon.IsArcVerifyable(ArcType: TArcType): Boolean;
Begin
   Result := ArcType In Verify_ArcType; //defined in ztvgbls
End;
//-------------------------------------------------------------

Function TZipCommon.IsArcSplitable(ArcType: TArcType): Boolean;
Begin
   Result := ArcType In Split_ArcType; //defined in ztvgbls
End;
//-------------------------------------------------------------

Function TZipCommon.IsArcSearchable(ArcType: TArcType): Boolean;
Begin
   Result := ArcType In Search_ArcType; //defined in ztvgbls
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetAttribute(Attr: TztvFileAttr; value: Boolean);
Begin
   If value Then
      Include(fAttributes, Attr)
   Else
      Exclude(fAttributes, Attr);
End;
//-------------------------------------------------------------

Procedure TZipCommon.SetAttributeEx(Attr: TztvFileAttr; value: Boolean);
Begin
   If value Then
      Include(fAttributesEx, Attr)
   Else
      Exclude(fAttributesEx, Attr);
End;
//-------------------------------------------------------------
(* Variable ProgressPosition must be defined prior to calling this procedure *)

Procedure TZipCommon.doBranchProgress(FileCurrent, FileTotal, ArchiveTotal: Int64);
Begin
   Try
      PercentByFile :=
         ztvGbls.CalcProgress64(FileCurrent, FileTotal);

      PercentByArchive :=
         ztvGbls.CalcProgress64(ArchiveTotal - ProgressPosition, ArchiveTotal);

      DoProgress(PercentByFile, PercentByArchive);
   Except
   	//ON e: exception DO ShowMessage( e.message )
   End;
End;
//-------------------------------------------------------------

Procedure TZipCommon.RaiseError(Const EClass: ExceptClass; FileName,
   ExtendedMsg, VolumeID: AnsiString; ECode: Integer);
Begin
   ZipTimer.Suspend();
   Try
      If (Self = Nil) Then Exit;

      If Cancel Then
         ECode := E_USERCANCEL;

      If Assigned(OnError) Then
         OnError(Self, FileName, ExtendedMsg, VolumeID, ECode);

      If Not Cancel Then
         Raise EClass.CreateRes(ECode);
   Finally
      ZipTimer.Resume();
   End;
End;
//-------------------------------------------------------------
(* This function doesn't actually raise an error.  The purpose
  of this function is to activate the onerror event, sending
  an error message to the user.  Example:  when a password
  fails, we just want to notify the programmer without actually
  re-routing our code flow *)

Procedure TZipCommon.RaiseErrorStr(FileName, ExtendedMsg, VolumeID: AnsiString;
   ECode: Integer);
Begin
   ZipTimer.Suspend();
   Try
      If (Self = Nil) Then Exit;

      If Cancel Then
         ECode := E_USERCANCEL;

      If Assigned(OnError) Then
         OnError(Self, FileName, ExtendedMsg, VolumeID, ECode)

   Finally
      ZipTimer.Resume();
   End;
End;

(*************************************************************)
(*************************************************************)
(*                          TCompBase                   	    *)
(*************************************************************)
(*************************************************************)

Constructor TCompBase.Create(AOwner: TComponent);
Var
	i: Integer;
Begin
   Inherited Create(AOwner);
   ArcType := atBh;
   fRecurseDirs := True;
   doSearchRecAction := SearchRecProc;
   doReportDirChange := Nil;
   fCompressionMethod := cmTempFile;
   fDeflateType := dtDeflateN;
   fDateAttribute := daFileDate;
   fStoreEmptySubDirs := False;
   Switch := swAdd;
   // v4.6.8 removed.  Added StoreFilesOfType property
   //StoreAlreadyCompressedFiles := True; // v4.6.8 removed.  Added StoreFilesOfType property
   VerifyBeforeDelete := False;
	fStoreFilesOfType := TStringList.Create();
	For i := 0 To ztvGbls.MaxExtArray Do
   	fStoreFilesOfType.Add(ExtArray[i]);
End;
//-------------------------------------------------------------

Destructor TCompBase.Destroy;
Begin
	fStoreFilesOfType.Free();
   Inherited Destroy;
End;
//------------------------------------------------------------

Function TCompBase.GetArchiveComment(Var Comment: PChar; Var CommentLen:
   Word): PChar;
Begin
   RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC); //virtual method...
   Result := Nil;
End;
//------------------------------------------------------------

Procedure TCompBase.SetArchiveComment(Comment: PChar; CommentLen: Word);
Begin
   RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC); //virtual method...
End;
//------------------------------------------------------------

Function TCompBase.GetFileComment(FileName: AnsiString; Comment: PChar;
	CommentLen: Word): Integer;
Begin;
   RaiseErrorStr(FileName, '', '0', E_INVALIDARC); //virtual method...
   Result := 0;
End;
//------------------------------------------------------------

Function TCompBase.CommentObjInit: Integer;
Begin
   RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC); //virtual method...
   Result := 0;
End;
//------------------------------------------------------------

{Function TCompBase.CalcChkSum: Integer;
Begin
	Result := 0;  // virtual method... DO NOT DELETE!
End;}
//------------------------------------------------------------

Procedure TCompBase.CommentObjDone;
Begin
   RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC); //virtual method...
End;
//------------------------------------------------------------

Procedure TCompBase.SetComment(Index: Integer; Comment: PChar; CommentLen: Word);
Begin
   RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC); //virtual method...
End;
//------------------------------------------------------------

Procedure TCompBase.GetComment(Index: Integer; Var FileName: AnsiString;
   Var Comment: PChar; Var CommentLen: Word);
Begin
   // Virtual method... do not delete!
End;
//-------------------------------------------------------------

Procedure TCompBase.SetArchiveFile(SFN: AnsiString);
Var
   FileExt: AnsiString;
Begin
   If CompressMethod = cmTarGzip Then
   Begin
      If CompareText(LowerCase(ExtractFileExt(SFN)), '.gz') = 0 Then
         SFN := Copy(SFN, 1, Length(SFN) - 3);

      If ztvFileExists(SFN) Then
      	EraseFile( SFN, doAllowUndo );

      fArcType := atTarGZip;
   End;

   If (csLoading In ComponentState) Then
      If Not ztvFileExists(SFN) Then
      Begin
         SetLength(fArchiveFile, 0);
         Exit;
      End;

   //CheckLoadingState( SFN );
   SFN := UnixToDosFilename(SFN);

   FileExt := ExtractFileExt(SFN);

   If Length(FileExt) = 0 Then
      fArchiveFile := Copy(SFN, 1, Length(SFN) -
         Length(ExtractFileExt(SFN))) + fDefaultExt
   Else
      fArchiveFile := SFN;

   fOffsetStart := -1;
   pCancel^ := False;
End;
//-------------------------------------------------------------

Function TCompBase.GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo;
Begin
   //Virtual method... do NOT delete!
End;
//-------------------------------------------------------------

Procedure TCompBase.FileInArchive(FileSpecIndex: Integer; DiskFilename: AnsiString;
	FindData: TWin32FindData; pHeaderObj: pCompHeaderObj);
Var
   HI: THeaderInfo;
   nSize: Int64;
   Replace: Boolean;
   NewFileName: AnsiString;
   Index, nDate, oAttr: Integer;

   Function doOnReplaceFile: Boolean;
   Begin
      Result := False;

      (* Filenames match... either Size, date, or attr does not *)
      If Assigned(OnReplaceFile) Then
      Begin

         OnReplaceFile(Self, FileName, DiskFilename,
            ConvertDate(HI.Date), ConvertDate(nDate),
            HI.uSize, nSize, oAttr, FindData.dwFileAttributes, Result);

         If Result Then
            pCBFI^.Status := hsSkip;

      End
      Else
         RaiseErrorStr(FileName, 'OnReplaceFile', '0', E_REQUIREDEVENT);
   End;

   Function doOnRenameDupeFile: Boolean;
   Begin
      If Assigned(OnRenameDupeFile) Then
         With pHeaderObj^ Do
         Begin
            Repeat

               NewFileName := FileList.Strings[Index];

               Result := True;
               OnRenameDupeFile(Self, NewFileName, TempFilename, Result);
               If Not Result Then
                  Break;

               If (ExtractFilename(TempFilename) <> '') Then
                  NewFileName := TempFilename;

               If (CompareText(FileList.Strings[Index], NewFileName) = 0) Then
               Begin
                  Result := False;
                  Break
               End;

            Until FileList.IndexOf(NewFileName) = -1;

         End
      Else
      Begin
         RaiseErrorStr(FileName, 'OnRenameDupeFile', '0', E_REQUIREDEVENT);
         Result := False;
      End;
   End;

   Function Compare: Boolean;
   Begin
      Result := (nSize = HI.uSize) And (nDate = HI.Date) And
      	(FindData.dwFileAttributes = oAttr);
   End;

Begin {FileInArchive}

   With FindData Do
   Begin

      With pHeaderObj^ Do
      Begin
         nSize  := (Int64(nFileSizeHigh) Shl 32) Or nFileSizeLow;
         nDate := ztvGbls.FileTimeToInt(ftLastWriteTime);

         If (dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY = 0) Then
         Begin
            If (nDate > fMaxAge) Then
               fMaxAge := nDate;

            DiskFileName := AppendDirTail(DiskFileName) + cFileName;
         End;

         (* First check the list for a FileName from an existing archive. *)
         (* The HeadObj.Status would equal "pCopy" if Result is greater   *)
         (* than -1.                                                      *)
         If (Not isNewArchive) And (FileList.Count > 0) Then
         Begin
            (* search for FileName with a stored directory name from an   *)
            (* "existing" archive.                                        *)
            Index := FileList.IndexOf(fFileName);

            (* Search for FileName (DiskFilename) as a FileName added by a  *)
            (* previous filespec specification for compression.             *)
            If (Index = -1) And (FileSpec.Count > 1) Then
               Index := FileList.IndexOf(DiskFileName);
         End Else
             Index := -1;


         (* HeadObj = FileInfo, Header, CommentLen                          *)
         (* FileInfo = uSize, pSize, Date, Attr, Offset, Status             *)
         If Index > -1 Then
         Begin

            pCBFI := FileLocationData(Index);
            If (pCBFI^.Status = hsAdd) Then
               Exit;                       //File was already added for compression

            HI := GetHeaderInfo(Header[Index], pCBFI);
            If ArcType = atTar Then
               oAttr := dwFileAttributes  //tar header does not store the external file attr.. bypass check
            Else
               oAttr := VerifyAttr(HI.Attr);

            If Compare() Then
            Begin
               If (Switch = swRefresh) Then
                  Exit	// .status remains default of hsCopy
               Else
                  Replace := False;
            End
            Else
               If Switch = swRefresh Then
               Begin
                  fAddFiles := True;
                  pCBFI^.Status := hsAdd;
                  RootDirLen[Index] := Pointer(0);	// for FileName re-assignment.. FileName shouldn't change
                  HI.uSize := nSize;
                  HI.pSize := nSize;
                  HI.Date := nDate;
                  HI.Attr := dwFileAttributes;
                  SetHeaderInfo(pHeaderObj^.Header[Index], @HI, CBFInew);
                  Exit;
               End Else
                  Replace := doOnReplaceFile();


            If (Not Replace) Then
            Begin
               fAddFiles := True;
               If doOnRenameDupeFile() Then	//doOnRenameDupeFile(Index)
               Begin
                  pCBFI^.Status := hsRename;
                  FileList.Strings[Index] := NewFileName;
               End
               Else
               Begin
                  pCBFI^.Status := hsCopy;
                  Exit;
               End;
            End;

         End Else
            If (Switch = swRefresh) Then
            Begin
               //pCBFI^.Status := hsCopy;
               Exit;
            End;

      End;


      If (HeadSize > 0) Then
      Begin
         // the rest of the CBFInew record variables are assigned with defaults
         // on activation of a TCompBase object
         CBFInew.EncryptedHeader := EncryptHeaders;
         CBFInew.HighUnpackedSize := nFileSizeHigh;

         pHeaderObj^.AddItem(CBFInew, DiskFilename, GetHeadPtr(),
            Length(fRootDir), HeadSize);

         fAddFiles := True;
         HI.uSize := nSize;
         HI.pSize := nSize;
         HI.Date := nDate;
         HI.Attr := dwFileAttributes;
         SetHeaderInfo(pHeaderObj^.Header[pHeaderObj^.FileCount - 1], @HI, CBFInew);
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TCompBase.DecypherHeader(FileName: AnsiString);
Begin
   (* Virtual (do not convert to abstract!) method - do not delete  *)
End;
//-------------------------------------------------------------

Function TCompBase.GetHeadPtr: Pointer;
Begin
   ShowMessage('GetHeadPtr: contact custsupt@ziptv.com!');
   Result := Nil;                       //Virtual method... do NOT delete!
End;
//-------------------------------------------------------------

Procedure TCompBase.InitializeHeader(pCBFI: pCompFileInfo);
Begin
   //Virtual method... do NOT delete!
End;
//-------------------------------------------------------------

Procedure TCompBase.PackFile(Outfile: TStream32; Index: Integer; pHeaderObj:
   pCompHeaderObj; Var Result: Boolean);
Begin

   Result := False;
   With pHeaderObj^ Do
   Begin

   	If CompressionMethod <> cmInMemory Then
         If (FileList.Strings[Index] =
               TTempFileStream(OutFile).fTempFilename) Then
            Exit;

      fFileName := FileList.Strings[Index];
      fRootDir := Copy(fFileName, 0, Integer(RootDirLen[Index]));

      If doOnBegin(fFileName, Count, pHeaderObj) Then
      Try
         pCBFI^.Offset := Outfile.Size;
         Result := AddFileProc(OutFile, Index, pHeaderObj);
      Finally
         doOnEnd();
      End;
   End;
End;
//-------------------------------------------------------------

Function TCompBase.GetHeadSize: Word;
Begin
   //v4.0: moved these from the base class to local compressor components
   ShowMessage('GetHeadSize: contact custsupt@ziptv.com!');
   Result := 0;
End;
//-------------------------------------------------------------

Function TCompBase.GetLocalHeaderSize: Integer;
Begin
   (* Virtual (dont convert to abstract!) method - do not delete  *)
   Result := 0;
End;
//-------------------------------------------------------------

Function TCompBase.doCleanUp(Outfile: TStream32; pHeaderObj: pCompHeaderObj): Boolean;
Begin
   Result := True;                      (* Virtual (do not convert to abstract!) method - do NOT delete! *)
End;
//-------------------------------------------------------------

Procedure TCompBase.doFinish(pHeaderObj: pCompHeaderObj);
Begin
   (* Virtual (dont convert to abstract!) method - do not delete  *)
End;
//-------------------------------------------------------------

Procedure TCompBase.RootDirChangeEvent(Sender: TObject; Dir: AnsiString);
Begin
   fRootDir := AppendDirTail(Dir);
End;
//------------------------------------------------------------

// note: this function should always be called with the value
// of Dir containing a trailing dir char
Procedure TCompBase.DoUpdateFileList(Const Dir: AnsiString; FindData: TWin32FindData;
   pHeaderObj: pCompHeaderObj);
Var
   ExcludedExt: Boolean;
Begin
	If (FindData.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY > 0) And
      ((Not fStoreEmptySubDirs) Or (Not IsDirEmpty(Dir))) Then
         Exit;

   If (fExcludeExts.Count > 0) Then
   	// is the extension of this file to be excluded?
      ExcludedExt := fExcludeExts.IndexOf(LowerCase(ExtractFileExt(FindData.cFilename))) > -1
   Else
      ExcludedExt := False;

   If (Not ExcludedExt) Then
      doSearchRecAction(0, Dir, FindData, pHeaderObj)
   Else If Assigned(OnExcludeFile) Then
      OnExcludeFile(Self, Dir + FindData.cFilename);
End;
//------------------------------------------------------------

Procedure TCompBase.ExecuteCompression(Var ArcFile: TStream32; TempFile: TStream32;
   pHeaderObj: pCompHeaderObj);
Var
   b: Boolean;
   FileScan: TztvFileScan;
	j,
   	Index,
      FilesToDelete: Integer;
   //BytesWritten,
   pArcFile: ^TTempFileStream;
   LOffset,
   	BytesToSave,
   	TempFileSize: Int64;
Begin
   If Switch <> swDelete Then
   Begin
      FileScan := TztvFileScan.Create(Self);
      Try
         FileScan.UpdateList := True;
         FileScan.pHeaderObj := pHeaderObj;
         FileScan.FileSpec := FileSpec;
         FileScan.Attributes := Attributes;
         FileScan.AttributesEx := AttributesEx;

         // if StoreEmptySubDirs, dir attr must be included
         If fStoreEmptySubDirs Then
            FileScan.SetAttribute(fsDirectory, True);

         FileScan.UpdateFilesList := DoUpdateFileList;	// <---
         FileScan.RecurseDirs := RecurseDirs;
         //FileScan.OnFinished := FinishedEvent;

         FileScan.IncludeHiddenDirs := IncludeHiddenDirs;
         FileScan.OnRootDirChange := RootDirChangeEvent;
         FileScan.OnScanFile := OnFileScanStatus;
         FileScan.OnRecurseDir := OnRecurseDir;

         FileScan.pCancel := @fCancel;
         FileScan.Scan();
      Finally
         fTotalUnpackedSize := FileScan.FilesSize;
         ProgressPosition := fTotalUnpackedSize;

         //FileScan.fFilesList.ClearList();  //no files are added to fFilesList... no need to clear
         FileScan.Free();
      End;
   End;

   // user canceled
   If pCancel^ Or ((Switch <> swDelete) And (Not fAddFiles))Then
   Begin
   	If (Not fAddFiles) Then
      	RaiseErrorStr(fArchiveFile, '', '0', E_NOTHINGTODO);

      pHeaderObj^.FileList.Clear();
      pHeaderObj^.FileCount := 0;
      Exit;
   End;

   If Assigned(OnElapsedTime) Then
   	ZipTimer.Start();

   With pHeaderObj^ Do
   Try

      FilesToDelete := FilesDeleted;
      FilesDeleted := 0;

      For Index := 0 To FileList.Count - 1 Do
      Begin

         If Cancel Then Break;
         Count := Index + 1;

         pCBFI := FileLocationData(Index);
         With pCBFI^ Do
         Begin
            HeadInfo := GetHeaderInfo(Header[Index], pCBFI);
            Bytes_To_Go := HeadInfo.pSize;

            If (TempFile <> Nil) Then
               TempFileSize := TempFile.Size
            Else
               TempFileSize := 0;

            Case Status Of
               hsAdd:

                  Begin
                     If (HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY > 0) And
                        (Not StoreEmptySubDirs) Then
                     Begin
                        Status := hsSkip;
                        Continue;
                     End;

                     PackFile(TempFile, Index, pHeaderObj, b);

                     If b Then
                     Begin
                        Inc(FilesCompressed);
                        If (Switch = swMove) And (HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY = 0) Then
                           If Not VerifyBeforeDelete Then
                              DeleteFileList.Add(FileList.Strings[Index]);

                        FileList.Strings[Index] := fFileName;
                     End Else Begin
                        If Cancel Then
                        Begin
                           Dec(Count);
                           For j := Index To FileList.Count - 1 Do
                           Begin
                              pCBFI := FileLocationData(j);
                              pCBFI.Status := hsSkip;
                           End;

                           // reset stream size to size before
                           // canceled compression of last file
                           If (TempFile <> Nil) Then
                              TempFile.Size := TempFileSize;
                        End;
                        Status := hsSkip
                     End;
                  End;

               hsCopy:

                  Begin
                     ArcFile.Position := pCBFI^.Offset;
                     BytesToSave := GetTotalRecordSize(ArcFile, TempFile, Header[Index]);
                     ArcFile.Position := pCBFI^.Offset; // v5.3 changed.  Was Offset only.


               		//Result := StoreStream(inStream, outStream, zsp)

                     If ArcType = atZipMV Then
                     Begin
                        Bytes_To_Go := BytesToSave;
                        LOffset := pCBFI^.Offset;
                        Repeat
                           If ((LOffset + Bytes_To_Go) - fLOF) > 0 Then
                           Begin
                              BytesToSave := Bytes_To_Go - ((LOffset + Bytes_To_Go) - fLOF);
                              LOffset := 0;
                           End Else
                              BytesToSave := Bytes_To_Go;

                           TempFile.CopyFrom(ArcFile, BytesToSave);
                           Dec(Bytes_To_Go, BytesToSave);

                           If Bytes_To_Go > 0 Then
                           Begin
                              pArcFile := @ArcFile;
                              If (Not GetNextZipVolume(pArcFile^, fVolNum,
                                    EndZipHeader.NumberOfThisDisk)) Then
                              Begin
                                 //RaiseErrorStr(FileName, 'OnNonWriteableArchive', '0', E_REQUIREDEVENT);
                              End;
                           End;
                        Until Bytes_To_Go < 1;
                     End Else
                     	TempFile.CopyFrom(ArcFile, BytesToSave);


{                     InflateRec.UnpackedSize := BytesToSave;
                  	Bytes_To_Go := Unstore(ArcFile, TempFile, 32, '0', InflateRec);

                     If (ArcType = atZipMV) And (Bytes_To_Go > 0) Then
                     Begin
                     	Repeat
                           pArcFile := @ArcFile;
                              If (Not GetNextZipVolume(pArcFile^, fVolNum,
                                    EndZipHeader.NumberOfThisDisk)) Then
                              Begin
         								//RaiseErrorStr(FileName, 'OnNonWriteableArchive', '0', E_REQUIREDEVENT);
                              End;

                        	InflateRec.UnpackedSize := Bytes_To_Go;
                  			Bytes_To_Go := Unstore(ArcFile, TempFile, 32, '0', InflateRec);
                        Until Bytes_To_Go < 1;

                     End Else Begin
                  		Bytes_To_Go := Unstore(ArcFile, TempFile, 32, '0', InflateRec);
                     End;
}

                     //With zsp Do
                     //Begin
                     //   Protect := fEncrypted;
                     //   CRC := Crc32Val;
                     //   pCancel := @fCancel;
                     //   pArchivePos := @ProgressPosition;
                     //End;
                     //ProgressPos := ProgressPosition;


                  End;

               hsRename:                //can reach this block only if the OnRenameDupeFile

                  Begin                 //event is assigned
                     FileList.Strings[Index] :=
                        CharToOemFilter(FileList.Strings[Index], fTransOemChar);

                     ArcFile.Position := Offset;
                     doPCopy(ArcFile, TempFile, Index, pHeaderObj);
                  End;

               hsSkip:
                  Begin
                     Inc(FilesDeleted);
                     If Assigned(OnDeleteFile) Then
                        OnDeleteFile(
                           Self,
                           FileList.Strings[Index],
                           FilesDeleted,
                           FilesToDelete);
                  End;

            End;
         End;
       End;
   Finally
      If Assigned(OnElapsedTime) Then
         ZipTimer.Stop();
   End;
End;
//-------------------------------------------------------------

// note: this function should always be called with the value
// of Dir containing a trailing dir char
Procedure TCompBase.SearchRecProc(Index: Integer; Dir: AnsiString;
   FindData: TWin32FindData; pHeaderObj: pCompHeaderObj);
Begin
   With FindData, pHeaderObj^ Do
   Begin
      If FindData.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY > 0 Then
         Filename := Dir
      Else
         FileName := Dir + cFileName;

      If CopySuccess Then
      Begin
         If (Switch = swMove) Then
            If (Not VerifyBeforeDelete) Then
               EraseFile(Dir + cFilename, DeleteOptions)
            Else If (FileList.IndexOf(FileName) > -1) Then
               EraseFile(Dir + cFilename, DeleteOptions);
      End Else
         If (FindData.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY > 0) Or
         	(Not FileInExcludeList(FileName, fRootDir)) Then
            FileInArchive(Index, Dir, FindData, pHeaderObj);
   End;
End;
//-------------------------------------------------------------

Procedure TCompBase.SetCompressMethod(SCM: TCompressMethod);
Begin
   If SCM In fCompressMethodState Then fCompressMethod := SCM;
End;
//-------------------------------------------------------------

Function TCompBase.IsPasswordSupported(ArcType: TArcType): Boolean;
Begin
   Result := ArcType In Compress_PasswordSupport;
End;
//-------------------------------------------------------------

Function TCompBase.ReadHeader(strm: TStream32; Var FileName: AnsiString;
   HeadType: Byte): Boolean;
Begin
   Result := False;                     //Virtual (dont convert to abstract!) method - do not delete
End;
//-------------------------------------------------------------

Function TCompBase.SetDeflateBitFlag(BitFlag: Word): Byte;
Begin
   Case fDeflateType Of
      dtDeflateS: Result := BitFlag Or 6;
      dtDeflateF: Result := BitFlag Or 4;
      dtDeflateX: Result := BitFlag Or 2;
   Else
      Result := BitFlag;
   End;
End;
//-------------------------------------------------------------

Procedure TCompBase.FillHeaderData(InStreamSize, CompressStreamSize: Int64);
Begin
   (* Virtual method - do not delete  *)
End;
//-------------------------------------------------------------

Function TCompBase.GetFirst(Var FileName: AnsiString; s: TStream32; Var HeadType: Byte;
	pHeaderObj: pCompHeaderObj): Boolean;
Begin
	If fOffsetStart > -1 Then
   	LastPos := fOffsetStart
   Else
   	Lastpos := 0;

   PrevPos := LastPos;
   Result := ReadHeader(s, FileName, 0);
End;
//-------------------------------------------------------------

Function TCompBase.GetNext(Var FileName: AnsiString; strm: TStream32;
   HeadType: Byte): Boolean;
Begin
   PrevPos := LastPos;
   If LastPos = 0 Then
      Result := False
   Else
      Result := ReadHeader(strm, FileName, HeadType);
End;
//-------------------------------------------------------------

Procedure TCompBase.ArcToList(strm: TStream32; pHeaderObj: pCompHeaderObj);
Begin
   (* Virtual (dont convert to abstract!) method - do not delete  *)
End;
//-------------------------------------------------------------

Procedure TCompBase.CompressArcType;
Begin
   If (Not (csLoading In ComponentState)) And
      (fArchiveFile <> '') And
      (ExtractFileExt(fArchiveFile) = '') Then
      fArchiveFile := fArchiveFile + fDefaultExt;
End;
//-------------------------------------------------------------

Function TCompBase.GetDefaultDir: AnsiString;
Var
   i, j: Integer;
   Path2: AnsiString;
   R: Array[0..255] Of char;
Begin
   Result := AppendDirTail(ExtractFilePath(FileSpec[0]));
   For i := 0 To FileSpec.Count - 2 Do
   Begin
      j := 1;
      ZeroMemory(@R, 256);
      Path2 := ExtractFilePath(FileSpec[i + 1]);
      While UpCase(Result[j]) = UpCase(Path2[j]) Do
      Begin
         R[j - 1] := Path2[j];
         Inc(j);
      End;
      Result := ExtractFilePath(R);
   End;
End;
//-------------------------------------------------------------

Function TCompBase.SetDefaultValues: Boolean;
Begin
   Bytes_To_Go := 0;
   CopySuccess := False;
   Count := 0;
   DoProgress := RetProgress;
	fAddFiles := False;
   fMaxAge := 0;
   fUnpackedSize := 0;
   fPackedSize := 0;
   fTotalPackedSize := 0;
   fTotalUnpackedSize := 0;
   fVolumeName := fArchiveFile;
   FilesCompressed := 0;
   FilesDeleted := 0;
   PercentByArchive := 0;
   ProgressPosition := 0;
   Cancel := False;

   // initialize the archive type's header size
   HeadSize := GetHeadSize();

   // initialize with defaults.  Variables which assign non-default variables
   // are assigned later in the TCompBase object.  This was added to prevent
   // repetative assignments of the same variables.
   CBFInew.Offset := 0;
   CBFInew.Status := hsAdd;
   CBFInew.FileComment := Nil;
   CBFInew.FileCommentLen := 0;
   CBFInew.ExtraField := Nil;
   CBFInew.ExtraFieldLen := 0;

   Result := DiskManager.GetDriveInfo(fArchiveFile, RaiseError, RaiseErrorStr,
   		OnDiskWriteProtectErr, OnDiskInDrvErr);

   If (Result And (FileSpec.Count > 0)) Then
   Begin
   	If Switch = swDelete Then
      	DefaultDir := ''
      Else
         Case StoredDirNames Of
            sdExplorer_UserDefineDefaultDir:
               If DefaultDir = '' Then
               Begin
                  RaiseErrorStr(fArchiveFile, 'DefaultDir', '0', E_REQUIREDPROPERTY);
                  Result := False;
               End;
            sdExplorer_Auto:
               DefaultDir := GetDefaultDir();
         End;
   End;

End;
//-------------------------------------------------------------

// write a local extended field header
Function TCompBase.Write64BitFieldHdr(s: TStream32; pCBFI: pCompFileInfo;
	HType: THeaderType): Integer;
Begin
	Result := 0;  // virtual method... do not delete.
End;
//-------------------------------------------------------------

Function TCompBase.Compress: Integer;
Begin
   (* do not change read/Write methods until future versions *)
   ReadMethod := faFile;
   WriteMethod := faFile;
   If SetDefaultValues() Then
      Result := doCompress()
   Else
      Result := 0;
End;
//-------------------------------------------------------------

Function TCompBase.doCompress: Integer;
Var
   HeaderObj: TCompHeaderObj;
   pHeaderObj: pCompHeaderObj;
Begin
	Try
      If Not _DirectoryExists(ExtractFilePath(fArchiveFile)) Then
      Begin
         RaiseErrorStr(ExtractFilePath(fArchiveFile), '', '0', E_DIRNOTFOUND);
         Exit;
      End;

      Try
         // Cancel can be defined as true in previous call to InitializeCompressor
         If (fArchiveFile <> '') And (Not fCancel) Then
         Begin
            DeleteFileList := TStringList.Create();
            HeaderObj := TCompHeaderObj.Create();
            pHeaderObj := @HeaderObj;
            Try
               pHeaderObj^.INIT();
               Try
                  CompressIT(pHeaderObj);  //Execute compression
               Finally
                  DeleteFileList.Clear();
                  DeleteFileList.Free();
               End;
            Finally
               pHeaderObj^.DONE();
               pHeaderObj^.Free();
            End;
         End;
      Finally
         If Switch = swDelete Then
            Count := FilesDeleted
         Else
            Count := FilesCompressed;

         Result := Count;
      End;
   Except
   	Result := 0;
   End;
End;
//-------------------------------------------------------------

Procedure TCompBase.CompressIT(pHeaderObj: pCompHeaderObj);
Var
   i: Integer;
   TempFile: TStream32;
   DiskWithThisFile: Word;
   ArcFile: TTempFileStream;
   //pArcFile: ^TTempFileStream;
   pArcFile: ^TStream32;
Begin

   Try                                  { except }
      isNewArchive := (Not ztvFileExists(fArchiveFile));

      If (Switch = swRead) Then
      Begin
         If (Not isNewArchive) Then
            ReadFileInfo(pHeaderObj);
         Exit;
      End;

      If isNewArchive Then
      Begin
         If (Switch = swDelete) Then
         Begin
            RaiseErrorStr(fArchiveFile, '', '0', M_NOTHING);
            Exit;
         End;
      End Else Begin
      	If (Not HandleNonWriteableFile(fArchiveFile)) Then
         	Exit;
      End;

      If Assigned(OnElapsedTime) Then
      	ZipTimer.Start();

      Try
      	If Assigned(OnActivate) Then
         	OnActivate(Self);

         Try
            If CompressionMethod = cmInMemory Then
               TempFile :=
               	TTempMemStream.Create(Self, fArchiveFile, fmCreate)
            Else Begin
               TempFile :=
                  TTempFileStream.Create(Self, TempDir, fArchiveFile, fmCreate);

   {            If CompareText(
                     ExtractFilePath(TTempFileStream(TempFile).fTempFileName),
                     ExtractFilePath(TempDir)) = 0 Then
               Begin
                  fArchiveFile :=
                     AppendDirTail(ExtractFileDir(fArchiveFile)) +
                     ExtractFileName(TTempFileStream(TempFile).fTempFileName);

                  //If RenameFile(fArchiveFile,
                  //	AppendDirTail(ExtractFileDir(fArchiveFile)) +
                  //   ExtractFileName(TTempFileStream(TempFile).fTempFileName)) Then
                  //   	ShowMessage('Here');
               End;
   }
               TTempFileStream(TempFile).DeleteOptions := fDeleteOptions;
            End;

            TempFile.CancelCallBackProc := @fCancel;
            TempFile.ProgressCallBackProc := Nil;

            Try
               If (Not isNewArchive) Then
               Begin
                  ArcFile :=
                     TTempFileStream.Create(Self, '', fArchiveFile, fmOpenRead Or
                        fmShareDenyWrite);

                  // if zip file, what kind?  atZip (normal), atZipMV (multi-volume, or atZipDS (disk-spanned)
                  If (Not isNewArchive) And (ArcType = atZip) Then
                  	// fLOF is defined in GetArcType
                  	fArcType := GetArcType(ArcFile)
                  Else
                  	fLOF := ArcFile.Size;
               End
               Else
               Begin
                  ArcFile := Nil;
                  fLOF := 0;
               End;

               Try
                  If Assigned(ArcFile) And (fLOF > 0) Then
                  Begin

                     If fOffsetStart > 0 Then
                        If (ArcType = atZipMV) Then
                        Begin
                        	//If (htLocal In HeaderTypeState) Then
                           //	TempFile.CopyFrom(ArcFile, fOffsetStart);

                           // save value before following call to ArcToList.
                           // ArcToList reads entire central header, changing
                           // CentralZipHeader to the last header in the
                           // central library.
                        	If (htCentral In HeaderTypeState) Then
                              DiskWithThisFile := CentralZipHeader.DiskNumberStart
                           Else
                           	DiskWithThisFile := EndZipHeader.NumberOfThisDisk;

                        End Else
                        	Case ArcType Of

                              atZipExe,
                              	atJarExe:
                                 TempFile.CopyFrom(ArcFile, CentralZipHeader.RelativeOffsetOfLocalHeader);

                              atAceExe,
                              	atArcExe,
                                 atArjExe,
                              	atBhExe,
                              	atCabExe,
                                 atLhaExe,
                                 atLzhExe,
                                 atPakExe,
                                 atRarExe:
                                 TempFile.CopyFrom(ArcFile, fOffsetStart);

                           End;

                     If (Switch = swDelete) Then
                        ExcludeSpec.Clear();

                     ArcFile.Position := fOffsetStart;
                     ArcToList(ArcFile, pHeaderObj);


                  	If (ArcType = atZipMV) Then
                     Begin
                     	If (htCentral In HeaderTypeState) Then
                        Begin
                           If (DiskWithThisFile <> EndZipHeader.NumberOfThisDisk) Then
                           Begin
                              If (Not GetNextZipVolume(ArcFile, DiskWithThisFile,
                                 EndZipHeader.NumberOfThisDisk)) Then
                                    Exit;

                              If (htCentral In HeaderTypeState) Then
                                 ArcFile.Position := CentralZipHeader.RelativeOffsetOfLocalHeader;

                           End;
                        End;
                     End;

                  End;

                  If pHeaderObj.FileList.Count = 0 Then
                     fLOF := 0;

                  pArcFile := @ArcFile;
                  ExecuteCompression(pArcFile^, TempFile, pHeaderObj);

                  // no change, delete the temp file and close all open files
                  If (FilesCompressed = 0) And (FilesDeleted = 0) Then
                  Begin
                     If CompressionMethod = cmInMemory Then
                        // prevent from moving or renaming of original archive
                        TTempMemStream(TempFile).FileMode := 0
                     Else
                        TTempFileStream(TempFile).FileMode := 0;
                     Exit;
                  End
                  Else
                     If (Switch = swDelete) And (FilesDeleted > 0) And
                        (FilesDeleted = pHeaderObj^.FileList.Count) Then
                     Begin
                        WriteZeroByteZipHeader(TempFile);
                     End;

                  doCleanUp(TempFile, pHeaderObj);
                  doFinish(pHeaderObj);

               Finally
                  If Assigned(ArcFile) Then
                     ArcFile.Free();
               End;

            Finally
               TempFile.Free();

               If fArcType = atTarGzip Then
               Begin
                  doTarGZip(fArchiveFile + '.gz');
                  fArchiveFile := fArchiveFile + '.gz';
               End;
            End;
   		Finally
            If Assigned(OnDeactivate) Then
               OnDeactivate(Self);
         End;

         If ztvFileExists(fArchiveFile) Then
         Begin
            CopySuccess := True;

            If (Switch = swMove) Then
            Begin
               DoProgress(0, 0);

               For i := 0 To DeleteFileList.Count - 1 Do
               Begin
                  If EraseFile(DeleteFileList[i], DeleteOptions) Then
                     If Assigned(OnDeleteFile) Then
                        OnDeleteFile(Self, DeleteFileList[i], i,
                           DeleteFileList.Count);
               End;
            End;
         End;

      Finally
         If Assigned(OnElapsedTime) Then
         Begin
      		ZipTimer.Stop();
            OnElapsedTime(Self, ZipTimer.ElapsedTime);
         End;
      End;
   Except
      FilesCompressed := 0;
      FilesDeleted := 0;
   End;
End;
//-------------------------------------------------------------

Function TCompBase.GetTotalRecordSize(ArcFile, TempFile: TStream32; pHeader:
         Pointer): Int64;
Begin
   Result := 0;                         // virtual method... do not delete!
End;
//-------------------------------------------------------------

Function TCompBase.HandleNonWriteableFile(FileName: AnsiString): Boolean;
Begin

   If Cancel Or (fOverwriteMode <> omOverwrite) Then
   Begin
      Result := False;
      Exit;
   End;

   If (Not IsWriteable(FileName)) Then  // non-writeable file attribute?
   Begin
      Result := False;                  //default
      If Assigned(OnNonWriteableArchive) Then
      Begin
         OnNonWriteableArchive(Self, FileName, Result);
         If (Not Result) Then           // user canceled
            Exit
         Else
            Result := SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_NORMAL);

      End
      Else
         RaiseErrorStr(FileName, 'OnNonWriteableArchive', '0', E_REQUIREDEVENT);
   End
   Else
      Result := True;

End;
//-------------------------------------------------------------

Procedure TCompBase.ReadFileInfo(pHeaderObj: pCompHeaderObj);
Var
   TempFile: TFileStream32;
   HeadType: Byte;
Begin
   Count := 0;
   If Assigned(OnRead) Then
   Begin
      TempFile :=
         TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyWrite);

   	If (TempFile.Handle < 0) Then
      Begin
         RaiseErrorStr(ArchiveFile, '', '0', E_FOPEN);
         Exit;
      End;

      Try
         If GetFirst(fFileName, TempFile, HeadType, pHeaderObj) Then
            Repeat
               DecypherHeader(fFileName);
               Inc(Count);
               OnRead(Self, TempFile.Position, Count);
            Until (Not GetNext(fFileName, TempFile, 0));
      Finally
         TempFile.Free();
      End;
   End
   Else
      RaiseErrorStr(fArchiveFile, 'OnRead', '0', E_REQUIREDEVENT);

End;
//-------------------------------------------------------------

Procedure TCompBase.SetFilename(SFN: AnsiString);
Begin
   fFileName := FormatFileName(SFN, fRootDir);
End;
//-------------------------------------------------------------

Procedure TCompBase.doRenameFile(Var sFilename: AnsiString;
   pHeaderObj: pCompHeaderObj);
Var
   NewFileName: AnsiString;
Begin
   If Assigned(OnRenameFile) Then       // removed for revisions
   Begin
      Repeat
         OnRenameFile(Self, sFilename, NewFileName);
         If ExtractFilename(NewFileName) = '' Then
         Begin
            SetLength(NewFileName, 0);
            Break;
         End
         Else
            NewFileName := ExtractFilePath(sFilename) +
               ExtractFilename(NewFileName);

         If CompareText(sFilename, NewFileName) = 0 Then
         Begin
            SetLength(NewFileName, 0);  // same FileName
            Break;
         End;

      Until pHeaderObj^.FileList.IndexOf(NewFileName) = -1;

      If (NewFileName <> '') And (CompareText(sFilename, NewFileName) <>
         0) Then
      Begin
         pCBFI^.Status := hsRename;
         sFilename := ExtractFilePath(sFilename) +
            ExtractFilename(NewFileName);
      End;
   End;
End;
//-------------------------------------------------------------

Function TCompBase.StoreStream(inStream, outStream: TStream32; zsp:
	ztv_stream_plus): Boolean;
Var
   StoreStream: TStoreStream;
Begin
   fGlobalCompressType := ZTV_STORED;
   StoreStream := TStoreStream.Create(outStream, 32, zsp, @fCancel, GlobalProgress);
   Try
      Result :=
         StoreStream.CopyStream(inStream) = inStream.Size;
   Finally
   	If Not zsp.Protect Then
      	Crc32Val := StoreStream.FZRec.cb.Crc Xor CRC_MASK;
      StoreStream.Free();
   End;
End;

// several methods are incorporated in the procedure to return the smallest
// possible resulting compressed stream.

Function TCompBase.AddFileProc(outStream: TStream32; Index: Integer; pHeaderObj:
   pCompHeaderObj): Boolean;

Var
   idx: Integer;
   HoldFN: AnsiString;

   zsp: ztv_stream_plus;
   DefType: TDeflateType;
   CryptHDR: String[RAND_HEAD_LEN];
   ProgressPos: Int64;
   outStreamPos: Int64;
   inStream: TFileStream32;
   CompressStream: TStream32;

   Function AddDirToList: Boolean;
   Begin
      fFileName := pHeaderObj^.FileList.Strings[Index]; //non-formating assignment

      Result := StoreEmptySubDirs And isDirEmpty(fFileName);
      If Result Then
      Begin

         CRC32Val := CRC_MASK;
         CRC16Val := 0;

         // v6.1 rem'd in favor of complete filename format according to
         // developers StoredDirNames setting
         //TempStoredDir := StoredDirNames;
         //Try
         //   Case StoredDirNames Of
         //      sdRelative:
         //         Begin
         //            StoredDirNames := sdRelativeStoreStart;
         //            fFileName := FormatFileName(fFileName, fRootDir);
         //         End;
         //   Else
         //      // format FileName
               FileName := fFileName;
         //   End;
         //Finally
         //   StoredDirNames := TempStoredDir;
         //End;

         fGlobalCompressType := ZTV_STORED;
         FillHeaderData(0, 0);          // v4.1 added

         // Write dir header
         outStream.WriteBuffer(HPtr^, HSize);

         // Convert to unix compatible file-naming convention
			fFileName := DOSToUnixFilename(fFileName);

         // Write dir name
         outStream.WriteBuffer(fFileName[1], Length(fFileName));
			fFileName := UnixToDosFilename(fFileName);

         WEI(outStream);                // v4.1 added

         // add header to list object
         RefreshHeader(Index, HeadInfo.Attr, pHeaderObj);
         Result := True;
      End;
   End;

Begin                                   //AddFileProc

   If (HeadInfo.Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
      Result := AddDirToList() And (Not Cancel)
   Else Begin
      DefType := DeflateType;
      // v4.6.8 removed StoreAlreadyCompressedFiles.  Added StoreFilesOfType property
      If (DefType <> dtDeflateS) {And StoreAlreadyCompressedFiles} Then
      Begin
         //v5.3 revised
         Idx := fStoreFilesOfType.IndexOf(ExtractFileExt(fFileName));
         If Idx > -1 Then
         Begin
            Crc32Val := 0;
            DefType := dtDeflateS;
         End;
      End;

      inStream :=
         TFileStream32.Create(pHeaderObj^.FileList.Strings[Index],
            fmOpenRead Or fmShareDenyNone);

   	If (inStream.Handle < 0) Then
      Begin
         RaiseErrorStr(pHeaderObj^.FileList.Strings[Index], '', '0', E_FOPEN);
         Result := False;
         Exit;
      End;

      Try
         Result := False;
         If (inStream.Size = 0) Then
         Begin
            fEncrypted := False;

            // prevent changing the BitFlag variable in
            // SetDeflateBitFlag call in FileHeaderData
            fGlobalCompressType := ZTV_STORED;
         End Else Begin
            fEncrypted := (Password <> '');
{$IFDEF DEFLATE64}
            fGlobalCompressType := Z_DEFLATE64;
{$ELSE}
            fGlobalCompressType := Z_DEFLATED;
{$ENDIF}
         End;

         Crc32Val := CRC_MASK;
         fUnpackedSize := inStream.Size;

         With zsp Do
         Begin
            Protect := fEncrypted;
            CRC := Crc32Val;
            pCancel := @fCancel;
            pArchivePos := @ProgressPosition;
         End;

         ProgressPos := ProgressPosition;

         // write header;
         outStreamPos := outStream.Position;
         outStream.WriteBuffer(HPtr^, HSize);

         HoldFN := fFileName;
         Try
            fFileName := CharToUnixOemFilter(fFileName, fTransOemChar);
            outStream.WriteBuffer(fFileName[1], Length(fFileName));

            If fEncrypted Then
            Begin
               Crc32Val :=
               	CalcStreamCRC32(
                  	InStream, InStream.Size, zsp, @fCancel,
                     GlobalProgress) Xor CRC_MASK;

               If Cancel Then Exit;
               CryptHDR := ztvEncryptHead(Password, 0, Crc32Val);
               outStream.WriteBuffer(CryptHDR[1], RAND_HEAD_LEN);
            End;

            // write a local extended field header
            Write64BitFieldHdr(outStream, pCBFI, htLocal);

            WEI(outStream);

            zsp.CRC := Crc32Val;
            ProgressPosition := ProgressPos;

            If DefType = dtDeflateS Then
               Result := StoreStream(inStream, outStream, zsp)
            Else Begin

               If inStream.Size < 257 Then
               Begin

                  CompressStream := TMemoryStream32.Create();
                  Try
                     Result :=
                        ztvCompressStreamProc(inStream, CompressStream, zsp,
                           Crc32Val, DefType, GlobalProgress, maxbits) And
                           (Not Cancel);

                     If CompressStream.Size > inStream.Size Then
                     Begin
                        If fEncrypted Then
                        Begin
                           // the encrypted header must be recalculated so the
                           // resulting crc matches the new encrypted stream
                           CryptHDR := ztvEncryptHead(Password, 0, Crc32Val);
                           outStream.Position := outStream.Position - RAND_HEAD_LEN;
                           outStream.WriteBuffer(CryptHDR[1], RAND_HEAD_LEN);
                        End;
                        Result := StoreStream(inStream, outStream, zsp)
                     End Else
                        outStream.CopyFrom(CompressStream, 0);
                  Finally
                     CompressStream.Free();
                  End;

               End Else Begin

                  Result :=
                     ztvCompressStreamProc(inStream, outStream, zsp,
                        Crc32Val, DefType, GlobalProgress, maxbits) And
                        (Not Cancel);
               End;

            End;

            FillHeaderData(
               inStream.Size,
               outStream.Size - outStreamPos - HSize - Length(fFileName));

            outStream.Position := outStreamPos;
            Try
            	outStream.WriteBuffer(HPtr^, HSize);
            	outStream.WriteBuffer(fFileName[1], Length(fFileName));

            	If fEncrypted Then
               	outStream.Seek(RAND_HEAD_LEN, soCurrent);

               pCBFI^.HighPackedSize :=
               	(outStream.Size - outStreamPos - HSize - Length(fFileName)) Div MAXDWORD;

            	Write64BitFieldHdr(outStream, pCBFI, htLocal);

            Except
            	//ON e: exception DO ShowMessage( e.message )
            End;

            outStream.Position := outStream.Size;
         Finally
         	fFileName := HoldFN;
         End;

         If (Not Result) Then Exit;

         (* add header to list object *)
         RefreshHeader(Index, HeadInfo.Attr, pHeaderObj);
      Finally
         inStream.Free();
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TCompBase.RefreshHeader(Index, ExtAttr: Integer; pHeaderObj:
   pCompHeaderObj);
Begin
   // Virtual method... do not delete!
End;
//-------------------------------------------------------------

Function TCompBase.doOnBegin(Var sFilename: AnsiString; FileNumber: Integer;
   pHeaderObj: pCompHeaderObj): Boolean;
Begin
   Result := True;

   If (Assigned(OnBegin)) Then
      OnBegin(Self, sFilename, FileNumber, Result);

   FileName := sFilename;

   If Result And (pCBFI^.Status <> hsRename) Then
      doRenameFile(fFileName, pHeaderObj);
End;
//-------------------------------------------------------------

Procedure TCompBase.doOnEnd;
Begin
   // virtual method... DO NOT DELETE!
End;
//-------------------------------------------------------------

Procedure TCompBase.doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
   pHeaderObj: pCompHeaderObj);
Begin
   //Virtual method... do not delete!
End;
//-------------------------------------------------------------

Procedure TCompBase.SetDefaultExt(SDE: AnsiString);
Var
   DE, Ext: AnsiString;
Begin
   DE := fDefaultExt;
   If (SDE <> '') And (SDE <> '.') Then
   Begin
      If Pos('.', SDE) = 0 Then
         SDE := '.' + SDE;

      If Length(SDE) > 4 Then
         SetLength(SDE, 4);

      fDefaultExt := LowerCase(SDE);
   End
   Else
      fDefaultExt := fMasterExt;

   If fArchiveFile <> '' Then
   Begin
      Ext := ExtractFileExt(fArchiveFile);
      If Ext = '' Then
         fArchiveFile := fArchiveFile + fDefaultExt
      Else If CompareText(Ext, DE) = 0 Then
         fArchiveFile := _ChangeFileExt(fArchiveFile, fDefaultExt);
   End;
End;
//-------------------------------------------------------------

Procedure TCompBase.SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo;
	CBFInew: TCompFileInfo);
Begin
   //Virtual method... do NOT delete!
End;
//-------------------------------------------------------------

Procedure TCompBase.WriteZeroByteZipHeader(TempStream: TStream32);
Begin
   //Virtual method... do NOT delete!
End;
//-------------------------------------------------------------

Procedure TCompBase.SetRootDir(SID: AnsiString);
Begin
   If SID = '' Then
      SID := GetCurrentDir();

   fRootDir := AppendDirTail(SID);
End;


(*************************************************************)
(*************************************************************)
(*                           TUnBase                  	    *)
(*************************************************************)
(*************************************************************)

Constructor TUnBase.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fCreateStoredDirs := False;
   fAsciiTranslate := False;
   fDateAttribute := daFileDate;
   fRecurseDirs := True;
   fRestoreFileAttr := True;
End;
//-------------------------------------------------------------

Destructor TUnBase.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TUnBase.Open_OutFile(Var Outfile: TStream32; FileName,
	OriginalFilename: AnsiString): Boolean;
Var
   FH: TStreamHeader;
Begin
   Result := fEncrypted And PasswordAttemptFailed;

   If (Not Result) And (FileName <> '') Then
      Case WriteMethod Of

         faAppend, faFile:
            Begin
               CreateDirEx(ExtractFileDir(FileName));
               If (WriteMethod = faAppend) Then
               Begin
               	Outfile := TFileStream32.Create(Filename, fmOpenReadWrite);
                  Outfile.Seek(0, soEnd);
               End Else
               	Outfile :=
                  	TFileStream32.Create(FileName, fmCreate Or fmShareExclusive);

               Result := TFileStream32(Outfile).Handle <> Integer(INVALID_HANDLE_VALUE);
            End;

         faUserStream:
            Result := Outfile <> Nil;

         faMemoryStream:
            Begin
               Result := Outfile <> Nil;
               If Result Then
               Begin
                  FH.Size := InflateRec.UnpackedSize;
                  FH.FileName := OriginalFilename;
                  FH.Offset := Outfile.Position;
                  TDecompMemoryStream(Outfile).AddItem(FH);
               End;
            End;
         faVerify: Result := True;
         faPointer: Result := True;
      Else
         Result := True;
      End;
End;
//-------------------------------------------------------------

Function TUnBase.RequestPassword(Infile: TStream32): Boolean;
Var
   j: Integer;
   Buffer: PChar;
   Action: Boolean;
Begin
   ZipTimer.Suspend();
   GetMem(Buffer, (RAND_HEAD_LEN * 2) + 1);
   Try

      // read the encrypted header of len RAND_HEAD_LEN into Buffer
      ReadBlock(Infile, Nil, Buffer^, False, 0, RAND_HEAD_LEN, dtHeader);

      // make a working copy of encrypted header in upper half of Buffer
      CopyMem(@Buffer[0], @Buffer[RAND_HEAD_LEN], RAND_HEAD_LEN);

      // does a password already exist from previous loops?  If so,
      // test to see its the correct one
      Result := VerifyPassword(Buffer);

      // first check the passwords string-list to see if the password already exists
      If Not Result Then
         For j := 1 To Passwords.Count Do
         Begin
            fPassword := Passwords.Strings[j - 1];
            Result := VerifyPassword(Buffer);
            If Result Then Break;
         End;

      // Password was not found in list, request from user
      If Not Result Then
         If Assigned(OnGetPassword) Then
         Begin

            // PasswordAttemps - #chances of entering correct pw
            Action := True;
            For j := 1 To PasswordAttempts Do
            Begin
               If Cancel Then Break;

               OnGetPassword(Self, FileName, fPassword, Action);

               If Action Then
               Begin
                  Result := VerifyPassword(Buffer);
                  If Result Then
                     Passwords.insert(0, fPassword)
                  Else
                  Begin
                     SetLength(fPassword, 0);
                     RaiseErrorStr(FileName, '', '0', M_INVALIDPW);
                     Continue;
                  End;
               End
               Else
               Begin                    // user requested cancel password entry
                  RaiseErrorStr(FileName, '', '0', M_INVALIDPW);
                  Break;
               End;

               Break;                   // Break out of getpassword loop
            End;
         End
         Else
            RaiseErrorStr(FileName, 'OnGetPassword', '0', E_REQUIREDEVENT);

   Finally
      ZipTimer.Resume();
      FreeMem(Buffer, (RAND_HEAD_LEN * 2) + 1);
   End;
End;
//-------------------------------------------------------------

Function CheckWildCard1(FileName: AnsiString; FileSpec, ExcludeSpec: TStrings): Boolean;
Var
   i, j: Integer;
   FN: AnsiString;
Begin
   Result := False;
   If FileName <> '' Then
   Begin
      FN := FileName;

      For i := 0 To FileSpec.Count - 1 Do
      Begin

         // extension management
      	If Pos('.', FileSpec[i]) > 0 Then
         Begin
            If Pos('.', FileName) > 0 Then
               FN := FileName
            Else
               FN := FileName + '.';
         End Else
         	If FN[Length(FN)] = '.' Then
               // use original (unedited) filename
   				FN := FileName;


         If MatchesMask(FN, FileSpec[i]) Then
         Begin
            If ExcludeSpec <> Nil Then
               For j := 0 To ExcludeSpec.Count - 1 Do
                  If MatchesMask(FN, ExcludeSpec[j]) Then
                     Exit;

            Result := True;
            Break;
         End;
      End;
   End;
End;
//-------------------------------------------------------------

Function CheckWildCard2(FileName: AnsiString; FileSpec, ExcludeSpec: TStrings;
	RecurseDirs: Boolean): Boolean;

    Function ExtractFilePath(FileName: AnsiString): AnsiString;
    Var
       i: Integer;
    Begin
       i := Length(FileName);

       If (i = 3) And (Pos(':', FileName) > 0) Then

          Result := FileName

       Else
       Begin

          While (i > 0) And Not (FileName[i] In ['\', '/', ':']) Do
             Dec(i);

          If i > 0 Then
             If (FileName[i] = '\') Or (FileName[i] = '/') Then
                If i <> 3 Then
                   Dec(i)
                Else If FileName[2] <> ':' Then
                   Dec(i);

          Result := Copy(FileName, 1, i);
       End;
    End;
 Var
   i, j: Integer;
   OrigFileName,
   	Dir1,
   	Dir2: AnsiString;
   b, Compare: Boolean;
 Begin
 	Result := False;
   If FileName = '' Then Exit;
   OrigFileName := FileName;

 	Dir1 := AppendDirTail(ExtractFilePath(FileName));
   For i := 0 To FileSpec.Count - 1 Do
   Begin
   	If (FileSpec[i] = '') Then Continue;

      If CompareText(FileSpec[i], OrigFileName) = 0 Then
      Begin
      	Result := True;
         Exit;
      End;

      FileName := OrigFileName;

      Dir2 := AppendDirTail(ExtractFilePath(FileSpec[i]));
      If RecurseDirs Then
      Begin
			If Dir2 = '' Then
         Begin
         	FileName := ExtractFileName(FileName);
          	Compare := True;
         End Else
         	Compare := CompareText(Dir2, Copy(Dir2, 1, Length(Dir1))) = 0;
      End Else
      	Compare := CompareText(Dir1, Dir2) = 0;

      If (Not Compare) And (Not RecurseDirs) Then
         Continue;

      b := MatchesMask(FileName, FileSpec[i]);
      If (Not b) Then
      Begin
      	If Pos( '.', FileName ) = 0 Then
            FileName := FileName + '.';

         If Pos( '.', FileSpec[i] ) = 0 Then
            FileSpec[i] := FileSpec[i] + '.';

         b := MatchesMask(FileName, FileSpec[i]);
      End;

      If b Then
      Begin
         If ExcludeSpec <> Nil Then
            For j := 0 To ExcludeSpec.Count - 1 Do
               If MatchesMask(FileName, ExcludeSpec[j]) Then
                  Exit;

         Result := True;
         Break;
      End;
   End;
 End;
//-------------------------------------------------------------

Function TUnBase.doOnBegin(BypassReturnTrue: Boolean): Boolean;
Var
   Action: Boolean;
   NewFileName: AnsiString;
   FileAttr: Integer;
Begin
   If BypassReturnTrue Then
      Result := True
   Else
   Begin
      ZipTimer.Suspend();
      Try
         Action := True;

         If Assigned(OnBegin) Then
         Begin
            If WriteToFile() Then
            Begin
               If Assigned(OnRenameFile) Then
               Begin
                  NewFileName := fFileName;
                  OnRenameFile(Self, fFileName, NewFileName);
                  If NewFileName <> '' Then
                  Begin
                  	// revised v6.2
      					CreateDirEx(ExtractFilePath(NewFilename));
                     //If Not _DirectoryExists(ExtractFilePath(NewFileName)) Then
                     //   NewFileName :=
                     //      ExtractFilePath(fFileName) + ExtractFileName(NewFileName);

                     fFileName := NewFileName
                  End;
               End;

               OnBegin(Self, fFileName, Count+1, Action)
            End Else
               OnBegin(Self, ActualFilename, Count+1, Action);
         End;

         If Cancel Or (Not Action) Then
         Begin
            Result := False;
            Exit;
         End;

         If (Not WriteToFile()) Then
         Begin
            Result := True;
            Inc(Count);
            Exit;
         End;

         If (Not fConfirmOverwrites) Then
         Begin
            Result := fOverwriteMode = omOverwrite;
            If Result Then Inc(Count);
            Exit;
         End;

         fOverwriteMode := omOverwrite;
         If FileExists(fFileName) Then
         Begin
            If Assigned(OnFileExists) Then
            Begin
               Repeat
                  NewFileName := '';
                  OnFileExists(Self, fFileName, NewFileName, fOverwriteMode);
                  If (NewFileName = '') Then NewFileName := fFileName;

                  // revised v6.2
                  CreateDirEx(ExtractFilepath(NewFilename));
                  //If Not _DirectoryExists(ExtractFilePath(NewFileName)) Then
                  //   NewFileName :=
                  //      ExtractFilePath(fFileName) + ExtractFileName(NewFileName);

                  If (fOverwriteMode = omOverwrite) And (NewFileName <> '') Then
                     If CompareText(fFileName, NewFileName) <> 0 Then
                        fFileName := NewFileName
                     Else
                        Break;
               Until Cancel Or (fOverwriteMode = omSkip) Or (Not ztvFileExists(fFileName));

               If (Not Cancel) And (fOverwriteMode <> omSkip) Then
               Begin
                  FileAttr := GetFileAttributes(PChar(fFileName));
                  If GetLastError() = NO_ERROR {ERROR_FILE_NOT_FOUND} Then
                     If (FileAttr And FILE_ATTRIBUTE_READONLY > 0) Then
                        If (FileAttr And FILE_ATTRIBUTE_SYSTEM = 0) And
                              (FileAttr And FILE_ATTRIBUTE_DIRECTORY = 0) Then
                           SetFileAttributes(PChar(fFileName), FILE_ATTRIBUTE_NORMAL)
                        Else Begin
                           RaiseErrorStr(fFileName, '', '0', E_FWRITE);
                           Result := False;
                           Exit;
                        End;
               End;
            End
            Else
            Begin
               RaiseErrorStr(FileName, 'OnFileExists', '0', E_REQUIREDEVENT);
               fOverwriteMode := omSkip;
            End;
         End;

         Result :=
         	(Not Cancel) And (fOverwriteMode = omOverwrite) And (fFileName <> '');

         If Result Then Inc(Count);

      Finally
         ZipTimer.Resume();
      End;
   End;
End;
//-------------------------------------------------------------
(* Returns the Boolean result of crc validation *)

Function TUnBase.doOnEnd(BitSize: Byte; CRC: u_long): Boolean;
Begin
	If Cancel Then
   	Result := False
   Else
      If BitSize = 16 Then
         Result := (Crc16Val = CRC) Or ((Crc16Val Xor CRC_MASK) = CRC)
      Else                                 //has to be 32
         Result := (Crc32Val = CRC) Or ((Crc32Val Xor CRC_MASK) = CRC);

   If Assigned(OnEnd) Then
   Begin
      If WriteToFile() Then
      	OnEnd(Self, fFileName, Result)
      Else
         OnEnd(Self, ActualFilename, Result);
   End
   Else If (Not Result) And Assigned(OnError) Then
      If WriteToFile() Then
      	RaiseErrorStr(fFileName, '', '0', E_CRCERROR)
      Else
         RaiseErrorStr(ActualFilename, '', '0', E_CRCERROR);
End;
//-------------------------------------------------------------

Procedure TUnBase.AdjustProgress(InflateRec: TInflateRec);
Begin
   With InflateRec Do
   Begin
      ProgressPosition := ProgressPosition - UnpackedSize;
      doBranchProgress(UnpackedSize, UnpackedSize, fTotalUnpackedSize);
   End;
End;
//-------------------------------------------------------------

Procedure TUnBase.CloseAndSetDate(s: TStream32; FileName: AnsiString;
	dt, fa: Integer);
Var
	r: Integer;
Begin

   If WriteToFile() Then
   Begin
      Try
         r := FileSetDate(TFileStream32(s).Handle, GetDateTime(dt));
         If r <> 0 Then ;
      Except
      End;

      If Not Close_OutFile(s) Then
         RaiseErrorStr(FileName, '', '0', E_FCLOSE);

      If RestoreFileAttr And (fa <> FILE_ATTRIBUTE_ARCHIVE) Then
         SetFileAttributes(PChar(FileName), fa);
   End
   Else
      fFileName := ActualFilename;

End;
//-------------------------------------------------------------

Function TUnBase.VerifyPassword(Buffer: PChar): Boolean;
Begin
   Result := False;                     // virtual method... do not delete!
End;
//-------------------------------------------------------------

Procedure TUnBase.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
Begin
   (* Virtual (dont convert to abstract!) method - do not delete  *)
End;
//-------------------------------------------------------------

Procedure TUnBase.SetArchiveFile(SAF: String);
Begin
	Inherited;
End;
//-------------------------------------------------------------

Procedure TUnBase.Reset;
Begin
   Cancel := False;
   Count := 0;
   DoProgress := RetProgress;
   fEncrypted := False;
   fFilePos := 0;
   FilesToExtract := 0;
   fMaxAge := 0;
   fTotalPackedSize := 0;
   fTotalUnpackedSize := 0;
   IsGzTarArchive := False;
   PasswordAttemptFailed := False;
   PercentByArchive := 0;
   ProgressPosition := 0;
   pUBFI := @UBFI;

   If CpuType = cpt80486 Then CpuType := cptAuto;

   fVolumeName := fArchiveFile;
   fExcludeSpec.Clear();             //isn't used in decompression components
End;
//-------------------------------------------------------------

Function TUnBase.InitializeArcFile(Var inStream: TStream32; Outfile: TStream32): Boolean;
Begin
   Result := False;

   If fArchiveFile = '' Then
   Begin
      RaiseErrorStr(fArchiveFile, '', '0', E_FILENOTFOUND);
      Exit;
   End;

   If fArcType In [atZipDS, atZipMV] Then
      If Not DiskManager.GetDriveInfo(
                  fArchiveFile, RaiseError,
                  RaiseErrorStr, OnDiskWriteProtectErr,
                  OnDiskInDrvErr) Then
      Exit;


   inStream := Nil;
   If Not (ArcType In Wrapper_ArcType) Then
   Begin
      inStream :=
         TFileStream32.Create(fArchiveFile,
            fmOpenRead Or fmShareDenyWrite);

      fLOF := inStream.Size;

      If (TFileStream32(inStream).Handle < 0) Then
      Begin
         RaiseErrorStr(ArchiveFile, '', '0', E_FOPEN);
         Exit;
      End;
   End;

   Result := True;
End;
//-------------------------------------------------------------

Procedure TUnBase.DestroyArcFile(Var inStream: TStream32; Outfile: TStream32);
Begin
   If inStream <> Nil Then
   Begin
      inStream.Free();
      inStream := Nil;
   End;
End;
//-------------------------------------------------------------

Function TUnBase.UnBase_Execute(Var inStream: TStream32; Outfile: TStream32): Integer;
Var
   StartArchiveName: AnsiString;
Begin

	Result := 0;
   Try
   	Reset();
      StartArchiveName := fArchiveFile;

      Try
         If Assigned(OnActivate) Then
            OnActivate(Self);

         Try

            If Assigned(OnElapsedTime) Then
               ZipTimer.Start();

            Try
               fPassword := '';
               fFileName := '';
               fOffsetEnd := fLOF;

               If (ArcType <> atMsGZ) And (ArcType <> atGZip) Then
                  AsciiTranslation := False;

               ExtractIT(Instream, Outfile);

            Finally
               If Assigned(OnElapsedTime) Then
               Begin
                  ZipTimer.Stop();
                  OnElapsedTime(Self, ZipTimer.ElapsedTime);
               End;
            End;

         Finally
            If Assigned(OnDeactivate) Then
               OnDeactivate(Self);
         End;
      Finally
         Result := Count;
         fArchiveFile := StartArchiveName;
      End;
   Except
   End;
End;
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Function TUnBase.ExtractStreamToDisk(Source: TStream): Integer;
Var
   Dest: TStream32;
Begin
   Dest := TMemoryStream32.Create();
   Try
   	Source.Position := 0;
      TStreamToZTVStream(Source, Dest);
      Result := ExtractStreamToDisk(Dest);
   Finally
      Dest.Free();
   End;
End;
{$endif}
//-------------------------------------------------------------

// do not include InitializeArcFile() in this Extract method!
Function TUnBase.ExtractStreamToDisk(Source: TStream32): Integer;
Var
	Outfile: TStream32;
Begin
	ReadMethod := faFile;
   WriteMethod := faFile;
   fArcType := GetArcType(Source);
   If IsArcValid(fArcType) Then
   	Result := UnBase_Execute(Source, Outfile);
End;
//-------------------------------------------------------------

Function TUnBase.Extract: Integer;
Var
   inStream, Outfile: TStream32;
Begin
   (* do not change read/Write methods until future versions *)
   ReadMethod := faFile;
   WriteMethod := faFile;
   If Not InitializeArcFile(inStream, Outfile) Then
   	Result := 0
   Else
   	Try
   		Result := UnBase_Execute(inStream, Outfile);
      Finally
      	DestroyArcFile(inStream, Outfile);
      End;
End;
//-------------------------------------------------------------

Function TUnBase.ExtractToFileStream(FileName: AnsiString; fStrm: TFileStream32): Integer;
Var
   HoldFileSpec: TStringList;
   inStream, Outfile: TStream32;
Begin
   If fStrm <> Nil Then
   Begin

      (* do not change read/Write methods until future versions *)
      ReadMethod := faFile;
      WriteMethod := faUserStream;

      HoldFileSpec := TStringList.Create();
      Try
         Try
            HoldFileSpec.Assign(FileSpec);
            FileSpec.Clear();
            FileSpec.Add(FileName);

            If Not InitializeArcFile(inStream, Outfile) Then
               Result := 0
            Else
               Try
                  Result := UnBase_Execute(inStream, Outfile);
               Finally
                  DestroyArcFile(inStream, Outfile);
               End;
         Finally
            FileSpec.Assign(HoldFileSpec);
         End;
      Finally
         HoldFileSpec.Free();
      End;
   End
   Else
   Begin
      RaiseErrorStr(fArchiveFile, '', '0', E_FILESTREAM);
      Result := 0;
   End;
End;
//-------------------------------------------------------------

Function TUnBase.ExtractToPointer(FileName: AnsiString; p: Pointer): Integer;
Var
   HoldFileSpec: TStrings;
   inStream, Outfile: TStream32;
Begin
   If p <> Nil Then
   Begin

      (* do not change read/Write methods until future versions *)
      ReadMethod := faFile;
      WriteMethod := faPointer;
		ztvWritePTR := p;

      HoldFileSpec := TStringList.Create();
      Try
         Try
            HoldFileSpec.Assign(FileSpec);
            FileSpec.Clear();
            FileSpec.Add(FileName);

            If Not InitializeArcFile(inStream, Outfile) Then
               Result := 0
            Else
               Try
                  Result := UnBase_Execute(inStream, Outfile);
               Finally
                  DestroyArcFile(inStream, Outfile);
               End;

         Finally
            FileSpec.Assign(HoldFileSpec);
         End;
      Finally
         HoldFileSpec.Free();
      End;
   End
   Else
   Begin
      RaiseErrorStr(fArchiveFile, '', '0', E_FILESTREAM);
      Result := 0;
   End;
End;
//-------------------------------------------------------------

Function TUnBase.ExtractToMemoryStream(MemStrm: TStream32{TDecompMemoryStream}): Integer;
Var
   inStream, Outfile: TStream32;
Begin
   If MemStrm <> Nil Then
   Begin

      (* do not change read/Write methods until future versions *)
      ReadMethod := faFile;
      WriteMethod := faMemoryStream;

      If InitializeArcFile(inStream, Outfile) Then
         Try
            Result := UnBase_Execute(inStream, MemStrm);
         Finally
            DestroyArcFile(inStream, Outfile);
         End
      Else
         Result := 0;
   End
   Else
   Begin
      RaiseErrorStr(fArchiveFile, '', '0', E_MEMSTREAM);
      Result := 0;
   End;

End;
//-------------------------------------------------------------

Function TUnBase.ExtractToSearch: Integer;
Var
   inStream, Outfile: TStream32;
Begin
   (* do not change read/Write methods until future versions *)
   ReadMethod := faFile;
   WriteMethod := faSearch;

   If Not InitializeArcFile(inStream, Outfile) Then
   	Result := 0
   Else
   	Try
   		Result := UnBase_Execute(inStream, Outfile);
      Finally
      	DestroyArcFile(inStream, Outfile);
      End;
End;
//-------------------------------------------------------------

Function TUnBase.ExtractToVerify: Integer;
Var
   inStream, Outfile: TStream32;
Begin
   (* do not change read/Write methods until future versions *)
   ReadMethod := faFile;
   WriteMethod := faVerify;
   If Not InitializeArcFile(inStream, Outfile) Then
   	Result := 0
   Else
   	Try
   		Result := UnBase_Execute(inStream, Outfile);
      Finally
      	DestroyArcFile(inStream, Outfile);
      End;
End;
//-------------------------------------------------------------

Function TUnBase.ExtractToNul: Integer;
Var
   inStream, Outfile: TStream32;
Begin
   (* do not change read/Write methods until future versions *)
   WriteMethod := faNul;
   ReadMethod := faFile;
   If Not InitializeArcFile(inStream, Outfile) Then
   	Result := 0
   Else
   	Try
   		Result := UnBase_Execute(inStream, Outfile);
      Finally
      	DestroyArcFile(inStream, Outfile);
      End;
End;
//-------------------------------------------------------------


(*************************************************************)
(*************************************************************)
(*                      Misc Functions              			 *)
(*************************************************************)
(*************************************************************)

Function is64BitEndingHdr(SignAtr: Integer): Boolean;
Begin
   Result :=
      (SignAtr = END_OF_CENTRAL64_HEADER_SIGNATURE) Or
      (SignAtr = END_OF_CENTRAL64_ENCRPT_SIGNATURE);
End;
//-------------------------------------------------------------

Function CPassword(Op: Byte; pw: AnsiString; xor_value: Byte): AnsiString;
Const
   CONVERT = 0;
   RESTORE = 1;
Var
   p: Word;
Begin
   For p := 1 To Length(pw) Do
      If Op = CONVERT Then
         pw[p] := char(Byte(pw[p]) + xor_value)
      Else
         pw[p] := char(Byte(pw[p]) - xor_value);

   Result := pw;
End;
//-------------------------------------------------------------

Function EraseFile(Const fName: AnsiString; method: TDeleteOptions): Boolean;
Type
   PSHNameMapping = ^TSHNameMapping;
Var
   R: Boolean;
   SHF: TSHFileOpStruct;
Begin
   If method = doAllowUndo Then
   Begin
      If ztvFileExists(fName) Then
      Begin
         R := True;
         With SHF Do
         Begin
            Wnd := Application.Handle;
            wFunc := FO_DELETE;
            pFrom := PChar(fName + #0);
            pTo := Nil;
            fFlags := FOF_SILENT Or FOF_NOCONFIRMATION Or FOF_RENAMEONCOLLISION;
            fAnyOperationsAborted := R;
            If method = doAllowUndo Then
               fFlags := fFlags Or FOF_ALLOWUNDO;
         End;
         Result := (SHFileOperation(SHF) = 0) And R;
      End
      Else
         Result := False;
   End
   Else
      Result := DeleteFile(fName);
End;
//-------------------------------------------------------------

Function AttributesToInt(Attr: TztvFileAttrs): Integer;
Begin
	Result := 0;
   If fsReadOnly In Attr Then Result := Result Or ZTV_FILE_ATTRIBUTE_READONLY;
   If fsHidden In Attr Then Result := Result Or ZTV_FILE_ATTRIBUTE_HIDDEN;
   If fsSysFile In Attr Then Result := Result Or ZTV_FILE_ATTRIBUTE_SYSTEM;
   If fsVolumeID In Attr Then Result := Result Or SysUtils.faVolumeID;
   If fsDirectory In Attr Then Result := Result Or ZTV_FILE_ATTRIBUTE_DIRECTORY;
   If fsArchive In Attr Then Result := Result Or ZTV_FILE_ATTRIBUTE_ARCHIVE;
   If fsCompressed In Attr Then Result := Result Or ZTV_FILE_ATTRIBUTE_COMPRESSED;
   If fsEncrypted In Attr Then Result := Result Or ZTV_FILE_ATTRIBUTE_ENCRYPTED;
End;
//-------------------------------------------------------------

Procedure Crc16_buf(str: PChar; Len: Integer; Var CRC: u_long);
Begin
   While Len > 0 Do
   Begin
      CRC := (CRC Shr 8) Xor CRC16Table[(CRC Xor (Byte(str^))) And $00FF];
      Inc(str);
      Dec(Len);
   End;
End;
//-------------------------------------------------------------

Procedure Crc32_buf(str: PChar; Len: Integer; Var CRC: u_long);
	Function UpdC32(Octet: Byte; CRC: u_long): u_long;
   Begin
   	Result := Crc32Table[(CRC Xor u_long(Octet)) And $00FF] Xor
      	((CRC Shr 8) And $00FFFFFF)
   End;
Begin
   While Len > 0 Do
   Begin
      CRC := UpdC32(Byte(str^), CRC);
      Inc(str);
      Dec(Len);
   End;
End;
//-------------------------------------------------------------

// v4.8.7 optimized for speed
Function CalcCheckSum(Header: TTarHeader): Integer;
Type
   HdrArray = Array[0..SizeOf(Header) - 1] Of Byte;
Var
   HeaderPtr: ^HdrArray;
   i,
   	EOChkSumPtr,
   	ChkSumPtr: Integer;
Begin
   Result := 0;
   HeaderPtr := @Header;
   ChkSumPtr := Ptr2Int(@Header.ChkSum);
   EOChkSumPtr := ChkSumPtr + SizeOf(Header.ChkSum);
   For i := 0 To High(HdrArray) Do
      If (Ptr2Int(@HeaderPtr^[i]) < ChkSumPtr) Or
         (Ptr2Int(@HeaderPtr^[i]) >= EOChkSumPtr) Then
         inc(Result, HeaderPtr^[i])
      Else
         inc(Result, ord(' '));
End;
//-------------------------------------------------------------

Function ValidateTarHeader(Buf: Pointer): Boolean;
Type
   HdrArray = Array[0..SizeOf(TTarHeader)] Of Byte;
Var
   i, R: Integer;
   HeaderPtr: ^HdrArray;
   pTarHeader: ^TTarHeader;
Begin
   R := 0;
   HeaderPtr := Buf;
   pTarHeader := Buf;
   Try
      With pTarHeader^ Do
      Begin
         For i := 0 To SizeOf(TTarHeader) - 1 Do
            If (Integer(@HeaderPtr^[i]) < Integer(@ChkSum)) Or
               (Integer(@HeaderPtr^[i]) >= (Integer(@ChkSum) +
               SizeOf(ChkSum))) Then
               Inc(R, HeaderPtr^[i])
            Else
               Inc(R, ord(' '));
      End;
   Finally
      Result := R = OctStrToInt(pTarHeader^.ChkSum);
   End;
End;


//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
Initialization
   dwTimeZoneMode := GetTimeZoneInformation(TZInfo);
End.
