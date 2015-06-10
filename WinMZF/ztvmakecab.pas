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
Unit ztvMakeCab;

(* CAUTION: (Delphi 2 only) If DYNLOADCABDLL is "not" defined, the
   CABINET.DLL must be found.  If not found, Delphi will be unable
   to load the cmplib32.dcl library the next time Delphi is run. *)

{$DEFINE DYNLOADCABDLL}

Interface

Uses
   Windows,
   Classes,
   SysUtils,
   ztvBase,
   Forms,
   Dialogs,
   ztvGbls,
   ztvHeaders,
   ztvCabGbls,
   ztvStreams,
   Err_Msgs;

{$I ZipTV.inc}                          //Declare the compiler defines

(* FCIERROR - Error codes returned in erf.erfOper field *)
Type
   TFCIERROR =
      (
      FCIERR_NONE,                      (* No error                                    	*)
      FCIERR_OPEN_SRC,                  (* Failure opening file to be stored in cabinet  *)
      (*  erf.erfTyp has C run-time *errno* value      *)
      FCIERR_READ_SRC,                  (* Failure reading file to be stored in cabinet  *)
      (*  erf.erfTyp has C run-time *errno* value      *)
      FCIERR_ALLOC_FAIL,                (* Out of memory in FCI                          *)
      FCIERR_TEMP_FILE,                 (* Could not create a temporary file             *)
      (*  erf.erfTyp has C run-time *errno* value      *)
      FCIERR_BAD_COMPR_TYPE,            (* Unknown compression TYPE                      *)
      FCIERR_CAB_FILE,                  (* Could not create cabinet file                 *)
      (*  erf.erfTyp has C run-time *errno* value      *)
      FCIERR_USER_ABORT,                (* Client requested abort                        *)
      FCIERR_MCI_FAIL                   (* Failure compressing data                      *)
      );

   (* HFCI - Handle to an FCI Context *)
Type
   HFCI = PVoid;

   (*    CCAB - Current Cabinet
    *
    *  This structure is used for passing in the cabinet parameters to FCI,
    *  and is passed back on certain FCI callbacks to provide cabinet
    *  information to the client.
    *)

Type
   TCCAB = Packed Record
      cb: TULONG;                       (* 4  size available for cabinet on this media 	*)
      cbFolderThresh: TULONG;           (* 8  Thresshold for forcing a new Folder      	*)
      cbReserveCFHeader: TUINT;         (* 10 Space to reserve in CFHEADER             	*)
      cbReserveCFFolder: TUINT;         (* 12 Space to reserve in CFFOLDER             	*)
      cbReserveCFData: TUINT;           (* 14 Space to reserve in CFDATA               	*)
      iCab: Integer;                    (* 18 sequential numbers for cabinets          	*)
      iDisk: Integer;                   (* 22 Disk number                              	*)

{$IFNDEF REMOVE_CHICAGO_M6_HACK}
      fFailOnIncompressible: Integer;   (* TRUE => Fail if a block is incompressible *)
{$ENDIF}

      setID: TUUSHORT;                  (* Cabinet set ID						*)
      szDisk: Array[0..256 - 1] Of char; (* current disk name             *)
      szCab: Array[0..256 - 1] Of char; (* current cabinet name          *)
      szCabPath: Array[0..256 - 1] Of char; (* path for creating cabinet     *)
   End;
   PCCAB = ^TCCAB;

   (*     FNFCIALLOC - Memory Allocation
    *      FNFCIFREE  - Memory Free
    *
    *  These are modeled after the C run-time routines malloc() and free()
    *  FCI expects error handling to be identical to these C run-time routines.
    *
    *  As long as you faithfully copy the semantics of malloc() and free(),
    *  you can supply any functions you like!
    *
    *  WARNING: You should never assume anything about the sequence of
    *           FNFCIALLOC and FNFCIFREE calls -- incremental releases of
    *           FCI may have radically different numbers of
    *           FNFCIALLOC calls and allocation sizes!
    *)

     (* File I/O functions for FCI *)
Type
   TFNFCIOPEN = Function(pszFile: PChar; oflag: Integer; pmode: Integer;
      err: PInteger; pv: Pointer): Integer; CDECL;
   PFNFCIOPEN = TFNFCIOPEN;

   TFNFCIREAD = Function(hf: Integer; memory: PVoid; cb: TUINT;
      err: PInteger; pv: Pointer): TUINT; CDECL;
   PFNFCIREAD = TFNFCIREAD;

   TFNFCIWRITE = Function(hf: Integer; memory: PVoid; cb: TUINT;
      err: PInteger; pv: Pointer): TUINT; CDECL;
   PFNFCIWRITE = TFNFCIWRITE;

   TFNFCICLOSE = Function(hf: Integer; err: PInteger; pv: Pointer):
      Integer; CDECL;
   PFNFCICLOSE = TFNFCICLOSE;

   TFNFCISEEK = Function(hf: Integer; dist: longint; SeekType: Integer;
      err: PInteger; pv: Pointer): longint; CDECL;
   PFNFCISEEK = TFNFCISEEK;

   TFNFCIDELETE = Function(pszFile: PChar; err: PInteger; pv: Pointer):
      Integer; CDECL;
   PFNFCIDELETE = TFNFCIDELETE;

   (*    FNFCIGETNEXTCABINET - Callback used to request new cabinet info
    *
    *  Entry:
    *      pccab     - Points to copy of old ccab structure to modify
    *      cbPrevCab - Estimate of size of previous cabinet
    *      pv        - Has the caller's context pointer
    *
    *  Exit-Success:
    *      returns True;
    *
    *  Exit-Failure:
    *      returns False;
    *)

Type
   TFNFCIGETNEXTCABINET = Function(PCCAB: PCCAB; cbPrevCab: TULONG;
      pv: Pointer): Boolean; CDECL;
   PFNFCIGETNEXTCABINET = TFNFCIGETNEXTCABINET;

   (*    FNFCIFILEPLACED - Notify FCI client that file was placed
    *
    *  Entry:
    *      pccab         - cabinet structure to fill in, with copy of previous one
    *      pszFile       - name of file, from cabinet
    *      cbFile        - length of file
    *      fContinuation - True if this is a later segment of a continued file
    *      pv            - the context of the client
    *
    *  Exit-Success:
    *      return value anything but -1
    *
    *  Exit-Failure:
    *      return value -1 means to abort
    *)

Type
   TFNFCIFILEPLACED = Function(PCCAB: PCCAB; pszFile: PChar;
      cbFile: longint; fContinuation: Boolean; pv: Pointer): Integer; CDECL;
   PFNFCIFILEPLACED = TFNFCIFILEPLACED;

   { pfnfcifp }

(*    FNCDIGETOPENINFO - Open source file, get date/time/attribs
*
*  Entry:
*      pszName  -- complete path to filename
*      pdate    -- location to return FAT-style date code
*      ptime    -- location to return FAT-style time code
*      pattribs -- location to return FAT-style attributes
*      pv       -- client's context
*
*  Exit-Success:
*      Return value is file handle of open file to read
*
*  Exit-Failure:
*      Return value is -1
*)

Type
   TFNFCIGETOPENINFO = Function(pszName: PChar; Var pdate: TUUSHORT;
      Var ptime: TUUSHORT; Var pattribs: TUUSHORT; err: PInteger; pv: Pointer):
      Integer; CDECL;
   PFNFCIGETOPENINFO = TFNFCIGETOPENINFO;

   { pfnfcigoi }

(*    FNFCISTATUS - Status/Cabinet Size callback
*
*  Entry:
*      typeStatus == statusFile if compressing a block into a folder
*                      cb1 = Size of compressed block
*                      cb2 = Size of uncompressed block
*
*      typeStatus == statusFolder if adding a folder to a cabinet
*                      cb1 = Amount of folder copied to cabinet so far
*                      cb2 = Total size of folder
*
*      typeStatus == statusCabinet if writing out a complete cabinet
*                      cb1 = Estimated cabinet size that was previously
*                              passed to fnfciGetNextCabinet().
*                      cb2 = Actual cabinet size
*                    NOTE: Return value is desired client size for cabinet
*                          file.  FCI updates the maximum cabinet size
*                          remaining using this value.  This allows a client
*                          to generate multiple cabinets per disk, and have
*                          FCI limit the size correctly -- the client can do
*                          cluster size rounding on the cabinet size!
*                          The client should either return cb2, or round cb2
*                          up to some larger value and return that.
*  Exit-Success:
*      Returns anything other than -1;
*      NOTE: See statusCabinet for special return values!
*
*  Exit-Failure:
*      Returns -1 to signal that FCI should abort;
*)

Const
   statusFile = 0;
   statusFolder = 1;
   statusCabinet = 2;

Type
   TFNFCISTATUS = Function(typeStatus: TUINT; cb1: TULONG; cb2: TULONG;
      pv: Pointer): longint; CDECL;
   PFNFCISTATUS = TFNFCISTATUS;

Type
   TERF = Packed Record
      erfOper: Integer;                 (* FCI/FDI error code -- see FDIERROR_XXX		*)
      (*  and FCIERR_XXX equates for details.      *)
      erfType: Integer;                 (* Optional error value filled in by FCI/FDI.*)
      (* For FCI, this is usually the C run-time   *)
      (* *errno* value.                            *)
      fError: Boolean;                  (* True => error present                     *)
   End;
   PERF = ^TERF;

   (* Memory functions for FCI *)
Type
   TFNFCIALLOC = Function(cb: TULONG): PVoid; CDECL;
   PFNFCIALLOC = TFNFCIALLOC;

   TFNFCIFREE = Function(memory: PVoid): Pointer; CDECL;
   PFNFCIFREE = TFNFCIFREE;

   (*    FNFCIGETTEMPFILE - Callback, requests temporary file name
    *
    *  Entry:
    *      pszTempName - Buffer to receive complete tempfile name
    *      cbTempName  - Size of pszTempName buffer
    *
    *  Exit-Success:
    *      return True
    *
    *  Exit-Failure:
    *      return False; could not create tempfile, or buffer too small
    *
    *  Note:
    *      It is conceivable that this function may return a filename
    *      that will already exist by the time it is opened.  For this
    *      reason, the caller should make several attempts to create
    *      temporary files before giving up.
    *)

Type
   TFNFCIGETTEMPFILE = Function(pszTempName: PChar; cbTempName: Integer;
      pv: Pointer): Boolean; CDECL;
   PFNFCIGETTEMPFILE = TFNFCIGETTEMPFILE;

   (*    FCICreate -- create an FCI context (an open CAB, an open FOL)
    *
    *  Entry:
    *      perf      - structure where we return error codes
    *      pfnfcifp  - callback to inform caller of eventual dest of files
    *      pfna      - memory allocation function callback
    *      pfnf      - memory free function callback
    *      pfnfcigtf - temp file name generator callback
    *      pccab     - pointer to cabinet/disk name & size structure
    *
    *  Notes:
    *  (1) The alloc/free callbacks must remain valid throughout
    *      the life of the context, up to and including the call to
    *      FCIDestroy.
    *  (2) The perf pointer is stored in the compression context (HCI),
    *      and any errors from subsequent FCI calls are stored in the
    *      erf that was passed in on *this* call.
    *
    *  Exit-Success:
    *      Returns non-NULL handle to an FCI context.
    *
    *  Exit-Failure:
    *      Returns NULL, perf filled in.
    *)

{$IFDEF DYNLOADCABDLL}
Type
   TFCICreate = Function(PERF: PERF; pfnfcifp: PFNFCIFILEPLACED;
      pfna: PFNFCIALLOC; pfnf: PFNFCIFREE; pfnopen: PFNFCIOPEN;
      pfnread: PFNFCIREAD; pfnwrite: PFNFCIWRITE; pfnclose: PFNFCICLOSE;
      pfnseek: PFNFCISEEK; pfndelete: PFNFCIDELETE;
      pfnfcigtf: PFNFCIGETTEMPFILE; PCCAB: PCCAB; pv: Pointer): HFCI; CDECL;

Var
   FCICreate: TFCICreate;

{$ELSE}
Function FCICreate(PERF: PERF; pfnfcifp: PFNFCIFILEPLACED;
   pfna: PFNFCIALLOC; pfnf: PFNFCIFREE; pfnopen: PFNFCIOPEN;
   pfnread: PFNFCIREAD; pfnwrite: PFNFCIWRITE; pfnclose: PFNFCICLOSE;
   pfnseek: PFNFCISEEK; pfndelete: PFNFCIDELETE;
   pfnfcigtf: PFNFCIGETTEMPFILE; PCCAB: PCCAB; pv: Pointer): HFCI; CDECL;
{$ENDIF}

(*   FCIAddFile - Add a disk file to a folder/cabinet
 *
 *  Entry:
 *      hfci          - FCI context handle
 *      pszSourceFile - Name of file to add to folder
 *      pszFileName   - Name to store into folder/cabinet
 *      fExecute      - Flag indicating execute on extract
 *      pfn_progress  - Progress callback
 *      pfnfcignc     - GetNextCabinet callback
 *      pfnfcis       - Status callback
 *      pfnfcigoi     - OpenInfo callback
 *      typeCompress  - Type of compression to use for this file
 *      pv            - pointer to caller's internal context
 *
 *  Exit-Success:
 *      returns True
 *
 *  Exit-Failure:
 *      returns False, error filled in
 *
 *    This is the main function used to add file(s) to a cabinet
 *    or series of cabinets.  If the current file causes the current
 *    folder/cabinet to overflow the disk image currently being built,
 *    the cabinet will be terminated, and a new cabinet/disk name will
 *    be prompted for via a callback.  The pending folder will be trimmed
 *    of the data which has already been generated in the finished cabinet.
 *)

{$IFDEF DYNLOADCABDLL}
Type
   TFCIAddFile = Function(
      HFCI: HFCI;
      pszSourceFile: PChar;
      pszFileName: PChar;
      fExecute: Boolean;
      //pfn_progress: TFCIProgress;
      pfnfcignc: PFNFCIGETNEXTCABINET;
      pfnfcis: PFNFCISTATUS;
      pfnfcigoi: PFNFCIGETOPENINFO;
      typeCompress: TCOMP
      //pv : POINTER
      ): Boolean; CDECL;

Var
   FCIAddFile: TFCIAddFile;

{$ELSE}
Function FCIAddFile(HFCI: HFCI; pszSourceFile: PChar; pszFileName: PChar;
   fExecute: Boolean; {pfn_progress: TFCIProgress; } pfnfcignc: PFNFCIGETNEXTCABINET;
   pfnfcis: PFNFCISTATUS; pfnfcigoi: PFNFCIGETOPENINFO;
   typeCompress: TCOMP {; pv : POINTER}): Boolean; CDECL;
{$ENDIF}

(*   FCIFlushCabinet - Complete the current cabinet under construction
 *
 *  This will cause the current cabinet (assuming it is not empty) to
 *  be gathered together and written to disk.
 *
 *  Entry:
 *      hfci        - FCI context
 *      fGetNextCab - True  => Call GetNextCab to get continuation info;
 *                    False => Don't call GetNextCab unless this cabinet
 *                             overflows.
 *      pfnfcignc   - callback function to get continuation cabinets
 *      pfnfcis     - callback function for progress reporting
 *      pv          - caller's internal context for callbacks
 *
 *  Exit-Success:
 *      return code True
 *
 *  Exit-Failure:
 *      return code False, error structure filled in
 *)

{$IFDEF DYNLOADCABDLL}
Type
   TFCIFlushCabinet = Function(HFCI: HFCI; fGetNextCab: Boolean;
      pfnfcignc: PFNFCIGETNEXTCABINET; pfnfcis: PFNFCISTATUS): Boolean; CDECL;
Var
   FCIFlushCabinet: TFCIFlushCabinet;

{$ELSE}

Function FCIFlushCabinet(HFCI: HFCI; fGetNextCab: Boolean;
   pfnfcignc: PFNFCIGETNEXTCABINET; pfnfcis: PFNFCISTATUS): Boolean; CDECL;

{$ENDIF}

(*   FCIFlushFolder - Complete the current folder under construction
 *
 *  This will force the termination of the current folder, which may or
 *  may not cause one or more cabinet files to be completed.
 *
 *  Entry:
 *      hfci        - FCI context
 *      GetNextCab  - callback function to get continuation cabinets
 *      pfnProgress - callback function for progress reporting
 *      pv          - caller's internal context for callbacks
 *
 *  Exit-Success:
 *      return code True
 *
 *  Exit-Failure:
 *      return code False, error structure filled in
 *)

{$IFDEF DYNLOADCABDLL}
Type
   TFCIFlushFolder = Function(HFCI: HFCI; pfnfcignc: PFNFCIGETNEXTCABINET;
      pfnfcis: PFNFCISTATUS): Boolean; CDECL;
Var
   FCIFlushFolder: TFCIFlushFolder;
{$ELSE}
Function FCIFlushFolder(HFCI: HFCI; pfnfcignc: PFNFCIGETNEXTCABINET;
   pfnfcis: PFNFCISTATUS): Boolean; CDECL;
{$ENDIF}

(*    FCIDestroy - Destroy a FCI context and delete temp files
 *
 *  Entry:
 *      hfci - FCI context
 *
 *  Exit-Success:
 *      return code True
 *
 *  Exit-Failure:
 *      return code False, error structure filled in
 *)

{$IFDEF DYNLOADCABDLL}
Type
   TFCIDestroy = Function(HFCI: HFCI): Boolean; CDECL;
Var
   FCIDestroy: TFCIDestroy;
{$ELSE}
Function FCIDestroy(HFCI: HFCI): Boolean; CDECL;
{$ENDIF}

Type
   (* Options for adding files *)
   TAddFileOption = (afoExecuteOnExtract);
   TAddFileOptions = Set Of TAddFileOption;

   (* Compression type *)
   TCompressionType = (ctStore, ctMsZip, ctLzx);

{$IFDEF CABDEBUG}

   TCompressionType = (ctNone, ctMsZip, ctQuantum, ctLzx);

   (* Parameters for compression *)
   TCompressionParameters = Packed Record
      Compression: TCompressionType;
      LzxLevel: 15..21;
      QuantumLevel: 1..7;
      QuantumMemory: 10..21;
   End;
{$ENDIF}

   (*********************************************************************)
   (* The following definations weren't required.  They have been rem'd	*)
   (* instead of deleted, in case developers find any useful.           *)
   (*********************************************************************)
   {TFileStatusEvent = PROCEDURE (Sender : TObject; CompressedSize,
     UncompressedSize : LONGINT; VAR ResultCode : Integer) OF OBJECT;
   TFolderStatusEvent = PROCEDURE (Sender : TObject; SizeCopied,
     TotalSize : LONGINT; VAR ResultCode : Integer) OF OBJECT;
   TCabinetStatusEvent = PROCEDURE (Sender : TObject; PreEstimatedSize,
     ActualSize : LONGINT; VAR WantedSize : Integer(*LONGINT*)) OF OBJECT;
   TFilePlacedEvent = PROCEDURE (Sender : TObject; VAR CabParameters : TCCAB;
     CONST FileName : STRING; FileLength : LONGINT; Continuation : BOOLEAN;
     VAR AbortProcessing : BOOLEAN) OF OBJECT;
   TGetTempFileEvent = PROCEDURE (Sender : TObject; VAR TempFileName : STRING;
     VAR Success : BOOLEAN) OF OBJECT;
   TGetOpenInfoEvent = PROCEDURE (Sender : TObject; CONST FileName : STRING;
     VAR Date, Time, Attributes : SMALLINT; VAR FileHandle,
     ResultCode : Integer(*LONGINT*)) OF OBJECT;}
   (*********************************************************************)
   TGetNextCabinetEvent = Procedure(Sender: TObject;
      Var CabParameters: TCCAB; Var NewCabFileName: String;
      PreviousCabEstimatedSize: longint; Var AbortCreation: Boolean) Of Object;

   TCabInterface = Class(TCompBase)
   Protected
      FErrorBuffer: TERF;
      FFileHandler: TCabinetFileHandler;
      Procedure CabCheck(Result: Boolean);
   Public
      Property FileHandler: TCabinetFileHandler Read FFileHandler Write FFileHandler;
   End;

   TMakeCab = Class(TCabInterface)
   Protected
      CabUncompressedSize: DWord;       // 092901
      NewArchive: Boolean;
      fContext: HFCI;
      fLzxLevel: Integer;               (* 15..21 	*)
      fQuantumType: TCompressionType;
      fQuantumLevel: Integer;           (* 1..7 		*)
      fQuantumMemory: Integer;          (* 10.21 	*)
      fFailOnIncompressible: Boolean;
      FOnGetNextCabinet: TGetNextCabinetEvent;
      (*********************************************************************)
      (* The following variables/procedures deal with the above definations*)
      (* that were rem'd... some developers may find them useful				*)
      (*********************************************************************)
      {fFileSupport 				: BOOLEAN;
      fOnGetOpenInfo 			: TGetOpenInfoEvent;
      fOnFileStatus 				: TFileStatusEvent;
      fOnFolderStatus 			: TFolderStatusEvent;
      fOnCabinetStatus 			: TCabinetStatusEvent;
      fOnFilePlacedEvent 		: TFilePlacedEvent;
      fOnGetTempFile 			: TGetTempFileEvent;
      Procedure doFileStatus(CompressedSize, UncompressedSize : LONGINT;
        VAR ResultCode : Integer); VIRTUAL;
      Procedure doFolderStatus(SizeCopied, TotalSize : LONGINT;
        VAR ResultCode : Integer); VIRTUAL;
      Procedure doCabinetStatus(PreEstimatedSize, ActualSize : LONGINT;
        VAR WantedSize : Integer); VIRTUAL;
      Procedure doFilePlaced(VAR CabParameters : TCCAB;
        CONST FileName : STRING; FileLength : LONGINT; Continuation : BOOLEAN;
        VAR AbortProcessing : BOOLEAN); VIRTUAL;}
      (*********************************************************************)
      Procedure doGetNextCabinet(Var CabParameters: TCCAB;
         Var NewCabFileName: String; PreviousCabEstimatedSize: longint;
         Var AbortCreation: Boolean); Virtual;
      Procedure doGetTempFile(Var TempFilename: String;
         Var Success: Boolean); Virtual;
      Procedure doGetOpenInfo(Const FN: String; Var Date, Time,
         Attributes: smallint; Var FileHandle, ResultCode: Integer {LONGINT};
         pv: Pointer); Virtual;
      Procedure doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
         pHeaderObj: pCompHeaderObj); Override;
      Function GetHeadPtr: Pointer; Override;
      Function GetHeadSize: word; Override;
      Function GetLocalHeaderSize: Integer; Override;
      Procedure SetLzxLevel(i: Integer);
      Procedure SetQuantumLevel(i: Integer);
      Procedure SetQuantumMemory(i: Integer);
      Procedure SetArcType(SAT: TArcType); Override;
      //Function Write64BitFieldHdr(s: TStream32; CFI: pCompFileInfo;
      //	HType: THeaderType): Integer; Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure AddFile(SourceFileName, DestFileName: String;
         Const Options: TAddFileOptions
{$IFDEF CABDEBUG}; Const Compression: TCompressionParameters{$ENDIF});
      Procedure Close;
      Procedure CompressIT(pHeaderObj: pCompHeaderObj); Override;
      Function doCleanUp(Outfile: TStream32; pHeaderObj: pCompHeaderObj):
         Boolean; Override;
      Procedure doFinish(pHeaderObj: pCompHeaderObj); Override;
      Procedure FlushCabinet(GetNextCabinet: Boolean);
      Procedure FlushFolder;
      Procedure Open(Const CabinetFileName, DiskName: String;
         MaximumCabSize, FolderThreshold, setID: longint);
      Procedure PackFile(Outfile: TStream32; Index: Integer;
         pHeaderObj: pCompHeaderObj; Var Result: Boolean); Override; //ABSTRACT;
      Function GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo; Override;
      Procedure SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo); Override;
      Property Context: HFCI Read fContext;
   Published
      Property ArcType;
      Property CompressMethod;
      Property CompressionMethod;
      Property DefaultExt;
      Property FailOnIncompressible: Boolean Read fFailOnIncompressible Write fFailOnIncompressible Default True;
      Property FileSpec;
      Property IncludeHiddenDirs;
      Property LzxLevel: Integer Read fLzxLevel Write SetLzxLevel Default 21;
      Property OnActivate;
      Property OnBegin;
      Property OnDeactivate;
      Property OnEnd;
      Property OnError;
      Property OnGetNextCabinet: TGetNextCabinetEvent Read FOnGetNextCabinet Write FOnGetNextCabinet;
      Property OnNonWriteableArchive;
      Property OnProgress;
      Property OnRead;
      Property OnReplaceFile;
      Property OnRenameFile;
      Property QuantumType: TCompressionType Read fQuantumType Write fQuantumType Default ctMsZip;
      Property QuantumLevel: Integer Read fQuantumLevel Write SetQuantumLevel Default 7;
      Property QuantumMemory: Integer Read fQuantumMemory Write SetQuantumMemory Default 21;
      Property RecurseDirs;
      Property StoredDirNames;
      Property TempDir;
      (*********************************************************************)
      (* The following properties deal with the above definations that were*)
      (* rem'd... some developers may find them useful							*)
      (*********************************************************************)
      {PROPERTY FileSupport: BOOLEAN READ fFileSupport WRITE fFileSupport DEFAULT True;
      PROPERTY OnGetOpenInfo : TGetOpenInfoEvent READ fOnGetOpenInfo WRITE fOnGetOpenInfo;
      PROPERTY OnFileStatus : TFileStatusEvent READ fOnFileStatus WRITE fOnFileStatus;
      PROPERTY OnFolderStatus : TFolderStatusEvent READ fOnFolderStatus WRITE fOnFolderStatus;
      PROPERTY OnCabinetStatus : TCabinetStatusEvent READ fOnCabinetStatus WRITE fOnCabinetStatus;
      PROPERTY OnFilePlacedEvent : TFilePlacedEvent READ fOnFilePlacedEvent WRITE fOnFilePlacedEvent;
      PROPERTY OnGetTempFile : TGetTempFileEvent READ fOnGetTempFile WRITE fOnGetTempFile;}
   End;

   (* Method declarations *)
{$IFDEF CABDEBUG}
Function MakeNoCompression: TCompressionParameters;
Function MakeMsZipCompression: TCompressionParameters;
Function MakeLzxCompression(level: Integer): TCompressionParameters;
{$ENDIF}

Implementation

{$IFDEF DYNLOADCABDLL}
Uses
   ztvLoadLib;
{$ENDIF}

Const
   CabinetDll = 'cabinet.dll';

{$IFNDEF DYNLOADCABDLL}
Function FCICreate; External CabinetDll name 'FCICreate';
Function FCIAddFile; External CabinetDll name 'FCIAddFile';
Function FCIFlushCabinet; External CabinetDll name 'FCIFlushCabinet';
Function FCIFlushFolder; External CabinetDll name 'FCIFlushFolder';
Function FCIDestroy; External CabinetDll name 'FCIDestroy';
{$ENDIF}

{$IFDEF DYNLOADCABDLL}
//Var
//   IsCABDLLAvailable: BOOLEAN; //v4.0 (* True if CABINET.DLL available. *);
//   CABDLLHandle     : THandle; //v4.0 (* Handle for CABINET.DLL *);
Const
   IsCABDLLAvailable: Boolean = False (* True if CABINET.DLL available. *);
   CABDLLHandle: THandle = 0 (* Handle for CABINET.DLL *);
{$ELSE}
//Var
//   IsCABDLLAvailable: BOOLEAN; //v4.0 (* CABINET.DLL assumed available. *);
Const
   IsCABDLLAvailable: Boolean = True (* CABINET.DLL assumed available. *);
{$ENDIF}

   //-------------------------------------------------------------

{Procedure CheckErf(Erf: TERF);
Begin
   If BOOL(Erf.fError) Then
   	RaiseCabinetError(Erf.erfOper, Erf.erfType);
End;}
//-------------------------------------------------------------
(* TCabInterface *)
//-------------------------------------------------------------

Procedure TCabInterface.CabCheck(Result: Boolean);
Begin
   //If Not Result Then
   //	CheckErf(FErrorBuffer);
End;
//-------------------------------------------------------------
(* Standard FCI procs for TMakeCAB *)
//-------------------------------------------------------------

Function StdFciOpen(pszFile: PChar; oflag: Integer; pmode: Integer;
   err: PInteger; pv: Pointer): Integer; Cdecl;
Begin
   err^ := 0;
   Result := TCabInterface(pv).FileHandler.Open(String(pszFile), oflag, pmode,
      err^, ftCabinet);
   //If String(pszFile) = TCabInterface(pv).ArchiveFile Then
   //	ShowMessage('Here');
End;
//-------------------------------------------------------------

Function StdFciRead(hf: Integer; memory: PVoid; cb: TUINT;
   err: PInteger; pv: Pointer): TUINT; Cdecl;
Begin
   err^ := 0;
   With TMakeCab(pv) Do
      If Cancel Then
      Begin
         err^ := -1;
         Result := 0;
         Bytes_To_Go := 0;
      End Else Begin
         Result := TCabInterface(pv).FileHandler.Read(hf, memory^, cb, err^);
            If (Result > 0) And (Bytes_To_Go > 0) Then
            Begin
               Try
                  Dec(Bytes_To_Go, Result);
                  ProgressPosition := ProgressPosition - Result;
                  //If Bytes_To_Go < 0 Then Bytes_To_Go := 0;
                  If ProgressPosition < 0 Then ProgressPosition := 0;
                  doBranchProgress(CabUncompressedSize - Bytes_To_Go,
                     CabUncompressedSize, fTotalUnpackedSize);
               Except
               End;
            End;
      End;
End;
//-------------------------------------------------------------

Function StdFciWrite(hf: Integer; memory: PVoid; cb: TUINT;
   err: PInteger; pv: Pointer): TUINT; Cdecl;
Begin
   With TMakeCab(pv) Do
      If Cancel Then
      Begin
         err^ := -1;
         Result := 0;
         Bytes_To_Go := 0;
      End Else Begin
         err^ := 0;
         Result := TCabInterface(pv).FileHandler.Write(hf, memory^, cb, err^);
   		If err^ = ERROR_DISK_FULL Then
         Begin
            // if disk-full, the temp file could not be flushed
            Count := 0;
            FilesDeleted := 0;
         	FilesCompressed := 0;
            RaiseErrorStr(ArchiveFile, '', '0', E_DISKFULL);
         End;
      End;
End;
//-------------------------------------------------------------

Function StdFciClose(hf: Integer; err: PInteger; pv: Pointer):
   Integer; Cdecl;
Begin
   err^ := 0;
   Result := TCabInterface(pv).FileHandler.Close(hf, err^);
End;
//-------------------------------------------------------------

Function StdFciSeek(hf: Integer; dist: longint; SeekType: Integer;
   err: PInteger; pv: Pointer): longint; Cdecl;
Begin
   With TMakeCab(pv) Do
      If Cancel Then
      Begin
         err^ := -1;
         Result := 0;
         Bytes_To_Go := 0;
      End Else Begin
         err^ := 0;
   		Result := TCabInterface(pv).FileHandler.Seek(hf, dist, SeekType, err^);
      End;
End;
//-------------------------------------------------------------

Function StdFciDelete(pszFile: PChar; err: PInteger; pv: Pointer):
   Integer; Cdecl;
Begin
   err^ := 0;
   Result := TMakeCab(pv).FileHandler.Delete(String(pszFile), err^);
End;
//-------------------------------------------------------------

Function StdFciGetTempFile(pszTempName: PChar; cbTempName: Integer;
   pv: Pointer): Boolean; Cdecl;
Var
   Buffer: String;
   Success: Boolean;
Begin
   Buffer := '';
   TMakeCab(pv).doGetTempFile(Buffer, Success);
   If Success And (Length(Buffer) < cbTempName) Then
   Begin
      StrLCopy(pszTempName, PChar(Buffer), cbTempName - 1);
      Result := True;
   End
   Else
      Result := False;
End;
//-------------------------------------------------------------

Function StdFciFilePlaced(PCCAB: PCCAB; pszFile: PChar;
   cbFile: longint; fContinuation: Boolean; pv: Pointer): Integer; Cdecl;
Var
   Abort: Boolean;
Begin
   Abort := False;

   (* Rem'd *)
   //TMakeCAB(pv).doFilePlaced(pccab^, STRING(pszFile), cbFile,
   //  BOOLEAN(fContinuation), Abort);

   If Abort Then
      Result := -1
   Else
      Result := 0;
End;
//-------------------------------------------------------------

Function StdFciGetOpenInfo(pszName: PChar; Var pdate: TUUSHORT;
   Var ptime: TUUSHORT; Var pattribs: TUUSHORT; err: PInteger;
   pv: Pointer): Integer; Cdecl;
Var
   FileHandle: Integer;
Begin

   FileHandle := 0;
   TMakeCab(pv).doGetOpenInfo(String(pszName), pdate, ptime, pattribs,
      FileHandle, err^, pv);

   If err^ <> 0 Then
      Result := -1
   Else
      Result := FileHandle;
End;
//-------------------------------------------------------------

Function StdFciGetNextCabinet(PCCAB: PCCAB; cbPrevCab: TULONG;
   pv: Pointer): Boolean; Cdecl;
Var
   Abort: Boolean;
   NewFilename: String;
Begin
   Abort := True;
   NewFilename := '';
   TMakeCab(pv).doGetNextCabinet(PCCAB^, NewFilename, cbPrevCab, Abort);

   If Abort Then
   Begin
      PCCAB^.szCab := #0;
      PCCAB^.szCabPath := #0;
      Result := False;
      Exit;
   End;

   StrCopy(PCCAB^.szCab, PChar(ExtractFilename(NewFilename)));
   StrCopy(PCCAB^.szCabPath, PChar(ExtractFilePath(NewFilename)));
   Result := True;
End;
//-------------------------------------------------------------
(* cb1 = CompressedSize     	*)
(* cb2 = UncompressedSize 	*)

Function StdFciStatus(typeStatus: TUINT; cb1: TULONG; cb2: TULONG;
   pv: Pointer): longint; Cdecl;
Var
   rc: Integer;
Begin
   rc := 0;

   (* Rem'd *)
   Case typeStatus Of
   	statusFile : ; //TMakeCAB(pv).doFileStatus(cb1, cb2, rc);
     	statusFolder :	; //TMakeCAB(pv).doFolderStatus(cb1, cb2, rc);
     	statusCabinet : ; //TMakeCAB(pv).doCabinetStatus(cb1, cb2, rc);
   Else
     rc := -1;
   End;
   Result := rc;
End;
//-------------------------------------------------------------
(* Cabinet procs *)
Type
   TCabinetProcRec = Packed Record
      (* FCI/FDI *)
      FciAlloc: PFNFCIALLOC;
      FciFree: PFNFCIFREE;
      (* FCI *)
      FciOpen: PFNFCIOPEN;
      FciRead: PFNFCIREAD;
      FciWrite: PFNFCIWRITE;
      FciClose: PFNFCICLOSE;
      FciSeek: PFNFCISEEK;
      FciDelete: PFNFCIDELETE;
      FciGetTempFile: PFNFCIGETTEMPFILE;
      FciFilePlaced: PFNFCIFILEPLACED;
      FciGetNextCabinet: PFNFCIGETNEXTCABINET;
      FciStatus: PFNFCISTATUS;
      FciGetOpenInfo: PFNFCIGETOPENINFO;
   End;

Const
   StdCabinetProcs: TCabinetProcRec =
   (FciAlloc: StdFciAlloc;
      FciFree: StdFciFree;
      FciOpen: StdFciOpen;
      FciRead: StdFciRead;
      FciWrite: StdFciWrite;
      FciClose: StdFciClose;
      FciSeek: StdFciSeek;
      FciDelete: StdFciDelete;
      FciGetTempFile: StdFciGetTempFile;
      FciFilePlaced: StdFciFilePlaced;
      FciGetNextCabinet: StdFciGetNextCabinet;
      FciStatus: StdFciStatus;
      FciGetOpenInfo: StdFciGetOpenInfo);
   //-------------------------------------------------------------
   (* TMakeCAB *)
   //-------------------------------------------------------------
   (* Range 15..21 *)

//-------------------------------------------------------------

Function TMakeCab.GetHeadPtr: Pointer;
Begin
   Result := @CFHeader;                 // Not used?
End;
//------------------------------------------------------------

Function TMakeCab.GetHeadSize: word;
Begin
   Result := SizeOf(CFHeader);          //SizeOf( CFFILE );  // Not used?
End;
//------------------------------------------------------------

Function TMakeCab.GetLocalHeaderSize: Integer;
Begin
   Result := SizeOf(CFHeader);          //SizeOf( CFFILE );  // Not used?
End;
//-------------------------------------------------------------

Procedure TMakeCab.SetLzxLevel(i: Integer);
Begin
   If i < 15 Then
      fLzxLevel := 15
   Else
      If i > 21 Then
         fLzxLevel := 21
      Else
         fLzxLevel := i;
End;
//-------------------------------------------------------------

Procedure TMakeCab.doPCopy(ArcFile, TempFile: TStream32; Index: Integer;
   pHeaderObj: pCompHeaderObj);
Begin
   //
End;
//-------------------------------------------------------------

Procedure TMakeCab.SetArcType(SAT: TArcType);
Begin
   fArcType := atCab;
   CompressArcType();
End;
//-------------------------------------------------------------

//Function TMakeCab.Write64BitFieldHdr(s: TStream32; CFI: pCompFileInfo;
//	HType: THeaderType): Integer;
//Begin
//	Result := 0;  // virtual method... do not delete.
//End;
//-------------------------------------------------------------

(* Range 1..7 *)

Procedure TMakeCab.SetQuantumLevel(i: Integer);
Begin
   If i < 1 Then
      fQuantumLevel := 1
   Else
      If i > 7 Then
         fQuantumLevel := 7
      Else
         fQuantumLevel := i;
End;
//-------------------------------------------------------------
(* Range 10..21 *)

Procedure TMakeCab.SetQuantumMemory(i: Integer);
Begin
   If i < 10 Then
      fQuantumMemory := 10
   Else
      If i > 21 Then
         fQuantumMemory := 21
      Else
         fQuantumMemory := i;
End;
//-------------------------------------------------------------
(* Rem'd *)
{Procedure TMakeCAB.doFileStatus (CompressedSize,
  UncompressedSize : LONGINT; VAR ResultCode : Integer);
BEGIN
  IF Assigned(fOnFileStatus) THEN
    fOnFileStatus(Self, CompressedSize, UncompressedSize, ResultCode);
END;}
//-------------------------------------------------------------
(* Rem'd *)
{Procedure TMakeCAB.doFolderStatus (SizeCopied, TotalSize : LONGINT;
  VAR ResultCode : Integer);
BEGIN
  IF Assigned(fOnFolderStatus) THEN
    fOnFolderStatus(Self, SizeCopied, TotalSize, ResultCode);
END;}
//-------------------------------------------------------------
(* Rem'd *)
{Procedure TMakeCAB.doCabinetStatus (PreEstimatedSize,
  ActualSize : LONGINT; VAR WantedSize : Integer);
BEGIN
  IF Assigned(fOnCabinetStatus) THEN
    fOnCabinetStatus(Self, PreEstimatedSize, ActualSize, WantedSize);
END;}
//-------------------------------------------------------------

Procedure TMakeCab.doGetNextCabinet(Var CabParameters: TCCAB;
   Var NewCabFileName: String; PreviousCabEstimatedSize: longint;
   Var AbortCreation: Boolean);
Begin
   If Assigned(FOnGetNextCabinet) Then
      FOnGetNextCabinet(Self, CabParameters, NewCabFileName,
         PreviousCabEstimatedSize, AbortCreation);
End;
//-------------------------------------------------------------
(* Rem'd *)
{Procedure TMakeCAB.doFilePlaced (VAR CabParameters : TCCAB;
  CONST FileName : STRING; FileLength : LONGINT; Continuation : BOOLEAN;
  VAR AbortProcessing : BOOLEAN);
BEGIN
  IF Assigned(fOnFilePlacedEvent) THEN
    fOnFilePlacedEvent(Self, CabParameters, FileName, FileLength, Continuation,
      AbortProcessing);
END;}
//-------------------------------------------------------------

Procedure TMakeCab.doGetTempFile(Var TempFilename: String;
   Var Success: Boolean);
Begin
   (* Rem'd *)
    //IF Assigned(fOnGetTempFile) THEN
    //  fOnGetTempFile(Self, TempFileName, Success)
    //ELSE BEGIN
       //TempFileName := GetTempFileNameStr(GetTempPathStr, Copy(ExtractFileName(ParamStr(0)), 1, 3), 0);
   TempFilename := GetTempFilenameStr(TempDir);
   Success := True;
   //END
End;
//-------------------------------------------------------------

Procedure TMakeCab.doGetOpenInfo(Const FN: String; Var Date,
   Time, Attributes: smallint; Var FileHandle, ResultCode: Integer; pv: Pointer);
Var
   FileTime: TFileTime;
   FT: Integer;
Begin

   (* Rem'd *)
   //IF Assigned(fOnGetOpenInfo) THEN
    //  fOnGetOpenInfo(Self, FN, Date, Time, Attributes, FileHandle, ResultCode)
    //ELSE BEGIN

    (* Rem'd *)
     //IF fFileSupport THEN
       //BEGIN
   Attributes := {Word(} GetFileAttributes(PChar(FN));
   If Attributes = {Word(} -1 Then
   Begin
      ResultCode := GetlastError();
      Exit;
   End;
   //END ELSE
   //  Attributes := 0;

   FileHandle := FFileHandler.Open(FN, _O_RDONLY Or _O_BINARY, 0,
      ResultCode, ftSource);

   If ResultCode = 0 Then
   Begin
      Date := 0;
      Time := 0;

      (* Rem'd *)
      If {fFileSupport AND }  GetFileTime(FileHandle, Nil, Nil, @FileTime) Then
      Begin
        (* following line added by: M. Suzuki.   Convert file time based on
         the Coordinated Universal Time (UTC) to a local file time.  *)
         FileTimeToLocalFileTime(FileTime, FileTime);

         (* Convert time to dos compatible date / time *)
         FileTimeToDosDateTime(FileTime, word(Date), word(Time));
         FT := GetDateTime(Makelong(word(Time), word(Date)));
         Date := HiWord(FT);
         Time := LoWord(FT);
      End;

      //CabUncompressedSize := GetFileSize( FileHandle, NIL );
      If Assigned(OnRead) Then
      Begin
         //FUnpackedSize := Bytes_To_Go;
         //FPackedSize := Bytes_To_Go;
         GlobalDate := ztvConvertDate(Makelong(word(Time), word(Date)));
         fExternalAttr := Attributes;
         fCRC := 0;
         fRatio := 0;
         fEncrypted := False;
         fVolumeName := fArchiveFile;
      End;
   End Else
   	RaiseErrorStr(FN, '', '0', E_FOPEN);

   //END;
End;
//-------------------------------------------------------------

Constructor TMakeCab.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);

   fMasterExt := '.CAB';
   DefaultExt := fMasterExt;

   (* Rem'd *)
   //fFileSupport := True;
   fFailOnIncompressible := True;
   FFileHandler := TStreamCabinetFileHandler.Create(Nil);

   CompressMethodState := [cmStore, cmMsZip, cmLzx, cmQuantum];
   CompressMethod := cmMsZip;
   fArcType := atCab;

   fLzxLevel := 21;
   fQuantumLevel := 7;
   fQuantumMemory := 21;
   fQuantumType := ctMsZip;
End;
//-------------------------------------------------------------

Destructor TMakeCab.Destroy;
Begin
   Close();
   FFileHandler.Free();
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TMakeCab.Open(Const CabinetFileName, DiskName: String;
   MaximumCabSize, FolderThreshold, setID: longint);
Var
   Parameters: TCCAB;
Begin
   Close();
   FillChar(Parameters, SizeOf(Parameters), 0);
   Parameters.cb := MaximumCabSize;
   Parameters.cbFolderThresh := FolderThreshold;
   Parameters.fFailOnIncompressible := Integer(fFailOnIncompressible);
   Parameters.setID := setID;
   StrCopy(Parameters.szDisk, PChar(DiskName));
   StrCopy(Parameters.szCab, PChar(ExtractFilename(CabinetFileName)));
   StrCopy(Parameters.szCabPath, PChar(ExtractFilePath(CabinetFileName)));
   With StdCabinetProcs Do
      fContext := FCICreate(@FErrorBuffer, FciFilePlaced, FciAlloc,
         FciFree, FciOpen, FciRead, FciWrite, FciClose,
         FciSeek, FciDelete, FciGetTempFile, @Parameters,
         Pointer(Self));

   CabCheck(fContext <> Nil);
End;
//-------------------------------------------------------------

Function TMakeCab.GetHeaderInfo(p: Pointer; pCBFI: pCompFileInfo): THeaderInfo;
Begin
   Result.pSize := TCAB_FILE_HEADER(p^).reserved1;
   Result.uSize := TCAB_FILE_HEADER(p^).reserved2;
   Result.Attr := TCAB_FILE_HEADER(p^).reserved3;
End;
//-------------------------------------------------------------

Procedure TMakeCab.SetHeaderInfo(pHeader: Pointer; HI: pHeaderInfo; CBFInew: TCompFileInfo);
Begin
   With TCAB_FILE_HEADER(pHeader^) Do
   Begin
      reserved1 := HI^.pSize;
      reserved2 := HI^.uSize;
      reserved3 := HI^.Attr;
   End;
End;
//-------------------------------------------------------------

Procedure TMakeCab.Close;
Begin
   If fContext <> Nil Then
   Begin
      CabCheck(FCIDestroy(fContext));
      fContext := Nil;
   End;
End;
//-------------------------------------------------------------

Function TMakeCab.doCleanUp;
Begin
   Result := True;
End;
//-------------------------------------------------------------

Procedure TMakeCab.doFinish;
Begin
   ;
End;
//-------------------------------------------------------------

Procedure TMakeCab.CompressIT;
Var
{$IFDEF CABDEBUG}
   Compr: TCompressionParameters;
{$ENDIF}
   Action: Boolean;
   ArcFile, TempFile: TStream32;
Begin

   Try
      If Not IsCABDLLAvailable Then
         RaiseError(E_RAISE, fArchiveFile, '', '0', E_NOCABDLL);

      NewArchive := Not FileExists(fArchiveFile);
      If (Switch = swDelete) Then
         RaiseError(E_RAISE, fArchiveFile, '', '0', E_INVALIDARC)
      Else
         If (Switch = swRead) And (Not NewArchive) Then
         Begin
            ReadFileInfo(pHeaderObj);
            Exit;
         End;

      If Not NewArchive Then
         If Not IsWriteable(fArchiveFile) Then
         Begin

            Action := False;
            If Assigned(OnNonWriteableArchive) Then
               OnNonWriteableArchive(Self, fArchiveFile, Action);

            If Not Action Then
            Begin
               If Not Assigned(OnNonWriteableArchive) Then
                  RaiseErrorStr(fArchiveFile, '', '0', E_FWRITE);

               Exit;
            End;

            If Not SetFileAttributes(PChar(fArchiveFile), FILE_ATTRIBUTE_NORMAL) Then
            Begin
               RaiseErrorStr(fArchiveFile, '', '0', E_FWRITE);
               Exit;
            End;
         End;

      NewArchive := True;
      StoreEmptySubDirs := False;       //v4.1 added

      If Assigned(OnActivate) Then OnActivate(Self);
      If Assigned(OnElapsedTime) Then ZipTimer.START;

      Open(fArchiveFile, '', MaxInt, MaxInt{900000}, 0);
      Try
         ArcFile := Nil;
         TempFile := Nil;
         ExecuteCompression(ArcFile{Nil}, TempFile{Nil}, pHeaderObj);
      Finally
         FlushCabinet(False {True});    (* <-- parameter stores value in HasNext *)
         Close();
         If Assigned(OnElapsedTime) Then
         Begin
            ZipTimer.Stop;
            OnElapsedTime(Self, ZipTimer.ElapsedTime);
         End;
         If Assigned(OnDeactivate) Then OnDeactivate(Self);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Procedure TMakeCab.PackFile(Outfile: TStream32; Index: Integer;
   pHeaderObj: pCompHeaderObj; Var Result: Boolean);
Var
   Action: Boolean;
Begin
   Action := True;
   Result := False;

   fPackedSize := Bytes_To_Go;
   fUnpackedSize := Bytes_To_Go;

   CabUncompressedSize := Bytes_To_Go;  (* or FileInfo.Size *)
   fFileName := pHeaderObj^.FileList.Strings[Index];

   (* Could not use the global DoOnBegin Procedure		*)
                   (* because OnRenameFile must be called	*)
                   (* elsewhere in the code                        	*)
   If Assigned(OnBegin) Then
      OnBegin(Self, fFileName, Count, Action);

   FileName := fFileName;               //format filename according to setting of StoredDirNames property

   If Action Then
   Begin
      Try
         //HeadList.Strings[Index] := DoCharToOem( FN, fTransOemChar );
         AddFile(
            pHeaderObj^.FileList.Strings[Index],
            ExtractFilename(pHeaderObj^.FileList.Strings[Index]), [] {, Compr});
            
         Result := (Not Cancel);
      Except
         If Assigned(OnEnd) Then
            OnEnd(Self, pHeaderObj^.FileList.Strings[Index], False);

         Exit;
      End;

      If Assigned(OnEnd) Then
         OnEnd(Self, pHeaderObj^.FileList.Strings[Index], (Not Cancel));
   End;
End;
//-------------------------------------------------------------

Procedure TMakeCab.AddFile(SourceFileName, DestFileName: String;
   Const Options: TAddFileOptions
{$IFDEF CABDEBUG}; Const Compression: TCompressionParameters{$ENDIF});
Var
   Compress: TCOMP;
Begin
{$IFDEF CABDEBUG}
   Compress := Integer(Compression.Compression);
   Case Compression.Compression Of
      ctQuantum:
         Compress := Compress Or
            (Compression.QuantumLevel Shl tcompSHIFT_QUANTUM_LEVEL) Or
            (Compression.QuantumMemory Shl tcompSHIFT_QUANTUM_MEM);
      ctLzx:
         Compress := Compress Or (Compression.LzxLevel Shl tcompSHIFT_LZX_WINDOW);
   End;
{$ELSE}
   Case CompressMethod Of
      cmStore:
         Begin
            Compress := tcompTYPE_NONE;
            fwCompressType := 0;
            fsCompressType := 'Stored';
         End;

      cmMsZip:
         Begin
            Compress := tcompTYPE_MSZIP;
            fwCompressType := 1;
            fsCompressType := 'MsZIP';
         End;
      cmLzx:
         Begin
            Compress := TCOMPfromLZXWindow(fLzxLevel);
            fwCompressType := 2;
            fsCompressType := 'Lzx';
         End;
      cmQuantum:
         Begin
            Compress := ord(fQuantumType);
            fwCompressType := Compress;
            fsCompressType := 'Quantum';

            (* force qtLzx to equal tcompTYPE_LZX *)
            If Compress = 2 Then inc(Compress);
            Compress := TCOMPfromTypeLevelMemory(Compress, fQuantumLevel, fQuantumMemory);
         End;
   Else
      Compress := 0;
   End;
{$ENDIF}

   DestFileName := CharToOemFilter(fFileName, fTransOemChar);

// version 4: rem'd... working here
//   doRenameFile( DestFilename, pHeaderObj );  // calls OnRenameFile

   With StdCabinetProcs Do
      CabCheck(FCIAddFile(fContext, PChar(SourceFileName),
         PChar(DestFileName), afoExecuteOnExtract In Options,
         FciGetNextCabinet, FciStatus, FciGetOpenInfo, Compress));

   If (Not Cancel) And Assigned(OnRead) Then
      OnRead(Self, 0, 0);

End;
//-------------------------------------------------------------

Procedure TMakeCab.FlushCabinet(GetNextCabinet: Boolean);
Begin
   With StdCabinetProcs Do
      CabCheck(FCIFlushCabinet(fContext, GetNextCabinet, FciGetNextCabinet, FciStatus));
End;
//-------------------------------------------------------------

Procedure TMakeCab.FlushFolder;
Begin
   With StdCabinetProcs Do
      CabCheck(FCIFlushFolder(fContext, FciGetNextCabinet, FciStatus));
End;
//-------------------------------------------------------------


//-------------------------------------------------------------
(* Functions *)
//-------------------------------------------------------------
{$IFDEF CABDEBUG}

Function MakeNoCompression: TCompressionParameters;
Begin
   Result.Compression := ctNone;
End;
{$ENDIF}
//-------------------------------------------------------------
{$IFDEF CABDEBUG}

Function MakeMsZipCompression: TCompressionParameters;
Begin
   Result.Compression := ctMsZip;
End;
{$ENDIF}
//-------------------------------------------------------------
{$IFDEF CABDEBUG}

Function MakeLzxCompression(level: Integer): TCompressionParameters;
Begin
   Result.Compression := ctLzx;
   Result.LzxLevel := level;
End;
{$ENDIF}
//-------------------------------------------------------------

//-------------------------------------------------------------
{$IFDEF DYNLOADCABDLL}

Function DummyFCICreate(PERF: PERF; pfnfcifp: PFNFCIFILEPLACED;
   pfna: PFNFCIALLOC; pfnf: PFNFCIFREE; pfnopen: PFNFCIOPEN;
   pfnread: PFNFCIREAD; pfnwrite: PFNFCIWRITE; pfnclose: PFNFCICLOSE;
   pfnseek: PFNFCISEEK; pfndelete: PFNFCIDELETE;
   pfnfcigtf: PFNFCIGETTEMPFILE; PCCAB: PCCAB; pv: Pointer): HFCI; Cdecl;
Begin
   Result := Nil;
End;
//-------------------------------------------------------------

Function DummyFCIAddFile(HFCI: HFCI; pszSourceFile: PChar; pszFileName: PChar;
   fExecute: Boolean; pfnfcignc: PFNFCIGETNEXTCABINET; pfnfcis: PFNFCISTATUS;
   pfnfcigoi: PFNFCIGETOPENINFO; typeCompress: TCOMP): Boolean; Cdecl;
Begin
   Result := True;
End;
//-------------------------------------------------------------

Function DummyFCIFlushCabinet(HFCI: HFCI; fGetNextCab: Boolean;
   pfnfcignc: PFNFCIGETNEXTCABINET; pfnfcis: PFNFCISTATUS): Boolean; Cdecl;
Begin
   Result := True;
End;
//-------------------------------------------------------------

Function DummyFCIFlushFolder(HFCI: HFCI; pfnfcignc: PFNFCIGETNEXTCABINET;
   pfnfcis: PFNFCISTATUS): Boolean; Cdecl;
Begin
   Result := True;
End;
//-------------------------------------------------------------

Function DummyFCIDestroy(HFCI: HFCI): Boolean; Cdecl;
Begin
   Result := True;
End;
//-------------------------------------------------------------
Initialization

   (* Try loading cabinet.dll . *)
   CABDLLHandle := MyLoadLibrary(PChar(CabinetDll + #0));

   (* If loaded, extract entry points *)
   (* used for decompression.         *)
   If (CABDLLHandle <> 0) Then
   Begin
      FCICreate := GetProcAddress(CABDLLHandle, 'FCICreate');
      FCIAddFile := GetProcAddress(CABDLLHandle, 'FCIAddFile');
      FCIFlushCabinet := GetProcAddress(CABDLLHandle, 'FCIFlushCabinet');
      FCIFlushFolder := GetProcAddress(CABDLLHandle, 'FCIFlushFolder');
      FCIDestroy := GetProcAddress(CABDLLHandle, 'FCIDestroy');

      IsCABDLLAvailable := True;
   End
   Else
   Begin
      FCICreate := @DummyFCICreate;
      FCIAddFile := @DummyFCIAddFile;
      FCIFlushCabinet := @DummyFCIFlushCabinet;
      FCIFlushFolder := @DummyFCIFlushFolder;
      FCIDestroy := @DummyFCIDestroy;

      IsCABDLLAvailable := False;
   End;

Finalization
   (* If cabinet.dll was loaded, unload it. *)

   Try
      If (CABDLLHandle <> 0) Then
         FreeLibrary(CABDLLHandle);
   Except;
   End;

   CABDLLHandle := 0;
   IsCABDLLAvailable := False;
{$ENDIF}

End.
