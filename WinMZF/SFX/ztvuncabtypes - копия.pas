Unit ztvUnCabTypes;

Interface

Uses
  Windows, ztvCabGbls;

Type
  TFDIERROR = (
    (* Description: No error   															*)
    (* Cause:       Function was successfull.					*)
    (* Response:    Keep going!              					*)
    FDIERROR_NONE,
    (* Description: Cabinet not found													*)
    (* Cause:       Bad file name or path passed to      			*)
    (* 				  FDICopy(), or returned to            		*)
    (* 				  fdintNEXT_CABINET.                   		*)
    (* Response:    To prevent this error, validate the     			*)
    (* 				  existence of the the cabinet 'before'		*)
    (* 				  passing the path to FDI.											*)
    FDIERROR_CABINET_NOT_FOUND,
    (* Description: Cabinet file does not have the correct format		*)
    (* Cause:       File passed to to FDICopy(), or returned to   	   	*)
    (*              fdintNEXT_CABINET, is too small to be a cabinet 	   	*)
    (*				  file, or does not have the cabinet signature in *)
    (* 				  its first four bytes.                         *)
    (* Response:    To prevent this error, call FDIIsCabinet() to		*)
    (* 				  check a cabinet before calling FDICopy() or   *)
    (* 				  returning the cabinet path to fdintNEXT_CABINET.*)
    FDIERROR_NOT_A_CABINET,
    (* Description: Cabinet file has an unknown version number.		 	*)
    (* Cause:       File passed to to FDICopy(), or returned to      		*)
    (*              fdintNEXT_CABINET, has what looks like a cabinet 		*)
    (*				  file header, but the version of the cabinet file*)
    (* 				  format is not one understood by this version of*)
    (*              FDI.  The erf.erfType field is filled in with the		*)
    (* 				  version number found in the cabinet file.     *)
    (* Response:    To prevent this error, call FDIIsCabinet() to		*)
    (*				  check a cabinet before calling FDICopy() or   *)
    (* 				  returning the cabinet path to fdintNEXT_CABINET.*)
    FDIERROR_UNKNOWN_CABINET_VERSION,
    (* Description: Cabinet file is corrupt
    (* Cause:       FDI returns this error any time it finds a problem		*)
    (*              with the logical format of a cabinet file, and any     *)
    (*              time one of the passed-in file I/O calls fails when    *)
    (*              operating on a cabinet (PFNOPEN, PFNSEEK, PFNREAD,     *)
    (*              or PFNCLOSE).  The client can distinguish these two    *)
    (*              cases based upon whether the last file I/O call        *)
    (*              failed or not.                                         *)
    (* Response:    Assuming this is not a real corruption problem in      *)
    (*              a cabinet file, the file I/O functions could attempt   *)
    (*              to do retries on failure (for example, if there is a   *)
    (*              temporary network connection problem).  If this does   *)
    (*              not work, and the file I/O call has to fail, then the  *)
    (*              FDI client will have to clean up and call the          *)
    (*              FDICopy() function again.                              *)
    FDIERROR_CORRUPT_CABINET,
    (* Description: Could not allocate enough memory                       *)
    (* Cause:       FDI tried to allocate memory with the PFNALLOC         *)
    (*              function, but it failed.                               *)
    (* Response:    If possible, PFNALLOC should take whatever steps       *)
    (*              are possible to allocate the memory requested.  If     *)
    (*              memory is not immediately available, it might post a   *)
    (*              dialog asking the user to free memory, for example.    *)
    (*              Note that the bulk of FDI's memory allocations are     *)
    (*              made at FDICreate() time and when the first cabinet    *)
    (*              file is opened during FDICopy().                       *)
    FDIERROR_ALLOC_FAIL,
    (* Description: Unknown compression type in a cabinet folder           *)
    (* Cause:       [Should never happen.]  A folder in a cabinet has an   *)
    (*              unknown compression type.  This is probably caused by  *)
    (*              a mismatch between the version of FCI.LIB used to      *)
    (*              create the cabinet and the FDI.LIB used to read the    *)
    (*              cabinet.                                               *)
    (* Response:    Abort.                                                 *)
    FDIERROR_BAD_COMPR_TYPE,
    (* Description: Failure decompressing data from a cabinet file         *)
    (* Cause:       The decompressor found an error in the data coming     *)
    (*              from the file cabinet.  The cabinet file was corrupted.*)
    (*              [11-Apr-1994 bens When checksuming is turned on, this  *)
    (*              error should never occur.]                             *)
    (* Response:    Probably should abort; only other choice is to cleanup *)
    (*              and call FDICopy() again, and hope there was some      *)
    (*              intermittent data error that will not reoccur.         *)
    FDIERROR_MDI_FAIL,
    (* Description: Failure writing to target file                         *)
    (* Cause:       FDI returns this error any time it gets an error back  *)
    (*              from one of the passed-in file I/O calls fails when    *)
    (*              writing to a file being extracted from a cabinet.      *)
    (* Response:    To avoid or minimize this error, the file I/O functions*)
    (*              could attempt to avoid failing.  A common cause might  *)
    (*              be disk full -- in this case, the PFNWRITE function    *)
    (*              could have a check for free space, and put up a dialog *)
    (*              asking the user to free some disk space.               *)
    FDIERROR_TARGET_FILE,
    (* Description: Cabinets in a set do not have the same RESERVE sizes   *)
    (* Cause:       [Should never happen]. FDI requires that the sizes of  *)
    (*              the per-cabinet, per-folder, and per-data block        *)
    (*              RESERVE sections be consistent across all the cabinets *)
    (*              in a set.                                              *)
    (* Response:    Abort.                                                 *)
    FDIERROR_RESERVE_MISMATCH,
    (* Description: Cabinet returned on fdintNEXT_CABINET is incorrect     *)
    (* Cause:       NOTE: THIS ERROR IS NEVER RETURNED BY FDICopy()!       *)
    (*              Rather, FDICopy() keeps calling the fdintNEXT_CABINET  *)
    (*              callback until either the correct cabinet is specified,*)
    (*              or you return ABORT.                                   *)
    (*              When FDICopy() is extracting a file that crosses a     *)
    (*              cabinet boundary, it calls fdintNEXT_CABINET to ask    *)
    (*              for the path to the next cabinet.  Not being very      *)
    (*              trusting, FDI then checks to make sure that the        *)
    (*              correct continuation cabinet was supplied!  It does    *)
    (*              this by checking the "setID" and "iCabinet" fields     *)
    (*              in the cabinet.  When MAKECAB.EXE creates a set of     *)
    (*              cabinets, it constructs the "setID" using the sum      *)
    (*              of the bytes of all the destination file names in      *)
    (*              the cabinet set.  FDI makes sure that the 16-bit       *)
    (*              setID of the continuation cabinet matches the          *)
    (*              cabinet file just processed.  FDI then checks that     *)
    (*              the cabinet number (iCabinet) is one more than the     *)
    (*              cabinet number for the cabinet just processed.         *)
    (* Response:    You need code in your fdintNEXT_CABINET (see below)    *)
    (*              handler to do retries if you get recalled with this    *)
    (*              error.  See the sample code (EXTRACT.C) to see how     *)
    (*              this should be handled.                                *)
    FDIERROR_WRONG_CABINET,
    (* Description: FDI aborted.                                           *)
    (* Cause:       An FDI callback returnd -1 (usually).                  *)
    (* Response:    Up to client.                                          *)
    FDIERROR_USER_ABORT
  );

(*  FDINOTIFICATION - Notification structure for PFNFDINOTIFY
*
*  See the FDINOTIFICATIONTYPE definition for information on usage and
*  meaning of these fields.
*)

   PFDINOTIFICATION = ^TFDINOTIFICATION;
   TFDINOTIFICATION = Packed Record
      cb: longint;                      (* uncompressed filesize *)
      psz1: PChar;                      (* next cabinet *)
      psz2: PChar;                      (* next disk *)
      psz3: PChar;                      (* cabinet path *)
      pv: PVoid;                        (* Value for client *)
      hf: Integer;
      Date: TUUSHORT;
      Time: TUUSHORT;
      Attribs: TUUSHORT;
      setID: TUUSHORT;                  (* Cabinet set ID *)
      iCabinet: TUUSHORT;               (* Cabinet number (0-based) *)
      iFolder: TUUSHORT;                (* Folder number (0-based) *)
      fdie: TFDIERROR;
   End;
   { fdin, pfdin }

(*    FDINOTIFICATIONTYPE - FDICopy notification types
*
*  The notification function for FDICopy can be called with the following
*  values for the fdint parameter.  In all cases, the pfdin->pv field is
*  filled in with the value of the pvUser argument passed in to FDICopy().
*
*  A typical sequence of calls will be something like this:
*      fdintCABINET_INFO     // Info about the cabinet
*      fdintENUMERATE        // Starting enumeration
*      fdintPARTIAL_FILE     // Only if this is not the first cabinet, and
*                            // one or more files were continued from the
*                            // previous cabinet.
*      ...
*      fdintPARTIAL_FILE
*      fdintCOPY_FILE        // The first file that starts in this cabinet
*      ...
*      fdintCOPY_FILE        // Now let's assume you want this file...
*      // PFNWRITE called multiple times to write to this file.
*      fdintCLOSE_FILE_INFO  // File done, set date/time/attributes
*
*      fdintCOPY_FILE        // Now let's assume you want this file...
*      // PFNWRITE called multiple times to write to this file.
*      fdintNEXT_CABINET     // File was continued to next cabinet!
*      fdintCABINET_INFO     // Info about the new cabinet
*      // PFNWRITE called multiple times to write to this file.
*      fdintCLOSE_FILE_INFO  // File done, set date/time/attributes
*      ...
*      fdintENUMERATE        // Ending enumeration
*
*  fdintCABINET_INFO:
*        Called exactly once for each cabinet opened by FDICopy(), including
*        continuation cabinets opened due to file(s) spanning cabinet
*        boundaries. Primarily intended to permit EXTRACT.EXE to
*        automatically select the next cabinet in a cabinet sequence even if
*        not copying files that span cabinet boundaries.
*      Entry:
*          pfdin->psz1     = name of next cabinet
*          pfdin->psz2     = name of next disk
*          pfdin->psz3     = cabinet path name
*          pfdin->setID    = cabinet set ID (a random 16-bit number)
*          pfdin->iCabinet = Cabinet number within cabinet set (0-based)
*      Exit-Success:
*          Return anything but -1
*      Exit-Failure:
*          Returns -1 => Abort FDICopy() call
*      Notes:
*          This call is made *every* time a new cabinet is examined by
*          FDICopy().  So if "foo2.cab" is examined because a file is
*          continued from "foo1.cab", and then you call FDICopy() again
*          on "foo2.cab", you will get *two* fdintCABINET_INFO calls all
*          told.
*
*  fdintCOPY_FILE:
*        Called for each file that *starts* in the current cabinet, giving
*        the client the opportunity to request that the file be copied or
*        skipped.
*      Entry:
*          pfdin->psz1    = file name in cabinet
*          pfdin->cb      = uncompressed size of file
*          pfdin->date    = file date
*          pfdin->time    = file time
*          pfdin->attribs = file attributes
*          pfdin->iFolder = file's folder index
*      Exit-Success:
*          Return non-zero file handle for destination file; FDI writes
*          data to this file use the PFNWRITE function supplied to FDICreate,
*          and then calls fdintCLOSE_FILE_INFO to close the file and set
*          the date, time, and attributes.  NOTE: This file handle returned
*          must also be closeable by the PFNCLOSE function supplied to
*          FDICreate, since if an error occurs while writing to this handle,
*          FDI will use the PFNCLOSE function to close the file so that the
*          client may delete it.
*      Exit-Failure:
*          Returns 0  => Skip file, do not copy
*          Returns -1 => Abort FDICopy() call
*
*  fdintCLOSE_FILE_INFO:
*        Called after all of the data has been written to a target file.
*        This function must close the file and set the file date, time,
*        and attributes.
*      Entry:
*          pfdin->psz1    = file name in cabinet
*          pfdin->hf      = file handle
*          pfdin->date    = file date
*          pfdin->time    = file time
*          pfdin->attribs = file attributes
*          pfdin->iFolder = file's folder index
*          pfdin->cb      = Run After Extract (0 - don't run, 1 Run)
*      Exit-Success:
*          Returns True
*      Exit-Failure:
*          Returns False, or -1 to abort;
*
*              IMPORTANT NOTE IMPORTANT:
*                  pfdin->cb is overloaded to no longer be the size of
*                  the file but to be a binary indicated run or not
*
*              IMPORTANT NOTE:
*                  FDI assumes that the target file was closed, even if this
*                  callback returns failure.  FDI will NOT attempt to use
*                  the PFNCLOSE function supplied on FDICreate() to close
*                  the file!
*
*  fdintPARTIAL_FILE:
*        Called for files at the front of the cabinet that are CONTINUED
*        from a previous cabinet.  This callback occurs only when FDICopy is
*        started on second or subsequent cabinet in a series that has files
*        continued from a previous cabinet.
*      Entry:
*          pfdin->psz1 = file name of file CONTINUED from a PREVIOUS cabinet
*          pfdin->psz2 = name of cabinet where file starts
*          pfdin->psz3 = name of disk where file starts
*      Exit-Success:
*          Return anything other than -1; enumeration continues
*      Exit-Failure:
*          Returns -1 => Abort FDICopy() call
*
*  fdintENUMERATE:
*        Called once after a call to FDICopy() starts scanning a CAB's
*        CFFILE entries, and again when there are no more CFFILE entries.
*        If CAB spanning occurs, an additional call will occur after the
*        first spanned file is completed.  If the pfdin->iFolder value is
*        changed from zero, additional calls will occur next time it reaches
*        zero.  If iFolder is changed to zero, FDICopy will terminate, as if
*        there were no more CFFILE entries.  Primarily intended to allow an
*        application with it's own file list to help FDI advance quickly to
*        a CFFILE entry of interest.  Can also be used to allow an
*        application to determine the cb values for each file in the CAB.
*      Entry:
*        pfdin->cb        = current CFFILE position
*        pfdin->iFolder   = number of files remaining
*        pfdin->setID     = current CAB's setID value
*      Exit-Don't Care:
*        Don't change anything.
*        Return anything but -1.
*      Exit-Forcing a skip:
*        pfdin->cb        = desired CFFILE position
*        pfdin->iFolder   = desired # of files remaining
*        Return anything but -1.
*      Exit-Stop:
*        pfdin->iFolder    = set to 0
*        Return anything but -1.
*      Exit-Failure:
*        Return -1 => Abort FDICopy call ("user aborted".)
*      Notes:
*        This call can be ignored by applications which want normal file
*        searching.  The application can adjust the supplied values to
*        force FDICopy() to continue it's search at another location, or
*        to force FDICopy() to terminate the search, by setting iFolder to 0.
*        (FDICopy() will report no error when terminated this way.)
*        FDI has no means to verify the supplied cb or iFolder values.
*        Arbitrary values are likely to cause undesirable results.  An
*        application should cross-check pfdin->setID to be certain the
*        external database is in sync with the CAB.  Reverse-skips are OK
*        (but may be inefficient) unless fdintNEXT_CABINET has been called.
*
*  fdintNEXT_CABINET:
*        This function is *only* called when fdintCOPY_FILE was told to copy
*        a file in the current cabinet that is continued to a subsequent
*        cabinet file.  It is important that the cabinet path name (psz3)
*        be validated before returning!  This function should ensure that
*        the cabinet exists and is readable before returning.  So, this
*        is the function that should, for example, issue a disk change
*        prompt and make sure the cabinet file exists.
*
*        When this function returns to FDI, FDI will check that the setID
*        and iCabinet match the expected values for the next cabinet.
*        If not, FDI will continue to call this function until the correct
*        cabinet file is specified, or until this function returns -1 to
*        abort the FDICopy() function.  pfdin->fdie is set to
*        FDIERROR_WRONG_CABINET to indicate this case.
*
*        If you *haven't* ensured that the cabinet file is present and
*        readable, or the cabinet file has been damaged, pfdin->fdie will
*        receive other appropriate error codes:
*
*              FDIERROR_CABINET_NOT_FOUND
*              FDIERROR_NOT_A_CABINET
*              FDIERROR_UNKNOWN_CABINET_VERSION
*              FDIERROR_CORRUPT_CABINET
*              FDIERROR_BAD_COMPR_TYPE
*              FDIERROR_RESERVE_MISMATCH
*              FDIERROR_WRONG_CABINET
*
*      Entry:
*          pfdin->psz1 = name of next cabinet where current file is continued
*          pfdin->psz2 = name of next disk where current file is continued
*          pfdin->psz3 = cabinet path name; FDI concatenates psz3 with psz1
*                          to produce the fully-qualified path for the cabinet
*                          file.  The 256-byte buffer pointed at by psz3 may
*                          be modified, but psz1 may not!
*          pfdin->fdie = FDIERROR_WRONG_CABINET if the previous call to
*                        fdintNEXT_CABINET specified a cabinet file that
*                        did not match the setID/iCabinet that was expected.
*      Exit-Success:
*          Return anything but -1
*      Exit-Failure:
*          Returns -1 => Abort FDICopy() call
*      Notes:
*          This call is almost always made when a target file is open and
*          being written to, and the next cabinet is needed to get more
*          data for the file.
*)

   TFDINOTIFICATIONTYPE = (
      fdintCABINET_INFO,                (* General information about cabinet	*)
      fdintPARTIAL_FILE,                (* First file in cabinet is continuation*)
      fdintCOPY_FILE,                   (* File to be copied			*)
      fdintCLOSE_FILE_INFO,             (* close the file, set relevant info    *)
      fdintNEXT_CABINET,                (* File continued to next cabinet       *)
      fdintENUMERATE                    (* Enumeration status                   *)
      );
   TFNFDINOTIFY_dummy = Function(fdint: TFDINOTIFICATIONTYPE;
      pfdin: PFDINOTIFICATION): Integer; CDECL;
   PFNFDINOTIFY = TFNFDINOTIFY_dummy;

   (* cpuType values for FDICreate()
    *
    *  (Ignored by 32-bit FDI.)
    *)

   //Const
   //  cpuUNKNOWN = (-1);
   //  cpu80286 = (0);
   //  cpu80386 = (1);

   (* FDIDECRYPTTYPE - PFNFDIDECRYPT command types *)

   TFDIDECRYPTTYPE = (
      fdidtNEW_CABINET,                 (* New cabinet *)
      fdidtNEW_FOLDER,                  (* New folder	*)
      fdidtDECRYPT                      (* Decrypt a data block *)
      );                                (* fdidt *)

   (* FDIDECRYPT - Data for PFNFDIDECRYPT function *)
   TFDIDECRYPT = Packed Record
      fdidt: TFDIDECRYPTTYPE;           (* Command type (selects union below) 	*)
      pvUser: PVoid;                    (* Decryption context			*)
      _noname1: Packed Record
         Case Integer Of
            1: (
               cabinet:
               Packed Record            (* fdidtNEW_CABINET *)
                  pHeaderReserve: PVoid; (* RESERVE section from CFHEADER *)
                  cbHeaderReserve: TUUSHORT; (* Size of pHeaderReserve 	   *)
                  setID: TUUSHORT;      (* Cabinet set ID		   *)
                  iCabinet: Integer;    (* Cabinet number in set (0 based) *)
               End;
               );
            2: (
               folder:
               Packed Record            // fdidtNEW_FOLDER
                  pFolderReserve: PVoid; (* RESERVE section from CFFOLDER *)
                  cbFolderReserve: TUUSHORT; (* Size of pFolderReserve    *)
                  iFolder: TUUSHORT;    (* Folder number in cabinet (0 based) *)
               End;
               );
            3: (
               decrypt:
               Packed Record            // fdidtDECRYPT
                  pDataReserve: PVoid;  (* RESERVE section from CFDATA	       *)
                  cbDataReserve: TUUSHORT; (* Size of pDataReserve             *)
                  pbData: PVoid;        (* Data buffer                         *)
                  cbData: TUUSHORT;     (* Size of data buffer                 *)
                  fSplit: Boolean;      (* True if this is a split data block  *)
                  cbPartial: TUUSHORT;  (* 0 if this is not a split block, or  *)
                  (*  the first piece of a split block;  *)
                  (* Greater than 0 if this is the       *)
                  (*  second piece of a split block.     *)
               End;
               );
      End;
   End;
   PFDIDECRYPT = ^TFDIDECRYPT;

   (*    FNALLOC - Memory Allocation
    *      FNFREE  - Memory Free
    *
    *  These are modeled after the C run-time routines malloc() and free()
    *  FDI expects error handling to be identical to these C run-time routines.
    *
    *  As long as you faithfully copy the semantics of malloc() and free(),
    *  you can supply any functions you like!
    *
    *  WARNING: You should never assume anything about the sequence of
    *           PFNALLOC and PFNFREE calls -- incremental releases of
    *           FDI may have radically different numbers of
    *           PFNALLOC calls and allocation sizes!
    *)

   (* Memory functions for FDI *)

   TFNALLOC_dummy = Function(cb: TULONG): PVoid; CDECL;
   PFNALLOC = TFNALLOC_dummy;

   TFNFREE_dummy = Function(pv: PVoid): Pointer; CDECL;
   PFNFREE = TFNFREE_dummy;

   (*      PFNOPEN  - File I/O callbacks for FDI
    *      PFNREAD
    *      PFNWRITE
    *      PFNCLOSE
    *      PFNSEEK
    *
    *  These are modeled after the C run-time routines _open, _read,
    *  _write, _close, and _lseek.  The values for the PFNOPEN oflag
    *  and pmode calls are those defined for _open.  FDI expects error
    *  handling to be identical to these C run-time routines.
    *
    *  As long as you faithfully copy these aspects, you can supply
    *  any functions you like!
    *
    *  WARNING: You should never assume you know what file is being
    *           opened at any one point in time!  FDI will usually
    *           stick to opening cabinet files, but it is possible
    *           that in a future implementation it may open temporary
    *           files or open cabinet files in a different order.
    *
    *  Notes for Memory Mapped File fans:
    *      You can write wrapper routines to allow FDI to work on memory
    *      mapped files.  You'll have to create your own "handle" type so that
    *      you can store the base memory address of the file and the current
    *      seek position, and then you'll allocate and fill in one of these
    *      structures and return a pointer to it in response to the PFNOPEN
    *      call and the fdintCOPY_FILE call.  Your PFNREAD and PFNWRITE
    *      functions will do memcopy(), and update the seek position in your
    *      "handle" structure.  PFNSEEK will just change the seek position
    *      in your "handle" structure.
    *)

   (* File I/O functions for FDI *)

   TFNOPEN_dummy = Function(pszFile: PChar; oflag: Integer; pmode: Integer): Integer; CDECL;
   pfnopen = TFNOPEN_dummy;

   TFNREAD_dummy = Function(hf: Integer; pv: PVoid; cb: TUINT): TUINT; CDECL;
   pfnread = TFNREAD_dummy;

   TFNWRITE_dummy = Function(hf: Integer; pv: PVoid; cb: TUINT): TUINT; CDECL;
   pfnwrite = TFNWRITE_dummy;

   TFNCLOSE_dummy = Function(hf: Integer): Integer; CDECL;
   pfnclose = TFNCLOSE_dummy;

   TFNSEEK_dummy = Function(hf: Integer; dist: longint; SeekType: Integer): longint; CDECL;
   pfnseek = TFNSEEK_dummy;

   (*    PFNFDIDECRYPT - FDI Decryption callback
    *
    *  If this function is passed on the FDICopy() call, then FDI calls it
    *  at various times to update the decryption state and to decrypt FCDATA
    *  blocks.
    *
    *  Common Entry Conditions:
    *      pfdid->fdidt  - Command type
    *      pfdid->pvUser - pvUser value from FDICopy() call
    *
    *  fdidtNEW_CABINET:   //** Notification of a new cabinet
    *      Entry:
    *        pfdid->cabinet.
    *          pHeaderReserve  - RESERVE section from CFHEADER
    *          cbHeaderReserve - Size of pHeaderReserve
    *          setID           - Cabinet set ID
    *          iCabinet        - Cabinet number in set (0 based)
    *      Exit-Success:
    *          returns anything but -1;
    *      Exit-Failure:
    *          returns -1; FDICopy() is aborted.
    *      Notes:
    *      (1) This call allows the decryption code to pick out any information
    *          from the cabinet header reserved area (placed there by DIACRYPT)
    *          needed to perform decryption.  If there is no such information,
    *          this call would presumably be ignored.
    *      (2) This call is made very soon after fdintCABINET_INFO.
    *
    *  fdidtNEW_FOLDER:    //** Notification of a new folder
    *      Entry:
    *        pfdid->folder.
    *          pFolderReserve  - RESERVE section from CFFOLDER
    *          cbFolderReserve - Size of pFolderReserve
    *          iFolder         - Folder number in cabinet (0 based)
    *      Exit-Success:
    *          returns anything but -1;
    *      Exit-Failure:
    *          returns -1; FDICopy() is aborted.
    *      Notes:
    *          This call allows the decryption code to pick out any information
    *          from the folder reserved area (placed there by DIACRYPT) needed
    *          to perform decryption.  If there is no such information, this
    *          call would presumably be ignored.
    *
    *  fdidtDECRYPT:       //** Decrypt a data buffer
    *      Entry:
    *        pfdid->folder.
    *          pDataReserve  - RESERVE section for this CFDATA block
    *          cbDataReserve - Size of pDataReserve
    *          pbData        - Data buffer
    *          cbData        - Size of data buffer
    *          fSplit        - True if this is a split data block
    *          cbPartial     - 0 if this is not a split block, or the first
    *                              piece of a split block; Greater than 0 if
    *                              this is the second piece of a split block.
    *      Exit-Success:
    *          returns True;
    *      Exit-Failure:
    *          returns False; error during decrypt
    *          returns -1; FDICopy() is aborted.
    *      Notes:
    *          FCI will split CFDATA blocks across cabinet boundaries if
    *          necessary.  To provide maximum flexibility, FDI will call the
    *          fdidtDECRYPT function twice on such split blocks, once when
    *          the first portion is read, and again when the second portion
    *          is read.  And, of course, most data blocks will not be split.
    *          So, there are three cases:
    *
    *           1) fSplit == False
    *              You have the entire data block, so decrypt it.
    *
    *           2) fSplit == True, cbPartial == 0
    *              This is the first portion of a split data block, so cbData
    *              is the size of this portion.  You can either choose to decrypt
    *              this piece, or ignore this call and decrypt the full CFDATA
    *              block on the next (second) fdidtDECRYPT call.
    *
    *           3) fSplit == True, cbPartial > 0
    *              This is the second portion of a split data block (indeed,
    *              cbPartial will have the same value as cbData did on the
    *              immediately preceeding fdidtDECRYPT call!).  If you decrypted
    *              the first portion on the first call, then you can decrypt the
    *              second portion now.  If you ignored the first call, then you
    *              can decrypt the entire buffer.
    *              NOTE: pbData points to the second portion of the split data
    *                    block in this case, *not* the entire data block.  If
    *                    you want to wait until the second piece to decrypt the
    *                    *entire* block, pbData-cbPartial is the address of the
    *                    start of the whole block, and cbData+cbPartial is its
    *                    size.
    *)

   TFNFDIDECRYPT_dummy = Function(pfdid: PFDIDECRYPT): Integer; CDECL;
   PFNFDIDECRYPT = TFNFDIDECRYPT_dummy;

   (* FDICABINETINFO - Information about a cabinet *)
   TFDICABINETINFO = Packed Record
      cbCabinet: longint;               (* 4  Total length of cabinet file	  *)
      cFolders: TUUSHORT;               (* 6  Count of folders in cabinet         *)
      cFiles: TUUSHORT;                 (* 8  Count of files in cabinet           *)
      setID: TUUSHORT;                  (* 10 Cabinet set ID                      *)
      iCabinet: TUUSHORT;               (* 12 Cabinet number in set (0 based)     *)
      fReserve: LongBool;               (* 16 True => RESERVE present in cabinet  *)
      hasprev: LongBool;                (* 20 True => Cabinet is chained prev     *)
      hasnext: LongBool;                (* 24 True => Cabinet is chained next     *)
   End;
   TPFDICABINETINFO = ^TFDICABINETINFO;

// functions

   (*************************************************************
    *  FDICreate - Create an FDI context
    *
    *  Entry:
    *      pfnalloc
    *      pfnfree
    *      pfnopen
    *      pfnread
    *      pfnwrite
    *      pfnclose
    *      pfnlseek
    *      cpuType  - Select CPU type (auto-detect, 286, or 386+)
    *                 NOTE: For the 32-bit FDI.LIB, this parameter is ignored!
    *      perf
    *
    *  Exit-Success:
    *      Returns non-NULL FDI context handle.
    *
    *  Exit-Failure:
    *      Returns NULL; perf filled in with error code
    *
    *************************************************************)
   (* CAUTION: If DYNLOADUNCABDLL is "not" defined, the CABINET.DLL
      must be found.  If not found, Delphi will be unable to load
      the cmplib32.dcl library the next time Delphi is run. *)

  (*
  *  HFDI - Handle to an FDI context
  *
  *  FDICreate() creates this, and it must be passed to all other FDI
  *  functions.
  *)

  HFDI = PVoid;

  PERF = ^TERF;
  TERF = Packed Record
    erfOper: Integer;                 (* FCI/FDI error code -- see FDIERROR_XXX    *)
                                      (*  and FCIERR_XXX equates for details.      *)
    erfType: Integer;                 (* Optional error value filled in by FCI/FDI *)
                                      (* For FCI, this is usually the C run-time   *)
                                      (* *errno* value.                            *)
    fError: LongBool;                 (* True => error present *)
  End;

  TFDICreate = Function(PFNALLOC: PFNALLOC; PFNFREE: PFNFREE; pfnopen: pfnopen;
    pfnread: pfnread; pfnwrite: pfnwrite; pfnclose: pfnclose;
    pfnseek: pfnseek; CpuType: Integer; PERF: PERF): HFDI; CDECL;

(*************************************************************
 *    FDIIsCabinet - Determines if file is a cabinet, returns info if it is
 *
 *  Entry:
 *      hfdi   - Handle to FDI context (created by FDICreate())
 *      hf     - File handle suitable for PFNREAD/PFNSEEK, positioned
 *               at offset 0 in the file to test.
 *      pfdici - Buffer to receive info about cabinet if it is one.
 *
 *  Exit-Success:
 *      Returns True; file is a cabinet, pfdici (variable CabInfo) filled in.
 *
 *  Exit-Failure:
 *      Returns False, file is not a cabinet;  If an error occurred,
 *          perf (passed on FDICreate call!) filled in with error.
 *************************************************************)

  TFDIIsCabinet = Function(HFDI: HFDI; hf: Integer; pfdici: TPFDICABINETINFO): Boolean; CDECL;

(*************************************************************
 *    FDICopy - extracts files from a cabinet
 *
 *  Entry:
 *      hfdi        - handle to FDI context (created by FDICreate())
 *      pszCabinet  - main name of cabinet file
 *      pszCabPath  - Path to cabinet file(s)
 *      flags       - Flags to modify behavior
 *      pfnfdin     - Notification function
 *      pfnfdid     - Decryption function (pass NULL if not used)
 *      pvUser      - User specified value to pass to notification function
 *
 *  Exit-Success:
 *      Returns True;
 *
 *  Exit-Failure:
 *      Returns False, perf (passed on FDICreate call!) filled in with
 *          error.
 *
 *  Notes:
 *  (1) If FDICopy() fails while a target file is being written out, then
 *      FDI will use the PFNCLOSE function to close the file handle for that
 *      target file that was returned from the fdintCOPY_FILE notification.
 *      The client application is then free to delete the target file, since
 *      it will not be in a valid state (since there was an error while
 *      writing it out).
 *
 *************************************************************)

  TFDICopy = Function(HFDI: HFDI; pszCabinet: PChar; pszCabPath: PChar;
    flags: Integer; pfnfdin: PFNFDINOTIFY; pfnfdid: PFNFDIDECRYPT;
    pvUser: PVoid): Boolean; CDECL;

(*************************************************************
 *    FDIDestroy - Destroy an FDI context
 *
 *  Entry:
 *      hfdi - handle to FDI context (created by FDICreate())
 *
 *  Exit-Success:
 *      Returns True;
 *
 *  Exit-Failure:
 *      Returns False;
 *************************************************************)

  TFDIDestroy = Function(HFDI: HFDI): Boolean; CDECL;

Implementation

End.
