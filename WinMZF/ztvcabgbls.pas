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
Unit ztvCabGbls;

Interface

Uses
   Classes,
   SysUtils,
   Windows,
   ztvStreams;

Type
   TBYTE = char;                        (* b  *)
   TUINT = Integer;                     (* ui *)
   TUUSHORT = smallint;                 (* us *)
   TULONG = longint;                    (* ul *)
   TCOMP = smallint;
   PVoid = Pointer;
   UUSHORT = word;

   (* Memory functions for FCI *)
Type
   TFNFCIALLOC = Function(cb: TULONG): PVoid; CDECL;
   PFNFCIALLOC = TFNFCIALLOC;

   TFNFCIFREE = Function(memory: PVoid): Pointer; CDECL;
   PFNFCIFREE = TFNFCIFREE;

Type
   TCHECKSUM = longint;                 { csum }
   TUOFF = longint;                     { uoff - uncompressed offset }
   TCOFF = longint;                     { coff - cabinet file offset }

Const
   CB_MAX_CHUNK = 32768;
   CB_MAX_DISK = $7FFFFFF;
   CB_MAX_FILENAME = 256;
   CB_MAX_CABINET_NAME = 256;
   CB_MAX_CAB_PATH = 256;
   CB_MAX_DISK_NAME = 256;

Const
   _O_RDONLY = $0000;
   _O_WRONLY = $0001;
   _O_RDWR = $0002;
   _O_APPEND = $0008;
   _O_CREAT = $0100;
   _O_TRUNC = $0200;
   _O_EXCL = $0400;
   _O_TEXT = $4000;
   _O_BINARY = $8000;
   _O_RAW = _O_BINARY;
   _O_NOINHERIT = $0080;
   _O_TEMPORARY = $0040;
   _O_SHORT_LIVED = $1000;
   _O_SEQUENTIAL = $0020;
   _O_RANDOM = $0010;

   (*    tcompXXX - Compression types
    *
    *  These are passed to FCIAddFile(), and are also stored in the CFFOLDER
    *  structures in cabinet files.
    *
    *  NOTE: We reserve bits for the TYPE, QUANTUM_LEVEL, and QUANTUM_MEM
    *        to provide room for future expansion.  Since this value is stored
    *        in the CFDATA records in the cabinet file, we don't want to
    *        have to change the format for existing compression configurations
    *        if we add new ones in the future.  This will allows us to read
    *        old cabinet files in the future.
    *)
Const
   tcompMASK_TYPE = $000F;
   tcompTYPE_NONE = $0000;
   tcompTYPE_MSZIP = $0001;
   tcompTYPE_QUANTUM = $0002;
   tcompTYPE_LZX = $0003;
   tcompBAD = $000F;
   tcompMASK_LZX_WINDOW = $1F00;
   tcompLZX_WINDOW_LO = $0F00;
   tcompLZX_WINDOW_HI = $1500;
   tcompSHIFT_LZX_WINDOW = 8;
   tcompMASK_QUANTUM_LEVEL = $00F0;
   tcompQUANTUM_LEVEL_LO = $0010;
   tcompQUANTUM_LEVEL_HI = $0070;
   tcompSHIFT_QUANTUM_LEVEL = 4;
   tcompMASK_QUANTUM_MEM = $1F00;
   tcompQUANTUM_MEM_LO = $0A00;
   tcompQUANTUM_MEM_HI = $1500;
   tcompSHIFT_QUANTUM_MEM = 8;
   tcompMASK_RESERVED = $E000;

   (* FAT file attribute flag used by FCI/FDI to indicate that
    * the filename in the CAB is a UTF string *)
Const
   _A_NAME_IS_UTF = $80;

   (* FAT file attribute flag used by FCI/FDI to indicate that
    * the file should be executed after extraction *)
Const
   _A_EXEC = $40;

Type
   TCabFileType = (ftCabinet, ftSource, ftDestination);

   TCabinetFileHandler = Class(TComponent)
   Public
      //FUNCTION FindStream (Handle : Integer) : TStream32; VIRTUAL; ABSTRACT;
      Function Open(Const FileName: String; OpenFlag, OpenMode: Integer;
         Var Error: Integer; FileType: TCabFileType): Integer; Virtual; Abstract;
      Function Read(FileHandle: Integer; Var Buffer;
         Count: Integer; Var Error: Integer): Integer; Virtual; Abstract;
      Function Write(FileHandle: Integer; Var Buffer;
         Count: Integer; Var Error: Integer): Integer; Virtual; Abstract;
      Function Close(FileHandle: Integer; Var Error: Integer)
         : Integer; Virtual; Abstract;
      Function Seek(FileHandle: Integer; Distance: longint;
         SeekType: Integer; Var Error: Integer): Integer; Virtual; Abstract;
      Function Delete(Const FileName: String; Var Error: Integer)
         : Integer; Virtual; Abstract;
   End;

   (* TStreamCabinetFileHandler CLASS -- uses a collection of streams for
      handling file access. The streams are accessed in the file system *)
   TStreamCabinetFileHandler = Class(TCabinetFileHandler)
   Protected
      fStreams: TList;
      //FUNCTION FindStream (Handle : Integer) : TStream32;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;

      Function FindStream(Handle: Integer): TStream32;
      Function Open(Const FileName: String; OpenFlag, OpenMode: Integer;
         Var Error: Integer; FileType: TCabFileType): Integer; Override;
      Function Read(FileHandle: Integer; Var Buffer;
         Count: Integer; Var Error: Integer): Integer; Override;
      Function Write(FileHandle: Integer; Var Buffer;
         Count: Integer; Var Error: Integer): Integer; Override;
      Function Close(FileHandle: Integer; Var Error: Integer): Integer; Override;
      Function Seek(FileHandle: Integer; Distance: longint;
         SeekType: Integer; Var Error: Integer): Integer; Override;
      Function Delete(Const FileName: String; Var Error: Integer): Integer; Override;
   End;

   (* Exceptions *)
   ECabinetError = Class(exception)
      ErrorCode: Integer;
      ErrorType: Integer;
   End;

   (* FUNCTION declarations *)
Procedure ApiCheck(Result: Boolean);
Procedure RaiseCabinetError(ErrorCode, ErrorType: Integer);
Function CompressionTypeFromTCOMP(tc: TCOMP): Integer;
Function CompressionLevelFromTCOMP(tc: TCOMP): Integer;
Function CompressionMemoryFromTCOMP(tc: TCOMP): Integer;
Function TCOMPfromTypeLevelMemory(t: Integer; l: Integer; m: Integer): Integer;
Function LZXCompressionWindowFromTCOMP(tc: TCOMP): Integer;
Function TCOMPfromLZXWindow(w: Integer): Integer;

(* Standard FCI/FDI procs *)
Function StdFciAlloc(cb: TULONG): PVoid; CDECL;
Function StdFciFree(memory: PVoid): Pointer; CDECL;

Implementation

Type
   EApiError = Class(exception)
   Protected
      FErrorCode: longint;
   Public
      Constructor Create(ErrorCode: longint);
      //CONSTRUCTOR CreateMsg (ErrorCode : Longint; CONST MESSAGE : String);
      Property ErrorCode: longint Read FErrorCode Write FErrorCode;
   End;

   //-------------------------------------------------------------
   (* Standard FCI/FDI procs *)
   //-------------------------------------------------------------

Function StdFciAlloc(cb: TULONG): PVoid; Cdecl;
Begin
   GetMem(Result, cb);
End;
//-------------------------------------------------------------

Function StdFciFree(memory: PVoid): Pointer; Cdecl;
Begin
   FreeMem(memory);
   Result := Nil;                       //!! Correct?
End;
//-------------------------------------------------------------

//-------------------------------------------------------------

Function CompressionTypeFromTCOMP(tc: TCOMP): Integer;
Begin
   Result := ((tc) And tcompMASK_TYPE);
End;
//-------------------------------------------------------------

Function CompressionLevelFromTCOMP(tc: TCOMP): Integer;
Begin
   Result := (((tc) And tcompMASK_QUANTUM_LEVEL) Shr tcompSHIFT_QUANTUM_LEVEL);
End;
//-------------------------------------------------------------

Function CompressionMemoryFromTCOMP(tc: TCOMP): Integer;
Begin
   Result := (((tc) And tcompMASK_QUANTUM_MEM) Shr tcompSHIFT_QUANTUM_MEM);
End;
//-------------------------------------------------------------

Function TCOMPfromTypeLevelMemory(t: Integer; l: Integer; m: Integer): Integer;
Begin
   Result := (((m) Shl tcompSHIFT_QUANTUM_MEM) Or ((l) Shl tcompSHIFT_QUANTUM_LEVEL) Or (t));
End;
//-------------------------------------------------------------

Function LZXCompressionWindowFromTCOMP(tc: TCOMP): Integer;
Begin
   Result := (((tc) And tcompMASK_LZX_WINDOW) Shr tcompSHIFT_LZX_WINDOW);
End;
//-------------------------------------------------------------

Function TCOMPfromLZXWindow(w: Integer): Integer;
Begin
   Result := (((w) Shl tcompSHIFT_LZX_WINDOW) Or (tcompTYPE_LZX));
End;
//-------------------------------------------------------------
(* TStreamCabinetFileHandler *)
//-------------------------------------------------------------

Function TStreamCabinetFileHandler.FindStream(Handle: Integer): TStream32;
Var
   i: Integer;
Begin
   For i := 0 To fStreams.Count - 1 Do
      If TFileStream32(fStreams.Items[i]).Handle = Handle Then
      Begin
         Result := TStream32(fStreams.Items[i]);
         Exit;
      End;
   Result := Nil;
End;
//-------------------------------------------------------------

Constructor TStreamCabinetFileHandler.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fStreams := TList.Create;
End;
//-------------------------------------------------------------

Destructor TStreamCabinetFileHandler.Destroy;
Var
   Stream: TStream32;
Begin
   While fStreams.Count > 0 Do
   Begin
      Stream := fStreams.last;
      Stream.Free;
      fStreams.Remove(Stream);
   End;
   fStreams.Free;
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TStreamCabinetFileHandler.Open(Const FileName: String; OpenFlag,
   OpenMode: Integer; Var Error: Integer; FileType: TCabFileType): Integer;
Var
   mode: word;
   Stream: TFileStream32;
Begin

   If OpenFlag And _O_CREAT <> 0 Then
      mode := fmCreate
   Else
   Begin
      mode := 0;
      If OpenFlag And _O_RDWR <> 0 Then
         mode := fmOpenReadWrite
      Else
         If OpenFlag And _O_WRONLY <> 0 Then
            mode := mode Or fmOpenWrite
         Else
            mode := mode Or fmOpenRead;

      If OpenFlag And _O_EXCL <> 0 Then
         mode := mode Or fmShareExclusive
      Else
         mode := mode Or fmShareDenyNone;
   End;


   Stream := TFileStream32.Create(FileName, mode);
   If (Stream.Handle < 0) Then
   Begin
      Result := -1;
      Error := -1; //GetlastError();
      Exit;
   End;

   fStreams.Add(Stream);
   Error := 0;
   Result := Stream.Handle;
End;
//-------------------------------------------------------------

Function TStreamCabinetFileHandler.Read(FileHandle: Integer; Var Buffer;
   Count: Integer; Var Error: Integer): Integer;
Var
   Stream: TStream32;
Begin
   Stream := FindStream(FileHandle);
   If Stream <> Nil Then
   Begin
      Result := Stream.Read(Buffer, Count);
      If Result <> Count Then
         Error := GetlastError()
      Else
         Error := 0;
   End
   Else
   Begin
      Error := -1;
      Result := 0;
   End;
End;
//-------------------------------------------------------------

Function TStreamCabinetFileHandler.Write(FileHandle: Integer; Var Buffer;
   Count: Integer; Var Error: Integer): Integer;
Var
   Stream: TStream32;
Begin
   Stream := FindStream(FileHandle);
   If Stream <> Nil Then
   Begin
      Result := Stream.Write(Buffer, Count);
      If Result <> Count Then
         Error := GetlastError()
      Else
         Error := 0;
   End
   Else
   Begin
      Error := -1;
      Result := 0;
   End;
End;
//-------------------------------------------------------------

Function TStreamCabinetFileHandler.Close(FileHandle: Integer;
   Var Error: Integer): Integer;
Var
   Stream: TStream32;
Begin
   Stream := FindStream(FileHandle);
   If Stream <> Nil Then
   Begin
      fStreams.Remove(Stream);
      Stream.Free;
   End;

   Error := 0;
   Result := 0;
End;
//-------------------------------------------------------------

Function TStreamCabinetFileHandler.Seek(FileHandle: Integer;
   Distance: longint; SeekType: Integer; Var Error: Integer): Integer;
Var
   Stream: TStream32;
Begin
   Stream := FindStream(FileHandle);
   If Stream <> Nil Then
   Begin
      Result := Stream.Seek(Distance, SeekType);

      If SeekType = 0 Then
      Begin
         If Result <> Distance Then
            Error := GetlastError()
         Else
            Error := 0;
      End
      Else
         Error := 0;

   End
   Else
   Begin
      Error := -1;
      Result := 0;
   End;
End;
//-------------------------------------------------------------

Function TStreamCabinetFileHandler.Delete(Const FileName: String;
   Var Error: Integer): Integer;
Begin
   If Not DeleteFile(PChar(FileName)) Then
      Error := GetlastError()
   Else
      Error := 0;

   Result := 0;
End;
//-------------------------------------------------------------
(* EApiError *)
//-------------------------------------------------------------

Constructor EApiError.Create(ErrorCode: longint);
Begin
   Inherited CreateFmt('Error %d: %s', [ErrorCode, SysErrorMessage(ErrorCode)]);
End;
//-------------------------------------------------------------
{CONSTRUCTOR EApiError.CreateMsg (ErrorCode : Longint; CONST MESSAGE : String);
BEGIN
  INHERITED CreateFmt(MESSAGE, [ErrorCode]);
END;}
//-------------------------------------------------------------
(* Functions *)
//-------------------------------------------------------------
(* Raises an EApiError with the specified system error code *)

Procedure ApiError(ErrorCode: longint);
Begin
   Raise EApiError.Create(ErrorCode);
End;
//-------------------------------------------------------------

Procedure ApiCheck(Result: Boolean);
Begin
   If Not Result Then
   	ApiError(GetlastError());
End;
//-------------------------------------------------------------

Procedure RaiseCabinetError(ErrorCode, ErrorType: Integer);
Var
   e: ECabinetError;
Begin
   e := ECabinetError.CreateFmt('Cabinet error %d (type %d)',
      [ErrorCode, ErrorType]);
   e.ErrorCode := ErrorCode;
   e.ErrorType := ErrorType;
   Raise e;
End;
//-------------------------------------------------------------

End.
