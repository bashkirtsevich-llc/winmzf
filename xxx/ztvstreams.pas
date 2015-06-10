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
Unit ztvStreams;

Interface

Uses
   Windows,
   Classes,
   SysUtils;

{$i ZipTV.inc}

{$ifdef DEL6_OR_HIGHER}
{$WARN SYMBOL_PLATFORM OFF}
Resourcestring
  SReadError = 'Stream read error';
  SWriteError = 'Stream write error';
  SMemoryStreamError = 'Out of memory while expanding memory stream';
{$endif}

Resourcestring
   SRangeError = 'Range check error';
   sSeekNotImplemented = 'Seek not implemented';


Type
{$ifndef DEL6_OR_HIGHER}
   TSeekOrigin = (soBeginning, soCurrent, soEnd);
{$endif}

   TDeflateType =
      (dtDeflateS, dtDeflateF, dtDeflateN, dtDeflateX);

{$ifdef DEL6_OR_HIGHER}
   TStream32 = Class(TStream)
   Private
		fProgressCallBackProc: TNotifyEvent;
   Protected
		pCancel: ^Boolean;
      fOnProgress: TNotifyEvent;
      Procedure Progress(Sender: TObject); Dynamic;
   Public
      CancelCallBackProc: Pointer;
    	Function CopyFrom(Source: TStream32; Count: Int64): Int64; Virtual;
      Property OnProgress: TNotifyEvent Read fOnProgress Write fOnProgress;
      Property ProgressCallBackProc: TNotifyEvent Read fProgressCallBackProc Write fProgressCallBackProc;
	End;
{$else}
   TStream32 = Class(TObject)
   Private
		fProgressCallBackProc: TNotifyEvent;
      Function GetPosition: Int64;
      Function GetSize: Int64; //Virtual;
      Procedure SetPosition(Const Pos: Int64); Virtual;
      Procedure SetSize64(Const NewSize: Int64);
   Protected
		pCancel: ^Boolean;
      fOnProgress: TNotifyEvent;
      Procedure Progress(Sender: TObject); Dynamic;
      Procedure SetSize(NewSize: Longint); Overload; Virtual;
      Procedure SetSize(Const NewSize: Int64); Overload; Virtual;
   Public
      CancelCallBackProc: Pointer;
      Function Read(Var Buffer; Count: Longint): Longint; Virtual; Abstract;
      Function Write(Const Buffer; Count: Longint): Longint; Virtual; Abstract;
    	Function Seek(Offset: Longint; Origin: Word): Longint; Overload; Virtual;
      Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Overload; Virtual;
      Procedure ReadBuffer(Var Buffer; Count: Longint);
      Procedure WriteBuffer(Const Buffer; Count: Longint);
    	Function CopyFrom(Source: TStream32; Count: Int64): Int64; Virtual;
      //Function CopyFrom(Source: TStream32; Count: Int64;
      //   CancelCallBackProc: Pointer; ProgressCallBackProc: TNotifyEvent):
      //   Int64; Virtual;
      Function ReadComponent(Instance: TComponent): TComponent;
      Function ReadComponentRes(Instance: TComponent): TComponent;
      Procedure WriteComponent(Instance: TComponent);
      Procedure WriteComponentRes(Const ResName: String; Instance: TComponent);
      Procedure WriteDescendent(Instance, Ancestor: TComponent); Virtual;
      Procedure WriteDescendentRes(Const ResName: String; Instance, Ancestor: TComponent);
      Procedure WriteResourceHeader(Const ResName: String; Out FixupInfo: Integer);
      Procedure FixupResourceHeader(FixupInfo: Integer);
      Procedure ReadResHeader;
      Property Position: Int64 Read GetPosition Write SetPosition;
      Property Size: Int64 Read GetSize Write SetSize64;
      Property OnProgress: TNotifyEvent Read fOnProgress Write fOnProgress;
      Property ProgressCallBackProc: TNotifyEvent Read fProgressCallBackProc Write fProgressCallBackProc;
   End;
{$endif DEL6_OR_HIGHER}

   THandleStream32 = Class(TStream32)
   Private
   Protected
      fHandle: Integer;
      Function GetSize: Int64; //Override;
      Procedure SetSize(NewSize: Longint); Overload; Override;
      Procedure SetSize(Const NewSize: Int64); Overload; Override;
   Public
      Constructor Create(AHandle: Integer);
      Destructor Destroy; Override;
      Function Read(Var Buffer; Count: Longint): Longint; Override;
      Function Write(Const Buffer; Count: Longint): Longint; Override;
    	Function Seek(Offset: Longint; Origin: Word): Longint; Overload; Override;
      Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Overload; Override;
      Property Handle: Integer Read fHandle;
   End;

   TFileStream32 = Class(THandleStream32)
   Public
      Constructor Create(Const FileName: String; Mode: Word);
      Destructor Destroy; Override;
   End;

   TCustomMemoryStream32 = Class(TStream32)
   Private
      fMemory: Pointer;
      fSize, fPosition: Longint;
   Protected
      Procedure SetPointer(Ptr: Pointer; Size: Longint);
   Public
      Function Read(Var Buffer; Count: Longint): Longint; Override;
      Function Seek(Offset: Longint; Origin: Word): Longint; Override;
      Procedure SaveToStream(Stream: TStream32);
      Procedure SaveToFile(Const FileName: String);
      Property Memory: Pointer Read fMemory;
   End;

   TMemoryStream32 = Class(TCustomMemoryStream32)
   Private
      fCapacity: Longint;
      Procedure SetCapacity(NewCapacity: Longint);
   Protected
      Function Realloc(Var NewCapacity: Longint): Pointer; Virtual;
      Property Capacity: Longint Read fCapacity Write SetCapacity;
   Public
      Destructor Destroy; Override;
      Procedure Clear;
      Procedure LoadFromStream(Stream: TStream32);
      Procedure LoadFromFile(Const FileName: String);
      Procedure SetSize(NewSize: Longint); Override;
      Function Write(Const Buffer; Count: Longint): Longint; Override;
   End;

Const
   WSIZE = 32768;                       (* window size--must be a power of two, and at least 32k *)
	CUSTOM_BUF_SIZE = WSIZE * 2; //High(Word);

Type
   _alloc_func = Function(opaque: voidpf; Items, Size: u_long): voidpf;
   _free_func = Procedure(opaque: voidpf; address: voidpf);
   _check_func = Function(Check: u_long; Buf: _pBytef; Len: uInt): u_long;

   zuIntArray = Array[0..(MaxMemBlock Div SizeOf(uInt)) - 1] Of u_long; //uInt; v6.1
   PuIntArray = ^zuIntArray;

   inflate_block_mode =
      (ZTYPE,                           { get type bits (3, including end bit) }
      LENS,                             { get lengths for stored }
      STORED,                           { processing stored block }
      TABLE,                            { get table lengths }
      BTREE,                            { get bit lengths tree for a dynamic block }
      dtree,                            { get length, distance trees for a dynamic block }
      CODES,                            { processing fixed or dynamic block }
      DRY,                              { output remaining window bytes }
      BLKDONE,                          { finished last block, done }
      BLKBAD);                          { got a data error--stuck here }

   ppInflate_huft = ^pInflate_huft;
   pInflate_huft = ^inflate_huft;
   inflate_huft = Record
      exop,                             { number of extra bits or operation }
      BITS: Byte;                       { number of bits in this code or subcode }
      {pad : uInt;}                     { pad structure to a power of 2 (4 bytes for }
                                        {  16-bit, 8 bytes for 32-bit int's) }
      BASE: uInt;                       { literal, length base, or distance base }
      { or table offset }
   End;

   inflate_codes_mode = (               { waiting for "i:"=input, "o:"=output, "x:"=nothing }
      Start,                            { x: set up for LEN }
      Len,                              { i: get length/literal/eob next }
      LENEXT,                           { i: getting length extra (have base) }
      dist,                             { i: get distance next }
      DISTEXT,                          { i: getting distance extra }
      zCOPY,                            { o: copying bytes in window, waiting for space }
      lit,                              { o: got literal, waiting for output space }
      WASH,                             { o: got eob, possibly still output waiting }
      zend,                             { x: got eob and all data flushed }
      BADCODE);                         { x: got error }

   pInflate_codes_state = ^inflate_codes_state;
   inflate_codes_state = Record
      mode: inflate_codes_mode;
      Len: uInt;
      sub: Record                       { submode }
         Case Byte Of
            0: (code: Record            { if LEN or DIST, where in tree }
                  tree: pInflate_huft;  { pointer into tree }
                  need: uInt;           { bits needed }
               End);
            1: (lit: uInt);             { if LIT, literal }
            2: (Copy: Record            { if EXT or COPY, where and how much }
                  get: uInt;            { bits to get for extra }
                  dist: uInt;           { distance back to copy from }
               End);
      End;

      lbits: Byte;                      { ltree bits decoded per branch }
      dbits: Byte;                      { dtree bits decoder per branch }
      ltree: pInflate_huft;             { literal/length/eob tree }
      dtree: pInflate_huft;             { distance tree }
   End;

   huft_field = Array[0..(MaxMemBlock Div SizeOf(inflate_huft)) - 1] Of inflate_huft;
   huft_ptr = ^huft_field;

   pInflate_blocks_state = ^inflate_blocks_state;
   inflate_blocks_state =
      Record
      mode: inflate_block_mode;
      sub:
      Record
         Case Byte Of
            0: (left: uInt);            { if STORED, bytes left to copy }
            1: (trees: Record           { if DTREE, decoding info for trees }
                  TABLE: u_long; //uInt; v6.1          { table lengths (14 bits) }
                  Index: u_long; //uInt; v6.1         { Index into blens (or border) }
                  blens: PuIntArray;    { bit lengths of codes }
                  bb: u_long; //uInt;  v6.1           { bit length tree depth }
                  tb: pInflate_huft;    { bit length decoding tree }
               End);
            2: (decode: Record          { if CODES, current state }
                  tl: pInflate_huft;
                  td: pInflate_huft;    { trees to free }
                  CODES: pInflate_codes_state;
               End);
      End;
      last: Boolean;                    { true if this block is the last block }
      bitk: u_long; //uInt; v6.1                      { bits in bit buffer }
      bitb: u_long;                     { bit buffer }
      hufts: huft_ptr;                  { single malloc for tree space }
      window: _pBytef;                  { sliding window }
      zend: _pBytef;                    { one byte after sliding window }
      Read: _pBytef;                    { window read pointer }
      Write: _pBytef;                   { window write pointer }
      CheckFn: _check_func;             { check function }
      Check: u_long;                    { check on output }
   End;

   inflate_mode = (
      method,                           { waiting for method byte }
      FLAG,                             { waiting for flag byte }
      DICT4,                            { four dictionary check bytes to go }
      DICT3,                            { three dictionary check bytes to go }
      DICT2,                            { two dictionary check bytes to go }
      DICT1,                            { one dictionary check byte to go }
      DICT0,                            { waiting for inflateSetDictionary }
      BLOCKS,                           { decompressing blocks }
      CHECK4,                           { four check bytes to go }
      CHECK3,                           { three check bytes to go }
      CHECK2,                           { two check bytes to go }
      CHECK1,                           { one check byte to go }
      DONE,                             { finished check, done }
      zBAD);                            { got an error--stay here }

   pInternal_state = ^internal_state;
   internal_state =
      Record
      mode: inflate_mode;
      sub:
      Record                            { submode }
         Case Byte Of
            0: (method: uInt);          { if FLAGS, method byte }
            1: (Check: Record           { if CHECK, check values to compare }
                  was: u_long;          { computed check value }
                  need: u_long;         { stream check value }
               End);
            2: (marker: uInt);          { if BAD, inflateSync's marker bytes count }
      End;
      nowrap: Boolean;                  { flag for no wrapper }
      wbits: uInt;                      { log2(window Size)  (8..15, defaults to 15) }
      BLOCKS: pInflate_blocks_state;    { current inflate_blocks state }
   End;

   ztv_stream_plus = Packed Record
      CRC: u_long;
      Protect: Boolean;
      pArchivePos: ^Int64;
      pCancel: ^Boolean;
      Eof: Boolean;
   End;

   ztv_streamp = ^ztv_stream;
   ztv_stream = Packed Record
      next_in: _pBytef;                 { next input byte }
      avail_in: uInt;                   { number of bytes available at next_in }
      total_in: Int64; //u_long;        { total nb of input bytes read so far }
      next_out: _pBytef;                { next output byte should be put there }
      avail_out: uInt;                  { remaining free space at next_out }
      total_out: Int64; //u_long;       { total nb of bytes output so far }
      //msg: String[255];                 { last error message, '' if no error }
      state: pInternal_state;           { not visible by applications }
      ZALLOC: _alloc_func;              { used to allocate the internal state }
      ZFREE: _free_func;                { used to free the internal state }
      opaque: voidpf;                   { private data object passed to zalloc and zfree }
      data_type: _int;                  { best guess about the data type: ascii or binary }
      adler: u_long;                    { adler32 value of the uncompressed data }
      Reserved: u_long;                 { reserved for future use }
      cb: ztv_stream_plus;
   End;

   TCustomStream = Class(TStream32)
   Private
   	BSize: Byte;
   Protected
      fProgressPosition: Double;
      // v5.2 revised to allocate/deallocate the required memory only when needed
      //fBuffer: Array[Word]{[0..WSIZE-1]} Of Char;
      fBuffer: Pointer;
   Public
      fStrm: TStream32;
      fStrmPos: Int64;
      FZRec: ztv_stream;
      Constructor Create(strm: TStream32);
      Destructor Destroy; Override;
   End;


   TCompressStream = Class(TCustomStream)
   Private
{$ifdef unitdebug}
   	Debug_TmpPos: Integer;
   	Debug_LocalZipHeader: TLocal;
{$endif}
   Public
      Constructor Create(dest: TStream32; CompressionLevel: TDeflateType; MAX_WBITS: ShortInt);
      Destructor Destroy; Override;
      Procedure doInitialize(dest: TStream32; CompressionLevel: TDeflateType;
         MAX_WBITS: ShortInt); Virtual;
      Procedure doCompStream; Virtual;
      Procedure doCompEnd; Virtual;
      Function Read(Var Buffer; Count: longint): longint; Override;
      Function Write(Const Buffer; Count: longint): longint; Override;
		Function Seek(Offset: longint; Origin: Word): longint; OverLoad; Override;
		Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Overload; Override;
   End;

   TDecompressStream = Class(TCustomStream)
   Private
      Procedure doInflateReset(Var z: ztv_stream); Virtual;
      Procedure doDecompEnd(Var z: ztv_stream); Virtual;
      Function doInflate(Var z: ztv_stream; f: _int): _int; Virtual;
      Procedure doInitialize(z: ztv_streamp; stream_size: _int;
         DEF_WBITS: ShortInt); Virtual;
   Protected
   Public
      Constructor Create(source: TStream32; DEF_WBITS: ShortInt);
      Destructor Destroy; Override;
      Function Read(Var Buffer; Count: longint): longint; Override;
      Function Write(Const Buffer; Count: longint): longint; Override;
      Function Seek(Offset: longint; Origin: Word): longint; Overload; Override;
		Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Overload; Override;
      Property OnProgress;
   End;

   TCalcStreamCRC = Class(TCustomStream)
   Private
   Public
      CRC: u_long;
      Constructor Create(inStream: TStream32; Len: Int64; BitSize: Byte;
			zsp: ztv_stream_plus; Cancel_CallBackProc: Pointer;
         Progress_CallBackProc: TNotifyEvent);
      Destructor Destroy; Override;
      Function Write(Const Buf; iCount: longint): longint; Override;
      Function Read(Var Buffer; iCount: longint): longint; Override;
		Function Seek(Offset: longint; Origin: Word): longint; Override;
   End;

   TEncryptStream = Class(TMemoryStream32)
   Private
      fStrm: TStream32;
   Public
      CRC: u_long;
      CryptHDR: String[RAND_HEAD_LEN];
      Constructor Create(Var inStream, dest: TStream32; Len: Int64;
      	Password: String; WriteHeader: Boolean);
      Destructor Destroy; Override;
      Function Write(Const Buffer; Count: longint): longint; Override;
   End;

   TDecryptStream = Class(TMemoryStream32)
   Private
      fPassword: String;
      fStrm: TStream32;
   Public
      Constructor Create(Var inStream, dest: TStream32; Password: String);
      Destructor Destroy; Override;
      Function Write(Const Buffer; Count: longint): longint; Override;
   End;

   TDecryptStreamCRC = Class(TMemoryStream32)
   Private
      fPassword: String;
      fCryptHDR: String[RAND_HEAD_LEN * 2];
      fHeadRead: Boolean;
      fStrm: TStream32;
   Public
      Constructor Create(Var inStream, dest: TStream32; Password: String;
         CRC: u_long);
      Destructor Destroy; Override;
      Function Write(Const Buffer; Count: longint): longint; Override;
   End;

   TStoreStream = Class(TCustomStream)
   Private
      pInStream: ^TStream32;
   	Encrypted: Boolean;
   Public
		Constructor Create(dest: TStream32; BitSize: Byte; zsp: ztv_stream_plus;
         CancelCallBackProc: Pointer; ProgressCallBackProc: TNotifyEvent);
      Destructor Destroy; Override;
      Function Read(Var Buffer; iCount: longint): longint; Override;
      Function Write(Const Buffer; iCount: longint): longint; Override;
		Function Seek(Offset: longint; Origin: Word): longint; OverLoad; Override;
		Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Overload; Override;
   	Function CopyStream(inStream: TStream32): Int64;
    	Function CopyFrom(Source: TStream32; Count: Int64): Int64; Virtual;
      //Function CopyFrom(Source: TStream32; Count: Int64; CancelCallBackProc:
      //	Pointer; ProgressCallBackProc: TNotifyEvent): Int64; Override;
      Property OnProgress;
   End;


Function _adler(a: u_long; Buf: _pBytef; Len: uInt): u_long;
Function CalcFileCRC(f: THandle; Len: Int64; zsp: ztv_stream_plus; CancelCallBackProc:
	Pointer; ProgressCallBackProc: TNotifyEvent): u_long;
Function CalcStreamCRC16(strm16: TStream32; Len: Int64; zsp:
	ztv_stream_plus): u_long;
Function CalcStreamCRC32(strm32: TStream32; Len: Int64; zsp:
	ztv_stream_plus; CancelCallBackProc: Pointer; ProgressCallBackProc:
   TNotifyEvent): u_long;

Procedure ZTVFreeMem(AppData, Block: Pointer);
Procedure TStreamToZTVStream(Source: TStream; Dest: TStream32);
Procedure ZTVStreamToTStream(Source: TStream32; Dest: TStream; Len: Int64);
Function ZTVAllocMem(AppData: Pointer; Items, Size: u_long): Pointer;


Implementation

Uses
   Dialogs,
   Forms,
   Consts,
   TypInfo,
   ZTVBase,
   ZTVGbls,
   ZTVCrypt,
   ZTVDeflate,
   ZTVInflate,
   ERR_MSGS;

Const
   MaxBufSize =  $F000;

//-------------------------------------------------------------

Constructor THandleStream32.Create(AHandle: Integer);
Begin
   //Inherited Create;                    // v4.1.10 added
   fHandle := AHandle;
   CancelCallBackProc := Nil;
   ProgressCallBackProc := Nil;
End;
//-------------------------------------------------------------

Destructor THandleStream32.Destroy;
Begin
	fHandle := -1;
	//Inherited Destroy();
End;
//-------------------------------------------------------------

Function THandleStream32.Read(Var Buffer; Count: Longint): Longint;
Begin
  	Result := FileRead(fHandle, Buffer, Count);
  	If Result = -1 Then
   	Result := 0;
End;
//-------------------------------------------------------------

Function THandleStream32.Write(Const Buffer; Count: Longint): Longint;
Begin
  	If Not WriteFile(THandle(fHandle), Buffer, Count, LongWord(Result), Nil) Then
   	Result := 0;
End;
//-------------------------------------------------------------

Function THandleStream32.GetSize: Int64;
Var
   dwSizeHigh: DWord;
Begin
   Result := Windows.GetFileSize(fHandle, @dwSizeHigh);
   If (Result = $FFFFFFFF) And (GetLastError <> NO_ERROR) Then
      Result := 0
   Else
		Result  := (Int64(dwSizeHigh) Shl 32) Or Result;

End;
//-------------------------------------------------------------

Function THandleStream32.Seek(Offset: Integer; Origin: Word): Integer;
Begin
   Result := FileSeek(fHandle, Offset, Origin);
End;
//-------------------------------------------------------------

// Note: Use FileSeek instead of SetFilePointer unless the value of dwSizeHigh
// is known.    FileSeek effienciently handles 4gig+ files.
Function THandleStream32.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Begin
   Result := FileSeek(fHandle, Offset, Ord(Origin));
End;
//-------------------------------------------------------------

Procedure THandleStream32.SetSize(NewSize: Longint);
Begin
   SetSize(Int64(NewSize));
End;
//-------------------------------------------------------------

Procedure THandleStream32.SetSize(Const NewSize: Int64);
Begin
   Seek(NewSize, soBeginning);
   Win32Check(SetEndOfFile(FHandle));
End;
//-------------------------------------------------------------

{ TFileStream32 }
Constructor TFileStream32.Create(Const FileName: String; Mode: Word);
Const
	fOpenErr = -1;
   fCreateErr = -2;
Begin
   CancelCallBackProc := Nil;
   ProgressCallBackProc := Nil;

   //fHandle := -1;
   If Mode = fmCreate Then
   Begin
    	fHandle := FileCreate(FileName);
      //Inherited Create(FileCreate(FileName));
// rem'd v4.6.1 and installed variable ResultCode
//      If fHandle < 0 Then
//{$ifndef DEL5_OR_HIGHER}
//         Raise EFCreateError.CreateFmt(SFCreateError, [FileName]);
//{$else}
//         Raise EFCreateError.CreateResFmt(@SFCreateError, [FileName]);
//{$endif}
   End
   Else
   Begin
    	fHandle := FileOpen(FileName, Mode);
   	//Inherited Create(FileOpen(FileName, Mode));
// rem'd v4.6.1 and installed variable ResultCode
//      If fHandle < 0 Then
//{$ifndef DEL5_OR_HIGHER}
//         Raise EFOpenError.CreateFmt(SFOpenError, [FileName]);
//{$else}
//         Raise EFOpenError.CreateResFmt(@SFOpenError, [FileName]);
//{$endif}
   End;
End;
//-------------------------------------------------------------

Destructor TFileStream32.Destroy;
Begin
   If Handle >= 0 Then
      If CloseHandle(THandle(fHandle)) Then
      	fHandle := -1;
   //Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TStreamToZTVStream(Source: TStream; Dest: TStream32);
Var
   Buffer: PChar;
	Count: Int64;
   BufSize,	n: Integer;
Begin
	Count := Source.Size - Source.Position;
   If Count > MaxBufSize Then
      BufSize := MaxBufSize
   Else
      BufSize := Count;

   //Source.Position := 0;

   GetMem(Buffer, BufSize);
   Try
      While (Count <> 0) Do // And (Not pCancel^)
      Begin
         If Count > BufSize Then
            n := BufSize
         Else
            n := Count;

         Application.ProcessMessages();
         Source.ReadBuffer(Buffer^, n);
         Dest.WriteBuffer(Buffer^, n);

         Dec(Count, n);
      End;
   Finally
      Dest.Position := 0;
      FreeMem(Buffer, BufSize);
   End;
End;
//-------------------------------------------------------------

// convert TStream32 to a Delphi compatible TStream
Procedure ZTVStreamToTStream(Source: TStream32; Dest: TStream; Len: Int64);
Var
   Buffer: PChar;
   BufSize,
   	n: Integer;
Begin
   If Len > (Source.Size - Source.Position) Then
   	Len := Source.Size - Source.Position;

   If Len > MaxBufSize Then
      BufSize := MaxBufSize
   Else
      BufSize := Len;

   GetMem(Buffer, BufSize);
   Try
      While (Len <> 0) Do // And (Not pCancel^)
      Begin
         If Len > BufSize Then
            n := BufSize
         Else
            n := Len;

         Application.ProcessMessages();
         Source.ReadBuffer(Buffer^, n);
         Dest.WriteBuffer(Buffer^, n);
         Dec(Len, n);
      End;
   Finally
   	Dest.Position := 0;
      FreeMem(Buffer, BufSize);
   End;
End;
//-------------------------------------------------------------

Function _adler(a: u_long; Buf: _pBytef; Len: uInt): u_long;
Var
   k: _int;
   s1, s2: u_long;
Begin
   If Not Assigned(Buf) Then
   Begin
      Result := u_long(1);
      Exit;
   End;

   s1 := a And $FFFF;
   s2 := (a Shr 16) And $FFFF;

   While (Len > 0) Do
   Begin
      If Len < NMAX Then
         k := Len
      Else
         k := NMAX;

      Dec(Len, k);

      While (k > 0) Do
      Begin
         Inc(s1, Buf^);
         Inc(s2, s1);
         Inc(Buf);
         Dec(k);
      End;

      s1 := s1 Mod BASE;
      s2 := s2 Mod BASE;
   End;
   Result := (s2 Shl 16) Or s1;
End;
//-------------------------------------------------------------

Function ZTVAllocMem(AppData: Pointer; Items, Size: u_long): Pointer;
Begin
   Result := AllocMem(Items * Size);  //GetMem(Result, Items * Size);
End;
//-------------------------------------------------------------

Procedure ZTVFreeMem(AppData, Block: Pointer);
Begin
   FreeMem(Block);
End;
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Function TStream32.GetPosition: Int64;
Begin
   Result := Seek(0, soCurrent);
End;
{$endif}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.SetPosition(Const Pos: Int64);
Begin
   Seek(Pos, soBeginning);
End;
{$endif}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Function TStream32.GetSize: Int64;
Var
   Pos: Int64;
Begin
   Pos := Seek(0, soCurrent);
   Result := Seek(0, soEnd);
   Seek(Pos, soBeginning);
End;
{$endif}
//-------------------------------------------------------------

Procedure TStream32.Progress(Sender: TObject);
Begin
   If Assigned(TStream32(Sender).OnProgress) Then
      TStream32(Sender).OnProgress(Sender);
End;
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.SetSize(NewSize: Longint);
Begin
  // default = do nothing  (read-only streams, etc)
End;
{$endif}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.SetSize(Const NewSize: Int64);
Begin
{ For compatibility with old stream implementations, this new 64 bit SetSize
  calls the old 32 bit SetSize.  Descendent classes that override this
  64 bit SetSize MUST NOT call inherited. Descendent classes that implement
  64 bit SetSize should reimplement their 32 bit SetSize to call their 64 bit
  version.}
   If (NewSize < Low(Longint)) Or (NewSize > High(Longint)) Then
{$ifndef DEL5_OR_HIGHER}
      Raise ERangeError.CreateFmt(SRangeError, []);
{$else}
      Raise ERangeError.CreateRes(@SRangeError);
{$endif}

   SetSize(Longint(NewSize));
End;
{$endif}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.SetSize64(Const NewSize: Int64);
Begin
   SetSize(NewSize);
End;
{$endif}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Function TStream32.Seek(Offset: Longint; Origin: Word): Longint;

   Procedure RaiseException;
   Begin
{$ifndef DEL5_OR_HIGHER}
      Raise EStreamError.CreateFmt(sSeekNotImplemented, [Classname]);
{$else}
      Raise EStreamError.CreateResFmt(@sSeekNotImplemented, [Classname]);
{$endif}
   End;

Type
   TSeek64 = Function(Const Offset: Int64; Origin: TSeekOrigin): Int64 Of Object;
Var
   Impl: TSeek64;
   Base: TSeek64;
   ClassTStream: TClass;
Begin
  //Deflect 32 seek requests to the 64 bit seek, if 64 bit is implemented.
  //No existing TStream32 classes should call this method, since it was originally
  //abstract.  Descendent classes MUST implement at least one of either
  //the 32 bit or the 64 bit version, and must not call the inherited
  //default implementation.
   Impl := Seek;
   ClassTStream := Self.ClassType;
   While (ClassTStream <> Nil) And (ClassTStream <> TStream32) Do
      ClassTStream := ClassTStream.ClassParent;
   If ClassTStream = Nil Then RaiseException;
   Base := TStream32(@ClassTStream).Seek;
   If TMethod(Impl).Code = TMethod(Base).Code Then
      RaiseException;
   Result := Seek(Int64(Offset), TSeekOrigin(Origin));
End;
{$endif}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Function TStream32.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Begin
{ Default implementation of 64 bit seek is to deflect to existing 32 bit seek.
  Descendents that override 64 bit seek must not call this default implementation. }
   If (Offset < Low(Longint)) Or (Offset > MAXDWORD{High(Longint)}) Then
{$ifndef DEL5_OR_HIGHER}
      Raise ERangeError.CreateFmt(SRangeError, []);
{$else}
      Raise ERangeError.CreateRes(@SRangeError);
{$endif}
   Result := Seek(Longint(Offset), Ord(Origin));
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.ReadBuffer(Var Buffer; Count: Longint);
Begin
   If (Count <> 0) And (Read(Buffer, Count) <> Count) Then
{$ifndef DEL5_OR_HIGHER}
      Raise EReadError.CreateFmt(SReadError, []);
{$else}
      Raise EReadError.CreateRes(@SReadError);
{$endif}

End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.WriteBuffer(Const Buffer; Count: Longint);
Var
	ECode: DWord;
Begin
   If (Count <> 0) And (Write(Buffer, Count) <> Count) Then
   Begin
   	ECode := GetLastError();
   	If ECode = ERROR_DISK_FULL Then
      	Raise EWriteError.Create(LoadStr(E_DISKFULL))
      Else
{$ifndef DEL5_OR_HIGHER}
      	Raise EWriteError.CreateFmt(SWriteError, []);
{$else}
      	Raise EWriteError.CreateRes(@SWriteError);
{$endif}
	End;
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

Function TStream32.CopyFrom(Source: TStream32; Count: Int64): Int64;
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

   // if pCancel = true, then assign the stream objects pCancel variable to
   // the current objects cancel variable to allow a process cancelation.
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
         If pCancel^ Then Break;

         Source.ReadBuffer(Buffer^, n);	//Source.Read(Buffer^, n);
         Try
         	WriteBuffer(Buffer^, n);	//Write(Buffer^, n);
         Except
         	Raise;
         End;
         Dec(Count, n);
      End;
   Finally
      FreeMem(Buffer, BufSize);
   End;
End;
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Function TStream32.ReadComponent(Instance: TComponent): TComponent;
Var
   Reader: TReader;
Begin
   Reader := TReader.Create(TStream(Self), 4096);
   Try
      Result := Reader.ReadRootComponent(Instance);
   Finally
      Reader.Free();
   End;
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.WriteComponent(Instance: TComponent);
Begin
   WriteDescendent(Instance, Nil);
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.WriteDescendent(Instance, Ancestor: TComponent);
Var
   Writer: TWriter;
Begin
   Writer := TWriter.Create(TStream(Self), 4096);
   Try
      Writer.WriteDescendent(Instance, Ancestor);
   Finally
      Writer.Free();
   End;
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Function TStream32.ReadComponentRes(Instance: TComponent): TComponent;
Begin
   ReadResHeader;
   Result := ReadComponent(Instance);
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.WriteComponentRes(Const ResName: String; Instance: TComponent);
Begin
   WriteDescendentRes(ResName, Instance, Nil);
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.WriteResourceHeader(Const ResName: String; Out FixupInfo: Integer);
Var
   HeaderSize: Integer;
   Header: Array[0..79] Of Char;
Begin
   Byte((@Header[0])^) := $FF;
   Word((@Header[1])^) := 10;
   HeaderSize := StrLen(StrUpper(StrPLCopy(@Header[3], ResName, 63))) + 10;
   Word((@Header[HeaderSize - 6])^) := $1030;
   Longint((@Header[HeaderSize - 4])^) := 0;
   WriteBuffer(Header, HeaderSize);
   FixupInfo := Position;
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.FixupResourceHeader(FixupInfo: Integer);
Var
   ImageSize: Integer;
Begin
   ImageSize := Position - FixupInfo;
   Position := FixupInfo - 4;
   WriteBuffer(ImageSize, SizeOf(Longint));
   Position := FixupInfo + ImageSize;
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.WriteDescendentRes(Const ResName: String; Instance,
   Ancestor: TComponent);
Var
   FixupInfo: Integer;
Begin
   WriteResourceHeader(ResName, FixupInfo);
   WriteDescendent(Instance, Ancestor);
   FixupResourceHeader(FixupInfo);
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------

{$ifndef DEL6_OR_HIGHER}
Procedure TStream32.ReadResHeader;
Var
   ReadCount: Cardinal;
   Header: Array[0..79] Of Char;
Begin
   FillChar(Header, SizeOf(Header), 0);
   ReadCount := Read(Header, SizeOf(Header) - 1);
   If (Byte((@Header[0])^) = $FF) And (Word((@Header[1])^) = 10) Then
      Seek(StrLen(Header + 3) + 10 - ReadCount, 1)
   Else
{$ifndef DEL5_OR_HIGHER}
      Raise EInvalidImage.CreateFmt(SInvalidImage, []);
{$else}
      Raise EInvalidImage.CreateRes(@SInvalidImage);
{$endif}
End;
{$endif DEL6_OR_HIGHER}
//-------------------------------------------------------------


{ TCustomMemoryStream32 }

Procedure TCustomMemoryStream32.SetPointer(Ptr: Pointer; Size: Longint);
Begin
   fMemory := Ptr;
   fSize := Size;
End;
//-------------------------------------------------------------

Function TCustomMemoryStream32.Read(Var Buffer; Count: Longint): Longint;
Begin
   If (fPosition >= 0) And (Count >= 0) Then
   Begin
      Result := fSize - fPosition;
      If Result > 0 Then
      Begin
         If Result > Count Then Result := Count;
         Move(Pointer(Longint(fMemory) + fPosition)^, Buffer, Result);
         Inc(fPosition, Result);
         Exit;
      End;
   End;
   Result := 0;
End;
//-------------------------------------------------------------

Function TCustomMemoryStream32.Seek(Offset: Longint; Origin: Word): Longint;
Begin
   Case Origin Of
      soFromBeginning: fPosition := Offset;
      soFromCurrent: Inc(fPosition, Offset);
      soFromEnd: fPosition := fSize + Offset;
   End;
   Result := fPosition;
End;
//-------------------------------------------------------------

Procedure TCustomMemoryStream32.SaveToStream;
Begin
   If fSize <> 0 Then
   	Stream.WriteBuffer(fMemory^, fSize);
End;
//-------------------------------------------------------------

Procedure TCustomMemoryStream32.SaveToFile(Const FileName: String);
Var
   Strm: TFileStream32;
Begin
   Strm := TFileStream32.Create(FileName, fmCreate);
   Try
   	If (Strm.Handle < 0) Then Exit;
      SaveToStream(Strm);
   Finally
      Strm.Free();
   End;
End;
//-------------------------------------------------------------

{ TMemoryStream32 }
Const
   MemoryDelta = $2000;                 { Must be a power of 2 }

Destructor TMemoryStream32.Destroy;
Begin
   Clear();
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.Clear;
Begin
   SetCapacity(0);
   fSize := 0;
   fPosition := 0;
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.LoadFromStream;
Var
   Count: Longint;
Begin
   Stream.Position := 0;
   Count := Stream.Size;
   SetSize(Count);
   If Count <> 0 Then Stream.ReadBuffer(fMemory^, Count);
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.LoadFromFile(Const FileName: String);
Var
   Strm: TFileStream32;
Begin
   Strm := TFileStream32.Create(FileName, fmOpenRead Or fmShareDenyWrite);
   Try
   	If (Strm.Handle < 0) Then Exit;
      LoadFromStream(Strm);
   Finally
      Strm.Free();
   End;
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.SetCapacity(NewCapacity: Longint);
Begin
   SetPointer(Realloc(NewCapacity), fSize);
   fCapacity := NewCapacity;
End;
//-------------------------------------------------------------

Procedure TMemoryStream32.SetSize(NewSize: Longint);
Var
   OldPosition: Longint;
Begin
   OldPosition := fPosition;
   SetCapacity(NewSize);
   fSize := NewSize;
   If OldPosition > NewSize Then
   	Seek(0, soFromEnd);
End;
//-------------------------------------------------------------
               
Function TMemoryStream32.Realloc(Var NewCapacity: Longint): Pointer;
Begin
   If (NewCapacity > 0) And (NewCapacity <> fSize) Then
      NewCapacity := (NewCapacity + (MemoryDelta - 1)) And Not (MemoryDelta - 1);
   Result := Memory;
   If NewCapacity <> fCapacity Then
   Begin
      If NewCapacity = 0 Then
      Begin
{$ifdef MSWINDOWS}
         GlobalFreePtr(Memory);
{$else}
         FreeMem(Memory);
{$endif}
         Result := Nil;
      End
      Else
      Begin
{$ifdef MSWINDOWS}
         If Capacity = 0 Then
            Result := GlobalAllocPtr(HeapAllocFlags, NewCapacity)
         Else
            Result := GlobalReallocPtr(Memory, NewCapacity, HeapAllocFlags);
{$else}
         If Capacity = 0 Then
            GetMem(Result, NewCapacity)
         Else
            ReallocMem(Result, NewCapacity);
{$endif}
         If Result = Nil Then
{$ifndef DEL5_OR_HIGHER}
            Raise EStreamError.CreateFmt(SMemoryStreamError, []);
{$else}
            Raise EStreamError.CreateRes(@SMemoryStreamError);
{$endif}

      End;
   End;
End;
//-------------------------------------------------------------

Function TMemoryStream32.Write(Const Buffer; Count: Longint): Longint;
Var
   Pos: Longint;
Begin
   If (fPosition >= 0) And (Count >= 0) Then
   Begin
      Pos := fPosition + Count;
      If Pos > 0 Then
      Begin
         If Pos > fSize Then
         Begin
            If Pos > fCapacity Then
               SetCapacity(Pos);
            fSize := Pos;
         End;
         System.Move(Buffer, Pointer(Longint(fMemory) + fPosition)^, Count);
         fPosition := Pos;
         Result := Count;
         Exit;
      End;
   End;
   Result := 0;
End;
//-------------------------------------------------------------
{.$endif}


(*************************************************************)
(*************************************************************)
(*                      TCustomStream                        *)
(*************************************************************)
(*************************************************************)

Constructor TCustomStream.Create(strm: TStream32);
Begin
   Inherited Create;
   fStrm := strm;
   fStrmPos := strm.Position;
   FZRec.ZALLOC := ZTVAllocMem;
   FZRec.ZFREE := ZTVFreeMem;
   GetMem(fBuffer, CUSTOM_BUF_SIZE);
End;
//-------------------------------------------------------------

Destructor TCustomStream.Destroy;
Begin
   FreeMem(fBuffer, CUSTOM_BUF_SIZE);
	Inherited Destroy;
End;


(*************************************************************)
(*************************************************************)
(*                      TCompressStream                      *)
(*************************************************************)
(*************************************************************)

Constructor TCompressStream.Create(dest: TStream32; CompressionLevel: TDeflateType;
   MAX_WBITS: ShortInt);
Begin
   Inherited Create(dest);
   FZRec.next_out := fBuffer;
   FZRec.avail_out := CUSTOM_BUF_SIZE;
   FZRec.cb.CRC := CRC_MASK;
   doInitialize(dest, CompressionLevel, MAX_WBITS);
End;
//-------------------------------------------------------------

Destructor TCompressStream.Destroy;
Begin
   FZRec.next_in := Nil;
   FZRec.avail_in := 0;
   doCompEnd();
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TCompressStream.Read(Var Buffer; Count: longint): longint;
Begin
   //ShowMessage( 'Error: InvalidStreamOp' );
   Result := -1;
End;
//-------------------------------------------------------------

Function TCompressStream.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Begin
{ Default implementation of 64 bit seek is to deflect to existing 32 bit seek.
  Descendents that override 64 bit seek must not call this default implementation. }
   If (Offset < Low(Longint)) Or (Offset > High(Longint)) Then
{$ifndef DEL5_OR_HIGHER}
      Raise ERangeError.CreateFmt(SRangeError, []);
{$else}
      Raise ERangeError.CreateRes(@SRangeError);
{$endif}

   //Result := Seek(Longint(Offset), Ord(Origin));
   If (Offset = 0) And (Origin = soCurrent) Then
      Result := FZRec.total_in
   Else
      Result := -1;
   //ShowMessage( 'Error: sInvalidStreamOp' );

End;
//-------------------------------------------------------------

Function TCompressStream.Seek(Offset: longint; Origin: Word): longint;
Begin
   If (Offset = 0) And (Origin = soFromCurrent) Then
      Result := FZRec.total_in
   Else
      Result := -1;
End;
//-------------------------------------------------------------

Function TCompressStream.Write(Const Buffer; Count: longint): longint;
Begin
   FZRec.next_in := @Buffer;
   FZRec.avail_in := Count;
   If (fStrm.Position <> fStrmPos) Then
      fStrm.Position := fStrmPos;

   doCompStream();
   Result := Count;
End;
//-------------------------------------------------------------

Procedure TCompressStream.doCompStream;
Begin
   While (FZRec.avail_in > 0) Do
   Begin

      If FZRec.cb.pCancel^ Then break;
      FZRec.cb.pArchivePos^ := FZRec.cb.pArchivePos^ - FZRec.avail_in;

      //CCheck(_deflate(FZRec, 0));
      _deflate(FZRec, 0);               // v4.1.7

      If FZRec.cb.pCancel^ Then break;

      If (FZRec.avail_out = 0) Then
      Begin
         FZRec.cb.pArchivePos^ := FZRec.cb.pArchivePos^ + FZRec.avail_in;

         If FZRec.cb.Protect Then
            ZTVEncodeBuf(fBuffer, CUSTOM_BUF_SIZE);

         fStrm.WriteBuffer(fBuffer^, CUSTOM_BUF_SIZE);

         FZRec.next_out := fBuffer;
         FZRec.avail_out := CUSTOM_BUF_SIZE;
         fStrmPos := fStrm.Position;
      End;

      Progress(Self);
   End;
End;
//-------------------------------------------------------------

Procedure TCompressStream.doInitialize(dest: TStream32; CompressionLevel: TDeflateType;
   MAX_WBITS: ShortInt);
Begin
{$IFDEF DEFLATE64}
   deflateInit2(FZRec, ZLevels[CompressionLevel], Z_DEFLATE64, MAX_WBITS, DEF_MEM_LEVEL, 0); // v4.1.7
{$ELSE}
   //CCheck(deflateInit2(FZRec, ZLevels[CompressionLevel], Z_DEFLATED, MAX_WBITS, DEF_MEM_LEVEL, 0));
   deflateInit2(FZRec, ZLevels[CompressionLevel], Z_DEFLATED, MAX_WBITS, DEF_MEM_LEVEL, 0); // v4.1.7
{$ENDIF}
End;
//-------------------------------------------------------------

Procedure TCompressStream.doCompEnd;
Var
   code: _int;
Begin
   Try
      If fStrm.Position <> fStrmPos Then
         fStrm.Position := fStrmPos;

      code := _deflate(FZRec, Z_FINISH);

      //While (CCheck(code) <> Z_STREAM_END)
      While (code <> Z_STREAM_END)      // v 4.1.7
      And (FZRec.avail_out = 0) And (Not FZRec.cb.pCancel^) Do
      Begin

         If (code = Z_FINISH) Or (code = Z_OK) Then
         Begin
            If FZRec.cb.Protect Then
               ZTVEncodeBuf(fBuffer, CUSTOM_BUF_SIZE);

            fStrm.WriteBuffer(fBuffer^, CUSTOM_BUF_SIZE);

            FZRec.next_out := fBuffer;
            FZRec.avail_out := CUSTOM_BUF_SIZE;
         End
         Else
            Exit;

         code := _deflate(FZRec, Z_FINISH);
      End;

      If (FZRec.avail_out < CUSTOM_BUF_SIZE) Then
      Begin
         If FZRec.cb.Protect Then
            ZTVEncodeBuf(fBuffer, CUSTOM_BUF_SIZE - FZRec.avail_out);

{$ifdef unitdebug}
         If fStrm.Size > 1000 Then
         Begin
            Debug_TmpPos := fStrm.Position;
            Try
               fStrm.Position := 0;
               ZeroMemory(@LocalZipHeader, SizeOf(TLocal));
               fStrm.ReadBuffer(LocalZipHeader, SizeOf(TLocal));
            Finally
               fStrm.Position := Debug_TmpPos;
            End;
         End;
{$endif}

         fStrm.WriteBuffer(fBuffer^, CUSTOM_BUF_SIZE - FZRec.avail_out);

{$ifdef unitdebug}
         // after
         If fStrm.Size > 1000 Then
         Begin
            Debug_TmpPos := fStrm.Position;
            Try
               fStrm.Position := 0;
               ZeroMemory(@Debug_LocalZipHeader, SizeOf(TLocal));
               fStrm.ReadBuffer(Debug_LocalZipHeader, SizeOf(TLocal));
            Finally
               fStrm.Position := Debug_TmpPos;
            End;
         End;
{$endif}
      End;
   Finally
      _deflateEnd(FZRec);
   End;
End;
//-------------------------------------------------------------


(*************************************************************)
(*************************************************************)
(*                      TDecompressStream                    *)
(*************************************************************)
(*************************************************************)

Constructor TDecompressStream.Create(source: TStream32; DEF_WBITS: ShortInt);
Begin
   Inherited Create(source);
   FZRec.next_in := fBuffer;
   FZRec.avail_in := 0;
   doInitialize(@FZRec, SizeOf(FZRec), DEF_WBITS);
End;
//-------------------------------------------------------------

Destructor TDecompressStream.Destroy;
Begin
	fStrm.Seek(-FZRec.avail_in, 1);
   doDecompEnd(FZRec);
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TDecompressStream.Read(Var Buffer; Count: longint): longint;
Var
   R: _int;
Begin
   FZRec.next_out := @Buffer;
   FZRec.avail_out := Count;

   If fStrm.Position <> fStrmPos Then
      fStrm.Position := fStrmPos;

   While (FZRec.avail_out > 0) Do
   Begin
      If FZRec.avail_in = 0 Then
      Begin
         FZRec.avail_in := fStrm.Read(fBuffer^, Count{CUSTOM_BUF_SIZE});

         // ------------------------------------------------------------
         // DO NOT USE THE FOLLOWING BLOCK!  If an archive's eof marker
         // immediately follows the compressed data, this function fails
         // using this block check.
         // ------------------------------------------------------------
         // FZRec.avail_in := fStrm.Read(fBuffer^, Count);
         // If FZRec.avail_in = 0 Then
         // Begin
         //    Result := Count - LongInt( FZRec.avail_out );
         //    Exit;
         // End;
         // ------------------------------------------------------------

         FZRec.next_in := fBuffer;
         fStrmPos := fStrm.Position;

         If (FZRec.avail_in > 0) Then
         Begin
            If FZRec.cb.Protect Then
               ZTVDecodeBuf(FZRec.next_in, FZRec.avail_in);
            Progress(Self);
         End;
      End;

      R := doInflate(FZRec, 0);

      If R = Z_STREAM_END Then          // 051601: added for compatibility with TZipKey
      Begin
      	FZRec.cb.EOF := True;
         break
      End Else
         If R < 0 Then                // ( r = Z_DATA_ERROR ) Or ( r = Z_STREAM_ERROR ) Then
         Begin
            Count := R;
            break;
         End;
   End;
   Result := Count;
End;
//-------------------------------------------------------------

Function TDecompressStream.Write(Const Buffer; Count: longint): longint;
Begin
   //Raise EDecompressionError.Create( sInvalidStreamOp );
   Result := -1;
End;
//-------------------------------------------------------------

Function TDecompressStream.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Var
   i: Integer;
   Buf: Array[0..4095] Of char;
   LocalOffset: Int64;
Begin

	LocalOffset := Offset;
{ Default implementation of 64 bit seek is to deflect to existing 32 bit seek.
  Descendents that override 64 bit seek must not call this default implementation. }
   If (Offset < Low(Longint)) Or (Offset > High(Longint)) Then
{$ifndef DEL5_OR_HIGHER}
      Raise ERangeError.CreateFmt(SRangeError, []);
{$else}
      Raise ERangeError.CreateRes(@SRangeError);
{$endif}

   //Result := Seek(Longint(Offset), Ord(Origin));
   If (LocalOffset = 0) And (Origin = soBeginning) Then
   Begin
      doInflateReset(FZRec);
      FZRec.next_in := fBuffer;
      FZRec.avail_in := 0;
      fStrm.Position := 0;
      fStrmPos := 0;
   End Else
      If ((LocalOffset >= 0) And (Origin = soCurrent)) Or
         (((LocalOffset - longint(FZRec.total_out)) > 0) And (Origin = soBeginning)) Then
      Begin
         If Origin = soBeginning Then
         	LocalOffset := LocalOffset - FZRec.total_out;

         If LocalOffset > 0 Then
         Begin
            For i := 1 To LocalOffset Div SizeOf(Buf) Do
               ReadBuffer(Buf, SizeOf(Buf));

            ReadBuffer(Buf, LocalOffset Mod SizeOf(Buf));
         End;
      End
      Else
      	; //Raise EDecompressionError.Create( sInvalidStreamOp );

   Result := FZRec.total_out;
End;
//-------------------------------------------------------------

Function TDecompressStream.Seek(Offset: longint; Origin: Word): longint;
Var
   i: Integer;
   Buf: Array[0..4095] Of char;
Begin
   If (Offset = 0) And (Origin = soFromBeginning) Then
   Begin
      doInflateReset(FZRec);
      FZRec.next_in := fBuffer;
      FZRec.avail_in := 0;
      fStrm.Position := 0;
      fStrmPos := 0;
   End
   Else If ((Offset >= 0) And (Origin = soFromCurrent)) Or
      (((Offset - longint(FZRec.total_out)) > 0) And (Origin = soFromBeginning)) Then
   Begin
      If Origin = soFromBeginning Then Dec(Offset, FZRec.total_out);
      If Offset > 0 Then
      Begin
         For i := 1 To Offset Div SizeOf(Buf) Do
            ReadBuffer(Buf, SizeOf(Buf));

         ReadBuffer(Buf, Offset Mod SizeOf(Buf));
      End;
   End
   Else
      ;                                 //Raise EDecompressionError.Create( sInvalidStreamOp );

   Result := FZRec.total_out;
End;
//-------------------------------------------------------------

Procedure TDecompressStream.doInitialize(z: ztv_streamp; stream_size: _int;
   DEF_WBITS: ShortInt);
Begin
   //CCheck(inflateInit_(@FZRec, DEF_WBITS, SizeOf(FZRec)));
   inflateInit_(@FZRec, DEF_WBITS, SizeOf(FZRec)); // v4.1.7
End;
//-------------------------------------------------------------

Procedure TDecompressStream.doDecompEnd(Var z: ztv_stream);
Begin
   inflateEnd(z);
End;
//-------------------------------------------------------------

Function TDecompressStream.doInflate(Var z: ztv_stream; f: _int): _int;
Begin
   //Result := CCheck(_inflate(FZRec, 0));
   //Result := _inflate(FZRec, 0);        // v4.1.7
   Result := _inflate(z, 0);        // v6.1
End;
//-------------------------------------------------------------

Procedure TDecompressStream.doInflateReset(Var z: ztv_stream);
Begin
   //CCheck(inflateReset(z));
   inflateReset(z);                     // v4.1.7
End;


(*************************************************************)
(*************************************************************)
(*                        TStoreStream                       *)
(*************************************************************)
(*************************************************************)

Destructor TStoreStream.Destroy;
Begin
   FZRec.next_in := Nil;
   FZRec.avail_in := 0;
   Inherited Destroy;
End;
//-------------------------------------------------------------

Constructor TStoreStream.Create(dest: TStream32; BitSize: Byte;
	zsp: ztv_stream_plus; CancelCallBackProc: Pointer;
   ProgressCallBackProc: TNotifyEvent);
Begin
   Inherited Create(dest);
   fStrm := dest;
   BSize := BitSize;
   pCancel := CancelCallBackProc;
   fOnProgress := ProgressCallBackProc;

   FZRec.cb := zsp;
   Encrypted := zsp.Protect;

   If (Not FZRec.cb.Protect) Then
   Begin
      Case BSize Of
         16: FZRec.cb.CRC := 0;
         32: FZRec.cb.CRC := CRC_MASK;
      End;
   End Else
      Case BSize Of
         16: FZRec.cb.CRC := Crc16Val;
         32: FZRec.cb.CRC := Crc32Val;
      End;

   FZRec.next_out := fBuffer;
   FZRec.avail_out := CUSTOM_BUF_SIZE;
End;
//-------------------------------------------------------------

Function TStoreStream.Read(Var Buffer; iCount: longint): longint;
Begin
   Result := -1;	//ShowMessage( 'Error: InvalidStreamOp' );
End;
//-------------------------------------------------------------

Function TStoreStream.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Begin
{ Default implementation of 64 bit seek is to deflect to existing 32 bit seek.
  Descendents that override 64 bit seek must not call this default implementation. }
   If (Offset < Low(Longint)) Or (Offset > High(Longint)) Then
{$ifndef DEL5_OR_HIGHER}
      Raise ERangeError.CreateFmt(SRangeError, []);
{$else}
      Raise ERangeError.CreateRes(@SRangeError);
{$endif}
   Result := Seek(Longint(Offset), Ord(Origin));
End;
//-------------------------------------------------------------

Function TStoreStream.Seek(Offset: longint; Origin: Word): longint;
Begin
   If (Offset = 0) And (Origin = soFromCurrent) Then
      Result := FZRec.total_in
   Else
      Result := -1;
   //ShowMessage( 'Error: sInvalidStreamOp' );
End;
//-------------------------------------------------------------

Function TStoreStream.Write(Const Buffer; iCount: longint): longint;
Begin
   FZRec.next_in := @Buffer;
   FZRec.avail_in := iCount;
   //If (fStrm.Position <> fStrmPos) Then
   //   fStrm.Position := fStrmPos;

   If (Not FZRec.cb.Protect) Then
      Case BSize Of
         16: Crc16_buf(@Buffer, iCount, FZRec.cb.CRC);
         32: Crc32_buf(@Buffer, iCount, FZRec.cb.CRC);
      End;

   If Encrypted Then
   	ZTVEncodeBuf(@Buffer, iCount);

   FZRec.cb.pArchivePos^ := FZRec.cb.pArchivePos^ - FZRec.avail_in;
   fStrm.WriteBuffer(Buffer, iCount);

   FZRec.next_out := fBuffer;
   FZRec.avail_out := CUSTOM_BUF_SIZE;
   //fStrmPos := fStrm.Position;

   Progress(pInStream^);
   Result := iCount;
End;
//-------------------------------------------------------------

Function TStoreStream.CopyFrom(Source: TStream32; Count: Int64): Int64;
Const
   MaxBufSize = $19000; //$186A0; //$F000;
Var
	TempCancel: Boolean;
   BufSize, n: Integer;
   Buffer: PChar;
Begin
   If Count = 0 Then
   Begin
      Source.Position := 0;
      Count := Source.Size;
   End;

   Result := Count;

   // if pCancel = true, then assign the stream objects pCancel variable to
   // the current objects cancel variable to allow a process cancelation.
   pCancel := CancelCallBackProc;
   If pCancel = Nil Then
   Begin
   	TempCancel := False;
      pCancel := @TempCancel;
   End;

   pInStream^.OnProgress := ProgressCallBackProc;

   If Count > MaxBufSize Then
      BufSize := MaxBufSize
   Else
      BufSize := Count;

   GetMem(Buffer, BufSize);
   Try
      While (Count <> 0) And (Not pCancel^) Do
      Begin

         If Count > MaxBufSize{BufSize} Then
            n := MaxBufSize{BufSize}
         Else
            n := Count;

         Application.ProcessMessages();
         Source.ReadBuffer(Buffer^, n);
         Try
         	WriteBuffer(Buffer^, n);
         Except
         	Raise;
         End;
         Dec(Count, n);
      End;
   Finally
      FreeMem(Buffer, BufSize);
   End;
End;
//-------------------------------------------------------------

Function TStoreStream.CopyStream(inStream: TStream32): Int64;
Begin
	InStream.CancelCallBackProc := pCancel;
   InStream.ProgressCallBackProc := fOnProgress;
   pInStream := @inStream;
   Result := CopyFrom(inStream, 0);
End;


(*************************************************************)
(*************************************************************)
(*                      TEncryptStream                       *)
(*************************************************************)
(*************************************************************)

Constructor TEncryptStream.Create(Var inStream, dest: TStream32;
   Len: Int64; Password: String; WriteHeader: Boolean);
Var
	fCancel: Boolean;
	zsp: ztv_stream_plus;
Begin
   fStrm := dest;
   fCancel := False;

   //With zsp Do
   //Begin
   //   Protect := False;
   //   CRC := Crc32Val;
   //   pCancel := @fCancel;
   //   pArchivePos := Nil; //@ProgressPosition;
   //End;

   CRC := CalcStreamCRC32(inStream, Len, zsp, Nil, Nil) Xor CRC_MASK;
   CryptHDR := ZTVEncryptHead(Password, 0, CRC);
   If WriteHeader Then
      fStrm.WriteBuffer(CryptHDR[1], RAND_HEAD_LEN);

	CancelCallBackProc := @fCancel;
	ProgressCallBackProc := Nil;
   CopyFrom(inStream, 0);
End;
//-------------------------------------------------------------

Function TEncryptStream.Write(Const Buffer; Count: longint): longint;
Begin
   ZTVEncodeBuf(@Buffer, Count);
   fStrm.WriteBuffer(Buffer, Count);
   Result := Count;
End;
//-------------------------------------------------------------

Destructor TEncryptStream.Destroy;
Begin
   fStrm.Position := 0;
   Inherited Destroy;
End;

(*************************************************************)
(*************************************************************)
(*                      TDecryptStreamCRC                       *)
(*************************************************************)
(*************************************************************)

// must be called with encrypted crc value!
Constructor TDecryptStreamCRC.Create(Var inStream, dest: TStream32;
   Password: String; CRC: u_long);
Begin
   //If (Password = '') Then
   //   Exit;

   fHeadRead := False;
   fPassword := Password;
   fStrm := dest;

	CancelCallBackProc := Nil;
	ProgressCallBackProc := Nil;

   If CopyFrom(inStream, RAND_HEAD_LEN) = RAND_HEAD_LEN Then
   Begin
      // make a working copy
      CopyMem(@fCryptHDR[1], @fCryptHDR[RAND_HEAD_LEN + 1], RAND_HEAD_LEN);

      If decrypt_pw(@fCryptHDR[1], {RAND_HEAD_LEN,} 0, CRC, 0, fPassword) Then
         CopyFrom(inStream, inStream.Size - RAND_HEAD_LEN);
   End;
End;
//-------------------------------------------------------------

Function TDecryptStreamCRC.Write(Const Buffer; Count: longint): longint;
Begin
   If Not fHeadRead Then
   Begin
      fHeadRead := True;
      CopyMem(@Buffer, @fCryptHDR[1], Count);
   End
   Else
   Begin
      ZTVDecodeBuf(@Buffer, Count);
      fStrm.WriteBuffer(Buffer, Count);
   End;
   Result := Count;
End;
//-------------------------------------------------------------

Destructor TDecryptStreamCRC.Destroy;
Begin
   fStrm.Position := 0;
   Inherited Destroy;
End;

(*************************************************************)
(*************************************************************)
(*                      TDecryptStreamPW                     *)
(*************************************************************)
(*************************************************************)

// must be called with password!
Constructor TDecryptStream.Create(Var inStream, dest: TStream32; Password: String);
Begin
   If (Password = '') Then
      Exit;

   fPassword := Password;
   fStrm := dest;

   If seed_keys(Password) = 0 Then
   Begin
		CancelCallBackProc := Nil;
		ProgressCallBackProc := Nil;
      CopyFrom(inStream, inStream.Size);
   End;
End;
//-------------------------------------------------------------

Function TDecryptStream.Write(Const Buffer; Count: longint): longint;
Begin
   ZTVDecodeBuf(@Buffer, Count);
   fStrm.WriteBuffer(Buffer, Count);
   Result := Count;
End;
//-------------------------------------------------------------

Destructor TDecryptStream.Destroy;
Begin
   fStrm.Position := 0;
   Inherited Destroy;
End;

(*************************************************************)
(*************************************************************)
(*                      TCalcStreamCRC                    *)
(*************************************************************)
(*************************************************************)

Constructor TCalcStreamCRC.Create(inStream: TStream32; Len: Int64; BitSize:
	Byte; zsp: ztv_stream_plus; Cancel_CallBackProc: Pointer;
   Progress_CallBackProc: TNotifyEvent);
Begin
   BSize := BitSize;
   fStrm := inStream;
   FZRec.cb := zsp;
   pCancel := CancelCallBackProc;
   fStrm.OnProgress := ProgressCallBackProc;

   Case BSize Of
      16: CRC := 0;
      32: CRC := CRC_MASK;
   End;

   CancelCallBackProc := Cancel_CallBackProc;
	ProgressCallBackProc := Progress_CallBackProc;
   CopyFrom(inStream, Len);
End;
//-------------------------------------------------------------

Function TCalcStreamCRC.Read(Var Buffer; iCount: longint): longint;
Begin
   Result := -1;	//ShowMessage( 'Error: InvalidStreamOp' );
End;
//-------------------------------------------------------------

Function TCalcStreamCRC.Seek(Offset: longint; Origin: Word): longint;
Begin
	Result := -1; //ShowMessage( 'Error: sInvalidStreamOp' );
End;
//-------------------------------------------------------------

Function TCalcStreamCRC.Write(Const Buf; iCount: longint): longint;
Begin
   FZRec.avail_in := iCount;

   Case BSize Of
      16: Crc16_buf(@Buf, iCount, CRC);
      32: Crc32_buf(@Buf, iCount, CRC);
   End;

   //If (FZRec.cb <> Nil) Then
	If Assigned(fStrm.OnProgress) Then
   Begin
   	FZRec.cb.pArchivePos^ := FZRec.cb.pArchivePos^ - FZRec.avail_in;
   	Progress(fStrm);
   End;
   Result := iCount;
End;
//-------------------------------------------------------------

Destructor TCalcStreamCRC.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function CalcStreamCRC16(strm16: TStream32; Len: Int64; zsp: ztv_stream_plus): u_long;
Var
   crcobj16: TCalcStreamCRC;
Begin
   crcobj16 := TCalcStreamCRC.Create(strm16, 16, Len, zsp, Nil, Nil);
   strm16.Position := 0;
   Result := crcobj16.CRC;
   crcobj16.Free();
End;
//-------------------------------------------------------------

Function CalcStreamCRC32(strm32: TStream32; Len: Int64; zsp: ztv_stream_plus;
	CancelCallBackProc: Pointer; ProgressCallBackProc: TNotifyEvent):
   u_long;
Var
   crcobj32: TCalcStreamCRC;
Begin
   crcobj32 :=
   	TCalcStreamCRC.Create(strm32, Len, 32, zsp, CancelCallBackProc, ProgressCallBackProc);
   strm32.Position := 0;
   Result := crcobj32.CRC;
   crcobj32.Free();
End;
//-------------------------------------------------------------

Function CalcFileCRC(f: THandle; Len: Int64; zsp: ztv_stream_plus; CancelCallBackProc:
	Pointer; ProgressCallBackProc: TNotifyEvent): u_long;
Var
   HS: THandleStream32;
Begin
   HS := THandleStream32.Create(f);
   Result := CalcStreamCRC32(HS, Len, zsp, CancelCallBackProc, ProgressCallBackProc);
   HS.Position := 0;
   HS.Free();
End;
//-------------------------------------------------------------


End.
