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
{$i compiler.inc}
{$E-,M-,V+,Y-,Z1}

{$warnings off}
{.$define ENABLE_DEBUG}  //remove the dot to allow the IDE to debug (code traceing etc.)
{$ifdef ENABLE_DEBUG}
  {$O-,D+,L+,Y+}    	//Enable debug information
  {$define DEBUG}
{$else}
  {$O+,D-,L-,Y-}     //Disable debug information
  {$undef DEBUG}
{$endif}

// for ztvLha.pas, ztvBase.pas, and demos\zipview\zptvdemo.dpr\unit1.pas
// frozen1 compression support
{.$define FROZEN1_SUPPORT}

{.$WARN UNIT_PLATFORM OFF}

{== Code Generation Directives ==}

{$F-}    { Force Far Calls }
{$A+}    { Word Align Data }
{$U-}    { Pentium-Save FDIV }
{$K-}    { Smart Callbacks }
{$W-}    { Windows Stack Frame }


{== Runtime Errors ==}

{$IFOPT D+}
{$else}
{$endif}
{$R-}    { Range Checking - Off }
{$S-}    { Stack Checking }
{$I+}    { I/O Checking }
{$Q-}    { Overflow Checking }


{== Syntax Options ==}

{$V+}    { Strict Var-Strings }  // v4.0... changed from $V-
{$B-}    { Complete Boolean Evaluation }
{$X+}    { Extended Syntax }
{$T-}    { Typed @ Operator }
{$P+}    { Open Parameters }
{$ifdef DEL4_OR_HIGHER}
{$J+}    { Writeable Typed Constants }	// v4.0... added the ifdef DEL4_OR_HIGHER conditional define
{$endif}

{== Miscellaneous Directives ==}

{$C MOVEABLE DEMANDLOAD DISCARDABLE}    { Code Segment Attribute }  // v4.0... changed from $C-
{$G+}    { Delphi 1: 286 Instructions / Delphi 3 & later: Imported Data }
{$N+}    { Numeric Coprocessor }
{$Z-}    { Word Size Enumerated Types }	// v4.0... added

{$ifdef WIN32}
{$H+}    { Long String Support }
{$endif}

{$define DYNLOADUNACEDLL}
{$define DYNLOADUNRARDLL}

Type
   uint = Integer; //LongWord;  // requires compiler option $R- (range checking off)
  	u_long  = Cardinal;
   fs_long = Cardinal;

Const
{$ifdef MAXSEG_64K}
   MAX_MEM_LEVEL = 8;
   DEF_MEM_LEVEL = MAX_MEM_LEVEL;
	MaxMemBlock = $FFFF;
{$else}
	MAX_MEM_LEVEL = 9;
   DEF_MEM_LEVEL = 8;
  	MaxMemBlock = MaxInt;
{$endif}

Type
	pBool = ^Boolean;
  	uIntf  = uInt;
  	ush  = Word;
  	uch  = Byte;
  	ushf = ush;
  	_unsigned = uInt;
  	ulg  = LongInt;
  	ptr2int = uInt;

  	_Bytef  = byte;
  	_pBytef = ^_Bytef;
  	charf  = byte;
  	pcharf = ^charf;
  	_Pos = ush;
  	Posf = _Pos;
  	pPosf = ^Posf;
 	IPos = uInt;

  	_int    = integer;
  	intf   = _int;

  	Long   = longint;
  	uLongf = u_long;

  	voidp  = pointer;
  	voidpf = voidp;
  	pIntf  = ^intf;
  	puIntf = ^uIntf;
  	puLong = ^uLongf;
   pBoolean = ^Boolean;

   {$ifndef DEL4_OR_HIGHER}
   TInteger8 = Extended; //Comp;          // 8-byte integer, since disk sizes may be > 2 GB
   pInteger8 = ^TInteger8;
   {$else}
   TInteger8 = Int64;                     // 8-byte integer, since disk sizes may be > 2 GB
   pInteger8 = ^TInteger8;
   {$endif}

Const
	Z_BINARY    = 0;
   Z_ASCII     = 1;
   Z_UNKNOWN   = 2;
   Z_DEFLATED  = 8;
   Z_DEFLATE64 = 9;
   Z_NULL     = NIL;

   Z_FILTERED            = 1;
   Z_HUFFMAN_ONLY        = 2;
   Z_DEFAULT_STRATEGY    = 0;

  	Z_NO_FLUSH      = 0;
  	Z_PARTIAL_FLUSH = 1;
  	Z_SYNC_FLUSH    = 2;
  	Z_FULL_FLUSH    = 3;
  	Z_FINISH        = 4;

   // return codes
  	Z_OK            = 0;
  	Z_STREAM_END    = 1;
  	Z_NEED_DICT     = 2;
  	Z_ERRNO         = (-1);
  	Z_STREAM_ERROR  = (-2);
  	Z_DATA_ERROR    = (-3);
  	Z_MEM_ERROR     = (-4);
  	Z_BUF_ERROR     = (-5);
  	Z_VERSION_ERROR = (-6);

   BASE = u_long( 65521 );                { largest prime smaller than 65536 }
   NMAX = 3854;                           { code with signed 32 bit _int }

   z_errbase = Z_NEED_DICT;
   //errmsg: Array[0..9] Of String[21] =
   //( 'need dictionary',                 { NEED_DICT       2  }
   //   'stream end',                     { STREAM_END      1  }
   //   '',                               { OK              0  }
   //   'file error',                     { ERRNO         (-1) }
   //   'stream error',                   { STREAM_ERROR  (-2) }
   //   'data error',                     { DATA_ERROR    (-3) }
   //   'insufficient memory',            { MEM_ERROR     (-4) }
   //   'buffer error',                   { BUF_ERROR     (-5) }
   //   'incompatible version',           { VERSION_ERROR (-6) }
   //   '' );

   INIT_STATE = 42;
   BUSY_STATE = 113;
   FINISH_STATE = 666;
   _MAX_WBITS = -15;                      { 32K LZ77 window }
  	RAND_HEAD_LEN = 12;                    { length of encryption Random header }
   CRC_MASK: u_long = $FFFFFFFF;


