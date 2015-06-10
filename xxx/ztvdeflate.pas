// compress
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
Unit ztvDeflate;

Interface


// DO NOT CHANGE THESE SWITCHES!  Revising these switches will
// produce unexpected errors, resulting in the failure of the
// deflate method.
{$J+}    { Writeable Typed Constants }     // v4.0 added
{$Q-}    { Overflow Checking }             // v4.5.3 added

Uses
	Windows,
   Classes,
   ztvBase,
   ztvGbls,
   ztvStreams;

{$I ZipTV.inc}

Const
   LITERALS = 256;
   LENGTH_CODES = 29;
{$IFDEF DEFLATE64}
   D_CODES_DEF64 = 30; //32;	//changed from 30 for deflate64
   D_CODES = 30;
{$ELSE}
   D_CODES = 30;
{$ENDIF}
   BL_CODES = 19;
   L_CODES = LITERALS + 1 + LENGTH_CODES;
   HEAP_SIZE = (2 * L_CODES + 1);
   MAX_BITS = 15;

Type
   ct_data_ptr = ^ct_data;
   ct_data = Record
      fc:
      Record
         Case Byte Of
            0: (freq: ush);
            1: (code: ush);
      End;
      dl:
      Record
         Case Byte Of
            0: (dad: ush);
            1: (Len: ush);
      End;
   End;

   zIntfArray = Array[0..(MaxMemBlock Div SizeOf(Intf)) - 1] Of Intf;
   ltree_type = Array[0..HEAP_SIZE - 1] Of ct_data;

{$IFDEF DEFLATE64}
   dtree_type = Array[0..2 * D_CODES_DEF64 + 1 - 1] Of ct_data;
{$ELSE}
   dtree_type = Array[0..2 * D_CODES + 1 - 1] Of ct_data;
{$ENDIF}

   htree_type = Array[0..2 * BL_CODES + 1 - 1] Of ct_data;
   tree_type = Array[0..(MaxMemBlock Div SizeOf(ct_data)) - 1] Of ct_data;
   zushfArray = Array[0..(MaxMemBlock Div SizeOf(ushf)) - 1] Of ushf;
   zPosfArray = Array[0..(MaxMemBlock Div SizeOf(Posf)) - 1] Of Posf;
   zByteArray = Array[0..(MaxMemBlock Div SizeOf(_Bytef)) - 1] Of _Bytef;

   pzPosfArray = ^zPosfArray;
   pushfArray = ^zushfArray;
   pzIntfArray = ^zIntfArray;
   tree_ptr = ^tree_type;
   pzByteArray = ^zByteArray;

Type
   static_tree_desc_ptr = ^static_tree_desc;
   static_tree_desc =
      Record
      {const} static_tree: tree_ptr;    { static tree or NIL }
      {const} extra_bits: pzIntfArray;  { extra bits for each code or NIL }
      extra_base: _int;                 { base index for extra_bits }
      elems: _int;                      { max number of elements in the tree }
      max_length: _int;                 { max bit length for the codes }
   End;

   tree_desc_ptr = ^tree_desc;
   tree_desc = Record
      dyn_tree: tree_ptr;               { the dynamic tree }
      max_code: _int;                   { largest code with non zero frequency }
      stat_desc: static_tree_desc_ptr;  { the corresponding static tree }
   End;

   zuchfArray = zByteArray;
   puchfArray = ^zuchfArray;

   deflate_state_ptr = ^deflate_state;
   deflate_state = Record
      strm: ztv_streamp;                { pointer back to this stream }
      Status: _int;                     { as the name implies }
      pending_buf: pzByteArray;         { output still pending }
      pending_buf_size: longint;        { size of pending_buf }
      pending_out: _pBytef;             { next pending byte to output to the stream }
      pending: _int;                    { nb of bytes in the pending buffer }
      noheader: _int;                   { suppress zlib header and adler32 }
      data_type: Byte;                  { UNKNOWN, BINARY or ASCII }
      method: Byte;                     { STORED (for zip only) or DEFLATED }
      last_flush: _int;                 { value of flush param for previous deflate call }

      w_size: uInt;                     { LZ77 window size (32K by default) }
      w_bits: uInt;                     { log2(w_size)  (8..16) }
      w_mask: uInt;                     { w_size - 1 }

      window: pzByteArray;
      window_size: longint;
      prev: pzPosfArray;
      head: pzPosfArray;                { Heads of the hash chains or NIL. }
      ins_h: uInt;                      { hash index of string to be inserted }
      hash_size: uInt;                  { number of elements in hash table }
      hash_bits: uInt;                  { log2(hash_size) }
      hash_mask: uInt;                  { hash_size-1 }
      hash_shift: uInt;
      block_start: longint;
      match_length: uInt;               { length of best match }
      prev_match: IPos;                 { previous match }
      match_available: Boolean;         { set if previous match exists }
      strstart: uInt;                   { start of string to insert }
      match_start: uInt;                { start of matching string }
      lookahead: uInt;                  { number of valid bytes ahead in window }
      prev_length: uInt;
      max_chain_length: uInt;
      level: _int;                      { compression level (1..9) }
      strategy: _int;                   { favor or force Huffman coding}
      good_match: uInt;
      nice_match: _int;                 { Stop searching when current match exceeds this }
      dyn_ltree: ltree_type;            { literal and length tree }
      dyn_dtree: dtree_type;            { distance tree }
      bl_tree: htree_type;              { Huffman tree for bit lengths }
      l_desc: tree_desc;                { desc. for literal tree }
      d_desc: tree_desc;                { desc. for distance tree }
      bl_desc: tree_desc;               { desc. for bit length tree }
      bl_count: Array[0..MAX_BITS + 1 - 1] Of ush;
      heap: Array[0..2 * L_CODES + 1 - 1] Of _int; { heap used to build the Huffman trees }
      heap_len: _int;                   { number of elements in the heap }
      heap_max: _int;                   { element of largest frequency }
      depth: Array[0..2 * L_CODES + 1 - 1] Of uch;
      l_buf: puchfArray;                { buffer for literals or lengths }
      lit_bufsize: uInt;
      last_lit: uInt;                   { running index in l_buf }
      d_buf: pushfArray;
      opt_len: longint;                 { bit length of current block with optimal trees }
      static_len: longint;              { bit length of current block with static trees }
      compressed_len: longint;          { total bit length of compressed file }
      matches: uInt;                    { number of string matches in current block }
      last_eob_len: _int;               { bit length of EOB code for last block }
      bi_buf: ush;
      bi_valid: _int;
      Case Byte Of
         0: (max_lazy_match: uInt);
         1: (max_insert_length: uInt);
   End;

Function _deflate(Var strm: ztv_stream; Flush: _int): _int;
Function _deflateEnd(Var strm: ztv_stream): _int;
Function deflateInit2(Var strm: ztv_stream; level: _int; method: _int;
   windowBits: _int; memLevel: _int; strategy: _int): _int;
Function deflateSetDictionary(Var strm: ztv_stream; dictionary: ztvStreams._pBytef;
   dictLength: word): _int;
Function deflateCopy(dest: ztv_streamp; source: ztv_streamp): _int;

Function ztvCompressStreamProc(InStream, OutStream: TStream32; zsp:
   ztv_stream_plus; Var CRC: u_long; DefType: TDeflateType; ProgressProc:
   TNotifyEvent; maxbits: ShortInt): Boolean;
//Function ztvCompressStream( InStream, OutStream: TStream32 ): _int;

Function ztvCompress_String(s: String; level: TDeflateType): String;
Function ztvCompress_BufToBuf(Const inbuf: Pointer; InBytes: _int;
   Var outbuf: Pointer; Var outBytes: _int; level: TDeflateType): _int;
Function ztvCompress_FileToStream(FileName: String; OutStream: TStream32): _int;
Function ztvCompress_StreamToClipboard(Stream: TStream32): _int;
Function ztvCompress_StreamToStream(InStream, OutStream: TStream32): _int;




Implementation

Uses
   SysUtils,
   Dialogs,
   ztvRegister;

Type
   block_state = (need_more, block_done, finish_started, finish_done);
   compress_func = Function(Var s: deflate_state; Flush: _int): block_state;

Function deflate_stored(Var s: deflate_state; Flush: _int): block_state; Far; Forward;
Function deflate_fast(Var s: deflate_state; Flush: _int): block_state; Far; Forward;
Function deflate_slow(Var s: deflate_state; Flush: _int): block_state; Far; Forward;
Procedure lm_init(Var s: deflate_state); Forward;


Const
   ZNIL = 0;
   TOO_FAR = 4096;
   SMALLEST = 1;
   DIST_CODE_LEN = 512;
   REP_3_6 = 16;
   REPZ_3_10 = 17;
   REPZ_11_138 = 18;
   BUF_SIZE = (8 * 2 * SizeOf(char));
   PRESET_DICT = $20;
   MAX_BL_BITS = 7;
   END_BLOCK = 256;
   STORED_BLOCK = 0;
   STATIC_TREES = 1;
   DYN_TREES = 2;
   MAX_MATCH = 258;
   MIN_MATCH = 3;
{$IFDEF DEFLATE64}
  	MAX_MATCH_DEF64 = 64 * 1024; {lengths are 3..65536  for deflate64}
   MIN_LOOKAHEAD_DEF64 = (MAX_MATCH_DEF64 + MIN_MATCH + 1);  //deflate64
{$ELSE}
   MIN_LOOKAHEAD = (MAX_MATCH + MIN_MATCH + 1);
{$ENDIF}


Type
   Config = Record
      good_length: ush;
      max_lazy: ush;
      nice_length: ush;
      max_chain: ush;
      func: compress_func;
   End;

Const
   configuration_table: Array[0..10 - 1] Of Config = (
      { good lazy nice chain }
      (good_length:  0; max_lazy:   0; nice_length:   0; max_chain:    0; func: deflate_stored), { store only }
      (good_length:  4; max_lazy:   4; nice_length:   8; max_chain:    4; func: deflate_fast),   { maximum speed, no lazy matches }
      (good_length:  4; max_lazy:   5; nice_length:  16; max_chain:    8; func: deflate_fast),
      (good_length:  4; max_lazy:   6; nice_length:  32; max_chain:   32; func: deflate_fast),
      (good_length:  4; max_lazy:   4; nice_length:  16; max_chain:   16; func: deflate_slow),   { lazy matches }
      (good_length:  8; max_lazy:  16; nice_length:  32; max_chain:   32; func: deflate_slow),
      (good_length:  8; max_lazy:  16; nice_length: 128; max_chain:  128; func: deflate_slow),
      (good_length:  8; max_lazy:  32; nice_length: 128; max_chain:  256; func: deflate_slow),
      (good_length: 32; max_lazy: 128; nice_length: 258; max_chain: 1024; func: deflate_slow),
      (good_length: 32; max_lazy: 258; nice_length: 258; max_chain: 4096; func: deflate_slow));  { maximum compression }
      //(good_length: 64; max_lazy: 512; nice_length: 512; max_chain: 9182)


Const
   static_ltree: Array[0..L_CODES + 2 - 1] Of ct_data = (
      (fc: (freq:  12); dl: (Len: 8)), (fc: (freq: 140); dl: (Len: 8)), (fc: (freq:  76); dl: (Len: 8)),
      (fc: (freq: 204); dl: (Len: 8)), (fc: (freq:  44); dl: (Len: 8)), (fc: (freq: 172); dl: (Len: 8)),
      (fc: (freq: 108); dl: (Len: 8)), (fc: (freq: 236); dl: (Len: 8)), (fc: (freq:  28); dl: (Len: 8)),
      (fc: (freq: 156); dl: (Len: 8)), (fc: (freq:  92); dl: (Len: 8)), (fc: (freq: 220); dl: (Len: 8)),
      (fc: (freq:  60); dl: (Len: 8)), (fc: (freq: 188); dl: (Len: 8)), (fc: (freq: 124); dl: (Len: 8)),
      (fc: (freq: 252); dl: (Len: 8)), (fc: (freq:   2); dl: (Len: 8)), (fc: (freq: 130); dl: (Len: 8)),
      (fc: (freq:  66); dl: (Len: 8)), (fc: (freq: 194); dl: (Len: 8)), (fc: (freq:  34); dl: (Len: 8)),
      (fc: (freq: 162); dl: (Len: 8)), (fc: (freq:  98); dl: (Len: 8)), (fc: (freq: 226); dl: (Len: 8)),
      (fc: (freq:  18); dl: (Len: 8)), (fc: (freq: 146); dl: (Len: 8)), (fc: (freq:  82); dl: (Len: 8)),
 {10} (fc: (freq: 210); dl: (Len: 8)), (fc: (freq:  50); dl: (Len: 8)), (fc: (freq: 178); dl: (Len: 8)),
      (fc: (freq: 114); dl: (Len: 8)), (fc: (freq: 242); dl: (Len: 8)), (fc: (freq:  10); dl: (Len: 8)),
      (fc: (freq: 138); dl: (Len: 8)), (fc: (freq:  74); dl: (Len: 8)), (fc: (freq: 202); dl: (Len: 8)),
      (fc: (freq:  42); dl: (Len: 8)), (fc: (freq: 170); dl: (Len: 8)), (fc: (freq: 106); dl: (Len: 8)),
      (fc: (freq: 234); dl: (Len: 8)), (fc: (freq:  26); dl: (Len: 8)), (fc: (freq: 154); dl: (Len: 8)),
      (fc: (freq:  90); dl: (Len: 8)), (fc: (freq: 218); dl: (Len: 8)), (fc: (freq:  58); dl: (Len: 8)),
      (fc: (freq: 186); dl: (Len: 8)), (fc: (freq: 122); dl: (Len: 8)), (fc: (freq: 250); dl: (Len: 8)),
      (fc: (freq:   6); dl: (Len: 8)), (fc: (freq: 134); dl: (Len: 8)), (fc: (freq:  70); dl: (Len: 8)),
      (fc: (freq: 198); dl: (Len: 8)), (fc: (freq:  38); dl: (Len: 8)), (fc: (freq: 166); dl: (Len: 8)),
      (fc: (freq: 102); dl: (Len: 8)), (fc: (freq: 230); dl: (Len: 8)), (fc: (freq:  22); dl: (Len: 8)),
 {20} (fc: (freq: 150); dl: (Len: 8)), (fc: (freq:  86); dl: (Len: 8)), (fc: (freq: 214); dl: (Len: 8)),
      (fc: (freq:  54); dl: (Len: 8)), (fc: (freq: 182); dl: (Len: 8)), (fc: (freq: 118); dl: (Len: 8)),
      (fc: (freq: 246); dl: (Len: 8)), (fc: (freq:  14); dl: (Len: 8)), (fc: (freq: 142); dl: (Len: 8)),
      (fc: (freq:  78); dl: (Len: 8)), (fc: (freq: 206); dl: (Len: 8)), (fc: (freq:  46); dl: (Len: 8)),
      (fc: (freq: 174); dl: (Len: 8)), (fc: (freq: 110); dl: (Len: 8)), (fc: (freq: 238); dl: (Len: 8)),
      (fc: (freq:  30); dl: (Len: 8)), (fc: (freq: 158); dl: (Len: 8)), (fc: (freq:  94); dl: (Len: 8)),
      (fc: (freq: 222); dl: (Len: 8)), (fc: (freq:  62); dl: (Len: 8)), (fc: (freq: 190); dl: (Len: 8)),
      (fc: (freq: 126); dl: (Len: 8)), (fc: (freq: 254); dl: (Len: 8)), (fc: (freq:   1); dl: (Len: 8)),
      (fc: (freq: 129); dl: (Len: 8)), (fc: (freq:  65); dl: (Len: 8)), (fc: (freq: 193); dl: (Len: 8)),
      (fc: (freq:  33); dl: (Len: 8)), (fc: (freq: 161); dl: (Len: 8)), (fc: (freq:  97); dl: (Len: 8)),
 {30} (fc: (freq: 225); dl: (Len: 8)), (fc: (freq:  17); dl: (Len: 8)), (fc: (freq: 145); dl: (Len: 8)),
      (fc: (freq:  81); dl: (Len: 8)), (fc: (freq: 209); dl: (Len: 8)), (fc: (freq:  49); dl: (Len: 8)),
      (fc: (freq: 177); dl: (Len: 8)), (fc: (freq: 113); dl: (Len: 8)), (fc: (freq: 241); dl: (Len: 8)),
      (fc: (freq:   9); dl: (Len: 8)), (fc: (freq: 137); dl: (Len: 8)), (fc: (freq:  73); dl: (Len: 8)),
      (fc: (freq: 201); dl: (Len: 8)), (fc: (freq:  41); dl: (Len: 8)), (fc: (freq: 169); dl: (Len: 8)),
      (fc: (freq: 105); dl: (Len: 8)), (fc: (freq: 233); dl: (Len: 8)), (fc: (freq:  25); dl: (Len: 8)),
      (fc: (freq: 153); dl: (Len: 8)), (fc: (freq:  89); dl: (Len: 8)), (fc: (freq: 217); dl: (Len: 8)),
      (fc: (freq:  57); dl: (Len: 8)), (fc: (freq: 185); dl: (Len: 8)), (fc: (freq: 121); dl: (Len: 8)),
      (fc: (freq: 249); dl: (Len: 8)), (fc: (freq:   5); dl: (Len: 8)), (fc: (freq: 133); dl: (Len: 8)),
      (fc: (freq:  69); dl: (Len: 8)), (fc: (freq: 197); dl: (Len: 8)), (fc: (freq:  37); dl: (Len: 8)),
 {40} (fc: (freq: 165); dl: (Len: 8)), (fc: (freq: 101); dl: (Len: 8)), (fc: (freq: 229); dl: (Len: 8)),
      (fc: (freq:  21); dl: (Len: 8)), (fc: (freq: 149); dl: (Len: 8)), (fc: (freq:  85); dl: (Len: 8)),
      (fc: (freq: 213); dl: (Len: 8)), (fc: (freq:  53); dl: (Len: 8)), (fc: (freq: 181); dl: (Len: 8)),
      (fc: (freq: 117); dl: (Len: 8)), (fc: (freq: 245); dl: (Len: 8)), (fc: (freq:  13); dl: (Len: 8)),
      (fc: (freq: 141); dl: (Len: 8)), (fc: (freq:  77); dl: (Len: 8)), (fc: (freq: 205); dl: (Len: 8)),
      (fc: (freq:  45); dl: (Len: 8)), (fc: (freq: 173); dl: (Len: 8)), (fc: (freq: 109); dl: (Len: 8)),
      (fc: (freq: 237); dl: (Len: 8)), (fc: (freq:  29); dl: (Len: 8)), (fc: (freq: 157); dl: (Len: 8)),
      (fc: (freq:  93); dl: (Len: 8)), (fc: (freq: 221); dl: (Len: 8)), (fc: (freq:  61); dl: (Len: 8)),
      (fc: (freq: 189); dl: (Len: 8)), (fc: (freq: 125); dl: (Len: 8)), (fc: (freq: 253); dl: (Len: 8)),
      (fc: (freq:  19); dl: (Len: 9)), (fc: (freq: 275); dl: (Len: 9)), (fc: (freq: 147); dl: (Len: 9)),
 {50} (fc: (freq: 403); dl: (Len: 9)), (fc: (freq:  83); dl: (Len: 9)), (fc: (freq: 339); dl: (Len: 9)),
      (fc: (freq: 211); dl: (Len: 9)), (fc: (freq: 467); dl: (Len: 9)), (fc: (freq:  51); dl: (Len: 9)),
      (fc: (freq: 307); dl: (Len: 9)), (fc: (freq: 179); dl: (Len: 9)), (fc: (freq: 435); dl: (Len: 9)),
      (fc: (freq: 115); dl: (Len: 9)), (fc: (freq: 371); dl: (Len: 9)), (fc: (freq: 243); dl: (Len: 9)),
      (fc: (freq: 499); dl: (Len: 9)), (fc: (freq:  11); dl: (Len: 9)), (fc: (freq: 267); dl: (Len: 9)),
      (fc: (freq: 139); dl: (Len: 9)), (fc: (freq: 395); dl: (Len: 9)), (fc: (freq:  75); dl: (Len: 9)),
      (fc: (freq: 331); dl: (Len: 9)), (fc: (freq: 203); dl: (Len: 9)), (fc: (freq: 459); dl: (Len: 9)),
      (fc: (freq:  43); dl: (Len: 9)), (fc: (freq: 299); dl: (Len: 9)), (fc: (freq: 171); dl: (Len: 9)),
      (fc: (freq: 427); dl: (Len: 9)), (fc: (freq: 107); dl: (Len: 9)), (fc: (freq: 363); dl: (Len: 9)),
      (fc: (freq: 235); dl: (Len: 9)), (fc: (freq: 491); dl: (Len: 9)), (fc: (freq:  27); dl: (Len: 9)),
 {60} (fc: (freq: 283); dl: (Len: 9)), (fc: (freq: 155); dl: (Len: 9)), (fc: (freq: 411); dl: (Len: 9)),
      (fc: (freq:  91); dl: (Len: 9)), (fc: (freq: 347); dl: (Len: 9)), (fc: (freq: 219); dl: (Len: 9)),
      (fc: (freq: 475); dl: (Len: 9)), (fc: (freq:  59); dl: (Len: 9)), (fc: (freq: 315); dl: (Len: 9)),
      (fc: (freq: 187); dl: (Len: 9)), (fc: (freq: 443); dl: (Len: 9)), (fc: (freq: 123); dl: (Len: 9)),
      (fc: (freq: 379); dl: (Len: 9)), (fc: (freq: 251); dl: (Len: 9)), (fc: (freq: 507); dl: (Len: 9)),
      (fc: (freq:   7); dl: (Len: 9)), (fc: (freq: 263); dl: (Len: 9)), (fc: (freq: 135); dl: (Len: 9)),
      (fc: (freq: 391); dl: (Len: 9)), (fc: (freq:  71); dl: (Len: 9)), (fc: (freq: 327); dl: (Len: 9)),
      (fc: (freq: 199); dl: (Len: 9)), (fc: (freq: 455); dl: (Len: 9)), (fc: (freq:  39); dl: (Len: 9)),
      (fc: (freq: 295); dl: (Len: 9)), (fc: (freq: 167); dl: (Len: 9)), (fc: (freq: 423); dl: (Len: 9)),
      (fc: (freq: 103); dl: (Len: 9)), (fc: (freq: 359); dl: (Len: 9)), (fc: (freq: 231); dl: (Len: 9)),
 {70} (fc: (freq: 487); dl: (Len: 9)), (fc: (freq:  23); dl: (Len: 9)), (fc: (freq: 279); dl: (Len: 9)),
      (fc: (freq: 151); dl: (Len: 9)), (fc: (freq: 407); dl: (Len: 9)), (fc: (freq:  87); dl: (Len: 9)),
      (fc: (freq: 343); dl: (Len: 9)), (fc: (freq: 215); dl: (Len: 9)), (fc: (freq: 471); dl: (Len: 9)),
      (fc: (freq:  55); dl: (Len: 9)), (fc: (freq: 311); dl: (Len: 9)), (fc: (freq: 183); dl: (Len: 9)),
      (fc: (freq: 439); dl: (Len: 9)), (fc: (freq: 119); dl: (Len: 9)), (fc: (freq: 375); dl: (Len: 9)),
      (fc: (freq: 247); dl: (Len: 9)), (fc: (freq: 503); dl: (Len: 9)), (fc: (freq:  15); dl: (Len: 9)),
      (fc: (freq: 271); dl: (Len: 9)), (fc: (freq: 143); dl: (Len: 9)), (fc: (freq: 399); dl: (Len: 9)),
      (fc: (freq:  79); dl: (Len: 9)), (fc: (freq: 335); dl: (Len: 9)), (fc: (freq: 207); dl: (Len: 9)),
      (fc: (freq: 463); dl: (Len: 9)), (fc: (freq:  47); dl: (Len: 9)), (fc: (freq: 303); dl: (Len: 9)),
      (fc: (freq: 175); dl: (Len: 9)), (fc: (freq: 431); dl: (Len: 9)), (fc: (freq: 111); dl: (Len: 9)),
 {80} (fc: (freq: 367); dl: (Len: 9)), (fc: (freq: 239); dl: (Len: 9)), (fc: (freq: 495); dl: (Len: 9)),
      (fc: (freq:  31); dl: (Len: 9)), (fc: (freq: 287); dl: (Len: 9)), (fc: (freq: 159); dl: (Len: 9)),
      (fc: (freq: 415); dl: (Len: 9)), (fc: (freq:  95); dl: (Len: 9)), (fc: (freq: 351); dl: (Len: 9)),
      (fc: (freq: 223); dl: (Len: 9)), (fc: (freq: 479); dl: (Len: 9)), (fc: (freq:  63); dl: (Len: 9)),
      (fc: (freq: 319); dl: (Len: 9)), (fc: (freq: 191); dl: (Len: 9)), (fc: (freq: 447); dl: (Len: 9)),
 {85} (fc: (freq: 127); dl: (Len: 9)), (fc: (freq: 383); dl: (Len: 9)), (fc: (freq: 255); dl: (Len: 9)),  //255
      (fc: (freq: 511); dl: (Len: 9)), (fc: (freq:   0); dl: (Len: 7)), (fc: (freq:  64); dl: (Len: 7)),
      (fc: (freq:  32); dl: (Len: 7)), (fc: (freq:  96); dl: (Len: 7)), (fc: (freq:  16); dl: (Len: 7)),
      (fc: (freq:  80); dl: (Len: 7)), (fc: (freq:  48); dl: (Len: 7)), (fc: (freq: 112); dl: (Len: 7)),
      (fc: (freq:   8); dl: (Len: 7)), (fc: (freq:  72); dl: (Len: 7)), (fc: (freq:  40); dl: (Len: 7)),
 {90} (fc: (freq: 104); dl: (Len: 7)), (fc: (freq:  24); dl: (Len: 7)), (fc: (freq:  88); dl: (Len: 7)),  //270
      (fc: (freq:  56); dl: (Len: 7)), (fc: (freq: 120); dl: (Len: 7)), (fc: (freq:   4); dl: (Len: 7)),
      (fc: (freq:  68); dl: (Len: 7)), (fc: (freq:  36); dl: (Len: 7)), (fc: (freq: 100); dl: (Len: 7)),
      (fc: (freq:  20); dl: (Len: 7)), (fc: (freq:  84); dl: (Len: 7)), (fc: (freq:  52); dl: (Len: 7)),
      (fc: (freq: 116); dl: (Len: 7)), (fc: (freq:   3); dl: (Len: 8)), (fc: (freq: 131); dl: (Len: 8)),
      (fc: (freq:  67); dl: (Len: 8)), (fc: (freq: 195); dl: (Len: 8)), (fc: (freq:  35); dl: (Len: 8)),  //285
 {96} (fc: (freq: 163); dl: (Len: 8)), (fc: (freq:  99); dl: (Len: 8)), (fc: (freq: 227); dl: (Len: 8))  //288
      );


   static_dtree: Array[0..D_CODES - 1] Of ct_data = (
      (fc: (freq:  0); dl: (Len: 5)), (fc: (freq: 16); dl: (Len: 5)), (fc: (freq:  8); dl: (Len: 5)),
      (fc: (freq: 24); dl: (Len: 5)), (fc: (freq:  4); dl: (Len: 5)), (fc: (freq: 20); dl: (Len: 5)),
      (fc: (freq: 12); dl: (Len: 5)), (fc: (freq: 28); dl: (Len: 5)), (fc: (freq:  2); dl: (Len: 5)),
      (fc: (freq: 18); dl: (Len: 5)), (fc: (freq: 10); dl: (Len: 5)), (fc: (freq: 26); dl: (Len: 5)),
      (fc: (freq:  6); dl: (Len: 5)), (fc: (freq: 22); dl: (Len: 5)), (fc: (freq: 14); dl: (Len: 5)),
      (fc: (freq: 30); dl: (Len: 5)), (fc: (freq:  1); dl: (Len: 5)), (fc: (freq: 17); dl: (Len: 5)),
      (fc: (freq:  9); dl: (Len: 5)), (fc: (freq: 25); dl: (Len: 5)), (fc: (freq:  5); dl: (Len: 5)),
      (fc: (freq: 21); dl: (Len: 5)), (fc: (freq: 13); dl: (Len: 5)), (fc: (freq: 29); dl: (Len: 5)),
      (fc: (freq:  3); dl: (Len: 5)), (fc: (freq: 19); dl: (Len: 5)), (fc: (freq: 11); dl: (Len: 5)),
      (fc: (freq: 27); dl: (Len: 5)), (fc: (freq:  7); dl: (Len: 5)), (fc: (freq: 23); dl: (Len: 5))
      );

   _length_code: Array[0..MAX_MATCH - MIN_MATCH + 1 - 1] Of uch = (
       0,  1,  2,  3,  4,  5,  6,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 12, 12, 12,
      13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16,
      17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19,
      19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
      21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23,
      23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
      24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
      25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
      25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
      27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28
      );

   _dist_code: Array[0..DIST_CODE_LEN - 1] Of uch = (
       0,  1,  2,  3,  4,  4,  5,  5,  6,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  8,
       8,  8,  8,  8,  9,  9,  9,  9,  9,  9,  9,  9, 10, 10, 10, 10, 10, 10, 10, 10,
      10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
      11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
      12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13,
      13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
      13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,  0,  0, 16, 17,
      18, 18, 19, 19, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22,
      23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
      24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27,
      27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
      27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
      28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
      28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
      28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
      29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
      29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
      29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29
      );


   //                            Distance Codes
   //                            --------------
   //      Extra             Extra               Extra               Extra
   // Code Bits Dist    Code Bits  Dist     Code Bits Dist      Code Bits Dist
   // ---- ---- ----    ---- ---- ------    ---- ---- ----      ---- ---- ----
   //   0   0    1        8   3   17-24      16    7  257-384     24   11  4097-6144
   //   1   0    2        9   3   25-32      17    7  385-512     25   11  6145-8192
   //   2   0    3       10   4   33-48      18    8  513-768     26   12  8193-12288
   //   3   0    4       11   4   49-64      19    8  769-1024    27   12 12289-16384
   //   4   1   5,6      12   5   65-96      20    9 1025-1536    28   13 16385-24576
   //   5   1   7,8      13   5   97-128     21    9 1537-2048    29   13 24577-32768
   //   6   2   9-12     14   6  129-192     22   10 2049-3072
   //   7   2  13-16     15   6  193-256     23   10 3073-4096
   //
   //       Extra
   //  Code Bits Dist
   //  ---- ---- ----
   //   ..   ..      ...
   //   29   13  24577-32768
   //   30   14  32769-49152
   //   31   14  49153-65536
   //
   // base_dist   = Dist column
   // extra_dbits = Bits column
   //

{$IFDEF DEFLATE64}
   extra_dbits: Array[0..D_CODES_DEF64 - 1] Of _int = (0, 0, 0, 0, 1, 1,
   	2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12,
      13, 13, 14, 14);

   base_dist: Array[0..D_CODES_DEF64 - 1] Of _int =
   	(0, 1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256, 384,
      512, 768, 1024, 1536, 2048, 3072, 4096, 6144, 8192, 12288, 16384, 24576,
      32768, 49152);
{$ELSE}
   extra_dbits: Array[0..D_CODES - 1] Of _int = (0, 0, 0, 0, 1, 1, 2, 2, 3, 3,
   	4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13);

   base_dist: Array[0..D_CODES - 1] Of _int =
   	(0, 1, 2, 3, 4, 6, 8, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256, 384,
      512, 768, 1024, 1536, 2048, 3072, 4096, 6144, 8192, 12288, 16384, 24576);
{$ENDIF}


   //                             Length Codes
   //                             ------------
   //      Extra             Extra              Extra              Extra
   // Code Bits Length  Code Bits Lengths   Code Bits Lengths   Code Bits Lengths
   // ---- ---- ------  ---- ---- -------   ---- ---- -------   ---- ---- ---------
   //  257   0     3     265   1   11,12     273   3   35-42     281   5  131-162
   //  258   0     4     266   1   13,14     274   3   43-50     282   5  163-194
   //  259   0     5     267   1   15,16     275   3   51-58     283   5  195-226
   //  260   0     6     268   1   17,18     276   3   59-66     284   5  227-258
   //  261   0     7     269   2   19-22     277   4   67-82     285   0    258
   //  262   0     8     270   2   23-26     278   4   83-98
   //  263   0     9     271   2   27-30     279   4   99-114
   //  264   0    10     272   2   31-34     280   4  115-130
   //
   //      Extra
   // Code Bits Lengths
   // ---- ---- ------
   //  ...  ..    ...
   //  284   5  227-258
   //  285  16  3-65538
   //
   //	extra_lbits = Bits column
   //

{$IFDEF DEFLATE64}
   extra_lbits: Array[0..30] Of _int =
      (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4,
      5, 5, 5, 5, 16, 99, 99);
{$ELSE}
   extra_lbits: Array[0..LENGTH_CODES - 1] Of _int =
   	(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4,
      5, 5, 5, 5, 0);
{$ENDIF}


   { extra bits for each bit length code }
   extra_blbits: Array[0..BL_CODES - 1] Of _int = (0, 0, 0, 0, 0, 0, 0, 0, 0,
   	0, 0, 0, 0, 0, 0, 0, 2, 3, 7);

   bl_order: Array[0..BL_CODES - 1] Of uch = (16, 17, 18, 0, 8, 7, 9, 6, 10,
   	5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

   base_length: Array[0..LENGTH_CODES - 1] Of _int =
   	(0, 1, 2, 3, 4, 5, 6,  7,  8, 10, 12, 14, 16, 20, 24, 28, 32, 40,
      48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 0);

  	//base_length: array [0..28] of word =
   //	(3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43,
   // 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 3);

   { note: the final 3 are for deflate64 only; for symbol 285,}
   {       lengths are stored as length - 3}


   static_l_desc: static_tree_desc =
   (static_tree: (@(static_ltree));     		{ pointer to array of ct_data }
      extra_bits: (@(extra_lbits));     		{ pointer to array of _int }
      extra_base: LITERALS + 1;
      elems: L_CODES;
      max_length: MAX_BITS);

   static_d_desc: static_tree_desc =
   (static_tree: (@(static_dtree));
      extra_bits: (@(extra_dbits));
      extra_base: 0;
{$IFDEF DEFLATE64}
      elems: D_CODES_DEF64;
{$ELSE}
      elems: D_CODES;
{$ENDIF}
      max_length: MAX_BITS);

   static_bl_desc: static_tree_desc =
   (static_tree: (Nil);
      extra_bits: @(extra_blbits);
      extra_base: 0;
      elems: BL_CODES;
      max_length: MAX_BL_BITS);

//-------------------------------------------------------------

Procedure INSERT_STRING(Var s: deflate_state; str: word; Var match_head: IPos);
Begin
{$IFDEF FASTEST}
   s.ins_h := ((s.ins_h Shl s.hash_shift) Xor
      (s.window^[(str) + (MIN_MATCH - 1)])) And s.hash_mask;

   match_head := s.head[s.ins_h];
   s.head[s.ins_h] := _Pos(str);
{$ELSE}
   s.ins_h := ((s.ins_h Shl s.hash_shift) Xor
      (s.window^[(str) + (MIN_MATCH - 1)])) And s.hash_mask;

   match_head := s.head^[s.ins_h];
   s.prev^[(str) And s.w_mask] := match_head;
   s.head^[s.ins_h] := _Pos(str);
{$ENDIF}
End;
//-------------------------------------------------------------

Function ZALLOC(Var strm: ztv_stream; Items: uInt; size: uInt): voidpf;
Begin
   ZALLOC := strm.ZALLOC(strm.opaque, Items, size);
End;
//-------------------------------------------------------------

Procedure ZFREE(Var strm: ztv_stream; ptr: voidpf);
Begin
   strm.ZFREE(strm.opaque, ptr);
End;
//-------------------------------------------------------------

Function bi_reverse(code: _unsigned;    { the value to invert }
   Len: _int): _unsigned;               { its bit length }

Var
   res: _unsigned;                      {register}
Begin
   res := 0;
   Repeat
      res := res Or (code And 1);
      code := code Shr 1;
      res := res Shl 1;
      Dec(Len);
   Until (Len <= 0);
   bi_reverse := res Shr 1;
End;
//-------------------------------------------------------------

Procedure gen_codes(tree: tree_ptr; max_code: _int; Var bl_count: Array Of ushf);
Var
   next_code: Array[0..MAX_BITS + 1 - 1] Of ush;
   code: ush;
   BITS: _int;
   n: _int;
Var
   Len: _int;
Begin
   code := 0;

   For BITS := 1 To MAX_BITS Do
   Begin
      code := ((code + bl_count[BITS - 1]) Shl 1);
      next_code[BITS] := code;
   End;

   For n := 0 To max_code Do
   Begin
      Len := tree^[n].dl.Len;
      If (Len = 0) Then
         Continue;

      { reverse the bits }
      tree^[n].fc.code := bi_reverse(next_code[Len], Len);
      inc(next_code[Len]);
   End;
End;
//-------------------------------------------------------------

Procedure tr_static_init;
{$IFDEF DEFLATE64}
{$define GEN_TREES_H}	//deflate64
{$ELSE}
{$UNDEF GEN_TREES_H}
{$ENDIF}
{$IFDEF GEN_TREES_H}
Const
   static_init_done: Boolean = False;
Var
   n: _int;                             { iterates over tree elements }
   BITS: _int;                          { bit counter }
   bLength: _int;                       { length value }
   code: _int;                          { code value }
   dist: _int;                          { distance index }
   bl_count: Array[0..MAX_BITS + 1 - 1] Of ush;
Begin
   If (static_init_done) Then
      Exit;

   bLength := 0;

   code := 0;
   While code < LENGTH_CODES - 1 Do
   Begin
      base_length[code] := bLength;
      For n := 0 To (1 Shl extra_lbits[code]) - 1 Do
      Begin
         _length_code[bLength] := uch(code);
         inc(bLength);
      End;
      Inc(Code);
   End;

//   For code := 0 To LENGTH_CODES - 1 - 1 Do
//   Begin
//      base_length[code] := bLength;
//      For n := 0 To (1 Shl extra_lbits[code]) - 1 Do
//      Begin
//         _length_code[bLength] := uch(code);
//         inc(bLength);
//      End;
//   End;

   Assert(bLength = 256, 'tr_static_init: blength <> 256');

   _length_code[bLength - 1] := uch(code);

   dist := 0;
   For code := 0 To 16 - 1 Do
   Begin
      base_dist[code] := dist;
      For n := 0 To (1 Shl extra_dbits[code]) - 1 Do
      Begin
         _dist_code[dist] := uch(code);
         inc(dist);
      End;
   End;

   Assert(dist = 256, 'tr_static_init: dist <> 256');
   dist := dist Shr 7;                  { from this point, distances are divided by 128 }

//{$IFDEF DEFLATE64}
//   For code := 16 To D_CODES_DEF64 - 1 Do
//{$ELSE}
   For code := 16 To D_CODES - 1 Do
//{$ENDIF}
   Begin
      base_dist[code] := dist Shl 7;
      For n := 0 To (1 Shl (extra_dbits[code] - 7)) - 1 Do
      Begin
         _dist_code[256 + dist] := uch(code);
         inc(dist);
      End;
   End;

   Assert(dist = 256, 'tr_static_init: 256+dist <> 512');

   For BITS := 0 To MAX_BITS Do
      bl_count[BITS] := 0;

   n := 0;
   While (n <= 143) Do
   Begin
      static_ltree[n].dl.Len := 8;
      inc(n);
      inc(bl_count[8]);
   End;

   While (n <= 255) Do
   Begin
      static_ltree[n].dl.Len := 9;
      inc(n);
      inc(bl_count[9]);
   End;

   While (n <= 279) Do
   Begin
      static_ltree[n].dl.Len := 7;
      inc(n);
      inc(bl_count[7]);
   End;

//{$IFDEF DEFLATE64}
//   While (n <= 287) Do
//{$ELSE}
   While (n <= 284) Do
//{$ENDIF}
   Begin
      static_ltree[n].dl.Len := 8;
      inc(n);
      inc(bl_count[8]);
   End;

{$IFDEF DEFLATE64}
  	//if FUseDeflate64 then
      static_ltree[285].dl.len := 8 + 16;
      //static_ltree[285].dl.len := 8;
{$ELSE}
  	//else
   	static_ltree[285].dl.len := 8;
{$ENDIF}

   gen_codes(tree_ptr(@static_ltree), L_CODES + 1, bl_count);

{$IFDEF DEFLATE64}
   For n := 0 To D_CODES_DEF64 - 1 Do
{$ELSE}
   For n := 0 To D_CODES - 1 Do
{$ENDIF}
   Begin
      static_dtree[n].dl.Len := 5;
      static_dtree[n].fc.code := bi_reverse(n, 5);
   End;
   static_init_done := True;

   //gen_trees_header;
{$ELSE}
Begin
{$ENDIF}
End;
//-------------------------------------------------------------

Procedure init_block(Var s: deflate_state);
Var
   n: _int;                             { iterates over tree elements }
Begin
   For n := 0 To L_CODES - 1 Do
      s.dyn_ltree[n].fc.freq := 0;

{$IFDEF DEFLATE64}
   For n := 0 To D_CODES_DEF64 - 1 Do
{$ELSE}
   For n := 0 To D_CODES - 1 Do
{$ENDIF}
      s.dyn_dtree[n].fc.freq := 0;

   For n := 0 To BL_CODES - 1 Do
      s.bl_tree[n].fc.freq := 0;

   s.dyn_ltree[END_BLOCK].fc.freq := 1;
   s.static_len := longint(0);
   s.opt_len := longint(0);
   s.matches := 0;
   s.last_lit := 0;
End;
//-------------------------------------------------------------

Procedure _tr_init(Var s: deflate_state);
Begin
   tr_static_init();

   s.compressed_len := longint(0);
   s.l_desc.dyn_tree := tree_ptr(@s.dyn_ltree);
   s.l_desc.stat_desc := @static_l_desc;
   s.d_desc.dyn_tree := tree_ptr(@s.dyn_dtree);
   s.d_desc.stat_desc := @static_d_desc;
   s.bl_desc.dyn_tree := tree_ptr(@s.bl_tree);
   s.bl_desc.stat_desc := @static_bl_desc;
   s.bi_buf := 0;
   s.bi_valid := 0;
   s.last_eob_len := 8;                 { enough lookahead for inflate }

   init_block(s);
End;
//-------------------------------------------------------------

Procedure TRY_FREE(Var strm: ztv_stream; ptr: voidpf);
Begin
   strm.ZFREE(strm.opaque, ptr);
End;
//-------------------------------------------------------------

Function _deflateEnd(Var strm: ztv_stream): _int;
Var
   Status: _int;
   s: deflate_state_ptr;
Begin
   If (strm.state = Z_NULL) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   s := deflate_state_ptr(strm.state);
   Status := s^.Status;
   If (Status <> INIT_STATE) And (Status <> BUSY_STATE) And
      (Status <> FINISH_STATE) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   { Deallocate in reverse order of allocations: }
   TRY_FREE(strm, s^.pending_buf);
   TRY_FREE(strm, s^.head);
   TRY_FREE(strm, s^.prev);
   TRY_FREE(strm, s^.window);

   ZFREE(strm, s);
   strm.state := Z_NULL;

   If Status = BUSY_STATE Then
      Result := Z_DATA_ERROR
   Else
      Result := Z_OK;
End;
//-------------------------------------------------------------

Function deflateReset(Var strm: ztv_stream): _int;
Var
   s: deflate_state_ptr;
Begin
   If                                   {(@strm = Z_NULL) or}
      (strm.state = Z_NULL)
      Or (Not Assigned(strm.ZALLOC)) Or (Not Assigned(strm.ZFREE)) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   strm.total_out := 0;
   strm.total_in := 0;
   //strm.msg := '';
   strm.data_type := Z_UNKNOWN;

   s := deflate_state_ptr(strm.state);
   s^.pending := 0;
   s^.pending_out := _pBytef(s^.pending_buf);

   If (s^.noheader < 0) Then
      s^.noheader := 0;

   If s^.noheader <> 0 Then
      s^.Status := BUSY_STATE
   Else
      s^.Status := INIT_STATE;

   strm.adler := 1;
   s^.last_flush := Z_NO_FLUSH;

   _tr_init(s^);
   lm_init(s^);

   Result := Z_OK;
End;
//-------------------------------------------------------------

Function deflateInit2_(Var strm: ztv_stream;
   level: _int;
   method: _int;
   windowBits: _int;
   memLevel: _int;
   strategy: _int;
   stream_size: _int): _int;
Var
   s: deflate_state_ptr;
   noheader: _int;
   overlay: pushfArray;
Begin
   noheader := 0;
   If (stream_size <> SizeOf(ztv_stream)) Then
   Begin
      Result := Z_VERSION_ERROR;
      Exit;
   End;

   //strm.msg := '';
   If Not Assigned(strm.ZALLOC) Then
   Begin
      strm.ZALLOC := @ztvAllocMem;
      strm.opaque := voidpf(0);
   End;
   If Not Assigned(strm.ZFREE) Then
      strm.ZFREE := @ztvFreeMem;

   If (level = Z_DEFAULT_COMPRESSION) Then
      level := ztvDeflateN;
{$IFDEF FASTEST}
   level := 1;
{$ENDIF}

   If (windowBits < 0) Then 
   Begin
      noheader := 1;
      windowBits := -windowBits;
   End;

{$IFDEF DEFLATE64}
   If (memLevel < 1) Or (memLevel > MAX_MEM_LEVEL) Or (method <> Z_DEFLATE64)
{$ELSE}
   If (memLevel < 1) Or (memLevel > MAX_MEM_LEVEL) Or (method <> Z_DEFLATED)
{$ENDIF}
      Or (windowBits < 8) Or (windowBits > 15) Or (level < 0)
      Or (level > 9) Or (strategy < 0) Or (strategy > Z_HUFFMAN_ONLY) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   s := deflate_state_ptr(ZALLOC(strm, 1, SizeOf(deflate_state)));
   If (s = Z_NULL) Then
   Begin
      Result := Z_MEM_ERROR;
      Exit;
   End;

   strm.state := pInternal_state(s);
   s^.strm := @strm;
   s^.noheader := noheader;
   s^.w_bits := windowBits;

{$IFDEF DEFLATE64}
   s^.w_size := 2 Shl s^.w_bits;
{$ELSE}
   s^.w_size := 1 Shl s^.w_bits;
{$ENDIF}

   s^.w_mask := s^.w_size - 1;
   s^.hash_bits := memLevel + 7;
   s^.hash_size := 1 Shl s^.hash_bits;
   s^.hash_mask := s^.hash_size - 1;
   s^.hash_shift := ((s^.hash_bits + MIN_MATCH - 1) Div MIN_MATCH);
   s^.window := pzByteArray(ZALLOC(strm, s^.w_size, 2 * SizeOf(Byte)));
   s^.prev := pzPosfArray(ZALLOC(strm, s^.w_size, SizeOf(_Pos)));
   s^.head := pzPosfArray(ZALLOC(strm, s^.hash_size, SizeOf(_Pos)));
{$IFDEF DEFLATE64}
   s^.lit_bufsize := 1 Shl (memLevel + 7); { 32K elements by default }
   //deflate64
{$ELSE}
   s^.lit_bufsize := 1 Shl (memLevel + 6); { 16K elements by default }
{$ENDIF}

   overlay := pushfArray(ZALLOC(strm, s^.lit_bufsize, SizeOf(ush) + 2));
   s^.pending_buf := pzByteArray(overlay);
   s^.pending_buf_size := longint(s^.lit_bufsize) * (SizeOf(ush) + longint(2));

   If (s^.window = Z_NULL) Or (s^.prev = Z_NULL) Or (s^.head = Z_NULL)
      Or (s^.pending_buf = Z_NULL) Then
   Begin
      //strm.msg := z_errmsg[z_errbase - Z_MEM_ERROR];
      _deflateEnd(strm);
      Result := Z_MEM_ERROR;
      Exit;
   End;

   s^.d_buf := pushfArray(@overlay^[s^.lit_bufsize Div SizeOf(ush)]);
   s^.l_buf := puchfArray(@s^.pending_buf^[(1 + SizeOf(ush)) * s^.lit_bufsize]);
   s^.level := level;
   s^.strategy := strategy;
   s^.method := Byte(method);

   Result := deflateReset(strm);
End;
//-------------------------------------------------------------

Function deflateInit2(Var strm: ztv_stream; level: _int; method: _int;
   windowBits: _int; memLevel: _int; strategy: _int): _int;
Begin
   Result := deflateInit2_(strm, level, method, windowBits, memLevel,
      strategy, SizeOf(ztv_stream));
End;
//-------------------------------------------------------------

Function deflateInit_(strm: ztv_streamp;
   level: _int;
   MAX_WBITS: smallint;
   stream_size: _int): _int;
Begin
   If (strm = Z_NULL) Then
      Result := Z_STREAM_ERROR
   Else
      Result := deflateInit2_(strm^, level, Z_DEFLATED, MAX_WBITS,
         DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, stream_size);
End;
//-------------------------------------------------------------

{Function _deflateInit( Var strm: ztv_stream; level: _int; MAX_WBITS: SmallInt ): _int;
Begin
   Result := deflateInit2_( strm, level, Z_DEFLATED, MAX_WBITS,
      DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, SizeOf( ztv_stream ) );
End;}
//-------------------------------------------------------------

Function deflateSetDictionary(Var strm: ztv_stream; dictionary: ztvStreams._pBytef;
   dictLength: word): _int;
Var
   s: deflate_state_ptr;
   Length: word;
   n: word;
   hash_head: IPos;
Var
   MAX_DIST: word;
Begin
   Length := dictLength;
   hash_head := 0;

   If (strm.state = Z_NULL) Or (dictionary = Z_NULL)
      Or (deflate_state_ptr(strm.state)^.Status <> INIT_STATE) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   s := deflate_state_ptr(strm.state);
   strm.adler := _adler(strm.adler, dictionary, dictLength);

   If (Length < MIN_MATCH) Then
   Begin
      Result := Z_OK;
      Exit;
   End;

{$IFDEF DEFLATE64}
   MAX_DIST := (s^.w_size - MIN_LOOKAHEAD_DEF64);
{$ELSE}
   MAX_DIST := (s^.w_size - MIN_LOOKAHEAD);
{$ENDIF}

   If (Length > MAX_DIST) Then
   Begin
      Length := MAX_DIST;
{$IFNDEF USE_DICT_HEAD}
      inc(dictionary, dictLength - Length); { use the tail of the dictionary }
{$ENDIF}
   End;

   CopyMem(@dictionary^, @s^.window^, Length);

   s^.strstart := Length;
   s^.block_start := longint(Length);
   s^.ins_h := s^.window^[0];
   s^.ins_h := ((s^.ins_h Shl s^.hash_shift) Xor (s^.window^[1]))
      And s^.hash_mask;

   For n := 0 To Length - MIN_MATCH Do
   Begin
      INSERT_STRING(s^, n, hash_head);
   End;

   Result := Z_OK;
End;
//-------------------------------------------------------------

{Function _deflateParams( Var strm: ztv_stream; level: _int; strategy: _int ): _int;
Var
   s: deflate_state_ptr;
   func: compress_func;
   err: _int;
Begin
   err := Z_OK;
   If ( strm.state = Z_NULL ) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   s := deflate_state_ptr( strm.state );

   If ( level = Z_DEFAULT_COMPRESSION ) Then
      level := ztvDeflateN;

   If ( level < 0 ) Or ( level > 9 ) Or ( strategy < 0 )
      Or ( strategy > Z_HUFFMAN_ONLY ) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   func := configuration_table[s^.level].func;

   If ( @func <> @configuration_table[level].func ) And ( strm.total_in <> 0 ) Then
      err := _deflate( strm, Z_PARTIAL_FLUSH );

   If ( s^.level <> level ) Then
   Begin
      s^.level := level;
      s^.max_lazy_match := configuration_table[level].max_lazy;
      s^.good_match := configuration_table[level].good_length;
      s^.nice_match := configuration_table[level].nice_length;
      s^.max_chain_length := configuration_table[level].max_chain;
   End;

   s^.strategy := strategy;
   Result := err;
End;}
//-------------------------------------------------------------

Procedure putShortMSB(Var s: deflate_state; b: word);
Begin
   s.pending_buf^[s.pending] := Byte(b Shr 8);
   inc(s.pending);
   s.pending_buf^[s.pending] := Byte(b And $FF);
   inc(s.pending);
End;
//-------------------------------------------------------------

Procedure flush_pending(Var strm: ztv_stream);
Var
   Len: _unsigned;
   s: deflate_state_ptr;
Begin
   s := deflate_state_ptr(strm.state);
   Len := s^.pending;

   If (Len > strm.avail_out) Then
      Len := strm.avail_out;

   If (Len = 0) Then
      Exit;

   CopyMem(@s^.pending_out^, @strm.next_out^, Len);

   inc(strm.next_out, Len);
   inc(s^.pending_out, Len);
   strm.total_out := strm.total_out + Len;
   Dec(strm.avail_out, Len);
   Dec(s^.pending, Len);

   If (s^.pending = 0) Then
      s^.pending_out := _pBytef(s^.pending_buf);

End;
//-------------------------------------------------------------

Procedure send_bits(Var s: deflate_state;
   value: _int;                         { value to send }
   Length: _int);                       { number of bits }
Begin
{$IFOPT Q+}{$Q-}{$DEFINE NoOverflowCheck}{$ENDIF}
{$IFOPT R+}{$R-}{$DEFINE NoRangeCheck}{$ENDIF}
   If (s.bi_valid > _int(BUF_SIZE) - Length) Then
   Begin
      s.bi_buf := s.bi_buf Or _int(value Shl s.bi_valid);
      s.pending_buf^[s.pending] := uch(s.bi_buf And $FF);
      inc(s.pending);
      s.pending_buf^[s.pending] := uch(ush(s.bi_buf) Shr 8);
      ;
      inc(s.pending);

      s.bi_buf := ush(value) Shr (BUF_SIZE - s.bi_valid);
      inc(s.bi_valid, Length - BUF_SIZE);
   End
   Else
   Begin
      s.bi_buf := s.bi_buf Or _int(value Shl s.bi_valid);
      inc(s.bi_valid, Length);
   End;
{$IFDEF NoOverflowCheck}{$Q+}{$UNDEF NoOverflowCheck}{$ENDIF}
{$IFDEF NoRangeCheck}{$Q+}{$UNDEF NoRangeCheck}{$ENDIF}
End;
//-------------------------------------------------------------

Procedure bi_flush(Var s: deflate_state);
Begin
   If (s.bi_valid = 16) Then
   Begin
      s.pending_buf^[s.pending] := uch(s.bi_buf And $FF);
      inc(s.pending);
      s.pending_buf^[s.pending] := uch(ush(s.bi_buf) Shr 8);
      inc(s.pending);

      s.bi_buf := 0;
      s.bi_valid := 0;
   End
   Else
      If (s.bi_valid >= 8) Then
      Begin
         s.pending_buf^[s.pending] := Byte(s.bi_buf);
         inc(s.pending);
         s.bi_buf := s.bi_buf Shr 8;
         Dec(s.bi_valid, 8);
      End;
End;
//-------------------------------------------------------------

Procedure _tr_align(Var s: deflate_state);
Begin
   send_bits(s, STATIC_TREES Shl 1, 3);
   send_bits(s, static_ltree[END_BLOCK].fc.code, static_ltree[END_BLOCK].dl.Len);
   inc(s.compressed_len, longint(10));  { 3 for block type, 7 for EOB }
   bi_flush(s);

   If (1 + s.last_eob_len + 10 - s.bi_valid < 9) Then
   Begin
      send_bits(s, STATIC_TREES Shl 1, 3);
      send_bits(s, static_ltree[END_BLOCK].fc.code, static_ltree[END_BLOCK].dl.Len);
      inc(s.compressed_len, longint(10));
      bi_flush(s);
   End;
   s.last_eob_len := 7;
End;
//-------------------------------------------------------------

Procedure bi_windup(Var s: deflate_state);
Begin
   If (s.bi_valid > 8) Then
   Begin
      s.pending_buf^[s.pending] := uch(s.bi_buf And $FF);
      inc(s.pending);
      s.pending_buf^[s.pending] := uch(ush(s.bi_buf) Shr 8);
      inc(s.pending);
   End
   Else
      If (s.bi_valid > 0) Then
      Begin
         s.pending_buf^[s.pending] := Byte(s.bi_buf);
         inc(s.pending);
      End;
   s.bi_buf := 0;
   s.bi_valid := 0;
End;
//-------------------------------------------------------------

Procedure copy_block(Var s: deflate_state; Buf: pcharf; Len: _unsigned; Header: Boolean);
Begin
   bi_windup(s);                        { align on byte boundary }
   s.last_eob_len := 8;                 { enough lookahead for inflate }

   If (Header) Then
   Begin
      s.pending_buf^[s.pending] := uch(ush(Len) And $FF);

      inc(s.pending);
      s.pending_buf^[s.pending] := uch(ush(Len) Shr 8);

      inc(s.pending);
      s.pending_buf^[s.pending] := uch(ush(Not Len) And $FF);

      inc(s.pending);
      s.pending_buf^[s.pending] := uch(ush(Not Len) Shr 8);

      inc(s.pending);
   End;

   While (Len <> 0) Do
   Begin
      Dec(Len);
      s.pending_buf^[s.pending] := Buf^;
      inc(Buf);
      inc(s.pending);
   End;
End;
//-------------------------------------------------------------

Procedure _tr_stored_block(Var s: deflate_state; Buf: pcharf; stored_len: ulg;
   EOF: Boolean);
Begin
   send_bits(s, (STORED_BLOCK Shl 1) + ord(EOF), 3); { send block type }
   s.compressed_len := (s.compressed_len + 3 + 7) And ulg(Not longint(7));
   inc(s.compressed_len, (stored_len + 4) Shl 3);
   copy_block(s, Buf, _unsigned(stored_len), True); { with header }
End;
//-------------------------------------------------------------

Procedure zmemzero(destp: _pBytef; Len: uInt);
Begin
   FillChar(destp^, Len, 0);
End;
//-------------------------------------------------------------

Function _deflate(Var strm: ztv_stream; Flush: _int): _int;
Var
   old_flush: _int;
   s: deflate_state_ptr;
   Header: word;
   level_flags: word;
   bstate: block_state;
Begin

   If (strm.cb.pCancel^) Or
      (strm.state = Z_NULL) Or
      (Flush > Z_FINISH) Or
      (Flush < 0) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   s := deflate_state_ptr(strm.state);

   If (strm.next_out = Z_NULL) Or
      ((strm.next_in = Z_NULL) And (strm.avail_in <> 0)) Or
      ((s^.Status = FINISH_STATE) And (Flush <> Z_FINISH)) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   If (strm.avail_out = 0) Then
   Begin
      Result := Z_BUF_ERROR;
      Exit;
   End;

   s^.strm := @strm;                    { just in case }
   old_flush := s^.last_flush;
   s^.last_flush := Flush;

   { Write the lib header }
   If (s^.Status = INIT_STATE) Then
   Begin
      Header := (Z_DEFLATED + ((s^.w_bits - 8) Shl 4)) Shl 8;
      level_flags := (s^.level - 1) Shr 1;

      If (level_flags > 3) Then
         level_flags := 3;

      Header := Header Or (level_flags Shl 6);

      If (s^.strstart <> 0) Then
         Header := Header Or PRESET_DICT;

      inc(Header, 31 - (Header Mod 31));
      s^.Status := BUSY_STATE;
      putShortMSB(s^, Header);

      { Save the adler32 of the preset dictionary: }
      If (s^.strstart <> 0) Then
      Begin
         putShortMSB(s^, word(strm.adler Shr 16));
         putShortMSB(s^, word(strm.adler And $FFFF));
      End;

      strm.adler := Long(1);
   End;

   If (s^.pending <> 0) Then
   Begin

      flush_pending(strm);
      If (strm.avail_out = 0) Then
      Begin
         s^.last_flush := -1;
         Result := Z_OK;
         Exit;
      End;

   End
   Else
      If (strm.avail_in = 0) And (Flush <= old_flush)
      And (Flush <> Z_FINISH) Then
      Begin
         Result := Z_BUF_ERROR;	//strm.msg := z_errmsg[z_errbase - Z_BUF_ERROR];
         Exit;
      End;

   If (s^.Status = FINISH_STATE) And (strm.avail_in <> 0) Then
   Begin
      Result := Z_BUF_ERROR;	//strm.msg := z_errmsg[z_errbase - Z_BUF_ERROR];
      Exit;
   End;

   If (strm.avail_in <> 0) Or (s^.lookahead <> 0)
      Or ((Flush <> Z_NO_FLUSH) And (s^.Status <> FINISH_STATE)) Then
   Begin
      bstate := configuration_table[s^.level].func(s^, Flush);

      If (bstate = finish_started) Or (bstate = finish_done) Then
         s^.Status := FINISH_STATE;

      If (bstate = need_more) Or (bstate = finish_started) Then
      Begin
         If (strm.avail_out = 0) Then
            s^.last_flush := -1;

         Result := Z_OK;
         Exit;
      End;

      If (bstate = block_done) Then
      Begin
         If (Flush = Z_PARTIAL_FLUSH) Then
            _tr_align(s^)
         Else
         Begin                          { FULL_FLUSH or SYNC_FLUSH }
            _tr_stored_block(s^, pcharf(Nil), longint(0), False);

            If (Flush = Z_FULL_FLUSH) Then
            Begin
               s^.head^[s^.hash_size - 1] := ZNIL;
               zmemzero(_pBytef(s^.head), _unsigned(s^.hash_size - 1) * SizeOf(s^.head^[0]));
            End;
         End;

         flush_pending(strm);
         If (strm.avail_out = 0) Then
         Begin
            s^.last_flush := -1;
            Result := Z_OK;
            Exit;
         End;

      End;
   End;

   If (Flush <> Z_FINISH) Then
   Begin
      Result := Z_OK;
      Exit;
   End;

   If (s^.noheader <> 0) Then
   Begin
      Result := Z_STREAM_END;
      Exit;
   End;

   { Write the lib trailer (adler32) }
   putShortMSB(s^, word(strm.adler Shr 16));
   putShortMSB(s^, word(strm.adler And $FFFF));
   flush_pending(strm);

   s^.noheader := -1;
   If (s^.pending <> 0) Then
      Result := Z_OK
   Else
      Result := Z_STREAM_END;
End;
//-------------------------------------------------------------

Function deflateCopy(dest, source: ztv_streamp): _int;
{$IFNDEF MAXSEG_64K}
Var
   ds: deflate_state_ptr;
   ss: deflate_state_ptr;
   overlay: pushfArray;
{$ENDIF}
Begin
{$IFDEF MAXSEG_64K}
   Result := Z_STREAM_ERROR;
   Exit;
{$ELSE}

   If (source = Z_NULL) Or (dest = Z_NULL) Or (source^.state = Z_NULL) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;
   ss := deflate_state_ptr(source^.state);
   dest^ := source^;

   ds := deflate_state_ptr(ZALLOC(dest^, 1, SizeOf(deflate_state)));
   If (ds = Z_NULL) Then
   Begin
      Result := Z_MEM_ERROR;
      Exit;
   End;
   dest^.state := pInternal_state(ds);
   ds^ := ss^;
   ds^.strm := dest;

   ds^.window := pzByteArray(ZALLOC(dest^, ds^.w_size, 2 * SizeOf(Byte)));
   ds^.prev := pzPosfArray(ZALLOC(dest^, ds^.w_size, SizeOf(_Pos)));
   ds^.head := pzPosfArray(ZALLOC(dest^, ds^.hash_size, SizeOf(_Pos)));
   overlay := pushfArray(ZALLOC(dest^, ds^.lit_bufsize, SizeOf(ush) + 2));
   ds^.pending_buf := pzByteArray(overlay);

   If (ds^.window = Z_NULL) Or (ds^.prev = Z_NULL) Or (ds^.head = Z_NULL)
      Or (ds^.pending_buf = Z_NULL) Then
   Begin
      _deflateEnd(dest^);
      Result := Z_MEM_ERROR;
      Exit;
   End;

   CopyMem(@ss^.window^, @ds^.window^, ds^.w_size * 2 * SizeOf(Byte));
   CopyMem(@ss^.prev^, @ds^.prev^, ds^.w_size * SizeOf(_Pos));
   CopyMem(@ss^.head^, @ds^.head^, ds^.hash_size * SizeOf(_Pos));
   CopyMem(@ss^.pending_buf^, @ds^.pending_buf^, word(ds^.pending_buf_size));

   ds^.pending_out := @ds^.pending_buf^[ptr2int(ss^.pending_out) - ptr2int(ss^.pending_buf)];
   ds^.d_buf := pushfArray(@overlay^[ds^.lit_bufsize Div SizeOf(ush)]);
   ds^.l_buf := puchfArray(@ds^.pending_buf^[(1 + SizeOf(ush)) * ds^.lit_bufsize]);

   ds^.l_desc.dyn_tree := tree_ptr(@ds^.dyn_ltree);
   ds^.d_desc.dyn_tree := tree_ptr(@ds^.dyn_dtree);
   ds^.bl_desc.dyn_tree := tree_ptr(@ds^.bl_tree);

   Result := Z_OK;
{$ENDIF}
End;
//-------------------------------------------------------------

Function read_buf(strm: ztv_streamp; Buf: _pBytef; size: _unsigned): _int;
Var
   Len: _unsigned;
Begin
   Len := strm^.avail_in;

   If (Len > size) Then
      Len := size;

   If (Len = 0) Then
   Begin
      Result := 0;
      Exit;
   End;

   Dec(strm^.avail_in, Len);

   If deflate_state_ptr(strm^.state)^.noheader = 0 Then
      strm^.adler := _adler(strm^.adler, strm^.next_in, Len);

   CopyMem(@strm^.next_in^, @Buf^, Len);

   If (Not strm^.cb.Protect) Then
   	Crc32_buf(@Buf^, Len, strm^.cb.CRC);

   inc(strm^.next_in, Len);
   strm^.total_in := strm^.total_in + Len;

   Result := _int(Len);
End;
//-------------------------------------------------------------

Procedure lm_init(Var s: deflate_state);
Begin
   s.window_size := longint(u_long(2) * u_long(s.w_size));
   s.head^[s.hash_size - 1] := ZNIL;
   zmemzero(_pBytef(s.head), _unsigned(s.hash_size - 1) * SizeOf(s.head^[0]));

   { Set the default configuration parameters: }
   s.max_lazy_match := configuration_table[s.level].max_lazy;
   s.good_match := configuration_table[s.level].good_length;
   s.nice_match := configuration_table[s.level].nice_length;
   s.max_chain_length := configuration_table[s.level].max_chain;

   s.strstart := 0;
   s.block_start := longint(0);
   s.lookahead := 0;
   s.prev_length := MIN_MATCH - 1;
   s.match_length := MIN_MATCH - 1;
   s.match_available := False;
   s.ins_h := 0;
End;
//-------------------------------------------------------------

{.$IFNDEF ASMV}
{$IFNDEF FASTEST}

Function longest_match(Var s: deflate_state;
   cur_match: IPos                      { current match }
   ): word;
Label
   nextstep;
Var
   chain_length: _unsigned;             { max hash chain length }
   {register} Scan: _pBytef;            { current string }
   {register} match: _pBytef;           { matched string }
   {register} Len: _int;                { length of current match }
   best_len: _int;                      { best match length so far }
   nice_match: _int;                    { stop if match LongInt enough }
   limit: IPos;

   prev: pzPosfArray;
   wmask: word;
{$IFDEF UNALIGNED_OK}
   {register} strend: _pBytef;
   {register} scan_start: ush;
   {register} scan_end: ush;
{$ELSE}
   {register} strend: _pBytef;
   {register} scan_end1: Byte;
   {register} scan_end: Byte;
{$ENDIF}
Var
   MAX_DIST: word;
Begin
   chain_length := s.max_chain_length;  { max hash chain length }
   Scan := @(s.window^[s.strstart]);
   best_len := s.prev_length;           { best match length so far }
   nice_match := s.nice_match;          { stop if match LongInt enough }

{$IFDEF DEFLATE64}
   MAX_DIST := s.w_size - MIN_LOOKAHEAD_DEF64;
{$ELSE}
   MAX_DIST := s.w_size - MIN_LOOKAHEAD;
{$ENDIF}

   {In order to simplify the code, particularly on 16 bit machines, match
   distances are limited to MAX_DIST instead of WSIZE. }

   If s.strstart > IPos(MAX_DIST) Then
      limit := s.strstart - IPos(MAX_DIST)
   Else
      limit := ZNIL;
   { Stop when cur_match becomes <= limit. To simplify the code,
     we prevent matches with the string of window index 0. }

   prev := s.prev;
   wmask := s.w_mask;

{$IFDEF UNALIGNED_OK}
   { Compare two bytes at a time. Note: this is not always beneficial.
     Try with and without -DUNALIGNED_OK to check. }

   strend := _pBytef(@(s.window^[s.strstart + MAX_MATCH - 1]));
   scan_start := pushf(Scan)^;
   scan_end := pushfArray(Scan)^[best_len - 1]; { fix }
{$ELSE}
   strend := _pBytef(@(s.window^[s.strstart + MAX_MATCH]));
{$IFOPT R+}{$R-}{$DEFINE NoRangeCheck}{$ENDIF}
   scan_end1 := pzByteArray(Scan)^[best_len - 1];
{$IFDEF NoRangeCheck}{$R+}{$UNDEF NoRangeCheck}{$ENDIF}
   scan_end := pzByteArray(Scan)^[best_len];
{$ENDIF}

   If (s.prev_length >= s.good_match) Then
      chain_length := chain_length Shr 2;

   If (word(nice_match) > s.lookahead) Then
      nice_match := s.lookahead;

   Repeat
      match := @(s.window^[cur_match]);

{$UNDEF DO_UNALIGNED_OK}
{$IFDEF UNALIGNED_OK}
{$IFDEF MAX_MATCH_IS_258}
{$DEFINE DO_UNALIGNED_OK}
{$ENDIF}
{$ENDIF}

{$IFDEF DO_UNALIGNED_OK}
      { This code assumes sizeof(_unsigned short) = 2. Do not use
        UNALIGNED_OK if your compiler uses a different size. }
{$IFOPT R+}{$R-}{$DEFINE NoRangeCheck}{$ENDIF}
      If (pushfArray(match)^[best_len - 1] <> scan_end) Or
         (pushf(match)^ <> scan_start) Then
         Goto nextstep;                 {continue;}
{$IFDEF NoRangeCheck}{$R+}{$UNDEF NoRangeCheck}{$ENDIF}

      inc(Scan);
      inc(match);

      Repeat
         inc(Scan, 2);
         inc(match, 2);
         If (pushf(Scan)^ <> pushf(match)^) Then break;
         inc(Scan, 2);
         inc(match, 2);
         If (pushf(Scan)^ <> pushf(match)^) Then break;
         inc(Scan, 2);
         inc(match, 2);
         If (pushf(Scan)^ <> pushf(match)^) Then break;
         inc(Scan, 2);
         inc(match, 2);
         If (pushf(Scan)^ <> pushf(match)^) Then break;
      Until (ptr2int(Scan) >= ptr2int(strend));
      //The "do while" generates better code on most compilers }

      //While (ptr2int(Scan) < ptr2int(strend)) Do
      //Begin
      //   inc(Scan, 2);
      //   inc(match, 2);
      //   If (pushf(Scan)^ <> pushf(match)^) Then break;
      //End;

      If (Scan^ = match^) Then
         inc(Scan);

      Len := (MAX_MATCH - 1) - _int(ptr2int(strend)) + _int(ptr2int(Scan));
      Scan := strend;
      Dec(Scan, (MAX_MATCH - 1));

{$ELSE}                                 { UNALIGNED_OK }

{$IFOPT R+}{$R-}{$DEFINE NoRangeCheck}{$ENDIF}
      If (pzByteArray(match)^[best_len] <> scan_end) Or
         (pzByteArray(match)^[best_len - 1] <> scan_end1) Or
         (match^ <> Scan^) Then
         Goto nextstep;                 {continue;}

{$IFDEF NoRangeCheck}{$R+}{$UNDEF NoRangeCheck}{$ENDIF}
      inc(match);

      If (match^ <> pzByteArray(Scan)^[1]) Then
         Goto nextstep;                 {continue;}

      inc(Scan, 2);
      inc(match);

      Repeat
         inc(Scan);
         inc(match);
         If (Scan^ <> match^) Then break;
         inc(Scan);
         inc(match);
         If (Scan^ <> match^) Then break;
         inc(Scan);
         inc(match);
         If (Scan^ <> match^) Then break;
         inc(Scan);
         inc(match);
         If (Scan^ <> match^) Then break;
         inc(Scan);
         inc(match);
         If (Scan^ <> match^) Then break;
         inc(Scan);
         inc(match);
         If (Scan^ <> match^) Then break;
         inc(Scan);
         inc(match);
         If (Scan^ <> match^) Then break;
         inc(Scan);
         inc(match);
         If (Scan^ <> match^) Then break;
      Until (ptr2int(Scan) >= ptr2int(strend));

      {While (ptr2int(Scan) < ptr2int(strend)) Do
      Begin
         inc(Scan);
         inc(match);
         If (Scan^ <> match^) Then break;
      End;}

      Len := MAX_MATCH - _int(ptr2int(strend) - ptr2int(Scan));
      Scan := strend;
      Dec(Scan, MAX_MATCH);

{$ENDIF}                                { UNALIGNED_OK }

      If (Len > best_len) Then
      Begin
         s.match_start := cur_match;
         best_len := Len;
         If (Len >= nice_match) Then
            break;
{$IFOPT R+}{$R-}{$DEFINE NoRangeCheck}{$ENDIF}
{$IFDEF UNALIGNED_OK}
         scan_end := pzByteArray(Scan)^[best_len - 1];
{$ELSE}
         scan_end1 := pzByteArray(Scan)^[best_len - 1];
         scan_end := pzByteArray(Scan)^[best_len];
{$ENDIF}
{$IFDEF NoRangeCheck}{$R+}{$UNDEF NoRangeCheck}{$ENDIF}
      End;
      nextstep:
      cur_match := prev^[cur_match And wmask];
      Dec(chain_length);
   Until (cur_match <= limit) Or (chain_length = 0);

   If (word(best_len) <= s.lookahead) Then
      longest_match := word(best_len)
   Else
      longest_match := s.lookahead;
End;
{.$ENDIF}{ ASMV }
//-------------------------------------------------------------

{$ELSE}                                 { FASTEST }

Function longest_match(Var s: deflate_state;
   cur_match: IPos                      { current match }
   ): word;
Var
   {register} Scan: _pBytef;            { current string }
   {register} match: _pBytef;           { matched string }
   {register} Len: _int;                { length of current match }
   {register} strend: _pBytef;
Begin
   Scan := @s.window^[s.strstart];
   strend := @s.window^[s.strstart + MAX_MATCH];
   match := s.window + cur_match;

   { Return failure if the match length is less than 2: }
   If (match[0] <> Scan[0]) Or (match[1] <> Scan[1]) Then
   Begin
      longest_match := MIN_MATCH - 1;
      Exit;
   End;

   Scan + = 2, match + = 2;
   Assert(Scan^ = match^, 'match[2]?');

   Repeat
      inc(Scan);
      inc(match);
      If Scan^ <> match^ Then break;
      inc(Scan);
      inc(match);
      If Scan^ <> match^ Then break;
      inc(Scan);
      inc(match);
      If Scan^ <> match^ Then break;
      inc(Scan);
      inc(match);
      If Scan^ <> match^ Then break;
      inc(Scan);
      inc(match);
      If Scan^ <> match^ Then break;
      inc(Scan);
      inc(match);
      If Scan^ <> match^ Then break;
      inc(Scan);
      inc(match);
      If Scan^ <> match^ Then break;
      inc(Scan);
      inc(match);
      If Scan^ <> match^ Then break;
   Until (ptr2int(Scan) >= ptr2int(strend));

   Assert(Scan <= s.window + _unsigned(s.window_size - 1), 'wild scan');

   Len := MAX_MATCH - _int(strend - Scan);

   If (Len < MIN_MATCH) Then
   Begin
      Return := MIN_MATCH - 1;
      Exit;
   End;

   s.match_start := cur_match;
   If Len <= s.lookahead Then
      longest_match := Len
   Else
      longest_match := s.lookahead;
End;
{$ENDIF}                                { FASTEST }
//-------------------------------------------------------------

Procedure fill_window(Var s: deflate_state);
Var
   {register} n, m: _unsigned;
   {register} p: pPosf;
   more: _unsigned;                     { Amount of free space at the end of the window. }
   WSIZE_LOCAL: uInt;
Begin
   WSIZE_LOCAL := s.w_size;
   Repeat
      more := _unsigned(s.window_size - longint(s.lookahead) - longint(s.strstart));

      { Deal with !@#$% 64K limit: }
      If (more = 0) And (s.strstart = 0) And (s.lookahead = 0) Then
         more := WSIZE_LOCAL
      Else
         If (more = _unsigned(-1)) Then
         Begin
            Dec(more);
         End
         Else
{$IFDEF DEFLATE64}
            If (s.strstart >= WSIZE_LOCAL + {MAX_DIST}(WSIZE_LOCAL - MIN_LOOKAHEAD_DEF64)) Then
{$ELSE}
            If (s.strstart >= WSIZE_LOCAL + {MAX_DIST}(WSIZE_LOCAL - MIN_LOOKAHEAD)) Then
{$ENDIF}
            Begin
               CopyMem(@s.window^[WSIZE_LOCAL], @s.window^, _unsigned(WSIZE_LOCAL));

               // the following block results in the failure of compression of some .exe files
               //If WSIZE_LOCAL > s.match_start Then
               //	Dec(s.match_start, WSIZE_LOCAL)
               //Else
               //	s.match_start := 0;
               Dec(s.match_start, WSIZE_LOCAL);

               Dec(s.strstart, WSIZE_LOCAL);  { we now have strstart >= MAX_DIST }
               Dec(s.block_start, longint(WSIZE_LOCAL));

               n := s.hash_size;
               p := @s.head^[n];
               Repeat
                  Dec(p);
                  m := p^;
                  If (m >= WSIZE_LOCAL) Then
                     p^ := _Pos(m - WSIZE_LOCAL)
                  Else
                     p^ := _Pos(ZNIL);
                  Dec(n);
               Until (n = 0);

               n := WSIZE_LOCAL;
{$IFNDEF FASTEST}
               p := @s.prev^[n];

               Repeat
                  Dec(p);
                  m := p^;
                  If (m >= WSIZE_LOCAL) Then
                     p^ := _Pos(m - WSIZE_LOCAL)
                  Else
                     p^ := _Pos(ZNIL);
                  Dec(n);
               Until (n = 0);
{$ENDIF}
               inc(more, WSIZE_LOCAL);
            End;
      If (s.strm^.avail_in = 0) Then
         Exit;

      n := read_buf(s.strm, _pBytef(@(s.window^[s.strstart + s.lookahead])),
         more);

      inc(s.lookahead, n);

      If (s.lookahead >= MIN_MATCH) Then
      Begin
         s.ins_h := s.window^[s.strstart];
         s.ins_h := ((s.ins_h Shl s.hash_shift) Xor s.window^[s.strstart + 1])
            And s.hash_mask;
{$IFDEF MIN_MATCH <> 3}
         Call UPDATE_HASH()MIN_MATCH - 3 more times
{$ENDIF}
      End;
{$IFDEF DEFLATE64}
   Until (s.lookahead >= MIN_LOOKAHEAD_DEF64) Or (s.strm^.avail_in = 0);
{$ELSE}
   Until (s.lookahead >= MIN_LOOKAHEAD) Or (s.strm^.avail_in = 0);
{$ENDIF}
End;
//-------------------------------------------------------------

Procedure set_data_type(Var s: deflate_state);
Var
   n: _int;
   ascii_freq: _unsigned;
   bin_freq: _unsigned;
Begin
   n := 0;
   ascii_freq := 0;
   bin_freq := 0;

   While (n < 7) Do
   Begin
      inc(bin_freq, s.dyn_ltree[n].fc.freq);
      inc(n);
   End;

   While (n < 128) Do
   Begin
      inc(ascii_freq, s.dyn_ltree[n].fc.freq);
      inc(n);
   End;

   While (n < LITERALS) Do
   Begin
      inc(bin_freq, s.dyn_ltree[n].fc.freq);
      inc(n);
   End;

   If (bin_freq > (ascii_freq Shr 2)) Then
      s.data_type := Byte(Z_BINARY)
   Else
      s.data_type := Byte(Z_ASCII);
End;
//-------------------------------------------------------------

Procedure pqdownheap(Var s: deflate_state;
   Var tree: tree_type;                 { the tree to restore }
   k: _int);                            { node to move down }
Var
   v: _int;
   j: _int;
Begin
   v := s.heap[k];
   j := k Shl 1;                        { left son of k }
   While (j <= s.heap_len) Do
   Begin

      If (j < s.heap_len) And
         {smaller(tree, s.heap[j+1], s.heap[j], s.depth)}
      ((tree[s.heap[j + 1]].fc.freq < tree[s.heap[j]].fc.freq) Or
         ((tree[s.heap[j + 1]].fc.freq = tree[s.heap[j]].fc.freq) And
         (s.depth[s.heap[j + 1]] <= s.depth[s.heap[j]]))) Then
      Begin
         inc(j);
      End;

      If ((tree[v].fc.freq < tree[s.heap[j]].fc.freq) Or
         ((tree[v].fc.freq = tree[s.heap[j]].fc.freq) And
         (s.depth[v] <= s.depth[s.heap[j]]))) Then
         break;

      s.heap[k] := s.heap[j];
      k := j;
      j := j Shl 1;
   End;
   s.heap[k] := v;
End;
//-------------------------------------------------------------

Procedure gen_bitlen(Var s: deflate_state;
   Var desc: tree_desc);                { the tree descriptor }
Var
   tree: tree_ptr;
   max_code: _int;
   stree: tree_ptr;                     {const}
   extra: pzIntfArray;                  {const}
   BASE: _int;
   max_length: _int;
   H: _int;                             { heap index }
   n, m: _int;                          { iterate over the tree elements }
   BITS: _int;                          { bit length }
   xbits: _int;                         { extra bits }
   f: ush;                              { frequency }
   overflow: _int;                      { number of elements with bit length too large }
Begin
   tree := desc.dyn_tree;
   max_code := desc.max_code;
   stree := desc.stat_desc^.static_tree;
   extra := desc.stat_desc^.extra_bits;
   BASE := desc.stat_desc^.extra_base;
   max_length := desc.stat_desc^.max_length;
   overflow := 0;

   For BITS := 0 To MAX_BITS Do
      s.bl_count[BITS] := 0;

   tree^[s.heap[s.heap_max]].dl.Len := 0; { root of the heap }

   For H := s.heap_max + 1 To HEAP_SIZE - 1 Do
   Begin
      n := s.heap[H];
      BITS := tree^[tree^[n].dl.dad].dl.Len + 1;
      If (BITS > max_length) Then
      Begin
         BITS := max_length;
         inc(overflow);
      End;
      tree^[n].dl.Len := ush(BITS);

      If (n > max_code) Then
         Continue;                      { not a leaf node }

      inc(s.bl_count[BITS]);
      xbits := 0;
      If (n >= BASE) Then
         xbits := extra^[n - BASE];

      f := tree^[n].fc.freq;
      inc(s.opt_len, ulg(f) * (BITS + xbits));

      If (stree <> Nil) Then
         inc(s.static_len, ulg(f) * (stree^[n].dl.Len + xbits));
   End;

   If (overflow = 0) Then
      Exit;

   Repeat
      BITS := max_length - 1;
      While (s.bl_count[BITS] = 0) Do
         Dec(BITS);
      Dec(s.bl_count[BITS]);            { move one leaf down the tree }
      inc(s.bl_count[BITS + 1], 2);     { move one overflow item as its brother }
      Dec(s.bl_count[max_length]);
      Dec(overflow, 2);
   Until (overflow <= 0);

   H := HEAP_SIZE;                      { Delphi3: compiler warning w/o this }
   For BITS := max_length Downto 1 Do
   Begin
      n := s.bl_count[BITS];
      While (n <> 0) Do
      Begin
         Dec(H);
         m := s.heap[H];
         If (m > max_code) Then
            Continue;
         If (tree^[m].dl.Len <> _unsigned(BITS)) Then
         Begin
            inc(s.opt_len, (longint(BITS) - longint(tree^[m].dl.Len))
               * longint(tree^[m].fc.freq));
            tree^[m].dl.Len := ush(BITS);
         End;
         Dec(n);
      End;
   End;
End;
//-------------------------------------------------------------

Procedure build_tree(Var s: deflate_state; Var desc: tree_desc);
Var
   tree: tree_ptr;
   stree: tree_ptr;                     {const}
   elems: _int;
   n, m: _int;                          { iterate over heap elements }
   max_code: _int;                      { largest code with non zero frequency }
   node: _int;                          { new node being created }
Begin
   tree := desc.dyn_tree;
   stree := desc.stat_desc^.static_tree;
   elems := desc.stat_desc^.elems;
   max_code := -1;

   s.heap_len := 0;
   s.heap_max := HEAP_SIZE;

   For n := 0 To elems - 1 Do
   Begin
      If (tree^[n].fc.freq <> 0) Then
      Begin
         max_code := n;
         inc(s.heap_len);
         s.heap[s.heap_len] := n;
         s.depth[n] := 0;
      End
      Else
      Begin
         tree^[n].dl.Len := 0;
      End;
   End;

   While (s.heap_len < 2) Do
   Begin
      inc(s.heap_len);
      If (max_code < 2) Then
      Begin
         inc(max_code);
         s.heap[s.heap_len] := max_code;
         node := max_code;
      End
      Else
      Begin
         s.heap[s.heap_len] := 0;
         node := 0;
      End;
      tree^[node].fc.freq := 1;
      s.depth[node] := 0;
      Dec(s.opt_len);
      If (stree <> Nil) Then
         Dec(s.static_len, stree^[node].dl.Len);
   End;
   desc.max_code := max_code;

   For n := s.heap_len Div 2 Downto 1 Do
      pqdownheap(s, tree^, n);

   node := elems;

   Repeat
      n := s.heap[SMALLEST];
      s.heap[SMALLEST] := s.heap[s.heap_len];
      Dec(s.heap_len);
      pqdownheap(s, tree^, SMALLEST);

      m := s.heap[SMALLEST];

      Dec(s.heap_max);
      s.heap[s.heap_max] := n;
      Dec(s.heap_max);
      s.heap[s.heap_max] := m;

      tree^[node].fc.freq := tree^[n].fc.freq + tree^[m].fc.freq;
      If (s.depth[n] >= s.depth[m]) Then
         s.depth[node] := uch(s.depth[n] + 1)
      Else
         s.depth[node] := uch(s.depth[m] + 1);

      tree^[m].dl.dad := ush(node);
      tree^[n].dl.dad := ush(node);

{$IFDEF DUMP_BL_TREE}
      If (tree = tree_ptr(@s.bl_tree)) Then
      Begin
         Writeln(#13'node ', node, '(', tree^[node].fc.freq, ') sons ', n,
            '(', tree^[n].fc.freq, ') ', m, '(', tree^[m].fc.freq, ')');
      End;
{$ENDIF}

      s.heap[SMALLEST] := node;
      inc(node);
      pqdownheap(s, tree^, SMALLEST);

   Until (s.heap_len < 2);

   Dec(s.heap_max);
   s.heap[s.heap_max] := s.heap[SMALLEST];
   gen_bitlen(s, desc);
   gen_codes(tree, max_code, s.bl_count);
End;
//-------------------------------------------------------------

Procedure scan_tree(Var s: deflate_state;
   Var tree: Array Of ct_data;          { the tree to be scanned }
   max_code: _int);                     { and its largest code of non zero frequency }
Var
   n: _int;                             { iterates over all tree elements }
   prevlen: _int;                       { last emitted length }
   curlen: _int;                        { length of current code }
   nextlen: _int;                       { length of next code }
   Count: _int;                         { repeat count of the current code }
   max_count: _int;                     { max repeat count }
   min_count: _int;                     { min repeat count }
Begin
   prevlen := -1;
   nextlen := tree[0].dl.Len;
   Count := 0;
   max_count := 7;
   min_count := 4;

   If (nextlen = 0) Then
   Begin
      max_count := 138;
      min_count := 3;
   End;
   tree[max_code + 1].dl.Len := ush($FFFF); { guard }

   For n := 0 To max_code Do
   Begin
      curlen := nextlen;
      nextlen := tree[n + 1].dl.Len;
      inc(Count);

      If (Count < max_count) And (curlen = nextlen) Then
         Continue
      Else
         If (Count < min_count) Then
            inc(s.bl_tree[curlen].fc.freq, Count)
         Else
            If (curlen <> 0) Then
            Begin
               If (curlen <> prevlen) Then
                  inc(s.bl_tree[curlen].fc.freq);
               inc(s.bl_tree[REP_3_6].fc.freq);
            End
            Else
               If (Count <= 10) Then
                  inc(s.bl_tree[REPZ_3_10].fc.freq)
               Else
                  inc(s.bl_tree[REPZ_11_138].fc.freq);

      Count := 0;
      prevlen := curlen;
      If (nextlen = 0) Then
      Begin
         max_count := 138;
         min_count := 3;
      End
      Else
         If (curlen = nextlen) Then
         Begin
            max_count := 6;
            min_count := 3;
         End
         Else
         Begin
            max_count := 7;
            min_count := 4;
         End;
   End;
End;
//-------------------------------------------------------------

Function build_bl_tree(Var s: deflate_state): _int;
Var
   max_blindex: _int;
Begin

   scan_tree(s, s.dyn_ltree, s.l_desc.max_code);
   scan_tree(s, s.dyn_dtree, s.d_desc.max_code);

   build_tree(s, s.bl_desc);

   For max_blindex := BL_CODES - 1 Downto 3 Do
   Begin
      If (s.bl_tree[bl_order[max_blindex]].dl.Len <> 0) Then
         break;
   End;

   inc(s.opt_len, 3 * (max_blindex + 1) + 5 + 5 + 4);
   build_bl_tree := max_blindex;
End;
//-------------------------------------------------------------

Procedure compress_block(Var s: deflate_state; Var ltree: Array Of ct_data;
   Var dtree: Array Of ct_data);
Var
   dist: _unsigned;
   lc: _int;
   lx: _unsigned;
   code: _unsigned;
   extra: _int;
Begin
   lx := 0;
   If (s.last_lit <> 0) Then
      Repeat

         dist := s.d_buf^[lx];
         lc := s.l_buf^[lx];
         inc(lx);

         If (dist = 0) Then
         Begin
            send_bits(s, ltree[lc].fc.code, ltree[lc].dl.Len);
         End
         Else
         Begin
            code := _length_code[lc];
            send_bits(s, ltree[code + LITERALS + 1].fc.code, ltree[code + LITERALS + 1].dl.Len);
            extra := extra_lbits[code];

            If (extra <> 0) Then
            Begin
               Dec(lc, base_length[code]);
               send_bits(s, lc, extra);
            End;

            Dec(dist);
            If (dist < 256) Then
               code := _dist_code[dist]
            Else
               code := _dist_code[256 + (dist Shr 7)];

            send_bits(s, dtree[code].fc.code, dtree[code].dl.Len);
            extra := extra_dbits[code];

            If (extra <> 0) Then
            Begin
               Dec(dist, base_dist[code]);
               send_bits(s, dist, extra);
            End;
         End;
      Until (lx >= s.last_lit);

   send_bits(s, ltree[END_BLOCK].fc.code, ltree[END_BLOCK].dl.Len);
   s.last_eob_len := ltree[END_BLOCK].dl.Len;
End;
//-------------------------------------------------------------

Procedure send_tree(Var s: deflate_state; Var tree: Array Of ct_data;
   max_code: _int);
Var
   n: _int;                             { iterates over all tree elements }
   prevlen: _int;                       { last emitted length }
   curlen: _int;                        { length of current code }
   nextlen: _int;                       { length of next code }
   Count: _int;                         { repeat count of the current code }
   max_count: _int;                     { max repeat count }
   min_count: _int;                     { min repeat count }
Begin
   prevlen := -1;
   nextlen := tree[0].dl.Len;
   Count := 0;
   max_count := 7;
   min_count := 4;

   If (nextlen = 0) Then
   Begin
      max_count := 138;
      min_count := 3;
   End;

   For n := 0 To max_code Do
   Begin
      curlen := nextlen;
      nextlen := tree[n + 1].dl.Len;
      inc(Count);
      If (Count < max_count) And (curlen = nextlen) Then
         Continue
      Else
         If (Count < min_count) Then
         Begin
            Repeat
               send_bits(s, s.bl_tree[curlen].fc.code, s.bl_tree[curlen].dl.Len);
               Dec(Count);
            Until (Count = 0);
         End
         Else
            If (curlen <> 0) Then
            Begin
               If (curlen <> prevlen) Then
               Begin
                  send_bits(s, s.bl_tree[curlen].fc.code, s.bl_tree[curlen].dl.Len);
                  Dec(Count);
               End;
               send_bits(s, s.bl_tree[REP_3_6].fc.code, s.bl_tree[REP_3_6].dl.Len);
               send_bits(s, Count - 3, 2);
            End
            Else
               If (Count <= 10) Then
               Begin
                  send_bits(s, s.bl_tree[REPZ_3_10].fc.code, s.bl_tree[REPZ_3_10].dl.Len);
                  send_bits(s, Count - 3, 3);
               End
               Else
               Begin
                  send_bits(s, s.bl_tree[REPZ_11_138].fc.code, s.bl_tree[REPZ_11_138].dl.Len);
                  send_bits(s, Count - 11, 7);
               End;

      Count := 0;
      prevlen := curlen;

      If (nextlen = 0) Then
      Begin
         max_count := 138;
         min_count := 3;
      End
      Else
         If (curlen = nextlen) Then
         Begin
            max_count := 6;
            min_count := 3;
         End
         Else
         Begin
            max_count := 7;
            min_count := 4;
         End;
   End;
End;
//-------------------------------------------------------------

Procedure send_all_trees(Var s: deflate_state; lcodes: _int; dcodes: _int;
   blcodes: _int);
Var
   rank: _int;
Begin
   send_bits(s, lcodes - 257, 5);
   send_bits(s, dcodes - 1, 5);
   send_bits(s, blcodes - 4, 4);
   For rank := 0 To blcodes - 1 Do
      send_bits(s, s.bl_tree[bl_order[rank]].dl.Len, 3);

   send_tree(s, s.dyn_ltree, lcodes - 1);
   send_tree(s, s.dyn_dtree, dcodes - 1);
End;
//-------------------------------------------------------------

Function _tr_flush_block(Var s: deflate_state; Buf: pcharf; stored_len: ulg;
   EOF: Boolean): ulg;
Var
   opt_lenb, static_lenb: ulg;
   max_blindex: _int;
Begin
   max_blindex := 0;

   If (s.level > 0) Then
   Begin
      If (s.data_type = Z_UNKNOWN) Then
         set_data_type(s);

      build_tree(s, s.l_desc);
      build_tree(s, s.d_desc);
      max_blindex := build_bl_tree(s);
      opt_lenb := (s.opt_len + 3 + 7) Shr 3;
      static_lenb := (s.static_len + 3 + 7) Shr 3;

      If (static_lenb <= opt_lenb) Then
         opt_lenb := static_lenb;

   End
   Else
   Begin
      static_lenb := stored_len + 5;
      opt_lenb := static_lenb;          { force a stored block }
   End;

{$IFDEF STORED_FILE_OK}
{$IFDEF FORCE_STORED_FILE}
   If EOF And (s.compressed_len = longint(0)) Then
   Begin                                { force stored file }
{$ELSE}
   If (stored_len <= opt_lenb) And EOF And (s.compressed_len = longint(0))
      And seekable()) Do
Begin
{$ENDIF}
   If (Buf = pcharf(0)) Then
      Error('block vanished');

   copy_block(Buf, _unsigned(stored_len), 0); { without header }
   s.compressed_len := stored_len Shl 3;
   s.method := STORED;
End
Else
{$ENDIF}                                { STORED_FILE_OK }

{$IFDEF FORCE_STORED}
   If (Buf <> PChar(0)) Then
   Begin                                { force stored block }
{$ELSE}
   If (stored_len + 4 <= opt_lenb) And (Buf <> pcharf(0)) Then
   Begin
{$ENDIF}
      _tr_stored_block(s, Buf, stored_len, EOF);

{$IFDEF FORCE_STATIC}
   End
   Else
      If (static_lenb >= 0) Then
      Begin                             { force static trees }
{$ELSE}
   End
   Else
      If (static_lenb = opt_lenb) Then
      Begin
{$ENDIF}
         send_bits(s, (STATIC_TREES Shl 1) + ord(EOF), 3);
         compress_block(s, static_ltree, static_dtree);
         inc(s.compressed_len, 3 + s.static_len);
      End
      Else
      Begin
         send_bits(s, (DYN_TREES Shl 1) + ord(EOF), 3);
         send_all_trees(s, s.l_desc.max_code + 1, s.d_desc.max_code + 1,
            max_blindex + 1);
         compress_block(s, s.dyn_ltree, s.dyn_dtree);
         inc(s.compressed_len, 3 + s.opt_len);
      End;

   init_block(s);

   If (EOF) Then
   Begin
      bi_windup(s);
      inc(s.compressed_len, 7);         { align on byte boundary }
   End;

   Result := s.compressed_len Shr 3;
End;
//-------------------------------------------------------------

Procedure FLUSH_BLOCK_ONLY(Var s: deflate_state; EOF: Boolean); {macro}
Begin
   If (s.block_start >= longint(0)) Then
      _tr_flush_block(s, pcharf(@s.window^[_unsigned(s.block_start)]),
         longint(longint(s.strstart) - s.block_start), EOF)
   Else
      _tr_flush_block(s, pcharf(Z_NULL),
         longint(longint(s.strstart) - s.block_start), EOF);

   s.block_start := s.strstart;
   flush_pending(s.strm^);
End;
//-------------------------------------------------------------

Function deflate_stored(Var s: deflate_state; Flush: _int): block_state;
Var
   max_block_size: longint;
   max_start: longint;
Begin
   max_block_size := $FFFF;
   If (max_block_size > s.pending_buf_size - 5) Then
      max_block_size := s.pending_buf_size - 5;

   While True Do
   Begin
      If (s.lookahead <= 1) Then
      Begin
         fill_window(s);
         If (s.lookahead = 0) And (Flush = Z_NO_FLUSH) Then
         Begin
            deflate_stored := need_more;
            Exit;
         End;

         If (s.lookahead = 0) Then
            break;                      { flush the current block }
      End;

      inc(s.strstart, s.lookahead);
      s.lookahead := 0;

      max_start := s.block_start + max_block_size;
      If (s.strstart = 0) Or (longint(s.strstart) >= max_start) Then
      Begin
         s.lookahead := word(longint(s.strstart) - max_start);
         s.strstart := word(max_start);
         FLUSH_BLOCK_ONLY(s, False);
         If (s.strm^.avail_out = 0) Then
         Begin
            deflate_stored := need_more;
            Exit;
         End;
      End;

      If (s.strstart - word(s.block_start) >= {MAX_DIST}
{$IFDEF DEFLATE64}
         s.w_size - MIN_LOOKAHEAD_DEF64) Then
{$ELSE}
         s.w_size - MIN_LOOKAHEAD) Then
{$ENDIF}
      Begin
         FLUSH_BLOCK_ONLY(s, False);
         If (s.strm^.avail_out = 0) Then
         Begin
            deflate_stored := need_more;
            Exit;
         End;
      End;
   End;

   FLUSH_BLOCK_ONLY(s, Flush = Z_FINISH);
   If (s.strm^.avail_out = 0) Then
   Begin
      If Flush = Z_FINISH Then
         deflate_stored := finish_started
      Else
         deflate_stored := need_more;
      Exit;
   End;

   If Flush = Z_FINISH Then
      deflate_stored := finish_done
   Else
      deflate_stored := block_done;
End;
//-------------------------------------------------------------

Function _tr_tally(Var s: deflate_state; dist: _unsigned; lc: _unsigned): Boolean;
Var
   code: ush;
{$IFDEF TRUNCATE_BLOCK}
Var
   out_length: ulg;
   in_length: ulg;
   dcode: int;
{$ENDIF}
Begin
   s.d_buf^[s.last_lit] := ush(dist);
   s.l_buf^[s.last_lit] := uch(lc);
   inc(s.last_lit);

   If (dist = 0) Then
      inc(s.dyn_ltree[lc].fc.freq)
   Else Begin
      inc(s.matches);
      Dec(dist);

      If (dist) < 256 Then
         code := _dist_code[dist]
      Else
         code := _dist_code[256 + (dist Shr 7)];

      inc(s.dyn_ltree[_length_code[lc] + LITERALS + 1].fc.freq);
      inc(s.dyn_dtree[code].fc.freq);
   End;

{$IFDEF TRUNCATE_BLOCK}
   If (s.last_lit And $1FFF = 0) And (s.level > 2) Then
   Begin
      out_length := ulg(s.last_lit) * Long(8);
      in_length := ulg(Long(s.strstart) - s.block_start);
{$IFDEF DEFLATE64}
      For dcode := 0 To D_CODES_DEF64 - 1 Do
{$ELSE}
      For dcode := 0 To D_CODES - 1 Do
{$ENDIF}
         inc(out_length, ulg(s.dyn_dtree[dcode].fc.freq *
            (Long(5) + extra_dbits[dcode])));

      out_length := out_length Shr 3;

      If (s.matches < s.last_lit Div 2) And (out_length < in_length Div 2) Then
      Begin
         _tr_tally := True;
         Exit;
      End;
   End;
{$ENDIF}
   _tr_tally := (s.last_lit = s.lit_bufsize - 1);
End;
//-------------------------------------------------------------

Function deflate_fast(Var s: deflate_state; Flush: _int): block_state;
Var
   hash_head: IPos;
   bflush: Boolean;
Begin
   hash_head := ZNIL;
   While True Do
   Begin

{$IFDEF DEFLATE64}
      If (s.lookahead < MIN_LOOKAHEAD_DEF64) Then
{$ELSE}
      If (s.lookahead < MIN_LOOKAHEAD) Then
{$ENDIF}
      Begin
         fill_window(s);
{$IFDEF DEFLATE64}
         If (s.lookahead < MIN_LOOKAHEAD_DEF64) And (Flush = Z_NO_FLUSH) Then
{$ELSE}
         If (s.lookahead < MIN_LOOKAHEAD) And (Flush = Z_NO_FLUSH) Then
{$ENDIF}
         Begin
            deflate_fast := need_more;
            Exit;
         End;

         If (s.lookahead = 0) Then
            break;                      { flush the current block }
      End;

      If (s.lookahead >= MIN_MATCH) Then
         INSERT_STRING(s, s.strstart, hash_head);

      If (hash_head <> ZNIL) And
{$IFDEF DEFLATE64}
         (s.strstart - hash_head <= (s.w_size - MIN_LOOKAHEAD_DEF64) {MAX_DIST}) Then
{$ELSE}
         (s.strstart - hash_head <= (s.w_size - MIN_LOOKAHEAD) {MAX_DIST}) Then
{$ENDIF}
      Begin
         If (s.strategy <> Z_HUFFMAN_ONLY) Then
            s.match_length := longest_match(s, hash_head);
      End;

      If (s.match_length >= MIN_MATCH) Then
      Begin
         bflush := _tr_tally(s, s.strstart - s.match_start,
            s.match_length - MIN_MATCH);

         Dec(s.lookahead, s.match_length);

{$IFNDEF FASTEST}
         If (s.match_length <= s.max_insert_length)
            And (s.lookahead >= MIN_MATCH) Then
         Begin
            Dec(s.match_length);
            Repeat
               inc(s.strstart);
               INSERT_STRING(s, s.strstart, hash_head);
               Dec(s.match_length);
            Until (s.match_length = 0);
            inc(s.strstart);
         End
         Else
{$ENDIF}
         Begin
            inc(s.strstart, s.match_length);
            s.match_length := 0;
            s.ins_h := s.window^[s.strstart];
            s.ins_h := ((s.ins_h Shl s.hash_shift) Xor
               s.window^[s.strstart + 1]) And s.hash_mask;

            If MIN_MATCH <> 3 Then      { the linker removes this }
            Begin
               {Call UPDATE_HASH() MIN_MATCH-3 more times}
            End;
         End;
      End
      Else
      Begin
         {_tr_tally_lit (s, 0, s.window^[s.strstart], bflush);}
         bflush := _tr_tally(s, 0, s.window^[s.strstart]);

         Dec(s.lookahead);
         inc(s.strstart);
      End;

      If bflush Then
      Begin
         FLUSH_BLOCK_ONLY(s, False);
         If (s.strm^.avail_out = 0) Then
         Begin
            deflate_fast := need_more;
            Exit;
         End;
      End;
   End;

   FLUSH_BLOCK_ONLY(s, Flush = Z_FINISH);
   If (s.strm^.avail_out = 0) Then
   Begin
      If Flush = Z_FINISH Then
         deflate_fast := finish_started
      Else
         deflate_fast := need_more;
      Exit;
   End;

   If Flush = Z_FINISH Then
      deflate_fast := finish_done
   Else
      deflate_fast := block_done;
End;
//-------------------------------------------------------------

Function deflate_slow(Var s: deflate_state; Flush: _int): block_state;
Var
   hash_head: IPos;
   bflush: Boolean;
Var
   max_insert: word;
Begin
   hash_head := ZNIL;

   While True Do
   Begin

{$IFDEF DEFLATE64}
      If (s.lookahead < MIN_LOOKAHEAD_DEF64) Then
{$ELSE}
      If (s.lookahead < MIN_LOOKAHEAD) Then
{$ENDIF}
      Begin
         fill_window(s);
{$IFDEF DEFLATE64}
         If (s.lookahead < MIN_LOOKAHEAD_DEF64) And (Flush = Z_NO_FLUSH) Then
{$ELSE}
         If (s.lookahead < MIN_LOOKAHEAD) And (Flush = Z_NO_FLUSH) Then
{$ENDIF}
         Begin
            Result := need_more;
            Exit;
         End;

         If (s.lookahead = 0) Then
            break;                      { flush the current block }
      End;

      If (s.lookahead >= MIN_MATCH) Then
         INSERT_STRING(s, s.strstart, hash_head);

      s.prev_length := s.match_length;
      s.prev_match := s.match_start;
      s.match_length := MIN_MATCH - 1;

      If (hash_head <> ZNIL) And (s.prev_length < s.max_lazy_match) And
{$IFDEF DEFLATE64}
         (s.strstart - hash_head <= {MAX_DIST}(s.w_size - MIN_LOOKAHEAD_DEF64)) Then
{$ELSE}
         (s.strstart - hash_head <= {MAX_DIST}(s.w_size - MIN_LOOKAHEAD)) Then
{$ENDIF}
      Begin

         If (s.strategy <> Z_HUFFMAN_ONLY) Then
            s.match_length := longest_match(s, hash_head);

         If (s.match_length <= 5) And ((s.strategy = Z_FILTERED) Or
            ((s.match_length = MIN_MATCH) And
            (s.strstart - s.match_start > TOO_FAR))) Then
            s.match_length := MIN_MATCH - 1;

      End;

      If (s.prev_length >= MIN_MATCH)
         And (s.match_length <= s.prev_length) Then
      Begin
         max_insert := s.strstart + s.lookahead - MIN_MATCH;
         bflush := _tr_tally(s, s.strstart - 1 - s.prev_match,
            s.prev_length - MIN_MATCH);

         Dec(s.lookahead, s.prev_length - 1);
         Dec(s.prev_length, 2);
         Repeat
            inc(s.strstart);
            If (s.strstart <= max_insert) Then
               INSERT_STRING(s, s.strstart, hash_head);

            Dec(s.prev_length);
         Until (s.prev_length = 0);
         s.match_available := False;
         s.match_length := MIN_MATCH - 1;
         inc(s.strstart);

         If (bflush) Then
         Begin
            FLUSH_BLOCK_ONLY(s, False);
            If (s.strm^.avail_out = 0) Then
            Begin
               Result := need_more;
               Exit;
            End;
         End;
      End
      Else
         If (s.match_available) Then
         Begin
            bflush := _tr_tally(s, 0, s.window^[s.strstart - 1]);

            If bflush Then
               FLUSH_BLOCK_ONLY(s, False);

            inc(s.strstart);
            Dec(s.lookahead);
            If (s.strm^.avail_out = 0) Then
            Begin
               Result := need_more;
               Exit;
            End;
         End
         Else
         Begin
            s.match_available := True;
            inc(s.strstart);
            Dec(s.lookahead);
         End;
   End;

   If (s.match_available) Then
   Begin
      _tr_tally(s, 0, s.window^[s.strstart - 1]);
      s.match_available := False;
   End;

   FLUSH_BLOCK_ONLY(s, Flush = Z_FINISH);

   If (s.strm^.avail_out = 0) Then
   Begin
      If Flush = Z_FINISH Then
         Result := finish_started
      Else
         Result := need_more;

      Exit;
   End;

   If Flush = Z_FINISH Then
      Result := finish_done
   Else
      Result := block_done;
End;
//-------------------------------------------------------------


(*************************************************************)
(*************************************************************)
(*                ZipTV Compression Procs                    *)
(*************************************************************)
(*************************************************************)


(* Returns CRC of compressed data *)

Function ztvCompress_StreamToClipboard(Stream: TStream32): _int;
Var
   Signature: _int;
   cStream: TMemoryStream32;
Begin
   Result := 0;
   cStream := TMemoryStream32.Create();
   Try
      Signature := LOCAL_CUST_HEADER_SIGNATURE;
      If cStream.Write(Signature, SizeOf(_int)) = 0 Then
         Exit;

      Result := ztvCompress_StreamToStream(Stream, TStream32(cStream));
      If (Result <> 0 {_int(CRC_MASK)}) Then
         If Not CopyStreamToClipboard(cStream, cStream.size) Then
            Result := 0;
   Finally
      cStream.Free();
   End;
End;
//-------------------------------------------------------------
(* Returns CRC of compressed data *)

Function ztvCompress_StreamToStream(InStream, OutStream: TStream32): _int;
Var
   inbuf, outbuf: Pointer;
   InBytes, outBytes: _int;
Begin
   GetMem(inbuf, InStream.size);
   Try
      InStream.Position := 0;
      InBytes := InStream.Read(inbuf^, InStream.size);

      outbuf := Nil;
      Try
         Result := ztvCompress_BufToBuf(inbuf, InBytes, outbuf, outBytes, dtDeflateN);
         If outBytes < InBytes Then
            OutStream.Write(outbuf^, outBytes)
         Else Begin
         	OutStream.CancelCallBackProc := Nil;
         	OutStream.ProgressCallBackProc := Nil;
            OutStream.CopyFrom(InStream, 0);
         End;

         OutStream.Position := 0;
      Finally
         If outbuf <> Nil Then
         	FreeMem(outbuf);
      End;
   Finally
      FreeMem(inbuf);
   End;
End;
//-------------------------------------------------------------
(* Returns CRC of compressed data *)

{Function ztvCompressStream( InStream, OutStream: TStream32 ): _int;
Var
   Cancel: Boolean;
   TempStream: TStream32;
   Stream: TCustomStream;
   ProgressPosition: _int;
Begin
   Result := CRC_MASK;

   ProgressPosition := InStream.Size;
   Cancel := False;

   TempStream := TMemoryStream32.Create();
   Try
      Try
         Stream := TCompressStream.Create( dtDeflateN, TempStream, _MAX_WBITS );
         With Stream Do
         Try
            With FZRec.cb Do
            Begin
               Protect := False; //Encrypted;
               Crc := CRC_MASK;
               pCancel := @Cancel;
               parchive_pos := @ProgressPosition;
            End;

            //TCompressStream( Stream ).OnProgress := ArcProgress;
            CopyFrom( InStream, 0 );
            TempStream.Position := 0;
            If ( Not FZRec.cb.Protect ) Then
               Result := FZRec.cb.Crc Xor CRC_MASK;

            If ( Result = CRC_MASK ) Then
             Result := 0;

         Finally
            Stream.Free();
         End;
      Except
         ON e: exception DO
         Begin
            ShowMessage( e.message );
            Result := 0;
         End;
      End;

      If InStream.Size > TempStream.Size Then
     OutStream.CopyFrom( TempStream, 0 )
      Else
     OutStream.CopyFrom( Instream, 0 )
   Finally
    TempStream.Free();
   End;
End;}
//-------------------------------------------------------------
(* Returns CRC of compressed data *)

Function ztvCompress_FileToStream(FileName: String; OutStream: TStream32): _int;
Var
   FileStream: TFileStream32;
Begin
   Result := 0;
   If (OutStream = Nil) Or (Not FileExists(FileName)) Then Exit;

   FileStream := TFileStream32.Create(FileName, fmOpenRead);
   If (FileStream.Handle < 0) Then Exit;

   Try
      Result := ztvCompress_StreamToStream(FileStream, OutStream);
   Finally
      FileStream.Free();
   End;
End;
//-------------------------------------------------------------

Function ztvCompressStreamProc(InStream, OutStream: TStream32; zsp: ztv_stream_plus;
   Var CRC: u_long; DefType: TDeflateType; ProgressProc: TNotifyEvent;
   maxbits: ShortInt): Boolean;
Var
   Stream: TCustomStream;
Begin
   Try

      Stream := TCompressStream.Create(OutStream, DefType, maxbits);
      Try
         Stream.FZRec.cb := zsp;
         Stream.OnProgress := ProgressProc;
         Stream.ProgressCallBackProc := ProgressProc;

         InStream.CancelCallBackProc := @zsp.pCancel^;
         Stream.CopyFrom(InStream, 0);         // activate the compression

         OutStream.Position := 0;
         If (Not Stream.FZRec.cb.Protect) Then
            CRC := u_long(Stream.FZRec.cb.CRC) Xor CRC_MASK;
            //Crc := FZRec.cb.Crc;  // v4.1.5 revised

         If (InStream.size = 0) And (Crc32Val = CRC_MASK) Then
            Crc32Val := 0;

         Result := (Not Stream.FZRec.cb.pCancel^);
      Finally
         Stream.Free();
      End;
   Except
      //On e: exception Do
      //Begin
      	Raise;
      //   ShowMessage(e.message);
      //   Result := False;
      //End;
   End;
End;
//-------------------------------------------------------------

Function ztvCompress_BufToBuf(Const inbuf: Pointer; InBytes: _int;
   Var outbuf: Pointer; Var outBytes: _int; level: TDeflateType): _int;
Var
   strm: ztv_stream;
   p: Pointer;
   Cancel: Boolean;
Begin
   FillChar(strm, SizeOf(strm), 0);
   strm.ZALLOC := @ztvAllocMem;
   strm.ZFREE := @ztvFreeMem;
   strm.cb.CRC := CRC_MASK;
   Try
      outBytes := ((InBytes + (InBytes Div 10) + 12) + 255) And Not 255;
      GetMem(outbuf, outBytes);
      Try
         Cancel := False;
         strm.next_in := inbuf;
         strm.avail_in := InBytes;
         strm.next_out := outbuf;
         strm.avail_out := outBytes;
         strm.cb.pCancel := @Cancel;

         deflateInit_(@strm, ZLevels[level], _MAX_WBITS, SizeOf(strm));	// v4.1.7
         Try
            While _deflate(strm, Z_FINISH) <> Z_STREAM_END Do	// v4.1.7
            Begin
               p := outbuf;
               inc(outBytes, 256);
               ReallocMem(outbuf, outBytes);
               strm.next_out := ztvStreams._pBytef(_int(outbuf) + (_int(strm.next_out) - _int(p)));
               strm.avail_out := 256;
            End;
         Finally
            _deflateEnd(strm);	// v4.1.7
         End;
         ReallocMem(outbuf, strm.total_out);
         outBytes := strm.total_out;
      Except
         FreeMem(outbuf);
         Raise
      End;
   Finally
      Result := strm.cb.CRC;
   End;
End;
//-------------------------------------------------------------

Function ztvCompress_String(s: String; level: TDeflateType): String;
Var
   size: _int;
   Buffer: Pointer;
Begin
   ztvCompress_BufToBuf(PChar(s), Length(s), Buffer, size, level);
   Try
      SetLength(Result, size);
   	Move( Buffer^, Result[1], Size );
   Finally
      FreeMem(Buffer);
   End;
End;
//-------------------------------------------------------------

End.
