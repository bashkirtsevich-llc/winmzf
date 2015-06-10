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
Unit ztvGbls;

Interface

Uses
   Windows,
   Dialogs,
   SysUtils,
   Classes,
   Forms,
   Buttons,
   StdCtrls,
   Controls,
   ComCtrls,
   ztvStreams,
   ztvCrypt,
   Err_Msgs;



   

{$I ZipTV.inc}

Type
   TztvDriveType =
      (dtUnknown, dtNoDrive, dtFloppy, dtFixed, dtNetwork, dtCDROM,
      dtRam, dtFloppy3, dtFloppy5);

   TOnDiskError = Procedure(Sender: TObject; Var Abort: Boolean)
      Of Object;

   //TExtSet = (ace, arc, arj, bh, cab, enc, gz, ha, jar, lha, lzh, pak, pk3,
   //   pk_, rar, tar, tgz, war, uue, uu, xxe, z, zip, zoo);

   TSearchRec32 = Record
      Time: Integer;
      Size: Cardinal;
      Attr: Integer;
      Name: TFileName;
      ExcludeAttr: Integer;
      FindHandle: THandle;
      FindData: TWin32FindData;
   End;

   E_RAISE = Class(exception);          //custom error handler

   TDateMethod = (dtCreate, dtLastWrite, dtLastAccess);
   {$IFDEF use_zlib}
   TFileStatus = (tfsCompressed, tfsStored); // zlib
   {$ENDIF}

   THeaderType = (htLocal, htCentral, htEnding, htLocator, htEnding64, htNone);
   THeaderTypeState = Set Of THeaderType;
   pHeaderTypeState = ^THeaderTypeState;

   (* ArcType set members *)
   TArcType = (atNA,
      atUnsupported,
      atFileOpenErr,
      atUnknown,
      atAce, atAceExe,
      atArc, atArcExe,
      atArj, atArjExe,
      atBh, atBhExe,
      atCab, atCabExe,
      atGZip,
      atHA,
      atJar, atJarExe,
      atLha, atLhaExe, atLzh, atLzhExe,
      atMsGZ,
      atPak, atPakExe,
      atRar, atRarExe,
      atTar,
      atTarGZip,
      atUUE,
      atZip, atZipExe, atZipDS, atZipMV,
      {$IFDEF use_zlib}
      atZlb, atZLib,
      {$ENDIF}
      atZoo);

Const
   Wrapper_ArcType: Set Of TArcType = [atAce, atAceExe, atRar, atRarExe, atCab,
      atCabExe];
   Invalid_ArcType: Set Of TArcType = [atNA..atUnknown]; // Invalid archive range
   Zipped_ArcType: Set Of TArcType = [atZip, atZipExe, atJar, atJarExe];
   Split_ArcType: Set Of TArcType = [atZip, atZipExe, atZipMV{, atJar, atJarExe}];

   (* Archive types supported with decompression *)
   Decompress_ArcType: Set Of TArcType = [
   atAce..atAceExe
      , atArc..atArcExe
      , atArj..atArjExe
      , atBh..atBhExe
      , atCab..atCabExe
      , atGZip
      , atJar..atJar
      , atLha..atLzhExe
      {$IFDEF DEVELOPMENT}
   , atMsGZ
      {$ENDIF}
   , atRar..atRarExe
      , atTar
      , atUUE
      , atZip..atZipMV
      , atZoo
      ];

   (* File types supported with Encoding *)
   Encode_ArcType: Set Of TArcType = [atUUE];

   (* Archive types supported with compression *)
   Compress_ArcType: Set Of TArcType = [
   	atBh..atBh
      , atCab
      , atGZip
      //,atZoo
   	, atJar
      , atLha
      , atLzh
      , atTar
      , atZip..atZip{, atZipExe}, atZipMV
      {$IFDEF use_zlib}
   	, atZlb..atZLib
      {$ENDIF}
   ];

   (* Archives TZipSearch supports *)
   Search_ArcType: Set Of TArcType = [
   atAce..atAceExe
      , atArc..atArcExe
      , atArj..atArjExe
      , atBh..atBhExe
      , atCab..atCabExe
      , atGZip
      , atJar..atJarExe
      , atLha..atLzhExe
      {$IFDEF DEVELOPMENT}
   , atMsGZ
      {$ENDIF}
   , atRar..atRarExe
      , atTar
      , atUUE
      , atZip..atZipExe
      , atZipDS
      , atZipMV
      , atZoo
      ];

   (* Archives TZipCheck supports *)
   Verify_ArcType: Set Of TArcType = [
   atAce..atAceExe
      , atArc..atArcExe
      , atArj..atArjExe
      , atBh..atBhExe
      //, atCab
   , atGZip
      , atJar..atJarExe
      , atLha..atLzhExe
      , atRar..atRarExe
      , atTar
      , atZip..atZipExe
      , atZipDS
      , atZipMV
      , atZoo
      , atUUE
      ];

   (* Compression components that support
                       password encryption *)
   Compress_PasswordSupport:
   Set Of TArcType = [
      atBh..atBh
      , atJar..atJar
      , atZip..atZip, atZipMV
      ];

   Comment_Support_ArcType: Set Of TArcType = [
      //atJar..atJarExe
   atZip..atZipExe
      ];

   UnSFX_ArcType: Set Of TArcType = [
   atArcExe
      , atArjExe
      , atAceExe
      , atBhExe
      , atLhaExe
      , atLzhExe
      , atPakExe
      , atRarExe
      , atZipExe
      ];

Const
   // ArcTypeNames must EXACTLY!! match
   // the order of the members of
   // TArcType (above)
   ArcTypeNames: Array[TArcType] Of String =
   ('Not avail. '
      , 'Unsupport.'
      , 'File Error'
      , 'Unknown   '
      , 'ACE       ', 'ACE SFX   '
      , 'ARC       ', 'ARC SFX   '
      , 'ARJ       ', 'ARJ SFX   '
      , 'BH        ', 'BH  SFX   '
      , 'CAB       ', 'CAB SFX   '
      , 'GZip      '
      , 'HA        '
      , 'Java JAR  ', 'JJAR SFX  '
      , 'LHA       ', 'LHA SFX   ', 'LZH       ', 'LZH SFX   '
      , 'MsGZ     	'
      , 'PAK       ', 'PAK SFX   '
      , 'RAR       ', 'RAR SFX   '
      , 'TAR       '
      , 'TarGzip   '
      , 'UUE       '
      , 'ZIP       ', 'ZIP SFX   '
      , 'Zip Span  '
      , 'Zip MVol  '
      {$IFDEF use_zlib}
      , 'ZLib      '
      , 'ZLIB (ztv)'
      {$ENDIF}
      , 'ZOO       '
      );

Const
   Arj_ArcType: Set Of TArcType = [atArj..atArjExe];
   Arc_ArcType: Set Of TArcType = [atArc..atArcExe];
   Bh_ArcType: Set Of TArcType = [atBh..atBhExe];
   Cab_ArcType: Set Of TArcType = [atCab..atCabExe];
   GZip_ArcType: Set Of TArcType = [atGZip..atGZip];
   Ha_ArcType: Set Of TArcType = [atHA..atHA];
   Lha_ArcType: Set Of TArcType = [atLha..atLzhExe];
   Pak_ArcType: Set Of TArcType = [atPak..atPakExe];
   Rar_ArcType: Set Of TArcType = [atRar..atRarExe];
   Tar_ArcType: Set Of TArcType = [atTar..atTar];
   Zip_ArcType: Set Of TArcType = [atJar..atJarExe, atZip..atZipExe,
      atZipDS..atZipMV];
   {$IFDEF use_zlib}
   ZLib_ArcType: Set Of TArcType = [atZlb..atZLib];
   {$ENDIF}
   Zoo_ArcType: Set Of TArcType = [atZoo..atZoo];

Const
   // GZip CompressType flags
   GZIP_MAGIC = $8B1F;                  // 35615
   OLD_GZIP_MAGIC = $9E1F;              // 40479
   LZW_MAGIC = $9D1F;                   // 40223
   LZH_MAGIC = $A01F;                   // 40991
   PACK_MAGIC = $1E1F;                  // 7711
   RootDir = $5C;                       // 92

   // Compression Methods
Const
   ZTV_STORED = 0;
   ztv_FUSE = 1;
   ZTV_FUSE6 = 3;
   ztv_GREATEST = 2;
   ztv_DEFLATE = 8;
   ztv_DEFLATE64 = 9;
   ztv_FROZEN0 = 48;
   ztv_FROZEN1 = 49;
   ztv_FROZEN5 = 53;
   ztv_FROZEN6 = 54;
   ztv_FROZEN7 = 55;

   // Deflate pack levels
Const
   ztvDeflateS = 1;
   ztvDeflateF = 2;
   ztvDeflateN = 6;                     //7; v4.6.8 revised
   ztvDeflateX = 9;

   // Header Sizes
Const
   GZipHdr_Size = 10;
   AceMainHeader_Size = 31;
   HAHdr_Size = 18;
   ZooHdr_Size = 43;
   ZooDirHdr_Size = 58;
   RAR1Hdr_Size = 13;
   //RARHdr_Size = 32;
   LZHHdr_Size = 22;
   ArcHdr_Size = 29;
   PakHdr_Size = 29;
   ArjHdr_Size = 30;

Const
   WSIZE = 32768; (* window size--must be a power of two, and at least 32k *)
   PW_PROTECTED = 1;
   PKBACK = 'PKBACK# ';
   LHA_UNIXOS = $4D;

Const
   // block types
   ACE_MAIN_BLK = 0;
   ACE_FILE_BLK = 1;
   ACE_REC_BLK = 2;

   // info in the Header Flag
   ACE_HDF_COMMENT = 1;                 // bit  1
   ACE_HDF_ANSI = 1024;                 // bit 11
   ACE_HDF_SPLIT_BEFORE = 2048;         // bit 12
   ACE_HDF_SPLIT_AFTER = 4096;          // bit 13
   ACE_HDF_PASSWORD = 8192;             // bit 14
   ACE_HDF_SOLID = 16384;               // bit 15

   ACECMT_OK = 0;
   ACECMT_SMALLBUF = 1;
   ACECMT_NONE = 255;

   // archive-header-flags
   ACE_LIM256 = 1024;
   ACE_MULT_VOL = 2048;
   ACE_AV = 4096;
   ACE_RECOV = 8192;
   ACE_LOCK = 16384;
   ACE_SOLID = 32768;

   // file-header-flags
   ACE_ADDSIZE = 1;
   ACE_COMMENT = 2;
   ACE_SP_BEFORE = 4096;
   ACE_SP_AFTER = 8192;
   ACE_PASSWORD = 16384;

   // known compression types
   ACE_TYPE_STORE = 0;
   ACE_TYPE_LZW1 = 1;

// RAR constants
Const
   (* RAR Archive header flags *)
   MHD_MULT_VOL = 1;
   MHD_COMMENT = 2;
   MHD_LOCK = 4;
   MHD_SOLID = 8;
   MHD_PACK_COMMENT = 16;
   MHD_NEWNUMBERING = 16;
   MHD_AV = 32;
   MHD_PROTECT = 64;

   LHD_LARGE = $0100;
   LHD_UNICODE = $0200;
   LHD_SALT = $0400;
   LHD_VERSION = $0800;
   LHD_EXTTIME = $1000;
   LHD_EXTFLAGS = $2000;

   (* RAR File header flags *)
   LHD_SPLIT_BEFORE = 1;
   LHD_SPLIT_AFTER = 2;
   LHD_PASSWORD = 4;
   LHD_COMMENT = 8;
   LHD_SOLID = 16;

   SKIP_IF_UNKNOWN = $4000;
   LONG_BLOCK = $8000;

   // rar header sizes
   SIZEOF_MARKHEAD = 7;
   SIZEOF_OLDMHD = 7;
   SIZEOF_NEWMHD = 13;
   SIZEOF_OLDLHD = 21;
   SIZEOF_NEWLHD = 32;
   SIZEOF_SHORTBLOCKHEAD = 7;
   SIZEOF_LONGBLOCKHEAD = 11;
   SIZEOF_COMMHEAD = 13;
   SIZEOF_PROTECTHEAD = 26;

   (* RAR internal block types *)
   ALL_HEAD = 0;
   MARK_HEAD = $72;                     // 114
   MAIN_HEAD = $73;                     // 115
   FILE_HEAD = $74;                     // 116
   COMM_HEAD = $75;                     // 117
   AV_HEAD = $76;                       // 118
   SUB_HEAD = $77;                      // 119
   PROTECT_HEAD = $78;                  // 120

Const
   // Tar linkflags - header types
   LF_OLDNORMAL = #0;                   // Normal disk file, Unix compat
   LF_NORMAL = '0';                     // Normal disk file
   LF_LINK = '1';                       // Link to previously dumped file
   LF_SYMLINK = '2';                    // Symbolic link
   LF_CHR = '3';                        // Character special file
   LF_BLK = '4';                        // Block special file
   LF_DIR = '5';                        // Directory
   LF_FIFO = '6';                       // FIFO special file
   LF_CONTIG = '7';                     // Contiguous file

Const
   ZTV_FILE_ATTRIBUTE_READONLY = $00000001;
   ZTV_FILE_ATTRIBUTE_HIDDEN = $00000002;
   ZTV_FILE_ATTRIBUTE_SYSTEM = $00000004;
   ZTV_FILE_ATTRIBUTE_DIRECTORY = $00000010;
   ZTV_FILE_ATTRIBUTE_ARCHIVE = $00000020;
   ZTV_FILE_ATTRIBUTE_DEVICE = $00000040;
   ZTV_FILE_ATTRIBUTE_NORMAL = $00000080;
   ZTV_FILE_ATTRIBUTE_TEMPORARY = $00000100;
   ZTV_FILE_ATTRIBUTE_SPARSE_FILE = $00000200;
   ZTV_FILE_ATTRIBUTE_REPARSE_POINT = $00000400;
   ZTV_FILE_ATTRIBUTE_COMPRESSED = $00000800;
   ZTV_FILE_ATTRIBUTE_OFFLINE = $00001000;
   ZTV_FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
   ZTV_FILE_ATTRIBUTE_ENCRYPTED = $00004000;

Const
   EndingHeader = $01;
   CentralHeader = $08;
   LocalHeader = $10;

Const
   MaxExtArray = 23;
   ExtArray: Array[0..MaxExtArray] Of WideString =
   ('.ACE', '.ARC', '.ARJ', '.BH', '.CAB', '.ENC', '.GZ', '.HA',
      '.JAR', '.LHA', '.LZH', '.PAK', '.PK3', '.PK_', '.RAR', '.TAR',
      '.TGZ', '.UUE', '.UU', '.WAR', '.XXE', '.Z', '.ZIP', '.ZOO');

Const
   Crc32Table: Array[0..255] Of u_long = (
      $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F,
      $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
      $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2,
      $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
      $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9,
      $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
      $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
      $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
      $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423,
      $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
      $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106,
      $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
      $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D,
      $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
      $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950,
      $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
      $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7,
      $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
      $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA,
      $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
      $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
      $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
      $EAD54739, $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84,
      $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
      $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB,
      $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
      $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8, $A1D1937E,
      $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
      $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55,
      $316E8EEF, $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
      $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28,
      $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
      $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F,
      $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
      $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
      $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
      $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69,
      $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
      $A7672661, $D06016F7, $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC,
      $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
      $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693,
      $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
      $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
      );

Const
   CRC16Table: Array[0..255] Of word =
   (
      $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241,
      $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
      $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40,
      $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
      $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
      $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
      $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641,
      $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
      $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240,
      $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
      $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
      $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
      $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41,
      $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
      $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
      $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
      $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240,
      $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
      $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41,
      $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
      $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41,
      $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
      $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640,
      $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
      $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
      $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
      $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40,
      $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
      $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40,
      $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
      $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641,
      $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040
      );

Function _ChangeFileExt(FileName, Ext: String): String;
Function _DirectoryExists(Const Name: String): Boolean;
Function AppendDirTail(Path: String): String;
Function AppendDrive(FileName: String): String;
Function CalcProgress64(dx, dy: int64): Byte;
Function CalcProgress32(dx, dy: Cardinal): Byte; Overload;
Function CalcProgressDbl(dx, dy: Double): Byte;
Function CalcRatio(dx, dy: int64): Integer; Overload;
Function CalcRatio(dx, dy: Double): Integer; Overload;
Function CalcRatio(dx, dy: Integer): Integer; Overload;
Function ChangeFileExtTmp(FileName: String): String;
Function CharToOemFilter(str: String; b: Boolean): String;
Function CharToUnixOemFilter(str: String; b: Boolean): String;
Function CompareMem(p1, p2: Pointer; Len: Integer): Boolean;
Function CreateDirEx(DirName: String): Boolean;
Function DecodeDir(Dir: String): String;
Function DirExists(Name: String): Boolean;
Function DOSToUnixFilename(Const str: String): String;
Function GetDeflateMethodStr(BitFlag: word): String;
Function GetDeflateMethodStr64(BitFlag: word): String;
Function ExtractFilenameOnly(str: pChar): Byte;
Function FileTimeToInt(FileTime: TFileTime): Integer;
Function GetImageIndex(FileName: String): Integer;
Function GetTempPathStr: String;
Function GetToken(Var str: String; Seperator: String): String;
Function isDirEmpty(Dir: String): Boolean;
Function IsGZipSignAttr(Attr: Integer): Boolean;
Function IsWriteable(FileName: String): Boolean;
Function IsUncPath(Path: String): Boolean;
Function OemToCharFilter(str: String; b: Boolean): String;
Function PCharToStr(pstr: pChar): String;
Function ReadFilename_NullTerminate(strm: TStream32; Var fPos: int64): String;
Function ReadFilename_DefinedLen(strm: TStream32; Len: Byte): AnsiString; Overload;
Function ReadFilename_DefinedLen(strm: TStream32; Var fPos: int64; Len: Byte):
   AnsiString; Overload;
Function RemoveDirTail(Path: String): String;
Function ShortName(HWind: Hwnd; Const FileName: String): String;
Function SlashSep(Const Path, s: String): String;
Function StringAsPChar(s: String): pChar;
Function SwapWords(SW: Integer): Integer;
Function TStringsToStr(ts: TStrings; SepChar: Char): String;
Function UnformatNumber(s: String): String;
Function UnixToDosFilename(str: String): String;
Function VerifyAttr(Attr: Integer): Integer;
Function ztvConvertDate(FileDate: Integer): TDateTime;
Function ztvFileExists(Const FileName: String): Boolean;

Procedure CopyMem(source, dest: Pointer; Count: Integer);
Procedure DestroyImageList(DIL: TListView);
Procedure InitializeImageList(Sender: TComponent; IIL: TListView);
Procedure StrToTStrings(p: pChar; ts: TStrings);

Implementation

Uses
   ShellApi,
   ztvbase;

//-------------------------------------------------------------

Function StringAsPChar(s: String): pChar;
Begin
   Result := pChar(s);
End;
//-------------------------------------------------------------

Function SlashSep(Const Path, s: String): String;
Begin
   Result := '';
   If Path = '' Then exit;
   If AnsiLastChar(Path)^ <> '\' Then
      Result := Path + '\' + s
   Else
      Result := Path + s;
End;
//-------------------------------------------------------------

Function AppendDirTail(Path: String): String;
Begin
   If Path <> '' Then
   	If AnsiLastChar(Path)^ <> '\' Then
         Path := Path + '\';

   Result := Path;
End;
//-------------------------------------------------------------

Function AppendDrive(FileName: String): String;
Begin
   If Pos(':\', FileName) = 0 Then
      Result := GetCurrentDir[1] + ':\' + FileName
   Else
      Result := FileName;
End;
//-------------------------------------------------------------

Function RemoveDirTail(Path: String): String;
Begin
   If Path = '' Then
   	Result := ''
   Else If Path[Length(Path)] = '\' Then
      //Delete(Path, Length(Path), 1); Result := Path;
        //or
      Result := copy(Path, 1, Length(Path) - 1)
   Else
      Result := Path;
End;
//-------------------------------------------------------------

Function CalcProgress64(dx, dy: int64): Byte;
Var
   R, s: EXTENDED;
Begin
   If dy > 0 Then
   Begin
      s := dx;
      s := s * 100;
      R := Round(s / dy);
      If (R < 0) Then R := 0;
   End
   Else
      R := 0;

   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //Result := Min(Round(R), 100);
   If Round(R) > 100 Then
      Result := 100
   Else
      Result := Round(R);

End;

{Function CalcProgress64(dx, dy: int64): Byte; Overload;
Var
   R: EXTENDED;
Begin
   If dy > 0 Then
   Begin
      dx := dx * 100;
      R := dx Div dy;
      If (R < 0) Then R := 0;
   End
   Else
      R := 0;

   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //Result := Min(Round(R), 100);
   If Round(R) > 100 Then
   //If R > 100 Then
      Result := 100
   Else
      Result := Round(R);
      //Result := R;

End;}
//-------------------------------------------------------------

Function CalcProgressDbl(dx, dy: Double): Byte;
Var
   R, s: EXTENDED;
Begin
   If dy > 0 Then
   Begin
      s := dx;
      s := s * 100;
      R := Round(s / dy);
      If (R < 0) Then R := 0;
   End
   Else
      R := 0;

   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //Result := Min(Round(R), 100);
   If Round(R) > 100 Then
      Result := 100
   Else
      Result := Round(R);

End;
//-------------------------------------------------------------

Function CalcProgress32(dx, dy: Cardinal): Byte;
Var
   R, s: EXTENDED;
Begin
   If dy > 0 Then
   Begin
      s := dx;
      s := s * 100;
      R := Round(s / dy);
      If (R < 0) Then R := 0;
   End
   Else
      R := 0;

   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //Result := Min(Round(R), 100);
   If Round(R) > 100 Then
      Result := 100
   Else
      Result := Round(R);

End;
//-------------------------------------------------------------

Function DecodeDir(Dir: String): String;
Const
   DELIM = $2F;
   DELIM2 = $FF;
   WIN_DELIM = $5C;
Var
   c: Byte;
   i: Word;
   flg: Boolean;
Begin
   flg := false;
   For i := 1 To Length(Dir) Do
   Begin
      c := Byte(Dir[i]);
      If flg Then
         flg := false
      Else If (c >= $80) And (c <= $9F) Or (c >= $E0) And (c <= $FD) Then
         flg := True
      Else If (Dir[i] = '\\') Or (c = DELIM) Or (c = DELIM2) Then
         Dir[i] := Char(WIN_DELIM);
   End;
   Result := Trim(Dir);                 (* Trim undesired ctrl codes *)
End;
//-------------------------------------------------------------

Function DirExists(Name: String): Boolean;
Var
   Code: Integer;
Begin
   Code := GetFileAttributes(pChar(Name));
   Result := (Code <> -1) And (FILE_ATTRIBUTE_DIRECTORY And Code <> 0);
End;
//-------------------------------------------------------------

Function OemToCharFilter(str: String; b: Boolean): String;
Begin
   If str <> '' Then
   Begin
      If b Then
         OemToChar(@str[1], @str[1]);
         //OemToCharBuff(@str[1], @str[1], Length(str));

      // v4.6.1 rem'd the following block
      //For i := 1 To Length(str) Do
      //	If str[i] = '0' Then
      //   	str[i] = #
      str := UnixToDosFilename(str);
   End;
   Result := str;
End;
//-------------------------------------------------------------

Function CharToUnixOemFilter(str: String; b: Boolean): String;
Begin
   If str <> '' Then
   Begin
      str := DOSToUnixFilename(str);
      If b Then
         CharToOem(@str[1], @str[1]);
         //CharToOemBuff(@str[1], @str[1], Length(str));
   End;
   Result := str;
End;
//-------------------------------------------------------------

Function CharToOemFilter(str: String; b: Boolean): String;
Begin
   If str <> '' Then
   Begin
      If b Then
         CharToOem(@str[1], @str[1]);
         //CharToOemBuff(@str[1], @str[1], Length(str));
   End;
   Result := str;
End;
//-------------------------------------------------------------

Function DOSToUnixFilename(Const str: String): String;
Var
   i: Integer;
Begin
   Result := str;
   For i := 1 To Length(str) Do
      If Result[i] = '\' Then Result[i] := '/';
End;
//-------------------------------------------------------------

Function ChangeFileExtTmp(FileName: String): String;
Var
   Ext: String;
Begin
   Ext := ExtractFileExt(FileName);
   If Length(Ext) > 3 Then
      Result := _ChangeFileExt(FileName, '.~' + copy(Ext, 2, Length(Ext) - 2))
   Else If Length(Ext) > 1 Then
      Result := _ChangeFileExt(FileName, '.~' + copy(Ext, 2, Length(Ext) - 1))
   Else
      Result := FileName + '.~' + Ext;
End;
//-------------------------------------------------------------
// call with dot in Ext

Function _ChangeFileExt(FileName, Ext: String): String;
Begin
   Result := copy(FileName, 1, ExtractFilenameOnly(@FileName[1])) + Ext;
End;
//-------------------------------------------------------------

Function ExtractFilenameOnly(str: pChar): Byte;
Var
   p: pChar;
Begin
   p := StrRScan(str, '.');
   If p = Nil Then
      Result := Strlen(str)
   Else
      Result := Integer(p) - Integer(str);
End;
//-------------------------------------------------------------

Function ztvFileExists(Const FileName: String): Boolean;
Var
   Handle: THandle;
   FindData: TWin32FindData;
Begin
   Handle := FindFirstFile(pChar(FileName), FindData);
   Result := Handle <> INVALID_HANDLE_VALUE;
   If Result Then Windows.FindClose(Handle);
End;
//-------------------------------------------------------------

Function GetTempPathStr: String;
Var
   Buffer: Array[0..MAX_PATH - 1] Of Char;
Begin
   If Windows.GetTempPath(SizeOf(Buffer) - 1, @Buffer[0]) <> 0 Then
      Result := String(Buffer)
   Else
      SetLength(Result, 0);
End;
//-------------------------------------------------------------

Function GetToken(Var str: String; Seperator: String): String;
Var
   i: word;
Begin
   i := Pos(Seperator, str);
   If i <> 0 Then
   Begin
      Result := System.copy(str, 1, i - 1);
      System.Delete(str, 1, i + (Length(Seperator) - 1));
   End
   Else
   Begin
      Result := str;
      SetLength(str, 0);
   End;
End;
//-------------------------------------------------------------

Function isDirEmpty(Dir: String): Boolean;
Var
   SearchRec: TSearchRec;
Begin
   Result := FindFirst(SlashSep(Dir, '*'), ZTV_FILE_ATTRIBUTE_NORMAL{faAnyFile} And (Not
      FILE_ATTRIBUTE_DIRECTORY),
      SearchRec) <> 0;

   If Not Result Then
      FindClose(SearchRec);
End;
//-------------------------------------------------------------

Function IsGZipSignAttr(Attr: Integer): Boolean;
Begin
   Result :=
      (Attr = GZIP_MAGIC) Or (Attr = OLD_GZIP_MAGIC) Or
      (Attr = LZW_MAGIC) Or (Attr = LZH_MAGIC) Or
      (Attr = PACK_MAGIC);
End;
//-------------------------------------------------------------

// some stored file's attributes (created from PkWare's zip) are stored
// as High(integer) + Attribute.  VerifyAttr is used to convert these
// attributes to compatible file-attribute variables, so crc comparisons
// are successful.

Function VerifyAttr(Attr: Integer): Integer;

	Procedure AddBit(Bit: Word);
   Begin
      If Attr And Bit > 0 Then Result := Result Or Bit;
   End;
Begin
   Result := 0;
   AddBit(ZTV_FILE_ATTRIBUTE_NORMAL);
   AddBit(ZTV_FILE_ATTRIBUTE_ARCHIVE);
   AddBit(ZTV_FILE_ATTRIBUTE_READONLY);
   AddBit(ZTV_FILE_ATTRIBUTE_HIDDEN);
   AddBit(SysUtils.faVolumeID);
   AddBit(ZTV_FILE_ATTRIBUTE_SYSTEM);
   AddBit(ZTV_FILE_ATTRIBUTE_DIRECTORY);
   AddBit(ZTV_FILE_ATTRIBUTE_ENCRYPTED);
End;
//-------------------------------------------------------------

Function IsWriteable(FileName: String): Boolean;
Var
   Attr: Integer;
Begin
   Attr := GetFileAttributes(pChar(FileName));
   Result :=
      ((Attr And FILE_ATTRIBUTE_DIRECTORY) = 0) And
      ((Attr And FILE_ATTRIBUTE_READONLY) = 0) And
      ((Attr And FILE_ATTRIBUTE_SYSTEM) = 0) And
      ((Attr And FILE_ATTRIBUTE_HIDDEN) = 0);
End;
//-------------------------------------------------------------

Function PCharToStr(pstr: pChar): String;
Begin
   SetLength(Result, Strlen(pstr));
   CopyMem(pstr, @Result[1], Length(Result));
End;
//-------------------------------------------------------------

Function ReadFilename_NullTerminate(strm: TStream32; Var fPos: int64): String;
Var
   Buf: Array[0..255] Of Char;
Begin
   strm.Position := fPos;
   strm.Read(Buf[0], 256);
   Result := String(Buf);
   Inc(fPos, Length(Result) + 1);
End;
//-------------------------------------------------------------

Function ReadFilename_DefinedLen(strm: TStream32; Len: Byte): AnsiString;
Var
   s: AnsiString;
	BytesRead: Integer;
   i: Integer;
Begin
	i := strm.Position;
   SetLength(s, Len);
   BytesRead := strm.Read(s[1], Len);
   If BytesRead <> 0 Then ;
   If i <> 0 Then ;
   Result := s;
End;
//-------------------------------------------------------------

Function ReadFilename_DefinedLen(strm: TStream32; Var fPos: Int64; Len:
   Byte): AnsiString;
Begin
   strm.Position := fPos;
   SetLength(Result, Len);
   ZeroMemory(@Result[1], Len);
   strm.Read(Result[1], Len);
   Inc(fPos, Len);
End;

{Function ReadFilename_DefinedLen(strm: TStream32; Var fPos: int64; Len: Byte):
   String;
Var
   s: pChar;
Begin
   strm.Position := fPos;

   GetMem(s, Len + 1);
   Try
      ZeroMemory(s, Len + 1);
      strm.Read(s[0], Len);
      //SetString(Result, s, Len);	// v4.6.7 changed from Result := StrPas(s);
      Result := String(s);
   Finally
      FreeMem(s, Len + 1);
   End;

   Inc(fPos, Len);
End;}
//-------------------------------------------------------------

Function SwapWords(SW: Integer): Integer;
Begin
   Result := Makelong(HiWord(SW), LoWord(SW));
End;
//-------------------------------------------------------------

Function UnixToDosFilename(str: String): String;
Var
   i: word;
Begin
   Result := str;
   For i := 1 To Length(Result) Do
      If Result[i] = '/' Then
         Result[i] := '\';
End;
//-------------------------------------------------------------

Function CompareMem(p1, p2: Pointer; Len: Integer): Boolean;
Var
   b1, b2: ^Byte;
Begin
   b1 := p1;
   b2 := p2;
   While (b1^ = b2^) And (longint(b1) < longint(p1) + Len) Do
   Begin
      Inc(b1);
      Inc(b2);
   End;
   Result := longint(b1) = longint(p1) + Len;
End;
//-------------------------------------------------------------

Function CreateDirEx(DirName: String): Boolean;

Var
   i: Integer;
   FullDirName: String;
   LFullDirName: Integer;
   PrevError: word;

Begin
   // if directory already exists, do nothing
   If DirExists(DirName) Then
      Result := True
   Else
   Begin
      // Disable "abort, retry, ignore..."
      PrevError := SetErrorMode(SEM_FAILCRITICALERRORS);

      // Make sure we have '\' at end of directory name
      FullDirName := AppendDirTail(ExpandFileName(DirName));
      LFullDirName := System.Length(FullDirName);

      // Search for initial '\' in directory name
      i := 1;
      While ((i <= LFullDirName) And (FullDirName[i] <> '\')) Do
         Inc(i);

      While (i <= LFullDirName) Do
      Begin
         // Create subdirectory up to current '\'
         CreateDir(copy(FullDirName, 1, Pred(i)));

         // Look for next '\' in directory name
         Inc(i);
         While ((i <= LFullDirName) And (FullDirName[i] <> '\')) Do
            Inc(i);

      End;
      // Reenable "abort, retry, ignore ..."
      SetErrorMode(PrevError);

      // Success if directory now exists
      Result := DirExists(DirName);
   End;

End;
//-------------------------------------------------------------

{Function ztvFileTimeToDateTime(FileTime: TFileTime): TDateTime;
Var
   SysTime: TSystemTime;
   LocalTime: TFileTime;
Begin
   FileTimeToLocalFileTime(FileTime, LocalTime);
   FileTimeToSystemTime(LocalTime, SysTime);
   Result :=
      EncodeDate(SysTime.wYear, SysTime.wMonth, SysTime.wDay) +
      EncodeTime(SysTime.wHour, SysTime.wMinute, SysTime.wSecond, SysTime.wMilliseconds);
End;}
//-------------------------------------------------------------
Function DoEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
Var
	I: Integer;
  	DayTable: PDayTable;
Begin
  	Result := False;
  	DayTable := @MonthDays[IsLeapYear(Year)];
  	If (Year >= 1) And (Year <= 9999) And (Month >= 1) And (Month <= 12) And
   	(Day >= 1) And (Day <= DayTable^[Month]) Then
  	Begin
    	For I := 1 To Month - 1 Do Inc(Day, DayTable^[I]);
    	I := Year - 1;
    	Date := I * 365 + I Div 4 - I Div 100 + I Div 400 + Day - DateDelta;
    	Result := True;
  	End;
End;

Function EncodeDate(Year, Month, Day: Word): TDateTime;
Begin
  If Not ztvGbls.DoEncodeDate(Year, Month, Day, Result) then
      Result := 29221.000694;           // 1/1/80 12:01 am
End;

//-------------------------------------------------------------

// Note: 2162720 is the integer value of 1/1/1980 12:01 am
Function ztvConvertDate(FileDate: Integer): TDateTime;
Begin
   Try
      If FileDate = 0 Then
         FileDate := 2162720; //1980

      // The below conversion is the same as:
      // Result := FileDateToDateTime( FileDate );
      // ...except FileDateToDateTime has no built-in error recovery
      Result :=
         EncodeDate(
         LongRec(FileDate).HI Shr 9 + 1980, // year
         (LongRec(FileDate).HI Shr 5 And 15) {Mod 13}, // month
         (LongRec(FileDate).HI And 31) {Mod 32}) + // day
         EncodeTime(
         (LongRec(FileDate).lo Shr 11) {Mod 25}, // hr
         (LongRec(FileDate).lo Shr 5 And 63) {Mod 61}, // min
         (LongRec(FileDate).lo And 31 Shl 1) {Mod 61}, // sec
         0);                            // milsec

   Except
      Result := 29221.000694;           // 1/1/80 12:01 am
   End;
End;
//-------------------------------------------------------------

Procedure DestroyImageList(DIL: TListView);
Begin
   DIL.LargeImages.Free();
   DIL.SmallImages.Free();
End;
//-------------------------------------------------------------

Function _DirectoryExists(Const Name: String): Boolean;
Var
   Code: Integer;
Begin
   Code := GetFileAttributes(pChar(Name));
   Result := (Code <> -1) And (FILE_ATTRIBUTE_DIRECTORY And Code <> 0);
End;
//-------------------------------------------------------------

Procedure InitializeImageList(Sender: TComponent; IIL: TListView);
Var
   SHFileInfo: TSHFileInfo;
Begin
   IIL.LargeImages := TImageList.Create(Sender);
   IIL.LargeImages.ShareImages := True;
   IIL.LargeImages.Handle :=
      ShGetFileInfo({'*.*'}'',
      0,
      SHFileInfo,
      SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX Or SHGFI_LARGEICON);

   IIL.SmallImages := TImageList.Create(Sender);
   IIL.SmallImages.ShareImages := True;
   IIL.SmallImages.Handle :=
      ShGetFileInfo({'*.*'}'',
      0,
      SHFileInfo,
      SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX Or SHGFI_SMALLICON);
End;
//-------------------------------------------------------------

Function TStringsToStr(ts: TStrings; SepChar: Char): String;
Const
   AllowedSepChars: Set Of Char = [' ', ',', ';', #0];
Var
   i: Integer;
Begin
   Result := '';
   If (SepChar In AllowedSepChars) Then
      For i := 0 To ts.Count - 1 Do
         Result := Result + ts[i];
End;
//-------------------------------------------------------------

Procedure StrToTStrings(p: pChar; ts: TStrings);
Const
   SepChars: Set Of Char = [',', ';', #0];
Var
   p2: pChar;
Begin
   While (p^ In SepChars) Do            // advance past seperators
      Inc(p);

   p2 := p;
   Repeat
      If (p^ In SepChars) Then
      Begin
         p2[p - p2] := #0;
         ts.Add(StrPas(p2));
         While (p^ In SepChars) Do      // advance past seperators
            Inc(p);
         p2 := p;
      End
      Else
         Inc(p);
   Until (p^ = #0);

   If Strlen(p2) > 0 Then
      ts.Add(StrPas(p2));
End;
//-------------------------------------------------------------

Procedure CopyMem(source, dest: Pointer; Count: Integer);
Var
   D: ^Byte Absolute dest;
   s: ^Byte Absolute source;
Begin
   While Count > 0 Do
   Begin
      D^ := s^;
      Inc(D);
      Inc(s);
      dec(Count);
   End;
End;
//-------------------------------------------------------------

Function CalcRatio(dx, dy: Integer): Integer; Overload;
Var
   R: Real;
Begin
   If (dy > 0) And (dy > dx) Then
      R := ((dy - dx) * 100) / dy
   Else
      R := 0;

   Result := Round(R);
End;
//-------------------------------------------------------------

Function CalcRatio(dx, dy: int64): Integer; Overload;
Var
   R: Real;
Begin
   If (dy > 0) And (dy > dx) Then
      R := ((dy - dx) * 100) / dy
   Else
      R := 0;

   Result := Round(R);
End;
//-------------------------------------------------------------

Function CalcRatio(dx, dy: Double): Integer; Overload;
Var
   R: Real;
Begin
   If (dy > 0) And (dy > dx) Then
      R := ((dy - dx) * 100) / dy
   Else
      R := 0;

   Result := Round(R);
End;
//-------------------------------------------------------------

Function GetDeflateMethodStr64(BitFlag: word): String;
Const
   DeflateMethods: Array[0..6] Of String =
   ('Def-64N', '', 'Def-64X', '', 'Def-64F', '', 'Def-64S');
Begin
   If (BitFlag < 7) And (BitFlag Mod 2 = 0) Then
      Result := DeflateMethods[BitFlag]
   Else
      Result := DeflateMethods[0];

   If Length(Result) = 0 Then
      Result := 'Deflate';
End;
//-------------------------------------------------------------

Function GetDeflateMethodStr(BitFlag: word): String;
Const
   DeflateMethods: Array[0..6] Of String =
   ('DeflateN', '', 'DeflateX', '', 'DeflateF', '', 'DeflateS');
Begin
   If (BitFlag < 7) And (BitFlag Mod 2 = 0) Then
      Result := DeflateMethods[BitFlag]
   Else
      Result := DeflateMethods[0];

   If Length(Result) = 0 Then
      Result := 'Deflate';
End;
//-------------------------------------------------------------

Function GetImageIndex(FileName: String): Integer;
Var
   SHFileInfo: TSHFileInfo;
Begin
   FillChar(SHFileInfo, SizeOf(SHFileInfo), #0);
   ShGetFileInfo(pChar(ExtractFileName(FileName)),
      0,
      SHFileInfo,
      SizeOf(TSHFileInfo),
      SHGFI_USEFILEATTRIBUTES Or
      SHGFI_SYSICONINDEX Or
      SHGFI_EXETYPE);

   Result := SHFileInfo.iIcon;
End;
//-------------------------------------------------------------

Function IsUncPath(Path: String): Boolean;
Begin
   If Length(Path) > 2 Then
      Result := (Path[1] = '\') And (Path[2] = '\')  //CompareStr( Copy(Path, 1, 2), '\\' ) = 0
   Else
      Result := false;
End;
//-------------------------------------------------------------

Function FileTimeToInt(FileTime: TFileTime): Integer;
Var
   LocalFileTime: TFileTime;
Begin
   FileTimeToLocalFileTime(FileTime, LocalFileTime);
   FileTimeToDosDateTime(LocalFileTime, LongRec(Result).HI, LongRec(Result).lo);
   //ShowMessage( FormatDateTime( 'mmddyy hh:mm', FileDateToDateTime( Result )) );
End;
//------------------------------------------------------------

Function ShortName(HWind: Hwnd; Const FileName: String): String;

   Function TooBig(hdcCtrl: hDC; Const width: Integer; Const s: String):
      Boolean;
   Var
      Size: TSize;
   Begin
      GetTextExtentPoint32(hdcCtrl, pChar(s), Length(s), Size);
      Result := Size.cx > width;
   End;

   Procedure CutFirstDirectory(Var s: String);
   Var
      ROOT: Boolean;
      p: Integer;
   Begin
      If s = '\' Then
         SetLength(s, 0)
      Else
      Begin
         If s[1] = '\' Then
         Begin
            ROOT := True;
            s := copy(s, 2, 255);
         End
         Else
            ROOT := false;

         If s[1] = '.' Then
            s := copy(s, 5, 255);

         p := Pos('\', s);

         If p <> 0 Then
            s := '...\' + copy(s, p + 1, 255)
         Else
            SetLength(s, 0);

         If ROOT Then
            s := '\' + s;
      End;
   End;
Var
   Drive: String[3];
   Dir, Name, Ext: String;
   p: Integer;
   rect: TRect;
   hdcCtrl: hDC;
   hwndCtrl: Hwnd;
Begin

   hwndCtrl := GetDlgItem(HWind, 1);
   hdcCtrl := GetDC(hwndCtrl);
   GetClientRect(hwndCtrl, rect);

   Result := FileName;
   Dir := ExtractFilePath(Result);
   Name := ExtractFileName(Result);
   p := Pos('.', Name);
   If p > 0 Then
      SetLength(Name, p - 1);

   Ext := ExtractFileExt(Result);

   If (Length(Dir) > 1) And (Dir[2] = ':') Then
   Begin
      Drive := copy(Dir, 1, 2);
      Dir := copy(Dir, 3, 255);
   End
   Else
      SetLength(Drive, 0);

   While ((Dir <> '') Or (Drive <> '')) And (TooBig(hdcCtrl, rect.Right -
      rect.Left, Result)) Do
   Begin
      If Dir = '\...\' Then
      Begin
         SetLength(Drive, 0);
         Dir := '...\';
      End
      Else If Dir = '' Then
         SetLength(Drive, 0)
      Else
         CutFirstDirectory(Dir);

      Result := Drive + Dir + Name + Ext;
   End;
   ReleaseDC(hwndCtrl, hdcCtrl);
End;
//-------------------------------------------------------------
// remove the comma from the formated number string
//-------------------------------------------------------------

Function UnformatNumber(s: String): String;
Var
   b: ^Byte;
   i: Integer;
Begin
   Result := '';
   b := @s[1];

   For i := 1 To Length(s) Do
   Begin
      // if a number, add it to the result string
      If (b^ >= 48) And (b^ <= 57) Then
         Result := Result + s[i];

      Inc(b);
   End;
End;
//------------------------------------------------------------


End.
