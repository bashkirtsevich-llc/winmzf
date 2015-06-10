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
Unit Err_Msgs;

Interface

{$R Err_Msgs.res}

Const
   E_BASE = 25100;                     (* max 25899 *)
   E_64BITHEADER = E_BASE + 1;
   E_32BITHEADER = E_BASE + 2;
   //E_ATTR = E_BASE + 3;
   E_ACEDLLERR = E_BASE + 3;          	// v4.8.6 added
   E_ARCREGISTRY = E_BASE + 4;         //v5.1 added TZLib 
   E_ATTRREADONLY = E_BASE + 6;
   E_ATTRHIDDEN = E_BASE + 7;
   E_ATTRSYSFILE = E_BASE + 8;
   E_ATTRDIRECTORY = E_BASE + 10;
   A_ATTRVOLUMEID = E_BASE + 12;
   A_FILEATTR = E_BASE + 13;
   E_BADHEADR = E_BASE + 14;
   E_BADBLOCK = E_BASE + 16;
   E_BADTABLE = E_BASE + 20;
   E_BADCODE = E_BASE + 21;
   E_BADCOMNT = E_BASE + 22;
   E_BEGIN = E_BASE + 23;
   E_BITSERR = E_BASE + 34;
   E_BROKENINDEX = E_BASE + 35;   		(* Multi-Vol archives *)
   E_FWRITE = E_BASE + 36;
   E_CABSETERR = E_BASE + 37;
   E_FCLOSE = E_BASE + 38;
   E_CODESET = E_BASE + 39;
   E_CORRUPTINPUT = E_BASE + 40;
   E_CREATEDIR = E_BASE + 50;
   E_CRCERROR = E_BASE + 51;
   E_DECTREE = E_BASE + 52;
   E_DECOMPRESSERR = E_BASE + 53;
   E_DISKLABELERROR = E_BASE + 54;
   E_DISKLABELSERIAL = E_BASE + 55;
   E_DISKSPACE = E_BASE + 56;
   E_DIRCREATEFAIL = E_BASE + 57;
   E_DIRNOTFOUND = E_BASE + 58;
   E_DIRUSECUR = E_BASE + 59;
   E_DISKFULL = E_BASE + 60;
   E_DRIVEERR = E_BASE + 70;
   E_DRVPROTECTED = E_BASE + 71;
   E_NOEOF = E_BASE + 72;               (* RAR *)
   E_EOF = E_BASE + 76;                 (* UUE *)
   E_EOL = E_BASE + 77;                 (* UUE *)
   E_EOLNOTFOUND = E_BASE + 78;         (* UUE *)
   E_EOFNOTFOUND = E_BASE + 79;         (* UUE *)
   E_FDELETE = E_BASE + 80;
   E_EXECFAILED = E_BASE + 90;
   E_FILESMATCH = E_BASE + 91;
   E_FILENOTFOUND = E_BASE + 92;
   E_FILESIZEMATCH = E_BASE + 93;
   E_FILETOBIG = E_BASE + 94;
   E_FLAGSUNKNOWN = E_BASE + 130;
   E_FLATSB = E_BASE + 132;
   E_FOPEN = E_BASE + 135;
   E_FREAD = E_BASE + 136;
   E_FCREATE = E_BASE + 137;
   E_HEADRCRC = E_BASE + 138;
   E_HDRERROR = E_BASE + 139;
   E_LINEBREAK = E_BASE + 140;          (* UUE *)
   E_LINEBEGIN = E_BASE + 141;          (* UUE *)
   E_ILLEGALCHAR = E_BASE + 142;        (* UUE *)
   E_INDEXBROKEN = E_BASE + 143;        (* TZLib - v5.1 added *)
   E_INVALIDARC = E_BASE + 144;
   E_INVALIDFN = E_BASE + 145;
   E_SENDBITLEN = E_BASE + 146;
   E_BITCOUNT = E_BASE + 147;
   E_INVALIDLEN = E_BASE + 148;
   E_LOCALHDR = E_BASE + 149;
   E_LOOKAHEAD = E_BASE + 150;
   E_MAXVOL = E_BASE + 151;
   E_MEMERR = E_BASE + 153;
   E_MEMSTRMERR = E_BASE + 154;
   E_MEMSTREAM = E_BASE + 155;
   E_FILESIZEEXCEED = E_BASE + 156;
   E_FILESTREAM = E_BASE + 158;
   E_NOCENTHDR = E_BASE + 159;
   E_NORARDLL = E_BASE + 160;
   E_NOCABDLL = E_BASE + 161;
   E_NOACEDLL = E_BASE + 162;
   E_MULTIVOL = E_BASE + 163;
   E_MULTIPARTGZIP = E_BASE + 164;
   E_NESTDIR = E_BASE + 165;
   E_NODECODE = E_BASE + 166;
   E_NOPASSWORD = E_BASE + 167;         // added in version 4.0
   E_NOTHINGTODO = E_BASE + 168;
   E_REQUIREDEVENT = E_BASE + 170;
   E_REQUIREDPROPERTY = E_BASE + 171;
   E_QUOTESYNTAX = E_BASE + 180;
   E_PARANSYNTAX = E_BASE + 181;
   E_PROTECTED = E_BASE + 190;
   E_REGERROR = E_BASE + 191;
   E_SEEKERR = E_BASE + 192;
   E_SHANNONFANO = E_BASE + 193;
   E_SHNODESKTOP = E_BASE + 194;
   E_SHSPECFOLDR = E_BASE + 195;
   E_SHMEMALLOC = E_BASE + 196;
   E_SIZETOSMALL = E_BASE + 197;  //ztvArchiveSplitter
   E_SRCHMODE = E_BASE + 198;
   E_STACK = E_BASE + 199;
   E_STREAMERROR = E_BASE + 200;
   E_STREAMSUPPORT = E_BASE + 224;
   E_TIMERERROR = E_BASE + 225;
   E_UNKNMETH = E_BASE + 226;
   E_UNKNTYPE = E_BASE + 227;
   E_UNKNVERS = E_BASE + 228;
   E_UPDATE = E_BASE + 250;
   E_UNKNOWN = E_BASE + 255;
   E_VERSIONERR = E_BASE + 302;
   E_USERCANCEL = E_BASE + 343;
   E_VOLSEQUENCE = E_BASE + 346;
   E_WIN32 = E_BASE + 350;
   //E_UPDATEZIPTV	  = E_BASE + 352;
   //E_ZIPLASTVOL	  = E_BASE + 355;

   V_BASE = 25900;                      (* Max 25999 *)
   V_VOLFIRST = V_BASE + 1;
   V_VOLPREV = V_BASE + 2;
   V_VOLNEXT = V_BASE + 3;
   V_VOLNOTINSET = V_BASE + 4;
   V_INSERTFIRSTDISK = V_BASE + 5;
   V_INSERTLASTDISK = V_BASE + 6;

   F_BASE = 26000;                      (* Max 26499 *)
   F_TZIPTV = F_BASE + 1;
   F_TUNSFX = F_BASE + 2;

   M_BASE = 26500;                      (* Max 26999 *)
   M_ENCRYPT = M_BASE + 13;
   M_INVALIDPW = M_BASE + 17;
   M_PASSWORDFAILED = M_BASE + 22;
   M_SKIPFILE = M_BASE + 300;
   M_NOTHING = M_BASE + 359;
   M_DIRNOTEXIST = M_BASE + 399;

Implementation
End.
