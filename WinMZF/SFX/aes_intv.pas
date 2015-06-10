unit AES_INTV;

{$ifdef VirtualPascal}
  {$stdcall+}
{$else}
  Error('Interface unit for VirtualPascal');
{$endif}

(*************************************************************************

 DESCRIPTION     :  Interface unit for AES_DLL

 REQUIREMENTS    :  VirtualPascal

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     02.07.04  W.Ehrhardt  Initial version from AES_Intf
 0.20     03.07.04  we          VirtualPascal syntax
 0.21     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.22     01.12.04  we          AES_Err_Data_After_Short_Block
 0.23     01.12.04  we          AES_ prefix for CTR increment routines
 0.24     24.12.04  we          AES_Get/SetFastInit
 0.25     09.07.06  we          CMAC, updated OMAC
 0.26     14.06.07  we          Type TAES_EAXContext
 0.27     16.06.07  we          AES_CPRF128
 0.28     29.09.07  we          AES_XTS
 0.29     25.12.07  we          AES_CFB8
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2002-2007 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)

interface


const
  AES_Err_Invalid_Key_Size       = -1;  {Key size <> 128, 192, or 256 Bits}
  AES_Err_Invalid_Mode           = -2;  {Encr/Decr with Init for Decr/Encr}
  AES_Err_Invalid_Length         = -3;  {No full block for cipher stealing}
  AES_Err_Data_After_Short_Block = -4;  {Short block must be last}
  AES_Err_MultipleIncProcs       = -5;  {More than one IncProc Setting    }
  AES_Err_NIL_Pointer            = -6;  {nil pointer to block with nonzero length}

const
  AESMaxRounds = 14;

type
  TAESBlock   = packed array[0..15] of byte;
  PAESBlock   = ^TAESBlock;
  TKeyArray   = packed array[0..AESMaxRounds] of TAESBlock;
  TIncProc    = procedure(var CTR: TAESBlock);   {user supplied IncCTR proc}
  TAESContext = packed record
                  RK      : TKeyArray;  {Key (encr. or decr.)   }
                  IV      : TAESBlock;  {IV or CTR              }
                  buf     : TAESBlock;  {Work buffer            }
                  bLen    : word;       {Bytes used in buf      }
                  Rounds  : word;       {Number of rounds       }
                  KeyBits : word;       {Number of bits in key  }
                  Decrypt : byte;       {<>0 if decrypting key  }
                  Flag    : byte;       {Bit 1: Short block     }
                  IncProc : TIncProc;   {Increment proc CTR-Mode}
                end;

type
  TAES_EAXContext = packed record
                      HdrOMAC : TAESContext; {Hdr OMAC1  context}
                      MsgOMAC : TAESContext; {Msg OMAC1  context}
                      ctr_ctx : TAESContext; {Msg AESCTR context}
                      NonceTag: TAESBlock;   {nonce tag         }
                      tagsize : word;        {tag size (unused) }
                      flags   : word;        {ctx flags (unused)}
                    end;

const
  AESBLKSIZE = sizeof(TAESBlock);


type
  TAES_XTSContext = packed record
                      main : TAESContext; {Main  context}
                      tweak: TAESContext; {Tweak context}
                    end;


function  AES_DLL_Version: PChar;
  {-Return DLL version as PChar}

procedure AES_XorBlock(const B1, B2: TAESBlock; var B3: TAESBlock);
  {-xor two blocks, result in third}

function  AES_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}

function  AES_Init_Encr(const  Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}

procedure AES_Encrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
  {-encrypt one block, not checked: key must be encryption key}

function  AES_Init_Decr(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, InvMixColumn(Key) for Decypt, error if invalid key size}

procedure AES_Decrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
  {-decrypt one block (in ECB mode)}

procedure AES_SetFastInit(value: boolean);
  {-set FastInit variable}

function  AES_GetFastInit: boolean;
  {-Returns FastInit variable}



function  AES_ECB_Init_Encr(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_ECB_Init_Decr(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_ECB_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}

function  AES_ECB_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}



function  AES_CBC_Init_Encr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_CBC_Init_Decr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_CBC_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}

function  AES_CBC_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}



function  AES_CFB_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_CFB_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}

function  AES_CFB_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}



function  AES_CFB8_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, store IV}

function  AES_CFB8_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB8 mode}

function  AES_CFB8_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB8 mode}



function  AES_OFB_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_OFB_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in OFB mode}

function  AES_OFB_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in OFB mode}



function  AES_CTR_Init(const Key; KeyBits: word; const CTR: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if inv. key size, encrypt CTR}

function  AES_CTR_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}

function  AES_CTR_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}

function  AES_SetIncProc(IncP: TIncProc; var ctx: TAESContext): integer;
  {-Set user supplied IncCTR proc}

procedure AES_IncMSBFull(var CTR: TAESBlock);
  {-Increment CTR[15]..CTR[0]}

procedure AES_IncLSBFull(var CTR: TAESBlock);
  {-Increment CTR[0]..CTR[15]}

procedure AES_IncMSBPart(var CTR: TAESBlock);
  {-Increment CTR[15]..CTR[8]}

procedure AES_IncLSBPart(var CTR: TAESBlock);
  {-Increment CTR[0]..CTR[7]}



function  AES_OMAC_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-OMAC init: AES key expansion, error if inv. key size}

function  AES_OMAC_Update(data: pointer; ILen: word; var ctx: TAESContext): integer;
  {-OMAC data input, may be called more than once}

procedure AES_OMAC_Final(var tag: TAESBlock; var ctx: TAESContext);
  {-end data input, calculate OMAC=OMAC1 tag}

procedure AES_OMAC1_Final(var tag: TAESBlock; var ctx: TAESContext);
  {-end data input, calculate OMAC1 tag}

procedure AES_OMAC2_Final(var tag: TAESBlock; var ctx: TAESContext);
  {-end data input, calculate OMAC2 tag}

function  AES_OMAC_UpdateXL(data: pointer; ILen: longint; var ctx: TAESContext): integer;
  {-OMAC data input, may be called more than once}

procedure AES_OMACx_Final(OMAC2: boolean; var tag: TAESBlock; var ctx: TAESContext);
  {-end data input, calculate OMAC tag}



function  AES_CMAC_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-CMAC init: AES key expansion, error if inv. key size}

function  AES_CMAC_Update(data: pointer; ILen: word; var ctx: TAESContext): integer;
  {-CMAC data input, may be called more than once}

procedure AES_CMAC_Final(var tag: TAESBlock; var ctx: TAESContext);
  {-end data input, calculate CMAC=OMAC1 tag}

function  AES_CMAC_UpdateXL(data: pointer; ILen: longint; var ctx: TAESContext): integer;
  {-CMAC data input, may be called more than once}



function  AES_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TAES_EAXContext): integer;
  {-Init hdr and msg OMACs, setp AESCTR with nonce tag}

function  AES_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TAES_EAXContext): integer;
  {-Supply a message header. The header "grows" with each call}

function  AES_EAX_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAES_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

function  AES_EAX_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAES_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

procedure AES_EAX_Final(var tag: TAESBlock; var ctx: TAES_EAXContext);
  {-Compute EAX tag from context}



function AES_CPRF128(const Key; KeyBytes: word; msg: pointer; msglen: longint; var PRV: TAESBlock): integer;
  {Calculate variable-length key AES CMAC Pseudo-Random Function-128 for msg}
  {returns AES_OMAC error and 128-bit pseudo-random value PRV}

function AES_CPRF128_selftest: boolean;
  {-Seftest with RFC 4615 test vectors}



function AES_XTS_Init_Encr({$ifdef CONST}const{$else}var{$endif} K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
  {-Init XTS encrypt context (key expansion), error if invalid key size}

function AES_XTS_Encrypt(ptp, ctp: Pointer; ILen: longint;
            {$ifdef CONST}const{$else}var{$endif} twk: TAESBlock; var ctx: TAES_XTSContext): integer;
  {-Encrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}

function AES_XTS_Init_Decr({$ifdef CONST}const{$else}var{$endif} K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
  {-Init XTS decrypt context (key expansion), error if invalid key size}

function AES_XTS_Decrypt(ctp, ptp: Pointer; ILen: longint;
            {$ifdef CONST}const{$else}var{$endif} twk: TAESBlock; var ctx: TAES_XTSContext): integer;
  {-Decrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}



implementation



function  AES_DLL_Version;         external 'AES_DLL' name 'AES_DLL_Version';

procedure AES_XorBlock;            external 'AES_DLL' name 'AES_XorBlock';

function  AES_Init;                external 'AES_DLL' name 'AES_Init';
function  AES_Init_Decr;           external 'AES_DLL' name 'AES_Init_Decr';
function  AES_Init_Encr;           external 'AES_DLL' name 'AES_Init_Encr';
procedure AES_Decrypt;             external 'AES_DLL' name 'AES_Decrypt';
procedure AES_Encrypt;             external 'AES_DLL' name 'AES_Encrypt';
procedure AES_SetFastInit;         external 'AES_DLL' name 'AES_SetFastInit';
function  AES_GetFastInit;         external 'AES_DLL' name 'AES_GetFastInit';


function  AES_ECB_Init_Encr;       external 'AES_DLL' name 'AES_ECB_Init_Encr';
function  AES_ECB_Init_Decr;       external 'AES_DLL' name 'AES_ECB_Init_Decr';
function  AES_ECB_Encrypt;         external 'AES_DLL' name 'AES_ECB_Encrypt';
function  AES_ECB_Decrypt;         external 'AES_DLL' name 'AES_ECB_Decrypt';

function  AES_CBC_Init_Encr;       external 'AES_DLL' name 'AES_CBC_Init_Encr';
function  AES_CBC_Init_Decr;       external 'AES_DLL' name 'AES_CBC_Init_Decr';
function  AES_CBC_Encrypt;         external 'AES_DLL' name 'AES_CBC_Encrypt';
function  AES_CBC_Decrypt;         external 'AES_DLL' name 'AES_CBC_Decrypt';

function  AES_CFB_Init;            external 'AES_DLL' name 'AES_CFB_Init';
function  AES_CFB_Encrypt;         external 'AES_DLL' name 'AES_CFB_Encrypt';
function  AES_CFB_Decrypt;         external 'AES_DLL' name 'AES_CFB_Decrypt';

function  AES_CFB8_Init;           external 'AES_DLL' name 'AES_CFB8_Init';
function  AES_CFB8_Encrypt;        external 'AES_DLL' name 'AES_CFB8_Encrypt';
function  AES_CFB8_Decrypt;        external 'AES_DLL' name 'AES_CFB8_Decrypt';

function  AES_CTR_Init;            external 'AES_DLL' name 'AES_CTR_Init';
function  AES_CTR_Encrypt;         external 'AES_DLL' name 'AES_CTR_Encrypt';
function  AES_CTR_Decrypt;         external 'AES_DLL' name 'AES_CTR_Decrypt';
function  AES_SetIncProc;          external 'AES_DLL' name 'AES_SetIncProc';
procedure AES_IncLSBFull;          external 'AES_DLL' name 'AES_IncLSBFull';
procedure AES_IncLSBPart;          external 'AES_DLL' name 'AES_IncLSBPart';
procedure AES_IncMSBFull;          external 'AES_DLL' name 'AES_IncMSBFull';
procedure AES_IncMSBPart;          external 'AES_DLL' name 'AES_IncMSBPart';

function  AES_OFB_Init;            external 'AES_DLL' name 'AES_OFB_Init';
function  AES_OFB_Encrypt;         external 'AES_DLL' name 'AES_OFB_Encrypt';
function  AES_OFB_Decrypt;         external 'AES_DLL' name 'AES_OFB_Decrypt';

function  AES_OMAC_Init;           external 'AES_DLL' name 'AES_OMAC_Init';
function  AES_OMAC_Update;         external 'AES_DLL' name 'AES_OMAC_Update';
function  AES_OMAC_UpdateXL;       external 'AES_DLL' name 'AES_OMAC_UpdateXL';
procedure AES_OMAC_Final;          external 'AES_DLL' name 'AES_OMAC_Final';
procedure AES_OMAC1_Final;         external 'AES_DLL' name 'AES_OMAC1_Final';
procedure AES_OMAC2_Final;         external 'AES_DLL' name 'AES_OMAC2_Final';
procedure AES_OMACx_Final;         external 'AES_DLL' name 'AES_OMACx_Final';

function  AES_CMAC_Init;           external 'AES_DLL' name 'AES_CMAC_Init';
function  AES_CMAC_Update;         external 'AES_DLL' name 'AES_CMAC_Update';
function  AES_CMAC_UpdateXL;       external 'AES_DLL' name 'AES_CMAC_UpdateXL';
procedure AES_CMAC_Final;          external 'AES_DLL' name 'AES_CMAC_Final';

function  AES_EAX_Init;            external 'AES_DLL' name 'AES_EAX_Init';
function  AES_EAX_Encrypt;         external 'AES_DLL' name 'AES_EAX_Encrypt';
function  AES_EAX_Decrypt;         external 'AES_DLL' name 'AES_EAX_Decrypt';
procedure AES_EAX_Final;           external 'AES_DLL' name 'AES_EAX_Final';
function  AES_EAX_Provide_Header;  external 'AES_DLL' name 'AES_EAX_Provide_Header';


function  AES_CPRF128;             external 'AES_DLL' name 'AES_CPRF128';
function  AES_CPRF128_selftest;    external 'AES_DLL' name 'AES_CPRF128_selftest';


function AES_XTS_Init_Encr;        external 'AES_DLL' name 'AES_XTS_Init_Encr';
function AES_XTS_Encrypt;          external 'AES_DLL' name 'AES_XTS_Encrypt';
function AES_XTS_Init_Decr;        external 'AES_DLL' name 'AES_XTS_Init_Decr';
function AES_XTS_Decrypt;          external 'AES_DLL' name 'AES_XTS_Decrypt';

end.

