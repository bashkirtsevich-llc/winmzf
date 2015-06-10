unit AES_INTF;

(*************************************************************************

 DESCRIPTION     :  Interface unit for AES_DLL

 REQUIREMENTS    :  D2-D7/D9-D10, FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     02.07.04  W.Ehrhardt  Initial version
 0.11     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.12     01.12.04  we          AES_Err_Data_After_Short_Block
 0.23     01.12.04  we          AES_ prefix for CTR increment routines
 0.24     24.12.04  we          AES_Get/SetFastInit
 0.25     09.07.06  we          CMAC, updated OMAC, checked: D9-D10
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


type
  TAES_XTSContext = packed record
                      main : TAESContext; {Main  context}
                      tweak: TAESContext; {Tweak context}
                    end;


const
  AESBLKSIZE = sizeof(TAESBlock);



function  AES_DLL_Version: PChar; 
stdcall;  external 'AES_DLL' name 'AES_DLL_Version';
  {-Return DLL version as PChar}


procedure AES_XorBlock(const B1, B2: TAESBlock; var B3: TAESBlock);
stdcall;  external 'AES_DLL' name 'AES_XorBlock';
  {-xor two blocks, result in third}

function  AES_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_Init';
  {-AES key expansion, error if invalid key size}

procedure AES_SetFastInit(value: boolean);
stdcall;  external 'AES_DLL' name 'AES_SetFastInit';
  {-set FastInit variable}

function  AES_GetFastInit: boolean;
stdcall;  external 'AES_DLL' name 'AES_GetFastInit';
  {-Returns FastInit variable}


function  AES_Init_Encr(const  Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_Init_Encr';
  {-AES key expansion, error if invalid key size}

procedure AES_Encrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
stdcall;  external 'AES_DLL' name 'AES_Encrypt';
  {-encrypt one block, not checked: key must be encryption key}



function  AES_ECB_Init_Encr(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_ECB_Init_Encr';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_ECB_Init_Decr(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_ECB_Init_Decr';
  {-AES key expansion, error if invalid key size, encrypt IV}



function  AES_ECB_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_ECB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}

function  AES_ECB_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_ECB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}



function  AES_Init_Decr(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_Init_Decr';
  {-AES key expansion, InvMixColumn(Key) for Decypt, error if invalid key size}

procedure AES_Decrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
stdcall;  external 'AES_DLL' name 'AES_Decrypt';
  {-decrypt one block (in ECB mode)}



function  AES_CBC_Init_Encr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CBC_Init_Encr';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_CBC_Init_Decr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CBC_Init_Decr';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_CBC_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CBC_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}

function  AES_CBC_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CBC_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}



function  AES_CFB_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CFB_Init';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_CFB_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CFB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}

function  AES_CFB_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CFB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}




function  AES_CFB8_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CFB8_Init';
  {-AES key expansion, error if invalid key size, store IV}

function  AES_CFB8_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CFB8_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB8 mode}

function  AES_CFB8_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CFB8_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB8 mode}



function  AES_OFB_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_OFB_Init';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_OFB_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_OFB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in OFB mode}

function  AES_OFB_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_OFB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in OFB mode}



function  AES_CTR_Init(const Key; KeyBits: word; const CTR: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CTR_Init';
  {-AES key expansion, error if inv. key size, encrypt CTR}

function  AES_CTR_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CTR_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}

function  AES_CTR_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CTR_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}

function  AES_SetIncProc(IncP: TIncProc; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_SetIncProc';
  {-Set user supplied IncCTR proc}

procedure AES_IncMSBFull(var CTR: TAESBlock);
stdcall;  external 'AES_DLL' name 'AES_IncMSBFull';
  {-Increment CTR[15]..CTR[0]}

procedure AES_IncLSBFull(var CTR: TAESBlock);
stdcall;  external 'AES_DLL' name 'AES_IncLSBFull';
  {-Increment CTR[0]..CTR[15]}

procedure AES_IncMSBPart(var CTR: TAESBlock);
stdcall;  external 'AES_DLL' name 'AES_IncMSBPart';
  {-Increment CTR[15]..CTR[8]}

procedure AES_IncLSBPart(var CTR: TAESBlock);
stdcall;  external 'AES_DLL' name 'AES_IncLSBPart';
  {-Increment CTR[0]..CTR[7]}



function  AES_OMAC_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_OMAC_Init';
  {-OMAC init: AES key expansion, error if inv. key size}

function  AES_OMAC_Update(data: pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_OMAC_Update';
  {-OMAC data input, may be called more than once}

procedure AES_OMAC_Final(var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'AES_DLL' name 'AES_OMAC_Final';
  {-end data input, calculate OMAC=OMAC1 tag}

procedure AES_OMAC1_Final(var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'AES_DLL' name 'AES_OMAC1_Final';
  {-end data input, calculate OMAC1 tag}

procedure AES_OMAC2_Final(var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'AES_DLL' name 'AES_OMAC2_Final';
  {-end data input, calculate OMAC2 tag}

procedure AES_OMACx_Final(OMAC2: boolean; var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'AES_DLL' name 'AES_OMACx_Final';
  {-end data input, calculate OMAC tag}

function  AES_OMAC_UpdateXL(data: pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_OMAC_UpdateXL';
  {-OMAC data input, may be called more than once}



function  AES_CMAC_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CMAC_Init';
  {-CMAC init: AES key expansion, error if inv. key size}

function  AES_CMAC_Update(data: pointer; ILen: word; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CMAC_Update';
  {-CMAC data input, may be called more than once}

procedure AES_CMAC_Final(var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'AES_DLL' name 'AES_CMAC_Final';
  {-end data input, calculate CMAC=OMAC1 tag}

function  AES_CMAC_UpdateXL(data: pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'AES_DLL' name 'AES_CMAC_UpdateXL';
  {-CMAC data input, may be called more than once}



function  AES_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TAES_EAXContext): integer;
stdcall;  external 'AES_DLL' name 'AES_EAX_Init';
  {-Init hdr and msg OMACs, setp AESCTR with nonce tag}

function  AES_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TAES_EAXContext): integer;
stdcall;  external 'AES_DLL' name 'AES_EAX_Provide_Header';
  {-Supply a message header. The header "grows" with each call}

function  AES_EAX_Encrypt(ptp, ctp: Pointer; ILen: word; var ctx: TAES_EAXContext): integer;
stdcall;  external 'AES_DLL' name 'AES_EAX_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

function  AES_EAX_Decrypt(ctp, ptp: Pointer; ILen: word; var ctx: TAES_EAXContext): integer;
stdcall;  external 'AES_DLL' name 'AES_EAX_Decrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

procedure AES_EAX_Final(var tag: TAESBlock; var ctx: TAES_EAXContext);
stdcall;  external 'AES_DLL' name 'AES_EAX_Final';
  {-Compute EAX tag from context}


function AES_CPRF128(const Key; KeyBytes: word; msg: pointer; msglen: longint; var PRV: TAESBlock): integer;
stdcall;  external 'AES_DLL' name 'AES_CPRF128';
  {Calculate variable-length key AES CMAC Pseudo-Random Function-128 for msg}
  {returns AES_OMAC error and 128-bit pseudo-random value PRV}

function AES_CPRF128_selftest: boolean;
stdcall;  external 'AES_DLL' name 'AES_CPRF128_selftest';
  {-Seftest with RFC 4615 test vectors}


function AES_XTS_Init_Encr(const K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
stdcall;  external 'AES_DLL' name 'AES_XTS_Init_Encr';
  {-Init XTS encrypt context (key expansion), error if invalid key size}

function AES_XTS_Encrypt(ptp, ctp: Pointer; ILen: longint; const twk: TAESBlock; var ctx: TAES_XTSContext): integer;
stdcall;  external 'AES_DLL' name 'AES_XTS_Encrypt';
  {-Encrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}

function AES_XTS_Init_Decr(const K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
stdcall;  external 'AES_DLL' name 'AES_XTS_Init_Decr';
  {-Init XTS decrypt context (key expansion), error if invalid key size}

function AES_XTS_Decrypt(ctp, ptp: Pointer; ILen: longint; const twk: TAESBlock; var ctx: TAES_XTSContext): integer;
stdcall;  external 'AES_DLL' name 'AES_XTS_Decrypt';
  {-Decrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}



implementation

end.
