unit OneCryptDef;

interface

function EncryptFileD(Key, InPath, OutPath : AnsiString) : integer; external 'OneCrypt.dll';
function DecryptFileD(Key, InPath, OutPath : AnsiString) : integer; external 'OneCrypt.dll';
function WipeFileD(InPath : AnsiString) : integer; external 'OneCrypt.dll';
function EncryptStringD(Key, InString : AnsiString) : AnsiString; external 'OneCrypt.dll';
function DecryptStringD(Key, InString : AnsiString) : AnsiString; external 'OneCrypt.dll';
function OpenCryptD(Key : AnsiString) : boolean; external 'OneCrypt.dll';
function EncryptBufferD(const InBuffer; var OutBuffer; NBytes : integer ) : boolean; external 'OneCrypt.dll';
function DecryptBufferD(const InBuffer; var OutBuffer; NBytes : integer ) : boolean; external 'OneCrypt.dll';
function CloseCryptD : boolean; external 'OneCrypt.dll';
{
  Note:

   When using EncryptBufferD and DecryptBufferD, define
   InBuffer and OutBuffer as array of byte. e.g.

   var
     InBuffer, OutBuffer : array[0..4095] of byte;
}

implementation

end.
