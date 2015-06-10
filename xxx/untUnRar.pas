//MYC:2006/08/17 = Correction in TRARHeaderDataEx interface in order to handle stress Filenames in archive
{
  see UNRARDLL.TXT for Informations about UnRar.dll - Functions and structures
}
unit untUnRar;

interface

uses
  Windows;

const
{ // deutsche Meldungen
  MSG1  = 'Fehler beim Schließen';
  MSG2  = 'Kein Passwort angegeben!';
  MSG3  = 'Verwendetes Passwort: ';
  MSG4  = 'UnRar.Dll nicht geladen';
  MSG5  = 'Fehlerhafte Daten';
  MSG6  = 'Fehlerhaftes Archiv';
  MSG7  = 'Unbekanntes Datenformat';
  MSG8  = 'Volumn-Fehler';
  MSG9  = 'Fehler beim Erstellen der Datei';
  MSG10 = 'Fehler beim Schließen der Datei';
  MSG11 = 'Lesefehler';
  MSG12 = 'Schreibfehler';
  MSG14 = 'Kein Speicher';
  MSG16 = 'Buffer zu klein';
  MSG17 = 'Datei-Header defekt';

  VOLDLGCAPTION  = 'Nächstes Archiv: ...';
  PASSDLGCAPTION = 'Passwort: ...';
  CANCELCAPTION  = 'Abbrechen';
  OKCAPTION      = 'Ok';

  COMPRESSMETHODSTORE   = 'speichern';
  COMPRESSMETHODFASTEST = 'sehr schnell';
  COMPRESSMETHODFAST    = 'schnell';
  COMPRESSMETHODNORMAL  = 'normal';
  COMPRESSMETHODGOOD    = 'gut';
  COMPRESSMETHODBEST    = 'sehr gut';
}

{ // delete comments for english text
  MSG1  = 'Error on close';
  MSG2  = 'No Password!';
  MSG3  = 'Used Pass is: ';
  MSG4  = 'UnRar.Dll not loaded';
  MSG5  = 'corrupt data';
  MSG6  = 'Fehlerhaftes Archiv';
  MSG7  = 'unknown format';
  MSG8  = 'Volumn-Error';
  MSG9  = 'error on create file';
  MSG10 = 'error on closing file';
  MSG11 = 'read error';
  MSG12 = 'write error';
  MSG14 = 'no memory';
  MSG16 = 'buffer to small';
  MSG17 = 'File Haeder corrupt';

  VOLDLGCAPTION  = 'Next Archive: ...';
  PASSDLGCAPTION = 'Password: ...';
  CANCELCAPTION  = 'Cancel';
  OKCAPTION      = 'Ok';

  COMPRESSMETHODSTORE   = 'store';
  COMPRESSMETHODFASTEST = 'fastest';
  COMPRESSMETHODFAST    = 'fast';
  COMPRESSMETHODNORMAL  = 'normal';
  COMPRESSMETHODGOOD    = 'good';
  COMPRESSMETHODBEST    = 'best';
}

  // delete comments for french text
  MSG1  = 'Erreur lors de la fermeture';
  MSG2  = 'Pas de mot de passe!';
  MSG3  = 'Le mot de passe utilisé est : ';
  MSG4  = 'UnRar.Dll not loaded';
  MSG5  = 'données corrompues';
  MSG6  = 'Archive défectueuse';
  MSG7  = 'format inconnu';
  MSG8  = 'Volumn-Error';
  MSG9  = 'Erreur à la création du fichier';
  MSG10 = 'Erreur à la fermeture du fichier';
  MSG11 = 'Erreur de lecture';
  MSG12 = 'Erreur d''écriture';
  MSG14 = 'Mémoire insuffisante';
  MSG16 = 'buffer trop petit';
  MSG17 = 'Entête de fichier corrompu';

  VOLDLGCAPTION  = 'Archive suivante : ...';
  PASSDLGCAPTION = 'Mot de passe : ...';
  CANCELCAPTION  = 'Annuler';
  OKCAPTION      = 'Ok';

  COMPRESSMETHODSTORE   = 'pas de compression';
  COMPRESSMETHODFASTEST = 'la plus rapide';
  COMPRESSMETHODFAST    = 'rapide';
  COMPRESSMETHODNORMAL  = 'normale';
  COMPRESSMETHODGOOD    = 'bonne';
  COMPRESSMETHODBEST    = 'la meilleure';

  // Constants not from UnRar.h !
  RAR_METHOD_STORE   = 48;
  RAR_METHOD_FASTEST = 49;
  RAR_METHOD_FAST    = 50;
  RAR_METHOD_NORMAL  = 51;
  RAR_METHOD_GOOD    = 52;
  RAR_METHOD_BEST    = 53;

  RAR_SUCCESS = 0;
  ERAR_COMMENTS_EXISTS = 1;
  ERAR_NO_COMMENTS     = 0;

  // Constants from UnRar.h
  ERAR_END_ARCHIVE     = 10;
  ERAR_NO_MEMORY       = 11;
  ERAR_BAD_DATA        = 12;
  ERAR_BAD_ARCHIVE     = 13;
  ERAR_UNKNOWN_FORMAT  = 14;
  ERAR_EOPEN           = 15;
  ERAR_ECREATE         = 16;
  ERAR_ECLOSE          = 17;
  ERAR_EREAD           = 18;
  ERAR_EWRITE          = 19;
  ERAR_SMALL_BUF       = 20;
  ERAR_UNKNOWN         = 21;
  RAR_OM_LIST          = 0;
  RAR_OM_EXTRACT       = 1;
  RAR_SKIP             = 0;
  RAR_TEST             = 1;
  RAR_EXTRACT          = 2;
  RAR_VOL_ASK          = 0;
  RAR_VOL_NOTIFY       = 1;
  RAR_DLL_VERSION      = 3;

  UCM_CHANGEVOLUME     = 0;
  UCM_PROCESSDATA      = 1;
  UCM_NEEDPASSWORD     = 2;

  // Max. Comment Size
  MAXRARCOMMENTSIZE = 1024 * 64; // 64kB

type
  // Callback functions, the first 2 are deprecated - use TUnRarCallBack instead
  TProcessDataProc = function(Addr: PByte; Size: integer): integer; stdcall;
  TChangeVolProc   = function(ArcName: PChar; Mode: integer): integer; stdcall;
  TUnRarCallBack   = function(msg: Cardinal; UserData, P1, P2: longint): integer; stdcall;

  // Header for every file in an archive
  TRARHeaderData = record
    ArcName    : array[0..259] of char;
    FileName   : array[0..259] of char;
    Flags      : cardinal;
    PackSize   : cardinal;
    UnpSize    : cardinal;
    HostOS     : cardinal;
    FileCRC    : cardinal;
    FileTime   : cardinal;
    UnpVer     : cardinal;
    Method     : cardinal;
    FileAttr   : cardinal;
    CmtBuf     : PChar;
    CmtBufSize : cardinal;
    CmtSize    : cardinal;
    CmtState   : cardinal;
  end;
  PRARHeaderData = ^TRARHeaderData;

  // extended Header - not used in this component
  TRARHeaderDataEx = record
    ArcName      : array[0..1023] of char;
    ArcNameW     : array[0..1023] of WideChar;
    FileName     : array[0..1023] of char;
    FileNameW    : array[0..1023] of WideChar;
    Flags        : cardinal;
    PackSize     : cardinal;
    PackSizeHigh : cardinal;
    UnpSize      : cardinal;
    UnpSizeHigh  : cardinal;
    HostOS       : cardinal;
    FileCRC      : cardinal;
    FileTime     : cardinal;
    UnpVer       : cardinal;
    Method       : cardinal;
    FileAttr     : cardinal;
    CmtBuf       : PChar;
    CmtBufSize   : cardinal;
    CmtSize      : cardinal;
    CmtState     : cardinal;
    Reserved     : array[1..1024] of cardinal;
  end;
//MYC:2006/08/17  //PRARHeaderDataEx = TRARHeaderDataEx;
  PRARHeaderDataEx = ^TRARHeaderDataEx;

  // Archive-Data for opening rar-archive
  TRAROpenArchiveData = record
    ArcName    : PChar;
    OpenMode   : cardinal;
    OpenResult : cardinal;
    CmtBuf     : PChar;
    CmtBufSize : cardinal;
    CmtSize    : cardinal;
    CmtState   : cardinal;
  end;
  PRAROpenArchiveData = ^TRAROpenArchiveData;

  // extended Archive-Data - not used in this component
  TRAROpenArchiveDataEx = record
    ArcName    : PChar;
    ArcNameW   : PWideChar;
    OpenMode   : cardinal;
    OpenResult : cardinal;
    CmtBuf     : PChar;
    CmtBufSize : cardinal;
    CmtSize    : cardinal;
    CmtState   : cardinal;
    Flags      : cardinal;
    Reserved   : array[1..32] of cardinal;
  end;
  PRAROpenArchiveDataEx = ^TRAROpenArchiveDataEx;

var
  // Flag for: Is Dll loaded...
  IsLoaded: boolean = false;
  // function Pointer - Dll is always dynamicly loaded
  RAROpenArchive        : function(ArchiveData: PRAROpenArchiveData): THandle; stdcall;
  RAROpenArchiveEx      : function(ArchiveData: PRAROpenArchiveDataEx): THandle; stdcall;
  RARCloseArchive       : function(hArcData: THandle): integer; stdcall;
  RARReadHeader         : function(hArcData: THandle; HeaderData: PRARHeaderData): Integer; stdcall;
  RARReadHeaderEx       : function(hArcData: THandle; HeaderData: PRARHeaderDataEx): Integer; stdcall;
  RARProcessFile        : function(hArcData: THandle; Operation: Integer; DestPath, DestName: PChar): Integer; stdcall;
  RARSetCallback        : procedure(hArcData: THandle; Callback: TUnRarCallback; UserData: longint); stdcall;
  RARSetChangeVolProc   : procedure(hArcData: THandle; ChangeVolProc: TChangeVolProc); stdcall;
  RARSetProcessDataProc : procedure(hArcData: THandle; ProcessDataProc: TProcessDataProc); stdcall;
  RARSetPassword        : procedure(hArcData: THandle; Password: PChar); stdcall;
  RARGetDllVersion      : function:Integer; stdcall;

// helper functions for (un)loading the Dll and check for loaded
procedure LoadRarLibrary;
procedure UnLoadRarLibrary;
function  IsRarLoaded: boolean;

implementation

var
  // Dll-Handle
  h: THandle;

// Loads the UnRar.dll
procedure LoadRarLibrary;
begin
  // UnRar.dll must exists in typically dll-paths
  // 1. Application-Directory
  // 2. Current Directory
  // 3. System-Directory
  // 4. Windows-Direcory
  // 5. Directories from PATH-Variable
  h := LoadLibrary('unrar.dll');
  if h <> 0 then
  begin
    IsLoaded := true;
    @RAROpenArchive        := GetProcAddress(h, 'RAROpenArchive');
    @RAROpenArchiveEx      := GetProcAddress(h, 'RAROpenArchiveEx');
    @RARCloseArchive       := GetProcAddress(h, 'RARCloseArchive');
    @RARReadHeader         := GetProcAddress(h, 'RARReadHeader');
    @RARReadHeaderEx       := GetProcAddress(h, 'RARReadHeaderEx');
    @RARProcessFile        := GetProcAddress(h, 'RARProcessFile');
    @RARSetCallback        := GetProcAddress(h, 'RARSetCallback');
    @RARSetChangeVolProc   := GetProcAddress(h, 'RARSetChangeVolProc');
    @RARSetProcessDataProc := GetProcAddress(h, 'RARSetProcessDataProc');
    @RARSetPassword        := GetProcAddress(h, 'RARSetPassword');
    @RARGetDllVersion      := GetProcAddress(h, 'RARGetDllVersion');
  end;
end;

// Unloading Library
procedure UnLoadRarLibrary;
begin
  if h <> 0 then
  begin
    FreeLibrary(h);
    IsLoaded := false;
    h := 0;
    RAROpenArchive        := nil;
    RAROpenArchiveEx      := nil;
    RARCloseArchive       := nil;
    RARReadHeader         := nil;
    RARReadHeaderEx       := nil;
    RARProcessFile        := nil;
    RARSetCallback        := nil;
    RARSetChangeVolProc   := nil;
    RARSetProcessDataProc := nil;
    RARSetPassword        := nil;
    RARGetDllVersion      := nil;
  end;
end;

// returns true if UnRar.Dll is loaded
function IsRarLoaded: boolean;
begin
  Result := IsLoaded;
end;

end.
