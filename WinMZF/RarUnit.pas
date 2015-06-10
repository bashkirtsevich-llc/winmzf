unit RarUnit;

interface
uses  Windows, Classes, BTMemoryModule;

const
  ERAR_END_ARCHIVE    = 10;
  ERAR_NO_MEMORY      = 11;
  ERAR_BAD_DATA       = 12;
  ERAR_BAD_ARCHIVE    = 13;
  ERAR_UNKNOWN_FORMAT = 14;
  ERAR_EOPEN          = 15;
  ERAR_ECREATE        = 16;
  ERAR_ECLOSE         = 17;
  ERAR_EREAD          = 18;
  ERAR_EWRITE         = 19;
  ERAR_SMALL_BUF      = 20;

  RAR_OM_LIST         =  0;
  RAR_OM_EXTRACT      =  1;

  RAR_SKIP            =  0;
  RAR_TEST            =  1;
  RAR_EXTRACT         =  2;

  RAR_VOL_ASK         =  0;
  RAR_VOL_NOTIFY      =  1;

type
  RARHeaderData=record
    ArcName:array[1..260] of char;
    FileName:array[1..260]of char;

    Flags,
    PackSize,
    UnpSize,
    HostOS,
    FileCRC,
    FileTime,
    UnpVer,
    Method,
    FileAttr: cardinal;

    CmtBuf:PChar;
    CmtBufSize,
    CmtSize,

    CmtState: cardinal;
  end;



  RAROpenArchiveData=Record
    ArcName:PChar;
    OpenMode, OpenResult:cardinal;
    CmtBuf:PChar;
    CmtBufSize, CmtSize, CmtState:cardinal;
  end;

type
  TChangeVolProc=Function(ArcName:pchar; Mode:integer):integer; cdecl;
  TProcessDataProc=Function(Addr:PChar;  Size:integer):integer; cdecl;

  TRAROpenArchive=function(var ArchiveData:RAROpenArchiveData):THandle;stdcall;
  TRARCloseArchive=function(hArcData:THandle):integer;stdcall;
  TRARReadHeader=function(hArcData:THandle;  var  HeaderData:RARHeaderData):integer;stdcall;
  TRARProcessFile=function(hArcData:THandle; Operation:Integer; DestPath, DestName:PChar):integer;stdcall;
  TRARSetChangeVolProc=procedure(hArcData:THandle; CVP:TChangeVolProc);stdcall;
  TRARSetProcessDataProc=procedure(hArcData:THandle; PDP:TProcessDataProc);stdcall;
  TRARSetPassword=procedure(hArcData:THandle; Password:PChar);stdcall;

var mp_DllData: Pointer;
    m_DllDataSize: Integer;
    mp_MemoryModule: PBTMemoryModule;
    //Procedure's
    RAROpenArchive:TRAROpenArchive;
    RARCloseArchive:TRARCloseArchive;
    RARReadHeader:TRARReadHeader;
    RARProcessFile:TRARProcessFile;
    RARSetChangeVolProc:TRARSetChangeVolProc;
    RARSetProcessDataProc:TRARSetProcessDataProc;
    RARSetPassword:TRARSetPassword;

procedure LoadDll(hInst:Cardinal);
procedure FreeDll;

implementation

{$R Rar.res}
    
procedure LoadDll(hInst:Cardinal);
Var ResStream:TResourceStream;
Begin
  ResStream:=TResourceStream.Create(hInst,'UNRAR','DLL');
  ResStream.Position := 0;
  m_DllDataSize := ResStream.Size;
  mp_DllData := GetMemory(m_DllDataSize);
  ResStream.Read(mp_DllData^, m_DllDataSize);
  ResStream.Free;
  mp_MemoryModule := BTMemoryLoadLibary(mp_DllData, m_DllDataSize);
  try
    @RAROpenArchive:=BTMemoryGetProcAddress(mp_MemoryModule,'RAROpenArchive');
    @RARCloseArchive:=BTMemoryGetProcAddress(mp_MemoryModule,'RARCloseArchive');
    @RARReadHeader:=BTMemoryGetProcAddress(mp_MemoryModule,'RARReadHeader');
    @RARProcessFile:=BTMemoryGetProcAddress(mp_MemoryModule,'RARProcessFile');
    @RARSetChangeVolProc:=BTMemoryGetProcAddress(mp_MemoryModule,'RARSetChangeVolProc');
    @RARSetProcessDataProc:=BTMemoryGetProcAddress(mp_MemoryModule,'RARSetProcessDataProc');
    @RARSetPassword:=BTMemoryGetProcAddress(mp_MemoryModule,'RARSetPassword');
  finally
  end;
End;

procedure FreeDll;
begin
  if mp_MemoryModule <> nil then
    BTMemoryFreeLibrary(mp_MemoryModule);
  FreeMemory(mp_DllData);
end;

end.

