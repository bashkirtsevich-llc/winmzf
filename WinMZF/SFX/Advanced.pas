(*

  Copyright...2008...MADMAN
      ___           ___           ___           ___           ___           ___
     /\__\         /\  \         /\  \         /\__\         /\  \         /\__\
    /::|  |       /::\  \       /::\  \       /::|  |       /::\  \       /::|  |
   /:|:|  |      /:/\:\  \     /:/\:\  \     /:|:|  |      /:/\:\  \     /:|:|  |
  /:/|:|__|__   /::\~\:\  \   /:/  \:\__\   /:/|:|__|__   /::\~\:\  \   /:/|:|  |__
 /:/ |::::\__\ /:/\:\ \:\__\ /:/__/ \:|__| /:/ |::::\__\ /:/\:\ \:\__\ /:/ |:| /\__\
 \/__/~~/:/  / \/__\:\/:/  / \:\  \ /:/  / \/__/~~/:/  / \/__\:\/:/  / \/__|:|/:/  /
       /:/  /       \::/  /   \:\  /:/  /        /:/  /       \::/  /      |:/:/  /
      /:/  /        /:/  /     \:\/:/  /        /:/  /        /:/  /       |::/  /
     /:/  /        /:/  /       \::/__/        /:/  /        /:/  /        /:/  /
     \/__/         \/__/         ~~            \/__/         \/__/         \/__/
*)


unit Advanced;       

interface

uses Classes,ComCtrls,SysUtils,Windows,ShellApi,ShlObj,ClipBrd,
     ULZMAEncoder,ULZMADecoder,ULZMACommon,Messages_u,AES_256_u,
     MAD7_u,RC5,RC6,IDEA,SignatureDetect_u, SeletFolder_u, Forms,
     Controls, Registry, IniFiles;

//type TCompressLevel = (clNone, clFastest, clDefault, clMax);
//Type TEncodeMode=(emBeforeCompress,emAfterCompress,emBeforeAfter);
Type TReWriteMode=(omRewrite,omSkip,omOverwriteAll,omSkipAll,omRename,omUnknown);
Type TEncodeAlgorithm=(eaAES256,eaMAD7,eaIDEA{,eaRC5,eaRC6});

Const
  MaxLength=1024*10;//10 KBytes

Type
  TWideString=Array [1..MaxLength] of Byte;

Type TWorkingFile=Packed Record
    Archive:String;
    ArcType:TFileType;
    FilterPath:String;
    Opened:Boolean;
  End;

Type
  TSFXhead=packed record
    Signature:Array[0..2] Of Char;
    Vendor:Array[0..5] Of Char;
    ArchType:TFileType;
    WndCaption:ShortString;
    ExtractDir:ShortString;
  end;

Type TArcHead=Packed Record
    Siganture:Packed Array[0..2]Of Char;
    Version:Packed Array[0..2]Of Char;
    //Vendor:Array[0..11]of Char;
  End;

Type TArcConfig=Packed Record
    EncodeHead,EncodeFile:Boolean;
    //EncodeMode:TEncodeMode;
    UseComment:Boolean;
    EncodeAlgorithm:TEncodeAlgorithm;
  End;

Type TFileHead=Packed Record
    Attr:Word;
    CRC:LongWord;
    Size:LongWord;
    PackedSize:LongWord;
    FileCreateDate:TDateTime;
    FileModifyDate:TDateTime;
    FileOpenDate:TDateTime;
    {DirCreateDate:TDateTime;
    DirModifyDate:TDateTime;
    DirAccessDate:TDateTime;}
    {FileName:ShortString;
    Path:ShortString;}
    LongFileName:String;
  End;
  
type
  TSystemPath = (spDesktop, spStartMenu,
    spPrograms, spStartup, spPersonal, spAppData,
    spFonts, spSendTo, spRecent, spFavorites, spCache,
    spCookies, spHistory, spNetHood, spPrintHood,
    spTemplates, spLocADat, spWindRoot, spWindSys,
    spTempPath, spRootDir, spProgFiles, spComFiles,
    spConfigPath, spDevicePath, spMediaPath, spWallPaper);

{Type TSearchCriteria=Packed Record
    FileName:String;
    Path:String;
    Size:LongWord;
    PackedSize:LongWord;
    Arrt:Word;
    FileCreateDate:TDateTime;
    FileModifyDate:TDateTime;
    FileOpenDate:TDateTime;
    UseFileName:Boolean;
    UsePath:Boolean;
    UseSize:Boolean;
    UsePackedSize:Boolean;
    UseAttr:Boolean;
    UseDate:Boolean;
  End; }

Const
  Signature='MZF';
  Vendor='M.A.D.M.A.N.';
  Version='2.4';
  Sign='MSi';
  VID='MADMAN';

  TimeFormat='mm-dd-yyyy hh:mm:ss';

  RootCaption='Root-[%s]';
  Title='WinMZF - [%s]';
  EmptyTitle='WinMZF';
  function ExtractArchiveNameExtended(FileName:string):string;
  //procedure Encode(InStream,OutStream:TStream;Pass:String);
  Procedure FillHead(Var Head:TArcHead);
  Function CheckHead(Var Head:TArcHead):Boolean;
  Function GetPercentDone(FMinValue,FCurValue,FMaxValue:LongWord): Byte;
  Procedure MakeDir(Path:string);
  Function GetPathToNode(Node: TTreeNode; Separator: Char): string;
  //function GetFileDateTime(FileName: string): TDateTime;
  Function TextInList(Text:String;List:TStringList):Boolean;
  Function GetDominateDir(FileList:TStringList):string;
  Function SubStractString(S1,S2:String):String;
  Function CharInString(C:Char;Text:String):Boolean;
  //function ExtractEndDir(Dir:String):String;
  //procedure FillArray(var Buffer:Array Of ShortString;var Count:Byte;S:ShortString);
  Procedure FillTreeViewWithDirs(TreeView: TTreeView; Strs: TStringList);
  //function GetFileDate(TheFileName: string): TDateTime;
  //Function GetCreationTime(FileName:String):TDateTime;
  //////////////////////////////////
  Function FileCreationTime(const FileName: string): Integer;
  Function FileAccessTime(const FileName: string): Integer;
  Function FileModifyTime(const FileName: string): Integer;
  Function SetFileDateTime(const FileName:string;CreationTime,
                              LastWriteTime,LastAccessTime:TDateTime;
                              UpdateModifyTime,UpdateAccessTime:Boolean):boolean;
  Function GetAttributesAsString(Attr:Word):String;
  Function ExpandStream(inStream, outStream :TStream):boolean;
  Function GetLastDirName(Dir:String):String;
  Function GetLastDir(Dir:String):String;
  Procedure ShellOpenFile(Handle:hWnd;FileName:String);
  Procedure ShellExecuteFile(Handle:hWnd;FileName:String;WaitForClose:Boolean);
  //
  Procedure SetPriority(ProcessPriority,ThreadPriority:Integer);
  Procedure CopyFilesToClipboard(FileList:string);
  {Procedure PasteFilesFromClipboard(PathTo: KOLString);}
  procedure WriteWideString(Stream:TStream;AValue: String);
  function ReadWideString(Stream:TStream): String;
  procedure CompressStream(InStream,OutStream:TStream);
  // Encode and decode
  Procedure EncodeStream(InStream,OutStream:TStream;Password:String;Algorithm:TEncodeAlgorithm);
  Procedure DecodeStream(InStream,OutStream:TStream;Password:String;Algorithm:TEncodeAlgorithm);
  //
  function GetDictBySize(Size:LongWord):Byte;
  function GetTempDir:String;
  function GetTempDirName:String;
  procedure RemoveFileTree(const Path: string);
  function GetSelectedDir:String;
  function GetImgIndexByExt(Extention:String):Byte;
  function FileMaskEquate(F, M: string): boolean;
  procedure SearchDir(Dir,Ext: string;Var List:TStringList);
  //Functions for read options
  function GetStringOptions(_Section,_Ident,_Default:String):String;
  function GetIntegerOptions(_Section,_Ident:String;_Default:Integer):Integer;
  function GetBoolOptions(_Section,_Ident:String;_Default:Boolean):Boolean;
  procedure SetStringOptions(_Section,_Ident,_Value:String);
  procedure SetIntegerOptions(_Section,_Ident:String;_Value:Integer);
  procedure SetBoolOptions(_Section,_Ident:String;_Value:Boolean);
  //End :-)
  function GetSystemPath(SystemPath: TSystemPath): string;
  function NameInMask(Mask,Ext:String):Boolean;
  procedure FillListWithMask(Var List:TStringList;Mask:String);
  //
  procedure CreateSFX(Source,Dest,Caption,ExtractDir,SFXModule:String);
  function ReadFromLanguage(_Section,_Ident,_Value:String):String;
  procedure RegisterApplication;
  procedure UnregisterApplication;

implementation

procedure RegisterApplication;
begin
  With TRegistry.Create Do
    Try
       RootKey := HKEY_CLASSES_ROOT;

       OpenKey('.mzf', True);
       WriteString('', 'WinMZF Archive');

       OpenKey('\WinMZF Archive', True);
       WriteString('', {'WinMZF Archive'}ReadFromLanguage('Association','Type','WinMZF Archive'));

       OpenKey('DefaultIcon', True);
       WriteString('', application.ExeName +',1');
       CloseKey();

       OpenKey('\WinMZF Archive' + '\Shell\Open\Command', True);
       WriteString('', '"'+application.exename+'" "%1"');
       
       OpenKey('\WinMZF Archive' + '\Shell\Open', True);
       WriteString('', ReadFromLanguage('Association','OpenWith','Open with WinMZF'));
       CloseKey();
    Finally
      Free();
    End;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, Nil, Nil);
end;

procedure UnregisterApplication;
   Procedure RegDeleteKey(RKey: HKey; KeyPath: String);
   Begin
      With TRegistry.Create Do
      Try
         RootKey := RKey;
         // Under Win95, all subkeys are automatically deleted.
         // Under WinNT, the subkeys will be left alone.
         DeleteKey(KeyPath);
      Finally
         Free();
      End;
   End;

   Procedure RemoveAssociation(Ext: String);
   Begin
      RegDeleteKey(HKEY_CLASSES_ROOT, '.' + Ext);
      RegDeleteKey(HKEY_CLASSES_ROOT, Ext + '_auto_file\shell\open\command');
      RegDeleteKey(HKEY_CLASSES_ROOT, Ext + '_auto_file');
   End;
begin
    With TRegistry.Create Do
      Try
        RemoveAssociation('.mzf');
        RootKey := HKEY_CLASSES_ROOT;
        DeleteKey('.mzf');
        DeleteKey('\' + 'WinMZF Archive');
      Finally
         Free();
      end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, Nil, Nil);
end;

procedure CreateSFX(Source,Dest,Caption,ExtractDir,SFXModule:String);
var ModuleStream:TFileStream;
    ArchiveStream:TFileStream;
    SFXStream:TMemoryStream;
    SFXHead:TSFXhead;
begin
  If Not FileExists(Source) Then Exit;
  SFXHead.Signature:=Sign;
  SFXHead.Vendor:=VID;
  SFXHead.ArchType:=GetArchiveTypeBySignature(Source);
  SFXHead.WndCaption:=Caption;
  SFXHead.ExtractDir:=ExtractDir;
  SFXStream:=TMemoryStream.Create;
  ModuleStream:=TFileStream.Create(SFXModule,fmOpenRead);
  SFXStream.CopyFrom(ModuleStream,ModuleStream.Size);
  ModuleStream.Free;
  SFXStream.Write(SFXHead,SizeOf(TSFXHead));
  ArchiveStream:=TFileStream.Create(Source,fmOpenRead);
  SFXStream.CopyFrom(ArchiveStream,ArchiveStream.Size);
  ArchiveStream.Free;
  SFXStream.SaveToFile(Dest);
  SFXStream.Free;
End;

procedure FillListWithMask(Var List:TStringList;Mask:String);
var _Pos:Word;
begin
  if Mask[Length(Mask)]<>';' then Mask:=Mask+';';
  //delete(s,1,1);
  while Pos(';',Mask)<>0 do
    begin
      _Pos:=pos(';',Mask);
      List.Add(Copy(Mask,1,_Pos-1));
      Delete(Mask,1,_Pos);
    end;
end;

function FileMaskEquate(F, M: string): boolean;
var
  Fl, Ml: integer; // length of file name and mask
  Fp, Mp: integer; // pointers
begin
  F := UpperCase(F);
  M := UpperCase(M);
  result := true;
  Fl := length(F);
  Ml := length(M);
  Fp := 1;
  Mp := 1;
  while Mp <= Ml do
  begin // wildcard
    case M[Mp] of //
      '?':
        begin // if one any char
          inc(Mp); // next char of mask
          inc(Fp); // next char of file name
        end; //
      '*':
        begin // if any chars
          if Mp = Ml then
            exit; // if last char in mask then exit
          if M[Mp + 1] = F[Fp] then
          begin // if next char in mask equate char in
            Inc(Mp); // file name then next char in mask and
          end
          else
          begin // else
            if Fp = Fl then
            begin // if last char in file name then
              result := false; // function return false
              exit; //
            end; // else, if not previous, then
            inc(Fp); // next char in file name
          end; //
        end; //
    else
      begin // other char in mask
        if M[Mp] <> F[Fp] then
        begin // if char in mask not equate char in
          result := false; // file name then function return
          exit; // false
        end; // else
        if (Mp=Ml) and (Fp<>Fl) then begin
        Result:=false;
        exit;
       end;
        inc(Fp); // next char of mask
        inc(Mp); // next char of file name
      end //
    end;
  end;
end;

procedure SearchDir(Dir,Ext: string;Var List:TStringList);
var
  SR: TSearchRec;
  FindRes: Integer;
begin
  FindRes := FindFirst(Dir + '*.*', faAnyFile, SR);
       while FindRes = 0 do
          begin
            if ((SR.Attr and faDirectory) = faDirectory) and
            ((SR.Name = '.') or (SR.Name = '..')) then
               begin
                 FindRes := FindNext(SR);
                 Continue;
               end;
            if ((SR.Attr and faDirectory) = faDirectory) then
              begin
                SearchDir(Dir + SR.Name + '\',Ext,List);
                FindRes := FindNext(SR);
                Continue;
              end;
            //Files.Add(Dir + SR.Name);//Add to list
          if FileMaskEquate(SR.Name, Ext) = true then begin
             List.Add(Dir+SR.Name);
            end;
            //showmessage(dir+sr.Name);
            FindRes := FindNext(SR);
          end;
  Windows.FindClose(FindRes);
end;

function ExtractArchiveNameExtended(FileName:string):string;
var
    L:Word;
    index:Word;
    temp:string;
begin
 L:=Length(FileName);
  for index := L downto 1 do
    if FileName[index]='\' then
      begin
        temp:=Copy(FileName,0,Index-1);
        if not FileExists(Temp) then continue;
          Result:=Temp;
          Exit;
        end;
end;

function NameInMask(Mask,Ext:String):Boolean;
var Index:Word;
    Temp:String;
begin
  If Mask[Length(Mask)]<>';' Then Mask:=Mask+';';
  Index:=1;
  While Pos(';',Mask)<>0 Do
    Begin
      Temp:=Copy(Mask,1,Pos(';',Mask)-1);
      Delete(Mask,1,Pos(';',Mask));
      If Not FileMaskEquate(ext,temp) Then Continue;
      Result:=True;
      Exit;
    End;
  Result:=False;
end;

function GetImgIndexByExt(Extention:String):Byte;
Begin
  Extention:=UpperCase(Extention);
  Result:=4;
  If Extention='.MZF' Then Result:=0;
  If Extention='.7Z' Then Result:=1;
  If Extention='.RAR' Then Result:=2;
  If Extention='.ZIP' Then Result:=3;
  If Extention='.BH' Then Result:=3;
  If Extention='.ZOO' Then Result:=3;
  If Extention='.GZ' Then Result:=3;
  If Extention='.LHA' Then Result:=3;
  If Extention='.CAB' Then Result:=3;
  If Extention='.ARJ' Then Result:=3;
  If Extention='.PKG5' Then Result:=3;
  If Extention='.ACE' Then Result:=3;
End;

function GetTempDir:String;
var a: Array[0..1024] Of Char;
    s:string;
    _Dir:String;
begin //
  Windows.GetTempPath(1024,a);
  _Dir:=StrPas(a)+'MZF';
  //Result:=_Dir;
  If _Dir=GetStringOptions('Config','TempDir',_Dir) Then
    Result:=_Dir
  Else
    Result:=GetStringOptions('Config','TempDir',_Dir);
end;

function GetTempDirName:String;
var a: Array[0..1024] Of Char;
begin
  Windows.GetTempFilename(PChar(GetTempDir),'MZF',FILE_ATTRIBUTE_DIRECTORY,a);
  Result:=StrPas(a);
end;

function GetSystemPath(SystemPath: TSystemPath): string;
var
  ph: PChar;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKey('\Software\Microsoft\Windows\CurrentVersion\' +
        'Explorer\Shell Folders', True);
      case SystemPath of
        spDesktop: Result   := ReadString('Desktop');
        spStartMenu: Result := ReadString('Start Menu');
        spPrograms: Result  := ReadString('Programs');
        spStartup: Result   := ReadString('Startup');
        spPersonal: Result  := ReadString('Personal');
        spAppData: Result   := ReadString('AppData');
        spFonts: Result     := ReadString('Fonts');
        spSendTo: Result    := ReadString('SendTo');
        spRecent: Result    := ReadString('Recent');
        spFavorites: Result := ReadString('Favorites');
        spCache: Result     := ReadString('Cache');
        spCookies: Result   := ReadString('Cookies');
        spHistory: Result   := ReadString('History');
        spNetHood: Result   := ReadString('NetHood');
        spPrintHood: Result := ReadString('PrintHood');
        spTemplates: Result := ReadString('Templates');
        spLocADat: Result   := ReadString('Local AppData');
        spWindRoot:
          begin
            GetMem(ph, 255);
            GetWindowsDirectory(ph, 254);
            Result := Strpas(ph);
            Freemem(ph);
          end;
        spWindSys:
          begin
            GetMem(ph, 255);
            GetSystemDirectory(ph, 254);
            Result := Strpas(ph);
            Freemem(ph);
          end;
        spTempPath:
          begin
            GetMem(ph, 255);
            GetTempPath(254, ph);
            Result := Strpas(ph);
            Freemem(ph);
          end;
        spRootDir:
          begin
            GetMem(ph, 255);
            GetSystemDirectory(ph, 254);
            Result := (Copy(Strpas(ph), 1, 2));
            Freemem(ph);
          end;
      end;
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion', True);
      case SystemPath of
        spProgFiles: Result := ReadString('ProgramFilesDir');
        spComFiles: Result := ReadString('CommonFilesDir');
        spConfigPath: Result := ReadString('ConfigPath');
        spDevicePath: Result := ReadString('DevicePath');
        spMediaPath: Result := ReadString('MediaPath');
        spWallPaper: Result := ReadString('WallPaperDir');
      end;
    finally
      CloseKey;
      Free;
    end;
{  if (Result <> '') and (Result[Length(Result)] <> '\') then
    Result := Result + '\';}
end;

function GetSelectedDir:String;
begin
  Application.CreateForm(TdlgSelectFolder,dlgSelectFolder);
  If dlgSelectFolder.ShowModal<>mrOk Then
    Begin
      Result:='';
      dlgSelectFolder.Free;
      Exit;
    End;
  Result:=dlgSelectFolder.edDestFolder.Text;
  dlgSelectFolder.Free;
end;

procedure RemoveFileTree(const Path: string);
var Found: integer;
    SearchRec: TSearchRec;
    FileName: string;
begin
  Found:= FindFirst(Path + '\*.*', faAnyFile, SearchRec);
  while Found = 0 do
  begin
    if ((SearchRec.Attr and faDirectory) = faDirectory)
    then
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
        then RemoveFileTree(Path+'\'+SearchRec.Name)
      else
    else
    begin
      FileName:= Path+'\'+SearchRec.Name+#0;
      DeleteFile(PChar(FileName));
    end;
  Found:= FindNext(SearchRec);
  end;
  SysUtils.FindClose(SearchRec);
  RemoveDir(Path);
end;

function GetDictBySize(Size:LongWord):Byte;
Var i:Byte;
Begin
  For i:=0 To 28 Do
    If Not (Size <=(1 shl i)) Then
      Begin
        Result:=i;
        exit;
      End;
  Result:=0;
End;

function StringToWidestring(Source:String;var Dest:TWideString):Integer;
var Index:Integer;
    Max:Integer;
begin
  Result:=0;
  Max:=Length(Source);
  for Index := 1 to Max do
    Dest[Index]:=Byte(Source[Index]);
  Result:=Max;
end;

function WidestringToString(Source:TWideString;Length:Integer):String;
var Index:Integer;
    Temp:String;
begin
  Temp:='';
  for Index := 1 to Length do
    Temp:=Temp+Char(Source[Index]);
  Result:=Temp;
end;

procedure WriteWideString(Stream:TStream;AValue: String);
var LLen:LongWord;
    Index:Integer;
    a:TWideString;
begin
  LLen:=Length(AValue);
  Stream.Write(LLen, SizeOf(LLen));
  For Index:=1 To LLen Do
    a[Index]:=Byte(AValue[Index]);
  if LLen > 0 then
    Stream.WriteBuffer(a,LLen);
end;

function ReadWideString(Stream:TStream): String;
var
  LLen: LongWord;
  Index:Integer;
  a:TWideString;
begin
  Result:='';
  if Stream.Read(LLen, SizeOf(LLen)) <> SizeOf(LLen) then
    raise Exception.Create(rsInvalidStringLength);
  Stream.ReadBuffer(a,LLen);
  for Index := 1 to LLen do
    Result:=Result+Char(a[Index]);    
  //Result:=WidestringToString(a,LLen);
end;

Procedure EncodeStream(InStream,OutStream:TStream;Password:String;Algorithm:TEncodeAlgorithm);
Begin
  Case Algorithm Of
    eaAES256:AES256_EncodeStream(InStream,OutStream,Password);
    eaMAD7:MAD7_EncodeStream(InStream,OutStream,Password);
    eaIDEA:IDEA_EncryptCopy(OutStream,InStream,InStream.Size,Password);
    //eaRC5:RC5_EncryptCopy(OutStream,InStream,InStream.Size,Password);
    //eaRC6:RC6_EncryptCopy(OutStream,InStream,InStream.Size,Password);
  End;
End;

Procedure DecodeStream(InStream,OutStream:TStream;Password:String;Algorithm:TEncodeAlgorithm);
Begin
  Case Algorithm Of
    eaAES256:AES256_DecodeStream(InStream,OutStream,Password);
    eaMAD7:MAD7_DecodeStream(InStream,OutStream,Password);
    eaIDEA:IDEA_DecryptCopy(OutStream,InStream,InStream.Size,Password);
    //eaRC5:RC5_DecryptCopy(OutStream,InStream,InStream.Size,Password);
    //eaRC6:RC6_EncryptCopy(OutStream,InStream,InStream.Size,Password);
  End;
End;

{Procedure PasteFilesFromClipboard(PathTo: KOLString);
Var
    DropFiles: PDropFiles;
    hGlobal: THandle;
    i, Count : Integer;
    PathFrom : PChar;
Begin
 if OpenClipboard(0) then
  begin
   hGlobal := GetClipboardData(CF_HDROP);
   DropFiles := GlobalLock(hGlobal);
   if DropFiles <> nil then
    begin
     Count := DragQueryFile(hGlobal, DWORD(-1), nil, 0);
     PathFrom := CoTaskMemAlloc(MAX_PATH);
     for i := 0 to Count - 1 do
      begin
       DragQueryFile(hGlobal, i, PathFrom, MAX_PATH);
       GlobalUnlock(hGlobal);
       uShellCopy(Form.Handle, PathFrom, PathTo);
      end; 
     CloseClipboard;
    end;
  end;
End; }

Procedure CopyFilesToClipboard(FileList:string);
Var
  DropFiles:PDropFiles;
  hGlobal:THandle;
  iLen:Integer;
Begin
  iLen:=Length(FileList)+2;
  FileList:=FileList+#0#0;
  hGlobal:=GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
    SizeOf(TDropFiles)+iLen);
  If (hGlobal= 0) then
    Raise Exception.Create('Could not allocate memory.');
  Begin
    DropFiles:=GlobalLock(hGlobal);
    DropFiles^.pFiles:=SizeOf(TDropFiles);
    Move(FileList[1],(PChar(DropFiles)+SizeOf(TDropFiles))^,iLen);
    GlobalUnlock(hGlobal);
    Clipboard.SetAsHandle(CF_HDROP,hGlobal);
  End;
End;

Procedure SetPriority(ProcessPriority,ThreadPriority:Integer);
Var
  ProcessID : DWORD;
  ProcessHandle : THandle;
  ThreadHandle : THandle;
Begin
  ProcessID := GetCurrentProcessID;
  ProcessHandle := OpenProcess(PROCESS_SET_INFORMATION,
  false, ProcessID);
  SetPriorityClass(ProcessHandle, ProcessPriority);
  ThreadHandle := GetCurrentThread;
  SetThreadPriority(ThreadHandle, ThreadPriority);
End;

Procedure ShellOpenFile(Handle:hWnd;FileName:String);
Var Info: TShellExecuteInfo;
Begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(TShellExecuteInfo);
  With Info Do
    Begin
      fMask := SEE_MASK_NOCLOSEPROCESS;
      Wnd := Handle;
      lpFile := PChar(FileName);
      lpDirectory := PChar(extractfiledir(FileName));
      nShow:=SW_SHOW;
    End;
  If Not ShellExecuteEx(@Info) Then
    MessageBox(Handle,PChar(ReadFromLanguage('Messages','CoudNotExec','Coud not execute file')),'WinMZF',MB_OK+MB_ICONSTOP);
End;

Procedure ShellExecuteFile(Handle:hWnd;FileName:String;WaitForClose:Boolean);
Var Info: TShellExecuteInfo;
    ExitCode: DWord;
Begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(TShellExecuteInfo);
  With Info Do
    Begin
      fMask := SEE_MASK_NOCLOSEPROCESS;
      Wnd := Handle;
      lpFile := PChar(FileName);
      lpDirectory := PChar(extractfiledir(FileName));
      nShow:=SW_SHOW;
    End;
  If ShellExecuteEx(@Info) Then
    If WaitForClose Then
      Repeat
        Application.ProcessMessages;
        GetExitCodeProcess(Info.hProcess, ExitCode);
        Sleep(100);
      Until (ExitCode <> STILL_ACTIVE) Or Application.Terminated;
End;

Function GetLastDir(Dir:String):String;
Var Index:Word;
Begin
  If Length(Dir)<1 Then Exit;
  If Dir[Length(Dir)]='\' Then Delete(Dir,Length(Dir)-1,1);
  For Index:= Length(Dir) DownTo 1 Do
    Begin
      If Not (Dir[Index]='\') Then Continue;
      Result:=Copy(Dir,1,Index-1);
      Exit;
    End;
  Result:='';
End;

Function GetLastDirName(Dir:String):String;
Var Index:Word;
Begin
  If Dir[Length(Dir)]='\' Then Delete(Dir,Length(Dir)-1,1);
  For Index:= Length(Dir) DownTo 1 Do
    Begin
      If Dir[Index]='\' Then
        Begin
          Result:=Copy(Dir,Index+1,Length(Dir));
          Exit;
        End;
      If Dir[Index]=':' Then
        Begin
          Result:=Copy(Dir,1,2);
          Exit;
        End;
    End;
  Result:='';
End;

procedure CompressStream(InStream,OutStream:TStream);
Var encoder:TLZMAEncoder;
    i:Byte;
    s:byte;
Begin
  InStream.Position:=0;
  OutStream.Position:=0;

  encoder:=TLZMAEncoder.Create;
  encoder.SetAlgorithm(1);  //1
  encoder.SetDictionarySize(0);//0
  encoder.SeNumFastBytes(273);    //273
  encoder.SetLcLpPb(0,0,0);       //0,0,0
  encoder.SetMatchFinder(2);      //0
  s:=inStream.Size;
  OutStream.Write(s,SizeOf(byte));
  {for i := 0 to 7 do
    WriteByte(outStream,(InStream.Size shr (8 * i)) and $FF);}
  encoder.Code(InStream,OutStream,-1,-1);

  encoder.Free;
  
  InStream.Position:=0;
  OutStream.Position:=0;
End;

function ExpandStream(inStream, outStream :TStream):boolean;
var Count: integer;
    decoder:TLZMADecoder;
    OutSize:LongWord;
    i,v:Byte;
const
    properties:array[0..4] of byte=
    ($00,$00,$00,$40,$00);
begin
  inStream.Seek(0,soFromBeginning);
  outStream.Seek(0,soFromBeginning);

  decoder:=TLZMADecoder.Create;
  //InStream.Read(Properties,5);
  If Not decoder.SetDecoderProperties(Properties) Then
    Begin
      Result:=False;
      Exit;
      decoder.Free;
    End;
  outSize := 0;
  InStream.Read(outSize,SizeOf(byte));
  {for i := 0 to 7 do
    begin
      v := (ReadByte(inStream));
      if v < 0 then
        Begin
          Result:=False;
          decoder.Free;
          Exit;
        End;
      outSize := outSize or v shl (8 * i);
    end; }
  decoder.Code(inStream, outStream, OutSize);
  decoder.Free;

  InStream.Position:=0;
  OutStream.Position:=0;
  Result:=True;
end;

Function SetFileDateTime(const FileName:string;CreationTime,
                              LastWriteTime,LastAccessTime:TDateTime;
                              UpdateModifyTime,UpdateAccessTime:Boolean):boolean;
// be careful! LastAccessTime may contain time part, but it won't be written!
var Z:dword;
    P:PChar;
    CT,LAT,LWT:TFileTime;
    ST:TSystemTime;
begin
  If UpdateModifyTime Then LastWriteTime:=Date+Time;
  If UpdateAccessTime Then LastAccessTime:=Date;
  SetLastError(0);
  GetMem(P,1024);
  StrPLCopy(P,FileName,1024);
  Result:=false;
  Z:=CreateFile(P,GENERIC_WRITE,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_FLAG_SEQUENTIAL_SCAN,0);
  if Z<>INVALID_HANDLE_VALUE then
    begin
      DateTimeToSystemTime(CreationTime,ST);
      SystemTimeToFileTime(ST,CT);
      LocalFileTimeToFileTime(CT,CT);
      DateTimeToSystemTime(LastAccessTime,ST);
      SystemTimeToFileTime(ST,LAT);
      LocalFileTimeToFileTime(LAT,LAT);
      DateTimeToSystemTime(LastWriteTime,ST);
      SystemTimeToFileTime(ST,LWT);
      LocalFileTimeToFileTime(LWT,LWT);
      Result:=SetFileTime(Z,@CT,@LAT,@LWT);
      CloseHandle(Z);
    end;
end;

function GetAttributesAsString(Attr:Word):String;
Begin
  Result:='';
  If (Attr and FILE_ATTRIBUTE_READONLY) <> 0 Then Result:=Result+'R';
  If (Attr and FILE_ATTRIBUTE_HIDDEN) <> 0 Then Result:=Result+'H';
  If (Attr and FILE_ATTRIBUTE_SYSTEM) <> 0 Then Result:=Result+'S';
  If (Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0 Then Result:=Result+'D';
  If (Attr and FILE_ATTRIBUTE_ARCHIVE) <> 0 Then Result:=Result+'A';
  If (Attr and FILE_ATTRIBUTE_NORMAL) <> 0 Then Result:=Result+'N';
  If (Attr and FILE_ATTRIBUTE_TEMPORARY) <> 0 Then Result:=Result+'T';
  If (Attr and FILE_ATTRIBUTE_COMPRESSED) <> 0 Then Result:=Result+'C';
  If (Attr and FILE_ATTRIBUTE_OFFLINE) <> 0 Then Result:=Result+'O';
End;

function FileCreationTime(const FileName: string): Integer;
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    {if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin }
      FileTimeToLocalFileTime(FindData.ftCreationTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    {end;}
  end;
  Result := -1;
end;

function FileAccessTime(const FileName: string): Integer;
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    {if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin}
      FileTimeToLocalFileTime(FindData.ftLastAccessTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    {end;}
  end;
  Result := -1;
end;

function FileModifyTime(const FileName: string): Integer;
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    {if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin}
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then Exit;
    {end;}
  end;
  Result := -1;
end;

{Function GetCreationTime(FileName:String):TDateTime;
Var File_Rec: TSearchRec;
Begin
  FindFirst(FileName, 1, File_Rec);
  Result:=FileDateToDateTime(File_Rec.Time);
End;}

procedure FillTreeViewWithDirs(TreeView: TTreeView; Strs: TStringList);
var
CachedStrs: TStringList; // CachedStrs вводится для ускорения поиска
 // в уже готовом дереве.

procedure AddItem(Lev: Integer; ParentNode: TTreeNode; S: string);
  function FindNodeWithText(AParent: TTreeNode; const S: string): TTreeNode;
  var
    K: Integer;
    fStr: string;
    tmpNode: TTreeNode;
  begin
    Result := nil;
    fStr := S + IntToStr(Integer(AParent));
    K := CachedStrs.IndexOf(fStr);
    if K > -1 then
      Result := Pointer(CachedStrs.Objects[K])
    else
    begin
      if AParent <> nil then
        tmpNode := AParent.getFirstChild
    else
    tmpNode := TreeView.Items.GetFirstNode;
    while tmpNode <> nil do
      begin
        if tmpNode.Text = S then
          begin
            Result := tmpNode;
            CachedStrs.AddObject(fStr, Pointer(tmpNode));
            break;
          end;
        tmpNode := tmpNode.getNextSibling;
      end;
    end
  end;

var
  prefix: string;
  ID: Integer;
  aNode: TTreeNode;
begin
  if S = '' then
    Exit;
  ID := Pos('\', S);
  prefix := '';
  if ID > 0 then
    prefix := Copy(S, 1, ID - 1)
  else
    begin
      prefix := S;
      S := '';
  end;

  aNode := FindNodeWithText(ParentNode, prefix);

  if aNode = nil then
    begin
      aNode := TreeView.Items.AddChild(ParentNode, prefix);
      aNode.ImageIndex:=0;
      aNode.SelectedIndex:=1;
    end;

  AddItem(Lev + 1, aNode, Copy(S, ID + 1, Length(S)));

end;

var
  K: Integer;
begin
  CachedStrs := TStringList.Create;
  CachedStrs.Duplicates := dupIgnore;
  CachedStrs.Sorted := True;
  try
    TreeView.Items.BeginUpdate;
    TreeView.SortType := stNone;
    for K := 0 to Strs.Count - 1 do
      AddItem(0, nil, Strs[K]);
  finally
    TreeView.Items.EndUpdate;
    CachedStrs.Free;
  end;
end;

{procedure FillArray(var Buffer:Array Of ShortString;var Count:Byte;S:ShortString);
var Index:Byte;
    SlashPos:Byte;
begin
  if S[Length(S)]<>'\' then S:=S+'\';
  delete(s,1,1);
  Index:=0;
  Count:=0;
  while Pos('\',S)<>0 do
    begin
      SlashPos:=pos('\',S);
      Buffer[index]:=Copy(S,1,SlashPos-1);
      Delete(S,1,SlashPos);
      Inc(Index);
      Inc(Count);
    end;
end;   }

{function ExtractEndDir(Dir:String):String;
var Index:Byte;
Begin
  For Index:=Length(Dir) DownTo 1 Do
    If Dir[Index]='\' Then
      Begin
        Result:=Copy(Dir,Index+1,Length(Dir));
        Exit;
      End;
End;}

function CharInString(C:Char;Text:String):Boolean;
Var Index:Byte;
Begin
  For Index:=1 To Length(Text) Do
    If Text[Index]=C Then
      Begin
        Result:=True;
        Exit;
      End;
  Result:=False;
end;

{procedure Encode(InStream,OutStream:TStream;Pass:String); //Парит мозги
var Data:Byte;
    PassPos:Byte;
Begin
  PassPos:=1;
  InStream.Position:=0;
  OutStream.Position:=0;
  (*OutStream.CopyFrom(InStream,InStream.Size);*)
  While InStream.Position<>InStream.Size Do
    Begin
      InStream.Read(Data,SizeOf(Byte));
      Data:=Data Xor (Byte(Pass[PassPos]) Xor PassPos-255);
      If PassPos>=Length(Pass) Then
        PassPos:=1;
      OutStream.Write(Data,SizeOf(Byte));
    End;
  InStream.Position:=0;
  OutStream.Position:=0;
End; }

procedure FillHead(Var Head:TArcHead);
Begin
  Head.Siganture:=Signature;
  //Head.Vendor:=Vendor;
  Head.Version:=Version;
End;

function CheckHead(Var Head:TArcHead):Boolean;
begin
  Result:=(Head.Siganture=Signature)And
          (Head.Version=Version){And
          (Head.Vendor=Vendor)};
end;

Procedure MakeDir(Path:string);
var i,x:integer;
    cur_dir:string;
    RootDir:String;
begin
  RootDir:=Path[1]+Path[2]+Path[3];
  SetCurrentDirectory(pchar(RootDir));
  x:=1;
  cur_dir:='';
  if (Path[1]='\') then x:=2;
  for i:=x to Length(Path) do
   begin
    if not (Path[i]='\')then
      cur_dir:=cur_dir+Path[i];
    if (Path[i]='\')or (i=length(Path)) then
     begin
      if not DirectoryExists(cur_dir) then
        CreateDirectory(pchar(cur_dir),0);
      SetCurrentDirectory(pchar(cur_dir));
      cur_dir:='';
     end;
   end;
end;

function GetPathToNode(Node: TTreeNode; Separator: Char): string;
begin
  Result := '';
  if Node = nil then
    exit;
  while Node <> nil do
  begin
    Result := Node.Text + Separator + Result;
    Node := Node.Parent;
  end;
  Delete(Result, length(Result), 1);
end;

function SubStractString(S1,S2:String):String;
var Index:Word;
begin
  For Index:=1 To Length(S1) do
    if UpperCase(S1[Index])<>UpperCase(S2[Index]) then
      begin
        Result:=Copy(S1,Index,Length(S1));
        Exit;
      end;
end;

{function GetFileDate(TheFileName: string): TDateTime;
var
  FHandle: integer;
begin
  FHandle := FileOpen(TheFileName, 0);
  result := FileDateToDateTime(FileGetDate(FHandle));
  FileClose(FHandle);
end;}

{function GetFileDateTime(FileName: string): TDateTime;
var intFileAge: LongInt; 
begin 
  intFileAge := FileAge(FileName); 
  if intFileAge = -1 then 
    Result := 0 
  else 
    Result := FileDateToDateTime(intFileAge) 
end;}

function TextInList(Text:String;List:TStringList):Boolean;
var Index:Integer;
begin
  If List.Count=0 Then
    Begin
      Result:=False;
      Exit;
    End;
  For Index:=0 to List.Count-1 do
    If UpperCase(Text)=UpperCase(List.Strings[Index]) Then
      Begin
        Result:=True;
        Exit;
      End Else Result:=False;
end;

Function GetDominateDir(FileList:TStringList):string;
var i:integer;
    l,aaa:integer;
    HalfResult:string;
    TempList:TStringList;
begin
  TempList:=TStringList.Create;
  aaa:=High(Integer);
  For i:=FileList.Count-1 DownTo 0 do
    Begin
      TempList.Add(ExtractFileDir(FileList.Strings[i]));
      If Length(FileList.Strings[i])<aaa Then
        aaa:=Length(FileList.Strings[i]);
    End;
  //aaa:=length(TempList.Strings[0]);
  for i:=0 to TempList.Count-1 do
  begin
    l:=length(TempList.Strings[i]);
    if (l<=aaa) then
    begin
      aaa:=l;
      HalfResult:=TempList.Strings[i];
    end;
  end;
  Result:=HalfResult;
  TempList.Free;
end;

function SolveForY(X, Z: LongWord): Byte;
begin
  if Z = 0 then Result := 0
  else Result := Byte(Trunc( (X * 100.0) / Z ));
end;

function GetPercentDone(FMinValue,FCurValue,FMaxValue:LongWord): Byte;
begin
  Result := SolveForY(FCurValue - FMinValue, FMaxValue - FMinValue);
end;

// Start Of Functions
function GetStringOptions(_Section,_Ident,_Default:String):String;
Var RegIni:TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  Result:=RegIni.ReadString(_Section,_Ident,_Default);
  RegIni.Free;
End;

function GetIntegerOptions(_Section,_Ident:String;_Default:Integer):Integer;
Var RegIni:TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  Result:=RegIni.ReadInteger(_Section,_Ident,_Default);
  RegIni.Free;
End;

function GetBoolOptions(_Section,_Ident:String;_Default:Boolean):Boolean;
Var RegIni:TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  Result:=RegIni.ReadBool(_Section,_Ident,_Default);
  RegIni.Free;
End;

procedure SetStringOptions(_Section,_Ident,_Value:String);
Var RegIni:TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  RegIni.WriteString(_Section,_Ident,_Value);
  RegIni.Free;
End;

procedure SetIntegerOptions(_Section,_Ident:String;_Value:Integer);
Var RegIni:TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  RegIni.WriteInteger(_Section,_Ident,_Value);
  RegIni.Free;
End;

procedure SetBoolOptions(_Section,_Ident:String;_Value:Boolean);
Var RegIni:TRegIniFile;
Begin
  RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
  RegIni.RootKey:=HKEY_CURRENT_USER;
  RegIni.WriteBool(_Section,_Ident,_Value);
  RegIni.Free;
End;
// End Of Functions

function ReadFromLanguage(_Section,_Ident,_Value:String):String;
var RegIni:TRegIniFile;
    LangIni:TIniFile;
    LangFile:String;
begin
  Result:=_Value;
  Try
    RegIni:=TRegIniFile.Create('Software\MADMAN Software\WinMZF');
    RegIni.RootKey:=HKEY_CURRENT_USER;

    LangFile:=RegIni.ReadString('Config','LanguageFile','');
    if Not FileExists(LangFile) then
      Exit;
    LangIni:=TIniFile.Create(LangFile);
    Result:=_Value;
    Result:=LangIni.ReadString(_Section,_Ident,_Value);
    LangIni.Free;
  Finally
    RegIni.Free;
    //LangIni.Free;
  End;
end;

end.
