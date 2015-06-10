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
     MAD7_u,RC5,RC6,IDEA;

//type TCompressLevel = (clNone, clFastest, clDefault, clMax);
Type TEncodeMode=(emBeforeCompress,emAfterCompress,emBeforeAfter);
Type TReWriteMode=(omRewrite,omSkip,omOverwriteAll,omSkipAll,omRename,omUnknown);
Type TEncodeAlgorithm=(eaAES256,eaMAD7,eaIDEA{,eaRC5,eaRC6});

Type TArcHead=Packed Record
    Siganture:Array[0..2]Of Char; 
    Version:Array[0..2]Of Char;
    //Vendor:Array[0..11]of Char;
  End;

Type TArcConfig=Packed Record
    EncodeHead,EncodeFile:Boolean;
    EncodeMode:TEncodeMode;
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
    LongFileName:WideString;
  End;

Type TSearchCriteria=Packed Record
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
  End;

Const
  Signature='MZF';
  Vendor='M.A.D.M.A.N.';
  Version='2.2';

  TimeFormat='mm-dd-yyyy hh:mm:ss';

  RootCaption='Root-[%s]';
  Title='WinMZF - [%s]';
  EmptyTitle='WinMZF';

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
  //
  Procedure SearchDir(Dir: string;FilesList:TStringList);
  Procedure SetPriority(ProcessPriority,ThreadPriority:Integer);
  Procedure CopyFilesToClipboard(FileList:string);
  {Procedure PasteFilesFromClipboard(PathTo: KOLString);}
  procedure WriteWideString(Stream:TStream;const AValue: WideString);
  function ReadWideString(Stream:TStream): WideString;
  procedure CompressStream(InStream,OutStream:TStream);
  // Encode and decode
  Procedure EncodeStream(InStream,OutStream:TStream;Password:String;Algorithm:TEncodeAlgorithm);
  Procedure DecodeStream(InStream,OutStream:TStream;Password:String;Algorithm:TEncodeAlgorithm);

implementation

procedure WriteWideString(Stream:TStream;const AValue: WideString);
var
  LLen: Integer;
begin
  LLen:=Length(AValue);
  Stream.Write(LLen, SizeOf(LLen));
  if LLen > 0 then
    Stream.Write(PWideChar(AValue)^, LLen * SizeOf(WideChar));
end;

function ReadWideString(Stream:TStream): WideString;
var
  LLen: Integer;
begin
  if Stream.Read(LLen, SizeOf(LLen)) <> SizeOf(LLen) then
    raise Exception.Create(rsInvalidStringLength);
  SetLength(Result, LLen);
  if LLen > 0 then
    If Stream.Read(PWideChar(Result)^, LLen * SizeOf(WideChar)) <> (LLen * SizeOf(WideChar)) Then
      Result:='';
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

procedure SearchDir(Dir: string;FilesList:TStringList);
var
  SR: TSearchRec; 
  FindRes: Integer; 
begin 
  FindRes := FindFirst(Dir + '*.*', faAnyFile, SR); 
       while FindRes = 0 do 
          begin
            if ((SR.Name = '.') or (SR.Name = '..')) then 
               begin 
                 FindRes := FindNext(SR); 
                 Continue; 
               end; 
            if ((SR.Attr and faDirectory) = faDirectory) then 
              begin 
                SearchDir(Dir + SR.Name + '\',FilesList);
                FindRes := FindNext(SR); 
                Continue; 
              end; 
            FilesList.Add(Dir + SR.Name);//Add to list
            FindRes := FindNext(SR);
          end;
  FindClose(FindRes);
end;

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
    MessageBox(Handle,'Coud not execute file','Error',MB_OK+MB_ICONSTOP);
End;

Function GetLastDir(Dir:String):String;
Var Index:Word;
Begin
  If Length(Dir)<1 Then Exit;
  If Dir[Length(Dir)]='\' Then Delete(Dir,Length(Dir)-1,1);
  For Index:= Length(Dir) DownTo 1 Do
    Begin
      If Dir[Index]='\' Then
        Begin
          Result:=Copy(Dir,1,Index-1);
          Exit;
        End;
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
Begin
  InStream.Position:=0;
  OutStream.Position:=0;

  encoder:=TLZMAEncoder.Create;
  encoder.SetAlgorithm(1);
  encoder.SetDictionarySize(28);
  encoder.SeNumFastBytes(273);
  encoder.SetLcLpPb(0,0,0);
  encoder.SetMatchFinder(2);
  encoder.WriteCoderProperties(OutStream);
  for i := 0 to 7 do
    WriteByte(outStream,(InStream.Size shr (8 * i)) and $FF);
  encoder.Code(InStream,OutStream,-1,-1);

  encoder.Free;
  
  InStream.Position:=0;
  OutStream.Position:=0;
End;

function ExpandStream(inStream, outStream :TStream):boolean;
var Count: integer;
    decoder:TLZMADecoder;
    properties:array[0..4] of byte;
    OutSize:LongWord;
    i,v:Byte;
begin
  inStream.Seek(0,soFromBeginning);
  outStream.Seek(0,soFromBeginning);

  decoder:=TLZMADecoder.Create;
  InStream.Read(Properties,5);
  If Not decoder.SetDecoderProperties(Properties) Then
    Begin
      Result:=False;
      Exit;
      decoder.Free;
    End;
  outSize := 0;
  for i := 0 to 7 do
    begin
      v := (ReadByte(inStream));
      if v < 0 then
        Begin
          Result:=False;
          decoder.Free;
          Exit;
        End;
      outSize := outSize or v shl (8 * i);
    end;
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
  For i:=0 to FileList.Count-1 do
    Begin
      TempList.Add(FileList.Strings[i]);
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

end.
