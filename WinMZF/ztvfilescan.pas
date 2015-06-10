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
Unit ztvFileScan;

Interface

Uses
   Windows,
   Messages,
   SysUtils,
   Classes,
   Forms,
   Dialogs,
   ztvBase,
   ztvGbls;

{$I ziptv.inc}

Type
   TfsScanOptions = (soScanDir, soScanDrvs);
   TOnFinishedEvent = Procedure(Sender: TObject; FilesCount: longint;
      FilesSize: Int64{Double}) Of Object;
   TUpdateFilesList = Procedure(Const aDir: String; aFile: TWin32FindData;
      pHeaderObj: pCompHeaderObj) Of Object;
   TOnRootDirChange = Procedure(Sender: TObject; Dir: String) Of Object;
   TScanMaskCollection = Procedure(Const Dir: String; Const aAttr: Integer;
      aMasks: TCollection) Of Object;

{$IFDEF DEL4_OR_HIGHER}
   {define SKIP_MASK_DEF}
{$ENDIF}

Type
{$IFNDEF SKIP_MASK_DEF}
   PMaskSet = ^TMaskSet;
   TMaskSet = Set Of char;
   TMaskStates = (msLiteral, msAny, msSet, msMBCSLiteral);
   TMaskState = Record
      SkipTo: Boolean;
      Case state: TMaskStates Of
         msLiteral: (Literal: char);
         msAny: ();
         msSet: (
            Negate: Boolean;
            CharSet: PMaskSet);
         msMBCSLiteral: (LeadByte, TrailByte: char);
   End;
   PMaskStateArray = ^TMaskStateArray;
   TMaskStateArray = Array[0..128] Of TMaskState;
{$ENDIF}

Type
   TInternalMask = Class
   Private
      fMask: Pointer;
      fSize: Integer;
      CheckExts: Boolean;               //Mask ends with literal dot char '.'
   Public
      Constructor Create(Const MaskValue: String); Virtual;
      Destructor Destroy; Override;
      Function matches(Const FileName: String): Boolean;
   End;

   TFileObject = Class(TObject)
      FileName: String;
      FullPath: String;
      FileSizeLow: Integer;
      FileSizeHigh: Integer;
      FileAttr: Integer;
      FileTime: Integer;
   Public
      Constructor Create(aName, aPath: String; aSizeLow, aSizeHigh, aAttr, aTime: Integer); Virtual;
      Destructor Destroy; Override;
   End;

   { Overriding TList existing methods for future using }
   TBaseFileList = Class(TList)
   Public
      Function Add(Item: Pointer): Integer; Virtual; Abstract;
      Procedure DeleteItem(Index: Integer); Virtual; Abstract;
      Procedure ClearList; Virtual; Abstract;
      Procedure Sort; Virtual; Abstract;
   End;

   TFilesList = Class(TBaseFileList)
   Private
      fSortCaseSen: Boolean;
      FSortOptions: TztvSortOptions;
      Procedure SetSortOptions(Const value: TztvSortOptions);
      Procedure QuickSort(l, R: Integer);
   Public
      Function Add(Item: Pointer): Integer; Override;
      Procedure ClearList; Override;
      Function Compare(Item1, Item2: Pointer): Integer; Virtual;
      Procedure DeleteItem(Index: Integer); Override;
      Function Find(s: Pointer; Var Index: Integer): Boolean; Virtual;
      Procedure Sort; Override;
   Published
      Property SortCaseSensitive: Boolean Read fSortCaseSen Write fSortCaseSen;
      Property Sorted: TztvSortOptions Read FSortOptions Write SetSortOptions;
   End;

   TztvFileScan = Class(TComponent)
   Private
      fAttributes: TztvFileAttrs;
      fAttributesEx: TztvFileAttrs;
      fFileMask: String;
      fFilesCount: longint;
      fFilesList: TFilesList;           // 061101 moved from private section
      fFileSpec: TStrings;
      fFilesSize: Int64; //Double;
      fIncludeHiddenDirs: Boolean;
      fOnScanFile: TOnScanFileEvent;
      fOnRecurseDir: TOnRecurseDir;
      fOnFinished: TOnFinishedEvent;
      fOnRootDirChange: TOnRootDirChange;
      fRootDir: String;
      fRecurseDirs: Boolean;
      fScanMaskCollection: TScanMaskCollection;
      fScanOptions: TfsScanOptions;
      //fSorted: TztvSortOptions;
      fUpdateFilesList: TUpdateFilesList;
      fUpdateList: Boolean;
      Function GetSorted: TztvSortOptions;
      Procedure SetSorted(value: TztvSortOptions);
      Procedure SetRootDir(Const value: String);
      Procedure ScanDir(Const Dir: String; Const aAttr: Integer;
         aMasks: TCollection);
      Procedure SetFileMask(FM: String);
      Procedure SetFileSpec(SML: TStrings);
      Procedure ScanDrvs(Const aAttr: Integer; MaskCollection: TCollection);
   Protected
   Public
      Cancel: Boolean;
      pCancel: ^Boolean;
      pHeaderObj: pCompHeaderObj;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Function GetFileAttrStr(Attr: Integer): String;
      Procedure Scan; Virtual;
      Procedure Stop; Virtual;
      Procedure DoScanMaskCollection(Const Dir: String; Const aAttr: Integer;
         aMasks: TCollection); Virtual;
      Procedure DoUpdateFileList(Const aDir: String; aFile: TWin32FindData;
         pHeaderObj: pCompHeaderObj); Virtual;
      Procedure SetAttribute(Attr: TztvFileAttr; value: Boolean);
      Procedure SetAttributeEx(Attr: TztvFileAttr; value: Boolean);
      Property Files: TFilesList Read fFilesList;
      Property FilesCount: longint Read fFilesCount;
      Property FileMask: String Read fFileMask Write SetFileMask;
      Property FilesSize: Int64 {Double} Read fFilesSize;
      Property UpdateFilesList: TUpdateFilesList Read fUpdateFilesList Write fUpdateFilesList;
      Property ScanMaskCollection: TScanMaskCollection Read fScanMaskCollection Write fScanMaskCollection;
   Published
      Property Sorted: TztvSortOptions Read GetSorted Write SetSorted;
      Property FileSpec: TStrings Read fFileSpec Write SetFileSpec Stored False;
      Property Attributes: TztvFileAttrs Read fAttributes Write fAttributes Stored True;
      Property AttributesEx: TztvFileAttrs Read fAttributesEx Write fAttributesEx Stored True;
      Property IncludeHiddenDirs: Boolean Read fIncludeHiddenDirs Write fIncludeHiddenDirs Default False;
      Property UpdateList: Boolean Read fUpdateList Write fUpdateList;
      Property ScanOptions: TfsScanOptions Read fScanOptions Write fScanOptions;
      Property RootDir: String Read fRootDir Write SetRootDir;
      Property RecurseDirs: Boolean Read fRecurseDirs Write fRecurseDirs;
      Property OnScanFile: TOnScanFileEvent Read fOnScanFile Write fOnScanFile;
      Property OnFinished: TOnFinishedEvent Read fOnFinished Write fOnFinished;
      Property OnRootDirChange: TOnRootDirChange Read fOnRootDirChange Write fOnRootDirChange;
      Property OnRecurseDir: TOnRecurseDir Read fOnRecurseDir Write
         fOnRecurseDir;
   End;

Function MatchesMask(Const FileName, Mask: String): Boolean;
Function SlashSep(Const Path, s: String): String;


Implementation


Type
   //TMbcsByteType = ( mbSingleByte, mbLeadByte, mbTrailByte );
   //TSysLocale = Packed Record
   //   DefaultLCID: LCID;
   //   PriLangID: LANGID;
   //   SubLangID: LANGID;
   //   FarEast: Boolean;
   //   MiddleEast: Boolean;
   //End;

   TMultiMask = Class(TCollectionItem)
      fMultiMask: TInternalMask;
   Public
      Destructor Destroy; Override;
   End;

//Var
//   LeadBytes: Set Of Char = [];
//   SysLocale: TSysLocale;

Const
   MaxCards = 30;

   //-------------------------------------------------------------

Function TztvFileScan.GetFileAttrStr(Attr: Integer): String;

Procedure AppendAttr(a: Integer; C: String);
   Begin
      If Attr And a > 0 Then
         Result := Result + C           //AppendStr( Result, c )
      Else
         Result := Result + '-';        //AppendStr( Result, '-' );
   End;
Begin
   Result := '';
   AppendAttr(FILE_ATTRIBUTE_ARCHIVE, 'A');
   AppendAttr(FILE_ATTRIBUTE_DIRECTORY, 'D');
   AppendAttr(FILE_ATTRIBUTE_HIDDEN, 'H');
   AppendAttr(FILE_ATTRIBUTE_READONLY, 'R');
   AppendAttr(FILE_ATTRIBUTE_SYSTEM, 'S');
   //AppendAttr(SysUtils.faVolumeID, 'V');
End;
//-------------------------------------------------------------

{Procedure InitSysLocale;
Var
   DefaultLCID: LCID;
   DefaultLangID: LANGID;
   AnsiCPInfo: TCPInfo;
   i: Integer;
   j: Byte;
Begin
   // Set default to English (US).
   SysLocale.DefaultLCID := $0409;
   SysLocale.PriLangID := LANG_ENGLISH;
   SysLocale.SubLangID := SUBLANG_ENGLISH_US;

   DefaultLCID := GetThreadLocale;
   If DefaultLCID <> 0 Then SysLocale.DefaultLCID := DefaultLCID;

   DefaultLangID := Word( DefaultLCID );
   If DefaultLangID <> 0 Then
   Begin
      SysLocale.PriLangID := DefaultLangID And $3FF;
      SysLocale.SubLangID := DefaultLangID Shr 10;
   End;

   SysLocale.MiddleEast := GetSystemMetrics( SM_MIDEASTENABLED ) <> 0;
   SysLocale.FarEast := GetSystemMetrics( SM_DBCSENABLED ) <> 0;
   If Not SysLocale.FarEast Then Exit;

   GetCPInfo( CP_ACP, AnsiCPInfo );
   With AnsiCPInfo Do
   Begin
      i := 0;
      While ( i < MAX_LEADBYTES ) And ( ( LeadByte[i] Or LeadByte[i + 1] ) <> 0 ) Do
      Begin
         For j := LeadByte[i] To LeadByte[i + 1] Do
            Include( LeadBytes, Char( j ) );
         Inc( i, 2 );
      End;
   End;
End;}
//-------------------------------------------------------------

{Function ByteTypeTest( p: PChar; Index: Integer ): TMbcsByteType;
Var
   i: Integer;
Begin
   Result := mbSingleByte;
   If ( p = Nil ) Or ( p[Index] = #$0 ) Then Exit;
   If ( Index = 0 ) Then
   Begin
      If p[0] In LeadBytes Then Result := mbLeadByte;
   End
   Else
   Begin
      i := Index - 1;
      While ( i >= 0 ) And ( p[i] In LeadBytes ) Do
         Dec( i );
      If ( ( Index - i ) Mod 2 ) = 0 Then
         Result := mbTrailByte
      Else If p[Index] In LeadBytes Then
         Result := mbLeadByte;
   End;
End;}
//-------------------------------------------------------------

{Function ByteType( Const s: String; Index: Integer ): TMbcsByteType;
Begin
   Result := mbSingleByte;
   If SysLocale.FarEast Then
      Result := ByteTypeTest( PChar( s ), Index - 1 );
End;}
//-------------------------------------------------------------

{$IFNDEF SKIP_MASK_DEF}

Function AnsiLastChar(Const s: String): PChar;
Var
   LastByte: Integer;
Begin
   LastByte := Length(s);
   If LastByte <> 0 Then
   Begin
      Result := @s[LastByte];
      //If ByteType( s, LastByte ) = mbTrailByte Then Dec( Result );
   End
   Else
      Result := Nil;
End;
{$ENDIF}
//-------------------------------------------------------------

Function SlashSep(Const Path, s: String): String;
Begin
   If Path = '' Then
   Begin
   	Result := '';
   	Exit;
   End;

   If AnsiLastChar(Path)^ <> '\' Then
      Result := Path + '\' + s
   Else
      Result := Path + s;
End;
//-------------------------------------------------------------

{ TMultiMask }

Destructor TMultiMask.Destroy;
Begin
   If Assigned(fMultiMask) Then
      fMultiMask.Free();
   Inherited;
End;
//-------------------------------------------------------------

{ TztvFileScan }

Constructor TztvFileScan.Create;
Begin
   Inherited Create(AOwner);
   Cancel := False;
   pHeaderObj := Nil;
   UpdateFilesList := DoUpdateFileList; // virtual procedure
   ScanMaskCollection := DoScanMaskCollection; // virtual procedure
   fFileMask := '*';
   RootDir := 'c:\';
   fAttributes := [fsZeroAttr, fsArchive, fsReadOnly, fsCompressed, fsEncrypted];
   fAttributesEx := [];
   fIncludeHiddenDirs := False;
   fScanOptions := soScanDir;
   fRecurseDirs := True;
   fUpdateList := True;
   pCancel := @Cancel;
   fFileSpec := TStringList.Create();
   fFilesList := TFilesList.Create();   //030401 changed...
   fFilesList.Sorted := soUnsort;       //030401 changed...
End;
//-------------------------------------------------------------

Destructor TztvFileScan.Destroy;
Begin
   fFilesList.ClearList();
   fFilesList.Free();
   fFileSpec.Free();
   Inherited Destroy();
End;
//-------------------------------------------------------------

Procedure TztvFileScan.SetAttribute(Attr: TztvFileAttr; value: Boolean);
Begin
   If value Then
      Include(fAttributes, Attr)
   Else
      Exclude(fAttributes, Attr);
End;
//-------------------------------------------------------------

Procedure TztvFileScan.SetAttributeEx;
Begin
   If value Then
      Include(fAttributesEx, Attr)
   Else
      Exclude(fAttributesEx, Attr);
End;
//-------------------------------------------------------------

Procedure TztvFileScan.SetRootDir;
Begin
   fRootDir := AppendDirTail(value);
End;
//-------------------------------------------------------------

Procedure TztvFileScan.ScanDrvs;
Var
   DriveNum: Integer;
   DriveChar: char;
   DriveType: TztvDriveType;
   DriveBits: Set Of 0..25;
Begin
   Integer(DriveBits) := GetLogicalDrives();
   For DriveNum := 0 To 25 Do
   Begin
      If Not (DriveNum In DriveBits) Then Continue;
      DriveChar := char(DriveNum + ord('a'));
      DriveType := TztvDriveType(GetDriveType(PChar(DriveChar + ':\')));
      DriveChar := UpCase(DriveChar);
      Case DriveType Of
         dtFixed,
            dtNetwork,
            dtCDROM,
            dtRam: ScanDir(DriveChar + ':', aAttr, MaskCollection);
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TztvFileScan.DoScanMaskCollection;
Begin
   Case fScanOptions Of
      soScanDir: ScanDir(Dir, aAttr, aMasks);
      soScanDrvs: ScanDrvs(aAttr, aMasks);
   End;
End;
//-------------------------------------------------------------

Procedure TztvFileScan.Scan;
Var
   MaskCollection: TCollection;
   WorkDir, Dir1, Dir2: String;
   i, k: Integer;
   FileSpecsMatch: Boolean;
   TempFileSpec: TStringList;

   Procedure CreateMasksFromList(aCol: TCollection; List: TStrings);
   Var
      p: TMultiMask;
      i: Integer;
   Begin
      For i := 0 To List.Count - 1 Do
      Begin
         p := TMultiMask(aCol.Add);
         If (List.Strings[i] = '') Or (List.Strings[i] = '*.*') Then
            List.Strings[i] := '*';
         p.fMultiMask := TInternalMask.Create(List.Strings[i]);
      End;
   End;

   Procedure CreateMasksFromStr(aCol: TCollection; aStr: String);
   Var
      p: TMultiMask;
      i: Integer;
      s: String;
   Begin
      i := 1;
      s := '';
      While i <= Length(aStr) Do
      Begin
         Case AnsiChar(aStr[i]) Of
            ' ':
               Begin
                  If s = '' Then
                     Inc(i)
                  Else
                  Begin
                     s := s + aStr[i];
                     If i = Length(aStr) Then
                     Begin
                        While AnsiLastChar(s)^ = ' ' Do
                           s := Copy(s, 1, Length(s) - 1);

                        p := TMultiMask(aCol.Add);
                        p.fMultiMask := TInternalMask.Create(s);
                        Exit;
                     End
                     Else
                        Inc(i);
                  End;
               End;
            ',':
               Begin
                  While AnsiLastChar(s)^ = ' ' Do
                     s := Copy(s, 1, Length(s) - 1);

                  p := TMultiMask(aCol.Add);
                  p.fMultiMask := TInternalMask.Create(s);
                  s := '';
                  If i = Length(aStr) Then
                     Exit
                  Else
                     Inc(i);
               End;
         Else
            Begin
               s := s + aStr[i];
               If i = Length(aStr) Then
               Begin
                  While AnsiLastChar(s)^ = ' ' Do
                     s := Copy(s, 1, Length(s) - 1);

                  p := TMultiMask(aCol.Add);
                  p.fMultiMask := TInternalMask.Create(s);
                  Exit;
               End Else
                  Inc(i);
            End;
         End;
      End;
   End;

Begin
   //fFilesList := TFilesList.Create;
   //Try
   //   fFilesList.Sorted := Sorted;
   fFilesList.ClearList();

   fFilesCount := 0;
   fFilesSize := 0;
   pCancel^ := False;

   If Length(fFileMask) > 0 Then
   Begin
      If (Length(fRootDir) = 0) Or (Not DirExists(fRootDir)) Then
         Exit;

      MaskCollection := TCollection.Create(TMultiMask);
      Try
         CreateMasksFromStr(MaskCollection, fFileMask);

         //ScanMaskCollection(fRootDir, GetAttrInt(), MaskCollection);
         ScanMaskCollection(fRootDir, AttributesToInt(fAttributes),
            MaskCollection);	// v4.7 revised
      Finally
         MaskCollection.Free();
      End;
   End
   Else
   Begin

      TempFileSpec := TStringList.Create();
      Try
      	i := fFileSpec.Count - 1;
         While i > -1 Do
         Begin
         	If Trim(fFileSpec[i]) = '' Then
            	fFileSpec.Delete(i);
            Dec(i);
         End;

         While fFileSpec.Count > 0 Do
         Begin
            If pCancel^ Then break;

            MaskCollection := TCollection.Create(TMultiMask);
            Try
               Try
                  If fFileSpec.Count < 1 Then Exit;

                  WorkDir := fRootDir;

                  Dir1 := ExtractFilePath(fFileSpec[0]);
                  If Length(Dir1) > 0 Then
                     WorkDir := Dir1;

                  If Length(WorkDir) < 1 Then
                     WorkDir := GetCurrentDir();

                  TempFileSpec.Add(ExtractFilename(fFileSpec[0]));

                  i := 1;
                  While i < fFileSpec.Count Do
                  Begin

                     Dir2 := ExtractFilePath(fFileSpec[i]);
                     If CompareText(WorkDir, Dir2) = 0 Then
                     Begin
                        FileSpecsMatch := False;
                        For k := 0 To TempFileSpec.Count - 1 Do
                        Begin

                           If MatchesMask(
                              ExtractFilename(fFileSpec[i]),
                              TempFileSpec[k]) Then
                           Begin
                              FileSpecsMatch := True;
                              fFileSpec.Delete(i);
                              Dec(i);
                              break;
                           End
                           Else
                              If MatchesMask(
                                 TempFileSpec[k],
                                 ExtractFilename(fFileSpec[i])) Then
                              Begin
                                 FileSpecsMatch := True;
                                 TempFileSpec[k] :=
                                    ExtractFilename(fFileSpec[i]);

                                 fFileSpec.Delete(i);
                                 Dec(i);
                                 break;
                              End;
                        End;

                        If (Not FileSpecsMatch) Then
                        Begin
                           TempFileSpec.Add(
                              ExtractFilename(fFileSpec[i]));
                           fFileSpec.Delete(i);
                           Dec(i);
                        End;
                     End;

                     Inc(i);

                  End;                  {while}

                  CreateMasksFromList(MaskCollection, TempFileSpec);

                  If fFileSpec.Count > 0 Then
                  Begin
                     If Assigned(fOnRootDirChange) Then
                        fOnRootDirChange(Self, WorkDir);

                     fFileSpec.Delete(0);
                  End;

               Finally
                  TempFileSpec.Clear();
               End;

               ///ScanMaskCollection(WorkDir {fRootDir}, GetAttrInt(), MaskCollection);
               ScanMaskCollection(WorkDir {fRootDir}, AttributesToInt(fAttributes),
                  MaskCollection);	// v4.7 revised

            Finally
               MaskCollection.Destroy();
            End;
         End;
      Finally
         TempFileSpec.Free();
      End;
   End;

   If Assigned(fOnFinished) Then
      fOnFinished(Self, fFilesCount, fFilesSize);

   //Finally
   //   fFilesList.ClearList( );
   //   fFilesList.Free( );
   //End;
End;
//-------------------------------------------------------------

Procedure TztvFileScan.SetFileMask(FM: String);
Begin
   fFileMask := FM;
   fFileSpec.Clear();
End;
//-------------------------------------------------------------

Procedure TztvFileScan.SetFileSpec(SML: TStrings);
Begin
   fFileSpec.Assign(SML);
   fFileMask := '';
End;
//-------------------------------------------------------------

Procedure TztvFileScan.ScanDir(Const Dir: String; Const aAttr: Integer;
	aMasks: TCollection);
Var
   LastOk: Boolean;
   Handle: THandle;
   FindData: TWin32FindData;
   zBuffer: Array[0..max_path] Of char;

   Function ExcludeThisAttr: Boolean;
   Begin
      With FindData Do
         Result :=
            ((fsEncrypted In fAttributesEx) And
            (dwFileAttributes And ZTV_FILE_ATTRIBUTE_ENCRYPTED > 0)) Or

            ((fsArchive In fAttributesEx) And
            (dwFileAttributes And FILE_ATTRIBUTE_ARCHIVE > 0)) Or

            ((fsZeroAttr In fAttributesEx) And
               (dwFileAttributes = 0)) Or

            ((fsZeroAttr In fAttributesEx) And
               (dwFileAttributes = FILE_ATTRIBUTE_NORMAL)) Or

            ((fsDirectory In fAttributesEx) And
               (dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY > 0)) Or

            ((fsReadOnly In fAttributesEx) And
               (dwFileAttributes And FILE_ATTRIBUTE_READONLY > 0)) Or

            ((fsHidden In fAttributesEx) And
               (dwFileAttributes And FILE_ATTRIBUTE_HIDDEN > 0)) Or

               //( ( fsVolumeID In fAttributesEx ) And
               //( dwFileAttributes And faVolumeID > 0 ) ) Or

            ((fsSysFile In fAttributesEx) And
               (dwFileAttributes And FILE_ATTRIBUTE_SYSTEM > 0)) Or

            ((fsCompressed In fAttributesEx) And
               (dwFileAttributes And FILE_ATTRIBUTE_COMPRESSED > 0));
   End;

   Function IncludeThisAttr: Boolean;
   Begin
      With FindData Do
         Result :=
				(dwFileAttributes And ZTV_FILE_ATTRIBUTE_NOT_CONTENT_INDEXED > 0) Or

            ((fsEncrypted In fAttributes) And
            (dwFileAttributes And ZTV_FILE_ATTRIBUTE_ENCRYPTED > 0)) Or

            ((fsArchive In fAttributes) And
            (dwFileAttributes And FILE_ATTRIBUTE_ARCHIVE > 0)) Or

            ((fsZeroAttr In fAttributes) And
               (dwFileAttributes=0)) Or

            ((fsZeroAttr In fAttributes) And
               (dwFileAttributes=FILE_ATTRIBUTE_NORMAL)) Or

            ((fsDirectory In fAttributes) And
               (dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY > 0)) Or

            ((fsReadOnly In fAttributes) And
               (dwFileAttributes And FILE_ATTRIBUTE_READONLY > 0)) Or

            ((fsHidden In fAttributes) And
               (dwFileAttributes And FILE_ATTRIBUTE_HIDDEN > 0)) Or

               //( ( fsVolumeID In fAttributesEx ) And
               //( dwFileAttributes And faVolumeID > 0 ) ) Or

            ((fsSysFile In fAttributes) And
               (dwFileAttributes And FILE_ATTRIBUTE_SYSTEM > 0)) Or

            ((fsCompressed In fAttributes) And
               (dwFileAttributes And FILE_ATTRIBUTE_COMPRESSED > 0));
   End;

   Function MultiMatches(n: String): Boolean;
   Var
      i: Integer;
   Begin
      For i := 0 To Pred(aMasks.Count) Do
         If TMultiMask(aMasks.Items[i]).fMultiMask.matches(n) Then
         Begin
            Result := True;
            Exit;
         End;

      Result := False;
   End;

Begin
	If Assigned(fOnRecurseDir) Then
   	fOnRecurseDir(Self, Dir);

   Handle := Windows.FindFirstFile(StrPCopy(zBuffer, SlashSep(Dir, '*.*')), FindData);
   LastOk := Handle <> INVALID_HANDLE_VALUE;

   With FindData Do
      Try
         While LastOk And (Not pCancel^) Do
         Begin
            If (String(cFilename) <> '.') And (String(cFilename) <> '..') Then
            Begin
               If IncludeThisAttr() And (Not ExcludeThisAttr()) Then
               Begin
                  If (dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY > 0) Then
                  Begin
                     If fUpdateList And Assigned(UpdateFilesList) Then
                        UpdateFilesList(
                        	AppendDirTail(SlashSep(Dir, cFilename)), FindData, pHeaderObj);
                  End
                  Else
                  Begin
                     If MultiMatches(cFilename) Then
                     Begin
                        Inc(fFilesCount);

								fFilesSize  :=
                           ((Int64(FindData.nFileSizeHigh) Shl 32) Or FindData.nFileSizeLow) +
                           fFilesSize;

                        If Assigned(fOnScanFile) Then
                           fOnScanFile(Self, cFileName, fFilesCount, fFilesSize);

                        If fUpdateList And Assigned(UpdateFilesList) Then
                            UpdateFilesList(AppendDirTail(Dir), FindData, pHeaderObj);
                     End;
                  End;
               End;

               If fRecurseDirs And
                  (dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY > 0) Then
               Begin
                  If ((dwFileAttributes And FILE_ATTRIBUTE_HIDDEN) > 0) Then
                  Begin
                     If fIncludeHiddenDirs Then
                        ScanDir(AppendDirTail(SlashSep(Dir, cFilename)), aAttr, aMasks)
                  End
                  Else
                     ScanDir(AppendDirTail(SlashSep(Dir, cFilename)), aAttr, aMasks);
               End;
            End;

            LastOk := Windows.FindNextFile(Handle, FindData);
            Application.ProcessMessages();

         End;

      Finally
         Windows.FindClose(Handle);
      End;
End;
//-------------------------------------------------------------

Procedure TztvFileScan.Stop;
Begin
   pCancel^ := True;
End;
//-------------------------------------------------------------

Procedure TztvFileScan.DoUpdateFileList(Const aDir: String;
	aFile: TWin32FindData; pHeaderObj: pCompHeaderObj);
Var
   f: TFileObject;
   FileTime: TFileTime;
   wDate, wTime: word;
Begin
   (* ftCreationTime, ftLastAccessTime, ftLastWriteTime *)
   FileTimeToLocalFileTime(aFile.ftLastWriteTime, FileTime);
   FileTimeToDosDateTime(FileTime, word(wDate), word(wTime));
   If aFile.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY > 0 Then
      // v4.7.1 changed
      //f := TFileObject.Create('', SlashSep(aDir, aFile.cFilename), aFile.nFileSizeLow,
      //   aFile.dwFileAttributes, Makelong(wTime, wDate));
      f := TFileObject.Create('', aDir, aFile.nFileSizeLow, aFile.nFileSizeHigh,
         aFile.dwFileAttributes, Makelong(wTime, wDate))
   Else
      f := TFileObject.Create(aFile.cFilename, SlashSep(aDir, ''), aFile.nFileSizeLow,
         aFile.nFileSizeHigh, aFile.dwFileAttributes, Makelong(wTime, wDate));

   fFilesList.Add(f);
End;
//-------------------------------------------------------------

Procedure TztvFileScan.SetSorted;
Begin
   fFilesList.Sorted := value;
   If Assigned(OnFinished) And (fFilesList.Count <> 0) Then
      OnFinished(Self, FilesCount, FilesSize);
End;
//-------------------------------------------------------------

Function TztvFileScan.GetSorted;
Begin
   Result := fFilesList.Sorted;
End;
//-------------------------------------------------------------

{ TInternalMask }
{$IFNDEF SKIP_MASK_DEF}

Function InitMaskStates(Const Mask: String;
   Var MaskStates: Array Of TMaskState): Integer;
Var
   i: Integer;
   SkipTo: Boolean;
   Literal: char;
   //LeadByte, TrailByte: Char;
   p: PChar;
   //Negate: Boolean;
   //CharSet: TMaskSet;
   Cards: Integer;

   {Procedure InvalidMask;
   Begin
      Raise Exception.CreateFmt( '''%s'' is an invalid mask at (%d)', [Mask,
         p - PChar( Mask ) + 1] );
   End;}

   Procedure Reset;
   Begin
      SkipTo := False;
      //Negate := False;
      //CharSet := [];
   End;

   Procedure WriteScan(MaskState: TMaskStates);
   Begin
      If i <= High(MaskStates) Then
      Begin
         If SkipTo Then
         Begin
            Inc(Cards);
            //If Cards > MaxCards Then InvalidMask;
         End;
         MaskStates[i].SkipTo := SkipTo;
         MaskStates[i].state := MaskState;

         MaskStates[i].Literal := UpCase(Literal);
         {Case MaskState Of
            msLiteral: MaskStates[i].Literal := Upcase( Literal );
            msSet:
               Begin
                  MaskStates[i].Negate := Negate;
                  New( MaskStates[i].CharSet );
                  MaskStates[i].CharSet^ := CharSet;
               End;
            msMBCSLiteral:
               Begin
                  MaskStates[i].LeadByte := LeadByte;
                  MaskStates[i].TrailByte := TrailByte;
               End;
         End;}
      End;
      Inc(i);
      Reset;
   End;

   {Procedure ScanSet;
   Var
      LastChar: Char;
      c: Char;
   Begin
      Inc( p );
      If p^ = '!' Then
      Begin
         Negate := True;
         Inc( p );
      End;
      LastChar := #0;
      While Not ( p^ In [#0, ']'] ) Do
      Begin
         // MBCS characters not supported in msSet!
         If p^ In LeadBytes Then
            Inc( p )
         Else
            Case p^ Of
               '-':
                  If LastChar = #0 Then
                     InvalidMask
                  Else
                  Begin
                     Inc( p );
                     For c := LastChar To Upcase( p^ ) Do
                        Include( CharSet, c );
                  End;
            Else
               LastChar := Upcase( p^ );
               Include( CharSet, LastChar );
            End;
         Inc( p );
      End;
      If ( p^ <> ']' ) Or ( CharSet = [] ) Then InvalidMask;
      WriteScan( msSet );
   End;}

Begin
   p := PChar(Mask);
   i := 0;
   Cards := 0;
   Reset;
   While p^ <> #0 Do
   Begin
      Case p^ Of
         '*': SkipTo := True;
         '?':
            If Not SkipTo Then WriteScan(msAny);
         //'[': ScanSet;
      Else
         //If p^ In LeadBytes Then
         //Begin
         //   LeadByte := p^;
         //   Inc( p );
         //   TrailByte := p^;
         //   WriteScan( msMBCSLiteral );
         //End
         //Else
         //Begin
         Literal := p^;
         WriteScan(msLiteral);
         //End;
      End;
      Inc(p);
   End;
   Literal := #0;
   WriteScan(msLiteral);
   Result := i;
End;
//-------------------------------------------------------------

Function MatchesMaskStates(Const FileName: String;
   MaskStates: Array Of TMaskState): Boolean;
Type
   TStackRec = Record
      SP: PChar;
      sI: Integer;
   End;
Var
   t: Integer;
   s: Array[0..MaxCards - 1] Of TStackRec;
   i: Integer;
   p: PChar;

   Procedure Push(p: PChar; i: Integer);
   Begin
   	s[t].SP := p;
      s[t].sI := i;
      Inc(t);
   End;

   Function Pop(Var p: PChar; Var i: Integer): Boolean;
   Begin
      If t = 0 Then
         Result := False
      Else
      Begin
         Dec(t);
         p := s[t].SP;
         i := s[t].sI;
         Result := True;
      End;
   End;

   Function matches(p: PChar; START: Integer): Boolean;
   Var
      i: Integer;
   Begin
      Result := False;
      For i := START To High(MaskStates) Do
         With MaskStates[i] Do
         Begin
            If SkipTo Then
            Begin
               Case state Of
                  msLiteral:
                     While (p^ <> #0) And (UpperCase(p^) <> Literal) Do
                        Inc(p);
                  msSet:
                     While (p^ <> #0) And Not (Negate Xor (UpCase(p^) In CharSet^)) Do
                        Inc(p);
               //   msMBCSLiteral:
               //      While ( p^ <> #0 ) Do
               //      Begin
               //         If ( p^ <> LeadByte ) Then
               //            Inc( p, 2 )
               //         Else
               //         Begin
               //            Inc( p );
               //            If ( p^ = TrailByte ) Then Break;
               //            Inc( p );
               //         End;
               //      End;
               End;
               If p^ <> #0 Then
                  Push(@p[1], i)
            End;

            Case state Of
               msLiteral:
                  If UpperCase(p^) <> Literal Then Exit;
               msSet:
                  If Not (Negate Xor (UpCase(p^) In CharSet^)) Then Exit;
            //   msMBCSLiteral:
            //      Begin
            //         If p^ <> LeadByte Then Exit;
            //         Inc( p );
            //         If p^ <> TrailByte Then Exit;
            //      End;
            End;
            Inc(p);
         End;
      Result := True;
   End;
Begin                                   {MatchesMaskStates}
   Result := True;
   t := 0;
   p := PChar(FileName);
   i := Low(MaskStates);
   Repeat
      If matches(p, i) Then
      	Exit;
   Until Not Pop(p, i);
   Result := False;
End;
//-------------------------------------------------------------

Procedure DoneMaskStates(Var MaskStates: Array Of TMaskState);
Var
   i: Integer;
Begin
   For i := Low(MaskStates) To High(MaskStates) Do
      If MaskStates[i].state = msSet Then
         dispose(MaskStates[i].CharSet);
End;
//-------------------------------------------------------------

Constructor TInternalMask.Create(Const MaskValue: String);
Var
   a: Array[0..0] Of TMaskState;
Begin
   fSize := InitMaskStates(MaskValue, a);
   fMask := AllocMem(fSize * SizeOf(TMaskState));
   InitMaskStates(MaskValue, Slice(PMaskStateArray(fMask)^, fSize));
   CheckExts := (MaskValue[Length(MaskValue)] = '.');
End;
//-------------------------------------------------------------

Destructor TInternalMask.Destroy;
Begin
   If fMask <> Nil Then
   Begin
      DoneMaskStates(Slice(PMaskStateArray(fMask)^, fSize));
      FreeMem(fMask, fSize * SizeOf(TMaskState));
   End;
End;
//-------------------------------------------------------------

Function TInternalMask.matches(Const FileName: String): Boolean;
Begin
   If CheckExts And (Pos('.', FileName) = 0) Then
      Result := MatchesMaskStates(FileName + '.', Slice(PMaskStateArray(fMask)^, fSize))
   Else
      Result := MatchesMaskStates(FileName, Slice(PMaskStateArray(fMask)^, fSize));
End;
//-------------------------------------------------------------

Function MatchesMask(Const FileName, Mask: String): Boolean;
Var
   CMask: TInternalMask;
Begin
   CMask := TInternalMask.Create(Mask);
   Try
      Result := CMask.matches(FileName);
   Finally
      CMask.Free();
   End;
End;
{$ENDIF}
//-------------------------------------------------------------

{ TFileObject }

Constructor TFileObject.Create;
Begin
   Inherited Create();
   FileName := aName;
   FullPath := aPath;
   FileSizeLow := aSizeLow;
   FileSizeHigh := aSizeHigh;
   FileAttr := aAttr;
   FileTime := aTime;
End;
//-------------------------------------------------------------

Destructor TFileObject.Destroy;
Begin
   Inherited;
End;
//-------------------------------------------------------------

{ TFilesList }

Function TFilesList.Add;
Begin
   If FSortOptions = soUnsort Then
      Result := Count
   Else
      Find(Item, Result);
   insert(Result, Item);
End;
//-------------------------------------------------------------

Procedure TFilesList.ClearList;
Var
   p: Pointer;
   i: Integer;
Begin
   For i := 0 To Count - 1 Do
   Begin
      p := List[i];
      If Assigned(p) Then
         TFileObject(p).Destroy();
   End;

   //While count <> 0 Do
   //   DeleteItem( 0 );
   Count := 0;
   Capacity := 0;
End;
//-------------------------------------------------------------

{Function AnsiLowerCaseFileName( Const S: String ): String;
Var
   I, l: Integer;
Begin
   If SysLocale.FarEast Then
   Begin
      l := Length( S );
      SetLength( Result, l );
      I := 1;
      While I <= l Do
      Begin
         Result[I] := S[I];
         If S[I] In LeadBytes Then
         Begin
            Inc( I );
            Result[I] := S[I];
         End
         Else If Result[I] In ['A'..'Z'] Then
            Inc( Byte( Result[I] ), 32 );
         Inc( I );
      End;
   End
   Else
      Result := AnsiLowerCase( S );
End;}
//-------------------------------------------------------------

{Function AnsiCompareFileName( Const S1, S2: String ): Integer;
Begin
   Result := AnsiCompareStr( AnsiLowerCaseFileName( S1 ), AnsiLowerCaseFileName( S2 ) );
   //Result := -AnsiCompareStr( AnsiLowerCaseFileName( S1 ), AnsiLowerCaseFileName( S2 ) );
End;}
//-------------------------------------------------------------

Function TFilesList.Compare(Item1, Item2: Pointer): Integer;

   Function IntCompare(Val1, Val2: Integer): Integer;
   Begin
      If Val1 < Val2 Then
         Result := -1
      Else
         If Val2 < Val1 Then
            Result := 1
         Else
            Result := 0;
   End;
Begin

   Case FSortOptions Of
      soByName:
         Begin
            If fSortCaseSen Then
               Result := AnsiCompareStr(
                  TFileObject(Item1).FileName,
                  TFileObject(Item2).FileName)
            Else
               Result := AnsiCompareText(
                  TFileObject(Item1).FileName,
                  TFileObject(Item2).FileName);
         End;

      soByType:
         Begin
            If fSortCaseSen Then
               Result := AnsiCompareStr(
                  ExtractFileExt(TFileObject(Item1).FileName),
                  ExtractFileExt(TFileObject(Item2).FileName))
            Else
               Result := AnsiCompareText(
                  ExtractFileExt(TFileObject(Item1).FileName),
                  ExtractFileExt(TFileObject(Item2).FileName));
         End;

      soBySize:
         Result := IntCompare(
            TFileObject(Item1).FileSizeLow,
            TFileObject(Item2).FileSizeLow);

      soByTime:
         Result := IntCompare(
            TFileObject(Item1).FileTime,
            TFileObject(Item2).FileTime);
   Else
      Result := 0;
   End;
End;
//-------------------------------------------------------------

Procedure TFilesList.DeleteItem;
Var
   p: Pointer;
Begin
   If (Index < 0) Or (Index >= Count) Then Exit;
   p := List[Index];
   If Assigned(p) Then
      TFileObject(p).Destroy();

   Count := Count - 1;
   If Index < Count Then
      System.Move(List^[Index + 1], List^[Index],
         (Count - Index) * SizeOf(Pointer));
End;
//-------------------------------------------------------------

Function TFilesList.Find(s: Pointer; Var Index: Integer): Boolean;
Var
   l, H, i, C: Integer;
Begin
   Result := False;
   l := 0;
   H := Count - 1;
   While l <= H Do
   Begin
      i := (l + H) Shr 1;
      C := Compare(List^[i], s);
      If C < 0 Then
         l := i + 1
      Else
      Begin
         H := i - 1;
         If C = 0 Then
            Result := True;
      End;
   End;
   Index := l;
End;
//-------------------------------------------------------------

Procedure TFilesList.QuickSort(l, R: Integer);
Var
   i, j: Integer;
   p, t: Pointer;
Begin
   Repeat
      i := l;
      j := R;
      p := List^[(l + R) Shr 1];
      Repeat
         While Compare(List^[i], p) < 0 Do
            Inc(i);
         While Compare(List^[j], p) > 0 Do
            Dec(j);
         If i <= j Then
         Begin
            t := List^[i];
            List^[i] := List^[j];
            List^[j] := t;
            Inc(i);
            Dec(j);
         End;
      Until i > j;
      If l < j Then QuickSort(l, j);
      l := i;
   Until i >= R;
End;
//-------------------------------------------------------------

Procedure TFilesList.SetSortOptions;
Begin
   If value <> FSortOptions Then
   Begin
      FSortOptions := value;
      Sort();
   End;
End;
//-------------------------------------------------------------

Procedure TFilesList.Sort;
Begin
   If (List <> Nil) And (Count > 0) Then
      QuickSort(0, Count - 1);
End;
//-------------------------------------------------------------


Initialization
{$IFNDEF DEL4_OR_HIGHER}
   //InitSysLocale( );
{$ENDIF}
End.
