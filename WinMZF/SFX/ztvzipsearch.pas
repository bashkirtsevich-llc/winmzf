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
(*

Added:

OnSearchBegin: Activates with the name of the file that is in que to be
              searched.  This filename can be a file that exists on disk
              (if the SearchNonArchives property is true), or the name of
              a file which is compressed inside an archive (if stDecompress
              is included in the SearchType property).

              NOTE(s):
              1. This event activates with filenames matching the FileSpec
                 property.
              2. With archive files, this event will activate at least one
                 time, with the name of a compressed file prior to the
                 OnMatchInFile event.  This is because OnMatchInFile
                 activates only on files in which a match is found...
                 OnSearchBegin activates with the name of every file ready to
                 be searched.
              3. The events SearchThisFile parameter provides the option to
                 bypass certain file(s) or filetypes.

OnMatchInFile: Activated when a match in a file is found.  It's event
              parameter (SearchFile) contains the name of the file (on disk)
              in which a match of SearchText was found.

              NOTE(s):
              1. This event does not return any filenames of compresssed
                 files... only the names of local files (disk files) in
                 which a match was found.
              2. It is activated on each file prior to the activation of any
                 OnMatch events.

     OnMatch: Activated once for every match found within an archive or
              file that is being searched.

              It's parameters return the name of the file of which a match
              of SearchText was found (if the file is a non-archive file),
              or the name of a compressed file if the file being searched is
              an archive and the offset position in the file where the match
              was found.

              NOTE(s):
              1. This event is always activated immediately following an
                 OnMatchInFile event.
              2. OnMatch can activate many times on one file.  The
                 OnMatchInFile event can activate only once per file.
                 Therefore, it is common for the OnMatchInFile event to
                 activate followed by several OnMatch events.
     *)
Unit ztvZipSearch;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   Forms,
   Dialogs,
   ztvBase,
   ztvGbls,
   ztvFileScan,
   ztvFileIo,
   ztvSearchEngine,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TOnChangeSearchFile =
      Procedure(Sender: TObject; FileName: String)
      Of Object;

   TOnFinish =
      Procedure(Sender: TObject; Files, matches, Bytes: Integer)
      Of Object;

   TOnMatchInFile =
      Procedure(Sender: TObject; SearchFile: String)
      Of Object;

   TOnMatch =
      Procedure(Sender: TObject; FileName: String; Offset: Integer)
      Of Object;

   TOnSearchBegin =
      Procedure(Sender: TObject; FileName: String; Count: Integer; Var SearchThisFile: Boolean)
      Of Object;

   TOnSearchEnd =
      Procedure(Sender: TObject; matches: Integer)
      Of Object;

   TztvSearchFind = (sfFirst, sfAll);
   TztvSearchMode = (smAsc, smBin, smDec, smHex, smOct);
   TztvSearchTypes = (stDecompress, stNonArchive, stProtected, stMultiVolume);
   TztvSearchType = Set Of TztvSearchTypes;



Type
   TZipSearch = Class(TUnBASE)
   Private
      fBeginOffset: DWord;
      fCaseSensitive: Boolean;
      fConvertedText: String;
      fEndOffset: DWord;
      fOnChangeDir: TOnRootDirChange;
      fOnChangeSearchFile: TOnChangeSearchFile;
      fOnSearchBegin: TOnSearchBegin;
      fOnSearchEnd: TOnSearchEnd;
      fOnFinish: TOnFinish;
      fOnMatchInFile: TOnMatchInFile;
      fOnMatch: TOnMatch;
      fSearchText: String;
      fSearchFind: TztvSearchFind;
      fSearchMode: TztvSearchMode;
      fSearchType: TztvSearchType;
      //fUseAscii: Boolean;
      l: Integer;                       //DWord;
      RemainingByteBuf: PChar;
      //Function ConvertBaseINT( Number: String; BaseNumber: Byte ): Integer;
      Function ConvertBaseStr(Number: String; BaseNumber, BaseConvertTo: Byte): String;
      Function DigitToValue(Digit: char): Byte;
      Function PositionValue(Position, BASE: Byte): longint;
      Function StringToValue(str: String; BASE: Byte): longint;
      Function ValueToString(value: longint; BASE: Byte): String;
      Procedure zsUnBaseNextVolume(Sender: TObject; Var VolumeName: String;
         VolumeID: String; fExists: Boolean; Var Cancel: Boolean);
      Procedure SearchRecProc(Index: Integer; Dir: String;
      	FindData: TWin32FindData; pHeaderObj: pCompHeaderObj); //WalkDirectories
      Procedure ArcOnChangeDir(Sender: TObject; Dir: String);
      Procedure ArcOnFileExists(Sender: TObject; FileName: String; Var NewFilename: String;
      	Var OverwriteMode: TOverwriteMode);
      Procedure ArcOnGetPassword(Sender: TObject; FileName: String;
         Var Password: String; Var TryAgain: Boolean);
      Procedure ArcOnNestedTarFile(Sender: TObject; FileName: String;
      	Var doUnTar: Boolean);


   Protected
      ContinuedBuffer: Boolean;
      FileChanged: Boolean;
      fLastPos: Integer;
      fMatches: Integer;
      fOfsBeg: Integer;
      fOfsEnd: Integer;
      fTotalBytes: Integer;
      fTotalFiles: Integer;
      fTotalMatches: Integer;
      Function ConvertBaseType(s: String): String;
      Procedure SearchNonArchive(Infile, Outfile: TStream32);
      Procedure SetArchiveFile(SFN: String); Override;
      Procedure SetExcludeExts(SEE: TStrings); Override;
      Procedure ArcOnActivate(Sender: TObject); Virtual;
      Procedure ArcOnDeactivate(Sender: TObject); Virtual;
      Procedure ArcOnBegin(Sender: TObject; FileName: String; Count: Integer;
         Var ExtractThisFile: Boolean); Virtual;
      Procedure ArcOnEnd(Sender: TObject; FileName: String; CRC_PASS: Boolean); Virtual;
      Procedure ArcOnError(Sender: TObject; FileName, ExtendedMsg,
         VolumeID: String; ECode: Integer); Virtual;
      Function ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
     		size: Byte; iCount: DWord; WriteType: TDataType): DWord; Virtual;
   Public
      DecompressObj: TUnBASE;
      TSO: TTurboSearchObj;
      TSOS: TSingleFindSearchObj;
      FoundPos: Integer;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Function CreateUnBase(AT: TArcType): TUnBASE;
      Procedure DoUpdateFilesList(Const Dir: String; FindData: TWin32FindData;
         pHeaderObj: pCompHeaderObj);
      Procedure Execute;
      Procedure Search; Virtual;
      Procedure SetSearchType(Search_Type: TztvSearchTypes; value: Boolean);
   Published
      Property ArchiveFile;
      Property Attributes;
      Property AttributesEx;
      Property ExcludeExts;
      Property FileSpec;
      Property IncludeHiddenDirs;
      Property SearchText: String Read fSearchText Write fSearchText;
      Property SearchType: TztvSearchType Read fSearchType Write fSearchType;
      Property Passwords;
      Property PasswordAttempts;
      Property RecurseDirs;
      Property OffsetBegin: DWord Read fBeginOffset Write fBeginOffset;
      Property OffsetEnd: DWord Read fEndOffset Write fEndOffset;
      //PROPERTY UseAscii: Boolean Read fUseAscii Write fUseAscii Default True;
      Property CaseSensitive: Boolean Read fCaseSensitive Write fCaseSensitive Default False;
      Property SearchFind: TztvSearchFind Read fSearchFind Write fSearchFind Default sfAll;
      Property SearchMode: TztvSearchMode Read fSearchMode Write fSearchMode Default smAsc;
      Property OnActivate;
      Property OnChangeDir: TOnRootDirChange Read fOnChangeDir Write fOnChangeDir;
      Property OnChangeSearchFile: TOnChangeSearchFile Read fOnChangeSearchFile Write fOnChangeSearchFile;
      Property OnDeactivate;
      Property OnError;
      Property OnExcludeFile;
      Property OnGetZipFirstDisk;
      Property OnGetZipNextDisk;
      Property OnGetZipLastDisk;
      Property OnMatch: TOnMatch Read fOnMatch Write fOnMatch;
      Property OnMatchInFile: TOnMatchInFile Read fOnMatchInFile Write fOnMatchInFile;
      //Property OnFileExists;
      //Property OnNestedTarFile;
      Property OnSearchBegin: TOnSearchBegin Read fOnSearchBegin Write fOnSearchBegin;
      Property OnSearchEnd: TOnSearchEnd Read fOnSearchEnd Write fOnSearchEnd;
      Property OnFinish: TOnFinish Read fOnFinish Write fOnFinish;
      Property OnGetPassword;
      Property OnNextVolume;
   End;

Implementation

Uses
   ztvUnCab,
   ztvUnBH,
   ztvUnZIP,
   ztvUnZOO,
   ztvUnTAR,
   ztvUnGZip,
   ztvUnARC,
   ztvUnACE2,
   ztvUnARJ,
   ztvUnLHA,
   ztvUnRAR,
   ztvUnJAR,
   ztvUUDecode,
   Err_Msgs;

Const
   GreaterDigit = 15;
   xDigits: Array[0..GreaterDigit] Of char = '0123456789ABCDEF';

   //-------------------------------------------------------------

Constructor TZipSearch.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   TSOS := TSingleFindSearchObj.Create(Self);
   TSO := TTurboSearchObj.Create(Self);

   doSearchRecAction := SearchRecProc;
   doReportDirChange := Nil;

   fCaseSensitive := False;
   fSearchFind := sfAll;
   fSearchMode := smAsc;
   RecurseDirs := False;
   fSearchType := [stDecompress];
   //UseAscii	:= True;
End;
//-------------------------------------------------------------

Destructor TZipSearch.Destroy;
Begin
   TSO.Destroy;
   TSOS.Destroy;
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TZipSearch.CreateUnBase(AT: TArcType): TUnBASE;
Begin

   Case AT Of

      atAce {,
      atAceExe}: Result := TUnACE.Create(Nil);

      atArc,
         atArcExe: Result := TUnARC.Create(Nil);

      atArj,
         atArjExe: Result := TUnArj.Create(Nil);

      atBh,
         atBhExe: Result := TUnBh.Create(Nil);

      atCab: Result := TUnCab.Create(Nil);

      atGZip: Result := TUnGZIP.Create(Nil);

      atJar: Result := TUnJar.Create(Nil);

      atLha,
         atLhaExe,
         atLzh,
         atLzhExe: Result := TUnLha.Create(Nil);

      atRar,
         atRarExe: Result := TUnRar.Create(Nil);

      atTar: Result := TUnTar.Create(Nil);

      atUUE: Result := TUUDecode.Create(Nil);

      atZipMV,
         atZip,
         atZipExe:
         Begin
            Result := TUnZip.Create(Nil);
            Result.ZipCmntBufSize := 32000;
         End;

      atZoo: Result := TUnZoo.Create(Nil);

   Else
      Result := Nil;
   End;
End;
//-------------------------------------------------------------

Procedure TZipSearch.SetArchiveFile(SFN: String);
Begin
   //CheckLoadingState( SFN );
   fArchiveFile := SFN;
End;
//-------------------------------------------------------------

Procedure TZipSearch.SetExcludeExts(SEE: TStrings);
Var
   i: Integer;
Begin
   For i := 0 To SEE.Count - 1 Do
      If Pos('.', SEE.Strings[i]) = 0 Then
         SEE.Strings[i] := '.' + LowerCase(SEE.Strings[i])
      Else
         SEE.Strings[i] := LowerCase(SEE.Strings[i]);

   ExcludeExts.Assign(SEE);
End;
//-------------------------------------------------------------

Procedure TZipSearch.ArcOnActivate(Sender: TObject);
Begin
   FileChanged := True;
   If Assigned(OnChangeSearchFile) Then
      OnChangeSearchFile(Self, fArchiveFile);
End;
//-------------------------------------------------------------

Procedure TZipSearch.ArcOnDeactivate(Sender: TObject);
Begin
   // Virtual method... do not delete!
End;
//-------------------------------------------------------------

Procedure TZipSearch.zsUnBaseNextVolume(Sender: TObject; Var VolumeName: String; VolumeID: String; fExists: Boolean; Var Cancel: Boolean);
Begin
   fArchiveFile := VolumeName;

   If (stMultiVolume In fSearchType) And Assigned(OnNextVolume) Then
      OnNextVolume(Self, VolumeName, VolumeID, fExists, Cancel)
   Else
      Cancel := True;
End;
//-------------------------------------------------------------

Procedure TZipSearch.ArcOnBegin(Sender: TObject; FileName: String; Count: Integer;
   Var ExtractThisFile: Boolean);
Begin
   fMatches := 0;

   If (stDecompress In fSearchType) And (DecompressObj <> Nil) Then
      ExtractThisFile :=
         (ExcludeExts.IndexOf(LowerCase(ExtractFileExt(FileName))) = -1);

   If ExtractThisFile Then
   Begin
      If ((stDecompress In fSearchType) And IsArcValid(ArcType)) Or
         (stNonArchive In fSearchType) Then
         If Assigned(OnSearchBegin) Then
            OnSearchBegin(Sender, FileName, Count, ExtractThisFile);

      If ExtractThisFile Then
         inc(fTotalFiles);

      fLastPos := 0;
      ContinuedBuffer := False;         //clear possible buffer overlap search
   End
   Else
      If Assigned(OnExcludeFile) Then
         OnExcludeFile(Self, FileName);
End;
//-------------------------------------------------------------

Procedure TZipSearch.ArcOnEnd(Sender: TObject; FileName: String; CRC_PASS: Boolean);
Begin
   If Assigned(OnSearchEnd) Then
      OnSearchEnd(Sender, fMatches);
End;
//-------------------------------------------------------------

Procedure TZipSearch.ArcOnError(Sender: TObject; FileName, ExtendedMsg, VolumeID: String; ECode: Integer);
Begin
   If ((stDecompress In fSearchType) And IsArcValid(ArcType))
      Or (stNonArchive In fSearchType) Then
      OnError(Sender, fArchiveFile, ExtractFilename(FileName), VolumeID, ECode);
End;
//-------------------------------------------------------------

Function TZipSearch.ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
	size: Byte; iCount: DWord; WriteType: TDataType): DWord;

   Procedure doOnMatch;
   Var
      fName: String;
   Begin
      If FileChanged Then
      Begin
         FileChanged := False;

         If DecompressObj = Nil Then
         Begin
            inc(fTotalMatches);
            inc(fMatches);
         End;

         If Assigned( OnMatchInFile ) Then
            OnMatchInFile( Self, fArchiveFile {, fConvertedText} );
      End;

      If (stDecompress In fSearchType) And (DecompressObj <> Nil) Then
         fName := UnixToDosFilename(DecompressObj.ActualFilename)
      Else
         fName := fArchiveFile;

      OnMatch(Self, fName, FoundPos + fLastPos);
   End;
Var
   SaveBuffer: Pointer;
Begin

   Result := iCount;

   (* Adjust for a possible broken search-String in the buffer.   *)
   If ContinuedBuffer Then
   Begin
      (* if ContinuedBuffer the byte count increases the length	*)
      (* of search-String (l).                                    *)
      inc(iCount, l);
      GetMem(SaveBuffer, iCount);
      //CopyMem( RemainingByteBuf^, SaveBuffer^, l );
      CopyMem(RemainingByteBuf, SaveBuffer, l);
      //CopyMem( Pointer( LongInt( @Buf ) )^, Pointer( DWord( SaveBuffer ) + l - 1 )^, Result );
      CopyMem(Pointer(Integer(@Buf)), Pointer(Integer(SaveBuffer) + l - 1), Result);
   End
   Else
      SaveBuffer := Pointer(@Buf);

   Try
      If (WriteType = dtData) And (Not Cancel) Then
      Begin

         If DecompressObj <> Nil Then
            inc(fTotalBytes, iCount);

         If (SearchFind = sfFirst) Then
         Begin

            If fLastPos = 0 Then
            Begin
               FoundPos := TSOS.SEARCH_BUFFER_FIRSTMATCH(SaveBuffer, iCount);
               If FoundPos > -1 Then
               Begin
                  inc(fTotalMatches);
                  inc(fMatches);
                  //Inc( fLastPos, Result );
                  If DecompressObj <> Nil Then
                     DecompressObj.Bytes_To_Go := 0;

                  doOnMatch();
               End;
            End;

         End
         Else
         Begin

            FoundPos := TSO.SEARCH_BUFFER_FIRSTMATCH(SaveBuffer, iCount);
            While FoundPos > -1 Do
            Begin

               If (FoundPos < fOfsBeg) Or (FoundPos > fOfsEnd) Then
                  break;

               inc(fTotalMatches);
               inc(fMatches);

               If ContinuedBuffer Then
                  Dec(FoundPos, (l - 1));

               doOnMatch();
               FoundPos := TSO.SEARCH_NEXTMATCH;
               //Inc( fLastPos, Result );
            End;

         End;
      End;

      inc(fLastPos, Result);

      If (SearchFind <> sfFirst) And (Result > DWord(l)) { AND (Bytes_To_Go > 0 )} Then
      Begin
            (* Text files are generally terminated with a carriage return/  *)
            (* line feed and are read into the buffer.  Adjust two bytes    *)
            (* for this possibility.  If they don't exist... just ignore.   *)
         If StrLComp(PChar(DWord(SaveBuffer) + (iCount - 2)), PChar(#13#10), 2) = 0 Then
         Begin
            //CopyMem( Pointer( DWord( SaveBuffer ) + ( iCount - l - 2 ) )^, RemainingByteBuf^, l );
            CopyMem(Pointer(DWord(SaveBuffer) + (iCount - DWord(l - 2))), RemainingByteBuf, l);
            l := Length(fConvertedText) + 2;
         End
         Else
         Begin
            //CopyMem( Pointer( DWord( SaveBuffer ) + ( iCount - l ) )^, RemainingByteBuf^, l );
            CopyMem(Pointer(DWord(SaveBuffer) + (iCount - DWord(l))), RemainingByteBuf, l);
            l := Length(fConvertedText);
         End;
      End;
      // Else
      //	l := 0;

   Finally
      If ContinuedBuffer Then
         FreeMem(SaveBuffer);

      ContinuedBuffer := Result > DWord(l);
   End;

   Application.ProcessMessages;
End;
//-------------------------------------------------------------

Function TZipSearch.ConvertBaseType(s: String): String;
Var
   i: word;
   Number, t: String;
   BaseNumber: Byte;
   BaseConvertTo: Byte;
   AddNumber: Boolean;
   w: ^word;
Const
   HexStr = ['0'..'9', 'A'..'F'];
   DecStr = ['0'..'9'];
   OctStr = ['0'..'7'];
   BinStr = ['0', '1'];
   BINARY = 2;
   OCTAL = 8;
   DECIMAL = 10;
   HEXADECIMAL = 16;
Begin

   SetLength(t, 0);
   BaseConvertTo := DECIMAL;

   Case SearchMode Of
      smAsc:
         Begin
            Result := s;
            Exit;
         End;
      smBin: BaseNumber := BINARY;
      smOct: BaseNumber := OCTAL;
      smDec: BaseNumber := DECIMAL;
      smHex:
         Begin
            s := UpperCase(s);
            BaseNumber := HEXADECIMAL;
         End;
   Else
      BaseNumber := DECIMAL;
   End;

   If Length(Trim(s)) > 0 Then
   Begin

      i := 1;
      Repeat

         If i > Length(s) Then break;

         While (s[i] = ' ') Do
         Begin
            w := @s[i];
            If w^ = 8224 Then t := t + ' '; //if w = '  ' (double-spc)
            inc(i);
         End;

         SetLength(Number, 0);

         While (i <= Length(s)) And (s[i] <> ' ') Do
         Begin

            Case SearchMode Of
               smHex: AddNumber := (s[i] In HexStr);
               smDec: AddNumber := (s[i] In DecStr);
               smOct: AddNumber := (s[i] In OctStr);
               smBin: AddNumber := (s[i] In BinStr);
            Else
               AddNumber := False;
            End;

            If AddNumber Then
               Number := Number + s[i];

            inc(i);

         End;

         If Length(Trim(Number)) > 0 Then
         Begin
            Try
               Case SearchMode Of
                  smDec: t := t + char(StrToInt(Number));
                  smHex: t := t + char(StrToInt('$' + Number));
               Else
                  t := t + char(StrToInt(ConvertBaseStr(Number, BaseNumber, BaseConvertTo)));
               End;
            Except
               //ON E: Exception DO
               On EConvertError Do
               Begin
                  SetLength(t, 0);
                  break;
               End;
            End;
         End;
      Until 1 <> 1;
   End
   Else
      t := s;

   Result := t;
End;
//-------------------------------------------------------------

Procedure TZipSearch.SearchNonArchive(Infile, Outfile: TStream32);
Type
   bufptr = ^Buffer;
   Buffer = Array[0..1000000] Of char;
Var
   Action: Boolean;
	MemSize: Integer;
   DataBuffer: bufptr;
   ReadResult: Cardinal;
   inHandle: THandle;
   
Begin
	inHandle := TFileStream32(Infile).Handle;
   If (stNonArchive In fSearchType) And
   	OpenArchive(inHandle, fArchiveFile) Then
   Begin
      Try
         inc(fTotalBytes, fLOF);

         ArcOnActivate(Self);

         Action := True;
         ArcOnBegin(Self, fArchiveFile, fTotalFiles, Action); //calls OnSearchBegin &
         If Not Action Then Exit;

         New(DataBuffer);
         Try

            If fOfsEnd >= fOfsEnd Then
               fLastPos := fOfsBeg
            Else
               fLastPos := 0;

            If fLastPos > 0 Then
               Infile.Seek(fLastPos, soBeginning);

            Bytes_To_Go := fOfsEnd - fOfsBeg;

            While Bytes_To_Go > 0 Do
            Begin

               // use the following block, instead of the min function...
               // the min function fails with files > 4 gig.
               //MemSize := Min(Bytes_To_Go, SizeOf(Buffer));
               If Bytes_To_Go > SizeOf(Buffer) Then
                  MemSize := SizeOf(Buffer)
               Else
                  MemSize := Bytes_To_Go;

               ReadResult := ReadBlock(Infile, Nil, DataBuffer^[0], False,
                  0, MemSize, dtHeader);

               If ReadResult = 0 Then break;

               ArcPutBlock(Outfile, DataBuffer^, False, 0, ReadResult, dtData);
               If Bytes_To_Go > 0 Then
                  Dec(Bytes_To_Go, ReadResult);
            End;
         Finally
            ArcOnEnd(Self, '', True);
            ArcOnDeactivate(Self);
            FreeMem(DataBuffer);
         End;
      Finally
         InFile.Free();
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TZipSearch.SearchRecProc(Index: Integer; Dir: String;
	FindData: TWin32FindData; pHeaderObj: pCompHeaderObj); //WalkDirectories
Var
   Infile, Outfile: TStream32;
Begin

   With FindData Do
      If (dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY = 0) Then
      Begin
         FillChar(RemainingByteBuf^, Length(fConvertedText), #255);

         Count := 0;
         ContinuedBuffer := False;

         fArchiveFile := Dir + cFilename;
         //If Assigned( OnChangeSearchFile ) Then
         //	OnChangeSearchFile( Self, fArchiveFile );

         If Length(fArchiveFile) = 0 Then
         Begin
            RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
            Exit;
         End
         Else
            If Not Assigned(OnMatch) Then
            Begin
               RaiseErrorStr(fArchiveFile, 'OnMatch', '0', E_REQUIREDEVENT);
               Exit;
            End
            Else
               If Length(fSearchText) = 0 Then
                  Exit;

         If (stDecompress In fSearchType) Then
            SetArcType(atUnknown)
         Else
            fArcType := atNA;

         If ArcType = atFileOpenErr Then Exit;
         fLOF := nFileSizeLow;

         Try
            If fBeginOffset > fLOF Then
               fOfsBeg := fLOF
            Else
               fOfsBeg := fBeginOffset;

            If (fEndOffset = 0) Or
               (fEndOffset < fBeginOffset) Or
               (fEndOffset > fLOF) Then
               fOfsEnd := fLOF
            Else
               fOfsEnd := fEndOffset;

            If (stDecompress In fSearchType) And IsArcSearchable(ArcType) Then
            Begin

               DecompressObj := CreateUnBase(ArcType); (* Create a DecompressObj object *)
               If DecompressObj = Nil Then
                  SearchNonArchive(Infile, Outfile)
               Else
               Try

                  (* Assign DecompressObj properties *)
                  DecompressObj.ArchiveFile := ArchiveFile;
                  If Not DecompressObj.IsArcValid(DecompressObj.ArcType) Then
                     (* ArchiveFile is not a valid archive... search non-decompress *)
                     SearchNonArchive(Infile, Outfile)
                  Else
                  Begin
                     DecompressObj.pCancel := pCancel;


                     DecompressObj.OnFileExists := ArcOnFileExists;
                     DecompressObj.OnNestedTarFile := ArcOnNestedTarFile;
                     DecompressObj.UseStoredDirs := True;
                     DecompressObj.FileSpec.Assign(FileSpec);
                     DecompressObj.Passwords := Passwords;
                     DecompressObj.PasswordAttempts := PasswordAttempts;
                     DecompressObj.TranslateOemChar := TranslateOemChar;

                     (* redirect writes to local ArcPutBlock *)
                     DecompressObj.ExtractWriteBlock := ArcPutBlock;

                     (* Assign DecompressObj events to point to
                        ZipSrch events *)
                     DecompressObj.OnError := ArcOnError;
                     DecompressObj.OnBegin := ArcOnBegin;
                     DecompressObj.OnEnd := ArcOnEnd;
                     DecompressObj.OnActivate := ArcOnActivate;
                     DecompressObj.OnDeactivate := ArcOnDeactivate;
                     DecompressObj.OnGetPassword := ArcOnGetPassword;
                     If (stMultiVolume In fSearchType) Then
       //If doSearchMultiVolumes Then
                     Begin
                        DecompressObj.OnGetZipFirstDisk := OnGetZipFirstDisk;
                        DecompressObj.OnGetZipNextDisk := OnGetZipNextDisk;
                        DecompressObj.OnGetZipLastDisk := OnGetZipLastDisk;
                     End;

                     DecompressObj.OnNextVolume := zsUnBaseNextVolume;

                     (* Activate decompression unique to
                        TZipSearch/TZipCheck *)

                     DecompressObj.ExtractToSearch();

                     (* Assign ZipSearch.Passwords as updated
                        DecompressObj.Passwords *)
                     Passwords := DecompressObj.Passwords;
                  End;
               Finally
                  DecompressObj.Destroy;
               End;

            End
            Else
            Begin
               DecompressObj := Nil;

             // no reason to search an archive in compressed format
               If (IsArchiveExt(ExtractFileExt(fArchiveFile)) = -1) Then
                  SearchNonArchive(Infile, Outfile);
            End;

         Finally
            (* Reset the write buffer *)
            ExtractWriteBlock := ExtractWriteProc;
         End;
      End;
End;
//-------------------------------------------------------------

Procedure TZipSearch.ArcOnChangeDir(Sender: TObject; Dir: String);
Begin
   If Assigned(OnChangeDir) Then
      OnChangeDir(Sender, Dir);
End;
//-------------------------------------------------------------

Procedure TZipSearch.ArcOnGetPassword(Sender: TObject; FileName: String;
   Var Password: String; Var TryAgain: Boolean);
Begin
   If Not (stProtected In fSearchType) Then
      TryAgain := False
   Else
      If Assigned(OnGetPassword) Then
         OnGetPassword(Sender, FileName, Password, TryAgain);
End;
//-------------------------------------------------------------

Procedure TZipSearch.ArcOnFileExists(Sender: TObject; FileName: String;
	Var NewFilename: String; Var OverwriteMode: TOverwriteMode);
Begin
	OverwriteMode := omOverwrite;
   ConfirmOverwrites := False;
   NewFilename := FileName;
End;
//-------------------------------------------------------------

Procedure TZipSearch.ArcOnNestedTarFile(Sender: TObject; FileName: String;
	Var doUnTar: Boolean);
Begin
	doUnTar := stDecompress In fSearchType;
End;
//-------------------------------------------------------------

Procedure TZipSearch.DoUpdateFilesList(Const Dir: String; FindData: TWin32FindData;
   pHeaderObj: pCompHeaderObj);
Begin
   If (ExcludeExts.Count > 0) Then
   Begin
      If (ExcludeExts.IndexOf(LowerCase(ExtractFileExt(
         FindData.cFilename))) = -1) Then
         doSearchRecAction(0, Dir, FindData, pHeaderObj)

      Else
         If (stNonArchive In fSearchType) And Assigned(OnExcludeFile) Then
         Begin
       //fArchiveFile := Dir + FindData.cFilename;
         //OnExcludeFile( Self, fArchiveFile );
            fArcType := atNA;
            SetLength(fArchiveFile, 0);
            OnExcludeFile(Self, Dir + FindData.cFilename);
         End;
   End
   Else
      doSearchRecAction(0, Dir, FindData, pHeaderObj);
End;
//-------------------------------------------------------------

Procedure TZipSearch.Execute;
Begin
 //
End;
//-------------------------------------------------------------

Procedure TZipSearch.Search;
Var
   HoldArchive: String;
   FileScan: TztvFileScan;
Begin
   If (Not (stDecompress In fSearchType)) And
      (Not (stNonArchive In fSearchType)) Then
      Exit;

   FoundPos := 0;
   Cancel := False;
   fTotalFiles := 0;
   fTotalBytes := 0;
   fTotalMatches := 0;
   fMatches := 0;

   fConvertedText := ConvertBaseType(fSearchText);
   If Length(fConvertedText) = 0 Then Exit;

   If Assigned(OnElapsedTime) Then ZipTimer.START;
   l := Length(fConvertedText);

   If Assigned(OnActivate) Then OnActivate(Self);
   GetMem(RemainingByteBuf, Length(fConvertedText));
   HoldArchive := fArchiveFile;

   Try
      //TSOS.ASSIGN_CALLBACKPROC( MYCALLBACK1 );
      If (SearchFind = sfFirst) Then
         TSOS.INIT_BUFFER_SEARCH(fConvertedText, {fUseAscii} SearchMode = smAsc, fCaseSensitive)
      Else
         TSO.INIT_BUFFER_SEARCH(fConvertedText, {fUseAscii} SearchMode = smAsc, fCaseSensitive);

      Try
         FileScan := TztvFileScan.Create(Self);
         Try
            FileScan.pHeaderObj := Nil;
            FileScan.Attributes := Attributes;
            FileScan.AttributesEx := AttributesEx;
            FileScan.IncludeHiddenDirs := IncludeHiddenDirs;
            FileScan.RootDir := ExtractFilePath(fArchiveFile);
            FileScan.FileMask := '';
            FileScan.FileSpec.Add(fArchiveFile); //FileSpec;
            FileScan.UpdateFilesList := DoUpdateFilesList;
            FileScan.RecurseDirs := RecurseDirs;
            //FileScan.OnFinished := FinishedEvent;
            //FileScan.OnRootDirChange := OnRootDirChange;
            FileScan.OnRecurseDir := ArcOnChangeDir;
            FileScan.pCancel := @fCancel;
            FileScan.Scan();
         Finally
            //FileScan.fFilesList.ClearList();  //no files added to fFilesList... no need to clear
            FileScan.Free();
         End;

      Finally
         If (SearchFind = sfFirst) Then
            TSOS.DONE
         Else
            TSO.DONE;
      End;
   Finally
      If Assigned(OnElapsedTime) Then ZipTimer.Stop;
      FreeMem(RemainingByteBuf);
      fArchiveFile := HoldArchive;
      If Assigned(OnFinish) Then
         OnFinish(Self, fTotalFiles, fTotalMatches, fTotalBytes);

      If Assigned(OnDeactivate) Then OnDeactivate(Self);
      If Assigned(OnElapsedTime) And (fTotalFiles > 0) And (fTotalBytes > 0) Then
         OnElapsedTime(Self, ZipTimer.ElapsedTime);
   End;
End;
//-------------------------------------------------------------

Procedure TZipSearch.SetSearchType(Search_Type: TztvSearchTypes; value: Boolean);
Begin
   If value Then
      Include(fSearchType, Search_Type)
   Else
      Exclude(fSearchType, Search_Type);
End;

//-------------------------------------------------------------
 (* start base conversion routines *)
//-------------------------------------------------------------

Function TZipSearch.DigitToValue(Digit: char): Byte;
Var
   Index: Byte;
Begin
   Digit := UpCase(Digit);
   Index := GreaterDigit;
   While (Index > 0) And (Digit <> xDigits[Index]) Do
      Dec(Index);

   (* unknow digit = 0 *)
   DigitToValue := Index;
End;
//-------------------------------------------------------------

Function TZipSearch.PositionValue(Position, BASE: Byte): longint;
Var
   value: longint;
   Index: Byte;
Begin
   value := 1;
   For Index := 2 To Position Do
      value := value * BASE;

   PositionValue := value;
End;
//-------------------------------------------------------------

Function TZipSearch.StringToValue(str: String; BASE: Byte): longint;
Var
   value: longint;
   Index: Byte;
Begin
   value := 0;
   For Index := 1 To Length(str) Do
      inc(value,
         DigitToValue(str[Index]) *
         PositionValue(Length(str) - Index + 1, BASE));

   StringToValue := value;
End;
//-------------------------------------------------------------

Function TZipSearch.ValueToString(value: longint; BASE: Byte): String;
Var
   str: String;
Begin
   If value = 0 Then
      str := xDigits[0]
   Else
   Begin
      SetLength(str, 0);
      While value > 0 Do
      Begin
         str := xDigits[value Mod BASE] + str;
         value := value Div BASE;
      End;
   End;
   Result := str;
End;
//-------------------------------------------------------------
(* IF Base = 'B' THEN BaseNumber/BaseConvert := 2;  *)
(* IF Base = 'O' THEN BaseNumber/BaseConvert := 8;  *)
(* IF Base = 'D' THEN BaseNumber/BaseConvert := 10; *)
(* IF Base = 'H' THEN BaseNumber/BaseConvert := 16; *)
//FUNCTION TZipSearch.ConvertBaseINT( Number: String; BaseNumber : BYTE ): INTEGER;
//BEGIN
//	Result := StringToValue (Number, BaseNumber);
//END;
//-------------------------------------------------------------

Function TZipSearch.ConvertBaseStr(Number: String; BaseNumber, BaseConvertTo: Byte): String;
Begin
   Result := ValueToString(StringToValue(Number, BaseNumber), BaseConvertTo);
End;
//-------------------------------------------------------------
 (* end base conversion routines *)
//-------------------------------------------------------------

End.
