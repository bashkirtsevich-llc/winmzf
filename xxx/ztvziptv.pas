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
// For using the TZipTV components streaming feature, we recommended you use
// a Delphi version 6 or greater.  The reasoning follows:
//
// TZipTV has 3 overriding Activate methods:
//   ZipTV.Activate();            {no streaming... must set ArchiveFile property}
//   ZipTV.Activate(s: TStream)   {slower due to the required overhead to convert to ZipTV's streaming}
//   ZipTV.Activate(s: TStream32) {fast direct streaming}
//
// ZipTV's streaming objects are compatible with Delphi starting at Delphi
// version 6.  For streaming compatiblility using Delphi 4 & 5, a conversion
// must be preformed.  For this reason, users of Delphi 6+ should use the
// streaming objects decending from TStream32 (TMemoryStream32, TFileStream32).
// In Delphi version 6, these objects are compatible with other Delphi objects
// such as CopyFrom, LoadFromStream, etc.  When using a TStream32 or decendant,
// the ZipTV.Activate(s: TStream32) method is called and is the fastest streaming
// method.
//
// Users of Delphi versions 4 & 5 should use Delphi's streaming objects which
// decend from Delphi's TStream object (TMemoryStream, TFileStream) for
// compatibility with other Delphi objects mentioned in the previous paragraph.
// However, when calling the Activate method, the ZipTV.Activate(s: TStream)
// method will be used and a conversion from TStream to TStream32 is preformed.
// After this conversion, the ZipTV.Activate(s: TStream32) method will be called
// internally using the converted TStream32 stream.
Unit ztvZipTV;

Interface

Uses
   Windows,
   Controls,
   SysUtils,
   Dialogs,
   Classes,
   Forms,
   StdCtrls,
   Buttons,
   ExtCtrls,
   ztvRegister,
   ztvBase,
   ztvGbls,
   ztvCrypt,
   ztvHeaders,
   ztvStreams,
   ztvFileIo,
   Err_Msgs;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TSetDate = (OrigDate, CurDate);
   TztvOperation = (opRead, opFileOnly, opQueryFile);
   TOnReadFile = Procedure(Sender: TObject; FileName: AnsiString) Of Object;

Type
	pReserved = Array[0..2] Of Pointer;

   TZipTV = Class(TZipCommon)
   Private
      ArcDir: AnsiString;
      Buffer: PChar;
      DiskSpannObj: TDiskSpannObj;
      IsArjMultiVolume: Boolean;
      pBlock: PChar;
      pRecord: Pointer;
      fRecordSize: Cardinal;            //Integer;
      fFilesList: TStrings;
      fShowEmptyDirs: Boolean;
      //Zip64LocalFiles: TZip64LocalFiles;
      //Zip64CentralFiles: TZip64CentralFiles;
      Central64Hdr: TCentral64Hdr;
      Vol_PackedSize: Int64;
      Vol_UnpackedSize: Int64;
      Waiting: Boolean;
      ztvOperation: TztvOperation;
      Procedure DecTotals(PackedSize, UnpackedSize: Int64);
      Procedure IncTotals(PackedSize, UnpackedSize: Int64);
		//Procedure OpenInfile(Var Instream: TFileStream32);
   Protected
      fOnReadBegin: TNotifyEvent;
      fOnReadEnd: TNotifyEvent;
      fOnReadFile: TOnReadFile;
      FOnTotals: TOnTotals;
      Procedure doProcRecord(Var Infile: TStream32);
      Procedure doRead(s: TStream32);
      Procedure doEvents(Position, Count: Cardinal);
      Procedure Setup(Var inStream: TStream32);
      Procedure SetFilename(SCF: AnsiString); Override;
   Public
   	inStream: TStream32;
      PtrBlock: pReserved;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure FilesInArchive(ts: TStrings);
      Function Activate: Boolean; Overload;
{$ifndef DEL6_OR_HIGHER}
		Function Activate(Source: TStream): Boolean; Overload;
		Function Activate(Instream: TStream32): Boolean; Overload;
{$else}
		Function Activate(Instream: TStream32): Boolean; Overload;
{$endif}
      Function doQueryInfo: Boolean;
		Function GetArchiveComment(p: PChar): Integer;
		Function GetCurrentArchiveComment(Infile: TFileStream32; p: PChar): Integer;
      Function GetFileInfo(FileName: AnsiString): Boolean;
      Function GetFileCrc(FileName: AnsiString): Integer;
      Function GetFileUnpackedSize(FileName: AnsiString): Int64;
      Function GetFilePackedSize(FileName: AnsiString): Int64;
      Function GetFileDate(FileName: AnsiString): TDateTime;
      Property FileName: AnsiString Read fFileName Write SetFilename;
   Published
      Property ArchiveFile;
      Property ArcType;
      Property FileSpec;
      Property Password;
      Property RecurseDirs;
      Property OnActivate;
      Property OnCorruptZipHeader;
      Property OnDeactivate;
      Property OnError;
      //Property OnGetZipNextVolume;
      Property OnGetZipFirstDisk;
      Property OnGetZipNextDisk;
      Property OnGetZipLastDisk;
      Property OnInsertDisk;
      Property OnNextVolume;
      Property OnRead;
      Property OnReadBegin: TNotifyEvent Read fOnReadBegin Write fOnReadBegin;
      Property OnReadEnd: TNotifyEvent Read fOnReadEnd Write fOnReadEnd;
      Property OnReadFile: TOnReadFile Read fOnReadFile Write fOnReadFile;
      Property OnTotals: TOnTotals Read FOnTotals Write FOnTotals;
      Property ShowEmptyDirs: Boolean Read fShowEmptyDirs Write fShowEmptyDirs
         Default False;
      Property ZipCmntBufSize;
   End;

Implementation

{$I CabStrut.inc}

Type
   TZipTV_EventHandlers = Class(TObject)
      Procedure ArcOnReadBegin(Sender: TObject);
      Procedure ArcOnTotals(Sender: TObject; UnpackSize, PackSize: Int64;
         Ratio, NumFiles: Integer);
      Procedure ArcOnRead(Sender: TObject; Offset, Filenum: Integer);
      Procedure ArcOnReadEnd(Sender: TObject);
      Procedure ArcOnActivate(Sender: TObject);
      Procedure ArcOnDeactivate(Sender: TObject);
   End;


//-------------------------------------------------------------

Constructor TZipTV.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   ztvOperation := opRead;
   ZipCmntBufSize := 32000;
   fShowEmptyDirs := False;
   DiskSpannObj := TDiskSpannObj.Create();
   RecurseDirs := True;
   Waiting := False;
End;
//-------------------------------------------------------------

Destructor TZipTV.Destroy;
Begin
   DiskSpannObj.Free();
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TZipTV.SetFilename(SCF: AnsiString);
Begin
   fFileName := OemToCharFilter(SCF, fTransOemChar);
End;
//-------------------------------------------------------------

Procedure TZipTV.Setup(Var inStream: TStream32);
Var
   bCancel: Boolean;
	MsGZ_DIR: TMsGZ_DIR;
   FilesInDir: Byte;
   i,	j,
      BufSize,
      BytesRead: Integer;
   FN: Array[0..255] Of Char;
Begin

   (* reset all properties... except FFileComment which is assigned later *)
   Count := 0;
   fCRC := 0;
   GlobalDate := 29221.000694;                 // 01/01/80 12:01 am
   fLOF := inStream.size;
   fOffsetEnd := fLOF;
   fPackedSize := 0;
   fRatio := 0;
   fsCompressType := '';
   fTotalPackedSize := 0;
   fTotalUnpackedSize := 0;
   fUnpackedSize := 0;
   Vol_PackedSize := 0;
   Vol_UnpackedSize := 0;

   Case ArcType Of

      atAce,
      	atAceExe:

         Begin
         	inStream.Position := fOffsetStart;
            inStream.Read(AceMHeader, SizeOf(AceMHeader));
            // ------------------------------------------------------
            //If (AceMHeader.HEAD_FLAGS AND ACE_ADDSIZE) > 0 THEN
            //	BytesRead := inStream.Read( skipsize, SizeOf( skipsize ) )
            //Else
            //   skipsize := 0;
            // ------------------------------------------------------

            // ------------------------------------------------------
            // for some strange reason, ace comments are encrypted.
            // Read the comment size (2 bytes)... the actual comment
            // follows this length field.
            //Ace_COMM_SIZE := 0;
            //If (AceMHeader.AC.HEAD_FLAGS And ACE_COMMENT > 0) Then
            //   inStream.Read(Ace_COMM_SIZE, SizeOf(Ace_COMM_SIZE));
            // ------------------------------------------------------
            pRecord := @AceFHeader;
            fRecordSize := SizeOf(AceFHeader);
            fOffsetStart := inStream.Position - SizeOf(AceMHeader) + AceMHeader.AC.Head_Size + 4 {+ AceMHeader.AV_SIZE};
         End;

      atArc,
      	atArcExe:

         Begin
            ArcDir := '';
            pRecord := @ArcHeader;
            fRecordSize := ArcHdr_Size;
         End;

      atArj,
      	atArjExe:

         Begin

            pArjHeader := Pointer(Buffer);
            pRecord := pArjHeader;

            With pArjHeader^ Do
            Begin
               inStream.Position := fOffsetStart;
               BufSize := SizeOf(HeadId) + SizeOf(HdrSize);
               BufSize := inStream.Read(pArjHeader^, BufSize);

               If (HeadId <> $EA60) Or (HdrSize = 0) Then
               Begin
                  //RaiseError(E_RAISE, fArchiveFile, '', '0', E_BADHEADR);
               	RaiseErrorStr(fArchiveFile, '', '0', E_BADHEADR);
                  Exit;
               End;

               inStream.Read(HeadSize, HdrSize);
               IsArjMultiVolume := ArjFlag And $04 > 0;

               fOffsetStart := fOffsetStart + Cardinal(BufSize + HdrSize);
            End;

            Inc(fOffsetStart, 4);
            Inc(fOffsetStart, 2);

            fRecordSize := BufSize;
         End;

      atBh,
      	atBhExe:

         Begin
            pRecord := @BhHeader;
            fRecordSize := SizeOf(TBh);
         End;

      atCab,
      	atCabExe:

         Begin
            inStream.Position := fOffsetStart;
            fFilePos := fOffsetStart;

            If (Not GetCabinet(inStream, fFilePos)) Then
            Begin
               RaiseErrorStr(fArchiveFile, '', '0', E_BADHEADR);
               fFilePos := fOffsetEnd;
            End
            Else
            Begin
               (* Read CFFOLDER information *)
               //If ( inStream.Read( CFFOLDER, CFFOLDERlen ) = 0 ) Then
               //Begin
               //   RaiseErrorStr( fArchiveFile, '', '0', E_FREAD );
               //   fFilePos := fOffsetEnd;
               //End;

               //inStream.Position := CFFOLDER.coffCabStart;
               //inStream.Read( CFDATA, CFDATAlen );
               //fFilePos := CFHeader.coffFiles;

               fOffsetStart := fOffsetStart + CFHeader.coffFiles;
               fOffsetEnd := inStream.size;
            End;

            pRecord := @CFFILE;
            fRecordSize := CFFILElen;
         End;

      atGZip:

         Begin
            pRecord := @GZipHeader;
            fRecordSize := GZipHdr_Size;
         End;

      atHA:

         Begin
            pRecord := @HAHeader;
            fRecordSize := HAHdr_Size;
            fOffsetStart := 3;
         End;

      atLha,
      	atLzh,
         atLhaExe,
         atLzhExe:

         Begin
            pRecord := @LzhHeader;
            fRecordSize := LZHHdr_Size;

            (* If no HeadLen then start at filepos 128 *)
            inStream.Position := fOffsetStart;
            inStream.Read(LzhHeader, LZHHdr_Size);
            If (LzhHeader.HeadLen = 0) Then
               fOffsetStart := fOffsetStart + 128;
         End;

      atMsGZ:

         Begin
            inStream.Read(MSGZMain, SizeOf(TMSGZMain));
            If (MSGZMain.OffsetToDirectory < DWord(inStream.size)) Then
            Begin
               inStream.Seek(MSGZMain.OffsetToDirectory, soBeginning);
               Repeat

                  inStream.Read(FilesInDir, SizeOf(FilesInDir));
                  If FilesInDir > 0 Then
                  Begin
                     inStream.Seek(-SizeOf(FilesInDir), soCurrent);
                  	inStream.Read(MsGZ_DIR.FilesInDir, 6);
                     inStream.Seek(MsGZ_DIR.HeaderLen - 6, soCurrent);
                  End Else
                  	Break;

               Until MsGZ_DIR.HeaderLen = 0;
               fOffsetStart := inStream.Position;
            End Else
               fOffsetStart := inStream.size;

            pRecord := @MSGZ;
            fRecordSize := SizeOf(TMSGZ);
         End;

      atPak,
      	atPakExe:

         Begin
            pRecord := @PakHeader;
            fRecordSize := PakHdr_Size;
         End;

      atRar,
      	atRarExe:

         Begin

            inStream.Position := fOffsetStart;
            inStream.Read(FN, 7);

            // ***********************************************************
            // The following lines determine if very old versions of
            // rar archive headers were used.  This was a time when
            // the rar archiver was not widely used.  At present, this
            // check is not used.  If we come across older archives that
            // used the old headers, we'll reimplement support at that time.
            // ***********************************************************
  				//If (
            //	(Byte(FN[0]) = $52 {82}) And
            //   (Byte(FN[1]) = $45 {69}) And
            //   (Byte(FN[2]) = $7e {126}) And
            //   (Byte(FN[3]) = $5e {94})) Then
            //   		ShowMessage('Old main header format');
            //Else
  				//	If (
            //		(Byte(FN[0]) = $52 {82}) And
            //      (Byte(FN[1]) = $61 {97}) And
            //      (Byte(FN[2]) = $72 {114}) And
            //      (Byte(FN[3]) = $21 {33}) And
            //      (Byte(FN[4]) = $1a {26}) And
            //      (Byte(FN[5]) = $07 { 7}) And
            //      (Byte(FN[6]) = $0  { 0})) Then
            //   		ShowMessage('New main header format');

            BytesRead := inStream.Read(Rar1Header, 13);
            If BytesRead <> 0 Then ;
            
            pRecord := @RarHeader;
            fRecordSize := SizeOf(TRarHeader);
            //fOffsetStart := inStream.Position;
            fOffsetStart := inStream.Position + (Rar1Header.HeadSize - 13);
         End;

      atTar:

         Begin
            pRecord := @TarHeader;
            fRecordSize := SizeOf(TarHeader);
         End;

      atUUE:

         Begin
            If (inStream.size < WSIZE) Then
               BufSize := inStream.size
            Else
               BufSize := WSIZE;

            GetMem(pBlock, BufSize);
            Try
               inStream.Position := 0;

               BytesRead := inStream.Read(pBlock[0], BufSize);

               i := 0;
               While (i < BytesRead) Do
               Begin

                  If (i + 2 < BytesRead) Then

                     If (i = 0) Or ((i > 0) And (pBlock[i - 1] = #10)) Then
                     Begin
                        If StrLComp(pBlock + i, 'begin ', 6) = 0 Then
                        Begin

                           If i = 0 Then
                              i := 6
                           Else
                              Inc(i, 6);

                           While (pBlock[i] <> #32) And (i < BytesRead) Do
                              Inc(i);

                           If (BytesRead > 255) Then
                              BytesRead := 255;

                           For j := i To i + BytesRead Do
                              If (pBlock[j] = #10) Or (pBlock[j] = #13) Then
                              Begin
                                 pBlock[j] := #0;
                                 break;
                              End;

                           fFileName := StrPas(pBlock + i + 1);
                           If (Pos(' ', fFileName) > 0) Then
                              SetLength(fFileName, Pos(' ', fFileName) - 1);

                           If Assigned(OnReadFile) Then
                              OnReadFile(Self, fFileName);

                           Case ztvOperation Of
                              opRead,
                                 opQueryFile:
                                 Begin
                                    If Assigned(OnRead) Then
                                    Begin
                                       GlobalDate := ztvConvertDate(FileAge(fArchiveFile));
                                       fVersionMadeBy := 0;
                                       fMinVersion := 0;
                                       fPackedSize := fLOF;
                                       fUnpackedSize := fLOF;
                                       fTotalPackedSize := fPackedSize;
                                       fTotalUnpackedSize := fUnpackedSize;
                                       fCRC := 0;
                                       fEncrypted := False;
                                       fsCompressType := '';
                                       fwCompressType := 0;
                                       fInternalAttr := 32;
                                       fExternalAttr := 32;
                                       fRatio := 0;
                                       doEvents(0, 1);
                                    End;
                                 End;
                              opFileOnly: fFilesList.Add(fFileName);
                           End;
                           Exit;
                        End;
                     End;
                  Inc(i);
               End;
            Finally
               FreeMem(pBlock, BufSize);
               fRecordSize := 0;        //prevent further block reads of UUE
               fFilePos := inStream.size;
            End;
         End;

      atJar,
      	atJarExe,
         atZip..atZipMV:
         Begin
            //If (htEnding In HeaderTypeState) And
            //   (EndZipHeader.CommentLen > 0) And
            //	(ArchiveCommentPos > 0) And
            //   (ArchiveCommentPos < FLOF) Then
            //Begin
            //	CmntStrm := TMemoryStream.Create();
            //   Try
            //      inStream.Position := ArchiveCommentPos;
            //      ztvStreamToTStream(inStream, CmntStrm);
            //      frmMain.Memo1.Lines.LoadFromStream( CmntStrm );
            //   Finally
            //   	CmntStrm.Destroy();
            //   End;
            //End;


            Case fArcType Of

               atZipDS:
                  Begin
                     DiskSpannObj.INIT(Self, RaiseError, RaiseErrorStr,
                        OnDiskWriteProtectErr, OnDiskInDrvErr, fCancel);

                     If DiskSpannObj.GetDisk(inStream) Then
                        fOffsetStart := EndZipHeader.CentralDirOffset
                     Else Begin
                        fOffsetStart := fLOF; 	//	force eof ptr position
                        Exit;
                     End;
                  End;

               atZipMV:
                  Begin
                  End;

            Else
               // do not include (htLocal In HeaderTypeState) check in
               // the following block... see \3\zip\SM12-MID.ZIP.
               If Not ((htCentral In HeaderTypeState) And
                  (htEnding In HeaderTypeState)) Then
               Begin
                  If Assigned(OnCorruptZipHeader) Then
                  Begin
                     bCancel := True;
                     OnCorruptZipHeader(Self, HeaderTypeState, bCancel);
                     If bCancel Then
                     Begin
                        fOffsetStart := fLOF;	//	force eof ptr position
                        Exit;
                     End;
                  End Else
                     RaiseErrorStr(fArchiveFile, 'OnCorruptZipHeader',
                        '0', E_REQUIREDEVENT);

               End;
            End;

            fOffsetEnd := fLOF;

            If Not (htCentral In HeaderTypeState) Then
               HeaderTypeState := [htLocal];

            inStream.Position := fOffsetStart;

            If (HeaderTypeState = [htLocal]) Then
            Begin
               If inStream.Read(LocalZipHeader, SizeOf(TLocal)) <> SizeOf(TLocal) Then
                  fOffsetStart := fOffsetEnd;	//	force eof ptr position

               If (LocalZipHeader.SignAtr = MULTIVOL_HEADER_SIGNATURE) Then
                  Inc(fOffsetStart, SizeOf(LocalZipHeader.SignAtr));

               fRecordSize := SizeOf(TLocal);
               pRecord := @LocalZipHeader;
            End
            Else Begin
               If (htCentral In HeaderTypeState) Then
               Begin
                  If inStream.Read(CentralZipHeader, SizeOf(TCentral)) <> SizeOf(TCentral) Then
                     fOffsetStart := fOffsetEnd;	//	force eof ptr position

                  fRecordSize := SizeOf(TCentral);
                  pRecord := @CentralZipHeader;
               End
               Else
                  RaiseErrorStr(fArchiveFile, '', '0', E_BADHEADR);

            End;
         End;

      atZoo:

         Begin
            pRecord := @ZooHeader;
            fRecordSize := ZooDirHdr_Size;
         End;
   End;
End;
//-------------------------------------------------------------

Procedure TZipTV_EventHandlers.ArcOnTotals(Sender: TObject; UnpackSize,
   PackSize: Int64; Ratio, NumFiles: Integer);
Begin
   (* for event redirection purposes... do not delete! *)
End;
//-------------------------------------------------------------

Procedure TZipTV_EventHandlers.ArcOnReadEnd(Sender: TObject);
Begin
   (* for event redirection purposes... do not delete! *)
End;
//-------------------------------------------------------------

Procedure TZipTV_EventHandlers.ArcOnReadBegin(Sender: TObject);
Begin
   (* for event redirection purposes... do not delete! *)
End;
//-------------------------------------------------------------

Procedure TZipTV_EventHandlers.ArcOnRead(Sender: TObject; Offset, Filenum: Integer);
Begin
   (* for event redirection purposes... do not delete! *)
End;
//-------------------------------------------------------------

Procedure TZipTV_EventHandlers.ArcOnActivate(Sender: TObject);
Begin
   (* for event redirection purposes... do not delete! *)
End;
//-------------------------------------------------------------

Procedure TZipTV_EventHandlers.ArcOnDeactivate(Sender: TObject);
Begin
   (* for event redirection purposes... do not delete! *)
End;
//-------------------------------------------------------------

Function TZipTV.doQueryInfo: Boolean;
Var
   OffsetStart: Int64;
   hOnRead: TOnRead;
   hOnTotals: TOnTotals;
   hOnReadEnd: TNotifyEvent;
   hOnActivate: TNotifyEvent;
   hOnReadBegin: TNotifyEvent;
   hOnDeactivate: TNotifyEvent;
   ZipEventHandler: TZipTV_EventHandlers;
Begin

   Result := False;
   If (fArchiveFile = '') Or (Not IsArcValid(fArcType)) Then Exit;

   (* save values for later restore *)
   hOnReadBegin := OnReadBegin;
   hOnRead := OnRead;
   hOnReadEnd := OnReadEnd;
   hOnActivate := OnActivate;
   hOnDeactivate := OnDeactivate;
   hOnTotals := OnTotals;
   OffsetStart := fOffsetStart;

   Try
      ZipEventHandler := TZipTV_EventHandlers.Create();
      Try
         OnRead := ZipEventHandler.ArcOnRead;
         OnTotals := ZipEventHandler.ArcOnTotals;
         OnReadBegin := ZipEventHandler.ArcOnReadBegin;
         OnReadEnd := ZipEventHandler.ArcOnReadEnd;
         OnActivate := ZipEventHandler.ArcOnActivate;
         OnDeactivate := ZipEventHandler.ArcOnDeactivate;
         Result := Activate();
      Finally
         (* restore original values *)
         fOffsetStart := OffsetStart;
         OnReadBegin := hOnReadBegin;
         OnRead := hOnRead;
         OnReadEnd := hOnReadEnd;
         OnActivate := hOnActivate;
         OnDeactivate := hOnDeactivate;
         OnTotals := hOnTotals;
         ZipEventHandler.Destroy();
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Procedure TZipTV.FilesInArchive(ts: TStrings);
Var
   hOperation: TztvOperation;
Begin
   If (ts = Nil) Then Exit;
   hOperation := ztvOperation;          (* save value for later restore *)
   ztvOperation := opFileOnly;          (* save value for later restore *)
   fFilesList := ts;
   Try
      If ( Not doQueryInfo() ) Then
         fFilesList.Clear();
   Finally
   	(* restore orig values *)
      ztvOperation := hOperation;
   End;
End;
//-------------------------------------------------------------

Function TZipTV.GetFileInfo(FileName: AnsiString): Boolean;
Var
   hFileSpec: TStringList;
   hOperation: TztvOperation;
Begin
   Result := False;
   If (FileName = '') Then Exit;

   hFileSpec := TStringList.Create();
   Try
      hFileSpec.Assign(FileSpec);       (* save value for later restore *)
      hOperation := ztvOperation;       (* save value for later restore *)
      ztvOperation := opQueryFile;
      Try
         Try
            FileSpec.Clear();
            FileSpec.Add(FileName);
            Result := doQueryInfo();
         Except
            Result := False;
         End;
      Finally
         FileSpec.Assign(hFileSpec);    (* restore orig value *)
         ztvOperation := hOperation;    (* restore orig value *)
      End;
   Finally
      hFileSpec.Free();
   End;
End;
//-------------------------------------------------------------

Function TZipTV.GetFileCrc(FileName: AnsiString): Integer;
Begin
   If GetFileInfo(FileName) Then
      Result := fCRC
   Else
      Result := -1;
End;
//-------------------------------------------------------------

Function TZipTV.GetFilePackedSize(FileName: AnsiString): Int64;
Begin
   If GetFileInfo(FileName) Then
      Result := PackedSize
   Else
      Result := -1;
End;
//-------------------------------------------------------------

Function TZipTV.GetFileUnpackedSize(FileName: AnsiString): Int64;
Begin
   If GetFileInfo(FileName) Then
      Result := UnpackedSize
   Else
      Result := -1;
End;
//-------------------------------------------------------------

Function TZipTV.GetFileDate(FileName: AnsiString): TDateTime;
Begin
   If GetFileInfo(FileName) Then
      Result := GlobalDate
   Else
      Result := 29221.000694;                 // 01/01/80 12:01 am
End;
//-------------------------------------------------------------

Procedure TZipTV.doEvents(Position, Count: Cardinal);
Begin
   If (fExternalAttr And ZTV_FILE_ATTRIBUTE_DIRECTORY) > 0 Then
      fFileName := AppendDirTail(fFileName);

   If Assigned(OnReadBegin) Then OnReadBegin(Self);
   OnRead(Self, Position, Count);
   If Assigned(OnReadEnd) Then OnReadEnd(Self);
End;
//-------------------------------------------------------------

Procedure TZipTV.doRead(s: TStream32);
Var
   Attr: Integer;
Begin

   Application.ProcessMessages();
   If pCancel^ Then
      Exit;

   Case ztvOperation Of

      opRead,
         opQueryFile:

         Begin
            If Assigned(OnRead) Then
               Case ArcType Of
                  atAce,
                  	atAceExe:

                     With AceFHeader Do
                     Begin
                        GlobalDate := ztvConvertDate(FTIME);
                        fVersionMadeBy := AceMHeader.VER_CR;
                        fMinVersion := AceMHeader.VER_CR;
                        fPackedSize := Vol_PackedSize;
                        fUnpackedSize := Vol_UnpackedSize;
                        fCRC := CRC32;
                        fEncrypted := (AC.HEAD_FLAGS And ACE_PASSWORD) > 0;
                        fsCompressType := GetCompressMethodStr(tech.Qual, 0);
                        fwCompressType := tech.Qual;
                        fInternalAttr := Attr;
                        fExternalAttr := Attr;
                        fRatio := CalcRatio(Vol_PackedSize, Vol_UnpackedSize);
								//doEvents(fFilePos - (AC.Head_Size + pSize + 4), Count);
                        doEvents(fOffsetStart, Count);
                     End;

                  atArc,
                     atArcExe:

                     With ArcHeader Do
                     Begin
                        GlobalDate := ztvConvertDate(SwapWords(FileDate));
                        fVersionMadeBy := 0; (* no Verion defined in ARC header *)
                        fMinVersion := 0; 	(* no Verion defined in ARC header *)
                        fPackedSize := PackedSize;
                        fUnpackedSize := UnpackedSize;
                        fCRC := CRC16;
                        fEncrypted := False;
                        fsCompressType := GetCompressMethodStr(CompressType, 0);
                        fwCompressType := CompressType;

                        If (CompressType = 30) Then
                           Attr := FILE_ATTRIBUTE_DIRECTORY
                        Else
                           Attr := FILE_ATTRIBUTE_ARCHIVE;

                        fInternalAttr := Attr;
                        fExternalAttr := Attr;
                        fRatio := CalcRatio(PackedSize, UnpackedSize);
                        //doEvents(fFilePos - fRecordSize, Count);
                        doEvents(fOffsetStart, Count);
                     End;

                  atArj,
                     atArjExe:

                     With pArjHeader^ Do
                     Begin
                        GlobalDate := ztvConvertDate(FileDate);
                        fVersionMadeBy := word(VerNum);
                        fMinVersion := word(MinVerNum);
                        fPackedSize := PackedSize;
                        fUnpackedSize := UnpackedSize;
                        fCRC := CRC32;
                        fEncrypted := (ArjFlag And $01) = $01;
                        fsCompressType := GetCompressMethodStr(CompressType, 0);
                        fwCompressType := CompressType;
                        fInternalAttr := ExternalAttr; (* no Attr defined with type ARJ *)
                        fExternalAttr := ExternalAttr; (* no Attr defined with type ARJ *)
                        fRatio := CalcRatio(PackedSize, UnpackedSize);
                        doEvents(fOffsetStart, Count);
                     End;

                  atBh,
                     atBhExe:

                     With BhHeader Do
                     Begin
                        GlobalDate := ztvConvertDate(FileDate);
                        fVersionMadeBy := word(VerNum);
                        fMinVersion := word(MinVerNum);
                        fEncrypted := (BitFlag And 1) = 1;
                        If fEncrypted And (PackedSize >= RAND_HEAD_LEN) Then
                           fPackedSize := PackedSize - RAND_HEAD_LEN
                        Else
                           fPackedSize := PackedSize;
                        fUnpackedSize := UnpackedSize;
                        fCRC := CRC32;
                        fsCompressType := GetCompressMethodStr(CompressType, BitFlag);
                        fwCompressType := CompressType;
                        fInternalAttr := LoWord(ExternalAttr); (* no Attr defined WITH TYPE BH *)
                        fExternalAttr := LoWord(ExternalAttr); (* no Attr defined WITH TYPE BH *)
                        fRatio := CalcRatio(PackedSize, UnpackedSize);
                        //doEvents(fFilePos - SizeOf(TBh) - FileNameLen, Count);
                        doEvents(fOffsetStart, Count);
                     End;

                  atCab,
                  	atCabExe:

                     With CFFILE Do
                     Begin
                        GlobalDate := ztvConvertDate(SwapWords(FileDate));
                        fVersionMadeBy := CFHeader.versionMinor;
                        fMinVersion := CFHeader.versionMajor;
                        fPackedSize := UnpackedSize;
                        fUnpackedSize := UnpackedSize;
                        fCRC := 0;
                        fEncrypted := False;
                        fsCompressType := GetCompressMethodStr(CFFOLDER.CompressType, 0);
                        fwCompressType := CFFOLDER.CompressType;
                        fInternalAttr := CFFILE.ExternalAttr;
                        fExternalAttr := CFFILE.ExternalAttr;
                        fRatio := 0;
                        //FVolumeName	:= IntToStr( CFHEADER.SetID );
                        //If ( ( CFHEADER.flags AND cfhdrNEXT_CABINET ) = 0 ) And
                        //		( ( CFHEADER.flags AND cfhdrPREV_CABINET ) = 0 ) Then
                        //	FVolumeName := '';

                        //doEvents(fFilePos - fRecordSize - Cardinal(Length(FileName) + 1), Count);
                        doEvents(fOffsetStart, Count);
                     End;

                  atGZip: ;

                  atPak,
                     atPakExe:

                     With PakHeader Do
                     Begin
                        GlobalDate := ztvConvertDate(SwapWords(FileDate));
                        fVersionMadeBy := 0; (* no VerNum defined WITH TYPE PAK *)
                        fMinVersion := 0; (* no VerNum defined WITH TYPE PAK *)
                        fUnpackedSize := UnpackedSize;
                        fPackedSize := PackedSize;
                        fCRC := CRC16;
                        fEncrypted := False;
                        fsCompressType := GetCompressMethodStr(CompressType, 0);
                        fwCompressType := CompressType;
                        fInternalAttr := FILE_ATTRIBUTE_ARCHIVE; (* no Attr defined WITH TYPE PAK *)
                        fExternalAttr := FILE_ATTRIBUTE_ARCHIVE; (* no Attr defined WITH TYPE PAK *)
                        fRatio := CalcRatio(PackedSize, UnpackedSize);
                        //doEvents(fFilePos - fRecordSize, Count);
                        doEvents(fOffsetStart, Count);
                     End;

                  atHA:

                     With HAHeader Do
                     Begin

                        (* FileDate := SwapWords( FileDate ); *)
                        (* dt.yr  :=  ( LoWord( FileDate ) SHR 9 )  AND $7f + 80;
                        dt.mo  := ( ( LoWord( FileDate ) SHR 5 )  AND $0f ) - 1;
                        dt.dy  :=  ( LoWord( FileDate )         AND $1f ) - 1;
                        dt.hr  := ( FileDate SHR 11 ) AND $1f;
                        dt.mm  := ( FileDate SHR 5 )  AND $3f;
                        dt.ss  := ( FileDate SHL 1 )  AND $3f; *)
                        (* dt.ss  := ( FileDate AND $1f ) * 2; *)

                        (* dt.yr := ( FileDate SHR 25 ) AND $7f;
                        dt.mo := ( FileDate SHR 21 ) AND $0f;
                        dt.dy := ( FileDate SHR 16 ) AND $1f;
                        dt.hr := ( FileDate SHR 11 ) AND $1f;
                        dt.mm := ( FileDate SHR  5 ) AND $3f;
                        dt.ss := ( FileDate SHL  1 ) AND $3f; *)

                        (* GlobalDate := EncodeDate( yr, mo, day ) +
                        	EncodeTime( hh, mm, ss, msec ); *)
                        (* GlobalDate := ztvConvertDate( SwapWords( FileDate ) ); *)

                        CompressType := VerNum And $F;
                        VerNum := VerNum Shr 4;

                        GlobalDate := Now;
                        fVersionMadeBy := VerNum;
                        fMinVersion := VerNum;
                        fPackedSize := PackedSize;
                        fUnpackedSize := UnpackedSize;
                        fCRC := CRC32;
                        fEncrypted := False;
                        fsCompressType := GetCompressMethodStr(CompressType, 0);
                        fwCompressType := CompressType;
                        fInternalAttr := FILE_ATTRIBUTE_ARCHIVE; (* no Attr defined WITH TYPE PAK *)
                        fExternalAttr := FILE_ATTRIBUTE_ARCHIVE;
                        fRatio := CalcRatio(PackedSize, UnpackedSize);
                        //doEvents(fFilePos - fRecordSize, Count);
                        doEvents(fOffsetStart, Count);
                     End;

                  atLha,
                     atLzh,
                     atLhaExe,
                     atLzhExe:

                     With LzhHeader Do
                     Begin
                        //IF Level = 2 THEN
                        If (OS = LHA_UNIXOS) Then
                           GlobalDate := UnixDateToDos(FileDate)
                        Else
                           GlobalDate := ztvConvertDate(FileDate);

                        fVersionMadeBy := 0; (* no VerNum *)
                        fMinVersion := 0; 	(* no VerNum *)

                        If (PackedSize > UnpackedSize) Then
                           fPackedSize := UnpackedSize
                        Else
                           fPackedSize := PackedSize;

                        fUnpackedSize := UnpackedSize;
                        //ztvSetFilePointer( InFile, FFilePos );
                        //BlockRead( InFile, fCRC, 2, BytesRead );
                        fCRC := LzhHeader.CRC16;
                        fEncrypted := False;
                        fsCompressType := GetCompressMethodStr(CompressType, 0);
                        fwCompressType := CompressType;
                        fInternalAttr := FILE_ATTRIBUTE_ARCHIVE; (* no Attr defined WITH TYPE PAK *)
                        fExternalAttr := Integer(ExternalAttr);
                        fRatio := CalcRatio(PackedSize, UnpackedSize);
                        doEvents(fOffsetStart, Count);
                     End;

                  atMsGZ:

                     With MSGZ Do
                     Begin
                        fUnpackedSize := UnpackedSize;
                        fPackedSize := PackedSize;

                        fCRC := 0;
                        fEncrypted := False;
                        fsCompressType := '';
                        fwCompressType := 0;
                        fExternalAttr := ExternalAttr;
                        fInternalAttr := fExternalAttr;
                        fRatio := CalcRatio(PackedSize, UnpackedSize);
                        GlobalDate := ztvConvertDate(SwapWords(FileDate));
                        doEvents(MSGZ.DataOffset, Count);
                     End;

                  atRar,
                     atRarExe:

                     With RarHeader Do
                     Begin
                        GlobalDate := ztvConvertDate(FileDate);
                        fVersionMadeBy := VerNum;
                        fMinVersion := 0; (* no MinVersion defined with type rar *)
                        fCRC := CRC32;
                        fEncrypted := (RarHeader.HeadFlags And LHD_PASSWORD > 0);
                        fsCompressType := GetCompressMethodStr(CompressType, 0);
                        fwCompressType := CompressType;
                        fExternalAttr := ExternalAttr;

                        //If HostOS = HostUNIX Then
                        //Begin
                        //	If (ExternalAttr And $4000 = 0) Then	(* Unix Dir *)
                        //   Begin
                        //		FInternalAttr	:= 32;
                        //		FExternalAttr  := FInternalAttr;
                        //   End Else
                        //   	FExternalAttr := FILE_ATTRIBUTE_DIRECTORY;
                        //End Else Begin
                        //	FInternalAttr	:= FILE_ATTRIBUTE_ARCHIVE;
                        //	FExternalAttr  := INTEGER( ExternalAttr );
                        //End;

                        If (RarHeader.HeadFlags And LHD_SPLIT_AFTER > 0) Then
                        Begin
                           fPackedSize := 0;
                           fUnpackedSize := Vol_UnpackedSize;
                           fRatio := 0;
                        End Else Begin
									//((Int64(PackedSize) Shl 32) Or PackedSize)
                           fPackedSize := Vol_PackedSize;
                           fUnpackedSize := Vol_UnpackedSize;
                           fRatio := CalcRatio(fPackedSize, fUnpackedSize);
                        End;

                        //doEvents(fFilePos - fRecordSize - FileNameLen, Count);
                        doEvents(fOffsetStart, Count);
                     End;

                  atTar:

                     With TarHeader Do
                     Begin
                        Try
                           GlobalDate := UnixDateToDos(OctStrToInt(MTime));
                        Except
                           GlobalDate := 29221.000694;                 // 01/01/80 12:01 am
                        End;

                        fPackedSize := OctStrToInt(AnsiString(size));
                        fUnpackedSize := fPackedSize;
                        fCRC := OctStrToInt(ChkSum);
                        fEncrypted := False;
                        fsCompressType := GetCompressMethodStr(0, 0);
                        fwCompressType := 0;

                        If (LinkFlag = LF_DIR) Then
                        Begin
                           fInternalAttr := faDirectory;
                           fExternalAttr := faDirectory;
                        End
                        Else
                        Begin
                           fInternalAttr := 0;
                           fExternalAttr := 0;
                        End;

                        fVersionMadeBy := 0;
                        fMinVersion := 0;
                        //doEvents(fFilePos - fRecordSize, Count);
                        doEvents(fOffsetStart, Count);
                     End;

                  atJar,
                     atJarExe,
                     atZip..atZipMV:
                     Begin
                        If HeaderTypeState = [htLocal] Then
                           With LocalZipHeader Do
                           Begin
                              GlobalDate := ztvConvertDate(zc.FileDate);
                              fVersionMadeBy := VerNum;
                              fMinVersion := VerNum;
                              fCRC := zc.CRC32;
                              fEncrypted := (zc.BitFlag And 1) = 1;

                              If fEncrypted And (zc.PackedSize >= RAND_HEAD_LEN) Then
                                 fPackedSize := zc.PackedSize - RAND_HEAD_LEN
                              Else
                                 fPackedSize := zc.PackedSize;

                     			fUnpackedSize :=
                              	(Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or zc.UnpackedSize;

                              fsCompressType := GetCompressMethodStr(zc.CompressType, zc.BitFlag);
                              fwCompressType := zc.CompressType;
                              fInternalAttr := 0; (* InternalAttr in CentralHead only *)
                              fExternalAttr := 0; (* ExternalAttr in CentralHead only *)
                              fRatio := CalcRatio(zc.PackedSize, zc.UnpackedSize);

                              (* already passed assignment check  ...call it 	*)
                              //doEvents(fFilePos - SizeOf(TLocal) - FileNameLen, Count);
                              doEvents(fOffsetStart, Count);
                           End
                        Else
                           With CentralZipHeader Do
                           Begin
                           	GlobalDate := ztvConvertDate(zc.FileDate);
                              fVersionMadeBy := VerNum;
                              fMinVersion := MinVerNum;
                              fCRC := zc.CRC32;
                              fEncrypted := (zc.BitFlag And 1) = 1;

                              If fEncrypted And (zc.PackedSize >= RAND_HEAD_LEN) Then
                     				fPackedSize := ((Int64(Central64Hdr.HiPackedSize) Shl 32) Or zc.PackedSize) - RAND_HEAD_LEN
                              Else
                     				fPackedSize := ((Int64(Central64Hdr.HiPackedSize) Shl 32) Or zc.PackedSize);

                     			fUnpackedSize := (Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or zc.UnpackedSize;

                              If (LoWord(ExternalAttr) And 16 > 0) Or (LoWord(ExternalAttr) And 8 > 0) Then
                                 fsCompressType := ''
                              Else
                                 fsCompressType := GetCompressMethodStr(zc.CompressType, zc.BitFlag);

                              fwCompressType := zc.CompressType;
                              fInternalAttr := LoWord(InternalAttr);
                              fExternalAttr := LoWord(ExternalAttr);
                              fRatio := CalcRatio(zc.PackedSize, zc.UnpackedSize);

                              Case fArcType Of
                              	atZipDS:
                                 	fVolumeName :=
                                    	PKBACK + Format('%.3d', [DiskNumberStart + 1]);
                                 atZipMV:
                                 	fVolumeName :=
                                    	GetNextVolumeName(
                                       	CentralZipHeader.DiskNumberStart,
                                          EndZipHeader.NumberOfThisDisk);
                              End;

                              (* already passed assignment check  ...call it 	*)
                              doEvents(RelativeOffsetOfLocalHeader - ZipSFX_OffsetAdjustment, Count);
                           End;

                     End;

                  atZoo:

                     With ZooDirHeader Do
                     Begin

                        (* year  =  ( direntry.date >> 9 ) & 0x7f;
                           month =  ( direntry.date >> 5 ) & 0x0f;
                           day   =  direntry.date        & 0x1f;

                           hours =  ( direntry.time >> 11 )& 0x1f;
                           min   =  ( direntry.time >> 5 ) & 0x3f;
                           sec   =  ( direntry.time & 0x1f ) * 2;
                           *)

                        FileDate := SwapWords(FileDate);
                        GlobalDate := ztvConvertDate(FileDate);
                        fVersionMadeBy := VerNum;
                        fMinVersion := MinVerNum;
                        fPackedSize := PackedSize;
                        fUnpackedSize := UnpackedSize;
                        fCRC := CRC16;
                        fEncrypted := False;
                        fsCompressType := GetCompressMethodStr(CompressType, 0);
                        fwCompressType := CompressType;
                        fInternalAttr := InternalAttr;
                        fExternalAttr := Integer(ExternalAttr);
                        fRatio := CalcRatio(PackedSize, UnpackedSize);
                        doEvents(fOffsetStart, Count);
                     End;
               End;
         End;

      opFileOnly: fFilesList.Add(fFileName);

   End;
End;
//-------------------------------------------------------------

Procedure TZipTV.DecTotals(PackedSize, UnpackedSize: Int64);
Begin
   fTotalPackedSize := fTotalPackedSize - PackedSize;
   fTotalUnpackedSize := fTotalUnpackedSize - UnpackedSize;
End;
//-------------------------------------------------------------

Procedure TZipTV.IncTotals(PackedSize, UnpackedSize: Int64);
Begin
   fTotalPackedSize := fTotalPackedSize + PackedSize;
   fTotalUnpackedSize := fTotalUnpackedSize + UnpackedSize;
End;
//-------------------------------------------------------------

Procedure TZipTV.doProcRecord(Var Infile: TStream32);

   Procedure GetComment(pCmnt: PChar; Len: Integer);
   Var
      i: Integer;
      BufSize, BytesRead: Cardinal;
   Begin

      If (Len = 0) Then
         BufSize := 2500                (* Arj max is 2400 including header... this should be plenty *)
      Else
         BufSize := Len;

      If (fFilePos + BufSize > Cardinal(Infile.size)) Then
         BufSize := Cardinal(Infile.size) - fFilePos - 1;

      BytesRead := Infile.Read(pCmnt^, BufSize);
      pCmnt[BytesRead] := #0;

      If (BytesRead = BufSize) Then
      Begin
         i := Strlen(pCmnt);
         If (Len = 0) Then Inc(i);      //if no length, read to null char
         Inc(fFilePos, i)
      End;
   End;

Var
   s: PChar;
   Dir: AnsiString;
   pWord: ^word;
   BytesRead: DWord;
   Attr,
   	i: Integer;
   frst_hdr_size: Byte;                 (* uchar *)
   ExtFieldLen: Integer;
   TranslateOem,
   	Encrypted: Boolean;
   RarHuge: TRarHugeFile;

   //Test, Test1: ^Int64;
   //EFL: pChar;

Const
   (* GZip header flags *)
   ASCII_FLAG = $01;                    (* bit 0 set: file probably ascii text *)
   CONTINUATION = $02;                  (* bit 1 set: continuation of multi-part gzip file *)
   EXTRA_FIELD = $04;                   (* bit 2 set: extra field present *)
   ORIG_NAME = $08;                     (* bit 3 set: original file name present *)
   Comment = $10;                       (* bit 4 set: file comment present *)
   gzENCRYPTED = $20;                   (* bit 5 set: file is encrypted *)
   Reserved = $C0;                      (* bit 6, 7:   reserved *)
   ACE_ADDSIZE = 1;
   CAB_FILE_CONTINUED = $FFFD;

Begin

   FileComment[0] := #0;
   Case ArcType Of

      atAce, atAceExe:

         With AceFHeader Do
         Begin

            If (AC.HEAD_TYPE = ACE_FILE_BLK) Then
            Begin
               (* Read remainder of header minus SizeOf(AceHeader)	*)
               (* that's already been read 									*)
               FileName :=
                  ReadFilename_DefinedLen(
                     Infile,
                     fFilePos,
                     FNAME_SIZE);

               If Assigned(OnReadFile) Then
                  OnReadFile(Self, fFileName);

               fFilePos :=
                  fFilePos - fRecordSize - FNAME_SIZE + AC.Head_Size + pSize + 4;

               If (AC.HEAD_FLAGS And ACE_SP_BEFORE > 0) Then
                  Inc(Vol_PackedSize, pSize)
               Else
                  Vol_PackedSize := pSize;

               Vol_UnpackedSize := size;

               If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
               Begin
                  If (Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
                  Begin
                     If ShowEmptyDirs Then doRead(Infile);
                  End
                  Else
                  Begin
                     Inc(Count);
                     IncTotals(pSize, size);
                     If (AC.HEAD_FLAGS And ACE_SP_BEFORE = 0) Then
                        doRead(Infile);
                  End;
               End;

               If (AC.HEAD_FLAGS And ACE_SP_AFTER > 0) Then
               Begin
                  If (Not doGetNextVolume(Infile, fVolumeName)) Then
                  Begin
                     Cancel := True;
                     Exit;
                  End;

                  fLOF := Infile.size;
                  fFilePos := 0;
                  fOffsetEnd := fLOF;

                  Infile.Position := fFilePos;
                  Infile.Read(AceMHeader, SizeOf(AceMHeader));
                  DecTotals(pSize, size);

                  fFilePos := AceMHeader.AC.Head_Size + 4;
                  Infile.Position := fFilePos;

                  // ace comments are encrypted... don't understand the reasoning
                  // of the need for an encrypted file comment.
                  //Ace_COMM_SIZE := 0;
                  //If (AceMHeader.AC.HEAD_FLAGS And ACE_COMMENT > 0) Then
                  //Begin
                  //   Infile.Read(Ace_COMM_SIZE, SizeOf(Ace_COMM_SIZE));
                  //   fFilePos := fFilePos + SizeOf(Ace_COMM_SIZE) + Ace_COMM_SIZE;
                  //End;
               End;

            End
            Else
               fFilePos := fOffsetEnd;

         End;

      atArc,
         atArcExe:

         With ArcHeader Do
         Begin

            If (marker = 26) Then
            Begin
               (* signals EOF for header reads 					*)
               If (CompressType = 31) Or (CompressType = 0) Then
                  (* force exit of read loop 						*)
                  fFilePos := fOffsetEnd

               Else
               Begin

                  (* Get filename prior to checking next CompressType byte *)
                  FileName := Copy(aFilename, 1, Pos(#0, aFilename) - 1);

                  (* Header is an appended directory entry 		*)
                  If (CompressType = 30) Then
                  Begin
                     ArcDir := ArcDir + AppendDirTail(fFileName);
                     fFileName := '';
                     PackedSize := 0;
                     UnpackedSize := 0;
                  End;

                  (* Retain dir name across file boundries.		*)
                  (*	Arc602 stores the dir as a separate header*)
                  (* prior to files in that dir.  No dirs are  *)
                  (* stored in compressed file headers.        *)
                  fFileName := ArcDir + fFileName;

                  If Assigned(OnReadFile) Then
                     OnReadFile(Self, fFileName);

                  If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
                  Begin
                     If (CompressType = 30) Then
                     Begin
                        If ShowEmptyDirs Then doRead(Infile);
                     End
                     Else
                     Begin
                        Inc(Count);
                        IncTotals(PackedSize, UnpackedSize);
                        doRead(Infile);
                     End;
                  End;

                  fFilePos := fFilePos + PackedSize;
               End;
            End
            Else
               fFilePos := fOffsetEnd;
         End;

      atArj,
         atArjExe:

         With pArjHeader^ Do
         Begin

            If (HeadId <> $EA60) Or (HdrSize = 0) Then
               fFilePos := fOffsetEnd
            Else
            Begin

               Infile.Read(HeadSize, HdrSize);
               frst_hdr_size := HeadSize;
               //fOffsetStart := fFilePos - fRecordSize;

               //If ( ( ArjFlag AND 8 ) = 8 ) Then
               //   Inc( fFilePos, Sizeof( Integer ) );		(* skip the CRC *)

               FileName := StrPas(PChar(@HeadSize) + frst_hdr_size);
               If Assigned(OnReadFile) Then
                  OnReadFile(Self, fFileName);

               //FileComment := PChar(@HeadSize) + frst_hdr_size + Length( FFilename ) + 1;
               Inc(fFilePos, HdrSize);
               fFilePos := fFilePos + 2; (* extended header size *)
               fFilePos := fFilePos + 4; (* header crc *)
               fFilePos := fFilePos + PackedSize;

               Vol_PackedSize := PackedSize;
               Vol_UnpackedSize := UnpackedSize;

               (* Flag values of 20, 24, and 28 are continuation files		*)
               (* 20 = beginning part of split compressed file 				*)
               (* 24 = rest of split file 											*)
               (* 28 = next part of split file spanning also to next part	*)

               (* Multi-volume archive *)
               If (ArjFlag And $04 = $04) Then
               Begin

                  ArjFlag := 0;
                  Repeat
                     If (Not doGetNextVolume(Infile, fVolumeName)) Then
                     Begin
                        Cancel := True;
                        Exit;
                     End;

                     fLOF := Infile.size;
                     fFilePos := 0;
                     fOffsetEnd := fLOF;

                     Infile.Read(HeadId, fRecordSize);
                     Infile.Read(HeadSize, HdrSize);
                     Inc(fFilePos, HdrSize + fRecordSize);

                     If (HeadId = $EA60) Then
                     Begin
                        IsArjMultiVolume := ArjFlag And $04 > 0;

                        //frst_hdr_size := pArjHeader^.HeadSize;
                        //FVolumeName := StrPas( PChar(@pArjHeader^.HeadSize) + frst_hdr_size );
                        fFilePos := fFilePos + 2; (* extended header size *)
                        fFilePos := fFilePos + 4; (* header crc *)

                        Infile.Position := fFilePos;
                        Infile.Read(pArjHeader^, fRecordSize);
                        Infile.Read(pArjHeader^.HeadSize, pArjHeader^.HdrSize);
                        frst_hdr_size := HeadSize;
                        Inc(fFilePos, HdrSize + fRecordSize);
                     End;

                     Inc(Vol_PackedSize, PackedSize);
                     Inc(Vol_UnpackedSize, UnpackedSize);

                  Until (ArjFlag And $04) = 0;

                  If (HeadId = $EA60) Then
                  Begin
                     FileName := StrPas(PChar(@HeadSize) + frst_hdr_size);
                     fFilePos := fFilePos + 4;
                     fFilePos := fFilePos + 2;
                     fFilePos := fFilePos + PackedSize;
                  End;

               End
               Else

                  (* if a secondary volume is opened before the archive	*)
                  If IsArjMultiVolume And ((ArjFlag And $08) = $08) Then
                     Exit;

               PackedSize := Vol_PackedSize;
               UnpackedSize := Vol_UnpackedSize;

               If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
               Begin
                  If (FileType = 3) Then
                  Begin
                     If ShowEmptyDirs Then doRead(Infile);
                  End
                  Else
                  Begin
                     IncTotals(PackedSize, UnpackedSize);
                     Inc(Count);
                     doRead(Infile);
                  End;
               End;
            End;
         End;

      atBh,
         atBhExe:

         With BhHeader Do
         Begin

            If (SignAtr = BLAKHOLE_SIGNATURE) Then
            Begin
               (* signals EOF for header reads *)
               If (HdrSize = 0) Then
                  fFilePos := fOffsetEnd (* Force exit of read loop *)
               Else
               Begin

                  FileName :=
                     ReadFilename_DefinedLen(
                        Infile,
                        fFilePos,
                        FileNameLen);

                  If Assigned(OnReadFile) Then
                     OnReadFile(Self, fFileName);

                  If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
                  Begin
                     If (ExternalAttr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
                     Begin
                        If ShowEmptyDirs Then doRead(Infile);
                     End
                     Else
                     Begin
                        Inc(Count);
                        IncTotals(PackedSize, UnpackedSize);
                        doRead(Infile);
                     End;
                  End;

                  fFilePos := fFilePos + PackedSize;
               End;
            End
            Else
               fFilePos := fOffsetEnd;

         End;

      atCab,
      	atCabExe:

         Begin

            If (CFHeader.cFiles = 0) Then
            Begin

               If (CFHeader.flags And cfhdrNEXT_CABINET = cfhdrNEXT_CABINET) Then
               Begin

                  fVolumeName := AppendDirTail(ExtractFileDir(
                     fArchiveFile)) + szCabinetNEXT;

                  If (CompareText(fVolumeName, fArchiveFile) = 0) Then
                  Begin
                     fFilePos := fOffsetEnd;
                     Exit;
                  End;

                  If (Not GetNextVolume(fVolumeName, szDiskNext)) Then
                  Begin
                     Cancel := True;
                     Exit;
                  End;

                  Infile.Free();
                  Infile := TFileStream32.Create(fVolumeName, fmOpenRead
                     Or fmShareDenyNone);

                  If (TFileStream(Infile).Handle < 0) Then
                  Begin
                     RaiseErrorStr(ArchiveFile, '', '0', E_FOPEN);
                     fOffsetEnd := Infile.size;
                     fFilePos := fOffsetEnd;
                     Infile.Free();
                     Infile := Nil;
                     Exit;
                  End;

                  fOffsetEnd := Infile.size;

                  If Not GetCabinet(Infile, fFilePos) Then
                  Begin
                     RaiseErrorStr(szCabinetNEXT, '', szDiskNext, E_BADHEADR);
                     fFilePos := fOffsetEnd;
                  End
                  Else
                  Begin
                     (* Read CFFOLDER information *)
                     //BytesRead := Infile.Read( CFFOLDER, CFFOLDERlen );
                     Infile.Position := CFFOLDER.coffCabStart;
                     //BytesRead := Infile.Read( CFDATA, CFDATAlen );
                     fFilePos := CFHeader.coffFiles;
                     //fOffsetStart := fFilePos;
                  End;
               End
               Else
                  fFilePos := fOffsetEnd;

            End
            Else
            Begin

               FileName := ReadFilename_NullTerminate(Infile, fFilePos);

               If Assigned(OnReadFile) Then
                  OnReadFile(Self, fFileName);

               Dec(CFHeader.cFiles);

               If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
               Begin
                  If (CFFILE.ExternalAttr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
                  Begin
                     If ShowEmptyDirs Then doRead(Infile);
                  End
                  Else
                  Begin
                     Inc(Count);
                     IncTotals(CFFILE.UnpackedSize, CFFILE.UnpackedSize);
                     doRead(Infile);
                  End;
               End;
            End;
         End;

      atPak,
         atPakExe:

         With PakHeader Do
         Begin

            If (marker = 26) Then
            Begin

               If (CompressType = 0) Then (* signals EOF for header reads *)
                  fFilePos := fOffsetEnd 	(* Force exit of read loop *)
               Else
               Begin

                  FileName := Copy(aFilename, 1, Pos(#0, aFilename) - 1);

                  If Assigned(OnReadFile) Then
                     OnReadFile(Self, fFileName);

                  If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
                  Begin
                     If (CompressType = 30) Then
                     Begin
                        If ShowEmptyDirs Then doRead(Infile);
                     End
                     Else
                     Begin
                        Inc(Count);
                        IncTotals(PackedSize, UnpackedSize);
                        doRead(Infile);
                     End;
                  End;

                  fFilePos := fFilePos + PackedSize;
               End;
            End
            Else
               fFilePos := fOffsetEnd;

         End;

      atHA:

         With HAHeader Do
         Begin

            Path := ReadFilename_NullTerminate(Infile, fFilePos);
            CompressedFileName := ReadFilename_NullTerminate(Infile, fFilePos);
            FileName := CompressedFileName;

            If Assigned(OnReadFile) Then
               OnReadFile(Self, fFileName);

            If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
            Begin
               Inc(Count);
               IncTotals(PackedSize, UnpackedSize);
               doRead(Infile);
            End;
            fFilePos := fFilePos + HAHeader.PackedSize + 2;
         End;

      atLha,
         atLzh,
         atLhaExe,
         atLzhExe:

         With LzhHeader Do
         Begin
            (* signals EOF for header reads *)
            If (HeadLen = 0) Then
               fFilePos := fOffsetEnd (* Force exit of read loop *)
            Else
            Begin

               Case level Of
                  0, 1:
                     Begin
                        FileName :=
                           ReadFilename_DefinedLen(
                              Infile,
                              fFilePos,
                              FileNameLen);

                     	Infile.Position := fFilePos;
                        Infile.Read(CRC16, 5);
                     End;            (* 0,1 *)
                  2:
                     Begin
                        Infile.Position := fFilePos - 1;
                        Infile.Read(CRC16, SizeOf(CRC16));
                        Infile.Read(OS, SizeOf(OS));
                        Infile.Read(FileNameLen, SizeOf(FileNameLen));
                        Inc(fFilePos, 5);
                        //InFile.Read( ExtHeaderSize, SizeOf(ExtHeaderSize) );

                        FileName :=
                           ReadFilename_DefinedLen(
                              Infile,
                              fFilePos,
                              FileNameLen - 3);

                        Infile.Position := fFilePos;
                        Infile.Read(ExtHeaderSize, SizeOf(ExtHeaderSize));
                     End;            (* 2 *)
               Else
                  Begin
                     fFilePos := fOffsetEnd;
                     Exit;
                  End;
               End;

               If Assigned(OnReadFile) Then
                  OnReadFile(Self, fFileName);

               GetMem(s, 256);
               Try
                  While (ExtHeaderSize > 0) And (ExtHeaderSize < 256) Do
                  Begin
                     Infile.Read(s^, ExtHeaderSize);
                     Case Byte(s^) Of
                        0: ;
                        1: ;
                        2:
                           Begin
                              Dir := StrPas(s);
                              ExtHeader := DecodeDir(Dir);
                              FileName := AppendDirTail(ExtHeader) + fFileName;
                           End;
                        $40: ;
                     End;

                     pWord := @s[ExtHeaderSize - 2];
                     ExtHeaderSize := pWord^;
                  End;
               Finally
                  FreeMem(s, 256);
               End;

               If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
               Begin
                  If (CompressType = 100) Then
                  Begin
                     If ShowEmptyDirs Then doRead(Infile);
                  End
                  Else
                  Begin
                     Inc(Count);
                     IncTotals(PackedSize, UnpackedSize);
                     doRead(Infile);
                  End;
               End;

               Case level Of
                  0, 1:
                  	fOffsetStart := fOffsetStart + HeadLen + PackedSize + 2;
                  2: fOffsetStart := fOffsetStart + HeadLen + PackedSize;
               Else
                  fOffsetStart := fOffsetEnd;
               End;

               fFilePos := fOffsetStart;

            End;
         End;

      atRar,
         atRarExe:

         With RarHeader Do
         Begin

            If (HeadType = COMM_HEAD) Then	// skip comment header
               fFilePos := fFilePos - fRecordSize + HeadSize
            Else
               If ((HeadFlags And LONG_BLOCK) <> LONG_BLOCK) Or
                  (HeadSize = 0) Or Cancel Then

                  (* Force exit of read loop *)
                  fFilePos := fOffsetEnd

               Else
                  If ((HeadFlags And SKIP_IF_UNKNOWN) = SKIP_IF_UNKNOWN) Or
                  (HeadType <> FILE_HEAD) Then

                     (* ---------------- *)
                     (* skip this header *)
                     (* ---------------- *)
                     fFilePos := fFilePos - fRecordSize + HeadSize + PackedSize

                  Else Begin


                  	If HeadFlags And LHD_LARGE > 0 Then
                     Begin
								inStream.Position := fFilePos;
                     	inStream.Read(RarHuge, SizeOf(TRarHugeFile));
                        fFilePos := fFilePos + SizeOf(TRarHugeFile);
                     End Else
                     	ZeroMemory(@RarHuge, SizeOf(TRarHugeFile));

            			If (HeadFlags And LHD_UNICODE) = 0 Then
                     	// formatted filename
                        FileName :=
                           ReadFilename_DefinedLen(Infile, fFilePos, FileNameLen)
                     Else Begin
                     	// non-formatted filename
                        fFileName :=
                           ReadFilename_DefinedLen(Infile, fFilePos, FileNameLen);

								//fFileName := OemToCharFilter(fFilename[FileNameLen], True{fTransOemChar});

                        // turn OEM translation off... filename is unicoded.
                        TranslateOem := TranslateOemChar;
                        TranslateOemChar := False;
                        Try
                        	FileName := DecodeRarFileName(fFileName, FileNameLen);
                        Finally
      							TranslateOemChar := TranslateOem;
                        End;
                     End;


                     If Assigned(OnReadFile) Then
                        OnReadFile(Self, fFileName);

							Vol_UnpackedSize := ((Int64(RarHuge.HighUnpackedSize) Shl 32) Or UnpackedSize);
                     // v3.51.22 added the following block for UXIX support
                     If RarHeader.HostOS = word(ord(osUnix)) Then
                        If (RarHeader.ExternalAttr And $4000 = 0) Then (* Unix Dir attr *)
                           ExternalAttr := 32
                        Else
                           ExternalAttr := FILE_ATTRIBUTE_DIRECTORY
                     Else
                        ExternalAttr := RarHeader.ExternalAttr;

                     If (ExternalAttr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
                        fFileName := AppendDirTail(fFileName)
                     Else
                        If (HeadFlags And LHD_SPLIT_BEFORE > 0) And
                           (HeadFlags And LHD_SPLIT_AFTER = 0) Then

                        Else
                           If (HeadFlags And LHD_SPLIT_AFTER = LHD_SPLIT_AFTER) Then
                           Begin

                              If Not doGetNextVolume(Infile, fVolumeName) Then
                              Begin
                                 Cancel := True;
                                 If (Count <> 0) Then Exit;
                              End;

                              fLOF := Infile.size;
                              fFilePos := 0;
                              fOffsetEnd := fLOF;

                              Infile.Position := 0 + 7;
                              BytesRead := Infile.Read(Rar1Header, 13);
                              If (BytesRead <> 13) Then
                                 RaiseError(E_RAISE, fArchiveFile, '', '0', E_FREAD);

                              fFilePos := Infile.Position;
                              fOffsetStart := fFilePos;
                           End;

                     If (HeadFlags And LHD_SPLIT_BEFORE > 0) Then
								Vol_PackedSize := Vol_PackedSize +
                        	((Int64(RarHuge.HighPackedSize) Shl 32) Or PackedSize)
                     Else
                        Vol_PackedSize := ((Int64(RarHuge.HighPackedSize) Shl 32) Or PackedSize);

                     If (HeadFlags And LHD_SPLIT_AFTER = 0) Or Cancel Then
                     Begin

                        If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
                           If (ExternalAttr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
                           Begin
                              If ShowEmptyDirs Then
                                 doRead(Infile);
                           End
                           Else
                           Begin
                              Inc(Count);
                              IncTotals(Vol_PackedSize, ((Int64(RarHuge.HighUnpackedSize) Shl 32) Or UnpackedSize));
                              doRead(Infile);
                           End;

                        fFilePos :=  fFilePos - fRecordSize - FilenameLen +
                        	HeadSize + ((Int64(RarHuge.HighPackedSize) Shl 32) Or PackedSize);

                     End;

                  End;
         End;

      atTar:

         //If ValidateTarHeader(pRecord) Then
            With TarHeader Do
            Begin

               FileName := UnixToDosFilename(StrPas(@TarFilename));
               If (FileName = '') Or
                  (OctStrToInt(AnsiString(ChkSum)) = 0) Then
               Begin
                  fFilePos := Infile.Position + OctStrToInt(AnsiString(size));
                  //fFilePos := fOffsetEnd;
                  Exit;
               End;

               If Assigned(OnReadFile) Then
                  OnReadFile(Self, fFileName);

               If CheckWildCard1(RemoveDirTail(fFileName), FileSpec, ExcludeSpec) Then
               Begin
                  Case LinkFlag Of
                     LF_SYMLINK:
                        Begin
                           IncTotals(PackedSize, UnpackedSize);
                           Inc(Count);
                           doRead(Infile);
                        End;

                     LF_DIR:
                        If ShowEmptyDirs Then
                        Begin
                           Inc(Count);
                           doRead(Infile);
                        End;

                     LF_NORMAL,
                        LF_OLDNORMAL,
                        LF_LINK:
                        Begin
                           IncTotals(PackedSize, UnpackedSize);
                           Inc(Count);
                           doRead(Infile);
                        End;

                  Else
                     //ShowMessage(FileName);
                  End (* CASE *);

               End;

               fFilePos := Infile.Position + OctStrToInt(AnsiString(size));
               If (fFilePos Mod 512 <> 0) Then
                  fFilePos := ((fFilePos + 511) Div 512) * 512;

            End;
         //Else
         //   fFilePos := fOffsetEnd;

      atGZip:

         With GZipHeader Do
         Begin
            If IsGZipSignAttr(SignAtr) Then
            Begin

               If ((SignAtr = GZIP_MAGIC) Or
                  (SignAtr = OLD_GZIP_MAGIC)) And
                  ((BitFlag And EXTRA_FIELD) = EXTRA_FIELD) Then
               Begin
                  Infile.Read(ExtFieldLen, SizeOf(ExtFieldLen));
                  Infile.Position := GZipHdr_Size;
                  GlobalDate := UnixDateToDos(FileDate);
               End
               Else
               Begin
                  ExtFieldLen := 0;
                  fCRC := 0;
                  fEncrypted := False;
                  fPackedSize := fLOF - GZipHdr_Size;
                  fUnpackedSize := fPackedSize;

                  If (SignAtr = LZW_MAGIC) Then
                     GlobalDate := ConvertDate(FileAge(fArchiveFile))
                  Else
                     GlobalDate := UnixDateToDos(FileDate);
               End;


               If ((SignAtr = GZIP_MAGIC) Or
                  (SignAtr = OLD_GZIP_MAGIC)) And
                  ((BitFlag And ORIG_NAME) = ORIG_NAME) Then
               Begin
                  GetMem(s, 256);
                  Try
                     BytesRead := Infile.Read(s^, 255);
                     s[BytesRead] := #0;
                     fFileName := StrPas(s);
                  Finally
                     FreeMem(s, 256);
                  End;
               End
               Else
               Begin
                  fFileName := ExtractFilename(fArchiveFile);
                  fFileName := Copy(fFileName, 1, ExtractFilenameOnly(PChar(fFileName)));
                  //If ( ( GZipHeader.SignAtr AND ASCII_FLAG ) = ASCII_FLAG  ) And
                  //		( Pos( '.', fFilename ) = 0 )THEN
                  //	fFilename := fFilename + '.txt';
               End;


               If (SignAtr = GZIP_MAGIC) Or
                  (SignAtr = OLD_GZIP_MAGIC) Then
               Begin
                  Infile.Position := Infile.size - (2 * SizeOf(Integer));
                  Infile.Read(i, SizeOf(Integer));
                  If (i = GZIP_HEADER_SIGNATURE) Then
                  Begin
                     Infile.Read(FileNameLen, SizeOf(Integer));

                     fFilePos :=
                     	fLOF - Cardinal(2 * SizeOf(Integer) + FileNameLen);
                     fFileName :=
                        ReadFilename_DefinedLen(Infile, fFilePos, FileNameLen);
                     Infile.Position :=
                        Integer(fLOF) - (4 * SizeOf(Integer)) + FileNameLen;

                     Infile.Read(fCRC, SizeOf(fCRC));
                     Inc(ExtFieldLen, FileNameLen + (2 * SizeOf(Integer)));

                     Infile.Read(fUnpackedSize, SizeOf(Integer));
                  End
                  Else
                     fCRC := i;

                  Infile.Read(fUnpackedSize, SizeOf(Integer));
                  fEncrypted := (BitFlag And gzENCRYPTED) = gzENCRYPTED;

                  (* Sub 8 - CRC & UnpackedSize stored
                     at end of archive *)
                  fPackedSize := fLOF - ExtFieldLen - 8;
               End;

               If (fPackedSize > fUnpackedSize) Then
                  fUnpackedSize := fPackedSize;

               Inc(Count);

               If Assigned(OnReadFile) Then
                  OnReadFile(Self, fFileName);

               If Assigned(OnRead) Then
               Begin
                  fExternalAttr := GetFileAttributes(PChar(fArchiveFile));
                  If (fExternalAttr = FILE_ATTRIBUTE_NORMAL) Then
                     fExternalAttr := 0;

                  fwCompressType := CompressType; (* SignAtr is the CompressType with GZip *)
                  fsCompressType := GetCompressMethodStr(CompressType, BitFlag);

                  IncTotals(PackedSize, UnpackedSize);
                  fRatio := CalcRatio(PackedSize, UnpackedSize);

                  fFileName := OemToCharFilter(fFileName, fTransOemChar);
                  Case ztvOperation Of
                     opRead,
                     	opQueryFile: doEvents(fOffsetStart, Count);
                     opFileOnly: fFilesList.Add(fFileName);
                  End;
               End;

               fFilePos := Infile.Position + fPackedSize;

            End
            Else
               fFilePos := fOffsetEnd;

         End;

      atMsGZ:

         With MSGZ Do
         Begin

         	fFileName :=
            	ReadFilename_DefinedLen(Infile, fFilePos, FileNameLen);

            If Assigned(OnReadFile) Then
               OnReadFile(Self, fFileName);

            If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
            Begin
               Inc(Count);
               IncTotals(PackedSize, UnpackedSize);
               doRead(Infile);
            End;

            fFilePos := fFilePos - SizeOf(TMSGZ) - FileNameLen + HeaderLen;
         End;

      atJar,
         atJarExe,
         atZip..atZipMV:
         //atZip,
         //atZipMV,
         //atZipExe:

         Begin

            If (htCentral In HeaderTypeState) Then
            Begin
               If Not (VerSig(CentralZipHeader.SignAtr, htCentral, Encrypted) = htCentral) Then
               Begin
                  If (Count = 0) Then
                     RaiseErrorStr(fArchiveFile, '', '0', E_BADHEADR);

                  fFilePos := Infile.size; (* Force exit of read loop *)
                  Exit;
               End;

               If Encrypted Then
               Begin
                  DecodeHeader(@CentralZipHeader, htCentral);
                  fFileName :=
                     ReadFilename_DefinedLen(
                        Infile,
                        fFilePos,
                        CentralZipHeader.ZC.FileNameLen);
                  DecodeFilename(@fFileName[1], CentralZipHeader.zc.FilenameLen);
               End Else
                  fFileName :=
                     ReadFilename_DefinedLen(
                        Infile,
                        fFilePos,
                        CentralZipHeader.ZC.FileNameLen);

               If CentralZipHeader.ExternalAttr And FILE_ATTRIBUTE_DIRECTORY = 0 Then
                  FileName := fFileName
               Else
               	fFileName := UnixToDOSFileName(fFileName);

               Central64Hdr :=
               	Read64BitFieldHdr(
                  	Infile,
                     CentralZipHeader.zc.ExtraFieldLen,
                     htCentral);

               //Test := Pointer(Integer(@Central64Hdr) + 2);
               //Test1 := Pointer(Integer(@Central64Hdr) + 2 + 8);

               {If zc.ExtraFieldLen > 0 Then
               Begin
                  GetMem(EFL, zc.ExtraFieldLen);
                  Try
                     //Infile.
                  Finally
                     FreeMem(EFL);
                  End;
               End;}

            End Else Begin
               If HeaderTypeState = [htLocal] Then
               Begin
                  If Not (VerSig(LocalZipHeader.SignAtr, htLocal, Encrypted) = htLocal) Then
                  Begin
                     If (Count = 0) Then
                        RaiseErrorStr(fArchiveFile, '', '0', E_BADHEADR);

                     fFilePos := Infile.size; (* Force exit of read loop *)
                     Exit;
                  End;

                  If Encrypted Then
                  Begin
                     DecodeHeader(@LocalZipHeader, htLocal);
                     fFileName :=
                        ReadFilename_DefinedLen(
                           Infile,
                           fFilePos,
                           LocalZipHeader.ZC.FileNameLen);

                     DecodeFilename(@fFileName[1], LocalZipHeader.zc.FilenameLen);
                  End Else
                     fFileName :=
                        ReadFilename_DefinedLen(
                           Infile,
                           fFilePos,
                           LocalZipHeader.ZC.FileNameLen);

                  // ExternalAttr field not present in Local header, just
                  // assign filename (see central-header assignment of
                  // fFilename above).
                  FileName := fFileName;

            		Central64Hdr :=
                  	Read64BitFieldHdr(
                     	Infile,
                        LocalZipHeader.zc.ExtraFieldLen,
                        htLocal);
               End Else Begin
                  fFilePos := Infile.size; (* Force exit of read loop *)
                  Exit;
               End
            End;

            If Assigned(OnReadFile) Then
               OnReadFile(Self, fFileName);

            //With Central64Hdr.Offset Do
            If (HeaderTypeState = [htLocal]) Then
               With LocalZipHeader Do
               Begin
                  If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
                  Begin
                     Inc(Count);
                     IncTotals(
                        (Int64(Central64Hdr.HiPackedSize) Shl 32) Or zc.PackedSize,
                        (Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or zc.UnpackedSize);
                     doRead(Infile);
                  End;

                  fFilePos :=
                  	((Int64(Central64Hdr.HiPackedSize) Shl 32) Or zc.PackedSize) +
                     zc.ExtraFieldLen +
                  	fFilePos;
               End
            Else
               With CentralZipHeader Do
               Begin

                  If (ArcType <> atZipDS) And
                        (ArcType <> atZipMV) And
                        (Not (htLocal In HeaderTypeState)) Then
                     RaiseErrorStr(fFileName, '', '0', E_BADHEADR);

                  If (CommentLen > 0) Then
                     GetComment(FileComment, CommentLen);

                  Attr := LoWord(ExternalAttr);

                  If CheckWildCard2(fFileName, FileSpec, ExcludeSpec, RecurseDirs) Then
                  Begin
                     If (Attr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
                     Begin
                        If ShowEmptyDirs Then
                        Begin
                           Inc(Count);
                           doRead(Infile);
                        End;
                     End
                     Else
                     Begin
                        Inc(Count);

                        IncTotals(
                           (Int64(Central64Hdr.HiPackedSize) Shl 32) Or zc.PackedSize,
                           (Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or zc.UnpackedSize);

                        doRead(Infile);
                     End;
                  End;

                  fFilePos := fFilePos + zc.ExtraFieldLen;
               End;
         End;

      atZoo:

         If (Count = 0) Then
         Begin

            With ZooHeader Do
            Begin
               If (START + Minus <> 0) Then
               Begin
                  fFilePos := fLOF;
                  Exit;
               End;

               If (CommentLen > 0) Then
               Begin
                  (* ZOO places comments at the end of the archive *)
                  Infile.Position := CommentPos;
                  GetComment(FileComment, CommentLen);
               End;

               fFilePos := START;
            End;

            pRecord := @ZooDirHeader;
            fRecordSize := ZooDirHdr_Size;
            Inc(Count);

         End
         Else
         Begin


            With ZooDirHeader Do
            Begin

               If (Next < 1) Then
               Begin
                  fFilePos := fLOF;
                  Exit;
               End;

               If (CommentLen > 0) Then
               Begin
                  (* ZOO places comments at the end of the archive *)
                  Infile.Position := CommentPos;
                  GetComment(FileComment, CommentLen);
               End;

               // v4.01.02: rem'd.. see 1.zoo.  No stored path support
               //If ( ZooDirHeader.DirLen > 0 ) Then
               //   fFilename :=
               //		AppendDirTail(
               //      	UnixToDosFilename(
               //         	ReadFilename_DefinedLen(
               //            	Infile,
               //               fFilePos,
               //               DirLen - 1 ) ) )
               //Else
               	fFileName := '';

               fFileName := fFileName + StrPas(@ZooFilename[1]);
               fFileName := OemToCharFilter(fFileName, fTransOemChar);

               If Assigned(OnReadFile) Then
                  OnReadFile(Self, fFileName);

               If CheckWildCard1(fFileName, FileSpec, ExcludeSpec) Then
               Begin
                  Inc(Count);
                  IncTotals(PackedSize, UnpackedSize);
                  doRead(Infile);
               End;

               If (Next > 0) Then
                  fFilePos := Next
               Else
                  fFilePos := fLOF;  (* Force exit *)

            End;
         End;

   Else
      fFilePos := fOffsetEnd;
   End;

End;
//-------------------------------------------------------------

(*
 Example Call:

 Var
 	CommentLen: Word;

 If IsArcValid( ArcType ) Then
 Begin
    GetMem( test, 32000 );
    Try
       CommentLen := GetCurrentArchiveComment( Infile, test );
       If CommentLen > 0 Then
       Begin
         ... process the comment here
       End;
    Finally
      FreeMem( test );
    End;
 End;

 - For achives already opened.  To retrieve an archive's comment from closed
   files, see the GetArchiveComment method.
 - Function returns the length of the archive's comment

*)
Function TZipTV.GetCurrentArchiveComment(Infile: TFileStream32; p: PChar): Integer;
Var
   BytesRead: DWord;
	ResetPos: Cardinal;
Begin
   Result := 0;
   Case ArcType Of
      atJar,
         atJarExe,
         atZip..atZipMV:
         //atZip,
         //atZipMV,
         //atZipExe:
         	With EndZipHeader Do
         	Begin
            	ResetPos := Infile.Position;
               Try
                  If (CommentLen > 0) And (CommentLen < word(High(smallint))) And
                   	(ArchiveCommentPos > 0) And (ArchiveCommentPos < FLOF)Then
                  Begin
                     Infile.Position := ArchiveCommentPos;
                     BytesRead := Infile.Read(p^, CommentLen);
                     If (BytesRead > 0) Then
                        Result := CommentLen;
                  End;
               Finally
               	Infile.Position := ResetPos;
               End;
            End;
   End;
End;
//-------------------------------------------------------------

(*
 Example Call:

 Procedure GetZipMainComment;
 Var
 	CommentLen: Word;
   ZipTV2: TZipTV;
 Begin
   ZipTV2 := TZipTV.Create(Nil);
   Try
      GetMem( test, 32000 );
      Try
         With ZipTV2 Do
         Begin
            ZipTV2.ArchiveFile := 'd:\3\zip\cmnts.zip';  //archive to open
            If IsArcValid(ZipTV2.ArcType) Then
            Begin
               CommentLen := ZipTV2.GetArchiveComment( test );
               If CommentLen > 0 Then
               Begin
                 ... process the comment here
               End;
            End;
         End;
      Finally
        FreeMem( test );
      End;
   Finally
   	ZipTV2.Destroy();
   End;
 End;

 - Opens and closes the archive defined in the ArchiveFile property.
   For achives already opened, use the GetCurrentArchiveComment method.
 - Opens the archive, calls GetCurrentArchiveComment, then closes the archive.
 - Function returns the length of the archive's comment

*)
Function TZipTV.GetArchiveComment(p: PChar): Integer;
Var
   s: TFileStream32;
Begin
   Result := 0;

   s :=
   	TFileStream32.Create(
      	fArchiveFile, fmOpenRead Or fmShareDenyNone);

   If (s.Handle < 0) Then
   Begin
      RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN);
      s.Free();
      Exit;
   End;

   Try
      Case ArcType Of
         atJar,
            atJarExe,
         	atZip..atZipMV:
            //atZip,
            //atZipMV,
            //atZipExe:
               With EndZipHeader Do
                  If (GetCentralDirOffset(s) > 0) And
                        (CommentLen > 0) And
                        (CommentLen < word(High(smallint))) Then
                     Result := GetCurrentArchiveComment(s, p);
      End;

   Finally
   	s.Free();
   End
End;
//-------------------------------------------------------------

{Procedure TZipTV.OpenInfile(Var Instream: TFileStream32);
Begin
	inStream :=
   	TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyWrite);
End;}

//-------------------------------------------------------------
{$ifndef DEL6_OR_HIGHER}
Function TZipTV.Activate(Source: TStream): Boolean;
Var
   Dest: TStream32;
Begin
   Dest := TMemoryStream32.Create();
   Try
      Source.Position := 0;
      TStreamToZTVStream(Source, Dest);
      Result := Activate(Dest);
   Finally
      Dest.Free();
   End;
End;
{$endif}
//-------------------------------------------------------------


Function TZipTV.Activate(Instream: TStream32): Boolean;
Const
   ARJ_FNAME_MAX = 512;
   ARJ_COMMENT_MAX = 2048;
   ARJ_FIRST_HDR_SIZE = 30;
   ARJ_HEADERSIZE_MAX = ARJ_FIRST_HDR_SIZE + 10 + ARJ_FNAME_MAX + ARJ_COMMENT_MAX;
Var
   BytesRead: DWord;
Begin
	If Instream = Nil Then Exit;

   Cancel := False;
   fVolumeName := fArchiveFile;
   DoProgress := Nil;
   ExcludeSpec.Clear();
   fArcType := GetArcType(inStream);

   Try

      Result := True;

      If (ArcType In [atArj, atArjExe]) Then
         GetMem(Buffer, ARJ_HEADERSIZE_MAX)
      Else
         Buffer := Nil;

    	GetMem(FileComment, WSIZE);
      Try

         Try
            If Assigned(OnActivate) Then OnActivate(Self);
            ZipTimer.START();
            Setup(inStream);              (* initialize beginning variables *)

            fFilePos := fOffsetStart;
            While (Not pCancel^) And (inStream <> Nil) And (fFilePos + fRecordSize < fLOF) And (fRecordSize > 0) Do
            Begin
            	fOffsetStart := fFilePos;
               If (fFilePos + fRecordSize > fLOF) Then break;
               inStream.Position := fFilePos;
               BytesRead := inStream.Read(pRecord^, fRecordSize);
               If (BytesRead < 1) Then break;
               Inc(fFilePos, BytesRead);
               doProcRecord(inStream);
            End;
         Finally
         	ZipTimer.Stop();

            If Assigned(FOnTotals) Then
            Begin
               fTotalRatio :=
                  CalcRatio(fTotalPackedSize, fTotalUnpackedSize);

               OnTotals(Self, fTotalUnpackedSize, fTotalPackedSize,
                  fTotalRatio, Count);
            End;

            If Assigned(OnDeactivate) Then OnDeactivate(Self);
            If Assigned(OnElapsedTime) Then
               OnElapsedTime(Self, ZipTimer.ElapsedTime);

            // must follow OnDeactivate so the Archive Comment can be retrieved
            // using the inStream file variable.
         End;

      Finally
      	FreeMem(FileComment, WSIZE);

         If (Buffer <> Nil) Then
            FreeMem(Buffer, ARJ_HEADERSIZE_MAX);

      End;
   Except
      Result := False; //On EControlC Do ;
   End;
End;
//-------------------------------------------------------------

Function TZipTV.Activate: Boolean;
Const
   ARJ_FNAME_MAX = 512;
   ARJ_COMMENT_MAX = 2048;
   ARJ_FIRST_HDR_SIZE = 30;
   ARJ_HEADERSIZE_MAX = ARJ_FIRST_HDR_SIZE + 10 + ARJ_FNAME_MAX + ARJ_COMMENT_MAX;
Var
   BytesRead: DWord;
Begin
   If Waiting Then Exit;
   Waiting := True;

   Try
      Cancel := False;
      fVolumeName := fArchiveFile;
      DoProgress := Nil;
      ExcludeSpec.Clear();

      Try

         If (fArchiveFile = '') Or (Not FileExists(fArchiveFile)) Then
            RaiseError(E_RAISE, fArchiveFile, '', '0', E_FILENOTFOUND)
         Else
            If (Not Assigned(OnRead)) Then
               RaiseError(E_RAISE, fArchiveFile, 'OnRead', '0', E_REQUIREDEVENT);

         Result := True;

         If (ArcType In [atArj, atCab, atCabExe]) And (Not FileExists(fArchiveFile)) Then
            If (Not GetNextVolume(fVolumeName, IntToStr(fVolNum))) Then
               Exit
            Else
               fArchiveFile := fVolumeName;

         If (ArcType In [atArj, atArjExe]) Then
            GetMem(Buffer, ARJ_HEADERSIZE_MAX)
         Else
            Buffer := Nil;

         If ((ArcType = atZip) And (fOffsetStart = 0)) Then
         Begin
            If Not DiskManager.GetDriveInfo(
                           fArchiveFile,
                           RaiseError,
                           RaiseErrorStr,
                           OnDiskWriteProtectErr,
                           OnDiskInDrvErr) Then
               Exit;

            If DiskManager.DriveType = dtFloppy Then
               fArcType := atZipDS;
         End;

         //inStream := Nil;
         GetMem(FileComment, WSIZE);
         Try

            inStream :=
               TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyWrite);

            //OpenInfile(inStream);
            If (TFileStream(inStream).Handle < 0) Then
            Begin
               RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN);
               Exit;
            End;

            Try

               If Assigned(OnActivate) Then OnActivate(Self);
               ZipTimer.START();
               Setup(inStream);              (* initialize beginning variables *)

               fFilePos := fOffsetStart;
               While (Not pCancel^) And (inStream <> Nil) And (fFilePos + fRecordSize < fLOF) And (fRecordSize > 0) Do
               Begin
                  fOffsetStart := fFilePos;
                  If (fFilePos + fRecordSize > fLOF) Then break;
                  inStream.Position := fFilePos;
                  BytesRead := inStream.Read(pRecord^, fRecordSize);
                  If (BytesRead < 1) Then break;
                  Inc(fFilePos, BytesRead);
                  doProcRecord(inStream);
               End;
            Finally
               ZipTimer.Stop();
               inStream.Free();

               If Assigned(FOnTotals) Then
               Begin
                  fTotalRatio :=
                     CalcRatio(fTotalPackedSize, fTotalUnpackedSize);

                  OnTotals(Self, fTotalUnpackedSize, fTotalPackedSize,
                     fTotalRatio, Count);
               End;

               If Assigned(OnDeactivate) Then OnDeactivate(Self);
               If Assigned(OnElapsedTime) Then
                  OnElapsedTime(Self, ZipTimer.ElapsedTime);

               // must follow OnDeactivate so the Archive Comment can be retrieved
               // using the inStream file variable.
            End;

         Finally
            FreeMem(FileComment, WSIZE);

            If (Buffer <> Nil) Then
               FreeMem(Buffer, ARJ_HEADERSIZE_MAX);

         End;
      Except
         Result := False; //On EControlC Do ;
      End;
   Finally
   	Waiting := False;
   End;
End;
{ -------------------------------------------------------------- }

End.
