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
Unit ztvUnLha;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   ztvRegister,
   ztvBase,
   ztvHeaders,
   ztvStreams;


{$I ZipTV.inc}                          //Declare the compiler defines

{ ----------------------------------------------------------- }
Type
   TUnLha = Class(TUnBASE)
   Private
      Function BuildHeadArray(Infile: TStream32): Integer;
      Function GetRemainingHeaderInfo(Infile: TStream32): Boolean;
      Function OpenAndExtractFile(Infile: TStream32; Outfile: TStream32; FileAttr: Integer): Boolean;
      Procedure ProcessHeaders(Infile, Outfile: TStream32);
   Protected
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure ExtractIT(Var Infile: TStream32; Outfile: TStream32); Override;
   Published
      Property ArcType;
      Property ConfirmOverwrites;
      Property CreateStoredDirs;
      Property DateAttribute;
      Property ExtractDir;
      Property FileSpec;
      Property OverwriteMode;
      Property RestoreFileAttr;
      Property UseStoredDirs;
      Property OnActivate;
      Property OnDeactivate;
      Property OnProgress;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnRenameFile;
   End;

Implementation

Uses
   ztvGbls,
   ztvLhaPack,
   ztvLzh3,
   Err_Msgs;

Const
   DASH = 45;


//-------------------------------------------------------------

Constructor TUnLha.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TUnLha.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TUnLha.GetRemainingHeaderInfo(Infile: TStream32): Boolean;
Var
   Dir: String;
   pWord: ^word;
   pFilename: PChar;
   BytesRead: Integer;
Begin

   Result := False;

   With LzhHeader Do
   Try
      If HeadLen >= LZHHdr_Size Then
      Begin
         GetMem(pFilename, 256);
         Try
            Case level Of
               0, 1:
                  Begin
                     ReadFilename(Infile, pFilename, FileNameLen);
                     ReadBlock(Infile, Nil, CRC16, False, 0,
                        HeadLen - LZHHdr_Size - FileNameLen + SizeOf(CRC16),
                        dtHeader);
                  End;
               2:
                  Begin
                     //ztvSetFilePointer(f, ztvGetFilePos(f) - 1, FILE_BEGIN);
         				Infile.Seek(-1, soCurrent);

                     ReadBlock(Infile, Nil, CRC16, False, 0, SizeOf(CRC16), dtHeader);
                     ReadBlock(Infile, Nil, OS, False, 0, SizeOf(OS), dtHeader);
                     ReadBlock(Infile, Nil, FileNameLen, False, 0, SizeOf(FileNameLen), dtHeader);

         				Infile.Seek(2, soCurrent);
                     ReadFilename(Infile, pFilename, FileNameLen - 3);
                     ReadBlock(Infile, Nil, ExtHeaderSize, False, 0, SizeOf(ExtHeaderSize), dtHeader);
                  End;

            Else
               Exit;
            End;

            ActualFilename := StrPas(pFilename);

            ZeroMemory(pFilename, 256);
            While (ExtHeaderSize > 0) And (ExtHeaderSize < 256) Do
            Begin
               BytesRead :=
               	ReadBlock(Infile, Nil, pFilename^, False, 0,
                  	ExtHeaderSize, dtHeader);

               If BytesRead = 0 Then break;
               Case Byte(pFilename^) Of
                  0: ;
                  1: ;
                  2:
                     Begin
                        Dir := StrPas(pFilename);
                        ExtHeader := DecodeDir(Dir);
                        ActualFilename := AppendDirTail(ExtHeader) + ActualFilename;
                     End;
                  $40: ;
               End;

               pWord := @pFilename[ExtHeaderSize - 2];
               ExtHeaderSize := pWord^;
            End;

         Finally
            FreeMem(pFilename, 256);
         End;

         Result := True;

      End
      Else
         RaiseErrorStr(ArchiveFile, '', '0', E_BADHEADR);
   Except
   End;
End;
//-------------------------------------------------------------

Function TUnLha.OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
Var
   bPBit: Byte;
   bDicBit: Byte;
   bBitSize: Byte;
   bCompressBit: Byte;
Begin

   Result := False;
   With LzhHeader Do
      If doOnBegin(CompressType = 100) Then
      Begin
         Try
            Try
               Crc16Val := 0;
               Crc32Val := CRC_MASK;
               Bytes_To_Go := PackedSize;

               If (CompressType = 100) Then
               Begin
                  If WriteToFile() And CreateStoredDirs Then
                     If Not CreateDirEx(fFileName) Then
                        Exit;

                  Crc32Val := CRC16;
               End
               Else
               Begin

                  If Open_OutFile(Outfile, FileName, ActualFilename) Then
                  Begin
                     Try
                        If (SignBegin = DASH) And (SignEnd = DASH) Then
                        Begin

                           bPBit := 0;
                           bDicBit := 0;
                           bBitSize := 0;
                           bCompressBit := 0; //satisfy compiler
                           Bytes_To_Go := PackedSize;

                           Case (CompressType - 48) Of
                              0:
                                 Unstore(Infile, Outfile, 16, '0', InflateRec);

                              1:	//-lh1- 		original format
                                 LZHUnpack(Self, Infile, Outfile, Nil, InflateRec);

                              4:	//-lh4-    	frozen4
                                 Begin
                                    bPBit := 16;
                                    bDicBit := 12;
                                    bBitSize := 4;
                                    bCompressBit := 3;
                                 End;

                              5:	//-lh5-    	frozen5
                                 Begin
                                    bPBit := 16;
                                    bDicBit := 13;
                                    bBitSize := 4;
                                    bCompressBit := 3;
                                 End;

                              6:	//-lh6-    	frozen6
                                 Begin
                                    bPBit := 16;
                                    bDicBit := 15;
                                    bBitSize := 5;
                                    bCompressBit := 5;
                                 End;

                              (* Not yet supported *)
                              7:	//-lh7-    frozen7
                                 BEGIN
                                    bPBit := 16; //32;
                                    bDicBit := 16;
                                    bBitSize := 5;
                                    bCompressBit := 6;
                                 END;

                              52:	//-lhd- dir
                                 If CreateStoredDirs And WriteToFile Then
                                    CreateDirEx(AppendDirTail(ExtractFileDir(fFileName)));
                           Else
                              RaiseError(E_RAISE, fFileName, '', '0', E_UNKNMETH);
                           End;

                           If bPBit > 0 Then
                           Begin
                              lzh_initialize(bPBit, bDicBit, bBitSize, bCompressBit);
                              lzh_decode(Self, Infile, Outfile, InflateRec);
                           End;
                        End
                        Else
                        Begin
                           RaiseErrorStr(fFileName, '', '0', E_UNKNMETH);
                           Exit;
                        End;

                     Finally

                        If OS = LHA_UNIXOS Then
                           //IF Level = 2 THEN
                           CloseAndSetDate(Outfile, fFileName,
                              DateTimeToFileDate(UnixDateToDos(FileDate)),
                              ExternalAttr)
                        Else
                           CloseAndSetDate(Outfile, fFileName,
                              FileDate, ExternalAttr);

                     End;                  // try/finally

                  End
                  Else
                  Begin
                     RaiseErrorStr(fFileName, '', '0', E_FOPEN);
                     AdjustProgress(InflateRec);
                     Dec(Count);
                  End;
               End;

            Except
             //ON e: exception DO ShowMessage( e.message );
            End;
         Finally
         	Result := doOnEnd(16, CRC16)
         End;
      End
      Else
         AdjustProgress(InflateRec);

End;
//-------------------------------------------------------------

Procedure TUnLha.ProcessHeaders(Infile, Outfile: TStream32);
Var
   i, BytesRead: Integer;
Begin
   ProgressPosition := fTotalUnpackedSize;
   For i := 0 To HeaderList.FileCount - 1 Do
      With LzhHeader Do
      Begin
         If Cancel Then break;

         pUBFI := HeaderList.FileLocationData(i);
         With pUBFI^ Do
         Begin
         	Infile.Seek(OffsetOfLocalHeader, soBeginning);

            BytesRead :=
            	ReadBlock(Infile, Nil, LzhHeader, False, 0,
               	LZHHdr_Size, dtHeader);

            If (BytesRead = LZHHdr_Size) And (Not GetRemainingHeaderInfo(Infile)) Then
               break;

            ActualFilename := OemToCharFilter(ActualFilename, fTransOemChar);
            FileName := ActualFilename;

            InflateRec.BitFlag := 0;    //no password protection
            InflateRec.CompressType := CompressType;
            InflateRec.PackedSize := PackedSize;
            InflateRec.UnpackedSize := UnpackedSize;

            // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
            GlobalDate := FileDate;
            OpenAndExtractFile(Infile, Outfile, FileAttr);
         End;
      End;
End;
//-------------------------------------------------------------

Function TUnLha.BuildHeadArray(Infile: TStream32): Integer;
Var
   BytesRead: Integer;
   //PackedMinus: BYTE;
   CurrentPos: u_long;
   UBFI: TUnBaseFileInfo;
Begin

   ZipTimer.Suspend;
   CurrentPos := fOffsetStart;

   With LzhHeader Do
   Try

      ExtHeaderSize := 0;
      OffsetToCompressed := 0;

      Infile.Seek(CurrentPos, soBeginning);
      BytesRead :=
      	ReadBlock(Infile, Nil, LzhHeader, False, 0, LZHHdr_Size, dtHeader);

      (* Adjust for 128 byte master header *)
      If HeadLen = 0 Then
      	Infile.Seek(128, soCurrent);

      While (HeadLen > 0) Do
      Begin
         If Cancel Then break;
         //PackedMinus := 0;

         If (BytesRead = LZHHdr_Size) And (Not GetRemainingHeaderInfo(Infile)) Then
            break;

         (* ActualFilename is assigned in the GetRemainingHeaderInfo procedure call above *)
         ActualFilename := OemToCharFilter(ActualFilename, fTransOemChar);
         FileName := ActualFilename;
         //PackedSize := PackedSize - PackedMinus;

         If CheckWildCard2(ActualFilename, FileSpec, ExcludeSpec, RecurseDirs) Then
         Begin
            With UBFI Do
            Begin
               DiskWithThisFile := 0;
               OffsetOfLocalHeader := CurrentPos;
               FileAttr := Integer(ExternalAttr);
            End;

            //If (CompressType = 100) Or ((CompressType >= 48) And (CompressType <= 54)) Then
            If (CompressType = 100) Or ((CompressType >= 48) And (CompressType <= 55)) Then
            Begin
               HeaderList.AddItem(UBFI, Nil, 0);

               If FileDate > fMaxAge Then
                  fMaxAge := FileDate;

               fTotalPackedSize := fTotalPackedSize + PackedSize;
               fTotalUnpackedSize := fTotalUnpackedSize + UnpackedSize;
            End;
         End;

         Case level Of
            0, 1: CurrentPos := CurrentPos + HeadLen + 2 + PackedSize {+ PackedMinus};
            2: CurrentPos := CurrentPos + HeadLen + PackedSize {+ PackedMinus};
         Else
            inc(CurrentPos, HeadLen {+ PackedMinus});
         End;

         ExtHeaderSize := 0;
         OffsetToCompressed := 0;
      	Infile.Seek(CurrentPos, soBeginning);
         ZeroMemory(@LzhHeader, LZHHdr_Size);
         BytesRead :=
         	ReadBlock(Infile, Nil, LzhHeader, False,
            	0, LZHHdr_Size, dtHeader);
      End;
   Finally
      Result := HeaderList.FileCount;
   End;
End;
//-------------------------------------------------------------

Procedure TUnLha.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
Begin
   Try
      HeaderList := TUnBaseHeaderObj.Create();
      Try
         HeaderList.INIT();
         Try
            If BuildHeadArray(Infile) > 0 Then
               ProcessHeaders(Infile, Outfile);
         Finally
            HeaderList.DONE();
         End;
      Finally
         HeaderList.Free();
      End;
   Except
   End;
End;
//-------------------------------------------------------------

End.
