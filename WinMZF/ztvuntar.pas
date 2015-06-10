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
Unit ztvUnTar;


Interface


Uses
   Windows,
   SysUtils,
   Classes,
   Dialogs,
   ztvRegister,
   ztvBase,
   ztvGbls,
   ztvStreams,
   ztvFileIo;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TUnTar = Class(TUnBASE)
   Private
      Function BuildHeadArray(Infile: TStream32): Integer;
		Function OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
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
   ztvHeaders,
   Err_Msgs;

//-------------------------------------------------------------

Constructor TUnTar.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TUnTar.Destroy;
Begin
   Inherited Destroy();
End;
//-------------------------------------------------------------

Function TUnTar.OpenAndExtractFile(Infile, Outfile: TStream32; FileAttr: Integer): Boolean;
Var
   dt: Integer;
Begin

   Result := False;
   If FileAttr And FILE_ATTRIBUTE_DIRECTORY > 0 Then
   Begin
      If WriteToFile() And CreateStoredDirs Then
         CreateDirEx(fFileName);
   End
   Else

      With TarHeader Do
         If doOnBegin(False) Then
         Begin
            Crc32Val := CRC_MASK;
            Try
               Try
                  If Open_OutFile(Outfile, FileName, ActualFilename) Then
                  Begin
                     Try
                        Try
                           Unstore(Infile, Outfile, 64, '0', InflateRec);
                        Finally
                           dt := DateTimeToFileDate(UnixDateToDos(OctStrToInt(MTime)));
                           CloseAndSetDate(Outfile, FileName, dt, 32);
                        End;
                     Except
                        Crc32Val := 999;   //force crc validation failure result
                     End
                  End Else Begin
                     Crc32Val := 999;   //force crc validation failure result
                     RaiseErrorStr(fFileName, '', '0', E_FOPEN);
                     AdjustProgress(InflateRec);
                     Dec(Count);
                  End;

               Finally
                  Result := doOnEnd(32, Crc32Val);
               End;

            Except
           		//ON e: exception DO ShowMessage( e.message );
            End;

         End
         Else
            AdjustProgress(InflateRec);

End;
//-------------------------------------------------------------

Procedure TUnTar.ProcessHeaders(Infile, Outfile: TStream32);
Var
   i: Integer;
Begin
   ProgressPosition := fTotalUnpackedSize;
   For i := 0 To HeaderList.FileCount - 1 Do
      With TarHeader Do
      Begin
         If Cancel Then break;

         pUBFI := HeaderList.FileLocationData(i);
         With pUBFI^ Do
         Begin
         	Infile.Seek(OffsetOfLocalHeader, soBeginning);

        (* Read Header & Filename *)
            ReadBlock(Infile, Nil, TarHeader, False, 0, SizeOf(TarHeader), dtHeader);

            ActualFilename := StrPas(@TarFilename[0]);
            ActualFilename := UnixToDosFilename(OemToCharFilter(ActualFilename, fTransOemChar));
            FileName := ActualFilename;

            InflateRec.BitFlag := 0;
            InflateRec.CompressType := 0;
            InflateRec.PackedSize := OctStrToInt(size);
            InflateRec.UnpackedSize := InflateRec.PackedSize;

        // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
            GlobalDate := DateTimeToFileDate(UnixDateToDos(OctStrToInt(MTime)));

            OpenAndExtractFile(Infile, Outfile, FileAttr);
         End;
      End;
End;
//-------------------------------------------------------------

Function TUnTar.BuildHeadArray(Infile: TStream32): Integer;
Var
   dt: Integer;
   CurrentPos, ps: u_long;
   UBFI: TUnBaseFileInfo;               //HeaderData
Begin

   ZipTimer.Suspend();
   Try
      CurrentPos := 0;
      Infile.Seek(CurrentPos, soBeginning);

      If ReadBlock(Infile, Nil, TarHeader, False, 0, SizeOf(TarHeader), dtHeader) = SizeOf(TarHeader) Then
         With TarHeader Do
            Repeat
            Begin
               If Cancel Then Exit;

               ps := +OctStrToInt(size);
               ActualFilename := StrPas(@TarFilename[0]);
               ActualFilename := UnixToDosFilename(OemToCharFilter(ActualFilename, fTransOemChar));

               If (ActualFilename = '') Or (OctStrToInt(AnsiString(ChkSum)) = 0) Then
               Begin
                  CurrentPos := Infile.Position + ps;
                  If CurrentPos + SizeOf(TarHeader) >= fLOF Then
                  	Exit;

                  Infile.Seek(CurrentPos, soBeginning);
                  If ReadBlock(Infile, Nil, TarHeader, False, 0, SizeOf(TarHeader), dtHeader) <> SizeOf(TarHeader) Then
                     Exit;

                  Continue;
               End;

               If CheckWildCard2(ActualFilename, FileSpec, ExcludeSpec, RecurseDirs) Then
               Begin

                  With UBFI Do
                  Begin
                     DiskWithThisFile := 0;
                     OffsetOfLocalHeader := CurrentPos;
                     If LinkFlag = LF_DIR Then
                        FileAttr := FILE_ATTRIBUTE_DIRECTORY
                     Else
                        FileAttr := FILE_ATTRIBUTE_NORMAL;

                  End;

                  HeaderList.AddItem(UBFI, Nil, 0);

                  dt := OctStrToInt(MTime);
                  If dt > fMaxAge Then
                     fMaxAge := dt;

                  fTotalPackedSize := fTotalPackedSize + ps;
                  fTotalUnpackedSize := fTotalUnpackedSize + ps;
               End;

               CurrentPos := Infile.Position + ps;
               If ((CurrentPos Mod 512) <> 0) Then
                  CurrentPos := ((CurrentPos + 511) Div 512) * 512;

               Infile.Seek(CurrentPos, soBeginning);

               If ReadBlock(Infile, Nil, TarHeader, False, 0, SizeOf(TarHeader), dtHeader) <> SizeOf(TarHeader) Then
                  Exit;
            End;
            Until False;
   Finally
      Result := HeaderList.FileCount;
      ZipTimer.Resume();
   End;
End;
//-------------------------------------------------------------

Procedure TUnTar.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
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
