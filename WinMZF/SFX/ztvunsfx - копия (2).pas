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
Unit ztvUnSfx;


Interface


Uses
   Windows,
   SysUtils,
   Classes,
   Dialogs,
   ztvRegister,
   ztvBase;

{$I ZipTV.inc}                          //Declare the compiler defines

Type

   TUnSfx = Class(TZipCommon)
   Private
      fOutFilename: String;
      fOnFileExists: TOnFileExists;
		Procedure ConvertStream;
   Protected
      Function GetArchiveFile: String;
      Procedure SetArchiveFile(SFN: String); Override;
      Procedure SetOutFileName(son: String);
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Activate;
   Published
      Property ArchiveFile: String Read GetArchiveFile Write SetArchiveFile Stored False;
      Property OutFilename: String Read fOutFilename Write SetOutFileName;
      Property OnFileExists: TOnFileExists Read fOnFileExists Write fOnFileExists;
      Property OnError;
   End;



Implementation


Uses
   ztvHeaders,
   ztvGbls,
   ztvFileIo,
   ztvStreams,
   Err_Msgs;


//-------------------------------------------------------------

Constructor TUnSfx.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TUnSfx.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TUnSfx.SetOutFileName(son: String);
Var
   sDir: String;
Begin
   If fArchiveFile <> '' Then
   Begin
      sDir := ExtractFileDir(son);
      If sDir = '' Then
         sDir := ExtractFileDir(fArchiveFile);

      If sDir = '' Then
         sDir := GetCurrentDir();

      son := AppendDirTail(sDir) + ExtractFilename(son);
      fOutFilename := son;
   End
   Else
      fOutFilename := '';
End;
//-------------------------------------------------------------

Function TUnSfx.GetArchiveFile: String;
Begin
   Result := fArchiveFile;
End;
//-------------------------------------------------------------

Procedure TUnSfx.SetArchiveFile(SFN: String);
Begin
   fTotalPackedSize := 0;
   fTotalUnpackedSize := 0;

   If SFN <> '' Then
   Begin
      Count := 0;
      SFN := UnixToDosFilename(SFN);
      fOutFilename := SFN;
      fArchiveFile := SFN;              (* Search deep into zip archive for ending header *)
      ZipCmntBufSize := 32000;
      SetArcType(atUnknown);

      If Pos('.', fOutFilename) > 0 Then
         SetLength(fOutFilename, Pred(Pos('.', fOutFilename)));

      Case ArcType Of
         atAceExe: fOutFilename := fOutFilename + '.ACE';
         atArcExe: fOutFilename := fOutFilename + '.ARC';
         atArjExe: fOutFilename := fOutFilename + '.ARJ';
         atBhExe: fOutFilename := fOutFilename + '.BH';
         atLhaExe: fOutFilename := fOutFilename + '.Lha';
         atLzhExe: fOutFilename := fOutFilename + '.Lzh';
         atPakExe: fOutFilename := fOutFilename + '.PAK';
         atRarExe: fOutFilename := fOutFilename + '.RAR';
         atZipExe: fOutFilename := fOutFilename + '.ZIP';
         atJarExe: fOutFilename := fOutFilename + '.JAR';
      Else
         fOutFilename := '';
         RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
      End;
   End
   Else
   Begin
      fArchiveFile := '';
      fArcType := atNA;
   End;
End;
//-------------------------------------------------------------

Procedure TUnSfx.ConvertStream;
Var
	InStream,
   	OutStream: TFileStream32;
Begin
	InStream :=
   	TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyNone);

   If (InStream.Handle < 0 ) Then
   	RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN)
   Else Begin
   	Try
      	InStream.Position := fOffsetStart;

         OutStream :=
            TFileStream32.Create(fOutFilename, fmCreate Or fmShareExclusive);

   		If (OutStream.Handle < 0) Then
   			RaiseErrorStr(fOutFilename, '', '0', E_FOPEN)
         Else
            Try
               OutStream.CancelCallBackProc := @fCancel;
               OutStream.ProgressCallBackProc := Nil;
               OutStream.CopyFrom(InStream, InStream.Size - fOffsetStart);
            Finally
            	OutStream.Free();
            End;
      Finally
      	InStream.Free()
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TUnSfx.Activate;
Var
   Sig: ^u_long;
   Buffer: PChar;
   Infile, Outfile: THandle;
   BytesRead, ReadBytes: DWord;
   EndZipHeadPos, BegOffset, BSize: u_long;

Begin

   Try
      Cancel := False;

      If fArchiveFile <> '' Then
      Begin

         If fArchiveFile = fOutFilename Then
            RaiseError(E_RAISE, fArchiveFile, '', '0', E_FILESMATCH)
         Else
            If FileExists(fOutFilename) Then
               If Assigned(fOnFileExists) Then
                  OnFileExists(Self, fOutFilename, ztvConvertDate(FileAge(fOutFilename)), fOverwriteMode)
               Else
                  Exit;

         If (fOverwriteMode = omOverwrite) Then
         Begin

            If ArcType In UnSFX_ArcType Then
            Begin

            	If Assigned(OnElapsedTime) Then
               	ZipTimer.START;

               Try
                  Case ArcType Of
                     atJarExe,
                        atZipExe:
                        Begin
                           GetMem(Buffer, WSIZE);
                           Try
                              If OpenArchive(Infile, fArchiveFile) Then
                              Begin
                                 Try
                                    Outfile := ztvOpenFileWrite(Self, @fOutFilename[1], True);
                                    If Outfile <> INVALID_HANDLE_VALUE Then
                                    Try
                                       If fLOF < WSIZE Then
                                          ReadBytes := fLOF
                                       Else
                                          ReadBytes := WSIZE;

                                       // Great probability the sfx headers offsets weren't revised.
                                       // Example: Pkware's old Zip2Exe.exe sfx utility.  Perform
                                       // a simple cut.
                                       If (HeaderTypeState = [htLocal]) Then
                                       Begin
                                          ztvSetFilePointer(Infile, fOffsetStart, FILE_BEGIN);
                                          ReadFile(Infile, LocalZipHeader, SizeOf(LocalZipHeader), BytesRead, Nil);

                                          If LocalZipHeader.SignAtr = LOCAL_FILE_HEADER_SIGNATURE Then
                                          Begin
                                             ztvSetFilePointer(Infile, fOffsetStart, FILE_BEGIN);
                                             While (BytesRead > 0) And (fOffsetStart < fLOF) Do
                                             Begin
                                                ReadFile(Infile, Buffer[0], ReadBytes, BytesRead, Nil);
                                                WriteFile(Outfile, Buffer[0], BytesRead, BytesRead, Nil);
                                                inc(fOffsetStart, BytesRead);
                                             End;

                                          End;

                                       End Else Begin

                                          ztvSetFilePointer(Infile, fLOF - ReadBytes, FILE_BEGIN);
                                          ReadFile(Infile, Buffer[0], ReadBytes, BytesRead, Nil);
                                          Dec(BytesRead, SizeOf(TEnd)-1);

                                          While BytesRead > 0 Do
                                          Begin

                                             Sig := @Buffer[BytesRead];
                                             If Sig^ = END_OF_CENTRAL_HEADER_SIGNATURE Then
                                             Begin

                                                EndZipHeadPos := ReadBytes - BytesRead;
                                                ztvSetFilePointer(Infile, fLOF - (EndZipHeadPos), FILE_BEGIN);
                                                ReadFile(Infile, EndZipHeader, SizeOf(EndZipHeader), BytesRead, Nil);

                                                If EndZipHeader.SignAtr = END_OF_CENTRAL_HEADER_SIGNATURE Then
                                                Begin

                                                   If (HeaderTypeState = [htLocal]) Then
                                                      BegOffset := fOffsetStart
                                                   Else
                                                   Begin

                                                     (* File by file *)
                                                      ztvSetFilePointer(Infile, EndZipHeader.CentralDirOffset, FILE_BEGIN);
                                                      ReadFile(Infile, CentralZipHeader, SizeOf(TCentral), BytesRead, Nil);
                                                      BegOffset := CentralZipHeader.RelativeOffsetOfLocalHeader;
                                                   End;

                                                  (* Append Local Dir section *)
                                                   ztvSetFilePointer(Infile, BegOffset, FILE_BEGIN);
                                                   ReadFile(Infile, LocalZipHeader, SizeOf(TLocal), BytesRead, Nil);
                                                   While (LocalZipHeader.SignAtr = LOCAL_FILE_HEADER_SIGNATURE) Do
                                                   Begin

                                                      WriteFile(Outfile, LocalZipHeader, BytesRead, BytesRead, Nil);
                                                      With LocalZipHeader Do
                                                      Begin
                                                         While fOffsetStart < ZC.PackedSize + ZC.FileNameLen + ZC.ExtraFieldLen Do
                                                         Begin
                                                            If PackedSize + ZC.FileNameLen + ZC.ExtraFieldLen - fOffsetStart >= WSIZE Then
                                                               BSize := WSIZE
                                                            Else
                                                               BSize := ZC.PackedSize + ZC.FileNameLen + ZC.ExtraFieldLen - fOffsetStart;

                                                            ReadFile(Infile, Buffer[0], BSize, BytesRead, Nil);
                                                            WriteFile(Outfile, Buffer[0], BytesRead, BytesRead, Nil);
                                                            inc(fOffsetStart, BytesRead);
                                                         End;
                                                         SignAtr := 0;
                                                      End;

                                                      ReadFile(Infile, LocalZipHeader, SizeOf(TLocal), BytesRead, Nil);
                                                   End;


                                                  (* Append the Central Dir *)
                                                   ztvSetFilePointer(Infile, ztvGetFilePos(Infile) - (SizeOf(TLocal)), FILE_BEGIN);
                                                   ReadFile(Infile, CentralZipHeader, SizeOf(TCentral), BytesRead, Nil);
                                                   While CentralZipHeader.SignAtr = CENTRAL_FILE_HEADER_SIGNATURE Do
                                                   Begin
                                                      Dec(CentralZipHeader.RelativeOffsetOfLocalHeader, BegOffset);
                                                      WriteFile(Outfile, CentralZipHeader, BytesRead, BytesRead, Nil);

                                                      With CentralZipHeader Do
                                                      Begin
                                                         While fOffsetStart < ZC.FileNameLen + CommentLen + ZC.ExtraFieldLen Do
                                                         Begin
                                                            If ZC.FileNameLen + CommentLen + ZC.ExtraFieldLen - fOffsetStart >= WSIZE Then
                                                               BSize := WSIZE
                                                            Else
                                                               BSize := ZC.FileNameLen + CommentLen + ZC.ExtraFieldLen - fOffsetStart;

                                                            ReadFile(Infile, Buffer[0], BSize, BytesRead, Nil);
                                                            inc(fOffsetStart, BytesRead);
                                                            WriteFile(Outfile, Buffer[0], BytesRead, BytesRead, Nil);
                                                         End;
                                                         SignAtr := 0;
                                                      End;

                                                      ReadFile(Infile, CentralZipHeader, SizeOf(TCentral), BytesRead, Nil);
                                                   End;

                                                   // Append the Ending Header
                                                   Dec(EndZipHeader.CentralDirOffset, BegOffset);
                                                   EndZipHeader.CommentLen := 0; //don't copy the comment
                                                   WriteFile(Outfile, EndZipHeader, SizeOf(TEnd), BytesRead, Nil);
                                                End;
                                                break;

                                             End;
                                             Dec(BytesRead);

                                          End;
                                       End;

                                    Finally
                                       CloseHandle(Outfile);
                                    End;
                                 Finally
                                    CloseHandle(Infile);
                                 End;
                              End Else
                                 RaiseError(E_RAISE, fArchiveFile, '', '0', E_FOPEN);
                           Finally
                              FreeMem(Buffer);
                           End;
                        End;
                  Else
                     // atArcExe, atArjExe, atBhEXE, atLha, atLzh,
                     // atPakExe, atRarExe
                     ConvertStream();
                  End;
               Finally
                  If Assigned(OnElapsedTime) Then
                  Begin
                     ZipTimer.Stop;
                     OnElapsedTime(Self, ZipTimer.ElapsedTime);
                  End;
               End;
            End;
         End;
      End
      Else
         RaiseError(E_RAISE, fArchiveFile, '', '0', E_FILENOTFOUND);
   Except
   End;
End;
//-------------------------------------------------------------


End.
