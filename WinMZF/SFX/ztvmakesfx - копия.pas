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
Unit ztvMakeSfx;

Interface

Uses
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   ztvBase,
   ztvHeaders,
   ztvGbls,
   ztvStreams,
   Err_Msgs;

{$I ZipTV.inc}                          //Declare the compiler defines


Type
   TMakeSFX = Class(TZipCommon)
   Private
      fStubDir: String;
      fTargetFile: String;
      //Procedure SetStubDir(SSD: String);
		Function GetStubDir: String;
      Function GetTargetFile: String;
      Function SlideZipOffset(Stream: TStream32; StubSize: word): Boolean;
   Protected
      Procedure SetArchiveFile(SFN: String); Override;
   Public
      Function CreateSfx: Boolean;
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
   Published
      Property ArchiveFile;
      Property TargetFile: String Read GetTargetFile Write fTargetFile;
      Property SfxStubDir: String Read GetStubDir Write fStubDir {SetStubDir};
      //Property SfxStubDir: String Read fStubDir Write fStubDir; 
      Property OnError;
      Property OnFileExists;
   End;



Implementation

Uses
	ztvRegister;

//------------------------------------------------------------

Constructor TMakeSFX.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   //fStubDir := ExtractFilePath(ParamStr(0));
End;
//------------------------------------------------------------

Destructor TMakeSFX.Destroy;
Begin
   Inherited Destroy;
End;
//------------------------------------------------------------

Function TMakeSFX.CreateSfx: Boolean;
Var
   SfxStubSize: Integer;
   SfxStubName: String;
   SfxStubStream,
      SourceStream,
      TargetStream: TFileStream32;
Begin
   Result := False;
   Try

      If Not FileExists(fArchiveFile) Then
         RaiseError(E_RAISE, fArchiveFile, '', '0', E_FILENOTFOUND)
      Else
         If (fTargetFile = fArchiveFile) Then
            RaiseError(E_RAISE, fArchiveFile, '', '0', E_FILESMATCH)
         Else
            If FileExists(fTargetFile) Then
            Begin
               fOverwriteMode := omOverwrite;
               If Assigned(OnFileExists) Then
               Begin
                  OnFileExists(Self,
                     fTargetFile,
                     ztvConvertDate(FileAge(fTargetFile)),
                     fOverwriteMode);
               End
               Else
               Begin
                  RaiseErrorStr('OnFileExists', 'OnFileExists', '0', E_REQUIREDEVENT);
                  Cancel := True;
               End;

               If fOverwriteMode <> omOverwrite Then
                  Cancel := True
               Else
                  EraseFile(fTargetFile, doFinal);
            End;

      If Cancel Then Exit;

      Case ArcType Of
         atAce: SfxStubName := 'Ace';
         atArj: SfxStubName := 'Arj';
         atBh: SfxStubName := 'Bh';
         atJar: SfxStubName := 'Jar';
         atLzh,
            atLha: SfxStubName := 'Lha';
         atRar: SfxStubName := 'Rar';
         atZip: SfxStubName := 'Zip';
      Else
         RaiseError(E_RAISE, fArchiveFile, '', '0', E_INVALIDARC)
      End;

      SfxStubName := AppendDirTail(SfxStubDir) + 'ztv_' + _ChangeFileExt(SfxStubName, '.SFX');

      If Not FileExists(SfxStubName) Then
         RaiseError(E_RAISE, SfxStubName, '', '0', E_FILENOTFOUND);

      TargetStream := TFileStream32.Create(TargetFile, fmCreate);

      If (TargetStream.Handle < 0) Then
      Begin
         RaiseErrorStr(TargetFile, '', '0', E_FOPEN);
         Exit;
      End;

      TargetStream.CancelCallBackProc := Nil;
      TargetStream.ProgressCallBackProc := Nil;

      Try
         SfxStubStream := TFileStream32.Create(SfxStubName, fmOpenRead Or fmShareDenyNone);
         If (SfxStubStream.Handle < 0) Then
         Begin
            RaiseErrorStr(SfxStubName, '', '0', E_FOPEN);
            Exit;
         End;

         Try
            TargetStream.CopyFrom(SfxStubStream, 0);
         Finally
            SfxStubSize := SfxStubStream.size;
            SfxStubStream.Free();
         End;

         SourceStream := TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyNone);
         If (SourceStream.Handle < 0) Then
         Begin
            RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN);
            Exit;
         End;

         Try
            TargetStream.CancelCallBackProc := Nil;
            TargetStream.ProgressCallBackProc := Nil;
            TargetStream.CopyFrom(SourceStream, 0);

            If (ArcType = atZip) Or (ArcType = atJar) Then
               If Not SlideZipOffset(TargetStream, SfxStubSize) Then
                  ShowMessage('SlideZipOffset error...');

         Finally
            SourceStream.Free();
         End;

         Result := True;
      Finally
         TargetStream.Free();
      End;

   Except
   End;
End;
//------------------------------------------------------------

Function TMakeSFX.GetTargetFile: String;
Begin
   If (fTargetFile = '') And (fArchiveFile <> '') And FileExists(fArchiveFile) Then
      fTargetFile := LowerCase(_ChangeFileExt(fArchiveFile, '.exe'));
   Result := fTargetFile;
End;
//-------------------------------------------------------------

Function TMakeSFX.SlideZipOffset(Stream: TStream32; StubSize: word): Boolean;

   Function GetEndDir: Boolean;
   Var
      pl: ^Integer;
      pBlock: PChar;
      Encrypted: Boolean;
      x, BufSize: Integer;
   Begin
      Result := False;

      If Stream.size = 0 Then
         Exit;

      If Stream.size < WSIZE Then
         BufSize := Stream.size - 1
      Else
         BufSize := WSIZE;

      GetMem(pBlock, BufSize + 2);
      Try
         (* set entry pointer into file *)
         Stream.Position := Stream.size - BufSize;
         BufSize := Stream.Read(pBlock^, BufSize);

         For x := (BufSize - (SizeOf(TEnd))) Downto 1 Do
         Begin
            pl := @pBlock[x];
            If VerSig(pl^, htEnding, Encrypted) = htEnding Then
            Begin
               Stream.Position := Stream.size - BufSize + x;
               Stream.Read(EndZipHeader, SizeOf(TEnd));

               If (VerSig(EndZipHeader.SignAtr, htEnding, Encrypted) = htEnding) Then
               Begin
                  If Encrypted Then
                     DecodeHeader(@EndZipHeader, htEnding);

                  EndZipHeader.CentralDirOffset :=
                     EndZipHeader.CentralDirOffset + StubSize;
               End;

               Stream.Position := Stream.size - BufSize + x;

               Result :=
                  Stream.Write(EndZipHeader, SizeOf(TEnd)) = SizeOf(TEnd);

               break;
            End;
         End;
      Finally
         FreeMem(pBlock, BufSize + 2);
      End;
   End;
Var
   Encrypted: Boolean;
	HeadType: THeaderType;
   Offset,
      BytesRead,
      BytesWritten: Integer;
Begin

   If GetEndDir() Then
   Begin
      Result := False;

      Offset := EndZipHeader.CentralDirOffset;
      With CentralZipHeader Do
         Repeat
            Stream.Position := Offset;

            ZeroMemory(@CentralZipHeader, SizeOf(TCentral));
            BytesRead :=
               Stream.Read(CentralZipHeader, SizeOf(CentralZipHeader));

            If (BytesRead <> SizeOf(CentralZipHeader)) Then
            Begin
               HeadType := VerSig(SignAtr, htCentral, Encrypted);
               If HeadType = htNone Then
               Begin
               	HeadType := VerSig(SignAtr, htEnding, Encrypted);
                  If (HeadType = htEnding) Then
               		Break
                  Else
                  	Exit;
               End Else
               	Exit;

            End Else Begin
               HeadType := VerSig(SignAtr, htCentral, Encrypted);
               If (HeadType = htNone) Then
               Begin
               	HeadType := VerSig(SignAtr, htEnding, Encrypted);
                  If (HeadType = htEnding) Then
               		Break
                  Else
                  	Exit;
               End;
            End;

            If Encrypted Then
               DecodeHeader(@CentralZipHeader, htCentral);

            RelativeOffsetOfLocalHeader := RelativeOffsetOfLocalHeader + StubSize;

            Stream.Position := Offset;
            BytesWritten := Stream.Write(CentralZipHeader, SizeOf(CentralZipHeader));
            If BytesWritten = 0 Then Exit;

            Offset := Offset + SizeOf(CentralZipHeader) + zc.FileNameLen +
               zc.ExtraFieldLen + CommentLen;

         Until 1 <> 1;
      Result := True;

   End
   Else
      Result := False;

End;
//------------------------------------------------------------

Function TMakeSFX.GetStubDir: String;
Var
  	sd: PChar;
Begin
	If _DirectoryExists(fStubDir) Then
   	Result := AppendDirTail(fStubDir)
   Else Begin
      GetMem(sd, 256);
      Try
         GetSystemDirectory(sd, 256);
         Result:= AppendDirTail(StrPas(sd));
      Finally
         FreeMem(sd, 256);
      End;
   End;
End;

//------------------------------------------------------------
{Procedure TMakeSFX.SetStubDir(SSD: String);
Begin
	If _DirectoryExists(SSD) Then
   	fStubDir := AppendDirTail(SSD)
   Else
   	fStubDir := AppendDirTail(GetTempPathStr());
End;}
//------------------------------------------------------------

Procedure TMakeSFX.SetArchiveFile(SFN: String);
Begin
   Inherited;
   GetTargetFile();
End;
//------------------------------------------------------------


End.
