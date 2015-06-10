(**********************************************************************

  Copyright 1998-2003,  Microchip Data Systems / Carl Bunton

  Under license agreement, this source module may be used only on a
  single computer.

  No portion of this module may be reproduced, copied, revised, edited,
  distributed or transmitted via electronic means except in compiled
  application format.

  Web-site:  http://www.ziptv.com
  Email:     custsupt@ziptv.com

**********************************************************************
  See usage notes in the header section of demo.pas!
**********************************************************************

  Version 4.0:
     1. Break on exception IDE setting no longer required.

     2. (WordListFile property coded by Klaus Holtorf)

        Property "WordListFile" added to support storage of passwords in a
        list on your local disk for easy and rapid retrieval.  Now you can
        use extra security by using LONG passwords (see above chart), save
        them in a password list file and you don't have to remember the
        password.

        NOTE: this enhances security against this brute force password
        attack, because the longer the password, the longer it takes to
        retrieve the password.  It's common practice to use short abbreviated
        passwords because we have to remember them.

        The file defined in WordListFile is a local file that is not
        distributed with your archives.  They are for personal use only.

*)
Unit ztvZipKey;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   Dialogs,
   ztvUnZIP,
   ztvBase,
   ztvGbls,
   ztvFileIo,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TOnChange = Procedure(Sender: TObject; Password: String; PasswordHits, ForcedHits: Integer) Of Object;
   TOnFound = Procedure(Sender: TObject; FileName, Password: String) Of Object;

Type
   TCharSets = (csNumeric,
      csAlphaUpperCase,
      csAlphaLowerCase,
      csAlphaULcase,
      csAlphaNumeric,
      csAlphaNumericU,
      csAlphaNumericL,
      csFullSet,
      csNonAlphaNumeric,
  (* csWordList : Charset *)
      csWordlist);

   TZipKey = Class(TUnZip)
   Private
      (* csWordList : Variable *)
      WlFile: Text;
      fCharSet: Array[0..255] Of char;
      fCharSets: TCharSets;
      fForcedHits: Integer;
      fOnChange: TOnChange;
      fOnFound: TOnFound;
      fPasswordHits: Integer;
      fPause: Boolean;
      fRecordSize: Integer;
      fStartPassword: String;
      fWordListFile: String;
      Procedure INIT;
      Procedure SetCharSets(SCS: TCharSets);
      Procedure SetPause(SP: Boolean);
      Procedure SetStartPassword(SSP: String);
      Function Get_CharSet_Len: word;
      Function GetCPos(C: char): char;
      Function UpdatePassword(Password: String): String;
      Function ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
      	size: Byte; Counter: DWord; WriteType: TDataType): DWord; Virtual;
   Protected
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Activate;
      Property Pause: Boolean Read fPause Write SetPause;
   Published
      Property ArcType;
      Property CharSets: TCharSets Read fCharSets Write SetCharSets;
      Property StartPassword: String Read fStartPassword Write SetStartPassword;
      Property WordListFile: String Read fWordListFile Write fWordListFile;
      Property FileSpec;
      Property ZipCmntBufSize;
      Property OnBegin;
      Property OnChange: TOnChange Read fOnChange Write fOnChange;
      Property OnEnd;
      Property OnError;
      Property OnFound: TOnFound Read fOnFound Write fOnFound;
   End;

Implementation

Uses
   Forms,
   ztvHeaders,
   ztvCrypt,
   Err_Msgs;

Const
   LettersInAlpha = 26;
   //RAND_HEAD_LEN    = 12;            (* length of encryption random header *)

Var
   aNumeric: Array[0..9] Of Byte;
   aAlphaUpperCase: Array[0..(LettersInAlpha - 1)] Of Byte;
   aAlphaLowerCase: Array[0..(LettersInAlpha - 1)] Of Byte;
   aAlphaULcase: Array[0..(LettersInAlpha * 2) - 1] Of Byte;
   aAlphaNumeric: Array[0..(LettersInAlpha * 2) + 9] Of Byte;
   aAlphaNumericU: Array[0..LettersInAlpha + 9] Of Byte;
   aAlphaNumericL: Array[0..LettersInAlpha + 9] Of Byte;
   aFullSet: Array[0..255] Of Byte;
   aNonAlphaNumeric: Array[0..192] Of Byte;
   //-------------------------------------------------------------

Constructor TZipKey.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TZipKey.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TZipKey.ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
	size: Byte; Counter: DWord; WriteType: TDataType): DWord;
Var
   i: Integer;
   p: ^Byte;
Begin
   Result := Counter;

   If IsEncrypted And (WriteType = dtData) And
      (ArcType In [atBh, atBhExe, atZip, atZipExe, atJar]) Then
   Begin
      p := @Buf;
      For i := 1 To Counter Do
      Begin
         p^ := p^ Xor ztvDecryptByte();
         update_keys(p^);
         inc(p);
      End;
   End;

   If WriteType = dtData Then
   Begin
      If size = 16 Then
         Crc16_buf(@Buf, Result, Crc16Val)
      Else
         If size = 32 Then
            Crc32_buf(@Buf, Result, Crc32Val);
   End;

   Application.ProcessMessages;
End;
//-------------------------------------------------------------

Function TZipKey.GetCPos(C: char): char;
Var
   i, j: word;
Begin
   Result := #0;
   j := Get_CharSet_Len();
   For i := 0 To j - 1 Do
      If fCharSet[i] = C Then
      Begin
         Result := fCharSet[i + 1];
         break;
      End;
End;
//-------------------------------------------------------------

Function TZipKey.UpdatePassword(Password: String): String;
Var
   io: Integer;
   l, n: Integer;
   pw: String;
Begin

   inc(fPasswordHits);

 (* csWordList : Read next Password *)
   If fCharSets = csWordlist Then
   Begin
{$I-}ReadLn(WlFile, pw);
{$I+}
      io := IoResult;
      If io <> 0 Then
         fPause := True;

      Result := pw;

      Exit;
   End;
 (* csWordList : End Read next Password *)

   Result := Password;
   l := Length(Password);
   If l = 0 Then
   Begin
      Result := fCharSet[0];
      Exit;
   End;

   If Result[l] = fCharSet[Get_CharSet_Len() - 1] Then
   Begin
      If l > 1 Then
      Begin
         Result[l] := fCharSet[0];      // first increment last char as first

         n := Pred(l);
         Repeat
            If Result[n] <> fCharSet[Get_CharSet_Len() - 1] Then
            Begin
               Result[n] := GetCPos(Result[n]);
               break;
            End
            Else
            Begin
               If fCharSet[0] = #0 Then
                  Result[n] := fCharSet[1]
               Else
                  Result[n] := fCharSet[0];

               If n = 1 Then
                  If fCharSet[0] = #0 Then
                     Result := Result + fCharSet[1]
                  Else
                     Result := Result + fCharSet[0];
            End;
            Dec(n);
         Until n = 0;

      End
      Else
      Begin
         Result[l] := fCharSet[0];
         Result := Result + fCharSet[0]
      End;

   End
   Else

      Result[l] := GetCPos(Result[l]);

End;
//-------------------------------------------------------------

Function TZipKey.Get_CharSet_Len: word;
Begin
   Case fCharSets Of
      csNumeric: Result := SizeOf(aNumeric);
      csAlphaLowerCase: Result := SizeOf(aAlphaLowerCase);
      csAlphaUpperCase: Result := SizeOf(aAlphaUpperCase);
      csAlphaULcase: Result := SizeOf(aAlphaULcase);
      csAlphaNumeric: Result := SizeOf(aAlphaNumeric);
      csAlphaNumericU: Result := SizeOf(aAlphaNumericU);
      csAlphaNumericL: Result := SizeOf(aAlphaNumericL);
      csFullSet: Result := SizeOf(aFullSet);
      csNonAlphaNumeric: Result := SizeOf(aNonAlphaNumeric);
   Else
      Result := 0;
   End;
End;
//-------------------------------------------------------------

Procedure TZipKey.INIT;
Var
   i: Integer;
Begin

   (* aNumeric *)
   For i := 0 To Pred(SizeOf(aNumeric)) Do
      aNumeric[i] := Byte('0') + i;

   (* aAlphaUpperCase *)
   For i := 0 To Pred(SizeOf(aAlphaUpperCase)) Do
      aAlphaUpperCase[i] := Byte('A') + i;

   (* aAlphaLowerCase *)
   For i := 0 To Pred(SizeOf(aAlphaLowerCase)) Do
      aAlphaLowerCase[i] := Byte('a') + i;

   (* FullSet *)
   For i := 0 To 255 Do
      aFullSet[i] := i;

   (* aAlphaULcase *)
   CopyMem(@aAlphaUpperCase[0], @aAlphaULcase[0], SizeOf(aAlphaUpperCase));
   CopyMem(@aAlphaLowerCase[0], @aAlphaULcase[SizeOf(aAlphaUpperCase)]
      , SizeOf(aAlphaLowerCase));
   (* aAlphaNumeric *)
   CopyMem(@aNumeric[0], @aAlphaNumeric[0], SizeOf(aNumeric));
   CopyMem(@aAlphaULcase[0], @aAlphaNumeric[SizeOf(aNumeric)], LettersInAlpha * 2);

   (* aAlphaNumericU *)
   CopyMem(@aNumeric[0], @aAlphaNumericU[0], SizeOf(aNumeric));
   CopyMem(@aAlphaUpperCase[0], @aAlphaNumericU[SizeOf(aNumeric)]
      , LettersInAlpha);
   (* aAlphaNumericL *)
   CopyMem(@aNumeric[0], @aAlphaNumericL[0], SizeOf(aNumeric));
   CopyMem(@aAlphaLowerCase[0], @aAlphaNumericL[SizeOf(aNumeric)]
      , LettersInAlpha);
   (* NonAlphaNumeric *)
   CopyMem(@aFullSet[0], @aNonAlphaNumeric[0], 47);
   CopyMem(@aFullSet[58], @aNonAlphaNumeric[47], 7);
   CopyMem(@aFullSet[91], @aNonAlphaNumeric[54], 6);
   CopyMem(@aFullSet[123], @aNonAlphaNumeric[60], 133);

   Case fCharSets Of
      csNumeric: CopyMem(@aNumeric[0], @fCharSet[0], SizeOf(aNumeric));
      csAlphaLowerCase: CopyMem(@aAlphaLowerCase[0], @fCharSet[0], SizeOf(aAlphaLowerCase));
      csAlphaUpperCase: CopyMem(@aAlphaUpperCase[0], @fCharSet[0], SizeOf(aAlphaUpperCase));
      csAlphaULcase: CopyMem(@aAlphaULcase[0], @fCharSet[0], SizeOf(aAlphaULcase));
      csAlphaNumeric: CopyMem(@aAlphaNumeric[0], @fCharSet[0], SizeOf(aAlphaNumeric));
      csAlphaNumericU: CopyMem(@aAlphaNumericU[0], @fCharSet[0], SizeOf(aAlphaNumericU));
      csAlphaNumericL: CopyMem(@aAlphaNumericL[0], @fCharSet[0], SizeOf(aAlphaNumericL));
      csFullSet: CopyMem(@aFullSet[0], @fCharSet[0], SizeOf(aFullSet));
      csNonAlphaNumeric: CopyMem(@aNonAlphaNumeric[0], @fCharSet[0], SizeOf(aNonAlphaNumeric));
   End;
End;
//-------------------------------------------------------------

Procedure TZipKey.SetCharSets(SCS: TCharSets);
Begin
	fCharSets := SCS;
End;
//-------------------------------------------------------------

Procedure TZipKey.SetPause(SP: Boolean);
Begin
	fPause := SP;
End;
//-------------------------------------------------------------

Procedure TZipKey.SetStartPassword(SSP: String);
Begin
	fStartPassword := SSP;
End;
//-------------------------------------------------------------

Procedure TZipKey.Activate;
Var
   Password: String;
   Infile,
   	Outfile: TStream32;
   Buffer, pFilename: PChar;
   SavePos, BytesRead: DWord;
   Action, isPassword: Boolean;

   Function GetEndCentralDirFile(Handle: THandle): Boolean;
   Var
      x: Integer;
      pl: ^longint;
      pBlock: PChar;
      BytesRead: DWord;
      BufSize: DWord;
   Begin
      If fLOF < WSIZE Then
         BufSize := fLOF - 1
      Else
         BufSize := WSIZE;

      GetMem(pBlock, BufSize + 1);
      Try
         ztvSetFilePointer(Handle, fLOF - BufSize, FILE_BEGIN);
         ReadFile(Handle, pBlock[0], BufSize, BytesRead, Nil);

         For x := (BufSize - (SizeOf(TEnd) - 1)) Downto 1 Do
         Begin
            pl := @pBlock[x];
            If pl^ = END_OF_CENTRAL_HEADER_SIGNATURE Then
            Begin
               CopyMem(@pBlock[x], @EndZipHeader, SizeOf(TEnd));
               break;
            End;
         End;
      Finally
         FreeMem(pBlock, BufSize + 1);
      End;
      Result := EndZipHeader.SignAtr = END_OF_CENTRAL_HEADER_SIGNATURE;
   End;
Begin
   Cancel := False;

   If Not fPause Then
   Begin
      Count := 0;
      fFilePos := 0;
      fPasswordHits := 0;
      fForcedHits := 0;
   End;

 	(* csWordList : Open Wordlist-File *)
   If fCharSets = csWordlist Then
   Begin
      If Not FileExists(fWordListFile) Then
         RaiseError(E_RAISE, fWordListFile, '', '0', E_INVALIDFN);

      FileMode := 0;
{$I-}
      AssignFile(WlFile, fWordListFile);
      Reset(WlFile);
{$I+}
      //Filemode := 2;
      If IoResult <> 0 Then
         Exit
   End;


   INIT();
   FileName := '';
   Try

      If (ArchiveFile = '') Then
         RaiseError(E_RAISE, ArchiveFile, '', '0', E_INVALIDFN)
      Else
         If Not (ArcType In [atZip, atZipExe]) Then
            RaiseError(E_RAISE, ArchiveFile, '', '0', E_INVALIDARC);

      If Not Assigned(OnFound) Then
         RaiseError(E_RAISE, ArchiveFile, 'OnFound', '0', E_REQUIREDEVENT);

      Count := 0;
      Infile :=
      	TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyNone);

      If (TFileStream32(Infile).Handle < 0) Then
      Begin
         RaiseErrorStr(ArchiveFile, '', '0', E_FOPEN);
      	Exit;
      End;

      GetMem(pFilename, 256);
      GetMem(Buffer, (RAND_HEAD_LEN * 2) + 1);

      Try
         If Not GetEndCentralDirFile(TFileStream32(Infile).Handle) Then
            RaiseError(E_RAISE, ArchiveFile, '', '0', E_UNKNTYPE);

         fOffsetStart := EndZipHeader.CentralDirOffset;
         fOffsetEnd := fLOF;
         fRecordSize := SizeOf(TCentral);
         fFilePos := fOffsetStart;

         //-
         //ztvSetFilePointer(Infile, fFilePos, FILE_BEGIN);
         Infile.Seek(fFilePos, soBeginning);
         //ReadFile(Infile, CentralZipHeader, fRecordSize, BytesRead, Nil);
         BytesRead := Infile.Read(CentralZipHeader, fRecordSize);
         inc(fFilePos, BytesRead);
         //-

         fPause := False;
         Password := fStartPassword;
         ExtractWriteBlock := ArcPutBlock;
         DoProgress := RetProgress;

         While fFilePos < fOffsetEnd Do
            With CentralZipHeader Do
            Begin
               If fPause Or Cancel Then break;

               If SignAtr <> CENTRAL_FILE_HEADER_SIGNATURE Then
                  fFilePos := fOffsetEnd
               Else
               Begin

                  If ZC.UnpackedSize = 0 Then
                  Begin
                     inc(fFilePos, ZC.FileNameLen + CommentLen);

                     //-
                     //ztvSetFilePointer(Infile, fFilePos, FILE_BEGIN);
                     Infile.Seek(fFilePos, soBeginning);
                     ZeroMemory(@CentralZipHeader, SizeOf(TCentral));
                     //ReadFile(Infile, CentralZipHeader, fRecordSize, BytesRead, Nil);
                     BytesRead := Infile.Read(CentralZipHeader, fRecordSize);
                     inc(fFilePos, BytesRead);
                     //-

                     Continue;
                  End;

                  //ztvSetFilePointer(Infile, fFilePos, FILE_BEGIN);
                  Infile.Seek(fFilePos, soBeginning);

                  If (ZC.FileNameLen < 256) Then
                     ReadFilename(Infile, pFilename, ZC.FileNameLen)
                  Else
                     Exit;

                  ActualFilename := StrPas(pFilename);

                  inc(fFilePos, ZC.FileNameLen);
                  If (ActualFilename <> '') And (ExternalAttr And 16 = 0) And (ExternalAttr And 8 = 0) Then
                  Begin
                     If CheckWildCard1(ActualFilename, FileSpec, ExcludeSpec) Then
                     Begin

                        If Assigned(OnElapsedTime) Then
                           ZipTimer.START;

                        With LocalZipHeader Do
                        Try
                           inc(Count);

                           //ztvSetFilePointer(Infile, RelativeOffsetOfLocalHeader, FILE_BEGIN);
                           Infile.Seek(RelativeOffsetOfLocalHeader, soBeginning);
                           ZeroMemory(@LocalZipHeader, SizeOf(TLocal));
                           //ReadFile(Infile, LocalZipHeader, SizeOf(TLocal), BytesRead, Nil);
                           BytesRead := Infile.Read(LocalZipHeader, SizeOf(TLocal));

                           If (BytesRead = SizeOf(TLocal)) And (SignAtr = LOCAL_FILE_HEADER_SIGNATURE) Then
                           Begin

                              InflateRec.CompressType := ZC.CompressType;
                              InflateRec.BitFlag := ZC.BitFlag;
                              InflateRec.PackedSize := ZC.PackedSize;
                              InflateRec.UnpackedSize := ZC.UnpackedSize;
                              ZipCompatible := IsZipCompatible(SignAtr);

                              Action := True;
                              If Assigned(OnBegin) Then
                                 OnBegin(Self, ActualFilename, Count, Action);

                              Try
                                 If Action Then
                                 Begin

                                    //ztvSetFilePointer(Infile,
                                    //   LocalZipHeader.ZC.FileNameLen, //+
                                    //   FILE_CURRENT);
                                    Infile.Seek(LocalZipHeader.ZC.FileNameLen, soCurrent);

                                    If (ZC.BitFlag And 1 > 0) Then
                                    Begin
                                       Action := True;
                                       ZeroMemory(Buffer, (RAND_HEAD_LEN * 2));

                                       (* Read the encrypted header of len RAND_HEAD_LEN into buffer *)
                                       //ReadFile(Infile, Buffer^, RAND_HEAD_LEN, BytesRead, Nil);
                                       {BytesRead :=} Infile.Read(Buffer^, RAND_HEAD_LEN);
                                       //SavePos := ztvGetFilePos(Infile);
                                       SavePos := Infile.Position;

                                       (* make a working copy of encrypted header in upper half OF buffer *)
                                       CopyMem(@Buffer[0],
                                          @Buffer[RAND_HEAD_LEN],
                                          RAND_HEAD_LEN);

                                       Repeat

                                          isPassword := decrypt_pw(Buffer,
                                             ZC.BitFlag, ZC.CRC32,
                                             ZC.FileDate, Password);

                                          If Not isPassword Then
                                          Begin
                                             Password := UpdatePassword(Password);

                                             If Assigned(OnChange) Then
                                                OnChange(Self, Password,
                                                   fPasswordHits, fForcedHits);

                                             isPassword := decrypt_pw(Buffer,
                                                ZC.BitFlag, ZC.CRC32,
                                                ZC.FileDate, Password);
                                          End;

                                          If fPause Then break;
                                          Application.ProcessMessages;

                                          If isPassword Then
                                          Begin
                                             Try
                                                Cancel := False;
                                                inc(fForcedHits);

                                                //Crc32Val := CRC_MASK;
                                                //ExtractInflate(
                                                //	Infile, Outfile, MaxBits
                                                //   );

                                                ExtractFile(Self,
                                                	ActualFilename, Infile,
                                                   Outfile, InflateRec, 0);

                                                If (Crc32Val <> CRC_MASK) And ((Crc32Val Xor CRC_MASK) = ZC.CRC32) Then
                                                Begin
                                                   If Assigned(OnElapsedTime) Then
                                                   Begin
                                                      ZipTimer.Stop;
                                                      OnElapsedTime(Self, ZipTimer.ElapsedTime);
                                                   End;
                                                   OnFound(Self, StrPas(pFilename), Password);
                                                End
                                                Else
                                                   If ZC.PackedSize > 0 Then
                                                      Password := UpdatePassword(Password);

                                             Except
                                                On e: exception Do ;
                                             End;

                                             If (Crc32Val Xor CRC_MASK) = ZC.CRC32 Then
                                                break
                                             Else
                                                isPassword := False;

                                             //ztvSetFilePointer(Infile, SavePos, FILE_BEGIN);
                                             Infile.Seek(SavePos, soBeginning);
                                             If isPassword Then break;
                                          End;
                                       Until 1 <> 1;

                                    End
                                    Else
                                    Begin
                                       ShowMessage(FileName + #13'FILE is NOT Password protected');
                                       break;
                                    End;
                                 End;
                              Finally
                                 If Assigned(OnEnd) Then
                                    OnEnd(Self, FileName, Action);
                              End;

                           End
                           Else
                              (* inconsistant local header *)
                              RaiseErrorStr(FileName, '', '0', E_LOCALHDR);

                        Finally
                           If Assigned(OnElapsedTime) Then
                              ZipTimer.Stop;
                        End;
                     End;

                     fFilePos := fFilePos + CommentLen + ZC.ExtraFieldLen;

                  End;
               End;
               inc(fFilePos, CommentLen);

               //-
               //ztvSetFilePointer(Infile, fFilePos, FILE_BEGIN);
               Infile.Seek(fFilePos, soBeginning);
               ZeroMemory(@CentralZipHeader, SizeOf(TCentral));
               //ReadFile(Infile, CentralZipHeader, fRecordSize, BytesRead, Nil);
               BytesRead := Infile.Read(CentralZipHeader, fRecordSize);
               inc(fFilePos, BytesRead);
               //-

            End;

      Finally

   		(* csWordList : Close Wordlist-File *)
         If fCharSets = csWordlist Then
{$I-}
				CloseFile(WlFile);
{$I+}
         Infile.Free(); //CloseHandle(Infile);
         FreeMem(pFilename, 256);
         FreeMem(Buffer, (RAND_HEAD_LEN * 2) + 1);
      End;
   Except
      On e: E_RAISE Do ;                //ShowMessage( E.Message );
   End;
End;
//-------------------------------------------------------------

End.
