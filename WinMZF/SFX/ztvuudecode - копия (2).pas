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
Unit ztvUUDecode;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   ztvBase,
   ztvGbls,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines


Type
   TUUDecode = Class(TUnBASE)
   Private
      FMaxHeaderLines: Integer;
      FVolumeLongFilenames: Boolean;
      FVolumeStartNum: Byte;
      FNextVolumeName: TNextVolumeName;
      Function decode_file(Infile, Outfile: TStream32; Var fSize: Integer): Integer;
      Function fgets(Infile: TStream32; s: PChar; MaxLen: Integer): Integer;
      Function GetString(Infile: TStream32; s: PChar): Integer;
      Function get_start(Infile: TStream32; infname: String): Boolean;
      Function get_outfname(Infile: TStream32): String;
      Procedure get_encmethod(Infile: TStream32; notable: Boolean);
      Procedure SetVolumeStartNum(SVSN: Byte);
      Procedure WriteBuf(Outfile: TStream32; Buf: PChar; Len: Integer);
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure ExtractIT(Var Infile: TStream32; Outfile: TStream32); Override;
      Property Cancel;
   Published
      Property ArcType;
      Property OverwriteMode;
      Property MaxHeaderLines: Integer Read FMaxHeaderLines Write FMaxHeaderLines Default 100;
      Property NextVolumeName: TNextVolumeName Read FNextVolumeName Write FNextVolumeName Default nvChangeExt;
      Property VolumeLongFilenames: Boolean Read FVolumeLongFilenames Write FVolumeLongFilenames Default False;
      Property VolumeStartNum: Byte Read FVolumeStartNum Write SetVolumeStartNum;
      Property ExtractDir;
      Property OnActivate;
      Property OnDeactivate;
      Property OnBegin;
      Property OnEnd;
      Property OnError;
      Property OnProgress;
      Property OnFileExists;
      Property OnNextVolume;
   End;

Implementation

Uses
   Err_Msgs;

Const
   iUUlen = 77;                         (* 'M' *)
   EOF = -1;
   UNDEF_CH = -2;                       (* undefined ch value in character map *)
   MAXLINELEN = 80;
   BIT6MASK = $3F;
   INLEN = 45;                          (* # chars in line to be encoded, must be multiple of 3 *)
   OUTLEN = 60;                         (* INLEN original bytes is OUTLEN encoded bytes *)
   system_id = 'ZipTV components: www.ziptv.com';
   EOL_STR = #13#10;                    (* end of line marking *)
   XXETABLE = '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
   UUETABLE = '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
   //UUETABLE = '`!\"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';

Var
   sMethod: Array[0..7] Of char;
   sXXtable: Array[0..63] Of char;
   sUUtable: Array[0..63] Of char;
   piNNmap: Array[-1..256] Of Integer;

   iXXlen,                              (* 'h' *)
      iNNlen,                           (* # bytes in a full length encoded line *)
      cLine,
      chFirst: Integer;
   BytesRead: Cardinal;
   InBufPos,
      CurInbufPos,
      OutBufPos: u_long;
   abLine,
      inbuf,
      outbuf: PChar;
   MaxMem: word;
   nobegin: Boolean;
   EndOfFile: Boolean;
   osize: u_long;
   ProgUpdate: u_long;

   //-------------------------------------------------------------

Constructor TUUDecode.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   FNextVolumeName := nvChangeExt;
   FVolumeLongFilenames := False;
   FMaxHeaderLines := 100;
End;
//-------------------------------------------------------------

Destructor TUUDecode.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function eGetNextChar(Sender: TZipCommon; Infile: TStream32): Byte;
Var
   Percentage: Byte;
   BufSize: Integer;
Begin

   If CurInbufPos + 1 >= BytesRead Then
   Begin

      // use the following block, instead of the min function...
      // the min function fails with files > 4 gig.
      //BufSize := Min(Sender.Bytes_To_Go, WSIZE);
      If Sender.Bytes_To_Go > WSIZE Then
         BufSize := WSIZE
      Else
         BufSize := Sender.Bytes_To_Go;

      BytesRead :=
      	Sender.ReadBlock(Infile, Nil, inbuf[0], False, 0, BufSize, dtData);

      If BytesRead = 0 Then
      Begin
         Sender.Bytes_To_Go := 0;
         EndOfFile := True;
         Result := 0;
         Exit;
      End;

      Dec(ProgUpdate, BytesRead);
      With Sender Do
      Begin
         ProgressPosition := ProgUpdate;
         Percentage := CalcProgress64(fLOF - ProgUpdate, fLOF);
         DoProgress(Percentage, Percentage);
      End;

      CurInbufPos := 0;
   End
   Else
      inc(CurInbufPos);

   inc(InBufPos);
   Dec(Sender.Bytes_To_Go);
   Result := Integer(inbuf[CurInbufPos]);
End;
//-------------------------------------------------------------

Procedure TUUDecode.SetVolumeStartNum(SVSN: Byte);
Begin
   If SVSN > 10 Then SVSN := 10;
   FVolumeStartNum := SVSN;
End;
//-------------------------------------------------------------

Procedure TUUDecode.WriteBuf(Outfile: TStream32; Buf: PChar; Len: Integer);
Var
   i: Integer;
Begin
   For i := 0 To Pred(Len) Do
   Begin
      If OutBufPos + 1 >= MaxMem Then
      Begin
         (* no crc calculations *)
         If ExtractWriteBlock(Outfile, outbuf^, False, 0, OutBufPos, dtData) = 0 Then
            doWriteError;

         OutBufPos := 0;
      End;

      outbuf[OutBufPos] := Buf[i];
      inc(OutBufPos);
   End;
End;
//-------------------------------------------------------------

Procedure TUUDecode.get_encmethod(Infile: TStream32; notable: Boolean);
Var
   i, ch: Integer;
Begin
   (* peek ahead to get line length	*)
   chFirst := eGetNextChar(Self, Infile);
   Dec(CurInbufPos);
   Dec(InBufPos);
   inc(Bytes_To_Go);

   (* determine encoding method *)
   If notable Then
   Begin
      If chFirst = iUUlen Then
      Begin
         sMethod[0] := 'U';
         sMethod[1] := 'U';
         CopyMem(@sUUtable[0], @sXXtable[0], 64); (* replace XXmap by UUmap *)
         iXXlen := iUUlen;
      End
      Else
         If chFirst <> iXXlen Then
            RaiseError(E_RAISE, FileName, '', fArchiveFile, E_UNKNMETH)

   End
   Else
   Begin
      sMethod[0] := 'U';
      sMethod[1] := 'U';
      iXXlen := chFirst;
   End;

   For i := 0 To 255 Do                 (* setup character map *)
      piNNmap[i] := UNDEF_CH;

   For i := 0 To 63 Do
   Begin
      ch := Integer(sXXtable[i]);
      If (piNNmap[ch] = UNDEF_CH) Then
         piNNmap[ch] := i;
      //Else
      //	ShowMessage( 'character ' + IntToStr( ch ) + ' already defined in table' );
   End;

   piNNmap[32] := 0;
   piNNmap[EOF] := EOF;

   (* replace encoded value by # bytes *)
   iNNlen := piNNmap[iXXlen];
End;
//-------------------------------------------------------------

Function TUUDecode.GetString(Infile: TStream32; s: PChar): Integer;
Var
   C, i: Integer;
Begin
   i := -1;
   Repeat
      C := eGetNextChar(Self, Infile);
      s[i + 1] := char(C);
      inc(i);
      //IF i > MAXLINELEN THEN Break;
   Until (C = 10) Or (Bytes_To_Go = 0);
   Result := i;
End;
//-------------------------------------------------------------

Function TUUDecode.fgets(Infile: TStream32; s: PChar; MaxLen: Integer): Integer;
Var
   C, i: Integer;
Begin
   i := 0;
   Repeat
      C := eGetNextChar(Self, Infile);
      If (C <> 13) And (C <> 10) And (Bytes_To_Go > 0) Then
      Begin
         s[i] := char(C);
         inc(i);
      End;
   Until (C = 10) Or EndOfFile Or (i > MaxLen);

   If EndOfFile Then
   Begin
      //Dec( i );
      Result := EOF;
   End
   Else
   Begin
      s[i] := #0;
      Result := i;
   End;
End;
//-------------------------------------------------------------

Function TUUDecode.get_outfname(Infile: TStream32): String;
Var
   i, j: Integer;
   notable: Boolean;
Begin
   notable := True;
   cLine := 1;
   Infile.Seek(0, soBeginning);

   For i := 1 To FMaxHeaderLines Do
   Begin
      If fgets(Infile, abLine, MAXLINELEN) = EOF Then break;
      //IF EndOfFile THEN Break;

      inc(cLine);

      If StrLComp(abLine, 'begin ', 6) = 0 Then
      Begin
         StrLCopy(abLine, abLine + 6, Strlen(abLine) - 5);

         // find the filename in this line.. bypassing spaces and numeric chars 
      	j := 0;
         While (Byte(abLine[j]) > 47) And
         		(Byte(abLine[j]) < 58) {Or
               (Byte(abLine[j]) = 32)} Do
         	Inc(j);

         If Byte(abLine[j]) = 32 Then
         	Inc(j);
            
         Result := StrPas(abLine + j);

         If Pos(' ', Result) > 0 Then
            SetLength(Result, Pos(' ', Result) - 1);

         get_encmethod(Infile, notable);
         break;
      End
      Else
         If StrLComp(abLine, 'size', 4) = 0 Then
            osize := StrToInt(StrPas(StrLCopy(abLine, abLine + 5, Strlen(abLine) - 4)))
         Else
            If StrLComp(abLine, 'table', 5) = 0 Then
            Begin
               notable := False;
               fgets(Infile, abLine, MAXLINELEN);
               fgets(Infile, abLine + 32, MAXLINELEN - 32);
               CopyMem(@abLine[0], @sXXtable[0], 64);
            End
            Else
               If StrLComp(abLine, 'charmap', 7) = 0 Then
               Begin
                  notable := False;
                  CopyMem(@abLine[i + 8], @sXXtable[i], 64);
               End;

      If EndOfFile Then break;
   End;
End;
//-------------------------------------------------------------

Function Decode_Line(sLine: PChar; NumBytes: Integer): Integer;
Var
   i, j: Integer;
   k1, k2, k3, k4: Integer;
Begin
   //NumBytes := piNNmap[Integer( sLine[0] )];
   //Inc( sLine );

   i := 0;
   j := 0;

   While i < NumBytes Do
   Begin
      k1 := piNNmap[Byte(sLine[j])];
      k2 := piNNmap[Byte(sLine[j + 1])];
      k3 := piNNmap[Byte(sLine[j + 2])];
      k4 := piNNmap[Byte(sLine[j + 3])];

      //IF k1 < 0 THEN k1 := recover( j	  , nbytes3, len );
      //IF k2 < 0 THEN k2 := recover( j + 1, nbytes3, len );
      //IF k3 < 0 THEN k3 := recover( j + 2, nbytes3, len );
      //IF k4 < 0 THEN k4 := recover( j + 3, nbytes3, len );
      //IF ( k1 < 0 ) OR ( k2 < 0 ) OR ( k3 < 0 ) OR ( k4 < 0 ) THEN
      //   Break;

      sLine[i] := char((k1 Shl 2) Or (k2 Shr 4));
      sLine[i + 1] := char((k2 Shl 4) Or (k3 Shr 2));
      sLine[i + 2] := char((k3 Shl 6) Or k4);

      inc(i, 3);
      inc(j, 4);
   End;
   Result := NumBytes;
End;
//-------------------------------------------------------------

Function TUUDecode.decode_file(Infile, Outfile: TStream32; Var fSize: Integer): Integer;
Var
   ch, i, NumBytes: Integer;
Begin
   Result := 1;
   If GetString(Infile, abLine) > 0 Then
      Repeat
         If Cancel Then break;

         If StrLComp(abLine, 'end', 3) = 0 Then
         Begin
            Result := 0;
            break;
         End;

         NumBytes := piNNmap[Integer(abLine[0])];
         If NumBytes = -1 Then
            break;

         Decode_Line(@abLine[1], NumBytes);
         WriteBuf(Outfile, @abLine[1], NumBytes);
         inc(fSize, NumBytes);

         (* skip remaining chars on current line *)
         i := -1;
         ch := Byte(inbuf[CurInbufPos]);
         While (piNNmap[ch] >= 0) Do
         Begin
            If Bytes_To_Go > 0 Then
               inc(i)
            Else
               break;

            ch := eGetNextChar(Self, Infile);
         End;

         (* no message for ending shorter line *)
         If (i > 0) And (NumBytes = iNNlen) Then
            RaiseErrorStr(fVolumeName, '', '0', E_EOL);

      Until GetString(Infile, abLine) {;Bytes_To_Go} = 0;

   If OutBufPos > 0 Then
   Begin
      (* no crc calculations *)
      OutBufPos := ExtractWriteBlock(Outfile, outbuf^, False, 0, OutBufPos, dtData);
      If OutBufPos = 0 Then doWriteError;
   End;
End;
//-------------------------------------------------------------

Function TUUDecode.get_start(Infile: TStream32; infname: String): Boolean;
Var
   ch: Integer;
Begin
   nobegin := True;
   cLine := 1;
   chFirst := 0;

   fLOF := Infile.Size;

   Bytes_To_Go := fLOF;
   ProgUpdate := fLOF;
   fTotalPackedSize := fLOF;

   EndOfFile := False;
   CurInbufPos := 0;
   InBufPos := 0;
   OutBufPos := 0;
   BytesRead := 0;

   ch := eGetNextChar(Self, Infile);
   If ch = iXXlen Then
      nobegin := False;

   While (nobegin) Do
   Begin
      abLine[0] := char(ch);
      If (ch <> 10) And (fgets(Infile, abLine + 1, MAXLINELEN) = EOF {0}) Then
         break;

      inc(cLine);

      If StrLComp(abLine, 'begin', 5) = 0 Then
         nobegin := False
      Else
      Begin
         ch := eGetNextChar(Self, Infile);
         If ch = iXXlen Then
            nobegin := False;
      End;
   End;

   If nobegin Then
      //RaiseErrorStr( FVolumeName, '', '0', E_NODECODE )
   Else
      If ch = iXXlen Then
      Begin
         chFirst := ch;
         RaiseErrorStr(fVolumeName,
            'line starting with ' + IntToStr(chFirst) + ' found before "begin"',
            '0',
            E_LINEBEGIN);
      End;
   Result := True;
End;
//-------------------------------------------------------------

Procedure TUUDecode.ExtractIT(Var Infile: TStream32; Outfile: TStream32);
Var
   EndFound: Boolean;
   fnum, fSize: Integer;
   sInfile, Vol_Count: String;
Begin
	fLOF := Infile.Size;
   Bytes_To_Go := fLOF;
   ProgUpdate := fLOF;

   // use the following block, instead of the min function...
   // the min function fails with files > 4 gig.
   //MaxMem := Min(Bytes_To_Go, WSIZE + 1);
   If Bytes_To_Go > WSIZE + 1 Then
      MaxMem := WSIZE + 1
   Else
      MaxMem := Bytes_To_Go;

   EndOfFile := False;
   CurInbufPos := 0;
   InBufPos := 0;
   OutBufPos := 0;
   BytesRead := 0;
   Count := 0;
   fCancel := False;
   //osize 	 	:= 0;
   fnum := FVolumeStartNum;
   fSize := 0;
   chFirst := 0;
   OutBufPos := 0;
   iXXlen := 104;

   Try

      sMethod := 'XXdecode';
      sXXtable := XXETABLE;
      sUUtable := UUETABLE;

      Try
         inbuf := Nil;
         GetMem(inbuf, MaxMem + 1);
         outbuf := Nil;
         GetMem(outbuf, MaxMem + 1);
         abLine := Nil;
         GetMem(abLine, MAXLINELEN + 1);

         ActualFilename := get_outfname(Infile);
   		If WriteMethod = faVerify Then
         	fFileName := ActualFilename
         Else
         	// format the output filename
         	FileName := ActualFilename;

         Count := 0;
         If doOnBegin(False) Then
         Try
            If Not Open_OutFile(Outfile, fFileName, ActualFilename) Then
               RaiseError(E_RAISE, fFileName, '', '0', E_FOPEN);

            Try
               fVolumeName := fArchiveFile;
               EndFound := False;

               Repeat
                  If Cancel Then break;

                  Case decode_file(Infile, Outfile, fSize) Of
                     0:
                        Begin
                           EndFound := True;
                           break;
                        End;

                     1:
                        Begin
                           If Cancel {OR EndOfFile} Then break;

                           inc(fnum);
                           Vol_Count := IntToStr(fnum);

                           sInfile :=
                           	GetNextVolName(
                              	fArchiveFile,
                                 fnum,
                                 FNextVolumeName,
                                 FVolumeLongFilenames);

                           If Not GetNextVolume(sInfile, Vol_Count) Then
                              Exit;

                           Try
                           	Infile.Free();

                              Infile :=
                                 TFileStream32.Create(sInfile,
                                    fmOpenRead Or fmShareDenyNone);

                        		If (TFileStream32(Infile).Handle < 0) Then
                              Begin
                                 RaiseErrorStr(sInfile, '', Vol_Count, E_FOPEN);
                                 Exit;
                              End;
                           Except
                              Infile :=
                                 TFileStream32.Create(fArchiveFile,
                                    fmOpenRead Or fmShareDenyNone);

                        		If (TFileStream32(Infile).Handle < 0) Then
                              Begin
                                 RaiseErrorStr(sInfile, '', Vol_Count, E_FOPEN);
                                 Exit;
                              End;

                              RaiseError(E_RAISE, fArchiveFile, '', Vol_Count, E_FCLOSE);
                           End;

                           (* find start of encoded text *)
                           If Not get_start(Infile, sInfile) Then
                              break;

                        End;
                  End;
               Until 1 <> 1;

               If Not EndFound Then
                  RaiseErrorStr(fVolumeName, '', '0', E_EOLNOTFOUND);

               //IF ( osize > 0 ) AND ( osize <> fsize ) THEN
               //   IF ( abLine[0] <> 'l' )  THEN
               //		RaiseErrorStr( Vol_Dir + FVolumeName, '', '0', E_FILESIZEMATCH );

            Finally
               If WriteToFile Then
               	Outfile.Free();
            End;
         Finally
            If Assigned(OnEnd) Then
               OnEnd(Self, fFileName, Cancel = False)
         End;
      Finally
         If abLine <> Nil Then FreeMem(abLine);
         If inbuf <> Nil Then FreeMem(inbuf);
         If outbuf <> Nil Then FreeMem(outbuf);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

End.
