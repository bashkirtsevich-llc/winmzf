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
{ NOTES:

  1. **Set the ArchiveFile property prior to setting any other property!
  2. NumberOfVolumes: this property returns the actual number of volumes,
     but does not count the output .zip file.
}

Unit ztvArchiveSplitter;

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
  	ztvRegister,
  	ztvBase,
  	ztvStreams,
  	ztvHeaders;


Type
   TOnFileExists =
      Procedure(Sender: TObject; SplitArchiveFile: String; Var Overwrite: Boolean)
      Of Object;

Const
	ztv_OneKB = 1024;
	ztv_OneMB = ztv_OneKB * 1000;
	MIN_CUST_SIZE = 65536;
   //VolumeSizeNames: Array[Ord(vs1mb)..Ord(vsOtherSize)] Of String =
   //('1.0mb',
   // '1.2mb floppy',
   // '1.44mb floppy',
   // '2.88mb floppy',
   // '100mb ZipDisk',
   // '250mb ZipDisk',
   // '650mb CD-Rom',
   // '700mb CD-Rom',
   // 'User defined');

Type
  	TArchiveSplitter = Class(TZipCommon)
	Private
   	fCustomSizeType: TCustomSizeType;
      fOnFileExists: TOnFileExists;
      fOnNewVolume: TOnNewVolume;
      fSplitArchiveFile: String;
      fWorking: Boolean;
		Function VerifyChunkSize: Boolean;
      Procedure SetPartSize(SKB: Cardinal);
      Function GetNumberOfVolumes: Cardinal;
      Function GetNumberOfVolumesStr: String;
      Procedure SetVolumeSizeNames(SVSN: TStrings);
  	Protected
   	fPartSize: Cardinal;
      fPartType: TPartType;
      fVolumeSizeNames: TStrings;
      Procedure SetPartType(SFS: TPartType); Override;
      Procedure SetArchiveFile(SFN: String); Override;
  	Public
  		Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Activate;
      Function GetSizeOfArchive: Int64;
      Function GetSizeOfArchiveStr: String;
		Function GetVolumeSizeInBytes(GSIB: TPartType): Int64;
      Function GetVolumeSizeInBytesStr: String;
		Function BytesToKBytes(Bytes: Integer): Int64;
		Function KBytesToBytes(KBytes: Integer): Int64;
		Function MBytesToBytes(MBytes: Integer): Int64;
      Property VolumeSizeNames: TStrings Read fVolumeSizeNames{GetVolumeSizeNames}
      	Write SetVolumeSizeNames;
  	Published
      Property ArchiveFile;
      Property ArcType;
      Property CustomSizeType: TCustomSizeType Read fCustomSizeType Write fCustomSizeType Default stBytes;
      Property NumberOfVolumes: Cardinal Read GetNumberOfVolumes Stored False;
      Property NumberOfVolumesStr: String Read GetNumberOfVolumesStr Stored False;
      Property SplitArchiveFile: String Read fSplitArchiveFile Write fSplitArchiveFile Stored False;
      Property PartType: TPartType Read fPartType Write SetPartType Default ptOtherSize;
		Property PartSize: Cardinal Read fPartSize Write SetPartSize Default 65536;
      Property OnError;
      Property OnFileExists: TOnFileExists Read fOnFileExists Write
         fOnFileExists;
      Property OnProgress;
      Property OnNewVolume: TOnNewVolume Read fOnNewVolume Write fOnNewVolume;
  	End;


Function IntToExtended(i: Integer): Extended;

Implementation

Uses
	ztvGbls,
   Err_Msgs;

//-------------------------------------------------------------

Function IntToExtended(i: Integer): Extended;
Begin
   Result := i;
End;
//-------------------------------------------------------------

Constructor TArchiveSplitter.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fWorking := False;
   ConfirmOverwrites := True;
   fPartType := ptOtherSize;
   fPartSize := 65536;
   fCustomSizeType := stBytes;
   fVolumeSizeNames := TStringList.Create();
   fVolumeSizeNames.Add('1.0mb');
   fVolumeSizeNames.Add('1.2mb floppy');
   fVolumeSizeNames.Add('1.44mb floppy');
   fVolumeSizeNames.Add('2.88mb floppy');
   fVolumeSizeNames.Add('100mb ZipDisk');
   fVolumeSizeNames.Add('250mb ZipDisk');
   fVolumeSizeNames.Add('650mb CD-Rom');
   fVolumeSizeNames.Add('700mb CD-Rom');
   fVolumeSizeNames.Add('Other Size');
End;
//-------------------------------------------------------------

Destructor TArchiveSplitter.Destroy;
Begin
	fVolumeSizeNames.Free();
   Inherited Destroy();
End;
//-------------------------------------------------------------

Function TArchiveSplitter.VerifyChunkSize: Boolean;
Begin
   Case fArcType Of
      atZip,
         atZipExe,
         atJar,
         atJarExe:
         	Result := fPartSize >
            	SizeOf(TLocal) + SizeOf(TCentral) + SizeOf(TEnd) + 1;
   Else
      Begin
         RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
         Result := False;
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TArchiveSplitter.Activate;
Var
	TotalVolumes: Cardinal;
	LHeader: TLocal;
   CHeader: TCentral;
   i, j: Integer;
   Overwrite,
   	LEncrypted,
   	CEncrypted: Boolean;
   Signature: Integer;
   BW,
      BytesToWrite,
   	BytesWritten: Int64;
	InStream,
   	OutStream,
		CentralHeaderStrm: TStream32;

	Function OpenOutfile(Var OutStream: TStream32; Index: Integer): Boolean;
   Var
   	SplitFileName: String;
      ProgressPercent: Byte;
   Begin
   	BytesWritten := 0;

      If Assigned(OnProgress) Then
      Begin
      	ProgressPercent :=
         	ztvGbls.CalcProgress32(Index, TotalVolumes);

      	OnProgress(Self, ProgressPercent, ProgressPercent);
      End;

		If Cardinal(Index) > TotalVolumes Then
      	SplitFileName := fSplitArchiveFile
      Else
         If Index < 100 Then
         Begin
            SplitFileName := ChangeFileExt(fSplitArchiveFile, format('.z%2.2d',[Index]))
         End Else
            SplitFileName := ChangeFileExt(fSplitArchiveFile, format('.z%3.3d',[Index]));

   	If OutStream <> Nil Then
      	OutStream.Free();

      OutStream :=
         TFileStream32.Create(SplitFileName, fmCreate Or fmShareExclusive);

      Result := TFileStream32(OutStream).Handle > 0;

      If Result Then
      Begin
      	OutStream.CancelCallBackProc := @fCancel;
         If Assigned(OnNewVolume) Then
         	OnNewVolume(Self, SplitFileName);
      End;
   End;

	Procedure WriteData(inStream: TStream32; Size: Cardinal; Var Index: Integer);
   Var
   	BW: Int64;
   Begin
      While Size > 0 Do
      Begin

      	If BytesWritten + Size > fPartSize Then
         	BytesToWrite := fPartSize - BytesWritten
         Else
         	BytesToWrite := Size;

         BW := outStream.CopyFrom(inStream, BytesToWrite);
         Dec(Size, BW);
         BytesWritten := BytesWritten + BW;

         If Size > 0 Then
         Begin
         	Inc(Index);
            OpenOutfile(OutStream, Index);
         End;
      End;
   End;

   Procedure WriteCentralDir(Var Index: Integer);
   Begin
      CentralHeaderStrm.Seek(0, soBeginning);
      EndZipHeader.CentralDirOffset := outStream.Position;
      WriteData(CentralHeaderStrm, EndZipHeader.SizeOfCentralDir, Index);
   End;

   Function GetNextCentralHdr(Var Encrypted: Boolean): Boolean;
   Begin
   	ZeroMemory(@CentralZipHeader, SizeOf(CentralZipHeader.SignAtr));
      If CentralHeaderStrm.Read(CentralZipHeader, SizeOf(TCentral)) = SizeOf(TCentral) Then
      	Result := VerSig(CentralZipHeader.SignAtr, htCentral, Encrypted) = htCentral
      Else
      	Result := False;
   End;

Begin

   If fWorking Then Exit;

   fWorking := True;
   Try
      If (fArchiveFile <> '') And IsArcSplitable(fArcType) Then
      Begin
         If (fSplitArchiveFile = '') Or (CompareText(fSplitArchiveFile, fArchiveFile) = 0) Then
         Begin
            RaiseErrorStr(ArchiveFile, '', '0', E_NOTHINGTODO);
            Exit;
         End;

         If (fPartType = ptOtherSize) And (fPartSize < MIN_CUST_SIZE) Then
         Begin
            RaiseErrorStr(ArchiveFile, '', '0', E_SIZETOSMALL);
            exit;
         End;

         If (fPartSize >= FLOF) Or (fPartSize = 0) Then
         Begin
            RaiseErrorStr(ArchiveFile, '', '0', E_NOTHINGTODO);
            exit;
         End;

         If Not VerifyChunkSize() Then exit;

         If FileExists(SplitArchiveFile) Then
            If Assigned(fOnFileExists) Then
            Begin
               Overwrite := False;
               fOnFileExists(Self, SplitArchiveFile, Overwrite);
               If Not Overwrite Then Exit;
            End Else Begin
               RaiseErrorStr(ArchiveFile, 'OnFileExists', '0', E_REQUIREDEVENT);
               Exit;
            End;

         If Assigned(OnElapsedTime) Then
            ZipTimer.Start();

         OutStream := Nil;
         Try
            // v6.2 moved following line to top part of this proc
            //fPartSize := GetVolumeSizeInBytes(fPartType);
            TotalVolumes := NumberOfVolumes;

            InStream :=
               TFileStream32.Create(fArchiveFile, fmOpenRead Or fmShareDenyWrite);

            If TFileStream32(InStream).Handle < 0 Then
            Begin
               RaiseErrorStr(fArchiveFile, '', '0', E_FOPEN);
               Exit;
            End Else Begin

               Try
                  CentralHeaderStrm := TMemoryStream32.Create();
                  InStream.Position := EndZipHeader.CentralDirOffset;
                  CentralHeaderStrm.CopyFrom(InStream, EndZipHeader.SizeOfCentralDir);
                  CentralHeaderStrm.Seek(0, soBeginning);

                  //If Is64BitEndingHdr(EndZipHeader.SignAtr) Then

                  //fTotalUnpackedSize := 0;
                  //While GetNextCentralHdr(CEncrypted) Do
                  //Begin
                  //   Central64Hdr :=
                  //      Read64BitFieldHdr(
                  //         CentralHeaderStrm,
                  //         CentralZipHeader.zc.ExtraFieldLen,
                  //         htCentral);
                  //
                  //   fTotalPackedSize :=
                  //      (Int64(Central64Hdr.HiPackedSize) Shl 32) Or
                  //      CentralZipHeader.zc.PackedSize;
                  //End;
                  //CentralHeaderStrm.Seek(0, soBeginning);

                  i := 1;

                  If Not OpenOutfile(OutStream, i) Then
                     Exit;

                  j := 1;
                  While GetNextCentralHdr(CEncrypted) Do
                  Begin

                     If CEncrypted Then
                        DecodeHeader(@CentralZipHeader, htCentral);

                     // save a decrypted copy of the central header to work with
                     ZeroMemory(@CHeader, SizeOf(TCentral));
                     Move(CentralZipHeader, CHeader, SizeOf(CHeader));

                     If j = 1 Then
                     Begin
                        Inc(j);
                        Signature := MULTIVOL_HEADER_SIGNATURE;
                        outStream.WriteBuffer(Signature, SizeOf(Integer));
                        Inc(BytesWritten, SizeOf(Integer));
                     End;

                     InStream.Seek(CHeader.RelativeOffsetOfLocalHeader, soBeginning);
                     ZeroMemory(@LocalZipHeader, SizeOf(LocalZipHeader.SignAtr));
                     InStream.Read(LocalZipHeader, SizeOf(LocalZipHeader));

                     If VerSig(LocalZipHeader.SignAtr, htLocal, LEncrypted) = htLocal Then
                     Begin

                        If LEncrypted Then
                           DecodeHeader(@LocalZipHeader, htLocal);

                        // save a decrypted copy of the local header to work with
                        ZeroMemory(@LHeader, SizeOf(TLocal));
                        Move(LocalZipHeader, LHeader, SizeOf(LHeader));

                        If LEncrypted Then
                           EncodeHeader(@LocalZipHeader, htLocal);

                        CentralZipHeader.RelativeOffsetOfLocalHeader := outStream.Position;
                        CentralZipHeader.DiskNumberStart := i - 1;
                        If CEncrypted Then
                           EncodeHeader(@CentralZipHeader, htCentral);

                        CentralHeaderStrm.Seek(-SizeOf(TCentral), soCurrent);
                        CentralHeaderStrm.Write(CentralZipHeader, SizeOf(CentralZipHeader));
                        CentralHeaderStrm.Seek(
                           CHeader.ZC.FileNameLen +
                           CHeader.ZC.ExtraFieldLen +
                           CHeader.CommentLen,
                           soCurrent);

                        InStream.Seek(-SizeOf(TLocal), soCurrent);
                        BW :=
                           outStream.CopyFrom(inStream,
                              SizeOf(TLocal) +
                              LHeader.zc.FileNameLen +
                              LHeader.zc.ExtraFieldLen
                              );

                        BytesWritten := BytesWritten + BW;
                        If BytesWritten >= fPartSize Then
                        Begin
                           Inc(i);
                           OpenOutfile(OutStream, i);
                        End;

                        WriteData(inStream, LHeader.zc.PackedSize, i);
                     End;
                  End;

                  If BytesWritten + EndZipHeader.SizeOfCentralDir + SizeOf(TEnd) >
                     fPartSize Then
                  Begin
                     Inc(i);
                     OpenOutfile(OutStream, i);
                  End;

                  WriteCentralDir(i);

                  EndZipHeader.DiskWithStartOfCentral := i - 1; 	//NumberOfVolumes;
                  EndZipHeader.NumberOfThisDisk := i - 1;
                  outStream.Write(EndZipHeader, SizeOf(TEnd));
               Finally
                  If outStream <> Nil Then
                     OutStream.Free();
                  InStream.Free();
               End;
            End;
         Finally
            If Assigned(OnElapsedTime) Then
            Begin
               ZipTimer.Stop();
               OnElapsedTime(Self, ZipTimer.ElapsedTime);
            End;
         End;
      End Else Begin
         RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
      End;
   Finally
   	fWorking := False;
   End;

End;
//-------------------------------------------------------------

Function TArchiveSplitter.BytesToKBytes(Bytes: Integer): Int64;
Begin
	Result := Bytes Div ztv_OneKB;
End;
//-------------------------------------------------------------

Function TArchiveSplitter.MBytesToBytes(MBytes: Integer): Int64;
Begin
	Result := MBytes * ztv_OneMB;
End;
//-------------------------------------------------------------

Function TArchiveSplitter.KBytesToBytes(KBytes: Integer): Int64;
Begin
	Result := KBytes * ztv_OneKB;
End;
//-------------------------------------------------------------

Function TArchiveSplitter.GetSizeOfArchive: Int64;
Begin
   Result := LengthOfFile;
End;
//-------------------------------------------------------------

Function TArchiveSplitter.GetSizeOfArchiveStr: String;
Begin
	Result := Format('%.0n', [IntToExtended(GetSizeOfArchive())]);
End;
//-------------------------------------------------------------

// Returns KByte value
Function TArchiveSplitter.GetVolumeSizeInBytes(GSIB: TPartType): Int64;
Begin
   Result := 65536;
	Try
      Case GSIB Of
         pt1mb           : Result := ztv_OneMB;
         ptFloppy_120mb  : Result := 1213952;
         ptFloppy_144mb  : Result := 1457664;
         ptFloppy_288mb  : Result := 2915328;
         ptZipDisk_100mb : Result := ztv_OneMB * 100;
         ptZipDisk_250mb : Result := ztv_OneMB * 250;
         ptCDRom_650mb   : Result := ztv_OneMB * 650;
         ptCDRom_700mb   : Result := ztv_OneMB * 700;
         ptOtherSize    :
            Begin
               Case fCustomSizeType Of
                  stBytes: Result := fPartSize;
                  stKB: Result := KBytesToBytes(fPartSize);
                  stMB: Result := MBytesToBytes(fPartSize);
               Else
               	Result := -1;
               End;
            End;
			ptNoSplit:
         	If IsArcValid(ArcType) Then
         		Result := fLOF
            Else
            	Result := 0;
      Else
         Result := -1;	//error
      End;
   Finally
      If ArchiveFile <> '' Then
         If (Result < 0) Or (Result > fLOF) Then
            Result := fLOF;
   End;
End;
//-------------------------------------------------------------

Function TArchiveSplitter.GetVolumeSizeInBytesStr: String;
Begin
	Result := Format('%.0n',[IntToExtended(GetVolumeSizeInBytes(PartType))]);
End;
//-------------------------------------------------------------

Procedure TArchiveSplitter.SetArchiveFile(SFN: String);
Begin
   If SFN = '' Then
   Begin
   	fArchiveFile := '';
      fArcType := atNA;
   End Else
		Inherited SetArchiveFile(SFN);
End;
//-------------------------------------------------------------

Procedure TArchiveSplitter.SetPartType(SFS: TPartType);
Begin
	//ShowMessage(VolumeSizeNames[Ord(SFS)]);
	fPartType := SFS;
   If SFS = ptOtherSize Then
      fPartSize := 65536
   Else
      fPartSize := GetVolumeSizeInBytes(SFS);
End;
//-------------------------------------------------------------

Procedure TArchiveSplitter.SetPartSize(SKB: Cardinal);
Begin
	If fPartType = ptOtherSize Then
   	If SKB < MIN_CUST_SIZE Then
      	fPartSize := MIN_CUST_SIZE
      Else
   		fPartSize := SKB;

   If IsArcSplitable(fArcType) Then
   	If fPartSize > fLOF Then
      	fPartSize := fLOF;
End;
//-------------------------------------------------------------

Function TArchiveSplitter.GetNumberOfVolumes: Cardinal;
Begin
	If IsArcSplitable(ArcType) Then
   Begin
   	Result := fLOF Div GetVolumeSizeInBytes(PartType)
   End Else
   	Result := 0;
End;
//-------------------------------------------------------------

Function TArchiveSplitter.GetNumberOfVolumesStr: String;
Begin
	Result := IntToStr(GetNumberOfVolumes);
End;
//-------------------------------------------------------------

Procedure TArchiveSplitter.SetVolumeSizeNames(SVSN: TStrings);
Begin
   fVolumeSizeNames.Assign(SVSN);
End;
//-------------------------------------------------------------



End.
