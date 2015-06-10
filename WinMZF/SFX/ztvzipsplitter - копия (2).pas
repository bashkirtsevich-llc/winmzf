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
  	ALWAYS set the ArchiveFile property prior to setting any other property!

	The following properties are included in the TZipSplitter component,
   but are not present in the TZip compressor component:

   1. VolumeSizeNames: TStrings (defined in ztvZipSplitter)
      A string listing describing the set members in the PartType property.
      Used to fill a combobox with PartType names for user selection.

   2. PartType: TPartType  (defined in ztvArchiveSplitter.pas)

      TPartType =
         (vs1mb, vsFloppy_120mb, vsFloppy_144mb, vsFloppy_288mb,
         vsZipDisk_100mb, vsZipDisk_250mb, vsCDRom_650mb, vsCDRom_700mb,
         vsOtherSize, vsNoSplit);

      This property determines the sizes of output volume sizes.
   --> 	- If PartType = vsNoSplit, a normal zip archive is created
         - If PartType = vsOtherSize, the user can enter the desired size of
         the output volumes into the PartSize property.
         - If PartType equals any of the predefined size settings (for example
         vsFloppy_120mb), the PartSize property is automatically defined with
         a predetermined volume size.
         - All PartType settings automatically set the PartSize property with
         predefined sizes with the exception of vsOtherSize (user defined
         setting) and vsNoSplit (no archive split will take place).
         - The default = vsNoSplit


	3. PartSize: Cardinal (defined in ztvZipSplitter.pas)
         - This property is automatically assigned a volumesize if the PartType
         property (see PartType above) equals one of the predefined settings.
         Predefined settings are all PartType selections with the exception of
         the vsOtherSize and vsNoSplit members.

	4. CustomSizeType: TCustomSizeType (defined in ztvArchiveSplitter.pas)
      	TCustomSizeType = (stBytes, stKB, stMB);
         - This property is used only when the PartType property equals
         vsOtherSize.
         - CustomSizeType = stBytes, the value of the PartSize property is
         calculated in bytes. If CustomSizeType = stKB, the value of the
         PartSize property is calculated based on kbytes... etc.
			- The default = stBytes

   5. To activate compression use the "Compress" method (same as TZip).

   6. All properties available in the TZip component are also present in
      TZipSplitter with the exception of the Switch property.  At present
      files can not be added or deleted from a split archive.  Only the
      creation of split archives is supported.       

   7. Since adding & deleting files are not yet supported with split archives,
      if the file specified in the ArchiveFile property exists, no task is
      preformed.  If the archive exists, you must first delete it prior to
      calling the Compress method.

  *)

Unit ztvZipSplitter;

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
   ztvZip,
   ztvArchiveSplitter;

Type
   TZipSplitter = Class(TZipBase)
   Private
   	fOnNewVolume: TOnNewVolume;
		ArchiveSplitter: TArchiveSplitter;
   Protected
		Function GetCustomSizeType: TCustomSizeType; Override;
		Function GetPartSize: Cardinal; Override;
		Function GetPartType: TPartType; Override;
      Function GetVolumeSizeNames: TStrings; Override;
      Procedure SetCustomSizeType(SCST: TCustomSizeType); Override;
      Procedure SetPartSize(SKB: Cardinal); Override;
      Procedure SetPartType(SFS: TPartType); Override;
      Procedure SetVolumeSizeNames(SVSN: TStrings); Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Loaded; Override;
      Procedure TempFileMoveFile(Sender: TTempFileStream; ToFileName,
      	FromFileName: AnsiString; SameDrive: Boolean; Size: Int64); Override;
      Function Compress: Integer; Override;
      Function GetSizeOfArchive: Int64;
      Function GetSizeOfArchiveStr: String;
		Function GetVolumeSizeInBytes(GSIB: TPartType): Int64;
      Function GetVolumeSizeInBytesStr: String; Override;
		Function BytesToKBytes(Bytes: Integer): Int64;
		Function KBytesToBytes(KBytes: Integer): Int64;
		Function MBytesToBytes(MBytes: Integer): Int64;
      Property VolumeSizeNames;
   Published
      Property CustomSizeType;
      Property PartType;
		Property PartSize;
      Property OnProgress;
      Property OnNextVolume;
      Property OnNewVolume: TOnNewVolume Read fOnNewVolume Write fOnNewVolume;
   End;


Implementation

Uses
	ztvGbls,
	Err_Msgs;

//-------------------------------------------------------------

Constructor TZipSplitter.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   ConfirmOverwrites := True;
   ArchiveSplitter := TArchiveSplitter.Create(Nil);
   PartType := ptNoSplit;
   PartSize := 65536;
   CustomSizeType := stBytes;
   VolumeSizeNames.Add('No split');
End;
//-------------------------------------------------------------

Destructor TZipSplitter.Destroy;
Begin
   ArchiveSplitter.Free();
   Inherited Destroy();
End;
//-------------------------------------------------------------

Procedure TZipSplitter.Loaded;
Begin
   Inherited Loaded;
	ArchiveSplitter.OnError := OnError;
   //ArchiveSplitter.PartType := ptNoSplit;
   //ArchiveSplitter.PartSize := 65536;
   //ArchiveSplitter.CustomSizeType := stBytes;
   ArchiveSplitter.OnProgress := OnProgress;
	ArchiveSplitter.OnNewVolume := OnNewVolume;
   //ArchiveSplitter.VolumeSizeNames.Add('No split');
   //fArcType := atZipMV;
End;
{-------------------------------------------------------------}

Procedure TZipSplitter.TempFileMoveFile(Sender: TTempFileStream; ToFileName,
	FromFileName: AnsiString; SameDrive: Boolean; Size: Int64);
Var
	VolSize: Int64;
Begin
	ArchiveSplitter.ArchiveFile := FromFileName;
   VolSize := GetVolumeSizeInBytes(PartType);

   // if writing to diskette, no split volume
	If DiskManager.DriveType = dtFloppy Then
   	ArchiveSplitter.PartType := ptNoSplit;

   If (ArchiveSplitter.PartType <> ptNoSplit) And (ArchiveSplitter.fLOF > VolSize) Then
   Begin
   	If FileExists(ToFileName) Then
      	DeleteFile(ToFileName);

		ArchiveSplitter.SplitArchiveFile := ToFileName;
      ArchiveSplitter.Activate();
   End Else
      Inherited;
End;
//-------------------------------------------------------------

Function TZipSplitter.GetCustomSizeType: TCustomSizeType;
Begin
	Result := ArchiveSplitter.CustomSizeType;
End;
//-------------------------------------------------------------

Procedure TZipSplitter.SetCustomSizeType(SCST: TCustomSizeType);
Begin
	ArchiveSplitter.CustomSizeType := SCST;
End;
//-------------------------------------------------------------

Function TZipSplitter.GetPartType: TPartType;
Begin
	Result := ArchiveSplitter.PartType;
End;
//-------------------------------------------------------------

Procedure TZipSplitter.SetPartType(SFS: TPartType);
Begin
	ArchiveSplitter.PartType := SFS;
End;
//-------------------------------------------------------------

Function TZipSplitter.GetPartSize: Cardinal;
Begin
	Result := ArchiveSplitter.PartSize;
End;
//-------------------------------------------------------------

Procedure TZipSplitter.SetPartSize(SKB: Cardinal);
Begin
	ArchiveSplitter.PartSize := SKB;
End;
//-------------------------------------------------------------

Function TZipSplitter.GetSizeOfArchive: Int64;
Begin
   Result := ArchiveSplitter.GetSizeOfArchive();
End;
//-------------------------------------------------------------

Function TZipSplitter.GetSizeOfArchiveStr: String;
Begin
	Result := ArchiveSplitter.GetSizeOfArchiveStr();
End;
//-------------------------------------------------------------

Function TZipSplitter.GetVolumeSizeInBytes(GSIB: TPartType): Int64;
Begin
	Result := ArchiveSplitter.GetVolumeSizeInBytes(GSIB);
End;
//-------------------------------------------------------------

Function TZipSplitter.GetVolumeSizeInBytesStr: String;
Begin
	Result := ArchiveSplitter.GetVolumeSizeInBytesStr();
End;
//-------------------------------------------------------------

Function TZipSplitter.BytesToKBytes(Bytes: Integer): Int64;
Begin
	Result := ArchiveSplitter.BytesToKBytes(Bytes);
End;
//-------------------------------------------------------------

Function TZipSplitter.KBytesToBytes(KBytes: Integer): Int64;
Begin
	Result := ArchiveSplitter.KBytesToBytes(KBytes);
End;
//-------------------------------------------------------------

Function TZipSplitter.MBytesToBytes(MBytes: Integer): Int64;
Begin
	Result := ArchiveSplitter.MBytesToBytes(MBytes);
End;
//-------------------------------------------------------------

Function TZipSplitter.Compress: Integer;
Begin
	Result := 0;
	If IsArcSplitable(fArcType) Then
			Result := Inherited Compress;
End;
//-------------------------------------------------------------

Function TZipSplitter.GetVolumeSizeNames: TStrings;
Begin
	Result := ArchiveSplitter.VolumeSizeNames;
End;
//-------------------------------------------------------------

Procedure TZipSplitter.SetVolumeSizeNames(SVSN: TStrings);
Begin
   ArchiveSplitter.VolumeSizeNames.Assign(SVSN);
End;
//-------------------------------------------------------------

End.
