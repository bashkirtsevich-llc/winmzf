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
Unit ztvArc2Arc;


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
  (* Global units *)
   ztvBase,
   ztvGbls,
   Err_Msgs,
  (* Utility compression components *)
   ztvZipTV,
   ztvZipCheck,
  (* Decompression/decoding components *)
   ztvUnBH,
   ztvUnARJ,
   ztvUnARC,
   ztvUnJAR,
   ztvUnZIP,
   ztvUnZOO,
   ztvUnTAR,
   ztvUnLHA,
   ztvUnGZip,
   ztvUnCab,
   ztvUnRAR,
   ztvUnACE2,
   ztvUUDecode,
  (* Compression/encoding components *)
   ztvZip,
   ztvBlakHole,
   ztvJar,
   ztvLHA,
   ztvMakeCab,
   ztvTar,
   ztvGZip,
   ztvUUEncode;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TOnFileExists =
      Procedure(Sender: TObject; FileName: String; Var OverwriteMode: TOverwriteMode)
      Of Object;

   TOutArcMethod = (amCreateNew, amAddToArc);
   TOutArcType = (tyBh, tyCab, tyLzh, tyZip, tyTar, tyTarGzip, tyJar);
   TOnIntegrityBegin = Procedure(Sender: TObject; FileName: String) Of Object;

   TArc2Arc = Class(TZipCommon)
   Private
      // CHRIS MODIFY START
      ExtractedNestedTar: Boolean;
      // CHRIS MODIFY END
      DeleteTheseFiles: String;
      ExtDir: String;
      ExtDirRoot: String;
      fCompBaseArcType: TArcType;
      fCounter: Integer;
      fIntegrityCheck: Boolean;
      fNewArchive: String;
      fOnActivate: TNotifyEvent;
      fOnBegin: TOnBegin;
      fOnCompressActivate: TNotifyEvent;
      fOnCompressBegin: TOnBegin;
      fOnCompressDeactivate: TNotifyEvent;
      fOnCompressEnd: TOnEnd;
      fOnDeactivate: TNotifyEvent;
      fOnEnd: TOnEnd;
      fOnFileExists: TOnFileExists;
      fOnIntegrityActivate: TNotifyEvent;
      fOnIntegrityDeactivate: TNotifyEvent;
      fOnIntegrityBegin: TOnIntegrityBegin;
      fOnIntegrityEnd: TOnIntegrityEnd;
      fOnIntegrityStatus: TOnIntegrityStatus;
      fOutArcMethod: TOutArcMethod;
      fOutArcType: TOutArcType;
      fTempFile: TextFile;
      Function DoDecompress: Boolean;
      Function GetNewFileName: String;

      Procedure ArcOnCompressBegin(Sender: TObject; FileName: String;
      	Count: Integer; Var Extract: Boolean);
      Procedure ArcOnCompressEnd(Sender: TObject; FileName: String;
      	CRC_PASS: Boolean);
      Procedure ArcOnCompressFileExists(Sender: TObject; FileName: String;
      	FileDate: TDateTime; Var OverwriteMode: TOverwriteMode);
      Procedure ArcOnDeactivate(Sender: TObject);
      Procedure ArcOnExtractBegin(Sender: TObject; FileName: String;
      	Count: Integer; Var Extract: Boolean);
      Procedure ArcOnExtractFileExists(Sender: TObject; FileName: String;
      	Var NewFilename: String; Var OverwriteMode: TOverwriteMode);
      Procedure ArcOnIntegrityBegin(Sender: TObject; FileName: String;
      	Count: Integer; Var Extract: Boolean);
      Procedure ArcOnNestedTarFile(Sender: TObject; FileName: String; Var doUnTar: Boolean);
		//Procedure doTarGzip;
      Procedure doVerify;
      Procedure RecompressExtractedFiles;
      Procedure RemoveDirTree(Dir: String);
      Procedure SetNewArchive(SNA: String);
      Procedure SetOutArcType(SOAT: TOutArcType);
      Procedure SetOutArcMethod(SOAM: TOutArcMethod);
   Protected
      Procedure ArcOnExtractEnd(Sender: TObject; FileName: String;
      	CRC_PASS: Boolean); Virtual;
      Procedure ArcOnExtractActivate(Sender: TObject); Virtual;
      Procedure ArcOnExtractDeactivate(Sender: TObject); Virtual;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Activate;
   Published
      Property ArchiveFile;
      Property Attributes;
      Property AttributesEx;
      Property ArcType;
      Property ConfirmOverwrites;
      Property DateAttribute;
      Property FileSpec;
      Property IntegrityCheck: Boolean Read fIntegrityCheck Write fIntegrityCheck Default True;
      Property NewArchive: String Read fNewArchive Write SetNewArchive;
      Property Passwords;
      Property PasswordAttempts;
   	//Property StoredDirNames;  (* Do not use... will store as sdRelative *)
      Property TempDir;
      Property OutArcMethod: TOutArcMethod Read fOutArcMethod Write SetOutArcMethod Default amCreateNew;
      Property OutArcType: TOutArcType Read fOutArcType Write SetOutArcType;

      Property OnError;
      Property OnExtractActivate: TNotifyEvent Read fOnActivate Write fOnActivate;
      Property OnExtractDeactivate: TNotifyEvent Read fOnDeactivate Write fOnDeactivate;
      Property OnExtractBegin: TOnBegin Read fOnBegin Write fOnBegin;
      Property OnExtractEnd: TOnEnd Read fOnEnd Write fOnEnd;

      Property OnCompressActivate: TNotifyEvent Read fOnCompressActivate Write fOnCompressActivate;
      Property OnCompressDeactivate: TNotifyEvent Read fOnCompressDeactivate Write fOnCompressDeactivate;
      Property OnCompressBegin: TOnBegin Read fOnCompressBegin Write fOnCompressBegin;
      Property OnCompressEnd: TOnEnd Read fOnCompressEnd Write fOnCompressEnd;

      Property OnIntegrityActivate: TNotifyEvent Read fOnIntegrityActivate Write fOnIntegrityActivate;
      Property OnIntegrityDeactivate: TNotifyEvent Read fOnIntegrityDeactivate Write fOnIntegrityDeactivate;
      Property OnIntegrityBegin: TOnIntegrityBegin Read fOnIntegrityBegin Write fOnIntegrityBegin;
      Property OnIntegrityEnd: TOnIntegrityEnd Read fOnIntegrityEnd Write fOnIntegrityEnd;
      Property OnIntegrityStatus: TOnIntegrityStatus Read fOnIntegrityStatus Write fOnIntegrityStatus;
      Property OnNestedTarFile;
      Property OnFileExists: TOnFileExists Read fOnFileExists Write fOnFileExists;
      Property OnProgress;
      Property OnGetPassword;
      Property OnNextVolume;
      Property OnRemoveTempfile;
   End;



Implementation


//-------------------------------------------------------------

Constructor TArc2Arc.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fOutArcMethod := amCreateNew;
   fIntegrityCheck := True;
   DateAttribute := daFileDate;
   Attributes := [fsArchive, fsHidden, fsReadOnly, fsSysFile];
	//AttributesEx := [];   
   ConfirmOverwrites := True;
End;
//-------------------------------------------------------------

Destructor TArc2Arc.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnExtractActivate(Sender: TObject);
Begin
   AssignFile(fTempFile, DeleteTheseFiles);
   Rewrite(fTempFile);

   If Assigned(OnExtractActivate) Then
      OnExtractActivate(Sender);
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnExtractBegin(Sender: TObject; FileName: String;
	Count: Integer; Var Extract: Boolean);
Begin
 (* write filename to tempfile *)
   Writeln(fTempFile, AnsiString(FileName));

   If Assigned(OnExtractBegin) Then
   Begin
      OnExtractBegin(Sender, TUnBASE(Sender).ActualFilename, Count, Extract);
      fCounter := Count;
   End;
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnExtractDeactivate(Sender: TObject);
Begin
   Flush(fTempFile);
   CloseFile(fTempFile);

   If Assigned(OnExtractDeactivate) Then
      OnExtractDeactivate(Sender);
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnExtractEnd(Sender: TObject; FileName: String;
	CRC_PASS: Boolean);
Begin
   If Assigned(OnExtractEnd) Then
      OnExtractEnd(Sender, FileName, CRC_PASS);
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnCompressFileExists(Sender: TObject; FileName: String;
	FileDate: TDateTime; Var OverwriteMode: TOverwriteMode);
Begin
   If Assigned(OnFileExists) Then
      OnFileExists(Sender, FileName, OverwriteMode);
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnExtractFileExists(Sender: TObject; FileName: String;
	Var NewFilename: String; Var OverwriteMode: TOverwriteMode);
Begin
   If Assigned(OnFileExists) Then
      OnFileExists(Sender, FileName, OverwriteMode);
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnIntegrityBegin(Sender: TObject; FileName: String;
	Count: Integer; Var Extract: Boolean);
Begin
   If Assigned(OnIntegrityBegin) Then
      OnIntegrityBegin(Sender, FileName);
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnCompressBegin(Sender: TObject; FileName: String;
	Count: Integer; Var Extract: Boolean);
Begin
   If Assigned(OnCompressBegin) Then
      OnCompressBegin(Sender, FileName, Count, Extract);
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnCompressEnd(Sender: TObject; FileName: String; CRC_PASS: Boolean);
Begin
   If Assigned(OnCompressEnd) Then
      OnCompressEnd(Sender, FileName, CRC_PASS);
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnDeactivate(Sender: TObject);
Begin
   If Assigned(OnCompressDeactivate) Then
      OnCompressDeactivate(Sender);

   If (TZipCommon(Sender).ArcType = atTar) And (OutArcType = tyTarGzip) Then
   Begin
      fOutArcType := tyTar;
      Try
         doVerify();
      Finally
         fOutArcType := tyTarGzip;
      End;
   End;
End;
//-------------------------------------------------------------

Procedure TArc2Arc.SetNewArchive(SNA: String);
Begin
   fNewArchive := SNA;
End;
//-------------------------------------------------------------

Procedure TArc2Arc.SetOutArcType(SOAT: TOutArcType);
Begin
   fOutArcType := SOAT;
End;
//-------------------------------------------------------------

Procedure TArc2Arc.SetOutArcMethod(SOAM: TOutArcMethod);
Begin
   fOutArcMethod := SOAM;
End;
//-------------------------------------------------------------

Procedure TArc2Arc.RemoveDirTree(Dir: String);
Var
   SearchRec: {TSearchRec32}TSearchRec;
Begin

  (* if root-dir is current... skip *)
   If (Pos(':\', Dir) > 0) And (Pos(':\', Dir) = Length(Dir) - 1) Then
      Exit;

   Try
{$I-}ChDir(Dir){$I+};
      If IoResult <> 0 Then Exit;

      //If ztvFindFirst('*', faAnyFile, SearchRec) = 0 Then
      If FindFirst('*', faAnyFile, SearchRec) = 0 Then
      Try
         Repeat

            If ((SearchRec.Attr And faDirectory) = faDirectory) And
               (SearchRec.name[1] <> '.') Then
            Begin

               RemoveDirTree(SearchRec.name);

{$I-}ChDir('..'){$I+};
               If IoResult <> 0 Then Exit;

                 (* Deletes an existing EMPTY ONLY directory *)
               RemoveDir(SearchRec.name);
            End;
         //Until ztvFindNext(SearchRec) <> 0;
         Until FindNext(SearchRec) <> 0;
      Finally
         //ztvGbls.ztvFindClose(SearchRec);
         FindClose(SearchRec);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Function TArc2Arc.DoDecompress: Boolean;
Var
   UnBase: TUnBASE;
Begin
   Result := False;
   Case ArcType Of

      atAce {,
    atArcExe}: UnBase := TUnACE.Create(Nil);

      atArc,
         atArcExe: UnBase := TUnARC.Create(Nil);

      atArj,
         atArjExe: UnBase := TUnArj.Create(Nil);

      atBh,
         atBhExe: UnBase := TUnBh.Create(Nil);

      atCab: UnBase := TUnCab.Create(Nil);

      atGZip: UnBase := TUnGZIP.Create(Nil);

      atJar: UnBase := TUnJar.Create(Nil);

      atLha,
         atLhaExe,
         atLzh,
         atLzhExe: UnBase := TUnLha.Create(Nil);

      atRar,
         atRarExe: UnBase := TUnRar.Create(Nil);

      atTar: UnBase := TUnTar.Create(Nil);

      atUUE: UnBase := TUUDecode.Create(Nil);

      atZipMV,
         atZip,
         atZipExe:
         Begin
            UnBase := TUnZip.Create(Nil);
            UnBase.ZipCmntBufSize := 32000;
         End;

      atZoo: UnBase := TUnZoo.Create(Nil);

   Else
      RaiseError(E_RAISE, fArchiveFile, '', '0', E_INVALIDARC);
      Exit;
   End;


   If UnBase <> Nil Then
   Begin
      UnBase.pCancel := pCancel;
      UnBase.Cancel := False;

      Try
         If Cancel Then Exit;

         ExtDir :=
            Copy(DeleteTheseFiles, 1, Length(DeleteTheseFiles) -
            Length(ExtractFileExt(DeleteTheseFiles)));

         CreateDirEx(ExtDir);
         ExtDirRoot := AppendDirTail(ExtractFilename(ExtDir));
         UnBase.ExtractDir := ExtDir;

        (* ------------------------------------------------------------*)
        (*	                            Extract                         *)
        (* ------------------------------------------------------------*)
         fCounter := 0;
         UnBase.ArchiveFile := ArchiveFile;
         UnBase.UseStoredDirs := True;
         UnBase.Passwords := Passwords;
         UnBase.PasswordAttempts := PasswordAttempts;
         UnBase.DateAttribute := daFileDate;
         UnBase.TranslateOemChar := TranslateOemChar;
         UnBase.OnError := OnError;
         UnBase.OnBegin := ArcOnExtractBegin;
         UnBase.OnEnd := ArcOnExtractEnd;
         UnBase.OnActivate := ArcOnExtractActivate;
         UnBase.OnDeactivate := ArcOnExtractDeactivate;
         UnBase.OnFileExists := ArcOnExtractFileExists; //OnFileExists;
         UnBase.OnGetPassword := OnGetPassword;
         UnBase.OnNextVolume := OnNextVolume;
         UnBase.OnProgress := OnProgress;

         // CHRIS MODIFY START
         if ArcType = atGZip then
         begin
           UnBase.OnNestedTarFile := ArcOnNestedTarFile;
           ExtractedNestedTar := False;
         end;
         // CHRIS MODIFY END

         UnBase.FileSpec.Clear();
         If FileSpec.Count > 0 Then
            UnBase.FileSpec.Assign(FileSpec)
         Else
            UnBase.FileSpec.Add('*.*');

         If UnBase.IsArcDecompressable(UnBase.ArcType) Then
         Begin
            UnBase.Extract();

            // CHRIS MODIFY START
            If (ArcType = atGZip) And ExtractedNestedTar Then
            Begin
            	// Delete the extracted nested Tar file from temp directory so that it's
              	// not added to the converted archive file.
              	If FileExists(AppendDirTail(ExtDir) + ChangeFileExt(ExtractFilename(ArchiveFile), '')) then
                	DeleteFile(AppendDirTail(ExtDir) + ChangeFileExt(ExtractFilename(ArchiveFile), ''));
            End;
            // CHRIS MODIFY END

            Result := True;
         End
         Else
         Begin
            RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
            Cancel := True;
         End;

         Passwords := UnBase.Passwords;
      Finally
         UnBase.Free();
      End;
   End;
End;
//-------------------------------------------------------------

Function TArc2Arc.GetNewFileName: String;
Var
	Ext, TarGzExt: String;
Begin
   If OutArcType = tyTarGzip Then
   Begin
      Ext := LowerCase(ExtractFileExt(fNewArchive));
      If (CompareText('.tgz', Ext) = 0) Then
      	Result := Copy(fNewArchive, 1, Length(fNewArchive) - 4) + '.tar'
      Else If (CompareText('.gz', Ext) = 0)  And (Length(fNewArchive) > 7) Then
      Begin
         TarGzExt :=
            LowerCase(
               Copy(
                  fNewArchive,
                  Length(fNewArchive) - 6,
                  Length(fNewArchive) - 6
               ));

         If CompareText('.tar.gz', TarGzExt) = 0 Then
            Result := Copy(fNewArchive, 1, Length(fNewArchive) - 3)
         Else
            If CompareText('.tgz', TarGzExt) = 0 Then
               Result := Copy(fNewArchive, 1, Length(fNewArchive) - 4)
            Else
               Result := fNewArchive;
      End Else
         Result := fNewArchive;

   End Else
      Result := fNewArchive;
End;
//-------------------------------------------------------------

{Procedure TArc2Arc.doTarGzip;
Var
   GZip: TGZip;
Begin
   //If CompressMethod = cmTarGzip Then
   Try
      GZip := TGZip.Create(Nil);
      Try
         GZip.pCancel := pCancel;
         //GZip.ArchiveFile := fArchiveFile + GZip.DefaultExt;
         //GZip.ArchiveFile := _ChangeFileExt(ArchiveFile, GZip.DefaultExt);
         GZip.isTarGZip := True;
         GZip.ArchiveFile := ArchiveFile + GZip.DefaultExt;
         GZip.FileSpec.Clear();
         GZip.FileSpec.Add(ArchiveFile);
         GZip.Switch := swAdd; //swMove;
         GZip.Extension := exNone;	//exAppendGZ;
         GZip.RecurseDirs := False;
         GZip.StoredDirNames := sdNone; //StoredDirNames;
         GZip.OnBegin := OnBegin;
         GZip.OnEnd := OnEnd;
         GZip.OnRead := OnRead;
         GZip.OnProgress := OnProgress;
         GZip.OnError := OnError;

         //GZip.ExcludedSpec.Clear();					// v4.7 added
         GZip.TranslateOemChar := TranslateOemChar;	// v4.7 added
         GZip.RecurseDirs := False;             // v4.7 added
         GZip.DateAttribute := DateAttribute;   // v4.7 added
         GZip.OnActivate := OnActivate;         // v4.7 added
         GZip.OnDeactivate := OnDeactivate;     // v4.7 added
         //GZip.OnFileExists := OnFileExists;		// v4.7 added
         GZip.Compress();	// activate compression

         (* Currently, GZip doesn't delete files via swMove *)
         //If ztvFileExists(GZip.ArchiveFile) Then
         //Begin
         //   //DeleteFile(ArchiveFile);
         //   fArchiveFile := GZip.fArchiveFile;
         //End;

      Finally
         GZip.Destroy();
      End;
   Except
   End;
End;}
//-------------------------------------------------------------

Procedure TArc2Arc.RecompressExtractedFiles;
Var
	TempName: String;
	SaveArcType: TArcType;
   fCompBASE: TCompBase;
Begin

   Case OutArcType Of
      tyBh:
         Begin
            fCompBASE := TBlakHole.Create(Nil);
            fCompBASE.CompressMethod := cmDeflate; //or cmFuse;
         End;
      tyCab:
         Begin
            fCompBASE := TMakeCab.Create(Nil);
            fCompBASE.CompressMethod := cmMsZip;
         End;
      tyLzh:
         Begin
            fCompBASE := TLha.Create(Nil);
            fCompBASE.CompressMethod := cmFrozen6;
         End;
      tyZip:
         Begin
            fCompBASE := TZip.Create(Nil);
            fCompBASE.CompressMethod := cmDeflate;
         End;
      tyTar:
         Begin
            fCompBASE := TTar.Create(Nil);
            fCompBASE.CompressMethod := cmTarred;
         End;
      tyTarGzip:
         Begin
            fCompBASE := TTar.Create(Nil);
            fCompBASE.CompressMethod := cmTarGzip;
         End;
      tyJar:
         Begin
            fCompBASE := TJar.Create(Nil);
            fCompBASE.CompressMethod := cmDeflate;
         End;
   Else
      Exit;
   End;

   Try
      fCompBaseArcType := fCompBASE.ArcType;

                              (* Define fCompBASE prior to call to		*)
                              (* ConvertDate so variable ArchiveDate 	*)
                              (* contains the correct date value 		*)
      TempName := fNewArchive;
      Try
      	SaveArcType := fArcType;
         fCompBASE.ArchiveFile := GetNewFileName();
         fCompBASE.fArcType := SaveArcType;	// ensure fArcType <> atTarGzip in ztvBase

         fNewArchive := fCompBASE.ArchiveFile;

         fCompBASE.DateAttribute := DateAttribute;
         fCompBASE.RecurseDirs := True;
         fCompBASE.Switch := swAdd;
         fCompBASE.StoredDirNames := sdRelative;
         fCompBASE.TranslateOemChar := TranslateOemChar;
         fCompBASE.ExcludeSpec.Clear();
         fCompBASE.FileSpec.Clear();
         fCompBASE.FileSpec.Add(AppendDirTail(ExtDir) + '*.*');
         //If Passwords.Count > 0 Then
         //   fCompBASE.Password := Passwords.Strings[0];
         fCompBASE.OnError := OnError;
         fCompBASE.OnFileExists := ArcOnCompressFileExists;
         fCompBASE.OnBegin := ArcOnCompressBegin;
         fCompBASE.OnEnd := ArcOnCompressEnd;
         fCompBASE.OnActivate := OnCompressActivate;
         fCompBASE.OnDeactivate := ArcOnDeactivate;
         fCompBASE.OnProgress := OnProgress;
         fCompBASE.pCancel := pCancel;
         fCompBASE.Compress();
      Finally
         fNewArchive := TempName;
      End;

      If fOutArcType = tyTarGzip Then
      	fCompBASE.doTarGzip(fNewArchive);

   Finally
      fCompBASE.Free();
   End;
End;
//-------------------------------------------------------------

Procedure TArc2Arc.ArcOnNestedTarFile(Sender: TObject; FileName: String;
	Var doUnTar: Boolean);
Begin
	// virtual method... DO NOT DELETE!

  // CHRIS MODIFY START
  If Assigned(OnNestedTarFile) Then
    OnNestedTarFile(Sender, FileName, doUnTar);

  // Set a flag to indicate nested tar file was extracted
  ExtractedNestedTar := doUnTar;
  // CHRIS MODIFY END
End;
//-------------------------------------------------------------

Procedure TArc2Arc.doVerify;
Var
   ZC: TZipCheck;
Begin
   If IntegrityCheck And Assigned(OnIntegrityStatus) Then
   Try
      ZC := TZipCheck.Create(Nil);
      Try
         ZC.UseStoredDirs := True;
         ZC.ArchiveFile := fNewArchive;
         ZC.Passwords := Passwords;
         ZC.FileSpec.Clear;
         ZC.FileSpec.Add('*.*');
         ZC.OnError := OnError;
         ZC.OnBegin := ArcOnIntegrityBegin;
         ZC.OnEnd := OnIntegrityEnd;
         ZC.OnActivate := OnIntegrityActivate;
         ZC.OnDeactivate := OnIntegrityDeactivate;
         ZC.OnGetPassword := OnGetPassword;
         ZC.OnProgress := OnProgress;
         ZC.OnStatus := OnIntegrityStatus;
         ZC.OnNextVolume := OnNextVolume;
         If OutArcType = tyTarGzip Then
            ZC.OnNestedTarFile := ArcOnNestedTarFile;

         If ZC.IsArcDecompressable(ZC.ArcType) Then
         Begin
            ZC.pCancel := pCancel;
            ZC.Activate();
         End
         Else
         Begin
            RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
            Exit;
         End;
      Finally
         If ZC <> Nil Then ZC.Destroy;
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Procedure TArc2Arc.Activate;
Var
   TmpFile: TextFile;
   TempName: String;
   PLine: Array[0..255] Of char;
Begin
   fCompBaseArcType := atNA;
   If (fArchiveFile <> '') And IsArcValid(ArcType) Then
   Try
      Cancel := False;
      DoProgress := RetProgress;
      DeleteTheseFiles := RemoveDirTail(GetTempFilenameStr(TempDir));

      Try
      	//TempName := fNewArchive;
      	TempName := GetNewFileName();
			If fOutArcType = tyTarGzip Then
         	fNewArchive := TempName + '.gz';

         If (OutArcMethod = amCreateNew) And FileExists(TempName) Then
         Begin

            If CompareText(fArchiveFile, TempName{fNewArchive}) = 0 Then
               RaiseError(E_RAISE, fArchiveFile, '', '0', E_FILESMATCH);

            If ConfirmOverwrites And WriteToFile Then
               If Assigned(OnFileExists) Then
                  OnFileExists(Self,
                     TempName,
                     //fNewArchive,
                     //ConvertDate( FileAge(fNewArchive) ),
                     fOverwriteMode)
               Else
                  Exit;

            If Cancel Or (fOverwriteMode = omSkip) Then Exit;
            DeleteFile(TempName{fNewArchive});
         End;

         //ExtDir := ExtractFileDir(fNewArchive);
         //If ExtDir <> '' Then
         //   CreateDirEx(ExtDir);

         If Assigned(OnElapsedTime) Then ZipTimer.START;

         Try

              (* ------------------------------------------------*)
              (*	                    Decompress                 *)
              (* ------------------------------------------------*)
            If (Not DoDecompress) Or Cancel Then Exit;

              (* ------------------------------------------------*)
              (*	                    Recompress                 	*)
              (* ------------------------------------------------*)
            RecompressExtractedFiles();

            If Cancel Then Exit;

              (* ------------------------------------------------*)
              (*	                    Verify                     	*)
              (* ------------------------------------------------*)
            doVerify();

         Finally

            Try
{$I-}
               AssignFile(TmpFile, DeleteTheseFiles);
               Reset(TmpFile);
{$I+}
               Try

                  While Not EOF(TmpFile) Do
                  Begin
{$I-}						ReadLn(TmpFile, PLine);
{$I+}
                     If Assigned(OnRemoveTempfile) Then
                        OnRemoveTempfile(Self, StrPas(@PLine));

                     DeleteFile(StrPas(@PLine));
                  End;

                  RemoveDirTree(ExtDir);

               Finally
{$I-}
						CloseFile(TmpFile);
{$I+}
               End;

            Finally

               If Assigned(OnElapsedTime) Then
               Begin
                  ZipTimer.Stop;
                  OnElapsedTime(Self, ZipTimer.ElapsedTime);
               End;
{$I-}
					ChDir('..')
{$I+};
               If IoResult = 0 Then
               Begin
                  RemoveDir(ExtDir);
                  If Assigned(OnRemoveTempfile) Then
                     OnRemoveTempfile(Self, ExtDirRoot);
               End;
            End;
         End;
      Finally
         DeleteFile(DeleteTheseFiles);
      End;
   Except
      On {E_RAISE}  e: exception Do ShowMessage(e.message);
   End;
End;
//-------------------------------------------------------------


End.
