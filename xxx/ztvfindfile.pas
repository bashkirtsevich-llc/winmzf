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
Unit ztvFindFile;


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
   ztvGbls,
   ztvFileScan,
   Err_Msgs,
  (* Utility units *)
   ztvZipTV,
  (* Decompression/decoding units *)
   ztvUnCab,
   ztvUnBH,
   ztvUnZIP,
   ztvUnZOO,
   ztvUnTAR,
   ztvUnGZip,
   ztvUnARC,
   ztvUnACE2,
   ztvUnARJ,
   ztvUnLHA,
   ztvUnRAR,
   ztvUUDecode,
   ztvUnJAR;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TOnArcChange = Procedure(Sender: TObject; ArchiveFile: String) Of Object;
   TOnCurrentDir = Procedure(Sender: TObject; Dir: String) Of Object;
   TOnCurrentFile = Procedure(Sender: TObject; FileName: String) Of Object;

   TztvFindFile = Class;

   TztvFindFile = Class(TZipCommon)
   Private
      DirectorySpec: String;
      pParentArchive: PChar;
      Pattern: String;
      fArchiveExt: TStringList;
      fDirSpec: String;
      fFileSpec: TStringList;
      fOnBegin: TOnBegin;
      fOnChangeDir: TOnRootDirChange;
      fOnCurrentArchive: TOnArcChange;
      fOnCurrentDir: TOnCurrentDir;
      fOnCurrentFile: TOnCurrentFile;
      fOnEnd: TOnEnd;
      fOnExtractActivate: TNotifyEvent;
      fOnExtractDeactivate: TNotifyEvent;
      fOnFoundFile: TNotifyEvent;
      fParentArchive: String;
      fRecurseNestedArchives: Boolean;
      fRecurseNestedProtectedArchives: Boolean;
      fRootDir: String;
      Procedure ArcOnActivate(Sender: TObject);
      Procedure ArcOnFileExists(Sender: TObject; FileName: String;
         Var NewFilename: String; Var OverwriteMode: TOverwriteMode); Virtual;
      Procedure ArcOnGetPassword(Sender: TObject; FN: String;
         Var Password: String; Var TryAgain: Boolean);
      Procedure ArcOnRead(Sender: TObject; Offset, Filenum: Integer);
      Procedure ArcOnRecurseDir(Sender: TObject; Dir: String);
      Procedure ReportDirChangeProc(CurrentDirectory: String);
      Procedure SetRootDir(SRD: String);
      Procedure SetFileSpec(SFS: TStringList);
      Procedure SetArchiveExt(SAE: TStringList);
      Procedure FindIT(DirSpec: String; PA: String);
      Procedure SetParentArchive(SPA: String);
      Procedure SearchRecProc(Index: Integer; Dir: String; FindData: TWin32FindData;
         pHeaderObj: pCompHeaderObj);
      Function GetParentArchive: String;
   Protected
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure DoUpdateFilesList(Const Dir: String; FindData: TWin32FindData;
         pHeaderObj: pCompHeaderObj);
      Procedure Execute;
      Procedure Find {( DirSpec: String )};
      Property ParentArchive: String Read GetParentArchive Write SetParentArchive;
   Published
      Property Attributes;
      Property AttributesEx;
      Property ArchiveExt: TStringList Read fArchiveExt Write SetArchiveExt;
      Property DirSpec: String Read fDirSpec Write fDirSpec;
      Property FileSpec: TStringList Read fFileSpec Write SetFileSpec;
      Property IncludeHiddenDirs;
      Property OnActivate;
      Property OnDeactivate;
      Property OnChangeDir: TOnRootDirChange Read fOnChangeDir Write fOnChangeDir;
      Property OnCurrentDir: TOnCurrentDir Read fOnCurrentDir Write fOnCurrentDir;
      Property OnCurrentFile: TOnCurrentFile Read fOnCurrentFile Write fOnCurrentFile;
      Property OnExtractActivate: TNotifyEvent Read fOnExtractActivate Write fOnExtractActivate;
      Property OnExtractBegin: TOnBegin Read fOnBegin Write fOnBegin;
      Property OnExtractDeactivate: TNotifyEvent Read fOnExtractDeactivate Write fOnExtractDeactivate;
      Property OnExtractEnd: TOnEnd Read fOnEnd Write fOnEnd;
      Property OnError;
      Property OnFoundFile: TNotifyEvent Read fOnFoundFile Write fOnFoundFile;
      Property OnCurrentArchive: TOnArcChange Read fOnCurrentArchive Write fOnCurrentArchive;
      Property OnGetPassword;
      Property OnGetZipFirstDisk;
      Property OnGetZipNextDisk;
      Property OnGetZipLastDisk;
      Property OnProgress;
      //PROPERTY  OnNextVolume;
      Property Passwords;
      Property RecurseNestedArchives: Boolean Read fRecurseNestedArchives Write fRecurseNestedArchives Default False;
      Property RecurseNestedProtectedArchives: Boolean Read
         fRecurseNestedProtectedArchives Write fRecurseNestedProtectedArchives
         Default False;
      Property RecurseDirs;
      Property RootDir: String Read fRootDir Write SetRootDir;
      Property ZipCmntBufSize;
   End;



Implementation


//-------------------------------------------------------------

Constructor TztvFindFile.Create(AOwner: TComponent);
Var
   i: Integer;
Begin
   Inherited Create(AOwner);
   doSearchRecAction := SearchRecProc;
   doReportDirChange := ReportDirChangeProc;
   fArchiveExt := TStringList.Create();
   fFileSpec := TStringList.Create();
   fRecurseNestedArchives := False;
   fRecurseNestedProtectedArchives := False;
   ZipCmntBufSize := 2000;

   For i := 0 To Pred(MaxExtArray) Do
      fArchiveExt.Add(ExtArray[i]);

   If (fRootDir = '') Or (Not DirExists(fRootDir)) Then
      fRootDir := GetCurrentDir();
End;
//-------------------------------------------------------------

Destructor TztvFindFile.Destroy;
Begin
   Inherited Destroy();
   fArchiveExt.Free();
   fFileSpec.Free();
End;
//-------------------------------------------------------------

Procedure TztvFindFile.ArcOnFileExists(Sender: TObject; FileName: String; Var NewFilename: String; Var OverwriteMode: TOverwriteMode);
Begin
  (* Virtual method... do not delete! If OnFileExists not	*)
  (* assigned, decompression components bypass extraction. *)
  (* ...thus this bypass method is required.               *)

  (* Nested archives are extracted to the windows\temp\, 	*)
  (* so there's not need to worry about overwriting files  *)
  (* in that directory.                                    *)

  (* NOTE: no files are extracted unless recursing nested  *)
  (* archives.                                             *)
   NewFilename := FileName;
End;
//-------------------------------------------------------------

Procedure TztvFindFile.ArcOnActivate(Sender: TObject);
Begin
   If Assigned(fOnCurrentArchive) Then
      OnCurrentArchive(Sender, TZipTV(Sender).ArchiveFile);
End;
//-------------------------------------------------------------

Procedure TztvFindFile.ArcOnGetPassword(Sender: TObject; FN: String;
   Var Password: String; Var TryAgain: Boolean);
Begin
   If (Not fRecurseNestedProtectedArchives) Then
      TryAgain := False
   Else
      If Assigned(OnGetPassword) Then
         OnGetPassword(Sender, FN, Password, TryAgain);
End;
//-------------------------------------------------------------

Procedure TztvFindFile.ArcOnRead(Sender: TObject; Offset, Filenum: Integer);
Var
   i: Integer;
   UnBase: TUnBASE;
   ZipTV: TZipTV;
   PatternFound: Boolean;
   FindFile: TztvFindFile;
Begin

   Try
      ZipTV := TZipTV(Sender);
      For i := 0 To FileSpec.Count - 1 Do
      Begin
         If Cancel Then break;

         If Assigned(OnCurrentFile) Then
            OnCurrentFile(ZipTV, ZipTV.FileName);

         Pattern := ExtractFilename(FileSpec.Strings[i]);
         PatternFound := MatchesMask(ZipTV.FileName, Pattern);

         If PatternFound Then
            OnFoundFile(Sender);

         If RecurseNestedArchives And (fArchiveExt.IndexOf(LowerCase(ExtractFileExt(ZipTV.FileName))) > -1) Then
         Begin

            Case ZipTV.ArcType Of

               atAce {,
              atAceExe}: UnBase := TUnACE.Create(Nil);

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

               atZip,
                  atZipMV,
                  atZipExe:
                  Begin
                     UnBase := TUnZip.Create(Nil);
                     UnBase.ZipCmntBufSize := 32000;
                  End;

               atZoo: UnBase := TUnZoo.Create(Nil);

            Else
               UnBase := Nil;
            End;


            If UnBase <> Nil Then
            Begin

               ExtractDir := GetTempPathStr();
               Try

            		(* Assign UnBASE properties 					*)
                  UnBase.ArchiveFile := ZipTV.ArchiveFile;
                  UnBase.ArcType := ZipTV.ArcType;
                  UnBase.ExtractDir := ExtractDir;
                  UnBase.UseStoredDirs := False; //True;
                  UnBase.FileSpec.Add(ZipTV.FileName);
                  UnBase.Passwords := Passwords;
                  UnBase.TranslateOemChar := TranslateOemChar;

            		(* Assign UnBASE events to point to    	*)
            		(* FindFile events 								*)
                  UnBase.OnBegin := OnExtractBegin;
                  UnBase.OnEnd := OnExtractEnd;
                  UnBase.OnActivate := OnExtractActivate;
                  UnBase.OnDeactivate := OnExtractDeactivate;
                  UnBase.OnError := OnError;
                  UnBase.OnFileExists := ArcOnFileExists;
                  UnBase.OnGetPassword := ArcOnGetPassword; //OnGetPassword;
                  UnBase.OnGetZipFirstDisk := OnGetZipFirstDisk;
                  UnBase.OnGetZipNextDisk := OnGetZipNextDisk;
                  UnBase.OnGetZipLastDisk := OnGetZipLastDisk;
            		//UnBASE.OnNextVolume := OnNextVolume;
                  UnBase.OnProgress := OnProgress;
                  UnBase.pCancel := pCancel;

            		(* Activate decompression component		*)
                  If Not Cancel Then
                     UnBase.Extract();

                  Passwords := UnBase.Passwords;
                  Count := UnBase.Count;
               Finally
                  UnBase.Destroy();
               End;

               ZipTV.fFileName := ExtractFilename(ZipTV.fFileName);
               If (Not Cancel) And (Count > 0) And FileExists(ExtractDir + ZipTV.FileName) Then
               Try
                  FindFile := TztvFindFile.Create(Nil);
                  Try
                     FindFile.RecurseNestedArchives := True;
                     FindFile.RootDir := ExtractDir;
                     FindFile.FileSpec := FileSpec;
                     FindFile.Passwords := Passwords;
                     FindFile.RecurseDirs := False;
                     FindFile.Attributes := Attributes;
                     FindFile.AttributesEx := AttributesEx;
                     FindFile.OnCurrentFile := ZipTV.OnReadFile;
                     FindFile.OnFoundFile := OnFoundFile;
                     FindFile.OnError := OnError;
                     FindFile.TranslateOemChar := TranslateOemChar;

                     FindFile.OnExtractBegin := OnExtractBegin;
                     FindFile.OnExtractEnd := OnExtractEnd;
                     FindFile.OnExtractActivate := OnExtractActivate;
                     FindFile.OnExtractDeactivate := OnExtractDeactivate;
               		//FindFile.OnNextVolume := OnNextVolume;
                     FindFile.OnGetPassword := OnGetPassword;
                     FindFile.OnProgress := OnProgress;
                     FindFile.DirSpec := DirSpec;
                     FindFile.pParentArchive := pParentArchive;
                     FindFile.ParentArchive := ExtractFilename(ZipTV.ArchiveFile) + '\';
                     StrPCopy(pParentArchive, FindFile.fParentArchive);
                     FindFile.pCancel := pCancel;
                     If Not Cancel Then
                        FindFile.FindIT(ZipTV.FileName, '');
                  Finally
                     FindFile.Destroy();
                     DeleteFile(ZipTV.FileName);
                     StrPCopy(pParentArchive, FindFile.fParentArchive);
                  End;
               Except
               	Cancel := True;
               End;

            End
            Else
               Application.ProcessMessages;
         End
         Else
            Application.ProcessMessages;

         If PatternFound Or Cancel Then break;

      End;
   Except
      Cancel := True;
   End;
End;
//-------------------------------------------------------------

Procedure TztvFindFile.ArcOnRecurseDir(Sender: TObject; Dir: String);
Begin
   If Assigned(OnChangeDir) Then
      OnChangeDir(Sender, Dir);
End;
//-------------------------------------------------------------

Procedure TztvFindFile.ReportDirChangeProc(CurrentDirectory: String);
Begin
   If Assigned(OnCurrentDir) Then
      OnCurrentDir(Self, CurrentDirectory);
End;
//-------------------------------------------------------------

Function TztvFindFile.GetParentArchive: String;
Begin
   Result := StrPas(pParentArchive);
End;
//-------------------------------------------------------------

Procedure TztvFindFile.SetParentArchive(SPA: String);
Begin
   fParentArchive := StrPas(pParentArchive) + SPA;
End;
//-------------------------------------------------------------

Procedure TztvFindFile.DoUpdateFilesList(Const Dir: String; FindData:
   TWin32FindData; pHeaderObj: pCompHeaderObj);
Begin
   doSearchRecAction(0, Dir, FindData, pHeaderObj);
End;
//-------------------------------------------------------------

Procedure TztvFindFile.Execute;
Begin
   Find {( fDirSpec )};
End;
//-------------------------------------------------------------

Procedure TztvFindFile.Find {( DirSpec: String )};
Begin

   If Not Assigned(OnFoundFile) Then
   Begin
      RaiseErrorStr(fArchiveFile, 'OnFoundFile', '0', E_REQUIREDEVENT);
      Exit;
   End;

   If Pos('\', fDirSpec) > 0 Then
      RootDir := ExtractFilePath(fDirSpec)
   Else
      If (RootDir = '') Or
         (Not DirExists(RootDir)) Or
         (FileSpec.Count < 1) Then
         Exit;

   Cancel := False;

   Try
      GetMem(pParentArchive, 255);
      ZeroMemory(pParentArchive, 255);
      Try
         If Assigned(OnActivate) Then
            OnActivate(Self);

         FindIT(fDirSpec, '');
      Finally
         FreeMem(pParentArchive);
      End;
   Except
   End;
End;
//-------------------------------------------------------------

Procedure TztvFindFile.FindIT(DirSpec: String; PA: String);
Var
   FileScan: TztvFileScan;
Begin

   If Cancel Then Exit;

   Try
      If Assigned(OnElapsedTime) Then ZipTimer.START;
      FileScan := TztvFileScan.Create(Self);
      Try
         DirectorySpec := DirSpec;
         FileScan.pHeaderObj := Nil;
         FileScan.Attributes := Attributes;
         FileScan.AttributesEx := AttributesEx;
         FileScan.RootDir := RootDir;
         FileScan.FileMask := '';
         FileScan.IncludeHiddenDirs := IncludeHiddenDirs;
         FileScan.FileSpec.Add(DirSpec);
         FileScan.UpdateFilesList := DoUpdateFilesList;
         FileScan.RecurseDirs := RecurseDirs;
         //FileScan.OnFinished := FinishedEvent;
         //FileScan.OnRootDirChange := OnRootDirChange;
         FileScan.OnRecurseDir := ArcOnRecurseDir;
         FileScan.pCancel := @fCancel;
         FileScan.Scan();
      Finally
         //no files added to fFilesList... no need to clear
         //FileScan.fFilesList.ClearList();
         FileScan.Free();
      End;
   Finally
      If Assigned(OnElapsedTime) Then
      Begin
         ZipTimer.Stop;
         OnElapsedTime(Self, ZipTimer.ElapsedTime);
      End;

      If Assigned(OnDeactivate) Then
         OnDeactivate(Self);
   End;
End;
//-------------------------------------------------------------

Procedure TztvFindFile.SearchRecProc(Index: Integer; Dir: String; FindData: TWin32FindData;
   pHeaderObj: pCompHeaderObj);
Var
   ZipTV: TZipTV;
   TempArchiveFile: String;
   TempArcType: TArcType;
Begin
   With FindData Do
      If (dwFileAttributes And faDirectory = 0) Then
         If (DirectorySpec = '*.*') Or
       	//( ( fArchiveExt.IndexOf( UpperCase( ExtractFileExt( cFilename) ) ) > -1 ) ) AND
         MatchesMask(cFilename, DirectorySpec) Then
         Begin
            Try
               If Cancel Then Exit;

               TempArchiveFile := fArchiveFile;
               TempArcType := ArcType;
               Try
                  ArchiveFile := AppendDirTail(Dir) + cFilename;
                  If IsArcValid(ArcType) Then
                  Begin
                     ZipTV := TZipTV.Create(Nil);
                     Try
                        ZipTV.fArchiveFile := fArchiveFile;
                        ZipTV.fArcType := fArcType;
                        ZipTV.FileSpec := FileSpec;
                        ZipTV.TranslateOemChar := TranslateOemChar;
                        ZipTV.OnActivate := ArcOnActivate;
                        ZipTV.OnError := OnError;
                        ZipTV.OnRead := ArcOnRead;
                        ZipTV.OnReadFile := OnCurrentFile;
                  		//ZipTV.OnNextVolume:= ArcOnNextVolume;  
                        ZipTV.Activate();
                     Finally
                        ZipTV.Destroy();
                     End;
                  End;
               Finally
                  fArchiveFile := TempArchiveFile;
                  fArcType := TempArcType;
               End;
            Except
            End;
         End
         //Else
         //   ShowMessage(cFilename);
End;
//-------------------------------------------------------------

Procedure TztvFindFile.SetArchiveExt(SAE: TStringList);
Var
   i: Integer;
Begin
   For i := 0 To Pred(SAE.Count) Do
      SAE.Strings[i] := LowerCase(SAE.Strings[i]);

   fArchiveExt.Assign(SAE);
End;
//-------------------------------------------------------------

Procedure TztvFindFile.SetFileSpec(SFS: TStringList);
Var
   i: Integer;
Begin
   If SFS.Count > 0 Then
   Begin
      i := 0;
      Repeat

         SFS.Strings[i] := ExtractFilename(UnixToDosFilename(SFS.Strings[i]));

         If SFS.Strings[i] = '' Then
         Begin
            SFS.Delete(i);
            Dec(i);
         End
         Else
         Begin

            If Pos('*', SFS.Strings[i]) > 0 Then
               If SFS.Strings[i] = '*.*' Then
               Begin
                  fFileSpec.Clear;
                  fFileSpec.Add('*.*');
                  Exit;
               End
               Else
                  If i > 0 Then
                  Begin
                     SFS.insert(0, SFS.Strings[i]);
                     SFS.Delete(i + 1);
                  End;

            inc(i);
         End;
      Until i >= Pred(SFS.Count);
   End;
   fFileSpec.Assign(SFS);
End;
//-------------------------------------------------------------

Procedure TztvFindFile.SetRootDir(SRD: String);
Begin
   If (SRD = '') Or (Not DirExists(SRD)) Then
      SRD := GetCurrentDir();

   fRootDir := SRD;
End;
//-------------------------------------------------------------


End.
