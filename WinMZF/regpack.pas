(* **************************************************************** *)
(* Component Registration Unit for the ZipTV Compression Components *)
(* **************************************************************** *)
Unit regpack;

Interface

{$I ZipTV.inc}

Procedure Register;

Implementation



Uses
{$IFDEF DEL6_OR_HIGHER}
   DesignIntf,
   DesignEditors,
   PropertyCategories,
{$ELSE}
   Dsgnintf,
{$ENDIF}

   Windows,
   FileCtrl,
   Classes,
   Dialogs,
   Forms,
   SysUtils,
{$ifdef INSTALL_ztvOPENDIALOG}
   FiltEdit,
{$endif}

   ztvRegister,
   Err_Msgs,
   ztvArc2Arc,
   ztvFindFile,
   ztvMakeSfx,
   ztvRegArchive,
   ztvArchiveEditor,
   ztvArchiveSplitter,
{$ifdef INSTALL_ztvOPENDIALOG}
   ztvOpenDlg,
{$endif}
   ztvTurboSearch,
   ztvUnSFX,
   ztvZipCheck,
   ztvZipKey,
   ztvZipRun,
   ztvZipSearch,
   ztvZipTV,
   ztvZipView,
   ztvFileScan,

   ztvZip,
   ztvZipSplitter,
   ztvBlakHole,
   ztvJar,
   ztvGZip,
   ztvLHA,
   ztvMakeCab,
   ztvTar,

   ztvUnACE2,
   ztvUnARC,
   ztvUnARJ,
   ztvUnBH,
   ztvUnCab,
   ztvUnCabTypes,
   ztvUnGZip,
   ztvUnJAR,
   ztvUnLHA,
   ztvUnRAR,
   ztvUnTAR,
   ztvUnZIP,
   ztvUnZOO,

   ztvUUDecode,
   ztvUUEncode;

Type
   TAboutShow = Class(TStringProperty)
   Public
      Procedure Edit; Override;
      Function GetAttributes: TPropertyAttributes; Override;
   End;

   TPropEditSetArchive = Class(TStringProperty)
   Public
      Procedure Edit; Override;
      Function GetAttributes: TPropertyAttributes; Override;
   End;

   // for use with ZipKey
   TPropEditWordList = Class(TStringProperty)
   Public
      Procedure Edit; Override;
      Function GetAttributes: TPropertyAttributes; Override;
   End;

   TShowDir = Class(TStringProperty)
   Public
      Procedure Edit; Override;
      Function GetAttributes: TPropertyAttributes; Override;
   End;

//------------------------------------------------------------

Procedure Register;
Begin
   (* ----- *)
   (* Tools *)
   (* ----- *)
   RegisterComponents('ZTV Tools', [TArc2Arc, TMakeSFX, TRegArchive,
      TTurboSearch, TUnSFX, TZipCheck, {TZipKey,} TZipRun, TZipSearch, TZipTV,
         TZipView, TztvFindFile, TztvFileScan, TArchiveEditor, TArchiveSplitter
{$ifdef INSTALL_ztvOPENDIALOG}
         ,TztvOpenDialog
{$endif}
         ]);

   RegisterPropertyEditor(TypeInfo(String), TArc2Arc, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TArc2Arc, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TArchiveEditor, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TArchiveEditor, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TArchiveSplitter, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TArchiveSplitter, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TMakeSFX, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TMakeSFX, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TMakeSFX, 'SfxStubDir', TShowDir);
{$ifdef INSTALL_ztvOPENDIALOG}
   RegisterPropertyEditor(TypeInfo(String), TztvOpenDialog, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TztvOpenDialog, 'FileName', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TztvOpenDialog, 'Filter', TFilterProperty);
{$endif}
   RegisterPropertyEditor(TypeInfo(String), TRegArchive, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TRegArchive, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TTurboSearch, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TTurboSearch, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnSFX, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnSFX, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TZipCheck, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TZipCheck, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TZipKey, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TZipKey, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TZipKey, 'WordlistFile', TPropEditWordList);
   RegisterPropertyEditor(TypeInfo(String), TZipRun, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TZipRun, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TZipSearch, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TZipSearch, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TZipTV, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TZipTV, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TztvFindFile, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TztvFindFile, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TztvFileScan, 'About', TAboutShow);

   (* ---------------------- *)
   (* Compressors / Encoders *)
   (* ---------------------- *)
   RegisterComponents('ZTV Compress', [TBlakHole, TGZip, TLha, TJar,
      TMakeCab, TTar, TZip, TZipSplitter, TUUEncode]);

   RegisterPropertyEditor(TypeInfo(String), TBlakHole, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TBlakHole, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TGZip, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TGZip, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TJar, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TJar, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TMakeCab, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TLha, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TLha, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TMakeCab, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TTar, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TTar, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUUEncode, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUUEncode, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TZip, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TZip, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TZipSplitter, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TZipSplitter, 'ArchiveFile', TPropEditSetArchive);

   (* ------------------------ *)
   (* Decompressors / Decoders *)
   (* ------------------------ *)
   RegisterComponents('ZTV Decompress', [TUnACE, TUnARC, TUnArj, TUnBh, TUnCab,
      TUUDecode, TUnGZIP, TUnJar, TUnLha, TUnRar, TUnTar, TUnZip, TUnZoo]);

   RegisterPropertyEditor(TypeInfo(String), TUnACE, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnACE, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnACE, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnARC, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnARC, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnARC, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnArj, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnArj, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnArj, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnBh, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnBh, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnBh, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnCab, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnCab, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnCab, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnGZIP, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnGZIP, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnGZIP, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnJar, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnJar, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnJar, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnLha, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnLha, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnLha, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnRar, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnRar, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnRar, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnTar, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnTar, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnTar, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnZip, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnZip, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnZip, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUnZoo, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUnZoo, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUnZoo, 'ExtractDir', TShowDir);
   RegisterPropertyEditor(TypeInfo(String), TUUDecode, 'About', TAboutShow);
   RegisterPropertyEditor(TypeInfo(String), TUUDecode, 'ArchiveFile', TPropEditSetArchive);
   RegisterPropertyEditor(TypeInfo(String), TUUDecode, 'ExtractDir', TShowDir);

End;
//------------------------------------------------------------

Procedure TAboutShow.Edit;
Begin
   Try
      // with ztvRegister.pas source, rem the following line when compiling packages.
      // DA();
   Except
      On e: exception Do
         messageDlg(e.message, mtError, [mbOk], 0);
   End;
End;
//------------------------------------------------------------

Function TAboutShow.GetAttributes: TPropertyAttributes;
Begin
   Result := [paDialog];
End;
//------------------------------------------------------------

Function TPropEditWordList.GetAttributes: TPropertyAttributes;
Begin
   Result := [paDialog];
End;
//------------------------------------------------------------

Procedure TPropEditWordList.Edit;
Var
   AFileOpen: TOpenDialog;
Begin
   AFileOpen := TOpenDialog.Create(Screen.ActiveForm);
   Try
      With AFileOpen Do
      Begin
         If GetValue <> '' Then
            InitialDir := ExtractFileDir(GetValue);

         Filter := 'Word-lists (*.wrd )|*.wrd';
         Options := [ofPathMustExist, ofHideReadOnly, ofFileMustExist];

         If Execute Then
         Begin
            SetValue(FileName);
            SetCurrentDir(ExtractFileDir(FileName));
         End;
      End;
   Finally
      AFileOpen.Free;
   End;
End;
//------------------------------------------------------------

Function TPropEditSetArchive.GetAttributes: TPropertyAttributes;
Begin
   Result := [paDialog];
End;
//------------------------------------------------------------

Procedure TPropEditSetArchive.Edit;
Begin
   With TOpenDialog.Create(Application) Do
   Try
      Title := GetName();               // name of property as OpenDialog caption
      FileName := GetValue();
      If FileName <> '' Then
         InitialDir := ExtractFileDir(GetValue);

      Filter := LoadStr(F_TZIPTV);
      Options := [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
      HelpContext := 0;

      If Execute() Then
      Begin
         SetValue(FileName);
         SetCurrentDir(ExtractFileDir(FileName));
      End;
   Finally
      Free();
   End;
End;
//------------------------------------------------------------

Procedure TShowDir.Edit;
Var
   Dir: String;
Begin
   Try
      Dir := GetCurrentDir();
      If SelectDirectory(Dir, [], 0) Then
         SetValue(Dir)
      Else
         SetValue('');
   Except
      //On e : exception DO
      //  messageDlg(e.message,mtError,[ mbOk ],0);
   End;
End;
//------------------------------------------------------------

Function TShowDir.GetAttributes: TPropertyAttributes;
Begin
   Result := [paDialog];
End;
//------------------------------------------------------------

End.
