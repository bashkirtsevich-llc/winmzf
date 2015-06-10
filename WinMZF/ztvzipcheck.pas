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

Unit ztvZipCheck;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   Forms,
   Dialogs,
   ztvBase,
   ztvHeaders,
   ztvGbls,
   //mFwUncab,
   ztvStreams,
   ztvUnTAR;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TOnIntegrityEnd =
   	Procedure(Sender: TObject; FileName: String) Of Object;

   TOnIntegrityStatus =
   	Procedure(Sender: TObject; FileName: String; PassFail: Boolean) Of Object;

   TZipCheck = Class(TUnBASE)
   Private
      UnBaseObj: TUnBASE;
      fOnEnd: TOnIntegrityEnd;
      fOnStatus: TOnIntegrityStatus;
      Procedure ArcOnNextVolume(Sender: TObject; Var VolName: String; VolumeID:
      	String; fExists: Boolean; Var Cancel: Boolean);
      Procedure ArcOnEnd(Sender: TObject; FileName: String; CRC_PASS: Boolean);
      Function ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
      	size: Byte; Counter: DWord; WriteMethod: TDataType): DWord;
   Protected
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Activate;
   Published
      Property ArcType;
      Property FileSpec;
      Property PasswordAttempts;
      Property Passwords;
      Property OnActivate;
      Property OnBegin;
      Property OnChangeArchive;
      Property OnCorruptZipHeader;
      Property OnDeactivate;
      Property OnEnd: TOnIntegrityEnd Read fOnEnd Write fOnEnd;
      Property OnError;
      Property OnFileExists;
      Property OnGetPassword;
      Property OnGetZipFirstDisk;
      Property OnGetZipNextDisk;
      Property OnGetZipLastDisk;
      Property OnNestedTarFile;
      Property OnNextVolume;
      Property OnProgress;
      Property OnStatus: TOnIntegrityStatus Read fOnStatus Write fOnStatus;
   End;

Implementation

Uses
   ztvZipTV,
   ztvUnBH,
   ztvUnARC,                            {ztvUnCAB,}
   ztvUnARJ,
   ztvUnACE2,
   ztvUnGZip,
   ztvUnJAR,
   ztvUnLHA,
   ztvUnRAR,
   ztvUnZIP,
   ztvUnZOO,
   ztvUUDecode,
   Err_Msgs;

//-------------------------------------------------------------

Constructor TZipCheck.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TZipCheck.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TZipCheck.ArcOnNextVolume(Sender: TObject; Var VolName: String; VolumeID: String; fExists: Boolean; Var Cancel: Boolean);
Begin
   If Assigned(OnNextVolume) Then
      OnNextVolume(Self, VolName, VolumeID, fExists, Cancel);
End;
//-------------------------------------------------------------

Function TZipCheck.ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
	size: Byte; Counter: DWord; WriteMethod: TDataType): DWord;
Begin
   Result := Counter;                   (* static for this intercepted function *)
   UpdateEncryptBuffer(IsEncrypted, Buf, Result);
   UpdateCrcBuffer(size, Buf, Result);
End;
//-------------------------------------------------------------

Procedure TZipCheck.ArcOnEnd(Sender: TObject; FileName: String; CRC_PASS: Boolean);
Begin
   If Assigned(OnStatus) Then
      OnStatus(Sender, FileName, CRC_PASS);

   If Assigned(OnEnd) Then
      OnEnd(Sender, FileName);
End;
//-------------------------------------------------------------

Procedure TZipCheck.Activate;
Begin
   Try
      If (fArchiveFile = '') Or
         (Not IsArcVerifyable(ArcType)) Then
         RaiseError(E_RAISE, fArchiveFile, '', '0', E_INVALIDARC)
      Else
         If Not Assigned(OnStatus) Then
            RaiseError(E_RAISE, fArchiveFile, 'OnStatus', '0', E_REQUIREDEVENT);

      Count := 0;
      UnBaseObj := Nil;
      Cancel := False;

      If Assigned(OnElapsedTime) Then ZipTimer.START();

      Try
         Case ArcType Of

            atAce..atAceExe:
            	UnBaseObj := TUnACE.Create(Nil);

            atArc..atArcExe:
            	UnBaseObj := TUnARC.Create(Nil);

            atArj..atArjExe:
               UnBaseObj := TUnArj.Create(Nil);

            atBh..atBhExe:
               UnBaseObj := TUnBh.Create(Nil);

            atGZip:
               Begin
                  UnBaseObj := TUnGZIP.Create(Nil);
                  // point TUnGZip's OnChangeArchive event to TZipCheck's
                  // OnChangeArchive event.
                  UnBaseObj.OnChangeArchive := OnChangeArchive;
               End;


            atJar..atJarExe:
            	Begin
            		UnBaseObj := TUnJar.Create(Nil);
                  UnBaseObj.ZipCmntBufSize := 32000;
               End;

            atLha..atLzhExe:
            	UnBaseObj := TUnLha.Create(Nil);

            atRar..atRarExe:
            	UnBaseObj := TUnRar.Create(Nil);

            atTar:
            	UnBaseObj := TUnTar.Create(Nil);

            //atCab:
            //	UnBaseObj := TztvUnCAB.Create ( NIL );

            atZip..atZipMV:
                  Begin
                     UnBaseObj := TUnZip.Create(Nil);
                     UnBaseObj.ZipCmntBufSize := 32000;
                  End;

            atZoo:
            	UnBaseObj := TUnZoo.Create(Nil);

            atUUE:
            	UnBaseObj := TUUDecode.Create(Nil);

         Else
            Begin
               RaiseErrorStr(fArchiveFile, '', '0', E_INVALIDARC);
               Exit;
            End;
         End;

         UnBaseObj.ExtractWriteBlock := ArcPutBlock;

         //v4.8.6 added for enhanced speed... rem'd below assignments
         //Move(TZipCommon(Self), TZipCommon(UnBaseObj), SizeOf(TZipCommon));

         (* Assign UnBaseObj properties *)
         UnBaseObj.CreateStoredDirs := False;
         UnBaseObj.UseStoredDirs := False;
         UnBaseObj.Passwords := Passwords;
         UnBaseObj.ZipSFX_OffsetAdjustment := ZipSFX_OffsetAdjustment;

         // -----------------------------------------------------------------
         // assign variables that are normally automatically set when setting
         // the ArchiveFile property.  This is to bypass the time-consuming
         // archive-type (ArcType) assignment.
         // -----------------------------------------------------------------
         UnBaseObj.fArchiveFile := fArchiveFile;
         UnBaseObj.fArchiveDate := fArchiveDate;
         UnBaseObj.fVolBegChar := fVolBegChar;
         UnBaseObj.fVolNum := fVolNum;
         UnBaseObj.fArcType := fArcType;
         UnBaseObj.fOffsetStart := fOffsetStart;
         UnBaseObj.EndZipHeader := EndZipHeader;
         UnBaseObj.HeaderTypeState := HeaderTypeState;
         // -----------------------------------------------------------------

         UnBaseObj.FileSpec := FileSpec;
         UnBaseObj.PasswordAttempts := PasswordAttempts;
         UnBaseObj.RecurseDirs := RecurseDirs;
         UnBaseObj.TranslateOemChar := TranslateOemChar;

         (* Assign UnBaseObj events to point to ZipSrch events *)
         UnBaseObj.OnBegin := OnBegin;
         UnBaseObj.OnEnd := ArcOnEnd;
         UnBaseObj.OnNextVolume := ArcOnNextVolume;
         UnBaseObj.OnActivate := OnActivate;
         UnBaseObj.OnDeactivate := OnDeactivate;
         UnBaseObj.OnError := OnError;
         UnBaseObj.OnGetPassword := OnGetPassword;
         UnBaseObj.OnGetZipFirstDisk := OnGetZipFirstDisk;
         UnBaseObj.OnGetZipNextDisk := OnGetZipNextDisk;
         UnBaseObj.OnGetZipLastDisk := OnGetZipLastDisk;
         UnBaseObj.OnProgress := OnProgress;
         UnBaseObj.OnCorruptZipHeader := OnCorruptZipHeader;
         UnBaseObj.pCancel := pCancel;
         UnBaseObj.OnNestedTarFile := OnNestedTarFile;
         UnBaseObj.OnFileExists := OnFileExists;

         (* Activate decompression unique to TZipSearch/TZipCheck *)
         UnBaseObj.ExtractToVerify();

         If (UnBaseObj.IsGzTarArchive) And (ArcType <> UnBaseObj.ArcType) Then
            ArchiveFile := UnBaseObj.FileName;

         (* Assign ZipSearch.Passwords as updated UnBaseObj.Passwords *)
         Passwords := UnBaseObj.Passwords;

      Finally
         If Assigned(OnElapsedTime) Then
         Begin
            ZipTimer.Stop();
            OnElapsedTime(Self, ZipTimer.ElapsedTime);
         End;

         (* Reset the write buffer for all other components *)
         If (UnBaseObj <> Nil) Then
            UnBaseObj.Destroy();
      End;

   Except
      //ON e : exception DO ShowMessage( e.message );
   End;
End;
//-------------------------------------------------------------

End.
