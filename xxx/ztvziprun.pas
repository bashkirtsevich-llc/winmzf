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
Unit ztvZipRun;

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
   ShellApi,
   ztvBase,
   ztvGbls,
   Err_Msgs,

   (* Decompression components *)
   ztvUnBH,
   ztvUnARJ,
   ztvUnARC,
   ztvUnACE2,
   ztvUnZIP,
   ztvUnZOO,
   ztvUnTAR,
   ztvUnLHA,
   ztvUnGZip,
   ztvUnCab,
   ztvUnJAR,
   ztvUnRAR,
   ztvUUDecode;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TExecState = (esNormal, esMinimized, esMaximized, esHidden);
   TExecMethod = (emExecuteNoWait, emExecuteAndWait);

Type
   TZipRun = Class(TUnBASE)
   Private
      InitialDir: String;
      fExecMethod: TExecMethod;
      fOnActivate: TNotifyEvent;
      fOnDeactivate: TNotifyEvent;
      fOnBegin: TOnBegin;
      fOnEnd: TOnEnd;
      fOnFileExists: TUnBase_OnFileExists;
      fWindowState: TExecState;
      fOnProgress: TOnProgress;
      fCommandLineParams: String;
      Function ztvFileExecuteWait(Const FileName, Params, StartDir: String): Integer;
   Protected
      { Protected declarations }
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Function Execute(FileToRun: String): Boolean;
   Published
      Property ArchiveFile;
      Property ArcType;
      Property ExecutionMethod: TExecMethod Read fExecMethod Write fExecMethod;
      Property FileSpec;
      Property CommandLineParams: String Read fCommandLineParams Write fCommandLineParams;
      Property Passwords;
      Property UseStoredDirs;
      Property WindowState: TExecState Read fWindowState Write fWindowState Default esNormal;
      Property OnExtractActivate: TNotifyEvent Read fOnActivate Write fOnActivate;
      Property OnExtractDeactivate: TNotifyEvent Read fOnDeactivate Write fOnDeactivate;
      Property OnExtractBegin: TOnBegin Read fOnBegin Write fOnBegin;
      Property OnExtractEnd: TOnEnd Read fOnEnd Write fOnEnd;
      Property OnExtractFileExists: TUnBase_OnFileExists Read fOnFileExists Write fOnFileExists;
      // v4.6.4 added OnNextVolume for .rar split & multi volume archives
      Property OnNextVolume;
      Property OnGetPassword;
      Property OnActivate;
      Property OnDeactivate;
      Property OnError;
      Property OnExtractProgress: TOnProgress Read fOnProgress Write fOnProgress;
      Property OnRemoveTempfile;
   End;

Implementation

//-------------------------------------------------------------

Function DeleteDirTree(Dir: String): Boolean;
Var
	fos: TSHFileOpStruct;
Begin
   If (Pos(':\', Dir) > 0) And (Pos(':\', Dir) = Length(Dir) - 1) Then
   Begin
   	Result := False;
      Exit;
   End;

	ZeroMemory(@fos, SizeOf(fos));
	With fos Do
   Begin
		wFunc := FO_DELETE;
		fFlags := FOF_SILENT Or FOF_NOCONFIRMATION;
		pFrom := PChar(dir + #0);
   End;
	Result := (0 = ShFileOperation(fos));
End;
//-------------------------------------------------------------

Constructor TZipRun.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fCommandLineParams := '';
   fWindowState := esNormal;
   fExecMethod := emExecuteAndWait;

   InitialDir := RemoveDirTail(GetTempFilenameStr(TempDir));
   DeleteFile(InitialDir);
   SetLength(InitialDir, Length(InitialDir) - 4);
End;
//-------------------------------------------------------------
(* DO NOT USE the variable ExtractDir in this destroy method!! *)

Destructor TZipRun.Destroy;
Begin
   //RemoveDirTree(Self, InitialDir);
   If Length(ArchiveFile) > 0 Then
      DeleteDirTree(InitialDir);

	{$I-}
   ChDir('..')
   {$I+};
   If IoResult = 0 Then
   Begin
      RemoveDir(InitialDir);
      If Assigned(OnRemoveTempfile) Then
         OnRemoveTempfile(Self, InitialDir);
   End;

   Inherited Destroy();
End;
//-------------------------------------------------------------

Function TZipRun.ztvFileExecuteWait(Const FileName, Params, StartDir: String): Integer;
{$IFDEF WIN32}
Var
   Info: TShellExecuteInfo;
   ExitCode: DWord;
Const
   SEE_MASK_NOCLOSEPROCESS = $00000040;
   ShowCommands: Array[TExecState] Of Integer = (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED, SW_HIDE);
Begin

   FillChar(Info, SizeOf(Info), 0);
   Info.cbSize := SizeOf(TShellExecuteInfo);

   With Info Do
   Begin
      fMask := SEE_MASK_NOCLOSEPROCESS;
      Wnd := Application.Handle;
      lpFile := PChar(FileName);
      lpParameters := PChar(Params);
      lpDirectory := PChar(StartDir);
      nShow := ShowCommands[fWindowState];
   End;

   SetCurrentDir(ExtractFileDir(FileName));

   If ShellExecuteEx(@Info) Then
   Begin
		If ExecutionMethod = emExecuteAndWait Then
      Begin
         //WaitforSingleObject(Info.hProcess,INFINITE);
           //Result := 0;
         Repeat
            Application.ProcessMessages;
            GetExitCodeProcess(Info.hProcess, ExitCode);
            //If Not GetExitCodeProcess(Info.hProcess, ExitCode) Then
            //	Break;
         Until (ExitCode <> STILL_ACTIVE) Or Application.Terminated;
      	Result := ExitCode;
      End Else
      	Result := 0;
   End
   Else
      Result := -1;
End;
{$ELSE}
Var
   Task: THandle;
Begin
   Result := 0;
   Task := FileExecute(FileName, Params, StartDir, InitialState);
   If Task >= HINSTANCE_ERROR Then
   Begin
      Repeat
         Application.ProcessMessages;
      Until (GetModuleUsage(Task) = 0) Or Application.Terminated;
   End
   Else
      Result := -1;
End;
{$ENDIF}
//-------------------------------------------------------------

Function TZipRun.Execute(FileToRun: String): Boolean;
Var
   FilesExtracted: Integer;
   fExtractComponent: TUnBASE;          (* Base class for decompression components *)
Begin
   Result := False;
   Cancel := False;
   ExtractDir := InitialDir;

   Try
      If (fArchiveFile = '') Or (FileToRun = '') Then Exit;

      Case ArcType Of

         atAce,
         	atAceExe: fExtractComponent := TUnACE.Create(Nil);

         atArc,
            atArcExe: fExtractComponent := TUnARC.Create(Nil);

         atArj,
            atArjExe: fExtractComponent := TUnArj.Create(Nil);

         atBh,
            atBhExe: fExtractComponent := TUnBh.Create(Nil);

         atCab: fExtractComponent := TUnCab.Create(Nil);

         atGZip: fExtractComponent := TUnGZIP.Create(Nil);

         atJar: fExtractComponent := TUnJar.Create(Nil);

         atLha,
            atLhaExe,
            atLzh,
            atLzhExe: fExtractComponent := TUnLha.Create(Nil);

         atRar,
            atRarExe: fExtractComponent := TUnRar.Create(Nil);

         atTar: fExtractComponent := TUnTar.Create(Nil);

         atUUE: fExtractComponent := TUUDecode.Create(Nil);

         atZip,
            atZipExe:
            Begin
               fExtractComponent := TUnZip.Create(Nil);
               fExtractComponent.ZipCmntBufSize := 32000;
            End;

         atZoo: fExtractComponent := TUnZoo.Create(Nil);

      Else
         Exit;
      End;

      If Assigned(OnActivate) Then OnActivate(Self);
      If Assigned(OnElapsedTime) Then ZipTimer.START;

      Try
         If fExtractComponent <> Nil Then
         Begin
            fExtractComponent.pCancel := @fCancel;
            fExtractComponent.ArchiveFile := ArchiveFile;

            If FileSpec.Count = 0 Then
               fExtractComponent.FileSpec.Add(FileToRun)
            Else
               fExtractComponent.FileSpec.Assign(FileSpec);

            fExtractComponent.ExtractDir := ExtractDir;
            fExtractComponent.Passwords := Passwords;
            fExtractComponent.UseStoredDirs := UseStoredDirs;
            fExtractComponent.OnActivate := OnExtractActivate;
            fExtractComponent.OnDeactivate := OnExtractDeactivate;
            fExtractComponent.OnBegin := OnExtractBegin;
            fExtractComponent.OnEnd := OnExtractEnd;
            fExtractComponent.OnFileExists := OnExtractFileExists;
            fExtractComponent.OnError := OnError;
            fExtractComponent.OnGetPassword := OnGetPassword;
            fExtractComponent.OnNextVolume := OnNextVolume;
            fExtractComponent.OnProgress := OnExtractProgress;
            fExtractComponent.RestoreFileAttr := False;
            fExtractComponent.TranslateOemChar := TranslateOemChar;

            // execute the extraction process
            FilesExtracted :=
               fExtractComponent.Extract();

            // were any files extracted?
            If FilesExtracted > 0 Then
            Begin
               If UseStoredDirs Then
                  FileToRun := fExtractComponent.ExtractDir + FileToRun
               Else
                  FileToRun := fExtractComponent.ExtractDir + ExtractFilename(FileToRun);

               {ExitCode :=} ztvFileExecuteWait(FileToRun, fCommandLineParams, fExtractComponent.ExtractDir)
            End;
         End
         Else
            {ExitCode :=} ztvFileExecuteWait(FileToRun, fCommandLineParams, InitialDir);

      Finally
         If Assigned(OnElapsedTime) Then
         Begin
            ZipTimer.Stop;
            OnElapsedTime(Self, ZipTimer.ElapsedTime);
         End;

         If Assigned(OnDeactivate) Then OnDeactivate(Self);
         fCommandLineParams := '';

         If fExtractComponent <> Nil Then
            fExtractComponent.Free();
      End;
   Except
   End;
End;
//-------------------------------------------------------------

End.
