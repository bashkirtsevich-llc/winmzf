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

NOTE:
  We have tried to make this component as simple and compact as possible, yet
  contain the power and features to effectively manage archive extension
  registry associations.  Any ideas for future enhancements would be much
  appreciated.

  The most common usage:
  1. drop the component on a form
  2. set the OnAssociation event
  3. code the OnAssoication event

  Thats all... start your application and all extensions will be associated
  to your application.  You don't even have to tell TRegArchive the name of
  your application.

  =======================
  To verify associations:
  =======================
  1. open explorer
  2. menu: View
  3. submenu: Folder Options...
  4. tab: File Types
  5. scroll down list to your applications filename (or in the Delphi IDE,
     the name of the project TRegArchive is used in) without the file ext.

  ===========
 PROPERTIES:
  ===========
  ActivateOnStartup:
     If the value of this boolean property is true this component will check
     file associations in the registry for supported archive extensions.  If
     one or more extensions are found to be associated to an application other
     than the components parent application the OnAssociation event will be
     activated.

  ExtDescription:
     The value of this property is the extensions description which is written
     to the registry for the associated extension.  Don’t confuse this property
     with that of a file’s extension.  It is the simply a “description” which
     is written to the registry.

  The default value set internally is [Appname].  For most applications, the
     default value of this property should be used.  When using this default
     value, your application's name will be used.  Example: if the name of your
     application is WinZip32.exe all extensions will be updated in the registry
     under the name WinZip32.

  There is a source demo application which demonstrates this component
     included in the package.

  FileDescription:
     The value of this string property is the files description which is
     written to the registry for the associated extension.

     The default value set internally is [Appname file].  When using this
     default value, your application's name is used with the word ' file' to
     follow.

  //(version 3.50.03 - property removed... use IconIndex below)
  //Icon:
  //	This property simply enables the application to store a desired icon in
  //   it's resouce section.  The purpose of this is to allow TRegArchive
  //   to write a specific icon to the registry which is associated with your
  //   application.  See IconIndex below...

  ParamString:
     The value of this string property allows the developer to define a
     required commandline options.  The default is '%1' which is usually
     replaced with the file or document being opened.

  IconIndex:
     An example of storing multiple icons in a .res file can be found in
     demos\zipview\zptvdemo.dpr.  In this demo, we created main.res (a resource
     file) using Delphi's "image editor" and stored 5 icons which will be
     included in the application's resource section when compiled.  Use the
     $R switch to make sure the resource file is included in the compiled
     application.  See: {$R main.res} in main.pas.

     If the value of IconIndex exceeds the number of icons in the application's
     resource section, the MAINICON is automatically used as the default.

     **There is one problem with the index of icons in a resource file.  This
     deals with the icon located at index 0 (first icon in the file).  If
     IconIndex is 0, the applications MAINICON will always be used instead of
     the actual icon located at index 0.  Therefore, the first icon defined in
     the main.res file is basically useless.  This is a Delphi internal bug,
     with no known workaround except to copy the first icon, and have it also
     reside at index 1.

  MenuName:	(contributed by: Nagy Róbert)
     This property adds an item to Explorer's context menu when a file
     registered to your registered extensions is right clicked on.

     Example: to display "Open with (your app name)" enter the below code
     block to the "loaded" procedure below:

     If fMenuName = '' Then
      fMenuName := 'Open with &' +
       ExtractFilenameOnly( ExtractFilename( ParamStr( 0 ) ) );

  ArcExtensions:
    The strings defined in this TStringList property is a list of extensions
     that ZipTV currently supports.

  ========
  METHODS:
  ========
 CheckExtensions:
     Normally this method won't be used if the ActivateOnStartup property is
     true.  This method checks the registry for extensions (listed in the
     ArcExtensions property) and returns a value of false if any of the listed
     extensions are not associated with your program.  If any of the extensions
     are associated with other programs, the return value will be false.  Only
     if ALL defined extensions are associated with your application will the
     return value be true.

  Install:
     This method will normally be called within the OnAssociation event.  If
     the ActivateOnStartup property is false, this method could be placed
     anywhere in code that would allow the user to Install (or associate) the
     supported archive's extensions in the registry.

  Uninstall:
     The Uninstall method is used to remove the extension(s) associated with
     your application in the registry.

  =======
  EVENTS:
  =======
  OnAssociation:
     This event is activated when one or more extensions are determined not
     to be associated with your application in the registry.  Within this
     event you can use the "Install" property if you elect to associate the
     extensions in the registry to your application.

**********************************************************************)
Unit ztvRegArchive;

Interface

Uses
   Windows,
   Classes,
   Graphics,
   Controls,
   Forms,
   SysUtils,
   Registry,
   Dialogs,
   ShlObj,
   ztvGbls;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TOnAssociation = Procedure(Sender: TObject; Appname: String) Of Object;

   TRegArchive = Class(TComponent)
   Private
      fActivateOnStartup: Boolean;
      fOnAssociation: TOnAssociation;
      fArcExtensions: TStrings;
      fExtDescription: String;
      fFileDescription: String;
      fOpenWith: String;
      fParamString: String;
      fMenuName: String;
      fIconIndex: Integer;
      Procedure SetExtDescription(SED: String);
      Procedure SetFileDescription(SFD: String);
      Function GetExtDescription: String;
      Function GetFileDescription: String;
      Function GetParamString: String;
      Procedure SetParamString(SPS: String);
      Procedure SetArcExtensions(SAE: TStrings);
      Procedure SplitStr(SplitChar: char; Var str, Str1: String);
   Protected
      Procedure Loaded; Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Install;
      Function UnInstall: Boolean;
      Function CheckExtensions: Boolean;
      Property OpenWith: String Read fOpenWith Write fOpenWith;
   Published
      Property ActivateOnStartup: Boolean Read fActivateOnStartup Write fActivateOnStartup Default True;
      Property ArcExtensions: TStrings Read fArcExtensions Write SetArcExtensions;
      Property ExtDescription: String Read GetExtDescription Write SetExtDescription;
      Property FileDescription: String Read GetFileDescription Write SetFileDescription;
      Property IconIndex: Integer Read fIconIndex Write fIconIndex;
      Property MenuName: String Read fMenuName Write fMenuName;
      Property OnAssociation: TOnAssociation Read fOnAssociation Write fOnAssociation;
      Property ParamString: String Read GetParamString Write SetParamString;
   End;

Function ExtractIcon(hInst: hInst; lpszExeFileName: PChar;
   nIconIndex: uInt): hIcon; Stdcall;


Implementation

{R *.RES}
{R ztvregarchive.res}

//------------------------------------------------------------
Function ExtractIcon; External 'shell32.dll' name 'ExtractIconA';
//------------------------------------------------------------

Function ExtractFilenameOnly(s: String): String;
Var
   p: Byte;
Begin
   p := Pos('.', s);
   If p = 0 Then
      Result := s
   Else
      Result := Copy(s, 1, p - 1);
End;
//------------------------------------------------------------

Constructor TRegArchive.Create(AOwner: TComponent);
Var
   i: Integer;
Begin
   Inherited Create(AOwner);
   fArcExtensions := TStringList.Create;
   fActivateOnStartup := True;

   //If ( csDesigning In ComponentState ) Then
   For i := 0 To MaxExtArray Do
   Begin
      If CompareText(ExtArray[i], '.exe') = 0 Then
         Continue;
      fArcExtensions.Add(ExtArray[i]);
   End;
End;
//------------------------------------------------------------

Destructor TRegArchive.Destroy;
Begin
   fArcExtensions.Free();
   Inherited Destroy;
End;
//------------------------------------------------------------

Procedure TRegArchive.Loaded;
Var
   sExtDescription: String;
Begin
   Inherited Loaded;

   If fExtDescription = '[Appname]' Then
      sExtDescription := ExtractFilenameOnly(ExtractFilename(ParamStr(0)))
   Else
      sExtDescription := fExtDescription;

   sExtDescription := UpperCase(sExtDescription);

   If (Not (csDesigning In ComponentState)) And fActivateOnStartup Then
      If Assigned(fOnAssociation) And (Not CheckExtensions) Then
         fOnAssociation(Self, sExtDescription);

End;
//------------------------------------------------------------

Procedure TRegArchive.SetExtDescription(SED: String);
Begin
   If SED = '' Then
      fExtDescription := '[Appname]'
   Else
      fExtDescription := SED;
End;
//------------------------------------------------------------

Function TRegArchive.GetExtDescription: String;
Begin
   If (fExtDescription = 'Delphi32') Or (fExtDescription = '') Then
      Result := '[Appname]'
   Else
      Result := fExtDescription;
End;
//------------------------------------------------------------

Procedure TRegArchive.SetFileDescription(SFD: String);
Begin
   If SFD = '' Then
      fFileDescription := '[Appname file]'
   Else
      fFileDescription := SFD;
End;
//------------------------------------------------------------

Function TRegArchive.GetFileDescription: String;
Begin
   If (fFileDescription = 'Delphi32 file') Or (fFileDescription = '') Then
      Result := '[Appname file]'
   Else
      Result := fFileDescription;
End;
//------------------------------------------------------------

Procedure TRegArchive.SetParamString(SPS: String);
Begin
   If SPS = '' Then
      fParamString := '%1'
   Else
      fParamString := SPS;
End;
//------------------------------------------------------------

Procedure TRegArchive.SetArcExtensions(SAE: TStrings);
Var
   i: Integer;
Begin
   fArcExtensions.Assign(SAE);
   For i := 0 To fArcExtensions.Count - 1 Do
      If CompareText(fArcExtensions.Strings[i], '.exe') = 0 Then
         fArcExtensions.Delete(i);
End;
//------------------------------------------------------------

Function TRegArchive.GetParamString: String;
Begin
   If fParamString = '' Then
      Result := '%1'
   Else
      Result := fParamString;
End;
//------------------------------------------------------------

Procedure TRegArchive.SplitStr(SplitChar: char; Var str, Str1: String);
Var
   i: Integer;
Begin
   i := Pos(SplitChar, str);
   If i <> 0 Then
   Begin
      Str1 := Copy(str, i + 1, Length(str) - i + 1);
      SetLength(str, i - 1);
   End
   Else
      fParamString := '';
End;
//------------------------------------------------------------

Function TRegArchive.CheckExtensions: Boolean;
Var
   i, j, l: Integer;
   lpPath: PChar;
   Appname,
   	Path,
   	OpenWith,
      ParamStrs,
      ExtDescript,
      FileDescript: String;
Begin
   Result := True;
	Path := ExtractFilePath(ParamStr(0)) + ExtractFilename(ParamStr(0));

   GetMem(lpPath, 256);
   Try
   	GetShortPathName(PChar(Path), lpPath, 256 );
   	Appname := StrPas(lpPath);
   Finally
   	FreeMem(lpPath);
   End;

   For l := 0 To fArcExtensions.Count - 1 Do
   Try

      If CompareText(fArcExtensions.Strings[l], '.exe') = 0 Then
         Continue;

      With TRegistry.Create Do
      Try

         RootKey := HKEY_CLASSES_ROOT;
         OpenKey(fArcExtensions.Strings[l], True);
         ExtDescript := ReadString('');

         OpenKey('\' + ExtDescript, True);
         FileDescript := ReadString('');

         OpenKey('DefaultIcon', True);
         OpenKey('\' + ExtDescript + '\Shell\Open\Command', True);
         OpenWith := ReadString('');

         i := Pos('"', OpenWith);
         If i = 1 Then
         Begin
            OpenWith := Copy(OpenWith, 2, Length(OpenWith) - 1);

            i := Pos('"', OpenWith);
            ParamStrs := Copy(OpenWith, i + 2, Length(OpenWith) - i - 1);

            j := Pos('"', ParamStrs);
            While j <> 0 Do
            Begin
               Delete(ParamStrs, j, 1);
               j := Pos('"', ParamStrs);
            End;

            OpenWith := Copy(OpenWith, 0, i - 1);

         End
         Else
            If (OpenWith <> '') Then
               SplitStr(' ', OpenWith, ParamStrs);

         Result := True;
      Finally
         Free();
      End;

      If (Appname <> OpenWith) Then
      Begin
         Result := False;
         Exit;
      End;
   Except
      Result := False;
   End;
End;
//------------------------------------------------------------

Procedure TRegArchive.Install;
Var
   i: Integer;
   sExtDescription: String;
   sFileDescription: String;
   Path: String;
   lpPath: PChar;
Begin
	Path := ExtractFilePath(ParamStr(0)) + ExtractFilename(ParamStr(0));

   GetMem(lpPath, 256);
   Try
   	GetShortPathName(PChar(Path), lpPath, 256 );
   	fOpenWith := StrPas(lpPath);
   Finally
   	FreeMem(lpPath);
   End;

   If fExtDescription = '[Appname]' Then
      sExtDescription := ExtractFilenameOnly(ExtractFilename(ParamStr(0)))
   Else
      sExtDescription := fExtDescription;

   If fFileDescription = '[Appname file]' Then
      sFileDescription := sExtDescription + ' file'
   Else
      sFileDescription := fFileDescription;

   If fParamString = '' Then
      fParamString := '%1';

   For i := 0 To fArcExtensions.Count - 1 Do
   Begin

      // ****************************************
      //              W A R N I N G
      // ****************************************
      // DO NOT REMOVE THE FOLLOWING TWO LINES!!!
      // YOU DO NOT WANT TO REGISTER EXECUTABLES
      // ****************************************

      // ****************************************
      // This warning may make you curious enough
      // to want to test to see what happens, but
      // please do not attempt registering .exe's
      // ****************************************

      // ****************************************
      //              W A R N I N G
      // ****************************************
    	// DO NOT REMOVE THE FOLLOWING TWO LINES!!!
      // YOU DO NOT WANT TO REGISTER EXECUTABLES
      // ****************************************
      If CompareText(fArcExtensions.Strings[i], '.exe') = 0 Then
         Continue;

      With TRegistry.Create Do
      Try
         Try
            RootKey := HKEY_CLASSES_ROOT;
            OpenKey(fArcExtensions[i], True);
            WriteString('', sExtDescription);

            OpenKey('\' + sExtDescription, True);
            WriteString('', sFileDescription);

            OpenKey('DefaultIcon', True);
            WriteString('', OpenWith + ',' + IntToStr(fIconIndex));
            CloseKey();

            OpenKey('\' + sExtDescription + '\Shell\Open\Command', True);
            WriteString('', '"' + OpenWith + '" "' + fParamString + '"');

            If fMenuName <> '' Then
            Begin
               OpenKey('\' + sExtDescription + '\Shell\Open', True);
               WriteString('', fMenuName);
            End;

            CloseKey();
         Except
         End;
      Finally
         Free();
      End;
   End;
   SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, Nil, Nil);
End;
//------------------------------------------------------------

Function TRegArchive.UnInstall: Boolean;

   Procedure RegDeleteKey(RKey: HKey; KeyPath: String);
   Begin
      With TRegistry.Create Do
      Try
         RootKey := RKey;
         // Under Win95, all subkeys are automatically deleted.
         // Under WinNT, the subkeys will be left alone.
         DeleteKey(KeyPath);
      Finally
         Free();
      End;
   End;

   Procedure RemoveAssociation(Ext: String);
   Begin
      RegDeleteKey(HKEY_CLASSES_ROOT, '.' + Ext);
      RegDeleteKey(HKEY_CLASSES_ROOT, Ext + '_auto_file\shell\open\command');
      RegDeleteKey(HKEY_CLASSES_ROOT, Ext + '_auto_file');
   End;
Var
   i: Integer;
Begin
   Try
      With TRegistry.Create Do
      Try
         If fArcExtensions.Count > 0 Then
            For i := 0 To fArcExtensions.Count - 1 Do
            Begin
               If CompareText(fArcExtensions.Strings[i], '.exe') = 0 Then
                  Continue;

            	// RemoveAssociation(Ext);
               RootKey := HKEY_CLASSES_ROOT;
               DeleteKey(fArcExtensions.Strings[i]);
               DeleteKey('\' + fExtDescription);
            End;
      Finally
         Free();
      End;

      Result := True;
   Except
      Result := False;
   End;
End;
//------------------------------------------------------------


End.
