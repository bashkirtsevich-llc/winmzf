For more comments look in the source and the file UNRARDLL.TXT
which is included in the UnRar.dll-Distribution-File.

There's no special Form-File for Password- and Volumn-Dialogs. The
Dialogs are created dynamicly in the DFUnRar-Unit.


/****************
The unit untUnRar.pas contains the constants, records and
functions for using the unrar.dll

To Create english messages delete the comments for the
constants (and translate in better english)
****************/
untUnRar.pas

// Loads UnRar.dll - Dll must be in the standard-Dll-Paths
procedure LoadRarLibrary;

// Unloading UnRar.dll
procedure UnLoadRarLibrary;

// returns true, if dll is loaded, otherwise false
function  IsRarLoaded: boolean;


/****************
The unit DFUnRar.pas contains a component for easy using the unrar.dll
Limitations: 
- dont uses the extended Data-Structures and functions
****************/

/** Enumerations

// Used for property >Mode<
TDFRarMode = (DFRAR_EXTRACT, DFRAR_LIST);

// Uses for Status Messages in the event OnStatus
TRarStatus = (RAR_ONOPEN, RAR_ONBEFOREOPEN, RAR_ONEXTRACTFILE, RAR_AFTERCLOSE, RAR_ONPASSWORD);

// Override Options for overriding files
TOverrideOptions = (OR_ALWAYS, OR_NEVER, OR_EVENT);


/** Data Structures
  TDFRARHeaderData = record
    ArchiveName         : string;    // Archiv-Name
    FileName            : string;    // FileName in Archiv with relativ Path
    FlagContinueNextVol : boolean;   // the rest of the File are in the next volumn
    FlagContinuePrevVol : boolean;   // the rest of the File are in the previous volumn
    FlagNeedPassword    : boolean;   // you need a password to extract this file
    IsDirectory         : boolean;   // this is a directory entry
    DictionarySize      : Integer;   // size of the dictionary (not used here)
    PackSize            : cardinal;  // packed filesize
    UnpSize             : cardinal;  // unpacked filesize
    HostOS              : string;    // Name of Host Operation System
    FileCRC             : string;    // CRC-Code of File as 'F4F5F6F7'
    FileTime            : TDateTime; // FileTime
    MajorVersionNeeded  : Cardinal;  // Major Version needed to extract a file (not used here !!)
    MinorVersionNeeded  : Cardinal;  // Minor Version needed to extract a file (not used here !!)
    Method              : string;    // Compress Method - see constants in untUnRar.pas - COMPRESSMETHODxxxxx
    FAArchive           : boolean;   // FileAttribut Archiv is set
    FACompressed        : boolean;   // FileAttribut compressed is set
    FADirectory         : boolean;   // FileAttribut directory is set
    FAHidden            : boolean;   // FileAttribut hidden is set
    FANormal            : boolean;   // FileAttribut normal is set
    FAOffLine           : boolean;   // FileAttribut Offline is set
    FAReadOnly          : boolean;   // FileAttribut Readonly is set
    FASystem            : boolean;   // FileAttribut System is set
    FATempporary        : boolean;   // FileAttribut Temp is set
  end;


/** Functions

// extract or list archive content
procedure   Extract;
// returns false if any erros occours
function    Test: boolean;


/** Properties
ArchivComment:
- Archiv-Comment
- read only 
- y can also use OnComment-Event 

CanProgress: 
- use OnProgress Event
- additional UnRar to calculate filecount and size of all files - slower

CommentSize: 
- size of Archive Comment
- read only

Directory: 
- target directory for extracting

DllVersion: 
- gets the Dll-Versionnumber
- y have to check this with the Major- and Minorversion of the archive
- I dont check this in this release of the component !!

FileList: 
- if not empty then only the files are extract which are in this list

FileName: 
- Archive-FileName

Mode: 
- Open-Mode
- List: List only the content of the archive
- you can use the Event OnFileProcessing for Informations

OverrideEvent: 
- if a file exists and you use the property: OR_EVENT this event fires

Password: 
- password for archive

PromptForPass: 
- use build in password-dialog

PromptForVolumn: 
- use build in volumn-dialog

StopProcessing: 
- set to true if you want to stop extracting/listing


/** Events
OnComment: 
- fires on the Archive Comment

OnError: 
- fires on Error - mostly the operation or not stopped
- use for stopping StopProcessing-Property

OnFileProcessing: 
- use this to get Information for a file to extract/list

OnOverride: 
- fires if a file already exists and y specify the OR_EVENT-Flag

OnPassword: 
- fires if a pass is needed

OnProgress: 
- fires if y set Property CanProgress

OnRarStatus: 
- fires on all status events which are defined in the component

OnVolChange: 
- fires if UnRar.dll cant find a volumn on a multi volumn-archive
