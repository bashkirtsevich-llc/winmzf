unit RarExtract_u;

interface

Uses SysUtils, DFUnRar, Classes, Advanced, Decompress_u;

//Type TRarReWriteMode=(omRewrite,omSkip,omOverwriteAll,omSkipAll,omRename,omUnknown);

Type TUnRARProgress=procedure (Sender:TObject;FProgress,AProgress:Byte;Var Abort:Boolean) of object;
Type TPassQuest=procedure (Sender:TObject;Var Pass:ShortString)of object;
Type TOverWritePrompt=procedure (Sender:TObject;Var AFile:TFileHead;Var Mode:TReWriteMode)of object;

Type TRarDecompressor=Class(TComponent)
  Private
    dfUnRar:TDFUnRar;
    fExtractList:TStringList;
    aFileName:String;
    aDestDir:String;
    aOnProgress:TUnRARProgress;
    aOnPassword:TPassQuest;
    Abort:Boolean;
    aOnOverWrite:TOverWritePrompt;
    aOnBegin:TNotifyEvent;
    aOnEnd:TNotifyEvent;
    fOverWriteMode:TReWriteMode;
    procedure dfUnRarProgress(Sender: TObject; FilesProcessed,
                  FileCount, SizeProcessed, SizeCount: Cardinal);
    procedure dfUnRarPassword(Sender: TObject; var Password: String);
    procedure dfUnRarOverride(Sender: TObject; FileName: String;
                  var OverRide: Boolean);
  Public
    Constructor Create;
    Property FileSpec:TStringList read fExtractList write fExtractList;
    Property FileName:String read aFileName write aFileName;
    Property DestDir:String read aDestDir write aDestDir;
    Property OverWriteMode:TReWriteMode read fOverWriteMode write fOverWriteMode;
    Property OnProgress:TUnRARProgress read aOnProgress write aOnProgress;
    Property OnPassword:TPassQuest read aOnPassword write aOnPassword;
    Property OnOverWritePrompt:TOverWritePrompt read aOnOverWrite write aOnOverWrite;
    Property OnBegin:TNotifyEvent read aOnBegin write aOnBegin;
    Property OnEnd:TNotifyEvent read aOnEnd write aOnEnd;
    Procedure Extract;
    Procedure Free;
    Destructor Destroy;
  End;

implementation

Constructor TRarDecompressor.Create;
Begin
  dfUnRar:=TDFUnRar.Create(Self);
  fExtractList:=TStringList.Create;
  Abort:=False;
End;

Procedure TRarDecompressor.Extract;
Var Index:LongWord;
Begin
  If Assigned(aOnBegin) Then
    aOnBegin(Self);
  dfUnRar.FileName:=aFileName;
  dfUnRar.FileList.Clear;
  If fExtractList.Count>0 Then
    For Index:=0 To fExtractList.Count-1 Do
      dfUnRar.FileList.Add({aDestDir+'\'+}fExtractList.Strings[Index]);
  dfUnRar.OnPassword:=dfUnRarPassword;
  dfUnRar.OnOverride:=dfUnRarOverride;
  Case fOverWriteMode Of
    omOverwriteAll:dfUnRar.OverrideEvent := OR_ALWAYS;
    omSkipAll:dfUnRar.OverrideEvent := OR_NEVER;
    omRewrite,omSkip:dfUnRar.OverrideEvent := OR_EVENT;
  End;
  dfUnRar.Directory := aDestDir;
  dfUnRar.Mode := DFRAR_EXTRACT;
  dfUnRar.Extract;
  If Assigned(aOnEnd) Then
    aOnEnd(Self);
End;

Procedure TRarDecompressor.dfUnRarProgress(Sender: TObject; FilesProcessed,
  FileCount, SizeProcessed, SizeCount: Cardinal);
Begin
  If Assigned(aOnProgress) Then
    aOnProgress(Self,SizeProcessed*100 div SizeCount,
                FilesProcessed * 100 div FileCount,Abort);
    //!Attention! !Correction!
  dfUnRar.StopProcessing:=Abort;
End;

procedure TRarDecompressor.dfUnRarPassword(Sender: TObject; var Password: String);
var Temp:ShortString;
begin
  If Assigned(aOnPassword) Then
    Begin
      aOnPassword(Self,Temp);
      Password:=Temp;
    End;
end;

procedure TRarDecompressor.dfUnRarOverride(Sender: TObject; FileName: String;
  var OverRide: Boolean);
Var Head:TFileHead;
    Mode:TReWriteMode;
begin
  If Assigned(aOnOverWrite) Then
    Begin
      Head.LongFileName:=FileName;
      aOnOverWrite(Self,Head,Mode);
      Case Mode Of
        omRewrite:OverRide := true;
        omSkip:OverRide := False;
        omOverwriteAll:dfUnRar.OverrideEvent := OR_ALWAYS;
        omSkipAll:dfUnRar.OverrideEvent := OR_NEVER;
      End;
    End;
end;

Procedure TRarDecompressor.Free;
Begin
  If Self<>Nil Then
    Destroy;
End;

Destructor TRarDecompressor.Destroy;
Begin
  dfUnRar.Free;
  fExtractList.Free;
End;

end.
