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
Unit ztvTurboSearch;

Interface

Uses
   Windows,
   Forms,
   SysUtils,
   Classes,
   ztvBase,
   ztvZipSearch,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TPriority = (prNONE, prAND, prOR, prNOT);

   TPhraseRecTyp = Class(TObject)
      level: Byte;
      Priority: TPriority;
      IsPhrase,
         PhraseResult: Boolean;
   End;

   TTurboSearch = Class(TZipSearch)
   Private
      PhraseObject: TStringList;
      Procedure AddItem(s: PChar; l: ShortInt; p: TPriority; rt: Boolean);
      Procedure ListToBase;
      Function CheckPhraseSyntax(str: String): Boolean;
      Function IsBlank(str: PChar): Boolean;
      Function SearchTextToList(str: PChar): Boolean;
      Function SearchResults(Var PhraseRecType: TPhraseRecTyp; Var LastResult: Boolean; Var ItemIndex: Integer): Boolean;
   Protected
      Procedure ArcOnActivate(Sender: TObject); Override;
      Procedure ArcOnDeactivate(Sender: TObject); Override;
      Procedure ArcOnBegin(Sender: TObject; FN: String; RecNum: Integer; Var Extract: Boolean); Override;
      Procedure ArcOnEnd(Sender: TObject; FN: String; CRC_PASS: Boolean); Override;
      Function ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
      	size: Byte; Count: DWord; WriteType: TDataType): DWord; Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Procedure Search; Override;
   Published
      Property OnNestedTarFile;
      Property OnError;
   End;

Implementation

Uses
   ztvGbls,
   Err_Msgs;

//-------------------------------------------------------------

Constructor TTurboSearch.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
End;
//-------------------------------------------------------------

Destructor TTurboSearch.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TTurboSearch.ArcPutBlock(Var f: TStream32; Var Buf; IsEncrypted: Boolean;
	size: Byte; Count: DWord; WriteType: TDataType): DWord;
Var
   j: word;
   PhraseRecType,
      NextData: TPhraseRecTyp;
Begin
   Result := Count;
   If (WriteType = dtData) And (Not Cancel) Then
   Begin

      For j := 0 To PhraseObject.Count - 1 Do
      Begin

         PhraseRecType := TPhraseRecTyp(PhraseObject.Objects[j]);
         If PhraseRecType.IsPhrase Then
            If Not PhraseRecType.PhraseResult Then
            Begin

               TSOS.CHANGE_SEARCHFOR(PhraseObject.Strings[j], SearchMode = smAsc, CaseSensitive);
               FoundPos := TSOS.SEARCH_BUFFER_FIRSTMATCH(Pointer(@Buf), Count);
               PhraseRecType.PhraseResult := (FoundPos > -1) And
                  (FoundPos + FLastPos >= FOfsBeg) And
                  (FoundPos + FLastPos <= FOfsEnd);

               If PhraseRecType.PhraseResult Then
               Begin
                  If DecompressObj = Nil Then

                     (* Phrase found in RawData search,  *)
                     (*	cancel search of this file 		*)
                     Bytes_To_Go := 0;
               End;

               (* Dependant on search Result and succeeding priority, set next
                  PhraseResult as True to bypass search.. due to an already determined
                  Result of the combined level equation.  The phrase and stored
                  priority must be from the same level *)
               If j + 2 <= Pred(PhraseObject.Count) Then
               Begin
                  (* IsPhrase should be False (a stored priority) *)
                  NextData := TPhraseRecTyp(PhraseObject.Objects[j + 1]);
                  If NextData.level = PhraseRecType.level Then
                  Begin
                     (* We should now have phrase next in que *)
                     NextData := TPhraseRecTyp(PhraseObject.Objects[j + 2]);

                     (* Do the levels still match? *)
                     If NextData.level = PhraseRecType.level Then
                        Case NextData.Priority Of
                           prAND:
                              If Not PhraseRecType.PhraseResult Then NextData.PhraseResult := True;
                           prOR:
                              If PhraseRecType.PhraseResult Then NextData.PhraseResult := True;
                           prNOT:
                              If Not PhraseRecType.PhraseResult Then NextData.PhraseResult := True;
                        End;
                  End;
               End;
            End;
      End;
   End;

   //UpdateEncryptBuffer( IsEncrypted, Buf, Count );
   //UpdateCrcBuffer( Size, Buf, Count );

   If DecompressObj <> Nil Then
      inc(FTotalBytes, Count);

   inc(FLastPos, Count);

   Application.ProcessMessages();
End;
//-------------------------------------------------------------

Procedure TTurboSearch.ArcOnActivate(Sender: TObject);
Begin
   Inherited;
End;
//-------------------------------------------------------------

Procedure TTurboSearch.ArcOnDeactivate(Sender: TObject);
Begin
   // Virtual method... do not delete!
End;
//-------------------------------------------------------------

Procedure TTurboSearch.ArcOnBegin(Sender: TObject; FN: String; RecNum: Integer; Var Extract: Boolean);
Var
   j: Integer;
Begin
   (* New file search, reset results to false *)
   For j := 0 To PhraseObject.Count - 1 Do
      TPhraseRecTyp(PhraseObject.Objects[j]).PhraseResult := False;

   Inherited;
End;
//-------------------------------------------------------------

Procedure TTurboSearch.ArcOnEnd(Sender: TObject; FN: String; CRC_PASS: Boolean);
Var
   ItemIndex: Integer;
   LastResult: Boolean;
   PhraseRecType: TPhraseRecTyp;
Begin
   If FileChanged Then
   Begin
      If FoundPos > -1 Then
      Begin
         FileChanged := False;

         If DecompressObj = Nil Then
            inc(FTotalMatches);

         If Assigned(OnMatchInFile) Then
            OnMatchInFile(Sender, fArchiveFile);
      End;
   End;

   ItemIndex := 0;
   LastResult := True;

   If SearchResults(PhraseRecType, LastResult, ItemIndex) Then
   Begin
      If DecompressObj <> Nil Then
      Begin
         inc(FTotalMatches);            //FTotalMatches also incremented when OnFileChange activated.
         OnMatch(Self, FN, FoundPos { + FLastPos})
      End;
   End;

   Inherited;
End;
//-------------------------------------------------------------

Procedure TTurboSearch.ListToBase;
Var
   j: Byte;
Begin
   For j := 0 To PhraseObject.Count - 1 Do
      If TPhraseRecTyp(PhraseObject.Objects[j]).IsPhrase Then
         PhraseObject.Strings[j] := ConvertBaseType(PhraseObject.Strings[j]);
End;
//-------------------------------------------------------------

Procedure TTurboSearch.Search;
Var
   j: word;
   FinishEventAdr: TOnFinish;
Begin
   Cancel := False;
   FLastPos := 0;

   If SearchText = '' Then Exit;

   PhraseObject := TStringList.Create;
   Try
      If (Not CheckPhraseSyntax(SearchText)) Or
         (Not SearchTextToList(PChar(SearchText))) Then
         Exit;

      // Convert each item of list to desired base
      ListToBase();

      SearchFind := sfFirst;
      FinishEventAdr := OnFinish;       // Save address of the OnFinish event
      OnFinish := Nil;                  // Prevent OnZipSearch

      Try
         Inherited;                     // activate TZipSearch's Search Method
      Finally
         OnFinish := FinishEventAdr;
         If Assigned(OnFinish) Then
            OnFinish(Self, FTotalFiles, FTotalMatches, FTotalBytes);
      End;

   Finally
      For j := 0 To PhraseObject.Count - 1 Do
         PhraseObject.Objects[j].Free;

      PhraseObject.Free;
   End;
End;
//-------------------------------------------------------------

Function TTurboSearch.CheckPhraseSyntax(str: String): Boolean;
Var
   i: word;
   NotInQuote,
      NotInParan: Boolean;
Begin
   NotInParan := True;
   NotInQuote := True;

   For i := 1 To Length(str) Do
      If (str[i] = '(') Or (str[i] = ')') Then
         If Not NotInQuote Then
            NotInParan := NotInParan Xor True
         Else
            If str[i] = '"' Then
               NotInQuote := NotInQuote Xor True;

   If Not NotInParan Then
      RaiseErrorStr(fArchiveFile, '', '0', E_PARANSYNTAX)
   Else
      If Not NotInQuote Then
         RaiseErrorStr(fArchiveFile, '', '0', E_QUOTESYNTAX)
      Else
         If Length(str) = 0 Then
            NotInParan := False;        (* Trigger a False Result *)

   Result := NotInParan And NotInQuote;
End;
//-------------------------------------------------------------

Function GetWordLen(str: PChar): Byte;
Const
   DELIMITOR = [#0, #32, #34, #40, #41];
Var
   i: Byte;
Begin
   i := 0;
   If Strlen(str) > 0 Then
      Repeat
         inc(i);
      Until (str[i] In DELIMITOR) Or (str[i - 1] In DELIMITOR);

   Result := i;
End;
//-------------------------------------------------------------

Function GetNextWordLen(str: PChar; Var i: Byte): Byte;
Var
   j: Byte;
Begin
   j := 0;
   While str[j] = #32 Do
      inc(j);
   i := i + j;                          (* Position of start of word *)
   Result := GetWordLen(str + j);       (* Return length of word *)
End;
//-------------------------------------------------------------

Function GetNextWord(dest, source: PChar; Var i: Byte): PChar;
Begin
   While source[i] = #32 Do
      inc(i);
   Result := StrLCopy(dest, source + i, GetNextWordLen(source + i, i));
End;
//-------------------------------------------------------------

Function IsEvaluator(s: PChar): TPriority;
Const
   KEYWORDS: Array[0..2] Of PChar = ('AND'#0, 'OR'#0, 'NOT'#0);
Var
   i: Byte;
Begin
   If s^ <> #0 Then
      For i := 0 To 2 Do
         If StrIComp(s, KEYWORDS[i]) = 0 Then
         Begin
            Result := TPriority(i + 1);
            Exit;
         End;

   Result := prNONE;
End;
//-------------------------------------------------------------

Procedure TTurboSearch.AddItem(s: PChar; l: ShortInt; p: TPriority; rt: Boolean);
Var
   PhraseRecType: TPhraseRecTyp;
Begin
   PhraseRecType := TPhraseRecTyp.Create;
   With PhraseRecType Do
   Begin
      level := l;
      Priority := p;
      IsPhrase := rt;
      PhraseResult := False;
      PhraseObject.AddObject(StrPas(s), PhraseRecType);
   End;
End;
//-------------------------------------------------------------

Function TTurboSearch.IsBlank(str: PChar): Boolean;
Var
   i: Byte;
Begin
   Result := True;
   If str^ <> #0 Then
      For i := 0 To Strlen(str) - 1 Do
         If str[i] <> #32 Then
         Begin
            Result := False;
            break;
         End;
End;
//-------------------------------------------------------------

Function TTurboSearch.SearchTextToList(str: PChar): Boolean;
Const
   PHRASEREC = True;                    (* phrase    *)
   TOKENREC = False;                    (* delimitor *)
Var
   s, Temp: PChar;
   InQuote: Boolean;
   Priority: TPriority;
   i, j, level, WordLen: Byte;
{$IFDEF DEBUG1}
   PhraseRecType: TPhraseRecTyp;
   msg: String;
{$ENDIF}
Begin
   Try
      InQuote := False;
      Priority := prNONE;

      GetMem(s, 1024);
      GetMem(Temp, 1024);
      Try

         i := 0;
         s^ := #0;
         level := 0;
         Try
            While i < Strlen(str) Do
            Begin

               WordLen := GetWordLen(str + i);

               Case str[i] Of

                  '"': InQuote := InQuote Xor True;

                  '(':
                     If Not InQuote Then
                     Begin
                        If Not IsBlank(s) Then
                           AddItem(s, level, Priority, PHRASEREC);
                        s^ := #0;
                        inc(level);
                     End;

                  ')':
                     If Not InQuote Then
                     Begin
                        If Not IsBlank(s) Then
                           AddItem(s, level, Priority, PHRASEREC);
                        s^ := #0;
                        Dec(level);
                     End;
               Else

                  StrLCat(s, str + i, WordLen + Strlen(s));

                  If Not InQuote Then
                  Begin

                     j := i + WordLen;
                     GetNextWord(Temp, str, j);
                     Priority := IsEvaluator(Temp);

                     If Priority > prNONE Then
                     Begin
                        If Not IsBlank(s) Then
                           AddItem(s, level, prNONE, PHRASEREC);

                        AddItem(Temp, level, Priority, TOKENREC);
                        WordLen := Strlen(Temp);

                        i := j;
                        If str[i + WordLen] = #32 Then
                           inc(i);

                        s^ := #0;
                     End;
                  End;
               End;

               inc(i, WordLen);
            End;
         Finally
            If Not IsBlank(s) Then
               AddItem(s, level, prNONE, PHRASEREC);
         End;
      Finally
         FreeMem(s);
         FreeMem(Temp);
      End;

      (***************************************************************)
{$IFDEF DEBUG1}
      For j := 0 To PhraseObject.Count - 1 Do
      Begin
         PhraseRecType := TPhraseRecTyp(PhraseObject.Objects[j]);
         msg := 'Phrase      = ' + PhraseObject.Strings[j] +
            #13'Level = ' + IntToStr(PhraseRecType.level) +
            #13'IsPhrase  = ';

         If PhraseRecType.IsPhrase Then
            msg := msg + 'True'#13
         Else
            msg := msg + 'False'#13;

         Case PhraseRecType.Priority Of
            prNONE: msg := msg + 'Priority    = NONE';
            prAND: msg := msg + 'Priority    = AND';
            prOR: msg := msg + 'Priority    = OR';
            prNOT: msg := msg + 'Priority    = NOT';
         End;

         ShowMessage(msg);
      End;
{$ENDIF}
      (***************************************************************)

   Finally
      Result := PhraseObject.Count > 0;
   End;
End;
//-------------------------------------------------------------

Function TTurboSearch.SearchResults(Var PhraseRecType: TPhraseRecTyp;
   Var LastResult: Boolean; Var ItemIndex: Integer): Boolean;
Var
   level: Byte;
   Priority: TPriority;
Begin

   Priority := prNONE;
   PhraseRecType := TPhraseRecTyp(PhraseObject.Objects[ItemIndex]);
   level := PhraseRecType.level;

   While ItemIndex < PhraseObject.Count Do
   Begin

      While PhraseRecType.level <> level Do
         If PhraseRecType.level > level Then
         Begin
            If ItemIndex < PhraseObject.Count Then
               LastResult := SearchResults(PhraseRecType, LastResult, ItemIndex)
            Else
               break;
         End
         Else
            If PhraseRecType.level < level Then
            Begin
               Result := LastResult;
               Exit;
            End;

      If PhraseRecType.IsPhrase Then
         Case Priority Of
            prNONE: LastResult := (PhraseRecType.PhraseResult And LastResult);
            prAND: LastResult := (PhraseRecType.PhraseResult And LastResult);
            prOR: LastResult := (PhraseRecType.PhraseResult Or LastResult);
            prNOT: LastResult := (PhraseRecType.PhraseResult Xor True) And LastResult;
         End
      Else
         Priority := PhraseRecType.Priority;

      inc(ItemIndex);
      If ItemIndex < PhraseObject.Count Then
         PhraseRecType := TPhraseRecTyp(PhraseObject.Objects[ItemIndex]);

   End;

   Result := LastResult;
End;
//-------------------------------------------------------------

End.
