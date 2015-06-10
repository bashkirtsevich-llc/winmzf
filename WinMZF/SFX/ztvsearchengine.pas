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

{+--------------------------------------------------------------------------+
    Copyright 1997-2003 Carl Bunton & Thomas Steg Softwareentwicklung

    This unit contains the search algorithms for searching
    files / decompressed buffers in the ZipTV component package.
 +--------------------------------------------------------------------------+}
Unit ztvSearchEngine;

Interface

Uses
   Classes;

Const
   TSMaxBufLen = 32768 * 20;            // max. 32K * X filebuffer
   TSSmallBufLen = 32768 * 5;
   MaxSearchCombinations = 20;
   MaxSearchStrLen = 60;

Type
   TurboSearchUserCallProc =
      Procedure(Var Terminateit: Boolean; IamSorting: Boolean; MatchesFound: Integer; PosinFilepercent: Byte);

   SStringTyp =
      String[MaxSearchStrLen];

   TSearchCombinationType =
      (TSCombi_MUST, TSCombi_CAN, TSCombi_NOT);

   TSSortTyp =
      (TSSort_None, TSSort_SString, TSSort_Position);

   PListSaveRecTyp = ^TListSaveRecTyp;
   TListSaveRecTyp = Record
      FoundPos: Integer;
      SearchStringnum: Byte;
   End;

  //TListSaveObj = Object
   TListSaveObj = Class(TObject)
      ResList: TList;
      TotPositionsCNT: Integer;
      Procedure INIT;
      Procedure CLEAR_LIST;
      Procedure DONE;
      Function AddItem(InRec: TListSaveRecTyp): Boolean;
   End;

  //TSFileObj = Object
   TSFileObj = Class(TObject)
      FileVAR: File;
      fSize: Integer;
      IsOpen: Boolean;
      IsEOF: Boolean;
   End;

   TFoundPosProc = Function(CurrentFoundPos, SStringNum: Integer): Boolean Of Object;

   //TSPatternData = Object
   TSPatternData = Class(TObject)
   Private
      Pat: Array[0..MaxSearchStrLen - 1] Of Byte;
      PatORGCopy: SStringTyp;           // backup of the users original searchphrase
      PatLen: Integer;                  // searchstring length
      PatLenPlus: Integer;              // searchstring-length plus 1
      HalfLen: Integer;                 // searchstrig halflen
      Shift: Array[0..255] Of Integer;
      CompTable: Array[0..255] Of Byte;
      ChkCase: Boolean;
      FoundPosProc: TFoundPosProc;
      Procedure
         INIT_SEARCHTABLE(
         SearchFor: SStringTyp;
         UseASCII: Boolean;
         CheckCase: Boolean);
   End;

   //TSearchObj = Object
   TSearchObj = Class(TObject)
   Private
      AbsPosInFile: Integer;
      LastReadResult: Integer;
      LongestPatternLen: Integer;
      LoopIndex: Integer;
      Buf: Pointer;
      res: Boolean;
      BufisCreated: Boolean;
      UseASCIICharSet: Boolean;
      FileData: TSFileObj;
      TerminateSearch: Boolean;
      FoundPosList: TListSaveObj;
      UserBackPosition: Integer;
      CallbackProc: TurboSearchUserCallProc;
      PattsUsed: - 1..MaxSearchCombinations - 1;
      PattData: Array[0..MaxSearchCombinations - 1] Of TSPatternData;
      Procedure CLOSE_LOCFILE;
      Function READ_FILEBLOCK: Boolean;
      Function OPEN_LOCFILE(FileName: shortstring): Boolean;
      Function DoSearch(Index, AbsFilePos, StartPos, BufSize: Integer): Integer;
      Function FOUNDPOSITION_TO_TLIST(CurrentFoundPos: Integer; Index: Integer): Boolean;
   Public
      Constructor Create(AOwner: TObject);
      Destructor Destroy; Override;
      Procedure ASSIGN_CALLBACKPROC(UsedProc: TurboSearchUserCallProc);
      Procedure CHANGE_CHARSET(UseASCII: Boolean; CheckCase: Boolean);
      Procedure CHANGE_SEARCHFOR(NewSearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
   End;


  	(* simple Turbosearch-object: searches only for the first appearence *)
   //TSingleFindSearchObj = Object(TSearchObj)
   TSingleFindSearchObj = Class(TSearchObj)
   Private
      LastSearchFName: shortstring;     // last searched filename
      LastFoundAbsPos: Integer;         // last found position
      Function FOUNDPOSITION_TO_TLIST(CurrentFoundPos: Integer; Index: Integer): Boolean;
   Public
      Procedure DUMP_FILEFIRSTMATCH(StartposCorrection, YBytesLen: Integer; Var OutBuffer);
      Procedure DONE;
      Procedure INIT_FILE_SEARCH(SearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
      Procedure INIT_BUFFER_SEARCH(SearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
      Function SEARCH_FILE_FIRSTMATCH(FileName: shortstring): Integer;
      Function SEARCH_BUFFER_FIRSTMATCH(inbuf: Pointer; InBufSize: Integer): Integer;
   End;


  	(* Examine ALL foundpositions *)
  	//TTurboSearchObj = Object(TSearchObj)
   TTurboSearchObj = Class(TSearchObj)
   Private
      Function PERFORM_FILE_SEARCH(FileName: shortstring): Boolean;
      Function PERFORM_BUFFER_SEARCH(inbuf: Pointer; InBufSize: Integer): Boolean;
   Public
      Procedure DONE;
      Procedure INIT_FILE_SEARCH(SearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
      Procedure INIT_BUFFER_SEARCH(SearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
      Function SEARCH_FILE_FIRSTMATCH(FileName: shortstring): Integer;
      Function SEARCH_NEXTMATCH: Integer;
      Function SEARCH_BUFFER_FIRSTMATCH(inbuf: Pointer; BufLen: Integer): Integer;
   End;


  	(* Examine ALL foundpositions with SearchString and/or/not combination *)
  	//TMultiTurboSearchObj = Object(TSearchObj)
   TMultiTurboSearchObj = Class(TSearchObj)
   Private
      PosinBuffer: Integer;
      PattComp: Array[0..MaxSearchCombinations - 1] Of TSearchCombinationType;
      OutputSearch: TSSortTyp;
      Procedure CLOSE_FILE;
      Procedure QuickSort(SortList: PPointerList; l, R: Integer; SCompare: TListSortCompare);
      Procedure SORT_RESULTS;
      Function PERFORM_FILE_SEARCH: Boolean;
      Function PERFORM_BUFFER_SEARCH(inbuf: Pointer; InBufSize: Integer): Boolean;
   Public
   	//Constructor Create(AOwner: TObject);
   	//Destructor Destroy; OVERRIDE;
      Procedure CLEAR_ALL_SEARCHSTRINGS;
      Procedure DONE;
      Procedure INIT_MULTIFILE_SEARCH(SortOutputfor: TSSortTyp; UseASCII: Boolean);
      Procedure INIT_MULTIBUFFER_SEARCH(SortOutputfor: TSSortTyp; UseASCII: Boolean);
      Function ADD_SEARCHSTRING(SearchFor: SStringTyp; CombiTyp: TSearchCombinationType;
         CheckCase: Boolean): Boolean;
      Function GETSEARCHSTRING_BY_NUMBER(InNumber: Integer): shortstring;
      Function SEARCH_FILE_FIRSTMATCH(FileName: shortstring; Var OutAbsPosition: Integer;
         Var OutSearchStringNumber: Byte): Boolean;
      Function SEARCH_NEXTMATCH(Var OutAbsPosition: Integer;
         Var OutSearchStringNumber: Byte): Boolean;
      Function SEARCH_BUFFER_FIRST(inbuf: Pointer; BufLen: Integer;
         Var OutAbsPosition: Integer; Var OutSearchStringNumber: Byte): Boolean;
   End;


Implementation


Uses
   SysUtils,
   Forms,
   Windows;


Var
   Found: Array[0..MaxSearchCombinations - 1] Of
   Record
      match: Boolean;                   // has the searchstring[0..n] matched?
      LastFoundPos: Integer;            // last found-position If any
   End;


//-------------------------------------------------------------
(* Calculates a percentage value *)

Function GET_PERCENT(TotalValue, CurValue: Integer): Byte;
Var
   res: Integer;
   RSum, R1, R2: Real;
Begin
   R1 := TotalValue;
   R2 := CurValue;
   If R1 = 0 Then R1 := 1.0;            // prevent division by zero
   If R2 = 0 Then R2 := 1.0;
   RSum := (R2 / R1) * 100;
   res := Trunc(RSum);
   If res < 0 Then res := 0;
   If res > 100 Then res := 100;
   Result := res;
End;
//-------------------------------------------------------------

Procedure TListSaveObj.INIT;
Begin
   ResList := TList.Create();             //!! Create-Check: TRY/FINALLY
   CLEAR_LIST();
End;
//-------------------------------------------------------------

Procedure TListSaveObj.CLEAR_LIST;
Begin
   ResList.Clear();
   TotPositionsCNT := 0;
End;
//-------------------------------------------------------------

Function TListSaveObj.AddItem(InRec: TListSaveRecTyp): Boolean;
Var
   DataPtr: Pointer;
Begin
   Result := True;
   Try
      GetMem(DataPtr, SizeOf(TListSaveRecTyp));
      Move(InRec, DataPtr^, SizeOf(TListSaveRecTyp));
     { Integer := } ResList.Add(DataPtr);
     { If Add-Ok Then } inc(TotPositionsCNT);
   Except
      Result := False;
   End;
End;
//-------------------------------------------------------------

Procedure TListSaveObj.DONE;
Begin
   CLEAR_LIST();
   ResList.Free();
End;
{+--------------------------------------------------------------------------+
   initializing searchpattern-array by search-String.
   Param: UseASCII must be set for searching on ANSI- or ASCII-Files/-Buffers.
          If you scan files then set UseASCII := True.
          If you scan e.g. a Memofile buffer set UseASCII := False *)
 +--------------------------------------------------------------------------+}

Procedure TSPatternData.INIT_SEARCHTABLE(SearchFor: SStringTyp;
   UseASCII: Boolean;
   CheckCase: Boolean);
Var
   i: Byte;
   s: shortstring;
Begin
   ChkCase := CheckCase;
   PatORGCopy := SearchFor;

 (*--------------------------------------------------------------------------+
  | The chars in every file on our harddisks were ASCII-coded - the full
  | charset from #0 to #255.  Any String here in Delphi was ANSI - to
  | search for e.g. a char outside the alphabet (e.g. a ASCII-drawline or
  | german umlaute) the searchprocess doesn't match and says "nothing found".
  |
  | As example fill a String with all chars from #0 to #255 and have a look
  | with the online debugger. You will see the following:
  |   ''#1#2#3#4#5#6#7#8#9#$A#$B#$C#$D#$E#$F#$10#$11#$12#$13#$14#$15#$16#$17#
  |   $18#$19#$1A#$1B#$1C#$1D#$1E#$1F' !"#$%&'()*+,-./0123456789:;<=>?
  |   @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~€‚ƒ„…†‡
  |   ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ
  |   ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ'
  |
  |
  | There are a lot of chars which will not match with an ASCII-Char at the
  | same position. (for Example: the Char at Position 252 (ü) is in the
  | ASCII-Table at position 129).
  | ==> FIX: Translate the incoming user-searchstring to the normal
  | ASCII-Table.
  *--------------------------------------------------------------------------*)
   Case UseASCII Of
      True:
         Begin
            CharToOem(@SearchFor, @s);
            Move(s[1], Pat[0], Length(SearchFor));
         End;
      False: Move(SearchFor[1], Pat[0], Length(SearchFor));
   End;

   PatLen := Length(SearchFor);
   PatLenPlus := PatLen;
   HalfLen := PatLen Div 2;

   For i := 0 To 255 Do
   Begin
      If CheckCase Then
         CompTable[i] := i
      Else
         CompTable[i] := ord(AnsiLowerCase(char(i))[1]);

      Shift[i] := PatLenPlus;
   End;

   For i := 0 To PatLen - 1 Do
      Shift[CompTable[Pat[i]]] := PatLenPlus - i;
End;
//-------------------------------------------------------------

Function TSingleFindSearchObj.FOUNDPOSITION_TO_TLIST(CurrentFoundPos: Integer;
   Index: Integer): Boolean;
Begin
   Result := False;                     // cause immediate break in calling routine
End;
{+--------------------------------------------------------------------------+
   Set Param USEASCII If you want to search on ASCII files / ASCII-Buffers (not ANSI).
   ==> If you scan files then set UseASCII := True.
       If you scan e.g. a Memofile buffer set UseASCII := False

       To check if a searchstring is in a file, do not need to load the full
       (e.g. 20 MB) file. The chance that the searchstring was found in the
       first half of the file is much greater. Read and search the file in
       small portions (TSSmallBufLen of size).

       That's more effective than reading in the entire file with a big
       filebuffer.  BUT (!) If ALL foundpositions are required then it's more
       effective to read the file in big-blocks.

       b.t.w. (E)IDE is quicker on smaller blocksizes...) *)
 +--------------------------------------------------------------------------+}

Procedure TSingleFindSearchObj.INIT_BUFFER_SEARCH(SearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
Begin
   BufisCreated := False;
   LastFoundAbsPos := -1;
   LastSearchFName := '';

  (* create searchpattern table *)
   PattsUsed := 0;
   PattData[0].INIT_SEARCHTABLE(SearchFor, UseASCII, CheckCase);
   PattData[0].FoundPosProc := FOUNDPOSITION_TO_TLIST;
End;
{+--------------------------------------------------------------------------+
   See above notes for TSingleFindSearchObj
 +--------------------------------------------------------------------------+}

Procedure TSingleFindSearchObj.INIT_FILE_SEARCH(SearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
Begin
   INIT_BUFFER_SEARCH(SearchFor, UseASCII, CheckCase);
   GetMem(Buf, TSSmallBufLen);          //!! TODO: check allocation (out of mem)
   BufisCreated := True;
End;
{+--------------------------------------------------------------------------+
   search the file for the first occurance of the searchpattern
 +--------------------------------------------------------------------------+}

Function TSingleFindSearchObj.SEARCH_FILE_FIRSTMATCH(FileName: shortstring): Integer;
Var
   FP, AbsFilePos, RRes: Integer;
Begin
   Result := -1;
   RRes := 0;
   AbsFilePos := 0;

   CLOSE_LOCFILE();

   If OPEN_LOCFILE(FileName) Then
      With PattData[0], FileData Do
         While Not EOF(FileVAR) And (Result = -1) Do
         Begin
            FP := FilePos(FileVAR);
            If (FP > 0) And (FP < fSize) Then
            Begin
               Seek(FileVAR, FP - PatLen);
               Dec(AbsFilePos, PatLen);
            End;

            inc(AbsFilePos, RRes);
            BlockRead(FileVAR, Pointer(longint(Buf))^, TSSmallBufLen, RRes);
            Application.ProcessMessages;
            Result := DoSearch(0, AbsFilePos, PattData[0].PatLen - 1, RRes);
         End;

   If Result > -1 Then
   Begin
      LastSearchFName := FileName;
      LastFoundAbsPos := Result;
   End;
End;
{+--------------------------------------------------------------------------+
   search any databuffer for pattern
   Result:   -1 = nothing found
           > -1 = found position (foundposition = 0 is the first position in
             "InBuffer") ==> zero based!

   Example: InBuffer = Array[0..n] of Char  (not Array[1..n])
            If your "InBuffer" doesn't start with zero, you have to correct
            the foundposition by yourself
 +--------------------------------------------------------------------------+}

Function TSingleFindSearchObj.SEARCH_BUFFER_FIRSTMATCH(inbuf: Pointer;
   InBufSize: Integer): Integer;
Begin
   Buf := inbuf;
   Result := DoSearch(0, 0, PattData[0].PatLen - 1, InBufSize);
End;
{+--------------------------------------------------------------------------+
   Dumps n Bytes from the last searched file if the searchstring was found!

   Correct the read-startposition by the value "StartposCorrection". (e.g.
   Foundposition is 1000, your startposition should Begin to read 10 bytes
   previous (at position 990) then launch PRC with (-10, Dumplength,
   YourBuffer) "YBytesLen" defines how much bytes we want to read.

   If an file-IO error occurs or no last foundposition exists the "OutBuffer"
   is internal FillChar't with zero with the length of "YBytesLen".
 +--------------------------------------------------------------------------+}

Procedure TSingleFindSearchObj.DUMP_FILEFIRSTMATCH(StartposCorrection,
   YBytesLen: Integer;
   Var OutBuffer);
Var
   RRes, SeekStartPos: Integer;
Begin
   FillChar(OutBuffer, YBytesLen, 0);
   If (LastSearchFName <> '') And (LastFoundAbsPos > -1) Then
   Begin
      SeekStartPos := LastFoundAbsPos + StartposCorrection;
      If SeekStartPos < 0 Then SeekStartPos := 0;

      OPEN_LOCFILE(LastSearchFName);
      Seek(FileData.FileVAR, SeekStartPos);
      BlockRead(FileData.FileVAR, OutBuffer, YBytesLen, RRes); // don't check Read-Result (RRes) with YBytesLen!
                                                     //  It's possible that YBytesLen initiate a read
                                                     //  over the file-end. Your "Outbuffer" then contains
                                                     //  only a part of data but already dumped information!

      CLOSE_LOCFILE();

     (* read-error? ==> clear Outbuffer *)
      If RRes <= 0 Then
         FillChar(OutBuffer, YBytesLen, 0);
   End;
End;
//-------------------------------------------------------------
(* Deinitialization and destroying searchbuffer *)

Procedure TSingleFindSearchObj.DONE;
Begin
   LastFoundAbsPos := -1;
   LastSearchFName := '';
   If BufisCreated Then
      FreeMem(Buf, TSSmallBufLen);
   BufisCreated := False;
End;
{+--------------------------------------------------------------------------+
   Change the uses Charset from ASCII to ANSI or backto ASCII. --> See
   the .INIT Procedure
 +--------------------------------------------------------------------------+}

Procedure TSearchObj.CHANGE_CHARSET(UseASCII: Boolean; CheckCase: Boolean);
Var
   i: Integer;
Begin
   UseASCIICharSet := UseASCII;
   For i := 0 To PattsUsed Do
      PattData[i].INIT_SEARCHTABLE(
         PattData[i].PatORGCopy,
         UseASCIICharSet,
         CheckCase);
End;
//-------------------------------------------------------------
(* Redefine the User's searchphrase without completely .DONE and re-.INIT the object *)

Procedure TSearchObj.CHANGE_SEARCHFOR(NewSearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
Begin
   PattData[0].INIT_SEARCHTABLE(NewSearchFor, UseASCII, CheckCase);
End;
//-------------------------------------------------------------

Procedure TSearchObj.CLOSE_LOCFILE;
Begin
   With FileData Do
   Begin
{$I-}
      If IoResult <> 0 Then ;           // clear IOResult
      CloseFile(FileVAR);
      If IoResult <> 0 Then ;
{$I+}

      fSize := 0;
      IsOpen := False;
      IsEOF := False;
   End;
End;
//-------------------------------------------------------------

Function TSearchObj.READ_FILEBLOCK: Boolean;
Var
   FP: Integer;
Begin
   LastReadResult := 0;
   Try
      With FileData Do
      Begin
{$I-}
         If IoResult <> 0 Then ;

        // correct buffer read wrap if the searchpattern was split at the end of the last fileread
        // and the beginning of the new fileread }
         FP := FilePos(FileVAR);
         If (FP > PattData[LoopIndex].PatLen) And (FP < FileData.fSize) Then
         Begin
          { If a very short searchstring and one VERY long, the short String
             would be found multiple times at the same position when we step back in
             the filebuffer, to fix any overlapped String at the end/beginning of the
             next filebuffer-load.

             E.g. SearchString1  = house                          Length =  5
                  SearchString2  = my household is very chaotic   Length = 28

                  Buffer ends on = 'my household is ver'
                                    ==> HERE THIS fileread ends at this point,
                                    only "HOUSE" is matching.

                  Now load the next portion of the file - first seek back in
                  the file for the longest searchstring (28 chars):
                  Next bufferload = 'deHOUSExxMY HOUSEHOLD IS VERY
                  CHAOTICxxxxxxxxxxx....'

                  !As you see, HOUSE was found once again (and Searchstring2
                  matches the first time)!

                  How to fix? ==> Step back is OK and needed to find ALL
                  matches in the file, but don't start the next search with
                  always the same position (or in the normal search-case with
                  length(SearchString)) - we have to set the starting point
                  individual for every searchstring search. (see mark at (1) )

                  FIX: cummulate the maximum backward-seek with the current
                  processed searchstring length! }
            Seek(FileVAR, FP - PattData[LoopIndex].PatLen {LongestPatternlen});
            Dec(AbsPosInFile, PattData[LoopIndex].PatLen {LongestPatternlen});
         End;
         inc(AbsPosInFile, LastReadResult);
         BlockRead(FileVAR, Pointer(longint(Buf))^, TSMaxBufLen, LastReadResult);
         IsEOF := (EOF(FileVAR) Or (LastReadResult <= 0));
         If IoResult <> 0 Then ;
{$I+}
      End;
   Finally
      Result := LastReadResult > 0;
   End;
End;
//-------------------------------------------------------------

Function TSearchObj.OPEN_LOCFILE(FileName: shortstring): Boolean;
Begin
   CLOSE_LOCFILE();

   With FileData Do
   Begin
{$I-}
      If IoResult <> 0 Then ;           // clear IOResult
      AssignFile(FileVAR, FileName);
      FileMode := 0;                    // read only
      Reset(FileVAR, 1);
      FileMode := 2;                    // read & write
      fSize := FileSize(FileVAR);
      Result := (IoResult = 0) And      // IO-Error? / filesize equal or
         (fSize >= PattData[0].PatLen);
                                                   // greater as the searchpattern-length?
      If IoResult <> 0 Then ;
{$I+}

      Case Result Of
         True:
            Begin
               IsOpen := True;          // file is open
               IsEOF := False;          // not EOF
            End;
         False:                         // ERROR?
            CLOSE_LOCFILE();
      End;
   End;
End;
//-------------------------------------------------------------

Function TSearchObj.DoSearch(Index, AbsFilePos, StartPos, BufSize: Integer): Integer;
Var
   i: Integer;
   b, CompareByte: ^Byte;
Begin
   Result := -1;

   With PattData[Index] Do
   Begin
      CompareByte := Pointer(longint(Buf) + StartPos {PatLen-1});
      While longint(CompareByte) < (longint(Buf) + BufSize - (PatLen + 1)) Do
      Begin
         i := PatLen - 1;
         If (CompTable[Pat[i]] <> CompTable[CompareByte^]) Then
         Begin
            inc(CompareByte, Shift[CompTable[pByte(longint(CompareByte) + 1)^]]);
         End
         Else
         Begin
            b := CompareByte;
            Repeat
               Dec(i);
               Dec(b);
            Until (i <= -1) Or (CompTable[Pat[i]] <> CompTable[b^]);

            If i = -1 Then              //all bytes compare
            Begin
               res := True;             // found something
               Result := (AbsFilePos) + (longint(b) - longint(Buf)) + 1;
               If Not FoundPosProc(Result, Index) Then
                  break;

               inc(CompareByte);
            End
            Else
               If i < HalfLen Then
                  inc(CompareByte)
               Else
                  inc(CompareByte, Shift[CompTable[pByte(longint(CompareByte) + 1)^]]);

         End;
      End;
   End;
End;
//-------------------------------------------------------------

Function TSearchObj.FOUNDPOSITION_TO_TLIST(CurrentFoundPos: Integer;
   Index: Integer): Boolean;
Var
   SaveData: TListSaveRecTyp;
Begin
   Try
      Found[Index].match := True;
      Found[Index].LastFoundPos := CurrentFoundPos;
      SaveData.FoundPos := CurrentFoundPos;
      SaveData.SearchStringnum := Byte(Index);
      Result := FoundPosList.AddItem(SaveData);
   Except
      Result := False;                  // cause immediate break in calling routine
   End;
End;
//-------------------------------------------------------------

Constructor TSearchObj.Create;
Var
   i: word;
Begin
   Inherited Create;
   For i := 0 To MaxSearchCombinations - 1 Do
      PattData[i] := TSPatternData.Create();

   FoundPosList := TListSaveObj.Create();
   FileData := TSFileObj.Create();
End;
//-------------------------------------------------------------

Destructor TSearchObj.Destroy;
Var
   i: word;
Begin
   For i := 0 To MaxSearchCombinations - 1 Do
      PattData[i].Free();

   FileData.Free();
   FoundPosList.Free();
   Inherited Destroy;
End;
{+--------------------------------------------------------------------------+
   Assigns the user callbackproc for statusinformation and the ability
   to terminate the search on a big file
 +--------------------------------------------------------------------------+}

Procedure TSearchObj.ASSIGN_CALLBACKPROC(UsedProc: TurboSearchUserCallProc);
Begin
   CallbackProc := @UsedProc;
End;
{+--------------------------------------------------------------------------+
   Search the user's phrase on the whole file.  The found positions were
   stored internal at a TList for backgiving it by calling the functions
   SEARCH_FILE_FIRSTMATCH/...FILENEXT

   It's much quicker to store the found-results temporarily and continue the
   searchprocess than interrupting --> giving back the first result -->
   continue search until EOF.
 +--------------------------------------------------------------------------+}

Function TTurboSearchObj.PERFORM_FILE_SEARCH(FileName: shortstring): Boolean;
Begin
   res := False;
   TerminateSearch := False;
   CLOSE_LOCFILE();                       // close file
   FoundPosList.CLEAR_LIST();

   If Assigned(CallbackProc) Then
      CallbackProc(TerminateSearch, False, 0, 0);

   Try
      (* keep searching? *)
      If (Not TerminateSearch) And OPEN_LOCFILE(FileName) Then
      Try
         AbsPosInFile := 0;
         LastReadResult := 0;

         Repeat
            If Not READ_FILEBLOCK Then break;

            If Assigned(CallbackProc) Then
            Begin
               CallbackProc(TerminateSearch, False, FoundPosList.TotPositionsCNT,
                  GET_PERCENT(FileData.fSize, AbsPosInFile));

               Application.ProcessMessages;
               If TerminateSearch Then break;
            End;

            If (Not TerminateSearch) And (LastReadResult > 0) Then
               DoSearch(0, AbsPosInFile, PattData[0].PatLen - 1, LastReadResult);

         Until ((FileData.IsEOF) And (LastReadResult <= 0));
      Finally
         CLOSE_LOCFILE();
      End;
   Finally
      Result := res;
      If Assigned(CallbackProc) Then
         CallbackProc(TerminateSearch, False,
            FoundPosList.TotPositionsCNT, 100);
   End;
End;
//-------------------------------------------------------------
(* search all matches in a buffer *)

Function TTurboSearchObj.PERFORM_BUFFER_SEARCH(inbuf: Pointer; InBufSize: Integer): Boolean;
Begin
   res := False;
   Try
      TerminateSearch := False;
      FoundPosList.CLEAR_LIST();

      If Assigned(CallbackProc) Then
         CallbackProc(TerminateSearch, False, 0, 0);

     (* keep on searching? *)
      If Not TerminateSearch Then
      Begin
         Buf := inbuf;
         DoSearch(0, 0, PattData[0].PatLen - 1, InBufSize);
      End;

     (* call user-backproc *)
      If Assigned(CallbackProc) Then
         CallbackProc(TerminateSearch, False,
            FoundPosList.TotPositionsCNT, 100);
   Finally
      Result := res;
   End;
End;
//-------------------------------------------------------------

Procedure TTurboSearchObj.INIT_BUFFER_SEARCH(SearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
Begin
   BufisCreated := False;
   CallbackProc := Nil;

  //PattData[0] := TSPatternData.Create;
  //FoundPosList := TListSaveObj.Create;

   FoundPosList.INIT;
   PattsUsed := 0;
   PattData[0].INIT_SEARCHTABLE(SearchFor, UseASCII, CheckCase);
   PattData[0].FoundPosProc := FOUNDPOSITION_TO_TLIST;
End;
{+--------------------------------------------------------------------------+
   Initialize the search for all position in a file
   Set USEASCII to search in ASCII-Files or ASCII-Buffers
   ==> If scanning files, set UseASCII := True.
       If scanning an object such as a Memofile buffer, set UseASCII := False
 +--------------------------------------------------------------------------+}

Procedure TTurboSearchObj.INIT_FILE_SEARCH(SearchFor: SStringTyp; UseASCII: Boolean; CheckCase: Boolean);
Begin
   INIT_BUFFER_SEARCH(SearchFor, UseASCII, CheckCase);
   GetMem(Buf, TSMaxBufLen);
   BufisCreated := True;
End;
{+--------------------------------------------------------------------------+
   Open a file and searches for the ALL appearence of the searchstring.
   The position in the file is strored internal and giving back by calling
   first this routine and until result = -1 by the Function SEARCH_NEXTMATCH.

   It works like the normal Findfirst / Findnext.
   Result:   -1 = a) file don't exists or file IOError   or
                  b) no match
           > -1 = Absoluteposition of the match (first Byte = Pos 0)
                  Resultposition is prepared for a Seek(File, Foundposition)

   Memo  : the call of this FNC always closes any open file and opens the new
           one.  With a single call of INIT, searches are premitten of multiple
           files by calling Searchfirst..next multiple times without
           permanently do a INIT / ... / DONE.
 +--------------------------------------------------------------------------+}

Function TTurboSearchObj.SEARCH_FILE_FIRSTMATCH(FileName: shortstring): Integer;
Begin
   Result := -1;
   UserBackPosition := 0;
   LongestPatternLen := PattData[0].PatLen;
   If PERFORM_FILE_SEARCH(FileName) Then
      Result := SEARCH_NEXTMATCH;
End;
{+--------------------------------------------------------------------------+
   Search a file for a specified pattern
   Result:   -1 = nothing / nothing more found
           > -1 = Absoluteposition (1st Byte = Pos 0)
                  prepared for e.g. using Seek (F, foundposition);
 +--------------------------------------------------------------------------+}

Function TTurboSearchObj.SEARCH_NEXTMATCH: Integer;
Var
   DataRec: TListSaveRecTyp;
Begin
   Result := -1;
   inc(UserBackPosition);
   If (UserBackPosition > 0) And (UserBackPosition <= FoundPosList.TotPositionsCNT) Then
   Begin
      Move(FoundPosList.ResList.Items[Pred(UserBackPosition)]^, DataRec, SizeOf(TListSaveRecTyp));
      Result := DataRec.FoundPos;
   End;
End;
{+--------------------------------------------------------------------------+
   Initilize search of the data-buffer which can have any structure. The
   Buffer can be of any size.
 +--------------------------------------------------------------------------+}

Function TTurboSearchObj.SEARCH_BUFFER_FIRSTMATCH(inbuf: Pointer; BufLen: Integer): Integer;
Begin
   Result := -1;
   UserBackPosition := 0;
   If PERFORM_BUFFER_SEARCH(inbuf, BufLen) Then
      Result := SEARCH_NEXTMATCH();
End;
//-------------------------------------------------------------

Procedure TTurboSearchObj.DONE;
Begin
   CLOSE_LOCFILE();
   If BufisCreated Then
      FreeMem(Buf, TSMaxBufLen);

   BufisCreated := False;
   FoundPosList.DONE();
End;
//-------------------------------------------------------------

Procedure TMultiTurboSearchObj.CLOSE_FILE;
Begin
   CLOSE_LOCFILE();
   PosinBuffer := -1;
   AbsPosInFile := -1;
   LastReadResult := 0;
End;
//-------------------------------------------------------------

Procedure TMultiTurboSearchObj.QuickSort(SortList: PPointerList;
   l, R: Integer;
   SCompare: TListSortCompare);
Var
   i, j: Integer;
   p, t: Pointer;
Begin
   Repeat
      i := l;
      j := R;
      p := SortList^[(l + R) Shr 1];
      Repeat
         While SCompare(SortList^[i], p) < 0 Do
            inc(i);
         While SCompare(SortList^[j], p) > 0 Do
            Dec(j);
         If i <= j Then
         Begin
            t := SortList^[i];
            SortList^[i] := SortList^[j];
            SortList^[j] := t;
            inc(i);
            Dec(j);
         End
         Else
            If i = j Then
            Begin
               inc(i);
               Dec(j);
            End;
      Until i > j;
      If l < j Then QuickSort(SortList, l, j, SCompare);
      l := i;
   Until i >= R;
End;
//-------------------------------------------------------------

Function COMPAREPROC_FOUNDPOS(Val1, Val2: Pointer): Integer;
Begin
   If TListSaveRecTyp(Val1^).FoundPos > TListSaveRecTyp(Val2^).FoundPos Then
      Result := 1
   Else
   Begin
      If TListSaveRecTyp(Val1^).FoundPos < TListSaveRecTyp(Val2^).FoundPos Then
         Result := -1
      Else
         Result := 0;
   End;
  // sort descendenting: Result := -Result;
End;
//-------------------------------------------------------------

Function COMPAREPROC_SSTRINGNUM(Val1, Val2: Pointer): Integer;
Const
   EmptyStr = '                   ';
Var
   TmpS: shortstring;                   // place here and not in FNC CHARSTR, because then
                         // this Var doesn't need to be created, stacked and
                         // disposed by the compiler on every CHARSTR-FNC call!

   Function CHARSTR(InInt1, InInt2: Integer): shortstring;
   Begin
      TmpS := EmptyStr + IntToStr(InInt2);
      CHARSTR := IntToStr(InInt1) + Copy(TmpS, Length(TmpS) - 13, Length(TmpS));
   End;

Begin
   Result := CompareText(CHARSTR(TListSaveRecTyp(Val1^).SearchStringnum,
      TListSaveRecTyp(Val1^).FoundPos),
      CHARSTR(TListSaveRecTyp(Val2^).SearchStringnum,
      TListSaveRecTyp(Val2^).FoundPos));

  // This caparison is rather slow.
  //   We have to sort the result position on the searchstring-number.
  //   In here i create a fixed length String in this format:
  //   "2  123456"  First digit: Searchstring number, second digit: foundposition }

  // sort descendenting: Result := -Result;               }
End;
//-------------------------------------------------------------

Procedure TMultiTurboSearchObj.SORT_RESULTS;
Var
   Terminateit: Boolean;
Begin
   Terminateit := False;

   If (FoundPosList.ResList.List <> Nil) And // Resultlist created?
      (FoundPosList.TotPositionsCNT > 0) And // more than one foundposition?
      (OutputSearch <> TSSort_None) Then // we shall sort the results
   Begin
   (* If lot's of matches this can take a while... *)
      If Assigned(CallbackProc) Then
         CallbackProc(Terminateit, True, FoundPosList.TotPositionsCNT, 100);

      If Not Terminateit Then
         Case OutputSearch Of

            TSSort_Position:

               QuickSort(FoundPosList.ResList.List,
                  0,
                  Pred(FoundPosList.TotPositionsCNT),
                  COMPAREPROC_FOUNDPOS);

            TSSort_SString:

               QuickSort(FoundPosList.ResList.List,
                  0,
                  Pred(FoundPosList.TotPositionsCNT),
                  COMPAREPROC_SSTRINGNUM);
         End;

      If Assigned(CallbackProc) Then
         CallbackProc(Terminateit, False, FoundPosList.TotPositionsCNT, 100);
   End;
End;
{+--------------------------------------------------------------------------+
   Initializes the MultisearchObject
   Set the UseASCII-Param like the following explanations:
   ==> If you scan files then set UseASCII := True.
       If you scan e.g. a Memofile buffer set UseASCII := False
 +--------------------------------------------------------------------------+}

Procedure TMultiTurboSearchObj.INIT_MULTIFILE_SEARCH(SortOutputfor: TSSortTyp; UseASCII: Boolean);
Begin
   INIT_MULTIBUFFER_SEARCH(SortOutputfor, UseASCII);
   GetMem(Buf, TSMaxBufLen);            //!! TODO: Getmem Check !
   BufisCreated := True;
End;
{+--------------------------------------------------------------------------+
 +--------------------------------------------------------------------------+}

Procedure TMultiTurboSearchObj.INIT_MULTIBUFFER_SEARCH(SortOutputfor: TSSortTyp; UseASCII: Boolean);
Begin
   BufisCreated := False;
   CallbackProc := Nil;
   LoopIndex := 0;

  (*create the Found-Postion TList which holds the necessary foundpositions.
    It is much quicker on searching BIG files if we temporarily store the
    found positions and give them on an external Function back to the
    main-program, than to terminate the search process, give the postion to
    the program and then continue searching. *)
   FoundPosList.INIT;
   UserBackPosition := 0;

   UseASCIICharSet := UseASCII;
   OutputSearch := SortOutputfor; (* How should the results be sorted?
                                          Because we read a big part of the
                                          searchfile and search at first for
                                          the 1st Searchstring, then this file
                                          partbuffer for next Searchstring and
                                          so on, we get the results not
                                          ascending ==> we have to sort (If
                                          the user wants it) the position
                                          results... *)

  //CLEAR_ALL_SEARCHSTRINGS;
   PattsUsed := -1;
   FoundPosList.CLEAR_LIST;
   UserBackPosition := 0;
End;
{+--------------------------------------------------------------------------+
   Clear all set Searchstrings for assigning new ones, without the need for
   .DONE and re-.INIT the Object                                                                           *
 +--------------------------------------------------------------------------+}

Procedure TMultiTurboSearchObj.CLEAR_ALL_SEARCHSTRINGS;
Begin
   PattsUsed := -1;
   FillChar(PattData[0], SizeOf(PattData[0]), 0);
   FoundPosList.CLEAR_LIST;
   UserBackPosition := 0;
End;
{+--------------------------------------------------------------------------+
   This Function adds up to 'MaxSearchCombinations' Strings which has to
   appear in a file.  Function returns FALSE, If the current Searchstring can't
   be added (array-limit passed)

         If you define NEW searchstrings you have to .DONE and re.INIT the
         object OR call Procedure CLEAR_ALL_SEARCHSTRINGS!!
   Each searchstring can be 'linked' with the following appearence-options:
   MUST, CAN, NOT.

   e.g. Search a file for: - S1, MUST (S1 MUST be found in the file)  and
                           - S2, CAN  (S2 CAN be found in the file)   and
                           - S3, CAN  (S3 CAN befound in the file)    and
                           - S4, NOT  (S4 don't has to appear in the file)

   When the search is started S1 must appear in the file. S2 and S3 can be
   found.  If S4 is found, then File is of no intererest for the
   user --> ignore.

   This type of searching is a little bit different like other combination
   algorithms, because a check of logical combinations is not required.

   Example: (S1 and S2) not (S3 or S4)   -or-   S1 and (S2 or S3) not S4.
            When combining searchstrings like the example, analyse the
            appearence of each search string over the WHOLE file.  After
            searching a complicated checking which condition has matched and
            which not.  But, if combining each Searchstring with only ONE
            possible combination without linking the strings (by and/or/...),
            it's much simpler to check which condition was OK and which was not.

            It is possible to terminate the search if e.g. a NOT combined
            String was found - without searching the rest of the file.
 +--------------------------------------------------------------------------+}

Function TMultiTurboSearchObj.ADD_SEARCHSTRING(SearchFor: SStringTyp;
   CombiTyp: TSearchCombinationType;
   CheckCase: Boolean): Boolean;
Begin
   Result := False;
   If PattsUsed < MaxSearchCombinations - 1 Then // enough room left? (Array-Limit check)
   Begin
      inc(PattsUsed);
      PattComp[PattsUsed] := CombiTyp;  // how should the String be appear?
      PattData[PattsUsed].INIT_SEARCHTABLE(SearchFor, UseASCIICharSet, CheckCase); // define the searchtable for this searchstring
      Result := True;
   End;
End;
{+--------------------------------------------------------------------------+
   If the search was finished, we got back a position only a "stringnumber".
   This says, which String was found on what position. With this Function we
   read out the (user-)String itself. The user searchstrings were stored in the
   appearence like they were assigned with Function .ADD_SEARCHSTRING
 +--------------------------------------------------------------------------+}

Function TMultiTurboSearchObj.GETSEARCHSTRING_BY_NUMBER(InNumber: Integer): shortstring;
Begin
   Result := '';
   If (InNumber > -1) And (InNumber <= PattsUsed) Then
      Result := PattData[InNumber].PatORGCopy;
End;
{+--------------------------------------------------------------------------+
   Start the Multisearch on a file.

   It works like the normal Findfirst and findnext routines.

   If we had a searchstring with the NOT condition, we had to read in and
   analyse the WHOLE file before we can say "found" or "not found". The
   search process was only handles with calling this Function. The
   .SEARCH_FILENEXT Function reads out only the stored result positions
 +--------------------------------------------------------------------------+}

Function TMultiTurboSearchObj.SEARCH_FILE_FIRSTMATCH(FileName: shortstring;
   Var OutAbsPosition: Integer;
   Var OutSearchStringNumber: Byte): Boolean;
Var
   i: Integer;
Begin
   Result := False;
   OutAbsPosition := -1;
   OutSearchStringNumber := 0;
   CLOSE_FILE();

   FoundPosList.CLEAR_LIST();

  // Examine the longest searchstring for buffer backward stepping reading
  // on loading the next filepart
   LongestPatternLen := 0;
   For i := 0 To PattsUsed Do
   Begin
      If PattData[i].PatLen >= LongestPatternLen Then
         LongestPatternLen := PattData[i].PatLen;

      PattData[i].FoundPosProc := FOUNDPOSITION_TO_TLIST;
   End;

   If OPEN_LOCFILE(FileName) Then
   Begin
      Result := PERFORM_FILE_SEARCH();
      If Result Then
      Begin
         UserBackPosition := 0;
         SORT_RESULTS();
         Result := SEARCH_NEXTMATCH(OutAbsPosition, OutSearchStringNumber);
      End;
   End;
End;
//-------------------------------------------------------------
(*  Give back the next stored found position *)

Function TMultiTurboSearchObj.SEARCH_NEXTMATCH(Var OutAbsPosition: Integer;
   Var OutSearchStringNumber: Byte): Boolean;
Var
   DataRec: TListSaveRecTyp;
Begin
   Result := False;
   inc(UserBackPosition);
   If (UserBackPosition > 0) And (UserBackPosition <= FoundPosList.TotPositionsCNT) Then
   Begin
      Move(FoundPosList.ResList.Items[Pred(UserBackPosition)]^, DataRec, SizeOf(TListSaveRecTyp));
      OutAbsPosition := DataRec.FoundPos;
      OutSearchStringNumber := Byte(DataRec.SearchStringnum);
      Result := True;
   End;

  (* Error? Access outside limit? ==> Clear reset out-VARs *)
   If Not Result Then
   Begin
      OutAbsPosition := -1;
      OutSearchStringNumber := 0;
   End;
End;
//-------------------------------------------------------------
(* Performs the search for multiple strings on a file *)

Function TMultiTurboSearchObj.PERFORM_FILE_SEARCH: Boolean;

   (* Check the result array (FOUND[]) if any CAN Searchstrings were found. *)

Function ANY_CANCONDITION_FOUND: Boolean;
   Var
      Dg: Integer;
   Begin
      Result := False;
      For Dg := 0 To PattsUsed Do
         If (PattComp[Dg] = TSCombi_CAN) And Found[Dg].match Then
         Begin
            Result := True;
            Exit;
         End;
   End;

   Function NOTCONDITION_FOUND: Boolean;
   Var
      Dg: Integer;
   Begin
      Result := False;
      For Dg := 0 To PattsUsed Do
         If (PattComp[Dg] = TSCombi_NOT) And (Found[Dg].match) Then
         Begin
            Result := True;
            Exit;
         End;
   End;

  (* Check if the uses has defined at minimum one MUST String-condition *)

   Function ANY_MUSTCONDITION_DEFINED: Boolean;
   Var
      Dg: Integer;
   Begin
      Result := False;
      For Dg := 0 To PattsUsed Do
         If (PattComp[Dg] = TSCombi_MUST) Then
         Begin
            Result := True;
            Exit;
         End;
   End;

  (* Check the result array (FOUND[]) if all MUST Searchstrings were found.
     If NO MUST conditioned Searchstring is defined by the user, the
     FNC returns allways "OK - found" (needed for checking the other options) *)

   Function ALL_MUSTCONDITIONS_FOUND: Boolean;
   Var
      Dg: Integer;
   Begin
      Result := True;
      For Dg := 0 To PattsUsed Do
         If (PattComp[Dg] = TSCombi_MUST) And (Not Found[Dg].match) Then
         Begin
            Result := False;
            Exit;
         End;
   End;

  (* Checks if the uses has defined at minimum one CAN String-condition *)

   Function ANY_CANCONDITION_DEFINED: Boolean;
   Var
      Dg: Integer;
   Begin
      Result := False;
      For Dg := 0 To PattsUsed Do
         If (PattComp[Dg] = TSCombi_CAN) Then
         Begin
            Result := True;
            Exit;
         End;
   End;

Begin
   Result := False;
   TerminateSearch := False;

   FoundPosList.CLEAR_LIST();
   UserBackPosition := 0;

   If Assigned(CallbackProc) Then
      CallbackProc(TerminateSearch, False, FoundPosList.TotPositionsCNT, 0);

   If Not TerminateSearch Then
      If (PattsUsed > -1) And (FileData.IsOpen) Then
      Begin
        // so far so good: start search - check every searchstring. Check the
        // appearence of every searchstring.  After all searches check the
        // Link-Combinations and set the FNC result
         FillChar(Found, SizeOf(Found), False);

        (* Loop through each searchstring *)
         Repeat
            If TerminateSearch Then break;

            If (PosinBuffer <= 0) Then
            Begin

               If Not READ_FILEBLOCK() Then break;

               If PosinBuffer = -1 Then // any buffer processed? ==> -1 is set only once after fileopen
                  AbsPosInFile := 0;

               PosinBuffer := 0;
            End;

            PERFORM_BUFFER_SEARCH(Buf, LastReadResult);

         Until (TerminateSearch) Or ((FileData.IsEOF) And (LastReadResult <= 0));
      End;


  (* Here comes the checking for the searchstring AND/OR/NOT combinations and
     setting the FNC result  *)
   If Not NOTCONDITION_FOUND Then       // first check the NOT condition
      Case ANY_MUSTCONDITION_DEFINED Of // is at minimum one MUST condition defined?
                                                 //  (! it's possible to have only CAN-conditions!)
         True:
            Result := ALL_MUSTCONDITIONS_FOUND; // were all MUST conditions (If any) found?
                                                 //   --> something (at min one match) is/was found
                                                 //   ==> If so: we can ignore results of all CAN conditions
         False:
            If ANY_CANCONDITION_DEFINED Then
               Result := ANY_CANCONDITION_FOUND;
      End;
  // all above (not present) "ELSE" cases doesn't match to the user defined
  // combination of the searchstrings ==> FNC result on the current searchfile
  // is "matches not found in file" (False)

   If Assigned(CallbackProc) Then
      CallbackProc(TerminateSearch, False,
         FoundPosList.TotPositionsCNT, 100);
End;
//-------------------------------------------------------------
(* Perform the search for multiple strings on a buffer *)

Function TMultiTurboSearchObj.PERFORM_BUFFER_SEARCH(inbuf: Pointer; InBufSize: Integer): Boolean;
Var
   LocPos,
      Loop: Integer;
Begin
   res := False;
   Try
      If (Not TerminateSearch) And (PattsUsed > -1) And (InBufSize > 0) Then
      Begin
         If Not BufisCreated Then       //if BufIsCreate... then is a file search, else buffer search
         Begin
            TerminateSearch := False;
            FoundPosList.CLEAR_LIST();
            UserBackPosition := 0;
            If Assigned(CallbackProc) Then
               CallbackProc(TerminateSearch, False, FoundPosList.TotPositionsCNT, 0);

            FillChar(Found, SizeOf(Found), False);
            Buf := inbuf;

            If (PosinBuffer <= 0) Then
            Begin
               If PosinBuffer = -1 Then
                  AbsPosInFile := 0;

               PosinBuffer := 0;
            End;
         End;


         For Loop := 0 To PattsUsed Do
         Begin

            If Assigned(CallbackProc) Then
            Begin
               CallbackProc(TerminateSearch, False,
                  FoundPosList.TotPositionsCNT,
                  GET_PERCENT(FileData.fSize, AbsPosInFile));
               Application.ProcessMessages();
               If TerminateSearch Then break;
            End;

            If Not TerminateSearch Then
            Begin
               If PosinBuffer = 0 Then
               Begin
                  If AbsPosInFile > 0 Then // have we ever loaded a buffer?
                     LocPos := LongestPatternLen // (1) ==> see comments at local PRC READ_FILEBLOCK
                  Else
                     LocPos := PattData[Loop].PatLen - 1;
               End
               Else
                  LocPos := PosinBuffer; // If here: we had loaded the filebuffer the first time

               DoSearch(Loop, AbsPosInFile, LocPos, InBufSize);
               LoopIndex := Loop;
            End;
         End;
      End;
   Finally
      Result := res;
   End;
End;
//-------------------------------------------------------------
{Constructor TMultiTurboSearchObj.Create;
Begin
 Inherited Create(Self);
End;
//-------------------------------------------------------------
Destructor TMultiTurboSearchObj.Destroy;
Begin
 Inherited Destroy;
End;}
{+--------------------------------------------------------------------------+
   Start the Multisearch on a buffer.
   It works like the normal Findfirst and findnext routines.
 +--------------------------------------------------------------------------+}

Function TMultiTurboSearchObj.SEARCH_BUFFER_FIRST(inbuf: Pointer;
   BufLen: Integer;
   Var OutAbsPosition: Integer;
   Var OutSearchStringNumber: Byte): Boolean;
Var
   i: Integer;
Begin
   Result := False;
   OutAbsPosition := -1;
   OutSearchStringNumber := 0;
   CLOSE_FILE();

   FoundPosList.CLEAR_LIST();

  (* Examine the longest searchstring for buffer backward stepping reading
     on loading the next filepart *)
   LongestPatternLen := 0;
   For i := 0 To PattsUsed Do
   Begin
      If PattData[i].PatLen > LongestPatternLen Then
         LongestPatternLen := PattData[i].PatLen;

      PattData[i].FoundPosProc := FOUNDPOSITION_TO_TLIST;
   End;

  (* Buffer contains at minimum so much data like the longest search phrase? *)
   If BufLen >= LongestPatternLen Then
   Begin
      Result := PERFORM_BUFFER_SEARCH(inbuf, BufLen);
      If Result Then
      Begin
         UserBackPosition := 0;
         SORT_RESULTS();
         Result := SEARCH_NEXTMATCH(OutAbsPosition, OutSearchStringNumber);
      End;
   End;
End;
//-------------------------------------------------------------
(* removes the Multisearch Object from Heap, closes all open files and
   free's the result-list    *)

Procedure TMultiTurboSearchObj.DONE;
Begin
   CLOSE_FILE();
   If BufisCreated Then
      FreeMem(Buf, TSMaxBufLen);
   BufisCreated := False;
   FoundPosList.DONE();
   FillChar(PattData[0], SizeOf(PattData[0]), 0);
   PattsUsed := -1;
End;
//-------------------------------------------------------------


End.
