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
Unit ztvUnZip;

Interface

Uses
   Windows,
   SysUtils,
   Classes,
   Dialogs,
   ztvRegister,
   ztvBase,
   ztvGbls,
   ztvHeaders,
   ztvStreams;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TUnZip = Class(TUnBASE)
   Private
      Central64Hdr: TCentral64Hdr;
      DiskSpannObj: TDiskSpannObj;
      Procedure RecoveryProcessHeaders(Var LocalStream: TStream32; outFile: TStream32);
      Procedure ProcessHeaders(Var LocalStream: TStream32; outFile: TStream32);
      Function RecoveryBuildHeadArray(Var LocalStream: TStream32): Integer;
      Function BuildHeadArray(Var LocalStream: TStream32): Integer;
      Function ExtractInflate(Var LocalStream: TStream32; outFile: TStream32;
         MAX_WBITS: smallint): Boolean;
      Function ExtractInflate32(Var LocalStream: TStream32; outFile: TStream32;
         MAX_WBITS: smallint): Boolean;
      Function ExtractInflate64(Var LocalStream: TStream32; outFile: TStream32;
         MAX_WBITS: smallint): Boolean;
      Function OpenAndExtractFile(Var LocalStream: TStream32; outFile: TStream32;
         FileAttr, Index: Integer): Boolean;
   Protected
		Function GetNextZipVolume(Var LocalStream: TStream32; VolNum,
			LastVolumeInSet: Integer): Boolean;
      Function VerifyPassword(Buffer: PChar): Boolean; Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
      Function ExtractFile(UnBase: TUnBASE; FName: String; Var inStream:
      	TStream32; outFile: TStream32; IRec: TInflateRec; Index: Integer):
         Boolean;
		Procedure ExtractIT(Var inStream: TStream32; outFile: TStream32); Override;
   Published
      Property ArcType;
      Property ConfirmOverwrites;
      Property CreateStoredDirs;
      Property DateAttribute;
      Property ExtractDir;
      Property FileSpec;
      Property OverwriteMode;
      Property PasswordAttempts;
      Property Passwords;
      Property RestoreFileAttr;
      Property UseStoredDirs;
      Property ZipCmntBufSize;
      Property OnActivate;
      Property OnBegin;
      Property OnDeactivate;
      Property OnEnd;
      Property OnError;
      Property OnGetPassword;
      Property OnNextVolume;
      Property OnGetZipFirstDisk;
      Property OnGetZipNextDisk;
      Property OnGetZipLastDisk;
      Property OnFileExists;
      Property OnProgress;
      Property OnCorruptZipHeader;
      Property OnRenameFile;
   End;


Implementation

Uses
   Forms,
   ztvInflate,
   ztvCrypt,
   Err_Msgs;

Type
   SF_Node = Packed Record
      LChild, RChild: Integer;
   End;

   SF_BuildRec = Packed Record
      Len: Byte;
      Val: Byte;
      code: word;
   End;

   LZW_Table_Rec = Packed Record
      Prefix: Integer;
      Suffix: Byte;
      ChildCount: word;                 (* if ChildCount = 0 then leaf node *)
   End;

   FollowerSet = Packed Record
      SetSize: word;
      FSet: Array[0..31] Of Byte;
   End;

Const
   FIRSTFREE = 257;
   MINCODESIZE = 9;
   MAXCODESIZE = 13;
   MAXDICTSIZE = 8192;                  (* size will be 4096 for unreduce and either  *)
   LENGTH_TREE_ROOT = 127;
   LITERAL_TREE_ROOT = 511;
   DISTANCE_TREE_ROOT = 127;
   MAX_SF_TREE_SIZE = 511;
   LZW_TABLE_SIZE = Pred(1 Shl MAXCODESIZE); (* 0..8191 *)
   LZW_STACK_SIZE = Pred(1 Shl MAXCODESIZE); (* 0..8191 *)

Type
   SF_BuildPtr = ^SF_BuildArray;
   SF_BuildArray = Array[0..High(word) * 2 {255}] Of SF_BuildRec;	// v4.8.5 changed
   DictPtr = ^DictArray;
   DictArray = Array[0..Pred(MAXDICTSIZE)] Of Byte;
   FreeListPtr = ^FreeListArray;
   FreeListArray = Array[FIRSTFREE..LZW_TABLE_SIZE] Of word;
   LZW_Table_Ptr = ^LZW_Table_Type;
   LZW_Table_Type = Array[0..LZW_TABLE_SIZE] Of LZW_Table_Rec;
   StackPtr = ^StackType;
   StackType = Array[0..LZW_STACK_SIZE] Of word;
   FollowerPtr = ^FollowerArray;
   FollowerArray = Array[0..255] Of FollowerSet;

Var
   SF_Literal: ^Integer;                //^Word
   SF_Distance: ^Integer;               //^Word
   SF_Length: ^Integer;                 //^Word
   NextFreeLiteral: word;               (* Free node pointers used while trees     *)
   NextFreeDistance: word;
   NextFreeLength: word;                (* are being constructed                   *)
   NumOfTrees: Byte;                    (* the # of SF trees needed ( 2 or 3 )  *)
   MinMatchLen: Byte;                   (* minimum dictionary match length ( 2 or 3 )*)
   SF_Build_Idx: Integer;               //Word
   FirstCh: Boolean = True;             (* Flag indicating first char being processed *)
   state: Byte;
   dictionary: DictPtr;                 (* array[ 0..MAXDICTSIZE - 1 ] of byte 		*)
   DictSize: word;                      (* size ( in bytes ) of sliding dictionary	*)
   DictIdx: word;                       (* Always points to next pos. To be filled *)
   ExtCount: Int64; //u_long;                    (* Count of characters written to output 	*)
   NextFree: word;                      (* Index for free list array      *)
   StackIdx: word;                      (* Stack array index variable            *)
   SF_Build: SF_BuildPtr;               (* Array used in building the     *)
   LZW_Table: LZW_Table_Ptr;            (* Code table for LZW decoding    *)
   FreeList: FreeListPtr;               (* List of free table entries     *)
   LZW_Stack: StackPtr;                 (* A stack used to build decoded strings   *)
   Followers: FollowerPtr;
   v: Byte;                             (* Static Global *)
   gLen: word;                          (* Static Global *)

   SaveByte: Byte;                      (* Our input code buffer - 1 Byte long 		*)
   BitsLeft: Byte;                      (* Unprocessed bits in the input code buffer *)

   (* global vars which are also globals in ztvInflate.pas *)
   FileProgressPos: Int64;
   WP: {Integer; //}Cardinal;  //v4.6.8 Integer;     (* Static Global *)
   Slide: Array[0..WSIZE] Of Byte;      //BufPtr;
   inbuf: Array[0..WSIZE] Of Byte;      //BufPtr;
   inptr: Integer;                      (* Index for ZipFile input buffer 			*)
   ZipCount: Integer;                   (* Count of bytes in ZipFile input buffer 	*)
   EOF: Boolean;

   TmpInt: Integer;                     //v4.0


//-------------------------------------------------------------

Function Get_Compressed(UnBase: TUnBASE; inFile: TStream32): Integer;
Var
	MemSize: Integer;
Begin
   With UnBase Do
   Begin

      If Cancel Then
      Begin
         Bytes_To_Go := 0;
         ZipCount := 0;
         EOF := True;
         Result := -1;
         Exit;
      End;

      Try
         If Bytes_To_Go <= 0 Then
         Begin
            ZipCount := 0;
            Result := -1;
         End
         Else
         Begin
            If inptr >= ZipCount Then
            Begin

               // use the following block, instead of the min function...
               // the min function fails with files > 4 gig.
               //BufSize := Min(Bytes_To_Go, WSIZE);
               If Bytes_To_Go > WSIZE Then
                  MemSize := WSIZE
               Else
                  MemSize := Bytes_To_Go;

               With UnBase Do
                  ZipCount :=
                  	ReadBlock(inFile, Nil, inbuf, (InflateRec.BitFlag And 1) = 1,
                     	0, MemSize, dtData);

               inptr := 0;
            End;

            If ZipCount = 0 Then
            Begin
               //Bytes_To_Go := 0;
               inptr := 0;
               Result := -1;
            End
            Else
            Begin
               Result := inbuf[inptr];
               Inc(inptr);
               Dec(Bytes_To_Go);
            End;
         End;
      Finally
         EOF := ZipCount = 0;
      End;
   End;
End;
//-------------------------------------------------------------

Function GetCode(UnBase: TUnBASE; inFile: TStream32; codesize: Byte): Integer;
Const
   Mask: Array[1..8] Of Byte = ($01, $03, $07, $0F, $1F, $3F, $7F, $FF);
Var
   BitsNeeded: Byte;
   HowMany: Byte;
   HoldCode: Integer;
Label
   ExitIT;
Begin
   BitsNeeded := 0;
   HoldCode := 0;

   If FirstCh Then
   Begin
      FirstCh := False;
      TmpInt := Get_Compressed(UnBase, inFile);
      If TmpInt = -1 Then
         Goto ExitIT;

      SaveByte := TmpInt;
      BitsLeft := 8;                    (* there's now 8 bits in the buffer  *)
   End;

   BitsNeeded := codesize;
   HoldCode := 0;

   While (BitsNeeded > 0) And {( NOT EOF )}(TmpInt <> -1) Do
   Begin
      If BitsNeeded >= BitsLeft Then
         HowMany := BitsLeft            (* HowMany <-- Min( BitsLeft, BitsNeeded ) *)
      Else
         HowMany := BitsNeeded;

      HoldCode := HoldCode Or ((SaveByte And Mask[HowMany]) Shl (codesize - BitsNeeded));
      SaveByte := SaveByte Shr HowMany;
      Dec(BitsNeeded, HowMany);
      Dec(BitsLeft, HowMany);

      If BitsLeft <= 0 Then             (* if no bits left in buffer ...     *)
      Begin
         TmpInt := Get_Compressed(UnBase, inFile);
         If TmpInt = -1 Then
            Goto ExitIT;

         SaveByte := TmpInt;
         BitsLeft := 8;
      End;
   End;

   ExitIT:
   If (BitsNeeded = 0) Then             (* if we got what we came for ... *)
      Result := HoldCode                (* ... then return it             *)
   Else
      Result := -1;                     (* ... Otherwise, return eof      *)
End;
//-------------------------------------------------------------

Procedure DictionaryInit;
Begin
   state := 0;
   FillChar(dictionary^, DictSize, $00);
   DictIdx := 0;
End;
//-------------------------------------------------------------

Procedure Write_Out_Block(Sender: TUnBASE; Var outFile: TStream32);
Begin
   With Sender Do
      If WP > 0 Then
      Begin
         If ExtractWriteBlock(outFile, Slide, False, 32, WP, dtData) = 0 Then
            RaiseError(E_RAISE, FileName, '', '0', E_FWRITE);

         ProgressPosition := ProgressPosition - WP;
         FileProgressPos := FileProgressPos - WP;

         doBranchProgress(InflateRec.UnpackedSize - FileProgressPos,
            InflateRec.UnpackedSize, fTotalUnpackedSize);

         WP := 0;
      End;
End;
//-------------------------------------------------------------

Procedure put_ext(UnBase: TUnBASE; Var outFile: TStream32; C: Byte);
Begin
   Slide[WP] := C;
   Inc(WP);
   ExtCount := ExtCount + 1;

   If WP = WSIZE Then
      Write_Out_Block(UnBase, outFile);
End;
//-------------------------------------------------------------

Procedure UpdateDictionary(UnBase: TUnBASE; Var outFile: TStream32; C: Byte);
Begin
   put_ext(UnBase, outFile, C);
   dictionary^[DictIdx] := C;
   DictIdx := SUCC(DictIdx) Mod DictSize;
End;
//-------------------------------------------------------------

Procedure Explode(UnBase: TUnBASE; inFile: TStream32; Var outFile: TStream32);

   Function Decode_SF_Data(UnBase: TUnBASE; inFile: TStream32; Var SF_Tree;
      SF_Root: word): Byte;
   (* Read bits from the input file and decode them using one of the 3 possible   *)
   (* Shannon-Fano trees.  The method is idential to that used in decoding files  *)
   (* encoded with the Huffman method ( popularaly known as "squeezing" ) in that *)
   (* the tree is traced from the root to either the right or left depending on   *)
   (* the last bit read until finally, one encounteres a leaf node.               *)
   Var
      SF_Array: Array[0..MAX_SF_TREE_SIZE] Of SF_Node Absolute SF_Tree;
      OneBit: Integer;
      CurrNode: word;
      LastLeaf: word;
   Begin
      CurrNode := SF_Root;              (* We start traversing the tree from it's root node    *)
      LastLeaf := Pred(SUCC(SF_Root) Div 2);

      While CurrNode > LastLeaf Do
      Begin

         (* Walk the tree until you hit a leaf node *)
         OneBit := GetCode(UnBase, inFile, 1);
         If OneBit = -1 Then
         //IF EOF THEN
         Begin
            Result := Byte(False);
            Exit;
         End;

         If Boolean(OneBit And $01) Then (* if the bit is a 1 ... *)
         Begin
            If SF_Array[CurrNode].RChild = -1 Then
               UnBase.RaiseError(E_RAISE, UnBase.FileName, '', '0', E_SHANNONFANO)
            Else
               CurrNode := SF_Array[CurrNode].RChild;
         End
         Else
         Begin
            If SF_Array[CurrNode].LChild = -1 Then
               UnBase.RaiseError(E_RAISE, UnBase.FileName, '', '0', E_SHANNONFANO)
            Else
               CurrNode := SF_Array[CurrNode].LChild;
         End;
      End;
      Result := CurrNode;
   End;

   Procedure Sort_SF_Build_Array(Count: word);
   (***** BUBBLE SORT **************************************************)
   (*  The list is scanned repeatedly, and adjacent items that are out of
       order are swapped.  When a pass occurs with no swaps, the list is
       sorted.  *)

      Function ShouldSwap(p1, p2: SF_BuildRec): Boolean;
      Begin
         Result := (p1.Len > p2.Len) Or ((p1.Len = p2.Len) And (p1.Val > p2.Val));
      End;

      Procedure Sort(lb, ub: Integer);

         Procedure Exchange(Var Node1, Node2: SF_BuildRec);
         Var
            Node3: SF_BuildRec;
         Begin
            Node3.Len := Node1.Len;
            Node3.Val := Node1.Val;
            // v3.5 added
            Node3.code := Node1.code;   (* the Code field is irrelevant at this point *)

            Node1.Len := Node2.Len;
            Node1.Val := Node2.Val;
            // v3.5 added
            Node1.code := Node2.code;   (* ditto *)

            Node2.Len := Node3.Len;
            Node2.Val := Node3.Val;
            // v3.5 added
            Node2.code := Node3.code;   (* ditto again *)
         End;
      Var
         cell: Integer;
         swapped: Boolean;
      Begin
         Repeat
            swapped := False;
            For cell := lb To Pred(ub) Do
            Begin
               If ShouldSwap(SF_Build^[cell], SF_Build^[SUCC(cell)]) Then
               Begin
                  Exchange(SF_Build^[cell], SF_Build^[SUCC(cell)]);
                  swapped := True;
               End;
            End;
         Until (swapped = False);
      End;
   Begin
      Sort(0, Count);
   End;

   Function Add_SF_SubTree(UnBase: TUnBASE;
      Var SF_Tree;
      Var SF_NextFree: word;
      SF_Root: word;
      SF_Code: word;
      SF_Code_Length: Byte;
      SF_Value: Byte): Boolean;

   (* Add the subtree defined by SF_Code to a Shannon-Fano tree *)
   Var
      SF_Array: Array[0..MAX_SF_TREE_SIZE] Of SF_Node Absolute SF_Tree;
      CurrNode: word;
      LastLeaf: word;
      i: Byte;
   Begin
      Result := False;

      (* The Shannon-Fano tree is implemented as an array of records. Each        	*)
      (* record contains both left and right pointers ( ie. this is a binary       	*)
      (* tree ).  The root of the tree is the last array element. The first N      	*)
      (* elements ( 0..N-1 ) are defined to be the "leaves" of the tree ( ie. they  *)
      (* represent the characters that the decode algorithm will generate ).  N    	*)
      (* may be 64 ( for the length tree ), 128 ( for the distance tree ), or 256   *)
      (* ( for the Literal tree ). The remaining elements OF the array are used to  *)
      (* represent the non-leaf and non-root nodes of the tree.                   	*)
      CurrNode := SF_Root;
      LastLeaf := Pred(SUCC(SF_Root) Div 2);

      (* All bits in the code except the least significant define non-leaf nodes  *)
      (* in the tree.  Process these first.                                       *)
      For i := Pred(SF_Code_Length) Downto 1 Do
      Begin
         If CurrNode <= LastLeaf Then
         Begin
            UnBase.RaiseErrorStr(UnBase.FileName, '', '0', E_SHANNONFANO);
            Exit;
         End;

         If Boolean((SF_Code Shr i) And $0001) Then (* if the bit is a 1  *)
         Begin
            If SF_Array[CurrNode].RChild = -1 Then (* no RChild yet      *)
            Begin
               SF_Array[CurrNode].RChild := SF_NextFree;
               Dec(SF_NextFree);
            End;
            CurrNode := SF_Array[CurrNode].RChild; (* on 1 bits, follow the *)
            (* right subtree         *)
         End
         Else
         Begin                          (* the bit is a 0     *)
            If SF_Array[CurrNode].LChild = -1 Then (* no LChild yet      *)
            Begin
               SF_Array[CurrNode].LChild := SF_NextFree;
               Dec(SF_NextFree);
            End;
            CurrNode := SF_Array[CurrNode].LChild; (* on 0 bits, follow the *)
            (* left subtree          *)
         End;
      End;

      (* All that's left now is to process the least significant bit of the code. *)
      (* This will define a leaf node.  The leaf node to be linked is defined by  *)
      (* the SF_Value that is passed to the procedure.                            *)
      If Boolean(SF_Code And $0001) Then
      Begin
         If SF_Array[CurrNode].RChild <> -1 Then
            UnBase.RaiseError(E_RAISE, UnBase.FileName, '', '0', E_SHANNONFANO)
         Else
            SF_Array[CurrNode].RChild := SF_Value
      End
      Else
         If SF_Array[CurrNode].LChild <> -1 Then
            UnBase.RaiseError(E_RAISE, UnBase.FileName, '', '0', E_SHANNONFANO)
         Else
            SF_Array[CurrNode].LChild := SF_Value;

      Result := True;
   End;

   Function Build_SF_Trees(UnBase: TUnBASE; inFile: TStream32): Boolean;
   (* Extract SF data from an imploded file and build the required SF trees *)
   Var
      OneByte: Byte;                    (* These "misc" variables are also used in *)
      CodeLen: Byte;                    (* building the SF trees                   *)
      CodeCount: Byte;

      SF_Table_Codes: word;             (* # of bytes representing SF tree data - 1*)
      BuildCount: word;                 (* total entries in SF_Build ARRAY         *)

      code: word;                       (* These three variables used in           *)
      CodeIncrement: word;              (* constructing the Shannon-Fano codes     *)
      LastBitLength: word;              (* that will be used to build the SF trees *)

      WhichTree: word;                  (* Counter indicating which SF tree is     *)
      (* currently under construction          	*)
      SF_Tree: Pointer;
      SF_NextFree: word;
      SF_Root: word;

      i, j: word;                       (* Generic loop counter                    *)
   Begin

      SF_Root := 0;
      SF_Tree := Nil;
      Result := True;

      For WhichTree := 1 To NumOfTrees Do
      Begin

         (* Before we go any further, determine which subtree-add procedure       *)
         (* parameters will be needed on the call to Add_SF_SubTree               *)
         Case NumOfTrees Of

            2:
               Case WhichTree Of

                  1:
                     Begin
                        SF_Tree := SF_Length;
                        SF_NextFree := NextFreeLength;
                        SF_Root := LENGTH_TREE_ROOT;
                     End;

                  2:
                     Begin
                        SF_Tree := SF_Distance;
                        SF_NextFree := NextFreeDistance;
                        SF_Root := DISTANCE_TREE_ROOT;
                     End;

               End;

            3:
               Case WhichTree Of

                  1:
                     Begin
                        SF_Tree := SF_Literal;
                        SF_NextFree := NextFreeLiteral;
                        SF_Root := LITERAL_TREE_ROOT;
                     End;

                  2:
                     Begin
                        SF_Tree := SF_Length;
                        SF_NextFree := NextFreeLength;
                        SF_Root := LENGTH_TREE_ROOT;
                     End;

                  3:
                     Begin
                        SF_Tree := SF_Distance;
                        SF_NextFree := NextFreeDistance;
                        SF_Root := DISTANCE_TREE_ROOT;
                     End;
               End;
         End;

         (* Build the Shannon-Fano tree *)
         SF_Build_Idx := 0;
         //BuildCount     := 0;
         //ztvSetFilePointer( inFile, 2, FILE_CURRENT );
         SF_Table_Codes := GetCode(UnBase, inFile, 8);

         For i := 0 To SF_Table_Codes Do
         Begin

            (* Load the SF_Build array with data from the compressed file *)
            OneByte := GetCode(UnBase, inFile, 8);
            CodeLen := (OneByte And $0F) + 1;
            CodeCount := (OneByte Shr 4);

            For j := 0 To CodeCount Do
            Begin
               SF_Build^[SF_Build_Idx].Len := CodeLen;
               SF_Build^[SF_Build_Idx].Val := SF_Build_Idx;
               Inc(SF_Build_Idx);
            End;

         End;

         BuildCount := Pred(SF_Build_Idx);

         (* Sort the SF_Build array based on the Len field *)
         Sort_SF_Build_Array(BuildCount);

         (* Generate the SF codes that will be used to grow the SF tree using the *)
         (* algorithm outlined in the AppNote.Txt file ( as distributed within the *)
         (* PKZip v1.0 self extracting ZIP archive ).                              *)
         code := 0;
         CodeIncrement := 0;
         LastBitLength := 0;

         For i := BuildCount Downto 0 Do
         Begin
            Inc(code, CodeIncrement);

            If SF_Build^[i].Len <> LastBitLength Then
            Begin
               LastBitLength := SF_Build^[i].Len;
               CodeIncrement := 1 Shl (16 - LastBitLength);
            End;

            SF_Build^[i].code := code Shr (16 - SF_Build^[i].Len);

            (* Now there's a value and a code.  This represents a subtree in      *)
            (* the Shannon-Fano tree structure.  Add it to the appropriate tree.  *)
            If (Not Add_SF_SubTree(UnBase, SF_Tree^, SF_NextFree, SF_Root,
               SF_Build^[i].code, SF_Build^[i].Len,
               SF_Build^[i].Val)) Then
            Begin
               Result := False;
               Exit;
            End;
         End;
      End;
   End;

Var
   OneByte: Byte;
   Literal: Byte;
   Length: word;
   DistVal: word;
   Distance: word;
   DictStart: Integer;
Begin
   If (Not Build_SF_Trees(UnBase, inFile)) Then
      Exit;

   TmpInt := 0;                         //v4.0

   DictionaryInit();
   Repeat
      OneByte := GetCode(UnBase, inFile, 1);
      //IF EOF THEN Break;

      If OneByte <> 0 Then
      Begin

         (* This is literal data ... no dictionary lookup involved *)
         If NumOfTrees = 3 Then
            Literal := Decode_SF_Data(UnBase, inFile, SF_Literal^, LITERAL_TREE_ROOT)
         Else
            Literal := GetCode(UnBase, inFile, 8);

         UpdateDictionary(UnBase, outFile, Literal);

      End
      Else
      Begin

         (* Data for output will come from the sliding dictionary *)
         If DictSize = 8192 Then
         Begin
            Distance := GetCode(UnBase, inFile, 7);
            DistVal := Decode_SF_Data(UnBase, inFile, SF_Distance^, DISTANCE_TREE_ROOT);
            Distance := (Distance Or (DistVal Shl 7)) And $1FFF;
         End
         Else
         Begin
            Distance := GetCode(UnBase, inFile, 6);
            DistVal := Decode_SF_Data(UnBase, inFile, SF_Distance^, DISTANCE_TREE_ROOT);
            Distance := (Distance Or (DistVal Shl 6)) And $0FFF;
         End;

         Length := Decode_SF_Data(UnBase, inFile, SF_Length^, LENGTH_TREE_ROOT);
         If Length = 63 Then
            Length := Length + GetCode(UnBase, inFile, 8);

         Length := Length + MinMatchLen;

         DictStart := DictIdx - (Distance + 1);

         If DictStart < 0 Then
            DictStart := DictStart + DictSize;

         While Length > 0 Do
         Begin
            UpdateDictionary(UnBase, outFile, dictionary^[DictStart]);
            DictStart := SUCC(DictStart) Mod DictSize;
            Dec(Length);
         End;
      End;
   Until (ExtCount >= UnBase.InflateRec.UnpackedSize) Or EOF;
End;
//-------------------------------------------------------------

Procedure UnShrink(UnBase: TUnBASE; inFile: TStream32; Var outFile: TStream32);

   Procedure Add_To_LZW_Table(Prefix: Integer; Suffix: Byte);
   Var
      i: word;
   Begin
      If NextFree <= LZW_TABLE_SIZE Then
      Begin
         i := FreeList^[NextFree];
         Inc(NextFree);
         LZW_Table^[i].Prefix := Prefix;
         LZW_Table^[i].Suffix := Suffix;
         Inc(LZW_Table^[Prefix].ChildCount);
      End;
   End;
   //-------------------------------------------------------------

   Procedure Clear_LZW_Table;
   Var
      i: word;
   Begin
      StackIdx := 0;

      For i := FIRSTFREE To LZW_TABLE_SIZE Do (* Find all leaf nodes *)
      Begin
         If LZW_Table^[i].ChildCount = 0 Then
         Begin
            LZW_Stack^[StackIdx] := i;  (* and put each on stack *)
            Inc(StackIdx);
         End;
      End;

      NextFree := SUCC(LZW_TABLE_SIZE);

      While StackIdx > 0 Do             (* clear all leaf nodes *)
      Begin
         Dec(StackIdx);
         i := LZW_Stack^[StackIdx];

         With LZW_Table^[i] Do
         Begin
            If LZW_Table^[i].Prefix <> -1 Then
               Dec(LZW_Table^[Prefix].ChildCount);

            Prefix := -1;
            Suffix := 0;
            ChildCount := 0;
         End;

         Dec(NextFree);                 (* add cleared nodes to freelist *)
         FreeList^[NextFree] := i;
      End;
   End;
   //-------------------------------------------------------------
Var
   codesize: Byte;                      (* Current size ( in bits ) of codes coming in  *)
   CurrCode: Integer;
   SaveCode: Integer;
   PrevCode: Integer;
   BaseChar: Byte;
Const
   SPECIAL = 256;
Begin

   TmpInt := 0;                         //v4.0
   codesize := MINCODESIZE;             (* Start with the smallest code size *)

   PrevCode := GetCode(UnBase, inFile, codesize);
   If (PrevCode = -1) Then
      UnBase.RaiseError(E_RAISE, UnBase.FileName, '', '0', E_BADBLOCK);

   BaseChar := PrevCode;
   put_ext(UnBase, outFile, BaseChar);  (* Unpack the first character *)

   (* Get next code to prime the while loop *)
   CurrCode := GetCode(UnBase, inFile, codesize);

   While (CurrCode <> -1) Do            (* repeat for all compressed bytes   *)
   Begin

      If CurrCode = SPECIAL Then        (* If we've got a "special" code ... *)
      Begin

         CurrCode := GetCode(UnBase, inFile, codesize);
         Case CurrCode Of
            1: Inc(codesize);           (* ... and if followed by a 1 ...    *)
            (* ... then increase code size       *)
            2: Clear_LZW_Table;         (* ... and if followed by a 2 ...    *)
            (* ... clear leaf nodes in the table *)
         Else                           (* ... if neither 1 or 2, discard    *)
            UnBase.RaiseError(E_RAISE, UnBase.FileName, '', '0', E_BADBLOCK);
         End;

      End
      Else
      Begin                             (* not a "special" code              *)

         SaveCode := CurrCode;          (* Save this code someplace safe...  *)
         If CurrCode > LZW_TABLE_SIZE Then
            UnBase.RaiseError(E_RAISE, UnBase.FileName, '', '0', E_BADBLOCK);

         If (CurrCode >= FIRSTFREE) And (LZW_Table^[CurrCode].Prefix = -1) Then
         Begin
            If StackIdx > LZW_STACK_SIZE Then
            Begin
               Write_Out_Block(UnBase, outFile);
               UnBase.RaiseError(E_RAISE, UnBase.FileName, '', '0', E_STACK);
            End;

            LZW_Stack^[StackIdx] := BaseChar;
            Inc(StackIdx);
            CurrCode := PrevCode;
         End;

         While CurrCode >= FIRSTFREE Do
         Begin
            If StackIdx > LZW_STACK_SIZE Then
            Begin
               Write_Out_Block(UnBase, outFile);
               UnBase.RaiseError(E_RAISE, UnBase.FileName, '', '0', E_STACK);
            End;

            LZW_Stack^[StackIdx] := LZW_Table^[CurrCode].Suffix;
            Inc(StackIdx);
            CurrCode := LZW_Table^[CurrCode].Prefix;
         End;

         BaseChar := LZW_Table^[CurrCode].Suffix; (* Get last character ... *)
         put_ext(UnBase, outFile, BaseChar);

         While (StackIdx > 0) Do
         Begin
            Dec(StackIdx);
            put_ext(UnBase, outFile, LZW_Stack^[StackIdx]);
         End;                           (* ...until there are none left *)

         Add_To_LZW_Table(PrevCode, BaseChar); (* Add new entry to table *)
         PrevCode := SaveCode;

      End;

      CurrCode := GetCode(UnBase, inFile, codesize); (* Get next code from input stream *)
   End;
End;
//-------------------------------------------------------------

Function Init_UnReduce: Boolean;
Begin
   Followers := Nil;
   dictionary := Nil;
   DictSize := 4096;
   New(Followers);
   New(dictionary);
   Result := (Followers <> Nil) And (dictionary <> Nil);
End;
//-------------------------------------------------------------

Procedure UnReduce(UnBase: TUnBASE; inFile: TStream32; Var outFile: TStream32);

   Procedure UnScrnch(C: Byte);

      Function l(x: Byte): Byte;
      Const
         Mask: Array[1..4] Of Byte = ($7F, $3F, $1F, $0F);
      Begin
         Result := x And Mask[Pred(UnBase.InflateRec.CompressType)];
      End;
      //-------------------------------------------------------------

      Function f(x: word): Byte;
      Const
         TestVal: Array[1..4] Of Byte = (127, 63, 31, 15);
      Begin
         If x = TestVal[Pred(UnBase.InflateRec.CompressType)] Then
            Result := 2
         Else
            Result := 3;
      End;
      //-------------------------------------------------------------

      Function d(x, y: Byte): word;
      Var
         tmp: longint;
      Begin
         x := x Shr (8 - Pred(UnBase.InflateRec.CompressType));
         tmp := x * 256;
         Result := tmp + SUCC(y);
      End;
      //-------------------------------------------------------------
   Var
      s: Integer;
      Count: word;
      OneByte: Byte;
      Tmp1: longint;
   Const
      DLE = $90;
   Begin                                (* UnScrnch *)
      Case state Of
         0:
            If C = DLE Then
               state := 1
            Else
               UpdateDictionary(UnBase, outFile, C);
         1:
            If C = 0 Then
            Begin
               UpdateDictionary(UnBase, outFile, DLE);
               state := 0;
            End
            Else
            Begin
               v := C;
               gLen := l(v);
               state := f(gLen);
            End;
         2:
            Begin
               Inc(gLen, longint(C));
               state := 3;
            End;
         3:
            Begin
               Tmp1 := d(v, C);
               s := DictIdx - Tmp1;
               If s < 0 Then
                  Inc(s, DictSize);

               Count := gLen + 3;

               While Count > 0 Do
               Begin
                  OneByte := dictionary^[s];
                  {Result := } UpdateDictionary(UnBase, outFile, OneByte);
                  s := SUCC(s) Mod DictSize;
                  Dec(Count);
               End;
               state := 0;
            End;
      End;
   End;
   //-------------------------------------------------------------

   Function MinBits(Val: Byte): Byte;
   Begin
      Dec(Val);
      Case Val Of
         0..1: Result := 1;
         2..3: Result := 2;
         4..7: Result := 3;
         8..15: Result := 4;
         16..31: Result := 5;
      Else
         Result := 6;
      End;
   End;
   //-------------------------------------------------------------
Var
   n, LastChar: Byte;
   i, j: word;
   code: Integer;
Begin

   TmpInt := 0;                         //v4.0
   For i := 255 Downto 0 Do             (* Load follower sets *)
   Begin
      n := GetCode(UnBase, inFile, 6);  (* Get size of 1st set *)
      Followers^[i].SetSize := n;
      If n > 0 Then
         For j := 0 To Pred(n) Do
            Followers^[i].FSet[j] := GetCode(UnBase, inFile, 8);
   End;

   DictionaryInit();
   LastChar := 0;
   Repeat
      If Followers^[LastChar].SetSize = 0 Then
      Begin
         code := GetCode(UnBase, inFile, 8);
         UnScrnch(code);
         LastChar := code;
      End
      Else
      Begin
         code := GetCode(UnBase, inFile, 1);
         If code <> 0 Then
         Begin
            code := GetCode(UnBase, inFile, 8);
            UnScrnch(code);
            LastChar := code;
         End
         Else
         Begin
            i := MinBits(Followers^[LastChar].SetSize);
            code := GetCode(UnBase, inFile, i);
            UnScrnch(Followers^[LastChar].FSet[code]);
            LastChar := Followers^[LastChar].FSet[code];
         End;
      End;
   Until UnBase.Bytes_To_Go <= 0;
End;
//-------------------------------------------------------------

Function TUnZip.ExtractInflate(Var LocalStream: TStream32; outFile: TStream32;
   MAX_WBITS: smallint): Boolean;
Var
   BufSize: uInt;
   Count: Integer;
   Buffer: Pointer;
   size,
   	FileSize: Int64;
   Stream: TDecompressStream;
Begin
   Stream := TDecompressStream.Create(LocalStream, MAX_WBITS);
   Try
      Stream.FZRec.cb.pCancel := @fCancel;

      If (HeaderTypeState = [htLocal]) Then
         With LocalZipHeader Do
         Begin
            FileSize :=
            	(Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or
               zc.UnpackedSize;
            Stream.FZRec.cb.Protect := zc.BitFlag And PW_PROTECTED > 0;
         End
      Else
         With CentralZipHeader Do
         Begin
            FileSize :=
            	(Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or
               zc.UnpackedSize;
            Stream.FZRec.cb.Protect := zc.BitFlag And PW_PROTECTED > 0;
         End;

      size := FileSize;

      // use the following block, instead of the min function...
      // the min function fails with files > 4 gig.
      //BufSize := Min(size, {4096}WSIZE);
      If size > WSIZE Then
         BufSize := WSIZE
      Else
         BufSize := size;

      GetMem(Buffer, BufSize);
      Try
         While True Do
         Begin
            If (size <= 0) Or Cancel Then break;

            Application.ProcessMessages();
            Try
               Count := Stream.Read(Buffer^, BufSize);
               If Count > 0 Then
               Begin
                  If ExtractWriteBlock(outFile, Buffer^,
                     False, 32, Count, dtData) = 0 Then
                     RaiseError(E_RAISE, FileName, '', '0', E_FWRITE);

                  ProgressPosition := ProgressPosition - Count;
                  FileProgressPos := FileProgressPos - Count;
               End Else Begin
                  If (Count = Z_BUF_ERROR) And
                     (Stream.FZRec.total_out < FileSize) And
                     (fArcType In [atZipDS..atZipMV]) Then
                     //(DiskManager.DriveType = dtFloppy) Then
                  Begin
                     Count := BufSize - Stream.fzrec.avail_out;

                     If (Count > 0) And (ExtractWriteBlock(outFile, Buffer^,
                        False, 32, Count, dtData) = 0) Then
                       RaiseError(E_RAISE, FileName, '', '0', E_FWRITE);

                     // set the beginning offset of the next file.  This
                     // is used to determine the correct diskette is
                     // placed into the drive, instead of a volume
                     // label.
                     pUBFI^.OffsetOfLocalHeader :=
                        ((Int64(Central64Hdr.HiPackedSize) Shl 32) Or
                           LocalZipHeader.zc.UnpackedSize) -
                           Stream.fzrec.Total_in;

                     Case fArcType Of
                     	atZipDS:
                     		If (NOT DiskSpannObj.GetDisk(LocalStream)) Then
                        		Exit;

                        atZipMV:
                        	If (Not GetNextZipVolume(
                                 LocalStream,
                                 fVolNum,
                                 EndZipHeader.NumberOfThisDisk)) Then
                           Begin
                           	Cancel := True;
                              Exit;
                           End;
                     End;

                     size := size - Count;

                     // use the following block, instead of the min function...
                     // the min function fails with files > 4 gig.
                     //BufSize := Min(size, WSIZE{4096});
                     If Size > WSIZE{4096} Then
                        BufSize := WSIZE{4096}
                     Else
                        BufSize := Size;

                     LocalStream.Position := 0;
                     Stream.fStrm := LocalStream;
                     Stream.fStrmPos := 0;

                     ProgressPosition := ProgressPosition - Count;
                     FileProgressPos := FileProgressPos - Count;

                     doBranchProgress(FileSize - FileProgressPos, FileSize,
                        fTotalUnpackedSize);

                     Continue;
                  End Else Begin
                     ProgressPosition := ProgressPosition - size;
                     FileProgressPos := FileProgressPos - size;
                     Bytes_To_Go := 0;
                  End;
               End;

               doBranchProgress(FileSize - FileProgressPos, FileSize,
                  fTotalUnpackedSize);

               If (Count <> -5) And (Count < 1) Then
                 break;

            Except
               On EWriteError Do
               	Raise;
            Else
            End;

            Size := Size - BufSize{Count};

            // use the following block, instead of the min function...
            // the min function fails with files > 4 gig.
            //BufSize := Min(size, WSIZE{4096});
            If Size > WSIZE{4096} Then
               BufSize := WSIZE
            Else
               BufSize := Size;

         End;

      Finally
         FreeMem(Buffer);
      End;
   Finally
   	Stream.Free();
   End;
End;
//-------------------------------------------------------------

Function TUnZip.ExtractInflate32(Var LocalStream: TStream32; outFile: TStream32;
   MAX_WBITS: smallint): Boolean;
Begin
	is64Bit := False;
   {$IFDEF PKZIP_BUG_WORKAROUND}
   MAXDISTS := 32;
   {$ELSE}
   MAXDISTS := 30;
   {$ENDIF PKZIP_BUG_WORKAROUND}
	Result := ExtractInflate(LocalStream, outFile, MAX_WBITS);
End;
//-------------------------------------------------------------

Function TUnZip.ExtractInflate64(Var LocalStream: TStream32; outFile: TStream32;
   MAX_WBITS: smallint): Boolean;
Begin
	is64Bit := True;
	MAXDISTS := 32;
	Result := ExtractInflate(LocalStream, outFile, MAX_WBITS);
End;
//-------------------------------------------------------------
(* The file position is set prior to entering this unit. *)

Function TUnZip.ExtractFile(UnBase: TUnBASE; FName: String; Var inStream:
	TStream32; outFile: TStream32; IRec: TInflateRec; Index: Integer): Boolean;
 //----------------------------------------------------------

   Function Init_Explode: Boolean;
   Var
      Failure: Boolean;
   Begin
      Failure := False;

      SF_Length := Nil;
      SF_Distance := Nil;
      SF_Literal := Nil;
      dictionary := Nil;
      SF_Build := Nil;

      (* Allocate memory for the Length & Distance Shannon-Fano trees             *)
      GetMem(SF_Length, (LENGTH_TREE_ROOT + 1) * SizeOf(SF_Node));
      GetMem(SF_Distance, (DISTANCE_TREE_ROOT + 1) * SizeOf(SF_Node));
      If (SF_Length = Nil) Or (SF_Distance = Nil) Then
         Failure := True;

      (* Extract pertinent info from the general purpose bit flag                 *)
      DictSize := (((UnBase.InflateRec.BitFlag Shr 1) And $01) * 4096) + 4096;
      NumOfTrees := ((UnBase.InflateRec.BitFlag Shr 2) And $01) + 2;

      MinMatchLen := NumOfTrees;

      (* Initialize Length & Distance nodes to all -1's and set the Next Free     *)
      (* Node pointers for each                                                   *)
      FillChar(SF_Length^, (LENGTH_TREE_ROOT + 1) * SizeOf(SF_Node), $FF);
      NextFreeLength := (LENGTH_TREE_ROOT - 1);
      FillChar(SF_Distance^, (DISTANCE_TREE_ROOT + 1) * SizeOf(SF_Node), $FF);
      NextFreeDistance := (DISTANCE_TREE_ROOT - 1);

      (* If we need a literal tree, then allocate the memory , initialize the     *)
      (* nodes to all -1's, and set the Next Free Node pointer                    *)
      If NumOfTrees = 3 Then
      Begin
         GetMem(SF_Literal, (LITERAL_TREE_ROOT + 1) * SizeOf(SF_Node));
         If SF_Literal = Nil Then Failure := True;

         FillChar(SF_Literal^, (LITERAL_TREE_ROOT + 1) * SizeOf(SF_Node), $FF);
         NextFreeLiteral := (LITERAL_TREE_ROOT - 1);
      End;

      New(dictionary);
      If dictionary = Nil Then Failure := True;

      New(SF_Build);
      If SF_Build = Nil Then Failure := True;

      Result := Not Failure;
   End;
   //----------------------------------------------------------

   Function LZW_Init: Boolean;
   Var
      i: word;
      Failure: Boolean;
   Begin

      Failure := False;
      FreeList := Nil;
      LZW_Stack := Nil;

      (* Initialize LZW Table *)
      New(LZW_Table);                   //LZW_Table := AllocMem( SizeOf( LZW_Table^ ) );
      For i := 0 To LZW_TABLE_SIZE Do
      Begin
         With LZW_Table^[i] Do
         Begin
            Prefix := -1;
            If i < 256 Then
               Suffix := i
            Else
               Suffix := 0;
            ChildCount := 0;
         End;
      End;

      New(FreeList);
      If FreeList = Nil Then
         Failure := True;

      (* Initialize the LZW Character Stack *)
      For i := FIRSTFREE To LZW_TABLE_SIZE Do
         FreeList^[i] := i;

      NextFree := FIRSTFREE;

      New(LZW_Stack);
      If LZW_Stack = Nil Then
         Failure := True;

      StackIdx := 0;
      Result := Not Failure;
   End;
   //----------------------------------------------------------

Begin                                   {ExtractFile}
   Result := True;
   EOF := False;

   With UnBase Do
   Try
      Try
         WP := 0;
         inptr := 0;
         ZipCount := 0;
         ExtCount := 0;
         FirstCh := True;
         InflateRec := IRec;
         Crc32Val := CRC_MASK;

         (* remove the SizeOf extra encryption header from PackedSize *)
         If (InflateRec.BitFlag And 1) > 0 Then
            Dec(InflateRec.PackedSize, RAND_HEAD_LEN);

         //If InflateRec.PackedSize = InflateRec.UnpackedSize Then
         //	InflateRec.CompressType := 0;

         Bytes_To_Go := InflateRec.PackedSize;
         FileProgressPos := InflateRec.UnpackedSize;

         Case InflateRec.CompressType Of
            0:                          (* None / stored *)
               Begin
                  Bytes_To_Go := Unstore(inStream, outFile, 32, '0', InflateRec);
                  If (fArcType In [atZipDS..atZipMV]) Then
                     While (Bytes_To_Go > 0) Do
                     Begin

                        UnBase.pUBFI^.OffsetOfLocalHeader := Bytes_To_Go;
                        Case UnBase.ArcType Of
                        	atZipDS: Result := DiskSpannObj.GetDisk(inStream);
                           atZipMV: Result := GetNextZipVolume(
                           	inStream, fVolNum, EndZipHeader.NumberOfThisDisk);

                        End;

                        If Not Result Then Exit;
                        InflateRec.UnpackedSize := Bytes_To_Go;
                        Bytes_To_Go := Unstore(inStream, outFile, 32, '0', InflateRec);
                     End;

               End;

            1:
               Begin                    (* Shrunk *)
                  Try
                     If LZW_Init Then
                        UnShrink(UnBase, inStream, outFile);
                  Finally
                     If LZW_Table <> Nil Then dispose(LZW_Table);
                     If FreeList <> Nil Then dispose(FreeList);
                     If LZW_Stack <> Nil Then dispose(LZW_Stack);
                  End;
               End;
            2, 3, 4, 5:                 (* Unreduce *)
               Begin
                  Try
                     If Init_UnReduce Then
                        UnReduce(UnBase, inStream, outFile);
                  Finally
                     If Followers <> Nil Then dispose(Followers);
                     If dictionary <> Nil Then dispose(dictionary);
                  End;
               End;
            6:
               Begin                    (* Imploded *)
                  Try
                     If Init_Explode Then
                        Explode(UnBase, inStream, outFile);
                  Finally
                     If SF_Length <> Nil Then
                        FreeMem(SF_Length, SUCC(LENGTH_TREE_ROOT) *
                           SizeOf(SF_Node));

                     If SF_Distance <> Nil Then
                        FreeMem(SF_Distance, SUCC(DISTANCE_TREE_ROOT) *
                           SizeOf(SF_Node));

                     If SF_Literal <> Nil Then
                        FreeMem(SF_Literal, SUCC(LITERAL_TREE_ROOT) *
                           SizeOf(SF_Node));

                     If dictionary <> Nil Then
                        dispose(dictionary);

                     If SF_Build <> Nil Then
                        dispose(SF_Build);
                  End;
               End;

            8: ExtractInflate32(inStream, outFile, MaxBits);
            9: ExtractInflate64(inStream, outFile, MaxBits);

         Else
            Begin
               RaiseErrorStr(FName, '', '0', E_UNKNMETH);
               Exit
            End;
         End;

      Except
         //ON E: E_RAISE DO ShowMessage( E.Message );
      End;
   Finally
      Write_Out_Block(UnBase, outFile);
   End;
End;
//-------------------------------------------------------------

Constructor TUnZip.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   DiskSpannObj := TDiskSpannObj.Create();
End;
//-------------------------------------------------------------

Destructor TUnZip.Destroy;
Begin
   DiskSpannObj.Free();
   Inherited Destroy;
End;
//-------------------------------------------------------------

Function TUnZip.GetNextZipVolume(Var LocalStream: TStream32; VolNum,
	LastVolumeInSet: Integer): Boolean;
Begin
   If Assigned(OnNextVolume) Then
   Begin
      LocalStream.Free();
      LocalStream := Nil;

      fVolumeName := GetNextVolumeName(VolNum, LastVolumeInSet);

      // if fArchiveFile was a specific volume (ie test.z01), the first defined
      // fVolumeName is going to be test.z01.
      If HeaderTypeState = [htLocal] Then
         If CompareText(fVolumeName, fArchiveFile) = 0 Then
         Begin
            VolNum := VolNum + 1;
            fVolumeName := GetNextVolumeName(VolNum, 9999);
         End;


      Repeat
         OnNextVolume(Self, fVolumeName, IntToStr(VolNum + 1),
            ztvFileExists(fVolumeName),  fCancel);

         Result := (Not fCancel) And ztvFileExists(fVolumeName);
      Until Result Or fCancel;

      If Result Then
      Begin
         fVolNum := VolNum + 1;

         LocalStream :=
            TFileStream32.Create(fVolumeName, fmOpenRead Or fmShareDenyWrite);
      End Else
         If fCancel Then
            RaiseErrorStr(fVolumeName, '', IntToStr(VolNum + 1), E_USERCANCEL)
         Else
            RaiseErrorStr(fVolumeName, '', IntToStr(VolNum + 1), E_FOPEN);

   End Else Begin
      Result := False;
      Cancel := True;
      RaiseErrorStr(fVolumeName, 'OnNextVolume', IntToStr(VolNum + 1), E_REQUIREDEVENT);
   End;
End;
//-------------------------------------------------------------

Function TUnZip.OpenAndExtractFile(Var LocalStream: TStream32; outFile:
	TStream32; FileAttr, Index: Integer): Boolean;
Begin
   Result := False;

   If doOnBegin((FileAttr And FILE_ATTRIBUTE_DIRECTORY > 0)) Then
   Begin

      // v4.7.2 moved the following RequestPassword block inside doOnBegin

      (* To keep search (TZipSearch & TTurboSearch components) and Validity
         checks (TZipCheck component) speed at a maximum while attempting to
         not bypass files, the variable "PasswordAttemptFailed" is used.  If
         a password attempt failed, bypass all remaining protected files in
         the archive but continue with non-protected files. *)
      If (FileAttr And FILE_ATTRIBUTE_DIRECTORY = 0) Then
      Begin
         fEncrypted := (InflateRec.BitFlag And PW_PROTECTED > 0);
         If fEncrypted And (Not PasswordAttemptFailed) Then
            If Not RequestPassword(LocalStream) Then
               PasswordAttemptFailed := True;
      End Else
         fEncrypted := False;


      Try

         (* JavaSoft jar doesn't store the files attribute *)
         If (ArcType = atJar) Or (ArcType = atJarExe) Then
            If ExtractFilename(FileName) = '' Then
               FileAttr := FILE_ATTRIBUTE_DIRECTORY
            Else
               FileAttr := 32;

         Try
            If (FileAttr And FILE_ATTRIBUTE_DIRECTORY > 0) Then
            Begin
               If WriteToFile() And CreateStoredDirs Then
                  If Not CreateDirEx(fFileName) Then
                  Begin
                     Result := False;
                     Exit;
                  End;

               Result := True;
               Crc32Val := LocalZipHeader.ZC.CRC32;
            End
            Else
            Begin

               (* If PasswordAttemptFailed = true, the doOnBegin event
                  returns true without actually opening the output file *)
               If Open_OutFile(Outfile, FileName, ActualFilename) Then
               Begin

                  With LocalZipHeader Do
                  Try
                     If (fEncrypted And PasswordAttemptFailed) Then
                     Begin
                        Dec(Count);
                        AdjustProgress(InflateRec);
                     End
                     Else
                     Begin
                        (* archives created using generic utilities sometimes *)
                        (* skip values in the local header... retrieve these  *)
                        (* values from the CentralZipHeader.                  *)
                        (* ...see zip\bug\alyssa130.zip                       *)
                        If (CentralZipHeader.ZC.PackedSize > 0) And (zc.CRC32 = 0) Then
                           zc.CRC32 := CentralZipHeader.ZC.CRC32;

                        ExtractFile(
                           Self, FileName, LocalStream, outFile, InflateRec, Index);

                        Crc32Val := Crc32Val Xor CRC_MASK;
                     End;

                  Finally

                     If (fEncrypted And PasswordAttemptFailed) Then
                        fFileName := ActualFilename
                     Else
                        CloseAndSetDate(outFile, FileName, zc.FileDate, FileAttr);

                  End;

               End
               Else
               Begin
                  RaiseErrorStr(fFileName, '', '0', E_FOPEN);
                  AdjustProgress(InflateRec);
                  Dec(Count);
               End;
            End;
         Finally
         	Result := doOnEnd(32, LocalZipHeader.ZC.CRC32);
         End;

      Except
         //On e: exception Do ShowMessage(e.message);
      End;
   End
   Else
      AdjustProgress(InflateRec);

End;
//-------------------------------------------------------------

Function TUnZip.VerifyPassword(Buffer: PChar): Boolean;
Begin
   With LocalZipHeader Do
      Result :=
      	decrypt_pw(Buffer, zc.BitFlag, zc.CRC32, zc.FileDate, fPassword);
End;
//-------------------------------------------------------------

Procedure TUnZip.RecoveryProcessHeaders(Var LocalStream: TStream32; outFile: TStream32);
Var
   Encrypted: Boolean;
   pFilename: PChar;
   i, BytesRead: Integer;
Begin
   GetMem(pFilename, 256);
   Try
      ProgressPosition := fTotalUnpackedSize;

      For i := 0 To HeaderList.FileCount - 1 Do
      Begin
         If Cancel Then break;

         pUBFI := HeaderList.FileLocationData(i);
         With pUBFI^ Do
         Begin

            (* Get the correct disk containing the requested file *)
            //If (fArcType In [atZipDS..atZipMV])  And (i = 0) Then
            If (fArcType = atZipDS) And (i = 0) Then
            Begin
               DiskSpannObj.VolNum := DiskWithThisFile;
               If Not DiskSpannObj.GetDisk(LocalStream) Then
                  Exit;
            End;

            LocalStream.Position := OffsetOfLocalHeader;

            (* Read LocalZipHeader & Filename *)
            BytesRead := LocalStream.Read(LocalZipHeader, SizeOf(TLocal));
            If BytesRead <> 0 Then ;

            With LocalZipHeader Do
            Begin
               VerSig(SignAtr, htLocal, Encrypted);
               If Encrypted Then
                  DecodeHeader(@LocalZipHeader, htLocal);

               ReadFilename(LocalStream, pFilename, ZC.FileNameLen);

               If Encrypted Then
                  DecodeFilename(pFilename, ZC.FileNameLen);

               //LocalStream.Seek(ZC.ExtraFieldLen, soCurrent);
   				Central64Hdr :=
               	Read64BitFieldHdr(
                  	LocalStream,
                     zc.ExtraFieldLen,
                     htLocal);

               InflateRec.BitFlag := ZC.BitFlag;
               //InflateRec.CompressType := ZC.CompressType;
               //InflateRec.UnpackedSize := ZC.UnpackedSize;

               InflateRec.PackedSize :=
               	(Int64(Central64Hdr.HiPackedSize) Shl 32) Or
                  zc.PackedSize;

               InflateRec.UnpackedSize :=
               	(Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or
                  zc.UnpackedSize;


               // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
               GlobalDate := ZC.FileDate;
            End;

            ActualFilename := OemToCharFilter(StrPas(pFilename), fTransOemChar);
            FileName := ActualFilename;

            ZipCompatible := IsZipCompatible(LocalZipHeader.SignAtr);
            OpenAndExtractFile(LocalStream, outFile, FileAttr, i)
         End;
      End;
   Finally
      FreeMem(pFilename, 256);
   End;
End;
//-------------------------------------------------------------

Function TUnZip.BuildHeadArray(Var LocalStream: TStream32): Integer;
Var
   i: Integer;
   pFilename: PChar;
   CurrentPos: Int64;
   UBFI: TUnBaseFileInfo;               //HeaderData
   ByPass,
   	Encrypted: Boolean;
Begin

   ZipTimer.Suspend();
   GetMem(pFilename, 256);

   With EndZipHeader Do
   Try

      (* Now build the array of CentralDirHeaders of files matching FileSpec *)
      CurrentPos := fOffsetStart; //CentralDirOffset + ZipSFX_OffsetAdjustment;
      LocalStream.Position := CurrentPos;

      For i := DiskWithStartOfCentral To NumberOfThisDisk Do
      Begin

         ZeroMemory(@CentralZipHeader, SizeOf(TCentral));
         LocalStream.Read(CentralZipHeader, SizeOf(TCentral));

         With CentralZipHeader Do
         Begin

            While VerSig(SignAtr, htCentral, Encrypted) = htCentral Do
            Begin
               If Cancel Then break;

               If Encrypted Then
                  DecodeHeader(@CentralZipHeader, htCentral);

               ReadFilename(LocalStream, pFilename, ZC.FileNameLen);

               If Encrypted Then
                  DecodeFilename(pFilename, zc.FileNameLen);

            	Central64Hdr :=
               	Read64BitFieldHdr(
                  	LocalStream,
                     zc.ExtraFieldLen,
                     htCentral);

               ActualFilename := OemToCharFilter(StrPas(pFilename), fTransOemChar);
               If CheckWildCard2(ActualFilename, FileSpec, ExcludeSpec, RecurseDirs) Then
               Begin

                  With UBFI Do
                  Begin
                     DiskWithThisFile := DiskNumberStart + 1;
                     //If Is64BitEndingHdr(EndZipHeader.SignAtr) Then
                     	OffsetOfLocalHeader :=
                        	((Int64(Central64Hdr.HiOffsetToLocal) Shl 32) Or
                        	RelativeOffsetOfLocalHeader) + ZipSFX_OffsetAdjustment;
                     //Else
                     //	OffsetOfLocalHeader :=
                     //		RelativeOffsetOfLocalHeader + ZipSFX_OffsetAdjustment;

                     FileAttr := ExternalAttr;
                  End;

                  ByPass := False;
                  If Not (fArcType In [atZipDS..atZipMV]) Then
                  Begin
                     LocalStream.Position :=
                        ((Int64(Central64Hdr.HiOffsetToLocal) Shl 32) Or
                        RelativeOffsetOfLocalHeader) + ZipSFX_OffsetAdjustment;

                  	ZeroMemory(@LocalZipHeader, SizeOf(TLocal));
                     LocalStream.Read(LocalZipHeader, SizeOf(TLocal));

                     ByPass := Not (VerSig(LocalZipHeader.SignAtr, htLocal,
                        Encrypted) = htLocal);

                     // test file: \3\zip\SM12-MID.ZIP
                     If ByPass Then
               			RaiseErrorStr(ActualFilename, '', '0', E_BADHEADR)
                     Else Begin
                     	// following two lines used for testing of Central64Hdr
                        // stored in the local header
                        //LocalStream.Seek(LocalZipHeader.zc.FileNameLen, soCurrent);
                        //Central64Hdr := Read64BitFieldHdr(LocalStream, zc.ExtraFieldLen);
                     End;

                     //LocalStream.Position := CurrentPos;
                  End;

                  If (Not ByPass) Then
                  Begin
                     HeaderList.AddItem(UBFI, @CentralZipHeader, SizeOf(TCentral));

                     If zc.FileDate > fMaxAge Then
                        fMaxAge := zc.FileDate;

            			fTotalPackedSize :=
                        (Int64(Central64Hdr.HiPackedSize) Shl 32) Or
                        zc.PackedSize +
								fTotalPackedSize;

            			fTotalUnpackedSize :=
                        (Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or
                        zc.UnpackedSize +
								fTotalUnpackedSize;

                     If zc.BitFlag And 1 > 0 Then
                        fTotalPackedSize := fTotalPackedSize - RAND_HEAD_LEN;
                  End;

               End;

               If Cancel Then break;

               Inc(CurrentPos, SizeOf(TCentral) + zc.FileNameLen +
                  CommentLen + zc.ExtraFieldLen);

               LocalStream.Position := CurrentPos;

               ZeroMemory(@CentralZipHeader, SizeOf(TCentral));
               {BytesRead :=} LocalStream.Read(CentralZipHeader, SizeOf(TCentral));
            End;
         End;
      End;

   Finally
      Result := HeaderList.FileCount;
      FreeMem(pFilename, 256);
      ZipTimer.Resume();
   End;
End;
//-------------------------------------------------------------

Procedure TUnZip.ProcessHeaders(Var LocalStream: TStream32; outFile: TStream32);
Var
   i, BytesRead: Integer;
   pFilename: PChar;
   Encrypted: Boolean;
Begin
   GetMem(pFilename, 256);
   Try
      ProgressPosition := fTotalUnpackedSize;

      For i := 0 To HeaderList.FileCount - 1 Do
      Begin
         If Cancel Then break;

         pUBFI := HeaderList.FileLocationData(i);
         With pUBFI^ Do
         Begin

            (* Get the correct disk containing the requested file *)
            Case fArcType Of
            	atZipDS:
                  If (i = 0) Then
                  Begin
                     DiskSpannObj.VolNum := DiskWithThisFile;
                     If Not DiskSpannObj.GetDisk(LocalStream) Then
                        Exit;
                  End;

               atZipMV:
                  If (fVolNum <> DiskWithThisFile) Then
                  	// note: do not change DiskWithThisFile - 1 for
                     // CentralZipHeader.DiskNumberStart in the following line!!
                     If (Not GetNextZipVolume(LocalStream, DiskWithThisFile - 1,
                           EndZipHeader.NumberOfThisDisk)) Then
                        Exit;
            End {Case fArcType Of};

            LocalStream.Position := OffsetOfLocalHeader;

            (* Read LocalZipHeader & Filename *)
            ZeroMemory(@LocalZipHeader, SizeOf(TLocal));
            BytesRead := LocalStream.Read(LocalZipHeader, SizeOf(TLocal));
            If BytesRead <> 0 Then ;

            With LocalZipHeader Do
            Begin
               VerSig(SignAtr, htLocal, Encrypted);
               If Encrypted Then
                  DecodeHeader(@LocalZipHeader, htLocal);

               ReadFilename(LocalStream, pFilename, ZC.FileNameLen);

               If Encrypted Then
                  DecodeFilename(pFilename, ZC.FileNameLen);

               //LocalStream.Seek(ZC.ExtraFieldLen, soCurrent);
   				Central64Hdr :=
               	Read64BitFieldHdr(
                  	LocalStream,
                     zc.ExtraFieldLen,
                     htLocal);

            End;

            // HeaderList.Header[i]^ has already been decoded
            CentralZipHeader := TCentral(HeaderList.Header[i]^);

            With CentralZipHeader Do
            Begin
               InflateRec.BitFlag := ZC.BitFlag;
               InflateRec.CompressType := ZC.CompressType;
               InflateRec.PackedSize :=
               	(Int64(Central64Hdr.HiPackedSize) Shl 32) Or
                  zc.PackedSize;

               InflateRec.UnpackedSize :=
               	(Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or
                  zc.UnpackedSize;

               ZipCompatible := IsZipCompatible(SignAtr);

               // Assign GlobalDate for access in OnFileExist event... see pibarx.dpr
               GlobalDate := ZC.FileDate;
            End;

            ActualFilename := OemToCharFilter(StrPas(pFilename), fTransOemChar);

            If WriteToFile() Then
            	FileName := ActualFilename  	// format filename
            Else
            	fFileName := ActualFilename;	// non-format filename

            OpenAndExtractFile(LocalStream, outFile, FileAttr, i)
         End;
      End;
   Finally
      FreeMem(pFilename, 256);
   End;
End;
//-------------------------------------------------------------

Function TUnZip.RecoveryBuildHeadArray(Var LocalStream: TStream32): Integer;
Var
   pFilename: PChar;
   BytesRead: Integer;
   CurrentPos: Int64;
   CancelNext,
   	Encrypted: Boolean;
Begin
   Result := 0;
   ZipTimer.Suspend;

   If Assigned(OnCorruptZipHeader) Then
   Begin
      CancelNext := True;
      OnCorruptZipHeader(Self, HeaderTypeState, CancelNext);
      If CancelNext Then Exit;
   End;

   GetMem(pFilename, 256);
   Try
      CurrentPos := fOffsetStart;

      LocalStream.Position := CurrentPos;
      BytesRead := LocalStream.Read(LocalZipHeader, SizeOf(TLocal));

      If (LocalZipHeader.SignAtr = MULTIVOL_HEADER_SIGNATURE) Then
      Begin
         Inc(CurrentPos, SizeOf(LocalZipHeader.SignAtr));
         LocalStream.Position := CurrentPos;
       	BytesRead := LocalStream.Read(LocalZipHeader, SizeOf(TLocal));
     End;

      With LocalZipHeader Do
         While (BytesRead = SizeOf(TLocal)) And
            (VerSig(LocalZipHeader.SignAtr, htLocal, Encrypted) = htLocal) Do
         Begin
            If Cancel Then break;
            Try
               If Encrypted Then
                  DecodeHeader(@LocalZipHeader, htLocal);

               ReadFilename(LocalStream, pFilename, ZC.FileNameLen);

               If Encrypted Then
                  DecodeFilename(pFilename, zc.FileNameLen);

            	Central64Hdr :=
               	Read64BitFieldHdr(
                  	LocalStream,
                     zc.ExtraFieldLen,
                     htLocal);

               ActualFilename := OemToCharFilter(StrPas(pFilename), fTransOemChar);
               If (ActualFilename <> '') And
               	CheckWildCard2(
                  	ActualFilename,
                     FileSpec,
                     ExcludeSpec,
                     RecurseDirs) Then
               Begin

                  With pUBFI^ Do
                  Begin
                     DiskWithThisFile := 1;
                     OffsetOfLocalHeader := CurrentPos;
                     FileAttr := ExternalAttr;
                  End;
                  HeaderList.AddItem(pUBFI^, Nil, 0);

                  If zc.FileDate > fMaxAge Then
                     fMaxAge := zc.FileDate;

                  fTotalPackedSize :=
                     (Int64(Central64Hdr.HiPackedSize) Shl 32) Or zc.PackedSize +
                  	fTotalPackedSize;

                  fTotalUnpackedSize :=
                  	(Int64(Central64Hdr.HiUnpackedSize) Shl 32) Or zc.UnpackedSize +
							fTotalUnpackedSize;

                  If zc.BitFlag And 1 > 0 Then
                     fTotalPackedSize := fTotalPackedSize - RAND_HEAD_LEN;

               End;
            Finally
            	CurrentPos := CurrentPos +
                     ((Int64(Central64Hdr.HiPackedSize) Shl 32) Or zc.PackedSize) +
                  	SizeOf(TLocal) +
                     zc.FileNameLen +
                     zc.ExtraFieldLen;
            End;

            LocalStream.Position := CurrentPos;
            BytesRead := LocalStream.Read(LocalZipHeader, SizeOf(TLocal));
         End;
   Finally
      Result := HeaderList.FileCount;
      FreeMem(pFilename, 256);
      ZipTimer.Resume;
   End;
End;
//-------------------------------------------------------------

Procedure TUnZip.ExtractIT(Var inStream: TStream32; outFile: TStream32);
Begin
   Try
   	//If (fArcType In [atZipDS..atZipMV]) Then
      If fArcType = atZipDS Then
      Begin
      	DiskSpannObj.INIT(Self, RaiseError, RaiseErrorStr,
         	OnDiskWriteProtectErr, OnDiskInDrvErr, fCancel);

         If Not DiskSpannObj.GetDisk(inStream) Then
            Exit;
      End;

      HeaderList := TUnBaseHeaderObj.Create();
      Try
         HeaderList.INIT();
         Try
            If (HeaderTypeState = [htLocal]) Then
            Begin
               If RecoveryBuildHeadArray(inStream) > 0 Then
                  RecoveryProcessHeaders(inStream, outFile);
            End
            Else
               If BuildHeadArray(inStream) > 0 Then
                  ProcessHeaders(inStream, outFile);
         Finally
            HeaderList.DONE();
         End;
      Finally
         HeaderList.Free();
      End;
   Except
   End;
End;
//-------------------------------------------------------------



End.
