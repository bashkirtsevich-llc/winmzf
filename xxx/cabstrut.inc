(**********************************************************************

  Copyright 1998-2002,  Microchip Data Systems / Carl Bunton

  Under license agreement, this source module may be used only on a
  single computer.

  No portion of this module may be reproduced, copied, revised, edited,
  distributed or transmitted via electronic means except in compiled
  application format.

  Web-site:  http://www.ziptv.com
  Email:     custsupt@ziptv.com

**********************************************************************)
var
	CFHEADER	 :  TCAB_FILE_HEADER;
	CFFOLDER  :  TCAB_FOLDER;
	CFDATA	 :  TCAB_CFDATA;
	//CFFILE	 :  TCAB_CFFILE;	//v4.10.0 moved to parent obj

	szCabinetPREV  : string;			{u1}	(* (optional) name of previous cabinet file *)
	szDiskPREV	   : string;			{u1}	(* (optional) name of previous disk *)
	szCabinetNEXT  : string;			{u1}	(* (optional) name of next cabinet file *)
	szDiskNEXT	   : string;			{u1}	(* (optional) name of next disk *)
	FolderStartPos : integer;

	cbCFHeader	   : word;
	cbCFFolder		: byte;
	cbCFData			: byte;



const
	(* flags *)
	cfhdrPREV_CABINET  	= $0001;
	cfhdrNEXT_CABINET  	= $0002;
	cfhdrRESERVE_PRESENT = $0004;

	ifoldCONTINUED_FROM_PREV 		= $FFFD;
	ifoldCONTINUED_TO_NEXT   	  	= $FFFE;
	ifoldCONTINUED_PREV_AND_NEXT 	= $FFFF;

	CFHEADERlen = 36;
	CFFOLDERlen = 8;
	CFDATAlen = 8;
	CFFILElen = 16;



function GetCabinet(Var Infile: TStream32; Var FFilePos: Int64): boolean;
var
	BytesRead: DWord;
	r: boolean;
	TempStr : pchar;
begin
   BytesRead := Infile.Read( CFHEADER, CFHEADERlen );
   r := BytesRead > 0;

   fFilePos := Infile.Position;

	if (CFHEADER.Signature = $4643534D) then
	begin
		//if CFHEADER.cFolders > 1 then
		//	MessageDlg('cFolders = ' + inttostr(CFHEADER.cFolders), mtInformation, [mbOK], 0);

		(* -------------------------------------------------- *)

		(* flags.cfhdrRESERVE_PRESENT is set if this cabinet file
			contains any reserved fields.
			When this bit is set, the cbCFHeader, cbCFFolder, and
			cbCFData fields are present in this CFHEADER. *)
		if (CFHEADER.flags and cfhdrRESERVE_PRESENT) = cfhdrRESERVE_PRESENT then
		begin

			//MessageDlg('We have cfhdrRESERVE_PRESENT flag!', mtInformation, [mbOK], 0);

			(* CFHEADER.cbCFHeader;
				If flags.cfhdrRESERVE_PRESENT is not set, this field
				is not present, and the value of cbCFHeader defaults
				to zero.  Indicates the size in bytes of the abReserve
				field in this CFHEADER.  Values for cbCFHeader range
				from 0 to 60,000. *)
         {BytesRead :=} Infile.Read( cbCFHeader, SizeOf( Word ) );

			(* CFHEADER.cbCFFolder;
				If flags.cfhdrRESERVE_PRESENT is not set, then this
				field is not present, and the value of cbCFFolder
				defaults to zero.  Indicates the size in bytes of the
				abReserve field in each CFFOLDER entry.  Values for
				cbCFFolder range from 0 to 255. *)
         {BytesRead :=} Infile.Read( cbCFFolder, SizeOf( Byte ) );

			(* CFHEADER.cbCFData;
				If flags.cfhdrRESERVE_PRESENT is set, then this field
				is not present, and the value for cbCFData defaults to
				zero.  Indicates the size in bytes of the abReserve
				field in each CFDATA entry.  Values for cbCFData range
				from 0 to 255. *)
         {BytesRead :=} Infile.Read( cbCFData, SizeOf( Byte ) );

         Infile.Position := Infile.Position + cbCFHeader;
		end else begin
			cbCFHeader := 0;
			cbCFFolder := 0;
			cbCFData   := 0;
		end;

		(* -------------------------------------------------- *)

		(* flags.cfhdrPREV_CABINET is set if this cabinet
			file is not the first in a set of cabinet files.
			When this bit is set, the szCabinetPrev and szDiskPrev
			fields are present in this CFHEADER. *)
		if (CFHEADER.flags and cfhdrPREV_CABINET) = cfhdrPREV_CABINET then
		begin
        	GetMem( TempStr, 256 );
			try
					(* szCabinetPrev[]	(optional)
						If flags.cfhdrPREV_CABINET is not set, then this field
						is not present.  NUL-terminated ASCII string containing
						the file name of the logically-previous cabinet file.
						May contain up to 255 bytes plus the NUL byte.  Note
						that this gives the name of the most-recently-preceding
						cabinet file that contains the initial instance of a
						file entry.  This might not be the immediately previous
						cabinet file, when the most recent file spans multiple
						cabinet files.  If searching in reverse for a specific
						file entry, or trying to extract a file which is reported
						to begin in the “previous cabinet”, szCabinetPrev would
						give the name of the cabinet to examine. *)
            If Infile.Read( TempStr^, 255 ) = 0 Then
            	r := false;

            szCabinetPrev := StrPas(TempStr);
            fFilePos := fFilePos + (length(szCabinetPrev) + 1);
            Infile.Position := fFilePos;

					(* szDiskPrev[]	(optional)
						If flags.cfhdrPREV_CABINET is not set, then this field
						is not present.  NUL-terminated ASCII string containing
						a descriptive name for the media containing the file
						named in szCabinetPrev, such as the text on the diskette
						label.  This string can be used when prompting the user
						to insert a diskette.  May contain up to 255 bytes plus
						the NUL byte. *)
            If Infile.Read( TempStr^, 255 ) = 0 Then
					r := false;

            szDiskPrev := StrPas(TempStr);
            fFilePos := fFilePos + (length(szDiskPrev) + 1);
            Infile.Position := fFilePos;
			finally
				FreeMem( TempStr, 256 );
			end;
		end else begin
			szCabinetPREV := '';
			szDiskPREV := '0';
		end;

		(* -------------------------------------------------- *)

		(* flags.cfhdrNEXT_CABINET is set if this cabinet file
			is not the last in a set of cabinet files.  When this
			bit is set, the szCabinetNext and szDiskNext fields
			are present in this CFHEADER. *)
		if (CFHEADER.flags and cfhdrNEXT_CABINET) = cfhdrNEXT_CABINET then
		begin
        	GetMem( TempStr, 256 );
			try
					(* szCabinetNext[]	(optional)
						If flags.cfhdrNEXT_CABINET is not set, then this field
						is not present.  NUL-terminated ASCII string containing
						the file name of the next cabinet file in a set.  May
						contain up to 255 bytes plus the NUL byte.  Files
						extending beyond the end of the current cabinet file
						are continued in the named cabinet file. *)
            If Infile.Read( TempStr^, 255 ) = 0 Then
            	r := false;

            szCabinetNext := StrPas(TempStr);
            fFilePos := fFilePos + (length(szCabinetNext) + 1);
            Infile.Position := fFilePos;

					(* szDiskNext[]	(optional)
						If flags.cfhdrNEXT_CABINET is not set, then this field
						is not present.  NUL-terminated ASCII string containing
						a descriptive name for the media containing the file
						named in szCabinetNext, such as the text on the diskette
						label.  May contain up to 255 bytes plus the NUL byte.
						This string can be used when prompting the user to insert
						a diskette. *)
            If Infile.Read( TempStr^, 255 ) = 0 Then
            	r := false;

            szDiskNext := strpas(TempStr);
            fFilePos := fFilePos + (length(szDiskNext) + 1);
            Infile.Position := fFilePos;
			finally
				FreeMem( TempStr, 256 );
			end;
		end else begin
			szCabinetNEXT := '';
			szDiskNEXT := '0';
		end;

		FolderStartPos := Infile.Position;

		//(* Read CFFOLDER information *)
     	If Infile.Read( CFFOLDER, CFFOLDERlen ) = 0 Then
     		r := false;

     Infile.Position := CFFOLDER.coffCabStart;
     Infile.Read( CFDATA, CFDATAlen );

		//FFilePos := CFHeader.coffFiles;
	end else
		r := false;

	Result := r;
end;



