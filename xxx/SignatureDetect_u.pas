unit SignatureDetect_u;

interface

uses Classes;

type TFileType=(ftMZF,ftZip,ftRar,ftZoo,ftBH,ftGZip,
                ftLha,ftCab,ftTar,ftACE2,ftJar,ftArc,
                ftArj,ftPKG5,ft7Z,ftAnyFile);

function GetArchiveTypeBySignature(AFileName:String):TFileType;                

implementation

Const
   LHA_SIGNATURE = $6C2D;
   ZOO_SIGNATURE = $4F5A;               //dec value = 20314   79/90
   ARJ_SIGNATURE = $EA60;
   DCP_SIGNATURE = $35474B50;

   MS_GZ_HEADER_SIGNATURE = $8C655D13;//???
   BLAKHOLE_SIGNATURE = $7054842;
   MAIN_RAR_HEADER_SIGNATURE = $21726152;
   ZLIB_HEADER_SIGNATURE = DCP_SIGNATURE;

   CAB_SIGNATURE = $4643534D;
   CAB_SIGNATURE64 = $3e0c140048d104d;
   GZIP_HEADER_SIGNATURE = $8D1F;
   ZIPTV_SFX_SIGNATURE = $5846535A;
   ZIP_SIGNATURE=$4B50;
   MZF_SIGNATURE=$465A4D;
   LZMA_SIGNATURE=$7A37;

   {LOCAL_FILE_HEADER_SIGNATURE = $04034B50;
   LOCAL_FILE_ENCRPT_SIGNATURE = $04034C50;
   LOCAL_CUST_HEADER_SIGNATURE = $04034D50;
   LOCAL_CUST_ENCRPT_SIGNATURE = $04034E50;

   CENTRAL_FILE_HEADER_SIGNATURE = $02014B50;
   CENTRAL_FILE_ENCRPT_SIGNATURE = $02014C50;
   CENTRAL_CUST_HEADER_SIGNATURE = $02014D50;
   CENTRAL_CUST_ENCRPT_SIGNATURE = $02014E50;

   CENTRAL_FILE_HEADER_DIGITAL = $05054b50;

   END_OF_CENTRAL_HEADER_SIGNATURE = $06054B50;
   END_OF_CENTRAL_ENCRPT_SIGNATURE = $06054C50;
   END_OF_CENTRAL64_HEADER_SIGNATURE = $06055B50;
   END_OF_CENTRAL64_ENCRPT_SIGNATURE = $06055C50;

   END_OF_CENTRAL_WZIP_HEADER_SIGNATURE = $06064B50;
   CENTRAL_WZIP_HEADER_LOCATOR = $07064b50;
   MULTIVOL_HEADER_SIGNATURE = $08074B50;    }

function GetArchiveTypeBySignature(AFileName:String):TFileType;
Var FileStream:TMemoryStream;
    Temp:Int64;
    ReadCount:Byte;
    Label Start;
Begin
  FileStream:=TMemoryStream.Create;
  FileStream.LoadFromFile(AFileName);
  FileStream.Position:=0;
  ReadCount:=1;
  Start:;
  Temp:=$00;
  FileStream.ReadBuffer(Temp,ReadCount);
  FileStream.Position:=$00;
  Case Temp Of
    MZF_SIGNATURE:Result:=ftMZF;
    ZIP_SIGNATURE:Result:=ftZip;
    LHA_SIGNATURE:Result:=ftLha;
    ZOO_SIGNATURE:Result:=ftZoo;
    ARJ_SIGNATURE:Result:=ftArj;
    DCP_SIGNATURE:Result:=ftPKG5;
    MAIN_RAR_HEADER_SIGNATURE:Result:=ftRar;
    CAB_SIGNATURE:Result:=ftCab;
    BLAKHOLE_SIGNATURE:Result:=ftBh;
    GZIP_HEADER_SIGNATURE:Result:=ftGZip;
    LZMA_SIGNATURE:Result:=ft7Z;
    Else
      Begin
        If ReadCount>8 Then
          Begin
            Result:=ftAnyFile;
            FileStream.Free;
            Exit;
          End;
        Inc(ReadCount);
        GoTo Start;
      End;
  End;
  FileStream.Free;
End;

end.
 