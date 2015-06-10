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
Unit ztvJar;

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
   ztvBase,
   ztvGbls,
   ztvZip;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TJar = Class(TZip)
   Private
   Protected
      Procedure SetArcType(SAT: TArcType); Override;
   Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;
   Published
      Property ArcType;
      Property OnError;
   End;

Implementation

Uses
   ztvHeaders;

//-------------------------------------------------------------

Constructor TJar.Create(AOwner: TComponent);
Begin
   Inherited Create(AOwner);
   fMasterExt := '.JAR';
   DefaultExt := fMasterExt;
   CompressMethodState := [cmStore, cmDeflate];
   CompressMethod := cmDeflate;
   fArcType := atJar;
End;
//-------------------------------------------------------------

Destructor TJar.Destroy;
Begin
   Inherited Destroy;
End;
//-------------------------------------------------------------

Procedure TJar.SetArcType(SAT: TArcType);
Begin
   fArcType := atJar;
   CompressArcType;
End;
//-------------------------------------------------------------

End.
