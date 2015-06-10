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
Unit ztvUnJar;

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
   ztvUnZIP;

{$I ZipTV.inc}                          //Declare the compiler defines

Type
   TUnJar = Class(TUnZip)
   Private
    { Private declarations }
   Protected
    { Protected declarations }
   Public
    { Public declarations }
   Published
    { Published declarations }
   End;



Implementation



End.
