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
  This unit sets conditional defines according to which compiler is
  in use.
**********************************************************************)
{$ifdef VER80}  {$define DEL} {$endif}  //not supported
{$ifdef VER90}  {$define DEL} {$endif}
{$ifdef VER100} {$define DEL} {$endif}
{$ifdef VER120} {$define DEL} {$endif}
{$ifdef VER130} {$define DEL} {$endif}		//(same as BCB 5)
{$ifdef VER140} {$define DEL} {$endif}		//(same as BCB 6)
{$ifdef VER150} {$define DEL} {$endif}		//(same as BCB 7)

// BCB5+ pre-defines the BCB define
//{$ifdef VER?}  {$define BCB1} {$define BCB} {$endif}	//not supported
//{$ifdef VER93}  {$define BCB} {$endif}
//{$ifdef VER110} {$define BCB} {$endif}
//{$ifdef VER125} {$define BCB} {$endif}

{$undef BCB}
//{$ifdef VER?}  {$define BCB1} {$define BCB} {$endif}	//not supported
{$ifdef VER93} {$define BCB2} {$define BCB} {$endif}
{$ifdef VER110} {$define BCB3} {$define BCB} {$endif}
{$ifdef VER125} {$define BCB4} {$define BCB} {$endif}
//{.$ifdef VER130} {$define BCB5} {$define BCB} {$endif}  	//(same as delphi 5)
//{.$ifdef VER140} {$define BCB6} {$define BCB} {$endif}  	//(bcb6 not available yet)

// extra insurance
{$ifdef BCB}
{$undef DEL}
{$endif BCB}

// Delphi 1 = VER80
// Delphi 2 = VER90
// Delphi 3 = VER100
// Delphi 4 = VER120
// Delphi 5 = VER130
// Delphi 6 = VER140
// Delphi 7 = VER150

// BCB5+ contains a compiler pre-defined BCB
// BCB2 = VER93
// BCB3 = VER110
// BCB4 = VER125
// BCB5 = BCB + VER130
// BCB6 = BCB + VER140
// BCB6 = BCB + VER150

{$ifdef BCB}
	{$ifndef VER110}                    		// bcb 3
      {$define BCB4_OR_HIGHER}
      {$define DEL4_OR_HIGHER}
      {$ifndef VER125}                    	// bcb 4
         {$define BCB5_OR_HIGHER}
         {$define DEL5_OR_HIGHER}
         {$ifndef VER130}                    // bcb 5
            {$define BCB6_OR_HIGHER}
            {$define DEL6_OR_HIGHER}
            {$ifndef VER140}
               {$define BCB7_OR_HIGHER}
            	{$define DEL7_OR_HIGHER}
            	{$ifndef VER150}
               	unsupported compiler
            	{$endif}
            {$endif VER140}
         {$endif VER130}
      {$endif VER125}
   {$else}
   	unsupported compiler
   {$endif VER110}
{$else}
	{$ifndef VER80}											// delphi 1... unsupported
   	{$ifndef VER90}                              // delphi 2
      	{$define DEL3_OR_HIGHER}
         {$ifndef VER100}                          // delphi 3
         	{$define DEL4_OR_HIGHER}
            {$ifndef VER120}                       // delphi 4
            	{$define DEL5_OR_HIGHER}
               {$ifndef VER130}                    // delphi 5
               	{$define DEL6_OR_HIGHER}
                  {$ifndef VER140}                 // delphi 6
                  	{$define DEL7_OR_HIGHER}
                  	{$ifndef VER150}              // delphi 7
         					{unsupported compiler}
                  	{$endif VER150}
               	{$endif VER140}
               {$endif VER130}
            {$endif VER120}
         {$else}
         	unsupported compiler
         {$endif VER100}
		{$else}
			unsupported compiler
     	{$endif VER90}
	{$else VER80}
   	unsupported compiler
	{$endif}
{$endif}

{$ifndef WIN32}
	unsupported 16-bit compiler
{$endif WIN32}

// BCB5+ pre-defines the BCB define
{$ifdef BCB}
   {$OBJEXPORTALL ON}
{$endif}

{$ifndef DEL4_OR_HIGHER}
{$ifndef BCB4_OR_HIGHER}
  unsupported compiler
{$endif BCB4_OR_HIGHER}
{$endif DEL4_OR_HIGHER}

{$define DEFLATE_BUG_WORKAROUND}



