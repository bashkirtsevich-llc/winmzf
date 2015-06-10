Unit ztvLoadLib;

(*--------------------------------------------------------------------------*)
(*                                                                          *)
(*       Unit   : MyLibLod                                                  *)
(*                                                                          *)
(*       Purpose: Loads library dynamically and suppresses error box.       *)
(*                                                                          *)
(*       Author : Philip R. "Pib" Burns.  98/08/10.                         *)
(*                                                                          *)
(*--------------------------------------------------------------------------*)

Interface

Uses
   Messages,
   Windows;

{$I ZipTV.inc}                          //Declare the compiler defines

(* EXPORTS *)
Function MyLoadLibrary(LibName: String): THandle;

(*--------------------------------------------------------------------------*)

Implementation

(*--------------------------------------------------------------------------*)
(*     MyLoadLibrary --- Loads library while suppressing error box.         *)
(*--------------------------------------------------------------------------*)

Function MyLoadLibrary(LibName: String): THandle;

Var
   PrevError: longint;

Begin                                   (* MyLoadLibrary *)
   (* Don't display an error box if the *)
   (* library can't be loaded.          *)

   PrevError := SetErrorMode(SEM_NOOPENFILEERRORBOX);

   (* Try loading the library. *)

   Result := LoadLibrary(PChar(LibName));

   (* Restore previous error box display *)
   (* state.                             *)
   SetErrorMode(PrevError);

End (* MyLoadLibrary *);

(*--------------------------------------------------------------------------*)

End.
