(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Damien Doligez, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Printf;;

(* PR#5233: Create a dangling pointer and use it to access random parts
   of the heap. *)

(* The buggy weak array will end up in smuggle. *)
let smuggle = ref (Weak.create 1);;

(* This will be the weak array (W). *)
let t = ref (Weak.create 1);;

(* Set a finalisation function on W. *)
Gc.finalise (fun w -> smuggle := w) !t;;

(* Free W and run its finalisation function. *)
t := Weak.create 1;;
Gc.full_major ();;

(* smuggle now contains W, whose pointers are not erased, even
   when the contents is deallocated. *)

let size = 1_000_000;;

let check o =
  printf "checking...";
  match o with
  | None -> printf " no value\n";
  | Some s ->
     printf " value found  /  testing...";
     for i = 0 to size - 1 do
       if s.[i] != ' ' then failwith "bad";
     done;
     printf " ok\n";
;;

Weak.set !smuggle 0 (Some (String.make size ' '));;

(* Check the data just to make sure. *)
check (Weak.get !smuggle 0);;

(* Get a dangling pointer in W. *)
Gc.full_major ();;

(* Fill the heap with other stuff. *)
let rec fill n accu = if n = 0 then accu else fill (n-1) (123 :: accu);;
let r = fill ((Gc.stat ()).Gc.heap_words / 3) [];;
Gc.minor ();;

(* Now follow the dangling pointer and exhibit the problem. *)
check (Weak.get !smuggle 0);;
