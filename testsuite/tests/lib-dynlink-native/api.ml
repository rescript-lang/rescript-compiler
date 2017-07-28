(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let mods = ref []

let reg_mod name =
  if List.mem name !mods then
    Printf.printf "Reloading module %s\n" name
  else (
    mods := name :: !mods;
    Printf.printf "Registering module %s\n" name
  )


let cbs = ref []

let add_cb f = cbs := f :: !cbs
let runall () = List.iter (fun f -> f ()) !cbs

(*
let () =
  at_exit runall
*)
