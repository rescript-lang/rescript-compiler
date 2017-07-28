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

let x = ref 0

let () =
  Api.reg_mod "Plugin_ref";

  Api.add_cb
    (fun () ->
       Printf.printf "current value for ref = %i\n" !x;
       incr x
    )
