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

(*external ex: int -> int = "caml_ex"*)

let () =
  Api.reg_mod "Plugin2";
  Api.add_cb (fun () -> print_endline "Callback from plugin2");
(*  let i = ex 3 in*)
  List.iter (fun i -> Printf.printf "%i\n" i) Plugin.facts;
  Printf.printf "XXX\n"
