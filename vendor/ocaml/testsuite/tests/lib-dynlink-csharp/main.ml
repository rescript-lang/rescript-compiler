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

let load s =
  Printf.printf "Loading %s\n%!" s;
  try
    Dynlink.loadfile s
  with Dynlink.Error e ->
    print_endline (Dynlink.error_message e)

let () =
  print_endline "Main is running.";
  Dynlink.init ();
  Dynlink.allow_unsafe_modules true;
  let s1,s2 =
    if Dynlink.is_native then
      "../../../otherlibs/bigarray/bigarray.cmxs",
      "plugin.cmxs"
    else
      "../../../otherlibs/bigarray/bigarray.cma",
      "plugin.cmo"
  in
  load s1;
  load s2;
  print_endline "OK."
