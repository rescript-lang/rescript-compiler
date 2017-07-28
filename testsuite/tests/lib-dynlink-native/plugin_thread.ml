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

let () =
  Api.reg_mod "Plugin_thread";
  let _t =
    Thread.create
      (fun () ->
         for i = 1 to 5 do
           print_endline "Thread"; flush stdout;
           Thread.delay 1.;
         done
      ) ()
  in
  for i = 1 to 10 do
    print_endline "Thread"; flush stdout;
    Thread.delay 0.50;
  done
