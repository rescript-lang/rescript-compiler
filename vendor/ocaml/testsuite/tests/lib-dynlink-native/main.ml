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
  Api.add_cb (fun () -> print_endline "Callback from main")

let ()  =
  Dynlink.init ();
  Dynlink.allow_unsafe_modules true;
  for i = 1 to Array.length Sys.argv - 1 do
    let name = Sys.argv.(i) in
    Printf.printf "Loading %s\n" name; flush stdout;
    try
      if name.[0] = '-'
      then Dynlink.loadfile_private
        (String.sub name 1 (String.length name - 1))
      else Dynlink.loadfile name
    with
      | Dynlink.Error err ->
          Printf.printf "Dynlink error: %s\n"
            (Dynlink.error_message err)
      | exn ->
          Printf.printf "Error: %s\n" (Printexc.to_string exn)
  done;
  flush stdout;
  try
    let oc = open_out_bin "marshal.data" in
    Marshal.to_channel oc !Api.cbs [Marshal.Closures];
    close_out oc;
    let ic = open_in_bin "marshal.data" in
    let l = (Marshal.from_channel ic : (unit -> unit) list) in
    close_in ic;
    List.iter (fun f -> f()) l
  with Failure s ->
    Printf.printf "Failure: %s\n" s
