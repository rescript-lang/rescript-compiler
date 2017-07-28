(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* A very simple slave:
   - read the string " Ping" on stdin,
   - then print the string "-pong" on stderr,
   - and send it back on stdout
   - until reading the string "stop" on stdin,
   - then print the string "!\n" on stderr,
   - send back the string "OK, bye!" on stdout,
   - and die.

   Use the communication module Test_scanf2_io. *)

open Tscanf2_io;;

let ib = Scanf.Scanning.from_channel stdin;;
let ob = Buffer.create 1024
and oc = stdout;;

let send_string_pong ob = send_string ob oc "-pong";;
let send_string_okbye ob = send_string ob oc "OK, bye!";;

while true do
  let s = receive_string ib in
  match s with
  | " Ping" -> Printf.eprintf "-pong"; flush stderr; send_string_pong ob
  | "stop" -> Printf.eprintf "!\n"; flush stderr; send_string_okbye ob; exit 0
  | s -> failwith ("Slave: unbound string " ^ s)
done
;;
