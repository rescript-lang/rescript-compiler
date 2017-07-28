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

(* A very simple communication module using buffers. It should help detecting
   advanced character reading by Scanf when using stdin. *)

let send_flush send ob oc t =
  send ob t;
  Buffer.output_buffer oc ob;
  Buffer.clear ob;
  flush oc
;;

(* The correct sending format for the test should be "%S\n",
   but to avoid problems when Scanf ask too early for the next character,
   "%S\n\n" is fine. *)
let send_string = send_flush (fun ob -> Printf.bprintf ob "%S\n");;

(* The correct reading format for the test should be "%S\n",
   but to avoid problems when Scanf ask too early for the next character,
   " %S\n" is fine. *)
let receive_string ib = Scanf.bscanf ib "%S\n" (fun s -> s);;
