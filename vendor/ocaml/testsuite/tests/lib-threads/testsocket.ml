(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Unix

let engine verbose number address =
  print_int number; print_string "> connecting"; print_newline();
  let (ic, oc) = open_connection (ADDR_INET(address, 80)) in
  print_int number; print_string "> connected"; print_newline();
  output_string oc "GET / HTTP1.0\r\n\r\n"; flush oc;
  try
    while true do
      let s = input_line ic in
      if verbose then begin
        print_int number; print_string ">"; print_string s; print_newline()
      end
    done;
  with End_of_file ->
    close_out oc;
    print_int number; print_string "> data retrieved"; print_newline()

let main() =
  let verbose, argv =
    match Sys.argv with
    |  [| _ |] -> false, [| Sys.argv.(0); "caml.inria.fr" |]
    | _ -> true, Sys.argv in
  let addresses = Array.make (Array.length argv - 1) inet_addr_any in
  for i = 1 to Array.length argv - 1 do
    addresses.(i - 1) <- (gethostbyname argv.(i)).h_addr_list.(0)
  done;
  let processes = Array.make (Array.length addresses) (Thread.self()) in
  for i = 0 to Array.length addresses - 1 do
    processes.(i) <- Thread.create (engine verbose i) addresses.(i)
  done;
  for i = 0 to Array.length processes - 1 do
    Thread.join processes.(i)
  done

let _ = Printexc.catch main (); exit 0
