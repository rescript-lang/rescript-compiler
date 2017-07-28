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

let main () =
  let (rd, wr) = Unix.pipe() in
  let t = Thread.create
    (fun () ->
      Thread.delay 1.0;
      print_endline "closing fd...";
      Unix.close wr;
    )
    () in
  let buf = String.create 10 in
  print_endline "reading...";
  begin try ignore (Unix.read rd buf 0 10) with Unix.Unix_error _ -> () end;
  print_endline "read returned";
  t

let t = Unix.handle_unix_error main ()

let _ = Thread.join t
