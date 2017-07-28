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

(* Torture test - lots of GC *)

let finished = ref false;;

let gc_thread () =
  while not !finished do
(*    print_string "gc"; print_newline(); *)
    Gc.minor();
    Thread.yield()
  done

let stdin_thread () =
  while not !finished do
    print_string ">"; flush stdout;
    let s = read_line() in
    print_string " >>> "; print_string s; print_newline()
  done

let writer_thread (oc, size) =
  while not !finished do
(*    print_string "writer "; print_int size; print_newline(); *)
    let buff = String.make size 'a' in
    Unix.write oc buff 0 size
  done;
  let buff = String.make size 'b' in
  Unix.write oc buff 0 size

let reader_thread (ic, size) =
  while true do
(*    print_string "reader "; print_int size; print_newline(); *)
    let buff = String.create size in
    let n = Unix.read ic buff 0 size in
(*    print_string "reader "; print_int n; print_newline(); *)
    for i = 0 to n-1 do
      if buff.[i] = 'b' then raise Exit
      else if buff.[i] <> 'a' then prerr_endline "error in reader_thread"
    done
  done

let main() =
  let t1 = Thread.create gc_thread () in
  let (out1, in1) = Unix.pipe() in
  let t2 = Thread.create writer_thread (in1, 4096) in
  let t3 = Thread.create reader_thread (out1, 4096) in
  let (out2, in2) = Unix.pipe() in
  let t4 = Thread.create writer_thread (in2, 16) in
  let t5 = Thread.create reader_thread (out2, 16) in
  try
    stdin_thread()
  with _ ->
    finished := true;
    List.iter Thread.join [t1; t2; t3; t4; t5]

let _ = main()
