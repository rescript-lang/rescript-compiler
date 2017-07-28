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

(* Test a file copy function *)

let test msg producer consumer src dst =
  print_string msg; print_newline();
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  let (in_fd, out_fd) = Unix.pipe() in
  let ipipe = Unix.in_channel_of_descr in_fd in
  let opipe = Unix.out_channel_of_descr out_fd in
  let prod = Thread.create producer (ic, opipe) in
  let cons = Thread.create consumer (ipipe, oc) in
  Thread.join prod;
  Thread.join cons;
  if Unix.system ("cmp " ^ src ^ " " ^ dst) = Unix.WEXITED 0
  then print_string "passed"
  else print_string "FAILED";
  print_newline()

(* File copy with constant-sized chunks *)

let copy_file sz (ic, oc) =
  let buffer = String.create sz in
  let rec copy () =
    let n = input ic buffer 0 sz in
    if n = 0 then () else begin
      output oc buffer 0 n;
      copy ()
    end in
  copy();
  close_in ic;
  close_out oc

(* File copy with random-sized chunks *)

let copy_random sz (ic, oc) =
  let buffer = String.create sz in
  let rec copy () =
    let s = 1 + Random.int sz in
    let n = input ic buffer 0 s in
    if n = 0 then () else begin
      output oc buffer 0 n;
      copy ()
    end in
  copy();
  close_in ic;
  close_out oc

(* File copy line per line *)

let copy_line (ic, oc) =
  try
    while true do
      output_string oc (input_line ic); output_char oc '\n'
    done
  with End_of_file ->
    close_in ic;
    close_out oc

(* Create long lines of text *)

let make_lines ofile =
  let oc = open_out ofile in
  for i = 1 to 256 do
    output_string oc (String.make (i*16) '.'); output_char oc '\n'
  done;
  close_out oc

(* Test input_line on truncated lines *)

let test_trunc_line ofile =
  print_string "truncated line"; print_newline();
  let oc = open_out ofile in
  output_string oc "A line without newline!";
  close_out oc;
  try
    let ic = open_in ofile in
    let s = input_line ic in
    close_in ic;
    if s = "A line without newline!"
    then print_string "passed"
    else print_string "FAILED";
    print_newline()
  with End_of_file ->
    print_string "FAILED"; print_newline()

(* The test *)

let main() =
  let ifile = try Sys.argv.(1) with _ -> "testio.ml" in
  let ofile = Filename.temp_file "testio" "" in
  test "256-byte chunks, 256-byte chunks"
       (copy_file 256) (copy_file 256) ifile ofile;
  test "4096-byte chunks, 4096-byte chunks"
       (copy_file 4096) (copy_file 4096) ifile ofile;
  test "65536-byte chunks, 65536-byte chunks"
       (copy_file 65536) (copy_file 65536) ifile ofile;
  test "256-byte chunks, 4096-byte chunks"
       (copy_file 256) (copy_file 4096) ifile ofile;
  test "4096-byte chunks, 256-byte chunks"
       (copy_file 4096) (copy_file 256) ifile ofile;
  test "4096-byte chunks, 65536-byte chunks"
       (copy_file 4096) (copy_file 65536) ifile ofile;
  test "263-byte chunks, 4011-byte chunks"
       (copy_file 263) (copy_file 4011) ifile ofile;
  test "613-byte chunks, 1027-byte chunks"
       (copy_file 613) (copy_file 1027) ifile ofile;
  test "0...8192 byte chunks"
       (copy_random 8192) (copy_random 8192) ifile ofile;
  test "line per line, short lines"
       copy_line copy_line "test-file-short-lines" ofile;
  let linesfile = Filename.temp_file "lines" "" in
  make_lines linesfile;
  test "line per line, short and long lines"
       copy_line copy_line linesfile ofile;
  test_trunc_line ofile;
  Sys.remove linesfile;
  Sys.remove ofile;
  exit 0

let _ = Unix.handle_unix_error main (); exit 0
