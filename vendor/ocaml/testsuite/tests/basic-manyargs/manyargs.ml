(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let manyargs a b c d e f g h i j k l m n o =
  print_string "a = "; print_int a; print_newline();
  print_string "b = "; print_int b; print_newline();
  print_string "c = "; print_int c; print_newline();
  print_string "d = "; print_int d; print_newline();
  print_string "e = "; print_int e; print_newline();
  print_string "f = "; print_int f; print_newline();
  print_string "g = "; print_int g; print_newline();
  print_string "h = "; print_int h; print_newline();
  print_string "i = "; print_int i; print_newline();
  print_string "j = "; print_int j; print_newline();
  print_string "k = "; print_int k; print_newline();
  print_string "l = "; print_int l; print_newline();
  print_string "m = "; print_int m; print_newline();
  print_string "n = "; print_int n; print_newline();
  print_string "o = "; print_int o; print_newline();
  print_string "---"; print_newline()

let manyargs_tail1 a b c d e f g h i j k l m n o =
  print_string "tail1:\n";
  manyargs a b c d e f g h i j k l m n o

let manyargs_tail2 a b =
  print_string "tail2:\n";
  manyargs a b a b a b a b a b a b a b a

let manyargs_tail3 a b c d e f g h i j k l m n o =
  print_string "tail3:\n";
  print_string "o = "; print_int o; print_newline();
  print_string "---"; print_newline()

let _ =
  manyargs 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15;
  manyargs_tail1 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15;
  manyargs_tail2 0 1;
  manyargs_tail3 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15

external manyargs_ext:
  int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int ->
    int
  = "manyargs_argv" "manyargs"

let _ =
  print_string "external:\n"; flush stdout;
  manyargs_ext 1 2 3 4 5 6 7 8 9 10 11
