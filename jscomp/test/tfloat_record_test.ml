(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
let buf = Buffer.create 50
let fmt = Format.formatter_of_buffer buf
let print_float f = Format.fprintf fmt "%s" (string_of_float f)
let print_newline () = Format.fprintf fmt "\n"
let s = {Float_record.f= Float_record.make 1.0}

;;
print_float (Float_record.from s.Float_record.f)
;;
print_newline ()

let b = (Float_array.small_float_array [@inlined]) 12
let c = (Float_array.longer_float_array [@inlined]) 34

let print_array a =
  Array.iter (fun f -> print_float f ; print_newline ()) a ;
  print_newline ()

let () =
  print_array (fst b) ;
  print_array (fst c)

let suites : Mt.pair_suites ref = ref []
let test_id = ref 0
let eq f a b = Mt_global.collect_eq test_id suites f a b

let () =
  eq __LOC__ (Buffer.contents buf)
    {|1.
1.
2.
3.

1.
2.
3.
4.
5.
6.
7.
8.
9.
0.
1.
2.
3.
4.
5.
6.
7.
8.
9.
0.
1.
2.
3.
4.
5.
6.
7.
8.
9.
0.
1.
2.
3.
4.
5.
6.
7.
8.
9.
0.

|}

let () = Mt.from_pair_suites __MODULE__ !suites
