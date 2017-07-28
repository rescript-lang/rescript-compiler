(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Test bound checks with ocamlopt *)

let a = [| 0; 1; 2 |]

let trail = ref []

let test n =
  let result =
    try
      trail := n :: !trail; ignore a.(n); "doesn't fail"
    with Invalid_argument s ->
           (* Check well-formedness of s *)
           if String.length s = 19
           && s = "index out of bounds"
           then "fails"
           else "bad Invalid_argument"
       | _ -> "bad exception"
  in
    print_int n; print_string ": "; print_string result; print_newline()

let _ =
  test 0; test 1; test 2; test 3; test 4; test (-1);
  Gc.full_major();
  print_string "Trail:";
  List.iter (fun n -> print_string " "; print_int n) !trail;
  print_newline()
