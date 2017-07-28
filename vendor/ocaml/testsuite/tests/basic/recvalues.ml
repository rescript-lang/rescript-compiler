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

(* Recursive value definitions *)

let _ =
  let rec x = 1 :: x in
  if match x with
       1 :: x' -> x == x'
     | _ -> false
  then print_string "Test 1: passed\n"
  else print_string "Test 1: FAILED\n";
  let one = 1 in
  let rec y = (one, one+1) :: y in
  if match y with
       (1,2) :: y' -> y == y'
     | _ -> false
  then print_string "Test 2: passed\n"
  else print_string "Test 2: FAILED\n";
  let rec z = (Gc.minor(); (one, one+1)) :: z in
  (* Trash the minor generation *)
  for i = 0 to 50000 do ignore (ref 0) done;
  if match z with
       (1,2) :: z' -> z == z'
     | _ -> false
  then print_string "Test 3: passed\n"
  else print_string "Test 3: FAILED\n";
;;

let rec s = "bar"
and idx = 1
and x1 = let f x = Printf.printf "%s\n" x in f "foo"; s, x4
and x2 = [| x1; x1 |]
and x3 = (fun () -> fst (x2.(idx))) :: x3
and x4 = {contents = x3}
;;

Gc.minor ();;
if (List.hd (!(snd (x2.(0))))) () == s
then print_string "Test 4: passed\n"
else print_string "Test 4: FAILED\n"
