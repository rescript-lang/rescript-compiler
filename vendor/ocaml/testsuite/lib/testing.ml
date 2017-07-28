(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Testing auxilliaries. *)

open Scanf;;

let all_tests_ok = ref true;;

let finish () =
  match !all_tests_ok with
  | true ->
      print_endline "\nAll tests succeeded."
  | _ ->
      print_endline "\n\n********* Test suite failed. ***********\n";;

at_exit finish;;

let test_num = ref (-1);;

let print_test_number () =
  print_string " "; print_int !test_num; flush stdout;;

let next_test () =
  incr test_num;
  print_test_number ();;

let print_test_fail () =
  all_tests_ok := false;
  print_string
   (Printf.sprintf "\n********* Test number %i failed ***********\n"
    !test_num);;

let print_failure_test_fail () =
  all_tests_ok := false;
  print_string
   (Printf.sprintf
      "\n********* Failure Test number %i incorrectly failed ***********\n"
    !test_num);;

let print_failure_test_succeed () =
  all_tests_ok := false;
  print_string
   (Printf.sprintf
      "\n********* Failure Test number %i failed to fail ***********\n"
    !test_num);;

let test b =
  next_test ();
  if not b then print_test_fail ();;

(* Applies f to x and checks that the evaluation indeed
   raises an exception that verifies the predicate [pred]. *)
let test_raises_exc_p pred f x =
  next_test ();
  try
    ignore (f x);
    print_failure_test_succeed ();
    false
  with
  | x ->
    pred x || (print_failure_test_fail (); false);;

(* Applies f to x and checks that the evaluation indeed
   raises some exception. *)
let test_raises_some_exc f = test_raises_exc_p (fun _ -> true) f;;
let test_raises_this_exc exc = test_raises_exc_p (fun x -> x = exc);;

(* Applies f to x and checks that the evaluation indeed
   raises exception Failure s. *)

let test_raises_this_failure s f x =
  test_raises_exc_p (fun x -> x = Failure s) f x;;

(* Applies f to x and checks that the evaluation indeed
   raises the exception Failure. *)
let test_raises_some_failure f x =
  test_raises_exc_p (function Failure _ -> true | _ -> false) f x;;

let failure_test f x s = test_raises_this_failure s f x;;
let any_failure_test = test_raises_some_failure;;

let scan_failure_test f x =
  test_raises_exc_p (function Scan_failure _ -> true | _ -> false) f x;;
