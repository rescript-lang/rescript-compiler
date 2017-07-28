(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Printf;;

let flush_all () = flush stdout; flush stderr;;

let message s = print_string s; print_newline ();;

let error_occurred = ref false;;
let immediate_failure = ref true;;

let error () =
 if !immediate_failure then exit 2 else begin
   error_occurred := true;
   flush_all ();
   false
 end;;

let success () = flush_all (); true;;

let function_tested = ref "";;

let testing_function s =
    flush_all ();
    function_tested := s;
    print_newline();
    message s;;

let test test_number eq_fun (answer, correct_answer) =
 flush_all ();
 if not (eq_fun answer correct_answer) then begin
   fprintf stderr ">>> Bad result (%s, test %d)\n" !function_tested test_number;
   error ()
 end else begin
   printf " %d..." test_number;
   success ()
 end;;

let failure_test test_number fun_to_test arg =
 flush_all ();
 try
   fun_to_test arg;
   fprintf stderr ">>> Failure expected (%s, test %d)\n"
                  !function_tested test_number;
   error ()
  with _ ->
   printf " %d..." test_number;
   success ();;

let failwith_test test_number fun_to_test arg correct_failure =
 flush_all ();
 try
   fun_to_test arg;
   fprintf stderr ">>> Failure expected (%s, test %d)\n"
                  !function_tested test_number;
   error ()
  with x ->
   if x = correct_failure then begin
     printf " %d..." test_number;
     success ()
   end else begin
     fprintf stderr ">>> Bad failure (%s, test %d)\n"
                    !function_tested test_number;
     error ()
   end;;

let end_tests () =
 flush_all ();
 print_newline ();
 if !error_occurred then begin
   print_endline "************* TESTS FAILED ****************"; exit 2
 end else begin
   print_endline "************* TESTS COMPLETED SUCCESSFULLY ****************";
   exit 0
 end;;

let eq = (==);;
let eq_int (i: int) (j: int) = (i = j);;
let eq_string (i: string) (j: string) = (i = j);;
let eq_nativeint (i: nativeint) (j: nativeint) = (i = j);;
let eq_int32 (i: int32) (j: int32) = (i = j);;
let eq_int64 (i: int64) (j: int64) = (i = j);;

let sixtyfour = (1 lsl 31) <> 0;;

let rec gcd_int i1 i2 =
  if i2 = 0 then abs i1 else gcd_int i2 (i1 mod i2);;

let rec num_bits_int_aux n =
  if n = 0 then 0 else succ(num_bits_int_aux (n lsr 1));;

let num_bits_int n = num_bits_int_aux (abs n);;

let sign_int i = if i = 0 then 0 else if i > 0 then 1 else -1;;

let length_of_int = Sys.word_size - 2;;

let monster_int = 1 lsl length_of_int;;
let biggest_int = monster_int - 1;;
let least_int = - biggest_int;;

let compare_int n1 n2 =
  if n1 == n2 then 0 else if n1 > n2 then 1 else -1;;
