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

open Test
open Nat
open Big_int
open Num

let intern_extern obj =
  let f = Filename.temp_file "testnum" ".data" in
  let oc = open_out_bin f in
  output_value oc obj;
  close_out oc;
  let ic = open_in_bin f in
  let res = input_value ic in
  close_in ic;
  Sys.remove f;
  res
;;

testing_function "output_value/input_value on nats";;

let equal_nat n1 n2 =
  eq_nat n1 0 (length_nat n1) n2 0 (length_nat n2)
;;

List.iter
  (fun (i, s) ->
    let n = nat_of_string s in
    ignore(test i equal_nat (n, intern_extern n)))
  [1, "0";
   2, "1234";
   3, "8589934592";
   4, "340282366920938463463374607431768211455";
   5, String.make 100 '3';
   6, String.make 1000 '9';
   7, String.make 20000 '8']
;;

testing_function "output_value/input_value on big ints";;

List.iter
  (fun (i, s) ->
    let b = big_int_of_string s in
    ignore(test i eq_big_int (b, intern_extern b)))
  [1, "0";
   2, "1234";
   3, "-1234";
   4, "1040259735709286400";
   5, "-" ^ String.make 20000 '7']
;;

testing_function "output_value/input_value on nums";;

List.iter
  (fun (i, s) ->
    let n = num_of_string s in
    ignore(test i eq_num (n, intern_extern n)))
  [1, "0";
   2, "1234";
   3, "-1234";
   4, "159873568791325097646845892426782";
   5, "1/4";
   6, "-15/2";
   7, "159873568791325097646845892426782/24098772507410987265987";
   8, String.make 10000 '3' ^ "/" ^ String.make 5000 '7']
;;
