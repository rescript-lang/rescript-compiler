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

open Test;;
open Nat;;

(* Can compare nats less than 2**32 *)
let equal_nat n1 n2 =
 eq_nat n1 0 (num_digits_nat n1 0 1)
        n2 0 (num_digits_nat n2 0 1);;

testing_function "num_digits_nat";;

test (-1) eq (false,not true);;
test 0 eq (true,not false);;

test 1
eq_int
(let r = make_nat 2 in
  set_digit_nat r 1 1;
  num_digits_nat r 0 1,1);;

testing_function "length_nat";;

test 1
eq_int
(let r = make_nat 2 in
  set_digit_nat r 0 1;
  length_nat r,2);;

testing_function "equal_nat";;

let zero_nat = make_nat 1 in

test 1
equal_nat (zero_nat,zero_nat);;
test 2
equal_nat (nat_of_int 1,nat_of_int 1);;

test 3
equal_nat (nat_of_string "2",nat_of_string "2");;
test 4
eq (equal_nat (nat_of_string "2")(nat_of_string "3"),false);;

testing_function "incr_nat";;

let zero = nat_of_int 0 in
let res = incr_nat zero 0 1 1 in
 test 1
  equal_nat (zero, nat_of_int 1) &&
 test 2
  eq (res,0);;

let n = nat_of_int 1 in
let res = incr_nat n 0 1 1 in
 test 3
  equal_nat (n, nat_of_int 2) &&
 test 4
  eq (res,0);;


testing_function "decr_nat";;

let n = nat_of_int 1 in
let res = decr_nat n 0 1 0 in
 test 1
  equal_nat (n, nat_of_int 0) &&
 test 2
  eq (res,1);;

let n = nat_of_int 2 in
let res = decr_nat n 0 1 0 in
 test 3
  equal_nat (n, nat_of_int 1) &&
 test 4
  eq (res,1);;

testing_function "is_zero_nat";;

let n = nat_of_int 1 in
test 1 eq (is_zero_nat n 0 1,false) &&
test 2 eq (is_zero_nat (make_nat 1) 0 1, true) &&
test 3 eq (is_zero_nat (make_nat 2) 0 2, true) &&
(let r = make_nat 2 in
  set_digit_nat r 1 1;
  test 4 eq (is_zero_nat r 0 1, true))
;;

testing_function "string_of_nat";;

let n = make_nat 4;;

test 1 eq_string (string_of_nat n, "0");;

complement_nat n 0 (if sixtyfour then 2 else 4);;

test 2 eq_string (string_of_nat n, "340282366920938463463374607431768211455");;

testing_function "string_of_nat && nat_of_string";;

for i = 1 to 20 do
  let s = String.make i '0' in
  String.set s 0 '1';
  ignore (test i eq_string (string_of_nat (nat_of_string s), s))
done;;

let set_mult_digit_nat n1 d1 l1 n2 d2 l2 n3 d3 =
  ignore (mult_digit_nat n1 d1 l1 n2 d2 l2 n3 d3)
;;

let s =
  "33333333333333333333333333333333333333333333333333333333333333333333\
   33333333333333333333333333333333333333333333333333333333333333333333"
in
test 21 equal_nat (
nat_of_string s,
(let nat = make_nat 15 in
  set_digit_nat nat 0 3;
  set_mult_digit_nat nat 0 15
                 (nat_of_string (String.sub s 0 135)) 0 14
                 (nat_of_int 10) 0;
  nat))
;;

test 22 eq_string (string_of_nat(nat_of_string "1073741824"), "1073741824");;

testing_function "gcd_nat";;

for i = 1 to 20 do
  let n1 = Random.int 1000000000
  and n2 = Random.int 100000 in
  let nat1 = nat_of_int n1
  and nat2 = nat_of_int n2 in
  ignore (gcd_nat nat1 0 1 nat2 0 1);
  ignore (test i eq (int_of_nat nat1, gcd_int n1 n2))
done
;;

testing_function "sqrt_nat";;

test 1 equal_nat (sqrt_nat (nat_of_int 1) 0 1, nat_of_int 1);;
test 2 equal_nat (let n = nat_of_string "8589934592" in
                  sqrt_nat n 0 (length_nat n),
                  nat_of_string "92681");;
test 3 equal_nat (let n = nat_of_string "4294967295" in
                  sqrt_nat n 0 (length_nat n),
                  nat_of_string "65535");;
test 4 equal_nat (let n = nat_of_string "18446744065119617025" in
                  sqrt_nat n 0 (length_nat n),
                  nat_of_string "4294967295");;
test 5 equal_nat (sqrt_nat (nat_of_int 15) 0 1,
                  nat_of_int 3);;
