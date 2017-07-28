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
open Big_int;;
open Ratio;;
open Arith_status;;

set_error_when_null_denominator false
;;

let infinite_failure = "infinite or undefined rational number";;

testing_function "create_ratio"
;;

let r = create_ratio (big_int_of_int 1) (big_int_of_int (-2)) in
test 1 eq_big_int (numerator_ratio r, big_int_of_int (-1)) &&
test 2 eq_big_int (denominator_ratio r, big_int_of_int 2)
;;

let r = create_ratio (big_int_of_int 2) (big_int_of_int 3) in
test 3 eq_big_int (numerator_ratio r, big_int_of_int 2) &&
test 4 eq_big_int (denominator_ratio r, big_int_of_int 3)
;;

set_normalize_ratio true
;;

let r = create_ratio (big_int_of_int 12) (big_int_of_int (-16)) in
test 5 eq_big_int (numerator_ratio r, big_int_of_int (-3)) &&
test 6 eq_big_int (denominator_ratio r, big_int_of_int 4)
;;

set_normalize_ratio false
;;

let r = create_ratio (big_int_of_int 0) (big_int_of_int 0) in
test 7 eq_big_int (numerator_ratio r, big_int_of_int 0) &&
test 8 eq_big_int (denominator_ratio r, big_int_of_int 0)
;;

testing_function "create_normalized_ratio"
;;

let r = create_normalized_ratio (big_int_of_int 1) (big_int_of_int (-2)) in
test 1 eq_big_int (numerator_ratio r, big_int_of_int (-1)) &&
test 2 eq_big_int (denominator_ratio r, big_int_of_int 2)
;;

let r = create_normalized_ratio (big_int_of_int 2) (big_int_of_int 3) in
test 3 eq_big_int (numerator_ratio r, big_int_of_int 2) &&
test 4 eq_big_int (denominator_ratio r, big_int_of_int 3)
;;

set_normalize_ratio true
;;

let r = create_normalized_ratio (big_int_of_int 12) (big_int_of_int (-16)) in
test 5 eq_big_int (numerator_ratio r, big_int_of_int (-12)) &&
test 6 eq_big_int (denominator_ratio r, big_int_of_int 16)
;;

set_normalize_ratio false
;;

let r = create_normalized_ratio (big_int_of_int 1) (big_int_of_int 0) in
test 7 eq_big_int (numerator_ratio r, big_int_of_int 1) &&
test 8 eq_big_int (denominator_ratio r, big_int_of_int 0)
;;

let r = create_normalized_ratio (big_int_of_int 0) (big_int_of_int 0) in
test 9 eq_big_int (numerator_ratio r, big_int_of_int 0) &&
test 10 eq_big_int (denominator_ratio r, big_int_of_int 0)
;;

testing_function "null_denominator"
;;

test 1
 eq (null_denominator (create_ratio (big_int_of_int 1) (big_int_of_int (-2))),
     false)
;;
test 2 eq
 (null_denominator (create_ratio (big_int_of_int 1) zero_big_int),true)
;;

(*****
testing_function "verify_null_denominator"
;;

test 1
 eq (verify_null_denominator (ratio_of_string "0/1"), false)
;;
test 2
 eq (verify_null_denominator (ratio_of_string "0/0"), true)
;;
*****)

testing_function "sign_ratio"
;;

test 1
eq_int (sign_ratio (create_ratio (big_int_of_int (-2)) (big_int_of_int (-3))),
        1)
;;
test 2
eq_int (sign_ratio (create_ratio (big_int_of_int 2) (big_int_of_int (-3))),
        (-1))
;;
test 3
eq_int (sign_ratio (create_ratio zero_big_int (big_int_of_int (-3))), 0)
;;

testing_function "normalize_ratio"
;;

let r = create_ratio (big_int_of_int 12) (big_int_of_int (-16)) in
ignore (normalize_ratio r);
test 1 eq_big_int (numerator_ratio r, big_int_of_int (-3)) &&
test 2 eq_big_int (denominator_ratio r, big_int_of_int 4)
;;

let r = create_ratio (big_int_of_int (-1)) zero_big_int in
ignore (normalize_ratio r);
test 3 eq_big_int (numerator_ratio r, big_int_of_int (-1)) &&
test 4 eq_big_int (denominator_ratio r, zero_big_int)
;;

testing_function "report_sign_ratio"
;;

test 1
eq_big_int (report_sign_ratio
            (create_ratio (big_int_of_int 2) (big_int_of_int (-3)))
            (big_int_of_int 1),
            big_int_of_int (-1))
;;
test 2
eq_big_int (report_sign_ratio
            (create_ratio (big_int_of_int 2) (big_int_of_int 3))
             (big_int_of_int 1),
            big_int_of_int 1)
;;

testing_function "is_integer_ratio"
;;

test 1 eq
 (is_integer_ratio (create_ratio (big_int_of_int 2) (big_int_of_int (-1))),
  true)
;;
test 2 eq
 (is_integer_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 3)),
  false)
;;

testing_function "add_ratio"
;;

let r = add_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 2))
                   (create_ratio (big_int_of_int 2) (big_int_of_int 3)) in
test 1 eq_big_int (numerator_ratio r, big_int_of_int 7) &&
test 2 eq_big_int (denominator_ratio r, big_int_of_int 6)
;;

let r = add_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 3))
                   (create_ratio (big_int_of_int 1) (big_int_of_int (-2))) in
test 3 eq_big_int (numerator_ratio r, big_int_of_int 1) &&
test 4 eq_big_int (denominator_ratio r, big_int_of_int 6)
;;

let r = add_ratio (create_ratio (big_int_of_int 2) zero_big_int)
                   (create_ratio (big_int_of_int 1) (big_int_of_int (-2))) in
test 5 eq_big_int (numerator_ratio r, big_int_of_int 4) &&
test 6 eq_big_int (denominator_ratio r, zero_big_int)
;;

let r = add_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 3))
                   (create_ratio (big_int_of_int 1) zero_big_int) in
test 7 eq_big_int (numerator_ratio r, big_int_of_int 3) &&
test 8 eq_big_int (denominator_ratio r, zero_big_int)
;;

let r = add_ratio (create_ratio (big_int_of_int 2) zero_big_int)
                   (create_ratio (big_int_of_int 1) zero_big_int) in
test 9 eq_big_int (numerator_ratio r, zero_big_int) &&
test 10 eq_big_int (denominator_ratio r, zero_big_int)
;;

let r = add_ratio (create_ratio (big_int_of_string "12724951")
                                 (big_int_of_string "26542080"))
                   (create_ratio (big_int_of_string "-1")
                                 (big_int_of_string "81749606400")) in
test 11 eq_big_int (numerator_ratio r,
                     big_int_of_string "1040259735682744320") &&
test 12 eq_big_int (denominator_ratio r,
                     big_int_of_string "2169804593037312000")
;;

let r1,r2 =
 (create_ratio (big_int_of_string "12724951")
                                 (big_int_of_string "26542080"),
                   create_ratio (big_int_of_string "-1")
                                 (big_int_of_string "81749606400")) in

let bi1 = mult_big_int (numerator_ratio r1) (denominator_ratio r2)
and  bi2 = mult_big_int (numerator_ratio r2) (denominator_ratio r1)
in
test 1
eq_big_int (bi1,
            big_int_of_string "1040259735709286400")
&&
test 2
eq_big_int (bi2,
            big_int_of_string "-26542080")
&& test 3
eq_big_int (mult_big_int (denominator_ratio r1) (denominator_ratio r2),
            big_int_of_string "2169804593037312000")
&& test 4
eq_big_int (add_big_int bi1 bi2,
            big_int_of_string "1040259735682744320")
;;

testing_function "sub_ratio"
;;

let r = sub_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 3))
                   (create_ratio (big_int_of_int 1) (big_int_of_int 2)) in
test 1  eq_big_int (numerator_ratio r, big_int_of_int 1) &&
test 2 eq_big_int (denominator_ratio r, big_int_of_int 6)
;;

let r = sub_ratio (create_ratio (big_int_of_int 2) zero_big_int)
                   (create_ratio (big_int_of_int 1) (big_int_of_int (-2))) in
test 3 eq_big_int (numerator_ratio r, big_int_of_int 4) &&
test 4 eq_big_int (denominator_ratio r, zero_big_int)
;;

let r = sub_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 3))
                   (create_ratio (big_int_of_int 1) zero_big_int) in
test 5 eq_big_int (numerator_ratio r, big_int_of_int (-3)) &&
test 6 eq_big_int (denominator_ratio r, zero_big_int)
;;

let r = sub_ratio (create_ratio (big_int_of_int 2) zero_big_int)
                   (create_ratio (big_int_of_int 1) zero_big_int) in
test 7 eq_big_int (numerator_ratio r, zero_big_int) &&
test 8 eq_big_int (denominator_ratio r, zero_big_int)
;;

testing_function "mult_ratio"
;;

let r = mult_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 3))
                    (create_ratio (big_int_of_int 7) (big_int_of_int 5)) in
test 1 eq_big_int (numerator_ratio r, big_int_of_int 14) &&
test 2 eq_big_int (denominator_ratio r, big_int_of_int 15)
;;

let r = mult_ratio (create_ratio (big_int_of_int 2) zero_big_int)
                    (create_ratio (big_int_of_int 1) (big_int_of_int (-2))) in
test 3 eq_big_int (numerator_ratio r, big_int_of_int (-2)) &&
test 4 eq_big_int (denominator_ratio r, zero_big_int)
;;

let r = mult_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 3))
                   (create_ratio (big_int_of_int 1) zero_big_int) in
test 5 eq_big_int (numerator_ratio r, big_int_of_int 2) &&
test 6 eq_big_int (denominator_ratio r, zero_big_int)
;;

let r = mult_ratio (create_ratio (big_int_of_int 2) zero_big_int)
                   (create_ratio (big_int_of_int 1) zero_big_int) in
test 7 eq_big_int (numerator_ratio r, big_int_of_int 2) &&
test 8 eq_big_int (denominator_ratio r, zero_big_int)
;;

testing_function "div_ratio"
;;

let r = div_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 3))
                   (create_ratio (big_int_of_int 5) (big_int_of_int 7)) in
test 1 eq_big_int (numerator_ratio r, big_int_of_int 14) &&
test 2 eq_big_int (denominator_ratio r, big_int_of_int 15)
;;

let r = div_ratio (create_ratio (big_int_of_int 2) zero_big_int)
                   (create_ratio (big_int_of_int 1) (big_int_of_int (-2))) in
test 3 eq_big_int (numerator_ratio r, big_int_of_int (-4)) &&
test 4 eq_big_int (denominator_ratio r, zero_big_int)
;;

let r = div_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 3))
                   (create_ratio (big_int_of_int 1) zero_big_int) in
test 5 eq_big_int (numerator_ratio r, zero_big_int) &&
test 6 eq_big_int (denominator_ratio r, big_int_of_int 3)
;;

let r = div_ratio (create_ratio (big_int_of_int 2) zero_big_int)
                   (create_ratio (big_int_of_int 1) zero_big_int) in
test 7 eq_big_int (numerator_ratio r, zero_big_int) &&
test 8 eq_big_int (denominator_ratio r, zero_big_int)
;;

testing_function "integer_ratio"
;;

test 1
eq_big_int (integer_ratio
            (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
            big_int_of_int 1)
;;
test 2
eq_big_int (integer_ratio
            (create_ratio (big_int_of_int 5) (big_int_of_int (-3))),
            big_int_of_int (-1))
;;
test 3
eq_big_int (integer_ratio
            (create_ratio (big_int_of_int 3) (big_int_of_int 2)),
            big_int_of_int 1)
;;
test 4
eq_big_int (integer_ratio
            (create_ratio (big_int_of_int 3) (big_int_of_int (-2))),
            big_int_of_int (-1))
;;

failwith_test 5
integer_ratio (create_ratio (big_int_of_int 3) zero_big_int)
(Failure("integer_ratio "^infinite_failure))
;;

testing_function "floor_ratio"
;;

test 1
eq_big_int (floor_ratio
            (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
            big_int_of_int 1)
;;
test 2
eq_big_int (floor_ratio
            (create_ratio (big_int_of_int 5) (big_int_of_int (-3))),
            big_int_of_int (-2))
;;
test 3
eq_big_int (floor_ratio
            (create_ratio (big_int_of_int 3) (big_int_of_int 2)),
            big_int_of_int 1)
;;
test 4
eq_big_int (floor_ratio
            (create_ratio (big_int_of_int 3) (big_int_of_int (-2))),
            big_int_of_int (-2))
;;

failwith_test 5 floor_ratio (create_ratio (big_int_of_int 3) zero_big_int)
Division_by_zero
;;


testing_function "round_ratio"
;;

test 1
eq_big_int (round_ratio
            (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
            big_int_of_int 2)
;;
test 2
eq_big_int (round_ratio
            (create_ratio (big_int_of_int 5) (big_int_of_int (-3))),
            big_int_of_int (-2))
;;
test 3
eq_big_int (round_ratio
            (create_ratio (big_int_of_int 3) (big_int_of_int 2)),
            big_int_of_int 2)
;;
test 4
eq_big_int (round_ratio
            (create_ratio (big_int_of_int 3) (big_int_of_int (-2))),
            big_int_of_int (-2))
;;

failwith_test 5
round_ratio (create_ratio (big_int_of_int 3) zero_big_int)
Division_by_zero
;;


testing_function "ceiling_ratio"
;;

test 1
eq_big_int (ceiling_ratio
            (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
            big_int_of_int 2)
;;
test 2
eq_big_int (ceiling_ratio
            (create_ratio (big_int_of_int 5) (big_int_of_int (-3))),
            big_int_of_int (-1))
;;
test 3
eq_big_int (ceiling_ratio
            (create_ratio (big_int_of_int 3) (big_int_of_int 2)),
            big_int_of_int 2)
;;
test 4
eq_big_int (ceiling_ratio
            (create_ratio (big_int_of_int 3) (big_int_of_int (-2))),
            big_int_of_int (-1))
;;
test 5
eq_big_int (ceiling_ratio
            (create_ratio (big_int_of_int 4) (big_int_of_int 2)),
            big_int_of_int 2)
;;
failwith_test 6
ceiling_ratio (create_ratio (big_int_of_int 3) zero_big_int)
Division_by_zero
;;

testing_function "eq_ratio"
;;

test 1
eq_ratio (create_ratio (big_int_of_int 5) (big_int_of_int 3),
          create_ratio (big_int_of_int (-20)) (big_int_of_int (-12)))
;;
test 2
eq_ratio (create_ratio (big_int_of_int 1) zero_big_int,
          create_ratio (big_int_of_int 2) zero_big_int)
;;

let neq_ratio x y = not (eq_ratio x y);;

test 3
neq_ratio (create_ratio (big_int_of_int 1) zero_big_int,
           create_ratio (big_int_of_int (-1)) zero_big_int)
;;
test 4
neq_ratio (create_ratio (big_int_of_int 1) zero_big_int,
           create_ratio zero_big_int zero_big_int)
;;
test 5
eq_ratio (create_ratio zero_big_int zero_big_int,
          create_ratio zero_big_int zero_big_int)
;;

testing_function "compare_ratio"
;;

test 1
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 0)),
        0)
;;
test 2
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 1) (big_int_of_int 0)),
        0)
;;
test 3
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 0))
                       (create_ratio (big_int_of_int (-1)) (big_int_of_int 0)),
        0)
;;
test 4
eq_int (compare_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 0)),
        0)
;;
test 5
eq_int (compare_ratio (create_ratio (big_int_of_int (-1)) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 0)),
        0)
;;
test 6
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
        0)
;;
test 7
eq_int (compare_ratio (create_ratio (big_int_of_int 5) (big_int_of_int 3))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 0)),
        0)
;;
test 8
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 0))
                       (create_ratio (big_int_of_int (-5)) (big_int_of_int 3)),
        0)
;;
test 9
eq_int (compare_ratio (create_ratio (big_int_of_int (-5)) (big_int_of_int 3))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 0)),
        0)
;;
test 10
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 1)),
        0)
;;
test 11
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 1))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 0)),
        0)
;;
test 12
eq_int (compare_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 1) (big_int_of_int 0)),
        0)
;;
test 13
eq_int (compare_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 2) (big_int_of_int 0)),
        0)
;;
test 14
eq_int (compare_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 0))
                       (create_ratio (big_int_of_int (-1)) (big_int_of_int 0)),
        1)
;;
test 15
eq_int (compare_ratio (create_ratio (big_int_of_int (-1)) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 1) (big_int_of_int 0)),
        (-1))
;;
test 16
eq_int (compare_ratio (create_ratio (big_int_of_int 5) (big_int_of_int 3))
                       (create_ratio (big_int_of_int 1) (big_int_of_int 0)),
        (-1))
;;
test 17
eq_int (compare_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
        1)
;;
test 18
eq_int (compare_ratio (create_ratio (big_int_of_int (-5)) (big_int_of_int 3))
                       (create_ratio (big_int_of_int 1) (big_int_of_int 0)),
        (-1))
;;
test 19
eq_int (compare_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 0))
                       (create_ratio (big_int_of_int (-5)) (big_int_of_int 3)),
        1)
;;
test 20
eq_int (compare_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 3)),
        1)
;;
test 21
eq_int (compare_ratio (create_ratio (big_int_of_int (-1)) (big_int_of_int 0))
                       (create_ratio (big_int_of_int (-1)) (big_int_of_int 0)),
        0)
;;
test 22
eq_int (compare_ratio (create_ratio (big_int_of_int (-1)) (big_int_of_int 0))
                       (create_ratio (big_int_of_int (-2)) (big_int_of_int 0)),
        0)
;;
test 23
eq_int (compare_ratio (create_ratio (big_int_of_int 5) (big_int_of_int 3))
                       (create_ratio (big_int_of_int (-1)) (big_int_of_int 0)),
        1)
;;
test 24
eq_int (compare_ratio (create_ratio (big_int_of_int (-1)) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
        (-1))
;;
test 25
eq_int (compare_ratio (create_ratio (big_int_of_int (-5)) (big_int_of_int 3))
                       (create_ratio (big_int_of_int (-1)) (big_int_of_int 0)),
        1)
;;
test 26
eq_int (compare_ratio (create_ratio (big_int_of_int (-1)) (big_int_of_int 0))
                       (create_ratio (big_int_of_int (-5)) (big_int_of_int 3)),
        (-1))
;;
test 27
eq_int (compare_ratio (create_ratio (big_int_of_int (-1)) (big_int_of_int 0))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 3)),
        (-1))
;;
test 28
eq_int (compare_ratio (create_ratio (big_int_of_int 5) (big_int_of_int 3))
                       (create_ratio (big_int_of_int 3) (big_int_of_int 2)),
        1)
;;
test 29
eq_int (compare_ratio (create_ratio (big_int_of_int 3) (big_int_of_int 2))
                       (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
        (-1))
;;
test 30
eq_int (compare_ratio (create_ratio (big_int_of_int 5) (big_int_of_int 3))
                       (create_ratio (big_int_of_int (-3)) (big_int_of_int 2)),
        1)
;;
test 31
eq_int (compare_ratio (create_ratio (big_int_of_int (-3)) (big_int_of_int 2))
                       (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
        (-1))
;;
test 32
eq_int (compare_ratio (create_ratio (big_int_of_int 3) (big_int_of_int 2))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 3)),
        1)
;;
test 33
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 2))
                       (create_ratio (big_int_of_int 5) (big_int_of_int 3)),
        (-1))
;;
test 34
eq_int (compare_ratio (create_ratio (big_int_of_int (-3)) (big_int_of_int 2))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 3)),
        (-1))
;;
test 35
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 2))
                       (create_ratio (big_int_of_int (-5)) (big_int_of_int 3)),
        1)
;;
test 36
eq_int (compare_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 2))
                       (create_ratio (big_int_of_int 0) (big_int_of_int 3)),
        0)
;;

testing_function "eq_big_int_ratio"
;;

test 1
eq_big_int_ratio (big_int_of_int 3,
                  (create_ratio (big_int_of_int 3) (big_int_of_int 1)))
;;
test 2
eq
(not (eq_big_int_ratio (big_int_of_int 1)
                       (create_ratio (big_int_of_int 3) (big_int_of_int 1))),
true)
;;

test 3
eq
(not (eq_big_int_ratio (big_int_of_int 1)
                       (create_ratio (big_int_of_int 3) (big_int_of_int 2))),
 true)
;;

test 4
eq
(not (eq_big_int_ratio (big_int_of_int 1)
                       (create_ratio (big_int_of_int 3) (big_int_of_int 0))),
 true)
;;

test 5
eq
(not (eq_big_int_ratio (big_int_of_int 1)
                       (create_ratio (big_int_of_int (-3)) (big_int_of_int 2))),
 true)
;;

testing_function "compare_big_int_ratio"
;;

test 1
eq_int (compare_big_int_ratio
           (big_int_of_int 1)
            (create_ratio (big_int_of_int 3) (big_int_of_int 0)), (-1))
;;
test 2
eq_int (compare_big_int_ratio
           (big_int_of_int 1)
            (create_ratio (big_int_of_int 0) (big_int_of_int 0)), 0)
;;
test 3
eq_int (compare_big_int_ratio
           (big_int_of_int 1)
            (create_ratio (big_int_of_int (-3)) (big_int_of_int 0)), 1)
;;
test 4
eq_int (compare_big_int_ratio
           (big_int_of_int (-1))
            (create_ratio (big_int_of_int 3) (big_int_of_int 0)), (-1))
;;
test 5
eq_int (compare_big_int_ratio
           (big_int_of_int (-1))
            (create_ratio (big_int_of_int 0) (big_int_of_int 0)), 0)
;;
test 6
eq_int (compare_big_int_ratio
           (big_int_of_int (-1))
            (create_ratio (big_int_of_int (-3)) (big_int_of_int 0)), 1)
;;
test 7
eq_int (compare_big_int_ratio
           (big_int_of_int 1)
            (create_ratio (big_int_of_int 1) (big_int_of_int 1)), 0)
;;
test 8
eq_int (compare_big_int_ratio
           (big_int_of_int 1)
            (create_ratio (big_int_of_int 3) (big_int_of_int 2)), (-1))
;;
test 9
eq_int (compare_big_int_ratio
           (big_int_of_int 1)
            (create_ratio (big_int_of_int 2) (big_int_of_int 3)), 1)
;;



testing_function "int_of_ratio"
;;

test 1
eq_int (int_of_ratio (create_ratio (big_int_of_int 4) (big_int_of_int 2)),
        2)
;;

test 2
eq_int (int_of_ratio
        (create_ratio (big_int_of_int biggest_int) (big_int_of_int 1)),
        biggest_int)
;;

failwith_test 3
int_of_ratio (create_ratio (big_int_of_int 4) (big_int_of_int 0))
(Failure "integer argument required")
;;

failwith_test 4
int_of_ratio (create_ratio (succ_big_int (big_int_of_int biggest_int))
                             (big_int_of_int 1))
(Failure "integer argument required")
;;

failwith_test 5
int_of_ratio (create_ratio (big_int_of_int 4) (big_int_of_int 3))
(Failure "integer argument required")
;;

testing_function "ratio_of_int"
;;

test 1
eq_ratio (ratio_of_int 3,
          create_ratio (big_int_of_int 3) (big_int_of_int 1))
;;

test 2
eq_ratio (ratio_of_nat (nat_of_int 2),
          create_ratio (big_int_of_int 2) (big_int_of_int 1))
;;

testing_function "nat_of_ratio"
;;

let nat1 = nat_of_ratio (create_ratio (big_int_of_int 3) (big_int_of_int 1))
and nat2 = nat_of_int 3 in
test 1
eq (eq_nat nat1 0 (length_nat nat1) nat2 0 (length_nat nat2), true)
;;

failwith_test 2
nat_of_ratio (create_ratio (big_int_of_int 3) (big_int_of_int 0))
(Failure "nat_of_ratio")
;;

failwith_test 3
nat_of_ratio (create_ratio (big_int_of_int (-3)) (big_int_of_int 1))
(Failure "nat_of_ratio")
;;

failwith_test 4
nat_of_ratio (create_ratio (big_int_of_int 3) (big_int_of_int 2))
(Failure "nat_of_ratio")
;;

testing_function "ratio_of_big_int"
;;

test 1
eq_ratio (ratio_of_big_int (big_int_of_int 3),
          create_ratio (big_int_of_int 3) (big_int_of_int 1))
;;

testing_function "big_int_of_ratio"
;;

test 1
eq_big_int (big_int_of_ratio
                (create_ratio (big_int_of_int 3) (big_int_of_int 1)),
            big_int_of_int 3)
;;
test 2
eq_big_int (big_int_of_ratio
                (create_ratio (big_int_of_int (-3)) (big_int_of_int 1)),
            big_int_of_int (-3))
;;

failwith_test 3
big_int_of_ratio (create_ratio (big_int_of_int 3) (big_int_of_int 0))
(Failure "big_int_of_ratio")
;;

testing_function "string_of_ratio"
;;

test 1
eq_string (string_of_ratio
              (create_ratio (big_int_of_int 43) (big_int_of_int 35)),
           "43/35")
;;
test 2
eq_string (string_of_ratio
              (create_ratio (big_int_of_int 42) (big_int_of_int 0)),
           "1/0")
;;

set_normalize_ratio_when_printing false
;;

test 3
eq_string (string_of_ratio
              (create_ratio (big_int_of_int 42) (big_int_of_int 35)),
           "42/35")
;;

set_normalize_ratio_when_printing true
;;

test 4
eq_string (string_of_ratio
              (create_ratio (big_int_of_int 42) (big_int_of_int 35)),
           "6/5")
;;

testing_function "ratio_of_string"
;;

test 1
eq_ratio (ratio_of_string ("123/3456"),
          create_ratio (big_int_of_int 123) (big_int_of_int 3456))
;;

(***********
test 2
eq_ratio (ratio_of_string ("12.3/34.56"),
          create_ratio (big_int_of_int 1230) (big_int_of_int 3456))
;;
test 3
eq_ratio (ratio_of_string ("1.23/325.6"),
          create_ratio (big_int_of_int 123) (big_int_of_int 32560))
;;
test 4
eq_ratio (ratio_of_string ("12.3/345.6"),
          create_ratio (big_int_of_int 123) (big_int_of_int 3456))
;;
test 5
eq_ratio (ratio_of_string ("12.3/0.0"),
          create_ratio (big_int_of_int 123) (big_int_of_int 0))
;;
***********)
test 6
eq_ratio (ratio_of_string ("0/0"),
          create_ratio (big_int_of_int 0) (big_int_of_int 0))
;;

test 7
eq_ratio (ratio_of_string "1234567890",
          create_ratio (big_int_of_string "1234567890") unit_big_int)
;;
failwith_test 8
ratio_of_string "frlshjkurty" (Failure "invalid digit");;

(***********
testing_function "msd_ratio"
;;

test 1
eq_int (msd_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 1)),
        0)
;;
test 2
eq_int (msd_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 12)),
        (-2))
;;
test 3
eq_int (msd_ratio (create_ratio (big_int_of_int 12) (big_int_of_int 1)),
        1)
;;
test 4
eq_int (msd_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 2)),
        (-1))
;;
test 5
eq_int (msd_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 1)),
        0)
;;
test 6
eq_int (msd_ratio (create_ratio (big_int_of_int 25) (big_int_of_int 21)),
        0)
;;
test 7
eq_int (msd_ratio (create_ratio (big_int_of_int 35) (big_int_of_int 21)),
        0)
;;
test 8
eq_int (msd_ratio (create_ratio (big_int_of_int 215) (big_int_of_int 31)),
        0)
;;
test 9
eq_int (msd_ratio (create_ratio (big_int_of_int 2) (big_int_of_int 30)),
        (-2))
;;
test 10
eq_int (msd_ratio (create_ratio (big_int_of_int 2345)
                                     (big_int_of_int 23456)),
        (-2))
;;
test 11
eq_int (msd_ratio (create_ratio (big_int_of_int 2345)
                                     (big_int_of_int 2346)),
        (-1))
;;
test 12
eq_int (msd_ratio (create_ratio (big_int_of_int 2345)
                                     (big_int_of_int 2344)),
        0)
;;
test 13
eq_int (msd_ratio (create_ratio (big_int_of_int 23456)
                                     (big_int_of_int 2345)),
        1)
;;
test 14
eq_int (msd_ratio (create_ratio (big_int_of_int 23467)
                                     (big_int_of_int 2345)),
        1)
;;
failwith_test 15
msd_ratio (create_ratio (big_int_of_int 1) (big_int_of_int 0))
("msd_ratio "^infinite_failure)
;;
failwith_test 16
msd_ratio (create_ratio (big_int_of_int (-1)) (big_int_of_int 0))
("msd_ratio "^infinite_failure)
;;
failwith_test 17
msd_ratio (create_ratio (big_int_of_int 0) (big_int_of_int 0))
("msd_ratio "^infinite_failure)
;;
*************************)

testing_function "round_futur_last_digit"
;;

let s = "+123456" in
test 1 eq (round_futur_last_digit s 1 (pred (String.length s)),
            false) &&
test 2 eq_string (s, "+123466")
;;

let s = "123456" in
test 3 eq (round_futur_last_digit s 0 (String.length s), false) &&
test 4 eq_string (s, "123466")
;;

let s = "-123456" in
test 5 eq (round_futur_last_digit s 1 (pred (String.length s)),
            false) &&
test 6 eq_string (s, "-123466")
;;

let s = "+123496" in
test 7 eq (round_futur_last_digit s 1 (pred (String.length s)),
            false) &&
test 8 eq_string (s, "+123506")
;;

let s = "123496" in
test 9 eq (round_futur_last_digit s 0 (String.length s), false) &&
test 10 eq_string (s, "123506")
;;

let s = "-123496" in
test 11 eq (round_futur_last_digit s 1 (pred (String.length s)),
            false) &&
test 12 eq_string (s, "-123506")
;;

let s = "+996" in
test 13 eq (round_futur_last_digit s 1 (pred (String.length s)),
            true) &&
test 14 eq_string (s, "+006")
;;

let s = "996" in
test 15 eq (round_futur_last_digit s 0 (String.length s), true) &&
test 16 eq_string (s, "006")
;;

let s = "-996" in
test 17 eq (round_futur_last_digit s 1 (pred (String.length s)),
             true) &&
test 18 eq_string (s, "-006")
;;

let s = "+6666666" in
test 19 eq (round_futur_last_digit s 1 (pred (String.length s)),
             false) &&
test 20 eq_string (s, "+6666676")
;;

let s = "6666666" in
test 21 eq (round_futur_last_digit s 0 (String.length s), false) &&
test 22 eq_string (s, "6666676")
;;

let s = "-6666666" in
test 23 eq (round_futur_last_digit s 1 (pred (String.length s)),
             false) &&
test 24 eq_string (s, "-6666676")
;;

testing_function "approx_ratio_fix"
;;

let s = approx_ratio_fix 5
                          (create_ratio (big_int_of_int 2)
                                        (big_int_of_int 3)) in
test 1
eq_string (s, "+0.66667")
;;

test 2
eq_string (approx_ratio_fix 5
                             (create_ratio (big_int_of_int 20)
                                           (big_int_of_int 3)),
           "+6.66667")
;;
test 3
eq_string (approx_ratio_fix 5
                             (create_ratio (big_int_of_int 2)
                                           (big_int_of_int 30)),
           "+0.06667")
;;
test 4
eq_string (approx_ratio_fix 5
                             (create_ratio (big_int_of_string "999996")
                                           (big_int_of_string "1000000")),
           "+1.00000")
;;
test 5
eq_string (approx_ratio_fix 5
                             (create_ratio (big_int_of_string "299996")
                                           (big_int_of_string "100000")),
           "+2.99996")
;;
test 6
eq_string (approx_ratio_fix 5
                             (create_ratio (big_int_of_string "2999996")
                                           (big_int_of_string "1000000")),
           "+3.00000")
;;
test 7
eq_string (approx_ratio_fix 4
                             (create_ratio (big_int_of_string "299996")
                                           (big_int_of_string "100000")),
           "+3.0000")
;;
test 8
eq_string (approx_ratio_fix 5
                             (create_ratio (big_int_of_int 29996)
                                           (big_int_of_string "100000")),
           "+0.29996")
;;
test 9
eq_string (approx_ratio_fix 5
                             (create_ratio (big_int_of_int 0)
                                           (big_int_of_int 1)),
           "+0")
;;
failwith_test 10
(approx_ratio_fix 5) (create_ratio (big_int_of_int 1) (big_int_of_int 0))
(Failure "approx_ratio_fix infinite or undefined rational number")
;;
failwith_test 11
(approx_ratio_fix 5) (create_ratio (big_int_of_int 0) (big_int_of_int 0))
(Failure "approx_ratio_fix infinite or undefined rational number")
;;

(* PR#4566 *)
test 12
eq_string (approx_ratio_fix 8
                            (create_ratio (big_int_of_int 9603)
                                          (big_int_of_string "100000000000")),

          "+0.00000010")
;;
test 13
eq_string (approx_ratio_fix 1
                            (create_ratio (big_int_of_int 94)
                                          (big_int_of_int 1000)),
          "+0.1")
;;
test 14
eq_string (approx_ratio_fix 1
                            (create_ratio (big_int_of_int 49)
                                          (big_int_of_int 1000)),
          "+0.0")
;;

testing_function "approx_ratio_exp"
;;

test 1
eq_string (approx_ratio_exp 5
                             (create_ratio (big_int_of_int 2)
                                           (big_int_of_int 3)),
           "+0.66667e0")
;;
test 2
eq_string (approx_ratio_exp 5
                             (create_ratio (big_int_of_int 20)
                                           (big_int_of_int 3)),
           "+0.66667e1")
;;
test 3
eq_string (approx_ratio_exp 5
                             (create_ratio (big_int_of_int 2)
                                           (big_int_of_int 30)),
           "+0.66667e-1")
;;
test 4
eq_string (approx_ratio_exp 5
                             (create_ratio (big_int_of_string "999996")
                                           (big_int_of_string "1000000")),
           "+1.00000e0")
;;
test 5
eq_string (approx_ratio_exp 5
                             (create_ratio (big_int_of_string "299996")
                                           (big_int_of_string "100000")),
           "+0.30000e1")
;;
test 6
eq_string (approx_ratio_exp 5
                             (create_ratio (big_int_of_int 29996)
                                           (big_int_of_string "100000")),
           "+0.29996e0")
;;
test 7
eq_string (approx_ratio_exp 5
                             (create_ratio (big_int_of_int 0)
                                           (big_int_of_int 1)),
           "+0.00000e0")
;;
failwith_test 8
(approx_ratio_exp 5) (create_ratio (big_int_of_int 1) (big_int_of_int 0))
(Failure "approx_ratio_exp infinite or undefined rational number")
;;
failwith_test 9
(approx_ratio_exp 5) (create_ratio (big_int_of_int 0) (big_int_of_int 0))
(Failure "approx_ratio_exp infinite or undefined rational number")
;;
