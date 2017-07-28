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
open Num;;
open Arith_status;;

testing_function "add_num";;

test 1
eq_num (add_num (Int 1) (Int 3), Int 4);;
test 2
eq_num (add_num (Int 1) (Big_int (big_int_of_int 3)), Int 4);;
test 3
eq_num (add_num (Int 1) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "7/4"));;
test 4
eq_num (add_num (Big_int (big_int_of_int 1)) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "7/4"));;
test 5
eq_num (add_num (Big_int (big_int_of_int 1)) (Big_int (big_int_of_int 3)),
        Int 4);;
test 6
eq_num (add_num (Big_int (big_int_of_int 1)) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "7/4"));;
test 7
eq_num (add_num (Ratio (ratio_of_string "2/3")) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "17/12"));;
test 8
eq_num (add_num (Int least_int) (Int 1),
        Int (- (pred biggest_int)));;
test 9
eq_num (add_num (Int biggest_int) (Int 1),
        Big_int (minus_big_int (pred_big_int (big_int_of_int least_int))));;

testing_function "sub_num";;

test 1
eq_num (sub_num (Int 1) (Int 3), Int (-2));;
test 2
eq_num (sub_num (Int 1) (Big_int (big_int_of_int 3)), Int (-2));;
test 3
eq_num (sub_num (Int 1) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "1/4"));;
test 4
eq_num (sub_num (Big_int (big_int_of_int 1)) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "1/4"));;
test 5
eq_num (sub_num (Big_int (big_int_of_int 1)) (Big_int (big_int_of_int 3)),
        Int (-2));;
test 7
eq_num (sub_num (Big_int (big_int_of_int 1)) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "1/4"));;
test 8
eq_num (sub_num (Ratio (ratio_of_string "2/3")) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "-1/12"));;
test 9
eq_num (sub_num (Int least_int) (Int (-1)),
        Int (- (pred biggest_int)));;
test 10
eq_num (sub_num (Int (-1)) (Int biggest_int), pred_num (Int least_int));;

testing_function "mult_num";;

test 1
eq_num (mult_num (Int 2) (Int 3), Int 6);;
test 2
eq_num (mult_num (Int 127) (Int (int_of_string "257")),
                  Int (int_of_string "32639"));;
test 3
eq_num (mult_num (Int 257) (Int (int_of_string "260")),
        Big_int (big_int_of_string "66820"));;
test 4
eq_num (mult_num (Int 2) (Big_int (big_int_of_int 3)), Int 6);;
test 5
eq_num (mult_num (Int 10) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "15/2"));;
test 6
eq_num (mult_num (Big_int (big_int_of_int 10)) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "15/2"));;
test 7
eq_num (mult_num (Big_int (big_int_of_int 2)) (Big_int (big_int_of_int 3)),
        Int 6);;
test 8
eq_num (mult_num (Big_int (big_int_of_int 10)) (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "15/2"));;
test 9
eq_num (mult_num (Ratio (ratio_of_string "2/3")) (Ratio (ratio_of_string "3/4"))
               , Ratio (ratio_of_string "1/2"));;

testing_function "div_num";;

test 1
eq_num (div_num (Int 6) (Int 3), Int 2);;
test 2
eq_num (div_num (Int (int_of_string "32639"))
                 (Int (int_of_string "257")), Int 127);;
test 3
eq_num (div_num (Big_int (big_int_of_string "66820"))
                 (Int (int_of_string "257")),
        Int 260);;
test 4
eq_num (div_num (Int 6) (Big_int (big_int_of_int 3)), Int 2);;
test 5
eq_num (div_num (Ratio (ratio_of_string "15/2"))
                 (Int 10),
        Ratio (ratio_of_string "3/4"));;
test 6
eq_num (div_num (Big_int (big_int_of_int 6)) (Big_int (big_int_of_int 3)),
        Int 2);;
test 7
eq_num (div_num (Ratio (ratio_of_string "15/2"))
                 (Big_int (big_int_of_int 10)),
        Ratio (ratio_of_string "3/4"));;
test 8
eq_num (div_num (Ratio (ratio_of_string "15/2"))
                 (Ratio (ratio_of_string "3/4")),
        Big_int (big_int_of_int 10));;
test 9
eq_num (div_num (Ratio (ratio_of_string "1/2"))
                 (Ratio (ratio_of_string "3/4")),
        Ratio (ratio_of_string "2/3"));;

testing_function "is_integer_num";;

test 1
eq (is_integer_num (Int 3),true);;
test 2
eq (is_integer_num (Big_int (big_int_of_string "1234567890")),true);;
test 3
eq (not (is_integer_num (Ratio (ratio_of_string "1/2"))),true);;
test 4
eq (is_integer_num (Ratio (ratio_of_string "1073774590/32770")),true);;

testing_function "num_of_ratio";;

test 1
eq_num (num_of_ratio (ratio_of_string "4/2"), Int 2);;
test 2
eq_num (num_of_ratio (ratio_of_string "11811160075/11"),
        Big_int (big_int_of_string "1073741825"));;
test 3
eq_num (num_of_ratio (ratio_of_string "123456789012/1234"),
        Ratio (ratio_of_string "61728394506/617"));;

testing_function "num_of_string";;

test 1
eq_num (num_of_string "123/3456", Ratio (ratio_of_string "123/3456"));;
(*********
test 2
eq_num (num_of_string "12.3/34.56", Ratio (ratio_of_string "1230/3456"));;
test 3
eq_num (num_of_string "1.23/325.6", Ratio (ratio_of_string "123/32560"));;
test 4
eq_num (num_of_string "12.3/345.6", Ratio (ratio_of_string "123/3456"));;
set_error_when_null_denominator false;;
test 5
eq_num (num_of_string "12.3/0.0", Ratio (ratio_of_string "123/0"));;
test 6
eq_num (num_of_string "0/0", Ratio (ratio_of_string "0/0"));;
set_error_when_null_denominator true;;
*********)
test 7
eq_num (num_of_string "1234567890",
        Big_int (big_int_of_string "1234567890"));;
test 8
eq_num (num_of_string "12345", Int (int_of_string "12345"));;
(*********
test 9
eq_num (num_of_string "0.23", Ratio (ratio_of_string "23/100"));;
test 10
eq_num (num_of_string "0.23", Ratio (ratio_of_string "0.23/1"));;
********)

failwith_test 11
num_of_string ("frlshjkurty") (Failure "num_of_string");;

test 12
eq_num (num_of_string "0xAbCdEf",
        Big_int (big_int_of_int 0xabcdef));;

test 13
eq_num (num_of_string "0b1101/0O1765",
        Ratio (ratio_of_string "0b1101/0o1765"));;

test 14
eq_num (num_of_string "-12_34_56",
        Big_int (big_int_of_int (-123456)));;

test 15
eq_num (num_of_string "0B101010", Big_int (big_int_of_int 42));;

(*******

testing_function "immediate numbers";;

standard arith false;;

let x = (1/2) in
test 0 eq_string (string_of_num x, "1/2");;

let y = 12345678901 in
test 1 eq_string (string_of_num y, "12345678901");;
testing_function "immediate numbers";;

let x = (1/2) in
test 0 eq_string (string_of_num x, "1/2");;

let y = 12345678901 in
test 1 eq_string (string_of_num y, "12345678901");;

testing_function "pattern_matching on nums";;

let f1 = function 0 -> true | _  -> false;;

test 1 eq (f1 0, true);;

test 2 eq (f1 1, false);;

test 3 eq (f1 (0/1), true);;

test 4 eq (f1 (let n = num_of_string "2000000000000000000000000" in n-n) ,
            true);;

test 5 eq (f1 (let n = num_of_string "2000000000000000000000000" in n/n-1) ,
            true);;

test 6 eq (f1 (let n = num_of_string "2000000000000000000000000" in n+1) ,
            false);;

test 7 eq (f1 (1/2), false);;

**************)
