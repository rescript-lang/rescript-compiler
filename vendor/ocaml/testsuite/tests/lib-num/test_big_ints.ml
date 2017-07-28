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
open List;;

testing_function "compare_big_int";;

test 1
eq_int (compare_big_int zero_big_int zero_big_int, 0);;
test 2
eq_int (compare_big_int zero_big_int (big_int_of_int 1), (-1));;
test 3
eq_int (compare_big_int zero_big_int (big_int_of_int (-1)), 1);;
test 4
eq_int (compare_big_int (big_int_of_int 1) zero_big_int, 1);;
test 5
eq_int (compare_big_int (big_int_of_int (-1)) zero_big_int, (-1));;
test 6
eq_int (compare_big_int (big_int_of_int 1) (big_int_of_int 1), 0);;
test 7
eq_int (compare_big_int (big_int_of_int (-1)) (big_int_of_int (-1)), 0);;
test 8
eq_int (compare_big_int (big_int_of_int 1) (big_int_of_int (-1)), 1);;
test 9
eq_int (compare_big_int (big_int_of_int (-1)) (big_int_of_int 1), (-1));;
test 10
eq_int (compare_big_int (big_int_of_int 1) (big_int_of_int 2), (-1));;
test 11
eq_int (compare_big_int (big_int_of_int 2) (big_int_of_int 1), 1);;
test 12
eq_int (compare_big_int (big_int_of_int (-1)) (big_int_of_int (-2)), 1);;
test 13
eq_int (compare_big_int (big_int_of_int (-2)) (big_int_of_int (-1)), (-1));;


testing_function "pred_big_int";;

test 1
eq_big_int (pred_big_int zero_big_int, big_int_of_int (-1));;
test 2
eq_big_int (pred_big_int unit_big_int, zero_big_int);;
test 3
eq_big_int (pred_big_int (big_int_of_int (-1)), big_int_of_int (-2));;

testing_function "succ_big_int";;

test 1
eq_big_int (succ_big_int zero_big_int, unit_big_int);;
test 2
eq_big_int (succ_big_int unit_big_int, big_int_of_int 2);;
test 3
eq_big_int (succ_big_int (big_int_of_int (-1)), zero_big_int);;

testing_function "add_big_int";;

test 1
eq_big_int (add_big_int zero_big_int zero_big_int, zero_big_int);;
test 2
eq_big_int (add_big_int zero_big_int (big_int_of_int 1),
            big_int_of_int 1);;
test 3
eq_big_int (add_big_int (big_int_of_int 1) zero_big_int,
            big_int_of_int 1);;
test 4
eq_big_int (add_big_int zero_big_int (big_int_of_int (-1)),
            big_int_of_int (-1));;
test 5
eq_big_int (add_big_int (big_int_of_int (-1)) zero_big_int,
            big_int_of_int (-1));;
test 6
eq_big_int (add_big_int (big_int_of_int 1) (big_int_of_int 1),
            big_int_of_int 2);;
test 7
eq_big_int (add_big_int (big_int_of_int 1) (big_int_of_int 2),
            big_int_of_int 3);;
test 8
eq_big_int (add_big_int (big_int_of_int 2) (big_int_of_int 1),
            big_int_of_int 3);;
test 9
eq_big_int (add_big_int (big_int_of_int (-1)) (big_int_of_int (-1)),
            big_int_of_int (-2));;
test 10
eq_big_int (add_big_int (big_int_of_int (-1)) (big_int_of_int (-2)),
            big_int_of_int (-3));;
test 11
eq_big_int (add_big_int (big_int_of_int (-2)) (big_int_of_int (-1)),
            big_int_of_int (-3));;
test 12
eq_big_int (add_big_int (big_int_of_int 1) (big_int_of_int (-1)),
            zero_big_int);;
test 13
eq_big_int (add_big_int (big_int_of_int (-1)) (big_int_of_int 1),
            zero_big_int);;
test 14
eq_big_int (add_big_int (big_int_of_int 1) (big_int_of_int (-2)),
            big_int_of_int (-1));;
test 15
eq_big_int (add_big_int (big_int_of_int (-2)) (big_int_of_int 1),
            big_int_of_int (-1));;
test 16
eq_big_int (add_big_int (big_int_of_int (-1)) (big_int_of_int 2),
            big_int_of_int 1);;
test 17
eq_big_int (add_big_int (big_int_of_int 2) (big_int_of_int (-1)),
            big_int_of_int 1);;


testing_function "sub_big_int";;

test 1
eq_big_int (sub_big_int zero_big_int zero_big_int, zero_big_int);;
test 2
eq_big_int (sub_big_int zero_big_int (big_int_of_int 1),
            big_int_of_int (-1));;
test 3
eq_big_int (sub_big_int (big_int_of_int 1) zero_big_int,
            big_int_of_int 1);;
test 4
eq_big_int (sub_big_int zero_big_int (big_int_of_int (-1)),
            big_int_of_int 1);;
test 5
eq_big_int (sub_big_int (big_int_of_int (-1)) zero_big_int,
            big_int_of_int (-1));;
test 6
eq_big_int (sub_big_int (big_int_of_int 1) (big_int_of_int 1),
            zero_big_int);;
test 7
eq_big_int (sub_big_int (big_int_of_int 1) (big_int_of_int 2),
            big_int_of_int (-1));;
test 8
eq_big_int (sub_big_int (big_int_of_int 2) (big_int_of_int 1),
            big_int_of_int 1);;
test 9
eq_big_int (sub_big_int (big_int_of_int (-1)) (big_int_of_int (-1)),
            zero_big_int);;
test 10
eq_big_int (sub_big_int (big_int_of_int (-1)) (big_int_of_int (-2)),
            big_int_of_int 1);;
test 11
eq_big_int (sub_big_int (big_int_of_int (-2)) (big_int_of_int (-1)),
            big_int_of_int (-1));;
test 12
eq_big_int (sub_big_int (big_int_of_int 1) (big_int_of_int (-1)),
            big_int_of_int 2);;
test 13
eq_big_int (sub_big_int (big_int_of_int (-1)) (big_int_of_int 1),
            big_int_of_int (-2));;
test 14
eq_big_int (sub_big_int (big_int_of_int 1) (big_int_of_int (-2)),
            big_int_of_int 3);;
test 15
eq_big_int (sub_big_int (big_int_of_int (-2)) (big_int_of_int 1),
            big_int_of_int (-3));;
test 16
eq_big_int (sub_big_int (big_int_of_int (-1)) (big_int_of_int 2),
            big_int_of_int (-3));;
test 17
eq_big_int (sub_big_int (big_int_of_int 2) (big_int_of_int (-1)),
            big_int_of_int 3);;

testing_function "mult_int_big_int";;

test 1
eq_big_int (mult_int_big_int 0 (big_int_of_int 3), zero_big_int);;
test 2
eq_big_int (mult_int_big_int 1 (big_int_of_int 3), big_int_of_int 3);;
test 3
eq_big_int (mult_int_big_int 1 zero_big_int, zero_big_int);;
test 4
eq_big_int (mult_int_big_int 2 (big_int_of_int 3), big_int_of_int 6);;

testing_function "mult_big_int";;

test 1
eq_big_int (mult_big_int zero_big_int zero_big_int,
            zero_big_int);;
test 2
eq_big_int (mult_big_int (big_int_of_int 2) (big_int_of_int 3),
            big_int_of_int 6);;
test 3
eq_big_int (mult_big_int (big_int_of_int 2) (big_int_of_int (-3)),
            big_int_of_int (-6));;
test 4
eq_big_int (mult_big_int (big_int_of_string "12724951")
                         (big_int_of_string "81749606400"),
            big_int_of_string "1040259735709286400");;
test 5
eq_big_int (mult_big_int (big_int_of_string "26542080")
                          (big_int_of_string "81749606400"),
            big_int_of_string "2169804593037312000");;

testing_function "quomod_big_int";;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 1) (big_int_of_int 1) in
 test 1 eq_big_int (quotient, big_int_of_int 1) &&
 test 2 eq_big_int (modulo, zero_big_int);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 1) (big_int_of_int (-1)) in
 test 3 eq_big_int (quotient, big_int_of_int (-1)) &&
 test 4 eq_big_int (modulo, zero_big_int);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int (-1)) (big_int_of_int 1) in
 test 5 eq_big_int (quotient, big_int_of_int (-1)) &&
 test 6 eq_big_int (modulo, zero_big_int);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 3) (big_int_of_int 2) in
 test 7 eq_big_int (quotient, big_int_of_int 1) &&
 test 8 eq_big_int (modulo, big_int_of_int 1);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 5) (big_int_of_int 3) in
 test 9 eq_big_int (quotient, big_int_of_int 1) &&
 test 10 eq_big_int (modulo, big_int_of_int 2);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int (-5)) (big_int_of_int 3) in
 test 11 eq_big_int (quotient, big_int_of_int (-2)) &&
 test 12 eq_big_int (modulo, big_int_of_int 1);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 1) (big_int_of_int 2) in
 test 13 eq_big_int (quotient, zero_big_int) &&
 test 14 eq_big_int (modulo, big_int_of_int 1);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int (-1)) (big_int_of_int 3) in
 test 15 eq_big_int (quotient, minus_big_int unit_big_int) &&
 test 16 eq_big_int (modulo, big_int_of_int 2);;

failwith_test 17
(quomod_big_int (big_int_of_int 1)) zero_big_int
Division_by_zero
;;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 10) (big_int_of_int 20) in
 test 18 eq_big_int (quotient, big_int_of_int 0) &&
 test 19 eq_big_int (modulo, big_int_of_int 10);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int (-10)) (big_int_of_int 20) in
 test 20 eq_big_int (quotient, big_int_of_int (-1)) &&
 test 21 eq_big_int (modulo, big_int_of_int 10);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int 10) (big_int_of_int (-20)) in
 test 22 eq_big_int (quotient, big_int_of_int 0) &&
 test 23 eq_big_int (modulo, big_int_of_int 10);;

let (quotient, modulo) =
      quomod_big_int (big_int_of_int (-10)) (big_int_of_int (-20)) in
 test 24 eq_big_int (quotient, big_int_of_int 1) &&
 test 25 eq_big_int (modulo, big_int_of_int 10);;


testing_function "gcd_big_int";;

test 1
eq_big_int (gcd_big_int zero_big_int zero_big_int,
            zero_big_int);;
test 2
eq_big_int (gcd_big_int zero_big_int (big_int_of_int 1),
            big_int_of_int 1);;
test 3
eq_big_int (gcd_big_int (big_int_of_int 1) zero_big_int,
            big_int_of_int 1);;
test 4
eq_big_int (gcd_big_int (big_int_of_int 1) (big_int_of_int 2),
            big_int_of_int 1);;
test 5
eq_big_int (gcd_big_int (big_int_of_int 2) (big_int_of_int 1),
            big_int_of_int 1);;
test 6
eq_big_int (gcd_big_int (big_int_of_int 1) (big_int_of_int 1),
            big_int_of_int 1);;
test 7
eq_big_int (gcd_big_int (big_int_of_int 9) (big_int_of_int 16),
            big_int_of_int 1);;
test 8
eq_big_int (gcd_big_int (big_int_of_int 12) (big_int_of_int 16),
            big_int_of_int 4);;

for i = 9 to 28 do
  let n1 = Random.int 1000000000
  and n2 = Random.int 100000 in
  let _ =
    test i eq
      (int_of_big_int (gcd_big_int (big_int_of_int n1) (big_int_of_int n2)),
       gcd_int n1 n2) in
  ()
done;;

testing_function "int_of_big_int";;

test 1
eq_int (int_of_big_int (big_int_of_int 1), 1);;
test 2
eq_int (int_of_big_int (big_int_of_int(-1)), -1);;
test 3
eq_int (int_of_big_int zero_big_int, 0);;
test 4
eq_int (int_of_big_int (big_int_of_int max_int), max_int);;
test 5
eq_int (int_of_big_int (big_int_of_int min_int), min_int);;
failwith_test 6
  (fun () -> int_of_big_int (succ_big_int (big_int_of_int max_int)))
  () (Failure "int_of_big_int");;
failwith_test 7
  (fun () -> int_of_big_int (pred_big_int (big_int_of_int min_int)))
  () (Failure "int_of_big_int");;
failwith_test 8
  (fun () -> int_of_big_int (mult_big_int (big_int_of_int min_int)
                                          (big_int_of_int 2)))
  () (Failure "int_of_big_int");;


testing_function "is_int_big_int";;

test 1
eq (is_int_big_int (big_int_of_int 1), true);;
test 2
eq (is_int_big_int (big_int_of_int (-1)), true);;
test 3
eq (is_int_big_int (succ_big_int (big_int_of_int biggest_int)), false);;
test 4
eq (int_of_big_int (big_int_of_int monster_int), monster_int);;
(* Should be true *)
test 5
eq (is_int_big_int (big_int_of_string (string_of_int biggest_int)), true);;
test 6
eq (is_int_big_int (big_int_of_string (string_of_int least_int)), true);;
test 7
eq (is_int_big_int (big_int_of_string (string_of_int monster_int)), true);;

(* Should be false *)
(* Successor of biggest_int is not an int *)
test 8
eq (is_int_big_int (succ_big_int (big_int_of_int (biggest_int))), false);;
test 9
eq (is_int_big_int
     (succ_big_int (succ_big_int (big_int_of_int (biggest_int)))), false);;
(* Negation of monster_int (as a big_int) is not an int *)
test 10
eq (is_int_big_int
     (minus_big_int (big_int_of_string (string_of_int monster_int))), false);;


testing_function "sys_string_of_big_int";;

test 1
eq_string (string_of_big_int (big_int_of_int 1), "1");;


testing_function "big_int_of_string";;

test 1
eq_big_int (big_int_of_string "1", big_int_of_int 1);;
test 2
eq_big_int (big_int_of_string "-1", big_int_of_int (-1));;
test 4
eq_big_int (big_int_of_string "0", zero_big_int);;

failwith_test 5 big_int_of_string "sdjdkfighdgf"
  (Failure "invalid digit");;

test 6
eq_big_int (big_int_of_string "123", big_int_of_int 123);;
test 7
eq_big_int (big_int_of_string "3456", big_int_of_int 3456);;

test 9
eq_big_int (big_int_of_string "-3456", big_int_of_int (-3456));;


let implode = List.fold_left (^) "";; (* Au diable l'efficacite *)

let l = rev [
"174679877494298468451661416292903906557638850173895426081611831060970135303";
"044177587617233125776581034213405720474892937404345377707655788096850784519";
"539374048533324740018513057210881137248587265169064879918339714405948322501";
"445922724181830422326068913963858377101914542266807281471620827145038901025";
"322784396182858865537924078131032036927586614781817695777639491934361211399";
"888524140253852859555118862284235219972858420374290985423899099648066366558";
"238523612660414395240146528009203942793935957539186742012316630755300111472";
"852707974927265572257203394961525316215198438466177260614187266288417996647";
"132974072337956513457924431633191471716899014677585762010115338540738783163";
"739223806648361958204720897858193606022290696766988489073354139289154127309";
"916985231051926209439373780384293513938376175026016587144157313996556653811";
"793187841050456120649717382553450099049321059330947779485538381272648295449";
"847188233356805715432460040567660999184007627415398722991790542115164516290";
"619821378529926683447345857832940144982437162642295073360087284113248737998";
"046564369129742074737760485635495880623324782103052289938185453627547195245";
"688272436219215066430533447287305048225780425168823659431607654712261368560";
"702129351210471250717394128044019490336608558608922841794819375031757643448";
"32"
] in

let bi1 = big_int_of_string (implode (rev l)) in

let bi2 = big_int_of_string (implode (rev ("3" :: tl l))) in

test 10
eq_big_int (bi1, (add_big_int (mult_big_int bi2 (big_int_of_string "10"))
                              (big_int_of_string "2")))
(* test 11
 &&
eq_big_int (bi1, (add_big_int (mult_big_int bi2 (big_int_of_string "10e0"))
                              (big_int_of_string "20e-1"))) &&
test 12
eq_big_int (minus_big_int bi1,
            (add_big_int (mult_big_int bi2 (big_int_of_string "-10e0"))
                         (big_int_of_string "-20e-1"))) &&
test 13
eq_big_int (bi1, (add_big_int (mult_big_int bi2 (big_int_of_string "+10e0"))
                              (big_int_of_string "+20e-1"))) &&
test 14
eq_big_int (minus_big_int bi1,
            (add_big_int (mult_big_int bi2 (big_int_of_string "-10e+0"))
                         (big_int_of_string "-20e-1"))) &&
test 15
eq_big_int (minus_big_int bi1,
            (add_big_int (mult_big_int bi2 (big_int_of_string "-1e+1"))
                         (big_int_of_string "-2e-0"))) &&
test 16
eq_big_int (minus_big_int bi1,
            (add_big_int (mult_big_int bi2 (big_int_of_string "-0.1e+2"))
                         (big_int_of_string "-2.0e-0"))) &&
test 17
eq_big_int (minus_big_int bi1,
            (add_big_int (mult_big_int bi2 (big_int_of_string "-1.000e+1"))
                         (big_int_of_string "-0.02e2")))*)
;;

test 18
eq_big_int (big_int_of_string "0xAbC", big_int_of_int 0xABC);;

test 19
eq_big_int (big_int_of_string "-0o452", big_int_of_int (-0o452));;

test 20
eq_big_int (big_int_of_string "0B110101", big_int_of_int 53);;

test 21
eq_big_int (big_int_of_string "0b11_01_01", big_int_of_int 53);;

testing_function "power_base_int";;

test 1
eq_big_int (big_int_of_nat (power_base_int 10 0), unit_big_int)
;;
test 2
eq_big_int (big_int_of_nat (power_base_int 10 8), big_int_of_int 100000000)
;;
test 3
eq_big_int (big_int_of_nat (power_base_int 2 (length_of_int + 2)),
            big_int_of_nat (let nat = make_nat 2 in
                              set_digit_nat nat 1 1;
                              nat))
;;

testing_function "base_power_big_int";;

test 1
eq_big_int (base_power_big_int 10 0 (big_int_of_int 2), big_int_of_int 2);;
test 2
eq_big_int (base_power_big_int 10 2 (big_int_of_int 2), big_int_of_int 200);;
test 3
eq_big_int (base_power_big_int 10 1 (big_int_of_int 123), big_int_of_int 1230)
;;

testing_function "power_int_positive_big_int";;

test 1
eq_big_int (power_int_positive_big_int 2 (big_int_of_int 10),
            big_int_of_int 1024);;
test 2
eq_big_int
 (power_int_positive_big_int 2 (big_int_of_int 65),
  big_int_of_string "36893488147419103232");;

test 3
eq_big_int
 (power_int_positive_big_int 3 (big_int_of_string "47"),
  big_int_of_string "26588814358957503287787");;

test 4
eq_big_int
 (power_int_positive_big_int 1 (big_int_of_string "1000000000000000000000"),
  big_int_of_int 1);;

test 5
eq_big_int
 (power_int_positive_big_int (-1) (big_int_of_string "1000000000000000000000"),
  big_int_of_int 1);;

test 6
eq_big_int
 (power_int_positive_big_int (-1) (big_int_of_string "1000000000000000000001"),
  big_int_of_int (-1));;

test 7
eq_big_int
 (power_int_positive_big_int 0 (big_int_of_string "1000000000000000000000"),
  big_int_of_int 0);;

testing_function "power_big_int_positive_int";;

test 1
eq_big_int (power_big_int_positive_int (big_int_of_int 2) 10,
            big_int_of_int 1024);;
test 2
eq_big_int
 (power_big_int_positive_int (big_int_of_int 100) 20,
  big_int_of_string "10000000000000000000000000000000000000000");;

test 3
eq_big_int
 (power_big_int_positive_int (big_int_of_string "3") 47,
  big_int_of_string "26588814358957503287787");;

test 4
eq_big_int
 (power_big_int_positive_int (big_int_of_string "200000000000000") 34,
  big_int_of_string
"17179869184000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000");;

test 5
eq_big_int
 (power_big_int_positive_int (big_int_of_string "2197609328765") 243,
  big_int_of_string
"12415638672345366257764851943822299490113545698929764576040102857365\
27920436565335427676982530274588056944387957287793378051852205028658\
73008292720317554332284838709453634119919368441951233982592586680844\
20765201140575612595182857026804842796931784944918059630667794516774\
58498235838834599150657873894983300999081942159304585449505963892008\
97855706440206825609657816209327492197604711437269361628626691080334\
38432768885637928268354258860147333786379766583179851226375449161073\
10396958979998161989562418169797611757651190037273397850239552735199\
63719988832594486235837899145390948533078339399890545062510060406048\
61331200657727576638170520036143007285549092686618686739320973444703\
33342725604091818763255601206325426337211467746377586080108631634250\
11232258578207762608797108802386708549785680783113606089879687396654\
54004281165259352412815385041917713969718327109245777066079665194617\
29230093411050053217775067781725651590160086483960457766025246936489\
92234225900994076609973190516835778346886551506344097474301175288686\
25662752919718480402972207084177612056491949911377568680526080633587\
33230060757162252611388973328501680433819585006035301408574879645573\
47126018243568976860515247053858204554293343161581801846081341003624\
22906934772131205632200433218165757307182816260714026614324014553342\
77303133877636489457498062819003614421295692889321460150481573909330\
77301946991278225819671075907191359721824291923283322225480199446258\
03302645587072103949599624444368321734975586414930425964782010567575\
43333331963876294983400462908871215572514487548352925949663431718284\
14589547315559936497408670231851521193150991888789948397029796279240\
53117024758684807981605608837291399377902947471927467827290844733264\
70881963357258978768427852958888430774360783419404195056122644913454\
24537375432013012467418602205343636983874410969339344956536142566292\
67710105053213729008973121773436382170956191942409859915563249876601\
97309463059908818473774872128141896864070835259683384180928526600888\
17480854811931632353621014638284918544379784608050029606475137979896\
79160729736625134310450643341951675749112836007180865039256361941093\
99844921135320096085772541537129637055451495234892640418746420370197\
76655592198723057553855194566534999101921182723711243608938705766658\
35660299983828999383637476407321955462859142012030390036241831962713\
40429407146441598507165243069127531565881439971034178400174881243483\
00001434950666035560134867554719667076133414445044258086968145695386\
00575860256380332451841441394317283433596457253185221717167880159573\
60478649571700878049257386910142909926740023800166057094445463624601\
79490246367497489548435683835329410376623483996271147060314994344869\
89606855219181727424853876740423210027967733989284801813769926906846\
45570461348452758744643550541290031199432061998646306091218518879810\
17848488755494879341886158379140088252013009193050706458824793551984\
39285914868159111542391208521561221610797141925061986437418522494485\
59871215531081904861310222368465288125816137210222223075106739997863\
76953125");;

testing_function "power_big_int_positive_big_int";;

test 1
eq_big_int
 (power_big_int_positive_big_int (big_int_of_int 2) (big_int_of_int 10),
  big_int_of_int 1024);;

test 2
eq_big_int
 (power_big_int_positive_big_int (big_int_of_int 2) (big_int_of_int 65),
  big_int_of_string "36893488147419103232");;

test 3
eq_big_int
 (power_big_int_positive_big_int
   (big_int_of_string "3") (big_int_of_string "47"),
  big_int_of_string "26588814358957503287787");;

test 4
eq_big_int
 (power_big_int_positive_big_int
     (big_int_of_string "200000000000000") (big_int_of_int 34),
  big_int_of_string
"17179869184000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000000000000000000000000000000000000000000000000000000000000\
00000000000");;

test 5
eq_big_int
 (power_big_int_positive_big_int (big_int_of_string "2197609328765")
                                 (big_int_of_string "243"),
  big_int_of_string
"12415638672345366257764851943822299490113545698929764576040102857365\
27920436565335427676982530274588056944387957287793378051852205028658\
73008292720317554332284838709453634119919368441951233982592586680844\
20765201140575612595182857026804842796931784944918059630667794516774\
58498235838834599150657873894983300999081942159304585449505963892008\
97855706440206825609657816209327492197604711437269361628626691080334\
38432768885637928268354258860147333786379766583179851226375449161073\
10396958979998161989562418169797611757651190037273397850239552735199\
63719988832594486235837899145390948533078339399890545062510060406048\
61331200657727576638170520036143007285549092686618686739320973444703\
33342725604091818763255601206325426337211467746377586080108631634250\
11232258578207762608797108802386708549785680783113606089879687396654\
54004281165259352412815385041917713969718327109245777066079665194617\
29230093411050053217775067781725651590160086483960457766025246936489\
92234225900994076609973190516835778346886551506344097474301175288686\
25662752919718480402972207084177612056491949911377568680526080633587\
33230060757162252611388973328501680433819585006035301408574879645573\
47126018243568976860515247053858204554293343161581801846081341003624\
22906934772131205632200433218165757307182816260714026614324014553342\
77303133877636489457498062819003614421295692889321460150481573909330\
77301946991278225819671075907191359721824291923283322225480199446258\
03302645587072103949599624444368321734975586414930425964782010567575\
43333331963876294983400462908871215572514487548352925949663431718284\
14589547315559936497408670231851521193150991888789948397029796279240\
53117024758684807981605608837291399377902947471927467827290844733264\
70881963357258978768427852958888430774360783419404195056122644913454\
24537375432013012467418602205343636983874410969339344956536142566292\
67710105053213729008973121773436382170956191942409859915563249876601\
97309463059908818473774872128141896864070835259683384180928526600888\
17480854811931632353621014638284918544379784608050029606475137979896\
79160729736625134310450643341951675749112836007180865039256361941093\
99844921135320096085772541537129637055451495234892640418746420370197\
76655592198723057553855194566534999101921182723711243608938705766658\
35660299983828999383637476407321955462859142012030390036241831962713\
40429407146441598507165243069127531565881439971034178400174881243483\
00001434950666035560134867554719667076133414445044258086968145695386\
00575860256380332451841441394317283433596457253185221717167880159573\
60478649571700878049257386910142909926740023800166057094445463624601\
79490246367497489548435683835329410376623483996271147060314994344869\
89606855219181727424853876740423210027967733989284801813769926906846\
45570461348452758744643550541290031199432061998646306091218518879810\
17848488755494879341886158379140088252013009193050706458824793551984\
39285914868159111542391208521561221610797141925061986437418522494485\
59871215531081904861310222368465288125816137210222223075106739997863\
76953125");;

test 6
eq_big_int
 (power_big_int_positive_big_int (big_int_of_int 1)
                                 (big_int_of_string "1000000000000000000000"),
  big_int_of_int 1);;

test 7
eq_big_int
 (power_big_int_positive_big_int (big_int_of_int (-1))
                                 (big_int_of_string "1000000000000000000000"),
  big_int_of_int 1);;

test 8
eq_big_int
 (power_big_int_positive_big_int (big_int_of_int (-1))
                                 (big_int_of_string "1000000000000000000001"),
  big_int_of_int (-1));;

test 9
eq_big_int
 (power_big_int_positive_big_int (big_int_of_int 0)
                                 (big_int_of_string "1000000000000000000000"),
  big_int_of_int 0);;

testing_function "square_big_int";;

test 1 eq_big_int
 (square_big_int (big_int_of_string "0"), big_int_of_string "0");;
test 2 eq_big_int
 (square_big_int (big_int_of_string "1"), big_int_of_string "1");;
test 3 eq_big_int
 (square_big_int (big_int_of_string "-1"), big_int_of_string "1");;
test 4 eq_big_int
 (square_big_int (big_int_of_string "-7"), big_int_of_string "49");;


testing_function "big_int_of_nativeint";;

test 1 eq_big_int
  (big_int_of_nativeint 0n, zero_big_int);;
test 2 eq_big_int
  (big_int_of_nativeint 1234n, big_int_of_string "1234");;
test 3 eq_big_int
  (big_int_of_nativeint (-1234n), big_int_of_string "-1234");;

testing_function "nativeint_of_big_int";;

test 1 eq_nativeint
  (nativeint_of_big_int zero_big_int, 0n);;
test 2 eq_nativeint
  (nativeint_of_big_int (big_int_of_string "1234"), 1234n);;
test 2 eq_nativeint
  (nativeint_of_big_int (big_int_of_string "-1234"), -1234n);;

testing_function "big_int_of_int32";;

test 1 eq_big_int
  (big_int_of_int32 0l, zero_big_int);;
test 2 eq_big_int
  (big_int_of_int32 2147483647l, big_int_of_string "2147483647");;
test 3 eq_big_int
  (big_int_of_int32 (-2147483648l), big_int_of_string "-2147483648");;

testing_function "int32_of_big_int";;

test 1 eq_int32
  (int32_of_big_int zero_big_int, 0l);;
test 2 eq_int32
  (int32_of_big_int (big_int_of_string "2147483647"), 2147483647l);;
test 3 eq_int32
  (int32_of_big_int (big_int_of_string "-2147483648"), -2147483648l);;
test 4 eq_int32
  (int32_of_big_int (big_int_of_string "-2147"), -2147l);;
let should_fail s =
  try ignore (int32_of_big_int (big_int_of_string s)); 0
  with Failure _ -> 1;;
test 5 eq_int
  (should_fail "2147483648", 1);;
test 6 eq_int
  (should_fail "-2147483649", 1);;
test 7 eq_int
  (should_fail "4294967296", 1);;
test 8 eq_int
  (should_fail "18446744073709551616", 1);;

testing_function "big_int_of_int64";;

test 1 eq_big_int
  (big_int_of_int64 0L, zero_big_int);;
test 2 eq_big_int
  (big_int_of_int64 9223372036854775807L, big_int_of_string "9223372036854775807");;
test 3 eq_big_int
  (big_int_of_int64 (-9223372036854775808L), big_int_of_string "-9223372036854775808");;
test 4 eq_big_int (*PR#4792*)
  (big_int_of_int64 (Int64.of_int32 Int32.min_int), big_int_of_string "-2147483648");;
test 5 eq_big_int
  (big_int_of_int64 1234L, big_int_of_string "1234");;
test 6 eq_big_int
  (big_int_of_int64 0x1234567890ABCDEFL, big_int_of_string "1311768467294899695");;
test 7 eq_big_int
  (big_int_of_int64 (-1234L), big_int_of_string "-1234");;
test 8 eq_big_int
  (big_int_of_int64 (-0x1234567890ABCDEFL), big_int_of_string "-1311768467294899695");;

testing_function "int64_of_big_int";;

test 1 eq_int64
  (int64_of_big_int zero_big_int, 0L);;
test 2 eq_int64
  (int64_of_big_int (big_int_of_string "9223372036854775807"), 9223372036854775807L);;
test 3 eq_int64
  (int64_of_big_int (big_int_of_string "-9223372036854775808"), -9223372036854775808L);;
test 4 eq_int64
  (int64_of_big_int (big_int_of_string "-9223372036854775"), -9223372036854775L);;
test 5 eq_int64 (* PR#4804 *)
  (int64_of_big_int (big_int_of_string "2147483648"), 2147483648L);;
let should_fail s =
  try ignore (int64_of_big_int (big_int_of_string s)); 0
  with Failure _ -> 1;;
test 6 eq_int
  (should_fail "9223372036854775808", 1);;
test 7 eq_int
  (should_fail "-9223372036854775809", 1);;
test 8 eq_int
  (should_fail "18446744073709551616", 1);;

(* build a 128-bit big int from two int64 *)

let big_int_128 hi lo =
  add_big_int (mult_big_int (big_int_of_int64 hi)
                            (big_int_of_string "18446744073709551616"))
              (big_int_of_int64 lo);;
let h1 = 0x7fd05b7ee46a29f8L
and h2 = 0x64b28b8ee70b6e6dL
and h3 = 0x58546e563f5b44f0L
and h4 = 0x1db72f6377ff3ec6L
and h5 = 0x4f9bb0a19c543cb1L;;

testing_function "and_big_int";;

test 1 eq_big_int
  (and_big_int unit_big_int zero_big_int, zero_big_int);;
test 2 eq_big_int
  (and_big_int zero_big_int unit_big_int, zero_big_int);;
test 3 eq_big_int
  (and_big_int unit_big_int unit_big_int, unit_big_int);;
test 4 eq_big_int
  (and_big_int (big_int_128 h1 h2) (big_int_128 h3 h4),
   big_int_128 (Int64.logand h1 h3) (Int64.logand h2 h4));;
test 5 eq_big_int
  (and_big_int (big_int_128 h1 h2) (big_int_of_int64 h5),
   big_int_of_int64 (Int64.logand h2 h5));;
test 6 eq_big_int
  (and_big_int (big_int_of_int64 h5) (big_int_128 h3 h4) ,
   big_int_of_int64 (Int64.logand h5 h4));;

testing_function "or_big_int";;

test 1 eq_big_int
  (or_big_int unit_big_int zero_big_int, unit_big_int);;
test 2 eq_big_int
  (or_big_int zero_big_int unit_big_int, unit_big_int);;
test 3 eq_big_int
  (or_big_int unit_big_int unit_big_int, unit_big_int);;
test 4 eq_big_int
  (or_big_int (big_int_128 h1 h2) (big_int_128 h3 h4),
   big_int_128 (Int64.logor h1 h3) (Int64.logor h2 h4));;
test 5 eq_big_int
  (or_big_int (big_int_128 h1 h2) (big_int_of_int64 h5),
   big_int_128 h1 (Int64.logor h2 h5));;
test 6 eq_big_int
  (or_big_int (big_int_of_int64 h5) (big_int_128 h3 h4) ,
   big_int_128 h3 (Int64.logor h5 h4));;

testing_function "xor_big_int";;

test 1 eq_big_int
  (xor_big_int unit_big_int zero_big_int, unit_big_int);;
test 2 eq_big_int
  (xor_big_int zero_big_int unit_big_int, unit_big_int);;
test 3 eq_big_int
  (xor_big_int unit_big_int unit_big_int, zero_big_int);;
test 4 eq_big_int
  (xor_big_int (big_int_128 h1 h2) (big_int_128 h3 h4),
   big_int_128 (Int64.logxor h1 h3) (Int64.logxor h2 h4));;
test 5 eq_big_int
  (xor_big_int (big_int_128 h1 h2) (big_int_of_int64 h5),
   big_int_128 h1 (Int64.logxor h2 h5));;
test 6 eq_big_int
  (xor_big_int (big_int_of_int64 h5) (big_int_128 h3 h4) ,
   big_int_128 h3 (Int64.logxor h5 h4));;

testing_function "shift_left_big_int";;

test 1 eq_big_int
  (shift_left_big_int unit_big_int 0,
   unit_big_int);;
test 2 eq_big_int
  (shift_left_big_int unit_big_int 1,
   big_int_of_int 2);;
test 2 eq_big_int
  (shift_left_big_int unit_big_int 31,
   big_int_of_string "2147483648");;
test 3 eq_big_int
  (shift_left_big_int unit_big_int 64,
   big_int_of_string "18446744073709551616");;
test 4 eq_big_int
  (shift_left_big_int unit_big_int 95,
   big_int_of_string "39614081257132168796771975168");;
test 5 eq_big_int
  (shift_left_big_int (big_int_of_string "39614081257132168796771975168") 67,
   big_int_of_string "5846006549323611672814739330865132078623730171904");;
test 6 eq_big_int
  (shift_left_big_int (big_int_of_string "-39614081257132168796771975168") 67,
   big_int_of_string "-5846006549323611672814739330865132078623730171904");;

testing_function "shift_right_big_int";;

test 1 eq_big_int
  (shift_right_big_int unit_big_int 0,
   unit_big_int);;
test 2 eq_big_int
  (shift_right_big_int (big_int_of_int 12345678) 3,
   big_int_of_int 1543209);;
test 3 eq_big_int
  (shift_right_big_int (big_int_of_string "5299989648942") 32,
   big_int_of_int 1234);;
test 4 eq_big_int
  (shift_right_big_int (big_int_of_string "5846006549323611672814739330865132078623730171904") 67,
   big_int_of_string "39614081257132168796771975168");;
test 5 eq_big_int
  (shift_right_big_int (big_int_of_string "-5299989648942") 32,
   big_int_of_int (-1235));;
test 6 eq_big_int
  (shift_right_big_int (big_int_of_string "-16570089876543209725755392") 27,
   big_int_of_string "-123456790123456789");;

testing_function "shift_right_towards_zero_big_int";;

test 1 eq_big_int
  (shift_right_towards_zero_big_int (big_int_of_string "-5299989648942") 32,
   big_int_of_int (-1234));;
test 2 eq_big_int
  (shift_right_towards_zero_big_int (big_int_of_string "-16570089876543209725755392") 27,
   big_int_of_string "-123456790123456789");;

testing_function "extract_big_int";;

test 1 eq_big_int
  (extract_big_int (big_int_of_int64 0x123456789ABCDEFL) 3 13,
   big_int_of_int 6589);;
test 2 eq_big_int
  (extract_big_int (big_int_128 h1 h2) 67 12,
   big_int_of_int 1343);;
test 3 eq_big_int
  (extract_big_int (big_int_of_string "-1844674407370955178") 37 9,
   big_int_of_int 307);;
test 4 eq_big_int
  (extract_big_int unit_big_int 2048 254,
   zero_big_int);;
test 5 eq_big_int
  (extract_big_int (big_int_of_int64 0x123456789ABCDEFL) 0 32,
   big_int_of_int64 2309737967L);;
test 6 eq_big_int
  (extract_big_int (big_int_of_int (-1)) 0 16,
   big_int_of_int 0xFFFF);;
test 7 eq_big_int
  (extract_big_int (big_int_of_int (-1)) 1027 12,
   big_int_of_int 0xFFF);;
test 8 eq_big_int
  (extract_big_int (big_int_of_int (-1234567)) 0 16,
   big_int_of_int 10617);;
test 9 eq_big_int
  (extract_big_int (minus_big_int (power_int_positive_int 2 64)) 64 20,
   big_int_of_int 0xFFFFF);;
test 10 eq_big_int
  (extract_big_int (pred_big_int (minus_big_int (power_int_positive_int 2 64))) 64 20,
   big_int_of_int 0xFFFFE);;

testing_function "hashing of big integers";;

test 1 eq_int (Hashtbl.hash zero_big_int,
               955772237);;
test 2 eq_int (Hashtbl.hash unit_big_int,
               992063522);;
test 3 eq_int (Hashtbl.hash (minus_big_int unit_big_int),
               161678167);;
test 4 eq_int (Hashtbl.hash (big_int_of_string "123456789123456789"),
               755417385);;
test 5 eq_int (Hashtbl.hash (sub_big_int
                               (big_int_of_string "123456789123456789")
                               (big_int_of_string "123456789123456789")),
               955772237);;
test 6 eq_int (Hashtbl.hash (sub_big_int
                               (big_int_of_string "123456789123456789")
                               (big_int_of_string "123456789123456788")),
              992063522);;
