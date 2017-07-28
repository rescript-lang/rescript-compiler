(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

open Int_misc
open Nat
open Big_int
open Arith_flags
open Ratio

type num = Int of int | Big_int of big_int | Ratio of ratio
        (* The type of numbers. *)

let biggest_INT = big_int_of_int biggest_int
and least_INT = big_int_of_int least_int

(* Coercion big_int -> num *)
let num_of_big_int bi =
 if le_big_int bi biggest_INT && ge_big_int bi least_INT
 then Int (int_of_big_int bi)
 else Big_int bi

let numerator_num = function
  Ratio r -> ignore (normalize_ratio r); num_of_big_int (numerator_ratio r)
| n -> n

let denominator_num = function
  Ratio r -> ignore (normalize_ratio r); num_of_big_int (denominator_ratio r)
| n -> Int 1

let normalize_num = function
  Int i -> Int i
| Big_int bi -> num_of_big_int bi
| Ratio r -> if is_integer_ratio r
              then num_of_big_int (numerator_ratio r)
              else Ratio r

let cautious_normalize_num_when_printing n =
 if (!normalize_ratio_when_printing_flag) then (normalize_num n) else n

let num_of_ratio r =
 ignore (normalize_ratio r);
 if not (is_integer_ratio r) then Ratio r
 else if is_int_big_int (numerator_ratio r) then
        Int (int_of_big_int (numerator_ratio r))
 else Big_int (numerator_ratio r)

(* Operations on num *)

let add_num a b = match (a,b) with
    ((Int int1), (Int int2)) ->
      let r =  int1 + int2 in
      if (int1 lxor int2) lor (int1 lxor (r lxor (-1))) < 0
      then Int r                        (* No overflow *)
      else Big_int(add_big_int (big_int_of_int int1) (big_int_of_int int2))
  | ((Int i), (Big_int bi)) ->
      num_of_big_int (add_int_big_int i bi)
  | ((Big_int bi), (Int i)) ->
      num_of_big_int (add_int_big_int i bi)

  | ((Int i), (Ratio r)) ->
      Ratio (add_int_ratio i r)
  | ((Ratio r), (Int i)) ->
      Ratio (add_int_ratio i r)

  | ((Big_int bi1), (Big_int bi2)) -> num_of_big_int (add_big_int bi1 bi2)

  | ((Big_int bi), (Ratio r)) ->
      Ratio (add_big_int_ratio bi r)
  | ((Ratio r), (Big_int bi)) ->
      Ratio (add_big_int_ratio bi r)

  | ((Ratio r1), (Ratio r2)) -> num_of_ratio (add_ratio r1 r2)

let ( +/ ) = add_num

let minus_num = function
  Int i -> if i = monster_int
              then Big_int (minus_big_int (big_int_of_int i))
              else Int (-i)
| Big_int bi -> Big_int (minus_big_int bi)
| Ratio r -> Ratio (minus_ratio r)

let sub_num n1 n2 = add_num n1 (minus_num n2)

let ( -/ ) = sub_num

let mult_num a b = match (a,b) with
   ((Int int1), (Int int2)) ->
    if num_bits_int int1 + num_bits_int int2 < length_of_int
       then Int (int1 * int2)
       else num_of_big_int (mult_big_int (big_int_of_int int1)
                                         (big_int_of_int int2))

 | ((Int i), (Big_int bi)) ->
     num_of_big_int (mult_int_big_int i bi)
 | ((Big_int bi), (Int i)) ->
     num_of_big_int (mult_int_big_int i bi)

 | ((Int i), (Ratio r)) ->
     num_of_ratio (mult_int_ratio i r)
 | ((Ratio r), (Int i)) ->
     num_of_ratio (mult_int_ratio i r)

 | ((Big_int bi1), (Big_int bi2)) ->
     num_of_big_int (mult_big_int bi1 bi2)

 | ((Big_int bi), (Ratio r)) ->
     num_of_ratio (mult_big_int_ratio bi r)
 | ((Ratio r), (Big_int bi)) ->
     num_of_ratio (mult_big_int_ratio bi r)

 | ((Ratio r1), (Ratio r2)) ->
     num_of_ratio (mult_ratio r1 r2)

let ( */ ) = mult_num

let square_num = function
   Int i -> if 2 * num_bits_int i < length_of_int
               then Int (i * i)
               else num_of_big_int (square_big_int (big_int_of_int i))
 | Big_int bi -> Big_int (square_big_int bi)
 | Ratio r -> Ratio (square_ratio r)

let div_num n1 n2 =
 match n1 with
 | Int i1 ->
    begin match n2 with
    | Int i2 ->
       num_of_ratio (create_ratio (big_int_of_int i1) (big_int_of_int i2))
    | Big_int bi2 -> num_of_ratio (create_ratio (big_int_of_int i1) bi2)
    | Ratio r2 -> num_of_ratio (div_int_ratio i1 r2) end

 | Big_int bi1 ->
     begin match n2 with
     | Int i2 -> num_of_ratio (create_ratio bi1 (big_int_of_int i2))
     | Big_int bi2 -> num_of_ratio (create_ratio bi1 bi2)
     | Ratio r2 -> num_of_ratio (div_big_int_ratio bi1 r2) end

 | Ratio r1 ->
     begin match n2 with
     | Int i2 -> num_of_ratio (div_ratio_int r1 i2)
     | Big_int bi2 -> num_of_ratio (div_ratio_big_int r1 bi2)
     | Ratio r2 -> num_of_ratio (div_ratio r1 r2) end
;;

let ( // ) = div_num

let floor_num = function
  Int i as n -> n
| Big_int bi as n -> n
| Ratio r -> num_of_big_int (floor_ratio r)

(* Coercion with ratio type *)
let ratio_of_num = function
  Int i -> ratio_of_int i
| Big_int bi -> ratio_of_big_int bi
| Ratio r -> r
;;

(* Euclidean division and remainder.  The specification is:

      a = b * quo_num a b + mod_num a b
      quo_num a b is an integer (Z)
      0 <= mod_num a b < |b|

A correct but slow implementation is:

      quo_num a b =
        if b >= 0 then floor_num (div_num a b)
                  else minus_num (floor_num (div_num a (minus_num b)))

      mod_num a b = 
        sub_num a (mult_num b (quo_num a b))

  However, this definition is vastly inefficient (cf PR #3473):
  we define here a better way of computing the same thing.

  PR#6753: the previous implementation was based on
    quo_num a b = floor_num (div_num a b)
  which is incorrect for negative b.
*)

let quo_num n1 n2 =
  match n1, n2 with
  | Int i1, Int i2 ->
      let q = i1 / i2 and r = i1 mod i2 in
      Int (if r >= 0 then q else if i2 > 0 then q - 1 else q + 1)
  | Int i1, Big_int bi2 ->
      num_of_big_int (div_big_int (big_int_of_int i1) bi2)
  | Int i1, Ratio r2 -> 
      num_of_big_int (report_sign_ratio r2
                         (floor_ratio (div_int_ratio i1 (abs_ratio r2))))
  | Big_int bi1, Int i2 ->
      num_of_big_int (div_big_int bi1 (big_int_of_int i2))
  | Big_int bi1, Big_int bi2 ->
      num_of_big_int (div_big_int bi1 bi2)
  | Big_int bi1, Ratio r2 ->
      num_of_big_int (report_sign_ratio r2
                        (floor_ratio (div_big_int_ratio bi1 (abs_ratio r2))))
  | Ratio r1, _ ->
      let r2 = ratio_of_num n2 in
      num_of_big_int (report_sign_ratio r2
                        (floor_ratio (div_ratio r1 (abs_ratio r2))))

let mod_num n1 n2 =
  match n1, n2 with
  | Int i1, Int i2 ->
      let r = i1 mod i2 in
      Int (if r >= 0 then r else if i2 > 0 then r + i2 else r - i2)
  | Int i1, Big_int bi2 ->
      num_of_big_int (mod_big_int (big_int_of_int i1) bi2)
  | Big_int bi1, Int i2 ->
      num_of_big_int (mod_big_int bi1 (big_int_of_int i2))
  | Big_int bi1, Big_int bi2 ->
      num_of_big_int (mod_big_int bi1 bi2)
  | _, _ ->
      sub_num n1 (mult_num n2 (quo_num n1 n2))

let power_num_int a b = match (a,b) with
   ((Int i), n) ->
       (match sign_int n with
           0 -> Int 1
         | 1 -> num_of_big_int (power_int_positive_int i n)
         | _ -> Ratio (create_normalized_ratio
                        unit_big_int (power_int_positive_int i (-n))))
| ((Big_int bi), n) ->
       (match sign_int n with
           0 -> Int 1
         | 1 -> num_of_big_int (power_big_int_positive_int bi n)
         | _ -> Ratio (create_normalized_ratio
                 unit_big_int (power_big_int_positive_int bi (-n))))
| ((Ratio r), n) ->
       (match sign_int n with
           0 -> Int 1
         | 1 -> Ratio (power_ratio_positive_int r n)
         | _ -> Ratio (power_ratio_positive_int
                         (inverse_ratio r) (-n)))

let power_num_big_int a b =  match (a,b) with
   ((Int i), n) ->
    (match sign_big_int n with
           0 -> Int 1
         | 1 -> num_of_big_int (power_int_positive_big_int i n)
         | _ -> Ratio (create_normalized_ratio
                         unit_big_int
                         (power_int_positive_big_int i (minus_big_int n))))
| ((Big_int bi), n) ->
       (match sign_big_int n with
           0 -> Int 1
         | 1 -> num_of_big_int (power_big_int_positive_big_int bi n)
         | _ -> Ratio (create_normalized_ratio
                         unit_big_int
                         (power_big_int_positive_big_int bi (minus_big_int n))))
| ((Ratio r), n) ->
       (match sign_big_int n with
           0 -> Int 1
         | 1 -> Ratio (power_ratio_positive_big_int r n)
         | _ -> Ratio (power_ratio_positive_big_int
                         (inverse_ratio r) (minus_big_int n)))

let power_num a b = match (a,b) with
  (n, (Int i)) -> power_num_int n i
| (n, (Big_int bi)) -> power_num_big_int n bi
| _ -> invalid_arg "power_num"

let ( **/ ) = power_num

let is_integer_num = function
  Int _     -> true
| Big_int _ -> true
| Ratio r   -> is_integer_ratio r

(* integer_num, floor_num, round_num, ceiling_num rendent des nums *)
let integer_num = function
  Int i as n -> n
| Big_int bi as n -> n
| Ratio r -> num_of_big_int (integer_ratio r)

and round_num = function
  Int i as n -> n
| Big_int bi as n -> n
| Ratio r -> num_of_big_int (round_ratio r)

and ceiling_num = function
  Int i as n -> n
| Big_int bi as n -> n
| Ratio r -> num_of_big_int (ceiling_ratio r)

(* Comparisons on nums *)

let sign_num = function
  Int i -> sign_int i
| Big_int bi -> sign_big_int bi
| Ratio r -> sign_ratio r

let eq_num a b = match (a,b) with
  ((Int int1), (Int int2)) -> int1 = int2

| ((Int i), (Big_int bi)) -> eq_big_int (big_int_of_int i) bi
| ((Big_int bi), (Int i)) -> eq_big_int (big_int_of_int i) bi

| ((Int i), (Ratio r)) -> eq_big_int_ratio (big_int_of_int i) r
| ((Ratio r), (Int i)) -> eq_big_int_ratio (big_int_of_int i) r

| ((Big_int bi1), (Big_int bi2)) -> eq_big_int bi1 bi2

| ((Big_int bi), (Ratio r)) -> eq_big_int_ratio bi r
| ((Ratio r), (Big_int bi)) -> eq_big_int_ratio bi r

| ((Ratio r1), (Ratio r2)) -> eq_ratio r1 r2

let ( =/ ) = eq_num

let ( <>/ ) a b = not(eq_num a b)

let compare_num a b = match (a,b) with
  ((Int int1), (Int int2)) -> compare_int int1 int2

| ((Int i), (Big_int bi)) -> compare_big_int (big_int_of_int i) bi
| ((Big_int bi), (Int i)) -> compare_big_int bi (big_int_of_int i)

| ((Int i), (Ratio r)) -> compare_big_int_ratio (big_int_of_int i) r
| ((Ratio r), (Int i)) -> -(compare_big_int_ratio (big_int_of_int i) r)

| ((Big_int bi1), (Big_int bi2)) -> compare_big_int bi1 bi2

| ((Big_int bi), (Ratio r)) -> compare_big_int_ratio bi r
| ((Ratio r), (Big_int bi)) -> -(compare_big_int_ratio bi r)

| ((Ratio r1), (Ratio r2)) -> compare_ratio r1 r2

let lt_num num1 num2 = compare_num num1 num2 < 0
and le_num num1 num2 = compare_num num1 num2 <= 0
and gt_num num1 num2 = compare_num num1 num2 > 0
and ge_num num1 num2 = compare_num num1 num2 >= 0

let ( </ ) = lt_num
and ( <=/ ) = le_num
and ( >/ ) = gt_num
and ( >=/ ) = ge_num

let max_num num1 num2 = if lt_num num1 num2 then num2 else num1
and min_num num1 num2 = if gt_num num1 num2 then num2 else num1

(* Coercions with basic types *)

(* Coercion with int type *)
let int_of_num = function
  Int i -> i
| Big_int bi -> int_of_big_int bi
| Ratio r -> int_of_ratio r

and num_of_int i =
  if i = monster_int
  then Big_int (big_int_of_int i)
  else Int i

(* Coercion with nat type *)
let nat_of_num = function
  Int i -> nat_of_int i
| Big_int bi -> nat_of_big_int bi
| Ratio r -> nat_of_ratio r

and num_of_nat nat =
  if (is_nat_int nat 0 (length_nat nat))
  then Int (nth_digit_nat nat 0)
  else Big_int (big_int_of_nat nat)

(* Coercion with big_int type *)
let big_int_of_num = function
  Int i -> big_int_of_int i
| Big_int bi -> bi
| Ratio r -> big_int_of_ratio r

let string_of_big_int_for_num bi =
  if !approx_printing_flag
     then approx_big_int !floating_precision bi
     else string_of_big_int bi

(* Coercion with string type *)

(* XL: suppression de sys_string_of_num *)

let string_of_normalized_num = function
  Int i -> string_of_int i
| Big_int bi -> string_of_big_int_for_num bi
| Ratio r -> string_of_ratio r
let string_of_num n =
    string_of_normalized_num (cautious_normalize_num_when_printing n)
let num_of_string s =
  try
    let flag = !normalize_ratio_flag in
    normalize_ratio_flag := true;
    let r = ratio_of_string s in
    normalize_ratio_flag := flag;
    if eq_big_int (denominator_ratio r) unit_big_int
    then num_of_big_int (numerator_ratio r)
    else Ratio r
  with Failure _ ->
    failwith "num_of_string"

(* Coercion with float type *)
let float_of_num = function
  Int i -> float i
| Big_int bi -> float_of_big_int bi
| Ratio r -> float_of_ratio r

(* XL: suppression de num_of_float, float_num *)

let succ_num = function
  Int i -> if i = biggest_int
              then Big_int (succ_big_int (big_int_of_int i))
              else Int (succ i)
| Big_int bi -> num_of_big_int (succ_big_int bi)
| Ratio r -> Ratio (add_int_ratio 1 r)

and pred_num = function
  Int i -> if i = monster_int
              then Big_int (pred_big_int (big_int_of_int i))
              else Int (pred i)
| Big_int bi -> num_of_big_int (pred_big_int bi)
| Ratio r -> Ratio (add_int_ratio (-1) r)

let abs_num = function
   Int i -> if i = monster_int
              then Big_int (minus_big_int (big_int_of_int i))
              else Int (abs i)
 | Big_int bi -> Big_int (abs_big_int bi)
 | Ratio r -> Ratio (abs_ratio r)

let approx_num_fix n num = approx_ratio_fix n (ratio_of_num num)
and approx_num_exp n num = approx_ratio_exp n (ratio_of_num num)

let incr_num r = r := succ_num !r
and decr_num r = r := pred_num !r
