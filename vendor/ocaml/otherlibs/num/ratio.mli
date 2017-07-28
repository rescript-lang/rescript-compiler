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

(** Operation on rational numbers.

    This module is used to support the implementation of {!Num} and
    should not be called directly. *)

open Nat
open Big_int

(* Rationals (type [ratio]) are arbitrary-precision rational numbers,
   plus the special elements [1/0] (infinity) and [0/0] (undefined).
   In constrast with numbers (type [num]), the special cases of
   small integers and big integers are not optimized specially. *)

type ratio

(**/**)

val null_denominator : ratio -> bool
val numerator_ratio : ratio -> big_int
val denominator_ratio : ratio -> big_int
val sign_ratio : ratio -> int
val normalize_ratio : ratio -> ratio
val cautious_normalize_ratio : ratio -> ratio
val cautious_normalize_ratio_when_printing : ratio -> ratio
val create_ratio : big_int -> big_int -> ratio (* assumes nothing *)
val create_normalized_ratio : big_int -> big_int -> ratio
                              (* assumes normalized argument *)
val is_normalized_ratio : ratio -> bool
val report_sign_ratio : ratio -> big_int -> big_int
val abs_ratio : ratio -> ratio
val is_integer_ratio : ratio -> bool
val add_ratio : ratio -> ratio -> ratio
val minus_ratio : ratio -> ratio
val add_int_ratio : int -> ratio -> ratio
val add_big_int_ratio : big_int -> ratio -> ratio
val sub_ratio : ratio -> ratio -> ratio
val mult_ratio : ratio -> ratio -> ratio
val mult_int_ratio : int -> ratio -> ratio
val mult_big_int_ratio : big_int -> ratio -> ratio
val square_ratio : ratio -> ratio
val inverse_ratio : ratio -> ratio
val div_ratio : ratio -> ratio -> ratio
val integer_ratio : ratio -> big_int
val floor_ratio : ratio -> big_int
val round_ratio : ratio -> big_int
val ceiling_ratio : ratio -> big_int
val eq_ratio : ratio -> ratio -> bool
val compare_ratio : ratio -> ratio -> int
val lt_ratio : ratio -> ratio -> bool
val le_ratio : ratio -> ratio -> bool
val gt_ratio : ratio -> ratio -> bool
val ge_ratio : ratio -> ratio -> bool
val max_ratio : ratio -> ratio -> ratio
val min_ratio : ratio -> ratio -> ratio
val eq_big_int_ratio : big_int -> ratio -> bool
val compare_big_int_ratio : big_int -> ratio -> int
val lt_big_int_ratio : big_int -> ratio -> bool
val le_big_int_ratio : big_int -> ratio -> bool
val gt_big_int_ratio : big_int -> ratio -> bool
val ge_big_int_ratio : big_int -> ratio -> bool
val int_of_ratio : ratio -> int
val ratio_of_int : int -> ratio
val ratio_of_nat : nat -> ratio
val nat_of_ratio : ratio -> nat
val ratio_of_big_int : big_int -> ratio
val big_int_of_ratio : ratio -> big_int
val div_int_ratio : int -> ratio -> ratio
val div_ratio_int : ratio -> int -> ratio
val div_big_int_ratio : big_int -> ratio -> ratio
val div_ratio_big_int : ratio -> big_int -> ratio
val approx_ratio_fix : int -> ratio -> string
val approx_ratio_exp : int -> ratio -> string
val float_of_rational_string : ratio -> string
val string_of_ratio : ratio -> string
val ratio_of_string : string -> ratio
val float_of_ratio : ratio -> float
val power_ratio_positive_int : ratio -> int -> ratio
val power_ratio_positive_big_int : ratio -> big_int -> ratio
