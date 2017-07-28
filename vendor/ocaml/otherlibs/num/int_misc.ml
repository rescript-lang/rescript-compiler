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

(* Some extra operations on integers *)

let rec gcd_int i1 i2 =
  if i2 = 0 then abs i1 else gcd_int i2 (i1 mod i2)
;;

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
