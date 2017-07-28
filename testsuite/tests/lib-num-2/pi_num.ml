(*************************************************************************)
(*                                                                       *)
(*                                 OCaml                                 *)
(*                                                                       *)
(*            Pierre Weis, projet Estime, INRIA Rocquencourt             *)
(*                                                                       *)
(*   Copyright 2008 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

(* Pi digits computed with the sreaming algorithm given on pages 4, 6
   & 7 of "Unbounded Spigot Algorithms for the Digits of Pi", Jeremy
   Gibbons, August 2004. *)

open Printf;;
open Num;;

let zero = num_of_int 0
and one = num_of_int 1
and three = num_of_int 3
and four = num_of_int 4
and ten = num_of_int 10
and neg_ten = num_of_int(-10)
;;

(* Linear Fractional Transformation *)
module LFT = struct

  let floor_ev (q, r, s, t) x = quo_num (q */ x +/ r) (s */ x +/ t);;

  let unit = (one, zero, zero, one);;

  let comp (q, r, s, t) (q', r', s', t') =
    (q */ q' +/ r */ s', q */ r' +/ r */ t',
     s */ q' +/ t */ s', s */ r' +/ t */ t')
;;

end
;;

let next z = LFT.floor_ev z three
and safe z n = (n =/ LFT.floor_ev z four)
and prod z n = LFT.comp (ten, neg_ten */ n, zero, one) z
and cons z k =
  let den = 2 * k + 1 in
  LFT.comp z (num_of_int k, num_of_int(2 * den), zero, num_of_int den)
;;

let rec digit k z n row col =
  if n > 0 then
    let y = next z in
    if safe z y then
      if col = 10 then (
    let row = row + 10 in
    printf "\t:%i\n%s" row (string_of_num y);
    digit k (prod z y) (n-1) row 1
      )
      else (
    print_string(string_of_num y);
    digit k (prod z y) (n-1) row (col + 1)
      )
    else digit (k + 1) (cons z k) n row col
  else
    printf "%*s\t:%i\n" (10 - col) "" (row + col)
;;

let digits n = digit 1 LFT.unit n 0 0
;;

let usage () =
  prerr_endline "Usage: pi_num <number of digits to compute for pi>";
  exit 2
;;

let main () =
  let args = Sys.argv in
  if Array.length args <> 2 then usage () else
  digits (int_of_string Sys.argv.(1))
;;

main ()
;;
