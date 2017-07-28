(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Damien Doligez, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* We cannot use bignums because we don't do custom runtimes, but
   int64 is a bit short, so we roll our own 37-digit numbers...
*)

let n0 = Int64.of_int 0;;
let n1 = Int64.of_int 1;;
let n2 = Int64.of_int 2;;
let n3 = Int64.of_int 3;;
let n5 = Int64.of_int 5;;

let ( % ) = Int64.rem;;
let ( * ) = Int64.mul;;
let ( / ) = Int64.div;;
let ( + ) = Int64.add;;
let digit = Int64.of_string "1000000000000000000";;

let mul n (pl, ph) = ((n * pl) % digit, n * ph + (n * pl) / digit);;
let cmp (nl, nh) (pl, ph) =
  if nh < ph then -1
  else if nh > ph then 1
  else if nl < pl then -1
  else if nl > pl then 1
  else 0
;;

let x2 = fun p -> mul n2 p;;
let x3 = fun p -> mul n3 p;;
let x5 = fun p -> mul n5 p;;

let nn1 = (n1, n0);;

let pr (nl, nh) =
  if compare nh n0 = 0
  then Printf.printf "%Ld\n" nl
  else Printf.printf "%Ld%018Ld\n" nh nl
;;

(*
  (* bignum version *)
open Num;;
let nn1 = num_of_int 1;;
let x2 = fun p -> (num_of_int 2) */ p;;
let x3 = fun p -> (num_of_int 3) */ p;;
let x5 = fun p -> (num_of_int 5) */ p;;
let cmp n p = sign_num (n -/ p);;
let pr n = Printf.printf "%s\n" (string_of_num n);;
*)


(* This is where the interesting stuff begins. *)

open Lazy;;

type 'a lcons = Cons of 'a * 'a lcons Lazy.t;;
type 'a llist = 'a lcons Lazy.t;;

let rec map f l =
  lazy (
    match force l with
    | Cons (x, ll) -> Cons (f x, map f ll)
  )
;;

let rec merge cmp l1 l2 =
  lazy (
    match force l1, force l2 with
    | Cons (x1, ll1), Cons (x2, ll2)
       -> let c = cmp x1 x2 in
          if c = 0
          then Cons (x1, merge cmp ll1 ll2)
          else if c < 0
          then Cons (x1, merge cmp ll1 l2)
          else Cons (x2, merge cmp l1 ll2)
  )
;;

let rec iter_interval f l (start, stop) =
  if stop = 0 then ()
  else match force l with
       | Cons (x, ll)
          -> if start <= 0 then f x;
             iter_interval f ll (start-1, stop-1)
;;

let rec hamming = lazy (Cons (nn1, merge cmp ham2 (merge cmp ham3 ham5)))
    and ham2 = lazy (force (map x2 hamming))
    and ham3 = lazy (force (map x3 hamming))
    and ham5 = lazy (force (map x5 hamming))
;;

iter_interval pr hamming (88000, 88100);;
