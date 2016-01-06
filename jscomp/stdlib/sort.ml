(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Merging and sorting *)

open Array

let rec merge order l1 l2 =
  match l1 with
    [] -> l2
  | h1 :: t1 ->
      match l2 with
        [] -> l1
      | h2 :: t2 ->
          if order h1 h2
          then h1 :: merge order t1 l2
          else h2 :: merge order l1 t2

let list order l =
  let rec initlist = function
      [] -> []
    | [e] -> [[e]]
    | e1::e2::rest ->
        (if order e1 e2 then [e1;e2] else [e2;e1]) :: initlist rest in
  let rec merge2 = function
      l1::l2::rest -> merge order l1 l2 :: merge2 rest
    | x -> x in
  let rec mergeall = function
      [] -> []
    | [l] -> l
    | llist -> mergeall (merge2 llist) in
  mergeall(initlist l)

let swap arr i j =
  let tmp = unsafe_get arr i in
  unsafe_set arr i (unsafe_get arr j);
  unsafe_set arr j tmp

(* There is a known performance bug in the code below.  If you find
   it, don't bother reporting it.  You're not supposed to use this
   module anyway. *)
let array cmp arr =
  let rec qsort lo hi =
    if hi - lo >= 6 then begin
      let mid = (lo + hi) lsr 1 in
      (* Select median value from among LO, MID, and HI. Rearrange
         LO and HI so the three values are sorted. This lowers the
         probability of picking a pathological pivot.  It also
         avoids extra comparisons on i and j in the two tight "while"
         loops below. *)
      if cmp (unsafe_get arr mid) (unsafe_get arr lo) then swap arr mid lo;
      if cmp (unsafe_get arr hi) (unsafe_get arr mid) then begin
        swap arr mid hi;
        if cmp (unsafe_get arr mid) (unsafe_get arr lo) then swap arr mid lo
      end;
      let pivot = unsafe_get arr mid in
      let i = ref (lo + 1) and j = ref (hi - 1) in
      if not (cmp pivot (unsafe_get arr hi))
         || not (cmp (unsafe_get arr lo) pivot)
      then raise (Invalid_argument "Sort.array");
      while !i < !j do
        while not (cmp pivot (unsafe_get arr !i)) do incr i done;
        while not (cmp (unsafe_get arr !j) pivot) do decr j done;
        if !i < !j then swap arr !i !j;
        incr i; decr j
      done;
      (* Recursion on smaller half, tail-call on larger half *)
      if !j - lo <= hi - !i then begin
        qsort lo !j; qsort !i hi
      end else begin
        qsort !i hi; qsort lo !j
      end
    end in
  qsort 0 (Array.length arr - 1);
  (* Finish sorting by insertion sort *)
  for i = 1 to Array.length arr - 1 do
    let val_i = (unsafe_get arr i) in
    if not (cmp (unsafe_get arr (i - 1)) val_i) then begin
      unsafe_set arr i (unsafe_get arr (i - 1));
      let j = ref (i - 1) in
      while !j >= 1 && not (cmp (unsafe_get arr (!j - 1)) val_i) do
        unsafe_set arr !j (unsafe_get arr (!j - 1));
        decr j
      done;
      unsafe_set arr !j val_i
    end
  done
