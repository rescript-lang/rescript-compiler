(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Good test for loops. Best compiled with -unsafe. *)

let rec qsort lo hi (a : int array) =
  if lo < hi then begin
    let i = ref lo in
    let j = ref hi in
    let pivot = a.(hi) in
    while !i < !j do
      while !i < hi && a.(!i) <= pivot do incr i done;
      while !j > lo && a.(!j) >= pivot do decr j done;
      if !i < !j then begin
        let temp = a.(!i) in a.(!i) <- a.(!j); a.(!j) <- temp
      end
    done;
    let temp = a.(!i) in a.(!i) <- a.(hi); a.(hi) <- temp;
    qsort lo (!i-1) a;
    qsort (!i+1) hi a
  end


(* Same but abstract over the comparison to force spilling *)

let cmp i j = i - j

let rec qsort2 lo hi (a : int array) =
  if lo < hi then begin
    let i = ref lo in
    let j = ref hi in
    let pivot = a.(hi) in
    while !i < !j do
      while !i < hi && cmp a.(!i) pivot <= 0 do incr i done;
      while !j > lo && cmp a.(!j) pivot >= 0 do decr j done;
      if !i < !j then begin
        let temp = a.(!i) in a.(!i) <- a.(!j); a.(!j) <- temp
      end
    done;
    let temp = a.(!i) in a.(!i) <- a.(hi); a.(hi) <- temp;
    qsort2 lo (!i-1) a;
    qsort2 (!i+1) hi a
  end


(* Test *)

let seed = ref 0

let random() =
  seed := !seed * 25173 + 17431; !seed land 0xFFF


exception Failed

let test_sort sort_fun size =
  let a = Array.make size 0 in
  let check = Array.make 4096 0 in
  for i = 0 to size-1 do
    let n = random() in a.(i) <- n; check.(n) <- check.(n)+1
  done;
  sort_fun 0 (size-1) a;
  try
    check.(a.(0)) <- check.(a.(0)) - 1;
    for i = 1 to size-1 do
      if a.(i-1) > a.(i) then raise Failed;
      check.(a.(i)) <- check.(a.(i)) - 1
    done;
    for i = 0 to 4095 do
      if check.(i) <> 0 then raise Failed
    done;
    print_string "OK";  print_newline()
  with Failed ->
    print_string "failed"; print_newline()


let main () =
  test_sort qsort 500000;
  test_sort qsort2 500000

let _ = main(); exit 0
