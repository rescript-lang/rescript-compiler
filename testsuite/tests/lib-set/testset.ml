(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

module S = Set.Make(struct type t = int let compare (x:t) y = compare x y end)

let testvals = [0;1;2;3;4;5;6;7;8;9]

let check msg cond =
  if not (List.for_all cond testvals) then
    Printf.printf "Test %s FAILED\n%!" msg

let checkbool msg b =
  if not b then
    Printf.printf "Test %s FAILED\n%!" msg

let normalize_cmp c =
  if c = 0 then 0 else if c > 0 then 1 else -1

let test x s1 s2 =

  checkbool "is_empty"
    (S.is_empty s1 = List.for_all (fun i -> not (S.mem i s1)) testvals);

  check "add"
    (let s = S.add x s1 in
     fun i -> S.mem i s = (S.mem i s1 || i = x));

  check "singleton"
    (let s = S.singleton x in
     fun i -> S.mem i s = (i = x));

  check "remove"
    (let s = S.remove x s1 in
     fun i -> S.mem i s = (S.mem i s1 && i <> x));

  check "union"
    (let s = S.union s1 s2 in
     fun i -> S.mem i s = (S.mem i s1 || S.mem i s2));

  check "inter"
    (let s = S.inter s1 s2 in
     fun i -> S.mem i s = (S.mem i s1 && S.mem i s2));

  check "diff"
    (let s = S.diff s1 s2 in
     fun i -> S.mem i s = (S.mem i s1 && not (S.mem i s2)));

  checkbool "elements"
    (S.elements s1 = List.filter (fun i -> S.mem i s1) testvals);

  checkbool "compare"
    (normalize_cmp (S.compare s1 s2)
     = normalize_cmp (compare (S.elements s1) (S.elements s2)));

  checkbool "equal"
    (S.equal s1 s2 = (S.elements s1 = S.elements s2));

  check "subset"
    (let b = S.subset s1 s2 in
     fun i -> if b && S.mem i s1 then S.mem i s2 else true);

  checkbool "subset2"
    (let b = S.subset s1 s2 in
     b || not (S.is_empty (S.diff s1 s2)));

  checkbool "for_all"
    (let p x = x mod 2 = 0 in
     S.for_all p s1 = List.for_all p (S.elements s1));

  checkbool "exists"
    (let p x = x mod 3 = 0 in
     S.exists p s1 = List.exists p (S.elements s1));

  checkbool "filter"
    (let p x = x >= 3 && x <= 6 in
     S.elements(S.filter p s1) = List.filter p (S.elements s1));

  checkbool "partition"
    (let p x = x >= 3 && x <= 6 in
     let (st,sf) = S.partition p s1
     and (lt,lf) = List.partition p (S.elements s1) in
     S.elements st = lt && S.elements sf = lf);

  checkbool "cardinal"
    (S.cardinal s1 = List.length (S.elements s1));

  checkbool "min_elt"
    (try
       let m = S.min_elt s1 in
       S.mem m s1 && S.for_all (fun i -> m <= i) s1
     with Not_found ->
       S.is_empty s1);

  checkbool "max_elt"
    (try
       let m = S.max_elt s1 in
       S.mem m s1 && S.for_all (fun i -> m >= i) s1
     with Not_found ->
       S.is_empty s1);

  checkbool "choose"
    (try
       let x = S.choose s1 in S.mem x s1
     with Not_found ->
       S.is_empty s1);

  check "split"
    (let (l, p, r) = S.split x s1 in
     fun i ->
       if i < x then S.mem i l = S.mem i s1
       else if i > x then S.mem i r = S.mem i s1
       else p = S.mem i s1)

let relt() = Random.int 10

let rset() =
  let s = ref S.empty in
  for i = 1 to Random.int 10 do s := S.add (relt()) !s done;
  !s

let _ =
  Random.init 42;
  for i = 1 to 25000 do test (relt()) (rset()) (rset()) done
