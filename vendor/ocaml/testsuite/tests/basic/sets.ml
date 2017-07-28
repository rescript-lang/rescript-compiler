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

module IntSet = Set.Make(struct type t = int let compare x y = x-y end)

let even = List.fold_right IntSet.add [0; -2; 2; 4; 6; -10] IntSet.empty

let odd = List.fold_right IntSet.add [9; -7; 5; 1; -3] IntSet.empty

let _ =
  for i = -10 to 10 do
    Printf.printf "%d  %B  %B\n" i (IntSet.mem i even) (IntSet.mem i odd)
  done

module PowerSet(BaseSet: Set.S)
               (SetOrd: functor(S: Set.S) -> Set.OrderedType) =
  Set.Make(SetOrd(BaseSet))

module IntSetSet = PowerSet(IntSet)(functor (S: Set.S) -> S)

let setofset = List.fold_right IntSetSet.add [even; odd] IntSetSet.empty

let _ =
  List.iter
    (fun s -> Printf.printf "%B\n" (IntSetSet.mem s setofset))
    [IntSet.empty; even; odd; IntSet.union even odd]

let _ = exit 0
