(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
include Set.Make(String)

(**
  does_match {foo, bar, baz} {foo} => ok
  does_match {foo, bar, baz} {foo, boo} => ko
  does_match {foo, bar, baz} {} => ok
  does_match {foo, bar, baz} {foo, bar, baz} => ok
*)
let does_match x y = subset y x

let of_list l = List.fold_right add l empty

open Format

let print f s =
  let () = fprintf f "@[<0>" in
  let _ =
    fold begin fun elt first ->
      if not first then fprintf f ",@ ";
      pp_print_string f elt;
      false
    end s true in
  fprintf f "@]"

module Operators = struct
  let ( ++ ) x y = add y x
  let ( -- ) x y = remove y x
  let ( +++ ) x = function Some y -> add y x | None -> x
  let ( --- ) x = function Some y -> remove y x | None -> x
end
