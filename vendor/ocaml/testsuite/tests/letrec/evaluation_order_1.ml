(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Gabriel Scherer, projet Gallium, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* test evaluation order

   'y' is translated into a constant, and is therefore considered
   non-recursive. With the current letrec compilation method,
   it should be evaluated before x and z.
*)
type tree = Tree of tree list

let test =
  let rec x = (print_endline "x"; Tree [y; z])
  and y = (print_endline "y"; Tree [])
  and z = (print_endline "z"; Tree [x])
  in
  match (x, y, z) with
    | (Tree [y1; z1], Tree[], Tree[x1]) ->
      assert (y1 == y);
      assert (z1 == z);
      assert (x1 == x)
    | _ ->
      assert false
