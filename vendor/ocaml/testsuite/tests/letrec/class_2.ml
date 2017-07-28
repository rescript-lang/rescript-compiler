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

(* class expressions may also contain local recursive bindings *)
class test =
  let rec f = print_endline "f"; fun x -> g x
      and g = print_endline "g"; fun x -> f x in
object
  method f : 'a 'b. 'a -> 'b = f
  method g : 'a 'b. 'a -> 'b = g
end
