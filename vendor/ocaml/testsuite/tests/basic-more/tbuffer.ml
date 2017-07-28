(*************************************************************************)
(*                                                                       *)
(*                                 OCaml                                 *)
(*                                                                       *)
(*            Pierre Weis, projet Estime, INRIA Rocquencourt             *)
(*                                                                       *)
(*   Copyright 2009 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

(* Dummy substitute function. *)

open Testing;;
open Buffer;;

let identity s = s;;

let b = Buffer.create 100;;

(* Pattern with a '\\' character in it. *)
let pat0 = "\\\\a" in
let n0 = String.length pat0 in

Buffer.add_substitute b identity pat0;

test (String.length (Buffer.contents b) = n0)
;;

(* Pattern with a '\\' character at the end. *)
let pat1 = "b\\" in
let n1 = String.length pat1 in

Buffer.clear b;
Buffer.add_substitute b identity pat1;
test (String.length (Buffer.contents b) = n1)
;;
