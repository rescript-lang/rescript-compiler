(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Module [Lazy]: deferred computations *)


(*
   WARNING: some purple magic is going on here.  Do not take this file
   as an example of how to program in OCaml.
*)


(* We make use of two special tags provided by the runtime:
   [lazy_tag] and [forward_tag].

   A value of type ['a Lazy.t] can be one of three things:
   1. A block of size 1 with tag [lazy_tag].  Its field is a closure of
      type [unit -> 'a] that computes the value.
   2. A block of size 1 with tag [forward_tag].  Its field is the value
      of type ['a] that was computed.
   3. Anything else except a float.  This has type ['a] and is the value
      that was computed.
   Exceptions are stored in format (1).
   The GC will magically change things from (2) to (3) according to its
   fancy.

   We cannot use representation (3) for a [float Lazy.t] because
   [caml_make_array] assumes that only a [float] value can have tag
   [Double_tag].

   We have to use the built-in type constructor [lazy_t] to
   let the compiler implement the special typing and compilation
   rules for the [lazy] keyword.
*)

type 'a t = 'a lazy_t;;

exception Undefined = CamlinternalLazy.Undefined;;

external make_forward : 'a -> 'a lazy_t = "caml_lazy_make_forward";;

external force : 'a t -> 'a = "%lazy_force";;

(* let force = force;; *)

let force_val = CamlinternalLazy.force_val;;

let from_fun (f : unit -> 'arg) =
  let x = Obj.new_block Obj.lazy_tag 1 in
  Obj.set_field x 0 (Obj.repr f);
  (Obj.obj x : 'arg t)
;;

let from_val (v : 'arg) =
  let t = Obj.tag (Obj.repr v) in
  if t = Obj.forward_tag || t = Obj.lazy_tag || t = Obj.double_tag then begin
    make_forward v
  end else begin
    (Obj.magic v : 'arg t)
  end
;;

let is_val (l : 'arg t) = Obj.tag (Obj.repr l) <> Obj.lazy_tag;;

let lazy_from_fun = from_fun;;

let lazy_from_val = from_val;;

let lazy_is_val = is_val;;
