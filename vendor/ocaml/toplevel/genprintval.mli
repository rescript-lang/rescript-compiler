(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Printing of values *)

open Types
open Format

module type OBJ =
  sig
    type t
    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
  end

module type EVALPATH =
  sig
    type valu
    val eval_path: Env.t -> Path.t -> valu
    exception Error
    val same_value: valu -> valu -> bool
  end

type ('a, 'b) gen_printer =
  | Zero of 'b
  | Succ of ('a -> ('a, 'b) gen_printer)

module type S =
  sig
    type t
    val install_printer :
          Path.t -> Types.type_expr -> (formatter -> t -> unit) -> unit
    val install_generic_printer :
          Path.t -> Path.t ->
          (int -> (int -> t -> Outcometree.out_value,
                   t -> Outcometree.out_value) gen_printer) ->
          unit
    val install_generic_printer' :
           Path.t -> Path.t ->
           (formatter -> t -> unit,
            formatter -> t -> unit) gen_printer ->
           unit
    (** [install_generic_printer' function_path constructor_path printer]
        function_path is used to remove the printer. *)

    val remove_printer : Path.t -> unit
    val outval_of_untyped_exception : t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> t -> type_expr -> Outcometree.out_value
  end

module Make(O : OBJ)(EVP : EVALPATH with type valu = O.t) :
         (S with type t = O.t)
