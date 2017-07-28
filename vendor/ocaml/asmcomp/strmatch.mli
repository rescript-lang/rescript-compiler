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

(* Translation of string matching from closed lambda to C-- *)

module type I = sig
  val string_block_length : Cmm.expression -> Cmm.expression
  val transl_switch :
      Cmm.expression -> int -> int ->
        (int * Cmm.expression) list -> Cmm.expression ->
          Cmm.expression
end

module Make(I:I) : sig
  (* Compile stringswitch (arg,cases,d)
     Note: cases should not contain string duplicates *)
  val compile : Cmm.expression (* arg *) -> Cmm.expression option (* d *) ->
    (string * Cmm.expression) list (* cases *)-> Cmm.expression
end
