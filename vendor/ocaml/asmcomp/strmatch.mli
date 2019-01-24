(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation of string matching from closed lambda to C-- *)

module type I = sig
  val string_block_length : Cmm.expression -> Cmm.expression
  val transl_switch :
      Location.t -> Cmm.expression -> int -> int ->
        (int * Cmm.expression) list -> Cmm.expression ->
          Cmm.expression
end

module Make(I:I) : sig
  (* Compile stringswitch (arg,cases,d)
     Note: cases should not contain string duplicates *)
  val compile : Debuginfo.t -> Cmm.expression (* arg *)
    -> Cmm.expression option (* d *) ->
    (string * Cmm.expression) list (* cases *)-> Cmm.expression
end
