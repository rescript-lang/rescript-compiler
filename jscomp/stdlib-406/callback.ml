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
[@@@bs.config { flags = [|"-bs-no-cross-module-opt" |]}]
(* Registering OCaml values with the C runtime for later callbacks *)

#if BS
let register _ _ = ()
let register_exception _ _  = ()
#else
external register_named_value : string -> Obj.t -> unit
                              = "caml_register_named_value"

let register name v =
  register_named_value name (Obj.repr v)

let register_exception name (exn : exn) =
  let exn = Obj.repr exn in
  let slot = if Obj.tag exn = Obj.object_tag then exn else Obj.field exn 0 in
  register_named_value name slot
#end