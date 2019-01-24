(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Closure_stack : sig
  type t

  val create : unit -> t

  val note_entering_closure
     : t
    -> closure_id:Closure_id.t
    -> dbg:Debuginfo.t
    -> t

  val note_entering_call
    : t
    -> closure_id:Closure_id.t
    -> dbg:Debuginfo.t
    -> t

  val note_entering_inlined : t -> t
  val note_entering_specialised : t -> closure_ids:Closure_id.Set.t -> t

end

val record_decision
   : Inlining_stats_types.Decision.t
  -> closure_stack:Closure_stack.t
  -> unit

val save_then_forget_decisions : output_prefix:string -> unit
