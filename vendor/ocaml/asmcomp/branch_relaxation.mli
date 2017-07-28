(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                   Mark Shinwell, Jane Street Europe                 *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Fix up conditional branches that exceed hardware-allowed ranges. *)

module Make (T : Branch_relaxation_intf.S) : sig
  val relax
     : Linearize.instruction
    (* [max_offset_of_out_of_line_code] specifies the furthest distance,
       measured from the first address immediately after the last instruction
       of the function, that may be branched to from within the function in
       order to execute "out of line" code blocks such as call GC and
       bounds check points. *)
    -> max_out_of_line_code_offset:T.distance
    -> unit
end
