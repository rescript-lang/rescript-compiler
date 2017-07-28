(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Int64ops
open Checkpoints
open Primitives
open Debugger_config

let history = ref ([] : int64 list)

let empty_history () =
  history := []

let add_current_time () =
  let time = current_time () in
    if !history = [] then
      history := [time]
    else if time <> List.hd !history then
      history := list_truncate !history_size (time::!history)

let previous_time_1 () =
  match !history with
    _::((time::_) as hist) ->
      history := hist; time
  | _ ->
      prerr_endline "No more information."; raise Toplevel

let rec previous_time n =
  if n = _1
  then previous_time_1()
  else begin ignore(previous_time_1()); previous_time(pre64 n) end
