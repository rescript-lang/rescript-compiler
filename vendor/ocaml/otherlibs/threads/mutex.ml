(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

type t = { mutable locked: bool; mutable waiting: Thread.t list }

let create () = { locked = false; waiting = [] }

let rec lock m =
  if m.locked then begin                (* test and set atomic *)
    Thread.critical_section := true;
    m.waiting <- Thread.self() :: m.waiting;
    Thread.sleep();
    lock m
  end else begin
    m.locked <- true                    (* test and set atomic *)
  end

let try_lock m =                        (* test and set atomic *)
  if m.locked then false else begin m.locked <- true; true end

let unlock m =
  (* Don't play with Thread.critical_section here because of Condition.wait *)
  let w = m.waiting in                  (* atomic *)
  m.waiting <- [];                      (* atomic *)
  m.locked <- false;                    (* atomic *)
  List.iter Thread.wakeup w
