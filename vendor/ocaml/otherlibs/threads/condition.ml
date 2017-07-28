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

type t = { mutable waiting: Thread.t list }

let create () = { waiting = [] }

let wait cond mut =
  Thread.critical_section := true;
  Mutex.unlock mut;
  cond.waiting <- Thread.self() :: cond.waiting;
  Thread.sleep();
  Mutex.lock mut

let signal cond =
  match cond.waiting with               (* atomic *)
    [] -> ()
  | th :: rem -> cond.waiting <- rem (* atomic *); Thread.wakeup th

let broadcast cond =
  let w = cond.waiting in                  (* atomic *)
  cond.waiting <- [];                      (* atomic *)
  List.iter Thread.wakeup w
