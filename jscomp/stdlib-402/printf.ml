(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*  Xavier Leroy and Pierre Weis, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

open CamlinternalFormatBasics
open CamlinternalFormat

let kfprintf k o (Format (fmt, _)) =
  make_printf (fun o acc -> output_acc o acc; k o) o End_of_acc fmt
let kbprintf k b (Format (fmt, _)) =
  make_printf (fun b acc -> bufput_acc b acc; k b) b End_of_acc fmt
let ikfprintf k oc (Format (fmt, _)) =
  make_printf (fun oc _ -> k oc) oc End_of_acc fmt

let fprintf oc fmt = kfprintf ignore oc fmt
let bprintf b fmt = kbprintf ignore b fmt
let ifprintf oc fmt = ikfprintf ignore oc fmt
let printf fmt = fprintf stdout fmt
let eprintf fmt = fprintf stderr fmt

let ksprintf k (Format (fmt, _)) =
  let k' () acc =
    let buf = Buffer.create 64 in
    strput_acc buf acc;
    k (Buffer.contents buf) in
  make_printf k' () End_of_acc fmt

let sprintf fmt = ksprintf (fun s -> s) fmt

let kprintf = ksprintf
