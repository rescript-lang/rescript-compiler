(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


val caml_sys_is_directory : string -> bool

val caml_sys_file_exists : string -> bool

(* this must match the definition in ../stdlib/pervasives.ml *)
type open_flag =
  | Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock

val caml_sys_open : string -> open_flag list -> int -> int

val caml_sys_remove : string -> unit

val caml_sys_close : int -> unit

val caml_ml_open_descriptor_out : int -> Caml_io.out_channel

val caml_ml_open_descriptor_in : int -> Caml_io.in_channel

val caml_ml_close_channel : Caml_io.out_channel -> unit

val caml_ml_input : Caml_io.in_channel -> bytes -> int -> int -> int

val caml_ml_input_char : Caml_io.in_channel -> char

val caml_ml_input_scan_line : Caml_io.in_channel -> int

