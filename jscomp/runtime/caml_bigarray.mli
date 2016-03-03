
(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)


val caml_array_bound_error : unit -> 'a
val caml_invalid_argument : string -> 'a
type num = nativeint
val caml_ba_get_size : num array -> num
val index_offset_c : int -> num array -> num array -> num
val index_offset_fortran : int -> num array -> num array -> num
val caml_ba_create :
  ('a, 'b) Bigarray.kind ->
  'c Bigarray.layout -> int array -> ('a, 'b, 'c) Bigarray.Genarray.t
val caml_ba_get_generic :
  ('a, 'b, 'c) Bigarray.Genarray.t -> int array -> 'a 
val caml_ba_set_generic :
  ('a, 'b, 'c) Bigarray.Genarray.t -> int array -> 'a -> unit
val caml_ba_num_dims : ('a, 'b, 'c) Bigarray.Genarray.t -> int
val caml_ba_dim : ('a, 'b, 'c) Bigarray.Genarray.t -> int -> int
val caml_ba_kind :
  ('a, 'b, 'c) Bigarray.Genarray.t -> ('a, 'b) Bigarray.kind
val caml_ba_layout :
  ('a, 'b, 'c) Bigarray.Genarray.t -> 'c Bigarray.layout 
val caml_ba_sub :
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  int -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
val caml_ba_slice :
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  int array -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t 
val caml_ba_blit :
  ('a, 'b, 'c) Bigarray.Genarray.t ->
  ('a, 'b, 'c) Bigarray.Genarray.t -> unit 
val caml_ba_fill : ('a, 'b, 'c) Bigarray.Genarray.t -> 'a -> unit 
val caml_ba_reshape :
  ('a, 'b, 'c) Bigarray.Genarray.t ->
  int array -> ('a, 'b, 'c) Bigarray.Genarray.t
val caml_ba_get_1 : ('a, 'b, 'c) Bigarray.Genarray.t -> int -> 'a 
val caml_ba_set_1 :
  ('a, 'b, 'c) Bigarray.Genarray.t -> int -> 'a -> unit
val caml_ba_set_2 :
  ('a, 'b, 'c) Bigarray.Genarray.t -> int -> int -> 'a -> unit 
val caml_ba_get_2 : ('a, 'b, 'c) Bigarray.Genarray.t -> int -> int -> 'a

val caml_ba_dim_1 : ('a, 'b, 'c) Bigarray.Genarray.t -> int
val caml_ba_dim_2 : ('a, 'b, 'c) Bigarray.Genarray.t -> int
val caml_ba_dim_3 : ('a, 'b, 'c) Bigarray.Genarray.t -> int
val caml_ba_get_3 :
  ('a, 'b, 'c) Bigarray.Genarray.t -> int -> int -> int -> 'a
val caml_ba_set_3 :
  ('a, 'b, 'c) Bigarray.Genarray.t -> int -> int -> int -> 'a -> unit

val caml_ba_map_file_bytecode :
  Unix.file_descr ->
  ('a, 'b) Bigarray.kind ->
  'c Bigarray.layout ->
  bool -> int array -> int64 -> ('a, 'b, 'c) Bigarray.Genarray.t
