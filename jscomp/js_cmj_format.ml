(* OCamlScript compiler
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



(* TODO: add a magic number *)
type cmj_value = {
  arity : Lam_stats.function_arities ;
  closed_lambda : Lambda.lambda option ; 
  (** Either constant or closed functor *)
}

type effect = string option

type cmj_table = {
  values : cmj_value String_map.t;
  pure : effect;
}

let dummy ?(pure=Some "dummy") () = 
  { values = String_map.empty ; pure }

let from_file name : cmj_table = Ext_marshal.from_file name

let from_string s : cmj_table = Marshal.from_string s 0

let to_file name v = Ext_marshal.to_file name v
