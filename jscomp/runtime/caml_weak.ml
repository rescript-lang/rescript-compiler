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

type 'a t = 'a Js.def array

let caml_weak_create n =
  Js.Array.new_uninitialized n 

let caml_weak_set xs i v = 
  match v with 
  | Some x -> xs.(i) <- Js.to_def x 
  | None -> ()

let caml_weak_get  xs i = 
  Js.from_def xs.(i) 

let caml_weak_get_copy  xs i = 
  match Js.from_def xs.(i) with 
  | None -> None 
  | Some x -> Some (Obj.magic (Obj.dup (Obj.repr x) ))

let caml_weak_check xs i = 
  not @@ Js.is_undef xs.(i)

let caml_weak_blit = Caml_array.caml_array_blit
