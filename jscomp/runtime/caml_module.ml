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
(** This module replaced camlinternalMod completely. 
  Note we can replace {!CamlinternalMod} completely, but it is not replaced 
  due to we believe this is an even low level dependency
*)

[@@@warning "-37"] (* `Function` may be used in runtime *)

type shape = 
   | Function
   | Lazy
   | Class
   | Module of (shape * string) array
   | Value of Caml_obj_extern.t
(* ATTENTION: check across versions *)
module Array = Caml_array_extern 

external set_field : Caml_obj_extern.t -> string -> Caml_obj_extern.t -> unit   = ""
  [@@bs.set_index]

external get_field : Caml_obj_extern.t -> string -> Caml_obj_extern.t = ""
  [@@bs.get_index]

module type Empty = sig end

(** Note that we have to provide a drop in replacement, since compiler internally will
    spit out ("CamlinternalMod".[init_mod|update_mod] unless we intercept it 
    in the lambda layer
 *)
let init_mod (loc : string * int * int) (shape : shape) =    
  let undef_module _ = raise (Undefined_recursive_module loc) in
  let rec loop (shape : shape) (struct_ : Caml_obj_extern.t) idx = 
    match shape with 
    | Function ->
      set_field struct_ idx (Obj.magic undef_module) 
    | Lazy ->
      set_field struct_ idx (Obj.magic (lazy undef_module)) 
    | Class ->
      set_field struct_ idx      
        (Obj.magic (*ref {!CamlinternalOO.dummy_class loc} *)
           (undef_module, undef_module, undef_module,  0)
           (* depends on dummy class representation *)
        ) 
    | Module comps 
      -> 
      let v = Caml_obj_extern.repr (module struct end :  Empty) in
      set_field struct_ idx v ;
      let len = Array.length comps in
      for i = 0 to len - 1 do
        let shape, name = comps.(i) in
        loop shape v name       
      done
    | Value v ->
      set_field struct_ idx v in
  let res = Caml_obj_extern.repr (module struct end :  Empty) in
  let dummy_name = "dummy" in
  loop shape res dummy_name;
  get_field res dummy_name 

(* Note the [shape] passed between [init_mod] and [update_mod] is always the same 
   and we assume [module] is encoded as an array
 *)
let update_mod (shape : shape)  (o : Caml_obj_extern.t)  (n : Caml_obj_extern.t) :  unit = 
  let rec aux (shape : shape) o n parent i  =
    match shape with
    | Function 
      -> set_field parent i n

    | Lazy 
    | Class -> 
      Caml_obj.update_dummy o n 
    | Module comps 
      -> 
      for i = 0 to Array.length comps - 1 do 
        let shape, name = comps.(i) in
        aux shape (get_field o name ) (get_field n name) o name       
      done
    | Value _ -> () in 
  match shape with 
  | Module comps -> 
    for i = 0 to Array.length comps - 1 do
      let shape, name = comps.(i) in
      aux shape (get_field o name) (get_field n name) o name
    done
  |  _ -> assert false 
