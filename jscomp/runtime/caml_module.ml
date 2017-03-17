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

(** *)

type shape = CamlinternalMod.shape
(** Note that we have to provide a drop in replacement, since compiler internally will
    spit out ("CamlinternalMod".[init_mod|update_mod] unless we intercept it 
    in the lambda layer
 *)
let init_mod (loc : string * int * int) (shape : shape) =  
  let undef_module _ = raise (Undefined_recursive_module loc) in
  let rec loop (shape : shape) (struct_ : Obj.t array) idx = 
    match shape with 
    | Function -> struct_.(idx)<-(Obj.magic undef_module)
    | Lazy -> struct_.(idx)<- (Obj.magic (lazy undef_module))
    | Class ->  
      struct_.(idx)<- 
        (Obj.magic (*ref {!CamlinternalOO.dummy_class loc} *)
           (undef_module, undef_module, undef_module,  0)
           (* depends on dummy class representation *)
        )
    | Module comps 
      -> 
      let v =  (Obj.magic [||]) in
      struct_.(idx)<- v ;
      let len = Array.length comps in
      for i = 0 to len - 1 do 
        loop comps.(i) v i       
      done
    | Value v ->
       struct_.(idx) <- v in
  let res = (Obj.magic [||] : Obj.t array) in
  loop shape res 0 ;
  res.(0)      

external caml_update_dummy : Obj.t -> Obj.t -> unit = "caml_update_dummy" 
(* Note the [shape] passed between [init_mod] and [update_mod] is always the same 
   and we assume [module] is encoded as an array
 *)
let update_mod (shape : shape)  (o : Obj.t)  (n : Obj.t) :  unit = 
  let rec aux (shape : shape) o n parent i  =
    match shape with
    | Function 
      -> Obj.set_field parent i n 

    | Lazy 
    | Class -> 
      caml_update_dummy o n 
    | Module comps 
      -> 
      for i = 0 to Array.length comps - 1 do 
        aux comps.(i) (Obj.field o i) (Obj.field n i) o i       
      done
    | Value _ -> () in 
  match shape with 
  | Module comps -> 
    for i = 0 to Array.length comps - 1 do  
      aux comps.(i) (Obj.field o i) (Obj.field n i) o  i
    done
  |  _ -> assert false 
