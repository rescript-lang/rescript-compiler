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


type nested = {
  depth : int ; [@bs.as "BS_PRIVATE_NESTED_SOME_NONE"]
}

(* INPUT: [x] should not be nullable *)
let isNested (x : Obj.t) : bool = 
  Obj.repr ((Obj.magic x : nested).depth) != Obj.repr Js.undefined 

let some ( x : Obj.t) : Obj.t = 
  if Obj.magic x =  None then 
    (Obj.repr {depth = 0})
  else 
    (* [x] is neither None nor null so it is safe to do property access *)
  if x != Obj.repr Js.null &&  isNested x then   
    Obj.repr {depth = (Obj.magic x : nested).depth + 1}
  else  x 

let nullable_to_opt (type t) ( x : t Js.nullable) : t option = 
  if Js.isNullable x then 
    None 
  else Obj.magic (some (Obj.magic x : 'a))

let undefined_to_opt (type t) ( x : t Js.undefined) : t option = 
  if (Obj.magic x) == Js.undefined then None 
  else Obj.magic (some (Obj.magic x : 'a))

let null_to_opt (type t ) ( x : t Js.null) : t option = 
  if (Obj.magic x) == Js.null then None 
  else Obj.magic (some (Obj.magic x : 'a) )

(* external valFromOption : 'a option -> 'a = 
   "#val_from_option"   *)



(** The input is already of [Some] form, [x] is not None, 
    make sure [x[0]] will not throw *)
let valFromOption (x : Obj.t) : Obj.t =   
  if  x != Obj.repr Js.null && isNested x
  then 
    let {depth } : nested = Obj.magic x in
    if depth = 0 then Obj.magic None
    else Obj.repr {depth = depth - 1}
  else Obj.magic x   


let option_get (x : 'a option) = 
  if x = None then Caml_undefined_extern.empty
  else Obj.magic (valFromOption (Obj.repr x))


type poly = {
  hash : int [@bs.as "HASH" (* Literals.polyvar_hash*)]; 
  value : Obj.t [@bs.as "VAL"]
}  

(** [input] is optional polymorphic variant *)  
let option_unwrap (x : poly option) = 
  match x with   
  | None -> Obj.repr x
  | Some x -> x.value
