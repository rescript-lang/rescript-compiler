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


external caml_obj_set_tag : Obj.t -> int -> unit = "caml_obj_set_tag"
external js_obj_set_length : Obj.t -> int -> unit = "js_obj_set_length"
external js_obj_length : Obj.t -> int = "js_obj_length"
external caml_obj_tag : Obj.t -> int = "caml_obj_tag"
external caml_obj_set_tag : Obj.t -> int -> unit = "caml_obj_set_tag"
external js_uninitialized_object : int -> int -> Obj.t = "js_uninitialized_object"
external js_is_instance_array : Obj.t -> bool = "js_is_instance_array"
external js_typeof : 'a -> string = "js_typeof"

(** Mainly used in camlinternalOO
    {[
      let dummy_met : item = obj (Obj.new_block 0 0)
      let obj = Obj.new_block Obj.object_tag table.size
    ]}

    Here we only need generate expression like this 
    {[
      { tag : tag ; length : size }      
    ]}
    we don't need fill fields, since it is not required by GC    
*)
let caml_obj_dup (x : Obj.t) = 
  let len = js_obj_length x in
  let v = js_uninitialized_object (caml_obj_tag x ) len in
  for i = 0 to len - 1 do 
    Obj.set_field v i (Obj.field v i)
  done;
  v   

let caml_obj_truncate (x : Obj.t) (new_size : int) = 
  let len = js_obj_length x in
  if new_size <= 0 || new_size > len then 
    raise (Invalid_argument "Obj.truncate")
  else 
  if len != new_size  then
    begin 
      for i = new_size  to len - 1  do
        Obj.set_field x  i (Obj.magic 0)
      done;
      js_obj_set_length x new_size 
    end

     
let caml_obj_is_block x = 
  js_typeof (Obj.repr (js_obj_length x )) = "undefined"

let caml_lazy_make_forward x = lazy x 

(** TODO: the dummy one should be [{}] *)
let caml_update_dummy x y = 
  let len = js_obj_length y in
  for i = 0 to len - 1 do  
    Obj.set_field x i (Obj.field y i)
  done  ;
  Obj.set_tag x (Obj.tag y);
  js_obj_set_length x   (js_obj_length y)

let caml_int_compare (x : int) (y: int) : int = 
  if  x < y then -1 else if x = y then 0 else  1

let caml_string_compare (x : string) (y: string) : int = 
  if  x < y then -1 else if x = y then 0 else  1

(** TODO: investigate total 
    [compare x y] returns [0] if [x] is equal to [y], 
    a negative integer if [x] is less than [y], 
    and a positive integer if [x] is greater than [y]. 
    The ordering implemented by compare is compatible with the comparison 
    predicates [=], [<] and [>] defined above, with one difference on the treatment of the float value 
    [nan]. 

    Namely, the comparison predicates treat nan as different from any other float value, 
    including itself; while compare treats [nan] as equal to itself and less than any other float value. 
    This treatment of [nan] ensures that compare defines a total ordering relation.
    compare applied to functional values may raise Invalid_argument. compare applied to cyclic structures 
    may not terminate.

    The compare function can be used as the comparison function required by the [Set.Make] and [Map.Make] functors,
    as well as the [List.sort] and [Array.sort] functions.
*)
let rec caml_compare (a : Obj.t) (b : Obj.t) : int = 
  if js_typeof a = "string" then
    caml_string_compare (Obj.magic a) (Obj.magic b )
  else if js_typeof a = "number" then
    caml_int_compare (Obj.magic a) (Obj.magic b )
  else
  (* if js_is_instance_array a then  *)
  (*   0 *)
  (* else  *)
    let tag_a = caml_obj_tag a in
    let tag_b = caml_obj_tag b in
    (* double_array_tag: 254
       forward_tag:250
    *)
    if tag_a = 250 then
      caml_compare (Obj.field a 0) b
    else if tag_b = 250 then 
      caml_compare a (Obj.field b 0)
    else if tag_a = 248 (* object/exception *)  then
      caml_int_compare (Obj.magic @@ Obj.field a 1) (Obj.magic @@ Obj.field b 1 )       
    else if tag_a = 251 (* abstract_tag *) then 
      raise (Invalid_argument "equal: abstract value")
    else if tag_a != tag_b then
      if tag_a < tag_b then (-1) else  1
    else
      let len_a = js_obj_length a in 
      let len_b = js_obj_length b in 
      if len_a = len_b then 
        aux_same_length a b 0 len_a 
      else if len_a < len_b then 
        aux_length_a_short a b 0 len_a 
      else 
        aux_length_b_short a b 0 len_b
and aux_same_length  (a : Obj.t) (b : Obj.t) i same_length = 
  if i = same_length then
    0
  else 
    let res = caml_compare (Obj.field a i) (Obj.field b i) in
    if res !=0 then res
    else aux_same_length  a b (i + 1) same_length
and aux_length_a_short (a : Obj.t)  (b : Obj.t)  i short_length    = 
  if i = short_length then -1 
  else 
    let res = caml_compare (Obj.field a i) (Obj.field b i) in
    if res !=0 then res
    else aux_length_a_short a b (i+1) short_length
and aux_length_b_short (a : Obj.t) (b : Obj.t) i short_length = 
  if i = short_length then 1
  else
    let res = caml_compare (Obj.field a i) (Obj.field b i) in
    if res !=0 then res
    else aux_length_b_short a b (i+1) short_length      

type eq = Obj.t -> Obj.t -> bool

let rec caml_equal (a : Obj.t) (b : Obj.t) : bool = 
  if js_typeof a = "string" then a == b 
  else if js_typeof a = "number" then a == b 
  else
    let tag_a = caml_obj_tag a in
    let tag_b = caml_obj_tag b in
    (* double_array_tag: 254
       forward_tag:250
    *)
    if tag_a = 250 then
      caml_equal (Obj.field a 0) b
    else if tag_b = 250 then 
      caml_equal a (Obj.field b 0)
    else if tag_a = 248 (* object/exception *)  then
      (Obj.magic @@ Obj.field a 1) ==  (Obj.magic @@ Obj.field b 1 )       
    else if tag_a = 251 (* abstract_tag *) then 
      raise (Invalid_argument "equal: abstract value")
    else if tag_a != tag_b then
      false      
    else
      let len_a = js_obj_length a in 
      let len_b = js_obj_length b in 
      if len_a = len_b then 
        aux_equal_length a b 0 len_a 
      else false
and aux_equal_length  (a : Obj.t) (b : Obj.t) i same_length = 
  if i = same_length then
    true
  else 
    caml_equal (Obj.field a i) (Obj.field b i) 
    && aux_equal_length  a b (i + 1) same_length

let caml_notequal a  b =  not (caml_equal a  b)

let caml_int32_compare = caml_int_compare
let caml_nativeint_compare = caml_int_compare
let caml_greaterequal a b = caml_compare a b >= 0 

let caml_greaterthan a b = caml_compare a b > 0 

let caml_lessequal a b = caml_compare a b <= 0

let caml_lessthan a b = caml_compare a b < 0



  
