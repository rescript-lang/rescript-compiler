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

(**
   Since now we change it back to use
   Array representation
   this function is higly dependent
   on how objects are encoded in buckle.

   There are potentially some issues with wrong implementation of
   `caml_obj_dup`, for example, people call `Obj.dup` for a record,
   and new record, since currently, `new record` will generate a
   `slice` function (which assume the record is an array), and the
   output is no longer an array. (it might be  something like { 0 : x , 1 : y} )

   {[
     let u : record = Obj.dup x in
     let h = {u with x = 3}
   ]}

   ==>

   {[
     var u = caml_obj_dup (x)
       var new_record = u.slice ()

   ]}
*)

let caml_obj_dup (x : Obj.t) =
  let len = Bs_obj.length x in
  let v = Caml_array.new_uninitialized  len in
  for i = 0 to len - 1 do
    Array.unsafe_set v i (Obj.field x i)
  done;
  Obj.set_tag (Obj.repr v) (Bs_obj.tag x );
  Obj.repr v



let caml_obj_truncate (x : Obj.t) (new_size : int) =
  let len = Bs_obj.length x in
  if new_size <= 0 || new_size > len then
    raise (Invalid_argument "Obj.truncate")
  else
  if len <> new_size  then
    begin
      for i = new_size  to len - 1  do
        Obj.set_field x  i (Obj.magic 0)
      done;
      Bs_obj.set_length x new_size
    end



let caml_lazy_make_forward x = lazy x

(** 
   For the empty dummy object, whether it's 
   [[]] or [{}] depends on how 
   runtime encoding works, and will affect 
   js polymorphic comparison(Js.(=)) (fine with caml polymoprhic comparison (Pervasives.equal))
   In most cases, rec value comes from record/modules, 
   whose tag is 0, we optimize that case
*)
let caml_update_dummy x y =
  (* let len = Bs_obj.length y in   
     for i = 0 to len - 1 do 
     Array.unsafe_set x i (Obj.field y i)
     done;
     Obj.set_tag (Obj.magic x) (Obj.tag y)
  *)
  let len = Bs_obj.length y in
  for i = 0 to len - 1 do
    Obj.set_field x i (Obj.field y i)
  done ; 
  let y_tag = Obj.tag y in 
  if y_tag <> 0 then
    Obj.set_tag x y_tag

(* Bs_obj.set_length x   (Bs_obj.length y) *)
(* [set_length] seems redundant here given that it is initialized as an array 
*)
let caml_int_compare (x : int) (y: int) : int =
  if  x < y then -1 else if x = y then 0 else  1

let caml_string_compare (x : string) (y: string) : int =
  if  x < y then -1 else if x = y then 0 else  1

let unsafe_js_compare x y =
  if x == y then 0 else
  if Js.unsafe_lt x y then -1
  else 1
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
  if a == b then 0 else
  (*front and formoest, we do not compare function values*)
  let a_type = Js.typeof a in 
  let b_type = Js.typeof b in 
  if a_type = "string" then
    caml_string_compare (Obj.magic a) (Obj.magic b )
  else 
    let is_a_number = a_type = "number" in 
    let is_b_number = b_type = "number" in 
    match is_a_number , is_b_number with 
    | true, true -> 
      caml_int_compare (Obj.magic a) (Obj.magic b )
    | true , false -> -1 (* Integer < Block in OCaml runtime GPR #1195 *)
    | false, true -> 1 
    | false, false -> 
      if a_type = "boolean"
      || a_type = "undefined"
      || a == (Obj.repr Js_null.empty)
      then (* TODO: refine semantics when comparing with [null] *)
        unsafe_js_compare a b
      else if a_type = "function" || b_type = "function"
      then raise (Invalid_argument "compare: functional value")
      else
        (* if #is_instance_array a then  *)
        (*   0 *)
        (* else  *)
        let tag_a = Bs_obj.tag a in
        let tag_b = Bs_obj.tag b in
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
        else if tag_a <> tag_b then
          if tag_a < tag_b then (-1) else  1
        else
          let len_a = Bs_obj.length a in
          let len_b = Bs_obj.length b in
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
    if res <> 0 then res
    else aux_same_length  a b (i + 1) same_length
and aux_length_a_short (a : Obj.t)  (b : Obj.t)  i short_length    =
  if i = short_length then -1
  else
    let res = caml_compare (Obj.field a i) (Obj.field b i) in
    if res <> 0 then res
    else aux_length_a_short a b (i+1) short_length
and aux_length_b_short (a : Obj.t) (b : Obj.t) i short_length =
  if i = short_length then 1
  else
    let res = caml_compare (Obj.field a i) (Obj.field b i) in
    if res <> 0 then res
    else aux_length_b_short a b (i+1) short_length

type eq = Obj.t -> Obj.t -> bool


(** It is easier to do equality check than comparision, since as long as its
  basic type is not the same, it will not equal 
*)
let rec caml_equal (a : Obj.t) (b : Obj.t) : bool =
  (*front and formoest, we do not compare function values*)
  if a == b then true
  else 
    let a_type = Js.typeof a in 
    if a_type = "string"
    ||  a_type = "number"
    ||  a_type = "boolean"
    ||  a_type = "undefined"
    ||  a == (Obj.magic Js_null.empty)
    then false
    else 
      let b_type = Js.typeof b in 
      if a_type = "function" || b_type = "function"
      then raise (Invalid_argument "equal: functional value")
      (* first, check using reference equality *)
      else (* a_type = "object" || "symbol" *)
      if b_type = "number" || b_type = "undefined" || b == Obj.magic Js_null.empty then false 
      else 
        let tag_a = Bs_obj.tag a in
        let tag_b = Bs_obj.tag b in
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
        else if tag_a <> tag_b then
          false
        else
          let len_a = Bs_obj.length a in
          let len_b = Bs_obj.length b in
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
