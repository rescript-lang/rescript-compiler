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









type t = Bs_obj.t 


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
let caml_obj_block tag size = 
  let v = Bs_obj.repr (Caml_array.new_uninitialized size) in 
  Bs_obj.set_tag  v tag ; 
  v

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

let caml_obj_dup (x : Bs_obj.t) =
  let len = Bs_obj.length x in
  let v = Caml_array.new_uninitialized  len in
  for i = 0 to len - 1 do
    Caml_array.unsafe_set v i (Bs_obj.field x i)
  done;
  Bs_obj.set_tag (Bs_obj.repr v) (Bs_obj.tag x );
  Bs_obj.repr v



let caml_obj_truncate (x : Bs_obj.t) (new_size : int) =
  let len = Bs_obj.length x in
  if new_size <= 0 || new_size > len then
    raise (Invalid_argument "Obj.truncate")
  else
  if len <> new_size  then
    begin
      for i = new_size  to len - 1  do
        Bs_obj.set_field x  i (Obj.magic 0)
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
     Array.unsafe_set x i (Bs_obj.field y i)
     done;
     Bs_obj.set_tag (Obj.magic x) (Bs_obj.tag y)
  *)
  let len = Bs_obj.length y in
  for i = 0 to len - 1 do
    Bs_obj.set_field x i (Bs_obj.field y i)
  done ; 
  let y_tag = Bs_obj.tag y in 
  if y_tag <> 0 then
    Bs_obj.set_tag x y_tag
(* Bs_obj.set_length x   (Bs_obj.length y) *)
(* [set_length] seems redundant here given that it is initialized as an array 
*)

type 'a selector = 'a -> 'a -> 'a 

module O = struct
  external isArray : 'a -> bool = "Array.isArray" [@@bs.val]
  type key = string
  let for_in : (Bs_obj.t -> (key -> unit) -> unit)  = 
    fun%raw o foo ->  {|
        for (var x in o) { foo(x) }
      |}
    
  let hasOwnProperty (o: Bs_obj.t) (key: key) : bool = (Obj.magic o)##hasOwnProperty(key)
  external get_value : Bs_obj.t -> key -> Bs_obj.t = ""[@@bs.get_index]
end

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
let rec caml_compare (a : Bs_obj.t) (b : Bs_obj.t) : int =
  if a == b then 0 else
  (*front and formoest, we do not compare function values*)
  let a_type = Js.typeof a in 
  let b_type = Js.typeof b in 
  match a_type, b_type with 
  | "undefined", _ -> - 1
  | _, "undefined" -> 1 
  (** [a] is of type string, b can not be None,
      [a] could be (Some (Some x)) in that case [b] could be [Some None] or [null]
       so [b] has to be of type string or null *)       
  | "string", "string" ->   
      Pervasives.compare (Obj.magic a : string) (Obj.magic b )
  | "string", _ -> 
      (* [b] could be [Some None] or [null] *)
      1 
  |  _, "string" -> -1  
  | "boolean", "boolean" ->       
      Pervasives.compare (Obj.magic a : bool) (Obj.magic b)
  | "boolean", _ -> 1     
  | _, "boolean" -> -1
  | "function", "function" -> 
      raise (Invalid_argument "compare: functional value")
  | "function", _ -> 1
  | _, "function" -> -1 
  | "number", "number" -> 
      Pervasives.compare (Obj.magic a : int) (Obj.magic b : int)
  | "number", _ ->        
      if b == Bs_obj.repr Js.null || Bs_obj.tag b = 256 then 1 (* Some (Some ..) < x *)
      else 
        -1 (* Integer < Block in OCaml runtime GPR #1195, except Some.. *)
  | _, "number" -> 
      if a == Bs_obj.repr Js.null || Bs_obj.tag a = 256 then -1
      else 1
  | _ ->        
      if a == Bs_obj.repr Js.null then 
        (** [b] could not be null otherwise would equal *)
        if Bs_obj.tag b = 256 then 1 else -1
     else if b == Bs_obj.repr Js.null then 
        if Bs_obj.tag a = 256 then -1 else 1    
     else    
        let tag_a = Bs_obj.tag a in
        let tag_b = Bs_obj.tag b in
        (* double_array_tag: 254
           forward_tag:250
        *)
        if tag_a = 250 then
          caml_compare (Bs_obj.field a 0) b
        else if tag_b = 250 then
          caml_compare a (Bs_obj.field b 0)
        else if tag_a = 256 then   
          if tag_b = 256 then 
            Pervasives.compare (Obj.magic (Bs_obj.field a 1) : int)
             (Obj.magic (Bs_obj.field b 1) : int)
            (* Some None < Some (Some None)) *)
          else  (* b could not be undefined/None *)
             (* Some None < Some ..*) 
             -1 
        else if tag_a = 248 (* object/exception *)  then
          Pervasives.compare (Obj.magic (Bs_obj.field a 1) : int) (Obj.magic (Bs_obj.field b 1 ))
        else if tag_a = 251 (* abstract_tag *) then
          raise (Invalid_argument "equal: abstract value")
        else if tag_a <> tag_b then
          if tag_a < tag_b then (-1) else  1
        else
          let len_a = Bs_obj.length a in
          let len_b = Bs_obj.length b in
          if len_a = len_b then
            if O.isArray(a)
            then aux_same_length a b 0 len_a
            else aux_obj_compare a b
          else if len_a < len_b then
            aux_length_a_short a b 0 len_a
          else
            aux_length_b_short a b 0 len_b
and aux_same_length  (a : Bs_obj.t) (b : Bs_obj.t) i same_length =
  if i = same_length then
    0
  else
    let res = caml_compare (Bs_obj.field a i) (Bs_obj.field b i) in
    if res <> 0 then res
    else aux_same_length  a b (i + 1) same_length
and aux_length_a_short (a : Bs_obj.t)  (b : Bs_obj.t)  i short_length    =
  if i = short_length then -1
  else
    let res = caml_compare (Bs_obj.field a i) (Bs_obj.field b i) in
    if res <> 0 then res
    else aux_length_a_short a b (i+1) short_length
and aux_length_b_short (a : Bs_obj.t) (b : Bs_obj.t) i short_length =
  if i = short_length then 1
  else
    let res = caml_compare (Bs_obj.field a i) (Bs_obj.field b i) in
    if res <> 0 then res
    else aux_length_b_short a b (i+1) short_length
and aux_obj_compare (a: Bs_obj.t) (b: Bs_obj.t) =
  let min_key_lhs = ref None in
  let min_key_rhs = ref None in
  let do_key (a, b, min_key) key =
    if not (O.hasOwnProperty b key) ||
       caml_compare (O.get_value a key) (O.get_value b key) > 0
    then
      match !min_key with
      | None -> min_key := Some key
      | Some mk ->
        if key < mk then min_key := Some key in
  let do_key_a = do_key (a, b, min_key_rhs) in
  let do_key_b = do_key (b, a, min_key_lhs) in
  O.for_in a do_key_a;
  O.for_in b do_key_b;
  let res = match !min_key_lhs, !min_key_rhs with
    | None, None -> 0
    | (Some _), None -> -1
    | None, (Some _) -> 1
    | (Some x), (Some y) -> Pervasives.compare x y in
  res

type eq = Bs_obj.t -> Bs_obj.t -> bool


(** It is easier to do equality check than comparision, since as long as its
  basic type is not the same, it will not equal 
*)
let rec caml_equal (a : Bs_obj.t) (b : Bs_obj.t) : bool =
  (*front and formoest, we do not compare function values*)
  if a == b then true
  else 
    let a_type = Js.typeof a in 
    if a_type = "string"
    ||  a_type = "number"
    ||  a_type = "boolean"
    ||  a_type = "undefined"
    ||  a == [%raw {|null|}]
    then false
    else 
      let b_type = Js.typeof b in 
      if a_type = "function" || b_type = "function"
      then raise (Invalid_argument "equal: functional value")
      (* first, check using reference equality *)
      else (* a_type = "object" || "symbol" *)
      if b_type = "number" || b_type = "undefined" || b == [%raw{|null|}] then false 
      else 
        (* [a] [b] could not be null, so it can not raise *)
        let tag_a = Bs_obj.tag a in
        let tag_b = Bs_obj.tag b in
        (* double_array_tag: 254
           forward_tag:250
        *)
        if tag_a = 250 then
          caml_equal (Bs_obj.field a 0) b
        else if tag_b = 250 then
          caml_equal a (Bs_obj.field b 0)
        else if tag_a = 248 (* object/exception *)  then
          (Obj.magic  (Bs_obj.field a 1)) ==  (Obj.magic (Bs_obj.field b 1 ))
        else if tag_a = 251 (* abstract_tag *) then
          raise (Invalid_argument "equal: abstract value")
        else if tag_a <> tag_b then
          false
        else if tag_a = 256 then 
          (Obj.magic (Bs_obj.field a 1) : int) = Obj.magic (Bs_obj.field b 1)
        else 
          let len_a = Bs_obj.length a in
          let len_b = Bs_obj.length b in
          if len_a = len_b then
            if O.isArray(a)
            then aux_equal_length a b 0 len_a
            else aux_obj_equal a b
          else false
and aux_equal_length  (a : Bs_obj.t) (b : Bs_obj.t) i same_length =
  if i = same_length then
    true
  else
    caml_equal (Bs_obj.field a i) (Bs_obj.field b i)
    && aux_equal_length  a b (i + 1) same_length
and aux_obj_equal (a: Bs_obj.t) (b: Bs_obj.t) =
  let result = ref true in
  let do_key_a key =
    if not (O.hasOwnProperty b key)
    then result := false in
  let do_key_b key =
    if not (O.hasOwnProperty a key) ||
       not (caml_equal (O.get_value b key) (O.get_value a key))
    then result := false in
  O.for_in a do_key_a ;
  if !result then O.for_in b do_key_b;
  !result

let caml_equal_null (x : Bs_obj.t) (y : Bs_obj.t Js.null) = 
  match Js.nullToOption y with    
  | None -> x == (Obj.magic y)
  | Some y -> caml_equal x y 

let caml_equal_undefined (x : Bs_obj.t) (y : Bs_obj.t Js.undefined) =    
  match Js.undefinedToOption y with 
  | None -> x == (Obj.magic y)
  | Some y -> caml_equal x y 

let caml_equal_nullable ( x: Bs_obj.t) (y : Bs_obj.t Js.nullable) =    
  match Js.toOption  y with 
  | None -> x == (Obj.magic y)
  | Some y -> caml_equal x y

let caml_notequal a  b =  not (caml_equal a  b)

let caml_greaterequal a b = caml_compare a b >= 0

let caml_greaterthan a b = caml_compare a b > 0

let caml_lessequal a b = caml_compare a b <= 0

let caml_lessthan a b = caml_compare a b < 0

let caml_min (x : Bs_obj.t) y =   
  if caml_compare  x y <= 0 then x else y 

let caml_max (x : Bs_obj.t) y =    
  if caml_compare x y >= 0 then x else y 

let caml_obj_set_tag = Bs_obj.set_tag  