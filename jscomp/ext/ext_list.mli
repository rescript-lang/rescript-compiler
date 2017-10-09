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


val map : ('a -> 'b) -> 'a list -> 'b list 

(** [map_last f xs ]
    will pass [true] to [f] for the last element, 
    [false] otherwise. 
    For empty list, it returns empty
*)
val map_last : (bool -> 'a -> 'b) -> 'a list -> 'b list

(** [last l]
    return the last element
    raise if the list is empty
*)
val last : 'a list -> 'a

val append : 'a list -> 'a list -> 'a list 

val map_append :  ('b -> 'a) -> 'b list -> 'a list -> 'a list

val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

val map2 : 
  ('a -> 'b -> 'c) ->
  'a list ->
  'b list ->
  'c list

val fold_left_with_offset : 
  (int -> 'acc -> 'a -> 'acc) -> 
  int -> 
  'acc -> 
  'a list -> 'acc 


(** @unused *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list  

(** [exclude p l] is the opposite of [filter p l] *)
val exclude : ('a -> bool) -> 'a list -> 'a list 

(** [excludes p l]
    return a tuple [excluded,newl]
    where [exluded] is true indicates that at least one  
    element is removed,[newl] is the new list where all [p x] for [x] is false

*)
val exclude_with_val : ('a -> bool) -> 'a list -> bool * 'a list 


val same_length : 'a list -> 'b list -> bool

val init : int -> (int -> 'a) -> 'a list

(** [split_at n l]
    will split [l] into two lists [a,b], [a] will be of length [n], 
    otherwise, it will raise
*)
val split_at : int -> 'a list -> 'a list * 'a list


(** [split_at_last l]
    It is equivalent to [split_at (List.length l - 1) l ]
*)
val split_at_last : 'a list -> 'a list * 'a

val filter_mapi : 
  (int -> 'a -> 'b option) -> 'a list -> 'b list

val filter_map2 : 
  ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list


val length_compare : 'a list -> int -> [`Gt | `Eq | `Lt ]

val length_ge : 'a list -> int -> bool
(**

   {[length xs = length ys + n ]}
   input n should be positive 
   TODO: input checking
*)

val length_larger_than_n : 
  int -> 'a list -> 'a list -> bool


(**
   [rev_map_append f l1 l2]
   [map f l1] and reverse it to append [l2]
   This weird semantics is due to it is the most efficient operation
   we can do
*)
val rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list


val flat_map : 
  ('a -> 'b list) -> 
  'a list -> 
  'b list

val flat_map_append : 
  ('a -> 'b list) -> 
  'a list -> 
  'b list  ->
  'b list


(**
    [stable_group eq lst]
    Example:
    Input:
   {[
     stable_group (=) [1;2;3;4;3]
   ]}
    Output:
   {[
     [[1];[2];[4];[3;3]]
   ]}
    TODO: this is O(n^2) behavior 
    which could be improved later
*)
val stable_group : ('a -> 'a -> bool) -> 'a list -> 'a list list 

(** [drop n list]
    raise when [n] is negative
    raise when list's length is less than [n]
*)
val drop : int -> 'a list -> 'a list 

(** [find_first_not p lst ]
    if all elements in [lst] pass, return [None] 
    otherwise return the first element [e] as [Some e] which
    fails the predicate
*)
val find_first_not : ('a -> bool) -> 'a list -> 'a option 

(** [find_opt f l] returns [None] if all return [None],  
    otherwise returns the first one. 
*)

val find_opt : ('a -> 'b option) -> 'a list -> 'b option 


val rev_iter : ('a -> unit) -> 'a list -> unit 

(** [for_all2_no_exn p xs ys]
    return [true] if all satisfied,
    [false] otherwise or length not equal
*)
val for_all2_no_exn : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool



(** [f] is applied follow the list order *)
val split_map : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list       

(** [fn] is applied from left to right *)
val reduce_from_left : 
  ('a -> 'a -> 'a) -> 'a list -> 'a

val sort_via_array :
  ('a -> 'a -> int) -> 'a list -> 'a list  




(** [assoc_by_string default key lst]
    if  [key] is found in the list  return that val,
    other unbox the [default], 
    otherwise [assert false ]
*)
val assoc_by_string : 
  'a  option -> string -> (string * 'a) list -> 'a  

val assoc_by_int : 
  'a  option -> int -> (int * 'a) list -> 'a   


val nth_opt : 'a list -> int -> 'a option  