type 'a t =private
    Empty
  | Leaf of 'a
  | Node of { l : 'a t; v : 'a; r : 'a t; h : int; }
(* type ('a, 'id) enumeration0 =
    End
  | More of 'a * ('a, 'id) t * ('a, 'id) enumeration0 *)
(* val cons_enum : 'a t-> ('a, 'b) enumeration0 -> ('a, 'b) enumeration0 *)
(* val height : 'a t-> int *)
val min_elt : 'a t-> 'a
val max_elt : 'a t-> 'a
val empty : 'a t
val is_empty : 'a t-> bool
val unsafe_two_elements : 
  'a -> 'a -> 'a t
(* val cardinal_aux : int -> 'a t-> int *)
val cardinal : 'a t-> int
(* val elements_aux : 'a list -> 'a t-> 'a list *)
val elements : 'a t-> 'a list
val choose : 'a t-> 'a
val iter : 'a t-> ('a -> unit) -> unit
val fold : 'a t-> 'c -> ('a -> 'c -> 'c) -> 'c
val for_all : 'a t-> ('a -> bool) -> bool
val exists : 'a t-> ('a -> bool) -> bool
(* val max_int_2 : int -> int -> int *)
(* exception Height_invariant_broken
exception Height_diff_borken *)
(* val check_height_and_diff : 'a t-> int *)
val check : 'a t-> unit
(* val create : 'a t-> 'a -> 'a t-> 'a t*)
val internal_bal : 'a t-> 'a -> 'a t-> 'a t
val remove_min_elt : 'a t-> 'a t
val singleton : 'a -> 'a t
val internal_merge : 'a t-> 'a t-> 'a t
val add_min_element : 'a -> 'a t-> 'a t
val add_max_element : 'a -> 'a t-> 'a t
val internal_join : 'a t-> 'a -> 'a t-> 'a t
val internal_concat : 'a t-> 'a t-> 'a t
(* val filter : 'a t-> ('a -> bool) -> 'a t *)
val partition : 'a t-> ('a -> bool) -> 'a t * 'a t
val of_sorted_array : 'a array -> 'a t
val is_ordered : cmp:('a -> 'a -> int) -> 'a t-> bool
val invariant : cmp:('a -> 'a -> int) -> 'a t-> bool
(* val compare_aux :
  cmp:('a -> 'b -> int) ->
  ('a, 'c) enumeration0 -> ('b, 'd) enumeration0 -> int *)
(* val compare : cmp:('a -> 'b -> int) -> 'a t -> ('b, 'd) t -> int *)
module type S =
  sig
    type elt
    type t
    val empty : t
    val is_empty : t -> bool
    val iter : t -> (elt -> unit) -> unit
    val fold : t -> 'a -> (elt -> 'a -> 'a) -> 'a
    val for_all : t -> (elt -> bool) -> bool
    val exists : t -> (elt -> bool) -> bool
    val singleton : elt -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    (* val max_elt : t -> elt *)
    val choose : t -> elt
    val mem : t -> elt -> bool
    val add : t -> elt -> t
    val remove : t -> elt -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val find : t -> elt -> elt
    val of_list : elt list -> t
    val of_sorted_array : elt array -> t
    (* val of_array : elt array -> t *)
    val invariant : t -> bool
    val print : Format.formatter -> t -> unit
  end
