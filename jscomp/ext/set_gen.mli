type 'a t0 =private
    Empty
  | Node of { l : 'a t0; v : 'a; r : 'a t0; h : int; }
(* type ('a, 'id) enumeration0 =
    End
  | More of 'a * ('a, 'id) t0 * ('a, 'id) enumeration0 *)
(* val cons_enum : 'a t0-> ('a, 'b) enumeration0 -> ('a, 'b) enumeration0 *)
(* val height : 'a t0-> int *)
val min_elt : 'a t0-> 'a
val max_elt : 'a t0-> 'a
val empty : 'a t0
val is_empty : 'a t0-> bool
(* val cardinal_aux : int -> 'a t0-> int *)
val cardinal : 'a t0-> int
(* val elements_aux : 'a list -> 'a t0-> 'a list *)
val elements : 'a t0-> 'a list
val choose : 'a t0-> 'a
val iter : 'a t0-> ('a -> 'c) -> unit
val fold : 'a t0-> 'c -> ('a -> 'c -> 'c) -> 'c
val for_all : 'a t0-> ('a -> bool) -> bool
val exists : 'a t0-> ('a -> bool) -> bool
(* val max_int_2 : int -> int -> int *)
(* exception Height_invariant_broken
exception Height_diff_borken *)
(* val check_height_and_diff : 'a t0-> int *)
val check : 'a t0-> unit
(* val create : 'a t0-> 'a -> 'a t0-> 'a t0*)
val internal_bal : 'a t0-> 'a -> 'a t0-> 'a t0
val remove_min_elt : 'a t0-> 'a t0
val singleton : 'a -> 'a t0
val internal_merge : 'a t0-> 'a t0-> 'a t0
val add_min_element : 'a -> 'a t0-> 'a t0
val add_max_element : 'a -> 'a t0-> 'a t0
val internal_join : 'a t0-> 'a -> 'a t0-> 'a t0
val internal_concat : 'a t0-> 'a t0-> 'a t0
(* val filter : 'a t0-> ('a -> bool) -> 'a t0 *)
val partition : 'a t0-> ('a -> bool) -> 'a t0 * 'a t0
val of_sorted_list : 'a list -> 'a t0
val of_sorted_array : 'a array -> 'a t0
val is_ordered : cmp:('a -> 'a -> int) -> 'a t0-> bool
val invariant : cmp:('a -> 'a -> int) -> 'a t0-> bool
(* val compare_aux :
  cmp:('a -> 'b -> int) ->
  ('a, 'c) enumeration0 -> ('b, 'd) enumeration0 -> int *)
(* val compare : cmp:('a -> 'b -> int) -> 'a t0 -> ('b, 'd) t0 -> int *)
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
    val max_elt : t -> elt
    val choose : t -> elt
    val mem : t -> elt -> bool
    val add : t -> elt -> t
    val remove : t -> elt -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val find : t -> elt -> elt
    val of_list : elt list -> t
    val of_sorted_list : elt list -> t
    val of_sorted_array : elt array -> t
    val of_array : elt array -> t
    val invariant : t -> bool
    val print : Format.formatter -> t -> unit
  end
