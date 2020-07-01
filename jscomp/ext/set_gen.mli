type ('a, 'id) t0 =private
    Empty
  | Node of { l : ('a, 'id) t0; v : 'a; r : ('a, 'id) t0; h : int; }
(* type ('a, 'id) enumeration0 =
    End
  | More of 'a * ('a, 'id) t0 * ('a, 'id) enumeration0 *)
(* val cons_enum : ('a, 'b) t0 -> ('a, 'b) enumeration0 -> ('a, 'b) enumeration0 *)
(* val height : ('a, 'b) t0 -> int *)
val min_elt : ('a, 'b) t0 -> 'a
val max_elt : ('a, 'b) t0 -> 'a
val empty : ('a, 'b) t0
val is_empty : ('a, 'b) t0 -> bool
(* val cardinal_aux : int -> ('a, 'b) t0 -> int *)
val cardinal : ('a, 'b) t0 -> int
(* val elements_aux : 'a list -> ('a, 'b) t0 -> 'a list *)
val elements : ('a, 'b) t0 -> 'a list
val choose : ('a, 'b) t0 -> 'a
val iter : ('a, 'b) t0 -> ('a -> 'c) -> unit
val fold : ('a, 'b) t0 -> 'c -> ('a -> 'c -> 'c) -> 'c
val for_all : ('a, 'b) t0 -> ('a -> bool) -> bool
val exists : ('a, 'b) t0 -> ('a -> bool) -> bool
(* val max_int_2 : int -> int -> int *)
(* exception Height_invariant_broken
exception Height_diff_borken *)
(* val check_height_and_diff : ('a, 'b) t0 -> int *)
val check : ('a, 'b) t0 -> unit
(* val create : ('a, 'b) t0 -> 'a -> ('a, 'b) t0 -> ('a, 'b) t0 *)
val internal_bal : ('a, 'b) t0 -> 'a -> ('a, 'b) t0 -> ('a, 'b) t0
val remove_min_elt : ('a, 'b) t0 -> ('a, 'b) t0
val singleton : 'a -> ('a, 'b) t0
val internal_merge : ('a, 'b) t0 -> ('a, 'b) t0 -> ('a, 'b) t0
val add_min_element : 'a -> ('a, 'b) t0 -> ('a, 'b) t0
val add_max_element : 'a -> ('a, 'b) t0 -> ('a, 'b) t0
val internal_join : ('a, 'b) t0 -> 'a -> ('a, 'b) t0 -> ('a, 'b) t0
val internal_concat : ('a, 'b) t0 -> ('a, 'b) t0 -> ('a, 'b) t0
val filter : ('a, 'b) t0 -> ('a -> bool) -> ('a, 'c) t0
val partition : ('a, 'b) t0 -> ('a -> bool) -> ('a, 'c) t0 * ('a, 'd) t0
val of_sorted_list : 'a list -> ('a, 'b) t0
val of_sorted_array : 'a array -> ('a, 'b) t0
val is_ordered : cmp:('a -> 'a -> int) -> ('a, 'b) t0 -> bool
val invariant : cmp:('a -> 'a -> int) -> ('a, 'b) t0 -> bool
(* val compare_aux :
  cmp:('a -> 'b -> int) ->
  ('a, 'c) enumeration0 -> ('b, 'd) enumeration0 -> int *)
val compare : cmp:('a -> 'b -> int) -> ('a, 'c) t0 -> ('b, 'd) t0 -> int
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
