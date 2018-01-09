(* This internal module 
  contains methods which does not rely on ordering. 
  Such methods could be shared between 
  [generic set/specalized set] whether mutable or immutable depends on use cases
*)
type 'elt t0 = 'elt node Js.null
and 'elt node  = private {
  mutable left : 'elt t0;
   key : 'elt ; 
  mutable right : 'elt t0;
  h : int 
} [@@bs.deriving abstract]
(* TODO: node is used in [subset] *)
external toOpt : 'a Js.null -> 'a option = "#null_to_opt"
external return : 'a -> 'a Js.null = "%identity"
external empty : 'a Js.null = "#null"



val copy : 'a t0 -> 'a t0
val create : 'a t0 -> 'a -> 'a t0 -> 'a t0
val bal : 'a t0 -> 'a -> 'a t0 -> 'a t0
val singleton0 : 'a -> 'a t0

val min0Aux : 'a node -> 'a
val minOpt0 : 'a t0 -> 'a option
val minNull0 : 'a t0 -> 'a Js.null
val max0Aux : 'a node -> 'a
val maxOpt0 : 'a t0 -> 'a option
val maxNull0 : 'a t0 -> 'a Js.null

val removeMinAuxWithRef : 'a node -> 'a ref -> 'a t0
(* [removeMinAuxWithRef n cell] return a new node with minimum removed and stored in cell *)
val empty0 : 'a t0
val isEmpty0 : 'a t0 -> bool
val stackAllLeft : 'a t0 -> 'a node list -> 'a node list
val iter0 : 'a t0 -> ('a -> 'b [@bs]) -> unit
val fold0 : 'a t0 -> 'b -> ('b -> 'a -> 'b [@bs]) -> 'b
val forAll0 : 'a t0 -> ('a -> bool [@bs]) -> bool
val exists0 : 'a t0 -> ('a -> bool [@bs]) -> bool
val join : 'a t0 -> 'a -> 'a t0 -> 'a t0
val concat : 'a t0 -> 'a t0 -> 'a t0
val filter0 : 'a t0 -> ('a -> bool [@bs]) -> 'a t0
val partition0 :
  'a t0 -> ('a -> bool [@bs]) -> 'a t0 * 'a t0

val length0 : 'a t0 -> int

val toList0 : 'a t0 -> 'a list
val checkInvariant : _ t0 -> bool
val toArray0 : 'a t0 -> 'a array
val ofSortedArrayAux : 'a array -> int -> int -> 'a t0
val ofSortedArrayUnsafe0 : 'a array -> 'a t0
val mem0 : cmp:('a, 'b) Bs_Cmp.cmp -> 'a t0 -> 'a -> bool
val cmp0 : 'a t0 -> 'a t0 -> cmp:('a, 'b) Bs_Cmp.cmp -> int
val eq0 : cmp:('a, 'b) Bs_Cmp.cmp -> 'a t0 -> 'a t0 -> bool
val subset0 : cmp:('a, 'b) Bs_Cmp.cmp -> 'a t0 -> 'a t0 -> bool
val findOpt0 : cmp:('a, 'b) Bs_Cmp.cmp -> 'a t0 -> 'a -> 'a option
val findNull0 : cmp:('a, 'b) Bs_Cmp.cmp -> 'a t0 -> 'a -> 'a Js.null


val ofArray0 : cmp:('a, 'b) Bs_Cmp.cmp -> 'a array -> 'a t0


val addMutate : cmp:('a, 'b) Bs_Cmp.cmp -> 'a t0 -> 'a -> 'a t0
val balMutate : 'a node -> 'a node
val removeMinAuxWithRootMutate : 'a node -> 'a node -> 'a t0
(* [rmeoveMinAuxMutateWithRoot root n]
   remove the minimum of n in place and store its value in the [key root]
 *)