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
val lengthAux : 'a node -> int
val length0 : 'a t0 -> int

val toList0 : 'a t0 -> 'a list
val checkInvariant : _ t0 -> bool
val toArray0 : 'a t0 -> 'a array
val balMutate : 'a node -> 'a node
val removeMinAuxMutateWithRoot : 'a node -> 'a node -> 'a t0
val ofSortedArrayAux : 'a array -> int -> int -> 'a t0
val ofSortedArrayUnsafe0 : 'a array -> 'a t0
