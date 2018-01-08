

type ('key, 'a) t0 = ('key, 'a) node Js.null
and ('k, + 'v) node  = {
  left : ('k,'v) t0;
  key : 'k; 
  value : 'v; 
  right : ('k,'v) t0;
  h : int 
} [@@bs.deriving abstract]

external toOpt : 'a Js.null -> 'a option = "#null_to_opt"
external return : 'a -> 'a Js.null = "%identity"
external empty : 'a Js.null = "#null"


val height : _ t0 -> int

val create :
  ('a,'b) t0 -> 'a -> 'b -> ('a,'b) t0 -> ('a,'b) t0
val singleton0 : 'a -> 'b -> ('a,'b) t0

val bal :
  ('a,'b) t0 -> 'a -> 'b -> ('a,'b) t0 -> ('a,'b) t0

val empty0 : _ t0

val isEmpty0 : _ t0 -> bool

val minBinding0 : ('a,'b) t0 -> ('a * 'b) option

val maxBinding0 : ('a,'b) t0 -> ('a * 'b) option
val removeMinAux : ('a, 'b) node -> ('a,'b) t0
val merge : ('a,'b) t0 -> ('a,'b) t0 -> ('a,'b) t0
val iter0 : ('a -> 'b -> 'c [@bs]) -> ('a,'b) t0 -> unit
val map0 : ('a -> 'b [@bs]) -> ('c, 'a) t0 -> ('c, 'b) t0
val mapi0 :
  ('a -> 'b -> 'c [@bs]) -> ('a,'b) t0 -> ('a, 'c) t0
val fold0 : ('a -> 'b -> 'c -> 'c [@bs]) -> ('a,'b) t0 -> 'c -> 'c
val forAll0 : ('a -> 'b -> bool [@bs]) -> ('a,'b) t0 -> bool
val exists0 : ('a -> 'b -> bool [@bs]) -> ('a,'b) t0 -> bool

val join : ('a,'b) t0 -> 'a -> 'b -> ('a,'b) t0 -> ('a, 'b) t0

val concat : ('a,'b) t0 -> ('a,'b) t0 -> ('a,'b) t0

val concat_or_join :
  ('a,'b) t0 -> 'a -> 'b option -> ('a,'b) t0 -> ('a, 'b) t0
val filter0 : ('a -> 'b -> bool [@bs]) -> ('a,'b) t0 -> ('a,'b) t0
val partition0 :
  ('a -> 'b -> bool [@bs]) ->
  ('a,'b) t0 -> ('a,'b) t0 * ('a,'b) t0

val stackAllLeft :
  ('a,'b) t0 -> ('a, 'b) node list -> ('a, 'b) node list
val lengthAux : ('a, 'b) node -> int
val length0 : ('a,'b) t0 -> int
val bindings_aux : ('a * 'b) list -> ('a,'b) t0 -> ('a * 'b) list
val bindings0 : ('a,'b) t0 -> ('a * 'b) list
val checkInvariant : ('a,'b) t0 -> bool
