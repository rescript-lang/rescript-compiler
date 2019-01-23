type ('key, 'a) t = ('key, 'a) node Js.null

and ('k,  'v) node  = private {
  mutable key : 'k [@bs.optional];
  mutable value : 'v; 
  height : int; 
  mutable left : ('k,'v) t;
  mutable right : ('k,'v) t
} [@@bs.deriving abstract]

(* include
  sig
    include
      module type of
        (struct
           type ('key,'a) t = ('key,'a) node Js.null
           and ('k,'v) node = private
             {
             mutable key: 'k;
             mutable value: 'v;
             height: int;
             mutable left: ('k,'v) t;
             mutable right: ('k,'v) t;}
         end : sig  end)
    type ('key,'a) t = ('key,'a) node Js.null
    and ('k,'v) node
    val keySet : ('k,'v) node -> 'k -> unit
    val key : ('k,'v) node -> 'k
    val valueSet : ('k,'v) node -> 'v -> unit
    val value : ('k,'v) node -> 'v
    val height : ('k,'v) node -> int
    val leftSet : ('k,'v) node -> ('k,'v) t -> unit
    val left : ('k,'v) node -> ('k,'v) t
    val rightSet : ('k,'v) node -> ('k,'v) t -> unit
    val right : ('k,'v) node -> ('k,'v) t
  end *)

val create : 'a -> 'b -> ('a,'b) t
