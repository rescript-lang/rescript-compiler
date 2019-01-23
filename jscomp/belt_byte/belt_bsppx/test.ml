type ('k, 'v) node  = {
  mutable key : 'k [@bs.optional];
  mutable value : 'v;
  mutable height : int;
  mutable left : ('k,'v) t;
  mutable right : ('k,'v) t
}
and ('key, 'a) t = ('key, 'a) node Js.null
[@@bs.deriving abstract]


let create x y =
    Js.Null.return (node ~left:Js.null ~key:x ~value:y ~right:Js.null
       ~height:1 ())
