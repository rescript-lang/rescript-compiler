type 'a linked_list = private
  {
    (* hd : 'a ;  *)
    tl : 'a linked_list Js.null
  }
  [@@bs.deriving abstract]



  type t = int -> int -> bool [@bs]
  and x = private {
    k : t;
    (* y : string *)
  } [@@bs.deriving abstract]


  val f :  x 