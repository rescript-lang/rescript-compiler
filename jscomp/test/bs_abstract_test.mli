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
type u
  val uf : u -> int 
  val uf1 : u -> int -> int 
  val uf2 : u -> int 

type u1  
  val uff : u1 -> int 
  val uff2 : u1 -> int 

  val uff3 : u1 -> int 