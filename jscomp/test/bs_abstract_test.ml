

type 'a linked_list = 
  {
    hd : 'a ; 
    mutable tl : 'a linked_list Js.null
  }
  [@@bs.deriving abstract]



let v = linked_list ~hd:3 ~tl:Js.null   

;; tlSet v (Js.Null.return v)

type t = int -> int -> bool [@bs]
and x = {
  k : t;
  y : string
} [@@bs.deriving abstract]


let f = x ~k:(fun[@bs] x y -> x = y) ~y:"x"

type u = {
  x : int ; 
  y0 : int -> int;
  y1 : int -> int -> int 
} [@@bs.deriving abstract]


let uf u =  u |. y0 1 
let uf1 u = u |. y1 1 
let uf2 u = u |. y1 1 2

type u1 = {
  x : int; 
  yyyy : (int -> int [@bs]);
  yyyy1 : (int -> int -> int  [@bs]);
  yyyy2 : int -> int  [@bs.optional]
} [@@bs.deriving abstract]

let uff f = 
  (f |. yyyy) 1 [@bs]

let uff2 f =   
  (f |. yyyy1) 1 2 [@bs]

let uff3 f =   
  match f |. yyyy2 with 
  | None -> 0
  | Some x  -> x 0 