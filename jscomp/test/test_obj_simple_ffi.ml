type t 
external mk_obj_spec : ?displayName:string -> test:int -> config:int  -> hi:string -> 
  unit ->  t =
  "caml_ignore" [@@bs.obj]
let v ?displayName () = mk_obj_spec ~test:3 ~config:3 ~hi:"ghos" ?displayName ()
let v2  = mk_obj_spec ~test:3 ~config:3 ~hi:"ghos"  ()
let v3  = mk_obj_spec ~test:3 ~config:3 ~hi:"ghos" ~displayName:"display" ()


class type x = object 
  method tet : (x Js.t -> int -> int -> int  [@meth])
end


class type y = object 
  method tet : y Js.t -> int -> int -> int  [@meth]
end


let u (x : x) : y = x 

type h = < bark : 'self -> int -> int [@meth] > Js.t as 'self
type hh = < bark :( 'self -> int -> int [@meth]) > Js.t as 'self

let ff (x  : h) : hh = x 

let f (u : x Js.t) = 
  u#.tet (1,2)


type 'a return = int -> 'a [@fn]
type 'a u = int -> string -> 'a return  [@fn]

type 'a u2 = int -> string -> int -> 'a [@fn]

type 'a u3 = int -> string -> (int -> 'a [@fn]) [@fn]

let fff (x : 'a u) :  'a u2 = 
  x 


let fff (x : 'a u) :  'a u3 = 
  x 


type 'a ret = 'a -> int [@uncurry]

type 'a u4 = < case : (int -> 'a ret ) > 

type 'a u5 = < case : (int -> (int -> 'a [@uncurry])) > 

let ff ( x : 'a u4) : 'a u5 =  x 


type 'a v0 = int -> 'a ret [@uncurry]
type 'a v1 =  int -> ('a -> int [@uncurry]) [@uncurry]

type 'a xx = int -> 'a [@meth]
type 'a w0 = int -> 'a xx [@uncurry]
type 'a w1 =  int -> (int -> 'a [@meth]) [@uncurry]
let f (x : 'a w0) : 'a w1 =  x

type 'a v2 = int -> 'a ret [@meth]
type 'a v3 = int -> ('a -> int [@uncurry]) [@meth]


let f (x : 'a v0) : 'a v1 = x
let ff (x : 'a v2 ) : 'a v3 = x 
 
type 'a u6 = < case : int -> 'a ret  ; >  [@uncurry] 

type 'a u7 = < case : int -> ('a  -> int [@uncurry])  ; >  [@uncurry]

let fff (x : 'a u6) : 'a u7= x 

let f : int -> int -> (int -> int -> int [@uncurry]) [@uncurry] = 
  fun [@uncurry] (x,y) -> fun [@uncurry](u,v) -> x + y + u + v
