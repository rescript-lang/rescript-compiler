
(*
To work around unused attribute checking

- we mark it used in ppx stage
we can not mark it in parsing since it won't
works for reason
*)
type t = {
  mutable x : int [@bs.as "THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE"]
   (* test key word later *)
}



let f x  = { x}

let set x = x.x <- 3 ; x.x * 2

type x = t = private {
  mutable x : int [@bs.as "THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE"]
}

type t0 = { x: t0 ; y : int}

let f1 u = 
match u with 
| {x = { x = {x={y}}}}   ->     y

type t1 = {
  mutable x' : int
}


let f2 (x : t1) = 
  x.x' <- x.x' + 3;
  {x' = x.x' + 3}  

type t2 = {
  mutable x' : int [@bs.as "open"]
}  

let f3 (x : t2) = 
  x.x' <- x.x' + 3;
  {x' = x.x' + 3}  

type t3 = {
    mutable x' : int [@bs.as "in"]
  }  
  
let f3 (x : t3) = 
  x.x' <- x.x' + 3;
  {x' = x.x' + 3}  

type entry  = {
  x : int  ; [@bs.as "EXACT_MAPPING_TO_JS_LABEL"]
  y : int ; [@bs.as "EXACT_2"]
  z : obj
} 
and obj = {
  hi : int ; [@bs.as "hello"]  
}    


let f4  ({ x; y; z = {hi }}: entry) = 
  (x + y + hi) * 2


#if 0 then
type t5 = {
  x : int ;
  y : int [@bs.as "x"] 
  (* either x or y is a mistake *)
}   

let v5  = {x = 3 ; y = 2}
#end

type t6 = {
  x : int [@bs.as "x"];
  y : int [@bs.as "y"]
}
(* allow this case *)


external ff : x:int -> h:(_[@bs.as 3]) -> _ = "" [@@bs.obj] 
external ff2 : x:int -> h:(_[@bs.as 3]) -> <x:int>  = "" [@@bs.obj] 
let u () = 
    ignore (ff ~x:3 );
    ff2 ~x:22