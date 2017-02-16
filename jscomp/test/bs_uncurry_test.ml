

external map : 'a array -> ('a -> 'b [@bs.uncurry  ]) -> 'b array = 
    "Array.prototype.map.call"
    [@@bs.val]
    

type id = int -> int     
external map2 : int array ->  (id [@bs.uncurry 1]) -> int array =
    "Array.prototype.map.cal"
    [@@bs.val]

(* [@bs.uncurry n] should not be documented,
    since such inconsistency could not be checked
*)    

(* if we know the return value of type we could do more optimizations here *)
let bs = map  [|1;2;3; 5 |] (fun x -> x + 1 ) 

let f (cb : int -> int ) = 
    map [|1;2;3;4|] cb


let xs = 
    map [|1;1;2|] 
    (fun x y -> y + x + 1 )


external map2 : 
    'a array -> 'b array -> ('a -> 'b -> 'c [@bs.uncurry])
    -> 'c array = "map2"    
    [@@bs.val]


external ff : 
    int -> (int [@bs.ignore]) -> (int -> int -> int [@bs.uncurry]) -> int 
    = "" [@@bs.val]


let f x y z = 
    map2 x y (fun x -> z x)    

let h x y  z = 
    map2 x y z     


let h1 x y u z = 
    map2 x y (z u)    

let add3 x y z = x  + y + z

let h2 x  = 
    ff x 2 (+)

let h3 x = 
    ff x 2 (add3 1 )    


(** used in return value 
    This should fail, we did not 
    support uncurry return value yet
*)
external v3 :
    int -> int -> (int -> int -> int [@bs.uncurry])
    = ""[@@bs.val]
(* ^ should be an error instead of warning *)    


(*
external ff : 
    int -> 
    (unit -> unit [@bs.uncurry]) -> 
    int = 
    ""
[@@bs.val]
*)


(*
So if we pass 
{[ (fun (() as x) ->  Js.log x  ) ]}

Then we call it on JS side 
[g ()], we are passing undefined 
to [x] which is incorrect.

We can also blame users 
[()=>] is really not representable  in OCaml 
You are writing the wrong FFI.... 

They need (fun ()[@bs] -> ..)
Maybe we can create a sugar 
{[ begin [@bs]  .... end  ]} 
*)

(*
external config : 
    hi: (int -> int [@bs.uncurry]) ->    
    lo: int -> 
    unit -> 
    _ = "" [@@bs.obj]

type expected = 
    < hi : int -> int [@bs]; 
      lo : int > Js.t 

let f : expected = 
    config 
    ~hi:(fun x -> x + 1 )
    ~lo:3 
    ()
*)    
(** we auto-uncurry 
so the inferred type would be 
*)

