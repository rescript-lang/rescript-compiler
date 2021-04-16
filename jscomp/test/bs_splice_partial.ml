

(* let test _g = 
    on_exit_slice3 __LINE__ [|1;2;3|] 

*)    

type t 
external on_exit_slice3 : 
  t 
  -> int 
  -> h:(_ [@bs.as 3]) 
  -> (_ [@bs.as "xxx"]) 
  -> int array
  -> unit 
  = 
  "xx"    [@@send] [@@bs.splice]




let test g = on_exit_slice3 g __LINE__ [|1;2;3|] 




external hi : int -> int array -> int option = "hi"
[@@bs.splice] [@@bs.return {null_to_opt}]
[@@send]


let test_hi x = 
  match x |. hi [|1;2;3|] with 
  | None -> 1 
  | Some y -> Js.log y ; 2


external hi__2 : int -> int array -> int option = "hi__2"
[@@bs.splice] [@@bs.return nullable ]
[@@send]

let test_hi__2 x = 
  match x |. hi__2 [||]with 
  | None -> 1    
  | Some _ -> 2 

type id = int -> int 

external cb : int -> string -> int array -> id = "cb"    
[@@bs.splice] [@@send]


type id2 = int -> int [@bs]
external cb2 : int -> string -> int array -> id2 = "cb2"    
[@@bs.splice] [@@send]


let test_cb x = 
  ignore ((x |. cb "hI" [|1;2;3|] ) 3);
  ignore @@ (cb x "hI" [|1;2;3|] ) 3 ;
  (cb2 x "hI" [|1;2;3|]  ) 3 [@bs]


type u = int -> int [@bs]
external v : u = "v" [@@bs.val]

let f  x = 
  ignore @@ (v x [@bs]) 

external fff0 : int -> int -> (_[@bs.as {json|[undefined,undefined]|json}]) -> int = "say"    
[@@bs.val]

let testUndefined () = 
  fff0 1 2 