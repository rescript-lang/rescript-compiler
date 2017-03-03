

(* let test _g = 
    on_exit_slice3 __LINE__ [|1;2;3|] 

*)    
  
type t 
external on_exit_slice3 : 
    int 
    -> (_ [@bs.as 3]) 
    -> (_ [@bs.as "xxx"]) 
    -> int array
    -> unit 
    = 
    "xx"    [@@bs.send.pipe: t] [@@bs.splice]




(* let test _g = 
    on_exit_slice3 __LINE__ [|1;2;3|] 

*)    


external hi : int array -> int option = ""
    [@@bs.splice] [@@bs.return {null_to_opt}]
    [@@bs.send.pipe:int]


let test_hi x = 
    match x |> hi [|1;2;3|] with 
    | None -> 1 
    | Some y -> Js.log y ; 2