
exception Custom_inline of int * int

let v = Custom_inline (1,2)

let vv  = [|1.; 2.; 3.|]

let long_v = [|1.; 2.;3.; 4.; 5.; 6. |]

let long_int_v = [|1; 2;3; 4; 5; 6 |]

let short_int_v = [|1|]
let empty : int array =  [||]

#if 0 then
(* compile error*)
let f () = 
  let a = [||] in 
  Js.Array2.push a 3 |> ignore ;
  Js.Array2.push a "3"  
#end  