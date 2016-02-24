


let f1 x = 
  match Js.from_opt x with 
  | None -> 
    let sum x y = x + y in 
    sum 1 2 
  | Some x -> 
    let sum x y = x + y in 
    sum x 1

let f2 x = 
  let u = Js.from_opt x in
  match  u with 
  | None -> 
    let sum x y = x + y in 
    sum 1 2 
  | Some x -> 
    let sum x y = x + y in 
    sum x 1



let f5 h x = 
  let u = Js.from_opt @@ h 32 in
  match  u with 
  | None -> 
    let sum x y = x + y in 
    sum 1 2
  | Some x -> 
    let sum x y = x + y in 
    sum x 1

let f4 h x = 
  let u = Js.from_opt @@ h 32 in
  let v = 32 + x  in
  match  u with 
  | None -> 
    let sum x y = x + y in 
    sum 1 v
  | Some x -> 
    let sum x y = x + y in 
    sum x 1

let f6 x y = x == y

let f7 x = 
  match Some x with 
  | None -> None 
  | Some x -> x  

(* can [from_opt x ]  generate [Some None] which has type ['a Js.opt Js.opt] ?
   No, if [x] is [null] then None else [Some x]
*)
let f8 (x : 'a Js.opt Js.opt)= 
  match Js.from_opt x with 
  | Some x ->
    (match Js.from_opt x with 
    | Some _ -> 0
    | None -> 1 )
  | None -> 2 

let u = f8 (Js.to_opt (Js.to_opt None))
