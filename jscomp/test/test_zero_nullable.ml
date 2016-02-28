


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

let f9 x = Js.from_opt x 

let f10 x = Js.is_nil x 

let f11 =  (Js.is_nil @@ Js.to_opt 3)
module Undef = struct 



  let f1 x = 
    match Js.from_def x with 
    | None -> 
      let sum x y = x + y in 
      sum 1 2 
    | Some x -> 
      let sum x y = x + y in 
      sum x 1

  let f2 x = 
    let u = Js.from_def x in
    match  u with 
    | None -> 
      let sum x y = x + y in 
      sum 1 2 
    | Some x -> 
      let sum x y = x + y in 
      sum x 1



  let f5 h x = 
    let u = Js.from_def @@ h 32 in
    match  u with 
    | None -> 
      let sum x y = x + y in 
      sum 1 2
    | Some x -> 
      let sum x y = x + y in 
      sum x 1

  let f4 h x = 
    let u = Js.from_def @@ h 32 in
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

  (* can [from_def x ]  generate [Some None] which has type ['a Js.opt Js.opt] ?
     No, if [x] is [null] then None else [Some x]
  *)
  let f8 x = 
    match Js.from_def x with 
    | Some x ->
      (match Js.from_def x with 
       | Some _ -> 0
       | None -> 1 )
    | None -> 2 

  let u = f8 (Js.to_def (Js.to_def None))

  let f9 x = Js.from_def x 

  let f10 x = Js.is_undef x 
  let f11 = Js.is_undef (Js.to_def 3 )
end
