let f (x : < a:int; .. > as 'me1) = (x : < b:bool; .. > as 'me2);;
let f (x : < a:int; .. > as 'me1) = (x : < a:int; b:bool; .. > as 'me2);;
let f (x : [> `A of int] as 'me1) = (x : [> `B of bool] as 'me2);;
let f (x : [> `A of int] as 'me1) = (x : [`A of int | `B of 'me2] as 'me2);;
