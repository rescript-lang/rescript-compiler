type t0  = A | B | C 
(* https://github.com/ocaml/ocaml/pull/632 *)


let f x  = 
  match x with 
  | B -> 2 
  | A -> 1
  | C ->  3


let f_0 x = 
  match x with 
  | A -> -1 
  | B -> 0 
  | C -> 1 

  
type t1 = 
  | T000
  | T001
  | T002
  | T003


let f2 = function  
  |'\000' -> T000
  | '\001'-> T001
  | '\002' -> T002
  | _ -> T003


type t2 = 
  | X0
  | X1 
  | X2 
  | X3 
  | X4  

type t3  =
  | Y0
  | Y1 
  | Y2 
  | Y3
  | Y4
  | Y5 of int    

let f3 = function
  | X0 -> Y0 
  | X1 -> Y1 
  | X2 -> Y2 
  | X3 -> Y3
  | X4 -> Y4 


type t4 = 
  | T400 

let f4 = function 
  | T400 -> 3  

type t5 = 
  | A 
  | B  
  | F
  | C of int 
  | D of string 
  | E of int

let f5 = function 
  | A -> 1 
  | B -> 3 
  | F -> 4
  | C _ | D _ -> 1
  | _ -> 2 

let f6 = function 
  | A | B -> 0
  | C _ | D _ | E _  ->  1 
  | _ -> 2

let f7 = function 
  | A -> 1 
  | B -> 2 
  | C _ -> 3 
  | D _ -> 4
  | _ -> -1


type t6 = 
  | T60  
  | T61
  | T62
  | T63
  | T64 of int 
  | T65 of int 
  | T66 of int 
  | T68 of int


let f8 = function 
  | T60
  | T61 -> 1 
  | T64 _ 
  | T65 _ -> 2
  | _ -> 3 


let f9 = function 
  | T60
  | T61
  | T62 -> 1 
  | T64 _ 
  | T65 _ -> 2
  | _ -> 3 

let f10 = function 
  | T60 
  | T61
  | T62 
  | T63 -> 1 
  | T64 _ 
  | T65 _ -> 2
  | _ -> 3   

let f10 = function 
  | T60 -> 0
  | T61 -> 2 
  | T62  -> 4 
  | T63 -> 1 
  | T64 _ 
  | T65 _ -> 2
  | _ -> 3     


(* let f11 = function 
  | T60 -> 0
  | T61 -> 2 
  | T62  -> 4 
  | T63 -> 1 
  | T64 _ 
  | T65 _ -> 2
  | _ -> 3        *)


type t11 = A | B | C | D of int  | E of char 

let f11 (x : t11) = 
  match x with 
  | D _ -> 1 
  | A 
  | B
  | C -> 2 

  | _ -> assert false 
  