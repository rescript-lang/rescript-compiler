[@@@config{
  flags = [|
  "-drawlambda";
  "-dsource";
  "-bs-diagnose"
  |]
}]

let h1 u = 
  u ## p

let h2 u = 
  u #@ m 1 2 
(*
  why this exists is because 
  [u ## m 1 2 [@bs]] was taken
  it is no longer the case.
*)
  
let h3 u = 
  let f = u##hi in 
  f 1 2   


 let h4 u = 
  u## hi 1 2  
(* method call still needs a sugar 
  This seems to be wrong in rescript syntax
  u ["hi"] (1, 2) 
*)

let h5 u =
  u ##hi #= 3   
(* assignment 
 This seems to be wrong in rescript syntax
  u["hi"] = 3 
*)

let h6 u = 
  u # p 

let h7 u = 
  u#m 1 2 [@bs]  

let h8 u = 
  let f = u#hi in 
  f 1 2 

let chain_f h = 
    h##x##y##z


let chain_g h =
  h#x#y#z  